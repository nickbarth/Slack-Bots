(ql:quickload '(:drakma :cl-json :websocket-driver-client :cl-ppcre) :silent t)

(defconstant SLACK_API_KEY "xxxx")
(defconstant HS_API_KEY "xxxx")

(defvar *slack-rtm-url* (concatenate 'string "https://slack.com/api/rtm.start?token=" SLACK_API_KEY))
(defvar *slack-msg-url* (concatenate 'string "https://slack.com/api/chat.postMessage?as_user=false&token=" SLACK_API_KEY))
(defvar *hs-api-url* "https://omgvamp-hearthstone-v1.p.mashape.com/cards/")

(defun https-json-request (url &optional headers params)
  (let* ((resp (flexi-streams:octets-to-string
        (drakma:http-request url :additional-headers headers :parameters params))))
    (json:decode-json-from-string resp)))

(defun make-client (slack)
  (wsd:make-client (cdr (assoc ':URL slack))))

(defun get-slack-info ()
  (let ((json (https-json-request *slack-rtm-url*)))
    (list (assoc ':ID (cdr (assoc ':SELF json)))
          (assoc ':NAME (cdr (assoc ':SELF json)))
          (assoc ':URL json))))

(defun parse-slack-data (data)
  (let ((payload (json:decode-json-from-string data)))
    (values (cdr (assoc ':TYPE payload))
            (cdr (assoc ':CHANNEL payload))
            (cdr (assoc ':TEXT payload)))))

(defun post-slack-message (slack message channel)
  (https-json-request *slack-msg-url* nil
    (list (cons "username" (cdr (assoc ':NAME slack))) (cons "channel" channel) (cons "text" message))))

(defun get-card-image (card)
  (let* ((api (concatenate 'string *hs-api-url* (cl-ppcre:regex-replace-all " " card "%20")))
        (headers (list (cons "x-mashape-key" HS_API_KEY)))
        (json (car (https-json-request api headers))))
    (if (eq (car json) ':ERROR) "Card Not Found"
  (cdr (assoc ':IMG json)))))

(defun asked-for-card? (slack type message)
  (and (string-equal type "message") 
  (search (concatenate 'string "<@" (cdr (assoc ':ID slack)) "> card ") message)))

(defun send-card-image (slack card channel)
  (post-slack-message slack (get-card-image card) channel))

(defun main ()
  (let* ((slack (get-slack-info))
        (client (make-client slack)))
    (wsd:on :message client
      (lambda (data)
        (multiple-value-bind (type channel message) (parse-slack-data data)
          (if (asked-for-card? slack type message)
            (send-card-image slack (subseq message 18) channel)))))
    (as:with-event-loop ()
      (wsd:start-connection client))))

(main)
