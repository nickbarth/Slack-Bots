(ql:quickload '(:drakma :cl-json :websocket-driver-client :cl-ppcre) :silent t)

(defconstant SLACK_API_KEY "XXXX")
(defconstant HS_API_KEY "XXXX")

(defvar *slack-rtm-url* (concatenate 'string "https://slack.com/api/rtm.start?token=" SLACK_API_KEY))
(defvar *slack-msg-url* (concatenate 'string "https://slack.com/api/chat.postMessage?as_user=true&token=" SLACK_API_KEY))
(defvar *hs-api-url* "https://omgvamp-hearthstone-v1.p.mashape.com/cards/")

(defun https-json-request (url &optional headers params)
  (let* ((resp (flexi-streams:octets-to-string
        (drakma:http-request url :additional-headers headers :parameters params))))
    (json:decode-json-from-string resp)))

(defun get-slack-info ()
  (let ((json (https-json-request *slack-rtm-url*)))
    (values (cdr (assoc ':ID (cdr (assoc ':SELF json))))
            (cdr (assoc ':URL json)))))

(defun parse-slack-data (data)
  (let ((payload (json:decode-json-from-string data)))
    (values (cdr (assoc ':TYPE payload))
            (cdr (assoc ':CHANNEL payload))
            (cdr (assoc ':TEXT payload)))))

(defun send-slack-message (message channel)
  (https-json-request *slack-msg-url* nil
    (list (cons "channel" channel) (cons "text" message))))

(defun get-card-image (card)
  (let* ((api (concatenate 'string *hs-api-url* (cl-ppcre:regex-replace-all " " card "%20")))
        (headers (list (cons "x-mashape-key" HS_API_KEY)))
        (json (car (https-json-request api headers))))
    (if (eq (car json) ':ERROR)
      "Card Not Found" (cdr (assoc ':IMG json)))))

(defun asked-for-card? (user-id type message)
  (and (string-equal type "message")
  (search (concatenate 'string "<@" user-id "> card ") message)))

(defun send-card-image (card channel)
  (send-slack-message (get-card-image card) channel))

(defun main ()
  (multiple-value-bind (user-id ws-url) (get-slack-info)
    (let ((client (wsd:make-client ws-url)))
      (wsd:on :message client
        (lambda (data)
          (multiple-value-bind (type channel message) (parse-slack-data data)
            (if (asked-for-card? user-id type message)
              (send-card-image (subseq message 18) channel)))))
      (as:with-event-loop ()
        (wsd:start-connection client)))))

(main)
