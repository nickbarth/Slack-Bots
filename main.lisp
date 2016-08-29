(ql:quickload '(:drakma :cl-json :websocket-driver-client :cl-ppcre) :silent t)

(defvar SLACK_API_KEY "xxxx")
(defvar HS_API_KEY "xxxx")

(defvar SLACK_RTM_URL (concatenate 'string "https://slack.com/api/rtm.start?token=" SLACK_API_KEY))
(defvar SLACK_MSG_URL (concatenate 'string "https://slack.com/api/chat.postMessage?as_user=false&token=" SLACK_API_KEY))
(defvar HS_API_URL "https://omgvamp-hearthstone-v1.p.mashape.com/cards/")

(defun https-json-request (url &optional headers)
  (let* ((resp (flexi-streams:octets-to-string
        (drakma:http-request url :additional-headers headers))))
    (json:decode-json-from-string resp)))

(defun make-client (slack)
  (wsd:make-client (cdr (assoc ':URL slack))))

(defun get-slack-info ()
  (let ((json (https-json-request SLACK_RTM_URL)))
    (list (assoc ':ID (cdr (assoc ':SELF json)))
          (assoc ':NAME (cdr (assoc ':SELF json)))
          (assoc ':URL json))))

(defun parse-slack-data (data)
  (let ((payload (json:decode-json-from-string data)))
    (values (cdr (assoc ':TYPE payload))
            (cdr (assoc ':CHANNEL payload))
            (cdr (assoc ':TEXT payload)))))

(defun post-slack-msg (slack msg channel)
  (https-json-request (concatenate 'string SLACK_MSG_URL
    "&username=" (cdr (assoc ':NAME slack)) "&channel=" channel "&text=" msg)))

(defun get-card-image (card)
  (let ((api (concatenate 'string HS_API_URL (cl-ppcre:regex-replace-all " " card "%20")))
      (headers (list (cons "x-mashape-key" HS_API_KEY))))
    (cdr (assoc ':IMG (car (https-json-request api headers))))))

(defun asked-for-card? (slack type msg)
  (and (string-equal type "message") 
  (search (concatenate 'string "<@" (cdr (assoc ':ID slack)) "> card ") msg)))

(defun send-card-image (slack card channel)
  (post-slack-msg slack (get-card-image card) channel))

(defun main ()
  (let* ((slack (get-slack-info))
        (client (make-client slack)))
    (wsd:on :message client
      (lambda (data)
        (multiple-value-bind (ptype pchannel pmsg) (parse-slack-data data)
          (if  (asked-for-card? slack ptype pmsg)
            (send-card-image slack (subseq pmsg 18) pchannel)))))
    (as:with-event-loop ()
      (wsd:start-connection client))))

(main)
