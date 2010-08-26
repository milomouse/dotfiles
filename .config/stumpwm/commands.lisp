(defcommand manpage (command) ((:rest "manpage: "))
  (run-shell-command
    (format nil "urxvt -e man ~a" command)))

(defcommand remstart () () (run-commands 
  "dump-screen-to-file /home/milo/.config/stumpwm/storage/screen_data_last"
  "restart-soft"))

(defcommand remquit () () (run-commands 
  "dump-screen-to-file /home/milo/.config/stumpwm/storage/screen_data_last"
  "quit"))

;(defcommand nm-applet () ()
;  (start-uniq-command-ps "nm-applet" :options "--sm-disable"))

(defcommand uptime () ()
  (echo-string (current-screen) (run-shell-command "print ${${$(uptime)[3]/,}" t)))

;(define-sudo-command eon
;  (concat "pon em"
;          (when (ps-exists "nm-applet")
;            (concat "&& sudo " (kill-ps-command "nm-applet"))))
;  :output t)

;(define-sudo-command dimmer "moodlight -d 1" :output t)
;(define-sudo-command reboot "reboot")
;(define-sudo-command shutdown "poweroff")
;(define-sudo-command backup "~/src/backup/backup.sh")

(defcommand prompt-mifo-command (filename) ((:rest "command: "))
  (run-shell-command (format nil "mifo --command ~a" filename)))
(defcommand prompt-mifo-next (filename) ((:rest "next: "))
  (run-shell-command (format nil "mifo --next ~a" filename)))
(defcommand prompt-mifo-prev (filename) ((:rest "previous: "))
  (run-shell-command (format nil "mifo --prev ~a" filename)))
(defcommand prompt-mifo-save (filename) ((:rest "save-as: "))
  (run-shell-command (format nil "mifo --save ~a" filename)))
(defcommand prompt-mifo-load (filename) ((:rest "load: "))
  (run-shell-command (format nil "mifo --load ~a" filename)))
(defcommand prompt-mifo-append (filename) ((:rest "append: "))
  (run-shell-command (format nil "mifo --append ~a" filename)))
(defcommand prompt-mifo-playlist (filename) ((:rest "playlist: "))
  (run-shell-command (format nil "mifo --playlist ~a" filename)))
(defcommand prompt-mifo-reload (filename) ((:rest "reload: "))
  (run-shell-command (format nil "mifo --reload ~a" filename)))

(defcommand ror__firefox () () (run-or-raise "firefox" '(:class "Firefox")))

(defcommand rrr () (:rest "run or raise: ")
  (run-or-raise ~a '(:instance ~a)))

;;(defcommand toggle-split () ()
;;  (let* ((group (current-group))
;;         (cur-frame (tile-group-current-frame group))
;;         (frames (group-frames group)))
;;    (if (eq (length frames) 2)
;;        (progn (if (or (neighbour :left cur-frame frames)
;;                       (neighbour :right cur-frame frames))
;;                   (progn
;;                     (only)
;;                     (vsplit))
;;                 (progn
;;                   (only)
;;                   (hsplit))))
;;      (message "Works only with 2 frames"))))

