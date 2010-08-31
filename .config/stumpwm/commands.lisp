(defcommand eval-line (cmd) ((:rest "eval: "))
  (handler-case
    (message "^20~{~a~^~%~}"
      (mapcar 'prin1-to-string
        (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

(defcommand manpage (command) ((:rest "manpage: "))
  (run-shell-command
    (format nil "urxvt -e man ~a" command)))

(defcommand remstart () () (run-commands 
  "dump-screen-to-file ~/.config/stumpwm/storage/screen_data_last"
  "restart-soft"))

(defcommand remquit () () (run-commands 
  "dump-screen-to-file ~/.config/stumpwm/storage/screen_data_last"
  "quit"))

(defcommand gview_1 () () (run-commands "gselect 1" "echo 1"))
(defcommand gview_2 () () (run-commands "gselect 2" "echo 2"))
(defcommand gview_3 () () (run-commands "gselect 3" "echo 3"))
(defcommand gview_4 () () (run-commands "gselect 4" "echo 4"))
(defcommand gview_5 () () (run-commands "gselect 5" "echo 5"))
(defcommand gview_6 () () (run-commands "gselect 6" "echo 6"))

(defcommand (master-focus tile-group) () () (run-commands "fselect 0"))

;(defcommand resize-left () () (run-commands "resize -15 0" "refresh"))
;(defcommand resize-right () () (run-commands "resize 15 0" "refresh"))
;(defcommand resize-up () () (run-commands "resize 0 -15" "refresh"))
;(defcommand resize-down () () (run-commands "resize 0 15" "refresh"))

(defcommand (master-swap tile-group) () ()
  (exchange-windows (current-window)
  (frame-window (frame-by-number (current-group 0)))
  ))

;(defcommand (master-swap tile-group) (dir &optional (win (current-window)))
;  ((:direction "Direction: "))
;  (if win
;  (let* ((frame-set (group-frames (window-group win)))
;         (neighbour (neighbour dir (window-frame win) frame-set)))
;  (if (and neighbour (frame-window neighbour))
;      (exchange-windows win (frame-window neighbour))
;      (message "no window in dir ~A" dir)))
;  (message "no window in current frame")))

;(defcommand (master-swap tile-group) () ()
;  (exchange-windows (frame-by-number (current-group) (current-window))
;  (frame-by-number (current-group) 0)))

(defcommand announce-mifo () ()
  (echo-string (current-screen) (run-shell-command "mifo --stumpwm" t)))

(defcommand announce-battery () ()
  (echo-string (current-screen) (run-shell-command "</proc/acpi/battery/BAT1/state" t)))

(defcommand run-my-command (cmd &optional collect-output-p)
  ((:shell "execute: ")) (if collect-output-p
    (run-prog-collect-output *shell-program* "-c sdcv -nu WordNet" cmd)
    (run-prog *shell-program* :args (list "-c sdcv -nu WordNet" cmd) :wait nil)))

(defun shell-command (command)
  (check-type command string)
  (echo-string (current-screen) (run-my-command command t)))

(defcommand dict (command) ((:string "dictionary: "))
  (check-type command string)
  (shell-command command))

(defcommand dictionary () ()
  (shell-command "echo example"))

(defcommand undo () () (run-commands
  "restore-from-file ~/.config/stumpwm/storage/.group_undo"))

; i don't like Colon showing/editable command in prompt
; perhaps i'll figure out a global function for this..
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

(defcommand ror_firefox () () (run-or-raise "firefox" '(:class "Firefox")))

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

