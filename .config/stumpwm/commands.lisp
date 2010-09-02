;;----------------------------------------------------------------------------
;; *data-dir*/commands.lisp
;;----------------------------------------------------------------------------

;; prompt with given arg as command, await further args and execute.
(defcommand pine (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

;; same as 'pine' but send output to echo-string. may hang if used wrong.
(defcommand pout (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (shell-command-output cmd))))

;; evaluate string.
(defcommand eval-line (cmd) ((:rest "eval: "))
  (handler-case
    (message "^20~{~a~^~%~}"
      (mapcar 'prin1-to-string
        (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

;; manpage reader. needs filename completion..
(defcommand manpage (command) ((:rest "manpage: "))
  (run-shell-command
    (format nil "urxvt -e man ~a" command)))

;; remember various states before executing certain commands.
(defcommand remstart () () (run-commands 
  "dump-screen-to-file ~/.config/stumpwm/storage/screen_data_last"
  "restart-soft"))
(defcommand remquit () () (run-commands 
  "dump-screen-to-file ~/.config/stumpwm/storage/screen_data_last"
  "quit"))
(defcommand remvsplit () () (run-commands
  "dump-screen-to-file /dev/shm/cache/.stumpwm_undo_data" "vsplit"
  "dump-screen-to-file /dev/shm/cache/.stumpwm_redo_data"))
(defcommand remhsplit () () (run-commands
  "dump-screen-to-file /dev/shm/cache/.stumpwm_undo_data" "hsplit"
  "dump-screen-to-file /dev/shm/cache/.stumpwm_redo_data"))

;; need to create a global command. gview X -> gselect X + echo X
(defcommand gview_1 () () (run-commands "gselect 1" "echo 1"))
(defcommand gview_2 () () (run-commands "gselect 2" "echo 2"))
(defcommand gview_3 () () (run-commands "gselect 3" "echo 3"))
(defcommand gview_4 () () (run-commands "gselect 4" "echo 4"))
(defcommand gview_5 () () (run-commands "gselect 5" "echo 5"))
(defcommand gview_6 () () (run-commands "gselect 6" "echo 6"))

;; better way is to use frame-number.. a little hard to 'make' though.
(defcommand (master-make tile-group) () () (run-commands
  "renumber 0" "repack-window-numbers"))
(defcommand (master-focus tile-group) () () (run-commands
  "select-window-by-number 0"))

;; this doesn't work yet.
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

;; better way is to 'refresh' all windows in group.. although harder.
;(defcommand resize-left () () (run-commands "resize -15 0" "refresh"))
;(defcommand resize-right () () (run-commands "resize 15 0" "refresh"))
;(defcommand resize-up () () (run-commands "resize 0 -15" "refresh"))
;(defcommand resize-down () () (run-commands "resize 0 15" "refresh"))

;; predefined echoes for speed, else use 'shell-command-output'.
;(defcommand announce-mifo () ()
;  (echo-string (current-screen) (run-shell-command "mifo --stumpwm" t)))

(defcommand shell-command-output (command) ((:string "shell/output: "))
  (check-type command string)
  (shell-command command))

;; undo [toggle] last remembered states.
(defcommand undo () () (run-commands
  "dump-screen-to-file /dev/shm/cache/.stumpwm_undo_tmp"
  "restore-from-file /dev/shm/cache/.stumpwm_undo_data"
  "exec mv -f /dev/shm/cache/.stumpwm_undo_tmp /dev/shm/cache/.stumpwm_undo_data"))

;; i don't like 'Colon' showing/editable command in prompt
;; perhaps i'll figure out a global macro/function for this..
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

;; run or raise.
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

