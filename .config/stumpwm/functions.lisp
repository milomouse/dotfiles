;;----------------------------------------------------------------------------
;; *data-dir*/functions.lisp
;;----------------------------------------------------------------------------

;; move current window to next group but do not focus.
(defun move-window-to-next-group (current list)
  (let ((next (next-group current (non-hidden-groups list)))
        (win (group-current-window current)))
    (when (and next win) (move-window-to-group win next))))

;; exchange windows but focus remains on current frame.
(defun exchange-windows-remain (win1 win2)
  (let ((f1 (window-frame win1))
        (f2 (window-frame win2)))
    (unless (eq f1 f2)
      (pull-window win1 f2)
      (pull-window win2 f1))))

;; focus frame [also when splitting] but do not show-frame-indicator in some cases.
(defun focus-frame (group f)
  (let ((w (frame-window f))
        (last (tile-group-current-frame group))
        (show-indicator nil))
    (setf (tile-group-current-frame group) f)
    (unless (eq f last)
      (setf (tile-group-last-frame group) last)
      (run-hook-with-args *focus-frame-hook* f last)
      (setf show-indicator t))
    (if w (focus-window w) (no-focus group (frame-window last)))
    (if show-indicator (show-frame-outline group))))

(defun split-frame-in-dir (group dir)
  (let ((f (tile-group-current-frame group)))
    (if (split-frame group dir)
        (progn
          (update-decoration (frame-window f)))
        (message "Canot split smaller than minimum size."))))

;; select a random image from *background-image-path* and display it on root window.
(defun select-random-bg-image ()
  (let ((file-list (directory (concatenate 'string *background-image-path* "*.png")))
        (*random-state* (make-random-state t)))
    (namestring (nth (random (length file-list)) file-list))))

;; run a shell command.
(defun shell-command (command)
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

;; expand filenames with special focus on home dir.
;(defun expand-file-name (path &optional default-directory)
;  (let ((first-char (subseq path 0 1))
;    (home-dir (concatenate 'string (getenv "HOME") "/"))
;    (dir (if default-directory
;      (if (string= (subseq (reverse default-directory) 0 1) "/")
;        default-directory
;        (concatenate 'string default-directory "/")))))
;  (cond ((string= first-char "~") (concatenate 'string home-dir (subseq path 2)))
;        ((string= first-char "/") path)
;        (dir (if (strings= (subseq 0 1) "/")
;          (concatenate 'string dir path)
;          (expand-file-name (concatenate 'string dir path))))
;        (t (concatenate 'string home-dir path)))))

;; to ease repetition in commands.
(defun remember-undo () () (dump-screen-to-file "/dev/shm/.stumpwm_undo_data"))
(defun remember-last () ()
  (dump-window-placement-rules "/home/milo/.config/stumpwm/storage/placement_rules")
  (dump-screen-to-file "/home/milo/.config/stumpwm/storage/screen_data_last"))

;; use prettier eval error and skip mode-line updates (since i don't use it)
(defun eval-command (cmd &optional interactivep)
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
		 (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
      ;; this fancy footwork lets us grab the backtrace from where the error actually happened.
      (restart-case (handler-bind 
          ((error (lambda (c)
                    (invoke-restart 'eval-command-error
                          (format nil "^B^0*{{ ^9*~a ^0*}} ^n~A~a" 
                                cmd c (if *show-command-backtrace* 
                                          (backtrace-string) ""))))))
            (parse-and-run-command cmd))
        (eval-command-error (err-text)
          (values err-text t)))
      (cond ((stringp result)
             (if error-p  (message-no-timeout "~a" result)
                          (message "~a" result)))
            ((eq result :abort)
             (unless *suppress-abort-messages* (message "Abort.")))))))

;; cleaner {i}resize bindings.
(defun update-resize-map ()
  (let ((m (setf *resize-map* (make-sparse-keymap))))
    (let ((i *resize-increment*))
    (labels ((dk (m k c) (define-key m k (format nil c i))))
      (dk m (kbd "k") "resize 0 -~D")
      (dk m (kbd "(") "resize 0 -~D")
      (dk m (kbd "j") "resize 0 ~D")
      (dk m (kbd ")") "resize 0 ~D")
      (dk m (kbd "h") "resize -~D 0")
      (dk m (kbd "9") "resize -~D 0")
      (dk m (kbd "l") "resize ~D 0")
      (dk m (kbd "0") "resize ~D 0")
      (dk m (kbd "RET") "exit-iresize")
      (dk m (kbd "ESC") "abort-iresize")
    M)))) (update-resize-map)

(defun fmt-group-status (group)
  (let ((screen (group-screen group)))
    (cond ((eq group (screen-current-group screen))
           #\*)
          ((and (typep (second (screen-groups screen)) 'group)
                (eq group (second (screen-groups screen))))
           #\+)
          (t #\-))))

(defvar *surfraw-bookmark-file* nil)
(defun display-file (file)
  (if (probe-file file)
      (run-shell-command (concatenate 'string "cat " file) t)
    (message "The file ~a does not exist." file)))
;(defmacro surfraw-selection (name engine)
;  `(defcommand ,name () ()
;    (surfraw ,engine (get-x-selection))))

;(defstruct scratchpad
;  (last-group '())
;  (group '()))

;(defvar *scratchpads* '()
;  "All scratchpads indexed by screen.")

;(defun current-scratchpad ()
;  (gethash (current-screen) *scratchpads*))

;(defun create-scratchpad-group (screen)
;  (let ((scratchpad-group (add-group screen "S")))
;    (setf (group-number scratchpad-group) 0)
;    scratchpad-group))

;(unless *scratchpads*
;  ;; Create a scratchpad for each screen
;  (setf *scratchpads* (make-hash-table :test #'eq))
;  (let ((start-screen (car *screen-list*)))
;    (loop for i in *screen-list*
;       do (progn (switch-to-screen i)
;                 (let ((scratchpad-group (create-scratchpad-group i)))
;                   ;;Store the scratchpad
;                   (setf (gethash (current-screen)
;                                  *scratchpads*)
;                         (make-scratchpad
;                          :group scratchpad-group)))))
;    (switch-to-screen start-screen)))

;(defcommand scratchpad () ()
;            (let ((scratchpad (current-scratchpad)))
;              (if scratchpad
;                (cond
;                    ((scratchpad-last-group scratchpad)
;                     (switch-to-group (scratchpad-last-group scratchpad))
;                     (setf (scratchpad-last-group scratchpad) nil))
;                    ((eq (current-group) (scratchpad-group scratchpad))
;                     (message "scratchpad: I don't know where home is"))
;                    (t
;                     (setf (scratchpad-last-group scratchpad) (current-group))
;                     (switch-to-group (scratchpad-group scratchpad))
;                     (message "scratchpad")))
;                  (message "No scratchpad for this screen."))))

