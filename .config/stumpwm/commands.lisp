;;----------------------------------------------------------------------------
;; *data-dir*/commands.lisp
;;----------------------------------------------------------------------------

;; designate master window/frame (should probably use current frame number, but less dynamic?)
(defcommand (master-make tile-group) () () (renumber 0) (repack-window-numbers) (remember-last))
(defcommand (master-focus tile-group) () () (select-window-by-number 0))

;; swap current window with master (should be 0 (from master-make)) and desginate it as the new master.
(defcommand (master-swap tile-group) (num &optional (group (current-group))) ((:window-number t))
  (labels ((match (win)
              (= (window-number win) num)))
  (let ((win (find-if #'match (group-windows group))))
    (when (and win group) (exchange-windows (current-window) win) (master-make)))))

;; [with *shell-program* "/bin/zsh"] look for detached 'tmux -L xorg' session and attach, else create new.
(defcommand tmux-attach-else-new () ()
  (run-shell-command
  "if [[ -n ${$(tmux -L xorg list-session|grep -v attached)[1]//:} ]]; then
    urxvt -e tmux -f ${XDG_CONFIG_DIR:-${HOME}/.config}/tmux/tmux.conf -L xorg attach-session -t $(print ${$(tmux -L xorg list-session|grep -v attached)[1]//:})
  else
    urxvt -e tmux -f ${XDG_CONFIG_DIR:-${HOME}/.config}/tmux/tmux.conf -L xorg new-session
  fi"))

;; remember layout before reloading/restarting/quitting (but do not overwrite original commands)
(defcommand rem-loadrc () () (remember-last) (loadrc))
(defcommand rem-restart () () (remember-last) (restart-soft))
(defcommand rem-quit () () (remember-last) (quit))

;; undo [toggle] last remembered states (made useful by most of my stumpwm commands)
(defcommand undo () ()
  (dump-screen-to-file "/dev/shm/cache/.stumpwm_undo_tmp")
  (restore-from-file "/dev/shm/cache/.stumpwm_undo_data")
  (run-shell-command "mv -f /dev/shm/cache/.stumpwm_undo_tmp /dev/shm/cache/.stumpwm_undo_data"))

;; move window to next/prev group without switching to it (unlike gnext-with-window)
(defcommand gmove-next () ()
  (move-window-to-next-group (current-group) (sort-groups (current-screen))))
(defcommand gmove-prev () ()
  (move-window-to-next-group (current-group) (reverse (sort-groups (current-screen)))))

;; remember frame positions before resizing.
(defcommand (resize tile-group) (width height) ((:number "+ Width: ")
                                                (:number "+ Height: "))
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (clear-frame-outlines group)
          (resize-frame group f width :width)
          (resize-frame group f height :height)
          (draw-frame-outlines group (current-head))
          (curframe))))) (defcommand (iresize tile-group) () ()
  (let ((frame (tile-group-current-frame (current-group))))
    (if (atom (tile-group-frame-head (current-group) (frame-head (current-group) frame)))
        (message "There's only 1 frame!")
        (progn
          (remember-undo)
          (when *resize-hides-windows*
            (dolist (f (head-frames (current-group) (current-head)))
              (clear-frame f (current-group))))
          (push-top-map *resize-map*)
          (draw-frame-outlines (current-group) (current-head)))
        ))) (defcommand (exit-iresize tile-group) () () (resize-unhide) (pop-top-map))
;; .. also have a quiet-resize that hides frame-outlines (not used in 'iresize')
(defcommand (quiet-resize tile-group) (width height) ((:number "+ Width: ")
                                                (:number "+ Height: "))
  (let* ((group (current-group))
         (f (tile-group-current-frame group)))
    (if (atom (tile-group-frame-tree group))
        (message "No more frames!")
        (progn
          (resize-frame group f width :width)
          (resize-frame group f height :height)))))
;; .. and undo resize if aborted.
(defcommand (abort-iresize tile-group) () ()
  (resize-unhide) (undo) (message "Abort resize") (pop-top-map))

;; remove frame and reallocate space while remembering removed frame position, also hiding frame-indicator.
(defcommand (remove-split tile-group)
(&optional (group (current-group)) (frame (tile-group-current-frame group))) ()
  (let* ((head (frame-head group frame))
         (current (tile-group-current-frame group))
         (tree (tile-group-frame-head group head))
         (s (closest-sibling (list tree) frame))
         (l (tree-accum-fn s
                           (lambda (&rest siblings)
                             (car siblings))
                           #'identity)))
    ;; <only remove the current frame if it has a sibling>
    (if (atom tree)
        (message "No more frames!")
        (when s
          (when (frame-is-head group frame)
          (remember-undo)
            (setf (frame-number l) (frame-number frame)))
          ;; <move the windows from the removed frame to its sibling>
          (migrate-frame-windows group frame l)
          ;; <if the frame has no window, give it the current window of the current frame.>
          (unless (frame-window l)
            (setf (frame-window l)
                  (frame-window frame)))
          ;; <unsplit>
          (setf (tile-group-frame-head group head) (remove-frame tree frame))
          ;; <update the current frame and sync all windows>
          (when (eq frame current)
            (setf (tile-group-current-frame group) l))
          (tree-iterate tree
                        (lambda (leaf)
                          (sync-frame-windows group leaf)))
          (frame-raise-window group l (frame-window l) nil)
          (when (frame-window l)
            (update-decoration (frame-window l)))))))
        
;; remember states if not already in 'only' mode (e.g., one frame).
(defcommand only () ()
  (let* ((screen (current-screen))
         (group (screen-current-group screen))
         (win (group-current-window group))
         (head (current-head group))
         (frame (copy-frame head)))
    (if (atom (tile-group-frame-head group head))
      (message "Will not remember state, already using one frame.")
      (progn
        (remember-undo)
        (mapc (lambda (w)
                (unless (eq (window-frame w) (tile-group-current-frame group))
                  (hide-window w))
                (setf (window-frame w) frame))
              (head-windows group head))
        (setf (frame-window frame) win
              (tile-group-frame-head group head) frame
              (tile-group-current-frame group) frame)
        (focus-frame group frame)
        (if (frame-window frame)
            (update-decoration (frame-window frame))
            (show-frame-indicator group))
        (sync-frame-windows group (tile-group-current-frame group))))))

;; remember frame positions before splitting (do not edit split-frames function for this)
(defcommand (hsplit tile-group) () () (remember-undo) (split-frame-in-dir (current-group) :column))
(defcommand (vsplit tile-group) () () (remember-undo) (split-frame-in-dir (current-group) :row))

;; dump to file, which is silent, but with more informative prompts.
(defcommand dump-group-to-file (file) ((:rest "group to file: "))
  (dump-to-file (dump-group (current-group)) file))
(defcommand dump-screen-to-file (file) ((:rest "screen to file: "))
  (dump-to-file (dump-screen (current-screen)) file))
(defcommand dump-desktop-to-file (file) ((:rest "desktop to file: "))
  (dump-to-file (dump-desktop) file))

;; predefined echoes for speed, else use 'shell-command-output'.
(defcommand announce-mifo () () (echo-string (current-screen) (run-shell-command "mifo -as" t)))
(defcommand announce-mifo-raw () () (echo-string (current-screen) (run-shell-command "mifo -ar" t)))
(defcommand announce-battery () () (echo-string (current-screen) (run-shell-command "</proc/acpi/battery/BAT1/state" t)))
(defcommand announce-memory () () (echo-string (current-screen) (run-shell-command "print free used base;free -m|awk 'NR==2 {print $3,$4,$2}'" t)))
(defcommand announce-loadavg () () (echo-string (current-screen) (run-shell-command "print ${$(</proc/loadavg)[1,3]}" t)))
(defcommand announce-highcpu () () (echo-string (current-screen) (run-shell-command "ps -U root --deselect -C tmux,urxvt k -%cpu opid,args:70,etime:8,%cpu,pmem" t)))
(defcommand announce-volume () () (echo-string (current-screen) (run-shell-command "print ${$(ossmix|awk 'NR==29')[4]}dB" t)))
(defcommand announce-volup () () (run-shell-command "ossvalt -i 1" t) (announce-volume))
(defcommand announce-voldown () () (run-shell-command "ossvalt -d 1" t) (announce-volume))
(defcommand announce-volmute () () (run-shell-command "ossvalt -m" t) (announce-volume))

;; sent output of command to echo-string. may hang if used wrong.
(defcommand shell-command-output (command) ((:string "shell/output: "))
  (check-type command string) (shell-command command))

;; prompt with given arg as command, and if needed await further args, and execute.
(defcommand pine (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (eval-command cmd t))))

;; same as 'pine' but send output to echo-string. may hang if used wrong.
(defcommand pout (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (shell-command-output cmd))))

;; manpage reader. needs filename completion..
(defcommand manpage (command) ((:rest "manpage: "))
  (run-shell-command (format nil "urxvt -e man ~a" command)))

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

;; evaluate string.
(defcommand eval-line (cmd) ((:rest "eval: "))
  (handler-case
    (message "^B^50~{~a~^~%~}"
      (mapcar 'prin1-to-string
        (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

;; use prettier eval errors and skip mode-line updates (since i don't use it)
(defun eval-command (cmd &optional interactivep)
  (labels ((parse-and-run-command (input)
             (let* ((arg-line (make-argument-line :string input :start 0))
                    (cmd (argument-pop arg-line)))
               (let ((*interactivep* interactivep))
		 (call-interactively cmd arg-line)))))
    (multiple-value-bind (result error-p)
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
             (if error-p (message-no-timeout "~a" result)
                         (message "~a" result)))
            ((eq result :abort)
             (unless *suppress-abort-messages* (message "Abort.")))))))


;; run or raise.
(defcommand ror_firefox () () (setf *run-or-raise-all-groups* t) (run-or-raise "firefox" '(:class "Firefox")))

;; select a random background image.
(defcommand display-random-bg () () (run-shell-command
  (concatenate 'string "display -window root -resize 1366x768! " (select-random-bg-image))))

