;;----------------------------------------------------------------------------
;; *data-dir*/functions.lisp
;;----------------------------------------------------------------------------

;(defvar *keysym-name-translations* (make-hash-table))
;(defvar *name-keysym-translations* (make-hash-table :test #'equal))
;
;(defun define-keysym (keysym name)
;  "Define a mapping from a keysym name to a keysym."
;  (setf (gethash keysym *keysym-name-translations*) name
;        (gethash name *name-keysym-translations*) keysym))
;
;(defun keysym-name->keysym (name)
;  "Return the keysym corresponding to NAME."
;  (multiple-value-bind (value present-p)
;      (gethash name *name-keysym-translations*)
;    (declare (ignore present-p))
;    value))
;
;(defun keysym->keysym-name (keysym)
;  "Return the name corresponding to KEYSYM."
;  (multiple-value-bind (value present-p)
;      (gethash keysym *keysym-name-translations*)
;    (declare (ignore present-p))
;    value))

(defun cat (&rest strings)
  (apply `concatenate `string strings))

(defun shell-command (command)
  (check-type command string)
  (echo-string (current-screen) (run-shell-command command t)))

(defun ps-exists (ps)
  (let ((f "ps -ef | grep ~S | grep -v -e grep -e stumpish | wc -l"))
    (< 0 (parse-integer (run-shell-command (format nil f ps) t)))))

(defun start-uniq-command-ps (command &key options (background t))
  (unless (ps-exists command)
    (run-shell-command
     (concat command " " options " " (when background "&")))))

(defun kill-ps-command (command)
  (format nil "kill -TERM `ps -ef | grep ~S | grep -v grep | awk '{print $2}'`"
          command))

(defun kill-ps (command)
  (run-shell-command (kill-ps-command command)))

(defun expand-file-name (path &optional default-directory)
  (let ((first-char (subseq path 0 1))
    (home-dir (cat (getenv "HOME") "/"))
    (dir (if default-directory
      (if (string= (subseq (reverse default-directory) 0 1) "/")
        default-directory
        (cat default-directory "/")))))
  (cond ((string= first-char "~") (cat home-dir (subseq path 2)))
        ((string= first-char "/") path)
        (dir (if (strings= (subseq 0 1) "/")
          (cat dir path)
          (expand-file-name (cat dir path))))
        (t (cat home-dir path)))))

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

