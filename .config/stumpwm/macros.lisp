;;----------------------------------------------------------------------------
;; *data-dir*/macros.lisp
;;----------------------------------------------------------------------------

(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (cat "gnewbg " n)) names)))
  `(run-commands ,@ns)))

(define-stumpwm-type :password (input prompt)
  (let ((history *input-history*)
        (arg (argument-pop input))
        (fn (symbol-function 'draw-input-bucket)))
    (unless arg
      (unwind-protect
           (setf (symbol-function 'draw-input-bucket)
                 (lambda (screen prompt input &optional errorp)
                   (let ((i (copy-structure input)))
                     (setf (input-line-string i)
                           (make-string (length (input-line-string i))
                                        :initial-element #\*))
                     (funcall fn screen prompt i)))
                 arg (read-one-line (current-screen) prompt))
        (setf (symbol-function 'draw-input-bucket) fn
              *input-history* history))
      arg)))

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
(define-frame-preference "1"
;; frame raise lock (lock AND raise == jumpto)
  (0    t     nil   :class "Konqueror" :role "...konqueror-mainwindow"))
 ;; (1    t     nil   :class "URxvt"))

(define-frame-preference "2"
  (0    t     t     :title "Ardour - Session Control")
  (1    t     t     :instance "ardour_mixer")
  (2    t     t     :instance "jvmetro")
  (1    t     t     :instance "qjackctl")
  (3    t     t     :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "3"
  (0    nil   t     :class "Firefox"))

(define-frame-preference "4"
  (1    t     t     :restore "emacs-editing-dump" :title "...xdvi")
  (0    t     t     :create "screen_data_04" :class "Emacs"))

(define-frame-preference "5"
  (1    t     t     :restore "emacs-editing-dump" :title "...xdvi")
  (0    t     t     :create "group_data_05" :class "Emacs"))

(define-frame-preference "6"
  (1    t     t     :restore "emacs-editing-dump" :title "...xdvi")
  (0    t     t     :create "group_data_06" :class "Emacs"))

