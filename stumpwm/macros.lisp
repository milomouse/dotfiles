;;----------------------------------------------------------------------------
;; *data-dir*/macros.lisp
;;----------------------------------------------------------------------------

;; define a few parameters first (not sure if this is necessary..)
;(defparameter X-TERM "exec urxvt")
;(defparameter X-TERM-BROWSER (concatenate 'string X-TERM " -e w3m"))
;(defparameter X-WWW-BROWSER "exec firefox")
;(defparameter X-IMAGE-VIEWER "exec gliv ")

;; create given groups while keeping focus on current.
(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
  `(run-commands ,@ns)))

;; faster hook management.
(defmacro replace-hook (hook fn)
  `(remove-hook, hook, fn)
  `(add-hook, hook, fn))

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.

;; frame raise lock (lock AND raise == jumpto)
;(define-frame-preference "1"
;  (0    t     t     :create "group_1"))
;  (4    t     t     :class "MPlayer"))

;(define-frame-preference "2"
;  (0    t     t     :title "Ardour - Session Control"))

;; internet related workspace:
(define-frame-preference "3"
  (1    t     t     :instance "Dialog")
  (0    nil   t     :instance "Navigator"))

;; largely undefined, temporal workspace:
(define-frame-preference "6"
  (0    t     t     :class "Ossxmix")
  (0    t     t     :class "Gliv"))

