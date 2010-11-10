;;----------------------------------------------------------------------------
;; *data-dir*/macros.lisp
;;----------------------------------------------------------------------------

;; define a few parameters first.
;(defparameter X-TERM "exec urxvt")
;(defparameter X-TERM-BROWSER (concatenate 'string X-TERM " -e w3m"))
;(defparameter X-WWW-BROWSER "exec firefox ")
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
;  (0    t     t     :create "group_1")
;  (4    t     t     :title "mifo"))

;(define-frame-preference "2"
;  (0    t     t     :title "Ardour - Session Control")
;  (1    t     t     :instance "ardour_mixer")
;  (2    t     t     :instance "jvmetro")
;  (1    t     t     :instance "qjackctl")
;  (3    t     t     :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "3"
  (0    nil   t     :instance "Navigator")
  (0    nil   t     :class "luakit")
  (0    nil   t     :class "Jumanji")
  (1    t     t     :title "Add-ons")
  (1    t     t     :title "Downloads")
  (2    t     nil   :class "URxvt"))

;(define-frame-preference "4"
;  (0    t     t     :create "group_4"))

;(define-frame-preference "5"
;  (0    t     t     :create "group_5"))

(define-frame-preference "6"
  (0    t     t     :instance "ossxmix"))

