;;----------------------------------------------------------------------------
;; *data-dir*/macros.lisp
;;----------------------------------------------------------------------------

;; create given groups while keeping focus on current.
(defmacro make-groups-bg (&rest names)
  (let ((ns (mapcar #'(lambda (n) (concatenate 'string "gnewbg " n)) names)))
  `(run-commands ,@ns)))

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
 ; (0    t     t     :class "MPlayer"))

(define-frame-preference "2"
  (0    t     t     :title "Ardour - Session Control")
  (1    t     t     :instance "ardour_mixer")
  (2    t     t     :instance "jvmetro")
  (1    t     t     :instance "qjackctl")
  (3    t     t     :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "3"
;  (0    t     t     :create "group_3")
  (0    nil   t     :instance "Navigator")
  (0    nil   t     :class "luakit")
  (0    t     t     :class "Jumanji")
  (0    t     nil   :class "MPlayer")
  (1    t     t     :title "Add-ons")
  (1    t     nil   :class "URxvt"))

;(define-frame-preference "4"
;  (0    t     t     :create "group_4"))

;(define-frame-preference "5"
;  (0    t     t     :create "group_5"))

(define-frame-preference "6"
  (0    t     t     :instance "ossxmix"))

