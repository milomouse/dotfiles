;;----------------------------------------------------------------------------
;; *data-dir*/hooks.lisp
;;----------------------------------------------------------------------------

;; show local windows in frame when focusing on it. unfortunately the echo
;; command is cropped when focused frame overlaps part of it's output.
;(defun local-list (to-frame from-frame)
;  (stumpwm:run-commands "echo-frame-windows"))
;(stumpwm:add-hook stumpwm:*focus-frame-hook* 'local-list)

;;(stumpwm:add-hook stumpwm:*urgent-window-hook*..

;; show local windows in frame when focusing on it.
;(defun show-groupfocus (to-group from-group)
;  (stumpwm:run-commands "vgroups"))
;(stumpwm:add-hook stumpwm:*focus-group-hook* 'show-groupfocus)
