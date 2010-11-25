;;----------------------------------------------------------------------------
;; *data-dir*/commands.lisp
;;----------------------------------------------------------------------------

;; move focused window to next/prev group without switching to it (unlike gnext-with-window)
(defcommand gmove-next () ()
  (move-window-to-next-group (current-group) (sort-groups (current-screen))))
(defcommand gmove-prev () ()
  (move-window-to-next-group (current-group) (reverse (sort-groups (current-screen)))))

;; same as 'exchange-direction' but focus remains on current frame
(defcommand (exchange-direction-remain tile-group) (dir &optional (win (current-window)))
    ((:direction "Direction: "))
  (if win
      (let* ((frame-set (group-frames (window-group win)))
             (neighbour (neighbour dir (window-frame win) frame-set)))
        (if (and neighbour (frame-window neighbour))
            (exchange-windows-remain win (frame-window neighbour))
            (message "No window in direction ~A!" dir)))
      (message "No window in current frame!")))

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
;; (useful for StumpWM crashes, as tmux windows survive crashes and this command brings them back)
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
  (dump-screen-to-file "/dev/shm/.stumpwm_undo_tmp")
  (restore-from-file "/dev/shm/.stumpwm_undo_data")
  (run-shell-command "mv -f /dev/shm/.stumpwm_undo_tmp /dev/shm/.stumpwm_undo_data"))

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
          (remember-undo)
          (when (frame-is-head group frame)
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
(defcommand (hsplit-resize tile-group) (group fraction) (horiz-split-frame group)
  (remember-undo)
  (let ((frame (tile-group-current-frame group)))
    (resize-frame group
                  frame
                  (truncate (* (- (* 2 fraction) 1) (frame-width frame)))
                  'width)))
(defcommand (vsplit-resize tile-group) (group fraction) (vert-split-frame group)
  (remember-undo)
  (let ((frame (tile-group-current-frame group)))
    (resize-frame group
                  frame
                  (truncate (* (- (* 2 fraction) 1) (frame-height frame)))
                  'height)))

;; dump to file, which is silent, but with more informative prompts.
(defcommand dump-group-to-file (file) ((:rest "group to file: "))
  (dump-to-file (dump-group (current-group)) file))
(defcommand dump-screen-to-file (file) ((:rest "screen to file: "))
  (dump-to-file (dump-screen (current-screen)) file))
(defcommand dump-desktop-to-file (file) ((:rest "desktop to file: "))
  (dump-to-file (dump-desktop) file))

;; predefined echoes for speed, else use 'shell-command-output'.
(defcommand echo-mifo-stumpwm () () (echo-string (current-screen) (run-shell-command "mifo --stumpwm" t)))
(defcommand echo-mifo-raw () () (echo-string (current-screen) (run-shell-command "mifo --raw" t)))
(defcommand echo-mifo-current-list () () (echo-string (current-screen) (run-shell-command "mifo --show current|grep -A 7 -B 7 $(mifo --raw)|sed 's|'$(mifo --raw)'|^B^1*&^n|'" t)))
(defcommand echo-mifo-playlists () () (echo-string (current-screen) (run-shell-command "mifo --show" t)))
(defcommand echo-mifo-fav-add () () (echo-string (current-screen) (run-shell-command "mifo --fav-add" t)))
(defcommand echo-mifo-fav-del () () (echo-string (current-screen) (run-shell-command "mifo --fav-delete" t)))
(defcommand echo-mifo-random () () (echo-string (current-screen) (run-shell-command "mifo -r" t)) (echo-mifo-stumpwm))
(defcommand echo-mifo-next () () (echo-string (current-screen) (run-shell-command "mifo --next" t)) (echo-mifo-stumpwm))
(defcommand echo-mifo-prev () () (echo-string (current-screen) (run-shell-command "mifo --prev" t)) (echo-mifo-stumpwm))
(defcommand echo-oss-vol () () (echo-string (current-screen) (run-shell-command "ossvol -a" t)))
(defcommand echo-oss-volup () () (run-shell-command "ossvol -i 1" t) (echo-oss-vol))
(defcommand echo-oss-voldown () () (run-shell-command "ossvol -d 1" t) (echo-oss-vol))
(defcommand echo-oss-volmute () () (run-shell-command "ossvol -m" t))
(defcommand echo-oss-speakers () () (echo-string (current-screen) (run-shell-command "ossvol --speakers --quiet" t)) (echo-oss-vol))
(defcommand echo-oss-headphones () () (echo-string (current-screen) (run-shell-command "ossvol --headphones --quiet" t)) (echo-oss-vol))
(defcommand echo-mail () () (echo-string (current-screen) (run-shell-command "print - @fea.st: ${#$(find /home/milo/mail/FastMail/*/new -type f)}" t)))
(defcommand echo-battery () () (echo-string (current-screen) (run-shell-command "</proc/acpi/battery/BAT1/state" t)))
(defcommand echo-free-hdd () () (echo-string (current-screen) (run-shell-command "df -hTP;print - '------------------------------------------------------';df -hTP --total|tail -1" t)))
(defcommand echo-free-mem () () (echo-string (current-screen) (run-shell-command "print '^B^6/free^1* used^5* base^n';free -m|awk 'NR==2 {print $4,$3,$2}'" t)))
(defcommand echo-highcpu-user () () (echo-string (current-screen) (run-shell-command "ps -U root,privoxy,postgres,named --deselect -C tmux,urxvt k -%cpu opid,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-highcpu-root () () (echo-string (current-screen) (run-shell-command "ps -U milo,privoxy,postgres,named --deselect -C tmux,urxvt k -%cpu opid,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-highcpu-rest () () (echo-string (current-screen) (run-shell-command "ps -U root,milo --deselect -C tmux,urxvt k -%cpu opid,args:70,etime:10,%cpu,pmem | head -75" t)))
(defcommand echo-loadavg () () (echo-string (current-screen) (run-shell-command "print ${$(</proc/loadavg)[1,3]}" t)))
(defcommand echo-colors-brief () () (echo-string (current-screen) (eval "
BOLD ^B^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
NONE ^0*black ^1*red ^2*green ^3*yellow ^4*blue ^5*magenta ^6*cyan ^7*white ^8*user ^9*user^n
")))
(defcommand echo-colors-full () () (echo-string (current-screen) (eval "
^n^B^0*black   ^B^00 B00 ^B^01 B01 ^B^02 B02 ^B^03 B03 ^B^04 B04 ^B^05 B05 ^B^06 B06 ^B^07 B07 ^B^08 B08 ^B^09 B09 
^n^n^0*black   ^n^00 N00 ^n^01 N01 ^n^02 N02 ^n^03 N03 ^n^04 N04 ^n^05 N05 ^n^06 N06 ^n^07 N07 ^n^08 N08 ^n^09 N09 
^n^B^1*red     ^B^10 B10 ^B^11 B11 ^B^12 B12 ^B^13 B13 ^B^14 B14 ^B^15 B15 ^B^16 B16 ^B^17 B17 ^B^18 B18 ^B^19 B19 
^n^n^1*red     ^n^10 N10 ^n^11 N11 ^n^12 N12 ^n^13 N13 ^n^14 N14 ^n^15 N15 ^n^16 N16 ^n^17 N17 ^n^18 N18 ^n^19 N19 
^n^B^2*green   ^B^20 B20 ^B^21 B21 ^B^22 B22 ^B^23 B23 ^B^24 B24 ^B^25 B25 ^B^26 B26 ^B^27 B27 ^B^28 B28 ^B^29 B29 
^n^n^2*green   ^n^20 N20 ^n^21 N21 ^n^22 N22 ^n^23 N23 ^n^24 N24 ^n^25 N25 ^n^26 N26 ^n^27 N27 ^n^28 N28 ^n^29 N29 
^n^B^3*yellow  ^B^30 B30 ^B^31 B31 ^B^32 B32 ^B^33 B33 ^B^34 B34 ^B^35 B35 ^B^36 B36 ^B^37 B37 ^B^38 B38 ^B^39 B39 
^n^n^3*yellow  ^n^30 N30 ^n^31 N31 ^n^32 N32 ^n^33 N33 ^n^34 N34 ^n^35 N35 ^n^36 N36 ^n^37 N37 ^n^38 N38 ^n^39 N39 
^n^B^4*blue    ^B^40 B40 ^B^41 B41 ^B^42 B42 ^B^43 B43 ^B^44 B44 ^B^45 B45 ^B^46 B46 ^B^47 B47 ^B^48 B48 ^B^49 B49 
^n^n^4*blue    ^n^40 N40 ^n^41 N41 ^n^42 N42 ^n^43 N43 ^n^44 N44 ^n^45 N45 ^n^46 N46 ^n^47 N47 ^n^48 N48 ^n^49 N49 
^n^B^5*magenta ^B^50 B50 ^B^51 B51 ^B^52 B52 ^B^53 B53 ^B^54 B54 ^B^55 B55 ^B^56 B56 ^B^57 B57 ^B^58 B58 ^B^59 B59 
^n^n^5*magenta ^n^50 N50 ^n^51 N51 ^n^52 N52 ^n^53 N53 ^n^54 N54 ^n^55 N55 ^n^56 N56 ^n^57 N57 ^n^58 N58 ^n^59 N59 
^n^B^6*cyan    ^B^60 B60 ^B^61 B61 ^B^62 B62 ^B^63 B63 ^B^64 B64 ^B^65 B65 ^B^66 B66 ^B^67 B67 ^B^68 B68 ^B^69 B69 
^n^n^6*cyan    ^n^60 N60 ^n^61 N61 ^n^62 N62 ^n^63 N63 ^n^64 N64 ^n^65 N65 ^n^66 N66 ^n^67 N67 ^n^68 N68 ^n^69 N69 
^n^B^7*white   ^B^70 B70 ^B^71 B71 ^B^72 B72 ^B^73 B73 ^B^74 B74 ^B^75 B75 ^B^76 B76 ^B^77 B77 ^B^78 B78 ^B^79 B79 
^n^n^7*white   ^n^70 N70 ^n^71 N71 ^n^72 N72 ^n^73 N73 ^n^74 N74 ^n^75 N75 ^n^76 N76 ^n^77 N77 ^n^78 N78 ^n^79 N79 
^n^B^8*color8  ^B^80 B80 ^B^81 B81 ^B^82 B82 ^B^83 B83 ^B^84 B84 ^B^85 B85 ^B^86 B86 ^B^87 B87 ^B^88 B88 ^B^89 B89 
^n^n^8*color8  ^n^80 N80 ^n^81 N81 ^n^82 N82 ^n^83 N83 ^n^84 N84 ^n^85 N85 ^n^86 N86 ^n^87 N87 ^n^88 N88 ^n^89 N89 
^n^B^9*color9  ^B^90 B90 ^B^91 B91 ^B^92 B92 ^B^93 B93 ^B^94 B94 ^B^95 B95 ^B^96 B96 ^B^97 B97 ^B^98 B98 ^B^99 B99 
^n^n^9*color9  ^n^90 N90 ^n^91 N91 ^n^92 N92 ^n^93 N93 ^n^94 N94 ^n^95 N95 ^n^96 N96 ^n^97 N97 ^n^98 N98 ^n^99 N99 
")))

;; sent output of command to echo-string. may hang if used wrong.
(defcommand shell-command-output (command) ((:string "shell/output: "))
  (check-type command string) (shell-command command))

;; prompt with given arg as command, and if needed await further args, and execute.
(defcommand pine (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (run-shell-command cmd t))))

;; same as 'pine' but send output to echo-string. may hang if used wrong.
(defcommand pout (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd (shell-command-output cmd))))

;; manpage reader. needs filename completion..
(defcommand manpage (command) ((:rest "manpage: "))
  (run-shell-command (format nil "urxvt -e man ~a" command)))

;; surfraw (copyright (C) 2008 Ivy Foster).
;; would rather not use the module as everything is predefined, etc.
(defcommand surfraw (engine search)
  ((:string "What engine? ") (:string "Search for what? "))
  (check-type engine string)
  (check-type search string)
  (run-shell-command (concatenate 'string "exex surfraw -g " engine " " search)))
(defcommand sr-bookmark (bmk) ((:string "Bookmark: "))
  (surfraw "" bmk))
(defcommand sr-bookmark-file-display () ()
  (display-file *surfraw-bookmark-file*))

;; surfraw engines i want (from surfraw-git package).
(defcommand amazon (search) ((:string "Search Amazon: ")) (surfraw "amazon" search))
(defcommand aur (search) ((:string "Search the AUR: ")) (surfraw "aur" search))
(defcommand cliki (search) ((:string "Search CLiki: ")) (surfraw "cliki" search))
(defcommand codesearch (search) ((:string "Search CodeSearch: ")) (surfraw "codesearch" search))
(defcommand ebay (search) ((:string "Search eBay: ")) (surfraw "ebay" search))
(defcommand google (search) ((:string "Search Google: ")) (surfraw "google" search))
(defcommand ixsearch (search) ((:string "Search ixsearch: ")) (surfraw "ixsearch" search))
(defcommand lastfm (search) ((:string "Search LastFM: ")) (surfraw "lastfm" search))
(defcommand piratebay (search) ((:string "Search the Pirate Bay: ")) (surfraw "piratebay" search))
(defcommand slashdot (search) ((:string "Search SlashDot: ")) (surfraw "slashdot" search))
(defcommand sourceforge (search) ((:string "Search SourceForge: ")) (surfraw "sourceforge" search))
(defcommand wikipedia (search) ((:string "Search Wikipedia: ")) (surfraw "wikipedia" search))
(defcommand youtube (search) ((:string "Search YouTube: ")) (surfraw "youtube" search))
;; surfraw engines (from personal made elvis).
(defcommand googlessl (search) ((:string "Search GoogleSSL: ")) (surfraw "googlessl" search))
(defcommand kickass (search) ((:string "Search KickAssTorrents: ")) (surfraw "kickass" search))

;; i don't like 'Colon' showing/editable command in prompt
;; perhaps i'll figure out a global macro/function for this..
(defcommand prompt-mifo-command (filename) ((:rest "mifo.command: "))
  (run-shell-command (format nil "mifo --command ~a" filename)))
(defcommand prompt-mifo-next (filename) ((:rest "mifo.next: "))
  (run-shell-command (format nil "mifo --next ~a" filename)) (echo-mifo-stumpwm))
(defcommand prompt-mifo-prev (filename) ((:rest "mifo.previous: "))
  (run-shell-command (format nil "mifo --prev ~a" filename)) (echo-mifo-stumpwm))
(defcommand prompt-mifo-save (filename) ((:rest "mifo.save-as: "))
  (echo-string (current-screen) (run-shell-command (format nil "mifo --save ~a" filename) t)))
(defcommand prompt-mifo-load (filename) ((:rest "mifo.load: "))
  (run-shell-command (format nil "mifo --load ~a" filename)))
(defcommand prompt-mifo-append (filename) ((:rest "mifo.append: "))
  (run-shell-command (format nil "mifo --append ~a" filename)))
(defcommand prompt-mifo-playlist (filename) ((:rest "mifo.playlist: "))
  (run-shell-command (format nil "mifo --playlist ~a" filename)))
(defcommand prompt-mifo-reload (filename) ((:rest "mifo.reload: "))
  (run-shell-command (format nil "mifo --reload ~a" filename)))

;; evaluate string.
(defcommand eval-line (cmd) ((:rest "eval: "))
  (handler-case
    (message "^B^50~{~a~^~%~}"
      (mapcar 'prin1-to-string
        (multiple-value-list (eval (read-from-string cmd)))))
    (error (c)
      (err "^B^1*~A" c))))

;; run or raise.
(defcommand ror_jumanji () () (setf *run-or-raise-all-groups* t) (run-or-raise "jumanji" '(:class "Jumanji")))
(defcommand ror_mutt () () (setf *run-or-raise-all-groups* nil)
  (run-or-raise "urxvt -e mutt -F ${XDG_CONFIG_DIR:-${HOME}/.config}/mutt/muttrc" '(:title "mutt")))

;; select a random background image.
(defcommand display-random-bg () () (run-shell-command
  (concatenate 'string "display -window root -resize 1366x768! " (select-random-bg-image))))

