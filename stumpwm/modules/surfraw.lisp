;; SURFRAW module for StumpWM.
;;
;; Copyright (C) 2008 Ivy Foster
;;
;; Maintainer: Ivy Foster
;;
;; This module is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;;; Commentary:
;;
;; I like surfraw (http://surfraw.alioth.debian.org). If you're
;; reading this, you probably like surfraw. I've got surfraw-related
;; code in my .stumpwmrc, and (judging from some judicious googling
;; for RC files early on in my use of stumpwm) I know that I'm not the
;; only one. So it seemed like a good idea to just put that code in
;; a library.

;;; Usage:
;;
;; Just add the following line to your .stumpwmrc file:
;;
;; (load "/path/to/stumpwm/contrib/surfraw.lisp")
;;
;; ...and then either call the functions here with "colon" (C-t ;) or
;; bind them to a key. I figure other people will probably have
;; different key preferences than I have, so I leave them entirely up
;; to you.
;;
;; If you want to use the bookmark functions, don't forget to tell
;; stumpwm where your *surfraw-bookmark-file* is.
;;
;; Note that there are also "surfraw-selection" variants on each
;; command that work on the X selection.

;;; FIXME:
;;
;; - Not all elvi are supported yet. Do they need to be?
;; - It would be pretty cool to have a macro like the
;;   surfraw-selection one but for regular surfraw commands.

;;; Code:

;;; Regular surfraw commands

(defcommand surfraw (engine search) 
  ((:string "What engine? ") (:string "Search for what? "))
  "Use SURFRAW to surf the net; reclaim heathen lands."
  (check-type engine string)
  (check-type search string)
  (run-shell-command (concat "exec surfraw -g " engine " " search)))

(defcommand amazon (search) 
  ((:string "Search Amazon: "))
  (surfraw "amazon" search))

(defcommand aur (search)
  ((:string "Search the AUR: "))
  (surfraw "aur" search))

(defcommand cliki (search) 
  ((:string "Search CLiki: "))
  (surfraw "cliki" search))

(defcommand codesearch (search) 
  ((:string "Search GoogleCode: "))
  (surfraw "codesearch" search))

(defcommand cnn (search)
  ((:string "Search CNN: "))
  (surfraw "cnn" search))

(defcommand deja (search)
  ((:string "Search the usenet: "))
  (surfraw "deja" search))

(defcommand ebay (search)
  ((:string "Search eBay: "))
  (surfraw "ebay" search))

(defcommand google (search) 
  ((:string "Search Google: "))
  (surfraw "google" search))

(defcommand ixsearch (search) 
  ((:string "Search ixsearch: "))
  (surfraw "ixsearch" search))

(defcommand lastfm (search)
  ((:string "Search LastFM: "))
  (surfraw "lastfm" search))

(defcommand piratebay (search) 
  ((:string "Search the Pirate Bay: "))
  (surfraw "piratebay" search))

(defcommand slashdot (search) 
  ((:string "Search SlashDot: "))
  (surfraw "slashdot" search))

(defcommand sourceforge (search)
  ((:string "Search SourceForge: "))
  (surfraw "freshmeat" search))

(defcommand thesaurus (search) 
  ((:string "Search a thesaurus: "))
  (surfraw "thesaurus" search))

(defcommand wayback (search) 
  ((:string "Search wayback: "))
  (surfraw "wayback" search))

(defcommand webster (search) 
  ((:string "Search the M-W Dictionary: "))
  (surfraw "webster" search))

(defcommand wikipedia (search) 
  ((:string "Search Wikipedia: "))
  (surfraw "wikipedia" search))

(defcommand youtube (search) 
  ((:string "Search YouTube: "))
  (surfraw "youtube" search))

;;; X selection

(defmacro surfraw-selection (name engine)
  `(defcommand ,name () ()
     (surfraw ,engine (get-x-selection))))

(surfraw-selection alioth-selection    "alioth")
(surfraw-selection amazon-selection    "amazon")
(surfraw-selection archpkg-selection   "archpkg")
(surfraw-selection bbcnews-selection   "bbcnews")
(surfraw-selection cddb-selection      "cddb")
(surfraw-selection cnn-selection       "cnn")
(surfraw-selection debbugs-selection   "debbugs")
(surfraw-selection deja-selection      "deja")
(surfraw-selection ebay-selection      "ebay")
(surfraw-selection etym-selection      "etym")
(surfraw-selection freebsd-selection   "freebsd")
(surfraw-selection freshmeat-selection "freshmeat")
(surfraw-selection genpkg-selection    "genpkg")
(surfraw-selection google-selection    "google")
(surfraw-selection thesaurus-selection "thesaurus")
(surfraw-selection wayback-selection   "wayback")
(surfraw-selection webster-selection   "webster")
(surfraw-selection wikipedia-selection "wikipedia")

;;; Bookmarks

(defun display-file (file)
  "Display a file in the message area."
  (if (probe-file file)
      (run-shell-command (concat "cat " file) t)
    (message "The file ~a does not exist." file)))

(defvar *surfraw-bookmark-file* nil
  "The surfraw bookmark file")

(defcommand sr-bookmark (bmk) ((:string "Bookmark: "))
  (surfraw "" bmk))

(defcommand sr-bookmark-file-display () ()
  (display-file *surfraw-bookmark-file*))

;;; surfraw.lisp ends here
