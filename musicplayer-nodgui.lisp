;; #!/usr/bin/env ciel

;; Same as musicplayer.lisp, for Ltk, here with nodgui.
;; Goal: use other Tk themes \o/
;;
;; Differences:
;; - in show-songname and listen, the index is given inside a nested list: ((0))
;; instead of (0) in Ltk. We use CAAR instead of FIRST.
;; - use with-nodgui instead of WITH-LTK, and that's it.

;; from https://peterlane.codeberg.page/ltk-examples/#_basic_widgets
;;
;; Simple dumb player:
;; - show list of files
;; - "listen" button.
;;
;; The goal is to display a list of something and make something with it.
;;
;; The original example is a list of countries.
;;
;; Depends-on:
;; FOF (file-object finder)

(uiop:define-package :ltk-tests
    (:shadow :listen)
  (:use :cl
   ;; :ltk))
   :nodgui))

(in-package :ltk-tests)


;; unused
(defparameter *players* '("mpv"
                          "vlc"
                          "smplayer"
                          "clementine"))

(defparameter *data* nil
  "internal cache. List of strings.")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find some files.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-music (directory search)
  "Search recursively on this directory for files whose basename contain \"search\".

  Return: list of FOF file objects. Use fof:basename or fof:path to get strings."
  (let ((str:*ignore-case* t))
    ;; XXX: still a pending MR.
    (fof:finder* :root directory :predicates (fof/p:every-path~ search))))
    ;; (fof:finder* :root directory :predicates (fof/p:path~ search))))
#+(or)
(fof:finder* :root "~/zique/" :predicates (fof/p:every-path~ "Forr" "mp3"))

(defun get-player-name (obj)
  "The value is upper cased when set with Ltk. Return the lowercase value.

  Ex: \"VLC\" (would not work) => \"vlc\"."
  (string-downcase (string obj)))

;; External command: read this file with a media player.
(defun listen (index player notification-label)
  (when (= 1 (length index))
    (let* ((idx (caar index)) ;; Ltk: first
           (player (get-player-name player))
           (item (nth idx *data*)))

      (setf (text notification-label)
            (format nil "Listening to ~a with ~a…" (fof:path item) player))

      ;; Run async:
      (uiop:launch-program (list player (fof:path item))))))

(defun normalize-item (item)
  "In our case: FOF file object -> file name (sans full path)."
  (fof:basename item))

(defun show-songname (index status-label)
  "Updates status label (window bottom)"
  ;; nice status at the window bottom.
  (when (= 1 (length index))
    (let* ((idx (caar index))  ;; in Ltk: "first" is enough. nodgui: caar.
           ;; (code (nth idx *country-codes*))
           (file (nth idx *data*)))
      (setf (text status-label)
            (format nil "Song: ~s" (normalize-item file)))))
  )

;; Adapted from nodgui.lisp
(defun theme-directory (theme)
  "I cloned ttkthemes to this project's root. Some themes are under png/ other under themes/
  The gif/ ones are not yet supported by nodgui
  https://notabug.org/cage/nodgui/issues/13"
  (cond

    ;; Forest
    ;; the light one is good.
    ;; https://github.com/rdbende/Forest-ttk-theme
    ((str:starts-with-p "forest-dark" theme)
     (error "forest-dark is not supported"))
    ((str:starts-with-p "forest-light" theme)
     "Forest-ttk-theme/forest-light.tcl")

    ;; Sun-Valley
    ;; https://github.com/rdbende/Sun-Valley-ttk-theme
    ;; fails to load.
    ((str:starts-with-p "sv_" theme)
     (log:error "Sun-Valley themes do not work, it seems")
     (cond
       ((equal theme "sv_light")
        "Sun-Valley-ttk-theme/theme/light.tcl")
       ((equal theme "sv_dark")
        "Sun-Valley-ttk-theme/theme/dark.tcl")
       (t
        (error "we don't know such Sun-Valley theme. Available: sv_light, sv_dark"))))

    ;; Azure - NOT OK
    ;; https://github.com/rdbende/Azure-ttk-theme
    ;; doesn't render properly.
    ((str:starts-with-p "azure" theme)
     (cond
       ;; from ale_themes
       ((equal theme "azure")
        "ale_themes/azure/azure.tcl")
       ;; azure-ttk-themes
       ((equal theme "azure-light")
        "Azure-ttk-theme/theme/light.tcl")
       ((equal theme "azure-dark")
        "Azure-ttk-theme/theme/dark.tcl")
       (t
        (error "we don't know such Azure them."))))

    ;; Ale themes
    ;; https://github.com/aplsimple/ale_themes/
    ;; OK
    ((str:s-member (list "darkbrown" "lightbrown") theme)
     (cond
       ((equal theme "lightbrown")
        "ale_themes/lightbrown/lightbrown.tcl")
       ((equal theme "darkbrown")
        "ale_themes/darkbrown/darkbrown.tcl")
       (t
        (error "we don't know such Ale them."))))

    ;; ttkthemes
    ;; Good ones.
    ((member theme (list "aquativo"   ;; OK
                         "black"      ;; OK
                         "blue"       ;; OK
                         "clearlooks" ;; OK
                         "elegance"   ;; OK but ugly
                         "plastik"    ;; OK
                         "radiance"   ;; OK
                         )
             :test #'equal)
     (format nil "ttkthemes/ttkthemes/themes/~a/~a.tcl"  theme theme))
    (t
     ;; other themes include:
     ;; adapta (renders OK)
     ;; arc (OK too!!)
     ;; breeze (renders OK)
     ;; equilux (renders !)
     ;; scid (BUGS)
     ;; ubuntu (renders !)
     ;; yaru (renders OK)
     ;;
     ;; in png/ directory.
     (format nil "ttkthemes/ttkthemes/png/~a/~a.tcl"  theme theme))))


;; GUI:
(defun musicplayer (&optional (data *data* data-p) &key (theme "yaru"))
  (when data-p
    (setf *data* data))
  (with-nodgui ()

    ;; (ltk::use-theme "clam")

    ;; (eval-tcl-file "ttkthemes/ttkthemes/png/yaru/yaru.tcl")
    ;; (use-theme "yaru")

    (log:info (theme-directory theme))
    (eval-tcl-file (theme-directory theme))
    (use-theme theme)

    ;; (eval-tcl-file "Forest-ttk-theme/forest-light.tcl")
    ;; (use-theme "forest-dark")

    (wm-title *tk* (format nil "Listbox Example: media player. Theme: ~a" theme))

    ;; create the outer content frame and other widgets
    (let* ((content (make-instance 'frame))
           (data-listbox (make-instance 'scrolled-listbox :master content))
           (player-1 (make-instance 'radio-button :master content
                                                  :text "vlc"
                                                  :value "vlc" :variable "player"))
           (player-2 (make-instance 'radio-button :master content
                                                  :text "mpv"
                                                  :value "mpv" :variable "player"))
           (status-label (make-instance 'label :master content :text "" :anchor "w"))
           (listen-btn (make-instance 'button
                                      :master content :text "Listen"
                                      :command (lambda ()
                                                 (listen (listbox-get-selection data-listbox)
                                                         (value player-1)
                                                         status-label))
                                      :default :active)))

      ;; grid the outer content frame
      (configure content :padding "5 5 12 0")
      (grid content 0 0 :sticky "nwes")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)
      (grid-columnconfigure content 0 :weight 1)
      (grid-rowconfigure content 5 :weight 1)

      ;; grid the other widgets
      (listbox-append data-listbox data)
      (grid data-listbox 0 0 :rowspan 6 :sticky "nsew")
      (grid player-1 1 1 :sticky "w" :padx 20)
      (grid player-2 2 1 :sticky "w" :padx 20)

      (grid listen-btn 5 2 :sticky "se")
      (grid status-label 6 0 :columnspan 2 :sticky "we")

      ;; Set event bindings for when the selection in the listbox changes,
      ;; when the user double clicks the list, and when they hit the Return key
      (bind (listbox data-listbox) "<<ListboxSelect>>"
        #'(lambda (evt)
            (declare (ignorable evt))
            (show-songname (listbox-get-selection data-listbox)
                           status-label)))
      (bind (listbox data-listbox) "<Double-1>"
        #'(lambda (evt)
            (declare (ignorable evt))
            (listen (listbox-get-selection data-listbox)
                    (value player-1)
                    (value player-1))))
      (bind *tk* "<Return>"
        #'(lambda (evt)
            (declare (ignorable evt))
            (listen (listbox-get-selection data-listbox)
                    (value player-1)
                    (value player-1))))

      (setf (value player-1) "vlc") ;; gives us upper case.
      (listbox-select data-listbox 0)
      (show-songname (listbox-get-selection data-listbox) status-label)
      ;; alternate colours in listbox
      (dotimes (i (length data))
        (when (evenp i)
          (listbox-configure (listbox data-listbox) i :background "#f0f0ff")))))
  )

#+(or)
(musicplayer (find-music "~/music/" "mp3"))

#+(or)
(musicplayer (find-music "~/zique/" "forr"))

#+ciel
(musicplayer (find-music "~/zique/" (second ciel-user:*script-args*)))
