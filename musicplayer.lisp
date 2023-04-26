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
    (:use :cl :ltk))

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

(defun find-music (&key directory search)
  "Search recursively on this directory for files whose basename contain \"search\".

  Return: list of FOF file objects. Use fof:basename or fof:path to get strings."
  (fof:finder* :root directory :predicates (fof/p:every-path~ search)))
#+(or)
(fof:finder* :root "~/zique/" :predicates (fof/p:every-path~ "Forr" "mp3"))

(defun get-player-name (obj)
  "The value is upper cased when set with Ltk. Return the lowercase value.

  Ex: \"VLC\" (would not work) => \"vlc\"."
  (string-downcase (string obj)))

;; External command: read this file with a media player.
(defun listen (index player notification-label)
  (when (= 1 (length index))
    (let* ((idx (first index))
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
  ;;
  (when (= 1 (length index))
    (let* ((idx (first index))
           ;; (code (nth idx *country-codes*))
           (file (nth idx *data*)))
      (setf (text status-label)
            (format nil "Song: ~s" (normalize-item file)))))
  )

;; GUI:
(defun musicplayer (&optional (data *country-names* data-p))
  (when data-p
    (setf *data* data))
  (with-ltk ()
    (wm-title *tk* "Listbox Example: media player")
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
        #'(lambda (evt) (show-songname (listbox-get-selection data-listbox)
                                         status-label)))
      (bind (listbox data-listbox) "<Double-1>"
        #'(lambda (evt) (listen (listbox-get-selection data-listbox)
                                (value player-1))))
      (bind *tk* "<Return>"
        #'(lambda (evt) (listen (listbox-get-selection data-listbox)
                                (value player-1))))

      (setf (value player-1) "vlc")     ;; gives us upper case.
      (listbox-select data-listbox 0)
      (show-songname (listbox-get-selection data-listbox) status-label)
      ;; alternate colours in listbox
      (dotimes (i (length data))
        (when (evenp i)
          (listbox-configure (listbox data-listbox) i :background "#f0f0ff")))))
  )

#+(or)
(musicplayer (find-music "~/music/" "mp3"))
