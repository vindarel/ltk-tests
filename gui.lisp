
;; quickload ltk before.
;; we also use nodgui below.
(defpackage ltk-first-example
  (:use :cl
        :ltk))

(in-package :ltk-first-example)

(defun calculate (feet-widget meter-widget)
  (let ((feet-input (text feet-widget)))
    (when (and feet-input (str:non-blank-string-p feet-input))
      (log:info feet-input)
      (setf (text meter-widget) (format nil "~,2F" (*
                                                    (read-from-string feet-input)
                                                    0.3048d0))))))

(defun gui ()
  (let ((ltk:*debug-tk* t))
  (with-ltk ()
    (wm-title *tk* "Feet to Meters")
    (let ((c (make-instance 'frame)))
      (grid c 0 0 :sticky "ne")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)
      (let* ((c.feet (grid (make-instance 'entry :width 7)
                           1 2 :sticky "we" :padx 5 :pady 5))
             (c.meters (grid (make-instance 'entry :state "readonly")
                             2 2 :sticky "we" :padx 5 :pady 5)))
        (grid (make-instance 'button
                             :text "Calculate"
                             :command (lambda () (calculate c.feet c.meters)))
              3 3 :sticky "w" :padx 5 :pady 5)
        (grid (make-instance 'label :text "feet")
              1 3 :sticky "w" :padx 5 :pady 5)
        (grid (make-instance 'label :text "is equivalent to")
              2 1 :sticky "w" :padx 5 :pady 5)
        (grid (make-instance 'label :text "meters")
2 3 :sticky "w" :padx 5 :pady 5))))))


(defun tree ()
  (let ((ltk:*debug-tk* t)
        (dummy-data '("foo"
                      "bar"
                      "team"
                      )))
    (with-ltk ()
      (wm-title *tk* "A tree")
      (let ((c (make-instance 'frame)))
        (grid c 0 0 :sticky "ne")
        (grid-columnconfigure *tk* 0 :weight 1)
        (grid-rowconfigure *tk* 0 :weight 1)
        (let* ((c.searchbox (grid (make-instance 'entry :width 7)
                                  0 0 :sticky "we" :padx 5 :pady 5))
               (c.tree (grid (make-instance 'scrolled-treeview
                                            :columns (list "col1" "col2"))
                             1 0 :sticky "we" :padx 5 :pady 5)))
          (loop for row in dummy-data
             do (treeview-insert c.tree
                                 :text row
                                 :column-values (list "two")))
          (grid (make-instance 'button
                               :text "foo"
                               :command (lambda ()
                                          (format t "the treeview selection is: ~a~&"
                                                  (treeview-get-selection c.tree))
                                          (format t "text is: ~a~&" (text c.searchbox))))
                2 0 :sticky "w" :padx 5 :pady 5)
          )))))

(defun treeview-with-columns ()
  ;; With nodgui (because of the examples I have at hand.
  ;; Its api is different sometimes (he says):
  ;; - we use scrolled-treeview, says that treeview is broken.
  ;; - it's treeview-insert-<<item>>.
  (with-nodgui ()
    (let ((c (make-instance 'frame)))
      (grid c 0 0 :sticky "ne")
      (let* ((c.searchbox (grid (make-instance 'entry :width 7)
                                0 0 :sticky "we" :padx 5 :pady 5))
             (c.tree (grid (make-instance 'scrolled-treeview
                                          :columns (list "col1" "col2"))
                           1 0 :sticky "we" :padx 5 :pady 5)))
        (loop for row in '("aaa")
           do (treeview-insert-item c.tree
                                    ;; text of the first column:
                                    :text row
                                    ;; text of the other columns:
                                    :column-values (list "two" "three")))
        (grid (make-instance 'button
                             :text "foo"
                             :command (lambda ()
                                        (format t "the treeview selection is: ~a~&"
                                                (treeview-get-selection c.tree))
                                        (format t "text is: ~a~&" (text c.searchbox))))
              2 0 :sticky "w" :padx 5 :pady 5)))))

(defpackage nodgui-examples
  (:use :cl
        :nodgui))

(in-package :nodgui-examples)

(defun example-treeview-with-columns-simplest ()
  (with-nodgui ()
    (let ((tree  (make-instance 'scrolled-treeview
                                :columns (list "col2"))))
      ;; the configuration to set the title of the firts column:
      (treeview-heading tree
                        +treeview-first-column-id+
                        :text "col1")
      ;; place our widget, or we won't see it.
      (grid tree 1 0)
      (loop for data in '("aaa" "bbb" "ccc")
         do (treeview-insert-item tree
                                  :text data
                                  :column-values (list "val2"))))))

(defun example-treeview-search-display ()
  "A searchbox, a button, and display results in the treeview.
   We did it in nodgui: for the examples at hand, and the author's support."
  (with-nodgui ()
    ;; Windows' title.
    (wm-title *tk* "My GUI")
    (let* ((tree (make-instance 'scrolled-treeview
                                ;; These are the second and third columns.
                                :columns (list "col2"
                                               "col3")))
           (searchbox (grid (make-instance 'entry :width 7)
                            0 0 :sticky "we" :padx 5 :pady 5))
           (button
            (make-instance
             'button
             :text "OK"
             :command (lambda ()
                        (format t "the treeview selection is: ~a~&"
                                (treeview-get-selection tree))
                        (format t "text is: ~a~&" (text searchbox))
                        (insert-results tree
                                        (list "hey"
                                              "that's cool"
                                              (format nil "you searched: ~a" (text searchbox))))))))

      ;; Name the first column:
      (treeview-heading tree +treeview-first-column-id+ :text "col1")
      ;; For resizing to do something: weight > 0
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid searchbox 0 0
            ;; it goes below the button :S
            :columnspan 2)
      (grid button 0 1
            ;; stick to the right (east).
            :sticky "e")
      (grid tree 1 0
            ;; so the button doesn't have a column by itself.
            :columnspan 2
            ;; sticky by all sides, for resizing to do something.
            :sticky "nsew"))))

(defun insert-results (tree results)
  "Insert results into that treeview. Clear contents beforehand."
  ;; Clear content.
  ;; this needs nodgui newer than feb, 24th 2019
  ;; with commit c9ae0ec389.
  (treeview-delete-all tree)
  (loop for result in results
     do (treeview-insert-item tree
                              :text result
                              :column-values (list (format nil "length: ~a" (length result))
                                                   (format nil "first letter: ~a" (aref result 0))))))
