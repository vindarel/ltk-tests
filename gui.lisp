
(defpackage ltk-first-example
  (:use "CL"
        "LTK"))

(in-package :ltk-first-example)


(defun calculate (feet-widget meter-widget)
  (setf (text meter-widget) (format nil "~,2F" (*
                                                (read-from-string (text feet-widget))
                                                0.3048d0))))

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

(defparameter *dummy* '(
                        "foo"
                        "bar"
                        "team"
                        ))

(defun tree ()
  (let ((ltk:*debug-tk* t))
    (with-ltk ()
      (wm-title *tk* "A tree")
      (let ((c (make-instance 'frame)))
        (grid c 0 0 :sticky "ne")
        (grid-columnconfigure *tk* 0 :weight 1)
        (grid-rowconfigure *tk* 0 :weight 1)
        (let* ((c.searchbox (grid (make-instance 'entry :width 7)
                                  0 0 :sticky "we" :padx 5 :pady 5))
               (c.tree (grid (make-instance 'treeview)
                             1 0 :sticky "we" :padx 5 :pady 5)))
          (loop for row in *dummy*
               do (treeview-insert c.tree :text row))
          (grid (make-instance 'button
                               :text "foo"
                               :command (lambda ()
                                          (format t "the treeview selection is: ~a~&"
                                                  (treeview-get-selection c.tree))
                                          (format t "text is: ~a~&" (text c.searchbox))))
                2 0 :sticky "w" :padx 5 :pady 5)
          )))))
