;;; bomb.el --- Bomb manual repl -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Joseph Morag
;;
;; Author: Joseph Morag <jm@josephmorag.com>
;; Maintainer: Joseph Morag <jm@josephmorag.com>
;; Created: June 30, 2024
;; Modified: June 30, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/jmorag/hbomb-repl
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Elisp version of hbomb-repl
;;
;;; Code:

(require 'transient)
(require 'dash)

(defclass bomb--variable-choices (transient-lisp-variable)
  (choices :initarg :choices))

(cl-defmethod transient-infix-read ((obj bomb--variable-choices))
  "Adapted from magit--git-variable:choices."
  (let ((choices (oref obj choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-format-value ((obj bomb--variable-choices))
  "Adapted from magit--git-variable:choices."
  (let ((value (oref obj value))
        (choices (oref obj choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face
                    (if (equal choice value)
                        'transient-value 'transient-inactive-value)))
      choices
      (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(defvar bomb--serial-parity nil)
(transient-define-infix bomb--serial-parity-state ()
  :variable 'bomb--serial-parity
  :choices '("even" "odd")
  :class 'bomb--variable-choices)

(defun bomb--ask-serial-odd ()
  "Determine if the last digit of the serial number is odd."
  (pcase bomb--serial-parity
    ("even" nil)
    ("odd" t)
    (`nil (let* ((answer (y-or-n-p "Is the last digit of the serial number odd?"))
                 (value (if answer "odd" "even")))
            (setq bomb--serial-parity value)
            ;; Update the transient to reflect that we know new stuff now
            (transient--refresh-transient)
            answer))))

(defun bomb--simple-wires ()
  "Dialog for simple wires."
  (interactive)
  (let ((wires (completing-read-multiple "Wires: " '("blue" "black" "white" "yellow" "red") nil t)))
    (cl-flet ((n (color) (-count (lambda (wire) (string= wire color)) wires))
              (cut (nth) (message "Cut the %s wire" nth)))
      (pcase wires
        (`(,_ ,_ ,w3)
         (cond
          ((= (n "red") 0) (cut "second"))
          ((string= w3 "white") (cut "last"))
          ((> (n "blue") 1) (cut "last blue"))
          (t (cut "last"))))
        (`(,_ ,_ ,_ ,w4)
         (cond
          ((and (> (n "red") 1) (bomb--ask-serial-odd)) (cut "second"))
          ((string= w4 "yellow") (cut "first"))
          ((= (n "blue") 1) (cut "first"))
          ((> (n "yellow") 1) (cut "last"))
          (t (cut "second"))))
        (`(,_ ,_ ,_ ,_ ,w5)
         (cond
          ((and (string= w5 "black") (bomb--ask-serial-odd)) (cut "fourth"))
          ((and (= (n "red") 1) (> (n "yellow") 1)) (cut "first"))
          ((= (n "black") 0) (cut "second"))
          (t (cut "first"))))
        (`(,_ ,_ ,_ ,_ ,_ ,_)
         (cond
          ((and (= (n "yellow") 0) (bomb--ask-serial-odd)) (cut "third"))
          ((and (= (n "yellow") 1) (> (n "white") 1)) (cut "fourth"))
          ((= (n "red") 0) (cut "last"))
          (t (cut "fourth"))))
        (_ (message "Bomb must have 3, 4, 5, or 6 wires"))))))

(transient-define-prefix bomb ()
  "Top level bomb manual interface."
  ["State"
   ("s" "Parity of the last digit of the serial number" bomb--serial-parity-state)]
  ["Modules"
   ("w" "Simple wires" bomb--simple-wires :transient t)])

(provide 'bomb)
;;; bomb.el ends here
