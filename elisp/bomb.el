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
  ;; Read the value out of the variable, not the value slot, because it gets
  ;; changed by the bomb-ask-* functions and we want the formatting to reflect
  ;; that.
  (let ((value (symbol-value (oref obj variable)))
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

;; Bomb state

(defvar bomb--serial-parity nil)
(transient-define-infix bomb--serial-parity-state ()
  :variable 'bomb--serial-parity
  :choices '("even" "odd")
  :class 'bomb--variable-choices)

(defvar bomb--serial-vowel nil)
(transient-define-infix bomb--serial-vowel-state ()
  :variable 'bomb--serial-vowel
  :choices '("yes" "no")
  :class 'bomb--variable-choices)

(defvar bomb--batteries nil)
(transient-define-infix bomb--batteries-state ()
  :variable 'bomb--batteries
  :choices '("0 or 1" "2" ">=3")
  :class 'bomb--variable-choices)

(defvar bomb--car nil)
(transient-define-infix bomb--car-state ()
  :variable 'bomb--car
  :choices '("yes" "no")
  :class 'bomb--variable-choices)

(defvar bomb--frk nil)
(transient-define-infix bomb--frk-state ()
  :variable 'bomb--frk
  :choices '("yes" "no")
  :class 'bomb--variable-choices)

(defvar bomb--parallel nil)
(transient-define-infix bomb--parallel-state ()
  :variable 'bomb--parallel
  :choices '("yes" "no")
  :class 'bomb--variable-choices)

(defun bomb--reset ()
  "Reset the bomb state."
  (interactive)
  (setq bomb--serial-parity nil)
  (setq bomb--serial-vowel nil)
  (setq bomb--batteries nil)
  (setq bomb--car nil)
  (setq bomb--frk nil)
  (setq bomb--parallel nil))

(defun bomb--ask-serial-odd ()
  "Determine if the last digit of the serial number is odd."
  (pcase bomb--serial-parity
    ("even" nil)
    ("odd" t)
    (`nil (let* ((answer (y-or-n-p "Is the last digit of the serial number odd?"))
                 (value (if answer "odd" "even")))
            (setq bomb--serial-parity value)
            answer))))

(defun bomb--ask-serial-vowel ()
  "Determine if the serial number has a vowel."
  (interactive)
  (pcase bomb--serial-vowel
    ("yes" t)
    ("no" nil)
    (`nil (let* ((answer (y-or-n-p "Does the serial number have a vowel?"))
                 (value (if answer "yes" "no")))
            (setq bomb--serial-vowel value)
            answer))))

(defun bomb--ask-batteries ()
  "Determine how many batteries the bomb has."
  (interactive)
  (pcase bomb--batteries
    ("0 or 1" 0)
    ("2" 2)
    (">=3" 3)
    (`nil (let* ((answer (- (read-char-choice "How many batteries does the bomb have? " '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)) ?0))
                 (value (pcase answer
                          (0 "0 or 1")
                          (1 "0 or 1")
                          (2 "2")
                          (_ ">=3"))))
            (setq bomb--batteries value)
            answer))))

(defun bomb--ask-car ()
  "Determine if the bomb has a lit indicator CAR."
  (interactive)
  (pcase bomb--car
    ("yes" t)
    ("no" nil)
    (`nil (let* ((answer (y-or-n-p "Does the bomb have a lit indicator CAR?"))
                 (value (if answer "yes" "no")))
            (setq bomb--car value)
            answer))))

(defun bomb--ask-frk ()
  "Determine if the bomb has a lit indicator FRK."
  (interactive)
  (pcase bomb--frk
    ("yes" t)
    ("no" nil)
    (`nil (let* ((answer (y-or-n-p "Does the bomb have a lit indicator FRK?"))
                 (value (if answer "yes" "no")))
            (setq bomb--frk value)
            answer))))

(defun bomb--ask-parallel ()
  "Determine if the bomb has a parallel port."
  (interactive)
  (pcase bomb--parallel
    ("yes" t)
    ("no" nil)
    (`nil (let* ((answer (y-or-n-p "Does the bomb have a parallel port?"))
                 (value (if answer "yes" "no")))
            (setq bomb--parallel value)
            answer))))

;; Module logic
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

(defun bomb--button-compute ()
  "Dialog for button."
  (interactive)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (color (transient-arg-value "--color=" args))
         (text (transient-arg-value "--text=" args)))
    ;; Infix args are not required by the transient, so we might need to ask again for them.
    (cl-flet* ((color () (or color
                             (let ((answer (completing-read "What color is the button? " '("blue" "white" "yellow" "red"))))
                               (setq color answer)
                               answer)))
               (text () (or text
                            (let ((answer (completing-read "What text is on the button? " '("abort" "detonate" "hold" "press"))))
                              (setq text answer)
                              answer)))
               (press () (message "Press and immediately release the button"))
               (hold () (message
                         (concat
                          "Hold the button"
                          "\n  Blue strip: release when the countdown timer has a 4 in any position"
                          "\n  Yellow strip: release when the countdown timer has a 5 in any position"
                          "\n  Any other color strip: release when the countdown timer has a 1 in any position"))))
      (cond
       ((and (equal (text) "abort") (equal (color) "blue")) (hold))
       ((and (equal (text) "detonate") (>= (bomb--ask-batteries) 2)) (press))
       ((and (equal (color) "white") (bomb--ask-car)) (hold))
       ((and (> (bomb--ask-batteries) 2) (bomb--ask-frk)) (press))
       ((equal (color) "yellow") (hold))
       ((and (equal (text) "hold") (equal (color) "red")) (press))
       (t (hold))))))

(transient-define-prefix bomb--button ()
  ["Button arguments"
   ("t" "Text" "--text=" :choices ("abort" "detonate" "hold" "press") :always-read t)
   ("c" "Color" "--color=" :choices ("blue" "white" "yellow" "red") :always-read t)]
  ["Actions"
   ("r" "Run" bomb--button-compute :transient nil)
   ("q" "Back" (lambda () (interactive) nil) :transient nil)])

(defun bomb--complicated-wires-compute ()
  "Dialog for complicated wires."
  (interactive)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (red (transient-arg-value "--red" args))
         (blue (transient-arg-value "--blue" args))
         (star (transient-arg-value "--star" args))
         (led (transient-arg-value "--led" args)))
    (cl-flet
        ((cut () (message "Cut the wire"))
         (dont () (message "Do NOT cut the wire")))
      (pcase (list red blue star led)
        (`(t t t t) (dont))
        (`(t t t nil) (if (bomb--ask-parallel) (cut) (dont)))
        (`(t t nil t) (if (not (bomb--ask-serial-odd)) (cut) (dont)))
        (`(t t nil nil) (if (not (bomb--ask-serial-odd)) (cut) (dont)))
        (`(t nil t t) (if (>= (bomb--ask-batteries) 2) (cut) (dont)))
        (`(t nil t nil) (cut))
        (`(t nil nil t) (if (>= (bomb--ask-batteries) 2) (cut) (dont)))
        (`(t ni nil nil) (if (not (bomb--ask-serial-odd)) (cut) (dont)))
        (`(nil t t t) (if (bomb--ask-parallel) (cut) (dont)))
        (`(nil t t nil) (dont))
        (`(nil t nil t) (if (bomb--ask-parallel) (cut) (dont)))
        (`(nil t nil nil) (if (not (bomb--ask-serial-odd)) (cut) (dont)))
        (`(nil nil t t) (if (>= (bomb--ask-batteries) 2) (cut) (dont)))
        (`(nil nil t nil) (cut))
        (`(nil nil nil t) (dont))
        (`(nil nil nil nil) (cut))))))

(transient-define-prefix bomb--complicated-wires ()
  ["Wire properties"
   [("d" "Red" "--red")
    ("b" "Blue" "--blue")
    ("s" "Star" "--star")
    ("l" "LED" "--led")]]
  ["Actions"
   ("r" "Run" bomb--complicated-wires-compute :transient t)
   ("q" "Back" (lambda () (interactive) nil) :transient nil)])

(transient-define-prefix bomb ()
  "Top level bomb manual interface."
  ["State"
   [("-s" "Parity of the last digit of the serial number" bomb--serial-parity-state)
    ("-v" "Does the serial number have a vowel?" bomb--serial-vowel-state)
    ("-b" "Number of batteries" bomb--batteries-state)
    ("-c" "Is there a lit indicator with the label CAR?" bomb--car-state)
    ("-f" "Is there a lit indicator with the label FRK?" bomb--frk-state)
    ("-p" "Does the bomb have a parallel port?" bomb--parallel-state)]]
  ["Modules"
   ("w" "Simple wires" bomb--simple-wires :transient t)
   ("b" "Button" bomb--button :transient t)
   ("c" "Complicated wires" bomb--complicated-wires :transient t)]
  ["Misc"
   ("-r" "Reset the bomb state" bomb--reset :transient t)
   ("q" "Exit this dialog" (lambda () (interactive) nil) :transient nil)]
  (interactive)
  (bomb--reset)
  (transient-setup 'bomb))

(provide 'bomb)
;;; bomb.el ends here
