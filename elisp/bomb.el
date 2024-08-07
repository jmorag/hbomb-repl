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

(defvar bomb--memory-stage 1)
(defvar bomb--memory-list '() "List of (pos . label) pairs.")

(defun bomb--reset ()
  "Reset the bomb state."
  (interactive)
  (setq bomb--serial-parity nil)
  (setq bomb--serial-vowel nil)
  (setq bomb--batteries nil)
  (setq bomb--car nil)
  (setq bomb--frk nil)
  (setq bomb--parallel nil)
  (setq bomb--memory-stage 1)
  (setq bomb--memory-list nil))

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
   ("RET" "Run" bomb--button-compute :transient nil)
   ("q" "Back" (lambda () (interactive) nil) :transient nil)])

(defun bomb--complicated-wires-compute ()
  "Dialog for complicated wires."
  (interactive)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (red (transient-arg-value "--red" args))
         (blue (transient-arg-value "--blue" args))
         (star (transient-arg-value "--star" args))
         (led (transient-arg-value "--led" args)))
    (cl-flet*
        ((cut () (message "Cut the wire"))
         (dont () (message "Do NOT cut the wire"))
         (parallel () (if (bomb--ask-parallel) (cut) (dont)))
         (serial-even () (if (not (bomb--ask-serial-odd)) (cut) (dont)))
         (two-or-more-batteries () (if (>= (bomb--ask-batteries) 2) (cut) (dont))))
      (pcase (list red blue star led)
        ('(t t t t) (dont))
        ('(t t t nil) (parallel))
        ('(t t nil t) (serial-even))
        ('(t t nil nil) (serial-even))
        ('(t nil t t) (two-or-more-batteries))
        ('(t nil t nil) (cut))
        ('(t nil nil t) (two-or-more-batteries))
        ('(t nil nil nil) (serial-even))
        ('(nil t t t) (parallel))
        ('(nil t t nil) (dont))
        ('(nil t nil t) (parallel))
        ('(nil t nil nil) (serial-even))
        ('(nil nil t t) (two-or-more-batteries))
        ('(nil nil t nil) (cut))
        ('(nil nil nil t) (dont))
        ('(nil nil nil nil) (cut))))))

(transient-define-prefix bomb--complicated-wires ()
  ["Wire properties"
   [("r" "Red" "--red")
    ("b" "Blue" "--blue")
    ("s" "Star" "--star")
    ("l" "LED" "--led")]]
  ["Actions"
   ("RET" "Run" bomb--complicated-wires-compute :transient t)
   ("q" "Back" (lambda () (interactive) nil) :transient nil)])

(defconst bomb--password-list
  '("about"
    "after"
    "again"
    "below"
    "could"
    "every"
    "first"
    "found"
    "great"
    "house"
    "large"
    "learn"
    "never"
    "other"
    "place"
    "plant"
    "point"
    "right"
    "small"
    "sound"
    "spell"
    "still"
    "study"
    "their"
    "there"
    "these"
    "thing"
    "think"
    "three"
    "water"
    "where"
    "which"
    "world"
    "would"
    "write"))

(defun bomb--password-match (input pwd)
  (-all? #'identity
         (-zip-with
          (lambda (pos-letters pwd-char)
            (let ((pos-letters-list (append pos-letters '())))
              (or (not pos-letters-list) (member pwd-char pos-letters-list))))
          input (append pwd '()))))

(defun bomb--passwords-compute ()
  "Dialog for passwords."
  (interactive)
  (let* ((args (transient-args (oref transient-current-prefix command)))
         (l1 (transient-arg-value "--letter-1=" args))
         (l2 (transient-arg-value "--letter-2=" args))
         (l3 (transient-arg-value "--letter-3=" args))
         (l4 (transient-arg-value "--letter-4=" args))
         (l5 (transient-arg-value "--letter-5=" args)))
    (message
     (mapconcat #'identity
                (-filter (lambda (pwd) (bomb--password-match (list l1 l2 l3 l4 l5) pwd))
                         bomb--password-list)
                " "))))

(transient-define-prefix bomb--passwords ()
  ["Letter possibilities"
   [("1" "Letter 1" "--letter-1=")
    ("2" "Letter 2" "--letter-2=")
    ("3" "Letter 3" "--letter-3=")
    ("4" "Letter 4" "--letter-4=")
    ("5" "Letter 5" "--letter-5=")]]
  ["Actions"
   ("RET" "Run" bomb--passwords-compute :transient t)
   ("q" "Back" (lambda () (interactive) nil) :transient nil)])

(defun bomb--read-1-4 (prompt) (- (read-char-choice prompt '(?1 ?2 ?3 ?4)) ?0))

(defun bomb--press-pos (n)
  (let* ((nth (pcase n (1 "1st") (2 "2nd") (3 "3rd") (4 "4th")))
         (message (format "Press the button in the %s position. What is the label? " nth))
         (label (bomb--read-1-4 message)))
    (cl-incf bomb--memory-stage)
    (push (cons n label) bomb--memory-list)))

(defun bomb--press-label (l)
  (let* ((message (format "Press the button labeled %s. What is the position? " l))
         (position (bomb--read-1-4 message)))
    (cl-incf bomb--memory-stage)
    (push (cons position l) bomb--memory-list)))

(defun bomb--get-pos (stage)
  (car (nth (- (length bomb--memory-list) stage) bomb--memory-list)))

(defun bomb--get-label (stage)
  (cdr (nth (- (length bomb--memory-list) stage) bomb--memory-list)))

(defun bomb--press-final (l)
  (message "Press the button labeled %s" l)
  (setq bomb--memory-stage 1)
  (setq bomb--memory-list '()))

(defun bomb--memory-compute ()
  "Dialog for bomb memory."
  (interactive)
  (let ((display-number (bomb--read-1-4 "What is on the display? ")))
    (pcase bomb--memory-stage
      (1 (pcase display-number
           (1 (bomb--press-pos 2))
           (2 (bomb--press-pos 2))
           (3 (bomb--press-pos 3))
           (4 (bomb--press-pos 4))))
      (2 (pcase display-number
           (1 (bomb--press-label 4))
           (2 (bomb--press-pos (bomb--get-pos 1)))
           (3 (bomb--press-pos 1))
           (4 (bomb--press-pos (bomb--get-pos 1)))))
      (3 (pcase display-number
           (1 (bomb--press-label (bomb--get-label 2)))
           (2 (bomb--press-label (bomb--get-label 1)))
           (3 (bomb--press-pos 3))
           (4 (bomb--press-label 4))))
      (4 (pcase display-number
           (1 (bomb--press-pos (bomb--get-pos 1)))
           (2 (bomb--press-pos 1))
           (3 (bomb--press-pos (bomb--get-pos 2)))
           (4 (bomb--press-pos (bomb--get-pos 2)))))
      (5 (pcase display-number
           (1 (bomb--press-final (bomb--get-label 1)))
           (2 (bomb--press-final (bomb--get-label 2)))
           (3 (bomb--press-final (bomb--get-label 4)))
           (4 (bomb--press-final (bomb--get-label 3))))))))

(transient-define-prefix bomb--memory ()
  [:description (lambda ()
                  (format "Current stage: %s%s"
                          bomb--memory-stage
                          (apply
                           #'concat
                           (-map-indexed
                            (lambda (ix pos-label)
                              (format "\n  Stage %s:\n    Position: %s\n    Label: %s"
                                      (- bomb--memory-stage ix 1) (car pos-label) (cdr pos-label)))
                            bomb--memory-list))))
                ("d" "Read display" bomb--memory-compute :transient t)
                ("r" "Reset memory" (lambda () (interactive)
                                      (setq bomb--memory-stage 1)
                                      (setq bomb--memory-list '())) :transient t)
                ("q" "Back" (lambda () (interactive)) :transient nil)])

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
   ("c" "Complicated wires" bomb--complicated-wires :transient t)
   ("p" "Passwords" bomb--passwords :transient t)
   ("m" "Memory" bomb--memory :transient t)]
  ["Misc"
   ("-r" "Reset the bomb state" bomb--reset :transient t)
   ("q" "Exit this dialog" (lambda () (interactive) nil) :transient nil)]
  (interactive)
  (bomb--reset)
  (transient-setup 'bomb))

(provide 'bomb)
;;; bomb.el ends here
