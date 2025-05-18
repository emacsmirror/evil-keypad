;;; evil-keypad-tests.el --- ERT tests for evil-keypad.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'evil-keypad)

;;----------------------------------------
;; Test Helper Function & Advice
;;----------------------------------------

(defun evil-keypad-test--null-try-execute (orig-fn &rest _args)
  "Advise `evil-keypad--try-execute` to do nothing and return nil.
Used to test only the state-update logic of `evil-keypad--handle-input`."
  ;; (message "Spy: evil-keypad--try-execute called, returning nil.") ; Optional debug
  nil)

(defmacro evil-keypad-test-translations-only (&rest body)
  "Execute BODY with `evil-keypad--try-execute` advised to do nothing and return nil."
  `(progn
     (advice-add 'evil-keypad--try-execute :around #'evil-keypad-test--null-try-execute)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'evil-keypad--try-execute #'evil-keypad-test--null-try-execute))))

(defun evil-keypad-test--simulate-translation (key-events-list)
  "Simulate typing KEY-EVENTS-LIST into evil-keypad for translation testing.
`evil-keypad--try-execute' is advised to do nothing and return nil.
Returns the formatted sequence string from `evil-keypad--keys`.
Resets evil-keypad internal state before and after each call."
  (let ((evil-keypad--keys nil) ; Isolate state for test run
        (evil-keypad--pending-modifier nil)
        (evil-keypad--control-inducing-sequence-p nil)
        (evil-keypad--session-active-prefix-arg nil)
        (result-sequence nil))

    (dolist (event key-events-list)
      (evil-keypad--handle-input event))

    (setq result-sequence (evil-keypad--format-sequence evil-keypad--keys))
    (evil-keypad--quit)
    result-sequence))

(defun evil-keypad-test--simulate-prefix-arg (key-events-list)
  "Simulate typing KEY-EVENTS-LIST into evil-keypad for prefix arg testing.
Returns the current prefix arg value after simulation.
Resets evil-keypad internal state before and after each call."
  (let ((evil-keypad--keys nil)
        (evil-keypad--pending-modifier nil)
        (evil-keypad--control-inducing-sequence-p nil)
        (evil-keypad--session-active-prefix-arg nil))

    (dolist (event key-events-list)
      (evil-keypad--handle-input event))

    (prog1 evil-keypad--session-active-prefix-arg
      (evil-keypad--quit))))

(defun evil-keypad-test--get-prefix-arg-display (key-events-list)
  "Simulate typing KEY-EVENTS-LIST and return the formatted prefix arg display.
Resets evil-keypad internal state before and after each call."
  (let ((evil-keypad--keys nil)
        (evil-keypad--pending-modifier nil)
        (evil-keypad--control-inducing-sequence-p nil)
        (evil-keypad--session-active-prefix-arg nil))

    (dolist (event key-events-list)
      (evil-keypad--handle-input event))

    (prog1 (evil-keypad--format-prefix-arg evil-keypad--session-active-prefix-arg)
      (evil-keypad--quit))))

;;----------------------------------------
;; ERT Test Cases
;;----------------------------------------

(ert-deftest evil-keypad-test-sequence-translations ()
  "Test core key sequence translations (state update and formatting)."
  (evil-keypad-test-translations-only
   ;; --- Basic C-x, M-x, C-c k ---
   (should (string= (evil-keypad-test--simulate-translation '(?x ?f)) "C-x C-f"))
   (should (string= (evil-keypad-test--simulate-translation '(?m ?x)) "M-x"))
   (should (string= (evil-keypad-test--simulate-translation '(?a)) "C-c a"))

   ;; --- Control-Inducing Logic & Literal SPC ---
   (should (string= (evil-keypad-test--simulate-translation '(?c ?a)) "C-c C-a"))
   (should (string= (evil-keypad-test--simulate-translation '(?x ?\s ?f)) "C-x f"))
   (should (string= (evil-keypad-test--simulate-translation '(?h ?v)) "C-h v"))
   (should (string= (evil-keypad-test--simulate-translation '(?x ?\s ?t ?a)) "C-x t C-a"))
   (should (string= (evil-keypad-test--simulate-translation '(?m ?s ?a)) "M-s C-a"))
   (should (string= (evil-keypad-test--simulate-translation '(?g ?s ?a)) "C-M-s C-a"))
   (should (string= (evil-keypad-test--simulate-translation '(?a ?s ?d)) "C-c a s d"))

   ;; --- Uppercase Handling ---
   (should (string= (evil-keypad-test--simulate-translation '(?x ?F)) "C-x C-S-f"))
   (should (string= (evil-keypad-test--simulate-translation '(?m ?F)) "M-F"))
   (should (string= (evil-keypad-test--simulate-translation '(?g ?F)) "C-M-S-f"))
   (should (string= (evil-keypad-test--simulate-translation '(?h ?\s ?F)) "C-h F"))
   (should (string= (evil-keypad-test--simulate-translation '(?a ?F)) "C-c a F"))

   ;; --- Control-Inducing Prefixes ---
   (should (string= (evil-keypad-test--simulate-translation '(?c ?t ?t)) "C-c C-t C-t"))

   ;; --- Trigger key interactions ---
   (should (string= (evil-keypad-test--simulate-translation '(?m ?m)) "M-m"))
   (should (string= (evil-keypad-test--simulate-translation '(?g ?g)) "C-M-g"))
   (should (string= (evil-keypad-test--simulate-translation '(?m ?g)) "M-g"))
   (should (string= (evil-keypad-test--simulate-translation '(?g ?m)) "C-M-m"))
   (should (string= (evil-keypad-test--simulate-translation '(?x ?\s ?\s)) "C-x SPC"))

   ;; --- Special keys and SPC as first key ---
   (should (string= (evil-keypad-test--simulate-translation (list (string-to-char " "))) "C-c SPC"))
   (should (string= (evil-keypad-test--simulate-translation (list (string-to-char "\t"))) "C-c TAB"))
   (should (string= (evil-keypad-test--simulate-translation (list evil-keypad-C-x-trigger (string-to-char "\r"))) "C-x C-RET"))))

(ert-deftest evil-keypad-test-undo-logic ()
  "Test undo functionality and its effect on sequence state and flags."
  (evil-keypad-test-translations-only ; Use same spy to prevent try-execute side effects
   (let ((evil-keypad--keys nil)
         (evil-keypad--pending-modifier nil)
         (evil-keypad--control-inducing-sequence-p nil)
         (current-prefix-arg nil))

     ;; 1. Type 'x', then 'm' (pending), then undo 'm'
     (evil-keypad--handle-input ?x)
     (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
     (should evil-keypad--control-inducing-sequence-p)

     (evil-keypad--handle-input ?m) ; Sets pending 'meta'
     (should (eq evil-keypad--pending-modifier 'meta))
     (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))

     (evil-keypad-undo) ; Clears pending 'meta'
     (should (null evil-keypad--pending-modifier))
     (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
     (should evil-keypad--control-inducing-sequence-p)

     ;; 2. Type 'f' (becomes C-f), then undo 'f'
     (evil-keypad--handle-input ?f)
     (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x C-f"))
     (evil-keypad-undo) ; Pops C-f
     (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
     (should evil-keypad--control-inducing-sequence-p)

     ;; 3. Undo 'x'
     (evil-keypad-undo) ; Pops C-x
     (should (null evil-keypad--keys))
     (should (not evil-keypad--control-inducing-sequence-p))

     ;; 4. Undo on empty state
     (evil-keypad-undo) ; Should ding, state remains empty
     (should (null evil-keypad--keys))
     (should (not evil-keypad--control-inducing-sequence-p))

     (evil-keypad--quit)))) ; Ensure cleanup of global state by main code

(ert-deftest evil-keypad-test-numeric-prefix-args ()
  "Test basic numeric prefix argument handling."
  (evil-keypad-test-translations-only
   ;; Single digits
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?5)) 5))

   ;; Multiple digits
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?4 ?2)) 42))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?1 ?2 ?3)) 123))

   ;; Leading zero
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?0)) 0))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?0 ?5)) 5))

   ;; Negative numbers
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?-)) '-))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?- ?5)) -5))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?- ?1 ?2)) -12))

   ;; Toggles with multiple minuses
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?- ?-)) nil))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?- ?- ?-)) '-))))

(ert-deftest evil-keypad-test-universal-prefix-args ()
  "Test universal argument (C-u) handling."
  (evil-keypad-test-translations-only
   ;; Basic universal argument
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u)) '(4)))

   ;; Multiple universal arguments
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u ?u)) '(16)))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u ?u ?u)) '(64)))

   ;; Universal with numbers
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u ?5)) 5))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u ?4 ?2)) 42))

   ;; Universal with negative
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?u ?-)) '(-4)))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?- ?u)) '(-4)))

   ;; Numbers before universal
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?5 ?u)) '(4)))
   (should (equal (evil-keypad-test--simulate-prefix-arg '(?4 ?2 ?u)) '(4)))))

(ert-deftest evil-keypad-test-prefix-arg-display ()
  "Test prefix argument display formatting."
  (evil-keypad-test-translations-only
   ;; Basic numeric display
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?5)) "C-u 5"))
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?4 ?2)) "C-u 42"))

   ;; Universal argument display
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?u)) "C-u"))
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?u ?u)) "C-u (16)"))

   ;; Negative number display
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?-)) "C-u -"))
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?- ?5)) "C-u -5"))

   ;; Complex combinations
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?u ?-)) "C-u -"))
   (should (string= (evil-keypad-test--get-prefix-arg-display '(?5 ?u)) "C-u"))

   ;; Empty/nil display
   (should (string= (evil-keypad-test--get-prefix-arg-display '()) ""))))
