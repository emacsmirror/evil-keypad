;;; evil-keypad-tests.el --- ERT tests for evil-keypad.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'evil)
(require 'evil-keypad)

;;----------------------------------------
;; Test Helper Functions & Advice
;;----------------------------------------

(defun evil-keypad-test--null-try-execute (orig-fn &rest _args)
  "Advise `evil-keypad--try-execute` to do nothing and return nil.
Used to test only the state-update logic of `evil-keypad--handle-input`."
  nil)

(defmacro evil-keypad-test-translations-only (&rest body)
  "Execute BODY with `evil-keypad--try-execute` advised to do nothing and return nil."
  (declare (indent 0))
  `(let ((evil-keypad--keys nil)
         (evil-keypad--pending-modifier nil)
         (evil-keypad--control-inducing-sequence-p nil)
         (evil-keypad--session-active-prefix-arg nil))
     (advice-add 'evil-keypad--try-execute :around #'evil-keypad-test--null-try-execute)
     (unwind-protect
         (progn ,@body)
       (evil-keypad--quit)
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

(defun evil-keypad-test--get-state ()
  "Return a snapshot of the current evil-keypad state."
  (list :keys evil-keypad--keys
        :pending-modifier evil-keypad--pending-modifier
        :control-inducing evil-keypad--control-inducing-sequence-p
        :prefix-arg evil-keypad--session-active-prefix-arg
        :initial-prefix-arg evil-keypad--session-initial-prefix-arg))

(defun evil-keypad-test--verify-clean-state ()
  "Verify that all state variables are properly reset."
  (should (null evil-keypad--keys))
  (should (null evil-keypad--pending-modifier))
  (should (null evil-keypad--control-inducing-sequence-p))
  (should (null evil-keypad--session-active-prefix-arg))
  (should (null evil-keypad--session-initial-prefix-arg)))

;;----------------------------------------
;; ERT Test Cases - Core Translation Tests
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

;;----------------------------------------
;; ERT Test Cases - State Management
;;----------------------------------------

(ert-deftest evil-keypad-test-clean-state ()
  "Test that state is properly initialized and cleaned."
  (evil-keypad-test-translations-only
   ;; Test initial state
   (evil-keypad-test--verify-clean-state)

   ;; Modify state
   (evil-keypad--handle-input ?x)
   (should evil-keypad--keys)
   (should evil-keypad--control-inducing-sequence-p)

   ;; Test cleanup
   (evil-keypad--quit)
   (evil-keypad-test--verify-clean-state)))

(ert-deftest evil-keypad-test-control-inducing-transitions ()
  "Test control-inducing flag transitions for different sequences."
  (evil-keypad-test-translations-only
   ;; Test C-x (should be control-inducing)
   (evil-keypad--handle-input ?x)
   (should evil-keypad--control-inducing-sequence-p)
   (evil-keypad--quit)

   ;; Test C-c (should be control-inducing)
   (evil-keypad--handle-input ?c)
   (should evil-keypad--control-inducing-sequence-p)
   (evil-keypad--quit)

   ;; Test C-h (should not be control-inducing)
   (evil-keypad--handle-input ?h)
   (should-not evil-keypad--control-inducing-sequence-p)
   (evil-keypad--quit)

   ;; Test regular key (should not be control-inducing)
   (evil-keypad--handle-input ?a)
   (should-not evil-keypad--control-inducing-sequence-p)
   (evil-keypad--quit)

   ;; Test Meta modifier (should be control-inducing)
   (evil-keypad--handle-input ?m)
   (should evil-keypad--control-inducing-sequence-p)
   (should (eq evil-keypad--pending-modifier 'meta))
   (evil-keypad--quit)

   ;; Test Control-Meta modifier (should be control-inducing)
   (evil-keypad--handle-input ?g)
   (should evil-keypad--control-inducing-sequence-p)
   (should (eq evil-keypad--pending-modifier 'control-meta))))

(ert-deftest evil-keypad-test-modifier-state-transitions ()
  "Test state transitions involving modifiers."
  (evil-keypad-test-translations-only
   ;; Test Meta modifier pending state
   (should (null evil-keypad--pending-modifier))
   (evil-keypad--handle-input ?m)
   (should (eq evil-keypad--pending-modifier 'meta))
   (should evil-keypad--control-inducing-sequence-p)

   ;; Test Meta modifier application
   (evil-keypad--handle-input ?x)
   (should (null evil-keypad--pending-modifier))
   (should (equal (car evil-keypad--keys) '(meta . "x")))

   ;; Test Control-Meta modifier pending state
   (evil-keypad--quit)
   (evil-keypad--handle-input ?g)
   (should (eq evil-keypad--pending-modifier 'control-meta))
   (should evil-keypad--control-inducing-sequence-p)

   ;; Test Control-Meta modifier application
   (evil-keypad--handle-input ?x)
   (should (null evil-keypad--pending-modifier))
   (should (equal (car evil-keypad--keys) '(control-meta . "x")))))

(ert-deftest evil-keypad-test-literal-modifier-state ()
  "Test literal modifier state in different contexts."
  (evil-keypad-test-translations-only
   ;; Test literal modifier in control-inducing sequence
   (evil-keypad--handle-input ?x)
   (should (equal evil-keypad--keys '((control . "x"))))
   (should evil-keypad--control-inducing-sequence-p)

   (evil-keypad--handle-input ?\s)
   (should (eq evil-keypad--pending-modifier 'literal))

   (evil-keypad--handle-input ?f)
   (should (equal evil-keypad--keys
                  '((literal . "f") (control . "x"))))
   (should evil-keypad--control-inducing-sequence-p)

   ;; Test that subsequent keys maintain control-inducing
   (evil-keypad--handle-input ?a)
   (should (equal evil-keypad--keys
                  '((control . "a") (literal . "f") (control . "x"))))
   (should evil-keypad--control-inducing-sequence-p)))

;;----------------------------------------
;; ERT Test Cases - Undo Logic
;;----------------------------------------

(ert-deftest evil-keypad-test-undo-logic ()
  "Test undo functionality and its effect on sequence state and flags."
  (evil-keypad-test-translations-only
   ;; 1. Type 'x', then 'm' (pending), then undo 'm'
   (evil-keypad--handle-input ?x)
   (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
   (should evil-keypad--control-inducing-sequence-p)

   (evil-keypad--handle-input ?m)
   (should (eq evil-keypad--pending-modifier 'meta))
   (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))

   (evil-keypad-undo)
   (should (null evil-keypad--pending-modifier))
   (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
   (should evil-keypad--control-inducing-sequence-p)

   ;; 2. Type 'f' (becomes C-f), then undo 'f'
   (evil-keypad--handle-input ?f)
   (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x C-f"))
   (evil-keypad-undo)
   (should (string= (evil-keypad--format-sequence evil-keypad--keys) "C-x"))
   (should evil-keypad--control-inducing-sequence-p)

   ;; 3. Undo 'x'
   (evil-keypad-undo)
   (should (null evil-keypad--keys))
   (should (not evil-keypad--control-inducing-sequence-p))

   ;; 4. Undo on empty state
   (evil-keypad-undo)
   (should (null evil-keypad--keys))
   (should (not evil-keypad--control-inducing-sequence-p))

   (evil-keypad--quit))) ; Ensure cleanup of global state by main code

;;----------------------------------------
;; ERT Test Cases - Prefix Arguments
;;----------------------------------------

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

(provide 'evil-keypad-tests)

;;; evil-keypad-tests.el ends here
