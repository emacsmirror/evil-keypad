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
        (evil-keypad--prefix-arg current-prefix-arg) ; Capture if set for test context
        (result-sequence nil))

    ;; evil-keypad--handle-input will call the advised evil-keypad--try-execute,
    ;; which will return nil. So, evil-keypad--handle-input will also return nil.
    ;; The dolist will complete all key events.
    (dolist (event key-events-list)
      (evil-keypad--handle-input event))

    (setq result-sequence (evil-keypad--format-sequence evil-keypad--keys))

    ;; Call quit to reset any other flags, though state here is let-bound
    ;; This is important if `evil-keypad--quit` has other side-effects
    ;; we want to include in testing (e.g. which-key var restoration).
    (evil-keypad--quit)
    result-sequence))

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
