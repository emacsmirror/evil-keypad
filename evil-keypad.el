;;; evil-keypad.el --- Modal command dispatch that speaks native Emacs keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Achyudh Ram <mail@achyudh.me>
;; Author: Achyudh Ram <mail@achyudh.me>
;; Maintainer: Achyudh Ram <mail@achyudh.me>
;; Created: 2025-05-03
;; Package-Requires: ((emacs "29.1") (which-key "3.0"))
;; Version: 0.1.2
;; Keywords: evil, keypad, modal, command, dispatch
;; URL: https://github.com/achyudh/evil-keypad

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Provides `evil-keypad-start', a command to activate a transient
;; keypad mode inspired by Meow Keypad. Allows entering complex Emacs
;; key sequences using a series of single key presses without holding
;; down modifier keys.
;;
;; After triggering the keypad, type a sequence representing modifiers
;; and keys (e.g., 'x f' for C-x C-f, 'm x' for M-x, 'a s' for C-c a
;; s). The keypad translates and executes the corresponding Emacs
;; command, then exits.
;;
;; Which-key integration is provided if `which-key-mode` is active,
;; showing the target Emacs keymap contents after a prefix is entered.

;;; Code:

(require 'cl-lib)
(require 'which-key nil t) ; Optionally load which-key

;;----------------------------------------
;; Internal State Variables
;;----------------------------------------

(defvar evil-keypad--keys nil
  "Internal list representing the keys sequence during keypad invocation.
Stores (MODIFIER . KEY-STRING) cons cells, ordered most recent first.
MODIFIER is a symbol like 'literal, 'control, 'meta, 'control-meta.
KEY-STRING is the result of `single-key-description'.")

(defvar evil-keypad--pending-modifier nil
  "Stores the pending modifier symbol ('meta, 'control-meta, 'literal) triggered
by the previous key press, to be applied to the next key.")

(defvar evil-keypad--prefix-arg nil
  "Stores the value of `current-prefix-arg' active when keypad started.")

(defvar evil-keypad--control-inducing-sequence-p nil
  "Non-nil if the current keypad sequence context implies subsequent keys default to Control.
Set based on the first action/key of the sequence.")

;; --- Which-Key Integration Variables ---
(defvar evil-keypad--display-timer nil
  "Holds the idle timer object for the which-key popup.")

(defvar evil-keypad--display-bindings-function #'ignore
  "Function called by the idle timer to display available bindings via which-key.")

(defvar evil-keypad--clear-display-function #'ignore
  "Function called to hide the which-key bindings display.")

(defvar evil-keypad--original-which-key-show-prefix nil
  "Stores the original value of `which-key-show-prefix` during keypad activation.")

;;----------------------------------------
;; Customization Variables (Triggers)
;;----------------------------------------

(defcustom evil-keypad-M-trigger ?m
  "Character entered in keypad to trigger the Meta modifier for the next key."
  :type 'character :group 'evil-keypad)

(defcustom evil-keypad-C-M-trigger ?g
  "Character entered in keypad to trigger the Control-Meta modifier for the next key."
  :type 'character :group 'evil-keypad)

(defcustom evil-keypad-literal-trigger ?\s ; Space character
  "Character entered in keypad (after first key) to trigger literal interpretation
\(no automatic Control modifier) for the next key."
  :type 'character :group 'evil-keypad)

(defcustom evil-keypad-C-x-trigger ?x
  "Character entered as the first key in keypad to represent the C-x prefix."
  :type 'character :group 'evil-keypad)

(defcustom evil-keypad-C-c-trigger ?c
  "Character entered as the first key in keypad to represent the C-c prefix."
  :type 'character :group 'evil-keypad)

(defcustom evil-keypad-C-h-trigger ?h
  "Character used as the first key in keypad to represent the C-h prefix."
  :type 'character :group 'evil-keypad)


;;----------------------------------------
;; Formatting Functions
;;----------------------------------------

(defun evil-keypad--format-key-pair (pair)
  "Format a single (MODIFIER . KEY-STRING) PAIR into a readable string."
  (let ((modifier (car pair)) (key-string (cdr pair)))
    (cond
     ((eq modifier 'meta) (format "M-%s" key-string))
     ((or (eq modifier 'control) (eq modifier 'control-meta))
      (let* ((char (and (> (length key-string) 0) (aref key-string 0)))
             (is-uppercase (and char (= (length key-string) 1) (<= ?A char) (<= char ?Z))))
        (if is-uppercase
            (format "%sS-%s" (if (eq modifier 'control) "C-" "C-M-") (downcase key-string))
          (format "%s%s" (if (eq modifier 'control) "C-" "C-M-") key-string))))
     (t key-string))))

(defun evil-keypad--format-sequence (keys-list)
  "Format the given KEYS-LIST into a readable key sequence string."
  (if keys-list (mapconcat #'evil-keypad--format-key-pair (reverse keys-list) " ") ""))

;;----------------------------------------
;; Which-Key Integration Logic
;;----------------------------------------

(declare-function which-key--create-buffer-and-show "which-key" (&optional prefix-keys from-keymap filter prefix-title))
(declare-function which-key--hide-popup "which-key" ())

(defun evil-keypad--cancel-display-timer ()
  "Cancel the which-key display timer if it exists."
  (when evil-keypad--display-timer
    (cancel-timer evil-keypad--display-timer)
    (setq evil-keypad--display-timer nil)))

(defun evil-keypad--cancel-display-timer-and-clear ()
  "Cancel the display timer and clear the which-key popup."
  (evil-keypad--cancel-display-timer)
  (funcall evil-keypad--clear-display-function))

(defun evil-keypad--trigger-which-key-display (target-keymap)
  "Show TARGET-KEYMAP using which-key."
  (evil-keypad--cancel-display-timer)
  (condition-case err
      (when (keymapp target-keymap)
        (which-key--create-buffer-and-show nil target-keymap nil nil))
    (error (message "Error showing which-key: %S" err)))) ; Log error

(defun evil-keypad--which-key-integration-setup ()
  "Set display/clear functions and manage `which-key-show-prefix'."
  (if (and (boundp 'which-key-mode) which-key-mode
           (fboundp 'which-key--create-buffer-and-show)
           (fboundp 'which-key--hide-popup))
      (progn
        (setq evil-keypad--display-bindings-function
              (lambda (keymap) (evil-keypad--trigger-which-key-display keymap)))
        (setq evil-keypad--clear-display-function #'which-key--hide-popup)
        (when (boundp 'which-key-show-prefix)
          (setq evil-keypad--original-which-key-show-prefix (symbol-value 'which-key-show-prefix))
          (setq which-key-show-prefix nil)))
    (setq evil-keypad--display-bindings-function #'ignore evil-keypad--clear-display-function #'ignore)
    (when (and (boundp 'which-key-show-prefix) evil-keypad--original-which-key-show-prefix)
      (setq which-key-show-prefix evil-keypad--original-which-key-show-prefix)
      (setq evil-keypad--original-which-key-show-prefix nil))))

(with-eval-after-load 'which-key
  (add-hook 'which-key-mode-hook #'evil-keypad--which-key-integration-setup)
  (evil-keypad--which-key-integration-setup))

;;----------------------------------------
;; Core Logic
;;----------------------------------------

(defun evil-keypad--keymap-has-ctrl-meta-bindings-p (keymap)
  "Return t if KEYMAP contains any C-M- modified single key bindings."
  (when (keymapp keymap)
    (let ((found nil))
      (map-keymap
       (lambda (key _binding)
         (unless (vectorp key) ; Process single events
           (let ((mods (event-modifiers key)))
             (when (and (memq 'control mods) (memq 'meta mods))
               (setq found t)
               (cl-return-from evil-keypad--keymap-has-ctrl-meta-bindings-p t)))))
       keymap)
      found)))

(defun evil-keypad--context-allows-modifier-type-p (modifier-type)
  "Check if the current keypad context allows for PENDING-MODIFIER-TYPE.
MODIFIER-TYPE is 'meta or 'control-meta."
  (if (null evil-keypad--keys) ; If at the very start of keypad input
      t ; Always allow m/g to trigger initially
    (let* ((seq-str (evil-keypad--format-sequence evil-keypad--keys))
           (current-map (if (string-empty-p seq-str)
                            (current-global-map)
                          (key-binding (kbd seq-str) t))))
      (if (keymapp current-map)
          (cl-case modifier-type
            (meta (lookup-key current-map (kbd "ESC") t))
            (control-meta (evil-keypad--keymap-has-ctrl-meta-bindings-p current-map)))
        nil))))

(defun evil-keypad--execute (command)
  "Execute COMMAND."
  (condition-case err
      (let ((current-prefix-arg evil-keypad--prefix-arg))
        (call-interactively command))
    (error (message "Error executing %s: %s" command err))))

(defun evil-keypad--try-execute ()
  "Check current sequence, execute/fallback, or update echo/which-key. Returns t to exit."
  (if evil-keypad--pending-modifier (evil-keypad--display-pending-state)
    (let ((seq-str (evil-keypad--format-sequence evil-keypad--keys)))
      (if (string-empty-p seq-str) (progn (message "") nil)
        (let* ((seq-vec (kbd seq-str)) (binding (key-binding seq-vec t)))
          (cond
           ((keymapp binding) (evil-keypad--handle-prefix-binding binding seq-str))
           ((commandp binding) (evil-keypad--handle-command-binding binding seq-str))
           (t (evil-keypad--handle-unbound-sequence seq-str))))))))

(defun evil-keypad--quit ()
  "Internal cleanup for keypad."
  (evil-keypad--cancel-display-timer-and-clear)
  (when (and (boundp 'which-key-show-prefix) evil-keypad--original-which-key-show-prefix)
    (setq which-key-show-prefix evil-keypad--original-which-key-show-prefix
          evil-keypad--original-which-key-show-prefix nil))
  (setq evil-keypad--keys nil
        evil-keypad--pending-modifier nil
        evil-keypad--prefix-arg nil
        evil-keypad--control-inducing-sequence-p nil))

(defun evil-keypad-quit-command ()
  "Interactive command to quit keypad."
  (interactive)
  (evil-keypad--quit) t)

(defun evil-keypad-undo ()
  "Interactive command to undo last keypad input."
  (interactive)
  (evil-keypad--cancel-display-timer-and-clear)
  (if evil-keypad--pending-modifier
      (setq evil-keypad--pending-modifier nil)
    ;; No pending modifier, so operate on evil-keypad--keys
    (if evil-keypad--keys
        (pop evil-keypad--keys)
      ;; Keys were already empty, and no pending modifier
      (ding)))
  (unless evil-keypad--keys
    (setq evil-keypad--control-inducing-sequence-p nil))
  (evil-keypad--try-execute)
  nil)

(defvar evil-keypad-state-keymap (make-sparse-keymap) "Keymap for keypad internal commands.")
(define-key evil-keypad-state-keymap (kbd "<escape>") #'evil-keypad-quit-command)
(define-key evil-keypad-state-keymap (kbd "ESC") #'evil-keypad-quit-command)
(define-key evil-keypad-state-keymap (kbd "<backspace>") #'evil-keypad-undo)
(define-key evil-keypad-state-keymap (kbd "DEL") #'evil-keypad-undo)

(defun evil-keypad--display-pending-state ()
  "Display the current sequence and pending modifier in the echo area. Returns nil."
  (message "%s%s"
           (if evil-keypad--keys (concat (evil-keypad--format-sequence evil-keypad--keys) " ") "")
           (cl-case evil-keypad--pending-modifier
             (meta "M-") (control-meta "C-M-") (literal "_")))
  nil)

(defun evil-keypad--handle-prefix-binding (binding seq-str)
  "Handle a key sequence that maps to a prefix (keymap).
Displays prefix in echo area and schedules which-key. Returns nil."
  (message "%s-" seq-str)
  (evil-keypad--cancel-display-timer)
  (setq evil-keypad--display-timer
        (run-with-idle-timer (or (bound-and-true-p which-key-idle-delay) 0.4)
                             nil evil-keypad--display-bindings-function binding))
  nil)

(defun evil-keypad--handle-command-binding (binding seq-str)
  "Handle a key sequence that maps to a command. Executes and returns t."
  (evil-keypad--cancel-display-timer-and-clear)
  (evil-keypad--execute binding)
  t)

(defun evil-keypad--handle-unbound-sequence (original-seq-str)
  "Handle an unbound ORIGINAL-SEQ-STR, attempting fallback.
Returns t to exit, nil to continue (if fallback leads to new prefix)."
  (let* ((last-pair (car evil-keypad--keys))
         (last-modifier (car last-pair))
         ;; Fallback is possible only if last modifier was 'control'
         (fallback-possible (and evil-keypad--keys (eq last-modifier 'control)))
         (fallback-binding nil)
         (fallback-keys nil)
         (fallback-seq-str nil))

    (when fallback-possible
      (setq fallback-keys (cons (cons 'literal (cdr last-pair))
                                (cdr evil-keypad--keys)))
      (setq fallback-seq-str (evil-keypad--format-sequence fallback-keys))
      (when (not (string-empty-p fallback-seq-str))
        (setq fallback-binding (key-binding (kbd fallback-seq-str) t))))

    (cond
     ((commandp fallback-binding)
      (evil-keypad--handle-command-binding fallback-binding fallback-seq-str)) ; Returns t
     ((keymapp fallback-binding)
      (setq evil-keypad--keys fallback-keys)
      ;; Let handle-prefix-binding display new prefix and schedule which-key
      (evil-keypad--handle-prefix-binding fallback-binding fallback-seq-str)) ; Returns nil
     (t
      (evil-keypad--cancel-display-timer-and-clear)
      (message "%s is undefined" original-seq-str)
      (evil-keypad--quit)
      t))))

(defun evil-keypad--set-new-pending-modifier (event)
  "Handle EVENT when it's a new modifier trigger (m, g, SPC). Returns nil."
  (let ((is-first-key-action (null evil-keypad--keys)))
    (cond
     ((eq event evil-keypad-M-trigger)
      (setq evil-keypad--pending-modifier 'meta)
      (when is-first-key-action (setq evil-keypad--control-inducing-sequence-p t)))
     ((eq event evil-keypad-C-M-trigger)
      (setq evil-keypad--pending-modifier 'control-meta)
      (when is-first-key-action (setq evil-keypad--control-inducing-sequence-p t)))
     ((eq event evil-keypad-literal-trigger) ; `current-event-is-new-trigger` ensures keys not nil
      (setq evil-keypad--pending-modifier 'literal))))
  (evil-keypad--try-execute) ; Display the pending state
  nil)

(defun evil-keypad--process-resolved-key (event mod-from-pending)
  "Process EVENT as a key that resolves MOD-FROM-PENDING or is a regular key.
Returns result of `evil-keypad--try-execute` (t to exit, nil to continue)."
  (let* ((key-string (single-key-description event))
         (is-first-actual-key (null evil-keypad--keys))
         (add-implicit-C-c-prefix-p nil)
         (modifier-for-current-key
          (cond
           (mod-from-pending mod-from-pending)
           (is-first-actual-key
            (cond
             ((or (eq event evil-keypad-C-x-trigger) (eq event evil-keypad-C-c-trigger))
              (setq evil-keypad--control-inducing-sequence-p t) 'control)
             ((eq event evil-keypad-C-h-trigger)
              (setq evil-keypad--control-inducing-sequence-p nil) 'control)
             (t
              (setq add-implicit-C-c-prefix-p t)
              (setq evil-keypad--control-inducing-sequence-p nil) 'literal)))
           (evil-keypad--control-inducing-sequence-p 'control)
           (t 'literal))))
    (push (cons modifier-for-current-key key-string) evil-keypad--keys)
    (when add-implicit-C-c-prefix-p
      (setq evil-keypad--keys (list (cons 'literal key-string) (cons 'control "c")))))
  (evil-keypad--try-execute))

(defun evil-keypad--handle-input (event)
  "Handle a single EVENT. Returns t if loop should exit."
  (evil-keypad--cancel-display-timer)
  (let ((cmd (lookup-key evil-keypad-state-keymap (vector event))))
    (if cmd (call-interactively cmd)
      (let ((mod-from-pending evil-keypad--pending-modifier)
            (key-is-m-trigger (eq event evil-keypad-M-trigger))
            (key-is-cm-trigger (eq event evil-keypad-C-M-trigger))
            (key-is-spc-trigger (and evil-keypad--keys (eq event evil-keypad-literal-trigger))))
        (setq evil-keypad--pending-modifier nil) ; Clear pending from previous step
        (cond
         ;; Case 1: Event is M-trigger, no mod pending, AND context allows Meta
         ((and (not mod-from-pending) key-is-m-trigger (evil-keypad--context-allows-modifier-type-p 'meta))
          (evil-keypad--set-new-pending-modifier event))

         ;; Case 2: Event is C-M-trigger, no mod pending, AND context allows Ctrl-Meta
         ((and (not mod-from-pending) key-is-cm-trigger (evil-keypad--context-allows-modifier-type-p 'control-meta))
          (evil-keypad--set-new-pending-modifier event))

         ;; Case 3: Event is SPC trigger, no mod pending
         ((and (not mod-from-pending) key-is-spc-trigger)
          (evil-keypad--set-new-pending-modifier event)) ; SPC does not require context check

         ;; Case 4: Event consumes a pending modifier OR is a regular data key
         ;; This includes M/C-M triggers if they didn't trigger above because context didn't allow
         (t
          (evil-keypad--process-resolved-key event mod-from-pending)))))))

;;----------------------------------------
;; Main Entry Point
;;----------------------------------------

(defun evil-keypad-start ()
  "Start the Evil Keypad transient mode."
  (interactive)
  (evil-keypad--cancel-display-timer-and-clear)
  (setq evil-keypad--keys nil
        evil-keypad--pending-modifier nil
        evil-keypad--prefix-arg current-prefix-arg
        evil-keypad--control-inducing-sequence-p nil)

  (when (and (boundp 'which-key-mode) which-key-mode)
    (evil-keypad--which-key-integration-setup))
  (message "") ; Clear echo area initially

  (unwind-protect
      (while (not (evil-keypad--handle-input (read-key))))
    (evil-keypad--quit)))


(provide 'evil-keypad)

;;; evil-keypad.el ends here
