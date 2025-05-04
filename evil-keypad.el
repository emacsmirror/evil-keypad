;;; evil-keypad.el --- Modal command dispatch that speaks native Emacs keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Achyudh Ram <mail@achyudh.me>
;; Author: Achyudh Ram <mail@achyudh.me>
;; Maintainer: Achyudh Ram <mail@achyudh.me>
;; Created: 2025-05-03
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (which-key "3.0"))
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
;; key sequences using single key presses without holding down modifier
;; keys.
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
MODIFIER is a symbol like 'literal, 'control, 'meta, 'both.
KEY-STRING is the result of `single-key-description'.")

(defvar evil-keypad--pending-modifier nil
  "Stores the pending modifier symbol ('meta, 'both, 'literal) triggered
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
     ((or (eq modifier 'control) (eq modifier 'both))
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

(defun evil-keypad--execute (command)
  "Execute COMMAND."
  (condition-case err
      (let ((current-prefix-arg evil-keypad--prefix-arg))
        (call-interactively command))
    (error (message "Error executing %s: %s" command err))))

(declare-function evil-keypad--try-execute "evil-keypad")

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

(defun evil-keypad--try-execute ()
  "Check current sequence, execute, or update echo/which-key. Returns t to exit loop."
  (if evil-keypad--pending-modifier
      (progn ; Echo pending state
        (message "%s%s"
                 (if evil-keypad--keys (concat (evil-keypad--format-sequence evil-keypad--keys) " ") "")
                 (cl-case evil-keypad--pending-modifier
                   (meta "M-") (both "C-M-") (literal "_")))
        nil) ; Continue loop
    (let ((seq-str (evil-keypad--format-sequence evil-keypad--keys)))
      (if (string-empty-p seq-str)
          (progn (message "") nil) ; Clear echo and continue
        (let* ((seq-vec (kbd seq-str)) (binding (key-binding seq-vec t)))
          (cond
           ((keymapp binding)
            (message "%s-" seq-str) ; Echo prefix
            (evil-keypad--cancel-display-timer)
            (setq evil-keypad--display-timer
                  (run-with-idle-timer (or (bound-and-true-p which-key-idle-delay) 0.4)
                                       nil evil-keypad--display-bindings-function binding))
            nil) ; Continue loop
           ((commandp binding)
            (evil-keypad--cancel-display-timer-and-clear)
            (evil-keypad--execute binding) t)
           (t ; Unbound
            (evil-keypad--cancel-display-timer-and-clear)
            (message "%s is undefined" seq-str) ; Echo unbound
            (evil-keypad--quit) t)))))))

(defun evil-keypad--handle-input (event)
  "Handle a single EVENT. Returns t if loop should exit."
  (evil-keypad--cancel-display-timer)
  (let ((cmd (lookup-key evil-keypad-state-keymap (vector event))))
    (if cmd (call-interactively cmd) ; Handle Undo/Quit
      ;; Process regular key input
      (let ((mod-from-pending evil-keypad--pending-modifier)
            (current-event-is-new-trigger
             (or (eq event evil-keypad-M-trigger)
                 (eq event evil-keypad-C-M-trigger)
                 (and evil-keypad--keys (eq event evil-keypad-literal-trigger)))))
        (setq evil-keypad--pending-modifier nil) ; Always clear pending from previous step

        (if (and (not mod-from-pending) current-event-is-new-trigger)
            ;; Case 1: Current event IS a new trigger (m,g,SPC), and NO mod was pending for it.
            (progn
              (let ((is-first-key-action (null evil-keypad--keys)))
                (cond
                 ((eq event evil-keypad-M-trigger)
                  (setq evil-keypad--pending-modifier 'meta)
                  (when is-first-key-action (setq evil-keypad--control-inducing-sequence-p t)))
                 ((eq event evil-keypad-C-M-trigger)
                  (setq evil-keypad--pending-modifier 'both)
                  (when is-first-key-action (setq evil-keypad--control-inducing-sequence-p t)))
                 ((eq event evil-keypad-literal-trigger) ; implies evil-keypad--keys non-nil
                  (setq evil-keypad--pending-modifier 'literal))))
              (evil-keypad--try-execute) ; Display pending state
              nil) ; Continue loop, wait for next key

          ;; Case 2: Current event is NOT a new trigger OR it IS consuming a pending modifier.
          (let* ((key-string (single-key-description event))
                 (is-first-actual-key (null evil-keypad--keys))
                 (add-explicit-C-c-prefix-p nil)
                 (modifier-for-current-key
                  (cond
                   (mod-from-pending mod-from-pending) ; Modifier from m,g,SPC takes precedence
                   (is-first-actual-key ; First key in sequence being built
                    (cond
                     ((or (eq event evil-keypad-C-x-trigger) ; Rule 3 for x, c
                          (eq event evil-keypad-C-c-trigger))
                      (setq evil-keypad--control-inducing-sequence-p t) 'control)
                     ((eq event evil-keypad-C-h-trigger) ; Rule 3 for h
                      (setq evil-keypad--control-inducing-sequence-p nil) 'control) ; C-h is not C-inducing
                     (t ; Rule 1 for other first keys
                      (setq add-explicit-C-c-prefix-p t)
                      (setq evil-keypad--control-inducing-sequence-p nil) ; C-c k is not C-inducing
                      'literal)))
                   (evil-keypad--control-inducing-sequence-p 'control) ; Subsequent key in C-inducing context
                   (t 'literal)))) ; Subsequent key, not in C-inducing context

            (push (cons modifier-for-current-key key-string) evil-keypad--keys)
            (when add-explicit-C-c-prefix-p
              (setq evil-keypad--keys (list (cons 'literal key-string) (cons 'control "c"))))
            (evil-keypad--try-execute))))))) ; Returns t/nil

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
