# Evil Keypad

## What Is Evil Keypad?

**Evil Keypad** is a transient key dispatch system for Emacs Evil users. Inspired by [Meow's Keypad](https://github.com/meow-edit/meow) and [God Mode](https://github.com/emacsorphanage/god-mode), it provides a fast and ergonomic way to enter Emacs commands—like `C-x C-f`, `M-x`, or `C-c a`—without holding modifier keys.

After pressing a trigger key (e.g., `,` or `SPC` in Evil normal state), you enter a short sequence of unmodified keys. Evil Keypad interprets this sequence into a standard Emacs keybinding and executes the resulting command, then automatically exits. Think modal input for commands, no chording, fewer custom leader keymaps needed.

## Why Evil Keypad?

Standard Emacs bindings, while powerful, can feel cumbersome when integrated with a modal editing paradigm like Evil. Holding `Ctrl` or `Meta` for multi-step command chains can break the ergonomic flow. While leader key setups (e.g., via `general.el`) mitigate this, they often require extensive manual configuration, as many commands might need explicit rebinding.

Meow’s *Keypad* introduced an elegant solution by translating sequences of simple keys into standard Emacs bindings on the fly. However, adopting Meow typically means forgoing Evil’s rich editing model. Evil Keypad aims to avoid this tradeoff: it brings the keypad translation concept to Evil users (and others) *without* requiring them to discard existing muscle memory or text editing workflows.

**Evil Keypad is designed to:**

* Execute `C-x`, `C-c`, `M-` (Meta), and `C-M-` (Control-Meta) based sequences using simple, sequential keypresses.
* Integrate smoothly with Evil’s modal editing (or be used standalone).
* Preserve Emacs’s native command namespace—no rebinding of standard commands is necessary.
* Provide live suggestions for next keys via `which-key` after a prefix is entered.

## Installation

Evil Keypad is not yet available on MELPA. For now, install it manually or via `use-package` with `:vc`.

### Using `use-package` with `:vc` (Recommended)

This requires Emacs 29+ (for built-in VC package management) or an appropriately configured `use-package` setup that can fetch from Git.

```emacs-lisp
(use-package evil-keypad
  :vc (:url "https://github.com/achyudh/evil-keypad" :branch "main") ; Ensure URL is correct
  :ensure t ; If your use-package setup supports :ensure with :vc
  :after evil ; Load after Evil if you're an Evil user
  :config
  (require 'evil-keypad) ; Ensure it's required before binding
  ;; Example binding for Evil normal state
  (define-key evil-normal-state-map (kbd ",") #'evil-keypad-start) ; Using comma as an example
  ;; For a global binding (non-Evil or for other states):
  ;; (global-set-key (kbd "C-;") #'evil-keypad-start)
  )
```

### Manual Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/achyudh/evil-keypad.git /path/to/your/emacs/site-lisp/evil-keypad
    ```
    *(Adjust path as needed)*
2.  **Add to `load-path` in your Emacs configuration (`init.el` or `~/.emacs.d/init.el`):**
    ```emacs-lisp
    (add-to-list 'load-path "/path/to/your/emacs/site-lisp/evil-keypad")
    (require 'evil-keypad)

    ;; Example binding (see "Binding the Trigger Key" below)
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "SPC") #'evil-keypad-start))
    ```

## Usage

### Binding the Trigger Key

After installation, bind the `evil-keypad-start` command to a key of your choice to activate the keypad.

**For Evil users (example: `,` or `SPC` in normal state):**
```emacs-lisp
(with-eval-after-load 'evil ; Ensure Evil is loaded first
  (define-key evil-normal-state-map (kbd "SPC") #'evil-keypad-start))
```

**For non-Evil users or other states (example: `C-;`):**
```emacs-lisp
(global-set-key (kbd "C-;") #'evil-keypad-start)
```

### Using Evil Keypad

1.  Press your chosen trigger key. The echo area will clear, indicating Evil Keypad is active.
2.  Type a sequence of keys according to the translation logic detailed in the next section.
    * If you type a sequence that translates to an Emacs prefix (e.g., keypad `x` for `C-x`), the echo area will show the prefix with a dash (e.g., `C-x-`).
    * If you type a modifier trigger (`m`, `g`, `SPC`), the echo area indicates the pending modifier (e.g., `C-x M-`).
    * If `which-key-mode` is active, a `which-key` popup will appear after `which-key-idle-delay`. It displays the standard Emacs keymap associated with the translated prefix (e.g., `ctl-x-map`). The echo area will show the current Evil Keypad sequence (e.g., `C-x-`).
2.  Once a complete command is recognized, it executes, and the keypad automatically exits.
3.  If the sequence is unbound, an `<sequence> is undefined` message appears in the `*Messages*` buffer, and the keypad exits.
4.  Press `ESC` anytime to cancel and exit the keypad.
5.  Press `Backspace` or `DEL` to undo the last keypress or pending modifier.

### Key Translation Logic

Evil Keypad uses the concept of **"Control-Persistent Prefixes."** When a sequence starts with such a prefix, subsequent keys (unless the literal modifier is used) will default to being `Ctrl`-modified.

* **First Key Typed:**
    * `x` (default): Translates to `C-x`. This prefix **is Control-Persistent.**
    * `c` (default): Translates to `C-c`. This prefix **is Control-Persistent.**
    * `h` (default): Translates to `C-h`. This prefix **is NOT Control-Persistent.** Subsequent keys default to literal.
    * `m` (default): Sets a pending `M-` (Meta) modifier for the *next* key. `M-` initiated sequences **are Control-Persistent.**
    * `g` (default): Sets a pending `C-M-` (Control-Meta) modifier for the *next* key. `C-M-` initiated sequences **are Control-Persistent.**
    * Any other key `k` (e.g., `a`, `f`, `SPC`): Interpreted as `C-c k`. This implicitly-started `C-c` prefix **is NOT Control-Persistent.** Subsequent keys default to literal.

* **Subsequent Keys:**
    * `m`, `g`: Set pending `M-` or `C-M-` modifier for the *next* key. Does not change the existing Control-Persistent status of the sequence.
    * `SPC` (if not the first key): Sets a pending `literal` modifier for the *next* key. This makes only the next key literal and does not change the Control-Persistent status.
    * **If a modifier was pending (`M-`, `C-M-`, `literal`):** It's applied to the current typed key.
    * **If no modifier was pending:**
        * If the sequence is currently in a **Control-Persistent** context (started by `x`, `c` trigger, `m` trigger, or `g` trigger): The current key `k` is modified with `Control` (becomes `C-k`).
        * Else (context is not Control-Persistent, e.g., after `h` trigger or `C-c k` from "other first key"): The current key `k` is treated literally.

* **Case:** Uppercase letters interact as expected with `Control` and `Meta` modifiers (e.g., `x F` translates to `C-x C-S-f`, but `m F` translates to `M-F`).

**Examples:**

| Keypad Input | Translated Emacs Sequence | Notes                                                         |
| :----------- | :------------------------ | :------------------------------------------------------------ |
| `a`          | `C-c a`                   | Implicit `C-c` is *not* Control-Persistent.                 |
| `a s`        | `C-c a s`                 | `s` is literal as context wasn't Control-Persistent.        |
| `c`          | `C-c`                     | Explicit `C-c` (from `c` trigger) *is* Control-Persistent.    |
| `c a`        | `C-c C-a`                 | `a` becomes `C-a` due to Control-Persistent context.        |
| `x f`        | `C-x C-f`                 | Explicit `C-x` (from `x` trigger) *is* Control-Persistent.    |
| `x SPC t`    | `C-x t`                   | `SPC` makes `t` literal; context remains Control-Persistent. |
| `x SPC t a`  | `C-x t C-a`               | `t` is literal; `a` gets `C-` from Control-Persistent context. |
| `m s`        | `M-s`                     | `m` sets pending `M-`; sequence start *is* Control-Persistent. |
| `m x f`      | `M-x C-f`                 | `x` is `M-x`; `f` becomes `C-f` due to Control-Persistent context from `M-`. |
| `h v`        | `C-h v`                   | `C-h` (from `h` trigger) is *not* Control-Persistent.       |


## Which-Key Integration

* Evil Keypad integrates with `which-key` to show available bindings after a prefix is entered.
* This requires the `which-key` package to be installed and `which-key-mode` to be enabled (`M-x which-key-mode`).
* *Note:* `which-key` is bundled with Emacs starting from version 30. For Emacs 29 and earlier, ensure the `which-key` package is installed (e.g., from ELPA/NonGNU ELPA). Evil Keypad requires Emacs >= 29.1.

## Customization

You can customize the trigger keys used by Evil Keypad via `M-x customize-group RET evil-keypad RET`.

| Variable                        | Default | Purpose                                                         |
| :------------------------------ | :------ | :-------------------------------------------------------------- |
| `evil-keypad-M-trigger`         | `m`     | Key to trigger `M-` for the next input.                         |
| `evil-keypad-C-M-trigger`       | `g`     | Key to trigger `C-M-` for the next input.                       |
| `evil-keypad-literal-trigger`   | `SPC`   | Key (after first key) for literal next input (no default `C-`). |
| `evil-keypad-C-x-trigger`       | `x`     | First key to represent the `C-x` prefix.                        |
| `evil-keypad-C-c-trigger`       | `c`     | First key to represent the `C-c` prefix.                        |
| `evil-keypad-C-h-trigger`       | `h`     | First key to represent the `C-h` prefix.                        |

## License

This package is licensed under the GNU General Public License v3.0.
