[![License GPL 3][badge-license]][copying]
[![CI](https://github.com/bbatsov/crux/actions/workflows/ci.yml/badge.svg)](https://github.com/bbatsov/crux/actions/workflows/ci.yml)
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# crux

A **C**ollection of **R**idiculously **U**seful e**X**tensions for Emacs.
crux bundles many useful interactive commands to enhance your
overall Emacs experience.

Most of the crux commands are related to the editing experience, but
there are also a bunch of utility commands that are just very useful
to have (e.g. `crux-open-with` and `crux-reopen-as-root`).

## Origins of crux

Many of the functions in crux started life as blog posts on
[Emacs Redux](https://emacsredux.com), then were included in
[Emacs Prelude](https://www.github.com/bbatsov/prelude), before finally
being [extracted](https://emacsredux.com/blog/2016/01/30/crux/)
to crux. You can see a full list of blog posts on functions
in crux on the [tags page](https://emacsredux.com/tags/#crux).

## Installation

crux requires Emacs 28.1 or newer.

It's available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `crux` using the following command:

<kbd>M-x package-install [RET] crux [RET]</kbd>

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

Alternatively, you can add the following code to your Emacs config:

```el
(unless (package-installed-p 'crux)
  (package-refresh-contents)
  (package-install 'crux))
```

## Keybindings

crux doesn't setup any keybindings for its commands out-of-the-box.
There are several reasons for this:

* Most users probably won't need all the commands, so it'd be an overkill to
define a minor mode consuming a lot of valuable keybindings
* Many of the optimal keybindings are in the user space anyways (e.g. `C-c some-letter`)
* Everyone has their own preferences when it comes to keybindings

Here's the list of some suggested keybindings. Feel free to bind
individual commands to whatever keybindings you prefer.

Command                                             | Suggested Keybinding(s)         | Description
----------------------------------------------------|---------------------------------|------------------------
`crux-open-with`                                    | <kbd>C-c o</kbd>   | Open the currently visited file with an external program.
`crux-smart-kill-line`                              | <kbd>C-k</kbd> | First kill to end of line, then kill the whole line.
`crux-smart-open-line-above`                        | <kbd>C-S-RET</kbd> or <kbd>Super-o</kbd> | Insert an empty line above the current line and indent it properly.
`crux-smart-open-line`                              | <kbd>S-RET</kbd> | Insert an empty line and indent it properly (as in most IDEs).
`crux-cleanup-buffer-or-region`                     | <kbd>C-c n</kbd> | Fix indentation and strip whitespace in the region, or in the whole buffer.
`crux-recentf-find-file`                            | <kbd>C-c f</kbd> or <kbd>Super-r</kbd> | Open recently visited file.
`crux-recentf-find-directory`                       | <kbd>C-c F</kbd> | Open recently visited directory.
`crux-view-url`                                     | <kbd>C-c u</kbd> | Open a new buffer containing the contents of URL.
`crux-eval-and-replace`                             | <kbd>C-c e</kbd> | Eval a bit of Emacs Lisp code and replace it with its result.
`crux-transpose-windows`                            | <kbd>C-x 4 t</kbd> | Transpose the buffers between two windows.
`crux-delete-file-and-buffer`                       | <kbd>C-c D</kbd> | Delete current file and buffer.
`crux-copy-file-preserve-attributes`                | <kbd>C-c c</kbd> | Copy current file with file attributes preserved
`crux-duplicate-current-line-or-region`             | <kbd>C-c d</kbd> | Duplicate the current line (or region).
`crux-duplicate-and-comment-current-line-or-region` | <kbd>C-c M-d</kbd> | Duplicate and comment the current line (or region).
`crux-rename-file-and-buffer`                       | <kbd>C-c r</kbd> | Rename the current buffer and its visiting file if any.
`crux-visit-term-buffer`                            | <kbd>C-c t</kbd> | Open a terminal emulator (`ansi-term`).
`crux-visit-shell-buffer`                           | <kbd>C-c z</kbd> | Open a shell buffer (`eshell`).
`crux-kill-other-buffers`                           | <kbd>C-c k</kbd> | Kill all open buffers except the one you're currently in.
`crux-indent-defun`                                 | <kbd>C-M-z</kbd> | Indent the definition at point.
`crux-indent-rigidly-and-copy-to-clipboard`         | <kbd>C-c TAB</kbd> | Indent and copy region to clipboard
`crux-sudo-edit`                                    | <kbd>C-c s</kbd> | Edit currently visited file as root.
`crux-find-user-init-file`                          | <kbd>C-c I</kbd> | Open user's init file.
`crux-find-user-custom-file`                        | <kbd>C-c ,</kbd> | Open user's custom file.
`crux-find-shell-init-file`                         | <kbd>C-c S</kbd> | Open shell's init file.
`crux-find-current-directory-dir-locals-file`       | <kbd>C-c l</kbd> | Open current directory's `.dir-locals.el` file.
`crux-top-join-line`                                | <kbd>Super-j</kbd> or <kbd>C-^</kbd> | Join lines
`crux-kill-whole-line`                              | <kbd>Super-k</kbd> | Kill whole line
`crux-kill-line-backwards`                          | <kbd>C-Backspace</kbd> | Kill line backwards
`crux-kill-and-join-forward`                        | <kbd>C-S-Backspace</kbd> | If at end of line, join with following; otherwise kill line.
`crux-kill-buffer-truename`                         | <kbd>C-c P</kbd> | Kill absolute path of file visited in current buffer.
`crux-ispell-word-then-abbrev`                      | <kbd>C-c i</kbd> | Fix word using `ispell` and then save to `abbrev`.
`crux-upcase-region`                                | <kbd>C-x C-u</kbd> | `upcase-region` when `transient-mark-mode` is on and region is active.
`crux-downcase-region`                              | <kbd>C-x C-l</kbd> | `downcase-region` when `transient-mark-mode` is on and region is active.
`crux-capitalize-region`                            | <kbd>C-x M-c</kbd> | `capitalize-region` when `transient-mark-mode` is on and region is active.
`crux-switch-to-previous-buffer`                    | <kbd>C-c b</kbd> | Switch to previously open buffer. Repeated calls toggle between the two most recent buffers.
`crux-other-window-or-switch-buffer`                | <kbd>M-o</kbd>     | Select other window, or switch to most recent buffer if only one window.
`crux-move-beginning-of-line`                       | <kbd>C-a</kbd>     | Move point to first non-whitespace character, or to the beginning of the line.
`crux-move-to-mode-line-start`                      |                    | Move to the start of the line's content, skipping prompts and markup like Org heading stars.
`crux-insert-date`                                  | <kbd>C-c T</kbd> | Insert a timestamp according to locale's date and time format.
`crux-create-scratch-buffer`                        |                                 | Create a new scratch buffer.
`crux-keyboard-quit-dwim`                           | <kbd>C-g</kbd>     | Do-What-I-Mean `keyboard-quit`. Closes the minibuffer or completions buffer even without focusing it.

Here's how you'd bind some of the commands to keycombos:

```el
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key [remap keyboard-quit] #'crux-keyboard-quit-dwim)
```

For `crux-ispell-word-then-abbrev` to be most effective you'll also need to add this to your config:

```el
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)
```

## Using the bundled advices

crux ships with some handy advice that can enhance the operation of existing commands.

#### `(crux-with-region-or-buffer)` ####

You can use `crux-with-region-or-buffer` to make a command acting
normally on a region to operate on the entire buffer in the absence of
a region. Here are a few examples you can stuff in your config:

```el
(crux-with-region-or-buffer indent-region)
(crux-with-region-or-buffer untabify)
```

#### `(crux-with-region-or-line)` ####

Likewise, you can use `crux-with-region-or-line` to make a command
alternately act on the current line if the mark is not active:

```el
(crux-with-region-or-line comment-or-uncomment-region)
```

#### `(crux-with-region-or-sexp-or-line)` ####

Similarly, `crux-with-region-or-sexp-or-line` makes a command that acts on the active region, or else
the current list (or string), or finally the current line:

```el
(crux-with-region-or-sexp-or-line kill-region)
```

#### `(crux-with-region-or-point-to-eol)` ####

Sometimes you might want to act on the point until the end of the
current line, rather than the whole line, in the absence of a region:

```el
(crux-with-region-or-point-to-eol kill-ring-save)
```

## Configuration

Most commands work fine out of the box, but a few of them can be tuned
via `M-x customize-group RET crux`:

* `crux-move-visually` - when non-nil, the line start commands work on
  visual lines instead of logical ones.
* `crux-line-start-regex-alist` - per-mode regexps describing what
  `crux-move-beginning-of-line` should skip at the start of a line
  (shell prompts, Org heading stars and so on).
* `crux-indent-sensitive-modes` and `crux-untabify-sensitive-modes` -
  modes (and their derived modes) where `crux-cleanup-buffer-or-region`
  won't reindent or untabify.
* `crux-term-func` and `crux-shell-func` - how `crux-visit-term-buffer`
  and `crux-visit-shell-buffer` start their buffers. `crux-shell` is
  the shell program used by the default `crux-term-func`, and there's a
  `crux-shell` function you can use for `crux-shell-func` if you prefer
  `M-x shell` to Eshell.
* `crux-shell-zsh-init-files`, `crux-shell-bash-init-files`,
  `crux-shell-tcsh-init-files`, `crux-shell-fish-init-files` and
  `crux-shell-ksh-init-files` - the files `crux-find-shell-init-file`
  looks for.

`crux-term-buffer-name` and `crux-shell-buffer-name` set the names of
the terminal and shell buffers. They're safe to set in `.dir-locals.el`
if you want a separate terminal per project.

## Built-in alternatives

Newer Emacs versions ship commands that overlap with some of crux's. If
you're on a recent Emacs you might prefer them:

crux command | Built-in alternative
-------------|---------------------
`crux-duplicate-current-line-or-region` | `duplicate-dwim` (Emacs 29.1)
`crux-recentf-find-file` | `recentf-open` (Emacs 29.1)
`crux-rename-file-and-buffer` | `rename-visited-file` (Emacs 29.1), though it doesn't go through VC
`crux-sudo-edit` | `tramp-revert-buffer-with-sudo` (Emacs 30.1)
`crux-open-with` (in Dired) | `dired-do-open` (Emacs 30.1)
`crux-indent-defun` | `prog-fill-reindent-defun` on <kbd>M-q</kbd> (Emacs 30.1)
`crux-switch-to-previous-buffer` | `mode-line-other-buffer`
`crux-upcase-region` and friends | `upcase-dwim`, `downcase-dwim` and `capitalize-dwim`, which fall back to the word at point instead of doing nothing

## Minor modes

#### `(crux-reopen-as-root-mode)` ####

Crux provides a `crux-reopen-as-root` command for reopening a file as
root. This global minor mode changes `find-file` so all root files are
automatically opened as root.

## License

Copyright © 2015-2025 Bozhidar Batsov and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: https://melpa.org/packages/crux-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/crux-badge.svg
[melpa-package]: https://melpa.org/#/crux
[melpa-stable-package]: https://stable.melpa.org/#/crux
[COPYING]: https://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/bbatsov/crux/contributors
[melpa]: https://melpa.org
[melpa stable]: https://stable.melpa.org
