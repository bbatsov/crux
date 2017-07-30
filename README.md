[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# crux

A **C**ollection of **R**idiculously **U**seful e**X**tensions for Emacs.
crux bundles a few useful interactive commands to enhance your
overall Emacs experience.

## Installation

Available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `crux` using the following command:

<kbd>M-x package-install [RET] crux [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'crux)
  (package-refresh-contents)
  (package-install 'crux))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

## Keybindings

crux doesn't setup any keybindings for its commands out-of-the-box.

Here's the list of some suggested keybindings. Feel free to bind
individual commands to whatever keybindings you prefer.

Command                                             | Suggested Keybinding(s)         | Description
----------------------------------------------------|---------------------------------|------------------------
`crux-open-with`                                    | <kbd>C-c o</kbd>   | Open the currently visited file with an external program.
`crux-smart-kill-line`                              | <kbd>C-k</kbd> or <kbd>Super-k</kbd> | First kill to end of line, then kill the whole line.
`crux-smart-open-line-above`                        | <kbd>C-S-RET</kbd> or <kbd>Super-o</kbd> | Insert an empty line above the current line and indent it properly.
`crux-smart-open-line`                              | <kbd>S-RET</kbd> or <kbd>M-o</kbd> | Insert an empty line and indent it properly (as in most IDEs).
`crux-cleanup-buffer-or-region`                     | <kbd>C-c n</kbd> | Fix indentation in buffer and strip whitespace.
`crux-recentf-find-file`                            | <kbd>C-c f</kbd> or <kbd>Super-r</kbd> | Open recently visited file.
`crux-view-url`                                     | <kbd>C-c u</kbd> | Open a new buffer containing the contents of URL.
`crux-eval-and-replace`                             | <kbd>C-c e</kbd> | Eval a bit of Emacs Lisp code and replace it with its result.
`crux-transpose-windows`                            | <kbd>C-x 4 t</kbd> | Transpose the buffers between two windows.
`crux-delete-file-and-buffer`                       | <kbd>C-c D</kbd> | Delete current file and buffer.
`crux-duplicate-current-line-or-region`             | <kbd>C-c d</kbd> | Duplicate the current line (or region).
`crux-duplicate-and-comment-current-line-or-region` | <kbd>C-c M-d</kbd> | Duplicate and comment the current line (or region).
`crux-rename-file-and-buffer`                       | <kbd>C-c r</kbd> | Rename the current buffer and its visiting file if any.
`crux-visit-term-buffer`                            | <kbd>C-c t</kbd> | Open a terminal emulator (`ansi-term`).
`crux-kill-other-buffers`                           | <kbd>C-c k</kbd> | Kill all open buffers except the one you're currently in.
`crux-indent-defun`                                 | <kbd>C-M z</kbd> | Indent the definition at point.
`crux-indent-rigidly-and-copy-to-clipboard`         | <kbd>C-c TAB</kbd> | Indent and copy region to clipboard
`crux-find-user-init-file`                          | <kbd>C-c I</kbd> | Open user's init file.
`crux-find-shell-init-file`                         | <kbd>C-c S</kbd> | Open shell's init file.
`crux-top-join-line`                                | <kbd>Super-j</kbd> or <kbd>C-^</kbd> | Join lines
`crux-kill-whole-line`                              | <kbd>Super-k</kbd> | Kill whole line
`crux-kill-line-backwards`                          | <kbd>C-Backspace</kbd> | Kill line backwards
`crux-ispell-word-then-abbrev`                      | <kbd>C-c i</kbd> | Fix word using `ispell` and then save to `abbrev`.

Here's how you'd bind some of the commands to keycombos:

```el
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
```

For `crux-ispell-word-then-abbrev` to be most effective you'll also need to add this to your config:

```el
(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)
```

## Using the bundled advices

crux ships with some handy advises that can enhance the operation of existing commands.

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

#### `(crux-with-region-or-point-to-eol)` ####

Sometimes you might want to act on the point until the end of the
current line, rather than the whole line, in the absence of a region:

``` el
(crux-with-region-or-point-to-eol kill-ring-save)
```

## Minor modes

#### `(crux-reopen-as-root-mode)` ####

Crux provides a `crux-reopen-as-root` command for reopening a file as
root. This global minor mode changes `find-file` so all root files are
automatically opened as root.

## License

Copyright © 2015 Bozhidar Batsov and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/crux-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/crux-badge.svg
[melpa-package]: http://melpa.org/#/crux
[melpa-stable-package]: http://stable.melpa.org/#/crux
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[contributors]: https://github.com/bbatsov/crux/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
