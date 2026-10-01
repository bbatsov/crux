# Changelog

## master (unreleased)

### New features

* [#101](https://github.com/bbatsov/crux/pull/101): Add `crux-find-current-directory-dir-locals-file`.
* [#104](https://github.com/bbatsov/crux/pull/104): Add `crux-keyboard-quit-dwim`.

### Changes

* [#110](https://github.com/bbatsov/crux/pull/110): Require Emacs 28.1.
* The `crux-with-region-or-*` macros use `advice-add` instead of `defadvice`, which was removed in Emacs 31.
* [#110](https://github.com/bbatsov/crux/pull/110): Don't load TRAMP when crux is loaded.
* [#110](https://github.com/bbatsov/crux/pull/110): Mark `crux-recompile-init` as obsolete.
* [#110](https://github.com/bbatsov/crux/pull/110): The `crux-with-region-or-*` macros and the line duplication commands check `use-region-p`, so an empty region, or any region with Transient Mark mode off, no longer counts as active.
* [#110](https://github.com/bbatsov/crux/pull/110): `crux-cleanup-buffer-or-region` also skips modes derived from the ones in `crux-indent-sensitive-modes` and `crux-untabify-sensitive-modes`, and the former now includes `python-ts-mode` and `yaml-ts-mode`.
* [#110](https://github.com/bbatsov/crux/pull/110): `crux-transpose-windows` keeps each window's point and scroll position.
* [#110](https://github.com/bbatsov/crux/pull/110): Mark `crux-term-buffer-name` and `crux-shell-buffer-name` as safe directory-local variables.
* [#111](https://github.com/bbatsov/crux/pull/111): Use `tramp-file-name-with-sudo` for remote files in `crux-sudo-edit` and `crux-reopen-as-root-mode` when it's available (Emacs 30.1+), and honor a customized `tramp-file-name-with-method` for local files.
* [#111](https://github.com/bbatsov/crux/pull/111): Make `crux-reopen-as-root-mode` leave Emacs's own files and packages on `load-path` alone.

### Bugs fixed

* [#102](https://github.com/bbatsov/crux/pull/102): Create nonexistent parent directories in `crux-copy-file-preserve-attributes`.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix byte-compilation on Emacs 31.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-cleanup-buffer-or-region` erroring or cleaning the wrong text unless `untabify` and `indent-region` had been advised with `crux-with-region-or-buffer`.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-rename-file-and-buffer` renaming remote version-controlled files to their old name, renaming after the user declined to save, and ignoring buffers without a file.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-sudo-edit` and `crux-reopen-as-root-mode` logging in as root over SSH for remote files, and expanding `~` to root's home directory.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-duplicate-current-line-or-region` duplicating an extra line when the region ends at the start of a line.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-duplicate-and-comment-current-line-or-region` uncommenting already commented text and misplacing point.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-eval-and-replace` ignoring `lexical-binding`.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-recentf-find-file` and `crux-recentf-find-directory` failing when `recentf` wasn't loaded yet.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-indent-defun` leaving the region active.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-open-with` failing on commands with arguments.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix the case region commands erroring when the mark was never set.
* [#110](https://github.com/bbatsov/crux/pull/110): Fix `crux-find-user-init-file` and `crux-find-shell-init-file` crashing when there's no init file.
* [#111](https://github.com/bbatsov/crux/pull/111): Fix `crux-view-url` erroring on failed fetches and cutting off content for URLs without HTTP headers.

## 0.5.0 (2024-02-29)

### New features

* [#94](https://github.com/bbatsov/crux/pull/94): Add `crux-with-region-or-sexp-or-line`.
* [#92](https://github.com/bbatsov/crux/pull/92): Consider derived modes when checking for major mode (`dired`, `org-mode`, `eshell`).

### Bugs fixed

* More robust `crux-rename-file-and-buffer`.
* Fix `sudo` not found error in OpenBSD and Alpine Linux (they use `doas`).
* [#100](https://github.com/bbatsov/crux/pull/100): More robust `crux-copy-file-preserve-attributes`.

## 0.4.0 (2021-08-10)

### New features

* [#65](https://github.com/bbatsov/crux/pull/65): Add a configuration option to move using visual lines in `crux-move-to-mode-line-start`.
* [#72](https://github.com/bbatsov/crux/pull/72): Add `crux-kill-buffer-truename`. Kills path of file visited by current buffer.
* [#78](https://github.com/bbatsov/crux/pull/78): Add `crux-recentf-find-directory`. Open recently visited directory.
* Add `crux-copy-file-preserve-attributes`.
* Add `crux-find-user-custom-file`.
* Add `crux-kill-and-join-forward`.
* Add `crux-other-window-or-switch-buffer`.
* Add support for org-mode links in `crux-view-url`.
* Add support for creating shell and terminal buffers.
* Add remote files support to `crux-sudo-edit`.
* Add `crux-smart-kill-line`.

### Changes

* Remove unused prefix argument from `crux-smart-kill-line`.
* Mark `crux-recentf-ido-find-file` as obsolete.

### Bugs fixed

* Fixed extra line issue when duplicating region.
* Various small fixes that we were too lazy to document properly.
* Fixed `sudo` not found in OpenBSD and Alpine Linux.

## 0.3.0 (2016-05-31)
