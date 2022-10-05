# Changelog

## master (unreleased)

* [#94](https://github.com/bbatsov/crux/pull/94): Add `crux-with-region-or-sexp-or-line`.

## 0.4.0 (2021-08-10)

### New features

* [#65](https://github.com/bbatsov/crux/pull/65): Add a configuration option to move using visual lines in `crux-move-to-mode-line-start`.
* [#72](https://github.com/bbatsov/crux/pull/72): Add `crux-kill-buffer-truename`. Kills path of file visited by current buffer.
* [#78](https://github.com/bbatsov/crux/pull/78): Add `crux-recentf-find-directory`. Open recently visited directory.
* Add `crux-copy-file-preserve-attribute`.
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

## 0.3.0 (2016-05-31)
