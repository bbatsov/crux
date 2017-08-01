;;; crux.el --- A Collection of Ridiculously Useful eXtensions -*- lexical-binding: t -*-
;;
;; Copyright © 2015-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/crux
;; Version: 0.3.0
;; Keywords: convenience
;; Package-Requires: ((seq "1.11"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A cornucopia of useful interactive commands to make your Emacs
;; experience more enjoyable.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thingatpt)
(require 'seq)
(require 'tramp)

(declare-function dired-get-file-for-visit "dired")

(defgroup crux nil
  "crux configuration."
  :prefix "crux-"
  :group 'convenience)

(defcustom crux-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'crux)

(defcustom crux-untabify-sensitive-modes
  '(makefile-bsdmake-mode)
  "Modes for which untabify is suppressed."
  :type 'list
  :group 'crux)

(defcustom crux-shell (getenv "SHELL")
  "The default shell to run with `crux-visit-term-buffer'."
  :type 'string
  :group 'crux)

(defcustom crux-shell-zsh-init-files '("$HOME/.zshrc" "$HOME/.zlogin" "$HOME/.zprofile" "$HOME/.zshenv" "$HOME/.zlogout" "/etc/zshenv" "/etc/zprofile" "/etc/zshrc" "/etc/zlogin" "/etc/zlogout" "$ZDOTDIR/.zshrc" "$ZDOTDIR/.zlogin" "$ZDOTDIR/.zprofile" "$ZDOTIR/.zshenv" "$ZDOTDIR/.zlogout")
  "The default init files of zsh."
  :type 'list
  :group 'crux)

(defcustom crux-shell-bash-init-files '("$BASH_ENV" "$HOME/.bashrc" "$HOME/.bash_profile" "$HOME/.bash_login" "$HOME/.profile" "$HOME/.bash_logout" "/etc/bashrc" "/etc/bash_profile" "/etc/bash_login" "/etc/profile" "/etc/bash_logout")
  "The default init files of bash."
  :type 'list
  :group 'crux)

(defcustom crux-shell-tcsh-init-files '("$HOME/.login" "$HOME/.cshrc" "$HOME/.tcshrc" "$HOME/.logout" "/etc/csh.cshrc" "/etc/csh.login" "/etc/csh.logout")
  "The default init files of tcsh."
  :type 'list
  :group 'crux)

(defcustom crux-shell-fish-init-files '("$HOME/.config/fish/config.fish" "$XDG_CONFIG_HOME/fish/config.fish")
  "The default init files of fish."
  :type 'list
  :group 'crux)

(defcustom crux-shell-ksh-init-files '("$HOME/.profile" "$ENV" "/etc/profile")
  "The default init files of ksh."
  :type 'list
  :group 'crux)

(defun crux-open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(defun crux-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defvar crux-term-buffer-name "ansi"
  "The default `ansi-term' name used by `crux-visit-term-buffer'.
This variable can be set via .dir-locals.el to provide multi-term support.")

(defun crux-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun crux-visit-term-buffer ()
  "Create or visit a terminal buffer.
If the process in that buffer died, ask to restart."
  (interactive)
  (crux-start-or-switch-to (lambda ()
                             (ansi-term crux-shell (concat crux-term-buffer-name "-term")))
                           (format "*%s-term*" crux-term-buffer-name))
  (when (and (null (get-buffer-process (current-buffer)))
             (y-or-n-p "The process has died. Do you want to restart it? "))
    (kill-buffer-and-window)
    (crux-visit-term-buffer)))

(defun crux-indent-rigidly-and-copy-to-clipboard (begin end arg)
  "Indent region between BEGIN and END by ARG columns and copy to clipboard."
  (interactive "r\nP")
  (let ((arg (or arg 4))
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) arg)
      (clipboard-kill-ring-save (point-min) (point-max)))))

(defun crux-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (insert "\n")
  (if electric-indent-inhibit
      ;; We can't use `indent-according-to-mode' in languages like Python,
      ;; as there are multiple possible indentations with different meanings.
      (let* ((indent-end (progn (move-to-mode-line-start) (point)))
             (indent-start (progn (move-beginning-of-line nil) (point)))
             (indent-chars (buffer-substring indent-start indent-end)))
        (forward-line -1)
        ;; This new line should be indented with the same characters as
        ;; the current line.
        (insert indent-chars))
    ;; Just use the current major-mode's indent facility.
    (progn
      (forward-line -1)
      (indent-according-to-mode))))

(defun crux-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (crux-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun crux-smart-kill-line (arg)
  "Kill to the end of the line and kill whole line on the next call"
  (interactive "P")
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (crux-kill-whole-line)
      (progn
        (goto-char orig-point)
        (kill-line))
      )))


(defun crux-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun crux-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (move-to-mode-line-start))

(defun crux-kill-line-backwards ()
  "Kill line backwards and adjust the indentation."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defvar crux-line-start-regex-term-mode "^[^#$%>\n]*[#$%>] "
  "Match terminal prompts.

Used by crux functions like crux-move-beginning-of-line to skip over the prompt")

(defvar crux-line-start-regex-eshell-mode "^[^$\n]*$ " "Match eshell prompt.

Used by crux functions like crux-move-beginning-of-line to skip over the prompt")

(defvar crux-line-start-regex "^[[:space:]]*" "Match whitespace in from of line.

Used by crux functions like crux-move-beginning-of-line to skip over whitespace")

(defun move-to-mode-line-start ()
  "Move to the beginning, skipping mode specific line start regex."
  (interactive)
  (move-beginning-of-line nil)
  (let ((line-start-regex (cond ((eq major-mode 'term-mode) crux-line-start-regex-term-mode)
                                ((eq major-mode 'eshell-mode) crux-line-start-regex-eshell-mode)
                                (t crux-line-start-regex))))
    (search-forward-regexp line-start-regex (line-end-position) t)))


(defun crux-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-to-mode-line-start)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun crux-indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(defun crux-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun crux-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
   If there's no region, the current line will be duplicated.  However, if
   there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
   If there's no region, the current line will be duplicated.  However, if
   there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (crux-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (dotimes (_ arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun crux-rename-file-and-buffer ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'crux-rename-buffer-and-file #'crux-rename-file-and-buffer)

(defun crux-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defalias 'crux-delete-buffer-and-file #'crux-delete-file-and-buffer)

(defun crux-view-url ()
  "Open a new buffer containing the contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (goto-char (point-min))
    (re-search-forward "^$")
    (delete-region (point-min) (point))
    (delete-blank-lines)
    (set-auto-mode)))

(defun crux-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode crux-untabify-sensitive-modes)
    (call-interactively #'untabify))
  (unless (member major-mode crux-indent-sensitive-modes)
    (call-interactively #'indent-region))
  (whitespace-cleanup))

(defun crux-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%s" value))))

(defun crux-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun crux-file-owner-uid (filename)
  "Return the UID of the FILENAME as an integer.

See `file-attributes' for more info."
  (nth 2 (file-attributes filename 'integer)))

(defun crux-file-owned-by-user-p (filename)
  "Return t if file FILENAME is owned by the currently logged in user."
  (equal (crux-file-owner-uid filename)
         (user-uid)))

(defun crux-find-alternate-file-as-root (filename)
  "Wraps `find-alternate-file' with opening FILENAME as root."
  (find-alternate-file (concat "/sudo:root@localhost:" filename)))

(require 'ido)
(defun crux-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (crux-find-alternate-file-as-root buffer-file-name)))

(defun crux-reopen-as-root ()
  "Find file as root if necessary."
  (unless (or (tramp-tramp-file-p buffer-file-name)
              (equal major-mode 'dired-mode)
              (not (file-exists-p (file-name-directory buffer-file-name)))
              (file-writable-p buffer-file-name)
              (crux-file-owned-by-user-p buffer-file-name))
    (crux-find-alternate-file-as-root buffer-file-name)))

;;;###autoload
(define-minor-mode crux-reopen-as-root-mode
  "Automatically reopen files as root if we can't write to them
as the current user."
  :global t
  (if crux-reopen-as-root-mode
      (add-hook 'find-file-hook #'crux-reopen-as-root)
    (remove-hook 'find-file-hook #'crux-reopen-as-root)))

(defun crux-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun crux-recentf-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (completing-read "Choose recent file: "
                               (mapcar #'abbreviate-file-name recentf-list)
                               nil t)))
    (when file
      (find-file file))))

(defalias 'crux-recentf-ido-find-file 'crux-recentf-find-file)

;; modified from https://www.emacswiki.org/emacs/TransposeWindows
(defun crux-transpose-windows (arg)
  "Transpose the buffers shown in two windows.
Prefix ARG determines if the current windows buffer is swapped
with the next or previous window, and the number of
transpositions to execute in sequence."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (cl-plusp arg) (1- arg) (1+ arg))))))

(defalias 'crux-swap-windows 'crux-transpose-windows)

(defun crux-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun crux-kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all buffers but the current one? ")
    (seq-each
     #'kill-buffer
     (delete (current-buffer) (seq-filter #'buffer-file-name (buffer-list))))))

(defun crux-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)))

(defun crux-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun crux-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/" t))))
         (shell-init-file (cond
                           ((string= "zsh" shell) crux-shell-zsh-init-files)
                           ((string= "bash" shell) crux-shell-bash-init-files)
                           ((string= "tcsh" shell) crux-shell-tcsh-init-files)
                           ((string= "fish" shell) crux-shell-fish-init-files)
                           ((string-prefix-p "ksh" shell) crux-shell-ksh-init-files)
                           (t (error "Unknown shell"))))
         (candidates (cl-remove-if-not 'file-exists-p (mapcar 'substitute-in-file-name shell-init-file))))
    (if (> (length candidates) 1)
        (find-file-other-window (completing-read "Choose shell init file:" candidates))
      (find-file-other-window (car candidates)))))

;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun crux-ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev.  Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer.  You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(defmacro crux-with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer.

Use to make commands like `indent-region' work on both the region
and the entire buffer (in the absense of a region)."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(defmacro crux-with-region-or-line (func)
  "When called with no active region, call FUNC on current line."
  `(defadvice ,func (before with-region-or-line activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (line-beginning-position) (line-beginning-position 2))))))

(defmacro crux-with-region-or-point-to-eol (func)
  "When called with no active region, call FUNC from the point to the end of line."
  `(defadvice ,func (before with-region-or-point-to-eol activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point) (line-end-position))))))

(provide 'crux)
;;; crux.el ends here
