;;; crux-test.el --- Tests for crux -*- lexical-binding: t; -*-

;; Copyright © 2015-2025 Bozhidar Batsov

;;; Commentary:

;; Unit tests for crux commands.

;;; Code:

(require 'buttercup)
(require 'crux)
(require 'recentf)

;;; Movement

(describe "crux-move-beginning-of-line"
  (it "moves to the first non-whitespace character"
    (with-temp-buffer
      (insert "    hello world")
      (goto-char (point-max))
      (crux-move-beginning-of-line 1)
      (expect (current-column) :to-equal 4)))

  (it "moves to column 0 when already at first non-whitespace"
    (with-temp-buffer
      (insert "    hello world")
      (goto-char (+ (point-min) 4))
      (crux-move-beginning-of-line 1)
      (expect (current-column) :to-equal 0)))

  (it "toggles between indentation and beginning of line"
    (with-temp-buffer
      (insert "    hello world")
      (goto-char (point-max))
      ;; First call goes to indentation
      (crux-move-beginning-of-line 1)
      (expect (current-column) :to-equal 4)
      ;; Second call goes to beginning
      (crux-move-beginning-of-line 1)
      (expect (current-column) :to-equal 0)
      ;; Third call goes back to indentation
      (crux-move-beginning-of-line 1)
      (expect (current-column) :to-equal 4))))

;;; External programs

(describe "crux-open-with"
  (it "signals a user error in buffers without a file"
    (with-temp-buffer
      (expect (crux-open-with nil) :to-throw 'user-error)))

  (it "runs the entered command through the shell with the file quoted"
    (let (command)
      (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "open -a Preview"))
                ((symbol-function 'call-process-shell-command)
                 (lambda (cmd &rest _) (setq command cmd))))
        (with-temp-buffer
          (setq buffer-file-name "/tmp/my file.pdf")
          (crux-open-with t)))
      (expect command :to-equal
              (concat "open -a Preview " (shell-quote-argument "/tmp/my file.pdf"))))))

(describe "crux-move-to-mode-line-start"
  (it "skips org heading stars"
    (with-temp-buffer
      (org-mode)
      (insert "** Heading")
      (crux-move-to-mode-line-start)
      (expect (current-column) :to-equal 3)))

  (it "skips an eshell-style prompt using the mode's regexp"
    (with-temp-buffer
      (let ((crux-line-start-regex-alist
             '((fundamental-mode . "^[^$\n]*\\$ ") (default . "^[[:space:]]*"))))
        (insert "~/src $ ls")
        (crux-move-to-mode-line-start)
        (expect (current-column) :to-equal 8))))

  (it "has a default eshell regexp that skips the prompt"
    (let ((regexp (alist-get 'eshell-mode crux-line-start-regex-alist)))
      (expect (string-match regexp "~/src $ ls") :to-equal 0)
      (expect (match-end 0) :to-equal 8)))

  (it "uses the default regexp for other modes"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "  (foo)")
      (crux-move-to-mode-line-start)
      (expect (current-column) :to-equal 2))))

;;; Line editing

(describe "crux-smart-open-line"
  (it "opens a line below"
    (with-temp-buffer
      (insert "first line")
      (goto-char (point-min))
      (crux-smart-open-line nil)
      (expect (buffer-string) :to-equal "first line\n")
      (expect (point) :to-equal (point-max))))

  (it "indents the new line according to the mode"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo ()")
      (crux-smart-open-line nil)
      (expect (current-column) :to-equal 2)))

  (it "opens a line above with a prefix argument"
    (with-temp-buffer
      (insert "first line")
      (crux-smart-open-line t)
      (expect (buffer-string) :to-equal "\nfirst line")
      (expect (point) :to-equal (point-min)))))

(describe "crux-smart-open-line-above"
  (it "opens a line above"
    (with-temp-buffer
      (insert "first line")
      (goto-char (point-max))
      (crux-smart-open-line-above)
      (expect (buffer-string) :to-equal "\nfirst line")
      (expect (point) :to-equal (point-min))))

  (it "reuses the current line's indentation when electric-indent-inhibit is set"
    (with-temp-buffer
      (insert "    first line")
      (setq-local electric-indent-inhibit t)
      (crux-smart-open-line-above)
      (expect (buffer-string) :to-equal "    \n    first line")
      (expect (point) :to-equal 5))))

(describe "crux-top-join-line"
  (it "joins current line with line below"
    (with-temp-buffer
      (insert "hello\nworld")
      (goto-char (point-min))
      (crux-top-join-line)
      (expect (buffer-string) :to-equal "hello world"))))

(describe "crux-kill-whole-line"
  (it "kills the entire current line"
    (with-temp-buffer
      (insert "first\nsecond\nthird")
      (goto-char (point-min))
      (forward-line 1)
      (crux-kill-whole-line 1)
      (expect (buffer-string) :to-equal "first\nthird"))))

(describe "crux-kill-line-backwards"
  (it "kills from point to beginning of line"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo\n  bar baz)")
      (goto-char 12) ; before "baz"
      (let ((last-command nil))
        (crux-kill-line-backwards))
      (expect (buffer-string) :to-equal "(foo\n baz)")
      (expect (current-kill 0) :to-equal "  bar "))))

(describe "crux-smart-kill-line"
  (it "kills to end of line when content remains"
    (with-temp-buffer
      (insert "hello world")
      (goto-char (point-min))
      (crux-smart-kill-line)
      (expect (buffer-string) :to-equal "")))

  (it "kills whole line when point is at end of line"
    (with-temp-buffer
      (insert "first\nsecond\nthird")
      (goto-char (point-min))
      (end-of-line)
      (crux-smart-kill-line)
      (expect (buffer-string) :to-equal "second\nthird"))))

(describe "crux-kill-and-join-forward"
  (it "joins with following line when at end of line"
    (with-temp-buffer
      (insert "hello\n  world")
      (goto-char (point-min))
      (end-of-line)
      (crux-kill-and-join-forward)
      (expect (buffer-string) :to-equal "hello world")))

  (it "kills line normally when not at end"
    (with-temp-buffer
      (insert "hello world")
      (goto-char (point-min))
      (crux-kill-and-join-forward)
      (expect (buffer-string) :to-equal ""))))

;;; Duplicate

(describe "crux-duplicate-current-line-or-region"
  (it "duplicates the current line"
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      (crux-duplicate-current-line-or-region 1)
      (expect (buffer-string) :to-equal "hello\nhello")))

  (it "duplicates multiple times with numeric arg"
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      (crux-duplicate-current-line-or-region 3)
      (expect (buffer-string) :to-equal "hello\nhello\nhello\nhello")))

  (it "keeps point at the same column in the last copy"
    (with-temp-buffer
      (insert "hello\nworld")
      (goto-char 3)
      (crux-duplicate-current-line-or-region 2)
      (expect (line-number-at-pos) :to-equal 3)
      (expect (current-column) :to-equal 2)))

  (it "duplicates all the lines touched by the region"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "a\nb\nc")
      (set-mark 2)
      (goto-char 4)
      (activate-mark)
      (crux-duplicate-current-line-or-region 1)
      (expect (buffer-string) :to-equal "a\nb\na\nb\nc")))

  (it "skips the line the region ends on when it ends at column 0"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "a\nb\nc\n")
      (set-mark (point-min))
      (goto-char 5)
      (activate-mark)
      (crux-duplicate-current-line-or-region 1)
      (expect (buffer-string) :to-equal "a\nb\na\nb\nc\n"))))

(describe "crux-duplicate-and-comment-current-line-or-region"
  (it "duplicates and comments the original line"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "hello")
      (goto-char (point-min))
      (crux-duplicate-and-comment-current-line-or-region 1)
      (expect (buffer-string) :to-equal ";; hello\nhello")))

  (it "keeps an already commented original commented"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert ";; hello")
      (goto-char (point-min))
      (crux-duplicate-and-comment-current-line-or-region 1)
      (goto-char (point-min))
      (expect (looking-at-p ";+ *;; hello\n;; hello\\'") :to-be t)))

  (it "puts point in the uncommented copy at the original column"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "hello")
      (goto-char 3)
      (crux-duplicate-and-comment-current-line-or-region 1)
      (expect (line-number-at-pos) :to-equal 2)
      (expect (current-column) :to-equal 2)))

  (it "puts the copy after the comment terminator in modes that have one"
    (with-temp-buffer
      (c-mode)
      (insert "int a;")
      (goto-char 3)
      (crux-duplicate-and-comment-current-line-or-region 1)
      (expect (buffer-string) :to-equal "/* int a; */\nint a;")
      (expect (current-column) :to-equal 2)))

  (it "comments every line of a multi-line region"
    (with-temp-buffer
      (emacs-lisp-mode)
      (transient-mark-mode 1)
      (insert "a\nb")
      (set-mark (point-max))
      (goto-char (point-min))
      (activate-mark)
      (crux-duplicate-and-comment-current-line-or-region 1)
      (expect (buffer-string) :to-equal ";; a\n;; b\na\nb"))))

;;; Buffer operations

(describe "crux-kill-other-buffers"
  :var (buf1 buf2 buf3)
  (before-each
    (setq buf1 (generate-new-buffer "test-file-1"))
    (setq buf2 (generate-new-buffer "test-file-2"))
    (setq buf3 (generate-new-buffer "test-file-3"))
    (with-current-buffer buf1 (setq buffer-file-name "/tmp/test1"))
    (with-current-buffer buf2 (setq buffer-file-name "/tmp/test2"))
    (with-current-buffer buf3 (setq buffer-file-name "/tmp/test3")))
  (after-each
    (when (buffer-live-p buf1) (kill-buffer buf1))
    (when (buffer-live-p buf2) (kill-buffer buf2))
    (when (buffer-live-p buf3) (kill-buffer buf3)))

  (it "kills file-visiting buffers except the current one"
    (with-current-buffer buf1
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (crux-kill-other-buffers)))
    (expect (buffer-live-p buf1) :to-be t)
    (expect (buffer-live-p buf2) :to-be nil)
    (expect (buffer-live-p buf3) :to-be nil)))

(describe "crux-create-scratch-buffer"
  (it "creates a new scratch buffer"
    (let ((buf (progn (crux-create-scratch-buffer) (current-buffer))))
      (expect (buffer-name buf) :to-match "\\*scratch\\*")
      (kill-buffer buf))))

(describe "crux-switch-to-previous-buffer"
  (it "switches to the previous buffer"
    (let ((buf1 (generate-new-buffer "prev-test-1"))
          (buf2 (generate-new-buffer "prev-test-2")))
      (unwind-protect
          (progn
            (switch-to-buffer buf1)
            (switch-to-buffer buf2)
            (crux-switch-to-previous-buffer)
            (expect (current-buffer) :to-be buf1))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

;;; Recent files

(describe "crux-recentf-find-file"
  (it "enables recentf-mode and offers the recent files"
    (let ((recentf-mode nil)
          (recentf-list '("/tmp/a.txt" "/tmp/b.txt"))
          candidates visited)
      (cl-letf (((symbol-function 'recentf-mode) (lambda (&rest _) (setq recentf-mode t)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq candidates coll) (car coll)))
                ((symbol-function 'find-file) (lambda (f &rest _) (setq visited f))))
        (crux-recentf-find-file))
      (expect recentf-mode :to-be t)
      (expect candidates :to-equal '("/tmp/a.txt" "/tmp/b.txt"))
      (expect visited :to-equal "/tmp/a.txt"))))

(describe "crux-recentf-find-directory"
  (it "offers the unique directories of recent files"
    (let ((recentf-mode t)
          (recentf-list '("/tmp/a.txt" "/tmp/b.txt" "/var/c.txt"))
          candidates)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq candidates coll) nil)))
        (crux-recentf-find-directory))
      (expect candidates :to-equal '("/tmp/" "/var/")))))

(describe "crux-transpose-windows"
  (it "swaps the buffers and keeps point in the current buffer"
    (save-window-excursion
      (delete-other-windows)
      (let* ((buf1 (generate-new-buffer "transpose-1"))
             (buf2 (generate-new-buffer "transpose-2"))
             (win1 (selected-window))
             (win2 (split-window)))
        (unwind-protect
            (progn
              (set-window-buffer win1 buf1)
              (set-window-buffer win2 buf2)
              (with-current-buffer buf1 (insert "hello world"))
              (set-window-point win1 7)
              (crux-transpose-windows 1)
              (expect (window-buffer win1) :to-be buf2)
              (expect (window-buffer win2) :to-be buf1)
              (expect (selected-window) :to-be win2)
              (expect (window-point win2) :to-equal 7))
          (kill-buffer buf1)
          (kill-buffer buf2))))))

(describe "crux-other-window-or-switch-buffer"
  (it "switches to the other window when there is one"
    (save-window-excursion
      (delete-other-windows)
      (let ((win2 (split-window)))
        (crux-other-window-or-switch-buffer)
        (expect (selected-window) :to-be win2))))

  (it "switches to the most recent buffer with a single window"
    (save-window-excursion
      (delete-other-windows)
      (let ((buf1 (generate-new-buffer "owsb-1"))
            (buf2 (generate-new-buffer "owsb-2")))
        (unwind-protect
            (progn
              (switch-to-buffer buf1)
              (switch-to-buffer buf2)
              (crux-other-window-or-switch-buffer)
              (expect (current-buffer) :to-be buf1))
          (kill-buffer buf1)
          (kill-buffer buf2))))))

;;; File path

(describe "crux-kill-buffer-truename"
  (it "copies the file path to kill ring"
    (with-temp-buffer
      (setq buffer-file-name "/tmp/test-file.txt")
      (crux-kill-buffer-truename)
      (expect (current-kill 0) :to-match "test-file\\.txt")))

  (it "shows message when buffer has no file"
    (with-temp-buffer
      (expect (crux-kill-buffer-truename)
              :not :to-throw))))

;;; Region operations

(describe "crux-upcase-region"
  (it "upcases the active region"
    (with-temp-buffer
      (insert "hello world")
      (goto-char (point-min))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (crux-upcase-region (point-min) (point-max))
      (expect (buffer-string) :to-equal "HELLO WORLD"))))

(describe "crux-downcase-region"
  (it "downcases the active region"
    (with-temp-buffer
      (insert "HELLO WORLD")
      (goto-char (point-min))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (crux-downcase-region (point-min) (point-max))
      (expect (buffer-string) :to-equal "hello world"))))

(describe "crux-capitalize-region"
  (it "capitalizes the active region"
    (with-temp-buffer
      (insert "hello world")
      (goto-char (point-min))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (crux-capitalize-region (point-min) (point-max))
      (expect (buffer-string) :to-equal "Hello World"))))

(describe "crux-upcase-region without an active region"
  (it "does nothing, even if the mark was never set"
    (with-temp-buffer
      (insert "hello")
      (call-interactively #'crux-upcase-region)
      (expect (buffer-string) :to-equal "hello")))

  (it "does nothing when the mark is set but inactive"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "hello")
      (set-mark (point-min))
      (deactivate-mark)
      (call-interactively #'crux-upcase-region)
      (expect (buffer-string) :to-equal "hello"))))

;;; Date insertion

(describe "crux-insert-date"
  (it "inserts a non-empty timestamp"
    (with-temp-buffer
      (crux-insert-date)
      (expect (buffer-string) :not :to-equal ""))))

;;; Eval and replace

(describe "crux-eval-and-replace"
  (it "replaces sexp with its value"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(+ 1 2)")
      (goto-char (point-max))
      (crux-eval-and-replace)
      (expect (buffer-string) :to-equal "3")))

  (it "evaluates with lexical binding in lexical-binding buffers"
    (with-temp-buffer
      (emacs-lisp-mode)
      (setq lexical-binding t)
      (insert "(funcall (let ((x 1)) (lambda () x)))")
      (crux-eval-and-replace)
      (expect (buffer-string) :to-equal "1"))))

;;; Cleanup

(describe "crux-cleanup-buffer-or-region"
  (it "cleans up the whole buffer when there's no region"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo\n\t\t\tbar)   \n")
      (crux-cleanup-buffer-or-region)
      (expect (buffer-string) :to-equal "(foo\n bar)\n")))

  (it "leaves text outside the active region alone"
    (with-temp-buffer
      (emacs-lisp-mode)
      (transient-mark-mode 1)
      (insert "(a)   \n(b)   \n")
      (set-mark (point-min))
      (goto-char 8) ; the whole first line
      (activate-mark)
      (crux-cleanup-buffer-or-region)
      (expect (buffer-string) :to-equal "(a)\n(b)   \n")))

  (it "treats an empty active region like no region at all"
    (with-temp-buffer
      (emacs-lisp-mode)
      (transient-mark-mode 1)
      (insert "(foo\nbar)   \n")
      (set-mark (point))
      (activate-mark)
      (crux-cleanup-buffer-or-region)
      (expect (buffer-string) :to-equal "(foo\n bar)\n")))

  (it "doesn't reindent in modes derived from an indent-sensitive mode"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(foo\nbar)\n")
      (let ((crux-indent-sensitive-modes '(lisp-data-mode)))
        (crux-cleanup-buffer-or-region))
      (expect (buffer-string) :to-equal "(foo\nbar)\n"))))

;;; Rename and delete

(describe "crux-rename-file-and-buffer"
  :var (dir file buf)
  (before-each
    (setq dir (file-name-as-directory (make-temp-file "crux-test" t)))
    (setq file (expand-file-name "old.txt" dir))
    (with-temp-file file (insert "content"))
    (setq buf (find-file-noselect file)))
  (after-each
    (when (buffer-live-p buf)
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf))
    (delete-directory dir t))

  (it "renames the file and the buffer"
    (let ((new (expand-file-name "new.txt" dir)))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) new)))
          (crux-rename-file-and-buffer))
        (expect buffer-file-name :to-equal new))
      (expect (file-exists-p new) :to-be t)
      (expect (file-exists-p file) :to-be nil)))

  (it "moves the file into a directory, keeping its name"
    (let ((subdir (file-name-as-directory (expand-file-name "sub" dir))))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) subdir)))
          (crux-rename-file-and-buffer))
        (expect buffer-file-name :to-equal (expand-file-name "old.txt" subdir)))))

  (it "aborts when the user declines to save a modified buffer"
    (let ((new (expand-file-name "new.txt" dir)))
      (with-current-buffer buf
        (insert "more")
        (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) new))
                  ((symbol-function 'y-or-n-p) #'ignore))
          (expect (crux-rename-file-and-buffer) :to-throw 'user-error)))
      (expect (file-exists-p file) :to-be t)
      (expect (file-exists-p new) :to-be nil)))

  (it "renames just the buffer when it is not visiting a file"
    (with-temp-buffer
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "crux-renamed")))
        (crux-rename-file-and-buffer))
      (expect (buffer-name) :to-equal "crux-renamed"))))

(describe "crux-delete-file-and-buffer"
  :var (file buf)
  (before-each
    (setq file (make-temp-file "crux-test"))
    (setq buf (find-file-noselect file)))
  (after-each
    (when (buffer-live-p buf) (kill-buffer buf))
    (when (file-exists-p file) (delete-file file)))

  (it "deletes the file and kills the buffer after confirmation"
    (with-current-buffer buf
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (let ((delete-by-moving-to-trash nil))
          (crux-delete-file-and-buffer))))
    (expect (file-exists-p file) :to-be nil)
    (expect (buffer-live-p buf) :to-be nil))

  (it "keeps both when the user declines"
    (with-current-buffer buf
      (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
        (crux-delete-file-and-buffer)))
    (expect (file-exists-p file) :to-be t)
    (expect (buffer-live-p buf) :to-be t)))

;;; Root access

(describe "crux--root-file-name"
  (it "expands ~ as the current user before switching to root"
    (cl-letf (((symbol-function 'executable-find) #'ignore))
      (expect (crux--root-file-name "~/notes.txt")
              :to-equal (concat "/sudo:root@localhost:"
                                (expand-file-name "~/notes.txt")))))

  (it "prefers doas when it's available"
    (cl-letf (((symbol-function 'executable-find) (lambda (cmd) (equal cmd "doas"))))
      (expect (crux--root-file-name "/etc/hosts")
              :to-equal "/doas:root@localhost:/etc/hosts")))

  (it "adds a sudo hop for remote files, keeping user and port"
    (expect (crux--root-file-name "/ssh:bob@example.com#2222:/etc/hosts")
            :to-equal "/ssh:bob@example.com#2222|sudo:root@example.com:/etc/hosts")))

(describe "crux-already-root-p"
  (it "is nil for local files"
    (expect (crux-already-root-p "/etc/hosts") :to-be nil))

  (it "recognizes sudo and root file names"
    (expect (crux-already-root-p "/sudo:root@localhost:/etc/hosts") :to-be-truthy)
    (expect (crux-already-root-p "/ssh:root@example.com:/etc/hosts") :to-be-truthy)
    (expect (crux-already-root-p "/ssh:bob@example.com:/etc/hosts") :to-be nil)))

(describe "crux-sudo-edit"
  (it "prompts for a file when the buffer isn't visiting one"
    (let (visited)
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) "/etc/hosts"))
                ((symbol-function 'executable-find) #'ignore)
                ((symbol-function 'find-file) (lambda (f &rest _) (setq visited f))))
        (with-temp-buffer
          (crux-sudo-edit)))
      (expect visited :to-equal "/sudo:root@localhost:/etc/hosts")))

  (it "doesn't add another hop to a file that's already opened as root"
    (let (visited)
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _) "/sudo:root@localhost:/etc/hosts"))
                ((symbol-function 'find-file) (lambda (f &rest _) (setq visited f))))
        (with-temp-buffer
          (crux-sudo-edit)))
      (expect visited :to-equal "/sudo:root@localhost:/etc/hosts"))))

;;; Keyboard quit DWIM

(describe "crux-keyboard-quit-dwim"
  (it "closes the completions window when it's selected"
    (let (closed)
      (cl-letf (((symbol-function 'delete-completion-window)
                 (lambda () (setq closed t))))
        (with-temp-buffer
          (completion-list-mode)
          (crux-keyboard-quit-dwim)))
      (expect closed :to-be t)))

  (it "deactivates an active region"
    (with-temp-buffer
      (transient-mark-mode 1)
      (insert "hello world")
      (goto-char (point-min))
      (set-mark (point-min))
      (goto-char (point-max))
      (activate-mark)
      (expect (region-active-p) :to-be t)
      ;; keyboard-quit signals 'quit, catch it to avoid killing the test runner
      (condition-case nil
          (crux-keyboard-quit-dwim)
        (quit nil))
      (expect (region-active-p) :to-be nil))))

;;; Configuration file finders

(describe "crux-find-user-init-file"
  (it "signals a user error when Emacs was started without an init file"
    (let ((user-init-file nil))
      (expect (crux-find-user-init-file) :to-throw 'user-error))))

(describe "crux-find-user-custom-file"
  (it "visits the custom file"
    (let ((custom-file "/tmp/crux-custom.el")
          visited)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq visited f))))
        (crux-find-user-custom-file))
      (expect visited :to-equal "/tmp/crux-custom.el"))))

(describe "crux-find-shell-init-file"
  :var (dir)
  (before-each (setq dir (file-name-as-directory (make-temp-file "crux-test" t))))
  (after-each (delete-directory dir t))

  (it "visits the only existing init file directly"
    (let ((crux-shell-zsh-init-files (list (concat dir ".zshrc") (concat dir ".zlogin")))
          visited)
      (with-temp-file (concat dir ".zshrc"))
      (cl-letf (((symbol-function 'getenv) (lambda (&rest _) "/bin/zsh"))
                ((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq visited f))))
        (crux-find-shell-init-file))
      (expect visited :to-equal (concat dir ".zshrc"))))

  (it "prompts when several init files exist"
    (let ((crux-shell-bash-init-files (list (concat dir ".bashrc") (concat dir ".profile")))
          candidates)
      (with-temp-file (concat dir ".bashrc"))
      (with-temp-file (concat dir ".profile"))
      (cl-letf (((symbol-function 'getenv) (lambda (&rest _) "/bin/bash"))
                ((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq candidates coll) (car coll)))
                ((symbol-function 'find-file-other-window) #'ignore))
        (crux-find-shell-init-file))
      (expect candidates :to-equal (list (concat dir ".bashrc") (concat dir ".profile")))))

  (it "signals a user error when no init file exists"
    (let ((crux-shell-fish-init-files (list (concat dir "config.fish"))))
      (cl-letf (((symbol-function 'getenv) (lambda (&rest _) "/usr/bin/fish")))
        (expect (crux-find-shell-init-file) :to-throw 'user-error))))

  (it "signals a user error for unknown shells"
    (cl-letf (((symbol-function 'getenv) (lambda (&rest _) "/bin/nu")))
      (expect (crux-find-shell-init-file) :to-throw 'user-error))))

(describe "crux-find-current-directory-dir-locals-file"
  :var (dir)
  (before-each (setq dir (file-name-as-directory (make-temp-file "crux-test" t))))
  (after-each (delete-directory dir t))

  (it "finds the file in a parent directory"
    (let* ((sub (file-name-as-directory (expand-file-name "a/b" dir)))
           (default-directory sub)
           visited)
      (make-directory sub t)
      (with-temp-file (expand-file-name ".dir-locals.el" dir))
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq visited f))))
        (crux-find-current-directory-dir-locals-file nil))
      (expect (expand-file-name visited)
              :to-equal (expand-file-name ".dir-locals.el" dir))))

  (it "falls back to the current directory, and handles the -2 variant"
    (let ((default-directory dir)
          visited)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq visited f))))
        (crux-find-current-directory-dir-locals-file t))
      (expect (expand-file-name visited)
              :to-equal (expand-file-name ".dir-locals-2.el" dir)))))

;;; Indent

(describe "crux-indent-defun"
  (it "reindents the current defun"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo ()\n(+ 1 2))")
      (goto-char (point-min))
      (crux-indent-defun)
      (expect (buffer-string) :to-equal "(defun foo ()\n  (+ 1 2))")))

  (it "leaves the mark and region alone"
    (with-temp-buffer
      (emacs-lisp-mode)
      (transient-mark-mode 1)
      (insert "(defun foo ()\n  (+ 1 2))")
      (goto-char 5)
      (crux-indent-defun)
      (expect (mark t) :to-be nil)
      (expect (region-active-p) :to-be nil))))

;;; Copy file

(describe "crux-copy-file-preserve-attributes"
  :var (dir file buf)
  (before-each
    (setq dir (file-name-as-directory (make-temp-file "crux-test" t)))
    (setq file (expand-file-name "orig.txt" dir))
    (with-temp-file file (insert "content"))
    (set-file-modes file #o600)
    (setq buf (find-file-noselect file)))
  (after-each
    (kill-buffer buf)
    (delete-directory dir t))

  (defun crux-test--copy-to (dest &optional answer)
    "Copy the test file to DEST, answering prompts with ANSWER."
    (with-current-buffer buf
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) dest))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) answer)))
        (crux-copy-file-preserve-attributes nil))))

  (it "copies the file, keeping its permissions"
    (let ((dest (expand-file-name "copy.txt" dir)))
      (crux-test--copy-to dest)
      (expect (file-exists-p dest) :to-be t)
      (expect (file-modes dest) :to-equal #o600)))

  (it "copies into a directory when the destination ends with a slash"
    (let ((subdir (file-name-as-directory (expand-file-name "sub" dir))))
      (make-directory subdir)
      (crux-test--copy-to subdir)
      (expect (file-exists-p (expand-file-name "orig.txt" subdir)) :to-be t)))

  (it "creates a missing directory after confirmation"
    (let ((subdir (file-name-as-directory (expand-file-name "new" dir))))
      (crux-test--copy-to subdir t)
      (expect (file-exists-p (expand-file-name "orig.txt" subdir)) :to-be t)))

  (it "does nothing when the user declines to create a directory"
    (let ((subdir (file-name-as-directory (expand-file-name "new" dir))))
      (crux-test--copy-to subdir nil)
      (expect (file-exists-p subdir) :to-be nil)))

  (it "doesn't overwrite an existing file unless confirmed"
    (let ((dest (expand-file-name "existing.txt" dir)))
      (with-temp-file dest (insert "old"))
      (crux-test--copy-to dest nil)
      (expect (with-temp-buffer (insert-file-contents dest) (buffer-string))
              :to-equal "old"))))

;;; Clipboard

(describe "crux-indent-rigidly-and-copy-to-clipboard"
  (it "copies the region indented by 4 columns by default, leaving the buffer alone"
    (with-temp-buffer
      (insert "foo\nbar\n")
      (let ((last-command nil))
        (crux-indent-rigidly-and-copy-to-clipboard (point-min) (point-max) nil))
      (expect (current-kill 0) :to-equal "    foo\n    bar\n")
      (expect (buffer-string) :to-equal "foo\nbar\n"))))

;;; Advice macros

(defun crux-test--region-fn (beg end)
  "Return the region between BEG and END, for testing the advice macros."
  (interactive "r")
  (list beg end))

(defmacro crux-test--with-advice (macro &rest body)
  "Evaluate BODY with `crux-test--region-fn' advised by MACRO."
  (declare (indent 1))
  `(let ((advices-before nil))
     (advice-mapc (lambda (f _) (push f advices-before)) #'crux-test--region-fn)
     (unwind-protect
         (progn (,macro crux-test--region-fn) ,@body)
       (advice-mapc (lambda (f _)
                      (unless (memq f advices-before)
                        (advice-remove #'crux-test--region-fn f)))
                    #'crux-test--region-fn))))

(describe "crux-with-region-or-buffer"
  (it "names the advice after the function"
    (crux-test--with-advice crux-with-region-or-buffer
      (expect (advice-member-p #'crux-crux-test--region-fn-region-or-buffer
                               #'crux-test--region-fn)
              :to-be-truthy)))

  (it "uses the whole buffer when no region is active"
    (crux-test--with-advice crux-with-region-or-buffer
      (with-temp-buffer
        (insert "hello\nworld")
        (goto-char 3)
        (expect (call-interactively #'crux-test--region-fn)
                :to-equal (list (point-min) (point-max))))))

  (it "uses the region when it is active"
    (crux-test--with-advice crux-with-region-or-buffer
      (with-temp-buffer
        (transient-mark-mode 1)
        (insert "hello world")
        (set-mark 2)
        (goto-char 5)
        (activate-mark)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(2 5))))))

(describe "crux-with-region-or-line"
  (it "uses the current line, including its newline, when no region is active"
    (crux-test--with-advice crux-with-region-or-line
      (with-temp-buffer
        (insert "first\nsecond\nthird")
        (goto-char 9)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(7 14))))))

(describe "crux-with-region-or-sexp-or-line"
  (it "uses the string around point"
    (crux-test--with-advice crux-with-region-or-sexp-or-line
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "(foo \"bar baz\" 1)")
        (goto-char 9)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(6 15)))))

  (it "uses the list around point outside of strings"
    (crux-test--with-advice crux-with-region-or-sexp-or-line
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "x (foo bar) y")
        (goto-char 5)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(3 12)))))

  (it "falls back to the current line when point isn't on a sexp"
    (crux-test--with-advice crux-with-region-or-sexp-or-line
      (with-temp-buffer
        (emacs-lisp-mode)
        (insert "foo bar\nbaz")
        (goto-char 4)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(1 9))))))

(describe "crux-with-region-or-point-to-eol"
  (it "uses the text from point to the end of the line"
    (crux-test--with-advice crux-with-region-or-point-to-eol
      (with-temp-buffer
        (insert "hello world\nnext")
        (goto-char 7)
        (expect (call-interactively #'crux-test--region-fn) :to-equal '(7 12))))))

;;; crux-test.el ends here
