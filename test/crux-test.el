;;; crux-test.el --- Tests for crux -*- lexical-binding: t; -*-

;; Copyright © 2015-2025 Bozhidar Batsov

;;; Commentary:

;; Unit tests for crux commands.

;;; Code:

(require 'buttercup)
(require 'crux)

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

;;; Line editing

(describe "crux-smart-open-line"
  (it "opens a line below"
    (with-temp-buffer
      (insert "first line")
      (goto-char (point-min))
      (crux-smart-open-line nil)
      (expect (buffer-string) :to-match "first line\n"))))

(describe "crux-smart-open-line-above"
  (it "opens a line above"
    (with-temp-buffer
      (insert "first line")
      (goto-char (point-max))
      (crux-smart-open-line-above)
      (expect (buffer-string) :to-match "\nfirst line"))))

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
      (fundamental-mode)
      (insert "hello world")
      (goto-char 6) ; after "hello"
      (crux-kill-line-backwards)
      ;; indent-according-to-mode may adjust leading whitespace
      (expect (buffer-string) :to-match "world"))))

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
  (it "has an interactive spec"
    (expect (commandp #'crux-delete-file-and-buffer) :to-be t)))

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
  (it "is an interactive command"
    (expect (commandp #'crux-keyboard-quit-dwim) :to-be t))

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
  (it "is an interactive command"
    (expect (commandp #'crux-find-user-init-file) :to-be t)))

(describe "crux-find-user-custom-file"
  (it "is an interactive command"
    (expect (commandp #'crux-find-user-custom-file) :to-be t)))

(describe "crux-find-shell-init-file"
  (it "is an interactive command"
    (expect (commandp #'crux-find-shell-init-file) :to-be t)))

;;; Indent

(describe "crux-indent-defun"
  (it "reindents the current defun"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun foo ()\n(+ 1 2))")
      (goto-char (point-min))
      (crux-indent-defun)
      (expect (buffer-string) :to-equal "(defun foo ()\n  (+ 1 2))"))))

;;; Copy file

(describe "crux-copy-file-preserve-attributes"
  (it "is an interactive command"
    (expect (commandp #'crux-copy-file-preserve-attributes) :to-be t)))

;;; Advice macros

(describe "crux-with-region-or-buffer"
  (it "is a macro"
    (expect (macrop 'crux-with-region-or-buffer) :to-be t))

  (it "advises a function to operate on the entire buffer when no region is active"
    (defun crux-test--buffer-fn (beg end)
      (interactive "r")
      (upcase-region beg end))
    (crux-with-region-or-buffer crux-test--buffer-fn)
    (with-temp-buffer
      (insert "hello")
      (goto-char (point-min))
      ;; No active region — should operate on the whole buffer
      (call-interactively #'crux-test--buffer-fn)
      (expect (buffer-string) :to-equal "HELLO"))
    (advice-remove #'crux-test--buffer-fn #'crux-crux-test--buffer-fn-region-or-buffer)))

(describe "crux-with-region-or-line"
  (it "is a macro"
    (expect (macrop 'crux-with-region-or-line) :to-be t)))

(describe "crux-with-region-or-sexp-or-line"
  (it "is a macro"
    (expect (macrop 'crux-with-region-or-sexp-or-line) :to-be t)))

(describe "crux-with-region-or-point-to-eol"
  (it "is a macro"
    (expect (macrop 'crux-with-region-or-point-to-eol) :to-be t)))

;;; crux-test.el ends here
