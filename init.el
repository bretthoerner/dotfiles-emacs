;; -------------
;; debug config
;; -------------

(setq debug-on-error nil)


;; -------------
;; custom files
;; -------------


(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))

;; work around a bug on OS X where system-name is FQDN
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

;; keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))


;; -------------
;; plugin config
;; -------------


;; add dotfiles/misc to path
(add-to-list 'load-path (concat dotfiles-dir "misc"))

;; ack
(require 'ack)

;; ansi-color
(require 'ansi-color)

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; clojure
(add-to-list 'load-path (concat dotfiles-dir "clojure-mode"))
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'idle-highlight)
(font-lock-add-keywords 'clojure-mode
                        '(("(\\|)" . 'esk-paren-face)))
(add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; dired
(require 'dired)
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
        (function
         (lambda nil (interactive) (joc-dired-single-buffer "..")))))
;; if dired is already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
        ;; we're good to go; just add our bindings
        (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; django
(require 'django-html-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))

;; erc
(let ((erc-config-file (expand-file-name "~/.erc.el")))
  (when (file-regular-p erc-config-file)
    (load erc-config-file)))

;; erlang
(let ((erlang-path "/opt/local/lib/erlang/"))
  (if (file-directory-p erlang-path)
    (progn
      (setq load-path (cons (concat erlang-path "lib/tools-2.6.2/") load-path))
      (setq erlang-root-dir erlang-path)
      (setq exec-path (cons (concat erlang-path "bin") exec-path))
      (require 'erlang-start))))

;; ffip
(require 'project)
(require 'find-file-in-project)
(setq ffip-patterns
  '("*.rb" "*.html" "*.el" "*.js" "*.py" "*.css"))

;; flymake
(setq-default flymake-gui-warnings-enabled nil)
(require 'flymake)
(load-library "flymake-cursor")
(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(setq flymake-allowed-file-name-masks
  (cons '("\\.py\\'" flymake-pyflakes-init)
    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
  (cons '("could not compile '\\([^']+\\)':\\([0-9]+\\):\\(\n.*\\)" 1 2 nil nil)
    flymake-err-line-patterns))

;; idle-highlight
(require 'idle-highlight)

;; ido
(setq ido-auto-merge-work-directories-length -1)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-save-directory-list-file nil)
(setq ido-use-filename-at-point t)
(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; imenu
(require 'imenu)

;; iswitchb
(iswitchb-mode 1)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(setq js2-highlight-level 3)

;; magit
(add-to-list 'load-path (concat dotfiles-dir "magit"))
(require 'magit)
(global-set-key [(control ?x) (?g)] 'magit-status)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))
(add-hook 'markdown-mode-hook '(lambda() ;; rebind yas over markdown-visibility-cycle
                                 (define-key markdown-mode-map [(tab)] 'yas/expand)))

;; nxml-mode
(when (> emacs-major-version 22)
  (setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|xhtml\\)\\'" . nxml-mode)
          auto-mode-alist)))

;; org-mode
(add-to-list 'load-path (concat dotfiles-dir "org-mode/lisp"))

;; paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(defun lisp-enable-paredit-hook () (paredit-mode 1))

;; php
(require 'php-mode)
(add-to-list 'auto-mode-alist
             '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

;; python-mode
(require 'python-mode)
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

;; redo
(require 'redo)

;; revbufs
(require 'revbufs)

;; sass
(when (> emacs-major-version 22)
  (require 'sass-mode))

;; saveplace
(setq-default save-place t)
(require 'saveplace)

;; slime and swank
(add-to-list 'load-path (concat dotfiles-dir "slime"))
(add-to-list 'load-path (concat dotfiles-dir "swank-clojure"))
(require 'swank-clojure-autoload)
(setq swank-clojure-binary (expand-file-name "~/bin/clojure"))
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))
(require 'slime)
(slime-setup)

;; tramp
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name nil)
(require 'tramp)

;; uniquify
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-separator ": ")
(require 'uniquify)

;; yasnippet
(add-to-list 'load-path (concat dotfiles-dir "yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "yasnippet/snippets"))


;; --------------
;; general config
;; --------------


;; add some directories to PATH if they exist and haven't been added
(defun add-to-path (path)
  "Add to path"
  (let ((path-to-add path)
        (current-path (getenv "PATH")))
    (if (file-directory-p path-to-add)
      (if (not (string-match path-to-add current-path))
        (progn
          (push path-to-add exec-path)
          (setenv "PATH" (concat path-to-add ":" current-path))))))
)

(add-to-path "/opt/local/bin")
(add-to-path "/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin")
(add-to-path (expand-file-name "~/bin"))

;; show time, just 'cause
(display-time-mode t)

;; use battery mode if we have it
;; (if (fboundp 'display-battery-mode)
;;   (display-battery-mode t))

;; 2 space indent for HTML
(add-hook 'sgml-mode-hook
  (lambda ()
    (setq tab-width 2)))

;; use text-mode not fundamental-mode
(setq default-major-mode 'text-mode)

;; always show trailing whitespace
(setq-default show-trailing-whitespace t)

;; no startup message
(setq inhibit-startup-message t)

;; don't copy selected text to kill-ring automatically
(setq mouse-drag-copy-region nil)

;; scroll one line at a time
;; (setq-default scroll-conservatively 10000)
(setq-default scroll-step 1)

;; don't split horizontally without me asking
(setq-default split-width-threshold nil)

;; indent via spaces not tabs
(setq-default indent-tabs-mode nil)

;; tab = 4 spaces
(setq-default default-tab-width 4)

;; set C indent to 4 spaces
(setq-default c-basic-offset 4)

;; set tab stops based on default-tab-width
(setq-default tab-stop-list (loop for i from default-tab-width to 120 by default-tab-width collect i))

;; C-k deletes the whole line
(setq kill-whole-line t)

;; backspace on whitespace turns to spaces and removes one
(setq backward-delete-char-untabify-method 'untabify)

;; completion in mini-buffer
(icomplete-mode t)

;; show line numbers
(line-number-mode t)

;; show column number in status bar
(column-number-mode 1)

;; use UTF-8
(prefer-coding-system 'utf-8)

;; follow symlinks to version controlled files
(setq vc-follow-symlinks t)

;; transparently open compressed files
(auto-compression-mode t)

;; titlebar = buffer unless filename
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; don't make *~ files
(setq make-backup-files nil)

; don't make ~/.saves-PID-hostname
(setq auto-save-list-file-prefix nil)

; disable auto-saving
(setq auto-save-default nil)

;; allows "y" instead of "yes" on exit
(fset 'yes-or-no-p 'y-or-n-p)

;; C-x C-m as replacement for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; C-x C-u for undo (a common typo of mine)
(global-set-key [(control ?x) (control ?u)] 'undo)

;; ido-recentf
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; file finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x t") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; eshell
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     ;; TODO: for some reason requiring this here breaks it, but
     ;; requiring it after an eshell session is started works fine.
     ;; (require 'eshell-vc)
     (setenv "PAGER" "cat")
     (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
           '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))

;; start eshell or switch to it if it's active
(global-set-key (kbd "C-x m") 'eshell)

;; start a new eshell even if one is active
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; define my own keyboard-escape-quit that doesn't delete other windows
(defun keyboard-escape-quit-no-delete-other-windows ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((region-active-p)
	 (deactivate-mark))
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

(global-set-key (kbd "ESC ESC ESC") 'keyboard-escape-quit-no-delete-other-windows)

;; define and bind ido-imenu (like a goto-symbol)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(global-set-key "\C-x\C-i" 'ido-imenu)

;; define and bind textmate-like shift-right and shift-left
(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.

A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

(global-set-key [(meta \[)] 'textmate-shift-left)
(global-set-key [(meta \])] 'textmate-shift-right)

;; define and bind textmate-like next-line
(defun textmate-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key [(meta return)] 'textmate-next-line)

;; M-l for goto-line
(global-set-key [(meta ?l)] 'goto-line)

;; M-x M-d = dired, a common typo of mine
(global-set-key [(control ?x) (control ?d)] 'ido-dired)

;; bind delete to forward-delete
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; bind backspace to delete whitespace
(global-set-key [?\d] 'backward-delete-char-untabify)

;; comment line(s)
(defun comment-or-uncomment-line (&optional lines)
  "Comment current line. Argument gives the number of lines
forward to comment"
  (interactive "P")
  (comment-or-uncomment-region
   (line-beginning-position)
   (line-end-position lines)))

;; comment region or just this line
(defun comment-or-uncomment-region-or-line (&optional lines)
  "If the line or region is not a comment, comments region
if mark is active, line otherwise. If the line or region
is a comment, uncomment."
  (interactive "P")
  (if mark-active
      (if (< (mark) (point))
          (comment-or-uncomment-region (mark) (point))
    (comment-or-uncomment-region (point) (mark)))
    (comment-or-uncomment-line lines)))

;; bind above to C-#
(global-set-key [(meta ?#)] 'comment-or-uncomment-region-or-line)

;; always replace region with M-|
(global-set-key [(meta ?|)]
                (lambda (beg end out command)
                  (interactive "r\nP\nsCommand: ")
                  (shell-command-on-region beg end command t t)))

;; show paired parenthesis
(show-paren-mode 1)

;; show marks visually
(transient-mark-mode 1)

;; delete regions when you type over them
(delete-selection-mode 1)

;; disable beep
(setq ring-bell-function 'ignore)

;; function for quickly toggling the display of trailing whitespace.
(defun toggle-trailing-whitespace-display ()
  "Toggle the display of trailing whitespace, by changing the
buffer-local variable `show-trailing-whitespace'."
  (interactive)
  (save-excursion
    (setq show-trailing-whitespace
      (not show-trailing-whitespace))
    (force-window-update (current-buffer)))
  (message (concat "Display whitespace: "
                   (if show-trailing-whitespace
                       "enabled" "disabled"))))

;; bind above function to C-c e
(global-set-key [(control ?c) ?e] 'toggle-trailing-whitespace-display)

;; remove trailing whitespace with C-c w
(global-set-key [(control ?c) ?w] 'delete-trailing-whitespace)

;; default is apropos-command which is less useful
(global-set-key [(control ?h) ?a] 'apropos)

;; M-s regex search forward
(global-set-key [(meta ?s)] 'isearch-forward-regexp)
(define-key isearch-mode-map [(meta ?s)] 'isearch-repeat-forward)

;; M-r regex search backward
(global-set-key [(meta ?r)] 'isearch-backward-regexp)
(define-key isearch-mode-map [(meta ?r)] 'isearch-repeat-backward)

;; C-M-h to kill word like in readline
(global-set-key [(control meta ?h)] 'backward-kill-word)


;; ----------
;; GUI config
;; ----------


(if window-system
  (progn
    ;; start emacs server
    (server-start)

    ;; highlight whole expression inside parens
    (setq show-paren-style 'expression)

    ;; darker background color for expression
    (set-face-background 'show-paren-match-face "gray9")

    ;; bar cursor
    (setq-default cursor-type 'bar)

    ;; disable tool-bar
    (tool-bar-mode -1)

    ;; disable scroll-bar
    (scroll-bar-mode -1)

    ;; highlight current line
    (global-hl-line-mode 1)

    ;; don't blink cursor
    (blink-cursor-mode -1)

    ;; shift+cursor to select text
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode 1)

    ;; make frame larger
    (setq initial-frame-alist '((width . 140) (height . 40)))

    ;; theme
    (set-background-color "black")
    (set-foreground-color "white")
    (set-border-color "black")
    (set-cursor-color "yellow")
    (set-mouse-color "white")
    (set-face-background 'hl-line "gray4")
    (set-face-background 'region "DarkSlateGray")
    (set-face-foreground 'font-lock-builtin-face "SteelBlue")
    (set-face-foreground 'font-lock-comment-face "AntiqueWhite3")
    (make-face-italic 'font-lock-comment-face)
    (set-face-foreground 'font-lock-constant-face "Gold")
    (make-face-bold 'font-lock-constant-face)
    (set-face-foreground 'font-lock-doc-face "BurlyWood")
    (make-face-italic 'font-lock-doc-face)
    (set-face-foreground 'font-lock-function-name-face "LightSkyBlue")
    (make-face-bold 'font-lock-function-name-face)
    (set-face-foreground 'font-lock-keyword-face "LightSkyBlue")
    (set-face-foreground 'font-lock-preprocessor-face "Gold")
    (make-face-bold 'font-lock-preprocessor-face)
    (set-face-foreground 'font-lock-string-face "BurlyWood")
    (set-face-foreground 'font-lock-type-face "PaleGreen")
    (make-face-bold 'font-lock-type-face)
    (set-face-foreground 'font-lock-variable-name-face "Aquamarine")
    (set-face-foreground 'font-lock-warning-face "chocolate")
    (make-face-bold 'font-lock-warning-face)

    ;; mac-specific
    (if (or (string= "mac" window-system) (string= "ns" window-system))
     (progn
        ;; font
        (set-face-font 'default "-apple-monaco-medium-r-normal--12-0-72-72-m-0-iso10646-1")

        ;; normal mac command shortcuts
        (require 'mac-key-mode)
        (mac-key-mode 1)

        ;; allow command-v to paste in search
        (define-key isearch-mode-map [(alt ?v)] 'isearch-yank-kill)

        ;; use alt for meta
        (setq mac-option-modifier 'meta))))

  ;; else (not in a window system)

  ;; disable menu bar in terminal
  (menu-bar-mode -1))

;; ----------------
;; custom config
;; ----------------


;; ----------------
;; end
;; ----------------

