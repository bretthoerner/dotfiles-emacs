(set-language-environment "utf-8")

;; -------------
;; debug config
;; -------------

(setq debug-on-error t)


;; -------------
;; color config
;; -------------

(defvar bjh-color 'dark) ;; light or dark?

;; -----------------
;; package.el config
;; -----------------

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

(defvar bjh-packages
  '(ac-cider
    ag
    auto-complete
    autopair
    browse-kill-ring
    buffer-move
    c-eldoc
    cider
    clojure-mode
    color-theme
    dash
    deft
    diminish
    dired-single
    enh-ruby-mode
    evil
    expand-region
    find-file-in-project
    flycheck
    flymake
    flymake-cursor
    flymake-rust
    full-ack
    gh
    ghc
    gist
    git-commit-mode
    gitignore-mode
    git-rebase-mode
    gitignore-mode
    gnuplot-mode
    go-autocomplete
    go-eldoc
    go-errcheck
    go-mode
    goto-last-change
    haskell-mode
    hi2
    highlight-parentheses
    gitconfig-mode
    htmlize
    idle-highlight-mode
    ido-ubiquitous
    js-comint
    js2-mode
    magit
    markdown-mode
    multi-term
    paredit
    php-mode
    protobuf-mode
    puppet-mode
    rainbow-delimiters
    rust-mode
    sbt-mode
    scala-mode2
    scpaste
    scratch
    smex
    smooth-scrolling
    toml-mode
    undo-tree
    vcl-mode
    xcscope
    yaml-mode
    yasnippet))

(defun bjh-install-packages ()
  (interactive)
  (package-refresh-contents)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
		(package-install package)))
        bjh-packages))

;(bjh-install-packages)

;; removed from Emacs but still used in some libraries http://repo.or.cz/w/emacs.git/patch/3ffbb1932753854b275cd2be6b8c0009cf3380a5
;(defun ad-advised-definition-p (definition)
;  "Return non-nil if DEFINITION was generated from advice information."
;  (if (or (ad-lambda-p definition)
;    (macrop definition)
;    (ad-compiled-p definition))
;      (let ((docstring (ad-docstring definition)))
;	(and (stringp docstring)
;       (get-text-property 0 'dynamic-docstring-function docstring)))))

;; -------------
;; plugin config
;; -------------


;; auto-complete
(require 'auto-complete)

;; flycheck
; (global-flycheck-mode)

;; flymake
;(setq-default flymake-gui-warnings-enabled nil)
;(require 'flymake)
;(load-library "flymake-cursor")

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; bnf-mode
(define-generic-mode 'bnf-mode
  () ;; comment char: inapplicable because # must be at start of line
  nil ;; keywords
  '(
    ("^#.*" . 'font-lock-comment-face) ;; comments at start of line
    ("^<[^ \t\n]*?>" . 'font-lock-function-name-face) ;; LHS nonterminals
    ("<[^ \T\n]*?>" . 'font-lock-builtin-face) ;; other nonterminals
    ("::=" . 'font-lock-const-face) ;; "goes-to" symbol
    ("\|" . 'font-lock-warning-face) ;; "OR" symbol
    )
  '("\\.bnf\\'") ;; filename suffixes
  nil ;; extra function hooks
  "Major mode for BNF highlighting.")

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; browse-url
(setq browse-url-browser-function 'browse-url-chromium
      browse-url-chromium-program "google-chrome-stable")

;; c-mode
;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
(setq-default c-eldoc-includes "-I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/irony-mode/elisp/"))
;(require 'irony)

(defun my-c++-hooks ()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  ;; be cautious, if yas is not enabled before (auto-complete-mode 1), overlays
  ;; *may* persist after an expansion.
  (yas/minor-mode-on)
  (auto-complete-mode 1)

  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-known-modes)
    (irony-mode 1)))

(add-hook 'c++-mode-hook 'my-c++-hooks)
(add-hook 'c-mode-hook 'my-c++-hooks)

;; cider
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-repl-history-file (expand-file-name "~/.emacs.d/cider-history"))
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(setq cider-stacktrace-frames-background-color "#2D2828")

;; clojure-mode
(when (require 'clojure-mode nil t)
 (add-hook 'clojure-mode-hook 'pretty-fns))

;; cscope
(require 'xcscope)
(cscope-setup)

;; cua-mode
(setq cua-rectangle-mark-key (kbd "<C-S-M-return>"))
(cua-mode t)
;; only use cua-mode for rectangle-edit
(setq cua-enable-cua-keys nil)
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; deft
(require 'deft)
(setq deft-text-mode 'org-mode
      deft-auto-save-interval 0
      deft-extension "org")

;; dired
(require 'dired)
(require 'dired-single)
(require 'dired-x)
(setq dired-omit-files-p t)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer ".."))))
  (define-key dired-mode-map "-"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; elisp
(require 'eldoc)
(setq eldoc-minor-mode-string nil)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;; evil
;; (require 'evil)
;; (setq evil-emacs-state-cursor 'bar)
;; (evil-mode 1)

;; find-file
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)

;; ffap
(when (fboundp 'find-file-at-point)
  (global-set-key (kbd "C-c F") 'find-file-at-point))

;; ffip
(require 'find-file-in-project)
(setq ffip-limit 2048
      ffip-patterns
      '("*.clj" "*.css" "*.el" "*.html" "*.js" "*.py" "*.rb"
        "*.txt" "*.dtjs" "*.dtpl" "*.erb" "*.yml" "*.conf"))

;; flyspell-mode
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra")
      flyspell-mode-line-string nil)

;; gist
(require 'gist)

;; go-mode
(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

(require 'go-autocomplete)
(require 'auto-complete-config)
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; goto-last-change
(require 'goto-last-change)

;; gnuplot
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; haskell-mode
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(require 'ghc)
(setq ghc-display-error 'minibuffer)
(require 'hi2)
(setq hi2-show-indentations nil)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; highlight-parentheses
(require 'highlight-parentheses)
(setq hl-paren-minor-mode-string nil
     hl-paren-colors
     '("orange1" "yellow1" "greenyellow" "green1"
       "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
(defun enable-highlight-parentheses-mode ()
 (highlight-parentheses-mode t))

;; ibuffer
(require 'ibuffer)

;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; ido
(setq ido-auto-merge-work-directories-length -1
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-save-directory-list-file nil)

(require 'ido)
; (require 'ido-ubiquitous)
(ido-mode t)
(ido-everywhere t)

;; imenu
(require 'imenu)

;; js-mode
(setq js-indent-level 2)

;; js2-mode
;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-highlight-level 3
;;       js2-bounce-indent-p t
;;       js2-consistent-level-indent-inner-bracket-p t
;;       js2-pretty-multiline-decl-indentation-p t
;;       js2-basic-offset 2)

;; js-comint
(require 'js-comint)
(setq inferior-js-program-command "/usr/bin/rhino")
;(setq inferior-js-program-command "/usr/local/bin/node-repl")
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; man
(setq Man-notify-method 'aggressive)

;; multi-term
; To get special control codes use: cat, xev, od -a
; Use `list-colors-display' to view all available colors
(require 'multi-term)
(define-key term-raw-map (kbd "<C-right>") 'term-send-forward-word)
(define-key term-raw-map (kbd "<C-left>") 'term-send-backward-word)
(define-key term-raw-map (kbd "<M-backspace>") 'term-send-backward-kill-word)
(define-key term-raw-map (kbd "<C-prior>") 'multi-term-prev)
(define-key term-raw-map (kbd "<C-next>") 'multi-term-next)
(global-set-key (kbd "C-S-t") 'multi-term)

(defface bjh-term-color-black
  '((t :foreground "black" :background "black"))
  "Face used to render black color code."
  :group 'term)

(defface bjh-term-color-red
  '((t :foreground "red2" :background "red2"))
  "Face used to render red2 color code."
  :group 'term)

(defface bjh-term-color-green
  '((t :foreground "green2" :background "green2"))
  "Face used to render green2 color code."
  :group 'term)

(defface bjh-term-color-yellow
  '((t :foreground "yellow2" :background "yellow2"))
  "Face used to render yellow2 color code."
  :group 'term)

(defface bjh-term-color-blue
  '((t :foreground "DodgerBlue2" :background "DodgerBlue2"))
  "Face used to render DodgerBlue2 color code."
  :group 'term)

(defface bjh-term-color-magenta
  '((t :foreground "magenta2" :background "magenta2"))
  "Face used to render magenta2 color code."
  :group 'term)

(defface bjh-term-color-cyan
  '((t :foreground "cyan2" :background "cyan2"))
  "Face used to render cyan2 color code."
  :group 'term)

(defface bjh-term-color-white
  '((t :foreground "white" :background "white"))
  "Face used to render white color code."
  :group 'term)

(if (eq bjh-color 'dark)
    (setq term-default-bg-color "#000000"
          term-default-fg-color "white"
          ansi-term-color-vector [term
                                  bjh-term-color-black
                                  bjh-term-color-red
                                  bjh-term-color-green
                                  bjh-term-color-yellow
                                  bjh-term-color-blue
                                  bjh-term-color-magenta
                                  bjh-term-color-cyan
                                  bjh-term-color-white]))

(defun bjh-term-line-mode ()
  (interactive)
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'transient-mark-mode) t)
  (term-line-mode))

(defun bjh-term-char-mode ()
  (interactive)
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (term-char-mode)
  (comint-goto-process-mark))

(add-hook 'term-mode-hook
          '(lambda ()
             (autopair-mode -1)
             (yas-minor-mode -1)
             (define-key term-raw-map (kbd "C-c C-j") 'bjh-term-line-mode)
             (define-key term-mode-map (kbd "C-c C-k") 'bjh-term-char-mode)
             (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; nxml-mode
(add-to-list 'auto-mode-alist '("\\.\\(html\\|xml\\|xsl\\|xhtml\\)\\'" . nxml-mode))

; set indent to 4
(setq nxml-child-indent 4)

; remap finish element
(global-set-key [(control ?.)] 'nxml-finish-element)

;; occur
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; org-mode
(setq org-replace-disputed-keys t
      org-startup-folded nil
      org-log-done 'time
      org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "FEATURE(f)" "|" "COMPLETED(c)")
        (sequence "BUG(b)" "|" "FIXED(x)")
        (sequence "|" "CANCELED(a)")))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org)

(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key [(control tab)])
            (flyspell-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; paredit
(require 'paredit)

;; paren-mode
(require 'paren)
(set-face-attribute 'show-paren-match nil :background "gray20" :foreground "white")

;; php-mode
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; puppet-mode
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(setq puppet-indent-level 4)

;; python
(require 'python)
(setq python-indent-offset 2)

;; rainbow-delimiters
;; (require 'rainbow-delimiters)

;; recentf
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; rcirc
(let ((irc-password-file (expand-file-name "~/.ircpass.el")))
 (when (file-regular-p irc-password-file)
   (load irc-password-file)

   ;; random bitlbee notes
   ; account add oscar <user> <pass> login.oscar.aol.com
   ; account add jabber <email> <pass> talk.google.com:5223:ssl
   ; chat add 0 <aim_channel>

   (require 'rcirc)
   ;; (require 'rcirc-notify)
   ;; (setq my-rcirc-notify-message "%s has mentioned you."
   ;;       my-rcirc-notify-keyword "%s has mentioned you."
   ;;       my-rcirc-notify-message-private "%s sent you a private message.")
   ;; (require 'rcirc-notify-mode)

   (add-hook 'rcirc-mode-hook
             '(lambda ()
                (set (make-local-variable 'blink-matching-paren) nil)
                (set (make-local-variable 'scroll-conservatively) 8192)
                (rcirc-track-minor-mode 1)
                (flyspell-mode 1)
                (autopair-mode -1)))

   (setq rcirc-fill-column 80
         rcirc-default-nick "brett_h"
         rcirc-default-user-name "brett"
         rcirc-default-full-name "Brett"
         rcirc-keywords '("brett" "bretthoerner" "hoerner")
         ;rcirc-authinfo `(;("freenode" nickserv "brett_h" ,bjh-freenode-password)
         ;                 ;("localhost" bitlbee "brett_h" ,bjh-bitlbee-password)
         ;                 ;("bretthoerner.com" nickserv "brett_h" ,bjh-freenode-password))
         rcirc-server-alist `(;("irc.freenode.net" :channels ("#emacs"))
                              ;("localhost")
                              ("localhost" :port 6668 :nick "brett_h" :password ,bjh-freenode-password)
                              ;("mozilla.local" :port 6668 :nick "brett_h/mozilla" :password ,bjh-mozilla-password)
                              ;("irc.mozilla.org" :port 6667 :nick "brett_h" :password ,bjh-mozilla-password)
                              ))

   (defun-rcirc-command clear (arg)
     "Clear rcirc buffer."
     (interactive "i")
     (let ((inhibit-read-only t))
       (delete-region
        (point-min) rcirc-prompt-start-marker)))

   (defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            channels query-buffers)
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-full-name
                      channels)))))

;; rust
(require 'rust-mode)
(setq rust-indent-offset 2)

;; savehist
(savehist-mode 1)

;; saveplace
(setq-default save-place t)
(require 'saveplace)

;; scala
(require 'sbt-mode)

;; scpaste
(require 'scpaste)
(setq scpaste-http-destination "http://b.hoerner.us"
      scpaste-scp-destination "bretthoerner.com:~/bretthoerner.com/media/paste"
      scpaste-scp-port "2222")

;; scratch
(autoload 'scratch "scratch" nil t)

;; slime
; (eval-after-load 'slime
;   '(define-key slime-mode-map (kbd "C-c p")
;      'slime-pprint-eval-last-expression))
;
; (eval-after-load 'slime-repl
;   '(define-key slime-repl-mode-map (kbd "C-c p")
;      'slime-pprint-eval-last-expression))
;
; (require 'slime nil t)
; (if (fboundp 'slime)
;   (progn
;     (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl")))
;           slime-net-coding-system 'utf-8-unix
;           slime-protocol-version 'ignore)
;     (global-set-key "\C-cs" 'slime-selector)
;     (slime-setup '(slime-repl))
;     (slime-require :swank-listener-hooks)))

;; smerge
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; smooth-scrolling for keyboard
(require 'smooth-scrolling)

;; mouse-wheel scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; but accelerate
(setq mouse-wheel-progressive-speed t)

;; sql
(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
  (concat "\n" output))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

;; tramp
(setq tramp-default-method "ssh"
      tramp-persistency-file-name nil)
(require 'tramp)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; uniquify
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator ": ")
(require 'uniquify)

;; varnish
(require 'vcl-mode)
(setq vcl-indent-level 2)

;; which-func-mode
;; (which-func-mode 1)

;; yasnippet
;(require 'yasnippet)
;(yas-global-mode 1)

;; diminish (needs to be run after other modes are loaded)
(when (require 'diminish nil 'noerror)
  (diminish 'eldoc-mode "")
  (diminish 'paredit-mode "Par")
  ;; (diminish 'auto-complete-mode "")
  (diminish 'auto-fill-function "")
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq mode-name "Elisp"))))


;; --------------
;; general config
;; --------------


;; can't seem to get this to work
;; (add-to-list 'minor-mode-alist '(auto-fill-function ""))

;; cold turkey
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

;; show line numbers
;; (require 'linum)
;; (global-linum-mode 1)

;; example of .dir-locals.el
;; ((js2-mode . ((indent-tabs-mode . t))))

;; fix for S-up being interpreted as <select>
;; and other keys like S-M-arrow
;; https://groups.google.com/d/topic/gnu.emacs.help/rR478H4BDU8/discussion
(when (and (tty-type) (string-match "xterm" (tty-type)))
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e\e[1;2C" [S-M-right])
  (define-key input-decode-map "\e\e[1;2D" [S-M-left]))

;; wrap at edge of window
(setq truncate-lines nil
      truncate-partial-width-windows nil)

;; final newlines are good
(setq require-final-newline t)

;; revert changed files automatically
(global-auto-revert-mode t)

;; kill out to clipboard
(setq x-select-enable-clipboard t)

;; useful for running commands that then output to the minibuffer itself
(setq enable-recursive-minibuffers t)

;; interact with system clipboard when using kill/yank on OS X
(when (eq system-type 'darwin)
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let* ((process-connection-type nil)
                 (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
            (process-send-string pbproxy text)
            (process-send-eof pbproxy))))

  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "pbpaste"))))

;; tell apropos to do more
(setq apropos-do-all t)

;; case-insensitive completion
(setq completion-ignore-case1 t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; use text-mode not fundamental-mode
(setq default-major-mode 'text-mode)

;; http://article.gmane.org/gmane.emacs.devel/64807
(setq parse-sexp-ignore-comments t)

;; completion in M-:
(when (keymapp read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

;; always try to indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; rename buffer on F2
(define-key global-map (kbd "<f2>") 'rename-buffer)

;; show full file path on F6
(define-key global-map (kbd "<f6>") (lambda ()
                                      (interactive "*")
                                      (message buffer-file-truename)))

;; revert buffer on F5
(define-key global-map (kbd "<f5>") 'revert-buffer)

;; no startup message or splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; don't insert instructions into the *scratch* buffer
(setq initial-scratch-message nil)

;; don't copy selected text to kill-ring automatically
(setq mouse-drag-copy-region nil)

;; mouse usage in terminal
(add-hook 'after-init-hook (lambda ()
                             (xterm-mouse-mode 1)))

;; automatically make files with #! executable on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; don't split without me asking
(setq-default split-width-threshold nil)
(setq-default split-height-threshold nil)

;; don't show vc info in modeline
(setq vc-display-status nil)

;; indent via spaces not tabs
(setq-default indent-tabs-mode nil)

;; tab = 4 spaces
(setq-default default-tab-width 2)
(setq-default tab-width 2)

;; set C indent to 2 spaces
(setq-default c-basic-offset 2
              c-default-style "linux")

;; set tab stops based on default-tab-width
(setq-default tab-stop-list (loop for i
                                  from default-tab-width to 120
                                  by default-tab-width
                                  collect i))

;; C-k deletes the whole line
(setq kill-whole-line t)

;; backspace on whitespace turns to spaces and removes one
(setq backward-delete-char-untabify-method 'untabify)

;; completion in mini-buffer
(icomplete-mode t)

;; show line numbers
(line-number-mode t)

;; matching pairs
;; (electric-pair-mode t)

;; show column number in status bar
(column-number-mode 1)

;; highlight characters after 80 columns
(setq whitespace-style '(face trailing lines-tail space-before-tab
                              space-after-tab))

;; use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
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

; don't make me double escape \
(require 're-builder)
(setq reb-re-syntax 'string)

;; allows "y" instead of "yes" on exit
(fset 'yes-or-no-p 'y-or-n-p)

;; always ask if you really want to exit
; (setq confirm-kill-emacs 'yes-or-no-p)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; default to unified diffs
(setq diff-switches "-u -w")

;; bind unbound join-line
(global-set-key (kbd "C-c q") 'join-line)

;; C-x C-m as replacement for M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; C-x C-u for undo (a common typo of mine)
(global-set-key [(control ?x) (control ?u)] 'undo)

;; keep things in the same window
(setq pop-up-windows nil)
(add-to-list 'same-window-buffer-names "*Help*")
(add-to-list 'same-window-buffer-names "*Apropos*")
(add-to-list 'same-window-buffer-names "*Summary*")

;; better C-a
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key "\C-a" 'smart-beginning-of-line)

;; lolworthy function from esk
(defun bjh-disapproval ()
  (interactive)
  (insert "ಠ_ಠ"))

;; hippie-expand
(require 'hippie-exp)
(global-set-key (kbd "M-/") 'hippie-expand)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(delete 'try-complete-file-name-partially hippie-expand-try-functions-list)
(delete 'try-complete-file-name hippie-expand-try-functions-list)

;; mark dedicated window
(defun toggle-sticky-buffer-window ()
  "Toggle whether this window is dedicated to this buffer."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window))))
  (if (window-dedicated-p (selected-window))
      (message "Window is now dedicated.")
    (message "Window is no longer dedicated.")))

(global-set-key [(super d)] 'toggle-sticky-buffer-window)

;; coding hook
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8) (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun pretty-fns ()
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           "\u0192") nil))))))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun show-parens ()
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1))

(require 'idle-highlight-mode)

(defun turn-on-idle-highlight ()
  (idle-highlight-mode t))

(defun adaptive-indent (beg end)
  "Indent the region between BEG and END with adaptive filling."
  (goto-char beg)
  (while
      (let ((lbp (line-beginning-position))
            (lep (line-end-position)))
        (put-text-property lbp lep 'wrap-prefix (fill-context-prefix lbp lep))
        (search-forward "\n" end t))))

(define-minor-mode adaptive-wrap-mode
  "Wrap the buffer text with adaptive filling."
  :lighter ""
  (save-excursion
    (save-restriction
      (widen)
      (let ((buffer-undo-list t)
            (inhibit-read-only t)
            (mod (buffer-modified-p)))
        (if adaptive-wrap-mode
            (progn
              (setq word-wrap t)
              (unless (member '(continuation) fringe-indicator-alist)
                (push '(continuation) fringe-indicator-alist))
              (jit-lock-register 'adaptive-indent))
          (jit-lock-unregister 'adaptive-indent)
          (remove-text-properties (point-min) (point-max) '(wrap-prefix pref))
          (setq fringe-indicator-alist
                (delete '(continuation) fringe-indicator-alist))
          (setq word-wrap nil))
        (restore-buffer-modified-p mod)))))

;; handy coding-hook to reuse
(add-hook 'coding-hook 'local-comment-auto-fill)
;(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
;(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'show-parens)
;(add-hook 'coding-hook 'turn-on-idle-highlight)
(add-hook 'coding-hook 'adaptive-wrap-mode)

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(add-hook 'puppet-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (turn-on-whitespace)
              (set (make-local-variable 'tab-width) 4))))

;; enable modes for lisp files
(mapc (lambda (mode-hook)
        (add-hook mode-hook 'enable-paredit-mode)
        (add-hook mode-hook 'enable-highlight-parentheses-mode)
        (add-hook mode-hook 'run-coding-hook))
      '(emacs-lisp-mode-hook
        clojure-mode-hook
        ielm-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        slime-repl-mode-hook))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (whitespace-cleanup))

;; edit as root
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; file finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x t") 'find-file-in-project)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(global-set-key [(control ?x) (tab)] 'ido-imenu)

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

(global-set-key [(control ?c) (meta \[)] 'textmate-shift-left)
(global-set-key [(control ?c) (meta \])] 'textmate-shift-right)

;; define and bind textmate-like next-line
(defun textmate-next-line ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key [(meta return)] 'textmate-next-line)

;; M-l for goto-line
;(global-set-key [(meta ?l)] 'goto-line)

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

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-S-s") 'isearch-forward)
(global-set-key (kbd "C-S-r") 'isearch-backward)

;; M-s regex search forward
;(global-set-key [(meta ?s)] 'isearch-forward-regexp)
;(define-key isearch-mode-map [(meta ?s)] 'isearch-repeat-forward)

;; M-r regex search backward
;(global-set-key [(meta ?r)] 'isearch-backward-regexp)
;(define-key isearch-mode-map [(meta ?r)] 'isearch-repeat-backward)

;; C-M-h to kill word like in readline
(global-set-key [(control meta ?h)] 'backward-kill-word)

;; switch buffers by scrolling on modeline
(global-set-key (kbd "<mode-line> <wheel-up>") 'next-buffer)
(global-set-key (kbd "<mode-line> <wheel-down>") 'previous-buffer)


;; ----------
;; GUI config
;; ----------


(if window-system
  (progn
    ;; color-theme
    ; C-u C-x = to get font info at point
    (require 'color-theme)
    (setq color-theme-is-global t
          frame-background-mode bjh-color)
    (color-theme-initialize)
    (if (eq bjh-color 'dark)
        (progn
          (load-file (expand-file-name "~/.emacs.d/themes/blackboard.el"))
          (color-theme-blackboard))
      (color-theme-gtk-ide)) ; else light

    ;; emacs 24+ themes
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

    ;; start emacs server
    ;; (server-start)

    ;; bar cursor
    (setq-default cursor-type 'bar)

    ;; disable tool-bar
    (tool-bar-mode -1)

    ;; disable menu bar
    (menu-bar-mode -1)

    ;; disable scroll-bar
    (scroll-bar-mode -1)

    ;; don't blink cursor
    (blink-cursor-mode -1)

    ;; restore old `exchange-point-and-mark' (which pc-selection-mode overrides)
    (global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)

    ;; don't support suspend in GUI mode
    (global-unset-key (kbd "C-z"))

    ;; fix path
    (mapc (lambda (bin-path)
            (let ((expanded-bin-path (expand-file-name bin-path)))
              (add-to-list 'exec-path expanded-bin-path)
              (setenv "PATH" (concat expanded-bin-path ":" (getenv "PATH")))))
          '("~/bin"
            "/usr/local/bin"
            "~/.go/bin"))

    ;; use thinkpad arrows to manipulate windows and buffers
    (defun other-other-window ()
      (interactive)
      (other-window -1))
    (global-set-key [(XF86Forward)] 'other-window)
    (global-set-key [(XF86Back)] 'other-other-window)
    (global-set-key [(control tab)] 'other-window)
    (global-set-key [(control shift iso-lefttab)] 'other-other-window)

    (global-set-key [(control XF86Forward)] 'next-buffer)
    (global-set-key [(control XF86Back)] 'previous-buffer)

    (global-set-key [(super z)] 'undo-tree-undo)
    (global-set-key [(shift super z)] 'undo-tree-redo)

    (require 'buffer-move)
    (global-set-key [(meta super right)] 'buf-move-right)
    (global-set-key [(meta super left)] 'buf-move-left)
    (global-set-key [(meta super up)] 'buf-move-up)
    (global-set-key [(meta super down)] 'buf-move-down)

    (global-set-key [(super right)] 'end-of-line)
    (global-set-key [(super left)] 'beginning-of-line)
    (global-set-key [(super up)] 'beginning-of-buffer)
    (global-set-key [(super down)] 'end-of-buffer)

    (defun bjh-set-frame-font-size (size)
      (set-frame-font (concat bjh-font "-" (number-to-string size)))
      (setq bjh-current-font-size size))
    (defun bjh-text-scale-increase (inc)
      (interactive "p")
      (bjh-set-frame-font-size (+ bjh-current-font-size inc)))
    (defun bjh-text-scale-decrease (dec)
      (interactive "p")
      (bjh-set-frame-font-size (- bjh-current-font-size dec)))

    ;; dim parens in lisp-modes
    (defface bjh-paren-face
      '((((class color) (background dark))
         (:foreground "grey50"))
        (((class color) (background light))
         (:foreground "grey55")))
      "Face used to dim parentheses."
      :group 'starter-kit-faces)

    (dolist (mode '(scheme emacs-lisp lisp clojure))
      (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                              '(("(\\|)" . 'bjh-paren-face))))

    ;; font
    (add-to-list 'default-frame-alist '(font . "Ubuntu Mono derivative Powerline-14"))
    (setq bjh-font "Ubuntu Mono derivative Powerline")
    (bjh-set-frame-font-size 14)

    ;; bind 'o' to run 'open' command on selected file in dired mode
    (define-key dired-mode-map "o" 'dired-open-gnome)
    (defun dired-open-gnome ()
      (interactive)
      (let ((file-name (dired-get-file-for-visit)))
        (if (file-exists-p file-name)
            (call-process "/usr/bin/gnome-open" nil 0 nil file-name))))

    (defun switch-full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
    (global-set-key [f11] 'switch-full-screen)

    ;; allow command-v to paste in search
    (define-key isearch-mode-map [(control ?y)] 'isearch-yank-kill))

  ;; else (not in a window system)
)
