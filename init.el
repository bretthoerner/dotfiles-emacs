(set-language-environment "utf-8")

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


;; -----------------
;; package.el config
;; -----------------

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages"))


;; -------------
;; plugin config
;; -------------


;; add dotfiles/misc to path
(add-to-list 'load-path (concat dotfiles-dir "misc"))

;; ack
(require 'full-ack)

;; auto-complete
;(add-to-list 'load-path (concat dotfiles-dir "auto-complete"))
;(setq ac-dictionary-directories (list (concat dotfiles-dir "auto-complete/dict")))
;(require 'auto-complete-config)
;(ac-config-default)

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
(setq browse-url-browser-function 'browse-url-firefox)

;; c-mode
;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
(setq c-eldoc-includes "-I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; clojure-mode
(add-to-list 'load-path (concat dotfiles-dir "clojure-mode"))
(require 'clojure-mode nil t)

;; cua-mode
(setq cua-rectangle-mark-key (kbd "<C-S-M-return>"))
(cua-mode t)
;; only use cua-mode for rectangle-edit
(setq cua-enable-cua-keys nil)
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

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
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; elisp
(require 'eldoc)

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

;; erc
(let ((erc-password-file (expand-file-name "~/.ercpass.el")))
 (when (file-regular-p erc-password-file)
   (load erc-password-file)

   ;; random bitlbee notes
   ; account add oscar <user> <pass> login.oscar.aol.com
   ; account add jabber <email> <pass> talk.google.com:5223:ssl
   ; chat add 0 <aim_channel>

   (require 'erc-match)
   (require 'erc-services)
   (require 'erc-libnotify)

   (erc-services-mode t)
   (add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)
   (add-to-list 'erc-server-alist '("Brett Hoerner's Bitlebee" bitlbee "localhost" 6668))
   (setq erc-prompt-for-nickserv-password nil
         erc-nickserv-passwords `((freenode (("brett_h" . ,bjh-freenode-password))))
         erc-fill-column 100
         erc-autojoin-channels-alist '(("freenode.net" "#disqus"))
         erc-keywords '("\\bbrett\\b" "\\bhoerner\\b"))

   (defun erc-proxy-enable ()
     (interactive)
     (setq socks-override-functions 1)
     (setq socks-noproxy '("localhost"))
     (require 'socks)
     (setq socks-server '("ssh-socks" "localhost" 9999 5))
     (setq erc-server-connect-function 'socks-open-network-stream))
   ;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))

   (defun bitlbee-identify ()
     "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
     (when (and (string= "localhost" erc-session-server)
                (string= "&bitlbee" (buffer-name)))
       (erc-message "PRIVMSG" (format "%s identify %s"
                                      (erc-default-target)
                                      bjh-bitlbee-password))))
   (add-hook 'erc-join-hook 'bitlbee-identify)

   (defun erc-connect-freenode ()
     (interactive)
     (erc :server "irc.freenode.net"
          :port 6667
          :nick "brett_h"))
   (defun erc-connect-bitlbee ()
     (interactive)
     (erc :server "localhost"
          :port 6668
          :nick "brett_h"
          :password bjh-bitlbee-server-password))

   (require 'rcirc)
   (require 'rcirc-notify)

   (rcirc-track-minor-mode 1)

   ;; clear screen
   ;; (let ((inhibit-read-only t))
   ;;   (delete-region
   ;;    (point-min) rcirc-prompt-start-marker))

   (setq rcirc-default-nick "brett_h"
         rcirc-default-user-name "brett"
         rcirc-default-full-name "Brett"
         rcirc-authinfo `(("freenode" nickserv "brett_h" ,bjh-freenode-password)
                          ("localhost" bitlbee "brett_h" ,bjh-bitlbee-password))
         rcirc-server-alist '(("irc.freenode.net" :channels ("#emacs"))))))

;; ffap
(when (fboundp 'find-file-at-point)
  (global-set-key (kbd "C-c F") 'find-file-at-point))

;; ffip
(require 'project)
(require 'find-file-in-project)
(setq ffip-patterns
      '("*.clj" "*.css" "*.el" "*.html" "*.js" "*.py" "*.rb" "*.txt"))

;; flymake
(setq-default flymake-gui-warnings-enabled nil)
(require 'flymake)
(load-library "flymake-cursor")

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

(push '("\\.py\\'" flymake-pyflakes-init)
      flymake-allowed-file-name-masks)
(push '("could not compile '\\([^']+\\)':\\([0-9]+\\):\\(\n.*\\)" 1 2 nil nil)
      flymake-err-line-patterns)

(add-hook 'python-mode-hook
          (lambda ()
            ;; Activate flymake unless buffer is a tmp buffer for the interpreter
            (unless (eq buffer-file-name nil) (flymake-mode t))))

;; flyspell-mode
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; goto-last-change
(require 'goto-last-change)

;; haskell-mode
(load (concat dotfiles-dir "haskell-mode/haskell-site-file"))

;; highlight-parentheses
(require 'highlight-parentheses)
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))
(defun enable-highlight-parentheses-mode ()
  (highlight-parentheses-mode t))

;; ido
(setq ido-auto-merge-work-directories-length -1
      ido-case-fold t
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-save-directory-list-file nil
      ;; ido-decorations (quote ("\n> " ; show choices vertically
      ;;                         ""
      ;;                         "\n "
      ;;                         "\n ..."
      ;;                         "[" "]"
      ;;                         " [No match]"
      ;;                         " [Matched]"
      ;;                         " [Not readable]"
      ;;                         " [Too big]"
      ;;                         " [Confirm]"))
      )

;; truncate long lines in choices
;; (defun my-ido-minibuffer-setup-hook ()
;;   ;; allow line wrapping in the minibuffer
;;   (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'my-ido-minibuffer-setup-hook)

;; ;; add additional keybindings
;; (defun my-ido-keys ()
;;   (define-key ido-completion-map [up] 'ido-prev-match)
;;   (define-key ido-completion-map [down] 'ido-next-match)
;;   (define-key ido-completion-map [(control n)] 'ido-next-match)
;;   (define-key ido-completion-map [(control p)] 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'my-ido-keys)

(require 'ido)
(ido-mode t)
(ido-everywhere t)

;; imenu
(require 'imenu)

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-highlight-level 3
      js2-bounce-indent-p t)

;; ;; Custom indentation function since JS2 indenting is terrible.
;; ;; Uses js-mode's (espresso-mode) indentation semantics.
;; ;;
;; ;; Based on: http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
;; ;; (Thanks!)
;; (defun my-js2-indent-function ()
;;   (interactive)
;;   (save-restriction
;;     (widen)
;;     (let* ((inhibit-point-motion-hooks t)
;;            (parse-status (save-excursion (syntax-ppss (point-at-bol))))
;;            (offset (- (current-column) (current-indentation)))
;;            (indentation (js--proper-indentation parse-status))
;;            node)

;;       (save-excursion

;;         ;; I like to indent case and labels to half of the tab width
;;         (back-to-indentation)
;;         (if (looking-at "case\\s-")
;;             (setq indentation (+ indentation (/ js-indent-level 2))))

;;         ;; consecutive declarations in a var statement are nice if
;;         ;; properly aligned, i.e:
;;         ;;
;;         ;; var foo = "bar",
;;         ;; bar = "foo";
;;         (setq node (js2-node-at-point))
;;         (when (and node
;;                    (= js2-NAME (js2-node-type node))
;;                    (= js2-VAR (js2-node-type (js2-node-parent node))))
;;           (setq indentation (+ 4 indentation))))

;;       (indent-line-to indentation)
;;       (when (> offset 0) (forward-char offset)))))

;; (defun my-js2-mode-hook ()
;;   (if (not (boundp 'js--proper-indentation))
;;       (progn (js-mode)
;;              (remove-hook 'js2-mode-hook 'my-js2-mode-hook)
;;              (js2-mode)
;;              (add-hook 'js2-mode-hook 'my-js2-mode-hook)))
;;   (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
;;   (define-key js2-mode-map [(return)] 'newline-and-indent)
;;   (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
;;   (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
;;   (message "JS2 mode hook ran."))

;; ;; Add the hook so this is all loaded when JS2-mode is loaded
;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)

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
(add-to-list 'load-path (concat dotfiles-dir "magit"))
(require 'magit nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-diff-options "-w")

;; malabar-mode
(let ((malabar-dir (concat dotfiles-dir "malabar-mode/")))
  (when (file-exists-p malabar-dir)
    (add-to-list 'load-path (concat malabar-dir "lisp"))
    (require 'cedet)
    (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                      global-semanticdb-minor-mode
                                      global-semantic-idle-summary-mode
                                      global-semantic-mru-bookmark-mode))
    (require 'malabar-mode)
    (add-hook 'malabar-mode-hook (lambda ()  (semantic-mode 1)))
    (setq malabar-groovy-lib-dir (concat malabar-dir "lib"))
    (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mkd$" . markdown-mode))

;; multi-term
; To get special control codes use: cat, xev, od -a
; Use `list-colors-display' to view all available colors
(setq term-default-bg-color "#0C1021"
      term-default-fg-color "white"
      ansi-term-color-vector [unspecified "black" "red2" "green2" "yellow2"
                                          "DodgerBlue2" "magenta2" "cyan2" "white"])
(require 'multi-term)

;; nxml-mode
(setq auto-mode-alist
      (cons '("\\.\\(html\\|xml\\|xsl\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

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
(setq org-replace-disputed-keys t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(require 'org-install)

(add-hook 'org-mode-hook
          (lambda ()
            ;; flyspell mode to spell check everywhere
            (flyspell-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; paredit
(require 'paredit)

;; puppet-mode
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
(setq puppet-indent-level 4)

;; quack
(require 'quack)
(setq quack-programs '("racket")
      quack-default-program "racket")

;; rainbow-mode
(require 'rainbow-mode)

;; recentf
(require 'recentf)
(recentf-mode 1)

;; redo
(require 'redo)
(global-set-key [(control ??)] 'redo)

;; savehist
(savehist-mode 1)

;; saveplace
(setq-default save-place t)
(require 'saveplace)

;; scpaste
(require 'scpaste)
(setq scpaste-http-destination "http://p.hoerner.us"
      scpaste-scp-destination "bretthoerner.com:~/bretthoerner.com/media/paste")

;; scratch
(autoload 'scratch "scratch" nil t)

;; slime and swank
(require 'ac-slime)
;(add-to-list 'ac-sources 'ac-source-slime-simple)
;(add-hook 'slime-mode-hook 'set-up-slime-ac)
;(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; old, ghetto auto complete
;(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(add-to-list 'load-path (concat dotfiles-dir "slime"))
(add-to-list 'load-path (concat dotfiles-dir "slime/contrib"))

(eval-after-load 'slime
  '(define-key slime-mode-map (kbd "C-c p")
     'slime-pprint-eval-last-expression))

(eval-after-load 'slime-repl
  '(define-key slime-repl-mode-map (kbd "C-c p")
     'slime-pprint-eval-last-expression))

(require 'slime nil t)
(if (fboundp 'slime)
  (progn
    (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))
                                       (clojure ("~/bin/clojure") :init swank-clojure-init))
          slime-net-coding-system 'utf-8-unix
          slime-use-autodoc-mode nil) ; swank-clojure doesn't support this
    (global-set-key "\C-cs" 'slime-selector)
    (slime-setup '(slime-fancy slime-tramp slime-asdf))
    (slime-require :swank-listener-hooks)))

(add-to-list 'load-path (concat dotfiles-dir "swank-clojure"))
(setq swank-clojure-binary "~/bin/clojure")
(require 'swank-clojure nil t)

;; smerge
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

;; smooth-scrolling for keyboard
;; below are previous 'solution' to scroll one line at a time,
;; replaced by smooth-scrolling.el
;(setq-default scroll-conservatively 10000)
;(setq-default scroll-step 1)
(require 'smooth-scrolling)

;; mouse-wheel scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; but accelerate
(setq mouse-wheel-progressive-speed t)

;; tramp
(setq tramp-default-method "ssh"
      tramp-persistency-file-name nil)
(require 'tramp)

;; uniquify
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator ": ")
(require 'uniquify)


;; --------------
;; general config
;; --------------


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

;; final newlines are good
(setq require-final-newline t)

;; revert changed files automatically
(global-auto-revert-mode t)

;; kill out to clipboard
(setq x-select-enable-clipboard t)

;; tell apropos to do more
(setq apropos-do-all t)

;; case-insensitive completion
(setq completion-ignore-case t)

;; use text-mode not fundamental-mode
(setq default-major-mode 'text-mode)

;; http://article.gmane.org/gmane.emacs.devel/64807
(setq parse-sexp-ignore-comments t)

;; completion in M-:
(when (keymapp read-expression-map)
  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol))

;; always try to indent on newline
(define-key global-map (kbd "RET") 'newline-and-indent)

;; show full file path on F2
(define-key global-map (kbd "<f2>") (lambda ()
                                      (interactive "*")
                                      (message buffer-file-truename)))

;; no startup message or splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t)

;; don't insert instructions into the *scratch* buffer
(setq initial-scratch-message nil)

;; don't copy selected text to kill-ring automatically
(setq mouse-drag-copy-region nil)

;; don't split without me asking
(setq-default split-width-threshold nil)
(setq-default split-height-threshold nil)

;; indent via spaces not tabs
(setq-default indent-tabs-mode nil)

;; tab = 4 spaces
(setq-default default-tab-width 4)
(setq-default tab-width 4)

;; set C indent to 4 spaces
(setq-default c-basic-offset 4)

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

;; show column number in status bar
(column-number-mode 1)

;; highlight characters after 80 columns
(setq whitespace-style '(face trailing lines-tail space-before-tab
                              space-after-tab))
(global-whitespace-mode 1)
;; global-whitespace-mode slows thngs down a lot, so disable (some)
;; fontification. Tip due to
;; http://www.emacswiki.org/emacs/WhiteSpace
;(defun whitespace-post-command-hook () nil)

;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
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

;; disable menu bar in terminal
(menu-bar-mode -1)

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

;; lolworthy function from esk
(defun bjh-disapproval ()
  (interactive)
  (insert "ಠ_ಠ"))

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(delete 'try-complete-file-name-partially hippie-expand-try-functions-list)
(delete 'try-complete-file-name hippie-expand-try-functions-list)

;; coding hook
(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8) (hl-line-mode t)))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-electric-pair-mode ()
  (electric-pair-mode))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun show-parens ()
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1))

;(require 'idle-highlight)

;; handy coding-hook to reuse
(add-hook 'coding-hook 'local-comment-auto-fill)
;(add-hook 'coding-hook 'turn-on-hl-line-mode)
(add-hook 'coding-hook 'turn-on-save-place-mode)
;(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'pretty-lambdas)
(add-hook 'coding-hook 'add-watchwords)
;(add-hook 'coding-hook 'turn-on-electric-pair-mode)
(add-hook 'coding-hook 'show-parens)
;(add-hook 'coding-hook 'idle-highlight)

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(add-hook 'puppet-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook 'run-coding-hook)
(add-hook 'python-mode-hook
          (lambda ()
            (progn
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
  (delete-trailing-whitespace))

;; edit as root
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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

;; don't deactivate my mark on kill-ring-save!
(defun kill-ring-save-keep-region (beg end)
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion (kill-ring-save beg end))))

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

;; M-s regex search forward
(global-set-key [(meta ?s)] 'isearch-forward-regexp)
(define-key isearch-mode-map [(meta ?s)] 'isearch-repeat-forward)

;; M-r regex search backward
(global-set-key [(meta ?r)] 'isearch-backward-regexp)
(define-key isearch-mode-map [(meta ?r)] 'isearch-repeat-backward)

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
    ;; start emacs server
    ;(server-start)

    ;; bar cursor
    (setq-default cursor-type 'bar)

    ;; disable tool-bar
    (tool-bar-mode -1)

    ;; disable scroll-bar
    (scroll-bar-mode -1)

    ;; don't blink cursor
    (blink-cursor-mode -1)

    ;; shift+cursor to select text
    (setq pc-select-selection-keys-only t)
    (pc-selection-mode 1)

    ;; restore old `exchange-point-and-mark' (which pc-selection-mode overrides)
    (global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)

    ;; don't support suspend in GUI mode
    (global-unset-key (kbd "C-z"))

    ;; use thinkpad arrows to manipulate windows and buffers
    (defun other-other-window ()
      (interactive)
      (other-window -1))
    (global-set-key [(XF86Forward)] 'other-window)
    (global-set-key [(XF86Back)] 'other-other-window)

    (global-set-key [(control XF86Forward)] 'next-buffer)
    (global-set-key [(control XF86Back)] 'previous-buffer)

    (require 'buffer-move)
    (global-set-key [(shift XF86Forward)] 'buf-move-right)
    (global-set-key [(shift XF86Back)] 'buf-move-left)

    ;; make frame larger
    (setq initial-frame-alist '((width . 140) (height . 40)))

    ;; color-theme
    ; C-u C-x = to get font info at point
    (require 'color-theme)
    (setq color-theme-is-global t
          frame-background-mode 'dark)
    (load-file (concat dotfiles-dir "themes/blackboard.el"))
    ;; (load-file (concat dotfiles-dir "themes/irblack.el"))
    (color-theme-blackboard)
    ;; (color-theme-irblack)

    (defun bjh-set-frame-font-size (size)
      (set-frame-font (concat bjh-font "-" (number-to-string size)))
      (setq bjh-current-font-size size))
    (defun bjh-text-scale-increase (inc)
      (interactive "p")
      (bjh-set-frame-font-size (+ bjh-current-font-size inc)))
    (defun bjh-text-scale-decrease (dec)
      (interactive "p")
      (bjh-set-frame-font-size (- bjh-current-font-size dec)))

    ;; shortcuts to resize fonts
    (define-key global-map (kbd "C-+") 'bjh-text-scale-increase)
    (define-key global-map (kbd "C--") 'bjh-text-scale-decrease)

    ;; mac-specific
    (if (or (string= "mac" window-system) (string= "ns" window-system))
     (progn
        ;; font
        (setq bjh-font "Monaco")
        (bjh-set-frame-font-size 10)

        ;; normal mac command shortcuts
        (require 'mac-key-mode)
        (mac-key-mode 1)

        ;; bind 'o' to run 'open' command on selected file in dired mode
        (define-key dired-mode-map "o" 'dired-open-mac)
        (defun dired-open-mac ()
               (interactive)
               (let ((file-name (dired-get-file-for-visit)))
                 (if (file-exists-p file-name)
                     (call-process "/usr/bin/open" nil 0 nil file-name))))

        ;; allow command-v to paste in search
        (define-key isearch-mode-map [(alt ?v)] 'isearch-yank-kill)

        ;; use alt for meta
        (setq mac-option-modifier 'meta))

     ;; else not mac
     (progn
        ;; font
        (setq bjh-font "Inconsolata-dz")
        (bjh-set-frame-font-size 10)

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
        (define-key isearch-mode-map [(control ?y)] 'isearch-yank-kill))))

  ;; else (not in a window system)
)
