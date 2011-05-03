;; Blackboard Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Blackboard colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-blackboard.el")
;;
;; And then (color-theme-blackboard) to activate it.
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme
;;
;; All patches welcome

(require 'color-theme)

;;;###autoload
(defun color-theme-blackboard ()
  "Color theme by JD Huntington, based off the TextMate Blackboard theme, created 2008-11-27"
  (interactive)
  (color-theme-install
   '(color-theme-blackboard
     ((background-color . "#000000")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (blue ((t (:foreground "blue"))))
     (bold-italic ((t (:bold t))))
     (bold ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#000000" :foreground "#F8F8F8"))))
     (default ((t (:background "#000000" :foreground "#F8F8F8"))))
     (diff-added ((t (:foreground "green4"))))
     (diff-removed ((t (:foreground "red3"))))
     (font-lock-builtin-face ((t (:foreground "#94bff3"))))
     (font-lock-comment-face ((t (:italic t :foreground "#AEAEAE"))))
     (font-lock-constant-face ((t (:foreground "#D8FA3C"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#FF6400"))))
     (font-lock-keyword-face ((t (:foreground "#FBDE2D"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#61CE3C"))))
     (font-lock-type-face ((t (:foreground "#8DA6CE"))))
     (font-lock-variable-name-face ((t (:foreground "#FF6400"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (highlight ((t (:background "#0b163b"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (magit-diff-add ((t (:foreground "green3"))))
     (magit-diff-del ((t (:foreground "red3"))))
     (magit-item-highlight ((t (:background "#0b163b"))))
     (mode-line ((t (:background "grey75" :foreground "black"))))
     (quack-pltish-defn-face ((t (:foreground "#FF6400"))))
     (quack-pltish-keyword-face ((t (:foreground "#FBDE2D"))))
     (quack-pltish-paren-face ((((class color) (background dark)) nil)))
     (rcirc-keyword ((t (:foreground "cyan"))))
     (rcirc-nick-in-message ((t (:foreground "cyan"))))
     (region ((t (:background "#162248"))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "ble")))))))
