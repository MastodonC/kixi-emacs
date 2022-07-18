;;; kixi-emacs.el --- Base config for the Mastodon C emacs environment -*- lexical-binding: t -*-
;;
;; Filename: kixi-emacs.el
;; Package-Requires: ((straight) (magit))
;;
;; heavily inspired by
;; - https://gitlab.com/magus/mes
;; - https://github.com/corgi-emacs/corgi
;; - https://github.com/otfrom/otfrom-org-emacs
;; - https://github.com/mpenet/emax/blob/master/init.el
;; - https://github.com/benjamin-asdf/dotfiles/blob/master/mememacs/.emacs-mememacs.d/init.el

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'use-package)
(require 'straight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-basics)
(setq inhibit-startup-message t)

;; Disable unnecessary stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq visible-bell t)

(column-number-mode)
;; (use-package display-line-numbers
;;   :hook ((prog-mode conf-mode) . display-line-numbers-mode)
;;   :custom (display-line-numbers-width 3))
(require 'display-line-numbers)
(add-hook 'prog-mode #'display-line-numbers-mode)
(setq display-line-numbers-width 3)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default indent-tabs-mode nil)
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'recentf-save-file
                        (expand-file-name ".cache/recentf" user-emacs-directory))

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for an command history
(savehist-mode 1)
(customize-set-variable 'savehist-file
                        (expand-file-name ".cache/history" user-emacs-directory))

;; (provide 'kixi-basics)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(straight-use-package 'magit)
(require 'magit)

(provide 'kixi-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kixi-emacs.el ends here
