;;; kixi-emacs.el --- Base config for the Mastodon C emacs environment -*- lexical-binding: t -*-
;;
;; Filename: kixi-emacs.el
;; Package-Requires: ((straight) (magit) (evil) (which-key) (doom-themes) (modus-themes) (doom-modeline) (rainbow-delimiters))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
(straight-use-package 'evil)
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-u-scroll t)

(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
(straight-use-package 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme and modeline
;; (straight-use-package 'doom-themes)
;; (load-theme 'doom-nord t)
(straight-use-package 'modus-themes)
(setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))
(modus-themes-load-themes)
(modus-themes-load-vivendi)

(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)
(customize-set-variable 'doom-modeline-height 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rainbow-delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode #'rainbow-delimiters-mode)

(provide 'kixi-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kixi-emacs.el ends here
