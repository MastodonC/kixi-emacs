;;; kixi-emacs.el --- Base config for the Mastodon C emacs environment -*- lexical-binding: t -*-
;;
;; Filename: kixi-emacs.el
;; Package-Requires: ((straight) (magit) (evil) (which-key) (doom-themes) (modus-themes) (doom-modeline) (rainbow-delimiters) (general) (evil-collection) (vertico) (orderless) (marginalia) (embark) (consult) (embark-consult) (corfu) (corfu-doc) (cape) (cider) (lsp-mode) (lsp-ui) (consult-lsp) (evil-cleverparens) (flycheck) (consult-flycheck) (flycheck-clj-kondo) (highlight-indent-guides) (orgit) (evil-org) (deadgrep) (command-log-mode) (csv-mode) (rainbow-mode) (commify))
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(customize-set-variable 'display-line-numbers-width 3)

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
;; evil
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-u-scroll t)
(straight-use-package 'evil)
(require 'evil)


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
(setq which-key-idle-delay 0.7)
(which-key-mode)


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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil General
(straight-use-package 'general)
(require 'general)
(general-evil-setup)

(general-create-definer kixi-leader-def
  :keymaps 'override
  :states '(normal motion visual emacs) 
  :prefix "SPC")

(kixi-leader-def
 "SPC" '(execute-extended-command :wk "M-x")
 "TAB" '(mode-line-other-buffer :wk "last buffer")
 "!" 'shell-command
 "u" 'universal-argument)

(kixi-leader-def
 :infix "b"
 "" '(:ignore t :wk "buffers")
 "b" '(consult-buffer :wk "switch")
 "d" '(kill-this-buffer :wk "kill")
 "e" '(erase-buffer :wk "erase"))

(kixi-leader-def
 :infix "E"
 "" '(:ignore t :wk "Emacs")
 "u" '(package-update-all :wk "update packages"))

(kixi-leader-def
 :infix "f"
 "" '(:ignore t :wk "files")
 "f" 'find-file
 "r" '(recentf-open :wk "recent")
 "s" 'save-buffer
 "S" 'save-some-buffers)

(kixi-leader-def
 :infix "h"
 "" '(:ignore t :wk "help/desc")
 "f" 'describe-function
 "i" 'info-display-manual
 "k" 'describe-key
 "m" 'describe-mode
 "v" 'describe-variable
 "w" 'woman)

(kixi-leader-def
 :infix "q"
 "" '(:ignore t :wk "quit")
 "r" 'restart-emacs
 "q" 'kill-emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yet more evil
(straight-use-package 'evil-collection)
(evil-collection-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(straight-use-package 'magit)
(require 'magit)
(kixi-leader-def
   :infix "g"
   "" '(:ignore t :wk "git")
   "c" 'magit-clone
   "i" 'magit-init
   "s" 'magit-status)

;; vertico
(setq vertico-cycle t)
;; (straight-use-package '( vertico :files (:defaults "extensions/*")
;;                          :includes (vertico-buffer
;;                                     vertico-directory
;;                                     vertico-flat
;;                                     vertico-indexed
;;                                     vertico-mouse
;;                                     vertico-quick
;;                                     vertico-repeat
;;                                     vertico-reverse)))
(require 'vertico)
(vertico-mode)
(with-eval-after-load 'evil
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous))

(setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
(straight-use-package 'orderless)
(require 'orderless)

(straight-use-package 'marginalia)
(require 'marginalia)
(marginalia-mode)

(straight-use-package 'consult)
(require 'consult)

(straight-use-package 'embark)
(require 'embark)

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point)

(straight-use-package 'corfu)
(require 'corfu)
(global-corfu-mode)
(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto t)
(customize-set-variable 'corfu-auto-delay 0.5)
(customize-set-variable 'corfu-auto-prefix 2)
(customize-set-variable 'corfu-echo-documentation 0.75)

(straight-use-package 'corfu-doc)
(require 'corfu-doc)
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(straight-use-package
 '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))
(require 'popon)

(straight-use-package
 '(corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(require 'corfu-terminal)
(unless (display-graphic-p)
    (corfu-terminal-mode +1))

(straight-use-package
 '(corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
(require 'corfu-doc-terminal)
(unless (display-graphic-p)
    (corfu-doc-terminal-mode +1))

(straight-use-package 'cape)
(require 'cape)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;; cider and clojure
(setq nrepl-log-messages t
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
(straight-use-package 'cider)
(require 'cider)
(add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))

(customize-set-variable 'lsp-completion-provider :none)
(setq cljr-add-ns-to-blank-clj-files nil
      lsp-enable-indentation nil
      lsp-headerline-breadcrumb-enable nil
      lsp-signature-auto-activate nil
      lsp-semantic-tokens-enable t
      ;; after last buffer closed, kill workspace
      lsp-keep-workspace-alive nil)

(straight-use-package 'lsp-mode)
(require 'lsp-mode)
(defun mpenet/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(flex)))
(add-hook 'clojure-mode #'lsp)
(add-hook 'lsp-completion-mode #'mpenet/lsp-mode-setup-completion)

(dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))


(setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil)
(straight-use-package 'lsp-ui)
(require 'lsp-ui)

(straight-use-package 'consult-lsp)
(require 'consult-lsp)

;; smartparens
(straight-use-package 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(setq sp-show-pair-from-inside t)

(add-hook 'lisp-mode-hook #'smartparens)

;; evil cleverparens
(straight-use-package 'evil-cleverparens)
(add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)

;; flycheck
(straight-use-package 'flycheck)

(straight-use-package 'consult-flycheck)
(require 'consult-flycheck)

(straight-use-package 'flycheck-clj-kondo)
(require 'flycheck-clj-kondo)
 
(add-hook 'sh-mode-hook #'flycheck-mode)
(add-hook 'clojure-mode-hook #'flycheck-mode)

(straight-use-package 'highlight-indent-guides)
(add-hook 'prog-mode #'highlight-indent-guides-mode)

;; org-mode
(straight-use-package 'orgit)
(require 'orgit)

(straight-use-package 'evil-org)
(add-hook 'org-mode-hook #'evil-org-mode)
(add-hook 'evil-org-mode-hook
          (lambda ()
              (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))

(straight-use-package 'deadgrep)
(require 'deadgrep)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(straight-use-package 'command-log-mode)
(require 'command-log-mode)
(global-command-log-mode 1)

(straight-use-package 'csv-mode)
(require 'csv-mode)

(straight-use-package 'rainbow-mode)
(require 'rainbow-mode)

(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let
      ((filename
        (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))

(straight-use-package 'commify)
(require 'commify)

(provide 'kixi-emacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kixi-emacs.el ends here

