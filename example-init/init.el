;; heavily inspired by
;; - https://gitlab.com/magus/mes
;; - https://github.com/corgi-emacs/corgi
;; - https://github.com/otfrom/otfrom-org-emacs
;; - https://github.com/mpenet/emax/blob/master/init.el
;; - https://github.com/benjamin-asdf/dotfiles/blob/master/mememacs/.emacs-mememacs.d/init.el

;; Added early-init.el
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)

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
(use-package display-line-numbers
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

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
;; (require 'kixi-usability)
(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t))

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :straight t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.7))

(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;;([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ;;([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :straight t
  :after evil
  :config
  (general-evil-setup))

(general-create-definer kixi/leader-def
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

(kixi/leader-def
  "SPC" '(execute-extended-command :wk "M-x")
  "TAB" '(mode-line-other-buffer :wk "last buffer")
  "!" 'shell-command
  "u" 'universal-argument)

(kixi/leader-def
  :infix "b"
  "" '(:ignore t :wk "buffers")
  "b" '(consult-buffer :wk "switch")
  "d" '(kill-this-buffer :wk "kill")
  "e" '(erase-buffer :wk "erase"))

(kixi/leader-def
  :infix "E"
  "" '(:ignore t :wk "Emacs")
  "u" '(package-update-all :wk "update packages"))

(kixi/leader-def
  :infix "f"
  "" '(:ignore t :wk "files")
  "f" 'find-file
  "r" '(recentf-open :wk "recent"))

(kixi/leader-def
  :infix "h"
  "" '(:ignore t :wk "help/desc")
  "f" 'describe-function
  "i" 'info-display-manual
  "k" 'describe-key
  "m" 'describe-mode
  "v" 'describe-variable
  "w" 'woman)

(kixi/leader-def
  :infix "q"
  "" '(:ignore t :wk "quit")
  "r" 'restart-emacs
  "q" 'kill-emacs)

;; (provide 'kixi-usability)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-evil)
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-anzu
  :straight t
  :after evil)

(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :straight t
  :after evil
  :hook (magit-mode-hook . turn-off-evil-snipe-override-mode)
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; (use-package evil-unimpaired ; not available?
;;   :after evil)

(use-package evil-mc
  :straight t
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-easymotion
  :straight t
  :after evil)

(general-def :keymaps 'evilem-map
  "j" 'evil-avy-goto-char-timer)

(kixi/leader-def
  "j" '(:keymap evilem-map :wk "evilem"))

(use-package evil-lisp-state
  :straight t
  :after evil)

(kixi/leader-def
  :infix "k"
  "" '(:ignore t :wk "lisp")
  "$" 'sp-end-of-sexp
  "(" '(evil-lisp-state-insert-sexp-before :wk "insert-sexp-before")
  ")" '(evil-lisp-state-insert-sexp-after :wk "insert-sexp-after")
  "a" 'sp-absorb-sexp
  "b" 'sp-forward-barf-sexp
  "h" 'sp-backward-symbol
  "j" 'evil-next-close-paren
  "k" 'evil-previous-open-paren
  "l" 'sp-forward-symbol
  "H" 'sp-backward-sexp
  "L" 'sp-forward-sexp
  "w" '((lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")) :wk "wrap"))

;; (provide 'kixi-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-completion)
;; ********** Completion
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-indexed
                                vertico-mouse
                                vertico-quick
                                vertico-repeat
                                vertico-reverse))
  :init
  (setq vertico-cycle t)
  (vertico-mode)
  :config
  (with-eval-after-load 'evil
    (define-key vertico-map (kbd "C-j") 'vertico-next)
    (define-key vertico-map (kbd "C-k") 'vertico-previous)))

;; TODO: figure out why this breaks C-k
;; (use-package vertico-buffer
;;   :after vertico
;;   :straight nil
;;   :config
;;   (customize-set-variable 'vertico-buffer-display-action
;;                           '(display-buffer-below-selected (window-height . 13)))
;;   (vertico-buffer-mode))

; (use-package vertico-directory
;   :after vertico
;   :ensure nil
;  :bind (:map vertico-map
;              ("C-h" . vertico-directory-up)
;              ("RET" . vertico-directory-enter)
;              ("DEL" . vertico-directory-delete-char)
;              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-quick
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("M-j" . vertico-quick-insert)))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; TODO add more consult packages
;; consult-ag
;; consult-project-extra / consult-projectile
;; consult-yasnippet
;; consult-org-roam
;; consult-eglot / consult-lsp
(use-package consult
  :straight t
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         :map evil-normal-state-map
         ("C-." . embark-act))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point))

(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2)
  (corfu-echo-documentation 0.75)
  :config
  (global-corfu-mode 1))

;; Needed to make corfu-terminal work
(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

;; Needed to make corfu work in a terminal
(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package corfu-doc
  :straight t
  :after corfu
  :bind (:map corfu-map
              ("M-k" . corfu-doc-scroll-up)
              ("M-j" . corfu-doc-scroll-down))
  :hook
  (corfu-mode . corfu-doc-mode))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode +1)))

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; (provide 'kixi-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-dev)
;; ********** Basic dev stuff
(use-package smartparens
  :straight t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package forge
  :straight t
  :after magit)

(use-package tree-sitter
  :straight t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(kixi/leader-def
  :infix "g"
  "" '(:ignore t :wk "git")
  "c" 'magit-clone
  "i" 'magit-init
  "s" 'magit-status)

;; (provide 'kixi-dev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-projects)
(use-package tabspaces
  :straight t
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*")))

(general-def :keymaps 'project-prefix-map
  "o" '(tabspaces-open-or-create-project-and-workspace :wk "open"))

(kixi/leader-def
  ;; "t" '(:keymap tabspaces-command-map :package tabspaces :wk "tabs") ;; I really don't want to clash with toggle
  "p" '(:keymap project-prefix-map :wk "projs"))

;; (provide 'kixi-projects)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'kixi-org)
(defun kixi/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . kixi/org-mode-setup))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(general-create-definer kixi/org-mode-leader-def
  :states 'normal
  :keymaps 'org-mode-map
  :prefix ",")

(kixi/org-mode-leader-def
  :infix "t"
  "" '(:ignore t :wk "toggle")
  "l" 'org-toggle-link-display
  "t" 'org-todo
  "c" 'org-toggle-checkbox)

;; (provide 'kixi-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CIDER and clojure config
(use-package cider
  :straight t
  :config
  (setq nrepl-log-messages t
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
  ;; (add-hook 'cider-repl-mode-hook #'paredit-mode)
  ;; use lsp
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (setq lsp-keymap-prefix "M-l")
  (defun mpenet/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex

  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (lsp-completion-mode . mpenet/lsp-mode-setup-completion))

  :bind (:map lsp-mode-map
              ("M-l M-l" . lsp-execute-code-action)
              ("M-j d" . lsp-find-definition)
              ("M-j M-d" . lsp-find-definition)
              ("M-j r" . lsp-find-references)
              ("M-j M-r" . lsp-find-references))

  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

  (setq cljr-add-ns-to-blank-clj-files nil
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-auto-activate nil
        lsp-semantic-tokens-enable t
        ;; after last buffer closed, kill workspace
        lsp-keep-workspace-alive nil)

  :custom-face
  (lsp-face-semhl-namespace  ((t :inherit font-lock-type-face :weight normal)))
  (lsp-face-semhl-definition  ((t :inherit font-lock-function-name-face :weight normal))))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package lsp-treemacs
  :straight t
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package consult-lsp
  :straight t)
