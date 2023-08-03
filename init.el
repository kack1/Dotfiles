(setq inhibit-splash-screen t
      inhibit-startup-message t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1) ;; Disable Tooltips
(set-fringe-mode 10) ;; Give some breathing room
;; Enableing Line numbers
(global-display-line-numbers-mode 1)
;; Relative Line numbers
(setq display-line-numbers 'relative)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-to-list 'image-types 'svg)

(set-face-attribute 'default nil :font "JetBrains Mono-17")
(set-face-attribute 'mode-line nil :font "JetBrains Mono-15")

;; Modus theme

;;(setq modus-themes-mode-line '(accented borderless)
      ;;modus-themes-region '(bg-only)
      ;;modus-themes-completions 'minimal
      ;;modus-themes-bold-constructs t
      ;;modus-themes-italic-constructs t
      ;;modus-themes-paren-match '(bold intense)
      ;;modus-themes-headings
      ;;'((1 . (rainbow overlone background 1.4))
	;;(2 . (rainbow background 1.3))
	;;(3 . (rainbow bold 1.2))
	;;(t . (semilight 1.1)))
      ;;modus-themes-org-blocks 'tinted-background)

;;load theme
;;(load-theme 'modus-vivendi t)
;; Highlight Current Line
(hl-line-mode 1)
;; Stops cursor blinking
(blink-cursor-mode -1)

;; Save Mini-Buffer History
(setq history-length 25)
(savehist-mode 1)

;; Remember where we left!
(save-place-mode 1)
;; Disable Line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Forward along custom settings
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; Now load it
(load custom-file 'noerror 'nomessage)
;; Refresh Dired
(setq global-auto-revert-non-file-buffers t)

;; Initalize Package source
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("epla" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  ( package-refresh-contents))

;; Initalize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-Key
(use-package which-key
  :init  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
 :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package general
   :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer ka/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ka/leader-keys
    "." 'counsel-find-file
    "b" '(:ignore t :which-key "Buffers")
    "bi" 'ibuffer
    "bk" 'kill-buffer
    "fs" 'save-buffer
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons")
)
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Desktop")
    (setq projectile-project-search-path '("~/Desktop")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(setq auth-sources '("~/.authinfo"))

       
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package org
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;;:hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))
; add below line to init.el
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package ement)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package yasnippet-snippets)
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package cmake-ide)
(use-package rtags)
(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

(use-package direnv
 :config
 (direnv-mode))

(use-package irony
  :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck-irony
  :config
  (eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
