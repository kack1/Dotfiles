(setq inhibit-splash-screen t
      inhibit-startup-message t
      use-file-dialog nil
      split-width-threshold 1
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

;; C prefs
(setq c-default-style "bsd"
      c-basic-offset 8)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(add-to-list 'image-types 'svg)

(hl-line-mode 1)
;; Stops cursor blinking
(blink-cursor-mode -1)

;; Save Mini-Buffer History
(setq history-length 25)

(put 'narrow-to-region 'disabled nil)
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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("epla" . "https://elpa.gnu.org/packages/")))

;; Straight.el Bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(load-theme 'leuven-dark)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which-Key
(use-package which-key
  :straight t
  :init  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1.0))

(use-package vertico
  :straight t
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert  ; Insert selected candidate into text area
   "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
   ;; NOTE 2022-02-05: Cycle through candidate groups
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group)
  :config
  (vertico-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :straight t
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light
						       nil))
  :init
  (marginalia-mode))

(use-package corfu
  :straight t
  ;; Optional customizations
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   ;; (corfu-auto t)                 ;; Enable auto completion
   (corfu-auto-delay 0)
   (corfu-auto-prefix 1)
   (corfu-separator ?\s)          ;; Orderless field separator
   (corfu-quit-no-match t)
   (corfu-popupinfo-delay 0.0)
   (corfu-popupinfo-max-height 20)
   :init
  (global-corfu-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )

;; ;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :init
  (setq register-preview-delay 1.0
        register-preview-function #'consult-register-format)
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
)

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  ([remap projectile-ripgrep] . consult-ripgrep)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package general
  :straight t
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer ka/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ka/leader-keys
    "." 'find-file
    "b" '(:ignore t :which-key "Buffers")
    "bi" 'ibuffer
    "bk" 'kill-buffer
    "fs" 'save-buffer
    "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/init.el")))))

(use-package evil
  :straight t
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
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons")
)
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package lambda-line
  :straight (el-patch :type git :host github :repo "Lambda-Emacs/lambda-line")
  :custom
    (setq lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
    (setq lambda-line-position 'bottom) ;; Set position of status-line
    (setq lambda-line-abbrev t) ;; abbreviate major modes
    (setq lambda-line-hspace "  ")  ;; add some cushion
    (setq lambda-line-prefix t) ;; use a prefix symbol
    (setq lambda-line-status-invert nil)  ;; no invert colors
    (setq lambda-line-gui-ro-symbol  " ⨂") ;; symbols
    (setq lambda-line-gui-rw-symbol  " ◯")
    (setq lambda-line-space-top +.20)  ;; padding on top and bottom of line
    (setq lambda-line-space-bottom -.20)
    (setq lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
    (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_")))
  :init
    ;; activate lambda-line
    (lambda-line-mode)
  )

(use-package magit
  :straight t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode))

(use-package org
  :straight t
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . efs/org-mode-visual-fill))

(defun efs/lsp-mode-setup ()
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;;:hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; (use-package python-mode
;;    :ensure t
;;    :hook (python-mode . lsp-deferred))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(straight-use-package 'ement)

(use-package blacken
  :straight t
  :hook ((python-mode . blacken-mode)))

(straight-use-package 'yasnippet-snippets)
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")

(require 'yasnippet)
(yas-global-mode 1)

(straight-use-package 'evil-tex)
(straight-use-package 'iedit)
(straight-use-package 'flymake-cursor)

(use-package cedet)
(semantic-mode 1)

(use-package magit-todos
  :defer t)

(straight-use-package 'consult-lsp)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;; Balanced Parenthsis, Brackets, etc...
(electric-pair-mode 1)

;; Got
(use-package vc-got
  :straight t
  :config
  (setq vc-got-program "~/bin/got"))
(setq debug-on-error t)

(provide 'init.el)
;;; init.el ends here

