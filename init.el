(setq inhibit-splash-screen t
      inhibit-startup-message t
      visible-bell t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enableing Line numbers
(global-display-line-numbers-mode 1)
;; Relative Line numbers
(setq display-line-numbers 'relative)

;;Theme
(load-theme 'modus-vivendi t)
;; Highlight Current Line
(hl-line-mode 1)
;; Stops cursor blinking
(blink-cursor-mode -1)

;; Save Mini-Buffer History
(setq history-length 25)
(savehist-mode 1)

;; Remember where we left!
(save-place-mode 1)

;; Forward along custom settings
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; Now load it
(load custom-file 'noerror 'nomessage)
