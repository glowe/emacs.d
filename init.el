;;; package -- Summary
;;;

;;; Commentary:

;;; Code:

;; Setup `package'.
(require 'package)

;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives
      '(("melpa"     . "http://melpa.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package theme-changer
  :ensure t
  :init
  (setq calendar-location-name "Toronto, ON")
  (setq calendar-latitude 43.8)
  (setq calendar-longitude -79.4)
  :config
  (change-theme 'sanityinc-tomorrow-day 'sanityinc-tomorrow-night))

(use-package general :ensure t)

(use-package hydra :ensure t)

(use-package counsel
  :ensure t
  :pin melpa
  :config (general-define-key
           "C-x C-m" 'counsel-M-x
           "C-x C-f" 'counsel-find-file
           "<f1> f" 'counsel-describe-function
           "<f1> v" 'counsel-describe-variable
           "<f1> l" 'counsel-load-library
           "<f2> i" 'counsel-info-lookup-symbol
           "<f2> u" 'counsel-unicode-char
           ;; shell and system tools
           "C-c g" 'counsel-git
           "C-c j" 'counsel-git-grep
           "C-c k" 'counsel-ag
           "C-x l" 'counsel-locate
           ))

(use-package swiper
  :ensure t
  :pin melpa
  :config (progn
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (general-define-key
             "C-s" 'swiper
             ;; resume
             "C-c C-r" 'ivy-resume
             "C-x b" 'ivy-switch-buffer
             )))

(use-package ivy-hydra
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :init (exec-path-from-shell-initialize))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . " Ⓢ"))

(use-package whitespace
  :ensure t
  :init (progn
          (setq whitespace-style '(face trailing tabs tab-mark lines-tail))
          (add-hook 'prog-mode-hook 'whitespace-mode))
  :config  (setq whitespace-line-column 80)
  :diminish (whitespace-mode . " Ⓦ"))

(use-package toml-mode
  :ensure t)

(use-package typo                       ; Automatically use typographic quotes
  :ensure t
  :init (progn
          (typo-global-mode)
          (dolist (hook '(markdown-mode-hook
                          rst-mode-hook
                          text-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish (typo-mode . " Ⓣ"))

(use-package jedi
  :ensure t
  :init (add-hook 'python-mode 'jedi:setup)
  :config (setq jedi:complete-on-dot t))

(use-package magit
  :ensure t)

(use-package rust-mode
  :ensure t
  :init (add-hook 'rust-mode-hook
                  (lambda ()
                    (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
  :diminish rust-mode)

(use-package cargo
  :ensure t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package yasnippet
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package company
  :ensure t
  :pin melpa)

(use-package racer
  :ensure t
  :config (progn
            (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
            (setq racer-rust-src-path "~/Code/rust/src") ;; Rust source code PATH
            )
  :init (progn
          (add-hook 'rust-mode-hook #'racer-mode)
          (add-hook 'racer-mode-hook #'eldoc-mode)
          (add-hook 'racer-mode-hook #'company-mode)
          (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
          (setq company-tooltip-align-annotations t)
          ))

(when (image-type-available-p 'xpm)
  (use-package powerline
    :ensure t
    :config
    (setq powerline-display-buffer-size nil)
    (setq powerline-display-mule-info nil)
    (setq powerline-display-hud nil)
    (when (display-graphic-p)
      (powerline-default-theme))))

(set-frame-font "Fantasque Sans Mono-16")
(setq column-number-mode t)
(setq dired-recursive-deletes 'top)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(put 'narrow-to-region 'disabled nil)
(set-scroll-bar-mode nil)

;; save backups to temp directory
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#424242"))
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#424242")
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow org-jira racer ivy-hydra password-store yasnippet use-package typo powerline osx-trash magit jedi hydra general flycheck-rust exec-path-from-shell counsel company cargo)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
