;;; package -- Summary
;;;

;;; Commentary:

;;; Code:

;; Key-bindings

;; This saves me from having to contort my fingers to reach Meta-x.
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)


;; Setup `package'.
(require 'package)

;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives
      '(("melpa"     . "https://melpa.org/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package general :ensure t)

(use-package hydra :ensure t)

(use-package counsel
  :ensure t
  :pin melpa
  :config (general-define-key
           "M-x" 'counsel-M-x
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
             )))

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
  :no-require t
  :init (global-flycheck-mode)
  :config
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (any " ")
                                     (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint)
  :diminish (flycheck-mode . " Ⓢ"))

(use-package whitespace
  :ensure t
  :init (progn
          (setq whitespace-style '(face trailing tabs tab-mark lines-tail))
          (add-hook 'prog-mode-hook 'whitespace-mode))
  :config  (setq whitespace-line-column 80)
  :diminish (whitespace-mode . " Ⓦ"))

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

(provide 'init)
