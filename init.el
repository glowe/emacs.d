;;; package -- Summary
;;;

;;; Commentary:

;;; Code:

;; Key-bindings

;; This saves me from having to contort my fingers to reach Meta-x.
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)
;; Override the default key binding: only delete ws _ahead_ of point.
(define-key global-map (kbd "M-\\") 'c-hungry-delete-forward)


;; Setup `package'.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(use-package helm
  :ensure t
  :init (helm-mode 1)
  :bind (
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-bookmarks)
         (:map helm-mode-map
               "<tab>" . helm-execute-persistent-action))
  :diminish helm-mode)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

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

(use-package nand2tetris
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :ensure t
  :config (progn
            (setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
            (setq racer-rust-src-path "/Users/gl/Code/rust/src") ;; Rust source code PATH
            )
  :init (progn
          (add-hook 'rust-mode-hook #'racer-mode)
          (add-hook 'racer-mode-hook #'eldoc-mode)
          (add-hook 'racer-mode-hook #'company-mode)))

(set-frame-font "Input Mono-14")
(setq column-number-mode t)
(setq dired-recursive-deletes 'top)
(tool-bar-mode -1)
(setq-default indent-tabs-mode nil)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
