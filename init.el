;;;;;;;;;;;;;;
;; Requires ;;
;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lib")

(require 'cc-mode)
(require 'cl)
(require 'package)
(require 'xcscope)

;;;;;;;;;;;;;;;;;;;;
;; Customizations ;;
;;;;;;;;;;;;;;;;;;;;

(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))
(set-default-font "Menlo-16")
;; Although I find backup files annoying, sometimes they are useful.
;; The least evil thing to do is keep them in one place.
(setq backup-directory-alist
      (cons (cons "." (expand-file-name "~/tmp/backups")) nil))
;; Enable downcase a region
(put 'downcase-region 'disabled nil)
;; For tunnel vision superpowers.
(put 'narrow-to-region 'disabled nil)
;; Enable viewing images in emacs.
(put 'image-toggle-display 'disabled nil)
;; Fontify all buffers
(global-font-lock-mode 1)
;; maximum colors
(setq font-lock-maximum-decoration t)
;; Always use transient marks
(transient-mark-mode 1)
(toggle-uniquify-buffer-names)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; This is essential to ensure that hard links are broken when saved.
(setq file-precious-flag t)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq-default truncate-lines nil)
(setq truncate-partial-width-windows nil)
(setq column-number-mode t)
(setq line-number-mode t)
(setq vc-stay-local nil)
(setq dired-recursive-deletes 'top)
(ansi-color-for-comint-mode-on)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;
;; Bindings ;;
;;;;;;;;;;;;;;

;; Keep the fingers on the Control key is superior than using meta
(global-set-key [?\C-x ?\C-m] 'execute-extended-command)
;; Override the default key binding: only delete ws _ahead_ of
;; point.
(global-set-key [?\M-\\] 'c-hungry-delete-forward)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map "\C-x\C-a" 'kmacro-call-macro)

;;;;;;;;;;;;;
;; Startup ;;
;;;;;;;;;;;;;

(server-start)
