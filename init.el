;;;;;;;;;;;;;;
;; Requires ;;
;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lib")

(require 'cc-mode)
(require 'cl)
(require 'package)
(require 'xcscope)

;; The following packages are installed via package
(package-initialize)
(require 'go-mode)
(require 'color-theme)


;;;;;;;;;;;;;;;;
;; Path setup ;;
;;;;;;;;;;;;;;;;

(defun read-file-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (condition-case nil (insert-file-contents file) (file-error nil))
    (split-string
     (buffer-string) "\n" t)))

;; Prepend paths in /etc/paths.d/ to our PATH environment variable.
(let* ((paths-dir (file-name-as-directory "/etc/paths.d"))
       (paths-dir-list1 (condition-case nil (directory-files paths-dir) (file-error nil)))
       (paths-dir-list2 (remove-if #'(lambda (s) (or (string= s ".") (string= s ".."))) paths-dir-list1))
       (paths-filenames (mapcar #'(lambda (s) (concat paths-dir s)) paths-dir-list2))
       (paths (apply #'append (mapcar #'read-file-lines paths-filenames)))
       (paths (nconc paths (list (getenv "PATH"))))
       (paths (nconc paths (read-file-lines "/etc/paths")))
       (path-string (mapconcat #'identity paths ":")))
  (setenv "PATH" path-string))

;;;;;;;;;;;;;;;;;;;;
;; Customizations ;;
;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook #'gofmt-before-save)

;; Experiment: See if disabling backups and autosave files helps
;; minimize the frequency with which emacs locks up.
(setq backup-inhibited t)
(setq auto-save-default nil)

(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))
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
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)
;; Override the default key binding: only delete ws _ahead_ of point.
(define-key global-map (kbd "M-\\") 'c-hungry-delete-forward)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-x C-a") 'kmacro-call-macro)

;;;;;;;;;;;;;
;; Startup ;;
;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Source Code Pro")))))
