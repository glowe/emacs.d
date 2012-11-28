;;;;;;;;;;;;;;
;; Requires ;;
;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lib")

(require 'cc-mode)
(require 'cl)
(require 'package)
(require 'xcscope)

(package-initialize)
(require 'go-mode)

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
(set-default-font "Source Code Pro-14")
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
