;; Python

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/jkakar/.emacs.d/plugins/epylint.py" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.tac\\'" flymake-pyflakes-init)))

(defun python-outline-settings ()
  (outline-setup "^class \\|[ 	]*def \\|^#"))

(defun my-flymake-find-file-hook ()
  (if (file-writable-p buffer-file-name)
      (flymake-find-file-hook)))

(defun unset-python-newline-and-indent ()
  (local-unset-key [?\C-j]))

(add-hook 'find-file-hook 'my-flymake-find-file-hook)
(add-hook 'python-mode-hook 'python-outline-settings)
(add-hook 'python-mode-hook
      '(lambda () "Defaults for Python mode." (setq fill-column 78)))
(add-hook 'python-mode-hook 'unset-python-newline-and-indent)
(setq auto-mode-alist (cons '("\\.tac$" . python-mode) auto-mode-alist))
(setq tags-table-list-default '("~/.etags/twisted" "~/.etags/txamqp"))

;; Javascript

(setq javascript-mode-hook
      '(lambda () "Defaults for Javascript mode." (setq fill-column 78)))


;; Restructured text

(setq rst-mode-hook
      '(lambda () "Defaults for ReST mode." (setq fill-column 78)))


;; C

(setq-default c-basic-offset 4)

;; C++

(setq c++-mode-hook
      '(lambda () "Defaults for C++ mode." (setq fill-column 78)))
(c-set-offset 'substatement-open 0)


;; XML

(setq auto-mode-alist (cons '("\\.pt$" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rdf$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xul$" . xml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zcml$" . xml-mode) auto-mode-alist))


;; Go

;; FIXME Figure out how to pass other local .go files to 6g to avoid
;; spurious errors.

;; (defvar go-compiler "6g")

;; (defun flymake-go-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list go-compiler (list "-o" "/dev/null" temp-file))))

;; (push '(".+\\.go$" flymake-go-init) flymake-allowed-file-name-masks)
;; (add-hook 'go-mode-hook 'flymake-mode)

(setq go-mode-hook
      '(lambda () "Defaults for Go mode." (setq fill-column 78)))
(add-hook 'go-mode-hook 'highlight-80+-mode)


;; org-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; Markdown

(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))


(provide 'programming)
