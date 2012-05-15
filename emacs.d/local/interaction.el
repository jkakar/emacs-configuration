;; Browsing

(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-generic)


;; Usability

(defun join-next-line ()
  (interactive) (join-line t))

(defun usability-settings ()
  (fset 'yes-or-no-p 'y-or-n-p )
  (setq-default indent-tabs-mode nil)
  (setq make-backup-files nil)
  (setq tramp-default-method "scp")
  (iswitchb-mode t)
  (toggle-uniquify-buffer-names)
  (put 'narrow-to-region 'disabled nil)
)


;; Keybindings

(defun set-key-bindings ()
  (add-hook 'outline-minor-mode-hook
	    (lambda ()
	      (define-key outline-minor-mode-map
		"\C-q" 'outline-toggle-children)))
  (add-hook 'python-mode-hook
	    (lambda ()
	      (define-key py-mode-map
		"\C-^" 'py-test-run)))
  (global-set-key [?\C-j] 'join-next-line))


;; Edit server

(server-start)


;; Load all settings

(usability-settings)
(set-key-bindings)

(provide 'interaction)
