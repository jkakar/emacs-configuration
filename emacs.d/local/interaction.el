;; Browsing

(setq browse-url-generic-program "chrome")
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
  (setq ring-bell-function #'ignore)
  (setq confirm-kill-emacs 'yes-or-no-p))

(defun integrate-copy-and-paste ()
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Keybindings

(defun set-key-bindings ()
  (add-hook 'outline-minor-mode-hook
	    (lambda ()
	      (define-key outline-minor-mode-map
		"\C-q" 'outline-toggle-children)))

  ;; (add-hook 'python-mode-hook
  ;;       (lambda ()
  ;;         (define-key py-mode-map
  ;;   	"\C-^" 'py-test-run)))

  ;; remap C-a to `smarter-move-beginning-of-line'
  (global-set-key [remap move-beginning-of-line]
                  'smarter-move-beginning-of-line)

  (add-hook 'ruby-mode-hook
	    (lambda ()
	      (define-key ruby-mode-map
		[?\C-j] 'join-next-line)))

  (global-set-key [?\C-j] 'join-next-line))

;; Spell checking

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Dump jump

(dumb-jump-mode)
(setq dumb-jump-force-searcher 'rg)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)
  :ensure)

;; Edit server

(server-start)

;; Load all settings

(usability-settings)
(set-key-bindings)
;;(integrate-copy-and-paste)

(provide 'interaction)
