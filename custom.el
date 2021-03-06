(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(exec-path-from-shell-check-startup-files nil)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "GOPATH")))
 '(gofmt-show-errors nil t)
 '(markdown-hide-urls t)
 '(package-selected-packages
   (quote
    (jsx-mode rjsx-mode wgrep protobuf-mode elixir elixir-mode base16-theme json-mode sql-mode git-link edts erlang thrift projectile counsel-projectile crux graphviz-dot-mode go-eldoc go-mode flycheck dumb-jump magit smex counsel swiper ws-butler exec-path-from-shell use-package)))
 '(projectile-globally-ignored-file-suffixes (quote (".pdf")))
 '(projectile-globally-unignored-files (quote (".projectile" ".dir-locals.el")))
 '(rust-format-on-save t t)
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-command-wrapper-function
           (lambda
             (command)
             (append
              (quote
               ("bundle" "exec"))
              command)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
