(cond ((file-exists-p "~/stripe/stripemacs/stripemacs.el")
       (defvar devbox-machine "qa-mydev--040facb995d3a8723.northwest.stripe.io")
       (defvar stripe-username "jkakar")
       (load "~/stripe/stripemacs/stripemacs.el")))

(provide 'stripe)
