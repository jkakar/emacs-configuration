;;; alpaca-mode --- A major mode for the Alpaca language.

;; Copyright (C) 2016 The Alpaca Community
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Author: Eric Bailey, Tyler Weir
;; Keywords: languages alpaca

;;; Commentary:

;;

;;; Code:

(defconst alpaca-font-lock-keywords
  ;; Reserved words
  `(,(rx symbol-start
         (or "let" "in"
             "match" "with"
             "beam"
             "spawn" "send" "receive" "after"
             "test"
             "error" "exit" "throw"
             "true" "false"
             "unit" "size" "end" "sign"
             "big" "little" "native" "utf8")
         symbol-end)

    ;; Module definition
    (,(rx symbol-start
          (group "module") (1+ space)
          (group (1+ (or word ?_))))
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ;; Export function
    (,(rx line-start
          (group "export") (1+ space)
          (group (: (1+ (or word ?_)) ?/ (1+ digit))
                 (* ?, (opt ?\n) (* space)
                    (: (1+ (or word ?_)) ?/ (1+ digit))))
          line-end)
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face))

    ;; Export type
    (,(rx line-start
          (group "export_type") (1+ space)
          (group (1+ (or word ?_))
                 (* ?, (opt ?\n) (* space)
                    (1+ (or word ?_))))
          line-end)
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))

    ;; FIXME: Multi-line comments
    ;; (,(rx (group "{-") (group (* anything)) (group "-}"))
    ;;  (1 font-lock-comment-delimiter-face)
    ;;  (2 font-lock-comment-face)
    ;;  (3 font-lock-comment-delimiter-face))

    ;; Type definition
    (,(rx line-start
          (group "type") (1+ space) (group (1+ (or word ?_))) (1+ space)
          (group (1+ ?' (1+ (or word ?_)) (1+ space))) ?=)
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))

    ;; Function definition
    (,(rx symbol-start
          (group (1+ (or word ?_))) (1+ space)
          (group (1+ (or word ?_ space))) (1+ space) ?=)
     (1 font-lock-function-name-face))

    ;; Data constructors
    (,(rx symbol-start
          (group (char upper) (1+ (or word ?_)))
          symbol-end)
     (1 font-lock-type-face))

    (,(rx (or digit symbol-start)
          (group (or ?- ?+ ?%))
          (or digit symbol-end))
     (1 font-lock-variable-name-face))

    (,(rx (not word) (group (or "->" "==" ?= "!=" ">=" "=<" ?> ?< ?=)))
     (1 font-lock-variable-name-face))

    ;; "Don't care"
    (,(rx space (group ?_) space)
     (1 font-lock-variable-name-face))

    ;; BIFs
    (,(rx symbol-start
          (group "is_" (or "integer" "float" "atom" "bool"
                           "list" "string" "chars" "pid" "binary"))
          symbol-end)
     (1 font-lock-builtin-face))))

;;;###autoload
(defun alpaca-mode ()
  "Major mode for editing Alpaca files."
  (interactive)
  (kill-all-local-variables)

  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    (set-syntax-table table))

  (setq major-mode 'alpaca-mode)
  (set (make-local-variable 'font-lock-defaults)
       '((alpaca-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords)
       alpaca-font-lock-keywords)

  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.alp\\'" . alpaca-mode))

(provide 'alpaca-mode)
;;; alpaca-mode.el ends here
