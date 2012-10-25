;;; python-test.el --- 
;; 
;; Filename: python-test.el
;; Description: Run Python unit-tests from Emacs
;; Author: Free Ekanayaka
;; Created: Wed May 13 20:37:09 2009 (+0200)
;; Version: 0.0.1
;; Last-Updated: Thu May 14 21:37:40 2009 (+0200)
;;           By: Free Ekanayaka
;;     Update #: 1
;; URL: 
;; Keywords: 
;; Compatibility: GNU Emacs 22.2
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; This package provides function to easy trigger Python unit-tests
;; from within Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Installation:
;;
;; Copy python-tests.el to your load-path and add to your ~/.emacs
;;
;;  (require 'multi-term)
;;
;; Note that this module relies on python-mode.el and unit-test.el.
;; 
;; Bind the function `py-test-run' to convenient keys with something
;; like:
;;
;;   (define-key global-map (kbd "C-c t") 'py-test-run)
;;
;; You will need to define a value for `py-test-project-root' and
;; `py-test-command'  in any buffer you are interested in running
;; tests from. If the hierarchy of your Python modules does not
;; start at the root of the project , you may need to define also
;; `py-test-module-root'. An easy way to achive this is to add
;; and hook to the python-module-mode, for example:
;;
;; (add-hook 'python-mode-hook 'python-test-settings)
;;
;; (defun python-test-settings ()
;;   (let ((dir (expand-file-name default-directory)))
;;     (setq py-test-project-root (expand-file-name ("~/my/project/")))
;;     (setq py-test-module-root "src/")
;;     (setq py-test-command "trial -r glib2 %module.%suite.%case")))
;;
;; When defining `py-test-command' you can use the three macros
;;  `%module', `%suite' and `%case' which will be replaced with
;; the pythoh module path, and the current testsuite/case
;;  respectively.
;;
;; For example, if you have a bunch of test suites and cases
;; in `/home/me/project/x/src/foo/test_foo.py' like:
;;
;; class MyTest(unittest.TestCase):
;;
;;     def test_me(self):
;;         pass
;; 
;; and your point is inside the body of `test_me', then the
;; command will be "trial -r glib2 foo.test_foo.MyTest.test_me".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 14-May-2009    Free Ekanayaka  Added documentation
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'python-mode)
(require 'unit-test)

(make-variable-buffer-local 'py-test-project-root)
(make-variable-buffer-local 'py-test-module-root)
(make-variable-buffer-local 'py-test-command)

; A couple of tweaks to unit-test to make things look nicer
(add-hook 'unit-tests-failed-hook 'py-test-failed-hook)
(add-hook 'unit-tests-passed-hook 'py-test-passed-hook)

(defun py-test-failed-hook ()
            (switch-to-buffer "*Shell Command Output*")
            (compilation-minor-mode 1)
            (switch-to-other-buffer)
)
(defun py-test-passed-hook ()
            (kill-buffer "*Shell Command Output*")
)


; These two functions should actually be better put in python-mode.el
(defun py-current-class ()
  "Name of the current class."
  (first (split-string (substring-no-properties (py-current-defun)) "\\.")))

(defun py-current-method ()
  "Name of the current method."
  (second (split-string (substring-no-properties (py-current-defun)) "\\.")))

; Real stuff comes here
(defun py-test-current-case ()
  "Name of the current test case."
  (let ((method (py-current-method)))
    (if (string-match "^test_" method)
        method
      nil)))

(defun py-test-current-suite ()
  "Name of the currente test suite."
  (let ((class (py-current-class)))
    (if (string-match "[[:alnum:]]+Test" class)
        class
      nil)))

(defun py-test-current-module ()
  "Name of the currente test module."
  (if (string-match "^test_" (file-name-nondirectory (buffer-file-name)))
      (replace-regexp-in-string
       py-test-module-root ""
       (replace-regexp-in-string
        "/" "\."
        (replace-regexp-in-string
         py-test-project-root ""
         (file-name-sans-extension (buffer-file-name)))))
    nil))

(defun py-test-run ()
  "Run a Python unit-test."
  (interactive)
  (if (buffer-modified-p (current-buffer))
      (save-buffer))
  (let ((command (replace-regexp-in-string
                 "%module" (py-test-current-module)
                 (replace-regexp-in-string
                  "%suite" (py-test-current-suite)
                  (replace-regexp-in-string
                   "%case" (py-test-current-case)
                   py-test-command)))))
    (progn
      (setq unit-test-command (lambda () (zerop (shell-command command))))
      (cd py-test-project-root)
      (run-unit-tests))))

(provide 'python-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-test.el ends here
