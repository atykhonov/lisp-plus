;;; lisp-plus.el --- This is extended version of the lisp.el Emacs
;;; library.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/lisp-plus/
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation

;; First of all please install the required dependency `smart-forward'
;; (Read: https://github.com/magnars/smart-forward.el)
;;
;; Then assuming that you cloned `lisp-plus' to the
;; `~/.emacs.d/lisp-plus/' folder. Add the following lines to your
;; `.emacs' file:
;;
;; (add-to-list 'load-path "~/.emacs.d/lisp-plus/")
;; (require 'lisp-plus)
;;

;;; Commentary:

;;
;;
;;

;;; Code:

(require 'smart-forward)


(defun lisp-plus-return ()
  "Breaks the code line depending on context."
  (interactive)
  (smart-forward)
  (newline-and-indent))

(defun lisp-plus-pre-return-command-hook ()
  "Hook for `pre-command-hook'."
  (interactive)
  (when (and (equal last-command
                    'lisp-plus-return)
             (equal this-command
                    'lisp-plus-return))
    (delete-indentation)))

(add-hook 'pre-command-hook 'lisp-plus-pre-return-command-hook)


(provide 'lisp-plus)

;;; lisp-plus.el ends here
