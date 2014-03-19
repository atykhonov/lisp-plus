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

;;
;; Assuming that you cloned `lisp-plus' to the
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

(require 'thingatpt)



(defvar lisp-plus-insert-undo-form nil)

(defvar lisp-plus-insert-mode nil)


(defun lisp-plus-newline () 
  "Breaks the code line depending on context." 
  (interactive) 
  (lisp-plus-goto-next-arg) 
  (newline-and-indent))

(defun lisp-plus-pre-command-hook ()
  "Hook for `pre-command-hook'."
  (interactive)
  (when (and (equal last-command
                    'lisp-plus-newline)
             (equal this-command
                    'lisp-plus-newline))
    (delete-indentation))
  (when (and (equal last-command
                    'lisp-plus-first-arg-insert)
             (equal this-command
                    'lisp-plus-first-arg-insert))
    (when lisp-plus-insert-undo-form
      (eval lisp-plus-insert-undo-form)
      (setq lisp-plus-insert-undo-form nil))))

(add-hook 'pre-command-hook 'lisp-plus-pre-command-hook)
(remove-hook 'pre-command-hook 'lisp-plus-pre-command-hook)

(defun lisp-plus-goto-first-arg ()
  (let ((bound (bounds-of-thing-at-point 'list)))
    (goto-char (car bound))
    (forward-char)
    (when (not (null (symbol-at-point)))
      (forward-sexp 1))))

(defun lisp-plus-goto-last-arg ()
  (let ((continue t))
    (while continue
      (condition-case nil
          (progn
            (forward-sexp))
        (error (setq continue nil))))))

(defun lisp-plus-first-arg-insert ()
  (interactive)
  (lisp-plus-goto-next-up-or-down-list)
  (lisp-plus-goto-first-arg)
  (insert " ")
  (if (looking-back "( ")
      (progn
        (backward-char)
        (setq lisp-plus-insert-undo-form '(delete-forward-char 1)))
    (setq lisp-plus-insert-undo-form '(delete-backward-char 1))))

(defun lisp-plus-first-arg-replace ()
  (lisp-plus-goto-first-arg)
  (mark-sexp)
  (delete-active-region t)
  (insert " "))

(defun lisp-plus-first-arg-delete ()
  (interactive)
  (lisp-plus-goto-first-arg)
  (mark-sexp)
  (delete-active-region))

(defun lisp-plus-first-arg-newline ()
  (interactive)
  (lisp-plus-goto-first-arg)
  (newline-and-indent))

(defun lisp-plus-last-arg-insert ()
  (interactive)
  (lisp-plus-goto-last-arg)
  (insert " "))

(defun lisp-plus-last-arg-replace ()
  (interactive)
  (lisp-plus-goto-last-arg)
  (backward-sexp)
  (mark-sexp)
  (delete-active-region t)
  (insert " "))

(defun lisp-plus-last-arg-delete ()
  (interactive)
  (lisp-plus-goto-last-arg)
  (backward-sexp)
  (mark-sexp)
  (delete-active-region))

(defun lisp-plus-last-arg-newline ()
  (interactive)
  (lisp-plus-goto-last-arg)
  (newline-and-indent))

(defun lisp-plus-goto-next-arg ()
  (interactive)
  (let* ((point (point))
         (sexp-point (point-max))
         (up-list-point (point-max))
         (down-list-point (point-max)))
    (save-excursion
      (condition-case nil
          (progn
            (forward-sexp 1)
            (setq sexp-point (point)))
        (error nil))
      (goto-char point)
      (condition-case nil
          (progn
            (up-list)
            (setq up-list-point (point)))
        (error nil))
      (goto-char point)
      (condition-case nil
          (progn
            (down-list)
            (setq down-list-point (point)))
        (error nil)))
    (goto-char (min sexp-point
                    up-list-point
                    down-list-point))))

(defun lisp-plus-goto-next-up-or-down-list ()
  (let ((point (point))
        (last-point (point))
        (next-point nil)
        (arg-point nil)
        (continue t))
    (save-excursion
      (while continue
        (lisp-plus-goto-next-arg)
        (when (equal last-point (point))
          (setq continue nil))
        (when continue
          (setq arg-point (point))
          (lisp-plus-goto-first-arg)
          (when (> (point) point)
            (setq continue nil)
            (setq next-point (point)))
          (goto-char arg-point))
        (setq last-point (point))))
    (goto-char next-point)))


(provide 'lisp-plus)

;;; lisp-plus.el ends here
