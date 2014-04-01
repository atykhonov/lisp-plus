(defun escr-region-screenshot ()
  "Make screenshot from the current region.  Please note that it
is possible to make screenshot only from the text which is
visible on the screen."
  (interactive)
  (let ((window-id (frame-parameter (selected-frame) 'window-id))
        (char-height (frame-char-height))
        (char-width (frame-char-width))
        (filename (expand-file-name
                   (concat escr-screenshot-directory
                           "/"
                           (format-time-string escr-filename-format
                                               (current-time)))))
        (window-start-line nil)
        (window-region-beginning-line nil)
        (window-region-end-line nil)
        (screenshot-height 0)
        (screenshot-width 0)
        (screenshot-x (nth 0 (window-pixel-edges)))
        (screenshot-y (nth 1 (window-pixel-edges)))
        (selection-start (region-beginning))
        (selection-end (region-end))
        (current-point (point))
        (crop ""))

    (escr--check-directory)

    (setq window-start-line (line-number-at-pos (window-start)))

    (goto-char selection-start)
    (setq window-region-beginning-line (line-number-at-pos))

    (goto-char selection-end)
    (setq window-region-end-line (line-number-at-pos))

    (goto-char current-point)

    (if (integerp escr-screenshot-width)
        (setq screenshot-width (* char-width escr-screenshot-width))
      (setq screenshot-width (nth 2 (window-pixel-edges))))

    (when escr-exclude-fringes
      (setq screenshot-width (- screenshot-width
                                (nth 0 (window-fringes))))
      (when (null escr-screenshot-width)
        (setq screenshot-width (- screenshot-width
                                  (nth 1 (window-fringes))))))

    (setq screenshot-height (* (+ (- window-region-end-line
                                     window-region-beginning-line)
                                  1)
                               char-height))

    (when escr-exclude-fringes
      (setq screenshot-x (+ screenshot-x
                            (nth 0 (window-fringes)))))

    (setq screenshot-y (+ screenshot-y
                          (* (- window-region-beginning-line
                                window-start-line)
                             char-height)))

    (deactivate-mark t)
    (redisplay t)

    (escr--screenshot screenshot-y
                      screenshot-x
                      screenshot-width
                      screenshot-height)))

;; (defmacro save-form-excursion (&rest body)
;;   `(let ((reg-beg (region-beginning))
;;          (reg-end (region-end)))
;;      ,@body
;;   )

(defmacro with-current-form (&rest body)
  `(let ((form-beginning (region-beginning))
         (form-end (region-end))
         (form-string nil)
         (form-formatting nil)
         (form nil))
     (setq form-string (buffer-substring-no-properties reg-beg reg-end))
     (setq form-formatting (save-formatting form-string))
     (setq form (read form-string))
     ,@body))

(defmacro update-current-form (&rest body)
  @body
  `(save-excursion
     (delete-region form-beginning form-end)
     (insert "(")
     (dolist (el (apply-formatting lst formatting))
       (insert el))
     (insert ")")
     (goto-char form-end)
     (exchange-point-and-mark)))

(defmacro save-form-excursion (&rest body)
  `(let ((form-save-excursion t)
         (deactivate-mark nil))
     ,@body))

(defmacro with-current-form (&rest body)
  `(let ((reg-beg (region-beginning))
         (reg-end (region-end))
         (form nil)
         (form-string nil)
         (form-formatting nil))
     (setq form-string (buffer-substring-no-properties reg-beg reg-end))
     (setq form-formatting (save-formatting form-string))
     (setq form (read form-string))
     ,@body
     (save-excursion
       (delete-region reg-beg reg-end)
       (insert "(")
       (dolist (el (apply-formatting form formatting))
         (insert el))
       (insert ")"))
     (goto-char reg-end)
     (exchange-point-and-mark)
     ;; (insert "(")
     ;; (dolist (el (apply-formatting form form-formatting))
     ;;   (insert el))
     ;; (insert ")")
     ;; (deactivate-mark)
     ;; (goto-char reg-beg)
     ;; (activate-mark)
     ;; (set-mark reg-end)
     ;; (redisplay t)
     ))

(if test
    (message "Test")
  (message "Best"))

(defun test ()
  (interactive)
  (let ((first nil)
        (second nil))
    (with-current-form
     (setq first (car (cdr form)))
     (setq second (cadr (cdr form)))
     (setf (car (cdr form)) second)
     (setf (cadr (cdr form)) first))))

(defun if-to-when ()
  (interactive)
  (let ((first nil)
        (second))
    (with-current-form
     (setf (car form) 'when)
     (setq form (delq (car (cdr (cdr (cdr form)))) form)))))

(defun when-to-if ()
  (interactive)
  (let ((first nil)
        (second))
    (with-current-form
     (setf (car form) 'if)
     (add-to-list 'form '("") t))))

(let* ((formatting (list))
       (match nil))
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (search-forward-regexp "\\([[:space:]\n\r]+\\)" nil t)
      (setq match (match-string 1))
      (when (not (null match))
        (setq formatting (append formatting `(,match))))))
  formatting)

(defun current-list-newline ()
  (interactive)
  (deactivate-mark)
  (forward-sexp)
  (newline-and-indent))

(defun current-list-indent ()
  (interactive)
  (indent-region (region-beginning) (region-end)))

;; (defun current-list-append-arg-newline ()
;;   (interactive)
;;   (with-current-form
;;    (add-to-list )))

(defun add-to-let-varlist ()
  (interactive)
  (with-current-form
   (setf (cdr (cdr (car (cdr form)))) '((test nil)))))

(defun let-to-let* ()
  (interactive)
  (with-current-form
   (setf (car form) 'let*)))

(defun let*-to-let ()
  (interactive)
  (with-current-form
   (setf (car form) 'let)))

(defun save-formatting (string)
  (interactive)
  (let ((formatting (list))
        (match nil))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (search-forward-regexp "\\([[:space:]\n\r]+\\)" nil t)
        (setq match (match-string 1))
        (when (not (null match))
          (setq formatting (append formatting `(,match))))))
    formatting))

(defun apply-formatting (form formatting)
  (interactive)
  (let ((result (list))
        (nthel 0))
    (dolist (el form)
      (setq result (append result (list (prin1-to-string el))))
      (when (nth nthel formatting)
        (setq result (append result (list (nth nthel formatting)))))
      (setq nthel (+ nthel 1)))
    result))

(defun exchange-args ()
  (interactive)
  (let ((reg-beg (region-beginning))
        (reg-end (region-end))
        (lst nil)
        (first nil)
        (second nil)
        (string nil)
        (formatting nil)
        (deactivate-mark nil))
    (setq string (buffer-substring-no-properties reg-beg reg-end))
    (setq formatting (save-formatting string))
    (setq lst (read string))
    (setq first (car (cdr lst)))
    (setq second (cadr (cdr lst)))
    (setf (car (cdr lst)) second)
    (setf (cadr (cdr lst)) first)
    (save-excursion
      (delete-region reg-beg reg-end)
      (insert "(")
      (dolist (el (apply-formatting lst formatting)) ;
        (insert el))
      (insert ")"))
    (goto-char reg-end)
    (exchange-point-and-mark)))
