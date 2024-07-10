(require 'cl-lib)

;;;###autoload
(defmacro condp (pred expr &rest clauses)
  "clojure's condp (kinda), by Vanya."
  (declare (indent 2))
  (let ((cond-expr (make-symbol "condp--expr"))
        cond-clauses)
    (while (cdr clauses)
      (push `((,pred ,(pop clauses) ,cond-expr)
              ,(pop clauses)) cond-clauses))
    (when clauses
      (push `(t ,(pop clauses))
            cond-clauses))
    `(let ((,cond-expr ,expr))
       (cond ,@(nreverse cond-clauses)))))

;; Old version, before Vanya showed me a way.
;; (defmacro condp (pred expr &rest clauses)
;;   "clojure's condp (kinda)"
;;   (let ((is-odd (cl-oddp (length clauses))))
;;     `(let ((condp-expr ,expr))
;;        (cond
;;         ,@(mapcar (pcase-lambda (`(,p1 ,p2))
;;                     `((,pred ,p1 condp-expr) ,p2))
;;                   (seq-partition (if is-odd (butlast clauses) clauses) 2))
;;         ,(if is-odd `(t ,@(last clauses)) '(t nil))))))

;; (condp = 3
;;        1 1
;;        3 9
;;        5 25)
;; Instead of this btw, I think (cl-case is better. Just because it's built-in.)

;; (cl-case 10
;;   (1 1)
;;   (3 9)
;;   (5 25)
;;   (otherwise (message "I only know the power for 1, 3, 5. Silly me.")))

;;;###autoload
(defun comp (&rest fns)
  "copied from dash.el basically"
  (let* ((fns (nreverse fns))
         (head (car fns))
         (tail (cdr fns)))
    (cond
     (tail (lambda (&rest args)
             (seq-reduce (lambda (acc fn) (funcall fn acc)) tail (apply head args))))
     (fns head)
     (t (lambda (&optional arg &rest _) arg)))))

;;;###autoload
(defun q/map-with-index (fn seq)
  (cl-mapcar fn seq (number-sequence 0 (1- (length seq)))))

;;;###autoload
(defun q/get-page-as-json (url)
  (let ((url-request-method "GET")
        (header nil))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "{")
      (forward-char -2)
      (json-parse-buffer :object-type 'plist))))

;; (apply (comp #'message #'number-to-string #'+) '(1 2 3 4))

;; (defun partial-id (fn)
;;   (apply-partially fn #'identity))

;; (message (condp funcall (list t nil)
;;       (partial-id #'cl-every) "ALL IS TRUE"
;;       (partial-id #'cl-some) "AT LEAST ONE IS TRUE"
;;       "NONE IS TRUE"))
;; This one is cool.

(provide 'q)
