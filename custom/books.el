(require 'cl-lib)
(require 'q)

(defun q/google-books-api-request (query)
  (let ((url (concat "https://www.googleapis.com/books/v1/volumes?q=" (url-hexify-string query))))
    (q/get-page-as-json url)))

(defun q/book-get-author (book-info)
  (string-join (plist-get book-info :authors) ", "))

(defun q/book-for-autocomplete (book)
  (let ((book-info (plist-get book :volumeInfo)))
    (format "%s - %s" (q/book-get-author book-info) (plist-get book-info :title))))

(defun q/ask-for-a-right-book-index (results)
  (let ((collection
         (q/map-with-index (lambda (book i) (cons (q/book-for-autocomplete book) i)) results)))
    (alist-get (completing-read "Книга: " collection nil t) collection nil nil #'string=)))

;;;###autoload
(defun q/request-book-info (type content)
  (when-let* ((query (condp string= type
                            "title" (format "intitle:%s" content)
                            "isbn" (format "isbn:%s" content)
                            content))
              (results (q/google-books-api-request query))
              (totalResults (plist-get results :totalItems))
              (bookIndex (cond
                     ((= totalResults 1) 0)
                     ((> totalResults 1) (q/ask-for-a-right-book-index (plist-get results :items)))))
              (book (plist-get (aref (plist-get results :items) bookIndex) :volumeInfo)))
              book))

(defvar q/book-org-file (concat (getenv "SYNCTHING") "org/books.org"))

(defun q/get-existing-property-value (property)
  (org-with-file-buffer q/book-org-file
    (org-property-values property)))

;;;###autoload
(defun q/org-capture-book-template ()
  (let ((book-template "* %s\n:PROPERTIES:\n:GOOGLEID: %s\n:AUTHOR: %s\n:PAGES: %s\n:PUB_DATE: %s\n:ISBNS: %s\n:POSITION: %%^{Где лежит?}\n:END:\n")
        (type (completing-read "Искать по: " (list "isbn" "title" "вручную") nil t)))
    (if (string= type "вручную")
        (format
         book-template
         "%^{Название}"
         "N/A"
         (completing-read "Автор: " (q/get-existing-property-value "AUTHOR"))
         "%^{Количество страниц}"
         "%^{Дата публикации}"
         "%^{Идентификаторы}")
      (let* ((content (read-string "ISBN/название: "))
             (book-info (q/request-book-info type content)))
        (format
         book-template
         (plist-get book-info :title)
         (q/book-get-author book-info)
         (plist-get book-info :id)
         (plist-get book-info :pageCount)
         (plist-get book-info :publishedDate)
         (mapconcat (lambda (x) (format "%s = %s" (plist-get x :type) (plist-get x :identifier))) (plist-get book-info :industryIdentifiers) ", "))))))

(provide 'books)
