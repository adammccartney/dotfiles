(defun adsqlite/create-db (dbname schema)
  ;; create a new sqlite database with schema
  ;; returns an rc code
  (let ((db (sqlite-open dbname)))
    (sqlite-execute db schema)))

(defun adsqlite/db-query (dbname query)
  ;; Open the database dbname and execute a single or multiple queries
  ;; Returns an integer value after completing the query
  (let ((db (sqlite-open dbname)))
    (pcase query
      ((pred stringp)
       (sqlite-execute db query))
      ((pred listp)
       (dolist (q query)
         (sqlite-execute db q)))
      (_ (error "Invalid query format"))))
  )

  
(provide 'ad-sqlite)
