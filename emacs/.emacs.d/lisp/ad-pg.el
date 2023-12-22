;; This is a tiny interface library to the pg-el mode

(defun ad/pg-dbquery (con query)
  ;; Open the database dbname and execute a single or multiple queries
  ;; Returns an integer value after completing the query
    (pcase query
      ((pred stringp)
       (pg-exec con query))
      ((pred listp)
       (dolist (q query)
         (pg-exec con q)))
      (_ (error "Invalid query format"))))

(provide 'ad-pg)
