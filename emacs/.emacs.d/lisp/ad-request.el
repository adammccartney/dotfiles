;; Very simple http request library
;; leverages curl, built in order to improve understanding of elisp
;; Much of the library is simply a copy of Chris Wellon's excellent elfeed.el

(defcustom ad-request-curl-program-name "curl"
  "Name/path by which to invoke curl program." 
  :type 'string)

(defcustom ad-request-use-curl
  (not (null (executable-find ad-request-curl-program-name)))
  "If non-nil, fetch feeds using curl instead of `url-retrieve`."
  :type 'boolean)

(defcustom rfc-doc-home "/home/mccartney/Documents/rfcmemos"
  "Name/path where rfc memos get stored"
  :type 'string)

(message ad-request-curl-program-name)
(message "%s" ad-request-use-curl)


(defun ad-request-rfc (rfcnum outdir)
  "Uses curl to make a web request to pull and rfc standard as text off the web
side effects are that this function will create a buffer with the "
  (let ((url (format "https://www.rfc-editor.org/rfc/rfc%d.txt" rfcnum))
        (outfile (format "%s/rfc%d.txt" outdir rfcnum)))
    (start-process "create-target" "*create-file*" "touch" outfile)
    (start-process "rfc-curl" "*rfc-curl*" "curl" "-o" outfile url)))


;; here are the docs that we want
(ad-request-rfc 7662 rfc-doc-home)

(ad-request-rfc 6750 rfc-doc-home)
