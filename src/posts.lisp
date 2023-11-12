(in-package :coleslaw)

(defclass post (content)
  ((title  :initarg :title  :reader title-of)
   (author :initarg :author :reader author-of)
   (excerpt :initarg :excerpt :reader excerpt-of)
   (format :initarg :format :reader post-format)
   (toot :initarg :toot :reader toot))
  (:default-initargs :author nil :excerpt nil :toot nil))

;; This function assumes that the input files follow the naming
;; scheme of Frog, i.e. yyyy-mm-dd-slug-derived-from-title.post.

(defun basename (post)
  (let* ((input-basename (pathname-name (content-file post)))
         (date-part (subseq input-basename 0 10))
         (title-part (subseq input-basename 11)))
    (format nil "~a/~a"
            (substitute #\/ #\- date-part)
            title-part)))

(defmethod initialize-instance :after ((object post) &key)
  (with-slots (url title author excerpt format text) object
    (let (post-content)
      (setf url (compute-url object (basename object))
            format (make-keyword (string-upcase format))
            post-content (render-text text format)
            excerpt (or excerpt
                        (first (split (excerpt-sep *config*)
                                      post-content
                                      :limit 2)))
            text post-content
            author (or author (author *config*))))))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
