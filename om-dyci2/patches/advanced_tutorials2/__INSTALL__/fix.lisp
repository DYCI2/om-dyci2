(in-package :om)


;;; a utility function to get the executable path from a .app on Mac
(defun real-exec-pathname (path)
  (let* ((path (pathname path))
         (name (car (last (pathname-directory path)))))
    (if (and (om-directory-pathname-p path)
             (string-equal "app" (subseq name (- (length name) 3))))
        ;;; path is an application bundle
        (make-pathname :directory (append (pathname-directory path) (list "Contents" "MacOS"))
                       :name (subseq name 0 (- (length name) 4)))
      ;;; otherwise...
      path)))
