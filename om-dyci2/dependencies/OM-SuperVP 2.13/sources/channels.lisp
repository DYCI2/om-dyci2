;============================================================================
; OM-SuperVP
; SuperVP sound analysis and processing for OpenMusic
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; PLIT/MERGE MULTICHANNEL FILES
; File author: J. Bresson Marco Stroppa 
;============================================================================


(in-package :svp)


;;; utils
(defmethod sound-n-channels ((self om::sound))
  (om::om-sound-n-channels self))

(defmethod sound-n-channels ((self pathname))
  (let ((thesound (om::get-sound self))) 
    (om::om-sound-n-channels thesound)))

(defmethod sound-n-channels ((self string))
  (sound-n-channels (pathname self)))


;;; seems to work only if files to merge have same or less channels than the first file
(defmethod! om::supervp-merge (files &optional outpath)
            :initvals '(nil)
            :indoc '("a list of sounds (or sound file pathnames) or a directory pathname" 
                     "output pathname")
            :icon 950
            :doc "
Merges all files in <files> into a single multi-channel audio file.

If <outname> is not specified, the merged file will be created in the in <files> if <files> is a directory, or in the OUT-FILES folder (see OM preferences) if not.
"
            (let ((file-list 
                   (mapcar 'namestring 
                           (cond ((listp files) 
                                  (remove nil (loop for item in files 
                                                    collect 
                                                    (if (subtypep (type-of item) 'om::sound) 
                                                        (or (om::om-sound-file-name item)
                                                            (om::om-beep-msg "This sound has no attached file on disk!"))
                                                      item)))) 
                                 ((and (pathnamep files) (om::om-directory-pathname-p files))
                                  (om::om-directory files :directories nil))
                                 (t nil))))
                  (outname (namestring 
                            (cond ((pathnamep outpath) outpath)
                                  ((and (pathnamep files) (om::om-directory-pathname-p files) (stringp outpath))
                                   (om::om-make-pathname :directory files :name outpath))
                                  ((and (pathnamep files) (om::om-directory-pathname-p files))
                                   (om::om-make-pathname :directory files :name "merge-file" :type "aiff"))
                                  (t (om::outfile "merge-file.aiff")))))
                  (param-file (om::svp-paramfile "tmp-merge.par")))
           
              (om::add-tmp-file param-file)
        
              (with-open-file (out param-file :direction :output 
                                   :if-does-not-exist :create :if-exists :supersede)
                
                (let ((i (1+ (sound-n-channels (car file-list))))) ;;; chan-number of first mixed channel
                  (loop for sndf in (cdr file-list) do
                        (format out "~s~%" sndf)
                        (dotimes (src-ch (sound-n-channels sndf))
                          (format out "[~D/0,~D]~%" i (1+ src-ch))
                          (setf i (+ i 1)))
                        ))
                )
          
              (om::supervp-command (format nil "-S~s -mixlist ~s ~s" (car file-list) (namestring param-file) outname))

              ;(om::maybe-clean-tmp-files)
        
              (or (probe-file outname) (om::om-beep-msg "SuperVP merging error"))))


(defmethod! om::supervp-mix (files &optional outpath)
  :initvals '(nil)
  :indoc '("a list of sounds (or sound file pathnames) or a directory pathname" "output pathname")
  :icon 950
  :doc "
Merges all files in <files> into a single mono file.

If <outname> is not specified, the merged file will be created in the in <files> if <files> is a directory, or in the OUT-FILES folder (see OM preferences) if not.
"
  (let ((source (om::save-sound (sound-silence 1000 8) (om::tmpfile "silence.aiff")))
        (file-list 
         (mapcar 'namestring 
                 (cond ((subtypep (type-of files) 'om::sound) 
                        (if (om::om-sound-file-name files) (list (om::om-sound-file-name files))
                          (om::om-beep-msg "This sound has no attached file on disk!")))
                       ((listp files) 
                        (remove nil (loop for item in files collect 
                                          (if (subtypep (type-of item) 'om::sound) 
                                              (or (om::om-sound-file-name item)
                                                  (om::om-beep-msg "This sound has no attached file on disk!"))
                                            item))))
                       ((and (pathnamep files) (om::om-directory-pathname-p files))
                        (om::om-directory files :directories nil))
                       (t nil))))
        (outname (namestring 
                  (cond ((pathnamep outpath) outpath)
                        ((and (pathnamep files) (om::om-directory-pathname-p files) (stringp outpath))
                         (om::om-make-pathname :directory files :name outpath))
                        ((and (pathnamep files) (om::om-directory-pathname-p files))
                         (om::om-make-pathname :directory files :name "mix-file" :type "aiff"))
                        (t (om::outfile "mix-file.aiff")))))
        (param-file (om::svp-paramfile "tmp-mix.par")))
      
    (om::add-tmp-file source)
    (om::add-tmp-file param-file)
    (with-open-file (out param-file :direction :output 
                         :if-does-not-exist :create :if-exists :supersede)
          ;(dotimes (src-ch (om::n-channels (om::get-sound (car file-list))))
          ;  (format out "[1/0,~D]~%" (1+ src-ch)))
      (loop for sndf in file-list do
            (let ((scrchans (sound-n-channels sndf)))
              (format out "~s~%" sndf)
              (dotimes (src-ch scrchans)
                (format out "[1/0,~D,.5]~%" (1+ src-ch))
                )))
      )
          
    (om::supervp-command (format nil "-S~s -mixlist ~s ~s" (namestring source) (namestring param-file) outname))

    (let ((monofile (om::supervp-split outname 1)))
      (delete-file outname)
      (rename-file monofile outname))

    (om::maybe-clean-tmp-files)
        
    (or (probe-file outname) (om::om-beep-msg "SuperVP mixing error"))))




;;;======================================================================================


(defmethod supervp-extract-one-channel (file n outfile)
  (om::supervp-command (format nil "-S~s -C~D ~s" (namestring file) n (namestring outfile)))
  (probe-file outfile))

(defmethod supervp-extract-one-channel ((file om::sound) n outfile)
  (if (om::om-sound-file-name file)
      (supervp-extract-one-channel (om::om-sound-file-name file) n outfile)
    (om::om-beep-msg "SUPERVP-SPLIT: input sound file must be saved on disk!")
    ))


(defmethod! om::supervp-split (file &optional channels outfolder)
            :initvals '(nil nil nil)
            :indoc '("a sound (or sound file pathname)" "channels to extract" "output directory pathname")
            :icon 950
            :doc "
Splits <file> into several mono sound files stored in <outfolder>.

<channels> can be an integer number or a list of numbers specifying specific channels to extract. If unspecific, all channels in <file> are extracted.
Channel files are named after the original <file> name cancatenated with the channel number (e.g. myfile-1.aiff, myfile-2.aiff, etc.)

If <outfolder> is not specified, the extracted channel files are stored in a sub-folder named after <file>.

"
      (let* ((nc (sound-n-channels file))
             (chans (if (null channels) (om::arithm-ser 1 nc 1)
                      (remove nil (loop for chan in (om::list! channels) collect 
                                        (if (and (integerp chan) (<= chan nc)) chan 
                                          (om::om-beep-msg (format nil "There is no channel ~D in ~s !" chan file)))))))
             (outfolder (or outfolder (om::om-make-pathname :directory (append (pathname-directory file) (list (pathname-name file)))))))
        
        (unless (probe-file outfolder)
          (om::om-create-directory outfolder))
        
        (if (and (integerp channels) (car chans))
            (supervp-extract-one-channel file (car chans)
                                           (om::om-make-pathname :directory (pathname-directory outfolder) 
                                                             :name (format nil "~a-~D" (pathname-name file) (car chans))
                                                             :type (pathname-type file)))
        (loop for channel in chans collect
              (supervp-extract-one-channel file channel
                                           (om::om-make-pathname :directory (pathname-directory outfolder) 
                                                             :name (format nil "~a-~D" (pathname-name file) channel)
                                                             :type (pathname-type file)))
              )
        )))


(defmethod! om::supervp-split ((file om::sound) &optional channels outfolder)
     (if (om::om-sound-file-name file)
         (om::supervp-split (om::om-sound-file-name file) channels outfolder)     
       (om::om-beep-msg "SUPERVP-SPLIT: input sound file must be saved on disk!")))
        
        