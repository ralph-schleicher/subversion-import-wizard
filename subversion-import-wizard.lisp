;; subversion-import-wizard.lisp --- yet another Lisp hack.

;; Copyright (C) 2012, 2013 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :subversion-import-wizard
  (:use :common-lisp
	:iterate
	:cl-fad
	:rs-cll))

;;;; Standalone application.

(in-package :subversion-import-wizard)

(defconst +PROGRAM+ "subversion-import-wizard"
  "Official name of the program.")

(defconst +VERSION+ "1.0.1"
  "Version number of the program.")

(defconst +ADDRESS+ (let ((sys (asdf:find-system :subversion-import-wizard)))
		      (or (ignore-errors
			    (asdf:system-maintainer sys))
			  (ignore-errors
			    (asdf:system-author sys))
			  (format nil "<~A@~A>" "rs" "ralph-schleicher.de")))
  "Mail address or URL for reporting bugs.")

(defparameter *repository* nil
  "Repository location.")

(defparameter *working-copy* nil
  "Working copy directory name.")

(defparameter *dry-run* nil
  "Non-null means to not execute any command.")

(defparameter *window-system* nil
  "Non-null means to use the host's window system.")

(defparameter *show-version* nil
  "Non-null means to print the version number.")

(defparameter *show-help* nil
  "Non-null means to print the help text.")

(defun show-version (&optional (stream *standard-output*))
  "Display version number information."
  (format stream "~
~A ~A

Copyright (C) 2012, 2013 Ralph Schleicher

This program is free software and distributed under the modified
BSD License.  There is NO warranty; not even for MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.~%"
	  +PROGRAM+ +VERSION+))

(defun show-help (&optional (stream *standard-output*))
  "Display help text."
  (format stream "~
Usage: ~A [OPTION...] [FILENAME...]

Import a project history from the file system into a Subversion
repository.

Optional FILENAME arguments are source files containing instructions
to be performed by the program.

Options:
  --repository=LOCATION
                    Location of the Subversion repository.  Default is
                    to create a local repository.
  --working-copy=DIRNAME
                    Directory name of the working copy.  Default is to
                    create a temporary directory.
  -n, --dry-run     Do not execute any command.
  -w, --window-system
                    Interact with the user via a graphical user interface.
                    This option only has an effect if a window system is
                    present.  This feature is not fully implemented.
  --version         Display version number information.
  --help            Display this help text.

Report bugs to ~A.~%"
	  (program-invocation-short-name) +ADDRESS+))

(export 'main)
(defun main (&rest arguments)
  "Program entry point."
  (declare (ignore arguments))
  (standalone-program)
  (let (file-names)
    ;; Initial window system support is determined heuristically.
    #+windows
    (unless (or (environment-variable "PROMPT") (environment-variable "PS1"))
      (setf *window-system* t))
    ;; Get options and arguments.
    (let ((opt (make-getopt '(("repository"
			       :argument :required
			       :action *repository*)
			      ("working-copy"
			       :argument :required
			       :action *working-copy*)
			      ("dry-run" #\n
			       :action *dry-run*)
			      ("window-system" #\w
			       :action *window-system*)
			      ("version"
			       :action *show-version*)
			      ("help"
			       :action *show-help*))
			    :help "--help")))
      (when (getopt opt)
	(show-help-hint-and-die opt))
      ;; Check for help.
      (when (or *show-version* *show-help*)
	(fresh-line)
	(when *show-version*
	  (show-version))
	(when *show-help*
	  (when *show-version*
	    (terpri) (terpri))
	  (show-help))
	(exit-success))
      ;; Save remaining arguments.
      (setf file-names (remaining-arguments opt)))
    (when (and *window-system* (null file-names))
      ;; TODO: Print version number information, then open file
      ;; selection dialogs to set file-names, *repository*, and
      ;; *working-copy*.
      (fresh-line)
      (show-version) (terpri) (terpri)
      (show-help) (terpri) (terpri)
      (princ "Press any key to continue... ")
      (finish-output)
      (read-char *standard-input* nil)
      (exit-success))
    ;; Run the actual program.
    (subversion-import-wizard file-names)
    (exit-success)))

;;;; Implementation.

(defvar here nil
  "The process's working directory.")

(defvar repo-path nil
  "Repository directory (a pathname).")

(defvar repo-dir nil
  "Repository directory as a file name (a string).")

(defvar repo-url nil
  "Repository URL of the root directory.
This includes the terminating slash character.")

(defvar work-path nil
  "Working copy directory (a pathname).")

(defvar work-dir nil
  "Working copy directory as a file name (a string).")

(define-symbol-macro path-sep
  #+unix
  #\/
  #+windows
  #\\
  #-(or unix windows)
  (error "Fix me."))

(defun ^ (&rest arg)
  "Build repository URL."
  (let (*print-pretty*)
    (format nil "~A~{~A~^/~}" repo-url arg)))

(defun ~ (&rest arg)
  "Concatenate file name parts."
  (let (*print-pretty*)
    (format nil #.(concatenate 'string "~{~A~^" (string path-sep) "~}") arg)))

(defun temp-dir (prefix)
  "Create a temporary directory."
  (temporary-directory :prefix prefix :directory here))

(defun chat (level templ &rest arg)
  (let ((stars (make-string (1+ level) :initial-element #\*)))
    (format *standard-output* "~A ~?~&" stars templ arg)))

(defun run (program &rest arguments)
  (execute-program program arguments :input nil :output nil))

(defmacro svn (command &rest arg)
  `(zerop (run "svn" ,command "--non-interactive" ,@arg)))

(defun svn-info (path)
  "Return either :directory, :file, or nil."
  (let (info)
    (with-input-from-program
	(stream "svn" (list "info" "--non-interactive" path))
      (iter (for line = (chomp (read-line stream nil)))
	    (when (null line)
	      (finish))
	    (when (string-match "\\A(?i:Node Kind):\\s*(\\w+)" line)
	      (setf info (intern (string-upcase (match-string 1)) :keyword))
	      (finish))))
    info))

(defun chomp (line)
  (let ((n (length line)))
    (when (and (> n 0) (char= (aref line (1- n)) #\Linefeed))
      (setf line (delete-if #'true line :from-end t :count 1))
      (setf n (length line)))
    (when (and (> n 0) (char= (aref line (1- n)) #\Return))
      (setf line (delete-if #'true line :from-end t :count 1))
      (setf n (length line)))
    line))

(defun ensure-repository-exists ()
  (chat 0 "Preparing repository")
  (setf repo-path (pathname-as-directory (or *repository* (temp-dir "repo-"))))
  (unless (directory-exists-p repo-path)
    (die "~A: No such directory" (namestring repo-path)))
  (setf repo-dir (namestring (truename repo-path)))
  #+windows
  (nsubstitute path-sep #\/ repo-dir)
  (let ((n (length repo-dir)))
    (when (and (> n 0) (char= (aref repo-dir (1- n)) path-sep))
      (setf repo-dir (remove path-sep repo-dir :from-end t :count 1))))
  (chat 1 "File system location is '~A'" repo-dir)
  #+unix
  (setf repo-url (concatenate 'string "file://" repo-dir "/"))
  #+windows
  (setf repo-url (concatenate 'string
			      (cond ((string-match "\\A(?:\\\\\\\\|\\/\\/)" repo-dir)
				     "file:")
				    ((string-match "\\A[A-Za-z]:" repo-dir)
				     "file:///")
				    (t
				     "file://"))
			      (substitute #\/ #\\ repo-dir)
			      "/"))
  #-(or unix windows)
  (error "Fix me.")
  (chat 1 "URL of root directory is '~A'" repo-url)
  (unless (file-exists-p (~ repo-dir "db" "fs-type"))
    (chat 0 "Creating repository")
    (unless (zerop (run "svnadmin" "create" "--fs-type" "fsfs" repo-dir))
      (die "~A: can not create repository" repo-dir))
    (chat 1 "Importing initial file structure")
    (with-temporary-directory (temp-dir)
      (iter (for sub-dir :in '("trunk" "branches" "tags"))
	    (ensure-directories-exist (merge-pathnames
				       (make-pathname :directory (list :relative sub-dir))
				       temp-dir)))
      (unless (svn "import" "-q" "-m" "Create initial file structure." (namestring temp-dir) repo-url)
	(die "~A: can not create initial file structure" repo-dir)))))

(defun ensure-working-copy-exists ()
  (chat 0 "Preparing working copy")
  (setf work-path (pathname-as-directory (or *working-copy* (temp-dir "work-"))))
  (unless (directory-exists-p work-path)
    (die "~A: No such directory" (namestring work-path)))
  (setf work-dir (namestring (truename work-path)))
  #+windows
  (nsubstitute path-sep #\/ work-dir)
  (let ((n (length work-dir)))
    (when (and (> n 0) (char= (aref work-dir (1- n)) path-sep))
      (setf work-dir (remove path-sep work-dir :from-end t :count 1))))
  (chat 1 "File system location is '~A'" work-dir))

(defmacro with-package (name &body body)
  "Switch to package NAME and evaluate BODY."
  `(let ((*package* *package*))
     (in-package ,name)
     ,@body))

(defun command-loop (stream file-name)
  (declare (ignore file-name))
  (let ((*read-default-float-format* 'double-float))
    (with-package :subversion-import-wizard-user
      (iter (for expr = (read stream nil))
	    (while expr)
	    (eval expr)))))

(defun evaluate-commands (file-name)
  (if (string= file-name "-")
      (progn
	(chat 0 "Reading commands from standard input")
	(command-loop *standard-input* "(standard input)"))
    (progn
      (chat 0 "Reading commands from '~A'" file-name)
      (with-open-file (stream file-name)
	(command-loop stream file-name)))))

(defun subversion-import-wizard (file-names)
  (setf here (get-working-directory))
  (unless *dry-run*
    (ensure-repository-exists)
    (ensure-working-copy-exists))
  (if (not file-names)
      (evaluate-commands "-")
    (iter (for file-name :in file-names)
	  (evaluate-commands file-name))))

;;;; User commands.

(defun %message (message)
  (let* ((m (or message ""))
	 (n (length m)))
    (if (or (zerop n) (member (aref m (1- n)) '(#\. #\? #\!)))
	m
      (concatenate 'string m "."))))

(defun %ensure-directory (dir &optional message)
  (if (not (eq (svn-info dir) :directory))
      (svn "mkdir" "-q" "--parents" "-m" (%message (or message "Create directory")) dir)
    t))

(defun %mkdir (dir &key message)
  #+windows
  (progn
    (setf dir (substitute #\/ #\\ dir)))
  (chat 0 "MKDIR ~S" (concatenate 'string "^/" dir))
  (when (not *dry-run*)
    (unless (%ensure-directory (^ dir) message)
      (die "Can not create directory")))
  (values))

(defun %copy (from to &key message)
  #+windows
  (progn
    (setf from (substitute #\/ #\\ from))
    (setf to (substitute #\/ #\\ to)))
  (chat 0 "COPY ~S ~S" (concatenate 'string "^/" from) (concatenate 'string "^/" to))
  (when (not *dry-run*)
    ;; Optionally create parent directories.
    (let ((end (position #\/ to :from-end t)))
      (when end
	(let ((parent (subseq to 0 end)))
	  ;; If the 'mkdir' command fails, it usually means that
	  ;; the file already exists.  If this was not the reason
	  ;; for failure, the 'copy' command will fail because the
	  ;; parent directory does not exist.
	  (%ensure-directory (^ parent)))))
    (unless (svn "copy" "-q" "-m" (%message (or message "Create copy")) (^ from) (^ to))
      (die "Can not create copy")))
  (values))

(defun %import (from to &key ignore message)
  #+windows
  (progn
    (setf from (substitute #\\ #\/ from))
    (setf to (substitute #\/ #\\ to)))
  (chat 0 "IMPORT ~S ~S" from (concatenate 'string "^/" to))
  (unless (directory-exists-p from)
    (die "No such directory '~A'" from))
  (when (not *dry-run*)
    ;; Optionally create destination directory.
    (%ensure-directory (^ to))
    ;; Switch working copy to destination.
    (if (directory-exists-p (merge-pathnames
			     (make-pathname :directory '(:relative ".svn"))
			     work-path))
	(progn
	  (chat 1 "Switching working copy to '^/~A'" to)
	  (unless (svn "switch" "-q" (^ to) work-dir)
	    (die "Can not switch working copy")))
      (progn
	(chat 1 "Checking out '^/~A'" to)
	(unless (svn "checkout" "-q" (^ to) work-dir)
	  (die "Can not check-out working copy"))))
    ;; Remove all files except '.svn' directories.
    (chat 1 "Cleaning working copy")
    (let ((from-dir (make-hash-table :test #'equal))
	  (to-dir (make-hash-table :test #'equal)))
      ;; Gather hierarchy of the directory to be imported.
      (walk-directory from
		      #'true
		      :directories :breadth-first
		      :test (lambda (file)
			      (when (directory-pathname-p file)
				(let ((key (subseq (namestring file) (length from))))
				  #+windows
				  (nstring-downcase key)
				  (setf (gethash key from-dir) file)))))
      ;; Likewise for the working copy directory.
      ;; Also remove all regular files.
      (walk-directory work-dir
		      (lambda (file)
			(unless (directory-pathname-p file)
			  #+windows
			  (run "attrib" "-R" (substitute #\\ #\/ (namestring file)))
			  (delete-file file)))
		      :directories :breadth-first
		      :test (lambda (file)
			      (if (directory-pathname-p file)
				  ;; Filter out '.svn' directories.
				  (unless (string= ".svn" (first (last (pathname-directory file))))
				    (let ((key (subseq (namestring file) (length work-dir))))
				      #+windows
				      (nstring-downcase key)
				      (setf (gethash key to-dir) file)))
				t)))
      ;; Remove directories in the working copy not existing in the
      ;; directory hierarchy to be imported.
      (iter (for (key dir) :in-hashtable to-dir)
	    (unless (gethash key from-dir)
	      #+windows
	      (run "attrib" "-R" (~ (substitute #\\ #\/ (namestring dir)) "*") "/S" "/D")
	      (delete-directory-and-files dir :if-does-not-exist :ignore))))
    ;; Copy directory hierarchy.
    (chat 1 "Copying files from '~A'" from)
    #+unix
    (progn
      (set-working-directory from)
      (unless (zerop (run "cp" "-a" "." work-dir))
	(die "Can not copy project files"))
      (set-working-directory here))
    #+windows
    (unless (zerop (run "xcopy" from work-dir "/E" "/Q" "/Y"))
      (die "Can not copy project files"))
    ;; Process changes.
    (chat 1 "Entering directory '~A'" work-dir)
    (set-working-directory work-path)
    (let ((modified (make-hash-table :test #'equal))
	  (added (make-hash-table :test #'equal))
	  (removed (make-hash-table :test #'equal)))
      (with-input-from-program
	  (stream "svn" `("status"
			  "--non-interactive"
			  ,@(unless ignore '("--no-ignore")))
		  :error t)
	(iter (for line = (chomp (read-line stream nil)))
	      (when (null line)
		(finish))
	      (for item = (subseq line 8))
	      #+unix
	      (for key = item)
	      #+windows
	      (for key = (string-downcase item))
	      #-(or unix windows)
	      (error "Fix me.")
	      (case (aref line 0)
		(#\M (setf (gethash key modified) item))
		(#\? (setf (gethash key added) item))
		(#\! (setf (gethash key removed) item)))))
      (when (or (> (hash-table-count modified) 0)
		(> (hash-table-count added) 0)
		(> (hash-table-count removed) 0))
	;; Process removed items.
	(iter (for (key old-name) :in-hashtable removed)
	      (for new-name = (gethash key added))
	      (if (null new-name)
		  (progn
		    (chat 2 "Deleting '~A'" old-name)
		    (unless (svn "delete" "-q" old-name)
		      (die "~A: can not delete file" old-name)))
		(progn
		  (chat 2 "Renaming '~A' to '~A'" old-name new-name)
		  (multiple-value-bind (unused orig temp)
		      (rename-file new-name (temporary-file-name))
		    ;; Resurrect old file and delete it.
		    (unless (svn "update" "-q" old-name)
		      (die "~A: can not restore file" old-name))
		    (unless (svn "delete" "-q" old-name)
		      (die "~A: can not delete file" old-name))
		    (unless (svn "commit" "-q" "-m" "Delete file for renaming." old-name)
		      (die "~A: can not commit changes" old-name))
		    ;; Install new file.
		    (rename-file temp orig)))))
	;; Process added items.
	(iter (for (key new-name) :in-hashtable added)
	      (chat 2 "Adding '~A'" new-name)
	      (unless (svn "add" "-q" new-name)
		(die "~A: can not add file" new-name)))
	;; Done.
	(unless (svn "commit" "-q" "-m" (%message (or message (format nil "Import '~A'" from))) ".")
	  (die "Can not commit changes"))))
    (chat 1 "Leaving directory '~A'" work-dir)
    (set-working-directory here))
  (values))

;; subversion-import-wizard.lisp ends here
