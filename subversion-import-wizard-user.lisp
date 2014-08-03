;; subversion-import-wizard-user.lisp --- user commands.

;; Copyright (C) 2012 Ralph Schleicher

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
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :subversion-import-wizard-user
  (:use :common-lisp)
  (:shadow #:import)
  (:import-from :subversion-import-wizard
		#:%mkdir #:%copy #:%import
		#:~))

(in-package :subversion-import-wizard-user)

(defun ^ (&rest arg)
  "Concatenate parts of a repository path."
  (let (*print-pretty*)
    (format nil "~{~A~^/~}" arg)))

(defvar trunk "trunk"
  "The repository path of the trunk directory.")

(defvar branches "branches"
  "The repository path of the branches directory.")

(defvar tags "tags"
  "The repository path of the tags directory.")

(defun mkdir (dir &key message)
  "Create a directory in the repository, if it does not exist.
Intermediate directories are made as needed.

First argument DIR is the repository path of the directory.
Keyword argument MESSAGE is the commit message."
  (%mkdir dir :message message))

(defun copy (from to &key message)
  "Duplicate a directory in the repository, remembering history.

First argument FROM is the repository path of the source directory.
Second argument TO is the repository path of the destination directory.
Keyword argument MESSAGE is the commit message.

The source directory has to exist.  Parent directories of the
destination directory are made as needed.

The `copy' command is used to create branches and tags."
  (%copy from to :message message))

(defun branch (from to &key message)
  "Create a branch.

The `branch' command has the same effect as the somewhat longer

     (copy FROM (^ branches TO) :message MESSAGE)

command with a default commit message of 'Create branch'."
  (%copy from (^ branches to) :message (or message "Create branch")))

(defun tag (from to &key message)
  "Create a tag.

The `tag' command has the same effect as the somewhat longer

     (copy FROM (^ tags TO) :message MESSAGE)

command with a default commit message of 'Create tag'."
  (%copy from (^ tags to) :message (or message "Create tag")))

(defun import (from to &key ignore message)
  "Copy an unversioned directory from the file system into the repository.

First argument FROM is the file system path of the source directory.
Second argument TO is the repository path of the destination directory.
If keyword argument IGNORE is true, omit ignored files and directories.
 Default is to import all files and directories.
Keyword argument MESSAGE is the commit message.

The source directory has to exist.  If the destination directory does
not exist, it is created.  If the destination directory is not empty,
files and directories are added, deleted, or updated as needed to
match the contents of the source directory."
  (%import from to :ignore ignore :message message))

;; subversion-import-wizard-user.lisp ends here
