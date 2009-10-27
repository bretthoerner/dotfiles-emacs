;;; project.el --- Keep track of the current project
 
;; Copyright (C) 2008 Nathan Weizenbaum
 
;; Author: Nathan Weizenbaum
;; URL: http://github.com/nex3/in-project-el
;; Version: 1.0
;; Created: 2008-01-06
;; Keywords: project, convenience
;; EmacsWiki: FindFileInProject
 
;; This file is NOT part of GNU Emacs.
 
;;; License:
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
 
;;; Commentary:

;; This library provides a simple, standard, extensible way of keeping
;; track of the project in which a user is working.

(eval-when-compile (require 'cl))

(defvar project-root-functions
  '(project-toplevel-file-p project-everylevel-file-p)
  "A list of functions that take the name of a directory and return t
if that directory is the root of a project and nil otherwise.

Each function is run on a single directory node before a higher node
is tried.")

(defvar project-toplevel-files
  '(".emacs-project" ".dir-settings.el" ".emacs-dirvars" ".git" ".hg"
    ".bzr" "_darcs")
  "A list of files or directories that signal the top level
of a project.")

(defvar project-everylevel-files '(".svn" "CVS" "RCS")
  "A list of files or directories that appear at every level
of a project.")

(defun* project-root (&optional buffer)
  "Return the root of the project for BUFFER, determined using
`project-root-functions', or nil if BUFFER isn't in a project.

No argument means use the current buffer."
  (do ((dir (file-truename (buffer-file-name buffer))
            (directory-file-name (file-name-directory dir))))
      ((string-equal dir "/"))
    (dolist (fn project-root-functions)
      (when (funcall fn dir) (return-from project-root dir)))))

(defun project-toplevel-file-p (dir)
  "Return t if dir contains a toplevel file as defined
by `project-toplevel-files', and nil otherwise."
  (dolist (file project-toplevel-files)
    (when (file-exists-p (concat dir "/" file)) (return t))))

(defun project-everylevel-file-p (dir)
  "Return t if dir is the toplevel dir as defined
by `project-everylevel-files', and nil otherwise."
  (dolist (file project-everylevel-files)
    (when (and (file-exists-p (concat dir "/" file))
               (not (file-exists-p
                     (concat
                      (directory-file-name
                       (file-name-directory dir))
                      "/" file))))
      (return t))))

(provide 'project)
