;;; consult-project.el --- Consult integration for porject  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  CHEN Xianan
;; URL: https://github.com/cxa/consult-project
;; Keywords: consult, project
;; Version: 0.1
;; Package-Requires: (project consult recentf subr-x dash)

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A multi-view for opened buffers and files within project.
;;
;; Just run the `consult-project' and/or bind it to a hotkey.
;;
;; To filter the multiview use:
;; B - For buffers inside `project-current'
;; F - For files inside `project-current'
;; R - For recent opened projects
;; P - For all known projects

;;; Code:

(require 'project)
(require 'consult)
(require 'recentf)
(require 'subr-x)
(require 'dash)

(defcustom consult-project-sources
  '(consult-project--source-buffer
    consult-project--source-file
    consult-project--source-project-recentf
    consult-project--source-project-all)
  "`consult-project' sources for `consult--multi'."
  :type '(repeat symbol)
  :group 'consult-project)

(defcustom consult-project-recentf-max-projects 10
  "Max recentf projects."
  :type 'integer
  :group 'consult-project)

(defvar consult-project--history nil)

(defun consult-project--list-files (root &optional files)
  "List files for project ROOT, or FILES if provided."
  ;; `preject' expands file names, so we must expand `root' to match up
  (setq root (expand-file-name root))
  (unless files
    (setq files (project-files (project--find-in-directory root))))
  (mapcar
   (lambda (f) (propertize
                (string-remove-prefix root f)
                'consult--project-root root))
   files))

(defun consult-project--file-action (root file)
  "Open FILE under ROOT."
    (consult--file-action (concat root file)))

(defun consult-project--files (selected-dir)
  "Create a view for project in SELECTED-DIR."
  (consult-project--file-action
   selected-dir
   (consult--read (consult-project--list-files selected-dir)
                  :prompt "Find project file: "
                  :sort t
                  :require-match t
                  :category 'file
                  :state (consult--file-preview)
                  :history 'file-name-history)))

(defun consult-project--recentf-projects ()
  "Return list of recentf projects."
  (let* ((get-proj (-compose #'project--find-in-directory #'file-name-directory))
         (projs (remq nil (mapcar get-proj recentf-list)))
         (dirs (delete-dups (mapcar (-compose #'abbreviate-file-name  #'project-root) projs))))
    (-take consult-project-recentf-max-projects dirs)))

(defvar consult-project--source-buffer
  `(
    :name     "Project buffer"
    :narrow   ?b
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :enabled  ,(lambda () (project-current))
    :items
    ,(lambda ()
       (when-let (root-dir (expand-file-name (project-root (project-current))))
         (mapcar
          #'buffer-name
          (seq-filter (lambda (x)
                        (when-let (file (buffer-file-name x))
                          (string-prefix-p root-dir file)))
                      (consult--buffer-query :sort 'visibility)))))))

(defvar consult-project--source-file
  `(
    :name      "Project file"
    :narrow    ?f
    :category  file
    :face      consult-file
    :history   file-name-history
    :action    ,(lambda (f)
                  (consult-project--file-action
                   (project-root (project-current)) f))
    :enabled   ,(lambda () (project-current))
    :items
    ,(lambda ()
       (let* ((proj     (project-current))
              (root-dir (project-root proj))
              (files    (project-files proj)))
         (consult-project--list-files root-dir files)))))

(defvar consult-project--source-project-recentf
  `(
    :name     "Recent Projects"
    :narrow   ?r
    :category file
    :face     consult-file
    :history  file-name-history
    :enabled  ,(lambda () recentf-mode)
    :action   ,#'consult-project--files
    :items    ,#'consult-project--recentf-projects))

(defvar consult-project--source-project-all
  `(
    :name     "All Projects"
    :narrow   ?p
    :category file
    :face     consult-file
    :history  file-name-history
    :enabled  ,(lambda() t)
    :action   ,#'consult-project--files
    :items    ,#'project-known-project-roots))

;;;###autoload
(defun consult-project ()
  "A consult multi view with project integration."
  (interactive)
  (when-let (buffer (consult--multi consult-project-sources
                                    :prompt "Switch to: "
                                    :require-match t
                                    :history consult-project--history
                                    :sort nil))
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))

(provide 'consult-project)
;;; consult-project.el ends here
