;;; consult-projectile.el --- Consult integration for porjectile  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  

;; Author:  Marco Pawłowski
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A multiview for displaying open buffers and files accociated with a project.
;; When no project is open in the current buffer display a list of known project.
;; and select a file from the selected project.
;;
;; Just run the function `consult-projectile' and/or bind it to a hotkey.
;;
;; To filter the multiview use:
;; B - For project related buffers
;; F - For project related files
;; P - For known projects

;;; Code:

(require 'projectile)
(require 'consult)


(setq consult--projectile-history nil)

(defcustom consult-projectile-sources
  '(consult--source-projectile-buffer
    consult--source-projectile-file
    consult--source-projectile-project)
  "Sources used by `consult-projectile'.

See `consult--multi' for a description of the source values."
  :type '(repeat symbol))

(defun consult-projectile--choose-file (root)
  (let* ((inv-root (propertize root 'invisible t))
         (files (projectile-project-files root)))
    (mapcar (lambda (f) (concat inv-root f)) files)))

(defun consult-projectile--file (selected-project)
  "Creates a view for selecting project files"
  (find-file (consult--read
              (consult-projectile--choose-file selected-project)
              :prompt "Project File: "
              :sort t
              :require-match t
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))

(setq consult-projectile--source-projectile-buffer
      `(:name      "Project Buffer"
                   :narrow    (?b . "Buffer")
                   :hidden    nil
                   :category  buffer
                   :face      consult-buffer
                   :history   buffer-name-history
                   :state     ,#'consult--buffer-state
                   :enabled   ,#'projectile-project-root
                   :items
                   ,(lambda ()
                      (when-let (root (funcall consult-project-root-function))
                        (mapcar #'buffer-name
                                (seq-filter (lambda (x)
                                              (when-let (file (buffer-file-name x))
                                                (string-prefix-p root file)))
                                            (consult--cached-buffers)))))))

(setq consult-projectile--source-projectile-file
      `(:name      "Project File"
                   :narrow    (?f . "File")
                   :hidden    nil
                   :category  file
                   :face      consult-file
                   :history   file-name-history
                   :action    ,#'consult--file-action
                   :enabled   ,#'projectile-project-root
                   :items
                   ,(lambda ()
                      (let*
                          ((root (projectile-acquire-root))
                           (inv-root (propertize root 'invisible t))
                           (files (projectile-project-files root)))
                        (mapcar (lambda (f) (concat inv-root f)) files)))))


(setq consult-projectile--source-projectile-project
      `(:name      "Known Project"
                   :narrow    (?p . "Project")
                   :hidden    nil
                   :category  bookmark
                   :face      consult-bookmark
                   :history   consult--projectile-history
                   :action    ,#'consult-projectile--file
                   :items
                   ,(lambda ()
                      (projectile-relevant-known-projects))))

;;;###autoload
(defun consult-projectile ()
  "Creates a multi view with projectile integration. Displays known projects when there are none
or the buffers/files accociated with the project."
  (interactive)
  (when-let (buffer (consult--multi consult-projectile-sources
                                    :prompt "Switch to: "
                                    :history 'consult--projectile-history
                                    :sort nil))
    ;; When the buffer does not belong to a source,
    ;; create a new buffer with the name.
    (unless (cdr buffer)
      (funcall consult--buffer-display (car buffer)))))


(provide 'consult-projectile)
;;; consult-projectile.el ends here
