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

(setq consult--source-projectile-buffer
      `(:name      "Project Buffer"
                   :narrow    (?b . "Buffer")
                   :hidden    nil
                   :category  buffer
                   :face      consult-buffer
                   :history   buffer-name-history
                   :state     ,#'consult--buffer-state
                   :enabled   ,(lambda () (and consult-project-root-function
                                               (projectile-project-root)))
                   :items
                   ,(lambda ()
                      (when-let (root (funcall consult-project-root-function))
                        (mapcar #'buffer-name
                                (seq-filter (lambda (x)
                                              (when-let (file (buffer-file-name x))
                                                (string-prefix-p root file)))
                                            (consult--cached-buffers)))))))

(setq consult--source-projectile-file
      `(:name      "Project File"
                   :narrow    (?f . "File")
                   :hidden    nil
                   :category  file
                   :face      consult-file
                   :history   file-name-history
                   :action    ,#'consult--file-action
                   :enabled   ,(lambda () (and consult-project-root-function
                                               (projectile-project-root)))
                   :items
                   ,(lambda ()
                      (let*
                          ((root (projectile-acquire-root))
                           (inv-root (propertize root 'invisible t))
                           (files (projectile-project-files root)))
                        (mapcar (lambda (f) (concat inv-root f)) files)))))


(setq consult--source-projectile-project
      `(:name      "Known Project"
                   :narrow    (?p . "Project")
                   :hidden    nil
                   :category  bookmark
                   :face      consult-bookmark
                   :history   consult--projectile-history
                   :action    ,#'consult-projectile--file ;,#'projectile-switch-project-by-name
                   :enabled   ,(lambda () (and consult-project-root-function))
                   :items
                   ,(lambda ()
                      (projectile-relevant-known-projects))))

;;;###autoload
(defun consult-projectile ()
  "Creates a multi view buffer/file switch. TODO"
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

