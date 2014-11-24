(defcustom wp-ctags-executable "/usr/bin/ctags"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-ctags-directory "/home/dan/.emacs.d/tmp"
  "Path to directory to store ctags.")

;; ctags command line configurations
(defconst wp-ctags-language "--languages=PHP")

;; default kinds to tag, classes, interfaces, functions
(defconst wp-ctags-generic-kinds "--PHP-kinds=+cif-dvj")

;; capture do_action declarations
(defconst wp-ctags-actions-regex "'--regex-PHP=/do_action\\(\\s?([\"]{1}|['\\'']{1})([a-zA-Z0-9\_]+)\\1/\\2/a,actions/'")
(defconst wp-ctags-actions-kinds "--PHP-kinds=+a-cidvfj")

;; capture apply_filters declarations
(defconst wp-ctags-filters-regex "'--regex-PHP=/apply_filters\\(\\s?([\"]{1}|['\\'']{1})([a-zA-Z0-9\_]+)\\1/\\2/h,filters/'")
(defconst wp-ctags-filters-kinds "--PHP-kinds=+h-cidvfj")

(defconst wp-in-action-regex "add_action([[:space:]]?+\\('\\|\"\\)")
(defconst wp-in-filter-regex "add_filter([[:space:]]?+\\('\\|\"\\)")

;; flags to be run, these are only modified when appending
(defvar wp-ctags-flags "-Ren")

(defconst wp-ctags-generic-file "wp_generic_tags")
(defconst wp-ctags-filters-file "wp_filters_tags")
(defconst wp-ctags-actions-file "wp_actions_tags")

(defun wp-get-ctags-executable()
  (if (null (tramp-tramp-file-p (buffer-file-name)))
      wp-ctags-executable
    (let ((v (tramp-dissect-file-name (buffer-file-name))))
      (tramp-make-tramp-file-name (tramp-file-name-method v)
                                  (tramp-file-name-user v)
                                  (tramp-file-name-host v)
                                  wp-ctags-executable))))

;; ctags integration
(defun wp-ctags()
  "Returns t if it's possible to run ctags."
  (and (wp-exists)
       (not (string= "" wp-ctags-executable))
       (file-executable-p wp-ctags-executable)))

(defun wp-ctags-destination(&optional ctags-file-name)
  "Returns the destination for the projects tags files, optionally
concatenates it with `ctags-file-name'."
  (if (wp-ctags)
      (concat wp-ctags-directory "/" (s-replace "/" "_" (wp-exists)) ctags-file-name)))

;; exclude autosave/backup files.
(defun wp--run-ctags(outfile path type &optional append)
  "Assembles the ctags command to be run with `shell-command'.

This append business is foolish, assuming the user never re-builds the tags
tables, after many saves they will grow to unusable sizes. (albeit, MANY saves)

The ideal for performance is what we do, just call --append with ctags, on only
the file being saved. The issue is --append will continue to append new tags, infinitely
many times, so do_action('foo') would appear n times, n being 1(initial build) + number of times files saved with that do_action.

Perhaps force a re-build over time? Inconvenient, but I'm sure IDEs rebuild all the time. Perhaps re-build over time async.

`type' is anything, and the ctags command is built based on `wp-ctags-TYPE-regex' and/or `wp-ctags-TYPE-kinds' being defined."
  (when (wp-ctags)
    (shell-command (format "%s %s %s %s -o %s %s"
                            wp-ctags-executable ;; ctags
                            wp-ctags-language ;; --languages=PHP
                            (concat
                             (if (boundp (intern (format "wp-ctags-%s-regex" type)))
                                 (eval (intern (format "wp-ctags-%s-regex" type)))) ;; --regex-PHP=...
                             " "
                             (if (boundp (intern (format "wp-ctags-%s-kinds" type)))
                                 (eval (intern (format "wp-ctags-%s-kinds" type))))) ;; --PHP-kinds=...
                            (concat
                             wp-ctags-flags
                             " "
                             (if append
                                 " --append")) ;; -Ren (--append)
                            outfile ;; WPTAGS or something similar
                            path)))) ;; project_dir or file.php

(defun wp-ctags-build-tables()
  "Builds all of the ctags tables, regardless of whether or not
they exist."
  (interactive)
  (when (wp-ctags)
    (mapc (lambda(arg)
            (message "Indexing %s to %s" (car (cdr arg)) (car arg))
            (wp--run-ctags (wp-ctags-destination (car arg)) (wp-exists) (car (cdr arg))))
          `((,wp-ctags-generic-file "generic")
            (,wp-ctags-actions-file "actions")
            (,wp-ctags-filters-file "filters")))
    (message "done.")))

(defun wp-ctags-table-list()
  (mapcar 'wp-ctags-destination `(,wp-ctags-generic-file ,wp-ctags-actions-file ,wp-ctags-filters-file)))



(fset 'company-etags-buffer-table 'wp-ctags-table-list)

(defun wp-get-tag-names(type)
  "Gets the names of all tags within the tag file."
  (when (string= type "all")
    (visit-tags-table (wp-ctags-destination wp-ctags-generic-file))
    (visit-tags-table (wp-ctags-destination wp-ctags-actions-file))
    (visit-tags-table (wp-ctags-destination wp-ctags-filters-file)))
  (when (string= type "generic")
    (visit-tags-table (wp-ctags-destination wp-ctags-generic-file)))
  (when (string= type "actions")
    (visit-tags-table (wp-ctags-destination wp-ctags-actions-file)))
  (when (string= type "filters")
    (visit-tags-table (wp-ctags-destination wp-ctags-filters-file)))

  (let ((tag-names '()))
    (mapatoms (lambda(ob)
                (push (symbol-name ob) tag-names)) (tags-completion-table))
    tag-names))

(defun wp-goto-declaration()
  (interactive)
  (find-tag (symbol-name (symbol-at-point))))

(defun wp-find-usages()
  (interactive)
  (tags-search (symbol-name (symbol-at-point))))

(defun wp-ctags-after-save()
  "Append any new tags from this file to the existing tags files."
  (when (wp-ctags)
    (wp--run-ctags (wp-ctags-destination "wp_generic_tags") (buffer-file-name) "generic" t)
    (run-hooks 'wp-ctags-after-save-hook)))

(add-hook 'after-save-hook 'wp-ctags-after-save)


(defun company-my-backend (command &optional arg &rest ignored)
    (pcase command
      (`prefix (when (looking-back wp-in-action-regex)
                (match-string 1)))
      (`candidates (list "OMFG_ITS_AN_ACTION"))
      (`meta (format "This value is named %s" arg))))

(defun company-my-backend (command &optional arg &rest ignored)
  (pcase command
    (`prefix (when (looking-back wp-in-filter-regex)
               (cons (match-string 1) t)))
    (`candidates (list "omfgitsafilter"))
    (`meta (format "This value is named %s" arg))))



(defun wp-company-etags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for etags."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-etags))
    (prefix (and (derived-mode-p php-mode)
                 (company-etags-buffer-table)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-etags--candidates arg))
    (location (let ((tags-table-list (company-etags-buffer-table)))
                (when (fboundp 'find-tag-noselect)
                  (save-excursion
                    (let ((buffer (find-tag-noselect arg)))
                      (cons buffer (with-current-buffer buffer (point))))))))
    (ignore-case company-etags-ignore-case)))






(provide 'wordpress-ctags)
