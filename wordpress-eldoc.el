(require 's)
(require 'thingatpt)
(require 'php-eldoc)

;;; Code:

(defconst wp-eldoc-php-function-regex "function \\([A-Za-z0-9\_]+\\)\(\\(.*\\)\)")

(defvar wp-eldoc-functions-hash (or (wp-eldoc-cached-hash-table)
                                    (wp-eldoc--build-hash-table)))

(defun wp-eldoc--build-hash-table()
  (let ((hash-table (make-hash-table :size 300 :test 'equal)))
    (dolist (tag (wp-get-tag-names "generic"))
      (with-current-buffer (find-tag-noselect tag)
        (let ((line (thing-at-point 'line)))
          (when (and (string-match wp-eldoc-php-function-regex line)
                     (not (string-match-p "{}" line))) ;; weird empty function thing, see add_action declarations
            (let* ((func-name (match-string 1 line))
                   (func-arg-str (match-string 2 line))
                   (func-args (mapcar 's-trim (s-split "," func-arg-str))))
              (puthash func-name func-args hash-table))))))
    hash-table))

(defun wp-eldoc-function ()
  (let* ((func (wp-function-and-argument))
         (hash-result (when func
                        (gethash (car func) wp-eldoc-functions-hash)))
         (arguments "")
         (counter 0))
    (when hash-result
      (cl-dolist (arg hash-result)
        (setq arguments
              (concat arguments
                      (if (equal counter (second func))
                          (propertize arg 'face '(:weight bold))
                        arg)
                      ", "))
        (incf counter)))
    (when (>= (length arguments) 2)
      (setq arguments (substring arguments 0 (- (length arguments) 2))))
    (when hash-result
      (concat (propertize (first func) 'face 'font-lock-function-name-face)
              "( " arguments " )"))))

(defun wp-eldoc-ac-candidates ()
  (let (result)
    (maphash (lambda (key value)
               (push key result))
             wp-eldoc-functions-hash)
    result))

(eval-after-load 'auto-complete
  '(ac-define-source wp-eldoc
     '((candidates . wp-eldoc-ac-candidates)
       (cache)
       (symbol . "f"))))

(defun wp-eldoc-enable()
  (interactive)
  (when (and (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (pushnew 'ac-source-wp-eldoc ac-sources))
  (setq-local eldoc-documentation-function 'wp-eldoc-function))

;; (defun wp-eldoc-cache-hash-table()
;;   (with-temp-buffer
;;     (prin1 wp-eldoc-functions-hash)
;;     (write-file (wp-destination "wp-eldoc.cache"))))

;; (defun wp-eldoc-cached-hash-table()
;;   (if (file-exists-p (wp-destination "wp-eldoc.cache"))
;;       (with-temp-buffer

;;       (read )





;; requires ctags to be working...

;; persist the hashtable to disk:
;; http://stackoverflow.com/questions/11745097/serializing-an-emacs-lisp-hash-to-file
(defun wp-eldoc()
  (php-eldoc-enable)
  (message "Building wp-eldoc list..")
  (dolist (tag (wp-get-tag-names "generic"))
    (with-current-buffer (find-tag-noselect tag)
      (let ((line (thing-at-point 'line)))
        (when (and (string-match wp-eldoc-php-function-syntax line)
                   (not (string-match-p "{}" line))) ;; weird empty function thing, see add_action declarations
          (let* ((func-name (match-string 1 line))
                 (func-arg-str (match-string 2 line))
                 (func-args (mapcar 's-trim (s-split "," func-arg-str))))
            (puthash func-name func-args php-eldoc-functions-hash))))
      )))

(defun wp-eldoc-after-save())
;; loop through the set minus of: (wp-get-tag-names "generic") - php-eldoc-functions-hash keys
;; add each one of those to the hashtable

(add-hook 'wp-ctags-after-save-hook 'wp-eldoc-after-save)


(provide 'wordpress-eldoc)

;;; wordpress-eldoc.el ends here
