(require 'json)

(defcustom wp-cli-executable "/usr/bin/wp"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-cli-buf-name "*wp-cli-output*"
  "Buffer to use to pass WP CLI commands through.")

(setq wp-cli-usage-hash (make-hash-table :test 'equal ;; stirng-equal ?
                                         :size 200))

;; disabling these commands until we can find a decent way of integrating them
(setq wp-disabled-cli-commands '("cli" "help" "shell" "export"))

(defun wp-cli()
  "Returns t if WP CLI commands are capable of being run."
  (and (wp-exists)
       (not (string= "" wp-cli-executable))
                                        ;       (file-executable-p wp-cli-executable)
       wp-cli-buf-name))

(defun wp-cli-shell-command-to-str(command)
  "Returns the output of running a shell command while logging the input
and output to `wp-cli-buf-name'."
  (when (wp-cli)
    (let ((output (shell-command-to-string (format "%s cli %s" wp-cli-executable command))))
      (with-current-buffer (get-buffer-create wp-cli-buf-name)
        (insert (format "\n>%s\n" command))
        (insert output))
      output)))

(defun wp-cli-build-usage-table()
  (let* ((cmd-dump-json (json-read-from-string (wp-cli-shell-command-to-str "cmd-dump")))
         (commands (cdr (assoc 'subcommands cmd-dump-json))))
    (mapcar (lambda(command)
              (let ((cmd-name (cdr (assoc 'name command)))
                    (subcommands (cdr (assoc 'subcommands command))))
                (unless (member cmd-name wp-disabled-cli-commands)
                  (puthash cmd-name subcommands wp-cli-usage-hash)))) commands))
  wp-cli-usage-hash)

(defun get-hash-keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys
    )
  )

(defun wp-cli-subcommands(command)
  "Takes a string such as 'cache' and returns all subcommands from `wp-cli-commands'. "
  (when (wp-cli)
    (mapcar (lambda(subcommand)
              (cdr (assoc 'name subcommand)))
            (gethash command wp-cli-usage-hash))))


(defun wp-cli-ido()
  "Prompts the user using ido for all top level WP CLI commands, upon completion it uses
`wp-cli-subcommands' to give another ido prompt for that commands subcommands.

It then lets the user input the rest of the arguments needed (if any) and shows the output
using `wp-cli-shell-command-to-str'."
  (interactive)
  (when (wp-cli)
    (let ((command (ido-completing-read "wp cli: " (get-hash-keys wp-cli-usage-hash))))
      (if (not (wp-cli-subcommands command))
          (message (wp-cli-shell-command-to-str (format "%s %s %s" wp-cli-executable command (read-string command))))
        (let ((subcommand (ido-completing-read (format "wp cli %s " command) (wp-cli-subcommands command)))
              (max-mini-window-height 0.001))
          (shell-command (s-join " " `(,wp-cli-executable ,command ,subcommand ,(read-string (format "wp cli %s %s " command subcommand))))))))))




(defun wp-cli-raw-actions(&rest args)
  "Takes n args such as 'cache' 'add' 'foo' 'bar' and runs them with `wp-cli-shell-command-to-str'
as 'wp cache add foo bar'."
  (message (wp-cli-shell-command-to-str (format "%s %s" wp-cli-executable (s-join " " args)))))

(provide 'wordpress-cli)
