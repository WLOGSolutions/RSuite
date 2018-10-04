;;; rsuite-projects.el --- Emacs for R Suite

;; Copyright (c) WLOG Solutions
;; Author: Wit Jakuczun <wit.jakuczun@wlogsolutions.com>
;; Version: 0.1
;; Keywords: rsuite, R, ess
;; URL: https://rsuite.io

;;; Commentary:

;; This package provides functions and mode for managind R Suite projects.

;;; Code:

(require 's)
(require 'dash)
(require 'tablist)

(require 'rsuite-group)

(defgroup rsuite-projects nil
  "R Suite projects management customization group."
  :group 'rsuite)

(defcustom rsuite-projects-default-sort-key '("Project" . nil)
  "Sort key for R Suite projects.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'rsuite-projects
  :type '(cons (choice (const "Version")
                       (const "R Version")
                       (const "Name")
		       (const "Path"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))


(defcustom rsuite/projects-dir "d:/Workplace/Projects"
  "Where are R Suite projects stored."

  :group 'rsuite-projects
  :type 'string)


(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun rsuite-parse-PARAMETERS-keyvals (keyvalstr)
  (s-split ": " keyvalstr))

(defun rsuite-filter-PARAMETERS (keyval)
  (s-matches? "RSuiteVersion\\|RVersion\\|Project" (car keyval)))

(defun rsuite-filter-PARAMETERS-vals (keyval)
  (pop (cdr keyval)))

(defun rsuite-is-valid-project (proj)
  (= (length (nth 1 proj)) 4))

(defun rsuite-parse-PARAMETERS (paramfile)
  (let* ((params (get-string-from-file paramfile))
	 (lines (s-split "\n" params t))
	 (keyvals (-filter #'rsuite-filter-PARAMETERS (-map #'rsuite-parse-PARAMETERS-keyvals lines)))
	 (vals (vconcat (-map #'rsuite-filter-PARAMETERS-vals keyvals) (vector paramfile)))
	 (name (file-name-directory paramfile)))
    (list name vals)))

(defun rsuite-projects-find ()
  (let* ((projs (s-split "\n"
			 (shell-command-to-string (format "%s %s %s %s"
							  find-program
							  rsuite/projects-dir
							  "-maxdepth 2"
							  "-name \"PARAMETERS\""))
			 t)))
    (-filter #'rsuite-is-valid-project
	     (-map #'rsuite-parse-PARAMETERS projs))))

(defun rsuite-build-proj-dir (name)
  (expand-file-name name rsuite/projects-dir)
  )

(defun rsuite-projects-entries-refresh ()
  "Return rsuite projects list."
  (setq tabulated-list-entries (rsuite-projects-find)))

(defun rsuite-projects ()
  "List R Suite projects."
  (interactive)
  (pop-to-buffer "*rsuite-projects*")
  (rsuite-projects-mode)
  (tablist-revert))

(defun rsuite-call-wrapper (fun)
  (tablist-put-mark)
  (let* ((entry (tabulated-list-get-entry))
	 (name (aref entry 2)))
    (funcall-interactively fun (rsuite-build-proj-dir name)))
  (tablist-unmark-all-marks)
  (tablist-revert))

(defun rsuite-proj-start-wrapper ()
  (interactive)
  (rsuite-proj-start rsuite/projects-dir))

(defun rsuite-proj-build-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-build))

(defun rsuite-proj-depsinst-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-depsinst))

(defun rsuite-proj-lock-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-lock))

(defun rsuite-proj-unlock-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-unlock))

(defun rsuite-proj-pkg-start-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-pkg-start))

(defun rsuite-proj-zip-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-proj-zip))

(defun rsuite-docker-zip-platform-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-docker-zip-platform))

(defun rsuite-docker-zip-platform-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-docker-zip-platform))

(defun rsuite-docker-zip-image-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-docker-zip-image))

(defun rsuite-docker-image-platform-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-docker-image-platform))

(defun rsuite-docker-image-image-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-docker-image-image))

(defun rsuite-sysreqs-collect-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-sysreqs-collect))

(defun rsuite-sysreqs-check-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-sysreqs-check))

(defun rsuite-sysreqs-script-wrapper ()
  "Collect and display system requirements for R Suite project."
  (interactive)
  (rsuite-call-wrapper 'rsuite-sysreqs-script))

(defun rsuite-sysreqs-install-wrapper ()
  (interactive)
  (rsuite-call-wrapper 'rsuite-sysreqs-install))

;; (defvar rsuite-projects-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "b" 'rsuite-proj-build-wrapper)
;;     (define-key map "l" 'rsuite-proj-lock-wrapper)
;;     (define-key map "z" 'rsuite-proj-zip-wrapper)
;;     map)
;;   "Keymap for `rsuite-projects-mode'.")

(define-derived-mode rsuite-projects-mode tabulated-list-mode "R Suite Projects"
  "Major mode for handling a list of R Suite projects."
  (setq tabulated-list-format [("Version" 10 t)("R Version" 10 t)("Name" 30 t)("Path" 60 t)])
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'rsuite-projects-entries-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'rsuite-projects)
