;;; rsuite.el --- Emacs for R Suite

;; Copyright (c) WLOG Solutions
;; Author: Wit Jakuczun <wit.jakuczun@wlogsolutions.com>
;; Version: 0.1
;; Keywords: rsuite, R, ess
;; URL: https://rsuite.io

;;; Commentary:

;; This package provides functions to handle R Suite projects

;;; Code:

(require 'dired)
(require 'ivy)

(require 'rsuite-group)

(defcustom rsuite/verbose nil "Make rsuite verbose."
  :group 'rsuite
  :type 'boolean)

(defconst rsuite/docker-platforms
  (list "ubuntu" "centos" "debian")
  "Docker platforms supported by R Suite.")
(defconst rsuite/buffer "*rsuite*" "Buffer for rsuite output.")
(defconst rsuite/err_buffer "*rsuite:error*" "Buffer for rsuite error output.")
(defconst rsuite/cli "rsuite" "Path to RSuite CLI.")

(defun run-async-rsuite (args)
  "Utility for running rsuite cli.
ARGS is a string of arguments forwarded to rsuite cli."
  (async-shell-command (concat rsuite/cli " " (concat args (if rsuite/verbose " -v" " "))) rsuite/buffer rsuite/err_buffer))

(defun run-sync-rsuite (args)
  "Utility for running rsuite cli.
ARGS is a string of arguments forwarded to rsuite cli."
  (shell-command (concat rsuite/cli " " (concat args (if rsuite/verbose " -v" " "))) rsuite/buffer rsuite/err_buffer))

(defun rsuite-detect-prj-path ()
  "Return R Suite's project path or throws error if none found."
  (let
      ((curr-dir default-directory)
       (up-dir nil))
    (if buffer-file-truename
	(setq curr-dir (file-name-directory (directory-file-name buffer-file-truename)))
      (setq curr-dir (dired-current-directory)))

    (while (not (file-exists-p (concat (file-name-as-directory curr-dir) "PARAMETERS")))
      (setq up-dir (file-name-directory (directory-file-name curr-dir)))
      (if (string= curr-dir up-dir)
	  (error "Cannot find RSuite project"))
      (cd up-dir)
      (setq curr-dir up-dir)
      )
    curr-dir))

(defun rsuite-get-templates ()
  "Generate list of available R Suite templates."
  (cons "builtin" (directory-files "~/.rsuite/templates" nil "^[^.].*" nil)))

(defun rsuite-toggle-verbose ()
  "Toggle R Suite verbosity."
  (interactive)
  (if rsuite/verbose
      (setq rsuite/verbose nil)
    (setq rsuite/verbose 1)))

(defun rsuite-call-within-dir (fun &optional projdir)
  "Runs fun within given projdir or in detected directory."
  
  (if projdir
      (let ((olddir default-directory))
	(cd projdir)
	(funcall fun)
	(cd olddir))
    (funcall fun)))

(defun rsuite-proj-start (&optional dir)
  "Start project."
  (interactive)
  (let ((proj_path nil)
	(proj_name nil)
	(proj_tmpl nil))    
    (setq proj_path (read-directory-name "Project path: " dir))
    (setq proj_name (read-string (concat "Give project name: " proj_path)))
    (setq proj_tmpl (ivy-read "Give project template: " (rsuite-get-templates)))
    (cd proj_path)
    (if (> (length proj_tmpl) 0)
	(run-sync-rsuite (concat "proj start --name=" proj_name " --tmpl=" proj_tmpl))
      (run-sync-rsuite (concat "proj start --name=" proj_name)))
    
    (dired (concat proj_path "/" proj_name))))

(defun rsuite-proj-pkg-start (&optional projdir)
  "Start package."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((pkg_name nil)
				  (pkg_tmpl nil))
			      (setq pkg_name (read-string "Give package name: "))
			      (setq pkg_tmpl (ivy-read "Give package template: " (rsuite-get-templates)))
			      (if (> (length pkg_tmpl) 0)
				  (run-async-rsuite (concat "proj pkgadd --name=" pkg_name " --tmpl=" pkg_tmpl))
				(run-async-rsuite (concat "proj pkgadd --name=" pkg_name)))))
			  projdir))

(defun rsuite-proj-build (&optional projdir)
  "Build current project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (run-async-rsuite "proj build"))
			  projdir))

(defun rsuite-proj-depsinst (&optional projdir)
  "Install dependencies."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (run-async-rsuite "proj depsinst"))
			  projdir))

(defun rsuite-proj-lock (&optional projdir)
  "Lock current version of packages."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (run-async-rsuite "proj lock"))
			  projdir))

(defun rsuite-proj-unlock (&optional projdir)
  "Release lock of current version of packages."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (run-async-rsuite "proj unlock"))
			  projdir))

(defun rsuite-proj-test (&optional projdir)
  "Run project test."
  (rsuite-call-within-dir (lambda ()
			    (run-async-rsuite "proj test"))
			  projdir))

(defun rsuite-proj-zip (&optional projdir)
  "Make deployment."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((version nil) (path nil))
			      (setq version (read-string "Version: "))
			      (setq path (read-directory-name "Path: "))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat "proj zip --version=" version " --path=" path))
				(run-async-rsuite (concat "proj zip --path=" path)))
			      ))
			  projdir))

(defun rsuite-docker-zip-image (&optional projdir)
  "Make deployment zip using docker and custom base image."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((rver nil)
				  (image nil)
				  (version nil)
				  (path nil)
				  (rcmd "docker zip"))
			      (setq version (read-string "Version: "))
			      (setq path (read-directory-name "Path: "))
			      (setq image (read-string "Image: "))
			      (setq rver (read-string "RVer: "))
			      (setq rcmd (concat rcmd
						 " --dest=" path
						 " --rver=" rver
						 " --image=" image))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))

(defun rsuite-docker-zip-platform (&optional projdir)
  "Make deployment zip using docker and image of one of the supported OSs."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((platform nil)
				  (version nil)
				  (path nil)
				  (rcmd "docker zip"))
			      (setq version (read-string "Version: "))
			      (setq path (read-directory-name "Path: " projdir))
			      (setq platform (ivy-read
					      "Platform: "
					      rsuite/docker-platforms))
			      (setq rcmd (concat rcmd
						 " --dest=" path
						 " --platform=" platform))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))

(defun rsuite-docker-image-platform (&optional projdir)
  "Make deployment docker image for supported OSs."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((platform nil)
				  (version nil)
				  (tag nil)
				  (rcmd "docker img"))
			      
			      (setq version (read-string "Version: "))
			      (setq tag (read-string "Tag: "))
			      (setq platform (ivy-read
					      "Platform: "
					      rsuite/docker-platforms))
			      (setq rcmd (concat rcmd
						 " --tag=" tag
						 " --platform=" platform))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))


(defun rsuite-docker-image-image (&optional projdir)
  "Make deployment docker image using custom base image."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((image nil)
				  (version nil)
				  (tag nil)
				  (rcmd "docker img"))
			      
			      (setq version (read-string "Version: "))
			      (setq tag (read-string "Tag: "))
			      (setq image (read-string "Image: "))
			      (setq rcmd (concat rcmd
						 " --tag=" tag
						 " --image=" image))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))


(defun rsuite-docker-image-template (&optional projdir)
  "Make deployment docker image using template Dockerfile."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((version nil)
				  (tag nil)
				  (tmpl nil)
				  (rcmd "docker img"))
			      
			      (setq version (read-string "Version: "))
			      (setq tag (read-string "Tag: "))
			      (setq tmpl (read-string "Dockerfile template file: "))
			      (setq rcmd (concat rcmd
						 " --tag=" tag
						 " --templ " tmpl))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))

(defun rsuite-proj-pack (&optional projdir)
  "Pack R Suite project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((version nil)
				  (path nil)
				  (rcmd "proj pack"))
			      (setq version (read-string "Version: "))
			      (setq path (read-directory-name "Path: "))
			      (setq rcmd (concat rcmd
						 " --path=" path))
			      (if (> (length version) 0)
				  (run-async-rsuite (concat rcmd
							    " --version=" version))
				(run-async-rsuite rcmd))))
			  projdir))

(defun rsuite-proj-find-master (&optional projdir)
  "Find existing or new master file.
If file exists it is opened.  Otherwise it is created and filled with R Suite init lines."
  (interactive)
  (rsuite-call-within-dir (lambda () 
			    (let ((m_path nil)
				  (m_name nil))
			      (setq m_path (concat (file-name-as-directory (rsuite-detect-prj-path)) "R/"))
			      (setq m_name (read-file-name "Master " m_path "master.R"))
			      (if (file-exists-p m_name)
				  (progn
				    (find-file m_name)
				    (goto-char (point-max))
				    )
				(progn	
				  (append-to-file
				   "# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- dirname(sub(\"--file=\", \"\", args[grep(\"--file=\", args)]))
  if (!length(script_path)) {
      return(\"R\")
  }
  if (grepl(\"darwin\", R.version$os)) {
      base <- gsub(\"~\\\\+~\", \" \", base) # on MacOS ~+~ in path denotes whitespace
  }
  return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, \"set_env.R\"), chdir = T)

config <- load_config()
args <- args_parser()

" nil m_name)
				  (find-file m_name)
				  (goto-char (point-max))))))
			  projdir))


(defun rsuite-proj-run-master (&optional projdir)
  "Run master file in a given project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((masterfile nil)
				  (cmd-args nil)
				  (master-args nil))
			      (setq masterfile (read-string "Master file: "))
			      (setq master-args (read-string "args: "))
			      (setq cmd-args (concat (file-name-as-directory (rsuite-detect-prj-path))
						     "R/"
						     (if (> (length masterfile) 0) masterfile
						       "master.R")
						     (if (> (length master-args) 0)
							 master-args
						       "")))
			      (async-shell-command (concat "Rscript " cmd-args)
						   (concat "*rsuite:run:" masterfile "*")
						   rsuite/err_buffer))
			    projdir)))

(defun rsuite-sysreqs-collect (&optional projdir)
  "Collect and display system requirements for R Suite project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((rcmd "sysreqs collect"))
			      (run-async-rsuite rcmd)))
			  projdir))

(defun rsuite-sysreqs-check (&optional projdir)
  "Check current system against required requirements for R Suite project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((rcmd "sysreqs check"))
			      (run-async-rsuite rcmd)))
			  projdir))

(defun rsuite-sysreqs-script (&optional projdir)
  "Collect and display system requirements for R Suite project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((rcmd "sysreqs script"))
			      (run-async-rsuite rcmd)))
			  projdir))

(defun rsuite-sysreqs-install (&optional projdir)
  "Install system requirements for R Suite project."
  (interactive)
  (rsuite-call-within-dir (lambda ()
			    (let ((rcmd "sysreqs install"))
			      (run-async-rsuite rcmd)))
			  projdir))


;; Load projects management functions
(require 'rsuite-projects)

(provide 'rsuite)
;;; rsuite.el ends here
