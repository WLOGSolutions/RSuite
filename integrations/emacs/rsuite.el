;;; rsuite.el --- Emacs for R Suite

;; Copyright (c) WLOG Solutions
;; Author: Wit Jakuczun <wit.jakuczun@wlogsolutions.com>
;; Version: 0.1
;; Keywords: rsuite, R, ess
;; URL: http://rsuite.io

;;; Commentary:

;; This package provides functions to handle R Suite projects

;;; Code:

(require 'dired)

(defvar rsuite-verbose nil "Make rsuite verbose.")
(defvar rsuite-docker-platforms
  (list "ubuntu" "centos")
  "Docker platforms supported by R Suite.")
(defconst rsuite/buffer "rsuite" "Buffer for rsuite output.")
(defconst rsuite/err_buffer "rsuite:error" "Buffer for rsuite error output.")
(defconst rsuite/cli "rsuite" "Path to RSuite CLI.")

(defun rsuite-toggle-verbose ()
  "Toggle R Suite verbosity."
  (interactive)
  (if rsuite-verbose
      (setq rsuite-verbose nil)
    (setq rsuite-verbose 1)))

(defun run-rsuite (args)
  "Utility for running rsuite cli.
ARGS is a string of arguments forwarded to rsuite cli."
  (async-shell-command (concat rsuite/cli " " (concat args (if rsuite-verbose " -v" " "))) rsuite/buffer rsuite/err_buffer))

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


(defun rsuite-proj-start ()
  "Start project."
  (interactive)
  (let ((proj_path nil)
	(proj_name nil))
    
    (setq proj_path (read-directory-name "Project path: "))
    (setq proj_name (read-string (concat "Give project name: " proj_path)))
    (cd proj_path)
    (run-rsuite (concat "proj start --name=" proj_name))
    (dired (concat proj_path "/" proj_name))))

(defun rsuite-proj-pkg-start ()
  "Start package."
  (interactive)
  (let ((pkg_name nil))
    (setq pkg_name (read-string "Give package name: "))
    (run-rsuite (concat "proj pkgadd --name=" pkg_name))))

(defun rsuite-proj-build ()
  "Build current project."
  (interactive)
  (run-rsuite "proj build"))

(defun rsuite-proj-depsinst ()
  "Install dependencies."
  (interactive)
  (run-rsuite "proj depsinst"))

(defun rsuite-proj-test ()
  "Run project test."
  (run-rsuite "proj test"))

(defun rsuite-proj-zip ()
  "Make deployment."
  (interactive)
  (let ((version nil) (path nil))
    (setq version (read-string "Version: "))
    (setq path (read-directory-name "Path: "))
    (if (> (length version) 0)
	(run-rsuite (concat "proj zip --version=" version " --path=" path))
      (run-rsuite (concat "proj zip --path=" path)))))

(defun rsuite-docker-zip-image ()
  "Make deployment zip using docker and custom base image."
  (interactive)
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
	(run-rsuite (concat rcmd
			    " --version=" version))
      (run-rsuite rcmd))))

(defun rsuite-docker-zip-platform ()
  "Make deployment zip using docker and image of one of the supported OSs."
  (interactive)
  (let ((platform nil)
	(version nil)
	(path nil)
	(rcmd "docker zip"))
    (setq version (read-string "Version: "))
    (setq path (read-directory-name "Path: "))
    (setq platform (ido-completing-read "Platform: " rsuite-docker-platforms))
    (setq rcmd (concat rcmd
		       " --dest=" path
		       " --platform=" platform))
    (if (> (length version) 0)
	(run-rsuite (concat rcmd
			    " --version=" version))
      (run-rsuite rcmd))))

(defun rsuite-docker-image-platform ()
  "Make deployment docker image for supported OSs."
  (interactive)
  (let ((platform nil)
	(version nil)
	(tag nil)
	(rcmd "docker img"))
    
    (setq version (read-string "Version: "))
    (setq tag (read-string "Tag: "))
    (setq platform (ido-completing-read "Platform: " rsuite-docker-platforms))
    (setq rcmd (concat rcmd
		       " --tag=" tag
		       " --platform=" platform))
    (if (> (length version) 0)
	(run-rsuite (concat rcmd
			    " --version=" version))
      (run-rsuite rcmd))))

(defun rsuite-docker-image-image ()
  "Make deployment docker image using custom base image."
  (interactive)
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
	(run-rsuite (concat rcmd
			    " --version=" version))
      (run-rsuite rcmd))))

(defun rsuite-proj-pack ()
  "Pack R Suite project."
  (interactive)
  (let ((version nil)
	(path nil)
	(rcmd "proj pack"))
    (setq version (read-string "Version: "))
    (setq path (read-directory-name "Path: "))
    (setq rcmd (concat rcmd
		       " --path=" path))
    (if (> (length version) 0)
	(run-rsuite (concat rcmd
			    " --version=" version))
      (run-rsuite rcmd))))

(defun rsuite-proj-find-master ()
  "Find existing or new master file.
If file exists it is opened.  Otherwise it is created and filled with R Suite init lines."
  (interactive)
  (let ((m_path nil)
	(m_name nil))
    (setq m_path (concat (file-name-as-directory (rsuite-detect-prj-path)) "/R/"))
    (setq m_name (read-file-name "Master" m_path "master.R"))
    (if (file-exists-p m_name)
	(progn
	  (find-file m_name)
	  (goto-char (point-max))
	  )
      (progn
       (write-region
	"# Detect proper script_path (you cannot use args yet as they are build with tools in set_env.r)
script_path <- (function() {
	args <- commandArgs(trailingOnly = FALSE)
	script_path <- dirname(sub(\"--file=\", \"\", args[grep(\"--file=\", args)]))
	if (!length(script_path)) { return(\".\") }
	return(normalizePath(script_path))
})()

# Setting .libPaths() to point to libs folder
source(file.path(script_path, \"set_env.R\"), chdir = T)

config <- load_config()
args <- args_parser()

" nil m_name)
       (find-file m_name)
       (goto-char (point-max))))))

(provide 'rsuite)
;;; rsuite.el ends here
