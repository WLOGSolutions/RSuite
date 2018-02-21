#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# This script builds environment to use with RSuite.
#
# This script was generated authomaticaly by RSuite. Do not change it please.
#----------------------------------------------------------------------------

args <- commandArgs()
if (any(grepl("--usage", args))) {
  cat("This script initializes global system environment to contain tools for deployment scripts to work propely.\n")
  cat("Call: Rscript 00_build_env.R <args>\n")
  cat("\t--verbose          if passed print lots of messages\n")
  cat("\t--usage            print this message and exit\n")
  stop("Noithing else to be done")
}

message("Detecting repository ...")
cran_path <- (function() {
  official_repo_path <- getOption('repos') # This can point to CRAN mirror
  if ('@CRAN@' %in% official_repo_path) {
    official_repo_path <- getCRANmirrors()$URL[[1]]
  }
  return(official_repo_path)
})()
s3_path <- (function() {
  'http://wlog-rsuite.s3.amazonaws.com'
})()

all_repos <- c(CRAN = cran_path,
               S3 = s3_path) # S3 is required untill RSuite is not available on CRAN
if (is.null(all_repos)) {
  stop("No repositories to look for dependencies in!")
}
message("Will use repo(s):")
for(n in names(all_repos)) {
  message(sprintf("\t%10s = %s", n, all_repos[[n]]))
}

required_pkgs <- c("RSuite")
if (!any(grepl("--force", args))) {
  installed_pkgs <- utils::installed.packages(.Library)[, "Package"]
  required_pkgs <- setdiff(required_pkgs, installed_pkgs)
}

verbose <- any(grepl("--verbose", args))

if (length(required_pkgs)) {
  message(sprintf("Installing %s packages...", length(required_pkgs)))

  # Installing stringi is specific if you are building in connectionless environment.
  #  During instalation it downloads ICUD library which in the case has to be predownloaded and pointed with ICUDR_DIR variable
  # TODO: pkgSpecificInstall("stringi", .Library, repos = all_repos, quiet = !verbose)
  utils::install.packages(required_pkgs, repos = all_repos, quiet = !verbose)

  installed_pkgs <- utils::installed.packages()[, "Package"]
  uninstalled <- setdiff(required_pkgs, installed_pkgs)
  if (length(uninstalled)) {
    stop(sprintf("Failed to install %s", paste(uninstalled, collapse = ", ")))
  }
} else {
  message("Environment is complete. Nothing to install.")
}
