#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# This script builds project packages and installs them into project local environment.
#
# This script was generated authomaticaly by RSuite. Do not change it please.
#----------------------------------------------------------------------------

args <- commandArgs()
if (any(grepl("--usage", args))) {
  cat("This script builds project packages and installs them to project lib folder.\n")
  cat("Call: Rscript 02_build_install_projects_pkgs.R <args>\n")
  cat("\t--extended-version check for SVN version and update packages before build to have it as last part of version(build number)\n")
  cat("\t--build-source     if passed source versions of packages will be installed (usefull in windows)\n")
  cat("\t--verbose          if passed print lots of messages\n")
  cat("\t--usage            print this message and exit\n")
  stop("Noithing else to be done")
}

if (any(grepl("--verbose", args))) {
  logging::setLevel("DEBUG")
}
RSuite::prj_build()
