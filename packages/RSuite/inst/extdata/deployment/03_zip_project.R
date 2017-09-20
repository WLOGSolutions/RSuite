#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# This script prepares deployment zip tagged with version (from SVN or enforced).
#
# This script was generated authomaticaly by RSuite. Do not change it please.
#----------------------------------------------------------------------------

args <- commandArgs()

if (any(grepl("--usage", args))) {
  cat("This script collects everything installed in project lib folder together with project master scripts and\n")
  cat("    specified in PROPERTIES artifacts, adds configuration template (config_templ.txt) and packs everything\n")
  cat("    into zip package stamped with a version. It can be therefore deployed on target environment as contains\n")
  cat("    everything to run project functionalities.\n")
  cat("Call: Rscript 03_zip_project.R <args>\n")
  cat("\t-v<ver>            specifies zip version to be used to stamp zip package.\n")
  cat("\t--verbose          if passed print lots of messages\n")
  cat("\t--usage            print this message and exit\n")
  stop("Noithing else to be done")
}

if (any(grepl("--verbose", args))) {
  logging::setLevel("DEBUG")
}

# Getting version
verArgs <- grepl("^-v([0-9]+)([\\._-][0-9]+)*$", args)
if (any(verArgs)) {
  zip_ver <- sub("^-v", "", args[verArgs][[1]])
} else {
  zip_ver <- NULL
}
RSuite::prj_zip(zip_ver = zip_ver)
