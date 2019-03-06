#----------------------------------------------------------------------------#
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package initialization.
#----------------------------------------------------------------------------#

rsuite_default_options <- list(
  rsuite.cache_path = "", # if unset prevents caching repository content and downloaded packages
  rsuite.user_templ_path = "" # if unset will prevent using user templates
)

.onLoad <- function(libpath, pkgname) {
  op <- options()
  toset <- !(names(rsuite_default_options) %in% names(op))
  if (any(toset)) options(rsuite_default_options[toset])

  if (length(logging::getLogger()[["handlers"]]) == 0) {
    logging::addHandler(logging::writeToConsole, level = "FINEST")
  }

  rsuite_register_repo_adapter(repo_adapter_create_cran(name = "CRAN"))
  rsuite_register_repo_adapter(repo_adapter_create_mran(name = "MRAN"))
  rsuite_register_repo_adapter(repo_adapter_create_url(name = "Url"))
  rsuite_register_repo_adapter(repo_adapter_create_s3(name = "S3"))
  rsuite_register_repo_adapter(repo_adapter_create_dir(name = "Dir"))

  rsuite_register_ci_adapter(ci_adapter_create_jenkins(name = "Jenkins"))

  rsuite_register_rc_adapter(rc_adapter_create_svn(name = "SVN"))
  rsuite_register_rc_adapter(rc_adapter_create_git(name = "GIT"))
}
