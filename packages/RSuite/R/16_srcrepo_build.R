#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for building source repository from github, git, svn or other sources.
#----------------------------------------------------------------------------


#'
#' @keywords internal
#' 
#' Common handling of source repository package downloading.
#' 
#' @param bld_prj project to build package in.
#' @param srcrepo_type type of src repository (one of: github, git, svn, bioc, bitbucket, url).
#' @param srcrepo src repository specific reference. see apropriate devtools::<type>_install
#'   documentation. (type: character)
#' @param ... src repository specific parameters. see apropriate devtools::<type>_install 
#'   documentation.
#' 
#' @return name of package retrieved.
#' 
get_srcrepo_package <- function(bld_prj, srcrepo_type, srcrepo, ...) {
  stopifnot(is_nonempty_char1(srcrepo_type))
  stopifnot(is_nonempty_char1(srcrepo))
  
  remote_builder <-
    switch(srcrepo_type,
           github = devtools:::github_remote,
           git = devtools:::git_remote,
           svn = devtools:::svn_remote,
           bioc = devtools:::bioc_remote,
           bitbucket = devtools:::bitbucket_remote,
           url = devtools:::url_remote,
           NULL)
  stopifnot(!is.null(remote_builder))
  
  remote <- remote_builder(srcrepo, ...)
  
  bundle <- tryCatch({
    devtools:::remote_download(remote, quiet = T)
  }, error = function(e) { NULL })
  assert(!is.null(bundle), "Failed to download from %s", srcrepo)
  on.exit({ unlink(bundle, recursive = T, force = T) }, add = TRUE)
  source <- tryCatch({
    devtools:::source_pkg(bundle, subdir = remote$subdir)
  }, error = function(e) { NULL })
  assert(!is.null(source), "Failed to retrieve sources out of package download from %s", srcrepo)
  on.exit({ unlink(source, recursive = T, force = T) }, add = TRUE)
  
  bld_params <- bld_prj$load_params()
  
  pkg_name <- read.dcf(file.path(source, "DESCRIPTION"))[1, "Package"]
  success <- 
    file.copy(from = source, to = bld_params$pkgs_path, recursive = T) &&
    file.rename(from = file.path(bld_params$pkgs_path, basename(source)),
                to = file.path(bld_params$pkgs_path, pkg_name))
  assert(success, "Failed to retrieve downloaded package source")
  unlink(source, force = T, recursive = T)
  
  return(pkg_name)
}
