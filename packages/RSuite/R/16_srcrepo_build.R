#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for building source repository from github, git, svn or other sources.
#----------------------------------------------------------------------------

#'
#' Retrieve devtools internal function by name.
#'
#' @param name name of function to retrieve
#'
#' @return object found in devtools namespace.
#'
#' @keywords internal
#'
.get_devtools_intern <- function(name) {
  search_res <- getAnywhere(name)
  ixs <- which(search_res$where == "namespace:devtools")
  if (!length(ixs)) {
    return()
  }
  ixs <- ixs[1]
  return(search_res$objs[[ixs]])
}


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
#' @keywords internal
#'
get_srcrepo_package <- function(bld_prj, srcrepo_type, srcrepo, ...) {
  stopifnot(is_nonempty_char1(srcrepo_type))
  stopifnot(is_nonempty_char1(srcrepo))

  remote_builder_name <-
    switch(srcrepo_type,
           github = "github_remote",
           git = "git_remote",
           svn = "svn_remote",
           bioc = "bioc_remote",
           bitbucket = "bitbucket_remote",
           url = "url_remote",
           NULL)
  stopifnot(!is.null(remote_builder_name))

  remote_builder <- .get_devtools_intern(remote_builder_name)
  assert(!is.null(remote_builder),
         "Source repository '%s' handler is not available",
         srcrepo_type)
  remote <- remote_builder(srcrepo, ...)

  bundle <- tryCatch({
    remote_download <- .get_devtools_intern("remote_download")
    remote_download(remote, quiet = T)
  }, error = function(e) { NULL })
  assert(!is.null(bundle), "Failed to download from %s", srcrepo)
  on.exit({ unlink(bundle, recursive = T, force = T) }, add = TRUE)
  source <- tryCatch({
    source_pkg <- .get_devtools_intern("source_pkg")
    source_pkg(bundle, subdir = remote$subdir)
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
