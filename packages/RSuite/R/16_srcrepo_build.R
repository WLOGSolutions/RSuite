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
#' @noRd
#'
.get_devtools_intern <- function(name) {
  search_res <- utils::getAnywhere(name)
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
#' @return named list with name of package('name') and package forlder ('dir').
#'
#' @keywords internal
#' @noRd
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
    remote_download(remote, quiet = FALSE)
  },
  error = function(e) NULL)

  assert(!is.null(bundle), "Failed to download from %s", srcrepo)
  on.exit({
    unlink(bundle, recursive = TRUE, force = TRUE)
  },
  add = TRUE)

  bld_params <- bld_prj$load_params()
  if (!file.info(bundle)$isdir) {
    bundle_path <- tryCatch({
      decompress <- .get_devtools_intern("decompress")
      decompress(bundle, bld_params$pkgs_path)
    },
    error = function(e) NULL)
    assert(!is.null(bundle_path), "Failed to decompress sources from %s", bundle)
  } else {
    success <- file.copy(from = bundle, to = bld_params$pkgs_path, recursive = TRUE)
    assert(success, "Failed to retrieve downloaded package source")
    bundle_path <- bundle
  }

  source <- tryCatch({
    source_pkg <- .get_devtools_intern("source_pkg")
    source_pkg(bundle_path, subdir = remote$subdir)
  },
  error = function(e) NULL)

  assert(!is.null(source), "Failed to retrieve sources out of package download from %s", srcrepo)

  # alter packages to point to retrieved package
  prj_path_toks <- unlist(strsplit(bld_params$prj_path, "[\\/]"))
  pkg_path_toks <- unlist(strsplit(rsuite_fullUnifiedPath(dirname(source)), "[\\/]"))
  pkgs_path <- paste(pkg_path_toks[- seq_along(prj_path_toks)], collapse = "/") # relative to prj_path

  bld_params_file <- file.path(bld_prj$path, "PARAMETERS")
  bld_params_df <- data.frame(read.dcf(bld_params_file), stringsAsFactors = FALSE)
  if (!("PackagesPath" %in% colnames(bld_params_df))) {
    bld_params_df <- cbind(bld_params_df, data.frame(PackagesPath = pkgs_path))
  } else {
    bld_params_df[, "PackagesPath"] <- pkgs_path
  }
  write.dcf(bld_params_df, file = bld_params_file)

  pkg_name <- read.dcf(file.path(source, "DESCRIPTION"))[1, "Package"]
  return(list(name = pkg_name, dir = basename(source)))
}
