#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools patching R functionalities.
#----------------------------------------------------------------------------

#'
#' This provides support for binary package installation from local CRAN on Linux.
#'
#' @keywords internal
#'
rsuite_contrib_url <- function(repos, type, rver = NA) {
  rver <- ifelse(is.na(rver), current_rver(), majmin_rver(rver))

  if (.Platform$pkgType != "source" || type != "binary" || .Platform$OS.type != "unix") {
    url <- utils::contrib.url(repos, type = type)
    if (type == "source") {
      return(url)
    }
    return(gsub("\\d+\\.\\d+$", rver, url))
  }

  get_os_version <- function(rel_file) {
    rel_str <- readLines(rel_file)[[1]]
    toks <- unlist(strsplit(rel_str, " "))
    ver <- toks[grep("^\\d+[.]\\d+([.]\\d+)?$", toks)][1]
    ver <- gsub("^(\\d+[.]\\d+)([.]\\d+)?$", "\\1", ver)
    return(ver)
  }
  if (file.exists('/etc/redhat-release')) {
    ver <- get_os_version('/etc/redhat-release')
    assert(!is.na(ver), "Failed to detect os version. Tried /etc/redhat-release.")

    rel <- paste0("rhel", ver)
  } else if (file.exists('/etc/debian_version')) {
    ver <- get_os_version('/etc/debian_version')
    if (is.na(ver)) {
      rel_str <- readLines('/etc/issue')[[1]]
      toks <- unlist(strsplit(rel_str, " "))
      toks <- toks[grep("^\\d+[.]\\d+([.]\\d+)?$", toks)]
      ver <- gsub("^(\\d+[.]\\d+)([.]\\d+)?$", "\\1", toks)[1]
    }
    assert(!is.na(ver), "Failed to detect os version. Tried /etc/debian_version and /etc/issue.")
    rel <- paste0("deb", ver)
  } else {
    rel <- .Platform$OS.type
  }

  os_path <- sprintf("%s_%s", rel, R.version$arch)

  res <- paste(gsub("/$", "", repos), "bin", os_path, "contrib", rver, sep="/")
  res
}

#'
#' Fail proof writing of PACKAGES files.
#'
#' In R3.4 they changed write_PACKAGES not to create intex files if repository is
#' empty. It causes problems then using such an empty repository. This function
#' wraps write_PACKAGES and ensures index files exist.
#'
#' @keywords internal
#'
rsuite_write_PACKAGES <- function(url, type) {
  if (!dir.exists(url)) {
    dir.create(url, recursive = TRUE)
  }
  if (file.access(url, 2) == -1) {
    pkg_logwarn("You do not have access to local repository folder %s", url)
    return()
  }

  if (type %in% c("win.binary", "source", "mac.binary")) {
    tools::write_PACKAGES(url, type = type, latestOnly = F, addFiles = T)
  } else {
    tools::write_PACKAGES(url, latestOnly = F, addFiles = T)
  }

  if (!file.exists(file.path(url, "PACKAGES"))) {
    con <- file(file.path(url, "PACKAGES"), "wt")
    write.dcf(NULL, con)
    close(con)
  }

  if (!file.exists(file.path(url, "PACKAGES.gz"))) {
    con <- gzfile(file.path(url, "PACKAGES.gz"), "wt")
    write.dcf(NULL, con)
    close(con)
  }

  if (file.exists(file.path(url, "PACKAGES.rds"))) {
    # get rid of PACKAGES.rds as it does not support package version history
    unlink(file.path(url, "PACKAGES.rds"), recursive = T, force = T)
  }
}

#'
#' Converts passed path to full path and replaces it to short names on Windows.
#'
#' @keywords internal
#'
rsuite_fullUnifiedPath <- function(path) {
  path <- suppressWarnings(normalizePath(path))
  if (.Platform$OS.type == "windows") {
    path <- suppressWarnings(utils::shortPathName(path))
  }
  return(sub("[/\\]*$", "", path))
}
