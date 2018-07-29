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
#' @noRd
#'
rsuite_contrib_url <- function(repos, type, rver = NA) {
  rver <- ifelse(is.na(rver), current_rver(), majmin_rver(rver))

  if (.Platform$pkgType != "source" || type != "binary" || get_os_type() != "unix") {
    url <- utils::contrib.url(repos, type = type)
    if (type == "source") {
      return(url)
    }
    return(gsub("\\d+\\.\\d+$", rver, url))
  }

  get_os_version <- function(rel_file) {
    rel_str <- readLines(rel_file)[[1]]
    toks <- unlist(strsplit(rel_str, " "))
    ver <- toks[grep("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", toks)][1]
    ver <- gsub("^(\\d+[.]\\d+)([.]\\d+)?$", "\\1", ver)
    return(ver)
  }
  if (file.exists("/etc/redhat-release")) {
    ver <- get_os_version("/etc/redhat-release")
    if (is.na(ver)) {
      os_path <- sprintf("rhel_%s",  R.version$platform)
      pkg_logwarn("Failed to detect os version. Tried /etc/redhat-release. Will use generic %s",
                  os_path)
    } else {
      os_path <- sprintf("rhel%s_%s", ver, R.version$arch)
    }
  } else if (file.exists("/etc/debian_version")) {
    ver <- get_os_version("/etc/debian_version")
    if (is.na(ver)) {
      rel_str <- readLines("/etc/issue")[[1]]
      toks <- unlist(strsplit(rel_str, " "))
      toks <- toks[grep("^\\d+[.]\\d+([.]\\d+)?$", toks)]
      ver <- gsub("^(\\d+[.]\\d+)([.]\\d+)?$", "\\1", toks)[1]
    }
    if (is.na(ver)) {
      os_path <- sprintf("deb_%s",  R.version$platform)
      pkg_logwarn("Failed to detect os version. Tried /etc/debian_version and /etc/issue. Will use generic %s",
                  os_path)
    } else {
      os_path <- sprintf("deb%s_%s", ver, R.version$arch)
    }
  } else {
    os_path <- sprintf("%s_%s", R.version$platform, R.version$arch)
    pkg_logwarn("Unknown platform neigher Debian-like nor RedHat-like. Will use generic %s", os_path)
  }

  res <- paste(gsub("/$", "", repos), "bin", os_path, "contrib", rver, sep = "/")
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
#' @noRd
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
    nr <- tools::write_PACKAGES(url, type = type, latestOnly = FALSE, addFiles = TRUE)
  } else if (grepl("^mac[.]binary[.]", type)) {
    # mac.binary.el-capitan is not accepted by write_PACKAGES
    nr <- tools::write_PACKAGES(url, type = "mac.binary", latestOnly = FALSE, addFiles = TRUE)
  } else {
    nr <- tools::write_PACKAGES(url, latestOnly = FALSE, addFiles = TRUE)
  }

  if (!file.exists(file.path(url, "PACKAGES")) || nr == 0) {
    con <- file(file.path(url, "PACKAGES"), "wt")
    write.dcf(NULL, con)
    close(con)
  }

  if (!file.exists(file.path(url, "PACKAGES.gz"))  || nr == 0) {
    con <- gzfile(file.path(url, "PACKAGES.gz"), "wt")
    write.dcf(NULL, con)
    close(con)
  }

  if (file.exists(file.path(url, "PACKAGES.rds"))) {
    # get rid of PACKAGES.rds as it does not support package version history
    unlink(file.path(url, "PACKAGES.rds"), recursive = TRUE, force = TRUE)
  }
}

#'
#' Converts passed path to full path and replaces it to short names on Windows.
#'
#' @keywords internal
#' @noRd
#'
rsuite_fullUnifiedPath <- function(path) {
  if (is.null(path) || length(path) == 0) {
    return(c())
  }
  path <- suppressWarnings(normalizePath(path))
  if (get_os_type() == "windows") {
    path <- suppressWarnings(utils::shortPathName(path))
  }
  return(sub("[/\\]*$", "", path))
}
