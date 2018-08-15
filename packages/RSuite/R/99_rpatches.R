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
  os_info <- get_os_info() # from 98_shell.R
  if (!(os_info$platform %in% c("RedHat", "Debian", "SunOS"))) {
    os_path <- sprintf("%s_%s", R.version$platform, R.version$arch)
    pkg_logwarn("Unknown platform %s. Only RedHat-like, Debian-like and SunOS are supported. Will use generic %s",
                os_info$platform, os_path)
  } else if (is.na(os_info$version)) {
    os_path = switch(os_info$platform,
                     RedHat = sprintf("rhel_%s", R.version$platform),
                     Debian = sprintf("deb_%s", R.version$platform),
                     SunOS  = sprintf("sol_%s", R.version$platrorm))
    pkg_logwarn("Could not detect %s(%s) version number. Will use generic %s",
                os_info$distrib, os_info$platform, os_path)
  } else {
    os_path = switch(os_info$platform,
                     RedHat = sprintf("rhel%s_%s", os_info$version, R.version$platform),
                     Debian = sprintf("deb%s_%s", os_info$version, R.version$platform),
                     SunOS = sprintf("sol%s_%s", os_info$version, R.version$platform),
                     NA_character_)
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
#' Converts passed path to full path and replaces it to short names on Windows. Short
#' argument specifies whether we want to convert the path to short names.
#'
#' @keywords internal
#' @noRd
#'
rsuite_fullUnifiedPath <- function(path, short = TRUE) {
  if (is.null(path) || length(path) == 0) {
    return(c())
  }
  path <- suppressWarnings(normalizePath(path))
  if (get_os_type() == "windows" && short) {
    path <- suppressWarnings(utils::shortPathName(path))
  }
  return(sub("[/\\]*$", "", path))
}


#'
#' Retrieve internal package object by name.
#'
#' @param package name of package to retrieve internal name from. (type: character(1))
#' @param name name to retrieve.  (type: character(1))
#'
#' @return object found in devtools namespace.
#'
#' @keywords internal
#' @noRd
#'
get_pkg_intern <- function(package, name) {
  search_res <- utils::getAnywhere(name)
  ixs <- which(search_res$where == sprintf("namespace:%s", package))
  if (!length(ixs)) {
    return()
  }
  ixs <- ixs[1]
  return(search_res$objs[[ixs]])
}

