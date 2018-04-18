#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities used by command management and RSuite install.
#----------------------------------------------------------------------------

.fatal_error <- function(messages) {
  for(m in messages) {
    write(sprintf("ERROR: %s", m), stderr())
  }
  quit(save = "no", status = 1, runLast = FALSE)
}
.base_dir <- normalizePath(".")
if (.Platform$OS.type == "windows") {
  .base_dir <- shortPathName(.base_dir)

  # get rid of tar from PATH as it breaks package building
  tar_path <- Sys.which('tar')
  if (file.exists(tar_path)) {
    path <- unlist(strsplit(Sys.getenv('PATH'), .Platform$path.sep))
    path <- path[gsub("\\\\$", "", path) != gsub('\\\\tar.exe$', '', tar_path)]
    Sys.setenv(PATH = paste(path, collapse = .Platform$path.sep))
  }
}

.usr_lib_path <- .libPaths()[1]
if (file.access(.usr_lib_path, 2) == -1) {
  .usr_lib_path <- Sys.getenv('R_LIBS_USER')
  if (!dir.exists(.usr_lib_path)) {
    message(sprintf("Creating user libs folder at %s", .usr_lib_path))
    dir.create(.usr_lib_path, recursive = T)
  }
  .libPaths(c(.usr_lib_path, .libPaths()))
}

verbose <- any(c('-v', '--verbose') %in% commandArgs(trailingOnly = T))
tryCatch({
  suppressWarnings({
    suppressPackageStartupMessages({
      if (!require(optparse)) {
        c_url <- sprintf("file:///%s", contrib.url(file.path(.base_dir, "packages"), type = "source"))
        install.packages(c("optparse", "getopt"), contriburl = c_url, type = "source", quiet = !verbose)
      }
      library(optparse)
    })
  })
}, error = function(e) {
  .fatal_error(c(geterrmessage(),
                 "optparse is required for RSuite CLI to work. Tried to install it but failed."))
})

tryCatch({
  suppressWarnings({
    suppressPackageStartupMessages({
      if (!require(logging)) {
        c_url <- sprintf("file:///%s", contrib.url(file.path(.base_dir, "packages"), type = "source"))
        install.packages(c("logging"), contriburl = c_url, type = "source", quiet = !verbose)
      }
      library(logging)
    })
  })
}, error = function(e) {
  .fatal_error(c(geterrmessage(),
                 "logging is required for RSuite CLI to work. Tried to install it but failed."))
})

(function(){
  tz <- suppressWarnings(Sys.timezone())
  if (is.na(tz) || tz == "unknown") {
    Sys.setenv(TZ = "UTC")
  }
})()

#'
#' Retrieves latest RSuite CLI version exposed on S3.
#'
#' @param platform_id one of rpm, deb, win-x32 or win-x64.
#' @return named list containing
#' \describe{
#'   \item{ver}{Version number of latest RSuite CLI exposed on S3 for the platform}
#'   \item{url}{Full url to retrieve installation package.}
#' }
#'
#' @keywords internal
#'
get_latest_release <- function(platform_id) {
  # check available versions
  loginfo("Retrieving RSuite CLI exposed on S3 package index ...")

  pkg_idx_conn <- url("http://wlog-rsuite.s3.amazonaws.com/cli/PKG_INDEX", open = "r")
  tryCatch({
    pkg_idx_lines <- readLines(pkg_idx_conn)
  }, error = function(e) {
    .fatal_error(geterrmessage())
  }, finally = close(pkg_idx_conn))

  pkg_idx_lines <- trimws(pkg_idx_lines)
  plat_pkgs <- pkg_idx_lines[grepl(sprintf("^%s: ", platform_id), pkg_idx_lines)]
  if (length(plat_pkgs) < 1) {
    .fatal_error(sprintf("No RSuite CLI package for %s platform exposed on S3", platform_id))
  }

  path <- gsub("^[^:]+: ", "", plat_pkgs[1])
  ver <- gsub("^.+rsuitecli[-_]v?([\\.0-9]+)[-_].+$", "\\1", plat_pkgs[1], ignore.case = T)
  loginfo("... found v%s (%s)", ver, path)
  return(list(
    url = paste0("http://wlog-rsuite.s3.amazonaws.com/cli/", path),
    ver = ver
  ))
}

#'
#' Retrieve platform specific package type.
#'
#' @param binary If TRUE retrieve binary type (type: logical)
#'
#' @return package type retrieved.
#'
#' @keywords internal
#'
get_pkg_type <- function(binary) {
  if (!binary) { return("source") }
  if (.Platform$pkgType != "source") { return(.Platform$pkgType) }
  return("binary")
}
