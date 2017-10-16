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

tryCatch({
  suppressWarnings({
    suppressPackageStartupMessages({
      if (!require(optparse)) {
        c_url <- sprintf("file:///%s", contrib.url(file.path(.base_dir, "packages"), type = "source"))
        install.packages(c("optparse", "getopt"), contriburl = c_url, type = "source", quiet = T)
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
        install.packages(c("logging"), contriburl = c_url, type = "source", quiet = T)
      }
      library(optparse)
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
