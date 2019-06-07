#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for building installation pack.
#----------------------------------------------------------------------------

#'
#' Wraps deployment zip into bash installer script.
#'
#' @details
#' Bash installer is just script containing also binary data at the end of file.
#' It has also some logic to install the package into more intelligent way than just
#' unzip.
#'
#' Shell script is created at the same location as zip package passed and will have
#' the same name with sh extension.
#'
#' @param zip_fpath path to zip package to wrap. It must exist. (type: character)
#' @return path to created bash installer script (invisible)
#'
#' @family in installer
#'
#' @examples
#' \donttest{
#'   inst_wrap_zip("myproj_0.1-1.zip") # creates myproj_0.1-1.sh
#' }
#' @export
#'
inst_wrap_zip <- function(zip_fpath) {
  assert(!is.null(zip_fpath) && file.exists(zip_fpath), "Existing file expected for 'zip_fpath'")
  dst_fpath <- gsub("[.]zip$", ".sh", zip_fpath)

  pkg_loginfo("Creating installation package into %s ...", dst_fpath)

  dst_con <- file(dst_fpath, "wb")
  on.exit(close(dst_con), add = TRUE)

  # installer header
  inst_fpath <- system.file("extdata/installer.sh", package = "RSuite")
  stopifnot(file.exists(inst_fpath))

  inst_lines <- readLines(inst_fpath, warn = FALSE)

  cat(paste(inst_lines, collapse = "\n"), file = dst_con)
  cat("\n", file = dst_con)
  flush(dst_con)

  # the package itself
  src_con <- file(zip_fpath, "rb")
  on.exit(close(src_con), add = TRUE)

  batch_size <- 5 * 1024 * 1024 # 5M
  while (TRUE) {
    in_dat <- readBin(src_con, raw(), n = batch_size)
    tryCatch(writeBin(in_dat, dst_con),
             error = function(e) {
               pkg_logerror("Failed to create installer package: %s", e)
               stop("Failed to create installer package; probably not enough disk space")
             })

    if (length(in_dat) < batch_size) {
      break
    }
  }
  flush(dst_con)

  pkg_loginfo("Creating installation package into %s ... done", dst_fpath)
  Sys.chmod(dst_fpath, "0755", use_umask = FALSE)
  return(invisible(dst_fpath))
}
