#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'update' command or CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_utils.R"), chdir = T)

options <- c(
  make_option(c("-v", "--verbose"), dest = "verbose", action="store_true", default=FALSE,
              help="If passed lots of messages are written to console during update"),
  make_option(c("-q", "--quiet"), dest = "quiet", action="store_true", default=FALSE,
              help="Show only critical messages (default: %default)")
)

tryCatch({
  opts <- parse_args(OptionParser(option_list = options,
                                  usage = "rsuite update [options]",
                                  description = "Update RSuite CLI to newest version."),
                     args = commandArgs(trailingOnly = T)[-1] # reg rid of 'update' command
  )
}, error = function(e){
  .fatal_error(geterrmessage())
})

# setup logger
lev <- "INFO"
if (!is.null(opts$quiet) && any(opts$quiet)) {
  lev <- "CRITICAL"
} else if (!is.null(opts$verbose) && any(opts$verbose)) {
  lev <- "DEBUG"
}
suppressPackageStartupMessages({
  library(logging)
})

basicConfig(level = lev)

get_latest_release <- function(platform_id) {
  # check available versions
  loginfo("Retrieving exposed package index ...")

  pkg_idx_conn <- url("http://wlog-rsuite.s3.amazonaws.com/cli/PKG_INDEX", open = "r")
  tryCatch({
    pkg_idx_lines <- readLines(pkg_idx_conn)
  }, error = function(e) {
    .fatal_error(geterrmessage())
  }, finally = close(pkg_idx_conn))

  pkg_idx_lines <- trimws(pkg_idx_lines)
  plat_pkgs <- pkg_idx_lines[grepl(sprintf("^%s: ", platform_id), pkg_idx_lines)]
  if (length(plat_pkgs) < 1) {
    .fatal_error(sprintf("No RSuite CLI package for %s platform exposed", platform_id))
  }

  return(list(
    url = paste0("http://wlog-rsuite.s3.amazonaws.com/cli/", gsub("^[^:]+: ", "", plat_pkgs[1])),
    ver = gsub("^.+rsuitecli[-_]v?([\\.0-9]+)[-_].+$", "\\1", plat_pkgs[1], ignore.case = T)
  ))
}

platform_dict <- list(
  windows = list(
    get_platform_id = function() { sprintf("win-%s", .Platform$r_arch) },
    installer = "msiexec",
    get_install_args = function(url) { c("-i", url) }
  ),
  redhat = list(
    get_platform_id = function() { "rpm" },
    installer = "rpm",
    get_install_args = function(url) { c("-i", "--replacepkgs", url) }
  ),
  debian = list(
    get_platform_id = function() { "deb" },
    installer = "dpkg",
    get_install_args = function(url) {
      dest_file <- file.path(dirname(tempdir()), basename(url))
      errcode <- download.file(url, destfile = dest_file,
                               mode = "wb", quiet = any(opts$quiet))
      if (errcode != 0) {
        .fatal_error(sprintf("Failed to download package from %s.", url))
      }
      c("-i", dest_file)
    }
  )
)

platform <- .Platform$OS.type
if (platform == "unix") {
  if (file.exists("/etc/redhat-release")) {
    platform <- "redhat"
  } else if (file.exists("/etc/debian_version")) {
    platform <- "debian"
  } else {
    platform <- "unknown"
  }
}
if (!(platform %in% names(platform_dict))) {
  .fatal_error(sprintf("Update of RSuite CLI on '%s' platform is not suported.", platform))
}

platform_desc <- platform_dict[[platform]]

rel <- get_latest_release(platform_desc$get_platform_id())
curr_ver <- trimws(readLines(file.path(base, "..", "version.txt"), n = 1, warn = F))

ver_diff <- compareVersion(curr_ver, rel$ver)
if (ver_diff > 0) {
  loginfo("Development version(%s) of RSuite CLI installed. Will not update to released v%s",
          curr_ver, rel$ver)
  quit(save = "no", status = 0, runLast = FALSE)
}

if (ver_diff == 0) {
  loginfo("Latest version(%s) of RSuite CLI installed already.", curr_ver)
  quit(save = "no", status = 0, runLast = FALSE)
}

loginfo("Updating RSuite CLI from v%s to v%s ...", curr_ver, rel$ver)

inst_cmd <- Sys.which(platform_desc$installer)
if (nchar(inst_cmd) == 0) {
  .fatal_error(sprintf("%s utility is not available. It is required to install RSuite CLI package",
                       platform_desc$installer))
}

inst_args <- platform_desc$get_install_args(rel$url)

logdebug("running cmd: %s %s", inst_cmd, paste(inst_args, collapse = " "))
retcode <- suppressWarnings({
  system2(inst_cmd, args = inst_args)
})
if (retcode != 0) {
  .fatal_error(sprintf("Failed to install RSuite CLI v%s", rel$ver))
}

loginfo("RSuite CLI v%s successfuly installed", rel$ver)
