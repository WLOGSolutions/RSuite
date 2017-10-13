#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'install' command or CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_utils.R"), chdir = T)

options <- c(
  make_option(c("-u", "--url"), dest = "url", default="http://wlog-rsuite.s3.amazonaws.com",
              help="Use provided url to search for RSuite package. (default: %default)"),
  make_option(c("-v", "--verbose"), dest = "verbose", action="store_true", default=FALSE,
              help="If passed lots of messages are written to console during installation.")
)

tryCatch({
  opts <- parse_args(OptionParser(option_list = options,
                                  usage = "rsuite install [options]",
                                  description = "Install RSuite package with all its required dependencies."),
                     args = commandArgs(trailingOnly = T)[-1] # reg rid of 'install' command
  )
}, error = function(e){
  .fatal_error(geterrmessage())
})

message("Detecting repositories ...")

cran_path <- (function() {
  official_repo_path <- getOption('repos') # This can point to CRAN mirror
  if ('@CRAN@' %in% official_repo_path) {
    official_repo_path <- getCRANmirrors()$URL[[1]]
  }
  return(official_repo_path)
})()
all_repos <- c(CRAN = cran_path, Other = opts$url)

message("Will use repositories:")
for(n in names(all_repos)) {
  message(sprintf("\t%10s = %s", n, all_repos[[n]]))
}

cli_ver <- suppressWarnings(readLines(file.path(base, "..", "version.txt")))
vbase <- gsub("^(\\d+[.]\\d+)[.-]\\d+$", "\\1", cli_ver)

message(sprintf("Installing RSuite(v%sx) package ...", vbase))

rsuite_pkg <- "RSuite"

wd <- setwd(.libPaths()[1]) # set wd to place there .Rprofile does not exist
tryCatch({
  # detect latest supported version to install
  rsuite_avails <- data.frame(utils::available.packages(repos = opts$url, filter = list()),
                              row.names = NULL, stringsAsFactors = F)
  ver_re <- sprintf("^%s[.-]", gsub("[.]", "[.]", vbase))
  rsuite_avails <- rsuite_avails[rsuite_avails$Package == rsuite_pkg & grepl(ver_re, rsuite_avails$Version), ]
  if (nrow(rsuite_avails) < 1) {
    .fatal_error(sprintf("Failed to detect RSuite(v%sx) package at %s", vbase, opts$url))
  }
  rsuite_avails <- rsuite_avails[order(rsuite_avails$Version, decreasing = T), ][1, ] # latest supported version
  
  # prepare local repository and download RSuite
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = T)
  on.exit({ unlink(tmp_dir, force = T, recursive = T) }, add = T)
  if (.Platform$pkgType != "source") {  
    bin_dir <- contrib.url(repos = tmp_dir, type = .Platform$pkgType)
    dir.create(bin_dir, recursive = T)
    write.dcf(NULL, file.path(bin_dir, "PACKAGES"))
  }
  
  src_dir <- utils::contrib.url(repos = tmp_dir, type = "source")
  dir.create(src_dir, recursive = T)
  dloaded <- utils::download.packages(rsuite_pkg, destdir = src_dir, available = rsuite_avails, repos = NULL,
                                      quiet = !opts$verbose)
  if (nrow(dloaded) != 1) {
    pkg_url <- sprintf("%s/%s", avails$Repository, paste(avails$File, collapse = " "))
    .fatal_error(sprintf("Failed to download RSuite package from %s", pkg_url))
  }
  tools::write_PACKAGES(dir = src_dir, type = "source")
  
  # install it from local repository, dependencies are from CRAN
  utils::install.packages("RSuite",
                          repos = c(CRAN = cran_path, Local = paste0("file:///", tmp_dir)),
                          quiet = !opts$verbose,
                          verbose = opts$verbose)
}, finally = { setwd(wd) })

if (!(rsuite_pkg %in% installed.packages()[, "Package"])) {
  .fatal_error(sprintf("Failed to install %s", rsuite_pkg))
}

message("All done.")
