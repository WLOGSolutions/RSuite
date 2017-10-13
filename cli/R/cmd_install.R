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

get_cran_path <- function() {
  official_repo_path <- getOption('repos') # This can point to CRAN mirror
  if ('@CRAN@' %in% official_repo_path) {
    official_repo_path <- getCRANmirrors()$URL[[1]]
  }
  return(official_repo_path)
}
all_repos <- c(CRAN = get_cran_path(), Other = opts$url)

message("Will use repositories:")
for(n in names(all_repos)) {
  message(sprintf("\t%10s = %s", n, all_repos[[n]]))
}

cli_ver <- suppressWarnings(readLines(file.path(base_dir, "..", "version.txt")))
vbase <- gsub("^(\\d+[.]\\d+)[.-]\\d+$", "\\1", cli_ver)

message(sprintf("Installing RSuite(v%sx) package ...", vbase))

wd <- setwd(.libPaths()[1]) # set wd to place there .Rprofile does not exist
tryCatch({
  # detect latest supported version to install
  avails <- data.frame(utils::available.packages(repos = opts$url, filter = list()),
                       row.names = NULL, stringsAsFactors = F)
  ver_re <- sprintf("^%s[.-]", gsub("[.]", "[.]", vbase))
  avails <- avails[avails$Package == 'RSuite' & grepl(ver_re, avails$Version), ]
  if (nrow(avails) < 1) {
    .fatal_error(sprintf("Failed to detect RSuite(v%sx) package at %s", vbase, opts$url))
  }
  avails <- avails[order(avails$Version, decreasing = T), ][1, ] # latest supported version
  
  # download it
  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = T)
  on.exit({ unlink(tmp_dir, force = T, recursive = T) }, add = T)
  dloaded <- utils::download.packages("RSuite", destdir = tmp_dir, available = avails, 
                                      quiet = !opts$verbose)
  if (nrow(dloaded) != 1) {
    pkg_url <- sprintf("%s/%s", avails$Repository, paste(avails$File, collapse = " "))
    .fatal_error(sprintf("Failed to download RSuite package from %s", pkg_url))
  }
  
  # install it
  utils::install.packages(dloaded[1,2],
                          available = utils::available.packages(all_repos),
                          quiet = !opts$verbose,
                          verbose = opts$verbose)
}, finally = { setwd(wd) })
  
installed_pkgs <- installed.packages()[, "Package"]
uninstalled <- setdiff(required_pkgs, installed_pkgs)
if (length(uninstalled)) {
  .fatal_error(sprintf("Failed to install %s", paste(uninstalled, collapse = ", ")))
}

message("All done.")
