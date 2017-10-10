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


message("Installing RSuite package...")

required_pkgs <- c("RSuite")
wd <- setwd(.libPaths()[1]) # set wd to place there .Rprofile does not exist
tryCatch({
  utils::install.packages(required_pkgs,
                          repos = all_repos,
                          quiet = !opts$verbose,
                          verbose = opts$verbose)
}, finally = { setwd(wd) })
  
installed_pkgs <- installed.packages()[, "Package"]
uninstalled <- setdiff(required_pkgs, installed_pkgs)
if (length(uninstalled)) {
  .fatal_error(sprintf("Failed to install %s", paste(uninstalled, collapse = ", ")))
}

message("All done.")
