#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'pkgzip' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)

.get_pkg_type <- function(binary) {
  if (!binary) { return("source") }
  if (.Platform$pkgType != "source") { return(.Platform$pkgType) }
  return("binary")
}

.common_options <- list(
  make_option(c("-p", "--path"), dest = "path", default = getwd(),
              help="Place created PKGZIP into path provided. (default: %default)")
)  

sub_commands <- list(
  proj = list(
    help = "Create PKGZIP out of project packages.",
    options = c(
      make_option(c("-n", "--names"), dest = "pkgs",
                  help = paste0("Comma separated list of project package names to include in PKGZIP.",
                                " If none passed will include all project packages")),
      make_option(c("--version"), dest = "version",
                  help=paste0("Tag project packages with the version before including in PKGZIP.",
                              " By default will use ZipVersion from project PARAMETERS and revision control.",
                              " Version is expected in form NN.NN.")),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Include binary form of project packages (default: %default)"),
      .common_options
    ),
    run = function(opts) {
      pkgs <- NULL
      if (!is.null(opts$pkgs)) {
        pkgs <- unlist(strsplit(opts$pkgs, split=","))
      }

      pkg_type <- .get_pkg_type(opts$binary)
      RSuite::pkgzip_build_prj_packages(pkgs = pkgs,
                                        zip_ver = opts$version,
                                        pkg_type = pkg_type,
                                        path = opts$path)
    }
  ),
  file = list(
    help = "Create PKGZIP out of in files packages.",
    options = c(
      make_option(c("-f", "--files"), dest = "files", metavar = "FILES",
                  help = "Comma separated list of package files to include in PKGZIP."),
      .common_options
    ),
    run = function(opts) {
      if (is.null(opts$files)) {
        stop("--files option is required")
      }

      files <- unlist(strsplit(opts$files, split=","))

      RSuite::pkgzip_build_package_files(files = files, path = opts$path)
    }
  ),
  ext = list(
    help = "Create PKGZIP out of external packages.",
    options = c(
      make_option(c("-n", "--names"), dest = "pkgs", metavar = "PACKAGES",
                  help = "Comma separated list of external packages to include in PKGZIP."),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Include binary form of external packages (default: %default)"),
      .common_options
    ),
    run = function(opts) {
      if (is.null(opts$pkgs)) {
        stop("--names option is required")
      }

      pkgs <- unlist(strsplit(opts$pkgs, split=","))

      pkg_type <- .get_pkg_type(opts$binary)
      RSuite::pkgzip_build_ext_packages(pkgs,
                                        pkg_type = pkg_type,
                                        path = opts$path)
    }
  ),
  github = list(
    help = "Create PKGZIP out of package build from GitHub.",
    options = c(
      make_option(c("-r", "--repo"), dest = "repo",
                  help = "Repository to upload from in form username/repo[/subdir][@ref|#pull]."),
      make_option(c("-H", "--host"), dest = "host", default="https://api.github.com",
                  help = paste0("GitHub API host to use. Override with your GitHub enterprise hostname,",
                                " for example, 'github.hostname.com/api/v3'")),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Build and upload binary package (default: %default)"),
      .common_options
    ),
    run = function(opts) {
      if (is.null(opts$repo)) {
        stop("--repo option is required")
      }

      pkg_type <- .get_pkg_type(opts$binary)
      RSuite::pkgzip_build_github_package(repo = opts$repo, host = opts$host, 
                                          pkg_type = pkg_type, 
                                          path = opts$path)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you create PKGZIP packages."
)
