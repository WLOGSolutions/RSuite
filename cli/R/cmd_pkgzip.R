#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'pkgzip' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
if (grepl("darwin", R.version$os)) {
  base <- gsub("~\\+~", " ", base) # on MacOS ~+~ in path denotes whitespace
}
source(file.path(base, "command_mgr.R"), chdir = T)

.get_pkg_type <- function(binary) {
  if (!binary) { return("source") }
  if (.Platform$pkgType != "source") { return(.Platform$pkgType) }
  return("binary")
}

.common_options <- list(
  make_option(c("-p", "--path"), dest = "path", default = getwd(),
              help = "Place created PKGZIP into path provided. (default: %default)")
)

sub_commands <- list(
  proj = list(
    help = "Create PKGZIP out of project packages.",
    options = c(
      make_option(c("-n", "--names"), dest = "pkgs",
                  help = paste("Comma separated list of project package names to include in PKGZIP.",
                               "If none passed will include all project packages",
                               sep = "\n\t\t")),
      make_option(c("--version"), dest = "version",
                  help = paste("Tag project packages with the version before including in PKGZIP.",
                               "By default will use ZipVersion from project PARAMETERS and revision control.",
                               "Version is expected in form NN.NN.",
                               sep = "\n\t\t")),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help = "Include binary form of project packages (default: %default)"),
      make_option(c("--with-deps"), dest = "with_deps", action="store_true", default=FALSE,
                  help = "If passed will include dependencies into PKGZIP (default: %default)"),
      make_option(c("--filter-repo"), dest = "filter_repo",
                  help = paste("Url to repository. If passed will not include dependencies into PKGZIP",
                               "which satisfying versions are present in the repository.",
                               "The parameter should be used together with --with-deps.",
                               sep = "\n\t\t")),
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
                                        path = opts$path,
                                        with_deps = opts$with_deps,
                                        filter_repo = opts$filter_repo)
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
                  help = "Include binary form of external packages (default: %default)"),
      make_option(c("--with-deps"), dest = "with_deps", action="store_true", default=FALSE,
                  help = "If passed will include dependencies into PKGZIP (default: %default)"),
      make_option(c("--filter-repo"), dest = "filter_repo",
                  help = paste("Url to repository. If passed will not include dependencies into PKGZIP",
                               "which satisfying versions are present in the repository.",
                               "The parameter should be used together with --with-deps.",
                               sep = "\n\t\t")),
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
                                        path = opts$path,
                                        with_deps = opts$with_deps,
                                        filter_repo = opts$filter_repo)
    }
  ),
  github = list(
    help = "Create PKGZIP out of package build from GitHub.",
    options = c(
      make_option(c("-r", "--repo"), dest = "repo",
                  help = "Repository to upload from in form username/repo[/subdir][@ref|#pull]."),
      make_option(c("-H", "--host"), dest = "host", default="https://api.github.com",
                  help = paste("GitHub API host to use. Override with your GitHub enterprise hostname,",
                               "for example, 'github.hostname.com/api/v3'",
                               sep = "\n\t\t")),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help = "Build and upload binary package (default: %default)"),
      make_option(c("--with-deps"), dest = "with_deps", action="store_true", default=FALSE,
                  help = "If passed will include dependencies into PKGZIP (default: %default)"),
      make_option(c("--filter-repo"), dest = "filter_repo",
                  help = paste("Url to repository. If passed will not include dependencies into PKGZIP",
                              "which satisfying versions are present in the repository.",
                              "The parameter should be used together with --with-deps.",
                              sep = "\n\t\t")),
      make_option(c("--skip-build-steps"), dest = "skip_build_steps", default=as.character(NULL),
                  help = paste("Comma separated list of steps to skip while building the package.",
                               "Following values are accepted:",
                               "\t specs - not process specifics",
                               "\t docs  - do not build documentation with roxygen",
                               "\t imps  - do not perform imports validation",
                               "\t tests - do not run package tests",
                               "\t rcpp_attribs - do not run Rcpp attributes compilation on the package",
                               "\t vignettes - do not run vignettes building",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option(c("--keep-sources"), dest = "keep_sources", action="store_true", default=FALSE,
                  help = "If passed will not remove temporary project used to build the package (default: %default)"),
      .common_options
    ),
    run = function(opts) {
      if (is.null(opts$repo)) {
        stop("--repo option is required")
      }

      pkg_type <- .get_pkg_type(opts$binary)
      RSuite::pkgzip_build_github_package(repo = opts$repo, host = opts$host,
                                          pkg_type = pkg_type,
                                          path = opts$path,
                                          with_deps = opts$with_deps,
                                          filter_repo = opts$filter_repo,
                                          skip_build_steps = unlist(strsplit(opts$skip_build_steps, ",")),
                                          keep_sources = opts$keep_sources)
    }
  ),
  bioc = list(
    help = "Create PKGZIP out of package build from BioConductor.",
    options = c(
      make_option(c("-r", "--repo"), dest = "repo",
                  help = "Repository to upload from in form [username:password@][release/]repo[#revision]."),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help = "Build and upload binary package (default: %default)"),
      make_option(c("--filter-repo"), dest = "filter_repo",
                  help = paste("Url to repository. If passed will not include dependencies into PKGZIP",
                               "which satisfying versions are present in the repository.",
                               "The parameter should be used together with --with-deps.",
                               sep = "\n\t\t")),
      make_option(c("--skip-build-steps"), dest = "skip_build_steps", default=as.character(NULL),
                  help = paste("Comma separated list of steps to skip while building the package.",
                               "Following values are accepted:",
                               "\t specs - not process specifics",
                               "\t docs  - do not build documentation with roxygen",
                               "\t imps  - do not perform imports validation",
                               "\t tests - do not run package tests",
                               "\t rcpp_attribs - do not run Rcpp attributes compilation on the package",
                               "\t vignettes - do not run vignettes building",
                               "(default: %default)",
                               sep = "\n\t\t")),
      make_option(c("--keep-sources"), dest = "keep_sources", action="store_true", default=FALSE,
                  help = "If passed will not remove temporary project used to build the package (default: %default)"),
      .common_options
    ),
    run = function(opts) {
      if (is.null(opts$repo)) {
        stop("--repo option is required")
      }

      pkg_type <- .get_pkg_type(opts$binary)
      RSuite::pkgzip_build_bioc_package(repo = opts$repo,
                                        pkg_type = pkg_type,
                                        path = opts$path,
                                        with_deps = FALSE,
                                        filter_repo = opts$filter_repo,
                                        skip_build_steps = unlist(strsplit(opts$skip_build_steps, ",")),
                                        keep_sources = opts$keep_sources)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you create PKGZIP packages."
)
