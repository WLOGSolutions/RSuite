#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'repo' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)

common_options <- list(
  make_option(c("-d", "--dir"), dest = "dir",
              help="Use passed in directory repository to upload files to."),
  make_option(c("-s", "--s3_url"), dest = "s3_url",
              help="Use passed URL to S3 repository to upload files to.")
)

start_management <- function(opts, rver, types) {
  if (length(c(opts$dir, opts$s3_url)) != 1) {
    stop("Exactly one option out of --dir and --s3_url must be specified")
  }

  # rver <- opts$rver
  if (is.null(rver)) {
    rver <- RSuite:::current_rver()
  }

  if (!is.null(opts$dir)) {
    path <- tryCatch(normalizePath(opts$dir, mustWork = T),
                     error = function(e) { NULL })
    if (is.null(path)) {
      stop(sprintf("Repository directory does not exist: %s", opts$dir))
    }
    return(RSuite::repo_mng_start("Dir", path = path, rver = rver, types = types))
  }
  if (!is.null(opts$s3_url)) {
    if (.Platform$OS.type == "windows") {
      # fix HOME
      Sys.setenv(HOME=Sys.getenv("USERPROFILE"))
    }
    return(RSuite::repo_mng_start("S3", url = opts$s3_url, rver = rver, types = types))
  }
  stop("Neither --dir nor --s3_url specified")
}

get_pkg_type <- function(binary) {
  if (!binary) { return("source") }
  if (.Platform$pkgType != "source") { return(.Platform$pkgType) }
  return("binary")
}

sub_commands <- list(
  addproj = list(
    help = "Upload project packages into repository.",
    options = c(
      make_option(c("-n", "--names"), dest = "names",
                  help = paste0("Comma separated list of project package names to upload into repository.",
                                " If none passed will upload all project packages")),
      make_option(c("--skip_rc"), dest = "skip_rc", action="store_true", default=FALSE,
                  help="Do not tag packages with revision number (default: %default)"),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Upload binary form of project packages (default: %default)"),
      common_options
    ),
    run = function(opts) {
      rver <- RSuite::prj_init()$load_params()$r_ver # use version from package
      pkg_type <- get_pkg_type(opts$binary)
      ra_mng <- start_management(opts, rver = rver, types = pkg_type)

      pkgs <- NULL
      if (!is.null(opts$names)) {
        pkgs <- unlist(strsplit(opts$names, split=","))
      }

      RSuite::repo_upload_prj_packages(repo_manager = ra_mng,
                                       pkgs = pkgs,
                                       skip_rc = opts$skip_rc,
                                       pkg_type = pkg_type)
      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  addfile = list(
    help = "Upload package files into repository.",
    options = c(
      make_option(c("-f", "--files"), dest = "files", metavar = "FILES",
                  help = "Comma separated list of package files to upload into repository."),
      common_options
    ),
    run = function(opts) {
      if (is.null(opts$files)) {
        stop("--files option is required")
      }
      pkg_infos <- RSuite:::get_package_files_info(opts$files)

      rver <- unique(pkg_infos$RVersion)
      rver <- rver[!is.na(rver)]
      if (length(rver) > 1) {
        stop(paste0("Packages for multiple R version cannot be added together.",
                    sprintf(" R Versions detected: %s", paste(rver, collapse = ", "))))
      } else if (length(rver) == 0) {
        rver <- RSuite:::current_rver()
      }

      ra_mng <- start_management(opts, rver = rver, types = unique(pkg_infos$Type))

      files <- unlist(strsplit(opts$files, split=","))
      RSuite::repo_upload_package_files(repo_manager = ra_mng, files = files)

      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  addext = list(
    help = paste0("Upload external packages by name into repository. Will look for",
                  " packages in repositories of project in context."),
    options = c(
      make_option(c("-n", "--names"), dest = "pkgs", metavar = "PACKAGES",
                  help = "Comma separated list of external packages to upload into repository."),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Upload binary form of external packages (default: %default)"),
      make_option(c("--rver"), dest = "rver", default=NULL,
                  help="R version to upload package for. If NULL current R version will be assumed. (default: %default)"),
      common_options
    ),
    run = function(opts) {
      if (is.null(opts$pkgs)) {
        stop("--names option is required")
      }

      pkg_type <- get_pkg_type(opts$binary)
      ra_mng <- start_management(opts, rver = opts$rver, types = pkg_type)

      pkgs <- unlist(strsplit(opts$pkgs, split=","))
      RSuite::repo_upload_ext_packages(repo_manager = ra_mng,
                                       pkgs = pkgs,
                                       pkg_type = pkg_type)

      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  addpkgzip = list(
    help = "Upload contents of PKGZIP into repository.",
    options = c(
      make_option(c("-z", "--pkgzip"), dest = "pkgzip", metavar = "PKGZIP",
                  help = "PKGZIP to upload into repository."),
      common_options
    ),
    run = function(opts) {
      if (is.null(opts$pkgzip)) {
        stop("--pkgzip option is required")
      }
      if (!file.exists(opts$pkgzip)) {
        stop(sprintf("file does not exist %s", opts$pkgzip))
      }

      pkgzip_info <- RSuite:::get_pkgzip_info(opts$pkgzip)
      rver <- unique(pkgzip_info$RVersion)
      rver <- c(rver[!is.na(rver)], RSuite:::current_rver())[1]
      types <- unique(pkgzip_info$Type)

      ra_mng <- start_management(opts, rver = rver, types = types)

      RSuite::repo_upload_pkgzip(repo_manager = ra_mng, pkgzip = opts$pkgzip)

      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  addgithub = list(
    help = "Upload package build from GitHub into repository.",
    options = c(
      make_option(c("-r", "--repo"), dest = "repo",
                  help = "Repository to upload from in form username/repo[/subdir][@ref|#pull]."),
      make_option(c("-H", "--host"), dest = "host", default="https://api.github.com",
                  help = paste0("GitHub API host to use. Override with your GitHub enterprise hostname,",
                                " for example, 'github.hostname.com/api/v3'")),
      make_option(c("-b", "--binary"), dest = "binary", type="logical", default=(.Platform$pkgType != "source"),
                  help="Build and upload binary package (default: %default)"),
      make_option(c("--rver"), dest = "rver", default=NULL,
                  help = paste0("R version to build and upload package for. If NULL current R version will",
                                " be assumed. (default: %default)")),
      common_options
    ),
    run = function(opts) {
      if (is.null(opts$repo)) {
        stop("--repo option is required")
      }

      pkg_type <- get_pkg_type(opts$binary)
      ra_mng <- start_management(opts, rver = opts$rver, types = pkg_type)

      RSuite::repo_upload_github_package(ra_mng, repo = opts$repo, host = opts$host,
                                         pkg_type = pkg_type)
      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  list = list(
    help = "Retrieve list of packages available in repository.",
    options = c(
      make_option(c("-b", "--binary"), dest = "binary", type = "logical", default = (.Platform$pkgType != "source"),
                  help = "Retrieve list of binary packages (default: %default)"),
      make_option(c("-c", "--cache"), dest = "cache", type = "logical", default = TRUE,
                  help = "If FALSE will delete cached list before retrieving (default: TRUE)"),
      common_options
    ),
    run = function(opts) {
      pkg_type <- get_pkg_type(opts$binary)

      ra_mng <- start_management(opts, rver = NULL, types = pkg_type)

      avail_pkgs <- RSuite::repo_mng_list(ra_mng, pkg_type, !opts$cache)
      write(sprintf("%-40s%-40s", "Package", "Version"), stdout())
      by(avail_pkgs, 1:nrow(avail_pkgs), function(row) write(sprintf("%-40s%-40s", row$Package, row$Version), stdout()))

      RSuite::repo_mng_stop(ra_mng)
    }
  ),
  remove = list(
    help = "Removes specified packages from repository.",
    options = c(
      make_option(c("-r", "--toremove"), dest = "toremove", type = "character",
                  help = "Comma separated list of Package==Version pairs to remove from repository."),
      make_option(c("-b", "--binary"), dest = "binary", type = "logical", default = (.Platform$pkgType != "source"),
                  help = "Retrieve list of binary packages (default: %default)"),
      common_options
    ),
    run = function(opts) {
      if (is.null(opts$toremove)) {
        stop("--toremove option is required")
      }

      pkgs <- c()
      vers <- c()
      for (p in unlist(strsplit(opts$toremove, split=","))) {
        pair <- unlist(strsplit(p, split = "=="))

        pkgs <- c(pkgs, pair[1])
        vers <- c(vers, pair[2])
      }
      toremove <- data.frame(Package = pkgs, Version = vers)

      pkg_type <- get_pkg_type(opts$binary)

      ra_mng <- start_management(opts, rver = NULL, types = pkg_type)

      RSuite::repo_mng_remove(ra_mng, toremove, pkg_type = pkg_type)

      RSuite::repo_mng_stop(ra_mng)
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you manage R repositories."
)
