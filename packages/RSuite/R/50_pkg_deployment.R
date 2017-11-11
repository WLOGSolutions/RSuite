#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to downloading/building/instaling packages.
#----------------------------------------------------------------------------

#'
#' Downloads packages to the folder.
#'
#' @param avail_pkgs data frame containing packages to download of structure
#'   as returend by available.packages
#' @param dest_dir folder to download packages to. Folder must exist.
#'
#' @return data frame with columns Package and Path.
#'
#' @keywords internal
#'
pkg_download <- function(avail_pkgs, dest_dir) {
  stopifnot(is.data.frame(avail_pkgs) && "Package" %in% colnames(avail_pkgs))
  stopifnot(dir.exists(dest_dir))

  if (nrow(avail_pkgs) == 0) {
    return(data.frame(Package = character(), Path = character()))
  }

  avail_pkgs <- deduce_package_files(avail_pkgs) # from 51_pkg_info.R
  dloaded <- data.frame()

  common_args <- c(rscript_arg("destdir", dest_dir),
                   rscript_arg("repos", NULL),
                   rscript_arg("contriburl", NULL),
                   "quiet = FALSE")
  do_dload <- function(pkgs) {
    in_file <- tempfile(fileext = ".RData")
    save(pkgs, file = in_file)

    ou_file <- tempfile(fileext = ".RData")
    on.exit({ unlink(c(in_file, ou_file), force = T) }, add = T)

    build_result <- run_rscript(c("load(%s)",
                                  "remote_paths <- download.packages(pkgs$Package, available = pkgs, %s)",
                                  "save(remote_paths, %s)"),
                                rscript_arg("file", in_file),
                                paste(common_args, collapse = ", "),
                                rscript_arg("file", ou_file))
    if (!is.null(build_result)) {
      if (build_result == FALSE) {
        pkg_logwarn("Downloading aborted")
      } else {
        pkg_logwarn("Downloading failed: %s", build_result)
      }
    } else {
      load(ou_file)
      return(remote_paths)
    }
  }

  remote_pkgs <- avail_pkgs[!grepl("^file:///", avail_pkgs$Repository), ]
  if (nrow(remote_pkgs)) {
    remote_paths <- do_dload(remote_pkgs)
    remote_paths <- as.data.frame(remote_paths, stringsAsFactors = F)
    colnames(remote_paths) <- c("Package", "Path")

    dloaded <- rbind(dloaded, remote_paths)
  }

  local_pkgs <- avail_pkgs[grepl("^file:///", avail_pkgs$Repository), ]
  if (nrow(local_pkgs)) {
    # download.packages does not copy local files
    #   here they are copied manually
    local_paths <- do_dload(local_pkgs)
    local_paths <- as.data.frame(local_paths, stringsAsFactors = F)
    colnames(local_paths) <- c("Package", "Path")

    file.copy(from = local_paths$Path, to = dest_dir, overwrite = T)
    local_paths$Path <- file.path(dest_dir, basename(local_paths$Path))

    dloaded <- rbind(dloaded, local_paths[, c("Package", "Path")])
  }

  assert(all(avail_pkgs$Package %in% dloaded$Package),
         "Failed to download packages: %s",
         paste(setdiff(avail_pkgs$Package, dloaded$Package), collapse = ", "))

  return(dloaded)
}

#'
#' Build a single package.
#'
#' @param pkg_path path to package folder.
#' @param dest_dir folder to build the package to. Result archive will be put
#'   there so the folder must exist.
#' @param binary if TRUE will build binary package. (type: logical)
#' @param rver R version to build package with. (type: character)
#' @param libpath library path to use during building. (type: character)
#' @param pre_build_steps additional steps to perform before building. Should contain only
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#' }
#' If not passed will perform all pre_build_steps (type: character(N)).
#'
#' @return Full path to builded package or NULL if failed to build. (type: character)
#'
#' @keywords internal
#'
pkg_build <- function(pkg_path, dest_dir, binary, rver, libpath,
                      pre_build_steps = c("specs", "docs", "imps", "tests")) {
  stopifnot(length(pkg_path) == 1 && dir.exists(pkg_path))
  stopifnot(length(dest_dir) == 1 && dir.exists(dest_dir))

  pkg_name <- basename(pkg_path)
  pkg_path <- rsuite_fullUnifiedPath(pkg_path)
  dest_dir <- rsuite_fullUnifiedPath(dest_dir)
  libpath <- rsuite_fullUnifiedPath(libpath)

  if (dir.exists(file.path(libpath, pkg_name))) {
    unlink(file.path(libpath, pkg_name), recursive = T, force = T)
  }

  if ("specs" %in% pre_build_steps) {
    makevars <- file.path(pkg_path, "src",
                          ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars.in"))
    if (file.exists(makevars)) {
      spec_desc <- get_pkg_specifics(pkg_name, for_source = T, load_specifics())
      if (!is.null(spec_desc$Makefile)) {
        lines <- unlist(strsplit(spec_desc$Makefile[!is.na(spec_desc$Makefile)], "\\n", fixed = T))
        cat(lines, file = makevars, sep = "\n", append = T)
      }
    }
  }

  if ("docs" %in% pre_build_steps
      && !pkg_build_docs(pkg_name, pkg_path, rver, libpath)) { # from 54_pkg_document.R
    # package without documentation is unuseable due to NAMESPACE is not generated also
    return(NULL)
  }

  if ("imps" %in% pre_build_steps
      && !validate_package_imports(pkg_name, pkg_path)) {  # from 54_pkg_document.R
    return(NULL)
  }

  if ("tests" %in% pre_build_steps
      && devtools::uses_testthat(pkg = pkg_path)) {
    test_res <- run_rscript(c("test_results <- devtools::test(%s)",
                              "if (!testthat:::all_passed(test_results)) { stop('Tests failed') }"),
                            rscript_arg("pkg", pkg_path),
                            rver = rver, ex_libpath = libpath)
    if (!is.null(test_res)) {
      if (test_res == FALSE) {
        pkg_logwarn("Testing aborted for %s", pkg_name)
      } else {
        pkg_logwarn("Testing for %s failed: %s", pkg_name, test_res)
      }
    }
  }

  bld_args <- c("quiet = F",
                rscript_arg("pkg", pkg_path),
                rscript_arg("path", dest_dir),
                rscript_arg("binary", binary))
  ou_file <- tempfile(fileext = ".RData")
  on.exit(unlink(ou_file, force = T))

  bld_res <- run_rscript(c("library(devtools)",
                           ".libPaths(%s)",
                           "setwd(%s)", # to prevent loading .Rprofile by R CMD
                           "ou_path <- build(%s)",
                           "save(ou_path, %s)"),
                         rscript_arg("new", libpath),
                         rscript_arg("dir", libpath),
                         paste(bld_args, collapse = ", "),
                         rscript_arg("file", ou_file),
                         rver = rver)
  if (!is.null(bld_res)) {
    if (bld_res == FALSE) {
      pkg_logwarn("Building of %s aborted", pkg_name)
    } else {
      pkg_logwarn("Building of %s failed: %s", pkg_name, bld_res)
    }
    return(NULL)
  }

  ou_path <- NULL # to prevent warning on ou_path not visible
  load(ou_file)
  return(ou_path)
}

#'
#' Removes package or packages(and detaches it to be sure).
#'
#' @param pkgs packages to remove  (type: character).
#' @param lib_dir directory to reinstall packages in (type: character).
#'
#' @keywords internal
#'
pkg_remove <- function(pkgs, lib_dir) {
  void <- lapply(X = pkgs,
                 FUN = function(pkg) {
                   search_item <- paste("package", pkg, sep = ":")
                   while (search_item %in% search()) {
                     attch_pkg_path <- rsuite_fullUnifiedPath(system.file(package = pkg))
                     inlib_pkg_path <- rsuite_fullUnifiedPath(file.path(lib_dir, pkg))
                     if (attch_pkg_path != inlib_pkg_path) {
                       # if it's the same package loaded from another location
                       #  there is no point to detach it
                       break
                     }
                     detach(search_item, unload = TRUE, character.only = TRUE)
                   }
                 })
  installed <- installed.packages(lib_dir)[, "Package"]
  to_remove <- intersect(pkgs, installed)
  try (expr = suppressMessages(remove.packages(to_remove, lib = lib_dir)))
}


#'
#' Simple wrapped around utils::install.packages with consideration
#' of specific package pecularities.
#'
#' @keywords internal
#'
pkg_install <- function(pkgs, lib_dir, type, repos, rver) {
  common_args <- c(rscript_arg("lib", lib_dir), 'quiet = F')
  if (!missing(type)) {
    common_args <- c(common_args, rscript_arg("type", type))
  }
  if (!missing(repos)) {
    common_args <- c(common_args, rscript_arg("repos", repos))
  }

  spec_desc <- load_specifics()

  install_package <- function(pkg) {
    pkg_args <- c(common_args, rscript_arg("pkgs", pkg))
    pkg_args <- c(pkg_args, get_specific_args(pkg, spec_desc))

    build_result <- run_rscript(c("setwd(%s)", # to prevent loading .Rprofile by R CMD
                                  "utils::install.packages(%s)"),
                                rscript_arg("dir", lib_dir),
                                paste(pkg_args, collapse = ", "),
                                rver = rver)
    if (is.null(build_result)) {
      return(TRUE)
    }

    if (build_result == FALSE) {
      pkg_logwarn("Installation of package %s failed", basename(pkg))
    } else {
      pkg_logwarn("Building of %s failed: %s", basename(pkg), build_result)
    }
    return(FALSE)
  }

  lapply(X = pkgs,
         FUN = function(pkg) {
           bld_res <- install_package(pkg)
           pkg_name <- gsub("^([^_]+)_.+$", "\\1", basename(pkg))
           pkg_path <- file.path(lib_dir, pkg_name)
           if (bld_res && !dir.exists(pkg_path)) {
             pkg_logwarn("= Restarting build of %s (succeded but no folder created)", basename(pkg))
             bld_res <- install_package(pkg)
           }
           if (bld_res && dir.exists(pkg_path)) {
             # verify if package is built for proper R version
             pkg_rver <- get_package_build_rver(lib_dir, pkg_name)
             if (is.na(pkg_rver) || majmin_rver(pkg_rver) != majmin_rver(rver)) {
               pkg_logwarn("Package %s is succesfully installed but R version it is build for(%s) is not the one requested(%s).",
                           pkg_name, pkg_rver, rver)
               pkg_logwarn("It is propably cause of inconsistent repository state. Package %s will be deleted as it is unusable.",
                           pkg_name)
               unlink(pkg_path, recursive = T, force = T)
             }
           }
         })
  invisible()
}

#'
#' Loads specificts for the platform.
#'
#' @return data.frame describing all specificts
#'
#' @keywords internal
#'
load_specifics <- function() {
  spec_files <- c(
    system.file(file.path("extdata", "pkg_specifics", sprintf("%s.dcf", .Platform$OS.type)),
                package = "RSuite"),
    file.path(Sys.getenv("HOME"), ".rsuite", "pkg_specifics.dcf")
  )
  if (.Platform$OS.type == "unix") {
    spec_files <- c(spec_files, "/etc/.rsuite/pkg_specifics.dcf")
  }

  lines <- c()

  spec_files <- spec_files[file.exists(spec_files)]
  for(sf in spec_files) {
    sf_lines <- readLines(sf)
    sf_lines <- sf_lines[!grepl("^#", sf_lines)]

    tryCatch({
      read.dcf(textConnection(sf_lines))
      lines <- c(lines, sf_lines, "")
    }, error = function(e) {
      pkg_logwarn("Failed to read %s: %s", sf, e)
    })
  }
  if (!length(lines)) {
    return(data.frame())
  }

  dcf <- read.dcf(textConnection(lines))
  spec_desc <- data.frame(dcf, stringsAsFactors = F)
  return(spec_desc)
}

#'
#' Retrieves specifics for package passed.
#'
#' @param pkg_name name of package to retrieve specifics for. (type: character)
#' @param for_source if T retrieve specifics for source build. If NULL retrieve all (type: logical)
#' @param spec_desc data.frame describing specifics as returned by load_specifics.
#'
#' @return data.frame describing specifics for concrete package.
#'
#' @keywords internal
#'
get_pkg_specifics <- function(pkg_name, for_source, spec_desc) {
  spec_desc <- spec_desc[spec_desc$Package == pkg_name, ]
  if (!is.null(spec_desc$Arch)) {
    spec_desc <- spec_desc[is.na(spec_desc$Arch) | spec_desc$Arch == .Platform$r_arch, ]
  }
  if (!is.null(spec_desc$Source) && !is.null(for_source)) {
    spec_desc <- spec_desc[is.na(spec_desc$Source) | spec_desc$Source == for_source, ]
  }
  return(spec_desc)
}

#'
#' Builds specifics for concrete package file.
#'
#' @return character vector to add to install.packages script args.
#'
#' @keywords internal
#'
get_specific_args <- function(pkg_file, spec_desc) {
  pkg_file <- basename(pkg_file)
  pkg_name <- gsub("^([^_]+)_.+$", "\\1", pkg_file)

  spec_desc <- get_pkg_specifics(pkg_name,
                                 for_source = grepl("^[^_]+_.*[.]tar[.](gz|bz2|xz)$", pkg_file),
                                 spec_desc = spec_desc)

  collapse_non_na <- function(vals, sep) {
    vals <- Filter(x = vals, f = function(v) {!is.na(v) })
    if (!length(vals)) {
      return(NULL)
    }
    return(paste(vals, collapse = sep))
  }

  # check environment variables
  env_var_pats <- collapse_non_na(spec_desc$EnvVariablePatterns, ", ")
  if (!is.null(env_var_pats)) {
    env_var_pats <- trimws(unlist(strsplit(env_var_pats, split = ",")))
  }
  for(evp in env_var_pats) {
    if (!grepl("^[^[]+\\[.+\\]$", evp)) {
      pkg_logwarn("Invalid variable requirement for %s detected: %s", pkg_name, evp)
      next
    }
    var_name <- gsub("^([^[]+)\\[(.+)\\]$", "\\1", evp)
    var_pat  <- gsub("^([^[]+)\\[(.+)\\]$", "\\2", evp)

    match_res <- tryCatch({
      grepl(var_pat, Sys.getenv(var_name))
    }, error = function(e) {
      pkg_logwarn("Invalid variable requirement for %s(var: %s): %s", pkg_name, var_name, e)
      NULL
    })
    if (is.null(match_res)) {
      next
    }
    assert(match_res,
           "Environment variable %s does not match pattern(%s) required for %s package",
           var_name, var_pat, pkg_name)
  }

  return(c(
    rscript_arg("configure_vars", collapse_non_na(spec_desc$ConfigureVars, sep = " ")),
    rscript_arg("configure_args", collapse_non_na(spec_desc$ConfigureArgs, sep = " ")),
    rscript_arg("INSTALL_opts", collapse_non_na(spec_desc$InstallOpts, sep = " "))
  ))
}

#'
#' Retrieves R version package is built for.
#'
#' @param lib_dir path there package is installed. (type: character)
#' @param pkg_name name of package to get information for. (type: character)
#'
#' @return R version number the packages is built for.
#'
#' @keywords internal
#'
get_package_build_rver <- function(lib_dir, pkg_name) {
  installed <- data.frame(installed.packages(lib.loc = lib_dir), stringsAsFactors = F)[, c("Package", "Built")]
  if (!(pkg_name %in% installed$Package)) {
    return(NA)
  }
  pkg_rver <- installed[pkg_name == installed$Package, "Built"][1]
  return(pkg_rver)
}
