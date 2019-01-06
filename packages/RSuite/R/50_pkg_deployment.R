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
#' @noRd
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
                   rscript_arg("repos", ""),
                   rscript_arg("contriburl", ""),
                   "quiet = FALSE")
  do_dload <- function(pkgs) {
    in_file <- tempfile(fileext = ".RData")
    save(pkgs, file = in_file)

    ou_file <- tempfile(fileext = ".RData")
    on.exit({
      unlink(c(in_file, ou_file), force = TRUE)
    },
    add = TRUE)

    build_result <- run_rscript(c("load(%s)",
                                  "remote_paths <- matrix(character(), 0L, 2L)",
                                  paste0("for (pIx in seq_len(nrow(pkgs))) {",
                                         "prow <- pkgs[pIx,];",
                                         "tryCatch({",
                                         "res <- download.packages(prow$Package, available = prow, %s); ",
                                         "remote_paths <- rbind(remote_paths, res)}, ",
                                         "warning = function(w) {}, ",
                                         "error = function(e) {})",
                                         "}"),
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
      return(data.frame(Package = character(0), Version = character(0), stringsAsFactors = FALSE))
    }

    remote_paths <- NULL # just to prevent warning: set in load below
    load(ou_file)

    remote_paths <- as.data.frame(remote_paths, stringsAsFactors = FALSE)
    colnames(remote_paths) <- c("Package", "Path")

    return(remote_paths)
  }

  remote_pkgs <- avail_pkgs[!is_local_url(avail_pkgs$Repository), ] # from 99_rpatches.R
  local_pkgs <- data.frame()
  if (nrow(remote_pkgs)) {
    # check/build download cache
    dload_cache_dir <- get_cache_dir("dload_cache") # from 98_shell.R
    if (!is.null(dload_cache_dir)) {
      remote_pkgs$CacheFile <-
        file.path(dload_cache_dir,
                  vapply(X = remote_pkgs$Repository,
                         FUN = function(repo) utils::URLencode(repo, TRUE),
                         FUN.VALUE = ""),
                  remote_pkgs$File)

      cache_exists <- file.exists(remote_pkgs$CacheFile)
      if (!all(cache_exists)) {
        # Cache files which are not present in cache
        to_dload_pkgs <- remote_pkgs[!cache_exists, ]
        remote_paths <- do_dload(to_dload_pkgs)
        dloaded_pkgs <- merge(remote_paths, remote_pkgs, by = "Package", all = FALSE)

        try({
          # cache them
          suppressWarnings({
            lapply(X = unique(dirname(dloaded_pkgs$CacheFile)),
                   FUN = function(dir_path) dir.create(dir_path, recursive = TRUE, showWarnings = FALSE))
            file.copy(from = dloaded_pkgs$Path, to = dloaded_pkgs$CacheFile, overwrite = TRUE)
          })
        },
        silent = TRUE)

        dload_failed_pkgs <- setdiff(to_dload_pkgs$Package, dloaded_pkgs$Package)
        assert(length(dload_failed_pkgs) == 0,
               "Failed to download packages: %s", paste(dload_failed_pkgs, collapse = ", "))
      } else {
        # All files are cached: available locally
        remote_paths <- data.frame()
      }

      if (any(cache_exists)) {
        local_pkgs <- remote_pkgs[cache_exists, ]
        local_pkgs$Repository <- path2local_url(dirname(local_pkgs$CacheFile)) # from 99_rpatches.R
        local_pkgs$CacheFile <- NULL # ret rid of extra column
        pkg_logdebug(sprintf("Will use '%s' from cached %s", local_pkgs$Package, local_pkgs$File))
      }
    } else {
      # Caching is off: just download them
      remote_paths <- do_dload(remote_pkgs)

      dload_failed_pkgs <- setdiff(remote_pkgs$Package, remote_paths$Package)
      assert(length(dload_failed_pkgs) == 0,
             "Failed to download packages: %s", paste(dload_failed_pkgs, collapse = ", "))
    }

    dloaded <- rbind(dloaded, remote_paths)
  }

  local_pkgs <- rbind(local_pkgs, avail_pkgs[is_local_url(avail_pkgs$Repository), ]) # from 99_rpatches.R
  if (nrow(local_pkgs)) {
    # download.packages does not copy local files
    #   here they are copied manually
    local_paths <- local_pkgs
    local_paths$Path <- file.path(local_url2path(local_pkgs$Repository), local_pkgs$File)

    file.copy(from = local_paths$Path, to = dest_dir, overwrite = TRUE)
    local_paths$Path <- file.path(dest_dir, basename(local_paths$Path))

    dloaded <- rbind(dloaded, local_paths[, c("Package", "Path")])
  }

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
#' @param sboxpath library path there support packages are placed. (type: character)
#' @param skip_build_steps steps to not perform before building. Can contain only
#' \describe{
#'   \item{specs}{Process packages specifics}
#'   \item{docs}{Try build documentation with roxygen}
#'   \item{imps}{Perform imports validation}
#'   \item{tests}{Run package tests}
#'   \item{rcpp_attribs}{Run rppAttribs on package}
#'   \item{vignettes}{Build package vignettes}
#' }
#' (type: character(N)).
#'
#' @return Full path to builded package or NULL if failed to build. (type: character)
#'
#' @keywords internal
#' @noRd
#'
pkg_build <- function(pkg_path, dest_dir, binary, rver, libpath, sboxpath, skip_build_steps = NULL) {
  stopifnot(length(pkg_path) == 1 && dir.exists(pkg_path))
  stopifnot(length(dest_dir) == 1 && dir.exists(dest_dir))

  pkg_name <- basename(pkg_path)
  pkg_path <- rsuite_fullUnifiedPath(pkg_path)
  dest_dir <- rsuite_fullUnifiedPath(dest_dir)
  libpath <- rsuite_fullUnifiedPath(libpath)
  sboxpath <- rsuite_fullUnifiedPath(sboxpath)

  # remove previous build package version if exists
  unlink(list.files(dest_dir, pattern = paste0(pkg_name, "_.*"), full.names = TRUE),
         force = TRUE)

  if ("specs" %in% skip_build_steps) {
    pkg_loginfo("Skipping specifics application")
  } else {
    makevars <- file.path(pkg_path, "src",
                          ifelse(get_os_type() == "windows", "Makevars.win", "Makevars.in"))
    if (file.exists(makevars)) {
      spec_desc <- get_pkg_specifics(pkg_name, for_source = TRUE, load_specifics())
      if (!is.null(spec_desc$Makefile)) {
        lines <- unlist(strsplit(spec_desc$Makefile[!is.na(spec_desc$Makefile)], "\\n", fixed = TRUE))
        cat(lines, file = makevars, sep = "\n", append = TRUE)
      }
    }
  }

  if ("docs" %in% skip_build_steps) {
    pkg_loginfo("Skipping documentation building")
  } else if (!pkg_build_docs(pkg_name, pkg_path, # from 54_pkg_document.R
                             rver, libpath, sboxpath)) {
    # package without documentation is unuseable due to NAMESPACE is not generated also
    return(NULL)
  }

  if ("imps" %in% skip_build_steps) {
    pkg_loginfo("Skipping imports validation")
  } else if (!validate_package_imports(pkg_name, # from 54_pkg_document.R
                                       pkg_path)) {
    return(NULL)
  }

  if ("rcpp_attribs" %in% skip_build_steps) {
    pkg_loginfo("Skipping Rcpp attributes compilation")
    # if devtools version is below 2.0.0 we need to Skip Rcpp attributes inside devtools
    # for devtools > 2.0.1 it uses pkgbuild package which does not build Rcpp attributes by defailt
    rcpp_attribs_handle_cmd <- paste(
      "if (compareVersion(as.character(packageVersion('devtools')), '2.0.0') < 0) {",
      "  assignInNamespace('compile_rcpp_attributes',",
      "    function(pkg) { cat('Skipping Rcpp::compileAttributes\\n' )},",
      "    'devtools')",
      "}")
  } else {
    rcpp_attribs_handle_cmd <- paste(
      "if (compareVersion(as.character(packageVersion('devtools')), '2.0.0') >= 0) {",
      sprintf("pkgbuild:::compile_rcpp_attributes(%s)", rscript_arg("path", pkg_path)),
      "}")
  }

  vign_cleanup <- NULL
  if ("vignettes" %in% skip_build_steps) {
    pkg_loginfo("Skipping vignettes building")
  } else {
    vign_cleanup <- pkg_build_vignettes(pkg_name, pkg_path, # from 54_pkg_document.R
                                        rver, ex_libpath = c(libpath, sboxpath))
  }
  on.exit({
    if (!is.null(vign_cleanup)) vign_cleanup()
  },
  add = TRUE)

  if ("tests" %in% skip_build_steps) {
    pkg_loginfo("Skipping package testing")
  } else if (devtools::uses_testthat(pkg = pkg_path)) {
    # it roughly builds package before testing, so skiping Rcpp attribs handling here also required
    test_res <- run_rscript(c(rcpp_attribs_handle_cmd,
                              "test_results <- devtools::test(%s)",
                              "if (!testthat:::all_passed(test_results)) { stop('Tests failed') }"),
                            rscript_arg("pkg", pkg_path),
                            rver = rver, ex_libpath = c(sboxpath, libpath))
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
  on.exit(unlink(ou_file, force = TRUE), add = TRUE)

  bld_res <- run_rscript(c("library(devtools)",
                           "library(rstudioapi)", # this is required for devtools >= 2.0.1
                           ".libPaths(%s)",
                           "setwd(%s)", # to prevent loading .Rprofile by R CMD
                           rcpp_attribs_handle_cmd,
                           "ou_path <- build(%s, vignettes = FALSE)",
                           "save(ou_path, %s)"),
                         rscript_arg("new", libpath),
                         rscript_arg("dir", libpath),
                         paste(bld_args, collapse = ", "),
                         rscript_arg("file", ou_file),
                         rver = rver,
                         ex_libpath = c(sboxpath, libpath))
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

  if (!file.exists(ou_path)) {
    pkg_logwarn(paste("R reported package(%s) build succeeded but no package file created;",
                      "Verify if tools are available (like zip)"),
                pkg_name)
    return(NULL)
  }
  return(ou_path)
}

#'
#' Removes package or packages(and detaches it to be sure).
#'
#' @param pkgs packages to remove  (type: character).
#' @param lib_dir directory to reinstall packages in (type: character).
#'
#' @return logical vertor of same size as pkgs with FALSE on positions
#'  which failed to remove.
#'
#' @keywords internal
#' @noRd
#'
pkg_remove <- function(pkgs, lib_dir) {
  to_remove <- lapply(X = pkgs,
                 FUN = function(pkg) {
                   dll_path <- get_package_dll_path(lib_dir, pkg)
                   if (!is.null(dll_path)) {
                     loaded_paths <- vapply(getLoadedDLLs(),
                                            FUN = function(dl) dl[["path"]],
                                            FUN.VALUE = "")
                     if (dll_path %in% loaded_paths) {
                       library.dynam.unload(pkg, file.path(lib_dir, pkg))
                     }

                     if (unlink(dll_path, force = TRUE) != 0) {
                       return(FALSE)
                     }
                   }

                   search_item <- paste("package", pkg, sep = ":")
                   while (search_item %in% search()) {
                     attch_pkg_path <- rsuite_fullUnifiedPath(system.file(package = pkg))
                     inlib_pkg_path <- rsuite_fullUnifiedPath(file.path(lib_dir, pkg))
                     if (attch_pkg_path != "" && attch_pkg_path != inlib_pkg_path) {
                       # if attch_pkg_path is empty, package folder was deleted partially
                       # if it's the same package loaded from another location
                       #  there is no point to detach it
                       break
                     }

                     detach(search_item, unload = TRUE, character.only = TRUE,
                            force = TRUE) # force even if dependent packages are loaded
                   }

                   return(TRUE)
                 })
  to_remove <- unlist(to_remove)

  installed <- utils::installed.packages(lib_dir)[, "Package"]
  locked_pkgs <- intersect(pkgs[!to_remove], installed)

  remove_pkgs <- intersect(pkgs[to_remove], installed)
  try (expr = suppressMessages(utils::remove.packages(remove_pkgs, lib = lib_dir)))

  still_installed <- utils::installed.packages(lib_dir)[, "Package"]
  failed_pkgs <- c(locked_pkgs, intersect(remove_pkgs, still_installed))

  if (length(failed_pkgs) > 0) {
    pkg_logwarn("Could not remove installed package(s): %s", failed_pkgs)
  }

  return(!(pkgs %in% failed_pkgs))
}


#'
#' Simple wrapped around utils::install.packages with consideration
#' of specific package pecularities.
#'
#' @param pkgs packages to install. (type: character(N))
#' @param lib_dir path to folder to install to. (type: character(1))
#' @param type package type to install. One of source, win-binary, binary etc.
#'   (type: character(1))
#' @param repos repositories to use. (type: character(N), optional)
#' @param rver R version to install packages for. (type: character(1))
#' @param check_repo_consistency If TRUE binary consistency with rver will be
#'   checked after installation of each package.
#'   (type: logical(1), default: TRUE)
#' @param ex_libpath extra path to add to .libPaths. (type: character, default: NULL)
#'
#' @keywords internal
#' @noRd
#'
pkg_install <- function(pkgs, lib_dir, type, repos, rver, check_repos_consistency = TRUE,
                        ex_libpath = NULL) {
  common_args <- c(rscript_arg("lib", lib_dir), "quiet = FALSE")
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
                                rver = rver,
                                ex_libpath = ex_libpath)
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
           pkg_name <- gsub("^([^_]+)_.+$", "\\1", basename(pkg))
           if (majmin_rver(rver) == current_rver()) {
             # force package unload & remove it completly
             pkg_remove(pkgs = pkg_name, lib_dir = lib_dir)
           }

           bld_res <- install_package(pkg)
           pkg_path <- file.path(lib_dir, pkg_name)
           if (bld_res && !dir.exists(pkg_path)) {
             pkg_logwarn("= Restarting build of %s (succeded but no folder created)", basename(pkg))
             bld_res <- install_package(pkg)
           }
           if (bld_res && dir.exists(pkg_path)) {
             # verify if package is built for proper R version
             pkg_rver <- get_package_build_rver(lib_dir, pkg_name)
             if (is.na(pkg_rver) || majmin_rver(pkg_rver) != majmin_rver(rver)) {
               pkg_logwarn(paste("Package %s is succesfully installed but R version it is build for(%s)",
                                 "is not the one requested(%s)."),
                           pkg_name, pkg_rver, rver)
               if (any(check_repos_consistency)) {
                 pkg_logwarn(paste("It is propably cause of inconsistent repository state.",
                                   "Package %s will be deleted as it is unusable."),
                             pkg_name)
                 unlink(pkg_path, recursive = TRUE, force = TRUE)
               } else {
                 pkg_logwarn("It is propably cause of inconsistent repository state.")
               }
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
#' @noRd
#'
load_specifics <- function() {
  spec_files <- c(
    system.file(file.path("extdata", "pkg_specifics", sprintf("%s.dcf", get_os_type())),
                package = "RSuite"),
    file.path(Sys.getenv("HOME"), ".rsuite", "pkg_specifics.dcf")
  )
  if (get_os_type() %in% c("macos", "unix")) {
    spec_files <- c(spec_files, "/etc/.rsuite/pkg_specifics.dcf")
  }

  lines <- c()

  spec_files <- spec_files[file.exists(spec_files)]
  for (sf in spec_files) {
    sf_lines <- readLines(sf)
    sf_lines <- sf_lines[!grepl("^#", sf_lines)]

    tryCatch({
      read.dcf(textConnection(sf_lines))
      lines <- c(lines, sf_lines, "")
    },
    error = function(e) {
      pkg_logwarn("Failed to read %s: %s", sf, e)
    })
  }
  if (!length(lines)) {
    return(data.frame())
  }

  dcf <- read.dcf(textConnection(lines))
  spec_desc <- data.frame(dcf, stringsAsFactors = FALSE)
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
#' @noRd
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
#' @noRd
#'
get_specific_args <- function(pkg_file, spec_desc) {
  pkg_file <- basename(pkg_file)
  pkg_name <- gsub("^([^_]+)_.+$", "\\1", pkg_file)

  spec_desc <- get_pkg_specifics(pkg_name,
                                 for_source = grepl("^[^_]+_.*[.]tar[.](gz|bz2|xz)$", pkg_file),
                                 spec_desc = spec_desc)

  collapse_non_na <- function(vals, sep) {
    vals <- Filter(x = vals, f = function(v) !is.na(v))
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
  for (evp in env_var_pats) {
    if (!grepl("^[^[]+\\[.+\\]$", evp)) {
      pkg_logwarn("Invalid variable requirement for %s detected: %s", pkg_name, evp)
      next
    }
    var_name <- gsub("^([^[]+)\\[(.+)\\]$", "\\1", evp)
    var_pat  <- gsub("^([^[]+)\\[(.+)\\]$", "\\2", evp)

    match_res <- tryCatch({
      grepl(var_pat, Sys.getenv(var_name))
    },
    error = function(e) {
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
#' @param lib_dir path where package is installed. (type: character)
#' @param pkg_name name of package to get information for. (type: character)
#'
#' @return R version number the packages is built for.
#'
#' @keywords internal
#' @noRd
#'
get_package_build_rver <- function(lib_dir, pkg_name) {
  installed <- data.frame(utils::installed.packages(lib.loc = lib_dir),
                          stringsAsFactors = FALSE)[, c("Package", "Built")]
  if (!(pkg_name %in% installed$Package)) {
    return(NA)
  }
  pkg_rver <- installed[pkg_name == installed$Package, "Built"][1]
  return(pkg_rver)
}


#'
#' Creates absolute path to package dll if package has one.
#'
#' @param lib_dir path where package is installed. (type: character)
#' @param pkg_name name of package to get information for. (type: character)
#'
#' @return absolute path to package dll or NULL if it does not have internal dlls.
#'
#' @keywords internal
#' @noRd
#'
get_package_dll_path <- function(lib_dir, pkg_name) {
  dll_name <- paste0(pkg_name, .Platform$dynlib.ext)
  lib_dir <- normalizePath(lib_dir, "/", TRUE)
  dll_path <- ifelse(nzchar(.Platform$r_arch),
                     file.path(lib_dir, pkg_name, "libs", .Platform$r_arch, dll_name),
                     file.path(lib_dir, pkg_name, "libs", dll_name))
  if (!file.exists(dll_path)) {
    return(NULL)
  }
  return(dll_path)
}
