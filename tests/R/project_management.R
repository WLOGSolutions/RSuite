#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Tools for project management during testing.
#----------------------------------------------------------------------------

init_test_project <- function(repo_adapters = c("Dir"), name = "TestProject",
                              tmpl = "builtin") {
  RSuite::prj_load() # load RSuite project not to miss it in .libPaths()

  prj <- RSuite::prj_start(name, skip_rc = T, path = get_wspace_dir(),
                           tmpl = tmpl)
  RSuite::prj_config_set_repo_adapters(repos = repo_adapters, prj = prj)

  unlink(file.path(prj$path, "deployment", "libs", "logging"),
         recursive = T, force = T) # remove precreated logger

  # remove SnapshotDate
  params_path <- file.path(prj$path, "PARAMETERS")
  params_df <- data.frame(read.dcf(file = params_path))
  params_df$SnapshotDate <- NULL
  write.dcf(params_df, file = params_path)

  on_test_exit(function() {
    unlink(prj$path, recursive = T, force = T)
  })
  return(prj)
}

deploy_package_to_lrepo <- function(pkg_file, prj, type = .Platform$pkgType) {
  loc_repo <- .get_local_repo_path(prj, type)
  file.copy(file.path("data", pkg_file), loc_repo, overwrite = T)
  RSuite:::rsuite_write_PACKAGES(loc_repo, type = type)
}

remove_package_from_lrepo <- function(pkg_file, prj, type = .Platform$pkgType) {
  loc_repo <- .get_local_repo_path(prj, type)
  file.remove(file.path(loc_repo, pkg_file), loc_repo, overwrite = T)
  RSuite:::rsuite_write_PACKAGES(loc_repo, type = type)
}

create_test_package <- function(name, prj, ver = "1.0", deps = "",
                                imps = "", tmpl = "builtin") {
  RSuite::prj_start_package(name, prj = prj, skip_rc = T, tmpl = tmpl)
  pkg_path <- file.path(prj$path, "packages", name)

  pkg_desc_fname <- file.path(pkg_path, "DESCRIPTION")
  
  if (file.exists(pkg_desc_fname)) {
    pkg_desc <- data.frame(read.dcf(file = pkg_desc_fname))
    pkg_desc$Version <- ver
    deps <- trimws(deps)
    if (nchar(deps)) {
      pkg_desc$Depends <- deps
    }
    imps <- trimws(imps)
    if (nchar(imps)) {
      pkg_desc$Imports <- imps
    }
    write.dcf(pkg_desc, file = pkg_desc_fname)
  }

  invisible(pkg_path)
}

set_test_package_ns_imports <- function(name, prj, imps) {
  imp_path <- file.path(prj$path, "packages", name, "R", "packages_import.R")
  writeLines(c(sprintf("#' @import %s", imps), "NULL"), con = imp_path)
}

create_test_master_script <- function(code, prj) {
  fn <- tempfile(pattern = "test_", fileext = ".R", tmpdir = file.path(prj$path, "R"))
  f <- file(fn, "w")
  writeLines(code, con = f)
  close(f)
  invisible(fn)
}

create_package_deploy_to_lrepo <- function(name, prj, ver = "1.0", type = .Platform$pkgType,
                                           deps = "", imps = "logging") {
  pkg_path <- create_test_package(name, prj, ver, deps = deps, imps = imps)
  set_test_package_ns_imports(name, prj, unlist(strsplit(imps, ",")))

  params <- prj$load_params()
  on.exit({
    unlink(pkg_path, recursive = T, force = T)
    unlink(file.path(params$lib_path, "*"), recursive = T, force = T)
  }, add = T)
  loc_repo <- .get_local_repo_path(prj, type)

  prj_install_deps(prj, clean = T)
  prj_build(prj, type = type)

  int_path <- RSuite:::rsuite_contrib_url(repos = params$get_intern_repo_path(), type = type)
  avails <- data.frame(available.packages(sprintf("file:///%s", int_path), type = type),
                       stringsAsFactors = F)
  pkg_file <- avails[avails$Package == name, "File"]

  file.copy(from = file.path(int_path, pkg_file), to = loc_repo)
  RSuite:::rsuite_write_PACKAGES(loc_repo, type = type)
}

remove_test_packages <- function(prj) {
  unlink(file.path(prj$path, "packages", "*"), recursive = T, force = T)
}

.get_local_repo_path <- function(prj, type) {
  path <- RSuite:::rsuite_contrib_url(repos = file.path(prj$path, "repository"), type = type)
  stopifnot(dir.exists(path))

  path <- normalizePath(path)
  return(path)
}


expect_that_packages_installed <- function(names, prj, versions = NULL) {
  stopifnot(is.null(versions) || length(names) == length(versions))

  lib_path <- file.path(prj$path, "deployment", "libs")
  installed <- installed.packages(lib.loc = lib_path, noCache = T)[, "Package"]
  pass <- setequal(installed, names)
  if (pass) {
    msg <- ""
  } else if (length(setdiff(names, installed)) > 0) {
    msg <- sprintf("Package(s) %s failed to install", paste(setdiff(names, installed), collapse = ", "))
  } else if (length(setdiff(installed, names)) > 0) {
    msg <- sprintf("Unexpected package(s) %s installed", paste(setdiff(installed, names), collapse = ", "))
  } else {
    stop(sprintf("Unexpected condition occured: %s != %s!!!", paste(names, collapse = ", "), paste(installed, collapse = ", ")))
  }

  if (pass && !is.null(versions)) {
    inst_vers <- as.data.frame(installed.packages(lib.loc = lib_path, noCache = T), stringsAsFactors = F)[, c("Package", "Version")]
    expt_vers <- data.frame(Package = names, Expected = versions)
    failed_vers <- merge(x = inst_vers, y = expt_vers, by.x = "Package", by.y = "Package")
    failed_vers <- failed_vers[!is.na(failed_vers$Expected) & failed_vers$Version != failed_vers$Expected, ]

    pass <- nrow(failed_vers) == 0
    if (!pass) {
      msg <- sprintf("Unexpected versions installed ([pkg]ver!=exp): %s",
                     paste(sprintf("[%s]%s!=%s", failed_vers$Package, failed_vers$Version, failed_vers$Expected),
                           collapse = ", "))
    }
  }

  expect(pass, msg)
  invisible(installed)
}

expect_that_has_docs <- function(topics, pkg_name, prj) {
  doc_path <- file.path(prj$path, "deployment", "libs", pkg_name, "help", "AnIndex")
  if (!file.exists(doc_path)) {
    pass <- F
    msg <- sprintf("No documentation index found for %s", pkg_name)
  } else {
    lines <- readLines(doc_path)
    all_topics <- unlist(lapply(strsplit(lines, "\t"), function(ent) { ent[1] }))

    pass <- all(topics %in% all_topics)
    if (!pass) {
      msg <- sprintf("Documetation topics not found in %s: %s",
                     pkg_name, paste(setdiff(topics, all_topics), collapse = ", "))
    } else {
      msg <- ""
    }
  }
  expect(pass, msg)
}

expect_that_packages_locked <- function(expects, params) {
  lock_data <- data.frame(read.dcf(params$lock_path), stringsAsFactors = FALSE)
  expected_data <- data.frame(Package = names(expects), Expected = expects)

  locked <- lock_data$Package
  pass <- setequal(locked, expected_data$Package)
  if (pass) {
    msg <- ""
  } else if (length(setdiff(expected_data$Package, locked)) > 0) {
    msg <- sprintf("Package(s) %s failed to lock", paste(setdiff(expected_data$Package, locked), collapse = ", "))
  } else if (length(setdiff(locked, expected_data$Package)) > 0) {
    msg <- sprintf("Unexpected package(s) %s locked", paste(setdiff(locked, expected_data$Package), collapse = ", "))
  } else {
    stop(sprintf("Unexpected condition occured: %s != %s!!!", paste(expected_data$Package, collapse = ", "), paste(locked, collapse = ", ")))
  }

  if (pass) {
    failed_vers <- merge(x = lock_data, y = expected_data, by.x = "Package", by.y = "Package")
    failed_vers <- failed_vers[!is.na(failed_vers$Expected) & failed_vers$Version != failed_vers$Expected, ]

    pass <- nrow(failed_vers) == 0
    msg <- sprintf("Unexpected versions locked ([pkg]ver!=exp): %s",
                   paste(sprintf("[%s]%s!=%s", failed_vers$Package, failed_vers$Version, failed_vers$Expected),
                       collapse = ", "))
  }

  expect(pass, msg)
}
