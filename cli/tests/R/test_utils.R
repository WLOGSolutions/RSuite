#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Test supporting tools.
#----------------------------------------------------------------------------

.test_env <- new.env()
assign("cleanup", c(), envir = .test_env)

test_that_managed <- function(desc, ...) {
  tryCatch({
    cat(sprintf("====> %s <====\n", desc), file = get_log_file(), append = T)
    init_test()
    test_that(desc, ...)
  }, finally = {
    fire_cleanups()
  })
}

init_test <- function() {
  path <- Sys.getenv("PATH")

  base_dir <- normalizePath(dirname(getwd()))
  in_path_base <- paste0(base_dir, .Platform$path.sep)
  if (substring(path, 1, nchar(in_path_base)) == in_path_base) {
    Sys.setenv(PATH=paste0(base_dir, .Platform$path.sep, path))
    on_test_exit(function(){
      Sys.setenv(PATH = path)
    })
  }

  unlink(get_wspace_dir(), recursive = T, force = T)
}

fire_cleanups <- function() {
  cleanups <- get("cleanup", envir = .test_env)
  for(cup in get("cleanup", envir = .test_env)) {
    cup()
  }
  assign("cleanup", c(), envir = .test_env)
}

on_test_exit <- function(cup) {
  cleanups <- get("cleanup", envir = .test_env)
  assign("cleanup", c(cup, cleanups), envir = .test_env)
}

get_wspace_dir <- function() { .get_create_dir("wspace") }
get_data_dir <- function() { .get_create_dir("data") }
get_log_dir <- function() { .get_create_dir("logs") }
get_log_file <- function() { file.path(get_log_dir(), sprintf("test_log_%s.log", Sys.Date())) }

.get_create_dir <- function(name) {
  dpath <- file.path(getwd(), name)
  if (!dir.exists(dpath)) {
    dir.create(dpath, recursive = T)
  }
  return(dpath)
}


rsuite_run <- function(args, wd) {
  log_file <- get_log_file()

  rsuite_cmd <- file.path(getwd(), "..", ifelse(.Platform$OS.type == "windows", "rsuite.cmd", "rsuite"))
  rsuite_cmd <- normalizePath(rsuite_cmd)
  cat(sprintf("----> Running '%s %s' in %s ...\n",
              rsuite_cmd, paste(args, collapse = " "), wd),
      file = log_file, append = T)

  rsuite_libpath <- dirname(system.file(package = "RSuite"))
  Sys.setenv(RSUITE_LIBLOC=rsuite_libpath)
  on.exit(Sys.unsetenv("RSUITE_LIBLOC"), add = T)
  unlink(file.path(wd, ".Rprofile"), force = T, recursive = T)

  cat(sprintf("----> \t... using RSuite from %s ...\n", rsuite_libpath),
      file = log_file, append = T)

  old_wd <- setwd(wd)
  on.exit(setwd(old_wd), add = T)

  stderr_file <- tempfile()
  on.exit(unlink(stderr_file, force = T), add = T)

  retcode <- system2(rsuite_cmd, args = c(args, ">>", log_file),
                     stderr = stderr_file, stdout = NULL)
  has_error <- F
  if (file.exists(stderr_file)) {
    stderr_lines <- readLines(stderr_file)
    cat(stderr_lines, file = log_file, sep = "\n", append = T)
    has_error <- any(grepl("^ERROR: ", stderr_lines))
  }
  cat(sprintf("----> ... done(retcode: %s, error: %s)\n\n", retcode, has_error),
      file = log_file, append = T)

  if (retcode == 0 && has_error) {
    retcode <- 255
  }
  return(retcode)
}

create_test_project <- function(name) {
  wspace_dir <- get_wspace_dir()
  on_test_exit(function() {
    unlink(file.path(wspace_dir, name), force = T, recursive = T)
  })

  retcode <- rsuite_run(args = c("proj", "start", "-v", "-n", name, "--skip_rc"),
                        wd = wspace_dir)
  return(retcode)
}

create_test_package <- function(proj_name, pkg_name) {
  retcode <- create_test_project(proj_name)
  if (retcode != 0) {
    return(NULL)
  }

  proj_path <- file.path(get_wspace_dir(), proj_name)
  retcode <- rsuite_run(args = c("proj", "pkgadd", "-v", "-n", pkg_name, "--skip_rc"),
                        wd = proj_path)
  if (retcode != 0) {
    return(NULL)
  }

  return(file.path(proj_path, "packages", pkg_name))
}

setup_package <- function(pkg_path, ...) {
  dcf <- data.frame(read.dcf(file.path(pkg_path, "DESCRIPTION")), stringsAsFactors = F)
  dots <- list(...)

  cols <- colnames(dcf)
  dcf <- cbind(dcf[, cols[!(cols %in% names(dots))]], dots)
  write.dcf(dcf, file = file.path(pkg_path, "DESCRIPTION"))
}

expect_that_packages_installed <- function(names, prj_path, versions = NULL) {
  stopifnot(is.null(versions) || length(names) == length(versions))

  lib_path <- file.path(prj_path, "deployment", "libs")
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


create_test_repo <- function(name) {
  repo_path <- file.path(get_wspace_dir(), "repo")
  dir.create(repo_path, recursive = T)
  on_test_exit(function() {
    unlink(repo_path, force = T, recursive = T)
  })
  return(repo_path)
}

expect_that_repo_contains <- function(names, repo_path, type) {
  pkgs <- suppressWarnings({
    available.packages(contriburl = contrib.url(sprintf("file:///%s", repo_path), type = type),
                       type = type)
  })
  failed <- names[!(names %in% pkgs[, 'Package'])]

  expect(length(failed) == 0,
         sprintf("Expected packages not available in repository: %s",
                 paste(failed, collapse = ", ")))
}
