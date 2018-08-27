#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Test supporting tools.
#----------------------------------------------------------------------------

options(rsuite.cache_path = "~/.rsuite")
options(rsuite.user_templ_path = "~/.rsuite/templates")

.test_env <- new.env()
assign("cleanup", c(), envir = .test_env)

test_that_managed <- function(desc, ...) {
  tryCatch({
    # setup logging
    root_level <- logging::getLogger()$level
    rsuite_level <- RSuite::rsuite_getLogger()$level
    on_test_exit(function() {
      logging::setLevel(root_level)
      logging::setLevel(rsuite_level, RSuite::rsuite_getLogger())
    })

    log_file <- file.path(.get_create_dir("logs"), sprintf("test_%s.log", Sys.Date()))
    cat(sprintf("====> %s <====\n", desc), file = log_file, append = T)

    logging::setLevel("CRITICAL")
    logging::setLevel("DEBUG", logging::getLogger("rsuite"))
    logging::addHandler(action = logging::writeToFile,
                        file = log_file,
                        handler = "RSuite.tests.file.logger", level = "DEBUG",
                        logger = RSuite::rsuite_getLogger())

    unlink(get_wspace_dir(), recursive = T, force = T)

    test_that(desc, ...)
  }, finally = {
    fire_cleanups()
    logging::removeHandler(handler = "RSuite.tests.file.logger")
  })
}

fire_cleanups <- function() {
  cleanups <- get("cleanup", envir = .test_env)
  for (cup in cleanups) {
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
get_wspace_template_dir <- function() { .get_create_dir("wspace/templates")}


.get_create_dir <- function(name) {
  dpath <- file.path(RSuite::prj_init()$path, "tests", name)
  if (!dir.exists(dpath)) {
    dir.create(dpath, recursive = T)
  }
  return(dpath)
}


get_repo_path <- function(dir) {
  system2("svn", args = c("upgrade", dir), stdout = NULL, stderr = NULL)

  cmd <- sprintf("svn info %s", dir)

  con <- pipe(cmd, open = "rt")
  lines <- tryCatch({
    readLines(con)
  }, finally = { close(con) })

  repo_path <- sub("^Repository Root: ", "", lines[grepl("^Repository Root: ", lines)])
  if (length(repo_path) > 0) {
    return(repo_path[1])
  }
  return("")
}


expect_log_message <- function(object, regexp) {
  log_file <- tempfile(fileext = ".log")

  logging::addHandler(action = logging::writeToFile,
                      file = log_file,
                      handler = "RSuite.tests.expect_file.logger", level = "DEBUG",
                      logger = RSuite::rsuite_getLogger())

  on.exit({
    logging::removeHandler(handler = "RSuite.tests.expect_file.logger",
                           logger = RSuite::rsuite_getLogger())
    unlink(log_file, force = TRUE)
  },
  add = TRUE)

  call <- deparse(substitute(object))
  try(object, silent = T)

  if (!file.exists(log_file)) {
    expect(FALSE, sprintf("%s produced no output", call))
  } else {
    output <- readLines(log_file)
    matched <- output[grepl(regexp, output)]
    expect(length(matched) != 0,
           sprintf("%s produced no output matching '%s'", call, regexp))
  }
}


test_that_template <- function(desc, ...) {
  # setup test environment options
  old_option <- options("rsuite.user_templ_path")
  options(rsuite.user_templ_path = get_wspace_template_dir())

  on_test_exit(function() {
    # clean up, set options back to normal
    options(rsuite.user_templ_path = unlist(old_option))
  })
  test_that_managed(desc, ...)
}


# used for the lock test project source creation, might be useful in the future
create_lock_test_prj <- function() {
  templ_path <- file.path("data", "LockTestProjectTemplate")
  if (dir.exists(templ_path)) {
    unlink(templ_path, recursive = TRUE, force = TRUE)
  }

  unzip(file.path("data", "LockTestProjectTemplate.zip"), exdir = "data")

  build_prj <- RSuite::prj_start("LockTestProjectBuild", skip_rc = TRUE, path = templ_path, tmpl = templ_path)
  params <- build_prj$load_params()
  on.exit({
    unlink(params$prj_path, recursive = TRUE, force = TRUE)
  },
  add = TRUE)


  dst_rmgr <- RSuite::repo_mng_start("Dir",
                                     path = normalizePath(file.path(templ_path, "project", "repository")),
                                     rver = params$r_ver,
                                     types = params$bin_pkgs_type)
  RSuite::repo_upload_ext_packages(dst_rmgr,
                                   pkgs = c("logging", "AddedTestDependency", "TestDependencyToRemove", "TestDependencyToUpdate"),
                                   prj = build_prj)

  # repo_upload_ext_packages builds TestDependencyToUpdate v1.1 we still need v1.0, so ...
  # ... we remove TestDependencyToUpdate v1.1 and uploading ext packages again
  src_rmgr <- RSuite::repo_mng_start("Dir",
                                     path = normalizePath(file.path(params$prj_path, "repository")),
                                     rver = params$r_ver,
                                     types = "source")
  RSuite::repo_mng_remove(src_rmgr,
                          toremove = data.frame(Package = "TestDependencyToUpdate", Version = "1.1", stringsAsFactors = FALSE),
                          pkg_type = "source")
  RSuite::repo_mng_stop(src_rmgr)

  RSuite::repo_upload_ext_packages(dst_rmgr,
                                   pkgs = c("TestDependencyToUpdate"),
                                   prj = build_prj)

  RSuite::repo_mng_stop(rmgr)
}

init_lock_test_prj <- function() {
  prj <- init_test_project(tmpl = file.path("data", "LockTestProjectTemplate"))

  on_test_exit(function() {
    unlink(file.path(get_wspace_dir(), "LockTestProjectTemplate"), force = T, recursive = T)
  })

  return(prj)
}
