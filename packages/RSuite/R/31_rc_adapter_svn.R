#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# RC adapter working with SVN.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Detects svn command line client path
#'
.get_svn_cmd <- function() {
  svn_cmd <- Sys.which("svn")
  assert(svn_cmd != "",
         paste0("No command line svn client available;",
                " Please install one and point its with PATH environment variable"))
  return(svn_cmd)
}

#'
#' @keywords internal
#'
#' Creates RC adapter to handle SVN repos.
#'
#' @param name under which RC adapter will be registered in RSuite.
#' @return object of type rsuite_rc_adapter
#'
rc_adapter_create_svn <- function(name) {
  result <- rc_adapter_create_base(name)

  class(result) <- c('rsuite_rc_adapter_svn', class(result))
  return(result)
}

#'
#' @keywords internal
#'
#' Implementation of rc_adapter_is_under_control for SVN rc adapted.
#'
#' Checks if folder is under SVN version control: contains administrative .svn
#' folder inside or up folder tree. If not, the folder is not under VC.
#'
#' Detects SVN command, commits exception if failed to find.
#'
#' With detected svn client checks if folder is indeed under version control or
#' if it is just created inside working copy. If so warning is commited.
#'
rc_adapter_is_under_control.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  # check if there is administrative .svn somethere up folder structure
  candidate_dir <- dir
  while(!dir.exists(file.path(candidate_dir, ".svn"))) {
    parent <- dirname(candidate_dir)
    if (parent == candidate_dir) {
      return(FALSE)
    }
    candidate_dir <- parent
  }

  # ask svn for information on the directory

  svn_cmd <- .get_svn_cmd()

  pkg_logdebug("Checking if %s is recognized by SVN ...", dir)

  info_lines <- get_cmd_lines("svn info", "%s info %s", svn_cmd, dir)
  wrk_root_path <- info_lines[grepl("^Working Copy Root Path: ", info_lines)]
  if (!is.null(wrk_root_path) && length(wrk_root_path) > 0) {
    wrk_root_path <- sub("^Working Copy Root Path: ", "", wrk_root_path[1])
    pkg_logdebug("... working copy root detected: %s", wrk_root_path)
    return(TRUE)
  }
  wrk_path <- info_lines[grepl("^Path: ", info_lines)]
  if (!is.null(wrk_path) && length(wrk_path) > 0) {
    wrk_path <- sub("^Path: ", "", wrk_path[1])
    pkg_logdebug("... working copy detected: %s", wrk_path)
    return(TRUE)
  }

  # try parent
  parent <- dirname(dir)
  if (parent == dir) {
    return(FALSE)
  }

  info_lines <- get_cmd_lines("svn info", "%s info %s", svn_cmd, parent)
  wrk_root_path <- info_lines[grepl("^Working Copy Root Path: ", info_lines)]
  if (!is.null(wrk_root_path) && length(wrk_root_path) > 0) {
    wrk_root_path <- sub("^Working Copy Root Path: ", "", wrk_root_path[1])
    pkg_logdebug("... working copy root detected: %s", wrk_root_path)
    return(TRUE)
  }

  return(FALSE)
}


#'
#' @keywords internal
#'
#' Object performing operations on SVN working copy
#'
.svn_manager <- function() {
  svn_cmd <- .get_svn_cmd()

  result <- list()
  result$is_added <- function(ent) {
    lns <- get_cmd_lines("svn status", "%s status -v --depth=empty %s", svn_cmd, ent)
    return(!any(grepl("^\\?", lns)))
  }
  result$add_dir <- function(dir) {
    if (result$is_added(dir)) {
      pkg_loginfo("Directory was added already to svn: %s", dir)
      return(invisible())
    }

    lns <- get_cmd_lines("svn add", "%s add --depth=empty %s", svn_cmd, dir)
    assert(any(grepl("^A", lns)), "Failed to add %s to svn", dir)
    pkg_loginfo("Added to svn: %s", dir)
  }
  result$add_dir_rec <- function(dir) {
    lns <- get_cmd_lines("svn add", "%s add --depth=infinity --force %s", svn_cmd, dir)
    if (any(grepl("^A", lns))) {
      pkg_loginfo("Added (recursively) to svn: %s", dir)
    }
  }
  result$add_files <- function(dir, files) {
    info_lns <- get_cmd_lines("svn status", "%s status -v --depth=files %s", svn_cmd, dir)
    new_files <- basename(sub("^\\?\\s+", "", info_lns[grepl("^\\?", info_lns)]))

    for(f in list.files(dir, pattern = paste(files, collapse = "|"), all.files = T)) {
      if (!(f %in% new_files)) {
        pkg_loginfo("File was added already to svn: %s", file.path(dir, f))
        next
      }

      fpath <- file.path(dir, f)
      lns <- get_cmd_lines("svn add", "%s add %s", svn_cmd, fpath)
      assert(any(grepl("^A", lns)), "Failed to add %s to svn", fpath)
      pkg_loginfo("Added to svn: %s", fpath)
    }
  }

  result$prop_rm <- function(dir, prop) {
    existing_props <- get_cmd_lines("svn add", "%s proplist %s", svn_cmd, dir)
    props_to_rm <- prop[prop %in% trimws(existing_props)]
    for(p in props_to_rm) {
      lns <- get_cmd_lines("svn propdel", "%s propdel %s %s", svn_cmd, p, dir)
      assert(any(grepl(sprintf("^property '%s' deleted", p), lns)), "Failed to remove property '%s' from %s", p, dir)

      pkg_loginfo("Property '%s' removed from %s", p, dir)
    }
  }
  result$prop_set <- function(dir, prop, val) {
    val_file <- tempfile("svn_props")
    writeLines(val, con = val_file)
    lns <- get_cmd_lines("svn propset", "%s propset %s -F %s %s", svn_cmd, prop, val_file, dir)
    unlink(val_file)

    assert(any(grepl(sprintf("^property '%s' set on", prop), lns)), "Failed to set property '%s' on %s", prop, dir)

    pkg_loginfo("Property '%s' set on %s", prop, dir)
  }

  result$get_root_path <- function(dir) {
    info_lines <- get_cmd_lines("svn info", "%s info %s", svn_cmd, dir)
    wrk_root_path <- sub("^Repository Root: ", "", info_lines[grepl("^Repository Root: ", info_lines)])
    if (length(wrk_root_path) > 0){
      return(wrk_root_path[1])
    }
    return("")
  }

  return(result)
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_prj_struct_add for SVN rc adapted.
#'
rc_adapter_prj_struct_add.rsuite_rc_adapter_svn <- function(rc_adapter, params) {
  svn <- .svn_manager()

  svn$add_dir(params$prj_path)
  svn$prop_rm(params$prj_path, "svn:externals")
  if (svn$get_root_path(file.path(params$prj_path, "deployment")) != svn$get_root_path(params$prj_path)) {
    # deployment is external
    .svn_dirs <- list.files(file.path(params$prj_path, "deployment"), pattern = "[.]svn$",
                            include.dirs = T, recursive = T, all.files = T, full.names = T)
    unlink(.svn_dirs, recursive = T, force = T)
  }

  svn$prop_set(params$prj_path, "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata", "config.txt"))
  svn$add_files(params$prj_path, c("PARAMETERS", "config_templ.txt", ".Rprofile", "*.Rproj"))

  svn$add_dir(file.path(params$prj_path, "deployment"))
  svn$add_files(file.path(params$prj_path, "deployment"), c("*.R", "*.r", "*.Rproj"))
  svn$prop_set(file.path(params$prj_path, "deployment"), "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata", "intrepo"))

  svn$add_dir(file.path(params$prj_path, "deployment", "libs"))
  svn$prop_set(file.path(params$prj_path, "deployment", "libs"), "svn:ignore", c("*.*", "*"))

  svn$add_dir(file.path(params$prj_path, "deployment", "sbox"))
  svn$prop_set(file.path(params$prj_path, "deployment", "sbox"), "svn:ignore", c("*.*", "*"))

  svn$add_dir(file.path(params$prj_path, "logs"))
  svn$prop_set(file.path(params$prj_path, "logs"), "svn:ignore", c("*.*", "*"))

  svn$add_dir(file.path(params$prj_path, "packages"))

  svn$add_dir(file.path(params$prj_path, "R"))
  svn$add_files(file.path(params$prj_path, "R"), c("*.R", "*.r", "*.Rproj"))
  svn$prop_set(file.path(params$prj_path, "R"), "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata"))

  if (dir.exists(file.path(params$prj_path, "tests"))) {
    svn$add_dir(file.path(params$prj_path, "tests"))
    svn$add_files(file.path(params$prj_path, "tests"), c("*.R", "*.r", "*.Rproj"))
    svn$prop_set(file.path(params$prj_path, "tests"), "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata"))
  }

  if (dir.exists(file.path(params$prj_path, "repository"))) {
    svn$add_dir_rec(file.path(params$prj_path, "repository"))
  }
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_pkg_struct_add for SVN rc adapted.
#'
rc_adapter_pkg_struct_add.rsuite_rc_adapter_svn <- function(rc_adapter, params, name) {
  pkg_dir <- file.path(params$pkgs_path, name)

  svn <- .svn_manager()

  svn$add_dir(pkg_dir)
  svn$prop_set(pkg_dir, "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata"))
  svn$add_files(pkg_dir, c("DESCRIPTION", "NAMESPACE", "NEWS", "*.Rproj", ".Rprofile"))

  svn$add_dir(file.path(pkg_dir, "data"))
  svn$add_dir(file.path(pkg_dir, "inst"))
  svn$add_dir(file.path(pkg_dir, "man"))
  svn$prop_set(file.path(pkg_dir, "man"), "svn:ignore", "*.*")

  svn$add_dir(file.path(pkg_dir, "R"))
  svn$add_files(file.path(pkg_dir, "R"), c("*.R", "*.r"))

  if (dir.exists(file.path(pkg_dir, "tests"))) {
    svn$add_dir(file.path(pkg_dir, "tests"))
    svn$add_files(file.path(pkg_dir, "tests"), c("*.R", "*.r"))
    svn$prop_set(file.path(pkg_dir, "tests"), "svn:ignore", c(".Rproj.user", ".Rhistory", ".Rdata"))
  }
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_get_version for SVN rc adapted.
#'
rc_adapter_get_version.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  svn_cmd <- .get_svn_cmd()

  # detect local revision
  info_lns <- get_cmd_lines("svn info", "%s info %s", svn_cmd, dir)
  rev_ln <- info_lns[grepl("^Revision: ", info_lns)]
  assert(!is.null(rev_ln) && length(rev_ln) > 0,
         "Failed to detect SVN revision at %s. Is directory under SVN control?", dir)
  revision <- trimws(sub("^Revision: ", "", rev_ln[[1]]))

  # detect if has changes
  stat_lns <- get_cmd_lines("svn status", "%s status %s", svn_cmd, dir)
  has_changes <- any(grepl("^[?!MA]", stat_lns))

  # detect if working copy needs update
  diff_lns <- get_cmd_lines("svn diff", "%s diff -r %s %s", svn_cmd, revision, dir)
  needs_update <- length(diff_lns) > 0

  return(list(
    has_changes = has_changes,
    revision = revision,
    needs_update = needs_update
  ))
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_remove_admins for SVN rc adapted.
#'
rc_adapter_remove_admins.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  admins <- list.files(dir, pattern = ".svn", recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  admins <- admins[dir.exists(file.path(dir, admins))]
  unlink(file.path(dir, admins), recursive = T, force = T)
}
