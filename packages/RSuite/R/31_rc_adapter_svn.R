#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# RC adapter working with SVN.
#----------------------------------------------------------------------------

#'
#' Detects svn command line client path
#'
#' @keywords internal
#' @noRd
#'
.get_svn_cmd <- function() {
  svn_cmd <- Sys.which("svn")
  assert(svn_cmd != "",
         paste0("No command line svn client available;",
                " Please install one and point its with PATH environment variable"))
  return(svn_cmd)
}

#'
#' Local version of get_cmd_outlines function (used inside) that sets lang variable to en_US.
#' Change was needed because rsuite depends on svn output that have to be in english.
#'
#' @keywords internal
#' @noRd
#'
.get_output_with_eng_lang <- function(desc, cmd, ..., log_debug = FALSE) {
  old_lang <- Sys.getenv("LANG")
  on.exit({
    Sys.setenv(LANG = old_lang)
  },
  add = TRUE)

  Sys.setenv(LANG = "en_US")
  return(get_cmd_outlines(desc, cmd, ..., log_debug = log_debug))
}

#'
#' Creates RC adapter to handle SVN repos.
#'
#' @param name under which RC adapter will be registered in RSuite.
#' @return object of type rsuite_rc_adapter
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_create_svn <- function(name) {
  result <- rc_adapter_create_base(name)

  class(result) <- c("rsuite_rc_adapter_svn", class(result))
  return(result)
}

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
#' @keywords internal
#' @noRd
#'
rc_adapter_is_under_control.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  # check if there is administrative .svn somewhere up folder structure
  candidate_dir <- dir
  while (!dir.exists(file.path(candidate_dir, ".svn"))) {
    parent <- dirname(candidate_dir)
    if (parent == candidate_dir) {
      return(FALSE)
    }
    candidate_dir <- parent
  }

  # ask svn for information on the directory

  svn_cmd <- .get_svn_cmd()

  pkg_logdebug("Checking if %s is recognized by SVN ...", dir)

  info_lines <- .get_output_with_eng_lang("svn info", "%s info %s", svn_cmd, dir)
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

  info_lines <- .get_output_with_eng_lang("svn info", "%s info %s", svn_cmd, parent)
  wrk_root_path <- info_lines[grepl("^Working Copy Root Path: ", info_lines)]
  if (!is.null(wrk_root_path) && length(wrk_root_path) > 0) {
    wrk_root_path <- sub("^Working Copy Root Path: ", "", wrk_root_path[1])
    pkg_logdebug("... working copy root detected: %s", wrk_root_path)
    return(TRUE)
  }

  return(FALSE)
}

#'
#' Object performing operations on SVN working copy
#'
#' @keywords internal
#' @noRd
#'
.svn_manager <- function() {
  svn_cmd <- .get_svn_cmd()

  result <- list()
  result$is_added <- function(ent) {
    lns <- .get_output_with_eng_lang("svn status", "%s status -v --depth=empty %s", svn_cmd, ent)
    return(!any(grepl("^\\?", lns)))
  }
  result$add_dir <- function(dir) {
    if (result$is_added(dir)) {
      pkg_loginfo("Directory was added already to svn: %s", dir)
      return(invisible())
    }

    lns <- .get_output_with_eng_lang("svn add", "%s add --depth=empty %s", svn_cmd, dir)
    assert(any(grepl("^A", lns)), "Failed to add %s to svn", dir)
    pkg_loginfo("Added to svn: %s", dir)
  }
  result$add_dir_rec <- function(dir) {
    lns <- .get_output_with_eng_lang("svn add", "%s add --depth=infinity --force %s", svn_cmd, dir)
    if (any(grepl("^A", lns))) {
      pkg_loginfo("Added (recursively) to svn: %s", dir)
    }
  }
  result$add_files <- function(dir, files) {
    info_lns <- .get_output_with_eng_lang("svn status", "%s status -v --depth=files %s", svn_cmd, dir)
    new_files <- basename(sub("^\\?\\s+", "", info_lns[grepl("^\\?", info_lns)]))

    for (f in list.files(dir, pattern = paste(files, collapse = "|"), all.files = TRUE)) {
      if (!(f %in% new_files)) {
        pkg_loginfo("File was added already to svn: %s", file.path(dir, f))
        next
      }

      fpath <- file.path(dir, f)
      lns <- .get_output_with_eng_lang("svn add", "%s add %s", svn_cmd, fpath)
      assert(any(grepl("^A", lns)), "Failed to add %s to svn", fpath)
      pkg_loginfo("Added to svn: %s", fpath)
    }
  }

  result$prop_rm <- function(dir, prop) {
    existing_props <- .get_output_with_eng_lang("svn add", "%s proplist %s", svn_cmd, dir)
    props_to_rm <- prop[prop %in% trimws(existing_props)]
    for (p in props_to_rm) {
      lns <- .get_output_with_eng_lang("svn propdel", "%s propdel %s %s", svn_cmd, p, dir)
      assert(any(grepl(sprintf("^property '%s' deleted", p), lns)), "Failed to remove property '%s' from %s", p, dir)

      pkg_loginfo("Property '%s' removed from %s", p, dir)
    }
  }
  result$prop_set <- function(dir, prop, val) {
    val_file <- tempfile("svn_props")
    writeLines(val, con = val_file)
    lns <- .get_output_with_eng_lang("svn propset", "%s propset %s -F %s %s", svn_cmd, prop, val_file, dir)
    unlink(val_file)

    assert(any(grepl(sprintf("^property '%s' set on", prop), lns)), "Failed to set property '%s' on %s", prop, dir)

    pkg_loginfo("Property '%s' set on %s", prop, dir)
  }
  result$prop_get <- function(dir, prop) {
    lns <- .get_output_with_eng_lang("svn propget", "%s propget %s %s", svn_cmd, prop, dir)
    if (any(grepl(sprintf("Property '%s' not found on", prop), lns))) {
      return()
    }
    return(lns)
  }

  result$get_root_path <- function(dir) {
    info_lines <- .get_output_with_eng_lang("svn info", "%s info %s", svn_cmd, dir)
    wrk_root_path <- sub("^Repository Root: ", "", info_lines[grepl("^Repository Root: ", info_lines)])
    if (length(wrk_root_path) > 0){
      return(wrk_root_path[1])
    }
    return("")
  }

  return(result)
}


#'
#' Implementation of rc_adapter_prj_struct_add for SVN rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_prj_struct_add.rsuite_rc_adapter_svn <- function(rc_adapter, params) {
  svn <- .svn_manager()

  svn$add_dir(params$prj_path)
  svn$prop_rm(params$prj_path, "svn:externals")
  if (svn$get_root_path(file.path(params$prj_path, "deployment")) != svn$get_root_path(params$prj_path)) {
    # deployment is external
    .svn_dirs <- list.files(file.path(params$prj_path, "deployment"), pattern = "[.]svn$",
                            include.dirs = TRUE, recursive = TRUE, all.files = TRUE, full.names = TRUE)
    unlink(.svn_dirs, recursive = TRUE, force = TRUE)
  }

  svn_add_folder(svn, params$prj_path)
}

#'
#' Implementation of rc_adapter_pkg_struct_add for SVN rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_pkg_struct_add.rsuite_rc_adapter_svn <- function(rc_adapter, params, name) {
  svn <- .svn_manager()
  svn$add_dir(params$pkgs_path)

  pkg_dir <- file.path(params$pkgs_path, name)
  svn_add_folder(svn, pkg_dir)
}


#'
#' Iterates recursively over folder structure and adds its content under
#'  version control.
#'
#' Detects files/folders to be ignored specified in rc_ignore files. Sets
#'  appropriate svn:ignore properties.
#'
#' @param svn svn_manager object
#' @param fld_path path to folder to be processed.
#' @param up_ignores ignores collected from upper folders.
#'
#' @keywords internal
#' @noRd
#'
svn_add_folder <- function(svn, fld_path, up_ignores = c()) {
  svn$add_dir(fld_path)

  ignores <- svn$prop_get(fld_path, "svn:ignore")
  ignores <- ignores[ignores != ""]

  new_ignores <- up_ignores
  ignores_file <- file.path(fld_path, "__rc_ignore")
  if (file.exists(ignores_file)) {
    new_ignores <- c(new_ignores, readLines(ignores_file))
    new_ignores <- new_ignores[new_ignores != ""]
    unlink(ignores_file, force = TRUE)
  }

  if (length(setdiff(new_ignores, ignores)) > 0) {
    ignores <- unique(c(new_ignores, ignores))
    svn$prop_set(fld_path, "svn:ignore", ignores)
  }
  ignores <- c(".svn", ignores)

  toadd <- list.files(fld_path, all.files = TRUE, no.. = TRUE)
  if (length(ignores) > 0) {
    ignores_rx <- utils::glob2rx(ignores)
    toadd <- lapply(X = toadd,
                    FUN = function(fn) {
                      matched_rx <- lapply(ignores_rx, function(rx) grepl(rx, fn))
                      if (any(unlist(matched_rx))) {
                        return()
                      }
                      return(fn)
                    })
    toadd <- unlist(toadd)
  }

  files_toadd <- toadd[!dir.exists(file.path(fld_path, toadd))]
  if (length(files_toadd) > 0) {
    svn$add_files(fld_path, files_toadd)
  }

  # ignores on subsequent folders
  down_path_ignores <- lapply(ignores, function(ig) unlist(strsplit(ig, "\\\\|/")))

  fldrs_toadd <- toadd[dir.exists(file.path(fld_path, toadd))]
  for (subfld in fldrs_toadd) {
    sub_ignores <- lapply(down_path_ignores,
                          function(ig_path) {
                            if (ig_path[[1]] == subfld) {
                              paste(ig_path[-1], collapse = .Platform$file.sep)
                            }
                          })
    sub_ignores <- unlist(sub_ignores)

    svn_add_folder(svn, file.path(fld_path, subfld), sub_ignores)
  }
}

#'
#' Implementation of rc_adapter_get_version for SVN rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_get_version.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  svn_cmd <- .get_svn_cmd()

  # detect local revision
  info_lns <- .get_output_with_eng_lang("svn info", "%s info %s", svn_cmd, dir)
  rev_ln <- info_lns[grepl("^Revision: ", info_lns)]
  assert(!is.null(rev_ln) && length(rev_ln) > 0,
         "Failed to detect SVN revision at %s. Is directory under SVN control?", dir)
  revision <- trimws(sub("^Revision: ", "", rev_ln[[1]]))

  # detect if has changes
  stat_lns <- .get_output_with_eng_lang("svn status", "%s status %s", svn_cmd, dir)
  has_changes <- any(grepl("^[?!MA]", stat_lns))

  # detect if working copy needs update
  diff_lns <- .get_output_with_eng_lang("svn diff", "%s diff -r %s %s", svn_cmd, revision, dir)
  needs_update <- length(diff_lns) > 0

  return(list(
    has_changes = has_changes,
    revision = revision,
    needs_update = needs_update
  ))
}


#'
#' Implementation of rc_adapter_remove_admins for SVN rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_remove_admins.rsuite_rc_adapter_svn <- function(rc_adapter, dir) {
  admins <- list.files(dir, pattern = ".svn", recursive = TRUE, include.dirs = TRUE, all.files = TRUE)
  admins <- admins[dir.exists(file.path(dir, admins))]
  unlink(file.path(dir, admins), recursive = TRUE, force = TRUE)
}
