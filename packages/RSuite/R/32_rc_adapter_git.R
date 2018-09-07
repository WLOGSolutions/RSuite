#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# RC adapter working with GIT.
#----------------------------------------------------------------------------

#'
#' Creates RC adapter to handle GIT repos.
#'
#' @param name under which RC adapter will be registered in RSuite.
#' @return object of type rsuite_rc_adapter
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_create_git <- function(name) {
  result <- rc_adapter_create_base(name)

  class(result) <- c("rsuite_rc_adapter_git", class(result))
  return(result)
}

#'
#' Implementation of rc_adapter_is_under_control for GIT RC adapted.
#'
#' Checks if folder is under GIT version control: contains administrative .git
#' folder inside or up folder tree. If not, the folder is not under VC.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_is_under_control.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  tryCatch({
    repo <- git2r::repository(dir, discover = TRUE)
    pkg_logdebug("Git working directory detected: %s", git2r::workdir(repo))
    TRUE
  },
  error = function(e) FALSE)
}

#'
#' Implementation of rc_adapter_prj_struct_add for GIT RC adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_prj_struct_add.rsuite_rc_adapter_git <- function(rc_adapter, params) {
  repo <- git2r::repository(params$prj_path, discover = TRUE)

  prj_gitbase <- sub(rsuite_fullUnifiedPath(workdir(repo)), "", params$prj_path, fixed = TRUE)
  if (!nchar(prj_gitbase)) {
    git_path <- file.path
  } else {
    git_path <- function(...) file.path(prj_gitbase, ...)
  }

  git_add_folder(repo, params$prj_path, git_path)
}


#'
#' Implementation of rc_adapter_pkg_struct_add for GIT rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_pkg_struct_add.rsuite_rc_adapter_git <- function(rc_adapter, params, name) {
  pkg_dir <- file.path(params$pkgs_path, name)
  repo <- git2r::repository(pkg_dir, discover = TRUE)

  pkg_gitbase <- sub(rsuite_fullUnifiedPath(workdir(repo)), "", pkg_dir, fixed = TRUE)
  if (!nchar(pkg_gitbase)) {
    git_path <- file.path
  } else {
    git_path <- function(...) file.path(pkg_gitbase, ...)
  }

  git_add_folder(repo, pkg_dir, git_path)
}


#'
#' Iterates recursively over folder structure and adds its content under
#'  version control.
#'
#' Detects files/folders to be ignored specified in rc_ignore files. Sets
#'  appropriate .gitignore.
#'
#' @param repo git repository object.
#' @param fld_path path to folder to be processed.
#' @param git_path_f function building git reference path.
#' @param up_ignores ignores collected from upper folders.
#'
#' @keywords internal
#' @noRd
#'
git_add_folder <- function(repo, fld_path, git_path_f, up_ignores = c()) {
  ignores <- character(0)

  git_ignore_file <- file.path(fld_path, ".gitignore")
  if (file.exists(git_ignore_file)) {
    ignores <- readLines(git_ignore_file)
    ignores <- ignores[ignores != ""]
  }

  new_ignores <- up_ignores
  new_ignore_file <- file.path(fld_path, "__rc_ignore")
  if (file.exists(new_ignore_file)) {
    new_ignores <- c(new_ignores, readLines(new_ignore_file))
    new_ignores <- new_ignores[new_ignores != ""]
    unlink(new_ignore_file, force = TRUE)
  }

  if (length(ignores) == 0 || length(setdiff(new_ignores, ignores)) > 0) {
    ignores <- unique(c(new_ignores, ignores))
    writeLines(ignores, con = git_ignore_file) # it will be added later
  }
  ignores <- c(".git", ignores)

  toadd <- list.files(fld_path, all.files = TRUE, no.. = TRUE)
  if (length(ignores) > 0) {
    inc_ignores <- ignores[grepl("^[^!]", ignores)]
    inc_ignores_rx <- utils::glob2rx(inc_ignores)

    exc_ignores <- ignores[grepl("^!", ignores)]
    exc_ignores <- gsub("^!", "", exc_ignores)
    exc_ignores_rx <- utils::glob2rx(exc_ignores)

    toadd <- lapply(X = toadd,
                    FUN = function(fn) {
                      matched_rx <- lapply(exc_ignores_rx, function(rx) grepl(rx, fn))
                      if (any(unlist(matched_rx))) {
                        return(fn)
                      }
                      matched_rx <- lapply(inc_ignores_rx, function(rx) grepl(rx, fn))
                      if (any(unlist(matched_rx))) {
                        return()
                      }
                      return(fn)
                    })
    toadd <- unlist(toadd)
  }

  files_toadd <- toadd[!dir.exists(file.path(fld_path, toadd))]
  if (length(files_toadd) > 0) {
    git2r::add(repo, git_path_f(files_toadd))
  }

  # ignores on subsequent folders
  down_path_ignores <- lapply(ignores, function(ig) unlist(strsplit(ig, "\\\\|/")))

  fldrs_toadd <- toadd[dir.exists(file.path(fld_path, toadd))]
  for (subfld in fldrs_toadd) {
    sub_ignores <- lapply(down_path_ignores,
                          function(ig_path) {
                            if (gsub("^!", "", ig_path[[1]]) != subfld) {
                              return()
                            }

                            ig <- paste(ig_path[-1], collapse = .Platform$file.sep)
                            if (grepl(ig_path[[1]], "^!")) {
                              ig <- paste0("!", ig)
                            }
                          })
    sub_ignores <- unlist(sub_ignores)

    git_add_folder(repo,
                   fld_path = file.path(fld_path, subfld),
                   git_path_f = function(...) git_path_f(subfld, ...),
                   up_ignores = sub_ignores)
  }
}

#'
#' Implementation of rc_adapter_get_version for GIT rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_get_version.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  repo <- git2r::repository(dir, discover = TRUE)
  st <- git2r::status(repo)

  # detect head target
  # in git2r > 0.21.0 head is deprecated and causes warnings: repository_head should be used
  # in earlier versions repository_head is not available and check will complain if referencing it directly
  git2r_ver <- paste0(utils::packageVersion("git2r"))
  head_branch <- if (utils::compareVersion(git2r_ver, "0.21.0") > 0) {
    get_pkg_intern("git2r", "repository_head")() # from 99_rpatches.R
  } else {
    git2r::head(repo)
  }
  head_target <- git2r::branch_target(head_branch)

  # detect if HEAD commit is tagged
  repo_tags <- git2r::tags(repo)
  assert(length(repo_tags), "Failed to find tags. Are there any tags in repository?")

  tag_target <- function(act_tag) {
    if (class(act_tag) == "git_tag") {
      return(ifelse(isS4(act_tag), act_tag@target, act_tag$target))
    } else {
      return(ifelse(isS4(act_tag), act_tag@sha, act_tag$sha))
    }
  }
  head_tag <- names(repo_tags)[vapply(X = repo_tags,
                                      FUN = function(x) tag_target(x) == head_target,
                                      FUN.VALUE = TRUE)]
  assert(length(head_tag), "Failed to find HEAD commit tag. Is it tagged?")
  assert(length(head_tag) == 1, "More than one HEAD commit tag found: %s.", paste(head_tag, collapse = ", "))

  # detect if working copy needs update
  diff_working_tree <- git2r::diff(repo, index = FALSE)
  diff_head <- git2r::diff(repo, index = TRUE)

  diff_working_tree_files <- if (isS4(diff_working_tree)) {
    diff_working_tree@files
  } else {
    diff_working_tree$files
  }
  diff_head_files <- if (isS4(diff_head)) {
    diff_head@files
  } else {
    diff_head$files
  }

  needs_update <- length(diff_working_tree_files) + length(diff_head_files) > 0

  return(list(
    has_changes = length(st$staged) + length(st$untracked) + length(st$unstaged) > 0,
    revision = head_tag,
    needs_update = needs_update
  ))
}


#'
#' Implementation of rc_adapter_remove_admins for GIT rc adapted.
#'
#' @keywords internal
#' @noRd
#'
rc_adapter_remove_admins.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  admins <- list.files(dir, pattern = ".gitignore", recursive = TRUE, all.files = TRUE)
  unlink(file.path(dir, admins), force = TRUE)
}
