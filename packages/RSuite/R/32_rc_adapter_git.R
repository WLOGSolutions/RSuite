#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# RC adapter working with GIT.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#'
#' Creates RC adapter to handle GIT repos.
#'
#' @param name under which RC adapter will be registered in RSuite.
#' @return object of type rsuite_rc_adapter
#'
rc_adapter_create_git <- function(name) {
  result <- rc_adapter_create_base(name)

  class(result) <- c('rsuite_rc_adapter_git', class(result))
  return(result)
}

#'
#' @keywords internal
#'
#' Implementation of rc_adapter_is_under_control for GIT RC adapted.
#'
#' Checks if folder is under GIT version control: contains administrative .git
#' folder inside or up folder tree. If not, the folder is not under VC.
#'
rc_adapter_is_under_control.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  tryCatch({
    repo <- git2r::repository(dir, discover = T)
    pkg_logdebug("Git working directory detected: %s", git2r::workdir(repo))
    TRUE
  }, error = function(e) {
    return(FALSE)
  })
}

#'
#' @keywords internal
#'
#' Implementation of rc_adapter_prj_struct_add for GIT RC adapted.
#'
rc_adapter_prj_struct_add.rsuite_rc_adapter_git <- function(rc_adapter, params) {
  repo <- git2r::repository(params$prj_path, discover = T)

  prj_gitbase <- sub(rsuite_fullUnifiedPath(workdir(repo)), "", params$prj_path, fixed = T)
  if (!nchar(prj_gitbase)) {
    git_path <- file.path
  } else {
    git_path <- function(...) { file.path(prj_gitbase, ...) }
  }

  writeLines(c(".Rproj.user", ".Rhistory", ".Rdata", ".Rbuildignore", ".Ruserdata", "config.txt"),
             con = file.path(params$prj_path, ".gitignore"))
  git2r::add(repo, git_path(".gitignore"))

  writeLines(c("intrepo"),
             con = file.path(params$prj_path, "deployment", ".gitignore"))
  git2r::add(repo, git_path("deployment", ".gitignore"))

  writeLines(c("*", "!.gitignore"),
             con = file.path(params$prj_path, "deployment", "libs", ".gitignore"))
  git2r::add(repo, git_path("deployment", "libs", ".gitignore"))

  writeLines(c("*", "!.gitignore"),
             con = file.path(params$prj_path, "deployment", "sbox", ".gitignore"))
  git2r::add(repo, git_path("deployment", "sbox", ".gitignore"))

  writeLines(c("*", "!.gitignore"),
             con = file.path(params$prj_path, "logs", ".gitignore"))
  git2r::add(repo, git_path("logs", ".gitignore"))

  writeLines("",
             con = file.path(params$prj_path, "packages", ".gitignore"))
  git2r::add(repo, git_path("packages", ".gitignore"))

  writeLines("",
             con = file.path(params$prj_path, "R", ".gitignore"))
  git2r::add(repo, git_path("R", ".gitignore"))

  git2r::add(repo, git_path(c("PARAMETERS", "config_templ.txt")))
  git2r::add(repo, git_path(c(".Rprofile", "*.Rproj")))
  git2r::add(repo, git_path("deployment", c("*.r", "*.R")))
  git2r::add(repo, git_path("R", c("*.r", "*.R", ".Rprofile")))

  if (dir.exists(file.path(params$prj_path, "tests"))) {
    writeLines("", con = file.path(params$prj_path, "tests", ".gitignore"))
    git2r::add(repo,
               git_path("tests",
                        c("*.R", "*.r", "*.Rproj", ".Rprofile", ".gitignore")))
  }
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_pkg_struct_add for GIT rc adapted.
#'
rc_adapter_pkg_struct_add.rsuite_rc_adapter_git <- function(rc_adapter, params, name) {
  pkg_dir <- file.path(params$pkgs_path, name)
  repo <- git2r::repository(pkg_dir, discover = T)

  pkg_gitbase <- sub(rsuite_fullUnifiedPath(workdir(repo)), "", pkg_dir, fixed = T)
  if (!nchar(pkg_gitbase)) {
    git_path <- file.path
  } else {
    git_path <- function(...) { file.path(pkg_gitbase, ...) }
  }

  writeLines("", con = file.path(pkg_dir, ".gitignore"))
  git2r::add(repo, git_path(".gitignore"))

  writeLines("", con = file.path(pkg_dir, "data", ".gitignore"))
  git2r::add(repo, git_path("data", ".gitignore"))

  writeLines("", con = file.path(pkg_dir, "inst", ".gitignore"))
  git2r::add(repo, git_path("inst", ".gitignore"))

  writeLines(c("*", "!.gitignore"),
             con = file.path(pkg_dir, "man", ".gitignore"))
  git2r::add(repo, git_path("man", ".gitignore"))

  git2r::add(repo, git_path(c("DESCRIPTION", "NAMESPACE", "NEWS", "*.Rproj", ".Rprofile")))
  git2r::add(repo, git_path("R", c("*.R", "*.r")))

  if (dir.exists(file.path(pkg_dir, "tests"))) {
    writeLines("", con = file.path(pkg_dir, "tests", ".gitignore"))
    git2r::add(repo, git_path("tests", c("*.R", "*.r", ".gitignore")))
  }
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_get_version for GIT rc adapted.
#'
rc_adapter_get_version.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  repo <- git2r::repository(dir, discover = T)
  st <- git2r::status(repo)

  # detect head target
  head_branch <- git2r::head(repo)
  head_target <- git2r::branch_target(head_branch)

  # detect if HEAD commit is tagged
  repo_tags <- git2r::tags(repo)
  assert(length(repo_tags), "Failed to find tags. Are there any tags in repository?")

  tag_target <- function(act_tag) {
    if (class(act_tag) == "git_tag") {
      return(act_tag@target)
    } else {
      return(act_tag@sha)
    }
  }
  head_tag <- names(repo_tags)[sapply(repo_tags, function(x) tag_target(x) == head_target)]
  assert(length(head_tag), "Failed to find HEAD commit tag. Is it tagged?")
  assert(length(head_tag) == 1, "More than one HEAD commit tag found.")

  # detect if working copy needs update
  diff_working_tree <- git2r::diff(repo, index = FALSE)
  diff_head <- git2r::diff(repo, index = TRUE)
  needs_update <- length(diff_working_tree@files) + length(diff_head@files) > 0

  return(list(
    has_changes = length(st$staged) + length(st$untracked) + length(st$unstaged) > 0,
    revision = head_tag,
    needs_update = needs_update
  ))
}


#'
#' @keywords internal
#'
#' Implementation of rc_adapter_remove_admins for GIT rc adapted.
#'
rc_adapter_remove_admins.rsuite_rc_adapter_git <- function(rc_adapter, dir) {
  admins <- list.files(dir, pattern = ".gitignore", recursive = TRUE, all.files = TRUE)
  unlink(file.path(dir, admins), force = T)
}
