#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo manager working on directory. Created by rsuite_repo_adapter_dir.
#----------------------------------------------------------------------------

#'
#' Create repo manager to manager repository in directory.
#'
#' @param path path to repository folder (type: character)
#' @param types package types to manage  (type: character)
#' @param rver R version to manage repository for. Can be NA if managing only
#'   source packages (type: character).
#'
#' @return object of type rsuite_repo_manager
#'
#' @keywords internal
#' @noRd
#'
repo_manager_dir_create <- function(path, types, rver) {
  assert( (is.na(rver) && all(types == "source")) || is_nonempty_char1(rver),
         "Non empty character(1) expected for rver")
  assert(is_nonempty_char1(path), "Non empty character(1) expected for path")

  if (!dir.exists(path)) {
    success <- dir.create(path, recursive = TRUE)
    assert(success, "Failed to create repository at %s", path)
  }
  path <- rsuite_fullUnifiedPath(path)

  result <- list(
    path = path,
    types = types,
    rver = rver
  )
  class(result) <- c("rsuite_repo_manager_dir", "rsuite_repo_manager")
  return(result)
}

#'
#' Implementation of repo_manager_get_info for rsuite_repo_manager_dir.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_get_info.rsuite_repo_manager_dir <- function(repo_manager) {
  return(list(
    types = repo_manager$types,
    rver = repo_manager$rver,
    url = sprintf("file:///%s", repo_manager$path)
  ))
}

#'
#' Implementation of repo_manager_init for rsuite_repo_manager_dir.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_init.rsuite_repo_manager_dir <- function(repo_manager, types) {
  if (missing(types)) {
    types <- repo_manager$types
  }

  repo_path <- repo_manager$path

  was_inited <- TRUE
  for (tp in types) {
    tp_path <- rsuite_contrib_url(repo_path, type = tp, rver = repo_manager$rver)
    if (!dir.exists(tp_path)) {
      success <- dir.create(tp_path, recursive = TRUE)
      assert(success, "Failed to initialize repository for %s at %s", tp, repo_path)
    }
    if (!file.exists(file.path(tp_path, "PACKAGES"))) {
      rsuite_write_PACKAGES(tp_path, tp)
      was_inited <- FALSE
    }
  }

  return(invisible(was_inited))
}


#'
#' Implementation of repo_manager_upload for rsuite_repo_manager_dir..
#'
#' @keywords internal
#' @noRd
#'
repo_manager_upload.rsuite_repo_manager_dir <- function(repo_manager, src_dir, types) {
  if (missing(types)) {
    types <- repo_manager$types
  }

  for (tp in types) {
    src_path <- rsuite_contrib_url(src_dir, type = tp, rver = repo_manager$rver)
    if (!dir.exists(src_path)) {
      pkg_loginfo("No package files found in %s.", src_path)
      next
    }

    dst_path <- rsuite_contrib_url(repo_manager$path, type = tp, rver = repo_manager$rver)
    if (!dir.exists(dst_path)) {
      dir.create(dst_path, recursive = TRUE)
    }

    pkg_loginfo("Copying package files from %s into %s ...", src_path, dst_path)

    for (f in list.files(src_path)) {
      if (grepl("^PACKAGES", f)) {
        next
      }

      success <- file.copy(from = file.path(src_path, f), to = file.path(dst_path, f),
                           overwrite = TRUE)
      assert(success, "Failed to copy %s into %s.", f, dst_path)
    }
    rsuite_write_PACKAGES(dst_path, tp)

    pkg_loginfo("... done")
  }
}

#'
#' Implementation of repo_adapter_stop_management for rsuite_repo_manager_dir.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_remove.rsuite_repo_manager_dir <- function(repo_manager, toremove, type) {
  path <- rsuite_contrib_url(repo_manager$path, type = type, rver = repo_manager$rver)
  if (!dir.exists(path)) {
    return(data.frame(Package = as.character(), Version = as.character()))
  }

  toremove$Removed <- unlist(
    lapply(X = sprintf("%s_%s.*", toremove$Package, toremove$Version),
           FUN = function(pattern) {
             file <- list.files(path = path, pattern = pattern, full.names = TRUE)
             if (length(file) > 0) {
               unlink(file, force = TRUE) == 0
             } else {
               FALSE
             }
           })
  )

  rsuite_write_PACKAGES(path, type)

  res <- toremove[toremove$Removed, c("Package", "Version")]
  return(res)
}

#'
#' Implementation of repo_manager_destroy for rsuite_repo_manager_dir.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_destroy.rsuite_repo_manager_dir <- function(repo_manager) {
  # noop
}
