#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Repo manager working on S3 repository. Created by rsuite_repo_adapter_s3.
#----------------------------------------------------------------------------


#'
#' Create repo manager to manager repository in directory.
#'
#' @param url url to S3 repository (type: character)
#' @param types package types to manage  (type: character)
#' @param rver R version to manage repository for. Can be NA if managing only
#'   source packages (type: character).
#'
#' @return object of type rsuite_repo_manager
#'
#' @keywords internal
#' @noRd
#'
repo_manager_s3_create <- function(url, types, rver, s3_profile) {
  assert(is_nonempty_char1(url), "Non empty character(1) expected for url")
  assert(grepl("^https?://[\\w\\d\\._-]+\\.s3\\.amazonaws\\.com(/.*)?$", url, perl = TRUE),
         paste0("Invalid url specified.",
                " Amazon S3 url should have <schema>://<bucket>.s3.amazonaws.com/<path> form;",
                " Url does not have required form: %s"), url)
  assert( (is.na(rver) && all(types == "source")) || is_nonempty_char1(rver),
         "Non empty character(1) expected for rver")

  if (is.null(s3_profile)) {
    s3_profile <- "default"
  }
  assert(is_nonempty_char1(s3_profile), "Non empty character(1) expected for url")



  bucket_url <- gsub("^https?://([\\w\\d\\._-]+)\\.s3\\.amazonaws\\.com(/.*)?$", "s3://\\1\\2",
                     url,
                     perl = TRUE)
  aws_cmd <- Sys.which("aws")
  assert(aws_cmd != "",
         "Failed to detect AWS CLI for management of S3 %s bucket.", bucket_url)


  # Create temporary file, upload it and delete. If succeeds bucket is potentially RW.
  bucket_base <- gsub("^s3://([^/]+)(/.+)?$", "\\1", bucket_url, perl = TRUE)
  tmp_file <- tempfile(paste0(bucket_base, "_rwtest_"), fileext = ".txt")
  writeLines(sprintf("It is a file for testing RW access to %s", bucket_url),
             con = tmp_file)

  tryCatch({
    pkg_logdebug("Uploading file onto %s to test RW permissions ...", bucket_url)

    upl_lines <- get_cmd_outlines("aws cp",
                                  "%s s3 --profile=%s cp %s %s",
                                  aws_cmd, s3_profile, tmp_file, bucket_url)
    upl_success <- any(grepl("^upload: ", upl_lines))
  },
  finally = {
    unlink(tmp_file, force = TRUE)
  })

  assert(upl_success,
         "Uploading file onto %s to test RW permissions ... failed. Probably no access.",
         bucket_url)

  file_url <- sprintf("%s/%s", bucket_url, basename(tmp_file))
  pkg_logdebug("Cleanup: removing %s ...", file_url)

  cln_lines <- get_cmd_outlines("aws rm", "%s s3 --profile=%s rm %s", aws_cmd, s3_profile, file_url)
  if (!any(grepl("^delete: ", cln_lines))) {
    pkg_logdebug("Cleanup: removing %s ... failed", file_url)
  }

  result <- list(
    s3_profile = s3_profile,
    types = types,
    rver = rver,
    url = url,
    bucket_url = bucket_url,
    aws_cmd = aws_cmd
  )
  class(result) <- c("rsuite_repo_manager_s3", "rsuite_repo_manager")
  return(result)
}

#'
#' Implementation of repo_manager_get_info for rsuite_repo_manager_s3.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_get_info.rsuite_repo_manager_s3 <- function(repo_manager) {
  return(list(
    types = repo_manager$types,
    rver = repo_manager$rver,
    url = repo_manager$url
  ))
}


#'
#' Implementation of repo_manager_init for rsuite_repo_manager_s3.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_init.rsuite_repo_manager_s3 <- function(repo_manager, types, ...) {
  if (missing(types)) {
    types <- repo_manager$mgr_types
  }

  tmp_file <- tempfile("PACKAGES_")
  rver <- repo_manager$rver
  inited_types <- c()

  # detect which types must be initialized
  tryCatch({
    for (tp in types) {
      s3_pkgs_url <- paste0(
        rsuite_contrib_url(repo_manager$bucket_url, type = tp, rver = rver),
        "/PACKAGES")
      dld_lines <- get_cmd_outlines("aws cp",
                                    "%s s3 --profile=%s cp %s %s",
                                    repo_manager$aws_cmd,
                                    repo_manager$s3_profile,
                                    s3_pkgs_url,
                                    tmp_file)
      dld_success <- any(grepl("^download: ", dld_lines))
      if (dld_success) {
        types <- setdiff(types, tp)
        inited_types <- c(inited_types, tp)
      }
    }
  },
  finally = {
    unlink(tmp_file, force = TRUE)
  })

  if (!length(types)) {
    pkg_loginfo("Repository %s inited already for %s types.",
                url, paste(inited_types, collapse = ", "))
    return(FALSE)
  }

  tmp_dir <- tempfile("s3_init_")
  tryCatch({
    tmp_ra <- repo_manager_dir_create(tmp_dir, types, rver)
    repo_manager_init(tmp_ra)

    pkg_loginfo("Initializing %s for %s types ...",
                repo_manager$bucket_url, paste(types, collapse = ", "))

    sync_lines <- get_cmd_outlines("aws sync",
                                   "%s s3 --profile=%s sync %s %s --acl public-read",
                                   repo_manager$aws_cmd,
                                   repo_manager$s3_profile,
                                   tmp_dir,
                                   repo_manager$bucket_url)
    assert(any(grepl("^upload: ", sync_lines)),
           "Failed to initialize repository at %s for %s",
           repo_manager$bucket_url, tp)

    pkg_loginfo("Initializing %s for %s types ... done",
                repo_manager$bucket_url, paste(types, collapse = ", "))
  },
  finally = {
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  })

  return(TRUE)
}


#'
#' Implementation of repo_manager_upload for rsuite_repo_manager_s3.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_upload.rsuite_repo_manager_s3 <- function(repo_manager, src_dir, types) {
  if (missing(types)) {
    types <- repo_manager$types
  }
  rver <- repo_manager$rver

  for (tp in types) {
    src_path <- rsuite_contrib_url(src_dir, type = tp, rver = rver)
    if (!dir.exists(src_path)) {
      pkg_logdebug("No package files found in %s.", src_path)
      next
    }

    pkg_loginfo("Preparing %s for upload ...", file.path(src_path, "PACKAGES"))

    s3_pkgs_url <- paste0(
      rsuite_contrib_url(repo_manager$bucket_url, type = tp, rver = rver),
      "/PACKAGES")
    loc_pkgs_path <- file.path(src_path, "PACKAGES")

    dld_lines <- get_cmd_outlines("aws cp",
                                  "%s s3 --profile=%s cp %s %s",
                                  repo_manager$aws_cmd,
                                  repo_manager$s3_profile,
                                  s3_pkgs_url,
                                  loc_pkgs_path)
    dld_success <- any(grepl("^download: ", dld_lines))
    if (!dld_success) {
      pkg_loginfo(paste0("Failed to retrieve PACKAGES for %s from %s.",
                         " Repository probably not inited.",
                         " Will assume it is empty."),
                  tp, repo_manager$bucket_url)
      s3_pkgs_dcf <- NULL
    } else {
      s3_pkgs_dcf <- read.dcf(loc_pkgs_path)
    }

    # Rewrite loc_pkgs_path with local packages content and read it in
    rsuite_write_PACKAGES(src_path, tp)
    loc_pkgs_dcf <- read.dcf(loc_pkgs_path)

    pkgs_dcf <- .merge_dcfs(loc_pkgs_dcf, s3_pkgs_dcf)
    # remove all subsequent duplicates
    pkgs_dcf <- pkgs_dcf[!duplicated(pkgs_dcf[, c("Package", "Version")]), ]

    # ... write result to PACKAGES, PACKAGES.gz, PACKAGES.rds
    write.dcf(pkgs_dcf, file = file.path(src_path, "PACKAGES"))
    pkgsgz_con <- gzfile(file.path(src_path, "PACKAGES.gz"), "wt")
    tryCatch({
      write.dcf(pkgs_dcf, file = pkgsgz_con)
    },
    finally = close(pkgsgz_con))

    dst_url <- rsuite_contrib_url(repo_manager$bucket_url, type = tp, rver = rver)
    pkg_loginfo("... done; synchronizing to %s ...", dst_url)

    sync_lines <- get_cmd_outlines("aws sync",
                                   "%s s3 --profile=%s sync %s %s --acl public-read",
                                   repo_manager$aws_cmd,
                                   repo_manager$s3_profile,
                                   src_path,
                                   dst_url)
    assert(any(grepl("^upload: ", sync_lines)),
           "Failed to syncronize repository at %s for %s",
           repo_manager$bucket_url, tp)

    # remove PACKAGES.rds as it does not support package history
    get_cmd_outlines("aws rm", "%s s3 --profile=%s rm %s",
                     repo_manager$aws_cmd,
                     repo_manager$s3_profile,
                     paste0(dst_url, "/PACKAGES.rds"))

    pkg_loginfo("... done")
  }
}

#'
#' Merges two dsfs into one. Converts them to data.frame before.
#'
#' Needs to control if they have same columns.
#'
#' Result is data.frame.
#'
#' @keywords internal
#' @noRd
#'
.merge_dcfs <- function(dcf1, dcf2) {
  dcf1 <- as.data.frame(dcf1, stringsAsFactors = FALSE)
  dcf2 <- as.data.frame(dcf2, stringsAsFactors = FALSE)

  if (!nrow(dcf1)) {
    return(dcf2)
  }
  if (!nrow(dcf2)) {
    return(dcf1)
  }

  cols <- union(colnames(dcf1), colnames(dcf2))
  if (length(setdiff(cols, colnames(dcf1))) > 0) {
    dcf1[, setdiff(cols, colnames(dcf1))] <- as.character(NA)
    dcf1 <- dcf1[, cols]
  }

  if (length(setdiff(cols, colnames(dcf2))) > 0) {
    dcf2[, setdiff(cols, colnames(dcf2))] <- as.character(NA)
    dcf2 <- dcf2[, cols]
  }

  return(rbind(dcf1, dcf2))
}


#'
#' Implementation of repo_adapter_stop_management for rsuite_repo_manager_s3.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_remove.rsuite_repo_manager_s3 <- function(repo_manager, toremove, type) {
  dst_url <- rsuite_contrib_url(repo_manager$bucket_url, type = type, rver = repo_manager$rver)

  pkg_loginfo("Updating %s ...", file.path(dst_url, "PACKAGES"))

  tmp_dir <- tempfile(pattern = "s3_remove_")
  dir.create(path = tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, force = TRUE, recursive = TRUE))

  dld_lines <- get_cmd_outlines("aws cp",
                                "%s s3 --profile=%s cp %s %s",
                                repo_manager$aws_cmd,
                                repo_manager$s3_profile,
                                paste0(dst_url, "/PACKAGES"),
                                file.path(tmp_dir, "PACKAGES"))
  dld_success <- any(grepl("^download: ", dld_lines))
  if (!dld_success) {
    pkg_loginfo(paste0("Failed to retrieve PACKAGES for %s from %s.",
                       " Repository probably not inited.",
                       " Will assume it is empty."),
                type, repo_manager$bucket_url)
    return(data.frame(Package = as.character(), Version = as.character()))
  }

  pkgs_dcf <- data.frame(read.dcf(file.path(tmp_dir, "PACKAGES")), stringsAsFactors = FALSE)

  to_remove_mask <- (sprintf("%s_%s", pkgs_dcf$Package, pkgs_dcf$Version)
                     %in% sprintf("%s_%s", toremove$Package, toremove$Version))
  if (!any(to_remove_mask)) {
    pkg_loginfo("No packages to remove.")
    return(data.frame(Package = as.character(), Version = as.character()))
  }

  left_pkgs_dcf <- pkgs_dcf[!to_remove_mask, ]

  # ... write result to PACKAGES and PACKAGES.gz
  write.dcf(left_pkgs_dcf, file = file.path(tmp_dir, "PACKAGES"))
  pkgsgz_con <- gzfile(file.path(tmp_dir, "PACKAGES.gz"), "wt")
  tryCatch({
    write.dcf(left_pkgs_dcf, file = pkgsgz_con)
  },
  finally = close(pkgsgz_con))

  pkg_loginfo("... done; synchronizing PACKAGES to %s ...", dst_url)

  sync_lines <- get_cmd_outlines("aws sync",
                                 "%s s3 --profile=%s sync %s %s --acl public-read",
                                 repo_manager$aws_cmd,
                                 repo_manager$s3_profile,
                                 tmp_dir,
                                 dst_url)
  assert(any(grepl("^upload: ", sync_lines)),
         "Failed to syncronize repository at %s for %s",
         repo_manager$bucket_url, type)

  pkg_loginfo("... done; removing package files ...")

  to_remove <- pkgs_dcf[to_remove_mask, ]
  rm_lines <- get_cmd_outlines("aws rm", '%s s3 --profile=%s rm %s --recursive --exclude "*" %s',
                               repo_manager$aws_cmd,
                               repo_manager$s3_profile,
                               dst_url,
                               paste(sprintf('--include "%s_%s.*"', to_remove$Package, to_remove$Version),
                                     collapse = " "))
  if (length(rm_lines[grepl("^delete: ", rm_lines)]) < nrow(to_remove)) {
    pkg_logwarn("Failed to remove some package files from %s", dst_url)
  }

  pkg_loginfo("... done")

  return(to_remove[, c("Package", "Version")])
}

#'
#' Implementation of repo_manager_destroy for rsuite_repo_manager_s3.
#'
#' @keywords internal
#' @noRd
#'
repo_manager_destroy.rsuite_repo_manager_s3 <- function(repo_manager) {
  # noop
}
