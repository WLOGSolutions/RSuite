#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities for managing project/package templates
#----------------------------------------------------------------------------

#'
#' Retrieves base folder(s) for template in user or global space.
#'
#' @param tmpl name of the template in user space. (type: character)
#'
#' @return list of paths in search order there template should be searched for.
#'   (type: character)
#'
#' @keywords internal
#' @noRd
#'
.get_base_tmpl_dirs <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  base_tmpl_dirs <- c()

  # look for templates in the user templates directory
  user_templ_base_dir <- get_user_templ_base_dir(create = FALSE)
  if (!is.null(user_templ_base_dir)) {
    user_tmpl_dir <- file.path(user_templ_base_dir, tmpl)
    base_tmpl_dirs <- c(base_tmpl_dirs, user_tmpl_dir)
  }

  # look for templates in the global templates directory
  base_tmpl_dirs <- c(base_tmpl_dirs, get_global_templ_dir())

  if (length(base_tmpl_dirs) == 0) {
    return(NULL)
  }

  base_tmpl_dirs <- base_tmpl_dirs[dir.exists(base_tmpl_dirs)]
  return(base_tmpl_dirs)
}

#'
#' Retrieves path to project template in user space or global (no builtin is considered).
#'
#' @param tmpl name of the project template or it's path. (type: character)
#'
#' @return path to the requested project template or NULL if not found. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_prj_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for template name")

  # treat tmpl as the path to the template
  if (dir.exists(tmpl)) {
    return(tmpl)
  }

  prj_templ_dirs <- file.path(.get_base_tmpl_dirs(tmpl), "project")
  if (length(prj_templ_dirs) == 0){
    return(NULL)
  }

  prj_templ_dirs <- prj_templ_dirs[dir.exists(prj_templ_dirs)]
  if (length(prj_templ_dirs) == 0) {
    return(NULL)
  }

  return(prj_templ_dirs[[1]]) # take first available in search path
}


#'
#' Retrieves path to package template.
#'
#' @param tmpl name of the project template or it's path. (type: character)
#'
#' @return path to the requested package template or NULL if not found. (type: character)
#'
#' @keywords internal
#' @noRd
#'
get_pkg_tmpl_dir <- function(tmpl) {
  assert(!is.null(tmpl) && is.character(tmpl) && length(tmpl) == 1 && nchar(tmpl) > 0,
         "Non empty character(1) required for name")

  # treat tmpl as the path to the template
  if (dir.exists(tmpl)) {
    return(tmpl)
  }

  pkg_templ_dirs <- file.path(.get_base_tmpl_dirs(tmpl), "package")
  if (length(pkg_templ_dirs) == 0){
    return(NULL)
  }

  pkg_templ_dirs <- pkg_templ_dirs[dir.exists(pkg_templ_dirs)]
  if (length(pkg_templ_dirs) == 0) {
    return(NULL)
  }

  return(pkg_templ_dirs[[1]])
}

#'
#' Checks whether folder has proper structure for project template.
#'
#' Logs warning if some requirements are not met.
#'
#' @param tmpl_dir path to project template. Must exist.
#'    (type: character).
#'
#' @return invisible TRUE if the folder structure satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'
validate_prj_tmpl_struct <- function(tmpl_dir) {
  stopifnot(dir.exists(tmpl_dir))

  required_files <- c("PARAMETERS")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  requirements_check <- required_files %in% files
  result <- all(requirements_check)

  if (!result) {
    pkg_logwarn("Template under %s does not contain required files: %s",
                tmpl_dir, required_files[!requirements_check])
  }
  return(invisible(result))
}


#'
#' Checks whether folder has proper structure for package template.
#'
#' Logs warning if some requirements are not met.
#'
#' @param tmpl_dir path to package template. Must exist.
#'    (type: character).
#'
#' @return invisible TRUE if the folder structure satisfies requirements.
#'
#' @keywords internal
#' @noRd
#'
validate_pkg_tmpl_struct <- function(tmpl_dir) {
  stopifnot(dir.exists(tmpl_dir))

  required_files <- c("DESCRIPTION")
  files <- list.files(tmpl_dir, include.dirs = TRUE, recursive = FALSE)

  requirements_check <- required_files %in% files
  result <- all(requirements_check)

  if (!result) {
    pkg_logwarn("Template under %s does not contain required files: %s",
                tmpl_dir, required_files[!requirements_check])
  }

  return(invisible(result))
}


#'
#' Replaces markers in the given input using keywords
#'
#' @param keywords list of data to replace markers with.
#'    (type: list).
#'
#' @param input lines with markers to replace.
#'    (type: character(N))
#'
#' @return input with markers replaced.
#'    (type: character(N))
#'
#' @keywords internal
#' @noRd
#'
replace_markers <- function(keywords, input) {
  keyword_data <- data.frame(Marker = names(keywords), Keyword = keywords)
  keyword_data$Marker <- sprintf("__%s__", keyword_data$Marker)

  for (i in seq_len(nrow(keyword_data))) {
    input <- gsub(keyword_data$Marker[i], keyword_data$Keyword[i], input)
  }

  return(input)
}

#'
#' Renames files containg markers.
#'
#' @param keywords list of data to replace markers with.
#'    (type: list).
#'
#' @param input lines with filenames with markers to replace.
#'    (type: character(N))
#'
#' @return input with markers replaced.
#'    (type: character(N))
#'
#' @keywords internal
#' @noRd
#'
rename_files_with_markers <- function(keywords, files) {
  if (length(files) == 0) {
    return()
  }
  # replace all markers in file names
  files_renamed <- replace_markers(keywords, files) # from 58_templates.R

  # retrieve files that had markers in their names
  files_renamed_indexes <- files_renamed != files
  files_renamed <- files_renamed[files_renamed_indexes]
  files <- files[files_renamed_indexes]
  files_with_markers <- files

  # check if any files will be overwritten while renaming and exclude them
  # to prevent losing any saved changes
  new_files <- !file.exists(files_renamed)
  files <- files[new_files]
  files_renamed <- files_renamed[new_files]


  if (length(files_renamed) > 0) {
    success <- file.rename(files, files_renamed)
    if (length(success) <= 0) {
      pkg_logwarn("Failed to rename files containg markers in their names: %s", files)
    }
  } else if (length(files_with_markers) > 0) {
    # delete files that would overwrite changes
    success <- file.remove(files_with_markers)
    if (length(success) <= 0) {
      pkg_logwarn("Failed to delete unnecessary files that already exist in the project %s", files_with_markers)
    }
  }
}

#'
#' Retrieves folder where user package and project templates are located.
#'
#' Folder path is taken from rsuite.user_templ_path option. If not specified
#' user templates will not be used.
#'
#' @param create if TRUE will create user templates folder (if specified).
#'   (type: logical(1), default: FALSE)
#'
#' @return path to user templates folder retrieved or NULL if not specified or
#'   failed to create.
#'
#' @keywords internal
#' @noRd
#'
get_user_templ_base_dir <- function(create = FALSE) {
  user_templ_base_dir <- getOption("rsuite.user_templ_path", "")
  if (nchar(user_templ_base_dir) == 0) {
    return()
  }

  if (get_os_type() == "windows") {
    user_templ_base_dir <- utils::shortPathName(user_templ_base_dir)
  }

  if (dir.exists(user_templ_base_dir)) {
    return(user_templ_base_dir)
  }

  if (!any(create)) {
    return()
  }

  if (dir.create(user_templ_base_dir, recursive = TRUE, showWarnings = FALSE)) {
    return(user_templ_base_dir)
  }

  pkg_logwarn("Failed to create folder for user project and package templates (%s)", user_templ_base_dir)
  return()
}

#'
#' Retrieves the folder where global package and project templates are located.
#'
#' This path only concerns Linux users as it returns the '/etc/.rsuite/templates' path
#'
#' @return path to global templates folder retrieved or NULL (if does not exist or
#' working on Windows platform)
#'
#' @keywords internal
#' @noRd
#'
get_global_templ_dir <- function() {
  if (!(get_os_type() %in% c("macos", "unix"))) {
    return(NULL)
  }

  global_tmpl_dir <- "/etc/.rsuite/templates"
  if (!dir.exists(global_tmpl_dir)) {
    return(NULL)
  }
  return(global_tmpl_dir)
}

#'
#' Retrieves builtin template zip path.
#'
#' @return path to builtin templates zip package. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
get_builtin_templs_zip <- function() {
  return(system.file(file.path("extdata", "builtin_templates.zip"), package = "RSuite"))
}

#'
#' Extracts builtin templates into tempdir() and returns path to the folder.
#'
#' Will assert if fails to extract builtin templates.
#'
#' @return path to builtin templates extracted temp folder. (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
get_builtin_templs_temp_base <- function() {
  temp_dir <- tempfile(pattern = "builtin_templs_")
  extracted_files <- suppressWarnings({
    utils::unzip(zipfile = get_builtin_templs_zip(), exdir = temp_dir)
  })
  assert(length(extracted_files) > 0,
         "Failed to extract builtin templates. Check RSuite installation.")
  return(temp_dir)
}



#'
#' Checks whether a file is a binary file.
#'
#' @param file path to the file to check (type: character(1))
#'
#' @param blocksize size of the block based on which the file will be identified
#' as binary or not (type: numeric)
#'
#' @return TRUE if a file is a binary file
#'
#' @keywords internal
#' @noRd
#'
is_binary <- function(file, blocksize = 512) {
  block <- readBin(file, "raw", n = blocksize)
  null_byte <- as.raw(00)

  special_chars <- sapply(c("\n", "\r", "\t", "\f", "\b"), charToRaw)
  names(special_chars) <- NULL
  text_chars <- c(as.raw(32:127), special_chars)

  if (null_byte %in% block) {
    return(TRUE)
  } else if (length(block) == 0) {
    return(FALSE)
  }

  nontext_chars <- block[!block %in% text_chars]

  return(length(nontext_chars) / length(block) > 0.3)
}
