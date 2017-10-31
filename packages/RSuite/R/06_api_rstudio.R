#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Package API related to RStudio integration.
#----------------------------------------------------------------------------

#'
#' RStudio addin which asks user for project parameters and starts the project.
#'
#' @export
#'
rstudio_01_prj_start <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  dir <- selectDirectory(caption = "Select folder to start project in")
  if (is.null(dir)) {
    return(invisible())
  }
  prj_start(name = basename(dir), path = dirname(dir))

  projs <- list.files(dir, pattern = "*.Rproj", full.names = T)
  if (!length(projs)) {
    return(invisible())
  }

  resp <- showQuestion(title = "Project open inquery",
                       message = "Open the newly created project?",
                       ok = "Yes",
                       cancel = "No")
  if (resp) {
    openProject(projs[1], newSession = T)
  }
}

#'
#' Loads project in context. If fails to load asks user to select the project
#' folder.
#'
#' @keywords internal
#'
.rstudio_get_prj <- function() {
  prj <- tryCatch({
    prj_init()
  }, error = function(e) {
    dir <- selectDirectory(caption = "Select project folder")
    if (is.null(dir)) {
      return(invisible())
    }
    prj_init(path = dir)
  })
  return(prj)
}


#'
#' RStudio addin which asks user for package parameters and starts it.
#'
#' @export
#'
rstudio_02_prj_start_package <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  prj <- .rstudio_get_prj()
  if (is.null(prj)) {
    return(invisible())
  }

  pkg_name <- showPrompt(title = "Package name inquery",
                         message = "Enter name of package to create, please.",
                         default = "anRPackage")
  if (is.null(pkg_name)) {
    return(invisible())
  }

  assert(grepl("^[a-zA-Z0-9]+$", pkg_name),
         "Invalid package name. It must consist of characters and/or digits only.")

  prj_start_package(name = pkg_name, prj = prj)
}

#'
#' RStudio addin which installs project dependencies.
#'
#' If no project in context shows dialog to select project directory.
#'
#' @export
#'
rstudio_03_prj_install_deps <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  prj <- .rstudio_get_prj()
  if (!is.null(prj)) {
    prj_install_deps(prj = prj)
  }
}

#'
#' RStudio addin which builds the project.
#'
#' If no project in context shows dialog to select project directory.
#'
#' @export
#'
rstudio_04_prj_build <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  prj <- .rstudio_get_prj()
  if (!is.null(prj)) {
    prj_build(prj = prj)
  }
}

#'
#' RStudio addin which cleans up project dependencies.
#'
#' If no project in context shows dialog to select project directory.
#'
#' @export
#'
rstudio_05_prj_clean_deps <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  prj <- .rstudio_get_prj()
  if (!is.null(prj)) {
    prj_clean_deps(prj = prj)
  }
}


#'
#' RStudio addin which creates project deployment zip.
#'
#' If no project in context shows dialog to select project directory.
#'
#' @export
#'
rstudio_06_prj_zip <- function() {
  assert(isAvailable(), "No RStudio available")
  rstudio_ver <- as.character(getVersion())
  assert(compareVersion(rstudio_ver, "1.1.287") >= 0,
         "RStudio version(%s) is too old. RStudio v1.1.287 at least is required.",
         rstudio_ver)

  prj <- .rstudio_get_prj()
  if (!is.null(prj)) {
    prj_zip(prj = prj)
  }
}
