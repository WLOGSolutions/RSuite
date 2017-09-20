#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Utilities related to managing information about packages project, in file and
#  pkgzips.
#----------------------------------------------------------------------------

#'
#' @keywords internal
#' 
#' Retrieves list of project packages from package folder in order to be
#' able to solve internal dependencies.
#'
#' @param pkgs_path path to folder containing project packages
#' 
#' @return vector of project package names (type: character).
#'
build_project_pkgslist <- function(pkgs_path) {
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) { file.exists(file.path(pkgs_path, n, "DESCRIPTION")) })
  if (!length(project_packages)) {
    return()
  }
  
  deps <- list()
  for (pkg in project_packages) {
    pkg_deps <- desc_retrieve_dependencies(pkgs_path, pkg)
    pkg_deps <- gsub("^\\s+|\\s+$", "",
                     gsub(x = pkg_deps,
                          pattern = "(.*)(\\(.*\\))",
                          replacement = "\\1"))
    pkg_deps <- pkg_deps[pkg_deps %in% project_packages]
    deps[[pkg]] <- pkg_deps
  }
  
  res <- project_packages[sapply(project_packages, function(p) { length(deps[[p]]) == 0 })]
  project_packages <- setdiff(project_packages, res)
  while(length(project_packages) > 0) {
    res <- c(res, project_packages[sapply(project_packages, function(p) { all(deps[[p]] %in% res) })])
    project_packages <- setdiff(project_packages, res)
  }
  
  return(res)
}

#'
#' @keywords internal
#' 
#' Retrieves projects packages together with their versions.
#' 
#' Packages are retrieved in order to be able to solve internal dependencies.
#'
#' @param pkgs_path path to folder containing project packages
#' @return named list with package names and package versions (type: list).
#'
retrieve_project_pkgsvers <- function(pkgs_path) {
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) { file.exists(file.path(pkgs_path, n, "DESCRIPTION")) })
  result <- list()
  for(pkg in project_packages) {
    descr <- read.dcf(file.path(pkgs_path, pkg, "DESCRIPTION"))
    result[[pkg]] <- as.character(descr[1, 'Version'])
  }
  return(result)
}


#'
#' @keywords internal
#' 
#' Updates versions of packages.
#' 
#' @param pkgs_path path to folder containing project packages
#' @param pkgsvers named list with package names as names and version to set as
#'   values.
#'   
#' @return named list with package names and package versions (type: list).
#'
update_project_pkgsvers <-function(pkgs_path, pkgsvers) {
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) { file.exists(file.path(pkgs_path, n, "DESCRIPTION")) })
  for(pkg in project_packages) {
    if (!(pkg %in% names(pkgsvers))) {
      next
    }
    
    desc_file <- file.path(pkgs_path, pkg, "DESCRIPTION")
    descr <- read.dcf(desc_file)
    descr[, 'Version'] <- pkgsvers[[pkg]]
    write.dcf(descr, file = desc_file)
  }
}

#'
#' @keywords internal
#' 
#' Retrieves dependencies from package description file.
#'
#' @param pkgs_path path to folder containing project packages.
#' @param pkg package name to retrieve description from.
#' @param fields fields to search dependencies in. 
#'   (type: character, default: c("Imports", "Depends", "LinkingTo"))
#'
#' @return list of unparsed dependency declarations.
#'
desc_retrieve_dependencies <- function(pkgs_path, pkg, fields = c("Imports", "Depends", "LinkingTo")) {
  stopifnot(length(fields) > 0)
  
  desc_file <- file.path(pkgs_path, pkg, "DESCRIPTION")
  stopifnot(file.exists(desc_file))
  
  desc <- read.dcf(desc_file, fields = fields)
  pkgs <- c()
  for(f in fields) {
    if(is.na(desc[1, f])) { next }
    pkgs <- c(pkgs,
              strsplit(x = gsub(pattern = "\\n",
                                replacement = "",
                                x = desc[1, f]),
                       split = ",")[[1]])
  }
  return(pkgs)
}

#'
#' @keywords internal
#' 
#' Backups DESCRIPTION files or project packages.
#' 
#' @param pkgs_path path to folder containing project packages.
#' 
#' @return object sutable to restore DESCRIPTION files.
#' 
backup_pkgdesc_files <- function(pkgs_path) {
  bkp_dir <- tempfile(pattern = "pkgdescs_")
  success <- dir.create(bkp_dir)
  assert(success, "Failed to create folder to backup DESCRIPTION files of project packages")
    
  project_packages <- Filter(x = list.dirs(pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) { file.exists(file.path(pkgs_path, n, "DESCRIPTION")) })
  for(pkg in project_packages) {
    success <- file.copy(from = file.path(pkgs_path, pkg, "DESCRIPTION"),
                         to = file.path(bkp_dir, pkg),
                         copy.date = T)
    assert(success, "Failed to backup DESCRIPTION file of %s", pkg)
  } 
  
  result <- list(bkp_dir = bkp_dir, pkgs_path = pkgs_path)
  class(result) <- "pkgdesc_backup"
  return(result)
}

#'
#' @keywords internal
#' 
#' Restores backuped DESCRIPTION files of project packages.
#' 
#' @param bkp object returned by backup_pkgdesc_files
#' 
restore_pkgdesc_files <- function(bkp) {
  stopifnot(class(bkp) == "pkgdesc_backup")
  
  project_packages <- Filter(x = list.dirs(bkp$pkgs_path,
                                           full.names = FALSE,
                                           recursive = FALSE),
                             f = function(n) { file.exists(file.path(bkp$pkgs_path, n, "DESCRIPTION")) })
  for(pkg in project_packages) {
    bkp_file <- file.path(bkp$bkp_dir, pkg)
    if (file.exists(bkp_file)) {
      success <- file.copy(from = bkp_file, to = file.path(bkp$pkgs_path, pkg, "DESCRIPTION"),
                           overwrite = T, copy.date = T)
      assert(success, "Failed to restore DESCRIPTION file of %s", pkg)
    }
  }
  
  unlink(bkp$bkp_dir, recursive = T, force = T)
}


#'
#' @keywords internal
#'
#' Retrieves information on file packages passed.
#'
#' @param files file packages to retrieve info for. (type: character)
#' @return data.table of following structure
#' \describe{
#'   \item{Package}{Name of package. (type: character)}
#'   \item{Version}{Package version. (type: character)}
#'   \item{Type}{Type of package: source, win.binary, mac.binary. (type: character)}
#'   \item{RVersion}{Version package was built for or NA if it is source package. (type: character)}
#'   \item{File}{File path. (type: character)}
#' }
#'
get_package_files_info <- function(files) {
  assert(!missing(files) && is.character(files) && length(files) > 0,
         "Expected character(N) for files")
  assert(all(file.exists(files)),
         "Files do not exist: %s", paste(files[!file.exists(files)], collapse = ", "))
  
  get_pkg_file_info <- function(file) {
    abs_file <- normalizePath(file)
    pkg_name <- gsub("^([^_]+)_.+$", "\\1", basename(abs_file))
    if (grepl("[.]zip$", abs_file)) {
      con <- unz(abs_file, file.path(pkg_name, "DESCRIPTION"))
      dcf <- tryCatch({
        data.frame(read.dcf(con), stringsAsFactors = F)[1,]
      }, error = function(e) { NULL }, finally = { close(con) })
    } else {
      tmp_dir <- tempfile(pattern = "pkg_info")
      dir.create(tmp_dir)
      dcf <- tryCatch({
        utils::untar(abs_file, files = file.path(pkg_name, "DESCRIPTION"), exdir = tmp_dir)
        data.frame(read.dcf(file.path(tmp_dir, pkg_name, "DESCRIPTION")), stringsAsFactors = F)[1,]
      }, error = function(e) { NULL }, finally = { unlink(tmp_dir, recursive = T, force = T) })
    }
    
    assert(!is.null(dcf), "Failed to read DESCRIPTION from package file: %s", file)
    
    if (!('Built' %in% colnames(dcf))) {
      pkg_type <- 'source'
      rver <- NA
    } else {
      pkg_type <- ifelse(grepl('; windows$', dcf$Built), 'win.binary', 'mac.binary')
      rver <- majmin_rver(gsub('^R ([^;]+);.+$', '\\1', dcf$Built))
    }
    
    data.frame(Package = pkg_name, Version = dcf$Version, Type = pkg_type, RVersion = rver, File = file,
               stringsAsFactors = F)
  }
  
  db <- do.call("rbind", lapply(X = files, F = get_pkg_file_info))
  return(db)
}

#'
#' @keywords internal
#'
#' Retrieves information on packages in PKGZIP passed.
#' 
#' @param pkgzip pkgipz to retrieve information from. (type: character)
#' 
#' @return data.table of following structure
#' \describe{
#'   \item{Package}{Name of package. (type: character)}
#'   \item{Version}{Package version. (type: character)}
#'   \item{Type}{Type of package: source, win.binary, mac.binary. (type: character)}
#'   \item{RVersion}{Version package was built for or NA if it is source package. (type: character)}
#'   \item{File}{File path inside archive. (type: character)}
#' }
#'
get_pkgzip_info <- function(pkgzip) {
  assert(is_nonempty_char1(pkgzip), "Non empty character(1) expected for pkgzip")
  assert(file.exists(pkgzip), "File %s does not exist", pkgzip)
  
  files <- unzip(pkgzip, list = T)$Name
  files <- files[grepl("^.+/[^_]+_[^/]+[.](zip|t(ar[.])?gz)$", files)]
  types <- ifelse(grepl("^src/contrib/[^/]+[.]tar[.]gz$", files), "source", 
                  ifelse(grepl("^bin/windows/contrib/.+[.]zip$", files), "win.binary",
                         ifelse(grepl("^bin/macosx/contrib/.+$", files), "mac.binary",
                                "binary")))
  rvers <- ifelse(grepl("^src/.+$", files), NA,
                  gsub("bin/[^/]+/contrib/([^/]+)/.+$", "\\1", files))
  return(data.frame(
    Package = gsub("^.+/([^_]+)_[^/]+$", "\\1", files),
    Version = gsub("^.+/[^_]+_([^/]+)[.](zip|t(ar[.])?gz)$", "\\1", files),
    Type = types,
    RVersion = rvers,
    File = files,
    stringsAsFactors = F
  ))
}
