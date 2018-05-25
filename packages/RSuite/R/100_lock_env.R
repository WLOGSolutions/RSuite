#'
#' Locks the project environment
#'
#' @export
#'
prj_lock_env <- function(prj = NULL){

  prj <- safe_get_prj(prj)
  stopifnot(!is.null(prj))

  params <- prj$load_params()
  prj_dep_vers <- collect_prj_direct_deps(params) # from 52_dependencies.R

  prj_pkgs <- build_project_pkgslist(params$pkgs_path) # from 51_pkg_info.R
  prj_dep_vers <- vers.rm(prj_dep_vers, prj_pkgs)

  available_packages <- available.packages()
  deployment_path = file.path(params$prj_path, 'deployment')

  filename = paste('env_', params$project, '.lock', sep="")
  lock_data <- data.frame(available_packages[prj_dep_vers$pkgs$pkg, c("Package", "Version")])
  write.table(lock_data, file.path(deployment_path, filename), row.names = FALSE, quote = FALSE)
}

#' Gets project dependencies lock
#'
#' @param params project parameters. (type: rsuite_project_params)
#'
#' @return data frame with locked environment dependencies
#'
#' @export
#'
get_lock_env_info <- function(params){
  deployment_path = file.path(params$prj_path, 'deployment')
  filename = paste('env_', params$project, '.lock', sep="")
}
