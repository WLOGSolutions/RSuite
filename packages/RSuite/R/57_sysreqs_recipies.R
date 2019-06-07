#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Recipies for checking/installing of sysreqs.
#----------------------------------------------------------------------------

#' Creates the base represantion of the sysreqs_recipe class.
#' Recipes are used to perform the following tasks:
#'     - collecting sysreqs
#'     - installing sysreqs
#'     - creating sysreqs installation scripts
#'
#'
#' @return object of type sysreqs_recipe
#'
#' @keywords internal
#' @noRd
#'
build_sysreqs_recipe <- function() {
  result <- list()
  class(result) <- "sysreqs_recipe"
  return(result)
}

#' Adds a recipe to a sysreqs_recipe object.
#'
#' @param recipe syreqs_recipe object (type: sysreqs_recipe)
#' @param req_name name of the requirement (type: character(1))
#' @param sysreq named list with package names and containing system
#'    requirements as value
#' @param pkg_name name of the package containg system requirements (type: character(1))
#'
#' @keywords internal
#' @noRd
#'
add_recipe <- function(recipe, req_name, sysreq, pkg_name) {
  UseMethod("add_recipe")
}


#' Removes all recipes concerning satisfied system requirements
#' from a sysreqs_recipte object.
#'
#' @param recipe sysreqs_recipe object (type: sysreqs_recipe)
#'
#' @keywords internal
#' @noRd
#'
rm_satisfied <- function(recipe) {
  UseMethod("rm_satisfied")
}

#' Performs(runs) all recipes from the sysreqs_recipe object.
#'
#' @param  recipe sysreqs_recipe object (type: sysreqs_recipe)
perform <- function(recipe) {
  UseMethod("perform")
}

#' Collects all system requirements recipes and adds them into
#' a sysreqs_recipe object.
#'
#' @param recipe sysreqs_recipe object (type: sysreqs_recipe)
#' @param sysreqs named list with package names and containing system
#'    requirements as value
#'
#' @return sysreqs_recipe object containing all system requirements recipes (type: sysreqs_recipe)
#'
#' @keywords internal
#' @noRd
#'
sysreqs_recipe_collect_all <- function(recipe, sysreqs) {
  for (pkg_name in names(sysreqs)) {
    pkg_sysreqs <- sysreqs[[pkg_name]]
    r_sysreqs <- get_sysreqs_for(pkg_name, pkg_sysreqs) # from 56_sysreqs_utils.R
    for (req_name in names(r_sysreqs)) {
      recipe <- add_recipe(recipe, req_name, r_sysreqs[[req_name]], pkg_name)
    }
  }
  recipe <- rm_satisfied(recipe)
  return(recipe)
}

#' Creates an object representing the sysreqs_check_recipe class - a specialization
#' of the sysreqs_recipe class.
#'
#' @return object of type sysreqs_check_recipe
#'
#' @keywords internal
#' @noRd
#'
build_sysreqs_check_recipe <- function() {
  result <- build_sysreqs_recipe()
  result$tools <- list()
  result$syslibs <- list()
  result$satisfied <- list()
  class(result) <- c("sysreqs_check_recipe", class(result))
  return(result)
}

#'
#' Implementation of add_recipe for the sysreqs_check_recipe class.
#'
#' @keywords internal
#' @noRd
#'
add_recipe.sysreqs_check_recipe <- function(recipe, req_name, sysreq, pkg_name) {
  check_handler <- sysreq$handlers$check
  if (is.null(check_handler) || check_handler == "[syslib]") {
    syslibs <- recipe$syslibs[[req_name]]
    if (is.null(syslibs)) {
      syslibs <- list(pkgs = pkg_name, plat_deps = sysreq$plat_spec)
    } else {
      syslibs <- list(pkgs = c(syslibs$pkgs, pkg_name), plat_deps = syslibs$plat_deps, sysreq$plat_spec)
    }
    recipe$syslibs[[req_name]] <- syslibs
  } else if (check_handler == "[tool]") {
    recipe$tools[[req_name]] <- c(unlist(recipe$tools[req_name]), pkg_name)
  } else {
    pkg_logwarn("[%s] Unknown check handler for %s: %s", pkg_name, req_name, check_handler)
    return(recipe)
  }

  for (req_satisfied in sysreq[[req_name]]$satisfies) {
    recipe$satisfied[[req_satisfied]] <- c(recipe$satisfied[[req_satisfied]], req_name)
  }
  return(recipe)
}

#'
#' Implementation of rm_satisfied for the sysreqs_check_recipe class.
#'
#' @keywords internal
#' @noRd
#'
rm_satisfied.sysreqs_check_recipe <- function(recipe) {
  for (req_satisfied in names(recipe$satisfied)) {
    who_satisfies <- recipe$satisfied[[req_satisfied]]
    pkg_loginfo("Requirement for %s will be satisfied by %s; not checking",
                req_satisfied, paste(who_satisfies, collapse = ", "))
    if (req_satisfied %in% names(recipe$tools)) {
      recipe$tools[[req_satisfied]] <- NULL
    }
    if (req_satisfied %in% names(recipe$syslibs)) {
      recipe$syslibs[[req_satisfied]] <- NULL
    }
  }
  return(recipe)
}

#'
#' Implementation of perform for the sysreqs_check_recipe class.
#'
#' @keywords internal
#' @noRd
#'
perform.sysreqs_check_recipe <- function(recipe) {
  result <- list()

  tools_exists <- file.exists(Sys.which(names(recipe$tools)))
  result$notools <- names(recipe$tools)[!tools_exists]
  if (length(result$notools) > 0) {
    pkg_loginfo("Lack of required tools detected in environment: %s",
                 paste(result$notools, collapse = ", "))
  }

  syslib_names <- unlist(lapply(recipe$syslibs, function(sl) sl$plat_deps))
  if (is.null(syslib_names)) {
    return(invisible(result))
  }

  plat_desc <- get_platform_desc()
  check_tool <- plat_desc$lib_tools$check
  if (is.null(check_tool)) {
    pkg_loginfo("Checking for system libraries for the platform is not supported")
    return(invisible(result))
  }

  check_tool_cmd <- get_libtool_cmd(check_tool, "check") # from 56_sysreqs_utils.R
  if (!is_libtool_user(check_tool)) {
    return(invisible(result))
  }
  result$nolibs <- .detect_unavailable_syslibs(check_tool_cmd, syslib_names)

  if (length(result$nolibs) > 0) {
    pkg_loginfo("Lack of required system libraries detected in environment: %s",
                paste(result$nolibs, collapse = ", "))
  }

  return(invisible(result))
}

#'
#' Detects which syslibs are not available.
#'
#' Helper for perform.sysreqs_check_recipe. Used also by perform.sysreqs_install_recipe
#' to detect libraries which are to be installed.
#'
#' @param check_tool_cmd command to run to check libraries. Must not be NULL.
#' @param syslib_names names of libraries to check for availability
#'
#' @return vector of libraries not currently available in the system.
#'
#' @noRd
#' @keywords internal
#'
.detect_unavailable_syslibs <- function(check_tool_cmd, syslib_names) {
  stopifnot(!is.null(check_tool_cmd))

  cmd <- sprintf("bash -c '%s'", gsub(":params", paste(syslib_names, collapse = " "), check_tool_cmd))
  # TODO handle this situation (now temporarly using cmd_lines, but not sure if this is a good idea.)
  unavail_syslibs <-
    get_cmd_outlines(sprintf("checking for system libraries: %s", paste(syslib_names, collapse = ", ")),
                     cmd = cmd,
                     log_debug = TRUE)
  return(unavail_syslibs)
}


#' Creates an object representing the sysreqs_install_recipe class - a specialization
#' of the sysreqs_recipe class.
#'
#' @return object of type sysreqs_install_recipe
#'
#' @keywords internal
#' @noRd
#'
build_sysreqs_install_recipe <- function(prj_path) {
  result <- build_sysreqs_recipe()
  result$install_specs <- list()
  result$build_specs <- list()
  result$satisfied <- list()
  result$prj_path <- prj_path
  class(result) <- c("sysreqs_install_recipe", class(result))
  return(result)
}

#'
#' Implementation of add_recipe for the sysreqs_install_recipe class.
#'
#' @keywords internal
#' @noRd
#'
add_recipe.sysreqs_install_recipe <- function(recipe, req_name, sysreq, pkg_name) {
  install_handler <- sysreq$handlers$install
  if (is.null(install_handler) || install_handler == "[syslib]") {
    if (!(req_name %in% names(recipe$install_specs))) {
      recipe$install_specs[[req_name]] <- list(syslibs = character(0), packages = character(0))
    }

    inst_spec <- recipe$install_specs[[req_name]]
    inst_spec$packages <- c(inst_spec$packages, pkg_name)
    inst_spec$syslibs <- c(inst_spec$syslibs, sysreq$plat_spec)
    inst_spec$info <- sysreq$info
    recipe$install_specs[[req_name]] <- inst_spec
  }

  build_handler <- sysreq$handlers$build
  if (!is.null(build_handler)) {
    if (!(req_name %in% names(recipe$build_specs))) {
      assert(grepl("^\\[shell\\]", build_handler),
             "[%s](%s) Build handler other than [shell] are not supported yet", pkg_name, req_name)
      cmd <- gsub("^\\[shell\\]\\s*(.+)$", "\\1", build_handler)
      recipe$build_specs[[req_name]] <- list(cmd = cmd,
                                             params = character(0),
                                             packages = character(0),
                                             tool = sysreq$plat_spec)
    } # else: they use same sysreqsdb so commands to run are same. Just add more parameters.

    build_spec <- recipe$build_specs[[req_name]]
    build_spec$params <- paste(build_spec$params, sysreq$params)
    build_spec$packages <- c(build_spec$packages, pkg_name)
    build_spec$info <- sysreq$info
    recipe$build_specs[[req_name]] <- build_spec
  }

  for (req_satisfied in sysreq$satisfies) {
    recipe$satisfied[[req_satisfied]] <- c(recipe$satisfied[[req_satisfied]], req_name)
  }
  return(recipe)
}

#'
#' Implementation of rm_satisfied for the sysreqs_install_recipe class.
#'
#' @keywords internal
#' @noRd
#'
rm_satisfied.sysreqs_install_recipe <- function(recipe) {
  for (req_satisfied in names(recipe$satisfied)) {
    who_satisfied <- recipe$satisfied[[req_satisfied]]
    pkg_loginfo("Requirement for %s will be satisfied by %s; Skipping the requirement",
                req_satisfied, paste(unique(who_satisfied), collapse = ", "), req_satisfied)
    if (req_satisfied %in% names(recipe$build_specs)) {
      recipe$build_specs[[req_satisfied]] <- NULL
    }
    if (req_satisfied %in% names(recipe$install_specs)) {
      recipe$install_specs[[req_satisfied]] <- NULL
    }
  }
  return(recipe)
}

#'
#' Implementation of perform for the sysreqs_install_recipe class.
#'
#' @keywords internal
#' @noRd
#'
perform.sysreqs_install_recipe <- function(recipe) {
  plat_desc <- get_platform_desc() # from 56_sysreqs_utils.R

  failed_reqs <- c()

  syslib_names <- unlist(lapply(recipe$install_specs, function(sl) sl$syslibs))
  if (!is.null(syslib_names)) {
    failed_reqs <- c(failed_reqs,
                     .install_syslibs(plat_desc, syslib_names))
  }

  for (req_name in names(recipe$build_specs)) {
    success <- .install_sysreq(req_name, recipe$build_specs[[req_name]], recipe$prj_path)
    if (!success) {
      failed_reqs <- c(failed_reqs, req_name)
    }
  }

  if (length(failed_reqs) > 0) {
    pkg_logwarn("Error(s) occurred while installing following system requirements: %s",
                paste(failed_reqs, collapse = ", "))
  } else {
    pkg_loginfo("All system requirements installed.")
  }
}

#'
#' Installs system libraries.
#'
#' Helper for perform.sysreqs_install_recipe process
#'
#' Checks which of them are not available with .detect_unavailable_syslibs.
#' Calls installation preparation utility and installs syslibs one by one.
#'
#' @param plat_desc description of the platform
#' @param syslib_names vector if syslib names to install
#'
#' @return vertor of system libraries' names which failed to be installed.
#'
#' @noRd
#' @keywords internal
#'
.install_syslibs <- function(plat_desc, syslib_names) {
  install_tool <- plat_desc$lib_tools$install
  if (is.null(install_tool)) {
    pkg_loginfo("Installing of system libraries on the platform is not supported")
    return(NULL)
  }

  install_tool_cmd <- get_libtool_cmd(install_tool, "install") # from 56_sysreqs_utils.R

  check_tool <- plat_desc$lib_tools$check
  check_tool_cmd <- get_libtool_cmd(check_tool, "check") # from 56_sysreqs_utils.R

  if (!is.null(check_tool_cmd) && is_libtool_user(check_tool, warn = FALSE)) {
    # install only unavailable syslibs
    unavail_syslibs <- .detect_unavailable_syslibs(check_tool_cmd, syslib_names)
    if (is.null(unavail_syslibs) || length(unavail_syslibs) == 0) {
      pkg_loginfo("It seems like all system libraries required are available already in the system.")
      return(NULL)
    }

    syslib_names <- unavail_syslibs
    pkg_loginfo("Following system libraries are not available (will install them): %s",
                paste(syslib_names, collapse = ", "))
  }

  instprep_tool <- plat_desc$lib_tools$instprep
  instprep_tool_cmd <- get_libtool_cmd(instprep_tool, "install pareparation") # from 56_sysreqs_utils.R

  if (!is_libtool_user(install_tool, warn = TRUE) ||
      (!is.null(instprep_tool_cmd) && !is_libtool_user(instprep_tool, warn = TRUE))) {
    # system user is not appropriate to execute action
    #  show some messages on what can be undertaken
    pkg_logwarn("If you have required access, please install system packages on your own like this:")
    if (!is.null(instprep_tool_cmd)) {
      pkg_logwarn("[user]$ %s", instprep_tool_cmd)
    }
    pkg_logwarn("[user]$ %s", gsub(":params", paste(syslib_names, collapse = " "), install_tool_cmd))
    return(syslib_names)
  }

  # prepare for syslibs installation
  if (!is.null(instprep_tool_cmd)) {
    cmd <- sprintf("bash -c '%s'", instprep_tool_cmd)
    pkg_loginfo("Preparing for installation of system libs...")
    cmd_retcode <- get_cmd_retcode("prepare for sys libs installation",
                                   cmd = cmd,
                                   log_debug = TRUE)
    if (is.null(cmd_retcode) || cmd_retcode > 0) {
      pkg_logwarn(paste("Something went wrong while preparing to install system libraries.",
                        "Process terminated with code: %s."),
                  cmd_retcode)
      return("<inst_prepare>")
    }
    pkg_loginfo("... done")
  }

  # install them one by one
  failed_syslibs <- c()
  for (syslib in syslib_names) {
    cmd <- sprintf("bash -c '%s'", gsub(":params", syslib, install_tool_cmd))
    pkg_loginfo("Installing system library '%s' ...", syslib)

    cmd_retcode <- get_cmd_retcode(sprintf("installing system library '%s'", syslib),
                                   cmd = cmd,
                                   log_debug = TRUE)
    if (is.null(cmd_retcode) || cmd_retcode > 0) {
      pkg_logwarn(paste("Failed to install system library '%s'.",
                        "Installation process terminated with code: %s."),
                  cmd_retcode)
      failed_syslibs <- c(failed_syslibs, syslib)
      next
    }

    pkg_loginfo("... done")
  }

  return(failed_syslibs)
}

#'
#' Installs single sysreq.
#'
#' Helper for perform.sysreqs_install_recipe process
#'
#' @param req_name name of sysreq to install.
#' @param build_spec build specification for the req
#' @param prj_path extra infor to update project env; project path.
#'
#' @noRd
#' @keywords internal
#'
.install_sysreq <- function(req_name, build_spec, prj_path) {
  if (!file.exists(Sys.which(build_spec$tool))) {
    pkg_logwarn("Tool %s for building %s is not available.", build_spec$tool, req_name)
    return(FALSE)
  }

  pkg_loginfo("Building %s for %s ...", req_name, paste(build_spec$packages, collapse = ", "))

  cmd <- gsub(":params", build_spec$params, build_spec$cmd, fixed = TRUE)

  tool_wspace <- gsub("\\", "/", file.path(prj_path, req_name), fixed = TRUE)
  cmd <- gsub(":path", tool_wspace, cmd, fixed = TRUE)
  cmd <- gsub(":tool", build_spec$tool, cmd, fixed = TRUE)
  cmd <- paste(cmd, "-v")

  cmd_retcode <- get_cmd_retcode(sprintf("building %s for %s",
                                         req_name, paste(build_spec$packages, collapse = ", ")),
                                 cmd = cmd,
                                 log_debug = TRUE)
  if (is.null(cmd_retcode) || cmd_retcode > 0) {
    pkg_logwarn("Something went wrong while building %s. Building process terminated with code: %s.",
                req_name, cmd_retcode)
    return(FALSE)
  }

  pkg_loginfo("... done")
  lapply(build_spec$info, function(msg) pkg_loginfo("[%s]: %s", req_name, msg))

  return(TRUE)
}


#' Creates an object representing the sysreqs_script_recipe class - a specialization
#' of the sysreqs_install_recipe class.
#'
#' @return object of type sysreqs_script_recipe
#'
#' @keywords internal
#' @noRd
#'
build_sysreqs_script_recipe <- function(prj_path) {
  result <- build_sysreqs_install_recipe(prj_path)
  class(result) <- c("sysreqs_script_recipe", class(result))
  return(result)
}

#'
#' Implementation of perform for the sysreqs_script_recipe class.
#'
#' @keywords internal
#' @noRd
#'
perform.sysreqs_script_recipe <- function(recipe) {
  plat_desc <- get_platform_desc()
  if (plat_desc$name == "Windows") {
    script_fpath <- build_win_script(recipe, plat_desc)
  } else {
    script_fpath <- build_bash_script(recipe, plat_desc)
    Sys.chmod(script_fpath)
  }

  return(script_fpath)
}


#'
#' Creates a cmd script to update the system to satisfy project requirements.
#'
#' @param recipe object of type sysreqs_script_recipe
#' @param plat_desc named list content:
#' \describe{
#'   \item{name}{One of Windows, MacOS, RedHat, Debian (type: character)}
#'   \item{distrib}{Distribution e.g. for Debian: Debian, Ubuntu; for RedHat: CentOS, RedHat, Fedora (type: character(1))}
#'   \item{release}{Distribution release e.g. for Debian: squeeze, wheezy, jessie (type: character(1))}
#'   \item{sysreq_type}{One of Windows, Pkg, RPM, DEB (type: character(1))}
#'   \item{build}{True if build environment is required (type: logical(1))}
#' }
build_win_script <- function(recipe, plat_desc) {
  script_lines <- c("@echo off",
                    "",
                    "set basedir=%~dps0")

  required_syslibs <- unlist(lapply(recipe$install_specs, function(sl) sl$syslibs))
  if (!is.null(required_syslibs)) {
    check_tool <- plat_desc$lib_tools$check
    if (!is.null(check_tool)) {
      check_tool_cmd <- get_libtool_cmd(check_tool, "check") # from 56_sysreqs_utils.R
      cmd <- gsub(":params", "%syslibs_to_install%", check_tool_cmd, fixed = TRUE)
      script_lines <- c(script_lines,
                        sprintf("set syslibs_check_tmp_file=%TEMP%\\%RANDOM%_check.tmp"),
                        sprintf("%s > %syslibs_check_tmp_file%"),
                        sprintf("set /P syslibs_to_install=<%syslibs_check_tmp_file%"),
                        sprintf("del %syslibs_check_tmp_file%"))
    } else {
      script_lines <- c(script_lines,
                        sprintf('set syslibs_to_install="%s"', paste(required_syslibs, collapse = " ")))
    }

    install_tool <- plat_desc$lib_tools$install
    if (is.null(install_tool)) {
      pkg_loginfo("Installing of system libraries on the platform is not supported")
    } else {
      install_tool_cmd <- get_libtool_cmd(install_tool, "install") # from 56_sysreqs_utils.R
      cmd <- gsub(":params", "%syslibs_to_install%", install_tool_cmd)
      script_lines <- c(script_lines,
                        "if \"%syslibs_to_install%\" == \"\" (",
                        "     echo No system libraries to install",
                        "     goto check_tools",
                        ")",
                        "echo Installing system libraries (%syslibs_to_install%) ...",
                        cmd,
                        ":check_tools")
    }
  }

  for (req_name in names(recipe$build_specs)) {
    build_spec <- recipe$build_specs[[req_name]]

    cmd <- gsub(":params", build_spec$params, build_spec$cmd, fixed = TRUE)
    tool_wspace <- gsub("\\", "/", paste0("%basedir%", req_name), fixed = TRUE)
    cmd <- gsub(":path", tool_wspace, cmd, fixed = TRUE)
    cmd <- gsub(":tool", build_spec$tool, cmd, fixed = TRUE)

    script_lines <- c(script_lines,
                      "",
                      sprintf("for %%%%i in (%s) do if \"%%%%~$PATH:i\" == \"\" (", build_spec$tool),
                      sprintf("   echo Tool %s is not available: environment cannot be built", build_spec$tool),
                      sprintf("   goto %s_failed", build_spec$tool),
                      sprintf(")"),
                      sprintf("if exist \"%s\" rmdir /S/Q \"%s\"", tool_wspace, tool_wspace),
                      sprintf("echo Building %s for %s ...", req_name, paste(build_spec$packages, collapse = ", ")),
                      sprintf("%s", cmd),
                      sprintf("if NOT ERRORLEVEL 0 ("),
                      sprintf("       echo ... failed"),
                      sprintf("       goto %s_failed", build_spec$tool),
                      sprintf(")"),
                      sprintf("echo ... done"),
                      sprintf(":%s_failed", build_spec$tool),
                      sprintf("echo."))
  }

  script_fpath <- file.path(recipe$prj_path, "sysreqs_install.cmd")
  writeLines(script_lines, con = script_fpath)
  return(script_fpath)
}

#'
#' Creates a bash script to update the system to satisfy project requirements.
#'
#' @param recipe object of type sysreqs_script_recipe
#' @param plat_desc named list content:
#' \describe{
#'   \item{name}{One of Windows, MacOS, RedHat, Debian (type: character)}
#'   \item{distrib}{Distribution e.g. for Debian: Debian, Ubuntu; for RedHat: CentOS, RedHat, Fedora (type: character(1))}
#'   \item{release}{Distribution release e.g. for Debian: squeeze, wheezy, jessie (type: character(1))}
#'   \item{sysreq_type}{One of Windows, Pkg, RPM, DEB (type: character(1))}
#'   \item{build}{True if build environment is required (type: logical(1))}
#' }
build_bash_script <- function(recipe, plat_desc) {
  script_lines <- c("#!/bin/bash",
                    "",
                    "basedir=$(dirname $0)",
                    "basedir=$(readlink -f $basedir)",
                    "exitcode=0")

  required_syslibs <- unlist(lapply(recipe$install_specs, function(sl) sl$syslibs))
  if (!is.null(required_syslibs)) {
    script_lines <- c(script_lines,
                      sprintf("export syslibs_to_install=\"%s\"", paste(required_syslibs, collapse = " ")))

    check_tool <- plat_desc$lib_tools$check
    if (!is.null(check_tool)) {
      check_tool_cmd <- get_libtool_cmd(check_tool, "check") # from 56_sysreqs_utils.R
      cmd <- gsub(":params", "${syslibs_to_install}", check_tool_cmd, fixed = TRUE)
      script_lines <- c(script_lines,
                        sprintf("export syslibs_to_install=$(bash -c '%s')", cmd))
    }

    instprep_tool <- plat_desc$lib_tools$instprep
    if (!is.null(instprep_tool)) {
      instprep_tool_cmd <- get_libtool_cmd(instprep_tool, "install preparation") # from 56_sysreqs_utils.R

      script_lines <- c(script_lines,
                        "if [ -n \"${syslibs_to_install}\" ]; then",
                        sprintf("     echo \"Preparing to install system libraries ...\""),
                        sprintf("     %s", instprep_tool_cmd),
                        sprintf("     if [ $? == 0 ]; then"),
                        sprintf("         echo -e \"... done\\n\\n\""),
                        sprintf("     else"),
                        sprintf("         echo -e \"... failed\\n\\n\""),
                        sprintf("         exitcode=1"),
                        sprintf("     fi"),
                        "fi")
    }

    install_tool <- plat_desc$lib_tools$install
    if (is.null(install_tool)) {
      pkg_loginfo("Installing of system libraries on the platform is not supported")
    } else {
      install_tool_cmd <- get_libtool_cmd(install_tool, "install") # from 56_sysreqs_utils.R
      cmd <- gsub(":params", "${syslibs_to_install}", install_tool_cmd)
      script_lines <- c(script_lines,
                        "",
                        "if [ -z \"${syslibs_to_install}\" ]; then",
                        "     echo \"No system libraries to install\"",
                        "else",
                        sprintf("     echo \"Installing system libraries (${syslibs_to_install}) ...\""),
                        sprintf("     %s", cmd),
                        sprintf("     if [ $? == 0 ]; then"),
                        sprintf("         echo -e \"... done\\n\\n\""),
                        sprintf("     else"),
                        sprintf("         echo -e \"... failed\\n\\n\""),
                        sprintf("         exitcode=1"),
                        sprintf("     fi"),
                        "fi")
    }
  }

  for (req_name in names(recipe$build_specs)) {
    build_spec <- recipe$build_specs[[req_name]]

    cmd <- gsub(":params", build_spec$params, build_spec$cmd, fixed = TRUE)
    tool_wspace <- gsub("\\", "/", file.path("${basedir}", req_name), fixed = TRUE)
    cmd <- gsub(":path", tool_wspace, cmd, fixed = TRUE)
    cmd <- gsub(":tool", build_spec$tool, cmd, fixed = TRUE)

    script_lines <- c(script_lines,
                      "",
                      sprintf("which %s > /dev/null", build_spec$tool),
                      sprintf("if [ $? == 0 ]; then"),
                      sprintf("   if [ -d %s ]; then rm -rf %s; fi", tool_wspace, tool_wspace),
                      sprintf("   echo \"Building %s for %s ...\"",
                              req_name, paste(build_spec$packages, collapse = ", ")),
                      sprintf("   %s", cmd),
                      sprintf("   if [ $? == 0 ]; then"),
                      sprintf("       echo -e \"... done\\n\\n\""),
                      sprintf("   else"),
                      sprintf("       echo -e \"... failed\\n\\n\""),
                      sprintf("       exitcode=1"),
                      sprintf("   fi"),
                      sprintf("else"),
                      sprintf("   echo \"Tool %s is not available: environment cannot be built\"",
                              build_spec$tool),
                      sprintf("   exitcode=1"),
                      sprintf("fi"))
  }
  script_lines <- c(script_lines,
                    "",
                    "exit ${exitcode}")

  script_fpath <- file.path(recipe$prj_path, "sysreqs_install.sh")
  writeLines(script_lines, con = script_fpath)
  return(script_fpath)
}
