#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Recipies for checking/installing of sysreqs.
#----------------------------------------------------------------------------

build_sysreqs_recipe <- function() {
  result <- list()
  class(result) <- "sysreqs_recipe"
  return(result)
}

add_recipe <- function(recipe, req_name, sysreq, pkg_name) {
  UseMethod("add_recipe")
}

rm_satisfied <- function(recipe) {
  UseMethod("rm_satisfied")
}

perform <- function(recipe) {
  UseMethod("perform")
}

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


build_sysreqs_check_recipe <- function() {
  result <- build_sysreqs_recipe()
  result$tools <- list()
  result$syslibs <- list()
  result$satisfied <- list()
  class(result) <- c("sysreqs_check_recipe", class(result))
  return(result)
}

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

perform.sysreqs_check_recipe <- function(recipe) {
  result <- list()

  tools_exists <- file.exists(Sys.which(names(recipe$tools)))
  result$notools <- names(recipe$tools)[!tools_exists]
  if (length(result$notools) > 0) {
    pkg_loginfo("Lack of required tools detected in environment: %s",
                 paste(result$notools, collapse = ", "))
  }

  required_syslibs <- unlist(lapply(recipe$syslibs, function(sl) sl$plat_deps))
  if (is.null(required_syslibs)) {
    return(invisible(result))
  }

  check_tool <- get_platform_desc()$lib_tools$check
  if (is.null(check_tool)) {
    pkg_loginfo("Checking for system libraries for the platform is not supported")
    return(invisible(result))
  }
  assert(grepl("^\\[shell\\] ", check_tool),
         "System libraries check handlers other than [shell] are not supported yet")
  check_tool <- gsub("^\\[shell\\] ", "", check_tool)

  cmd <- sprintf("bash -c '%s'", gsub(":params", paste(required_syslibs, collapse = " "), check_tool))
  # TODO handle this situation (now temporarly using cmd_lines, but not sure if this is a good idea.)
  result$nolibs <-
    get_cmd_outlines(sprintf("checking for system libraries: %s", paste(required_syslibs, collapse = ", ")),
                     cmd = cmd,
                     log_debug = TRUE)

  if (length(result$nolibs) > 0) {
    pkg_loginfo("Lack of required system libraries detected in environment: %s",
                paste(result$nolibs, collapse = ", "))
  }

  return(invisible(result))
}


build_sysreqs_install_recipe <- function(prj_path) {
  result <- build_sysreqs_recipe()
  result$install_specs <- list()
  result$build_specs <- list()
  result$satisfied <- list()
  result$prj_path <- prj_path
  class(result) <- c("sysreqs_install_recipe", class(result))
  return(result)
}

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

perform.sysreqs_install_recipe <- function(recipe) {
  .is_retcode_failure <- function(cmd_ret_code, req_name) {
    return(is.null(cmd_ret_code) || cmd_ret_code > 0)
  }
  plat_desc <- get_platform_desc() # from 56_sysreqs_utils.R

  failed_reqs <- c()

  required_syslibs <- unlist(lapply(recipe$install_specs, function(sl) sl$syslibs))
  if (!is.null(required_syslibs)) {
    install_tool <- plat_desc$lib_tools$install
    if (is.null(install_tool)) {
      pkg_loginfo("Installing of system libraries on the platform is not supported")
    } else {
      assert(grepl("^\\[shell\\] ", install_tool),
             "System libraries install handlers other than [shell] are not supported yet")
      install_tool <- gsub("^\\[shell\\]\\s*", "", install_tool)
      cmd <- sprintf("bash -c '%s'", gsub(":params", paste(required_syslibs, collapse = " "), install_tool))

      pkg_loginfo("Installing system libraries(%s) ...", paste(required_syslibs, collapse = ", "))
      cmd_retcode <- get_cmd_retcode(sprintf("installing system libraries: %s",
                                             paste(required_syslibs, collapse = ", ")),
                                     cmd = cmd,
                                     log_debug = TRUE)
      if (.is_retcode_failure(cmd_retcode, "sys libs")) {
        failed_reqs <- c(failed_reqs, "sys libs")
        pkg_logwarn(paste("Something went wrong while installing system libraries.",
                          "Installation process terminated with code: %s."),
                    cmd_retcode)
      } else {
        pkg_loginfo("... done")
        lapply(names(required_syslibs),
               function(req_name) {
                 lapply(required_syslibs[[req_name]]$info,
                        function(msg) pkg_loginfo("[%s]: %s", req_name, msg))
               })
      }
    }
  }

  for (req_name in names(recipe$build_specs)) {
    build_spec <- recipe$build_specs[[req_name]]

    if (!file.exists(Sys.which(build_spec$tool))) {
      pkg_logwarn("Tool %s for building %s is not available.", build_spec$tool, req_name)
      failed_reqs <- c(failed_reqs, req_name)
      next
    }

    pkg_loginfo("Building %s for %s ...", req_name, paste(build_spec$packages, collapse = ", "))

    cmd <- gsub(":params", build_spec$params, build_spec$cmd, fixed = TRUE)

    tool_wspace <- gsub("\\", "/", file.path(recipe$prj_path, req_name), fixed = TRUE)
    cmd <- gsub(":path", tool_wspace, cmd, fixed = TRUE)
    cmd <- gsub(":tool", build_spec$tool, cmd, fixed = TRUE)
    cmd <- paste(cmd, "-v")

    cmd_retcode <- get_cmd_retcode(sprintf("building %s for %s",
                                           req_name, paste(build_spec$packages, collapse = ", ")),
                                   cmd = cmd,
                                   log_debug = TRUE)
    if (.is_retcode_failure(cmd_retcode)) {
      failed_reqs <- c(failed_reqs, req_name)
      pkg_logwarn("Something went wrong while building %s. Building process terminated with code: %s.",
                  req_name, cmd_retcode)
    } else {
      pkg_loginfo("... done")
      lapply(build_spec$info, function(msg) pkg_loginfo("[%s]: %s", req_name, msg))
    }
  }

  if (length(failed_reqs) > 0) {
    pkg_logwarn("Error(s) occurred while installing following system requirements: %s",
                paste(failed_reqs, collapse = ", "))
  } else {
    pkg_loginfo("All system requirements installed.")
  }
}


build_sysreqs_script_recipe <- function(prj_path) {
  result <- build_sysreqs_install_recipe(prj_path)
  class(result) <- c("sysreqs_script_recipe", class(result))
  return(result)
}

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

build_win_script <- function(recipe, plat_desc) {
  script_lines <- c("@echo off",
                    "",
                    "set basedir=%~dps0")

  required_syslibs <- unlist(lapply(recipe$install_specs, function(sl) sl$syslibs))
  if (!is.null(required_syslibs)) {
    check_tool <- plat_desc$lib_tools$check
    if (!is.null(check_tool)) {
      if (!is.null(check_tool)) {
        assert(grepl("^\\[shell\\] ", check_tool),
               "System libraries check handlers other than [shell] are not supported yet")
        check_tool <- gsub("^\\[shell\\] ", "", check_tool)
      }

      cmd <- gsub(":params", "%syslibs_to_install%", check_tool, fixed = TRUE)
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
      assert(grepl("^\\[shell\\] ", install_tool),
             "System libraries install handlers other than [shell] are not supported yet")
      install_tool <- gsub("^\\[shell\\]\\s*", "", install_tool)
      cmd <- gsub(":params", "%syslibs_to_install%", install_tool)
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
      assert(grepl("^\\[shell\\] ", check_tool),
             "System libraries check handlers other than [shell] are not supported yet")
      check_tool <- gsub("^\\[shell\\] ", "", check_tool)

      cmd <- gsub(":params", "${syslibs_to_install}", check_tool, fixed = TRUE)
      script_lines <- c(script_lines,
                        sprintf("export syslibs_to_install=$(bash -c '%s')", cmd))
    }

    install_tool <- plat_desc$lib_tools$install
    if (is.null(install_tool)) {
      pkg_loginfo("Installing of system libraries on the platform is not supported")
    } else {
      assert(grepl("^\\[shell\\] ", install_tool),
             "System libraries install handlers other than [shell] are not supported yet")
      install_tool <- gsub("^\\[shell\\]\\s*", "", install_tool)
      cmd <- gsub(":params", "${syslibs_to_install}", install_tool)
      script_lines <- c(script_lines,
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
