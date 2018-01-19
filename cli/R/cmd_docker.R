#----------------------------------------------------------------------------
# RSuite
# Copyright (c) 2017, WLOG Solutions
#
# Handles 'pkgzip' command of CLI utility.
#----------------------------------------------------------------------------

args <- commandArgs()
base <- dirname(gsub("--file=", "", args[grepl("^--file=", args)]))[1]
source(file.path(base, "command_mgr.R"), chdir = T)
source(file.path(base, "docker_utils.R"))

.build_prj_zip <- function(platform, version, rver, dont_rm, pkgs, exc_master, dest_dir, sh) {
  if (is.null(version) || is.na(version)) {
    version <- NULL
  }

  if (!is.null(pkgs)) {
    pkgs <- trimws(unlist(strsplit(pkgs, ",")))
  }

  prj <- RSuite::prj_init()
  docker_image <- get_docker_image(prj, rver, platform) # from docker_utils.R

  pack_fpath <- RSuite::prj_pack(prj = prj, path = tempdir(), pack_ver = version,
                                 pkgs = pkgs, inc_master = !exc_master,
                                 rver = rver)

  cont_name <- run_container(docker_image) # from docker_utils.R
  on.exit({
    if (is.null(dont_rm) || !dont_rm) {
      stop_container(cont_name) # from docker_utils.R
    } else {
      loginfo("Container %s not removed (--dont-rm passed).", cont_name)
    }
  }, add = T)

  loginfo("Copying project pack into container %s ...", cont_name)
  exec_docker_cmd(c("cp", pack_fpath, sprintf("%s:/opt/", cont_name)), # from docker_utils.R
                  "Copying project pack into container")
  loginfo("... done.")

  prj_name <- prj$load_params()$get_safe_project_name()
  run_incont_cmd(cont_name, sprintf("unzip %s", basename(pack_fpath))) # from docker_utils.R
  if (!is.null(sh)) {
    run_incont_cmd(cont_name, sh)
  }
  run_incont_cmd(cont_name, sprintf("cd %s && rsuite proj depsinst -v", prj_name)) # ...
  run_incont_cmd(cont_name, sprintf("cd %s && rsuite proj build -v", prj_name)) # ...
  cmd <- sprintf("cd %s && rsuite proj zip -v -p /opt", prj_name)
  if (!is.null(version)) {
    cmd <- paste0(cmd, " --version=", version)
  }
  run_incont_cmd(cont_name, cmd) # ...

  zip_name <- gsub("^[^_]+_", "", basename(pack_fpath))
  zip_fpath <- file.path(dest_dir, zip_name)

  loginfo("Copying project deployment zip back from container %s ...", cont_name)
  exec_docker_cmd(c("cp", sprintf("%s:/opt/%s", cont_name, zip_name), dest_dir), # from docker_utils.R
                  "Copying project deployment zip back from container")
  loginfo("... done.")

  return(zip_fpath)
}

sub_commands <- list(
  zip = list(
    help = "Build project and generate deployment zip in docker container.",
    options = list(
      make_option(c("-p", "--platform"), dest = "platform", default = "ubuntu",
                  help="Build project for plaform passed. One of ubuntu or centos (default: %default)"),
      make_option(c("--rver"), dest = "rver", default=NULL,
                  help="If passed will enforce zip building for passed version of R."),
      make_option(c("--sh"), dest = "sh",
                  help="Extra command to execute on container before building project."),
      make_option(c("--dont-rm"), dest = "dont_rm", action="store_true", default=FALSE,
                  help="If passed will not remove build container on error or success."),
      make_option(c("--version"), dest = "version",
                  help=paste("Version to use for project pack tagging.",
                             "(default: use ZipVersion form PARAMETERS and revision from RC)",
                             sep = "\n\t\t")),
      make_option(c("--packages"), dest = "pkgs",
                  help=paste("Comma separated list of project packages to include into project pack.",
                             "If not passed all project packages will be included.",
                             sep = "\n\t\t")),
      make_option(c("--exc-master"), dest = "exc_master", action="store_true", default=FALSE,
                  help="If passed will exclude master scripts from project pack created. (default: %default)"),
      make_option(c("-d", "--dest"), dest = "dest",
                  help="Directory to put built deployment zip into. It must exist. (default: current directory)")
    ),
    run = function(opts) {
      if (is.null(opts$dest) || is.na(opts$dest)) {
        opts$dest <- getwd()
      }
      if (!dir.exists(opts$dest)) {
        stop(sprintf("Destination folder does not exist: %s", opts$dest))
      }

      .build_prj_zip(opts$platform, opts$version, opts$rver,
                     opts$dont_rm, opts$pkgs, opts$exc_master,
                     opts$dest, opts$sh)
      invisible()
    }
  ),
  img = list(
    help = "Build docker image containg deployed project.",
    options = list(
      make_option(c("-t", "--tag"), dest = "tag",
                  help=paste("Tag for newly created image.",
                             "It tag does not contain version part project version number will added.",
                             " (required)",
                             sep = "\n\t\t")),
      make_option(c("--tag-latest"), dest = "tag_latest", action="store_true", default=FALSE,
                  help = "If passed generated image will be also tagged with :latest"),
      make_option(c("-f", "--from"), dest = "from",
                  help=paste("Image to use as base container(FROM clause) for the debloyment.",
                             "If not passed will use wlog/rsuite:<platform>_r<rver> as default",
                             sep = "\n\t\t")),
      make_option(c("--templ"), dest = "templ",
                  help=paste("Template of Dockerfile to use for image building.",
                             "If not passed will create default just with project deployment.",
                             "It should exist.",
                             sep = "\n\t\t")),
      make_option(c("--templ-ctx"), dest = "templ_ctx",
                  help=paste("Comma separated folders to copy into context for Dockerfile to use for image building.",
                             "All should exist.",
                             sep = "\n\t\t")),
      make_option(c("-z", "--zip"), dest = "zip",
                  help=paste("Project zip to deploy.",
                             "If not passed zip will be created and deployed from project in context.",
                             "The file must exists.",
                             sep = "\n\t\t")),
      make_option(c("-p", "--platform"), dest = "platform", default = "ubuntu",
                  help=paste("Will be used if -z (--zip) is not passed or no -f (--from) passed.",
                             "Build project zip for plaform passed. One of ubuntu or centos (default: %default)",
                             sep = "\n\t\t")),
      make_option(c("--rver"), dest = "rver", default=NULL,
                  help=paste("Will be used if -z (--zip) is not passed or no -f (--from) passed.",
                             "If passed will enforce zip building for passed version of R.")),
      make_option(c("--sh"), dest = "sh",
                  help=paste("Will be used if -z (--zip) is not passed",
                             "Extra command to execute on container before building project zip.",
                             sep = "\n\t\t")),
      make_option(c("--version"), dest = "version",
                  help=paste("Will be used if -z (--zip) is not passed.",
                             "Version to use for project pack tagging.",
                             "(default: use ZipVersion form PARAMETERS and revision from RC)",
                             sep = "\n\t\t")),
      make_option(c("--packages"), dest = "pkgs",
                  help=paste("Will be used if -z (--zip) is not passed.",
                             "Comma separated list of project packages to deploy with project.",
                             "If not passed all project packages will be deployed.",
                             sep = "\n\t\t")),
      make_option(c("--exc-master"), dest = "exc_master", action="store_true", default=FALSE,
                  help=paste("Will be used if -z (--zip) is not passed.",
                             "If passed will not deploy master scripts. (default: %default)",
                             sep = "\n\t\t"))
    ),
    run = function(opts) {
      if (is.null(opts$tag)) {
        stop("Image tag is required. Provide --tag argument.")
      }

      if (!is.null(opts$templ) && !file.exists(opts$templ)) {
        stop(sprintf("Template file does not exist: %s", opts$templ))
      }
      if (!is.null(opts$templ_ctx)) {
        ctx_dirs <- trimws(unlist(strsplit(opts$templ_ctx, ",")))
        exists <- dir.exists(ctx_dirs)
        if (!all(exists)) {
          stop(sprintf("Template context folder(s) not found: %s",
                       paste(ctx_dirs[!exists], collapse = ", ")))
        }
      } else {
        ctx_dirs <- NULL
      }


      if (is.null(opts$from)) {
        prj <- RSuite::prj_init()
        opts$from <- get_docker_base_image(prj, opts$rver, opts$platform) # from docker_utils.R
        loginfo("Will use %s as base image!", opts$from)
      }

      tmp_dir <- tempfile("imgbuild_")
      dir.create(tmp_dir, showWarnings = F, recursive = T)
      on.exit({ unlink(tmp_dir, recursive = T, force = T) }, add = T)

      if (is.null(opts$zip)) {
        opts$zip <- .build_prj_zip(platform = opts$platform,
                                   version = opts$version,
                                   rver = opts$rver,
                                   dont_rm = NULL,
                                   pkgs = opts$pkgs,
                                   exc_master = opts$exc_master,
                                   dest_dir = tmp_dir,
                                   sh = opts$sh)
      } else {
        if (!file.exists(opts$zip)) {
          stop("Project zip file passed does not exist: %s.", opts$zip)
        }
        success <- file.copy(from = opts$zip, to = tmp_dir, overwrite = T, recursive = T)
        if (!success) {
          stop("Failed to prepare build env. Could not copy %s.", opts$zip)
        }
      }

      # copy template context
      if (!is.null(ctx_dirs)) {
        success <- file.copy(from = ctx_dirs, to = tmp_dir, recursive = T)
        if (!all(success)) {
          stop(sprintf("Failed to copy context folder(s) into template context: %s",
                       paste(ctx_dirs[!success], collapse = ", ")))
        }
      }

      # create Dockerfile
      docker_fpath <- file.path(tmp_dir, "Dockerfile")
      build_dockerfile(opts$templ, docker_fpath,
                       tags = list(
                         From = opts$from,
                         DeployProject = c(sprintf("COPY %s /tmp/", basename(opts$zip)),
                                           sprintf("RUN unzip /tmp/%s -d /opt && rm -rf /tmp/%s",
                                                   basename(opts$zip), basename(opts$zip)))
                        ))

      nover_tag <- gsub(":.+$", "", opts$tag)
      if (nover_tag == opts$tag) {
        image_tag <- paste0(opts$tag, ":", gsub("^.+_([0-9.-]+[-_.][0-9]+x?)[.]zip$", "\\1", opts$zip))
      } else {
        image_tag <- opts$tag
      }

      loginfo("Building image %s ...", image_tag)
      exec_docker_cmd(c("build", "-t", image_tag, "-f", docker_fpath, tmp_dir), # from docker_utils.R
                      "Building image")
      loginfo("... done.")

      if (opts$tag_latest) {
        loginfo("Tagging %s as %s:latest ...", image_tag, nover_tag)
        exec_docker_cmd(c("tag", image_tag, paste0(nover_tag, ":latest")), # from docker_utils.R
                        "Tagging built image as latest")
        loginfo("... done.")
      }
    }
  )
)

handle_subcommands(
  sub_commands = sub_commands,
  cmd_help = "The command helps you handle building for other platform with use of docker."
)
