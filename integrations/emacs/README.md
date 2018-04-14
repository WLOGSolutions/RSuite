# Instalation

Currently installation is supported only via manual. It is planned to
add this package to [MELPA
repository](https://melpa.org/#/getting-started "MELPA repository"). 

## Download package

You have to download `rsuite.el` using this
[https://github.com/WLOGSolutions/RSuite](link "rsuite.el") into your
`~\.emacs.d\myscripts\` folder. You can use any other folder but
remember to adapt next steps to your folder.

## Make package available to Emacs

We use [https://github.com/jwiegley/use-package](use-package
"use-package") for managing our Emacs' packages. So first you have to
install `use-package` package. We recommend using [MELPA
repository](https://melpa.org/#/getting-started "MELPA repository"). 

Having `use-package` you can add the following set of instructions to
your `~\.emacs.d\init.el` file. Note the keybindings can be customized
to your needs.

``` emacs-lisp
(use-package rsuite
  :load-path "~/.emacs.d/myscripts/"
  :bind (
	 ("C-x r p s" . rsuite-proj-start)
	 ("C-x r p p" . rsuite-proj-pkg-start)
	 ("C-x r p b" . rsuite-proj-build)
	 ("C-x r p d" . rsuite-proj-depsinst)
	 ("C-x r p z" . rsuite-proj-zip)
	 ("C-x r d z i" . rsuite-docker-zip-image)
	 ("C-x r d z p" . rsuite-docker-zip-platform)
	 ("C-x r d i p" . rsuite-docker-image-platform)
	 ("C-x r d i i" . rsuite-docker-image-image)))
```

# Instruction

## Variables
Currently the `rsuite.el` package provides following variables:

* `rsuite-verbose` - switching verbose mode. Can be changed with
  `rsuite-toggle-verbose` function.
* `rsuite-docker-platforms` - list of supported docker
  platforms. Currently those are Ubuntu (Debian) and Centos.
* `rsuite/cli` - path to rsuite cli.

## Functions


### Utils

* `rsuite-toggle-verbose` - toggle verbose mode

### Project section

* `rsuite-proj-start` - start a new project
* `rsuite-proj-pkg-start` - start a new package in current project
* `rsuite-proj-depsinst` - install dependencies
* `rsuite-proj-build` - build custom packages
* `rsuite-proj-test` - run project tests
* `rsuite-proj-zip` - build deployment ZIP for the project

### Docker section

* `rsuite-docker-zip-image` - build deployment ZIP using given docker
  image as a base.
* `rsuite-docker-zip-platform` - build deployment ZIP using image with
  one of the supported OSs.
* `rsuite-docker-image-image` - build production docker image using
  custom base image
* `rsuite-docker-image-platform` - build production docker image using
  base image with one of the supported OSs
