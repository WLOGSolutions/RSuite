# Introduction

![Emacs integration with R Suite exemplary
screencast](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/rsuite_emacs_scrncast.png
"Emacs integration with R Suite")

## **Got stuck?**

If you are stuck feel free to contact us:

* through R Suite website (https://rsuite.io#contact) or 
* using Gitter [R Suite room](https://gitter.im/WLOGSolutions/RSuite
  "Gitter R Suite room")
* directly by sending email with your problem description to
  [rsuite@wlogsolutions.com](mailto:rsuite@wlogsolutions.com).

# Installation

Currently installation is supported only via manual. It is planned to
add this package to [MELPA
repository](https://melpa.org/#/getting-started "MELPA repository"). 

## Download package

You have to download `rsuite.el` using this
[link](https://github.com/WLOGSolutions/RSuite "rsuite.el") into your
`~\.emacs.d\myscripts\` folder. You can use any other folder but
remember to adapt next steps to your folder.

## Make package available to Emacs

We use [use-package](https://github.com/jwiegley/use-package
"use-package") for managing our Emacs' packages. So first you have to
install `use-package` package. We recommend using [MELPA repository](https://melpa.org/#/getting-started "MELPA repository"). 

Having `use-package` you can add the following set of instructions to
your `~\.emacs.d\init.el` file. Note the keybindings can be customized
to your needs.

``` emacs-lisp
(use-package rsuite
  :load-path "~/.emacs.d/myscripts/"
  :bind (
	 ("C-x M-r p s" . rsuite-proj-start)
	 ("C-x M-r p p" . rsuite-proj-pkg-start)
	 ("C-x M-r p b" . rsuite-proj-build)
	 ("C-x M-r p d" . rsuite-proj-depsinst)
	 ("C-x M-r p z" . rsuite-proj-zip)
	 ("C-x M-r d i" . rsuite-docker-zip-image)
	 ("C-x M-r d p" . rsuite-docker-zip-platform)
	 ("C-x M-r d P" . rsuite-docker-image-platform)
	 ("C-x M-r d I" . rsuite-docker-image-image))
     ("C-x M-r r c" . rsuite-sysreqs-collect)
	 ("C-x M-r r e" . rsuite-sysreqs-check)
	 ("C-x M-r r s" . rsuite-sysreqs-script)
	 ("C-x M-r r i" . rsuite-sysreqs-install))
```

### Using hydra ###

![Emacs Hydra integration with R Suite exemplary
screencast](https://github.com/WLOGSolutions/RSuite/blob/master/docs/media/rsuite_emacs_hydra.png
"Emacs Hydra integration with R Suite")

An alternative configuration is to use [Hydra](https://github.com/abo-abo/hydra) to define keybindings. The exemplary instructions are presented below. It creates three sets of keybindings: (a) project management, (b) docker integration, (c) system requirements management.

``` emacs-lisp
(use-package rsuite
  :load-path "~/.emacs.d/myscripts"
  :defer 1
  :demand t
  :init
  (add-hook 'rsuite-projects-mode-hook 'hl-line-mode)
  :config
  (defhydra rsuite-proj (:hint nil :color pink)
    "
_S_tart project
_b_uild project    
install _d_ependencies
add _p_ackage
_l_ock project 
_u_nlock project 
_z_ip project

_q_uit
"
    ("b" rsuite-proj-build :exit t)
    ("d" rsuite-proj-depsinst :exit t)
    ("p" rsuite-proj-pkg-start  :exit t)
    ("l" rsuite-proj-lock :exit t)
    ("u" rsuite-proj-unlock :exit t)
    ("z" rsuite-proj-zip :exit t)
    ("S" rsuite-proj-start :exit t)
    
    ("q" nil)
    ("." nil :color blue))

  (defhydra rsuite-docker (:hint nil :color pink)
    "
zip _p_latform
zip _i_mage
image _P_latform
image _I_mage

_q_uit
"
    ("p" rsuite-docker-zip-platform :exit t)
    ("i" rsuite-docker-zip-image :exit t)
    ("P" rsuite-docker-image-platform :exit t)
    ("I" rsuite-docker-image-image :exit t)
    ("q" nil)
    ("." nil :color blue))

  (defhydra rsuite-sysreqs (:hint nil :color pink)
    "
_c_ollect
ch_e_ck
_s_cript
_i_nstall

_q_uit
"
    ("c" rsuite-sysreqs-collect :exit t)
    ("e" rsuite-sysreqs-check :exit t)
    ("s" rsuite-sysreqs-script :exit t)
    ("i" rsuite-sysreqs-install :exit t)
    ("q" nil :coloer blue)
    ("." nil :color blue))

  :bind (
	 ("C-x M-r p" . rsuite-proj/body)
	 ("C-x M-r d" . rsuite-docker/body)
	 ("C-x M-r r" . rsuite-sysreqs/body)))
```


# Reference

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
