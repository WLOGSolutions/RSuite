## Test environments
* local Windows install, R 3.5.0, R-devel
* ubuntu 14.04, R 3.2.5
* debian 8.9, R 3.3.3
* centos 7.4, R 3.4.2
* MacOS Sierra 10.12, R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
No issues detected

## Previous submission comments
* Authomatic building used to fail because some examples (and vignette building) 
  assumed todays MRAN is available. While creating R Suite project available MRAN 
  is detected now, so there is no dependency on time of build. Examples which 
  required MRAN access marked with donttest.
* imports from httr fixed not to collide with git2r
