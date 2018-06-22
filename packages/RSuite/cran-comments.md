## Test environments
* local Windows install, R 3.5.0, R-devel
* ubuntu 14.04, R 3.2.5
* debian 8.9, R 3.3.3
* centos 7.4, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs.

Single NOTE generates by win-builder:
Missing or unexported object: 'git2r::diff'

Which seems to be misreporting as RSuite imports the whole
git2r namespace.

## Downstream dependencies
No issues detected

## Previous submission comments
* examples added to all API functions and none of them runs 
	longer than 5secs. Examples which cannot take less time
	(as their main purpose is to present functionality of
	downloading/instaling/building packages) are marked with
	\donttest{}. But all of them checked with devtools::run_examples.
* package functionalities do not modify user space (only tempdir()
	is in use) unless user specifies to put cache (or project/package
	templates) there by means of setting option rsuite.cache_path.
* fixed wording in package DESCRIPTION.
