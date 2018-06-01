## Test environments
* local Windows install, R 3.5.0, R-devel
* ubuntu 14.04, R 3.2.5
* debian 8.9, R 3.3.3
* centos 7.4, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs.

NOTES are reported that running of examples took too much time:
```
** running examples for arch 'i386' ... [400s] NOTE
Examples with CPU or elapsed time > 10s
                            user system elapsed
prj_zip                     4.47   0.84   60.34
pkgzip_build_prj_packages   4.51   0.75   97.66
repo_upload_prj_packages    4.30   0.93   53.27
prj_build                   3.85   0.92   66.55
pkgzip_build_github_package 3.11   0.91   38.07
repo_upload_github_package  2.83   0.70   34.19
sysreqs_script              0.91   0.25   11.25
** running examples for arch 'x64' ... [307s] NOTE
Examples with CPU or elapsed time > 10s
                            user system elapsed
pkgzip_build_prj_packages   4.10   0.73   53.54
prj_zip                     4.03   0.78   52.23
repo_upload_prj_packages    3.90   0.78   51.57
prj_build                   3.95   0.66   54.75
pkgzip_build_github_package 3.29   0.67   32.12
repo_upload_github_package  2.87   0.78   29.62
```

## Downstream dependencies
No issues detected
