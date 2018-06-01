**Before submitting a pull request**, please make sure the following is done:

1. Run checks (devtools::check) without ERRORs, WARNINGs or important NOTEs
2. Run examples (devtools::run_examples) without errors
3. Build vignettes without errors
4. Fix linkr (lintr::lint_package) complains
5. Ensure documentation in RSuite/R/package_imports.R is consistent with API
6. Make sure RSuite/NEWS.md is updated
7. Pass tests for RSuite and RSuite CLI (at least on Windows, CentOS and Ubuntu)
