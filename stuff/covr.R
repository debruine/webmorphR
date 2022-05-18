# code coverage

Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(function_exclusions = "quick_delin")
covr::codecov(coverage = cov)
