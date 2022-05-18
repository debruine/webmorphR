# code coverage

Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(function_exclusions = "quick_delin")
covr::report(cov)
# commit first
covr::codecov(coverage = cov)
