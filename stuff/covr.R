# code coverage

Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage()
covr::codecov(coverage = cov)
