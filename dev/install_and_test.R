# dev/install_and_test.R #########################################################
# Installs redistrict from source into a throwaway library (dev/lib, gitignored),
# including its test suite, then runs the tests from that installed copy. Safe to
# re-run anytime. Run dev/roxygenize.R first if you've changed any roxygen
# comments.
#
# Run from the project root:
#   Rscript dev/install_and_test.R

pkg_root <- getwd()
lib_dir  <- file.path(pkg_root, "dev", "lib")

dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

cat("Installing redistrict into", lib_dir, "...\n")
install.packages(
  pkg_root,
  repos        = NULL,
  type         = "source",
  lib          = lib_dir,
  INSTALL_opts = "--install-tests"
)

cat("\nRunning tests from the installed copy in", lib_dir, "...\n")
library(redistrict, lib.loc = lib_dir)
testthat::test_dir(system.file("tests", "testthat", package = "redistrict"))
