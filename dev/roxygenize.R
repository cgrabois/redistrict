# dev/roxygenize.R ###############################################################
# Regenerates NAMESPACE and man/ from roxygen comments. Safe to re-run anytime.
#
# Run from the project root:
#   Rscript dev/roxygenize.R

roxygen2::roxygenise(getwd())
