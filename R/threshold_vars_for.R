#' Companion allocation-factor variables for thresholding
#'
#' Returns the allocation-factor variable(s) used for thresholding when the
#' matching variable is `variable`. For `"pop"` and `"area"`, both directions
#' of the afact pair must exceed the threshold. For afact variables, only the
#' variable itself is checked, since the user already chose a single
#' direction.
#'
#' @param variable String — one of `"pop"`, `"area"`, `"afact_s2t"`,
#'   `"afact_t2s"`, `"afact_s2t_area"`, `"afact_t2s_area"`.
#'
#' @return Character vector of one or two variable names:
#'   \describe{
#'     \item{`"pop"`}{`c("afact_s2t", "afact_t2s")`}
#'     \item{`"area"`}{`c("afact_s2t_area", "afact_t2s_area")`}
#'     \item{any `afact_*` variable}{itself}
#'   }
#'
#' @export
threshold_vars_for <- function(variable) {
  switch(variable,
         "pop"            = c("afact_s2t",      "afact_t2s"),
         "area"           = c("afact_s2t_area", "afact_t2s_area"),
         "afact_s2t"      = "afact_s2t",
         "afact_t2s"      = "afact_t2s",
         "afact_s2t_area" = "afact_s2t_area",
         "afact_t2s_area" = "afact_t2s_area",
         stop(paste0("Unknown variable: '", variable, "'"))
  )
}
