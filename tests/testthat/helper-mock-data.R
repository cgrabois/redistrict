make_mock_data <- function(
    mat, cyc = "cd101_cd102", st = "XX",
    vars = c("pop", "area", "afact_s2t", "afact_t2s", "afact_s2t_area", "afact_t2s_area")
) {
  stats::setNames(
    lapply(vars, function(v) stats::setNames(list(stats::setNames(list(mat), st)), cyc)),
    vars
  )
}
