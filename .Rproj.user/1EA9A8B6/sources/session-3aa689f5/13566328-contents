test_that("", {
  any_data = make_fakelog()
  audit_log = any_data |> as_auditLog()
  testthat::expect_is(audit_log, "auditLog")
  period_log = audit_log |> reduce_noise("month")
  period_log
})

