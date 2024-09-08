test_that("Fake Log Should Generate Correct Number of Logs", {
  rd_max = sample(seq(3,50), size=1) # min 3 max 12 number statuses
  rd_max_freq = sample(seq(5, 9), size=1) # 1 object should go through n number of status changes
  error = 0

  message(sprintf(glue::glue(
    'Testing has maximum %i of object\n',
    'Each object have %i number of log\n',
    'For this test we are not including any inconsistent status change'
    ), rd_max, rd_max_freq
    ))
  fakelog=make_fakelog(
    n_ids = rd_max,
    max_freq=rd_max_freq,
    n_error =error
  )

  fakelog |>
    count(id) |>
    nrow() -> number_of_object
  expect_identical(
    number_of_object,
    rd_max,
  )

  fakelog |>
    group_by(id) |>
    summarise(n=n()) |>
    pull(n) |>
    max() -> max_number_of_freq
  expect_true(
    max_number_of_freq <= rd_max_freq
  )

  fakelog |>
    group_by(id) |>
    arrange(logtime) |>
    mutate(
      consistency = to == lead(from) | row_number() == n()
    ) |>
    pull(consistency) -> log_consistent
  expect_true(
    all(log_consistent)
  )
})
