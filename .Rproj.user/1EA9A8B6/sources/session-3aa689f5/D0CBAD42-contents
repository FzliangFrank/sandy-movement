## This script for use to generat fake data used for audit log

#' Generage 1 Fake Log
#' @details Should not be called directly
#' @param fake_id what you use for object id
#' @param freq number of audit log that one object should pass through.
#' @noMd
make_fakelog_1 = function(fake_id, freq, n_status = 3) {

  fake_to = sample(seq(n_status), size=freq, replace=T)
  fake_from=lag(fake_to)
  wts = rnorm(freq, 10, 2) # average value is always 10 or 2 so

  x = sample(seq(freq), size=1)
  # Generate Time-stamp For One Single Year
  Year_23 = seq(ymd('2023-01-01'), ymd('2023-12-31'), by='1 day')

  fake_timelog=sort(sample(Year_23, size=freq))
  data.frame(
    id = fake_id,
    from = fake_from,
    to = fake_to,
    logtime=fake_timelog,
    weight = round(wts,2)
  )
}

#' Generate Fake Audit Log for Demo
#' @param n_ids number of unique object id to appear in the log
#' @param max_freq max number of status flow to generate by a consecutive number
#' @param n_status
#' @return data.frame
#' @export
make_fakelog=function(
    n_ids = 4
  , max_freq=9
  , n_status=4
  , state_schema = NULL
  , state_noise = NULL
  ) {
  stopifnot(state_noise <= 1 & state_noise >= 0)
  ## for each object, generate maximum frequency
  ## the maximum frequency is 9 
  sample(seq(1, max_freq)# a vector of frequency
         , n_ids #number of object
         , replace=T) |>
    purrr::imap_dfr(
      ~make_fakelog_1(fake_id=.y, freq=.x, n_status=n_status)
    )
}


validate_incflow = function(netflow) {
  netflow |>
    group_by(.state) |>
    arrange(.state,.period) |>
    mutate(n_state = cumsum(.inc_flow)) |>
    ungroup() -> history_state

  history_state |>
    group_by(.period) |>
    summarise(n_obj = sum(n_state)) -> creation_curve

  fake_log |>
    group_by(id) |>
    arrange(logtime) |>
    slice_head(n=1) |>
    ungroup() |>
    rename(create_time = logtime) |>
    mutate(create_month = lubridate::floor_date(create_time, reduce_period)) |>
    # select(create_month) |>
    mutate(n =1) |>
    arrange(create_month) |>
    mutate(n_obj=cumsum(n)) |>
    group_by(create_month) |>
    slice_tail(n=1) |>
    select(create_month, n_obj) -> tho_creation_cruve

  full_join(
    creation_curve,
    tho_creation_cruve,
    by=c(".period"="create_month", "n_obj"="n_obj")
  ) -> merged

  expect_true(
    all(!is.na(merged$`.period`)) && all(!is.na(merged$n_obj))
  )
}
