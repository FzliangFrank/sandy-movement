#' Create Incremental Status Audit
#' @name report_increment
#' @export
report_increment <- function(x,...) UseMethod('report_increment', x)

#' @rdname report_increment
#' @export
report_increment.periodAuditlog <- function(audit_log_p, clean = T) {
  
  # Label Certain Code Chunk To Corresponding.
  if(clean) {
    audit_log_p |>
      # remove_head() |>
      consistency() |>
      # from this part forward
      mutate(
        was_added = .to,# in the end this effect needs to be reversed
        was_removed = lag(.to, order_by=.logtime),# same as above
        could_removed = .from, # this effect could be added
        could_add = lead(.from, order_by=.logtime)
      ) |> 
      dplyr::filter(!(.from_is_valid & .to_is_valid)) -> consistent_checked # only down to those has problems
    
    # stopifnot(
    #     inherits(consistent_checked,'periodAuditlog')
    # )
    need_cleaning = sum(pull(count(consistent_checked))) != 0
  } else {
    need_cleaning = F
  }
  if(need_cleaning) {
    warning("The audit Log are inconsistent. Treating log by remove invalid from.\n")
   
    a1 = consistent_checked |>
      mutate(
        .from = was_removed,
        .to = was_added
      ) |>
      cal_netflow() |>
      select(.state, .period, .net_flow)

    # Taken Out Because Of Force Lean
    a2 = consistent_checked |>
      mutate(
        .from = could_removed,
        .to = could_add
      ) |>
      cal_netflow() |>
      select(.state,.period,.net_flow)

    # This Code Chunk Force Clean Audit Log
    a3 = audit_log_p |>
      group_by(.id,.add=T) |>
      arrange(.id, .period)|>
      mutate(
        .from = lag(.to, order_by=.period)
      ) |>
      ungroup(.id) |>
      cal_netflow()

    a4 = a3 |>
      join(a1, suffix=c('', '_was')) |>
      join(a2, suffix=c('', '_could')) |>
      mutate(
        .error = coalesce(.net_flow_could,0) - coalesce(.net_flow_was,0)
      ) |>
      arrange(.state) |>
      select(-.net_flow_was,-.net_flow_could)
  } else {
    message("Audit Log is okay!")
    a4 = audit_log_p |>
      cal_netflow() |>
      mutate(.error=0)
  }
  # What Has Been Added Before?
  rn = a4 |>
    rename(.increment=.net_flow) |>
    as_auditlogIncrement()
  # a4 <- as_auditlogHistory(a4)
  return(rn)
}

#' Create Historical State From Periodic Reduced Log
#' @rdname report_history
#' @export
report_history<- function(x,...) UseMethod('report_history', x)

#' @rdname report_history
#' @export
report_history.auditlogIncrement = function(increment_state) {
  increment_state |>
    group_by(.state) |>
    context_order(.state, .period) |>
    mutate(.history=cumsum(.increment), .error=cumsum(.error)) |>
    ungroup(.state) |>
    as_auditlogHistory()
}

#' @rdname report_history
#' @export
report_history.periodAuditlog <- function(audit_log_p, clean=F) {
  audit_log_p |>
    report_increment() |>
    report_history.auditlogIncrement()
}
