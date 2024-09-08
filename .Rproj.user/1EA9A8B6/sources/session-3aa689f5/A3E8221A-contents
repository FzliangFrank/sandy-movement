
#' @noMd
context_order = function(x,...) {
  if(inherits(x, 'tbl_lazy')) {
    dbplyr::window_order(x,...)
  } else {
    arrange(x,...)
  }
}
#' Validation Checker ==========================================================
#' @rdname Mischief
#' @export
consistency=function(x) UseMethod('consistency',x)
#' @rdname Mischief
#' @export
consistency.auditLog = function(x,...) {
  rn = x |>
    group_by(.id, .add=T) |>
    context_order(.logtime) |>
    mutate(
      .theory_from = lag(.to, order_by=.logtime),
      .theory_to = lead(.from, order_by=.logtime)
    ) |>
    mutate(
      .from_is_valid=.theory_from == .from | is.na(.theory_from),
      .to_is_valid=.theory_to == .to | is.na(.theory_to)
    ) |>
    mutate(
      .consistent = all(.from_is_valid & .to_is_valid)
    ) |>
    ungroup(.id)
  if(x |> is.auditLog()) rn = as_auditLog(rn)
  if(x |> is.periodAuditlog()) rn=as_periodAuditlog(rn, attr(x, 'period'))
  return(rn)
}

# Validation Handler ==========================================================
#' Handling Consistency
#' @name Mischief-Consistency-Handler
#' @export
force_consist = function(x,...) UseMethod('force_consist',x)
#' @rdname Mischief-Consistency-Handler
#' @export
force_consist.auditLog = function(x,...) {
  x |>
    group_by(.id, .add=T) |>
    # arrange(.id, .logtime)|>
    mutate(
      .from = coalesce(lag(.to,order_by=.logtime), .from)
    ) |>
    ungroup(.id) |>
    as_auditLog()
}
#' @rdname Mischief-Consistency-Handler
#' @export
force_consist.periodAuditlog = function(x,...) {
  P <- attr(x, 'period')
  as_periodAuditlog(NextMethod(...), P)
}
#' @rdname Mischief-Consistency-Handler
#' @export
remove_head = function(x,...) UseMethod('remove_head',x)
#' @rdname Mischief-Consistency-Handler
#' @export
remove_head.auditLog = function(x,...) {
  t = x |>
    group_by(.id) |>
    filter(!is.na(.from) & row_number(.logtime) == 1) |>
    ungroup() |>
    count() |>
    pull() |>
    as.numeric()
  if(t!=0) {
    warning(sprintf('%i logs has first status set to NA because there are no log time.\n',t))
    x |>
      group_by(.id) |>
      mutate(
        .from = ifelse(row_number(.logtime)==1, NA, .from)
      ) |>
      ungroup() |>
      as_auditLog()
  } else {
    return(x)
  }
}
remove_head.periodAuditlog = function(x,...) {
  P <- attr(x, 'period')
  as_periodAuditlog(NextMethod(...), P)
}



