#' Audit Log Analytic Process
#'
#' Formula to restore a historical status is
#' = Accumulative Sum of Net Flow of Each Status in Each Month.
#'
#' Net-Flow
#' = In-Flow Number of Objects in Each Status - Out-Flow Number of Object
#'
#' Period to this process. Audit log assume to be continuous and Complete.
#' which means the date when object is first created and first status is
#' recorded.
#'
#' Periodic Audit log is the process reduce the noise of audit log by period
#' start and period end

#' @name Process
#' @export
reconstruct.auditLog <- function(x, full_period, by, strategy='force') {
  # First Step
  print(sprintf('Aggregating Data By Period %s', by))
  Periodic_Log = x |> reduce_noise(by=by) |> compute()
  print('Temporary Table Created For The Session')

  print('Creating In and Out Flow')
  inflow=Periodic_Log |> cal_inflow()
  outflow=Periodic_Log |> cal_outflow()
  inc_flow = combine(full_period, inflow, outflow)

  print('Created Flow')
}

# All Arrows =================================================================


# All Objects =================================================================
#'@noMd
as_ <- function(x, objName) {
  class(x) <- c(objName, class(x))
  return(x)
}

#' Class Definition for Log Class
#' @name logclass
#' @export
as_auditLog <- function(x) {
  if(inherits(x,'auditLog')) return(x)
  if(!inherits(x,'tbl_lazy')) stopifnot(all(c('.from','.to','.logtime') %in% names(x)))
  x = as_(x, 'auditLog')
  return(x)
}
#' @rdname logclass
#' @export
as_periodAuditlog <- function(x, P) {
  if(inherits(x,'periodAuditlog')) return(x)
  if(!is.auditLog(x)) {
    x = as_auditLog(x)
  }
  if(!inherits(x,'tbl_lazy')) stopifnot('.period' %in% names(x))
  x = as_(x, 'periodAuditlog')
  attr(x,'period') <- P
  return(x)
}
#' @rdname logclass
#' @export
as_auditlogFlows <- function(x) {
  if(inherits(x,'auditlogFlows')) return(x)
  colReq=c('.state', '.period')
  if(!inherits(x,'tbl_lazy')) stopifnot(all(colReq %in% names(x)))
  x = as_(x,'auditlogFlows')
  return(x)
}

#' @rdname logclass
#' @export
as_auditlogIncrement <- function(x) {
  if(inherits(x,'auditlogIncrement')) return(x)
  colReq=c('.state', '.period', '.increment', '.error')
  if(!inherits(x,'tbl_lazy')) stopifnot(all(colReq %in% names(x)))

  x = as_(x,'auditlogIncrement')
  return(x)
}
#' @rdname logclass
#' @export
as_auditlogHistory <- function(x) {
  if(inherits(x,'auditlogHistory')) return(x)
  colReq=c('.state', '.period', '.history', '.error')
  if(!inherits(x,'tbl_lazy')) stopifnot(all(colReq %in% names(x)))

  x = as_(x,'auditlogHistory')
  return(x)
}


# Class Validation =============================================================

is.auditLog <- function(x) inherits(x, 'auditLog')
is.periodAuditlog <- function(x) {
  inherits(x, 'periodAuditlog') && !is.null(attr(x, 'period'))
}
is.auditlogFlows <- function(x) inherits(x, c('auditlogFlows'))
is.auditlogIncflow <- function(x) inherits(x, c('auditlogIncflow'))

# Method Exported From Other Package -------------------------------------------
#' Foreign Class imported from other packages
#' @name foreign
# Import or Export Method From Other Package Will Keep
#' @rdname foreign
#' @export
compute_auditlog <- function(x,...) {
  P <- attr(x, 'period')
  as_periodAuditlog(dplyr::compute(x),P)
}
#' @rdname foreign
#' @export
collect_auditlog <- function(x,...) {
  P <- attr(x, 'period')
  as_periodAuditlog(dplyr::collect(x) |> as_tibble(),P)
}

#' @rdname foreign
#' @export
group_by.auditlog <- function(x, ...) {
  rn=NextMethod('group_by',x)
  return(as_auditLog(rn))
}
#' @rdname foreign
#' @export
group_by.periodAuditlog <- function(x, ...) {
  P <- attr(x, 'period')
  rn = NextMethod('group_by',x)
  return(as_periodAuditlog(rn, P))
}
#' @rdname foreign
#' @export
group_by.auditlogFlows <- function(x,...) {
  rn=NextMethod('group_by',x)
  return(as_auditlogFlows(rn))
}

#' @rdname foreign
#' @export
ungroup.auditLog <- function(x, ...) {
  rn=NextMethod('ungroup', x)
  as_auditLog(rn)
}
#' @rdname foreign
#' @export
ungroup.periodAuditlog <- function(x,...) {
  P <- attr(x, 'period')
  rn=NextMethod('ungroup',x)
  return(as_periodAuditlog(rn, P))
  
}
ungroup.auditlogFlows <- function(x) {
  rn=NextMethod('ungroup',x)
  return(as_auditlogFlows(rn))
}

#' @rdname foreign
#' @export
arrange.auditlog <- function(x, ...) {
  as_auditLog(NextMethod('arrange',x))
}
#' @rdname foreign
#' @export
arrange.periodAuditlog <- function(x, ...) {
  P <- attr(x, 'period')
  rn = NextMethod('arrange',x)
  return(as_periodAuditlog(rn, P))
}
#' @rdname foreign
#' @export
arrange.auditlogFlows <- function(x,...) {
  rn=NextMethod('arrange',x)
  return(as_auditlogFlows(rn))
}

#' @rdname foreign
#' @export
mutate.auditlog <- function(x, ...) {
  rn=NextMethod('arrange',x)
  as_auditLog(rn)
}
#' @rdname foreign
#' @export
mutate.periodAuditlog <- function(x, ...) {
  P <- attr(x, 'period')
  rn = NextMethod('arrange',x)
  return(as_periodAuditlog(rn, P))
}
#' @rdname foreign
#' @export
mutate.auditlogFlows <- function(x,...) {
  rn=NextMethod('arrange',x)
  return(as_auditlogFlows(rn))
}

#' @rdname foreign
#' @export
filter.auditlog <- function(x, ...) {
  rn=NextMethod('arrange',x)
  as_auditLog(rn)
}
#' @rdname foreign
#' @export
filter.periodAuditlog <- function(x, ...) {
  P <- attr(x, 'period')
  rn = NextMethod('arrange',x)
  return(as_periodAuditlog(rn, P))
}
#' @rdname foreign
#' @export
filter.auditlogFlows <- function(x,...) {
  rn=NextMethod('arrange',x)
  return(as_auditlogFlows(rn))
}

#' @rdname foreign
#' @export
select.auditlog <- function(x, ...) {
  rn=NextMethod('arrange',x)
  as_auditLog(rn)
}
#' @rdname foreign
#' @export
select.periodAuditlog <- function(x, ...) {
  P <- attr(x, 'period')
  rn = NextMethod('arrange',x)
  return(as_periodAuditlog(rn, P))
}
#' @rdname foreign
#' @export
select.auditlogFlows <- function(x,...) {
  rn=NextMethod('arrange',x)
  return(as_auditlogFlows(rn))
}
