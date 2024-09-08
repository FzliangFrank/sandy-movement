#' Modules
#' 1. Complete Audit Log
#' @name audit_log_class
#' @param dbi a dbi connection object
#' @param id,from,to,logtime mandatory field for a dbi object
#' @param weight, an optional column to track and aggregate,
#' by defaul this creating object as weight = 0
#' @param period,P for making period audit log instead. Period for column indicating
#' periodic time stamp, P for frequency.
#' @export
new_auditLog <- function(
    dbi,
    id,
    from,
    to,
    logtime,
    weight,
    period,
    P=NULL
  ) {
  classColoni=c('.id','.from','.to','.logtime','.weight')
  nameTest=classColoni %in% names(dbi)
  if(any(nameTest)) {
    msg=stringr::str_flatten_comma(paste0('`', classColoni[nameTest], '`'))
    warning(glue::glue(
      "{msg} will be occupied by auditLog class!"
    ))
  }
  
  ## 
  x = dbi |>
    rename(.id = {{id}},
           .from={{from}},
           .to = {{to}},
           .logtime={{logtime}}
    )
  if(!missing(weight)) {
    x = mutate(x, .weight={{weight}})
  } else {
    x = mutate(x, .weight=1)
  }
  
  if(!missing(period)) {
    stopifnot(P %in% c('month','day','week','year'))
    x = x |> 
      rename(.period={{period}})
    x = as_periodAuditlog(x, P)
  } else {
    class(x) <- c('auditLog', class(x))
  }
  return(x)
}

#' Complete Audit Log
#' @rdname audit_log_class
#' @export
complete_with.auditLog <- function(x, df) {
  stopifnot(is.auditLog(x))
  print('placeholder in dev')
}

#' @rdname reduce_noise
#' @export
reduce_noise <- function(x, P) UseMethod("reduce_noise", x)
#' 2. Periodic Audit Log
#' @rdname reduce_noise
#' @param P month, day, week or any other period
#' @export
reduce_noise.auditLog <- function(x, P='month') {
  # Create The Period You Need
  obj = x |>
    mutate(.period = lubridate::floor_date(.logtime,unit=P)) |>
  # Aggregation
    group_by(.period, .id, .add=T) |>
    mutate(
      .from=dplyr::first(.from,order_by=.logtime),
      .to=dplyr::last(.to,order_by=.logtime),
      .logtime=dplyr::last(.logtime,order_by=.logtime)
      ) |>
    ungroup(
      .period, .id
    ) |>
    distinct(.id, .period, .keep_all=T)
  as_periodAuditlog(obj, P)
}

#' 3. Net Flow =================================================================
#' @name NetFlow
#' @details
#' Generate Timeline will by default creating only one possible


#' @rdname NetFlow
#' @export
generate_fullperiod_db <- function(conn, start, end, P, all_state) {
  stopifnot('.state' %in% names(all_state))
  interval=paste(1, P)
  tl = conn |> tbl(sql(glue::glue(
    r'(select generate_series('{start}'::date, '{end}'::date, '{interval}'::interval) as ".period")'
  ))) |>
    cross_join(all_state) |>
    as_auditlogFlows()
  return(tl)
}

#' @rdname NetFlow
#' @export
generate_fullperiod_loc <- function(start, end, P, all_state_v) {
  data.frame(
    .period = seq(from = ymd(start),to= ymd(end), by=paste(1,P))
  ) |>
    cross_join(data.frame(.state=all_state_v)) |>
    as_auditlogFlows()
}

#' @rdname NetFlow
#' @export
cal_outflow <- function(x,...) UseMethod('cal_outflow', x)

#' @rdname NetFlow
#' @export
cal_flow <- function(x, colVar, name) {
  x |>
    filter(!is.na({{colVar}})) |>
    group_by({{colVar}}, .period, .add=T) |>
    count(wt=.weight,name=name) |>
    ungroup({{colVar}}, .period) |>
    rename(.state={{colVar}}) |>
    as_auditlogFlows()
}
#' @rdname NetFlow
#' @export
cal_inflow <- function(x,...) UseMethod("cal_inflow", x)
#' @rdname NetFlow
#' @export
cal_inflow.periodAuditlog <- function(x) {
  stopifnot(is.periodAuditlog(x))
  df = x
  inflow=df |>
    cal_flow(.to, '.in_flow')
  return(inflow)
}
#' @rdname NetFlow
#' @export
cal_outflow.periodAuditlog <-function(x){
  stopifnot(is.periodAuditlog(x))
  df = x
  outflow=df |>
    cal_flow(.from, '.out_flow')
  return(outflow)
}

#' @rdname NetFlow
#' @export
cal_netflow <- function(x,...) UseMethod('cal_netflow',x)
#' @rdname NetFlow
#' @export
cal_netflow.periodAuditlog <- function(x, fillgap = F) {
  if(is.null(attr(x,'period'))) stop("Audit Log Must have Attribute Period!")
  gVars<-group_vars(x)
  inflow = x |> cal_inflow()
  outflow = x |> cal_outflow()
  if(fillgap) {
    start = summarise(ungroup(x), min(.period, na.rm=T)) |> pull() # these two are not
    end = summarise(ungroup(x), max(.period, na.rm=T)) |> pull()
    interval = attr(x, 'period')
    all_state = distinct(ungroup(dplyr::union_all(
      select(x, .state=.from) |> filter(!is.na(.state)),
      select(x, .state=.to)
    ))) |> pull()

    full_flow=join(inflow, outflow)
    full_flow |> 
      ungroup() |> 
      tidyr::expand(.period,.state,dplyr::vars(gVars)) |> 
      as_auditlogFlows() |> 
      join(full_flow) |> 
      mutate(across(c(.in_flow, .out_flow), ~coalesce(.x, 0))) |>
      mutate(.net_flow= .in_flow - .out_flow)
  } else {
    join(inflow, outflow) |>
      mutate(across(c(.in_flow, .out_flow), ~coalesce(.x, 0))) |>
      mutate(.net_flow= .in_flow - .out_flow)
  }
}


#' @rdname Process
#' @export
join <- function(x,y,...) UseMethod('join',x)

#' @rdname NetFlow
#' @export
join.auditlogFlows <- function(x, y,...) {
  stopifnot(is.auditlogFlows(x) && is.auditlogFlows(y))
  g_vars=union(group_vars(x),group_vars(y))
  
  x |>
    full_join(y, by=c('.period', '.state', g_vars), copy=T,...,
                 relationship='one-to-one') |>
    as_auditlogFlows()
}

