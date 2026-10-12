
## Here save an original copy of data and use to validate
## support dpblyr backend and duck-end
# log |> saveRDS("dev/test-use-log.RDS")
log = readRDS("dev/test-audit-log.RDS")
log |> 
  new_auditLog(
    id = id
    , from = from
    , to = to 
    , logtime = logtime
    # , weight = weight
    ) |> 
  reduce_noise("month") |> 
  new_auditLog() -> ag_log

ag_log |> 
  reduce_noise("month") |>
  cal_netflow()

## like this graph view... 
rp_incmt = ag_log |> 
  reduce_noise("month") |>
  report_increment()

rp_hstry = ag_log |> 
  reduce_noise("month") |>
  report_history()

output = list(
  original_audit_log = log,
  incremental_flow = rp_incmt,
  history_recreated = rp_hstry,
  incremental_flow.plot = plot(rp_incmt) + ggtitle("Each month increment log should look something like this"),
  history_recreated.plot = plot(rp_hstry) + ggtitle("History recreated should looks something like this")
)

saveRDS(output, "dev/test-log-output.RDS")



## Here save an original copy of data and use to validate
## support dpblyr backend and duck-end
# log |> saveRDS("dev/test-use-log.RDS")
log = readRDS("dev/test-use-log.RDS")
log |> 
  new_auditLog(
    id = id
    , from = from
    , to = to 
    , logtime = logtime
    , weight = weight
  ) |> 
  reduce_noise("month") -> ag_log

ag_log |> 
  reduce_noise("month") |>
  cal_netflow()

## like this graph view... 
rp_incmt = ag_log |> 
  reduce_noise("month") |>
  report_increment()

rp_hstry = ag_log |> 
  reduce_noise("month") |>
  report_history()

output = list(
  original_audit_log = log,
  incremental_flow = rp_incmt,
  history_recreated = rp_hstry,
  incremental_flow.plot = plot(rp_incmt) + ggtitle("Each month increment log should look something like this"),
  history_recreated.plot = plot(rp_hstry) + ggtitle("History recreated should looks something like this")
)

# saveRDS(output, "dev/test-audit-log-output-wt.RDS")
library(tidyverse)

## Now we need to reinvent this system
## This is a more granular version of the log
## the name should be customized?
flow_log_1 = log |> 
  mutate(
    pv_weight = lag(weight, order_by = logtime)
  ) |> 
  pivot_longer(
    cols = c(from,to),
    names_to = "flow",
    values_to="state"
  ) |> 
  mutate(
    flow = case_when(flow == "from" ~"out", flow == "to" ~ "in")
  ) |> 
  mutate(
    value = case_when(
      flow == "out" ~ -pv_weight,
      flow == "in" ~ weight
    ) |> coalesce(0)
  ) |> 
  select(id,flow,state, logtime,value)
## this model does not have value flow yet because there are no ones that status 
## is the same but weight diff


## since `reduce_noise` is my fave I need a think over if this should be implemented
## again but here we no longer have period anymore in this data anymore
log |> 
  filter(from == to)



## let's see if we can go without reduce noise
flow_log |> 
  filter(!is.na(state)) |> 
  group_by(logtime, state) |> 
  summarise(value = sum(value)) |> 
  ggplot() + 
  geom_col(aes(x = logtime, y = value)) + 
  facet_wrap(~state)

p_flow_log = flow_log |> 
  filter(!is.na(state)) |> 
  mutate(p = floor_date(logtime, "month")) |> 
  group_by(p, state) |> 
  summarise(value = sum(value)) 

p_flow_log |> 
  ggplot() + 
  geom_col(aes(x = p, y = value)) + 
  facet_wrap(~state)

## seems we created exactly same plot by modify log time;
## 
output$incremental_flow |> 
  left_join(p_flow_log, c(".state"="state",".period"="p")) |> 
  filter(value != ".increment")

## Next just need to think about presentation of this very.
flow_log |> 
  mutate(period = floor_date(logtime,"month")) |> 
  filter(period == "2023-10-01" & state == 4) |> 
  arrange(id) |> 
  filter(id == 29)


flow_log |> 
  mutate(period = floor_date(logtime,"month")) |> 
  filter(id == 29 & period == "2023-10-01") |> 
  arrange(logtime)

log |> filter(id == 29)

## This is a stochastic graph log generated
library(ggraph)
ordered_log=make_ordered_log(Exposed -+ Aware -+ Ghosted:Interested,Interested-+Purchase, 
                             n_particle = 10)
sand = ordered_log |> 
  grind_log()

  

ordered_log |> 
  new_auditLog(id,from,to,logtime,weight) |> 
  reduce_noise("month") |> 
  report_history() |> 
  select(.state, .period, .increment) -> x1
names(x1) <- sub(".", "", names(x1), fixed= T)

sand |> 
  mutate(period = floor_date(logtime, "months")) |> 
  group_by(state, period) |> 
  summarise(flow = sum(value)) -> x2
## are these value different because of value flow??
x2 |> 
  left_join(x1,by = join_by(state, period)) |> 
  filter(flow != increment) |> 
  ungroup() |> 
  slice_sample(n=1) |> 
  glimpse()
## first example
# $ state     <chr> "Aware"
# $ period    <date> 2023-02-01
# $ flow      <dbl> 21.49278 # flow is correct t
# $ increment <dbl> 24.52166

## second example
# $ state     <chr> "Exposed"
# $ period    <date> 2023-06-01
# $ flow      <dbl> -23.63015 
# $ increment <dbl> -18.47234 ## increment is correct???
# but actual this may not be correct 
ordered_log |> 
 group_by(id) |> 
 mutate(weight_from = lag(weight, order_by =logtime)) |> 
 filter(month(logtime) == 6)
 
## outflow = 9.63 +  8.85; inflow = 0
## outflow =  12.6 + 11.0 (23.6); so actually sand is correct
## total exposure will reduce by the weight that has been...
 
## this suggest yes the simpler data structure may as well have been 
## somewhat level of snap-short data structure only having one dimension;
## instead of two is much easier to understand.
## our weight can also be devious because we also don't know what weight means
## right? for object 1, could that means object allowed partial transfer of
## transfer fro 9.63 to aware but the residual (12.6 - 9.63) left "exposed"

## What's the problem? We cannot infer data type for users. If each of these 
## stands for accounting transactions this will goes all wrong.

## Some research had suggested that if data you are trying to capture is
## a business logic then separate table should be built to solely capture that
## But in most case this job works with a snap-short table;

## so we create a snap short?
## this also make thing easier because you will be creating one class instead 
## of two## nwo the problem with this function is that you will have to try
## swap names
grind_log = function(x) {
  ## let from and to capture categorical variable change
  ## let weight capture value change
  ## so that each can measure its immediate change
  names(x)
  
  x0 = x |> 
    ## change this to row-wise comparative
    group_by(id) |> 
    mutate(
      from = lag(state, order_by = logtime),
      pv_weight = lag(weight, order_by = logtime)
    ) |> 
    rename(to=state) |> 
    ungroup() 
    ## charge this so that laps of status log change back
  
  ## now split these two groups, this group captures values goes ups and downs
  ## within the same group.
  x2 = x0 |> 
    filter(from == to) |> 
    mutate(value = weight - pv_weight) |> 
    filter(value != 0) |> 
    mutate(flow ="value") |> 
    rename(state=to)
    # select(id,flow,state=to, logtime,value)
  
  ## one is 
  x1 = x0 |> 
    filter(from != to | is.na(from) ) |> 
    pivot_longer(
      cols = c(from,to),
      names_to = "flow",
      values_to="state"
    ) |> 
    mutate(
      flow = case_when(
        flow == "from" ~"out", 
        flow == "to" ~ "in")
    ) |> 
    mutate(
      value = case_when(
        flow == "out" ~ -pv_weight,
        flow == "in" ~ weight
      ) |> coalesce(0)
    )
    # select(id,flow,state, logtime,value)
  bind_rows(x1,x2) |> 
    select(-from, -pv_weight)
}
disrupted_log = ordered_log |> 
  disrupt_log(0.05)


snapshot = disrupted_log |> 
  select(id,state=to,logtime,weight)
sand=snapshot |> 
  grind_log() |> 
  mutate(period = floor_date(logtime,"months"))
x1=sand |> 
  group_by(period,state) |> 
  summarise(value = sum(value)) |> 
  ungroup()
x0=disrupted_log |> 
  new_auditLog(id=id,from=from,to=to,logtime=logtime,weight) |> 
  reduce_noise("months") |> 
  report_increment()
names(x0) <- sub(".","", names(x0))

## this 
x0 |> 
  left_join(x1, c("state","period")) |> 
  filter(increment != value) |> 
  filter(  state == "Exposed"
         & period == "2023-03-01")
disrupted_log |>
  group_by(id) |> 
  mutate(pw = lag(weight, order_by = logtime)) |> 
  filter(month(logtime) == 3)
sand |> 
  filter(state == "Exposed" 
       & month(period) == 3)

sand

as_snapshot=function(x,...) UseMethod("as_snapshot",x)
as_snapshot.data.frame = function(x,id,state,logtime,weight) {
  req_ns = c("id","state","logtime", "weight")
  arglist=as.list(match.call())[-1]
  dfnames = names(x)
  
  attr_ = arglist[-1]
  attr_ns = lapply(attr_, deparse)
  
  
  ## name guess when not all attributes are input
  require_guess_names = !all(req_ns %in% names(attr_ns))
  
  if(require_guess_names) {
    ## guess by name match
    to_guess = req_ns[!(req_ns %in% names(attr_ns))]
    guessed = dfnames[dfnames %in% to_guess]
    cli::cli_alert_info("Gussed attribute {paste0('`',guessed,'`',collapse=',')}")
    
    if(length(guessed) != length(to_guess)) stop()
    
    attr_plus = as.list(guessed)
    names(attr_plus) <- attr_plus
    attr_ns <- attr_ns |> append(attr_plus)
  }
  
  ## automatically parse versus overwrite
  
  if(all("from" %in% names(x))) message("column name from will overwritten from?")
  
  class(x) <- c("snapshot-tbl", class(x))
  attr(x, "sandy-attr") <- attr_ns
  x
}

snapshot |> 
  # select(-weight) |> 
  as_snapshot(id,state)
