
#'@param ... expression to send to igraph::graph_from_literal
#'@param n_particles number particles to generate. Representation of object
#'@export
make_ordered_log = function(
    ...,
    n_particle = 1000
    ) {
  aggs = head(as.list(match.call())[-1], -1)
  state_schema = do.call(igraph::graph_from_literal, aggs)
  
  ## either sample terminals or sample intermediate terminals
  leaves = igraph::V(state_schema)[igraph::degree(state_schema, mode="out" ) == 0]
  paths = (state_schema |> igraph::shortest_paths(1, leaves))$vpath
  edge_to = sample(paths,n_particle,replace=T) |> imap_dfr(~tibble(id = .y, to = names(.x)))
  edge_list = edge_to |>  group_by(id) |> 
    mutate(.order = row_number(), from = lag(to, order_by =.order)) |> 
    select(id,from,to)
  
  ## try assign random years to log
  Year_23 = seq(ymd('2023-01-01'), ymd('2023-12-31'), by='1 day')
  n_per_obj = pull(count(edge_list, id),n)
  logtime = n_per_obj |> map_dfr(~tibble(logtime = sort(sample(Year_23, .x))))
  weight = n_per_obj |> map_dfr(~tibble(weight = rnorm(.x, mean = 10, sd = 2) ))
  log = bind_cols(edge_list, logtime, weight)
  return(log)
}

disrupt_log = function(log, prop = 005) {
  possible_status = unique(c(log$from,log$to))
  
  ## index each row
  log_ided = log |> 
    ungroup() |> 
    mutate(row_id = row_number()) |> 
    relocate(row_id)
  
  ## random slice 0.05 of the row 
  rows_to_change=log_ided |> 
    slice_sample(prop = prop) |> 
    select(row_id, to)
  
  ## replace the to value to random sampled value
  new_to_s = sample(possible_status, nrow(rows_to_change),replace=T)
  new_rows = rows_to_change |> 
    mutate(to = new_to_s, .disrupted = T)
  
  ## correct the from to something else
  noisy_log = log_ided |> 
    mutate(.disrupted = F) |> 
    rows_update(new_rows, by = "row_id") |> 
    group_by(id) |> 
    mutate(from = lag(to, order_by = logtime)) |> 
    ungroup() |> 
    select(-row_id)
  return(noisy_log)
}
### we can possiblely try one with schema just to see what difference it makes


