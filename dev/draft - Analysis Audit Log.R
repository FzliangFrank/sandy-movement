

## Study Reconstruct Process Flow
## Study Event Time

## If object is created, then the first time being created should be logged as 
## creation status such as NEW instead of the first NULL status

library(tidyverse)
theme_set(theme_bw())
## okay I need to revise how a lot is faked
## actaully make sense to generate radom log to follow particular distribution
df = make_fakelog(1e3)
## has 100 object now
time_delta.log = function(df) {
  df |> 
    group_by(id) |> 
    mutate(last_time = lag(logtime, order_by = logtime)) |> 
    mutate(time_delta = logtime - last_time) |> 
    filter(!is.na(from)) |> 
    ungroup(id)
}
  
change_prequency = df |> 
  time_delta.log()

## radom sample 1:9 given mean of status change frequency loc:
loc = (1 + 9)/2
## n number frequency is random sampled from 365 days
## the mean of frequency should be normally distributed (because from same uniform distribution)
## the average days between status change is now this
avg_day_change = 365/loc

## Statistics Experiment =================================
## Study characteristic of sampled random frequency
plot_against_nrml = function(x) {
  n = length(x)
  norm_x = rnorm(n, mean(x), sd(x))
  ggplot() + 
    geom_density(data = data.frame(x),aes(x=x), color="blue",linewidth=1) + 
    geom_density(data = data.frame(norm_x), aes(x = norm_x), color="black",linetype = "dashed") + 
    theme(
      title = ggtext::element_markdown()
    ) + 
    ggtitle("<b>normal</b> against <b style='color:blue'>x</b>") 
}

plot_against_exp = function(x) {
  n = length(x)
  norm_x = rexp(n, mean(x))
  ggplot() + 
    geom_density(data = data.frame(x),aes(x=x), color="blue",linewidth=1) + 
    geom_density(data = data.frame(norm_x), aes(x = norm_x), color="black",linetype = "dashed") + 
    theme(
      title = ggtext::element_markdown()
    ) + 
    ggtitle("<b>exponential</b> against <b style='color:blue'>x</b>") 
}


## (1) Are uniform sample interval exponential or normal? 
n = 1e4
x = runif(n)
diff_x = (x - lag(x))[-1]
norm_x = rnorm(n, mean(delta), sd(delta))
plot_against_nrml(norm_x) + 
  ggtitle("<b>normal</b> against <b style='color:blue'>x: runif unsorted</b>") 

## (2) Are uniform sample interval exponential or normal? 
## this type with sorting
n = 1e3
x = sort(runif(n))
diff_x = (x - lag(x))[-1]
plot_against_nrml(diff_x) + 
  ggtitle("<b>normal</b> against <b style='color:blue'>x: runif sorted</b>") 

## (3) instead of runif just sample from a vector
n = 1e3
x = sort(sample(1:(n*10), n,replace = T))
diff_x = (x - lag(x))[-1]
plot_against_nrml(diff_x) +
  ggtitle("<b>normal</b> against <b style='color:blue'>x: sample from a fector</b>")

## now use the similar tool to inspect stochestic status log
change_prequency |> 
  pull(time_delta) |> 
  as.numeric() |> 
  plot_against_nrml()
## looks acceptable

## it is possible to try approximate distribution of different path
## and use distribution to re-sample /predict time_delta

## first let's create a graph
## Status graph may look like this is simply because the process is stochestic
library(ggraph)

change_prequency |> 
  mutate(total_n = n()) |> 
  group_by(from, to) |> 
  summarise(
    median_td = median(time_delta),
    sd = sd(time_delta),
    total_n = first(total_n),
    n = n()
  ) |>
  tidygraph::as_tbl_graph() |> 
  ggraph() +
  geom_node_label(aes(label = name)) + 
  geom_edge_bend(aes(width = n),arrow = arrow(type="closed"), alpha = 0.1) +
  ggtitle("Graph method")

## Then possible to create distribution for each edge type
change_prequency |> 
  mutate(label = paste(from ,"->",to)) |> 
  ggplot() + 
  facet_wrap(~label) + 
  geom_histogram(aes(x = time_delta)) + 
  ggtitle("Distribution of Time in Each Sample")

## The most random state is stochestic and full graph
summary.log = function(change_prequency) {
  change_prequency |>
    ungroup() |> 
    mutate(total_n = n()) |> 
    group_by(from) |> mutate(from_n = n()) |> ungroup() |> 
    group_by(from, to) |> 
    summarise(
      median_td = median(time_delta),
      sd = sd(time_delta),
      total_n = first(total_n),
      from_n = first(from_n),
      n = n()
    )
}
change_freq_smry=change_prequency |>
  summary.log()
## use grid method to study posterior distribution
freq_posterior.log = function(
      change_freq_smry
    , total_n = total_n
    , grid_rs = 500
  ) {
  grid_rs = 500
  # .shrink_ = (grid_rs/nrow(change_freq_smry) * 2)
  p_grid = (1:grid_rs/grid_rs)[1:(grid_rs)]
  freq_pst_grid = change_freq_smry |> 
    select(from,to,{{total_n}}, n) |> 
    cross_join(tibble(p=p_grid)) |> 
    mutate(
      likelyhood = dbinom(n, {{total_n}}, p)
    ) |> 
    group_by(from, to) |> 
    mutate(posterior = likelyhood / sum(likelyhood))
  return(freq_pst_grid)
}
p_grid = (1:grid_rs/grid_rs)[1:(grid_rs/.shrink_)]
freq_pst_grid = change_freq_smry |> 
  freq_posterior.log()

plot.freq_posterior = function(freq_pst_grid) {
  freq_pst_grid |> 
    mutate(label = paste(from , "->", to)) |> 
    filter(posterior != 0) |> 
    filter(!is.na(posterior)) |> 
    ggplot(aes(x = p, y= posterior)) + 
    geom_line(
      data=freq_pst_grid |> mutate(label1 = paste(from,to))
      , aes(x = p, y= posterior, group = label1),color="grey",alpha=0.4,linewidth=0.4
    ) +
    geom_line() +
    facet_wrap(~label,scales="free")
}
freq_pst_grid |> plot.freq_posterior() + 
  ggtitle("Complete Random Graph") + 
  scale_x_continuous(limits = c(0.025, 0.10))

### we can possiblely try one with schema just to see what difference it makes
library(igraph)
n_particle = 500

state_schema = igraph::graph_from_literal(1 -+ 2 -+ 3:4,4-+5)
## either sample terminals or sample intermediate terminals
leaves = V(state_schema)[degree(state_schema, mode="out" ) == 0]
paths = (state_schema |> shortest_paths(1, leaves))$vpath
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
log |> 
  time_delta.log() |> 
  summary.log() |> 
  freq_posterior.log() |> 
  plot.freq_posterior() +
  ggtitle("Natural Progressing Status Log: Posterial of Edge"
          , stringr::str_replace_all(stringr::str_wrap(
            width = 80
            , string = glue::glue(
                ""
              , "If <b>every object has passed through</b> all the status till the end"
              , ", the joined edges has higher probability to occur than branches."
              , "This is because of the topology of graph: "
              , "the closer the edge path is towards the destiny of graph lower probability"
            )), "\n", "<br>")
  ) + 
  theme(title = ggtext::element_markdown())

## 2 - 3 and 2 - 4 has the same probability occurrence this is because they are 
## not the same as the very 
## so you will need depend prior probability 

## New Posterior Based on From Status ==========================================
log |> 
  time_delta.log() |> 
  summary.log() |> 
  freq_posterior.log(from_n) |> 
  plot.freq_posterior() +
  ggtitle("Natural Progressing Status Log: Posterior of Possible Status"
          , stringr::str_replace_all(stringr::str_wrap(
            width = 80
            , string = glue::glue(
              ""
              , "If <b>every object has passed through</b> all the status till the end,"
              , "where there are branches, the status the will split; but "
              , " anytime there are only one path forward the certaintiny increase"
            )), "\n", "<br>")
  ) + 
  theme(title = ggtext::element_markdown())

## Reconstruct Graph From Random Status Log ------------------------------------
## Is this good enough for us to tell which path is true path? 
## Possibly yes, if the path is not happening, they would goes near the 0 probability
## Anywhere if there are certainty towards our model
## we expect probability density to be much thinner and more local;

## Does this model able to tell which path is real in a sensible way? ----------
## Yes we are comparing this process of each object moving through each status as 
## a random process of flipping a coin If the heads up, then move from 2 to 3, 
## if tills up, move from 2 -> 4. 
## Eventually we get a posterior tell us, 50% of the time it move 2->3 and 50%
## of the time 2->4

## So when do the status do not move at all?
## An interesting question to think about is perhaps
## This also creating the problem where status could stagnate or could other wise 
## not able to progress forward.





