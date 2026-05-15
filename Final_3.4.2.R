# Final 3.4.2 Group DL6
# Sensitivity and Connectivity Functions
# Updated to match Table 1, Table 3, and Table 4
# Graphs and key outputs are assigned to objects and printed at the end.

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(archr)
library(purrr)
library(stringr)

rm(list = ls())
cat("\014")

####################################################
# Architecture Calculation Function
####################################################

calculate_architecture <- function(airframe, payload, communication, propulsion, power) {
  
  ####################################################
  # M1: System Cost
  ####################################################
  
  airframe_cost <- c(40000, 75000, 120000)
  payload_costs <- c(25000, 10000, 75000, 5000, 150000)
  communication_costs <- c(8000, 3000, 80000)
  propulsion_costs <- c(150000, 20000, 250000, 75000)
  power_costs <- c(35000, 25000, 60000)
  
  sc <- airframe_cost[airframe] +
    sum(payload_costs[payload]) +
    sum(communication_costs[communication]) +
    propulsion_costs[propulsion] +
    power_costs[power]
  
  ####################################################
  # M2: Deployment Time
  ####################################################
  
  airframe_TTD <- c(0.5, 1.5, 1.25)
  payload_TTD <- c(0.5, 0.25, 0.75, 0.25, 1)
  communication_TTD <- c(0.25, 0.1, 0.75)
  propulsion_TTD <- c(1.5, 0.25, 2, 1)
  power_TTD <- c(0.75, 0.25, 0.75)
  
  payload_setup <- max(payload_TTD[payload])
  communication_setup <- max(communication_TTD[communication])
  electronics_setup <- max(payload_setup, communication_setup)
  
  dt <- airframe_TTD[airframe] +
    power_TTD[power] +
    propulsion_TTD[propulsion] +
    electronics_setup
  
  ####################################################
  # M3: Data Acquisition Success Rate
  ####################################################
  
  airframe_data <- c(0.95, 0.98, 0.96)
  payload_data <- c(0.97, 0.99, 0.94, 0.98, 0.93)
  communication_data <- c(0.98, 0.85, 0.96)
  propulsion_data <- c(0.94, 0.97, 0.82, 0.95)
  power_data <- c(0.98, 0.95, 0.90)
  
  pdest <- airframe_data[airframe] *
    propulsion_data[propulsion] *
    power_data[power]
  
  psensors <- 1 - prod(1 - payload_data[payload])
  ptransmit <- 1 - prod(1 - communication_data[communication])
  
  da <- pdest * psensors * ptransmit
  
  ####################################################
  # M4: Surveillance Quality
  ####################################################
  
  payload_surveillance <- c(3, 4, 5, 1.5, 4.5)
  airframe_stability <- c(0.9, 0.8, 0.65)
  communication_quality <- c(0.5, 0.9, 1.0)
  power_compatibility <- c(1, 1, 0.6)
  
  sq <- min(sum(payload_surveillance[payload]), 5) *
    airframe_stability[airframe] *
    max(communication_quality[communication]) *
    power_compatibility[power]
  
  ####################################################
  # M5: Endurance
  ####################################################
  
  airframe_drag <- c(1.6, 0.7, 1.1)
  payload_power_penalty <- c(1.05, 1.00, 1.15, 1.00, 1.25)
  propulsion_power <- c(300, 100, 1000, 220)
  power_energy <- c(1200, 500, 250)
  
  payload_penalty <- prod(payload_power_penalty[payload])
  
  es <- power_energy[power] /
    (propulsion_power[propulsion] *
       airframe_drag[airframe] *
       payload_penalty)
  
  ####################################################
  # M6: Detectability
  ####################################################
  
  payload_emissions <- c(1, 1, 3, 1, 5)
  communication_emissions <- c(3, 2, 4)
  airframe_detectability <- c(3, 2, 4)
  propulsion_acoustic <- c(5, 1, 5, 4)
  
  ss <- 0.35 * propulsion_acoustic[propulsion] +
    0.30 * airframe_detectability[airframe] +
    0.20 * max(payload_emissions[payload]) +
    0.15 * max(communication_emissions[communication])
  
  return(list(
    m1 = sc,
    m2 = dt,
    m3 = da,
    m4 = sq,
    m5 = es,
    m6 = ss
  ))
}

####################################################
# Enumerate Architecture Space
####################################################

PayloadSets <- unlist(
  lapply(1:5, function(k) combn(1:5, k, simplify = FALSE)),
  recursive = FALSE
)

CommunicationSets <- unlist(
  lapply(1:3, function(k) combn(1:3, k, simplify = FALSE)),
  recursive = FALSE
)

raw_data <- expand_grid(
  d1 = 1:3,
  d2 = seq_along(PayloadSets),
  d3 = seq_along(CommunicationSets),
  d4 = 1:4,
  d5 = 1:3
) %>%
  mutate(
    payload_list = PayloadSets[d2],
    communication_list = CommunicationSets[d3]
  )

data <- raw_data %>%
  filter(!(d5 == 3 & d4 != 2)) %>%
  rowwise() %>%
  mutate(
    output = list(
      calculate_architecture(
        airframe = d1,
        payload = payload_list,
        communication = communication_list,
        propulsion = d4,
        power = d5
      )
    )
  ) %>%
  ungroup() %>%
  unnest_wider(output) %>%
  mutate(
    payload_label = sapply(payload_list, paste, collapse = "-"),
    communication_label = sapply(communication_list, paste, collapse = "-")
  )

dimension_summary <- tibble(
  Dataset = c("Raw unconstrained architectures", "Feasible constrained architectures"),
  Rows = c(nrow(raw_data), nrow(data)),
  Expected_Rows = c(7812, 5859)
)

####################################################
# Sensitivity and Connectivity Helper Functions
####################################################

sensitivity <- function(data, decision_i, metric) {
  metric_means <- data %>%
    group_by(.data[[decision_i]]) %>%
    summarize(
      mean_metric = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pull(mean_metric)
  
  max(metric_means, na.rm = TRUE) - min(metric_means, na.rm = TRUE)
}

connectivity <- function(data, decision_i, decisions, metric) {
  other_decisions <- setdiff(decisions, decision_i)
  
  base_sensitivity <- sensitivity(
    data = data,
    decision_i = decision_i,
    metric = metric
  )
  
  connection_scores <- sapply(other_decisions, function(decision_j) {
    conditional_scores <- data %>%
      group_by(.data[[decision_j]]) %>%
      group_modify(~{
        tibble(
          conditional_sensitivity = sensitivity(
            data = .x,
            decision_i = decision_i,
            metric = metric
          )
        )
      }) %>%
      ungroup() %>%
      pull(conditional_sensitivity)
    
    mean(abs(conditional_scores - base_sensitivity), na.rm = TRUE)
  })
  
  sum(connection_scores, na.rm = TRUE)
}

####################################################
# Sensitivity and Connectivity Table
####################################################

decision_list <- c("d1", "d2", "d3", "d4", "d5")
metric_list <- c("m1", "m2", "m3", "m4", "m5", "m6")

points <- expand_grid(
  decision = decision_list,
  metric = metric_list
) %>%
  rowwise() %>%
  mutate(
    c = connectivity(
      data,
      decision_i = decision,
      decisions = decision_list,
      metric = metric
    ),
    s = sensitivity(
      data,
      decision_i = decision,
      metric = metric
    )
  ) %>%
  mutate(
    s = ifelse(is.nan(s), 0, s),
    c = ifelse(is.nan(c), 0, c)
  ) %>%
  ungroup()

####################################################
# Graph Objects
####################################################

sensitivity_connectivity_plot <- ggplot() +
  geom_point(
    data = points,
    mapping = aes(x = c, y = s, color = decision),
    size = 10
  ) +
  geom_text(
    data = points,
    mapping = aes(x = c, y = s, label = decision),
    color = "white"
  ) +
  facet_wrap(~metric, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Sensitivity and Connectivity by Decision and Metric",
    x = "Connectivity Score",
    y = "Sensitivity Score"
  )

plot_metric_sensitivity <- function(metric_id, metric_name) {
  metric_points <- points %>%
    filter(metric == metric_id)
  
  ggplot() +
    geom_point(
      data = metric_points,
      mapping = aes(x = c, y = s, color = decision),
      size = 10
    ) +
    geom_text(
      data = metric_points,
      mapping = aes(x = c, y = s, label = decision),
      color = "white"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = paste("Sensitivity and Connectivity:", metric_name),
      x = paste("Connectivity Score:", metric_name),
      y = paste("Sensitivity Score:", metric_name)
    )
}

plot_m1 <- plot_metric_sensitivity("m1", "System Cost")
plot_m2 <- plot_metric_sensitivity("m2", "Deployment Time")
plot_m3 <- plot_metric_sensitivity("m3", "Data Acquisition Success Rate")
plot_m4 <- plot_metric_sensitivity("m4", "Surveillance Quality")
plot_m5 <- plot_metric_sensitivity("m5", "Endurance")
plot_m6 <- plot_metric_sensitivity("m6", "Detectability")

####################################################
# Feature Association Metrics
####################################################

archsets <- data %>%
  mutate(
    is_good = (d1 == 1 & d5 < 3),
    F1 = (d1 == 1),                                      # Multirotor
    F2 = (str_detect(payload_label, "(^|-)2(-|$)")),     # EO camera included
    F3 = (str_detect(communication_label, "(^|-)1(-|$)")), # Radio included
    F4 = (d4 == 2 & d5 == 2),                            # Electric propulsion + large battery
    F5 = (d4 == 1),                                      # Gas turbine propulsion
    F6 = (d5 == 1)                                       # Generator power
  )

calc_metrics <- function(data, feature) {
  N <- nrow(data)
  G <- data$is_good
  F_feat <- data[[feature]]
  
  supp_F <- sum(F_feat) / N
  supp_GF <- sum(G & F_feat) / N
  supp_G <- sum(G) / N
  
  tibble(
    feature = feature,
    supp_F = supp_F,
    supp_GF = supp_GF,
    conf_FG = ifelse(supp_F > 0, supp_GF / supp_F, 0),
    conf_GF = ifelse(supp_G > 0, supp_GF / supp_G, 0),
    lift = ifelse(
      supp_F > 0 & supp_G > 0,
      supp_GF / (supp_F * supp_G),
      0
    )
  )
}

final_metrics <- map_df(
  paste0("F", 1:6),
  ~calc_metrics(archsets, .x)
)

####################################################
# Key Output Objects
####################################################

metric_summary <- data %>%
  summarize(
    Architecture_Count = n(),
    Min_System_Cost = min(m1),
    Max_System_Cost = max(m1),
    Min_Deployment_Time = min(m2),
    Max_Deployment_Time = max(m2),
    Min_Data_Success = min(m3),
    Max_Data_Success = max(m3),
    Min_Surveillance = min(m4),
    Max_Surveillance = max(m4),
    Min_Endurance = min(m5),
    Max_Endurance = max(m5),
    Min_Detectability = min(m6),
    Max_Detectability = max(m6)
  )

top_sensitivity_by_metric <- points %>%
  group_by(metric) %>%
  slice_max(order_by = s, n = 1, with_ties = FALSE) %>%
  ungroup()

top_connectivity_by_metric <- points %>%
  group_by(metric) %>%
  slice_max(order_by = c, n = 1, with_ties = FALSE) %>%
  ungroup()

####################################################
# Final Outputs
####################################################

# Architecture-count check.
# Confirms the unconstrained and constrained tradespace sizes.
dimension_summary

# Full evaluated architecture data.
# Contains all feasible architectures and metrics m1-m6.
data

# Summary range of each metric across the feasible tradespace.
metric_summary

# Sensitivity/connectivity table for every decision-metric pair.
points

# Highest-sensitivity decision for each metric.
top_sensitivity_by_metric

# Highest-connectivity decision for each metric.
top_connectivity_by_metric

# Feature association table.
# Includes support, joint support, confidence, and lift for F1-F6.
final_metrics

# Faceted sensitivity/connectivity plot for all metrics.
sensitivity_connectivity_plot

# Individual metric plots.
plot_m1
plot_m2
plot_m3
plot_m4
plot_m5
plot_m6
