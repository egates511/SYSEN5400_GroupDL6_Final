# Final 3.4 Group DL6
# SYSEN 5400
# Group DL6
# UAV Architecture Enumeration + Evaluation
# Updated to match final report Section 3

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(GGally)

rm(list = ls())
cat("\014")

#########################################################
# 3.1 / 3.2 Architecture Enumeration
#########################################################

# Decisions:
# D1 Airframe
#   1 = Multirotor
#   2 = Fixed Wing
#   3 = Single Rotor
#
# D2 Payload
#   1 = Infrared Camera
#   2 = EO Camera
#   3 = Lidar
#   4 = Audio Sensor
#   5 = Radar
#
# D3 Communication
#   1 = Radio
#   2 = Cellular
#   3 = Satellite
#
# D4 Propulsion
#   1 = Gas Turbine
#   2 = Electric
#   3 = Rocket-Propelled
#   4 = Piston Engine
#
# D5 Electrical Power
#   1 = Generator
#   2 = Large Battery
#   3 = Solar
#
# Constraint:
#   If Power = Solar, then Propulsion = Electric

#########################################################
# Generate down-selected payload sets
#########################################################

PayloadSets <- unlist(
  lapply(1:5, function(k) combn(1:5, k, simplify = FALSE)),
  recursive = FALSE
)

#########################################################
# Generate down-selected communication sets
#########################################################

CommunicationSets <- unlist(
  lapply(1:3, function(k) combn(1:3, k, simplify = FALSE)),
  recursive = FALSE
)

#########################################################
# Full factorial architecture enumeration
#########################################################

raw_arch <- expand_grid(
  Airframe = 1:3,
  PayloadIndex = seq_along(PayloadSets),
  CommunicationIndex = seq_along(CommunicationSets),
  Propulsion = 1:4,
  Power = 1:3
) %>%
  mutate(
    Payload = PayloadSets[PayloadIndex],
    Communication = CommunicationSets[CommunicationIndex]
  )

# Apply constraint: Solar power requires electric propulsion
arch <- raw_arch %>%
  filter(!(Power == 3 & Propulsion != 2))

dim(raw_arch)
dim(arch)

#########################################################
# 3.3 Metric Evaluation Values
#########################################################

# D1 Airframe
Airframe_Costs <- c(40000, 75000, 120000)
Airframe_TTD <- c(0.5, 1.5, 1.25)
P_Airframe <- c(0.95, 0.98, 0.96)
Airframe_Stability <- c(0.9, 0.8, 0.65)
Airframe_Drag <- c(1.6, 0.7, 1.1)
Airframe_Detectability <- c(3, 2, 4)

# D2 Payload
Payload_Costs <- c(25000, 10000, 75000, 5000, 150000)
Payload_TTD <- c(0.5, 0.25, 0.75, 0.25, 1)
P_Payload <- c(0.97, 0.99, 0.94, 0.98, 0.93)
Payload_Surveillance <- c(3, 4, 5, 1.5, 4.5)
Payload_PowerPenalty <- c(1.05, 1.00, 1.15, 1.00, 1.25)
Payload_Emissions <- c(1, 1, 3, 1, 5)

# D3 Communication
Communication_Costs <- c(8000, 3000, 80000)
Communication_TTD <- c(0.25, 0.1, 0.75)
P_Communication <- c(0.98, 0.85, 0.96)
Communication_Quality <- c(0.5, 0.9, 1.0)
Communication_Emissions <- c(3, 2, 4)

# D4 Propulsion
Propulsion_Costs <- c(150000, 20000, 250000, 75000)
Propulsion_TTD <- c(1.5, 0.25, 2, 1)
P_Propulsion <- c(0.94, 0.97, 0.82, 0.95)
Propulsion_Power <- c(300, 100, 1000, 220)
Propulsion_Acoustic <- c(5, 1, 5, 4)

# D5 Electrical Power
Power_Costs <- c(35000, 25000, 60000)
Power_TTD <- c(0.75, 0.25, 0.75)
P_Power <- c(0.98, 0.95, 0.90)
Power_Compatibility <- c(1, 1, 0.6)
Power_Energy <- c(1200, 500, 250)

#########################################################
# Evaluation Function
#########################################################

evaluate_architecture <- function(Airframe, Payload, Communication, Propulsion, Power) {
  
  ############################
  # M1: System Cost
  ############################
  
  System_Cost <-
    Airframe_Costs[Airframe] +
    sum(Payload_Costs[Payload]) +
    sum(Communication_Costs[Communication]) +
    Propulsion_Costs[Propulsion] +
    Power_Costs[Power]
  
  ############################
  # M2: Deployment Time
  ############################
  
  Payload_Setup_Time <- max(Payload_TTD[Payload])
  Communication_Setup_Time <- max(Communication_TTD[Communication])
  Electronics_Setup_Time <- max(Payload_Setup_Time, Communication_Setup_Time)
  
  Deployment_Time <-
    Airframe_TTD[Airframe] +
    Power_TTD[Power] +
    Propulsion_TTD[Propulsion] +
    Electronics_Setup_Time
  
  ############################
  # M3: Data Acquisition Success Rate
  ############################
  
  P_Destination <-
    P_Airframe[Airframe] *
    P_Propulsion[Propulsion] *
    P_Power[Power]
  
  P_Sensors <- 1 - prod(1 - P_Payload[Payload])
  P_Transmit <- 1 - prod(1 - P_Communication[Communication])
  
  Data_Success_Rate <- P_Destination * P_Sensors * P_Transmit
  
  ############################
  # M4: Surveillance Quality
  ############################
  
  Surveillance_Quality <-
    min(sum(Payload_Surveillance[Payload]), 5) *
    Airframe_Stability[Airframe] *
    max(Communication_Quality[Communication]) *
    Power_Compatibility[Power]
  
  ############################
  # M5: Endurance
  ############################
  
  Payload_Penalty <- prod(Payload_PowerPenalty[Payload])
  
  Endurance <-
    Power_Energy[Power] /
    (
      Propulsion_Power[Propulsion] *
        Airframe_Drag[Airframe] *
        Payload_Penalty
    )
  
  ############################
  # M6: Detectability
  ############################
  
  Detectability <-
    0.35 * Propulsion_Acoustic[Propulsion] +
    0.30 * Airframe_Detectability[Airframe] +
    0.20 * max(Payload_Emissions[Payload]) +
    0.15 * max(Communication_Emissions[Communication])
  
  return(
    tibble(
      System_Cost = System_Cost,
      Deployment_Time = Deployment_Time,
      Data_Success_Rate = Data_Success_Rate,
      Surveillance_Quality = Surveillance_Quality,
      Endurance = Endurance,
      Detectability = Detectability
    )
  )
}

#########################################################
# Evaluate Full Architecture Space
#########################################################

arch_eval <- arch %>%
  rowwise() %>%
  mutate(
    metrics = list(
      evaluate_architecture(
        Airframe = Airframe,
        Payload = Payload,
        Communication = Communication,
        Propulsion = Propulsion,
        Power = Power
      )
    )
  ) %>%
  ungroup() %>%
  unnest(metrics) %>%
  mutate(
    Payload_Label = sapply(Payload, paste, collapse = "-"),
    Communication_Label = sapply(Communication, paste, collapse = "-"),
    Airframe_Label = factor(
      Airframe,
      levels = c(1, 2, 3),
      labels = c("Multirotor", "Fixed Wing", "Single Rotor")
    ),
    Propulsion_Label = factor(
      Propulsion,
      levels = c(1, 2, 3, 4),
      labels = c("Gas Turbine", "Electric", "Rocket", "Piston")
    ),
    Power_Label = factor(
      Power,
      levels = c(1, 2, 3),
      labels = c("Generator", "Large Battery", "Solar")
    )
  )

arch_eval

#write_csv(arch_eval, "UAV_Architectures_Evaluated.csv")

#########################################################
# Reference Architecture Check
#########################################################

# Reference architecture:
# Airframe: Fixed-wing (2)
# Payload: Infrared camera (1) + Audio sensor (4)
# Communication: Radio (1)
# Propulsion: Piston engine (4)
# Power: Large battery (2)

reference_arch <- evaluate_architecture(
  Airframe = 2,
  Payload = c(1, 4),
  Communication = c(1),
  Propulsion = 4,
  Power = 2
)

reference_arch

#########################################################
# Deterministic Subset
#########################################################

arch_sorted <- arch_eval %>%
  arrange(Airframe, CommunicationIndex, Propulsion, Power)

n_total <- nrow(arch_sorted)
n_det <- 50
idx <- round(seq(1, n_total, length.out = n_det))

arch_det <- arch_sorted[idx, ]

#write_csv(arch_det, "UAV_Deterministic_Subset.csv")

#########################################################
# Random Subset
#########################################################

set.seed(1)

arch_rand <- arch_eval %>%
  slice_sample(n = 100)

#write_csv(arch_rand, "UAV_Random_Architectures.csv")

#########################################################
# Distribution Checks
#########################################################

table(arch_eval$Airframe)
table(arch_eval$Propulsion)
table(arch_eval$Power)

table(arch_rand$Airframe)
table(arch_rand$Propulsion)
table(arch_rand$Power)

#########################################################
# Pareto Ranking Function
#########################################################

pareto_rank_manual <- function(mat) {
  n <- nrow(mat)
  rank <- rep(1, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        if (all(mat[j, ] <= mat[i, ]) && any(mat[j, ] < mat[i, ])) {
          rank[i] <- rank[i] + 1
        }
      }
    }
  }
  
  return(rank)
}

#########################################################
# Compute Pareto Rank
#########################################################

# Convert all objectives to minimization form:
# Minimize cost
# Minimize deployment time
# Maximize data success -> minimize negative success
# Maximize surveillance -> minimize negative surveillance
# Maximize endurance -> minimize negative endurance
# Minimize detectability

obj_matrix <- arch_eval %>%
  transmute(
    System_Cost = System_Cost,
    Deployment_Time = Deployment_Time,
    Negative_Data_Success_Rate = -Data_Success_Rate,
    Negative_Surveillance_Quality = -Surveillance_Quality,
    Negative_Endurance = -Endurance,
    Detectability = Detectability
  ) %>%
  as.matrix()

arch_eval$Pareto_Rank <- pareto_rank_manual(obj_matrix)

pareto_front <- arch_eval %>%
  filter(Pareto_Rank == 1)

#write_csv(pareto_front, "UAV_Pareto_Front.csv")

pareto_front

#########################################################
# 6x6 Tradespace Scatterplot Matrix
# Pareto Front = Purple, Other Architectures = Gray
#########################################################

plot_data <- arch_eval %>%
  mutate(
    Pareto_Class = ifelse(Pareto_Rank == 1, "Pareto Front", "Architecture"),
    Pareto_Class = factor(
      Pareto_Class,
      levels = c("Architecture", "Pareto Front")
    )
  ) %>%
  select(
    Pareto_Class,
    System_Cost,
    Deployment_Time,
    Data_Success_Rate,
    Surveillance_Quality,
    Endurance,
    Detectability
  )

PlotMatrix_Pareto <- ggpairs(
  plot_data,
  columns = 2:7,
  aes(color = Pareto_Class, fill = Pareto_Class),
  upper = list(continuous = wrap("points", size = 0.6, alpha = 0.35)),
  lower = list(continuous = wrap("points", size = 0.6, alpha = 0.35)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
  columnLabels = c(
    "System Cost",
    "Deployment Time",
    "Success Rate",
    "Surveillance Quality",
    "Endurance",
    "Detectability"
  )
) +
  scale_color_manual(
    values = c(
      "Architecture" = "gray75",
      "Pareto Front" = "purple"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Architecture" = "gray75",
      "Pareto Front" = "purple"
    )
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "bottom"
  )

#########################################################
# Optional: Airframe-Colored 6x6 Matrix
#########################################################

PlotMatrix_Airframe <- ggpairs(
  arch_eval %>%
    select(
      Airframe_Label,
      System_Cost,
      Deployment_Time,
      Data_Success_Rate,
      Surveillance_Quality,
      Endurance,
      Detectability
    ),
  columns = 2:7,
  aes(color = Airframe_Label, alpha = 0.6),
  upper = list(continuous = wrap("points", size = 0.6)),
  lower = list(continuous = wrap("points", size = 0.6)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)),
  columnLabels = c(
    "System Cost",
    "Deployment Time",
    "Success Rate",
    "Surveillance Quality",
    "Endurance",
    "Detectability"
  )
) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "bottom"
  )

#########################################################
# Summary Statistics
#########################################################

summary_stats <- arch_eval %>%
  summarize(
    Architecture_Count = n(),
    Pareto_Count = sum(Pareto_Rank == 1),
    Min_Cost = min(System_Cost),
    Max_Cost = max(System_Cost),
    Min_Deployment_Time = min(Deployment_Time),
    Max_Deployment_Time = max(Deployment_Time),
    Min_Success_Rate = min(Data_Success_Rate),
    Max_Success_Rate = max(Data_Success_Rate),
    Min_Surveillance = min(Surveillance_Quality),
    Max_Surveillance = max(Surveillance_Quality),
    Min_Endurance = min(Endurance),
    Max_Endurance = max(Endurance),
    Min_Detectability = min(Detectability),
    Max_Detectability = max(Detectability)
  )
#########################################################
# Separate System Cost vs. Other Metric Plots
#########################################################

plot_cost_vs_metric <- function(y_var, y_label, plot_title) {
  ggplot(
    plot_data,
    aes(
      x = System_Cost,
      y = .data[[y_var]],
      color = Pareto_Class
    )
  ) +
    geom_point(alpha = 0.45, size = 1.2) +
    scale_color_manual(
      values = c(
        "Architecture" = "gray75",
        "Pareto Front" = "purple"
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = plot_title,
      x = "System Cost ($)",
      y = y_label,
      color = "Architecture Class"
    )
}

Plot_Cost_vs_Deployment <- plot_cost_vs_metric(
  y_var = "Deployment_Time",
  y_label = "Deployment Time (hrs)",
  plot_title = "System Cost vs. Deployment Time"
)

Plot_Cost_vs_Success <- plot_cost_vs_metric(
  y_var = "Data_Success_Rate",
  y_label = "Data Success Rate",
  plot_title = "System Cost vs. Data Success Rate"
)

Plot_Cost_vs_Surveillance <- plot_cost_vs_metric(
  y_var = "Surveillance_Quality",
  y_label = "Surveillance Quality (1-5)",
  plot_title = "System Cost vs. Surveillance Quality"
)

Plot_Cost_vs_Endurance <- plot_cost_vs_metric(
  y_var = "Endurance",
  y_label = "Endurance (hrs)",
  plot_title = "System Cost vs. Endurance"
)

Plot_Cost_vs_Detectability <- plot_cost_vs_metric(
  y_var = "Detectability",
  y_label = "Detectability (1-5)",
  plot_title = "System Cost vs. Detectability"
)

#########################################################
# Final Outputs
#########################################################

# Architecture-count check.
# Confirms the unconstrained and constrained tradespace sizes.
dimension_summary <- tibble(
  Dataset = c(
    "Raw unconstrained architectures",
    "Feasible constrained architectures"
  ),
  Rows = c(nrow(raw_arch), nrow(arch)),
  Expected_Rows = c(7812, 5859)
)

# Distribution checks for major standard-form decisions.
airframe_distribution <- table(arch_eval$Airframe)
propulsion_distribution <- table(arch_eval$Propulsion)
power_distribution <- table(arch_eval$Power)

random_airframe_distribution <- table(arch_rand$Airframe)
random_propulsion_distribution <- table(arch_rand$Propulsion)
random_power_distribution <- table(arch_rand$Power)

# Full evaluated tradespace.
arch_eval

# Architecture count summary.
dimension_summary

# Reference architecture from Table 3.
reference_arch

# Summary statistics for all six metrics.
summary_stats

# Full Rank-1 Pareto front.
pareto_front

# Distribution outputs.
airframe_distribution
propulsion_distribution
power_distribution

random_airframe_distribution
random_propulsion_distribution
random_power_distribution

# 6x6 tradespace matrix colored by Pareto membership.
#PlotMatrix_Pareto

# Optional 6x6 tradespace matrix colored by airframe.
#PlotMatrix_Airframe

# Separate system cost tradeoff plots.
Plot_Cost_vs_Deployment
Plot_Cost_vs_Success
Plot_Cost_vs_Surveillance
Plot_Cost_vs_Endurance
Plot_Cost_vs_Detectability
