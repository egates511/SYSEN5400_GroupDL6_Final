# Final 3.5 Group DL6
# UAV Architecture Optimization + Full-Factorial Comparison
# Updated to match final report Table 3, Table 4, and final enumeration code

library(archr)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(GGally)
library(GA)
library(rmoo)

rm(list = ls())
cat("\014")

#########################################################
# 3.1a: Chromosome Definition
#########################################################

# D1 Airframe:       3 alternatives  -> 2 bits
# D2 Payload set:   31 alternatives -> 5 bits
# D3 Communication: 7 alternatives  -> 3 bits
# D4 Propulsion:    4 alternatives  -> 2 bits
# D5 Power:         3 alternatives  -> 2 bits
# Total chromosome length = 14 bits

meta <- tibble(
  did = c("D1", "D2", "D3", "D4", "D5"),
  decision = c(
    "Airframe",
    "Payload",
    "Communication",
    "Propulsion",
    "Power"
  ),
  lower = c(1, 1, 1, 1, 1),
  upper = c(3, 31, 7, 4, 3),
  n_alts = c(3, 31, 7, 4, 3),
  n_bits = c(2, 5, 3, 2, 2)
)

total_bits <- sum(meta$n_bits)

#########################################################
# 3.1b: Bitstring Encoding / Decoding
#########################################################

as_bit_vector <- function(bits) {
  if (length(bits) == 1 && is.character(bits)) {
    return(as.integer(strsplit(bits, "")[[1]]))
  }
  return(as.integer(bits))
}

bit2int <- function(bits) {
  bits <- as_bit_vector(bits)
  sum(bits * 2^rev(seq_along(bits) - 1))
}

int_to_bits <- function(x, n_bits) {
  bits <- decimal2binary(x = x, length = n_bits)
  as.integer(bits)
}

#########################################################
# Architecture Sets
#########################################################

PayloadSets <- unlist(
  lapply(1:5, function(k) combn(1:5, k, simplify = FALSE)),
  recursive = FALSE
)

CommunicationSets <- unlist(
  lapply(1:3, function(k) combn(1:3, k, simplify = FALSE)),
  recursive = FALSE
)

bits_to_arch <- function(bits) {
  bits <- as_bit_vector(bits)
  
  if (length(bits) != total_bits) {
    stop("Chromosome must be exactly 14 bits long.")
  }
  
  d1 <- bit2int(bits[1:2]) %% 3 + 1
  d2 <- bit2int(bits[3:7]) %% 31 + 1
  d3 <- bit2int(bits[8:10]) %% 7 + 1
  d4 <- bit2int(bits[11:12]) %% 4 + 1
  d5 <- bit2int(bits[13:14]) %% 3 + 1
  
  list(
    Airframe = d1,
    PayloadIndex = d2,
    Payload = PayloadSets[[d2]],
    CommunicationIndex = d3,
    Communication = CommunicationSets[[d3]],
    Propulsion = d4,
    Power = d5
  )
}

arch_to_bits <- function(arch) {
  c(
    int_to_bits(arch$Airframe - 1, 2),
    int_to_bits(arch$PayloadIndex - 1, 5),
    int_to_bits(arch$CommunicationIndex - 1, 3),
    int_to_bits(arch$Propulsion - 1, 2),
    int_to_bits(arch$Power - 1, 2)
  )
}

repair_arch <- function(arch) {
  # Constraint: Solar power requires electric propulsion.
  if (arch$Power == 3 && arch$Propulsion != 2) {
    arch$Propulsion <- 2
  }
  return(arch)
}

#########################################################
# 3.3 Metric Evaluation Values
#########################################################

# D1 Airframe: 1 = Multirotor, 2 = Fixed Wing, 3 = Single Rotor
Airframe_Costs <- c(40000, 75000, 120000)
Airframe_TTD <- c(0.5, 1.5, 1.25)
P_Airframe <- c(0.95, 0.98, 0.96)
Airframe_Stability <- c(0.9, 0.8, 0.65)
Airframe_Drag <- c(1.6, 0.7, 1.1)
Airframe_Detectability <- c(3, 2, 4)

# D2 Payload: 1 = IR, 2 = EO, 3 = Lidar, 4 = Audio, 5 = Radar
Payload_Costs <- c(25000, 10000, 75000, 5000, 150000)
Payload_TTD <- c(0.5, 0.25, 0.75, 0.25, 1)
P_Payload <- c(0.97, 0.99, 0.94, 0.98, 0.93)
Payload_Surveillance <- c(3, 4, 5, 1.5, 4.5)
Payload_PowerPenalty <- c(1.05, 1.00, 1.15, 1.00, 1.25)
Payload_Emissions <- c(1, 1, 3, 1, 5)

# D3 Communication: 1 = Radio, 2 = Cellular, 3 = Satellite
Communication_Costs <- c(8000, 3000, 80000)
Communication_TTD <- c(0.25, 0.1, 0.75)
P_Communication <- c(0.98, 0.85, 0.96)
Communication_Quality <- c(0.5, 0.9, 1.0)
Communication_Emissions <- c(3, 2, 4)

# D4 Propulsion: 1 = Gas Turbine, 2 = Electric, 3 = Rocket, 4 = Piston
Propulsion_Costs <- c(150000, 20000, 250000, 75000)
Propulsion_TTD <- c(1.5, 0.25, 2, 1)
P_Propulsion <- c(0.94, 0.97, 0.82, 0.95)
Propulsion_Power <- c(300, 100, 1000, 220)
Propulsion_Acoustic <- c(5, 1, 5, 4)

# D5 Power: 1 = Generator, 2 = Large Battery, 3 = Solar
Power_Costs <- c(35000, 25000, 60000)
Power_TTD <- c(0.75, 0.25, 0.75)
P_Power <- c(0.98, 0.95, 0.90)
Power_Compatibility <- c(1, 1, 0.6)
Power_Energy <- c(1200, 500, 250)

#########################################################
# Evaluation Function
#########################################################

evaluate_arch <- function(arch) {
  Airframe <- arch$Airframe
  Payload <- arch$Payload
  Communication <- arch$Communication
  Propulsion <- arch$Propulsion
  Power <- arch$Power
  
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
  
  c(
    cost = System_Cost,
    deployment_time = Deployment_Time,
    success_rate = Data_Success_Rate,
    surveillance_quality = Surveillance_Quality,
    endurance = Endurance,
    detectability = Detectability
  )
}

#########################################################
# Normalization Bounds for Fitness Function
#########################################################

# Enumerate full feasible architecture space to estimate metric ranges
# Otherwise the fitness (and therefore the GA) will favor more significant values like cost (100,000s vs 1-5)
norm_raw_arch <- expand_grid(
  Airframe = 1:3,
  PayloadIndex = seq_along(PayloadSets),
  CommunicationIndex = seq_along(CommunicationSets),
  Propulsion = 1:4,
  Power = 1:3
) %>%
  mutate(
    Payload = PayloadSets[PayloadIndex],
    Communication = CommunicationSets[CommunicationIndex]
  ) %>%
  filter(!(Power == 3 & Propulsion != 2))

norm_metrics <- norm_raw_arch %>%
  rowwise() %>%
  mutate(
    metrics = list(
      evaluate_arch(
        list(
          Airframe = Airframe,
          PayloadIndex = PayloadIndex,
          Payload = Payload,
          CommunicationIndex = CommunicationIndex,
          Communication = Communication,
          Propulsion = Propulsion,
          Power = Power
        )
      )
    )
  ) %>%
  ungroup() %>%
  unnest_wider(metrics)

metric_bounds <- norm_metrics %>%
  summarize(
    min_cost = min(cost),
    max_cost = max(cost),
    min_deployment_time = min(deployment_time),
    max_deployment_time = max(deployment_time),
    min_success_rate = min(success_rate),
    max_success_rate = max(success_rate),
    min_surveillance_quality = min(surveillance_quality),
    max_surveillance_quality = max(surveillance_quality),
    min_endurance = min(endurance),
    max_endurance = max(endurance),
    min_detectability = min(detectability),
    max_detectability = max(detectability)
  )

safe_norm <- function(x, min_x, max_x) {
  if (max_x == min_x) return(0)
  (x - min_x) / (max_x - min_x)
}

safe_norm_inverse <- function(x, min_x, max_x) {
  if (max_x == min_x) return(0)
  (max_x - x) / (max_x - min_x)
}

#########################################################
# Normalized Fitness Function for rmoo
#########################################################

# rmoo rewards larger values, so every objective is scaled so larger = better.
fitness <- function(x, nobj = 6, ...) {
  bits <- as_bit_vector(x)
  arch <- bits_to_arch(bits)
  arch <- repair_arch(arch)
  metrics <- evaluate_arch(arch)
  
  b <- metric_bounds
  
  norm_cost <- safe_norm_inverse(
    metrics["cost"],
    b$min_cost,
    b$max_cost
  )
  
  norm_deployment_time <- safe_norm_inverse(
    metrics["deployment_time"],
    b$min_deployment_time,
    b$max_deployment_time
  )
  
  norm_success_rate <- safe_norm(
    metrics["success_rate"],
    b$min_success_rate,
    b$max_success_rate
  )
  
  norm_surveillance_quality <- safe_norm(
    metrics["surveillance_quality"],
    b$min_surveillance_quality,
    b$max_surveillance_quality
  )
  
  norm_endurance <- safe_norm(
    metrics["endurance"],
    b$min_endurance,
    b$max_endurance
  )
  
  norm_detectability <- safe_norm_inverse(
    metrics["detectability"],
    b$min_detectability,
    b$max_detectability
  )
  
  matrix(
    c(
      norm_cost,
      norm_deployment_time,
      norm_success_rate,
      norm_surveillance_quality,
      norm_endurance,
      norm_detectability
    ),
    nrow = 1,
    ncol = 6
  )
}

#########################################################
# 3.1c: Repair Function
#########################################################

repair_bits <- function(bits) {
  bits <- as_bit_vector(bits)
  
  if (length(bits) != total_bits) {
    stop("Chromosome must be exactly 14 bits long.")
  }
  
  arch <- bits_to_arch(bits)
  arch <- repair_arch(arch)
  arch_to_bits(arch)
}

#########################################################
# Reference Test
#########################################################

# Example bitstring of length 14
test_bits <- "01101001000000"

arch_raw <- bits_to_arch(test_bits)
arch_repaired <- repair_arch(arch_raw)
bits_repaired <- repair_bits(test_bits)

metrics <- evaluate_arch(arch_repaired)
fit <- fitness(test_bits)

arch_raw
arch_repaired
bits_repaired
metrics
fit

#########################################################
# 3.1d: Run Genetic Algorithm
#########################################################

set.seed(5400)

ref <- generate_reference_points(m = 6, h = 4)

o <- rmoo(
  fitness = fitness,
  type = "binary",
  algorithm = "NSGA-III",
  lower = rep(0, total_bits),
  upper = rep(1, total_bits),
  monitor = TRUE,
  summary = TRUE,
  nObj = 6,
  nBits = total_bits,
  popSize = 80,
  maxiter = 100
  # reference_dirs = ref
)

o
summary(o)
o@solution

#########################################################
# Decode GA Solutions
#########################################################

solutions <- o@solution

decoded <- lapply(1:nrow(solutions), function(i) {
  bits <- solutions[i, ]
  
  arch <- bits_to_arch(bits)
  arch <- repair_arch(arch)
  metrics <- evaluate_arch(arch)
  
  data.frame(
    Airframe = arch$Airframe,
    PayloadIndex = arch$PayloadIndex,
    Payload = paste(arch$Payload, collapse = "-"),
    CommunicationIndex = arch$CommunicationIndex,
    Communication = paste(arch$Communication, collapse = "-"),
    Propulsion = arch$Propulsion,
    Power = arch$Power,
    cost = metrics["cost"],
    deployment_time = metrics["deployment_time"],
    success_rate = metrics["success_rate"],
    surveillance_quality = metrics["surveillance_quality"],
    endurance = metrics["endurance"],
    detectability = metrics["detectability"]
  )
}) %>%
  bind_rows() %>%
  distinct()

decoded

#########################################################
# Full-Factorial Enumeration Matching Final Code
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

full_arch_raw <- raw_arch %>%
  filter(!(Power == 3 & Propulsion != 2))

full_arch <- full_arch_raw %>%
  rowwise() %>%
  mutate(
    metrics = list(
      evaluate_arch(
        list(
          Airframe = Airframe,
          PayloadIndex = PayloadIndex,
          Payload = Payload,
          CommunicationIndex = CommunicationIndex,
          Communication = Communication,
          Propulsion = Propulsion,
          Power = Power
        )
      )
    )
  ) %>%
  ungroup() %>%
  unnest_wider(metrics) %>%
  mutate(
    Payload = sapply(Payload, paste, collapse = "-"),
    Communication = sapply(Communication, paste, collapse = "-")
  ) %>%
  distinct()

dim(raw_arch)        # Expected: 7812 rows
dim(full_arch_raw)   # Expected: 5859 rows
dim(full_arch)

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
  
  rank
}

#########################################################
# GA Pareto Front
#########################################################

ga_obj_matrix <- decoded %>%
  transmute(
    cost = cost,
    deployment_time = deployment_time,
    neg_success_rate = -success_rate,
    neg_surveillance_quality = -surveillance_quality,
    neg_endurance = -endurance,
    detectability = detectability
  ) %>%
  as.matrix()

decoded$pareto_rank <- pareto_rank_manual(ga_obj_matrix)

ga_pareto <- decoded %>%
  filter(pareto_rank == 1)

ga_pareto

#########################################################
# Full-Factorial Pareto Front
#########################################################

full_obj_matrix <- full_arch %>%
  transmute(
    cost = cost,
    deployment_time = deployment_time,
    neg_success_rate = -success_rate,
    neg_surveillance_quality = -surveillance_quality,
    neg_endurance = -endurance,
    detectability = detectability
  ) %>%
  as.matrix()

full_arch$pareto_rank <- pareto_rank_manual(full_obj_matrix)

full_pareto <- full_arch %>%
  filter(pareto_rank == 1)

full_pareto

#########################################################
# Composition Comparison
#########################################################

make_key <- function(df) {
  paste(
    df$Airframe,
    df$PayloadIndex,
    df$CommunicationIndex,
    df$Propulsion,
    df$Power,
    sep = "-"
  )
}

ga_pareto$key <- make_key(ga_pareto)
full_pareto$key <- make_key(full_pareto)

common_keys <- intersect(ga_pareto$key, full_pareto$key)

comparison_summary <- tibble(
  GA_Pareto_Count = nrow(ga_pareto),
  Full_Factorial_Pareto_Count = nrow(full_pareto),
  Common_Architectures = length(common_keys),
  Percent_GA_Pareto_Found_In_Full_Pareto =
    100 * length(common_keys) / nrow(ga_pareto),
  Percent_Full_Pareto_Recovered_By_GA =
    100 * length(common_keys) / nrow(full_pareto)
)

comparison_summary

common_architectures <- ga_pareto %>%
  filter(key %in% common_keys)

common_architectures

#########################################################
# 6x6 Tradespace Matrix: Full Pareto vs GA Pareto
#########################################################

full_arch_plot <- full_arch %>%
  mutate(
    Plot_Set = case_when(
      pareto_rank == 1 ~ "Full Pareto Rank 1",
      TRUE ~ "All Other Architectures"
    )
  )

ga_pareto_plot <- ga_pareto %>%
  mutate(
    Plot_Set = "GA Pareto"
  )

matrix_plot_data <- bind_rows(
  full_arch_plot,
  ga_pareto_plot
) %>%
  mutate(
    Plot_Set = factor(
      Plot_Set,
      levels = c(
        "All Other Architectures",
        "Full Pareto Rank 1",
        "GA Pareto"
      )
    )
  ) %>%
  select(
    Plot_Set,
    cost,
    deployment_time,
    success_rate,
    surveillance_quality,
    endurance,
    detectability
  )

pareto_panel <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(
      data = data %>% filter(Plot_Set == "All Other Architectures"),
      aes(color = Plot_Set),
      alpha = 0.18,
      size = 0.5
    ) +
    geom_point(
      data = data %>% filter(Plot_Set == "Full Pareto Rank 1"),
      aes(color = Plot_Set),
      alpha = 0.9,
      size = 1.0
    ) +
    geom_point(
      data = data %>% filter(Plot_Set == "GA Pareto"),
      aes(color = Plot_Set),
      alpha = 1,
      size = 2.0
    )
}

metric_plots <- ggpairs(
  matrix_plot_data,
  columns = 2:7,
  mapping = aes(color = Plot_Set),
  upper = list(continuous = pareto_panel),
  lower = list(continuous = pareto_panel),
  diag = list(continuous = wrap("densityDiag", alpha = 0.3)),
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
      "All Other Architectures" = "gray75",
      "Full Pareto Rank 1" = "purple",
      "GA Pareto" = "orange"
    )
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6),
    legend.position = "bottom"
  )

metric_plots

#########################################################
# Hypervolume Proxy
#########################################################

add_hv_score <- function(df, reference_df) {
  df %>%
    mutate(
      cost_score = (max(reference_df$cost) - cost) /
        (max(reference_df$cost) - min(reference_df$cost)),
      time_score = (max(reference_df$deployment_time) - deployment_time) /
        (max(reference_df$deployment_time) - min(reference_df$deployment_time)),
      success_score = (success_rate - min(reference_df$success_rate)) /
        (max(reference_df$success_rate) - min(reference_df$success_rate)),
      surveillance_score = (surveillance_quality - min(reference_df$surveillance_quality)) /
        (max(reference_df$surveillance_quality) - min(reference_df$surveillance_quality)),
      endurance_score = (endurance - min(reference_df$endurance)) /
        (max(reference_df$endurance) - min(reference_df$endurance)),
      detectability_score = (max(reference_df$detectability) - detectability) /
        (max(reference_df$detectability) - min(reference_df$detectability)),
      hv_proxy = cost_score * time_score * success_score *
        surveillance_score * endurance_score * detectability_score
    )
}

ga_hv <- add_hv_score(ga_pareto, full_arch)
full_hv <- add_hv_score(full_pareto, full_arch)

hv_summary <- tibble(
  Set = c("GA Pareto", "Full-Factorial Pareto"),
  Hypervolume_Proxy = c(sum(ga_hv$hv_proxy), sum(full_hv$hv_proxy)),
  Mean_HV_Contribution = c(mean(ga_hv$hv_proxy), mean(full_hv$hv_proxy))
)

hv_summary

# Final values:

# Summary of the rmoo optimization result.
# Useful for quickly checking convergence behavior and final objective performance.
summary(o)

# Final solution chromosomes returned by the GA.
# Each row is a binary chromosome that must be decoded into architecture decisions.
o@solution

# Decoded GA solutions.
# Converts the binary chromosomes into architecture decisions and evaluates all six metrics.
decoded

# Pareto Rank 1 solutions within the GA output.
# These are the nondominated architectures found by the genetic algorithm.
ga_pareto

# Pareto Rank 1 solutions from the full-factorial enumeration.
# This is the nondominated set from the complete feasible tradespace and should be treated as the ground-truth Pareto front.
full_pareto

# Comparison between the GA Pareto front and the full-factorial Pareto front.
# Shows how many GA Pareto architectures overlap with the full-factorial Pareto set.
comparison_summary

# Architectures that appear in both the GA Pareto front and the full-factorial Pareto front.
# These are GA-discovered solutions that are also globally nondominated in the full enumeration.
common_architectures

# 6x6 tradespace scatterplot matrix.
# Shows relationships among all six metrics, with full-factorial Pareto architectures and GA Pareto architectures highlighted.
metric_plots

# Summary table of hypervolume of GA vs Pareto
hv_summary
