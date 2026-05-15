# Final 3.1 Group DL6
# Load data wrangling and viz packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
# Load our system architecture toolkit
library(archr)

rm(list = ls())
cat("\014")

############## Decision Definitions ##################
# n = number of options per choice, k = maximum number of total choices
# .did just provides the labeling for each decision for when all of the decisions are combined


d1 = enumerate_sf(n = c(3), .did = 1) #Standard Form - Airframe Selection
d2 = enumerate_ds(n = 5, k = 5, .did = 2) %>% # Downselection where 
  mutate(count = d2_1 + d2_2 + d2_3 + d2_4 + d2_5) %>% #between 1-5 choices
  filter(count >= 1) %>%
  select(-count)
d3 = enumerate_ds(n = 3, k = 3, .did = 3) %>% # Downselection - Communication
  mutate(count = d3_1 + d3_2 + d3_3) %>% #between 1-3 choices
  filter(count >= 1) %>%
  select(-count)
d4 = enumerate_sf(n = c(4), .did = 4) #Standard Form - Propulsion Selection
d5 = enumerate_sf(n = c(3), .did = 5) #Standard Form - Electrical Power Selection

# Viewing Only
d1
d2
d3
d4
d5

# Combining all of the decision definitions into a grid
raw_arch = expand_grid(d1, d2, d3, d4, d5)
raw_arch
# Constraint
# Create a conditional case for d5, where a selection in d4 results in a value for d5
# filters out the solutions in which d4 is NOT 1, then D5 will also NOT be 2
# aka, the only instance in which d5 is 2 is when d4 is 1

arch = raw_arch %>%
  filter(!(d5 == 2 & d4 != 1))

arch


# Test case ONLY to double check work. We can see that D5 is ONLY 2 when D4 is equal to 1.
test = arch %>% filter(
  d5 == 2
) %>%  glimpse()


# per homework request / example, outputting to csv
# arch %>% readr::write_csv("UAV_Architectures.csv")

dim(arch) # rows and columns of the output
dim(raw_arch) # Double check, constraint removed about 2000 options

#########################################################
#2.2
#########################################################
# demonstrative subset of the arch decisions
# Deterministic subset of feasible architectures

# Sort architectures by key architectural decisions
# airframe, propulsion, power
arch_sorted <- arch %>%
  arrange(d1, d4, d5)

# Select desired number of deterministic architectures
n_total <- nrow(arch_sorted)
n_det <- 50

# Generate evenly spaced row indices
idx <- round(seq(1, n_total, length.out = n_det))

# Extract deterministic subset
arch_det <- arch_sorted[idx, ]

# View result
arch_det
write_csv(arch_det, "UAV_Deterministic_Subset.csv")
# Check no major decision is over-represented
table(arch_det$d1)
table(arch_det$d4)
table(arch_det$d5)
# Result: mostly even aside from d7:2 
# solar is excluded from non-electric solutions - makes sense

##################################################################
# 2.3
##################################################################
#Get random sample of 100 different architectures
set.seed(1)
# can make it random and not reproducible by using:
# set.seed(sample(1:1000,1))

arch_rand <- arch %>%
  slice_sample(n = 100)

# output CSV of 100 random architectures
write_csv(arch_rand, "UAV_Random_Architectures.csv")

# Prove they are random
# Get count of each decision
table(arch_rand$d1)  # airframe
colSums(arch_rand %>% select(starts_with("d2_")))  # payload usage
colSums(arch_rand %>% select(starts_with("d3_")))  # comms usage
table(arch_rand$d4)  # propulsion
table(arch_rand$d5)  # power


#######################
# ranbdom outputs:
#######################


############
# plot random outputs

#
barplot(table(arch_rand$d1), main = "Airframe")
barplot(
  colSums(arch_rand %>% select(starts_with("d2_"))),
  main = "Payload Sensor Usage",
  ylab = "Count"
)
barplot(
  colSums(arch_rand %>% select(starts_with("d3_"))),
  main = "Communication Usage",
  ylab = "Count"
)
barplot(table(arch_rand$d4), main = "Propulsion")
barplot(table(arch_rand$d5), main = "Power")

# Prepare data
d1_df <- bind_rows(
  arch %>% mutate(Source = "Population"),
  arch_det %>% mutate(Source = "Deterministic"),
  arch_rand %>% mutate(Source = "Random")
) %>%
  count(Source, d1) %>%
  group_by(Source) %>%
  mutate(Percent = n / sum(n) * 100)

# Plot
d1_plots <- ggplot(d1_df, aes(x = factor(d1), y = Percent)) +
  geom_col(width = 0.7, fill = "blue") +
  geom_text(aes(label = sprintf("%.0f%%", Percent)),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~Source, nrow = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Comparison of Airframe Distribution (D1)",
    x = "Airframe Option",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )+ scale_x_discrete(
    labels = c("0" = "Multirotor",
               "1" = "Fixed Wing",
               "2" = "Single Rotor")
  )

# Prepare data
d4_df <- bind_rows(
  arch %>% mutate(Source = "Population"),
  arch_det %>% mutate(Source = "Deterministic"),
  arch_rand %>% mutate(Source = "Random")
) %>%
  count(Source, d4) %>%
  group_by(Source) %>%
  mutate(Percent = n / sum(n) * 100)

# Plot
d4_plots <- ggplot(d4_df, aes(x = factor(d4), y = Percent)) +
  geom_col(width = 0.7, fill = "blue") +
  geom_text(aes(label = sprintf("%.0f%%", Percent)),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~Source, nrow = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Comparison of Propulsion System (D4)",
    x = "Propulsion Option",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  scale_x_discrete(labels = c("0" = "Gas Turbine", 
                              "1" = "Electric", 
                              "2" = "Rocket",
                              "3" = "Piston engine"))

# Prepare data
d5_df <- bind_rows(
  arch %>% mutate(Source = "Population"),
  arch_det %>% mutate(Source = "Deterministic"),
  arch_rand %>% mutate(Source = "Random")
) %>%
  count(Source, d5) %>%
  group_by(Source) %>%
  mutate(Percent = n / sum(n) * 100)

# Plot
d5_plots <- ggplot(d5_df, aes(x = factor(d5), y = Percent)) +
  geom_col(width = 0.7, fill = "blue") +
  geom_text(aes(label = sprintf("%.0f%%", Percent)),
            vjust = -0.5, size = 3.5) +
  facet_wrap(~Source, nrow = 1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Comparison of Power Distribution (D5)",
    x = "Power Option",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  scale_x_discrete(labels = c("0" = "Generator", 
                              "1" = "Battery", 
                              "2" = "Solar"))

d1_plots
d4_plots
d5_plots
