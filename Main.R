# ---- Simulation setup ----
## ---- libraries ----
library(ggplot2)
library(lme4)
library(nlme)
library(openxlsx)
library(knitr)
library(kableExtra)
library(tidyr)
library(dplyr)

## ---- sources ----

source("EP15_Evaluation_Functions.r")
source("EP15_implementation_Functions.r")
source("Data_generation.r")
source("Structural_validation.r")
source("Others.r")

## ---- set number of simulations. ----

# For a quick test of the simulation code, set nSim <- 2.
# The full study uses 2,000 simulations per scenario.
# A full run may take several hours or days, depending on computer performance.
# Progress indicators are included in the simulation scripts to allow users
# to monitor execution during long-running simulations.

# nSim <- 2
nSim <- 2000

# ---- Simulations ----
# Some simulation scripts are retained in the execution sequence even though
# their results were not included in the article.
# These scripts affect the state of the random number generator and must
# be executed in the original order to reproduce the reported results.
# Omitting them may lead to slightly different simulation outcomes.


## ---- run single altered parameter scenarios ----
source("Scenarios/NC1_R.R") # Scenario 1 - Between-run imprecision increment
source("Scenarios/NC2_R.R") # Scenario 2 - Within-run imprecision increment

source("Scenarios/NC4_R.R") # Absolute bias analysis (not included in the published results).

source("Scenarios/NC5_R.R") # Scenario 3 - relative bias increment

# ---- Effect of the number of experimental days ----
Iwl = 0
source("Scenarios/NC6_R_byDay_a.R") # Not included in the published results.

source("Scenarios/NC6_R_byDay_b.R") # Effect of the number of experimental days on Swl estimates.

# ---- Evaluation of the CLSI EP05 protocol (40 runs, 2 replicates per run) ----
source("Scenarios/NC6_EP05.R") # Results not included in the article.

# ---- Additional simulations: varying imprecision levels ----
# Variation in relative bias with different levels of
# within-laboratory imprecision (Iwl).

source("Scenarios/NC5b_R.R")  # Iwl= 6% - only 525; Not included in the published results.
source("Scenarios/NC5c1_R.R") # Iwl= 1% - only 525; Not included in the published results.
source("Scenarios/NC5c2_R.R") # Iwl= 2% - only 525; Not included in the published results.
source("Scenarios/NC5c3_R.R") # Iwl= 3% - only 525; Not included in the published results.
source("Scenarios/NC5c4_R.R") # Iwl= 4% - only 525; Not included in the published results.
source("Scenarios/NC5c5_R.R") # Iwl= 5% - only 525; Not included in the published results.
source("Scenarios/NC5c6_R.R") # Iwl= 7% - only 525; Not included in the published results.

# Additional simulations with varying levels of
# repeatability imprecision (Ir).

source("Scenarios/NC6c1_R.R")  # Ir= 1% - only 525; Not included in the published results.
source("Scenarios/NC6c2_R.R")  # Ir= 2% - only 525; Not included in the published results.
source("Scenarios/NC6c3_R.R")  # Ir= 3% - only 525; Not included in the published results.
source("Scenarios/NC6c4_R.R")  # Ir= 4% - only 525; Not included in the published results.
source("Scenarios/NC6c5_R.R")  # Ir= 5% - only 525; Not included in the published results.
source("Scenarios/NC6c6_R.R")  # Ir= 6% - only 525; Not included in the published results.
source("Scenarios/NC6c7_R.R")  # Ir= 7% - only 525; Not included in the published results.
source("Scenarios/NC5c8_R.R")  # Ir= 8% - only 525; Not included in the published results.
source("Scenarios/NC5c9_R.R")  # Ir= 9% - only 525; Not included in the published results.

# ---- Simultaneous increase in imprecision and bias ----
reset_variables()

## ---- Scenario 4: Within-laboratory imprecision and relative bias ----
for (Iwl in 0:7) {
  
  cat("Simulation started | NC7_R  Iwl =", Iwl, "%\n")

  source("Scenarios/NC7_R.R", local = FALSE)
  
  cat("Simulation completed | Iwl =", Iwl, "%\n")
}

reset_variables()

## ---- Scenario 4: Additional within-laboratory imprecision levels ----
# The 8% and 9% levels are evaluated using the 525 and 535 designs.

for (Iwl in 8:9) {
  
  cat("Simulation started |NC7_R Iwl =", Iwl, "%\n")

  source("Scenarios/NC7 525_535_R.R", local = FALSE)
  
  cat("Simulation completed | Iwl =", Iwl, "%\n")
}

## ---- Scenario 5: Repeatability imprecision and relative bias ----

for (Ir in 0:7) {
  
  cat("Início da simulação | Ir =", Ir, "%\n")

  source("Scenarios/NC8_R.R", local = FALSE)
  
  cat("Simulation completed | Ir =", Ir, "%\n")
}

cat("\nRepeatability imprecision and bias simulations completed.\n")
print("END OF  All RUNs")

