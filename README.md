# CLSI-Simulation-study

This repository contains the R simulation framework developed for the study “Statistical Performance of the CLSI EP15-A3 Protocol for Precision and Trueness Verification: A Simulation Study.”

The code was developed to evaluate the performance of the CLSI EP15-A3 analytical method verification protocol through Monte Carlo simulations.

## Repository contents

- `Main.R` — Main script coordinating the simulations.
- `Data_generation.R — 	Generation of simulated measurements under predefined analytical conditions.
- `EP15_Evaluation_Functions.R` — Evaluation and summarisation of verification outcomes.
- `Scenarios/` — Scripts implementing the simulation scenarios and experimental designs described in the article.
- `Structural_validation.r  — Assessment of whether generated data reproduce their specified parameters..
- `EP15_implementation_Functions.r — Implementation of calculations derived from CLSI EP15-A3; **excluded from public distribution**.
- `Others.r - Additional supporting functions.

## Running the simulations

The code can be executed in R or RStudio.

1. Download or clone the repository.
2. Install the required R packages.
3. Set the working directory to the repository's root directory.
4. Run `Main.R`.

The complete simulation uses 2,000 Monte Carlo repetitions per condition. A reduced number can be specified in `Main.R` for testing purposes, as the full simulations may take several hours.

The scripts must be executed in the specified order to reproduce the reported results. Intermediate files required by subsequent simulations are generated automatically.

**Important**: The public repository is not independently executable end-to-end, because main.R references a restricted module that is not distributed. Full reproduction of the CLSI EP15-A3 verification analyses requires a separately obtained, lawfully usable implementation of those calculations and any additional dependencies specified in the scripts. The public files should not be interpreted as a complete, standalone implementation of the guideline.

## Copyright and distribution restrictions
EP15_implementation_Functions.r contains material implementing content from the CLSI EP15-A3 guideline. Because this material cannot be shared publicly without the relevant authorisation from the Clinical and Laboratory Standards Institute (CLSI), the file is deliberately **omitted** from this repository.

This repository is an independent research project. It is not an official CLSI implementation, and it does not reproduce or grant rights to redistribute the CLSI EP15-A3 guideline. For use of protected material, consult the CLSI and obtain the necessary permissions.

## Citation

*[Full reference and DOI of the associated article to be added upon publication.]*
