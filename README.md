main_file.R
-------------------------------------------------------
All senarios can be run from the file, "mail_file.R". This will create nested folders for saving outputs, including mean and sd concentrations in states over time, a list with dataframes including each simulation iteration, and a list with dataframes including input parameters per iteration. From this file, the other below files will be run with the exception of the iteration check files.

The main file also runs sensitivity analyses related to the droplet scenario exploration and exploring a variety of mask efficacies and their associated risk reductions.

scenarios.R
--------------------------------------------------------
Each mask scenario is run and saved for creating some of the files to be saved.

defining_parameters.R
-----------------------------------------------------
Randomly sampling parameters to be used for determining transition rates, transitional probabilities, and for use in the exposure model.

defining_rates.R
-----------------------------------------------------
Where transitional rates are defined

defining_probabilities.R
-----------------------------------------------------
Where transitional probabilities are defined

expsure_model.R
----------------------------------------------------
Simulation loop and saving output

figures.R
--------------------------------------------------
Code for producing figures unrelated to sensitivity analysis

sensitivity analysis.R
--------------------------------------------------
Code for sensitivity analysis results and figure

Exact_BetaPoisson_Bootstrap.csv
----------------------------------------------------
Contains alpha and beta values for dose-response calculations.

iteration checks
-----------------------------
Iterations and timesteps were compared with the "iteration_check.R" and "exposure_model_iteration_check.R" files.

exposure_model_sensitivity.R, defining_parameters_part_2.R, & droplet_fraction_estimate.R
----------------------------------
Code for the droplet sensitivity analysis

exposure_model_sensitivity_2.R & defining_parameters_mask_exploration.R
------------------------------------
Code for mask efficacy exploration
