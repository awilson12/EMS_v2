main_file.R
-------------------------------------------------------
All senarios can be run from the file, "mail_file.R". This will create nested folders for saving outputs, including mean and sd concentrations in states over time, a list with dataframes including each simulation iteration, and a list with dataframes including input parameters per iteration.

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

Exact_BetaPoisson_Bootstrap.csv
----------------------------------------------------
Contains alpha and beta values for dose-response calculations.

iteration checks
-----------------------------
Iterations and timesteps were compared with the "iteration_check.R" and "exposure_model_iteration_check.R" files.
