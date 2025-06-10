
# *Simulations of DAnCES*  

Here, the pipeline of running multiple simulations and analysis for different aspects of our study subject is hosted.
Specifically:
- config_templates: a config template for each simulation/analysis set.
- generated_configs: where multiple configs are stored, to be used in different simulation sets. Each folder's name links to the config template used to produce them. Not in git but gets created with each experiment run.
- sim_logs: the log files from running simulation sets. Not in git but gets created with each experiment run.
- models: the exact executable of the model used to run each simulation set.
- run_scripts: contains scripts for each simulations set and analysis. Each script uses a config file as a template (from *config_templates* folder), fills in analysis specific parameters, creates a set of configs for all different parameter combinations (saved in *generated_configs* folder), runs the model (from the *models* directory) for each config with as many repetitions defined in the file, and then analyzes the simulated data to produce results (plots or data files, saved in the results folder in the main directory, in the tmp subfolder, in a subdirectory named after the analysis). 

Thus to reproduce a part of our results, one needs a config template, an executable of the model and an R script.

## Prerequisites
* Define enviornmental variable *dancesPATH* with the path to the full *dances* repo
* See prerequesites of submodules (R code rDaNCES and C++ model pigeons)

## Authors
* **Marina Papadopoulou** - For any problem email at: <m.papadopoulou.rug@gmail.com>
