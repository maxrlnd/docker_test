# Drought Decision Model: Simulation Version

## Model Workflow

All functions are run within `master.R` and call additional scripts from the `R` folder.

* In `master.R`:
  * **SETUP**
    * Get the **station gauge attributes**, and assign them to a sub-environment (`getStationGauge`).
      * **FILL THIS IN!!**
    * Get the **model constant variables** by calling `getConstantVars` (*should rename*). These include:
      * **FILL THIS IN!!**
  * **GENERATE MODEL INPUTS**
    * `generateRunParams` is a wrapper that calls the function `getSimVars` in `xx.R` to populate another environment `simvars` with model inputs. We will call this *n* times where *n* is the number of simulation runs.
      * Currently simulated variables:
        * **Starting Year**, from a uniform distribution.
        * **Acreage**, from a normal distribution with mean=3000 and sd=xx, currently not enabled (?)
        * **Productivity Factor**, from a uniform distribution
        * **Forage Potential**, default `FALSE`. Not really a simulated variable, but should be set to `TRUE` if using a custom location.
  * **PARALLELIZE SIMULATION RUNS**
    * This enables us to perform many model runs faster.
    * Load required packages for parallelization using `snowfall`.
      * These *should not* be loaded into the global environment because `snowfall` will not recognize them.
    * Perform the simulations:
      1. Save each model run's outputs to a list `parouts`
      2. Convert `parouts` to a data frame object for summary, plotting, etc.
    * Save simulation inputs/results:
      * Input data (`simruns` to `output/simulation_inputs_baseline.RData`)
      * All results (`parouts` to `simulation_results_baseline.RData`)
  * **SUMMARY,**
    * **FILL THIS IN!**
