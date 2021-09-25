# ReadME

This repo contains all code written for the `The Analysis of Serve Decisions in Tennis using Bayesian Hierarchical Models` paper.


Some starting notes: 
* The Bayesian models were applied only on Roland Garros tracking data. 
* The scraped tracking data (from RG's Court Vision feature) was saved in early December 2020. 
* For possible data privacy reasons, the scraped data is not included in this repo.

`modelling`
-----------
* `fit_all_stan_models.R`
  * Fit and save all Bayesian models of interest.
  * Compare model fits via WAIC (using LOO R package).

 * `get_expanded_model_results.R`
   * Look at posterior results for the expanded model (player-varying intercepts + common covariates). This includes all quantiles, posterior means and standard errors.

* `diagnose_player_varying_effects.R`
  * Fit all Bayesian models using approximation methods. These are used sparingly as prototypes.

* `/saved_models/`
  * `/stan_loglik/` include model results using STAN. Pointwise log-likelihoods are included for model fit diagnostics.
  * `/advi/` include model results using posterior approximation methods.

 * `/stan_files/`
   * Contain all .stan files.

 * `plot_player_varying_intercept.R`
   * Plot player-varying model intercepts (model 2).
   * `/compare_intercepts_probs/` contains intermediate data files produced to help produce player-varying intercept plots

* `apply_posterior_prediction.R`
  * Plot posterior prediction results of expanded model (i.e. the Roger Federer illustration).

 * `/plots/`
   * All model result plots.

`data`
--------------
* The tracking data is not included (July 31st, 2021).

`eda`
------

* `plot_atp_and_wta_heatmaps.R`
  * Plot 6 players (3 ATP & 3 WTA) intended serve direction heatmaps.

* `plot_atp_serve_heatmaps.R`
  * EDA script plotting serve direction of 6 ATP players across a bunch of match scenarios.

* `plot_wta_serve_heatmaps.R`
  * EDA script plotting serve direction of 6 WTA players across a bunch of match scenarios.

* `get_data_summary.R`
  * Collect summary metrics of the data. Eg: How many matches do we have? How many players? How many faults are long/wide/net?

* `point_importance_eda.R`
  * Federer serve direction plot on point importance

* `serve_impact_location.R`
  * Boxplots of lateral position vs serve direction

`src`
-----
* Contains some functions that we repeatedly use throughout the project


