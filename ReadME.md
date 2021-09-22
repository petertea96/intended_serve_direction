### ReadME

This repo contains all code written for the `The Analysis of Serve Decisions in Tennis using Bayesian Hierarchical Models` paper.


* The bayesian model were applied only on Roland Garros tracking data, however similar tracking data from the Australian Open is available too.

* The scraped tracking data (from RG's Court Vision feature) was saved in early December 2020. 

`modelling`
-----------
* Fit all Bayesian models of interest 
* Obtain Posterior predictions
* Plot model results (eg: compare player-varying intercepts)


`data`
--------------
* Processed tracking data from Roland Garros along with scraped data of player heights
* Unfortunately, the data is not included.

`eda`
------
* Plots for the EDA section
* Eg: Federer's point importance, serve speeds vs serve number, player-specific heatmaps

`src`
-----
* Contains some functions that I repeatedly use throughout the project


Court dimensions are labelled in metres. For convenience, here's a plot.

![alt text](tennis_court.jpg)

