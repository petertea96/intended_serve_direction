### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==    SCRIPT FITTING STAN BAYESIAN MODELS W/ RSTAN           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# What does this script do?
# -- Fit 4 Bayesian models on both ATP and WTA data
# -- Save model fits in .RDS files
# -- Compare model fits using LOO R package
# -- > Note: WAIC = -2*LPPD

setwd("/Users/petertea/Documents/intended_serve_direction")

library(dplyr)
library(rstan)
library(loo)
options(tibble.print_max = 30)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====              Data Processing Steps                      =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
atp_data <- read.csv('./data/atp_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE,
                     na.strings=c("","NA"))

atp_training_data <- atp_data %>%
  distinct() %>%
  mutate( 
    # -- Remember, the last level is the reference level in the STAN model
    y = ifelse(intended_serve_dir == 'Body', 3, 
               ifelse(intended_serve_dir == 'T', 1, 
                      ifelse(intended_serve_dir == 'Wide', 2, NA))),
    is_first_serve = ifelse(serve_num == 1, 1,0) ) %>%
  select(y, server_name, returner_name, 
         is_break_point, x_ball_at_serve,
         is_first_serve,
         court_side,
         is_prev_doublefault, is_prev_ace, 
         serve_impact_from_center,
         server_hand, returner_hand,
         point_importance, 
         z_scaled_point_importance,
         returner_backhand_loc,
         prev_intended_serve_loc1,
         prev_intended_serve_loc2,
         player_hands_match,
         z_ball_at_serve,
         player_hands_match
  ) %>%
  mutate(## --model.matrix() not cooperating with factors...I'll do this manually
    prev_intended_serve_loc1T = ifelse(prev_intended_serve_loc1 == 'T',1,0),
    prev_intended_serve_loc1Wide = ifelse(prev_intended_serve_loc1 == 'Wide',1,0),
    prev_intended_serve_loc2T = ifelse(prev_intended_serve_loc2 == 'T',1,0),
    prev_intended_serve_loc2Wide = ifelse(prev_intended_serve_loc2 == 'Wide',1,0),
    is_second_serve = ifelse(is_first_serve == 1,0,1),
    court_side_ad = ifelse(court_side == 'DeuceCourt', 0,1),
    returner_backhand_locT = ifelse(returner_backhand_loc == 'T', 1,0),
    server_handL = ifelse(server_hand == 'left-handed', 1,0),
    distance_inside_serve = 11.89 - abs(x_ball_at_serve),
    interaction_s_hand_court_side = server_handL * court_side_ad,
    interaction_ss_bh_T = is_second_serve*returner_backhand_locT
  ) %>%
  filter(complete.cases(.))

atp_training_data$server_index <- as.numeric(factor(atp_training_data$server_name))

# Prepare lists as inputs to STAN 
atp_common_intercept_datalist <- list(
  N = nrow(atp_training_data),
  y = atp_training_data$y,
  K = 3
)

atp_varying_intercept_datalist <- list(
  N = nrow(atp_training_data),
  num_players = length(unique(atp_training_data$server_index)),
  player_id = atp_training_data$server_index,
  y = atp_training_data$y,
  K = 3
)


atp_expanded_datalist <- list(
  N = nrow(atp_training_data),
  num_players = length(unique(atp_training_data$server_index)),
  y = atp_training_data$y,
  player_id = atp_training_data$server_index,
  x1 = atp_training_data$is_second_serve,
  x2 = atp_training_data$serve_impact_from_center,
  x3 = atp_training_data$distance_inside_serve,
  x4 = atp_training_data$prev_intended_serve_loc1T,
  x5 = atp_training_data$prev_intended_serve_loc1Wide,
  x6 = atp_training_data$returner_backhand_locT,
  x7 = atp_training_data$point_importance,
  x8 = atp_training_data$server_handL,
  x9 = atp_training_data$court_side_ad,
  x10 = atp_training_data$interaction_s_hand_court_side,
  x11 = atp_training_data$interaction_ss_bh_T,
  K = 3
)


atp_varying_effects_datalist <- list(
  N = nrow(atp_training_data),
  num_players = length(unique(atp_training_data$server_index)),
  y = atp_training_data$y,
  player_id = atp_training_data$server_index,
  x1 = atp_training_data$point_importance, # Only this covariate is player-varying
  x2 = atp_training_data$serve_impact_from_center,
  x3 = atp_training_data$distance_inside_serve,
  x4 = atp_training_data$prev_intended_serve_loc1T,
  x5 = atp_training_data$prev_intended_serve_loc1Wide,
  x6 = atp_training_data$returner_backhand_locT,
  x7 = atp_training_data$court_side_ad,
  x8 = atp_training_data$server_handL,
  x9 = atp_training_data$is_second_serve,
  x10 = atp_training_data$interaction_s_hand_court_side,
  x11 = atp_training_data$interaction_ss_bh_T,
  K = 3
)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  FIT STAN MODELS                        =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# -- Model 1: Common intercept
# -- Model 2: Player-varying intercept
# -- Model 3: Player-varying intercept + common covariates
# -- Model 4: Player-varying intercept and PI + common covariates

options(mc.cores = 4)
common_intercept <- stan_model('./modelling/stan_files/common_intercept.stan')
varying_intercept <- stan_model('./modelling/stan_files/player_varying_intercept.stan')
expanded <- stan_model('./modelling/stan_files/expanded_model.stan')
varying_effects <- stan_model('./modelling/stan_files/player_varying_intercept_and_covariate.stan')




# ~1 minute to run
atp_fit_common_intercept <- sampling(common_intercept, 
                                     data = atp_common_intercept_datalist, 
                                     iter = 2000)
saveRDS(atp_fit_common_intercept, 
        file = "./modelling/saved_models/stan_loglik/atp_common_intercept_fit.RDS")

#~10 minutes to run
atp_fit_varying_intercept <- sampling(varying_intercept, 
                                      data = atp_varying_intercept_datalist, 
                                      iter = 2000)
saveRDS(atp_fit_varying_intercept,
        file = "./modelling/saved_models/stan_loglik/atp_varying_intercept_fit.RDS")

# ~ 102 minutes
atp_fit_expanded <- sampling(expanded, 
                             data = atp_expanded_datalist, 
                             iter = 2000)
saveRDS(atp_fit_atp_model3, file = "./modelling/saved_models/stan_loglik/atp_expanded_fit.RDS")

# || --- || --- || --- || --- || --- || --- || --- || --- || 
# -- Player varying intercept and (one) slope -----
# || --- || --- || --- || --- || --- || --- || --- || --- || 
# ~ Many minutes
atp_fit_varying_effects <- sampling(varying_effects, 
                                    data = atp_varying_effects_datalist, 
                                    iter = 2000)
saveRDS(atp_fit_varying_effects, 
        file = "./modelling/saved_models/stan_loglik/atp_varying_effects_fit.RDS")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  COMPUTE ATP WAIC                       =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
atp_fit_common_intercept <- readRDS("./modelling/saved_models/stan_loglik/atp_common_intercept_fit.RDS")
atp_fit_varying_intercept <- readRDS("./modelling/saved_models/stan_loglik/atp_varying_intercept_fit.RDS")
atp_fit_expanded <- readRDS("./modelling/saved_models/stan_loglik/atp_expanded_fit.RDS")
atp_fit_varying_effects <- readRDS("./modelling/saved_models/stan_loglik/atp_varying_effects_fit.RDS")


log_lik_common_intercept <- extract_log_lik(atp_fit_common_intercept)
log_lik_varying_intercept <- extract_log_lik(atp_fit_varying_intercept)
log_lik_expanded <- extract_log_lik(atp_fit_expanded)
log_lik_varying_effects <- extract_log_lik(atp_fit_varying_effects)


waic_common_intercept <- waic(log_lik_common_intercept)
waic_varying_intercept <- waic(log_lik_varying_intercept)
waic_expanded <- waic(log_lik_expanded)
waic_varying_effects <- waic(log_lik_varying_effects)

print(waic_common_intercept)
print(waic_varying_intercept)
print(waic_expanded)
print(waic_varying_effects)

print(loo_compare(waic_common_intercept , 
                  waic_varying_intercept, 
                  waic_expanded, 
                  waic_varying_effects))


#NOTE:  waic is -2*elpd_diff


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ==  FITTING STAN BAYESIAN MODELS W/ WTA DATA           =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

wta_data <- read.csv('./data/wta_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE)


wta_training_data <- wta_data %>%
  distinct() %>%
  mutate( 
    # -- Remember, the last level is the reference level in the STAN model
    y = ifelse(intended_serve_dir == 'Body', 3, 
               ifelse(intended_serve_dir == 'T', 1, 
                      ifelse(intended_serve_dir == 'Wide', 2, NA))),
    is_first_serve = ifelse(serve_num == 1, 1,0) ) %>%
  select(y, server_name, returner_name, 
         is_break_point, x_ball_at_serve,
         is_first_serve,
         court_side,
         is_prev_doublefault, is_prev_ace, 
         serve_impact_from_center,
         server_hand, returner_hand,
         point_importance, 
         z_scaled_point_importance,
         returner_backhand_loc,
         prev_intended_serve_loc1,
         prev_intended_serve_loc2,
         player_hands_match,
         z_ball_at_serve,
         player_hands_match
  ) %>%
  mutate(## --model.matrix() not cooperating with factors...I'll do this manually
    prev_intended_serve_loc1T = ifelse(prev_intended_serve_loc1 == 'T',1,0),
    prev_intended_serve_loc1Wide = ifelse(prev_intended_serve_loc1 == 'Wide',1,0),
    prev_intended_serve_loc2T = ifelse(prev_intended_serve_loc2 == 'T',1,0),
    prev_intended_serve_loc2Wide = ifelse(prev_intended_serve_loc2 == 'Wide',1,0),
    is_second_serve = ifelse(is_first_serve == 1,0,1),
    court_side_ad = ifelse(court_side == 'DeuceCourt', 0,1),
    returner_backhand_locT = ifelse(returner_backhand_loc == 'T', 1,0),
    server_handL = ifelse(server_hand == 'left-handed', 1,0),
    distance_inside_serve = 11.89 - abs(x_ball_at_serve),
    interaction_s_hand_court_side = server_handL * court_side_ad,
    interaction_ss_bh_T = is_second_serve*returner_backhand_locT
  ) %>%
  filter(complete.cases(.))

wta_training_data$server_index <- as.numeric(factor(wta_training_data$server_name))


wta_common_intercept_datalist <- list(
  N = nrow(wta_training_data),
  y = wta_training_data$y,
  K = 3
)

wta_varying_intercept_datalist <- list(
  N = nrow(wta_training_data),
  num_players = length(unique(wta_training_data$server_index)),
  player_id = wta_training_data$server_index,
  y = wta_training_data$y,
  K = 3
)

wta_expanded_datalist <- list(
  N = nrow(wta_training_data),
  num_players = length(unique(wta_training_data$server_index)),
  y = wta_training_data$y,
  player_id = wta_training_data$server_index,
  x1 = wta_training_data$is_second_serve,
  x2 = wta_training_data$serve_impact_from_center,
  x3 = wta_training_data$distance_inside_serve,
  x4 = wta_training_data$prev_intended_serve_loc1T,
  x5 = wta_training_data$prev_intended_serve_loc1Wide,
  x6 = wta_training_data$returner_backhand_locT,
  x7 = wta_training_data$point_importance,
  x8 = wta_training_data$server_handL,
  x9 = wta_training_data$court_side_ad,
  x10 = wta_training_data$interaction_s_hand_court_side,
  x11 = wta_training_data$interaction_ss_bh_T,
  K = 3
)

wta_varying_effects_datalist <- list(
  N = nrow(wta_training_data),
  num_players = length(unique(wta_training_data$server_index)),
  y = wta_training_data$y,
  player_id = wta_training_data$server_index,
  x1 = wta_training_data$point_importance, # Only this covariate is player-varying
  x2 = wta_training_data$serve_impact_from_center,
  x3 = wta_training_data$distance_inside_serve,
  x4 = wta_training_data$prev_intended_serve_loc1T,
  x5 = wta_training_data$prev_intended_serve_loc1Wide,
  x6 = wta_training_data$returner_backhand_locT,
  x7 = wta_training_data$court_side_ad,
  x8 = wta_training_data$server_handL,
  x9 = wta_training_data$is_second_serve,
  x10 = wta_training_data$interaction_s_hand_court_side,
  x11 = wta_training_data$interaction_ss_bh_T,
  K = 3
)


# ~1 minute to run

wta_fit_common_intercept <- sampling(common_intercept, 
                                     data = wta_common_intercept_datalist, 
                                     iter = 2000)
saveRDS(wta_fit_common_intercept, 
        file = "./modelling/saved_models/stan_loglik/wta_common_intercept_fit.RDS")

#~10 minutes to run
wta_fit_varying_intercept <- sampling(varying_intercept, 
                                      data = wta_varying_intercept_datalist, 
                                      iter = 2000)

saveRDS(wta_fit_varying_intercept, 
        file = "./modelling/saved_models/stan_loglik/wta_varying_intercept_fit.RDS")

# ~ 102 minutes
wta_fit_expanded <- sampling(expanded, 
                             data = wta_expanded_datalist, 
                             iter = 2000)
saveRDS(wta_fit_expanded, 
        file = "./modelling/saved_models/stan_loglik/wta_expanded_fit.RDS")


wta_fit_varying_effects <- sampling(varying_effects, 
                                    data = wta_varying_effects_datalist, 
                                    iter = 2000)
saveRDS(wta_fit_varying_effects, 
        file = "./modelling/saved_models/stan_loglik/wta_varying_effects_fit.RDS")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====                  COMPUTE WTA WAIC                       =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
wta_fit_common_intercept <- readRDS("./modelling/saved_models/stan_loglik/wta_common_intercept_fit.RDS")
wta_fit_varying_intercept <- readRDS("./modelling/saved_models/stan_loglik/wta_varying_intercept_fit.RDS")
wta_fit_expanded <- readRDS("./modelling/saved_models/stan_loglik/wta_expanded_fit.RDS")
wta_fit_varying_effects <- readRDS("./modelling/saved_models/stan_loglik/wta_varying_effects_fit.RDS")


wta_log_lik_common_intercept <- extract_log_lik(wta_fit_common_intercept)
wta_log_lik_varying_intercept <- extract_log_lik(wta_fit_varying_intercept)
wta_log_lik_expanded <- extract_log_lik(wta_fit_expanded)
wta_log_lik_varying_effects <- extract_log_lik(wta_fit_varying_effects)

wta_waic_common_intercept <- waic(wta_log_lik_common_intercept)
wta_waic_varying_intercept  <- waic(wta_log_lik_varying_intercept)
wta_waic_expanded <- waic(wta_log_lik_expanded)
wta_waic_varying_effects <- waic(wta_log_lik_varying_effects)

print(wta_waic_common_intercept)
print(wta_waic_varying_intercept)
print(wta_waic_expanded)
print(wta_waic_varying_effects)
print(loo_compare(wta_waic_common_intercept, 
                  wta_waic_varying_intercept, 
                  wta_waic_expanded, 
                  wta_waic_varying_effects ))


