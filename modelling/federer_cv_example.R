# --- || --- || --- || --- || --- || --- || --- || --- || --- || --- || 
### -- Cross-Validation Prediction Example -----
# A reviewer asks to evaluate the expanded model by calculating its accuracy 
#  based on predictions on a training set. 
# We fit the expanded model using the entire data except for Federer's
# last 100 serve observations. Predictions are then assessed on the 
# held-out sample.
# --- || --- || --- || --- || --- || --- || --- || --- || --- || --- || 

setwd("/Users/petertea/Documents/intended_serve_direction")
library(dplyr)
library(rstan)
source(file = "./src/helper_functions.R")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### =====              Data Processing Steps                      =====
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
atp_data <- read.csv('./data/atp_processed_roland_garros_tracking_data.csv',
                     stringsAsFactors = FALSE,
                     na.strings=c("","NA"))

atp_data <- atp_data %>%
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

atp_data$server_index <- as.numeric(factor(atp_data$server_name))


# -- Test data includes Federer's last 100 observations
# -- Training data is everything else
num_fed_obs <- sum(atp_data$server_name == 'R.FEDERER')
test_data_indices <- which(atp_data$server_name == 'R.FEDERER')[(num_fed_obs-99):num_fed_obs]
training_data_indices <- setdiff(1:nrow(atp_data), test_data_indices)

test_data <- atp_data %>%
  slice(test_data_indices)

training_data <- atp_data %>%
  slice(training_data_indices)


# --- || --- || --- || --- || --- || --- || --- || --- || --- || --- || 
# --- FIT EXPANDED MODEL ----
# --- || --- || --- || --- || --- || --- || --- || --- || --- || --- || 
atp_expanded_datalist <- list(
  N = nrow(training_data),
  num_players = length(unique(training_data$server_index)),
  y = training_data$y,
  player_id = training_data$server_index,
  x1 = training_data$is_second_serve,
  x2 = training_data$serve_impact_from_center,
  x3 = training_data$distance_inside_serve,
  x4 = training_data$prev_intended_serve_loc1T,
  x5 = training_data$prev_intended_serve_loc1Wide,
  x6 = training_data$returner_backhand_locT,
  x7 = training_data$point_importance,
  x8 = training_data$server_handL,
  x9 = training_data$court_side_ad,
  x10 = training_data$interaction_s_hand_court_side,
  x11 = training_data$interaction_ss_bh_T,
  K = 3
)


options(mc.cores = 4)
# -- Load a stan script that doesn't compute pointwise log-likelihood (bottleneck!)
quick_expanded <- stan_model('./modelling/stan_files/quick_expanded_model.stan')

atp_fit_quick_expanded <- sampling(quick_expanded, 
                                   data = atp_expanded_datalist, 
                                   iter = 2000)

saveRDS(atp_fit_quick_expanded, 
        file = "./modelling/saved_models/stan_loglik/atp_quick_expanded_fit.RDS")


# -- Get posterior predictive distributions on test data: -----
extract_quick_expanded_model <- extract(atp_fit_quick_expanded)

#levels(factor(training_data$server_name))
fed_id <- 62

post_prediction <- vector()
set.seed(824)
for(serve_obs_id in 1:nrow(test_data)){
  
  x1 = test_data[serve_obs_id, 'is_second_serve']
  x2 = test_data[serve_obs_id, 'serve_impact_from_center']
  x3 = test_data[serve_obs_id, 'distance_inside_serve']
  x4 = test_data[serve_obs_id, 'prev_intended_serve_loc1T']
  x5 = test_data[serve_obs_id, 'prev_intended_serve_loc1Wide']
  x6 = test_data[serve_obs_id, 'returner_backhand_locT']
  x7 = test_data[serve_obs_id, 'point_importance']
  x8 = test_data[serve_obs_id, 'server_handL']
  x9 = test_data[serve_obs_id, 'court_side_ad']
  x10 = test_data[serve_obs_id, 'interaction_s_hand_court_side']
  x11 = test_data[serve_obs_id, 'interaction_ss_bh_T']

  
  prob_T_logit <-(extract_quick_expanded_model$v_intercept[,fed_id,1] + extract_quick_expanded_model$B_0[,1]) + # Intercept
    (extract_quick_expanded_model$B_1[,1] * x1) +
    (extract_quick_expanded_model$B_2[,1] * x2) + 
    (extract_quick_expanded_model$B_3[,1] * x3) + 
    (extract_quick_expanded_model$B_4[,1] * x4) + 
    (extract_quick_expanded_model$B_5[,1] * x5) + 
    (extract_quick_expanded_model$B_6[,1] * x6) + 
    (extract_quick_expanded_model$B_7[,1] * x7) + 
    (extract_quick_expanded_model$B_8[,1] * x8) + 
    (extract_quick_expanded_model$B_9[,1] * x9) + 
    (extract_quick_expanded_model$B_10[,1] * x10) + 
    (extract_quick_expanded_model$B_11[,1] * x11) 
  
  prob_Wide_logit <-(extract_quick_expanded_model$v_intercept[,fed_id,2] + extract_quick_expanded_model$B_0[,2]) + # Intercept
    (extract_quick_expanded_model$B_1[,2] * x1) +
    (extract_quick_expanded_model$B_2[,2] * x2) + 
    (extract_quick_expanded_model$B_3[,2] * x3) + 
    (extract_quick_expanded_model$B_4[,2] * x4) + 
    (extract_quick_expanded_model$B_5[,2] * x5) + 
    (extract_quick_expanded_model$B_6[,2] * x6) + 
    (extract_quick_expanded_model$B_7[,2] * x7) + 
    (extract_quick_expanded_model$B_8[,2] * x8) + 
    (extract_quick_expanded_model$B_9[,2] * x9) + 
    (extract_quick_expanded_model$B_10[,2] * x10) + 
    (extract_quick_expanded_model$B_11[,2] * x11) 
  
  prob_Body_logit <- rep(0, times = 4000)
  
  prob_logit_df <- data.frame(
    T_logit = prob_T_logit,
    W_logit = prob_Wide_logit,
    B_logit = prob_Body_logit 
  )
  
  # -- Posterior probabilites for all 3 serve directions (4000 x 3 DF)
  normalized_probs <- apply(prob_logit_df, MARGIN = 1, FUN = softmax) %>% t() 
  
  # -- For each posterior sample, generate a prediction
  federer_prediction <- apply(normalized_probs, MARGIN = 1, FUN = predict_dir)
  
  # -- Posterior mean prediction
  post_prediction[serve_obs_id] <- sort(table(federer_prediction), decreasing = TRUE)[1] %>% names()
  
  
}
post_prediction <- as.numeric(post_prediction)

table(post_prediction,
      test_data$y)

sum(post_prediction == test_data$y)





