// PLAYER-VARYING INTERCEPT MODEL
data{
    int N; // Sample size
    int num_players; // Number of unique players
    int y[N]; // Response Variable
    int player_id[N]; // ID column of player
    int K; // No. of response group categories
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    
  // Random Effects
    matrix[K-1, num_players] z_intercept; // matrix of standardized player-varying effects
    vector<lower=0>[K-1] sigma_intercept; // stddev of player-varying effect
    cholesky_factor_corr[K-1] L_Rho_intercept; // correlation matrix of player-varying effects


}
transformed parameters{
  
  matrix[num_players, K-1] v_intercept;  // matrix of scaled player-varying effects
  
  v_intercept = (diag_pre_multiply(sigma_intercept, L_Rho_intercept) * z_intercept)';

}

model{
    
    // priors
    B_0 ~ normal(0,5);

    // hyper-prior
    to_vector(z_intercept) ~ normal(0,5);
    sigma_intercept ~ cauchy(0, 2.5); // used to be exponential(1)
    L_Rho_intercept ~ lkj_corr_cholesky(2);
    

    // likelihood
    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + v_intercept[player_id[i],k];
        p[K] = 0;
        y[i] ~ categorical_logit( p );
    }
}


  // Sample from predictive posterior distribution
generated quantities{
    vector[K] p;
    matrix[K-1,K-1] Rho_intercept;

    vector[K] player_probs[num_players];
    vector[num_players] player_preds;


    Rho_intercept = L_Rho_intercept * L_Rho_intercept';


    for (the_player_id in 1:num_players){

        for ( resp_lev in 1:(K-1) ) 
            p[resp_lev] = (B_0[resp_lev] + v_intercept[the_player_id,resp_lev]);
        p[K] = 0;


        player_probs[the_player_id] = softmax(p);
        player_preds[the_player_id] = categorical_logit_rng(p);
    }

}
