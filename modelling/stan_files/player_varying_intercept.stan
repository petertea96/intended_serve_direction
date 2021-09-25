// PLAYER-VARYING INTERCEPT MODEL
data{
    int N; // Sample size
    int num_players; // Number of players
    int y[N]; // Response Variable
    int player_id[N]; // Player ID column
    int K; // No. of response group categories
}
parameters{
  // Fixed Effects
    real B_0[K-1];  // intercepts for each response level
    
  // Random Effects
    matrix[K-1, num_players] z_intercept; // matrix of player standardized random effects
    vector<lower=0>[K-1] sigma_intercept; // stddev of player-varying effects
    cholesky_factor_corr[K-1] L_Rho_intercept; // correlation matrix of player-varying effects (between T and Wide)


}
transformed parameters{
  
  matrix[num_players, K-1] v_intercept;  // matrix of scaled player-varying effects
  
  v_intercept = (diag_pre_multiply(sigma_intercept, L_Rho_intercept) * z_intercept)';

}

model{
    
    // priors
    B_0 ~ normal(0,3);

    // hyper-prior
    to_vector(z_intercept) ~ normal(0,3);
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


  // Save pointwise log-likelihood to calculate LOO-CV and WAIC for model diagnostics
  // Save a vector of length N for the log likelihood values
  // categorical_logit_lpmf generates the likelihood of each observation, conditional
  // on the model. 
generated quantities{

    matrix[K-1,K-1] Rho_intercept;
    vector[N] log_lik;
    Rho_intercept = L_Rho_intercept * L_Rho_intercept';

    for ( i in 1:N ) {
        vector[K] p;
        for ( k in 1:(K-1) ) 
            p[k] = B_0[k] + v_intercept[player_id[i],k];
        p[K] = 0;
        log_lik[i] = categorical_logit_lpmf( y[i] | p );
    }
}
