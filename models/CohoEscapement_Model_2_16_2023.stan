data{
  int T;
  int P;
  matrix[T,P] F_miles;
  matrix[T,P] GRTS_miles;
  matrix[T,P] missed_miles;
  //pM
  int n_pm; //number of pMark Likelihoods
  int H[n_pm]; //Hatchery adults
  int MS[n_pm]; //Mark-Sample adults
  int pop_pm[n_pm]; //pop index
  int yr_pm[n_pm]; //yr index
  //pF
  int n_pf; //number of pFemale Likelihoods
  int F[n_pf]; //Hatchery adults
  int AS[n_pf]; //Adult Sample
  int pop_pf[n_pf]; //pop index
  int yr_pf[n_pf]; //yr index
  //mr
  int n_mr; //number of mark-recap Likelihoods
  int n1[n_mr]; //marks
  int n2[n_mr]; //captures
  int m[n_mr]; //recaptures
  int pop_mr[n_mr]; //pop index
  int yr_mr[n_mr]; //yr index
  //index redds
  int n_Y2; //number of index redd Likelihoods
  int Y2[n_Y2]; //index redd count
  int pop_Y2[n_Y2]; //pop index
  int yr_Y2[n_Y2]; //yr index
  //grts redds
  int n_Y; //number of grts redd Likelihoods
  int Y[n_Y]; //GRTS redd count
  vector[n_Y] g; //GRTS reach length
  int pop_Y[n_Y]; //pop index
  int yr_Y[n_Y]; //yr index
  //indexes to identify when >0 GRTS and >0 Index reaches
  int index_data_only[T,P];
  int n_grts_and_index;
  int yr_grts_and_index[n_grts_and_index];
  int pop_grts_and_index[n_grts_and_index];
  //Trap and Haul Data
  int n_TH_pops; //count of trap and haul pops
  int TH_pops[n_TH_pops]; //pop indexes for each TH pop 
  int TH_a_UM[T,n_TH_pops]; //TH unmarked adults
  int TH_a_M[T,n_TH_pops]; //TH marked adults
  int n_crc_years; //count of crc years with non-zero, non-NA data. 
  int n_crc_pops; //count of crc pops with at least one year of non-zero, non-NA data.
  int crc_pops[n_crc_pops]; //pop indexes for each crc pop 
  int crc_years[n_crc_years]; // year indexes for each crc likelihood (assumes available years have data for all pops)
  matrix[n_crc_years,n_crc_pops] catch_a_M_mu; //crc MLEs
  matrix[n_crc_years,n_crc_pops] catch_a_M_SD; //crc variances converted via moment matching to log-normal SD's
  //priors
  real<lower=0> cauchy_scale;
}
transformed data{
}
parameters{
  //first states
  real log_lambda_0_mu;
  real<lower=0> log_lambda_0_sd;
  vector[P] log_lambda_0_eps;
  vector[P] logit_pM_0;
  //shared density process errrors
  vector[T-1] eps_lambda_all;
  real<lower=0> sigma_lambda_all;
  //pop-specfic density process errors  
  matrix[T-1,P] eps_lambda;
  real<lower=0> sigma_lambda_mu;
  real<lower=0> sigma_lambda_sigma;
  vector[P] eps_sigma_lambda; 
  //pop-specfic pHOS process errors  
  matrix[T-1,P] eps_pM;
  real<lower=0> sigma_pM;
  //pop-specfic pF random effects 
  vector[T*P] eps_pF;
  real<lower=0,upper=1> mu_pF;
  real<lower=0> sigma_pF;
  //RpF
  real<lower=0> RpF;
  //p_MR
  vector<lower=0,upper=1>[n_mr] p_MR;
  //pop-specfic GRTS overdispersion  
  real<lower=0> sigma_disp_mu;
  real<lower=0> sigma_disp_sigma;
  vector[P] eps_sigma_disp;
  //P_Index_both
  vector<lower=0,upper=1>[n_grts_and_index] p_Index_both;
  //pop-specfic Harvest Rate random effects 
  matrix[T,n_crc_pops] eps_HR;
  real<lower=0> sigma_HR;
  vector<lower=0,upper=1>[n_crc_pops] mu_HR;
}
transformed parameters{
  //fish density
  matrix[T,P] log_lambda;
  //Redds
  matrix[T,P] Redds;
  //GRTS redd overdispersion
  vector<lower=0>[P] sigma_disp = exp(log(sigma_disp_mu) + eps_sigma_disp * sigma_disp_sigma);
  //Total Abundance
  matrix[T,P] Adults;
  //UM Abundance
  matrix[T,P] UM_ad;
  //M Abundance
  matrix[T,P] M_ad;
  //proccess SD for fish Density
  vector<lower=0>[P] sigma_lambda = exp(log(sigma_lambda_mu) + eps_sigma_lambda * sigma_lambda_sigma);
  //pHOS
  matrix<lower=0,upper=1>[T,P] pM;
  //pF
  matrix<lower=0,upper=1>[T,P] pF;
  //p_Index
  matrix<lower=0,upper=1>[T,P] p_Index;
  //HR_a_M
  matrix<lower=0,upper=1>[T,n_crc_pops] HR_a_M;
  //initiate first states
  log_lambda[1,1:P] = to_row_vector(log_lambda_0_mu + log_lambda_0_eps[1:P] * log_lambda_0_sd);
  pM[1,1:P] = to_row_vector(inv_logit(logit_pM_0[1:P]));
  //process models
  for(t in 2:T){
    log_lambda[t,1:P] = to_row_vector(to_vector(log_lambda[t-1,1:P]) + eps_lambda_all[t-1] * sigma_lambda_all +  diag_matrix(sigma_lambda) * to_vector(eps_lambda[t-1,1:P]));
    pM[t,1:P] = to_row_vector(inv_logit(logit(pM[t-1,1:P]) + eps_pM[t-1,1:P] * sigma_pM));
  }
  //pF
  pF = to_matrix(inv_logit(logit(mu_pF) + eps_pF * sigma_pF),T,P,2);
  //HR (hierarchical across years)
  for(p in 1:n_crc_pops){
    HR_a_M[1:T,p] = inv_logit(logit(mu_HR[p]) + eps_HR[1:T,p] * sigma_HR);
  }
  //Adults
  UM_ad = exp(log_lambda + log(F_miles));
  //M_ad
  M_ad = exp(log(UM_ad) + log(pM) - log(1-pM)); //same as UM_ad * (pM/(1-pM))
  //Adults
  Adults = UM_ad + M_ad;
  //Redds
  Redds = exp(log(Adults) + log(pF) + log(RpF));
  //p_Index
  for(t in 1:T){
    for(p in 1:P){
      p_Index[t,p] = index_data_only[t,p];
    }
  }
  for(i in 1:n_grts_and_index){
      p_Index[yr_grts_and_index[i],pop_grts_and_index[i]] += p_Index_both[i];
  }
}
model{
  //=========local variables=======
  real mu_local[n_Y]; //mean GRTS redd density
  int match_crc_TH;
  //=========Priors================
  //shared density process errors
  sigma_lambda_all ~ cauchy(0,cauchy_scale);
  eps_lambda_all[1:T-1] ~ std_normal();
  //pop-specfic density process errors
  sigma_lambda_mu ~ inv_gamma(1,0.125); 
  sigma_lambda_sigma ~ std_normal();
  eps_sigma_lambda ~ std_normal();
  to_vector(eps_lambda) ~ std_normal();
  //pop-specfic pM process errors
  sigma_pM ~ cauchy(0,cauchy_scale);
  to_vector(eps_pM) ~ std_normal();
  //initial states
  log_lambda_0_mu ~ normal(3,1.5);
  log_lambda_0_sd ~ cauchy(0,cauchy_scale);
  log_lambda_0_eps ~ std_normal();
  logit_pM_0 ~ normal(0,3);
  //pF
  sigma_pF ~ cauchy(0,cauchy_scale);
  mu_pF ~ beta(0.5,0.5);
  eps_pF ~ std_normal();
  //HR
  sigma_HR ~ cauchy(0,cauchy_scale);
  mu_HR ~ beta(0.5,0.5);
  to_vector(eps_HR) ~ std_normal();
  //RpF
  RpF ~ lognormal(0,1);
  //p_MR (mark-recapture prob of capture)
  p_MR ~ beta(0.5,0.5);
  //pop-specfic GRTS overdispersion  
  sigma_disp_mu ~ lognormal(0,1);
  sigma_disp_sigma ~ std_normal();
  eps_sigma_disp ~ std_normal();
  //P_Index_both
  p_Index_both ~ beta(0.5,0.5); 
  //=========Likelihoods===========
  //pM
  for(i in 1:n_pm){
    H[i] ~ binomial(MS[i],pM[yr_pm[i],pop_pm[i]]);
  }
  //pF
  for(i in 1:n_pf){
    F[i] ~ binomial(AS[i],pF[yr_pf[i],pop_pf[i]]);
  }
  //MR
  for(i in 1:n_mr){
    n1[i] ~ poisson(Adults[yr_mr[i],pop_mr[i]] * p_MR[i]);
    m[i] ~ binomial(n2[i],p_MR[i]);
  }
  //Y2 index redds
  for(i in 1:n_Y2){
    Y2[i] ~ poisson(Redds[yr_Y2[i],pop_Y2[i]] * p_Index[yr_Y2[i],pop_Y2[i]]);
  }
  //Y GRTS redds
  for(i in 1:n_Y){
    mu_local[i] = (Redds[yr_Y[i],pop_Y[i]] * (1 - p_Index[yr_Y[i],pop_Y[i]])) / (GRTS_miles[yr_Y[i],pop_Y[i]] + missed_miles[yr_Y[i],pop_Y[i]]);
    Y[i] ~ neg_binomial_2(mu_local[i] * g[i], 1/square(sigma_disp[pop_Y[i]]));
  }
  // Trap and Haul Data
  for(t in 1:T){
    for(p in 1:n_TH_pops){
      match_crc_TH = 0;
      for(k in 1:n_crc_pops){
        if(TH_pops[p] == crc_pops[k]){
          match_crc_TH += k;
        }
      }
      if(match_crc_TH>0){
        TH_a_M[t,p] ~ poisson(M_ad[t,TH_pops[p]] / (1 - HR_a_M[t,match_crc_TH]));
        TH_a_UM[t,p] ~ poisson(UM_ad[t,TH_pops[p]] / (1 - HR_a_M[t,match_crc_TH] * 0.1));
      }else{
        TH_a_M[t,p] ~ poisson(M_ad[t,TH_pops[p]]);
        TH_a_UM[t,p] ~ poisson(UM_ad[t,TH_pops[p]]);
      }
    }
  }
  //CRC data
  for(t in 1:n_crc_years){
    for(p in 1:n_crc_pops){
      catch_a_M_mu[t,p] ~ lognormal(log(M_ad[crc_years[t],crc_pops[p]] * HR_a_M[crc_years[t],p]/(1-HR_a_M[crc_years[t],p])) - square(catch_a_M_SD[t,p])/2, catch_a_M_SD[t,p]); 
    }
  }
}
generated quantities {
  // Posterior Predictive for pM
  int<lower=0> H_rep[n_pm];
  for (i in 1:n_pm) {
    H_rep[i] = binomial_rng(MS[i], pM[yr_pm[i], pop_pm[i]]);
  }

  // Posterior Predictive for pF
  int<lower=0, upper=1> F_rep[n_pf];
  for (i in 1:n_pf) {
    F_rep[i] = binomial_rng(AS[i], pF[yr_pf[i], pop_pf[i]]);
  }

  // Posterior Predictive for MR
  int<lower=0> n1_rep[n_mr];
  int<lower=0> m_rep[n_mr];
  for (i in 1:n_mr) {
    n1_rep[i] = poisson_rng(Adults[yr_mr[i], pop_mr[i]] * p_MR[i]);
    m_rep[i] = binomial_rng(n2[i], p_MR[i]);
  }

  // Posterior Predictive for Y2
  int<lower=0> Y2_rep[n_Y2];
  for (i in 1:n_Y2) {
    Y2_rep[i] = poisson_rng(Redds[yr_Y2[i], pop_Y2[i]] * p_Index[yr_Y2[i], pop_Y2[i]]);
  }

  // Posterior Predictive for Y
  real mu_local_rep[n_Y];
  int<lower=0> Y_rep[n_Y];
  for (i in 1:n_Y) {
    mu_local_rep[i] = (Redds[yr_Y[i], pop_Y[i]] * (1 - p_Index[yr_Y[i], pop_Y[i]])) / (GRTS_miles[yr_Y[i], pop_Y[i]] + missed_miles[yr_Y[i], pop_Y[i]]);
    Y_rep[i] = neg_binomial_2_rng(mu_local_rep[i] * g[i], 1 / square(sigma_disp[pop_Y[i]]));
  }

  // Posterior Predictive for Trap and Haul Data
  int<lower=0> TH_a_M_rep[T, n_TH_pops];
  int<lower=0> TH_a_UM_rep[T, n_TH_pops];
  for (t in 1:T) {
    for (p in 1:n_TH_pops) {
      int match_crc_TH_rep = 0;
      for (k in 1:n_crc_pops) {
        if (TH_pops[p] == crc_pops[k]) {
          match_crc_TH_rep += k;
        }
      }
      if (match_crc_TH_rep > 0) {
        TH_a_M_rep[t, p] = poisson_rng(M_ad[t, TH_pops[p]] / (1 - HR_a_M[t, match_crc_TH_rep]));
        TH_a_UM_rep[t, p] = poisson_rng(UM_ad[t, TH_pops[p]] / (1 - HR_a_M[t, match_crc_TH_rep] * 0.1));
      } else {
        TH_a_M_rep[t, p] = poisson_rng(M_ad[t, TH_pops[p]]);
        TH_a_UM_rep[t, p] = poisson_rng(UM_ad[t, TH_pops[p]]);
      }
    }
  }

  // Posterior Predictive for CRC data
  real<lower=0> catch_a_M_mu_rep[n_crc_years, n_crc_pops];
  for (t in 1:n_crc_years) {
    for (p in 1:n_crc_pops) {
      catch_a_M_mu_rep[t, p] = exp(normal_rng(log(M_ad[crc_years[t], crc_pops[p]] * HR_a_M[crc_years[t], p] / (1 - HR_a_M[crc_years[t], p])) - square(catch_a_M_SD[t, p])/2, catch_a_M_SD[t, p]));
    }
  }
}

