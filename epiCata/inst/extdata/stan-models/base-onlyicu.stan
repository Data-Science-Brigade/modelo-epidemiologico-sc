data {
  int <lower=1> M; // number of countries
  int <lower=1> P; // number of covariates
  int <lower=1> N0; // number of days for which to impute infections
  int<lower=1> N[M]; // days of observed data for country m. each entry must be <= N2
  int<lower=1> N2; // days of observed data + # of days to forecast
  int cases[N2,M]; // reported cases
  int icu_beds[N2,M]; // reported icu bed occupants
  int deaths[N2, M]; // reported deaths -- the rows with i > N contain -1 and should be ignored
  matrix[N2, M] f; // h * s
  matrix[N2, P] X[M]; // features matrix
  int EpidemicStart[M];
  real pop[M];
  real SI[N2]; // fixed pre-calculated SI using emprical data from Neil
}

transformed data {
  vector[N2] SI_rev; // SI in reverse order
  vector[N2] f_rev[M]; // f in reversed order

  for(i in 1:N2)
    SI_rev[i] = SI[N2-i+1];

  for(m in 1:M){
    for(i in 1:N2) {
     f_rev[m, i] = f[N2-i+1,m];
    }
  }
}


parameters {
  real<lower=0> mu[M]; // intercept for Rt
  real<lower=0> alpha_hier[P]; // sudo parameter for the hier term for alpha
  real<lower=0> gamma;
  vector[M] lockdown;
  real<lower=0> kappa;
  real<lower=0> y[M];
  real<lower=0> phi;
  real<lower=0> tau;
  real <lower=0> ifr_noise[M];
  real alpha[P];
  real alpha1[P,M];
  real alpha_pop;
  real alpha1_pop[M];

  //real<lower=0> infection_overestimate[M];
  //real<lower=0,upper=1> asymptomatic_pct[M];
  real<lower=0,upper=1> icu_pct[M];
}

transformed parameters {
    matrix[N2, M] prediction = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths  = rep_matrix(0,N2,M);
    matrix[N2, M] Rt = rep_matrix(0,N2,M);
    matrix[N2, M] Rt_adj = Rt;

    {
      matrix[N2,M] cumm_sum = rep_matrix(0,N2,M);
      vector[N2] linear_effect;
      for (m in 1:M){
        linear_effect = rep_vector(0,N2);
        prediction[1:N0,m] = rep_vector(y[m],N0); // learn the number of cases in the first N0 days
        cumm_sum[2:N0,m] = cumulative_sum(prediction[2:N0,m]);
        for (i in (N0+1):N2) {
          for(p in 1:P) {
            linear_effect[i] -= X[m,i,p] * (alpha[p] + alpha1[p,m]);
          }
          linear_effect[i] -= ((pop[m]-cumm_sum[i,m]) / pop[m]) * (alpha_pop + alpha1_pop[m]);
        }
        Rt[1:N0,m] = mu[m] * 2 * inv_logit(linear_effect[1:N0]);
        Rt_adj[1:N0,m] = Rt[1:N0,m];
        for (i in (N0+1):N2) {
          real convolution = dot_product(sub_col(prediction, 1, m, i-1), tail(SI_rev, i-1));
          cumm_sum[i,m] = cumm_sum[i-1,m] + prediction[i-1,m];

          for(p in 1:P) {
            linear_effect[i] -= X[m,i,p] * (alpha[p] + alpha1[p,m]);
          }
          linear_effect[i] -= ((pop[m]-cumm_sum[i,m]) / pop[m]) * (alpha_pop + alpha1_pop[m]);
          Rt[i,m] = mu[m] * 2 * inv_logit(linear_effect[i]);

          Rt_adj[i,m] = ((pop[m]-cumm_sum[i,m]) / pop[m]) * Rt[i,m];
          prediction[i, m] = Rt_adj[i,m] * convolution;
        }
        E_deaths[1, m]= 1e-15 * prediction[1,m];
        for (i in 2:N2){
          E_deaths[i,m] = ifr_noise[m] * dot_product(sub_col(prediction, 1, m, i-1), tail(f_rev[m], i-1));
        }
      }
    }
}
model {
  tau ~ exponential(0.03);
  for (m in 1:M){
      y[m] ~ exponential(1/tau);
  }
  gamma ~ normal(0,.2);
  lockdown ~ normal(0,gamma);
  phi ~ normal(0,5);
  kappa ~ normal(0,0.5);
  mu ~ normal(3.28, kappa); // citation: https://academic.oup.com/jtm/article/27/2/taaa021/5735319
  alpha ~ normal(0,0.5);
  alpha_pop ~ normal(0,0.5);
  for (i in 1:P)
    alpha1[i,] ~ normal(0,gamma);
  alpha1_pop ~ normal(0,gamma);
  alpha_hier ~ gamma(.1667,1);
  ifr_noise ~ normal(1,0.1);

  //infection_overestimate ~ normal(11.5,2);
  // We could use the asymptomatic_pct instead of infection_overestimate:
  // https://doi.org/10.3138/jammi-2020-0030
  // Meta-analysis (fixed effect) found that the proportion of asymptomatic cases was 17% (95% CI: 14%–20%) overall;
  // So mean on 0.17 with a 2*std hitting the edge of the 14%–20% range
  //asymptomatic_pct ~ normal(0.17,0.015);

  // http://dx.doi.org/10.1590/1806-9282.66.8.1157
  // In addition to a similar flu presentation, COVID-19 can manifest itself as a neurological syndrome, heart failure, or acute myocardial infarction7 , 8 . Most infections (80%) are mild. However, 6-10% will require transfer to the ICU9.
  // So mean on 0.08 with a std that slightly overshoots the 6-10% range (No 95%  C.I. given)
  icu_pct ~ normal(0.08,0.015);

  for(m in 1:M){
    icu_beds[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(prediction[EpidemicStart[m]:N[m], m] * icu_pct[m], phi);
    //cases[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(prediction[EpidemicStart[m]:N[m], m] / infection_overestimate[m], phi);
    deaths[EpidemicStart[m]:N[m], m] ~ neg_binomial_2(E_deaths[EpidemicStart[m]:N[m], m], phi);
   }
}

generated quantities {
    matrix[N2, M] prediction0 = rep_matrix(0,N2,M);
    matrix[N2, M] E_deaths0  = rep_matrix(0,N2,M);

    {
      matrix[N2,M] cumm_sum0 = rep_matrix(0,N2,M);
      for (m in 1:M){
         for (i in 2:N0){
          cumm_sum0[i,m] = cumm_sum0[i-1,m] + y[m];
        }
        prediction0[1:N0,m] = rep_vector(y[m],N0);
        for (i in (N0+1):N2) {
          real convolution0 = dot_product(sub_col(prediction0, 1, m, i-1), tail(SI_rev, i-1));
          cumm_sum0[i,m] = cumm_sum0[i-1,m] + prediction0[i-1,m];
          prediction0[i, m] = ((pop[m]-cumm_sum0[i,m]) / pop[m]) * mu[m] * convolution0;
        }
        E_deaths0[1, m]= 1e-15 * prediction0[1,m];
        for (i in 2:N2){
          E_deaths0[i,m] = ifr_noise[m] * dot_product(sub_col(prediction0, 1, m, i-1), tail(f_rev[m], i-1));
        }
      }
    }
}

