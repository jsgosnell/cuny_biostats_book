J = 10                # number of individuals
n_j = 30              # number of observations per individual 
sigma_b = 5           # variance of the random intercept
sigma_e = 1           # variance of the random noise
fixed_effect = 1      # population-level effect
fixed_intercept = 5   # population-level intercept

data = data.frame(
  subject_id = rep(1:J,
  
  # Generate subject-specific intercept deviations
  b_i = rnorm(J, mean = 0, sd = sigma_b),
  
  # Generate uniform predictor (easier to visualize)
  X = map(subject_id, function(id) {
    runif(n_j, min = 0, max = 10)
  }), 
  
  # Generate outcome from predictor & subject-specific intercept
  Y = map2(b_i, X, function(bi0, x) {
    (fixed_intercept + bi0) + fixed_effect  * x + rnorm(n_j, mean = 0, sd = sigma_e)
  }) 
) |> 
  unnest(c(X, Y))

data2 <- data.frame(tank = c(rep(1:J,each=n_j)))