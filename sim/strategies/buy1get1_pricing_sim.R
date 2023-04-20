simulate_buy1get1_revenue <- function(n_sim, n_customers, base_price, discount, bundle_size, mu, sigma, e) {
  set.seed(123) # set seed for reproducibility

  # Simulate customer preferences for buy 1 get 1 pricing
  buy1get1_pref <- matrix(0, n_sim, n_customers)
  for (i in 1:n_sim) {
    for (j in 1:n_customers) {
      buy1get1_pref[i, j] <- rlnorm(1, mu, sigma) + rlnorm(1, mu, sigma)
    }
  }

  # Calculate demand and revenue for buy 1 get 1 pricing
  demand_buy1get1 <- matrix(0, n_sim, n_customers)
  revenue_buy1get1 <- matrix(0, n_sim, n_customers)
  for (i in 1:n_sim) {
    for (j in 1:n_customers) {
      # Calculate quantity demanded at different prices
      q1 <- exp(mu + sigma * rnorm(1)) # quantity demanded at base price
      q2 <- exp(mu + sigma * rnorm(1)) # quantity demanded at higher price
      p1 <- base_price # base price
      p2 <- p1 * (1 + e) # higher price

      # Calculate price elasticity of demand
      elasticity <- ((q2 - q1) / q1) / ((p2 - p1) / p1)

      # Calculate demand and revenue based on price elasticity
      if (elasticity < e) {
        demand_buy1get1[i, j] <- 2 # customer gets one product for free
        revenue_buy1get1[i, j] <- (2 * p1 * (1 - discount))
      } else {
        demand_buy1get1[i, j] <- 1 # customer pays for both products
        revenue_buy1get1[i, j] <- (2 * p1 * (1 - discount))
      }
    }
  }

  # Calculate expected revenue for buy 1 get 1 pricing
  expected_revenue_buy1get1 <- mean(rowSums(revenue_buy1get1))

  # Return expected revenue for buy 1 get 1 pricing
  return(expected_revenue_buy1get1)
}

# Call the function with the provided parameters
simulate_buy1get1_revenue(n_sim = 10000, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(50), sigma = 0.5, e = -1.5)
