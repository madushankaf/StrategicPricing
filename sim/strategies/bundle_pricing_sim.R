simulate_bundle_pricing <- function(n_sim, n_customers, base_price, elasticity, discount, bundle_size, mu, sigma, seed) {
  set.seed(seed)

  # Simulate customer preferences for bundle pricing
  bundle_pref <- matrix(0, n_sim, n_customers)
  for (i in 1:n_sim) {
    for (j in 1:n_customers) {
      for (k in 1:bundle_size) {
        bundle_pref[i, j] <- bundle_pref[i, j] + rlnorm(1, mu, sigma)
      }
    }
  }

  # Calculate demand and revenue for bundle pricing
  demand_bundle <- matrix(0, n_sim, n_customers)
  revenue_bundle <- matrix(0, n_sim, n_customers)
  for (i in 1:n_sim) {
    for (j in 1:n_customers) {
      bundle_demand <- rep(0, bundle_size)
      for (k in 1:bundle_size) {
        bundle_demand[k] <- max(round(base_price[k] * (1 - elasticity[k] * (bundle_pref[i, j] - base_price[k]) / base_price[k]), 0), 0)
      }
      demand_bundle[i, j] <- min(bundle_demand)
      revenue_bundle[i, j] <- ifelse(demand_bundle[i, j] > 0, (bundle_size * base_price[1] * (1 - discount)), 0)
    }
  }

  # Calculate expected revenue for bundle pricing
  expected_revenue_bundle <- mean(rowSums(revenue_bundle))

  return(round(expected_revenue_bundle, 2))
}

# Call the function with the same parameters as in the original code
# simulate_bundle_pricing( n_sim = 10000, n_customers = 10000, base_price = c(100, 120, 80),elasticity = c(-2, -1, -0.5), discount = 0.0, bundle_size = 3,mu = log(50), sigma = 0.5)
