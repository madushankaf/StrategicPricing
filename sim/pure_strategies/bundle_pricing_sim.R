set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_price <- c(100, 120, 80) # base price of each product
elasticity <- c(-2, -1, -0.5) # price elasticity of each product
discount <- 0.0 # discount for bundle pricing
bundle_size <- 3 # number of products in the bundle
mu <- log(50) # mean of log normal distribution for customer preference
sigma <- 0.5 # standard deviation of log normal distribution for customer preference

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

# Print expected revenue for bundle pricing
cat("Expected revenue for bundle pricing: $", round(expected_revenue_bundle, 2))
