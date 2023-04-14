set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10 # number of simulations
n_customers <- 10 # number of customers
base_price <- 10 # base price of each product
discount <- 0.2 # discount for bundle pricing
bundle_size <- 2 # number of products in the bundle
mu <- log(50) # mean of log normal distribution for customer preference
sigma <- 0.5 # standard deviation of log normal distribution for customer preference

# Simulate customer preferences for bundle pricing
bundle_pref <- matrix(0, n_sim, n_customers)
for (i in 1:n_sim) {
  for (j in 1:n_customers) {
    bundle_pref[i, j] <- sum(rlnorm(bundle_size, mu, sigma))
  }
}

# Calculate demand and revenue for bundle pricing
demand_bundle <- matrix(0, n_sim, n_customers)
revenue_bundle <- matrix(0, n_sim, n_customers)
for (i in 1:n_sim) {
  for (j in 1:n_customers) {
    demand_bundle[i, j] <- ifelse(bundle_pref[i, j] > base_price, 0, 1)
    revenue_bundle[i, j] <- ifelse(demand_bundle[i, j] == 1, (bundle_size * base_price * (1 - discount)), 0)
  }
}

# Calculate expected revenue for bundle pricing
expected_revenue_bundle <- mean(rowSums(revenue_bundle))

# Print expected revenue for bundle pricing
cat("Expected revenue for bundle pricing: $", round(expected_revenue_bundle, 2))
