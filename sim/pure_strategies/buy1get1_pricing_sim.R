set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_price <- 100 # base price of each product
discount <- 0.0 # discount for bundle pricing
bundle_size <- 2 # number of products in the bundle
mu <- log(50) # mean of log normal distribution for customer preference
sigma <- 0.5 # standard deviation of log normal distribution for customer preference

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
    if (buy1get1_pref[i, j] >= base_price) {
      demand_buy1get1[i, j] <- 2 # customer gets one product for free
      revenue_buy1get1[i, j] <- (2 * base_price * (1 - discount))
    } else {
      demand_buy1get1[i, j] <- 1 # customer pays for both products
      revenue_buy1get1[i, j] <- (2 * base_price * (1 - discount))
    }
  }
}

# Calculate expected revenue for buy 1 get 1 pricing
expected_revenue_buy1get1 <- mean(rowSums(revenue_buy1get1))

# Print expected revenue for buy 1 get 1 pricing
cat("Expected revenue for buy 1 get 1 pricing: $", round(expected_revenue_buy1get1, 2))
