set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_price <- 100 # base price of each product
discount <- 0.0 # discount for bundle pricing
bundle_size <- 2 # number of products in the bundle
mu <- log(150) # desired price point for price skimming
sigma <- 0.5 # standard deviation of log normal distribution for customer preference
elasticity <- -1.5 # elasticity for price skimming

# Simulate customer preferences for price skimming
price_skimming_pref <- matrix(rlnorm(n_customers, mu, sigma), n_sim, n_customers)

# Calculate demand and revenue for price skimming
demand_price_skimming <- base_price / ((price_skimming_pref/base_price)^elasticity)
revenue_price_skimming <- ifelse(demand_price_skimming >= 1, base_price * (1 - discount), demand_price_skimming * price_skimming_pref * (1 - discount))

# Calculate expected revenue for price skimming
expected_revenue_price_skimming <- mean(rowSums(revenue_price_skimming))

# Print expected revenue for price skimming
cat("Expected revenue for price skimming: $", round(expected_revenue_price_skimming, 2))
