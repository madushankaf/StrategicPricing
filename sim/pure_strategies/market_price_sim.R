set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_price <- 100 # base price of the product
discount <- 0.0 # no discount for market pricing
mu <- log(50) # desired price point for market pricing
sigma <- 0.5 # standard deviation of log normal distribution for customer preference

# Simulate customer preferences for market pricing
market_pref <- matrix(rlnorm(n_customers, mu, sigma), n_sim, n_customers)

# Calculate demand and revenue for market pricing
demand_market <- ifelse(market_pref >= base_price, 1, 0)
revenue_market <- ifelse(demand_market == 1, base_price * (1 - discount), 0)

# Calculate expected revenue for market pricing
expected_revenue_market <- mean(rowSums(revenue_market))

# Print expected revenue for market pricing
cat("Expected revenue for market pricing: $", round(expected_revenue_market, 2))
