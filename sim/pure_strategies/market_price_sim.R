set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_cost <- 50 # cost of producing the product
elasticity <- -2 # elasticity of demand
sigma <- 0.5 # standard deviation of log normal distribution for customer preference
discount <- 0.0 # discount for market pricing

# Calculate optimal price
optimal_price <- (1 + 1/elasticity) * base_cost

# Simulate customer preferences for market pricing
market_pref <- matrix(rlnorm(n_customers, log(optimal_price), sigma), n_sim, n_customers)

# Calculate demand and revenue for market pricing
demand_market <- ifelse(market_pref >= optimal_price, 1, 0)
revenue_market <- ifelse(demand_market == 1, optimal_price * (1 - discount), 0)

# Calculate expected revenue for market pricing
expected_revenue_market <- mean(rowSums(revenue_market))

# Print expected revenue and optimal price for market pricing
cat("Optimal price for the product: $", round(optimal_price, 2), "\n")
cat("Expected revenue for market pricing: $", round(expected_revenue_market, 2))
