# Calculate optimal price and expected revenue for market pricing
simulate_market_pricing <- function(n_sim, n_customers, base_cost, elasticity, sigma, discount) {

  # Set random seed for reproducibility
  set.seed(123)

  # Calculate optimal price
  optimal_price <- (1 + 1/elasticity) * base_cost

  # Simulate customer preferences
  market_pref <- matrix(rlnorm(n_customers, log(optimal_price), sigma), n_sim, n_customers)

  # Calculate demand and revenue
  demand_market <- ifelse(market_pref >= optimal_price, 1, 0)
  revenue_market <- ifelse(demand_market == 1, optimal_price * (1 - discount), 0)

  # Calculate expected revenue
  expected_revenue_market <- mean(rowSums(revenue_market))

  # Return optimal price and expected revenue
  return(expected_revenue_market)
}


simulate_market_pricing(n_sim = 10000, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(150), sigma = 0.5, elasticity = -1.5)
