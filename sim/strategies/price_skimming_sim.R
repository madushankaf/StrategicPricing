simulate_price_skimming <- function(n_sim, n_customers, base_price, discount, bundle_size, mu, sigma, elasticity, seed) {
  set.seed(seed) # set seed for reproducibility
  
  # Simulate customer preferences for price skimming
  price_skimming_pref <- matrix(rlnorm(n_customers, mu, sigma), n_sim, n_customers)
  
  # Calculate demand and revenue for price skimming
  demand_price_skimming <- base_price / ((price_skimming_pref/base_price)^elasticity)
  revenue_price_skimming <- ifelse(demand_price_skimming >= 1, base_price * (1 - discount), demand_price_skimming * price_skimming_pref * (1 - discount))
  
  # Calculate expected revenue for price skimming
  expected_revenue_price_skimming <- mean(rowSums(revenue_price_skimming))
  
  # Return expected revenue for price skimming
  return(expected_revenue_price_skimming)
}

# Example usage:
# simulate_price_skimming(n_sim = 10000, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(150), sigma = 0.5, elasticity = -1.5)
