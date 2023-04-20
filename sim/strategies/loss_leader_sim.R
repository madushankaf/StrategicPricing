simulate_loss_leader <- function(n_sim, n_customers, base_price, discount, mu, sigma, elasticity, seed) {
  set.seed(seed) # set seed for reproducibility
  
  # Simulate customer preferences for loss leader pricing
  loss_leader_pref <- matrix(rlnorm(n_customers, mu, sigma), n_sim, n_customers)
  
  # Calculate demand and revenue for loss leader pricing
  demand_loss_leader <- ifelse(loss_leader_pref >= base_price, 1, 0)
  price_effect <- (base_price * discount) / (base_price * (1 - discount))
  new_price <- base_price * (1 - discount)
  new_demand <- demand_loss_leader * (1 + elasticity * (new_price - base_price) / base_price)
  revenue_loss_leader <- ifelse(new_demand == 1, new_price, 0)
  
  # Calculate expected revenue for loss leader pricing
  expected_revenue_loss_leader <- mean(rowSums(revenue_loss_leader))
  
  return(expected_revenue_loss_leader)
}


# simulate_loss_leader(n_sim = 10000, n_customers = 10000, base_price = 100, discount = 0.5, mu = log(50), sigma = 0.5, elasticity = -1.5)
