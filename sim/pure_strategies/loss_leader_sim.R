set.seed(123) # set seed for reproducibility

# Define parameters
n_sim <- 10000 # number of simulations
n_customers <- 10000 # number of customers
base_price <- 100 # base price of the product
discount <- 0.5 # discount for loss leader pricing
mu <- log(50) # desired price point for loss leader pricing
sigma <- 0.5 # standard deviation of log normal distribution for customer preference

# Simulate customer preferences for loss leader pricing
loss_leader_pref <- matrix(rlnorm(n_customers, mu, sigma), n_sim, n_customers)

# Calculate demand and revenue for loss leader pricing
demand_loss_leader <- ifelse(loss_leader_pref >= base_price, 1, 0)
revenue_loss_leader <- ifelse(demand_loss_leader == 1, base_price * (1 - discount), 0)

# Calculate expected revenue for loss leader pricing
expected_revenue_loss_leader <- mean(rowSums(revenue_loss_leader))

# Print expected revenue for loss leader pricing
cat("Expected revenue for loss leader pricing: $", round(expected_revenue_loss_leader, 2))
