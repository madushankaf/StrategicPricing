source("./strategies/bundle_pricing_sim.R")
source("./strategies/market_price_sim.R")
source("./strategies/price_skimming_sim.R")
source("./strategies/buy1get1_pricing_sim.R")
source("./strategies/loss_leader_sim.R")

library(GNE)
library(nleqslv)

set.seed(123)
seed <- sample(1:100, 1)


# Define the players and actions
players <- c("Firm 1", "Firm 2")
actions_1 <- c("bundle_pricing", "buy1_get1", "loss_leader", "market_pricing", "price_skimming")
actions_2 <- c("bundle_pricing", "buy1_get1", "loss_leader", "market_pricing", "price_skimming")

game_matrix <- matrix(rep(0, 50), nrow = 10, byrow = TRUE)

n_sim <- 100

# bundling strategy
payoffs_A_bundle <- numeric(length = 5)
for (i in 1:5) {
  payoffs_A_bundle[i] <- simulate_bundle_pricing(n_sim = n_sim, n_customers = 10000, base_price = c(100, 120, 80), elasticity = c(-2, -1, -0.5), discount = 0.0, bundle_size = 3, mu = log(50), sigma = 0.5, seed = seed + i)
}
payoffs_B_bundle <- numeric(length = 5)
for (i in 1:5) {
  payoffs_B_bundle[i] <- simulate_bundle_pricing(n_sim = n_sim, n_customers = 10000, base_price = c(100, 120, 80), elasticity = c(-2, -1, -0.5), discount = 0.0, bundle_size = 3, mu = log(50), sigma = 0.5, seed = seed + 1 + i)
}
game_matrix[1, ] <- payoffs_A_bundle
game_matrix[6:10, 1] <- payoffs_B_bundle

# buy1_get1
payoffs_A_buy1get1 <- numeric(length = 5)
for (i in 1:5) {
  payoffs_A_buy1get1[i] <- simulate_buy1get1_revenue(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(50), sigma = 0.5, e = -1.5, seed = seed + i * 2)
}
payoffs_B_buy1get1 <- numeric(length = 5)
for (i in 1:5) {
  payoffs_B_buy1get1[i] <- simulate_buy1get1_revenue(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(50), sigma = 0.5, e = -1.5, seed = seed + 2 + i * 2)
}
game_matrix[2, ] <- payoffs_A_buy1get1
game_matrix[6:10, 2] <- payoffs_B_buy1get1

# loss_leader
payoffs_A_loss_leader <- numeric(length = 5)
for (i in 1:5) {
  payoffs_A_loss_leader[i] <- simulate_loss_leader(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0, mu = log(50), sigma = 0.5, elasticity = -1.5, seed = seed + i * 3)
}
payoffs_B_loss_leader <- numeric(length = 5)
for (i in 1:5) {
  payoffs_B_loss_leader[i] <- simulate_loss_leader(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0, mu = log(50), sigma = 0.5, elasticity = -1.5, seed = seed + 3 + i * 3)
}
game_matrix[3, ] <- payoffs_A_loss_leader
game_matrix[6:10, 3] <- payoffs_B_loss_leader

# market price
payoffs_A_market_price <- numeric(length = 5)
for (i in 1:5) {
  payoffs_A_market_price[i] <- simulate_market_pricing(n_sim = n_sim, n_customers = 10000, base_cost = 100, discount = 0.0, sigma = 0.5, elasticity = -1.5, seed = seed + i * 4)
}
payoffs_B_market_price <- numeric(length = 5)
for (i in 1:5) {
  payoffs_B_market_price[i] <- simulate_market_pricing(n_sim = n_sim, n_customers = 10000, base_cost = 100, discount = 0.0, sigma = 0.5, elasticity = -1.5, seed = seed + 4 + i * 4)
}
game_matrix[4, ] <- payoffs_A_market_price
game_matrix[6:10, 4] <- payoffs_B_market_price

# price skimming
payoffs_A_price_skim <- numeric(length = 5)
for (i in 1:5) {
  payoffs_A_price_skim[i] <- simulate_price_skimming(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(150), sigma = 0.5, elasticity = -1.5, seed = seed + i * 5)
}
payoffs_B_price_skim <- numeric(length = 5)
for (i in 1:5) {
  payoffs_B_price_skim[i] <- simulate_price_skimming(n_sim = n_sim, n_customers = 10000, base_price = 100, discount = 0.0, bundle_size = 2, mu = log(150), sigma = 0.5, elasticity = -1.5, seed = seed + 5 + i * 5)
}
game_matrix[5, ] <- payoffs_A_price_skim
game_matrix[6:10, 5] <- payoffs_B_price_skim

n_players <- 2
n_strategies <- 5
br <- list()
for (i in 1:n_players) {
  for (j in 1:n_strategies) {
    br_fun <- function(x) {
      opponent_strategies <- x[-i]
      my_strategy <- x[i]
      payoffs <- game_matrix[((i - 1) * n_strategies + j), ]
      my_payoff <- payoffs[my_strategy]
      opponent_payoffs <- payoffs[opponent_strategies]
      expected_opponent_payoff <- sum(opponent_payoffs) / (n_strategies - 1)
      c(my_payoff - expected_opponent_payoff)
    }
    br[[j + (i - 1) * n_strategies]] <- br_fun
  }
}


nash_eq <- GNE(br, rep(1 / n_strategies, n_players), approach = "minimization", tol = 1e-10, max_iter = 1000, verbose = TRUE, method = "Nelder-Mead", control = list(fnscale = -1))


print(game_matrix)
print(nash_eq)
