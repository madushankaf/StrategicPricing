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

n_sim <- 10

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

print(game_matrix)

n_players <- 2
n_strategies <- 5

compl <- function(x, lam) {
  n <- length(x)
  m <- length(lam)
  out <- rep(0, n + m)
  for (i in 1:n) {
    for (j in 1:m) {
      if (i == j) {
        out[i] <- out[i] + x[i] * lam[j]
      } else {
        out[i] <- out[i] + x[j] * game_matrix[i, j]
      }
    }
  }
  return(out)
}

heobj <- function(x, i, j, k) {
    2 * (i == j && j == k)
}


grobj <- function(x, i, j) {
  if (i == 1) {
    # Player 1's payoff function
    res <- (sum(game_matrix[1:n_strategies, ] * c(x[1:n_strategies], rep(1, n_strategies)))) # nolint
  } else {
    # Player 2's payoff function
    res <- (sum(game_matrix[(n_strategies + 1):(2 * n_strategies), ] * c(rep(1, n_strategies), x[(n_strategies + 1):(2 * n_strategies)]))) # nolint
  }
  res[j]
}

dimx <- rep(n_strategies, n_players)
dimlam <- rep(0, n_players)

z0 <- rep(0, sum(dimx) + sum(dimlam))

result <- GNE.nseq(z0, dimx, dimlam, grobj = grobj, heobj = heobj, compl = phiFB, gcompla = GrAphiFB, gcomplb = GrBphiFB, method = "Broyden", control = list(trace = 1))


eq <- result$z[1:n_strategies]
print(result)
# Print the Nash equilibrium
cat("Nash equilibrium:", eq)
