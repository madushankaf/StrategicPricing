library(GNE)

# Define the number of strategies for each player
dimx <- c(2, 2)

# Define the objective function for each player
# Define the payoff matrix for each player
payoff_matrix <- matrix(c(
    2, 0,
    3, 1,
    2, 3,
    0, 1
), nrow = 4, ncol = 2)


# Define the objective function for each player
# Define the objective function for each player
grobj <- function(x, i, j) {
    # Define the payoff matrix for the game
    payoff_matrix <- matrix(c(
        3, 1,
        0, 2,
        2, 1,
        0, 3
    ), nrow = 4, ncol = 2)

    # Calculate the objective function for player 1
    if (i == 1) {
        res <- c(
            payoff_matrix[1, 1] * (1 - x[1]) + payoff_matrix[2, 1] * x[1],
            payoff_matrix[1, 2] * (1 - x[2]) + payoff_matrix[2, 2] * x[2]
        )
    }

    # Calculate the objective function for player 2
    if (i == 2) {
        res <- c(
            payoff_matrix[3, 1] * (1 - x[1]) + payoff_matrix[4, 1] * x[1],
            payoff_matrix[3, 2] * (1 - x[2]) + payoff_matrix[4, 2] * x[2]
        )
    }

    # Print the objective function and return the j-th element
    
   # print(res[j])
    res[j]
}

print(payoff_matrix[1, 1])
print(payoff_matrix[2, 1])
print(payoff_matrix[1, 2])
print(payoff_matrix[2, 2])

print(payoff_matrix[3, 1])
print(payoff_matrix[4, 1])
print(payoff_matrix[3, 2])
print(payoff_matrix[4, 2])


# Define the Hessian of the objective function for each player
heobj <- function(x, i, j, k) {
    0
}

# Define the number of constraints for each player
dimlam <- c(1, 1)

# Define the constraint function for each player
g <- function(x, i) {
    if (i == 1) {
        res <- sum(x[1:2]) - 1
    }
    if (i == 2) {
        res <- sum(x[3:4]) - 1
    }
    res
}

# Define the gradient of the constraint function for each player
grg <- function(x, i, j) {
    if (i == 1) {
        res <- rep(1, 2)
    }
    if (i == 2) {
        res <- rep(1, 2)
    }
    res[j]
}

# Define the Hessian of the constraint function for each player
heg <- function(x, i, j, k) {
    0
}

# Set initial values for the variables
z0 <- rep(0, sum(dimx) + sum(dimlam))

# Solve the game using the Newton method
result <- GNE.nseq(z0, dimx, dimlam,
    grobj = grobj, NULL, heobj = heobj, NULL,
    constr = g, NULL, grconstr = grg, NULL, heconstr = heg, NULL,
    compl = phiFB, gcompla = GrAphiFB, gcomplb = GrBphiFB, method = "Broyden",
    control = list(trace = 1)
)

eq <- result$z[1:2]
print(result)
# Print the Nash equilibrium
cat("Nash equilibrium:", eq)
