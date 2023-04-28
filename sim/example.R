library(GNE)

#-------------------------------------------------------------------------------
# (1) Example 5 of von Facchinei et al. (2007)
#-------------------------------------------------------------------------------
dimx <- c(1, 1)
# Gr_x_j O_i(x)
grobj <- function(x, i, j) {
    if (i == 1) {
        res <- c(2 * (x[1] - 1), 0)
    }
    if (i == 2) {
        res <- c(0, 2 * (x[2] - 1 / 2))
    }
    print(res[j])
    res[j]
}
# Gr_x_k Gr_x_j O_i(x)
heobj <- function(x, i, j, k) {
    2 * (i == j && j == k)
}
dimlam <- c(1, 1)
# constraint function g_i(x)
g <- function(x, i) {
    sum(x[1:2]) - 1
}
# Gr_x_j g_i(x)
grg <- function(x, i, j) {
    1
}
# Gr_x_k Gr_x_j g_i(x)
heg <- function(x, i, j, k) {
    0
}
# true value is (3/4, 1/4, 1/2, 1/2)
z0 <- rep(0, sum(dimx) + sum(dimlam))

funSSR(z0, dimx, dimlam, grobj = grobj, constr = g, grconstr = grg, compl = phiFB, echo = FALSE)

jacSSR(z0, dimx, dimlam,
    heobj = heobj, constr = g, grconstr = grg,
    heconstr = heg, gcompla = GrAphiFB, gcomplb = GrBphiFB
)
GNE.nseq(z0, dimx, dimlam,
    grobj = grobj, NULL, heobj = heobj, NULL,
    constr = g, NULL, grconstr = grg, NULL, heconstr = heg, NULL,
    compl = phiFB, gcompla = GrAphiFB, gcomplb = GrBphiFB, method = "Newton",
    control = list(trace = 1)
)
GNE.nseq(z0, dimx, dimlam,
    grobj = grobj, NULL, heobj = heobj, NULL,
    constr = g, NULL, grconstr = grg, NULL, heconstr = heg, NULL,
    compl = phiFB, gcompla = GrAphiFB, gcomplb = GrBphiFB, method = "Broyden",
    control = list(trace = 1)
)
