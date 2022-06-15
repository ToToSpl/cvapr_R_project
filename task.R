# setting correct seed
set.seed(1)


# point A
x <- rnorm(100, 0, 1)

# point B
eps <- rnorm(100, 0, 0.25)

# point C
y <- -1.0 + 0.5 * x + eps

# vector y should have lenght 100, we can check it here:
len_of_y <- length(y)
len_of_y

# what are beta coeffs?
# beta0 is -1.0
# beta1 is 0.5
# TODO: CHECK IF THIS IS CORRECT! (chapter 6)

# point D
plot(x, y, main = "Relation between Y and X", xlab = "X", ylab = "Y", col = "red")


lin_reg_res <- lsfit(x, y)
beta_prim_0 <- lin_reg_res$coefficients[[1]]
beta_prim_1 <- lin_reg_res$coefficients[[2]]

# beta0: -1.00942315347694
# beta1: 0.499734903457018

# point F
# Add line
line_x <- seq(-2.5, 2.5, 0.1)
line_y <- beta_prim_0 + beta_prim_1 * line_x
lines(line_x, line_y, pch = 18, col = "blue", type = "l")

# legend("topleft",
#        legend = c("Data", "regretion line"),
#        col = c("red", "blue"), lwd = 2, lty = 1, text.width = 1
# )

# point G
poly_reg_res <- lm(y ~ x + I(x^2))
beta_prim_0_poly <- poly_reg_res$coefficients[[1]]
beta_prim_1_poly <- poly_reg_res$coefficients[[2]]
beta_prim_2_poly <- poly_reg_res$coefficients[[3]]
# beta0: -0.98582124777092983159
# beta1: 0.50429021861348155564
# beta2: -0.02973

# plot to compare linear regretion and quadratic
line_y <- beta_prim_0_poly + (beta_prim_1_poly + beta_prim_2_poly * line_x) * line_x
lines(line_x, line_y, pch = 18, col = "green", type = "l")
legend("topleft",
       legend = c("Data", "linear coeffs", "quadratic coeffs"),
       col = c("red", "blue", "green"), lwd = 2, lty = 1, text.width = 1.5
)
