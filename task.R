# setting correct seed
set.seed(1)
par(mfrow=c(2,2))

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
plot(x, y, main="Y X relation for variance = 0.25", xlab="X", ylab="Y", col="red") 

# point E
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

legend("topleft",
       legend = c("Data", "regretion line"),
       col = c("red", "blue"), lwd = 2, lty = 1, text.width = 1.7, y.intersp=2
)


# point G
plot(x, y, main="Y X relation with model x^2", xlab="X", ylab="Y", col="red") 
lines(line_x, line_y, pch = 18, col = "blue", type = "l")

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
       legend = c("Data", "linear coeffs", "model x^2"),
       col = c("red", "blue", "green"), lwd = 2, lty = 1, text.width = 1.7, y.intersp=2
)

# point H
x_H <- rnorm(100, 0, 1)
eps_H <- rnorm(100, 0, 0.1)
y_H <- -1.0 + 0.5 * x_H + eps_H

len_of_y_H <- length(y_H)
len_of_y_H # Length of vector y_H is 100

plot(x_H, y_H, main="Y X relation for variance = 0.1", xlab="X", ylab="Y", col="red") 

lin_reg_res_H <- lsfit(x_H, y_H)
beta_prim_0_H <- lin_reg_res_H$coefficients[[1]]
beta_prim_1_H <- lin_reg_res_H$coefficients[[2]]

#beta_prim_0_H: -1.0047452424192024889
#beta_prim_1_H: 0.49250511190754014956

line_x_H <- seq(-2.5, 2.5, 0.2)
line_y_H <- beta_prim_0_H + beta_prim_1_H * line_x_H
lines(line_x_H, line_y_H, pch = 18, col = "blue", type = "l")

legend("topleft", legend=c("data", "regression line"),
       col=c("red", "blue"), lwd=2, lty=1, text.width = 2.5, y.intersp=2)


# point I
x_I <- rnorm(100, 0, 1)
eps_I <- rnorm(100, 0, 0.5)
y_I <- -1.0 + 0.5 * x_I + eps_I

len_of_y_I <- length(y_I)
len_of_y_I # Length of vector y_I is 100

plot(x_I, y_I, main="Y X relation for variance = 0.5", xlab="X", ylab="Y", col="red") 

lin_reg_res_I <- lsfit(x_I, y_I)
beta_prim_0_I <- lin_reg_res_I$coefficients[[1]]
beta_prim_1_I <- lin_reg_res_I$coefficients[[2]]

#beta_prim_0_I: -1.0237262120960126666
#beta_prim_1_I: 0.46252555953770113639

line_x_I <- seq(-2.5, 2.5, 0.2)
line_y_I <- beta_prim_0_I + beta_prim_1_I * line_x_I
lines(line_x_I, line_y_I, pch = 18, col = "blue", type = "l")

legend("topleft", legend=c("data", "regression line"),
       col=c("red", "blue"), lwd=2, lty=1, text.width = 2.8, y.intersp=2)

# point J
# default confidence level of 95%
lm1 <- lm(y ~ x)
int1 <- confint(lm1)
lm2 <- lm(y_H ~ x_H)
int2 <- confint(lm2)
lm3 <- lm(y_I ~ x_I)
int3 <- confint(lm3)