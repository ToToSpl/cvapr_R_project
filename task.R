# setting correct seed
set.seed(1)


# point A
x <- rnorm(100, 0, 1)

# point B
eps <- rnorm(100, 0, 0.25)

# point C
y <- -1.0 + 0.5 * x + eps

