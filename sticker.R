library(hexSticker)
library(tidyverse)
library(VisualStats)
# source('mle_functions.R')
data(mtcars)

x <- mtcars$hp
y <- mtcars$wt

loglikelihood <- function(parameters, predictor, outcome) {
	a <- parameters[1]     # intercept
	b <- parameters[2]     # slope / beta coefficient
	sigma <- parameters[3] # error
	ll.vec <- dnorm(outcome, a + b * predictor, sigma, log = TRUE)
	return(sum(ll.vec))
}

optim.ll <- optim(
	runif(3),                     # Random initial values
	loglikelihood,                # Log-likelihood function
	lower = c(-Inf, -Inf, 1.e-5), # The lower bounds for the values, note sigma (error), cannot be negative
	method = "L-BFGS-B",
	control = list(fnscale = -1), # Indicates that the maximum is desired rather than the minimum
	predictor = x,
	outcome = y
)

intercept <- optim.ll$par[1]
slope <- optim.ll$par[2]
sigma <- optim.ll$par[3]
pt <- 22

height <- .4 # fraction of the width of the plot the distribution should take
k <- 4 # number of standard deviations to plot
xvals <- seq(-k * sigma, k * sigma, length.out = 50)
heightFactor <- height * diff(range(x, na.rm = TRUE)) / max(dnorm(xvals, 0, sigma) / dnorm(0, 0, sigma))
yvals <- dnorm(xvals, 0, sigma) / dnorm(0, 0, sigma) * heightFactor
x0 <- x[pt]
y0 <- slope * x0 + intercept
path <- data.frame(x = yvals + x0, y = xvals + y0)
segment <- data.frame(x = x0, y = y0 - k*sigma, xend = x0, yend = y0 + k*sigma)
segment2 <- data.frame(x = x0,
					   y = y[pt],
					   xend = dnorm(y0 - abs(y[pt]), 0, sigma) / dnorm(0, 0, sigma) * heightFactor + x0,
					   yend = y[pt])


p <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
	geom_point(size = 0.25) +
	geom_abline(intercept = intercept, slope = slope, alpha = 1, color = '#ff7f00') +
	geom_segment(data = segment, aes(x = x, y = y, xend = xend, yend = yend), alpha = 0.5) +
	geom_segment(data = segment2, aes(x = x, y = y, xend = xend, yend = yend), color = '#e31a1c') +
	geom_vline(xintercept = x0) +
	geom_point(data = data.frame(x = x[pt], y = y[pt]), color = '#e31a1c', size = 1) +
	geom_path(data = path, aes(x = x, y = y), color = "#33a02c") +
	theme_void()

p

hexSticker::sticker(p,
					filename = 'man/figures/VisualStats.png',
					p_size = 18,
					package = 'VisualStats',
					url = "github.com/jbryer/VisualStats",
					u_size = 5,
					s_width = 1.5, s_height = 1.2,
					s_x = 1, s_y = 0.79,
					p_x = 1, p_y = 1.49,
					p_color = "#FFFFFF",
					h_fill = '#a6cee3',
					h_color = '#1f78b4',
					white_around_sticker = FALSE)

