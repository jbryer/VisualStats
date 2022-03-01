library(tidyverse)
library(gganimate)
library(visualMLE)
library(psych)
library(reshape2)
library(magick)

set.seed(1456)
x <- rnorm(15)
# rescale x to a specified mean/sd.
xdev <- x-(mean(x))
xz <- xdev/(sd(x))
x <- (xz*15)+100
xdf <- as.data.frame(x)
c('mean'=mean(xdf$x),'sd'=sd(xdf$x)) # double check

nllik1 <- function(x,par) {
	m=par[1]
	s=par[2]
	#n=length(x)
	-sum(dnorm(x, mean=m, sd=s, log=T))
}

set.seed(1465)
norm1 <- optim_save(c(85,10), nllik1, x=xdf$x)
it2_dfa <- norm1$iterations_df
headTail(it2_dfa)

fps <- 2
scale_range <- 2 # How many standard deviations around the mean should the x-axis include. This will use the smallest and largest mu from optim, not the raw data.

names(it2_dfa) <- c("Mu", "Sigma", "NegLogLikelihood", "Iteration")
dfa.melt <- melt(it2_dfa, id.var = 'Iteration')
names(dfa.melt) <- c("Iteration", "Parameter", "Value")
p2 <- ggplot(dfa.melt, aes(x = Iteration, y = Value, color = Parameter)) +
	geom_vline(aes(xintercept=Iteration), color="grey55") +
	geom_line()+
	facet_wrap(~ Parameter, scales = "free_y", ncol = 1) +
	xlab('Iteration') + ylab('Parameter Estimate') +
	scale_color_manual(values=c("steelblue2", "#E69F00", "black")) +
	geom_point(size=2) +
	theme(legend.position = 'none') +
	transition_reveal(Iteration)
p2_gif <- animate(p2, width = 480, height = 480, fps = fps, nframes = nrow(it2_dfa), renderer = magick_renderer())

max_sigma <- max(it2_dfa$Sigma)
min_mu <- min(it2_dfa$Mu)
max_mu <- max(it2_dfa$Mu)
xlim <- c(min_mu = min_mu - scale_range * max_sigma,
		  max_mu = max_mu + scale_range * max_sigma)
n_iter <- nrow(it2_dfa)
n_steps <- 100

dists <- data.frame(Iteration = rep(1:n_iter, each = n_steps),
					x = rep(seq(from = xlim[1], to = xlim[2], length = n_steps)),
					y = NA_real_)
for(i in 1:nrow(dists)) {
	dists[i,]$y <- dnorm(dists[i,]$x,
						 mean = it2_dfa[dists[i,]$Iteration,]$Mu,
						 sd = it2_dfa[dists[i,]$Iteration,]$Sigma)
}

ylim <- c(0, max(dists$y) * 1.05)


p1 <- ggplot(xdf, aes(x = x)) +
	geom_polygon(data = dists, aes(x = x, y = y, group = Iteration),
				 fill = '#E69F00', alpha = 0.1) +
	# Make this a function parameter
	geom_function(fun = dnorm, args = list(mean = it2_dfa[nrow(it2_dfa),]$Mu,
										   sd = it2_dfa[nrow(it2_dfa),]$Sigma),
				  color = 'steelblue2', alpha = 0.6) +
	geom_line(data = dists, aes(x = x, y = y, group = Iteration)) +
	geom_vline(data = it2_dfa, aes(xintercept = Mu), color = 'steelblue2') +
	geom_point(y = 0) +
	# expand_limits(y = 0) +
	ylim(ylim) + xlim(xlim) +
	ylab('') +
	theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
	transition_time(Iteration) +
	labs(title = "Iteration: {frame_time}")
p1_gif <- animate(p1, width = 480, height = 480, fps = fps, nframes = nrow(it2_dfa), renderer = magick_renderer())

new_gif <- image_append(c(p1_gif[1], p2_gif[1]))
for(i in 2:nrow(it2_dfa)){
	combined <- image_append(c(p1_gif[i], p2_gif[i]))
	new_gif <- c(new_gif, combined)
}
new_gif


