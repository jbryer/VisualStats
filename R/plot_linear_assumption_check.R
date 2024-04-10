#' Utility function that will return numeric columns used in the regression model.
#'
#' @export
get_numeric_vars <- function(glm_out) {
	numeric_vars <- unlist(lapply(glm_out$data, is.numeric))
	numeric_vars <- names(numeric_vars)[numeric_vars]
	numeric_vars <- numeric_vars[
		unlist(lapply(glm_out$data[,numeric_vars], FUN = function(x) { length(unique(x)) })) > 2]
	return(numeric_vars[numeric_vars %in% all.vars(glm_out$formula)[-1]])
}

#' Visual plot to check the assumption that the predicted values are linearly
#' related to the predictors.
#'
#' @param glm_out the results of the logistic regression (i.e. the result from
#'        \code{\link{glm}}).
#' @param vars character vector with the name of the independent variables to
#'        include. If omitted, all numeric variables will be used.
#' @param the number of groups to use. By default groups will have a minimum of
#'        six observations up to a maximum of 10 groups.
#' @param widths the relative widths of the two plots. If you only want the first
#'        plot, use \code{widths = c(0, 1)}; if only the second plot, use
#'        \code{widths = c(1, 0)}.
#' @return a ggplot2 expression.
#' @export
#' @examples
#' study <- data.frame(
#'     Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
#' 		       3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
#'     Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
#' )
#' lr.out <- glm(Pass ~ Hours, data = study,
#' 			  family = binomial(link='logit'))
#' plot_linear_assumption_check(glm_out = lr.out, n_groups = 5)
plot_linear_assumption_check <- function(glm_out,
										 n_groups,
										 vars = get_numeric_vars(glm_out),
										 # boxTidwell = TRUE,
										 # boxTidwell_digits = 3,
										 # boxTidwell_position_x,
										 # boxTidwell_position_y = .85,
										 na.rm = TRUE,
										 x_lab = 'Predicted Probabilities',
										 y_lab = all.vars(glm_out$formula)[1],
										 x_limits = c(0, 1),
										 coord_equal = TRUE,
										 coord_flip = FALSE,
										 widths = NULL,
										 legend.position = 'bottom',
										 varplot_nrow = NULL,
										 varplot_ncol = NULL) {
	boxTidwell <- FALSE # Not working reliability so leaving out for now.

	if(glm_out$family$family != 'binomial') {
		stop(paste0('This function is intended for logistic regression, not ',
					glm_out$family$family))
	}
	if(length(vars) == 0) {
		warning('No numeric variables found. Excluding variable plot.')
		widths <- c(1, 0)
	}
	if(missing(n_groups)) {
		n_groups <- min(floor(nrow(glm_out$data) / 6), 10)
	}
	if(is.null(x_limits)) {
		x_limits <- as.integer(c(NA, NA))
	}

	probs <- seq(0, 1, 1 / n_groups)

	predicted_prob <- predict(glm_out, type = 'response')

	y_var <- all.vars(glm_out$formula)[1]
	Y <- glm_out$data[,y_var, drop = TRUE]

	p2 <- NULL
	if(length(vars) > 0) {
		X <- as.data.frame(glm_out$data[,vars, drop = FALSE])

		X_melt <- reshape2::melt(
			cbind(X, predicted_prob = predicted_prob),
			id.var = 'predicted_prob'
		)

		p2 <- ggplot(X_melt, aes(x = predicted_prob, y = value, group = variable)) +
			geom_smooth(method = 'loess', formula = y ~ x) +
			geom_point(alpha = 0.4) +
			facet_wrap(~ variable,
					   scales = ifelse(coord_flip, "free_x", "free_y"),
					   nrow = varplot_nrow,
					   ncol = varplot_ncol) +
			xlim(x_limits) +
			ylab(NULL) + xlab(x_lab)
	}

	q <- quantile(predicted_prob, probs = probs, na.rm = na.rm)
	breaks <- cut(predicted_prob, breaks = q, include.lowest = TRUE)
	tab <- cbind(
		describe_by(Y, group = breaks, mat = TRUE, skew = FALSE)[,c('group1', 'mean', 'se')],
		x_pos = describe_by(predicted_prob, group = breaks, mat = TRUE)[,'median']
	)

	tab$ymin <- apply(tab[,c('mean', 'se')], 1, FUN = function(x) { max(x['mean'] - 1.96 * x['se'], 0) })
	tab$ymax <- apply(tab[,c('mean', 'se')], 1, FUN = function(x) { min(x['mean'] + 1.96 * x['se'], 1) })
	tab$color <- ifelse(tab$ymin <= tab$x_pos & tab$x_pos <= tab$ymax, 'p > 0.05', 'p ≤ 0.05')

	p1 <- ggplot(data.frame(predicted_prob = predicted_prob, Y = Y),
		   aes(x = predicted_prob, y = Y)) +
		geom_vline(xintercept = q, linetype = 2, alpha = 0.3) +
		geom_jitter(alpha = 0.4, width = 0, height = 0.05) +
		geom_segment(data = tab, aes(x = x_pos, xend = x_pos, y = ymin, yend = ymax, color = color), size = 4) +
		geom_point(data = tab, aes(x = x_pos, y = mean), color = 'white', size = 2) +
		geom_abline(slope = 1, intercept = 0, alpha = 0.3) +
		scale_color_manual(values = c('#e41a1c', '#377eb8'),
						   limits = c('p ≤ 0.05', 'p > 0.05')) +
		theme(legend.position = legend.position) +
		xlab(x_lab) + ylab(y_lab) +
		xlim(x_limits) #+ ylim(c(0, 1))

	if(length(vars) > 0 & boxTidwell) {
		numeric_vars <- get_numeric_vars(glm_out)
		df <- glm_out$data[,c(y_var, numeric_vars)]
		# out <- car::boxTidwell(as.formula(paste0(y_var, ' ~ ', paste0(numeric_vars, collapse = ' + '))),
		# 					   data = df) # Looking for a non-significant p-value
		vars
		out <- car::boxTidwell(y = glm_out$data[,y_var,drop=TRUE],
							   x1 = glm_out$data[,numeric_vars,drop=FALSE])
		txt <- 'Box-Tidwell Test'
		for(i in 1:length(out$result)) {
			txt <- paste0(txt, '\n', colnames(out$result)[i], ': ',
						  round(out$result[i], digits = boxTidwell_digits))
		}
		p1 <- p1 + annotate("label",
							x = ifelse(missing(boxTidwell_position_x),
									   max(c(x_limits, predicted_prob), na.rm = TRUE),
									   boxTidwell_position_x),
							y = boxTidwell_position_y,
							label = txt,
							hjust = 1)
	}

	if(coord_equal) {
		p1 <- p1 + coord_equal()
	}

	if(coord_flip) {
		p1 <- p1 + coord_flip()
		if(!is.null(p2)) {
			p2 <- p2 + coord_flip()
		}
	}

	if(!is.null(widths)) {
		if(widths[1] == 0) {
			return(p2)
		} else if(widths[2] == 0) {
			return(p1)
		}
	} else if(is.null(p2)) {
		return(p1)
	}

	egg::ggarrange(p1 + theme_vs(),
				   p2 + theme_vs(),
				   nrow = 1,
				   widths = widths)
}

if(FALSE) { # Testing
	library(ggplot2)

	data(mtcars)
	mtcars_glm_out <- glm(formula = vs ~ wt + disp, data = mtcars, family = binomial(link='logit'))
	summary(mtcars_glm_out)
	plot_linear_assumption_check(mtcars_glm_out)

	y_var <- all.vars(mtcars_glm_out$formula)[1]
	df <- mtcars_glm_out$data[,c(y_var, get_numeric_vars(mtcars_glm_out))]
	# df[,y_var] <- df[,y_var] + 1
	out <- car::boxTidwell(as.formula(paste0(y_var, ' ~ .')),
						   data = df) # Looking for a non-significant p-value

	boxTidwell(as.logical(vs) ~ cyl + disp + hp, ~as.factor(am) + poly(gear, 2),
			   data = mtcars)

	# This example is from OpenIntro, 4th edition beginning on page 371
	library(openintro)
	data(resume)
	glm_out1 <- glm(received_callback ~ job_city + college_degree + years_experience +
				          honors + military + has_email_address + race + gender + job_ad_id,
				   data = resume,
				   family = binomial(link = 'logit'))
	summary(glm_out1)
	plot_linear_assumption_check(glm_out1,
								 x_limits = NULL,
								 coord_equal = FALSE,
								 coord_flip = FALSE,
								 legend.position = 'none',
								 varplot_nrow = 2,
								 widths = c(1,2))

	plot_linear_assumption_check(glm_out1,
								 x_limits = NULL,
								 coord_equal = FALSE,
								 widths = c(1, 0))

	plot_linear_assumption_check(glm_out1,
								 x_limits = NULL,
								 coord_equal = FALSE,
								 widths = c(0, 1))

	plot_linear_assumption_check(glm_out1,
								 vars = c('years_experience'),
								 x_limits = NULL,
								 coord_equal = FALSE,
								 widths = c(0, 1))

	# http://www.statisticalassociates.com/logistic10.htm
	# The right-hand predictor side of the equation must be linear with the
	# left-hand outcome side of the equation. You must test for linearity in the
	# logit (in logistic regression the logit is the outcome side). This is
	# commonly done with the Box-Tidwell Transformation (Test): Add to the
	# logistic model interaction terms which are the crossproduct of each
	# independent times its natural logarithm [(X)ln(X)]. If these terms are
	# significant, then there is nonlinearity in the logit. This method is not
	# sensitive to small nonlinearities.
	y_var <- all.vars(glm_out1$formula)[1]
	df <- glm_out1$data[,c(y_var, get_numeric_vars(glm_out1))]
	out <- car::boxTidwell(as.formula(paste0(y_var, ' ~ .')),
					data = df) # Looking for a non-significant p-value

	glm_out2 <- glm(received_callback ~ job_city + years_experience +
						honors + military + has_email_address + race + gender,
					data = resume,
					family = binomial(link = 'logit'))
	summary(glm_out2)
	plot_linear_assumption_check(glm_out2,
								 x_limits = NULL,
								 coord_equal = FALSE,
								 legend.position = 'none',
								 varplot_nrow = 2,
								 widths = c(1,2))

	admit <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
	admit_glm_out <- glm(admit ~ gre + gpa + rank,
						 data = admit,
						 family = binomial(link = 'logit'))
	plot_linear_assumption_check(admit_glm_out, widths = c(1, 2))

}
