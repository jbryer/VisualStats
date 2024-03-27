#' Basic summary statistics by group
#'
#' This is a re-implementation of the [psych::describeBy] function using the `dplyr`
#' package for creating the resulting table. The goal is for this function to be
#' a drop-in replacement, however the results are always returned as a data.frame
#' consistent with the `mat = TRUE` parameter setting.
#'
#' The `psych` package is an incredibly useful package but some of the dependencies
#' are problematic for deploying Shiny applications using `shinylive`. Since this
#' package only uses the `describeBy` function from the `psych` package, we
#' re-implemented it in order to remove the dependency on the `psych` package.
#'
#' @param x a numeric vector to summarize.
#' @param group a grouping vector.
#' @param mat not used, provided to provide direct compatability with [psych::describeBy].
#' @param type type of skewness, see [psych::skew] for details.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end
#'        of x before the mean is computed. Values of trim outside that range are
#'        taken as the nearest endpoint.
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values
#'        should be stripped before the computation proceeds.
#' @param ... not currently used. Included to ensure compatibility.
#' @return a data.frame with the summary statistic.
#' @importFrom rlang sym
#' @importFrom dplyr group_by summarize n ungroup relocate mutate
#' @export
describe_by <- function(x,
						group = NULL,
						type = 3,
						trim = 0.1,
						na.rm = TRUE,
						...) {
	n_groups <- 0
	df <- data.frame(x = x,
			   stringsAsFactors = FALSE)
	if(is.list(group)) {
		n_groups <- length(group)
		for(i in seq_len(n_groups)) {
			df[,paste0('group', i)] <- group[[i]]
		}
	} else if(is.vector(group)) {
		df$group1 <- group
		n_groups <- 1
	} else if(!is.null(group)) {
		stop('group variable must be a vector or list.')
	}

	if(n_groups > 0) {
		df <- df |> dplyr::group_by(across(all_of(
			paste0('group', seq_len(n_groups))
		)))
		# df <- df |> dplyr::group_by(!!rlang::sym(
		# 	paste0('group', seq_len(n_groups))
		# ))
	}

	df |>
		dplyr::summarize(
			n = dplyr::n(),
			mean = mean(x, na.rm = na.rm),
			sd = sd(x, na.rm = na.rm),
			median = median(x, na.rm = na.rm),
			trimmed = mean(x, trim = trim, na.rm = na.rm),
			mad = mad(x, na.rm = na.rm),
			min = min(x, na.rm = na.rm),
			max = max(x, na.rm = na.rm),
			range = diff(range(x, na.rm = na.rm)),
			skew = skew(x, na.rm = na.rm, type = type),
			kurtosis = kurtosi(x, na.rm = na.rm, type = type),
			se = sd(x, na.rm = na.rm) / dplyr::n(),
			IQR = IQR(x, na.rm = na.rm)
		) |>
		dplyr::ungroup() |>
		dplyr::mutate(item = 1:dplyr::n()) |>
		dplyr::relocate(item)
}

if(FALSE) {
	data(mtcars)
	describe_by(mtcars$mgp)

	psych::describeBy(mtcars$mpg, group = mtcars$cyl, mat = TRUE, IQR = TRUE)
	describe_by(mtcars$mpg, group = mtcars$cyl)

	psych::describeBy(mtcars$mpg, group = list(mtcars$cyl, mtcars$am), mat = TRUE, IQR = TRUE)
	describe_by(mtcars$mpg, group = list(mtcars$cyl, mtcars$am))

	# Examples from the describeBy documentation
	data(sat.act)
	describeBy(sat.act, group = sat.act$gender) #just one grouping variable
	describeBy(SATV + SATQ ~ gender, data = sat.act)  #specify the data set if using formula

}

#' Copied from the psych package.
skew <- function (x, na.rm = TRUE, type = 3) {
	if (length(dim(x)) == 0) {
		if (na.rm) {
			x <- x[!is.na(x)]
		}
		sdx <- sd(x, na.rm = na.rm)
		mx <- mean(x)
		n <- length(x[!is.na(x)])
		switch(type, {
			skewer <- sqrt(n) * (sum((x - mx)^3, na.rm = na.rm) /
								 	(sum((x - mx)^2, na.rm = na.rm)^(3/2)))
		}, {
			skewer <- n * sqrt(n - 1) * (sum((x - mx)^3, na.rm = na.rm) /
									((n - 2) * sum((x - mx)^2, na.rm = na.rm)^(3/2)))
		}, {
			skewer <- sum((x - mx)^3)/(n * sd(x)^3)
		})
	} else {
		skewer <- rep(NA, dim(x)[2])
		if (is.matrix(x)) {
			mx <- colMeans(x, na.rm = na.rm)
		}
		else {
			mx <- apply(x, 2, mean, na.rm = na.rm)
		}
		sdx <- apply(x, 2, sd, na.rm = na.rm)
		for (i in 1:dim(x)[2]) {
			n <- length(x[!is.na(x[, i]), i])
			switch(type, {
				skewer[i] <- sqrt(n) * (sum((x[, i] - mx[i])^3, na.rm = na.rm) /
										(sum((x[, i] - mx[i])^2, na.rm = na.rm)^(3/2)))
			}, {
				skewer[i] <- n * sqrt(n - 1) *
					(sum((x[, i] - mx[i])^3, na.rm = na.rm) /
					 	((n - 2) * sum((x[, i] - mx[i])^2, na.rm = na.rm)^(3/2)))
			}, {
				skewer[i] <- sum((x[, i] - mx[i])^3, na.rm = na.rm) /
					(n * sdx[i]^3)
			})
		}
	}
	return(skewer)
}

#' Copied from the psych package
kurtosi <- function (x, na.rm = TRUE, type = 3) {
	if (length(dim(x)) == 0) {
		if (na.rm) {
			x <- x[!is.na(x)]
		}
		if (is.matrix(x)) {
			mx <- colMeans(x, na.rm = na.rm)
		}
		else {
			mx <- mean(x, na.rm = na.rm)
		}
		sdx <- sd(x, na.rm = na.rm)
		n <- length(x[!is.na(x)])
		switch(type, {
			kurt <- sum((x - mx)^4, na.rm = na.rm) * n /
				(sum((x - mx)^2, na.rm = na.rm)^2) - 3
		}, {
			kurt <- n * (n + 1) * sum((x - mx)^4, na.rm = na.rm) /
				((n - 1) * (n - 2) * (n - 3) * (sum((x - mx)^2, na.rm = na.rm)/(n -
																   															  	1))^2) - 3 * (n - 1)^2/((n - 2) * (n - 3))
		}, {
			kurt <- sum((x - mx)^4)/(n * sdx^4) - 3
		})
	} else {
		kurt <- rep(NA, dim(x)[2])
		mx <- apply(x, 2, mean, na.rm = na.rm)
		if (type == 3)
			sdx <- apply(x, 2, sd, na.rm = na.rm)
		for (i in 1:dim(x)[2]) {
			n <- length(x[!is.na(x[, i]), i])
			switch(type, {
				kurt[i] <- sum((x[, i] - mx[i])^4, na.rm = na.rm) *
					length(x[, i])/(sum((x[, i] - mx[i])^2, na.rm = na.rm)^2) -
					3
			}, {
				xi <- x[, i] - mx[i]
				kurt[i] <- n * (n + 1) * sum((x[, i] - mx[i])^4, na.rm = na.rm) /
					((n - 1) * (n - 2) * (n - 3) * (sum((x[, i] - mx[i])^2, na.rm = na.rm) /
											(n -  1))^2) - 3 * (n - 1)^2/((n - 2) * (n - 3))
			}, {
				kurt[i] <- sum((x[, i] - mx[i])^4, na.rm = na.rm) /
					((length(x[, i]) - sum(is.na(x[, i]))) * sdx[i]^4) - 3
			}, {
				NULL
			})
			names(kurt) <- colnames(x)
		}
	}
	return(kurt)
}

