When working with distributions in R, each distribution has four
functions, namely:

-   `dXXX` - density function.
-   `rXXX` - generate random number from this distribution.
-   `pXXX` - returns the area to the left of the given value.
-   `qXXX` - returns the quantile for the given value.

Where `XXX` is the distribution name (e.g. `norm`, `binom`, `t`, etc.).

The `VisualStats::plot_distributions()` function will generate four
plots representing the four R distribution functions. For each subplot
points correspond to the first parameter of the corresponding function
(note the subplot for the random `rXXX` function does not have points
since this simply returns random values from that distribution). The
arrows correspond to what that function will return.

The top two plots (`dXXX` and `rXXX`) plot the distribution. The bottom
two plots are the cumulative density function for the given
distribution. The CDF describes the probability that a random variable
(X) will be less than or equal to a specific value (`x`), written as
F(x) = P(X ≤ x). The CDF provides a complete view of a random variable’s
distribution by accumulating probabilities up to that point.

### Available distributions

-   [Beta](https://en.wikipedia.org/wiki/Beta_distribution): `Xbeta`

-   [Binomial](https://en.wikipedia.org/wiki/Binomial_distribution):
    `Xbinom`

-   [Cauchy](https://en.wikipedia.org/wiki/Cauchy_distribution):
    `Xcauchy`

-   [Chi-Squared](https://en.wikipedia.org/wiki/Chi-squared_distribution):
    `Xchisq`

-   [Exponential](https://en.wikipedia.org/wiki/Exponential_distribution):
    `Xexp`

-   [F](https://en.wikipedia.org/wiki/F-distribution): `Xf`

-   [Gamma](https://en.wikipedia.org/wiki/Gamma_distribution): `Xgamma`

-   [Geometric](https://en.wikipedia.org/wiki/Geometric_distribution):
    `Xgeom`

-   [Hypergeometric](https://en.wikipedia.org/wiki/Hypergeometric_distribution):
    `Xhyper`

-   [Logistic](https://en.wikipedia.org/wiki/Logistic_distribution):
    `Xlogis`

-   [Log Normal](https://en.wikipedia.org/wiki/Log-normal_distribution):
    `Xlnorm`

-   [Negative
    Binomial](https://en.wikipedia.org/wiki/Negative_binomial_distribution):
    `Xnbinom`

-   [Normal](https://en.wikipedia.org/wiki/Normal_distribution): `Xnorm`

-   [Poisson](https://en.wikipedia.org/wiki/Poisson_distribution):
    `Xpois`

-   [Student
    t](https://en.wikipedia.org/wiki/Student%27s_t-distribution): `Xt`

-   [Uniform](https://en.wikipedia.org/wiki/Continuous_uniform_distribution):
    `Xunif`

-   [Weibull](https://en.wikipedia.org/wiki/Weibull_distribution):
    `Xweibull`

<!-- -->

    helptext <- help(dt, package = stats)
    tools:::Rd2txt(utils:::.getHelpFile(as.character(helptext)))
    db <- tools::Rd_db("stats")
    tmp <- lapply(db, tools:::.Rd_get_metadata, "description")
    tmp <- lapply(db, tools:::.Rd_get_metadata, "usage")
    tmp$Normal.Rd
