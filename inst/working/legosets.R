library(brickset)
library(dplyr)
library(ggplot2)
library(VisualStats)

data(legosets)

legosets23 <- legosets |>
	dplyr::filter(year == 2023 & availability == 'Retail') |>
	tidyr::drop_na(pieces, US_retailPrice)

ggplot(legosets23, aes(x = pieces, y = US_retailPrice)) +
	geom_point()

legosets23 |> dplyr::filter(US_retailPrice > 400)

