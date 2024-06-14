if(require(webr)) { # This is running in the browser using webr
	webr::install("VisualStats", repos = c("https://visualstats.bryer.org/", "https://repo.r-wasm.org/"))
}
library(VisualStats)
library(shiny)
shinyApp(ui = anova_shiny_ui, server = anova_shiny_server)
