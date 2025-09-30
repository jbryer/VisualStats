if(require(webr)) { # This is running in the browser using webr
	webr::install("VisualStats", repos = c("https://visualstats.bryer.org/", "https://repo.r-wasm.org/"))
}
library(VisualStats)
library(shiny)
shinyApp(ui = distributions_shiny_ui, server = distributions_shiny_server)
