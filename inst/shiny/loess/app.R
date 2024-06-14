if(require(webr)) { # This is running in the browser using webr
	webr::install("VisualStats", repos = c("https://visualstats.bryer.org/", "https://repo.r-wasm.org/"))
}
library(VisualStats)
library(shiny)
shinyApp(ui = loess_shiny_ui, server = loess_shiny_server)
