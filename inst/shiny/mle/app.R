if(require(webr)) { # This is running in the browser using webr
	webr::install("VisualStats", repos = c("https://visualstats.bryer.org/", "https://repo.r-wasm.org/"))
}
library(VisualStats)
library(shiny)
shiny::shinyApp(ui = mle_shiny_ui, mle_shiny_server)
