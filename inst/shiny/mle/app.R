library(shiny)
library(visualMLE)
shiny::shinyApp(ui = visualMLE:::shiny_ui,
				server = visualMLE:::shiny_server)
