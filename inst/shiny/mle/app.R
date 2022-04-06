library(shiny)
library(VisualStats)
shiny::shinyApp(ui = VisualStats:::shiny_ui,
				server = VisualStats:::shiny_server)
