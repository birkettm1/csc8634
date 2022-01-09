#install shiny
install.packages("shiny")
library("shiny")

#setup the app
source("src/ui.R")
source("src/server.R")
shinyApp(ui = ui, server = server)