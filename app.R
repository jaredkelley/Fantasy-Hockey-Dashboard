
# Load data and libraries -------------------------------------------------

library(shiny)
source("ui.R")
source("server.R")


# Create Shiny Application ------------------------------------------------


shinyApp(ui = ui, server = server)

library(rsconnect)
library(packrat)
rsconnect::deployApp('/Users/jaredkelley/Desktop/ R Projects/Shiny App/')
