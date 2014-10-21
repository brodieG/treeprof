library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Miles Per Gallon"),

  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Cylinders" = "cyl",
                     "Transmission" = "am",
                     "Gears" = "gear")),

    checkboxInput("outliers", "Show outliers", FALSE)
  ),
  mainPanel(
    h3(textOutput("caption")),

    plotOutput("mpgPlot")
) ) )
