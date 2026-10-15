# Names: Anvitha Ghanta and Janhavi Lonari
# Last Updated: 07/08/2025
# Details: This is the UI file for the Shiny app made for MMI Textiles

# call the needed libraries
library(shiny)
library(DT)
library(bslib)

# this defines the UI
ui <- navbarPage(
  title = div(
    tags$img(
      src = "logo.png",
      height = "100px",
      style = "margin-right:15px;"
    ), # Adds the logo
    "MMI Textiles Manufacturing Summary" # names the navigation bar
  ),
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    ) # links the stylesheet
  ),
  theme = bs_theme( # adds theme and color to the front end
    version = 5,
    bootswatch = "flatly",
    primary = "#C8102E",
    base_font = font_google("Gelasio"),
    heading_font = font_google("Lora")
  ),
  # this creates tab 1 "Introduction"
  tabPanel(
    "Introduction",
    mainPanel(
      h4("How to use this app:"),
      p("To be added"),
      p("wait 10 secs for graphs to load")
    )
  ),
  # this creates tab 2 "Tolerance Graphs"
  tabPanel(
    "Tolerance Graphs",
    sidebarLayout(
      sidebarPanel(
        actionButton("reload_tolerance", "Reload Data"),
        selectInput(inputId = "styleChoice_tolerance",
                    label = "Style:",
                    choices = "loading...",
                    selected = "loading..."),
        selectInput(
          inputId = "loom_tolerance",
          label = "Loom:",
          choices = "loading...",
          selected = "loading..."
        )
      ),
      mainPanel(
        plotOutput("distPlot"),
        h4(textOutput("outCount"))
      )
    )
  ),

  # this creates tab 3 "CPK Graphs"
  tabPanel(
    "CPK Graphs",
    sidebarLayout(
      sidebarPanel(
        actionButton("reload_cpk", "Reload Data"),
        selectInput(
          inputId = "styleChoice_cpk",
          label = "Style:",
          choices = "loading...",
          selected = "loading..."
        )
      ),
      mainPanel(
        plotOutput("mistPlot")
      )
    )
  ),

  # this creates tab 4 "Google Form"
  tabPanel(
    "Google Form",
    sidebarPanel(
      # Add sidebar content if needed
    ),
    mainPanel(
      div(
        style = "display: flex; justify-content: center;",
        tags$iframe(
          src = "https://tally.so/r/w4l7LA",
          width = "85%",
          height = "600px",
          frameborder = "0"
        )
      )
    )
  ),

  # this creates tab 5 "Tolerance and CPK Data"
  tabPanel(
    "Tolerance and CPK Data",
    mainPanel(
      DTOutput("sheet_table")
    )
  )
)
