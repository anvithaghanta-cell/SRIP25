# Names: Anvitha Ghanta and Janhavi Lonari
# Last Updated: 07/08/2025
# Details: This is the UI file for the Shiny app made for MMI Textiles

# call the needed libraries
library(shiny)
library(googlesheets4)
library(ggplot2)

# this authenticates the service account JSON
gs4_auth(path = "the-service-account.json")

# here is the ID to the google sheet for the service account JSON to access it
sheet_id <- "15ixLLlQ4rz1dVibUBTokF5_hik6YnYoWsA9EzufGVL0"


server <- function(input, output, session) {
  # this makes it so that the data can be stored reactively
  vals <- reactiveValues(data = NULL)

  # Reload data when the Tolerance Graphs button is clicked.
  observeEvent(input$reload_tolerance,
    {
      vals$data <- read_sheet(sheet_id)
    },
    ignoreNULL = FALSE
  )

  # Reload data when the CPK Graphs button is clicked.
  observeEvent(input$reload_cpk,
    {
      vals$data <- read_sheet(sheet_id)
    },
    ignoreNULL = FALSE
  )

  # this creates the histogram on the CPK Graph tab
  output$mistPlot <- renderPlot({
    # requires these values to exist before proceeding
    req(vals$data, input$styleChoice_cpk)

    # Filter rows by the selected style.
    new_data <- vals$data[
      grepl(input$styleChoice_cpk, vals$data$Style, ignore.case = TRUE),
    ]

    # Keep only the columns containing the width measurements.
    other_data <- new_data[, c(
      "Width Result #1", "Width Result #2",
      "Width Result #3", "Width Result #4"
    )]

    # Combine the measurements into one numeric vector for plotting.
    flattened <- as.numeric(unlist(other_data))

    # this calculates mean and sigma using the long numeric vector
    mu <- mean(flattened, na.rm = TRUE) # removes missing values
    sigma <- sd(flattened, na.rm = TRUE) # removes missing values

    # Pull the tolerance bounds from the first matching row.
    usl <- as.numeric(new_data$Maximum[1])
    lsl <- as.numeric(new_data$Minimum[1])

    # this solves for CPK calculation
    cpu <- (usl - mu) / (3 * sigma)
    cpl <- (mu - lsl) / (3 * sigma)
    cpk <- min(cpu, cpl)
    cpk <- round(cpk, 3)

    # create and return the CPK histogram
    ggplot(data.frame(Value = flattened), aes(x = .data$Value)) +
      # plots the histogram bars
      geom_histogram(aes(y = after_stat(density)), # scale as density curve
        binwidth = 0.05, fill = "violet", color = "black"
      ) +
      # Draw the normal distribution density curve.
      stat_function(
        fun = dnorm, args = list(mean = mu, sd = sigma),
        color = "black", size = 1.2
      ) +
      # Draw the upper and lower bounds and the mean.
      geom_vline(
        xintercept = c(lsl, usl),
        color = "red",
        linetype = "dashed"
      ) +
      geom_vline(
        xintercept = mu,
        color = "black",
        linetype = "dotted",
        linewidth = 1.2
      ) +
      # creates the text label that shows CPK on the graph
      annotate("label",
        x = mu - 1, y = Inf, label = paste("CPK =", cpk),
        vjust = 2, hjust = 0.05, size = 5,
        color = "blue", fontface = "bold"
      ) +
      labs(
        # create labels for title, x-axis, and y-axis
        title = paste(input$styleChoice_cpk, "CPK Distribution"),
        x = "Edge Width (mm)",
        y = "Count"
      ) +
      # remove grid lines
      theme_minimal()
  })

  # this creates the histogram on the Tolerance Graph tab
  output$distPlot <- renderPlot({
    req(vals$data, input$styleChoice_tolerance)

    # Filter rows by the selected style.
    new_data <- vals$data[
      grepl(
        input$styleChoice_tolerance,
        vals$data$Style,
        ignore.case = TRUE
      ),
    ]

    # Then optionally filter by Loom only if selected
    if (!is.null(input$loom_tolerance) &&
          input$loom_tolerance != "Unselected") {
      new_data <- new_data[new_data$Loom == input$loom_tolerance, ]
    }

    # Keep only the columns containing the width measurements.
    other_data <- new_data[, c(
      "Width Result #1", "Width Result #2",
      "Width Result #3", "Width Result #4"
    )]

    # Combine the measurements into one numeric vector for plotting.
    flattened <- as.numeric(unlist(other_data))

    # Pull the tolerance bounds from the first matching row.
    usl <- as.numeric(new_data$Maximum[1])
    lsl <- as.numeric(new_data$Minimum[1])

    out_count <- sum(flattened < lsl | flattened > usl)

    paste("Out of Range", out_count)
    print(out_count)

    # create and return the tolerance levels histogram
    ggplot(data.frame(Value = flattened), aes(x = .data$Value)) +
      # plots the histogram bars
      geom_histogram(
        binwidth = 0.05,
        fill = "skyblue",
        color = "black",
        boundary = 0
      ) +
      # creates the lines for the upper and lower bound
      geom_vline(
        xintercept = c(lsl, usl),
        color = "red",
        linetype = "dashed"
      ) +
      labs(
        # create labels for title, x-axis, and y-axis
        title = paste(input$styleChoice_tolerance, "Tolerance Distribution"),
        x = "Edge Width (mm)",
        y = "Count"
      ) +
      theme_minimal()
  })

  # using the style column in the data this creates the dropdown choices
  observe({
    req(vals$data)
    updateSelectInput(
      session,
      "styleChoice_tolerance", # on the Tolerance Graphs tab
      choices = unique(vals$data$Style),
      selected = unique(vals$data$Style)[1]
    )
  })

  # using the style column in the data this creates the dropdown choices
  observe({
    req(vals$data)
    updateSelectInput(
      session,
      "styleChoice_cpk", # on the CPK Graphs tab
      choices = unique(vals$data$Style),
      selected = unique(vals$data$Style)[1]
    )
  })

  observe({
    req(vals$data, input$styleChoice_tolerance)

    # Filter data by selected Style
    filtered_data <- vals$data[vals$data$Style == input$styleChoice_tolerance, ]

    # Get unique Loom values for that Style
    loom_options <- unique(filtered_data$Loom)

    # Update Loom dropdown
    updateSelectInput(
      session,
      "loom_tolerance",
      choices = c("Unselected", loom_options),
      selected = loom_options[1]
    )
  })

  output$outCount <- renderText({
    req(vals$data, input$styleChoice_tolerance)

    # Filter the data based on selected style
    new_data <- vals$data[
      grepl(
        input$styleChoice_tolerance,
        vals$data$Style,
        ignore.case = TRUE
      ),
    ]

    if (!is.null(input$loom_tolerance) &&
          input$loom_tolerance != "Unselected") {
      new_data <- new_data[new_data$Loom == input$loom_tolerance, ]
    }

    # Extract the numeric measurement columns.
    other_data <- new_data[, c(
      "Width Result #1", "Width Result #2",
      "Width Result #3", "Width Result #4"
    )]

    # Flatten to single numeric vector
    flattened <- as.numeric(unlist(other_data))

    # Get LSL and USL
    usl <- as.numeric(new_data$Maximum[1])
    lsl <- as.numeric(new_data$Minimum[1])

    # Remove NAs
    flattened <- flattened[!is.na(flattened)]

    # Count in and out of range
    in_range <- sum(flattened >= lsl & flattened <= usl)
    out_range <- sum(flattened < lsl | flattened > usl)
    total <- length(flattened)

    # Display result
    paste0(
      "Total Points: ", total,
      " | In Range: ", in_range,
      " | Out of Range: ", out_range
    )
  })
}
