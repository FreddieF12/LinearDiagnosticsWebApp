library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Linear Diagnostics"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("sampleNameInputs"),
      
      numericInput(inputId = "threshold_value",
                   label = "CT Value Threshold:",
                   value = 10),
      
      uiOutput("sampleInputs"),
      
      actionButton(inputId = "add_sample",
                   label = "Add Sample")
    ),
    
    mainPanel(
      plotOutput(outputId = "fluorescencePlot"),
      tableOutput(outputId = "differencesTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  sample_counter <- reactiveVal(1)
  sample_names <- reactiveVal(list("Sample 1"))
  fluorescence_data <- reactiveVal(list(list("", "", "", "")))
  show_sample <- reactiveVal(list(TRUE))
  
  observeEvent(input$add_sample, {
    new_count <- sample_counter() + 1
    sample_counter(new_count)
    
    names <- sample_names()
    names <- append(names, paste("Sample", new_count))
    sample_names(names)
    
    data <- fluorescence_data()
    data <- append(data, list(list("", "", "", "")))
    fluorescence_data(data)
    
    show <- show_sample()
    show <- append(show, TRUE)
    show_sample(show)
  })
  
  output$sampleNameInputs <- renderUI({
    lapply(seq_along(sample_names()), function(i) {
      fluidRow(
        column(8, textInput(inputId = paste0("sample_name_", i),
                            label = paste("Sample Name", i, ":"),
                            value = sample_names()[[i]])),
        column(4, checkboxInput(inputId = paste0("show_sample_", i),
                                label = "Show in Graph",
                                value = show_sample()[[i]]))
      )
    })
  })
  
  observe({
    lapply(seq_along(sample_names()), function(i) {
      observeEvent(input[[paste0("sample_name_", i)]], {
        names <- sample_names()
        names[[i]] <- input[[paste0("sample_name_", i)]]
        sample_names(names)
      })
      observeEvent(input[[paste0("show_sample_", i)]], {
        show <- show_sample()
        show[[i]] <- input[[paste0("show_sample_", i)]]
        show_sample(show)
      })
    })
  })
  
  output$sampleInputs <- renderUI({
    lapply(seq_along(sample_names()), function(i) {
      tagList(
        h3(sample_names()[[i]]),
        textAreaInput(inputId = paste0("fluorescence_values1_", i),
                      label = "Paste Fluorescence Series 1 (each value on a new line):",
                      rows = 10,
                      value = fluorescence_data()[[i]][[1]]),
        textAreaInput(inputId = paste0("fluorescence_values2_", i),
                      label = "Paste Fluorescence Series 2 (each value on a new line):",
                      rows = 10,
                      value = fluorescence_data()[[i]][[2]]),
        textAreaInput(inputId = paste0("fluorescence_values3_", i),
                      label = "Paste Fluorescence Series 3 (each value on a new line):",
                      rows = 10,
                      value = fluorescence_data()[[i]][[3]]),
        textAreaInput(inputId = paste0("fluorescence_values4_", i),
                      label = "Paste Fluorescence Series 4 (each value on a new line):",
                      rows = 10,
                      value = fluorescence_data()[[i]][[4]])
      )
    })
  })
  
  observe({
    lapply(seq_along(sample_names()), function(i) {
      observeEvent(input[[paste0("fluorescence_values1_", i)]], {
        data <- fluorescence_data()
        data[[i]][[1]] <- input[[paste0("fluorescence_values1_", i)]]
        fluorescence_data(data)
      })
      observeEvent(input[[paste0("fluorescence_values2_", i)]], {
        data <- fluorescence_data()
        data[[i]][[2]] <- input[[paste0("fluorescence_values2_", i)]]
        fluorescence_data(data)
      })
      observeEvent(input[[paste0("fluorescence_values3_", i)]], {
        data <- fluorescence_data()
        data[[i]][[3]] <- input[[paste0("fluorescence_values3_", i)]]
        fluorescence_data(data)
      })
      observeEvent(input[[paste0("fluorescence_values4_", i)]], {
        data <- fluorescence_data()
        data[[i]][[4]] <- input[[paste0("fluorescence_values4_", i)]]
        fluorescence_data(data)
      })
    })
  })
  
  process_values <- reactive({
    lapply(seq_along(sample_names()), function(i) {
      lapply(1:4, function(j) {
        series <- fluorescence_data()[[i]][[j]]
        if (nchar(series) == 0) return(NULL)
        
        values <- strsplit(series, "\n")[[1]]
        values <- as.numeric(trimws(values))
        
        if (any(is.na(values))) return(NULL)
        
        time <- seq(0, by = 30, length.out = length(values))
        data.frame(Time = time, Fluorescence = values)
      })
    })
  })
  
  output$fluorescencePlot <- renderPlot({
    data <- process_values()
    show <- show_sample()
    
    if (all(sapply(unlist(data, recursive = FALSE), is.null))) {
      plot.new()
      title("Please enter valid numeric fluorescence values")
      return()
    }
    
    colors <- rainbow(length(sample_names()))
    plot(NULL, type = "n", xlab = "Time (seconds)", ylab = "Fluorescence", 
         xlim = c(0, max(sapply(unlist(data, recursive = FALSE), function(series) if (!is.null(series)) max(series$Time, na.rm = TRUE) else 0), na.rm = TRUE)), 
         ylim = range(unlist(lapply(unlist(data, recursive = FALSE), function(series) if (!is.null(series)) series$Fluorescence)), na.rm = TRUE), 
         main = "Fluorescence Time-based Values")
    
    for (i in seq_along(data)) {
      if (show[[i]]) {
        valid_data <- data[[i]][!sapply(data[[i]], is.null)]
        for (j in seq_along(valid_data)) {
          series <- valid_data[[j]]
          lines(series$Time, series$Fluorescence, type = "o", col = colors[i], lty = j, pch = j)
        }
      }
    }
    
    legend("topright", legend = sample_names()[unlist(show)], col = colors[unlist(show)], lty = 1:length(sample_names()), pch = 1:length(sample_names()))
  })
  
  output$differencesTable <- renderTable({
    data <- process_values()
    threshold <- input$threshold_value
    
    differences <- lapply(seq_along(data), function(i) {
      sample_data <- data[[i]]
      lapply(sample_data, function(series) {
        if (is.null(series)) return(data.frame(Max_Difference_Time = NA, Result = "Invalid Input"))
        
        diff_values <- c(NA, diff(series$Fluorescence))
        max_diff_time <- series$Time[which.max(abs(diff_values))]
        
        result <- if (max_diff_time > threshold) "Negative" else "Positive"
        data.frame(Max_Difference_Time = max_diff_time, Result = result)
      })
    })
    
    do.call(rbind, lapply(seq_along(differences), function(i) {
      sample_diffs <- do.call(rbind, differences[[i]])
      sample_diffs$Sample <- sample_names()[[i]]
      sample_diffs
    }))
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
