library(shiny)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(lubridate)
library(rpart)
library(rpart.plot)


boston_raw <- read_excel("C:/Users/Rooooohan/Documents/data_final/boston_crime.xlsx", n_max = 5000)


boston_clean <- boston_raw %>%
  filter(YEAR == 2018) %>%
  select(
    OFFENSE_CODE_GROUP, OFFENSE_DESCRIPTION, DISTRICT,
    OCCURRED_ON_DATE, MONTH, DAY_OF_WEEK, HOUR,
    UCR_PART, Lat, Long
  ) %>%
  rename(
    Crime_Type = OFFENSE_CODE_GROUP,
    Description = OFFENSE_DESCRIPTION,
    Date = OCCURRED_ON_DATE,
    Latitude = Lat,
    Longitude = Long
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(
    Date = ymd_hms(Date),
    City = "Boston"
  )


chicago_raw <- read_csv("C:/Users/Rooooohan/Documents/data_final/chicago_crime.csv", n_max = 5000)


chicago_clean <- chicago_raw %>%
  select(`Primary Type`, Description, Date, `Location Description`,
         Arrest, Domestic, Latitude, Longitude) %>%
  rename(
    Crime_Type = `Primary Type`,
    Location = `Location Description`
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(
    City = "Chicago",
    Date = mdy_hms(Date)
  )


combined_df <- bind_rows(
  boston_clean %>% select(Crime_Type, Description, Date, Latitude, Longitude, City),
  chicago_clean %>% select(Crime_Type, Description, Date, Latitude, Longitude, City)
)



ui <- navbarPage("Crime Comparison: Boston vs Chicago",
                 
                 tabPanel("Home",
                          fluidPage(
                            titlePanel("Crime Comparison: Boston vs Chicago"),
                            p("This project explores the nature, frequency, and location of crimes in Boston and Chicago during the year 2018. 
        Using datasets from data.gov and Kaggle, we clean, compare, and visualize patterns using R Shiny. 
        We chose this topic because urban crime patterns give insight into public safety strategies and help cities better allocate resources.")
                          )
                 ),
                 
                 tabPanel("Project Scope",
                          fluidPage(
                            h3("Project Requirements & Backlog"),
                            p("This application requires us to join two datasets, create multiple charts, 
        implement a map, and optionally include a prediction model. Below are the project goals:"),
                            tags$ul(
                              tags$li("Clean and merge crime data from Boston and Chicago"),
                              tags$li("Visualize crime types, time patterns, and locations"),
                              tags$li("Compare city trends side by side"),
                              tags$li("Include a geospatial view using Leaflet"),
                              tags$li("Document code and maintain GitHub repo with Kanban")
                            )
                          )
                 ),
                 
                 tabPanel("Data View",
                          fluidPage(
                            h3("Sample of Combined Data"),
                            DTOutput("table_output")
                          )
                 ),
                 
                 tabPanel("Boston Crimes",
                          fluidPage(
                            h3("Top Crime Types in Boston"),
                            plotOutput("boston_crime_plot"),
                            h3("Hourly Crime Distribution in Boston"),
                            plotOutput("boston_hour_plot")
                          )
                 ),
                 
                 tabPanel("Chicago Crimes",
                          fluidPage(
                            h3("Top Crime Types in Chicago"),
                            plotOutput("chicago_crime_plot"),
                            h3("Arrest vs Non-Arrest Counts"),
                            plotOutput("chicago_arrest_plot")
                          )
                 ),
                 
                 tabPanel("City Comparison",
                          fluidPage(
                            h3("Crime Type Comparison Between Cities"),
                            plotOutput("compare_crime_plot")
                          )
                 ),
                 
                 tabPanel("Boston Map",
                          fluidPage(
                            h3("Crime Map: Boston (sample of 500 incidents)"),
                            leafletOutput("boston_map", height = 600)
                          )
                 ),
                 
                 tabPanel("Chicago Map",
                          fluidPage(
                            h3("Crime Map: Chicago (sample of 500 incidents)"),
                            leafletOutput("chicago_map", height = 600)
                          )
                 ),
                 tabPanel("Prediction Model",
                          fluidPage(
                            h3("Prediction: Will a Chicago Crime Lead to an Arrest?"),
                            verbatimTextOutput("model_summary"),
                            h5("Model Rationale & LLM Citation"),
                            p("We built a decision tree to predict arrests in Chicago using crime type and domestic flag."),
                            p("An LLM recommended logistic regression as a comparison model, citing a relevant study:"),
                            p(em("Mohanty, M., & Dey, L. (2022). Predicting arrest likelihood using logistic regression on urban crime datasets.")),
                            p("Since our dataset and features are similar, this validates our model choice."),
                            
                            br(), hr(), br(),
                            
                            h4("Try the Model Yourself"),
                            selectInput("user_crime", "Crime Type", choices = NULL),
                            selectInput("user_domestic", "Domestic Incident?", choices = c("TRUE", "FALSE")),
                            actionButton("predict_btn", "Predict"),
                            br(), br(),
                            h5("Prediction Result:"),
                            verbatimTextOutput("user_prediction")
                          )
                 )
)



server <- function(input, output) {
  
  output$table_output <- renderDT({
    datatable(head(combined_df, 100))
  })
  
  output$boston_crime_plot <- renderPlot({
    boston_clean %>%
      count(Crime_Type, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(Crime_Type, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "Crime Type", y = "Count", title = "Top 10 Crime Types in Boston")
  })
  
  output$boston_hour_plot <- renderPlot({
    boston_clean %>%
      count(HOUR) %>%
      ggplot(aes(x = HOUR, y = n)) +
      geom_line(color = "darkgreen") +
      labs(x = "Hour of Day", y = "Crime Count", title = "Hourly Crime Trends in Boston")
  })
  
  output$chicago_crime_plot <- renderPlot({
    chicago_clean %>%
      count(Crime_Type, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(Crime_Type, n), y = n)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      labs(x = "Crime Type", y = "Count", title = "Top 10 Crime Types in Chicago")
  })
  
  output$chicago_arrest_plot <- renderPlot({
    chicago_clean %>%
      count(Arrest) %>%
      ggplot(aes(x = Arrest, y = n, fill = Arrest)) +
      geom_col() +
      labs(x = "Arrest Made", y = "Count", title = "Arrest vs Non-Arrest in Chicago")
  })
  
  output$compare_crime_plot <- renderPlot({
    combined_df %>%
      count(City, Crime_Type) %>%
      group_by(City) %>%
      top_n(5, n) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(Crime_Type, n), y = n, fill = City)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(x = "Crime Type", y = "Count", title = "Top Crimes in Each City")
  })
  
  output$boston_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = sample_n(boston_clean, 500),
        ~Longitude, ~Latitude,
        radius = 2,
        color = "blue",
        label = ~Crime_Type,
        popup = ~Description
      )
  })
  
  output$chicago_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = sample_n(chicago_clean, 500),
        ~Longitude, ~Latitude,
        radius = 2,
        color = "red",
        label = ~Crime_Type,
        popup = ~Description
      )
  })
  
  
  chicago_model <- chicago_clean %>%
    mutate(Hour = hour(Date)) %>%
    select(Arrest, Crime_Type, Location, Domestic) %>%
    filter(!is.na(Arrest))
  
  
  chicago_model$Crime_Type <- as.factor(chicago_model$Crime_Type)
  chicago_model$Location <- as.factor(chicago_model$Location)
  chicago_model$Domestic <- as.factor(chicago_model$Domestic)
  chicago_model$Arrest <- as.factor(chicago_model$Arrest)
  
  
  set.seed(123)
  n <- nrow(chicago_model)
  train_idx <- sample(1:n, 0.8 * n)
  train_data <- chicago_model[train_idx, ]
  test_data <- chicago_model[-train_idx, ]
  
  
  model_tree <- rpart(Arrest ~ Crime_Type + Domestic,
                      data = train_data, method = "class")
  
  
  pred <- predict(model_tree, test_data, type = "class")
  conf_matrix <- table(Predicted = pred, Actual = test_data$Arrest)
  accuracy <- round(sum(diag(conf_matrix)) / sum(conf_matrix) * 100, 2)
  
  
  output$model_summary <- renderPrint({
    cat("Confusion Matrix:\n")
    print(conf_matrix)
    cat("\nModel Accuracy:", accuracy, "%")
  })
  observe({
    updateSelectInput(inputId = "user_crime", choices = levels(chicago_model$Crime_Type))
  })
  
  observeEvent(input$predict_btn, {
    
    user_input <- data.frame(
      Crime_Type = factor(input$user_crime, levels = levels(chicago_model$Crime_Type)),
      Domestic = factor(input$user_domestic, levels = c("TRUE", "FALSE"))
    )
    
    
    pred_user <- predict(model_tree, user_input, type = "class")
    
    output$user_prediction <- renderText({
      paste("The model predicts:", as.character(pred_user))
    })
  })
}


shinyApp(ui = ui, server = server)
