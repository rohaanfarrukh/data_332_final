# Data 332 Final | Basil & Rohaan

# Crime Analysis

## 📦 Libraries Used
```r
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
```
## 🧼 Data Cleaning: Boston Crime Dataset
```r
boston_raw <- read_excel("C:/Users/Rooooohan/Documents/data_final/data/boston_crime.xlsx")

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
```
## 🧼 Data Cleaning: Boston Crime Dataset
```r
chicago_raw <- read_csv("C:/Users/Rooooohan/Documents/data_final/data/chicago_crime.csv")

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
```
## 🔗 Combined Dataset
```r
combined_df <- bind_rows(
  boston_clean %>% select(Crime_Type, Description, Date, Latitude, Longitude, City),
  chicago_clean %>% select(Crime_Type, Description, Date, Latitude, Longitude, City)
)
```
## 🖼️ UI Design with navbarPage
```r
ui <- navbarPage("Crime Comparison: Boston vs Chicago",
  tabPanel("Home", ...),
  tabPanel("Project Scope", ...),
  tabPanel("Data View", ...),
  tabPanel("Boston Crimes", ...),
  tabPanel("Chicago Crimes", ...),
  tabPanel("City Comparison", ...),
  tabPanel("Boston Map", ...),
  tabPanel("Chicago Map", ...),
  tabPanel("Prediction Model", ...)
)
```
## 🧠 Server Logic: Data Tables & Visualizations
```r
output$table_output <- renderDT({
  datatable(head(combined_df, 100))
})

output$boston_crime_plot <- renderPlot({ ... })
output$boston_hour_plot <- renderPlot({ ... })
output$chicago_crime_plot <- renderPlot({ ... })
output$chicago_arrest_plot <- renderPlot({ ... })
output$compare_crime_plot <- renderPlot({ ... })
```
## 🗺️ Leaflet Maps for Geospatial Visualization
```r
output$boston_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = sample_n(boston_clean, 500),
                     ~Longitude, ~Latitude, radius = 2,
                     color = "blue", label = ~Crime_Type,
                     popup = ~Description)
})

output$chicago_map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = sample_n(chicago_clean, 500),
                     ~Longitude, ~Latitude, radius = 2,
                     color = "red", label = ~Crime_Type,
                     popup = ~Description)
})
```
## 🔍 Prediction Model: Decision Tree on Chicago Crimes
```r
chicago_model <- chicago_clean %>%
  mutate(Hour = hour(Date)) %>%
  select(Arrest, Crime_Type, Location, Domestic) %>%
  filter(!is.na(Arrest)) %>%
  mutate(across(everything(), as.factor))

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
```
## 🧪 User Input Prediction
```r
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
```
## 🚀 Launch the App
```r
shinyApp(ui = ui, server = server)
```
