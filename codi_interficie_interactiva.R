
library(shiny)
library(shinyauthr)
library(dplyr)
library(randomForest)
library(lightgbm)
library(readr)
library(ggplot2)
library(tidyr)

datos <- read_csv("/Users/ainalluis/Desktop/dades.csv")

datos$id <- NULL
datos$...1 <- NULL

names(datos) <- gsub("/","_",names(datos))
names(datos) <- gsub("-","_",names(datos))
names(datos) <- gsub("\\s+","_",names(datos))

datos$Gender <- as.factor(datos$Gender)
datos$Customer_Type <- as.factor(datos$Customer_Type)
datos$Type_of_Travel <- as.factor(datos$Type_of_Travel)
datos$Class <- as.factor(datos$Class)
datos$satisfaction <- as.factor(datos$satisfaction)

datos <- na.omit(datos)

indices_prueba <- sample(1:nrow(datos), size = floor(0.2 * nrow(datos)))
datos_entrenamiento <- datos[-indices_prueba, ]
datos_prueba <- datos[indices_prueba, ]


data_matrix <- lgb.Dataset(data = as.matrix(datos_entrenamiento[, -ncol(datos_entrenamiento)]), 
                           label = as.integer(datos_entrenamiento$satisfaction) - 1,
                           categorical_feature = c("Gender", "Customer_Type", "Type_of_Travel", "Class"))

params <- list(objective = "multiclass",
               num_class = length(levels(datos$satisfaction)),
               metric = "multi_logloss")

modelo_lgbm <- lgb.train(params = params,
                         data = data_matrix,
                         nrounds = 100)

credentials <- data.frame(
  user = c("airline1", "airline2"),
  password = c("airline1", "airline2"),
  expire = c(NA, "2032-12-31"),
  admin = c(TRUE, FALSE),
  comment = "authentification mechanism",
  stringsAsFactors = FALSE,
  moreInfo = c("someData1", "someData2"),
  level = c(0,2)
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyauthr::loginUI("login"),
  uiOutput("app")
)

server <- function(input, output, session) {
  credentials <- shinyauthr::loginServer(
    "login",
    data = credentials,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  logout_init <- reactiveVal(FALSE)
  
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) {
      output$app <- renderUI({
        fluidPage(
          titlePanel("Predicció de la satisfacció del client"),
          sidebarLayout(
            sidebarPanel(
              div(class = "row",
                  div(class = "col-sm-6",
                      selectInput("Gender", "Gènere", choices = c("Male", "Female")),
                      selectInput("Customer_Type", "Tipus de Client", choices = c("Loyal Customer", "disloyal Customer")),
                      numericInput("Age", "Edad", value = 30, min = 7, max = 100, step = 1),
                      selectInput("Type_of_Travel", "Tipus de viatge", choices = c("Business travel", "Personal Travel")),
                      selectInput("Class", "Classe", choices = c("Eco", "Business", "Eco Plus")),
                      sliderInput("Flight_Distance", "Distància de vol", min = 0, max = 5000, value = 1000),
                      selectInput("Inflight_wifi_service", "Servei wifi a bord", choices = 0:5),
                      selectInput("Departure_Arrival_time_convenient", "Conveniència d'horari de sortida i arribada", choices = 0:5),
                      selectInput("Ease_of_Online_booking", "Facilitat de reserva online", choices = 0:5),
                      selectInput("Gate_location", "Ubicació de la porta", choices = 0:5),
                      selectInput("Food_and_drink", "Menjar i beguda", choices = 0:5)
                  ),
                  div(class = "col-sm-6",
                      selectInput("Online_boarding", "Embarcament online", choices = 0:5),
                      selectInput("Seat_comfort", "Comoditat del seient", choices = 0:5),
                      selectInput("Inflight_entertainment", "Entreteniment a bord", choices = 0:5),
                      selectInput("On_board_service", "Servei a bord", choices = 0:5),
                      selectInput("Leg_room_service", "Espai para a les cames", choices = 0:5),
                      selectInput("Baggage_handling", "Maneig d'equipatge", choices = 0:5),
                      selectInput("Checkin_service", "Servei de Check-in", choices = 0:5),
                      selectInput("Inflight_service", "Servei en el vol", choices = 0:5),
                      selectInput("Cleanliness", "Neteja", choices = 0:5),
                      numericInput("Departure_Delay_in_Minutes", "Retard en la sortida (Minuts)", value = 0, min = 0, max = 1200, step = 1),
                      numericInput("Arrival_Delay_in_Minutes", "Retard en l'arribada (Minuts)", value = 0, min = 0, max = 1200, step = 1)
                  )
              ),
              actionButton("predict", "Predir")
            ),
            mainPanel(
              textOutput("prediction"),
              plotOutput("probPlot"),
              tableOutput("probTable")
            )
          )
        )
      })
    }
  })
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      Gender = factor(input$Gender, levels = c("Male", "Female")),
      Customer_Type = factor(input$Customer_Type, levels = c("Loyal Customer", "disloyal Customer")),
      Age = as.numeric(input$Age),
      Type_of_Travel = factor(input$Type_of_Travel, levels = c("Business travel", "Personal Travel")),
      Class = factor(input$Class, levels = c("Eco", "Business", "Eco Plus")),
      Flight_Distance = as.numeric(input$Flight_Distance),
      Inflight_wifi_service = as.numeric(input$Inflight_wifi_service),
      Departure_Arrival_time_convenient = as.numeric(input$Departure_Arrival_time_convenient),
      Ease_of_Online_booking = as.numeric(input$Ease_of_Online_booking),
      Gate_location = as.numeric(input$Gate_location),
      Food_and_drink = as.numeric(input$Food_and_drink),
      Online_boarding = as.numeric(input$Online_boarding),
      Seat_comfort = as.numeric(input$Seat_comfort),
      Inflight_entertainment = as.numeric(input$Inflight_entertainment),
      On_board_service = as.numeric(input$On_board_service),
      Leg_room_service = as.numeric(input$Leg_room_service),
      Baggage_handling = as.numeric(input$Baggage_handling),
      Checkin_service = as.numeric(input$Checkin_service),
      Inflight_service = as.numeric(input$Inflight_service),
      Cleanliness = as.numeric(input$Cleanliness),
      Departure_Delay_in_Minutes = as.numeric(input$Departure_Delay_in_Minutes),
      Arrival_Delay_in_Minutes = as.numeric(input$Arrival_Delay_in_Minutes)
    )
    
    probabilities <- predict(modelo_lgbm, as.matrix(new_data))
    predictions <- as.data.frame(probabilities)
    
    long_predictions <- predictions %>%
      pivot_longer(cols = everything(), names_to = "Class", values_to = "Probability") %>%
      mutate(Probability = round(Probability * 100, 2),
             Probability = paste0(Probability, "%")) %>%
      arrange(desc(Probability))
    
    long_predictions$Class <- ifelse(long_predictions$Class == "V1", "Satisfied", "Neutral or Dissatisfied")
    
    output$prediction <- renderText({
      if(nrow(long_predictions) > 0) {
        predicted_class <- long_predictions$Class[1]
        paste("La predicció de satisfacció és:", predicted_class)
      } else {
        "No hi ha suficient informació per a fer una predicció"
      }
    })
    
    output$probPlot <- renderPlot({
      long_predictions <- long_predictions %>%
        mutate(Probability = as.numeric(gsub("%", "", Probability)))
      
      ggplot(long_predictions, aes(x = "", y = Probability, fill = Class)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        labs(x = NULL, y = NULL, fill = "Class", title = "Probability of Customer Satisfaction") +
        theme_void() +
        theme(legend.text = element_text(size = 12),
              legend.title = element_text(size = 14),
              plot.title = element_text(size = 16, hjust = 0.5))
    })
    
    output$probTable <- renderTable({
      long_predictions
    })
  })
}

shinyApp(ui, server)
