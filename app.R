library(shiny)    
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # App title ----
    titlePanel("Real estate sales market in Gdynia"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar to demonstrate various slider options ----
        sidebarPanel(
            
    
    
                sliderInput("Rooms", "Enter number of rooms:", value = c(min(df$Number_of_rooms), 
                            max(df$Number_of_rooms)), min = min(df$Number_of_rooms), 
                            max = max(df$Number_of_rooms)),
                
                sliderInput("Surface", "Select the range of the flat area:", value = c(min(df$Surface), max(df$Surface)), 
                            min = min(df$Surface), max = max(df$Surface)),
                
                sliderInput("Floor", "Select the floor of the apartment:", value = c(min(df$Floor), max(df$Floor)), 
                            min = min(df$Floor), max = max(df$Floor)),
                
                radioButtons("heating", 
                             "Choose the method of heating the apartment you are looking for:",
                             c("Gas" = "Gas",
                               "Fireplace" = "Fireplace",
                               "Electric" = "Electric", 
                               "Heat pump" = "Heat pump", 
                               "Own boiler room" = "Own boiler room",
                               "District heating" = "District heating"
                              ),
                            ),
    
    
                selectInput("District",
                            "Where do you want to buy an apartment?",
                            c("Babie Doly" = "Babie Doly",
                              "Chwarzno-Wiczlino" = "Chwarzno-Wiczlino",
                              "Chylonia" = "Chylonia",
                              "Cisowa" = "Cisowa",
                              "Dabrowa" = "Dabrowa",
                              "Dzialki Lesne" = "Dzialki Lesne",
                              "Grabowek" = "Grabowek",
                              "Kamienna Gora" = "Kamienna Gora",
                              "Karwiny" = "Karwiny",
                              "Leszczynki" = "Leszczynki",
                              "Maly Kack" = "Maly Kack",
                              "Obluze" = "Obluze",
                              "Oksywie" = "Oksywie",
                              "Orlowo" = "Orlowo",
                              "Pogorze" = "Pogorze",
                              "Pustki Cisowskie-Demptowo" = "Pustki Cisowskie-Demptowo",
                              "Redlowo" = "Redlowo",
                              "Srodmiescie" = "Srodmiescie",
                              "Wielki Kack" = "Wielki Kack",
                              "Witomino" = "Witomino",
                              "Wzgorze sw. Maksymiliana" = "Wzgorze sw. Maksymiliana"
                              
                              ),
                
                multiple = FALSE,
                selectize = TRUE
                
                            )
                ),
    


    mainPanel(
        
        
        
                h2("Selection board:"),
                tableOutput("values"),
                
                hr(),
                br(),
                br(),
                
            
        
              )
         )
    )


server <- function(input, output, session) {
    

    apartments_df2 <- df

    
    filterData = reactiveVal(apartments_df2 %>% mutate(key = 1:nrow(apartments_df2)))
    
    sliderValues <- reactive({
        
        filtered <- filterData() %>% filter(Number_of_rooms >= input$Rooms[1] & Number_of_rooms <= input$Rooms[2])
        filtered <- filtered %>% filter(Surface >= input$Surface[1] & Surface <= input$Surface[2])
        filtered <- filtered %>% filter(Floor >= input$Floor[1] & Floor <= input$Floor[2])
        filtered <- filtered %>% filter(Heating_type %in% input$heating)
        filtered <- filtered %>% filter(District %in% input$District)
        filtered2 <- filtered %>% select(Price)
        filtered2 <- as.numeric(mean(filtered2$Price)) 
        filtered2 <- round(filtered2, 0)
        
      
      
        
        data.frame(
            Name = c("No. of rooms (min)",
                     "No. of rooms (max)",
                     "Surface in m2 (min)",
                     "Surface in m2 (max)",
                     "Floor (min)",
                     "Floor (max)",
                     "Heating",
                     "District",
                     "Average price"
                     ),
            Selected = as.character(c(input$Rooms,
                                   input$Surface,
                                   input$Floor,
                                   input$heating,
                                   input$District,
                                   ifelse((is.nan(filtered2)), "There are no apartments in the database that meet all the requirements. Please change filters and try again.", as.numeric(mean(filtered2))))),
            
            stringsAsFactors = FALSE)

            
                
    })
    
    
    output$values <- renderTable({
        sliderValues()
    })
  
    
}
# Run the application
shinyApp(ui = ui, server = server)
