library(shiny)
#install.packages("shinythemes")
library(shinythemes)
library(leaflet)
ui <- fluidPage(
    theme = shinytheme("united"),
    
    titlePanel("Earn Money As An AirBnb Host in Paris"),
    ### Side Bar Layout
    
    sidebarLayout(
        # Side bar Panel
        sidebarPanel(
            helpText("Enter These Values To Determine The Monthly Income for Your Airbnb"),
            
            # entire apt, private room, shared room
            selectInput(inputId = "type", 
                        label = "Select your listing type",
                        choices = c("Entire Apartment","Private Room","Shared Room")
            ),
            
            #zipcode
            selectInput(inputId = "zip",
                        label = "Zipcode",
                        choices = c("Please Select a Zipcode",
                                    "75001",
                                    "75002",
                                    "75003",
                                    "75004",
                                    "75005",
                                    "75006",
                                    "75007",
                                    "75008",
                                    "75009",
                                    "75010",
                                    "75011",
                                    "75012",
                                    "75013",
                                    "75014",
                                    "75015",
                                    "75016",
                                    "75017",
                                    "75018",
                                    "75019",
                                    "75020"),
                        selected = TRUE
            ),
            

            #Square Metre
            sliderInput(inputId="sq", 
                        label = "Square Meter ",
                        min = 10, max = 400,
                        value = 60,
                        width = '100%'),

            # cluster 
            selectInput(inputId = "clust",
                        label = "How often will you be renting your place?",
                        choices = c("Full-time (as often as possible)",
                                    "Regularily (1-2 weeks per month)",
                                    "Occasionally (less than a week per month)"
                        )
            ),
            
            # accomodates
            numericInput(inputId = "acc",
                         label = "Accomodates",
                         value = 2,
                         min = 1,
                         max = 12,
                         step = 1,
                         width = '100%'
            ),
            
            # bedrooms 
            numericInput(inputId = "bedroom",
                        label = "How many bedrooms does your place have?",
                        value = 1,
                        min = 1, max = 6, 
                        step = 1,
                        width = '100%'
            ),
            
            # bathrooms 
            numericInput(inputId = "bath", 
                         label = "How many bathrooms will guests have access to?", 
                         value = 1,
                         min = 1, max = 4.5,
                         step = 0.5
            ),
            
            # beds 
            numericInput(inputId = "bed", 
                         label = "How many beds will guests have access to?", 
                         value = 1,
                         min = 1, max = 10,
                         step = 1
            ),
            
            # isFamilyFriendly
            checkboxInput(inputId = "fam", 
                          label = "Is your place family friendly?", 
                          value = FALSE),
            
            # amenities 
            checkboxGroupInput(inputId = "amen",
                               label = "Select the amenities you will be including",
                               choices = c("TV","Shampoo","AC","Desk","Iron","HairDryer","Kitchen"),
                               inline = TRUE
                               
            ),
            

            
            #submitButton(text = "Calculate Monthly Income"),
            actionButton("Enter", "Calculate Potential Earnings"),
            
            h2("Monthly Income: "),

            textOutput("dynamicText")
            
        ), # Panel Stops Here
        
        
        mainPanel(
            leafletOutput("mymap",height = 750, width = 750)
        ),
        position = c("left")
    )
)
