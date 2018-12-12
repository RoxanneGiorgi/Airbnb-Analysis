server <- function(input, output, session) {
    
    observeEvent(input$data,{
        output$data <- renderText({
            paste("zip:  ", input$zip)
        })
    })

    
    # Output Zipcode
    output$selected_var <- renderText({
        paste("Zipcode: ", input$var)
    })
    
    # Output Cluster Type
    output$selected_type <- renderText({
        paste("Property Type: ", input$type)
    })
    
    #Output Square Meter
    output$selected_integer = renderText({
        paste("Square Meter: ", input$integer)
    })
    
    #Output Price
    output$selected_price = renderText({
        paste("Price: ", input$price)
    })
    
    # Output Accomodates
    output$selected_accomodates = renderText({
        paste("Accomodates: ", input$accomodates)
    })
    

    ####### Predictions #######
    
    observeEvent(input$Enter,{
        zip = input$zip
        smeter = input$sq
        accom = input$acc
        clust = input$clust
        type = input$type
        bedr = input$bedroom
        bath = input$bath
        beds = input$bed
        amens = input$amen
        
        clusterNum = ifelse(clust == "Full-time (as often as possible)",3,
                            ifelse(clust == "Regularily (1-2 weeks per month)",2,
                                   ifelse(clust == "Occasionally (less than a week per month)",1,"ERROR")
                            )
                     )
        
        t = data.frame(zipcode = zip, accommodates = accom, bathrooms = bath, bedrooms = bedr, beds = beds, hasTV = 0, hasShampoo = 1, hasAC = 1, hasDesk = 1, hasIron = 1, hasHairDryer = 0, hasKitchen = 1, isFamilyFriendly = 1,square_meter = smeter, cluster = clusterNum)
        
        t$accommodates = as.numeric(t$accommodates)
        t$square_meter = as.numeric(t$square_meter)
        t$zipcode = as.factor(t$zipcode)
        t$cluster = as.factor(t$cluster)
        t$zipcode <- factor(t$zipcode, levels = levels(clusterInputs$zipcode))
        t$cluster <- factor(t$cluster, levels = levels(clusterInputs$cluster))
        
        
        appForest = randomForest(IncomePerMonth~., ntree=500, data=importantVars, importance=TRUE, do.trace = 100)
        
        output$dynamicText = renderPrint({
            paste("PREDICTION : ", predict(appForest,newdata = t))
        })
    })
    
    
    ############## Heatmap Code #################
    data <- reactive({
        x <- air2
    })
    
    output$mymap <- renderLeaflet({
        air2 = data()
        
        pal <- colorFactor(c("red", "green", "blue", "yellow", "orange", "mediumvioletred", 
                             "mediumpurple4", "navy","black", "palevioletred1", "lawngreen","lightpink3","sienna4","peru",
                             "brown","burlywood","darkgreen","goldenrod4","darkviolet","lightslateblue"), 
                           domain = c("75001", "75002","75003","75004","75005","75006","75007","75008",
                                      "75009","75010","75011","75012","75013","75014","75015","75016",
                                      "75017","75018","75019","75020"))
        labs <- as.list(air2$zipcode)
        m <- leaflet(data = air2) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude,
                             radius = 6,
                             label = lapply(labs, HTML),
                             color = ~pal(zipcode),
                             stroke = FALSE, fillOpacity = 0.5)
        
    })
    
    
}
