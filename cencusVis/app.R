library(shiny)
library(ggplot2)
library(ggmap)
library(dplyr)


####ui

ui <- fluidPage(
  titlePanel("USA CENSUS VISUALIZATION", windowTitle = "CensusVis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 Census"),
      selectInput(inputId="var",label="Choose avariable to display",choices=list("Percent White","Percent Black","Percent Hispanic","Percent Asian"),
                  selected = "Percent White"
                  )
    ),
    mainPanel(
      textOutput(outputId = "selected_var"),
      plotOutput(outputId = "plot")
    )
  )
)


####server


server <- function(input, output){
  
  output$selected_var <- renderText(paste("You have selected", input$var))
  output$plot<-renderPlot({
    counties <- reactive({
      race=readRDS("data/counties.rds")
      counties_map=map_data("county")
      
      ## In order to join both table that we are combining them by both state and county as a unique identifier.
      ## SO we can combine region and subregion in counties_map into a new variable called name
      
      
      counties_map = counties_map %>%
        mutate(name=paste(region,subregion, sep=","))
      
      left_join(counties_map,race,by="name")
      
      #if names different, by = c("fn"="sn")
      
      
    })
    
    race = switch(input$var,
                  "Percent White"=counties()$white,
                  "Percent Black"=counties()$black,
                  "Percent Hispanic"=counties()$hispanic,
                  "Percent Asian"=counties()$asian
    )
    ggplot(counties(), aes(x=long, y=lat, fill=race, group=group))+
    geom_polygon(color="black")+
    scale_fill_gradient(low="white",high="red")
    })
  
}

#run app
shinyApp(ui,server)