setwd("C:/Users/Edwin/Documents/R Projects/Rshiny/KisumuHealthInfrastructure")
library("shiny")
library("tidyverse")
library(scales)
library(extrafont)
loadfonts(device = "win")
windowsFonts()

data <- read.csv("kisumu-county-health-infrastructure-2016_1.csv")
names(data) <- c("Tier", "Infrastructure", "Provider", "Numbers", "Date")
legendColors <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)


server <- function(input, output){
  output$plotDisplay <- renderPlot({
    
    data %>%
      filter(Tier == input$tier) %>%
      ggplot(aes(x=Provider, y=rescale(Numbers, to=c(1,input$theScale)), fill=Infrastructure)) +
        geom_bar(stat="identity", position="dodge") + 
        geom_text(aes(label=Numbers), vjust=1.5, colour="white", fontface="bold", position=position_dodge(.9), size=4) +
        ggtitle(input$tier) + 
        theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times New Roman", face="bold.italic", colour="black")) +
        scale_y_continuous(breaks=NULL) +  
        theme(axis.ticks = element_blank()) +
        theme(axis.text.x = element_text(family="Times New Roman", face="bold", colour="blue", size=rel(1.2), vjust=8)) +
        theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
        theme(axis.title.x=element_text(face="bold", size=14)) +
        ylab(paste0("Numbers (scaled 1:", input$theScale, ")")) +
        theme(axis.title.y=element_text(face="bold", size=14)) +
        theme(panel.border = element_blank(), axis.line = element_line(colour="black"), axis.line.x = element_blank()) +
        theme(panel.background = element_blank())+
        scale_fill_manual(values=legendColors)
  })
  
  output$plotTitle <- renderText({
    paste0(input$tier)
  })
  
  
  output$outputLink <- renderText({
    link = "https://data.humdata.org/dataset/kenya-health-infrastructure-in-kisumu-county/resource/f9a5ea25-30c5-4693-bd6d-04ca1510182d?view_id=d8cca7af-2566-4b81-aa9e-3d81255b1106"
    
    paste0('<b>Source:</b><a href="', link,'">https://data.humdata.org/dataset/kenya-health-infrastructure-in-kisumu-county/</a>')
  })  
}


ui <- fluidPage(
  tags$head(HTML("<link href='http://fonts.googleapis.com/css?family=Jura' rel='stylesheet' type='text/css'>")),
  
  h2("Health Infrastructure", style = "font-family: 'Jura'; color: green; font-size: 64px;"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("tier", "Tier", c("Primary Care facilities", "Referral Facilities")),
      sliderInput(inputId = "theScale",
                  label = "Scale",
                  min = 0,
                  max = 100,
                  value = 20,
                  sep = "",
                  step = 5
      )
    ),
  
  mainPanel(
    h3("Kisumu"),
    #HTML(paste0("<p><em>", {{ htmlOutput("plotTitle") }}, "</em></p>")),
    plotOutput("plotDisplay"),
    htmlOutput("outputLink")
  )    
  )
)



shinyApp(ui=ui, server=server)