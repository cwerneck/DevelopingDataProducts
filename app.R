#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Read Data
library(readxl)

DataSet <- "PIB dos Municipios - base de dados 2010-2015.xls"

Brasil <- read_excel(DataSet, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

MinasGerais<-subset(Brasil, Brasil[2]==31, c(1,2,4,5,7,8,17,18,19,20))

names(MinasGerais)[1]<-"Year"
names(MinasGerais)[4]<-"Municipality"
names(MinasGerais)[5]<-"MesoReg"
names(MinasGerais)[7]<-"GDP"
names(MinasGerais)[8]<-"Population"
names(MinasGerais)[9]<-"GDP_per_capita"
names(MinasGerais)[10]<-"Industry"


Reg3101Data<-subset(MinasGerais, MinasGerais[5]==3101)
Reg3102Data<-subset(MinasGerais, MinasGerais[5]==3102)
Reg3110Data<-subset(MinasGerais, MinasGerais[5]==3110)


#print(str(Minas Gerais GDP))

# Define UI for application that draws a plot

ui <- fluidPage(
   
   # Application title
      
   titlePanel(title = h3("Minas Gerais(Brazil) GDP of municipalities ", align="center")),br(), br(),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(  
              #------------------------------------------------------------------
              # Add radio button to choice for north pole or south pole data
              radioButtons("MesoReg", 
                           label = "Select Region: ",
                           choices = list("Northwest" = '3101', "North" = '3102', "South/Southeast" = '3110'),
                           selected = '3110'),
              br(),   br(),
              #------------------------------------------------------------------
              # Add Variable for Year Selection
              sliderInput("YearRange", "Select Year Range : ", min=2010, max=2015, value=c(2010, 2012), step=1
                          
              ),      
              
              br(),   br()
              #------------------------------------------------------------------              
       ),
      
      # Show a plot of the generated distribution
      mainPanel(
              #------------------------------------------------------------------
              # Create tab panes
              tabsetPanel(type="tab",
                          tabPanel("Summary",verbatimTextOutput("sumry")),
                          tabPanel("Structure", verbatimTextOutput("struct")),
                          tabPanel("Data", tableOutput("displayData")),
                          tabPanel("Plot", plotOutput("mygraph"))
              )
              
              #------------------------------------------------------------------
      )
   )
)

# Define server logic required to draw a plot

server <- function(input, output) {
        cols <- reactive({
                as.numeric(c(input$var))
                
        })
        mylabel <- reactive({
                if(input$MesoReg=='3101'){
                        lable <- "Plot for Northwest Region Data"
                }
                if(input$MesoReg=='3102'){
                        lable <- "Plot for North Region Data"
                }
                if(input$MesoReg=='3110'){
                        lable <- "Plot for South/Spoutheast Region Data"
                }
                lable
        })
        
        
        myFinalData <- reactive({
                #------------------------------------------------------------------
                # Select data according to selection of ratdio button
                if(input$MesoReg=='3101'){
                        mydata <- Reg3101Data
                        
                }
                
                if(input$MesoReg=='3102'){
                        mydata <- Reg3102Data
                }
                
                if(input$MesoReg=='3110'){
                        mydata <- Reg3110Data
                }
                #------------------------------------------------------------------
                # Get data rows for selected year
                mydata1 <- mydata[mydata$Year >= input$YearRange[1], ] # From Year
                mydata1 <- mydata1[mydata1$Year <= input$YearRange[2], ] # To Year
                #------------------------------------------------------------------
                # Get Data for selected months as variable
                mydata2<- mydata1[, c(1,4,7,8,9, sort(cols()))]
                #------------------------------------------------------------------
                # Get data rows for selected year
                data.frame(mydata2)
                #------------------------------------------------------------------
                
        })
        
        # Prepare "Data tab"
        output$displayData <- renderTable({
                myFinalData()
        })
        
        # Prepare Structure Tab
        renderstr <- reactive({ str(myFinalData())})
        
        output$struct <- renderPrint({
                renderstr()
        })
        
        # Prepare Summary Tab
        rendersumry <- reactive({ summary(myFinalData())})
        
        output$sumry <- renderPrint({
                rendersumry()
        })
        
        # Prepare Plot Tab
        output$mygraph <- renderPlot({
                plotdata <- myFinalData()
                f<-ggplot(plotdata, aes(x=Population, y=GDP, size=GDP_per_capita, fill=Year)) +
                  geom_point(shape=21, colour = "grey")
                f<-f+ggtitle("Minas Gerais(Brazil) Municipalities") +
                        theme(plot.title = element_text(hjust = 0.5))+
                        theme(plot.caption = element_text(hjust = 0.5))+
                   labs(x = "Population (Inhab.)", y = "GDP R$1.000", caption="based on data from IBGE - Brazilian Institute of Geography and Statistics,\n brazilian agency responsible for official collection of statistical data \n at https://www.ibge.gov.br/estatisticas-novoportal/economicas/contas-nacionais/9088-produto-interno-bruto-dos-municipios.html?&t=resultados")
                
                f    
        })
        
        
  
}

# Run the application 
shinyApp(ui = ui, server = server)

