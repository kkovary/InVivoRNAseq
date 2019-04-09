# sudo nano RNAseq-shiny-20180309-152352-41371.log
# sudo rm -r RNAseq
# sudo cp -r Desktop/app_180403.R /srv/shiny-server/apps/RNAseq/app.R
#  sudo systemctl restart shiny-server
# sudo su -     -c "R -e \"install.packages(c('ggplot2'), repos='http://cran.rstudio.com/')\""


library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

normalized_data_genelevel_tpm = read.csv(file = "normalized_data_genelevel_tpm.csv")

# Define UI ----
ui <- fluidPage(
  titlePanel(strong("In vivo RNA-seq Plots")),
  h6(em("Stefan Tholen, Kyle Kovary, Mary N. Teruel")),
  sidebarLayout(
    sidebarPanel(
      textOutput("result"),
      
      checkboxGroupInput("Condition", "Condition to Plot:",
                         c("Control_na" = "Control_na",
                           "Cort_injection" = "Cort_injection",
                           "Cort_pellet" = "Cort_pellet",
                           "PBS_injection" = "PBS_injection",
                           "Placebo_pellet" = "Placebo_pellet"),
                         selected = c("Control_na","Cort_injection","Cort_pellet","PBS_injection","Placebo_pellet"),
                         inline = TRUE),
      
      checkboxGroupInput("Day", "Time Points to Plot (Days):",
                         c("0" = 0,"3" = 3, "7" = 7, "14" = 14),
                         selected = c(0,3,7,14),
                         inline = TRUE),
      
      textInput("genes", "Genes (separate by space):", value = "Pparg"),
      
      
    h3(strong("Download")),
    
    # Download Plot Settings
    h5("PDF Dimensions"),
    splitLayout(
      textInput("width", "Width (in)", value = 10),
      textInput("height", "Height (in)", value = 5)
    ),
    
    downloadButton("downloadPlot", "Export plot as PDF"),
    
    # Download Data Settings
    downloadButton("downloadData", "Export table as CSV")
      
      
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    plot_data <- filter(normalized_data_genelevel_tpm, GeneName %in% unlist(strsplit(input$genes, " ")))
    #plot_data <- gather(plot_data, "sample", "TPM", 2:ncol(plot_data))
    #plot_data <- plot_data %>% separate(Sample, into = c("siRNA", "Day", "Replicate"), sep = "\\_")
    #plot_data$siRNA <- paste0("si",plot_data$siRNA)
    plot_data$Day <- as.numeric(plot_data$Day)
    plot_data <- filter(plot_data, Condition %in% input$Condition, Day %in% input$Day)
  })
  
  tableFormat <- reactive({
    tab <- datasetInput()
    tab <- select(tab, GeneName, sample, TPM)
    #tab <- spread(tab, key = sample, value = TPM)
    #tab <- unite(tab,"Day_Replicate", c("Day", "Replicate"))
    tab <- spread(tab,"sample","TPM")
    #tab <- unite(tab, "Condition", c("GeneName","siRNA"))
    mat <- t(tab[2:ncol(tab)])
    mat = cbind(colnames(tab[,2:ncol(tab)]),mat)
    colnames(mat) = c('Sample',as.character(tab$GeneName))
    mat = as.data.frame(mat)
    mat = separate(mat, col = Sample, into = c('Treatment','Delivery','Day','Replicate'))
    mat = unite(mat, col = 'Sample', c('Treatment','Delivery'), sep = '_')
    #rownames(mat) = c("Sample","Pparg")
    #mat <- as.data.frame(mat) %>% separate(Sample, into = c("Day","Replicate"), sep = "\\_")
  })
  

  
  datasetPlot <- reactive({
    ggplot(datasetInput(), aes(x = as.factor(Day), y = TPM)) + 
      geom_boxplot(aes(fill = GeneName), alpha = 0.5) + geom_point() + 
      theme_bw() + facet_grid(GeneName~Condition, scales = 'free') + xlab('Day')
    
  })
  
  plotDims <- reactive({
    c(as.numeric(input$width), as.numeric(input$height))
  })
  
  # Plot Data  
  output$plot <- renderPlot({
   
    datasetPlot()
    
  })
  
  # Download PDF of plotted dataset ----
  output$downloadPlot <-downloadHandler(
    filename = function() {
      "Plot.pdf"
    },
    content = function(file){
      #x = plotDims()
      renderPlot({
        datasetPlot()
      })
      ggsave(file, width = plotDims()[1], height = plotDims()[2], units = c('in'))
    }
  )
  
  # Table of selected dataset ----
  output$table <- renderTable({
    tableFormat()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$genes, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tableFormat(), file, row.names = FALSE)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
