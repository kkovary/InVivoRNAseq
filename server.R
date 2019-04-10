library(shiny)
library(tidyverse)
library(ggpubr)
#source('code/functions.R')

# Read in data
batTPM = read_csv('data/bat_normalized_data_genelevel_tpm.csv')
watTPM = read_csv("data/wat_normalized_data_genelevel_tpm.csv")

shinyServer(function(input, output) {
   
###### BAT Page Handling ######
  BATdatasetInput <- reactive({
    plotData = filter(batTPM, GeneName %in% input$BATgenes) %>% mutate(FoldChange = TPM / mean(TPM[Day == 0]))
    plotData = rbind(filter(plotData, Day != 0),
                     mutate(filter(plotData, Day == 0), Delivery = 'injection'),
                     mutate(filter(plotData, Day == 0), Delivery = 'pellet'))
  })
  
  BATtableFormat <- reactive({
    filter(batTPM, GeneName %in% input$BATgenes) %>% mutate(FoldChange = TPM / mean(TPM[Day == 0]))
  })
  
  BATdatasetPlot <- reactive({
    ggplot(BATdatasetInput(), aes(y = log2(FoldChange),x = Treatment, fill = Treatment)) + geom_boxplot(alpha = 0.5) +
      facet_grid(Delivery ~ as.numeric(Day)) + theme_bw() + stat_compare_means(method = "t.test",size = 3) +
      ylab('Log2 Fold Change TPM') + ggtitle(input$BATgenes)
  })
  
  BATplotDims <- reactive({
    c(as.numeric(input$BATwidth), as.numeric(input$BATheight))
  })
  
  # Plot Data  
  output$BATplot <- renderPlot({
    BATdatasetPlot()
  })
  
  # Download PDF of plotted dataset ----
  output$BATdownloadPlot <- downloadHandler(
    filename = function() {
      "Plot.pdf"
    },
    content = function(file){
      renderPlot({
        BATdatasetPlot()
      })
      ggsave(file, width = BATplotDims()[1], height = BATplotDims()[2], units = c('in'))
    }
  )
  
  # Table of selected dataset ----
  output$BATtable <- renderTable({
    BATtableFormat()
  })
  
  # Downloadable csv of selected dataset ----
  output$BATdownloadData <- downloadHandler(
    filename = function() {
      paste(input$BATgenes, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BATtableFormat(), file, row.names = FALSE)
    }
  )
  
  
###### WAT Page Handling ######
  # Reactive value for selected dataset ----
  WATdatasetInput <- reactive({
    plot_data <- filter(watTPM, GeneName %in% unlist(strsplit(input$WATgenes, " ")))
    plot_data$Day <- as.numeric(plot_data$Day)
    plot_data <- filter(plot_data, Condition %in% input$Condition, Day %in% input$Day)
  })
  
  WATtableFormat <- reactive({
    tab <- WATdatasetInput()
    tab <- select(tab, GeneName, sample, TPM)
    tab <- spread(tab,"sample","TPM")
    mat <- t(tab[2:ncol(tab)])
    mat = cbind(colnames(tab[,2:ncol(tab)]),mat)
    colnames(mat) = c('Sample',as.character(tab$GeneName))
    mat = as.data.frame(mat)
    mat = separate(mat, col = Sample, into = c('Treatment','Delivery','Day','Replicate'))
    mat = unite(mat, col = 'Sample', c('Treatment','Delivery'), sep = '_')
  })
  
  
  
  WATdatasetPlot <- reactive({
    ggplot(WATdatasetInput(), aes(x = as.factor(Day), y = TPM)) + 
      geom_boxplot(aes(fill = GeneName), alpha = 0.5) + geom_point() + 
      theme_bw() + facet_grid(GeneName~Condition, scales = 'free') + xlab('Day')
    
  })
  
  WATplotDims <- reactive({
    c(as.numeric(WATinput$width), as.numeric(WATinput$height))
  })
  
  # Plot Data  
  output$WATplot <- renderPlot({
    
    WATdatasetPlot()
    
  })
  
  # Download PDF of plotted dataset ----
  output$WATdownloadPlot <-downloadHandler(
    filename = function() {
      "Plot.pdf"
    },
    content = function(file){
      #x = plotDims()
      renderPlot({
        WATdatasetPlot()
      })
      ggsave(file, width = WATplotDims()[1], height = WATplotDims()[2], units = c('in'))
    }
  )
  
  # Table of selected dataset ----
  output$WATtable <- renderTable({
    WATtableFormat()
  })
  
  # Downloadable csv of selected dataset ----
  output$WATdownloadData <- downloadHandler(
    filename = function() {
      paste(input$WATgenes, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(WATtableFormat(), file, row.names = FALSE)
    }
  )
  
})
