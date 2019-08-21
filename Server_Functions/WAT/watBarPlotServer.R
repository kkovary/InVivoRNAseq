WATdatasetInput <- reactive({
  watPlotData = filter(watTPM, GeneName %in% input$WATgenes) %>% mutate(FoldChange = TPM / mean(TPM[Day == 0]))
  watPlotData = rbind(filter(watPlotData, Day != 0),
                   mutate(filter(watPlotData, Day == 0), Delivery = 'injection'),
                   mutate(filter(watPlotData, Day == 0), Delivery = 'pellet'))
})

WATtableFormat <- reactive({
  filter(watTPM, GeneName %in% input$WATgenes) %>% mutate(FoldChange = TPM / mean(TPM[Day == 0]))
})

WATdatasetPlot <- reactive({
  
  my_comparisons <- list(c('1', '2'), c('1', '3'), c('1', '4'), c('2','5'), c('3','6'), c('4','7'))
  
  plotDat = WATdatasetInput() %>% mutate(order = c(rep(7,4),rep(5,4),rep(6,4),
                                                   rep(7,3),rep(5,4),rep(6,4),
                                                   rep(4,4),rep(2,4),rep(3,4),
                                                   rep(4,4),rep(2,4),rep(3,4),
                                                   rep(1,8))) %>%
    unite('Color', c('Treatment','Delivery'), sep = ' ', remove = F)
  
  plotDat$Delivery <- ordered(plotDat$Delivery, levels=c('pellet','injection'))
  plotDat$Color <- ordered(plotDat$Color, levels=c('Control pellet','Control injection',
                                                   'Placebo pellet', 'PBS injection',
                                                   'Cort pellet', 'Cort injection'))
  
  mypal = c('#000000', '#000000', '#f94040','#099963', '#0000ff','#ad07e3')
  
  ggbarplot(plotDat, x = "order", y = "FoldChange", add = "mean_sd", fill = "Color", color = 'Color',
            palette = mypal, position = position_dodge(0.8), facet.by = 'Delivery') + 
    scale_x_discrete(breaks = 1:7, labels = c('0','3','7','14','3','7','14')) + 
    xlab('Day') + ylab('Fold Change') +
    stat_compare_means( method = "t.test", size = 3, comparisons = my_comparisons)
  
})

WATplotDims <- reactive({
  c(as.numeric(input$WATwidth), as.numeric(input$WATheight))
})

# Plot Data  
output$WATplot <- renderPlot({
  WATdatasetPlot()
})

# Download PDF of plotted dataset ----
output$WATdownloadPlot <- downloadHandler(
  filename = function() {
    "Plot.pdf"
  },
  content = function(file){
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