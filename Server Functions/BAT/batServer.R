
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
    
    my_comparisons <- list(c('1', '2'), c('1', '3'), c('1', '4'), c('2','5'), c('3','6'), c('4','7'))
    
    plotDat = BATdatasetInput() %>% mutate(order = c(rep(7,4),rep(5,4),rep(6,4),
                                                     rep(7,4),rep(5,4),rep(6,4),
                                                     rep(4,4),rep(2,4),rep(3,4),
                                                     rep(4,4),rep(2,4),rep(3,4),
                                                     rep(1,8))) %>%
      unite('Color', c('Treatment','Delivery'), sep = ' ', remove = F)
    
    plotDat$Delivery <- ordered(plotDat$Delivery, levels=c('pellet','injection'))
    plotDat$Color <- ordered(plotDat$Color, levels=c('Control pellet','Control injection',
                                                     'Sham pellet', 'Sham injection',
                                                     'Cort pellet', 'Cort injection'))
    
    mypal = c('#000000', '#000000', '#f94040','#099963', '#0000ff','#ad07e3')
    
    ggbarplot(plotDat, x = "order", y = "FoldChange", add = "mean_sd", fill = "Color", color = 'Color',
              palette = mypal, position = position_dodge(0.8), facet.by = 'Delivery') + 
      scale_x_discrete(breaks = 1:7, labels = c('0','3','7','14','3','7','14')) + 
      xlab('Day') + ylab('Fold Change') +
      stat_compare_means( method = "t.test", size = 3, comparisons = my_comparisons)
    
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
  
  ######################
  #### Volcano Plot ####
  ######################
  
  volPlotGroups <- reactive({
    if(length(input$volGroups) > 0){
      volConditions = batTPM %>% filter(!duplicated(sample)) %>% dplyr::select(sample, Treatment, Delivery, Day) %>%
        unite('Group', input$volGroups, remove = F)
      
      volConditions = volConditions$Group %>% unique() %>% as.vector()
    } else{
      volConditions = NA
    }
    
  })
  
  observe({
    updateSelectInput(session, 'volNumerator',
                      choices = volPlotGroups(),
                      selected = NA)
    
    updateSelectInput(session, 'volDenominator',
                      choices = volPlotGroups(),
                      selected = NA)
  })
  
  volGroups <- eventReactive(input$volPlotButton, {input$volGroups})
  volNumerator <- eventReactive(input$volPlotButton, {input$volNumerator})
  volDenominator <- eventReactive(input$volPlotButton, {input$volDenominator})
  fcCut <- eventReactive(input$volPlotButton, {input$fcCut})
  pvalCut <- eventReactive(input$volPlotButton, {input$pvalCut})
  pChoice <- eventReactive(input$volPlotButton, {input$pChoice})
  
  
  volPlotData <- reactive({
    dat = batTPM %>% unite('Group', volGroups()) %>% 
      filter(Group %in% c(volNumerator(), volDenominator())) %>%
      group_by(GeneName) %>% 
      summarise(foldChange = mean(TPM[Group == volNumerator()], na.rm = T) / mean(TPM[Group == volDenominator()], na.rm = T),
                pvalue = pvalue(TPM[Group == volNumerator()], TPM[Group == volDenominator()])) %>% 
      mutate(padj = p.adjust(pvalue, 'BH'))
    
    if(is.null(input$plot_brush)){
      dat = dat %>% mutate(hit = ifelse(abs(log2(foldChange)) >= log2(fcCut()) & !!rlang::sym(pChoice()) <= pvalCut(), T, F)) %>%
        dplyr::select(GeneName, foldChange, padj, everything())
    } else{
      dat = dat %>% mutate(hit = ifelse(foldChange >= 2^input$plot_brush$xmin &
                                          foldChange <= 2^input$plot_brush$xmax &
                                          !!rlang::sym(pChoice()) <= 10^-input$plot_brush$ymin &
                                          !!rlang::sym(pChoice()) >= 10^-input$plot_brush$ymax, T, F)) %>%
        dplyr::select(GeneName, foldChange, padj, everything())
    }
  })
  
  volPlot <- reactive({
    ggplot(volPlotData(), aes(x = log2(foldChange), y = -log10(!!rlang::sym(pChoice())), colour = hit)) + geom_point(alpha = 0.5) + 
      geom_vline(xintercept = c(log2(fcCut()), log2(1/fcCut())), colour = 'red', linetype = 'dashed') + 
      geom_hline(yintercept = -log10(pvalCut()), colour = 'red', linetype = 'dashed') + theme_bw() + 
      scale_color_manual(values = c('#bababa','#e08214')) + theme(legend.position="none") +
      xlim(-10,10) + ylim(0,10)
  })
  
  output$volPlot <- renderPlot({
    volPlot() + geom_point(data = filter(volPlotData(), hit == T)[input$volGroupHits_rows_selected,], 
                           aes(x = log2(foldChange), y = -log10(!!rlang::sym(pChoice()))), 
                           colour = 'black', size = 5) 
  })
  
  output$volGroupHits <- renderDataTable({
    datatable(filter(volPlotData(), hit == T), selection = 'single')
  })
  
  output$volPlotUniprot <- renderUI({
    s = input$volGroupHits_rows_selected
    if (length(s)) {
      
      sGene = filter(volPlotData(), hit == T)[s,]$GeneName
      uniProt = filter(uniprotData, GeneName == sGene)$UniProt
      
      entireHTML = read_html(paste0('https://www.uniprot.org/uniprot/', uniProt))
      html_nodes(entireHTML, 'body main div')[22] %>% as.character() %>% HTML()
    }
  })
  
  observe({
    s = input$volGroupHits_rows_selected
    if(length(s)){
      sGene = filter(volPlotData(), hit == T)[s,]$GeneName
      updateTextInput(session, "BATgenes", value = sGene)
    }
  })
  
  output$volBATplot <- renderPlot({
    BATdatasetPlot()
  })
  
  # Download PDF of Volcano Plot
  output$volDownloadPlot <-downloadHandler(
    filename = function() {
      paste(volNumerator(),' vs ',volDenominator(), ".pdf", sep = "")
    },
    content = function(file){
      #x = plotDims()
      renderPlot({
        volPlot()
      })
      ggsave(file, width = input$volWidth, height = input$volHeight, units = c('in'))
    }
  )
  
  # Downloadable csv Volcano Plot
  output$volDownloadData <- downloadHandler(
    filename = function() {
      paste(volNumerator(),' vs ',volDenominator(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(volPlotData(), file, row.names = FALSE)
    }
  )
  
