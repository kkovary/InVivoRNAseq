WATvolPlotGroups <- reactive({
  if(length(input$WATvolGroups) > 0){
    volConditions = watTPM %>% filter(!duplicated(sample)) %>% dplyr::select(sample, Treatment, Delivery, Day) %>%
      unite('Group', input$WATvolGroups, remove = F)
    
    volConditions = volConditions$Group %>% unique() %>% as.vector()
  } else{
    volConditions = NA
  }
  
})

observe({
  updateSelectInput(session, 'WATvolNumerator',
                    choices = WATvolPlotGroups(),
                    selected = NA)
  
  updateSelectInput(session, 'WATvolDenominator',
                    choices = WATvolPlotGroups(),
                    selected = NA)
})

WATvolGroups <- eventReactive(input$WATvolPlotButton, {input$WATvolGroups})
WATvolNumerator <- eventReactive(input$WATvolPlotButton, {input$WATvolNumerator})
WATvolDenominator <- eventReactive(input$WATvolPlotButton, {input$WATvolDenominator})
WATfcCut <- eventReactive(input$WATvolPlotButton, {input$WATfcCut})
WATpvalCut <- eventReactive(input$WATvolPlotButton, {input$WATpvalCut})
WATpChoice <- eventReactive(input$WATvolPlotButton, {input$WATpChoice})


WATvolPlotData <- reactive({
  dat = watTPM %>% unite('Group', WATvolGroups()) %>% 
    filter(Group %in% c(WATvolNumerator(), WATvolDenominator())) %>%
    group_by(GeneName) %>% 
    summarise(foldChange = mean(TPM[Group == WATvolNumerator()], na.rm = T) / mean(TPM[Group == WATvolDenominator()], na.rm = T),
              pvalue = pvalue(TPM[Group == WATvolNumerator()], TPM[Group == WATvolDenominator()])) %>% 
    mutate(padj = p.adjust(pvalue, 'BH'))
  
  if(is.null(input$WATplot_brush)){
    dat = dat %>% mutate(hit = ifelse(abs(log2(foldChange)) >= log2(WATfcCut()) & !!rlang::sym(WATpChoice()) <= WATpvalCut(), T, F)) %>%
      dplyr::select(GeneName, foldChange, padj, everything())
  } else{
    dat = dat %>% mutate(hit = ifelse(foldChange >= 2^input$WATplot_brush$xmin &
                                        foldChange <= 2^input$WATplot_brush$xmax &
                                        !!rlang::sym(WATpChoice()) <= 10^-input$WATplot_brush$ymin &
                                        !!rlang::sym(WATpChoice()) >= 10^-input$WATplot_brush$ymax, T, F)) %>%
      dplyr::select(GeneName, foldChange, padj, everything())
  }
})

WATvolPlot <- reactive({
  ggplot(WATvolPlotData(), aes(x = log2(foldChange), y = -log10(!!rlang::sym(WATpChoice())), colour = hit)) + geom_point(alpha = 0.5) + 
    geom_vline(xintercept = c(log2(WATfcCut()), log2(1/WATfcCut())), colour = 'red', linetype = 'dashed') + 
    geom_hline(yintercept = -log10(WATpvalCut()), colour = 'red', linetype = 'dashed') + theme_bw() + 
    scale_color_manual(values = c('#bababa','#e08214')) + theme(legend.position="none") +
    xlim(-10,10) + ylim(0,10)
})

output$WATvolPlot <- renderPlot({
  WATvolPlot() + geom_point(data = filter(WATvolPlotData(), hit == T)[input$WATvolGroupHits_rows_selected,], 
                         aes(x = log2(foldChange), y = -log10(!!rlang::sym(WATpChoice()))), 
                         colour = 'black', size = 5)
})

output$WATvolGroupHits <- renderDataTable({
  datatable(filter(WATvolPlotData(), hit == T), selection = 'single')
})

output$WATvolPlotUniprot <- renderUI({
  s = input$WATvolGroupHits_rows_selected
  if (length(s)) {
    
    sGene = filter(WATvolPlotData(), hit == T)[s,]$GeneName
    uniProt = filter(uniprotData, GeneName == sGene)$UniProt
    
    entireHTML = read_html(paste0('https://www.uniprot.org/uniprot/', uniProt))
    html_nodes(entireHTML, 'body main div')[22] %>% as.character() %>% HTML()
  }
})

observe({
  s = input$WATvolGroupHits_rows_selected
  if(length(s)){
    sGene = filter(WATvolPlotData(), hit == T)[s,]$GeneName
    updateTextInput(session, "WATgenes", value = sGene)
  }
})

output$volWATbarPlot <- renderPlot({
  WATdatasetPlot()
})

# Download PDF of Volcano Plot
output$WATvolDownloadPlot <-downloadHandler(
  filename = function() {
    paste(WATvolNumerator(),' vs ',WATvolDenominator(), ".pdf", sep = "")
  },
  content = function(file){
    #x = plotDims()
    renderPlot({
      WATvolPlot()
    })
    ggsave(file, width = input$WATvolWidth, height = input$WATvolHeight, units = c('in'))
  }
)

# Downloadable csv Volcano Plot
output$WATvolDownloadData <- downloadHandler(
  filename = function() {
    paste(WATvolNumerator(),' vs ',WATvolDenominator(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(WATvolPlotData(), file, row.names = FALSE)
  }
)

