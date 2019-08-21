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

