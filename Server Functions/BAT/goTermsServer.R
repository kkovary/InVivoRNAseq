library(clusterProfiler)
library(org.Mm.eg.db)

goTermGroups <- reactive({
  if(length(input$goGroups) > 0){
    goConditions = batTPM %>% filter(!duplicated(sample)) %>%
      dplyr::select(sample, Treatment, Delivery, Day) %>%
      unite('Group', input$goGroups, remove = F)

    goConditions = goConditions$Group %>% unique() %>% as.vector()
  } else{
    goConditions = NA
  }
})

observe({
  updateSelectInput(session, 'goNumerator',
                    choices = goTermGroups(),
                    selected = NA)

  updateSelectInput(session, 'goDenominator',
                    choices = goTermGroups(),
                    selected = NA)
})


goGroups <- eventReactive(input$goPlotButton, {input$goGroups})
goNumerator <- eventReactive(input$goPlotButton, {input$goNumerator})
goDenominator <- eventReactive(input$goPlotButton, {input$goDenominator})
goFCCut <- eventReactive(input$goPlotButton, {input$goFCCut})
goPValCut <- eventReactive(input$goPlotButton, {input$goPValCut})
goPChoice <- eventReactive(input$goPlotButton, {input$goPChoice})


goPlotData <- reactive({
  dat = batTPM %>% unite('Group', goGroups()) %>% 
    filter(Group %in% c(goNumerator(), goDenominator())) %>% group_by(GeneName) %>% 
    summarise(FC = mean(TPM[Group == goNumerator()], na.rm = T) / mean(TPM[Group == goDenominator()], na.rm = T),
              pval = pvalue(TPM[Group == goNumerator()], TPM[Group == goDenominator()])) %>%
    mutate(padj = p.adjust(pval, 'BH'),
           hit = ifelse(abs(log2(FC)) >= log2(1.5) &  pval <= 0.05, T, F))
  
    if(is.null(input$goPlot_brush)){
      dat = dat %>% mutate(hit = ifelse(abs(log2(FC)) >= log2(goFCCut()) & !!rlang::sym(goPChoice()) <= goPValCut(), T, F)) %>%
        dplyr::select(GeneName, FC, padj, everything())
    } else{
      dat = dat %>% mutate(hit = ifelse(FC >= 2^input$goPlot_brush$xmin &
                                          FC <= 2^input$goPlot_brush$xmax &
                                          !!rlang::sym(goPChoice()) <= 10^-input$goPlot_brush$ymin &
                                          !!rlang::sym(goPChoice()) >= 10^-input$goPlot_brush$ymax, T, F)) %>%
        dplyr::select(GeneName, FC, padj, everything())
    }
})




allEnrichGO <- reactive({
  gene.df <- bitr(filter(goPlotData(), hit == T)$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
  universe <- bitr(goPlotData()$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
  
  goDF = enrichGO(gene = gene.df$ENTREZID, universe = universe$ENTREZID, ont = 'ALL', OrgDb = org.Mm.eg.db)
  goDF = goDF %>% as.tibble() %>% rowwise() %>% dplyr::mutate(zscore = zScore(geneID, goPlotData()))
})


goPlot <- reactive({
  ggplot(allEnrichGO(), aes(x = zscore, y = -log10(p.adjust), size = Count, colour = ONTOLOGY)) + 
    geom_point(alpha = 0.15) + xlim(-10,10) + theme_bw() + facet_wrap(~ONTOLOGY)
})

output$goPlot <- renderPlot({
  goPlot()
  # volPlot() + geom_point(data = filter(volPlotData(), hit == T)[input$volGroupHits_rows_selected,], 
  #                        aes(x = log2(foldChange), y = -log10(!!rlang::sym(pChoice()))), 
  #                        colour = 'black', size = 5) 
})

output$goTermTable <- renderDataTable({
  datatable(dplyr::select(allEnrichGO(), ONTOLOGY, Description, ID, GeneRatio, zscore, p.adjust), 
            selection = 'single')
})

output$goTermGenes <- renderDataTable({
  s <- input$goTermTable_rows_selected
  if (length(s)) {
    
    sGO <- allEnrichGO()[s,'geneID'] %>% unlist() %>% strsplit('/') %>% unlist() %>% as.character()
    select(org.Mm.eg.db, keys = sGO, columns =  c('SYMBOL', 'GENENAME'))
    
  }
})

