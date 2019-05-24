library(org.Mm.eg.db)
library(clusterProfiler)


goTermGroups <- reactive({
  if(length(input$goGroups) > 0){
    goConditions <- batTPM %>% dplyr::filter(!duplicated(sample)) %>%
      dplyr::select(sample, Treatment, Delivery, Day) %>%
      tidyr::unite('Group', input$goGroups, remove = F)
    
    goConditions <- goConditions$Group %>% unique() %>% as.vector()
  } else{
    goConditions <- NA
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
  dat <- batTPM %>% tidyr::unite('Group', goGroups()) %>% 
    dplyr::filter(Group %in% c(goNumerator(), goDenominator())) %>% dplyr::group_by(GeneName) %>% 
    dplyr::summarise(FC = mean(TPM[Group == goNumerator()], na.rm = T) / mean(TPM[Group == goDenominator()], na.rm = T),
              pval = pvalue(TPM[Group == goNumerator()], TPM[Group == goDenominator()])) %>%
    dplyr::mutate(padj = p.adjust(pval, 'BH'),
           hit = ifelse(abs(log2(FC)) >= log2(1.5) &  pval <= 0.05, T, F))
  
  if(is.null(input$goPlot_brush)){
    dat <- dat %>% dplyr::mutate(hit = ifelse(abs(log2(FC)) >= log2(goFCCut()) & !!rlang::sym(goPChoice()) <= goPValCut(), T, F)) %>%
      dplyr::select(GeneName, FC, padj, everything())
  } else{
    dat <- dat %>% dplyr::mutate(hit = ifelse(FC >= 2^input$goPlot_brush$xmin &
                                        FC <= 2^input$goPlot_brush$xmax &
                                        !!rlang::sym(goPChoice()) <= 10^-input$goPlot_brush$ymin &
                                        !!rlang::sym(goPChoice()) >= 10^-input$goPlot_brush$ymax, T, F)) %>%
      dplyr::select(GeneName, FC, padj, everything())
  }
  
  gene.df <- clusterProfiler::bitr(dplyr::filter(dat, hit == T)$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
  universe <- clusterProfiler::bitr(dat$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
  
  goDF <- clusterProfiler::enrichGO(gene = gene.df$ENTREZID, universe = universe$ENTREZID, ont = 'ALL', OrgDb = org.Mm.eg.db)
  goDF <- goDF %>% tibble::as_tibble() %>% dplyr::rowwise() %>% dplyr::mutate(zscore = zScore(geneID, dat, universe))
})


goPlot <- reactive({
  ggplot(goPlotData(), aes(x = zscore, y = -log10(p.adjust), size = Count, colour = ONTOLOGY)) + 
    geom_point(alpha = 0.15) + xlim(-10,10) + theme_bw() + facet_wrap(~ONTOLOGY)
})

output$goPlot <- renderPlot({
  goPlot()
})


output$goTermTable  <- DT::renderDataTable(
  DT::datatable(
    {dplyr::select(goPlotData(), ONTOLOGY, Description, ID, Count, zscore, p.adjust)},
    selection = 'single',
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    ),
    
    class = "display"
  ),
  server = FALSE
)

output$goTermGenes  <- DT::renderDataTable(
  DT::datatable(
    {
      s <- input$goTermTable_rows_selected
      if (length(s)) {
        
        sGO <- goPlotData()[s,'geneID'] %>% unlist() %>% strsplit('/') %>% unlist() %>% as.character()
        AnnotationDbi::select(org.Mm.eg.db, keys = sGO, columns =  c('SYMBOL', 'GENENAME'))
        
      }
    },
    selection = 'single',
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip',
      buttons = c('csv', 'excel')
    ),
    
    class = "display"
  ),
  server = FALSE
)

