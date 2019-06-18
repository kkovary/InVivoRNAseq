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


goFC <- reactive({
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
  
  
})

goPlotData <- reactive({
  withProgress(message = 'Calculating GO Enrichment', value = 0, {
    gene.df <- clusterProfiler::bitr(dplyr::filter(goFC(), hit == T)$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    universe <- clusterProfiler::bitr(goFC()$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    goDF <- clusterProfiler::enrichGO(gene = gene.df$ENTREZID, universe = universe$ENTREZID, ont = 'ALL', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    goDF <- goDF %>% tibble::as_tibble() %>% dplyr::rowwise() %>% dplyr::mutate(zscore = zScore(geneID, goFC(), universe))
    incProgress(0.25, detail = "Done")
    
    goDF
  })
})

goPlot <- reactive({
  ggplot(goPlotData(), aes(x = zscore, y = -log10(p.adjust), size = Count, colour = ONTOLOGY)) + 
    geom_point(alpha = 0.15) + theme_bw() + facet_wrap(~ONTOLOGY, scales = 'free') + xlim(-10,10) + 
    geom_point(data = filter(goPlotData())[input$goTermTable_rows_selected,], 
               aes(x = zscore, y = -log10(p.adjust), colour = ONTOLOGY, size = Count)) +
    geom_text_repel(data = filter(goPlotData())[input$goTermTable_rows_selected,],
                    aes(label = Description), colour = 'black', size = 4, force = 3)
})

output$goPlot <- renderPlot({
  goPlot()
})


output$goTermTable  <- DT::renderDataTable(
  DT::datatable(
    {dplyr::select(goPlotData(), ONTOLOGY, Description, ID, Count, zscore, p.adjust)},
    selection = 'multiple',
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

goTermGenes <- reactive({
  s <- input$goTermTable_rows_selected
  if (length(s)) {
    
    sGO <- goPlotData()[s,'geneID'] %>% unlist() %>% strsplit('/') %>% unlist() %>% as.character() %>% unique()
    sGOtable <- AnnotationDbi::select(org.Mm.eg.db, keys = sGO, columns =  c('SYMBOL', 'GENENAME')) %>%
      rename(GeneName = SYMBOL)
    
    sGOtable <- dplyr::left_join(sGOtable, goFC(), by = 'GeneName') %>% select(-hit)

    sGOtable <- dplyr::left_join(sGOtable,
                                 select(uniprotData, GeneName, UniProt),
                                 by = 'GeneName') %>%
      rename(SYMBOL = GeneName, UNIPROT = UniProt) %>% select(ENTREZID, UNIPROT, everything())
  }
})

output$goTermGenesTable  <- DT::renderDataTable(
  DT::datatable(
    {
      goTermGenes()
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

output$goPlotUniprot <- renderUI({
  s <- input$goTermGenesTable_rows_selected
  if (length(s)) {
    sGene <- goTermGenes()$UNIPROT[s]
    entireHTML <- read_html(paste0('https://www.uniprot.org/uniprot/', sGene))
    html_nodes(entireHTML,'body main div')[22] %>% as.character() %>% HTML()
  }
})

observe({
  s = input$goTermGenesTable_rows_selected
  if(length(s)){
    sGene <- goTermGenes()$SYMBOL[s]
    updateTextInput(session, "BATgenes", value = sGene)
  }
})

output$goBATplot <- renderPlot({
  BATdatasetPlot()
})

goHeatData <- reactive({
  
  genes <- goTermGenes() %>% select(SYMBOL) %>% unlist() %>% as.character()
  
  dat <- batTPM %>% tidyr::unite('Group', goGroups()) %>%
    dplyr::filter(Group %in% c(goNumerator(), goDenominator()),
                  GeneName %in% genes) %>%
    select(GeneName, sample, TPM) %>% spread(sample, TPM)
  
  mat <- as.matrix(dat[2:ncol(dat)])
  rownames(mat) <- dat$GeneName
  mat <- t(log2(apply(mat, 1, function(x) x / mean(x, na.rm = T))))
  mat[which(is.na(mat))] = 0
  mat[which(is.infinite(mat))] = 0
  mat[which(mat > 3)] = 3
  mat[which(mat < -3)] = -3
  mat
})

# interactive heatmap prep
interactiveHeatmap <- reactive({
  heatmaply(goHeatData(), limits = c(-3,3), colors = rev(colorRampPalette(brewer.pal(11, 'RdBu'))(201)))
})

# interactive heatmap output
output$interactive <- renderPlotly({
  if(!is.null(goHeatData()))
    withProgress(message = 'Making interactive heatmap:', value = 0, {
      genexp <- goHeatData()
      genexp_df <- as.data.frame(genexp)
      names_genexp_df <- genexp_df[,1]
      n <- NROW(names_genexp_df)
      for (i in 1:n) {
        incProgress(1/n, detail = "Please wait...")
      }
      interactiveHeatmap()
    })	
})

# output$goHeat <- renderD3heatmap({
#   d3heatmap(goHeatData())
#   #pheatmap(goHeatData(), cluster_cols = F, cluster_rows = F)
# })

# Download PDF of Volcano Plot
output$goDownloadPlot <-downloadHandler(
  filename = function() {
    paste(goNumerator(),' vs ',goDenominator(), ".pdf", sep = "")
  },
  content = function(file){
    #x = plotDims()
    renderPlot({
      goPlot()
    })
    ggsave(file, width = input$goWidth, height = input$goHeight, units = c('in'))
  }
)