WATgoTermGroups <- reactive({
  if(length(input$WATgoGroups) > 0){
    WATgoConditions <- watTPM %>% dplyr::filter(!duplicated(sample)) %>%
      dplyr::select(sample, Treatment, Delivery, Day) %>%
      tidyr::unite('Group', input$WATgoGroups, remove = F)
    
    WATgoConditions <- WATgoConditions$Group %>% unique() %>% as.vector()
  } else{
    WATgoConditions <- NA
  }
})

observe({
  updateSelectInput(session, 'WATgoNumerator',
                    choices = WATgoTermGroups(),
                    selected = NA)
  
  updateSelectInput(session, 'WATgoDenominator',
                    choices = WATgoTermGroups(),
                    selected = NA)
})


WATgoGroups <- eventReactive(input$WATgoPlotButton, {input$WATgoGroups})
WATgoNumerator <- eventReactive(input$WATgoPlotButton, {input$WATgoNumerator})
WATgoDenominator <- eventReactive(input$WATgoPlotButton, {input$WATgoDenominator})
WATgoFCCut <- eventReactive(input$WATgoPlotButton, {input$WATgoFCCut})
WATgoPValCut <- eventReactive(input$WATgoPlotButton, {input$WATgoPValCut})
WATgoPChoice <- eventReactive(input$WATgoPlotButton, {input$WATgoPChoice})


WATgoFC <- reactive({
  dat <- watTPM %>% tidyr::unite('Group', WATgoGroups()) %>% 
    dplyr::filter(Group %in% c(WATgoNumerator(), WATgoDenominator())) %>% dplyr::group_by(GeneName) %>% 
    dplyr::summarise(FC = mean(TPM[Group == WATgoNumerator()], na.rm = T) / mean(TPM[Group == WATgoDenominator()], na.rm = T),
                     pval = pvalue(TPM[Group == WATgoNumerator()], TPM[Group == WATgoDenominator()])) %>%
    dplyr::mutate(padj = p.adjust(pval, 'BH'),
                  hit = ifelse(abs(log2(FC)) >= log2(1.5) &  pval <= 0.05, T, F))
  
  if(is.null(input$WATgoPlot_brush)){
    dat <- dat %>% dplyr::mutate(hit = ifelse(abs(log2(FC)) >= log2(WATgoFCCut()) & !!rlang::sym(WATgoPChoice()) <= WATgoPValCut(), T, F)) %>%
      dplyr::select(GeneName, FC, padj, everything())
  } else{
    dat <- dat %>% dplyr::mutate(hit = ifelse(FC >= 2^input$WATgoPlot_brush$xmin &
                                                FC <= 2^input$WATgoPlot_brush$xmax &
                                                !!rlang::sym(WATgoPChoice()) <= 10^-input$WATgoPlot_brush$ymin &
                                                !!rlang::sym(WATgoPChoice()) >= 10^-input$WATgoPlot_brush$ymax, T, F)) %>%
      dplyr::select(GeneName, FC, padj, everything())
  }
  
  
})

WATgoPlotData <- reactive({
  withProgress(message = 'Calculating GO Enrichment', value = 0, {
    gene.df <- clusterProfiler::bitr(dplyr::filter(WATgoFC(), hit == T)$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    universe <- clusterProfiler::bitr(WATgoFC()$GeneName, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    goDF <- clusterProfiler::enrichGO(gene = gene.df$ENTREZID, universe = universe$ENTREZID, ont = 'ALL', OrgDb = org.Mm.eg.db)
    incProgress(0.25, detail = "Please be patient")
    
    goDF <- goDF %>% tibble::as_tibble() %>% dplyr::rowwise() %>% dplyr::mutate(zscore = zScore(geneID, WATgoFC(), universe))
    incProgress(0.25, detail = "Done")
    
    goDF
  })
})

WATgoPlot <- reactive({
  ggplot(WATgoPlotData(), aes(x = zscore, y = -log10(p.adjust), size = Count, colour = ONTOLOGY)) + 
    geom_point(alpha = 0.15) + theme_bw() + facet_wrap(~ONTOLOGY, scales = 'free') + xlim(-10,10) + 
    geom_point(data = filter(WATgoPlotData())[input$WATgoTermTable_rows_selected,], 
               aes(x = zscore, y = -log10(p.adjust), colour = ONTOLOGY, size = Count)) +
    geom_text_repel(data = filter(WATgoPlotData())[input$WATgoTermTable_rows_selected,],
                    aes(label = Description), colour = 'black', size = 4, force = 3)
})

output$WATgoPlot <- renderPlot({
  WATgoPlot()
})


output$WATgoTermTable  <- DT::renderDataTable(
  DT::datatable(
    {dplyr::select(WATgoPlotData(), ONTOLOGY, Description, ID, Count, zscore, p.adjust)},
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

WATgoTermGenes <- reactive({
  s <- input$WATgoTermTable_rows_selected
  if (length(s)) {
    
    sGO <- WATgoPlotData()[s,'geneID'] %>% unlist() %>% strsplit('/') %>% unlist() %>% as.character() %>% unique()
    sGOtable <- AnnotationDbi::select(org.Mm.eg.db, keys = sGO, columns =  c('SYMBOL', 'GENENAME')) %>%
      rename(GeneName = SYMBOL)
    
    sGOtable <- dplyr::left_join(sGOtable, WATgoFC(), by = 'GeneName') %>% select(-hit)
    
    sGOtable <- dplyr::left_join(sGOtable,
                                 select(uniprotData, GeneName, UniProt),
                                 by = 'GeneName') %>%
      rename(SYMBOL = GeneName, UNIPROT = UniProt) %>% select(ENTREZID, UNIPROT, everything())
  }
})

output$WATgoTermGenesTable  <- DT::renderDataTable(
  DT::datatable(
    {
      WATgoTermGenes()
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

output$WATgoPlotUniprot <- renderUI({
  s <- input$WATgoTermGenesTable_rows_selected
  if (length(s)) {
    sGene <- WATgoTermGenes()$UNIPROT[s]
    entireHTML <- read_html(paste0('https://www.uniprot.org/uniprot/', sGene))
    html_nodes(entireHTML,'body main div')[22] %>% as.character() %>% HTML()
  }
})

observe({
  s = input$WATgoTermGenesTable_rows_selected
  if(length(s)){
    sGene <- WATgoTermGenes()$SYMBOL[s]
    updateTextInput(session, "WATgenes", value = sGene)
  }
})

output$goWATplot <- renderPlot({
  WATdatasetPlot()
})

WATgoHeatData <- reactive({
  
  genes <- WATgoTermGenes() %>% select(SYMBOL) %>% unlist() %>% as.character()
  
  dat <- watTPM %>% tidyr::unite('Group', WATgoGroups()) %>%
    dplyr::filter(Group %in% c(WATgoNumerator(), WATgoDenominator()),
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
WATinteractiveHeatmap <- reactive({
  heatmaply(WATgoHeatData(), limits = c(-3,3), colors = rev(colorRampPalette(brewer.pal(11, 'RdBu'))(201)))
})

# interactive heatmap output
output$WATinteractive <- renderPlotly({
  if(!is.null(WATgoHeatData()))
    withProgress(message = 'Making interactive heatmap:', value = 0, {
      genexp <- WATgoHeatData()
      genexp_df <- as.data.frame(genexp)
      names_genexp_df <- genexp_df[,1]
      n <- NROW(names_genexp_df)
      for (i in 1:n) {
        incProgress(1/n, detail = "Please wait...")
      }
      WATinteractiveHeatmap()
    })	
})

# output$goHeat <- renderD3heatmap({
#   d3heatmap(goHeatData())
#   #pheatmap(goHeatData(), cluster_cols = F, cluster_rows = F)
# })

# Download PDF of Volcano Plot
output$WATgoDownloadPlot <-downloadHandler(
  filename = function() {
    paste(WATgoNumerator(),' vs ',WATgoDenominator(), ".pdf", sep = "")
  },
  content = function(file){
    #x = plotDims()
    renderPlot({
      WATgoPlot()
    })
    ggsave(file, width = input$goWidth, height = input$goHeight, units = c('in'))
  }
)