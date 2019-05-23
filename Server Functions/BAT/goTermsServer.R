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


# goPlotData <- reactive({
#   dat = batTPM %>% unite('Group', goGroups()) %>%
#     filter(Group %in% c(goNumerator(), goDenominator())) %>%
#     group_by(GeneName) %>%
#     summarise(foldChange = mean(TPM[Group == goNumerator()], na.rm = T) / mean(TPM[Group == goDenominator()], na.rm = T),
#               pvalue = pvalue(TPM[Group == goNumerator()], TPM[Group == goDenominator()])) %>%
#     mutate(padj = p.adjust(pvalue, 'BH'))
# 
#   if(is.null(input$goPlot_brush)){
#     dat = dat %>% mutate(hit = ifelse(abs(log2(foldChange)) >= log2(goFCCut()) & !!rlang::sym(goPChoice()) <= goPValCut(), T, F)) %>%
#       dplyr::select(GeneName, foldChange, padj, everything())
#   } else{
#     dat = dat %>% mutate(hit = ifelse(foldChange >= 2^input$goPlot_brush$xmin &
#                                         foldChange <= 2^input$goPlot_brush$xmax &
#                                         !!rlang::sym(goPChoice()) <= 10^-input$goPlot_brush$ymin &
#                                         !!rlang::sym(goPChoice()) >= 10^-input$goPlot_brush$ymax, T, F)) %>%
#       dplyr::select(GeneName, foldChange, padj, everything())
#   }
# })