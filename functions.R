### Additional functions ###

formatHTML <- function(origFile, newFile){
  
  # This function reformats an HTLM file produced by Rmarkdown
  # so that it's compatible with a Shiny app that uses navbarMenu.
  
  require(magrittr)
  require(xml2)
  require(rvest)
  
  xml2::read_html(origFile) %>% 
    rvest::html_node('body') %>% 
    xml2::write_html(newFile)
  
}


volPlotDataFun <- function(numerator, denominator, data, pvalueCut, foldChangeCut){
  # This function takes in the RNAseq data, and calculates the fold change
  # and pvalue from two conditions (numerator and denominator) and labels
  # which genes are significant or not based on the user defined pvalue and
  # fold change cutoffs.
  
  data %>% filter(Condition %in% c(numerator, denominator)) %>%
    group_by(GeneName) %>% 
    summarise(foldChange = mean(TPM[Condition == numerator], na.rm = T) / mean(TPM[Condition == denominator], na.rm = T),
              pvalue = t.test(TPM[Condition == numerator], TPM[Condition == denominator], na.rm = T)$p.value) %>%
    mutate(Hit = ifelse(pvalue <= pvalueCut & abs(log2(foldChange)) >= log2(foldChangeCut), TRUE, FALSE))
  
}

