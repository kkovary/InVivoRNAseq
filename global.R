library(shiny)
library(ggpubr)
library(rvest)
library(xml2)
library(DT)
library(ggrepel)
library(clusterProfiler)
library(org.Mm.eg.db)
library(tidyverse)
library(d3heatmap)
library(heatmaply)
library(RColorBrewer)

# Read in data

batTPM = read_csv('data/bat_normalized_data_genelevel_tpm.csv')
watTPM = read_csv('data/wat_normalized_data_genelevel_tpm.csv')
uniprotData = read_csv('data/UniprotData.csv')


#uniprotHTML('https://www.uniprot.org/uniprot/P12345', 'test.html')

### Additional functions ###

formatHTML <- function(origFile, newFile){
  
  # This function reformats an HTLM file produced by Rmarkdown
  # so that it's compatible with a Shiny app that uses navbarMenu.
  
  require(magrittr)
  require(xml2)
  require(rvest)
  
  read_html(origFile) %>% 
    html_node('body') %>% 
    write_html(newFile)
  
}

uniprotHTML <- function(uniprot, filename){
  entireHTML = read_html(paste0('https://www.uniprot.org/uniprot/',uniprot))
  section = html_nodes(entireHTML, 'body main div')[22]
  write_html(section, filename)
}

###########################
#### p-value functions ####
###########################


zscore <- function(x,y){
  x = x[!is.na(x)]
  y = y[!is.na(y)]
  
  -abs((mean(x) - mean(y) - 0) / ((sd(x)/length(x)) / (sd(y)/length(y))))
}


t_test <- function(x,y){
  x = x[!is.na(x)]
  y = y[!is.na(y)]
  
  -abs((mean(x) - mean(y)) / (sqrt((sd(x)^2 / length(x)) + (sd(y)^2 / length(x)))))
}

deg_free <- function(x,y){
  x = x[!is.na(x)]
  y = y[!is.na(y)]
  
  ((sd(x)^2 / length(x)) + (sd(y)^2 / length(y)))^2 / 
    ((sd(x)^4 / (length(x)^2 * (length(x) - 1))) + (sd(y)^4 / (length(y)^2 * (length(y) - 1))))
}

pvalue <- function(x, y){
  
  2*pt(t_test(x,y), deg_free(x,y))
  
}


zScore <- function(x, subsetFC, universe){
  genes <-  x %>% strsplit('/') %>% unlist()
  convert  <-  dplyr::filter(universe, ENTREZID %in% genes)$SYMBOL
  convert <- convert[!duplicated(convert)]
  
  FC <-  dplyr::filter(subsetFC, GeneName %in% convert)$FC
  return((sum(FC >= 1.5) - sum(FC <= (1/1.5))) / sqrt(length(FC)))
}