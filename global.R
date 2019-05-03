library(shiny)
library(tidyverse)
library(ggpubr)

# Read in data

batTPM = read_csv('data/bat_normalized_data_genelevel_tpm.csv')
watTPM = read_csv("data/wat_normalized_data_genelevel_tpm.csv")


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
