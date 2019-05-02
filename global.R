library(shiny)
library(tidyverse)
library(ggpubr)

# Read in data

batTPM = read_csv('data/bat_normalized_data_genelevel_tpm.csv')
watTPM = read_csv("data/wat_normalized_data_genelevel_tpm.csv")