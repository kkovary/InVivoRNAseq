source('UI Functions/titlePage.R')
source('UI Functions/BAT UI/batTab.R')
source('UI Functions/WAT UI/watTab.R')

shinyUI(
  navbarPage('InVivoRNAseq',
             titleTab,
             batTab,
             watTab
  )
)

