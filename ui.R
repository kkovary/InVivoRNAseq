source('UI Functions/titlePage.R')
source('UI Functions/BAT UI/batTabUI.R')
source('UI Functions/WAT UI/watTab.R')

shinyUI(
  navbarPage('InVivoRNAseq',
             titleTab,
             batTabUI,
             watTab
  )
)