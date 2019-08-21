source('UI/titlePage.R')
source('UI/BAT/batTabUI.R')
source('UI/WAT/watTabUI.R')

shinyUI(
  navbarPage('InVivoRNAseq',
             titleTab,
             batTabUI,
             watTabUI
  )
)