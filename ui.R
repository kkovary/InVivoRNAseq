source('UI_Functions/titlePage.R')
source('UI_Functions/BAT_UI/batTabUI.R')
source('UI_Functions/WAT_UI/watTabUI.R')

shinyUI(
  navbarPage('InVivoRNAseq',
             titleTab,
             batTabUI,
             watTabUI
  )
)