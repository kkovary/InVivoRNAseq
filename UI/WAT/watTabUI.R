source('UI/WAT/watBarPlotUI.R')
source('UI/WAT/watGoTermsUI.R')
source('UI/WAT/watVolcanoUI.R')

watTabUI <- navbarMenu("WAT",
                       tabPanel('Analysis', includeHTML('HTML_and_Rmd_Files/Wat_Analysis.html')),
                       watBarPlotUI,
                       watGoTermsUI,
                       watVolcanoUI
)
