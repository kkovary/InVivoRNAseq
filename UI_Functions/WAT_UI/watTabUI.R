source('UI_Functions/WAT_UI/watBarPlotUI.R')
source('UI_Functions/WAT_UI/watGoTermsUI.R')
source('UI_Functions/WAT_UI/watVolcanoUI.R')

watTabUI <- navbarMenu("WAT",
                       tabPanel('Analysis', includeHTML('HTML_and_Rmd_Files/Wat_Analysis.html')),
                       watBarPlotUI,
                       watGoTermsUI,
                       watVolcanoUI
                       #tabPanel('Heat Maps')
)
