source('UI Functions/WAT UI/watBarPlotUI.R')
source('UI Functions/WAT UI/watGoTermsUI.R')
source('UI Functions/WAT UI/watVolcanoUI.R')

watTabUI <- navbarMenu("WAT",
                       tabPanel('Analysis', includeHTML('HTML and Rmd Files/Wat_Analysis.html')),
                       watBarPlotUI,
                       watGoTermsUI,
                       watVolcanoUI
                       #tabPanel('Heat Maps')
)
