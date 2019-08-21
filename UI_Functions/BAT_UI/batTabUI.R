source('UI_Functions/BAT_UI/barPlotUI.R')
source('UI_Functions/BAT_UI/goTermsUI.R')
source('UI_Functions/BAT_UI/volcanoUI.R')

batTabUI <- navbarMenu("BAT",
                     tabPanel('Analysis', includeHTML('HTML_and_Rmd_Files/Analysis.html')),
                     barPlotUI,
                     goTermsUI,
                     volcanoUI,
                     tabPanel('Heat Maps')
)
