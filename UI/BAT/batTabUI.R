source('UI/BAT/barPlotUI.R')
source('UI/BAT/goTermsUI.R')
source('UI/BAT/volcanoUI.R')

batTabUI <- navbarMenu("BAT",
                     tabPanel('Analysis', includeHTML('HTML_and_Rmd_Files/Analysis.html')),
                     barPlotUI,
                     goTermsUI,
                     volcanoUI
)
