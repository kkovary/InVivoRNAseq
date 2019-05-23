source('UI Functions/BAT UI/barPlotUI.R')
source('UI Functions/BAT UI/goTermsUI.R')
source('UI Functions/BAT UI/volcanoUI.R')

batTabUI <- navbarMenu("BAT",
                     tabPanel('Analysis', includeHTML('HTML and Rmd FIles/Analysis.html')),
                     barPlotUI,
                     goTermsUI,
                     volcanoUI,
                     tabPanel('Heat Maps')
)
