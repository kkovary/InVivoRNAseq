source('UI Functions/BAT UI/barPlot.R')
source('UI Functions/BAT UI/goTerm.R')
source('UI Functions/BAT UI/volcano.R')

batTab <- navbarMenu("BAT",
                     tabPanel('Analysis', includeHTML('HTML and Rmd FIles/Analysis.html')),
                     barPlot,
                     goTerm,
                     volcano,
                     tabPanel('Heat Maps')
)
