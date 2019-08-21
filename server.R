
shinyServer(function(input, output, session) {
  
  # BAT Server Functions
  source('Server Functions/BAT/volcanoServer.R', local = TRUE)
  source('Server Functions/BAT/barPlotServer.R', local = TRUE)
  source('Server Functions/BAT/goTermsServer.R', local = TRUE)
  
  # WAT Server Functions
  source('Server Functions/WAT/watBarPlotServer.R', local = TRUE)
  source('Server Functions/WAT/watGoTermsServer.R', local = TRUE)
  source('Server Functions/WAT/watVolcanoServer.R', local = TRUE)
  
})
