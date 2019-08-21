
shinyServer(function(input, output, session) {
  
  # BAT Server Functions
  source('./server/BAT/volcanoServer.R', local = TRUE)
  source('./server/BAT/barPlotServer.R', local = TRUE)
  source('./server/BAT/goTermsServer.R', local = TRUE)
  
  # WAT Server Functions
  source('./server/WAT/watBarPlotServer.R', local = TRUE)
  source('./server/WAT/watGoTermsServer.R', local = TRUE)
  source('./server/WAT/watVolcanoServer.R', local = TRUE)
  
})
