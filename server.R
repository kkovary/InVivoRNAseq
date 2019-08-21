
shinyServer(function(input, output, session) {
  
  # BAT Server Functions
  source('Server_Functions/BAT/volcanoServer.R', local = TRUE)
  source('Server_Functions/BAT/barPlotServer.R', local = TRUE)
  source('Server_Functions/BAT/goTermsServer.R', local = TRUE)
  
  # WAT Server Functions
  source('Server_Functions/WAT/watBarPlotServer.R', local = TRUE)
  source('Server_Functions/WAT/watGoTermsServer.R', local = TRUE)
  source('Server_Functions/WAT/watVolcanoServer.R', local = TRUE)
  
})
