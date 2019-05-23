
shinyServer(function(input, output, session) {
  
  # BAT Server Functions
  source('Server Functions/BAT/volcano.R', local = TRUE)
  source('Server Functions/BAT/barPlot.R', local = TRUE)
  
  # WAT Server Functions
  source('Server Functions/WAT/watServer.R', local = TRUE)
  
})
