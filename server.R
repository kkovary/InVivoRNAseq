
shinyServer(function(input, output, session) {
  
  source('Server Functions/BAT/batServer.R', local = TRUE)
  source('Server Functions/WAT/watServer.R', local = TRUE)
  
})
