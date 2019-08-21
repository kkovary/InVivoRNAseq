watBarPlotUI <- tabPanel('Bar Plots',
                      sidebarPanel(
                        titlePanel(strong("WAT RNA-seq Plots")),
                        h6(em("Stefan Tholen, Kyle M. Kovary, Mary N. Teruel")),
                        
                        textInput("WATgenes", "Input a single gene:", value = "Pparg"),
                        
                        h3(strong("Download")),
                        
                        # Download Plot Settings
                        h5("PDF Dimensions"),
                        splitLayout(
                          textInput("WATwidth", "Width (in)", value = 10),
                          textInput("WATheight", "Height (in)", value = 5)
                        ),
                        
                        downloadButton("WATdownloadPlot", "Export plot as PDF"),
                        
                        # Download Data Settings
                        downloadButton("WATdownloadData", "Export table as CSV")
                        
                      ),
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Plots", 
                                   plotOutput("WATplot")
                          ),
                          tabPanel("Data Table", 
                                   tableOutput("WATtable")
                          )
                        )
                      )
)