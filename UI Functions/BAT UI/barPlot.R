barPlot <- tabPanel('Bar Plots',
                       sidebarPanel(
                         titlePanel(strong("BAT RNA-seq Plots")),
                         h6(em("Stefan Tholen, Kyle M. Kovary, Mary N. Teruel")),
                         
                         textInput("BATgenes", "Input a single gene:", value = "Pparg"),
                         
                         h3(strong("Download")),
                         
                         # Download Plot Settings
                         h5("PDF Dimensions"),
                         splitLayout(
                           textInput("BATwidth", "Width (in)", value = 10),
                           textInput("BATheight", "Height (in)", value = 5)
                         ),
                         
                         downloadButton("BATdownloadPlot", "Export plot as PDF"),
                         
                         # Download Data Settings
                         downloadButton("BATdownloadData", "Export table as CSV")
                         
                       ),
                       
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Plots", 
                                    plotOutput("BATplot")
                           ),
                           tabPanel("Data Table", 
                                    tableOutput("BATtable")
                           )
                         )
                       )
)