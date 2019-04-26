library(shiny)
library(httr)

# Load HTML from Box
#request <- GET(url="https://stanfordmedicine.box.com/shared/static/fiuukx0xhuupthptm51jo33cv8epo43l.html")
#writeLines(content(request, as="text"), file('box.html'))

shinyUI(
  navbarPage('InVivoRNAseq',
             tabPanel('BAT Analysis',
                      fluidPage(
                        #htmlOutput("includeHTML")
                        includeHTML('box.html')
                      )
             ),
             tabPanel('BAT Plots',
                      sidebarPanel(
                        titlePanel(strong("BAT RNA-seq Plots")),
                        h6(em("Stefan Tholen, Kyle Kovary, Mary N. Teruel")),
                        
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
                      ),
             tabPanel('WAT Plots',
                      sidebarPanel(
                        titlePanel(strong("WAT RNA-seq Plots")),
                        h6(em("Stefan Tholen, Kyle Kovary, Mary N. Teruel")),
                        checkboxGroupInput("Condition", "Condition to Plot:",
                                           c("Control_na" = "Control_na",
                                             "Cort_injection" = "Cort_injection",
                                             "Cort_pellet" = "Cort_pellet",
                                             "PBS_injection" = "PBS_injection",
                                             "Placebo_pellet" = "Placebo_pellet"),
                                           selected = c("Control_na","Cort_injection","Cort_pellet","PBS_injection","Placebo_pellet"),
                                           inline = TRUE),
                        
                        checkboxGroupInput("Day", "Time Points to Plot (Days):",
                                           c("0" = 0,"3" = 3, "7" = 7, "14" = 14),
                                           selected = c(0,3,7,14),
                                           inline = TRUE),
                        
                        textInput("WATgenes", "Genes (separate by space):", value = "Pparg"),
                        
                        
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
             )
  )
