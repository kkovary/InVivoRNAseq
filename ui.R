shinyUI(
  navbarPage('InVivoRNAseq',
             
             tabPanel('Introduction',
                      includeHTML('HTML and Rmd FIles/Introduction.html')
             ),
             navbarMenu("BAT",
                        tabPanel('Analysis',
                                 includeHTML('HTML and Rmd FIles/Analysis.html')),
                        tabPanel('Bar Plots',
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
                                 )),
                        tabPanel('GO Term Analysis'),
                        tabPanel('Volcano Plots',
                                 sidebarPanel(
                                   tags$p(strong('Please be patient,'), em('it may take a minute to display the plot.')),
                                   tags$hr(),
                                   tags$p("To create volcano plots, select the grouping parameters that you're interested in.
                               For example, to see how genes are differentially expressed
                               between the cort pellet and sham pellet across all days, select Treatment and Delivery and unckec Day. 
                               Next, select your numerator, denominator, and cutoffs for FDR corrected p-values and fold change before clicking Plot."),
                                   checkboxGroupInput("volGroups", 'Grouping Variables',
                                                      c('Treatment' = 'Treatment','Delivery' = 'Delivery', 'Day' = 'Day'),
                                                      selected = c('Treatment', 'Delivery'), inline = TRUE),
                                   selectInput("volNumerator", "Numerator:", choices = c()),
                                   selectInput("volDenominator", "Denominator:", choices = c()),
                                   tags$hr(),
                                   tags$p('Select the pvalue and fold change cutoff'),
                                   radioButtons("pChoice", "Choose p-value",
                                                choices = c(`FDR Adjusted` = "padj",
                                                            `Non Adjusted` = "pvalue"),
                                                selected = "padj", inline = T),
                                   numericInput('pvalCut', 'pvalue Cutoff', value = 0.05, min = 0, max = 1, step = 0.01),
                                   numericInput('fcCut', 'Fold Change Cutoff', value = 1.5, step = 0.25, min = 1),
                                   actionButton('volPlotButton','Plot'),
                                   tags$hr(),
                                   tags$p(strong('Download')),
                                   h5("PDF Dimensions"),
                                   splitLayout(
                                     numericInput("volWidth", "Width (in)", value = 10),
                                     numericInput("volHeight", "Height (in)", value = 5)
                                   ),
                                   
                                   downloadButton("volDownloadPlot", "Export plot as PDF"),
                                   
                                   # Download Data Settings
                                   downloadButton("volDownloadData", "Export table as CSV")
                                   
                                 ),
                                 mainPanel(
                                   plotOutput('volPlot', brush = brushOpts('plot_brush')),
                                   tabsetPanel(
                                     tabPanel('Hits Table', DT::dataTableOutput('volGroupHits')),
                                     tabPanel('UniProt Info', htmlOutput('volPlotUniprot')),
                                     tabPanel("Bar Plot", plotOutput("volBATplot"))
                                     )
                                 )
                        ),
                        tabPanel('Heat Maps')
             ),
             
             navbarMenu("WAT",
                        tabPanel('Analysis'),
                        tabPanel('Bar Plots',
                                 sidebarPanel(
                                   titlePanel(strong("WAT RNA-seq Plots")),
                                   h6(em("Stefan Tholen, Kyle M. Kovary, Mary N. Teruel")),
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
                                 )),
                        tabPanel('GO Term Analysis'),
                        tabPanel('Volcano Plots'),
                        tabPanel('Heat Maps')
             )
  )
  
)

