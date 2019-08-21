watGoTermsUI <- tabPanel('GO Term Analysis',
                      sidebarPanel(
                        tags$p(strong('Please be patient,'), em('it may take a minute to display the plot.')),
                        tags$hr(),
                        tags$p("To analyze GO Terms, select the grouping parameters that you're interested in.
                               For example, to see how genes are differentially expressed
                               between the cort pellet and sham pellet across all days, select Treatment and Delivery and uncheck Day. 
                               Next, select your numerator, denominator, and cutoffs for FDR corrected p-values and fold change before clicking Plot."),
                        checkboxGroupInput("WATgoGroups", 'Grouping Variables',
                                           c('Treatment' = 'Treatment','Delivery' = 'Delivery', 'Day' = 'Day'),
                                           selected = c('Treatment', 'Delivery'), inline = TRUE),
                        selectInput("WATgoNumerator", "Numerator:", choices = c()),
                        selectInput("WATgoDenominator", "Denominator:", choices = c()),
                        tags$hr(),
                        tags$p('Select the pvalue and fold change cutoff'),
                        radioButtons("WATgoPChoice", "Choose p-value",
                                     choices = c(`FDR Adjusted` = "padj",
                                                 `Non Adjusted` = "pval"),
                                     selected = "padj", inline = T),
                        numericInput('WATgoPValCut', 'pvalue Cutoff', value = 0.05, min = 0, max = 1, step = 0.01),
                        numericInput('WATgoFCCut', 'Fold Change Cutoff', value = 1.5, step = 0.25, min = 1),
                        actionButton('WATgoPlotButton','Plot'),
                        tags$hr(),
                        tags$p(strong('Download')),
                        h5("PDF Dimensions"),
                        splitLayout(
                          numericInput("WATgoWidth", "Width (in)", value = 10),
                          numericInput("WATgoHeight", "Height (in)", value = 5)
                        ),
                        
                        downloadButton("WATgoDownloadPlot", "Export plot as PDF")
                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel('GO Term Plots', plotOutput('WATgoPlot', brush = brushOpts('WATgoPlot_brush'))),
                          #tabPanel('Heat Map', d3heatmapOutput('goHeat', width = "100%", height = "600px"))
                          tabPanel("Interactive Heatmap", plotlyOutput("WATinteractive", height = "700px"))
                        ),
                        
                        tabsetPanel(
                          tabPanel('GO Terms Table', DT::dataTableOutput('WATgoTermTable')),
                          tabPanel('Gene List', DT::dataTableOutput('WATgoTermGenesTable')),
                          tabPanel('Uniprot Info', htmlOutput('WATgoPlotUniprot')),
                          tabPanel("Bar Plot", plotOutput("goWATplot"))
                        )
                      )
)