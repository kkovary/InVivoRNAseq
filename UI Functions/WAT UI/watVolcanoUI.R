watVolcanoUI <- tabPanel('Volcano Plots',
                      sidebarPanel(
                        tags$p(strong('Please be patient,'), em('it may take a minute to display the plot.')),
                        tags$hr(),
                        tags$p("To create volcano plots, select the grouping parameters that you're interested in.
                               For example, to see how genes are differentially expressed
                               between the cort pellet and sham pellet across all days, select Treatment and Delivery and unckec Day. 
                               Next, select your numerator, denominator, and cutoffs for FDR corrected p-values and fold change before clicking Plot."),
                        checkboxGroupInput("WATvolGroups", 'Grouping Variables',
                                           c('Treatment' = 'Treatment','Delivery' = 'Delivery', 'Day' = 'Day'),
                                           selected = c('Treatment', 'Delivery'), inline = TRUE),
                        selectInput("WATvolNumerator", "Numerator:", choices = c()),
                        selectInput("WATvolDenominator", "Denominator:", choices = c()),
                        tags$hr(),
                        tags$p('Select the pvalue and fold change cutoff'),
                        radioButtons("WATpChoice", "Choose p-value",
                                     choices = c(`FDR Adjusted` = "padj",
                                                 `Non Adjusted` = "pvalue"),
                                     selected = "padj", inline = T),
                        numericInput('WATpvalCut', 'pvalue Cutoff', value = 0.05, min = 0, max = 1, step = 0.01),
                        numericInput('WATfcCut', 'Fold Change Cutoff', value = 1.5, step = 0.25, min = 1),
                        actionButton('WATvolPlotButton','Plot'),
                        tags$hr(),
                        tags$p(strong('Download')),
                        h5("PDF Dimensions"),
                        splitLayout(
                          numericInput("WATvolWidth", "Width (in)", value = 10),
                          numericInput("WATvolHeight", "Height (in)", value = 5)
                        ),
                        
                        downloadButton("WATvolDownloadPlot", "Export plot as PDF"),
                        
                        # Download Data Settings
                        downloadButton("WATvolDownloadData", "Export table as CSV")
                        
                      ),
                      mainPanel(
                        plotOutput('WATvolPlot', brush = brushOpts('WATplot_brush')),
                        tabsetPanel(
                          tabPanel('Hits Table', DT::dataTableOutput('WATvolGroupHits')),
                          tabPanel('UniProt Info', htmlOutput('WATvolPlotUniprot')),
                          tabPanel("Bar Plot", plotOutput("WATvolPlot"))
                        )
                      )
)