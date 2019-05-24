goTermsUI <- tabPanel('GO Term Analysis',
                   sidebarPanel(
                     tags$p(strong('Please be patient,'), em('it may take a minute to display the plot.')),
                     tags$hr(),
                     tags$p("To analyze GO Terms, select the grouping parameters that you're interested in.
                               For example, to see how genes are differentially expressed
                               between the cort pellet and sham pellet across all days, select Treatment and Delivery and uncheck Day. 
                               Next, select your numerator, denominator, and cutoffs for FDR corrected p-values and fold change before clicking Plot."),
                     checkboxGroupInput("goGroups", 'Grouping Variables',
                                        c('Treatment' = 'Treatment','Delivery' = 'Delivery', 'Day' = 'Day'),
                                        selected = c('Treatment', 'Delivery'), inline = TRUE),
                     selectInput("goNumerator", "Numerator:", choices = c()),
                     selectInput("goDenominator", "Denominator:", choices = c()),
                     tags$hr(),
                     tags$p('Select the pvalue and fold change cutoff'),
                     radioButtons("goPChoice", "Choose p-value",
                                  choices = c(`FDR Adjusted` = "padj",
                                              `Non Adjusted` = "pval"),
                                  selected = "padj", inline = T),
                     numericInput('goPValCut', 'pvalue Cutoff', value = 0.05, min = 0, max = 1, step = 0.01),
                     numericInput('goFCCut', 'Fold Change Cutoff', value = 1.5, step = 0.25, min = 1),
                     actionButton('goPlotButton','Plot'),
                     tags$hr(),
                     tags$p(strong('Download')),
                     h5("PDF Dimensions"),
                     splitLayout(
                       numericInput("goWidth", "Width (in)", value = 10),
                       numericInput("goHeight", "Height (in)", value = 5)
                     ),
                     
                     downloadButton("goDownloadPlot", "Export plot as PDF"),
                     
                     # Download Data Settings
                     downloadButton("goDownloadData", "Export table as CSV")
                     
                   ),
                   mainPanel(
                     plotOutput('goPlot', brush = brushOpts('goPlot_brush')),
                     tabsetPanel(
                       tabPanel('GO Terms Table', DT::dataTableOutput('goTermTable')),
                       tabPanel('Gene List', DT::dataTableOutput('goTermGenes'))
                     )
                   )
)