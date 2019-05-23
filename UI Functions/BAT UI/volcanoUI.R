volcanoUI <- tabPanel('Volcano Plots',
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
)