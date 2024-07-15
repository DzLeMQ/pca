dbHeader <- dashboardHeader(title = "Interactive Principle Component Analysis (PCA) Explorer",titleWidth = 450,
                            tags$li(a(href = 'http://',
                                      icon("envelope-o"),
                                      title = "Contact Us"),
                                    class = "dropdown"))

#Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dbHeader,
    dashboardSidebar(
      sidebarMenu(id = "sidebars",
      
        menuItem("", tabName  =  "raw")
        
      )
    ),
    dashboardBody(
      useShinyjs(),
    tabsetPanel(
       
        tabPanel("Load Data", 
          br(),
          
          p("This tool lets you inspect your data in csv file and compute Principle Component Analysis, and present the outputs in several forms of plots and tables."),
          #p("Before uploading your data, check that it is clean, especially ensure that the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
          p("Before you update your data here are a few points you need to take note - rows that contain one or more NAs and columns that contain a mixture of numbers and text will not be included in the computation of the PCA results."),
          p("Have a look at the ", a("iris.csv", href = "https://raw.githubusercontent.com/benmarwick/Interactive_PCA_Explorer/master/iris.csv"),  
            " file for your reference of what a clean CSV file looks like."), 
         
          p("The ", a("WisconsinCancer.csv ", href = "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"),  
            "  dataset has been uploaded by default once you start this tool. You can start to explore by navigating the tabs 
                       (e.g. click on 'Inspect Raw Data')", style  =  "text-align:center;color:Green;font-size:120%"), 
          
          # tags$hr(),
          p("If you have your own dataset then select the options that match your CSV file, then upload your file:"),
                    
          radioButtons(inputId = 'header',inline = TRUE, 
                       label = 'Header',
                       choices = c('Columns have headers'='Yes',
                                 'Columns do not have headers'='No'), 
                       selected = 'Yes'),
                      
          radioButtons('sep', 'Separator', inline = TRUE,
                      c(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                      ','),
                      
          radioButtons('quote', 'Quote', inline = TRUE,
                     c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                     '"'),
                      
          tags$hr(),
                
          fileInput('file1', 'Choose a CSV file to upload:',
                    accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.tsv'
                    )),
          p("After uploading your CSV file, start to explore your data by clicking on the 'Inspect Raw Data' tab"),br(),
          p("This project is referenced to the work at ", a("IPCA", href = "https://github.com/benmarwick/Interactive_PCA_Explorer"))
        ),
        tabPanel("Inspect Raw Data",
                 DT::dataTableOutput('contents')
                 # tableOutput('summary')),
                 
        ), # end  tab
        tabPanel("Inspect Data Summary",
                  plotOutput("tableplot"),
                  tags$hr(),
                   
                  fluidRow(
                      column (width = 3, uiOutput("choose_columns_tabplot"),
                      uiOutput("dropbox_guide")),        
                      #p("Click in the while space of the features box for a dropdown to select more features. Click on a feature to highlight it + Del to remove a feature.")),
                      #uiOutput("tableHeader"),                     
                      column (width = 9, uiOutput("tableHeader"), dataTableOutput('summary'))
                  )
                  
        ), # end  tab
        
        tabPanel("Correlation Plots",
                   fluidRow(column (width = 2, uiOutput("choose_columns_biplot")),
                            column (width = 6, br(p("Click in the while space of the features box to select more features. Click on a feature (to highlight) + Del to remove a feature."))
                                    ),
                            column (width = 4, br(br(p("The Summary of correlations table below shows only numeric columns and excludes columns with zero variance")))
                            )
                    ),
                  
                   #tags$hr(),
                   fluidRow(column (width = 8,
                      p("This plot may take a few moments to appear when analysing large datasets. You may want to exclude highly correlated variables from the PCA."),
                      plotOutput("corr_plot",  height=600)),
                   
                      column (width = 4, h4("Summary of correlations"),
                          dataTableOutput("corr_tables", width = 500))
                   )
        ), # end  tab
        tabPanel("PCA Output Computation", 
                   
                  br(),
                  column(width = 1, align = "left",# p("Choose the columns/features of your data to include in the PCA."),
                         br(p("Only columns containing numeric data are listed as PCA doesn't support non-numeric data.")),
                         p("The PCA is automatically re-computed each time a selection is made."),
                         p("Observations (ie. rows) are automatically removed if they contain any missing values."),
                         p("Variables with zero variance have been automatically removed because they're not useful in a PCA.")
                  ), 
                  column(width = 2, align = "left", 
                        uiOutput("choose_columns_pca"),
                        radioButtons(inputId = 'center',
                                    label = 'Center',
                                    choices = c('Shift variables to be zero centered'='Yes',
                                            'Do not shift variables'='No'),
                                    selected = 'Yes'),

                        # column(width = 2, align = "left", 
                        radioButtons(inputId = 'scale.', label = 'Scale', 
                                    choices = c('Scale variables to have unit variance'='Yes',
                                              'Do not scale variables'='No'),
                                    selected = 'Yes'))
                   ,
                 
                  column(width = 1, br(p("Click on the white space area in the Features box to select more features/columns)"))),
                  
                  column(width = 4, align = "left", verbatimTextOutput("pca_rotation")),
                  tags$head(tags$style("#pca_rotation{color::black; font-size:12px; overflow-y:scroll; max-height: 500px; background: ghostwhite;}")),
                  column(width = 4, align = "left", verbatimTextOutput("pca_summary")),
                  tags$head(tags$style("#pca_summary{color:black; font-size:12px; overflow-y:scroll; max-height: 500px; background: ghostwhite;}"))
                  
        ), # end  tab
          
          
        tabPanel("PC Plots", 
                  fluidRow(column(width = 2, h2("Screen Plot")),
                    column (width = 10, br(p("Plots in this tap are automatically updated based on features selected in PCA Output Computation tab. The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %).")))),
                  fluidRow(
                    plotOutput("plot2", height = "250px"),
                    tags$hr()),
                  fluidRow(
                    column(width = 3,
                      uiOutput("grouping_var_guide"),
                      br(uiOutput("the_grouping_variable")),
                      uiOutput("the_pcs_to_plot_x"),
                      uiOutput("the_pcs_to_plot_y")),

                    column(width = 6, plotOutput ("z_plot1", height = 300,
                                                  brush = brushOpts(
                                                    id = "z_plot1Brush",
                                                    resetOnNew = TRUE))),

                    column(width = 3,
                      uiOutput("plot_zoom_guide"))     
                      
                 ),
                 plotOutput("z_plot2", height = 400,
                            brush = brushOpts(
                              id = "plot_brush_after_zoom",
                              resetOnNew = TRUE)),
                 tags$hr(),
                 p("Details of the brushed points"),
                 tableOutput("brush_info_after_zoom")
                
        ), # end  tab
          
          
          # 
          # tabPanel("PCA Output",
          # 
          #          verbatimTextOutput("pca_details")
          # 
          # ), # end  tab
        tabPanel("Diagnostic",
                 br(p("Among SPSS users, these tests are considered providing some guidelines on the suitability of the data for a principal components analysis. However, they may be safely ignored in favour of common sense. Variables with zero variance are excluded.")),
                
                 uiOutput("barlettHeader"),
                 dataTableOutput("bartlett", width = 380),
                 br(p("Bartlett's test of sphericity tests whether the data comes from multivariate normal distribution with zero covariances. If p > 0.05 then PCA may not be very informative"))
                 #p("Here is the output of the Kaiser-Meyer-Olkin (KMO) index test. The overall measure varies between 0 and 1, and values closer to 1 are better. A value of 0.6 is a suggested minimum. "),
                 #verbatimTextOutput("kmo")                
             
         )
       )#end tabsetpanel
    )#end dashboard body                                       
  )#DashboardPage
)#UI


