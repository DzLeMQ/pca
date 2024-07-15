#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  # default data set
  url = "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"
  
  drop_cols_fn <-function(the_data){
    the_data <- the_data[,!colnames(the_data) %in% c("id","X","x"), drop = FALSE]
    #only for breast cancer dataset used only 
    if (c("diagnosis" ) %in% names(the_data)){
      if (which(colnames(the_data) == c("diagnosis" )) == 1){
        diagnosis <- the_data$diagnosis
        the_data <- the_data[,!colnames(the_data) %in% c("diagnosis"), drop = FALSE]
        the_data$diagnosis <- diagnosis
      }
    }
    return (the_data)
  }
  
  the_data_fn <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile)) {
      inFile$datapath <- url
    }

    the_data <-   read.csv(inFile$datapath, header = (input$header == "Yes"),
                           sep = input$sep, quote = input$quote, stringsAsFactors=FALSE)

    the_data <- drop_cols_fn(the_data)

    return(the_data)
    
  })
  
  output$contents <-  DT::renderDataTable(data.frame(the_data_fn())
                                          ,options = list(scrollX = TRUE, pageLength = 15))
  
  correlation <- reactive({
    if(is.null(the_data_fn())) return()
    the_data <- the_data_fn() %>% na.omit()
    val <- the_data %>%
        sample_n(569) %>%
        mutate_if(is.factor, as.numeric) %>%
        dplyr::select(diagnosis, everything()) %>%
        cor %>%
        {.[order(abs(.[, 1]), decreasing = TRUE),
           order(abs(.[, 1]), decreasing = TRUE)]}
    
  })
  
  
  output$choose_columns_tabplot <- renderUI({

    the_data <- the_data_fn()
    if (is.null(the_data)) return (NULL)

    # exclude cols with zero variance
    the_data <- the_data[,!apply(the_data, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    colnames <- names(the_data)
 
   
    if (is.null(input$columns_tabplot)) {
     
      ifelse (length(colnames) < 8,  selected_columns <- c(colnames), selected_columns <- c(colnames[1:7], colnames[length(colnames)]))
      
      updateSelectInput(session, "columns_tabplot", choices = c(colnames), selected = selected_columns)
    }
    output$dropbox_guide <-renderUI("Click in the while space of the features box for a dropdown to select more features. Click on a feature to highlight it + Del to remove a feature")
    
    # # Create the dropdown list and select them all by default
    selectInput(inputId = "columns_tabplot", label = "Choose Columns/Features:", multiple = TRUE,
                choices = c(colnames),
                selected = input$columns_tabplot)

  })
  
  
  # tableplot
  output$tableplot <- renderPlot({
      
    if(is.null(the_data_fn())) return()

    the_data <- the_data_fn() 
    options(ffbatchbytes = 1024^2 * 128) # 128 MB
    options(ffmaxbytes = 1024^2 * 128 * 32) # 4096 MB
    
    ifelse((is.null(input$columns_tabplot)), return(), columns_tabplot <-    input$columns_tabplot)
    
    the_data_subset <- the_data[, columns_tabplot, drop = FALSE]
  
    
    plotdata <- tabplot::tableplot(the_data_subset)
    
    plot(plotdata,title="",
         fontsize=10, max_print_levels = 15,
         legend.lines=9,
         fontsize.title=0, show.legend = FALSE)
    
  })
  
  # Check boxes to choose columns
  output$choose_columns_biplot <- renderUI({
    
    the_data <- the_data_fn()
    
    colnames <- names(the_data)
    
    if (is.null(input$columns_biplot)) {
      ifelse (length(colnames) < 8,  selected_columns <- c(colnames), selected_columns <- c(colnames[1:10], colnames[length(colnames)]))
      updateSelectInput(session, "columns_biplot", choices = c(colnames), selected = selected_columns)
    }
    
   
    selectInput(inputId = "columns_biplot", label = "Choose Columns/Features", multiple = TRUE,
                choices = c(colnames),
                selected = input$columns_biplot)
  })
  
  # corr plot
  output$corr_plot <- renderPlot({
    the_data <- the_data_fn()
    # Keep the selected columns
    if (is.null(input$columns_biplot)) return(NULL)
    columns_biplot <-    input$columns_biplot
    the_data_subset_biplot <- the_data[, columns_biplot, drop = FALSE]
    ggpairs(the_data_subset_biplot, upper = list (continuous = wrap(ggally_cor, displayGrid = FALSE)))
  })
  
  # corr tables
  output$corr_tables <- renderDataTable({
    the_data <- the_data_fn()
   
    
    if (is.null(input$columns_biplot)) return(NULL)
    columns_biplot <-    input$columns_biplot
    the_data <- the_data[, columns_biplot, drop = FALSE]
    
    # we only want to show numeric cols
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]

    res <- Hmisc::rcorr(as.matrix(the_data_num))
    cormat <- res$r
    pmat <- res$P
    ut <- upper.tri(cormat)
    df <- data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  = (cormat)[ut],
      p = pmat[ut]
    )
    
    with(df, df[order(-cor), ])
    
  },options = list(paging = TRUE, pageLength = 10), rownames = FALSE)
  
  
  output$choose_columns_pca <- renderUI({
    
    the_data <- the_data_fn()
    if (is.null(the_data)) return (NULL)
    # we only want to show numeric cols
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
 
    colnames <- names(the_data_num)
       
    if (is.null(input$columns)) {
      ifelse (length(colnames) < 8,  selected_columns <- c(colnames), selected_columns <- c(colnames[1:7], colnames[length(colnames)]))
      updateSelectInput(session, "columns", choices = c(colnames), selected = selected_columns )
    }
    
    # Create the dropdown list and select them all by default
    selectInput(inputId = "columns", label = "Choose Columns/Features", multiple = TRUE,
                choices = c(colnames),
                selected = input$columns)
  
  })
  
  output$pca_summary <- renderPrint({ 
    if (is.null(pca_objects()$the_data)) return (NULL)
    #print(pca_objects()$pca_output$rotation)
    summary(pca_objects()$pca_output)
    
  })
  
  output$pca_rotation <- renderPrint({ 
    if (is.null(pca_objects()$the_data)) return (NULL)
    print(pca_objects()$pca_output$rotation)
  })
  

  pca_objects <- reactive({
    # Keep the selected columns
    
    if (is.null(input$columns)) return(NULL)
  
    columns <-    input$columns
    
    the_data <- na.omit(the_data_fn())
    
    
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
  
    if (is.null(the_data_subset)) return (NULL)

    # from http://rpubs.com/sinhrks/plot_pca
    pca_output <- prcomp(na.omit(the_data_subset), 
                         center = (input$center == 'Yes'), 
                         scale. = (input$scale. == 'Yes'))
    
    pcs_df <- cbind(the_data, pca_output$x)

    return(list(the_data = the_data,
                the_data_subset = the_data_subset,
                pca_output = pca_output,
                pcs_df = pcs_df))

  })
 
  # display a summary of the CSV contents
  output$summary <-  renderDataTable({
    
    the_data <- the_data_fn()
    
    
    # #the_data_num <- the_data[,sapply(the_data, is.u)]
    # 
    #the_data <- the_data %>%
    #  dplyr::select(-Gender, everything())

    output$tableHeader <- renderUI(tags$div(tags$h4("Data Summary Statistics (Excluded non-numeric data classes)")))
    the_data <- the_data[,sapply(the_data, is.numeric)]
    psych::describe(the_data)
  },options = list(scrollX = FALSE, paging = TRUE, pageLength = 10),rownames = FALSE)
  
  output$plot2 <- renderPlot({
    pca_output <- pca_objects()$pca_output
    if (is.null(pca_output))
      return (NULL)
    else{
    eig = (pca_output$sdev)^2
    variance <- eig*100/sum(eig)
    cumvar <- paste(round(cumsum(variance),1), "%")
    eig_df <- data.frame(eig = eig,
                         PCs = colnames(pca_output$x),
                         cumvar =  cumvar)
    ggplot(eig_df, aes(reorder(PCs, -eig), eig)) +
      geom_bar(stat = "identity", fill = "white", colour = "black") +
      geom_text(label = cumvar, size = 4,
                vjust=-0.4) +
      theme_bw(base_size = 14) +
      xlab("PC") +
      ylab("Variances") +
      ylim(0,(max(eig_df$eig) * 1.1))
    }
  })
  # 
  output$the_pcs_to_plot_x <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    
    if (is.null(input$the_pcs_to_plot_x)) {
      #   #ifelse (length(colnames) < 8,  selected_columns <- c(colnames), selected_columns <- c(colnames[1:7], colnames[length(colnames)]))
      updateSelectInput(session, "the_pcs_to_plot_x", choices = c(colnames(pca_output)), selected = 'PC1')
    }
    

    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_x",
                label = "X axis:",
                choices= colnames(pca_output),
                selected = input$the_pcs_to_plot_x)
  })

  output$the_pcs_to_plot_y <- renderUI({
    pca_output <- pca_objects()$pca_output$x
    
    if (is.null(input$the_pcs_to_plot_y)) {
      ifelse (length(colnames(pca_output)) >= 2,  selected_columns <- c("PC2"), selected_columns <- c("PC1"))
      updateSelectInput(session, "the_pcs_to_plot_y", choices = c(colnames(pca_output)), selected = selected_columns)
    }
    # drop down selection
    selectInput(inputId = "the_pcs_to_plot_y",
                label = "Y axis:",
                choices= colnames(pca_output),
                selected = input$the_pcs_to_plot_y)
  })
  #
  # for zooming
  output$z_plot1 <- renderPlot({
    
    output$plot_zoom_guide <-renderUI(
    "Click and drag on the plot (left) to zoom into a region on the plot.
      Alternatively, you can go directly to the second plot below to select points by clicking and dragging around the points.
      Monitor the table below the second plot to get more information about the selected points.")
    pca_biplot()


  })
  #
  # zoom ranges
  zooming <- reactiveValues(x = NULL, y = NULL)
  #
  #
  # # PC plot
  pca_biplot <- reactive({
    
    pcs_df <- pca_objects()$pcs_df
    
    if (is.null(pcs_df)) return (NULL)
   
    pca_output <-  pca_objects()$pca_output

    var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
    var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
    labels <- rownames(pca_output$x)

    ifelse (is.null(input$the_grouping_variable), grouping <- c("None"), grouping <- input$the_grouping_variable)

    if(grouping == 'None'){
      if (is.null(input$the_pcs_to_plot_x)) return(NULL)
      if (is.null(input$the_pcs_to_plot_y)) return(NULL)

      # plot without grouping variable
      pc_plot_no_groups  <- ggplot(pcs_df,
                                   aes_string(input$the_pcs_to_plot_x,
                                              input$the_pcs_to_plot_y
                                   )) +

        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)"))
      # the plot
      pc_plot_no_groups
      

    } else {
      # plot with grouping variable

      pcs_df$fill_ <-  as.character(pcs_df[, grouping, drop = TRUE])
      pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x,
                                                   input$the_pcs_to_plot_y,
                                                   fill = 'fill_',
                                                   colour = 'fill_'
      )) +
        stat_ellipse(geom = "polygon", alpha = 0.1) +

        geom_text(aes(label = labels),  size = 5) +
        theme_bw(base_size = 14) +
        scale_colour_discrete(guide = FALSE) +
        guides(fill = guide_legend(title = "groups")) +
        theme(legend.position="top") +
        coord_equal() +
        xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
        ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)"))
      # the plot
      pc_plot_groups
    }
    
  })
  # choose a grouping variable
  output$the_grouping_variable <- renderUI({

    the_data <- the_data_fn()

    # for grouping we want to see only cols where the number of unique values are less than
    # 10% the number of observations

    grouping_cols <- sapply(seq(1, ncol(the_data)), function(i) length(unique(the_data[,i])) < nrow(the_data)/10 )

    the_data_group_cols <- the_data[, grouping_cols, drop = FALSE]

    if (is.null(input$the_grouping_variable)) {

    #   #ifelse (length(colnames) < 8,  selected_columns <- c(colnames), selected_columns <- c(colnames[1:7], colnames[length(colnames)]))
       updateSelectInput(session, "the_grouping_variable", choices = c('None', names(the_data_group_cols)), selected = 'None')
    }

    output$grouping_var_guide <-renderUI("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful).")

    # drop down selection
    selectInput(inputId = "the_grouping_variable",
                label = "Grouping variable:",
                #choices = input$the_group_variable)
                choices=c("None", names(the_data_group_cols)),
                selected = input$the_grouping_variable)

  })
  
  brush <- reactive({
    input$z_plot1Brush
  })
  
  # observe({
  #   
  #   if (!is.null(brush)) {
  #     zooming$x <- c(brush$xmin, brush$xmax)
  #     zooming$y <- c(brush$ymin, brush$ymax)
  #   }
  #   else {
  #     zooming$x <- NULL
  #     zooming$y <- NULL
  #   }
  # })

  # # # for zooming
  output$z_plot2 <- renderPlot({
      if (is.null(pca_biplot())) return (NULL)
      pca_biplot() + coord_cartesian(xlim = zooming$x, ylim = zooming$y)
  
  })
  output$brush_info_after_zoom <- renderTable({
    # the brushing function
    brushedPoints(pca_objects()$pcs_df, input$plot_brush_after_zoom)
  })
  
  output$bartlett <- renderDataTable({
    the_data <- the_data_fn()
    the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    bdata <-data.frame(cortest.bartlett(cor(the_data_num), n = nrow(the_data_num)))
    names(bdata) <- c("Chi-Square", "P-Value", "Degree of Freedom")
    
    if (!is.null(bdata)) output$barlettHeader <- renderUI(tags$div(tags$h4("Summary by Bartllet's Test of Sphericity")))
    
    bdata
    
  },options = list(paging = FALSE, pageLength = 5,  bLengthChange=0,  # show/hide records per page dropdown
    bFilter=0,  # global search box on/off
    bInfo=0),   # information on/off (how many records filtered, etc), 
    rownames = FALSE)  
  
  output$kmo <- renderPrint({
    the_data <- the_data_fn()
    the_data_num <- the_data[,sapply(the_data,is.numeric)]
    
    # exclude cols with zero variance
    the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
    
    # http://www.opensubscriber.com/message/r-help@stat.math.ethz.ch/7315408.html
    # KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy 
    kmo = function( data ){ 
      
      library(MASS) 
      X <- cor(as.matrix(data)) 
      iX <- ginv(X) 
      S2 <- diag(diag((iX^-1))) 
      AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix 
      IS <- X+AIS-2*S2                         # image covariance matrix 
      Dai <- sqrt(diag(diag(AIS))) 
      IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix 
      AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix 
      a <- apply((AIR - diag(diag(AIR)))^2, 2, sum) 
      AA <- sum(a) 
      b <- apply((X - diag(nrow(X)))^2, 2, sum) 
      BB <- sum(b) 
      MSA <- b/(b+a)                        # indiv. measures of sampling adequacy 
      
      AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the 
      # correlation matrix. That is the 
      # negative of the partial correlations, 
      # partialling out all other variables. 
      
      kmo <- BB/(AA+BB)                     # overall KMO statistic 
      
      # Reporting the conclusion 
      if (kmo >= 0.00 && kmo < 0.50){ 
        test <- 'The KMO test yields a degree of common variance 
        unacceptable for FA.' 
      } else if (kmo >= 0.50 && kmo < 0.60){ 
        test <- 'The KMO test yields a degree of common variance miserable.' 
      } else if (kmo >= 0.60 && kmo < 0.70){ 
        test <- 'The KMO test yields a degree of common variance mediocre.' 
      } else if (kmo >= 0.70 && kmo < 0.80){ 
        test <- 'The KMO test yields a degree of common variance middling.' 
      } else if (kmo >= 0.80 && kmo < 0.90){ 
        test <- 'The KMO test yields a degree of common variance meritorious.' 
      } else { 
        test <- 'The KMO test yields a degree of common variance marvelous.' 
      } 
      
      ans <- list(  overall = kmo, 
                    report = test, 
                    individual = MSA, 
                    AIS = AIS, 
                    AIR = AIR ) 
      return(ans) 
      
    }    # end of kmo() 
    
    table(kmo(na.omit(the_data_num)))
    
  }) 
  
  observeEvent(input$sidebars,
               {
                 # for desktop browsers
                 addClass(selector = "body", class = "sidebar-collapse")
                 # for mobile browsers
                 #removeClass(selector = "body", class = "sidebar-open")
               })
  
})
