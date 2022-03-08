#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library (dplyr)
library (LSD)
library(vioplot)
library(shinyjs)
library(tidyr)
library(DT)
library(tibble)
library(DBI)
library(shinycssloaders)
library(RMySQL)

source('homepage_UI.R', local=TRUE)

options(mysql = list(
  "host" = "#",
  "port" = 3306,
  "user" = "#",
  "password" = "#"
))

databaseName <- "substitutions"

db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                port = options()$mysql$port, user = options()$mysql$user, 
                password = options()$mysql$password)

gene_query <- "SELECT gene_name AS 'Gene' FROM GENES;"
query_genes <- dbGetQuery(db, gene_query)
gene_list <- query_genes$Gene
genes <- append(gene_list, "", 0)


header <- dashboardHeader(title = 'ABPEPserver')

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Analyze", tabName = "Plots", icon = icon("chart-bar")),
    menuItem("W>F substitutants", tabName = "Download", icon = icon("table"))
))

body <- dashboardBody(
    useShinyjs(),
    tags$head(tags$script(src = 'ABPEPserverJS.js')),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ABPEPserverCSS.css")),
    tabItems(tabItem(tabName = "Plots", 
                     # App title ----
                     
                     fluidRow(# Sidebar layout with input and output definitions
                              sidebarLayout(# Sidebar panel for inputs 
                                  sidebarPanel(id = 'sidebar', 
                                               h2(id="big-heading", "Analyze W-Substitutants"),
                                               selectizeInput(inputId = 'cancertype2', label = 'Select cancer type to analyze:', choices = NULL),
                                               
                                               HTML("<b>Select plots: </b><br>"),
                                               
                                               checkboxInput(inputId = "barplot_box", "Barplot depicting number of W-substitutants"),
                                               checkboxInput(inputId = "scatterplot_box", "Scatter contour plot"),
                                               checkboxInput(inputId = "violinplot_box", "Violin plot"),
                                                                                     
                                               radioButtons(inputId = "interactive", "Make plots interactive", c("Yes", "No"), selected = "No", inline = TRUE),
                                               
                                               
                                               br(), br(),
                                               actionButton("plotButton", "Plot", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold;
                                      bottom: 25px; position: absolute;")
                                               
                                  ),
                                  # Main panel for displaying outputs ----
                                  mainPanel(hidden(plotlyOutput("barplot_interactive"),
                                                   plotOutput("barplot"), selectInput(inputId = "samples", 
                                                                                       label = "Select sample types:",
                                                                                       choices = c("All samples" = "all", "Tumour tissue samples" = "tumor",
                                                                                       "Normal tissue samples" = "normal"), selected = "all"))))),
                     fluidRow(hidden(div(id = "scatterbox", sidebarLayout(sidebarPanel(id = "sidebar2", selectizeInput(inputId = "gene_scatter", choices = genes, label = "Select gene: ")),
                                                                                                 mainPanel(plotlyOutput("scatterplot_interactive"),
                                                                                                           plotOutput("scatterplot", height = 450, width = "500px")), position = "right")))),
                     
                     fluidRow(hidden(div(id = "violinbox", uiOutput("violin_scatter"))))),
             
             # Second tab content
             tabItem(tabName = "home", homepage),
             tabItem(tabName = "Download", div(style ="position: relative; width: 100%; ",
                                               div(id = "table_page", selectInput(inputId = 'cancertype', label = 'Select cancer type:', 
                                                               choices = NULL),
                                               actionButton("load_data", "Load data", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold; "),
                                               uiOutput("download"), br(),
                                               helpText("W>F substitutants in the peptides are shown with a lowercase f. "),
                                               helpText("The column 'Total' accounts for the total occurences of the peptide in all samples.
                                               The column 'Σ Total' accounts for the total of occurences in tumor tissue samples and the column 'Σ Normal' accounts for the total of occurences in adj. normal tissue samples. 
                                               The following columns showcases for each sample whether the peptide is found."), br(), br()),
                                               div(style="display:inline-block; float: right; padding-left: 40px; padding-right:40px; padding-bottom: 10px; padding-top: 35px; width:70%;", plotlyOutput("pep_plot", width = "auto") %>% withSpinner())),
                                            DTOutput('substitutant_table'))))
                                            

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'blue',
                    header,
                    sidebar,
                    body)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    cancer_type_subs <- eventReactive(input$load_data, {
      full = input$cancertype
      type = cancer_types()
      type[type$name == full, ]$cancer_id
    })
    
    interactiv <- eventReactive(input$plotButton, {
      input$interactive
    })
    
    scatter_yes <- eventReactive(input$plotButton, {
      input$scatterplot_box
    })
    
    barplot_yes <- eventReactive(input$plotButton, {
      input$barplot_box
    })
    
    violinplot_yes <- eventReactive(input$plotButton, {
      input$violinplot_box
    })

    cancer_type <- eventReactive(input$plotButton, {
      full = input$cancertype2
      type = cancer_types()
      type[type$name == full, ]$cancer_id
    })
  
    
    
    dataset <- function(cancertype = input$cancertype) {
      query <- "SELECT SAMPLE.sample_id, WFpeptides.peptideWf AS 'Peptide', WFpeptides.proteinID AS 'ProteinID', WFpeptides.genesymbol AS 'Gene', WFpeptides.obs FROM WFpeptides 
        JOIN SAMPLE ON WFpeptides.sample_id = SAMPLE.sample_id  WHERE SAMPLE.cancer_id = 'cancertype' 
        AND WFpeptides.cancer_id = 'cancertype'"
      
      
      pep_query <- gsub("cancertype", cancertype, query)
      wfpeps <- dbGetQuery(db, pep_query)
      
      spreaded_peps <- spread(wfpeps, key = sample_id, value = obs)
      
      normal_sampleid <- suppressWarnings(normal_counts(cancertype))$id
      tumor_sampleid <- suppressWarnings(tumor_counts(cancertype))$id
      
      df <- data.frame(Peptide = spreaded_peps$Peptide, ProteinID = spreaded_peps$ProteinID, Gene = spreaded_peps$Gene,
                       Total = rowSums(spreaded_peps[, 4:ncol(spreaded_peps)]), 
                       "Total T" = rowSums(spreaded_peps[tumor_sampleid]),
                       "Total N" = rowSums(spreaded_peps[normal_sampleid]))
      
      
      normal_peps <- spreaded_peps[normal_sampleid]
      tumor_peps <- spreaded_peps[tumor_sampleid]
      
      normal_peps$Peptide <- spreaded_peps$Peptide
      tumor_peps$Peptide <- spreaded_peps$Peptide
      
      data <- merge(df, tumor_peps, by = "Peptide")
      data <- merge(data, normal_peps, by = "Peptide")
      
      data <- data %>% arrange(desc(Total))
      
      data[data == ""] <- "N.A."
      
      data
    }
    
    cancer_types <- function(){
      cancer_types_query <- "SELECT cancer_id, name FROM CANCER"
      cancer_types <- dbGetQuery(db, cancer_types_query)
      
      cancer_types
    }
    
    tumor_counts <- function(cancertype = input$cancertype2) {
      query <- "SELECT COUNTS.sample_id AS 'id', 
          SUM(case when substitution = 'WF' then count end) WF, 
          SUM(case when substitution = 'WY' then count end) WY, 
          SUM(case when substitution = 'WA' then count end) WA, 
          SUM(case when substitution = 'WG' then count end) WG, 
          SUM(case when substitution = 'WH' then count end) WH, 
          SUM(case when substitution = 'WC' then count end) WC, 
          SUM(case when substitution = 'WD' then count end) WD, 
          SUM(case when substitution = 'WE' then count end) WE, 
          SUM(case when substitution = 'WI' then count end) WI, 
          SUM(case when substitution = 'WL' then count end) WL, 
          SUM(case when substitution = 'WM' then count end) WM, 
          SUM(case when substitution = 'WN' then count end) WN, 
          SUM(case when substitution = 'WP' then count end) WP, 
          SUM(case when substitution = 'WQ' then count end) WQ, 
          SUM(case when substitution= 'WS' then count end) WS, 
          SUM(case when substitution = 'WT' then count end) WT, 
          SUM(case when substitution= 'WV' then count end) WV 
          FROM COUNTS JOIN SAMPLE ON COUNTS.sample_id=SAMPLE.sample_id 
          AND SAMPLE.tumor = TRUE AND SAMPLE.cancer_id = 'cancertype' GROUP BY COUNTS.sample_id;"
      
      tumor_counts_query <- sub("cancertype", cancertype, query)
      tumor_counts_sql <- dbGetQuery(db, tumor_counts_query)
      
      tumor_counts_sql
      
    }
    
    normal_counts <- function(cancertype = input$cancertype2) {
      query <- "SELECT COUNTS.sample_id AS 'id', 
                      SUM(case when substitution = 'WF' then count end) WF, 
                      SUM(case when substitution = 'WY' then count end) WY, 
                      SUM(case when substitution = 'WA' then count end) WA, 
                      SUM(case when substitution = 'WG' then count end) WG, 
                      SUM(case when substitution = 'WH' then count end) WH, 
                      SUM(case when substitution = 'WC' then count end) WC, 
                      SUM(case when substitution = 'WD' then count end) WD, 
                      SUM(case when substitution = 'WE' then count end) WE, 
                      SUM(case when substitution = 'WI' then count end) WI, 
                      SUM(case when substitution = 'WL' then count end) WL, 
                      SUM(case when substitution = 'WM' then count end) WM, 
                      SUM(case when substitution = 'WN' then count end) WN, 
                      SUM(case when substitution = 'WP' then count end) WP, 
                      SUM(case when substitution = 'WQ' then count end) WQ, 
                      SUM(case when substitution = 'WS' then count end) WS, 
                      SUM(case when substitution = 'WT' then count end) WT, 
                      SUM(case when substitution = 'WV' then count end) WV 
                      FROM COUNTS JOIN SAMPLE ON COUNTS.sample_id=SAMPLE.sample_id 
                      AND SAMPLE.normal = TRUE AND SAMPLE.cancer_id = 'cancertype'  GROUP BY COUNTS.sample_id;"
      normal_counts_query <- sub("cancertype", cancertype, query)
      normal_counts_sql <- dbGetQuery(db, normal_counts_query)
      
      normal_counts_sql
    }
    
    
    tumor_data <- function(cancertype = input$cancertype2) {
      tumor_counts_sql <- tumor_counts(cancertype)
      
      query2 <- "SELECT GENE_EXP.sample_id AS 'id', GENES.gene_name, 
                        gene_expression FROM GENE_EXP JOIN SAMPLE ON GENE_EXP.sample_id = SAMPLE.sample_id 
                        JOIN GENES ON GENE_EXP.gene_id=GENES.gene_id 
                        WHERE SAMPLE.cancer_id = 'cancertype' AND SAMPLE.tumor = TRUE;"
      tumor_gene_exp_query <- sub("cancertype", cancertype, query2)
      
      tumor_gene_exp <- dbGetQuery(db, tumor_gene_exp_query)
      spreaded_tumor <- spread(tumor_gene_exp, key = gene_name, value = gene_expression)
      tumor_genes2 <- merge(tumor_counts_sql, spreaded_tumor, by = 'id')

      tumor_genes2
    }
    
    normal_data <- function(cancertype = input$cancertype2) {
      normal_counts_sql <- normal_counts(cancertype)
      
      query2 <- "SELECT GENE_EXP.sample_id AS 'id', GENES.gene_name, 
                          gene_expression FROM GENE_EXP JOIN SAMPLE ON GENE_EXP.sample_id = SAMPLE.sample_id 
                        JOIN GENES ON GENE_EXP.gene_id=GENES.gene_id 
                          WHERE SAMPLE.cancer_id = 'cancertype' AND SAMPLE.normal = TRUE;"
      normal_gene_exp_query <- sub("cancertype", cancertype, query2)
      
      normal_gene_exp <- dbGetQuery(db, normal_gene_exp_query)
      spreaded_normal <- spread(normal_gene_exp, key = gene_name, value = gene_expression)
      normal_genes2 <- merge(normal_counts_sql, spreaded_normal, by = 'id')
      
      normal_genes2
      
    }
    
    tumor_cluster_data <- function(cancertype = input$cancertype2) {
      query <- "SELECT GENES.gene_name, wf_up, wf_down, wy_up, wy_down, 
                num_up, num_down FROM CLUSTER_TUMOR JOIN GENES ON CLUSTER_TUMOR.gene_id=GENES.gene_id 
                WHERE cancer_id = 'cancertype';"
      tumor_cluster_query <- sub("cancertype", cancertype, query)
      
      tumor_cluster <- dbGetQuery(db, tumor_cluster_query)
      colnames (tumor_cluster)=c("id","WF_UP","WF_DOWN", "WY_UP","WY_DOWN","NUM_UP","NUM_DOWN")
      tumor_cluster = tumor_cluster[ tumor_cluster$NUM_UP > 15 & tumor_cluster$NUM_DOWN > 15,]
      
      tumor_cluster
    }
    
    normal_cluster_data <- function(cancertype = input$cancertype2) {
      query <- "SELECT GENES.gene_name, wf_up, wf_down, wy_up, wy_down, 
                  num_up, num_down FROM CLUSTER_NORMAL JOIN GENES ON CLUSTER_NORMAL.gene_id=GENES.gene_id 
                  WHERE cancer_id = 'cancertype';"
      normal_cluster_query <- sub("cancertype", cancertype, query)
      
      normal_cluster <- dbGetQuery(db, normal_cluster_query)
      colnames (normal_cluster)=c("id","WF_UP","WF_DOWN", "WY_UP","WY_DOWN","NUM_UP","NUM_DOWN")
      normal_cluster = normal_cluster[ normal_cluster$NUM_UP > 15 & normal_cluster$NUM_DOWN > 15,]
      
      normal_cluster
    }
    
    
    observeEvent(input$load_data, {
      output$download <- renderUI({
        div(style ="bottom: 25px; position: absolute;",
          div(style="display:inline-block; padding-right: 10px;", actionButton('plotData', 'Plot peptides', style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold; margin-top: 5px;")),
          div(style="display:inline-block", downloadButton("downloadData", "Download", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold; margin-top: 5px;"))
        )
      })
    })

    peptide_plot <- eventReactive(input$plotData, {
      data_peps <- dataset(cancer_type_subs())
      
      
      fig <- plot_ly(data = data_peps, x = ~Total.T, y = ~Total.N, type = 'scatter',
                     text = ~paste("Peptide: ", Peptide, "<br>Gene: ", Gene),
                     marker = list(color = "rgb(40, 52, 60)"))
      
      fig <- fig %>% layout(title = paste("W>F peptides in", cancer_type_subs()),
                            yaxis = list(title = "Total occurences in adj. normal tissue samples"),
                            xaxis = list(title = "Total occurences in tumor tissue samples"))
    })
    
    
    output$pep_plot <- renderPlotly({
      plot <- peptide_plot()
      
      data_peps <- dataset(cancer_type_subs())
      
      point <- input$substitutant_table_rows_selected
      
      marker_x <- data_peps[point, ]$Total.T
      marker_y <- data_peps[point, ]$Total.N
      peptide <- data_peps[point, ]$Peptide
      
      if (length(point)) {
        point <- list(
          x = marker_x,
          y = marker_y,
          text = peptide,
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          align = "right",
          ax = 20,
          ay = -40,
          arrowwidth = 2)
        
        plot %>% layout(annotations = point)
      } else {
        plot %>% toWebGL()
      }
    })
    

    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(cancer_type_subs(), "WFpeps.txt", sep = "_")
      },
      content = function(file) {
        write.table(dataset(cancer_type_subs()), file, row.names = FALSE, quote = FALSE, sep = "\t")
      }
    )

  
    output$substitutant_table<- renderDT({
      data <- dataset(cancer_type_subs())
      info <- colnames(data)

      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(th(colspan = '6', ' '),
            th(colspan = length(suppressWarnings(tumor_counts(cancer_type_subs()))$id), 'Tumor tissue samples'),
            th(colspan = length(suppressWarnings(normal_counts(cancer_type_subs()))$id), 'Adj. Normal tissue samples')
          ),
          
          tr(HTML('<th id = "peps">Peptide</th>'),
            HTML('<th id = "peps">Protein ID</th>'),
            HTML('<th id = "peps">Gene Symbol</th>'),
            HTML('<th id = "peps">Total</th>'),
            HTML('<th id = "peps">&#8721;Tumor</th>'),
            HTML('<th id = "peps">&#8721;Normal</th>'),
            lapply(info[-c(1:6)], function(x){tags$th(id = "peps", x)})
          )
        )
      ))
      
      headjs <- "function(thead) {
                 $(thead).closest('thead').find('th').eq(0).css('background-color', 'white');
                 $(thead).closest('thead').find('th').eq(1).css('background-color', '#FF0000');
                 $(thead).closest('thead').find('th').eq(2).css('background-color', '#008000');
              }"
                    
      
      datatable(data, caption = paste('W>F substitutant peptides found in', cancer_type_subs(), sep = " "),
                rownames = FALSE, container = sketch, extensions = c('FixedColumns'), 
                options = list(
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  pageLength = 25, headerCallback = JS(headjs))
      ) 
    })


    violin_tumor_data <- eventReactive(input$plotButton, {
      if (violinplot_yes()) {
        tumor = tumor_data(cancer_type())
        tumor
      }
      
    })
    
    violin_normal_data <- eventReactive(input$plotButton, {
      if (violinplot_yes()) {
        normal = normal_data(cancer_type())
        normal
      }
    })

    
    output$violin_scatter <- renderUI({
        if (scatter_yes()) {
          if (interactiv() == "No") {
            sidebarLayout(sidebarPanel(id = "sidebar3", selectizeInput(inputId = "gene",
                                                   label = "Select gene:",
                                                   choices = gene_list),
                                       selectInput(inputId = "subs",
                                                   label = "Select substitutants to compare:",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       selectInput(inputId = "subs2", label = "",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       actionButton(inputId = "plot_violin", label = "Plot", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold;
                                    bottom: 25px; position: absolute;")),
                          mainPanel(tags$style(HTML("#violinplot { padding-top: 30px; padding-right: 30px;}")),
                                    plotOutput("violinplot") %>% withSpinner()), position = "left")
          } else {
            sidebarLayout(sidebarPanel(id = "sidebar3", selectizeInput(inputId = "gene",
                                                   label = "Select gene:",
                                                   choices = gene_list),
                                       selectInput(inputId = "subs",
                                                   label = "Select substitutants to compare:",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       selectInput(inputId = "subs2", label = "",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       actionButton(inputId = "plot_violin_interactive", label = "Plot", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold;
                                    bottom: 25px; position: absolute;")),
                          mainPanel(tags$style(HTML("#violinplot_interactive {padding-top: 30px; padding-right: 20px;}")),
                                    plotlyOutput("violinplot_interactive") %>% withSpinner()), position = "left")
          } 
            
        } else {
          if (interactiv() == "No") {
            sidebarLayout(sidebarPanel(id = "sidebar3", selectizeInput(inputId = "gene",
                                                   label = "Select gene:",
                                                   choices = gene_list),
                                       selectInput(inputId = "subs",
                                                   label = "Select substitutants to compare:",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       selectInput(inputId = "subs2", label = "",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       actionButton(inputId = "plot_violin", label = "Plot", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold;
                                    bottom: 25px; position: absolute;")),
                          mainPanel(tags$style(HTML("#violinplot { padding-top: 30px; padding-left: 30px; }")),
                                    plotOutput("violinplot") %>% withSpinner()), position = "right")
          } else {
            sidebarLayout(sidebarPanel(id = "sidebar3", selectizeInput(inputId = "gene",
                                                   label = "Select gene:",
                                                   choices = gene_list),
                                       selectInput(inputId = "subs",
                                                   label = "Select substitutants to compare:",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       selectInput(inputId = "subs2", label = "",
                                                   choices = c("W>F" = "WF", "W>M" = "WM", "W>A" = "WA",
                                                               "W>Q" = "WQ", "W>C" = "WC", "W>E" = "WE", 
                                                               "W>Y" = "WY", "W>D" = "WD", "W>H" = "WH", 
                                                               "W>N" = "WN", "W>S" = "WS", "W>G" = "WG", 
                                                               "W>I" = "WI", "W>P" = "WP", "W>T" = "WT",
                                                               "W>V" = "WV")),
                                       actionButton(inputId = "plot_violin_interactive", label = "Plot", style = "color: #000; background-color: #bee0ec; font-size: 15px; border: 0px; font-weight: bold;
                                    bottom: 25px; position: absolute;")),
                          mainPanel(tags$style(HTML("#violinplot_interactive {padding-top: 30px; padding-left: 20px;}")),
                                    plotlyOutput("violinplot_interactive") %>% withSpinner()), position = "right")
          }
          }
            
        
    })
    
    observeEvent(input$plotButton, {
        if (interactiv() == "No") {
            toggle("barplot", condition = input$barplot_box)
            hide("barplot_interactive")
            toggle("samples", condition = input$barplot_box)
            
            hide("scatterplot_interactive")
            show("scatterplot")
            toggle("scatterbox", condition = input$scatterplot_box)
            
            toggle("violinbox", condition = input$violinplot_box)
        } else if (interactiv() == "Yes") {
            toggle("barplot_interactive", condition = input$barplot_box)
            hide("barplot")
            hide("samples")
            
            show("scatterplot_interactive")
            hide("scatterplot")
            toggle("scatterbox", condition = input$scatterplot_box)
            
            toggle("violinbox", condition = input$violinplot_box)
        }
        
    })
    
    output$barplot_interactive <- renderPlotly ({
        if (barplot_yes()) {
          cancertype <- cancer_type()
          
          tumor_genes <- suppressWarnings(tumor_counts(cancertype))
          normal_genes <- suppressWarnings(normal_counts(cancertype))
          combined = rbind(tumor_genes, normal_genes)
          
          tumor_counts = tumor_genes[, 1:18]
          normal_counts = normal_genes[, 1:18]
          counts = combined[, 1:18]
          
          tumor_counts$WL <- NULL
          normal_counts$WL <- NULL
          counts$WL <- NULL
          
          plot_ly(
            x = colnames(tumor_counts [,-1]),
            y = colSums (tumor_counts [,-1]),
            name = "Tumor samples", 
            marker = list(color = 'red'), hoverinfo = 'text',
            text = ~paste("</br>Substitutant: ", toupper(colnames(tumor_counts [,-1])),
                          "</br># Substitutants in tumor samples: ", colSums (tumor_counts [,-1]),
                          "</br># Total substitutants: ", (colSums (normal_counts [,-1]))+colSums (tumor_counts [,-1])),
            type = "bar") %>% layout(xaxis = list(categoryorder = "total ascending"), title = paste("W-substitutants in", cancertype)) %>% add_trace(y = colSums (normal_counts [,-1]), 
                                                                                                                                                     color = 'green', name = 'Adj. Normal samples',  hoverinfo = 'text',
                                                                                                                                                     text = ~paste('</br>Substitutant: ', toupper(colnames(normal_counts [,-1])), 
                                                                                                                                                                   '</br># Substitutants in adj. normal samples: ', colSums (normal_counts [,-1]), 
                                                                                                                                                                   '</br>Total substitutants: ', (colSums (normal_counts [,-1]))+colSums (tumor_counts [,-1])), 
                                                                                                                                                     marker = list(color = 'green')) %>% layout(yaxis = list(title = '# Substitutant'), barmode = 'stack')
        }
        
        
    })
    

    output$barplot <- renderPlot({
        if (barplot_yes()) {
          cancertype = cancer_type()
          
          tumor_genes <- suppressWarnings(tumor_counts(cancertype))
          normal_genes <- suppressWarnings(normal_counts(cancertype))
          combined = rbind(tumor_genes, normal_genes)
          
          tumor_counts = tumor_genes[, 1:18]
          normal_counts = normal_genes[, 1:18]
          counts = combined[, 1:18]
          
          tumor_counts$WL <- NULL
          normal_counts$WL <- NULL
          counts$WL <- NULL
          
          choice = input$samples
          
          if (choice == "all") {
            barplot (sort (colSums (counts [,-1])), ylab = "#detected W-substitutants", main = paste("W-substitutants in", cancertype))
          }
          else if (choice == "normal") {
            barplot (sort (colSums (normal_counts [,-1])), ylab = "#detected W-substitutants", main = paste("W-substitutants in", cancertype))
          }
          else if (choice == "tumor") {
            barplot (sort (colSums (tumor_counts [,-1])), ylab = "#detected W-substitutants", main = paste("W-substitutants in", cancertype))
          }
        }
        
        
    })
    
    
    output$scatterplot <- renderPlot({
        if (scatter_yes()) {
          cancertype = cancer_type()
          
          genes_tumor = tumor_cluster_data(cancertype)
          genes_normal = normal_cluster_data(cancertype)
          
          gene <- input$gene_scatter
          
          genes <- append(genes_tumor$id, "", 0)
          
          string <- paste(paste("Selected gene is not found in", cancertype), "data. Select another gene.")
          
          validate(
            need(gene %in% genes, string)
          )
          
          x_min_counts_t <- min(na.omit(genes_tumor$WF_UP))
          x_max_counts_t <- max(na.omit(genes_tumor$WF_UP))
          
          y_min_counts_t <- min(na.omit(genes_tumor$WF_DOWN))
          y_max_counts_t <- max(na.omit(genes_tumor$WF_DOWN))
          
          x_min_counts_n <- min(na.omit(genes_normal$WF_UP))
          x_max_counts_n <- max(na.omit(genes_normal$WF_UP))
          
          y_min_counts_n <- min(na.omit(genes_normal$WF_DOWN))
          y_max_counts_n <- max(na.omit(genes_normal$WF_DOWN))
          
          x_min <- min(c(x_min_counts_n, x_min_counts_t))-as.integer(strsplit(as.character(min(c(x_min_counts_n, x_min_counts_t))), "")[[1]][length(strsplit(as.character(min(c(x_min_counts_n, x_min_counts_t))), "")[[1]])])
          x_max <- max(c(x_max_counts_n, x_max_counts_t))
          
          y_min <- min(c(y_min_counts_n, y_min_counts_t))-as.integer(strsplit(as.character(min(c(y_min_counts_n, y_min_counts_t))), "")[[1]][length(strsplit(as.character(min(c(y_min_counts_n, y_min_counts_t))), "")[[1]])])
          y_max <- max(c(y_max_counts_n, y_max_counts_t))
          
          
          tryCatch(
            { heatscatter (genes_tumor$WF_UP, genes_tumor$WF_DOWN, cor=FALSE,add.contour=TRUE,color.contour="red",
                           greyscale=TRUE, xlab ="W>F in UP", ylab ="W>F in DOWN", cex=0, xlim=c(x_min,x_max), ylim=c(y_min,y_max),
                           main = paste(cancertype, ":W>F (Proteomics)"))
              
              heatscatterpoints(genes_normal$WF_UP, genes_normal$WF_DOWN, cor=FALSE,
                                add.contour=TRUE,color.contour="green",greyscale=TRUE, xlab ="W>F in UP", ylab ="W>F in DOWN", cex=0)
   
              }, # Code that might error goes here, between the { }
            error = function(e) {""} # Leave this line as-is.
          )
          
          
         
          if ((grepl("[A-Za-z]", gene)) == TRUE) {
            text(genes_tumor[genes_tumor$id == gene, ]$WF_UP, genes_tumor[genes_tumor$id == gene, ]$WF_DOWN, col = 'black', labels=gene)
          } 
          
          legend("topleft", legend = c("Tumour tissue", "Normal tissue"), fill = c(2, 3))
        }
        
    })
    
    
    
    output$scatterplot_interactive <- renderPlotly({
        if (scatter_yes()) {
          cancertype <- cancer_type()
          
          genes_tumor = tumor_cluster_data(cancertype)
          genes_normal = normal_cluster_data(cancertype)
          
          gene <- input$gene_scatter
          
          genes <- append(genes_tumor$id, "", 0)
          
          string <- paste(paste("Selected gene is not found in", cancertype), "data. Select another gene.")
          
          validate(
            need(gene %in% genes, string)
          )
          
          if (nrow(genes_normal) < 5) {
            m <- ggplot(genes_tumor, aes(x = WF_UP, y = WF_DOWN)) +
              geom_point(aes(color = 'Tumor sample', text = paste("Gene: ", id)), alpha = 0.2) +
              geom_density_2d(aes(color = 'Tumor sample 2D density contours')) + 
              labs(x = "W>F substitutants in up-regulated genes",
                   y = "W>F substitutants in down-regulated genes",
                   title= paste(cancertype, ":W>F (Proteomics)")) + 
              scale_color_manual(name = 'Sample types: ',   guide = 'legend', values = c('Tumor sample' = 'red', 'Tumor sample 2D density contours' = 'black')) 
          } else {
            m <- ggplot(genes_tumor, aes(x = WF_UP, y = WF_DOWN)) +
              geom_point(aes(color = 'Tumor sample', text = paste("Gene: ", id)), alpha = 0.2) +
              geom_density_2d(aes(color = 'Tumor sample 2D density contours')) +
              geom_point(data = genes_normal, aes(color = 'Adj. Normal sample', text = paste("Gene: ", id)), alpha = 0.2) +
              geom_density_2d(data = genes_normal, aes(color = 'Adj. Normal sample 2D density contours'))  +
              labs(x = "W>F substitutants in up-regulated genes",
                   y = "W>F substitutants in down-regulated genes",
                   title= paste(cancertype, ":W>F (Proteomics)")) + 
              scale_color_manual(name = 'Sample types: ',   guide = 'legend', values = c('Adj. Normal sample' = 'green', 'Tumor sample' = 'red', 'Adj. Normal sample 2D density contours' = 'blue',
                                                                                         'Tumor sample 2D density contours' = 'blue')) 
          }
          
          
          p <- ggplotly(m + theme_bw() + theme(legend.title = element_blank(), plot.background = element_blank()), panel.grid.minor = element_blank()) 
          
          
          if ((grepl("[A-Za-z]", gene)) == TRUE &&  gene %in% genes_tumor$id) {
            point <- list(
              x = genes_tumor[genes_tumor$id == gene, ]$WF_UP,
              y = genes_tumor[genes_tumor$id == gene, ]$WF_DOWN,
              text = gene,
              xref = "x",
              yref = "y",
              showarrow = TRUE,
              align = "right",
              ax = 20,
              ay = -40,
              arrowwidth = 2) 
            p <- p %>% layout(annotations = point)    }
          tryCatch(
            { p$x$data[[2]]$line$width <- 0.6
              p$x$data[[4]]$line$width <- 0.6
            }, # Code that might error goes here, between the { }
            error = function(e) {""} # Leave this line as-is.
          )
          p %>% toWebGL()
        }
        
        
    })
    
    output$violinplot <- renderPlot ({
      showPlot()
    })
    
    
    #Calls function 'plotInput' and prints output plot.
    showPlot <- eventReactive(input$plot_violin, {
      cancertype = cancer_type()
      gene <- input$gene
      
      tumor_genes <- suppressWarnings(violin_tumor_data())
      normal_genes <- suppressWarnings(violin_normal_data())
      
      genes <- colnames(tumor_genes)
      
      string <- paste(paste("Selected gene is not found in", cancertype), "data. Select another gene.")
      
      validate(
        need(gene %in% genes, string)
      )
 
      subs1 <- input$subs
      subs2 <- input$subs2
      
      nl1 <- max(normal_genes[normal_genes[ , gene] < 0, ][, subs1], na.rm = TRUE)
      nh1 <- max(normal_genes[normal_genes[ , gene] > 0, ][, subs1], na.rm = TRUE)
      tl1 <- max(tumor_genes[tumor_genes[ , gene] < 0, ][, subs1], na.rm = TRUE)
      th1 <- max(tumor_genes[tumor_genes[ , gene] > 0, ][, subs1], na.rm = TRUE)
      
      nl2 <- max(normal_genes[normal_genes[ , gene] < 0, ][, subs2], na.rm = TRUE)
      nh2 <- max(normal_genes[normal_genes[ , gene] > 0, ][, subs2], na.rm = TRUE)
      tl2 <- max(tumor_genes[tumor_genes[ , gene] < 0, ][, subs2], na.rm = TRUE)
      th2 <- max(tumor_genes[tumor_genes[ , gene] > 0, ][, subs2], na.rm = TRUE)
      
      yaxis_max <- max (nl1, nh1, tl1, th1, nl2, nh2, tl2, th2, na.rm = TRUE)
      
      if (yaxis_max < 50) {
        yaxis_max = 50
      } else {
        yaxis_max <- yaxis_max + 5
      }
      
      func <- function(x){
        if (length(na.omit(x)) == 0){
          x = c(1)
          na.omit(x)
        } else {
          na.omit(x)
        }
      }
      
      par(mfrow=c(1,2))
      
      vioplot(func(normal_genes[normal_genes[ , gene] < 0, ][, subs1]), func(normal_genes[normal_genes[ , gene] > 0, ][, subs1]),
              func(tumor_genes[tumor_genes[ , gene] < 0, ][, subs1]), func(tumor_genes[tumor_genes[ , gene] > 0, ][, subs1]),
              names=c("Low", "High", "Low", "High"), col=c("green", "green", "red", "red"), 
              xlab = toupper(paste(strsplit(subs1, "")[[1]][1], strsplit(subs1, "")[[1]][2], sep = ">")),
              ylab = '# Substitutants', ylim = c(0, yaxis_max))
      
      
      legend("topleft", legend = c("Tumour tissue", "Normal tissue"), fill = c(2, 3))
      title(main = paste(paste(cancertype, gene, sep = ":"), " assosiation"))
      
      vioplot(func(normal_genes[normal_genes[ , gene] < 0, ][, subs2]), func(normal_genes[normal_genes[ , gene] > 0, ][, subs2]),
              func(tumor_genes[tumor_genes[ , gene] < 0, ][, subs2]), func(tumor_genes[tumor_genes[ , gene] > 0, ][, subs2]),
              names=c("Low", "High", "Low", "High"), col=c("green", "green", "red", "red"), 
              xlab = toupper(paste(strsplit(subs2, "")[[1]][1], strsplit(subs2, "")[[1]][2], sep = ">")), ylim = c(0, yaxis_max))
      
    })
    

    output$violinplot_interactive <- renderPlotly({
      showPlot_interactive()
    })
    
    showPlot_interactive <- eventReactive(input$plot_violin_interactive, {
      cancertype = cancer_type()
      gene <- input$gene
      
      tumor_genes <- suppressWarnings(violin_tumor_data())
      normal_genes <- suppressWarnings(violin_normal_data())
      
      genes <- colnames(tumor_genes)
      
      string <- paste(paste("Selected gene is not found in", cancertype), "data. Select another gene.")
      
      validate(
        need(gene %in% genes, string)
      )
      
      subs1 <- input$subs
      subs2 <- input$subs2
      
      normal_sub1_low <- as.data.frame(normal_genes[normal_genes[ , gene] < 0, ][, subs1])
      colnames(normal_sub1_low) <- "# Substitutants in sample"
      normal_sub1_high <- as.data.frame(normal_genes[normal_genes[ , gene] > 0, ][, subs1])
      colnames(normal_sub1_high) <- "# Substitutants in sample"
      
      tumor_sub1_low <- as.data.frame(tumor_genes[tumor_genes[ , gene] < 0, ][, subs1])
      colnames(tumor_sub1_low) <- "# Substitutants in sample"
      tumor_sub1_high <- as.data.frame(tumor_genes[tumor_genes[ , gene] > 0, ][, subs1])
      colnames(tumor_sub1_high) <- "# Substitutants in sample"
      
      
      sub1 <- ggplot(normal_sub1_low, aes(x = "N Low", y = `# Substitutants in sample`)) + 
        geom_violin(alpha = 0.3, aes(color = 'Adj. Normal Tissues'), fill = 'green') +
        geom_boxplot() +
        
        geom_violin(data = normal_sub1_high, aes(x = "N High", y = `# Substitutants in sample`, color = 'Adj. Normal Tissues'), fill = 'green', alpha = 0.3) + 
        geom_boxplot(data = normal_sub1_high, aes(x = "N High", y = `# Substitutants in sample`)) +
        
        geom_violin(data = tumor_sub1_low, aes(x = "T Low", y = `# Substitutants in sample`, color = 'Tumors'), fill = 'red', alpha = 0.5) +
        geom_boxplot(data = tumor_sub1_low, aes(x = "T Low", y = `# Substitutants in sample`)) + 
        
        geom_violin(data = tumor_sub1_high, aes(x = "T High", y = `# Substitutants in sample`, color = 'Tumors'), fill = 'red', alpha = 0.5) +
        geom_boxplot(data = tumor_sub1_high, aes(x = "T High", y = `# Substitutants in sample`)) +
        labs(x = toupper(paste(strsplit(subs1, "")[[1]][1], strsplit(subs1, "")[[1]][2], sep = ">")), y = "# Substitutants") +
        scale_color_manual(name = ' ',   guide = 'legend', values = c('Adj. Normal Tissues' = 'green', 'Tumors' = 'red')) +
        scale_x_discrete(limits = c("N Low", "N High", "T Low", "T High"), labels = c("N Low" = "Low", "N High" = "High", "T Low" = "Low", "T High" = "High"))
      
      p <- plotly_build(sub1 + theme_classic())
      
      for (i in 1:length(p$x$data)) {
        p$x$data[[i]]$marker$opacity <- 0
        p$x$data[[i]]$line$width <- 1
        p$x$data[[i]]$width <- 0.1
      }
      
      
      normal_sub2_low <- as.data.frame(normal_genes[normal_genes[ , gene] < 0, ][, subs2])
      colnames(normal_sub2_low) <- "# Substitutants in sample"
      normal_sub2_high <- as.data.frame(normal_genes[normal_genes[ , gene] > 0, ][, subs2])
      colnames(normal_sub2_high) <- "# Substitutants in sample"
      
      tumor_sub2_low <- as.data.frame(tumor_genes[tumor_genes[ , gene] < 0, ][, subs2])
      colnames(tumor_sub2_low) <- "# Substitutants in sample"
      tumor_sub2_high <- as.data.frame(tumor_genes[tumor_genes[ , gene] > 0, ][, subs2])
      colnames(tumor_sub2_high) <- "# Substitutants in sample"
      
      sub2 <- ggplot(normal_sub2_low, aes(x = "N Low", y = `# Substitutants in sample`)) + 
        geom_violin(alpha = 0.3, aes(color = 'Adj. Normal Tissues'), fill = 'green') +
        geom_boxplot() +
        
        geom_violin(data = normal_sub2_high, aes(x = "N High", y = `# Substitutants in sample`, color = 'Adj. Normal Tissues'), fill = 'green', alpha = 0.3) + 
        geom_boxplot(data = normal_sub2_high, aes(x = "N High", y = `# Substitutants in sample`)) +
        
        geom_violin(data = tumor_sub2_low, aes(x = "T Low", y = `# Substitutants in sample`, color = 'Tumors'), fill = 'red', alpha = 0.5) +
        geom_boxplot(data = tumor_sub2_low, aes(x = "T Low", y = `# Substitutants in sample`)) + 
        
        geom_violin(data = tumor_sub2_high, aes(x = "T High", y = `# Substitutants in sample`, color = 'Tumors'), fill = 'red', alpha = 0.5) +
        geom_boxplot(data = tumor_sub2_high, aes(x = "T High", y = `# Substitutants in sample`)) +
        labs(x = toupper(paste(strsplit(subs2, "")[[1]][1], strsplit(subs2, "")[[1]][2], sep = ">")), y = "# Substitutants") +
        scale_color_manual(name = ' ',   guide = 'legend', values = c('Adj. Normal Tissues' = 'green', 'Tumors' = 'red')) +
        scale_x_discrete(limits = c("N Low", "N High", "T Low", "T High"), labels = c("N Low" = "Low", "N High" = "High", "T Low" = "Low", "T High" = "High"))
      
      p2 <- plotly_build(sub2 + theme_classic())
      
      for (j in 1:length(p2$x$data)) {
        p2$x$data[[j]]$marker$opacity <- 0
        p2$x$data[[j]]$line$width <- 1
        p2$x$data[[j]]$width <- 0.1
      }
      
      mrg <- list(l = 50, r = 50, b = 50, t = 50, pad = 20)
      
      yaxis_max <- c(p$x$layout$yaxis$range[2], p2$x$layout$yaxis$range[2])
      yaxis_min <- c(p$x$layout$yaxis$range[1], p2$x$layout$yaxis$range[1])
      
      plot <- subplot(style(p, showlegend = F), p2, shareY = TRUE, titleX = TRUE) %>% layout(title = paste(paste(cancertype, gene, sep = ":"), " association"), margin = mrg)
      
      x <- (0:(ceiling(max(yaxis_max))/10))*10
      labels <- x
      labels_text <- as.character(x)
      limits <- c(min(yaxis_min), max(yaxis_max))
      
      plot$x$layout$yaxis$range <- limits
      plot$x$layout$yaxis$ticktext <- labels_text
      plot$x$layout$yaxis$tickvals <- labels
      plot$x$layout$yaxis$categoryarray <- labels_text
      
      plot %>% toWebGL()
      
    })
    
    observe({
      updateSelectizeInput(session = session, inputId = "cancertype2", choices = cancer_types()$name)
      updateSelectInput(session = session, inputId = "cancertype", choices = cancer_types()$name)

    })
}

onStop(function() {
  dbDisconnect(db)
})

# Run the application 
shinyApp(ui = ui, server = server)












