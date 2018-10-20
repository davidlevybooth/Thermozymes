library(shiny)

ui2 <-  shinyUI(navbarPage(theme = "login.css",
  
  
  
  id = "nav_tabs",
  "Thermozymes v0.1",
  tabPanel("Overview"),
  tabPanel("DYPs",
           mainPanel(width='100%', fluidRow(
             h2("Interactive Tree"),
             tags$iframe(seamless = "seamless",
                         src = "https://itol.embl.de/tree/128189119129330991536355286",
                         height = 500, width = '100%')
           ),
           br(),
           h2("Inspect Sequences"),
           uiOutput("dyp_table_inputs"),
           uiOutput("dyp_gene_tables"),
           br(),
           h2("Selected Sequences"),
           tableOutput("dyp_data_out"),
           br(),
           br(),
           h2("Comments"),
           uiOutput("dyp_comments_table"),
           uiOutput("dyp_comments_entry")
           #,
           # h2("Download Selected Sequences"),
           # div(style="display: inline-block", downloadButton("downloadDNA", "Download DNA Sequences")),
           # div(style="display: inline-block", downloadButton("downloadProtein", "Download Protein Sequences"))
           )
           
  ) ,
  tabPanel("Laccases",
           mainPanel(width='100%', fluidRow(
             h2("Interactive Tree"),
             tags$iframe(seamless = "seamless",
                         src = "https://itol.embl.de/tree/14210360204229041537054224",
                         height = 500, width = '100%')
           ),
           br(),
           h2("Inspect Sequences"),
           uiOutput("lac_table_inputs"),
           uiOutput("lac_gene_tables"),
           br(),
           h2("Selected Sequences"),
           tableOutput("lac_data_out"),
           br(),
           br(),
           h2("Comments"),
           uiOutput("lac_comments_table"),
           uiOutput("lac_comments_entry")
           )
  ),
  tabPanel("AA3 Oxidoreductases",
           mainPanel(width='100%', fluidRow(
             h2("Interactive Tree"),
             tags$iframe(seamless = "seamless",
                         src = "https://itol.embl.de/tree/1421036020432081537833180",
                         height = 500, width = '100%')
           ),
           br(),
           h2("Inspect Sequences"),
           uiOutput("AA3_table_inputs"),
           uiOutput("AA3_gene_tables"),
           br(),
           h2("Selected Sequences"),
           tableOutput("AA3_data_out"),
           br(),
           br(),
           h2("Comments"),
           uiOutput("AA3_comments_table"),
           uiOutput("AA3_comments_entry")
           #,
           # h2("Download Selected Sequences"),
           # div(style="display: inline-block", downloadButton("downloadDNA", "Download DNA Sequences")),
           # div(style="display: inline-block", downloadButton("downloadProtein", "Download Protein Sequences"))
           )
           
  )
) #navbarPage
) #shinyUI)

