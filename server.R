######################################################################################
# Thermozymes App UI
# D. Levy-Booth 2018-09-26
#
# Server
#
#####################################################################################


### Libraries ----
#####################################################################################
library(shiny)
#library(dplyr)
library(seqinr)
library(openssl)

#####################################################################################





### Login information ----
#####################################################################################
source('ui1.R', local = TRUE) #login page
source('ui2.R', local = TRUE)
source("local.R", local = TRUE)
login_table <<- as.data.frame(loadData())

Logged = FALSE; #Change for login
USER <- reactiveValues(Logged = Logged)

#####################################################################################



### Load data -----
#####################################################################################

### Dyp Data -----
dyps <- read.csv2("data/Dyps.table1.txt", sep="\t")
dyps_fasta <- read.fasta(file = "data/Dyp_aln_HOG_HOT.dd.faa", 
                         seqtype = "AA",as.string = TRUE, set.attributes = FALSE)
dyps_dna_fasta <- read.fasta(file = "data/Dyps.fna", 
                             seqtype = "DNA",as.string = TRUE, set.attributes = FALSE)
selected_dyps <- dyps



### Lac Data -----
lacs <- read.csv2("data/lac.table2.txt", sep="\t")
lacs_fasta <- read.fasta(file = "data/Lacs.faa",
                         seqtype = "AA",as.string = TRUE, set.attributes = FALSE)
lacs_dna_fasta <- read.fasta(file = "data/Lacs.fna",
                             seqtype = "DNA",as.string = TRUE, set.attributes = FALSE)
selected_lacs <- lacs



### AA3 Data -----
AA3s <- read.csv2("data/AA3.table.txt", sep="\t")
AA3s_fasta <- read.fasta(file = "data/AA3.all.faa",
                         seqtype = "AA",as.string = TRUE, set.attributes = FALSE)
AA3s_dna_fasta <- read.fasta(file = "data/AA3.all.fna",
                             seqtype = "DNA",as.string = TRUE, set.attributes = FALSE)
selected_AA3s <- AA3s
#####################################################################################


#Server start
#####################################################################################
shinyServer(function(input, output, session) {

  
#####################################################################################
  
  
### Login and authentication ----
#####################################################################################

  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      log_text <- ""
      if (!is.null(input$Login)) {
        if (input$Login > 0 || length(input$enter_key) > 0 && input$enter_key == 13) {
          Email <- isolate(input$email)
          Password <- isolate(sha512(input$passwd))
          Id.username <<- isolate(login_table[which(login_table$email %in% Email),]$user) #Make this more robust
          Id.password <- login_table[which(login_table$password %in% Password),]$user
          if (length(Id.password)==0) {
            log_text <- "Incorrect Password"
          }
          if (length(Id.username)==0) {
            log_text <- "User Email not found"
          }
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username != Id.password) {
              log_text <- "User Email and Password do not match"
            } 
          }
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              saveComments(collection = "visits", data = data.frame(user = Id.username, time = as.character(format(Sys.time(), "%Y-%m-%d %X"))))
              USER$Logged <- TRUE
              log_text <- "Login successful"
            } 
          }
        } 
        output$login_message <- renderUI(p(log_text))
      }
    }    
  })
  
  
  
  
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer", do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <-renderUI({ ui2 })
    }
  })

  


### Table Inputs ----
#####################################################################################
render_table_inputs <- function(tab) {
  renderUI({
    list(
      div(style="display: inline-block", actionButton(paste0(tab, "inspectAll"), label = "Inspect All")),
      div(style="display: inline-block", actionButton(paste0(tab, "selectAll"), label = "Select All")),
      div(style="display: inline-block", actionButton(paste0(tab, "deselectAll"), label = "Deselect All")),
      div(style="display: inline-block", downloadButton(paste0(tab, "downloadDNA"), "Download DNA")),
      div(style="display: inline-block", downloadButton(paste0(tab, "downloadProtein"), "Download Protein")),
      div(style="display: inline-block", actionButton(paste0(tab, "Rec"), "Recommend Selected"))
    )
  })
}

output$dyp_table_inputs <- render_table_inputs("dyp")
output$lac_table_inputs <- render_table_inputs("lac")
output$AA3_table_inputs <- render_table_inputs("AA3")

#####################################################################################  



### Render sequence characteristics ----
#####################################################################################


render_gene_tables <- function(tab) {
  renderUI({
    if(tab == "dyp") {     Data_TAB <- dyps
    Data_faa <- dyps_fasta
    Data_fna <- dyps_dna_fasta
    selected_dyps <- dyps}
    if(tab == "lac") { Data_TAB <- lacs
    Data_faa <- lacs_fasta
    Data_fna <- lacs_dna_fasta
    selected_lacs <- lacs}
    if(tab == "AA3") { Data_TAB <- AA3s
    Data_faa <- AA3s_fasta
    Data_fna <- AA3s_dna_fasta
    selected_lacs <- AA3s}
    
        #Recommend Genes
    dyp_selected_saved <- unique(loadComments(collection = "dyprec"))
    dyp_selected_table <- aggregate(dyp_selected_saved, by=list(dyp_selected_saved$ID), FUN=paste)
    dyp_selected_isolated <- cbind(dyp_selected_table[,1], dyp_selected_table[,grep("rec", colnames(dyp_selected_table)) ])

    gene_output_list <- lapply(as.character(Data_TAB[,1]), function(i) {
      list(
        div(class="gene_btn", style="display: inline-block; width: 70%", actionButton(paste(i, "_in", sep=""), label = i, width='100%')),
        div(style="display: inline-block; width: 10%", checkboxInput(paste(i, "_check", sep=""), "Select", value = FALSE, width = NULL)),
        conditionalPanel(condition = paste("input.", i, "_in%2==1 || input.", tab, "inspectAll%2==1", sep=""),
                         h3("Identity and Enrichment"),
                         div(style="width: 75%", renderTable(Data_TAB[which(Data_TAB$ID==i),2:ncol(Data_TAB)])),
                         h3("Recommendations"),
                         div(style="width: 75%", renderTable(dyp_selected_isolated[which(dyp_selected_isolated[,1] == i),-1 ], colnames=FALSE)),
                         h3("Protein Sequence"),
                         div(style="width: 75%", renderPrint(Data_faa[c(which(names(Data_faa) %in% i))])),
                         h3("DNA Sequence"),
                         div(style="width: 75%", renderPrint(Data_fna[c(which(names(Data_fna) %in% i))]))
        )
      )
    })
    do.call(tagList, unlist(gene_output_list, recursive = FALSE))
  })
}

output$dyp_gene_tables <- render_gene_tables("dyp")
output$lac_gene_tables <- render_gene_tables("lac")
output$AA3_gene_tables <- render_gene_tables("AA3")


#####################################################################################


### Render selected sequences ----
#####################################################################################
render_data_out <- function(tab) {
  renderTable({
    if(tab == "dyp") {     
      Data_TAB <- dyps
      Data_faa <- dyps_fasta
      Data_fna <- dyps_dna_fasta
      selected_data <- dyps}
    if(tab == "lac") { 
      Data_TAB <- lacs
      Data_faa <- lacs_fasta
      Data_fna <- lacs_dna_fasta
      selected_data <- lacs}
    if(tab == "AA3") { 
      Data_TAB <- AA3s
      Data_faa <- AA3s_fasta
      Data_fna <- AA3s_dna_fasta
      selected_data <- AA3s}
    gene_select_list <- lapply(as.character(Data_TAB[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_data <<- Data_TAB[which(selected == TRUE), ]
    Data_TAB[which(selected == TRUE), ]
  })
}  


output$dyp_data_out <- render_data_out("dyp")
output$lac_data_out <- render_data_out("lac")
output$AA3_data_out <- render_data_out("AA3")


#####################################################################################  


### Sequence selection and display -----
#####################################################################################


# Select all sequences ----
observeEvent(
  eventExpr = input$dypselectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(dyps[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = TRUE)
    })
  })

# Select all sequences ----
observeEvent(
  eventExpr = input$lacselectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(lacs[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = TRUE)
    })
  })


# Select all sequences ----
observeEvent(
  eventExpr = input$AA3selectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(AA3s[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = TRUE)
    })
  })



# Deselect all sequences ----
observeEvent(
  eventExpr = input$dypdeselectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(dyps[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = FALSE)
    })
  })

# Deselect all sequences ----
observeEvent(
  eventExpr = input$lacdeselectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(lacs[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = FALSE)
    })
  })

# Deselect all sequences ----
observeEvent(
  eventExpr = input$AA3deselectAll,
  handlerExpr =
  { 
    lapply(paste0(as.character(AA3s[,1]), "_check", sep=""), function(x){
      updateCheckboxInput(session = session,
                          inputId = x,
                          value = FALSE)
    })
  })
#####################################################################################




### Download Handling ----
#####################################################################################

# Downloadable protein fasta of selected dataset ----
output$dypdownloadProtein <- downloadHandler(
  filename = "Selected_Dyps.faa",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(dyps[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_dyps <- dyps[which(selected == TRUE), ]
    selected_dyps_fasta <- dyps_fasta[c(which(names(dyps_fasta) %in% selected_dyps$ID))]
    write.fasta(sequences = selected_dyps_fasta, names = names(selected_dyps_fasta), file.out = file)
  }
)

# Downloadable DNA fasta of selected dataset ----
output$dypdownloadDNA <- downloadHandler(
  filename = "Selected_Dyps.fna",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(dyps[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_dyps <- dyps[which(selected == TRUE), ]
    selected_dyps_dna_fasta <- dyps_dna_fasta[c(which(names(dyps_dna_fasta) %in% selected_dyps$ID))]
    write.fasta(sequences = selected_dyps_dna_fasta, names = names(selected_dyps_dna_fasta), file.out = file)
  }
)

# Downloadable protein fasta of selected dataset ----
output$lacdownloadProtein <- downloadHandler(
  filename = "Selected_Laccases.faa",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(lacs[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_lacs <- lacs[which(selected == TRUE), ]
    selected_lacs_fasta <- lacs_fasta[c(which(names(lacs_fasta) %in% selected_lacs$ID))]
    write.fasta(sequences = selected_lacs_fasta, names = names(selected_lacs_fasta), file.out = file)
  }
)

# Downloadable DNA fasta of selected dataset ----
output$lacdownloadDNA <- downloadHandler(
  filename = "Selected_Laccases.fna",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(lacs[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_lacs <- lacs[which(selected == TRUE), ]
    selected_lacs_dna_fasta <- lacs_dna_fasta[c(which(names(lacs_dna_fasta) %in% selected_lacs$ID))]
    write.fasta(sequences = selected_lacs_dna_fasta, names = names(selected_lacs_dna_fasta), file.out = file)
  }
)



# Downloadable protein fasta of selected dataset ----
output$AA3downloadProtein <- downloadHandler(
  filename = "Selected_AA3s.faa",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(lacs[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_AA3s <- AA3s[which(selected == TRUE), ]
    selected_AA3s_fasta <- AA3s_fasta[c(which(names(AA3s_fasta) %in% selected_AA3s$ID))]
    write.fasta(sequences = selected_AA3s_fasta, names = names(selected_AA3s_fasta), file.out = file)
  }
)

# Downloadable DNA fasta of selected dataset ----
output$AA3downloadDNA <- downloadHandler(
  filename = "Selected_AA3s.fna",
  
  content = function(file) {
    gene_select_list <- lapply(as.character(AA3s[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))
    selected_AA3s <- AA3s[which(selected == TRUE), ]
    selected_AA3s_dna_fasta <- AA3s_dna_fasta[c(which(names(AA3s_dna_fasta) %in% selected_AA3s$ID))]
    write.fasta(sequences = selected_AA3s_dna_fasta, names = names(selected_AA3s_dna_fasta), file.out = file)
  }
)



### Comments ----
#####################################################################################


dyp_comment_render <- function() {
  renderUI({
    if (USER$Logged == TRUE) {
      
      dyp_comments <- loadComments("dypcom")
      
      if(nrow(dyp_comments) >= 1) {
        comment_list <- lapply(as.character(dyp_comments[,4]), function(i) {
          com <- dyp_comments[which(dyp_comments$Comment==i),]
          list(
            div(style="width: 100%", p(tags$b(com$User), paste(" at ", com$Time, ": ", com$Comment)))
          )
        })
        do.call(tagList, unlist(comment_list, recursive = FALSE))
      } else {
        p("No Comments Yet.")
      }
    }
  })
}
output$dyp_comments_table <- dyp_comment_render()
  
output$dyp_comments_entry <- renderUI({ 
  list(textAreaInput("dyp_entry", "", width="600px", height = "100px", resize = "none"),
  div(class="span2 center",
        actionButton("dyp_send", "Send"))
    )})
  
  #Listen for input$send changes (i.e. when the button is clicked)
observeEvent(
  eventExpr = input$dyp_send,
  handlerExpr =
  {
    if (USER$Logged == TRUE) {
      if(input$dyp_send < 1){
        # The code must be initializing, b/c the button hasn't been clicked yet.
        return()
      }
      isolate({
        # Add the current entry to the chat log.
        if(input$dyp_entry != "") {
          comment <- data.frame(User = Id.username, Time = as.character(format(Sys.time(), "%Y-%m-%d %X")), Show = TRUE, Comment = input$dyp_entry)
          saveComments(data = comment, collection = "dypcom")
        }
      })
      # Clear out the text entry field.
      updateTextAreaInput(session, "dyp_entry", value="")
      output$dyp_comments_table <- dyp_comment_render()
      
    } 
  }
)



#### Lac comments
lac_comment_render <- function() {
  renderUI({
    if (USER$Logged == TRUE) {
      
      lac_comments <- loadComments("laccom")
      
      if(nrow(lac_comments) >= 1) {
        comment_list <- lapply(as.character(lac_comments[,4]), function(i) {
          com <- lac_comments[which(lac_comments$Comment==i),]
          list(
            div(style="width: 100%", p(tags$b(com$User), paste(" at ", com$Time, ": ", com$Comment)))
          )
        })
        do.call(tagList, unlist(comment_list, recursive = FALSE))
      } else {
        p("No Comments Yet.")
      }
    }
  })
}
output$lac_comments_table <- lac_comment_render()

output$lac_comments_entry <- renderUI({ 
  list(textAreaInput("lac_entry", "", width="600px", height = "100px", resize = "none"),
       div(class="span2 center",
           actionButton("lac_send", "Send"))
  )})

#Listen for input$send changes (i.e. when the button is clicked)
observeEvent(
  eventExpr = input$lac_send,
  handlerExpr =
  {
    if (USER$Logged == TRUE) {
      if(input$lac_send < 1){
        print(input$lac_send)
        # The code must be initializing, b/c the button hasn't been clicked yet.
        return()
      }
      isolate({
        # Add the current entry to the chat log.
        if(input$lac_entry != "") {
          comment <- data.frame(User = Id.username, Time = as.character(format(Sys.time(), "%Y-%m-%d %X")), Show = TRUE, Comment = input$lac_entry)
          saveComments(data = comment, collection = "laccom")
        }
      })
      # Clear out the text entry field.
      updateTextAreaInput(session, "lac_entry", value="")
      output$lac_comments_table <- lac_comment_render()
      
    } 
  }
)



#### AA3 comments
AA3_comment_render <- function() {
  renderUI({
    if (USER$Logged == TRUE) {
      
      AA3_comments <- loadComments("AA3com")
      
      if(nrow(AA3_comments) >= 1) {
        comment_list <- lapply(as.character(AA3_comments[,4]), function(i) {
          com <- AA3_comments[which(AA3_comments$Comment==i),]
          list(
            div(style="width: 100%", p(tags$b(com$User), paste(" at ", com$Time, ": ", com$Comment)))
          )
        })
        do.call(tagList, unlist(comment_list, recursive = FALSE))
      } else {
        p("No Comments Yet.")
      }
    }
  })
}
output$AA3_comments_table <- AA3_comment_render()

output$AA3_comments_entry <- renderUI({ 
  list(textAreaInput("AA3_entry", "", width="600px", height = "100px", resize = "none"),
       div(class="span2 center",
           actionButton("AA3_send", "Send"))
  )})

#Listen for input$send changes (i.e. when the button is clicked)
observeEvent(
  eventExpr = input$AA3_send,
  handlerExpr =
  {
    if (USER$Logged == TRUE) {
      if(input$AA3_send < 1){
        print(input$AA3_send)
        # The code must be initializing, b/c the button hasn't been clicked yet.
        return()
      }
      isolate({
        # Add the current entry to the chat log.
        if(input$AA3_entry != "") {
          comment <- data.frame(User = Id.username, Time = as.character(format(Sys.time(), "%Y-%m-%d %X")), Show = TRUE, Comment = input$AA3_entry)
          saveComments(data = comment, collection = "AA3com")
        }
      })
      # Clear out the text entry field.
      updateTextAreaInput(session, "AA3_entry", value="")
      output$AA3_comments_table <- AA3_comment_render()
      
    } 
  }
)



#####################################################################################




#Update recommendation engine
#####################################################################################

observeEvent(
  eventExpr = input$dypRec,
  handlerExpr =
  { 

    gene_select_list <- lapply(as.character(dyps[,1]), function(i) {
      list(input[[paste(i, "_check", sep="")]])
    })
    selected <- unlist(do.call(tagList, unlist(gene_select_list, recursive = FALSE)))

    selected_dyps <- dyps[which(selected == TRUE), ]
    
    #genes_selected_df <- data.frame(dyps[,1], selected)
    #genes_selected_df$rec <- list(ifelse(selected, Id.username, ""))
    selected_dyps$rec <- Id.username
    genes_selected_df <- selected_dyps[,c("ID","rec")]
    saveComments(collection = "dyprec", data = genes_selected_df)
    
  })


     #Have recommend selection be binary for each user. If selected -> icon

  
################################################################################
})