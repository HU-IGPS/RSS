pacman::p_load(tidyRSS,dplyr,DT,purrr,stringr,lubridate,shinydashboard,shiny,
               shinyWidgets)
source("src/advance_filter.R")
fnam <- list.files("data",".*raw.RDS")
rawdf <- map_dfr(fnam,~{
  file.path("data",.x) %>% readRDS()
}) %>% distinct()
mn<- function(x){
  lengths(regmatches(x, gregexpr("\\,", x)))
}

# -------------------------------------------------------------------------
# https://stackoverflow.com/questions/61346922/batch-searching-regex-in-shiny-dt-datatables-with-spaces-not-pipes
# -------------------------------------------------------------------------


dataformat <- function(df){
  df %>% 
    dplyr::filter(!item_title=="Editorial Board") %>% 
    mutate(
      pub_date=as.Date(feed_last_build_date,format="%Y-%m-%d"),
      title=str_glue("{item_title}, <strong> <a target = '_blank' href = '{item_link}'> >></a></strong>"),
      keyn= mn(key)) %>%
    arrange(desc(keyn),desc(pub_date)) %>% 
    dplyr::select(c(journal,key,pub_date,title)) %>%
    DT::datatable(., filter = 'top', escape = FALSE, rownames = FALSE,
                  options = list(
                    search = list(regex = TRUE, caseInsensitive = FALSE),
                    pageLength = 10,
                    lengthMenu = c(10, 20, 30, 40),
                    autowidth = TRUE,
                    columnDefs = list(
                      list(width = '80%', targets = list(3)))
                  )
    )
}

# -------------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <-   dashboardPage(
  dashboardHeader(title="RSS Feed"),
  dashboardSidebar(
    
    sidebarMenu(
      
      
      menuItem('Keywords',
               icon = icon("dashboard"),
               startExpanded = TRUE,
               
               menuSubItem(
                 # column(,wellPanel(
                 selectizeInput(
                   'e1', 'Must have key words', choices = c("wheat","yield"),select="wheat",
                   
                   multiple = TRUE,
                   options = list(create = TRUE)
                 )),
               
               menuSubItem(
                 selectizeInput(
                   'e2', 'Optional key words', 
                   choices = c("yield","breeding","stay green","yield stability","mobile",
                               "post anthesis","water soluble carbohydrate","stay green"),
                   select="yield",
                   multiple = TRUE,
                   options = list(create = TRUE
                                  # placeholder ="yield"
                                  )
                 )))),
    materialSwitch(inputId = "id", 
                   label = "Search only in title?",
                   status = "success"
    ),
    materialSwitch(inputId = "cid", 
                   label = "Ignore case?",
                   status = "danger"
    ),
    img(src='logo.png',width=150)
  ),
  dashboardBody(
    helpText('Enter user-define key words:',size=20),
    textOutput("n"), dataTableOutput("df")
  )
)


# -------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <-function(input, output) {
  key <- reactive({input[["e1"]]})
  title.only <- reactive({input$id})
  caseignore <- reactive({input$cid})
  
  op <- reactive({
    map(input[["e2"]],function(inp){
      logi<- map_lgl(optional_patterns,~{
        grepl(.x,inp,ignore.case=caseignore(),perl=T)
      })
      res <- ifelse(any(logi),optional_patterns[logi],inp)
      names(res) <- ifelse(any(logi),names(optional_patterns)[logi],inp)
      return(res)
    }) %>% Reduce("c",.)
    
  })
  
  nam <- reactive(expand.grid(names(op()),key()))
  pattern <- reactive({generate_pattern(key(),op())})

  
  rdf <- reactive({
    purrr::map(1:length(pattern()),function(i){
      # search in title
      tid <- grepl(pattern()[i],rawdf$item_title,ignore.case = caseignore(),perl=T) %>% which()
      # search in abstract
      aid <- grepl(pattern()[i],rawdf$item_description,ignore.case = caseignore(),perl=T) %>% which()
      if(title.only()){
        mid <-tid
      }else{
        mid <-c(tid,aid) %>%  unique()
      }
      mid%>%
        rawdf[.,] %>%
        mutate(key=nam()$Var1[i])
    })%>% 
      Reduce("rbind",.) %>%
      # remove same journal that match more than one key
      distinct() %>%
      # merge rows with different key words into one
      group_by(across(-key)) %>%
      summarize(key = paste(key, collapse = ","),.groups = "drop") %>%
      ungroup()
  })
  
  output$df <- renderDataTable(
    rdf() %>%
      dataformat()
  )
  
  observeEvent(input$id | input$cid, {
    # Update op when either title.only or caseignore changes
    op()
    nam()
    pattern()
    rdf()
    output$df <- renderDataTable(
      rdf() %>%
        dataformat()
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
