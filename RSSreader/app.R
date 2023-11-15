pacman::p_load(tidyRSS,dplyr,DT,purrr,stringr,lubridate,shinydashboard,shiny)
source("src/advance_filter.R")
fnam <- list.files("data",".*raw.RDS")
rawdf <- map_dfr(fnam,~{
  file.path("data",.x) %>% readRDS()
}) %>% distinct()

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
                 )), menuSubItem(
                   selectizeInput(
                     'e2', 'Optional key words', 
                     choices = c("yield","breeding","stay green","yield stability","mobile",
                                 "post anthesis","water soluble carbohydrate","stay green"),
                     select="yield",
                     multiple = TRUE,
                     options = list(create = TRUE)
                   )))),
    img(src='logo.png',width=150)
  ),
  dashboardBody(
    helpText('Enter user-define key words:',size=20),
    # verbatimTextOutput('pattern'),
    textOutput("n"), dataTableOutput("df")
  )
)


# -------------------------------------------------------------------------
# Define server logic required to draw a histogram
server <-function(input, output) {
  key <- reactive({input[["e1"]]})
  op <- reactive({
    map(input[["e2"]],function(inp){
      logi<- map_lgl(optional_patterns,~{
        grepl(.x,inp,perl=T)
      })
      res <- ifelse(any(logi),optional_patterns[logi],inp)
      names(res) <- ifelse(any(logi),names(optional_patterns)[logi],inp)
      return(res)
    }) %>% Reduce("c",.)
  })
  # output$pattern <- renderText(generate_pattern(key(),op()))
  nam <- reactive(expand.grid(names(op()),key()))
  pattern <- reactive({generate_pattern(key(),op())})
  # print(pattern)
  # system.time(
  
  rdf <- reactive({
    purrr::map(1:length(pattern()),function(i){
      c( # search in title
        grepl(pattern()[i],rawdf$item_title,ignore.case = T,perl=T) %>% which()
        ,# search in abstract
        grepl(pattern()[i],rawdf$item_description,ignore.case = T,perl=T) %>% which()
      )%>%unique() %>%
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
      dplyr::filter(!item_title=="Editorial Board") %>% 
      mutate(
        pub_date=as.Date(feed_last_build_date,format="%Y-%m-%d"),
        title=str_glue("{item_title}, <strong> <a target = '_blank' href = '{item_link}'> >></a></strong>")) %>%
      dplyr::select(-c(item_link,item_title,item_description,feed_last_build_date)) %>%
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
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
