rm(list=ls())

myfeeds <- data.frame(
  feed_title =c(
    "Field Crops Research",                
    "Journal of Experimental Botany"      ,
    "European Journal of Agronomy"        ,
    "Frontiers in Plant Science"          ,
    "Theoretical and Applied Genetics"    ,
    "Trends in Plant Science"             ,
    "Crop Science"                        ,
    "Euphytica"                           ,
    "Plant and Soil"                    ,  
    "Agricultural and Forest Meteorology" ,
    "New Phytologist"                     ,
    "Plant, Cell & Environment"         ,
    "Agricultural Systems"                ,
    "Agronomy for Sustainable Development"
    # "Annual Review of Plant Biology" 
    ),
  feed_url =c("https://rss.sciencedirect.com/publication/science/03784290",
              "https://academic.oup.com/rss/site_5304/3170.xml",
              "https://rss.sciencedirect.com/publication/science/11610301",
              "https://www.frontiersin.org/journals/plant-science/rss",
              "https://link.springer.com/search.rss?facet-content-type=Article&facet-journal-id=122&channel-name=Theoretical%20and%20Applied%20Genetics",
              "https://rss.sciencedirect.com/publication/science/13601385",
              "https://acsess.onlinelibrary.wiley.com/action/showFeed?jc=14350653&type=etoc&feed=rss",
              "https://link.springer.com/search.rss?facet-content-type=Article&facet-journal-id=10681&channel-name=Euphytica",
              "https://link.springer.com/search.rss?facet-journal-id=11104&package=openaccessarticles&search-within=Journal&query=",
              "https://rss.sciencedirect.com/publication/science/01681923",
              "https://onlinelibrary.wiley.com/feed/14698137/most-recent",
              "https://onlinelibrary.wiley.com/feed/13653040/most-recent",
              "https://rss.sciencedirect.com/publication/science/0308521X",
              "https://link.springer.com/search.rss?facet-journal-id=13593&package=openaccessarticles&search-within=Journal&query="
              # "https://www.annualreviews.org/r/arplant_rss"
             
  )
) %>% 
  arrange(feed_title)

wrangle_feed <- function(the_feed_url, the_feed_dataframe = myfeeds) {
  my_feed_data <- tidyRSS::tidyfeed(the_feed_url)
  if(!any(grepl("feed_last_build_date",names(my_feed_data)))){
    my_feed_data <- my_feed_data %>% 
      rename(feed_last_build_date=feed_pub_date)
  }
  
  my_feed_data <- my_feed_data %>% 
    select(feed_last_build_date,item_link,item_title,item_description)
  return(my_feed_data)
}

RSS_ls<- purrr::imap(1:nrow(myfeeds), ~{
  # print(.y)
  # print(myfeeds$feed_title[.y])
  wrangle_feed(myfeeds[.x,"feed_url"])
})

sdf <- RSS_ls %>% Reduce("rbind",.)

# -------------------------------------------------------------------------

source("src/advance_filter.R")
pattern <- generate_pattern(key, optional_patterns)
# print(pattern)

n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)

system.time(
  res <- foreach(
    i  = 1:length(pattern),
    .packages = c("dplyr","purrr")
  ) %dopar% {
    
    source("src/advance_filter.R")
    # or define it here
    c( # search in title 
      grepl(pattern[i],sdf$item_title,ignore.case = T,perl=T) %>% which() 
      ,# search in abstract
      grepl(pattern[i],sdf$item_description,ignore.case = T,perl=T) %>% which()
    )%>%unique() %>% 
      sdf[.,] %>% 
      mutate(key=nam$Var1[i])
    
  }
)

doParallel::stopImplicitCluster()

rdf <- res %>% Reduce("rbind",.) %>% 
  # remove same journal that match more than one key
  distinct() %>% 
  # merge rows with different key words into one
  group_by(across(-key)) %>%
  summarize(key = paste(key, collapse = ",")) %>%
  ungroup()

saveRDS(rdf,paste0(getwd(),"/data/",sprintf("%s.RDS",Sys.Date())),
        compress=T)
