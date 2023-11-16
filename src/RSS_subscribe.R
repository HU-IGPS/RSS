rm(list=ls())
pacman::p_load(dplyr,purrr,foreach)
hdir <- "D:/Seafile/RSS"
source(paste0(hdir,"/src/feed_list.R"))

wrangle_feed <- function(the_feed_url, the_feed_dataframe = myfeeds) {
  my_feed_data<- tryCatch({
    tidyRSS::tidyfeed(the_feed_url) %>% suppressMessages()
  },error=function(cond){
    feedeR::feed.extract(the_feed_url) %>% .$items %>% 
      rename(feed_last_build_date=date,item_description=description,
             item_title=title,item_link=link)
  })
  
  if(!any(grepl("feed_last_build_date",names(my_feed_data)))){
    if(!any(grepl("feed_pub_date",names(my_feed_data)))){
      my_feed_data <- my_feed_data %>% 
        mutate(feed_last_build_date=NA)
    }else{
      my_feed_data <- my_feed_data %>% 
        rename(feed_last_build_date=feed_pub_date)
    }
    
  }
  
  if(!any(grepl("item_description",names(my_feed_data)))){
    my_feed_data <- my_feed_data %>% 
      mutate(item_description=NA)
  }
  my_feed_data <- my_feed_data %>% 
    dplyr::select(feed_last_build_date,item_link,item_title,item_description)
  return(my_feed_data)
}

pb = txtProgressBar(min = 0, max = nrow(myfeeds),
                    style = 3,    # Progress bar style (also available style = 1 and style = 2)
                    width = 30,initial = 0)

RSS_ls<- purrr::map(1:nrow(myfeeds), ~{
  # print(.x)
  # print(myfeeds$feed_title[.y])
  res <- wrangle_feed(myfeeds[.x,"feed_url"])%>%
    mutate(journal=myfeeds$feed_title[.x])
  # print(nrow(res))
  setTxtProgressBar(pb,.x)
  return(res)
})

sdf <- RSS_ls %>% Reduce("rbind",.)
saveRDS(sdf,paste0(hdir,"/data/",sprintf("%s_raw.RDS",Sys.Date())),
        compress=T)
# -------------------------------------------------------------------------
# 
# source("src/advance_filter.R")
# pattern <- generate_pattern(key, optional_patterns)
# # print(pattern)
# 
# n.cores <- parallel::detectCores() - 1
# #create the cluster
# my.cluster <- parallel::makeCluster(
#   n.cores,
#   type = "PSOCK"
# )
# doParallel::registerDoParallel(cl = my.cluster)
# 
# system.time(
#   res <- foreach(
#     i  = 1:length(pattern),
#     .packages = c("dplyr","purrr")
#   ) %dopar% {
#     
#     source("src/advance_filter.R")
#     # or define it here
#     c( # search in title 
#       grepl(pattern[i],sdf$item_title,ignore.case = T,perl=T) %>% which() 
#       ,# search in abstract
#       grepl(pattern[i],sdf$item_description,ignore.case = T,perl=T) %>% which()
#     )%>%unique() %>% 
#       sdf[.,] %>% 
#       mutate(key=nam$Var1[i])
#     
#   }
# )
# 
# doParallel::stopImplicitCluster()
# 
# rdf <- res %>% Reduce("rbind",.) %>% 
#   # remove same journal that match more than one key
#   distinct() %>% 
#   # merge rows with different key words into one
#   group_by(across(-key)) %>%
#   summarize(key = paste(key, collapse = ",")) %>%
#   ungroup()

# saveRDS(rdf,paste0(getwd(),"/data/",sprintf("%s.RDS",Sys.Date())),
#         compress=T)
