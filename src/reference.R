rm(list=ls())
pacman::p_load(dplyr,purrr,foreach)


df <- bib2df::bib2df("data/My Library.bib")

colvec <- c("JOURNAL","YEAR","BIBTEXKEY","TITLE","ABSTRACT")

sdf <- df %>% 
  filter(CATEGORY=="ARTICLE",
         # YEAR>2020,
         !is.na(JOURNAL)) %>% 
  dplyr::select(all_of(colvec)) %>% 
  mutate(AUTHOR=strsplit(BIBTEXKEY,"_") %>% 
           map_chr(.,~{
             .x[1]
           }),
         TITLE=gsub("(\\{|\\})","",TITLE)
  ) %>% 
  select(-BIBTEXKEY) %>% 
  distinct()
# columna name rename
names(sdf) <- tolower(names(sdf))
# pattern match -------------------------------------------------------------------------

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
      grepl(pattern[i],sdf$title,ignore.case = T,perl=T) %>% which() 
      ,# search in abstract
      grepl(pattern[i],sdf$abstract,ignore.case = T,perl=T) %>% which()
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

# -------------------------------------------------------------------------

rdf$key %>% unique()

d <- rdf %>% filter()
  group_by(journal) %>% summarise(count=n()) %>% arrange(desc(count))

