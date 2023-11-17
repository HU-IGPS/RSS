# majour key words, must contain either elements
key <- c("wheat","crop")
# special pattern for straw WSC
## straw WSC, WSC or WSC in straw
strawwsc <- "(?:straw|non[-\\s]?structural|water[-\\s]?soluble)?(?:carbohydrate|sugar|fructan|carbon[-\\s]?assimilat)(?:in straw|straw)?"
# sprintf("(?:%s\\s)?%s(?:\\sin\\s%s)", "straw","water[-\\s]?soluble carbohydrate", "straw")
optional_patterns <- c(post="post[-\\s]?anthesis",# post-anthesis, post anthesis, postanthesis
                       carbohydrate=strawwsc,
                       # yield="yield(?:[-\\s]?stability)?",# yield-stability, yield stability, yield
                       sink="(sink|source)",
                       breed="breeding(?:[-\\s]?progress)?",
                       green="stay[-\\s]?green",
                       mobile="(mobil|transolcat)")


nam <- expand.grid(names(optional_patterns),key)

# Function to create the positive lookahead patterns
anypo <- function(pat) {
  # match pattern at any position 
  paste0("(?=.*", pat, ")")
}

generate_pattern <- function(key, optional_patterns) {
  
  # Construct the regular expression pattern
  res <- purrr::map(key,~{
    pattern <- paste0(
      # "(?-i)",  # set to not Case-insensitive
      # must have part
      anypo(.x),
      # paste0("(?=.*", .x, ")"),
      # key %>% purrr::map(.,anypo) %>% Reduce("paste0",.),
      # optional part
      anypo(paste0("(?:", optional_patterns, ")+"))
      # "(?=.*", paste0("(?:", optional_patterns, ")+"), ")"
    )
  }) %>% Reduce("c",.)

  return(res)
}


# 
# key <- c("yield", "wheat")
# optional_patterns <- c("post[-\\s]?anthesis", "water[-\\s]?soluble carbohydrate","straw")
# 
# 
# pattern <- generate_pattern(key, optional_patterns)
# print(pattern)
# fdf <- map_dfr(1:3,~{
#   grepl(pattern[.x],sdf$TITLE,ignore.case = T,perl=T) %>% which() %>% 
#     sdf[.,]
# })
#  
# 
