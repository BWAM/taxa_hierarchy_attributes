## ------------------------------------------------------------------------
taxa.vec <- unique(master.df$final_id) %>% 
  str_replace_all("_", " ")

## ---- results = 'hide'---------------------------------------------------
library(parallel)
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl = cl,
              varlist = c("taxa.vec"),
              envir = environment())

invisible(clusterEvalQ(cl, c(library(tidyverse))))

## ------------------------------------------------------------------------
# taxa.i <- taxa.missing[1]
taxa.df <- parLapply(cl, taxa.vec, function(taxa.i) {
  tryCatch({
  temp.df <- ritis::search_scientific(taxa.i) 
  
  if (nrow(temp.df) > 0) {
    temp.df <- temp.df %>% 
      mutate(combinedName = tolower(combinedName),
             final_id = taxa.i,
             found = TRUE,
             full_hierarchy = TRUE) %>% 
      filter(combinedName == taxa.i,
             kingdom == "Animalia") %>% 
      slice(1)
  } else if (nrow(temp.df) == 0 & length(str_split(taxa.i, " ", simplify = TRUE)) > 1) {
    temp.df2 <- str_split(taxa.i, " ", simplify = TRUE)[1] %>% 
      ritis::search_scientific()
    
    if (nrow(temp.df2) > 0) {
    temp.df2 %>% 
      mutate(combinedName = tolower(combinedName),
             final_id = taxa.i,
             found = TRUE,
             full_hierarchy = FALSE) %>% 
      filter(combinedName == str_split(taxa.i, " ", simplify = TRUE)[1],
             kingdom == "Animalia") %>% 
        slice(1)
    } else {
    temp.df <- tibble(combinedName = taxa.i,
           final_id = taxa.i,
           found = FALSE,
           full_hierarchy = FALSE)
  }
  } else {
    temp.df <- tibble(combinedName = taxa.i,
           final_id = taxa.i,
           found = FALSE,
           full_hierarchy = FALSE)
  }
  
  
  
  
}, error = function(e) return(tibble(error = paste0("taxon '", taxa.i, "'", 
                                      " caused the error: '", e, "'"))))
}) %>% 
  bind_rows() %>% 
  select(found, everything()) %>% 
  mutate(final_id = tolower(final_id),
         final_id = str_replace(final_id, " ", "_"))

on.exit(stopCluster(cl))

## ------------------------------------------------------------------------
taxa.sub <- str_replace(taxa.vec, " ", "_")
taxa.missing <- taxa.sub[!taxa.sub %in% taxa.df$final_id]

## ------------------------------------------------------------------------
data.table::fwrite(taxa.df, 
                   file.path(root.dir, 
                             "data",
                             "processed",
                             "tsn_df.csv"))

