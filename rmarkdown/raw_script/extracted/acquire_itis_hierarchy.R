## ------------------------------------------------------------------------
tsn.vec <- unique(taxa.df$tsn)
tsn.vec <- tsn.vec[!is.na(tsn.vec)]
tsn.df <- dplyr::tibble(tsn = tsn.vec)

## ---- results = 'hide'---------------------------------------------------
library(parallel)
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl = cl,
              varlist = c("tsn.vec", "taxa.df"),
              envir = environment())

invisible(clusterEvalQ(cl, c(library(tidyverse))))

## ------------------------------------------------------------------------
tsn.i <- 101296
tsn.i <- tsn.vec[2]
class(tsn.i)
hier.df <- parLapply(cl, tsn.vec, function(tsn.i) {
  # print(tsn.i)
  tryCatch({
    
  if (ncol(ritis::usage(tsn.i)) == 0) return(data.frame(org_tsn = as.integer(tsn.i),
                                                stringsAsFactors = FALSE))
  if (!ritis::usage(tsn.i)$taxonUsageRating %in% c("accepted", "valid")) {
    tsn.accepted <- ritis::accepted_names(as.integer(tsn.i))$acceptedTsn[1] %>% 
      as.integer()
  } else {
    tsn.accepted <- as.integer(tsn.i)
  }
    
    taxa.sub <- taxa.df %>% 
      filter(tsn == tsn.i) %>% 
      select(final_id, found, full_hierarchy) %>% 
      rename(org_id = final_id) %>% 
      mutate(final_tsn = tsn.accepted) %>% 
      mutate_if(is.character, list(~tolower(.) %>% stringr::str_replace_all(" ", "_")))
  
  full.df <- ritis::hierarchy_full(tsn.accepted) %>% 
    select(rankname, taxonname, tsn) %>% 
    slice(1:which(tsn == tsn.accepted)) %>% 
    mutate(final_tsn = as.integer(tsn.accepted),
           final_id = slice(., which(tsn == tsn.accepted))$taxonname
           ) %>% 
    mutate_if(is.character, list(~tolower(.) %>% stringr::str_replace_all(" ", "_"))) %>% 
    full_join(taxa.sub, by = "final_tsn")
  
  }, error = function(e) return(dplyr::tibble(error = paste0("TSN '", tsn.i, "'", 
                                      " caused the error: '", e, "'"))))
  
}) %>% 
  bind_rows()
  
on.exit(stopCluster(cl))

## ------------------------------------------------------------------------
hier.df <- hier.df %>% 
  select(-tsn) %>%
  spread(rankname, taxonname) %>% 
  rename_all(tolower)

## ------------------------------------------------------------------------
column.vec <- c("org_tsn", "org_id",
                "final_tsn", "final_id",
                "kingdom", "subkingdom", "infrakingdom", 
                "superdivision", "division", "subdivision", "infradivision",
                "superphylum", "phylum", "subphylum", "infraphylum", 
                "superclass", "class", "subclass", "infraclass", 
                "superorder", "order", "suborder", "infraorder", 
                "superfamily", "family", "subfamily", 
                "tribe", "subtribe", 
                "genus", "subgenus", 
                "species", "subspecies")

column.vec <- column.vec[column.vec %in% names(hier.df)]

## ------------------------------------------------------------------------
hier.df <- hier.df %>% 
  select(column.vec) 

## ------------------------------------------------------------------------
hier.df <- hier.df %>% 
  mutate(species = if_else(is.na(species) & stringr::str_detect(org_id, "_"),
                           org_id,
                           species))

## ------------------------------------------------------------------------
missing.df <- master.df %>% 
  filter(!final_id %in% hier.df$org_id)

## ------------------------------------------------------------------------
hier.neostempellina <- hier.df %>% 
  filter(final_id == "tanytarsini") %>% 
  mutate(org_id = "neostempellina_sp.",
         final_id = "neostempellina",
         final_tsn = as.character(NA),
         genus = "neostempellina")

hier.reissi <- hier.neostempellina %>% 
  mutate(org_id = "neostempellina_reissi",
         final_id = "neostempellina_reissi",
         species = "reissi")

hier.anafroptilum <-  hier.df %>% 
  filter(final_id == "baetidae") %>% 
  mutate(org_id = "anafroptilum",
         final_id = "anafroptilum",
         final_tsn = as.character(NA),
         genus = "anafroptilum")

hier.sphaeromais <-  hier.df %>% 
  filter(final_id == "ceratopogonidae") %>% 
  mutate(org_id = "sphaeromais_sp.",
         final_id = "sphaeromais",
         final_tsn = as.character(NA),
         genus = "sphaeromais")

hier.nymphomyia <-  hier.df %>% 
  filter(final_id == "nymphomyiidae") %>% 
  mutate(org_id = "nymphomyia_sp.",
         final_id = "nymphomyia",
         final_tsn = as.character(NA),
         genus = "nymphomyia")


## ------------------------------------------------------------------------
hier.df <- hier.df %>% 
  filter(!org_id %in% c("neostempellina_sp.",
                        "neostempellina_reissi",
                        "anafroptilum",
                        "sphaeromais_sp.",
                        "nymphomyia_sp."
                        ))
  bind_rows(hier.neostempellina,
            hier.reissi,
            hier.anafroptilum,
            hier.sphaeromais,
            hier.nymphomyia)

## ------------------------------------------------------------------------
data.table::fwrite(hier.df, 
                   file.path(root.dir, 
                             "data",
                             "processed",
                             "hier_df.csv"))

