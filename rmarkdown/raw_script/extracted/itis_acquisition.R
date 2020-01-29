## ------------------------------------------------------------------------
library(tidyverse)
library(ritis)

## ------------------------------------------------------------------------
root.dir <- rprojroot::find_root("taxa_hierarchy_attributes.Rproj")

## ------------------------------------------------------------------------
org.df <- data.table::fread(file.path(root.dir, "data/master_macro_species.csv"))

## ------------------------------------------------------------------------
master.df <- org.df %>% 
  rename_all(tolower) %>% 
  mutate_if(is.character,
            funs(tolower(.) %>% trimws)) %>% 
  mutate_if(is.character,
            funs(str_replace_all(., " ", "_"))) %>% 
  rename(
    tol_int = hbi_tolerance,
    tol_p_int = "nbi-p_tolerance",
    tol_n_int = "nbi-n_tolerance",
    tol_char = tolname,
    ffg = feedinghab
  ) %>% 
  mutate(
    ffg = case_when(
      ffg == "c-f" ~ "filterer",
      ffg == "c-g" ~ "gatherer",
      ffg == "prd" ~ "predator",
      ffg == "scr" ~ "scraper",
      ffg == "shr" ~ "shredder",
      TRUE ~ as.character(NA)
    ),
    tol_char = case_when(
      tol_char  == "intolerat" ~ "intolerant",
      tol_char  == "prd" ~ as.character(NA),
      TRUE ~ tol_char 
    )
  ) 


## ------------------------------------------------------------------------
replace.vec <- c("_1",
                 "_2",
                 "_3",
                 "_4",
                 "_5",
                 "\\?",
                 "_sp\\.",
                 "_spp\\.",
                 "_gr\\.",
                 "_nr\\.",
                 "_cf\\.",
                 "_\\(ps.\\)",
                 '_""ozarks""', 
                 '_""santa_fe""',
                 "_\\(iron\\)",
                 "_\\(nanocladius\\)",
                 "_\\(plecopt.\\)",
                 "_\\(plecopteracoluthus\\)",
                 "_\\(nixe\\)",
                 "_\\(eudactylocladius\\)",
                 "_\\(euorthoclad.\\)",
                 "_\\(symposiocladius\\)",
                 "_\\(tripodura\\)",
                 "_\\(holotanypus\\)",
                 "_\\(allopsectrocladius\\)",
                 "_\\(monosectrocladius\\)",
                 "undetermined_",
                 "undet."
                 )

## ------------------------------------------------------------------------
master.df <- master.df %>% 
  mutate(
    final_id = str_replace_all(macro_genspecies, paste(replace.vec, collapse = "|"), ""),
    final_id = str_replace_all(final_id, "einfeldia_a", "einfeldia"),
    final_id = str_replace_all(final_id, "natarsia_a", "natarsia"),
    final_id = str_replace_all(final_id, "orthocladiinae_c", "orthocladiinae"),
    final_id = str_replace_all(final_id, "micropsectra_b", "micropsectra"),
    final_id = str_replace_all(final_id, "ceratopsyche_bronta/morosa", "ceratopsyche"),
    final_id = str_replace_all(final_id, "cricotopus/orthocladius_complex", "orthocladiinae"),
    final_id = str_replace_all(final_id, "micropsectra/tanytarsus_complex", "tanytarsini"),
    final_id = str_replace_all(final_id,
                               "tribelos/endochironomus/phaenopsectra_co",
                               "chironomini"),
    final_id = str_replace_all(final_id, "tubificidae_w/_cap._setae", "tubificidae"),
    final_id = str_replace_all(final_id, "tubificidae_w/o_cap._setae", "tubificidae"),
    final_id = str_replace_all(final_id, "aeschnidae", "aeshnidae"),
    final_id = str_replace_all(final_id, "amnicola_grana", "amnicola_granum"),
    final_id = str_replace_all(final_id, "amnicola_limosa", "amnicola_limosus"),
    final_id = str_replace_all(final_id, "anthopotamus_verticus", "anthopotamus_verticis"),
    final_id = str_replace_all(final_id, "aulodrilus_piqueti", "aulodrilus_pigueti"),
    final_id = str_replace_all(final_id, "caecidotea_intermedius", "caecidotea_intermedia"),
    final_id = str_replace_all(final_id, "claasenia", "claassenia"),
    final_id = str_replace_all(final_id, "georthocladius_fimbriatus", "georthocladius_fimbriosus"),
    final_id = str_replace_all(final_id, "helisoma_campanulata", "helisoma_campanulatum"),
    final_id = str_replace_all(final_id, "mystacides_alafimbriata", "mystacides_alafimbriatus"),
    final_id = str_replace_all(final_id, "natarsia_baltimorea", "natarsia_baltimoreus"),
    final_id = str_replace_all(final_id, "orthocladius_oliverei", "orthocladius_oliveri"),
    final_id = str_replace_all(final_id, "parachironomus_potamogeton", "parachironomus_potamogeti"),
    final_id = str_replace_all(final_id,
                              "paralauterborniella_nigrohalteralis",
                              "paralauterborniella_nigrohalterale"),
    final_id = str_replace_all(final_id, "paratendipes_subequalis", "paratendipes_subaequalis"),
    final_id = str_replace_all(final_id, "phaenopsectra_obdiens", "phaenopsectra_obediens"),
    final_id = str_replace_all(final_id, "stenelmis_vittapennis", "stenelmis_vittipennis"),
    final_id = str_replace_all(final_id, "stylogomphus_albystilus", "stylogomphus_albistylus"),
    final_id = str_replace_all(final_id, "tribelos_jucundum", "tribelos_jucundus"),
    final_id = str_replace_all(final_id, "tyrellia", "tyrrellia"),
    final_id = str_replace_all(final_id, "crustacean", "crustacea"),
    final_id = str_replace_all(final_id, "dreisseniidae", "dreissenidae"),
    final_id = str_replace_all(final_id, "nannatacidae", "naticidae"),
    final_id = str_replace_all(final_id, "pteronarcidae", "pteronarcyidae"),
    final_id = str_replace_all(final_id, "unniellal", "unniella")
  ) %>% 
  select(final_id, everything())

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
taxa.df <- parLapply(cl, taxa.vec, function(taxa.i) {
  
  temp.df <- ritis::search_scientific(taxa.i) 
  
  if (nrow(temp.df) > 0) {
    temp.df %>% 
      mutate(combinedName = tolower(combinedName),
             final_id = taxa.i,
             found = TRUE,
             full_hierarchy = TRUE) %>% 
      filter(combinedName == taxa.i)
  } else if (nrow(temp.df) == 0 && length(str_split(taxa.i, " ", simplify = TRUE)) > 1) {
    temp.df <- str_split(taxa.i, " ", simplify = TRUE)[1] %>% 
      ritis::search_scientific()
  }
  
  if (nrow(temp.df) > 0) {
    temp.df %>% 
      mutate(combinedName = str_split(taxa.i, " ", simplify = TRUE)[1],
             final_id = taxa.i,
             found = TRUE,
             full_hierarchy = FALSE) %>% 
      filter(combinedName == taxa.i)
  } else {
    tibble(combinedName = taxa.i,
           final_id = taxa.i,
           found = FALSE,
           full_hierarchy = FALSE
    )
  }
}) %>% 
  bind_rows() %>% 
  select(found, everything())

on.exit(stopCluster(cl))

## ------------------------------------------------------------------------
not.found.vec <- taxa.df %>% 
  filter(found == FALSE) %>% 
  select(combinedName)

write.csv(not.found.vec, "delete.csv")

not.found.vec

## ------------------------------------------------------------------------
taxa.df2 <- taxa.df %>% 
  group_by(tsn) %>% 
  mutate(count = n())

## ------------------------------------------------------------------------
tsn.vec <- unique(taxa.df$tsn)
tsn.vec <- tsn.vec[!is.na(tsn.vec)]
tsn.df <- tibble(tsn = tsn.vec)

## ---- results = 'hide'---------------------------------------------------
library(parallel)
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl = cl,
              varlist = c("tsn.vec"),
              envir = environment())

invisible(clusterEvalQ(cl, c(library(tidyverse))))

## ------------------------------------------------------------------------
tsn.i <- "568571"
hier.df <- parLapply(cl, tsn.vec, function(tsn.i) {
  # print(tsn.i)
  tryCatch({
    
  
  if (ncol(ritis::usage(tsn.i)) == 0) return(data.frame(org_tsn = as.integer(tsn.i),
                                                stringsAsFactors = FALSE))
  if (!ritis::usage(tsn.i)$taxonUsageRating %in% c("accepted", "valid")) {
    tsn.accepted <- ritis::accepted_names(as.integer(tsn.i))$acceptedTsn[1]
  } else {
    tsn.accepted <- as.integer(tsn.i)
  }
  
  full.df <- ritis::hierarchy_full(tsn.accepted) %>% 
    select(rankname, taxonname, tsn) %>% 
    slice(1:which(tsn == tsn.accepted)) %>% 
    mutate(org_tsn = as.integer(tsn.i),
           org_id = ritis::scientific_name(tsn.i)$combinedname,
           final_tsn = as.integer(tsn.accepted),
           final_id = slice(., which(tsn == tsn.accepted))$taxonname)
  
  }, error = function(e) return(tibble(error = paste0("TSN '", tsn.i, "'", 
                                      " caused the error: '", e, "'"))))
  
}) %>% 
  bind_rows() %>% 
  select(-tsn) %>% 
  spread(rankname, taxonname) %>% 
  rename_all(tolower)

on.exit(stopCluster(cl))

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
# hier.df <- hier.df[, column.vec]
hier.df <- hier.df %>% 
  select(column.vec) 

## ------------------------------------------------------------------------
taxa.sub <- taxa.df %>% 
  select(tsn, combinedName, found, full_hierarchy) %>% 
  mutate(org_tsn = as.integer(tsn))

dev.df <- full_join(hier.df, taxa.sub)

test <- dev.df %>% 
  filter(tolower(org_id) != combinedName) %>% 
  

## ------------------------------------------------------------------------
dir.create(file.path(project.dir, "data/itis"),
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(hier.wide,
                   file.path(rprojroot::find_rstudio_root_file(),
                             "data/itis",
                             "macro_itis_hierarchy.csv"))

