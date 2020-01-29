## ------------------------------------------------------------------------
master.sub <- master.df %>% 
  select(final_id, macro_genspecies,
         tol_int, tol_char, tol_p_int, tol_n_int, ffg, 
         reference, prevnames)

## ------------------------------------------------------------------------
hier.df <- hier.df %>% 
  mutate(org_id = tolower(org_id),
         org_id = str_replace(org_id, " ", "_"))

## ------------------------------------------------------------------------
final.df <- left_join(master.sub, hier.df, by = c("final_id" = "org_id"))

## ------------------------------------------------------------------------
issues.df <- final.df %>% 
  filter(is.na(kingdom))

## ------------------------------------------------------------------------
data.table::fwrite(final.df, 
                   file.path(root.dir, 
                             "data",
                             "processed",
                             "macro_taxa_hier_attributes.csv"))

