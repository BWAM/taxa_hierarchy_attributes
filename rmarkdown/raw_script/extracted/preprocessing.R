## ------------------------------------------------------------------------
master.df <- org.df %>% 
  rename_all(tolower) %>% 
  mutate_if(is.character,
            list(~tolower(.) %>% trimws)) %>% 
  mutate_if(is.character,
            list(~str_replace_all(., " ", "_"))) %>% 
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
                 "undet._"
                 )

## ------------------------------------------------------------------------
master.df <- master.df %>% 
  mutate(
    final_id = str_replace_all(macro_genspecies, paste(replace.vec, collapse = "|"), ""),
    final_id = str_replace_all(final_id, "einfeldia_a", "einfeldia"),
    final_id = str_replace_all(final_id, "natarsia_a", "natarsia"),
    final_id = str_replace_all(final_id, "orthocladiinae_c", "orthocladiinae"),
    final_id = str_replace_all(final_id, "micropsectra_b", "micropsectra"),
    final_id = str_replace_all(final_id, "tanytarsus_o", "tanytarsus"),
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
    final_id = str_replace_all(final_id, "stempellina_johannseni", "stempellina_johannsenii"),
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
master.df  <- master.df %>%
  bind_rows(tibble(
    final_id = c(
        "chauliodes_pectinicornis",
        "amphipoda",
        "neocloeon",
        "nymphomyiidae",
        "nigronia")
  ))

