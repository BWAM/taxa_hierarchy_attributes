## ------------------------------------------------------------------------
library(tidyverse)
library(ritis)

## ------------------------------------------------------------------------
root.dir <- rprojroot::find_root("taxa_hierarchy_attributes.Rproj")

## ------------------------------------------------------------------------
org.df <- data.table::fread(file.path(root.dir, "data/master_macro_species.csv"))

