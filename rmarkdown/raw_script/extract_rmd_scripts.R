# Extract -----------------------------------------------------------------
sections.path <- "rmarkdown/sections"
extracted.path <- c("rmarkdown/raw_script/extracted")

extract_code <- function(rmd.path, extracted.path) {
  r.files.vec <- list.files(rmd.path)
  r.files.vec <- r.files.vec[grepl(".Rmd", r.files.vec)]
  
  purrr::map(r.files.vec, function(file.i) {
    file.name <- gsub(".Rmd", "", file.i)
    extracted.file <- paste0(file.name, ".R")
    knitr::purl(file.path(rmd.path, file.i),
                file.path(extracted.path, extracted.file))
  })
  
}
extract_code(sections.path, extracted.path)

# Run ---------------------------------------------------------------------
extracted.path <- c("rmarkdown/raw_script/extracted")
source.vec <- c(
  "prepare_r.R",
  "preprocessing.R",
  "acquire_itis_tsn.R",
  "acquire_itis_hierarchy.R"
)

purrr::map(source.vec, function(source.i) {
  source(file.path(extracted.path, source.i))
})

