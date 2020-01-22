texts_pl_df <- read.csv("inst/raw/texts_pl.csv", stringsAsFactors = FALSE)
texts_en_df <- read.csv("inst/raw/texts_en.csv", stringsAsFactors = FALSE)

texts_pl <- setNames( as.list(texts_pl_df[["value"]]), texts_pl_df[["key"]])
texts_en <- setNames( as.list(texts_en_df[["value"]]), texts_en_df[["key"]])

texts = list(en = texts_en, pl = texts_pl)
usethis::use_data(texts, internal = TRUE, overwrite = TRUE)
