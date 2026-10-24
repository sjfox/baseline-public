# Make every manuscript figure and table listed in manuscript/items.csv, in manuscript order.
# Each script reads the pipeline outputs and writes manuscript/output/<number>.png/.pdf (figures) or
# .csv (tables). Run from the project root: Rscript manuscript/make_all.R
items <- data.table::fread("manuscript/items.csv")
for (i in seq_len(nrow(items))) {
  folder <- if (startsWith(items$item[i], "Table")) "tables" else "figures"
  message("== ", items$item[i], ": ", items$script[i])
  source(file.path("manuscript", folder, paste0(items$script[i], ".R")), local = new.env())
}
message("Manuscript figures and tables written to manuscript/output/")
