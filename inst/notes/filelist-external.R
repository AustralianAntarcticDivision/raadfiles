listdirs <- c("/rdsi/PRIVATE/raad/data_staging",
              "/rdsi/PRIVATE/raad/data_deprecated",

              "/rdsi/PRIVATE/raad/data_local/aad.gov.au",
              "/rdsi/PRIVATE/raad/data",
              "/rdsi/PUBLIC/raad/data")[-c(5)]

lockfile <- "#rawfilelistlock#"

system.time({
  #l <- vector("list", length(listdirs))

  for (i in seq_along(listdirs)) {
    fff <- gsub(paste0(listdirs[i], "/"), "", system(sprintf("rg --files %s", listdirs[i]), intern = TRUE))
    files <- data.frame(root = character(0), file = character(0))

    if (length(fff) > 0) files <- data.frame(root = rep(listdirs[i], length(fff)), file = fff)
    outfile <- file.path(listdirs[i], "rawfile_list.tab")
    print(outfile)
    print(head(files))
    #write.table(files, outfile, row.names = FALSE, header = TRUE))
  }
  #print(sum(lengths(l)))
})
