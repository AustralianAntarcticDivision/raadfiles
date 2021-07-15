# context("all-files-functions")
#
# files_funs_names <- grep("files$", ls("package:raadfiles"), value = TRUE)
#
# # test_that("all files functions work", {
# #      skip_if_not(!is.null(get_raad_data_roots()))
# #   for (i in seq_along(files_funs_names)) {
# #     get_fun <- get(files_funs_names[i])
# #     d <- try(get_fun())
# #     if (inherits(d, "try-error")) next;
# #     print(files_funs_names[i])
# #     print(names(d))
# #    # d %>% expect_s3_class("tbl_df") %>% expect_named(c("date", "fullname"))
# #   #  expect_gt(nrow(d), 0)
# #   }
# # })
