test_that("f_data_input", {

  f_path <- system.file("A2SIT_data_input_template_GTM.xlsx", package = "A2SIT")

  # correct spec
  df_geom <- readRDS(system.file("geom", "GTM.rds", package = "A2SIT"))
  coin <- f_data_input(f_path, df_geom = df_geom)
  expect_s3_class(coin, "coin")

  # # cached country, but wrong one
  # coin_error <- f_data_input(f_path, "ARG")
  # expect_null(coin_error)
  #
  # # not cached country
  # expect_error(f_data_input(f_path, "XXX"), "ISO3 %in% valid_ISOs is not TRUE")

  # check that number of indicators and units agrees with manual inspection
  # Currently has 54 indicators, but 13 have no data so should be removed

  n_ind <- 41
  n_unit <- 340

  expect_equal(get_n_indicators(coin), n_ind)
  expect_equal(get_n_units(coin), n_unit)

})


