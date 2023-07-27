test_that("results correct", {

  # we can assume that the core COINr functions are correct since they are already
  # unit tested: https://github.com/bluefoxr/COINr/tree/master/tests/testthat

  # load example coin, then rebuild
  coin <- GTM_coin
  coin <- f_build_index(coin, agg_method = "a_amean", only_aggregate = FALSE)

  # the steps are 1. treat, 2. normalise, 3. aggregate

  # TREATMENT
  dft <- COINr::get_dset(coin, "Treated", also_get = "none")
  dfr <- COINr::get_dset(coin, "Raw", also_get = "none")

  # manually treat data frame with set paras
  dft2 <- COINr::qTreat(dfr,
                        winmax = 5,
                        skew_thresh = 2,
                        kurt_thresh = 3.5,
                        f2 = "log_CT_plus")$x_treat
  # check same
  expect_equal(dft, dft2, ignore_attr = TRUE)

  # NORMALISATION
  dfn <- COINr::get_dset(coin, "Normalised", also_get = "none")
  # manually normalise
  dfn2 <- COINr::Normalise(
    dft, directions = coin$Meta$Ind[c("Direction", "iCode")],
    global_specs = list(f_n = "n_minmax",
                        f_n_para = list(l_u = c(1, 100)))
  )
  expect_equal(dfn, dfn2, ignore_attr = TRUE)

  # aggregation
  dfa <- COINr::get_dset(coin, "Aggregated")
  # some basic checks - take one row
  x <- dfa[1,]
  # arithmetic mean calculation
  expect_equal(x$MVI, mean(c(x$Amenazas, x$Cap_Resp, x$Movilidad, x$Vulnerabilidad)))

  # reaggregate with geometric
  coin <- f_build_index(coin, agg_method = "a_gmean", only_aggregate = TRUE)
  dfa <- COINr::get_dset(coin, "Aggregated")
  x <- dfa[1,]
  expect_equal(x$MVI, COINr::a_gmean(c(x$Amenazas, x$Cap_Resp, x$Movilidad, x$Vulnerabilidad)))

  # reaggregate with harmonic
  coin <- f_build_index(coin, agg_method = "a_hmean", only_aggregate = TRUE)
  dfa <- COINr::get_dset(coin, "Aggregated")
  x <- dfa[1,]
  expect_equal(x$MVI, COINr::a_hmean(c(x$Amenazas, x$Cap_Resp, x$Movilidad, x$Vulnerabilidad)))

  # change weights - check
  coin$Log$new_coin$iMeta$Weight[coin$Log$new_coin$iMeta$iCode == "Amenazas"] <- 0.5
  coin <- COINr::Regen(coin)
  dfa <- COINr::get_dset(coin, "Aggregated")
  x <- dfa[1,]
  expect_equal(x$MVI, COINr::a_hmean(c(x$Amenazas, x$Cap_Resp, x$Movilidad, x$Vulnerabilidad), w = c(0.5,1,1,1)))

  # SEVERITY categories
  # go back to default coin
  coin <- f_build_index(coin, agg_method = "a_amean", only_aggregate = TRUE)

  expect_true("Severity" %in% names(coin$Data))
  expect_true("Severity" %in% names(coin$Results))
  sev <- coin$Data$Severity

  # note severity is only applied at the aggregate level
  # check index level
  sev_index <- sev$MVI

  # recalculate - get score and then transform
  MVI <- coin$Data$Aggregated$MVI
  sev_index2 <- cut(MVI, 5, labels = FALSE)
  expect_equal(sev_index, sev_index2)
  # check max score is 5
  expect_equal(sev_index[MVI == max(MVI, na.rm = TRUE)], 5)
  # min score is 1
  expect_equal(sev_index[MVI == min(MVI, na.rm = TRUE)], 1)

})



# also check
# - add/remove indicators
# - results table is correctly reloaded
# - make sure weights are correctly accounted for when changed in iMeta
# - make sure scenarios correctly regenerated when indicators added/dropped, and with change of aggregation method
