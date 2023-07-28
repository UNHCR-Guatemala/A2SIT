test_that("analysis table", {

  # check analysis tables
  coin <- GTM_coin

  # run analysis
  coin <- f_analyse_indicators(coin)

  # check presence of tables
  expect_true(all(c("FlaggedStats", "Flags", "Stats") %in% names(coin$Analysis$Raw)))

  # check FlaggedStats table
  df_fs <- coin$Analysis$Raw$FlaggedStats

  # check all iCodes present
  expect_setequal(df_fs$iCode, get_indicator_codes(coin, code_types = "Indicator", F, F))

  # data avail
  avail_Graduand <- df_fs$Frc.Avail[df_fs$iCode == "Graduand"]
  avail_check <- mean(!is.na(coin$Data$Raw$Graduand)) |> signif(3)
  expect_equal(avail_Graduand, avail_check)

  # same
  same_Refugiados <- df_fs$Frc.Same[df_fs$iCode == "Refugiados"]
  same_check <- max(table(coin$Data$Raw$Refugiados))/sum(!is.na(coin$Data$Raw$Refugiados))
  expect_equal(same_Refugiados, same_check)

  # skew/kurt - have to get back to numeric format
  sk_Afrodesc <- df_fs$SkewKurt[df_fs$iCode == "Afrodesc"] |>
    strsplit(" / ") |>
    unlist() |>
    as.numeric()

  expect_equal(sk_Afrodesc[1], signif(COINr::skew(coin$Data$Raw$Afrodesc, na.rm = TRUE),3))
  expect_equal(sk_Afrodesc[2], signif(COINr::kurt(coin$Data$Raw$Afrodesc, na.rm = TRUE),3))

  # check negative correlation
  # from table, Dormitoria is negatively correlated with VivCollectCalle

  cor_check <- stats::cor(coin$Data$Raw$Dormitoria, coin$Data$Raw$VivCollectCalle,
                          use = "pairwise.complete.obs", method = "spearman")
  expect_lt(cor_check, -0.4)

  # all indicators should be in
  expect_true(all(df_fs$Status == "In"))

  # test removing indicator
  coin <- f_remove_indicators(coin, "Afrodesc")
  expect_equal(coin$Analysis$Raw$FlaggedStats$Status[coin$Analysis$Raw$FlaggedStats$iCode == "Afrodesc"], "OUT")
  # test replace indicator
  coin <- f_add_indicators(coin, "Afrodesc")
  expect_equal(coin$Analysis$Raw$FlaggedStats$Status[coin$Analysis$Raw$FlaggedStats$iCode == "Afrodesc"], "In")

  # test flags
  df_f <- coin$Analysis$Raw$Flags
  expect_equal(ncol(df_f), ncol(df_fs))
  expect_equal(nrow(df_f), nrow(df_fs))

  # I'm just going to manually check some things based on visual inspection
  # recall that TRUE means flagged
  expect_false(all(df_f$Frc.Avail))
  expect_true(df_f$Frc.Same[df_f$iCode == "Refugiados"])
  all_negcorr <- !is.na(df_fs$NegCorr)
  expect_true(all(df_f$NegCorr[all_negcorr]))

  # don't test stats table as this is direct COINr output and tested there
  # https://github.com/bluefoxr/COINr/blob/master/tests/testthat/test-stats.R

})
