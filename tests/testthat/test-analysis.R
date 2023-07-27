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



})
