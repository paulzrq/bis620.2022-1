test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <-  accel_plot(ukb_accel[1:100, ])
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The accel_plot() errors when no time or freq column.",
  {
    data(iris)
    expect_error(accel_plot(iris))
  }
)

test_that(
  "The accel_plot() is correct for time-series data.",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    vdiffr::expect_doppelganger("first-100-samples", p)
    #you can review graphically with testthat::snapshot_review().
  }
)



test_that(
  "The accel_plot() errors when no time or freq column.",
  {
    data(iris)
    expect_error(spectral_signature(iris))
  }
)

test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <-  spectral_signature(ukb_accel[1:100, ]) |> accel_plot()

    expect_true(inherits(p, "gg"))
  }
)
test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <-  spectral_signature(ukb_accel[1:100, ],
                             take_log = TRUE)

    expect_true(inherits(p, "data.frame"))
  }
)
