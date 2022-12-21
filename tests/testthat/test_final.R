test_that(
  "The summary_baseline returns a double object.",
  {
    data(ukb_accel)
    a <- summary_baseline(ukb_accel)
    expect_type(a, "double")
  }
)

test_that(
  "The summary_KRAS returns a list object.",
  {
    data(ukb_accel)
    a <- summary_kras(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The test_DTHDY returns a double object.",
  {
    data(ukb_accel)
    a <- test_dthdy(ukb_accel)
    expect_type(a, "double")
  }
)

test_that(
  "The summary_response returns a list object.",
  {
    data(ukb_accel)
    a <- summary_response(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The linear_regresion returns a double object.",
  {
    data(ukb_accel)
    a <- linear_regression(ukb_accel)
    expect_type(a, "double")
  }
)

test_that(
  "The summary_sex returns a list object.",
  {
    data(ukb_accel)
    a <- summary_sex(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The summary returns a list object.",
  {
    data(ukb_accel)
    a <- summary(ukb_accel)
    expect_type(a, "character")
  }
)

test_that(
  "The survivalATRT_age returns a list object.",
  {
    data(ukb_accel)
    a <- survivalatrt_age(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The survivalATRT_sex returns a list object.",
  {
    data(ukb_accel)
    a <- survivalatrt_sex(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The survival_ATRT returns a list object.",
  {
    data(ukb_accel)
    a <- survival_atrt(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The Survival_KRAS returns a list object.",
  {
    data(ukb_accel)
    a <- survival_kras(ukb_accel)
    expect_type(a, "list")
  }
)

test_that(
  "The summary returns a list object.",
  {
    data(ukb_accel)
    a <- summary(ukb_accel)
    expect_type(a, "character")
  }
)

test_that(
  "The survival_ATRT_KRAS returns a list object.",
  {
    data(ukb_accel)
    a <- survival_atrt_kras(ukb_accel)
    expect_type(a, "NULL")
  }
)
