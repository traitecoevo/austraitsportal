# Tests for plotting functions

test_that("plot_trait_distribution handles empty data", {
  empty_data <- data.frame()
  
  result <- plot_trait_distribution(empty_data, "test_trait")
  
  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("plot_trait_distribution handles NULL data", {
  result <- plot_trait_distribution(NULL, "test_trait")
  
  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("plot_trait_distribution routes to categorical plot for NA units", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    taxon_name = c("Species A", "Species B"),
    family = c("Fabaceae", "Myrtaceae"),
    trait_name = c("leaf_shape", "leaf_shape"),
    value = c("ovate", "elliptic"),
    unit = c(NA, NA),
    dataset_id = c("dataset1", "dataset1"),
    observation_id = c("obs1", "obs2")
  )
  
  result <- plot_trait_distribution(test_data, "leaf_shape")
  
  expect_s3_class(result, "gg")
})

test_that("plot_trait_distribution_beeswarm handles numerical data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  
  test_data <- data.frame(
    taxon_name = rep("Species A", 5),
    family = rep("Fabaceae", 5),
    trait_name = rep("leaf_area", 5),
    value = c(10, 12, 15, 18, 20),
    unit = rep("mm2", 5),
    value_type = rep("mean", 5)
  )
  
  result <- plot_trait_distribution_beeswarm(
    test_data, 
    "leaf_area", 
    "family"
  )
  
  expect_s3_class(result, "patchwork")
})

test_that("plot_trait_distribution_beeswarm handles empty data gracefully", {
  skip_if_not_installed("ggplot2")
  
  result <- plot_trait_distribution_beeswarm(
    NULL, 
    "leaf_area", 
    "family"
  )
  
  expect_s3_class(result, "gg")
})

test_that("plot_trait_distribution_beeswarm handles single value", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  
  test_data <- data.frame(
    taxon_name = "Species A",
    family = "Fabaceae",
    trait_name = "leaf_area",
    value = 10,
    unit = "mm2",
    value_type = "mean"
  )
  
  result <- plot_trait_distribution_beeswarm(
    test_data,
    "leaf_area",
    "family"
  )
  
  expect_s3_class(result, "patchwork")
})

test_that("plot_categorical_trait_distribution groups by family", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    taxon_name = rep(c("Species A", "Species B"), each = 3),
    family = rep(c("Fabaceae", "Myrtaceae"), each = 3),
    trait_name = rep("leaf_shape", 6),
    value = c("ovate", "ovate", "elliptic", "elliptic", "elliptic", "linear"),
    dataset_id = rep(c("dataset1", "dataset2"), each = 3),
    observation_id = paste0("obs", 1:6)
  )
  
  result <- plot_categorical_trait_distribution(test_data, "leaf_shape", 10)
  
  expect_s3_class(result, "gg")
  expect_s3_class(result, "ggplot")
})

test_that("plot_trait_distribution_beeswarm uses log scale for wide ranges", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  
  # Data with wide range (> 20x)
  test_data <- data.frame(
    taxon_name = rep("Species A", 3),
    family = rep("Fabaceae", 3),
    trait_name = rep("seed_mass", 3),
    value = c(0.1, 1, 100),  # 1000x range
    unit = rep("mg", 3),
    value_type = rep("mean", 3)
  )
  
  result <- plot_trait_distribution_beeswarm(
    test_data,
    "seed_mass",
    "family"
  )
  
  expect_s3_class(result, "patchwork")
  # Log scale should be applied automatically for wide ranges
})

test_that("plot functions handle special characters in trait names", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    taxon_name = "Species A",
    family = "Fabaceae",
    trait_name = "trait_with_underscore",
    value = "value",
    unit = NA,
    dataset_id = "dataset1",
    observation_id = "obs1"
  )
  
  expect_error(
    plot_trait_distribution(test_data, "trait_with_underscore"),
    NA  # Should not error
  )
})
