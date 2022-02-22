

test_that("effective_sample_sizes converts raw sample sizes correctly", {
  expect_equal(effective_sample_sizes(sample_sizes_raw = 100), 18)
})
