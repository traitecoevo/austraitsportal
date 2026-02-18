test_that("add_target_blank adds target attribute to links", {
  html_input <- '<a href="https://example.com">Link</a>'
  result <- add_target_blank(html_input)
  
  expect_true(grepl('target="_blank"', result))
  expect_true(grepl('href=', result))
})

test_that("add_target_blank handles multiple links", {
  html_input <- '<p><a href="https://example1.com">Link1</a> and <a href="https://example2.com">Link2</a></p>'
  result <- add_target_blank(html_input)
  
  expect_equal(
    stringr::str_count(result, 'target="_blank"'),
    2
  )
})

test_that("add_target_blank handles HTML without links", {
  html_input <- '<p>No links here</p>'
  result <- add_target_blank(html_input)
  
  expect_equal(result, html_input)
})

test_that("add_target_blank handles empty string", {
  result <- add_target_blank("")
  expect_equal(result, "")
})

test_that("add_target_blank doesn't duplicate target attribute", {
  html_input <- '<a target="_blank" href="https://example.com">Link</a>'
  result <- add_target_blank(html_input)
  
  # Function doesn't duplicate - target already present, so keeps it as is
  expect_equal(
    stringr::str_count(result, 'target="_blank"'),
    1  # Just the one that was already there
  )
})
