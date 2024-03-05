# Triad tests
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
   expect_equal(calculate_triad_zone(top = 31, left = 60, right = 9), "L")
  })
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 60, left = 31, right = 9), "T")
})
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 31, left = 8, right = 61), "R")
})
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 55, left = 30, right = 15), "LT")
})
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 55, left = 5, right = 40), "TR")
})
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 5, left = 40, right = 55), "LR")
})
test_that("calculate_triad_zone() calculates a triad zone from top, left and right percentage values", {
  expect_equal(calculate_triad_zone(top = 33, left = 33, right = 34), "Centre")
})
test_that("calculate_triad_zone() sum of top, left and right should be 100", {
  expect_error(calculate_triad_zone(top = 43, left = 33, right = 34))
})
test_that("calculate_triad_zone() no top, left or right value should be lver 100", {
  expect_error(calculate_triad_zone(top = 0, left = 101, right = 0))
})
test_that("calculate_triad_zone() no, top, left or right  value should be under 0", {
  expect_error(calculate_triad_zone(top = 51, left = 51, right = -1))
})
test_that("calculate_triad_zone() no, top, left or right value should be character", {
  expect_error(calculate_triad_zone(top = 50, left = 50, right = "john smith"))
})
test_that("calculate_triad_zone() no, top, left or right value should be character", {
  expect_error(calculate_triad_zone(top = 50, left = "john smith", right = 50))
})
test_that("calculate_triad_zone() no, top, left or right value should be character", {
  expect_error(calculate_triad_zone(top = "john smith", left = 50, right = 50))
})


# Dyad tests
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left and right percentage values", {
  expect_equal(calculate_dyad_zone(left = 81, right = 19), "Right")
})
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left and right percentage values", {
  expect_equal(calculate_dyad_zone(left = 61, right = 39), "Centre-Right")
})
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left and right percentage values", {
  expect_equal(calculate_dyad_zone(left = 41, right = 59), "Centre")
})
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left and right percentage values", {
  expect_equal(calculate_dyad_zone(left = 21, right = 79), "Centre-Left")
})
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left and right percentage values", {
  expect_equal(calculate_dyad_zone(left = 0, right = 100), "Left")
})
test_that("calculate_dyad_zone() calculate_dyad_zone() calculates a zone zone from left only percentage values", {
  expect_equal(calculate_dyad_zone(left = 0), "Left")
})
test_that("sum of dyad left and right should be 100", {
  expect_error(calculate_dyad_zone(left = 51, right = 50))
})
test_that("calculate_dyad_zone() no, left or right value should be character", {
  expect_error(calculate_dyad_zone(left = "john smith", right = 100))
})
test_that("calculate_dyad_zone() no, left or right value should be character", {
  expect_error(calculate_dyad_zone(left = 100, right = "john smith"))
})
test_that("calculate_dyad_zone() no, left or right should be less than 0", {
  expect_error(calculate_dyad_zone(left = 100, right = -1))
})
test_that("calculate_dyad_zone() no, left or right should be less than 0", {
  expect_error(calculate_dyad_zone(left = -1, right = 100))
})
test_that("calculate_stone_x_y_zone()  x should be double", {
  expect_error(calculate_stone_x_y_zone(x = "fred"))
})
test_that("calculate_stone_x_y_zone() x should be >= 0", {
  expect_error(calculate_stone_x_y_zone(x = -.01))

})
test_that("calculate_stone_x_y_zone() x  should be <= 1", {
  expect_error(calculate_stone_x_y_zone(x = 1.01))
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.81), "Right")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.80), "Centre-Right")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.61), "Centre-Right")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.60), "Centre")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.41), "Centre")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.40), "Centre-Left")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.21), "Centre-Left")
})
test_that("calculate_stone_x_y_zone() Calculates Zone with x values for stone XLeft OR YTop", {
  expect_equal(calculate_stone_x_y_zone(x = 0.20), "Left")
})
test_that("calculate_stone_4_zone()  x and y should be double", {
  expect_error(calculate_stone_4_zone(x = "fred", y = 0.45))
})
test_that("calculate_stone_4_zone()  x and y should be double", {
  expect_error(calculate_stone_4_zone(x = 0.45, y = "fred"))
})
test_that("calculate_stone_4_zone()  x and y should be double", {
  expect_error(calculate_stone_4_zone(x = "help", y = "fred"))
})
test_that("calculate_stone_4_zone()  x should be >= 0", {
  expect_error(calculate_stone_4_zone(x = -.01, y = 0.35))
})
test_that("calculate_stone_4_zone()  x should be <= 1", {
  expect_error(calculate_stone_4_zone(x = 1.01, y = 0.35))
})
test_that("calculate_stone_4_zone()  y should be >= 0", {
  expect_error(calculate_stone_4_zone(x = 0.35, y = -.01))
})
test_that("calculate_stone_4_zone()  y should be <= 1", {
  expect_error(calculate_stone_4_zone(x = 0.35, y = 1.01))
})
test_that("calculate_stone_4_zone() x and y should be >= 0", {
  expect_error(calculate_stone_4_zone(x = -.01, y = -.01))
})
test_that("calculate_stone_4_zone() x and y should be <= 1", {
  expect_error(calculate_stone_4_zone(x = 1.01, y = 1.01))
})
test_that("calculate_stone_4_zone() x and y should be between 0 and 1", {
  expect_error(calculate_stone_4_zone(x = 1.01, y = -.01))
})
test_that("calculate_stone_4_zone() x and y should be between 0 and 1", {
  expect_error(calculate_stone_4_zone(x = -.01, y = 1.01))
})
test_that("calculate_stone_4_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_4_zone(x = 0.5, y = 0.5), "Top_Right")
})
test_that("calculate_stone_4_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_4_zone(x = 0.5, y = 0.499), "Bottom_Right")
})
test_that("calculate_stone_4_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_4_zone(x = 0.499, y = 0.499), "Bottom_Left")
})
test_that("calculate_stone_4_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_4_zone(x = 0.499, y = 0.5), "Top_Left")
})

test_that("calculate_stone_9_zone()  x and y should be double", {
  expect_error(calculate_stone_9_zone(x = "fred", y = 0.45))
})
test_that("calculate_stone_9_zone()  x and y should be double", {
  expect_error(calculate_stone_9_zone(x = 0.45, y = "fred"))
})
test_that("calculate_stone_9_zone()  x and y should be double", {
  expect_error(calculate_stone_9_zone(x = "help", y = "fred"))
})
test_that("calculate_stone_9_zone()  x should be >= 0", {
  expect_error(calculate_stone_9_zone(x = -.01, y = 0.35))
})
test_that("calculate_stone_9_zone()  x should be <= 1", {
  expect_error(calculate_stone_9_zone(x = 1.01, y = 0.35))
})
test_that("calculate_stone_9_zone()  y should be >= 0", {
  expect_error(calculate_stone_9_zone(x = 0.35, y = -.01))
})
test_that("calculate_stone_9_zone()  y should be <= 1", {
  expect_error(calculate_stone_9_zone(x = 0.35, y = 1.01))
})
test_that("calculate_stone_9_zone() x and y should be >= 0", {
  expect_error(calculate_stone_9_zone(x = -.01, y = -.01))
})
test_that("calculate_stone_9_zone() x and y should be <= 1", {
  expect_error(calculate_stone_9_zone(x = 1.01, y = 1.01))
})
test_that("calculate_stone_9_zone() x and y should be between 0 and 1", {
  expect_error(calculate_stone_9_zone(x = 1.01, y = -.01))
})
test_that("calculate_stone_9_zone() x and y should be between 0 and 1", {
  expect_error(calculate_stone_9_zone(x = -.01, y = 1.01))
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.66666666, y = 0.66666666), "Top_Right")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.66666666, y = 0.33333333), "Centre_Right")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.66666666, y = 0.32), "Bottom_Right")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.33333333, y = 0.66666666), "Top_Centre")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.33333333, y = 0.33333333), "Centre")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.33333333, y = 0.32), "Bottom_Centre")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.32, y = 0.66666666), "Top_Left")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.32, y = 0.33333333), "Centre_Left")
})
test_that("calculate_stone_9_zone() Calculates Zone with x and y values for stone XLeft and YTop", {
  expect_equal(calculate_stone_9_zone(x = 0.32, y = 0.32), "Bottom_Left")
})
