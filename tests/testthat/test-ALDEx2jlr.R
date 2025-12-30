test_that("Setup ALDEx2.jl works", {
  skip_on_cran() # Skip on CRAN since Julia setup is time-consuming
  skip_if_not_installed("JuliaCall")

  # Call setup function
  aldex2jl <- aldex2jl_setup(pkg_check = TRUE)

  # Test that hello_world returns the expected message
  result <- aldex2jl$hello_world()
  expect_equal(result, "Hello World from ALDEx2.jl!")

  # Test that hello_world! returns the expected message
  result_bang <- aldex2jl$hello_world_bang()
  print(result_bang)
  expect_equal(result_bang, "Hello World with a !BANG! in the name from ALDEx2.jl!")
})

test_that("Setup ALDEx2GPU.jl works", {
  skip_on_cran() # Skip on CRAN since Julia setup is time-consuming
  skip_if_not_installed("JuliaCall")

  # Call setup function for ALDEx2GPU.jl
  aldex2gpu <- aldex2gpu_setup(pkg_check = TRUE)

  # Test that hello_world_gpu returns the expected message
  result <- aldex2gpu$hello_world_gpu()
  expect_equal(result, "Hello World from ALDEx2GPU.jl!")

  # Test that hello_world_gpu! returns the expected message
  result_bang <- aldex2gpu$hello_world_gpu_bang()
  print(result_bang)
  expect_equal(result_bang, "Hello World with a !BANG! in the name from ALDEx2GPU.jl!")
})


test_that("julia_locate works and returns the correct Julia version", {
  skip_on_cran() # Skip on CRAN since Julia setup is time-consuming
  skip_if_not_installed("JuliaCall")

  # Call julia_locate to get the Julia executable path
  julia_path <- julia_locate()

  # Test that it returns a character string
  expect_type(julia_path, "character")

  # Test that the path is non-empty
  expect_true(length(julia_path) > 0)
  expect_true(nchar(julia_path[1]) > 0)

  # Test that the Julia version is 1.9.4
  # Get Julia version by running julia --version command
  expect_true(grepl("1\\.9\\.4", julia_path[1]))
})
