## Functions to create an R to Julia Bridge

#' Setup ALDEx2jlr
#'
#' This function initializes Julia and the ALDEx2.jl package.
#' The first time will be long since it includes precompilation.
#' Additionally, this will install Julia and the required packages
#' if they are missing.
#'
#' @param pkg_check logical, check for ALDEx2.jl package and install if necessary
#' @param ... Parameters are passed down to JuliaCall::julia_setup
#'
#' @examples
#' \dontrun{
#' ## aldex2jlr_setup() is time-consuming and requires Julia + ALDEx2.jl
#'
#' aldex2jlr::aldex2jl_setup()
#' }
#'
#' @export
aldex2jl_setup <- function(pkg_check = TRUE, ...) {
  JuliaCall::julia_setup(installJulia = TRUE, version = "1.9.4", ...)

  if (pkg_check) {
    JuliaCall::julia_command("using Pkg")
    JuliaCall::julia_command('Pkg.develop(path="/Users/markpampuch/Dropbox/KAUST/PhD/ALDEx2_jl/ALDEx2_dev/ALDEx2.jl/ALDEx2")')
  }

  JuliaCall::julia_library("ALDEx2")

  functions <- JuliaCall::julia_eval(
    'filter(isascii, replace.(string.(propertynames(ALDEx2)), "!" => "_bang"))'
  )
  aldex2jl <- julia_pkg_import("ALDEx2", functions)
  aldex2jl
}

#' Setup ALDEx2GPU.jl
#'
#' This function initializes Julia and the ALDEx2GPU.jl package.
#' The first time will be long since it includes precompilation.
#' Additionally, this will install Julia and the required packages
#' if they are missing.
#'
#' @param pkg_check logical, check for ALDEx2GPU.jl package and install if necessary
#' @param ... Parameters are passed down to JuliaCall::julia_setup
#'
#' @examples
#' \dontrun{
#' ## aldex2gpu_setup() is time-consuming and requires Julia + ALDEx2GPU.jl
#'
#' aldex2jlr::aldex2gpu_setup()
#' }
#'
#' @export
aldex2gpu_setup <- function(pkg_check = TRUE, ...) {
  JuliaCall::julia_setup(installJulia = TRUE, version = "1.9.4", ...)

  if (pkg_check) {
    JuliaCall::julia_command("using Pkg")
    JuliaCall::julia_command('Pkg.develop(path="/Users/markpampuch/Dropbox/KAUST/PhD/ALDEx2_jl/ALDEx2_dev/ALDEx2GPU.jl/ALDEx2GPU")')
  }

  JuliaCall::julia_library("ALDEx2GPU")

  functions <- JuliaCall::julia_eval(
    'filter(isascii, replace.(string.(propertynames(ALDEx2GPU)), "!" => "_bang"))'
  )
  aldex2gpu <- julia_pkg_import("ALDEx2GPU", functions)
  aldex2gpu
}

# Create a single R wrapper for the JuliaCall::julia_locate function to determine the location of the Julia executable from R (equivalent to JuliaCall:::julia_locate)
julia_locate <- do.call(":::", list("JuliaCall", quote(julia_locate)))

# Creates a single R wrapper for a Julia function
## Builds the fully qualified Julia function name (PackageName.functionName)
## Returns an R function that calls the Julia function via JuliaCall::julia_do.call
## Handles lazy initialization (calls env$setup() if not initialized)
julia_function <- function(
    func_name, pkg_name = "Main",
    env = emptyenv()) {
  # Convert R-friendly function name back to Julia function name (e.g., "hello_world_bang" -> "hello_world!")
  julia_func_name <- gsub("_bang$", "!", func_name)
  fname <- paste0(pkg_name, ".", julia_func_name) # Build the fully qualified Julia function name
  force(fname) # Ensures fname is evaluated immediately. Forces the evaluation of a lazy argument immediately, instead of waiting until it's first used. This ensures the variable keeps the value it had when the function was created, not some later value.
  f <- function(...,
                need_return = c("R", "Julia", "None"),
                show_value = FALSE) {
    if (!isTRUE(env$initialized)) {
      env$setup() # Lazy initialization: run setup if Julia is not ready yet
    }
    JuliaCall::julia_do.call(
      func_name = fname, list(...),
      need_return = match.arg(need_return),
      show_value = show_value
    )
  }
  force(f) # Ensure the function f is fully created before storing it in the environment.
  env[[func_name]] <- f # Store the R wrapper in the environment
}


# Creates an environment with R wrappers for all functions in a Julia package.
## Creates a new environment with a setup() function for lazy Julia initialization
## Uses julia_function to create wrappers for each function in the provided list
## Returns the environment so users can call env$functionName(...)
julia_pkg_import <- function(pkg_name, func_list) {
  env <- new.env(parent = emptyenv()) # Create a new environment
  env$setup <- function(...) { # Define setup() for lazy Julia initialization
    JuliaCall::julia_setup(...) # Start Julia
    JuliaCall::julia_library(pkg_name) # Load Julia package
    env$initialized <- TRUE # Mark as initialized
  }
  for (fname in func_list) {
    julia_function(
      func_name = fname,
      pkg_name = pkg_name,
      env = env
    )
  }
  env # Return the environment with wrappers for all functions in the package
}
