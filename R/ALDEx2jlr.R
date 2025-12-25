## Functions to create an R to Julia Bridge
# Creates a single R wrapper for a Julia function
## Builds the fully qualified Julia function name (PackageName.functionName)
## Returns an R function that calls the Julia function via JuliaCall::julia_do.call
## Handles lazy initialization (calls env$setup() if not initialized)
julia_function <- function(
    func_name, pkg_name = "Main",
    env = emptyenv()) {
  fname <- paste0(pkg_name, ".", func_name) # Build the fully qualified Julia function name
  force(fname) # Ensures fname is evaluated immediately. Forces the evaluation of a lazy argument immediately, instead of waiting until it’s first used. This ensures the variable keeps the value it had when the function was created, not some later value.
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
