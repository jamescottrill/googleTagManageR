#' Function argument names
#' @noRd
function_args <- function(f, include_dots = FALSE){
  if(include_dots){
    return(names(formals(f))) 
  }
  setdiff(names(formals(f)),"...")
}

#' check package installed
#' @noRd
#' @importFrom purrr walk
check_packages_installed <- function(x, stop_if_not = TRUE, load_them = FALSE){
  if(is.null(x)){
    return()
  }
  stopifnot(is.character(x))
  
  check_one <- function(y){
    its_here <- TRUE
    if (!requireNamespace(y, quietly = TRUE)) {
      nope <- sprintf("%s needed for this function to work. Please install it via install.packages('%s')",
                      y,y)
      if(stop_if_not) stop(nope, call. = FALSE) else myMessage(nope)
      if(!stop_if_not) its_here <- FALSE
    }
    return(its_here)
  }
  
  walk(x, check_one)
  
  if(load_them){
    walk(x, library, character.only = TRUE)
  }
  
}

# common paging function
get_attr_nextLink <- function(x){
  attr(x, "nextLink")
}

#' Custom message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' @keywords internal
#' @noRd
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    message(Sys.time() ,"> ", ...)
  }
  
}