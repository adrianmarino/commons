#' Creates a variable under scope global
#' @param name variable name
#' @param value variable value
#' @export
set_global_var_value <- function(name, value) {
  eval(
    parse(text =paste("{", name, "<-", value, "}", sep = "")), 
    envir=.GlobalEnv
  )
}

#' Get a global variable value
#' @param name variable name
#' @return variable value
#' @export
get_global_var_value <- function(name) eval(parse(text = name), envir=.GlobalEnv)

