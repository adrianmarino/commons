#' Allows load a script file under global context. 
#' @param script_path a script path to load.
#' @export
import <- function(scritp_path) {
  tryCatch(
    expr = {
      source(scritp_path)
      print(paste("-> '", scritp_path, "' script loadded successfuly!", sep=''))
    },
    error = function(e){ 
      print(paste("-> ERROR to load '", scritp_path, "' script!", sep=''))
      print(e)
    }
  )  
}
