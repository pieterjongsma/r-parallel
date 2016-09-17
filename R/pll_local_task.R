#' @export
PLLLocalTask <- R6Class("PLLLocalTask",
  inherit = PLLTask,
  
  public = list(
    execute = function(...) {
      private$cachedResult <- do.call(private$taskDefinition$FUN, list(...))
      return(self)
    }
  )
)