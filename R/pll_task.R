#
# PLLTask is an object (reference type) to reference a task while it is being executed.
# PLLTask should be subclassed and never used directly.
# Default subclasses: PLLLocalTask, PLLRemoteTask
#

#' @export
PLLTask <- R6Class("PLLTask",
  public = list(
    initialize = function(taskDefinition) {
      private$taskDefinition = taskDefinition
    },
    
    print = function() {
      cat("<PLLTask>\n")
    },

    hasResult = function() {
      return(!.SafeIsNa(private$cachedResult))
    }
  ),
  
  private = list(
    taskDefinition = NA,
    cachedResult = NA,

    obtainResult = function() {
      stop("obtainResult() not implemented")
    }
  ),

  active = list(
    result = function() {
      if (!self$hasResult()) {
        if (private$obtainResult()) {
          return(private$cachedResult)
        } else {
          return(NA)
        }
      } else {
        return(private$cachedResult)
      }
    }
  )
)
