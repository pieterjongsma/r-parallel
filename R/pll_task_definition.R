#
# PLLTaskDefinition
# Wraps a function and provides methods to execute the function locally or remotely
#

#' @export
PLLTaskDefinition <- R6Class("PLLTaskDefinition",
  public = list(
    FUN = NA,
    identifier = NA,
    maxDuration = 5*60*60,
    configuration = list(),
    
    initialize = function(FUN) {
      self$FUN <- FUN
      self$identifier <- UUIDgenerate()
      
      self$configuration <- list(
        task=list(
          signature=unbox(.FunctionSignature(self$FUN)),
          max_duration=unbox(self$maxDuration),
          definition_id=unbox(self$identifier)
        )
      )
      
      private$localIdentifier <- UUIDgenerate()
    },
    
    executeLocally = function(...) {
      task <- PLLLocalTask$new(self)
      task$execute(...)
      return(task)
    },
    
    # executeThreaded = function(...) {
    #   task <- PLLLocalThreadedTask$new(self)
    #   task$execute()
    #   return(task)
    # },
    
    executeRemotely = function(...) {
      task <- PLLRemoteTask$new(self)
      task$execute(...)
      return(task)
    },
    
    executeRemotelyBatch = function(batchSize=1, ...) {
      templateTask <- NA
      lapply(1:batchSize, function(i) {
        task <- PLLRemoteTask$new(self)
        
        if (typeof(templateTask) != "environment") {
          templateTask <<- task # Will be assigned to tasks created in subsequent iterations
          task$execute(...)
        } else {
          task$templateTask <- templateTask
          task$execute(..., attach=list()) # Do not attach environment. This will have been attached to the template task
        }
        
        return(task)
      })
    },
    
    print = function() {
      cat("<PLLTaskDefinition>\n")
      cat(.FunctionSignature(self$FUN))
      cat("\n")
    }
  ),
  
  private = list(
    localIdentifier = NA
  )
)
