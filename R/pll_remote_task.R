#' @export
PLLRemoteTask <- R6Class("PLLRemoteTask",
  inherit = PLLTask,
  
  public = list(
    templateTask = NA,
    
    initialize = function(object) {
      if (inherits(object, "PLLTaskDefinition")) {
        super$initialize(object)
      } else if (is.character(object)) {
        private$remoteIdentifier <- object
      }
    },
    
    execute = function(..., attach=.AllObjects()) {
      FUN <- private$taskDefinition$FUN
      ARG <- list(...)
      
      attachFile <- file.path(tempdir(), paste0(UUIDgenerate(), "-attach.RData"))
      # Note: ARG can be used with do.call(FUN, ARG)
      attach <- as.character(c("FUN", "ARG", attach))
      save(list=attach, file=attachFile) # Can be loaded with load()
      
      configuration <- private$taskDefinition$configuration
      if (self$hasTemplateTask) {
        configuration$task$template_task_id <- self$templateTask$identifier
      }
      
      response <- .RParallelPOST("/api/tasks",
                             body=configuration,
                             encode="json")
      taskAttributes <- content(response)$task
      private$attributes <- taskAttributes
      private$remoteIdentifier <- private$attributes$id
    
      # Sample bash script for uploading to S3: http://tmont.com/blargh/2014/1/uploading-to-s3-in-bash
      uploadUrl <- private$attributes$environment_put_location
      response <- .PUTFile(uploadUrl, attachFile)
      
      # Remove scratch file
      file.remove(attachFile)
    
      return(self)
    },
    
    print = function() {
      cat("<PLLRemoteTask>\n")
      if (!is.na(private$remoteIdentifier)) {
        cat(private$remoteIdentifier)
        cat("\n")
      }
      if (self$hasTemplateTask) {
        cat("Template: ")
        cat(self$templateTask$identifier)
        cat("\n")
      }
    }
  ),
  
  private = list(
    remoteIdentifier = NA,
    
    attributes = NA,
    
    resultFile = NA,
    
    obtainResult = function() {
      cat("Result has not yet been downloaded. Downloading now...\n")
      
      private$resultFile <- file.path(tempdir(), paste0(UUIDgenerate(), "-result.RData"))
      
      path <- paste0("/api/tasks/", private$remoteIdentifier, "/result")
      
      # Check for result-file existence
      retryInterval = 5 # seconds
      timeout = 60*60 # 1 hour
      maxTries = round(timeout / retryInterval)
      for (i in 0:maxTries) {
        response <- .RParallelHEAD(path)
        if (response$status_code < 400) {
          break
        } else if (response$status_code == 406) {
          if (i == 0) {
            cat("Result is not yet available. Waiting for task to finish...")
          } else if (i == maxTries) {
            cat("Waiting for result timed out")
            return(FALSE)
          }
        }
      }
      
      # Get the actual response
      response <- .RParallelGET(path,
                            write_disk(private$resultFile)) # FIXME Replace with function that does not check for json
      
      load(private$resultFile)
      private$cachedResult <- OUTPUT
      return(TRUE)
    }
  ),
  
  active = list(
    identifier = function() {
      private$remoteIdentifier
    },
    
    hasTemplateTask = function() {
      typeof(self$templateTask) == "environment"
    }
  )
)
