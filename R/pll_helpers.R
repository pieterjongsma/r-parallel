#' Retrieve task objects that exist in the R-Parallel service.
#'
#' @param definition.identifier The identifier of a function definition, as given in the R-Parallel web interface
#' @return A list of PLLRemoteTask objects for the given definition identifier
#' @examples
#' PLLRemoteTasksWithDefinition("29151F65-18AE-45E5-BD51-6D7E319BB9A8")
#' @export
PLLRemoteTasksWithDefinition <- function(definition.identifier) {
  path <- paste0("/task_definitions/", definition.identifier, ".json")
  response <- .RParallelGET(path)
  
  attributes <- content(response)$task_definition
  tasks <- lapply(attributes$task_ids, function(task_id) {
    return(PLLRemoteTask$new(task_id))
  })
  
  return(tasks)
}

#' Retrieve the results of a list of tasks.
#'
#' @param tasks A list of PLLTask objects
#' @return A list of the results of each of the PLLTask objects
#' @export
PLLResultsForTasks <- function(tasks) {
  lapply(tasks, function(task) task$result)
}

#' Retrieve the results of a list of tasks and join the results into a single list object.
#' This requires the result of each task to be a list object.
#'
#' @param tasks A list of PLLTask objects
#' @param join.attributes A list of strings specifying which keys to join
#' @export
PLLJoinedResultsForTasks <- function(tasks, join.attributes=NA) {
  if (.SafeIsNa(join.attributes)) join.attributes <- names(tasks[[1]]$result)
  
  results <- PLLResultsForTasks(tasks)
  results.joined <- lapply(join.attributes, function(attribute) {
    unlist(lapply(results, function(result) result[[attribute]]))
  })
  names(results.joined) <- join.attributes
  
  return(results.joined)
}

#' @export
PLLExecuteRemotelyBatch <- function(batchSize=1, FUN, ...) {
  PLLTaskDefinition$new(FUN)$executeRemotelyBatch(batchSize, ...)
}
