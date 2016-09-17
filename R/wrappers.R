#' @export
PLLExecuteRemotelyBatch <- function(FUN, batchSize=1) {
  PLLTaskDefinition$new(FUN)$executeRemotelyBatch(batchSize)
}
