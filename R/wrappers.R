PLLExecuteRemotelyBatch <- function(FUN, batchSize=1) {
  PLLTaskDefinition(FUN)$executeRemotelyBatch(batchSize)
}
