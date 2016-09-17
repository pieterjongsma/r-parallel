.GenerateSeed <- function(n=1) {
  return(sample(1:999999, n))
}

.FunctionSignature <- function(FUN) {
  # Also consider using substitute()
  return(paste(deparse(FUN), collapse="\n"))
}

.AllObjects <- function(env=parent.frame(), objects=list()) {
  objectsAppended <- c(objects, ls(all.names=TRUE, envir=env))
  if (identical(env, globalenv())) { # Stop at global env; no need to include objects in base env
    return(objectsAppended)
  } else {
    .AllObjects(parent.env(env), objectsAppended)
  }
}

.SafeIsNa <- function(var) {
  typeof(var) == 'logical' && is.na(var)
}