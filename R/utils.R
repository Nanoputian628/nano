quiet <- function(fun, quiet = TRUE) {
  if (!quiet) 
    return(fun)
  sink(tempfile())
  on.exit(sink())
  invisible(force(fun))
}


set.tictoc <- function (which, id) {
  now <- proc.time()["elapsed"]
  aux <- getOption(which)
  name <- sprintf("%s_%s", which, id)
  if (is.null(aux)) 
    aux <- list()
  aux[[name]] <- now
  return(aux)
}


tic <- function(id = 1, quiet = FALSE) {
  tic <- set.tictoc("tic", id)
  options(tic = tic)
  if (!quiet) 
    message(sprintf("Tic `id = %s` start time: %s", 
                    id, Sys.time()))
  invisible(tic)
}


toc <- function (id = 1, msg = "Elapsed time:", units = TRUE, 
                 signif = 3, quiet = FALSE) {
  if (!sprintf("tic_%s", id) %in% names(getOption("tic"))) 
    stop(sprintf("You need to tic(id = '%s') before you toc(id = '%s')", 
                 id, id))
  toc <- set.tictoc("toc", id)
  options(toc = toc)
  tic <- getOption("tic")[[paste0("tic_", id)]]
  toc <- getOption("toc")[[paste0("toc_", id)]]
  time <- as.numeric(toc - tic)
  if (units) {
    x <- time
    u <- ifelse(x < 60, "s", ifelse(x < 3600, "m", 
                                    ifelse(x < 86400, "h", "d")))
    d <- ifelse(x < 60, 1, ifelse(x < 3600, 60, ifelse(x < 
                                                         86400, 3600, 86400)))
    timer <- paste0(signif(time/d, signif), u)
  }
  else timer <- paste0(signif(time, signif), "s")
  msg <- sprintf("%s %s", msg, timer)
  if (!quiet) 
    message(msg)
  res <- list(tic = tic, toc = toc, time = time, msg = msg)
  invisible(res)
}






