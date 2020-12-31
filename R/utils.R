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

wafflecut <- function (x, intervals, buckets = intervals, na.bucket = NA, 
                       unmatched.bucket = NA, out.as.factor = TRUE) {
  l <- length(intervals)
  if (l != length(buckets)) {
    stop("FancyCut requires a 1-1 map from intervals to buckets")
  }
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  out <- rep(NA, length(x))
  intervals_df <- nano:::parse_intervals(intervals)
  for (index in 1:l) {
    b <- buckets[index]
    lower <- intervals_df$left[index]
    upper <- intervals_df$right[index]
    left <- intervals_df$left_strict[index]
    right <- intervals_df$right_strict[index]
    mask <- rep(FALSE, length(x))
    if (left & right) {
      mask <- x >= lower & x <= upper
    }
    if (left & !right) {
      mask <- x >= lower & x < upper
    }
    if (!left & right) {
      mask <- x > lower & x <= upper
    }
    if (!left & !right) {
      mask <- x > lower & x < upper
    }
    out[mask] <- b
  }
  if (sum(is.na(x)) == 0L) {
    na.bucket <- NULL
  }
  else {
    out[is.na(x)] <- na.bucket
  }
  if (sum(is.na(out)) == 0L) {
    unmatched.bucket <- NULL
  }
  else {
    out[is.na(out)] <- unmatched.bucket
  }
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  if (out.as.factor) {
    return(factor(out, levels = levels, exclude = NULL))
  }
  else {
    return(out)
  }
}


# define parse_intervals function. From fancycut package but seems to be missing from the
# package now, so manualy created function here. 
parse_intervals <- function(intervals) {
  rx <- "^\\s*(\\(|\\[)\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*,\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*(\\)|\\])\\s*$"
  lindex <- regexec(rx, intervals)
  lmatch <- regmatches(intervals, lindex)
  nrows <- length(lmatch)
  ncols <- sapply(lmatch, length)
  mmatch <- matrix(NA_character_, nrow = nrows, ncol = 5)
  
  for (x in 1:nrows) {
    row <- lmatch[[x]]
    n <- length(row)
    if (n > 0) {
      mmatch[x, 1:n] <- lmatch[[x]][1:n]
    }
  }
  
  intervals_df <- data.frame(
    interval = intervals,
    left = as.numeric(mmatch[, 3]),
    right = as.numeric(mmatch[, 4]),
    left_strict = (mmatch[, 2] == '['),
    right_strict = (mmatch[, 5] == ']'),
    match_count = ncols,
    stringsAsFactors = FALSE
  )
  
  # Fix if point values
  rx <- "^[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?$"
  points <- grepl(rx, intervals)
  intervals_df$interval[points] <- intervals[points]
  intervals_df$left[points] <- as.numeric(intervals[points])
  intervals_df$right[points] <- as.numeric(intervals[points])
  intervals_df$right_strict[points] <- TRUE
  intervals_df$left_strict[points] <- TRUE
  intervals_df$match_count[points] <- 5
  
  
  for (x in 1:nrows) {
    
    if (intervals_df$match_count[x] != 5) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
      next
    }
    
    if (intervals_df$right[x] < intervals_df$left[x]) {
      warning(paste0('The interval "',intervals_df$interval[x],'" has right < left.'))
    }
    
    if (intervals_df$right[x] == intervals_df$left[x] &
        (!intervals_df$left_strict[x] | !intervals_df$right_strict[x])) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
    }
    
  }
  
  return(intervals_df)
}


# function copied from fancycut package 
wafflecut <- function (x, intervals, buckets = intervals, na.bucket = NA, 
                       unmatched.bucket = NA, out.as.factor = TRUE) {
  l <- length(intervals)
  if (l != length(buckets)) {
    stop("FancyCut requires a 1-1 map from intervals to buckets")
  }
  if (!is.numeric(x)) 
    stop("'x' must be numeric")
  out <- rep(NA, length(x))
  intervals_df <- nano:::parse_intervals(intervals)
  for (index in 1:l) {
    b <- buckets[index]
    lower <- intervals_df$left[index]
    upper <- intervals_df$right[index]
    left <- intervals_df$left_strict[index]
    right <- intervals_df$right_strict[index]
    mask <- rep(FALSE, length(x))
    if (left & right) {
      mask <- x >= lower & x <= upper
    }
    if (left & !right) {
      mask <- x >= lower & x < upper
    }
    if (!left & right) {
      mask <- x > lower & x <= upper
    }
    if (!left & !right) {
      mask <- x > lower & x < upper
    }
    out[mask] <- b
  }
  if (sum(is.na(x)) == 0L) {
    na.bucket <- NULL
  }
  else {
    out[is.na(x)] <- na.bucket
  }
  if (sum(is.na(out)) == 0L) {
    unmatched.bucket <- NULL
  }
  else {
    out[is.na(out)] <- unmatched.bucket
  }
  levels <- unique(c(buckets, na.bucket, unmatched.bucket))
  if (out.as.factor) {
    return(factor(out, levels = levels, exclude = NULL))
  }
  else {
    return(out)
  }
}






