
sandbox.parse <- function(src) {
  p <- base::parse(text = src)
  res <- lapply(p, function(x) {
    sandbox.parse_rec(base::deparse(x))
  })
  return(p)
}

sandbox.parse_rec <- function(src) {
  blacklist = as.character(unlist(commands.blacklist()))
  src.r <- suppressWarnings(tryCatch(base::parse(text = src), error = function(e) NULL))
  if (is.null(nrow(getParseData(src.r)))) {
    stop(paste0("Parse error"), call. = FALSE)
  }
  p <- getParseData(src.r)
  calls   <- sort(unique(p$text[which(p$token == 'SYMBOL_FUNCTION_CALL')]))
  vars    <- sort(unique(p$text[which(p$token == 'SYMBOL')]))
  pkgs    <- sort(unique(p$text[which(p$token == 'SYMBOL_PACKAGE')]))
  lefts   <- sort(unique(p$text[which(p$token == 'SYMBOL_SUB')]))

  # Filter foreign calls
  if (length(pkgs) > 0) {
    plurality <- ifelse(length(pkgs) == 1, '', 's')
    names <- paste0(pkgs, collapse = ', ')
    prefix <- 'Tried to call a function outside of the active namespace from package%s: %s'
    stop(sprintf(prefix, plurality, names), call. = FALSE)
  }
  NS <- which(p$token == 'NS_GET')
  if (length(NS) > 0) {
    plurality <- ifelse(length(NS) == 1, '', 's')
    names <- paste0(p$text[NS - 1], collapse = ', ')
    prefix <- 'Tried to call function outside of the active namespace, probably from package%s: %s'
    stop(sprintf(prefix, plurality, names), call. = FALSE)
  }

  # Filtering forbidden function calls: e.g. getwd()
  calls.forbidden <- calls %in% blacklist
  if (any(calls.forbidden)) {
    plurality <- ifelse(sum(calls.forbidden) == 1, '', 's')
    names <- paste0(calls[which(calls.forbidden)], collapse = ', ')
    stop(sprintf('Forbidden function%s called: %s.', plurality, names), call. = FALSE)
  }

  # Filtering forbidden functions used as symbols: e.g. lapply(foo, getwd)
  calls.forbidden <- vars %in% blacklist
  if (any(calls.forbidden)) {
    plurality <- ifelse(length(calls.forbidden) == 1, '', 's')
    names <- paste0(vars[which(calls.forbidden)], collapse = ', ')
    stop(sprintf('Forbidden function%s used as symbol: %s.', plurality, names), call. = FALSE)
  }

  # Don't allow anything like foo(envir = bar)
  if (any(lefts=="envir")) {
    stop(sprintf("Envir argument not allowed"), call. = FALSE)
  }

  return(invisible(TRUE))
}

