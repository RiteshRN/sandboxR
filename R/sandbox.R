#' Sandboxed environment
#'
#' This function returns a special environment pre-loaded with bunch of forked functions from \code{base}, \code{stats}, \code{graphics} etc. to act as a jail for later evaluation.
#'
#' Some of the forked functions \code{stop}s by default (to prevent using those inside the environment), for a full list see \code{\link{commands.blacklist}}. Other functions (found in package's namespace) behaves differently then usual: some parameters are forbidden (like \code{eval}'s \code{env}) to prevent breaking out from the sandbox, some parameters and returned values are checked for "malicious" signs.
#' @param blacklist character vector of function names which should be banned
#' @return environment
#' @export
sandbox.env <- function(blacklist = as.character(unlist(commands.blacklist()))) {

    ## TODO: check if sandboxed env was created before and return that instead of regenerating

    ## prepare a custom environment with dummy functions
    sandboxed.env <- new.env()
    for (cmd in blacklist) {

        ## disarm forbidden function
        cmd.update <- sprintf("%s <- function(x) stop('Forbidden function called: %s')", cmd, cmd)

        base::eval(base::parse(text = cmd.update), envir = sandboxed.env)

    }

    return(sandboxed.env)

}


#' Checking expressions for malicious code
#'
#' This function tests a character vector of R commands agains a list of banned functions and \code{stop}s if any found.
#' @param src character vector of R commands
#' @param blacklist character vector of function names which should be banned
#' @param envir environment
#' @return invisibly \code{TRUE} if tests passed
#' @export
sandbox.pretest <- function(src, blacklist = as.character(unlist(commands.blacklist())), envir = parent.frame()) {

    ## dummy checks
    if (missing(src))
        stop('Nothing provided to check.')

    ## parse elements of src
    src.r <- suppressWarnings(tryCatch(base::parse(text = src), error = function(e) NULL))
    if (is.null(nrow(getParseData(src.r))))
        stop(paste0('Parsing command 1 (`', src, '`) failed, possible syntax error.'))
    p       <- attr(src.r, 'data')
    calls   <- sort(unique(p$text[which(p$token.desc == 'SYMBOL_FUNCTION_CALL')]))
    strings <- sort(unique(p$text[which(p$token.desc == 'STR_CONST')]))
    vars    <- sort(unique(p$text[which(p$token.desc == 'SYMBOL')]))
    pkgs    <- sort(unique(p$text[which(p$token.desc == 'SYMBOL_PACKAGE')]))

    ## filtering foreign calls
    if (length(pkgs) > 0)
        stop(sprintf('Tried to call at least one function outside of the active namespace from package%s: %s', ifelse(length(pkgs) == 1, '', 's'), paste0(pkgs, collapse = ', ')))
    NS <- which(p$token.desc == 'NS_GET')
    if (length(NS) > 0)
        stop(sprintf('Tried to call at least one function outside of the active namespace, probably from package%s: %s', ifelse(length(NS) == 1, '', 's'), paste0(p$text[NS - 1], collapse = ', ')))

    ## filtering forbidden function calls: e.g. get()
    calls.forbidden <- calls %in% blacklist
    if (any(calls.forbidden))
        stop(sprintf('Forbidden function%s called: %s.', ifelse(sum(calls.forbidden) == 1, '', 's'), paste0(calls[which(calls.forbidden)], collapse = ', ')))

    ## filtering forbidden functions used as symbol: e.g. lappy(foo, get)
    calls.forbidden <- vars %in% blacklist
    if (any(calls.forbidden))
        stop(sprintf('Forbidden function%s used as symbol: %s.', ifelse(length(calls.forbidden) == 1, '', 's'), paste0(vars[which(calls.forbidden)], collapse = ', ')))

    ## parse for quoted fns
    p <- base::parse(text = src)
    lapply(p, function(c) {

        d <- base::deparse(c)
        s <- suppressWarnings(tryCatch(base::parse(text = d), error = function(e) NULL))
        if (is.null(nrow(getParseData(s))))
            stop(paste0('Parsing command 2 (`', d, '`) failed, possible syntax error.'))
        l <- getParseData(s)
        f <- which(l$token.desc == 'SYMBOL_FUNCTION_CALL')
        calls <- l$text[f]

        if (length(f) > 0) {

            ## filtering forbidden function calls:
            calls.forbidden <- calls %in% blacklist
            if (any(calls.forbidden))
                stop(sprintf('Forbidden function%s called: %s.', ifelse(sum(calls.forbidden) == 1, '', 's'), paste0(calls[which(calls.forbidden)], collapse = ', ')))

            ## extract all sub-fn calls
            se <- data.frame(start = which(l$id %in% l$id[f]), end = sapply(l$parent[f+1], function(x) which(x == l$id)))
            if (nrow(se) > 1)
                fs <- sapply(apply(se, 1, function(x) l$text[x[1]:x[2]]), paste, collapse = '')
            else
                fs <- paste(l$text[se[1, 1]:se[1, 2]], collapse = '')

            ## check all fn calls for envir argument
            lapply(fs, function(S) {
                c <- base::parse(text = S)
                l <- match.call(base::get(as.character(c[[1]]), envir = envir), c)
                if (any(names(l) == 'envir'))
                    stop(sprintf('Tried to leave sandboxed enviroment with the "envir" argument of "%s".', as.character(l[[1]])))
            })

        }

    })

    return(invisible(TRUE))

}


#' Eval in sandbox
#' @param src character vector of R commands
#' @param envir the environment where the calls would be tested. This should be omitted or preset with \code{\link{sandbox.env}}.
#' @examples \dontrun{
#' sandbox('paste(rev(c(")", "whatever", "(", "m", "e", "t", "s", "y", "s")), sep = "", collapse = "")')
#' sandbox('get(paste("","y", "tem", sep="s"))("whoami")')
#' sandbox(c("x1 <- 's'", "x2 <- 'y'", "x3 <- 't'", "x4 <- 'e'", "x5 <- 'm'", "x <- paste(x1, x2, x1, x3, x4, x5, sep = '')", "lm(sprintf(\"%s('echo hello > /tmp/xxx') ~ 1\", x))"))
#' sandbox('paste("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")')
#' sandbox(c("x <- system", "x('ls')"))
#' sandbox('lm("as.numeric(system(\'ls -la | wc -l\', intern=T)) ~ 1")')
#' }
#' @export
sandbox <- function(src, envir) {

    if (missing(envir))
        envir <- sandbox.env()

    ## saving global options
    opts.bak <- options()

    ## parse expressions
    p <- base::parse(text = src)

    ## evaluate per expression and check
    res <- lapply(p, function(x) {

        sandbox.pretest(base::deparse(x), envir = envir)
        res <- tryCatch(base::eval(x, envir = envir), error = function(e) e)

        if (any(class(res) == 'error'))
            stop(res[[1]])

        return(res)

    })

    ## setting back global options and removing time limit
    options(opts.bak)
    return(res)

}
