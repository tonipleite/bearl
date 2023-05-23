#' aculog acumulates messages in a form of phrases in the variable ".log" and if the variable ".log_file" contains a file name, it prints there the sentences. Also, it takes into account the current level of left margin.
#' @export
aculog <- function(...){
    if (!exists(".marginLevel")) { .marginLevel <<- 0 }
    frase <- paste0(paste0(rep("\t", .marginLevel), collapse = ""), ..., " [", as.character(Sys.time()),"]\n")
    cat(frase)
    if (exists(".log_file")) { write(x = frase, file = .log_file, append = T) }
    if (!exists(".log")) { .log <<- "" }
    .log <<- paste0(.log, frase)
}
#' For more margin. It increases the margin in one single tab.
#' @export
moremarginlog <- function(){
    if (!exists(".marginLevel")) {
        .marginLevel <<- 0
    } else {
        .marginLevel <<- .marginLevel + 1
    }
}
#' For less margin. It decreases the margin in one single tab.
#' @export
lessmarginlog <- function(){
    if (!exists(".marginLevel")) {
        .marginLevel <<- 0
    } else {
        if (.marginLevel > 0) {
            .marginLevel <<- .marginLevel - 1
        }
    }
}

