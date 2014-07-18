#' Extract text contains between two tags
#' 
#' This function allows to extract all accurences of text contained between two tags.
#' 
#' @param txt the text to mine.
#' @param pattern.start the opening tag. Can be a static character string or a regex.
#' @param pattern.stop the closing tag. Can be a static character string or a regex.
#' @param quiet if \code{FALSE} the function prints for each match both the extracted text and the remaining text.
#' @export
#' @examples
#' txt <- "bla blo1 blu ble bla blo2 blu ble bla blo3 blu ble bla blo4 blu ble"
#' pattern.start = "bla"
#' pattern.stop = "blu"
#' 
#' debate.txt.extract.markup(
#'   txt,
#'   pattern.start,
#'   pattern.stop
#' )
#' 
#' txt <- "qdsfbsqdfiusd  \nM. Genecand. Contrib1. \n\nLe président. kjdsfpiou,kjsdfp. qsdfkjhi. \n\nM. Genecand. Contrib2. \n\nLe président. kjdsfpiou,kjsdfp. qsdfkjhi."
#' pattern.start = "\nM. Genecand."
#' pattern.stop = "\nLe président."
#' 
#' debate.txt.extract.markup(
#' txt,
#' pattern.start,
#' pattern.stop
#' )
debate.txt.extract.markup <- function(txt, pattern.start, pattern.stop, quiet=TRUE) {
  out <- list()
  i <- 0
  pattern.full = paste0("(.*)(", pattern.start, "(.+)", pattern.stop, ")(.*)")
  if(!quiet) {
    message(paste("Round ", i))
    print(txt)
    print(grep(pattern.full, txt))
  }
  while (length(grep(pattern.full, txt)) > 0) {
    i <- i+1
    current <- gsub(pattern.full, "\\3", txt)
    current <- strsplit(current, pattern.stop)
    out <- c(out, current[[1]][1])
    txt <- gsub(pattern.full, "\\1", txt)
    if(!nzchar(txt)) {break}
    if(!quiet) {
      message(paste("Round ", i))
      print(txt)
      print(grep(pattern.full, txt))
    }
  }
  out <- rev(out)
  return(out)
}



#' Count all occurences of keywords or sentences within a character string
#' 
#' This functions is basically a wrapper for the \code{\link[base]{gregexpr}} function, allowing to count all occurences of several items in one single step.
#' @param txt the character string to mine.
#' @param items a list of character strings for which we want to count occurences.
#' @export
#' @examples
#' path.to.pdf <- paste0(path.package("debate"), "/doc/BOACG_Tome_21_article_49.pdf")
#' txt <- debate.pdftotext(path.to.pdf)
#' txt <- debate.txt.clean(txt)
#' debate.txt.count.matches(txt = txt, items = "président")
#' debate.txt.count.matches(txt = txt, items = "Président")
#' debate.txt.count.matches(txt = txt, items = "soumis au vote de la séance plénière de l'Assemblée constituante")
#' debate.txt.count.matches(txt = txt, items = "qsdfbnmsdfui")
#' debate.txt.count.matches(txt = txt, items = c("qsdfbnmsdfui", "président", "merci", "soumis au vote"))
debate.txt.count.matches <- function(txt, items) {
  out <- mapply(.debate.txt.count.matches.aux, txt, items)
  names(out) <- items
  return(out)
}
.debate.txt.count.matches.aux <- function(txt, pattern) {
  stopifnot(length(txt) == 1)
  out <- gregexpr(pattern, txt)[[1]]
  if(out[1]==-1)
    return(0)
  else
    return(length(out))
}