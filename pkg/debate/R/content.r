#' Count occurences of terms defined within a dictionary
#' 
#' This function extracts from a text, represented as a character string, all occurences of terms defined in a dictionary.
#' 
#' @param txt the text in which to count occurences
#' @param dictionary the dictionary defining terms to count. See \code{\link{debate.example.dic}} for a example of the dictionary data structure expected by this function.
#' @param escape list of characters that have to be removed from the \code{text} parameter before analysis.
#' @param lowercase if \code{TRUE} the \code{txt} parameter will be lowercased before analysis.
#' @export
#' @examples
#' path.to.pdf <- paste0(path.package("debate"), "/doc/BOACG_Tome_21_article_49.pdf")
#' txt <- debate.pdftotext(path.to.pdf)
#' txt <- debate.txt.clean(txt)
#' dic <- debate.example.dic()
#' debate.content.extract.dictionary(txt, dic)
debate.content.extract.dictionary <- function(txt, dictionary, escape = "[!\"#$%&()*+,./:;<=>?^_|]", lowercase = TRUE) {
  
  #   [:punct:]
  #   Punctuation characters:
  #     ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
  txt <- gsub(escape, " ", txt)
  if(lowercase) txt <- tolower(txt)

  for (i in 1:length(dictionary)) {
    dictionary[[i]]$count = debate.txt.count.matches(txt=txt, items=dictionary[[i]]$match)
    #       dictionary[[i]]$count[is.na(dictionary[[i]]$count)] <- 0
    dictionary[[i]]$count.overall = sum(dictionary[[i]]$count)
  }
  return(dictionary)
  #   }
}


debate.content.extract.aux <- function(txt, contributors, dictionary = NULL, escape = "[!\"#$%&()*+,./:;<=>?^_|]", lowercase = TRUE) {
  # extract all contributors interventions
  for (i in 1:length(contributors)) {
    contrib = debate.txt.extract.markup(
      txt,
      contributors[[i]]$pattern.start,
      contributors[[i]]$pattern.stop
    )
    if(!is.null(dictionary)) {
      if(length(contrib)>0) {
        for (j in 1:length(contrib)) {
          contrib[[j]] <- list(
            value = contrib[[j]],
            count = debate.content.extract.dictionary(contrib[[j]], dictionary, escape = escape, lowercase = lowercase)
          )
        }
      }
    }
    contributors[[i]]$contributions = contrib
  }
  attr(contributors, "dictionary") <- dictionary
  return(contributors)
}



debate.content.extract.mps <- function(txt, contributors, escape = "[!\"#$%&()*+,./:;<=>?^_|]", lowercase = TRUE) {
  return(debate.content.extract.aux(
    txt = txt,
    contributors = contributors,
    dictionary = NULL,
    escape = escape,
    lowercase = lowercase
  ))
}


debate.content.extract.all <- function(txt, contributors, dictionary, escape = "[!\"#$%&()*+,./:;<=>?^_|]", lowercase = TRUE) {
  return(debate.content.extract.aux(
    txt = txt,
    contributors = contributors,
    dictionary = dictionary,
    escape = escape,
    lowercase = lowercase
  ))
}


debate.content.extract <- function(txt, contributors, dictionary, count.by = "intervention", escape = "[!\"#$%&()*+,./:;<=>?^_|]", lowercase = TRUE) {
  stopifnot(count.by %in% c("intervention", "occurence"))
  contributors.name <- character(0) # names of the contributors
  contributors.count <- numeric(0) # count for each item of the dictionary
  contributors.list <- list() # store the output
  
  x <- debate.content.extract.all(
    txt = txt,
    contributors = contributors,
    dictionary = dictionary,
    escape = escape,
    lowercase = lowercase
  )
  
  dic <- attr(x, "dictionary")
  
  dictionary.name.first <- character(0)
  for(d in 1:length(dic)) {
    dictionary.name.first <- c(dictionary.name.first, dic[[d]]$name)
  }
  print(dictionary.name.first)
  
  dictionary.name <- character(0)
  dictionary.count <- numeric(0)
  for(i in 1:length(x)) { # for all contributors
    current <- x[[i]]
    contributors.name <- c(contributors.name, current$name)
    if(length(current$contributions)>0) { # if there is at least one contribution
      for (j in 1:length(current$contributions)) { # for all contributions
        current.contrib <- current$contributions[[j]]$count
        for(k in 1:length(current.contrib)) { # for all items of the dictionnary
          if(i==1 && j==1) {dictionary.name.first <- c(dictionary.name.first, current.contrib[[k]]$name)}
          dictionary.name <- c(dictionary.name, current.contrib[[k]]$name)
          if(count.by == "occurence") {
            dictionary.count <- c(dictionary.count, current.contrib[[k]]$count.overall)
          }
          if(count.by == "intervention") {
            if (current.contrib[[k]]$count.overall > 0) {
              tmp = 1
            } else {
              tmp = 0
            }
            dictionary.count <- c(dictionary.count, tmp)
          }
        }
        #       print(dictionary.count) # ok
        #       if(i>1 || j>1) {
        #         stopifnot(all(dictionary.name == dictionary.name.first))
        #       }
        if(j==1) {
          contributors.count <- dictionary.count
        } else {
          contributors.count <- contributors.count + dictionary.count
        }
        
        dictionary.name <- character(0)
        dictionary.count <- numeric(0)
      }
    }
    contributors.list[[i]] <- contributors.count
    contributors.count <- numeric(0)
  }
  names(contributors.list) <- contributors.name
  
  out.nrow <- length(contributors.list)
  out.ncol <- length(contributors.list[[1]])
  out <- matrix(rep(NA, out.nrow*out.ncol), nrow = out.nrow)
  for (i in 1:length(contributors.list)) {
    out[i,] <- contributors.list[[i]]
  }
  dimnames(out) = list(contributors.name, dictionary.name.first)
  
  return(out)
}
