#' An example of data structure used in this package for storing a dictionary
#' 
#' This function returns a basic example of dictionary data structure expected to be processed by the \code{\link{debate.content.extract.dictionary}} function FIXME.
#' @export
#' @examples
#' dic <- debate.example.dic()
#' dic
debate.example.dic <- function() {
  return(list(
    list(
      name = "Naturalisation",
      positive.matches = c("naturalisation", "naturalisations", "naturaliser", "naturalisent",
                "national","nationaux","nationale","passeport","droit du sol",
                "droit du sang", "devenir suisse"),
      negative.matches = character(0)
      
    ),
    list(
      name = "Multiculturalisme",
      positive.matches = c("multiculturalisme","multiculturel","multiculturelle","cosmopolite",
                "richesse","enrichissent","rayonnement"),
      negative.matches = character(0)
    ),
    list(
      name = "Motivation",
      positive.matches = c("taux de participation","ViVRE","Kultura","n'en ont pas fait la demande",
                "revendication est largement minoritaire","ils participeraient moins"),
      negative.matches = character(0)
    )
  ))
}


#' An example of data structure used in this package for storing the members of parliament list
#' 
#' This function returns a basic example of list of members of parliament list data structure expected to be processed by the \code{\link{debate.content.extract.mps}}.
#' The member of parliament list is a list whose each element represents a member of parliament. Each member of parliament is also represented as a list. This list have to contain three items:
#' 1. \code{name}: the name of the member of parliament
#' 2. \code{pattern.start}: a character string which will be used to detect a start of speech.
#' 3. \code{pattern.stop}: a character string which will be used to detect a end of speech.
#' @export
#' @examples
#' mp <- debate.example.mp()
#' mp
debate.example.mp <- function() {
  return(list(
      list(
        name = c("Alder"),
        pattern.start = c("\nM. Murat Julian Alder."),
        pattern.stop = c(.example.lepresident())
      ),
      list(
        name = c("Gardiol"),
        pattern.start = c("\nM. Maurice Gardiol."),
        pattern.stop = c(.example.lepresident())
      ),
      list(
        name = c("Kunz"),
        pattern.start = c("\nM. Pierre Kunz."),
        pattern.stop = c(.example.lepresident())
      ),
      list(
        name = c("Maurice"),
        pattern.start = c("\nM. Antoine Maurice."),
        pattern.stop = c(.example.lepresident())
      ),
      list(
        name = c("Tschudi"),
        pattern.start = c("\nM. Pierre-Alain Tschudi."),
        pattern.stop = c(.example.lepresident())
      ),
      list(
        name = c("Zwahlen"),
        pattern.start = c("\nM. Guy Zwahlen."),
        pattern.stop = c(.example.lepresident())
      )
    ))
}

.example.lepresident <- function() {
  return(paste0("\nLe pr", parse(text = paste0("'", '\u00e9', "'"))[[1]], "sident."))
}