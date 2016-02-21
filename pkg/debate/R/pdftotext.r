#' Convert a PDF file to a text file
#' 
#' This function extracts text from a PDF file and store it in a character string.
#' 
#' @param file the path of the file which the text have to be extract from.
#' @param backend specify the tool to be use for extracting the text. Current options are \code{pdftotext} and \code{tesseract}. See details.
#' @param control control parameters to use with \code{debate.pdftotext}. Ignore when using \code{tesseract}.
#' @param lang language to use when performing an OCR task (\code{tesseract} option). Language codes are based on ISO 963-2. Please consult the \code{tesseract} website for more information.
#' @details It is not so easy to extract text from a PDF file. 
#' Option 1: The \code{pdftotext} command works well in a lot of cases and is quite efficient. We recommend the user to start with this option. To use this option, the \code{pdftotext} command have to be installed on your system. On most Linux distributions, the command is included as part of the \code{poppler-utils} package. On Windows, the command is included as part of the \code{Xpdf} software that you can download from it official website (\url{http://www.foolabs.com/xpdf}).
#' Option 2: Some PDF file can embed complex structures as for example several layers, fonts or forms. Although the PDF is correctly rendered by the PDF viewer, the conversion to text partially fails and a lot of text may be lost or corrupted. To avoid this, a strategy can be to convert the PDF file into a image and then pass it through an Optical Character Recognition (OCR) software. This second option generally achieve better results, but is also more time consuming. The \code{tesseract} option allows to run this strategy using the tesseract OCR. For using this strategy, you first need to install the tesseract OCR. On most linux distributions the software is provided as part of the \code{tesseract-ocr} package. You may also need to install specific language utilities, for example the \code{tesseract-ocr-fra} package for enabling French support. Windows users can download the software from the tesseract website (\url{https://code.google.com/p/tesseract-ocr/}). You also need to install the \code{convert} utility. This utility, part of the \code{imagemagick} software, is used in backend for converting PDF files into images. On most linux distribution the command is included as part of the \code{imagemagick} package. Windows users can download \code{imagemagick} from its official website (\url{http://www.imagemagick.org/}).
#' @export
#' @examples
#' \dontrun{
#' 
#' path.to.pdf <- paste0(path.package("debate"), "/doc/BOACG_Tome_21_article_49.pdf")
#' 
#' mytext1 <- debate.pdftotext(path.to.pdf)
#' 
#' mytext2 <- debate.pdftotext(path.to.pdf, backend = "tesseract", lang = "fra")
#' # we save the output to avoid running the OCR again
#' save(mytext2, file = "mytext2.RData")
#' }
debate.pdftotext <- function(file, backend = "debate.pdftotext", control = c('-layout'), lang = "eng") {
  stopifnot(backend %in% c("debate.pdftotext", "tesseract"))
  
  if(backend == "debate.pdftotext") {
    # convert pdf to txt
    debate.pdftotext.path <- Sys.which('pdftotext')
    if(!nzchar(debate.pdftotext.path)) {
      stop("Unable to find the 'debate.pdftotext' command. You need it to use this functionnality.")
    }
    
##### depreciated:start
#     cmd <- paste("debate.pdftotext", paste(control, collapse=" "), file)
#     system(cmd)
#     # get txt-file name and open it
#     filetxt <- sub(".pdf", ".txt", file)
#     # filetxt <- sub(".pdf", ".txt", basename(file))
#     
#     txt <- readChar(filetxt, file.info(filetxt)$size)
#     file.remove(filetxt) # remove txt file
##### depreciated:end

    cmd <- paste("debate.pdftotext", paste(control, collapse=" "), file, "-")
    txt <- paste(system2(debate.pdftotext.path, args=c(paste(control, collapse=" "), file, "-"), stdout=TRUE, stderr=TRUE), collapse = "\n")

  }
  
  if(backend == "tesseract") {
    tesseract.path <- Sys.which('tesseract')
    if(!nzchar(tesseract.path)) {
      stop("Unable to find the 'tesseract' command. You need it to use this functionnality.")
    }
    convert.path <- Sys.which('convert')
    if(!nzchar(convert.path)) {
      stop("Unable to find the 'convert' command (from 'imagemagic'). You need it to use this functionnality.")
    }
    
    cat("This operation may take a long time...\n")
    d <- tempdir()
    if(!file.exists(d)) {dir.create(d)}
    d <- paste0(d, "/convert")
    dir.create(d)
    
    cat("* Converting the PDF file to JPG images\n")
    cmd.convert <- paste("convert -density 300 -quality 85", file, paste0(d,"/in-%06d.jpg"))
    #     print(cmd.convert)
    system(cmd.convert)
    
    f <- list.files(d, pattern=".jpg$")
    #     print(f)
    txt <- ""
    
    cat("* Processing pages through the OCR\n")
    for (i in 1:length(f)) {
      cat("\r", paste(rep(' ', 255), collapse=''), sep='')
      flush.console()
      cat(paste0("\r    => ", i, "/", length(f)))
      flush.console()
      f.tmp <- paste0(d, '/', paste0("out", i))
      f.tmp.txt <- paste0(f.tmp, ".txt")
      #       cmd.tess <- paste("tesseract -l", lang, paste0(d, '/', f[i]), f.tmp)
      cmd.tess.out <- system2(tesseract.path, args=c(paste("-l", lang), paste0(d, '/', f[i]), f.tmp), stdout=TRUE, stderr=TRUE)
      txt <- paste(txt, readChar(f.tmp.txt, file.info(f.tmp.txt)$size))
    }
    cat("\n* Deleting temporary files\n")
    unlink(d, recursive= TRUE)
    cat("Done.")
  }
  
  return(txt)
}


#' Clean a character string by remplacing non-standard characters
#' 
#' This function replace non-standard characters within a character strings. This especially concerns characters coming from a PDF to TXT conversion.  For example `’` will be replace by `'`.
#' 
#' @param txt the character string to be clean
#' @export
#' @examples
#' debate.txt.clean("--  ’  --  \\f  --  -\\n  --")
debate.txt.clean <- function(txt){
  #   txt <- tolower(x)
  txt <- gsub(parse(text = paste0("'", '\u2019', "'"))[[1]],"'", txt)
  txt <- gsub("-\\n","", txt)
  txt <- gsub("\\f","\\n\\n\\n\\n", txt)
  #   txt <- removeWords(txt, c("\\f", stopwords()))
  return(txt)
}

### depreciated
# corpus.prepare <- function(x){
#   corpus <- Corpus(VectorSource(x))
# #   corpus <- tm_map(corpus, removePunctuation)
# }