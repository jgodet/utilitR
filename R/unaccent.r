# unaccent.r
# written by JuG
# December 16 2017


#' Transform accented into unaccented characters
#' @author JuG
#' @description Transform accented into unaccented characters
#' @param text an in put that can be coerced as character
#' @details from \url{http://data.hypotheses.org/564}
#' @examples
#'unaccent(c("é","à","è","ù","ç",'ü',"a,"))
#'unaccent("À Süßen (Ba.-Wü.), j'ai mangé des Curry Würstchen")
#' @return text
#' @export


unaccent<- function(text){
    text <- gsub("['`^~\"]", " ", text)
    text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
    text <- gsub("['`^~\"]", "", text)
    return(text)
}
