# newRepport.r
# written by JuG
# July 22 2021


#' Create Rmd file for data analysis repport template using Rmd LateX pdf
#' @author JuG
#' @description Create Rmd file for data analysis repport template using Rmd/LateX-pdf
#' @param nom Surname
#' @param prenom Forename
#' @param path path
#' @param filename filename
#' @details adapted from  Michael Schaeffer template (gmrcfun package)
#' @examples
#' newRmdRepport(nom="G",prenom="J",filename = NULL,path=NULL)
#' newRmdRepport(nom="G",prenom="J",path=getwd(),filename= "test")
#' @return Rnw file
#' @export



newRmdRepport<- function(nom = "NOM", prenom = "PRENOM", path = NULL, filename = NULL){
  st1 <-"---\noutput:\n  pdf_document:\n    keep_tex: true\n\nurlcolor: blue\nheader-includes:\n   - \\definecolor{oldlace}{rgb}{1.00, 0.976,0.93}\n   - \\usepackage[francais]{babel}\n   - \\usepackage{lmodern}\n---\n\n```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE)\n```\n"
  #st2 <-paste("\n\\definecolor{oldlace}{rgb}{1.00, 0.976,0.93}\n\\newcommand{\\nom}{",nom,"} % le nom du clinicien (titre du rapport et bas de page)\n\\newcommand{\\prenom}{",prenom,"}% le prenom du clinicien (titre du rapport et bas de page)\n\n\n",sep='')
  #st3 <-"\\voffset -0.2 in \n\\textheight 20cm \n\\headheight 0.6cm \n\\headsep 1 cm	\n\n\n\n\n\\begin{document}\n"
  if(length(grep("macOS",sessionInfo()$running))>=1){
    st4 <-paste("\\begin{titlepage}\n\\begin{center}\n\\includegraphics[width=0.4\\textwidth]{/Users/jgodet/Seafile/Hus/logo1.png}~ \\\\ [0.5cm]\n
\\textsc{\\normalsize H\\^opitaux Universitaires de Strasbourg}\\\\ [2.5cm]\n\n\\textsc{\\LARGE R\\'esultats d'analyses statistiques}\\\\ [0.5cm]\n\\textsc{\\LARGE pour}\\\\ [0.5cm]\n\\textsc{\\LARGE  ", prenom, "~", nom,"}\\\\ \n",sep='')
  }else{
    if(length(grep("Ubuntu",sessionInfo()$running))>=1){
      st4 <-paste("\\begin{titlepage}\n\\begin{center}\n\\includegraphics[width=0.4\\textwidth]{/home/jgodet/Seafile/Hus/logo1.png}~ \\\\ [0.5cm]\n
\\textsc{\\normalsize Hôpitaux Universitaires de Strasbourg}\\\\ [2.5cm]\n\n\\textsc{\\LARGE R\\'esultats d'analyses statistiques}\\\\ [0.5cm]\n\\textsc{\\LARGE pour}\\\\ [0.5cm]\n\\textsc{\\LARGE  ", prenom, "~", nom,"}\\\\ \n",sep='')
    }else{
      st4 <-paste("\\begin{titlepage}\n\\begin{center}\n\\includegraphics[width=0.4\\textwidth]{C:/Users/godetjul/Seafile/hus/logo1.png}~ \\\\ [0.5cm]\n
\\textsc{\\normalsize Hôpitaux Universitaires de Strasbourg}\\\\ [2.5cm]\n\n\\textsc{\\LARGE R\\'esultats d'analyses statistiques}\\\\ [0.5cm]\n\\textsc{\\LARGE pour}\\\\ [0.5cm]\n\\textsc{\\LARGE  ", prenom, "~", nom,"}\\\\ \n",sep='')
    }
      }
  st5 <-"\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n"
  st6 <-"\\begin{minipage}{0.75\\textwidth}\n\\begin{flushright}\n
\\emph{Julien GODET}\\\\ \n\\emph{PU-PH au GMRC}\\\\ \n
\\emph{P\\^ole de Sant\\'e Publique}\\\\ \n\\emph{\\href{mailto:julien.godet@chru-strasbourg.fr}{julien.godet@chru-strasbourg.fr}}\\\\\ \n\\emph{\\href{mailto:jgodet@unistra.fr}{julien.godet@unistra.fr}} \n~\\\\ \n\\emph{\\today}\\\\ \n"
  st7 <-"\\end{flushright}\n\\end{minipage}\n\\vfill\n\\vfill\n\\end{center}\n\\end{titlepage}\n\\newpage\n\\tableofcontents\n\\newpage\n\n\n"
  st8 <-"# Objectifs et critères de jugement\n"


  if(is.null(path)){
    cat(st1,st4,st5,st6,st7,st8, sep = '')
  }else{
    if(dir.exists(path)){
      if(is.null(filename)){
        cat(file= paste(path,"/Analyse",format(Sys.Date(), "%Y%m%d"),".Rmd",sep=''),  st1,st4,st5,st6,st7,st8,sep='')
        }else{
        cat(file= paste(path,"/",filename,".Rmd",sep=''),  st1,st4,st5,st6,st7,st8,sep='')
        }
     }else{
    cat("Directory not found -- no Rmd file created !")
     }
  }
}
