# newRepport.r
# written by JuG
# December 18 2017


#' Create Rnw file for data analysis repport template using LateX
#' @author JuG
#' @description Create Rnw file for data analysis repport template using LateX
#' @param nom Surname
#' @param prenom Forename
#' @param path path
#' @param filename filename
#' @details adapted from  Michael Schaeffer template (gmrcfun package)
#' @examples
#' newRepport(nom="G",prenom="J",filename = NULL,path=NULL)
#' newRepport(nom="G",prenom="J",path=getwd(),filename= "test")
#' @return Rnw file
#' @export



newRepport<- function(nom = "NOM", prenom = "PRENOM", path = NULL, filename = NULL){
  st1 <-"\\documentclass{article}\n\\usepackage{longtable}\n\\usepackage[french]{babel}\n\\usepackage[T1]{fontenc}\n\\usepackage[utf8]{inputenc}\n\\usepackage[table]{xcolor}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{latexsym}\n\\usepackage{lscape}\n\\usepackage{float}\n\\usepackage[colorlinks=true,urlcolor=blue]{hyperref}\n  "
  st2 <-paste("\n\\definecolor{oldlace}{rgb}{1.00, 0.976,0.93}\n\\newcommand{\\nom}{",nom,"} % le nom du clinicien (titre du rapport et bas de page)\n\\newcommand{\\prenom}{",prenom,"}% le prenom du clinicien (titre du rapport et bas de page)\n\n\n",sep='')
  st3 <-"\\voffset -0.2 in \n\\textheight 20cm \n\\headheight 0.6cm \n\\headsep 1 cm	\n\n\n\n\n\\begin{document}\n"
  if(grep("macOS",sessionInfo()$running)==1){
    st4 <-"\\begin{titlepage}\n\\begin{center}\n\\includegraphics[width=0.4\\textwidth]{/Users/jgodet/Seafile/MaBibliotheque/hus/logo1.png}~ \\\\ [0.5cm]\n
  \\textsc{\\normalsize Hôpitaux Universitaires de Strasbourg}\\\\ [2.5cm]\n\n\\textsc{\\LARGE Résultats d'analyses statistiques}\\\\ [0.5cm]\n\\textsc{\\LARGE pour}\\\\ [0.5cm]\n\\textsc{\\LARGE \\prenom ~\\nom}\\\\ \n"
  }else{
    st4 <-"\\begin{titlepage}\n\\begin{center}\n\\includegraphics[width=0.4\\textwidth]{/home/jgodet/Seafile/MaBibliotheque/hus/logo1.png}~ \\\\ [0.5cm]\n
  \\textsc{\\normalsize Hôpitaux Universitaires de Strasbourg}\\\\ [2.5cm]\n\n\\textsc{\\LARGE Résultats d'analyses statistiques}\\\\ [0.5cm]\n\\textsc{\\LARGE pour}\\\\ [0.5cm]\n\\textsc{\\LARGE \\prenom ~\\nom}\\\\ \n"
  }
  st5 <-"\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n"
  st6 <-"\\begin{minipage}{0.75\\textwidth}\n\\begin{flushright}\n
  \\emph{Julien GODET}\\\\ \n\\emph{MCU-PH}\\\\ \n
  \\emph{Pôle de Santé Publique}\\\\ \n\\emph{julien.godet@chru-strasbourg.fr}\\\\ \n\\emph{\\href{mailto:jgodet@unistra.fr}{julien.godet@unistra.fr}} \n
  ~\\\\ \n\\emph{\\today}\\\\ \n"
  st7 <-"\\end{flushright}\n\\end{minipage}\n\\vfill\n\\vfill\n\\end{center}\n\\end{titlepage}\n\\newpage\n\\tableofcontents\n\\newpage\n"
  st8 <-"\\section{Objectif}\n"
  st9 <- "\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\vfill\n\\end{document}"

  if(is.null(path)){
    cat(st1,st2,st3,st4,st5,st6,st7,st8,st9)
  }else{
    if(dir.exists(path)){
      if(is.null(filename)){
        cat(file= paste(path,"/Analyse",format(Sys.Date(), "%Y%m%d"),".Rnw",sep=''),  st1,st2,st3,st4,st5,st6,st7,st8,st9)
        }else{
        cat(file= paste(path,"/",filename,".Rnw",sep=''),  st1,st2,st3,st4,st5,st6,st7,st8,st9)
        }
     }else{
    cat("Directory not found -- no Rnw file created !")
     }
  }
}
