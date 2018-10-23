# plotCorMat.r
# written by JuG
# Fri Dec  1 21:58:59 2017"



#' Plot correlation matrix
#' @author JuG
#' @description Plot correlation matrix
#' @param cormat  correlation matrix
#' @param cexlab label size
#' @param angle.label cex.label size
#' @return plot
#' @examples
#'
#'  #correlation matrix - simulated (or alternative fact)
#'  corMat <- matrix(runif(400,-1,1),ncol=20)
#'  diag(corMat)<-1
#'  colnames(corMat) <- rownames(corMat) <- paste("Vble",1:20,sep='')
#'  #plot CorMat
#'  plotCorMat(corMat)
#'
#'  # Correlation matrix from mtcars
#'  corMat2 <-cor(mtcars)
#'  plotCorMat(corMat2)
#' @export

plotCorMat <- function(cormat, cexlab=12, angle.label=45){
  library(reshape2)
  library(ggplot2)
  # Obtenir le triangle inférieur
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Obtenir le triangle supérieur
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  reorder_cormat <- function(cormat){
    # Utiliser la corrélation entre les variables
    # comme mesure de distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  cormat [which(is.na(cormat))] <- 0
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  # Fondre la matrice de corrélation
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  # Créer un ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.y=element_text(size=cexlab))+
    theme(axis.text.x = element_text(angle = angle.label, vjust = 1, size = cexlab, hjust = 1))+
    coord_fixed()
  # Afficher heatmap
  print(ggheatmap)
}








