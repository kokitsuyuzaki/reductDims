#' Plot PCA and t-SNE
#' Output object of reductDims is specified as the input
#' @param out Output object of reductDims
#' @return Two scatter plots (PCA and t-SNE)
#' @examples
#' input <- matrix(runif(50*100), nrow=50, ncol=100)
#' out <- reductDims(input, dim_pca=10, dim_tsne=2)
#' tmp <- tempfile()
#' png(file=tmp, width=1000, height=500)
#' reductDims::plotDims(out)
#' dev.off()
#' @importFrom grDevices rgb
#' @importFrom graphics layout
#' @export
plotDims <- function(out){
  layout(t(1:2))
  plot(out$pca$u[,1:2], main="PCA",
       xlab="Dim1", ylab="Dim2",
       col=rgb(0,0,1), pch=16)
  plot(out$tsne$Y, main="t-SNE",
       xlab="Dim1", ylab="Dim2",
       col=rgb(0,0,1), pch=16)
}
