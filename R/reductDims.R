#' Performs PCA and t-SNE
#' The eigenvectors of PCA are specified as the input of t-SNE
#' @param input A two-dimensional matrix
#' @param dim_pca The number of lower dimension of PCA
#' @param dim_tsne The number of lower dimension of t-SNE
#' @param type_pca "svd" or "irlba" can be specified (Default: svd)
#' @param verbose Verbose option to show the message (Default: TRUE)
#' @return A list containing the results of PCA and t-SNE
#' @examples
#' input <- matrix(runif(50*100), nrow=50, ncol=100)
#' reductDims(input, dim_pca=10, dim_tsne=2)
#' @importFrom irlba irlba
#' @importFrom Rtsne Rtsne
#' @export
reductDims <- function(input, dim_pca=10, dim_tsne=2,
                       type_pca=c("svd", "irlba"), verbose=TRUE){
  # Argument Check
  .checkreductDims(input, dim_pca, dim_tsne, verbose)
  type_pca <- match.arg(type_pca)
  # Initialization
  scaled_input <- .initreductDims(input)
  # PCA
  if(type_pca == "svd"){
    if(verbose){
      message("SVD (svd) is performing...")
    }
    out_PCA <- svd(scaled_input, nu=dim_pca, nv=dim_pca)
  }
  if(type_pca == "irlba"){
    if(verbose){
      message("SVD (irlba) is performing...")
    }
    out_PCA <- irlba(scaled_input, nu=dim_pca, nv=dim_pca)
  }
  # t-SNE
  if(verbose){
    message("t-SNE is performing...")
  }
  out_tsne <- Rtsne(out_PCA$u, dims=dim_tsne, perplexity=10)
  # Output
  list(pca=out_PCA, tsne=out_tsne)
}

# Check Function
.checkreductDims <- function(input, dim_pca, dim_tsne, verbose){
  # 1. Check Input
  if(!is.matrix(input)){
    stop("Specify the input as a matrix object!")
  }
  # 2. Check dim_pca
  min_dim <- min(dim(input))
  if(dim_pca > min_dim){
    stop("Specify the dim_pca smaller than min(dim(input))!")
  }
  # 3. Check dim_tsne
  if(dim_tsne > min_dim){
    stop("Specify the dim_tsne smaller than min(dim(input))!")
  }
  # 5. Check verbose
  stopifnot(is.logical(verbose))
}

# Initialization Function
.initreductDims <- function(input){
  scale(input, center=TRUE, scale=FALSE)
}
