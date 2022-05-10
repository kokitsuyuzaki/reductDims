input <- matrix(runif(50*100), nrow=50, ncol=100)
out <- reductDims::reductDims(input, dim_pca=10, dim_tsne=2)

expect_equal(length(out), 2)

tmp <- tempfile()
png(file=tmp, width=1000, height=500)
reductDims::plotDims(out)
dev.off()

expect_true(file.exists(tmp))
