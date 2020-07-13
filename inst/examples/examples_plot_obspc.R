# For a prcomp object
pca1 <- prcomp(USArrests, scale=TRUE)  # Example
plot_obspc(pca1) # Default plot
plot_obspc(pca1, lchar=3, col='blue', pch=8)  # Customized plot

# For a princomp object
pca2 <- princomp(x=USArrests, cor=TRUE)
plot_obspc(pca2, col='red', ylim=c(-4, 4), xlim=c(-4, 4)) 
