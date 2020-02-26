# This script is an example about how to perform PCA analysis

# Install packages (in case those haven't been installed before)
#install.packages("ggplot2")
#install.packages("FactoMineR")
#install.packages("factoextra")

# Load libraries
library(ggplot2)
library(FactoMineR)
library(factoextra)

# Load data (Example data iris)
iris_data <- iris
head(iris_data)

# Perform analysis
iris_PCA = prcomp(iris_data[,-5],scale = TRUE) # The last column contains the species name

# Principal components
iris_PCA_values <- data.frame(iris_PCA$x) # Extract the PCA values
iris_PCA_values$Species <- iris_data$Species # Adds the columns with the species name
head(iris_PCA_values)

# Obtain variance percentage
eigenvalues <- get_eigenvalue(iris_PCA) # Obtain eigen values and variance percentages
eigenvalues

# Plot PCA
plot_PCA <- ggplot(iris_PCA_values, aes(x = PC1, y = PC2))+
  geom_point(aes(color = iris_PCA_values$Species),size=5,alpha=0.9) +
  labs(x = paste("PC1: ",
                 format(round(eigenvalues["Dim.1","variance.percent"], 2), nsmall = 2),
                 "% variance"),
       y = paste("PC2: ",
                 format(round(eigenvalues["Dim.2","variance.percent"], 2), nsmall = 2),
                 "% variance")) +
  theme(axis.text = element_text(size=14),axis.title = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot_PCA

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(iris_PCA)

# Visualize PCA with package
fviz_pca_ind(iris_PCA,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

# Perform analysis with FactoMineR
iris.PCA <- PCA(iris[,-5],  graph = FALSE)

# Visualize PCA with package
fviz_pca_ind(iris.PCA,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)

# Get loadings from variables
var <- get_pca_var(iris.PCA)
var$contrib

# PCA variables contribution plot
fviz_pca_var(iris.PCA, col.var = "black")

# Contributions of variables to PC1
fviz_contrib(iris.PCA, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(iris.PCA, choice = "var", axes = 2, top = 10)

# Biplot
fviz_pca_biplot(iris.PCA, repel = TRUE,habillage = iris$Species,
                addEllipses = TRUE)