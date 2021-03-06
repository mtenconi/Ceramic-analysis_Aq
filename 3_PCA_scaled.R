# scale transformation
data<- new_data[5:34]
data <- scale(data)
data <- data[,-c(11)]  # S exclusion from dataset
data <- data.frame(sample_id = data_xrf$sample_id, fabric = new_data$fabric, data)
rownames(data) <- new_data$sample_id

# Perform pca
pca <- prcomp(data[,3:31], center = TRUE, scale = FALSE)

# Analysis of results:
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))
round(get_eigenvalue(pca)[1:4,],2)
# first 4 principal components explain 67% of the total variation in the dataset

# Contribution of variables to PC1 and PC2 (Dim1, Dim2)
fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) + 
  theme_minimal() + 
  ggtitle("Variables - PCA")

# Contribution of variables to PC1, PC2, PC3
var <- get_pca_var(pca)
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
fviz_contrib(pca, choice = "var", axes = 3, top = 10)

head(var$contrib[,1:4], 4)

#########################################################################
#########################################################################
# Plots of results:

# Biplot
# Most important (or, contributing) variables highlighted
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "contrib", # Variables color
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.ind = "#696969",  # Individuals color
) + theme_minimal() + ggtitle("PCA - Biplot")


# PCA graph of individuals with most important (or, contributing) highlighted
fviz_pca_ind(pca, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_contrib(pca, choice = "ind", axes = 1:2, top = 10)

fviz_pca_ind(pca, habillage=new_data$fabric, 
             labelsize = 1, pointsize =3, font.family = "Arial",
             geom = "point",
             pointshape = 16) + 
  scale_color_brewer(palette="Paired",name = "Fabric")


#############################################################################
# new data set obtained by the pca, with pc1, pc2, pc3:
pca_df <- data.frame(pc1 = pca$x[,1], pc2 = pca$x[,2], pc3 = pca$x[,3], 
                     fabric = data$fabric)

# Scatter plot pf pc1 and pc2, grouped by fabric
pca12 <- pca_df %>% 
  ggplot(aes(pc1, pc2,color = fabric, fill = fabric)) +
  geom_point(aes(color = fabric, fill = fabric), size = 3) +
  #  geom_encircle(data = subset(pca_df, fabric == 1, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 2, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 3, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 4, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_text_repel(aes(pc1, pc2, label = new_data$sample_id), size = 3 )+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"),
        legend.position="none")
pca12

pca13 <- pca_df %>% 
  ggplot(aes(pc1, pc3,color = fabric, fill = fabric)) +
  geom_point(aes(color = fabric, fill = fabric), size = 3) +
  #  geom_encircle(data = subset(pca_df, fabric == 1, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 2, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 3, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 4, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_text_repel(aes(pc1, pc3, label = new_data$sample_id), size = 3 )+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"),legend.position="none")
pca13

pca23 <- pca_df %>% 
  ggplot(aes(pc2, pc3,color = fabric, fill = fabric)) +
  geom_point(aes(color = fabric, fill = fabric), size = 3) +
  #  geom_encircle(data = subset(pca_df, fabric == 1, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 2, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 3, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_encircle(data = subset(pca_df, fabric == 4, size =4, s_shape= 1, expand=0.05, spread = 0), alpha = 0.2) +
  #  geom_text_repel(aes(pc2, pc3, label = new_data$sample_id), size = 3 )+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"),
        legend.key = element_blank())
pca23

grid.arrange(pca12, pca13, pca23, ncol = 1)
