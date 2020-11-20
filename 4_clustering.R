# Assess clustering tendency: evaluate whether the dataset contains meaningful clusters or not 
#Hopkins' statistic & visual approach
hc_data <- new_data[,5:34]  
hc_data <- hc_data[-c(11)]   

# Visual approach: nr of square shaped blocks along the diagonal 
# Hopkins' statistic: H value > 0.5 = the dataset is clusterable
gradient.color <- list(low = "red",  high = "yellow")
set.seed(123)
hc_data %>%
  scale() %>%
  get_clust_tendency(n = 35, gradient = gradient.color)

#########################################################################################
# Determining the optimal number of clusters

# Methods:
hc_data <- scale(hc_data)
head(hc_data,2)
dim(hc_data)
rownames(hc_data) <- new_data$sample_id

# Elbow method: the location of a bend (knee) in the plot = indicator of the appropriate K
fviz_nbclust(hc_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method: the location of the max = appropriate number of clusters
fviz_nbclust(hc_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# # Gap statistic
set.seed(123)
gap_stat <- clusGap(hc_data, FUN = kmeans, nstart = 30, K.max = 10, B = 500)
fviz_gap_stat(gap_stat) + theme_minimal() + ggtitle("fviz_gap_stat: Gap Statistic") 

# NbClust() function: 30 indices for choosing the best number of clusters
set.seed(123)
res.nbclust <- hc_data %>%
  NbClust(distance = "euclidean",min.nc = 2, max.nc = 10,method = "complete", index ="all")
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

# Elbow method: 4 clusters solution suggested
# Silhouette method: 3 clusters solution suggested
# Gap statistic method: 1 cluster solution suggested
# NbClust, several methods ("complete",...): 3 clusters suggested

###########################################################################################
# K-means clustering: 
set.seed(123)
km.res <- kmeans(hc_data, 3, nstart = 30)   # k = 3

# Visualization
fviz_cluster(km.res, data = hc_data,
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE, 
             ggtheme = theme_minimal())

# comparison of fabrics with kmean clusters:
new_data$fabric[which(km.res$cluster == 1)]
new_data$fabric[which(km.res$cluster == 2)]
new_data$fabric[which(km.res$cluster == 3)]

###########################################################################################
# Examine several different values of k:
kmean_calc <- function(df, ...){
  kmeans(df, scaled = ..., nstart = 30)
}

km2 <- kmean_calc(hc_data, 2)
km3 <- kmean_calc(hc_data, 3)
km4 <- kmeans(hc_data, 4)
p1 <- fviz_cluster(km2, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 2") 
p2 <- fviz_cluster(km3, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 3")
p3 <- fviz_cluster(km4, data = hc_data, frame.type = "convex") + theme_minimal() + ggtitle("k = 4")
plot_grid(p1, p2, p3, labels = c("k2", "k3", "k4"))
plot_grid(p1, labels = c("k2"))
plot_grid(p2, labels = c("k3"))
plot_grid(p3, labels = c("k4"))

# comparison of fabrics with kmean clusters
new_data$fabric[which(km4$cluster == 1)]
new_data$fabric[which(km4$cluster == 2)]
new_data$fabric[which(km4$cluster == 3)]
new_data$fabric[which(km4$cluster == 4)]

###########################################################################################
###########################################################################################
# Hierarchical clustering
hc_data <- new_data[,5:34]  
hc_data <- hc_data[-c(11)]   
rownames(hc_data) <- new_data$sample_id

hc_ward <- hc_data %>%
  scale() %>%                    
  dist(method = "euclidean") %>% 
  hclust(method = "ward.D2")     

hc_single <- hc_data %>%
  scale() %>%           
  dist(method = "euclidean") %>%
  hclust(method = "single")     

hc_complete <- hc_data %>%
  scale() %>%                   
  dist(method = "euclidean") %>%
  hclust(method = "complete")   

hc_median <- hc_data %>%
  scale() %>%     
  dist(method = "euclidean") %>%
  hclust(method = "median")   

hc_average <- hc_data %>%
  scale() %>%                   
  dist(method = "euclidean") %>%
  hclust(method = "average")    

hc_centroid <- hc_data %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "centroid")

# ward method
fviz_dend(hc_ward, k = 4, # Cut in four groups
          main = "Dendrogram - ward",
          cex = 0.5, # label size
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE  # color labels by groups
)

# sinlge method
fviz_dend(hc_single, k = 4,
          main = "Dendrogram - sinlge",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# complete method
fviz_dend(hc_complete, k = 4,
          main = "Dendrogram - complete",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# median method
fviz_dend(hc_median, k = 4,
          main = "Dendrogram - median",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# average method
fviz_dend(hc_average, k = 4,
          main = "Dendrogram - average",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)

# centroid method
fviz_dend(hc_centroid, k = 4,
          main = "Dendrogram - centroid",
          cex = 0.5,
          k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
          color_labels_by_k = TRUE
)
