
##---------------------------------------------------------------
##                  Loading Packages and Data                  --
##---------------------------------------------------------------


library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(GGally) # Extension to 'ggplot2'
library(knitr) # A General-Purpose Package for Dynamic Report Generation in R
library(ggcorrplot) # Visualization of a Correlation Matrix using 'ggplot2'
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(cluster) # "Finding Groups in Data": Cluster Analysis Extended Rousseeuw et al.
library(pastecs) # Package for Analysis of Space-Time Ecological Series
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(clValid) # Validation of Clustering Results
library(naniar) # Data Structures, Summaries, and Visualisations for Missing Data
library(DEGreport) # Report of DEG analysis
library(scatterplot3d) # 3D Scatter Plot
library(ggfortify) # Data Visualization Tools for Statistical Analysis Results
library(NbClust) # Determining the Best Number of Clusters in a Data Set
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics
library(magrittr) # A Forward-Pipe Operator for R
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(qgraph) # Graph Plotting Methods, Psychometric Data Visualization and Graphical Model Estimation
library(ggdendro) # Create Dendrograms and Tree Diagrams Using 'ggplot2'


# reading data
raw_lines <- readLines("data/FPL.csv") # reading data by line
raw_lines <- gsub('(^"|"$)', "", raw_lines) # removing the outer quotes and then using double quotes as quotes
raw_data <- read.csv(textConnection(raw_lines), quote = '""', header = TRUE, row.names = 1) # reading data


##---------------------------------------------------------------
##                  Exploratory Data Analysis                  --
##---------------------------------------------------------------

dim(raw_data) # dimensions: 480 x 16 matrix
str(raw_data) # structure of data
head(raw_data) # fist six rows
gg_miss_var(raw_data) # viz missing value
summary(raw_data) # summary of data
stat.desc(raw_data)

# Density Plot
raw_data %>% 
  melt(
    # ID variables - all the variables to keep but not split apart on
     id.vars=c('Team', 'Position'),
     # The source columns
     measure.vars=c(colnames(raw_data)[-c(1,2)]),
     # add name to variables and new variable
     variable.name="measurment",
     value.name="value"
) %>% # set scales free since all variables in different range
  ggplot(aes(value)) +geom_density(color = 'steelblue', fill = 'steelblue') + facet_wrap(~measurment, scales = "free") +
  labs(title = 'Density Plot of Variables') + theme(plot.title=element_text(color='black',hjust=0.5,size=12)) 


# correlation  calculation

correlationMatrix <- cor(raw_data[,-c(1,2)], use = 'complete.obs', method = 'pearson') # corr matrix
kable(correlationMatrix) # print corr matrix
correlationMatrix[correlationMatrix <= 0.7 | correlationMatrix ==1] <- ""

# Corr vizz
ggcorr(data = raw_data[,-c(1,2)], name = "corr", label = TRUE, method ='complete.obs' )+
  labs(title="Correlation Matrix of Numeric Variables")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

# Scatter Plots of The Most Correlated Variables

list(
  transmute(raw_data, x = Threat, y = Cost, dataset = 'Threat vs Cost'),
  transmute(raw_data, x = points, y = Creativity, dataset = 'Points vs Creativity'),
  transmute(raw_data, x = Minutes, y = Influence, dataset = 'Minutes vs Influence'),
  transmute(raw_data, x = Bonus, y = Influence, dataset = 'Bonus vs Influence'),
  transmute(raw_data, x = points, y = Influence, dataset = 'points vs Influence'),
  transmute(raw_data, x = Goals_scored, y = Threat, dataset = 'Goals_scored vs Threat'),
  transmute(raw_data, x = points, y = Threat, dataset = 'points vs Threat'),
  transmute(raw_data, x = Goals_conceded, y = Influence, dataset = 'Goals_conceded vs Influence'),
  transmute(raw_data, x = Minutes, y = Goals_conceded, dataset = 'Minutes vs Goals_conceded')
)%>%
  bind_rows() %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)+
  geom_cor(method = 'pearson')+ labs(title = 'Scatter Plots of The Most Correlated Variables', x = '', y = '')+
  theme(plot.title=element_text(color='black',hjust=0.5,size=12)) +
  facet_wrap(~ dataset, scales = "free")

# Scaling data

scaled_data <- scale(raw_data[,-c(1,2)], center = TRUE, scale = TRUE)
scaled_data <- data.frame(raw_data[,c(1,2)],scaled_data)
head(scaled_data)


##---------------------------------------------------------------
##                 Data Pre-Processing for PCA                 --
##---------------------------------------------------------------


scaled_df_corr_matrix <- cor(scaled_data[,-c(1,2)], method = 'pearson', use = 'complete.obs')
scaled_df_eigen <- eigen(x = scaled_df_corr_matrix)
print(scaled_df_eigen)
scaled_df_var <- scaled_df_eigen$values/sum(scaled_df_eigen$values)
print(scaled_df_var)
scaled_data_cumsum_var <- cumsum(scaled_df_var)
tibble(.rows = 1:14, scaled_df_eigen$values, scaled_df_var, scaled_data_cumsum_var) 


##----------------------------------------------------------------
##                         PCA Analysis                         --
##----------------------------------------------------------------


scaled_df_pca <- prcomp(x = scaled_data[,-c(1,2)])
print(scaled_df_pca)
get_eig(scaled_df_pca)
fviz_screeplot(scaled_df_pca, ggtheme = theme_gray())
# Extract the results for variables
var <- get_pca_var(scaled_df_pca)
# Contributions of variables to PC1
fviz_contrib(scaled_df_pca, choice = "var", axes = 1, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC2
fviz_contrib(scaled_df_pca, choice = "var", axes = 2, top = 10, ggtheme = theme_gray())
# Contributions of variables to PC3
fviz_contrib(scaled_df_pca, choice = "var", axes = 3, top = 10, ggtheme = theme_gray())
fviz_pca_var(X = scaled_df_pca, col.var = 'contrib', repel = TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), ggtheme = theme_gray()) + ggtitle("Variables - PCA")
autoplot(scaled_df_pca, data = scaled_data, colour = 'Position', loadings = TRUE, label = TRUE, label.size = 2.5,
         loadings.label = TRUE, loadings.label.size  = 4)
scatterplot3d(scaled_df_pca$x[,1:3], pch=20, color="blue") 
rotationed_df <- as.data.frame(predict(scaled_df_pca))


##----------------------------------------------------------------
##              Data Pre-Processing for Clusturing              --
##----------------------------------------------------------------

dist_m <- as.matrix(dist(scaled_data[1:50,-c(1,2)]))
dist_mi <- 1/dist_m # one over, as qgraph takes similarity matrices as input
qgraph(dist_mi, layout='spring', vsize=3)

# Choosing the right algorithm with internal measures
scaled_opt_algorithm_internal <- clValid(scaled_data[,-c(1,2)], nClust = 2:10, clMethods = c('hierarchical','kmeans','pam','clara'), validation = "internal", verbose = TRUE, method = 'ward')
summary(scaled_opt_algorithm_internal)
# Choosing the right algorithm with internal measures
scaled_opt_algorithm_stability <- clValid(scaled_data[,-c(1,2)], nClust = 2:10, clMethods = c('hierarchical','kmeans','pam','clara'), validation = "stability", verbose = TRUE, method = 'ward')
summary(scaled_opt_algorithm_stability)

# find optimal cluster

set.seed(31)
# function to compute total within-cluster sum of squares
fviz_nbclust(scaled_data[,-c(1,2)], kmeans, method = "wss", k.max = 24) + ggtitle("the Elbow Method") + theme_gray()
# Gap Statistics
fviz_nbclust(scaled_data[,-c(1,2)], kmeans, method = "gap_stat", k.max = 24) + ggtitle("Gap Statistics") + theme_gray()
# The Silhouette Method
fviz_nbclust(scaled_data[,-c(1,2)], kmeans, method = "silhouette", k.max = 24) + ggtitle("Silhouette Method") + theme_gray()
# NbCluster method

scaled_nbclust <- NbClust(scaled_data[,-c(1,2)], distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index ="all")
fviz_nbclust(scaled_nbclust) + theme_gray() + ggtitle("NbClust's optimal number of clusters")



###------------------------------------------------------------------------
###------------------------------------------------------------------------
###                                                                     ---
###                         CLUSTURING ANALYSIS                         ---
###                                                                     ---
###------------------------------------------------------------------------
###------------------------------------------------------------------------



##----------------------------------------------------------------
##                      K-Means Clusturing                      --
##----------------------------------------------------------------


k2 <- kmeans(scaled_data[,-c(1,2)], centers = 2, nstart = 25)
k3 <- kmeans(scaled_data[,-c(1,2)], centers = 3, nstart = 25)
k4 <- kmeans(scaled_data[,-c(1,2)], centers = 4, nstart = 25)
k5 <- kmeans(scaled_data[,-c(1,2)], centers = 5, nstart = 25)
k6 <- kmeans(scaled_data[,-c(1,2)], centers = 6, nstart = 25)
k7 <- kmeans(scaled_data[,-c(1,2)], centers = 7, nstart = 25)
k8 <- kmeans(scaled_data[,-c(1,2)], centers = 8, nstart = 25)
k9 <- kmeans(scaled_data[,-c(1,2)], centers = 9, nstart = 25)
k10 <- kmeans(scaled_data[,-c(1,2)], centers = 10, nstart = 25)
k11 <- kmeans(scaled_data[,-c(1,2)], centers = 11, nstart = 25)
k12 <- kmeans(scaled_data[,-c(1,2)], centers = 12, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, data = scaled_data[,-c(1,2)]) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, data = scaled_data[,-c(1,2)]) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, data = scaled_data[,-c(1,2)]) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, data = scaled_data[,-c(1,2)]) + ggtitle("k = 5")
p6 <- fviz_cluster(k6, data = scaled_data[,-c(1,2)]) + ggtitle("k = 6")
p7 <- fviz_cluster(k7, data = scaled_data[,-c(1,2)]) + ggtitle("k = 7")
p8 <- fviz_cluster(k8, data = scaled_data[,-c(1,2)]) + ggtitle("k = 8")
p9 <- fviz_cluster(k9, data = scaled_data[,-c(1,2)]) + ggtitle("k = 9")
p10 <- fviz_cluster(k10, data = scaled_data[,-c(1,2)]) + ggtitle("k = 10")
p11 <- fviz_cluster(k11, data = scaled_data[,-c(1,2)]) + ggtitle("k = 11")
p12 <- fviz_cluster(k12, data = scaled_data[,-c(1,2)]) + ggtitle("k = 12")

grid.arrange(p2, p3, p4, p5, p6, nrow = 3)
grid.arrange(p7, p8, p9, p10, p11, p12, nrow = 3)


ssc <- data.frame(kmeans = c(2,3,4,5,6,7,8,9,10,11,12),
  withinss = c(mean(k2$withinss), mean(k3$withinss),mean(k4$withinss),mean(k5$withinss), mean(k6$withinss), mean(k7$withinss), 
               mean(k8$withinss),mean(k9$withinss),mean(k10$withinss),mean(k11$withinss), mean(k12$withinss)),
  betweenss = c(k2$betweenss, k3$betweenss,k4$betweenss,k5$betweenss, k6$betweenss, k7$betweenss, k8$betweenss,
                k9$betweenss,k10$betweenss,k11$betweenss, k12$betweenss))

ssc %<>% gather(., key = "measurement", value = value, -kmeans)
ssc %>% ggplot(., aes(x=kmeans, y=log(value), fill = measurement)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Cluster Model Comparison") + xlab("Number of Clusters") + ylab("Log10 Total Sum of Squares") + 
  scale_x_discrete(name = "Number of Clusters", limits = c('0',"2", "3",'4','5', "6", "7", "8",'9','10','11', "12"))

#Final Cluster
k3 <- kmeans(scaled_data[,-c(1,2)], centers = 3, nstart = 25)
fviz_cluster(k3, data = scaled_data[,-c(1,2)]) + ggtitle("k = 3")
scaled_data[,-c(1,2)] %>% mutate(Cluster = k3$cluster) %>% group_by(Cluster) %>% summarise_all("mean") %>% kable() %>% kable_styling()

jaccard_df <- data.frame(actual = as.numeric(as.factor(scaled_data$Position)), predicted = k3$cluster)

# margin 1 for wide 2 for long format 
jaccard <- function(df, margin) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}


jaccard(jaccard_df, margin = 1)


##---------------------------------------------------------------
##                   Hierarchical Clustering                   --
##---------------------------------------------------------------

## find optimal agloritm for hierarchical clustering ##

# methods to assess

methods <- c( "average", "single", "complete", "ward")
names(methods) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(scaled_data[,-c(1,2)], method = x)$ac
}

method_result <- map_dbl(methods, ac)
method_result
method_df <- data.frame(methods, method_result)

# Barplot
ggplot(method_df, aes(x=methods, y=method_result)) + 
  geom_bar(stat = "identity", fill = 'steelblue') + labs(title = 'Cluster Methods Comparation', x = 'models', y = 'Percentage of Ac')

## find optimal cluster ##

set.seed(31)
# function to compute total within-cluster sum of squares
fviz_nbclust(scaled_data[,-c(1,2)], FUN = hcut, method = "wss", k.max = 24) + ggtitle("The Elbow Method") + theme_gray()
# Gap Statistics
fviz_nbclust(scaled_data[,-c(1,2)], FUN = hcut, method = "gap_stat", k.max = 24) + ggtitle("Gap Statistics") + theme_gray()
# The Silhouette Method
fviz_nbclust(scaled_data[,-c(1,2)], FUN = hcut, method = "silhouette", k.max = 24) + ggtitle("Silhouette Method") + theme_gray()


# Dissimilarity matrix
distance <- dist(scaled_data[,-c(1,2)], method = "euclidean")
scaled_h_clust <- hclust(distance, method = "ward.D2")
plot(scaled_h_clust, cex = 0.6)
abline(h = 5, lty = 2)
rect.hclust(scaled_h_clust, k = 5, border = 2:5)
cut_deng<- as.dendrogram(scaled_h_clust)
plot(cut(cut_deng, h = 28)$lower[[2]],
     main = "Second branch of lower tree with cut at h=28")














