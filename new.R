library(tidyverse)
library(cluster)
library(factoextra)

df <- agriculture
head(df)
sum(is.na(df))
dfnorm <- scale(df)
get_distance <- get_dist(dfnorm, method ="euclidean")
fviz_dist(get_distance, gradient = list(low= "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(dfnorm, centers = 2, nstart = 25)
str(k2)
k2
fviz_cluster(k2, data = dfnorm)

dfnorm %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(agriculture)) %>%
  ggplot(aes(x, y, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(dfnorm, centers = 3, nstart = 25)
k4 <- kmeans(dfnorm, centers = 4, nstart = 25)
k5 <- kmeans(dfnorm, centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom ="point", data = dfnorm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom ="point", data = dfnorm) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom ="point", data = dfnorm) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom ="point", data = dfnorm) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1,p2,p3,p4, nrow = 2)

#menentukan nilai optimal kluster dengan menggunakan metode elbow dan silhouette
#Elbow 
fviz_nbclust(dfnorm, kmeans, method = "wss")

#sillhoute
fviz_nbclust(dfnorm, kmeans, method = "silhouette")

set.seed(123)
final <- kmeans(dfnorm, 3, nstart = 25)
print(final)
fviz_cluster(final, data = dfnorm)
