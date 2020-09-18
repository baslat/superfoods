library(tidyverse)
library(readxl)
library(plotly)
library(tm)
library(SnowballC)
library(wordcloud)
library(factoextra)
library(NbClust)

food_df <-
  read_excel("AUSNUT.xlsx", sheet = "Food Nutrient Database")

names(food_df) <- make.names(names(food_df))

# remove the categorical columns (id is 1, name is 6)

names <- food_df$Food.Name
ids <- food_df$Food.ID
df <- select(food_df, -c(1:6))

pca <- prcomp(df, center = T, scale. = T)

summary(pca)
biplot(pca)

# Variability of each principal component: pr.var
pr.var <- pca$sdev^2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)


# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


pca$x[,1:23]

fviz_nbclust(pca$x[,1:23], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(pca$x[,1:23], kmeans, nstart = 25,  method = "gap_stat", nboot = 30)+
  labs(subtitle = "Gap statistic method")




nb <- NbClust(pca$x[,1:23], distance = "euclidean", min.nc = 2,
              max.nc = 24, method = "kmeans")

fviz_nbclust(nb)





# Initialize total within sum of squares error: wss
set.seed(1)
wss <- 0

# Look over 1 to i possible clusters
for (i in 1:30) {
  # Fit the model: km.out
  km.out <- kmeans(pca$x[,1:23], centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:30, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 2

# Build model with k clusters: km.out
km.out <- kmeans(pca$x[,1:23], centers = k, nstart = 20, iter.max = 50)

# View the resulting model
table(km.out$cluster)

food_df$cluster <- as.factor(km.out$cluster)

names(food_df)
glimpse(plot_df)

nuts <- read_excel("AUSNUT_BI.xlsx", sheet = "nutrient groups")

nuts$Nutrient <- make.names(nuts$Nutrient)
glimpse(nuts)


plot_df <- food_df %>%
  gather(key = "nutrient", value = "value", 7:59)

plot_df <- left_join(plot_df, nuts, by = c("nutrient" = "Nutrient"))

energy <- ggplot(filter(plot_df, Major.group != "Miscellaneous" & Relevant == "Yes" & `Sub-category` == "Energy"), aes(x = cluster, y = value, colour = cluster)) + 
  geom_boxplot() +
  facet_wrap(~nutrient, scales = "free_y")

#energy

minerals <- ggplot(filter(plot_df, Major.group != "Miscellaneous" & Relevant == "Yes" & `Sub-category` == "Minerals"), aes(x = cluster, y = value, colour = cluster)) + 
  geom_boxplot() +
  facet_wrap(~nutrient, scales = "free_y")

minerals

vitamins <- ggplot(filter(plot_df, Major.group != "Miscellaneous" & Relevant == "Yes" & `Sub-category` == "Vitamins"), aes(x = cluster, y = value, colour = cluster)) + 
  geom_boxplot() +
  facet_wrap(~nutrient, scales = "free_y")

#vitamins

# minerals: 6, 12 and 22 look pretty good
# vitamins: 15, 12, 6, 22
# energy: 22, 14, 6


#View(unique(filter(plot_df, cluster == 15))[,"Food.Name"])


# 12 = cereals  -blue
# 22 = herbs - pink
# 6 = nuts and seeds - olive green/beige
# 15 = livers - blue
# 14 = spreads, oils, mayo etc - light blue

## make a word cloud
names(source)
source <- data.frame(filter(plot_df, cluster == 12))
class(source)
words <- Corpus(VectorSource(source$Food.Name)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) #%>%
  #tm_map(PlainTextDocument) #%>%
  #tm_map(stemDocument)


# tdm <- TermDocumentMatrix(words) %>%
#   as.matrix()
# 
# m <- as.matrix(tdm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "Blues")
pal <- pal[-(1:2)]
wordcloud(words, random.order=T, colors=pal)

