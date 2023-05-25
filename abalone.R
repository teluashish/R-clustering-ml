remove.packages("ggplot2")
remove.packages("rlang")
install.packages(c("sna", "psych","corrplot","factoextra","ggplot2","rlang","class","gmodels","pacman"))
library(gmodels)
library(class)
library(rlang)
library(sna)
library(psych)
library(corrplot)
library(ggplot2)
library(factoextra)
library(pacman)

abalone <- read.table("/abalone.data", sep=",", stringsAsFactors = FALSE)
abalone.names <- c("Sex","Length","Diameter","Height","Whole weight","Shucked weight","Viscera weight","Shell weight","Rings")
names(abalone) <- abalone.names
head(abalone)
str(abalone)

# Create the box plot
box_plot <- ggplot(abalone, aes(x = Sex, y = Rings)) +
  geom_boxplot() +
  labs(title = "Box Plot of Rings by Sex", x = "Sex", y = "Rings") +
  theme_minimal()

# Display the box plot
plot(box_plot)

summary(abalone)
describe(abalone)

# Converting the categorical attribute to numeric
before_mapping = abalone$Sex[1:10]
sex_mapping <- c("M" = 1, "F" = 2, "I" = 3)
abalone$Sex <- sex_mapping[abalone$Sex]
before_mapping
abalone$Sex[1:10]


abalonecor <- cor(abalone)
abalonecor
corrplot(abalonecor)

pairs(abalone)
plot(abalone)

abalone.na = na.omit(abalone)
str(abalone.na)

#Removing "Sex" Attribute
abalone.na2 = abalone.na[, -1]
head(abalone.na2)

normalize <- function(x) {((x-min(x))/(max(x)-min(x)))}
abalone.norm <- as.data.frame(lapply(abalone.na2, normalize))
abalone.norm[1:10,]
abalone.na2.min.Rings <- min(abalone.na2$Rings)
abalone.na2.min.Rings
abalone.na2.max.Rings <- max(abalone.na2$Rings)
abalone.na2.max.Rings
ring1 <- 0.5000000*(abalone.na2.max.Rings - abalone.na2.min.Rings) + abalone.na2.min.Rings
ring1

zscore <- function(x){(x-mean(x))/sd(x)}
abalone.znorm <- as.data.frame(lapply(abalone.na2,scale))
abalone.znorm[1:10,]
ring2 <- 1.57135544* sd(abalone.na2$Rings) + mean(abalone.na2$Rings)
ring2 



# K-means Clustering

abalone.k2 <- kmeans(abalone.norm, centers = 2,nstart = 2,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k2
facto <- fviz_cluster(abalone.k2,abalone.norm)
facto

abalone.k3 <- kmeans(abalone.norm, centers = 3, nstart = 3,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k3
facto <- fviz_cluster(abalone.k3,abalone.norm)
facto

abalone.k4 <- kmeans(abalone.norm, centers = 4, nstart = 4,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k4
facto <- fviz_cluster(abalone.k4,abalone.norm)
facto

abalone.k5 <- kmeans(abalone.norm, centers = 5, nstart = 5,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k5
facto <- fviz_cluster(abalone.k5,abalone.norm)
facto

abalone.k6 <- kmeans(abalone.norm, centers = 6, nstart = 6,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k6
facto <- fviz_cluster(abalone.k6,abalone.norm)
facto

abalone.k7 <- kmeans(abalone.norm, centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k7
facto <- fviz_cluster(abalone.k7,abalone.norm)
facto

abalone.k8 <- kmeans(abalone.norm, centers = 8, nstart = 8,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k8
facto <- fviz_cluster(abalone.k8,abalone.norm)
facto

abalone.k9 <- kmeans(abalone.norm, centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k9
facto <- fviz_cluster(abalone.k9,abalone.norm)
facto

abalone.k10 <- kmeans(abalone.norm, centers = 10,nstart = 10,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k10
facto <- fviz_cluster(abalone.k10,abalone.norm)
facto

abalone.k11 <- kmeans(abalone.norm, centers = 11,nstart = 11,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.k11
facto <- fviz_cluster(abalone.k11,abalone.norm)
facto


wssplot <- function(data, nc = 15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
}

wssplot(abalone.na2,nc=4,seed=1234)

factoextra::fviz_nbclust(abalone.norm, FUNcluster = kmeans, method = "wss", k.max = 20, verbose = TRUE)


# KNN Algorithm, with Train-Test Split of (70%, 30%) ------------------------------------------------------------------------------------------------------------------
abalone.norm.rows = nrow(abalone.norm)
abalone.norm.sample = 0.7
abalone.rows = abalone.norm.sample * abalone.norm.rows
abalone.rows

abalone.train.index = sample(abalone.norm.rows, abalone.rows)
length(abalone.train.index)

abalone.train = abalone.norm[abalone.train.index,]
abalone.train[1:20,]

abalone.test = abalone.norm[-abalone.train.index,]
abalone.test[1:20,]

# KNN with k = 9
abalone.train.k9 = kmeans(abalone.train, centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
head(abalone.train.k9)

abalone.test.k9 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 9)
head(abalone.test.k9)

abalone.test.kmeans.k9 = kmeans(abalone.test, centers = 9)
abalone.test.kmeans.k9

abalone.test.k9.labels = abalone.test.kmeans.k9$cluster
length(abalone.test.k9.labels)
head(abalone.test.k9.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
head(abalone.test.pred)
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k9 = kmeans(abalone.test.pred, centers = 9)
abalone.test.pred.k9

abalone.test.pred.k9.CT = CrossTable(abalone.test.pred.k9$cluster, abalone.test.kmeans.k9$cluster, prop.chisq = TRUE)
abalone.test.pred.k9.CT

dim(abalone.test.pred.k9)
length(abalone.test.pred.k9$cluster)

confusion_matrix <- abalone.test.pred.k9.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 7
abalone.train.k7 = kmeans(abalone.train, centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k7

abalone.test.k7 = knn(abalone.train, abalone.test, abalone.train.k7$cluster, k = 7)
abalone.test.k7

abalone.test.kmeans.k7 = kmeans(abalone.test, centers = 7)
abalone.test.kmeans.k7

abalone.test.k7.labels = abalone.test.kmeans.k7$cluster
length(abalone.test.k7.labels)
head(abalone.test.k7.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k7 = kmeans(abalone.test.pred, centers = 7)
head(abalone.test.pred.k7)

abalone.test.pred.k7.CT = CrossTable(abalone.test.pred.k7$cluster, abalone.test.kmeans.k7$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k7)
length(abalone.test.pred.k7$cluster)

confusion_matrix <- abalone.test.pred.k7.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 5
abalone.train.k5 = kmeans(abalone.train, centers = 5, centers = 5 ,nstart = 5 ,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k5

abalone.test.k5 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 5)
abalone.test.k5

abalone.test.kmeans.k5 = kmeans(abalone.test, centers = 5)
abalone.test.kmeans.k5

abalone.test.k5.labels = abalone.test.kmeans.k5$cluster
length(abalone.test.k5.labels)
abalone.test.k5.labels

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k5 = kmeans(abalone.test.pred, centers = 5)
abalone.test.pred.k5

abalone.test.pred.k5.CT = CrossTable(abalone.test.pred.k5$cluster, abalone.test.kmeans.k5$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k5)
length(abalone.test.pred.k5$cluster)

confusion_matrix <- abalone.test.pred.k5.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN Algorithm, with Train-Test Split of (60%, 40%) ------------------------------------------------------------------------------------------------------------------
abalone.norm.rows = nrow(abalone.norm)
abalone.norm.sample = 0.6
abalone.rows = abalone.norm.sample * abalone.norm.rows
abalone.rows
abalone.train.index = sample(abalone.norm.rows, abalone.rows)

abalone.train = abalone.norm[abalone.train.index,]
abalone.train[1:5,]

abalone.test = abalone.norm[-abalone.train.index,]
abalone.test[1:5,]

# KNN with k = 9
abalone.train.k9 = kmeans(abalone.train, centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
head(abalone.train.k9)

abalone.test.k9 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 9)
head(abalone.test.k9)

abalone.test.kmeans.k9 = kmeans(abalone.test, centers = 9)
abalone.test.kmeans.k9

abalone.test.k9.labels = abalone.test.kmeans.k9$cluster
length(abalone.test.k9.labels)
head(abalone.test.k9.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
head(abalone.test.pred)
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k9 = kmeans(abalone.test.pred, centers = 9)
abalone.test.pred.k9

abalone.test.pred.k9.CT = CrossTable(abalone.test.pred.k9$cluster, abalone.test.kmeans.k9$cluster, prop.chisq = TRUE)
abalone.test.pred.k9.CT

dim(abalone.test.pred.k9)
length(abalone.test.pred.k9$cluster)

confusion_matrix <- abalone.test.pred.k9.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 7
abalone.train.k7 = kmeans(abalone.train, centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k7

abalone.test.k7 = knn(abalone.train, abalone.test, abalone.train.k7$cluster, k = 7)
abalone.test.k7

abalone.test.kmeans.k7 = kmeans(abalone.test, centers = 7)
abalone.test.kmeans.k7

abalone.test.k7.labels = abalone.test.kmeans.k7$cluster
length(abalone.test.k7.labels)
head(abalone.test.k7.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k7 = kmeans(abalone.test.pred, centers = 7)
head(abalone.test.pred.k7)

abalone.test.pred.k7.CT = CrossTable(abalone.test.pred.k7$cluster, abalone.test.kmeans.k7$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k7)
length(abalone.test.pred.k7$cluster)

confusion_matrix <- abalone.test.pred.k7.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 5
abalone.train.k5 = kmeans(abalone.train, centers = 5 ,nstart = 5 ,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k5

abalone.test.k5 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 5)
abalone.test.k5

abalone.test.kmeans.k5 = kmeans(abalone.test, centers = 5)
abalone.test.kmeans.k5

abalone.test.k5.labels = abalone.test.kmeans.k5$cluster
length(abalone.test.k5.labels)
abalone.test.k5.labels

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k5 = kmeans(abalone.test.pred, centers = 5)
abalone.test.pred.k5

abalone.test.pred.k5.CT = CrossTable(abalone.test.pred.k5$cluster, abalone.test.kmeans.k5$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k5)
length(abalone.test.pred.k5$cluster)

confusion_matrix <- abalone.test.pred.k5.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN Algorithm, with Train-Test Split of (50%, 50%) ------------------------------------------------------------------------------------------------------------------
abalone.norm.rows = nrow(abalone.norm)
abalone.norm.sample = 0.5
abalone.rows = abalone.norm.sample * abalone.norm.rows
abalone.rows

abalone.train.index = sample(abalone.norm.rows, abalone.rows)
length(abalone.train.index)

abalone.train = abalone.norm[abalone.train.index,]
abalone.train[1:5,]

abalone.test = abalone.norm[-abalone.train.index,]
abalone.test[1:5,]

# KNN with k = 9
abalone.train.k9 = kmeans(abalone.train, centers = 9,nstart = 9,algorithm = c("Hartigan-Wong"),trace = FALSE)
head(abalone.train.k9)

abalone.test.k9 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 9)
head(abalone.test.k9)

abalone.test.kmeans.k9 = kmeans(abalone.test, centers = 9)
abalone.test.kmeans.k9

abalone.test.k9.labels = abalone.test.kmeans.k9$cluster
length(abalone.test.k9.labels)
head(abalone.test.k9.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
head(abalone.test.pred)
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k9 = kmeans(abalone.test.pred, centers = 9)
abalone.test.pred.k9

abalone.test.pred.k9.CT = CrossTable(abalone.test.pred.k9$cluster, abalone.test.kmeans.k9$cluster, prop.chisq = TRUE)
abalone.test.pred.k9.CT

dim(abalone.test.pred.k9)
length(abalone.test.pred.k9$cluster)

confusion_matrix <- abalone.test.pred.k9.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 7
abalone.train.k7 = kmeans(abalone.train, centers = 7,nstart = 7,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k7

abalone.test.k7 = knn(abalone.train, abalone.test, abalone.train.k7$cluster, k = 7)
abalone.test.k7

abalone.test.kmeans.k7 = kmeans(abalone.test, centers = 7)
abalone.test.kmeans.k7

abalone.test.k7.labels = abalone.test.kmeans.k7$cluster
length(abalone.test.k7.labels)
head(abalone.test.k7.labels)

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k7 = kmeans(abalone.test.pred, centers = 7)
head(abalone.test.pred.k7)

abalone.test.pred.k7.CT = CrossTable(abalone.test.pred.k7$cluster, abalone.test.kmeans.k7$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k7)
length(abalone.test.pred.k7$cluster)

confusion_matrix <- abalone.test.pred.k7.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure

# KNN with k = 5
abalone.train.k5 = kmeans(abalone.train, centers = 5, nstart = 5 ,algorithm = c("Hartigan-Wong"),trace = FALSE)
abalone.train.k5

abalone.test.k5 = knn(abalone.train, abalone.test, abalone.train.k9$cluster, k = 5)
abalone.test.k5

abalone.test.kmeans.k5 = kmeans(abalone.test, centers = 5)
abalone.test.kmeans.k5

abalone.test.k5.labels = abalone.test.kmeans.k5$cluster
length(abalone.test.k5.labels)
abalone.test.k5.labels

# Linear Modeling
abalone.train.glm = glm(formula = Rings~Length + Diameter + Height + Whole.weight + Shucked.weight + Viscera.weight + Shell.weight, family = gaussian, data = abalone.train)
summary(abalone.train.glm)

abalone.train.glm.anova = anova(abalone.train.glm, test = "Chisq")
abalone.train.glm.anova

plot(abalone.train.glm)

dim(abalone.train)
dim(abalone.test)
abalone.test.pred = predict(abalone.train.glm, newdata = abalone.test)
abalone.test.pred
length(abalone.test.pred)

summary(abalone.test.pred)

# Confidence Intervals
confint(abalone.train.glm)

# Comparing Actual vs Prediction
abalone.test.pred.k5 = kmeans(abalone.test.pred, centers = 5)
abalone.test.pred.k5

abalone.test.pred.k5.CT = CrossTable(abalone.test.pred.k5$cluster, abalone.test.kmeans.k5$cluster, prop.chisq = TRUE)
dim(abalone.test.pred.k5)
length(abalone.test.pred.k5$cluster)

confusion_matrix <- abalone.test.pred.k5.CT$t
confusion_matrix

n_classes <- nrow(confusion_matrix)
n_classes
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
precision
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
recall
specificity <- (sum(diag(confusion_matrix)) - diag(confusion_matrix)) / (sum(confusion_matrix) - rowSums(confusion_matrix))
specificity
error <- 1 - accuracy
error
f_measure <- 2 * precision * recall / (precision + recall)
f_measure
