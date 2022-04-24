# Re-run from here
load('allPlays.RData')
library(buildmodels)
library(dplyr)
library(tidyr)
library(png)
library(stringr)
library(pROC)
library(dimRed)
library(ggplot2)
library(corrplot)
library(MASS)
library(e1071)
library(xgboost)
library(randomForest)
library(Rtsne)
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(ClusterR)
library(pscl)
library(caret)
library(dplyr)
library(rlang)
select <- dplyr::select
chr <- rlang::chr()

formatForIEEE <- function(init = FALSE){
    if (init) {
        update_geom_defaults("line", list(size = 4))
        update_geom_defaults("point", list(size = 10))
    } else {
    theme(
    axis.text=element_text(size=30),
    axis.title=element_text(size=40),
    legend.text=element_text(size=50),
    legend.title=element_text(size=50),
    plot.title = element_text(size=50)
    )}
}
formatForIEEE(init = TRUE)

# Partition the data for train and test
plays.regression <- plays.regression %>%
    mutate(
        numberOfPassRushers = as.factor(numberOfPassRushers),
        quarter = as.factor(quarter),
        down = as.factor(down),
        possessionInd = as.factor(possessionInd)
    ) %>% mutate_if(is.numeric, scale) %>%
    mutate(numberOfPassRushers = as.numeric(levels(numberOfPassRushers)[as.numeric(numberOfPassRushers)]))
#plays.regression <- plays.regression %>% sample_n(1000) # This is for testing purposes only. Need to revert in final run

# Diagnostics
# T-SNE Models
labels <- as.factor(plays.regression$numberOfPassRushers)
data <- plays.regression %>% select(-c(numberOfPassRushers))
tsne.sample <- sample(1:nrow(data), 1000)
tsne <- Rtsne(data, initial_dims=ncol(data))
tsne.df <- cbind.data.frame(tsne$Y[tsne.sample,1:2], 
    levels(labels)[as.numeric(labels[tsne.sample])])
colnames(tsne.df) <- c("tSNE1", "tSNE2", "numberOfPassRushers")
tsne.plot <- ggplot(tsne.df, aes(x=tSNE1, y=tSNE2)) +
    geom_point(aes(color = numberOfPassRushers), position = "jitter") +
    labs(color = "Pass\nRushers") +
    formatForIEEE()
ggsave("plots/tsne.png", tsne.plot)

# K-Means Clustering
source("silhouette_k_sweep.r")
sil_sweep_kmeans = silhouette_k_sweep(kmeans, data)
# plot loss across k (look for bend where loss begin to flatten out)
kmeans.scores <- as.data.frame(cbind(2:(length(sil_sweep_kmeans$sil_scores)+1),
    sil_sweep_kmeans$loss_scores,sil_sweep_kmeans$sil_scores))
colnames(kmeans.scores) <- c("K","loss_scores","sil_scores")
kmeans.elbow.plot <- ggplot(kmeans.scores, aes(x=K, y=loss_scores)) +
    geom_line() +
    scale_y_continuous("Loss Score", labels = function(x) format(x, scientific = TRUE)) +
    labs(title="K-Means vs. Loss Score") +
    formatForIEEE()
ggsave("plots/kmeans_elbow.png", kmeans.elbow.plot)
# plot silhouette score across k (larger is better)
kmeans.silhouette.plot <- ggplot(kmeans.scores, aes(x=K, y=sil_scores)) +
    geom_line() +
    labs(title="K-Means vs. Silhouette Score") +
    scale_y_continuous("Silhouette Score") +
    formatForIEEE()
ggsave("plots/kmeans_silhouette.png", kmeans.silhouette.plot)

# Drop improbable clusters
table(plays.regression$numberOfPassRushers)

plays.regression <- plays.regression %>% 
    filter(!numberOfPassRushers %in% c(1,2,7,8))
plays.regression$Partition <- sample(
    x = c("Train", "Test"),
    size = nrow(plays.regression),
    replace = TRUE,
    prob = c(0.7, 0.3)
) 
plays.regression %>% group_by(Partition, numberOfPassRushers) %>% 
    count()
plays.train <- plays.regression %>% ungroup() %>% 
    filter(Partition == 'Train') %>% select(-Partition)
plays.test <- plays.regression %>% ungroup() %>% 
    filter(Partition == 'Test') %>% select(-Partition)

# Check the shape of the frequency plot
labels.df <- as.data.frame(as.numeric(levels(labels)[as.numeric(labels)]))
colnames(labels.df) <- "numberOfPassRushers"
hist.all.plot <- ggplot(labels.df, aes(x=numberOfPassRushers)) +
    geom_bar(width = .5) +
    scale_x_continuous("Pass Rushers", breaks=min(labels.df):max(labels.df)) +
    formatForIEEE()
ggsave("plots/hist_all.png", hist.all.plot)
hist.subset.plot <- ggplot(plays.train, aes(x=numberOfPassRushers)) +
    geom_bar(width = .5) +
    scale_x_continuous("Pass Rushers", breaks=min(plays.train$numberOfPassRushers):max(plays.train$numberOfPassRushers)) +
    formatForIEEE()
ggsave("plots/hist_subset.png", hist.subset.plot)

# Statistics of numberOfPassRushers
mu <- mean(plays.regression$numberOfPassRushers)
mu
sig.2 <- var(plays.regression$numberOfPassRushers)
sig.2

# Extending the buildmodels functions
models <- list(
    list(name = "GLM", method = glm, 
        tune.parameters = list(family = c("gaussian", "poisson", "quasipoisson"))),
    list(name = "Hurdle", method = hurdle,
        tune.parameters = list(dist = c("poisson", "negbin", "geometric"), 
            zero.dist=c("binomial", "poisson", "negbin", "geometric"))),
    list(name = "SVM Linear", method = svm, 
        method.parameters = list(kernel="linear", probability = TRUE)),
    list(name = "SVM Gaussian", method = svm, 
        method.parameters = list(kernel="radial", probability = TRUE), 
        tune.parameters = list(gamma = 10^(-3:-1))),
    list(name = "Random Forest", method = randomForest, 
        method.parameters = list(ntree = 500)),
    list(name = "Gradient Boosted", method = xgbStandardize, 
        method.parameters = list(verbose=0, nrounds=500), 
        tune.parameters = list(eta = 10^(-4:-1)))
)
#models <- buildmodels("numberOfPassRushers", plays.train, plays.test, models, bin = list(round, digits=0))
for (model in models) {
    
    print(paste(model$name, "Accuracy", round(model$results$accuracy,2)))
    print(paste(model$name, "MSE", round(model$results$MSE,2)))
}
# Accuracy of only predicting four
print(paste("All Fours Accuracy", round(mean(plays.test$numberOfPassRushers == 4),2)))
print(paste("All Fours MSE", round(mean((plays.test$numberOfPassRushers - 4)^2),2)))
heatmap(models[[6]]$results$confusion)

# PCA for Dimension Reduction with factors treated as ordinal
plays.pca <- prcomp(plays.train %>% 
    lapply(function(x) as.numeric(as.character(x))) %>% 
    as.data.frame() %>% 
    select(-numberOfPassRushers))
plays.pca$var <- as.data.frame((plays.pca$sdev^2)/sum(plays.pca$sdev^2))
colnames(plays.pca$var) <- "Variance"
sum(plays.pca$var[1:2,])
min(which(cumsum(plays.pca$var)>0.7))
pca.var.plot <- ggplot(plays.pca$var, aes(x = as.numeric(rownames(plays.pca$var)), y=Variance)) +
    geom_point() +
    labs(x = "Principal Component") +
    formatForIEEE()
ggsave("plots/pca_var.png", pca.var.plot)
pca.pc1pc2.plot <- ggplot(data.frame(plays.pca$x), aes(x=PC1, y=PC2)) +
    geom_point(aes(color = as.character(plays.train$numberOfPassRushers)), position = "jitter") +
    labs(color = "Pass\nRushers") +
    formatForIEEE()
ggsave("plots/pca_pc1pc2.png", pca.pc1pc2.plot)
plays.train.pca <- cbind(plays.train$numberOfPassRushers, plays.pca$x) %>%
    as.data.frame() %>%
    mutate(numberOfPassRushers = V1, drop = TRUE) %>% 
    select(numberOfPassRushers, PC1, PC2)
plays.test.pca <- cbind(plays.test$numberOfPassRushers, 
    predict(plays.pca, newdata = plays.test %>% 
    lapply(function(x) as.numeric(as.character(x))) %>% 
    as.data.frame() %>% 
    select(-numberOfPassRushers))) %>%
    as.data.frame() %>%
    mutate(numberOfPassRushers = V1, drop = TRUE) %>% 
    select(numberOfPassRushers, PC1, PC2)

models2 <- buildmodels("numberOfPassRushers", plays.train.pca, plays.test.pca, models, bin = list(round, digits=0))
for (model in models2) print(paste(model$name, round(model$results$accuracy,2)))
for (model in models2) print(paste(model$name, round(model$results$MSE,2)))

# Variable Importance from the Models
xgb.model <- models[[6]]$model
class(xgb.model) <- 'xgb.Booster'
xgb.imp <- xgb.importance(model = xgb.model)
varimportance.plot <- ggplot(xgb.imp %>% arrange(-Gain) %>% top_n(10), 
    aes(x = Gain, y = reorder(Feature,Gain))) +
    geom_col() +
    labs(y = "Feature") +
    formatForIEEE()
ggsave("plots/varimportance.png", varimportance.plot)
corrplot.plot <- plot(plays.train %>% 
    select(numberOfPassRushers, minutesRemaining, defendersInTheBox, e.dist.7, e.dist.4))
ggsave("plots/corrplot.png", corrplot.plot)

# Time single iteration
library(pracma)
tic()
pred.model <- models[[6]]$model
predict(pred.model, newdata=plays.test[round(runif(2,1,1000)),])
toc()

# Old
# qplot(tsne$Y[tsne.sample,1], tsne$Y[tsne.sample,2], 
#     colour=col_vector[as.numeric(labels[tsne.sample])],
#     size=I(15)) +
#     scale_color_manual(labels = unique(labels[tsne.sample]), 
#     values = col_vector[1:length(levels(labels[tsne.sample]))]) + 
#     labs(colour="Pass Rushers") +
#     xlab("t-SNE 1") + ylab("t-SNE 2") +
#     formatForIEEE()

# plot(2:(length(sil_sweep_kmeans$sil_scores)+1), sil_sweep_kmeans$loss_scores, 
#     main="K-means Loss scores across K", xlab="K", ylab="loss score", type='l')
# plot(2:(length(sil_sweep_kmeans$sil_scores)+1), sil_sweep_kmeans$sil_scores, 
#     main="K-means Silhouette scores across K", xlab="K", ylab="sil. score", type='l')

# # Sparse PCA - Not used
# sparse.pca.train <- spca(plays.train %>% 
#     lapply(function(x) as.numeric(as.character(x))) %>% 
#     as.data.frame() %>% 
#     select(-numberOfPassRushers), K = 2, type = "predictor", sparse = "varnum", para = c(4, 4))

# kmeans.re <- kmeans(data, centers = 8, nstart = 20)
# kmeans.re
# kmeans.re$cluster
# table(labels, kmeans.re$cluster)
# kmeans.re$centers

# hist(as.numeric(labels)+1)

# hist(plays.train$numberOfPassRushers)

# varImpPlot(models[[5]]$model)