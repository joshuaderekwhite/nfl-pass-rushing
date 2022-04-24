library(dplyr)
library(tidyr)
library(png)
library(stringr)
library(pROC)
library(dimRed)
library(ggplot2)
library(corrplot)
library(MASS)
select <- dplyr::select

# Games and Players Data
games <- read.csv('dataset/games.csv')
players <- read.csv('dataset/players.csv')

# Plays Data
plays <- read.csv('dataset/plays.csv') %>% 
    inner_join(games %>% select(gameId, homeTeamAbbr, visitorTeamAbbr)) %>%
    mutate(possessionInd = if_else(possessionTeam == homeTeamAbbr, 'home', 'away')) %>%
    mutate(scoreDiff = if_else(possessionInd == 'home', 
        preSnapHomeScore - preSnapVisitorScore,
        preSnapVisitorScore - preSnapHomeScore)) %>%
    mutate(minutesRemaining = if_else(quarter < 4, 4-quarter, 0)*15 + 
        as.double(substr(gameClock, 1, 2)) + 
        as.double(substr(gameClock, 4, 5))/60)
# Goal: Predict plays$numberOfPassRushers from all other pre-snap details
# Given that the offensive team intends to throw the ball

# Tracking Data
for (i in 1:17){
tracking <- rbind(
    if(exists('tracking')) tracking,
    read.csv(paste0('dataset/week',i,'.csv')) %>% 
    mutate(x = if_else(playDirection == 'right', 120 - x, x)) %>%
    mutate(dir.x = if_else(playDirection == 'right', cos(dir), cos(dir - 180))) %>%
    mutate(dir.y = if_else(playDirection == 'right', sin(dir), sin(dir - 180))) %>%
    mutate(o.x = if_else(playDirection == 'right', cos(o), cos(o - 180))) %>%
    mutate(o.y = if_else(playDirection == 'right', sin(o), sin(o - 180)))
)
}

# Football position for each play
plays.football <- plays %>% 
    inner_join(tracking, by = c('gameId', 'playId')) %>% 
    filter(team == 'football') %>%
    mutate(football.x = x, football.y = y) %>%
    select(gameId, playId, football.x, football.y, frameId)

# Validation of plays.football table
checkDupes <- function(df1, df2, by.fields = NULL){
    require(dplyr)
    ms1 <- paste("Number of Entries in", deparse(substitute(df1)), 
        "but not", deparse(substitute(df2)), ":",
        dim(anti_join(df1, df2, by = by.fields))[1])

    ms2 <- paste("Number of Entries in", deparse(substitute(df2)), 
        "but not", deparse(substitute(df1)), ":",
        dim(anti_join(df1, df2, by = by.fields))[1])

    ms3 <- paste("Number of Duplicate Entries in", deparse(substitute(df1)), ":",
        dim(df1[duplicated(df1 %>% select(by.fields)),])[1])

    ms4 <- paste("Number of Duplicate Entries in", deparse(substitute(df2)), ":",
        dim(df2[duplicated(df2 %>% select(by.fields)),])[1])

    print(noquote(c(ms1, ms2, ms3, ms4)))
}
checkDupes(plays.football, plays, c("gameId", "playId"))

# Find frameID for each snap
# Snap is when football changes direction by 0.1 yards - 
# 0.01 may be error or pre-snap incidental ball movement
snap.football <- plays.football %>%
    mutate(football.x.rounded = round(football.x, 1), 
    football.y.rounded = round(football.y, 1)) %>%
    inner_join(plays.football %>% 
        filter(frameId == 1) %>% 
        select(-frameId) %>% 
        mutate(football.x.rounded = round(football.x, 1), 
        football.y.rounded = round(football.y, 1)) %>%
        select(-c('football.x', 'football.y')), 
    by = c('gameId', 'playId', 'football.x.rounded', 'football.y.rounded')) %>%
    group_by(gameId, playId) %>%
    mutate(orderNo = max(frameId)) %>%
    ungroup() %>%
    filter(orderNo == frameId) %>%
    select(-c(orderNo, football.x.rounded, football.y.rounded))

# Sample queries for snap data
samples <- 5
snap.football.sample <- sample_n(snap.football, samples)
for (i in 1:samples){
    View(plays.football %>% filter(gameId == snap.football.sample$gameId[i],
        playId == snap.football.sample$playId[i],
        frameId >= max(snap.football.sample$frameId[i]-3,1),
        frameId <= snap.football.sample$frameId[i]+20
    ) %>%
        mutate(refframe = snap.football.sample$frameId[i]))
}

# Validation of snaps data
checkDupes(plays, snap.football, c("gameId", "playId"))

# Counts of tracking data where gameId, playId, and frameId match snap.football
tracking %>% inner_join(snap.football, by = c("gameId", "playId", "frameId")) %>%
    group_by(gameId, playId, frameId) %>%
    summarise(countOfPlayers = n()) %>%
    ungroup() %>%
    group_by(countOfPlayers) %>%
    summarise(numberOfPlays = n()) %>%
    ungroup() %>%
    select(countOfPlayers, numberOfPlays)
# Expected 23 for each (11 each side and the football)
# Not every instance accounted for

# Analyze a single play in the tracking data set
plays.analyze <- plays %>%
    inner_join(snap.football, by = c("gameId", "playId")) %>%
    inner_join(tracking, by = c("gameId", "playId", "frameId")) %>%
    filter(team != possessionInd, team != 'football') %>%
    mutate(teamStatus = 'defense') %>%
    mutate(e.dist = sqrt((x-football.x)^2 + (y-football.y)^2))
plays.random <- ceiling(runif(1,0,dim(plays.analyze)[1]))
View(plays.analyze %>% filter(playId == plays.analyze[plays.random,]$playId &
    gameId == plays.analyze[plays.random,]$gameId)) # Sample data of selected criteria

# Counts of tracking data where gameId, playId, and frameId match snap.football
plays.analyze %>% inner_join(snap.football, by = c("gameId", "playId", "frameId")) %>%
    group_by(gameId, playId, frameId) %>%
    summarise(countOfPlayers = n()) %>%
    ungroup() %>%
    group_by(countOfPlayers) %>%
    summarise(numberOfPlays = n()) %>%
    ungroup() %>%
    select(countOfPlayers, numberOfPlays)
# Most defensive plays track 7 players. Only use plays between 7 and 11 tracking
# if more than 7, only take 7 at random

# Ready the data for regression analysis
plays.regression <- plays.analyze %>%
    group_by(gameId, playId) %>%
    mutate(tracked = n()) %>%
    filter(tracked >= 7) %>%
    slice_sample(n = 7) %>%
    arrange(gameId, playId, e.dist) %>% 
    group_by(gameId, playId) %>% 
    mutate(rank = row_number(-e.dist)) %>% 
    arrange(gameId, playId, rank) %>%
    ungroup() %>%
    replace(is.na(.), 0) %>%
    select(-c(time, o:position)) %>%
    pivot_wider(names_from = rank, 
        names_sep = '.',
    values_from = c(e.dist, x, y, s, a, dis, o.x, o.y, dir.x, dir.y)) %>%
    mutate(possessionInd = ifelse(possessionInd == 'away', 0, 1)) %>%
    select(c(numberOfPassRushers, defendersInTheBox, quarter:yardsToGo, 
        possessionInd:football.y, e.dist.1:dir.y.7))

# Save the data set for re-use without the large tracking data
rm(tracking)
save.image('allPlays.RData')

# Re-run from here
load('allPlays.RData')
source("reusefunctions.r")

# Partition the data for train and test
plays.regression$Partition <- sample(
    x = c("Train", "Test"),
    size = nrow(plays.regression),
    replace = TRUE,
    prob = c(0.7, 0.3)
)
plays.regression %>% group_by(Partition, numberOfPassRushers) %>% count()
plays.train <- plays.regression %>% ungroup() %>% filter(Partition == 'Train') %>% select(-Partition)
plays.test <- plays.regression %>% ungroup() %>% filter(Partition == 'Test') %>% select(-Partition)

# Check the shape of the frequency plot
hist(plays.train$numberOfPassRushers)

# Statistics of numberOfPassRushers
mu <- mean(plays.regression$numberOfPassRushers)
mu
sig.2 <- var(plays.regression$numberOfPassRushers)
sig.2
N = max(plays.train$numberOfPassRushers)
N

# Histograms of potential models
hist(rnorm(1000,mu,sig.2))
hist(rbinom(1000,N,mu/N))

# GLM Regression Models
plays.linear <- glm(numberOfPassRushers~., data = plays.train, family = gaussian)
plays.binomial <- glm(cbind(numberOfPassRushers,N-numberOfPassRushers)~., data = plays.train, family = binomial)

# Predictions on the Test Data Set
plays.predict.linear <- predict(plays.linear, plays.test)
plays.predict.binomial <- round(N*exp(predict(plays.binomial, plays.test))/
    (1+exp(predict(plays.binomial, plays.test))))

# Mean Square Errors
plays.error.linear <- mean((plays.predict.linear - plays.test$numberOfPassRushers)^2)
plays.error.linear
plays.error.binomial <- mean((plays.predict.binomial - plays.test$numberOfPassRushers)^2)
plays.error.binomial

# Error of always predicting 4 rushers
plays.error.4 <- mean((4 - plays.test$numberOfPassRushers)^2)
plays.error.4



tryMethods(methods = list(
    name = "Random Forest", 
        method = randomForest, method.parameters = list(ntree = 1000), tune = list(mtry = 2^(0:4)),
        pred.parameters = list(type="prob")
    ),

)

testit <- function(x = sort(runif(20)), ...)
{
    pb <- txtProgressBar(...)
    for(i in c(0, x, 1)) {Sys.sleep(0.5); setTxtProgressBar(pb, i)}
    Sys.sleep(1)
    close(pb)
}
testit(runif(10))


# other methods: SVM and tree models
# Get Hyperparameters for SVM Linear and Radial
svm.linear.hyper <- gridCV(
    data = weather.train, predictor = "RainTomorrow", 
    method = svm, method.parameters = list(kernel="linear"), folds = 4,
    tune = list("cost" = 10^(-1:3))
    )
svm.radial.hyper <- gridCV(
    method = svm, method.parameters = list(kernel="radial"), folds = 4,
    data = weather.train, predictor = "RainTomorrow", 
    tune = list("gamma" = 10^(-1:3), "cost" = 10^(-1:3))
    )

# Optimized SVM Models
svm.linear <- svm(RainTomorrow~., data = weather.train, 
    kernel = "linear", cost = svm.linear.hyper$cost)
svm.radial <- svm(RainTomorrow~., data = weather.train, 
    kernel = "radial", cost = svm.radial.hyper$cost, gamma = svm.radial.hyper$gamma)