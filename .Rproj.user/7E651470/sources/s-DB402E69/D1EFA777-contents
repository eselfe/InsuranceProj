
# Assignment 1
# Moneyball

install.packages(ggplot2)
install.packages("magrittr")
library(magrittr)
library(ggplot2)


mean(data$TEAM_BATTING_HR)



summary(data$TEAM_BATTING_HR)
summary(data$TEAM_BATTING_H)


hist(data$TEAM_BATTING_HR)

summary(data$TEAM_PITCHING_SO)
missmap(data)
summary(data$TEAM_PITCHING_SO)



ggplot(data = data) + 
  geom_point(mapping = aes(x = TARGET_WINS, y = TEAM_PITCHING_SO), color = "blue") + 
  ylim(0,7500)


ggplot(data = data) + 
  geom_bar(mapping = aes(x = TARGET_WINS))  # INCLUDED

ggplot(data = data) + 
  geom_bar(mapping = aes(x = TEAM_BATTING_HR))


hist(data$TARGET_WINS)
hist(data$TEAM_FIELDING_DP)

cor(data$TARGET_WINS, data$TEAM_BATTING_H)


pairs.panels(data[,2:17], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


data = data %>%
    rename(
      Index = INDEX,
      numWins = TARGET_WINS,
      baseHits = TEAM_BATTING_H,
      doubles = TEAM_BATTING_2B,
      triples = TEAM_BATTING_3B,
      homeruns = TEAM_BATTING_HR,
      walksTaken = TEAM_BATTING_BB,
      freeBase = TEAM_BATTING_HBP,
      strikeOuts = TEAM_BATTING_SO,
      stolenBases = TEAM_BASERUN_SB,
      caughtStealing = TEAM_BASERUN_CS,
      errors = TEAM_FIELDING_E,
      doublePlays = TEAM_FIELDING_DP,
      walksAllowed = TEAM_PITCHING_BB,
      hitsAllowed = TEAM_PITCHING_H,
      homerunsAllowed = TEAM_PITCHING_HR,
      strikeOuts_byPitchers = TEAM_PITCHING_SO
    )


dataNames = colnames(data)
posCor = matrix(nrow = 6, ncol = 4)
negCor = matrix(nrow = 4, ncol = 4)

i = 1
r = 1
cH = 1    # count Hold
rowC = 0
count = 0
countTotal = 0
for (i in 2:(ncol(data)-1)){
  for (r in (i+1):ncol(data)){
    if(as.double(cor(data[,i], data[,r], method = "pearson", use = "complete.obs")) > 0.5){
      count = count + 1
      countTotal = countTotal + 1
      
      if(count == 1){
        rowC = rowC + 1
        posCor[rowC,count] = dataNames[i]
      }
      posCor[rowC,count+1] = dataNames[r]
    }
    r = r + 1
  }
  if(cH <= count){
    cH = cH + 1
  }
  count = 0
  i = i + 1
}

i = 1
r = 1
cH = 1    # count Hold
rowC = 0
countTotal = 0
for (i in 2:(ncol(data)-1)){
  for (r in (i+1):ncol(data)){
    if(as.double(cor(data[,i], data[,r], method = "pearson", use = "complete.obs")) < -0.5){
      count = count + 1
      countTotal = countTotal + 1
      
      if(count == 1){
        rowC = rowC + 1
        negCor[rowC,count] = dataNames[i]
      }
      negCor[rowC,count+1] = dataNames[r]
    }
    r = r + 1
  }
  if(cH <= count){
    cH = cH + 1
  }
  count = 0
  i = i + 1
}


# Positive Correlation
pos1 <- ggplot(data = data, mapping = aes(x = baseHits, y = doubles)) + 
  geom_point(color = "blue4") + 
  geom_smooth(color = "green1")

pos2 <- ggplot(data = data, mapping = aes(x = triples, y = stolenBases)) + 
  geom_point(color = "steelblue") + 
  geom_smooth(color = "purple4")
pos3 <- ggplot(data = data, mapping = aes(x = triples, y = errors)) + 
  geom_point(color = "steelblue") + 
  geom_smooth(color = "blue1")

pos4 <- ggplot(data = data, mapping = aes(x = homeruns, y = walksTaken)) + 
  geom_point(color = "red1") + 
  geom_smooth(color = "magenta1")
pos5 <- ggplot(data = data, mapping = aes(x = homeruns, y = strikeOuts)) + 
  geom_point(color = "red1") + 
  geom_smooth(color = "orange1")
pos6 <- ggplot(data = data, mapping = aes(x = homeruns, y = homerunsAllowed)) + 
  geom_point(color = "red1") + 
  geom_smooth(color = "yellow2")

pos7 <- ggplot(data = data, mapping = aes(x = strikeOuts, y = homerunsAllowed)) + 
  geom_point(color = "orange1") + 
  geom_smooth(color = "yellow2")

pos8 <- ggplot(data = data, mapping = aes(x = stolenBases, y = caughtStealing)) + 
  geom_point(color = "purple4") + 
  geom_smooth(color = "cyan1")
pos9 <- ggplot(data = data, mapping = aes(x = stolenBases, y = errors)) + 
  geom_point(color = "purple4") + 
  geom_smooth(color = "blue1")

pos10 <- ggplot(data = data, mapping = aes(x = hitsAllowed, y = errors)) + 
  geom_point(color = "aquamarine4") + 
  geom_smooth(color = "blue1")

grid.arrange(pos1, pos4, pos5, pos6, pos7,
             pos2, pos3, pos8, pos9, pos10,  nrow=2)

grid.arrange(pos1, pos4, 
             pos5, pos6, 
             pos3, pos2,
             pos9, pos8, 
             pos7, pos10,  nrow=5)

grid.arrange(pos1, pos2, 
             pos3, pos4, 
             pos5, pos6,
             pos7, pos8, 
             pos9, pos10,  nrow=5)



# Negative Correlation
neg1 <- ggplot(data = data, mapping = aes(x = triples, y = homeruns)) + 
  geom_point(color = "grey3") + 
  geom_smooth(color = "springgreen1")
neg2 <- ggplot(data = data, mapping = aes(x = triples, y = strikeOuts)) + 
  geom_point(color = "grey3") + 
  geom_smooth(color = "springgreen1")
neg3 <- ggplot(data = data, mapping = aes(x = triples, y = homerunsAllowed)) + 
  geom_point(color = "grey3") + 
  geom_smooth(color = "springgreen1")

neg4 <- ggplot(data = data, mapping = aes(x = homeruns, y = errors)) + 
  geom_point(color = "darkgreen") + 
  geom_smooth(color = "springgreen1")

neg5 <- ggplot(data = data, mapping = aes(x = walksTaken, y = errors)) + 
  geom_point(color = "skyblue3") + 
  geom_smooth(color = "springgreen1")

neg6 <- ggplot(data = data, mapping = aes(x = strikeOuts, y = errors)) + 
  geom_point(color = "firebrick2") + 
  geom_smooth(color = "springgreen1")

grid.arrange(neg1, neg4, neg2, neg5, neg3, neg6, nrow = 3)

# --- below --- bottomr page 2 plot

test1 <- ggplot(data, aes(x = numWins)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightgreen")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Number of Wins")
test2 <- ggplot(data, aes(x = baseHits)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="blue4")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Base Hits by Batters")
test3 <- ggplot(data, aes(x = doubles)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="green1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Doubles by Batters")
test4 <- ggplot(data, aes(x = triples)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="steelblue")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Triples by Batters")
test5 <- ggplot(data, aes(x = homeruns)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="red1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Homeruns by Batters")
test6 <- ggplot(data, aes(x = walksTaken)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="magenta1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Walks by Batters")
test7 <- ggplot(data, aes(x = strikeOuts)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="orange1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Strikeouts by Batters")
test8 <- ggplot(data, aes(x = stolenBases)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="purple4")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Stolen Bases")
test9 <- ggplot(data, aes(x = caughtStealing)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="cyan1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Caught Stealing")
test10 <- ggplot(data, aes(x = freeBase)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey44")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Batters Hit by Pitch")
test11 <- ggplot(data, aes(x = hitsAllowed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="aquamarine4")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Hits Allowed")
test12 <- ggplot(data, aes(x = homerunsAllowed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="yellow2")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Homeruns Allowed")
test13 <- ggplot(data, aes(x = walksAllowed)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey44")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Walks Allowed")
test14 <- ggplot(data, aes(x = strikeOuts_byPitchers)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey44")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Strikeouts by Pitcher")
test15 <- ggplot(data, aes(x = errors)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="blue1")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Errors")
test16 <- ggplot(data, aes(x = doublePlays)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="grey44")+
  geom_density(alpha=.2, fill="grey82") +
  ggtitle("Double Plays")

grid.arrange(test1, test2, test3, test4, 
             test5, test6, test7, test8, 
             test9, test10, test11, test12,
             test13, test14, test15, test16, 
             nrow=4, ncol = 4)


# ---


ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = triples))


batterData <- data[, c(2, 3, 4, 5, 6, 7, 8)]
pitcherData <- data[, c(2, 12, 13, 14, 15)]

gameData <- data[, c(2, 9, 10, 11, 16, 17)]


ggplot(data = batterData) + 
  geom_boxplot(mapping = aes(x = numWins)) + 
  geom_boxplot(mapping = aes(x = baseHits))

ggplot(data = batterData) + 
  geom_boxplot(mapping = aes(x = data[1,]))

#



ggplot(data = batterData) + 
  geom_boxplot(mapping = aes(x = numWins, y = baseHits))




ggplot(data = batterData) + 
  geom_point(mapping = aes(x = numWins, y = c(baseHits, doubles, triples)))

ggplot(data = batterData) + 
  geom_point(mapping = aes(x = numWins, y = doubles))


# --- below --- page 5 plot

batterPlot <- ggplot(batterData, aes(numWins, y = value, color = Variables)) + 
      geom_point(aes(y = baseHits, col = "Base Hits")) + 
      geom_point(aes(y = doubles, col = "Doubles")) + 
      geom_point(aes(y = triples, col = "Triples")) + 
      geom_point(aes(y = homeruns, col = "Home Runs")) + 
      geom_point(aes(y = walksTaken, col = "Walks Taken")) + 
      geom_point(aes(y = strikeOuts, col = "Strike Outs")) +
      scale_color_manual(values = c("Base Hits" = "blue4",
                                    "Doubles" = "green1",
                                    "Triples" = "steelblue",
                                    "Home Runs" = "red1",
                                    "Walks Taken" = "magenta1",
                                    "Strike Outs" = "orange1")) + 
      ggtitle("Batting Variables")

baseHits <- ggplot(data = data, x = numWins) + 
  geom_boxplot(mapping = aes(y = baseHits), color = "blue4") +
  ylim(891,2500)
  
pitcherPlot <- ggplot(pitcherData, aes(numWins, y = value, color = Variables)) + 
      geom_point(aes(y = hitsAllowed, col = "Hits Allowed")) + 
      geom_point(aes(y = homerunsAllowed, col = "Home Runs Allowed")) + 
      geom_point(aes(y = walksAllowed, col = "Walks Allowed")) + 
      geom_point(aes(y = strikeOuts_byPitchers, col = "Strike Outs")) +
      scale_color_manual(values = c("Hits Allowed" = "aquamarine4",
                                    "Home Runs Allowed" = "yellow2",
                                    "Walks Allowed" = "grey44",
                                    "Strike Outs" = "sienna")) + 
      ggtitle("Pitching Variables")

hitsAllowed <- ggplot(data = data, x = numWins) + 
  geom_boxplot(mapping = aes(y = hitsAllowed), color = "aquamarine4") +
  ylim(1137,2000)


gamePlot <- ggplot(gameData, aes(numWins, y = value, color = Variables)) + 
      geom_point(aes(y = stolenBases, col = "Stolen Bases")) +
      geom_point(aes(y = caughtStealing, col = "Caught Stealing")) +
      geom_point(aes(y = freeBase, col = "Free Base Given")) +
      geom_point(aes(y = errors, col = "Errors")) +
      geom_point(aes(y = doublePlays, col = "Double Plays")) +
      scale_color_manual(values = c("Stolen Bases" = "purple4",
                                    "Caught Stealing" = "cyan1",
                                    "Free Base Given" = "grey6",
                                    "Errors" = "blue1",
                                    "Double Plays" = "grey44")) + 
      ggtitle("Game Variables")

errors <- ggplot(data = data, x = numWins) + 
  geom_boxplot(mapping = aes(y = errors), color = "blue1") +
  ylim(65,300)


grid.arrange(batterPlot, baseHits, pitcherPlot, hitsAllowed, gamePlot, errors, nrow = 3, widths = c(2,1))


# --- 

# DATA PREPARATION

mdlData <- data[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 16, 17)]

mdlData$caughtStealing[is.na(mdlData$caughtStealing)] <- mean(mdlData$caughtStealing, na.rm = TRUE)
mdlData$doublePlays[is.na(mdlData$doublePlays)] <- mean(mdlData$doublePlays, na.rm = TRUE)
mdlData$stolenBases[is.na(mdlData$stolenBases)] <- mean(mdlData$stolenBases, na.rm = TRUE)
mdlData$strikeOuts[is.na(mdlData$strikeOuts)] <- median(mdlData$strikeOuts, na.rm = TRUE)


data_noNA <- data[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
data_noNA <- na.omit(data_noNA)

data_noNA$caughtSteal_prob <- data_noNA$caughtStealing / data_noNA$stolenBases



# Change between batting v fielding
data_noNA$bat_field <- data_noNA$strikeOuts / 3
# Average hits per inning
data_noNA$hits_inning <-  data_noNA$totHits / mdlData$bat_field 
data_noNA$totHits <- data_noNA$baseHits + data_noNA$doubles + data_noNA$triples + data_noNA$homeruns
data_noNA$sacFly <- ((data_noNA$strikeOuts / data_noNA$triples))
data_noNA$bat_field <- data_noNA$strikeOuts / 3


data_noNA$OBP <- ((data_noNA$totHits + data_noNA$walksTaken + data_noNA$freeBase) 
                  / (data_noNA$bat_field + data_noNA$walksTaken + data_noNA$freeBase + data_noNA$sacFly))

data_noNA$SLG <- (((data_noNA$baseHits) + (2 * data_noNA$doubles) + (3 * data_noNA$triples) + (4 * data_noNA$homeruns)) / 
                    (data_noNA$bat_field * 1.7))

mylm <- lm(numWins ~ OBP + SLG + totHits + errors, data = data_noNA)
summary(mylm)





# Exploring top and bottom 10% ----

noNA_top10 <- data_noNA %>% filter(data_noNA$numWins > as.integer(quantile(data_noNA$numWins, prob=.90)))
noNA_bottom10 <- data_noNA %>% filter(data_noNA$numWins < as.integer(quantile(data_noNA$numWins, prob=.10)))


# Hits Taken v Hits Allowed
noNA_top10$hits_hitsAllowed <- noNA_top10$baseHits / noNA_top10$hitsAllowed
noNA_bottom10$hits_hitsAllowed <- noNA_bottom10$baseHits / noNA_bottom10$hitsAllowed

# Home Run Frequency (number of hits per home run)
noNA_top10$hr_hits <- noNA_top10$baseHits / noNA_top10$homeruns
noNA_bottom10$hr_hits <- noNA_bottom10$baseHits / noNA_bottom10$homeruns

# Hits Allowed to Errors (number of hits allowed before error)
noNA_top10$hitsAllowed_errors <- noNA_top10$hitsAllowed / noNA_top10$errors
noNA_bottom10$hitsAllowed_errors <- noNA_bottom10$hitsAllowed / noNA_bottom10$errors

# Loaded Bases ---
# Walks Taken v Walks Allowed - virtually same as Hits v Hits Allowed
noNA_top10$walks_walksT <- noNA_top10$walksTaken / noNA_top10$walksAllowed
noNA_bottom10$walks_walksT <- noNA_bottom10$walksTaken / noNA_bottom10$walksAllowed

# Hits per inning ---
# Total Hits
noNA_top10$totHits <- noNA_top10$baseHits + noNA_top10$doubles + noNA_top10$triples + noNA_top10$homeruns
noNA_bottom10$totHits <- noNA_bottom10$baseHits + noNA_bottom10$doubles + noNA_bottom10$triples + noNA_bottom10$homeruns

# Change between hitting and fielding
noNA_top10$bat_field <- noNA_top10$strikeOuts / 3
noNA_bottom10$bat_field <- noNA_bottom10$strikeOuts / 3

# Average hits per inning
noNA_top10$hits_inning <- noNA_top10$totHits / noNA_top10$bat_field
noNA_bottom10$hits_inning <- noNA_bottom10$totHits / noNA_bottom10$bat_field

# Strikeout v Home Runs

# misc ----

test <- mdlData %>% filter(mdlData[,7] == 0)

# --- 
topOff <- noNA_top10[,c(1,2,3,4,5,12)]
topOff$hits_dbl <- topOff$baseHits / topOff$doubles

botOff <- noNA_bottom10[,c(1,2,3,4,5,12)]
botOff$hits_dbl <- botOff$baseHits / botOff$doubles

mdlData$baseHitperStrikeout <- mdlData$baseHits / mdlData$strikeOuts

mdlData_top10 <- mdlData %>% filter(mdlData$numWins > as.integer(quantile(mdlData$numWins, prob=.90)))
mdlData_bottom10 <- mdlData %>% filter(mdlData$numWins < as.integer(quantile(mdlData$numWins, prob=.10)))


# Total Hits
mdlData$totHits <- mdlData$baseHits + mdlData$doubles + mdlData$triples + mdlData$homeruns

# Hits Taken v Hits Allowed
mdlData$hits_hitsAllowed <- mdlData$totHits / mdlData$hitsAllowed

# Probability that a hit is a homerun
mdlData$hr_prob <- mdlData$homeruns / mdlData$totHits

# Probability that an allowed hit will result in an error
mdlData$error_prob <- mdlData$errors / mdlData$hitsAllowed

# Hits per inning ---
# Change between batting v fielding
mdlData$bat_field <- mdlData$strikeOuts / 3
# Average hits per inning
mdlData$hits_inning <- mdlData$bat_field / mdlData$totHits




look <- mdlData %>% filter(mdlData$hr_prob >= 0.1)




# MODELING ----

mylm <- lm(numWins ~ totHits + errors + strikeOuts + 
             stolenBases + error_prob + stealRate, data = mdlData)


mylm <- lm(numWins ~ totHits + errors, data = mdlData)




mylm <- lm(formula = log(numWins) ~ totHits + strikeOuts + 
             hitsAllowed + hit_prob + errors + 
             stolenBases + homer_prob, data = mdlData)

summary(mylm)
plot(mylm)


mylm <- lm(numWins ~ totHits + strikeOuts + 
             hitsAllowed + hit_prob + errors + 
             error_prob + stolenBases + homer_prob, data = mdlData)


mylm <- lm(numWins ~ ., data = mdlData)

summary(mylm)

mylm <- lm(numWins ~ totHits + bat_field + hits_inning + hit_prob + 
             hit_diff + homer_prob + homer_diff + error_prob + errors, data = mdlData)



testPrep <- read.csv("Data/moneyball-evaluation-data.csv", stringsAsFactors = T)


testPrep = testPrep %>%
  rename(
    Index = INDEX,
    baseHits = TEAM_BATTING_H,
    doubles = TEAM_BATTING_2B,
    triples = TEAM_BATTING_3B,
    homeruns = TEAM_BATTING_HR,
    walksTaken = TEAM_BATTING_BB,
    freeBase = TEAM_BATTING_HBP,
    strikeOuts = TEAM_BATTING_SO,
    stolenBases = TEAM_BASERUN_SB,
    caughtStealing = TEAM_BASERUN_CS,
    errors = TEAM_FIELDING_E,
    doublePlays = TEAM_FIELDING_DP,
    walksAllowed = TEAM_PITCHING_BB,
    hitsAllowed = TEAM_PITCHING_H,
    homerunsAllowed = TEAM_PITCHING_HR,
    strikeOuts_byPitchers = TEAM_PITCHING_SO
  )

testPrep$strikeOuts[is.na(testPrep$strikeOuts)] <- median(testPrep$strikeOuts, na.rm=TRUE)
testPrep$stolenBases[is.na(testPrep$stolenBases)] <- mean(testPrep$stolenBases, na.rm = TRUE)


# Total Hits
testPrep$totHits <- testPrep$baseHits + testPrep$doubles + testPrep$triples + testPrep$homeruns

# Change between batting v fielding
testPrep$bat_field <- testPrep$strikeOuts / 3
# Average hits per inning
testPrep$hits_inning <-  testPrep$totHits / testPrep$bat_field 


# Hits Taken v Hits Allowed
testPrep$hit_prob <- testPrep$totHits / testPrep$hitsAllowed
testPrep$hit_diff <- testPrep$totHits - testPrep$hitsAllowed

# Probability that a hit is a home run
testPrep$homer_prob <- testPrep$homeruns / testPrep$totHits
testPrep$homer_diff <- testPrep$homeruns - testPrep$homerunsAllowed

# Probability that an allowed hit will result in an error
testPrep$error_prob <- testPrep$errors / testPrep$hitsAllowed



teamINDEX <- testPrep[,1]
mypred <- round(predict(mylm, testPrep))

test <- testPrep[,1] 


mydf <- data.frame(teamINDEX, mypred)


#












# Steal Rate
testPrep$stealRate <- testPrep$stolenBases / testPrep$caughtStealing

# Sac Fly estimate
testPrep$sacFly <- ((testPrep$strikeOuts / testPrep$triples))



























mylm <- lm((((mdlData$numWins)^lambda-1) / lambda) ~ totHits + errors, data = mdlData)

bc <- boxcox(mdlData$numWins ~ mdlData$strikeOuts)
(lambda <- bc$x[which.max(bc$y)])

new_model <- lm((((mdlData$numWins)^lambda-1) / lambda) ~ mdlData$strikeOuts)
summary(new_model)





# Steal Rate
mdlData$stealRate <- mdlData$stolenBases / mdlData$caughtStealing



# On Base Percentage Estimate
mdlData$OBP <- ((mdlData$totHits + mdlData$walksTaken + mdlData$freeBase) 
                / (mdlData$bat_field + mdlData$walksTaken + mdlData$freeBase + mdlData$sacFly))

# Slugging Percentage
mdlData$SLG <- (((mdlData$baseHits) + (2 * mdlData$doubles) + (3 * mdlData$triples) + (4 * mdlData$homeruns)) / 
                  (mdlData$bat_field * 100))





































baseHits_top25 <- data %>% filter(data$baseHits > as.integer(quantile(data$baseHits, prob=.75)))
baseHits_low25 <- data %>% filter(data$baseHits < as.integer(quantile(data$baseHits, prob=.25)))

win_top25 <- data %>% filter(data$numWins > as.integer(quantile(data$numWins, prob=.75)))
win_top10 <- data %>% filter(data$numWins > as.integer(quantile(data$numWins, prob=.90)))
win_bottom10 <- data %>% filter(data$numWins < as.integer(quantile(data$numWins, prob=.10)))





mylm <- lm(numWins ~ baseHits + doubles + strikeOuts + hitsAllowed + homerunsAllowed + doublePlays + errors, data = mdlData)






















mylm <- lm(numWins ~ hits_inning + error_prob + homer_prob, data = mdlData) # 0.09564
mylm <- lm(numWins ~ hits_hitsAllowed + hitsAllowed_errors, data = mdlData)

mylm <- lm(numWins ~ totHits + hr_prob + strikeOuts + homerunsAllowed + hitsAllowed + error_prob, data = mdlData) # 0.2311
mylm <- lm(numWins ~ hr_prob + strikeOuts + homerunsAllowed + hitsAllowed + error_prob, data = mdlData) # 0.1378

mylm <- lm(numWins ~ hr_prob + strikeOuts + hits_hitsAllowed, data = mdlData) # 0.06061

mylm <- lm(numWins ~ totHits + hitsAllowed + error_prob, data = mdlData) # 0.2249

mylm <- lm(numWins ~ totHits + hitsAllowed + error_prob + stealRate + hits_hitsAllowed + strikeOuts, data = mdlData) # 0.2481
mylm <- lm(numWins ~ totHits + strikeOuts + hitsAllowed + hits_hitsAllowed + errors + error_prob + stolenBases + stealRate, data = mdlData) # .2851

mylm <- lm(numWins ~ totHits + hitsAllowed + 
             homeruns + homerunsAllowed + 
             strikeOuts + strikeOuts_byPitchers +
             stolenBases + pitcher_error + errors + stealRate, data = mdlData) # 0.2693

mylm <- lm(numWins ~ totHits + hits_inning + strikeOuts + 
             hitsAllowed + hit_prob + errors + 
             error_prob + stolenBases + stealRate, data = mdlData) # 0.2887

mylm <- lm(numWins ~ totHits + hits_inning + strikeOuts + 
             hitsAllowed + hit_prob + errors + 
             error_prob + stolenBases + homer_prob, data = mdlData) # 0.2978

#





ggplot(data = mdlData) + 
  geom_point(mapping = aes(x = homer_diff, y = numWins))








# Change between batting v fielding
mdlData$bat_field <- mdlData$strikeOuts / 3
# Average hits per inning
mdlData$hits_inning <-  mdlData$totHits / mdlData$bat_field 

# Steal Rate
mdlData$stealRate <- mdlData$stolenBases / mdlData$caughtStealing

# IDK
mdlData$pitcher_error <- mdlData$strikeOuts_byPitchers * mdlData$error_prob

# IDK again
mdlData$hr_prob__bat_field <- mdlData$hr_prob * mdlData$bat_field

# Hit Proportions
mdlData$baseHit_prop <- mdlData$baseHits / mdlData$totHits
mdlData$double_prop <- mdlData$doubles / mdlData$totHits
mdlData$triple_prop <- mdlData$triples / mdlData$totHits
mdlData$hr_prop <- mdlData$homeruns / mdlData$totHits





























































