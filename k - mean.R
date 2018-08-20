library(haven)
library(reshape2)
setwd("C:/Users/yifei.liu/Desktop/k mean")
db <- read_spss("C:/Users/yifei.liu/Desktop/k_mean_data.sav")

# yes - no question
general.health <- c("GH1", "GH2", "GH4_1","GH4_2","GH4_3","GH4_4", "GH5", "GH6","GH7")
barriers <- c("B1_1","B1_2","B1_3","B1_4","B1_5","B1_6","B1_7","B1_8","B1_9","B1_10","B1_11","B1_12","B1_13","B1_14","B1_15")

# attitudinal question 7 point
fitness.attitude <- c("PF6_1","PF6_2","PF6_3","PF6_4","PF6_5","PF6_6","PF6_7","PF6_8")
stress.attitude <- c("ST4_1","ST4_2","ST4_3","ST4_4","ST4_5","ST4_6","ST4_7","ST4_8")
sleep.attitude <- c("SL4_1","SL4_2","SL4_3","SL4_4","SL4_5","SL4_6")
nutrition.attitude <- c("NE31","NE32","NE33","NE34","NE35","NE36","NE37","NE38")

############################################################################################################
# k mean first try 
# input all 7 point attitudinal inputs
mdb <- db[, c(fitness.attitude, stress.attitude, sleep.attitude, nutrition.attitude)]
result <- as.data.frame(db$identity)

for(i in 1:20){
  s <- sample(1:1000000,1)
  set.seed(s)
  km <- kmeans(mdb, 8, nstart = 20, iter.max = 30) # big nstart helps to kill bad local optimal
  print(paste("seed is ", s, " ;between_ss/total_ss = ", round(km$betweenss/km$totss, 4), sep = ""))
}
# seed = 142660  
set.seed(142660)
km <- kmeans(mdb, 3, nstart = 20)
seg3<- km$cluster

# seed = 416853
set.seed(416853)
km <- kmeans(mdb, 4, nstart = 20)
seg4<- km$cluster

# seed = 132651
set.seed(132651)
km <- kmeans(mdb, 5, nstart = 20)
seg5<- km$cluster

# seed = 35766
set.seed(35766)
km <- kmeans(mdb, 6, nstart = 20)
seg6<- km$cluster

# seed = 832999
set.seed(832999)
km <- kmeans(mdb, 7, nstart = 20)
seg7<- km$cluster

# seed = 727080
set.seed(727080)
km <- kmeans(mdb, 8, nstart = 20, iter.max = 30)
seg8<- km$cluster

result <- cbind(db[,1], seg3, seg4, seg5, seg6, seg7, seg8)
write.csv(result, "k means 3 to 8 segment solution.csv", row.names = FALSE)

