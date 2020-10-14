h2006 <- read.csv(file = "D:/DataMining/WorkingDirectory/Project2/datasets_all/hourly_2006.g", header = FALSE, sep ="")
h2006 <- h2006[-1,]
h2006 <- h2006[, -c(20:23)]


colnames(h2006) <- c("STN", "WBAN", "yearModa_hr", "Temp", "DewP", "Count", "SLP", "Count"
                     , "STP", "Count", "Visib", "Count", "WDSP", "Count", "MXSDP"
                     , "Gust", "PRCP", "SNDP", "FRSHIFT")

library(lubridate)
h2006$yearModa_hr <- ymd_h(h2006$yearModa_hr)
h2006$Year <- factor(year(h2006$yearModa_hr))
h2006$Month <- factor(month(h2006$yearModa_hr))
h2006$Day <- factor(day(h2006$yearModa_hr))
h2006$Hour <- factor(hour(h2006$yearModa_hr))
h2006 <- na.omit(h2006)
set.seed(25)

new_data_2006 = subset(h2006, Month == 10, select = STN:Hour)

new_index_2006 = sample(1:nrow(new_data_2006), 4000)
sample_dataset_2006 = new_data_2006[new_index_2006,]

train_index_2006 = sample(1:nrow(sample_dataset_2006), 0.8 * nrow(sample_dataset_2006))
test_index_2006 <- setdiff(1:nrow(sample_dataset_2006), train_index_2006)

train_set_2006 = sample_dataset_2006[train_index_2006,]
test_set_2006 = sample_dataset_2006[test_index_2006,]

train_set_2006[train_set_2006$Temp==9999.9, "Temp"] <- NA
train_set_2006[train_set_2006$DewP==9999.9, "DewP"] <- NA
train_set_2006[train_set_2006$SLP==9999.9, "SLP"] <- NA
train_set_2006[train_set_2006$STP==9999.9, "STP"] <- NA
train_set_2006[train_set_2006$Visib==999.9, "Visib"] <- NA
train_set_2006[train_set_2006$WDSP==999.9, "WDSP"] <- NA
train_set_2006[train_set_2006$MXSDP==999.9, "MXSDP"] <- NA
train_set_2006[train_set_2006$Gust==999.9, "Gust"] <- NA

train_set_2006$Temp <- as.numeric(as.character(train_set_2006$Temp))


train_set_2006$DewP <- as.numeric(as.character(train_set_2006$DewP))
for (i in 1:nrow(train_set_2006)) {
  if(is.na(train_set_2006[i, "DewP"])){
    train_set_2006[i, "DewP"] <- mean(train_set_2006[which(train_set_2006[,"Month"]==train_set_2006[i, "Month"]),"DewP"],na.rm = TRUE)
  }
}

train_set_2006$STP <- as.numeric(as.character(train_set_2006$STP))

for (i in 1:nrow(train_set_2006)) {
  if(is.na(train_set_2006[i, "STP"])){
    train_set_2006[i, "STP"] <- mean(train_set_2006[which(train_set_2006[,"Month"]==train_set_2006[i, "Month"]),"STP"],na.rm = TRUE)
  }
}


train_set_2006$WDSP <- as.numeric(as.character(train_set_2006$WDSP))

for (i in 1:nrow(train_set_2006)) {
  if(is.na(train_set_2006[i, "WDSP"])){
    train_set_2006[i, "WDSP"] <- mean(train_set_2006[which(train_set_2006[,"Month"]==train_set_2006[i, "Month"]),"WDSP"],na.rm = TRUE)
  }
}

obv_2006 <- aggregate(train_set_2006[c(4,5,9,13)], list(STN = train_set_2006$STN), mean)
obv_2006 = scale(obv_2006[,2:5])

library(amap)
set.seed(25)

result_euc_25_2006 <- Kmeans(obv_2006[,2:5],5, method = 'euclidean')

print(result_euc_25_2006)

plot_Temp_euc_25_2006 <- plot(x = c(1:5), y = result_euc_25_2006$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_25_2006 <- Kmeans(obv_2006[,2:5],5, method = 'pearson')

print(result_pear_25_2006)

plot_Temp_pear_25_2006 <- plot(x = c(1:5), y = result_pear_25_2006$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_25_2006 <- plot(x = c(1:5), y = result_euc_25_2006$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_25_2006 <- plot(x = c(1:5), y = result_pear_25_2006$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_25_2006 <- plot(x = c(1:5), y = result_euc_25_2006$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_25_2006 <- plot(x = c(1:5), y = result_pear_25_2006$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_25_2006 <- plot(x = c(1:5), y = result_euc_25_2006$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_25_2006 <- plot(x = c(1:5), y = result_pear_25_2006$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

set.seed(20)


result_euc_20_2006 <- Kmeans(obv_2006[,2:5],5, method = 'euclidean')

plot_Temp_euc_20_2006 <- plot(x = c(1:5), y = result_euc_20_2006$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_20_2006 <- Kmeans(obv_2006[,2:5],5, method = 'pearson')

plot_Temp_pear_20_2006 <- plot(x = c(1:5), y = result_pear_20_2006$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_20_2006 <- plot(x = c(1:5), y = result_euc_20_2006$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_20_2006 <- plot(x = c(1:5), y = result_pear_20_2006$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_20_2006 <- plot(x = c(1:5), y = result_euc_20_2006$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_20_2006 <- plot(x = c(1:5), y = result_pear_20_2006$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_20_2006 <- plot(x = c(1:5), y = result_euc_20_2006$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_20_2006 <- plot(x = c(1:5), y = result_pear_20_2006$centers[,4], xlab = 'Clusters', ylab = 'WDSP')


h2007 <- read.csv(file = "D:/DataMining/WorkingDirectory/Project2/datasets_all/hourly_2007.g", header = FALSE, sep ="")
h2007 <- h2007[-1,]
h2007 <- h2007[, -c(20:23)]

colnames(h2007) <- c("STN", "WBAN", "yearModa_hr", "Temp", "DewP", "Count", "SLP", "Count"
                     , "STP", "Count", "Visib", "Count", "WDSP", "Count", "MXSDP"
                     , "Gust", "PRCP", "SNDP", "FRSHIFT")

h2007$yearModa_hr <- ymd_h(h2007$yearModa_hr)
h2007$Year <- factor(year(h2007$yearModa_hr))
h2007$Month <- factor(month(h2007$yearModa_hr))
h2007$Day <- factor(day(h2007$yearModa_hr))
h2007$Hour <- factor(hour(h2007$yearModa_hr))
h2007 <- na.omit(h2007)
set.seed(25)


new_data_2007 = subset(h2007, Month == 10, select = STN:Hour)

new_index_2007 = sample(1:nrow(new_data_2007), 4000)
sample_dataset_2007 = new_data_2007[new_index_2007,]

train_index_2007 = sample(1:nrow(sample_dataset_2007), 0.8 * nrow(sample_dataset_2007))
test_index_2007 <- setdiff(1:nrow(sample_dataset_2007), train_index_2007)

train_set_2007 = sample_dataset_2007[train_index_2007,]
test_set_2007 = sample_dataset_2007[test_index_2007,]

train_set_2007[train_set_2007$Temp==9999.9, "Temp"] <- NA
train_set_2007[train_set_2007$DewP==9999.9, "DewP"] <- NA
train_set_2007[train_set_2007$SLP==9999.9, "SLP"] <- NA
train_set_2007[train_set_2007$STP==9999.9, "STP"] <- NA
train_set_2007[train_set_2007$Visib==999.9, "Visib"] <- NA
train_set_2007[train_set_2007$WDSP==999.9, "WDSP"] <- NA
train_set_2007[train_set_2007$MXSDP==999.9, "MXSDP"] <- NA
train_set_2007[train_set_2007$Gust==999.9, "Gust"] <- NA


train_set_2007$Temp <- as.numeric(as.character(train_set_2007$Temp))


train_set_2007$DewP <- as.numeric(as.character(train_set_2007$DewP))
for (i in 1:nrow(train_set_2007)) {
  if(is.na(train_set_2007[i, "DewP"])){
    train_set_2007[i, "DewP"] <- mean(train_set_2007[which(train_set_2007[,"Month"]==train_set_2007[i, "Month"]),"DewP"],na.rm = TRUE)
  }
}

train_set_2007$STP <- as.numeric(as.character(train_set_2007$STP))

for (i in 1:nrow(train_set_2007)) {
  if(is.na(train_set_2007[i, "STP"])){
    train_set_2007[i, "STP"] <- mean(train_set_2007[which(train_set_2007[,"Month"]==train_set_2007[i, "Month"]),"STP"],na.rm = TRUE)
  }
}


train_set_2007$WDSP <- as.numeric(as.character(train_set_2007$WDSP))

for (i in 1:nrow(train_set_2007)) {
  if(is.na(train_set_2007[i, "WDSP"])){
    train_set_2007[i, "WDSP"] <- mean(train_set_2007[which(train_set_2007[,"Month"]==train_set_2007[i, "Month"]),"WDSP"],na.rm = TRUE)
  }
}

obv_2007 <- aggregate(train_set_2007[c(4,5,9,13)], list(STN = train_set_2007$STN), mean)

set.seed(25)

result_euc_25_2007 <- Kmeans(obv_2007[,2:5],5, method = 'euclidean')

plot_Temp_euc_25_2007 <- plot(x = c(1:5), y = result_euc_25_2007$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_25_2007 <- Kmeans(obv_2007[,2:5],5, method = 'pearson')

plot_Temp_pear_25_2007 <- plot(x = c(1:5), y = result_pear_25_2007$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_25_2007 <- plot(x = c(1:5), y = result_euc_25_2007$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_25_2007 <- plot(x = c(1:5), y = result_pear_25_2007$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_25_2007 <- plot(x = c(1:5), y = result_euc_25_2007$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_25_2007 <- plot(x = c(1:5), y = result_pear_25_2007$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_25_2007 <- plot(x = c(1:5), y = result_euc_25_2007$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_25_2007 <- plot(x = c(1:5), y = result_pear_25_2007$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

set.seed(20)


result_euc_20_2007 <- Kmeans(obv_2007[,2:5],5, method = 'euclidean')

plot_Temp_euc_20_2007 <- plot(x = c(1:5), y = result_euc_20_2007$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_20_2007 <- Kmeans(obv_2007[,2:5],5, method = 'pearson')

plot_Temp_pear_20_2007 <- plot(x = c(1:5), y = result_pear_20_2007$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_20_2007 <- plot(x = c(1:5), y = result_euc_20_2007$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_20_2007 <- plot(x = c(1:5), y = result_pear_20_2007$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_20_2007 <- plot(x = c(1:5), y = result_euc_20_2007$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_20_2007 <- plot(x = c(1:5), y = result_pear_20_2007$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_20_2007 <- plot(x = c(1:5), y = result_euc_20_2007$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_20_2007 <- plot(x = c(1:5), y = result_pear_20_2007$centers[,4], xlab = 'Clusters', ylab = 'WDSP')



h2008 <- read.csv(file = "D:/DataMining/WorkingDirectory/Project2/datasets_all/hourly_2008.g", header = FALSE, sep ="")
h2008 <- h2008[-1,]
h2008 <- h2008[, -c(20:23)]

colnames(h2008) <- c("STN", "WBAN", "yearModa_hr", "Temp", "DewP", "Count", "SLP", "Count"
                     , "STP", "Count", "Visib", "Count", "WDSP", "Count", "MXSDP"
                     , "Gust", "PRCP", "SNDP", "FRSHIFT")

h2008$yearModa_hr <- ymd_h(h2008$yearModa_hr)
h2008$Year <- factor(year(h2008$yearModa_hr))
h2008$Month <- factor(month(h2008$yearModa_hr))
h2008$Day <- factor(day(h2008$yearModa_hr))
h2008$Hour <- factor(hour(h2008$yearModa_hr))
h2008 <- na.omit(h2008)
set.seed(25)


new_data_2008 = subset(h2008, Month == 10, select = STN:Hour)

new_index_2008 = sample(1:nrow(new_data_2008), 4000)
sample_dataset_2008 = new_data_2008[new_index_2008,]

train_index_2008 = sample(1:nrow(sample_dataset_2008), 0.8 * nrow(sample_dataset_2008))
test_index_2008 <- setdiff(1:nrow(sample_dataset_2008), train_index_2008)

train_set_2008 = sample_dataset_2008[train_index_2008,]
test_set_2008 = sample_dataset_2008[test_index_2008,]

train_set_2008[train_set_2008$Temp==9999.9, "Temp"] <- NA
train_set_2008[train_set_2008$DewP==9999.9, "DewP"] <- NA
train_set_2008[train_set_2008$SLP==9999.9, "SLP"] <- NA
train_set_2008[train_set_2008$STP==9999.9, "STP"] <- NA
train_set_2008[train_set_2008$Visib==999.9, "Visib"] <- NA
train_set_2008[train_set_2008$WDSP==999.9, "WDSP"] <- NA
train_set_2008[train_set_2008$MXSDP==999.9, "MXSDP"] <- NA
train_set_2008[train_set_2008$Gust==999.9, "Gust"] <- NA


train_set_2008$Temp <- as.numeric(as.character(train_set_2008$Temp))


train_set_2008$DewP <- as.numeric(as.character(train_set_2008$DewP))
for (i in 1:nrow(train_set_2008)) {
  if(is.na(train_set_2008[i, "DewP"])){
    train_set_2008[i, "DewP"] <- mean(train_set_2008[which(train_set_2008[,"Month"]==train_set_2008[i, "Month"]),"DewP"],na.rm = TRUE)
  }
}

train_set_2008$STP <- as.numeric(as.character(train_set_2008$STP))

for (i in 1:nrow(train_set_2008)) {
  if(is.na(train_set_2008[i, "STP"])){
    train_set_2008[i, "STP"] <- mean(train_set_2008[which(train_set_2008[,"Month"]==train_set_2008[i, "Month"]),"STP"],na.rm = TRUE)
  }
}


train_set_2008$WDSP <- as.numeric(as.character(train_set_2008$WDSP))

for (i in 1:nrow(train_set_2008)) {
  if(is.na(train_set_2008[i, "WDSP"])){
    train_set_2008[i, "WDSP"] <- mean(train_set_2008[which(train_set_2008[,"Month"]==train_set_2008[i, "Month"]),"WDSP"],na.rm = TRUE)
  }
}

obv_2008 <- aggregate(train_set_2008[c(4,5,9,13)], list(STN = train_set_2008$STN), mean)

set.seed(25)

result_euc_25_2008 <- Kmeans(obv_2008[,2:5],5, method = 'euclidean')

plot_Temp_euc_25_2008 <- plot(x = c(1:5), y = result_euc_25_2008$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_25_2008 <- Kmeans(obv_2008[,2:5],5, method = 'pearson')

plot_Temp_pear_25_2008 <- plot(x = c(1:5), y = result_pear_25_2008$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_25_2008 <- plot(x = c(1:5), y = result_euc_25_2008$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_25_2008 <- plot(x = c(1:5), y = result_pear_25_2008$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_25_2008 <- plot(x = c(1:5), y = result_euc_25_2008$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_25_2008 <- plot(x = c(1:5), y = result_pear_25_2008$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_25_2008 <- plot(x = c(1:5), y = result_euc_25_2008$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_25_2008 <- plot(x = c(1:5), y = result_pear_25_2008$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

set.seed(20)


result_euc_20_2008 <- Kmeans(obv_2008[,2:5],5, method = 'euclidean')

plot_Temp_euc_20_2008 <- plot(x = c(1:5), y = result_euc_20_2008$centers[,1], xlab = 'Clusters', ylab = 'Temp')

result_pear_20_2008 <- Kmeans(obv_2008[,2:5],5, method = 'pearson')

plot_Temp_pear_20_2008 <- plot(x = c(1:5), y = result_pear_20_2008$centers[,1], xlab = 'Clusters', ylab = 'Temp')

plot_DewP_euc_20_2008 <- plot(x = c(1:5), y = result_euc_20_2008$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_DewP_pear_20_2008 <- plot(x = c(1:5), y = result_pear_20_2008$centers[,2], xlab = 'Clusters', ylab = 'DewP')

plot_STP_euc_20_2008 <- plot(x = c(1:5), y = result_euc_20_2008$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_STP_pear_20_2008 <- plot(x = c(1:5), y = result_pear_20_2008$centers[,3], xlab = 'Clusters', ylab = 'STP')

plot_WDSP_euc_20_2008 <- plot(x = c(1:5), y = result_euc_20_2008$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

plot_WDSP_pear_20_2008 <- plot(x = c(1:5), y = result_pear_20_2008$centers[,4], xlab = 'Clusters', ylab = 'WDSP')

library(clusteval)
max_val_25_2006 <- array()
decision_25_2006 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_2006[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2006$cluster == i), (result_pear_25_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_2006[i] < similarity) {
      max_val_25_2006[i] <- similarity
      decision_25_2006[i]<-j
    }
  }
}

max_val_25_2007 <- array()
decision_25_2007 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_2007[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2007$cluster == i), (result_pear_25_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_2007[i] < similarity) {
      max_val_25_2007[i] <- similarity
      decision_25_2007[i]<-j
    }
  }
}

max_val_25_2008 <- array()
decision_25_2008 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_2008[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2008$cluster == i), (result_pear_25_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_2008[i] < similarity) {
      max_val_25_2008[i] <- similarity
      decision_25_2008[i]<-j
    }
  }
}

max_val_25_0607 <- array()
decision_25_0607 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0607[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2006$cluster == i), (result_pear_25_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0607[i] < similarity) {
      max_val_25_0607[i] <- similarity
      decision_25_0607[i]<-j
    }
  }
}

max_val_25_0608 <- array()
decision_25_0608 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0608[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2006$cluster == i), (result_pear_25_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0608[i] < similarity) {
      max_val_25_0608[i] <- similarity
      decision_25_0608[i]<-j
    }
  }
}

max_val_25_0706 <- array()
decision_25_0706 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0706[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2007$cluster == i), (result_pear_25_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0706[i] < similarity) {
      max_val_25_0706[i] <- similarity
      decision_25_0706[i]<-j
    }
  }
}

max_val_25_0708 <- array()
decision_25_0708 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0708[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2007$cluster == i), (result_pear_25_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0708[i] < similarity) {
      max_val_25_0708[i] <- similarity
      decision_25_0708[i]<-j
    }
  }
}

max_val_25_0806 <- array()
decision_25_0806 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0708[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2008$cluster == i), (result_pear_25_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0806[i] < similarity) {
      max_val_25_0806[i] <- similarity
      decision_25_0806[i]<-j
    }
  }
}

max_val_25_0807 <- array()
decision_25_0807 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_25_0807[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_25_2008$cluster == i), (result_pear_25_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_25_0807[i] < similarity) {
      max_val_25_0807[i] <- similarity
      decision_25_0807[i]<-j
    }
  }
}


max_val_20_2006 <- array()
decision_25_2006 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_2006[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2006$cluster == i), (result_pear_20_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_2006[i] < similarity) {
      max_val_20_2006[i] <- similarity
      decision_25_2006[i]<-j
    }
  }
}

max_val_20_2007 <- array()
decision_20_2007 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_2007[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2007$cluster == i), (result_pear_20_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_2007[i] < similarity) {
      max_val_20_2007[i] <- similarity
      decision_20_2007[i]<-j
    }
  }
}

max_val_20_2008 <- array()
decision_20_2008 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_2008[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2008$cluster == i), (result_pear_20_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_2008[i] < similarity) {
      max_val_20_2008[i] <- similarity
      decision_20_2008[i]<-j
    }
  }
}

max_val_20_0607 <- array()
decision_20_0607 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0708[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2006$cluster == i), (result_pear_20_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0607[i] < similarity) {
      max_val_20_0607[i] <- similarity
      decision_20_0607[i]<-j
    }
  }
}

max_val_20_0608 <- array()
decision_20_0608 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0608[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2006$cluster == i), (result_pear_20_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0608[i] < similarity) {
      max_val_20_0608[i] <- similarity
      decision_20_0608[i]<-j
    }
  }
}

max_val_20_0706 <- array()
decision_20_0706 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0608[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2007$cluster == i), (result_pear_20_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0706[i] < similarity) {
      max_val_20_0706[i] <- similarity
      decision_20_0706[i]<-j
    }
  }
}

max_val_20_0708 <- array()
decision_20_0708 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0708[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2007$cluster == i), (result_pear_20_2008$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0708[i] < similarity) {
      max_val_20_0708[i] <- similarity
      decision_20_0708[i]<-j
    }
  }
}

max_val_20_0806 <- array()
decision_20_0806 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0806[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2008$cluster == i), (result_pear_20_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0806[i] < similarity) {
      max_val_20_0806[i] <- similarity
      decision_20_0806[i]<-j
    }
  }
}

max_val_20_0807 <- array()
decision_20_0807 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20_0807[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2008$cluster == i), (result_pear_20_2007$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20_0807[i] < similarity) {
      max_val_20_0807[i] <- similarity
      decision_20_0807[i]<-j
    }
  }
}


max_val_20 <- array()
d_25 <- c(0,0,0,0,0)
for( i in c(1:5)) { 
  max_val_20[i] <- 0 
  for (j in c(1:5)){ 
    similarity <- cluster_similarity((result_euc_20_2006$cluster == i), (result_pear_20_2006$cluster == j), similarity = 'jaccard', method = 'independence')
    if(max_val_20[i] < similarity) {
      max_val_20[i] <- similarity
      d_25[i]<-j
    }
  }
}



