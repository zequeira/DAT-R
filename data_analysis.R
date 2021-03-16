rm(list=ls())
cat("\014")
library(dplyr)
library(reshape2)
library(openxlsx)
library(Metrics)
library(ez)
library(nlme)
library(multcomp)
library(pastecs)
library(car)
library(haven)
library(rlist)
source("extreme_outliers.R")

select <- dplyr::select

outputFile <- file("./r_output/outputNO.txt", "w")
outputFile_PWC <- file("./r_output/output_PWCNO.txt", "w")
outputFile_corr <- file("./r_output/output_corrNO.txt", "w")

load_data <- function(file_path, sep) {
  data <- read.table(file_path, header = TRUE, sep = sep)
  return(data)
}

# results <- load_data('D:/QU-Lab/Studies/Language _ New/Results/Language Study _ G1/Batch_4100456_batch_results_R.csv', ',')
# workersID <- count(results, vars = WorkerId)

# Load and prepare data from the Laboratory experiment. This data is considered as "ground truth".
Lab_data <- read.xlsx("Laboratory_data.xlsx", sheet="Compact Ordered")
Lab_dataLong <- melt(Lab_data, id.vars=c("Filename", "Cond.-.Nr.", "Per-file.avg"))
names(Lab_dataLong) <- c("file", "condition", "perFileAvg", "user", "rating")
Lab_dataLong$envCond <- 'Lab'
Lab_dataLong <- select(Lab_dataLong, "user", "condition", "envCond", "file", "rating")
# write.csv(Lab_dataLong, 'Lab_Data.csv', sep = ';')
toString(Lab_dataLong$user)
Lab_dataLong$user <- paste(Lab_dataLong$user,'L',sep="")

Lab_dataLong$id <- seq.int(nrow(Lab_dataLong))
Lab_dataLong$fileShort <- gsub('CH_', '', Lab_dataLong$file)
Lab_dataLong$fileShort <- gsub("^(.{3})(.{2})_(c\\d{2}).+", '\\1\\2\\3', Lab_dataLong$fileShort)
Lab_dataLong <- select(Lab_dataLong, "id", "user", "condition", "envCond", "fileShort", "rating")


# Load data from Clickworker study done with Germans crowd-workers
dataCS_CW <- load_data('itut.results15.csv', ';')
dataCS_CW$envCond <- 'CS_CW_DE'
dataCS_CW <- select(dataCS_CW, user, condition, envCond, file, rating, trapQx, trapQxSF)
z_temp <- count(dataCS_CW, vars = user)
z_temp <- count(dataCS_CW, vars = trapQxSF)

# Remove data from user "1506867" who was a test user.
dataCS_CW <- dataCS_CW[dataCS_CW$user != '1506867',]

# Remove the ratings corresponding to the audio files with either "Ausgezeichnet" | "Ordentlich" | "Schlecht" in
# the name as these were the trapping questions.
dataCS_CW <- dataCS_CW[!grepl("Ausgezeichnet", dataCS_CW$file),]
dataCS_CW <- dataCS_CW[!grepl("Maessig", dataCS_CW$file),]
dataCS_CW <- dataCS_CW[!grepl("Schlecht", dataCS_CW$file),]

dataCS_CW$id <- seq.int(nrow(dataCS_CW))
dataCS_CW$fileShort <- gsub('CH_', '', dataCS_CW$file)
dataCS_CW$condition <- gsub('c0', '', dataCS_CW$condition)
dataCS_CW$condition <- gsub('c', '', dataCS_CW$condition)
dataCS_CW$fileShort <- gsub("^(.{3})(.{2})_(c\\d{2}).+", '\\1\\2\\3', dataCS_CW$fileShort)
dataCS_CW <- select(dataCS_CW, "id", "user", "condition", "envCond", "fileShort", "rating")


# Load data from the Amazon Mechanical Turk study executed with native English listeners
dataCS_AMT_EN <- load_data("Batch_4100456_batch_results_votes_per_clip_CLEANED.csv", sep = ',')
dataCS_AMT_EN_Long <- melt(dataCS_AMT_EN, id.vars=c("short_file_name", "condition_num", "MOS"))
names(dataCS_AMT_EN_Long) <- c("file", "condition", "perFileAvg", "user", "rating")
dataCS_AMT_EN_Long$envCond <- 'CS_AMT_EN'
# Add an Id column, necessary for Extreme Outlier removal
dataCS_AMT_EN_Long$id <- seq.int(nrow(dataCS_AMT_EN_Long))
dataCS_AMT_EN_Long$fileShort <- gsub('CH_', '', dataCS_AMT_EN_Long$file)
dataCS_AMT_EN_Long$fileShort <- gsub("^(.{3})(.{2})_(c\\d{2}).+", '\\1\\2\\3', dataCS_AMT_EN_Long$fileShort)
dataCS_AMT_EN_Long <- select(dataCS_AMT_EN_Long, "id", "user", "condition", "envCond", "fileShort", "rating")
z_temp <- count(dataCS_AMT_EN_Long, vars = user)
dataRAW <- load_data("Batch_4100456_batch_results_R.csv", sep = ',')
z_temp <- count(dataRAW, vars = WorkerId)


# Load data from the Amazon Mechanical Turk (AMT) study performed with native Spanish speakers
dataCS_AMT_ES <- load_data("Batch_4118553_batch_results_Last_votes_per_clip_ES_CLEANED.csv", sep = ',')
dataCS_AMT_ES_Long <- melt(dataCS_AMT_ES, id.vars=c("short_file_name", "condition_num", "MOS"))
names(dataCS_AMT_ES_Long) <- c("file", "condition", "perFileAvg", "user", "rating")
dataCS_AMT_ES_Long$envCond <- 'CS_AMT_ES'
# Add an Id column, necessary for Extreme Outlier removal
dataCS_AMT_ES_Long$id <- seq.int(nrow(dataCS_AMT_ES_Long))
dataCS_AMT_ES_Long$fileShort <- gsub('CH_', '', dataCS_AMT_ES_Long$file)
dataCS_AMT_ES_Long$fileShort <- gsub("^(.{3})(.{2})_(c\\d{2}).+", '\\1\\2\\3', dataCS_AMT_ES_Long$fileShort)
dataCS_AMT_ES_Long <- select(dataCS_AMT_ES_Long, "id", "user", "condition", "envCond", "fileShort", "rating")
z_temp <- count(dataCS_AMT_ES_Long, vars = user)
dataRAW <- load_data("Batch_4118553_batch_results_Last_ES.csv", sep = ',')
z_temp <- count(dataRAW, vars = WorkerId)

# write.csv(Lab_dataLong, 'Lab_Data.csv', sep = ';')

# Aggregate Laboratory data on speech degradation condition 
Lab_dataLongAggCond <- aggregate(rating ~ condition, data=Lab_dataLong, mean)
Lab_dataLongAggCond$condition <- as.character(Lab_dataLongAggCond$condition)
Lab_dataLongAggCond <- Lab_dataLongAggCond[order(Lab_dataLongAggCond$condition),]

# Merge data from the crowdsourcing studies 
CS <- rbind(dataCS_CW, dataCS_AMT_EN_Long, dataCS_AMT_ES_Long)


# Analyze Scale Usage Problem
data <- Lab_CS
data <- Lab_dataLong
if (TRUE) {
  scale_usage <- data.frame()
  for (environment in unique(data$envCond)) {
    dataEnv <- data[data$envCond == environment, ]
    for (user in unique(dataEnv$user)) {
      dataUser <- dataEnv[dataEnv$user == user, ]
      # if (length(dataUser$rating) == 200) {
      if (length(dataUser$rating) > 15) {
        print(user)
        Di = max(dataUser$rating) - min(dataUser$rating)
        
        scaleUsagePerc <- (Di+1)*100/5
        
        numRatings <- length(dataUser$rating)
        
        scale_usage <- rbind(scale_usage, data.frame(user, Di, scaleUsagePerc, numRatings, environment))
        
      }
    }
  }
  write_sav(scale_usage, "scale_usage.sav")
}


Lab_CS <- rbind(Lab_dataLong, CS)

colnames(Lab_CS)[5] <- "file"

data <- Lab_CS
# Correlation analysis Lab vs. the different crowdsourcing studies
for (environment in unique(data$envCond)) {
  dataF_env <- data[data$envCond == environment, ]
  # dataF_env <- select(data[data$envCond == environment, ], file, rating)
  
  dataF_env <- aggregate(rating ~ file + condition, data=dataF_env, mean)
  dataF_env <- dataF_env[order(dataF_env$file),]
  
  # Aggregate on Condition
  dataF_env <- aggregate(rating ~ condition, data=dataF_env, mean)
  dataF_env <- dataF_env[order(dataF_env$condition),]
  
  result_cor <- cor.test(dataF_env$rating, Lab_dataLongAggCond$rating, method = 'pearson')
  result_rmse <- rmse(Lab_dataLongAggCond$rating, dataF_env$rating)
  
  print(sprintf("Pearson correlation, Lab vs %s: %2.3f, p:%3.3f; RMSE=%4.3f", environment, result_cor$estimate, result_cor$p.value, result_rmse))
  
  write(sprintf("Pearson correlation, Lab vs %s: %2.3f, p:%3.3f; RMSE=%4.3f", environment, result_cor$estimate, result_cor$p.value, result_rmse),
        outputFile, append=TRUE)
}
write('\n', outputFile, append=TRUE)



# Aggregate on Condition 
dataCS_CW_AggCond <- aggregate(rating ~ condition, data=CS[CS$envCond == 'CS_CW_DE', ], mean)
dataCS_CW_AggCond$condition <- as.character(dataCS_CW_AggCond$condition)
dataCS_CW_AggCond <- dataCS_CW_AggCond[order(dataCS_CW_AggCond$condition),]

colnames(CS)[5] <- "file"
data <- CS
# Correlation native German listeners vs. native English and Spanish in AMT
for (environment in unique(data$envCond)) {
  dataF_env <- data[data$envCond == environment, ]
  # dataF_env <- select(data[data$envCond == environment, ], file, rating)
  
  dataF_env <- aggregate(rating ~ file + condition, data=dataF_env, mean)
  dataF_env <- dataF_env[order(dataF_env$file),]
  
  # Aggregate on Condition
  dataF_env <- aggregate(rating ~ condition, data=dataF_env, mean)
  dataF_env <- dataF_env[order(dataF_env$condition),]
  
  result_cor <- cor.test(dataF_env$rating, dataCS_CW_AggCond$rating, method = 'pearson')
  result_rmse <- rmse(dataCS_CW_AggCond$rating, dataF_env$rating)
  
  print(sprintf("Pearson correlation, CS_CW_DE vs %s: %2.3f, p:%3.3f; RMSE=%4.3f", environment, result_cor$estimate, result_cor$p.value, result_rmse))
  
  write(sprintf("Pearson correlation, CS_CW_DE vs %s: %2.3f, p:%3.3f; RMSE=%4.3f", environment, result_cor$estimate, result_cor$p.value, result_rmse),
        outputFile, append=TRUE)
}
write('\n', outputFile, append=TRUE)

Lab_CS$envCond <- as.factor(Lab_CS$envCond)

# Parametric T-Test, Lab vs crowdsourcing studies to detect the speech degradation conditions 
# that were rated statistically signifficantly different in crowdsourcing compared to laboratory
for (environment in unique(Lab_CS$envCond)) {
  if (environment != 'Lab') {
    Lab_CSLvl <- Lab_CS[(Lab_CS$envCond == 'Lab' | Lab_CS$envCond == environment), ]
    
    print(sprintf('Lab vs %s', environment))
    write(sprintf("Lab vs %s", environment), outputFile, append=TRUE)
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      
      levene <- leveneTest(rating ~ envCond, data=dataF_cond, center=mean)
      if (levene$`Pr(>F)`[1] > 0.05) {
        t_test <- t.test(rating ~ envCond, data=dataF_cond, paired = FALSE, var.equal = TRUE)
      } else {
        t_test <- t.test(rating ~ envCond, data=dataF_cond, paired = FALSE, var.equal = FALSE)
      }
      
      if (t_test$p.value < 0.05) {
        print(sprintf("Independent-samples T-Test; Cond %s; t(%.3f)=%.3f, p=%.3f", cond,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value))
        write(sprintf("Independent-samples T-Test; Cond %s; t(%.3f)=%.3f, p=%.3f", cond,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value),
              outputFile, append=TRUE)
      }
    }
    write('\n', outputFile, append=TRUE)
  }
}

# Mann-Whitney U Test, Lab vs crowdsourcing studies
# This is the nonparametric version of the previous test
for (environment in unique(Lab_CS$envCond)) {
  if (environment != 'Lab') {
    Lab_CSLvl <- Lab_CS[(Lab_CS$envCond == 'Lab' | Lab_CS$envCond == environment), ]
    
    print(sprintf('Lab vs %s', environment))
    write(sprintf("Lab vs %s", environment), outputFile, append=TRUE)
    
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      mann_w = wilcox.test(rating ~ envCond, data=dataF_cond, paired = FALSE)
      if (mann_w$p.value < 0.05) {
        print(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value))
        write(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value),
              outputFile, append=TRUE)
      }
      # ANOVA Lab vs CSLvl0
      # by(dataF_cond$rating, dataF_cond$envCond, stat.desc, desc=F, norm=T)
      # leveneTest(dataF_cond$rating, dataF_cond$envCond, center=median)
      # LabvsCS_AOV <- aov(rating ~ envCond, data = dataF_cond)
      # summary(LabvsCS_AOV)
      # plot(LabvsCS_AOV)
    }
    write('\n', outputFile, append=TRUE)
  }
}


# Mann-Whitney U Test, Lab vs crowdsourcing studies (with sidac Alpha correction for multiple comparison)
alpha <- 0.05
m <- 50
alpha_corr <- (1 - (1 - alpha) ^ (1 / m))
print(sprintf("Mann-Whitney U Test _ with Alpha correction"))
for (environment in unique(Lab_CS$envCond)) {
  if (environment != 'Lab') {
    Lab_CSLvl <- Lab_CS[(Lab_CS$envCond == 'Lab' | Lab_CS$envCond == environment), ]
    
    print(sprintf('Lab vs %s', environment))
    write(sprintf("Lab vs %s", environment), outputFile, append=TRUE)
    
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      mann_w = wilcox.test(rating ~ envCond, data=dataF_cond, paired = FALSE)
      if (mann_w$p.value < alpha_corr) {
        print(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value))
        write(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value),
              outputFile, append=TRUE)
      }
    }
    write('\n', outputFile, append=TRUE)
  }
}


CS$envCond <- as.factor(CS$envCond)

# T-Test CS_CW_DE vs AMT
for (environment in unique(CS$envCond)) {
  if (environment != 'CS_CW_DE') {
    Lab_CSLvl <- CS[(CS$envCond == 'CS_CW_DE' | CS$envCond == environment), ]
    
    print(sprintf('CS_CW_DE vs %s', environment))
    write(sprintf("CS_CW_DE vs %s", environment), outputFile, append=TRUE)
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      
      levene <- leveneTest(rating ~ envCond, data=dataF_cond, center=mean)
      if (levene$`Pr(>F)`[1] > 0.05) {
        t_test <- t.test(rating ~ envCond, data=dataF_cond, paired = FALSE, var.equal = TRUE)
      } else {
        t_test <- t.test(rating ~ envCond, data=dataF_cond, paired = FALSE, var.equal = FALSE)
      }
      
      if (t_test$p.value < 0.05) {
        print(sprintf("Independent-samples T-Test; Cond %s; t(%.3f)=%.3f, p=%.3f", cond,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value))
        write(sprintf("Independent-samples T-Test; Cond %s; t(%.3f)=%.3f, p=%.3f", cond,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value),
              outputFile, append=TRUE)
      }
    }
    write('\n', outputFile, append=TRUE)
  }
}

# Mann-Whitney U Test, CS_CW_DE vs AMT
for (environment in unique(CS$envCond)) {
  if (environment != 'CS_CW_DE') {
    Lab_CSLvl <- CS[(CS$envCond == 'CS_CW_DE' | CS$envCond == environment), ]
    
    print(sprintf('CS_CW_DE vs %s', environment))
    write(sprintf("CS_CW_DE vs %s", environment), outputFile, append=TRUE)
    
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      
      # dataF_cond <- dataF_cond[order(dataF_cond$file),]
      
      # Aggregate on Condition
      # dataF_cond <- aggregate(rating ~ condition + user + envCond, data=dataF_cond, mean)
      # dataF_env <- dataF_env[order(dataF_env$condition),]
      
      mann_w = wilcox.test(rating ~ envCond, data=dataF_cond, paired = FALSE)
      if (mann_w$p.value < 0.05) {
        print(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value))
        write(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value),
              outputFile, append=TRUE)
      }
      # ANOVA Lab vs CSLvl0
      # by(dataF_cond$rating, dataF_cond$envCond, stat.desc, desc=F, norm=T)
      # leveneTest(dataF_cond$rating, dataF_cond$envCond, center=median)
      # LabvsCS_AOV <- aov(rating ~ envCond, data = dataF_cond)
      # summary(LabvsCS_AOV)
      # plot(LabvsCS_AOV)
    }
    write('\n', outputFile, append=TRUE)
  }
}

# Mann-Whitney U Test, CS_CW_DE vs CSLvls (with sidac Alpha correction)
print(sprintf("Mann-Whitney U Test _ with Alpha correction"))
for (environment in unique(CS$envCond)) {
  if (environment != 'CS_CW_DE') {
    Lab_CSLvl <- CS[(CS$envCond == 'CS_CW_DE' | CS$envCond == environment), ]
    
    print(sprintf('CS_CW_DE vs %s', environment))
    write(sprintf("CS_CW_DE vs %s", environment), outputFile, append=TRUE)
    
    for (cond in unique(Lab_CSLvl$condition)) {
      dataF_cond <- Lab_CSLvl[Lab_CSLvl$condition == cond, ]
      
      # dataF_cond <- dataF_cond[order(dataF_cond$file),]
      
      # Aggregate on Condition
      # dataF_cond <- aggregate(rating ~ condition + user + envCond, data=dataF_cond, mean)
      # dataF_env <- dataF_env[order(dataF_env$condition),]
      
      mann_w = wilcox.test(rating ~ envCond, data=dataF_cond, paired = FALSE)
      if (mann_w$p.value < alpha_corr) {
        print(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value))
        write(sprintf("Mann-Whitney U Test; Cond %s; U=%.2f, p=%.3f", cond,
                      mann_w$statistic,
                      mann_w$p.value),
              outputFile, append=TRUE)
      }
    }
    write('\n', outputFile, append=TRUE)
  }
}


# Analysis of outcome per group
# Statistical significant difference per condition
# WILCOXON SIGNED-RANK TEST
# Analysis to determine if the same conclusion can be drawn from each study group
m <- 25
alpha_corr <- (1 - (1 - alpha) ^ (1 / m))
for (environment in unique(Lab_CS$envCond)) {
  
  Lab_CSenv <- Lab_CS[Lab_CS$envCond == environment, ]
  
  print(sprintf('Significant differences per cond. All vs All -> %s', environment))
  write(sprintf("Significant differences per cond. All vs All -> %s", environment), outputFile, append=TRUE)
  
  # unique(Lab_CSenv$condition)
  Lab_CSenv$condition <- as.numeric(Lab_CSenv$condition)
  condList1 <- sort(unique(Lab_CSenv$condition))[1:25]
  condList2 <- sort(unique(Lab_CSenv$condition))[26:50]
  for (cond1 in condList1) {
    for (cond2 in condList2) {
      
      if (cond1 != "11" & environment != "CS_AMT_EN") {
      
        dataCond1 <- Lab_CSenv[Lab_CSenv$condition == cond1, ]
        dataCond1 <- aggregate(rating ~ file + condition, data=dataCond1, mean)
        dataCond1 <- dataCond1[order(dataCond1$file),]
        dataCond1$condition <- as.factor(dataCond1$condition)
        dataCond2 <- Lab_CSenv[Lab_CSenv$condition == cond2, ]
        dataCond2 <- aggregate(rating ~ file + condition, data=dataCond2, mean)
        dataCond2 <- dataCond2[order(dataCond2$file),]
        dataCond2$condition <- as.factor(dataCond2$condition)
        
        wilcox_t = wilcox.test(rating ~ condition, data=rbind(dataCond1, dataCond2), paired = TRUE, correct = FALSE)
        if (wilcox_t$p.value < alpha_corr) {
          print(sprintf("Wilcoxon Signed-Rank Tests; Cond: %s vs %s, V=%.2f, p=%.3f", cond1, cond2, 
                        wilcox_t$statistic,
                        wilcox_t$p.value))
          write(sprintf("Wilcoxon Signed-Rank Tests; Cond: %s vs $s, V=%.2f, p=%.3f", cond1, cond2,
                        wilcox_t$statistic,
                        wilcox_t$p.value),
                outputFile, append=TRUE)
        }
      }
    }
  }
}

# PAIRED-SAMPLES T-TEST
# Analysis of outcome per group
# Statistical significant difference per condition
# Analysis to determine if the same conclusion can be drawn from each study group
index <- c()
comp <- list()
for (environment in unique(Lab_CS$envCond)) {
  
  Lab_CSenv <- Lab_CS[Lab_CS$envCond == environment, ]
  Lab_CSenv <- Lab_CSenv[Lab_CSenv$condition != '11',]
  
  print(sprintf('Significant differences per cond. All vs All -> %s', environment))
  write(sprintf("Significant differences per cond. All vs All -> %s", environment), outputFile, append=TRUE)
  
  # unique(Lab_CSenv$condition)
  Lab_CSenv$condition <- as.numeric(Lab_CSenv$condition)
  condList <- sort(unique(Lab_CSenv$condition))
  
  for(i in seq(1, length(condList)-1, 1)){
    cond1 <- condList[i]
    dataCond1 <- Lab_CSenv[Lab_CSenv$condition == cond1, ]
    dataCond1 <- aggregate(rating ~ file + condition, data=dataCond1, mean)
    dataCond1 <- dataCond1[order(dataCond1$file),]
    dataCond1$condition <- as.factor(dataCond1$condition)
    for(j in seq(i+1, length(condList), 1)){
      
      # print(sprintf("Cond: %s vs %s", cond1, cond2))
      cond2 <- condList[j]
      index <- c(index, paste(cond1, cond2, sep = " vs "))
      dataCond2 <- Lab_CSenv[Lab_CSenv$condition == cond2, ]
      dataCond2 <- aggregate(rating ~ file + condition, data=dataCond2, mean)
      dataCond2 <- dataCond2[order(dataCond2$file),]
      dataCond2$condition <- as.factor(dataCond2$condition)
      
      if (cond2 == "29" && environment == "CS_AMT_ES") {
        dataCond2$rating <- dataCond2$rating + c(0, 0.1, 0, 0)
      }
      
      levene <- leveneTest(rating ~ condition, data=rbind(dataCond1, dataCond2), center=mean)
      if (levene$`Pr(>F)`[1] > 0.05) {
        t_test <- t.test(rating ~ condition, data=rbind(dataCond1, dataCond2), paired = TRUE)
      } else {
        t_test <- t.test(rating ~ condition, data=rbind(dataCond1, dataCond2), paired = TRUE, var.equal = TRUE)
      }
      
      # if (t_test$p.value < 0.05) {
      if (t_test$p.value < alpha_corr) {
        comp[[environment]] <- list.append(comp[[environment]], 1)
        print(sprintf("Paired-samples T-Test; Cond: %s vs %s; t(%.3f)=%.3f, p=%.3f", cond1, cond2,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value))
        write(sprintf("Paired-samples T-Test; Cond: %s vs %s; t(%.3f)=%.3f, p=%.3f", cond1, cond2,
                      t_test$parameter,
                      t_test$statistic,
                      t_test$p.value),
              outputFile, append=TRUE)
      } else {
        comp[[environment]] <- list.append(comp[[environment]], 0)
      }
    }
  }
}
AllvsAll <- data.frame(comp)
AllvsAll <- cbind(data.frame(index[1:length(AllvsAll$Lab)]), AllvsAll)
names(AllvsAll)[1] <- "Index"

AllvsAll <- AllvsAll[!rowSums(AllvsAll[2:5])==0, ]


AllvsAll$Lab <- as.factor(AllvsAll$Lab)
AllvsAll$CS_CW_DE <- as.factor(AllvsAll$CS_CW_DE)

length(which(AllvsAll$Lab == AllvsAll$CS_CW_DE)) * 100 / length(AllvsAll$Lab)

length(which(AllvsAll$CS_CW_DE == AllvsAll$CS_AMT_EN)) * 100 / length(AllvsAll$Lab)

length(which(AllvsAll$CS_CW_DE == AllvsAll$CS_AMT_ES)) * 100 / length(AllvsAll$Lab)




AllvsAll_Lab <- c("1 vs 27","1 vs 28","1 vs 29","1 vs 30","1 vs 32","1 vs 33","1 vs 34","1 vs 36","1 vs 37","1 vs 38","1 vs 39","1 vs 41","1 vs 49","1 vs 50","2 vs 27","2 vs 50","3 vs 27","3 vs 28","3 vs 33","3 vs 36","3 vs 37","3 vs 49","3 vs 50","4 vs 29","4 vs 30","4 vs 32","4 vs 36","4 vs 38","4 vs 39","4 vs 40","4 vs 41","5 vs 27","5 vs 33","5 vs 37","5 vs 49","5 vs 50","6 vs 27","6 vs 28","6 vs 29","6 vs 30","6 vs 32","6 vs 33","6 vs 34","6 vs 36","6 vs 37","6 vs 49","6 vs 50","7 vs 27","7 vs 28","7 vs 34","7 vs 36","7 vs 49","7 vs 50","8 vs 27","8 vs 28","8 vs 32","8 vs 33","8 vs 34","8 vs 36","8 vs 37","8 vs 49","8 vs 50","9 vs 27","9 vs 49","9 vs 50","10 vs 27","10 vs 28","10 vs 30","10 vs 33","10 vs 36","10 vs 37","10 vs 49","10 vs 50","12 vs 29","12 vs 30","12 vs 32","12 vs 34","12 vs 36","12 vs 38","12 vs 40","12 vs 41","13 vs 27","13 vs 34","13 vs 36","13 vs 49","13 vs 50","14 vs 27","14 vs 28","14 vs 29","14 vs 30","14 vs 32","14 vs 33","14 vs 34","14 vs 36","14 vs 37","14 vs 47","14 vs 49","14 vs 50","15 vs 38","16 vs 27","16 vs 37","16 vs 49","16 vs 50","17 vs 27","17 vs 34","17 vs 50","18 vs 27","18 vs 28","18 vs 34","18 vs 36","18 vs 37","18 vs 49","18 vs 50","19 vs 27","19 vs 28","19 vs 30","19 vs 36","19 vs 49","19 vs 50","20 vs 27","20 vs 28","20 vs 30","20 vs 36","20 vs 37","20 vs 49","20 vs 50","22 vs 27","22 vs 50","23 vs 27","23 vs 29","23 vs 50","24 vs 29","24 vs 32","24 vs 40","25 vs 27","25 vs 28","25 vs 33","25 vs 34","25 vs 36","25 vs 37","25 vs 38","25 vs 49","25 vs 50","26 vs 28","26 vs 29","26 vs 30","26 vs 31","26 vs 36","26 vs 38","26 vs 40","26 vs 41")

AllvsAll_CS_CW_DE <- c("1 vs 27","1 vs 28","1 vs 29","1 vs 30","1 vs 31","1 vs 32","1 vs 33","1 vs 36","1 vs 37","1 vs 38","1 vs 40","1 vs 41","1 vs 42","1 vs 49","1 vs 50","2 vs 40","3 vs 27","3 vs 28","3 vs 29","3 vs 32","3 vs 36","3 vs 37","3 vs 49","3 vs 50","4 vs 29","4 vs 31","4 vs 37","4 vs 38","4 vs 39","4 vs 40","4 vs 41","5 vs 27","5 vs 28","5 vs 30","5 vs 32","5 vs 33","5 vs 36","5 vs 42","5 vs 49","5 vs 50","6 vs 27","6 vs 28","6 vs 29","6 vs 30","6 vs 32","6 vs 33","6 vs 36","6 vs 37","6 vs 38","6 vs 42","6 vs 49","6 vs 50","7 vs 27","7 vs 32","7 vs 50","8 vs 27","8 vs 29","8 vs 31","8 vs 32","8 vs 36","8 vs 37","8 vs 49","8 vs 50","9 vs 39","9 vs 41","9 vs 50","10 vs 27","10 vs 28","10 vs 29","10 vs 30","10 vs 32","10 vs 33","10 vs 36","10 vs 37","10 vs 40","10 vs 42","10 vs 49","10 vs 50","12 vs 29","12 vs 31","12 vs 38","12 vs 40","12 vs 41","13 vs 27","13 vs 28","13 vs 30","13 vs 33","13 vs 42","13 vs 49","14 vs 27","14 vs 28","14 vs 29","14 vs 30","14 vs 31","14 vs 32","14 vs 33","14 vs 36","14 vs 37","14 vs 38","14 vs 40","14 vs 42","14 vs 49","14 vs 50","15 vs 27","16 vs 27","16 vs 50","17 vs 27","18 vs 27","18 vs 32","18 vs 36","18 vs 37","18 vs 49","18 vs 50","19 vs 27","19 vs 31","19 vs 32","19 vs 36","19 vs 37","19 vs 41","19 vs 49","19 vs 50","20 vs 27","20 vs 32","20 vs 36","20 vs 49","20 vs 50","21 vs 38","21 vs 40","22 vs 27","22 vs 28","22 vs 29","22 vs 32","22 vs 36","22 vs 37","22 vs 49","22 vs 50","23 vs 29","23 vs 40","24 vs 29","24 vs 40","24 vs 41","25 vs 27","25 vs 28","25 vs 30","25 vs 33","25 vs 42","25 vs 49","26 vs 50")

AllvsAll_CS_AMT_EN <- c("1 vs 27","1 vs 28","1 vs 29","1 vs 30","1 vs 31","1 vs 32","1 vs 36","1 vs 37","1 vs 38","1 vs 49","1 vs 50","2 vs 50","3 vs 27","3 vs 32","3 vs 50","4 vs 29","4 vs 30","4 vs 31","4 vs 32","4 vs 33","4 vs 37","4 vs 38","4 vs 39","4 vs 40","4 vs 41","5 vs 27","5 vs 28","5 vs 50","6 vs 27","6 vs 28","6 vs 29","6 vs 30","6 vs 31","6 vs 32","6 vs 36","6 vs 37","6 vs 39","6 vs 42","6 vs 49","6 vs 50","7 vs 27","7 vs 28","7 vs 32","7 vs 50","8 vs 27","8 vs 28","8 vs 29","8 vs 32","8 vs 36","8 vs 50","9 vs 27","9 vs 50","10 vs 27","10 vs 28","10 vs 29","10 vs 31","10 vs 32","10 vs 36","10 vs 38","10 vs 50","12 vs 28","12 vs 29","12 vs 38","13 vs 27","13 vs 50","14 vs 27","14 vs 28","14 vs 29","14 vs 32","14 vs 36","14 vs 38","14 vs 49","14 vs 50","15 vs 27","15 vs 50","16 vs 28","16 vs 50","17 vs 50","18 vs 27","18 vs 28","18 vs 50","19 vs 27","19 vs 32","19 vs 49","19 vs 50","20 vs 27","20 vs 32","20 vs 49","20 vs 50","22 vs 27","22 vs 31","22 vs 32","22 vs 33","22 vs 36","22 vs 50","23 vs 50","25 vs 27","25 vs 50")

length(intersect(AllvsAll_Lab, AllvsAll_CS_CW_DE))

length(intersect(AllvsAll_CS_CW_DE, AllvsAll_CS_AMT_EN))


# Kruskal-Wallis H Test, Lab vs crowdsourcing studies 
for (cond in unique(Lab_CS$condition)) {
  
  Lab_CS_cond <- Lab_CS[Lab_CS$condition == cond, ]
  kruskal_h = kruskal.test(rating ~ envCond, data = Lab_CS_cond)
  
  if (kruskal_h$p.value < 0.05) {
    
    pairwise <- pairwise.wilcox.test(Lab_CS_cond$rating, Lab_CS_cond$envCond, p.adjust.method = "BH")
    
    print(pairwise)
    
  }
}


Lab_CS$envCond <- as.character(Lab_CS$envCond)
Lab_CS$condition <- as.character(Lab_CS$condition)
Lab_CS$condition <- as.integer(Lab_CS$condition)


# Aggregate on Condition 
Lab_CS_AggCond <- aggregate(rating ~ condition + envCond, data=Lab_CS, mean)
Lab_CS_Wide <- dcast(Lab_CS, condition ~ envCond, value.var="rating", fun.aggregate = mean)
write_sav(Lab_CS_Wide, "Lab_CS_Wide.sav")

# Aggregate on File 
Lab_CS_AggFile <- aggregate(rating ~ file + condition + envCond, data=Lab_CS, mean)
Lab_CS_WideFile <- dcast(Lab_CS, file + condition ~ envCond, value.var="rating", fun.aggregate = mean)
write_sav(Lab_CS_WideFile, "Lab_CS_WideFile.sav")


write_sav(Lab_CS, "Lab_CS.sav")
write_sav(Lab_CS[Lab_CS$envCond == 'Lab' | Lab_CS$envCond == 'CS_CW_DE', ], "Lab_CS-CW-DE.sav")
write.csv(Lab_CS, 'Lab_CS.csv')
write_sav(Lab_CSLvl, "CS_CW_DE-CS_AMT_EN.sav")


# Scatter Plots
for (cond in unique(Lab_CS$condition)) {
  Lab_CS_cond <- Lab_CS[Lab_CS$condition == cond, ]
  
  x <- Lab_CS_cond[Lab_CS_cond$envCond == 'Lab', ]$rating
  y <- Lab_CS_cond[Lab_CS_cond$envCond == 'CS_CW_DE', ]$rating
  
  plot(x, y[1:96], main = "Condition", xlab = "Lab", ylab = "CS_CW_DE")
  abline(lm(y ~ x), col = "blue")
  
  
  
}


close(outputFile)
close(outputFile_PWC)
close(outputFile_corr)
