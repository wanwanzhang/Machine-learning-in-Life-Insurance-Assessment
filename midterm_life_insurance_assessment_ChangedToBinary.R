#Machine learning in Life Insurance Assessment

#loading and exploring data
train <- read.csv("C:/Users/Wanwan Zhang/Desktop/2016FALL/ADS/midterm project/train.csv")
str(train)
summary(train)
dim(train)
head(train)

#columns with missing/NA values
colnames(train)[colSums(is.na(train)) > 0]

#plotting target variable
hist(train$Response, xlab="Response", ylab="Frequency", main="Target variable frequency")

#total number of missing values in some columns
#[1] "Employment_Info_1"   "Employment_Info_4"   "Employment_Info_6"   "Insurance_History_5" "Family_Hist_2"      
#[6] "Family_Hist_3"       "Family_Hist_4"       "Family_Hist_5"       "Medical_History_1"   "Medical_History_10" 
#[11] "Medical_History_15"  "Medical_History_24"  "Medical_History_32" 
sum(is.na(train$Family_Hist_5))
sum(is.na(train$Insurance_History_5))
sum(is.na(train$Employment_Info_1))

#pre-processing and cleaning data for continuous columns
train$Employment_Info_1[which(is.na(train$Employment_Info_1))] <- mean(train$Employment_Info_1, na.rm = TRUE)
sum(is.na(train$Employment_Info_1))

train$Employment_Info_4[which(is.na(train$Employment_Info_4))] <- mean(train$Employment_Info_4, na.rm = TRUE)
sum(is.na(train$Employment_Info_4))

train$Employment_Info_6[which(is.na(train$Employment_Info_6))] <- mean(train$Employment_Info_6, na.rm = TRUE)
sum(is.na(train$Employment_Info_6))

train$Employment_Info_5[which(is.na(train$Employment_Info_5))] <- mean(train$Employment_Info_5, na.rm = TRUE)
sum(is.na(train$Employment_Info_5))

train$Insurance_History_5[which(is.na(train$Insurance_History_5))] <- mean(train$Insurance_History_5, na.rm = TRUE)
sum(is.na(train$Insurance_History_5))

train$Family_Hist_2[which(is.na(train$Family_Hist_2))] <- mean(train$Family_Hist_2, na.rm = TRUE)
sum(is.na(train$Family_Hist_2))

train$Family_Hist_3[which(is.na(train$Family_Hist_3))] <- mean(train$Family_Hist_3, na.rm = TRUE)
sum(is.na(train$Family_Hist_3))

train$Family_Hist_4[which(is.na(train$Family_Hist_4))] <- mean(train$Family_Hist_4, na.rm = TRUE)
sum(is.na(train$Family_Hist_4))

train$Family_Hist_5[which(is.na(train$Family_Hist_5))] <- mean(train$Family_Hist_5, na.rm = TRUE)
sum(is.na(train$Family_Hist_5))

#pre-processing and cleaning data for discrete columns

#checking min, max, mean of some discrete columns 
min(train$Medical_History_1, na.rm = TRUE)
max(train$Medical_History_1, na.rm = TRUE)
mean(train$Medical_History_1, na.rm = TRUE)
mean(train$Medical_History_15, na.rm = TRUE)

sum(is.na(train$Medical_History_1))
table(train$Medical_History_1)
train$Medical_History_1[which(is.na(train$Medical_History_1))] <- ceiling(mean(train$Medical_History_1, na.rm = TRUE))
hist(train$Medical_History_1)

sum(is.na(train$Medical_History_15))
table(train$Medical_History_15)
train$Medical_History_15[which(is.na(train$Medical_History_15))] <- ceiling(mean(train$Medical_History_15, na.rm = TRUE))
hist(train$Medical_History_15)

#eliminating discrete columns having more than 90% missing values more
sum(is.na(train$Medical_History_10))
table(train$Medical_History_10)
train$Medical_History_10 <- NULL
dim(train)

sum(is.na(train$Medical_History_24))
table(train$Medical_History_24)
train$Medical_History_24 <- NULL
dim(train)

sum(is.na(train$Medical_History_32))
table(train$Medical_History_32)
train$Medical_History_32 <- NULL
dim(train)

#pre-processing and cleaning data for categorical columns

#eliminating categorical columns with many categories
unique(train$Product_Info_3)
train$Product_Info_3 <- NULL
dim(train)

unique(train$Medical_History_2)
train$Medical_History_2 <- NULL
dim(train)

unique(train$Employment_Info_2)
train$Employment_Info_2 <- NULL
dim(train)

unique(train$InsuredInfo_3)
train$InsuredInfo_3 <- NULL
dim(train)

#dummy variables coding for all categorical columns

#first exploring the train dataset
head(train)
tail(train)
dim(train)
str(train)

install.packages("dplyr")
##select the rows which test data and train data both have 
train=dplyr::filter(train, Product_Info_7!=2)
train=dplyr::filter(train, Insurance_History_3!=2)
train=dplyr::filter(train, Medical_History_5!=3)
train=dplyr::filter(train, Medical_History_6!=2)
train=dplyr::filter(train, Medical_History_9!=3)
train=dplyr::filter(train, Medical_History_12!=1)
train=dplyr::filter(train, Medical_History_16!=2)
train=dplyr::filter(train, Medical_History_17!=1)
train=dplyr::filter(train, Medical_History_23!=2)
train=dplyr::filter(train, Medical_History_31!=2)
train=dplyr::filter(train, Medical_History_37!=3)
train=dplyr::filter(train, Medical_History_41!=2)

##get the data with binary values 
trainbinary <- c("Product_Info_7", "Product_Info_6", "Product_Info_5", "Product_Info_1", "InsuredInfo_2", 
                 "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Employment_Info_3",
                 "Employment_Info_5", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", 
                 "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_9", 
                 "Medical_History_12", "Medical_History_16", 
                 "Medical_History_17", "Medical_History_22", "Medical_History_23", "Medical_History_31", 
                 "Medical_History_37",
                 "Medical_History_41", 
                 paste("Medical_Keyword_", 1:48, sep=""))


# convert value to 0 or 1 in trainbinary columns 
for (f in trainbinary) {
  levels <- unique(c(train[[f]], test[[f]]))
  train[[f]] <- as.integer(factor(train[[f]], levels=levels))-1
  #test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))-1
}

#column 2
#unique(train$Product_Info_1)
#train$Product_Info_1_1 <- as.numeric(train$Product_Info_1 == 1)
#train$Product_Info_1_2 <- as.numeric(train$Product_Info_1 == 2)
#train$Product_Info_1 <- NULL

#column 3

unique(train$Product_Info_2)
train$Product_Info_2_A1 <- as.numeric(train$Product_Info_2 == "A1")
train$Product_Info_2_A2 <- as.numeric(train$Product_Info_2 == "A2")
train$Product_Info_2_A3 <- as.numeric(train$Product_Info_2 == "A3")
train$Product_Info_2_A4 <- as.numeric(train$Product_Info_2 == "A4")
train$Product_Info_2_A5 <- as.numeric(train$Product_Info_2 == "A5")
train$Product_Info_2_A6 <- as.numeric(train$Product_Info_2 == "A6")
train$Product_Info_2_A7 <- as.numeric(train$Product_Info_2 == "A7")
train$Product_Info_2_A8 <- as.numeric(train$Product_Info_2 == "A8")
train$Product_Info_2_B1 <- as.numeric(train$Product_Info_2 == "B1")
train$Product_Info_2_B2 <- as.numeric(train$Product_Info_2 == "B2")
train$Product_Info_2_C1 <- as.numeric(train$Product_Info_2 == "C1")
train$Product_Info_2_C2 <- as.numeric(train$Product_Info_2 == "C2")
train$Product_Info_2_C3 <- as.numeric(train$Product_Info_2 == "C3")
train$Product_Info_2_C4 <- as.numeric(train$Product_Info_2 == "C4")
train$Product_Info_2_D1 <- as.numeric(train$Product_Info_2 == "D1")
train$Product_Info_2_D2 <- as.numeric(train$Product_Info_2 == "D2")
train$Product_Info_2_D3 <- as.numeric(train$Product_Info_2 == "D3")
train$Product_Info_2_D4 <- as.numeric(train$Product_Info_2 == "D4")
train$Product_Info_2_E1 <- as.numeric(train$Product_Info_2 == "E1")
train$Product_Info_2 <- NULL

#column 5
train$Product_Info_4_C <- train$Product_Info_4
train$Product_Info_4 <- NULL


#column 13
train$Employment_Info_1_C <- train$Employment_Info_1
train$Employment_Info_1 <- NULL



#column 16
train$Employment_Info_4_C <- train$Employment_Info_4
train$Employment_Info_4 <- NULL



#column 18
train$Employment_Info_6_C <- train$Employment_Info_6
train$Employment_Info_6 <- NULL

#column 19
unique(train$InsuredInfo_1)
train$InsuredInfo_1_1 <- as.numeric(train$InsuredInfo_1 == 1)
train$InsuredInfo_1_2 <- as.numeric(train$InsuredInfo_1 == 2)
train$InsuredInfo_1_3 <- as.numeric(train$InsuredInfo_1 == 3)
train$InsuredInfo_1 <- NULL


#column 29
unique(train$Insurance_History_4)
train$Insurance_History_4_1 <- as.numeric(train$Insurance_History_4 == 1)
train$Insurance_History_4_2 <- as.numeric(train$Insurance_History_4 == 2)
train$Insurance_History_4_3 <- as.numeric(train$Insurance_History_4 == 3)
train$Insurance_History_4 <- NULL

#column 30
train$Insurance_History_5_C <- train$Insurance_History_5
train$Insurance_History_5 <- NULL
train$Insurance_History_5 
#column 31
unique(train$Insurance_History_7)
train$Insurance_History_7_1 <- as.numeric(train$Insurance_History_7 == 1)
train$Insurance_History_7_2 <- as.numeric(train$Insurance_History_7 == 2)
train$Insurance_History_7_3 <- as.numeric(train$Insurance_History_7 == 3)
train$Insurance_History_7 <- NULL

#column 32
unique(train$Insurance_History_8)
train$Insurance_History_8_1 <- as.numeric(train$Insurance_History_8 == 1)
train$Insurance_History_8_2 <- as.numeric(train$Insurance_History_8 == 2)
train$Insurance_History_8_3 <- as.numeric(train$Insurance_History_8 == 3)
train$Insurance_History_8 <- NULL

#column 33
unique(train$Insurance_History_9)
train$Insurance_History_9_1 <- as.numeric(train$Insurance_History_9 == 1)
train$Insurance_History_9_2 <- as.numeric(train$Insurance_History_9 == 2)
train$Insurance_History_9_3 <- as.numeric(train$Insurance_History_9 == 3)
train$Insurance_History_9 <- NULL

#column 34
unique(train$Family_Hist_1)
train$Family_Hist_1_1 <- as.numeric(train$Family_Hist_1 == 1)
train$Family_Hist_1_2 <- as.numeric(train$Family_Hist_1 == 2)
train$Family_Hist_1_3 <- as.numeric(train$Family_Hist_1 == 3)
train$Family_Hist_1 <- NULL

#column 35
train$Family_Hist_2_C <- train$Family_Hist_2
train$Family_Hist_2 <- NULL

#column 36
train$Family_Hist_3_C <- train$Family_Hist_3
train$Family_Hist_3 <- NULL

#column 37
train$Family_Hist_4_C <- train$Family_Hist_4
train$Family_Hist_4 <- NULL

#column 38
train$Family_Hist_5_C <- train$Family_Hist_5
train$Family_Hist_5 <- NULL

#column 39
train$Medical_History_1_D <- train$Medical_History_1
train$Medical_History_1 <- NULL

#column 41
unique(train$Medical_History_3)
train$Medical_History_3_1 <- as.numeric(train$Medical_History_3 == 1)
train$Medical_History_3_2 <- as.numeric(train$Medical_History_3 == 2)
train$Medical_History_3_3 <- as.numeric(train$Medical_History_3 == 3)
train$Medical_History_3 <- NULL



#column 45
unique(train$Medical_History_7)
train$Medical_History_7_1 <- as.numeric(train$Medical_History_7 == 1)
train$Medical_History_7_2 <- as.numeric(train$Medical_History_7 == 2)
train$Medical_History_7_3 <- as.numeric(train$Medical_History_7 == 3)
train$Medical_History_7 <- NULL

#column 46
unique(train$Medical_History_8)
train$Medical_History_8_1 <- as.numeric(train$Medical_History_8 == 1)
train$Medical_History_8_2 <- as.numeric(train$Medical_History_8 == 2)
train$Medical_History_8_3 <- as.numeric(train$Medical_History_8 == 3)
train$Medical_History_8 <- NULL


#column 49
unique(train$Medical_History_11)
train$Medical_History_11_1 <- as.numeric(train$Medical_History_11 == 1)
train$Medical_History_11_2 <- as.numeric(train$Medical_History_11 == 2)
train$Medical_History_11_3 <- as.numeric(train$Medical_History_11 == 3)
train$Medical_History_11 <- NULL


#column 51
unique(train$Medical_History_13)
train$Medical_History_13_1 <- as.numeric(train$Medical_History_13 == 1)
train$Medical_History_13_2 <- as.numeric(train$Medical_History_13 == 2)
train$Medical_History_13_3 <- as.numeric(train$Medical_History_13 == 3)
train$Medical_History_13 <- NULL

#column 52
unique(train$Medical_History_14)
train$Medical_History_14_1 <- as.numeric(train$Medical_History_14 == 1)
train$Medical_History_14_2 <- as.numeric(train$Medical_History_14 == 2)
train$Medical_History_14_3 <- as.numeric(train$Medical_History_14 == 3)
train$Medical_History_14 <- NULL

#column 53
train$Medical_History_15_D <- train$Medical_History_15
train$Medical_History_15 <- NULL


#column 56
unique(train$Medical_History_18)
train$Medical_History_18_1 <- as.numeric(train$Medical_History_18 == 1)
train$Medical_History_18_2 <- as.numeric(train$Medical_History_18 == 2)
train$Medical_History_18_3 <- as.numeric(train$Medical_History_18 == 3)
train$Medical_History_18 <- NULL

#column 57
unique(train$Medical_History_19)
train$Medical_History_19_1 <- as.numeric(train$Medical_History_19 == 1)
train$Medical_History_19_2 <- as.numeric(train$Medical_History_19 == 2)
train$Medical_History_19_3 <- as.numeric(train$Medical_History_19 == 3)
train$Medical_History_19 <- NULL

#column 58
unique(train$Medical_History_20)
train$Medical_History_20_1 <- as.numeric(train$Medical_History_20 == 1)
train$Medical_History_20_2 <- as.numeric(train$Medical_History_20 == 2)
train$Medical_History_20_3 <- as.numeric(train$Medical_History_20 == 3)
train$Medical_History_20 <- NULL

#column 59
unique(train$Medical_History_21)
train$Medical_History_21_1 <- as.numeric(train$Medical_History_21 == 1)
train$Medical_History_21_2 <- as.numeric(train$Medical_History_21 == 2)
train$Medical_History_21_3 <- as.numeric(train$Medical_History_21 == 3)
train$Medical_History_21 <- NULL

#column 60
unique(train$Medical_History_22)
train$Medical_History_22_1 <- as.numeric(train$Medical_History_22 == 1)
train$Medical_History_22_2 <- as.numeric(train$Medical_History_22 == 2)
train$Medical_History_22 <- NULL

#column 63
unique(train$Medical_History_25)
train$Medical_History_25_1 <- as.numeric(train$Medical_History_25 == 1)
train$Medical_History_25_2 <- as.numeric(train$Medical_History_25 == 2)
train$Medical_History_25_3 <- as.numeric(train$Medical_History_25 == 3)
train$Medical_History_25 <- NULL

#column 64
unique(train$Medical_History_26)
train$Medical_History_26_1 <- as.numeric(train$Medical_History_26 == 1)
train$Medical_History_26_2 <- as.numeric(train$Medical_History_26 == 2)
train$Medical_History_26_3 <- as.numeric(train$Medical_History_26 == 3)
train$Medical_History_26 <- NULL

#column 65
unique(train$Medical_History_27)
train$Medical_History_27_1 <- as.numeric(train$Medical_History_27 == 1)
train$Medical_History_27_2 <- as.numeric(train$Medical_History_27 == 2)
train$Medical_History_27_3 <- as.numeric(train$Medical_History_27 == 3)
train$Medical_History_27 <- NULL

#column 66
unique(train$Medical_History_28)
train$Medical_History_28_1 <- as.numeric(train$Medical_History_28 == 1)
train$Medical_History_28_2 <- as.numeric(train$Medical_History_28 == 2)
train$Medical_History_28_3 <- as.numeric(train$Medical_History_28 == 3)
train$Medical_History_28 <- NULL

#column 67
unique(train$Medical_History_29)
train$Medical_History_29_1 <- as.numeric(train$Medical_History_29 == 1)
train$Medical_History_29_2 <- as.numeric(train$Medical_History_29 == 2)
train$Medical_History_29_3 <- as.numeric(train$Medical_History_29 == 3)
train$Medical_History_29 <- NULL

#column 68
unique(train$Medical_History_30)
train$Medical_History_30_1 <- as.numeric(train$Medical_History_30 == 1)
train$Medical_History_30_2 <- as.numeric(train$Medical_History_30 == 2)
train$Medical_History_30_3 <- as.numeric(train$Medical_History_30 == 3)
train$Medical_History_30 <- NULL


#column 71
unique(train$Medical_History_33)
train$Medical_History_33_1 <- as.numeric(train$Medical_History_33 == 1)
train$Medical_History_33_3 <- as.numeric(train$Medical_History_33 == 3)
train$Medical_History_33 <- NULL

#column 72
unique(train$Medical_History_34)
train$Medical_History_34_1 <- as.numeric(train$Medical_History_34 == 1)
train$Medical_History_34_2 <- as.numeric(train$Medical_History_34 == 2)
train$Medical_History_34_3 <- as.numeric(train$Medical_History_34 == 3)
train$Medical_History_34 <- NULL

#column 73
unique(train$Medical_History_35)
train$Medical_History_35_1 <- as.numeric(train$Medical_History_35 == 1)
train$Medical_History_35_2 <- as.numeric(train$Medical_History_35 == 2)
train$Medical_History_35_3 <- as.numeric(train$Medical_History_35 == 3)
train$Medical_History_35 <- NULL

#column 74
unique(train$Medical_History_36)
train$Medical_History_36_1 <- as.numeric(train$Medical_History_36 == 1)
train$Medical_History_36_2 <- as.numeric(train$Medical_History_36 == 2)
train$Medical_History_36_3 <- as.numeric(train$Medical_History_36 == 3)
train$Medical_History_36 <- NULL

#column 76
unique(train$Medical_History_38)
train$Medical_History_38_1 <- as.numeric(train$Medical_History_38 == 1)
train$Medical_History_38_2 <- as.numeric(train$Medical_History_38 == 2)
train$Medical_History_38 <- NULL

#column 77
unique(train$Medical_History_39)
train$Medical_History_39_1 <- as.numeric(train$Medical_History_39 == 1)
train$Medical_History_39_2 <- as.numeric(train$Medical_History_39 == 2)
train$Medical_History_39_3 <- as.numeric(train$Medical_History_39 == 3)
train$Medical_History_39 <- NULL

#column 78
unique(train$Medical_History_40)
train$Medical_History_40_1 <- as.numeric(train$Medical_History_40 == 1)
train$Medical_History_40_2 <- as.numeric(train$Medical_History_40 == 2)
train$Medical_History_40_3 <- as.numeric(train$Medical_History_40 == 3)
train$Medical_History_40 <- NULL


#normalizing the discrete columns
x <- train$Medical_History_1_D
normalized <- (x - min(x))/(max(x) - min(x))
train$Medical_History_1_D <- normalized

y <- train$Medical_History_15_D
normalized <- (y - min(y))/(max(y) - min(y))
train$Medical_History_15_D <- normalized

#exploring the train dataset again after several changes as done above
dim(train)
tail(train)
head(train)
summary(train)
str(train)
write.csv(train, file="train_cleanedfinal.csv")
getwd()
dim(train)
#dummy copy of train dataset
dummy <- train

#sample model
sample <- lm(train$Response~train$Product_Info_1_2, train)
summary(sample)

hist(sample$residuals)
anova(sample)
plot(train$Response, train$Ht)