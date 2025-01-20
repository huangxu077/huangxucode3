library(readxl)
library(xlsx)
library(caret)
library(AER)
library(ggplot2)
#huangxu Huangxu
setwd("D:/R_Project/Final_Project")

data1<-read_excel("Rdata.xlsx",sheet=1)
data1
#delete rows containing NA
data2<-na.omit(data1)
#delete ID row
data2<-data2[,-1]
data2
#变量解释
#变量中文名	变量名	            取值含义
#是否再婚	 remarried_ever	 0表示未再婚，1表示再婚
#性别	     male	           0表示女性，1表示男性
#户籍类型	 agriHukou	     0表示非农业户口，1表示农业户口
#年龄	     Age	          退休人口：女55以上，男60以上
#受教育程度	education	    受教育年限，在1-11年之间
#子女数目	 childnum	       >=0,表示一个人有几个子女  
#是否有同村/同社区居住的子女	childnear_liv	0表示没有，1表示有


#将数据分成训练集和测试集，分成不同比例方便查看效果
#  训练集采用probit和logit模型进行回归分析，然后再用测试集进行验证
#第1组: 训练集50% 测试集50%
train_index1 = createDataPartition(data2$remarried_ever, p = 0.50, list = FALSE)
# 训练集和测试集
train_set = data2[train_index1, ]
test_set = data2[-train_index1, ]
# 训练集样本出现百分比
percentage <- prop.table(table(train_set$remarried_ever)) * 100
# 查看各样本出现的次数和比例
cbind(freq = table(train_set$remarried_ever), percentage = percentage)
#probit模型回归分析

#训练集构建模型
probit1 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
                  family = binomial(link = "probit"), 
                  data = train_set)
summary(probit1)
# 测试集进行预测
predictions1 <- predict(probit1,newdata=test_set,type="response")
predictions<-as.numeric(predictions1)
testprobs1<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs1))

#logit模型回归分析
#训练集构建模型
logit1 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
               family = binomial(link = "logit"), 
               data = train_set)
summary(logit1)
# 测试集进行预测
predictions2 <- predict(logit1,newdata=test_set,type="response")
predictions<-as.numeric(predictions2)
testprobs2<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs2))

#第2组: 训练集60% 测试集40%
train_index2 = createDataPartition(data2$remarried_ever, p = 0.60, list = FALSE)
# 训练集和测试集
train_set = data2[train_index2, ]
test_set = data2[-train_index2, ]
# 训练集样本出现百分比
percentage <- prop.table(table(train_set$remarried_ever)) * 100
# 查看各样本出现的次数和比例
cbind(freq = table(train_set$remarried_ever), percentage = percentage)
#probit模型回归分析

#训练集构建模型
probit2 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
               family = binomial(link = "probit"), 
               data = train_set)
summary(probit2)
# 测试集进行预测
predictions3 <- predict(probit2,newdata=test_set,type="response")
predictions<-as.numeric(predictions3)
testprobs3<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs3))

#logit模型回归分析
#训练集构建模型
logit2 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
              family = binomial(link = "logit"), 
              data = train_set)
summary(logit2)
# 测试集进行预测
predictions4 <- predict(logit2,newdata=test_set,type="response")
predictions<-as.numeric(predictions4)
testprobs4<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs4))

#第3组: 训练集70% 测试集30%
train_index3 = createDataPartition(data2$remarried_ever, p = 0.70, list = FALSE)
# 训练集和测试集
train_set = data2[train_index3, ]
test_set = data2[-train_index3, ]
# 训练集样本出现百分比
percentage <- prop.table(table(train_set$remarried_ever)) * 100
# 查看各样本出现的次数和比例
cbind(freq = table(train_set$remarried_ever), percentage = percentage)
#probit模型回归分析

#训练集构建模型
probit3 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
               family = binomial(link = "probit"), 
               data = train_set)
summary(probit3)
# 测试集进行预测
predictions5 <- predict(probit3,newdata=test_set,type="response")
predictions<-as.numeric(predictions5)
testprobs5<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs5))

#logit模型回归分析
#训练集构建模型
logit3 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
              family = binomial(link = "logit"), 
              data = train_set)
summary(logit3)
# 测试集进行预测
predictions6 <- predict(logit3,newdata=test_set,type="response")
predictions<-as.numeric(predictions6)
testprobs6<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs6))

#第4组: 训练集80% 测试集20%
train_index4 = createDataPartition(data2$remarried_ever, p = 0.80, list = FALSE)
# 训练集和测试集
train_set = data2[train_index4, ]
test_set = data2[-train_index4, ]
# 训练集样本出现百分比
percentage <- prop.table(table(train_set$remarried_ever)) * 100
# 查看各样本出现的次数和比例
cbind(freq = table(train_set$remarried_ever), percentage = percentage)
#probit模型回归分析

#训练集构建模型
probit4 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
               family = binomial(link = "probit"), 
               data = train_set)
summary(probit4)
# 测试集进行预测
predictions7 <- predict(probit4,newdata=test_set,type="response")
predictions<-as.numeric(predictions7)
testprobs7<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs7))

#logit模型回归分析
#训练集构建模型
logit4 <- glm(remarried_ever ~ male+agriHukou+Age+education+childnum+childnear_liv, 
              family = binomial(link = "logit"), 
              data = train_set)
summary(logit4)
# 测试集进行预测
predictions8 <- predict(logit4,newdata=test_set,type="response")
predictions<-as.numeric(predictions8)
testprobs8<-ifelse(predictions>=0.5,1,0)
confusionMatrix(factor(test_set$remarried_ever),factor(testprobs8))


