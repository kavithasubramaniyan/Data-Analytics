_# %% [code]
#rm(list=ls())#Clears workspace

#Reads source file from current working directory
source_file=as.data.frame(read.csv('../input/house-prices-advanced-regression-techniques/train.csv',header=TRUE,sep=','))


###Model Build Process started

#Identifying categorical variables

library(tidyverse)
categorical=source_file %>% select_if(negate(is.numeric))
#Collecting number columns
number_cols=source_file[!(source_file %in% categorical)]
#Taking age of property and renovated age of property
Age_house=source_file$YrSold-source_file$YearBuilt
Renovated_age=source_file$YrSold-source_file$YearRemodAdd
#Deleting the below columns as we are going to cbind the calculated property age
number_cols$YearBuilt=NULL
number_cols$YearRemodAdd=NULL
number_cols$YrSold=NULL
number_cols$MoSold=NULL
number_cols=cbind(number_cols,Age_house,Renovated_age)#Attaching these two new columns with the number columns file

cat_colnames=colnames(categorical)
#Identifying how many levels are present in categorical-so that if the column has less than 2 levels,we can delete it(Meaning,it has more unique values wihch is not helpful for prediction)
for (i in cat_colnames)
{
  
  i=noquote(i)
  n_occur <- data.frame(table(categorical[i]))
  
  if(nrow(n_occur)<=2)
  {
    categorical[i]=NULL
    
  }
  else
  {
    cat(i,"Retaining the column")
  }
}
#Finding columns which has missing values in categorical variables
na=as.data.frame(colSums(is.na(categorical)))
colnames(na)=c('NA_Count')

na$names=NULL#Deleting the column
na$Names <- rownames(na)#Taking the names
#Taking columns which has missing values(count>0)
na_columns=na[na$NA_Count>0,]
#Replacing missing values with most frequent ones-categorical columns
file_rec=nrow(source_file)#Taking the length of source file
percent=(25/100)*file_rec#calculating percentage to make it as threshold for missing values present in each column
i=1
while(i<=nrow(na_columns)){
  if(na_columns$NA_Count[i]>percent){#Checking the na count for each column
    name=na_columns$Names[i]
    print(name)
    source_file[[name]]=NULL#If na count is greater than percent value,then data is deleted
    categorical[[name]]=NULL
    print("Data has been deleted")
  }
  else{
    name=na_columns$Names[i]#if na count is less than percent value,then data is replaced
    print(name)
    print("Value is replaced")
    agg_name=aggregate(source_file[[name]], by=list(source_file[[name]]), FUN=length)#Taking aggregate
    replace=as.character(agg_name$Group.1[agg_name$x==max(agg_name$x)])#Replacing the missing value with max value
    source_file[[name]][is.na(source_file[[name]])]<-replace
    agg_name=aggregate(categorical[[name]], by=list(categorical[[name]]), FUN=length)#Replacing the missing value in categorical dataframe
    replace=as.character(agg_name$Group.1[agg_name$x==max(agg_name$x)])
    categorical[[name]][is.na(categorical[[name]])]<-replace
  }
  i=i+1
}



#Taking mean values and replacing the missing values-numeric columns

num=as.data.frame(colSums(is.na(number_cols)))
colnames(num)=c('NA_Count')

num$names=NULL
num$Names <- rownames(num)
#Taking columns which has missing values(count>0)
na_columns1=num[num$NA_Count>0,]
#Replacing missing values with mean of the column
file_rec=nrow(source_file)#Taking length of source file
percent=(25/100)*file_rec#Calculating percentage to make it as threshold for missing values present in each column
i=1
while(i<=nrow(na_columns1)){
  if(na_columns1$NA_Count[i]>percent){#Checking the na count for each column
    name=na_columns1$Names[i]
    print(name)
    source_file[[name]]=NULL#If na count is greater than percent value,then data is deleted
    number_cols[[name]]=NULL
    print("Data has been deleted")
  }
  else{
    name=na_columns1$Names[i]#if na count is less than percent value,data is replaced
    print(name)
    mm=as.integer(mean(source_file[[name]],na.rm=TRUE))#Taking mean
    source_file[[name]][is.na(source_file[[name]])]=mm#Replacing source file columns with mean
    nn=as.integer(mean(number_cols[[name]],na.rm=TRUE))#Taking mean
    number_cols[[name]][is.na(number_cols[[name]])]=nn#Replacing number_cols dataframe with mean
    print("Value is replaced")
  }
  i=i+1
}
corr=cor(number_cols)
#install.packages("corrplot")
library(corrplot)
corrplot(corr, method="circle")
#Converting categorical to numerical ones
dataf<- data.frame(sapply(categorical,function(x) data.frame(model.matrix(~x-1,data =categorical))[,-1]))
#Combining number and categorical columns
final_df=cbind(number_cols,dataf)
final_df$Id=NULL#Removing id column as it is not necessary
#calling file which does preprocessing of test data
#Reads source file from current working directory
source_file=as.data.frame(read.csv('../input/house-prices-advanced-regression-techniques/test.csv',header=TRUE,sep=','))


###Model Build Process started

#Identifying categorical variables


library(tidyverse)
categorical=source_file %>% select_if(negate(is.numeric))
#Collecting number columns
number_cols=source_file[!(source_file %in% categorical)]
#Taking age of property and renovated age of property
Age_house=source_file$YrSold-source_file$YearBuilt
Renovated_age=source_file$YrSold-source_file$YearRemodAdd
#Deleting the below columns as we are going to cbind the calculated property age
number_cols$YearBuilt=NULL
number_cols$YearRemodAdd=NULL
number_cols$YrSold=NULL
number_cols$MoSold=NULL
number_cols=cbind(number_cols,Age_house,Renovated_age)#Attaching these two new columns with the number columns file

cat_colnames=colnames(categorical)
#Identifying if duplicated values are present in categorical
for (i in cat_colnames)
{
  
  i=noquote(i)
  
  n_occur <- data.frame(table(categorical[i]))
  
  if(nrow(n_occur)<=2)
  {
    categorical[i]=NULL
    
  }
  else
  {
    print("Retaining the column")
  }
}
#Finding columns which has missing values in categorical variables
na=as.data.frame(colSums(is.na(categorical)))
colnames(na)=c('NA_Count')

na$names=NULL#Deleting the column
na$Names <- rownames(na)#Taking the names
#Taking columns which has missing values(count>0)
na_columns=na[na$NA_Count>0,]
#Replacing missing values with most frequent ones-categorical columns
file_rec=nrow(source_file)#Taking the length of source file
percent=(25/100)*file_rec#calculating percentage to make it as threshold for missing values present in each column
i=1
while(i<=nrow(na_columns)){
  if(na_columns$NA_Count[i]>percent){#Checking the na count for each column
    name=na_columns$Names[i]
    print(name)
    source_file[[name]]=NULL#If na count is greater than percent value,then data is deleted
    categorical[[name]]=NULL
    print("Data has been deleted")
  }
  else{
    name=na_columns$Names[i]#if na count is less than percent value,then data is replaced
    print(name)
    print("Value is replaced")
    agg_name=aggregate(source_file[[name]], by=list(source_file[[name]]), FUN=length)#Taking aggregate
    replace=as.character(agg_name$Group.1[agg_name$x==max(agg_name$x)])#Replacing the missing value with max value
    source_file[[name]][is.na(source_file[[name]])]<-replace
    agg_name=aggregate(categorical[[name]], by=list(categorical[[name]]), FUN=length)#Replacing the missing value in categorical dataframe
    replace=as.character(agg_name$Group.1[agg_name$x==max(agg_name$x)])
    categorical[[name]][is.na(categorical[[name]])]<-replace
  }
  i=i+1
}



#Taking mean values and replacing the missing values-numeric columns

num=as.data.frame(colSums(is.na(number_cols)))
colnames(num)=c('NA_Count')

num$names=NULL
num$Names <- rownames(num)
#Taking columns which has missing values(count>0)
na_columns1=num[num$NA_Count>0,]
#Replacing missing values with mean of the column
file_rec=nrow(source_file)#Taking length of source file
percent=(25/100)*file_rec#Calculating percentage to make it as threshold for missing values present in each column
i=1
while(i<=nrow(na_columns1)){
  if(na_columns1$NA_Count[i]>percent){#Checking the na count for each column
    name=na_columns1$Names[i]
    print(name)
    source_file[[name]]=NULL#If na count is greater than percent value,then data is deleted
    number_cols[[name]]=NULL
    print("Data has been deleted")
  }
  else{
    name=na_columns1$Names[i]#if na count is less than percent value,data is replaced
    print(name)
    mm=as.integer(mean(source_file[[name]],na.rm=TRUE))#Taking mean
    source_file[[name]][is.na(source_file[[name]])]=mm#Replacing source file columns with mean
    nn=as.integer(mean(number_cols[[name]],na.rm=TRUE))#Taking mean
    number_cols[[name]][is.na(number_cols[[name]])]=nn#Replacing number_cols dataframe with mean
    print("Value is replaced")
  }
  i=i+1
}
corr=cor(number_cols)
#install.packages("corrplot")
library(corrplot)
corrplot(corr, method="circle")
#Converting categorical to numerical ones
dataf<- data.frame(sapply(categorical,function(x) data.frame(model.matrix(~x-1,data =categorical))[,-1]))
final_df1=cbind(number_cols,dataf)

#Building the first model

#Removing the columns which show 'NA' in the regression output

j = 1
repeat {
  relation <- lm(final_df$SalePrice ~ ., data = final_df)#build model
  relation_sum <- summary(relation)  # model summary
  pvalues <- relation_sum[[4]][, 4][-1] #Excluded na ones
  nn = as.data.frame(names(pvalues))
  
  full_vars1 <- as.data.frame(names(relation[[1]])[-1])#Full list
  cat("full_vars1-rows:", nrow(full_vars1), "\n")
  if (nrow(nn) != nrow(full_vars1))#if model column names and actual final_df column names are not equal,then 'NA' is present in summary of model(as a result of multicollinearity)
  {
    
    j = j + 1
    number_cols1 = as.character(full_vars1$`names(relation[[1]])[-1]`[!(full_vars1$`names(relation[[1]])[-1]` %in% nn$`names(pvalues)`)])
    
    
    row_num = length(number_cols1)
    
    i = 1
    while (row_num != 0) {
      not_sign_val = number_cols1[i]
      final_df[[not_sign_val]] = NULL
     
      row_num = row_num - 1
      i = i + 1
      
    }
  }
  else{
    break
  }
}

#install.packages('caTools')
library(caTools)

library(car)
#Taking the vif for all columns built in the 2nd model
vifs=vif(relation)
all <- names(vifs)


# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(vifs > 4)){
  max_vif <- names(which(vifs == max(vifs)))  # get the var with max vif
  all <- all[!(all) %in% max_vif]  # remove
  formula <- as.formula(paste("SalePrice ~ ", paste (all, collapse=" + "), sep=""))  # new formula
  final_model <- lm(formula, data=final_df)  # re-build model with new formula
  vifs <- car::vif(final_model)
}
summary(final_model)

#After removing multicollinearity columns,build the model
full_vars <- names(final_model[[1]])[-1]  # names of all X variables
model3_df=subset(final_df ,select= full_vars)
relation_full=lm(final_df$SalePrice~.,data=model3_df)
full_vars1 <- names(relation_full[[1]])[-1]
# Get the non-significant vars
relation_sum <- summary(relation_full)  # model summary
pvalues <- relation_sum[[4]][, 4]  # get all p values
not_sign_val=pvalues[which(pvalues==max(pvalues))]#Get max p-value
# If there are any non-significant variables, 
while(not_sign_val >= 0.05){
  model3_df[names(not_sign_val)]=NULL
  temp_model=lm(final_df$SalePrice~.,data=model3_df)#Build model
  temp_model_summ=summary(temp_model)#Summary
  temp_pvalues <- temp_model_summ[[4]][, 4]#pvalues
  not_sign_val=temp_pvalues[which(temp_pvalues==max(temp_pvalues))]#Take max value
  
}
summary(temp_model)



res=rstandard(temp_model)#Residuals check
plot(fitted(temp_model),res,main="Predicted vs residual plot")#Residual plot
abline(a=0,b=0,col='red')

#Taking qq plot
qqnorm(res)#QQ-Plot
qqline(res,col=4)#QQ-plot

#Identify influential points and remove it
plot(cooks.distance(temp_model))#plot cooks distance
cook=cooks.distance(temp_model)#Take values of cook distance
inf_points=4/(nrow(model3_df))#taking influential points
position1=cook>inf_points#taking values
modf=model3_df[!position1,]#Removing the observations greater than threshold

#full_train=train$SalePrice
mod_train=final_df[!position1,]

#Removing columns which show NA value
j = 1
repeat {
  relation <- lm(mod_train$SalePrice~.,data=modf)#build model
  relation_sum <- summary(relation)  # model summary
  pvalues <- relation_sum[[4]][, 4][-1] #Excluded na ones
  nn = as.data.frame(names(pvalues))
 # print(nrow(nn))
  
  full_vars1 <- as.data.frame(names(relation[[1]])[-1])#Full list
  
  cat("full_vars1-rows:", nrow(full_vars1), "\n")
  if (nrow(nn) != nrow(full_vars1))#if model column names and actual final_df column names are not equal,then 'NA' is present in summary of model(as a result of multicollinearity)
  {
    
    j = j + 1
    number_cols1 = as.character(full_vars1$`names(relation[[1]])[-1]`[!(full_vars1$`names(relation[[1]])[-1]` %in% nn$`names(pvalues)`)])
    
    
    row_num = length(number_cols1)
    print(row_num)
    i = 1
    while (row_num != 0) {
      not_sign_val = number_cols1[i]
      modf[[not_sign_val]] = NULL
      
      row_num = row_num - 1
      i = i + 1
      
    }
  }
  else{
    break
  }
}


full_vars2 <- names(relation[[1]])[-1]#Extracting column names
# Get the non-significant vars
relation_sum2 <- summary(relation)  # model summary
pvalues2 <- relation_sum2[[4]][, 4]  # get all p values
not_sign_val1=pvalues2[which(pvalues2==max(pvalues2))]#Taking max of pvalues
# If there are any non-significant variables, 
while(not_sign_val1 >= 0.05){
  
  modf[names(not_sign_val1)]=NULL#Deleting the column
  
  temp_model1=lm(mod_train$SalePrice~.,data=modf)#Building the model
  temp_model_summ1=summary(temp_model1)#Summary of model
  temp_pvalues1 <- temp_model_summ1[[4]][, 4]#taking p-values
  not_sign_val1=temp_pvalues1[which(temp_pvalues1==max(temp_pvalues1))]#Taking max of pvalues
  
}


#Doing the final round
full_vars2 <- names(temp_model1[[1]])[-1]
#final_df1 variable is present in House_salePrice_test_data_separate.R
final_df_names=colnames(final_df1)#Considering the output of test_data file preprocessing
#Comparing the column names of model and final_df1
#If the column exists in model and not in final_df1,then add that column to final_df1 with value as 0
for( i in full_vars2)
{
  if(i %in% final_df_names)
  {
    #print(i)
    cat('colname:',i,'is present',"\n")
  }
  else{
    final_df1[i]=0
    cat('colname:',i,'is not present',"\n")
  }
}

#Prediction on final_df1 on temp_model
predicted4=as.data.frame(predict.glm(temp_model1,final_df1))
names(predicted4)[1]='Price'
sale_price_pred <- data.frame(ID = final_df1$Id,
                              SalePrice = predicted4$Price)

write.csv(sale_price_pred,'Kavitha_Submissions.csv',row.names = FALSE)

#2nd way-Stepwise Model
relation2 <- lm(final_df$SalePrice ~ ., data = final_df)#Building model
temp_model3=step(relation2,direction='both',trace=T)#stepwise regression

plot(cooks.distance(temp_model3))#Plot cooks distance
cook=cooks.distance(temp_model3)#take values from cook distance
inf_points=4/(nrow(final_df))#take influential points
position1=cook>inf_points#Identify cook greater than influential points
modf=final_df[!position1,]#Delete the observations


relation3 <- lm(modf$SalePrice ~ ., data = modf)#Rebuild model
temp_model4=step(relation3,direction='both',trace=T)#stepwise model

full_vars2 <- names(temp_model4[[1]])[-1]
#final_df1 variable is present in House_salePrice_test_data_separate.R
final_df_names=colnames(final_df1)#Considering the output of test_data file preprocessing
#Comparing the column names of model and final_df1
#If the column exists in model and not in final_df1,then add that column to final_df1 with value as 0
for( i in full_vars2)
{
  if(i %in% final_df_names)
  {
    #print(i)
    cat('colname:',i,'is present',"\n")
  }
  else{
    final_df1[i]=0
    cat('colname:',i,'is not present',"\n")
  }
}



#Prediction on final_df1 on temp_model
predicted4=as.data.frame(predict.glm(temp_model4,final_df1))
names(predicted4)[1]='Price'
sale_price_pred <- data.frame(ID = final_df1$Id,
                              SalePrice = predicted4$Price)

write.csv(sale_price_pred,'Kavitha_Submissions1.csv',row.names = FALSE)



