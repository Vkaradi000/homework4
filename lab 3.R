Victoria Karadimas 

Lab 3

Safinaz and Adeel
  

[Workspace loaded from ~/.RData]

load("/Users/victoria/Desktop/B2000/acs2017_ny/acs2017_ny_data.RData")
load("~/.RData")
View(acs2017_ny)
dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
attach(dat_NYC)
borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))
norm_varb <- function(X_in) {
  +     (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
  + }
is.na(OWNCOST) <- which(OWNCOST == 9999999) # that's how data codes NA values
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)
data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(borough_f,good_obs_data_use)
set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
summary(cl_data)
Bronx     Manhattan Staten Island      Brooklyn        Queens 
4880          5250          1891         12416         10923 
prop.table(summary(cl_data))
Bronx     Manhattan Staten Island      Brooklyn        Queens 
0.13800905    0.14847285    0.05347851    0.35113122    0.30890837 
summary(train_data)
norm_inc_tot     norm_housing_cost
Min.   :0.00000   Min.   :0.00000  
1st Qu.:0.01191   1st Qu.:0.02493  
Median :0.02693   Median :0.96917  
Mean   :0.04265   Mean   :0.58972  
3rd Qu.:0.05219   3rd Qu.:0.97784  
Max.   :1.00000   Max.   :1.00000  
require(class)
Loading required package: class
for (indx in seq(1, 9, by= 2)) {
  +     pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  +     num_correct_labels <- sum(pred_borough == true_data)
  +     correct_rate <- num_correct_labels/length(true_data)
  +     print(c(indx,correct_rate))
  + }
[1] 1.0000000 0.3540087
[1] 3.0000000 0.3437859
[1] 5.0000000 0.3550425
[1] 7.0000000 0.3708936
[1] 9.0000000 0.3721571
cl_data_n <- as.numeric(cl_data)

model_ols1 <- lm(cl_data_n ~ train_data$norm_inc_tot + train_data$norm_housing_cost)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 1])
[1] 3.476403
> mean(y_hat[cl_data_n == 2])
[1] 3.375686
> mean(y_hat[cl_data_n == 3])
[1] 3.753125
> mean(y_hat[cl_data_n == 4])
[1] 3.541435
> mean(y_hat[cl_data_n == 5])
[1] 3.62329


 cl_data_n1 <- as.numeric(cl_data_n == 1)
 model_ols_v1 <- lm(cl_data_n1 ~ train_data$norm_inc_tot + train_data$norm_housing_cost)
 y_hat_v1 <- fitted.values(model_ols_v1)
 mean(y_hat_v1[cl_data_n1 == 1])
[1] 0.1592435
 mean(y_hat_v1[cl_data_n1 == 0])
[1] 0.1346093

is.na(OWNCOST) <- which(OWNCOST == 9999999) 
housing_cost <- OWNCOST + RENT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(housing_cost)

My Group decided to compare the amount of bedrooms to income 


is.na( INCTOT) <- which( INCTOT == 9999999) 
housing_cost <- ROOMS + INCTOT
norm_inc_tot <- norm_varb(INCTOT)
norm_housing_cost <- norm_varb(ROOMS)

data_use_prelim <- data.frame(norm_inc_tot,norm_housing_cost)
good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(ROOMS,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   3.000   4.000   4.762   6.000  16.000 
prop.table(summary(cl_data))
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00000 0.08886 0.11848 0.14104 0.17772 0.47391 
summary(train_data)
norm_inc_tot     norm_housing_cost
Min.   :0.00000   Min.   :0.0000   
1st Qu.:0.01191   1st Qu.:0.1875   
Median :0.02693   Median :0.2500   
Mean   :0.04265   Mean   :0.2976   
3rd Qu.:0.05219   3rd Qu.:0.3750   
Max.   :1.00000   Max.   :1.0000  

