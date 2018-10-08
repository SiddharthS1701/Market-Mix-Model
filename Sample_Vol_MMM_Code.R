#--- This code is for performing market mix modeling for growplan volume sales ---#

#--- environment set up and data load steps ---#

## setting the working directory location to location where input files are stored
setwd('C:/Users/sidshriv2/Desktop/Projects/01.Delivery/04.wescafe & growplan/14.Project/input')
getwd()

## load the required libraries
library(ggplot2) # visualization
library(data.table) # data wrangling
library(dplyr) # data wrangling
library(scales)
library(gridExtra)
library(ggcorrplot) # visualization for correlation matrix

#--- ingesting the base datasets required for the model and storing the combined one ---#

## parameters to enter the names of the files to be read - spends, volume and model dataset ##
spends_dataset <- 'growplan_media_spends.csv' ## dataset containing media spends
volume_dataset <- 'growplan_volume_data.csv' ## dataset containing sales and distribution information
model_dataset<-  'model_final.csv' ## base dataset to build the first cut of the model

## reading the datasets into R using the above specified names  
spends_data <- fread(spends_dataset)
sales_data <- fread(volume_dataset)
model_base <- fread(model_dataset)

## joining the spends and volume dataset together to form a single dataset
full_data <- inner_join(sales_data, spends_data, by = 'time_period')
head(full_data)

## storing the full dataset back in the working directory after merging the two
write.csv(full_data, 'model_dataset_combined.csv')

## remove certain columns from the model dataset before using the regression function
model_data1 <- select(model_base, -year, -month, -time_period, -avg_price_tm_r_u, -mc_tv_spends_r)

#--- Feature engineering - Adding new_variables ---#

## tv - adstock variants
### tv - adstock - Urban
model_data1$muht_tv_spends_u_adstock_30 <- stats::filter(x=model_data1$muht_tv_spends_u, filter = 0.30, method = 'recursive')
model_data1$muht_tv_spends_u_adstock_40 <- stats::filter(x=model_data1$muht_tv_spends_u, filter = 0.40, method = 'recursive')
model_data1$muht_tv_spends_u_adstock_50 <- stats::filter(x=model_data1$muht_tv_spends_u, filter = 0.50, method = 'recursive')
model_data1$muht_tv_spends_u_30_x5 <- (model_data1$muht_tv_spends_u_adstock_30)^0.5
model_data1$muht_tv_spends_u_40_x5 <- (model_data1$muht_tv_spends_u_adstock_40)^0.5
model_data1$muht_tv_spends_u_50_x5 <- (model_data1$muht_tv_spends_u_adstock_50)^0.5

### tv - adstock - Rural
model_data1$muht_tv_spends_r_adstock_30 <- stats::filter(x=model_data1$muht_tv_spends_r, filter = 0.30, method = 'recursive')
model_data1$muht_tv_spends_r_adstock_40 <- stats::filter(x=model_data1$muht_tv_spends_r, filter = 0.40, method = 'recursive')
model_data1$muht_tv_spends_r_adstock_50 <- stats::filter(x=model_data1$muht_tv_spends_r, filter = 0.50, method = 'recursive')
model_data1$muht_tv_spends_r_30_x5 <- (model_data1$muht_tv_spends_r_adstock_30)^0.5
model_data1$muht_tv_spends_r_40_x5 <- (model_data1$muht_tv_spends_r_adstock_40)^0.5
model_data1$muht_tv_spends_r_50_x5 <- (model_data1$muht_tv_spends_r_adstock_50)^0.5

#--- Compute correlation between all the continuous variables ---#

## use this option for casewise deletion to deal with missing values
correlation_result <- round(cor(model_data1, use = "complete.obs"), 2) # round to 2 decimal places
head(correlation_result[,1:6])

## visualize the correlation matrix using ggcorrplot package
## method = 'square' (default)
ggcorrplot(correlation_result)

## alternate method
## correlation_result <- cor(model_data1, use = "pairwise") 

## store the output of the correlation analysis in excel file
cor_result <- as.data.frame(correlation_result)

## exporting the correlation matrix
write.csv(cor_result, file = 'C:/Users/sidshriv2/Desktop/Projects/01.Delivery/04.wescafe & growplan/14.Project/working datasets/correlation_results.csv')

#--- baseline model for volume sales ---#
## volume sales model using the lm function
model_baseline <- lm(vol_sales_tm_r_u  ~ dl_ba + growplan_ba + ov_ba + muht_fb_ads_impressions  + mp_tv_norm_grps_u , data = model_data1)
summary(model_baseline)

#--- regression model diagnostics and plots ---#

### controling the plot area to 2X2 and creating all important plots
par(mfrow = c(2,2))
plot(model_baseline)

### outlier test for the final set of variables
outlierTest(model_baseline)

### Q-Q plots
qqplot(model_baseline, main='QQ Plot')

### residuals vs Leverage plots
leveragePlots(model_baseline)

### check for multi-collinearity using VIF
vif(model_baseline)
sqrt(vif(model_baseline))

### added-variable plots
av.Plots(model_baseline)

### components + residual plots
crPlots(model_baseline)

### ceres plots
ceresPlots(model_baseline)

