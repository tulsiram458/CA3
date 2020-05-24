weather_df <- read.csv("dataset.csv", header = TRUE)
head(weather_df)

class(weather_df)

str(weather_df)


#data cleaning

#blanks to mean value for one column
# column1[is.na(column1)] <- round(mean(column1,na.rm = TRUE))

#missing values to mean for one column
# na2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
# replace(DF, TRUE, lapply(DF, na2mean))            


na2mean <- function(clamt) replace(clamt, is.na(clamt), mean(clamt, na.rm = TRUE))
#na2mean


 abc <- replace(weather_df, TRUE, lapply(weather_df, na2mean))            
abc <- na.omit(abc)
View(abc)

#weather_df[!complete.cases(weather_df),]

#finding mean value to replace
mean <- mean(is.na(abc$clamt))
mean

#abcd <- replace(weather_df, TRUE, lapply(weather_df,mean))


#library(dplyr)

#weather_df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
#---------------------------------------------------------------------------------------------------------

#identifying missing values

library(mice)

#md.pattern(weather_df)

library(VIM)
missing <- aggr(abc, prop = FALSE, numbers = TRUE)
summary(missing)

#matrixplot(weather_df)



# --------------------------------------------------------------------------------------------------------
  
#Co-relation
library(ggplot2) 
hist(abc$temp, main = "histogram for temperature", xlab = "temperature")
plot <- ggplot(abc, aes(x = sun, y = temp))
plot <- plot + stat_smooth(method = "lm", col = "green", se = FALSE)
plot <- plot + geom_point()
#print(plot)

#subset numeric data
abc <- subset(abc, select=c(rain,temp,wetb,dewpt,vappr,rhum,msl,wdsp,wddir))
colnames(abc)
str(abc)

#--------------------------------------------------------------------------------------------------------
#PCA

pca <- prcomp(abc, center = TRUE, scale. = TRUE)
#summary(pca)

str(pca)

library("factoextra")
eig_values <- get_eigenvalue(pca)
eig_values

library("FactoMineR")
pca2 <- PCA(abc, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
corrplot(pca_for_variables$cos2, is.corr = FALSE)

fviz_pca_var(pca, col.var = "black")

head(pca_for_variables$cos2, 10)

fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"), 
             repel = TRUE # Avoid text overlapping
)  

head(pca_for_variables$contrib, 20)

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
)

# install.packages("factoextra")
# library(factoextra)
# ?fviz_contrib
# fviz_contrib(pca, choice = "var", axes = 1, top = 20)
# 
# fviz_contrib(pca, choice = "var", axes = 2, top = 20)
# 
#  fviz_contrib(pca, choice = "var", axes = 1:5, top = 20)
# 
# fviz_pca_ind(pca,
#              axes = c(1, 2),
#              geom.ind = "point", # show points only (but not "text values")
#              col.ind = abc$temp, # colour by groups
#              palette = c("Red", "Green"),
#              addEllipses = TRUE, # Concentration ellipses
#              legend.title = "temp"
# )
# # 
# # biplot <- fviz_pca_ind(pca, geom = "point", col.ind = data_file$Vote)
# # ggpubr::ggpar(biplot,
# #               title = "Principal Component Analysis",
# #               subtitle = "Brexit dataset",
# #               caption = "Source: BBC",
# #               xlab = "PC 1", ylab = "PC 2",
# #               legend.title = "Vote", legend.position = "top",
# #               ggtheme = theme_gray(), palette = "jco")
# # 
# # 
# # biplot <- fviz_pca_ind(pca, 
# #                        axes = c(3, 4),
# #                        geom = "point", 
# #                        col.ind = data_file$Vote)
# # ggpubr::ggpar(biplot,
# #               title = "Principal Component Analysis",
# #               subtitle = "Brexit dataset",
# #               caption = "Source: BBC",
# #               xlab = "PC 3", ylab = "PC 4",
# #               legend.title = "Vote", legend.position = "top",
# #               ggtheme = theme_gray(), palette = "jco")

#--------------------------------------------------------------------------------------------------------

# Hypothetical testing

# abc <- cor.test(x=abc$temp, y=abc$wetb, method = 'spearman', exact = F)
# 
# 
# qqnorm(abc$temp)
# qqline(abc$temp, col = 'blue')

#install.packages("pwr")
library(pwr)
#calculating the effective size
effective_size <- cohen.ES(test = "r", size = "large")
effective_size
#------------------------------------------------------------------------------------------------------

#Considering effective size and alpha as 5% ,Power analysis is calculated. #pwr.t.test for corelation.
power_analysis <-pwr.t.test(d=0.5,n=NULL,sig.level=0.05,  power=0.95, type="one.sample",alternative="two.sided")
power_analysis
#plotting power analysis
plot(power_analysis)

#---------------------------------------------------------------------------------------------------------------------
subset_abc <- weather_df[, c("date", "temp")]
# ts(subset_weather_df)

subset_abc <- replace(subset_abc, TRUE, lapply(subset_abc, na2mean))            
subset_abc <- na.omit(subset_abc)


# gdf1 <- read.csv(subset_abc, header=TRUE, stringsAsFactors = FALSE, na.strings="" )

temper <- c(subset_abc$temp)

class(temper)
library(tseries)
ts <- ts(temper, start=c(2005, 6),end = c(2018,12), frequency=12)
ts
cycle(ts)
plot(ts)
start(ts)
end(ts)
frequency(ts)
class(ts)
print(summary(ts))
na_records <- ts[!complete.cases(ts)]
sum(na_records)
plot(aggregate(ts,FUN=mean))
boxplot(ts ~ cycle(ts),
        xlab="Date", 
        ylab = "Temperature" ,
        main ="Monthly Temperature Boxplot from 2004 to 2017")
seasonal_decomposition <- stl(ts, s.window = "periodic")
plot(seasonal_decomposition)


# additional plots
monthplot(ts)
library(forecast)
seasonplot(ts)

library(forecast)
deseasonal_count <- seasadj(seasonal_decomposition)
adf.test(ts, alternative = "stationary")
library(forecast)
Acf(ts)
Pacf(ts)
count <- diff(deseasonal_count, differences = 2)
plot(count)
adf.test(count, alternative = "stationary")
Acf(count, main = "ACF OF DIFFERNCED TEMPERATURES")
Pacf(count, main = "PACF OF DIFFERNCED TEMPERATURES")
auto.arima(deseasonal_count)

# simple exponential - models level
fit <- HoltWinters(ts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(ts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(ts)

# predictive accuracy
library(forecast)
accuracy(forecast(fit))

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))



#-------------------------------------------------------------------------------------------
#building seasonal ARIMA model.
fit <- arima(deseasonal_count, 
             c(3,2,5), 
             seasonal = list(order = c(0,0,0), 
                             period = 12))
fit

prediction <- predict(fit, n.ahead = 3 * 12)
prediction
#using the forecast function with confidence 95% and
#h is the forecast horizon period in months

forecast_ts <- forecast(fit, level = c(99), h = 36)
forecast_ts

#plot a forecast of the time series 
autoplot(forecast_ts)

plot(forecast(forecast_ts, 3), xlab = "Year", ylab = "Annual Temperature")

auto_arima_model <- auto.arima(deseasonal_count)
auto_arima_model

accuracy(auto_arima_model)
accuracy(fit)
plot(forecast(auto_arima_model, 3 * 12), xlab = "Year", ylab = "Annual Temperature")
plot(forecast(fit, 3 * 12), xlab = "Year", ylab = "Annual Flow")

#evaluating the model
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)



#Box.test() function tests that the autocorrelations are all zero.
Box.test(auto_arima_model$residuals, type = "Ljung-Box")
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type = "Ljung-Box")
train <- window(x = ts, start=c(2005,6), end=c(2015, 12))
test <- window(x = ts, start=c(2016))
train
test
fit <- arima(train, 
             c(3,0,2), 
             seasonal = list(order = c(0,0,0), 
                             period = 12))
fit


# predictive accuracy
library(forecast)
accuracy(forecast(fit))

# predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5))


auto_arima_model <- auto.arima(train)
auto_arima_model

predict_auto_ARIMA <- forecast(auto_arima_model, 3 * 12)
predict_auto_ARIMA

precict_manual_ARIMA <- forecast(fit, 3 * 12)
precict_manual_ARIMA



