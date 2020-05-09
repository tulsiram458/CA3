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
# View(abc)

#weather_df[!complete.cases(weather_df),]

#finding mean value to replace
mean <- mean(is.na(weather_df$clamt))
mean

#abcd <- replace(weather_df, TRUE, lapply(weather_df,mean))


#library(dplyr)

#weather_df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

# --------------------------------------------------------------------------------------------------------
  
#Co-relation
library(ggplot2) 
hist(abc$temp, main = "histogram for temperature", xlab = "temperature")
plot <- ggplot(abc, aes(x = sun, y = temp))
plot <- plot + stat_smooth(method = "lm", col = "darkblue", se = FALSE)
plot <- plot + geom_point()
print(plot)

#subset numeric data
abc <- subset(abc, select=c(rain,temp,wetb,dewpt,vappr,rhum,msl,wdsp,wddir))
colnames(abc)
str(abc)
