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
#---------------------------------------------------------------------------------------------------------

#identifying missing values

library(mice)

md.pattern(weather_df)

library(VIM)
missing <- aggr(weather_df, prop = FALSE, numbers = TRUE)
summary(missing)

#matrixplot(weather_df)



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

#--------------------------------------------------------------------------------------------------------
#PCA

pca <- prcomp(abc, center = TRUE, scale. = TRUE)
summary(pca)

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
