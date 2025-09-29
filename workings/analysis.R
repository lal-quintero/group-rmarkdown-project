
library(patchwork)
library(ggplot2)
library(ggpubr)
library(GGally)
library(ggbiplot)
library(ggcorrplot)
library(extrafont)
library(matlib)
library(bookdown)
library(RColorBrewer)
library(ggpubr)
library(ggthemes)
library(extrafont)
library(knitr)
library(kableExtra)
library(gridExtra)
library(grid)
library(dplyr)
library(reshape2)
library(Hotelling)
library(mvtnorm)
library(ggExtra)
library(reshape2)
library(plotly)


#after EDA, it appears using logged data reduces multivariate outliers,
#confirms normality of multivar dist, allows better distinction in
#PCA loading directionality -> used for kmeans/medoids modelling

# Create the whisky data as a dataframe
whisky_data <- data.frame(
  Sample_no = c("1", "2a", "3a", "4a", "5a", "6a", "7a", "8a", "9a", "10a", "11a", "12a", "13a", "14a", "15a", "16", "17", "18a", "19", "20a", "21", "22a", "23a", "24", "25", "26", "27", "28", "29", "30", "31", "32"),

  Descriptor = c("Blend", "Blend", "Blend", "Blend", "Blend", "Blend", "Blend", "Blend", "Counterfeit", "Counterfeit", "Counterfeit", "Counterfeit", "Counterfeit", "Grain", "Grain", "Highland", "Highland", "Island", "Island", "Island", "Island", "Lowland", "Lowland", "Speyside", "Speyside", "Speyside", "Speyside", "Speyside", "Speyside", "Speyside", "Speyside", "Speyside"),

  Distillery = c("Baile Nicol Jarvie", "Bells", "Chivas", "Dewars", "Johnnie Walker", "The Famous Grouse", "Whyte and Mackay", "William Grant", "Unknown 1", "Unknown 2", "Unknown 3", "Unknown 4", "Unknown 5", "Grain matured", "Grain unmatured", "Glengoyne", "Glenmorangie", "Bowmore", "Bruichladdie", "Bunnahabhain", "Talisker", "Auchentoshan", "Glenkinchie", "Balvenie", "Craigellachie", "Dufftown", "Glen Elgin", "Glenburgie", "Glennfiddich", "Glenrothes", "Knockando", "Linkwood"),

  P = c(0.152, 0.653, 0.375, 0.121, 0.326, 0.145, 0.067, 0.239, 0.089, 0.088, 0.279, 0.320, 0.120, 0.034, 0.084, 1.04, 0.126, 0.914, 1.63, 2.24, 0.034, 0.169, 0.108, 0.695, 0.096, 0.883, 0.115, 2.00, 0.317, 0.953, 0.051, 0.276),

  S = c(1.10, 1.58, 0.809, 1.16, 1.09, 0.615, 0.576, 0.748, 4.06, 14.7, 15.9, 22.1, 26.1, 2.23, 5.53, 5.57, 0.796, 6.67, 5.48, 7.54, 4.85, 1.46, 2.45, 3.85, 0.819, 4.64, 1.35, 7.91, 2.72, 4.11, 1.03, 1.05),

  Cl = c(0.173, 0.238, 0.193, 0.157, 0.180, 0.097, 0.151, 0.147, 0.066, 0.072, 0.083, 0.596, 0.071, 0.252, 0.113, 0.343, 0.245, 0.316, 0.697, 1.35, 0.362, 0.417, 0.176, 0.120, 0.177, 0.130, 0.404, 0.185, 0.344, 0.399, 0.191, 0.207),

  K = c(7.86, 4.93, 4.31, 3.20, 5.48, 2.74, 2.36, 2.84, 0.336, 1.23, 0.811, 2.32, 2.37, 6.44, 3.25, 24.2, 6.95, 21.1, 36.5, 36.2, 5.67, 11.7, 7.76, 20.3, 6.11, 14.0, 9.27, 37.7, 12.4, 16.7, 5.14, 6.22),

  Ca = c(1.45, 1.40, 1.22, 1.14, 0.526, 0.416, 0.745, 0.976, 1.24, 1.40, 1.36, 1.78, 1.63, 1.04, 1.35, 0.857, 0.859, 0.868, 4.13, 2.12, 0.607, 0.681, 0.738, 0.765, 0.633, 1.05, 1.40, 1.65, 0.660, 1.83, 0.605, 1.01),

  Mn = c(0.032, 0.019, 0.019, 0.011, 0.018, 0.009, 0.012, 0.010, 0.007, 0.006, 0.006, 0.008, 0.010, 0.013, 0.012, 0.023, 0.035, 0.037, 0.038, 0.051, 0.018, 0.042, 0.031, 0.031, 0.024, 0.030, 0.031, 0.053, 0.029, 0.041, 0.017, 0.020),

  Fe = c(0.027, 0.110, 0.044, 0.050, 0.103, 0.050, 0.047, 0.021, 0.154, 0.025, 0.057, 0.019, 0.082, 0.115, 0.076, 0.197, 0.025, 0.148, 0.288, 0.184, 0.070, 0.128, 0.106, 0.121, 0.094, 0.078, 0.046, 0.134, 0.132, 0.137, 0.094, 0.064),

  Cu = c(0.186, 0.242, 0.196, 0.189, 0.286, 0.208, 0.159, 0.137, 0.085, 0.052, 0.038, 0.038, 0.187, 0.174, 0.164, 1.251, 0.523, 0.548, 0.587, 0.580, 0.277, 1.32, 0.434, 0.380, 0.239, 0.533, 0.195, 0.198, 0.519, 1.030, 0.432, 0.769),

  Zn = c(0.015, 0.021, 0.007, 0.018, 0.020, 0.007, 0.019, 0.020, 0.038, 0.018, 0.016, 0.015, 0.194, 0.019, 0.046, 0.041, 0.011, 0.032, 0.066, 0.057, 0.033, 0.037, 0.022, 0.035, 0.025, 0.024, 0.029, 0.043, 0.193, 0.029, 0.020, 0.019),

  Br = c(0.002, 0.005, 0.003, 0.003, 0.002, 0.002, 0.003, 0.003, 0.005, 0.004, 0.002, 0.068, 0.012, 0.004, 0.010, 0.004, 0.003, 0.007, 0.034, 0.014, 0.003, 0.006, 0.002, 0.005, 0.005, 0.002, 0.006, 0.008, 0.004, 0.007, 0.008, 0.004),

  Rb = c(0.006, 0.003, 0.002, 0.003, 0.002, 0.001, 0.002, 0.002, 0.001, 0.001, 0.002, 0.001, 0.005, 0.006, 0.003, 0.016, 0.006, 0.018, 0.039, 0.037, 0.006, 0.012, 0.007, 0.024, 0.006, 0.014, 0.009, 0.026, 0.013, 0.014, 0.005, 0.006)
)

# View the dataframe
head(whisky_data)
str(whisky_data)

whisky_data$Sample_no <- as.factor(whisky_data$Sample_no)
whisky_data$Descriptor <- as.factor(whisky_data$Descriptor)
whisky_data$Distillery <- as.factor(whisky_data$Distillery)

str(whisky_data)
levels(whisky_data$Descriptor)

#subset data for handling
noclass_whisky <- subset(whisky_data, select = -c(Sample_no, Descriptor, Distillery))

class_whisky <- subset(whisky_data, select = -c(Sample_no, Distillery))

head(noclass_whisky)
head(class_whisky)

#melt for boxplots
whiskyclass.melt <- melt(data= class_whisky,
                         measure.vars = 2:12,
                         variable.name = "Variable",
                         value.name = "Value",
                         id.vars = 1)

str(whiskyclass.melt)
head(whiskyclass.melt)

#boxplots
#unlogged, no class

ggplot(data = whiskyclass.melt, aes(x = Variable, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")

#logged, no class

ggplot(data = whiskyclass.melt, aes(x = Variable, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  scale_y_log10() +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")


##########################################################################

# Boxplot faceted by Descriptor
#expand in another window
#note P, Cl, Mn, Cu, Rb - good candidates for separating at least 2 groups

#log scale

ggplot(data = whiskyclass.melt, aes(x = Descriptor, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  scale_y_log10() +
  facet_wrap(~ Variable, scales = "free") +
  # scale_fill_brewer(palette = "Dark2") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")


######################################################

#mean vector tests
#cov tests

head(class_whisky)
levels(class_whisky$Descriptor)

#speyside vs blend, dif mean vectors
(hotelling.test(subset(class_whisky, Descriptor=="Speyside")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs blend, dif mean vectors
(hotelling.test(subset(class_whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs speyside, weak evidence dif mean vectors
(hotelling.test(subset(class_whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(class_whisky, Descriptor =="Speyside")[,-c(1)]))

#island vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Island")[,-c(1)],
                subset(class_whisky, Descriptor =="Speyside")[,-c(1)]))

#counterfeit vs blend, no dif mean vectors
(hotelling.test(subset(class_whisky, Descriptor=="Island")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))

#do a variance test as well to rule out LDA


###########
#corr test
#high corr among k-rb, p -rb, Mn-K; K may display the most redundant info

#unlogged
ggcorrplot(cor(class_whisky[,-(1)]),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")

#logged
ggcorrplot(cor(log(class_whisky[,-(1)])),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")





###################################################################
#unlogged#, has extrem ebservations not meeting multivar normal dist

####mahalbanobis

mu.hat.w <- colMeans(noclass_whisky)
mu.hat.w

sigma.hat.w <- cov(noclass_whisky)
sigma.hat.w

dMw <- mahalanobis(noclass_whisky, center=mu.hat.w, cov=sigma.hat.w)

str(noclass_whisky)

upper.quantiles.w <- qchisq(c(.9, .95, .99), df=11)
density.at.quantiles.w <- dchisq(x=upper.quantiles.w, df=11)
cut.points.w <- data.frame(upper.quantiles.w, density.at.quantiles.w)


ggplot(data.frame(dMw), aes(x=dMw)) +
  geom_histogram(aes(y=after_stat(density)), bins=nclass.FD(dMw),
                 fill="white", col="black") +
  geom_rug() +
  stat_function(fun=dchisq, args = list(df=11),
                col="firebrick", size=1.5, alpha=.7, xlim=c(0,25)) +
  geom_segment(data=cut.points.w,
               aes(x=upper.quantiles.w, xend=upper.quantiles.w,
                   y=rep(0,3), yend=density.at.quantiles.w),
               col="navy", size=2) +
  xlab("Mahalanobis distances and cut points") +
  ylab("Histogram and density")


#mahalabanobis table

noclass_whisky2 <- noclass_whisky

noclass_whisky2$dMw <- dMw

noclass_whisky2$surprise <- cut(noclass_whisky2$dMw,
                                breaks= c(0, upper.quantiles.w, Inf),
                                labels=c("Typical", "Somewhat", "Surprising", "Very"))

surprise_summaryw <- table(noclass_whisky2$surprise)
surprise_summaryw

surprise_df <- data.frame(
  Category = names(surprise_summaryw),
  Count = as.numeric(surprise_summaryw),
  Percentage = round(100 * as.numeric(surprise_summaryw) / sum(surprise_summaryw), 1)
)

#surprise observation dataframe

surprise_df

#more detailed surprise observation
noclass_whisky2$surprise_detailed <- cut(dMw,
                                         breaks = c(0,
                                                    qchisq(0.5, df=ncol(noclass_whisky)),   # 50%
                                                    qchisq(0.75, df=ncol(noclass_whisky)),  # 75%
                                                    qchisq(0.9, df=ncol(noclass_whisky)),   # 90%
                                                    qchisq(0.95, df=ncol(noclass_whisky)),  # 95%
                                                    qchisq(0.99, df=ncol(noclass_whisky)),  # 99%
                                                    Inf),
                                         labels = c("Bottom_50%", "50-75%", "75-90%",

                                                    "90-95%", "95-99%", "Top_1%"))


#more detailed look at suprising observations
print(table(noclass_whisky2$surprise_detailed))


surprise_summary.w <- table(noclass_whisky2$surprise_detailed)

surprise_df.w <- data.frame(
  Category = names(surprise_summary.w),
  Count = as.numeric(surprise_summary.w),
  Percentage = round(100 * as.numeric(surprise_summary.w) / sum(surprise_summary.w), 1)
)
surprise_df.w

head(noclass_whisky2)

#########################################33
#pairs, had to exclude single somewhat suprising obs for denisty plotting
ggpairs(noclass_whisky2[noclass_whisky2$surprise != "Somewhat", ], columns=1:11,
        ggplot2::aes(col=surprise, alpha=.5),
        upper = list(continuous = "density", combo = "box_no_facet")) +
  ggplot2::scale_color_manual(values=c("green", "blue", "red")) +
  ggplot2::theme(axis.text.x = element_text(angle=90, hjust=1))


######################################



#logged#

####mahalbanobis

nc_whisky.log <- log(noclass_whisky)

mu.hat.l <- colMeans(nc_whisky.log)
mu.hat.l


sigma.hat.l <- cov(nc_whisky.log)
sigma.hat.l

l.dMw <- mahalanobis(nc_whisky.log, center=mu.hat.l, cov=sigma.hat.l)

str(nc_whisky.log)

upper.quantiles.w <- qchisq(c(.9, .95, .99), df=11)
density.at.quantiles.w <- dchisq(x=upper.quantiles.w, df=11)
cut.points.w <- data.frame(upper.quantiles.w, density.at.quantiles.w)


ggplot(data.frame(l.dMw), aes(x=l.dMw)) +
  geom_histogram(aes(y=after_stat(density)), bins=nclass.FD(l.dMw),
                 fill="white", col="black") +
  geom_rug() +
  stat_function(fun=dchisq, args = list(df=11),
                col="firebrick", size=1.5, alpha=.7, xlim=c(0,25)) +
  geom_segment(data=cut.points.w,
               aes(x=upper.quantiles.w, xend=upper.quantiles.w,
                   y=rep(0,3), yend=density.at.quantiles.w),
               col="navy", size=2) +
  xlab("Mahalanobis distances and cut points") +
  ylab("Histogram and density")


#mahalabanobis table

nc_whisky.log2 <- nc_whisky.log

nc_whisky.log2$dMw <- l.dMw

nc_whisky.log2$surprise <- cut(nc_whisky.log2$dMw,
                               breaks= c(0, upper.quantiles.w, Inf),
                               labels=c("Typical", "Somewhat", "Surprising", "Very"))

log_surprise_sum <- table(nc_whisky.log2$surprise)
log_surprise_sum

log.surprise_df <- data.frame(
  Category = names(log_surprise_sum),
  Count = as.numeric(log_surprise_sum),
  Percentage = round(100 * as.numeric(log_surprise_sum) / sum(log_surprise_sum), 1)
)

#surprise observation dataframe
#multivar normalacy achieved
log.surprise_df

#more detailed surprise observation
nc_whisky.log2$surprise_detailed <- cut(l.dMw,
                                        breaks = c(0,
                                                   qchisq(0.5, df=ncol(noclass_whisky)),   # 50%
                                                   qchisq(0.75, df=ncol(noclass_whisky)),  # 75%
                                                   qchisq(0.9, df=ncol(noclass_whisky)),   # 90%
                                                   qchisq(0.95, df=ncol(noclass_whisky)),  # 95%
                                                   qchisq(0.99, df=ncol(noclass_whisky)),  # 99%
                                                   Inf),
                                        labels = c("Bottom_50%", "50-75%", "75-90%",

                                                   "90-95%", "95-99%", "Top_1%"))
print(table(nc_whisky.log2$surprise_detailed))


det.logsuprise <- table(nc_whisky.log2$surprise_detailed)

log.supr_df.2 <- data.frame(
  Category = names(det.logsuprise),
  Count = as.numeric(det.logsuprise),
  Percentage = round(100 * as.numeric(det.logsuprise) / sum(det.logsuprise), 1)
)
log.supr_df.2



#########################################33
#here we can see more normal dist

filtered_data <- nc_whisky.log2 %>%
  filter(!surprise %in% c("Surprising", "Very"))

ggpairs(filtered_data, columns=1:11,
        ggplot2::aes(col=surprise, alpha=.5),
        upper = list(continuous = wrap("density", mapping = aes(fill=surprise), alpha=0.3)),
        lower = list(continuous = wrap("points", alpha=0.7))) +
  ggplot2::scale_color_manual(values=c("gray", "orange")) +
  ggplot2::scale_fill_manual(values=c("gray", "orange")) +
  ggplot2::theme(axis.text.x = element_text(angle=90, hjust=1))
#####################################










#PCA

#non-logged
head(class_whisky)

PCA.whisky.scaled <- prcomp(class_whisky[,-(1)], center = TRUE, scale = TRUE)
whisky.pca.summary <- summary(PCA.whisky.scaled)
kable(whisky.pca.summary$importance, caption = "Standardized PCA Summary", digits = 4)%>%
  kable_styling(latex_options = "hold_position")


#logged, as in paper

logged.c.whisky <- class_whisky
logged.c.whisky[,-1] <- log(class_whisky[,-1])
head(logged.c.whisky)


head(logged.c.whisky)

#slightly different, will investigate (PC1 0.4694 vs 0.476, PC2 0.2369 vs 0.212)
#provides more accounting of variation within the first principal components

PCA.whisky.log <- prcomp(logged.c.whisky[,-(1)], center = TRUE, scale = TRUE)
log.whisky.summary <- summary(PCA.whisky.log)
kable(log.whisky.summary$importance, caption = "Standardized PCA Summary", digits = 4)%>%
  kable_styling(latex_options = "hold_position")

#plot
plot(PCA.whisky.scaled, type="l")

plot(PCA.whisky.log, type="l")

#eigenvectors/pc components
#generally positive, approximate equal loadoings for first PCA component
#generally negative loadings for second pca component
#PC3 shows general class differentaition begining with pos/neg loadings
PCA.whisky.log$rotation

#biplot, logged, more clarity
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (class_whisky$Descriptor), ellipse = TRUE)

#triplot

# Extract PC scores
pc_scores <- data.frame(PCA.whisky.log$x[,1:3])
pc_scores$groups <- class_whisky$Descriptor  # or your grouping variable

# Create interactive 3D plot
p1 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups,
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5)) %>%
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p1)

#unlogged biplot
ggbiplot(PCA.whisky.scaled, obs.scale = 1, var.scale = 1,
         groups = (class_whisky$Descriptor), ellipse = TRUE)



#############################################################

#kmeans


############################################################
#############################################################

kmeans_scotch.func <- function(data){
  results <- list()
  for (i in 1:10){
    results[[as.character(i)]] <- kmeans(data,
                                         algorithm="MacQueen",
                                         centers=i, iter.max = 100, nstart = 50)

  }

  wss <- sapply(results, function(x) sum(x$withinss))

  return(list(km_results = results, wss = wss))
}

scotch.kmean <- erupt.func(logged.c.whisky[,-1])

wss.scotch <- data.frame(y = scotch.kmean$wss, x = c(1:10))

ggplot(wss.scotch, aes(x = x, y = y)) +
  geom_point(color = "black", size = 3) +
  geom_line(linetype = "solid", color = "black") +
  theme_tufte() +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 109.43011, ymax = 161.11470, alpha = 0.3, fill = "indianred") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 109.43011, ymax = 45.42382, alpha = 0.2, fill = "coral") +
  theme_tufte() +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +  # set ticks 1 to 10
  scale_y_continuous(limits = c(0, 300)) +
  labs(
    y = "Total Within Sum of Squares (WSS)",
    x = "Number of Clusters (K)"
  ) +
  theme(axis.text = element_text(angle =45, size = 10),
        axis.text.x = element_text(face = "italic"))

#intraclass var scores
wss.scotch

#summary
(scotch.kmean$km_results$`3`)
(scotch.kmean$km_results$`4`)


#silhouettes
kmean.w.sil3 <- silhouette(scotch.kmean$km_results$`3`$cluster, dist(logged.c.whisky[,-1]))
kmean.sum.3 <- round(summary(kmean.w.sil3 [,3]),3)

#generally decent clustering, except 2 = 0.19
plot(kmean.w.sil3,main = "K = 3", border=NA)


kmean.w.sil4 <- silhouette(scotch.kmean$km_results$`4`$cluster, dist(logged.c.whisky[,-1]))
kmean.sum.4 <- round(summary(kmean.w.sil4 [,3]),3)

#weaker clustering among 1 and 2
plot(kmean.w.sil4,main = "K = 4", border=NA)


sil_sum.1 <-data.frame(stat = names(kmean.sum.3), k3 = as.numeric(kmean.sum.3), k4 = as.numeric(kmean.sum.4))


kable(sil_sum.1, caption = "Silhouette Width summary statistics for k = 3 and k =4", float = "H")%>%
  kable_styling(latex_options = "hold_position")

###########biplots all 3-4 using kmeans

ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (scotch.kmean$km_results$`4`$cluster), ellipse = TRUE)


ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (scotch.kmean$km_results$`3`$cluster), ellipse = TRUE)



#triplot of 3
#assign kmeans groups, 3
pc_scores$groups2 <- scotch.kmean$km_results$`3`$cluster  # or your grouping variable

# Create plot
p2 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups2,
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5)) %>%
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p2)



#triplot 3, kmeans = 4

pc_scores$groups3 <- scotch.kmean$km_results$`4`$cluster  # or your grouping variable

# Create interactive 3D plot
p3 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups3,
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5)) %>%
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p3)



############################################################
#pam cluster analysis



library(cluster)

kmedoid_scotch.func <- function(data, k_max = 10) {
  pam_fit <- list()
  wss <- numeric(k_max)        # preallocate
  sil_width <- numeric(k_max)  # preallocate

  for (i in 2:k_max) { # PAM doesn't work with k=1
    pam_fit[[as.character(i)]] <- pam(data, k = i)

    # within-cluster dissimilarity (objective function)
    wss[i] <- pam_fit[[as.character(i)]]$objective[2]

    # average silhouette width
    sil_width[i] <- pam_fit[[as.character(i)]]$silinfo$avg.width
  }

  return(list(pam_fit = pam_fit, wss = wss, silhouette = sil_width))
}

pam.scotch <- kmedoid_scotch.func(logged.c.whisky[,-1])

summary(pam.scotch$pam_fit$`3`)
summary(pam.scotch$pam_fit$`4`)


###pam1 function, allejondro helped with

# Ideally, try several distances and estimate the "optimum" number of classes
# Use that number of clusters, and repeat the visualisation per cluster

pam1 <- cluster::pam(class_whisky[,2:12], k=4)


summary(pam1)

#biplots

ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (pam.scotch$pam_fit$`2`$clustering), ellipse = TRUE)

ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (pam.scotch$pam_fit$`3`$clustering), ellipse = TRUE)

#both kmeans and pam point to 3-4 groups for optimum clustering
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (pam.scotch$pam_fit$`4`$clustering), ellipse = TRUE)

#triplot 4, PAM = 3

pc_scores$groups4 <- pam.scotch$pam_fit$`3`$clustering

# Create interactive 3D plot
p4 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups4,
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5)) %>%
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p4)




#triplot 4, PAM = 3

pc_scores$groups5 <- pam.scotch$pam_fit$`4`$clustering

# Create interactive 3D plot
p5 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups5,
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5)) %>%
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p5)
######################################################

#silhouette plots
#silhouettes
pam.w.sil3 <- silhouette(pam.scotch$pam_fit$`3`$clustering, dist(logged.c.whisky[,-1]))
pam.sum.3 <- round(summary(pam.w.sil3 [,3]),3)

#generally decent clustering
plot(pam.w.sil3,main = "K = 3", border=NA)


pam.w.sil4 <- silhouette(pam.scotch$pam_fit$`4`$clustering, dist(logged.c.whisky[,-1]))
pam.sum.4 <- round(summary(pam.w.sil4 [,3]),3)

#generally decent clustering
plot(pam.w.sil4,main = "K = 4", border=NA)


sil_sum.2 <-data.frame(stat = names(pam.sum.3), k3 = as.numeric(pam.sum.3), k4 = as.numeric(pam.sum.4))


kable(sil_sum.2, caption = "Silhouette Width summary statistics for k = 3 and k =4", float = "H")%>%
  kable_styling(latex_options = "hold_position")


