
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
library(cluster)


#after EDA, it appears using logged data reduces multivariate outliers,
#confirms normality of multivar dist, allows better distinction in
#PCA loading directionality -> used for kmeans/medoids/hierarchical modelling

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

#logged as in paper
logged.c.whisky <- class_whisky
logged.c.whisky[,-1] <- log(class_whisky[,-1])
head(logged.c.whisky)

#melt for boxplots
whiskyclass.melt <- melt(data= class_whisky,
                         measure.vars = 2:12,
                         variable.name = "Variable",
                         value.name = "Value",
                         id.vars = 1)

str(whiskyclass.melt)
head(whiskyclass.melt)

#######################################################################
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
#non-log scale boxplaots faceted by descriptor

#USE NEXT TO UNCORRECTED MAHALABANOBIS IN PANEL
#TO SHOW REASON FOR LOGGING

ggplot(data = whiskyclass.melt, aes(x = Descriptor, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  facet_wrap(~ Variable, scales = "free") +
  # scale_fill_brewer(palette = "Dark2") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")


#USE NEXT TO CORRECTED MAHALABANOBIS IN PANEL
#TO SHOW BENEFIT OF STRUCTURE EMERGINF FROM LOGGING

#
# Boxplot faceted by Descriptor
#expand in another window
#note P, Cl, Mn, Cu, Rb,K
#- good candidates for separating at least 2 groups

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
#to display mean differences between
#whiskies of provenance, blended and counterfeit

logged.c.whisky


head(class_whisky)
levels(class_whisky$Descriptor)

#speyside vs blend, dif mean vectors
(hotelling.test(subset(logged.c.whisky, Descriptor=="Speyside")[,-c(1)],
                subset(logged.c.whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs blend, dif mean vectors
(hotelling.test(subset(logged.c.whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(logged.c.whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs speyside, evidence dif mean vectors
(hotelling.test(subset(logged.c.whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(logged.c.whisky, Descriptor =="Speyside")[,-c(1)]))

#island vs speyside (ie no dif in provenence)
(hotelling.test(subset(logged.c.whisky, Descriptor=="Island")[,-c(1)],
                subset(logged.c.whisky, Descriptor =="Speyside")[,-c(1)]))

#suggests that there are different mean vectors among at least 3 groups





#do a variance test along differing factors showing
#dif correlation structure and median differences
#to support that is multivar norm dist

var.test(subset(logged.c.whisky, Descriptor=="Speyside")$Mn,
         subset(logged.c.whisky, Descriptor=="Counterfeit")$Mn,
         alternative = "two.sided")

var.test(subset(logged.c.whisky, Descriptor=="Speyside")$Fe,
         subset(logged.c.whisky, Descriptor=="Counterfeit")$Fe,
         alternative = "two.sided")

var.test(subset(logged.c.whisky, Descriptor=="Speyside")$Mn,
         subset(logged.c.whisky, Descriptor=="Blend")$Mn,
         alternative = "two.sided")

var.test(subset(logged.c.whisky, Descriptor=="Speyside")$Fe,
         subset(logged.c.whisky, Descriptor=="Blend")$Fe,
         alternative = "two.sided")


###########
#corr test
#high corr among k-rb, p -rb, Mn-K; K may display the most redundant info



#logged
#note generally positive correlations,
#tied to upper right corner

ggcorrplot(cor(log(class_whisky[,-(1)])),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")


#speyside
#note lack of correlation where whole dataset displayed before,
# and correlation density shift


speyside_data <- subset(logged.c.whisky, logged.c.whisky[,1] == "Speyside")

ggcorrplot(cor(speyside_data[,-1]),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")

#completely different correlation sturcture
#drives Zn, Cu corner density corr seen in overrall data set
#moderately high negative correlations induced by Fe, Cl and P

counterfeit_data <- subset(logged.c.whisky, logged.c.whisky[,1] == "Counterfeit")

ggcorrplot(cor(counterfeit_data[,-1]),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")


#different structure again
blend_data <- subset(logged.c.whisky, class_whisky[,1] == "Blend")

ggcorrplot(cor(blend_data[,-1]),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")

#all points to distinct groups, at least 3 emerging
#as in paper



###################################################################
#unlogged, has extreme ebservations not meeting multivar normal dist

####mahalbanobis
#pair general results and plot with the unlogged boxes to show
#suitability of logging data

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

#summary suprise obs
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

#SHOW THIS, shows doesnt meet multivar norm dist
#show this to show that 25% of obs in somewhat suprising range
#> 20% suprising or very, also where 40% of observations should be
#there are 6%

surprise_df.w

head(noclass_whisky2)

#########################################33
#pairs, had to exclude single somewhat suprising obs for denisty plotting
ggpairs(noclass_whisky2[noclass_whisky2$surprise != "Somewhat", ], columns=1:11,
        ggplot2::aes(col=surprise, alpha=.5),
        upper = list(continuous = "density", combo = "box_no_facet")) +
  ggplot2::scale_color_manual(values=c("gray", "orange","red")) +
  ggplot2::scale_fill_manual(values=c("gray", "orange","red")) +
  ggplot2::theme(axis.text.x = element_text(angle=90, hjust=1))

#very right skewed dist
#lots of orange/red, not ideal
#again shows not multi var normal

######################################



#logged#

####################################################mahalbanobis

#much better, show general plots and distribution of observations,
#now no outliers

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

#just about perfectly aligns now - great! show
log.supr_df.2



#########################################
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




head(logged.c.whisky)

#slightly different
#provides more accounting of variation within the first principal components
#close to what the paper produced variance wise

#we will use logged data from here on out

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

str(logged.c.whisky)

logged.w.plot <- logged.c.whisky

logged.w.plot$Provenance <- ifelse(logged.c.whisky$Descriptor %in% c("Blend", "Counterfeit", "Grain"),
                                     as.character(logged.c.whisky$Descriptor),
                                     "Provenance")
logged.w.plot$Provenance <- ifelse(logged.w.plot$Provenance %in% c("Grain", "Blend"),
                                   "Grain_Blend",
                                   as.character(logged.w.plot$Provenance))

logged.w.plot$Provenance<- as.factor(logged.w.plot$Provenance)
logged.w.plot$sample <- as.factor(whisky_data$Sample_no)


# Use both color and shape to denote classes
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = logged.c.whisky$Descriptor,
         ellipse = TRUE,
         var.axes = TRUE) +
  geom_point(aes(shape = logged.c.whisky$Descriptor), size = 3) +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4))

#adjusted
# Use both color and shape to denote classes
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = logged.w.plot$Provenance,
         ellipse = TRUE,
         var.axes = TRUE) +
  geom_point(aes(shape = logged.w.plot$Descriptor,
                 color =logged.w.plot$Provenance), size = 4) +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4))  +
  geom_text(aes(label = logged.w.plot$sample),
            size = 3, vjust = -1, hjust = 0.5)

#note variable loading directionality
#S, Ca, Br primarily acting positively on PC2
# Zn between
#P - rest primarily on PC1
#fairly equally distributed loading contributions
#Though as PC1 contributes more variance (twice as much)
#P - Cu should contribute more, many of which we saw
#median differentials of within boxplots

#triplot

# Extract PC scores
pc_scores <- data.frame(PCA.whisky.log$x[,1:3])
pc_scores$groups <- class_whisky$Descriptor  # or your grouping variable
pc_scores$groups2 <- logged.w.plot$Provenance  # or your grouping variable


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

#linear separation of fakes is well seen here
#also clear seperation of 2 island whiskies, which is reflected in
#the papers dierarchical clustering


#using 3 groups

#likely proposed groups shown more clearly here
#can see that will need at least two planes to seperate
#blends from everyone else, and even then a few mistakes

#will likely be refelcted in clustering


p1.1 <- plot_ly(pc_scores,
              x = ~PC1, y = ~PC2, z = ~PC3,
              color = ~groups2,
              symbol = ~groups2,  # Add this line for shapes
              type = 'scatter3d',
              mode = 'markers',
              marker = list(size = 5),
              symbols = c('circle', 'square', 'cross')) %>%  # Specify symbols
  layout(title = "3D PCA Plot - First 3 Components",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p1.1)

#unlogged biplot
ggbiplot(PCA.whisky.scaled, obs.scale = 1, var.scale = 1,
         groups = (class_whisky$Descriptor), ellipse = TRUE)

#even more evidence for using log, gives more distinction to all groups


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

scotch.kmean <- kmeans_scotch.func(logged.c.whisky[,-1])

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

#we can see visually that over 1/3 of variation reduction is held in
#going from 2 to 3 groups


#intraclass var scores
wss.scotch

#summary
(scotch.kmean$km_results$`3`)
(scotch.kmean$km_results$`4`)


#silhouettes
#k=3
kmean.w.sil3 <- silhouette(scotch.kmean$km_results$`3`$cluster, dist(logged.c.whisky[,-1]))
kmean.sum.3 <- round(summary(kmean.w.sil3 [,3]),3)

#generally decent clustering, except 2 = 0.19
#which will likely be our blend/proveneance overlap
plot(kmean.w.sil3,main = "K = 3", border=NA)

#k=4
kmean.w.sil4 <- silhouette(scotch.kmean$km_results$`4`$cluster, dist(logged.c.whisky[,-1]))
kmean.sum.4 <- round(summary(kmean.w.sil4 [,3]),3)

#weaker clustering among 4th group
plot(kmean.w.sil4,main = "K = 4", border=NA)


sil_sum.1 <-data.frame(stat = names(kmean.sum.3), k3 = as.numeric(kmean.sum.3), k4 = as.numeric(kmean.sum.4))

#summary stats of silhouette scores

kable(sil_sum.1, caption = "Silhouette Width summary statistics for k = 3 and k =4", float = "H")%>%
  kable_styling(latex_options = "hold_position")

###########biplots all 3-4 using kmeans

#ignore
# k=4
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = as.factor(scotch.kmean$km_results$`4`$cluster), ellipse = TRUE)


################### 4, again, can see here the discrimatory
#begins to collapse in the between blend and provenance

ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = as.factor(scotch.kmean$km_results$`4`$cluster), ellipse = TRUE) +
  geom_point(aes(shape = logged.w.plot$Descriptor,
                 color = as.factor(scotch.kmean$km_results$`4`$cluster)), size = 4)  +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4), name = "Descriptor") +
  geom_text(aes(label = logged.w.plot$sample),
            size = 3, vjust = -1, hjust = 0.5)


#################



#3
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (scotch.kmean$km_results$`3`$cluster), ellipse = TRUE)


install.packages("ggnewscale")
library(ggnewscale)




#################################
#### similar to PAM, pam seems better though
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = as.factor(scotch.kmean$km_results$`3`$cluster), ellipse = TRUE) +
  geom_point(aes(shape = logged.w.plot$Descriptor,
                 color = as.factor(scotch.kmean$km_results$`3`$cluster)), size = 4)  +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4), name = "Descriptor") +
  geom_text(aes(label = logged.w.plot$sample),
            size = 3, vjust = -1, hjust = 0.5)

#most similar to manhattan distance cutting, exactly the same OAcc
#when using provenance, blend/grain and counterfeit groups


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
  layout(title = "PCA Triplot using K = 3",
         scene = list(xaxis = list(title = paste0("PC1 (", round(summary(PCA.whisky.log)$importance[2,1]*100, 1), "%)")),
                      yaxis = list(title = paste0("PC2 (", round(summary(PCA.whisky.log)$importance[2,2]*100, 1), "%)")),
                      zaxis = list(title = paste0("PC3 (", round(summary(PCA.whisky.log)$importance[2,3]*100, 1), "%)"))))

print(p2)



#triplot 3, kmeans = 4, dont use, just for visual

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

#voronoi
###### NOT SURE IF RIGHT, allejondro agreed wouldnt add much either

library(deldir)
# k = 3 --------------------------------------------------
pc_scores.v <- as.data.frame(PCA.whisky.log$x[,1:2])  # PC1 and PC2
pc_scores.v$cluster3 <- scotch.kmean$km_results[[3]]$cluster
pc_scores.v$Descriptor <- logged.c.whisky$Descriptor    # Add descriptor column
#Extract the centroids
centers3 <- scotch.kmean$km_results[[3]]$centers[, 1:2]  # Only PC1 and PC2
centers3 <- as.data.frame(centers3)
names(centers3) <- c("PC1", "PC2")

#Define plot boundaries
x_range <- range(pc_scores.v$PC1)
y_range <- range(pc_scores.v$PC2)

#Create Voronoi using centroids
vor.w.3 <- deldir(
  x = centers3$PC1,              # Only 3 points!
  y = centers3$PC2,
  rw = c(x_range[1], x_range[2],
         y_range[1], y_range[2])
)

tiles.kmean3 <- tile.list(vor.w.3)

#Plot
cluster_colors.w3 <- c("firebrick", "orange", "skyblue")

plot(tiles.kmean3,
     fillcol = cluster_colors.w3,  # Color by cluster number (1, 2, 3)
     border = "black", lwd = 2)

# Add all 32 whiskies
points(pc_scores.v$PC1, pc_scores.v$PC2,
       col = "white",
       pch = 21,      # Hollow circle (can be filled)
       bg = cluster_colors.w3[pc_scores.v$cluster3],  # Fill color (white interior)
       lwd = 2,
       cex = 1.5)

# Add the 3 centers as stars
points(centers3$PC1, centers3$PC2,
       pch = 10, cex = 2, lwd = 2, col = "white")

title("K-means Voronoi Diagram (k=3)")
# k = 4 --------------------------------------------------
pc_scores.v$cluster4 <- scotch.kmean$km_results[[4]]$cluster

vor.w.4 <- deldir(pc_scores.v$PC1, pc_scores.v$PC2)
tiles.kmean4 <- tile.list(vor.w.4)

cluster_colors.w4 <- c("indianred", "orchid", "skyblue", "lightgreen")
fill_colors.w4 <- cluster_colors.w4[as.numeric(pc_scores.v$cluster4)]

plot(tiles.kmean4, pch = 19, fillcol = fill_colors.w4, border = "white")






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

#####################################

#### best maybe?
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = as.factor(pam.scotch$pam_fit$`3`$clustering), ellipse = TRUE) +
geom_point(aes(shape = logged.w.plot$Provenance,
               color = as.factor(pam.scotch$pam_fit$`3`$clustering)), size = 4)  +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4), name = "Descriptor") +
  geom_text(aes(label = logged.w.plot$sample),
            size = 3, vjust = -1, hjust = 0.5)

#################################################3


#slightly better? disciminates against all 5 counterfeits cohesively

#4
#both kmeans and pam point to 3 groups for optimum clustering


ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = as.factor(pam.scotch$pam_fit$`4`$clustering), ellipse = TRUE) +
  geom_point(aes(shape = logged.w.plot$Descriptor,
                 color = as.factor(pam.scotch$pam_fit$`4`$clustering)), size = 4)  +
  scale_shape_manual(values = c(15, 16, 17, 7, 8, 3, 4), name = "Descriptor") +
  geom_text(aes(label = logged.w.plot$sample),
            size = 3, vjust = -1, hjust = 0.5)


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




#triplot 4, PAM = 4

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

#generally decent clustering and comparable to kmeans, same avg sil width
#but better segregation counterfeights which is important,
plot(pam.w.sil3,main = "K = 3", border=NA)


pam.w.sil4 <- silhouette(pam.scotch$pam_fit$`4`$clustering, dist(logged.c.whisky[,-1]))
pam.sum.4 <- round(summary(pam.w.sil4 [,3]),3)

#similar to kmeans result
plot(pam.w.sil4,main = "K = 4", border=NA)


sil_sum.2 <-data.frame(stat = names(pam.sum.3), k3 = as.numeric(pam.sum.3), k4 = as.numeric(pam.sum.4))

#summary
kable(sil_sum.2, caption = "Silhouette Width summary statistics for k = 3 and k =4", float = "H")%>%
  kable_styling(latex_options = "hold_position")

#voronoi
###### NOT SURE IF RIGHT, ignore

library(deldir)
### PAM with k = 3 ---------------------------------------------------------
# Cluster assignments
pc_scores.v$cluster_pam3 <- pam.scotch$pam_fit$`3`$clustering

# Extract medoids (original space) and project into PCA space
medoid3 <- pam.scotch$pam_fit$`3`$medoids

medoid3.pca <- predict(PCA.whisky.log, medoid3)[,1:2]

data_2d <- pca$x[, 1:2]

medoid3.pca <- as.data.frame(medoid3.pca)
names(medoid3.pca) <- c("PC1", "PC2")

# STEP 4: Define boundaries for the Voronoi diagram
# Without this, the edges go to infinity (your disconnected lines issue)
x_range <- range(pc_scores.v$PC1)  # Min and max of PC1
y_range <- range(pc_scores.v$PC2)  # Min and max of PC2

# Voronoi tessellation around medoids
vor.pam3 <- deldir(x = medoid3.pca$PC1,
                   y = medoid3.pca$PC2,rw = c(x_range[1],
                   x_range[2], # Rectangular window: xmin, xmax,
                  y_range[1], y_range[2]) #                     ymin, ymax
)

# STEP 6: Convert to plottable tiles
tiles.pam3 <- tile.list(vor.pam3)

# STEP 7: Plot everything
plot(tiles.pam3, border = "black", lwd = 2)  # Draw Voronoi boundaries

# Add all 32 whiskies, colored by their cluster
points(pc_scores.v$PC1, pc_scores.v$PC2,
       col = cluster_colors.w3[pc_scores.v$cluster_pam3],
       pch = 19)

# Add the 3 medoids as stars
points(medoid3.pca$PC1, medoid3.pca$PC2,
       pch = 8, cex = 2, lwd = 2, col = "black")

title("PAM Voronoi Diagram (k=3)")
# k = 4 --------------------------------------------------
pc_scores.v$cluster4 <- scotch.kmean$km_results[[4]]$cluster

vor.w.4 <- deldir(pc_scores.v$PC1, pc_scores.v$PC2)
tiles.kmean4 <- tile.list(vor.w.4)

cluster_colors.w4 <- c("indianred", "orchid", "skyblue", "lightgreen")
fill_colors.w4 <- cluster_colors.w4[as.numeric(pc_scores.v$cluster4)]

plot(tiles.kmean4, pch = 19, fillcol = fill_colors.w4, border = "white")


##############################################################
#clustering dendros

# Add descriptor with PCA scores
pc_scores.v$ID <- whisky_data$Sample_no
pc_scores.v <- as.data.frame(PCA.whisky.log$x[, 1:2])
pc_scores.v$Descriptor <- logged.c.whisky$Descriptor
pc_scores.v$Label <- paste(1:32, pc_scores.v$Descriptor, sep = "-")

# find euclidian distance matrix
dist_matrix <- dist(scale(logged.c.whisky[, -1]), method = "euclidean")

# Perform hierarchical clustering
hc_whisky <- hclust(dist_matrix, method = "complete")


# plot
plot(hc_whisky,
     main = "Euclidian Hierarchical Clustering Dendrogram",
     xlab = "Whisky Index",
     ylab = "Height (Distance)",
     hang = -1,                    # Align labels at bottom
     labels = pc_scores.v$Label)  # Use descriptors as labels

# show k=3 clusters
rect.hclust(hc_whisky, k = 3, border = c("indianred", "orange", "skyblue"))

#  add to dataframe
clusters_hc3euc <- cutree(hc_whisky, k = 3)
pc_scores.v$cluster_hc3euc <- clusters_hc3euc

#  View cluster composition
table(pc_scores.v$Descriptor, pc_scores.v$cluster_hc3euc)

#euclidian good
#seperates out islands completely in 3
#almost all regional in 1 (malt)
#counterfeits contained to group 2, along with some blends/grains


#best overrall at detection region and outliers/counterfeits
#along with manhattan


###
##################Chebyshev distance

#chebyshev dist matrix, lets see if any large apparent dif despite
#equal loadings

dist_matrix2 <- dist(scale(logged.c.whisky[, -1]), method = "maximum")

#Perform clustering
hc_whisky2 <- hclust(dist_matrix2, method = "complete")

#plot
plot(hc_whisky2,
     main = "Chebyshev Hierarchical Clustering Dendrogram",
     xlab = "Whisky Index",
     ylab = "Height (Distance)",
     hang = -1,                    # Align labels at bottom
     labels = pc_scores.v$Label)  # Use descriptors as labels

#rectangles to show k=3 clusters
rect.hclust(hc_whisky2, k = 3, border = c("indianred", "orange", "skyblue"))

#Cut tree and add to dataframe
clusters_hc3max <- cutree(hc_whisky2, k = 3)
pc_scores.v$cluster_hc3max <- clusters_hc3max

# cluster composition
table(pc_scores.v$Descriptor, pc_scores.v$cluster_hc3max)

#fairly poor performance in comparison euclidian
#correctly completely isolates 3 counterfeits (group 3)
#however are included closer to groups of provenance than blends
#likely as both are highly different from group one, but vary in directionality
#as only only considers the largest absolute difference between two whiskies across all PCs



#Counterfeit and a single malt might appear closer to each other than
#to the average Blend,
#because their largest deviations along some PC dimension
#are similar in magnitude, even if the direction is different.


################################ correlation distance (1-r)



# cor distance matrix
dist_matrix3 <- as.dist(1 - cor(t(whisky_scaled)))

#  clustering
hc_whisky3 <- hclust(dist_matrix3, method = "complete")

# plot
plot(hc_whisky3,
     main = "R Distance Hierarchical Clustering Dendrogram",
     xlab = "Whisky Index",
     ylab = "Height (Distance)",
     hang = -1,                    # Align labels at bottom
     labels = pc_scores.v$Label)  # Use descriptors as labels

# STEP 5: Add colored rectangles to show k=3 clusters
rect.hclust(hc_whisky3, k = 3, border = c("indianred", "orange", "skyblue"))

# STEP 6: Cut tree and add to dataframe
clusters_hc3cor <- cutree(hc_whisky3, k = 3)
pc_scores.v$cluster_hc3cor <- clusters_hc3cor

# STEP 7: View cluster composition
table(pc_scores.v$Descriptor, pc_scores.v$cluster_hc3cor)

#decent separation counterfeit in group 3
#hard to distinguish any provenance in groups 1 and 2
#but better relationship shown than chebyhov
#generally better at showing provenance as well


##############################

# STEP 2: Calculate distance matrix
dist_matrix4 <- dist(scale(logged.c.whisky[, -1]), method = "manhattan")

# STEP 3: Perform hierarchical clustering
hc_whisky4 <- hclust(dist_matrix4, method = "complete")



# STEP 4: PLOT THE DENDROGRAM
plot(hc_whisky4,
     main = "Manhattan Hierarchical Clustering Dendrogram",
     xlab = "Whisky Index",
     ylab = "Height (Distance)",
     hang = -1,                    # Align labels at bottom
     labels = pc_scores.v$Label)  # Use descriptors as labels

# STEP 5: Add colored rectangles to show k=3 clusters
rect.hclust(hc_whisky4, k = 3, border = c("indianred", "orange", "skyblue"))

# STEP 6: Cut tree and add to dataframe
clusters_hc3manh <- cutree(hc_whisky4, k = 3)
pc_scores.v$cluster_hc3manh <- clusters_hc3manh

# STEP 7: View cluster composition
table(pc_scores.v$Descriptor, pc_scores.v$cluster_hc3manh)

#probably the best#####################

#group one - < provenance, all 8 blends
#group 2 - all counterfeits and one grain
#group 3 - only specific distillaries, > provenance

#makes sense as all pc loadings have approximately the same influence
#and no outliars when scaled




#################################

#matrix function
confusion_metric <- function(c_matrix) {


  ################## produce class measures of quality ##########################

  ###### class TP,FP,TN, FN counts

  # generate an empty list for TP, FP, TN, FN counts
  rawposneg <- list()


  #1: true positives per class
  rawposneg$TP <- diag(c_matrix)


  #2: true negatives per class


  rawposneg$TN <- sum(c_matrix) - rowSums(c_matrix) - colSums(c_matrix) + rawposneg$TP

  #3: False positives per class

  #vector generated as before
  rawposneg$FP <-  rowSums(c_matrix) - diag(c_matrix)


  #4: False negative per class

  rawposneg$FN <- colSums(c_matrix) - diag(c_matrix)

  # convert list to table for easier viewing

  #determine number of classes for table output and further quality metrics
  n <- ncol(c_matrix)

  count_table <- as.data.frame(rawposneg)
  rownames(count_table) <- paste0("Class_", 1:n)

  ###### class quality metrics


  #extract TP,TN,FP and FN from list structure
  TP <- rawposneg$TP
  FP <- rawposneg$FP
  FN <- rawposneg$FN
  TN <- rawposneg$TN


  classquality_meas <- list()

  #1: Accuracy - ACC_i
  classquality_meas$ACC_i <- (TP + TN)/ (TP + TN + FP + FN)

  #2: Misclassification rate - MR_i
  classquality_meas$MR_i <- (FP + FN)/ (TP + TN + FP + FN)

  #3: Precision - PPV_i
  classquality_meas$PPV_i <- TP/ (TP + FP)

  #4: Recall - TPR_i
  classquality_meas$TPR_i <- TP/ (TP + FN)

  #5: Specificity - TNR_i
  classquality_meas$TNR_i <- TN/ (TN + FP)

  #6: F1-score_i
  PPV_i <- classquality_meas$PPV_i
  TPR_i <- classquality_meas$TPR_i
  classquality_meas$F_class <- (2*PPV_i*TPR_i)/(PPV_i + TPR_i)


  classqual_table <- as.data.frame(classquality_meas)
  rownames(classqual_table) <- paste0("Class_", 1:n)

  ######################## global quality measures ##########################

  #generate an empty list for global quality metrics
  globalquality_meas <- list()

  #1: OAcc or overall accuracy
  globalquality_meas$OAcc <- sum(diag(c_matrix))/sum(c_matrix)

  ###### averaged metrics

  #2: Average accuracy or AAcc
  globalquality_meas$AAcc <- sum(classquality_meas$ACC_i)/n

  #3: F1-score_m
  globalquality_meas$F1_m <- sum(classquality_meas$F_class)/n

  #4:Macro Precision or TNR_m
  globalquality_meas$TNR_m <- sum(classquality_meas$TNR_i)/n

  ###### weighted metrics

  #1: Micro F-1 score - F1_mu

  globalquality_meas$F1_mu <- (sum(TP * classquality_meas$F_class))/(sum(TP))

  #2: Micro Precision - PPV_mu

  globalquality_meas$PPV_mu <- (sum(TP * PPV_i))/(sum(TP))

  #3:Micro Recall - TPR_mu

  globalquality_meas$TPR_mu <- (sum(TP * TPR_i))/(sum(TP))

  globalqual_table <- as.data.frame(globalquality_meas)


  #Return TRUE if each condition held to allow to see where matrix fails
  #Return both class and global quality metrics
  return(list(count_table =  count_table,
              classqual_table = classqual_table,
              globalqual_table = globalqual_table))
}





# confusion matrix KMEANS, PAM, HIERARCHIAL, with k = 3

scotch.kmean$km_results$`3`$cluster


logged.w.plot$kmean <- scotch.kmean$km_results$`3`$cluster

logged.w.plot



logged.w.plot$kmean <- factor(logged.w.plot$kmean,
                              levels = c(1, 2, 3),
                              labels = c("1:Counterfeit", "2:Grain_Blend", "3:Provenance"))

kmean_c_matrix <- table(Predicted = logged.w.plot$kmean,
                          Actual = logged.w.plot$Provenance)
kmean_c_matrix

confusion_metric(kmean_c_matrix)


#####pam

pam.scotch$pam_fit$`3`$clustering

logged.w.plot$pam <- pam.scotch$pam_fit$`3`$clustering

logged.w.plot



logged.w.plot$pam <- factor(logged.w.plot$pam,
                              levels = c(2, 1, 3),
                              labels = c("1:Counterfeit", "2:Grain_Blend", "3:Provenance"))

pam_c_matrix <- table(Predicted = logged.w.plot$pam,
                        Actual = logged.w.plot$Provenance)
pam_c_matrix

confusion_metric(pam_c_matrix)

#agglomoterative clustering

#manhattan

clusters_hc3manh


logged.w.plot$manh <- clusters_hc3manh

logged.w.plot



logged.w.plot$manh <- factor(logged.w.plot$manh,
                            levels = c(2, 1, 3),
                            labels = c("1:Counterfeit", "2:Grain_Blend", "3:Provenance"))

manh_c_matrix <- table(Predicted = logged.w.plot$manh,
                      Actual = logged.w.plot$Provenance)
manh_c_matrix

confusion_metric(manh_c_matrix)

#euclidian


clusters_hc3euc


logged.w.plot$euc <- clusters_hc3euc


logged.w.plot



logged.w.plot$euc <- factor(logged.w.plot$euc,
                             levels = c(2, 1, 3),
                             labels = c("1:Counterfeit", "2:Grain_Blend", "3:Provenance"))

euc_c_matrix <- table(Predicted = logged.w.plot$euc,
                       Actual = logged.w.plot$Provenance)
euc_c_matrix

confusion_metric(euc_c_matrix)


#euclidian actually very poor



























###############################################

#just looking/ignore


# Load package

library(dendextend)
library(corrplot)

d_whisky <- dist(scale(logged.c.whisky[, -1]))

#vector all linkage methids
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty",
                    "median", "centroid", "ward.D2")
#empty dend vector
whisky_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc.whisky <- hclust(d_whisky, method = hclust_methods[i])
  whisky_dendlist <- dendlist(whisky_dendlist, as.dendrogram(hc.whisky))
}
names(whisky_dendlist) <- hclust_methods
whisky_dendlist

whisky_dendlist_cor <- cor.dendlist(whisky_dendlist)
whisky_dendlist_cor

corrplot::corrplot(whisky_dendlist_cor, "pie", "lower")


#distances

whisky_scaled <- scale(logged.c.whisky[, -1])


distance_methods <- list(
  "euclidean"   = dist(whisky_scaled, method = "euclidean"),
  "manhattan"   = dist(whisky_scaled, method = "manhattan", p = 3),
  "maximum"     = dist(whisky_scaled, method = "maximum"),
  "minkowski"   = dist(whisky_scaled, method = "minkowski"),
  "correlation" = as.dist(1 - cor(t(whisky_scaled)))  # correlation-based distance
)


distance_dendlist <- dendlist()

for(dist_name in names(distance_methods)){
  hc <- hclust(distance_methods[[dist_name]], method = "complete")
  distance_dendlist <- dendlist(distance_dendlist, as.dendrogram(hc))
}

names(distance_dendlist) <- names(distance_methods)

# Check dendlist correlations
distance_dendlist_cor <- cor.dendlist(distance_dendlist)
distance_dendlist_cor

# Visualize
corrplot(distance_dendlist_cor, method = "pie", type = "lower")


