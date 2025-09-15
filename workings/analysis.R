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

noclass_whisky <- subset(whisky_data, select = -c(Sample_no, Descriptor, Distillery))

class_whisky <- subset(whisky_data, select = -c(Sample_no, Distillery))

head(noclass_whisky)
head(class_whisky)

whiskyclass.melt <- melt(data= class_whisky,
                         measure.vars = 2:11,
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


#no log, low fidelity, by class

ggplot(data = whiskyclass.melt, aes(x = Variable, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  facet_wrap(~ Descriptor, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")






#log scale, by class

ggplot(data = whiskyclass.melt, aes(x = Variable, y = Value)) +
  geom_boxplot(aes(fill = Variable), notch = TRUE) +
  scale_y_log10() +
  facet_wrap(~ Descriptor, scales = "free") +
  scale_fill_brewer(palette = "Dark2") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none") +
  labs(x = "Chemical Elements", y = "Measured Values")



# Boxplot grouped by Descriptor
ggplot(data = whiskyclass.melt, aes(x = Variable, y = Value)) +
  geom_boxplot(aes(fill = Descriptor), notch = TRUE) +
  scale_y_log10() +
  scale_fill_brewer(palette = "Dark2") +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Whisky Chemical Elements",
       y = "Measured Values",
       fill = "Whisky Type")

head(class_whisky)
levels(class_whisky$Descriptor)

#speyside vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Speyside")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))

#counterfeit vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Counterfeit")[,-c(1)],
                subset(class_whisky, Descriptor =="Speyside")[,-c(1)]))

#counterfeit vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Island")[,-c(1)],
                subset(class_whisky, Descriptor =="Speyside")[,-c(1)]))

#counterfeit vs blend
(hotelling.test(subset(class_whisky, Descriptor=="Island")[,-c(1)],
                subset(class_whisky, Descriptor =="Blend")[,-c(1)]))


###########
#corr test

ggcorrplot(cor(class_whisky[,-(1)]),
           method = "square",
           lab=TRUE,
           ggtheme = theme_tufte,
           colors = c("cyan", "white", "coral"),
           hc.order = TRUE,
           type = "lower")


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


#table

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
print(table(noclass_whisky2$surprise_detailed))

cat("\nSummary of Mahalanobis distances by surprise level:\n")
by(dMw, noclass_whisky2$surprise, summary)

surprise_summary.w <- table(noclass_whisky2$surprise_detailed)

surprise_df.w <- data.frame(
  Category = names(surprise_summary.w),
  Count = as.numeric(surprise_summary.w),
  Percentage = round(100 * as.numeric(surprise_summary.w) / sum(surprise_summary.w), 1)
)
surprise_df.w

head(noclass_whisky2)

#pairs (didnt work likely from too few obs)
ggpairs(noclass_whisky2, columns=1:11,
        ggplot2::aes(col=surprise, alpha=.5),
        upper = list(continuous = "density", combo = "box_no_facet")) +
  ggplot2::scale_color_manual(values=c("lightgray", "green", "blue", "red")) +
  ggplot2::theme(axis.text.x = element_text(angle=90, hjust=1))


######################################
#PCA

#non-logged

PCA.whisky.scaled <- prcomp(class_whisky[,-(1)], center = TRUE, scale = TRUE)
whisky.pca.summary <- summary(PCA.whisky.scaled)
kable(whisky.pca.summary$importance, caption = "Standardized PCA Summary", digits = 4)%>%
  kable_styling(latex_options = "hold_position")


#logged, as in paper

logged.c.whisky <- log(class_whisky[,-(1)])

head(logged.c.whisky)

#slightly different, will investigate (PC1 0.4694 vs 0.476, PC2 0.2369 vs 0.212)
PCA.whisky.log <- prcomp(logged.c.whisky[,-(1)], center = TRUE, scale = TRUE)
log.whisky.summary <- summary(PCA.whisky.log)
kable(log.whisky.summary$importance, caption = "Standardized PCA Summary", digits = 4)%>%
  kable_styling(latex_options = "hold_position")

#plot

plot(PCA.whisky.log, type="l")

#eigenvectors/pc components

PCA.whisky.log$rotation

#biplot, logged, more clarity
ggbiplot(PCA.whisky.log, obs.scale = 1, var.scale = 1,
         groups = (class_whisky$Descriptor), ellipse = TRUE)

#unlogged
ggbiplot(PCA.whisky.scaled, obs.scale = 1, var.scale = 1,
         groups = (class_whisky$Descriptor), ellipse = TRUE)
