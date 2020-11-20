# Exploratory analysis:
# Analysis of data grouped by fabrics(groups observed through petrographic analysis)

##############################################################################
# Count Plot by vessel type
new_data %>% 
  ggplot(aes(y = vessel_type, fill = vessel_type))  + 
  geom_bar(size = 1) +
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"),
        legend.position = "none")
group_by(new_data, vessel_type) %>% count()
group_by(new_data, vessel_type) %>% summarise(n_percento = round(n()*100/49))

##############################################################################
# Count Plot by chronology
new_data %>% 
  ggplot(aes(y = chronology, fill = chronology))  + 
  geom_bar(size = 1)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
group_by(new_data, chronology) %>% count()
group_by(new_data, chronology) %>% summarise(n_percento = round(n()*100/49))

##############################################################################
# Count Plot of fabrics
new_data %>% 
  ggplot(aes(y = fabric, fill = fabric))  + 
  geom_bar(size = 1) +
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
group_by(new_data, fabric) %>% count()
group_by(new_data, fabric) %>% summarise(n_percento = round(n()*100/49))

##############################################################################
# Fabric 1:
# Summary of the data:
filter(new_data, fabric == "1")[5:34] %>% summary() %>% kable(caption = "Fabric 1") %>% kable_styling()
summary_major_fabric1 <- filter(new_data, fabric == "1") %>% 
  summarize(avg_SiO2 = round(mean(SiO2),2), sd_SiO2 = round(sd(SiO2),2), 
            avg_TiO2 = round(mean(TiO2),2), sd_TiO2 = round(sd(TiO2),2), 
            avg_Al2O3 = round(mean(Al2O3),2), sd_Al2O3 = round(sd(Al2O3),2), 
            avg_Fe2O3TOT = round(mean(Fe2O3TOT),2), sd_Fe2O3TOT = round(sd(Fe2O3TOT),2), 
            avg_MnO = round(mean(MnO),2), sd_MnO = round(sd(MnO),2), 
            avg_MgO = round(mean(MgO),2), sd_MgO = round(sd(MgO),2), 
            avg_CaO = round(mean(CaO),2), sd_CaO = round(sd(CaO),2), 
            avg_Na2O = round(mean(Na2O),2), sd_Na2O = round(sd(Na2O),2), 
            avg_K2O = round(mean(K2O),2), sd_K2O = round(sd(K2O),2), 
            avg_P2O5 = round(mean(P2O5),2), sd_P2O5 = round(sd(P2O5),2))
summary_major_fabric1 <- data.frame("Major elements (weight percent)" = c("SiO2","TiO2","Al2O3","Fe2O3TOT","MnO","MgO","CaO","Na2O","K2O","P2O5"),
                                    "avg" = c(summary_major_fabric1$avg_SiO2,summary_major_fabric1$avg_TiO2,summary_major_fabric1$avg_Al2O3,summary_major_fabric1$avg_Fe2O3TOT,summary_major_fabric1$avg_MnO,summary_major_fabric1$avg_MgO,summary_major_fabric1$avg_CaO,summary_major_fabric1$avg_Na2O,summary_major_fabric1$avg_K2O,summary_major_fabric1$avg_P2O5), 
                                    "sd" = c(summary_major_fabric1$sd_SiO2,summary_major_fabric1$sd_TiO2,summary_major_fabric1$sd_Al2O3,summary_major_fabric1$sd_Fe2O3TOT,summary_major_fabric1$sd_MnO,summary_major_fabric1$sd_MgO,summary_major_fabric1$sd_CaO,summary_major_fabric1$sd_Na2O,summary_major_fabric1$sd_K2O,summary_major_fabric1$sd_P2O5))%>%
  kable(caption = "Fabric 1") %>% kable_styling()
summary_major_fabric1

# Fabric 2:
filter(new_data, fabric == "2")[5:34] %>% summary() %>% kable(caption = "Fabric 2") %>% kable_styling()
summary_major_fabric2 <- filter(new_data, fabric == "2") %>% 
  summarize(avg_SiO2 = round(mean(SiO2),2), sd_SiO2 = round(sd(SiO2),2), 
            avg_TiO2 = round(mean(TiO2),2), sd_TiO2 = round(sd(TiO2),2), 
            avg_Al2O3 = round(mean(Al2O3),2), sd_Al2O3 = round(sd(Al2O3),2), 
            avg_Fe2O3TOT = round(mean(Fe2O3TOT),2), sd_Fe2O3TOT = round(sd(Fe2O3TOT),2), 
            avg_MnO = round(mean(MnO),2), sd_MnO = round(sd(MnO),2), 
            avg_MgO = round(mean(MgO),2), sd_MgO = round(sd(MgO),2), 
            avg_CaO = round(mean(CaO),2), sd_CaO = round(sd(CaO),2), 
            avg_Na2O = round(mean(Na2O),2), sd_Na2O = round(sd(Na2O),2), 
            avg_K2O = round(mean(K2O),2), sd_K2O = round(sd(K2O),2), 
            avg_P2O5 = round(mean(P2O5),2), sd_P2O5 = round(sd(P2O5),2))
summary_major_fabric2 <- data.frame("Major elements (weight percent)" = c("SiO2","TiO2","Al2O3","Fe2O3TOT","MnO","MgO","CaO","Na2O","K2O","P2O5"),
                                    "avg" = c(summary_major_fabric2$avg_SiO2,summary_major_fabric2$avg_TiO2,summary_major_fabric2$avg_Al2O3,summary_major_fabric2$avg_Fe2O3TOT,summary_major_fabric2$avg_MnO,summary_major_fabric2$avg_MgO,summary_major_fabric2$avg_CaO,summary_major_fabric2$avg_Na2O,summary_major_fabric2$avg_K2O,summary_major_fabric2$avg_P2O5), 
                                    "sd" = c(summary_major_fabric2$sd_SiO2,summary_major_fabric2$sd_TiO2,summary_major_fabric2$sd_Al2O3,summary_major_fabric2$sd_Fe2O3TOT,summary_major_fabric2$sd_MnO,summary_major_fabric2$sd_MgO,summary_major_fabric2$sd_CaO,summary_major_fabric2$sd_Na2O,summary_major_fabric2$sd_K2O,summary_major_fabric2$sd_P2O5))%>%
  kable(caption = "Fabric 2") %>% kable_styling()
summary_major_fabric2

# Fabric 3:
# Summary of the data:
filter(new_data, fabric == "3")[5:34] %>% summary() %>% kable(caption = "Fabric 3") %>% kable_styling()
summary_major_fabric3 <- filter(new_data, fabric == "3") %>% 
  summarize(avg_SiO2 = round(mean(SiO2),2), sd_SiO2 = round(sd(SiO2),2), 
            avg_TiO2 = round(mean(TiO2),2), sd_TiO2 = round(sd(TiO2),2), 
            avg_Al2O3 = round(mean(Al2O3),2), sd_Al2O3 = round(sd(Al2O3),2), 
            avg_Fe2O3TOT = round(mean(Fe2O3TOT),2), sd_Fe2O3TOT = round(sd(Fe2O3TOT),2), 
            avg_MnO = round(mean(MnO),2), sd_MnO = round(sd(MnO),2), 
            avg_MgO = round(mean(MgO),2), sd_MgO = round(sd(MgO),2), 
            avg_CaO = round(mean(CaO),2), sd_CaO = round(sd(CaO),2), 
            avg_Na2O = round(mean(Na2O),2), sd_Na2O = round(sd(Na2O),2), 
            avg_K2O = round(mean(K2O),2), sd_K2O = round(sd(K2O),2), 
            avg_P2O5 = round(mean(P2O5),2), sd_P2O5 = round(sd(P2O5),2))
summary_major_fabric3 <- data.frame("Major elements (weight percent)" = c("SiO2","TiO2","Al2O3","Fe2O3TOT","MnO","MgO","CaO","Na2O","K2O","P2O5"),
                                    "avg" = c(summary_major_fabric3$avg_SiO2,summary_major_fabric3$avg_TiO2,summary_major_fabric3$avg_Al2O3,summary_major_fabric3$avg_Fe2O3TOT,summary_major_fabric3$avg_MnO,summary_major_fabric3$avg_MgO,summary_major_fabric3$avg_CaO,summary_major_fabric3$avg_Na2O,summary_major_fabric3$avg_K2O,summary_major_fabric3$avg_P2O5), 
                                    "sd" = c(summary_major_fabric3$sd_SiO2,summary_major_fabric3$sd_TiO2,summary_major_fabric3$sd_Al2O3,summary_major_fabric3$sd_Fe2O3TOT,summary_major_fabric3$sd_MnO,summary_major_fabric3$sd_MgO,summary_major_fabric3$sd_CaO,summary_major_fabric3$sd_Na2O,summary_major_fabric3$sd_K2O,summary_major_fabric3$sd_P2O5))%>%
  kable(caption = "Fabric 3") %>% kable_styling()
summary_major_fabric3

# Fabric 4:
filter(new_data, fabric == "4")[5:34] %>% summary() %>% kable(caption = "Fabric 4") %>% kable_styling()
summary_major_fabric4 <- filter(new_data, fabric == "4") %>% 
  summarize(avg_SiO2 = round(mean(SiO2),2), sd_SiO2 = round(sd(SiO2),2), 
            avg_TiO2 = round(mean(TiO2),2), sd_TiO2 = round(sd(TiO2),2), 
            avg_Al2O3 = round(mean(Al2O3),2), sd_Al2O3 = round(sd(Al2O3),2), 
            avg_Fe2O3TOT = round(mean(Fe2O3TOT),2), sd_Fe2O3TOT = round(sd(Fe2O3TOT),2), 
            avg_MnO = round(mean(MnO),2), sd_MnO = round(sd(MnO),2), 
            avg_MgO = round(mean(MgO),2), sd_MgO = round(sd(MgO),2), 
            avg_CaO = round(mean(CaO),2), sd_CaO = round(sd(CaO),2), 
            avg_Na2O = round(mean(Na2O),2), sd_Na2O = round(sd(Na2O),2), 
            avg_K2O = round(mean(K2O),2), sd_K2O = round(sd(K2O),2), 
            avg_P2O5 = round(mean(P2O5),2), sd_P2O5 = round(sd(P2O5),2))
summary_major_fabric4 <- data.frame("Major elements (weight percent)" = c("SiO2","TiO2","Al2O3","Fe2O3TOT","MnO","MgO","CaO","Na2O","K2O","P2O5"),
                                    "avg" = c(summary_major_fabric4$avg_SiO2,summary_major_fabric4$avg_TiO2,summary_major_fabric4$avg_Al2O3,summary_major_fabric4$avg_Fe2O3TOT,summary_major_fabric4$avg_MnO,summary_major_fabric4$avg_MgO,summary_major_fabric4$avg_CaO,summary_major_fabric4$avg_Na2O,summary_major_fabric4$avg_K2O,summary_major_fabric4$avg_P2O5), 
                                    "sd" = c(summary_major_fabric4$sd_SiO2,summary_major_fabric4$sd_TiO2,summary_major_fabric4$sd_Al2O3,summary_major_fabric4$sd_Fe2O3TOT,summary_major_fabric4$sd_MnO,summary_major_fabric4$sd_MgO,summary_major_fabric4$sd_CaO,summary_major_fabric4$sd_Na2O,summary_major_fabric4$sd_K2O,summary_major_fabric4$sd_P2O5))%>%
  kable(caption = "Fabric 4") %>% kable_styling()
summary_major_fabric4

##############################################################################
##############################################################################
# 1. SiO2
print("SiO2")

##############################################################################
# Box plot grouped by fabrics: major elements
b1 <- new_data %>% ggplot(aes(x = fabric, y = SiO2, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b1

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h1_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(SiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(SiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("SiO2 - fabric 1")+
  xlim(min(new_data$SiO2)-7*mean(new_data$SiO2/100),max(new_data$SiO2)+7*mean(new_data$SiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h1_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(SiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(SiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("SiO2 - fabric 2")+
  xlim(min(new_data$SiO2)-7*mean(new_data$SiO2/100),max(new_data$SiO2)+7*mean(new_data$SiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h1_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(SiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(SiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("SiO2 - fabric 3")+
  xlim(min(new_data$SiO2)-7*mean(new_data$SiO2/100),max(new_data$SiO2)+7*mean(new_data$SiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h1_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(SiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(SiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("SiO2 - fabric 4")+
  xlim(min(new_data$SiO2)-7*mean(new_data$SiO2/100),max(new_data$SiO2)+7*mean(new_data$SiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h1_1,h1_2,h1_3,h1_4,ncol = 2, top = "SiO2")

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$SiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq1_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$SiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq1_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$SiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm(p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq1_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
x1 <- filter(new_data, fabric == "4")$SiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,"observed_quantiles" = observed_quantiles)
qq1_4 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 4")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(qq1_1,qq1_2,qq1_3,qq1_4,ncol = 2, top = "SiO2")

# shapiro - test
# H0: normal distribution: p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$SiO2)
shapiro.test(filter(new_data, fabric == "2")$SiO2)
shapiro.test(filter(new_data, fabric == "3")$SiO2)
shapiro.test(filter(new_data, fabric == "4")$SiO2)

##############################################################################
##############################################################################
# 2. TiO2
print("TiO2")

##############################################################################
# Box plot grouped by fabrics: major elements
b2 <- new_data %>% ggplot(aes(x = fabric, y = TiO2, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b2

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h2_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(TiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(TiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("TiO2 - fabric 1")+
  xlim(min(new_data$TiO2)-7*mean(new_data$TiO2/100),max(new_data$TiO2)+7*mean(new_data$TiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h2_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(TiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(TiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("TiO2 - fabric 2")+
  xlim(min(new_data$TiO2)-7*mean(new_data$TiO2/100),max(new_data$TiO2)+7*mean(new_data$TiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h2_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(TiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(TiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("TiO2 - fabric 3")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))+
  xlim(min(new_data$TiO2)-7*mean(new_data$TiO2/100),max(new_data$TiO2)+7*mean(new_data$TiO2/100))
# Fabric 4
h2_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(TiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(TiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("TiO2 - fabric 4")+
  xlim(min(new_data$TiO2)-7*mean(new_data$TiO2/100),max(new_data$TiO2)+7*mean(new_data$TiO2/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h2_1,h2_2,h2_3,h2_4,ncol = 2)

##############################################################################
##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$TiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq2_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$TiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq2_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$TiO2
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq2_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq2_1,qq2_2,qq2_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$TiO2)
shapiro.test(filter(new_data, fabric == "2")$TiO2)
shapiro.test(filter(new_data, fabric == "3")$TiO2)
shapiro.test(filter(new_data, fabric == "4")$TiO2)

##############################################################################
##############################################################################
# 3. Al2O3
print("Al2O3")

##############################################################################
# Box plot grouped by fabrics: major elements
b3 <- new_data %>% ggplot(aes(x = fabric, y = Al2O3, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b3

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h3_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(Al2O3))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Al2O3, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Al2O3 - fabric 1")+
  xlim(min(new_data$Al2O3)-7*mean(new_data$Al2O3/100),max(new_data$Al2O3)+7*mean(new_data$Al2O3/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h3_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(Al2O3))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Al2O3, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Al2O3 - fabric 2")+
  xlim(min(new_data$Al2O3)-7*mean(new_data$Al2O3/100),max(new_data$Al2O3)+7*mean(new_data$Al2O3/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h3_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(Al2O3))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Al2O3, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Al2O3 - fabric 3")+
  xlim(min(new_data$Al2O3)-7*mean(new_data$Al2O3/100),max(new_data$Al2O3)+7*mean(new_data$Al2O3/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h3_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(Al2O3))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Al2O3, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Al2O3 - fabric 4")+
  xlim(min(new_data$Al2O3)-7*mean(new_data$Al2O3/100),max(new_data$Al2O3)+7*mean(new_data$Al2O3/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h3_1,h3_2,h3_3,h3_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$Al2O3
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq3_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$Al2O3
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq3_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$Al2O3
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq3_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq3_1,qq3_2,qq3_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution
# p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$Al2O3)
shapiro.test(filter(new_data, fabric == "2")$Al2O3)
shapiro.test(filter(new_data, fabric == "3")$Al2O3)
shapiro.test(filter(new_data, fabric == "4")$Al2O3)

##############################################################################
##############################################################################
# 4. Fe2O3TOT
print("Fe2O3TOT")

##############################################################################
# Box plot grouped by fabrics: major elements
b4 <- new_data %>% ggplot(aes(x = fabric, y = Fe2O3TOT, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b4

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h4_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(Fe2O3TOT))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Fe2O3TOT, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Fe2O3TOT - fabric 1")+
  xlim(min(new_data$Fe2O3TOT)-7*mean(new_data$Fe2O3TOT/100),max(new_data$Fe2O3TOT)+7*mean(new_data$Fe2O3TOT/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h4_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(Fe2O3TOT))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Fe2O3TOT, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Fe2O3TOT - fabric 2")+
  xlim(min(new_data$Fe2O3TOT)-7*mean(new_data$Fe2O3TOT/100),max(new_data$Fe2O3TOT)+7*mean(new_data$Fe2O3TOT/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h4_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(Fe2O3TOT))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Fe2O3TOT, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Fe2O3TOT - fabric 3")+
  xlim(min(new_data$Fe2O3TOT)-7*mean(new_data$Fe2O3TOT/100),max(new_data$Fe2O3TOT)+7*mean(new_data$Fe2O3TOT/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h4_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(Fe2O3TOT))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Fe2O3TOT, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Fe2O3TOT - fabric 4")+
  xlim(min(new_data$Fe2O3TOT)-7*mean(new_data$Fe2O3TOT/100),max(new_data$Fe2O3TOT)+7*mean(new_data$Fe2O3TOT/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h4_1,h4_2,h4_3,h4_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$Fe2O3TOT
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq4_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$Fe2O3TOT
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq4_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$Fe2O3TOT
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq4_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq4_1,qq4_2,qq4_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$Fe2O3TOT)
shapiro.test(filter(new_data, fabric == "2")$Fe2O3TOT)
shapiro.test(filter(new_data, fabric == "3")$Fe2O3TOT)
shapiro.test(filter(new_data, fabric == "4")$Fe2O3TOT)

##############################################################################
##############################################################################
# 5. MnO
print("MnO")

##############################################################################
# Box plot grouped by fabrics: major elements
b5 <- new_data %>% ggplot(aes(x = fabric, y = MnO, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b5

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h5_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(MnO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MnO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MnO - fabric 1")+
  xlim(min(new_data$MnO)-0.1,max(new_data$MnO)+0.1)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h5_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(MnO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MnO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MnO - fabric 2")+
  xlim(min(new_data$MnO)-0.1,max(new_data$MnO)+0.1)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h5_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(MnO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MnO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MnO - fabric 3")+
  xlim(min(new_data$MnO)-0.1,max(new_data$MnO)+0.1)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h5_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(MnO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MnO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MnO - fabric 4")+
  xlim(min(new_data$MnO)-0.1,max(new_data$MnO)+0.1)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h5_1,h5_2,h5_3,h5_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$MnO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq5_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$MnO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq5_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$MnO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq5_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq2_1,qq2_2,qq2_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$MnO)
shapiro.test(filter(new_data, fabric == "2")$MnO)
shapiro.test(filter(new_data, fabric == "3")$MnO)
shapiro.test(filter(new_data, fabric == "4")$MnO)

##############################################################################
##############################################################################
# 6. MgO
print("MgO")

##############################################################################
# Box plot grouped by fabrics: major elements
b6 <- new_data %>% ggplot(aes(x = fabric, y = MgO, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b6

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h6_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(MgO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MgO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MgO - fabric 1")+
  xlim(min(new_data$MgO)-7*mean(new_data$MgO/100),max(new_data$MgO)+7*mean(new_data$MgO/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h6_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(MgO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MgO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MgO - fabric 2")+
  xlim(min(new_data$MgO)-7*mean(new_data$MgO/100),max(new_data$MgO)+7*mean(new_data$MgO/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h6_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(MgO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MgO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MgO - fabric 3")+
  xlim(min(new_data$MgO)-7*mean(new_data$MgO/100),max(new_data$MgO)+7*mean(new_data$MgO/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h6_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(MgO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(MgO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("MgO - fabric 4")+
  xlim(min(new_data$MgO)-7*mean(new_data$MgO/100),max(new_data$MgO)+7*mean(new_data$MgO/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h6_1,h6_2,h6_3,h6_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$MgO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq6_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$MgO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq6_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$MgO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq6_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq6_1,qq6_2,qq6_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$MgO)
shapiro.test(filter(new_data, fabric == "2")$MgO)
shapiro.test(filter(new_data, fabric == "3")$MgO)
shapiro.test(filter(new_data, fabric == "4")$MgO)


##############################################################################
##############################################################################
# 7. CaO
print("CaO")

##############################################################################
# Box plot grouped by fabrics: major elements
b7 <- new_data %>% ggplot(aes(x = fabric, y = CaO, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b7

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h7_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(CaO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(CaO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("CaO - fabric 1")+
  xlim(min(new_data$CaO)-7*mean(new_data$MgO/100),5)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h7_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(CaO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(CaO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("CaO - fabric 2")+
  xlim(min(new_data$CaO)-7*mean(new_data$CaO/100),5)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h7_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(CaO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(CaO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("CaO - fabric 3")+
  xlim(min(new_data$CaO)-7*mean(new_data$CaO/100),5)+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h7_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(CaO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(CaO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("CaO - fabric 4")+
  xlim(min(new_data$CaO)-7*mean(new_data$CaO/100),max(new_data$CaO)+7*mean(new_data$CaO/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
grid.arrange(h7_1,h7_2,h7_3,h7_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$CaO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq7_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$CaO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq7_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$CaO
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq7_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq7_1,qq7_2,qq7_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$CaO)
shapiro.test(filter(new_data, fabric == "2")$CaO)
shapiro.test(filter(new_data, fabric == "3")$CaO)
shapiro.test(filter(new_data, fabric == "4")$CaO)

##############################################################################
##############################################################################
# 8. Na2O
print("Na2O")

##############################################################################
# Box plot grouped by fabrics: major elements
b8 <- new_data %>% ggplot(aes(x = fabric, y = Na2O, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b8

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h8_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(Na2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Na2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Na2O - fabric 1")+
  xlim(min(new_data$Na2O)-7*mean(new_data$Na2O/100),max(new_data$Na2O)+7*mean(new_data$Na2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h8_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(Na2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Na2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Na2O - fabric 2")+
  xlim(min(new_data$Na2O)-7*mean(new_data$Na2O/100),max(new_data$Na2O)+7*mean(new_data$Na2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h8_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(Na2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(Na2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("Na2O - fabric 3")+
  xlim(min(new_data$Na2O)-7*mean(new_data$Na2O/100),max(new_data$Na2O)+7*mean(new_data$Na2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
grid.arrange(h8_1,h8_2,h8_3,h8_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$Na2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq8_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$Na2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq8_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$Na2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq8_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq8_1,qq8_2,qq8_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$Na2O)
shapiro.test(filter(new_data, fabric == "2")$Na2O)
shapiro.test(filter(new_data, fabric == "3")$Na2O)
shapiro.test(filter(new_data, fabric == "4")$Na2O)

##############################################################################
##############################################################################
# 9. K2O
print("K2O")

##############################################################################
# Box plot grouped by fabrics: major elements
b9 <- new_data %>% ggplot(aes(x = fabric, y = K2O, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b9

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h9_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(K2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(K2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("K2O - fabric 1")+
  xlim(min(new_data$K2O)-7*mean(new_data$K2O/100),max(new_data$K2O)+7*mean(new_data$K2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h9_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(K2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(K2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("K2O - fabric 2")+
  xlim(min(new_data$K2O)-7*mean(new_data$K2O/100),max(new_data$K2O)+7*mean(new_data$K2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h9_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(K2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(K2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("K2O - fabric 3")+
  xlim(min(new_data$K2O)-7*mean(new_data$K2O/100),max(new_data$K2O)+7*mean(new_data$K2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h9_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(K2O))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(K2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("K2O - fabric 4")+
  xlim(min(new_data$K2O)-7*mean(new_data$K2O/100),max(new_data$K2O)+7*mean(new_data$K2O/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h9_1,h9_2,h9_3,h9_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$K2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq9_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$K2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq9_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$K2O
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq9_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq9_1,qq9_2,qq9_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$K2O)
shapiro.test(filter(new_data, fabric == "2")$K2O)
shapiro.test(filter(new_data, fabric == "3")$K2O)
shapiro.test(filter(new_data, fabric == "4")$K2O)

##############################################################################
##############################################################################
# 10. P2O5
print("P2O5")

##############################################################################
# Box plot grouped by fabrics: major elements
b10 <- new_data %>% ggplot(aes(x = fabric, y = P2O5, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
b10

##############################################################################
# Histograms of major elements overlapped by the smooth curve:
# Fabric 1
h10_1 <- filter(new_data, fabric == "1") %>% 
  ggplot(aes(P2O5))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(P2O5, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("P2O5 - fabric 1")+
  xlim(min(new_data$P2O5)-7*mean(new_data$P2O5/100),max(new_data$P2O5)+7*mean(new_data$P2O5/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
h10_2 <- filter(new_data, fabric == "2") %>% 
  ggplot(aes(P2O5))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(P2O5, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("P2O5 - fabric 2")+
  xlim(min(new_data$P2O5)-7*mean(new_data$P2O5/100),max(new_data$P2O5)+7*mean(new_data$P2O5/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
h10_3 <- filter(new_data, fabric == "3") %>% 
  ggplot(aes(P2O5))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(P2O5, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("P2O5 - fabric 3")+
  xlim(min(new_data$P2O5)-7*mean(new_data$P2O5/100),max(new_data$P2O5)+7*mean(new_data$P2O5/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
h10_4 <- filter(new_data, fabric == "4") %>% 
  ggplot(aes(P2O5))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 20, fill = "light blue", color = "white") + 
  geom_line(aes(P2O5, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  xlab("P2O5 - fabric 4")+
  xlim(min(new_data$P2O5)-7*mean(new_data$P2O5/100),max(new_data$P2O5)+7*mean(new_data$P2O5/100))+
  theme(panel.background = element_rect(fill = "white",colour = "white"),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
grid.arrange(h10_1,h10_2,h10_3,h10_4,ncol = 2)

##############################################################################
# Normality test:
# Quantile-quantile QQ plots
p <- seq(0.05, 0.95, 0.05)
# Fabric 1
x1 <- filter(new_data, fabric == "1")$P2O5
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq10_1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 1")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 2
x1 <- filter(new_data, fabric == "2")$P2O5
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq10_2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 2")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 3
x1 <- filter(new_data, fabric == "3")$P2O5
observed_quantiles <- quantile(x1, p)
theoretical_quantiles <- qnorm( p, mean = mean(x1), sd = sd(x1))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles, "observed_quantiles" = observed_quantiles)
qq10_3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  ggtitle("F. 3")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Fabric 4
# ...
grid.arrange(qq10_1,qq10_2,qq10_3,qq1_4,ncol = 2, top = "TiO2")

# shapiro - test
# H0: normal distribution; p-value > 0.05: H0 accepted
shapiro.test(filter(new_data, fabric == "1")$P2O5)
shapiro.test(filter(new_data, fabric == "2")$P2O5)
shapiro.test(filter(new_data, fabric == "3")$P2O5)
shapiro.test(filter(new_data, fabric == "4")$P2O5)
