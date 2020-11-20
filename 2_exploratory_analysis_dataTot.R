# Exploratory analysis:
# 2.1 analysis of the entire data
# 2.2 analysis of data grouped by fabrics(groups observed through petrographic analysis)

##############################################################################
##############################################################################
# 2.1 analysis of the entire data

##############################################################################
# Summary of the data:
str(new_data)
summary(new_data[5:34]) %>% kable() %>% kable_styling()

# Average and standard:
# Major elements:
summary_major <- new_data %>% 
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
summary_major <- data.frame("Major elements (weight percent)" = c("SiO2","TiO2","Al2O3","Fe2O3TOT","MnO","MgO","CaO","Na2O","K2O","P2O5"),
    "avg" = c(summary_major$avg_SiO2,summary_major$avg_TiO2,summary_major$avg_Al2O3,summary_major$avg_Fe2O3TOT,summary_major$avg_MnO,summary_major$avg_MgO,summary_major$avg_CaO,summary_major$avg_Na2O,summary_major$avg_K2O,summary_major$avg_P2O5), 
    "sd" = c(summary_major$sd_SiO2,summary_major$sd_TiO2,summary_major$sd_Al2O3,summary_major$sd_Fe2O3TOT,summary_major$sd_MnO,summary_major$sd_MgO,summary_major$sd_CaO,summary_major$sd_Na2O,summary_major$sd_K2O,summary_major$sd_P2O5))%>%
  kable() %>% kable_styling()
summary_major 

# Trace elements:
summary_ppm <- new_data %>% 
  summarize(avg_S = round(mean(S),2), sd_S = round(sd(S),2), 
            avg_V = round(mean(V),2), sd_V = round(sd(V),2), 
            avg_Cr = round(mean(Cr),2), sd_Cr = round(sd(Cr),2), 
            avg_Co = round(mean(Co),2), sd_Co = round(sd(Co),2), 
            avg_Ni = round(mean(Ni),2), sd_Ni = round(sd(Ni),2), 
            avg_Cu = round(mean(Cu),2), sd_Cu = round(sd(Cu),2), 
            avg_Zn = round(mean(Zn),2), sd_Zn = round(sd(Zn),2), 
            avg_Ga = round(mean(Ga),2), sd_Ga = round(sd(Ga),2), 
            avg_Rb = round(mean(Rb),2), sd_Rb = round(sd(Rb),2), 
            avg_Sr = round(mean(Sr),2), sd_Sr = round(sd(Sr),2), 
            avg_Y = round(mean(Y),2), sd_Y = round(sd(Y),2), 
            avg_Zr = round(mean(Zr),2), sd_Zr = round(sd(Zr),2), 
            avg_Nb = round(mean(Nb),2), sd_Nb = round(sd(Nb),2), 
            avg_Ba = round(mean(Ba),2), sd_Ba = round(sd(Ba),2), 
            avg_La = round(mean(La),2), sd_La = round(sd(La),2), 
            avg_Ce = round(mean(Ce),2), sd_Ce = round(sd(Ce),2), 
            avg_Nd = round(mean(Nd),2), sd_Nd = round(sd(Nd),2), 
            avg_Pb = round(mean(Pb),2), sd_Pb = round(sd(Pb),2), 
            avg_Th = round(mean(Th),2), sd_Th = round(sd(Th),2), 
            avg_U = round(mean(U),2), sd_U = round(sd(U),2))

summary_ppm <- data.frame("Minor elements (ppm)"  = c("S","V","Cr","Co","Ni","Cu","Zn","Ga","Rb","Sr","Y","Zr","Nb","Ba","La","Ce","Nd","Pb","Th","U" ),
                "avg" = c(summary_ppm$avg_S,summary_ppm$avg_V,summary_ppm$avg_Cr,summary_ppm$avg_Co,summary_ppm$avg_Ni,summary_ppm$avg_Cu,summary_ppm$avg_Zn,summary_ppm$avg_Ga,
                          summary_ppm$avg_Rb,summary_ppm$avg_Sr,summary_ppm$avg_Y,summary_ppm$avg_Zr,summary_ppm$avg_Nb,summary_ppm$avg_Ba,summary_ppm$avg_La,summary_ppm$avg_Ce,
                          summary_ppm$avg_Nd,summary_ppm$avg_Pb,summary_ppm$avg_Th,summary_ppm$avg_U),
                "sd" = c(summary_ppm$sd_S,summary_ppm$sd_V,summary_ppm$sd_Cr,summary_ppm$sd_Co,summary_ppm$sd_Ni,summary_ppm$sd_Cu,summary_ppm$sd_Zn,
                         summary_ppm$sd_Ga,summary_ppm$sd_Rb,summary_ppm$sd_Sr,summary_ppm$sd_Y,summary_ppm$sd_Zr,summary_ppm$sd_Nb,summary_ppm$sd_Ba,
                         summary_ppm$sd_La,summary_ppm$sd_Ce,summary_ppm$sd_Nd,summary_ppm$sd_Pb,summary_ppm$sd_Th,summary_ppm$sd_U))%>%
  kable() %>% kable_styling()
summary_ppm

##############################################################################
print("Major elements:")
# SiO2
# Histogram and Box plot
# Histograms of major elements overlapped by the smooth curve:
## Histograms of the major and minor elements overlapped by the smooth curve:
h1 <- new_data %>% ggplot(aes(SiO2))+ 
  scale_y_sqrt()+ 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(SiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

# Box plot grouped by fabrics
b1 <- new_data %>% ggplot(aes(x = fabric, y = SiO2, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
# QQ plots: assess how well the normal distribution fits the data,
# 1. Define a series of proportions p = ...; 
# 2. For each, determine the value so that the proportion of values in the data below q(=quantiles) is p
p <- seq(0.05, 0.95, 0.05)
x <- new_data$SiO2
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))  # theoretical normal distribution quantiles, with corresponding avg and sd:
# see if they match, plot them against each other and draw the identity line:
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq1 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h1,b1,qq1,ncol = 2,top = "SiO2")

# with standard units
z <- scale(x)
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles, main="SiO2")
abline(0,1)

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
# ascertain whether data show or not a serious deviation from normality
# H0: normal distribution
shapiro.test(new_data$SiO2) # p-value > 0.05: Normal distrbution

##############################################################################
# 2. TiO2
# Histogram and Box plot
h2 <- new_data %>% ggplot(aes(TiO2))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(TiO2, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b2 <- new_data %>% ggplot(aes(x = fabric,y = TiO2,group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$TiO2
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq2 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h2,b2,qq2,ncol = 2, top = "TiO2")

# shapiro-wilk's: 
shapiro.test(new_data$TiO2) # p-value > 0.05: Normal distrbution

##############################################################################
# 3. Al2O3
# Histogram and Box plot
h3 <- new_data %>% ggplot(aes(Al2O3))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Al2O3, y = ..count..), stat="density", bw = 1, size =  1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b3 <- new_data %>% ggplot(aes(x = fabric,y = Al2O3,group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
# QQ plots: assess how well the normal distribution fits the data,
x <- new_data$Al2O3
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq3 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h3,b3,qq3,ncol = 2, top = "Al2O3")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Al2O3) # p-value > 0.05: Normal distrbution

##############################################################################
# 4. Fe2O3TOT
# Histogram and Box plot
h4 <- new_data %>% ggplot(aes(Fe2O3TOT))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Fe2O3TOT, y = ..count..), stat="density", bw =1, size =  1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))+
  xlab("Fe2O3tot")
# Box plot grouped by fabrics
b4 <- new_data %>% ggplot(aes(x = fabric,y = Fe2O3TOT,group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric", y = "Fe2Otot")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Fe2O3TOT
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq4 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
grid.arrange(h4,b4,qq4,ncol = 2, top = "Fe2O3tot")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Fe2O3TOT) # p-value > 0.05: Normal distrbution

##############################################################################
# 5. MnO
# Histogram and Box plot
h5 <- new_data %>% ggplot(aes(MnO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(MnO, y = ..count..), stat="density", bw = 1, size =  1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b5 <- new_data %>% ggplot(aes(x = fabric,y = MnO,group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$MnO
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq5 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h5,b5,qq5,ncol = 2, top = "MnO")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$MnO) # p-value > 0.05: Normal distrbution

##############################################################################
# 6. MgO
# Histogram and Box plot
h6 <- new_data %>% ggplot(aes(MgO))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(MgO, y = ..count..), stat="density", bw = 1, size =  1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b6 <- new_data %>% ggplot(aes(x = fabric,y = MgO,group = fabric))  + 
  geom_boxplot() +
  labs(x ="fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$MgO
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq6 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h6,b6,qq6,ncol = 2, top = "MgO")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$MgO) # p-value > 0.05: Normal distrbution

##############################################################################
# 7. CaO
# Histogram and Box plot
h7 <- new_data %>% ggplot(aes(CaO)) +
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(CaO, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b7 <- new_data %>% ggplot(aes(x = fabric,y = CaO,group = fabric))  + 
  geom_boxplot()+
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$CaO
z <- scale(x)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq7 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h7,b7,qq7,ncol = 2, top = "CaO")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$CaO) # p-value > 0.05: Normal distrbution

##############################################################################
# 8. Na2O
# Histogram and Box plot
h8 <- new_data %>% ggplot(aes(Na2O))+
  scale_y_sqrt()+
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Na2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b8 <- new_data %>% ggplot(aes(x = fabric,y = Na2O,group = fabric))  + 
  geom_boxplot()+
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Na2O
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq8 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h8,b8,qq8,ncol = 2, top = "Na2O")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Na2O) # p-value > 0.05: Normal distrbution

##############################################################################
# 9. K2O
# Histogram and Box plot
h9 <- new_data %>% ggplot(aes(K2O))+
  scale_y_sqrt()+
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(K2O, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b9 <- new_data %>% ggplot(aes(x = fabric,y = K2O,group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$K2O
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq9 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h9,b9,qq9,ncol = 2, top = "K2O")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$K2O) # p-value > 0.05: Normal distrbution

##############################################################################
# 10. P2O5
# Histogram and Box plot
h10 <- new_data %>% ggplot(aes(P2O5))+
  scale_y_sqrt()+
  geom_histogram(bins = 10, fill = "grey", color = "white")+
  geom_line(aes(P2O5, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b10 <- new_data %>% ggplot(aes(x = fabric, y = P2O5, group = fabric))  + 
  geom_boxplot()+
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$P2O5
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq10 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h10,b10,qq10,ncol = 2, top = "P2O5")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$P2O5) # p-value > 0.05: Normal distrbution


##############################################################################
##############################################################################
print("Trace elements:")
# S
# Histogram and Box plot
h11 <- new_data %>% ggplot(aes(S))+
  scale_y_sqrt()+
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(S, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b11 <- new_data %>% ggplot(aes(x = fabric, y = S, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$S
z <- scale(x)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq11 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h11,b11,qq11,ncol = 2, top = "S")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$S) # p-value > 0.05: Normal distrbution

##############################################################################
# V
# Histogram and Box plot
h12 <- new_data %>% ggplot(aes(V))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(V, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b12 <- new_data %>% ggplot(aes(x = fabric, y = V, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$V
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq12 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h12,b12,qq12,ncol = 2, top = "V")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$V) # p-value > 0.05: Normal distrbution

##############################################################################
# Cr
# Histogram and Box plot
h13 <- new_data %>% ggplot(aes(Cr))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Cr, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b13 <- new_data %>% ggplot(aes(x = fabric, y = Cr, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Cr
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq13 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h13,b13,qq13,ncol = 2, top = "Cr")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Cr) # p-value > 0.05: Normal distrbution

##############################################################################
# Co
# Histogram and Box plot
h14 <- new_data %>% ggplot(aes(Co))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Co, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b14 <- new_data %>% ggplot(aes(x = fabric, y = Co, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Co
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq14 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h14,b14,qq14,ncol = 2, top = "Co")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Co) # p-value > 0.05: Normal distrbution

##############################################################################
# Ni
# Histogram and Box plot
h15 <- new_data %>% ggplot(aes(Ni))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Ni, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b15 <- new_data %>% ggplot(aes(x = fabric, y = Ni, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Ni
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq15 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h15,b15,qq15,ncol = 2, top = "Ni")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Ni) # p-value > 0.05: Normal distrbution

##############################################################################
# Cu
# Histogram and Box plot
h16 <- new_data %>% ggplot(aes(Cu))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Cu, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b16 <- new_data %>% ggplot(aes(x = fabric, y = Cu, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Cu
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq16 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h16,b16,qq16,ncol = 2, top = "Cu")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Cu) # p-value > 0.05: Normal distrbution

##############################################################################
# Zn
# Histogram and Box plot
h17 <- new_data %>% ggplot(aes(Zn))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Zn, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b17 <- new_data %>% ggplot(aes(x = fabric, y = Zn, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Zn
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq17 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h17,b17,qq17,ncol = 2, top = "Zn")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Zn) # p-value > 0.05: Normal distrbution

##############################################################################
# Ga
# Histogram and Box plot
h18 <- new_data %>% ggplot(aes(Ga))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Ga, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b18 <- new_data %>% ggplot(aes(x = fabric, y = Ga, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Ga
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq18 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h18,b18,qq18,ncol = 2, top = "Ga")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Ga) # p-value > 0.05: Normal distrbution

##############################################################################
# Rb
# Histogram and Box plot
h19 <- new_data %>% ggplot(aes(Rb))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Rb, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b19 <- new_data %>% ggplot(aes(x = fabric, y = Rb, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Rb
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq19 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h19,b19,qq19,ncol = 2, top = "Rb")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Rb) # p-value > 0.05: Normal distrbution

##############################################################################
# Sr
# Histogram and Box plot
h20 <- new_data %>% ggplot(aes(Sr))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Sr, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b20 <- new_data %>% ggplot(aes(x = fabric, y = Sr, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Sr
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq20 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h20,b20,qq20,ncol = 2, top = "Sr")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Sr) # p-value > 0.05: Normal distrbution

##############################################################################
# Y
# Histogram and Box plot
h21 <- new_data %>% ggplot(aes(Y))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Y, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b21 <- new_data %>% ggplot(aes(x = fabric, y = Y, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Y
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq21 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h21,b21,qq21,ncol = 2, top = "Y")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Y) # p-value > 0.05: Normal distrbution

##############################################################################
# Zr
# Histogram and Box plot
h22 <- new_data %>% ggplot(aes(Zr))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Zr, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b22 <- new_data %>% ggplot(aes(x = fabric, y = Zr, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Zr
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq22 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h22,b22,qq22,ncol = 2, top = "Zr")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Zr) # p-value > 0.05: Normal distrbution

##############################################################################
# Nb
# Histogram and Box plot
h23 <- new_data %>% ggplot(aes(Nb))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Nb, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b23 <- new_data %>% ggplot(aes(x = fabric, y = Nb, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Nb
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq23 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h23,b23,qq23,ncol = 2, top = "Nb")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Nb) # p-value > 0.05: Normal distrbution

##############################################################################
# Ba
# Histogram and Box plot
h24 <- new_data %>% ggplot(aes(Ba))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Ba, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b24 <- new_data %>% ggplot(aes(x = fabric, y = Ba, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Ba
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq24 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h24,b24,qq24,ncol = 2, top = "Ba")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Ba) # p-value > 0.05: Normal distrbution

##############################################################################
# La
# Histogram and Box plot
h25 <- new_data %>% ggplot(aes(La))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(La, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b25 <- new_data %>% ggplot(aes(x = fabric, y = La, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$La
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq25 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h25,b25,qq25,ncol = 2, top = "La")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$La) # p-value > 0.05: Normal distrbution

##############################################################################
# Ce
# Histogram and Box plot
h26 <- new_data %>% ggplot(aes(Ce))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Ce, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b26 <- new_data %>% ggplot(aes(x = fabric, y = Ce, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Ce
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq26 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h26,b26,qq26,ncol = 2, top = "Ce")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Ce) # p-value > 0.05: Normal distrbution

##############################################################################
# Nd
# Histogram and Box plot
h27 <- new_data %>% ggplot(aes(Nd))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Nd, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b27 <- new_data %>% ggplot(aes(x = fabric, y = Nd, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Nd
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq27 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h27,b27,qq27,ncol = 2, top = "Nd")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Nd) # p-value > 0.05: Normal distrbution

##############################################################################
# Pb
# Histogram and Box plot
h28 <- new_data %>% ggplot(aes(Pb))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Pb, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b28 <- new_data %>% ggplot(aes(x = fabric, y = Pb, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Pb
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq28 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h28,b28,qq28,ncol = 2, top = "Pb")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Pb) # p-value > 0.05: Normal distrbution

##############################################################################
# Th
# Histogram and Box plot
h29 <- new_data %>% ggplot(aes(Th))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(Th, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b29 <- new_data %>% ggplot(aes(x = fabric, y = Th, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$Th
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq29 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h29,b29,qq29,ncol = 2, top = "Th")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$Th) # p-value > 0.05: Normal distrbution

##############################################################################
# U
# Histogram and Box plot
h30 <- new_data %>% ggplot(aes(U))  + 
  scale_y_sqrt()  + 
  geom_histogram(bins = 10, fill = "grey", color = "white") + 
  geom_line(aes(U, y = ..count..), stat="density", bw = 1, size = 1, color = "blue")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))
# Box plot grouped by fabrics
b30 <- new_data %>% ggplot(aes(x = fabric, y = U, group = fabric))  + 
  geom_boxplot() +
  labs(x = "fabric")+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

##############################################################################
# Normality test:
# Visual methods: Density plot and QQ plot
x <- new_data$U
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
qq_df <- data.frame("theoretical_quantiles"  = theoretical_quantiles,
                    "observed_quantiles" = observed_quantiles)
qq30 <- qq_df %>% ggplot(aes(x=theoretical_quantiles, y=observed_quantiles))+ 
  geom_point()+
  geom_abline()+
  theme(panel.background = element_blank(),
        axis.line = element_line(size = 0.2,linetype = 'solid',color = "black"))

grid.arrange(h30,b30,qq30,ncol = 2, top = "U")

# shapiro-wilk's: significance test comparing the sample distribution to a normal one
shapiro.test(new_data$U) # p-value > 0.05: Normal distrbution
