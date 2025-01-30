library(ggplot2)
library(data.table)
library(dplyr)

Pourcentage=as.data.frame(fread("WT_ABCD.csv",na.strings=c("",NA,"NULL"),dec="."))
Inoc <- Pourcentage[Pourcentage$Condition == "RB84"| Pourcentage$Condition == "All2",]

Col=c("#FF3000","#75b8d1","#d18975", "#B276B2", "#60BD68" )
Col_gg=c("#619CFF","#F8766D","#00BA38")


### Just M (n°76)
Pourcentage_M <- Pourcentage[Pourcentage$Genotype %in% c("M"), ]
Inoc_M <- Pourcentage_M[Pourcentage_M$Condition == "RB84"| Pourcentage_M$Condition == "All2",]
attach(Pourcentage_M)
pairwise.wilcox.test(Poids,Condition,p.adjust.method="bonferroni")

attach(Inoc_M)
pairwise.wilcox.test(Pourcentage_3dpi,Condition,p.adjust.method="bonferroni")
pairwise.wilcox.test(Pourcentage_5dpi,Condition,p.adjust.method="bonferroni")
pairwise.wilcox.test(Pourcentage_7dpi,Condition,p.adjust.method="bonferroni")
pairwise.wilcox.test(Pourcentage_10dpi,Condition,p.adjust.method="bonferroni")
pairwise.wilcox.test(Pourcentage_12dpi,Condition,p.adjust.method="bonferroni")

ggplot(Pourcentage_M, aes(x = Condition, y = Poids, fill=Condition, alpha=0.5)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.7, lwd = 0.7) +
  geom_jitter(aes(color = Condition, shape = Rep), size = 2, width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = Col_gg) +
  scale_color_manual(values = Col_gg) +
  facet_grid(~ Genotype, labeller = labeller(Genotype = as_labeller(c(
    'M' = "M" ), default = label_parsed))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  ylim(0.02, 0.15) +
  ggtitle("Fresh weight 21 dpi") +
  xlab("Condition") +
  ylab("Weight (g)") +
  scale_x_discrete(limits = c("ni","ATCC201834", "RB84")) +
  theme_classic(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 12))

Inoc_loop=Pourcentage_M %>% select(Condition, Pourcentage_3dpi, Pourcentage_5dpi, Pourcentage_7dpi, Pourcentage_10dpi, Pourcentage_12dpi)
day=3
mean_per_cond=Pourcentage_M %>% group_by(Condition) %>% summarise(moy_perc=mean(!!as.symbol(paste0("Pourcentage_",day,"dpi")), na.rm=T))
mean_per_cond$day=rep(day, nrow(mean_per_cond))


for (day in c(3,5,7,10,12)) {
  #day=3
  mean_per_cond_interm=Pourcentage_M %>% group_by(Condition) %>% summarise(moy_perc=mean(!!as.symbol(paste0("Pourcentage_",day,"dpi")), na.rm=T))
  mean_per_cond_interm$day=rep(day, nrow(mean_per_cond_interm))
  mean_per_cond=bind_rows(mean_per_cond, mean_per_cond_interm)
  }


mean_per_cond$Condition <- recode(mean_per_cond$Condition, "All2" = "ATCC201834")
mean_per_cond <- data.frame()
for (day in c(3, 5, 7, 10, 12)) {
  col_name <- paste0("Pourcentage_", day, "dpi")
  mean_per_cond_interm <- Pourcentage_M %>% 
    group_by(Condition) %>% 
    summarise(
      moy_perc = mean(!!as.symbol(col_name), na.rm = TRUE),
      se_perc = sd(!!as.symbol(col_name), na.rm = TRUE) / sqrt(n())
    )
  mean_per_cond_interm$day <- day
  mean_per_cond <- bind_rows(mean_per_cond, mean_per_cond_interm)
}

mean_per_cond$day <- as.numeric(mean_per_cond$day)

ggplot(mean_per_cond, aes(x = day, y = moy_perc, color = Condition)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = moy_perc - se_perc, ymax = moy_perc + se_perc), width = 0.2) +
  scale_color_manual(values = Col_gg) +
  theme_light() +
  scale_x_continuous(breaks = c(3, 5, 7, 10, 12)) +
  ggtitle("Kinetics of Average Brown Root Surface") +
  xlab("dpi") +
  ylab("Percentage of Brown Surface Area") +
  ylim(0, 80)+
  theme_classic(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 12))

#All accessions only RB84

Pourcentage_no_A <- Pourcentage %>% filter(!Rep %in% c("a"))
Inoc_no_A <- Pourcentage_no_A[Pourcentage_no_A$Condition == "RB84",]

attach(Pourcentage_no_A)
pairwise.wilcox.test(Poids,Geno_Cond,p.adjust.method="bonferroni")
attach(Pourcentage_no_A)
pairwise.wilcox.test(AUC_Brun, Genotype, p.adjust.method="bonferroni")

### Modele lineaire
fit_3 = lm(Pourcentage_3dpi ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_3)
fit_5 = lm(Pourcentage_5dpi ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_5)
fit_7 = lm(Pourcentage_7dpi ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_7)
fit_10 = lm(Pourcentage_10dpi ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_10)
fit_12 = lm(Pourcentage_12dpi ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_12)
fit_AUC = lm(AUC_Brun ~ Genotype + Rep,  data = Inoc_no_A, contrasts = list(Genotype = contr.treatment(5, 4)))  # comparison with "M"
summary(fit_AUC)


Pourcentage_no_A$Genotype <- factor(Pourcentage_no_A$Genotype, levels = c('M', '21', 'W', 'H', 'G'))
Inoc_no_A$Genotype <- factor(Inoc_no_A$Genotype, levels = c('M', '21', 'W', 'H', 'G'))

ggplot(Pourcentage_no_A, aes(x = Condition, y = Poids, fill = Condition), alpha=0.5) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.7, lwd = 0.7) +
  geom_jitter(aes(color = Condition, shape = Rep), size = 2, width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = Col_gg) +
  scale_color_manual(values = Col_gg) +
  facet_grid(~ Genotype, labeller = labeller(Genotype = as_labeller(c(
    'M' = "M", 
    '21' = "21", 
    'W' = "W", 
    'H' = "H", 
    'G' = "G"
  ), default = label_parsed))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  ylim(0.02, 0.15) +
  ggtitle("") +
  xlab("Condition") +
  ylab("Fresh weight (g)") +
  scale_x_discrete(limits = c("ni", "RB84")) +
  theme_classic(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 12))



ggplot(Inoc_no_A, aes(x = Genotype, y = AUC_Brun, fill = Genotype, alpha=0.5)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.7, lwd = 0.7) +
  geom_jitter(aes(color = Genotype, shape = Rep), size = 2, width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = Col) +
  scale_color_manual(values = Col) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  ylim(0, 750) +
  ggtitle("AUC of percentage of brown surface area") +
  xlab("Genotype") +
  ylab("AUC") +
  scale_x_discrete(limits = c('M', '21','W','H','G'))+
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

#Mutants 
Pourcentage=as.data.frame(fread("Pheno_Mutants.csv",na.strings=c("",NA,"NULL"),dec="."))
Pourcentage$Poids <- as.numeric(Pourcentage$Poids)
Inoc <- Pourcentage[Pourcentage$Condition == "RB84",]


attach(Pourcentage)
pairwise.wilcox.test(Poids,Geno_Cond,p.adjust.method="bonferroni")
fit_3 = lm(Pourcentage_3dpi ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_3)
fit_5 = lm(Pourcentage_5dpi ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_5)
fit_7 = lm(Pourcentage_7dpi ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_7)
fit_10 = lm(Pourcentage_10dpi ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_10)
fit_12 = lm(Pourcentage_12dpi ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_12)
fit_AUC = lm(AUC_Brun ~ Genotype + Rep,  data = Inoc, contrasts = list(Genotype = contr.treatment(6, 3)))  # comparison with "M"
summary(fit_AUC)


Pourcentage$Genotype <- factor(Pourcentage$Genotype, levels = c('M', 'E26', 'J42', 'V21', 'X30', 'T26'))
Inoc$Genotype <- factor(Inoc$Genotype, levels = c('M', 'E26', 'J42', 'V21', 'X30', 'T26'))
Col=c("#FF3000", "#FFCC00","#E69F00","#0072B2","#56B4E9","#00CC66")
Col_gg=c("#619CFF","#F8766D")

ggplot(Pourcentage, aes(x = Condition, y = Poids, fill = Condition, alpha = 0.5)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.7, lwd = 0.7) +
  geom_jitter(aes(color = Condition, shape = Rep), size = 2, width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = Col_gg) +
  scale_color_manual(values = Col_gg) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  facet_grid(~ Genotype, labeller = labeller(Genotype = as_labeller(c(
    'M' = "WT", 
    'E26' = "italic('crk-1')", 
    'J42' = "italic('crk-2')", 
    'V21' = "italic('rlck2-1')", 
    'X30' = "italic('rlck2-2')", 
    'T26' = "italic('ccamk')"
  ), default = label_parsed))) +
  ylim(0.03, 0.2) +
  ggtitle("") +
  xlab("Condition") +
  ylab("Fresh weight (g)") +
  scale_x_discrete(limits = c("ni", "RB84")) +
  theme_classic(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    strip.placement = "outside", 
    strip.background = element_blank(), 
    panel.spacing = unit(1, "lines")
  )
ggplot(Inoc, aes(x = Genotype, y = AUC_Brun, fill = Genotype, alpha=0.5)) +
  geom_boxplot(outlier.alpha = 0, alpha = 0.7, lwd = 0.7) +
  geom_jitter(aes(color = Genotype, shape = Rep), size = 2, width = 0.2, alpha = 0.8) +
  scale_fill_manual(values = Col) +
  scale_color_manual(values = Col) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "black") +
  ylim(0, 700) +
  ggtitle("") +
  xlab("Genotype") +
  ylab("AUC of brown surface area") +
  scale_x_discrete(
    limits = c('M', 'E26', 'J42', 'V21', 'X30', 'T26'), 
    labels = c(
      'M' = "WT", 
      'E26' = expression(italic("crk-1")), 
      'J42' = expression(italic("crk-2")), 
      'V21' = expression(italic("rlck2-1")), 
      'X30' = expression(italic("rlck2-2")), 
      'T26' = expression(italic("ccamk"))
    )
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

