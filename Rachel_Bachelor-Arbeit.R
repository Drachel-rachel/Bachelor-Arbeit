rm(list=ls())
gc()
options(stringsAsFactors = F) ##False

library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(survival)
library(KMsurv)
library(multcomp)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##    read data and prepare data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- read_excel("/Users/racheldraude/Desktop/Bachelor Arbeit /Daten/including_day.xlsx", 
                  na = c("N.A.", ""))

## make Summer summer, so that we don't have two different summer groups (capatalized changes otherwise the group)
dat[dat == "Summer"] <- "summer"

## Make the plots the according climate, don't need to subset anymore
dat$Climate <- ifelse(dat$Plot_ID %in% c("1_1", "3_4", "5_5", "8_3", "10_2"), "ambient", "future")

## kick everything where we don't know which species it is (also deletes the missing seasons, only delets 3 rows)
dat <- subset(dat, !is.na(Species))
dat <- subset(dat, !is.na(Seeds._total...viable))

## calculate the germination rate and the seed per single seed
dat$germination_rate <- as.numeric(dat$`Germinated`)/as.numeric(dat$Seeds._total...viable)
dat$single_seed <- as.numeric(dat$Weight..g.)/as.numeric(dat$Seeds._total...viable)
dat$Seeds._total...viable <- as.numeric(dat$Seeds._total...viable)
dat$fail <- dat$Seeds._total...viable - dat$Germinated

## subset data into Dianthus and Scabiosa
dat_dia <- subset(dat, Species == "Dia_car")
dat_sca <- subset(dat, Species == "Sca_och")

dat_dia <- subset(dat_dia, Weight..g.  != 0.00031)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      Visualizing data
##      Control for normal distribution example
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 1: tests you can use for normal distribution, example area!!!
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      1.1: Visualize with histogram
## Histogram to see the weight distribution of a single weight
## What a normal distribution should look like: 

set.seed(1) ## sets a seed that your result is always the same (rnorm is a random function)
normal_example <- rnorm(1000)
example <- ggplot(mapping = aes(x = normal_example)) + geom_histogram(fill = "grey", color = "black") + 
  theme_classic() + ggtitle("Example of an normal distribution")
example

## What our data looks like:
weight <- ggplot(dat, aes(x = as.numeric(Weight..g.))) + geom_histogram(fill = "grey", color = "black") + 
  theme_classic() + ggtitle("Seed weight")
weight

weight_ind <- ggplot(dat, aes(x = single_seed)) + 
  geom_histogram(fill = "grey", color = "black") + theme_classic() + ggtitle("Weight of a single seed")
weight_ind

germ_rate <- ggplot(dat, aes(x = germination_rate)) + geom_histogram(fill = "grey", color = "black") + 
  theme_classic() + ggtitle("Germination rate")

germ_rate

ggpubr::ggarrange(example, weight, weight_ind, germ_rate,ncol = 2, nrow = 2)

##      1.2 Calculating, using the shapiro test
shapiro.test(normal_example) ## the p value for the test is 0.7256 meaning 
                             ## the data is not significant different from a normal distribution
shapiro.test(as.numeric(dat$Weight..g.)) ## p value is significant, so no normal distribution
shapiro.test(dat$single_seed) ## p value is significant, so no normal distribution
shapiro.test(dat$germination_rate) ## p value is significant, so no normal distirbution

##      1.3 Visualizing qqplot

qqnorm(normal_example)
qqline(normal_example, col = "red")

qqnorm(as.numeric(dat$Weight..g.), main=" Q-Q Plot (Seed weight)")
qqline(as.numeric(dat$Weight..g.), col = "red") 

qqnorm((dat$single_seed), main=" Q-Q Plot (Weight of a single seed)")
qqline(dat$single_seed, col = "red")

qqnorm((dat$germination_rate), main=" Q-Q Plot (Germination rate)")
qqline(dat$germination_rate, col = "red")

## Note we checked the data just now but what really should be normal distributed are your residuals! Can do that later
## once we have our models
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## calculate mean over treatments to create barplots + use custome colors that future will be red and ambient blue
## standard error function
standard_error <- function(x) sd(x, na.rm = T) / sqrt(length(x))

## colors we want to use
rbPalette <- c("#0072B2", "#D55E00")

dia_mean_weight <- aggregate(as.numeric(dat_dia$Weight..g...Seed), 
                             by = list(dat_dia$Season, dat_dia$Climate, dat_dia$Pollination), 
                             FUN = mean, na.rm = T)

colnames(dia_mean_weight) <- c("season", "climate", "pollination", "mean_weight")

dia_mean_weight$se <- aggregate(as.numeric(dat_dia$Weight..g...Seed), 
                                by = list(dat_dia$Season, dat_dia$Climate, dat_dia$Pollination), 
                                FUN = standard_error)$x

dia_mean_weight$season <- factor(dia_mean_weight$season, levels = c("summer", "fall"))

dia_bar <- ggplot(dia_mean_weight, aes(x = pollination, y = mean_weight, fill = climate)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = mean_weight - se, ymax = mean_weight + se), 
                position = position_dodge(width = 0.8), width = 0.5) + 
  ylab("Mean single seed weight") + xlab("") + scale_fill_manual(values = rbPalette) + 
  guides(fill = guide_legend(title = "Climate")) + theme_bw() + ggtitle("Dianthus carthusianorum") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  
sca_mean_weight <- aggregate(as.numeric(dat_sca$Weight..g...Seed), 
                             by = list(dat_sca$Season, dat_sca$Climate, dat_sca$Pollination),
                             FUN = mean, na.rm = T)

colnames(sca_mean_weight) <- c("season", "climate", "pollination", "mean_weight")

sca_mean_weight$se <- aggregate(dat_sca$Weight..g...Seed, 
                                by = list(dat_sca$Season, dat_sca$Climate, dat_sca$Pollination), 
                                FUN = standard_error)$x

sca_mean_weight$season <- factor(sca_mean_weight$season, levels = c("summer", "fall"))

sca_bar <- ggplot(sca_mean_weight, aes(x = pollination, y = mean_weight, fill = climate)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = mean_weight - se, ymax = mean_weight + se), 
                position = position_dodge(width = 0.8), width = 0.5) + 
  ylab("Mean single seed weight") + xlab("") + scale_fill_manual(values = rbPalette) + 
  guides(fill = guide_legend(title = "Climate")) + theme_bw() + ggtitle("Scabiosa ochroleuca")

ggpubr::ggarrange(dia_bar, sca_bar, nrow = 2, common.legend = T)
##facet_wrap gibt 2 Plots aus

##Plot geändert
dia_mean_weight <- aggregate(as.numeric(dat_dia$Weight..g...Seed), 
                             by = list(dat_dia$Season, dat_dia$Climate, dat_dia$Pollination), 
                             FUN = mean, na.rm = T)

colnames(dia_mean_weight) <- c("season", "climate", "pollination", "mean_weight")

dia_mean_weight$se <- aggregate(as.numeric(dat_dia$Weight..g...Seed), 
                                by = list(dat_dia$Season, dat_dia$Climate, dat_dia$Pollination), 
                                FUN = standard_error)$x

dia_mean_weight$season <- factor(dia_mean_weight$season, levels = c("summer", "fall"))

dia_bar <- ggplot(dia_mean_weight, aes(x = climate, y = mean_weight, fill = pollination)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = mean_weight - se, ymax = mean_weight + se), 
                position = position_dodge(width = 0.8), width = 0.5) + 
  ylab("Mean single seed weight") + xlab("") + scale_fill_manual(values = rbPalette) + 
  guides(fill = guide_legend(title = "Pollination")) + theme_bw() + ggtitle("Dianthus carthusianorum") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


sca_mean_weight <- aggregate(as.numeric(dat_sca$Weight..g...Seed), 
                             by = list(dat_sca$Season, dat_sca$Climate, dat_sca$Pollination),
                             FUN = mean, na.rm = T)

colnames(sca_mean_weight) <- c("season", "climate", "pollination", "mean_weight")

sca_mean_weight$se <- aggregate(dat_sca$Weight..g...Seed, 
                                by = list(dat_sca$Season, dat_sca$Climate, dat_sca$Pollination), 
                                FUN = standard_error)$x

sca_mean_weight$season <- factor(sca_mean_weight$season, levels = c("summer", "fall"))

sca_bar <- ggplot(sca_mean_weight, aes(x = climate, y = mean_weight, fill = pollination)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ season) + 
  geom_errorbar(aes(ymin = mean_weight - se, ymax = mean_weight + se), 
                position = position_dodge(width = 0.8), width = 0.5) + 
  ylab("Mean single seed weight") + xlab("") + scale_fill_manual(values = rbPalette) + 
  guides(fill = guide_legend(title = "Pollination")) + theme_bw() + ggtitle("Scabiosa ochroleuca")

ggpubr::ggarrange(dia_bar, sca_bar, nrow = 2, common.legend = T)


### ANOVA#
mod_dia <- aov(log(single_seed) ~ Climate * Pollination * Season , data = dat_dia)
summary(mod_dia)

Anova(lm(log(single_seed) ~ Climate * Pollination * Season , data = dat_sca))
mod_sca <- aov(log(single_seed) ~ Climate * Pollination * Season , data = dat_sca)
summary(mod_sca)

## everything for scabiosa looks good!
shapiro.test(rstandard(mod_dia))
shapiro.test(rstandard(mod_sca))

qqnorm(rstandard(mod_dia), main=" Q-Q Plot (D. carthusianorum)")
qqline(rstandard(mod_dia), col = "red")

qqnorm(rstandard(mod_sca), main=" Q-Q Plot (S. ochroleuca)")
qqline(rstandard(mod_sca), col = "red")

hist(residuals(mod_dia), main=" Histogram of residuals (D. carthusianorum)")

hist(residuals(mod_sca), main=" Histogram of residuals (S. ochroleuca)")

##test auf Signifikanz
TukeyHSD(mod_sca, ordered = T)
TukeyHSD(mod_dia, ordered = T) ## not really needed
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Germination rate 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat$Season <- factor(dat$Season, levels = c("summer", "fall"))

mean_ger <- aggregate(dat$germination_rate, by = list(dat$Species, dat$Season, dat$Climate, dat$Pollination),
                      FUN = mean, na.rm = T)

colnames(mean_ger) <- c("species", "season", "climate", "pollination", "mean_ger")

mean_ger$se <- aggregate(dat$germination_rate, by = list(dat$Species, dat$Season, dat$Climate, dat$Pollination),
                         FUN = standard_error)$x

mean_ger$species <- ifelse(mean_ger$species == "Sca_och", "Scabiosa ochroleuca", "Dianthus carthusianorum")
mean_ger$season <- factor(mean_ger$season, levels = c("summer", "fall"))

dia_ger <- ggplot(subset(mean_ger, species == "Dianthus carthusianorum"), 
                  aes(x = climate, y = mean_ger, fill = pollination)) + 
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(aes(ymin = mean_ger - se, ymax = mean_ger + se), 
                          width = .5, position = position_dodge(width = .8)) + facet_wrap(~ season) + 
            scale_fill_manual(values = rbPalette) + ggtitle("Dianthus carthusianorum") + 
            ylab("Mean germination rate") + 
            xlab("") + theme_bw() + guides(fill = guide_legend(title = "Pollination")) + 
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

sca_ger <- ggplot(subset(mean_ger, species == "Scabiosa ochroleuca"), 
                  aes(x = climate, y = mean_ger, fill = pollination)) + 
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(aes(ymin = mean_ger - se, ymax = mean_ger + se), 
                          width = .5, position = position_dodge(width = .8)) + facet_wrap(~ season) + 
            scale_fill_manual(values = rbPalette) + ggtitle("Scabiosa ochroleuca") + 
            ylab("Mean germination rate") + 
            xlab("") + theme_bw() + guides(fill = guide_legend(title = "Pollination")) + 
            scale_y_continuous(limits = c(0,1))

ggpubr::ggarrange(dia_ger, sca_ger, nrow = 2, common.legend = T)

library(lme4)
dat_dia <- subset(dat_dia, !is.na(Seeds._total...viable))
dat_dia <- subset(dat_dia, germination_rate <= 1)
dat_dia$fail <- as.numeric(dat_dia$Seeds._total...viable) - dat_dia$Germinated

fit.glmm_dia <- glmer(cbind(Germinated, fail) ~ Pollination * Climate * Season + (1|Chamber_Block),
         family = binomial, data = dat_dia)

fit.glmm.intercept_dia <- glmer(cbind(Germinated, fail) ~ 1 + (1|Chamber_Block), 
                            family = binomial, data=dat_dia)
mod_germ_dia <- Anova(fit.glmm_dia)

dat_sca <- subset(dat_sca, !is.na(Seeds._total...viable))
dat_sca <- subset(dat_sca, germination_rate <= 1)

fit.glmm_sca <- glmer(cbind(Germinated, fail) ~ Pollination * Climate * Season + (1|Chamber_Block),
                  family = binomial, data = dat_sca)

fit.glmm.intercept_sca <- glmer(cbind(Germinated, fail) ~ 1 + (1|Chamber_Block), 
                            family = binomial, data=dat_sca)


mod_germ_sca <- Anova(fit.glmm_sca)

glht(mod_germ_dia, mcp(Pollination = "Tukey"))

mod_germ_dia
mod_germ_sca

##install.packages("lsmeans") ## need to install that package!
lsmeans::lsmeans(fit.glmm_dia, pairwise ~ Pollination * Climate * Season, adjust = "Tukey")
lsmeans::lsmeans(fit.glmm_sca, pairwise ~ Pollination * Climate * Season, adjust = "Tukey")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      Time till germination
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1. Set up your data table in a better way
rows <- nrow(dat_dia)
dia_ger <- data.frame(climate = dat_dia$Climate, pollination = dat_dia$Pollination, season = dat_dia$Season,
                      day = c(rep(4, rows), rep(6, rows), rep(8, rows), rep(10, rows), rep(12, rows), rep(14, rows), 
                              rep(16, rows), rep(18, rows), rep(20, rows), rep(22, rows), rep(24, rows), rep(26, rows)),
                      used_seeds = as.numeric(dat_dia$Seeds._total...viable),
                      germinated = c(dat_dia$day4, dat_dia$day6, dat_dia$day8, dat_dia$day10, dat_dia$day12, dat_dia$day14,
                                     dat_dia$day16, dat_dia$day18, dat_dia$day20, dat_dia$day22, dat_dia$day24, 
                                     dat_dia$day26))

rows <- nrow(dat_sca)
sca_ger <- data.frame(climate = dat_sca$Climate, pollination = dat_sca$Pollination, season = dat_sca$Season,
                      day = c(rep(4, rows), rep(6, rows), rep(8, rows), rep(10, rows), rep(12, rows), rep(14, rows), 
                              rep(16, rows), rep(18, rows), rep(20, rows), rep(22, rows), rep(24, rows), rep(26, rows)),
                      used_seeds = as.numeric(dat_sca$Seeds._total...viable),
                      germinated = c(dat_sca$day4, dat_sca$day6, dat_sca$day8, dat_sca$day10, dat_sca$day12, dat_sca$day14,
                                     dat_sca$day16, dat_sca$day18, dat_sca$day20, dat_sca$day22, dat_sca$day24, 
                                     dat_sca$day26))

dia_ger[order(dia_ger$pollination),] ## thats how you order data frames in r

dia_ger <- subset(dia_ger, !is.na(used_seeds))
sca_ger <- subset(sca_ger, !is.na(used_seeds))

## for loop to make it binary
temp_list <- c()

for(i in 1:nrow(dia_ger))
{
  temp_list[i] <- list(data.frame(climate = rep(dia_ger[i, "climate"], dia_ger[i, "germinated"]),
                                  pollination = rep(dia_ger[i, "pollination"], dia_ger[i, "germinated"]),
                                  season = rep(dia_ger[i, "season"], dia_ger[i, "germinated"]),
                                  day = rep(dia_ger[i, "day"], dia_ger[i, "germinated"]),
                                  used_seeds = rep(dia_ger[i,"used_seeds"], dia_ger[i, "germinated"]),
                                  germination = rep(1, dia_ger[i, "germinated"]))) 
  
  
  
  if(i == nrow(dia_ger))
  {
    dia_bi_ger <- do.call("rbind", temp_list)
    print(aggregate(dia_bi_ger$germination, 
                    by = list(dia_bi_ger$climate, dia_bi_ger$pollination, dia_bi_ger$season), FUN = sum))
    
    #print(aggregate(dia_bi_ger$used_seeds, 
                    #by = list(dia_bi_ger$climate, dia_bi_ger$pollination, dia_bi_ger$season), FUN = sum))
  }
}

## 
dia_used <- aggregate(as.numeric(dat_dia$Seeds._total...viable), 
                      by = list(dat_dia$Climate, dat_dia$Pollination, dat_dia$Season),
                      FUN = sum, na.rm = T)
dia_max_ger <- aggregate(dia_bi_ger$germination, 
                        by = list(dia_bi_ger$climate, dia_bi_ger$pollination, dia_bi_ger$season), 
                        FUN = sum)

colnames(dia_max_ger) <- c("climate", "pollination", "season","total_germinated") 

dia_max_ger$total_used <- dia_used$x
dia_max_ger$not_germinated <- dia_max_ger$total_used - dia_max_ger$total_germinated

temp_list <- c()
for(i in 1:nrow(dia_max_ger))
{
  temp_list[i] <- list(data.frame(climate = rep(dia_max_ger[i, "climate"], dia_max_ger[i, "not_germinated"]),
                                  pollination = rep(dia_max_ger[i, "pollination"], dia_max_ger[i, "not_germinated"]),
                                  season = rep(dia_max_ger[i, "season"], dia_max_ger[i, "not_germinated"]),
                                  day = rep(max(dia_bi_ger$day), dia_max_ger[i, "not_germinated"]),
                                  used_seeds = rep(dia_max_ger[i, "total_used"], dia_max_ger[i, "not_germinated"]),
                                  germination = rep(0, dia_max_ger[i, "not_germinated"])))
  print(i)
}

not_germinated <- do.call("rbind", temp_list)

dia_bi_ger <- rbind(not_germinated, dia_bi_ger)

dia_mod_surv <- survfit(Surv(day, germination) ~ climate + pollination + season, data=dia_bi_ger, type="kaplan-meier")
plot(dia_mod_surv, col = c("red", "blue", "black", "green", "yellow", "orange", "grey"))

survdiff(Surv(day, germination) ~ climate + pollination + season, data = dia_bi_ger)

temp_list <- c()
for(i in 1:nrow(sca_ger))
{
  temp_list[i] <- list(data.frame(climate = rep(sca_ger[i, "climate"], sca_ger[i, "germinated"]),
                                  pollination = rep(sca_ger[i, "pollination"], sca_ger[i, "germinated"]),
                                  season = rep(sca_ger[i, "season"], sca_ger[i, "germinated"]),
                                  day = rep(sca_ger[i, "day"], sca_ger[i, "germinated"]),
                                  germination = rep(1, sca_ger[i, "germinated"]))) 
}
sca_bi_ger <- do.call("rbind", temp_list)

sca_mod_surv <- survfit(Surv(day, germination) ~ climate + pollination + season, data=sca_bi_ger, type="kaplan-meier")
plot(sca_mod_surv, col = c("red", "blue", "black", "green", "yellow", "orange", "grey"), main = "test")


dat <- data.frame(surv = dia_mod_surv$surv, time = dia_mod_surv$time)

ggplot(dat, aes(y = surv, x = time)) + geom_line()

survdiff(Surv(day, germination) ~ climate + pollination + season, data = sca_bi_ger)

temp_list <- c()
for(i in 1:nrow(dat_dia))
{
  temp_list[i] <- list(data.frame(species = rep(dat[i, "Species"], dat[i, "Seeds._total...viable"]),
                                  pollination = rep(dat[i, "Pollination"], dat[i, "Seeds._total...viable"]),
                                  climate = rep(dat[i, "Climate"], dat[i, "Seeds._total...viable"]),
                                  season = rep(dat[i, "Season"], dat[i, "Seeds._total...viable"]),
                                  block = rep(dat[i, "Chamber_Block"], dat[i, "Seeds._total...viable"]),
                                  germination_rate = c(rep(1, dat[i, "Germinated"]), 
                                                       rep(0, as.numeric(dat[i, "Seeds._total...viable"]) - 
                                                             dat[i, "Germinated"]))))
}
dia_bi_ger <- do.call("rbind", temp_list)
## day till event model
dia_mod_surv <- survreg(Surv(day, germination) ~ climate + pollination + season, frailty.gaussian(block, df = 3, sparse = F),
                        data=dia_bi_ger)



## binomial
germ_rate_glm <- do.call("rbind", temp_list)

dat_dia$Germinated
dat <- as.data.frame(dat)

germ_dia <- subset(germ_rate_glm, species == "Dia_car")

summary(glm(germ_dia$germination_rate ~ germ_dia$pollination * germ_dia$climate * germ_dia$season + (1|germ_dia$block), 
            family = binomial))