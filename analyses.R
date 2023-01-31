library(tidyverse)
library(lubridate)
library(knitr)
library(lmerTest)
library(brms)

setwd('/home/bms2202/newmetoo')

####### clean ########
t_raw <- read_csv('data/PrePost_TWEETS_indiv.csv')

t <- t_raw %>%
  rename(timepoint = "tweet_period") %>% 
  filter(datediff > 0) %>% 
  mutate(outrage.d = ifelse(outrage_score >= .7,1,0),
         datediff = datediff - 1,
         weekdiff = weekdiff - 1,
         positivity_st = recode(positivity_st,`2` = 1)) 
  
t_concat_raw = read_csv('data/ALL_TWEETS.csv')
t_concat <- t_concat_raw %>% 
  rename(timepoint = "tweet_period") %>% 
  filter(datediff > 0) %>% 
  mutate(timepoint.d = ifelse(timepoint == "pre",0,1),
         datediff = datediff - 1,
         weekdiff = weekdiff - 1,
         datetime = as_datetime(date),
         overall_date_diff = as.double(difftime(datetime,ymd("2017-10-05"),
                                                units = "days")),
         overall_week_diff = overall_date_diff/7) %>% 
  select(-c(tweet))

# calculate DOC stuff - don't use
t_doc <- t %>% 
  group_by(PublicFigure,timepoint,datediff) %>% 
  summarize(outrage_pct = (sum(outrage.d == 1)/n())*100,
            outrage_mean = mean(outrage_score))

t_concat <- t_concat %>% 
  left_join(t_doc)

# calculate pre stuff
t_pres <- t_concat %>% 
  filter(timepoint == "pre") %>% 
  group_by(PublicFigure) %>% 
  summarize(care.vice_pre = mean(care.vice),
            outrage_pct_pre = mean(outrage_pct),
            outrage_mean_pre = mean(outrage_mean),
            care.vice_pre.c = scale(care.vice_pre, scale = F),
            outrage_pct_pre.c = scale(outrage_pct_pre, scale = F),
            outrage_mean_pre.c = scale(outrage_mean_pre, scale = F))

t_concat <- t_concat %>% 
  left_join(t_pres)

### bring together motivating factors

doc_base <- t

## liking
# st
liking <- t %>% 
  filter(timepoint == "pre") %>% 
  group_by(PublicFigure) %>% 
  summarize(liking_st = mean(positivity_st))

# afinn
liking2_raw <- read_csv('data/afinn_scores.csv')
liking2 <- liking2_raw %>% 
  filter(tweet_period == "pre") %>% 
  group_by(PublicFigure) %>% 
  summarize(liking_afinn = mean(liking_afinn))

## severity
sevp <- read_csv("data/event_severity.csv")
sevp <- sevp %>% 
  select(c(PublicFigure, mean)) %>% 
  rename("severity_rating" = mean)

sevr <- read_csv("data/severity_rubric_scores.csv")
sevr <- sevr %>% 
  select(c(PublicFigure,Total)) %>% 
  rename("severity_rubric" = Total)

## familiarity
# prominence
powprom_raw <- read_csv("data/pow_prom_stats.csv")
powprom <- powprom_raw %>% 
  select(c(PublicFigure,pre_prom_mean,pre_pow_mean,
           post_prom_mean,post_pow_mean)) %>% 
  rename(prominence = "pre_prom_mean",
         power = "pre_pow_mean",
         post_prominence = "post_prom_mean",
         post_power = "post_pow_mean")

# count
count <- t %>% 
  filter(timepoint == "pre") %>% 
  group_by(PublicFigure) %>% 
  summarize(tweet_count = n())

# news
news <- read_csv("data/ALL_NEWS.csv")
news <- news %>% 
  filter(timepoint == "pre") %>% 
  group_by(publicfigure) %>% 
  summarize(articles = n()) %>% 
  rename("PublicFigure" = publicfigure)

motivators <- count %>% 
  left_join(news) %>% 
  left_join(powprom) %>% 
  left_join(sevr) %>% 
  left_join(sevp) %>% 
  left_join(liking) %>% 
  left_join(liking2)

t_concat <- t_concat %>% 
  left_join(motivators) %>% 
  mutate(tweet_count.c = scale(log10(tweet_count), center = T, scale = F),
         articles.c = scale(articles, center = T, scale = F),
         prominence.c = scale(prominence, center = T, scale = F),
         power.c = scale(power, center = T, scale = F),
         severity_rubric.c = scale(severity_rubric, center = T, scale = F),
         severity_rating.c = scale(severity_rating, center = T, scale = F),
         liking_st.c = scale(liking_st, center = T, scale = F),
         liking_afinn.c = scale(liking_afinn, scale = F),
         care.virtue.c = scale(care.virtue, scale = F),
         care.vice.c = scale(care.vice, scale = F),
         tweet_count.cs = scale(log10(tweet_count)),
         articles.cs = scale(articles),
         prominence.cs = scale(prominence),
         power.cs = scale(power),
         severity_rubric.cs = scale(severity_rubric),
         severity_rating.cs = scale(severity_rating),
         liking_st.cs = scale(liking_st),
         liking_afinn.cs = scale(liking_afinn))

t_concat$familiarity <- rowMeans(subset(t_concat, select = c(prominence.cs,
                                                             power.cs,
                                   tweet_count.cs,
                                   articles.cs)),
                                 na.rm = T)

t_concat$liking <- rowMeans(subset(t_concat, select = c(liking_st.cs,
                                                             liking_afinn.cs)),
                            na.rm = T)

t_concat$severity <- rowMeans(subset(t_concat, select = c(severity_rubric.cs,
                                                        severity_rating.cs)),
                              na.rm = T)

tweets <- t_concat

######## MODELS ######

## model 1
# effect of time period
m1 <- brm(care.vice ~ timepoint.d +
            (1 + timepoint.d | PublicFigure),
          data = tweets %>% 
            filter(timepoint != 'oneyear'), 
          cores = 8, chains = 2, iter = 6000,
          control = list(adapt_delta = .99,max_treedepth = 15))


## model 2
# effect of MFs on post
m2 <- brm(care.vice ~ familiarity*liking*severity + care.vice_pre +
            (1 + familiarity*liking*severity | PublicFigure), 
          data = tweets %>% 
            filter(timepoint == 'post'), 
          cores = 8, chains = 2, iter = 4000,
          control = list(adapt_delta = .99,max_treedepth = 15))


## model 3
# effect of day
m3 <- brm(care.vice ~ datediff +
            (1 + datediff | PublicFigure),
          data = tweets %>% 
            filter(timepoint == 'post'),
          cores = 8, chains = 2, iter = 4000,
          control = list(adapt_delta = .99,max_treedepth = 15)) 


# effect of logarithm day slightly different calc
m3a <- brm(log(care.vice*10) ~ datediff +
             (1 + datediff | PublicFigure),
           data = tweets %>% 
             filter(timepoint == 'post') %>% 
             mutate(care.vice = recode(care.vice, `0` = 1)),
           cores = 8, chains = 2, iter = 4000,
           control = list(adapt_delta = .99,max_treedepth = 15)) 

## model 4
# interaction between day and MFs
m4 <- brm(care.vice ~ datediff*familiarity*liking*severity +
            (1 + datediff | PublicFigure), 
          data = tweets %>% 
            filter(timepoint == 'post'), 
          cores = 8, chains = 2, iter = 4000,
          control = list(adapt_delta = .99,max_treedepth = 15))

# interaction between day and MFs, first week only
m4a <- brm(care.vice ~ datediff*familiarity*liking*severity +
             (1 + datediff | PublicFigure), 
           data = tweets %>% 
             filter(timepoint == 'post',
                    weekdiff == 0), 
           cores = 8, chains = 2, iter = 4000,
           control = list(adapt_delta = .99,max_treedepth = 15))


## model 5
# difference between morality in pre tweets and one year later
m5 <- brm(care.vice ~ timepoint.d +
            (1 + timepoint.d | PublicFigure),
          data = tweets %>% 
            filter(timepoint != 'post') %>% 
            mutate(timepoint.d = recode(timepoint.d,`2` = 1)),
          cores = 8, chains = 2, iter = 4000,
          control = list(adapt_delta = .99,max_treedepth = 15))
# still significantly higher!

# difference between morality in post tweets tweets and one year later
m5a <- brm(care.vice ~ timepoint.d +
             (1 + timepoint.d | PublicFigure),
           data = tweets %>% 
             filter(timepoint != 'pre') %>% 
             mutate(timepoint.d = recode(timepoint.d,`2` = 1,
                                         `1` = 0)),
           cores = 8, chains = 2, iter = 4000,
           control = list(adapt_delta = .99,max_treedepth = 15))

# effect of MFs on oneyear
m5b <- brm(care.vice ~ familiarity*liking*severity + 
             care.vice_pre +
             (1 + familiarity*liking*severity | PublicFigure), 
           data = tweets %>% 
             filter(timepoint == 'oneyear'), 
           cores = 8, chains = 2, iter = 4000,
           control = list(adapt_delta = .99,max_treedepth = 15))


######## FIGURES #######
## figure 1 
ggplot(tweets %>% 
         group_by(PublicFigure, timepoint) %>% 
         summarize(care.vice = mean(care.vice)), 
       aes(x = factor(timepoint, 
                      level = c("pre","post","oneyear")), 
           y = care.vice)) +
  geom_violin(fill = "darkcyan", alpha = .7) +
  theme_classic()  +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .3) +
  labs(y = '% Immoral Expressions', 
       title = 'Immoral Expressions by time period') +
  scale_x_discrete(labels = c("6 months\nprior to\nallegations",
                              "Initial 3\nweeks after\allegations",
                              "1 year\nafter\nallegations")) +
  theme(plot.title = element_text(hjust = .5, size = 18),
        axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_blank())
ggsave("figs/fig1.jpg")

## figure 2a
x = c("Liking","Familiarity","Severity",
      "Liking x\nFamiliarity","Liking x\nSeverity","Familiarity x\nSeverity")
measure = c(rep("Main effect",3),rep("Interaction",3))
fixef_11f = fixef(m2)

effect = c(fixef_11f[3,1],fixef_11f[2,1],fixef_11f[4,1],
           fixef_11f[6,1],fixef_11f[8,1],fixef_11f[7,1])

lower = c(fixef_11f[3,3],fixef_11f[2,3],fixef_11f[4,3],
          fixef_11f[6,3],fixef_11f[8,3],fixef_11f[7,3])

upper = c(fixef_11f[3,4],fixef_11f[2,4],fixef_11f[4,4],
          fixef_11f[6,4],fixef_11f[8,4],fixef_11f[7,4])

ci80 = posterior_samples(m2)

# 80% CIs
lower80 = c(quantile(ci80[,3],.1),
            quantile(ci80[,2],.1),
            quantile(ci80[,4],.1),
            quantile(ci80[,6],.1),
            quantile(ci80[,8],.1),
            quantile(ci80[,7],.1))

# 80% CIs
upper80 = c(quantile(ci80[,3],.9),
            quantile(ci80[,2],.9),
            quantile(ci80[,4],.9),
            quantile(ci80[,6],.9),
            quantile(ci80[,8],.9),
            quantile(ci80[,7],.9))

pdf4 <- data_frame(x = x, measure = measure,
                   effect = effect, lower = lower,
                   upper = upper,
                   lower80 = lower80, upper80 = upper80)
pdf4$x <- factor(pdf4$x, levels = rev(x))

p2<- ggplot(pdf4, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkcyan') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'darkcyan') +  
  geom_point(size = 3, color = 'darkcyan') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",title = "Effect Sizes for Motivating Factors\nInitial Three Weeks") +
  scale_x_continuous(n.breaks = 10) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))
ggsave("figs/fig2a.jpg")

## figure 2b
sevplot <- tweets %>% 
  filter(timepoint == "post") %>% 
  mutate(sev.d = ifelse(severity < median(severity, na.rm = T),0,1)) %>% 
  group_by(PublicFigure) %>% 
  summarize(liking = mean(liking),
            care.vice = mean(care.vice),
            sev.d = as.factor(mean(sev.d, na.rm = T))) %>% 
  filter(sev.d != 'NaN')

ggplot(sevplot, aes(x = liking, y = care.vice,
                    group = sev.d, color = sev.d)) +
  geom_smooth(method = 'lm', aes(fill = sev.d), alpha = .2, fullrange = T) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2",labels = c("Low","High"), ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-1.5,2,.5)) +
  guides(fill = F) +
  labs(x = "Liking", y = "% Immoral Expressions",
       title = "Severity x Liking interaction\nInitial Three Weeks",
       color = "Severity Level") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('figs/fig2b.jpg')

## figure 3
p3 <- ggplot(data = tweets %>% 
               filter(timepoint != "oneyear") %>% 
               mutate(datediff = ifelse(timepoint == 'pre',datediff - 21,datediff)) %>% 
               group_by(datediff,timepoint) %>% 
               summarize(mean_moral = mean(care.vice),
                         se_moral = sd(care.vice)/sqrt(20)),
             aes(x = datediff, y = mean_moral, group = timepoint, color = timepoint)) + 
  geom_line() + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_moral - se_moral,
                    ymax = mean_moral + se_moral)) +
  geom_vline(xintercept  = 0, color = 'darkgray', linetype = 'dashed') +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = c(-11,11),
                     labels = c("Baseline (6 months prior)\nby day",
                                "Initial three weeks\nby day")) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black")) +
  labs(
    y = '% Immoral Expressions',color = "Time period") +
  ggtitle('Immoral Expressions by day') +
  theme(plot.title = element_text(hjust = .5, size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        legend.position = "none")
ggsave('figs/fig3.jpg')

## figure 4a

x = c("Liking","Familiarity","Severity",
      "Liking x\nFamiliarity","Liking x\nSeverity","Familiarity x\nSeverity")
measure = c(rep("Main effect",3),rep("Interaction",3))
fixef_11f = fixef(m5b)

effect = c(fixef_11f[3,1],fixef_11f[2,1],fixef_11f[4,1],
           fixef_11f[6,1],fixef_11f[8,1],fixef_11f[7,1])

lower = c(fixef_11f[3,3],fixef_11f[2,3],fixef_11f[4,3],
          fixef_11f[6,3],fixef_11f[8,3],fixef_11f[7,3])

upper = c(fixef_11f[3,4],fixef_11f[2,4],fixef_11f[4,4],
          fixef_11f[6,4],fixef_11f[8,4],fixef_11f[7,4])

ci80 = posterior_samples(m5b)

# 80% CIs
lower80 = c(quantile(ci80[,3],.1),
            quantile(ci80[,2],.1),
            quantile(ci80[,4],.1),
            quantile(ci80[,6],.1),
            quantile(ci80[,8],.1),
            quantile(ci80[,7],.1))

# 80% CIs
upper80 = c(quantile(ci80[,3],.9),
            quantile(ci80[,2],.9),
            quantile(ci80[,4],.9),
            quantile(ci80[,6],.9),
            quantile(ci80[,8],.9),
            quantile(ci80[,7],.9))

pdf4 <- data_frame(x = x, measure = measure,
                   effect = effect, lower = lower,
                   upper = upper,
                   lower80 = lower80, upper80 = upper80)
pdf4$x <- factor(pdf4$x, levels = rev(x))

p4<- ggplot(pdf4, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkcyan') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'darkcyan') +  
  geom_point(size = 3, color = 'darkcyan') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effect Sizes for Motivating Factors\nOne year later") +
  scale_x_continuous(n.breaks = 10) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))
ggsave("figs/fig4a.jpg")

## figure 4b
sevplot <- tweets %>% 
  filter(timepoint == "oneyear") %>% 
  mutate(sev.d = ifelse(severity < median(severity, na.rm = T),0,1)) %>% 
  group_by(PublicFigure) %>% 
  summarize(liking = mean(liking),
            care.vice = mean(care.vice),
            sev.d = as.factor(mean(sev.d, na.rm = T))) %>% 
  filter(sev.d != 'NaN')

ggplot(sevplot, aes(x = liking, y = care.vice,
                    group = sev.d, color = sev.d)) +
  geom_smooth(method = 'lm', aes(fill = sev.d), alpha = .2, fullrange = T) +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2",labels = c("Low","High"), ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(-1.5,2,.5)) +
  guides(fill = F) +
  labs(x = "Liking", y = "% Immoral Expressions",
       title = "Severity x Liking interaction\nOne Year Later",
       color = "Severity Level") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
ggsave('figs/fig4b.jpg')
