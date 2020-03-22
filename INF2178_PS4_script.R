library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(broom)
library(skimr)
library(huxtable)

#import the dataset
getwd()
newspapers <- read_dta("input/116438-V1/data/dta/Angelucci_Cage_AEJMicro_dataset.dta")
skim(newspapers)

#Select columns needed, remove NAs
cleaned_newspapers <- newspapers %>% 
  dplyr::select(year, id_news, after_national, local, national, 
                ra_cst, ra_s) %>% 
  mutate_at(vars(id_news, after_national, local, national), ~as.factor(.)) %>% 
  mutate(year = as.integer(year)) %>%
  filter(!is.na(ra_s)) 

summary(cleaned_newspapers)

#Graph 1
cleaned_newspapers %>% 
  mutate(type = if_else(local == 1, "Local", "National")) %>% 
  ggplot(aes(x = year, y = ra_s)) +
  geom_point(alpha = 0.5) +
  scale_y_continuous(labels = percent_format(prefix="", suffix = "%", scale = 1)) +
  labs(x = "Year",
       y = "Percent Share of Advertising in Total Revenue") +
  facet_wrap(vars(type),
             nrow = 2) +
  theme_classic() +
  geom_vline(xintercept = 1966.5, linetype = "dashed")

ggsave("output/graphs/loc_nat_percent_adrev.png")

#Graph 2
indiv_nat_percent_adrev<-cleaned_newspapers %>%
  filter(national==1) %>%
  ggplot (aes(x = year, y= ra_s)) +
  geom_line () +
  facet_wrap(id_news~.) +
  scale_y_continuous(labels = percent_format(prefix="", suffix = "%", scale = 1)) +
  geom_vline(xintercept = 1966.5, linetype = "dashed") +
  labs(x = "Year", y = "Percent Share of Advertising in Total Revenue") 

ggsave("output/graphs/individ_percent_nat_adrev.png")

#Graph 3
national_newspapers <- cleaned_newspapers %>%
  filter(national==1)
  
local_newspapers <- cleaned_newspapers %>%
  filter(national==0)
 
national_ad_share <- national_newspapers %>%
  group_by(year) %>%
  summarise(min = min(ra_s),
            mean = mean(ra_s),
            max = max(ra_s))
national_ad_share

local_ad_share <-local_newspapers %>%
  filter(!is.na(ra_s)) %>%
  group_by(year) %>%
  summarise(min = min(ra_s),
            mean = mean(ra_s),
            max = max(ra_s))
local_ad_share

ggplot()+
  geom_line(data = national_ad_share, aes(x = year, y = mean), color = "blue")+
  geom_line(data = local_ad_share, aes(x = year, y = mean), color = "red")+
  xlab('Year')+
  ylab('Share of Advertising Revenue')+
  ggtitle('Comparing the share of advertising revenue')+
  geom_vline(xintercept = 1966.5, linetype = "dashed")

ggsave("output/graphs/comparing the share of advertising revenue.png")

#Graph 4
diff <- cleaned_newspapers %>%
  select(year, id_news, national, ra_s)%>%
  mutate(before_or_after = if_else (year < 1967, "Before 1967", "After 1967")) %>%
  group_by(id_news, before_or_after, national) %>%
  summarise(mean = mean(ra_s))

diff$before_or_after <- factor (diff$before_or_after, levels = c("Before 1967", "After 1967"))
levels(diff$national) <- c("Local Newspapers","National Newspapers")
diff

diff_plot <- diff %>%
  ggplot(aes(x = before_or_after, y = mean, colour = national)) +
  geom_point() +
  geom_line(aes(group = id_news), alpha = 0.5) +
  labs(x = "Before or After 1967", y = "Share of Advertising Revenue") +
  facet_grid(.~national) +
  theme_classic()+
  scale_color_brewer(palette = "Set1")

diff_plot

#Table 3
diff_table <- cleaned_newspapers %>%
  select(year, id_news, national, ra_s)%>%
  mutate(before_or_after = if_else (year < 1967, "Before 1967", "After 1967")) %>%
  group_by(before_or_after, national) %>%
  summarise(mean = mean(ra_s))
diff_table

### the model

model_dataset <- cleaned_newspapers %>%
  mutate_at(vars(id_news, after_national, local, national), ~as.factor(.)) %>% 
  mutate(year = as.integer(year))

ad_share_of_revenue <- lm(log(ra_s) ~ after_national + id_news + year, data = model_dataset)
summary(ad_share_of_revenue)
plot(ad_share_of_revenue)

omit_me <- c("(Intercept)", "id_news3", "id_news6", "id_news7", "id_news13", 
             "id_news16", "id_news25", "id_news28", "id_news34", "id_news38", 
             "id_news44", "id_news48", "id_news51", "id_news53", "id_news54", 
             "id_news57", "id_news60", "id_news62", "id_news66", "id_news67", 
             "id_news70", "id_news71", "id_news72", "id_news80", "id_news82", 
             "id_news88", "id_news95", "id_news97", "id_news98", "id_news103", 
             "id_news105", "id_news106", "id_news118", "id_news119", "id_news127", 
             "id_news136", "id_news138", "id_news148", "id_news151", "id_news153", 
             "id_news154", "id_news157", "id_news158", "id_news161", "id_news163", 
             "id_news167", "id_news169", "id_news179", "id_news184", "id_news185", 
             "id_news187", "id_news196", "id_news206", "id_news210", "id_news212", 
             "id_news213", "id_news224", "id_news225", "id_news234", "id_news236", 
             "id_news245", "id_news247", "id_news310", "id_news452", "id_news467", 
             "id_news469", "id_news480", "id_news20040", "id_news20345", 
             "id_news20346", "id_news20347", "id_news20352", "id_news20354", 
             "id_news21006", "id_news21025", "id_news21173", "id_news21176", 
             "id_news33718", "id_news34689", "id_news73")
huxreg(ad_share_of_revenue,
       omit_coefs = omit_me, 
       number_format = 2)

citation()
citation("tidyverse")
citation("haven")
citation("dplyr")
citation("ggplot2")
citation("scales")
citation("broom")
citation("skimr")
citation("huxtable")
