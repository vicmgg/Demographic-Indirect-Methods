rm(list = ls())

library(tidyverse)
library(stringr)
library(ggpubr)
library(lubridate)
library(xlsx)
library(forecast)

base0 <- readxl::read_xlsx("HNV_2020.xlsx", sheet = "hnv_hf_long")

base1 <- base0 %>% 
  # filter(edo == "Total") %>% 
  select(edo, age, ceb = hnv, cd = hf, tot = Total) %>% 
  mutate(age2 = str_sub(age, 1, 2)) %>% 
  type_convert()

base2 <- rbind(base1 %>% 
                 filter(age2 < 50),
               base1 %>% 
                 filter(age2 >= 50) %>% 
                 group_by(edo, ceb, cd) %>% 
                 summarise(tot = sum(tot), .groups = "drop") %>% 
                 mutate(age = "50+", .before = ceb) %>% 
                 mutate(age2 = 50)) %>% 
  filter(!ceb %in% c("NE"), !cd %in% c("NE")) %>% 
  type_convert() %>% 
  filter(!age %in% c("12-14", "50+")) %>% 
  select(-age2)

# brass estimate infant mortality using parameters available in tools for demographic estimation ----


child_brass <- function(base, census.date, coef.family, mlt.family){
  
  load("brass_params.RData")
  
  Wx <- base %>% 
  group_by(age) %>% 
  summarise(pop = sum(tot))

child <- full_join(base %>% 
  mutate(ceb = ceb*tot, cd = cd*tot) %>% 
  group_by(age) %>% 
  summarise(ceb = sum(ceb), cd = sum(cd), .groups = "drop") %>% 
  mutate(pdx = cd/ceb),
  Wx, by = "age") %>% 
  mutate(mceb = ceb/pop, mcs = (ceb - cd)/pop) 

parity <- vector()
for (i in 1:6) {
  parity[i] <- child$mceb[i]/child$mceb[i + 1]
}
  
coef <- coef %>% 
  filter(family == coef.family)

mlt <- mlt %>% 
  filter(family == mlt.family)

cens.date <- mdy(census.date)
ref.date <- mdy(paste("01", "01", year(cens.date), sep = "/"))

table1 <- full_join(child %>% 
  select(age, pdx),
  coef %>% 
    select(-family, -d), by = "age") %>% 
  mutate(Qn = pdx*(a + b*parity[1] + c*parity[2]),
         tx = e + f*parity[1] + g*parity[2],
         ref_date = as.numeric(year(cens.date) + (cens.date - ref.date)/365.25) - tx,
         Yn = 0.5*log(Qn/(1 - Qn)))

table2 <- full_join(table1 %>% 
            select(age, x, ref_date, Yn),
          mlt %>% 
            select(x, YnS = Combined) %>% 
            filter(x != 4), by = "x") %>% 
  mutate(alpha = Yn - YnS,
         q0 = 1/(1 + exp(-2*(alpha + .[["Yn"]][1]))),
         q5 = 1/(1 + exp(-2*(alpha + .[["Yn"]][4]))))

table.brass <- table2 %>% 
  select(age, x, ref_date, q0, q5)

print(table.brass)
}

edos <- names(table(base2$edo))

test <- list()
for (i in 1:32) {
  test[[i]] <- child_brass(base = base2 %>% 
                             filter(edo == edos[i]),
                      census.date = "03/15/2020",
                      coef.family = "Princeton - West",
                      mlt.family = "Princeton West")  %>% 
    mutate(edo = edos[i], .before = "age")
}

basetotal <- do.call(rbind, test)

###############################################################################

ggplot() +
  geom_point(data = basetotal %>% 
              select(edo, ref_date, q0),
    aes(x = ref_date, y = 1000*q0, colour = edo)) +
  scale_x_continuous(n.breaks = 12) +
  theme_light()
