##################################################################

# Code for Acta Botanica Brasilica paper "What are we planting 
# in ecological restoration of tropical open biomes? A systematic 
# review of controlled experiments" (Medeiros et al. 2025)

# all graphs were edited using Inkscape

# Aug 2025



##################################################################

library("ggpattern")
library("ggplot2")
library("dplyr")
library("tidyr")
library("writexl")

###############################################################
### reading tables


data <-read.table ("r_planted_species.txt", head=T)
head(data)


#############################################################



#############################################################

# Figure 1: plant richness used in tropical open ecosystem 
# active restoration planting/seeding research experiments

#############################################################

familly <- data %>%
  select(species_planted, familly) 

sp_familly <- familly %>%
  count(familly)

familly_filtered <- subset(sp_familly, n > 1)

familly_filtered$familly <- factor(familly_filtered$familly, 
                                   levels = familly_filtered$familly[order(-familly_filtered$n)])

ggplot(familly_filtered, aes(x = familly, y = n)) +
  geom_bar(stat = "identity", fill = "#005148") +
  labs(title = "Plant diversity used in TGB active restoration",
       x = "Familly",
       y = "Usage frequency") +
  theme_minimal() +
  theme(panel.grid.major = element_blank())+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5))


#############################################################

# Figure 2: growth-form of the plants used in tropical open 
# ecosystem active restoration planting/seeding research 
# experiments by countries

#############################################################

total_growth_form <- data.frame(data$species_planted, data$growth_form, data$biome)

total_growth_form <- total_growth_form %>% distinct(data.species_planted, .keep_all = TRUE)

total_growth_form <- total_growth_form %>%
  group_by(data.growth_form) %>%
  summarise(total_species = n()) %>%
  arrange(desc(total_species))

growth_form_country <- data %>%
  group_by(id, growth_form, country) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = growth_form, values_from = count, 
              values_fill = 0)
growth_form_country <- data.frame(growth_form_country)
growth_form_country$country[growth_form_country$country == "Hawaii"] <- "USA"
growth_form_country$country[growth_form_country$country == "EUA"] <- "USA"
growth_form_country <- growth_form_country[,-11]

growth_form_country$herbaceous <- ifelse(growth_form_country$herb > 1 | 
                                           growth_form_country$herb.climb.subshrub > 1 |
                                           growth_form_country$herb.subshrub > 1 | 
                                           growth_form_country$herb.subshrub.shrub > 1 |
                                           growth_form_country$herb.shrub > 1, 
                                 "yes", "no")

growth_form_country2 <- growth_form_country %>%
  group_by(id, herbaceous, country) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = herbaceous, values_from = count, 
              values_fill = 0)
growth_form_country2 <- data.frame(growth_form_country2)
growth_form_country2 <- aggregate(cbind(yes, no) ~ country, 
                          data = growth_form_country2, sum)
long_growth_form_country <- pivot_longer(growth_form_country2, 
                                         cols = c(yes, no), 
                                         names_to = "type", 
                                         values_to = "count")

long_growth_form_country$country <- factor(long_growth_form_country$country, 
                                           levels = unique (long_growth_form_country$country[order(long_growth_form_country$count, decreasing = TRUE)]))

sum_growth_form_country <- long_growth_form_country %>%
  group_by(country) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

long_growth_form_country$country <- factor(long_growth_form_country$country, 
                                           levels = sum_growth_form_country$country)

ggplot(long_growth_form_country, aes(x = country, 
                                     y = count, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("yes" = "#44803F", "no" = "#214001")) + 
  labs(title = "Growth-form of of the species planted/sown in 
       TGB active restoration", x = "Country", 
       y = "Number of experiments", fill = "Growth-form") +
  theme_minimal() +
  theme(panel.grid.major = element_blank())


#############################################################

# Figure 3: proportion of herbaceous, shrub and tree species 
# used in tropical open ecosystem active restoration 
# planting/seeding research experiments - data set is available
# at GitHub but the graph was made at RawGraphs 
# < https://www.rawgraphs.io/ > 

#############################################################

alluvial <- data.frame(data$species_planted, 
                       data$growth_form, data$biome)

alluvial <- alluvial %>% distinct(data.species_planted,
                                  .keep_all = TRUE)
biome <- alluvial %>%
  group_by(data.biome, data.growth_form) %>%
  summarise(n_species = n_distinct(data.species_planted))

biome_clean <- biome %>%
  filter(data.growth_form != "na")

total_species_by_biome <- biome_clean %>%
  group_by(data.biome) %>%
  summarise(total_species = sum(n_species))
print (total_species_by_biome)

biome_renamed <- biome_clean %>%
  mutate(growth_form = recode(data.growth_form, 
                         "herb-climb-subshurb" = "herb",
                         "herb-climb-subshrub" = "herb",
                         "herb-subshrub" = "herb",
                         "herb-subshrub-shrub" = "herb",
                         "herb-shrub" = "herb",
                         "shrub-tree" = "shrub",
                         "subshrub" = "shrub",
                         "subshrub-shrub" = "shrub"))

growth_form <- biome_renamed %>%
  group_by(growth_form) %>%
  summarise(total_plants = sum(n_species))
print(growth_form)


#############################################################

# Figure 4: origin of plants used in tropical open ecosystem 
# active restoration planting/seeding research experiments 

#############################################################

native_exotic_sp <- data %>%
  group_by(id, native_exotic, country) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = native_exotic, 
              values_from = count, values_fill = 0)
native_exotic_sp <- data.frame(native_exotic_sp)
native_exotic_sp <- native_exotic_sp[,-4]

unique_native_exotic <- native_exotic_sp%>%
  distinct(country, id) %>%
  group_by(country) %>%
  mutate(exp_number = row_number()) %>%
  ungroup() %>%
  mutate(new_column = paste(country, "_", exp_number, sep = ""))

native_exotic_sp <- native_exotic_sp %>%
  left_join(select(unique_native_exotic, country, 
                   id, new_column), by = c("country", "id"))

long_native_exotic <- pivot_longer(native_exotic_sp, 
                                   cols = c(exotic, native), 
                                   names_to = "type", 
                                   values_to = "count")
long_native_exotic$new_column <- factor(long_native_exotic$new_column, 
                                        levels = unique(long_native_exotic$new_column[order(long_native_exotic$count, decreasing = TRUE)]))

ggplot(long_native_exotic, aes(x = new_column, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("exotic" = "#FFEC5C", "native" = "#44803F")) + 
  labs(title = "Origin of the species planted/sown in TGB active restoration", x = "Restoration experiments", y = "Number of species", fill = "Species origin") +
  theme_minimal() +
  theme(panel.grid.major = element_blank())


#############################################################

# Figure 5: how many times a single plant species was used in
# different tropical open ecosystem restoration experiments

#############################################################

sp_comp <- data %>%
  count(species_planted)

sp_freq <- sp_comp %>%
  group_by(n) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(sp_freq, aes(x = factor(n), y = count)) +
  geom_bar(stat = "identity", fill = "#005148") +
  labs(x = "Usage frequency of the same species", 
       y = "Occurrance in experiments", 
       title = "The usage of species in TGB active restoration") +
  theme_minimal()+
  theme(panel.grid.major = element_blank())


filtered_sp_comp <- sp_comp %>%
  filter(n >4) %>%
  arrange(desc(n))
write_xlsx(filtered_sp_comp, "most_used_sp.xlsx") #save it as .txt

most_used_sp <-read.table ("r_most_used_sp.txt", head=T)

most_used_sp_gf <- most_used_sp %>%
  group_by(growth_form) %>%
  summarise(count = n(), .groups = 'drop')
head(most_used_sp_gf)

most_used_sp_gf <- most_used_sp_gf %>%
  mutate(new_growth_form = case_when(
    growth_form %in% c("herb", "herb-subshrub") ~ "herbaceous",
    TRUE ~ "woody"))

ggplot(most_used_sp_gf, aes(x = "", y = count, fill = new_growth_form)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Growth-form of the most used species") +
  scale_fill_manual(values = c("herbaceous" = "#44803F", 
                               "woody" = "#214001"))+
  theme_void() + theme(legend.title = element_blank())

head(most_used_sp_gf)
(11*100)/58 #19% herbaceous
(47*100)/58 #81% woody



#############################################################

# Figure S1. Mean number of plant species planted in open 
# biome restoration experiments, depending on their growth 
# form: herbs (grass, forbs, graminoids) or woody plant species 
# (ANOVA; n = 29 experiments; p>0.05; F = 0.0706, mean ± SE)

#############################################################

herb_wood <- herb_wood <- data.frame(data$id, data$species_planted, data$growth_form)

herb_wood <- herb_wood %>%
  mutate(data.growth_form = recode(data.growth_form, "herb-climb-subshrub" = "herb", 
                                   "herb-subshrub" = "herb", "herb-subshrub-shrub" = "herb",
                                   "herb-shrub" = "herb", "tree" = "woody", "shrub" = "woody",
                                   "shrub-tree" = "woody", "subshrub-shrub" = "woody",
                                   "subshrub" = "woody"))

herb_wood <- herb_wood %>% distinct(data.species_planted, .keep_all = TRUE)
herb_wood <- herb_wood[-176,]

herb_wood <- herb_wood %>%
  group_by(data.id, data.growth_form) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = data.growth_form, values_from = count, 
              values_fill = 0)
herb_wood <- data.frame(herb_wood)
herb_wood <- herb_wood[,-5]
herb_wood <- herb_wood[,-4]

long_herb_wood <- reshape(herb_wood, varying = c("herb", "woody"), 
                          v.names = "use", timevar = "growth_form", 
                          times = c("herb", "woody"),
                          direction = "long")

anova_test<-lm(use~growth_form, data=long_herb_wood)
plot(anova_test)
anova(anova_test)

ggplot(long_herb_wood, aes(x = growth_form, y = use)) +
  geom_bar(stat = "summary", fun = "mean", 
           fill = c("#44803F", "#704214" )) +
  stat_summary(fun.data = "mean_se",  geom = "errorbar", 
               width = 0.2) + theme_minimal() +
  labs(y = "Number of species", x = "Plant growth form")+
  theme (panel.grid.major = element_blank())


#############################################################

# Figure S2. Mean number of species planted in open biome 
# restoration experiments, depending on their origin: native 
# or exotic plant species 
# (n = 29 experiments; p<0,001; z value = 15.048, mean ± SE)
# GLM (formula = number_plants ~ origin, family = "poisson")

#############################################################

native_exotic_all <- data %>%
  group_by(id, native_exotic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = native_exotic, 
              values_from = count, values_fill = 0)
native_exotic_all <- data.frame(native_exotic_all)
native_exotic_all <- native_exotic_all[,-3]
native_exotic_all$total <- rowSums(native_exotic_all
                                   [, c("exotic", "native")])

shapiro_test_native <- shapiro.test(native_exotic_all$native)
print(shapiro_test_native) 
hist(native_exotic_all$native, main = "Native", 
     xlab = "native plants", col = "lightblue", border = "black")

shapiro_test_exotic <- shapiro.test(native_exotic_all$exotic)
print(shapiro_test_exotic) 
hist(native_exotic_all$exotic, main = "Exotic", 
     xlab = "exotic plants", col = "lightblue", border = "black")


long_native_exotic_all <- reshape(native_exotic_all, 
                                  varying = c("native", "exotic"), 
                                  v.names = "use", timevar = "origin",
                                  times = c("native", "exotic"),
                                  direction = "long")

glm_native_exotic <- glm(use~origin, family = "poisson", 
                         data = long_native_exotic_all)
summary (glm_native_exotic)


ggplot(long_native_exotic_all, aes(x = origin, y = use)) +
  geom_bar(stat = "summary", fun = "mean", 
           fill = c("#214001", "#44803F" )) +
  stat_summary(fun.data = "mean_se",  geom = "errorbar", 
               width = 0.2) + theme_minimal() +
  labs(y = "Number of species", x = "Plant Origin")+
  theme (panel.grid.major = element_blank())


#############################################################
#############################################################