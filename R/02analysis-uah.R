# Code 2: Analysis --------------------------------------------------------
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, sjPlot, ggsci)

theme_set(theme_sjplot2())

# 2. Load data ------------------------------------------------------------
data <- readRDS("output/data-surveys.rds")


# 3. Data clean ----------------------------------------------------------------------
uah <- data %>%
  filter(university == "UAH" & rut !="")

names(uah)


# Use R -------------------------------------------------------------------
sjPlot::plot_frq(uah, use_r, type = "bar", geom.colors = "#1a9875", show.n = F)

# Install R ---------------------------------------------------------------
sjPlot::plot_frq(uah, instalation_r, type = "bar", geom.colors = "#1a9875", show.n = F)

# Install before ---------------------------------------------------------------
before <- uah %>% select(1:number_reporte) %>% 
  pivot_longer(cols =c(beforer_1:beforer_7), names_to = "number_mention", values_to = "beforer") %>%
  filter(!is.na(beforer)) %>% 
  group_by(beforer) %>% 
  summarise(n = n()*10) %>%
  arrange(-n) %>% 
  ungroup() %>%
  mutate(beforer1 = beforer) %>% 
  column_to_rownames(var = "beforer1")

wordcloud2::wordcloud2(before)

uah %>% select(1:number_reporte) %>% 
  pivot_longer(cols =c(beforer_1:beforer_7), names_to = "number_mention", values_to = "beforer") %>%
  filter(!is.na(beforer)) %>% 
  group_by(beforer) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ggplot(aes(x = reorder(beforer, -prop), y = prop, fill = beforer)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(prop,1), "%")), vjust=-0.25) +
  labs(x = "", y = "%", title = "Conceptos asociados a R", subtitle = "Respuesta de selección múltiple") + 
  guides(fill = F) +
  scale_fill_futurama()

# Reason ------------------------------------------------------------------
uah %>% select(1:number_reporte) %>% 
  group_by(reason) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ggplot(aes(x = reorder(reason, -prop), y = prop, fill = reason)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(prop,1), "%")), vjust=-0.25) +
  labs(x = "", y = "%", title = "Razón principal para tomar el curso") + 
  guides(fill = F) +
  scale_fill_futurama()

# Aprendi -----------------------------------------------------------------
aprendi <- uah %>% select(1:number_reporte) %>% 
  pivot_longer(cols =c(aprendi_1:aprendi_5), names_to = "number_mention", values_to = "aprendi") %>%
  filter(!is.na(aprendi)) %>% 
  group_by(aprendi) %>% 
  summarise(n = n()*10) %>%
  arrange(-n) %>% 
  ungroup() %>%
  mutate(aprendi1 = aprendi) %>% 
  column_to_rownames(var = "aprendi1")

uah %>% select(1:number_reporte) %>% 
  pivot_longer(cols =c(aprendi_1:aprendi_5), names_to = "number_mention", values_to = "aprendi") %>%
  filter(!is.na(aprendi)) %>% 
  group_by(aprendi) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)*100) %>% 
  ggplot(aes(x = reorder(aprendi, -prop), y = prop, fill = aprendi)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(prop,1), "%")), vjust=-0.25) +
  labs(x = "", y = "%", title = "Contenidos preferentes a estudiar en el curso", subtitle = "Respuesta de selección múltiple") + 
  guides(fill = F) +
  scale_fill_futurama()

