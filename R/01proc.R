# Code 1: Import data formR -----------------------------------------------
# 1. Install packages -----------------------------------------------------
if(!require(devtools)) install.packages("devtools")
devtools::install_github("rubenarslan/formr")
if(!require(httr)) install.packages("httr")

library(httr)
library(formr)
library(tidyverse)
library(Hmisc)

# 2. Login using your client ID and client Secret to get an access token ------------------------------------------------------------
login <- list( # define login credentials
  client_id = "18a9624103a7e963684b3306c17fb766",
  client_secret = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9IjE4YTk2MjQxMDNhN2U5NjM2",
  grant_type = "client_credentials"
)
request <- httr::POST( # send POST request
  "https://learn-r.formr.org/oauth/access_token",
  body = login, 
  encode = "form"
)

# parse response to get access token
# If there an error then the response object would contain the details of the error in response$error
response <- content(request)
access_token <- response$access_token

#With a valid access token, call API to get the results of a particular study (run)
query <- list(
  access_token = access_token,
  "run[learn-r]" = "/learn-r/",
  "run[session53]" = "/53X50msYHxuuVpGSrUm_Iv7Lw5A7K73VVj8t96tSHTK2A1ls48szgkEN6qJ5hx_a/",
  "surveys[contact]" = "", # comma separated list of items to get from the survey or leave empty string to get all items
  "surveys[reporteuah]" = ""
)
request <- GET("https://learn-r.formr.org/get/results", query=query)
results <- content(request)

# With a valid response you can, for example, extract the results of a particular survey say 'survey_1':
contact = results$contact


# 3. GET data -------------------------------------------------------------
formr::formr_connect("valentinaandrade@uchile.cl", "Anid2020")

resp <- httr::GET(paste0("https://formr.org/admin/run/learn-r/export_data?format=csv"))
run <- read.table(text=httr::text_content(resp), header=TRUE, sep=",", fill=TRUE, row.names=NULL, as.is=TRUE)

# 4. Process --------------------------------------------------------------
# Explore data
run %>%
  group_by(session) %>% 
  summarise()

# Re-estructure -----------------------------------------------------------
data <- run %>% filter(!type %in% c("note", "submit", "iframenote", "note_feedback", "note_initreporte", "get")) %>%
  select(id=session, survey_name, unit_position,day= created, time = answered, item_name, answer,showif, label) %>%
  pivot_wider(id_cols = c(id, day), names_from = "item_name", values_from = "answer") %>%
  group_by(id) %>%
  fill(university:expectative, .direction = "down") %>%
  ungroup() %>% 
  mutate(number_reporte = ifelse(is.na(number_reporte), 0, number_reporte),
         university = car::recode(.$university, c("1='UAH';2='UDP'; c(3,4)='Otra'")),
         aprendi = car::recode(.$aprendi, c("1='Colaboración y reproducibilidad R'; 2='Procesamiento y manipulación de datos';
                                            3='Análisis descriptivo de datos';4='Análisis de regresión y otras técnicas de modelamiento';
                                            5='Reportes y visualización de resultados'")),
         reason = car::recode(.$reason, c("1='Análisis';2='Investigación';3='Empleabilidad';4='Ciencia abierta';5='No sabe'"))) %>% 
    mutate_at(vars(matches("objetivos|class|reporte")),
            list(~ ifelse(is.na(as.numeric(.)),NA,as.numeric(.)))) %>% 
  mutate_at(vars(matches("objetivos")), list(~car::recode(., c("1='No';2='Parcialmente';3='Si'", as.factor = T,
                                                         levels =c("No", "Parcialmente", "Si"))))) %>%
  separate(before_r, sep = ",", into =c("beforer_1", "beforer_2","beforer_3",
                                        "beforer_4", "beforer_5","beforer_6",
                                        "beforer_7", "beforer_8","beforer_9"), convert = T) %>%
  mutate_at(vars(contains("beforer_")), list(~car::recode(.,c("1='Estadística';2='Difícil';3='Versátil';4='Cuantitativo';5='Sencillo';6='Necesario';7='Incomprensible';8='Accesible';9='Gratis'")))) %>% 
  mutate_at(vars(c("use_r", "instalation_r")), list(~car::recode(., c("1='Si';2='No';3='No lo sé'", as.factor = T,
                                                         levels = c("No", "Si", "No lo sé"))))) %>% 
  mutate(class_asistance = car::recode(.$class_asistance, c("0='No';1='Si'"), as.factor = T))

# Label data --------------------------------------------------------------
var.labels = setNames(as.character(run$label), run$item_name)

Hmisc::label(data) = as.list(var.labels[match(names(data), names(var.labels))])

# Save --------------------------------------------------------------------
saveRDS(data, file = "output/data-surveys.rds")
