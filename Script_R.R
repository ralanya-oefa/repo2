####################################################################-
#####################  BE Oefa: PIGARS y PMR  ######################-
########################### By Grupo 2 #############################-

################ I. Librerías, drivers y directorio ################

# I.1 Librerías

#install.packages("googledrive")
library(googledrive)
#install.packages("googlesheets4")
library(googlesheets4)
#install.packages("dplyr")
library(dplyr)
#install.packages("httpuv")
library(httpuv)
#install.packages("lubridate")
library(lubridate)
#install.packages("stringr")
library(stringr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)

# I.2 Drivers

# i) Google
correo_usuario <- "proyectossefa@oefa.gob.pe"
drive_auth(email = correo_usuario) 
gs4_auth(token = drive_auth(email = correo_usuario), 
         email = correo_usuario)
#*El token debe estar almacenado y con los permisos de Google


#-----------------------------------------------------------------

################## II. Descarga de información ##################

# II.1 Bases de datos
BASE_MONITOREO <- "1l6j2pNT0CeheA_wbR75GbNeSn-VL58QJYgO-x4qr87w"
HOJA_MONITOREO <- "Control-Tratamiento"

## II.2 Descarga
MONITOREO <- read_sheet(BASE_MONITOREO, sheet = HOJA_MONITOREO)


#-----------------------------------------------------------------

################## III. Proceso de información ##################

MONITOREO_MUNI <- MONITOREO %>%
  mutate(GRUPO_ANALISIS = case_when(GRUPO == "Control" ~ 0,
                                    GRUPO == "Tratamiento" ~ 1,
                                    TRUE ~ NA_real_)) %>%
  filter(!is.na(GRUPO_ANALISIS),
         SUP_21 == "NO")

#-----------------------------------------------------------------

################## III. Análisis exploratorio ##################

# 3.0 Demographics

# 3.0.1 Legal Training Pie

# Data processing
# legal_tr_sum_1 <- my_data %>%
#   filter(Group == "Group 1") %>%
#   summarize(Yes = round(mean(legal_tr)*100,1),
#             No = round((1 - mean(legal_tr))*100,1)) %>%
#   gather(key = "Answer", value = "Percentage") %>%
#   mutate(Group = "Group 1",
#          label = paste0(Percentage,"%"))
# legal_tr_sum_2 <- my_data %>%
#   filter(Group == "Group 2") %>%
#   summarize(Yes = round(mean(legal_tr)*100,1),
#             No = round((1 - mean(legal_tr))*100,1)) %>%
#   gather(key = "Answer", value = "Percentage") %>%
#   mutate(Group = "Group 2",
#          label = paste0(Percentage,"%"))
# legal_tr_sum <- rbind (legal_tr_sum_1, legal_tr_sum_2)
# # Graph
# legal_tr_pie <- legal_tr_sum %>%
#   ggplot(aes(x=1,y = Percentage, fill = Answer)) + 
#   geom_bar(stat="identity",width=2) + 
#   coord_polar(theta='y') +
#   theme(panel.background = element_blank(),
#         axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title = element_blank()) +
#   geom_text(aes(x = 2.5, label = label),
#             position = position_stack(vjust = 0.5)) +
#   scale_x_continuous(limits=c(-1,2.5))+
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   facet_wrap(~Group) 
# legal_tr_pie
# ggsave("D_legal_tr.png",  width = 18, height = 9.5, dpi=350, units="in")
# # Freeing up RAM
# rm(legal_tr_sum_1, legal_tr_sum_2, legal_tr_sum, legal_tr_pie)
# 
# # 3.0.2 Population pyramid
# pop_pyr <- my_data %>%
#   mutate(Category = cut(age, seq(15, 70, 5))) %>%
#   group_by(Category, Group) %>%
#   summarize(Freq = n())  %>%
#   arrange(Group, Category)
# 
# pop_pyr_graph <- pop_pyr %>%
#   ggplot(aes(x = Category)) + 
#   geom_bar(data = pop_pyr[pop_pyr$Group == "Group 1", ], 
#                  aes(y = Freq, fill = Group), 
#                  stat = "identity") + 
#   geom_bar(data = pop_pyr[pop_pyr$Group == "Group 2", ],
#                  aes(y = -Freq, fill = Group), 
#                  stat = "identity") + 
#   coord_flip(ylim=c(-35, 35)) + 
#   scale_y_continuous(breaks=seq(-35, 35, 10), 
#                      labels=c(35, 25, 15, 5, 5, 15, 25, 35)) + 
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum()
# pop_pyr_graph
# ggsave("D_age.png",  width = 15, height = 9.5, dpi=350, units="in")
# # Freeing up RAM
# rm(pop_pyr, pop_pyr_graph)
# 
# # 3.1 Gráficos ----
# 
# # 3.1.1 Time spent on pages with information
# # The two histograms in the same axis
# C_TS_hist <- my_data %>%
#   ggplot( aes(x = TS_Consent, fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   labs(fill="")
# # Two separated histograms
# C_TS_hist_sep <- my_data %>%
#   ggplot( aes(x = TS_Consent, fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   labs(fill="") +
#   facet_wrap(~M1_type, dir = "v")
# # Histograms with log transformation
# C_TS_hist_log <- my_data %>%
#   ggplot( aes(x = log(TS_Consent), fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity') +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   labs(fill="") +
#   facet_wrap(~M1_type, dir = "v")
# # Saving graphs
# C_TS_hist
# ggsave("C_TS_hist1.png",  width = 15, height = 9.5, dpi=350, units="in")
# C_TS_hist_sep
# ggsave("C_TS_hist2.png",  width = 15, height = 9.5, dpi=350, units="in")
# C_TS_hist_log
# ggsave("C_TS_hist3.png",  width = 15, height = 9.5, dpi=350, units="in")
# # Freeing up RAM
# rm(C_TS_hist, C_TS_hist_sep)
# 
# # 3.1.2 Scores
# # The two histograms in the same axis
# C_Scores_hist <- my_data %>%
#   ggplot( aes(x = Score_Consent, fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, 
#                  position = 'identity', binwidth = 1) +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   scale_x_continuous(breaks = seq(0, 10, by=1), limits = c(0,10)) +
#   labs(fill="") 
# # Two separated histograms
# C_Scores_hist_sep <- my_data %>%
#   ggplot( aes(x = Score_Consent, fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, 
#                  position = 'identity', binwidth = 1) +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   labs(fill="") +
#   scale_x_continuous(breaks = seq(0, 10, by=1), limits = c(0,10)) +
#   facet_wrap(~M1_type, dir = "v")
# # Histograms with log transformation
# C_Scores_hist_log <- my_data %>%
#   ggplot( aes(x = log(Score_Consent), fill = M1_type)) +
#   geom_histogram(color = "#e9ecef", alpha = 0.6, 
#                  position = 'identity') +
#   scale_fill_manual(values = c("#69b3a2", "#404080")) + 
#   theme_ipsum() +
#   labs(fill="") +
#   facet_wrap(~M1_type, dir = "v")
# # Saving graphs
# C_Scores_hist
# ggsave("C_Scores_hist1.png",  width = 15, height = 9.5, dpi=350, units="in")
# C_Scores_hist_sep
# ggsave("C_Scores_hist2.png",  width = 15, height = 9.5, dpi=350, units="in")
# C_Scores_hist_log
# ggsave("C_Scores_hist3.png",  width = 15, height = 9.5, dpi=350, units="in")
# # Freeing up RAM
# rm(C_Scores_hist, C_Scores_hist_sep)
# 
# 

#-----------------------------------------------------------------

#################### IV. Análisis econométrico ##################


# IV.1 Modelo MCO con dummy
# Base
DIF_INICIAL = lm(`PUNTAJE INICIAL`  ~ GRUPO_ANALISIS, data = MONITOREO_MUNI)
summary(DIF_INICIAL)

# Tratamiento
DIF_MONITOREO = lm(`PUNTAJE FINAL`  ~ GRUPO_ANALISIS, data = MONITOREO_MUNI)
summary(DIF_MONITOREO)

# IV.1 Modelo MCO con dummy y control
# Base
DIF_INICIAL_1 = lm(`PUNTAJE INICIAL`  ~ GRUPO_ANALISIS + factor(CATEGORIA) - 1, data = MONITOREO_MUNI)
summary(DIF_INICIAL_1)

# Tratamiento
DIF_MONITOREO_1 = lm(`PUNTAJE FINAL`  ~ GRUPO_ANALISIS + factor(CATEGORIA) - 1, data = MONITOREO_MUNI)
summary(DIF_MONITOREO_1)
