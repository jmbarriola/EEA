# Ejercicios para practicar


rm(list=ls())
library(tidyverse)
library(ggthemes)
library(ggridges)

bases.dir      <-  "../Fuentes/"
resultados.dir <- "../Resultados/"


Individual_t117 <- read.table(paste0(bases.dir,"usu_individual_t117.txt"),
                              sep=";", dec=",", header = TRUE, fill = TRUE)

#### - Graficar la distribución del ingreso por ocupación principal (p21) según categoría ocupacional (CAT_OCUP) ####

ggdata <- Individual_t117 %>% 
  select(P21, CAT_OCUP, PONDIH) %>% 
  filter(P21>0) %>% 
  mutate(CAT_OCUP = as.factor(case_when(CAT_OCUP == 1 ~ "Patrón",
                                        CAT_OCUP == 2 ~ "Cuenta propia",
                                        CAT_OCUP == 3 ~ "Obrero",
                                        CAT_OCUP == 4 ~ "Trabajador familiar",
                                        CAT_OCUP == 9 ~ "Ns./Nr.",
                                        CAT_OCUP == 0 ~ "No corresponde",
                                        FALSE         ~ "No responde")))


ggplot(ggdata, aes(x = P21, y = CAT_OCUP, weights = PONDIH, 
                   group = CAT_OCUP, fill = CAT_OCUP)) +
  geom_density_ridges(alpha=0.6)+
  labs(x="", y="",
       title="Distribución del ingreso por ocupación principal",
       subtitle = "según categoría ocupacional", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))

#### Incorporar en el gráfico anterior la condición de precariedad laboral (PP07H). ####

ggdata <- Individual_t117 %>% 
  select(P21, CAT_OCUP, PONDIH, PP07H) %>% 
  filter(P21>0) %>% 
  mutate(CAT_OCUP = as.factor(case_when(CAT_OCUP == 1 ~ "Patrón",
                                        CAT_OCUP == 2 ~ "Cuenta propia",
                                        CAT_OCUP == 3 ~ "Obrero",
                                        CAT_OCUP == 4 ~ "Trabajador familiar",
                                        CAT_OCUP == 9 ~ "Ns./Nr.",
                                        CAT_OCUP == 0 ~ "No corresponde",
                                        FALSE         ~ "No responde")),
         PP07H    = as.factor(case_when(PP07H == 1 ~ "Recibe aportes \n jubilatorios",
                                        PP07H == 2 ~ "No Recibe aportes \n jubilatorios",
                                        PP07H == 0 ~ "No corresponde",
                                        FALSE      ~ "No corresponde")))




ggplot(ggdata, aes(x = P21, y = CAT_OCUP, weights = PONDIH, 
                   group = CAT_OCUP, fill = CAT_OCUP)) +
  geom_density_ridges(alpha=0.6)+
  labs(x="", y="",
       title="Distribución del ingreso por ocupación principal",
       subtitle = "según categoría ocupacional y precariedad", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,50000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))+
  facet_grid(PP07H~., scales = "free")


#### Quedarse sólo con los asalariados (CAT_OCUP = 3), y graficar la relación entre ingreso por ocupación principal(p21), ####
### precariedad laboral (PP07H) y tamaño del establecimiento(PP04C99). 

ggdata <- Individual_t117 %>% 
  select(P21, CAT_OCUP, PONDIH, PP07H, PP04C99, ESTADO) %>% 
  filter(P21>0, CAT_OCUP ==3, ESTADO == 1, PP04C99 %in% 1:3) %>% 
  mutate(PP07H    = as.factor(case_when(PP07H == 1 ~ "Recibe aportes \n jubilatorios",
                                        PP07H == 2 ~ "No Recibe aportes \n jubilatorios",
                                        PP07H == 0 ~ "No corresponde",
                                        FALSE      ~ "No corresponde")),
         PP04C99    = factor(case_when(PP04C99 == 1 ~ 'Hasta 5',
                                          PP04C99 == 2 ~ '6 a 40',
                                          PP04C99 == 3 ~ 'Más de 40',
                                          PP04C99 == 9 ~ 'Ns. / Nr.',
                                          FALSE        ~ 'No corresponde'), 
                             levels = c( 'Hasta 5','6 a 40','Más de 40',
                                         'Ns. / Nr.', 'No corresponde')))

ggplot(ggdata, aes(x = P21, y = PP04C99, weights = PONDIH, 
                   group = PP04C99, fill = PP04C99)) +
  geom_density_ridges2(alpha=0.6)+
  labs(x="", y="",
       title="Distribución del ingreso por ocupación principal",
       subtitle = "según tamaño del establecimiento y precariedad", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_x_continuous(limits = c(0,30000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))+
  facet_grid(PP07H~., scales = "free")

### - Hacer boxplots del ingreso por ocuapción principal, según sexo
### y condición de precariedad.

ggdata <- Individual_t117 %>% 
  select(P21, CH04, PONDIH, PP07H, CAT_OCUP) %>% 
  filter(P21>0, CAT_OCUP %in% c(2,3)) %>% 
  mutate(PP07H    = as.factor(case_when(PP07H == 1 ~ "Obrero \n Recibe aportes \n jubilatorios",
                                        PP07H == 2 ~ "Obrero \n No Recibe aportes \n jubilatorios",
                                        PP07H == 0 ~ "Cuenta Propia",
                                        FALSE      ~ "No corresponde")),
         CH04     = factor(case_when(CH04 == 1 ~ 'Varon',
                                     CH04 == 2 ~ 'Mujer',
                                     FALSE     ~ 'otro')))

ggplot(ggdata, aes(x = CH04 , y = P21, fill = CH04)) +
  geom_boxplot(alpha=0.6)+
  labs(x="", y="",
       title="Distribución del ingreso por ocupación principal",
       subtitle = "según género y precariedad", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_y_continuous(limits = c(0,30000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))+
  facet_grid(.~ PP07H , scales = "free")

### - Incluir en el gráfico anteriro la dimensión de tamaño del establecimiento.

ggdata <- Individual_t117 %>% 
  select(P21, CH04, PONDIH, PP07H, CAT_OCUP, PP04C99) %>% 
  filter(P21>0, CAT_OCUP %in% c(2,3), PP04C99 !=0) %>% 
  mutate(PP07H    = as.factor(case_when(PP07H == 1 ~ "Obrero \n Recibe aportes \n jubilatorios",
                                        PP07H == 2 ~ "Obrero \n No Recibe aportes \n jubilatorios",
                                        PP07H == 0 ~ "Cuenta Propia",
                                        FALSE      ~ "No corresponde")),
         CH04     = factor(case_when(CH04 == 1 ~ 'Varon',
                                     CH04 == 2 ~ 'Mujer',
                                     FALSE     ~ 'otro')),
         PP04C99  = factor(case_when(PP04C99 == 1 ~ 'Hasta 5',
                                     PP04C99 == 2 ~ '6 a 40',
                                     PP04C99 == 3 ~ 'Más de 40',
                                     PP04C99 == 9 ~ 'Ns. / Nr.',
                                     FALSE        ~ 'No corresponde'), 
                             levels = c( 'Hasta 5','6 a 40','Más de 40',
                                         'Ns. / Nr.', 'No corresponde')))

ggplot(ggdata, aes(x = PP04C99 , y = P21, fill = PP04C99)) +
  geom_boxplot(alpha=0.6)+
  labs(x="", y="",
       title="Distribución del ingreso por ocupación principal",
       subtitle = "según género, precariedad y tamaño del establecimiento", 
       caption = "Fuente: Encuesta Permanente de Hogares")+
  scale_y_continuous(limits = c(0,35000))+
  theme_tufte()+
  scale_fill_gdocs()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title   = element_text(size = 12))+
  facet_grid( CH04 ~ PP07H , scales = "free")


#### Horas trabajadas 

variables <- c('CH04',        # Sexo
               'PP3E_TOT',    # Horas trabajadas
               'PP03G')       # La semana pasada, ¿quería trabajar más horas? 1-Si 2-No

datos <- Individual_t117 %>% 
  select(., one_of(variables))

# Me quedo con los asalariados que respondieron sobre su ingreso
datos1 <- datos %>% 
  filter(!is.na(PP3E_TOT),
         !is.na(PP03G),
         !PP3E_TOT %in% c(0,999),
         PP03G    !=9) %>%
  mutate(PP03G= case_when(PP03G==1 ~ 0,
                          PP03G==2 ~ 1),
         Sexo = case_when(CH04==1 ~'Varon',
                          CH04==2 ~'Mujer')) %>% 
  select(-CH04)

ggplot(datos1, aes(x=PP3E_TOT, y=PP03G, color= Sexo, group=Sexo))+
  geom_jitter(alpha=0.03,width = 0,height = 0.01)+ 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=T)+
  geom_vline(xintercept = 35, linetype= 'dashed')+
  labs(x = "Total de Horas trabajadas en la semana en la ocupación principal",
       y ="¿Quería trabajar más horas?",
       title= "Probabilidad de querer trabajar más horas según género")+
  theme_tufte()+
  theme(legend.position = "bottom",
        text = element_text(size=15))+
  scale_color_gdocs()+
  scale_y_continuous(breaks = c(0,.5,1),labels = c("Si","50%","No"))+
  scale_x_continuous(limits = c(0,100))

