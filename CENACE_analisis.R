# Cargar librerías
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(tidyquant)  
## Datos de:
# https://www.cenace.gob.mx/SIM/VISTA/REPORTES/H_PronosticosGen.aspx?N=245&opc=divCssPronosticosGen&site=Pron%C3%B3sticos%20de%20Generaci%C3%B3n%20Intermitente&tipoArch=C&tipoUni=ALL&tipo=All&nombrenodop=
# https://www.cenace.gob.mx/Paginas/SIM/MercadoCP.aspx


CENACE <- read.csv("PronGenInter MDA.csv")
options(scipen=10000)

# Agregar col
CENACE$Dia <- substr(CENACE$Fecha,1,2)
CENACE$Mes <- substr(CENACE$Fecha,4,6)
CENACE$Anio <- substr(CENACE$Fecha,8,11)

# Usar fechas en formato estándar
# COnvertir a fechas en formato POSIX
mes <- CENACE$Mes
mes_num <- case_when(mes == "ene" ~ "01",
                     mes == "feb" ~ "02",
                     mes == "mar" ~ "03",
                     mes == "abr" ~ "04",
                     mes == "may" ~ "05",
                     mes == "jun" ~ "06",
                     mes == "jul" ~ "07",
                     mes == "ago" ~ "08",
                     mes == "sep" ~ "09",
                     mes == "oct" ~ "10",
                     mes == "nov" ~ "11",
                     mes == "dic" ~ "12")
CENACE <- 
  CENACE %>% mutate(Mes = mes_num, Fecha = dmy(paste(Dia,Mes,Anio,sep="/")))
head(CENACE)

# Análisis numérico
## Análisis por tipo de tecnología
CENACE_tipo_tecnologia <- CENACE %>% group_by(Tipo_de_Tecnologia, Fecha) %>% 
  summarise(Pronostico_de_Generacion_MWh_total = sum(Pronostico_de_Generacion_MWh)) 
CENACE_tipo_tecnologia

# Análisis gráfico
## Gráfico por día
plot_gen_dia <- CENACE_tipo_tecnologia %>%  ggplot(aes(x = Fecha, y = Pronostico_de_Generacion_MWh_total,
                                       fill = Tipo_de_Tecnologia, colour = Tipo_de_Tecnologia))+
  geom_line(alpha = 0.3)+
  geom_ma(ma_fun = SMA, n = 14, linetype = 5, size = 1.25)+
  expand_limits(y = 0)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL)+
  theme_bw() +
  labs(title = "Pronóstico de Generación Intermitente diario por tipo de tecnología",
       y = "Pronóstico de Generación (MWh)",
       subtitle = "Del 01/jun/2019 al 30/ago/2020. Línea punteada: promedio móvil a 14 días",
       caption = "Con información de: Área Pública del Sistema de Información del Mercado (SIM) - CENACE \n
       https://www.cenace.gob.mx/Paginas/SIM/MercadoCP.aspx | RCM",
       colour = "Tipo de tecnología")+ 
  theme(legend.justification=c(1,0),
        legend.position="top",
        panel.border = element_blank()) +
  scale_colour_manual(values=c("#0e69b0", "#f9ae4a"))+
  scale_y_continuous(labels = scales::comma,n.breaks = 8)

png("Gráficos/PronGenTipo.png", width = 3200, height = 1900, units = "px",res = 300)
plot_gen_dia
dev.off()


## Gráfico por día - área
plot_gen_dia_acum <- CENACE_tipo_tecnologia %>%  ggplot(aes(x = Fecha, y = Pronostico_de_Generacion_MWh_total, fill = Tipo_de_Tecnologia))+
  geom_area()+
  expand_limits(y = 0)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL)+
  theme_bw() +
  labs(title = "Pronóstico de Generación Intermitente diario por tipo de tecnología - Total",
       y = "Pronóstico de Generación (MWh)",
       subtitle = "Del 01/jun/2019 al 30/ago/2020.",
       caption = "Con información de: Área Pública del Sistema de Información del Mercado (SIM) - CENACE \n
       https://www.cenace.gob.mx/Paginas/SIM/MercadoCP.aspx | RCM",
       fill = "Tipo de tecnología")+ 
  theme(legend.justification=c(1,0),
        legend.position="top",
        panel.border = element_blank()) +
  scale_fill_manual(values=c("#0e69b0", "#f9ae4a"))+
  scale_y_continuous(labels = scales::comma,n.breaks = 8)

png("Gráficos/PronGenTipoTotal.png", width = 3200, height = 1900, units = "px",res = 300)
plot_gen_dia_acum
dev.off()

## Promedio movil de generación total
plot_gen_dia_tot <- CENACE_tipo_tecnologia %>% 
  pivot_wider(names_from = Tipo_de_Tecnologia, values_from = Pronostico_de_Generacion_MWh_total) %>%
  mutate(TOTAL = EOLICA + FOTOVOLTAICA) %>% 
  ggplot(aes(x = Fecha, y = TOTAL)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.2)+
  geom_ma(ma_fun = SMA, n = 15, linetype = 5, size = 1.25)+
  expand_limits(y = 0)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b", name = NULL)+
  theme_bw() +
  labs(title = "Pronóstico de Generación Intermitente diario - Total",
       y = "Pronóstico de Generación (MWh)",
       subtitle = "Del 01/jun/2019 al 30/ago/2020. Línea punteada: promedio móvil a 14 días",
       caption = "Con información de: Área Pública del Sistema de Información del Mercado (SIM) - CENACE \n
       https://www.cenace.gob.mx/Paginas/SIM/MercadoCP.aspx | RCM")+ 
  theme(panel.border = element_blank()) +
  scale_y_continuous(labels = scales::comma,n.breaks = 8)

png("Gráficos/PronGenTotal.png", width = 3200, height = 1900, units = "px",res = 300)
plot_gen_dia_tot
dev.off()


## En proceso
# Análisis por semana/tecnología

# Análisis por hora/mes/tecnología
# CENACE %>% group_by(Mes,Hora,Tipo_de_Tecnologia) %>%
#   summarise(suma_gen = sum(Pronostico_de_Generacion_MWh)) %>%
#   pivot_wider(names_from = Tipo_de_Tecnologia, values_from = suma_gen) %>% 
#   mutate(Total = EOLICA + FOTOVOLTAICA) %>% write.csv("Gener_mes_hora.csv",row.names = F)
 