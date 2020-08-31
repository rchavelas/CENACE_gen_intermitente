#### INSTRUCCIONES ####
# 1) Colocar todos los archivos a unir en la carpeta "arch_CENACE"
# 2) Correr archivo en RStudio usando el botón "Source"
# 3) Esperar la magia
# 4) Abrir el archivo llamado "PronGenInter MDA.csv"


#### Magia ####
# Créditos: R. Chavelas

# 1) Inicializar data frame maestro
encab <- c("Sistema", "Tipo_de_Tecnologia", "Hora", "Pronostico_de_Generacion_MWh","Fecha")
pron_data_all <- as.data.frame(matrix(,ncol=5,nrow=0))
names(pron_data_all) <- encab

# 2) Leer archivos de carpeta arch_CENACE
archivos_CENACE_lista <- list.files("arch_CENACE")
print("Lista de archivos:")
archivos_CENACE_lista

# 3) Juntar archivos y procesar fecha
for(archivo_CENACE in archivos_CENACE_lista){
  print(paste("Leyendo archivo:",archivo_CENACE))
  nom_archivo <- paste("arch_CENACE/",archivo_CENACE,sep = "")
  pron_data <- read.csv(nom_archivo, skip = 8, header = F, col.names = encab)
  fecha <- gsub(pattern = "Fecha: ",replacement = "",fixed = T,
                x = read.csv(nom_archivo, nrows = 4, header = F,stringsAsFactors = F)[4,])
  pron_data$Fecha <- fecha
  cat(paste("\n Leyendo archivo -",nom_archivo,"-"))
  cat("\n Primeros 5 registros: \n")
  print(head(pron_data))
  pron_data_all <- rbind(pron_data_all,pron_data)
}

# 4) Escribir data frame maestro en CSV
write.csv(pron_data_all, file = "PronGenInter MDA.csv",row.names = F)

# 5) Revisa salida
head(pron_data_all)
summary(pron_data_all)
