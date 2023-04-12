##### KOPPEN-GEIGER CLASSIFICATION #####

## Chargement des packages
library(rgdal) # Manipulation de shapefiles
library(raster) # Manipulation de rasters
library(dplyr) # Manipulation de donnees
library(tidyr) # Manipulation de donnees




#### CLASSIFICATION DONNEES HISTORIQUES ----

### Configuration de l'espace de travail 
setwd("~/_THESE/SIG/RASTERS/CLASSI_KG/LAEA/CLIMATOLOGIES")


### Import des donnees

## Masque de la zone d'etude
Mask <- raster("~/_THESE/SIG/RASTERS/CLASSI_KG/Zone_etude_2.5m_laea.tif")

## Import des moyennes mensuelles de temperature et precipitations
Temp1 <- mask(raster("wc2.1_2.5m_tavg_custom_01_laea.tif"), Mask)
Temp2 <- mask(raster("wc2.1_2.5m_tavg_custom_02_laea.tif"), Mask)
Temp3 <- mask(raster("wc2.1_2.5m_tavg_custom_03_laea.tif"), Mask)
Temp4 <- mask(raster("wc2.1_2.5m_tavg_custom_04_laea.tif"), Mask)
Temp5 <- mask(raster("wc2.1_2.5m_tavg_custom_05_laea.tif"), Mask)
Temp6 <- mask(raster("wc2.1_2.5m_tavg_custom_06_laea.tif"), Mask)
Temp7 <- mask(raster("wc2.1_2.5m_tavg_custom_07_laea.tif"), Mask)
Temp8 <- mask(raster("wc2.1_2.5m_tavg_custom_08_laea.tif"), Mask)
Temp9 <- mask(raster("wc2.1_2.5m_tavg_custom_09_laea.tif"), Mask)
Temp10 <- mask(raster("wc2.1_2.5m_tavg_custom_10_laea.tif"), Mask)
Temp11 <- mask(raster("wc2.1_2.5m_tavg_custom_11_laea.tif"), Mask)
Temp12 <- mask(raster("wc2.1_2.5m_tavg_custom_12_laea.tif"), Mask)
Prec1 <- mask(raster("wc2.1_2.5m_prec_01_laea.tif"), Mask)
Prec2 <- mask(raster("wc2.1_2.5m_prec_02_laea.tif"), Mask)
Prec3 <- mask(raster("wc2.1_2.5m_prec_03_laea.tif"), Mask)
Prec4 <- mask(raster("wc2.1_2.5m_prec_04_laea.tif"), Mask)
Prec5 <- mask(raster("wc2.1_2.5m_prec_05_laea.tif"), Mask)
Prec6 <- mask(raster("wc2.1_2.5m_prec_06_laea.tif"), Mask)
Prec7 <- mask(raster("wc2.1_2.5m_prec_07_laea.tif"), Mask)
Prec8 <- mask(raster("wc2.1_2.5m_prec_08_laea.tif"), Mask)
Prec9 <- mask(raster("wc2.1_2.5m_prec_09_laea.tif"), Mask)
Prec10 <- mask(raster("wc2.1_2.5m_prec_10_laea.tif"), Mask)
Prec11 <- mask(raster("wc2.1_2.5m_prec_11_laea.tif"), Mask)
Prec12 <- mask(raster("wc2.1_2.5m_prec_12_laea.tif"), Mask)


### Creation des variables utilisees pour la classification - voir Peel et al 2007

## Variables sur l'annee

# Mean Annual Precipitation
MAP <- Prec1 + Prec2 + Prec3 + Prec4 + Prec5 + Prec6 + Prec7 + Prec8 + Prec9 + Prec10 + Prec11 + Prec12
# Mean Annual Temperature
MAT <- (Temp1 + Temp2 + Temp3 + Temp4 + Temp5 + Temp6 + Temp7 + Temp8 + Temp9 + Temp10 + Temp11 + Temp12)/12
# Temperature of the hottest month
T_hot <- max(Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, Temp7, Temp8, Temp9, Temp10, Temp11, Temp12)
# Temperature of the coldest month
T_cold <- min(Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, Temp7, Temp8, Temp9, Temp10, Temp11, Temp12)
# Precipitations of the driest month
P_dry <- min(Prec1, Prec2, Prec3, Prec4, Prec5, Prec6, Prec7, Prec8, Prec9, Prec10, Prec11, Prec12)

## Variables sur Ete ou Hiver
T_AMJJAS <- (Temp4 + Temp5 + Temp6 + Temp7 + Temp8 + Temp9)/6
T_ONDJFM <- (Temp1 + Temp2 + Temp3 + Temp10 + Temp11 + Temp12)/6
T_delta <- T_AMJJAS - T_ONDJFM

## Apres calcul, on definit l'Ete par les mois AMJJAS et l'Hiver par les mois ONDJFM

# Precipitations of the driest month in summer
P_sdry <- min(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)
# Precipitations of the driest month in winter
P_wdry <- min(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)
# Precipitations of the wettest month in summer
P_swet <- max(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)
# Precipitations of the wettest month in winter
P_wwet <- max(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)

## Construction variable P_threshold

# Precipitations in winter and summer
P_winter <- Prec1 + Prec2 + Prec3 + Prec10 + Prec11 + Prec12
P_summer <- Prec4 + Prec5 + Prec6 + Prec7 + Prec8 + Prec9
# Configuration de la variable selon l'equilibre des precipitations
P_threshold <- (2 * MAT) + 14
P_threshold[(P_summer/MAP) >= 0.7] <- (2 * MAT[(P_summer/MAP) >= 0.7]) + 28
P_threshold[(P_winter/MAP) >= 0.7] <- 2 * MAT[(P_winter/MAP) >= 0.7]

## Construction variable T_mon10

# Attribution d'un 1 pour chaque mois ou la temperature depasse les 10 degres
Temp10_1 <- Temp1
Temp10_1[Temp10_1 <= 10] <- 0
Temp10_1[Temp10_1 > 10] <- 1
Temp10_2 <- Temp2
Temp10_2[Temp10_2 <= 10] <- 0
Temp10_2[Temp10_2 > 10] <- 1
Temp10_3 <- Temp3
Temp10_3[Temp10_3 <= 10] <- 0
Temp10_3[Temp10_3 > 10] <- 1
Temp10_4 <- Temp4
Temp10_4[Temp10_4 <= 10] <- 0
Temp10_4[Temp10_4 > 10] <- 1
Temp10_5 <- Temp5
Temp10_5[Temp10_5 <= 10] <- 0
Temp10_5[Temp10_5 > 10] <- 1
Temp10_6 <- Temp6
Temp10_6[Temp10_6 <= 10] <- 0
Temp10_6[Temp10_6 > 10] <- 1
Temp10_7 <- Temp7
Temp10_7[Temp10_7 <= 10] <- 0
Temp10_7[Temp10_7 > 10] <- 1
Temp10_8 <- Temp8
Temp10_8[Temp10_8 <= 10] <- 0
Temp10_8[Temp10_8 > 10] <- 1
Temp10_9 <- Temp9
Temp10_9[Temp10_9 <= 10] <- 0
Temp10_9[Temp10_9 > 10] <- 1
Temp10_10 <- Temp10
Temp10_10[Temp10_10 <= 10] <- 0
Temp10_10[Temp10_10 > 10] <- 1
Temp10_11 <- Temp11
Temp10_11[Temp10_11 <= 10] <- 0
Temp10_11[Temp10_11 > 10] <- 1
Temp10_12 <- Temp12
Temp10_12[Temp10_12 <= 10] <- 0
Temp10_12[Temp10_12 > 10] <- 1

# Somme des variables mensuelles
T_mon10 <- Temp10_1 + Temp10_2 + Temp10_3 + Temp10_4 + Temp10_5 + Temp10_6 + Temp10_7 + Temp10_8 + Temp10_9 + Temp10_10 + Temp10_11 +  Temp10_12



### Classification


## Premier niveau de classification (A = 100, B = 200, etc.)

# Import d'un raster avec les NA en mer et attribution d'un zero partout
Classi <- Temp1
Classi[is.na(Classi) == F] <- 0

# Criteres de classification
Classi[T_cold >= 18] <- 100
Classi[(T_hot > 10) & (T_cold < 18) & (T_cold > 0)] <- 300
Classi[(T_hot > 10) & (T_cold <= 0)] <- 400
Classi[T_hot <= 10] <- 500
# On finit par la categorie B car les zones en B peuvent etre aussi dans d'autres categories
Classi[MAP < 10 * P_threshold] <- 200


## Deuxieme niveau de classification

# Import du premier niveau
Classi2 <- Classi

# Criteres de classification
Classi2[(Classi == 100) & (P_dry >= 60)] <- 110
Classi2[(Classi == 100) & (Classi2 != 110) & (P_dry >= (100 - (MAP/25)))] <- 120
Classi2[(Classi == 100) & (Classi2 != 110) & (P_dry < (100 - (MAP/25)))] <- 130

Classi2[(Classi == 200) & (MAP < 5 * P_threshold)] <- 210
Classi2[(Classi == 200) & (MAP >= 5 * P_threshold)] <- 220

Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 310
Classi2[(Classi == 300) & (P_wdry < (P_swet/10))] <- 320
# Categories 310 et 320 non mutuellement exclusives, on rajoute un critere pour les discriminer (selon Peel et al 2007)
Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 310
Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 320
Classi2[(Classi == 300) & (Classi2 != 310) & (Classi2 != 320)] <- 330

Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 410
Classi2[(Classi == 400) & (P_wdry < (P_swet/10))] <- 420
# Categories 410 et 420 non mutuellement exclusives, on rajoute un critere pour les discriminer (selon Peel et al 2007)
Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 410
Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 420
Classi2[(Classi == 400) & (Classi2 != 410) & (Classi2 != 420)] <- 430

Classi2[(Classi == 500) & (T_hot > 0)] <- 510
Classi2[(Classi == 500) & (T_hot <= 0)] <- 520


## Troisieme niveau de classification

# Import du second niveau
Classi3 <- Classi2

# Criteres de classification
Classi3[(Classi2 == 210) & (MAT >= 18)] <- 211
Classi3[(Classi2 == 210) & (MAT < 18)] <- 212
Classi3[(Classi2 == 220) & (MAT >= 18)] <- 221
Classi3[(Classi2 == 220) & (MAT < 18)] <- 222

Classi3[(Classi2 == 310) & (T_hot >= 22)] <- 311
Classi3[(Classi2 == 310) & (Classi3 != 311) & (T_mon10 >= 4)] <- 312
Classi3[(Classi2 == 310) & (Classi3 != 311) & (Classi3 != 312) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 313
Classi3[(Classi2 == 320) & (T_hot >= 22)] <- 321
Classi3[(Classi2 == 320) & (Classi3 != 321) & (T_mon10 >= 4)] <- 322
Classi3[(Classi2 == 320) & (Classi3 != 321) & (Classi3 != 322) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 323
Classi3[(Classi2 == 330) & (T_hot >= 22)] <- 331
Classi3[(Classi2 == 330) & (Classi3 != 331) & (T_mon10 >= 4)] <- 332
Classi3[(Classi2 == 330) & (Classi3 != 331) & (Classi3 != 332) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 333

Classi3[(Classi2 == 410) & (T_hot >= 22)] <- 411
Classi3[(Classi2 == 410) & (Classi3 != 411) & (T_mon10 >= 4)] <- 412
Classi3[(Classi2 == 410) & (Classi3 != 411) & (Classi3 != 412) & (T_cold < -38)] <- 414
Classi3[(Classi2 == 410) & (Classi3 != 411) & (Classi3 != 412) & (Classi3 != 414)] <- 413
Classi3[(Classi2 == 420) & (T_hot >= 22)] <- 421
Classi3[(Classi2 == 420) & (Classi3 != 421) & (T_mon10 >= 4)] <- 422
Classi3[(Classi2 == 420) & (Classi3 != 421) & (Classi3 != 422) & (T_cold < -38)] <- 424
Classi3[(Classi2 == 420) & (Classi3 != 421) & (Classi3 != 422) & (Classi3 != 424)] <- 423
Classi3[(Classi2 == 430) & (T_hot >= 22)] <- 431
Classi3[(Classi2 == 430) & (Classi3 != 431) & (T_mon10 >= 4)] <- 432
Classi3[(Classi2 == 430) & (Classi3 != 431) & (Classi3 != 432) & (T_cold < -38)] <- 434
Classi3[(Classi2 == 430) & (Classi3 != 431) & (Classi3 != 432) & (Classi3 != 424)] <- 433

# Enregistrement du troisieme niveau au format raster
writeRaster(Classi3, filename = "Climate_classi_KG_1970_2000_2.5m_laea.tif")



#### CLASSIFICATION PROJECTIONS FUTURES ----

### Configuration de l'espace de travail 
setwd("~/_THESE/SIG/RASTERS/CLASSI_KG/LAEA/FUTURE_CMIP6")

## Masque de la zone d'etude
Mask <- raster("~/_THESE/SIG/RASTERS/CLASSI_KG/Zone_etude_2.5m_laea.tif")

### Modeles climatiques et scenarios futurs
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "IPSL-CM6A-LR",
         "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")

### Boucle pour chaque GCM et SSP
for (i in SSP) {
  for (j in GCM) {

    ## Import des moyennes mensuelles de temperature et precipitations
    Temp1 <- mask(raster(paste0("wc2.1_2.5m_tavg_1_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp2 <- mask(raster(paste0("wc2.1_2.5m_tavg_2_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp3 <- mask(raster(paste0("wc2.1_2.5m_tavg_3_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp4 <- mask(raster(paste0("wc2.1_2.5m_tavg_4_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp5 <- mask(raster(paste0("wc2.1_2.5m_tavg_5_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp6 <- mask(raster(paste0("wc2.1_2.5m_tavg_6_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp7 <- mask(raster(paste0("wc2.1_2.5m_tavg_7_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp8 <- mask(raster(paste0("wc2.1_2.5m_tavg_8_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp9 <- mask(raster(paste0("wc2.1_2.5m_tavg_9_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp10 <- mask(raster(paste0("wc2.1_2.5m_tavg_10_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Temp11 <- mask(raster(paste0("wc2.1_2.5m_tavg_11_", j,"_",  i, "_2081-2100_laea.tif")), Mask)
    Temp12 <- mask(raster(paste0("wc2.1_2.5m_tavg_12_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec1 <- mask(raster(paste0("wc2.1_2.5m_prec_1_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec2 <- mask(raster(paste0("wc2.1_2.5m_prec_2_",j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec3 <- mask(raster(paste0("wc2.1_2.5m_prec_3_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec4 <- mask(raster(paste0("wc2.1_2.5m_prec_4_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec5 <- mask(raster(paste0("wc2.1_2.5m_prec_5_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec6 <- mask(raster(paste0("wc2.1_2.5m_prec_6_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec7 <- mask(raster(paste0("wc2.1_2.5m_prec_7_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec8 <- mask(raster(paste0("wc2.1_2.5m_prec_8_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec9 <- mask(raster(paste0("wc2.1_2.5m_prec_9_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec10 <- mask(raster(paste0("wc2.1_2.5m_prec_10_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec11 <- mask(raster(paste0("wc2.1_2.5m_prec_11_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    Prec12 <- mask(raster(paste0("wc2.1_2.5m_prec_12_", j, "_", i, "_2081-2100_laea.tif")), Mask)
    
    
    ### Creation des variables utilisees pour la classification - voir Peel et al 2007
    
    ## Variables sur l'annee
    
    # Mean Annual Precipitation
    MAP <- Prec1 + Prec2 + Prec3 + Prec4 + Prec5 + Prec6 + Prec7 + Prec8 + Prec9 + Prec10 + Prec11 + Prec12
    # Mean Annual Temperature
    MAT <- (Temp1 + Temp2 + Temp3 + Temp4 + Temp5 + Temp6 + Temp7 + Temp8 + Temp9 + Temp10 + Temp11 + Temp12)/12
    # Temperature of the hottest month
    T_hot <- max(Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, Temp7, Temp8, Temp9, Temp10, Temp11, Temp12)
    # Temperature of the coldest month
    T_cold <- min(Temp1, Temp2, Temp3, Temp4, Temp5, Temp6, Temp7, Temp8, Temp9, Temp10, Temp11, Temp12)
    # Precipitations of the driest month
    P_dry <- min(Prec1, Prec2, Prec3, Prec4, Prec5, Prec6, Prec7, Prec8, Prec9, Prec10, Prec11, Prec12)
    
    ## Variables sur Ete ou Hiver
    T_AMJJAS <- (Temp4 + Temp5 + Temp6 + Temp7 + Temp8 + Temp9)/6
    T_ONDJFM <- (Temp1 + Temp2 + Temp3 + Temp10 + Temp11 + Temp12)/6
    T_delta <- T_AMJJAS - T_ONDJFM
    
    ## Pour la quasi totalite des cases on definit l'Ete par les mois AMJJAS (T_delta > 0) et l'Hiver par les mois ONDJFM
    
    # Precipitations of the driest month in summer
    P_sdry <- min(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)
    P_sdry[T_delta < 0] <- min(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)[T_delta < 0]
    # Precipitations of the driest month in winter
    P_wdry <- min(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)
    P_wdry[T_delta < 0] <- min(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)[T_delta < 0]
    # Precipitations of the wettest month in summer
    P_swet <- max(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)
    P_swet[T_delta < 0] <- max(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)[T_delta < 0]
    # Precipitations of the wettest month in winter
    P_wwet <- max(Prec1, Prec2, Prec3, Prec10, Prec11, Prec12)
    P_wwet[T_delta < 0] <- max(Prec4, Prec5, Prec6, Prec7, Prec8, Prec9)[T_delta < 0]
    
    ## Construction variable P_threshold
    
    # Precipitations in winter and summer
    P_winter <- Prec1 + Prec2 + Prec3 + Prec10 + Prec11 + Prec12
    P_winter[T_delta < 0] <- (Prec4 + Prec5 + Prec6 + Prec7 + Prec8 + Prec9)[T_delta < 0]
    P_summer <- Prec4 + Prec5 + Prec6 + Prec7 + Prec8 + Prec9
    P_summer[T_delta < 0] <- (Prec1 + Prec2 + Prec3 + Prec10 + Prec11 + Prec12)[T_delta < 0]
    # Configuration de la variable selon l'equilibre des precipitations
    P_threshold <- (2 * MAT) + 14
    P_threshold[(P_summer/MAP) >= 0.7] <- (2 * MAT[(P_summer/MAP) >= 0.7]) + 28
    P_threshold[(P_winter/MAP) >= 0.7] <- 2 * MAT[(P_winter/MAP) >= 0.7]
    
    ## Construction variable T_mon10
    
    # Attribution d'un 1 pour chaque mois ou la temperature depasse les 10 degres
    Temp10_1 <- Temp1
    Temp10_1[Temp10_1 <= 10] <- 0
    Temp10_1[Temp10_1 > 10] <- 1
    Temp10_2 <- Temp2
    Temp10_2[Temp10_2 <= 10] <- 0
    Temp10_2[Temp10_2 > 10] <- 1
    Temp10_3 <- Temp3
    Temp10_3[Temp10_3 <= 10] <- 0
    Temp10_3[Temp10_3 > 10] <- 1
    Temp10_4 <- Temp4
    Temp10_4[Temp10_4 <= 10] <- 0
    Temp10_4[Temp10_4 > 10] <- 1
    Temp10_5 <- Temp5
    Temp10_5[Temp10_5 <= 10] <- 0
    Temp10_5[Temp10_5 > 10] <- 1
    Temp10_6 <- Temp6
    Temp10_6[Temp10_6 <= 10] <- 0
    Temp10_6[Temp10_6 > 10] <- 1
    Temp10_7 <- Temp7
    Temp10_7[Temp10_7 <= 10] <- 0
    Temp10_7[Temp10_7 > 10] <- 1
    Temp10_8 <- Temp8
    Temp10_8[Temp10_8 <= 10] <- 0
    Temp10_8[Temp10_8 > 10] <- 1
    Temp10_9 <- Temp9
    Temp10_9[Temp10_9 <= 10] <- 0
    Temp10_9[Temp10_9 > 10] <- 1
    Temp10_10 <- Temp10
    Temp10_10[Temp10_10 <= 10] <- 0
    Temp10_10[Temp10_10 > 10] <- 1
    Temp10_11 <- Temp11
    Temp10_11[Temp10_11 <= 10] <- 0
    Temp10_11[Temp10_11 > 10] <- 1
    Temp10_12 <- Temp12
    Temp10_12[Temp10_12 <= 10] <- 0
    Temp10_12[Temp10_12 > 10] <- 1
    
    # Somme des variables mensuelles
    T_mon10 <- Temp10_1 + Temp10_2 + Temp10_3 + Temp10_4 + Temp10_5 + Temp10_6 + Temp10_7 + Temp10_8 + Temp10_9 + Temp10_10 + Temp10_11 +  Temp10_12
    
    
    
    ### Classification
    
    
    ## Premier niveau de classification (A = 100, B = 200, etc.)
    
    # Import d'un raster avec les NA en mer et attribution d'un zero partout
    Classi <- Temp1
    Classi[is.na(Classi) == F] <- 0
    
    # Criteres de classification
    Classi[T_cold >= 18] <- 100
    Classi[(T_hot > 10) & (T_cold < 18) & (T_cold > 0)] <- 300
    Classi[(T_hot > 10) & (T_cold <= 0)] <- 400
    Classi[T_hot <= 10] <- 500
    # On finit par la categorie B car les zones en B peuvent etre aussi dans d'autres categories
    Classi[MAP < 10 * P_threshold] <- 200
    
    
    ## Deuxieme niveau de classification
    
    # Import du premier niveau
    Classi2 <- Classi
    
    # Criteres de classification
    Classi2[(Classi == 100) & (P_dry >= 60)] <- 110
    Classi2[(Classi == 100) & (Classi2 != 110) & (P_dry >= (100 - (MAP/25)))] <- 120
    Classi2[(Classi == 100) & (Classi2 != 110) & (P_dry < (100 - (MAP/25)))] <- 130
    
    Classi2[(Classi == 200) & (MAP < 5 * P_threshold)] <- 210
    Classi2[(Classi == 200) & (MAP >= 5 * P_threshold)] <- 220
    
    Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 310
    Classi2[(Classi == 300) & (P_wdry < (P_swet/10))] <- 320
    # Categories 310 et 320 non mutuellement exclusives, on rajoute un critere pour les discriminer (selon Peel et al 2007)
    Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 310
    Classi2[(Classi == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 320
    Classi2[(Classi == 300) & (Classi2 != 310) & (Classi2 != 320)] <- 330
    
    Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 410
    Classi2[(Classi == 400) & (P_wdry < (P_swet/10))] <- 420
    # Categories 410 et 420 non mutuellement exclusives, on rajoute un critere pour les discriminer (selon Peel et al 2007)
    Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 410
    Classi2[(Classi == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 420
    Classi2[(Classi == 400) & (Classi2 != 410) & (Classi2 != 420)] <- 430
    
    Classi2[(Classi == 500) & (T_hot > 0)] <- 510
    Classi2[(Classi == 500) & (T_hot <= 0)] <- 520
    
    
    ## Troisieme niveau de classification
    
    # Import du second niveau
    Classi3 <- Classi2
    
    # Criteres de classification
    Classi3[(Classi2 == 210) & (MAT >= 18)] <- 211
    Classi3[(Classi2 == 210) & (MAT < 18)] <- 212
    Classi3[(Classi2 == 220) & (MAT >= 18)] <- 221
    Classi3[(Classi2 == 220) & (MAT < 18)] <- 222
    
    Classi3[(Classi2 == 310) & (T_hot >= 22)] <- 311
    Classi3[(Classi2 == 310) & (Classi3 != 311) & (T_mon10 >= 4)] <- 312
    Classi3[(Classi2 == 310) & (Classi3 != 311) & (Classi3 != 312) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 313
    Classi3[(Classi2 == 320) & (T_hot >= 22)] <- 321
    Classi3[(Classi2 == 320) & (Classi3 != 321) & (T_mon10 >= 4)] <- 322
    Classi3[(Classi2 == 320) & (Classi3 != 321) & (Classi3 != 322) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 323
    Classi3[(Classi2 == 330) & (T_hot >= 22)] <- 331
    Classi3[(Classi2 == 330) & (Classi3 != 331) & (T_mon10 >= 4)] <- 332
    Classi3[(Classi2 == 330) & (Classi3 != 331) & (Classi3 != 332) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 333
    
    Classi3[(Classi2 == 410) & (T_hot >= 22)] <- 411
    Classi3[(Classi2 == 410) & (Classi3 != 411) & (T_mon10 >= 4)] <- 412
    Classi3[(Classi2 == 410) & (Classi3 != 411) & (Classi3 != 412) & (T_cold < -38)] <- 414
    Classi3[(Classi2 == 410) & (Classi3 != 411) & (Classi3 != 412) & (Classi3 != 414)] <- 413
    Classi3[(Classi2 == 420) & (T_hot >= 22)] <- 421
    Classi3[(Classi2 == 420) & (Classi3 != 421) & (T_mon10 >= 4)] <- 422
    Classi3[(Classi2 == 420) & (Classi3 != 421) & (Classi3 != 422) & (T_cold < -38)] <- 424
    Classi3[(Classi2 == 420) & (Classi3 != 421) & (Classi3 != 422) & (Classi3 != 424)] <- 423
    Classi3[(Classi2 == 430) & (T_hot >= 22)] <- 431
    Classi3[(Classi2 == 430) & (Classi3 != 431) & (T_mon10 >= 4)] <- 432
    Classi3[(Classi2 == 430) & (Classi3 != 431) & (Classi3 != 432) & (T_cold < -38)] <- 434
    Classi3[(Classi2 == 430) & (Classi3 != 431) & (Classi3 != 432) & (Classi3 != 424)] <- 433
    
    # Enregistrement du troisieme niveau au format raster
    writeRaster(Classi3,
                filename = paste0("Climate_classi_KG_2090_", 
                                  i, 
                                  "_", 
                                  j, 
                                  "_2.5m_laea.tif"))
    
    print(j)
  }
  print(i)
}



#### CALCUL DU MODE POUR L'ENSEMBLE DES MODELES ----

## Configuration de l'espace de travail 
setwd("~/_THESE/SIG/RASTERS/CLASSI_KG/CLASSI")

## Import des donnees
R1 <- raster("Climate_classi_KG_2090_ssp585_BCC-CSM2-MR_2.5m_laea.tif")
R2 <- raster("Climate_classi_KG_2090_ssp585_CanESM5_2.5m_laea.tif")
R3 <- raster("Climate_classi_KG_2090_ssp585_CNRM-CM6-1_2.5m_laea.tif")
R4 <- raster("Climate_classi_KG_2090_ssp585_CNRM-ESM2-1_2.5m_laea.tif")
R5 <- raster("Climate_classi_KG_2090_ssp585_IPSL-CM6A-LR_2.5m_laea.tif")
R6 <- raster("Climate_classi_KG_2090_ssp585_MIROC6_2.5m_laea.tif")
R7 <- raster("Climate_classi_KG_2090_ssp585_MIROC-ES2L_2.5m_laea.tif")
R8 <- raster("Climate_classi_KG_2090_ssp585_MRI-ESM2-0_2.5m_laea.tif")

## Calcul et enregistrement du mode
R_mode <- modal(R1, R2, R3, R4, R5, R6, R7, R8, ties = "random")
writeRaster(R_mode, filename = "Climate_classi_KG_2090_ssp585_mean_model_2.5m_laea.tif")

## Calcul et enregistrement de la frequence du mode
R_error <- modal(R1, R2, R3, R4, R5, R6, R7, R8, freq = TRUE)
writeRaster(R_error, filename = "Climate_classi_KG_2090_ssp585_mean_model_freq_2.5m_laea.tif")






#### DIFFERENCES CLASSIFICATIONS FUTURES ET ACTUELLES ZONE ETUDE ----

## Configuration de l'espace de travail 
setwd("~/_THESE/SIG/RASTERS/CLASSI_KG/CLASSI")


### Construction d'un raster avec des 2 quand changement de climat et des 1 sinon

## Preparation du traitement
SSP <- c("ssp126", "ssp245", "ssp370", "ssp585")
GCM <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1",
         "IPSL-CM6A-LR", "MIROC6", "MIROC-ES2L", "MRI-ESM2-0")
Rp <- raster("~/_THESE/SIG/RASTERS/CLASSI_KG/CLASSI/Climate_classi_KG_1970_2000_2.5m_laea.tif")

## Boucle pour chaque GCM et SSP
for (i in SSP) {
  for (j in GCM) {
    Rf <- raster(paste0("Climate_classi_KG_2090_", i, "_", j, "_2.5m_laea.tif"))
    Rdif <- Rp - Rf
    Rdif[Rdif != 0] <- 2
    Rdif[Rdif == 0] <- 1
    writeRaster(Rdif, filename = paste0("Difference_Climate_classi_KG_2090_", i, "_", j, "_2.5m_laea.tif"))
    print(j)
  }
}




##### FIN DU SCRIPT #####