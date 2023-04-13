##### KOPPEN-GEIGER CLIMATE CLASSIFICATION #####




Classi.KG <- function(Temp, Prec, level = 3) {
  
  ## Load required package
  if (!("raster" %in% installed.packages()[, "Package"])) {
    install.packages("raster", dependencies = TRUE)
  }
  
  library(raster)
  
  ## Compute criteria
  # Definition of winter and summer
  Delta_T <- mean(Temp[[4]], Temp[[5]], Temp[[6]], Temp[[7]], Temp[[8]], Temp[[9]]) - mean(Temp[[1]], Temp[[2]], Temp[[3]], Temp[[10]], Temp[[11]], Temp[[12]])
  # Mean Annual Precipitation
  MAP <- Prec[[1]] + Prec[[2]] + Prec[[3]] + Prec[[4]] + Prec[[5]] + Prec[[6]] + Prec[[7]] + Prec[[8]] + Prec[[9]] + Prec[[10]] + Prec[[11]] + Prec[[12]]
  # Mean Annual Temperature
  MAT <- mean(Temp[[1]], Temp[[2]], Temp[[3]], Temp[[4]], Temp[[5]], Temp[[6]], Temp[[7]], Temp[[8]], Temp[[9]], Temp[[10]], Temp[[11]], Temp[[12]])
  # Temperature of the hottest month
  T_hot <- max(Temp[[1]], Temp[[2]], Temp[[3]], Temp[[4]], Temp[[5]], Temp[[6]], Temp[[7]], Temp[[8]], Temp[[9]], Temp[[10]], Temp[[11]], Temp[[12]])
  # Temperature of the coldest month
  T_cold <- min(Temp[[1]], Temp[[2]], Temp[[3]], Temp[[4]], Temp[[5]], Temp[[6]], Temp[[7]], Temp[[8]], Temp[[9]], Temp[[10]], Temp[[11]], Temp[[12]])
  # Precipitations of the driest month
  P_dry <- min(Prec[[1]], Prec[[2]], Prec[[3]], Prec[[4]], Prec[[5]], Prec[[6]], Prec[[7]], Prec[[8]], Prec[[9]], Prec[[10]], Prec[[11]], Prec[[12]])
  # Precipitations of the driest month in summer
  P_sdry <- min(Prec[[4]], Prec[[5]], Prec[[6]], Prec[[7]], Prec[[8]], Prec[[9]])
  P_sdry[Delta_T < 0] <- min(Prec[[1]], Prec[[2]], Prec[[3]], Prec[[10]], Prec[[11]], Prec[[12]])[Delta_T < 0]
  # Precipitations of the driest month in winter
  P_wdry <- min(Prec[[1]], Prec[[2]], Prec[[3]], Prec[[10]], Prec[[11]], Prec[[12]])
  P_wdry[Delta_T < 0] <- min(Prec[[4]], Prec[[5]], Prec[[6]], Prec[[7]], Prec[[8]], Prec[[9]])[Delta_T < 0]
  # Precipitations of the wettest month in summer
  P_swet <- max(Prec[[4]], Prec[[5]], Prec[[6]], Prec[[7]], Prec[[8]], Prec[[9]])
  P_swet[Delta_T < 0] <- max(Prec[[1]], Prec[[2]], Prec[[3]], Prec[[10]], Prec[[11]], Prec[[12]])[Delta_T < 0]
  # Precipitations of the wettest month in winter
  P_wwet <- max(Prec[[1]], Prec[[2]], Prec[[3]], Prec[[10]], Prec[[11]], Prec[[12]])
  P_wwet[Delta_T < 0] <- max(Prec[[4]], Prec[[5]], Prec[[6]], Prec[[7]], Prec[[8]], Prec[[9]])[Delta_T < 0]
  # Precipitations in winter and summer
  P_winter <- Prec[[1]] + Prec[[2]] + Prec[[3]] + Prec[[10]] + Prec[[11]] + Prec[[12]]
  P_winter[Delta_T < 0] <- (Prec[[4]] + Prec[[5]] + Prec[[6]] + Prec[[7]] + Prec[[8]] + Prec[[9]])[Delta_T < 0]
  P_summer <- Prec[[4]] + Prec[[5]] + Prec[[6]] + Prec[[7]] + Prec[[8]] + Prec[[9]]
  P_summer[Delta_T < 0] <- (Prec[[1]] + Prec[[2]] + Prec[[3]] + Prec[[10]] + Prec[[11]] + Prec[[12]])[Delta_T < 0]
  # Precipitation threshold
  P_threshold <- (2 * MAT) + 14
  P_threshold[(P_summer/MAP) >= 0.7] <- (2 * MAT[(P_summer/MAP) >= 0.7]) + 28
  P_threshold[(P_winter/MAP) >= 0.7] <- 2 * MAT[(P_winter/MAP) >= 0.7]
  # Number of months when the temperature is above 10
  Temp10_1 <- Temp[[1]]
  Temp10_1[Temp10_1 <= 10] <- 0
  Temp10_1[Temp10_1 > 10] <- 1
  Temp10_2 <- Temp[[2]]
  Temp10_2[Temp10_2 <= 10] <- 0
  Temp10_2[Temp10_2 > 10] <- 1
  Temp10_3 <- Temp[[3]]
  Temp10_3[Temp10_3 <= 10] <- 0
  Temp10_3[Temp10_3 > 10] <- 1
  Temp10_4 <- Temp[[4]]
  Temp10_4[Temp10_4 <= 10] <- 0
  Temp10_4[Temp10_4 > 10] <- 1
  Temp10_5 <- Temp[[5]]
  Temp10_5[Temp10_5 <= 10] <- 0
  Temp10_5[Temp10_5 > 10] <- 1
  Temp10_6 <- Temp[[6]]
  Temp10_6[Temp10_6 <= 10] <- 0
  Temp10_6[Temp10_6 > 10] <- 1
  Temp10_7 <- Temp[[7]]
  Temp10_7[Temp10_7 <= 10] <- 0
  Temp10_7[Temp10_7 > 10] <- 1
  Temp10_8 <- Temp[[8]]
  Temp10_8[Temp10_8 <= 10] <- 0
  Temp10_8[Temp10_8 > 10] <- 1
  Temp10_9 <- Temp[[9]]
  Temp10_9[Temp10_9 <= 10] <- 0
  Temp10_9[Temp10_9 > 10] <- 1
  Temp10_10 <- Temp[[10]]
  Temp10_10[Temp10_10 <= 10] <- 0
  Temp10_10[Temp10_10 > 10] <- 1
  Temp10_11 <- Temp[[11]]
  Temp10_11[Temp10_11 <= 10] <- 0
  Temp10_11[Temp10_11 > 10] <- 1
  Temp10_12 <- Temp[[12]]
  Temp10_12[Temp10_12 <= 10] <- 0
  Temp10_12[Temp10_12 > 10] <- 1
  T_mon10 <- Temp10_1 + Temp10_2 + Temp10_3 + Temp10_4 + Temp10_5 + Temp10_6 + Temp10_7 + Temp10_8 + Temp10_9 + Temp10_10 + Temp10_11 +  Temp10_12

  ## Compute 1st level of Koppen-Geiger climate classification
  Classi_1 <- Temp[[1]]
  Classi_1[is.na(Classi_1) == F] <- 0
  Classi_1[T_cold >= 18] <- 100
  Classi_1[(T_hot > 10) & (T_cold < 18) & (T_cold > 0)] <- 300
  Classi_1[(T_hot > 10) & (T_cold <= 0)] <- 400
  Classi_1[T_hot <= 10] <- 500
  Classi_1[MAP < 10 * P_threshold] <- 200
  
  ## Compute 2nd level of Koppen-Geiger climate classification
  if (level >= 2) {
    Classi_2 <- Classi_1
    Classi_2[(Classi_1 == 100) & (P_dry >= 60)] <- 110
    Classi_2[(Classi_1 == 100) & (Classi_2 != 110) & (P_dry >= (100 - (MAP/25)))] <- 120
    Classi_2[(Classi_1 == 100) & (Classi_2 != 110) & (P_dry < (100 - (MAP/25)))] <- 130
    Classi_2[(Classi_1 == 200) & (MAP < 5 * P_threshold)] <- 210
    Classi_2[(Classi_1 == 200) & (MAP >= 5 * P_threshold)] <- 220
    Classi_2[(Classi_1 == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 310
    Classi_2[(Classi_1 == 300) & (P_wdry < (P_swet/10))] <- 320
    Classi_2[(Classi_1 == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 310
    Classi_2[(Classi_1 == 300) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 320
    Classi_2[(Classi_1 == 300) & (Classi_2 != 310) & (Classi_2 != 320)] <- 330
    Classi_2[(Classi_1 == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3))] <- 410
    Classi_2[(Classi_1 == 400) & (P_wdry < (P_swet/10))] <- 420
    Classi_2[(Classi_1 == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter > P_summer)] <- 410
    Classi_2[(Classi_1 == 400) & (P_sdry < 40) & (P_sdry < (P_wwet/3)) & (P_wdry < (P_swet/10)) & (P_winter < P_summer)] <- 420
    Classi_2[(Classi_1 == 400) & (Classi_2 != 410) & (Classi_2 != 420)] <- 430
    Classi_2[(Classi_1 == 500) & (T_hot > 0)] <- 510
    Classi_2[(Classi_1 == 500) & (T_hot <= 0)] <- 520
  }
  
  ## Compute 3rd level of Koppen-Geiger climate classification
  if (level == 3) {
    Classi_3 <- Classi_2
    Classi_3[(Classi_2 == 210) & (MAT >= 18)] <- 211
    Classi_3[(Classi_2 == 210) & (MAT < 18)] <- 212
    Classi_3[(Classi_2 == 220) & (MAT >= 18)] <- 221
    Classi_3[(Classi_2 == 220) & (MAT < 18)] <- 222
    Classi_3[(Classi_2 == 310) & (T_hot >= 22)] <- 311
    Classi_3[(Classi_2 == 310) & (Classi_3 != 311) & (T_mon10 >= 4)] <- 312
    Classi_3[(Classi_2 == 310) & (Classi_3 != 311) & (Classi_3 != 312) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 313
    Classi_3[(Classi_2 == 320) & (T_hot >= 22)] <- 321
    Classi_3[(Classi_2 == 320) & (Classi_3 != 321) & (T_mon10 >= 4)] <- 322
    Classi_3[(Classi_2 == 320) & (Classi_3 != 321) & (Classi_3 != 322) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 323
    Classi_3[(Classi_2 == 330) & (T_hot >= 22)] <- 331
    Classi_3[(Classi_2 == 330) & (Classi_3 != 331) & (T_mon10 >= 4)] <- 332
    Classi_3[(Classi_2 == 330) & (Classi_3 != 331) & (Classi_3 != 332) & (T_mon10 < 4) & (T_mon10 >= 1)] <- 333
    Classi_3[(Classi_2 == 410) & (T_hot >= 22)] <- 411
    Classi_3[(Classi_2 == 410) & (Classi_3 != 411) & (T_mon10 >= 4)] <- 412
    Classi_3[(Classi_2 == 410) & (Classi_3 != 411) & (Classi_3 != 412) & (T_cold < -38)] <- 414
    Classi_3[(Classi_2 == 410) & (Classi_3 != 411) & (Classi_3 != 412) & (Classi_3 != 414)] <- 413
    Classi_3[(Classi_2 == 420) & (T_hot >= 22)] <- 421
    Classi_3[(Classi_2 == 420) & (Classi_3 != 421) & (T_mon10 >= 4)] <- 422
    Classi_3[(Classi_2 == 420) & (Classi_3 != 421) & (Classi_3 != 422) & (T_cold < -38)] <- 424
    Classi_3[(Classi_2 == 420) & (Classi_3 != 421) & (Classi_3 != 422) & (Classi_3 != 424)] <- 423
    Classi_3[(Classi_2 == 430) & (T_hot >= 22)] <- 431
    Classi_3[(Classi_2 == 430) & (Classi_3 != 431) & (T_mon10 >= 4)] <- 432
    Classi_3[(Classi_2 == 430) & (Classi_3 != 431) & (Classi_3 != 432) & (T_cold < -38)] <- 434
    Classi_3[(Classi_2 == 430) & (Classi_3 != 431) & (Classi_3 != 432) & (Classi_3 != 424)] <- 433
  }
  if (level == 1) {return(Classi_1)}
  if (level == 2) {return(Classi_2)}
  if (level == 3) {return(Classi_3)}
}




##### END OF SCRIPT #####