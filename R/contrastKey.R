##### functions for finding contrasts #####
library(dplyr)
load("data/scaleContrast.rda")

Lkey <- list(e_L0Mp1 = 0,
             e_Lp1Mm1 = 1,
             e_Lp1M0 = 1,
             e_Lp1Mm2 = 1,
             e_Lp1Mp1 = 1,
             e_Lp1Mp2 = 1,
             e_Lp2Mp1 = 2,
             e_Lp2Mm1 = 2)

Lkey2 <- 
  data.frame(type = names(Lkey), C = unlist(Lkey)) %>%
  left_join(scaleContrast) %>%
  mutate(Cnew = C * 100 / C100)
Lkey3 <- as.list(Lkey2$Cnew)
names(Lkey3) <- Lkey2$type

Lkey <- Lkey3



Mkey <- list(e_L0Mp1 = 1,
             e_Lp1Mm1 = -1,
             e_Lp1M0 = 0,
             e_Lp1Mm2 = -2,
             e_Lp1Mp1 = 1,
             e_Lp1Mp2 = 2,
             e_Lp2Mp1 = 1,
             e_Lp2Mm1 = -1)


Mkey2 <- 
  data.frame(type = names(Mkey), C = unlist(Mkey)) %>%
  left_join(scaleContrast) %>%
  mutate(Cnew = C * 100 / C100)
Mkey3 <- as.list(Mkey2$Cnew)
names(Mkey3) <- Mkey2$type

Mkey <- Mkey3


getM <- function(type_string) { return(unlist(Mkey[type_string])) } 
getM <- Vectorize(getM)
getL <- function(type_string) { return(unlist(Lkey[type_string])) } 
getL <- Vectorize(getL)