# SIMPLIFY GEOMETRIES AND CREATE RDS FILES
## This script allows to simplify .shp files and create RDS files
## Resulting RDS files should be stored in inst>geom folder for A2SIT to read them
## Original shapefiles need to be stored in local device to avoid GitHub file size limits
## In the simplification chunk, it is necessary to play around with the "keep" value to make sure not to lose variables

# load libraries
library(sf)
library(rmapshaper)
library(ggplot2)
library(magrittr)
library(dplyr)

# _______________________________________________________________________________________


# COSTA RICA ----
# read shapefile
CRI <- sf::st_read("C:/NoSync/R/A2SIT_Maps/CRI/Cantones_CR.shp")

# simplify geometries keeping shared borders
CRI_simplified <- ms_simplify(CRI, keep = 0.001,
                              keep_shapes = FALSE)

# remove duplicate values in CRI_simplified
CRI_simplified <- distinct(CRI_simplified)

# Check duplicates in Admn2_code
CRI_duplicate_adm2_source_code <- CRI_simplified$adm2_source_code[duplicated(CRI_simplified$adm2_source_code)]
print(CRI_duplicate_adm2_source_code)

# change column names
names(CRI_simplified)[names(CRI_simplified) == "cod_canton"] <- "adm2_source_code"
names(CRI_simplified)[names(CRI_simplified) == "canton"] <- "gis_name"

# add CRI to the admin2_code value
CRI_simplified$adm2_source_code <- paste("CRI", CRI_simplified$adm2_source_code, sep = "")

# remove all columns except adm2_source_code, gis_name and geometry
CRI_simplified2 <- CRI_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = CRI_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Cantones de Costa Rica") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(CRI),
        object.size(CRI_simplified2)) / 1024)

# save as RDS file
saveRDS(CRI_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/CRI.rds")


# _______________________________________________________________________________________


# PERÚ ----
# read shapefile
PER <- sf::st_read("C:/NoSync/R/A2SIT_Maps/PER/INEI/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")

# simplify geometries keeping shared borders
PER_simplified <- ms_simplify(PER, keep = 0.002,
                              keep_shapes = FALSE)

# change column names
names(PER_simplified)[names(PER_simplified) == "NOMBPROV"] <- "gis_name"
names(PER_simplified)[names(PER_simplified) == "IDPROV"] <- "adm2_source_code"

# remove all columns except adm2_source_code, gis_name and geometry
PER_simplified2 <- PER_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# add PER to the admin2_code value
PER_simplified2$adm2_source_code <- paste("PER", PER_simplified2$adm2_source_code, sep = "")

# Check duplicates in Admn2_code
PER_duplicate_adm2_source_code <- PER_simplified2$adm2_source_code[duplicated(PER_simplified2$adm2_source_code)]
print(PER_duplicate_adm2_source_code)

# Plot simplified version
ggplot() +
  geom_sf(data = PER_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Provincias de Peru") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(PER),
        object.size(PER_simplified2)) / 1024)

# save as RDS file
saveRDS(PER_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/PER.rds")


# _______________________________________________________________________________________


# BRAZIL (CORRECTION NEEDED - no adm2 codes in the .shp)----
# read shapefile
BRA <- sf::st_read("C:/NoSync/R/A2SIT_Maps/BRA/geoBoundaries-BRA-ADM2_simplified.shp")

# simplify geometries keeping shared borders
BRA_simplified <- ms_simplify(BRA, keep = 0.39,
                              keep_shapes = FALSE)

# Check duplicates in Admn2_code
BRA_duplicate_adm2_source_code <- PER_simplified$adm2_source_code[duplicated(PER_simplified$adm2_source_code)]
print(PER_duplicate_adm2_source_code)

# Plot simplified version
ggplot() +
  geom_sf(data = BRA_simplified, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de Brazil") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(BRA),
        object.size(BRA_simplified)) / 1024)

# save as RDS file
saveRDS(BRA_simplified, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/BRA.rds")


# _______________________________________________________________________________________


# ECUADOR ----
# read shapefile
ECU <- sf::st_read("C:/NoSync/R/A2SIT_Maps/ECU/ecu_admbnda_adm2_inec_20190724.shp")

# simplify geometries keeping shared borders
ECU_simplified <- ms_simplify(ECU, keep = 0.005,
                              keep_shapes = FALSE)

# change column names
names(ECU_simplified)[names(ECU_simplified) == "ADM2_PCODE"] <- "Admin2_source_name"
names(ECU_simplified)[names(ECU_simplified) == "ADM2_ES"] <- "gis_name"

# Check duplicates in Admn2_code
ECU_duplicate_adm2_source_code <- ECU_simplified$adm2_source_code[duplicated(ECU_simplified$adm2_source_code)]
print(ECU_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
ECU_simplified2 <- PER_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = ECU_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Cantones de Ecuador") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(ECU),
        object.size(ECU_simplified2)) / 1024)

# save as RDS file
saveRDS(ECU_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/ECU.rds")


# _______________________________________________________________________________________


# HONDURAS ----
# read shapefile
HND <- sf::st_read("C:/NoSync/R/A2SIT_Maps/HND/SHP_HND/HN_Municipios.shp")

# simplify geometries keeping shared borders
HND_simplified <- ms_simplify(HND, keep = 0.009,
                              keep_shapes = FALSE)

# change column names
names(HND_simplified)[names(HND_simplified) == "nom_muni"] <- "gis_name"
names(HND_simplified)[names(HND_simplified) == "cod_muni"] <- "adm2_source_code"

# Check duplicates in Admn2_code
HND_duplicate_adm2_source_code <- HND_simplified$adm2_source_code[duplicated(HND_simplified$adm2_source_code)]
print(HND_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
HND_simplified2 <- HND_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = HND_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de HNDduras") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(HND),
        object.size(HND_simplified2)) / 1024)

# save as RDS file
saveRDS(HND_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/HND.rds")


# _______________________________________________________________________________________


# EL SALVADOR ----
# read shapefile
SLV <- sf::st_read("C:/NoSync/R/A2SIT_Maps/SLV/LÍMITES MUNICIPALES.shp")

# simplify geometries keeping shared borders
SLV_simplified <- ms_simplify(SLV, keep = 0.008,
                              keep_shapes = FALSE)

# add SLV to the NA3 value
SLV_simplified$NA3 <- paste("SLV", SLV_simplified$NA3, sep = "")

# change column names
names(SLV_simplified)[names(SLV_simplified) == "NAM"] <- "gis_name"
names(SLV_simplified)[names(SLV_simplified) == "NA3"] <- "adm2_source_code"

# Check duplicates in Admn2_code
SLV_duplicate_adm2_source_code <- SLV_simplified$adm2_source_code[duplicated(SLV_simplified$adm2_source_code)]
print(SLV_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
SLV_simplified2 <- SLV_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = SLV_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de El Salvador") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(SLV),
        object.size(SLV_simplified2)) / 1024)

# save as RDS file
saveRDS(SLV_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/SLV.rds")


# _______________________________________________________________________________________


# VENEZUELA ----
# read shapefile
VEN <- sf::st_read("C:/NoSync/R/A2SIT_Maps/VEN/VEN_admin2-1.1.shp")

# simplify geometries keeping shared borders
VEN_simplified <- ms_simplify(VEN, keep = 0.04,
                              keep_shapes = FALSE)

# change column names
names(VEN_simplified)[names(VEN_simplified) == "NAME_2"] <- "gis_name"
names(VEN_simplified)[names(VEN_simplified) == "ID_2"] <- "adm2_source_code"

# Check duplicates in Admn2_code
VEN_duplicate_adm2_source_code <- VEN_simplified$adm2_source_code[duplicated(VEN_simplified$adm2_source_code)]
print(VEN_duplicate_adm2_source_code)

# add VEN to the adm2_source_code value
VEN_simplified$adm2_source_code <- paste("VEN", VEN_simplified$adm2_source_code, sep = "")

# remove all columns except adm2_source_code, gis_name and geometry
VEN_simplified2 <- VEN_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = VEN_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de Venezuela") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(VEN),
        object.size(VEN_simplified2)) / 1024)

# save as RDS file
saveRDS(VEN_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/VEN.rds")


# _______________________________________________________________________________________


# PANAMA (REQUIERE REVISIÓN. No tiene identificadores únicos) ----

# read shapefile
PAN <- sf::st_read("C:/NoSync/R/A2SIT_Maps/PAN/PAN_adm2.shp")

# simplify geometries keeping shared borders
PAN_simplified <- ms_simplify(PAN, keep = 0.015,
                              keep_shapes = FALSE)

# change column names
names(PAN_simplified)[names(PAN_simplified) == "ID_2"] <- "adm2_source_code"
names(PAN_simplified)[names(PAN_simplified) == "NAME_2"] <- "gis_name"

# Check duplicates in Admn2_code
PAN_duplicate_adm2_source_code <- PAN_simplified$adm2_source_code[duplicated(PAN_simplified$adm2_source_code)]
print(PAN_duplicate_adm2_source_code)

# add VEN to the adm2_source_code value
PAN_simplified$adm2_source_code <- paste("PAN", PAN_simplified$adm2_source_code, sep = "")

# remove all columns except adm2_source_code, gis_name and geometry
PAN_simplified2 <- PAN_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = PAN_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Distritos de Panama") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(PAN),
        object.size(PAN_simplified2)) / 1024)

# save as RDS file
saveRDS(PAN_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/PAN.rds")


# _______________________________________________________________________________________


# UNITED STATES ----
# read shapefile
USA <- sf::st_read("C:/NoSync/R/A2SIT_Maps/USA/cb_2018_us_county_500k.shp")

# simplify geometries keeping shared borders
USA_simplified <- ms_simplify(USA, keep = 0.7,
                              keep_shapes = FALSE)

# change column names
names(USA_simplified)[names(USA_simplified) == "NAME"] <- "gis_name"
names(USA_simplified)[names(USA_simplified) == "GEOID"] <- "adm2_source_code"

# Check duplicates in Admin2_code
USA_duplicate_adm2_source_code <- USA_simplified$adm2_source_code[duplicated(USA_simplified$Admin2_source_ode)]
print(USA_duplicate_adm2_source_code)

# add USA to the admin2_code value
USA_simplified$adm2_source_code <- paste("USA", USA_simplified$adm2_source_code, sep = "")

# remove all columns except adm2_source_code, gis_name and geometry
USA_simplified2 <- USA_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = USA_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Condados de Estados Unidos") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(USA),
        object.size(USA_simplified2)) / 1024)

# save as RDS file
saveRDS(USA_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/USA.rds")


# _______________________________________________________________________________________


# MEXICO ----
# read shapefile
MEX <- sf::st_read("C:/NoSync/R/A2SIT_Maps/MEX/mex_admbnda_adm2_govmex_20210618.shp")

# simplify geometries keeping shared borders
MEX_simplified <- ms_simplify(MEX, keep = 0.03,
                              keep_shapes = FALSE)

# change column names
names(MEX_simplified)[names(MEX_simplified) == "ADM2_ES"] <- "gis_name"
names(MEX_simplified)[names(MEX_simplified) == "ADM2_PCODE"] <- "adm2_source_code"

# Check duplicates in Admin2_code
MEX_duplicate_adm2_source_code <- MEX_simplified$adm2_source_code[duplicated(MEX_simplified$Admin2_source_ode)]
print(MEX_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
MEX_simplified2 <- MEX_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = MEX_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de México") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(MEX),
        object.size(MEX_simplified2)) / 1024)

# save as RDS file
saveRDS(MEX_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/MEX.rds")


# _______________________________________________________________________________________


# CHILE ----
# read shapefile
CHL <- sf::st_read("C:/NoSync/R/A2SIT_Maps/CHL/chl_admbnda_adm2_bcn_20211008.shp")

# simplify geometries keeping shared borders
CHL_simplified <- ms_simplify(CHL, keep = 0.005,
                              keep_shapes = FALSE)

# change column names
names(CHL_simplified)[names(CHL_simplified) == "ADM2_ES"] <- "gis_name"
names(CHL_simplified)[names(CHL_simplified) == "ADM2_PCODE"] <- "adm2_source_code"

# Check duplicates in Admin2_code
CHL_duplicate_adm2_source_code <- CHL_simplified$adm2_source_code[duplicated(CHL_simplified$Admin2_source_ode)]
print(CHL_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
CHL_simplified2 <- CHL_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = CHL_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de Chile") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(CHL),
        object.size(CHL_simplified2)) / 1024)

# save as RDS file
saveRDS(CHL_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/CHL.rds")



# _______________________________________________________________________________________


# ARGENTINA ----
# read shapefile
ARG <- sf::st_read("C:/NoSync/R/A2SIT_Maps/ARG/arg_admbnda_adm2_unhcr2017.shp")

# simplify geometries keeping shared borders
ARG_simplified <- ms_simplify(ARG, keep = 0.005,
                              keep_shapes = FALSE)

# change column names
names(ARG_simplified)[names(ARG_simplified) == "ADM2_ES"] <- "gis_name"
names(ARG_simplified)[names(ARG_simplified) == "ADM2_PCODE"] <- "adm2_source_code"

# Check duplicates in Admin2_code
ARG_duplicate_adm2_source_code <- ARG_simplified$adm2_source_code[duplicated(ARG_simplified$Admin2_source_ode)]
print(ARG_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
ARG_simplified2 <- ARG_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = ARG_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Departamentos de Argentina") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(ARG),
        object.size(ARG_simplified2)) / 1024)

# save as RDS file
saveRDS(ARG_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/ARG.rds")


# _______________________________________________________________________________________


# COLOMBIA ----
# read shapefile
COL <- sf::st_read("C:/NoSync/R/A2SIT_Maps/COL/col_admbnda_adm2_mgn_20200416.shp")

# simplify geometries keeping shared borders
COL_simplified <- ms_simplify(COL, keep = 0.004,
                              keep_shapes = FALSE)

# change column names
names(COL_simplified)[names(COL_simplified) == "ADM2_ES"] <- "gis_name"
names(COL_simplified)[names(COL_simplified) == "ADM2_PCODE"] <- "adm2_source_code"

# Check duplicates in Admin2_code
COL_duplicate_adm2_source_code <- COL_simplified$adm2_source_code[duplicated(COL_simplified$Admin2_source_ode)]
print(COL_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
COL_simplified2 <- COL_simplified[c('adm2_source_code', 'gis_name', 'geometry')]


# Plot simplified version
ggplot() +
  geom_sf(data = COL_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de Colombia") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(COL),
        object.size(COL_simplified2)) / 1024)

# save as RDS file
saveRDS(COL_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/COL.rds")


# _______________________________________________________________________________________


# URUGUAY ----
# read shapefile
URY <- sf::st_read("C:/NoSync/R/A2SIT_Maps/URY/ury_admbnda_adm2_2020.shp")

# simplify geometries keeping shared borders
URY_simplified <- ms_simplify(URY, keep = 0.29,
                              keep_shapes = FALSE)

# change column names
names(URY_simplified)[names(URY_simplified) == "ADM2_ES"] <- "gis_name"
names(URY_simplified)[names(URY_simplified) == "ADM2_PCODE"] <- "adm2_source_code"

# Check duplicates in Admin2_code
URY_duplicate_adm2_source_code <- URY_simplified$adm2_source_code[duplicated(URY_simplified$Admin2_source_ode)]
print(URY_duplicate_adm2_source_code)

# remove all columns except adm2_source_code, gis_name and geometry
URY_simplified2 <- URY_simplified[c('adm2_source_code', 'gis_name', 'geometry')]

# Plot simplified version
ggplot() +
  geom_sf(data = URY_simplified2, size = 1.5, color = "black", fill = NA) +
  ggtitle("Municipios de Uruguay") +
  coord_sf()

# Compare original vs simplified object size
round(c(object.size(URY),
        object.size(URY_simplified2)) / 1024)

# save as RDS file
saveRDS(URY_simplified2, file = "C:/NoSync/A2SIT repo/A2SIT/inst/geom/URY.rds")


