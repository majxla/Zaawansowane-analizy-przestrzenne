
# WCZYTANIE DANYCH

library(sf)
library(dplyr)
library(spData)

gminy_06 = read_sf("gminy/gminy_lubelskie.gpkg")
gminy_18 = read_sf("gminy/gminy_lubelskie.gpkg")


gestosc_06 = read.csv("gestosc_zaludnienia_2006/gestosc_zaludnienia_2006_gminy.csv", encoding = "UTF-8", sep=";")
gestosc_18 = read.csv("gestosc_zaludnienia_2018/gestosc_zaludnienia_2018_gminy.csv", encoding = "UTF-8", sep=";")

# setdiff(gestosc_18$Kod, gestosc_06$Kod)

clc_lub_06 = read_sf("clc2006/clc_06_lubelskie.gpkg")
clc_lub_18 = read_sf("clc2018/clc_18_lubelskie.gpkg")



# USUNIĘCIE NIEPOTRZEBNYCH KOLUMN w gminach i CLC
gminy_06_ = gminy_06[ , c(4:5, 34)]
gminy_18_ = gminy_18[ , c(4:5, 34)]

clc_lub_06_ = clc_lub_06[, c(1, 8:9)]
clc_lub_18_ = clc_lub_18[, c(2, 5:6)]

colnames(clc_lub_18_)[1] = "AREA" # zmiana nazwy kolumny zawierającej powierzchnię żeby była taka sama jak w clc_lub_06_



# przeliczenie powierzchni dla gmin i clc zeby byly w tych samych jednostkach

gminy_06_ = mutate(gminy_06_, SHAPE_AREA = st_area(gminy_06_)) 
gminy_18_ = mutate(gminy_18_, SHAPE_AREA = st_area(gminy_18_)) 

clc_lub_06_= mutate(clc_lub_06_, AREA = st_area(clc_lub_06_))
clc_lub_18_= mutate(clc_lub_18_, AREA = st_area(clc_lub_18_))




# AGREGACJA I OBLICZENIE POWIERZCHNI POSZCZEGÓLNYCH KODÓW W GMINACH I PRZYŁĄCZENIE DO GMIN (PO KODZIE GMINY)

#interscetion do gmin

clc_06_gminy_2 = st_intersection(clc_lub_06_, gminy_06_) 
clc_06_gminy_2 = mutate(clc_06_gminy_2, AREA = st_area(clc_06_gminy_2)) 
clc_06_agg_2 = clc_06_gminy_2 %>% group_by(JPT_KOD_JE, code) %>% summarize(Total_Area = sum(AREA))

clc_18_gminy_2 = st_intersection(clc_lub_18_, gminy_18_) 
clc_18_gminy_2 = mutate(clc_18_gminy_2, AREA = st_area(clc_18_gminy_2)) 
clc_18_agg_2 = clc_18_gminy_2 %>% group_by(JPT_KOD_JE, code) %>% summarize(Total_Area = sum(AREA))




# OBLICZENIE UDZIAŁÓW ZAGODPODAROWANIA TERENU NA PODSTAWIE CLC06 I CLC18

gminy_06_

codes = c(sort(unique(clc_06_agg_2$code))) # wektor zawierający wszystkie kody które znajdują się na badanym terenie
codes18 = c(sort(unique(clc_18_agg_2$code)))

#kopia
gminy_06_5 = gminy_06_
gminy_18_5 = gminy_18_




# pętla na dodanie nowych kolumn które będą przechowywać te udziały, wypełnione wartością 0

for (name in as.character(codes)) {
  gminy_06_5[[name]] = rep(0, nrow(gminy_06_5))
}

for (name in as.character(codes18)) {
  gminy_18_5[[name]] = rep(0, nrow(gminy_18_5))
}



# kopia
clc_06_agg_c = clc_06_agg_2
clc_06_agg_c = st_drop_geometry(clc_06_agg_c) # zrzucenie geometrii bo na mnie krzyczy coś dalej i i tak nie jest tu potrzebna

clc_18_agg_c = clc_18_agg_2
clc_18_agg_c = st_drop_geometry(clc_18_agg_c)


# kod na uzupełnienie danych procentowych 

for (id_gminy in gminy_06_5$JPT_KOD_JE) {
  # print(id_gminy)
  view_temp = filter(clc_06_agg_2, JPT_KOD_JE == id_gminy)
  
  for (row in 1:nrow(view_temp)) {
    row_temp = view_temp[row, ]
    # print(row_temp$code)
    
    if (row_temp$code %in% codes) {
      gm_num = which(gminy_06_5$JPT_KOD_JE == id_gminy)
      shape_area = as.double(gminy_06_5[gm_num, "SHAPE_AREA"])
      #print(shape_area[1])
      gminy_06_5[gm_num, as.character(row_temp$code)] = as.double(row_temp$Total_Area) / shape_area[1] * 100
    }
  }
}

# filter(gminy_06_5, JPT_NAZWA_ == "Izbica")

for (id_gminy in gminy_18_5$JPT_KOD_JE) {
  # print(id_gminy)
  view_temp = filter(clc_18_agg_2, JPT_KOD_JE == id_gminy)
  
  for (row in 1:nrow(view_temp)) {
    row_temp = view_temp[row, ]
    # print(row_temp$code)
    
    if (row_temp$code %in% codes18) {
      gm_num = which(gminy_18_5$JPT_KOD_JE == id_gminy)
      shape_area = as.double(gminy_18_5[gm_num, "SHAPE_AREA"])
      #print(shape_area[1])
      gminy_18_5[gm_num, as.character(row_temp$code)] = as.double(row_temp$Total_Area) / shape_area[1] * 100
    }
  }
}





# zmiana nazw kolumn
columns_codes = paste("per_code", codes, sep="_") # wektor zawierający nazwy nowych kolumn, w których będą przechowywane udziały clc
columns_codes = c(columns_codes)

colnames(gminy_06_5) = c("JPT_KOD_JE", "JPT_NAZWA_", "SHAPE_AREA", "geom", columns_codes)
colnames(gminy_18_5) = c("JPT_KOD_JE", "JPT_NAZWA_", "SHAPE_AREA", "geom", columns_codes)



# PRZYŁĄCZENIE DO GMIN INFORMACJI O ZAGĘSZCZENIU LUDNOŚCI 

#kopie
gminy_06_c = gminy_06_5
gestosc_06_c = gestosc_06
gestosc_06_c = gestosc_06_c[ , c(1, 5) ]


gminy_18_c = gminy_18_5
gestosc_18_c = gestosc_18
gestosc_18_c = gestosc_18_c[ , c(1, 5) ]




# trzeba zmienić typ kolumny kod w ludnosci zeby sie zgadzala z typem kolumny w gminach bo inaczej krzyczy 
gestosc_06_c$Kod = as.character(gestosc_06_c$Kod)
gestosc_18_c$Kod = as.character(gestosc_18_c$Kod)



# i trzeba dodać 0 na początek wartości w tej kolumnie bo durny excel usuwa 0 z początku
gestosc_06_c$JPT_KOD_JE = paste0("0", gestosc_06_c$Kod)
gestosc_18_c$JPT_KOD_JE = paste0("0", gestosc_18_c$Kod)


gminy_06_lud = left_join(gminy_06_c, gestosc_06_c[ , 2:3], by = "JPT_KOD_JE")
colnames(gminy_06_lud)[18] = "os_km2"

gminy_18_lud = left_join(gminy_18_c, gestosc_18_c[ , 2:3], by = "JPT_KOD_JE")
colnames(gminy_18_lud)[18] = "os_km2"


gminy_06_lud$geom = st_centroid(gminy_06_lud$geom)
gminy_18_lud$geom = st_centroid(gminy_18_lud$geom)


# ------------------------------------------- 


# UCZENIE MASZYNOWE
library(mlr3)
library(mlr3learners)
library(mlr3spatiotempcv)



gminy_06_lud[is.na(gminy_18_lud$os_km2), ]
# filter(gminy_06_lud, JPT_KOD_JE == "0602011")
nr = which(gminy_06_lud$JPT_KOD_JE == "0604011")

gmin06 = st_centroid(gminy_06_lud)
gmin06$SHAPE_AREA = as.numeric(gmin06$SHAPE_AREA)
gmin06 = na.omit(gmin06)
gminy_bez_kod06 = gmin06[ , -c(1, 2)]

gmin18 = st_centroid(gminy_18_lud)
gmin18$SHAPE_AREA = as.numeric(gmin18$SHAPE_AREA)
gmin18 = na.omit(gmin18)
gminy_bez_kod18 = gmin18[ , -c(1, 2)]




task = as_task_regr_st(
  gminy_bez_kod06, # dane
  target = "os_km2", # kolumna którą chcemy przewidzieć
)

learner = mlr3::lrn("regr.lm")
resampling = mlr3::rsmp("repeated_spcv_coords", folds = 5, repeats = 100) 


#uczenie

gestosc_lud_learn = mlr3::resample(task = task,
                                   learner = learner,
                                   resampling = resampling)


miara1 = mlr3::mlr_measures$get("regr.rmse") 
miara2 = mlr3::mlr_measures$get("regr.rsq") # r2
score_rr = gestosc_lud_learn$score(measures = c(miara1, miara2))

median(score_rr$regr.rmse)
median(score_rr$regr.rsq, na.rm = TRUE)

learner$train(task)
model = learner$model



# predyckja - nie ruszone, wymagane 2018 - predictors
library(terra)

pred_cv = terra::predict(model, gminy_bez_kod18)

pred_cv[57]
gminy_bez_kod18[57, 16]
gminy_bez_kod06[57, ]

filter(gminy_06_)

# plot(pred_cv)


# CSV PORÓWNANIE

gminy_bez_kod18['os_km2']
gminy_bez_kod06['os_km2']
gmin06

gminy06_g = gmin06[ , c(1, 2, 18)]
gminy18_g = gmin18[ , c(1, 2, 18)]


gminy_06_18 =  st_join(gminy06_g, gminy18_g, by = "JPT_KOD_JE")
gminy_06_18 = gminy_06_18[, c(1:3, 7)] 
colnames(gminy_06_18) = c("JPT_KOD_JE", "JPT_NAZWA_", "os_km2_06", "os_km2_18", "geom")


gminy_06_18_c = gminy_06_18
gminy_06_18_c = st_drop_geometry(gminy_06_18_c)


pred_cv

gminy_por = cbind(gminy_06_18_c, pred_cv)
typeof(gminy_por)

write.csv(gminy_por, "porownanie_lud.csv", row.names = FALSE, fileEncoding = "UTF-8")

gmin06_c = gmin06
gmin18_c = gmin18

gmin06_c = st_drop_geometry(gmin06_c)
gmin18_c = st_drop_geometry(gmin18_c)

write.csv(gmin06_c, "calosc_2006.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(gmin18_c, "calosc_2018.csv", row.names = FALSE, fileEncoding = "UTF-8")
