#---------------------------------------------------------------------------------------------------
# clean
#---------------------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())



#---------------------------------------------------------------------------------------------------
# set up
#---------------------------------------------------------------------------------------------------

# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, purrr, sf, sp, lwgeom, mapview)


# avoid scientific notation
options(scipen=999)



# path to github for shapefiles
folder_github = "https://github.com/bjornsh/gis_data/raw/main/"


data_input = paste0(getwd(), "/data_input")


#---------------------------------------------------------------------------------------------------
# ladda data
#---------------------------------------------------------------------------------------------------

### SCB tätorter
# nedladdning från: https://www.scb.se/vara-tjanster/oppna-data/oppna-geodata/tatorter/
# URL kan ändras! 

tatort_url = "https://www.scb.se/contentassets/3ee03ca6db1e48ff808b3c8d2c87d470/tatorter_1980_2020_2011-11-24.zip"

download.file(tatort_url, destfile = paste0(data_input, "/tatort.zip"))
unzip(paste0(data_input, "/tatort.zip"), exdir = data_input)

list.files(data_input)

st_layers(dsn = "data_input/Tatorter_1980_2020.gpkg")

tatort = sf::st_read("data_input/Tatorter_1980_2020.gpkg", 
                   layer = "To2020_SR99TM") #, driver = "FileGDB")

tatort = tatort %>% 
  filter(LAN == "03" & BEF >= 15000) %>% 
  dplyr::select(-X_koord, -Y_koord)

st_crs(tatort)


#### NVDB 
st_layers(dsn = "data_input/UppsalaNvdbGDB/UppsalaNvdbGDB_UPPSALA_LAN.gdb")


nvdb = sf::st_read("data_input/UppsalaNvdbGDB/UppsalaNvdbGDB_UPPSALA_LAN.gdb", 
                   layer = "UppsalaNvdbGDB_UPPSALA_LAN") #, driver = "FileGDB")

st_crs(nvdb)



#---------------------------------------------------------------------------------------------------
# Variabel levels
#---------------------------------------------------------------------------------------------------

# https://www.nvdb.se/globalassets/upload/publikationer-och-rapporter/100394_trafikverkets_dataprodukter_vag_och_jarnvagsdata_utg3_201501.pdf

##### Vägens slitlager (information om vägen är belagd eller om det är en grusväg)
table(nvdb$Slitl_152) # 1 = belagd, 2 = grus, NA finns
nrow(filter(nvdb, is.na(Slitl_152))) # NA = 67 rader


##### Väghållare: Den som ansvarar för drift och underhåll av det gällande väg-nätet.
table(nvdb$Vagha_6) # Väghållare: 1 = statlig, 2 = kommunal, 3 = enskild, NA finns
nrow(filter(nvdb, is.na(Vagha_6))) # NA = 1 rad


###### Funktionell vägklass: Vägens ”status”. Vägkategori indelas i europa-, riksvägar, primära, sekundära och tertiära länsvägar
# https://www.trafikverket.se/TrvSeFiler/Dataproduktspecifikationer/V%C3%A4gdataprodukter/DPS_E-G/1010funktionell_vagklass.pdf
table(df$Vagde_10379)
nrow(filter(nvdb, is.na(Vagde_10379))) # NA = 117 101 rader


#### Vägtrafiknät: Beskriver huvudsakligt användningssätt som nätkomponent är ämnad för.
table(df$Vagtr_474) # 1 = bilnät, 2 = cykelnät, 3 = färjenät, 4 = gångnät
nrow(filter(nvdb, is.na(Vagtr_474))) # NA = 1 rad


# Vägnummer: Vägnummer för allmänna vägar
summary(df$Vagnr_10370) # 
nrow(filter(nvdb, is.na(Vagnr_10370))) # NA = 117 092 rader

#### GCM-vägtyp
# https://www.trafikverket.se/TrvSeFiler/Dataproduktspecifikationer/V%C3%A4gdataprodukter/DPS_E-G/1054gcm-vagtyp.pdf
table(df$GCM_t_502)
# 1 Cykelbana, 2 Cykelfält, 3 Cykelöverfart i plan/cykelpassag, 
# 4 Övergångsställ, 5 Gatupassage utan utmärkning, 8 Koppling till annat nät,
# 9 Annan cykelbar förbindelse, 10 Annan ej cykelbar förbindelse, 11 Gångbana,
# 12 Trottoar, 13 Fortsättning i näte, 14 Passage genom byggnad, 15 Ramp, 
# 16 Perrong, 17 Trappa, 18 Rulltrappa, 19 Rullande trottoar, 20 Hiss, 21 Snedbanehiss, 
# 22 Linbana, 23 Bergbana, 24 Torg, 25 Kaj, 26 Öppen yta, 27 Färja, 28 Cykelpassage och övergångsställ


#### Vägbredd: Bredd_156

#### ÅDT
summary(nvdb$ADT_f_117)
nrow(filter(nvdb, !is.na(ADT_f_117)))

#### Hastighetsbegränsning i km/h
summary(nvdb$B_Hogst_225)
summary(nvdb$F_Hogst_225)

nrow(filter(nvdb, !is.na(B_Hogst_225)))
nrow(filter(nvdb, !is.na(F_Hogst_225)))

#---------------------------------------------------------------------------------------------------
# Kategorisa vägnätet
#---------------------------------------------------------------------------------------------------


# https://www.trafikverket.se/contentassets/85adaf43869a4bd19669851d286e367e/dataproduktspecifikationer/dps_transportnat_kartografi.pdf
# https://www.trafikverket.se/tjanster/data-kartor-och-geodatatjanster/leverera-in-data/data-om-anlaggningen--vag/dataproduktspecifikationer-vag--och-vagtrafikdata/

# Vagtr_474
# https://www.trafikverket.se/TrvSeFiler/Dataproduktspecifikationer/V%C3%A4gdataprodukter/DPS_U-%C3%96/1075vagtrafiknat.pdf


df = nvdb %>%
#  as.data.frame() %>%
  mutate(Hogst_225 = ifelse(B_Hogst_225 > F_Hogst_225, B_Hogst_225, F_Hogst_225)) %>% 
  mutate(steg1 = ifelse(Farjeled == "1",             # är en färjeled
                        "farjeled", NA)) %>% 
  mutate(steg1 = ifelse(is.na(steg1) & 
                          (Vagtr_474 == "2" |        # är del av cykelnät eller
                             Vagtr_474 == "4"),      # gångnät
                        "cykelvag", steg1)) %>%     
  mutate(steg1 = ifelse(is.na(steg1) & 
                          (Motorvag == "1" |         # är en Motorvag eller
                             Motortrafikled == "1"), # Motortrafikled
                        "cykelforbud", steg1)) %>%      
  mutate(steg1 = ifelse(is.na(steg1) & 
                          is.na(Vagde_10379) &       # saknar "Funktionell vägklass"
                          is.na(Vagnr_10370) &       # saknar "Vägnummer"
                          Vagha_6 == "3" &           # Väghållare = enskild
                          Slitl_152 == "2",          # slitlager = grus
                        "oklassificerad_grusvag", steg1)) %>%
  mutate(steg1 = ifelse(is.na(steg1) & 
                          Vagtr_474 == "1" &        # Vägtrafiknät = bilnät
                          is.na(Hogst_225),         # saknar "hastighetsbegränsning"
                        "allman_vag_utan_hastighet", steg1)) %>%
  mutate(steg1 = ifelse(is.na(steg1) & 
                          !is.na(ADT_f_117),        # har ÅDT data
                        "har_adt", steg1))

table(df$steg1)
nrow(filter(df, is.na(steg1))) # FME: 35 141



df %>% 
  as.data.frame() %>% 
  filter(steg1 == "cykelvag") %>% 
  group_by(Vagtr_474, GCM_t_502, Slitl_152) %>% 
  summarise(n = n()) %>% 
  print(., n = Inf)



df1 = df %>% 
  #### Cykelväg
  mutate(vag_kat = ifelse(steg1 == "cykelvag" & 
                            Vagtr_474 == "4",     # del av gångnät
                          "gangvag", NA)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & steg1 == "cykelvag" &
                            GCM_t_502 == "2",     # Cykelfält
                          "c3", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & steg1 == "cykelvag" & 
                            Slitl_152 == "2",     # grus
                          "c2", vag_kat)) %>%
  mutate(vag_kat = ifelse(is.na(vag_kat) & steg1 == "cykelvag", 
                          "c1", vag_kat)) %>%     # Cykelväg eftersom inte gångväg, inte cykelfät och inte grus

  ### motorväg
  mutate(vag_kat = ifelse(is.na(vag_kat) & 
                            (Motortrafikled == "1" | 
                               Motorvag == "1"), 
                          "b5", vag_kat)) %>%
  
  #### bilväg b2 - b4 med ÅDT 
  mutate(vag_kat = ifelse(is.na(vag_kat) & !is.na(ADT_f_117) &
                        (Hogst_225 > 80 |
                           (Hogst_225 > 70 & ADT_f_117 >= 1000) |
                           (Hogst_225 > 60 & ADT_f_117 >= 2000) |
                           (Hogst_225 > 40 & ADT_f_117 >= 2500)), 
                        "b4", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & !is.na(ADT_f_117) &
                        (Hogst_225 > 70 |
                           (Hogst_225 > 60 & ADT_f_117 >= 1000) |
                           (Hogst_225 > 40 & ADT_f_117 >= 2000) |
                           (Hogst_225 > 30 & ADT_f_117 >= 2500)), 
                        "b3", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & !is.na(ADT_f_117), "b2", vag_kat)) %>% 
  
  #### bilväg b2 - b4 utan ÅDT
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 (Hogst_225 >= 90 | 
                                    Klass_181 < 4), 
                          "b4", vag_kat)) %>%
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 Hogst_225 <= 30, 
                          "b1", vag_kat)) %>%
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 (Hogst_225 > 40 | 
                                    Klass_181 == "4"), 
                          "b4", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 (Hogst_225 <= 40 | 
                                    Klass_181 == "4"), 
                          "b3", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 (Klass_181 == "5" | 
                                    Klass_181 == "6"), 
                          "spatial_filter", vag_kat)) %>% 
  mutate(vag_kat = ifelse(is.na(vag_kat) & is.na(ADT_f_117) &
                                 Klass_181 >= 7, 
                          "b2", vag_kat)) %>% 
  
  #### b1
  mutate(vag_kat = ifelse(vag_kat == "b2" & !is.na(ADT_f_117) & 
                            (ADT_f_117 <= 250 | 
                               Bredd_156 <= 3.5), 
                          "b1", vag_kat)) %>% 
  mutate(vag_kat = ifelse(vag_kat == "b2" & !is.na(ADT_f_117) & 
                            Hogst_225 <= 30 & 
                            Klass_181 >= 7, 
                          "b1", vag_kat)) %>% 
  
  #### grusväg
  mutate(vag_kat = ifelse((vag_kat == "b3" | 
                             vag_kat == "b4" | 
                             vag_kat == "b5") & 
                            Slitl_152 == "2", 
                          "g2", vag_kat)) %>%
  mutate(vag_kat = ifelse(vag_kat == "b1" & 
                            Slitl_152 == "2", 
                          "g1", vag_kat))



table(df1$vag_kat)

### karta
mapview(df1, zcol = "vag_kat")





##### identifiera vägar inom och utanför tätorter ######

line_inom_tatort = df1 %>% 
  st_zm() %>% 
  st_intersection(., tatort)

mapview(line_inom_tatort, zcol = "vag_kat")






         
         
         
         