
#****************************************************
# 2. ASIGNAMOS LA CARPETA DE TRABAJO
#****************************************************
setwd("~/5_MINCUL/Data_ENL")
#****************************************************
# 3. UNIMOS LA DATA DE CAPÍTULO 200, 300 Y 400
#****************************************************
#* Unimos la base de datos 200, 300 y 400
{
  ENL100_600_P501_Hogar <- read_sav("DATA_ENL2022_FINAL_05_04_2023/V_ENL2022_100_600_P501.sav") %>%
    unite(IDHOG, c("CONG", "NSELV", "HOGAR"), sep = "", remove = F) %>%
    unite(IDVIV, c("CONG", "NSELV"), sep = "", remove = F) %>%
    mutate(Miembro = "1") %>% #Asignamos el 1 de jefe de hogar, que representa al conjunto de habitantes
    mutate(seccion1 = "Hogar") %>%
    unite(IDHUMAN, c("CONG", "NSELV", "HOGAR", "Miembro"), sep = "", remove = TRUE) %>%
    select(-TOT_HOGAR, -CCDD, -NOMBREDD, -CCPP, -NOMBREPP, -CCDI, -NOMBREDI, -CCCP, -NOMBRECP, -UBIGEO, -VIVIENDA, -PER, -RESFIN, -RESFIN_O,
           -INF_500, -DOMINIO, -ESTRATO, -ESTRATOSOCIO, -FACTORFINAL_HOGAR)
  # Quitamos los P501 porque en la base ENL500_Per0a17 se responde a nivel de personas, en esta sección se agrupa por hogar (toda persona del hogar responde lo mismo)
  # Quitaron: ID, AREA, ODEI, NOM_ODEI
  ENL200_Persontotal <- read_sav("DATA_ENL2022_FINAL_05_04_2023/V_ENL2022_200.sav") %>%
    mutate(seccion2 = "Persontotal") %>%
    unite(IDHUMAN, c("CONG", "NSELV", "HOGAR", "P201"), sep = "", remove = FALSE) %>%
    select(-RESFIN_V)
  ENL300_400_Per18a64 <- read_sav("DATA_ENL2022_FINAL_05_04_2023/V_ENL2022_300_400.sav") %>%
    mutate(seccion3 = "Person18a64") %>%
    unite(IDHUMAN, c("CONG", "NSELV", "HOGAR", "P201"), sep = "", remove = TRUE) %>%
    select(-TOT_HOGAR, -CCDD, -NOMBREDD, -CCPP, -NOMBREPP, -CCDI, -NOMBREDI, -CCCP, -NOMBRECP, -UBIGEO, -VIVIENDA, -PER, -RES_PER, -PERS_NRO, -RESFIN, -RESFIN_O, -RESFIN_V,-RESFIN_V_O,
           -TIPO_ENT, -TIPO_ENT_O,
           -DOMINIO, -ESTRATO, -ESTRATOSOCIO, -FACTOR200_FINAL, -FACTORFINAL_HOGAR, -P203, -P206, -P207, -P208, -P209, -P210_A, -P210_M)
  # Quitaron: ID, CCDD, -NOMBREDD, -CCPP, -NOMBREPP, -CCDI, -NOMBREDI, -UBIGEO, -AREA, PER, -ODEI, -NOM_ODEI
  ENL500_Per0a17 <- read_sav("DATA_ENL2022_FINAL_05_04_2023/V_ENL2022_500.sav") %>%
    mutate(seccion4 = "Person0a17") %>%
    unite(IDHUMAN, c("CONG", "NSELV", "HOGAR", "P201"), sep = "", remove = TRUE) %>%
    select(-TOT_HOGAR, -CCDD, -NOMBREDD, -CCPP, -NOMBREPP, -CCDI, -NOMBREDI, -CCCP, -NOMBRECP, -UBIGEO, -VIVIENDA, -PER, -PERS_NRO, -RESFIN, -RESFIN_O, -RESFIN_V,-RESFIN_V_O,
           -DOMINIO, -ESTRATO, -ESTRATOSOCIO, -FACTOR200_FINAL, -FACTORFINAL_HOGAR, -P203, -P206, -P207, -P208, -P209, -P210_A, -P210_M,
           -P501_1_1, -P501_1_2, -P501_2_1, -P501_2_2, -P501_3_1, -P501_3_2, -P501_4_1, -P501_4_2, -P501_5_1, -P501_5_2, -P501_6_1, -P501_6_2, -P501_7_1, -P501_7_2)
  # Quitaron: ID, CCDD, -NOMBREDD, -CCPP, -NOMBREPP, -CCDI, -NOMBREDI, -UBIGEO, -AREA, PER, -ODEI, -NOM_ODEI
  ENLgeneralhog <- merge(ENL100_600_P501_Hogar, ENL200_Persontotal, by="IDHUMAN", all.x = TRUE, all.y = TRUE)
  ENLgeneralhog <- merge(ENLgeneralhog, ENL300_400_Per18a64, by="IDHUMAN", all.x = TRUE, all.y = TRUE)
  ENLgeneralhog <- merge(ENLgeneralhog, ENL500_Per0a17, by="IDHUMAN", all.x = TRUE, all.y = TRUE)
  ENLgeneral <- ENLgeneralhog %>%
    filter(seccion2 != "NA") %>%
    mutate(RESHAB = case_when(P206 == 1 & P207 == 1 ~ "No",
                              P206 == 2 & P208 == 2 ~ "No")) %>%
    filter(is.na(RESHAB))
  rm(ENL100_600_P501_Hogar, ENL200_Persontotal, ENL300_400_Per18a64, ENL500_Per0a17)
}  




# 400. PRÁCTICAS LECTORAS (Para todas las personas de 18 a 64 años) #****************************************************

# A. PRÁCTICAS LECTORAS GENERALES
#****************************************************
#* P401_1: A usted le han leído libros, periódicos, revistas, contenidos digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P401_1 = case_when(P401_1 == 1 ~ "a. Sí",
                              P401_1 == 2 ~ "b. No"))
  
  P401 <- ENLgeneral %>%
    select(P401_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P401_1sexo <- P401 %>%
    select(P401_1, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P401_1edad_group <- P401 %>%
    select(P401_1, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P401_1niv_educ <- P401 %>%
    select(P401_1, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>%
    
    ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P401_1auto_etnia <- P401 %>%
    select(P401_1, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P401_1estrasoc <- P401 %>%
    select(P401_1, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P401_1area <- P401 %>%
    select(P401_1, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>%
    
    mutate(Variables = "Área de residencia")
  
  P401_1zona <- P401 %>%
    select(P401_1, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P401_1dom <- P401 %>%
    select(P401_1, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P401_1region_nat <- P401 %>%
    select(P401_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P401_1dep <- P401 %>%
    select(P401_1, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>%
    
    ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P401_1total <- P401 %>%
    select(P401_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P401_1 != "NA") %>%
    group_by(P401_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401_1)) %>% ungroup %>%
    spread(P401_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P401 <- bind_rows(P401_1total, P401_1sexo, P401_1edad_group, P401_1niv_educ, P401_1auto_etnia, P401_1estrasoc, P401_1area, P401_1zona, P401_1dom, P401_1region_nat, P401_1dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P401_1total, P401_1sexo, P401_1edad_group, P401_1niv_educ, P401_1auto_etnia, P401_1estrasoc, P401_1area, P401_1zona, P401_1dom, P401_1region_nat, P401_1dep)
  
  P401 <- P401 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P401A: Frecuencia con la que le han leído
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P401 = case_when(P401 == 1 ~ "a. Diariamente", P401 == 2 ~ "b. Varias veces a la semana", P401 == 3 ~ "c. Una vez a la semana", P401 == 4 ~ "d. Una vez al mes",
                            P401 == 5 ~ "e. Una vez cada tres meses", P401 == 6 ~ "f. Por lo menos una vez al año"))
  
  P401A <- ENLgeneral %>%
    select(P401_1, P401, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P401_1 == "a. Sí")
  
  P401sexo <- P401A %>%
    select(P401, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P401edad_group <- P401A %>%
    select(P401, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P401niv_educ <- P401A %>%
    select(P401, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P401auto_etnia <- P401A %>%
    select(P401, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P401estrasoc <- P401A %>%
    select(P401, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P401area <- P401A %>%
    select(P401, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P401zona <- P401A %>%
    select(P401, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P401dom <- P401A %>%
    select(P401, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P401region_nat <- P401A %>%
    select(P401, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P401dep <- P401A %>%
    select(P401, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P401total <- P401A %>%
    select(P401, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P401 != "NA") %>%
    group_by(P401, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P401)) %>% ungroup %>%
    
    spread(P401, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P401A <- bind_rows(P401total, P401sexo, P401edad_group, P401niv_educ, P401auto_etnia, P401estrasoc, P401area, P401zona, P401dom, P401region_nat, P401dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P401total, P401sexo, P401edad_group, P401niv_educ, P401auto_etnia, P401estrasoc, P401area, P401zona, P401dom, P401region_nat, P401dep)
  
  P401A <- P401A %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Una vez al mes (%)` = round (100 * `d. Una vez al mes` / `Total`, 1)) %>%
    mutate(`e. Una vez cada tres meses (%)` = round (100 * `e. Una vez cada tres meses` / `Total`, 1))
  %>%
    mutate(`f. Por lo menos una vez al año (%)` = round (100 * `f. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P402: Usted ha leído publicaciones / contenidos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P402 = case_when(P402 == 1 ~ "a. Sí",
                            P402 == 2 ~ "b. No"))
  
  P402 <- ENLgeneral %>%
    select(P402, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P402sexo <- P402 %>%
    select(P402, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>%
    
    mutate(Variables = "Sexo")
  
  P402edad_group <- P402 %>%
    select(P402, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P402niv_educ <- P402 %>%
    select(P402, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P402auto_etnia <- P402 %>%
    select(P402, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P402estrasoc <- P402 %>%
    select(P402, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>%
    
    ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P402area <- P402 %>%
    select(P402, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P402zona <- P402 %>%
    select(P402, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P402dom <- P402 %>%
    select(P402, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>%
    
    mutate(Variables = "Dominio")
  
  P402region_nat <- P402 %>%
    select(P402, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P402dep <- P402 %>%
    select(P402, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P402total <- P402 %>%
    select(P402, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P402 != "NA") %>%
    group_by(P402, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402)) %>% ungroup %>%
    spread(P402, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P402 <- bind_rows(P402total, P402sexo, P402edad_group, P402niv_educ, P402auto_etnia, P402estrasoc, P402area, P402zona, P402dom, P402region_nat, P402dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P402total, P402sexo, P402edad_group, P402niv_educ, P402auto_etnia, P402estrasoc, P402area, P402zona, P402dom, P402region_nat, P402dep)
  
  P402 <- P402 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P402a: Usted ha leído publicaciones / contenidos 0 a 64 años
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P402b = case_when(P402 == 1 ~ "a. Sí", P402 == 2 ~ "b. No",
                             between(P505, 1, 6) ~ "a. Sí", P505 == 7 ~ "b. No",
                             P505 == 8 ~ "b. No",
                             between(P506, 1, 7) ~ "a. Sí", P506 == 8 ~ "b. No",
                             P506 == 9 ~ "b. No",
                             between(P507, 1, 5) ~ "a. Sí", P507 == 6 ~ "b. No",
                             P507 == 7 ~ "b. No",
                             between(P508, 1, 5) ~ "a. Sí", P508 == 6 ~ "b. No",
                             P508 == 7 ~ "b. No"))
  
  sdk <- ENLgeneral %>%
    select(edad, P402b, P402, P505, P506, P507, P508)
  
  P402b <- ENLgeneral %>%
    select(P402b, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 0 & edad <= 64)
  
  P402bsexo <- P402b %>%
    select(P402b, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P402bedad_group <- P402b %>%
    select(P402b, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>%
    
    ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P402bniv_educ <- P402b %>%
    select(P402b, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P402bauto_etnia <- P402b %>%
    select(P402b, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P402bestrasoc <- P402b %>%
    select(P402b, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>%
    
    mutate(Variables = "Estrato socioeconómico")
  
  P402barea <- P402b %>%
    select(P402b, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P402bzona <- P402b %>%
    select(P402b, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P402bdom <- P402b %>%
    select(P402b, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P402bregion_nat <- P402b %>%
    select(P402b, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>%
    
    ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P402bdep <- P402b %>%
    select(P402b, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P402btotal <- P402b %>%
    select(P402b, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P402b != "NA") %>%
    group_by(P402b, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P402b)) %>% ungroup %>%
    spread(P402b, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P402b <- bind_rows(P402btotal, P402bsexo, P402bedad_group, P402bniv_educ, P402bauto_etnia, P402bestrasoc, P402barea, P402bzona, P402bdom, P402bregion_nat, P402bdep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P402btotal, P402bsexo, P402bedad_group, P402bniv_educ, P402bauto_etnia, P402bestrasoc, P402barea, P402bzona, P402bdom, P402bregion_nat, P402bdep)
  
  P402b <- P402b %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

#* P403: Frecuencia con la que ha leído publicaciones / contenidos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P403 = case_when(P403 == 1 ~ "a. Diariamente", P403 == 2 ~ "b. Varias veces a la semana", P403 == 3 ~ "c. Una vez a la semana", P403 == 4 ~ "d. Una vez al mes",
                            P403 == 5 ~ "e. Una vez cada tres meses", P403 == 6 ~ "f. Por lo menos una vez al año"))
  
  P403 <- ENLgeneral %>%
    select(P402, P403, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P402 == "a. Sí") %>% filter(sabe_leer == "a. Sí")
  
  P403sexo <- P403 %>%
    select(P403, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P403edad_group <- P403 %>%
    select(P403, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P403niv_educ <- P403 %>%
    select(P403, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P403auto_etnia <- P403 %>%
    select(P403, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P403estrasoc <- P403 %>%
    select(P403, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P403area <- P403 %>%
    select(P403, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P403zona <- P403 %>%
    select(P403, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P403dom <- P403 %>%
    select(P403, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P403region_nat <- P403 %>%
    select(P403, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P403dep <- P403 %>%
    select(P403, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P403total <- P403 %>%
    select(P403, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P403 != "NA") %>%
    group_by(P403, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P403)) %>% ungroup %>%
    spread(P403, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P403 <- bind_rows(P403total, P403sexo, P403edad_group, P403niv_educ, P403auto_etnia, P403estrasoc, P403area, P403zona, P403dom, P403region_nat, P403dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P403total, P403sexo, P403edad_group, P403niv_educ, P403auto_etnia, P403estrasoc, P403area, P403zona, P403dom, P403region_nat, P403dep)
  
  P403 <- P403 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Una vez al mes (%)` = round (100 * `d. Una vez al mes` / `Total`, 1)) %>%
    mutate(`e. Una vez cada tres meses (%)` = round (100 * `e. Una vez cada tres meses` / `Total`, 1))
  %>%
    mutate(`f. Por lo menos una vez al año (%)` = round (100 * `f. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P404: Razones por las cuales no leyó publicaciones / contenidos
{
  ENLgeneral <- ENLgeneral %>% mutate(P404_1 = as.numeric(P404_1)) %>%
    mutate(P404_1 = case_when(P404_1 == 1 ~ "a. Sí",
                              P404_1 == 0 ~ "b. No")) %>%
    mutate(P404_2 = as.numeric(P404_2)) %>% mutate(P404_2 = case_when(P404_2 == 1 ~ "a. Sí",
                                                                      P404_2 == 0 ~ "b. No")) %>%
    mutate(P404_3 = as.numeric(P404_3)) %>%
    
    mutate(P404_3 = case_when(P404_3 == 1 ~ "a. Sí",
                              P404_3 == 0 ~ "b. No")) %>%
    mutate(P404_4 = as.numeric(P404_4)) %>% mutate(P404_4 = case_when(P404_4 == 1 ~ "a. Sí",
                                                                      P404_4 == 0 ~ "b. No")) %>%
    mutate(P404_5 = as.numeric(P404_5)) %>% mutate(P404_5 = case_when(P404_5 == 1 ~ "a. Sí",
                                                                      P404_5 == 0 ~ "b. No")) %>%
    mutate(P404_6 = as.numeric(P404_6)) %>% mutate(P404_6 = case_when(P404_6 == 1 ~ "a. Sí",
                                                                      P404_6 == 0 ~ "b. No")) %>%
    mutate(P404_7 = as.numeric(P404_7)) %>% mutate(P404_7 = case_when(P404_7 == 1 ~ "a. Sí",
                                                                      P404_7 == 0 ~ "b. No")) %>%
    mutate(P404_8 = as.numeric(P404_8)) %>% mutate(P404_8 = case_when(P404_8 == 1 ~ "a. Sí",
                                                                      P404_8 == 0 ~ "b. No")) %>%
    mutate(P404_9 = as.numeric(P404_9)) %>% mutate(P404_9 = case_when(P404_9 == 1 ~ "a. Sí",
                                                                      P404_9 == 0 ~ "b. No"))
  
  ENLgeneral404_1 <- ENLgeneral %>% select(P402, P404 = P404_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Prefiere realizar otras actividades culturales")
  ENLgeneral404_2 <- ENLgeneral %>% select(P402, P404 = P404_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Prefiere realizar otras actividades recreativas y/o sociales")
  ENLgeneral404_3 <- ENLgeneral %>% select(P402, P404 = P404_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Por falta de tiempo")
  ENLgeneral404_4 <- ENLgeneral %>% select(P402, P404 = P404_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Por falta de dinero")
  ENLgeneral404_5 <- ENLgeneral %>% select(P402, P404 = P404_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Falta de bibliotecas cerca a su hogar o centro de estudios") ENLgeneral404_6 <- ENLgeneral %>% select(P402, P404 = P404_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                                                                                                                                                                                                                                                                                                               FACTOR200_FINAL) %>% mutate(Tipo = "f. No le gusta leer / Falta de interés")
  ENLgeneral404_7 <- ENLgeneral %>% select(P402, P404 = P404_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Las publicaciones no están en su lengua materna")
  ENLgeneral404_8 <- ENLgeneral %>% select(P402, P404 = P404_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. No hay publicaciones disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral404_9 <- ENLgeneral %>% select(P402, P404 = P404_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Otro")
  
  P404 <- bind_rows(ENLgeneral404_1, ENLgeneral404_2, ENLgeneral404_3, ENLgeneral404_4, ENLgeneral404_5, ENLgeneral404_6, ENLgeneral404_7, ENLgeneral404_8, ENLgeneral404_9) %>%
    filter(!is.na(P404)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>%
    
    filter(P402 == "b. No")
  
  rm(ENLgeneral404_1, ENLgeneral404_2, ENLgeneral404_3, ENLgeneral404_4, ENLgeneral404_5, ENLgeneral404_6, ENLgeneral404_7, ENLgeneral404_8, ENLgeneral404_9)
  
  P404sexo <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P404edad_group <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P404niv_educ <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P404auto_etnia <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P404estrasoc <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P404area <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P404zona <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(zona) %>% adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P404dom <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P404region_nat <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P404dep <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P404 != "NA") %>%
    group_by(P404, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P404)) %>%
    ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P404total <- P404 %>%
    select(P404, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P404 != "NA") %>%
    group_by(P404, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P404)) %>% ungroup %>%
    spread(P404, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P404 <- bind_rows(P404total, P404sexo, P404edad_group, P404niv_educ, P404auto_etnia, P404estrasoc, P404area, P404zona, P404dom, P404region_nat, P404dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P404total, P404sexo, P404edad_group, P404niv_educ, P404auto_etnia, P404estrasoc, P404area, P404zona, P404dom, P404region_nat, P404dep)
  
  P404 <- P404 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P405: En que lugares ha leído publicaciones / contenidos
{
  ENLgeneral <- ENLgeneral %>% mutate(P405_1 = as.numeric(P405_1)) %>%
    mutate(P405_1 = case_when(P405_1 == 1 ~ "a. Sí",
                              P405_1 == 0 ~ "b. No")) %>%
    mutate(P405_2 = as.numeric(P405_2)) %>% mutate(P405_2 = case_when(P405_2 == 1 ~ "a. Sí",
                                                                      P405_2 == 0 ~ "b. No")) %>%
    mutate(P405_3 = as.numeric(P405_3)) %>% mutate(P405_3 = case_when(P405_3 == 1 ~ "a. Sí",
                                                                      P405_3 == 0 ~ "b. No")) %>%
    mutate(P405_4 = as.numeric(P405_4)) %>% mutate(P405_4 = case_when(P405_4 == 1 ~ "a. Sí",
                                                                      P405_4 == 0 ~ "b. No")) %>%
    mutate(P405_5 = as.numeric(P405_5)) %>% mutate(P405_5 = case_when(P405_5 == 1 ~ "a. Sí",
                                                                      P405_5 == 0 ~ "b. No")) %>%
    mutate(P405_6 = as.numeric(P405_6)) %>% mutate(P405_6 = case_when(P405_6 == 1 ~ "a. Sí",
                                                                      P405_6 == 0 ~ "b. No")) %>%
    mutate(P405_7 = as.numeric(P405_7)) %>% mutate(P405_7 = case_when(P405_7 == 1 ~ "a. Sí",
                                                                      P405_7 == 0 ~ "b. No")) %>%
    mutate(P405_8 = as.numeric(P405_8)) %>% mutate(P405_8 = case_when(P405_8 == 1 ~ "a. Sí",
                                                                      P405_8 == 0 ~ "b. No")) %>%
    mutate(P405_9 = as.numeric(P405_9)) %>% mutate(P405_9 = case_when(P405_9 == 1 ~ "a. Sí",
                                                                      
                                                                      P405_9 == 0 ~ "b. No")) %>%
    mutate(P405_10 = as.numeric(P405_10)) %>% mutate(P405_10 = case_when(P405_10 == 1 ~ "a. Sí",
                                                                         P405_10 == 0 ~ "b. No")) %>%
    mutate(P405_11 = as.numeric(P405_11)) %>% mutate(P405_11 = case_when(P405_11 == 1 ~ "a. Sí",
                                                                         P405_11 == 0 ~ "b. No")) %>%
    mutate(P405_12 = as.numeric(P405_12)) %>% mutate(P405_12 = case_when(P405_12 == 1 ~ "a. Sí",
                                                                         P405_12 == 0 ~ "b. No")) %>%
    mutate(P405_13 = as.numeric(P405_13)) %>% mutate(P405_13 = case_when(P405_13 == 1 ~ "a. Sí",
                                                                         P405_13 == 0 ~ "b. No"))
  
  ENLgeneral405_1 <- ENLgeneral %>% select(P402, P405 = P405_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. En su casa o en un lugar privado")
  ENLgeneral405_2 <- ENLgeneral %>% select(P402, P405 = P405_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. En el transporte")
  ENLgeneral405_3 <- ENLgeneral %>% select(P402, P405 = P405_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. En el salón de clase")
  ENLgeneral405_4 <- ENLgeneral %>% select(P402, P405 = P405_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. En el sitio de trabajo")
  ENLgeneral405_5 <- ENLgeneral %>% select(P402, P405 = P405_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. En parques, alamedas, plazas, malecones")
  ENLgeneral405_6 <- ENLgeneral %>% select(P402, P405 = P405_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. En lugares junto a la playa, al río o lagunas/lagos, bosques, espacios verdes, pampas")
  ENLgeneral405_7 <- ENLgeneral %>% select(P402, P405 = P405_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. En centros comerciales/ mercados")
  ENLgeneral405_8 <- ENLgeneral %>% select(P402, P405 = P405_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. En librerías")
  ENLgeneral405_9 <- ENLgeneral %>% select(P402, P405 = P405_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. En bibliotecas")
  ENLgeneral405_10 <- ENLgeneral %>% select(P402, P405 = P405_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. En un local comunal / espacio de la comFACTOR200_FINAL / malocas")
  ENLgeneral405_11 <- ENLgeneral %>% select(P402, P405 = P405_11, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. En cafeterías/restaurantes")
  ENLgeneral405_12 <- ENLgeneral %>% select(P402, P405 = P405_12, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "l. En locales con acceso público a TIC/internet (cabinas)")
  ENLgeneral405_13 <- ENLgeneral %>% select(P402, P405 = P405_13, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "m. Otro")
  
  P405 <- bind_rows(ENLgeneral405_1, ENLgeneral405_2, ENLgeneral405_3, ENLgeneral405_4, ENLgeneral405_5, ENLgeneral405_6, ENLgeneral405_7, ENLgeneral405_8, ENLgeneral405_9,
                    ENLgeneral405_10, ENLgeneral405_11, ENLgeneral405_12, ENLgeneral405_13) %>% filter(!is.na(P405)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P402 == "a. Sí")
  
  rm(ENLgeneral405_1, ENLgeneral405_2, ENLgeneral405_3, ENLgeneral405_4, ENLgeneral405_5, ENLgeneral405_6, ENLgeneral405_7, ENLgeneral405_8, ENLgeneral405_9,
     ENLgeneral405_10, ENLgeneral405_11, ENLgeneral405_12, ENLgeneral405_13)
  
  P405sexo <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P405edad_group <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P405niv_educ <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P405auto_etnia <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P405estrasoc <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P405area <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P405zona <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P405dom <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P405region_nat <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P405dep <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P405)) %>%
    ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P405total <- P405 %>%
    select(P405, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P405 != "NA") %>%
    group_by(P405, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P405)) %>% ungroup %>%
    spread(P405, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P405 <- bind_rows(P405total, P405sexo, P405edad_group, P405niv_educ, P405auto_etnia, P405estrasoc, P405area, P405zona, P405dom, P405region_nat, P405dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P405total, P405sexo, P405edad_group, P405niv_educ, P405auto_etnia, P405estrasoc, P405area, P405zona, P405dom, P405region_nat, P405dep)
  
  P405 <- P405 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P406: Principales razones por las que leyó publicaciones / contenidos
{
  ENLgeneral <- ENLgeneral %>% mutate(P406_1 = as.numeric(P406_1)) %>%
    mutate(P406_1 = case_when(P406_1 == 1 ~ "a. Sí",
                              P406_1 == 0 ~ "b. No")) %>%
    mutate(P406_2 = as.numeric(P406_2)) %>% mutate(P406_2 = case_when(P406_2 == 1 ~ "a. Sí",
                                                                      P406_2 == 0 ~ "b. No")) %>%
    mutate(P406_3 = as.numeric(P406_3)) %>% mutate(P406_3 = case_when(P406_3 == 1 ~ "a. Sí",
                                                                      P406_3 == 0 ~ "b. No")) %>%
    mutate(P406_4 = as.numeric(P406_4)) %>% mutate(P406_4 = case_when(P406_4 == 1 ~ "a. Sí",
                                                                      P406_4 == 0 ~ "b. No")) %>%
    mutate(P406_5 = as.numeric(P406_5)) %>% mutate(P406_5 = case_when(P406_5 == 1 ~ "a. Sí",
                                                                      P406_5 == 0 ~ "b. No")) %>%
    mutate(P406_6 = as.numeric(P406_6)) %>% mutate(P406_6 = case_when(P406_6 == 1 ~ "a. Sí",
                                                                      P406_6 == 0 ~ "b. No")) %>%
    mutate(P406_7 = as.numeric(P406_7)) %>%
    
    mutate(P406_7 = case_when(P406_7 == 1 ~ "a. Sí",
                              P406_7 == 0 ~ "b. No")) %>%
    mutate(P406_8 = as.numeric(P406_8)) %>% mutate(P406_8 = case_when(P406_8 == 1 ~ "a. Sí",
                                                                      P406_8 == 0 ~ "b. No")) %>%
    mutate(P406_9 = as.numeric(P406_9)) %>% mutate(P406_9 = case_when(P406_9 == 1 ~ "a. Sí",
                                                                      P406_9 == 0 ~ "b. No"))
  
  ENLgeneral406_1 <- ENLgeneral %>% select(P402, P406 = P406_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Por placer, gusto o entretenimiento")
  ENLgeneral406_2 <- ENLgeneral %>% select(P402, P406 = P406_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Por razones de estudio personal")
  ENLgeneral406_3 <- ENLgeneral %>% select(P402, P406 = P406_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Para informarse de los sucesos cotidianos")
  ENLgeneral406_4 <- ENLgeneral %>% select(P402, P406 = P406_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Por motivos laborales")
  ENLgeneral406_5 <- ENLgeneral %>% select(P402, P406 = P406_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Por desarrollo personal")
  ENLgeneral406_6 <- ENLgeneral %>% select(P402, P406 = P406_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Por motivos religiosos")
  ENLgeneral406_7 <- ENLgeneral %>% select(P402, P406 = P406_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Por cultura general")
  ENLgeneral406_8 <- ENLgeneral %>% select(P402, P406 = P406_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Otro")
  ENLgeneral406_9 <- ENLgeneral %>% select(P402, P406 = P406_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Para apoyar en el estudio/entretenimiento a sus hijos/ otras personas")
  P406 <- bind_rows(ENLgeneral406_1, ENLgeneral406_2, ENLgeneral406_3, ENLgeneral406_4, ENLgeneral406_5, ENLgeneral406_6, ENLgeneral406_7, ENLgeneral406_8, ENLgeneral406_9) %>%
    filter(!is.na(P406)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P402 == "a. Sí")
  
  rm(ENLgeneral406_1, ENLgeneral406_2, ENLgeneral406_3, ENLgeneral406_4, ENLgeneral406_5, ENLgeneral406_6, ENLgeneral406_7, ENLgeneral406_8, ENLgeneral406_9)
  
  P406sexo <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P406edad_group <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P406niv_educ <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P406auto_etnia <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P406estrasoc <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P406area <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P406zona <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P406dom <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P406region_nat <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P406dep <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P406)) %>%
    ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P406total <- P406 %>%
    select(P406, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P406 != "NA") %>%
    group_by(P406, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P406)) %>% ungroup %>%
    spread(P406, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>%
    
    filter(Tipo != "Total")
  
  P406 <- bind_rows(P406total, P406sexo, P406edad_group, P406niv_educ, P406auto_etnia, P406estrasoc, P406area, P406zona, P406dom, P406region_nat, P406dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P406total, P406sexo, P406edad_group, P406niv_educ, P406auto_etnia, P406estrasoc, P406area, P406zona, P406dom, P406region_nat, P406dep)
  
  P406 <- P406 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P407: Actividades vinculadas a la lectura en general
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P407_1 = case_when(P407_1 == 1 ~ "a. Sí",
                              P407_1 == 2 ~ "b. No")) %>%
    mutate(P407_2 = case_when(P407_2 == 1 ~ "a. Sí",
                              P407_2 == 2 ~ "b. No")) %>%
    mutate(P407_3 = case_when(P407_3 == 1 ~ "a. Sí",
                              P407_3 == 2 ~ "b. No")) %>%
    mutate(P407_4 = case_when(P407_4 == 1 ~ "a. Sí",
                              P407_4 == 2 ~ "b. No"))
  
  ENLgeneral407_1 <- ENLgeneral %>% select(P402, P407 = P407_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Comentó o conversó con amigos/as, familiares sobre lo que ha leído")
  ENLgeneral407_2 <- ENLgeneral %>% select(P402, P407 = P407_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Buscó lecturas similares o información adicional sobre lo que ha leído o sobre un tema de su interés")
  ENLgeneral407_3 <- ENLgeneral %>% select(P402, P407 = P407_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Escribió un texto sobre lo que ha leído")
  ENLgeneral407_4 <- ENLgeneral %>% select(P402, P407 = P407_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Participó en clubes o círculos de lectores, lecturas colectivas o tertulias (presenciales o virtuales)")
  
  P407 <- bind_rows(ENLgeneral407_1, ENLgeneral407_2, ENLgeneral407_3, ENLgeneral407_4)
  %>%
    filter(!is.na(P407)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P402 == "a. Sí")
  rm(ENLgeneral407_1, ENLgeneral407_2, ENLgeneral407_3, ENLgeneral407_4) P407sexo <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, sexo) %>%
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>%
    
    mutate(ID= row_number(P407)) %>% ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P407edad_group <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P407niv_educ <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P407auto_etnia <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P407estrasoc <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P407area <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P407zona <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P407dom <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>%
    
    mutate(ID= row_number(P407)) %>% ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P407region_nat <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P407dep <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P407total <- P407 %>%
    select(P407, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P407)) %>% ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>%
    
    adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P407 <- bind_rows(P407total, P407sexo, P407edad_group, P407niv_educ, P407auto_etnia, P407estrasoc, P407area, P407zona, P407dom, P407region_nat, P407dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P407total, P407sexo, P407edad_group, P407niv_educ, P407auto_etnia, P407estrasoc, P407area, P407zona, P407dom, P407region_nat, P407dep)
  
  P407 <- P407 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P407_1: Frecuencia con la que realizó actividades vinculadas a la lectura en general
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P407_1_1 = case_when(P407_1_1 == 1 ~ "a. Diariamente", P407_1_1 == 2 ~ "b. Varias veces a la semana", P407_1_1 == 3 ~ "c. Una vez a la semana", P407_1_1 == 4 ~ "d. Una vez al mes",
                                P407_1_1 == 5 ~ "e. Una vez cada tres meses", P407_1_1 == 6 ~ "f. Por lo menos una vez al año")) %>%
    mutate(P407_2_1 = case_when(P407_2_1 == 1 ~ "a. Diariamente", P407_2_1 == 2 ~ "b. Varias veces a la semana", P407_2_1 == 3 ~ "c. Una vez a la semana", P407_2_1 == 4 ~ "d. Una vez al mes",
                                P407_2_1 == 5 ~ "e. Una vez cada tres meses", P407_2_1 == 6 ~ "f. Por lo menos una vez al año")) %>%
    mutate(P407_3_1 = case_when(P407_3_1 == 1 ~ "a. Diariamente", P407_3_1 == 2 ~ "b. Varias veces a la semana", P407_3_1 == 3 ~ "c. Una vez a la semana", P407_3_1 == 4 ~ "d. Una vez al mes",
                                P407_3_1 == 5 ~ "e. Una vez cada tres meses", P407_3_1 == 6 ~ "f. Por lo menos una vez al año")) %>%
    mutate(P407_4_1 = case_when(P407_4_1 == 1 ~ "a. Diariamente", P407_4_1 == 2 ~ "b. Varias veces a la semana", P407_4_1 == 3 ~ "c. Una vez a la semana", P407_4_1 == 4 ~ "d. Una vez al mes",
                                P407_4_1 == 5 ~ "e. Una vez cada tres meses", P407_4_1 == 6 ~ "f. Por lo menos una vez al año"))
  
  ENLgeneral407_1 <- ENLgeneral %>% select(P402, P407 = P407_1_1, P407_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Comentó o conversó con amigos/as, familiares sobre lo que ha leído") %>% filter(P407_1 == "a. Sí")
  ENLgeneral407_2 <- ENLgeneral %>% select(P402, P407 = P407_2_1, P407_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Buscó lecturas similares o información adicional sobre lo que ha leído o sobre un tema de su interés") %>% filter(P407_2 == "a. Sí")
  ENLgeneral407_3 <- ENLgeneral %>% select(P402, P407 = P407_3_1, P407_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                           
                                           FACTOR200_FINAL) %>% mutate(Tipo = "c. Escribió un texto sobre lo que ha leído") %>% filter(P407_3 == "a. Sí")
  ENLgeneral407_4 <- ENLgeneral %>% select(P402, P407 = P407_4_1, P407_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Participó en clubes o círculos de lectores, lecturas colectivas o tertulias (presenciales o virtuales)") %>% filter(P407_4 == "a. Sí")
  
  P407_1 <- bind_rows(ENLgeneral407_1, ENLgeneral407_2, ENLgeneral407_3, ENLgeneral407_4)
  %>%
    filter(!is.na(P407)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P402 == "a. Sí")
  rm(ENLgeneral407_1, ENLgeneral407_2, ENLgeneral407_3, ENLgeneral407_4) P407sexo <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, sexo) %>%
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P407edad_group <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P407niv_educ <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P407auto_etnia <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P407estrasoc <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P407area <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P407zona <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P407dom <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P407region_nat <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P407dep <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P407)) %>%
    ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P407total <- P407_1 %>%
    select(P407, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P407 != "NA") %>%
    group_by(P407, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P407)) %>% ungroup %>%
    spread(P407, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P407_1 <- bind_rows(P407total, P407sexo, P407edad_group, P407niv_educ, P407auto_etnia, P407estrasoc, P407area, P407zona, P407dom, P407region_nat, P407dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P407total, P407sexo, P407edad_group, P407niv_educ, P407auto_etnia, P407estrasoc, P407area, P407zona, P407dom, P407region_nat, P407dep)
  
  P407_1 <- P407_1 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Una vez al mes (%)` = round (100 * `d. Una vez al mes` / `Total`, 1)) %>%
    mutate(`e. Una vez cada tres meses (%)` = round (100 * `e. Una vez cada tres meses` / `Total`, 1))
  %>%
    mutate(`f. Por lo menos una vez al año (%)` = round (100 * `f. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

# B. LECTURA DE LIBROS
#****************************************************            #* P408: Usted ha leído Libros
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P408 = case_when(P408 == 1 ~ "a. Sí",
                            P408 == 2 ~ "b. No"))
  
  P408 <- ENLgeneral %>%
    
    select(P408, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P408sexo <- P408 %>%
    select(P408, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P408edad_group <- P408 %>%
    select(P408, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P408niv_educ <- P408 %>%
    select(P408, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P408auto_etnia <- P408 %>%
    select(P408, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P408estrasoc <- P408 %>%
    select(P408, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P408area <- P408 %>%
    select(P408, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P408zona <- P408 %>%
    select(P408, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(zona) %>% adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P408dom <- P408 %>%
    select(P408, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P408region_nat <- P408 %>%
    select(P408, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P408dep <- P408 %>%
    select(P408, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P408total <- P408 %>%
    select(P408, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P408 != "NA") %>%
    group_by(P408, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P408)) %>% ungroup %>%
    spread(P408, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P408 <- bind_rows(P408total, P408sexo, P408edad_group, P408niv_educ, P408auto_etnia, P408estrasoc, P408area, P408zona, P408dom, P408region_nat, P408dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P408total, P408sexo, P408edad_group, P408niv_educ, P408auto_etnia, P408estrasoc, P408area, P408zona, P408dom, P408region_nat, P408dep)
  
  P408 <- P408 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P409: Frecuencia con la que leyó Libros
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P409 = case_when(P409 == 1 ~ "a. Diariamente",
                            P409 == 2 ~ "b. Por lo menos una vez a la semana", P409 == 3 ~ "b. Por lo menos una vez a la semana", P409 == 4 ~ "c. Por lo menos una vez al año",
                            P409 == 5 ~ "c. Por lo menos una vez al año", P409 == 6 ~ "c. Por lo menos una vez al año"))
  
  P409 <- ENLgeneral %>%
    select(P408, P409, P212, P313, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P408 == "a. Sí") %>% filter(sabe_leer == "a. Sí")
  
  P409est_civil <- P409 %>%
    select(P409, FACTOR200_FINAL, P313) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(P313) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(P313) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = P313, everything()) %>% mutate(Variables = "Estado civil")
  
  P409sexo <- P409 %>%
    select(P409, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P409edad_group <- P409 %>%
    select(P409, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P409niv_educ <- P409 %>%
    select(P409, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P409auto_etnia <- P409 %>%
    select(P409, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P409estrasoc <- P409 %>%
    select(P409, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P409area <- P409 %>%
    select(P409, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P409zona <- P409 %>%
    select(P409, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P409dom <- P409 %>%
    select(P409, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P409region_nat <- P409 %>%
    select(P409, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P409dep <- P409 %>%
    select(P409, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P409total <- P409 %>%
    select(P409, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P409 != "NA") %>%
    
    group_by(P409, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P409)) %>% ungroup %>%
    spread(P409, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P409 <- bind_rows(P409total, P409sexo, P409edad_group, P409niv_educ, P409auto_etnia, P409est_civil, P409estrasoc, P409area, P409zona, P409dom, P409region_nat, P409dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P409total,	P409sexo,	P409edad_group,	P409niv_educ,	P409auto_etnia,	P409est_civil, P409estrasoc, P409area, P409zona, P409dom, P409region_nat, P409dep)
  
  P409 <- P409 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Por lo menos una vez a la semana (%)` = round (100 * `b. Por lo menos una vez a la semana` / `Total`, 1)) %>%
    mutate(`c. Por lo menos una vez al año (%)` = round (100 * `c. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P410: Principales razones por las que no leyó Libros
{
  ENLgeneral <- ENLgeneral %>% mutate(P410_1 = as.numeric(P410_1)) %>%
    mutate(P410_1 = case_when(P410_1 == 1 ~ "a. Sí",
                              P410_1 == 0 ~ "b. No")) %>%
    mutate(P410_2 = as.numeric(P410_2)) %>% mutate(P410_2 = case_when(P410_2 == 1 ~ "a. Sí",
                                                                      P410_2 == 0 ~ "b. No")) %>%
    mutate(P410_3 = as.numeric(P410_3)) %>% mutate(P410_3 = case_when(P410_3 == 1 ~ "a. Sí",
                                                                      P410_3 == 0 ~ "b. No")) %>%
    mutate(P410_4 = as.numeric(P410_4)) %>% mutate(P410_4 = case_when(P410_4 == 1 ~ "a. Sí",
                                                                      P410_4 == 0 ~ "b. No")) %>%
    mutate(P410_5 = as.numeric(P410_5)) %>% mutate(P410_5 = case_when(P410_5 == 1 ~ "a. Sí",
                                                                      P410_5 == 0 ~ "b. No")) %>%
    mutate(P410_6 = as.numeric(P410_6)) %>% mutate(P410_6 = case_when(P410_6 == 1 ~ "a. Sí",
                                                                      P410_6 == 0 ~ "b. No")) %>%
    mutate(P410_7 = as.numeric(P410_7)) %>% mutate(P410_7 = case_when(P410_7 == 1 ~ "a. Sí",
                                                                      P410_7 == 0 ~ "b. No")) %>%
    mutate(P410_8 = as.numeric(P410_8)) %>% mutate(P410_8 = case_when(P410_8 == 1 ~ "a. Sí",
                                                                      P410_8 == 0 ~ "b. No")) %>%
    
    mutate(P410_9 = as.numeric(P410_9)) %>% mutate(P410_9 = case_when(P410_9 == 1 ~ "a. Sí",
                                                                      P410_9 == 0 ~ "b. No")) %>%
    mutate(P410_10 = as.numeric(P410_10)) %>% mutate(P410_10 = case_when(P410_10 == 1 ~ "a. Sí",
                                                                         P410_10 == 0 ~ "b. No"))
  
  ENLgeneral410_1 <- ENLgeneral %>% select(P408, P410 = P410_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Prefiere realizar otras actividades culturales")
  ENLgeneral410_2 <- ENLgeneral %>% select(P408, P410 = P410_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Prefiere realizar otras actividades recreativas y/o sociales")
  ENLgeneral410_3 <- ENLgeneral %>% select(P408, P410 = P410_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Prefiere leer otro tipo de publicaciones")
  ENLgeneral410_4 <- ENLgeneral %>% select(P408, P410 = P410_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Por falta de tiempo")
  ENLgeneral410_5 <- ENLgeneral %>% select(P408, P410 = P410_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Por falta de dinero")
  ENLgeneral410_6 <- ENLgeneral %>% select(P408, P410 = P410_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Falta de bibliotecas cerca a su hogar o centro de estudios") ENLgeneral410_7 <- ENLgeneral %>% select(P408, P410 = P410_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                                                                                                                                                                                                                                                                                                               FACTOR200_FINAL) %>% mutate(Tipo = "g. No le gusta leer libros / Falta de interés") ENLgeneral410_8 <- ENLgeneral %>% select(P408, P410 = P410_8, sexo, edad_group, edad,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Los libros no están en su lengua materna")
  ENLgeneral410_9 <- ENLgeneral %>% select(P408, P410 = P410_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. No hay libros disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral410_10 <- ENLgeneral %>% select(P408, P410 = P410_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Otro")
  P410 <- bind_rows(ENLgeneral410_1, ENLgeneral410_2, ENLgeneral410_3, ENLgeneral410_4, ENLgeneral410_5, ENLgeneral410_6, ENLgeneral410_7, ENLgeneral410_8, ENLgeneral410_9, ENLgeneral410_10) %>%
    filter(!is.na(P410)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P408 == "b. No")
  
  rm(ENLgeneral410_1, ENLgeneral410_2, ENLgeneral410_3, ENLgeneral410_4, ENLgeneral410_5, ENLgeneral410_6, ENLgeneral410_7, ENLgeneral410_8, ENLgeneral410_9, ENLgeneral410_10)
  
  P410sexo <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P410edad_group <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P410niv_educ <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P410auto_etnia <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P410estrasoc <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P410area <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P410zona <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P410dom <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P410region_nat <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P410dep <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P410)) %>%
    ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P410total <- P410 %>%
    select(P410, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P410 != "NA") %>%
    group_by(P410, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P410)) %>% ungroup %>%
    spread(P410, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P410 <- bind_rows(P410total, P410sexo, P410edad_group, P410niv_educ, P410auto_etnia, P410estrasoc, P410area, P410zona, P410dom, P410region_nat, P410dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P410total, P410sexo, P410edad_group, P410niv_educ, P410auto_etnia, P410estrasoc, P410area, P410zona, P410dom, P410region_nat, P410dep)
  
  P410 <- P410 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P411: Principales razones por las que leyó Libros
{
  ENLgeneral <- ENLgeneral %>% mutate(P411_1 = as.numeric(P411_1)) %>%
    mutate(P411_1 = case_when(P411_1 == 1 ~ "a. Sí",
                              P411_1 == 0 ~ "b. No")) %>%
    mutate(P411_2 = as.numeric(P411_2)) %>% mutate(P411_2 = case_when(P411_2 == 1 ~ "a. Sí",
                                                                      P411_2 == 0 ~ "b. No")) %>%
    mutate(P411_3 = as.numeric(P411_3)) %>% mutate(P411_3 = case_when(P411_3 == 1 ~ "a. Sí",
                                                                      P411_3 == 0 ~ "b. No")) %>%
    mutate(P411_4 = as.numeric(P411_4)) %>% mutate(P411_4 = case_when(P411_4 == 1 ~ "a. Sí",
                                                                      P411_4 == 0 ~ "b. No")) %>%
    mutate(P411_5 = as.numeric(P411_5)) %>% mutate(P411_5 = case_when(P411_5 == 1 ~ "a. Sí",
                                                                      P411_5 == 0 ~ "b. No")) %>%
    mutate(P411_6 = as.numeric(P411_6)) %>% mutate(P411_6 = case_when(P411_6 == 1 ~ "a. Sí",
                                                                      P411_6 == 0 ~ "b. No")) %>%
    mutate(P411_7 = as.numeric(P411_7)) %>% mutate(P411_7 = case_when(P411_7 == 1 ~ "a. Sí",
                                                                      P411_7 == 0 ~ "b. No")) %>%
    mutate(P411_8 = as.numeric(P411_8)) %>% mutate(P411_8 = case_when(P411_8 == 1 ~ "a. Sí",
                                                                      P411_8 == 0 ~ "b. No")) %>%
    mutate(P411_9 = as.numeric(P411_9)) %>% mutate(P411_9 = case_when(P411_9 == 1 ~ "a. Sí",
                                                                      P411_9 == 0 ~ "b. No"))
  
  ENLgeneral411_1 <- ENLgeneral %>% select(P408, P411 = P411_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Por placer, gusto o entretenimiento")
  ENLgeneral411_2 <- ENLgeneral %>% select(P408, P411 = P411_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Por razones de estudio personal")
  
  ENLgeneral411_3 <- ENLgeneral %>% select(P408, P411 = P411_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Para informarse de los sucesos cotidianos")
  ENLgeneral411_4 <- ENLgeneral %>% select(P408, P411 = P411_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Por motivos laborales")
  ENLgeneral411_5 <- ENLgeneral %>% select(P408, P411 = P411_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Por desarrollo personal")
  ENLgeneral411_6 <- ENLgeneral %>% select(P408, P411 = P411_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Por motivos religiosos")
  ENLgeneral411_7 <- ENLgeneral %>% select(P408, P411 = P411_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Por cultura general")
  ENLgeneral411_8 <- ENLgeneral %>% select(P408, P411 = P411_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Otro")
  ENLgeneral411_9 <- ENLgeneral %>% select(P408, P411 = P411_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Para apoyar en el estudio/entretenimiento a sus hijos/ otras personas")
  
  P411 <- bind_rows(ENLgeneral411_1, ENLgeneral411_2, ENLgeneral411_3, ENLgeneral411_4, ENLgeneral411_5, ENLgeneral411_6, ENLgeneral411_7, ENLgeneral411_8, ENLgeneral411_9) %>%
    filter(!is.na(P411)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P408 == "a. Sí")
  
  rm(ENLgeneral411_1, ENLgeneral411_2, ENLgeneral411_3, ENLgeneral411_4, ENLgeneral411_5, ENLgeneral411_6, ENLgeneral411_7, ENLgeneral411_8, ENLgeneral411_9)
  
  P411sexo <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P411edad_group <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P411niv_educ <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P411auto_etnia <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P411estrasoc <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>%
    
    mutate(Variables = "Estrato socioeconómico")
  
  P411area <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P411zona <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P411dom <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P411region_nat <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P411dep <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P411)) %>%
    ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P411total <- P411 %>%
    select(P411, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P411 != "NA") %>%
    group_by(P411, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P411)) %>% ungroup %>%
    spread(P411, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P411 <- bind_rows(P411total, P411sexo, P411edad_group, P411niv_educ, P411auto_etnia, P411estrasoc, P411area, P411zona, P411dom, P411region_nat, P411dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P411total, P411sexo, P411edad_group, P411niv_educ, P411auto_etnia, P411estrasoc, P411area, P411zona, P411dom, P411region_nat, P411dep)
  
  P411 <- P411 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

#* P412_1_2: Número de libros impresos / digitales
{
  ENLgeneral <- ENLgeneral %>% mutate(P412_1_2 = P412_1 + P412_2) %>%
    mutate(P412_1_2 = case_when(between(P412_1_2, 1, 3) ~ "a. 1 a 3 libros",
                                between(P412_1_2, 4, 5) ~ "b. 4 a 5 libros", P412_1_2 >= 6 ~ "c. 6 a más"))
  
  P412_1_2 <- ENLgeneral %>%
    select(P408, P412_1_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P408 == "a. Sí") %>% filter(sabe_leer == "a. Sí")
  
  P412_1_2sexo <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P412_1_2edad_group <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P412_1_2niv_educ <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P412_1_2auto_etnia <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P412_1_2estrasoc <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P412_1_2area <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P412_1_2zona <- P412_1_2 %>%
    
    select(P412_1_2, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P412_1_2dom <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P412_1_2region_nat <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P412_1_2dep <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P412_1_2total <- P412_1_2 %>%
    select(P412_1_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1_2 != "NA") %>% group_by(P412_1_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1_2)) %>% ungroup %>%
    spread(P412_1_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P412_1_2 <- bind_rows(P412_1_2total, P412_1_2sexo, P412_1_2edad_group, P412_1_2niv_educ, P412_1_2auto_etnia, P412_1_2estrasoc, P412_1_2area, P412_1_2zona, P412_1_2dom, P412_1_2region_nat, P412_1_2dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P412_1_2total, P412_1_2sexo, P412_1_2edad_group, P412_1_2niv_educ, P412_1_2auto_etnia, P412_1_2estrasoc, P412_1_2area, P412_1_2zona, P412_1_2dom, P412_1_2region_nat, P412_1_2dep)
  
  P412_1_2 <- P412_1_2 %>%
    mutate(`a. 1 a 3 libros (%)` = round (100 * `a. 1 a 3 libros` / `Total`, 1)) %>%
    mutate(`b. 4 a 5 libros (%)` = round (100 * `b. 4 a 5 libros` / `Total`, 1)) %>% mutate(`c. 6 a más (%)` = round (100 * `c. 6 a más` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}
#* P412_1: Número de libros impresos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P412_1 = case_when(between(P412_1, 1, 3) ~ "a. 1 a 3 libros",
                              between(P412_1, 4, 5) ~ "b. 4 a 5 libros", P412_1 >= 6 ~ "c. 6 a más"))
  
  P412_1 <- ENLgeneral %>%
    select(P408, P412_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P408 == "a. Sí") %>% filter(sabe_leer == "a. Sí")
  
  P412_1sexo <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, sexo) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P412_1edad_group <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P412_1niv_educ <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P412_1auto_etnia <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P412_1estrasoc <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P412_1area <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P412_1zona <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P412_1dom <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, dominio) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P412_1region_nat <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P412_1dep <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P412_1total <- P412_1 %>%
    select(P412_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_1 != "NA") %>%
    group_by(P412_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_1)) %>% ungroup %>%
    spread(P412_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P412_1 <- bind_rows(P412_1total, P412_1sexo, P412_1edad_group, P412_1niv_educ, P412_1auto_etnia, P412_1estrasoc, P412_1area, P412_1zona, P412_1dom, P412_1region_nat, P412_1dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P412_1total, P412_1sexo, P412_1edad_group, P412_1niv_educ, P412_1auto_etnia, P412_1estrasoc, P412_1area, P412_1zona, P412_1dom, P412_1region_nat, P412_1dep)
  
  P412_1 <- P412_1 %>%
    mutate(`a. 1 a 3 libros (%)` = round (100 * `a. 1 a 3 libros` / `Total`, 1)) %>%
    mutate(`b. 4 a 5 libros (%)` = round (100 * `b. 4 a 5 libros` / `Total`, 1)) %>% mutate(`c. 6 a más (%)` = round (100 * `c. 6 a más` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P412_2: Número de libros digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P412_2 = case_when(between(P412_2, 1, 3) ~ "a. 1 a 3 libros",
                              between(P412_2, 4, 5) ~ "b. 4 a 5 libros", P412_2 >= 6 ~ "c. 6 a más"))
  
  P412_2 <- ENLgeneral %>%
    select(P408, P412_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P408 == "a. Sí") %>% filter(sabe_leer == "a. Sí")
  
  P412_2sexo <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P412_2edad_group <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P412_2niv_educ <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P412_2auto_etnia <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P412_2estrasoc <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P412_2area <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P412_2zona <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P412_2dom <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P412_2region_nat <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P412_2dep <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P412_2total <- P412_2 %>%
    select(P412_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P412_2 != "NA") %>%
    group_by(P412_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P412_2)) %>% ungroup %>%
    spread(P412_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P412_2 <- bind_rows(P412_2total, P412_2sexo, P412_2edad_group, P412_2niv_educ, P412_2auto_etnia, P412_2estrasoc, P412_2area, P412_2zona, P412_2dom, P412_2region_nat, P412_2dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P412_2total, P412_2sexo, P412_2edad_group, P412_2niv_educ, P412_2auto_etnia, P412_2estrasoc, P412_2area, P412_2zona, P412_2dom, P412_2region_nat, P412_2dep)
  
  P412_2 <- P412_2 %>%
    mutate(`a. 1 a 3 libros (%)` = round (100 * `a. 1 a 3 libros` / `Total`, 1)) %>%
    
    mutate(`b. 4 a 5 libros (%)` = round (100 * `b. 4 a 5 libros` / `Total`, 1)) %>% mutate(`c. 6 a más (%)` = round (100 * `c. 6 a más` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}
#* P413: Tipo de libros que leyó
{
  ENLgeneral <- ENLgeneral %>% mutate(P413_1 = as.numeric(P413_1)) %>%
    mutate(P413_1 = case_when(P413_1 == 1 ~ "a. Sí",
                              P413_1 == 0 ~ "b. No")) %>%
    mutate(P413_2 = as.numeric(P413_2)) %>% mutate(P413_2 = case_when(P413_2 == 1 ~ "a. Sí",
                                                                      P413_2 == 0 ~ "b. No")) %>%
    mutate(P413_3 = as.numeric(P413_3)) %>% mutate(P413_3 = case_when(P413_3 == 1 ~ "a. Sí",
                                                                      P413_3 == 0 ~ "b. No")) %>%
    mutate(P413_4 = as.numeric(P413_4)) %>% mutate(P413_4 = case_when(P413_4 == 1 ~ "a. Sí",
                                                                      P413_4 == 0 ~ "b. No")) %>%
    mutate(P413_5 = as.numeric(P413_5)) %>% mutate(P413_5 = case_when(P413_5 == 1 ~ "a. Sí",
                                                                      P413_5 == 0 ~ "b. No")) %>%
    mutate(P413_6 = as.numeric(P413_6)) %>% mutate(P413_6 = case_when(P413_6 == 1 ~ "a. Sí",
                                                                      P413_6 == 0 ~ "b. No")) %>%
    mutate(P413_7 = as.numeric(P413_7)) %>% mutate(P413_7 = case_when(P413_7 == 1 ~ "a. Sí",
                                                                      P413_7 == 0 ~ "b. No")) %>%
    mutate(P413_8 = as.numeric(P413_8)) %>% mutate(P413_8 = case_when(P413_8 == 1 ~ "a. Sí",
                                                                      P413_8 == 0 ~ "b. No")) %>%
    mutate(P413_9 = as.numeric(P413_9)) %>% mutate(P413_9 = case_when(P413_9 == 1 ~ "a. Sí",
                                                                      P413_9 == 0 ~ "b. No")) %>%
    mutate(P413_10 = as.numeric(P413_10)) %>% mutate(P413_10 = case_when(P413_10 == 1 ~ "a. Sí",
                                                                         P413_10 == 0 ~ "b. No")) %>%
    mutate(P413_11 = as.numeric(P413_11)) %>% mutate(P413_11 = case_when(P413_11 == 1 ~ "a. Sí",
                                                                         P413_11 == 0 ~ "b. No")) %>%
    mutate(P413_12 = as.numeric(P413_12)) %>% mutate(P413_12 = case_when(P413_12 == 1 ~ "a. Sí",
                                                                         P413_12 == 0 ~ "b. No")) %>%
    mutate(P413_13 = as.numeric(P413_13)) %>% mutate(P413_13 = case_when(P413_13 == 1 ~ "a. Sí",
                                                                         P413_13 == 0 ~ "b. No")) %>%
    mutate(P413_14 = as.numeric(P413_14)) %>% mutate(P413_14 = case_when(P413_14 == 1 ~ "a. Sí",
                                                                         P413_14 == 0 ~ "b. No")) %>%
    mutate(P413_15 = as.numeric(P413_15)) %>% mutate(P413_15 = case_when(P413_15 == 1 ~ "a. Sí",
                                                                         P413_15 == 0 ~ "b. No")) %>%
    mutate(P413_16 = as.numeric(P413_16)) %>% mutate(P413_16 = case_when(P413_16 == 1 ~ "a. Sí",
                                                                         P413_16 == 0 ~ "b. No")) %>%
    
    mutate(P413_17 = as.numeric(P413_17)) %>% mutate(P413_17 = case_when(P413_17 == 1 ~ "a. Sí",
                                                                         P413_17 == 0 ~ "b. No")) %>%
    mutate(P413_18 = as.numeric(P413_18)) %>% mutate(P413_18 = case_when(P413_18 == 1 ~ "a. Sí",
                                                                         P413_18 == 0 ~ "b. No")) %>%
    mutate(P413_20 = as.numeric(P413_20)) %>% mutate(P413_20 = case_when(P413_20 == 1 ~ "a. Sí",
                                                                         P413_20 == 0 ~ "b. No"))
  
  ENLgeneral413_1 <- ENLgeneral %>% select(P408, P413 = P413_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Libros infantiles")
  ENLgeneral413_2 <- ENLgeneral %>% select(P408, P413 = P413_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Libros juveniles")
  ENLgeneral413_3 <- ENLgeneral %>% select(P408, P413 = P413_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Enciclopedias y diccionarios")
  ENLgeneral413_4 <- ENLgeneral %>% select(P408, P413 = P413_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Libros de textos escolares o universitarios")
  ENLgeneral413_5 <- ENLgeneral %>% select(P408, P413 = P413_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Manuales o guías metodológicas")
  ENLgeneral413_6 <- ENLgeneral %>% select(P408, P413 = P413_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Cocina")
  ENLgeneral413_7 <- ENLgeneral %>% select(P408, P413 = P413_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Religión")
  ENLgeneral413_8 <- ENLgeneral %>% select(P408, P413 = P413_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Filosofía y psicología")
  ENLgeneral413_9 <- ENLgeneral %>% select(P408, P413 = P413_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Superación personal/autoayuda")
  ENLgeneral413_10 <- ENLgeneral %>% select(P408, P413 = P413_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Ciencias sociales")
  ENLgeneral413_11 <- ENLgeneral %>% select(P408, P413 = P413_11, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. Idiomas/Gramática/Lenguaje")
  ENLgeneral413_12 <- ENLgeneral %>% select(P408, P413 = P413_12, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "l. Ciencias naturales y matemáticas")
  ENLgeneral413_13 <- ENLgeneral %>% select(P408, P413 = P413_13, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "m. Geografía e historia")
  ENLgeneral413_14 <- ENLgeneral %>% select(P408, P413 = P413_14, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "n. Tecnología (ciencias aplicadas)")
  ENLgeneral413_15 <- ENLgeneral %>% select(P408, P413 = P413_15, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "o. Arte, diseño y decoración")
  
  ENLgeneral413_16 <- ENLgeneral %>% select(P408, P413 = P413_16, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "p. Literatura")
  ENLgeneral413_17 <- ENLgeneral %>% select(P408, P413 = P413_17, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "q. Biografía o memorias")
  ENLgeneral413_18 <- ENLgeneral %>% select(P408, P413 = P413_18, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "r. Ensayo/crónica")
  ENLgeneral413_20 <- ENLgeneral %>% select(P408, P413 = P413_20, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "s. Otro")
  
  P413 <- bind_rows(ENLgeneral413_1, ENLgeneral413_2, ENLgeneral413_3, ENLgeneral413_4, ENLgeneral413_5, ENLgeneral413_6, ENLgeneral413_7, ENLgeneral413_8, ENLgeneral413_9, ENLgeneral413_10,
                    ENLgeneral413_11,	ENLgeneral413_12,	ENLgeneral413_13,	ENLgeneral413_14, ENLgeneral413_15, ENLgeneral413_16, ENLgeneral413_17, ENLgeneral413_18, ENLgeneral413_20)
  %>%
    filter(!is.na(P413)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P408 == "a. Sí")
  
  rm(ENLgeneral413_1, ENLgeneral413_2, ENLgeneral413_3, ENLgeneral413_4, ENLgeneral413_5, ENLgeneral413_6, ENLgeneral413_7, ENLgeneral413_8, ENLgeneral413_9, ENLgeneral413_10,
     ENLgeneral413_11,	ENLgeneral413_12,	ENLgeneral413_13,	ENLgeneral413_14, ENLgeneral413_15, ENLgeneral413_16, ENLgeneral413_17, ENLgeneral413_18, ENLgeneral413_20)
  
  P413sexo <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P413edad_group <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P413niv_educ <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P413auto_etnia <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P413estrasoc <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P413area <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P413zona <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P413dom <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P413region_nat <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P413dep <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P413)) %>%
    ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P413total <- P413 %>%
    select(P413, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P413 != "NA") %>%
    group_by(P413, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P413)) %>% ungroup %>%
    spread(P413, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P413 <- bind_rows(P413total, P413sexo, P413edad_group, P413niv_educ, P413auto_etnia, P413estrasoc, P413area, P413zona, P413dom, P413region_nat, P413dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P413total, P413sexo, P413edad_group, P413niv_educ, P413auto_etnia, P413estrasoc, P413area, P413zona, P413dom, P413region_nat, P413dep)
  
  P413 <- P413 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P414: Principales razones por las que lee un Libro
{
  ENLgeneral <- ENLgeneral %>% mutate(P414_1 = as.numeric(P414_1)) %>%
    mutate(P414_1 = case_when(P414_1 == 1 ~ "a. Sí",
                              
                              P414_1 == 0 ~ "b. No")) %>%
    mutate(P414_2 = as.numeric(P414_2)) %>% mutate(P414_2 = case_when(P414_2 == 1 ~ "a. Sí",
                                                                      P414_2 == 0 ~ "b. No")) %>%
    mutate(P414_3 = as.numeric(P414_3)) %>% mutate(P414_3 = case_when(P414_3 == 1 ~ "a. Sí",
                                                                      P414_3 == 0 ~ "b. No")) %>%
    mutate(P414_4 = as.numeric(P414_4)) %>% mutate(P414_4 = case_when(P414_4 == 1 ~ "a. Sí",
                                                                      P414_4 == 0 ~ "b. No")) %>%
    mutate(P414_5 = as.numeric(P414_5)) %>% mutate(P414_5 = case_when(P414_5 == 1 ~ "a. Sí",
                                                                      P414_5 == 0 ~ "b. No")) %>%
    mutate(P414_6 = as.numeric(P414_6)) %>% mutate(P414_6 = case_when(P414_6 == 1 ~ "a. Sí",
                                                                      P414_6 == 0 ~ "b. No")) %>%
    mutate(P414_7 = as.numeric(P414_7)) %>% mutate(P414_7 = case_when(P414_7 == 1 ~ "a. Sí",
                                                                      P414_7 == 0 ~ "b. No")) %>%
    mutate(P414_8 = as.numeric(P414_8)) %>% mutate(P414_8 = case_when(P414_8 == 1 ~ "a. Sí",
                                                                      P414_8 == 0 ~ "b. No")) %>%
    mutate(P414_9 = as.numeric(P414_9)) %>% mutate(P414_9 = case_when(P414_9 == 1 ~ "a. Sí",
                                                                      P414_9 == 0 ~ "b. No")) %>%
    mutate(P414_10 = as.numeric(P414_10)) %>% mutate(P414_10 = case_when(P414_10 == 1 ~ "a. Sí",
                                                                         P414_10 == 0 ~ "b. No")) %>%
    mutate(P414_11 = as.numeric(P414_11)) %>% mutate(P414_11 = case_when(P414_11 == 1 ~ "a. Sí",
                                                                         P414_11 == 0 ~ "b. No")) %>%
    mutate(P414_12 = as.numeric(P414_12)) %>% mutate(P414_12 = case_when(P414_12 == 1 ~ "a. Sí",
                                                                         P414_12 == 0 ~ "b. No")) %>%
    mutate(P414_13 = as.numeric(P414_13)) %>% mutate(P414_13 = case_when(P414_13 == 1 ~ "a. Sí",
                                                                         P414_13 == 0 ~ "b. No"))
  
  ENLgeneral414_1 <- ENLgeneral %>% select(P408, P414 = P414_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. El autor/la autora")
  ENLgeneral414_2 <- ENLgeneral %>% select(P408, P414 = P414_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. El título")
  ENLgeneral414_3 <- ENLgeneral %>% select(P408, P414 = P414_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. El tema")
  ENLgeneral414_4 <- ENLgeneral %>% select(P408, P414 = P414_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. La recomendación de un amigo/a o familiar")
  ENLgeneral414_5 <- ENLgeneral %>% select(P408, P414 = P414_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. La recomendación de un/a bibliotecario/a – mediador/a")
  
  ENLgeneral414_6 <- ENLgeneral %>% select(P408, P414 = P414_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. La recomendación de un/a librero/a")
  ENLgeneral414_7 <- ENLgeneral %>% select(P408, P414 = P414_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. La recomendación del docente/profesor/a")
  ENLgeneral414_8 <- ENLgeneral %>% select(P408, P414 = P414_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Por la presentación (carátula, diseño, libro-álbum)")
  ENLgeneral414_9 <- ENLgeneral %>% select(P408, P414 = P414_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. El precio")
  ENLgeneral414_10 <- ENLgeneral %>% select(P408, P414 = P414_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Por comentarios y/o anuncios en prensa, radio o televisión")
  ENLgeneral414_11 <- ENLgeneral %>% select(P408, P414 = P414_11, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. Recomendaciones por redes sociales y/o sitios web Especializados")
  ENLgeneral414_12 <- ENLgeneral %>% select(P408, P414 = P414_12, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "l. Otro")
  ENLgeneral414_13 <- ENLgeneral %>% select(P408, P414 = P414_13, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "m. Solo leo libros por motivos de trabajo y/o estudio")
  P414 <- bind_rows(ENLgeneral414_1, ENLgeneral414_2, ENLgeneral414_3, ENLgeneral414_4, ENLgeneral414_5, ENLgeneral414_6, ENLgeneral414_7, ENLgeneral414_8, ENLgeneral414_9, ENLgeneral414_10,
                    ENLgeneral414_11, ENLgeneral414_12, ENLgeneral414_13) %>% filter(!is.na(P414)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P408 == "a. Sí")
  
  rm(ENLgeneral414_1, ENLgeneral414_2, ENLgeneral414_3, ENLgeneral414_4, ENLgeneral414_5, ENLgeneral414_6, ENLgeneral414_7, ENLgeneral414_8, ENLgeneral414_9, ENLgeneral414_10,
     ENLgeneral414_11, ENLgeneral414_12, ENLgeneral414_13)
  
  P414sexo <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P414edad_group <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P414niv_educ <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P414auto_etnia <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P414estrasoc <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P414area <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P414zona <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P414dom <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P414region_nat <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P414dep <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P414)) %>%
    ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P414total <- P414 %>%
    select(P414, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P414 != "NA") %>%
    group_by(P414, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P414)) %>% ungroup %>%
    spread(P414, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P414 <- bind_rows(P414total, P414sexo, P414edad_group, P414niv_educ, P414auto_etnia, P414estrasoc, P414area, P414zona, P414dom, P414region_nat, P414dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P414total, P414sexo, P414edad_group, P414niv_educ, P414auto_etnia, P414estrasoc, P414area, P414zona, P414dom, P414region_nat, P414dep)
  
  P414 <- P414 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P415: Consiguió libros pagados o gratuitos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P415 = case_when(P415 == 1 ~ "a. Sí",
                            P415 == 2 ~ "b. No"))
  
  P415 <- ENLgeneral %>%
    select(P415, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P415sexo <- P415 %>%
    select(P415, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P415edad_group <- P415 %>%
    select(P415, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P415niv_educ <- P415 %>%
    select(P415, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P415auto_etnia <- P415 %>%
    select(P415, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P415estrasoc <- P415 %>%
    select(P415, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P415area <- P415 %>%
    select(P415, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P415zona <- P415 %>%
    select(P415, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P415dom <- P415 %>%
    select(P415, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P415region_nat <- P415 %>%
    select(P415, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P415dep <- P415 %>%
    select(P415, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P415total <- P415 %>%
    select(P415, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P415 <- bind_rows(P415total, P415sexo, P415edad_group, P415niv_educ, P415auto_etnia, P415estrasoc, P415area, P415zona, P415dom, P415region_nat, P415dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P415total, P415sexo, P415edad_group, P415niv_educ, P415auto_etnia, P415estrasoc, P415area, P415zona, P415dom, P415region_nat, P415dep)
  
  P415 <- P415 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P415: Consiguió libros pagados o gratuitos
{
  P415sabeleer <- ENLgeneral %>%
    select(P415, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P415sexo <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P415edad_group <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P415niv_educ <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P415auto_etnia <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P415estrasoc <- P415sabeleer %>%
    
    select(P415, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P415area <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P415zona <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P415dom <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P415region_nat <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P415dep <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P415total <- P415sabeleer %>%
    select(P415, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P415 != "NA") %>%
    group_by(P415, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P415)) %>% ungroup %>%
    spread(P415, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P415sabeleer <- bind_rows(P415total, P415sexo, P415edad_group, P415niv_educ, P415auto_etnia, P415estrasoc, P415area, P415zona, P415dom, P415region_nat, P415dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P415total, P415sexo, P415edad_group, P415niv_educ, P415auto_etnia, P415estrasoc, P415area, P415zona, P415dom, P415region_nat, P415dep)
  
  P415sabeleer <- P415sabeleer %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P416: Como consiguió los libros
{
  ENLgeneral <- ENLgeneral %>% mutate(P416_1 = as.numeric(P416_1)) %>%
    mutate(P416_1 = case_when(P416_1 == 1 ~ "a. Sí",
                              P416_1 == 0 ~ "b. No")) %>%
    mutate(P416_2 = as.numeric(P416_2)) %>% mutate(P416_2 = case_when(P416_2 == 1 ~ "a. Sí",
                                                                      P416_2 == 0 ~ "b. No")) %>%
    mutate(P416_3 = as.numeric(P416_3)) %>% mutate(P416_3 = case_when(P416_3 == 1 ~ "a. Sí",
                                                                      P416_3 == 0 ~ "b. No")) %>%
    mutate(P416_4 = as.numeric(P416_4)) %>% mutate(P416_4 = case_when(P416_4 == 1 ~ "a. Sí",
                                                                      P416_4 == 0 ~ "b. No")) %>%
    mutate(P416_5 = as.numeric(P416_5)) %>% mutate(P416_5 = case_when(P416_5 == 1 ~ "a. Sí",
                                                                      P416_5 == 0 ~ "b. No")) %>%
    mutate(P416_6 = as.numeric(P416_6)) %>% mutate(P416_6 = case_when(P416_6 == 1 ~ "a. Sí",
                                                                      P416_6 == 0 ~ "b. No")) %>%
    mutate(P416_8 = as.numeric(P416_8)) %>% mutate(P416_8 = case_when(P416_8 == 1 ~ "a. Sí",
                                                                      P416_8 == 0 ~ "b. No")) %>%
    mutate(P416_9 = as.numeric(P416_9)) %>% mutate(P416_9 = case_when(P416_9 == 1 ~ "a. Sí",
                                                                      P416_9 == 0 ~ "b. No")) %>%
    mutate(P416_10 = as.numeric(P416_10)) %>% mutate(P416_10 = case_when(P416_10 == 1 ~ "a. Sí",
                                                                         P416_10 == 0 ~ "b. No"))
  
  ENLgeneral416_1 <- ENLgeneral %>% select(P415, P416 = P416_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Los compró")
  ENLgeneral416_2 <- ENLgeneral %>% select(P415, P416 = P416_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Pagó por la fotocopia de los libros")
  ENLgeneral416_3 <- ENLgeneral %>% select(P415, P416 = P416_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Se los regalaron")
  ENLgeneral416_4 <- ENLgeneral %>% select(P415, P416 = P416_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Se los prestaron")
  
  ENLgeneral416_5 <- ENLgeneral %>% select(P415, P416 = P416_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Los pidió prestados en bibliotecas")
  ENLgeneral416_6 <- ENLgeneral %>% select(P415, P416 = P416_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Los descargó gratuitamente de internet")
  ENLgeneral416_8 <- ENLgeneral %>% select(P415, P416 = P416_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Por suscripción gratuita en plataformas digitales? con licencia de autor")
  ENLgeneral416_9 <- ENLgeneral %>% select(P415, P416 = P416_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Los tenía en su biblioteca personal (física o virtual)")
  ENLgeneral416_10 <- ENLgeneral %>% select(P415, P416 = P416_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Otro")
  
  P416 <- bind_rows(ENLgeneral416_1, ENLgeneral416_2, ENLgeneral416_3, ENLgeneral416_4, ENLgeneral416_5, ENLgeneral416_6, ENLgeneral416_8, ENLgeneral416_9, ENLgeneral416_10) %>%
    filter(!is.na(P416)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P415 == "a. Sí")
  
  rm(ENLgeneral416_1, ENLgeneral416_2, ENLgeneral416_3, ENLgeneral416_4, ENLgeneral416_5, ENLgeneral416_6, ENLgeneral416_8, ENLgeneral416_9, ENLgeneral416_10)
  
  P416sexo <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P416edad_group <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>%
    
    mutate(Variables = "Grupos de edad")
  
  P416niv_educ <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P416auto_etnia <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P416estrasoc <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P416area <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P416zona <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P416dom <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P416region_nat <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>%
    
    mutate(Variables = "Region natural")
  
  P416dep <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P416)) %>%
    ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P416total <- P416 %>%
    select(P416, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P416 != "NA") %>%
    group_by(P416, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P416)) %>% ungroup %>%
    spread(P416, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P416 <- bind_rows(P416total, P416sexo, P416edad_group, P416niv_educ, P416auto_etnia, P416estrasoc, P416area, P416zona, P416dom, P416region_nat, P416dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P416total, P416sexo, P416edad_group, P416niv_educ, P416auto_etnia, P416estrasoc, P416area, P416zona, P416dom, P416region_nat, P416dep)
  
  P416 <- P416 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P417: Donde compro los libros
{
  ENLgeneral <- ENLgeneral %>% mutate(P417_1 = as.numeric(P417_1)) %>%
    mutate(P417_1 = case_when(P417_1 == 1 ~ "a. Sí",
                              P417_1 == 0 ~ "b. No")) %>%
    mutate(P417_2 = as.numeric(P417_2)) %>% mutate(P417_2 = case_when(P417_2 == 1 ~ "a. Sí",
                                                                      
                                                                      P417_2 == 0 ~ "b. No")) %>%
    mutate(P417_3 = as.numeric(P417_3)) %>% mutate(P417_3 = case_when(P417_3 == 1 ~ "a. Sí",
                                                                      P417_3 == 0 ~ "b. No")) %>%
    mutate(P417_4 = as.numeric(P417_4)) %>% mutate(P417_4 = case_when(P417_4 == 1 ~ "a. Sí",
                                                                      P417_4 == 0 ~ "b. No")) %>%
    mutate(P417_5 = as.numeric(P417_5)) %>% mutate(P417_5 = case_when(P417_5 == 1 ~ "a. Sí",
                                                                      P417_5 == 0 ~ "b. No")) %>%
    mutate(P417_6 = as.numeric(P417_6)) %>% mutate(P417_6 = case_when(P417_6 == 1 ~ "a. Sí",
                                                                      P417_6 == 0 ~ "b. No")) %>%
    mutate(P417_7 = as.numeric(P417_7)) %>% mutate(P417_7 = case_when(P417_7 == 1 ~ "a. Sí",
                                                                      P417_7 == 0 ~ "b. No")) %>%
    mutate(P417_8 = as.numeric(P417_8)) %>% mutate(P417_8 = case_when(P417_8 == 1 ~ "a. Sí",
                                                                      P417_8 == 0 ~ "b. No")) %>%
    mutate(P417_9 = as.numeric(P417_9)) %>% mutate(P417_9 = case_when(P417_9 == 1 ~ "a. Sí",
                                                                      P417_9 == 0 ~ "b. No")) %>%
    mutate(P417_10 = as.numeric(P417_10)) %>% mutate(P417_10 = case_when(P417_10 == 1 ~ "a. Sí",
                                                                         P417_10 == 0 ~ "b. No"))
  
  ENLgeneral417_1 <- ENLgeneral %>% select(P417 = P417_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Librerías físicas")
  ENLgeneral417_2 <- ENLgeneral %>% select(P417 = P417_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Vendedores ambulantes o puntos de venta de libros copiados")
  ENLgeneral417_3 <- ENLgeneral %>% select(P417 = P417_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Librerías físicas de segunda mano ")
  ENLgeneral417_4 <- ENLgeneral %>% select(P417 = P417_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Almacenes de cadena / Supermercados (tienda física o virtual)")
  ENLgeneral417_5 <- ENLgeneral %>% select(P417 = P417_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Ferias del libro presencialeso")
  ENLgeneral417_6 <- ENLgeneral %>% select(P417 = P417_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Quioscos físicos ")
  ENLgeneral417_7 <- ENLgeneral %>% select(P417 = P417_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Librerías digitales")
  ENLgeneral417_8 <- ENLgeneral %>% select(P417 = P417_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Tiendas virtuales en Ferias del Libro")
  ENLgeneral417_9 <- ENLgeneral %>% select(P417 = P417_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Por suscripción paga a plataformas digitales")
  
  ENLgeneral417_10 <- ENLgeneral %>% select(P417 = P417_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Otro")
  
  P417 <- bind_rows(ENLgeneral417_1, ENLgeneral417_2, ENLgeneral417_3, ENLgeneral417_4, ENLgeneral417_5, ENLgeneral417_6, ENLgeneral417_7, ENLgeneral417_8, ENLgeneral417_9, ENLgeneral417_10) %>%
    filter(!is.na(P417)) %>% filter(edad >= 18 & edad <= 64)
  
  rm(ENLgeneral417_1, ENLgeneral417_2, ENLgeneral417_3, ENLgeneral417_4, ENLgeneral417_5, ENLgeneral417_6, ENLgeneral417_7, ENLgeneral417_8, ENLgeneral417_9, ENLgeneral417_10)
  
  P417sexo <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P417edad_group <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P417niv_educ <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P417auto_etnia <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P417estrasoc <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P417area <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P417zona <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P417dom <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P417region_nat <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P417dep <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P417)) %>%
    ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P417total <- P417 %>%
    select(P417, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P417 != "NA") %>%
    group_by(P417, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P417)) %>% ungroup %>%
    spread(P417, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P417 <- bind_rows(P417total, P417sexo, P417edad_group, P417niv_educ, P417auto_etnia, P417estrasoc, P417area, P417zona, P417dom, P417region_nat, P417dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P417total, P417sexo, P417edad_group, P417niv_educ, P417auto_etnia, P417estrasoc, P417area, P417zona, P417dom, P417region_nat, P417dep)
  
  P417 <- P417 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P418: Cuanto pagó por los libros adquiridos para usted u otras personas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P418 = case_when(between(P418, 1, 50) ~ "a. 1 a 50 soles",
                            between(P418, 51, 100) ~ "b. 51 a 100 soles",
                            between(P418, 101, 500) ~ "c. 101 a 500 soles", P418 >= 501 ~ "d. más de 500 soles"))
  
  P418<- ENLgeneral %>%
    select(P418, P418_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P418sexo <- P418 %>%
    select(P418, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P418edad_group <- P418 %>%
    select(P418, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P418niv_educ <- P418 %>%
    select(P418, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P418auto_etnia <- P418 %>%
    select(P418, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P418estrasoc <- P418 %>%
    select(P418, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P418area <- P418 %>%
    select(P418, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P418zona <- P418 %>%
    select(P418, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P418dom <- P418 %>%
    select(P418, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P418region_nat <- P418 %>%
    select(P418, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P418dep <- P418 %>%
    select(P418, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P418total <- P418 %>%
    select(P418, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P418 != "NA") %>%
    group_by(P418, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P418)) %>% ungroup %>%
    spread(P418, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>%
    
    filter(Desagregacion == "Total")
  
  P418 <- bind_rows(P418total, P418sexo, P418edad_group, P418niv_educ, P418auto_etnia, P418estrasoc, P418area, P418zona, P418dom, P418region_nat, P418dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P418total, P418sexo, P418edad_group, P418niv_educ, P418auto_etnia, P418estrasoc, P418area, P418zona, P418dom, P418region_nat, P418dep)
  
  P418 <- P418 %>%
    mutate(`a. 1 a 50 soles (%)` = round (100 * `a. 1 a 50 soles` / `Total`, 1)) %>%
    mutate(`b. 51 a 100 soles (%)` = round (100 * `b. 51 a 100 soles` / `Total`, 1)) %>%
    mutate(`c. 101 a 500 soles (%)` = round (100 * `c. 101 a 500 soles` / `Total`, 1)) %>% mutate(`d. más de 500 soles (%)` = round (100 * `d. más de 500 soles` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}

# C. LECTURA DE PERIÓDICOS
#****************************************************            #* P419_1_2: Usted ha leído Periódicos impresos y/o digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P419 = case_when(P419_1 == 1 & P419_2 == 1 ~ "a. Sí", P419_1 == 2 & P419_2 == 1 ~ "a. Sí",
                            P419_1 == 1 & P419_2 == 2 ~ "a. Sí", P419_1 == 2 & P419_2 == 2 ~ "b. No"))
  
  P419 <- ENLgeneral %>%
    select(P419, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P419sexo <- P419 %>%
    select(P419, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P419edad_group <- P419 %>%
    select(P419, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>%
    
    ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P419niv_educ <- P419 %>%
    select(P419, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P419auto_etnia <- P419 %>%
    select(P419, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P419estrasoc <- P419 %>%
    select(P419, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>%
    
    mutate(Variables = "Estrato socioeconómico")
  
  P419area <- P419 %>%
    select(P419, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P419zona <- P419 %>%
    select(P419, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P419dom <- P419 %>%
    select(P419, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P419region_nat <- P419 %>%
    select(P419, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>%
    
    ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P419dep <- P419 %>%
    select(P419, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P419total <- P419 %>%
    select(P419, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P419 != "NA") %>%
    group_by(P419, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419)) %>% ungroup %>%
    spread(P419, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P419 <- bind_rows(P419total, P419sexo, P419edad_group, P419niv_educ, P419auto_etnia, P419estrasoc, P419area, P419zona, P419dom, P419region_nat, P419dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P419total, P419sexo, P419edad_group, P419niv_educ, P419auto_etnia, P419estrasoc, P419area, P419zona, P419dom, P419region_nat, P419dep)
  
  P419 <- P419 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

#* P419_1: Usted ha leído Periódicos impresos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P419_1 = case_when(P419_1 == 1 ~ "a. Sí",
                              P419_1 == 2 ~ "b. No"))
  
  P419_1 <- ENLgeneral %>%
    select(P419_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P419_1sexo <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P419_1edad_group <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P419_1niv_educ <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P419_1auto_etnia <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P419_1estrasoc <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P419_1area <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P419_1zona <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P419_1dom <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P419_1region_nat <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P419_1dep <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P419_1total <- P419_1 %>%
    select(P419_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_1 != "NA") %>%
    group_by(P419_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_1)) %>% ungroup %>%
    spread(P419_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P419_1 <- bind_rows(P419_1total, P419_1sexo, P419_1edad_group, P419_1niv_educ, P419_1auto_etnia, P419_1estrasoc, P419_1area, P419_1zona, P419_1dom, P419_1region_nat, P419_1dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P419_1total, P419_1sexo, P419_1edad_group, P419_1niv_educ, P419_1auto_etnia, P419_1estrasoc, P419_1area, P419_1zona, P419_1dom, P419_1region_nat, P419_1dep)
  
  P419_1 <- P419_1 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P419_2: Usted ha leído Periódicos digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P419_2 = case_when(P419_2 == 1 ~ "a. Sí",
                              P419_2 == 2 ~ "b. No"))
  
  P419_2 <- ENLgeneral %>%
    select(P419_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P419_2sexo <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P419_2edad_group <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P419_2niv_educ <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P419_2auto_etnia <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P419_2estrasoc <- P419_2 %>%
    
    select(P419_2, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P419_2area <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P419_2zona <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P419_2dom <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P419_2region_nat <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P419_2dep <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P419_2total <- P419_2 %>%
    select(P419_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P419_2 != "NA") %>%
    group_by(P419_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P419_2)) %>% ungroup %>%
    spread(P419_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P419_2 <- bind_rows(P419_2total, P419_2sexo, P419_2edad_group, P419_2niv_educ, P419_2auto_etnia, P419_2estrasoc, P419_2area, P419_2zona, P419_2dom, P419_2region_nat, P419_2dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P419_2total, P419_2sexo, P419_2edad_group, P419_2niv_educ, P419_2auto_etnia, P419_2estrasoc, P419_2area, P419_2zona, P419_2dom, P419_2region_nat, P419_2dep)
  
  P419_2 <- P419_2 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P420: Principales razones por las que no leyó Periódicos
{
  ENLgeneral <- ENLgeneral %>% mutate(P420_1 = as.numeric(P420_1)) %>%
    mutate(P420_1 = case_when(P420_1 == 1 ~ "a. Sí",
                              P420_1 == 0 ~ "b. No")) %>%
    mutate(P420_2 = as.numeric(P420_2)) %>% mutate(P420_2 = case_when(P420_2 == 1 ~ "a. Sí",
                                                                      P420_2 == 0 ~ "b. No")) %>%
    mutate(P420_3 = as.numeric(P420_3)) %>% mutate(P420_3 = case_when(P420_3 == 1 ~ "a. Sí",
                                                                      P420_3 == 0 ~ "b. No")) %>%
    mutate(P420_4 = as.numeric(P420_4)) %>% mutate(P420_4 = case_when(P420_4 == 1 ~ "a. Sí",
                                                                      P420_4 == 0 ~ "b. No")) %>%
    mutate(P420_5 = as.numeric(P420_5)) %>% mutate(P420_5 = case_when(P420_5 == 1 ~ "a. Sí",
                                                                      P420_5 == 0 ~ "b. No")) %>%
    mutate(P420_6 = as.numeric(P420_6)) %>% mutate(P420_6 = case_when(P420_6 == 1 ~ "a. Sí",
                                                                      P420_6 == 0 ~ "b. No")) %>%
    mutate(P420_7 = as.numeric(P420_7)) %>% mutate(P420_7 = case_when(P420_7 == 1 ~ "a. Sí",
                                                                      P420_7 == 0 ~ "b. No")) %>%
    mutate(P420_8 = as.numeric(P420_8)) %>% mutate(P420_8 = case_when(P420_8 == 1 ~ "a. Sí",
                                                                      P420_8 == 0 ~ "b. No")) %>%
    mutate(P420_9 = as.numeric(P420_9)) %>% mutate(P420_9 = case_when(P420_9 == 1 ~ "a. Sí",
                                                                      P420_9 == 0 ~ "b. No")) %>%
    mutate(P420_10 = as.numeric(P420_10)) %>% mutate(P420_10 = case_when(P420_10 == 1 ~ "a. Sí",
                                                                         P420_10 == 0 ~ "b. No"))
  
  ENLgeneral420_1 <- ENLgeneral %>% select(P419, P420 = P420_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Prefiere realizar otras actividades culturales (cine, televisión, conciertos, museos, exposiciones, danzas, entre otros)")
  ENLgeneral420_2 <- ENLgeneral %>% select(P419, P420 = P420_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                           
                                           FACTOR200_FINAL) %>% mutate(Tipo = "b. Prefiere realizar otras actividades recreativas y/o sociales (deporte, visitas familiares, entre otros)")
  ENLgeneral420_3 <- ENLgeneral %>% select(P419, P420 = P420_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Prefiere leer otro tipo de publicación (libros, revistas, otros contenidos digitales) ")
  ENLgeneral420_4 <- ENLgeneral %>% select(P419, P420 = P420_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Por falta de tiempo")
  ENLgeneral420_5 <- ENLgeneral %>% select(P419, P420 = P420_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Por falta de dinero")
  ENLgeneral420_6 <- ENLgeneral %>% select(P419, P420 = P420_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Falta de bibliotecas cerca a su hogar o centro de estudios") ENLgeneral420_7 <- ENLgeneral %>% select(P419, P420 = P420_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                                                                                                                                                                                                                                                                                                               FACTOR200_FINAL) %>% mutate(Tipo = "g. No le gusta leer periódicos / Falta de interés") ENLgeneral420_8 <- ENLgeneral %>% select(P419, P420 = P420_8, sexo, edad_group, edad,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Los periódicos no están en su lengua materna")
  ENLgeneral420_9 <- ENLgeneral %>% select(P419, P420 = P420_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. No hay periódicos disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral420_10 <- ENLgeneral %>% select(P419, P420 = P420_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Otro")
  
  P420 <- bind_rows(ENLgeneral420_1, ENLgeneral420_2, ENLgeneral420_3, ENLgeneral420_4, ENLgeneral420_5, ENLgeneral420_6, ENLgeneral420_7, ENLgeneral420_8, ENLgeneral420_9, ENLgeneral420_10) %>%
    filter(!is.na(P420)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P419 == "b. No")
  
  rm(ENLgeneral420_1, ENLgeneral420_2, ENLgeneral420_3, ENLgeneral420_4, ENLgeneral420_5, ENLgeneral420_6, ENLgeneral420_7, ENLgeneral420_8, ENLgeneral420_9, ENLgeneral420_10)
  
  P420sexo <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P420edad_group <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P420niv_educ <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P420auto_etnia <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P420estrasoc <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P420area <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P420zona <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P420dom <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P420region_nat <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P420dep <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P420)) %>%
    ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P420total <- P420 %>%
    select(P420, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P420 != "NA") %>%
    group_by(P420, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P420)) %>% ungroup %>%
    spread(P420, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P420 <- bind_rows(P420total, P420sexo, P420edad_group, P420niv_educ, P420auto_etnia, P420estrasoc, P420area, P420zona, P420dom, P420region_nat, P420dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P420total, P420sexo, P420edad_group, P420niv_educ, P420auto_etnia, P420estrasoc, P420area, P420zona, P420dom, P420region_nat, P420dep)
  
  P420 <- P420 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P421: Frecuencia con la que leyó Periódicos
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P421 = case_when(P421 == 1 ~ "a. Diariamente", P421 == 2 ~ "b. Varias veces a la semana", P421 == 3 ~ "c. Una vez a la semana", P421 == 4 ~ "d. Varias veces al mes", P421 == 5 ~ "e. Una vez al mes"))
  
  P421 <- ENLgeneral %>%
    select(P419, P421, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P419 == "a. Sí")
  
  P421sexo <- P421 %>%
    select(P421, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P421edad_group <- P421 %>%
    select(P421, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P421niv_educ <- P421 %>%
    select(P421, FACTOR200_FINAL, niv_educ) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P421auto_etnia <- P421 %>%
    select(P421, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P421estrasoc <- P421 %>%
    select(P421, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P421area <- P421 %>%
    select(P421, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P421zona <- P421 %>%
    select(P421, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P421dom <- P421 %>%
    select(P421, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P421region_nat <- P421 %>%
    select(P421, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P421dep <- P421 %>%
    select(P421, FACTOR200_FINAL, NOMBREDD) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P421total <- P421 %>%
    select(P421, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P421 != "NA") %>%
    group_by(P421, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P421)) %>% ungroup %>%
    spread(P421, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P421 <- bind_rows(P421total, P421sexo, P421edad_group, P421niv_educ, P421auto_etnia, P421estrasoc, P421area, P421zona, P421dom, P421region_nat, P421dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P421total, P421sexo, P421edad_group, P421niv_educ, P421auto_etnia, P421estrasoc, P421area, P421zona, P421dom, P421region_nat, P421dep)
  
  P421 <- P421 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Varias veces al mes (%)` = round (100 * `d. Varias veces al mes` / `Total`, 1)) %>% mutate(`e. Una vez al mes (%)` = round (100 * `e. Una vez al mes` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P422: Tipo de contenido de periódicos que leyó
{
  ## Titulares
  ENLgeneral <- ENLgeneral %>% mutate(P422_1 = as.numeric(P422_1)) %>%
    mutate(P422_1 = case_when(P422_1 == 1 ~ "a. Sí",
                              P422_1 == 0 ~ "b. No")) %>%
    mutate(P422_2 = as.numeric(P422_2)) %>%
    
    mutate(P422_2 = case_when(P422_2 == 1 ~ "a. Sí",
                              P422_2 == 0 ~ "b. No")) %>%
    mutate(P422_3 = as.numeric(P422_3)) %>% mutate(P422_3 = case_when(P422_3 == 1 ~ "a. Sí",
                                                                      P422_3 == 0 ~ "b. No")) %>%
    mutate(P422_4 = as.numeric(P422_4)) %>% mutate(P422_4 = case_when(P422_4 == 1 ~ "a. Sí",
                                                                      P422_4 == 0 ~ "b. No")) %>%
    mutate(P422_5 = as.numeric(P422_5)) %>% mutate(P422_5 = case_when(P422_5 == 1 ~ "a. Sí",
                                                                      P422_5 == 0 ~ "b. No")) %>%
    mutate(P422_6 = as.numeric(P422_6)) %>% mutate(P422_6 = case_when(P422_6 == 1 ~ "a. Sí",
                                                                      P422_6 == 0 ~ "b. No")) %>%
    mutate(P422_7 = as.numeric(P422_7)) %>% mutate(P422_7 = case_when(P422_7 == 1 ~ "a. Sí",
                                                                      P422_7 == 0 ~ "b. No")) %>%
    mutate(P422_8 = as.numeric(P422_8)) %>% mutate(P422_8 = case_when(P422_8 == 1 ~ "a. Sí",
                                                                      P422_8 == 0 ~ "b. No")) %>%
    mutate(P422_9 = as.numeric(P422_9)) %>% mutate(P422_9 = case_when(P422_9 == 1 ~ "a. Sí",
                                                                      P422_9 == 0 ~ "b. No")) %>%
    mutate(P422_10 = as.numeric(P422_10)) %>% mutate(P422_10 = case_when(P422_10 == 1 ~ "a. Sí",
                                                                         P422_10 == 0 ~ "b. No")) %>%
    mutate(P422_11 = as.numeric(P422_11)) %>% mutate(P422_11 = case_when(P422_11 == 1 ~ "a. Sí",
                                                                         P422_11 == 0 ~ "b. No")) %>%
    mutate(P422_12 = as.numeric(P422_12)) %>% mutate(P422_12 = case_when(P422_12 == 1 ~ "a. Sí",
                                                                         P422_12 == 0 ~ "b. No")) %>%
    mutate(P422_13 = as.numeric(P422_13)) %>% mutate(P422_13 = case_when(P422_13 == 1 ~ "a. Sí",
                                                                         P422_13 == 0 ~ "b. No")) %>%
    mutate(P422_14 = as.numeric(P422_14)) %>% mutate(P422_14 = case_when(P422_14 == 1 ~ "a. Sí",
                                                                         P422_14 == 0 ~ "b. No"))
  
  ENLgeneral422_1 <- ENLgeneral %>% select(P419, P422 = P422_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Titulares")
  ENLgeneral422_2 <- ENLgeneral %>% select(P419, P422 = P422_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Noticias locales")
  ENLgeneral422_3 <- ENLgeneral %>% select(P419, P422 = P422_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Noticias nacionales ")
  ENLgeneral422_4 <- ENLgeneral %>% select(P419, P422 = P422_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Noticias internacionales")
  ENLgeneral422_5 <- ENLgeneral %>% select(P419, P422 = P422_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Opinión / editorial")
  
  ENLgeneral422_6 <- ENLgeneral %>% select(P419, P422 = P422_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Política ")
  ENLgeneral422_7 <- ENLgeneral %>% select(P419, P422 = P422_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Economía y negocios")
  ENLgeneral422_8 <- ENLgeneral %>% select(P419, P422 = P422_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Ciencia y tecnología")
  ENLgeneral422_9 <- ENLgeneral %>% select(P419, P422 = P422_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Medio ambiente")
  ENLgeneral422_10 <- ENLgeneral %>% select(P419, P422 = P422_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Deportes")
  ENLgeneral422_11 <- ENLgeneral %>% select(P419, P422 = P422_11, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. Cultura y entretenimiento")
  ENLgeneral422_12 <- ENLgeneral %>% select(P419, P422 = P422_12, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "l. Sociales/farándula")
  ENLgeneral422_13 <- ENLgeneral %>% select(P419, P422 = P422_13, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "m. Avisos clasificados")
  ENLgeneral422_14 <- ENLgeneral %>% select(P419, P422 = P422_14, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "n. Otro")
  P422 <- bind_rows(ENLgeneral422_1, ENLgeneral422_2, ENLgeneral422_3, ENLgeneral422_4, ENLgeneral422_5, ENLgeneral422_6, ENLgeneral422_7, ENLgeneral422_8, ENLgeneral422_9, ENLgeneral422_10,
                    ENLgeneral422_11, ENLgeneral422_12, ENLgeneral422_13, ENLgeneral422_14) %>% filter(!is.na(P422)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P419 == "a. Sí")
  
  rm(ENLgeneral422_1, ENLgeneral422_2, ENLgeneral422_3, ENLgeneral422_4, ENLgeneral422_5, ENLgeneral422_6, ENLgeneral422_7, ENLgeneral422_8, ENLgeneral422_9, ENLgeneral422_10,
     ENLgeneral422_11, ENLgeneral422_12, ENLgeneral422_13, ENLgeneral422_14)
  
  P422sexo <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P422edad_group <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P422niv_educ <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P422auto_etnia <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P422estrasoc <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P422area <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P422zona <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P422dom <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P422region_nat <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P422dep <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P422)) %>%
    ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P422total <- P422 %>%
    select(P422, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P422 != "NA") %>%
    group_by(P422, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P422)) %>% ungroup %>%
    spread(P422, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P422 <- bind_rows(P422total, P422sexo, P422edad_group, P422niv_educ, P422auto_etnia, P422estrasoc, P422area, P422zona, P422dom, P422region_nat, P422dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P422total, P422sexo, P422edad_group, P422niv_educ, P422auto_etnia, P422estrasoc, P422area, P422zona, P422dom, P422region_nat, P422dep)
  
  P422 <- P422 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P423: Cuanto pagó por los periódicos adquiridos para usted u otras personas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P423 = case_when(between(P423, 0.1, 5) ~ "a. 1 a 5 soles",
                            between(P423, 6, 10) ~ "b. 6 a 10 soles",
                            between(P423, 11, 50) ~ "c. 11 a 50 soles", P423 >= 51 ~ "d. mayor a 50 soles"))
  
  P423<- ENLgeneral %>%
    select(P423, P423_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P423sexo <- P423 %>%
    select(P423, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P423edad_group <- P423 %>%
    select(P423, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P423niv_educ <- P423 %>%
    select(P423, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P423auto_etnia <- P423 %>%
    select(P423, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P423estrasoc <- P423 %>%
    select(P423, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P423area <- P423 %>%
    select(P423, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(area) %>% adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P423zona <- P423 %>%
    select(P423, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P423dom <- P423 %>%
    select(P423, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P423region_nat <- P423 %>%
    select(P423, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P423dep <- P423 %>%
    select(P423, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P423total <- P423 %>%
    select(P423, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P423 != "NA") %>%
    group_by(P423, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P423)) %>% ungroup %>%
    spread(P423, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P423 <- bind_rows(P423total, P423sexo, P423edad_group, P423niv_educ, P423auto_etnia, P423estrasoc, P423area, P423zona, P423dom, P423region_nat, P423dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P423total, P423sexo, P423edad_group, P423niv_educ, P423auto_etnia, P423estrasoc, P423area, P423zona, P423dom, P423region_nat, P423dep)
  
  P423 <- P423 %>%
    mutate(`a. 1 a 5 soles (%)` = round (100 * `a. 1 a 5 soles` / `Total`, 1)) %>%
    mutate(`b. 6 a 10 soles (%)` = round (100 * `b. 6 a 10 soles` / `Total`, 1)) %>%
    mutate(`c. 11 a 50 soles (%)` = round (100 * `c. 11 a 50 soles` / `Total`, 1)) %>% mutate(`d. mayor a 50 soles (%)` = round (100 * `d. mayor a 50 soles` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}

# D. LECTURA DE REVISTAS
#****************************************************            #* P424_1_2: Usted ha leído Periódicos impresos y/o digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P424 = case_when(P424_1 == 1 & P424_2 == 1 ~ "a. Sí", P424_1 == 2 & P424_2 == 1 ~ "a. Sí",
                            P424_1 == 1 & P424_2 == 2 ~ "a. Sí", P424_1 == 2 & P424_2 == 2 ~ "b. No"))
  
  P424 <- ENLgeneral %>%
    select(P424, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P424sexo <- P424 %>%
    select(P424, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P424edad_group <- P424 %>%
    select(P424, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P424niv_educ <- P424 %>%
    select(P424, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P424auto_etnia <- P424 %>%
    
    select(P424, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P424estrasoc <- P424 %>%
    select(P424, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P424area <- P424 %>%
    select(P424, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P424zona <- P424 %>%
    select(P424, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P424dom <- P424 %>%
    select(P424, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P424region_nat <- P424 %>%
    select(P424, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P424dep <- P424 %>%
    select(P424, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P424total <- P424 %>%
    
    select(P424, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P424 != "NA") %>%
    group_by(P424, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424)) %>% ungroup %>%
    spread(P424, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P424 <- bind_rows(P424total, P424sexo, P424edad_group, P424niv_educ, P424auto_etnia, P424estrasoc, P424area, P424zona, P424dom, P424region_nat, P424dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P424total, P424sexo, P424edad_group, P424niv_educ, P424auto_etnia, P424estrasoc, P424area, P424zona, P424dom, P424region_nat, P424dep)
  
  P424 <- P424 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P424_1: Usted ha leído Revistas impresas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P424_1 = case_when(P424_1 == 1 ~ "a. Sí",
                              P424_1 == 2 ~ "b. No"))
  
  P424_1 <- ENLgeneral %>%
    select(P424_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P424_1sexo <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P424_1edad_group <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P424_1niv_educ <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P424_1auto_etnia <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P424_1estrasoc <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P424_1area <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P424_1zona <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P424_1dom <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P424_1region_nat <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P424_1dep <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P424_1total <- P424_1 %>%
    select(P424_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_1 != "NA") %>%
    group_by(P424_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_1)) %>% ungroup %>%
    spread(P424_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P424_1 <- bind_rows(P424_1total, P424_1sexo, P424_1edad_group, P424_1niv_educ, P424_1auto_etnia, P424_1estrasoc, P424_1area, P424_1zona, P424_1dom, P424_1region_nat, P424_1dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P424_1total,	P424_1sexo,	P424_1edad_group,	P424_1niv_educ,	P424_1auto_etnia, P424_1estrasoc, P424_1area, P424_1zona, P424_1dom, P424_1region_nat, P424_1dep)
  
  P424_1 <- P424_1 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P424_2: Usted ha leído Revistas digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P424_2 = case_when(P424_2 == 1 ~ "a. Sí",
                              P424_2 == 2 ~ "b. No"))
  
  P424_2 <- ENLgeneral %>%
    select(P424_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  P424_2sexo <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P424_2edad_group <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P424_2niv_educ <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P424_2auto_etnia <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P424_2estrasoc <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P424_2area <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P424_2zona <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P424_2dom <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P424_2region_nat <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P424_2dep <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P424_2total <- P424_2 %>%
    select(P424_2, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P424_2 != "NA") %>%
    group_by(P424_2, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P424_2)) %>% ungroup %>%
    spread(P424_2, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P424_2 <- bind_rows(P424_2total, P424_2sexo, P424_2edad_group, P424_2niv_educ, P424_2auto_etnia, P424_2estrasoc, P424_2area, P424_2zona, P424_2dom, P424_2region_nat, P424_2dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P424_2total, P424_2sexo, P424_2edad_group, P424_2niv_educ, P424_2auto_etnia, P424_2estrasoc, P424_2area, P424_2zona, P424_2dom, P424_2region_nat, P424_2dep)
  
  P424_2 <- P424_2 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P425: Principales razones por las que no leyó Revistas
{
  ENLgeneral <- ENLgeneral %>% mutate(P425_1 = as.numeric(P425_1)) %>%
    mutate(P425_1 = case_when(P425_1 == 1 ~ "a. Sí",
                              P425_1 == 0 ~ "b. No")) %>%
    mutate(P425_2 = as.numeric(P425_2)) %>% mutate(P425_2 = case_when(P425_2 == 1 ~ "a. Sí",
                                                                      P425_2 == 0 ~ "b. No")) %>%
    mutate(P425_3 = as.numeric(P425_3)) %>% mutate(P425_3 = case_when(P425_3 == 1 ~ "a. Sí",
                                                                      P425_3 == 0 ~ "b. No")) %>%
    mutate(P425_4 = as.numeric(P425_4)) %>%
    
    mutate(P425_4 = case_when(P425_4 == 1 ~ "a. Sí",
                              P425_4 == 0 ~ "b. No")) %>%
    mutate(P425_5 = as.numeric(P425_5)) %>% mutate(P425_5 = case_when(P425_5 == 1 ~ "a. Sí",
                                                                      P425_5 == 0 ~ "b. No")) %>%
    mutate(P425_6 = as.numeric(P425_6)) %>% mutate(P425_6 = case_when(P425_6 == 1 ~ "a. Sí",
                                                                      P425_6 == 0 ~ "b. No")) %>%
    mutate(P425_7 = as.numeric(P425_7)) %>% mutate(P425_7 = case_when(P425_7 == 1 ~ "a. Sí",
                                                                      P425_7 == 0 ~ "b. No")) %>%
    mutate(P425_8 = as.numeric(P425_8)) %>% mutate(P425_8 = case_when(P425_8 == 1 ~ "a. Sí",
                                                                      P425_8 == 0 ~ "b. No")) %>%
    mutate(P425_9 = as.numeric(P425_9)) %>% mutate(P425_9 = case_when(P425_9 == 1 ~ "a. Sí",
                                                                      P425_9 == 0 ~ "b. No")) %>%
    mutate(P425_10 = as.numeric(P425_10)) %>% mutate(P425_10 = case_when(P425_10 == 1 ~ "a. Sí",
                                                                         P425_10 == 0 ~ "b. No"))
  
  ENLgeneral425_1 <- ENLgeneral %>% select(P424, P425 = P425_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Prefiere realizar otras actividades culturales")
  ENLgeneral425_2 <- ENLgeneral %>% select(P424, P425 = P425_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Prefiere realizar otras actividades recreativas y/o sociales")
  ENLgeneral425_3 <- ENLgeneral %>% select(P424, P425 = P425_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Prefiere leer otro tipo de publicaciones ")
  ENLgeneral425_4 <- ENLgeneral %>% select(P424, P425 = P425_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Por falta de tiempo")
  ENLgeneral425_5 <- ENLgeneral %>% select(P424, P425 = P425_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Por falta de dinero")
  ENLgeneral425_6 <- ENLgeneral %>% select(P424, P425 = P425_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Falta de bibliotecas cerca a su hogar o centro de estudios") ENLgeneral425_7 <- ENLgeneral %>% select(P424, P425 = P425_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer,
                                                                                                                                                                                                                                                                                                                               FACTOR200_FINAL) %>% mutate(Tipo = "g. No le gusta leer revistas/ Falta de interés") ENLgeneral425_8 <- ENLgeneral %>% select(P424, P425 = P425_8, sexo, edad_group, edad,
                                                                                                                                                                                                                                                                                                                                                                                                                                                             niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Las revistas no están en su lengua materna")
  ENLgeneral425_9 <- ENLgeneral %>% select(P424, P425 = P425_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. No hay revistas disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral425_10 <- ENLgeneral %>% select(P424, P425 = P425_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Otro")
  
  P425 <- bind_rows(ENLgeneral425_1, ENLgeneral425_2, ENLgeneral425_3, ENLgeneral425_4, ENLgeneral425_5, ENLgeneral425_6, ENLgeneral425_7, ENLgeneral425_8, ENLgeneral425_9, ENLgeneral425_10) %>%
    filter(!is.na(P425)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P424 == "b. No")
  
  rm(ENLgeneral425_1, ENLgeneral425_2, ENLgeneral425_3, ENLgeneral425_4, ENLgeneral425_5, ENLgeneral425_6, ENLgeneral425_7, ENLgeneral425_8, ENLgeneral425_9, ENLgeneral425_10)
  
  P425sexo <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P425edad_group <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P425niv_educ <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P425auto_etnia <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P425estrasoc <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P425area <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P425zona <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>%
    
    mutate(ID= row_number(P425)) %>% ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P425dom <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P425region_nat <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P425dep <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P425)) %>%
    ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P425total <- P425 %>%
    select(P425, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P425 != "NA") %>%
    group_by(P425, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P425)) %>% ungroup %>%
    spread(P425, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P425 <- bind_rows(P425total, P425sexo, P425edad_group, P425niv_educ, P425auto_etnia, P425estrasoc, P425area, P425zona, P425dom, P425region_nat, P425dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P425total, P425sexo, P425edad_group, P425niv_educ, P425auto_etnia, P425estrasoc, P425area, P425zona, P425dom, P425region_nat, P425dep)
  
  P425 <- P425 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P426: Frecuencia con la que leyó Revistas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P426 = case_when(P426 == 1 ~ "a. Diariamente", P426 == 2 ~ "b. Varias veces a la semana", P426 == 3 ~ "c. Una vez a la semana", P426 == 4 ~ "d. Varias veces al mes", P426 == 5 ~ "e. Una vez al mes"))
  
  P426 <- ENLgeneral %>%
    select(P424, P426, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P424 == "a. Sí")
  
  P426sexo <- P426 %>%
    select(P426, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P426edad_group <- P426 %>%
    select(P426, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P426niv_educ <- P426 %>%
    select(P426, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P426auto_etnia <- P426 %>%
    select(P426, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P426estrasoc <- P426 %>%
    select(P426, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P426area <- P426 %>%
    select(P426, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P426zona <- P426 %>%
    select(P426, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P426dom <- P426 %>%
    select(P426, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P426region_nat <- P426 %>%
    select(P426, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P426dep <- P426 %>%
    select(P426, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P426total <- P426 %>%
    select(P426, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P426 != "NA") %>%
    group_by(P426, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P426)) %>% ungroup %>%
    spread(P426, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>%
    
    mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P426 <- bind_rows(P426total, P426sexo, P426edad_group, P426niv_educ, P426auto_etnia, P426estrasoc, P426area, P426zona, P426dom, P426region_nat, P426dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P426total, P426sexo, P426edad_group, P426niv_educ, P426auto_etnia, P426estrasoc, P426area, P426zona, P426dom, P426region_nat, P426dep)
  
  P426 <- P426 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Varias veces al mes (%)` = round (100 * `d. Varias veces al mes` / `Total`, 1)) %>% mutate(`e. Una vez al mes (%)` = round (100 * `e. Una vez al mes` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P427: Tipo de Revistas que leyó
{
  ENLgeneral <- ENLgeneral %>% mutate(P427_1 = as.numeric(P427_1)) %>%
    mutate(P427_1 = case_when(P427_1 == 1 ~ "a. Sí",
                              P427_1 == 0 ~ "b. No")) %>%
    mutate(P427_2 = as.numeric(P427_2)) %>% mutate(P427_2 = case_when(P427_2 == 1 ~ "a. Sí",
                                                                      P427_2 == 0 ~ "b. No")) %>%
    mutate(P427_3 = as.numeric(P427_3)) %>% mutate(P427_3 = case_when(P427_3 == 1 ~ "a. Sí",
                                                                      P427_3 == 0 ~ "b. No")) %>%
    mutate(P427_4 = as.numeric(P427_4)) %>% mutate(P427_4 = case_when(P427_4 == 1 ~ "a. Sí",
                                                                      P427_4 == 0 ~ "b. No")) %>%
    mutate(P427_5 = as.numeric(P427_5)) %>% mutate(P427_5 = case_when(P427_5 == 1 ~ "a. Sí",
                                                                      P427_5 == 0 ~ "b. No")) %>%
    mutate(P427_6 = as.numeric(P427_6)) %>% mutate(P427_6 = case_when(P427_6 == 1 ~ "a. Sí",
                                                                      P427_6 == 0 ~ "b. No")) %>%
    mutate(P427_7 = as.numeric(P427_7)) %>% mutate(P427_7 = case_when(P427_7 == 1 ~ "a. Sí",
                                                                      P427_7 == 0 ~ "b. No")) %>%
    mutate(P427_8 = as.numeric(P427_8)) %>% mutate(P427_8 = case_when(P427_8 == 1 ~ "a. Sí",
                                                                      P427_8 == 0 ~ "b. No")) %>%
    mutate(P427_9 = as.numeric(P427_9)) %>% mutate(P427_9 = case_when(P427_9 == 1 ~ "a. Sí",
                                                                      P427_9 == 0 ~ "b. No")) %>%
    mutate(P427_10 = as.numeric(P427_10)) %>% mutate(P427_10 = case_when(P427_10 == 1 ~ "a. Sí",
                                                                         P427_10 == 0 ~ "b. No")) %>%
    mutate(P427_11 = as.numeric(P427_11)) %>% mutate(P427_11 = case_when(P427_11 == 1 ~ "a. Sí",
                                                                         P427_11 == 0 ~ "b. No")) %>%
    mutate(P427_12 = as.numeric(P427_12)) %>%
    
    mutate(P427_12 = case_when(P427_12 == 1 ~ "a. Sí",
                               P427_12 == 0 ~ "b. No")) %>%
    mutate(P427_13 = as.numeric(P427_13)) %>% mutate(P427_13 = case_when(P427_13 == 1 ~ "a. Sí",
                                                                         P427_13 == 0 ~ "b. No"))
  
  ENLgeneral427_1 <- ENLgeneral %>% select(P424, P427 = P427_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Infantiles")
  ENLgeneral427_2 <- ENLgeneral %>% select(P424, P427 = P427_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Juveniles")
  ENLgeneral427_3 <- ENLgeneral %>% select(P424, P427 = P427_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. De historietas ")
  ENLgeneral427_4 <- ENLgeneral %>% select(P424, P427 = P427_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. De música/video/cine/fotografía")
  ENLgeneral427_5 <- ENLgeneral %>% select(P424, P427 = P427_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. De deportes")
  ENLgeneral427_6 <- ENLgeneral %>% select(P424, P427 = P427_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. De naturaleza/medioambiente/animales ")
  ENLgeneral427_7 <- ENLgeneral %>% select(P424, P427 = P427_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Profesionales/científicas/tecnológicas")
  ENLgeneral427_8 <- ENLgeneral %>% select(P424, P427 = P427_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Religiosas")
  ENLgeneral427_9 <- ENLgeneral %>% select(P424, P427 = P427_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. De arte/cultura/literatura")
  ENLgeneral427_10 <- ENLgeneral %>% select(P424, P427 = P427_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. De moda/cocina/espectáculos")
  ENLgeneral427_11 <- ENLgeneral %>% select(P424, P427 = P427_11, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. De política/economía y negocios")
  ENLgeneral427_12 <- ENLgeneral %>% select(P424, P427 = P427_12, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "l. Esotéricas")
  ENLgeneral427_13 <- ENLgeneral %>% select(P424, P427 = P427_13, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "m. Otro")
  P427 <- bind_rows(ENLgeneral427_1, ENLgeneral427_2, ENLgeneral427_3, ENLgeneral427_4, ENLgeneral427_5, ENLgeneral427_6, ENLgeneral427_7, ENLgeneral427_8, ENLgeneral427_9, ENLgeneral427_10,
                    ENLgeneral427_11, ENLgeneral427_12, ENLgeneral427_13) %>% filter(!is.na(P427)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>% filter(P424 == "a. Sí")
  
  rm(ENLgeneral427_1, ENLgeneral427_2, ENLgeneral427_3, ENLgeneral427_4, ENLgeneral427_5, ENLgeneral427_6, ENLgeneral427_7, ENLgeneral427_8, ENLgeneral427_9, ENLgeneral427_10,
     ENLgeneral427_11, ENLgeneral427_12, ENLgeneral427_13)
  
  P427sexo <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P427edad_group <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P427niv_educ <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P427auto_etnia <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P427estrasoc <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P427area <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P427zona <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P427dom <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P427region_nat <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P427dep <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    group_by(P427, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P427)) %>%
    ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P427total <- P427 %>%
    select(P427, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P427 != "NA") %>%
    
    group_by(P427, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P427)) %>% ungroup %>%
    spread(P427, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P427 <- bind_rows(P427total, P427sexo, P427edad_group, P427niv_educ, P427auto_etnia, P427estrasoc, P427area, P427zona, P427dom, P427region_nat, P427dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P427total, P427sexo, P427edad_group, P427niv_educ, P427auto_etnia, P427estrasoc, P427area, P427zona, P427dom, P427region_nat, P427dep)
  
  P427 <- P427 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P428: Cuanto pagó por las revistas adquiridas para usted u otras personas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P428 = case_when(between(P428, 1, 5) ~ "a. 1 a 5 soles",
                            between(P428, 6, 10) ~ "b. 6 a 10 soles",
                            between(P428, 11, 50) ~ "c. 11 a 50 soles", P428 > 51 ~ "d. más de 50 soles"))
  
  P428<- ENLgeneral %>%
    select(P428, P428_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P428sexo <- P428 %>%
    select(P428, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P428edad_group <- P428 %>%
    
    select(P428, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P428niv_educ <- P428 %>%
    select(P428, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P428auto_etnia <- P428 %>%
    select(P428, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P428estrasoc <- P428 %>%
    select(P428, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P428area <- P428 %>%
    select(P428, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P428zona <- P428 %>%
    select(P428, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P428dom <- P428 %>%
    select(P428, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P428region_nat <- P428 %>%
    
    select(P428, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P428dep <- P428 %>%
    select(P428, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P428total <- P428 %>%
    select(P428, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P428 != "NA") %>%
    group_by(P428, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P428)) %>% ungroup %>%
    spread(P428, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P428 <- bind_rows(P428total, P428sexo, P428edad_group, P428niv_educ, P428auto_etnia, P428estrasoc, P428area, P428zona, P428dom, P428region_nat, P428dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P428total, P428sexo, P428edad_group, P428niv_educ, P428auto_etnia, P428estrasoc, P428area, P428zona, P428dom, P428region_nat, P428dep)
  
  P428 <- P428 %>%
    mutate(`a. 1 a 5 soles (%)` = round (100 * `a. 1 a 5 soles` / `Total`, 1)) %>%
    mutate(`b. 6 a 10 soles (%)` = round (100 * `b. 6 a 10 soles` / `Total`, 1)) %>%
    mutate(`c. 11 a 50 soles (%)` = round (100 * `c. 11 a 50 soles` / `Total`, 1)) %>% mutate(`d. más de 50 soles (%)` = round (100 * `d. más de 50 soles` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}

# E. LECTURA DE OTROS CONTENIDOS DIGITALES
#****************************************************            #* P429: Tipo de Contenido Digital que leyó
{
  ENLgeneral <- ENLgeneral %>% mutate(P429_1 = as.numeric(P429_1)) %>%
    mutate(P429_1 = case_when(P429_1 == 1 ~ "a. Sí",
                              P429_1 == 2 ~ "b. No",
                              P429_1 == 3 ~ "c. No conoce")) %>% mutate(P429_2 = as.numeric(P429_2)) %>% mutate(P429_2 = case_when(P429_2 == 1 ~ "a. Sí",
                                                                                                                                   P429_2 == 2 ~ "b. No",
                                                                                                                                   P429_2 == 3 ~ "c. No conoce")) %>% mutate(P429_3 = as.numeric(P429_3)) %>% mutate(P429_3 = case_when(P429_3 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                        P429_3 == 2 ~ "b. No",
                                                                                                                                                                                                                                        P429_3 == 3 ~ "c. No conoce")) %>% mutate(P429_4 = as.numeric(P429_4)) %>% mutate(P429_4 = case_when(P429_4 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                             P429_4 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                             P429_4 == 3 ~ "c. No conoce")) %>% mutate(P429_5 = as.numeric(P429_5)) %>% mutate(P429_5 = case_when(P429_5 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                  P429_5 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                  P429_5 == 3 ~ "c. No conoce")) %>% mutate(P429_6 = as.numeric(P429_6)) %>% mutate(P429_6 = case_when(P429_6 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       P429_6 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       P429_6 == 3 ~ "c. No conoce")) %>% mutate(P429_7 = as.numeric(P429_7)) %>% mutate(P429_7 = case_when(P429_7 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            P429_7 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            P429_7 == 3 ~ "c. No conoce")) %>% mutate(P429_8 = as.numeric(P429_8)) %>% mutate(P429_8 = case_when(P429_8 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 P429_8 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 P429_8 == 3 ~ "c. No conoce")) %>% mutate(P429_9 = as.numeric(P429_9)) %>% mutate(P429_9 = case_when(P429_9 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      P429_9 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      P429_9 == 3 ~ "c. No conoce")) %>% mutate(P429_10 = as.numeric(P429_10)) %>% mutate(P429_10 = case_when(P429_10 == 1 ~ "a. Sí",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              P429_10 == 2 ~ "b. No",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              P429_10 == 3 ~ "c. No conoce")) %>%
    mutate(P429_X = paste(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P429_9, P429_10)) %>%
    
    mutate(P429_X = case_when(str_detect(P429_X, "a. Sí") ~ "a. Sí",
                              str_detect(P429_X, "c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce c. No conoce") ~ "c. No conoce",
                              TRUE ~ "b. No" ))
  
  ENLgeneral429_1 <- ENLgeneral %>% select(P429 = P429_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Correos electrónicos")
  ENLgeneral429_2 <- ENLgeneral %>% select(P429 = P429_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Textos en Facebook")
  ENLgeneral429_3 <- ENLgeneral %>% select(P429 = P429_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Textos en Twitter ")
  ENLgeneral429_4 <- ENLgeneral %>% select(P429 = P429_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Textos en Instagram")
  ENLgeneral429_5 <- ENLgeneral %>% select(P429 = P429_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Textos en Whatsaap")
  ENLgeneral429_6 <- ENLgeneral %>% select(P429 = P429_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Páginas web ")
  ENLgeneral429_7 <- ENLgeneral %>% select(P429 = P429_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Foros / blogs")
  ENLgeneral429_8 <- ENLgeneral %>% select(P429 = P429_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Documentos de trabajo/laborales y/o académicos")
  ENLgeneral429_9 <- ENLgeneral %>% select(P429 = P429_9, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Boletines electrónicos")
  ENLgeneral429_10 <- ENLgeneral %>% select(P429 = P429_10, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Textos en redes sociales")
  ENLgeneral429_X <- ENLgeneral %>% select(P429 = P429_X, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "k. Al menos un contenido digital")
  P429 <- bind_rows(ENLgeneral429_1, ENLgeneral429_2, ENLgeneral429_3, ENLgeneral429_4, ENLgeneral429_5, ENLgeneral429_6, ENLgeneral429_7, ENLgeneral429_8, ENLgeneral429_9, ENLgeneral429_10, ENLgeneral429_X) %>%
    filter(!is.na(P429)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  rm(ENLgeneral429_1, ENLgeneral429_2, ENLgeneral429_3, ENLgeneral429_4, ENLgeneral429_5, ENLgeneral429_6, ENLgeneral429_7, ENLgeneral429_8, ENLgeneral429_9, ENLgeneral429_10, ENLgeneral429_X)
  
  P429sexo <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P429edad_group <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P429niv_educ <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P429auto_etnia <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>%
    
    mutate(Variables = "Autoidentificación étnica")
  
  P429estrasoc <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P429area <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P429zona <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P429dom <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P429region_nat <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P429dep <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429)) %>%
    ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P429total <- P429 %>%
    select(P429, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429 != "NA") %>%
    group_by(P429, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P429)) %>% ungroup %>%
    spread(P429, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>%
    
    mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P429 <- bind_rows(P429total, P429edad_group, P429niv_educ, P429auto_etnia, P429estrasoc, P429area, P429zona, P429dom, P429region_nat, P429dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P429total, P429sexo, P429edad_group, P429niv_educ, P429auto_etnia, P429estrasoc, P429area, P429zona, P429dom, P429region_nat, P429dep)
  
  P429 <- P429 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>%
    mutate(`c. No conoce (%)` = round (100 * `c. No conoce` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1)) %>%
    filter(Tipo == "k. Al menos un contenido digital") %>% mutate(`b. No (%)` = `b. No (%)` + `c. No conoce (%)`)
}
#* P429_1: Frecuencia con la que leyó Contenidos Digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P429_1_1 = case_when(P429_1_1 == 1 ~ "a. Varias veces al día", P429_1_1 == 2 ~ "b. Una vez al día",
                                P429_1_1 == 3 ~ "c. Varias veces a la semana", P429_1_1 == 4 ~ "d. Una vez a la semana", P429_1_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_2_1 = case_when(P429_2_1 == 1 ~ "a. Varias veces al día", P429_2_1 == 2 ~ "b. Una vez al día",
                                P429_2_1 == 3 ~ "c. Varias veces a la semana", P429_2_1 == 4 ~ "d. Una vez a la semana", P429_2_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_3_1 = case_when(P429_3_1 == 1 ~ "a. Varias veces al día", P429_3_1 == 2 ~ "b. Una vez al día",
                                P429_3_1 == 3 ~ "c. Varias veces a la semana", P429_3_1 == 4 ~ "d. Una vez a la semana", P429_3_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_4_1 = case_when(P429_4_1 == 1 ~ "a. Varias veces al día", P429_4_1 == 2 ~ "b. Una vez al día",
                                P429_4_1 == 3 ~ "c. Varias veces a la semana", P429_4_1 == 4 ~ "d. Una vez a la semana", P429_4_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_5_1 = case_when(P429_5_1 == 1 ~ "a. Varias veces al día", P429_5_1 == 2 ~ "b. Una vez al día",
                                P429_5_1 == 3 ~ "c. Varias veces a la semana", P429_5_1 == 4 ~ "d. Una vez a la semana", P429_5_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_6_1 = case_when(P429_6_1 == 1 ~ "a. Varias veces al día", P429_6_1 == 2 ~ "b. Una vez al día",
                                P429_6_1 == 3 ~ "c. Varias veces a la semana", P429_6_1 == 4 ~ "d. Una vez a la semana", P429_6_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_7_1 = case_when(P429_7_1 == 1 ~ "a. Varias veces al día", P429_7_1 == 2 ~ "b. Una vez al día",
                                P429_7_1 == 3 ~ "c. Varias veces a la semana", P429_7_1 == 4 ~ "d. Una vez a la semana",
                                
                                P429_7_1 == 5 ~ "e. Una vez al mes")) %>% mutate(P429_8_1 = case_when(P429_8_1 == 1 ~ "a. Varias veces al día",
                                                                                                      P429_8_1 == 2 ~ "b. Una vez al día",
                                                                                                      P429_8_1 == 3 ~ "c. Varias veces a la semana", P429_8_1 == 4 ~ "d. Una vez a la semana", P429_8_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_9_1 = case_when(P429_9_1 == 1 ~ "a. Varias veces al día", P429_9_1 == 2 ~ "b. Una vez al día",
                                P429_9_1 == 3 ~ "c. Varias veces a la semana", P429_9_1 == 4 ~ "d. Una vez a la semana", P429_9_1 == 5 ~ "e. Una vez al mes")) %>%
    mutate(P429_10_1 = case_when(P429_10_1 == 1 ~ "a. Varias veces al día", P429_10_1 == 2 ~ "b. Una vez al día",
                                 P429_10_1 == 3 ~ "c. Varias veces a la semana", P429_10_1 == 4 ~ "d. Una vez a la semana", P429_10_1 == 5 ~ "e. Una vez al mes"))
  
  ENLgeneral429_1_1 <- ENLgeneral %>% select(P429_1, P429_1A = P429_1_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Correos electrónicos") %>% filter(P429_1 == "a. Sí")
  ENLgeneral429_1_2 <- ENLgeneral %>% select(P429_2, P429_1A = P429_2_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Textos en Facebook") %>% filter(P429_2 == "a. Sí")
  ENLgeneral429_1_3 <- ENLgeneral %>% select(P429_3, P429_1A = P429_3_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Textos en Twitter") %>% filter(P429_3 == "a. Sí")
  ENLgeneral429_1_4 <- ENLgeneral %>% select(P429_4, P429_1A = P429_4_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Textos en Instagram") %>% filter(P429_4 == "a. Sí")
  ENLgeneral429_1_5 <- ENLgeneral %>% select(P429_5, P429_1A = P429_5_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Textos en Whatsaap") %>% filter(P429_5 == "a. Sí")
  ENLgeneral429_1_6 <- ENLgeneral %>% select(P429_6, P429_1A = P429_6_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Páginas web ") %>% filter(P429_6 == "a. Sí")
  ENLgeneral429_1_7 <- ENLgeneral %>% select(P429_7, P429_1A = P429_7_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Foros / blogs") %>% filter(P429_7 == "a. Sí")
  ENLgeneral429_1_8 <- ENLgeneral %>% select(P429_8, P429_1A = P429_8_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Documentos de trabajo/laborales y/o académicos") %>% filter(P429_8 == "a. Sí")
  ENLgeneral429_1_9 <- ENLgeneral %>% select(P429_9, P429_1A = P429_9_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "i. Boletines electrónicos") %>% filter(P429_9 == "a. Sí")
  ENLgeneral429_1_10 <- ENLgeneral %>% select(P429_10, P429_1A = P429_10_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "j. Redes sociales") %>% filter(P429_10 == "a. Sí")
  P429_1 <- bind_rows(ENLgeneral429_1_1, ENLgeneral429_1_2, ENLgeneral429_1_3, ENLgeneral429_1_4,	ENLgeneral429_1_5,	ENLgeneral429_1_6,	ENLgeneral429_1_7, ENLgeneral429_1_8, ENLgeneral429_1_9, ENLgeneral429_1_10) %>%
    filter(!is.na(P429_1A)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí")
  
  rm(ENLgeneral429_1_1, ENLgeneral429_1_2, ENLgeneral429_1_3, ENLgeneral429_1_4, ENLgeneral429_1_5,	ENLgeneral429_1_6,	ENLgeneral429_1_7,	ENLgeneral429_1_8, ENLgeneral429_1_9, ENLgeneral429_1_10)
  
  P429_1Asexo <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P429_1Aedad_group <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P429_1Aniv_educ <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P429_1Aauto_etnia <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P429_1Aestrasoc <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P429_1Aarea <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P429_1Azona <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(zona) %>% adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P429_1Adom <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P429_1Aregion_nat <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P429_1Adep <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P429_1A != "NA") %>%
    group_by(P429_1A, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P429_1A)) %>%
    ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P429_1Atotal <- P429_1 %>%
    select(P429_1A, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P429_1A != "NA") %>% group_by(P429_1A, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P429_1A)) %>% ungroup %>%
    spread(P429_1A, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P429_1 <- bind_rows(P429_1Atotal, P429_1Asexo, P429_1Aedad_group, P429_1Aniv_educ, P429_1Aauto_etnia, P429_1Aestrasoc, P429_1Aarea, P429_1Azona, P429_1Adom, P429_1Aregion_nat, P429_1Adep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P429_1Atotal, P429_1Asexo, P429_1Aedad_group, P429_1Aniv_educ, P429_1Aauto_etnia, P429_1Aestrasoc, P429_1Aarea, P429_1Azona, P429_1Adom, P429_1Aregion_nat, P429_1Adep)
  
  P429_1 <- P429_1 %>%
    mutate(`a. Varias veces al día (%)` = round (100 * `a. Varias veces al día` / `Total`, 1)) %>% mutate(`b. Una vez al día (%)` = round (100 * `b. Una vez al día` / `Total`, 1)) %>%
    mutate(`c. Varias veces a la semana (%)` = round (100 * `c. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`d. Una vez a la semana (%)` = round (100 * `d. Una vez a la semana` / `Total`, 1)) %>% mutate(`e. Una vez al mes (%)` = round (100 * `e. Una vez al mes` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P430: Principales razones por las que no leyó Contenidos digitales - Personas que no leyeron o no conocen los contenidos digitales
{
  ENLgeneral <- ENLgeneral %>% mutate(P430_1 = as.numeric(P430_1)) %>%
    mutate(P430_1 = case_when(P430_1 == 1 ~ "a. Sí",
                              P430_1 == 0 ~ "b. No")) %>%
    mutate(P430_2 = as.numeric(P430_2)) %>% mutate(P430_2 = case_when(P430_2 == 1 ~ "a. Sí",
                                                                      P430_2 == 0 ~ "b. No")) %>%
    mutate(P430_3 = as.numeric(P430_3)) %>% mutate(P430_3 = case_when(P430_3 == 1 ~ "a. Sí",
                                                                      P430_3 == 0 ~ "b. No")) %>%
    mutate(P430_4 = as.numeric(P430_4)) %>% mutate(P430_4 = case_when(P430_4 == 1 ~ "a. Sí",
                                                                      P430_4 == 0 ~ "b. No")) %>%
    mutate(P430_5 = as.numeric(P430_5)) %>% mutate(P430_5 = case_when(P430_5 == 1 ~ "a. Sí",
                                                                      P430_5 == 0 ~ "b. No")) %>%
    mutate(P430_6 = as.numeric(P430_6)) %>% mutate(P430_6 = case_when(P430_6 == 1 ~ "a. Sí",
                                                                      P430_6 == 0 ~ "b. No")) %>%
    mutate(P430_7 = as.numeric(P430_7)) %>% mutate(P430_7 = case_when(P430_7 == 1 ~ "a. Sí",
                                                                      
                                                                      P430_7 == 0 ~ "b. No")) %>%
    mutate(P430_8 = as.numeric(P430_8)) %>% mutate(P430_8 = case_when(P430_8 == 1 ~ "a. Sí",
                                                                      P430_8 == 0 ~ "b. No"))
  
  ENLgeneral430_1 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. No le gusta / Falta de interés")
  ENLgeneral430_2 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. No sabe utilizar dispositivos tecnológicos")
  ENLgeneral430_3 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. No cuenta con dispositivos tecnológicos")
  ENLgeneral430_4 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. No cuenta con Internet")
  ENLgeneral430_5 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Los contenidos digitales no están en su lengua materna")
  ENLgeneral430_6 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Prefiere formatos impresos")
  ENLgeneral430_7 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. No hay contenidos digitales disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral430_8 <- ENLgeneral %>% select(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8, P430 = P430_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Otro")
  P430 <- bind_rows(ENLgeneral430_1, ENLgeneral430_2, ENLgeneral430_3, ENLgeneral430_4, ENLgeneral430_5, ENLgeneral430_6, ENLgeneral430_7, ENLgeneral430_8) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(sabe_leer == "a. Sí") %>%
    mutate(caracteres = paste(P429_1, P429_2, P429_3, P429_4, P429_5, P429_6, P429_7, P429_8))
  
  %>%
    
    
    mutate(Leerev = case_when(str_detect(caracteres, "a. Sí") ~ "a. Sí", TRUE ~ "b. No" )) %>%
    filter(Leerev == "b. No")
  
  
  rm(ENLgeneral430_1, ENLgeneral430_2, ENLgeneral430_3, ENLgeneral430_4, ENLgeneral430_5, ENLgeneral430_6, ENLgeneral430_7, ENLgeneral430_8)
  
  P430sexo <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, sexo, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "sexo")
  
  P430edad_group <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, edad_group, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupo de edad")
  
  P430auto_etnia <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, auto_etnia, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P430niv_educ <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, niv_educ, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>%
    
    mutate(Variables = "Nivel educativo")
  
  P430estratsoc <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, ESTRATOSOCIO, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P430)) %>% ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P430area <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, area, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P430dep <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, NOMBREDD, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P430)) %>% ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P430dom <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, dominio, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P430region_nat <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, Tipo, region_nat, FACTOR200_FINAL) %>% mutate(ID= row_number(P430)) %>%
    ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Región natural")
  
  P430total <- P430 %>%
    select(P430, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P430 != "NA") %>%
    group_by(P430, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P430)) %>% ungroup %>%
    spread(P430, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P430 <- bind_rows(P430total, P430sexo, P430edad_group, P430auto_etnia, P430niv_educ, P430estratsoc , P430area, P430dep, P430dom, P430region_nat) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P430total,	P430sexo,	P430edad_group,	P430auto_etnia,	P430niv_educ,	P430estratsoc, P430area, P430dep, P430dom, P430region_nat)
  
  P430 <- P430 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
  
}

# F. ASISTENCIA A BIBLIOTECAS
#****************************************************            #* P431: Asistió de manera presencial a las bibliotecas
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P431 = case_when(P431 == 1 ~ "a. Sí",
                            P431 == 2 ~ "b. No"))
  
  P431 <- ENLgeneral %>%
    select(P431, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P431sexo <- P431 %>%
    select(P431, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P431edad_group <- P431 %>%
    select(P431, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P431niv_educ <- P431 %>%
    select(P431, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P431auto_etnia <- P431 %>%
    select(P431, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P431estrasoc <- P431 %>%
    select(P431, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P431area <- P431 %>%
    select(P431, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P431zona <- P431 %>%
    
    select(P431, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P431dom <- P431 %>%
    select(P431, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P431region_nat <- P431 %>%
    select(P431, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P431dep <- P431 %>%
    select(P431, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P431total <- P431 %>%
    select(P431, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P431 != "NA") %>%
    group_by(P431, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P431)) %>% ungroup %>%
    spread(P431, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P431 <- bind_rows(P431total, P431sexo, P431edad_group, P431niv_educ, P431auto_etnia, P431estrasoc, P431area, P431zona, P431dom, P431region_nat, P431dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P431total, P431sexo, P431edad_group, P431niv_educ, P431auto_etnia, P431estrasoc, P431area, P431zona, P431dom, P431region_nat, P431dep)
  
  P431 <- P431 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P432: Frecuencia con la que asistió a las bibliotecas de manera presencial
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P432 = case_when(P432 == 1 ~ "a. Diariamente", P432 == 2 ~ "b. Varias veces a la semana", P432 == 3 ~ "c. Una vez a la semana", P432 == 4 ~ "d. Una vez al mes",
                            P432 == 5 ~ "e. Una vez cada tres meses", P432 == 6 ~ "f. Por lo menos una vez al año"))
  
  P432 <- ENLgeneral %>%
    select(P431, P432, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P431 == "a. Sí")
  
  P432sexo <- P432 %>%
    select(P432, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P432edad_group <- P432 %>%
    select(P432, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P432niv_educ <- P432 %>%
    select(P432, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P432auto_etnia <- P432 %>%
    select(P432, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P432estrasoc <- P432 %>%
    select(P432, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P432area <- P432 %>%
    select(P432, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P432zona <- P432 %>%
    select(P432, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P432dom <- P432 %>%
    select(P432, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P432region_nat <- P432 %>%
    select(P432, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P432dep <- P432 %>%
    select(P432, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P432total <- P432 %>%
    select(P432, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P432 != "NA") %>%
    group_by(P432, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P432)) %>% ungroup %>%
    spread(P432, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P432 <- bind_rows(P432total, P432sexo, P432edad_group, P432niv_educ, P432auto_etnia, P432estrasoc, P432area, P432zona, P432dom, P432region_nat, P432dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P432total, P432sexo, P432edad_group, P432niv_educ, P432auto_etnia, P432estrasoc, P432area, P432zona, P432dom, P432region_nat, P432dep)
  
  P432 <- P432 %>%
    mutate(`a. Diariamente (%)` = round (100 * `a. Diariamente` / `Total`, 1)) %>%
    mutate(`b. Varias veces a la semana (%)` = round (100 * `b. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`c. Una vez a la semana (%)` = round (100 * `c. Una vez a la semana` / `Total`, 1)) %>% mutate(`d. Una vez al mes (%)` = round (100 * `d. Una vez al mes` / `Total`, 1)) %>%
    mutate(`e. Una vez cada tres meses (%)` = round (100 * `e. Una vez cada tres meses` / `Total`, 1))
  %>%
    mutate(`f. Por lo menos una vez al año (%)` = round (100 * `f. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P433: Principales razones por las que no asistió a bibliotecas
{
  ENLgeneral <- ENLgeneral %>% mutate(P433_1 = as.numeric(P433_1)) %>%
    mutate(P433_1 = case_when(P433_1 == 1 ~ "a. Sí",
                              P433_1 == 0 ~ "b. No")) %>%
    mutate(P433_2 = as.numeric(P433_2)) %>% mutate(P433_2 = case_when(P433_2 == 1 ~ "a. Sí",
                                                                      P433_2 == 0 ~ "b. No")) %>%
    mutate(P433_3 = as.numeric(P433_3)) %>% mutate(P433_3 = case_when(P433_3 == 1 ~ "a. Sí",
                                                                      P433_3 == 0 ~ "b. No")) %>%
    mutate(P433_4 = as.numeric(P433_4)) %>% mutate(P433_4 = case_when(P433_4 == 1 ~ "a. Sí",
                                                                      P433_4 == 0 ~ "b. No")) %>%
    mutate(P433_5 = as.numeric(P433_5)) %>% mutate(P433_5 = case_when(P433_5 == 1 ~ "a. Sí",
                                                                      P433_5 == 0 ~ "b. No")) %>%
    mutate(P433_6 = as.numeric(P433_6)) %>% mutate(P433_6 = case_when(P433_6 == 1 ~ "a. Sí",
                                                                      P433_6 == 0 ~ "b. No")) %>%
    mutate(P433_7 = as.numeric(P433_7)) %>% mutate(P433_7 = case_when(P433_7 == 1 ~ "a. Sí",
                                                                      P433_7 == 0 ~ "b. No"))
  
  ENLgeneral433_1 <- ENLgeneral %>% select(P431, P433 = P433_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Falta de bibliotecas cerca de su hogar o centro de trabajo")
  
  ENLgeneral433_2 <- ENLgeneral %>% select(P431, P433 = P433_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. No le gustan las bibliotecas / Falta de interés")
  ENLgeneral433_3 <- ENLgeneral %>% select(P431, P433 = P433_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Falta de tiempo ")
  ENLgeneral433_4 <- ENLgeneral %>% select(P431, P433 = P433_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. No hay publicaciones en su lengua materna")
  ENLgeneral433_5 <- ENLgeneral %>% select(P431, P433 = P433_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Consigue sus publicaciones por otros medios (hogar, en otros espacios o canales)")
  ENLgeneral433_6 <- ENLgeneral %>% select(P431, P433 = P433_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. No hay publicaciones disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral433_7 <- ENLgeneral %>% select(P431, P433 = P433_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Otro")
  
  P433 <- bind_rows(ENLgeneral433_1, ENLgeneral433_2, ENLgeneral433_3, ENLgeneral433_4, ENLgeneral433_5, ENLgeneral433_6, ENLgeneral433_7) %>%
    filter(!is.na(P433)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P431 == "b. No")
  
  rm(ENLgeneral433_1, ENLgeneral433_2, ENLgeneral433_3, ENLgeneral433_4, ENLgeneral433_5, ENLgeneral433_6, ENLgeneral433_7)
  
  P433sexo <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P433edad_group <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P433niv_educ <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P433auto_etnia <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P433estrasoc <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P433area <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, area) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P433zona <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P433dom <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P433region_nat <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P433dep <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P433)) %>%
    ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P433total <- P433 %>%
    select(P433, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P433 != "NA") %>%
    group_by(P433, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P433)) %>% ungroup %>%
    spread(P433, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P433 <- bind_rows(P433total, P433sexo, P433edad_group, P433niv_educ, P433auto_etnia, P433estrasoc, P433area, P433zona, P433dom, P433region_nat, P433dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P433total, P433sexo, P433edad_group, P433niv_educ, P433auto_etnia, P433estrasoc, P433area, P433zona, P433dom, P433region_nat, P433dep)
  
  P433 <- P433 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P434: Hizo uso de los servicios de bibliotecas digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P434_1 = case_when(P434_1 == 1 ~ "a. Sí",
                              
                              P434_1 == 2 ~ "b. No"))
  
  P434 <- ENLgeneral %>%
    select(P434_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P434_1sexo <- P434 %>%
    select(P434_1, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P434_1edad_group <- P434 %>%
    select(P434_1, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P434_1niv_educ <- P434 %>%
    select(P434_1, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(niv_educ) %>% summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P434_1auto_etnia <- P434 %>%
    
    select(P434_1, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(auto_etnia) %>% summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P434_1estrasoc <- P434 %>%
    select(P434_1, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P434_1area <- P434 %>%
    select(P434_1, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P434_1zona <- P434 %>%
    select(P434_1, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    
    group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P434_1dom <- P434 %>%
    select(P434_1, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(dominio) %>% summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P434_1region_nat <- P434 %>%
    select(P434_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P434_1dep <- P434 %>%
    select(P434_1, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(NOMBREDD) %>% summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P434_1total <- P434 %>%
    
    select(P434_1, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    filter(P434_1 != "NA") %>%
    group_by(P434_1, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434_1)) %>% ungroup %>%
    spread(P434_1, FACTOR200_FINAL, fill = 0) %>% select(-ID) %>%
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P434 <- bind_rows(P434_1total, P434_1sexo, P434_1edad_group, P434_1niv_educ, P434_1auto_etnia, P434_1estrasoc, P434_1area, P434_1zona, P434_1dom, P434_1region_nat, P434_1dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P434_1total, P434_1sexo, P434_1edad_group, P434_1niv_educ, P434_1auto_etnia, P434_1estrasoc, P434_1area, P434_1zona, P434_1dom, P434_1region_nat, P434_1dep)
  
  P434 <- P434 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P434A: Frecuencia con la que hizo uso de bibliotecas digitales
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P434 = case_when(P434 == 1 ~ "a. Varias veces al día", P434 == 2 ~ "b. Una vez al día",
                            P434 == 3 ~ "c. Varias veces a la semana", P434 == 4 ~ "d. Una vez a la semana", P434 == 5 ~ "e. Varias veces al mes", P434 == 6 ~ "f. Una vez al mes"))
  
  P434A <- ENLgeneral %>%
    select(P434_1, P434, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P434_1 == "a. Sí")
  
  P434sexo <- P434A %>%
    select(P434, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P434edad_group <- P434A %>%
    select(P434, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P434niv_educ <- P434A %>%
    select(P434, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P434auto_etnia <- P434A %>%
    select(P434, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P434estrasoc <- P434A %>%
    select(P434, FACTOR200_FINAL, ESTRATOSOCIO) %>%
    
    mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P434area <- P434A %>%
    select(P434, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P434zona <- P434A %>%
    select(P434, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P434dom <- P434A %>%
    select(P434, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P434region_nat <- P434A %>%
    select(P434, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P434dep <- P434A %>%
    select(P434, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P434total <- P434A %>%
    select(P434, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P434 != "NA") %>%
    group_by(P434, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P434)) %>% ungroup %>%
    spread(P434, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P434A <- bind_rows(P434total, P434sexo, P434edad_group, P434niv_educ, P434auto_etnia, P434estrasoc, P434area, P434zona, P434dom, P434region_nat, P434dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P434total, P434sexo, P434edad_group, P434niv_educ, P434auto_etnia, P434estrasoc, P434area, P434zona, P434dom, P434region_nat, P434dep)
  
  P434A <- P434A %>%
    mutate(`a. Varias veces al día (%)` = round (100 * `a. Varias veces al día` / `Total`, 1)) %>% mutate(`b. Una vez al día (%)` = round (100 * `b. Una vez al día` / `Total`, 1)) %>%
    mutate(`c. Varias veces a la semana (%)` = round (100 * `c. Varias veces a la semana` / `Total`, 1)) %>%
    mutate(`d. Una vez a la semana (%)` = round (100 * `d. Una vez a la semana` / `Total`, 1)) %>% mutate(`e. Varias veces al mes (%)` = round (100 * `e. Varias veces al mes` / `Total`, 1)) %>% mutate(`f. Una vez al mes (%)` = round (100 * `f. Una vez al mes` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

# G. ASISTENCIA A FERIAS Y FESTIVALES DEL LIBRO Y LA LECTURA
#****************************************************
#* P435: Asistió de manera presencial o virtual a ferias y festivales del libro y la lectura
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P435 = case_when(P435 == 1 ~ "a. Sí",
                            P435 == 2 ~ "b. No"))
  
  P435 <- ENLgeneral %>%
    select(P435, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P435sexo <- P435 %>%
    select(P435, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P435edad_group <- P435 %>%
    select(P435, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(edad_group) %>% summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P435niv_educ <- P435 %>%
    select(P435, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P435auto_etnia <- P435 %>%
    select(P435, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P435estrasoc <- P435 %>%
    select(P435, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P435area <- P435 %>%
    
    select(P435, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P435zona <- P435 %>%
    select(P435, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P435dom <- P435 %>%
    select(P435, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P435region_nat <- P435 %>%
    select(P435, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>%
    
    group_by(region_nat) %>% summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P435dep <- P435 %>%
    select(P435, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P435total <- P435 %>%
    select(P435, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P435 != "NA") %>%
    group_by(P435, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P435)) %>% ungroup %>%
    spread(P435, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P435 <- bind_rows(P435total, P435sexo, P435edad_group, P435niv_educ, P435auto_etnia, P435estrasoc, P435area, P435zona, P435dom, P435region_nat, P435dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P435total, P435sexo, P435edad_group, P435niv_educ, P435auto_etnia, P435estrasoc, P435area, P435zona, P435dom, P435region_nat, P435dep)
  
  P435 <- P435 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P436: Frecuencia con la que hizo uso de bibliotecas digitales
{
  ENLgeneral <- ENLgeneral %>%
    
    mutate(P436 = case_when(P436 == 1 ~ "a. Una vez al mes", P436 == 2 ~ "b. Una vez cada tres meses", P436 == 3 ~ "c. Por lo menos una vez al año"))
  
  P436 <- ENLgeneral %>%
    select(P435, P436, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P435 == "a. Sí")
  
  P436sexo <- P436 %>%
    select(P436, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P436edad_group <- P436 %>%
    select(P436, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P436niv_educ <- P436 %>%
    select(P436, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>%
    
    mutate(Variables = "Nivel educativo")
  
  P436auto_etnia <- P436 %>%
    select(P436, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P436estrasoc <- P436 %>%
    select(P436, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P436area <- P436 %>%
    select(P436, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P436zona <- P436 %>%
    select(P436, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>%
    
    ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P436dom <- P436 %>%
    select(P436, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P436region_nat <- P436 %>%
    select(P436, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P436dep <- P436 %>%
    select(P436, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>%
    
    mutate(Variables = "Departamento")
  
  P436total <- P436 %>%
    select(P436, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P436 != "NA") %>%
    group_by(P436, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P436)) %>% ungroup %>%
    spread(P436, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P436 <- bind_rows(P436total, P436sexo, P436edad_group, P436niv_educ, P436auto_etnia, P436estrasoc, P436area, P436zona, P436dom, P436region_nat, P436dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P436total, P436sexo, P436edad_group, P436niv_educ, P436auto_etnia, P436estrasoc, P436area, P436zona, P436dom, P436region_nat, P436dep)
  
  P436 <- P436 %>%
    mutate(`a. Una vez al mes (%)` = round (100 * `a. Una vez al mes` / `Total`, 1)) %>%
    mutate(`b. Una vez cada tres meses (%)` = round (100 * `b. Una vez cada tres meses` / `Total`, 1))
  %>%
    mutate(`c. Por lo menos una vez al año (%)` = round (100 * `c. Por lo menos una vez al año` /
                                                           `Total`, 1)) %>%
    mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P437: Principales razones por las que no asistió ferias y festivales del libro y la lectura
{
  ## No le gustan las ferias y festivales del libro y la lectura /Falta de interés ENLgeneral <- ENLgeneral %>%
  mutate(P437_1 = as.numeric(P437_1)) %>% mutate(P437_1 = case_when(P437_1 == 1 ~ "a. Sí",
                                                                    P437_1 == 0 ~ "b. No")) %>%
    mutate(P437_2 = as.numeric(P437_2)) %>% mutate(P437_2 = case_when(P437_2 == 1 ~ "a. Sí",
                                                                      P437_2 == 0 ~ "b. No")) %>%
    mutate(P437_3 = as.numeric(P437_3)) %>% mutate(P437_3 = case_when(P437_3 == 1 ~ "a. Sí",
                                                                      P437_3 == 0 ~ "b. No")) %>%
    mutate(P437_4 = as.numeric(P437_4)) %>% mutate(P437_4 = case_when(P437_4 == 1 ~ "a. Sí",
                                                                      P437_4 == 0 ~ "b. No")) %>%
    mutate(P437_5 = as.numeric(P437_5)) %>% mutate(P437_5 = case_when(P437_5 == 1 ~ "a. Sí",
                                                                      P437_5 == 0 ~ "b. No")) %>%
    mutate(P437_6 = as.numeric(P437_6)) %>% mutate(P437_6 = case_when(P437_6 == 1 ~ "a. Sí",
                                                                      
                                                                      P437_6 == 0 ~ "b. No")) %>%
    mutate(P437_7 = as.numeric(P437_7)) %>% mutate(P437_7 = case_when(P437_7 == 1 ~ "a. Sí",
                                                                      P437_7 == 0 ~ "b. No")) %>%
    mutate(P437_8 = as.numeric(P437_8)) %>% mutate(P437_8 = case_when(P437_8 == 1 ~ "a. Sí",
                                                                      P437_8 == 0 ~ "b. No"))
  
  ENLgeneral437_1 <- ENLgeneral %>% select(P435, P437 = P437_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. No le gustan las ferias y festivales del libro y la lectura
/Falta de interés")
  ENLgeneral437_2 <- ENLgeneral %>% select(P435, P437 = P437_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. No encuentra publicaciones en su lengua materna")
  ENLgeneral437_3 <- ENLgeneral %>% select(P435, P437 = P437_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Consigue sus publicaciones por otros medios(hogar, en otros espacios o canales)")
  ENLgeneral437_4 <- ENLgeneral %>% select(P435, P437 = P437_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. No conoce ferias y/o festivales del libro y la lectura")
  ENLgeneral437_5 <- ENLgeneral %>% select(P435, P437 = P437_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. No hay publicaciones disponibles en formatos amigables para personas con discapacidad")
  ENLgeneral437_6 <- ENLgeneral %>% select(P435, P437 = P437_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Falta de tiempo ")
  ENLgeneral437_7 <- ENLgeneral %>% select(P435, P437 = P437_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. Falta de dinero")
  ENLgeneral437_8 <- ENLgeneral %>% select(P435, P437 = P437_8, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "h. Otro")
  
  P437 <- bind_rows(ENLgeneral437_1, ENLgeneral437_2, ENLgeneral437_3, ENLgeneral437_4, ENLgeneral437_5, ENLgeneral437_6, ENLgeneral437_7, ENLgeneral437_8) %>%
    filter(!is.na(P437)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P435 == "b. No")
  
  rm(ENLgeneral437_1, ENLgeneral437_2, ENLgeneral437_3, ENLgeneral437_4, ENLgeneral437_5, ENLgeneral437_6, ENLgeneral437_7, ENLgeneral437_8)
  
  P437sexo <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(sexo) %>% adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P437edad_group <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P437niv_educ <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P437auto_etnia <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P437estrasoc <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P437area <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P437zona <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P437dom <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(dominio) %>% adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P437region_nat <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P437dep <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P437)) %>%
    ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P437total <- P437 %>%
    select(P437, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P437 != "NA") %>%
    group_by(P437, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P437)) %>% ungroup %>%
    spread(P437, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P437 <- bind_rows(P437total, P437sexo, P437edad_group, P437niv_educ, P437auto_etnia, P437estrasoc, P437area, P437zona, P437dom, P437region_nat, P437dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P437total, P437sexo, P437edad_group, P437niv_educ, P437auto_etnia, P437estrasoc, P437area, P437zona, P437dom, P437region_nat, P437dep)
  
  P437 <- P437 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P438: Actividades que realizó durante su asistencia a las ferias y festivales del libro y la lectura
{
  ENLgeneral <- ENLgeneral %>% mutate(P438_1 = as.numeric(P438_1)) %>%
    mutate(P438_1 = case_when(P438_1 == 1 ~ "a. Sí",
                              P438_1 == 0 ~ "b. No")) %>%
    mutate(P438_2 = as.numeric(P438_2)) %>% mutate(P438_2 = case_when(P438_2 == 1 ~ "a. Sí",
                                                                      P438_2 == 0 ~ "b. No")) %>%
    mutate(P438_3 = as.numeric(P438_3)) %>% mutate(P438_3 = case_when(P438_3 == 1 ~ "a. Sí",
                                                                      P438_3 == 0 ~ "b. No")) %>%
    mutate(P438_4 = as.numeric(P438_4)) %>% mutate(P438_4 = case_when(P438_4 == 1 ~ "a. Sí",
                                                                      P438_4 == 0 ~ "b. No")) %>%
    mutate(P438_5 = as.numeric(P438_5)) %>% mutate(P438_5 = case_when(P438_5 == 1 ~ "a. Sí",
                                                                      P438_5 == 0 ~ "b. No")) %>%
    mutate(P438_6 = as.numeric(P438_6)) %>% mutate(P438_6 = case_when(P438_6 == 1 ~ "a. Sí",
                                                                      P438_6 == 0 ~ "b. No")) %>%
    mutate(P438_7 = as.numeric(P438_7)) %>% mutate(P438_7 = case_when(P438_7 == 1 ~ "a. Sí",
                                                                      P438_7 == 0 ~ "b. No"))
  
  ENLgeneral438_1 <- ENLgeneral %>% select(P435, P438 = P438_1, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "a. Participó en conferencias, conversatorios, talleres, presentaciones de publicaciones")
  ENLgeneral438_2 <- ENLgeneral %>% select(P435, P438 = P438_2, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "b. Participó en actividades: cuentacuentos, recital, lectura en voz alta, lectura colectiva")
  ENLgeneral438_3 <- ENLgeneral %>% select(P435, P438 = P438_3, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "c. Participó en actividades culturales: conciertos, exposiciones, proyecciones de cine, entre otros")
  ENLgeneral438_4 <- ENLgeneral %>% select(P435, P438 = P438_4, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "d. Visitó stands o consultar publicaciones")
  ENLgeneral438_5 <- ENLgeneral %>% select(P435, P438 = P438_5, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "e. Compró publicaciones")
  
  ENLgeneral438_6 <- ENLgeneral %>% select(P435, P438 = P438_6, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "f. Otro")
  ENLgeneral438_7 <- ENLgeneral %>% select(P435, P438 = P438_7, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>% mutate(Tipo = "g. No sabe/No responde")
  
  P438 <- bind_rows(ENLgeneral438_1, ENLgeneral438_2, ENLgeneral438_3, ENLgeneral438_4, ENLgeneral438_5, ENLgeneral438_6, ENLgeneral438_7) %>%
    filter(!is.na(P438)) %>%
    filter(edad >= 18 & edad <= 64) %>% filter(P435 == "a. Sí")
  
  rm(ENLgeneral438_1, ENLgeneral438_2, ENLgeneral438_3, ENLgeneral438_4, ENLgeneral438_5, ENLgeneral438_6, ENLgeneral438_7)
  
  P438sexo <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P438edad_group <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P438niv_educ <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(niv_educ, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P438auto_etnia <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P438estrasoc <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO, Tipo) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P438area <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P438zona <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P438dom <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P438region_nat <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P438dep <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, Tipo, FACTOR200_FINAL) %>% mutate(ID= row_number(P438)) %>%
    ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    
    select(-ID) %>% group_by(NOMBREDD, Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P438total <- P438 %>%
    select(P438, FACTOR200_FINAL, Tipo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P438 != "NA") %>%
    group_by(P438, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P438)) %>% ungroup %>%
    spread(P438, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(Tipo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(Tipo) %>%
    adorn_totals("col") %>% adorn_totals("row") %>% mutate(Desagregacion = "Nacional") %>% mutate(Variables = "Nacional") %>% filter(Tipo != "Total")
  
  P438 <- bind_rows(P438total, P438sexo, P438edad_group, P438niv_educ, P438auto_etnia, P438estrasoc, P438area, P438zona, P438dom, P438region_nat, P438dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P438total, P438sexo, P438edad_group, P438niv_educ, P438auto_etnia, P438estrasoc, P438area, P438zona, P438dom, P438region_nat, P438dep)
  
  P438 <- P438 %>%
    mutate(`a. Sí (%)` = round (100 * `a. Sí` / `Total`, 1)) %>% mutate(`b. No (%)` = round (100 * `b. No` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

# H. IDENTIDAD DE GÉNERO Y ORIENTACIÓN SEXUAL
#****************************************************
#* P439: De acuerdo con su identidad de género, usted se considera
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P439 = case_when(P439 == 1 ~ "a. Hombre", P439 == 2 ~ "b. Mujer",
                            P439 == 3 ~ "c. Transfemenina, mujer trans (transexual, transgénero, travesti)", P439 == 4 ~ "d. Transmasculino, hombre trans",
                            P439 == 5 ~ "e. Persona de género no binario", P439 == 6 ~ "f. Otro",
                            P439 == 7 ~ "g. No quiso responder"))
  
  P439 <- ENLgeneral %>%
    select(P439, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P439sexo <- P439 %>%
    select(P439, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P439edad_group <- P439 %>%
    select(P439, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P439niv_educ <- P439 %>%
    select(P439, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P439auto_etnia <- P439 %>%
    select(P439, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P439estrasoc <- P439 %>%
    select(P439, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P439area <- P439 %>%
    select(P439, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P439zona <- P439 %>%
    select(P439, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P439dom <- P439 %>%
    select(P439, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P439region_nat <- P439 %>%
    select(P439, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P439dep <- P439 %>%
    select(P439, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P439total <- P439 %>%
    select(P439, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P439 != "NA") %>%
    group_by(P439, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P439)) %>% ungroup %>%
    
    spread(P439, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P439 <- bind_rows(P439total, P439sexo, P439edad_group, P439niv_educ, P439auto_etnia, P439estrasoc, P439area, P439zona, P439dom, P439region_nat, P439dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P439total, P439sexo, P439edad_group, P439niv_educ, P439auto_etnia, P439estrasoc, P439area, P439zona, P439dom, P439region_nat, P439dep)
  
  P439 <- P439 %>%
    mutate(`a. Hombre (%)` = round (100 * `a. Hombre` / `Total`, 1)) %>% mutate(`b. Mujer (%)` = round (100 * `b. Mujer` / `Total`, 1)) %>%
    mutate(`c. Transfemenina, mujer trans (transexual, transgénero, travesti) (%)` = round (100 * `c.
                                                                                            Transfemenina, mujer trans (transexual, transgénero, travesti)` / `Total`, 1)) %>%
    mutate(`d. Transmasculino, hombre trans (%)` = round (100 * `d. Transmasculino, hombre trans` /
                                                            `Total`, 1)) %>%
    mutate(`e. Persona de género no binario meses (%)` = round (100 * `e. Persona de género no binario` / `Total`, 1)) %>%
    #mutate(`f. Otro (%)` = round (100 * `f. Otro` / `Total`, 1)) %>%
    mutate(`g. No quiso responder (%)` = round (100 * `g. No quiso responder` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}
#* P440: De acuerdo con su orientación sexual, usted se considera
{
  ENLgeneral <- ENLgeneral %>%
    mutate(P440 = case_when(P440 == 1 ~ "a. Heterosexual (Preferencia por personas del sexo opuesto)",
                            
                            
                            
                            
                            múltiples géneros)",
 
P440 == 2 ~ "b. Gay (Preferencia por personas del mismo sexo)", P440 == 3 ~ "c. Lesbiana (Preferencia por personas del mismo sexo)", P440 == 4 ~ "d. Bisexual (Preferencia por personas de ambos sexos)",
P440 == 5 ~ "e. Pansexual (Preferencia por personas del mismo sexo, ambos sexos,
                           
                           P440 == 6 ~ "f. Asexual (Persona que tiene solo atracción estética y emocional, no
 
experimenta deseo sexual)",
                           P440 == 7 ~ "g. Otro",
                           P440 == 8 ~ "h. Ninguna", P440 == 9 ~ "i. No sabe",
                           P440 == 10 ~ "j. No quiso responder"))
  
  P440 <- ENLgeneral %>%
    select(P440, sexo, edad_group, edad, niv_educ, auto_etnia, ESTRATOSOCIO, area, zona, dominio, region_nat, NOMBREDD, sabe_leer, FACTOR200_FINAL) %>%
    filter(edad >= 18 & edad <= 64)
  
  P440sexo <- P440 %>%
    select(P440, FACTOR200_FINAL, sexo) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(sexo) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(sexo) %>%
    adorn_totals("col") %>%
    select(Desagregacion = sexo, everything()) %>% mutate(Variables = "Sexo")
  
  P440edad_group <- P440 %>%
    select(P440, FACTOR200_FINAL, edad_group) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(edad_group) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(edad_group) %>% adorn_totals("col") %>%
    select(Desagregacion = edad_group, everything()) %>% mutate(Variables = "Grupos de edad")
  
  P440niv_educ <- P440 %>%
    select(P440, FACTOR200_FINAL, niv_educ) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(niv_educ) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(niv_educ) %>% adorn_totals("col") %>%
    select(Desagregacion = niv_educ, everything()) %>% mutate(Variables = "Nivel educativo")
  
  P440auto_etnia <- P440 %>%
    select(P440, FACTOR200_FINAL, auto_etnia) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(auto_etnia) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(auto_etnia) %>% adorn_totals("col") %>%
    select(Desagregacion = auto_etnia, everything()) %>% mutate(Variables = "Autoidentificación étnica")
  
  P440estrasoc <- P440 %>%
    select(P440, FACTOR200_FINAL, ESTRATOSOCIO) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(ESTRATOSOCIO) %>% summarise_all(sum) %>% ungroup() %>% arrange(ESTRATOSOCIO) %>%
    adorn_totals("col") %>%
    select(Desagregacion = ESTRATOSOCIO, everything()) %>% mutate(Variables = "Estrato socioeconómico")
  
  P440area <- P440 %>%
    select(P440, FACTOR200_FINAL, area) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(area) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(area) %>%
    adorn_totals("col") %>%
    select(Desagregacion = area, everything()) %>% mutate(Variables = "Área de residencia")
  
  P440zona <- P440 %>%
    select(P440, FACTOR200_FINAL, zona) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(zona) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(zona) %>%
    adorn_totals("col") %>%
    select(Desagregacion = zona, everything()) %>% mutate(Variables = "Zona geográfica")
  
  P440dom <- P440 %>%
    select(P440, FACTOR200_FINAL, dominio) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>%
    
    filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(dominio) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(dominio) %>%
    adorn_totals("col") %>%
    select(Desagregacion = dominio, everything()) %>% mutate(Variables = "Dominio")
  
  P440region_nat <- P440 %>%
    select(P440, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(region_nat) %>% adorn_totals("col") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Region natural")
  
  P440dep <- P440 %>%
    select(P440, FACTOR200_FINAL, NOMBREDD) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(NOMBREDD) %>%
    summarise_all(sum) %>% ungroup() %>% arrange(NOMBREDD) %>% adorn_totals("col") %>%
    select(Desagregacion = NOMBREDD, everything()) %>% mutate(Variables = "Departamento")
  
  P440total <- P440 %>%
    select(P440, FACTOR200_FINAL, region_nat) %>% mutate(FACTOR200_FINAL = round(FACTOR200_FINAL, 1)) %>% filter(P440 != "NA") %>%
    group_by(P440, FACTOR200_FINAL) %>%
    mutate(ID= row_number(P440)) %>% ungroup %>%
    spread(P440, FACTOR200_FINAL, fill = 0) %>%
    select(-ID) %>% group_by(region_nat) %>%
    summarise_all(sum) %>% ungroup() %>%
    
    arrange(region_nat) %>% adorn_totals("col") %>% adorn_totals("row") %>%
    select(Desagregacion = region_nat, everything()) %>% mutate(Variables = "Nacional") %>% filter(Desagregacion == "Total")
  
  P440 <- bind_rows(P440total, P440sexo, P440edad_group, P440niv_educ, P440auto_etnia, P440estrasoc, P440area, P440zona, P440dom, P440region_nat, P440dep) %>%
    select(Variables, Desagregacion, everything())
  
  rm(P440total, P440sexo, P440edad_group, P440niv_educ, P440auto_etnia, P440estrasoc, P440area, P440zona, P440dom, P440region_nat, P440dep)
  
  P440 <- P440 %>%
    mutate(`a. Heterosexual (Preferencia por personas del sexo opuesto) (%)` = round (100 * `a.
                                                                                      Heterosexual (Preferencia por personas del sexo opuesto)` / `Total`, 1)) %>%
    mutate(`b. Gay (Preferencia por personas del mismo sexo) (%)` = round (100 * `b. Gay (Preferencia por personas del mismo sexo)` / `Total`, 1)) %>%
    mutate(`c. Lesbiana (Preferencia por personas del mismo sexo) (%)` = round (100 * `c. Lesbiana (Preferencia por personas del mismo sexo)` / `Total`, 1)) %>%
    mutate(`d. Bisexual (Preferencia por personas de ambos sexos) (%)` = round (100 * `d. Bisexual (Preferencia por personas de ambos sexos)` / `Total`, 1)) %>%
    mutate(`e. Pansexual (Preferencia por personas del mismo sexo, ambos sexos, múltiples géneros) (%)` = round (100 * `e. Pansexual (Preferencia por personas del mismo sexo, ambos sexos, múltiples géneros)` / `Total`, 1)) %>%
    mutate(`f. Asexual (Persona que tiene solo atracción estética y emocional, no experimenta deseo sexual) (%)` = round (100 * `f. Asexual (Persona que tiene solo atracción estética y emocional, no experimenta deseo sexual)` / `Total`, 1)) %>%
    mutate(`h. Ninguna (%)` = round (100 * `h. Ninguna` / `Total`, 1)) %>% mutate(`g. Otro (%)` = round (100 * `g. Otro` / `Total`, 1)) %>% mutate(`i. No sabe (%)` = round (100 * `i. No sabe` / `Total`, 1)) %>%
    mutate(`j. No quiso responder (%)` = round (100 * `j. No quiso responder` / `Total`, 1)) %>% mutate(`Total (%)` = round (100 * `Total` / `Total`, 1))
}

