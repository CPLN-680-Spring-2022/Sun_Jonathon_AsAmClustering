
# Carnegie Classification ---------------------
FE <- list.files("Data/Carnegie_Classification")
files <- paste0("Data/Carnegie_Classification/",FE)

Carnegie_Variables <- read_xlsx(files[2], 3)

Carnegie_Variables <- Carnegie_Variables %>%
  fill(Variable, Label...2) %>%
  mutate(Variable = toupper(Variable),
         Value = as.numeric(Value)) %>%
  filter(grepl("BASIC",Variable)) 

Carnegie_Classification <- read_xlsx(files[2], 4) %>%
  select(1, starts_with("basic")) 

Recode <- Carnegie_Variables %>%
  filter(Variable == "BASIC2021") %>%
  mutate(Numeric_code = Value + 1)

Carnegie_Classification <- Carnegie_Classification%>%
  mutate(basic2021 = basic2021 + 1,
         Carnegie_Classifcation2021 = recode(basic2021, !!!Recode$Label...4),
         Short_Carnegie = ifelse(str_detect(Carnegie_Classifcation2021,":") == TRUE, 
                                 gsub(":.*","",Carnegie_Classifcation2021),
                                 Carnegie_Classifcation2021))

Universities <- left_join(Universities %>%
                            mutate(UNITID = as.numeric(UNITID)),
                          Carnegie_Classification %>%
                            select(unitid,Carnegie_Classifcation2021, Short_Carnegie) %>%
                            rename(UNITID = unitid), 
                          by = "UNITID","NAME")