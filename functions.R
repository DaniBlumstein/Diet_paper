#Important equations:
#  RQ = CO2 eliminated/O2 consumed
#  EE = 0.06 * (3.941 * VO2 + 1.106 * VCO2)
#  
#  from 14.4 in Leighton book
#  LabDiet 5015 = (26.101/100).71+(19.752/100).83+.54148 = .8907387
#  LabDiet low fat 5015 = (22.8/100).71+(6.6/100).83+.706 = .92266
  

  #import data function
  bring_in_data <- function(data_file, Sex)
  {
    data <- paste(path,data_file,sep="")
    raw <- read_csv(data,
                    col_types = cols(Animal = col_double(), 
                                     StartDate = col_date(format = "%m/%d/%y"),
                                     deltaCO2 = col_double(), 
                                     deltaH2O = col_double(),
                                     H2Oml = col_double(),
                                     Deg_C = col_double(),
                                     VCO2 = col_double(),
                                     StartTime = col_time(format = "%H:%M:%S")))
    
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    raw <- raw %>% 
      mutate(EE = 0.06*(3.941*VO2 + 1.106*VCO2)) %>% 
      mutate(RQ = VCO2/VO2) %>%
      mutate(animal = round(Animal, digits=0)) %>%
      mutate(Animal = NULL) %>%
      mutate(Sex  = Sex) %>%
      unite("DateTime", StartDate:StartTime, remove = FALSE, sep =  " ") %>%
      mutate(weight = 
               ifelse(animal == 0, cageweight0, 
                      ifelse(animal == 1, cageweight1,
                             ifelse(animal == 2, cageweight2,
                                    ifelse(animal == 3, cageweight3,
                                           ifelse(animal == 4, cageweight4,
                                                  ifelse(animal == 5, cageweight5,
                                                         ifelse(animal == 6, cageweight6, NA)))))))) %>% 
      mutate(Animal_ID = 
               ifelse(animal == 0, animalID0, 
                      ifelse(animal == 1, animalID1,
                             ifelse(animal == 2, animalID2,
                                    ifelse(animal == 3, animalID3,
                                           ifelse(animal == 4, animalID4,
                                                  ifelse(animal == 5, animalID5,
                                                         ifelse(animal == 6, animalID6, NA)))))))) %>% 
      mutate(H2Omg_edit = 
               ifelse(hour(StartTime) == 8, H2Omg,
                      ifelse(hour(StartTime) == 7, H2Omg,
                             ifelse(hour(StartTime) == 9, H2Omg,
                                    ifelse(hour(StartTime) == 10, H2Omg,
                                           ifelse(hour(StartTime) == 19, H2Omg,
                                                  ifelse(hour(StartTime) == 20, H2Omg,
                                                         ifelse(hour(StartTime) == 21, H2Omg,
                                                                ifelse(hour(StartTime) == 22, H2Omg,
                                                                       ifelse(hour(StartTime) %!in% c(7,8,9,10,20,21,22,19), H2Omg, NA)))))))))) %>% 
      mutate_at("H2Omg_edit", as.numeric)
    
    #metric <- "corEE"
    
    target <- c(0,1,2,3,4,5,6,7)
    cages <- raw %>% filter(animal %in% target)
    
    
    #start_time <- ymd_hms(subset[[5]][1])
    #begin_experiment <- start_time + dhours(2)
    #end_time <- begin_experiment + dhours(72)
    #filtered <- subset %>% filter(raw$DateTime >= begin_experiment & raw$DateTime <= end_time)
    
    
    return(cages)
  }

  
  #mouse id and weights function
  #will add electrolytes....one day
  weight_ID <- function(date)
  {  
    subset <- electrolyte_data[which(electrolyte_data$experiment_date == date), names(electrolyte_data) %in% c("sex", "mouse_ID", "cage_number", "weight", "Na", "K", "Cl", "TCO2", "BUN", "Crea", "Glu", "iCa", "AnGap", "Hct", "Hb*")]
    
    ids <- subset$mouse_ID
    weights <- as.double(subset$weight)
    
    x = 0
    
    for (i in 1:length(ids))
    {
      assign(paste("animalID", x, sep = ""), ids[i], envir = parent.frame())
      assign(paste("cageweight", x, sep = ""), weights[i], envir = parent.frame())
      x = x + 1
    }
  } 
  
  
  #merge data and subset for 72 hours function
  merge_data <- function(cage)
  {
    start_time <- ymd_hms(cage[[1]][1])
    begin_experiment <- start_time + dhours(2)
    end_time <- begin_experiment + dhours(72)
    filtered <- cage %>% filter(DateTime >= begin_experiment & DateTime <= end_time)
    
    return(filtered)
  }
  