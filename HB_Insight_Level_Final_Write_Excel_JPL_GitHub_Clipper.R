#####Hierarchical Bayes for InSight data set with Clipper data formating
####and sampling efficiency fitted to the sampling efficiency paper

##“Copyright 2025, Battelle Energy Alliance, LLC, ALL RIGHTS RESERVED” 


###June 26, 2025

#Load packages
library(R2jags)
library(mcmcplots)
library(Metrics)
library(invgamma)
library(MCMCvis)
library(rjags)
library(runjags)
library(tidyverse)
library(readxl);
library(stringr)
library(collapse)
library(dplyr)
library(readr)
library(ggplot2)
library(bayesplot)
library(listr)
library(openxlsx)
library(WriteXLS)



rm(list = ls()) # Clear the workspace
startTime <- Sys.time()

write_flag<-1 ##Flag to save the results to Excel file. 0 - do not save

raw_data_All<-read_excel("C:/Users/Josh/Documents/MATLAB/Work/NASA/InSight_Data/PPEL_forModel_Draft_v3_Spec_AG_Macro_NO.xlsx",sheet = 5)##Read the data spread sheet


##Select InSight compoenents only
InSight_components<-raw_data_All[str_detect(raw_data_All$"Rollup Level 1: Project", "InSight"), ]
InSight_components<-InSight_components[,1:21]


##Replace NA with "Data"

InSight_components[which(is.na(InSight_components$`Rollup Level 4: Assembly`)),4]<-c("Data")

InSight_components[which(is.na(InSight_components$`Rollup Level 5: Subassembly`)),5]<-c("Data")

InSight_components[which(is.na(InSight_components$`Rollup Level 6: Component`)),6]<-c("Data")


Number_of_samples=1857 ##Total number of Insight samples including spec and implied
Number_of_samples=90

##Parse components into 6 different levels
Components_all<-rsplit(InSight_components,InSight_components[.c('Rollup Level 1: Project', 'Rollup Level 2: Payload / Spacecraft','Rollup Level 3: Instrument / Flight Element','Rollup Level 4: Assembly','Rollup Level 5: Subassembly','Rollup Level 6: Component')][1:Number_of_samples,],drop = FALSE,simplify = TRUE,sort=FALSE)
##Depth of the components tree
Roll_up_depth<-ldepth(Components_all)


##Variables initialization

Number_of_component<-0  ###Total number of components on all levels
Area<-list()
CFU<-list()
Area_sampled<-list()
Pour_fraction<-list()
Sampling_device_type<-list()
Component_area<-list()
Number_of_samples_per_component<-list()
Component_name<-list()

##########################################
Number_of_components_Level_1<-0###Number of components on level 1-InSight
Number_of_components_Level_2<-0
Number_of_components_Level_3<-0
Number_of_components_Level_4<-0
Number_of_components_Level_5<-0
Number_of_components_Level_6<-0



###Sample-wise number of CFUs for each level

CFU_Level_1_samples<-list()
CFU_Level_2_samples<-list()
CFU_Level_3_samples<-list()
CFU_Level_4_samples<-list()
CFU_Level_5_samples<-list()
CFU_Level_6_samples<-list()

##Sample-wise exposure for each level

Exposure_Level_1_samples<-list()
Exposure_Level_2_samples<-list()
Exposure_Level_3_samples<-list()
Exposure_Level_4_samples<-list()
Exposure_Level_5_samples<-list()
Exposure_Level_6_samples<-list()


##Sample-wise sampling device for each level

Sampling_device_Level_1_samples<-list()
Sampling_device_Level_2_samples<-list()
Sampling_device_Level_3_samples<-list()
Sampling_device_Level_4_samples<-list()
Sampling_device_Level_5_samples<-list()
Sampling_device_Level_6_samples<-list()


CFU_All_Levels_Samples<-Components_all
Exposure_All_Levels_Samples<-Components_all
Sampling_device_All_Levels_Samples<-Components_all


##Extract data for all levels as lists

for (i in seq_along((Components_all))){
  
  Number_of_components_Level_1<-Number_of_components_Level_1+1
  
  for (j in seq_along(Components_all[[i]])){
    Number_of_components_Level_2<-Number_of_components_Level_2+1
    
    
    for (k in seq_along(Components_all[[i]][[j]])){
      Number_of_components_Level_3<-Number_of_components_Level_3+1
      
      
      for (l in seq_along(Components_all[[i]][[j]][[k]])){  
        Number_of_components_Level_4<-Number_of_components_Level_4+1
        
        
        
        for (m in seq_along(Components_all[[i]][[j]][[k]][[l]]) )       {
          Number_of_components_Level_5<-Number_of_components_Level_5+1
          
          
          
          for (n in seq_along(Components_all[[i]][[j]][[k]][[l]][[m]] )) {
            
            Number_of_components_Level_6<-Number_of_components_Level_6+1
            
            Number_of_component<-Number_of_component+1
            Area[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12]  
            Component_area[Number_of_component]<-unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12]    ) ##Area of the sampled components
            CFU[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] 
            Area_sampled[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15] 
            Pour_fraction[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19] 
            Sampling_device_type[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [17] 
            Number_of_samples_per_component[Number_of_component]<-nrow(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [17])
            Component_name[Number_of_component]<-Components_all[[i]][[j]][[k]][[l]][[m]][[n]]  [13] 
            ###############################################################################################################
            
            
            
            CFU_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]]<- unlist( as.numeric(unlist((Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  )
            
            CFU_Level_1_samples[[ Number_of_components_Level_1 ]]<-unname(unlist(CFU_All_Levels_Samples[[i]]))
            
            CFU_Level_2_samples[[Number_of_components_Level_2]] <-unname(unlist(CFU_All_Levels_Samples[[i]][[j]]  ))
            
            CFU_Level_3_samples[[Number_of_components_Level_3]] <-unname(unlist(CFU_All_Levels_Samples[[i]][[j]][[k]]  ))
            # 
            CFU_Level_4_samples[[Number_of_components_Level_4]] <-unname(unlist(CFU_All_Levels_Samples[[i]][[j]][[k]][[l]]  ))
            # 
            CFU_Level_5_samples[[Number_of_components_Level_5]] <-unname(unlist(CFU_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]] ))
            # 
            CFU_Level_6_samples[[Number_of_components_Level_6]] <-unname(unlist(CFU_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]] ))
            
            
            Exposure_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]]<-unlist(as.numeric(unlist((Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]),use.names=TRUE)) )
            
            
            Exposure_Level_1_samples[[ Number_of_components_Level_1 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]]))
            
            Exposure_Level_2_samples[[ Number_of_components_Level_2 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]][[j]]      ))
            
            Exposure_Level_3_samples[[ Number_of_components_Level_3 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]][[j]][[k]]      ))
            
            Exposure_Level_4_samples[[ Number_of_components_Level_4 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]][[j]][[k]][[l]]     ))
            
            Exposure_Level_5_samples[[ Number_of_components_Level_5 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]]     ))
            
            Exposure_Level_6_samples[[ Number_of_components_Level_6 ]]<-unname(unlist(Exposure_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]]    ))
            
            
            Sampling_device_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]]<- unlist((unlist((Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [17] ) ,use.names=TRUE   ))  )
            
            
            Sampling_device_Level_1_samples[[ Number_of_components_Level_1 ]]<-unname(unlist(Sampling_device_All_Levels_Samples[[i]]))
            
            Sampling_device_Level_2_samples[[Number_of_components_Level_2]] <-unname(unlist(Sampling_device_All_Levels_Samples[[i]][[j]]  ))
            
            Sampling_device_Level_3_samples[[Number_of_components_Level_3]] <-unname(unlist(Sampling_device_All_Levels_Samples[[i]][[j]][[k]]  ))
            # 
            Sampling_device_Level_4_samples[[Number_of_components_Level_4]] <-unname(unlist(Sampling_device_All_Levels_Samples[[i]][[j]][[k]][[l]]  ))
            # 
            Sampling_device_Level_5_samples[[Number_of_components_Level_5]] <-unname(unlist(Sampling_device_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]] ))
            # 
            Sampling_device_Level_6_samples[[Number_of_components_Level_6]] <-unname(unlist(Sampling_device_All_Levels_Samples[[i]][[j]][[k]][[l]][[m]][[n]] ))
            
            
            
          }
        }
      }
    }
  }
}




##Initialize level-wise variables
Area_Level_0<-Components_all
Area_Level_1<-Components_all
Area_Level_2<-Components_all
Area_Level_3<-Components_all
Area_Level_4<-Components_all
Area_Level_5<-Components_all
Area_Level_6<-Components_all

CFU_Level_1<-Components_all
CFU_Level_2<-Components_all
CFU_Level_3<-Components_all
CFU_Level_4<-Components_all
CFU_Level_5<-Components_all
CFU_Level_6<-Components_all

Samples_Level_1<-Components_all
Samples_Level_2<-Components_all
Samples_Level_3<-Components_all
Samples_Level_4<-Components_all
Samples_Level_5<-Components_all
Samples_Level_6<-Components_all

Names_Level_0<-Components_all
Names_Level_1<-Components_all
Names_Level_2<-Components_all
Names_Level_3<-Components_all
Names_Level_4<-Components_all
Names_Level_5<-Components_all
Names_Level_6<-Components_all
Names_Level_6_1<-Components_all

Exposure_Level_1<-Components_all
Exposure_Level_2<-Components_all
Exposure_Level_3<-Components_all
Exposure_Level_4<-Components_all
Exposure_Level_5<-Components_all
Exposure_Level_6<-Components_all



#Extract level-wise and component-wise data

for (i in seq_along((Components_all))){
  
  #Variables initialization
  CFU_Level_1[[i]]<-numeric(length(Components_all[[i]] ))
  Samples_Level_1[[i]]<-numeric(length(Components_all[[i]] ))
  Area_Level_0[[i]]<-numeric(length(Components_all ))
  Area_Level_1[[i]]<-numeric(length(Components_all[[i]] ))
  Exposure_Level_1[[i]]<-numeric(length(Components_all[[i]] ))
  
  
  for (j in seq_along(Components_all[[i]])){
    #Variables initialization
    Area_Level_2[[i]][[j]]<-numeric(length(Components_all[[i]][[j]] ))
    CFU_Level_2[[i]][[j]]<-numeric(length(Components_all[[i]][[j]] ))
    Samples_Level_2[[i]][[j]]<-numeric(length(Components_all[[i]][[j]] ))
    Exposure_Level_2[[i]][[j]]<-numeric(length(Components_all[[i]][[j]] ))
    
    
    
    # CFU_Level_2_samples[[i]][[j]]<-numeric(length(Components_all[[i]][[j]] ))
    
    
    
    
    for (k in seq_along(Components_all[[i]][[j]])){
      
      Area_Level_3[[i]][[j]][[k]]  <-numeric(length(Components_all[[i]][[j]][[k]] ))
      CFU_Level_3[[i]][[j]][[k]]  <-numeric(length(Components_all[[i]][[j]][[k]] ))
      Samples_Level_3[[i]][[j]][[k]]  <-numeric(length(Components_all[[i]][[j]][[k]] ))
      Exposure_Level_3[[i]][[j]][[k]]  <-numeric(length(Components_all[[i]][[j]][[k]] ))
      
      
      
      for (l in seq_along(Components_all[[i]][[j]][[k]])){  
        Area_Level_4[[i]][[j]][[k]][[l]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]] ))
        CFU_Level_4[[i]][[j]][[k]][[l]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]] ))
        Samples_Level_4[[i]][[j]][[k]][[l]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]] ))
        Exposure_Level_4[[i]][[j]][[k]][[l]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]] ))
        
        
        
        
        
        for (m in seq_along(Components_all[[i]][[j]][[k]][[l]]) )       {
          Area_Level_5[[i]][[j]][[k]][[l]][[m]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]] ))
          CFU_Level_5[[i]][[j]][[k]][[l]][[m]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]] ))
          Samples_Level_5[[i]][[j]][[k]][[l]][[m]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]] ))
          Exposure_Level_5[[i]][[j]][[k]][[l]][[m]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]] ))
          
          
          for (n in seq_along(Components_all[[i]][[j]][[k]][[l]][[m]]    )) {
            
            Area_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] ))
            CFU_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] ))
            Samples_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] ))
            Exposure_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <-numeric(length(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] ))
            Names_Level_6_1[[i]][[j]][[k]][[l]][[m]][[n]]  <-(names(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]]   ))
            
            
            
            ###Components area calculations roll-up
            
            Area_Level_0[[i]]<- Area_Level_0[[i]]+as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ),use.names=TRUE    ))  ###Insight level total
            Area_Level_1[[i]][[j]]  <- Area_Level_1[[i]][[j]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12]  ),use.names=TRUE   ))  
            Area_Level_2[[i]][[j]][[k]]  <- Area_Level_2[[i]][[j]][[k]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ) ,use.names=TRUE   ))  
            Area_Level_3[[i]][[j]][[k]][[l]]  <- Area_Level_3[[i]][[j]][[k]][[l]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ) ,use.names=TRUE   ))  
            Area_Level_4[[i]][[j]][[k]][[l]][[m]]  <- Area_Level_4[[i]][[j]][[k]][[l]][[m]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ) ,use.names=TRUE   ))  
            Area_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  <- Area_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ) ,use.names=TRUE   ))  
            Area_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <- Area_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(  unique(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [12] ) ,use.names=TRUE   ))  
            Area_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]<-unique(   Area_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]    )
            
            
            ###Number of CFUs per component
            CFU_Level_1[[i]][[j]]  <- CFU_Level_1[[i]][[j]]  +as.numeric(unlist(sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20]  ),use.names=TRUE   ))  
            CFU_Level_2[[i]][[j]][[k]]  <- CFU_Level_2[[i]][[j]][[k]]  +as.numeric(unlist(  sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            CFU_Level_3[[i]][[j]][[k]][[l]]  <- CFU_Level_3[[i]][[j]][[k]][[l]]  +as.numeric(unlist(sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            CFU_Level_4[[i]][[j]][[k]][[l]][[m]]  <- CFU_Level_4[[i]][[j]][[k]][[l]][[m]]  +as.numeric(unlist(sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            CFU_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  <- CFU_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   )) 
            
            
            CFU_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <- CFU_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(sum(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            CFU_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]<-unique(   CFU_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]    )
       
            
            
            ###Number of Samples per component
            Samples_Level_1[[i]][[j]]  <- Samples_Level_1[[i]][[j]]  +as.numeric(unlist(lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20]  ),use.names=TRUE   ))  
            Samples_Level_2[[i]][[j]][[k]]  <- Samples_Level_2[[i]][[j]][[k]]  +as.numeric(unlist(  lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            Samples_Level_3[[i]][[j]][[k]][[l]]  <- Samples_Level_3[[i]][[j]][[k]][[l]]  +as.numeric(unlist(  lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            Samples_Level_4[[i]][[j]][[k]][[l]][[m]]  <- Samples_Level_4[[i]][[j]][[k]][[l]][[m]]  +as.numeric(unlist(  lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            Samples_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  <- Samples_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(  lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            Samples_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  <- Samples_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]  +as.numeric(unlist(  lengths(Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [20] ) ,use.names=TRUE   ))  
            Samples_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]<-unique(   Samples_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]    )
            
            
            ##Exposure per component
            Exposure_Level_1[[i]][[j]]  <-Exposure_Level_1[[i]][[j]]+ as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_2[[i]][[j]][[k]]  <-Exposure_Level_2[[i]][[j]][[k]] + as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_3[[i]][[j]][[k]][[l]]  <-Exposure_Level_3[[i]][[j]][[k]][[l]]   + as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_4[[i]][[j]][[k]][[l]][[m]]  <-Exposure_Level_4[[i]][[j]][[k]][[l]][[m]]   + as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]  <-Exposure_Level_5[[i]][[j]][[k]][[l]][[m]][[n]]   + as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]   <-Exposure_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]    + as.numeric(unlist(sum(  Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [15]*Components_all[[i]][[j]][[k]][[l]][[m]][[n]] [19]  ),use.names=TRUE   ))  
            Exposure_Level_6[[i]][[j]][[k]][[l]][[m]][[n]]<-unique(Exposure_Level_6[[i]][[j]][[k]][[l]][[m]][[n]])
            
            
          }
        }
      }
    }
  }
}

# 
# Component_name_unique<-lapply(Component_name,unique)
# Component_number<-parse_number(unlist(Component_name_unique))




##Calculate level-wise and component-wise variables

Level_1_all_components_area<-rapply(Area_Level_1,sum)
Level_2_all_components_area<-rapply(Area_Level_2,sum)
Level_3_all_components_area<-rapply(Area_Level_3,sum)
Level_4_all_components_area<-rapply(Area_Level_4,sum)
Level_5_all_components_area<-rapply(Area_Level_5,sum)
Level_6_all_components_area<-rapply(Area_Level_6,sum)


Level_1_all_components_CFU<-rapply(CFU_Level_1,sum)
Level_2_all_components_CFU<-rapply(CFU_Level_2,sum)
Level_3_all_components_CFU<-rapply(CFU_Level_3,sum)
Level_4_all_components_CFU<-rapply(CFU_Level_4,sum)
Level_5_all_components_CFU<-rapply(CFU_Level_5,sum)
Level_6_all_components_CFU<-rapply(CFU_Level_6,sum)


Level_1_all_components_Samples<-rapply(Samples_Level_1,sum)
Level_2_all_components_Samples<-rapply(Samples_Level_2,sum)
Level_3_all_components_Samples<-rapply(Samples_Level_3,sum)
Level_4_all_components_Samples<-rapply(Samples_Level_4,sum)
Level_5_all_components_Samples<-rapply(Samples_Level_5,sum)
Level_6_all_components_Samples<-rapply(Samples_Level_6,sum)



Level_1_all_components_Exposure<-rapply(Exposure_Level_1,sum)
Level_2_all_components_Exposure<-rapply(Exposure_Level_2,sum)
Level_3_all_components_Exposure<-rapply(Exposure_Level_3,sum)
Level_4_all_components_Exposure<-rapply(Exposure_Level_4,sum)
Level_5_all_components_Exposure<-rapply(Exposure_Level_5,sum)
Level_6_all_components_Exposure<-rapply(Exposure_Level_6,sum)




##Select components from specific level only

Names_Level_1<-unlist(map_depth(Components_all,0,names),use.names = FALSE )
Names_Level_2<-unlist(map_depth(Components_all,1,names),use.names = FALSE )
Names_Level_3<-unlist(map_depth(Components_all,2,names),use.names = FALSE )
Names_Level_4<-unlist(map_depth(Components_all,3,names),use.names = FALSE )
Names_Level_5<-unlist(map_depth(Components_all,4,names),use.names = FALSE )
Names_Level_6<-unlist(map_depth(Components_all,5,names),use.names = FALSE )

Names_Level_1_only<-Names_Level_1[   c(   (which(Names_Level_1!="Data")))    ]
Names_Level_2_only<-Names_Level_2[   c(   (which(Names_Level_2!="Data")))    ]
Names_Level_3_only<-Names_Level_3[   c(   (which(Names_Level_3!="Data")))    ]
Names_Level_4_only<-Names_Level_4[   c(   (which(Names_Level_4!="Data")))    ]
Names_Level_5_only<-Names_Level_5[   c(   (which(Names_Level_5!="Data")))    ]
Names_Level_6_only<-Names_Level_6[   c(   (which(Names_Level_6!="Data")))    ]

##Total area for each components at each level
Area_Level_1_only<-Level_1_all_components_area   [   c(   (which(Names_Level_1!="Data")))    ]
Area_Level_2_only<-  Level_2_all_components_area  [   c(   (which(Names_Level_2!="Data")))    ]
Area_Level_3_only<-  Level_3_all_components_area   [   c(   (which(Names_Level_3!="Data")))    ]
Area_Level_4_only<- Level_4_all_components_area     [   c(   (which(Names_Level_4!="Data")))    ]
Area_Level_5_only<-   Level_5_all_components_area    [   c(   (which(Names_Level_5!="Data")))    ]
Area_Level_6_only<-   Level_6_all_components_area     [   c(   (which(Names_Level_6!="Data")))    ]

##Number of CFUs for each components at each level

CFU_Level_1_only<-Level_1_all_components_CFU   [   c(   (which(Names_Level_1!="Data")))    ]
CFU_Level_2_only<-  Level_2_all_components_CFU  [   c(   (which(Names_Level_2!="Data")))    ]
CFU_Level_3_only<-  Level_3_all_components_CFU   [   c(   (which(Names_Level_3!="Data")))    ]
CFU_Level_4_only<- Level_4_all_components_CFU     [   c(   (which(Names_Level_4!="Data")))    ]
CFU_Level_5_only<-   Level_5_all_components_CFU    [   c(   (which(Names_Level_5!="Data")))    ]
CFU_Level_6_only<-   Level_6_all_components_CFU     [   c(   (which(Names_Level_6!="Data")))    ]

##Number of samples for each components at each level
Samples_Level_1_only<-  Level_1_all_components_Samples   [   c(   (which(Names_Level_1!="Data")))    ]
Samples_Level_2_only<-  Level_2_all_components_Samples  [   c(   (which(Names_Level_2!="Data")))    ]
Samples_Level_3_only<-  Level_3_all_components_Samples   [   c(   (which(Names_Level_3!="Data")))    ]
Samples_Level_4_only<-  Level_4_all_components_Samples     [   c(   (which(Names_Level_4!="Data")))    ]
Samples_Level_5_only<-  Level_5_all_components_Samples    [   c(   (which(Names_Level_5!="Data")))    ]
Samples_Level_6_only<-  Level_6_all_components_Samples     [   c(   (which(Names_Level_6!="Data")))    ]

##Exposure for each components at each level
Exposure_Level_1_only<-  Level_1_all_components_Exposure   [   c(   (which(Names_Level_1!="Data")))    ]
Exposure_Level_2_only<-  Level_2_all_components_Exposure  [   c(   (which(Names_Level_2!="Data")))    ]
Exposure_Level_3_only<-  Level_3_all_components_Exposure   [   c(   (which(Names_Level_3!="Data")))    ]
Exposure_Level_4_only<-  Level_4_all_components_Exposure     [   c(   (which(Names_Level_4!="Data")))    ]
Exposure_Level_5_only<-  Level_5_all_components_Exposure    [   c(   (which(Names_Level_5!="Data")))    ]
Exposure_Level_6_only<-  Level_6_all_components_Exposure     [   c(   (which(Names_Level_6!="Data")))    ]



###Create level indices for samples grouping into components

Level_1_index<-NULL
Level_2_index<-NULL
Level_3_index<-NULL
Level_4_index<-NULL
Level_5_index<-NULL
Level_6_index<-NULL



for (i in  (1:length( Samples_Level_1_only  )))     {
  
  Level_1_index<-c(Level_1_index, rep(i, times=Samples_Level_1_only[i]      )     )    
  
  
}

for (i in  (1:length( Samples_Level_2_only  )))     {
  
  Level_2_index<-c(Level_2_index, rep(i, times=Samples_Level_2_only[i]      )     )    
  
  
}


for (i in  (1:length( Samples_Level_3_only  )))     {
  
  Level_3_index<-c(Level_3_index, rep(i, times=Samples_Level_3_only[i]      )     )    
  
  
}


for (i in  (1:length( Samples_Level_4_only  )))     {
  
  Level_4_index<-c(Level_4_index, rep(i, times=Samples_Level_4_only[i]      )     )    
  
  
}

for (i in  (1:length( Samples_Level_5_only  )))     {
  
  Level_5_index<-c(Level_5_index, rep(i, times=Samples_Level_5_only[i]      )     )    
  
  
}


for (i in  (1:length( Samples_Level_6_only  )))     {
  
  Level_6_index<-c(Level_6_index, rep(i, times=Samples_Level_6_only[i]      )     )    
  
  
}


MLE_Level_1<- CFU_Level_1_only/aggregate( unname(unlist(Exposure_Level_1_samples)),by=list(Level_1_index),sum)

MLE_Level_2<- CFU_Level_2_only/aggregate( unname(unlist(Exposure_Level_2_samples)),by=list(Level_2_index),sum)

MLE_Level_3<- CFU_Level_3_only/aggregate( unname(unlist(Exposure_Level_3_samples)),by=list(Level_3_index),sum)




MLE_Level_4<- CFU_Level_4_only/aggregate( unname(unlist(Exposure_Level_4_samples     [  c(   (which(Names_Level_4!="Data")))   ]  )),by=list(Level_4_index),sum)

MLE_Level_5<- CFU_Level_5_only/aggregate( unname(unlist(Exposure_Level_5_samples     [  c(   (which(Names_Level_5!="Data")))   ]  )),by=list(Level_5_index),sum)

MLE_Level_6<- CFU_Level_6_only/aggregate( unname(unlist(Exposure_Level_6_samples     [  c(   (which(Names_Level_6!="Data")))   ]  )),by=list(Level_6_index),sum)





##Pooled MLE estimate for each level
MLE_Level_1_pooled<- sum(CFU_Level_1_only)/sum(Exposure_Level_1_only)
MLE_Level_2_pooled<- sum(CFU_Level_2_only)/sum(Exposure_Level_2_only)
MLE_Level_3_pooled<- sum(CFU_Level_3_only)/sum(Exposure_Level_3_only)
MLE_Level_4_pooled<- sum(CFU_Level_4_only)/sum(Exposure_Level_4_only)
MLE_Level_5_pooled<- sum(CFU_Level_5_only)/sum(Exposure_Level_5_only)
MLE_Level_6_pooled<- sum(CFU_Level_6_only)/sum(Exposure_Level_6_only)


MLE_Level<-list(MLE_Level_1[,2] , MLE_Level_2[,2],MLE_Level_3[,2],MLE_Level_4[,2],MLE_Level_5[,2],MLE_Level_6[,2] )
Names_Level<-list(Names_Level_1_only,Names_Level_2_only,Names_Level_3_only,Names_Level_4_only,Names_Level_5_only,Names_Level_6_only   )
CFU_Level<-list(CFU_Level_1_only,CFU_Level_2_only,CFU_Level_3_only,CFU_Level_4_only,CFU_Level_5_only,CFU_Level_6_only)

CFU_Level_samples<-list(CFU_Level_1_samples,CFU_Level_2_samples,CFU_Level_3_samples,CFU_Level_4_samples,CFU_Level_5_samples,CFU_Level_6_samples)
Names_Level_samples<-list(Names_Level_1,Names_Level_2,Names_Level_3,Names_Level_4,Names_Level_5,Names_Level_6  )

Exposure_Level_samples<-list(Exposure_Level_1_samples,Exposure_Level_2_samples,Exposure_Level_3_samples,Exposure_Level_4_samples,Exposure_Level_5_samples,Exposure_Level_6_samples)

Sampling_device_Level_samples<-list(Sampling_device_Level_1_samples,Sampling_device_Level_2_samples,Sampling_device_Level_3_samples,Sampling_device_Level_4_samples,Sampling_device_Level_5_samples,Sampling_device_Level_6_samples)

MLE_Level_pooled<-c(MLE_Level_1_pooled,MLE_Level_2_pooled,MLE_Level_3_pooled,MLE_Level_4_pooled,MLE_Level_5_pooled,MLE_Level_6_pooled)

Index_Level<-list(Level_1_index,Level_2_index,Level_3_index,Level_4_index,Level_5_index,Level_6_index)
Level_Component_area<-list(Area_Level_1_only,Area_Level_2_only,Area_Level_3_only,Area_Level_4_only,Area_Level_5_only,Area_Level_6_only)
Level_Exposure<-list(Exposure_Level_1_only,Exposure_Level_2_only,Exposure_Level_3_only,Exposure_Level_4_only,Exposure_Level_5_only,Exposure_Level_6_only)

Summary_all_Levels<-vector(mode = "list", length =Roll_up_depth)
Summary_all_Levels_se<-vector(mode = "list", length =Roll_up_depth)

lambda_Risk<-NULL
CFU_Risk<-NULL    

lambda_Risk_post<-NULL
CFU_Risk_post<-NULL   


CFU_sum<-NULL

#Sampling_density<-list()
Sampling_density_se<-list()


################################## For Loop for roll-up levels
for (i in 1:length(CFU_Level)){
  
  L<-as.character(i)
  title_main<-paste("Level",L)   
  
  ###Sampling efficiency distributions 
  alpha_level<-NULL
  beta_level<-NULL

  CFU_vector_Level<-unlist(CFU_Level_samples[[i]][  c(   (which(Names_Level_samples[[i]]!="Data")))   ]   )
  
  CFU_vector_Level<-ceiling(CFU_vector_Level)
  
  Exposure_Level<-unlist(Exposure_Level_samples[[i]][  c(   (which(Names_Level_samples[[i]]!="Data")))   ]   )
  
  Sampling_device_type_Level<- unlist(    Sampling_device_Level_samples[[i]]   [  c(   (which(Names_Level_samples[[i]]!="Data")))    ]       )
  
  index_puritan_cotton<-str_detect(unlist(Sampling_device_type_Level),"Puritan Cotton") 
  index_tx<-str_detect(unlist(Sampling_device_type_Level),"TX3221") 
  index_no_sampling_device<-!index_puritan_cotton&!index_tx
  
  ###Puritan cotton swab NASA standard w/ Milliflex filter NASA 31% (26%, 36%)
  # alpha_level[index_puritan_cotton]<-101.6087633494236
  # beta_level[index_puritan_cotton]<-226.16144100355572
  
  ###Puritan cotton swab NASA standard ESA 25% (19%, 31%)
  
  alpha_level[index_puritan_cotton]<-45.56431672969219
  beta_level[index_puritan_cotton]<-100.24149680532281
  
  ####TX3211 wipe NASA standard w/ Milliflex filter NASA 27% (6%, 56%)
  
  
  alpha_level[index_tx]<-alpha_tx<-2.8141412121959855
  beta_level[index_tx]<-beta_tx<-7.6086040181595145
  
  ##no sampling device-efficiency 1
  alpha_level[index_no_sampling_device]<-500
  beta_level[index_no_sampling_device]<-1
  
  ###Make variance of spec components very small 
  CFU_vector_Level[index_no_sampling_device]<- CFU_vector_Level[index_no_sampling_device]*1e+6
  Exposure_Level[index_no_sampling_device]<-Exposure_Level[index_no_sampling_device]*1e+6
  
  
  Total_number_of_samples_Level<-length(Index_Level[[i]])
  Number_components_Level<-length(CFU_Level[[i]])
  
  # alpha_efficiency<-2
  # beta_effciency<-alpha_efficiency*2.3
  
  
  MLE_CFU_Level_Pooled<-MLE_Level_pooled[i]*sum(Level_Component_area[[i]])
  
  
  datlist          <- list(y = CFU_vector_Level, 
                           x = Exposure_Level,
                           N = Total_number_of_samples_Level,
                           J=Number_components_Level,
                           Level_index=Index_Level[[i]],
                           Area=Level_Component_area[[i]],
                           alpha_beta=alpha_level,
                           beta_beta=beta_level,
                           pop_mean=MLE_Level_pooled[i]
                           
                           
  ) ##data list for hierarchical Bayes
  


############################################################################


#Hierarchical Bayes model Level-wise with fixed prior sampling efficiency theta. No updating for theta. Posterior is the same as prior

modelString_se_fixed<-"


data {
  for (i in 1:N) {
  theta[i] ~ dbeta(alpha_beta[i],beta_beta[i])#prior beta distribution for sampling efficiency 
  
  }

}




model {
## sampling model/Likelihood
for (i in 1:N){

y[i] ~ dpois(lambda[Level_index[i]]*x[i]*theta[i])#Likelihood


}



## priors and predicitive distribution for each component
for (j in 1:J){
lambda[j] ~ dgamma(alpha, beta)
y_pred[j]~dpois(lambda[j]*Area[j])

}


###Hyper priors


#Constrained non-informative (CNI) hiperprior set to the level average MLE
alpha ~ dgamma(1,2)
beta ~ dgamma(1,(2*pop_mean))

# alpha ~ dgamma(10,10)
# beta ~ dgamma(10,10)


# alpha ~ dgamma(1,1)
# beta ~ dgamma(1,1)

y_pred_pop_var~dpois(pmean_lambda*sum(Area))##Population average predcitive distribution 

pmean_lambda <- alpha/beta #mean of the population average
pstd_lambda <- sqrt(alpha/beta^2)

}
"

theta_list_list<-list()
gelman_list<-list()


for (j in 1:100) {
  jm_samples_se_fixed <- jags.model(file = textConnection(modelString_se_fixed),
                                    data = datlist,
                                    #inits = initslist_samples,
                                    n.chains = 3)
  
  
  jm_coda_samples_se_fixed <- coda.samples(model = jm_samples_se_fixed,
                                           variable.names = c("lambda","pmean_lambda","y_pred","y_pred_pop_var"),
                                           n.iter = 3000)
 
  
  theta_list_list[[j]]<-jm_coda_samples_se_fixed


  gelman_list[[j]]<-gelman.diag(jm_coda_samples_se_fixed, multivariate = FALSE)[[1]][,1]
  
} #For loop for theta sampling 

theta_list<-combine.mcmc(theta_list_list[c(1:length(theta_list_list))], collapse.chains = TRUE,thin = 30)

gelman_list<-rapply( gelman_list, f=function(x) ifelse(is.nan(x),1,x), how="replace" )

gelman<-rowMeans(matrix(unlist(gelman_list), ncol=length(gelman_list)))




Summary_parameters<-summary(theta_list)[[1]]
Summary_CI<-summary(theta_list)[[2]]




lambda_gelman<-unname(gelman[1:(Number_components_Level+1)])

CFU_gelman<-unname(gelman[(Number_components_Level+2):length(gelman)])


#plot bioburden density estimates and predcited number of CFUs

if (i==1){
  
  MCMCplot(theta_list,
           params = c('lambda','pmean_lambda'),ref = MLE_Level_pooled[i],xlab = "Bioburden density, CFUs/m^2",guide_lines=TRUE, labels =  c("InSight" ,"Population variability"),main = c("Bioburden density",title_main),rank = FALSE)
  

  
  
  MCMCplot(theta_list,params = c('y_pred','y_pred_pop_var'),ref= MLE_CFU_Level_Pooled,labels=c("Insight" ,"Population variability" ),main = c("Predicted number of CFUs",title_main),guide_lines=TRUE,xlab = "# of CFUs")
  
  
  
  
}else{
  MCMCplot(theta_list,
           params = c('lambda','pmean_lambda'),ref = MLE_Level_pooled[i],xlab = "Bioburden density, CFUs/m^2",guide_lines=TRUE, labels =  c(parse_number(Names_Level[[i]] ) ,"Population variability"),main = c("Bioburden density",title_main),rank = FALSE)
  
  
  MCMCplot(theta_list,params = c('y_pred','y_pred_pop_var'),ref= MLE_CFU_Level_Pooled,labels=c(parse_number(Names_Level[[i]] ) ,"Population variability" ),main = c("Predicted number of CFUs",title_main),guide_lines=TRUE,xlab = "# of CFUs")
  

  
}

# denplot(theta_list, parms =c('lambda'),ci=0.95,collapse = TRUE,lwd=4 , ylab = "PDF", xlab = "lambda, CFUs/m^2",main = "Bioburden density with 30% prior fixed sampling efficiency, Level 6" ,xlim = c(0, 1000) )
# 
# 
# plot(theta_list[,1])




lambda_summary<-as.data.frame(Summary_parameters[1:(Number_components_Level+1),1:2])
CFU_summary<-as.data.frame(Summary_parameters[(Number_components_Level+2):length(Summary_parameters[,1]),1:2])


CFU_summary[,1]<-ceiling(CFU_summary[,1])



lambda_CI<-as.data.frame(Summary_CI[1:(Number_components_Level+1),])
CFU_CI<-as.data.frame(Summary_CI[(Number_components_Level+2):length(Summary_parameters[,1]),]     )

CFU_CI<-ceiling(CFU_CI)

lambda_CFU_CI_summary<-NULL

lambda_CFU_CI_summary<-bind_cols(lambda_summary,lambda_CI,CFU_summary,CFU_CI, .name_repair = c("minimal"))


lambda_Risk[i]<-sum(lambda_CFU_CI_summary[,2])
CFU_Risk[i]<-sum(lambda_CFU_CI_summary[,9])





row.names(lambda_CFU_CI_summary )<-make.names(c(Names_Level[[i]] ,"Population average")  , unique=TRUE)
#
CI_names<-names(lambda_CI)
#
#
names(lambda_CFU_CI_summary)<-c("Bioburden average, CFU/m^2", "Bioburden std, CFU/m^2",CI_names,"CFU count average","CFU count std",CI_names)
#

lambda_CFU_CI_summary[,15]<-lambda_gelman
lambda_CFU_CI_summary[,16]<-CFU_gelman

lambda_CFU_CI_summary[1,17]<-lambda_Risk[i]
lambda_CFU_CI_summary[1,18]<-CFU_Risk[i]

names(lambda_CFU_CI_summary)<-c("Bioburden average, CFU/m^2", "Bioburden std, CFU/m^2",CI_names,"CFU count average","CFU count std",CI_names,"Gelman lambda","Gelman CFU")



names(lambda_CFU_CI_summary)[17]<-"Level Lambda Risk"
names(lambda_CFU_CI_summary)[18]<-"Level CFU Risk"




Summary_all_Levels_se[[i]]<-lambda_CFU_CI_summary

Sampling_density_se[[i]]<-theta_list


Sampling_density_se[[i]][,(Number_components_Level+2):length(Summary_parameters[,1])] <-ceiling(     Sampling_density_se[[i]]  [,(Number_components_Level+2):length(Summary_parameters[,1])])






colnames(Sampling_density_se[[i]])<-c( paste( Names_Level[[i]][1:Number_components_Level] , ",lambda")    ,"Population average, lambda" , paste( Names_Level[[i]][1:Number_components_Level] , ",CFU")   , "Population average,CFU") 


Sampling_summary_se<-append(Summary_all_Levels_se,Sampling_density_se)






}###For loop for levels end 



endTime <- Sys.time()

print(endTime - startTime)


#print(paste("Run time = ",endTime - startTime ,"mins"))

#Save excel file 

if (write_flag==1){
  
  
  
  
write.xlsx(Sampling_summary_se, file="Insight_Bayes_se_30K_NO_Milliflex_1.xlsx", col_names=TRUE, rowNames=TRUE,  sheetName =c( "Level 1 statistics","Level 2 statistics","Level 3 statistics","Level 4 statistics","Level 5 statistics","Level 6 statistics","Level 1 Sampling","Level 2 Sampling","Level 3 Sampling","Level 4 Sampling","Level 5 Sampling","Level 6 Sampling"))
  
  
  
}






















