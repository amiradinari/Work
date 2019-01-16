#==========================================================================================
#===========================30 min data ===================================================
#==========================================================================================
#==========================================================================================

library("readxl")
#my_data <- read_excel(file.choose())


# check data only related to desalters
DS_data <- read_excel("C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/30 min data w clusters_OnlyDS tags_NoWW_NoMD_NoChl.xlsx")
DS_data$`Clusters` = as.factor(DS_data$`Clusters`)

# apply decision tree
library(rpart)
names(DS_data)
rDS=rpart(DS_data$`Clusters`~.,data=DS_data[,2:128],method = "class")
library(rpart.plot)
rpart.plot(rDS)


# validate decision tree with the same 30 min cleaned data to get stats
predRDS30 <- predict(object=rDS,DS_data[,2:128],type="class")
t <- table(DS_data$Clusters,predRDS30)
library(caret)
confusionMatrix(t)


#==========================================================================================
#===========================1 min data ====================================================
#==========================================================================================
#==========================================================================================
library(openxlsx)
library(tseries)
library(doParallel)
library(foreach)
library(plyr)
library(parallel)
memory.limit(size = 30000)
setwd("D:/bp data 1min/additional")
no_cores <- detectCores()
# Setup cluster
clust <- makeCluster(no_cores) #This line will take time


#1 min data paths
strAdditionalPath <- "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/1 min data/additional"
strControllersPath <- "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/1 min data/controllers"
strProcessPath <- "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/1 min data/process"


# define reading function ================================================
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist =  parLapply(clust,filenames, function(x){read.csv(file=x,header=T,stringsAsFactors = FALSE)})
  Reduce(function(x,y) {rbind(x,y)}, datalist)}
#=========================================================================

# Read additional materials =====
clust <- makeCluster(no_cores) 
mergeAdditional=multmerge(strAdditionalPath)
stopCluster(clust)

# Read controllers tags =====
clust <- makeCluster(no_cores) 
mergeControllers=multmerge(strControllersPath)
stopCluster(clust)

# Read process tags =====
clust <- makeCluster(no_cores) 
mergeProcess=multmerge(strProcessPath)
stopCluster(clust)


# Clean the merged data : additional

mergeAdditional_back = mergeAdditional
mergeAdditional[5,] = paste(mergeAdditional[5,], mergeAdditional[6,],mergeAdditional[4,], sep="_")
mergeAdditionalCLN=as.numeric((mergeAdditional[,5]))
mergeAdditionalCLN=as.data.frame(mergeAdditionalCLN)
mergeAdditionalCLN=cbind(mergeAdditional[,4],mergeAdditionalCLN)
names(mergeAdditionalCLN)=mergeAdditional[5,4:5]
mergeAdditionalCLN=na.omit(mergeAdditionalCLN)
mergeAdditionalCLN = mergeAdditionalCLN[,1:2]


# Clean the merged data : controllers
mergeControllers_back = mergeControllers 
mergeControllers[5,] = paste(mergeControllers[5,], mergeControllers[6,],mergeControllers[4,], sep="_")

h=5:13
mergeControllersCLN=lapply(h,function(x){as.numeric(mergeControllers[,x])})
mergeControllersCLN=as.data.frame(mergeControllersCLN)
mergeControllersCLN=cbind(mergeControllers[,4],mergeControllersCLN)
names(mergeControllersCLN)=mergeControllers[5,4:13]
mergeControllersCLN=na.omit(mergeControllersCLN)
mergeControllersCLN = mergeControllersCLN[,1:10]

# Clean the merged data : Process
mergeProcess_back = mergeProcess
mergeProcess[4,] = paste(mergeProcess[4,], mergeProcess[5,],mergeProcess[3,], sep="_")

mergeProcessTemp = mergeProcess
names(mergeProcessTemp)=mergeProcessTemp[ 4,]
i=which(mergeProcessTemp[,5]!="")
mergeProcessTemp=mergeProcessTemp[i,4:204]
mergeProcessTemp=mergeProcessTemp[,-26]
mergeProcessTemp=mergeProcessTemp[,-26]
mergeProcessCLN=mergeProcessTemp[,1:199]
mergeProcessTemp = NULL
mergeProcessCLN = mergeProcessCLN[6:nrow(mergeProcessCLN),]

h=2:ncol(mergeProcessCLN)
mergeProcessCLN_temp2 = mergeProcessCLN
mergeProcessCLN_temp=lapply(h,function(x){as.numeric(mergeProcessCLN_temp2[,x])})
mergeProcessCLN_temp=as.data.frame(mergeProcessCLN_temp)
mergeProcessCLN_temp=cbind(mergeProcessCLN[,1],mergeProcessCLN_temp)
names(mergeProcessCLN_temp)=names(mergeProcessCLN)
mergeProcessCLN = mergeProcessCLN_temp

#Merge controller and additional (inner)
names(mergeControllersCLN)
#names(mergeControllersCLN)=c( "Description" , "EXCH BACK PRESSURE.op"    ,  "EXCH BACK PRESSURE.pv",      "EXCH BACK PRESSURE.sp" ,     "#1 DESALTER WATER LEVEL.op","#1 DESALTER WATER LEVEL.pv", "#1 DESALTER WATER LEVEL.sp" ,"#2 DESALTER--TOP OF H2O.op" ,"#2 DESALTER--TOP OF H2O.pv" ,"#2 DESALTER--TOP OF H2O.sp")
mergeControllersCLN$Description_UOM_Tag =as.character(mergeControllersCLN$Description_UOM_Tag)
mergeAdditionalCLN$Description_UOM_Tag =as.character(mergeAdditionalCLN$Description_UOM_Tag)
MergeContr_Add=join(mergeControllersCLN,mergeAdditionalCLN,type="inner")


#remove columns from contadd that are already in the process data
MergeContr_Add[,6]=NULL
MergeContr_Add[,8]=NULL
# Merge columns (innerjoin) of Process, additional and controllers data
mergeProcessCLN$Description_UOM_Tag =as.character(mergeProcessCLN$Description_UOM_Tag)
MergeContr_Add_process=join(MergeContr_Add,mergeProcessCLN,type="inner")
MergeContr_Add_process[,10]=NULL #bclean duplicate column
names(MergeContr_Add_process)


#get the names of the 
c=mergeProcessCLN[1,]

#define shift function to change the position of columns in a data frame
shift=function(variable_name,index,data)
{i=which(colnames(data)==variable_name)
n=ncol(data)
data_r=data
if (i>index )
{data[,index]=data[,i]
names(data)[index]=names(data_r)[i]
data_r[,i]=NULL
data[,index+1:ncol(data)]=NULL
data[,index+1:n]=data_r[,index:ncol(data_r)]
data=data[,1:n]}
if(i<index && index!=n)
{val_i=data[,i]
data[,i]=NULL
data$empty=0
data[,index]=val_i
names(data)[index]=names(data_r)[i]
data_r[,i]=NULL
data[,index:n]=data_r[,index-1:ncol(data_r)]
data=data[,1:n]}
if(i==index)
{print("The variable is there already")}
if(index==n)
{val_i=data[,i]
data[,i]=NULL
data$empty=0
data[,index]=val_i
names(data)[index]=names(data_r)[i]}

return(data)}




#test shift function

temp = mergeControllersCLN
names(temp)
temp2=shift("EXCH BACK PRESSURE.sp",3,temp)
names(temp2)

temp2 = NULL
temp = NULL

# get the names of the 1 min data file
names(MergeContr_Add_process)
View(MergeContr_Add_process)
# get the names of the 30 min data file
names(DS_data)


names(temp2)


############################## change the position of tags in the 1 min data to match 30 min data variables order ---------------------------------------

MergeContr_Add_process=shift("Description_UOM_Tag",1,MergeContr_Add_process)
MergeContr_Add_process=shift("TOTAL REFINRY CRUDE RATE_MBPH_10FC161.PV_MBPH_10FC161.PV",2,MergeContr_Add_process)
MergeContr_Add_process=shift("CRU API GRAV CALC_'API(60F)_10DY162A.PV_'API(60F)_10DY162A.PV",3,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DSALT-#3GRID VOLTX1000_KVOLTS_10EI795.PV_KVOLTS_10EI795.PV",4,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DSALT-#2GRID VOLTX1000_KVOLTS_10EI794.PV_KVOLTS_10EI794.PV",5,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DSALT-#1GRID VOLTX1000_KVOLTS_10EI793.PV_KVOLTS_10EI793.PV",6,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALT - #3 GRID AMPS_AMPS_10II798.PV_AMPS_10II798.PV",7,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALT - #2 GRID AMPS_AMPS_10II797.PV_AMPS_10II797.PV",8,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALT - #1 GRID AMPS_AMPS_10II796.PV_AMPS_10II796.PV",9,MergeContr_Add_process)
MergeContr_Add_process=shift("#2DSALT-#3GRID VOLTX1000_KVOLTS_10EI801.PV_KVOLTS_10EI801.PV",10,MergeContr_Add_process)
MergeContr_Add_process=shift("#2DSALT-#2GRID VOLTX1000_KVOLTS_10EI800.PV_KVOLTS_10EI800.PV",11,MergeContr_Add_process)
MergeContr_Add_process=shift("#2DSALT-#1GRID VOLTX1000_KVOLTS_10EI799.PV_KVOLTS_10EI799.PV",12,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALT - #3 GRID AMPS_AMPS_10II804.PV_AMPS_10II804.PV",13,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALT - #2 GRID AMPS_AMPS_10II803.PV_AMPS_10II803.PV",14,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALT - #1 GRID AMPS_AMPS_10II802.PV_AMPS_10II802.PV",15,MergeContr_Add_process)
MergeContr_Add_process=shift("%WASH WTR TO #2 DESALTER_% OF TOT CRU_10FK5287.pv_% OF TOT CRU_10FK5287.pv",16,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTR INTERFACE LVL_%_10LI59.PV_%_10LI59.PV",17,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DESALTR--TOP OF MUD _%_10LI701A.PV_%_10LI701A.PV",18,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DESALTR--TOP OF H2O_%_10LI701B.PV_%_10LI701B.PV",19,MergeContr_Add_process)
MergeContr_Add_process=shift("#1DESALTR--TOP OF RAG_%_10LI701C.PV_%_10LI701C.PV",20,MergeContr_Add_process)
MergeContr_Add_process=shift("CRUDE FROM #1 DESALTER_DEG F_10TI1765.PV_DEG F_10TI1765.PV",21,MergeContr_Add_process)
MergeContr_Add_process=shift("DEMULSIFIER TANK LEVEL_FEET_10LI1502.PV_FEET_10LI1502.PV",22,MergeContr_Add_process)
MergeContr_Add_process=shift("DEMULSFR TO CRU CHG PUMPS_GPM_10FY437.PV_GPM_10FY437.PV",23,MergeContr_Add_process)
MergeContr_Add_process=shift("#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV_PPM CL_10AI1355.PV",24,MergeContr_Add_process)
MergeContr_Add_process=shift("CRUDE TWR TPA DELTA P_PSID_10py943.PV_PSID_10py943.PV",25,MergeContr_Add_process)
MergeContr_Add_process=shift("CRUDE TO DESALTER_DEG F_10TI380.PV_DEG F_10TI380.PV",26,MergeContr_Add_process)
MergeContr_Add_process=shift("CRUDE FROM #2 DESALTER_DEG F_10TI35.PV_DEG F_10TI35.PV",27,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DSLTR OUTLET PRESS_PSIG_10PI609.PV_PSIG_10PI609.PV",28,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALTR E.MIX VALV DP_PSID_10PI1641.PV_PSID_10PI1641.PV",29,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALTR W.MIX VALV DP_PSID_10PI607.PV_PSID_10PI607.PV",30,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALTER WATER LEVEL_% LVL_10LC701.PV_% LVL_10LC701.PV",31,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTER PRESSURE_PSIG_10PI62.PV_PSIG_10PI62.PV",32,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.PV_%LEVEL_10LC750.PV",33,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALTER E. MIX VALVE_% OPEN_10HC1641.PV_% OPEN_10HC1641.PV",34,MergeContr_Add_process)
MergeContr_Add_process=shift("#1 DESALTER W. MIX VALVE_% OPEN_10HC607.PV_% OPEN_10HC607.PV",35,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTER--TOP OF MUD_%LEVEL_10LI750A.PV_%LEVEL_10LI750A.PV",36,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTER--TOP OF H2O_%LEVEL_10LI750B.PV_%LEVEL_10LI750B.PV",37,MergeContr_Add_process)
MergeContr_Add_process=shift("#2 DESALTER--TOP OF RAG_%LEVEL_10LI750C.PV_%LEVEL_10LI750C.PV",38,MergeContr_Add_process)
MergeContr_Add_process=shift("VDF DIESEL PA_BPH_10FC633.PV_BPH_10FC633.PV",39,MergeContr_Add_process)
MergeContr_Add_process=shift("Wash Wtr to C1360-01/02_BPH_10FI1465.PV_BPH_10FI1465.PV",40,MergeContr_Add_process)
MergeContr_Add_process=shift("Wash Wtr to C1360-03/04_BPH_10FI1466.PV_BPH_10FI1466.PV",41,MergeContr_Add_process)
MergeContr_Add_process=shift("DESALT EFF TO STRIPPER_BPH_10fi3150.pv_BPH_10fi3150.pv",42,MergeContr_Add_process)
MergeContr_Add_process=shift("WTR FM U17 TO #1 DESALTR_BPH WTR_10FC1649.PV_BPH WTR_10FC1649.PV",43,MergeContr_Add_process)
MergeContr_Add_process=shift("E.WASH WTR INLT->#1DSLTR_BPH WW_10fi1639.pv_BPH WW_10fi1639.pv",44,MergeContr_Add_process)
MergeContr_Add_process=shift("W.WASH WTR INLT->#1DSLTR_BPH WW_10fi1640.pv_BPH WW_10fi1640.pv",45,MergeContr_Add_process)
MergeContr_Add_process=shift("VDF TOWER TOP VAP TEMP_DEG F_10TC3110.pv_DEG F_10TC3110.pv",46,MergeContr_Add_process)
MergeContr_Add_process=shift("LIGHT GAS OIL FM COOLER_DEG F_10Tc720.PV_DEG F_10Tc720.PV",47,MergeContr_Add_process)
MergeContr_Add_process=shift("SOUR H2O TO DESALTER_DEG F_10TI3178.PV_DEG F_10TI3178.PV",48,MergeContr_Add_process)
MergeContr_Add_process=shift("PREFRAC TOWER OVHD PRESS_PSIG_10PI863.PV_PSIG_10PI863.PV",49,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFLR TUBE43-BTM-DENS_G/ML_10N13043.pv_G/ML_10N13043.pv",50,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE42 DENSITY_G/ML_10N13042.pv_G/ML_10N13042.pv",51,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE41 DENSITY_G/ML_10N13041.pv_G/ML_10N13041.pv",52,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE40 DENSITY_G/ML_10N13040.pv_G/ML_10N13040.pv",53,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE39 DENSITY_G/ML_10N13039.pv_G/ML_10N13039.pv",54,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE38 DENSITY_G/ML_10N13038.pv_G/ML_10N13038.pv",55,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE37 DENSITY_G/ML_10N13037.pv_G/ML_10N13037.pv",56,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE36 DENSITY_G/ML_10N13036.pv_G/ML_10N13036.pv",57,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE35 DENSITY_G/ML_10N13035.pv_G/ML_10N13035.pv",58,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE34 DENSITY_G/ML_10N13034.pv_G/ML_10N13034.pv",59,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE33 DENSITY_G/ML_10N13033.pv_G/ML_10N13033.pv",60,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE32 DENSITY_G/ML_10N13032.pv_G/ML_10N13032.pv",61,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE31 DENSITY_G/ML_10N13031.pv_G/ML_10N13031.pv",62,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE30 DENSITY_G/ML_10N13030.pv_G/ML_10N13030.pv",63,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE29 DENSITY_G/ML_10N13029.pv_G/ML_10N13029.pv",64,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE28 DENSITY_G/ML_10N13028.pv_G/ML_10N13028.pv",65,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE27 DENSITY_G/ML_10N13027.pv_G/ML_10N13027.pv",66,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE26 DENSITY_G/ML_10N13026.pv_G/ML_10N13026.pv",67,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE25 DENSITY_G/ML_10N13025.pv_G/ML_10N13025.pv",68,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE24 DENSITY_G/ML_10N13024.pv_G/ML_10N13024.pv",69,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE23 DENSITY_G/ML_10N13023.pv_G/ML_10N13023.pv",70,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE22 DENSITY_G/ML_10N13022.pv_G/ML_10N13022.pv",71,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE21 DENSITY_G/ML_10N13021.pv_G/ML_10N13021.pv",72,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE20 DENSITY_G/ML_10N13020.pv_G/ML_10N13020.pv",73,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE19 DENSITY_G/ML_10N13019.pv_G/ML_10N13019.pv",74,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE18 DENSITY_G/ML_10N13018.pv_G/ML_10N13018.pv",75,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE17 DENSITY_G/ML_10N13017.pv_G/ML_10N13017.pv",76,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE16 DENSITY_G/ML_10N13016.pv_G/ML_10N13016.pv",77,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE15 DENSITY_G/ML_10N13015.pv_G/ML_10N13015.pv",78,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE14 DENSITY_G/ML_10N13014.pv_G/ML_10N13014.pv",79,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE13 DENSITY_G/ML_10N13013.pv_G/ML_10N13013.pv",80,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE12 DENSITY_G/ML_10N13012.pv_G/ML_10N13012.pv",81,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE11 DENSITY_G/ML_10N13011.pv_G/ML_10N13011.pv",82,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE10 DENSITY_G/ML_10N13010.pv_G/ML_10N13010.pv",83,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE9 DENSITY_G/ML_10N13009.pv_G/ML_10N13009.pv",84,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE8 DENSITY_G/ML_10N13008.pv_G/ML_10N13008.pv",85,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE7 DENSITY_G/ML_10N13007.pv_G/ML_10N13007.pv",86,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE6 DENSITY_G/ML_10N13006.pv_G/ML_10N13006.pv",87,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE5 DENSITY_G/ML_10N13005.pv_G/ML_10N13005.pv",88,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE4 DENSITY_G/ML_10N13004.pv_G/ML_10N13004.pv",89,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE3 DENSITY_G/ML_10N13003.pv_G/ML_10N13003.pv",90,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE2 DENSITY_G/ML_10N13002.pv_G/ML_10N13002.pv",91,MergeContr_Add_process)
MergeContr_Add_process=shift("#1PROFILR TUBE1-TOP-DENS_G/ML_10N13001.pv_G/ML_10N13001.pv",92,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFLR TUBE43-BTM-DENS_G/ML_10N13243.pv_G/ML_10N13243.pv",93,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE42 DENSITY_G/ML_10N13242.pv_G/ML_10N13242.pv",94,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE41 DENSITY_G/ML_10N13241.pv_G/ML_10N13241.pv",95,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE40 DENSITY_G/ML_10N13240.pv_G/ML_10N13240.pv",96,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE39 DENSITY_G/ML_10N13239.pv_G/ML_10N13239.pv",97,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE38 DENSITY_G/ML_10N13238.pv_G/ML_10N13238.pv",98,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE37 DENSITY_G/ML_10N13237.pv_G/ML_10N13237.pv",99,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE36 DENSITY_G/ML_10N13236.pv_G/ML_10N13236.pv",100,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE35 DENSITY_G/ML_10N13235.pv_G/ML_10N13235.pv",101,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE34 DENSITY_G/ML_10N13234.pv_G/ML_10N13234.pv",102,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE33 DENSITY_G/ML_10N13233.pv_G/ML_10N13233.pv",103,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE32 DENSITY_G/ML_10N13232.pv_G/ML_10N13232.pv",104,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE31 DENSITY_G/ML_10N13231.pv_G/ML_10N13231.pv",105,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE30 DENSITY_G/ML_10N13230.pv_G/ML_10N13230.pv",106,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE29 DENSITY_G/ML_10N13229.pv_G/ML_10N13229.pv",107,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE28 DENSITY_G/ML_10N13228.pv_G/ML_10N13228.pv",108,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE27 DENSITY_G/ML_10N13227.pv_G/ML_10N13227.pv",109,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE26 DENSITY_G/ML_10N13226.pv_G/ML_10N13226.pv",110,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE25 DENSITY_G/ML_10N13225.pv_G/ML_10N13225.pv",111,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE24 DENSITY_G/ML_10N13224.pv_G/ML_10N13224.pv",112,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE23 DENSITY_G/ML_10N13223.pv_G/ML_10N13223.pv",113,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE22 DENSITY_G/ML_10N13222.pv_G/ML_10N13222.pv",114,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE21 DENSITY_G/ML_10N13221.pv_G/ML_10N13221.pv",115,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE20 DENSITY_G/ML_10N13220.pv_G/ML_10N13220.pv",116,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE19 DENSITY_G/ML_10N13219.pv_G/ML_10N13219.pv",117,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE18 DENSITY_G/ML_10N13218.pv_G/ML_10N13218.pv",118,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE17 DENSITY_G/ML_10N13217.pv_G/ML_10N13217.pv",119,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE16 DENSITY_G/ML_10N13216.pv_G/ML_10N13216.pv",120,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE15 DENSITY_G/ML_10N13215.pv_G/ML_10N13215.pv",121,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE14 DENSITY_G/ML_10N13214.pv_G/ML_10N13214.pv",122,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE13 DENSITY_G/ML_10N13213.pv_G/ML_10N13213.pv",123,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE12 DENSITY_G/ML_10N13212.pv_G/ML_10N13212.pv",124,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE11 DENSITY_G/ML_10N13211.pv_G/ML_10N13211.pv",125,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE10 DENSITY_G/ML_10N13210.pv_G/ML_10N13210.pv",126,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE9 DENSITY_G/ML_10N13209.pv_G/ML_10N13209.pv",127,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE8 DENSITY_G/ML_10N13208.pv_G/ML_10N13208.pv",128,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE7 DENSITY_G/ML_10N13207.pv_G/ML_10N13207.pv",129,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE6 DENSITY_G/ML_10N13206.pv_G/ML_10N13206.pv",130,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE5 DENSITY_G/ML_10N13205.pv_G/ML_10N13205.pv",131,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE4 DENSITY_G/ML_10N13204.pv_G/ML_10N13204.pv",132,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE3 DENSITY_G/ML_10N13203.pv_G/ML_10N13203.pv",133,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE2 DENSITY_G/ML_10N13202.pv_G/ML_10N13202.pv",134,MergeContr_Add_process)
MergeContr_Add_process=shift("#2PROFILR TUBE1-TOP-DENS_G/ML_10N13201.pv_G/ML_10N13201.pv",135,MergeContr_Add_process)
MergeContr_Add_process=shift("DWS WATER TO DESALTER_BPH_10FC928.PV_BPH_10FC928.PV",136,MergeContr_Add_process)
MergeContr_Add_process=shift("SOUR H2O TO #1 DESALTER_BPH_10FI523.PV_BPH_10FI523.PV",137,MergeContr_Add_process)



########################

# Implicit step to replace the names of 30 min data columns (done in Excel) according to the names of the 1 min data


# Don't use this !! validate decision tree with the 1 min cleaned data to get stats and cluster variable
predRDS1min <- predict(object=rDS,MergeContr_Add_process[,2:137],type="class")
MergeContr_Add_process$Clusters = predRDS1min
t <- table(MergeContr_Add_process$Clusters,predRDS1min)
library(caret)
confusionMatrix(t)


names(MergeContr_Add_process)

dtChlora = cbind(MergeContr_Add_process[,1],MergeContr_Add_process[,24],MergeContr_Add_process[,207])
dtChlora = cbind(MergeContr_Add_process[,1],MergeContr_Add_process[,13],MergeContr_Add_process[,207])#amperage
dtChlora=as.data.frame(dtChlora)
write.table(dtChlora, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/mydata.txt", sep=";") 
names(dtChlora) = c(";timestamp","chlorides","Class")

dtChlora=cbind(All_data_Fltr[,2],All_data_Fltr[,3])
dtChlora_light =dtChlora[50:60,]
dtChlora_light$`;timestamp` =as.Date(dtChlora_light$`;timestamp`,"%d%b%y")
format(dtChlora_light$`;timestamp`, format="%m/%d/%Y")
write.table(dtChlora_light, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/mydata.txt", sep=";") 
View(dtChlora_light)

names(MergeContr_Add_process)

# -----------------
#Clean RAM
c = NULL
clust = NULL
DS_data= NULL
dtChlora= NULL
dtChlora_light= NULL
mergeAdditional= NULL
mergeAdditional_back= NULL
mergeAdditionalCLN= NULL
mergeControllers= NULL
mergeControllers_back= NULL
mergeControllersCLN= NULL
MergeContr_Add= NULL
mergeProcess= NULL
mergeProcess_back= NULL
mergeProcessCLN= NULL
mergeProcessCLN_temp= NULL
mergeProcessCLN_temp2= NULL
temp = NULL
temp2 = NULL

gc()

# remove data when chlorides > 2
min_chloride = 0.013
max_chloride = 2

i=which(MergeContr_Add_process$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV_PPM CL_10AI1355.PV`<max_chloride)
All_data_Fltr=MergeContr_Add_process[i,]

names(MergeContr_Add_process)
names(All_data_Fltr)


# remove duplicate data
All_data_Fltr[,199]   = NULL
All_data_Fltr[,187]   = NULL
All_data_Fltr[,185]   = NULL
All_data_Fltr[,180]   = NULL
All_data_Fltr[,168]   = NULL
All_data_Fltr[,156]   = NULL
All_data_Fltr[,155]   = NULL
All_data_Fltr[,154]   = NULL
All_data_Fltr[,153]   = NULL
All_data_Fltr[,152]   = NULL
All_data_Fltr[,197]   = NULL
names(All_data_Fltr)

#Adjust 1 min positions to match cleaned 30 min data (no WW no Mud & no chlorides)

All_data_Fltr=shift("WTR FM U17 TO #1 DESALTR_BPH WTR_10FC1649.PV_BPH WTR_10FC1649.PV",196,All_data_Fltr)
All_data_Fltr=shift("Wash Wtr to C1360-03/04_BPH_10FI1466.PV_BPH_10FI1466.PV",196,All_data_Fltr)
All_data_Fltr=shift("Wash Wtr to C1360-01/02_BPH_10FI1465.PV_BPH_10FI1465.PV",196,All_data_Fltr)
All_data_Fltr=shift("#2 DESALTER--TOP OF MUD_%LEVEL_10LI750A.PV_%LEVEL_10LI750A.PV",196,All_data_Fltr)
All_data_Fltr=shift("#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV_PPM CL_10AI1355.PV",196,All_data_Fltr)
All_data_Fltr=shift("#1DESALTR--TOP OF MUD _%_10LI701A.PV_%_10LI701A.PV",196,All_data_Fltr)
All_data_Fltr=shift("%WASH WTR TO #2 DESALTER_% OF TOT CRU_10FK5287.pv_% OF TOT CRU_10FK5287.pv",196,All_data_Fltr)


names(All_data_Fltr)






# validate decision tree with the 1 min cleaned data to get stats and cluster variable --- Pending !!!
predRDS1min <- predict(object=rDS,All_data_Fltr[,2:128],type="class")
All_data_Fltr$Clusters = predRDS1min
t <- table(All_data_Fltr$Clusters,predRDS1min)



write.xlsx(All_data_Fltr_back, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/All_data_fltr.xlsx") 

# Clean not required data for the chlorides model
All_data_Fltr = All_data_Fltr_back

All_data_Fltr[,195]  = NULL
All_data_Fltr[,193]  = NULL
All_data_Fltr[,189]  = NULL
All_data_Fltr[,188]  = NULL
All_data_Fltr[,187]  = NULL
All_data_Fltr[,186]  = NULL
All_data_Fltr[,185]  = NULL
All_data_Fltr[,184]  = NULL
All_data_Fltr[,183]  = NULL
All_data_Fltr[,182]  = NULL
All_data_Fltr[,181]  = NULL
All_data_Fltr[,180]  = NULL
All_data_Fltr[,179]  = NULL
All_data_Fltr[,178]  = NULL
All_data_Fltr[,177]  = NULL
All_data_Fltr[,176]  = NULL
All_data_Fltr[,175]  = NULL
All_data_Fltr[,174]  = NULL
All_data_Fltr[,173]  = NULL
All_data_Fltr[,172]  = NULL
All_data_Fltr[,170]  = NULL
All_data_Fltr[,169]  = NULL
All_data_Fltr[,168]  = NULL
All_data_Fltr[,167]  = NULL
All_data_Fltr[,166]  = NULL
All_data_Fltr[,165]  = NULL
All_data_Fltr[,164]  = NULL
All_data_Fltr[,163]  = NULL
All_data_Fltr[,162]  = NULL
All_data_Fltr[,161]  = NULL
All_data_Fltr[,160]  = NULL
All_data_Fltr[,159]  = NULL
All_data_Fltr[,158]  = NULL
All_data_Fltr[,157]  = NULL
All_data_Fltr[,156]  = NULL
All_data_Fltr[,155]  = NULL
All_data_Fltr[,154]  = NULL
All_data_Fltr[,153]  = NULL
All_data_Fltr[,152]  = NULL
All_data_Fltr[,151]  = NULL
All_data_Fltr[,150]  = NULL
All_data_Fltr[,149]  = NULL
All_data_Fltr[,148]  = NULL
All_data_Fltr[,147]  = NULL
All_data_Fltr[,146]  = NULL
All_data_Fltr[,145]  = NULL
All_data_Fltr[,144]  = NULL
All_data_Fltr[,143]  = NULL
All_data_Fltr[,142]  = NULL
All_data_Fltr[,141]  = NULL
All_data_Fltr[,140]  = NULL
All_data_Fltr[,139]  = NULL
All_data_Fltr[,133]  = NULL
All_data_Fltr[,132]  = NULL
All_data_Fltr[,131]  = NULL


names(All_data_Fltr)
names(DS_data)


#library(forecast)
names(All_data_Fltr)



#Extract 1 min data (not available in 30 min data) to perform range and filter coefficient identification

All_data_Fltr_onlyextra = cbind(All_data_Fltr[,1],All_data_Fltr[,129:142])
write.xlsx(All_data_Fltr_onlyextra, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/All_data_Fltr_onlyextra.xlsx") 

# Remove additional variables (VFD, Prefrac, LGO)
All_data_Fltr[,42]  = NULL
All_data_Fltr[,40]  = NULL
All_data_Fltr[,39]  = NULL
All_data_Fltr[,35]  = NULL
RnageMin = 5.5
Rangemax = 11

All_data_Fltr_back138 = All_data_Fltr
All_data_Fltr = All_data_Fltr_back138
i=which(All_data_Fltr$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV`<Rangemax) 
All_data_Fltr=All_data_Fltr[i,]
i = NULL

i=which(RnageMin<All_data_Fltr$`TOTAL REFINRY CRUDE RATE_MBPH_10FC161.pv` & All_data_Fltr$`TOTAL REFINRY CRUDE RATE_MBPH_10FC161.pv`<Rangemax) 
All_data_Fltr=All_data_Fltr[i,]
All_data_Fltr=All_data_Fltr[i,]


#define exp smoothing funcion
exponentiel_smoothing=function(data,alpha,i)
{x=data[,i]
S_now=c()
for(j in 1:length(x) )
{if (j==1)
  S_now[j]=S_pre=x[1]
else
  S_now[j]=alpha*S_pre+(1-alpha)*x[j]
S_pre=S_now[j]
}

return(S_now)}


# clean missing values
All_data_FltrCLN = na.omit(All_data_Fltr)


#Excrtract chlorides smoothed and non smoothed
names(All_data_FltrCLN)

Chlorides_NotSmoothed = All_data_Fltr
Chlorides_Smoothed = Chlorides_NotSmoothed
Chlorides_Smoothed[,136] = exponentiel_smoothing(Chlorides_NotSmoothed,0.01,136)


All_data_Fltr_onlyextra = cbind(All_data_Fltr[,1],All_data_Fltr[,129:142])
write.xlsx(All_data_Fltr_onlyextra, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/All_data_Fltr_onlyextra.xlsx") 

names(All_data_FltrCLN)

# smooth variables
All_data_FltrCLN[,2]=exponentiel_smoothing(All_data_FltrCLN,0.34,2)
All_data_FltrCLN[,3]=exponentiel_smoothing(All_data_FltrCLN,0.01,3)
All_data_FltrCLN[,4]=exponentiel_smoothing(All_data_FltrCLN,0.63,4)
All_data_FltrCLN[,5]=exponentiel_smoothing(All_data_FltrCLN,0.47,5)
All_data_FltrCLN[,6]=exponentiel_smoothing(All_data_FltrCLN,0.83,6)
All_data_FltrCLN[,7]=exponentiel_smoothing(All_data_FltrCLN,0.71,7)
All_data_FltrCLN[,8]=exponentiel_smoothing(All_data_FltrCLN,0.56,8)
All_data_FltrCLN[,9]=exponentiel_smoothing(All_data_FltrCLN,0.8,9)
All_data_FltrCLN[,10]=exponentiel_smoothing(All_data_FltrCLN,0.76,10)
All_data_FltrCLN[,11]=exponentiel_smoothing(All_data_FltrCLN,0.77,11)
All_data_FltrCLN[,12]=exponentiel_smoothing(All_data_FltrCLN,0.63,12)
All_data_FltrCLN[,13]=exponentiel_smoothing(All_data_FltrCLN,0.19,13)
All_data_FltrCLN[,14]=exponentiel_smoothing(All_data_FltrCLN,0.23,14)
All_data_FltrCLN[,15]=exponentiel_smoothing(All_data_FltrCLN,0.23,15)
All_data_FltrCLN[,16]=exponentiel_smoothing(All_data_FltrCLN,0.99,16)
All_data_FltrCLN[,17]=exponentiel_smoothing(All_data_FltrCLN,0.19,17)
All_data_FltrCLN[,18]=exponentiel_smoothing(All_data_FltrCLN,0.19,18)
All_data_FltrCLN[,19]=exponentiel_smoothing(All_data_FltrCLN,0.01,19)
All_data_FltrCLN[,20]=exponentiel_smoothing(All_data_FltrCLN,0.01,20)
All_data_FltrCLN[,21]=exponentiel_smoothing(All_data_FltrCLN,0.37,21)
All_data_FltrCLN[,22]=exponentiel_smoothing(All_data_FltrCLN,0.62,22)
All_data_FltrCLN[,23]=exponentiel_smoothing(All_data_FltrCLN,0.01,23)
All_data_FltrCLN[,24]=exponentiel_smoothing(All_data_FltrCLN,0.34,24)
All_data_FltrCLN[,25]=exponentiel_smoothing(All_data_FltrCLN,0.43,25)
All_data_FltrCLN[,26]=exponentiel_smoothing(All_data_FltrCLN,0.18,26)
All_data_FltrCLN[,27]=exponentiel_smoothing(All_data_FltrCLN,0.18,27)
All_data_FltrCLN[,28]=exponentiel_smoothing(All_data_FltrCLN,0.2,28)
All_data_FltrCLN[,29]=exponentiel_smoothing(All_data_FltrCLN,0.42,29)
All_data_FltrCLN[,30]=exponentiel_smoothing(All_data_FltrCLN,0.86,30)
All_data_FltrCLN[,31]=exponentiel_smoothing(All_data_FltrCLN,0.01,31)
All_data_FltrCLN[,32]=exponentiel_smoothing(All_data_FltrCLN,0.01,32)
All_data_FltrCLN[,33]=exponentiel_smoothing(All_data_FltrCLN,0.86,33)
All_data_FltrCLN[,34]=exponentiel_smoothing(All_data_FltrCLN,0.86,34)
All_data_FltrCLN[,35]=exponentiel_smoothing(All_data_FltrCLN,0.01,35)
All_data_FltrCLN[,36]=exponentiel_smoothing(All_data_FltrCLN,0.73,36)
All_data_FltrCLN[,37]=exponentiel_smoothing(All_data_FltrCLN,0.72,37)
All_data_FltrCLN[,38]=exponentiel_smoothing(All_data_FltrCLN,0.14,38)
All_data_FltrCLN[,39]=exponentiel_smoothing(All_data_FltrCLN,0.56,39)
All_data_FltrCLN[,40]=exponentiel_smoothing(All_data_FltrCLN,0.59,40)
All_data_FltrCLN[,41]=exponentiel_smoothing(All_data_FltrCLN,0.58,41)
All_data_FltrCLN[,42]=exponentiel_smoothing(All_data_FltrCLN,0.51,42)
All_data_FltrCLN[,43]=exponentiel_smoothing(All_data_FltrCLN,0.46,43)
All_data_FltrCLN[,44]=exponentiel_smoothing(All_data_FltrCLN,0.43,44)
All_data_FltrCLN[,45]=exponentiel_smoothing(All_data_FltrCLN,0.4,45)
All_data_FltrCLN[,46]=exponentiel_smoothing(All_data_FltrCLN,0.38,46)
All_data_FltrCLN[,47]=exponentiel_smoothing(All_data_FltrCLN,0.34,47)
All_data_FltrCLN[,48]=exponentiel_smoothing(All_data_FltrCLN,0.37,48)
All_data_FltrCLN[,49]=exponentiel_smoothing(All_data_FltrCLN,0.36,49)
All_data_FltrCLN[,50]=exponentiel_smoothing(All_data_FltrCLN,0.36,50)
All_data_FltrCLN[,51]=exponentiel_smoothing(All_data_FltrCLN,0.37,51)
All_data_FltrCLN[,52]=exponentiel_smoothing(All_data_FltrCLN,0.33,52)
All_data_FltrCLN[,53]=exponentiel_smoothing(All_data_FltrCLN,0.32,53)
All_data_FltrCLN[,54]=exponentiel_smoothing(All_data_FltrCLN,0.29,54)
All_data_FltrCLN[,55]=exponentiel_smoothing(All_data_FltrCLN,0.28,55)
All_data_FltrCLN[,56]=exponentiel_smoothing(All_data_FltrCLN,0.34,56)
All_data_FltrCLN[,57]=exponentiel_smoothing(All_data_FltrCLN,0.44,57)
All_data_FltrCLN[,58]=exponentiel_smoothing(All_data_FltrCLN,0.41,58)
All_data_FltrCLN[,59]=exponentiel_smoothing(All_data_FltrCLN,0.28,59)
All_data_FltrCLN[,60]=exponentiel_smoothing(All_data_FltrCLN,0.31,60)
All_data_FltrCLN[,61]=exponentiel_smoothing(All_data_FltrCLN,0.19,61)
All_data_FltrCLN[,62]=exponentiel_smoothing(All_data_FltrCLN,0.2,62)
All_data_FltrCLN[,63]=exponentiel_smoothing(All_data_FltrCLN,0.17,63)
All_data_FltrCLN[,64]=exponentiel_smoothing(All_data_FltrCLN,0.05,64)
All_data_FltrCLN[,65]=exponentiel_smoothing(All_data_FltrCLN,0.29,65)
All_data_FltrCLN[,66]=exponentiel_smoothing(All_data_FltrCLN,0.4,66)
All_data_FltrCLN[,67]=exponentiel_smoothing(All_data_FltrCLN,0.8,67)
All_data_FltrCLN[,68]=exponentiel_smoothing(All_data_FltrCLN,0.77,68)
All_data_FltrCLN[,69]=exponentiel_smoothing(All_data_FltrCLN,0.77,69)
All_data_FltrCLN[,70]=exponentiel_smoothing(All_data_FltrCLN,0.79,70)
All_data_FltrCLN[,71]=exponentiel_smoothing(All_data_FltrCLN,0.78,71)
All_data_FltrCLN[,72]=exponentiel_smoothing(All_data_FltrCLN,0.78,72)
All_data_FltrCLN[,73]=exponentiel_smoothing(All_data_FltrCLN,0.79,73)
All_data_FltrCLN[,74]=exponentiel_smoothing(All_data_FltrCLN,0.82,74)
All_data_FltrCLN[,75]=exponentiel_smoothing(All_data_FltrCLN,0.81,75)
All_data_FltrCLN[,76]=exponentiel_smoothing(All_data_FltrCLN,0.81,76)
All_data_FltrCLN[,77]=exponentiel_smoothing(All_data_FltrCLN,0.84,77)
All_data_FltrCLN[,78]=exponentiel_smoothing(All_data_FltrCLN,0.85,78)
All_data_FltrCLN[,79]=exponentiel_smoothing(All_data_FltrCLN,0.87,79)
All_data_FltrCLN[,80]=exponentiel_smoothing(All_data_FltrCLN,0.88,80)
All_data_FltrCLN[,81]=exponentiel_smoothing(All_data_FltrCLN,0.88,81)
All_data_FltrCLN[,82]=exponentiel_smoothing(All_data_FltrCLN,0.93,82)
All_data_FltrCLN[,83]=exponentiel_smoothing(All_data_FltrCLN,0.97,83)
All_data_FltrCLN[,84]=exponentiel_smoothing(All_data_FltrCLN,0.9,84)
All_data_FltrCLN[,85]=exponentiel_smoothing(All_data_FltrCLN,0.86,85)
All_data_FltrCLN[,86]=exponentiel_smoothing(All_data_FltrCLN,0.84,86)
All_data_FltrCLN[,87]=exponentiel_smoothing(All_data_FltrCLN,0.84,87)
All_data_FltrCLN[,88]=exponentiel_smoothing(All_data_FltrCLN,0.84,88)
All_data_FltrCLN[,89]=exponentiel_smoothing(All_data_FltrCLN,0.79,89)
All_data_FltrCLN[,90]=exponentiel_smoothing(All_data_FltrCLN,0.78,90)
All_data_FltrCLN[,91]=exponentiel_smoothing(All_data_FltrCLN,0.76,91)
All_data_FltrCLN[,92]=exponentiel_smoothing(All_data_FltrCLN,0.77,92)
All_data_FltrCLN[,93]=exponentiel_smoothing(All_data_FltrCLN,0.77,93)
All_data_FltrCLN[,94]=exponentiel_smoothing(All_data_FltrCLN,0.76,94)
All_data_FltrCLN[,95]=exponentiel_smoothing(All_data_FltrCLN,0.73,95)
All_data_FltrCLN[,96]=exponentiel_smoothing(All_data_FltrCLN,0.71,96)
All_data_FltrCLN[,97]=exponentiel_smoothing(All_data_FltrCLN,0.74,97)
All_data_FltrCLN[,98]=exponentiel_smoothing(All_data_FltrCLN,0.76,98)
All_data_FltrCLN[,99]=exponentiel_smoothing(All_data_FltrCLN,0.76,99)
All_data_FltrCLN[,100]=exponentiel_smoothing(All_data_FltrCLN,0.76,100)
All_data_FltrCLN[,101]=exponentiel_smoothing(All_data_FltrCLN,0.77,101)
All_data_FltrCLN[,102]=exponentiel_smoothing(All_data_FltrCLN,0.77,102)
All_data_FltrCLN[,103]=exponentiel_smoothing(All_data_FltrCLN,0.79,103)
All_data_FltrCLN[,104]=exponentiel_smoothing(All_data_FltrCLN,0.79,104)
All_data_FltrCLN[,105]=exponentiel_smoothing(All_data_FltrCLN,0.79,105)
All_data_FltrCLN[,106]=exponentiel_smoothing(All_data_FltrCLN,0.78,106)
All_data_FltrCLN[,107]=exponentiel_smoothing(All_data_FltrCLN,0.84,107)
All_data_FltrCLN[,108]=exponentiel_smoothing(All_data_FltrCLN,0.9,108)
All_data_FltrCLN[,109]=exponentiel_smoothing(All_data_FltrCLN,0.95,109)
All_data_FltrCLN[,110]=exponentiel_smoothing(All_data_FltrCLN,0.97,110)
All_data_FltrCLN[,111]=exponentiel_smoothing(All_data_FltrCLN,0.97,111)
All_data_FltrCLN[,112]=exponentiel_smoothing(All_data_FltrCLN,0.97,112)
All_data_FltrCLN[,113]=exponentiel_smoothing(All_data_FltrCLN,0.97,113)
All_data_FltrCLN[,114]=exponentiel_smoothing(All_data_FltrCLN,0.96,114)
All_data_FltrCLN[,115]=exponentiel_smoothing(All_data_FltrCLN,0.97,115)
All_data_FltrCLN[,116]=exponentiel_smoothing(All_data_FltrCLN,0.97,116)
All_data_FltrCLN[,117]=exponentiel_smoothing(All_data_FltrCLN,0.97,117)
All_data_FltrCLN[,118]=exponentiel_smoothing(All_data_FltrCLN,0.97,118)
All_data_FltrCLN[,119]=exponentiel_smoothing(All_data_FltrCLN,0.97,119)
All_data_FltrCLN[,120]=exponentiel_smoothing(All_data_FltrCLN,0.97,120)
All_data_FltrCLN[,121]=exponentiel_smoothing(All_data_FltrCLN,0.97,121)
All_data_FltrCLN[,122]=exponentiel_smoothing(All_data_FltrCLN,0.97,122)
All_data_FltrCLN[,123]=exponentiel_smoothing(All_data_FltrCLN,0.97,123)
All_data_FltrCLN[,124]=exponentiel_smoothing(All_data_FltrCLN,0.97,124)
All_data_FltrCLN[,125]=exponentiel_smoothing(All_data_FltrCLN,0.01,125)
All_data_FltrCLN[,126]=exponentiel_smoothing(All_data_FltrCLN,0.55,126)
All_data_FltrCLN[,127]=exponentiel_smoothing(All_data_FltrCLN,0.01,127)
All_data_FltrCLN[,128]=exponentiel_smoothing(All_data_FltrCLN,0.01,128)
All_data_FltrCLN[,129]=exponentiel_smoothing(All_data_FltrCLN,0.42,129)
All_data_FltrCLN[,130]=exponentiel_smoothing(All_data_FltrCLN,0.47,130)
All_data_FltrCLN[,131]=exponentiel_smoothing(All_data_FltrCLN,0.48,131)
All_data_FltrCLN[,132]=exponentiel_smoothing(All_data_FltrCLN,0.01,132)
All_data_FltrCLN[,133]=exponentiel_smoothing(All_data_FltrCLN,0.96,133)
All_data_FltrCLN[,134]=exponentiel_smoothing(All_data_FltrCLN,0.85,134)
All_data_FltrCLN[,135]=exponentiel_smoothing(All_data_FltrCLN,0.88,135)
All_data_FltrCLN[,136]=exponentiel_smoothing(All_data_FltrCLN,0.01,136)
All_data_FltrCLN[,137]=exponentiel_smoothing(All_data_FltrCLN,0.47,137)



All_data_FltrCLN_smoothed_back = All_data_FltrCLN
# Apply range constraint to clean variables
All_data_FltrCLN = All_data_FltrCLN_smoothed_back


i=which(5.5<All_data_FltrCLN$`TOTAL REFINRY CRUDE RATE_MBPH_10FC161.pv` & All_data_FltrCLN$`TOTAL REFINRY CRUDE RATE_MBPH_10FC161.pv`<11)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(28<All_data_FltrCLN$`CRU API GRAV CALC_'API(60F)_10DY162A.PV` & All_data_FltrCLN$`CRU API GRAV CALC_'API(60F)_10DY162A.PV`<40)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(12.5<All_data_FltrCLN$`#1DSALT-#3GRID VOLTX1000_KVOLTS_10EI795.PV` & All_data_FltrCLN$`#1DSALT-#3GRID VOLTX1000_KVOLTS_10EI795.PV`<14)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(10<All_data_FltrCLN$`#1DSALT-#2GRID VOLTX1000_KVOLTS_10EI794.PV` & All_data_FltrCLN$`#1DSALT-#2GRID VOLTX1000_KVOLTS_10EI794.PV`<13)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(8<All_data_FltrCLN$`#1DSALT-#1GRID VOLTX1000_KVOLTS_10EI793.PV` & All_data_FltrCLN$`#1DSALT-#1GRID VOLTX1000_KVOLTS_10EI793.PV`<11.5)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(3<All_data_FltrCLN$`#1 DESALT - #3 GRID AMPS_AMPS_10II798.PV` & All_data_FltrCLN$`#1 DESALT - #3 GRID AMPS_AMPS_10II798.PV`<20)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(3<All_data_FltrCLN$`#1 DESALT - #2 GRID AMPS_AMPS_10II797.PV` & All_data_FltrCLN$`#1 DESALT - #2 GRID AMPS_AMPS_10II797.PV`<20)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(4<All_data_FltrCLN$`#1 DESALT - #1 GRID AMPS_AMPS_10II796.PV` & All_data_FltrCLN$`#1 DESALT - #1 GRID AMPS_AMPS_10II796.PV`<24)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(13.2<All_data_FltrCLN$`#2DSALT-#3GRID VOLTX1000_KVOLTS_10EI801.PV` & All_data_FltrCLN$`#2DSALT-#3GRID VOLTX1000_KVOLTS_10EI801.PV`<14.7)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(11<All_data_FltrCLN$`#2DSALT-#2GRID VOLTX1000_KVOLTS_10EI800.PV` & All_data_FltrCLN$`#2DSALT-#2GRID VOLTX1000_KVOLTS_10EI800.PV`<13)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(8.5<All_data_FltrCLN$`#2DSALT-#1GRID VOLTX1000_KVOLTS_10EI799.PV` & All_data_FltrCLN$`#2DSALT-#1GRID VOLTX1000_KVOLTS_10EI799.PV`<12.7)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(3<All_data_FltrCLN$`#2 DESALT - #3 GRID AMPS_AMPS_10II804.PV` & All_data_FltrCLN$`#2 DESALT - #3 GRID AMPS_AMPS_10II804.PV`<33)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(4<All_data_FltrCLN$`#2 DESALT - #2 GRID AMPS_AMPS_10II803.PV` & All_data_FltrCLN$`#2 DESALT - #2 GRID AMPS_AMPS_10II803.PV`<20)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(6<All_data_FltrCLN$`#2 DESALT - #1 GRID AMPS_AMPS_10II802.PV` & All_data_FltrCLN$`#2 DESALT - #1 GRID AMPS_AMPS_10II802.PV`<36)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(22<All_data_FltrCLN$`#2 DESALTR INTERFACE LVL_%_10LI59.PV` & All_data_FltrCLN$`#2 DESALTR INTERFACE LVL_%_10LI59.PV`<35)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(43<All_data_FltrCLN$`#1DESALTR--TOP OF H2O_%_10LI701B.PV` & All_data_FltrCLN$`#1DESALTR--TOP OF H2O_%_10LI701B.PV`<73)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(43<All_data_FltrCLN$`#1DESALTR--TOP OF RAG_%_10LI701C.PV` & All_data_FltrCLN$`#1DESALTR--TOP OF RAG_%_10LI701C.PV`<73)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(215<All_data_FltrCLN$`CRUDE FROM #1 DESALTER_DEG F_10TI1765.PV` & All_data_FltrCLN$`CRUDE FROM #1 DESALTER_DEG F_10TI1765.PV`<290)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`DEMULSIFIER TANK LEVEL_FEET_10LI1502.PV` & All_data_FltrCLN$`DEMULSIFIER TANK LEVEL_FEET_10LI1502.PV`<13)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.02<All_data_FltrCLN$`DEMULSFR TO CRU CHG PUMPS_GPM_10FY437.PV` & All_data_FltrCLN$`DEMULSFR TO CRU CHG PUMPS_GPM_10FY437.PV`<0.26)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(-0.9<All_data_FltrCLN$`CRUDE TWR TPA DELTA P_PSID_10py943.PV` & All_data_FltrCLN$`CRUDE TWR TPA DELTA P_PSID_10py943.PV`<1.2)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(220<All_data_FltrCLN$`CRUDE TO DESALTER_DEG F_10TI380.PV` & All_data_FltrCLN$`CRUDE TO DESALTER_DEG F_10TI380.PV`<310)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(220<All_data_FltrCLN$`CRUDE FROM #2 DESALTER_DEG F_10TI35.PV` & All_data_FltrCLN$`CRUDE FROM #2 DESALTER_DEG F_10TI35.PV`<295)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(210<All_data_FltrCLN$`#1 DSLTR OUTLET PRESS_PSIG_10PI609.PV` & All_data_FltrCLN$`#1 DSLTR OUTLET PRESS_PSIG_10PI609.PV`<260)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(9<All_data_FltrCLN$`#1 DESALTR E.MIX VALV DP_PSID_10PI1641.PV` & All_data_FltrCLN$`#1 DESALTR E.MIX VALV DP_PSID_10PI1641.PV`<18)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(9<All_data_FltrCLN$`#1 DESALTR W.MIX VALV DP_PSID_10PI607.PV` & All_data_FltrCLN$`#1 DESALTR W.MIX VALV DP_PSID_10PI607.PV`<18)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(43<All_data_FltrCLN$`#1 DESALTER WATER LEVEL_% LVL_10LC701.PV` & All_data_FltrCLN$`#1 DESALTER WATER LEVEL_% LVL_10LC701.PV`<73)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(180<All_data_FltrCLN$`#2 DESALTER PRESSURE_PSIG_10PI62.PV` & All_data_FltrCLN$`#2 DESALTER PRESSURE_PSIG_10PI62.PV`<240)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(48<All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.PV` & All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.PV`<68)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(12<All_data_FltrCLN$`#1 DESALTER E. MIX VALVE_% OPEN_10HC1641.PV` & All_data_FltrCLN$`#1 DESALTER E. MIX VALVE_% OPEN_10HC1641.PV`<21)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(14<All_data_FltrCLN$`#1 DESALTER W. MIX VALVE_% OPEN_10HC607.PV` & All_data_FltrCLN$`#1 DESALTER W. MIX VALVE_% OPEN_10HC607.PV`<25)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(48<All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LI750B.PV` & All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LI750B.PV`<68)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(48<All_data_FltrCLN$`#2 DESALTER--TOP OF RAG_%LEVEL_10LI750C.PV` & All_data_FltrCLN$`#2 DESALTER--TOP OF RAG_%LEVEL_10LI750C.PV`<68)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(400<All_data_FltrCLN$`DESALT EFF TO STRIPPER_BPH_10fi3150.pv` & All_data_FltrCLN$`DESALT EFF TO STRIPPER_BPH_10fi3150.pv`<950)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(200<All_data_FltrCLN$`E.WASH WTR INLT->#1DSLTR_BPH WW_10fi1639.pv` & All_data_FltrCLN$`E.WASH WTR INLT->#1DSLTR_BPH WW_10fi1639.pv`<380)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(220<All_data_FltrCLN$`W.WASH WTR INLT->#1DSLTR_BPH WW_10fi1640.pv` & All_data_FltrCLN$`W.WASH WTR INLT->#1DSLTR_BPH WW_10fi1640.pv`<400)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(185<All_data_FltrCLN$`SOUR H2O TO DESALTER_DEG F_10TI3178.PV` & All_data_FltrCLN$`SOUR H2O TO DESALTER_DEG F_10TI3178.PV`<230)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFLR TUBE43-BTM-DENS_G/ML_10N13043.pv` & All_data_FltrCLN$`#1PROFLR TUBE43-BTM-DENS_G/ML_10N13043.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE42 DENSITY_G/ML_10N13042.pv` & All_data_FltrCLN$`#1PROFILR TUBE42 DENSITY_G/ML_10N13042.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE41 DENSITY_G/ML_10N13041.pv` & All_data_FltrCLN$`#1PROFILR TUBE41 DENSITY_G/ML_10N13041.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE40 DENSITY_G/ML_10N13040.pv` & All_data_FltrCLN$`#1PROFILR TUBE40 DENSITY_G/ML_10N13040.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE39 DENSITY_G/ML_10N13039.pv` & All_data_FltrCLN$`#1PROFILR TUBE39 DENSITY_G/ML_10N13039.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE38 DENSITY_G/ML_10N13038.pv` & All_data_FltrCLN$`#1PROFILR TUBE38 DENSITY_G/ML_10N13038.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE37 DENSITY_G/ML_10N13037.pv` & All_data_FltrCLN$`#1PROFILR TUBE37 DENSITY_G/ML_10N13037.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE36 DENSITY_G/ML_10N13036.pv` & All_data_FltrCLN$`#1PROFILR TUBE36 DENSITY_G/ML_10N13036.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE35 DENSITY_G/ML_10N13035.pv` & All_data_FltrCLN$`#1PROFILR TUBE35 DENSITY_G/ML_10N13035.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE34 DENSITY_G/ML_10N13034.pv` & All_data_FltrCLN$`#1PROFILR TUBE34 DENSITY_G/ML_10N13034.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE33 DENSITY_G/ML_10N13033.pv` & All_data_FltrCLN$`#1PROFILR TUBE33 DENSITY_G/ML_10N13033.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE32 DENSITY_G/ML_10N13032.pv` & All_data_FltrCLN$`#1PROFILR TUBE32 DENSITY_G/ML_10N13032.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE31 DENSITY_G/ML_10N13031.pv` & All_data_FltrCLN$`#1PROFILR TUBE31 DENSITY_G/ML_10N13031.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE30 DENSITY_G/ML_10N13030.pv` & All_data_FltrCLN$`#1PROFILR TUBE30 DENSITY_G/ML_10N13030.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE29 DENSITY_G/ML_10N13029.pv` & All_data_FltrCLN$`#1PROFILR TUBE29 DENSITY_G/ML_10N13029.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE28 DENSITY_G/ML_10N13028.pv` & All_data_FltrCLN$`#1PROFILR TUBE28 DENSITY_G/ML_10N13028.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE27 DENSITY_G/ML_10N13027.pv` & All_data_FltrCLN$`#1PROFILR TUBE27 DENSITY_G/ML_10N13027.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE26 DENSITY_G/ML_10N13026.pv` & All_data_FltrCLN$`#1PROFILR TUBE26 DENSITY_G/ML_10N13026.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE25 DENSITY_G/ML_10N13025.pv` & All_data_FltrCLN$`#1PROFILR TUBE25 DENSITY_G/ML_10N13025.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE24 DENSITY_G/ML_10N13024.pv` & All_data_FltrCLN$`#1PROFILR TUBE24 DENSITY_G/ML_10N13024.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE23 DENSITY_G/ML_10N13023.pv` & All_data_FltrCLN$`#1PROFILR TUBE23 DENSITY_G/ML_10N13023.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE22 DENSITY_G/ML_10N13022.pv` & All_data_FltrCLN$`#1PROFILR TUBE22 DENSITY_G/ML_10N13022.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE21 DENSITY_G/ML_10N13021.pv` & All_data_FltrCLN$`#1PROFILR TUBE21 DENSITY_G/ML_10N13021.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE20 DENSITY_G/ML_10N13020.pv` & All_data_FltrCLN$`#1PROFILR TUBE20 DENSITY_G/ML_10N13020.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE19 DENSITY_G/ML_10N13019.pv` & All_data_FltrCLN$`#1PROFILR TUBE19 DENSITY_G/ML_10N13019.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE18 DENSITY_G/ML_10N13018.pv` & All_data_FltrCLN$`#1PROFILR TUBE18 DENSITY_G/ML_10N13018.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE17 DENSITY_G/ML_10N13017.pv` & All_data_FltrCLN$`#1PROFILR TUBE17 DENSITY_G/ML_10N13017.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE16 DENSITY_G/ML_10N13016.pv` & All_data_FltrCLN$`#1PROFILR TUBE16 DENSITY_G/ML_10N13016.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE15 DENSITY_G/ML_10N13015.pv` & All_data_FltrCLN$`#1PROFILR TUBE15 DENSITY_G/ML_10N13015.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE14 DENSITY_G/ML_10N13014.pv` & All_data_FltrCLN$`#1PROFILR TUBE14 DENSITY_G/ML_10N13014.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE13 DENSITY_G/ML_10N13013.pv` & All_data_FltrCLN$`#1PROFILR TUBE13 DENSITY_G/ML_10N13013.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE12 DENSITY_G/ML_10N13012.pv` & All_data_FltrCLN$`#1PROFILR TUBE12 DENSITY_G/ML_10N13012.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE11 DENSITY_G/ML_10N13011.pv` & All_data_FltrCLN$`#1PROFILR TUBE11 DENSITY_G/ML_10N13011.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE10 DENSITY_G/ML_10N13010.pv` & All_data_FltrCLN$`#1PROFILR TUBE10 DENSITY_G/ML_10N13010.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE9 DENSITY_G/ML_10N13009.pv` & All_data_FltrCLN$`#1PROFILR TUBE9 DENSITY_G/ML_10N13009.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE8 DENSITY_G/ML_10N13008.pv` & All_data_FltrCLN$`#1PROFILR TUBE8 DENSITY_G/ML_10N13008.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE7 DENSITY_G/ML_10N13007.pv` & All_data_FltrCLN$`#1PROFILR TUBE7 DENSITY_G/ML_10N13007.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE6 DENSITY_G/ML_10N13006.pv` & All_data_FltrCLN$`#1PROFILR TUBE6 DENSITY_G/ML_10N13006.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE5 DENSITY_G/ML_10N13005.pv` & All_data_FltrCLN$`#1PROFILR TUBE5 DENSITY_G/ML_10N13005.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE4 DENSITY_G/ML_10N13004.pv` & All_data_FltrCLN$`#1PROFILR TUBE4 DENSITY_G/ML_10N13004.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE3 DENSITY_G/ML_10N13003.pv` & All_data_FltrCLN$`#1PROFILR TUBE3 DENSITY_G/ML_10N13003.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE2 DENSITY_G/ML_10N13002.pv` & All_data_FltrCLN$`#1PROFILR TUBE2 DENSITY_G/ML_10N13002.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#1PROFILR TUBE1-TOP-DENS_G/ML_10N13001.pv` & All_data_FltrCLN$`#1PROFILR TUBE1-TOP-DENS_G/ML_10N13001.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFLR TUBE43-BTM-DENS_G/ML_10N13243.pv` & All_data_FltrCLN$`#2PROFLR TUBE43-BTM-DENS_G/ML_10N13243.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE42 DENSITY_G/ML_10N13242.pv` & All_data_FltrCLN$`#2PROFILR TUBE42 DENSITY_G/ML_10N13242.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE41 DENSITY_G/ML_10N13241.pv` & All_data_FltrCLN$`#2PROFILR TUBE41 DENSITY_G/ML_10N13241.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE40 DENSITY_G/ML_10N13240.pv` & All_data_FltrCLN$`#2PROFILR TUBE40 DENSITY_G/ML_10N13240.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE39 DENSITY_G/ML_10N13239.pv` & All_data_FltrCLN$`#2PROFILR TUBE39 DENSITY_G/ML_10N13239.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE38 DENSITY_G/ML_10N13238.pv` & All_data_FltrCLN$`#2PROFILR TUBE38 DENSITY_G/ML_10N13238.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE37 DENSITY_G/ML_10N13237.pv` & All_data_FltrCLN$`#2PROFILR TUBE37 DENSITY_G/ML_10N13237.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE36 DENSITY_G/ML_10N13236.pv` & All_data_FltrCLN$`#2PROFILR TUBE36 DENSITY_G/ML_10N13236.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE35 DENSITY_G/ML_10N13235.pv` & All_data_FltrCLN$`#2PROFILR TUBE35 DENSITY_G/ML_10N13235.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE34 DENSITY_G/ML_10N13234.pv` & All_data_FltrCLN$`#2PROFILR TUBE34 DENSITY_G/ML_10N13234.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE33 DENSITY_G/ML_10N13233.pv` & All_data_FltrCLN$`#2PROFILR TUBE33 DENSITY_G/ML_10N13233.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE32 DENSITY_G/ML_10N13232.pv` & All_data_FltrCLN$`#2PROFILR TUBE32 DENSITY_G/ML_10N13232.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE31 DENSITY_G/ML_10N13231.pv` & All_data_FltrCLN$`#2PROFILR TUBE31 DENSITY_G/ML_10N13231.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE30 DENSITY_G/ML_10N13230.pv` & All_data_FltrCLN$`#2PROFILR TUBE30 DENSITY_G/ML_10N13230.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE29 DENSITY_G/ML_10N13229.pv` & All_data_FltrCLN$`#2PROFILR TUBE29 DENSITY_G/ML_10N13229.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE28 DENSITY_G/ML_10N13228.pv` & All_data_FltrCLN$`#2PROFILR TUBE28 DENSITY_G/ML_10N13228.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE27 DENSITY_G/ML_10N13227.pv` & All_data_FltrCLN$`#2PROFILR TUBE27 DENSITY_G/ML_10N13227.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE26 DENSITY_G/ML_10N13226.pv` & All_data_FltrCLN$`#2PROFILR TUBE26 DENSITY_G/ML_10N13226.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE25 DENSITY_G/ML_10N13225.pv` & All_data_FltrCLN$`#2PROFILR TUBE25 DENSITY_G/ML_10N13225.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE24 DENSITY_G/ML_10N13224.pv` & All_data_FltrCLN$`#2PROFILR TUBE24 DENSITY_G/ML_10N13224.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE23 DENSITY_G/ML_10N13223.pv` & All_data_FltrCLN$`#2PROFILR TUBE23 DENSITY_G/ML_10N13223.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE22 DENSITY_G/ML_10N13222.pv` & All_data_FltrCLN$`#2PROFILR TUBE22 DENSITY_G/ML_10N13222.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE21 DENSITY_G/ML_10N13221.pv` & All_data_FltrCLN$`#2PROFILR TUBE21 DENSITY_G/ML_10N13221.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE20 DENSITY_G/ML_10N13220.pv` & All_data_FltrCLN$`#2PROFILR TUBE20 DENSITY_G/ML_10N13220.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE19 DENSITY_G/ML_10N13219.pv` & All_data_FltrCLN$`#2PROFILR TUBE19 DENSITY_G/ML_10N13219.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE18 DENSITY_G/ML_10N13218.pv` & All_data_FltrCLN$`#2PROFILR TUBE18 DENSITY_G/ML_10N13218.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE17 DENSITY_G/ML_10N13217.pv` & All_data_FltrCLN$`#2PROFILR TUBE17 DENSITY_G/ML_10N13217.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE16 DENSITY_G/ML_10N13216.pv` & All_data_FltrCLN$`#2PROFILR TUBE16 DENSITY_G/ML_10N13216.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE15 DENSITY_G/ML_10N13215.pv` & All_data_FltrCLN$`#2PROFILR TUBE15 DENSITY_G/ML_10N13215.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE14 DENSITY_G/ML_10N13214.pv` & All_data_FltrCLN$`#2PROFILR TUBE14 DENSITY_G/ML_10N13214.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE13 DENSITY_G/ML_10N13213.pv` & All_data_FltrCLN$`#2PROFILR TUBE13 DENSITY_G/ML_10N13213.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE12 DENSITY_G/ML_10N13212.pv` & All_data_FltrCLN$`#2PROFILR TUBE12 DENSITY_G/ML_10N13212.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE11 DENSITY_G/ML_10N13211.pv` & All_data_FltrCLN$`#2PROFILR TUBE11 DENSITY_G/ML_10N13211.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE10 DENSITY_G/ML_10N13210.pv` & All_data_FltrCLN$`#2PROFILR TUBE10 DENSITY_G/ML_10N13210.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE9 DENSITY_G/ML_10N13209.pv` & All_data_FltrCLN$`#2PROFILR TUBE9 DENSITY_G/ML_10N13209.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE8 DENSITY_G/ML_10N13208.pv` & All_data_FltrCLN$`#2PROFILR TUBE8 DENSITY_G/ML_10N13208.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE7 DENSITY_G/ML_10N13207.pv` & All_data_FltrCLN$`#2PROFILR TUBE7 DENSITY_G/ML_10N13207.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE6 DENSITY_G/ML_10N13206.pv` & All_data_FltrCLN$`#2PROFILR TUBE6 DENSITY_G/ML_10N13206.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE5 DENSITY_G/ML_10N13205.pv` & All_data_FltrCLN$`#2PROFILR TUBE5 DENSITY_G/ML_10N13205.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE4 DENSITY_G/ML_10N13204.pv` & All_data_FltrCLN$`#2PROFILR TUBE4 DENSITY_G/ML_10N13204.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE3 DENSITY_G/ML_10N13203.pv` & All_data_FltrCLN$`#2PROFILR TUBE3 DENSITY_G/ML_10N13203.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE2 DENSITY_G/ML_10N13202.pv` & All_data_FltrCLN$`#2PROFILR TUBE2 DENSITY_G/ML_10N13202.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0.7<All_data_FltrCLN$`#2PROFILR TUBE1-TOP-DENS_G/ML_10N13201.pv` & All_data_FltrCLN$`#2PROFILR TUBE1-TOP-DENS_G/ML_10N13201.pv`<1.3)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(300<All_data_FltrCLN$`DWS WATER TO DESALTER_BPH_10FC928.PV` & All_data_FltrCLN$`DWS WATER TO DESALTER_BPH_10FC928.PV`<750)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(300<All_data_FltrCLN$`SOUR H2O TO #1 DESALTER_BPH_10FI523.PV` & All_data_FltrCLN$`SOUR H2O TO #1 DESALTER_BPH_10FI523.PV`<750)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(10<All_data_FltrCLN$`#1 DESALTER WATER LEVEL_%_10LC701.OP` & All_data_FltrCLN$`#1 DESALTER WATER LEVEL_%_10LC701.OP`<90)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(40<All_data_FltrCLN$`#1 DESALTER WATER LEVEL_% LVL_10LC701.SP` & All_data_FltrCLN$`#1 DESALTER WATER LEVEL_% LVL_10LC701.SP`<80)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(30<All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.OP` & All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.OP`<100)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(40<All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.SP` & All_data_FltrCLN$`#2 DESALTER--TOP OF H2O_%LEVEL_10LC750.SP`<75)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`WWTR TO CRUDE/TPA 1302_BPH WW_10FC1464.PV ` & All_data_FltrCLN$`WWTR TO CRUDE/TPA 1302_BPH WW_10FC1464.PV `<200)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`DEMUL STORAGE TANK LEVEL_FEET_33li235.pv` & All_data_FltrCLN$`DEMUL STORAGE TANK LEVEL_FEET_33li235.pv`<13)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(18<All_data_FltrCLN$`WTR FM U17 TO #1 DESALTR_BPH WTR_10FC1649.PV` & All_data_FltrCLN$`WTR FM U17 TO #1 DESALTR_BPH WTR_10FC1649.PV`<54)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`Wash Wtr to C1360-03/04_BPH_10FI1466.PV` & All_data_FltrCLN$`Wash Wtr to C1360-03/04_BPH_10FI1466.PV`<100)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`Wash Wtr to C1360-01/02_BPH_10FI1465.PV` & All_data_FltrCLN$`Wash Wtr to C1360-01/02_BPH_10FI1465.PV`<100)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV` & All_data_FltrCLN$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV`<1.8)  
All_data_FltrCLN=All_data_FltrCLN[i,]
i=which(0<All_data_FltrCLN$`%WASH WTR TO #2 DESALTER_% OF TOT CRU_10FK5287.pv` & All_data_FltrCLN$`%WASH WTR TO #2 DESALTER_% OF TOT CRU_10FK5287.pv`<20)  
All_data_FltrCLN=All_data_FltrCLN[i,]

# add duplicate chlorides to apply heavy smoothing
All_data_FltrCLN$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355_heavy.PV` = All_data_FltrCLN$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355.PV`
All_data_FltrCLN[,139]=exponentiel_smoothing(All_data_FltrCLN,0.99,139)
names(All_data_FltrCLN)
i=which(0.026<All_data_FltrCLN$`#2DESLTR CRUDE CHLORIDES_PPM CL_10AI1355_heavy.PV`)  
All_data_FltrCLN=All_data_FltrCLN[i,]

#Export chlorides to identify which coefficient for the exp filter
ChloridesTag = cbind(All_data_Fltr[,1],All_data_Fltr[,136])
write.xlsx(ChloridesTag, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/ChloridesTag.xlsx") 

names(All_data_FltrCLN)

names(All_data_FltrCLN)[1]=";Description_UOM_Tag"
#Export cleaned and smoothed data
All_data_FltrCLN_sample = All_data_FltrCLN[1:10,]
write.csv(All_data_FltrCLN, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/All_data_FltrCLN_csv.csv") 
write.table(All_data_FltrCLN, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/All_data_FltrCLN_TXT.txt", sep=";") 


# Split each cluster data in a separate excel file  
temp=All_data_FltrCLN[,138]
All_data_FltrCLN[,139] = NULL
names(All_data_FltrCLN)

i1=which(All_data_FltrCLN$`Clusters`=="1")  
i2=which(All_data_FltrCLN$`Clusters`=="2") 
i3=which(All_data_FltrCLN$`Clusters`=="3") 
i4=which(All_data_FltrCLN$`Clusters`=="4") 


Cluster1_data=All_data_FltrCLN[i1,]
Cluster2_data=All_data_FltrCLN[i2,]
Cluster3_data=All_data_FltrCLN[i3,]
Cluster4_data=All_data_FltrCLN[i4,]

write.xlsx(Cluster1_data, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/Cluster1_data.xlsx") 
write.xlsx(Cluster2_data, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/Cluster2_data.xlsx") 
write.xlsx(Cluster3_data, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/Cluster3_data.xlsx") 
write.xlsx(Cluster4_data, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/Cluster4_data.xlsx") 

#Chourouk export clean and filtered data 



write.xlsx(All_data_FltrCLN, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/All_data_FltrCLN_CJ.xlsx")

write.xlsx(mergeControllersCLN, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/mergeControllersCLN.xlsx")


names(mergeControllersCLN)
names(mergeControllers)


names(MergeContr_Add_process)

API = cbind(MergeContr_Add_process[,1],MergeContr_Add_process[,10])
write.xlsx(API, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/API.xlsx")

Tag = cbind(MergeContr_Add_process[,3])
write.xlsx(Tag, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/Tag.xlsx")



names(MergeContr_Add_process)
AdditionalDataForChourouk=c()
AdditionalDataForChourouk$Description_UOM_Tag=MergeContr_Add_process[,1]


names(MergeContr_Add_process)[3]
names(MergeContr_Add_process)[72]
names(MergeContr_Add_process)[73]
names(MergeContr_Add_process)[82]
names(MergeContr_Add_process)[120]

AdditionalDataForChourouk$`EXCH BACK PRESSURE_PSIG_10PC63.PV`=MergeContr_Add_process[,3]
AdditionalDataForChourouk$`VDF TOWER TOP VAP TEMP_DEG F_10TC3110.pv`=MergeContr_Add_process[,72]
AdditionalDataForChourouk$`LIGHT GAS OIL FM COOLER_DEG F_10Tc720.PV`=MergeContr_Add_process[,73]
AdditionalDataForChourouk$`SOUR H2O TO DESALTER_DEG F_10TI3178.PV`=MergeContr_Add_process[,82]
AdditionalDataForChourouk$`SOUR H2O TO #1 DESALTER_BPH_10FI523.PV`=MergeContr_Add_process[,120]





#define exp smoothing funcion
exponentiel_smoothing=function(data,alpha,i)
{x=data[,i]
S_now=c()
for(j in 1:length(x) )
{if (j==1)
  S_now[j]=S_pre=x[1]
else
  S_now[j]=alpha*S_pre+(1-alpha)*x[j]
S_pre=S_now[j]
}

return(S_now)}





class(AdditionalDataForChourouk)
AdditionalDataForChourouk= as.data.frame(AdditionalDataForChourouk) 

AdditionalDataForChourouk[,2]=exponentiel_smoothing(AdditionalDataForChourouk,0.02,2)
AdditionalDataForChourouk[,3]=exponentiel_smoothing(AdditionalDataForChourouk,0.05,3)
AdditionalDataForChourouk[,4]=exponentiel_smoothing(AdditionalDataForChourouk,0.01,4)
AdditionalDataForChourouk[,5]=exponentiel_smoothing(AdditionalDataForChourouk,0.14,5)
AdditionalDataForChourouk[,6]=exponentiel_smoothing(AdditionalDataForChourouk,0.71,6)


AdditionalDataForChourouk_back = AdditionalDataForChourouk

i=which(160<AdditionalDataForChourouk$EXCH.BACK.PRESSURE_PSIG_10PC63.PV & AdditionalDataForChourouk$EXCH.BACK.PRESSURE_PSIG_10PC63.PV<240)  
AdditionalDataForChourouk=AdditionalDataForChourouk[i,]

names(AdditionalDataForChourouk)


i=which(240<AdditionalDataForChourouk$VDF.TOWER.TOP.VAP.TEMP_DEG.F_10TC3110.pv & AdditionalDataForChourouk$VDF.TOWER.TOP.VAP.TEMP_DEG.F_10TC3110.pv<440)  
AdditionalDataForChourouk=AdditionalDataForChourouk[i,]

i=which(40<AdditionalDataForChourouk$LIGHT.GAS.OIL.FM.COOLER_DEG.F_10Tc720.PV & AdditionalDataForChourouk$LIGHT.GAS.OIL.FM.COOLER_DEG.F_10Tc720.PV<320)  
AdditionalDataForChourouk=AdditionalDataForChourouk[i,]

i=which(180<AdditionalDataForChourouk$SOUR.H2O.TO.DESALTER_DEG.F_10TI3178.PV & AdditionalDataForChourouk$SOUR.H2O.TO.DESALTER_DEG.F_10TI3178.PV<235)  
AdditionalDataForChourouk=AdditionalDataForChourouk[i,]

i=which(300<AdditionalDataForChourouk$SOUR.H2O.TO..1.DESALTER_BPH_10FI523.PV & AdditionalDataForChourouk$SOUR.H2O.TO..1.DESALTER_BPH_10FI523.PV<750)  
AdditionalDataForChourouk=AdditionalDataForChourouk[i,]



write.xlsx(AdditionalDataForChourouk, "C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/Splitted data per cluster/AdditionalDataForChourouk.xlsx")

####read the lab data###
i=1:length(i1)
All_data_FltrCLN$predicted_chloride[i1]=joined1$predicted[i]
i=1:length(i2)
All_data_FltrCLN$predicted_chloride[i2]=joined2$predicted[i]
i=1:length(i3)
All_data_FltrCLN$predicted_chloride[i3]=joined3$predicted[i]
i=1:length(i4)
All_data_FltrCLN$predicted_chloride[i4]=joined4$predicted[i]

lab_data=read.xlsx("C:/Users/Administrator/Desktop/BP IKA Do Not Delete/30min data with clusters/1minchlorideswcuster/BP-KNET CHP Desalter trial File1.xlsx",sheet=3)
lab_data$Date=as.Date(lab_data$Date,origin="1899-12-30")
Date_joined=as.POSIXct(MergeContr_Add$Description,format ="%d-%b-%Y")
Date_joined1 <- as.POSIXlt(Date_joined)
Date_joined1$year=Date_joined1$year+2000
new_data=All_data_FltrCLN
new_data$origina_date=new_data$Description_UOM_Tag
new_data$Description_UOM_Tag=Date_joined1
names(new_data)[1]="Date"
MergeContr_Add$Date=as.character( MergeContr_Add$Date)
lab_data$Date=as.character(lab_data$Date)
joined_all=join( MergeContr_Add,lab_data,type="inner")
