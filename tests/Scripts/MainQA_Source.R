# Ensure data exists in PostGRE
# CSV_Path <- 'C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Testing_Data'
# CSV_Path <- 'C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/QA_DataSets'
# Files <- list.files(CSV_Path)
#
# # Create table loop
# for(xxx in Files) {
#
#   # Print
#   print(xxx)
#
#   # Load csv
#   data <- data.table::fread(file = file.path(CSV_Path, xxx))
#
#   # TableNames
#   TN <- sub(pattern = '', replacement = '', x = xxx)
#   RemixAutoML::PostGRE_RemoveCreateAppend(
#     data = data,
#     TableName = TN,
#     CloseConnection = TRUE,
#     CreateSchema = NULL,
#     Host = "localhost",
#     DBName = "RemixAutoML",
#     User = "postgres",
#     Port = 5432,
#     Password = "Aa1028#@",
#     Temporary = FALSE,
#     Connection = NULL,
#     Append = FALSE)
# }

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# QA Main Script

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Use job
Job <- FALSE
OverallStart <- Sys.time()
ScriptsPath <- 'C:/Users/Bizon/Documents/GitHub/RemixAutoML/tests/Scripts'

# Run time table
RunTimes <- data.table::data.table(
  Job = c('CatBoost_Classifier','CatBoost_MultiClass','CatBoost_Regression', 'CatBoost_Hurdle',
          'LightGBM_Classifier','LightGBM_MultiClass','LightGBM_Regression', 'LightGBM_Hurdle',
          'XGBoost_Classifier','XGBoost_MultiClass','XGBoost_Regression', 'XGBoost_Hurdle',
          'H2O_Classifier','H2O_MultiClass','H2O_Regression',
          'CatBoostCARMA','XGBoostCARMA','LightGBMCARMA',
          'CatBoost_HurdleCARMA', 'XGBoost_HurdleCARMA', 'LightGBM_HurdleCARMA',
          'CatBoostVectorCARMA',
          "CatBoostFunnelCARMA","LightGBMFunnelCARMA","XGBoostFunnelCARMA"),
  RunTimeMins = rep(NA_real_, 25))

# Helpers
KeepList <- c('RunTimes','Counter','Job','Start','OverallStart','ScriptsPath','KeepList')
Packages <- c('RemixAutoML','data.table')

# Incrementer
Counter <- 1L

# CatBoost Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostClassifier_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostClassifier_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostMultiClass_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostMultiClass_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostRegression_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostRegression_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleModel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleModel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# LightGBM Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMClassifier_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMClassifier_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMMultiClass_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMMultiClass_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMRegression_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMRegression_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleModel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleModel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# XGBoost Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostClassifier_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostClassifier_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostMultiClass_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostMultiClass_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostRegression_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostRegression_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleModel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleModel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# H2O Classifiers ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Classifier_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Classifier_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# H2O MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_MultiClass_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_MultiClass_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# H2O Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Regression_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Regression_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# Hurdle CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Hurdle CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Hurdle CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# Vector CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostVectorCARMA_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostVectorCARMA_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L


# . ----

# . ----

# Funnel CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostFunnel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostFunnel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Funnel CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMFunnel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMFunnel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Funnel CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostFunnel_QA.R')))
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostFunnel_QA.R')))
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# CatBoost Evaluation ----
CatBoost_QA_Results_Classifier <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostClassifier_QA"')[['data']]
CatBoost_QA_Results_MultiClass <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostMultiClass_QA"')[['data']]
CatBoost_QA_Results_Regression <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostRegression_QA"')[['data']]
CatBoost_QA_Results_HurdleModel <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostHurdleModel_QA"')[['data']]
CatBoost_QA <- data.table::rbindlist(list(
  CatBoost_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'CatBoost'],
  CatBoost_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'CatBoost'],
  CatBoost_QA_Results_Regression[, Type := 'Regression'][, Model := 'CatBoost'],
  CatBoost_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'CatBoost']),
  use.names = TRUE, fill = TRUE)
View(CatBoost_QA)

# . ----

# LightGBM Evaluation ----
LightGBM_QA_Results_Classifier <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMClassifier_QA"')[['data']]
LightGBM_QA_Results_MultiClass <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMMultiClass_QA"')[['data']]
LightGBM_QA_Results_Regression <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMRegression_QA"')[['data']]
LightGBM_QA_Results_HurdleModel <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMHurdleModel_QA"')[['data']]
LightGBM_QA <- data.table::rbindlist(list(
  LightGBM_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'LightGBM'],
  LightGBM_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'LightGBM'],
  LightGBM_QA_Results_Regression[, Type := 'Regression'][, Model := 'LightGBM'],
  LightGBM_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(LightGBM_QA)

# . ----

# XGBoost Evaluation ----
XGBoost_QA_Results_Classifier  <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostClassifier_QA"')[['data']]
XGBoost_QA_Results_MultiClass  <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostMultiClass_QA"')[['data']]
XGBoost_QA_Results_Regression  <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostRegression_QA"')[['data']]
XGBoost_QA_Results_HurdleModel <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostHurdleModel_QA"')[['data']]
XGBoost_QA <- data.table::rbindlist(list(
  XGBoost_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'XGBoost'],
  XGBoost_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'XGBoost'],
  XGBoost_QA_Results_Regression[, Type := 'Regression'][, Model := 'XGBoost'],
  XGBoost_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'XGBoost']),
  use.names = TRUE, fill = TRUE)
View(XGBoost_QA)

# . ----

# H2O Evaluation ----
H2O_QA_Results_Classifier <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostClassifier_QA"')[['data']]
H2O_QA_Results_MultiClass <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostMultiClass_QA"')[['data']]
H2O_QA_Results_Regression <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostRegression_QA"')[['data']]
H2O_QA <- data.table::rbindlist(list(
  H2O_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'h2o'],
  H2O_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'h2o'],
  H2O_QA_Results_Regression[, Type := 'Regression'][, Model := 'h2o']),
  use.names = TRUE, fill = TRUE)
View(H2O_QA)

# . ----

# CARMA Evaluation ----
CatBoostCARMA <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostCARMA_QA"')[['data']]
XGBoostCARMA <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostCARMA_QA"')[['data']]
LightGBMCARMA <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMCARMA_QA"')[['data']]
CARMA_QA <- data.table::rbindlist(list(
  CatBoostCARMA[, Type := 'CARMA'][, Model := 'CatBoost'],
  XGBoostCARMA[, Type := 'CARMA'][, Model := 'XGBoost'],
  LightGBMCARMA[, Type := 'CARMA'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(CARMA_QA)

# . ----

# Hurdle CARMA Evaluation ----
CatBoostHurdleCARMA <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostHurdleCARMA_QA"')[['data']]
XGBoostHurdleCARMA <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostHurdleCARMA_QA"')[['data']]
LightGBMHurdleCARMA <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMHurdleCARMA_QA"')[['data']]
HurdleCARMA_QA <- data.table::rbindlist(list(
  CatBoostHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'CatBoost'],
  XGBoostHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'XGBoost'],
  LightGBMHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(HurdleCARMA_QA)

# . ----

# Vector CARMA Evaluation ----
CatBoostVectorCARMA <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostVectorCARMA_QA"')[['data']]
VectorCARMA <- CatBoostVectorCARMA[, Model := 'CatBoost']
View(CatBoostVectorCARMA)

# . ----

# Funnel CARMA Evaluation ----
CatBoostFunnel <- RemixAutoML:::Post_Query_Helper('"AutoCatBoostFunnel_QA"')[['data']]
LightGBMFunnel <- RemixAutoML:::Post_Query_Helper('"AutoLightGBMFunnel_QA"')[['data']]
XGBoostFunnel <- RemixAutoML:::Post_Query_Helper('"AutoXGBoostFunnel_QA"')[['data']]
Funnel_QA <- data.table::rbindlist(list(
  CatBoostFunnel[, Type := 'Funnel_CARMA'][, Model := 'CatBoost'],
  LightGBMFunnel[, Type := 'Funnel_CARMA'][, Model := 'LightGBM'],
  XGBoostFunnel[, Type := 'Funnel_CARMA'][, Model := 'XGBoost']),
  use.names = TRUE, fill = TRUE)
View(Funnel_QA)

# . ----

# . ----

# Meta ----
if(exists("CatBoost_QA")) Cat <- CatBoost_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success","ScoreSuccess")]
if(exists("LightGBM_QA")) Light <- LightGBM_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success","Score")]
if(exists("XGBoost_QA")) xgb <- XGBoost_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success","ScoreSuccess")]
if(exists("H2O_QA")) h2o <- H2O_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("CARMA_QA")) Carma <- CARMA_QA[, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("HurdleCARMA_QA")) HurdleCARMA <- HurdleCARMA_QA[, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("CatBoostVectorCARMA")) CatBoostVectorCARMA <- CatBoostVectorCARMA[, Model := "CatBoost"][, Type := "Vector_CARMA"][, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("Funnel_QA")) FunnelCarma <- Funnel_QA[, Type := "Funnel_CARMA"][, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Training","Forecast")]
All_Methods <- list()
for(zz in c("Cat","Light","xgb","h2o","Carma","HurdleCARMA","FunnelCarma","CatBoostVectorCARMA")) { #
  print(zz)
  if(exists(zz)) All_Methods[[zz]] <- eval(expr = parse(text = zz))
}
All_Methods <- data.table::rbindlist(All_Methods, fill = TRUE)
OverallEnd <- Sys.time()
print(difftime(time1 = OverallEnd, time2 = OverallStart, units = 'mins'))
View(All_Methods)

# . ----

# . ----
