# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# QA Main Script

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Use job
Job <- FALSE
OverallStart <- Sys.time()
ScriptsPath <- 'C:/Users/Bizon/Documents/GitHub/QA_Code'
CSVPath <- 'C:/Users/Bizon/Documents/GitHub/QA_Code/QA_CSV'

# Run time table
RunTimes <- data.table::data.table(
  Job = c('CatBoost_Classifier','CatBoost_MultiClass','CatBoost_Regression',
          'LightGBM_Classifier','LightGBM_MultiClass','LightGBM_Regression',
          'XGBoost_Classifier','XGBoost_MultiClass','XGBoost_Regression',
          'H2O_Classifier','H2O_MultiClass','H2O_Regression',
          'CatBoostCARMA','XGBoostCARMA','LightGBMCARMA',
          'CatBoostHurdleCARMA',
          'CatBoostVectorCARMA',
          "CatBoostFunnelCARMA","LightGBMFunnelCARMA","XGBoostFunnelCARMA"),
  RunTimeMins = rep(NA_real_, 20))

# Helpers
KeepList <- c('RunTimes','Counter','Job','Start','OverallStart','ScriptsPath','CSVPath')
Packages <- c('RemixAutoML','data.table')

# Incrementer
Counter <- 1L

# CatBoost Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostClassifier_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostClassifier_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostMultiClass_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostMultiClass_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostRegression_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostRegression_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CatBoost Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleModel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleModel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# LightGBM Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMClassifier_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMClassifier_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMMultiClass_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMMultiClass_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMRegression_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMRegression_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# LightGBM Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleModel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleModel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# XGBoost Classifier ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostClassifier_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostClassifier_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostMultiClass_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostMultiClass_QA.R')))
  rm(list=ls()[!ls() %in% c('RunTimes','Counter','Counter','Job','Start','OverallStart','ScriptsPath','CSVPath')])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostRegression_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostRegression_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# XGBoost Hurdle Model ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleModel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleModel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# H2O Classifiers ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Classifier_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Classifier_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# H2O MultiClass ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_MultiClass_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_MultiClass_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# H2O Regression ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Regression_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoH2O_Regression_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# Hurdle CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostHurdleCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Hurdle CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostHurdleCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Hurdle CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMHurdleCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# Vector CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostVectorCARMA_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostVectorCARMA_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L


# . ----

# . ----

# Funnel CARMA CatBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostFunnel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoCatBoostFunnel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Funnel CARMA LightGBM ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMFunnel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoLightGBMFunnel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# Funnel CARMA XGBoost ----
Start <- Sys.time()
if(Job) {
  job::job(packages = Packaages, {
    system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostFunnel_QA.R')));
    rm(list=ls()[!ls() %in% KeepList])
  })
} else {
  system(paste0('Rscript --vanilla ', file.path(ScriptsPath, 'AutoXGBoostFunnel_QA.R')))
  rm(list=ls()[!ls() %in% KeepList])
}
data.table::set(RunTimes, i = Counter, j = 'RunTimes', value = difftime(Sys.time(), Start, units = 'mins'))
Counter <- Counter + 1L

# . ----

# . ----

# CatBoost Evaluation ----
CatBoost_QA_Results_Classifier <- data.table::fread(file.path(CSVPath, 'AutoCatBoostClassifier_QA.csv'))
CatBoost_QA_Results_MultiClass <- data.table::fread(file.path(CSVPath, 'AutoCatBoostMultiClass_QA.csv'))
CatBoost_QA_Results_Regression <- data.table::fread(file.path(CSVPath, 'AutoCatBoostRegression_QA.csv'))
CatBoost_QA_Results_HurdleModel <- data.table::fread(file.path(CSVPath, 'AutoCatBoostHurdleModel_QA.csv'))
CatBoost_QA <- data.table::rbindlist(list(
  CatBoost_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'CatBoost'],
  CatBoost_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'CatBoost'],
  CatBoost_QA_Results_Regression[, Type := 'Regression'][, Model := 'CatBoost'],
  CatBoost_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'CatBoost']),
  use.names = TRUE, fill = TRUE)
View(CatBoost_QA)

# . ----

# LightGBM Evaluation ----
LightGBM_QA_Results_Classifier <- data.table::fread(file.path(CSVPath, 'AutoLightGBMClassifier_QA.csv'))
LightGBM_QA_Results_MultiClass <- data.table::fread(file.path(CSVPath, 'AutoLightGBMMultiClass_QA.csv'))
LightGBM_QA_Results_Regression <- data.table::fread(file.path(CSVPath, 'AutoLightGBMRegression_QA.csv'))
LightGBM_QA_Results_HurdleModel <- data.table::fread(file.path(CSVPath, 'AutoLightGBMHurdleModel_QA.csv'))
LightGBM_QA <- data.table::rbindlist(list(
  LightGBM_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'LightGBM'],
  LightGBM_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'LightGBM'],
  LightGBM_QA_Results_Regression[, Type := 'Regression'][, Model := 'LightGBM'],
  LightGBM_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(LightGBM_QA)

# . ----

# XGBoost Evaluation ----
XGBoost_QA_Results_Classifier <- data.table::fread(file.path(CSVPath, 'AutoXGBoostClassifier_QA.csv'))
XGBoost_QA_Results_MultiClass <- data.table::fread(file.path(CSVPath, 'AutoXGBoostMultiClass_QA.csv'))
XGBoost_QA_Results_Regression <- data.table::fread(file.path(CSVPath, 'AutoXGBoostRegression_QA.csv'))
XGBoost_QA_Results_HurdleModel <- data.table::fread(file.path(CSVPath, 'AutoXGBoostHurdleModel_QA.csv'))
XGBoost_QA <- data.table::rbindlist(list(
  XGBoost_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'XGBoost'],
  XGBoost_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'XGBoost'],
  XGBoost_QA_Results_Regression[, Type := 'Regression'][, Model := 'XGBoost'],
  XGBoost_QA_Results_HurdleModel[, Type := 'Hurdle'][, Model := 'XGBoost']),
  use.names = TRUE, fill = TRUE)
View(XGBoost_QA)

# . ----

# H2O Evaluation ----
H2O_QA_Results_Classifier <- data.table::fread(file.path(CSVPath, 'AutoCatBoostClassifier_QA.csv'))
H2O_QA_Results_MultiClass <- data.table::fread(file.path(CSVPath, 'AutoCatBoostMultiClass_QA.csv'))
H2O_QA_Results_Regression <- data.table::fread(file.path(CSVPath, 'AutoCatBoostRegression_QA.csv'))
H2O_QA <- data.table::rbindlist(list(
  H2O_QA_Results_Classifier[, Type := 'Classifier'][, Model := 'h2o'],
  H2O_QA_Results_MultiClass[, Type := 'MultiClass'][, Model := 'h2o'],
  H2O_QA_Results_Regression[, Type := 'Regression'][, Model := 'h2o']),
  use.names = TRUE, fill = TRUE)
View(H2O_QA)

# . ----

# CARMA Evaluation ----
CatBoostCARMA <- data.table::fread(file.path(CSVPath, 'AutoCatBoostCARMA_QA.csv'))
XGBoostCARMA <- data.table::fread(file.path(CSVPath, 'AutoXGBoostCARMA_QA.csv'))
LightGBMCARMA <- data.table::fread(file.path(CSVPath, 'AutoLightGBMCARMA_QA.csv'))
CARMA_QA <- data.table::rbindlist(list(
  CatBoostCARMA[, Type := 'CARMA'][, Model := 'CatBoost'],
  XGBoostCARMA[, Type := 'CARMA'][, Model := 'XGBoost'],
  LightGBMCARMA[, Type := 'CARMA'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(CARMA_QA)

# . ----

# Hurdle CARMA Evaluation ----
CatBoostHurdleCARMA <- data.table::fread(file.path(CSVPath, 'AutoCatBoostHurdleCARMA_QA.csv'))
XGBoostHurdleCARMA <- data.table::fread(file.path(CSVPath, 'AutoXGBoostHurdleCARMA_QA.csv'))
LightGBMHurdleCARMA <- data.table::fread(file.path(CSVPath, 'AutoLightGBMHurdleCARMA_QA.csv'))
HurdleCARMA_QA <- data.table::rbindlist(list(
  CatBoostHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'CatBoost'],
  XGBoostHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'XGBoost'],
  LightGBMHurdleCARMA[, Type := 'Hurdle_CARMA'][, Model := 'LightGBM']),
  use.names = TRUE, fill = TRUE)
View(HurdleCARMA_QA)

# . ----

# Vector CARMA Evaluation ----
CatBoostVectorCARMA <- data.table::fread(file.path(CSVPath, 'AutoCatBoostVectorCARMA_QA.csv'))
View(CatBoostVectorCARMA)

# . ----

# Funnel CARMA Evaluation ----
CatBoostFunnel <- data.table::fread(file.path(CSVPath, 'AutoCatBoostFunnel_QA.csv'))
LightGBMFunnel <- data.table::fread(file.path(CSVPath, 'AutoLightGBMFunnel_QA.csv'))
XGBoostFunnel <- data.table::fread(file.path(CSVPath, 'AutoXGBoostFunnel_QA.csv'))
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
if(exists("XGBoost_QA")) xgb <- XGBoost_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success","Score")]
if(exists("H2O_QA")) h2o <- H2O_QA[, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("CARMA_QA")) Carma <- CARMA_QA[, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("HurdleCARMA_QA")) HurdleCARMA <- HurdleCARMA_QA[, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("VectorCARMA")) VectorCARMA <- CatBoostVectorCARMA[, Model := "CatBoost"][, Type := "Vector_CARMA"][, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Success")]
if(exists("Funnel_QA")) FunnelCarma <- Funnel_QA[, Type := "Vector_CARMA"][, RunNumber := seq_len(.N)][, .SD, .SDcols = c("Model","Type","RunNumber","Training","Forecast")]
All_Methods <- list()
for(zz in c("Cat","Light","xgb","h2o","Carma","HurdleCARMA","VectorCARMA","FunnelCarma")) {
  if(exists(zz)) All_Methods[[zz]] <- eval(expr = parse(text = zz))
}
All_Methods <- data.table::rbindlist(All_Methods, fill = TRUE)
OverallEnd <- Sys.time()
print(difftime(time1 = OverallEnd, time2 = OverallStart, units = 'mins'))

# . ----

# . ----
