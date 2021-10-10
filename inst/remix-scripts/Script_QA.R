library(data.table)
library(RemixAutoML)
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ReinforcementLearningFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/GridTuning.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/MiscFunctions.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/CatBoostHelpers.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelMetrics.R"))
source(file.path("C:/Users/Bizon/Documents/GitHub/RemixAutoML/R/ModelEvaluationPlots.R"))

# MultiClass CatBoost ----
OutputSelection = if(FeatureTune) c("EvalMetrics") else ArgsList$Modeling$CatBoost[["ReturnServices"]]
task_type = ArgsList$Modeling$CatBoost[["task_type"]]
NumGPUs = ArgsList$Modeling$CatBoost[["NumGPUs"]]
TrainOnFull = TOF
PassInGrid = PassInGrid.
DebugMode = TRUE
ModelID = paste0(ArgsList$MetaData[["ProjectID"]], "_AutoCatBoostRegression")
model_path = ArgsList$MetaData[["Model_Path"]]
metadata_path = ArgsList$MetaData[["Results_Path"]]
SaveModelObjects = if(FeatureTune) FALSE else TRUE
ReturnModelObjects = TRUE
data = TrainData
ValidationData = ValidationData
TestData = TestData
TargetColumnName = ArgsList$Data[["TargetVariables"]]
FeatureColNames = Features
PrimaryDateColumn = ArgsList$Data[["DateVariables"]][1L]
WeightsColumnName = ArgsList$Data[["WeightsColumnName"]]
IDcols = IDcols.
ClassWeights = ArgsList$Modeling$CatBoost[["ClassWeights"]]
eval_metric = ArgsList$Modeling$CatBoost[["eval_metric"]]
loss_function = ArgsList$Modeling$CatBoost[["loss_function"]]
grid_eval_metric = ArgsList$Modeling$CatBoost[["grid_eval_metric"]]
MetricPeriods = ArgsList$Modeling$CatBoost[["MetricPeriods"]]
NumOfParDepPlots = ArgsList$Modeling$CatBoost[["NumOfParDepPlots"]]
GridTune = GridTune
MaxModelsInGrid = ArgsList$Modeling$CatBoost[["MaxModelsInGrid"]]
MaxRunsWithoutNewWinner = ArgsList$Modeling$CatBoost[["MaxRunsWithoutNewWinner"]]
MaxRunMinutes = ArgsList$Modeling$CatBoost[["MaxRunMinutes"]]
BaselineComparison = "default"
Trees = Args[["Trees"]]
Depth = Args[["Depth"]]
LearningRate = Args[["LearningRate"]]
L2_Leaf_Reg = Args[["L2_Leaf_Reg"]]
RandomStrength = Args[["RandomStrength"]]
BorderCount = Args[["BorderCount"]]
RSM = Args[["RSM"]]
BootStrapType = Args[["BootStrapType"]]
GrowPolicy = Args[["GrowPolicy"]]
model_size_reg = ArgsList$Modeling$CatBoost[["model_size_reg"]]
langevin = ArgsList$Modeling$CatBoost[["langevin"]]
diffusion_temperature = ArgsList$Modeling$CatBoost[["diffusion_temperature"]]
feature_border_type = ArgsList$Modeling$CatBoost[["feature_border_type"]]
sampling_unit = ArgsList$Modeling$CatBoost[["sampling_unit"]]
subsample = ArgsList$Modeling$CatBoost[["subsample"]]
score_function = ArgsList$Modeling$CatBoost[["score_function"]]
min_data_in_leaf = ArgsList$Modeling$CatBoost[["min_data_in_leaf"]]

# Encode
RunMode='train'
ArgsList=ArgsList
TrainData=TrainData
ValidationData=ValidationData
TestData=TestData

# EncodeCharacterVariables
RunMode = 'train'
ModelType = ArgsList$MetaData$ModelType
TrainData = TrainData
ValidationData = ValidationData
TestData = TestData
TargetVariableName = ArgsList$Data$TargetVariables
CategoricalVariableNames = ArgsList$Data$GroupVariables
EncodeMethod = ArgsList$FE_Args$Encoding$EncodeMethod
KeepCategoricalVariables = ArgsList$FE_Args$Encoding$KeepCharColumns
ReturnMetaData = FALSE
MetaDataPath = ArgsList$MetaData$MetaData_Path
MetaDataList = NULL
ImputeMissingValue = ArgsList$FE_Args$Encoding$EncodeImpute


# CategoricalEncoding
data=temp_train
ML_Type=ModelType
GroupVariables=CategoricalVariableNames
TargetVariable=TargetVariableName
Method=EncodeMethod
SavePath=MetaDataPath
Scoring=Score
ImputeValueScoring=ImputeMissingValue
ReturnFactorLevelList=TRUE
SupplyFactorLevelList=MetaDataList
KeepOriginalFactors=KeepCategoricalVariables

# ML_Tune_Path
TargetType = "classifier"
Results.=Results
ArgsList=ArgsList
FeatureTune.=FeatureTune
GridTune.=GridTune
LeaveOneOut.=LeaveOneOut
MetricName="Utility"
