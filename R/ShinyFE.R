#' @title Shiny.FE.Date.Calendar
#'
#' @description server.R observeEvent() for executing CreateCalendarVariables() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Date.Calendar <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  print('FE Calendar Variables')

  # Dispatch
  CalendarVar_DateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CalendarVariables_DateVariables']]}, error=function(x) NULL), VarName = 'CalendarVariables_DateVariables', Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(CalendarVar_DateVariables)
  if(length(CalendarVar_DateVariables) != 0) {

    # Pull in values
    CalendarVar_TimeUnits <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CalendarVariables_TimeUnits']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
    temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$CalendarVariables_SelectData}, error=function(x) NULL), VarName = 'CalendarVariables_SelectData', Type = 'character', Default = NULL, Debug = Debug)

    # Caching
    if(Debug) print('FE Calendar Variables 1')
    if(length(CacheDir) == 0L) {
      path <- NULL
      x <- DataList[[temp]]
    } else {
      path <- RemixAutoML:::Shiny.Utils.CachePath(CacheName, CacheDir, Ext = '.csv')
      x <- NULL
      if(Debug) {print(path);print(length(path))}
    }

    # Run function
    if(Debug) print('FE Calendar Variables 2')
    x <- RemixAutoML::CreateCalendarVariables(
      data = x,
      DateCols = CalendarVar_DateVariables,
      AsFactor = FALSE,
      TimeUnits = CalendarVar_TimeUnits,
      CachePath = path,
      Debug = Debug)

    # Caching
    if(Debug) print('FE Calendar Variables 3')
    if(length(CacheDir) == 0L) {
      DataList[[temp]] <- x
    } else {
      DataList[[temp]] <- RemixAutoML:::ReactiveLoadCSV(Infile = path, Debug = Debug)
    }

    # Display data
    if(Debug) print('FE Calendar Variables 4')
    output$FE_DisplayData <- DT::renderDataTable({
      RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000))])
    })
  }

  # Code
  if(Debug) print('FE Calendar Variables 5')
  CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Calendar Variables\n",
    "temp <- ", RemixAutoML:::CEP(temp),"\n",
    "DataList[[temp]] <- RemixAutoML::CreateCalendarVariables(\n  ",
    "data = DataList[[temp]],\n  ",
    "DateCols = ", RemixAutoML:::ExpandText(CalendarVar_DateVariables), ",\n  ",
    "AsFactor = FALSE,\n  ",
    "TimeUnits = ", RemixAutoML:::ExpandText(CalendarVar_TimeUnits), ")\n"
  ))

  # Return
  if(Debug) {
    print('FE Calendar Variables 6')
    print(names(DataList))
    print(CodeList)
  }

  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FE.Date.Holiday
#'
#' @description server.R observeEvent() for executing CreateHolidayVariables() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Date.Holiday <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Holiday Variable creation has begun..', value = 0, {

    print('FE Holiday Variables')

    # Dispatch
    HolidayVar_DateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_DateVariables']]}, error=function(x) NULL), VarName = NULL, Type = 'character', Default = NULL, Debug = Debug)
    if(length(HolidayVar_DateVariables) > 0L) {

      # Pull in values
      if(Debug) print('FE Holiday Variables 1')
      HolidayVar_HolidayGroups <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_HolidayGroups']]}, error=function(x) NULL), VarName = 'HolidayVariables_HolidayGroups', Type = 'character', Default = NULL, Debug = Debug)
      HolidayVar_LookbackDays <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['HolidayVariables_LookbackDays']]}, error=function(x) NULL), VarName = 'HolidayVariables_LookbackDays', Type = 'numeric', Default = 1, Debug = Debug)
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$HolidayVariables_SelectData}, error=function(x) NULL), VarName = 'HolidayVariables_SelectData', Type = 'character', Default = NULL, Debug = Debug)
      print(CacheDir)

      # Caching
      if(Debug) print('FE Calendar Variables 1')
      if(length(CacheDir) == 0L) {
        path <- NULL
        x <- DataList[[temp]]
      } else {
        path <- RemixAutoML:::Shiny.Utils.CachePath(CacheName, CacheDir, Ext = '.csv')
        x <- NULL
        if(Debug) {print(path);print(length(path))}
      }

      # if path is a character then data will be pulled inside the function
      #  otherwise you're passing data directly to function
      if(Debug) print('FE Holiday Variables 3')
      x <- RemixAutoML::CreateHolidayVariables(
        x,
        DateCols = HolidayVar_DateVariables,
        LookbackDays = HolidayVar_LookbackDays,
        HolidayGroups = HolidayVar_HolidayGroups,
        CachePath = path,
        Debug = Debug)

      # Caching
      if(Debug) print('FE Calendar Variables 3')
      if(length(CacheDir) == 0L) {
        DataList[[temp]] <- x
      } else {
        DataList[[temp]] <- RemixAutoML:::ReactiveLoadCSV(Infile = path, Debug = Debug)
      }

      # Output table
      if(Debug) print('FE Holiday Variables 5')
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000))])
      })

      # Create code
      if(Debug) print('FE Holiday Variables 6')
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Holiday Variables\n",
        "temp <- ", RemixAutoML:::CEP(temp),"\n",
        "DataList[[temp]] <- RemixAutoML::CreateHolidayVariables(\n  ",
        "DataList[[temp]],\n  ",
        "DateCols = ", RemixAutoML:::ExpandText(HolidayVar_DateVariables), ",\n  ",
        "LookbackDays = ", RemixAutoML:::CEP(HolidayVar_LookbackDays), ",\n  ",
        "HolidayGroups = ", RemixAutoML:::ExpandText(HolidayVar_HolidayGroups),
        ")\n"
      ))

      # Return
      if(Debug) {
        print('FE Holiday Variables 6')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))

    } else {
      print(' :( FAILED ): ')
    }
  })
}

#' @title Shiny.FE.Numeric.PercentRank
#'
#' @description server.R observeEvent() for executing PercRank() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Numeric.PercentRank <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric PercentRank has begun..', value = 0, {
    print('FE Percent Rank')
    PercentRank_ColNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_ColNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(PercentRank_ColNames) != 0) {
      PercentRank_GroupVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      PercentRank_Granularity <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['PercentRank_Granularity']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$PercentRank_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp]]
      x <- RemixAutoML::PercRank(data = x, ColNames = PercentRank_ColNames, GroupVars = PercentRank_GroupVars, Granularity = PercentRank_Granularity)
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE PercRank 6')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Percent Rank: value --> percentile\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::PercRank(\n  ",
      "data = DataList[[temp]],\n  ",
      "ColNames = ", RemixAutoML:::ExpandText(PercentRank_ColNames), ",\n  ",
      "GroupVars = ", RemixAutoML:::ExpandText(PercentRank_GroupVars), ",\n  ",
      "Granularity = ", RemixAutoML:::CEP(PercentRank_Granularity),")\n"
    ))

    # Return
    if(Debug) {
      print('FE PercRank 6')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Numeric.Interactions
#'
#' @description server.R observeEvent() for executing AutoInteraction() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Numeric.Interactions <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric Variable Interaction has begun..', value = 0, {
    print('FE Interaction')
    AutoInteraction_NumericVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_NumericVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoInteraction_NumericVars) != 0) {
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoInteraction_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoInteraction_InteractionDepth <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_InteractionDepth']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)
      AutoInteraction_Scale <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Scale']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      AutoInteraction_Center <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoInteraction_Center']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      x <- DataList[[temp]]
      x <- RemixAutoML::AutoInteraction(
        data = x,
        NumericVars = AutoInteraction_NumericVars,
        InteractionDepth = AutoInteraction_InteractionDepth,
        Center = AutoInteraction_Center,
        Scale = AutoInteraction_Scale,
        SkipCols = NULL,
        Scoring = FALSE,
        File = NULL)
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE Numeric Interaction 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Interaction: value --> percentile\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::AutoInteraction(\n  ",
      "data = DataList[[temp]],\n  ",
      "NumericVars = ", RemixAutoML:::ExpandText(AutoInteraction_NumericVars), ",\n  ",
      "InteractionDepth = ", RemixAutoML:::CEP(AutoInteraction_InteractionDepth), ",\n  ",
      "Center = ", RemixAutoML:::CEP(AutoInteraction_Center), ",\n  ",
      "Scale = ", RemixAutoML:::CEP(AutoInteraction_Scale), ",\n  ",
      "SkipCols = NULL,\n  ",
      "Scoring = FALSE,\n  ",
      "File = NULL)\n"
    ))

    # Return
    if(Debug) {
      print('FE Numeric Interaction 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  })
}

#' @title Shiny.FE.Numeric.Transformations
#'
#' @description server.R observeEvent() for executing AutoTransformationCreate() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Numeric.Transformations <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Numeric Variable Transformations has begun..', value = 0, {
    print('FE Numeric Transformations')
    AutoTransformationCreate_ColumnNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_ColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoTransformationCreate_ColumnNames) != 0) {
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoTransformationCreate_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoTransformationCreate_Methods <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoTransformationCreate_Methods']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp]]
      x <- RemixAutoML::AutoTransformationCreate(
        data = x,
        ColumnNames = AutoTransformationCreate_ColumnNames,
        Methods = AutoTransformationCreate_Methods,
        Path = NULL,
        TransID = "ModelID",
        SaveOutput = FALSE)$Data
      DataList[[eval(input$AutoTransformationCreate_SelectData)]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE Numeric Transformation 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Numeric Transformation: value --> percentile\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::AutoTransformationCreate(\n  ",
      "data = DataList[[temp]],\n  ",
      "ColumnNames = ", RemixAutoML:::ExpandText(AutoTransformationCreate_ColumnNames), ",\n  ",
      "Methods = ", RemixAutoML:::ExpandText(AutoTransformationCreate_Methods), ",\n  ",
      "Path = NULL,\n  ",
      "TransID = 'ModelID',\n  ",
      "SaveOutput = FALSE)\n"
    ))

    # Return
    if(Debug) {
      print('FE Numeric Transformation 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.Categorical.Dummify
#'
#' @description server.R observeEvent() for executing DummifyDT() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Categorical.Dummify <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Dummy variables creation has begun..', value = 0, {
    print('FE Partial Dummies')
    DummifyDT_Cols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_Cols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(DummifyDT_Cols) != 0) {
      DummifyDT_TopN <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_TopN']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      DummifyDT_KeepBaseCols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_KeepBaseCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['DummifyDT_SelectData']]}, error = function(x) NULL), VarName = 'DummifyDT_SelectData', Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp]]
      x <- RemixAutoML::DummifyDT(
        data = x,
        cols = DummifyDT_Cols,
        TopN = DummifyDT_TopN,
        KeepFactorCols = as.logical(DummifyDT_KeepBaseCols),
        OneHot=FALSE, SaveFactorLevels=FALSE, SavePath=NULL, ImportFactorLevels=FALSE, FactorLevelsList=NULL, ClustScore=FALSE, ReturnFactorLevels=FALSE, GroupVar=FALSE)
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE Partial Dummies 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Partial Dummify Variables\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::DummifyDT(\n  ",
      "data = DataList[[temp]],\n  ",
      "cols = ", RemixAutoML:::ExpandText(DummifyDT_Cols), ",\n  ",
      "TopN = ", RemixAutoML:::CEP(DummifyDT_TopN), ",\n  ",
      "KeepFactorCols = ", RemixAutoML:::CEP(as.logical(DummifyDT_KeepBaseCols)), ",\n  ",
      "OneHot = FALSE,\n  ",
      "SaveFactorLevels = FALSE,\n  ",
      "SavePath = NULL,\n  ",
      "ImportFactorLevels = FALSE,\n  ",
      "FactorLevelsList = NULL,\n  ",
      "ReturnFactorLevels = FALSE)\n"
    ))

    # Return
    if(Debug) {
      print('FE Partial Dummies 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  })
}

#' @title Shiny.FE.CrossRow.CategoricalEncoding
#'
#' @description server.R observeEvent() for executing CategoricalEncoding() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.CrossRow.CategoricalEncoding <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Categorical Encoding has begun..', value = 0, {
    print('FE Categorical Encoding')
    CategoricalEncoding_GroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(CategoricalEncoding_GroupVariables) != 0) {
      CategoricalEncoding_TargetVariable <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_TargetVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      CategoricalEncoding_Method <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['CategoricalEncoding_Method']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      temp_train <- RemixAutoML:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TrainData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp_train]]
      temp_validate <- RemixAutoML:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_ValidationData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(temp_validate) > 0L) y <- DataList[[temp_validate]] else y <- NULL
      temp_test <- RemixAutoML:::ReturnParam(xx = tryCatch({input$CategoricalEncoding_TestData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(temp_test) > 0L) z <- DataList[[temp_train]] else z <- NULL

      # Identify target type
      if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {
        MLType <- 'MultiClass'
      } else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {
        MLType <- 'Classification'
      } else {
        MLType <- 'Regression'
      }

      # Build Features
      Output <- RemixAutoML:::EncodeCharacterVariables(
        RunMode = 'train',
        ModelType = MLType,
        TrainData = x,
        ValidationData = y,
        TestData = z,
        TargetVariableName = CategoricalEncoding_TargetVariable,
        CategoricalVariableNames = CategoricalEncoding_GroupVariables,
        EncodeMethod = CategoricalEncoding_Method,
        KeepCategoricalVariables = TRUE,
        ReturnMetaData = TRUE,
        MetaDataPath = NULL,
        MetaDataList = NULL,
        ImputeMissingValue = 0)
      DataList[[temp_train]] <- x
      if(length(y) > 0L) DataList[[temp_validate]] <- y
      if(length(z) > 0L) DataList[[temp_test]] <- z
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000L))])
      })
    }

    # Create code
    if(Debug) print('FE Categorical Encoding 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Categorical Encoding\n",
      "temp_train <- ", RemixAutoML:::CEP(temp_train),"\n",
      "if(class(x[[eval(CategoricalEncoding_TargetVariable)]])[1L] %in% c('character','factor')) {\n  ",
      "temp_validate <- ", RemixAutoML:::CEP(temp_validate), "\n",
      "if(length(temp_validate) > 0L) y <- DataList[[temp_validate]] else y <- NULL\n",
      "temp_test <- ", RemixAutoML:::CEP(temp_test), "\n",
      "if(length(temp_test) > 0L) z <- DataList[[temp_train]] else z <- NULL\n  ",
      "MLType <- 'multiclass'\n",
      "} else if(all(unique(x[[eval(CategoricalEncoding_TargetVariable)]]) %in% c(0,1))) {\n  ",
      "MLType <- 'classification'\n",
      "} else {\n  ",
      "MLType <- 'regression'\n",
      "}\n",
      "Output <- RemixAutoML:::EncodeCharacterVariables(","\n  ",
      "RunMode = 'train',\n  ",
      "ModelType = MLType,\n  ",
      "TrainData = x,\n  ",
      "ValidationData = y,\n  ",
      "TestData = z,\n  ",
      "TargetVariableName = ", RemixAutoML:::CEP(CategoricalEncoding_TargetVariable), ",\n  ",
      "CategoricalVariableNames = ", RemixAutoML:::ExpandText(CategoricalEncoding_GroupVariables), ",\n  ",
      "EncodeMethod = ", RemixAutoML:::CEP(CategoricalEncoding_Method), ",\n  ",
      "KeepCategoricalVariables = TRUE,\n  ",
      "ReturnMetaData = TRUE,\n  ",
      "MetaDataPath = NULL,\n  ",
      "MetaDataList = NULL,\n  ",
      "ImputeMissingValue = 0)","\n",
      "TrainData <- Output$TrainData\n",
      "ValidationData <- Output$ValidationData\n",
      "TestData <- Output$TestData; rm(Output)\n"
    ))

    # Return
    if(Debug) {
      print('FE Categorical Encoding 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  })
}

#' @title Shiny.FE.CrossRow.RollingMode
#'
#' @description server.R observeEvent() for executing AutoLagRollMode() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.CrossRow.RollingMode <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Rolling Mode has begun..', value = 0, {
    if(Debug) print('FE Auto Lag Roll Mode')
    AutoLagRollMode_Targets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 1')
    AutoLagRollMode_SortDateName <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_SortDateName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Auto Lag Roll Mode 6')
    if(Debug) print(input[['AutoLagRollMode_WindowingLag']])
    if(length(AutoLagRollMode_Targets) > 0 && length(AutoLagRollMode_SortDateName) > 0) {
      if(Debug) print('FE Auto Lag Roll Mode 2')
      AutoLagRollMode_Lags <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_Lags', Type = 'numeric']]}, error=function(x) NULL), Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 3')
      AutoLagRollMode_ModePeriods <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_ModePeriods']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 4')
      AutoLagRollMode_GroupingVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_GroupingVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 5')
      AutoLagRollMode_WindowingLag <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollMode_WindowingLag']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 7')
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoLagRollMode_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 8')
      x <- DataList[[temp]]

      # Checkpoint
      gggg <- TRUE; for(ggg in seq_along(AutoLagRollMode_Targets)) if(class(x[[eval(ggg)]])[1L] %in% 'numeric') gggg <- FALSE
      if(!gggg) return(NULL)

      if(Debug) print('FE Auto Lag Roll Mode 9')

      if(Debug) print(x)
      if(Debug) print(AutoLagRollMode_Targets)
      if(Debug) print(AutoLagRollMode_GroupingVars)
      if(Debug) print(AutoLagRollMode_SortDateName)
      if(Debug) print(AutoLagRollMode_WindowingLag)
      if(Debug) print(AutoLagRollMode_Lags)
      if(Debug) print(AutoLagRollMode_ModePeriods)


      x <- RemixAutoML::AutoLagRollMode(
        data = x,
        Targets = AutoLagRollMode_Targets,
        GroupingVars = AutoLagRollMode_GroupingVars,
        SortDateName = AutoLagRollMode_SortDateName,
        WindowingLag = AutoLagRollMode_WindowingLag,
        Lags = AutoLagRollMode_Lags,
        ModePeriods = AutoLagRollMode_ModePeriods,
        Type = c("Lag"),
        SimpleImpute = TRUE,
        Debug = Debug)
      if(Debug) print('FE Auto Lag Roll Mode 10')
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
      })

      if(Debug) print('FE Auto Lag Roll Mode 11')

      # Create code
      if(Debug) print('FE Auto Lag Roll Mode 1')
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Auto Lag Roll Mode\n",
        "temp <- ", RemixAutoML:::CEP(temp),"\n",
        "DataList[[temp]] <- RemixAutoML::AutoLagRollMode(\n  ",
        "data = DataList[[temp]],\n  ",
        "Targets = ", RemixAutoML:::ExpandText(AutoLagRollMode_Targets), ",\n  ",
        "GroupingVars = ", RemixAutoML:::ExpandText(AutoLagRollMode_GroupingVars), ",\n  ",
        "SortDateName = ", RemixAutoML:::CEP(AutoLagRollMode_SortDateName), ",\n  ",
        "WindowingLag = ", RemixAutoML:::CEP(AutoLagRollMode_WindowingLag), ",\n  ",
        "Lags = ", RemixAutoML:::ExpandText(AutoLagRollMode_Lags), ",\n  ",
        "ModePeriods = ", RemixAutoML:::ExpandText(AutoLagRollMode_ModePeriods), ",\n  ",
        "Type = 'Lag',\n  ",
        "SimpleImpute = TRUE)\n"
      ))

      # Return
      if(Debug) {
        print('FE Auto Lag Roll Mode 2')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))
    }
  })
}

#' @title Shiny.FE.CrossRow.RollingStats
#'
#' @description server.R observeEvent() for executing AutoLagRollStats() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.CrossRow.RollingStats <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  shiny::withProgress(message = 'Rolling Stats has begun..', value = 0, {
    print('FE Auto Lag Roll Stats')
    AutoLagRollStats_Targets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Targets']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_DateColumn <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_DateColumn']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoLagRollStats_TimeUnits <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_TimeUnits']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(length(AutoLagRollStats_Targets) != 0 && length(AutoLagRollStats_DateColumn) != 0 && length(AutoLagRollStats_TimeUnits) != 0) {
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoLagRollStats_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp]]
      AutoLagRollStats_GroupVars <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_GroupVars']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1L) {
        x[, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]
        AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')
      }
      AutoLagRollStats_Lags <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Lags']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_RollOnLag1 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_RollOnLag1']]}, error=function(x) NULL), Type = 'logical', Default = NULL, Debug = Debug)
      AutoLagRollStats_MA_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_MA_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_SD_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_SD_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Skew_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Skew_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Kurt_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Kurt_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantile_RollWindows <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantile_RollWindows']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      AutoLagRollStats_Quantiles_Selected <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoLagRollStats_Quantiles_Selected']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
      x <- RemixAutoML::AutoLagRollStats(
        data                 = x,
        Targets              = AutoLagRollStats_Targets,
        HierarchyGroups      = NULL,
        IndependentGroups    = AutoLagRollStats_GroupVars,
        DateColumn           = AutoLagRollStats_DateColumn,
        TimeUnit             = AutoLagRollStats_TimeUnits,
        TimeUnitAgg          = AutoLagRollStats_TimeUnits,
        TimeGroups           = AutoLagRollStats_TimeUnits,
        TimeBetween          = NULL,
        RollOnLag1           = AutoLagRollStats_RollOnLag1,
        Type                 = "Lag",
        SimpleImpute         = TRUE,
        Lags                 = AutoLagRollStats_Lags,
        MA_RollWindows       = AutoLagRollStats_MA_RollWindows,
        SD_RollWindows       = AutoLagRollStats_SD_RollWindows,
        Skew_RollWindows     = AutoLagRollStats_Skew_RollWindows,
        Kurt_RollWindows     = AutoLagRollStats_Kurt_RollWindows,
        Quantile_RollWindows = AutoLagRollStats_Quantile_RollWindows,
        Quantiles_Selected   = AutoLagRollStats_Quantiles_Selected,
        Debug = Debug)
      if(length(AutoLagRollStats_GroupVars) > 1) {
        data.table::set(x, j = paste0(AutoLagRollStats_GroupVars, collapse = '_'), value = NULL)
      }
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE Auto Lag Roll Stats 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Auto Lag Roll Stats\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "AutoLagRollStats_GroupVars <- ", RemixAutoML:::ExpandText(AutoLagRollStats_GroupVars), "\n",
      "if(length(AutoLagRollStats_GroupVars) > 1L) {\n  ",
      "DataList[[temp]][, paste0(AutoLagRollStats_GroupVars, collapse = '_') := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(AutoLagRollStats_GroupVars)]\n  ",
      "AutoLagRollStats_GroupVars <- paste0(AutoLagRollStats_GroupVars, collapse = '_')\n",
      "}\n",
      "DataList[[temp]] <- RemixAutoML::AutoLagRollStats(\n  ",
      "data <- DataList[[temp]],\n  ",
      "Targets = ",RemixAutoML:::ExpandText(AutoLagRollStats_Targets), ",\n  ",
      "HierarchyGroups = NULL,\n  ",
      "IndependentGroups = AutoLagRollStats_GroupVars,\n  ",
      "DateColumn = ", RemixAutoML:::CEP(AutoLagRollStats_DateColumn), ",\n  ",
      "TimeUnit = ", RemixAutoML:::ExpandText(AutoLagRollStats_TimeUnits), ",\n  ",
      "TimeUnitAgg = ", RemixAutoML:::ExpandText(AutoLagRollStats_TimeUnits), ",\n  ",
      "TimeGroups = ", RemixAutoML:::ExpandText(AutoLagRollStats_TimeUnits), ",\n  ",
      "TimeBetween = NULL,\n  ",
      "RollOnLag1 = ", RemixAutoML:::CEP(AutoLagRollStats_RollOnLag1), ",\n  ",
      "Type <- 'Lag',\n  ",
      "SimpleImpute = TRUE,\n  ",
      "Lags = ", RemixAutoML:::ExpandText(AutoLagRollStats_Lags), ",\n  ",
      "MA_RollWindows = ", RemixAutoML:::ExpandText(AutoLagRollStats_MA_RollWindows), ",\n  ",
      "SD_RollWindows = ", RemixAutoML:::ExpandText(AutoLagRollStats_SD_RollWindows), ",\n  ",
      "Skew_RollWindows = ", RemixAutoML:::ExpandText(AutoLagRollStats_Skew_RollWindows), ",\n  ",
      "Kurt_RollWindows = ", RemixAutoML:::ExpandText(AutoLagRollStats_Kurt_RollWindows),",\n  ",
      "Quantile_RollWindows = ", RemixAutoML:::ExpandText(AutoLagRollStats_Quantile_RollWindows), ",\n  ",
      "Quantiles_Selected = ", RemixAutoML:::ExpandText(AutoLagRollStats_Quantiles_Selected), ",\n  ",
      "KeepOriginalFactors = TRUE)\n"
    ))

    # Return
    if(Debug) {
      print('FE Auto Lag Roll Stats 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  })
}

#' @title Shiny.FE.CrossRow.Differencing
#'
#' @description server.R observeEvent() for executing AutoDiffLagN() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.CrossRow.Differencing <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  print('FE Auto Diff')
  shiny::withProgress(message = 'Differencing has begun..', value = 0, {
    AutoDiffLagN_DateVariable <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DateVariable']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag1 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag1']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    AutoDiffLagN_NLag2 <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_NLag2']]}, error=function(x) NULL), Type = 'numeric', Default = NULL, Debug = Debug)
    if(length(AutoDiffLagN_DateVariable) != 0 &&
       length(AutoDiffLagN_NLag1) != 0 &&
       length(AutoDiffLagN_NLag2) != 0) {
      AutoDiffLagN_GroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_GroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffDateVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffDateVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      AutoDiffLagN_DiffGroupVariables <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDiffLagN_DiffGroupVariables']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoDiffLagN_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
      x <- DataList[[temp]]
      x <- RemixAutoML::AutoDiffLagN(
        data = x,
        DateVariable = AutoDiffLagN_DateVariable,
        GroupVariables = AutoDiffLagN_GroupVariables,
        DiffVariables = AutoDiffLagN_DiffVariables,
        DiffDateVariables = AutoDiffLagN_DiffDateVariables,
        DiffGroupVariables = AutoDiffLagN_DiffGroupVariables,
        NLag1 = AutoDiffLagN_NLag1,
        NLag2 = AutoDiffLagN_NLag2,
        Sort = FALSE,
        RemoveNA = TRUE)
      DataList[[temp]] <- x
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000))])
      })
    }

    # Create code
    if(Debug) print('FE Auto Diff Lag N 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Differencing Variables\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::AutoDiffLagN(\n  ",
      "data = DataList[[temp]],\n  ",
      "DateVariable = ", RemixAutoML:::ExpandText(AutoDiffLagN_DateVariable), ",\n  ",
      "GroupVariables = ", RemixAutoML:::ExpandText(AutoDiffLagN_GroupVariables), ",\n  ",
      "DiffVariables = ", RemixAutoML:::ExpandText(AutoDiffLagN_DiffVariables), ",\n  ",
      "DiffDateVariables = ", RemixAutoML:::ExpandText(AutoDiffLagN_DiffDateVariables), ",\n  ",
      "DiffGroupVariables = ", RemixAutoML:::ExpandText(AutoDiffLagN_DiffGroupVariables), ",\n  ",
      "NLag1 = ", RemixAutoML:::CEP(AutoDiffLagN_NLag1), ",\n  ",
      "NLag2 = ", RemixAutoML:::CEP(AutoDiffLagN_NLag2), ",\n  ",
      "Sort = TRUE,\n  ",
      "RemoveNA = TRUE)\n"
    ))

    # Return
    if(Debug) {
      print('FE Auto Diff Lag N 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  })
}

#' @title Shiny.FE.ModelDataPrep
#'
#' @description server.R observeEvent() for executing ModelDataPrep() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.ModelDataPrep <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  print('FE Type Conversion')
  shiny::withProgress(message = 'Type Casting has begun..', value = 0, {
    ModelDataPrep_IgnoreCols <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IgnoreCols']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    ModelDataPrep_CharToFactor <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_CharToFactor']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_FactorToChar <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_FactorToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_DateToChar <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_DateToChar']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IDateConversion <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IDateConversion']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_RemoveDates <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_RemoveDates']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_IntToNumeric <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_IntToNumeric']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_LogicalToBinary <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_LogicalToBinary']]}, error=function(x) NULL), Type = 'logical', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissFactor <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissFactor']]}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    ModelDataPrep_MissNum <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['ModelDataPrep_MissNum']]}, error=function(x) NULL), Type = 'numeric', Default = FALSE, Debug = Debug)
    temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$ModelDataPrep_SelectData}, error=function(x) NULL), Type = 'character', Default = FALSE, Debug = Debug)
    x <- DataList[[temp]]
    x <- RemixAutoML::ModelDataPrep(
      x,
      Impute          = FALSE,
      CharToFactor    = ModelDataPrep_CharToFactor,
      FactorToChar    = ModelDataPrep_FactorToChar,
      IntToNumeric    = ModelDataPrep_IntToNumeric,
      LogicalToBinary = ModelDataPrep_LogicalToBinary,
      DateToChar      = ModelDataPrep_DateToChar,
      IDateConversion = ModelDataPrep_IDateConversion,
      RemoveDates     = ModelDataPrep_RemoveDates,
      MissFactor      = ModelDataPrep_MissFactor,
      MissNum         = ModelDataPrep_MissNum,
      IgnoreCols      = ModelDataPrep_IgnoreCols)
    DataList[[temp]] <- x
    output$FE_DisplayData <- DT::renderDataTable({
      RemixAutoML::DataTable(DataList[[temp]][seq_len(min(.N, 1000))])
    })

    # Create code
    if(Debug) print('FE Auto Diff Lag N 1')
    CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Data Partitioning\n",
      "temp <- ", RemixAutoML:::CEP(temp),"\n",
      "DataList[[temp]] <- RemixAutoML::ModelDataPrep(","\n  ",
      "DataList[[temp]],\n  ",
      "CharToFactor = ", RemixAutoML:::CEP(ModelDataPrep_CharToFactor), ",\n  ",
      "FactorToChar = ", RemixAutoML:::CEP(ModelDataPrep_FactorToChar), ",\n  ",
      "IntToNumeric = ", RemixAutoML:::CEP(ModelDataPrep_IntToNumeric), ",\n  ",
      "LogicalToBinary = ", RemixAutoML:::CEP(ModelDataPrep_LogicalToBinary), ",\n  ",
      "DateToChar = ", RemixAutoML:::CEP(ModelDataPrep_DateToChar), ",\n  ",
      "IDateConversion <- ", RemixAutoML:::CEP(ModelDataPrep_IDateConversion), ",\n  ",
      "RemoveDates <- ", RemixAutoML:::CEP(ModelDataPrep_RemoveDates), ",\n  ",
      "MissFactor <- ", RemixAutoML:::CEP(ModelDataPrep_MissFactor), ",\n  ",
      "MissNum <- ", RemixAutoML:::CEP(ModelDataPrep_MissNum), ",\n  ",
      "IgnoreCols <- ", RemixAutoML:::ExpandText(ModelDataPrep_IgnoreCols), ")\n"
    ))

    # Return
    if(Debug) {
      print('FE Auto Diff Lag N 2')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  })
}

#' @title Shiny.FE.PartitionData
#'
#' @description server.R observeEvent() for executing AutoDataPartition() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.PartitionData <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  if(Debug) print('FE Data Partition')
  shiny::withProgress(message = 'Data Partitioning has begun..', value = 0, {
    if(Debug) print('FE Data Partition 1')
    AutoDataPartition_NumDataSets <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_NumDataSets']]}, error=function(x) NULL), Type = 'numeric', Default = 3L, Debug = Debug)
    if(Debug) print('FE Data Partition 2')
    AutoDataPartition_Ratios_Train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Train']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.70), Debug = Debug)
    if(Debug) print('FE Data Partition 3')
    AutoDataPartition_Ratios_Validation <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Validation']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.20), Debug = Debug)
    if(Debug) print('FE Data Partition 4')
    AutoDataPartition_Ratios_Test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_Ratios_Test']]}, error=function(x) NULL), Type = 'numeric', Default = c(0.10), Debug = Debug)
    if(Debug) print('FE Data Partition 5')
    AutoDataPartition_PartitionType <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_PartitionType']]}, error=function(x) NULL), Type = 'character', Default = "random", Debug = Debug)
    if(Debug) print('FE Data Partition 6')
    AutoDataPartition_StratifyColumnNames <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_StratifyColumnNames']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Data Partition 7')
    AutoDataPartition_TimeColumnName <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoDataPartition_TimeColumnName']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print('FE Data Partition 8')
    temp <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoDataPartition_SelectData}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print(temp)
    x <- DataList[[temp]]
    if(Debug) print(AutoDataPartition_Ratios_Train)
    if(Debug) print(AutoDataPartition_Ratios_Validation)
    if(Debug) print(AutoDataPartition_Ratios_Test)
    if(Debug) print(AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test < 1)
    if(Debug) print(AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test > 1)
    xx <- AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test < 1 || AutoDataPartition_Ratios_Train + AutoDataPartition_Ratios_Validation + AutoDataPartition_Ratios_Test > 1
    if(Debug) print(xx)
    if(xx) {
      DataSets <- RemixAutoML::AutoDataPartition(
        x,
        NumDataSets = AutoDataPartition_NumDataSets,
        Ratios = c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test),
        PartitionType = AutoDataPartition_PartitionType,
        StratifyColumnNames = AutoDataPartition_StratifyColumnNames,
        TimeColumnName = AutoDataPartition_TimeColumnName)
      DataList[[paste0(temp, '_TrainData')]] <- DataSets[['TrainData']]
      DataList[[paste0(temp, '_ValidationData')]] <- DataSets[['ValidationData']]
      DataList[[paste0(temp, '_TestData')]] <- DataSets[['TestData']]
      rm(DataSets)
      output$FE_DisplayData <- DT::renderDataTable({
        RemixAutoML::DataTable(x[seq_len(min(.N, 1000L))])
      })

      # Create code
      if(Debug) print('FE Auto Diff Lag N 1')
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Partition\n",
        "temp <- ", RemixAutoML:::CEP(temp),"\n",
        "DataSets <- RemixAutoML::AutoDataPartition(\n  ",
        "DataList[[temp]],\n  ",
        "NumDataSets = ", RemixAutoML:::CEP(AutoDataPartition_NumDataSets), ",\n  ",
        "Ratios = ", RemixAutoML:::ExpandText(c(AutoDataPartition_Ratios_Train, AutoDataPartition_Ratios_Validation, AutoDataPartition_Ratios_Test)), ",\n  ",
        "PartitionType = ", RemixAutoML:::CEP(AutoDataPartition_PartitionType), ",\n  ",
        "StratifyColumnNames = ", RemixAutoML:::ExpandText(AutoDataPartition_StratifyColumnNames), ",\n  ",
        "TimeColumnName = ", RemixAutoML:::CEP(AutoDataPartition_TimeColumnName), ")\n",
        "DataList[[paste0(temp, '_TrainData')]] <- DataSets[['TrainData']]\n",
        "DataList[[paste0(temp, '_ValidationData')]] <- DataSets[['ValidationData']]\n",
        "DataList[[paste0(temp, '_TestData')]] <- DataSets[['TestData']]\n",
        "rm(DataSets)\n"
      ))

      # Return
      if(Debug) {
        print('FE Data Partition 2')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))


    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Partition ratios do not sum to 1.0', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  })
}

#' @title Shiny.FE.Word2Vec.H2O
#'
#' @description server.R Word2Vec_H2O() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Word2Vec.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {
  print('FE Word2Vec H2O')
  if(Debug) print('FE Word2Vec H2O 1')

  # Data
  stringCol <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_stringCol']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(Debug) print(input[['H2O_Word2Vec_TrainData']])
  temp_train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]]
  temp_validate <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL
  temp_test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL
  if(Debug) print('FE Word2Vec H2O 2')

  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Word2Vec H2O: Data\n",
    "stringCol <- ", RemixAutoML:::ExpandText(stringCol),"\n",
    "temp_train <- ", RemixAutoML:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", RemixAutoML:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", RemixAutoML:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"
  ))

  # Build
  if(length(stringCol) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Word2Vec H2O has begun..', value = 0, {

      # Args
      BuildType <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_BuildType']]}, error=function(x) NULL), Type = 'character', Default = 'combined', Debug = Debug)
      KeepStringCol <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_KeepStringCol']]}, error=function(x) NULL), Type = 'logical', Default = TRUE, Debug = Debug)
      vects <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_vects']]}, error=function(x) NULL), Type = 'numeric', Default = 30, Debug = Debug)
      H2O_Word2Vec_MinWords <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 1, Debug = Debug)
      WindowSize <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_WindowSize']]}, error=function(x) NULL), Type = 'numeric', Default = 5, Debug = Debug)
      Epochs <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_Epochs']]}, error=function(x) NULL), Type = 'numeric', Default = 10, Debug = Debug)
      MinWords <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Word2Vec_H2O_MinWords']]}, error=function(x) NULL), Type = 'numeric', Default = 2, Debug = Debug)

      # Args tracking
      ModelID <- 'temp'
      model_path <- NULL
      Threads <- max(1L, parallel::detectCores()-2L)
      MaxMemory <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('WFE Word2Vec H2O 4')
      zz <- "awk "
      zzz <- "'/MemFree/ {print $2}' "
      zzzz <- "/proc/meminfo"
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Args\n",
        "BuildType <- ", RemixAutoML:::CEP(BuildType),"\n",
        "KeepStringCol <- ", RemixAutoML:::CEP(KeepStringCol),"\n",
        "vects <- ", RemixAutoML:::CEP(vects), "\n",
        "H2O_Word2Vec_MinWords <- ", RemixAutoML:::CEP(H2O_Word2Vec_MinWords),"\n",
        "WindowSize <- ", RemixAutoML:::CEP(WindowSize), "\n",
        "Epochs <- ", RemixAutoML:::CEP(Epochs),
        "MinWords <- ", RemixAutoML:::CEP(MinWords),
        "ModelID <- ", RemixAutoML:::CEP(ModelID),
        "model_path <- ", RemixAutoML:::CEP(model_path),
        "Threads <- ", RemixAutoML:::CEP(Threads),
        "gc()\n",
        "MaxMemory <- paste0(as.character(floor(as.numeric(system(", shQuote(paste0(zz,zzz,zzzz)), "intern=TRUE)) / 1000000)),'G')}\n"
      ))

      # Run function
      if(Debug) print('FE Word2Vec H2O 3')

      # MetaData
      Start <- Sys.time()
      tempnames <- names(TrainData)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O MetaData\n",
        "Start <- Sys.time()\n",
        "tempnames <- names(TrainData)\n"
      ))

      # Run AutoWord2VecModeler
      TrainData <- RemixAutoML::AutoWord2VecModeler(
        data = TrainData,
        BuildType = BuildType,
        stringCol = stringCol,
        KeepStringCol = TRUE,
        ModelID = ModelID,
        model_path = model_path,
        vects = vects,
        MinWords = MinWords,
        WindowSize = WindowSize,
        Epochs = Epochs,
        SaveModel = "standard",
        Threads = Threads,
        MaxMemory = MaxMemory)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Generate\n",
        "TrainData <- RemixAutoML::AutoWord2VecModeler(\n  ",
        "data = TrainData,\n  ",
        "BuildType = BuildType,\n  ",
        "stringCol = stringCol,\n  ",
        "KeepStringCol = TRUE,\n  ",
        "ModelID = ModelID,\n  ",
        "model_path = model_path,\n  ",
        "vects = vects,\n  ",
        "MinWords = MinWords,\n  ",
        "WindowSize = WindowSize,\n  ",
        "Epochs = Epoch,\n  ",
        "SaveModel = 'standard',\n  ",
        "Threads = Threads)\n"
      ))

      # Updates
      DataList[[temp_train]] <- TrainData; rm(TrainData); gc()
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Update DataList\n",
        "DataList[[temp_train]] <- TrainData; rm(TrainData); gc()\n"
      ))

      # Run time tracking
      End <- Sys.time()
      H2OWord2Vec_Training <- difftime(End, Start, units = "mins")

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Word2Vec H2O Generate\n",
        "End <- Sys.time()\n",
        "H2OWord2Vec_Training <- difftime(End, Start, units = 'mins')\n"
      ))

      # Score validation data
      if(!is.null(ValidationData)) {
        ValidationData <- RemixAutoML::AutoWord2VecScoring(
          data = ValidationData,
          BuildType = BuildType,
          stringCol = stringCol,
          KeepStringCol = TRUE,
          ModelID = ModelID,
          ModelObject = NULL,
          model_path = model_path,
          H2OStartUp = TRUE,
          H2OShutdown = TRUE,
          Threads = Threads,
          MaxMemory = MaxMemory)
        DataList[[temp_validate]] <- ValidationData; rm(ValidationData); gc()
        shiny::incProgress(2/3, detail = 'Validation Data is complete. Test Data is next up')
      } else {
        shiny::incProgress(2/3, detail = 'Validation Data is NULL')
      }

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Score validation data\n",
        "if(!is.null(ValidationData)) {\n  ",
        "ValidationData <- RemixAutoML::AutoWord2VecScoring(\n    ",
        "data = ValidationData,\n    ",
        "BuildType = BuildType,\n    ",
        "stringCol = stringCol,\n    ",
        "KeepStringCol = TRUE,\n    ",
        "ModelID = ModelID,\n    ",
        "ModelObject = NULL,\n    ",
        "model_path = model_path,\n    ",
        "H2OStartUp = TRUE,\n    ",
        "H2OShutdown = TRUE,\n    ",
        "Threads = Threads,\n    ",
        "MaxMemory = MaxMemory)\n  ",
        "DataList[[temp_validate]] <- ValidationData; rm(ValidationData); gc()\n",
        "}\n"
      ))

      # Score test data
      if(!is.null(TestData)) {
        TestData <- RemixAutoML::AutoWord2VecScoring(
          data = TestData,
          BuildType = BuildType,
          stringCol = stringCol,
          KeepStringCol = KeepStringCol,
          ModelID = ModelID,
          ModelObject = NULL,
          model_path = model_path,
          H2OStartUp = TRUE,
          H2OShutdown = TRUE,
          Threads = Threads,
          MaxMemory = MaxMemory)
        DataList[[temp_test]] <- TestData; rm(TestData); gc()
        shiny::incProgress(3/3, detail = 'Test Data is complete')
      } else {
        shiny::incProgress(3/3, detail = 'Test Data is NULL')
      }

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Score test data\n",
        "if(!is.null(ValidationData)) {\n  ",
        "TestData <- RemixAutoML::AutoWord2VecScoring(\n    ",
        "data = TestData,\n    ",
        "BuildType = BuildType,\n    ",
        "stringCol = stringCol,\n    ",
        "KeepStringCol = TRUE,\n    ",
        "ModelID = ModelID,\n    ",
        "ModelObject = NULL,\n    ",
        "model_path = model_path,\n    ",
        "H2OStartUp = TRUE,\n    ",
        "H2OShutdown = TRUE,\n    ",
        "Threads = Threads,\n    ",
        "MaxMemory = MaxMemory)\n  ",
        "DataList[[temp_test]] <- TestData; rm(TestData); gc()\n",
        "}\n"
      ))

      # Finalize
      output$FE_DisplayData <- DT::renderDataTable({RemixAutoML::DataTable(DataList[[temp_train]][seq_len(min(.N, 1000L))])})

      # Return
      if(Debug) {
        print('FE Word2Vec_H2O 5')
        print(names(DataList))
        print(CodeList)
      }

      return(list(
        DataList = DataList,
        CodeList = CodeList
      ))

    })

  } else {
    shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Text Columns is NULL or TrainData is NULL. Check to see if those inputs were filled out.', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  }
}

#' @title Shiny.FE.DimReduction.AutoEncoder.H2O
#'
#' @description server.R observeEvent() for executing AutoEncoder_H2O() for given inputs from app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.DimReduction.AutoEncoder.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  print('FE AutoEncoder H2O')

  # Initialize List
  if(!exists('ArgsList')) ArgsList <- list()

  # Features
  Features <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['AutoEncoder_H2O_TrainData']])
  temp_train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  TrainData <- DataList[[temp_train]]

  # ValidationData
  temp_validate <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL

  # TestData
  temp_test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['AutoEncoder_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
  if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL


  # Create code
  if(Debug) print('FFE Word2Vec H2O 3')
  CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE AutoEncoder H2O: Data\n",
    "Features <- ", RemixAutoML:::ExpandText(Features),"\n",
    "temp_train <- ", RemixAutoML:::CEP(temp_train),"\n",
    "TrainData <- DataList[[temp_train]]\n",
    "temp_validate <- ", RemixAutoML:::CEP(temp_validate),"\n",
    "if(length(temp_validate) > 0L) ValidationData <- DataList[[temp_validate]] else ValidationData <- NULL\n",
    "temp_test <- ", RemixAutoML:::CEP(temp_test),"\n",
    "if(length(temp_test) > 0L) TestData <- DataList[[temp_test]] else TestData <- NULL\n"
  ))


  # Build
  if(length(Features) > 0L && length(TrainData) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'AutoEncoder H2O has begun..', value = 0, {

      if(Debug) print('FE AutoEncoder H2O 2')

      # Non-Data Args
      AnomalyDetection <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_AnomalyDetection}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      DimensionReduction <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_DimensionReduction}, error = function(x) NULL), Type = 'logical', Default = TRUE)
      per_feature <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_per_feature}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      RemoveFeatures <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_RemoveFeatures}, error = function(x) NULL), Type = 'logical', Default = FALSE)
      LayerStructure <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_LayerStructure}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      NodeShrinkRate <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_NodeShrinkRate}, error = function(x) NULL), Type = 'numeric', Default = sqrt(5)/2-0.5)
      ReturnLayer <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ReturnLayer}, error = function(x) NULL), Type = 'numeric', Default = 2L)
      Epochs <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_Epochs}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      L2 <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_L2}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveraging <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveraging}, error = function(x) NULL), Type = 'logical', Default = NULL)
      ElasticAveragingMovingRate <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingMovingRate}, error = function(x) NULL), Type = 'numeric', Default = NULL)
      ElasticAveragingRegularization <- RemixAutoML:::ReturnParam(xx = tryCatch({input$AutoEncoder_H2O_ElasticAveragingRegularization}, error = function(x) NULL), Type = 'numeric', Default = NULL)

      # Create Layer Structure
      LS <- c()
      Nodes <- length(Features)
      for(i in seq_len(LayerStructure)) LS <- c(LS, floor(Nodes * NodeShrinkRate ^ i))
      LayerStructure <- LS

      # Args tracking
      ModelID <- 'temp'
      Models_Path <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('FE AutoEncoder H2O 3')
      zz <- "awk "
      zzz <- "'/MemFree/ {print $2}' "
      zzzz <- "/proc/meminfo"
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# AutoEncoder H2O Args\n",
        "AnomalyDetection <- ", RemixAutoML:::CEP(AnomalyDetection),"\n",
        "DimensionReduction <- ", RemixAutoML:::CEP(DimensionReduction),"\n",
        "per_feature <- ", RemixAutoML:::CEP(per_feature), "\n",
        "RemoveFeatures <- ", RemixAutoML:::CEP(RemoveFeatures),"\n",
        "LayerStructure <- ", RemixAutoML:::CEP(LayerStructure), "\n",
        "NodeShrinkRate <- ", RemixAutoML:::CEP(NodeShrinkRate), "\n",
        "ReturnLayer <- ", RemixAutoML:::CEP(ReturnLayer), "\n",
        "Epochs <- ", RemixAutoML:::CEP(Epochs), "\n",
        "L2 <- ", RemixAutoML:::CEP(L2), "\n",
        "ElasticAveraging <- ", RemixAutoML:::CEP(ElasticAveraging), "\n",
        "ElasticAveragingMovingRate <- ", RemixAutoML:::CEP(ElasticAveragingMovingRate), "\n",
        "ElasticAveragingRegularization <- ", RemixAutoML:::CEP(ElasticAveragingRegularization), "\n",
        "ModelID <- ", RemixAutoML:::CEP(ModelID), "\n",
        "Models_Path <- ", RemixAutoML:::CEP(Models_Path), "\n",
        "NThreads <- ", RemixAutoML:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- paste0(as.character(floor(as.numeric(system(", shQuote(paste0(zz,zzz,zzzz)), "intern=TRUE)) / 1000000)),'G')}\n"
      ))

      # Run function
      if(Debug) print('FE AutoEncoder H2O 3')
      TrainData <- RemixAutoML::ModelDataPrep(data=TrainData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(ValidationData) > 0L) ValidationData <- RemixAutoML::ModelDataPrep(data=ValidationData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(TestData) > 0L) TestData <- RemixAutoML::ModelDataPrep(data=TestData, Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# TrainData: Convert character columns to factors\n",
        "TrainData <- RemixAutoML::ModelDataPrep(\n  ",
        "data = TrainData,\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"
      ))

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# ValidationData: Convert character columns to factors\n",
        "if(length(ValidationData) > 0L) {\n  ",
        "ValidationData <- RemixAutoML::ModelDataPrep(\n    ",
        "data = ValidationData,\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# TestData: Convert character columns to factors\n",
        "if(length(TestData) > 0L) {\n  ",
        "TestData <- RemixAutoML::ModelDataPrep(\n    ",
        "data = TestData,\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      if(Debug) print('AE ::: 1')

      # Metadata Args
      if(length(ModelID) == 0L) ModelID <- 'temp1'
      if(length(model_path) == 0L) model_path <- getwd()

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Metadata Args\n",
        "if(length(ModelID) == 0L) ModelID <- 'temp1'\n",
        "if(length(model_path) == 0L) model_path <- getwd()\n"
      ))

      if(Debug) print('AE ::: 2')

      # Metadata
      Start <- Sys.time()
      tempnames <- names(data.table::copy(TrainData))

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Metadata\n",
        "Start <- Sys.time()\n",
        "tempnames <- names(data.table::copy(TrainData))\n"
      ))

      # Run function
      TrainData <- RemixAutoML::H2OAutoencoder(

        # Select the service
        AnomalyDetection = AnomalyDetection,
        DimensionReduction = DimensionReduction,

        # Data related args
        data = TrainData,
        Features = Features,
        per_feature = per_feature,
        RemoveFeatures = RemoveFeatures,
        ModelID = ModelID,
        model_path = Models_Path,

        # H2O Environment
        NThreads = NThreads,
        MaxMem = MaxMem,
        H2OStart = TRUE,
        H2OShutdown = TRUE,

        # H2O ML Args
        LayerStructure = LayerStructure,
        NodeShrinkRate = NodeShrinkRate,
        ReturnLayer = ReturnLayer,
        Activation = "Tanh",
        Epochs = Epochs,
        L2 = L2,
        ElasticAveraging = ElasticAveraging,
        ElasticAveragingMovingRate = ElasticAveragingMovingRate,
        ElasticAveragingRegularization = ElasticAveragingRegularization)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Run function\n",
        "TrainData <- RemixAutoML::H2OAutoencoder(\n",
        "\n  ",
        "# Select services\n  ",
        "AnomalyDetection = AnomalyDetection,\n  ",
        "DimensionReduction = DimensionReduction,\n",
        "n\  ",
        "# Data related args\n  ",
        "data = TrainData,\n  ",
        "Features = Features,\n  ",
        "per_feature = per_feature,\n  ",
        "RemoveFeatures = RemoveFeatures,\n  ",
        "ModelID = ModelID,\n  ",
        "model_path = Models_Path,\n",
        "\n  ",
        "# H2O Environment\n  ",
        "NThreads = NThreads,\n  ",
        "MaxMem = MaxMem,\n  ",
        "H2OStart = TRUE,\n  ",
        "H2OShutdown = TRUE,\n",
        "\n  ",
        "# H2O ML Args\n  ",
        "LayerStructure = LayerStructure,\n  ",
        "NodeShrinkRate = NodeShrinkRate,\n  ",
        "ReturnLayer = ReturnLayer,\n  ",
        "Activation = 'Tanh',\n  ",
        "Epochs = Epochs,\n  ",
        "L2 = L2,\n  ",
        "ElasticAveraging = ElasticAveraging,\n  ",
        "ElasticAveragingMovingRate = ElasticAveragingMovingRate,\n  ",
        "ElasticAveragingRegularization = ElasticAveragingRegularization)\n"
      ))

      # Store Data
      DataList[[temp_train]] <- TrainData

      if(Debug) print('AE ::: 3')

      # New columns tracking
      NewColumns <- setdiff(names(data.table::copy(TrainData)), tempnames)
      rm(TrainData); gc()

      # Increment the progress bar, and update the detail text.
      shiny::incProgress(1/3, detail = 'Train Data is complete. Moving on to Validation Data')

      if(Debug) print('AE ::: 4')

      # Run time tracking
      End <- Sys.time()
      RunTime_Training <- difftime(End, Start, units = "mins")

      if(Debug) print('AE ::: 5')

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Store Data\n",
        "DataList[[temp_train]] <- TrainData\n",
        "NewColumns <- setdiff(names(data.table::copy(TrainData)), tempnames)\n",
        "rm(TrainData); gc()\n\n",
        "# Run time tracking\n",
        "End <- Sys.time()\n",
        "RunTime_Training <- difftime(End, Start, units = 'mins')\n"
      ))

      # Score validation Data
      if(length(ValidationData) > 0L) {

        if(Debug) print('AE ::: 6')

        # Pause
        Sys.sleep(8L)

        # Score model
        ValidationData <- RemixAutoML::H2OAutoencoderScoring(

          # Select the service
          AnomalyDetection = AnomalyDetection,
          DimensionReduction = DimensionReduction,

          # Data related args
          data = ValidationData,
          Features = Features,
          per_feature = per_feature,
          RemoveFeatures = RemoveFeatures,
          ModelObject = NULL,
          ModelID = ModelID,
          model_path = model_path,

          # H2O args
          NThreads = NThreads,
          MaxMem = MaxMem,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ReturnLayer = ReturnLayer)

        DataList[[temp_validate]] <- ValidationData
        rm(ValidationData); gc()

        shiny::incProgress(2/3, detail = 'Validation data is complete. Moving on to Test Data')

      } else {
        shiny::incProgress(2/3, detail = 'Validation Data not supplied')
      }

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Score Validation Data\n",
        "if(length(ValidationData) > 0L) {\n  ",
        "Sys.sleep(8L)\n\n  ",
        "# Score model\n  ",
        "ValidationData <- RemixAutoML::H2OAutoencoderScoring(\n\n    ",
        "# Select the service\n    ",
        "AnomalyDetection = AnomalyDetection,\n    ",
        "DimensionReduction = DimensionReduction,\n\n    ",
        "# Data related args\n    ",
        "data = ValidationData,\n    ",
        "Features = Features,\n    ",
        "per_feature = per_feature,\n    ",
        "RemoveFeatures = RemoveFeatures,\n    ",
        "ModelObject = NULL,\n    ",
        "ModelID = ModelID,\n    ",
        "model_path = model_path,\n\n    ",
        "# H2O args\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "H2OStart = TRUE,\n    ",
        "H2OShutdown = TRUE,\n    ",
        "ReturnLayer = ReturnLayer)\n  ",
        "DataList[[temp_validate]] <- ValidationData\n  ",
        "rm(ValidationData); gc()\n"
      ))

      # Score Test Data
      if(length(TestData) > 0L) {

        if(Debug) print('AE ::: 7')

        # Pause
        Sys.sleep(8L)

        # Score model
        TestData <- RemixAutoML::H2OAutoencoderScoring(

          # Select the service
          AnomalyDetection = AnomalyDetection,
          DimensionReduction = DimensionReduction,

          # Data related args
          data = TestData,
          Features = Features,
          per_feature = per_feature,
          RemoveFeatures = RemoveFeatures,
          ModelObject = NULL,
          ModelID = ModelID,
          model_path = model_path,

          # H2O args
          NThreads = NThreads,
          MaxMem = MaxMem,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ReturnLayer = ReturnLayer)

        # Store Data
        DataList[[temp_test]] <- TestData
        rm(TestData); gc()
        shiny::incProgress(3/3, detail = 'Test data is complete.')
      } else {
        shiny::incProgress(3/3, detail = 'Test Data not supplied')
      }

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Score Test Data\n",
        "if(length(TestData) > 0L) {\n  ",
        "Sys.sleep(8L)\n\n  ",
        "# Score model\n  ",
        "TestData <- RemixAutoML::H2OAutoencoderScoring(\n\n    ",
        "# Select the service\n    ",
        "AnomalyDetection = AnomalyDetection,\n    ",
        "DimensionReduction = DimensionReduction,\n\n    ",
        "# Data related args\n    ",
        "data = TestData,\n    ",
        "Features = Features,\n    ",
        "per_feature = per_feature,\n    ",
        "RemoveFeatures = RemoveFeatures,\n    ",
        "ModelObject = NULL,\n    ",
        "ModelID = ModelID,\n    ",
        "model_path = model_path,\n\n    ",
        "# H2O args\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "H2OStart = TRUE,\n    ",
        "H2OShutdown = TRUE,\n    ",
        "ReturnLayer = ReturnLayer)\n  ",
        "DataList[[temp_validate]] <- TestData\n  ",
        "rm(TestData); gc()\n"
      ))

      # Collect Output
      if(Debug) print('FE AutoEncoder H2O 4')

      # Finalize
      output$FE_DisplayData <- DT::renderDataTable({RemixAutoML::DataTable(DataList[[temp_train]][seq_len(min(.N, 1000L))])})
    })

    # Return
    if(Debug) {
      print('FE Word2Vec_H2O 5')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  } else {
    shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Text Columns is NULL or TrainData is NULL. Check to see if those inputs were filled out.', type = NULL, btn_labels = 'warning', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  }
}

#' @title Shiny.FE.AnomalyDetection.IsolationForest.H2O
#'
#' @description Utilize an H2O isolation forest to provide anomaly detection
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.AnomalyDetection.IsolationForest.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  print('FE IsolationForest H2O')

  # Initialize List
  if(!exists('ArgsList')) ArgsList <- list()

  # Features
  Features <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Features}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['IsolationForest_H2O_TrainData']])
  temp_train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['IsolationForest_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)


  # Create code
  if(Debug) print('FE IsolationForest H2O 3')
  CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE IsolationForest H2O: Data\n",
    "Features <- ", RemixAutoML:::ExpandText(Features),"\n",
    "temp_train <- ", RemixAutoML:::CEP(temp_train),"\n",
    "temp_validate <- ", RemixAutoML:::CEP(temp_validate),"\n",
    "temp_test <- ", RemixAutoML:::CEP(temp_test),"\n"
  ))

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    # AutoEncoder_H2O
    shiny::withProgress(message = 'IsolationForest H2O has begun..', value = 0, {

      if(Debug) print('FE IsolationForest H2O 2')
      IDcols <- setdiff(names(DataList[[temp_train]]), Features)
      Threshold <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_Threshold}, error = function(x) NULL), Type = 'numeric', Default = 0.95)
      NTrees <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_NTrees}, error = function(x) NULL), Type = 'numeric', Default = 50)
      MaxDepth <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MaxDepth}, error = function(x) NULL), Type = 'numeric', Default = 20)
      MinRows <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_MinRows}, error = function(x) NULL), Type = 'numeric', Default = 1)
      RowSampleRate <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_RowSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRate <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRate}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerLevel <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerLevel}, error = function(x) NULL), Type = 'numeric', Default = 1)
      ColSampleRatePerTree <- RemixAutoML:::ReturnParam(xx = tryCatch({input$IsolationForest_H2O_ColSampleRatePerTree}, error = function(x) NULL), Type = 'numeric', Default = 1)
      CategoricalEncoding <- 'AUTO'

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      # Create code
      if(Debug) print('FE IsolationForest H2O 3')
      zz <- "awk "
      zzz <- "'/MemFree/ {print $2}' "
      zzzz <- "/proc/meminfo"
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# IsolationForest H2O Args\n",
        "IDcols <- ", RemixAutoML:::CEP(IDcols),"\n",
        "ModelID <- ", RemixAutoML:::CEP(ModelID),"\n",
        "Threshold <- ", RemixAutoML:::CEP(Threshold), "\n",
        "NTrees <- ", RemixAutoML:::CEP(NTrees),"\n",
        "MaxDepth <- ", RemixAutoML:::CEP(MaxDepth), "\n",
        "MinRows <- ", RemixAutoML:::CEP(MinRows), "\n",
        "RowSampleRate <- ", RemixAutoML:::CEP(RowSampleRate), "\n",
        "ColSampleRate <- ", RemixAutoML:::CEP(ColSampleRate), "\n",
        "ColSampleRatePerLevel <- ", RemixAutoML:::CEP(ColSampleRatePerLevel), "\n",
        "ColSampleRatePerTree <- ", RemixAutoML:::CEP(ColSampleRatePerTree), "\n",
        "CategoricalEncoding <- ", RemixAutoML:::CEP(CategoricalEncoding), "\n",
        "NThreads <- ", RemixAutoML:::CEP(NThreads), "\n",
        "gc()\n",
        "MaxMem <- paste0(as.character(floor(as.numeric(system(", shQuote(paste0(zz,zzz,zzzz)), "intern=TRUE)) / 1000000)),'G')}\n"
      ))

      # Run function
      if(Debug) print('FE IsolationForest H2O 3')
      DataList[[temp_train]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_train]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_validate]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_test]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "DataList[[temp_train]] <- RemixAutoML::ModelDataPrep(\n  ",
        "data = DataList[[temp_train]],\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"
      ))

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_validate]]: Convert character columns to factors\n",
        "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
        "DataList[[temp_validate]] <- RemixAutoML::ModelDataPrep(\n    ",
        "data = DataList[[temp_validate]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_test]]: Convert character columns to factors\n",
        "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
        "DataList[[temp_test]] <- RemixAutoML::ModelDataPrep(\n    ",
        "data = DataList[[temp_test]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      if(Debug) print('AE ::: 1')

      # Run function
      DataList[[temp_train]] <- RemixAutoML::H2OIsolationForest(
        data = DataList[[temp_train]],
        Features = Features,
        IDcols = IDcols,
        ModelID = ModelID,
        SavePath = SavePath,
        NThreads = NThreads,
        MaxMem = MaxMem,
        Threshold = Threshold,
        NTrees = NTrees,
        MaxDepth = MaxDepth,
        MinRows = MinRows,
        RowSampleRate = RowSampleRate,
        ColSampleRate = ColSampleRate,
        ColSampleRatePerLevel = ColSampleRatePerLevel,
        ColSampleRatePerTree = ColSampleRatePerTree,
        CategoricalEncoding = CategoricalEncoding,
        Debug = Debug)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "if(length(DataList[[temp_train]]) > 0L) {\n  ",
        "DataList[[temp_train]] <- RemixAutoML::H2OIsolationForest(\n    ",
        "data = DataList[[temp_train]],\n    ",
        "Features = Features,\n    ",
        "IDcols = IDcols,\n    ",
        "ModelID = ModelID,\n    ",
        "SavePath = SavePath,\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "Threshold = Threshold,\n    ",
        "NTrees = NTrees,\n    ",
        "MaxDepth = MaxDepth',\n    ",
        "MinRows = MinRows',\n    ",
        "RowSampleRate = RowSampleRate',\n    ",
        "ColSampleRate = ColSampleRate',\n    ",
        "ColSampleRatePerLevel = ColSampleRatePerLevel',\n    ",
        "ColSampleRatePerTree = ColSampleRatePerTree',\n    ",
        "CategoricalEncoding = CategoricalEncoding',\n    ",
        "Debug = Debug)\n",
        "}\n"
      ))

      # Updates
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      if(Debug) print('AE ::: 2')

      # Score validation data
      if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_validate]] <- RemixAutoML::H2OIsolationForestScoring(
          data = DataList[[temp_validate]],
          Features = Features,
          IDcols = IDcols,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ModelID = ModelID,
          SavePath = SavePath,
          Threshold = Threshold,
          MaxMem = MaxMem,
          NThreads = NThreads,
          Debug = Debug)

        # Code
        CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
          "DataList[[temp_validate]] <- RemixAutoML::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_validate]],\n    ",
          "Features = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem)\n",
          "}\n"
        ))

        # Updates
        shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')
      }

      # Score Test Data
      if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_test]] <- RemixAutoML::H2OIsolationForestScoring(
          data = DataList[[temp_test]],
          Features = Features,
          IDcols = IDcols,
          H2OStart = TRUE,
          H2OShutdown = TRUE,
          ModelID = ModelID,
          SavePath = SavePath,
          Threshold = Threshold,
          MaxMem = MaxMem,
          NThreads = NThreads,
          Debug = Debug)

        # Code
        CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
          "DataList[[temp_test]] <- RemixAutoML::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_test]],\n    ",
          "Features = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "H2OStart = TRUE,\n    ",
          "H2OShutdown = TRUE,\n    ",
          "ModelID = ModelID,\n    ",
          "ModelObject = NULL,\n    ",
          "model_path = model_path,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMem = MaxMem)\n",
          "}\n"
        ))
      }
    })

    # Return
    if(Debug) {
      print('FE IsolationForest H2O 6')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  }
}

#' @title Shiny.FE.AnomalyDetection.IsolationForest.H2O
#'
#' @description Utilize an H2O isolation forest to provide anomaly detection
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param DataList DataList contains the data sets in session
#' @param CodeList From app
#' @param CacheDir From app
#' @param CacheName 'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.
#' @param Debug Debug from app
#'
#' @keywords internal
Shiny.FE.Clustering.Kmeans.H2O <- function(input,output,session,DataList,CodeList,CacheDir=NULL,CacheName='data',Debug=FALSE) {

  print('FE Kmeans H2O')

  # Features
  Features <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_Features']]}, error = function(x) NULL), Type = 'character', Default = NULL)

  # Data
  if(Debug) print(input[['Kmeans_H2O_TrainData']])
  temp_train <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TrainData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # ValidationData
  temp_validate <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ValidationData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # TestData
  temp_test <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_TestData']]}, error=function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

  # Create code
  if(Debug) {
    print('FE Kmeans H2O 3')
    print(temp_train)
    print(Features)
  }
  CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# FE Kmeans H2O: Data\n",
    "Features <- ", RemixAutoML:::ExpandText(Features),"\n",
    "temp_train <- ", RemixAutoML:::CEP(temp_train),"\n",
    "temp_validate <- ", RemixAutoML:::CEP(temp_validate),"\n",
    "temp_test <- ", RemixAutoML:::CEP(temp_test),"\n"
  ))

  if(Debug) print('FE Kmeans H2O 4')

  # Build
  if(length(Features) > 0L && length(temp_train) > 0L) {

    if(Debug) print('FE Kmeans H2O 5')

    # AutoEncoder_H2O
    shiny::withProgress(message = 'Kmeans H2O has begun..', value = 0, {

      if(Debug) print('FE Kmeans H2O 6')

      MaxClusters <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_MaxClusters']]}, error = function(x) NULL), Type = 'numeric', Default = 50)
      ClusterMetric <- RemixAutoML:::ReturnParam(xx = tryCatch({input[['Kmeans_H2O_ClusterMetric']]}, error = function(x) NULL), Type = 'character', Default = 0.95)

      if(Debug) print('FE Kmeans H2O 7')

      # Args tracking
      ModelID <- 'temp'
      SavePath <- getwd()
      NThreads <- max(1L, parallel::detectCores()-2L)
      MaxMem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")}

      if(Debug) print('FE Kmeans H2O 8')

      # Create code
      if(Debug) print('FE Kmeans H2O 3')
      zz <- "awk "
      zzz <- "'/MemFree/ {print $2}' "
      zzzz <- "/proc/meminfo"
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Kmeans H2O Args\n",
        "ModelID <- ", RemixAutoML:::CEP(ModelID),"\n",
        "NThreads <- ", RemixAutoML:::CEP(NThreads), "\n",
        "MaxClusters <- ", RemixAutoML:::CEP(MaxClusters), "\n",
        "ClusterMetric <- ", RemixAutoML:::CEP(ClusterMetric), "\n",
        "gc()\n",
        "MaxMem <- paste0(as.character(floor(as.numeric(system(", shQuote(paste0(zz,zzz,zzzz)), "intern=TRUE)) / 1000000)),'G')}\n"
      ))

      # Run function
      if(Debug) print('FE Kmeans H2O 9')
      DataList[[temp_train]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_train]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_validate) > 0L) DataList[[temp_validate]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_validate]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)
      if(length(temp_test) > 0L) DataList[[temp_test]] <- RemixAutoML::ModelDataPrep(data=DataList[[temp_test]], Impute=TRUE, CharToFactor=TRUE, FactorToChar=FALSE, IntToNumeric=TRUE, LogicalToBinary=TRUE, DateToChar=TRUE, IDateConversion=FALSE, RemoveDates=FALSE, MissFactor='missing', MissNum=-1, IgnoreCols=NULL)

      if(Debug) print('FE Kmeans H2O 10')

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "DataList[[temp_train]] <- RemixAutoML::ModelDataPrep(\n  ",
        "data = DataList[[temp_train]],\n  ",
        "Impute = TRUE,\n  ",
        "CharToFactor = TRUE,\n  ",
        "FactorToChar = FALSE,\n  ",
        "IntToNumeric = TRUE,\n  ",
        "LogicalToBinary = TRUE,\n  ",
        "DateToChar = FALSE,\n  ",
        "IDateConversion = FALSE,\n  ",
        "RemoveDates = FALSE,\n  ",
        "MissFactor = 'missing',\n  ",
        "MissNum = -1,\n  ",
        "IgnoreCols=NULL)\n"
      ))

      if(Debug) print('FE Kmeans H2O 11')

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_validate]]: Convert character columns to factors\n",
        "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
        "DataList[[temp_validate]] <- RemixAutoML::ModelDataPrep(\n    ",
        "data = DataList[[temp_validate]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      if(Debug) print('FE Kmeans H2O 12')

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_test]]: Convert character columns to factors\n",
        "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
        "DataList[[temp_test]] <- RemixAutoML::ModelDataPrep(\n    ",
        "data = DataList[[temp_test]],\n    ",
        "Impute = TRUE,\n    ",
        "CharToFactor = TRUE,\n    ",
        "FactorToChar = FALSE,\n    ",
        "IntToNumeric = TRUE,\n    ",
        "LogicalToBinary = TRUE,\n    ",
        "DateToChar = FALSE,\n    ",
        "IDateConversion = FALSE,\n    ",
        "RemoveDates = FALSE,\n    ",
        "MissFactor = 'missing',\n    ",
        "MissNum = -1,\n    ",
        "IgnoreCols=NULL)\n  ",
        "}\n"
      ))

      if(Debug) print('Kmeans ::: 13')

      # Run function
      DataList[[temp_train]] <- RemixAutoML::AutoClustering(
        data = DataList[[temp_train]],
        FeatureColumns = Features,
        ModelID = 'temp_kmeans',
        SavePath = SavePath,
        NThreads = max(1L, parallel::detectCores()-2L),
        MaxMem = MaxMem,
        MaxClusters = MaxClusters,
        ClusterMetric = ClusterMetric,
        RunDimReduction = FALSE, ShrinkRate = NULL, Epochs = NULL, L2_Reg = NULL,
        ElasticAveraging = NULL, ElasticAveragingMovingRate = NULL, ElasticAveragingRegularization = NULL)

      # Code
      CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# DataList[[temp_train]]: Convert character columns to factors\n",
        "if(length(DataList[[temp_train]]) > 0L) {\n  ",
        "DataList[[temp_train]] <- RemixAutoML::AutoClustering(\n    ",
        "data = DataList[[temp_train]],\n    ",
        "FeatureColumns = Features,\n    ",
        "ModelID = 'temp_kmeans',\n    ",
        "SavePath = SavePath,\n    ",
        "NThreads = NThreads,\n    ",
        "MaxMem = MaxMem,\n    ",
        "MaxClusters = MaxClusters,\n    ",
        "ClusterMetric = ClusterMetric,\n    ",
        "RunDimReduction = FALSE)\n",
        "}\n"
      ))

      if(Debug) print('FE Kmeans H2O 14')

      # Updates
      shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')

      if(Debug) print('FE Kmeans H2O 15')

      # Score validation data
      if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {

        if(Debug) print('FE Kmeans H2O 16')

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_validate]] <- RemixAutoML::AutoClusteringScoring(
          data = DataList[[temp_validate]],
          FeatureColumns = Features,
          ModelID = ModelID,
          SavePath = SavePath,
          NThreads = NThreads,
          MaxMemory = MaxMem,
          DimReduction = FALSE)

        if(Debug) print('FE Kmeans H2O 17')

        # Code
        CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_validate) > 0L && length(DataList[[temp_validate]]) > 0L) {\n  ",
          "DataList[[temp_validate]] <- RemixAutoML::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_validate]],\n    ",
          "FeatureColumns = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "ModelID = 'temp_kmeans',\n    ",
          "ModelObject = NULL,\n    ",
          "SavePath = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMemory = MaxMem)\n",
          "}\n"
        ))

        # Updates
        shiny::incProgress(1/3, detail = 'Train Data is complete. Validation Data is next up')
      }

      if(Debug) print('FE Kmeans H2O 18')

      # Score Test Data
      if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {

        if(Debug) print('FE Kmeans H2O 19')

        # Pause
        Sys.sleep(10L)

        # Score model
        DataList[[temp_test]] <- RemixAutoML::AutoClusteringScoring(
          data = DataList[[temp_test]],
          FeatureColumns = Features,
          ModelID = ModelID,
          SavePath = SavePath,
          NThreads = NThreads,
          MaxMemory = MaxMem,
          DimReduction = FALSE)

        if(Debug) print('FE Kmeans H2O 20')

        # Code
        CodeList <- RemixAutoML:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Score test data\n",
          "if(length(temp_test) > 0L && length(DataList[[temp_test]]) > 0L) {\n  ",
          "DataList[[temp_test]] <- RemixAutoML::H2OIsolationForestScoring(\n    ",
          "data = DataList[[temp_test]],\n    ",
          "FeatureColumns = Features,\n    ",
          "IDcols = IDcols,\n    ",
          "ModelID = 'temp_kmeans',\n    ",
          "ModelObject = NULL,\n    ",
          "SavePath = SavePath,\n    ",
          "NThreads = NThreads,\n    ",
          "MaxMemory = MaxMem)\n",
          "}\n"
        ))
      }
    })

    # Return
    if(Debug) {
      print('FE Kmeans H2O 6')
      print(names(DataList))
      print(CodeList)
    }

    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  }
}

