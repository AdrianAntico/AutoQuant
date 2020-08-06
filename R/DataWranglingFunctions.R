#' ColumnSubsetDataTable
#'
#' ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#' @author Adrian Antico
#' @param data data.table
#' @param Target Target variable
#' @param Date Date variable
#' @param GroupVars Group variables
#' @export
ColumnSubsetDataTable <- function(data,
                                  TargetColumnName = NULL,
                                  DateColumnName = NULL,
                                  GroupVars = NULL) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }

  # Subset----
  data <- data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVars))]

  # Ensure Date Column is Date----
  if(is.character(data[[eval(DateColumnName)]])) {
    x <- data[1,get(DateColumnName)]
    x1 <- lubridate::guess_formats(x, orders = c("mdY","BdY","Bdy","bdY","bdy","mdy","dby","Ymd","Ydm"))
    data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
  }

  # Return data----
  return(data)
}

#' DataDisplayMeta
#'
#' DataDisplayMeta
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param data Source data
#' @export
DataDisplayMeta <- function(data) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) return(NULL)

  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, i = Counter, j = "Variable", value = eval(name))
    data.table::set(x, i = Counter, j = "DataType", value = class(data[[eval(name)]]))
  }

  # Return table
  return(x[Variable != "donotuse"])
}

#' TimeSeriesMelt
#'
#' TimeSeriesMelt
#'
#' @family Data Wrangling
#' @author Adrian Antico
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
#' @export
TimeSeriesMelt <- function(data,
                           TargetVariable = NULL,
                           DateVariable = NULL,
                           GroupVariables = NULL) {

  # 2 Cases:
  #  Multiple Targets + Grouping Variables
  #  Multiple Targets + No Grouping Variables
  if(length(TargetVariable) > 1) {
    if(!is.null(GroupVariables)) {
      data <- data.table::melt(
        data = data,
        id.vars = c(eval(DateVariable),eval(GroupVariables)),
        measure.vars = eval(TargetVariable),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    } else {
      data <- data.table::melt(
        data = data,
        id.vars = eval(DateVariable),
        measure.vars = c(eval(TargetVariable)),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    }
  }

  # Return
  return(data)
}

#' DifferenceData
#'
#' DifferenceData differences your data set
#' @family Time Series
#' @author Adrian Antico
#' @param data Source data
#' @param ColumnsToDiff The column numbers you want differenced
#' @param CARMA Set to TRUE for CARMA functions
#' @param TargetVariable The target variable name
#' @param GroupingVariable Difference data by group
#' @export
DifferenceData <- function(data,
                           ColumnsToDiff = c(names(data)[2:ncol(data)]),
                           CARMA = FALSE,
                           TargetVariable = NULL,
                           GroupingVariable = NULL) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Keep First Row of Data
  if(!is.null(GroupingVariable)) {
    FirstRow <- data[data[, .I[1], get(GroupingVariable)]$V1]
  } else {
    FirstRow <- data[1,]
  }

  # Keep Last Row of Target Variable----
  if(!is.null(GroupingVariable)) {
    LastRow <- data[data[, .I[.N], get(GroupingVariable)]]$V1
  } else {
    LastRow <- data[data[, .I[.N]]]
  }

  # Diff data
  if(!is.null(GroupingVariable)) {
    DiffData <- cbind(data[1:(data[, .I[.N-1], get(GroupingVariable)]$V1),1],data[, lapply(.SD,diff), by = eval(GroupingVariable), .SDcols = ColumnsToDiff])
  } else {
    DiffData <- cbind(data[1:(nrow(data)-1),1],data[, lapply(.SD,diff), .SDcols = ColumnsToDiff])
  }

  # Return data
  if(!CARMA) {
    return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = data[nrow(data),]))
  } else {
    if(!is.null(GroupingVariable)) {
      FirstRow <- FirstRow[, get(TargetVariable), by = eval(GroupingVariable)]
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    } else {

      #FirstRow <- FirstRow[, get(TargetVariable)]
      return(list(DiffData = DiffData, FirstRow = FirstRow, LastRow = LastRow))
    }
  }
}

#' DifferenceDataReverse
#'
#' DifferenceDataReverse reverses the difference
#' @family Time Series
#' @author Adrian Antico
#' @param data Pre differenced scoring data
#' @param ScoreData Predicted values from ML model
#' @param LastRow The last row from training data target variables
#' @param TargetCol Target column name
#' @param CARMA Set to TRUE for CARMA utilization
#' @param FirstRow The first row of the target variable
#' @param GroupingVariables Group columns
#' @export
DifferenceDataReverse <- function(data,
                                  ScoreData = Forecasts$Predictions,
                                  LastRow = DiffTrainOutput$LastRow$Weekly_Sales,
                                  CARMA = FALSE,
                                  TargetCol = TargetColumnName,
                                  FirstRow = DiffTrainOutput$FirstRow,
                                  GroupingVariables = NULL) {

  # Turn on full speed ahead----
  data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L))

  # Ensure data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  ModifiedData <- data.table::copy(data)
  if(!CARMA) {
    if(!is.null(GroupingVariables)) {
      ""
    } else {
      return(ModifiedData[, Predictions := cumsum(c(LastRow,ScoreData))])
    }
  } else {
    if(!is.null(GroupingVariables)) {
      ""
    } else {
      x <- cumsum(c(FirstRow,ModifiedData[[eval(TargetCol)]]))
      xx <- x[-length(x)]
      return(ModifiedData[, eval(TargetCol) := xx][, Predictions := xx])
    }
  }
}

#' FullFactorialCatFeatures
#'
#' FullFactorialCatFeatures reverses the difference
#' @family Data Wrangling
#' @author Adrian Antico
#' @param GroupVars Character vector of categorical columns to fully interact
#' @param BottomsUp TRUE or FALSE. TRUE starts with the most comlex interaction to the main effects
#' @export
FullFactorialCatFeatures <- function(GroupVars = GroupVariables,
                                     BottomsUp = TRUE) {

  N <- length(GroupVars)
  Categoricals <- c()

  # Binomial Expansion
  for(i in 1:N) {

    # Case 1: N choose 1 - Store each individual column name separately (main effects)
    if(i == 1) {
      for(j in 1:N) Categoricals <- c(Categoricals,GroupVars[j])

    # Case 2: N choose 2 up to N choose N-1: Middle-Hierarchy Interactions
    } else if(i < N) {
      temp <- combinat::combn(GroupVars, m = i)
      temp2 <- c()
      for(k in 1:ncol(temp)) {
        for(l in 1:nrow(temp)) {
          if(l == 1) {
            temp2 <- temp[l,k]
          } else {
            temp2 <- paste(temp2,temp[l,k], sep = "_")
          }
        }
        Categoricals <- c(Categoricals, temp2)
      }

    # Case 3: N choose N - Full Interaction
    } else {
      temp <- combinat::combn(GroupVars, m = i)
      for(m in 1:N) {
        if(m == 1) {
          temp2 <- temp[m]
        } else {
          temp2 <- paste(temp2,temp[m], sep = "_")
        }
      }
      Categoricals <- c(Categoricals, temp2)
    }
  }

  # Order of output----
  if(BottomsUp) {
    return(rev(Categoricals))
  } else {
    return(Categoricals)
  }
}

#' AutoDataDictionaries
#'
#' AutoDataDictionaries is a function to return data dictionary data in table form
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param Type = "sqlserver" is currently the only system supported
#' @param DBConnection This is a RODBC connection object for sql server
#' @param DDType Select from 1 - 6 based on this article
#' @param Query Supply a query
#' @param ASIS Set to TRUE to pull in values without coercing types
#' @param CloseChannel Set to TRUE to disconnect
#' @export
AutoDataDictionaries <- function(Type = "sqlserver",
                                 DBConnection,
                                 DDType = 1L,
                                 Query = NULL,
                                 ASIS = FALSE,
                                 CloseChannel = TRUE) {

  # Ensure DBConnection is proper----
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")

  library(RODBC)

  # Queries----
  if(!is.null(Query)) {
    x <- data.table::as.data.table(RODBC::sqlQuery(DBConnection, Query, as.is = ASIS))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Tables and number of columns----
  if(DDType == 1L) {
    qry <- "select schema_name(tab.schema_id) as schema_name,
       tab.name as table_name,
       tab.create_date as created,
       tab.modify_date as last_modified,
       p.rows as num_rows,
       ep.value as comments
  from sys.tables tab
       inner join (select distinct
                          p.object_id,
                          sum(p.rows) rows
                     from sys.tables t
                          inner join sys.partitions p
                              on p.object_id = t.object_id
                    group by p.object_id,
                          p.index_id) p
            on p.object_id = tab.object_id
        left join sys.extended_properties ep
            on tab.object_id = ep.major_id
           and ep.name = 'MS_Description'
           and ep.minor_id = 0
           and ep.class_desc = 'OBJECT_OR_COLUMN'
  order by schema_name,
        table_name"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Tables and number of columns----
  if(DDType == 2L) {
    qry <- "select schema_name(v.schema_id) as schema_name,
       v.name as view_name,
       v.create_date as created,
       v.modify_date as last_modified,
       m.definition,
       ep.value as comments
  from sys.views v
       left join sys.extended_properties ep
           on v.object_id = ep.major_id
          and ep.name = 'MS_Description'
          and ep.minor_id = 0
          and ep.class_desc = 'OBJECT_OR_COLUMN'
       inner join sys.sql_modules m
           on m.object_id = v.object_id
 order by schema_name,
          view_name"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Tables and number of columns----
  if(DDType == 3L) {
    qry <- "select schema_name(tab.schema_id) as schema_name,
       tab.name as table_name,
       col.name as column_name,
       t.name as data_type,
       t.name +
       case when t.is_user_defined = 0 then
                 isnull('(' +
                 case when t.name in ('binary', 'char', 'nchar',
                           'varchar', 'nvarchar', 'varbinary') then
                           case col.max_length
                                when -1 then 'MAX'
                                else
                                     case when t.name in ('nchar',
                                               'nvarchar') then
                                               cast(col.max_length/2
                                               as varchar(4))
                                          else cast(col.max_length
                                               as varchar(4))
                                     end
                           end
                      when t.name in ('datetime2', 'datetimeoffset',
                           'time') then
                           cast(col.scale as varchar(4))
                      when t.name in ('decimal', 'numeric') then
                            cast(col.precision as varchar(4)) + ', ' +
                            cast(col.scale as varchar(4))
                 end + ')', '')
            else ':' +
                 (select c_t.name +
                         isnull('(' +
                         case when c_t.name in ('binary', 'char',
                                   'nchar', 'varchar', 'nvarchar',
                                   'varbinary') then
                                    case c.max_length
                                         when -1 then 'MAX'
                                         else
                                              case when t.name in
                                                        ('nchar',
                                                        'nvarchar') then
                                                        cast(c.max_length/2
                                                        as varchar(4))
                                                   else cast(c.max_length
                                                        as varchar(4))
                                              end
                                    end
                              when c_t.name in ('datetime2',
                                   'datetimeoffset', 'time') then
                                   cast(c.scale as varchar(4))
                              when c_t.name in ('decimal', 'numeric') then
                                   cast(c.precision as varchar(4)) + ', '
                                   + cast(c.scale as varchar(4))
                         end + ')', '')
                    from sys.columns as c
                         inner join sys.types as c_t
                             on c.system_type_id = c_t.user_type_id
                   where c.object_id = col.object_id
                     and c.column_id = col.column_id
                     and c.user_type_id = col.user_type_id
                 )
        end as data_type_ext,
        case when col.is_nullable = 0 then 'N'
             else 'Y' end as nullable,
        case when def.definition is not null then def.definition
             else '' end as default_value,
        case when pk.column_id is not null then 'PK'
             else '' end as primary_key,
        case when fk.parent_column_id is not null then 'FK'
             else '' end as foreign_key,
        case when uk.column_id is not null then 'UK'
             else '' end as unique_key,
        case when ch.check_const is not null then ch.check_const
             else '' end as check_contraint,
        cc.definition as computed_column_definition,
        ep.value as comments
   from sys.tables as tab
        left join sys.columns as col
            on tab.object_id = col.object_id
        left join sys.types as t
            on col.user_type_id = t.user_type_id
        left join sys.default_constraints as def
            on def.object_id = col.default_object_id
        left join (
                  select index_columns.object_id,
                         index_columns.column_id
                    from sys.index_columns
                         inner join sys.indexes
                             on index_columns.object_id = indexes.object_id
                            and index_columns.index_id = indexes.index_id
                   where indexes.is_primary_key = 1
                  ) as pk
            on col.object_id = pk.object_id
           and col.column_id = pk.column_id
        left join (
                  select fc.parent_column_id,
                         fc.parent_object_id
                    from sys.foreign_keys as f
                         inner join sys.foreign_key_columns as fc
                             on f.object_id = fc.constraint_object_id
                   group by fc.parent_column_id, fc.parent_object_id
                  ) as fk
            on fk.parent_object_id = col.object_id
           and fk.parent_column_id = col.column_id
        left join (
                  select c.parent_column_id,
                         c.parent_object_id,
                         'Check' check_const
                    from sys.check_constraints as c
                   group by c.parent_column_id,
                         c.parent_object_id
                  ) as ch
            on col.column_id = ch.parent_column_id
           and col.object_id = ch.parent_object_id
        left join (
                  select index_columns.object_id,
                         index_columns.column_id
                    from sys.index_columns
                         inner join sys.indexes
                             on indexes.index_id = index_columns.index_id
                            and indexes.object_id = index_columns.object_id
                    where indexes.is_unique_constraint = 1
                    group by index_columns.object_id,
                          index_columns.column_id
                  ) as uk
            on col.column_id = uk.column_id
           and col.object_id = uk.object_id
        left join sys.extended_properties as ep
            on tab.object_id = ep.major_id
           and col.column_id = ep.minor_id
           and ep.name = 'MS_Description'
           and ep.class_desc = 'OBJECT_OR_COLUMN'
        left join sys.computed_columns as cc
            on tab.object_id = cc.object_id
           and col.column_id = cc.column_id
  order by schema_name,
        table_name,
        column_name"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Tables and number of columns----
  if(DDType == 4L) {
    qry <- "SELECT
       schema_name(tab.schema_id) AS table_schema_name,
       tab.name AS table_name,
       col.name AS column_name,
       fk.name AS constraint_name,
       schema_name(tab_prim.schema_id) AS primary_table_schema_name,
       tab_prim.name AS primary_table_name,
       col_prim.name AS primary_table_column,
       schema_name(tab.schema_id) + '.' + tab.name + '.' + col.name + ' = ' + schema_name(tab_prim.schema_id) + '.' + tab_prim.name + '.' + col_prim.name AS join_condition,
       case when count(*) over (partition by fk.name) > 1 then 'Y' else 'N' end AS complex_fk,
       fkc.constraint_column_id AS fk_part
    FROM sys.tables AS tab
       INNER JOIN sys.foreign_keys AS fk
           ON tab.object_id = fk.parent_object_id
       INNER JOIN sys.foreign_key_columns AS fkc
           ON fk.object_id = fkc.constraint_object_id
       INNER JOIN sys.columns AS col
           ON fkc.parent_object_id = col.object_id
          AND fkc.parent_column_id = col.column_id
       INNER JOIN sys.columns AS col_prim
           ON fkc.referenced_object_id = col_prim.object_id
          AND fkc.referenced_column_id = col_prim.column_id
       INNER JOIN sys.tables AS tab_prim
           ON fk.referenced_object_id = tab_prim.object_id
     ORDER BY
       table_schema_name,
       table_name,
       primary_table_name,
       fk_part"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Views and Columns----
  if(DDType == 5L) {
    qry <- "SELECT
      schema_name(v.schema_id) AS schema_name,
      v.name AS view_name,
      col.name AS column_name,
      t.name AS data_type,
      t.name +
      CASE WHEN t.is_user_defined = 0 THEN
                 ISNULL('(' +
                 CASE WHEN t.name IN ('binary', 'char', 'nchar','varchar', 'nvarchar', 'varbinary') THEN
                   CASE col.max_length when -1 THEN 'MAX'
                     ELSE
                       CASE WHEN t.name IN ('nchar','nvarchar') THEN
                         CAST(col.max_length/2 AS varchar(4)) ELSE
                         CAST(col.max_length AS varchar(4))
                   END
                 END
                      when t.name IN ('datetime2',
                           'datetimeoffset', 'time') THEN
                            cast(col.scale AS varchar(4))
                      when t.name IN ('decimal', 'numeric') THEN
                           cast(col.precision AS varchar(4)) + ', ' +
                           cast(col.scale AS varchar(4))
                 END + ')', '')
            ELSE ':' +
                 (SELECT c_t.name +
                         ISNULL('(' +
                         CASE WHEN c_t.name IN ('binary','char','nchar', 'varchar', 'nvarchar','varbinary') THEN
                           CASE c.max_length
                             WHEN -1 THEN 'MAX'
                             ELSE
                               CASE WHEN t.name IN ('nchar','nvarchar')
                                 THEN cast(c.max_length/2 AS varchar(4))
                                 ELSE cast(c.max_length AS varchar(4))
                                             END
                                   END
                              WHEN c_t.name IN ('datetime2',
                                   'datetimeoffset', 'time') THEN
                                   cast(c.scale AS varchar(4))
                              WHEN c_t.name IN ('decimal', 'numeric') THEN
                                   cast(c.precision AS varchar(4)) +
                                   ', ' + cast(c.scale AS varchar(4))
                         END + ')', '')
                  FROM
                    sys.columns AS c
                  INNER JOIN
                    sys.types AS c_t
                  ON
                    c.system_type_id = c_t.user_type_id
                  WHERE c.object_id = col.object_id
                    and c.column_id = col.column_id
                    and c.user_type_id = col.user_type_id
                 ) END AS data_type_ext,
       CASE WHEN col.is_nullable = 0 THEN 'N' ELSE 'Y' END AS nullable,
       ep.value AS comments
  FROM
    sys.views AS v
  JOIN
    sys.columns AS col
  ON
    v.object_id = col.object_id
  LEFT JOIN
    sys.types AS t
  ON
    col.user_type_id = t.user_type_id
  LEFT JOIN
    sys.extended_properties AS ep
  ON
    v.object_id = ep.major_id
    AND col.column_id = ep.minor_id
    AND ep.name = 'MS_Description'
    AND ep.class_desc = 'OBJECT_OR_COLUMN'
 ORDER BY
   schema_name,
   view_name,
   column_name"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }

  # Tables and number of columns----
  if(DDType == 6L) {
    qry <- "SELECT
    schema_name(tab.schema_id) AS schema_name,
      tab.name AS table_name,
      COUNT(*) AS columns
    FROM sys.tables AS tab
    INNER JOIN
      sys.columns AS col
    ON
      tab.object_id = col.object_id
    GROUP BY
      schema_name(tab.schema_id),
      tab.name
    ORDER BY
      COUNT(*) DESC"

    # Return data----
    x <- data.table::as.data.table(sqlQuery(DBConnection, qry))
    if(CloseChannel) close(DBConnection)
    return(x)
  }
}

#' SQL_Server_DBConnection
#'
#' SQL_Server_DBConnection is a function to return data dictionary data in table form
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DataBaseName Name of the database
#' @param Server Name of the server to use
#' @export
SQL_Server_DBConnection <- function(DataBaseName = "",
                                    Server = "") {
  return(RODBC::odbcDriverConnect(connection  = paste0("Driver={SQL Server};
                                  server=",Server,"; database=",DataBaseName,";
                                  trusted_connection=yes;")))
}

#' SQL_Query
#'
#' SQL_Query get data from a database
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DBConnection RemixAutoML::SQL_Server_DBConnection()
#' @param Query The SQL statement you want to run
#' @param ASIS Auto column typing
#' @param CloseChannel TRUE to close when done, FALSE to leave the channel open
#' @export
SQL_Query <- function(DBConnection,
                      Query,
                      ASIS = FALSE,
                      CloseChannel = TRUE,
                      RowsPerBatch = 1024) {
  library(RODBC)
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")
  if(!is.null(Query)) {
    x <- data.table::as.data.table(RODBC::sqlQuery(channel = DBConnection, query = Query, as.is = ASIS, rows_at_time = RowsPerBatch))
    if(CloseChannel) close(DBConnection)
    return(x)
  }
}

#' SQL_ClearTable
#'
#' SQL_ClearTable get data from a database
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DBConnection RemixAutoML::SQL_Server_DBConnection()
#' @param SQLTableName The SQL statement you want to run
#' @param CloseChannel TRUE to close when done, FALSE to leave the channel open
#' @param Errors Set to TRUE to halt, FALSE to return -1 in cases of errors
#' @export
SQL_ClearTable <- function(DBConnection,
                           SQLTableName = "",
                           CloseChannel = TRUE,
                           Errors = TRUE) {
  library(RODBC)
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")
  RODBC::sqlClear(
    channel = DBConnection,
    sqtable = SQLTableName,
    errors  = Errors)
  if(CloseChannel) close(DBConnection)
}

#' SQL_DropTable
#'
#' SQL_DropTable get data from a database
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DBConnection RemixAutoML::SQL_Server_DBConnection()
#' @param SQLTableName The SQL statement you want to run
#' @param CloseChannel TRUE to close when done, FALSE to leave the channel open
#' @param Errors Set to TRUE to halt, FALSE to return -1 in cases of errors
#' @export
SQL_DropTable <- function(DBConnection,
                          SQLTableName = "",
                          CloseChannel = TRUE,
                          Errors = TRUE) {
  library(RODBC)
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")
  RODBC::sqlClear(
    channel = DBConnection,
    sqtable = SQLTableName,
    errors  = Errors)
  if(CloseChannel) close(DBConnection)
}

#' SQL_SaveTable
#'
#' SQL_SaveTable get data from a database
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DataToPush data to be sent to warehouse
#' @param DBConnection RemixAutoML::SQL_Server_DBConnection()
#' @param SQLTableName The SQL statement you want to run
#' @param rownames c("Segment","Date")
#' @param VarTypes = NULL,
#' @param AppendData TRUE or FALSE
#' @param AddPK Add a PK column to table
#' @param CloseChannel TRUE to close when done, FALSE to leave the channel open
#' @param Errors Set to TRUE to halt, FALSE to return -1 in cases of errors
#' @export
SQL_SaveTable <- function(DataToPush,
                          DBConnection,
                          SQLTableName = "",
                          RowNames = NULL,
                          ColNames = TRUE,
                          VarTypes = NULL,
                          CloseChannel = TRUE,
                          AppendData = FALSE,
                          AddPK = TRUE,
                          Errors = TRUE,
                          Safer = TRUE) {
  library(RODBC)
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")
  RODBC::sqlSave(
    rownames  = RowNames,
    colnames  = ColNames,
    varTypes  = VarTypes,
    channel   = DBConnection,
    dat       = DataToPush,
    tablename = SQLTableName,
    addPK     = AddPK,
    append    = AppendData,
    errors    = Errors,
    safer     = Safer)
  if(CloseChannel) close(DBConnection)
}

#' SQL_UpdateTable
#'
#' SQL_UpdateTable get data from a database
#'
#' @author Adrian Antico
#' @family Data Wrangling
#' @param DataToPush Update data table in warehouse with new values
#' @param DBConnection RemixAutoML::SQL_Server_DBConnection()
#' @param SQLTableName The SQL statement you want to run
#' @param Index Column name of index
#' @param Verbose TRUE or FALSE
#' @param Test Set to TRUE to see if what you plan to do will work
#' @param NAString Supply character string to supply missing values
#' @param Fast Set to TRUE to update table in one shot versus row by row
#' @param CloseChannel TRUE to close when done, FALSE to leave the channel open
#' @export
SQL_UpdateTable <- function(DataToPush,
                            DBConnection,
                            SQLTableName = "",
                            Index = NULL,
                            CloseChannel = TRUE,
                            Verbose = TRUE,
                            Test = FALSE,
                            NAString = "NA",
                            Fast = TRUE) {
  library(RODBC)
  if(!class(DBConnection) == "RODBC") return("Invalid DBConnection")
  RODBC::sqlUpdate(
    channel   = DBConnection,
    dat       = DataToPush,
    tablename = SQLTableName,
    index     = Index,
    verbose   = Verbose,
    test      = Test,
    nastring  = NAString,
    fast      = Fast)
  if(CloseChannel) close(DBConnection)
}
