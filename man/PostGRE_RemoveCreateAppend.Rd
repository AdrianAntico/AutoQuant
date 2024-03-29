% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SQL_Functions.R
\name{PostGRE_RemoveCreateAppend}
\alias{PostGRE_RemoveCreateAppend}
\title{PostGRE_RemoveCreateAppend}
\usage{
PostGRE_RemoveCreateAppend(
  data = NULL,
  TableName = NULL,
  CloseConnection = TRUE,
  CreateSchema = NULL,
  Host = NULL,
  DBName = NULL,
  User = NULL,
  Port = NULL,
  Password = NULL,
  Temporary = FALSE,
  Connection = NULL,
  Append = TRUE
)
}
\arguments{
\item{data}{See args from related functions}

\item{TableName}{See args from related functions}

\item{CloseConnection}{See args from related functions}

\item{CreateSchema}{See args from related functions}

\item{Host}{See args from related functions}

\item{DBName}{See args from related functions}

\item{User}{See args from related functions}

\item{Port}{See args from related functions}

\item{Password}{See args from related functions}

\item{Temporary}{See args from related functions}

\item{Connection}{See args from related functions}

\item{Append}{See args from related functions}
}
\description{
PostGRE_RemoveCreateAppend will DROP the table specified
}
\seealso{
Other Database: 
\code{\link{AutoDataDictionaries}()},
\code{\link{PostGRE_AppendData}()},
\code{\link{PostGRE_CreateTable}()},
\code{\link{PostGRE_GetTableNames}()},
\code{\link{PostGRE_ListTables}()},
\code{\link{PostGRE_Query}()},
\code{\link{PostGRE_RemoveTable}()},
\code{\link{PosteGRE_CreateDatabase}()},
\code{\link{PosteGRE_DropDB}()},
\code{\link{PosteGRE_ListDatabases}()},
\code{\link{SQL_ClearTable}()},
\code{\link{SQL_DropTable}()},
\code{\link{SQL_Query_Push}()},
\code{\link{SQL_Query}()},
\code{\link{SQL_SaveTable}()},
\code{\link{SQL_Server_DBConnection}()}
}
\author{
Adrian Antico
}
\concept{Database}
