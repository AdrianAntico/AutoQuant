#' AutoMarketBasketModel function runs a market basket analysis automatically
#'
#' AutoMarketBasketModel function runs a market basket analysis automatically. It will convert your data, run the algorithm, and add on additional significance values not orginally contained within.
#'
#' @seealso Chi-sq statistics and p-values based on this paper: http://www.cs.bc.edu/~alvarez/ChiSquare/chi2tr.pdf
#'
#' @author Adrian Antico and Douglas Pestana
#' @family Recommender Systems
#' @importClassesFrom arules transactions
#' @param data This is your transactions data set
#' @param OrderIDColumnName Supply your column name for the Order ID Values
#' @param ItemIDColumnName Supply your column name for the Item ID Values
#' @param LHS_Delimeter Default delimeter for separating multiple ItemID's is a comma.
#' @param Support Threshold for inclusion using support
#' @param Confidence Threshold for inclusion using confidence
#' @param MaxLength Maximum combinations of Item ID (number of items in basket to consider)
#' @param MinLength Minimum length of combinations of ItemID (number of items in basket to consider)
#' @param MaxTime Max run time per iteration (default is 5 seconds)
#' @examples
#' \dontrun{
#' rules_data <- AutoMarketBasketModel(
#'   data,
#'   OrderIDColumnName = "OrderNumber",
#'   ItemIDColumnName = "ItemNumber",
#'   LHS_Delimeter = ",",
#'   Support = 0.001,
#'   Confidence = 0.1,
#'   MaxLength = 2,
#'   MinLength = 2,
#'   MaxTime = 5)
#' }
#' @export
AutoMarketBasketModel <- function(data,
                                  OrderIDColumnName,
                                  ItemIDColumnName,
                                  LHS_Delimeter = ",",
                                  Support = 0.001,
                                  Confidence = 0.1,
                                  MaxLength = 2,
                                  MinLength = 2,
                                  MaxTime = 5) {

  # data.table optimize----
  if(parallel::detectCores() > 10) data.table::setDTthreads(threads = max(1L, parallel::detectCores() - 2L)) else data.table::setDTthreads(threads = max(1L, parallel::detectCores()))

  # Convert to data.table----
  if(!data.table::is.data.table(data)) data.table::setDT(data)

  # Total number of unique OrderIDColumns----
  n <- length(unique(data[[eval(OrderIDColumnName)]]))

  # Check args----
  if(!is.character(OrderIDColumnName)) return("OrderIDColumnName needs to be a character value")
  if(!is.character(ItemIDColumnName)) return("ItemIDColumnName needs to be a character value")
  if(!(Confidence > 0 & Confidence < 1)) return("Confidence needs to be between zero and one")
  if(!(Support > 0 & Support < 1)) return("Support needs to be between zero and one")
  if(MaxLength <= 0) return("MaxLength needs to be a positive number")
  if(MinLength > MaxLength | MinLength < 0) return("MinLength needs to be less than MaxLength and greater than zero")

  # Subset data----
  data <- data[, .(get(OrderIDColumnName), get(ItemIDColumnName))]
  data.table::setnames(data, c("V1", "V2"), c(eval(OrderIDColumnName),eval(ItemIDColumnName)))

  # Look into data.table split----
  TransactionData <- methods::as(split(data[[eval(ItemIDColumnName)]], data[[eval(OrderIDColumnName)]]), "transactions")

  # Association rules----
  options(warn = -1L)
  rules <- arules::apriori(
    data = TransactionData,
    parameter = list(
      support = Support,
      confidence = Confidence,
      target = "rules",
      minlen = MinLength,
      maxlen = MaxLength,
      maxtime = MaxTime))
  options(warn = 0L)

  # Convet back to data.table---
  rules_data <- data.table::data.table(ProductA = arules::labels(arules::lhs(rules)), ProductB = arules::labels(arules::rhs(rules)), rules@quality)
  data.table::setnames(rules_data, c("support","confidence","lift","count"), c("Support","Confidence", "Lift","Count"))

  # Delimeter time----
  if(LHS_Delimeter != ",") rules_data[, ProductA := gsub(",", LHS_Delimeter, ProductA)]

  # Remove left Brackets from ProductA, ProductB Columns----
  rules_data[, ':=' (ProductA = gsub("\\}.*", "", ProductA, ignore.case = TRUE), ProductB = gsub("\\}.*", "", ProductB, ignore.case = TRUE))]

  # Remove right Brackets from ProductA, ProductB Columns----
  rules_data[, ':=' (ProductA = gsub("\\{", "", ProductA, ignore.case = TRUE), ProductB = gsub("\\{", "", ProductB, ignore.case = TRUE))]

  # Compute Chi-Sq. and P-Value----
  rules_data[, Chi_SQ := n * (Lift - 1) ^ 2 * Support * Confidence / ((Confidence - Support) * (Lift - Confidence))]
  rules_data[, P_Value := round(1 - pchisq(Chi_SQ, 1L), 10L)]

  # Sort data properly----
  rules_data <- rules_data[order(ProductA, -Lift)]

  # Add Rule Rank by ProductA----
  rules_data[, RuleRank := 1L:.N, by = "ProductA"]

  # Change Column Names----
  data.table::setnames(rules_data, c("ProductA", "ProductB"), c(paste0(ItemIDColumnName, "_LHS"), paste0(ItemIDColumnName, "_RHS")))

  # Done----
  return(rules_data)
}
