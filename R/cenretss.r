#' Census retirement state summaries.
#'
#' State-level summaries of Census Bureau Retirement Survey, selected years 1957-1977, annual 1982-2016.
#'
#' @source U.S. Bureau of the Census
#' @format Data frame with 1 row per state per administration level per variable per year.
#' \describe{
#' \item{year}{Pension systems' fiscal year, integer}
#' \item{stabbr}{State postal abbreviation, character}
#' \item{admin}{Administration level: 1 (State-local), 2 (State), or 3 (Local), integer}
#' \item{adminf}{Administration level - factor: State-local, State, or Local, character}
#' \item{variable}{Variable name}
#' \item{value}{State-level summary of value; dollar amounts are in $ thousands, numeric}
#' }
#' @examples
#' library(dplyr)
#' glimpse(cenretss)
"cenretss"

