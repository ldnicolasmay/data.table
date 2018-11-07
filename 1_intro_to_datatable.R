# intro_to_datatable.R

# https://cran.r-project.org/web/packages/data.table/vignettes/
# datatable-intro.html

library(data.table)
library(magrittr)

# devtools::install_github("arunsrinivasan/flights")

# flights::downloadflightlogs(year = 2014L, month = 1:12, path = "./", 
#                             dir = "flights", verbose = TRUE)
flights <- flights::nycflights14(path = "./", dir = "flights", verbose = TRUE)
# ?fread

dim(flights)
class(flights)

DT <- data.table(
  ID = c("b", "b", "b", "a", "a", "c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)
DT
class(DT)
class(DT$ID)
class(DT$a)

dplyr::glimpse(flights)

# 1. Basics ----

# _ a) What is data.table? ----

DT = data.table(
  ID = c("b","b","b","a","a","c"),
  a = 1:6,
  b = 7:12,
  c = 13:18
)
DT
class(DT)
class(DT$ID)
class(DT$a)


# _ b) General form - in what way is a data.table enhanced? ----


# _ c) Subset rows in i ----

# --- Get all the flights with “JFK” as the origin airport in the month of June.
ans <- flights[origin == 'JFK' & month == 6L]
# ans <- flights[origin == 'JFK' & month == 6, , ] # commas OK, but unnecessary
ans
unique(ans$origin)
unique(ans$month)

# --- Get the first two rows from `flights`.
ans <- flights[1:2]
ans

# --- Sort flights first by column origin in ascending order, 
#     and then by dest in descending order:
ans <- flights[order(origin, -dest)]
ans
unique(flights$origin)


# _ d) Select columns in j ----

# --- Select `arr_delay` column, but return it as a vector.

ans <- flights[, arr_delay]       # returns a vector

# --- Select arr_delay column, but return as a data.table instead.
ans <- flights[, list(arr_delay)] # returns a data.table object
# Equivalent statment:
ans <- flights[, .(arr_delay)]    # returns a data.table object
head(ans)
class(ans)

# --- Select both arr_delay and dep_delay columns.
ans <- flights[, .(arr_delay, dep_delay)]
ans
mean(ans$arr_delay, na.rm = TRUE)
mean(ans$dep_delay, na.rm = TRUE)

# --- Select both arr_delay and dep_delay columns and 
#     rename them to delay_arr and delay_dep.
ans <- flights[, .(delay_arr = arr_delay, delay_dep = dep_delay)]
ans


# _ e) Compute or do in j ----

# --- How many trips have had total delay < 0 ?
ans <- flights[, (arr_delay + dep_delay)] # int vector
head(ans, n = 20)
ans <- flights[, (arr_delay + dep_delay) < 0] # logical vector
head(ans, n = 20)
ans <- flights[, sum( (arr_delay + dep_delay) < 0)] # NA_integer_ result
ans
ans <- flights[, sum( (arr_delay + dep_delay) < 0, na.rm = TRUE)] # int result
ans


# _ f) Subset in i and do in j ----

# --- Calculate the average arrival and departure delay for all flights 
#     with “JFK” as the origin airport in the month of June.
ans <- flights[origin == 'JFK' & month == 6L, 
               .(mean_arr_delay = mean(arr_delay),
                 mean_dep_delay = mean(dep_delay))] # returns NA_real_ values
ans
class(ans[1L, mean_arr_delay])

ans <- flights[origin == 'JFK' & month == 6L, 
               .(mean_arr_delay = mean(arr_delay, na.rm = TRUE),
                 mean_dep_delay = mean(dep_delay, na.rm = TRUE))]
ans
class(ans)

# My own questions...
# --- Calculate how many NAs there are in arr_delay and dep_delay 
#     from the same subset (JFK origin in June)
my_ans <- flights[origin == 'JFK' & month == 6L,
                  .(arr_delay_NAs = sum(is.na(arr_delay)),
                    dep_delay_NAs = sum(is.na(dep_delay)))]
my_ans
my_ans / nrow(flights)

# --- Which destinations have NAs for arr_delay and dep_delay
#     ... arr_delay
my_ans <- flights[origin == 'JFK' & month == 6L & is.na(arr_delay), table(dest)]
sort(my_ans, decreasing = TRUE)
# ... arr_delay
my_ans <- flights[origin == 'JFK' & month == 6L & is.na(dep_delay), table(dest)]
sort(my_ans, decreasing = TRUE)

# --- How many trips have been made in 2014 from “JFK” airport 
#     in the month of June?
dplyr::glimpse(flights)
unique(flights$year) # only 2014
ans <- flights[origin == 'JFK' & month == 6L, length(dest)]
ans 

# Special symbol `.N`
# flights[origin == 'JFK' & month == 6L, .N]
ans <- flights[origin == 'JFK' & month == 6L, .N]
ans


# _ g) How can I refer to columns by names in j (like in a data.frame)? ----

# --- Select both arr_delay and dep_delay columns the data.frame way.
ans <- flights[, c('arr_delay', 'dep_delay')]
ans

# --- Select columns named in a variable using the `..` prefix
select_cols <- c('arr_delay', 'dep_delay')
ans <- flights[, ..select_cols] # notice `..` before variable name
ans

# --- Select columns named in a variable using `with = FALSE`
select_cols <- c('arr_delay', 'dep_delay')
ans <- flights[, select_cols, with = FALSE]
ans
# This `with` argument is similar to base R `with()` function
DF = data.frame(x = c(1,1,1,2,2,3,3,3), y = 1:8)
# Normal subsetting
DF[DF$x > 1, ]
# `with()` subsetting
with(DF, x > 1) # returns logical vector
DF[with(DF, x > 1), ]

# --- Deselect columns using `-` or `!`
ans <- flights[, -c('arr_delay', 'dep_delay')]
ans
ans <- flights[, !c('arr_delay', 'dep_delay')]
ans
# --- Deselect columns using variable
select_cols <- c('arr_delay', 'dep_delay')
ans <- flights[, -..select_cols]
ans
ans <- flights[, !..select_cols]
ans
# --- Deselect columsn using variable and `with` arg
ans <- flights[, -select_cols, with = FALSE]
ans
ans <- flights[, !select_cols, with = FALSE]
ans

# --- Select column ranges using `:`
dplyr::glimpse(flights)
ans <- flights[, year:day]
ans
ans <- flights[, day:year]
ans
# --- Deselect column ranges using -/! and :
ans <- flights[, -(year:day)]
ans
ans <- flights[, !(year:day)]
ans



# 2. Aggregations ----

# _ a) Grouping using `by` ----

# --- How can we get the number of trips corresponding to each origin airport?
ans <- flights[, .(.N), by = .(origin)]
ans
sum(ans$N) == nrow(flights)
# ans <- flights[, .(.N), by = 'origin'] # equivalent to above
# ans

# --- How can we calculate the number of trips for each origin airport 
#     for carrier code "AA"?
ans <- flights[carrier == 'AA', .(.N), by = .(origin)]
ans

# ans <- flights[carrier == 'AA', .(.N), by = .(origin, dest)][order(origin)]
# ans
# unique(flights$carrier)
# ans <- flights[carrier == 'DL', .(.N), by = .(origin, dest)][order(origin, -N)]
# ans

# --- How can we get the total number of trips for each origin, dest pair 
#     for carrier code "AA"?
ans <- flights[carrier == 'AA', .(.N), by = .(origin, dest)]
ans

# --- How can we get the average arrival and departure delay 
#     for each orig,dest pair for each month for carrier code "AA"?
ans <- flights[carrier == 'AA',                                      # i
               .(mean(dep_delay, na.rm = TRUE), # j
                 mean(arr_delay, na.rm = TRUE)),
               by = .(origin, dest, month)]                          # by
# ans[order(origin, -mean_arr, -mean_dep)]
ans


# _ b) Sorted `by`: `keyby` ----

# --- So how can we directly order by all the grouping variables?
ans <- flights[carrier == 'AA',
               .(mean(dep_delay, na.rm = TRUE),
                 mean(arr_delay, na.rm = TRUE)),
               keyby = .(origin, dest, month)]
ans
str(ans)


# _ c) Chaining ----

# --- Get the total number of trips for each origin, dest pair for carrier “AA”.
ans <- flights[carrier == 'AA', .(.N), by = .(origin, dest)]
ans
# ans <- ans[order(origin, -dest)]
# ans
ans <- ans[order(origin, -N)]
ans

# ... alternatively, just chain them together
ans <- flights[carrier == 'AA', .(.N), by = .(origin, dest)][order(origin, -N)]
ans

# ... using the magrittr pipe
ans <- flights[carrier == 'AA', .(.N), by = .(origin, dest)] %>% 
  .[order(origin, -N)]
ans

# # benchmarking data.table pipe (...[][]) vs. magrittr pipe (%>%)
# `%>%` <- magrittr::`%>%`
# set.seed(123)
# dt <- data.table(a = sample(letters, 1e5, replace = TRUE),
#                  b = abs(rnorm(1e5)))
# 
# datatable_pipe <- function(){
#   dt[, x := sqrt(b)
#      ][, y := b^2
#       ][, z := paste0(a , b)
#         ]
# }
# 
# magrittr_pipe <- function(){
#   dt[, x := sqrt(b)] %>%
#     .[, y := b^2] %>%
#     .[, z := paste0(a , b)]
# }
# 
# microbenchmark::microbenchmark(datatable_pipe(), magrittr_pipe(), times = 50)
# # Basically the same speed performance! ... magrittr is more legible to me


# _ d) Expressions in `by` ----

# --- Can `by` accept expressions as well or does it just take columns?
ans <- flights[, .(.N), .(dep_delay>0, arr_delay>0)]
tibble::as.tibble(ans)
ans
sum(ans$N) == nrow(flights)

ans <- flights[, .(.N), .(dep_delay>0, arr_delay>0)] %>% 
  .[!is.na(dep_delay) & !is.na(arr_delay)] %>% 
  .[order(dep_delay, arr_delay)]
ans


# _ e) Multiple columns in `j` - `.SD` ----
#      (SD = subset of data)

# --- Do we have to compute mean for each column individually? No!
ans <- flights[carrier == 'AA',
               lapply(.SD, mean, na.rm = TRUE),
               by = .(origin, dest, month),
               .SDcols = c('arr_delay', 'dep_delay')]
ans

dplyr::glimpse(flights)
ans <- flights[, 
               lapply(.SD, mean, na.rm = TRUE), 
               .SDcols = c('dep_delay', 'arr_delay', 'air_time')]
ans
# ... using purrr
ans <- flights[,
               purrr::map(.SD, mean, na.rm = TRUE),
               .SDcols = c('dep_delay', 'arr_delay', 'air_time')]
ans
ans <- flights[,
               lapply(.SD, mean, na.rm = TRUE),
               by = .(origin),
               .SDcols = c('dep_delay', 'arr_delay', 'air_time')]
ans
ans <- flights[,
               lapply(.SD, mean, na.rm = TRUE),
               by = .(origin, dest),
               .SDcols = c('dep_delay', 'arr_delay', 'air_time')]
ans
ans <- flights[, .(.N), by = .(origin, dest)][order(origin, -N)]
ans

library(ggplot2)
max(flights$dep_delay, na.rm = TRUE)
max(flights$arr_delay, na.rm = TRUE)
t <- flights[dest %in% c('LAX', 'SFO', 'MCO', 'ATL')] %>% 
  ggplot(data = ., aes(x = dep_delay)) + 
  geom_histogram(binwidth = 10, color = 'black', fill = 'lightgray') +
  scale_x_continuous(limits = c(-60, 120))
t + facet_grid(origin ~ dest)


# _ f) Subset .SD for each group: ----

# --- How can we return the first two rows for each month?

ans <- flights[, head(.SD, n = 2), by = .(month)]
ans
ans <- flights[, 
               head(.SD, n = 2), 
               by = .(month), 
               .SDcols = c('dep_delay', 'arr_delay', 'carrier')]
ans
ans <- flights[,
               head(.SD, n = 3),
               by = .(carrier, origin),
               .SDcols = c('month', 'dep_delay', 'arr_delay')]
ans


# _ g) Why keep j so flexible? ----

# --- How can we concatenate columns a and b for each group in ID?
DT
DT[, .(val = c(a,b)), by = ID]
DT[, .(val = c('a','b')), by = ID]
DT
DT[, .(val = c(a,b,c)), by = ID]
my_cols <- quote(c(a,b,c))
DT[, .(val = eval(my_cols)), by = ID]

# --- What if we would like to have all the values 
#     of column a and b concatenated, but returned as a list column?
DT
DT[, .(val = list(c(a,b))), by = ID]

## (1) look at the difference between
DT[, print(c(a,b)), by = ID]

## (2) and
DT[, print(list(c(a,b))), by = ID]














