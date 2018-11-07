# reference_semantics.R

# https://cloud.r-project.org/web/packages/data.table/vignettes/
# datatable-reference-semantics.html

library(data.table)
`%>%` <- magrittr::`%>%`

# Load 'flights' data
flights <- flights::nycflights14(path = "./", dir = "flights", verbose = TRUE)
dim(flights)

# 1. Reference semantics ----

# _ a) Background ----

DF = data.frame(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12, c = 13:18)
DF

tracemem(DF)
DF$c <- 18:13
tracemem(DF)

tracemem(DF)
DF$c[DF$ID == 'b'] <- 100:102
tracemem(DF)


# _ b) The := operator ----

# Use := to add/update/delete columns BY REFERENCE

DF
class(DF)
setDT(DF)
DF
class(DF)

DF[, c("col_a", "col_b") := .(a, b)] # modifies DF in place with setDT
DF

DF[, col_c := c]
DF

DF[, `:=`(col_d = a + b, # great for adding comments
          col_e = b + c)  # more comments here!
   ]
DF

# Notice that the result is returned invisibly... meaning that we don't have
# to assign the result back to the same (or different) variable


# 2. Add/update/delete columns by reference ----

# _ a) Add columns by reference ----

# --- How can we add columns `speed` and total `delay` 
#     of each flight to flights data.table?
dplyr::glimpse(flights)
flights[, `:=`(speed = distance / (air_time/60), # speed in miles / (min/60)
               delay = dep_delay + arr_delay)    # delay in minutes
        ]
# # ... alternatively, using the 'LHS := RHS' form
# flights[,
#         c('speed', 'delay') := 
#           .(distance/(air_time/60), arr_delay + dep_delay)]
dplyr::glimpse(flights)
# Again notice that the result wasn't assigned back to `flights` with `<-`


# _ b) Update some rows of columns by reference - sub-assign by reference ----

# You can use := along with 'i'

flights[, sort(unique(hour))] # there are 24s instead 

# --– Replace those rows where hour == 24 with the value 0
flights[hour == 24L, hour := 0L] # set all the 24s to 0s

# ... to see the results immediately, use empty brackets after statement
flights[hour == 24L, hour := 0L][]

# ... check that it worked
flights[, sort(unique(hour))] # Yep!

# flights[hour == 24L, hour := 0L] doesn't need assignment to a variable to be
# captured... but
# flights[hour == 24L][, hour := 0L] would need assignment to a variable to be
# captured...
# For details, read the "Note:" section of ?`:=`


# _ c) Delete column by reference ----

# --- Remove `delay` column

dplyr::glimpse(flights)
flights[, c('delay') := NULL]
# # ... or using the functional form
# flights[, `:=`(delay = NULL)]
# # ... when there's a single column to drop, you can just use the column name
# flights[, delay := NULL]
dplyr::glimpse(flights)


# _ d) := along with grouping using 'by' ----

# You can use := along with 'i' and/or 'by'

# --– How can we add a new column which contains 
#     for each orig,dest pair the maximum speed?

# ... one new field
flights[, max_speed := max(speed, na.rm = TRUE), by = .(origin, dest)]
dplyr::glimpse(flights)

flights[, c('max_speed') := .(max(speed, na.rm = TRUE)), by = .(origin, dest)]
dplyr::glimpse(flights)

# ... two new fields
flights[, 
        `:=`(max_speed = max(speed, na.rm = TRUE),
             min_speed = min(speed, na.rm = TRUE)), 
        by = .(origin, dest)]
dplyr::glimpse(flights)

flights[,
        c('max_speed', 'min_speed') :=
          .(max(speed, na.rm = TRUE), min(speed, na.rm = TRUE)),
        by = .(origin, dest)]
dplyr::glimpse(flights)


# _ e) Multiple columns and := ----

# --– How can we add two more columns computing max() 
#     of dep_delay and arr_delay for each month, using .SD?

in_cols = c('dep_delay', 'arr_delay')
out_cols = c('max_dep_delay', 'max_arr_delay')

# No grouping... just overall max
flights[, c(out_cols) := lapply(.SD, max, na.rm = TRUE), .SDcols = in_cols]
dplyr::glimpse(flights)
# ... notice that `out_cols` has to be wrapped in `c()` in order to evaluate
#     `out_cols` to a vector with 2 strings... otherwise data.table would try
#     to be assigning a list of length 2 into one variable name, `out_cols`

# Grouping by carrier... max'es for each carrier
flights[, 
        c(out_cols) := lapply(.SD, max, na.rm = TRUE), 
        by = .(carrier),
        .SDcols = in_cols]
dplyr::glimpse(flights)

# Grouping by month... max'es for each month
flights[,
        c(out_cols) := lapply(.SD, max, na.rm = TRUE),
        by = .(month),
        .SDcols = in_cols]
dplyr::glimpse(flights)
flights %>% 
  dplyr::select(month, max_dep_delay, max_arr_delay) %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(dplyr::desc(max_dep_delay))
unique(flights[, .(month, max_dep_delay, max_arr_delay)], 
       by = c('month', 'max_dep_delay', 'max_arr_delay'))[order(-max_dep_delay)]

# ... now I'll try with min's
in_cols <- c('dep_delay', 'arr_delay')
out_cols <- c('min_dep_delay', 'min_arr_delay')
# Group by origin,dest pair
# flights[, 
#         out_cols := lapply(.SD, min, na.rm = TRUE), 
#         by = .(origin, dest),
#         .SDcols = in_cols]
# ^above won't work becase `out_cols` needs to be wrapped with `c()`
flights[, 
        c(out_cols) := lapply(.SD, min, na.rm = TRUE),
        by = .(origin, dest),
        .SDcols = in_cols]
dplyr::glimpse(flights)

# Quick practice deleting columns by reference
# Single column
flights[, min_speed := NULL]
dplyr::glimpse(flights)
# Multiple columns; Functional form
flights[, `:=`(min_dep_delay = NULL,
               min_arr_delay = NULL)]
dplyr::glimpse(flights)
# Multiple columns; LHS := RHS form with multi-column variable `out_cols`
flights[, c(out_cols) := .(NULL, NULL)]
dplyr::glimpse(flights)

# Multiple columns; LHS := RHS form... simplified
flights[, c('speed', 'max_speed', 'max_dep_delay', 'max_arr_delay') := NULL]
dplyr::glimpse(flights)


# 3) := and copy() ----

# We can use := for its side effect or use copy() to not modify 
# the original object while updating by reference.

# _ a) := for its side effect ----

# Let’s say we would like to create a function that would return 
# the maximum speed for each month. But at the same time, we would also 
# like to add the column speed to flights.

foo <- function(DT) {
  DT[, speed := distance / (air_time/60)] # creates new col by reference
  # ^modifies `flights` data.table outside of function b/c modif. by reference
  DT[, .(max_speed = max(speed, na.rm = TRUE)), by = month]
  # ^retuns DT, but only with `month` and `max_speed` columns
}

ans <- foo(flights)
ans
dplyr::glimpse(flights) # see... `flights` was modified by `foo` b/c of ref


# _ b) The copy() function ----

# Sometimes, we would like to pass a data.table object to a function, 
# and might want to use the := operator, but wouldn’t want to update 
# the original object. We can accomplish this using the function copy().

# First let's delete the `speed` column
flights[, speed := NULL]
str(flights)

# copy() makes a deep copy of the DT.

foo <- function(DT) {
  DT <- copy(DT)                              # deep copy of `flights`
  DT[, speed := distance / (air_time/60)]     # doesn't affect `flights`
  DT[, .(max_speed = max(speed)), by = month]
}

str(flights)
ans <- foo(flights)
str(flights)

# This goes for the data in the data.table...
# AND also for the column names!

DT = data.table(x = 1L, y = 2L)
DT
DT_n = names(DT)
DT_n

# add a new column by reference
DT[, z := 3L]

# DT_n also gets updated
DT_n

# So use copy() for a static version of DT_n
DT_n = copy(names(DT))
DT_n
DT[, w := 4L]

# DT_n doesn't get updated
DT_n



















