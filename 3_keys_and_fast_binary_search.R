# keys_and_fast_binary_search.R

# https://cloud.r-project.org/web/packages/data.table/vignettes/
# datatable-keys-fast-subset.html

library(data.table)

# Load 'flights' data
flights <- flights::nycflights14(path = "./", dir = "flights", verbose = TRUE)


# 1. Keys ----

# _ a) What is a key? ----

set.seed(1L)
DF = data.frame(ID1 = sample(letters[1:2], size = 10, replace = TRUE),
                ID2 = sample(1:3, size = 10, replace = TRUE),
                val = sample(10),
                stringsAsFactors = FALSE,
                row.names = sample(LETTERS[1:10]))
DF
rownames(DF)

DF['C', ] # subset a data.frame using row name(s)
# Row names are like an index to rows of a data.frame
# But each row is limited to exactly one (one and only one) row name

# Now let's convert it to a data.table

DT = as.data.table(DF)
DT
rownames(DT)
# The row names have been reset... 
#    data.table doesn't use row names meaningfully
# You can preserve the row names... kinda...
DT_rn = as.data.table(DF, keep.rownames = TRUE)
DT_rn # row names are preserved in the column `rn`

# In data.table, keys are used instead of row names.

# Keys and their properties
#   1. We can set keys on multiple columns and the column can be 
#      of different types – integer, numeric, character, factor, integer64 etc. 
#      But list and complex types are not supported yet.
#   2. Uniqueness is not enforced, i.e., duplicate key values are allowed. 
#      Since rows are sorted by key, any duplicates in the key columns 
#      will appear consecutively.
#   3. Setting a key does TWO things:
#      a. physically reorders the rows of the data.table by the column(s)
#         provided, BY REFERENCE, always in INCREASING ORDER.
#      b. marks those columns as key columns by setting an attribute called
#         'sorted' to the data.table.
# Since the rows are reordered, a data.table can have at most one key because
# it cannot be sorted in more than one way.


# _ b) Set, get, and use keys on a data.table ----

# --- How can we set the column `origin` as key in the data.table `flights`?
setkey(flights, NULL)
key(flights)
setkey(flights, origin)
key(flights) # "sorted" attribute set to "origin"
data.table::

# # alternatively we can provide character vectors to the function 'setkeyv()'
# setkeyv(flights, 'origin')
# str(flights)
# # This is useful when programming with data.table

# --- set* and :=
# In data.table, the := operator and all the set* (e.g., setkey, setorder, 
# setnames, etc.) functions are the only ones which modify the input object 
# by reference.

# --– Use the key column origin to subset all rows where the origin 
#     airport matches “JFK”
# flights[origin == 'JFK'] # If keys weren't set, this would be the way...
flights[.('JFK')] # Key is 'origin', just pass a list of the key values
str(flights)

# Equivalent statements as above
flights['JFK']
flights[c('JFK')]
flights[.(c('JFK'))]

# Returning rows of more than one key value
flights[c('JFK', 'LGA')]
flights[.(c('JFK', 'LGA'))]

# --– How can we get the column(s) a data.table is keyed by?
key(flights)


# _ c) Keys and multiple columns ----

# Keys are like supercharged row names. 
# We can set key on multiple columns and they can be of multiple types.

# --- How can I set keys on both 'origin' and 'dest' columns?

setkey(flights, origin, dest)
# setkeyv(flights, c('origin', 'dest')) # alternative when coding
key(flights)
str(flights)

# --- Subset all rows using key columns 
#     where first key column origin matches “JFK” and 
#     second key column dest matches “MIA”
flights[.('JFK', 'MIA')]

# --- Subset all rows where just the first key column 'origin' matches “JFK”
key(flights)
flights[.('JFK')]

# --- Subset all rows where just the second key column 'dest' matches "MIA"
key(flights)
flights[.(c('EWR', 'JFK', 'LGA'), 'MIA')]
flights[.(unique(origin), 'MIA')] # better way


# 2. Combining keys with 'j' and 'by' ----

# _ a) Select in 'j' ----

# --- Return 'arr_delay' column as a data.table corresponding to
#     'origin' = 'LGA' and 'dest' = 'TPA'
key(flights)
flights[.('LGA', 'TPA'), .(arr_delay)]
# ... remember these
flights[.('LGA', 'TPA'), arr_delay] # returns a vector
flights[.('LGA', 'TPA'), 'arr_delay', with = FALSE] # returns a data.table

# _ b) Chaining ----

# --- On the result obtained above, use chaining to order the column 
#     in decreasing order.
flights[.('LGA', 'TPA'), .(arr_delay)][order(-arr_delay)]


# _ c) Compute or do in 'j' ----

# --- Find the maximum arrival delay corresponding to 
#     origin = 'LGA' and 'dest' = 'TPA'
# return as numeric:
flights[.('LGA', 'TPA'), max(arr_delay, na.rm = TRUE)]
# return as data.table:
flights[.('LGA', 'TPA'), .(max_arr_delay = max(arr_delay, na.rm = TRUE))]


# _ d) Sub-assign by reference using := in 'j' ----

flights[, sort(unique(hour))] # some 'hour' values are 24...

# Let's set any 24s to 0s (zeros)
setkey(flights, hour)
key(flights)
flights[.(24)]
flights[.(24), hour := 0L]
key(flights) # since values in the key column have been replaced,
             # the data.table isn't properly ordered, 
             # so the key is automatically removed

# Check our work
flights[, sort(unique(hour))] # checks out, A+


# _ e) Aggregation using 'by'

# Let's set the key back to 'origin', 'dest'
setkeyv(flights, c('origin', 'dest'))
key(flights)

# --- Get the maximum departure delay for each 'month' corresponding to
#     'origin' = 'JFK'. Order the result by 'month'.
ans <- flights[.('JFK'), 
               .(max_dep_delay = max(dep_delay, na.rm = TRUE)), 
               by = .(month)][order(month)]
ans
# better alternative using 'keyby'
ans <- flights[.('JFK'),
               .(max_dep_delay = max(dep_delay, na.rm = TRUE)),
               keyby = .(month)]
ans
key(ans) # keyby sets 'month' as the key in `ans`
key(flights) # ... but not in `flights`


# 3. Additional arguments - `mult` and `nomatch` ----

# _ a) The `mult` argument ----

# We can choose, for each query, if “all” the matching rows should be 
# returned, or just the “first” or “last” using the mult argument. 
# The default value is “all” - what we’ve seen so far.
# `mult` arg values are only 'all', 'first', and 'last'

# --- Subset only the first matching row from all rows where 
#     origin matches “JFK” and dest matches “MIA”
key(flights)
flights[.('JFK', 'MIA'), mult = 'first']

# --- Subset only the last matching row of all the rows where 'origin' matches
#     'LGA', 'JFK', 'EWR', and 'dest' matches 'XNA'
key(flights)
flights[.(unique(origin), 'XNA'), mult = 'last']


# _ b) The `nomatch` argument ----

# Options for `nomatch` arg are NA (default), meaning it'll return even NA rows;
# and 0L, which gets rid of NA rows

# --- From the previous example, subset all rows only if there’s a match
key(flights)
flights[.(unique(origin), 'XNA'), nomatch = 0L] # mult = 'all' is default

# --- Subset only the last matching row of all the rows where 'origin' matches
#     'LGA', 'JFK', 'EWR', and 'dest' matches 'XNA'... 
#     EXCLUDING rows where there isn't a match!
flights[.(unique(origin), 'XNA'), mult = 'last', nomatch = 0L]


# 4. Binary search vs. vector scans ----

f1 <- function() { # function using no keys
  setkey(flights, NULL)
  flights[origin == 'JFK' & dest == 'MIA']
}
f2 <- function() { # function using keys
  setkey(flights, origin, dest)
  flights[.('JFK', 'MIA')]
}

perf <- microbenchmark::microbenchmark(
  ans1 <- f1(),
  ans2 <- f2(),
  times = 100
)
perf
# class(perf)
# as.data.frame(perf)
# str(perf)
# as.data.frame(perf[perf$expr == 'ans1 <- f1()', ])[, 'time']
# as.data.frame(perf[perf$expr == 'ans2 <- f2()', ])[, 'time']
mean(as.data.frame(perf[perf$expr == 'ans1 <- f1()', ])[, 'time']) /
  mean(as.data.frame(perf[perf$expr == 'ans2 <- f2()', ])[, 'time'])


# _ a) Performance of binary search approach ----

# To illustrate, let’s create a sample data.table with 20 million rows and 
# three columns and key it by columns x and y.

set.seed(2L)
N = 2e7L
DT = data.table(x = sample(letters, N, TRUE),
                y = sample(1000L, N, TRUE),
                val = runif(N))
print(object.size(DT), units = "Mb")

# No key set
setkey(DT, NULL)
key(DT)
# ... vector search subsetting
t1 <- system.time(ans1 <- DT[x == 'g' & y == 877L])
t1
head(ans1)
dim(ans1)

# Set keys x and y
setkeyv(DT, c('x', 'y'))
key(DT)
# ... key subsetting
t2 <- system.time(ans2 <- DT[.('g', 877L)])
t2
head(ans2)
dim(ans2)

# are ans1 and ans2 identical
identical(ans1, ans2) # returns FALSE ... ???

# Above doesn't account for time setting keys (reordering data.table)



















