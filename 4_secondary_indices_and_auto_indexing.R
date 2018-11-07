# secondary_indices_and_auto_indexing.R

library(data.table)
keys <- data.table::key # plural pseudonym to match data.table::indices

# Load 'flights' data
flights <- flights::nycflights14(path = "./", dir = "flights", verbose = TRUE)


# 1. Secondary indices ----

# _ a) What are secondary indices? ----

# Secondary indices are similar to keys in data.table, 
# except for two major differences:
#   1. It doesn’t physically reorder the entire data.table in RAM. 
#      Instead, it only computes the order for the set of columns provided 
#      and stores that order vector in an additional attribute called index.
#   2. There can be more than one secondary index for a data.table 
#      (as we will see below).


# _ b) Set and get secondary indices ----

# --- How can we set the 'column' origin as a secondary index
#     in the data.table `flights`?
setindex(flights, origin)
indices(flights)
head(flights)
str(flights)

# Alternatively, you can use the `setindexv()` when programming
setindexv(flights, c('origin'))
indices(flights)

attributes(flights)
names(attributes(flights))

# # Remove the index by...
# setindex(flights, NULL)
# names(attributes(flights))

# --- How can we get all the secondary indices set so far in 'flights'?
indices(flights)

# --- How can we set multiple secondary indices?
setindexv(flights, c('origin', 'dest')) # programming alternative
# setindex(flights, origin, dest)       # interactive alternative
indices(flights)

names(attributes(flights))
attributes(flights)$index


# _ c) Why do we need secondary indices? ----

# --- Reordering a data.table can be expensive and not always ideal

# # don't run
# setkey(flights, origin)
# flights[.('JFK')]

# `setkey()` requires:
#   1. computing the order vector for the column(s) provided (here origin) and
#   2. reordering the entire data.table, by reference, based 
#      on the order vector computed

# Computing the reorder isn't time-consuming... the actual reordering is.
# Unless the task at hand involved repeated subsetting on the key column,
# fast key-based subsetting could be effectively nullified by the time to
# reorder, depending on our data.table dimensions.

# --- There can only be one 'key' at most

# There can be many columns that make up the key, 
# but ultimately only one key that makes up the ordering.
# Any re-setting of the key requires that the whole data.table be reorderd:

# # don't run
# setkey(flights, dest)
# flights[.('LAX')]

# --- Secondary indices can be reused 
# Since there can be multiple secondary indices, and creating an index is 
# as simple as storing the order vector as an attribute, this allows us 
# to even eliminate the time to recompute the order vector if an index 
# already exists.

# --- The new `on` argument allows for cleaner syntax and automatic creation
#     and reuse of secondary indices
# There are several advantages to using the `on` argument. It:
#   1. enables subsetting by computing secondary indices on the fly;
#      this eliminates having to `setindex()` every time;
#   2. allows easy reuse of exisiting indices by just checking the attributes;
#   3. allows for cleaner syntax by having the columns on which to subset
#      is performed as part of the syntax; this makes the code easier to
#      follow when looking at it at a later point.
#
# Note that the `on` arg can also be used on keyed subsets as well; in fact,
# providing the `on` arg is encouraged even when subsetting by keys for
# better readability.


# 2. Fast subsetting using `on` argument and secondary indices ----

# `on` agurment builds secondary indices on the fly... automagically.

# _ a) Fast subsets on 'i' ----

# --- Subset all rows where the origin airport matches 'JFK' using `on`
key(flights)
indices(flights)
setindex(flights, NULL)
flights['JFK', on = 'origin'] 
   # origin indexing happens on the fly (automagically)
# flights[.('JFK'), on = 'origin'] # same as above
indices(flights)

# If we'd already created the secondary index 'origin', the index subsetting
# would use that instead of (re)computing it.
flights[.('JFK'), on = 'origin', verbose = TRUE][1:5] 
# Verbose print out: "Calculated ad hoc index in 0.006sec"
setindex(flights, origin)
flights[.('JFK'), on = 'origin', verbose = TRUE][1:5]
# Verbose print out: "on= matches existing index, using index"

# --- How can I subset on 'origin' and 'dest' columns?
flights[.('JFK', 'LAX'), on = c('origin', 'dest'), verbose = TRUE][1:5]

# Since the time to compute the secondary index is quite small, we don’t 
# have to use `setindex()``, unless, once again, the task involves repeated 
# subsetting on the same column.


# _ b) Select in 'j' ----

# --- Return 'arr_delay' column alone as a data.table corresponding to 
#     'origin' = 'LGA' and 'dest' = 'TPA'
flights[.('LGA', 'TPA'), .(arr_delay), on = c('origin', 'dest')]


# _ c) Chaining ----

# --- On the result obtained above, use chaining to order the column
#     in decreasing order
flights[.('LGA', 'TPA'), .(arr_delay), on = c('origin', 'dest')
        ][order(-arr_delay)
          ]

# _ d) Compute or do in 'j' ----

# --- Find the maximum arrival delay corresponding to 'origin' = 'LGA' and
#     'dest' = 'TPA'
flights[.('LGA', 'TPA'), max(arr_delay, na.rm = TRUE), on = c('origin', 'dest')]


# _ e) Sub-assign by reference using := in 'j' ----

# get all 'hour' values in `flights`
flights[, sort(unique(hour))]

# # Previously we've done the following:
# flights[hour == 24L, hour := 0L] # vector subsetting + reference semantics
# flights[.(24L), hour := 0L]      # key subsetting + reference semantics

# Now we can combine fastest techniques known so far:
                                 # index subsetting + reference semantics
flights[.(24L), hour := 0L, on = c('hour')]
flights[, sort(unique(hour))] # it worked


# _ f) Aggregation using `by` ----

# --- Get the maximum departure delay for each 'month' corresponding to
#     'origin' = 'JFK'; order the result by 'month'
flights[.('JFK'), 
        .(max_dep_delay = max(dep_delay, na.rm = TRUE)), 
        keyby = .(month), 
        on = c('origin')]


# _ g) The `mult` argument ----

# --- Subset only the first matching row where 'dest' matches 'BOS' and 'DAY
flights[c('BOS', 'DAY'), , on = c('dest'), mult = 'first']

# --- Subset only the last matching row where 'origin' matches 
#     'LGA', 'JFK', 'EWR' and 'dest' matches 'XNA'
flights[.(c('LGA', 'JFK', 'EWR'), 'XNA'),  # i
        ,                                  # j
        on = c('origin', 'dest'),          # on
        mult = 'last']                     # mult


# _ h) The `nomatch` argument ----

# --- From the previous example, subset all rows only if there's a match
flights[.(c('LGA', 'JFK', 'EWR'), 'XNA'),  # i
        ,                                  # j
        on = c('origin', 'dest'),          # on
        mult = 'last',                     # mult
        nomatch = 0L]                      # nomatch


# 3. Auto indexing ----

# First we learned about fast subsetting using keys (reordering table in RAM).
# Then we learned about fast subsetting using indices (using index vectors).

# Auto indexing:
#   1. currently only implemented for `==` and `%in%` operators
#   2. auto indices are automatically created and saved as attributes

# Create a bit data.table
set.seed(1L)
dt = data.table(x = sample(1e5L, 1e7L, TRUE), y = runif(100L))
print(object.size(dt), units = 'Mb')

# When we use `==` or `%in%` on a single column for the first time, 
# a secondary index is created automatically. And that secondary index
# is used to perform the subset.

names(attributes(dt))

# Run the first time
(t1 <- system.time(ans <- dt[x == 989L]))
head(ans)

# secondary index has been created on 'x' after using `==` above
names(attributes(dt))
indices(dt)

# Successive subsetting is now faster thanks to auto indexing
(t2 <- system.time(dt[x == 989L]))
(t3 <- system.time(dt[x %in% 1989:2012]))

# Auto indexing can be disabled by setting the global argument:
# options(datatable.auto.index = FALSE)

# All indexing can be disabled fully by setting this global argument:
# options(datatable.use.index = FALSE)

















