# data_table_workshop.R

# Load packages
# lapply(c('tidyverse', 'dbplyr', 'Lahman', 'data.table'), install.packages)
library(tidyverse) # dplyr & magrittr & tidyr
library(dbplyr)
library(Lahman)
library(data.table)

# Create a local SQLlite database of the Lahman data:
lahman <- lahman_sqlite()

# Copy the batting table to memory as a tibble:
batting_tbl <- lahman %>% 
  tbl('BATTING') %>% 
  collect()
class(batting_tbl)

# Convert the copy in memory to a data.table:
batting_dt <- as.data.table(batting_tbl)
class(batting_dt)


# Select with `j`

# data.table
names(batting_dt)
batting_dt[, .(playerID, yearID, league = lgID, stint)]
# .() is an alias for list()
batting_dt[, list(playerID, yearID, league = lgID, stint)] # same as above


# Compute in `j`

# data.table
batting_dt[, .(playerID, H, AB)]
batting_dt[, .(playerID, avg = H / AB)]


# Aggregate in `j`

# data.table: aggregate using a valid R expression
batting_dt[, .(HBP)]
batting_dt[, .(max_HBP = max(HBP, na.rm=TRUE))]


# Grouping with `by`

# data.table: by
#batting_dt[ , .(avg = sum(H) / sum(AB)), by = .(playerID, yearID, lgID)]
batting_dt[ , .(avg = sum(H) / sum(AB)), .(playerID, yearID, lgID)]

# data.table: keyby
# keyby orders records by groups specified
# Here the parameter name is required
batting_dt[ , .(avg = sum(H) / sum(AB)), keyby = .(playerID, yearID, lgID)]


# Select columns in `i`

# data.table
# Logical indexing
batting_dt[ yearID == 2016, .(playerID, HBP)]

# data.table
# indexing with keys -- faster way of finding records
setkey(batting_dt, 'teamID') # make keys using `teamID`
batting_dt['DET', .(playerID, teamID, HBP)]

key(batting_dt)

setkey(batting_dt, 'yearID')
batting_dt[.(2016), .(playerID, yearID, HBP)]

# Compare the difference
batting_dt[2016, .(playerID, yearID, HBP)] # passes the 2016th row!

# Remove key for later examples
setkey(batting_dt, NULL)


# Chaining

# data.table
# pipes
batting_dt[ yearID > 2000, .(HR = sum(HR)), .(playerID)] %>%
  .[HR > 400]
batting_dt[ yearID > 2000, .(HR = sum(HR)), .(playerID)] %>%
  `[`(HR > 400)

# dplyr equivalent
# dplyr: Use %>% to chain
batting_tbl %>% 
  filter(yearID > 2000) %>%
  group_by(playerID) %>%
  summarize( HR = sum(HR) ) %>%
  ## Here's the pipe from above
  filter( HR > 400)

# data.table
# chaining
batting_dt[ yearID > 2000, .(HR = sum(HR)), .(playerID)][HR > 400]


# Order in `i`

# data.table
batting_dt[ yearID > 2000, .(HR = sum(HR)), .(playerID)][HR > 400][order(-HR)]

batting_dt[ yearID > 2000, .(HR = sum(HR)), .(playerID)] %>% 
  .[HR > 400] %>% 
  .[order(-HR)] # `order` is from data.table, not base R: ?data.table::order


# Using `.N`

# data.table -- multiple stints
batting_dt[yearID == 2016 & AB > 99, .N, .(playerID)][N>1]

# dplyr: n()
batting_tbl %>% 
  filter(AB > 99 & yearID == 2016) %>%
  # filter(yearID == 2016) %>%
  group_by(playerID) %>%
  summarize(N = n()) %>%
  filter(N > 1)

# data.table
batting_dt[yearID > 2000 , .(SB = sum(SB), HR = sum(HR)), .(playerID, yearID)] 
batting_dt[yearID > 2000 , .(SB = sum(SB), HR = sum(HR)), .(playerID, yearID)] %>%
  .[SB > 19 & HR > 19]
batting_dt[yearID > 2000 , .(SB = sum(SB), HR = sum(HR)), .(playerID, yearID)] %>%
  .[SB > 19 & HR > 19] %>% .[ , .N, yearID]


# Exercises

# 1
# dplyr
batting_tbl %>%
  filter(yearID == 2016 & teamID == 'DET' & AB > 100) %>%
  transmute(playerID, avg = H / AB)
# data.table
batting_dt[yearID == 2016 & teamID == 'DET' & AB > 100, .(playerID, avg = H / AB)]

# 2
# dplyr
batting_tbl %>%
  filter(yearID == 2016 & teamID == 'DET' & AB > 100) %>%
  transmute(playerID, avg = H / AB) %>%
  filter(avg == max(avg))
# data.table
batting_dt[yearID == 2016 & teamID == 'DET' & AB > 100, .(playerID, avg = H / AB)] %>% 
  .[avg == max(avg)]

# 3
# dplyr
batting_tbl %>%
  filter(yearID %in% 2001:2010) %>%
  group_by(playerID) %>%
  summarize(HR = sum(HR)) %>%
  filter(HR == max(HR))
# data.table
batting_dt[yearID %in% 2001:2010, .(HR = sum(HR)), by = .(playerID)] %>% 
  .[HR == max(HR)]

# 4
# dplyr
batting_tbl %>%
  filter(yearID %in% 2016) %>%
  group_by(playerID) %>%
  summarize(H = sum(H)) %>%
  filter(H > 199) %>%
  arrange( desc(H) )
# data.table
batting_dt[yearID %in% 2016, .(H = sum(H)), by = .(playerID)] %>% 
  .[H > 199] %>% 
  .[order(-H)]

# 5
# dplyr
batting_tbl %>%
  filter(yearID > 1999) %>%
  group_by(yearID, playerID) %>%
  summarize(H = sum(H)) %>% # still grouped by yearIDs
  filter(H > 199) %>%       # or just summarize( n = sum(H > 199))
  summarize( n = n() ) %>%
  arrange( desc(yearID) )
# data.table
batting_dt[yearID > 1999, .(H = sum(H)), by = .(yearID, playerID)] %>% 
  .[H > 199, .N, yearID] %>% .[order(-yearID)]
# data.table parsed up
batting_dt[yearID > 1999,,] %>%                     # filter
  .[, .(H = sum(H)), by = .(yearID, playerID)] %>%  # summarize + group_by
  .[H > 199,,] %>%                                  # filter
  .[, .N, by = yearID] %>%                          # summarize w/ `n()`
  .[order(-yearID),,]                               # arrange



# Reference semantics

# R -- copy on modify semantics...
# R copies an entire data.frame if changing even one element ... !!!
df = data.frame(x = 1:10, y = 21:30)
tracemem(df$x)
# tracemem(df$y)
df$x[1] <- 0

n = 1e7
X = data.table( id = sample(LETTERS, n, replace = TRUE), value = rnorm(n) )
tracemem(X)
tracemem(X$value)

X
X[1, value := 0] # `:=` is "modify by reference operator"
tracemem(X$value)
# equivalent syntx
X[1, `:=`(value = 1, id = 'new')]
tracemem(X$value)
X

tracemem(df)
df_new = df
tracemem(df_new)
df$id = 1 # base R copies df to new location once it's modified
tracemem(df)
tracemem(df_new)

Y = X
tracemem(Y)
tracemem(X)
tracemem(Y$id)
tracemem(X$id)

Z = copy(X)
tracemem(X)
tracemem(Z)
tracemem(X$id)
tracemem(Z$id)

X[, new_value := value^2]
tracemem(X) # didn't copy data.table before modifying
names(X)
names(Y) # Y is updated because pointer to Y and X are the same
names(Z)


# data.table scoping

# data.table package: ---------------------------------------------------------
library(data.table)
rm( list = ls() )

# Example data.tables: --------------------------------------------------------
X = data.table(x = rep(c("a", "b"), c(2, 3)), y = 1:5)
Y = data.table(z = rep(c(TRUE, FALSE), c(2, 3)), x = rep('yy', 5), y = 0:4 )

# Joins: --------------------------------------------------------------------

# Use merge
?merge
merge(X, Y, by = 'y') # Inner
merge(X, Y, by = 'y', all.x = TRUE) # Left
merge(X, Y, by = 'y', all.y = TRUE) # Right
merge(X, Y, by = 'y', all.x = TRUE, all.y = TRUE) # Outer

## Construct an environment to compute in "j" using columns from two data.tables
# Left join
X[Y, .(y, x, z), on = 'y']
dplyr::left_join(X, Y, by = 'y')
# Right join
Y[X, .(y, x, z), on = 'y']

# Inner join
X[Y, .(y, x, z), on = 'y', nomatch = 0L]
Y[X, .(y, x, z), on = 'y', nomatch = 0L]

# Scoping rules for joins: ----------------------------------------------------
m = 'global'; n = 'global'
list1 = list( m = 'list1', n = 'list1')
list2 = list( m = 'list2', n = 'list2')

setkey(X, 'y')
setkey(Y, 'y')

X[Y, .(x), verbose=TRUE] 
Y[X, .(x)] # `on` uses data.table keys by default when they're defined

X[, .(m)]

f = function(X){
  m = 'm_from_f'
  X[ , .(m, n)]
}

with(list1, 
     within(list2, {f = f(X)})
)


# SD ... "subsets of data"

batting_dt <- as.data.table(Batting)
names(batting_dt)
batting_dt[, .(SO = sum(SO), RBI = sum(RBI), R = sum(R))]
batting_dt[yearID > 2000, lapply(.SD, sum), .SDcols = c('SO', 'R', 'HR')]
batting_dt[yearID > 2000, lapply(.SD, sum), by = yearID, .SDcols = c('SO', 'R', 'HR')]
batting_dt[yearID > 2000, lapply(.SD, sum), by = playerID, .SDcols = c('SO', 'R', 'HR')]



# dplyr
batting_tbl %>% 
  filter(playerID == 'trumbma01') %>% 
  group_by(yearID) %>% 
  mutate(Total_G = sum(G), Total_HR = sum(HR)) %>% 
  transmute(teamID, Total_G, Total_HR, HRperG = Total_HR / Total_G)
  
# data.table
batting_dt[playerID == 'trumbma01',,] %>% 
  .[, .(G, HR, HRperG = HR / G), by = .(playerID, yearID)]



























