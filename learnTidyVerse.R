library(dslabs)
library(tidyverse)
library(readxl)


# mutate, select & filter
murders %>% mutate(rate=total/population*1e5, 
                   rank = rank(-rate)) %>% filter(rate < 1 & 
                                                    (region %in% c('West', 'Northeast'))) %>% select(state, rate, rank)
# group_by, summarize & pull
murders %>% group_by(region) %>% summarize(rate=sum(total) / sum(population) * 1e5) %>% pull(rate)

# group_by & do, case_when
my_summary <- function(data){
  x = quantile(data$population, c(0, 0.5, 1))
  tibble(min=x[1], median=x[2], max= x[3])
}
murders %>% mutate(group=case_when(abb %in% c('NH', 'VT', 'MA', 'CT', 'RI') ~ 'New England',
                                   abb %in% c('CA', 'OR', 'WA') ~ 'West Coast',
                                   region == 'South' ~ 'South',
                                   TRUE ~ 'Other')) %>% group_by(group) %>% do(my_summary(.))

# map_df: tibble; map: list; map_dbl: vector
my_sum <- function(n){
  tibble(n=n, n_s=sum(1:n), n_s_2=sum((1:n)^2))
}
map_df(1:100, my_sum)

# learn import -- file.path(dir, fileName), file.copy(orig, dest), system.file(package=''), list.files(dir)
file.copy(file.path(system.file('extdata', package='dslabs'), 'murders.csv'), 'murders.csv')
dat <- read_csv('murders.csv')
view(dat)
dat2 <- read_excel(file.path(system.file('extdata', package='dslabs'), '2010_bigfive_regents.xls'), sheet='Sheet1')
view(dat2)
excel_sheets(file.path(system.file('extdata', package='dslabs'), '2010_bigfive_regents.xls'))

