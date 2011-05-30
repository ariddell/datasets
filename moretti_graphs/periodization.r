###############################################################################
# Reproducing Figure 9 (page 19 in Graphs, Maps, Trees) from Moretti's data.
# Alternate version highlights possible clusters, based on the description on
# p. 18. 
###############################################################################

### Load data
# british_novelistic_genres_1740_1900.csv is from
# https://github.com/ariddell/datasets/raw/master/moretti_graphs/british_novelistic_genres_1740_1900.csv
genres = read.csv("datasets/moretti_graphs/british_novelistic_genres_1740_1900.csv")

### Reorder cluster_guess (Riddell's guess based on p. 18 GMT)
cluster_levels = c("late 1760s", "early 1790s", "late 1820s", "1850", "early 1870s", "Mid-late 1880s")
genres$cluster_guess = factor(genres$cluster_guess, levels=rev(cluster_levels))

### Load ggplot2 for graphing
library(ggplot2)

### Set up the graph
rect_h = 1.7
minyear = min(genres$start)
maxyear = max(genres$end)

ggplot(data = genres) +
layer(
 mapping = aes(
 fill = cluster_guess,
 xmin = start,
 xmax = end,
 ymin = order(start) + 1:nrow(genres),
 ymax = order(start) + 1:nrow(genres) + rect_h),
 geom = "rect"
) +
scale_fill_brewer("Cluster") +
layer(
 mapping = aes(
 label = genre_name,
 size=3, # smaller font, there's a better way to do this, I think
 vjust=0,
 hjust=1,
 x=start - 0.5,
 y=order(start) + 1:nrow(genres) + rect_h/6),
 geom = "text"
) +
scale_size(legend=FALSE) +
scale_x_continuous("",limits=c(minyear-30,maxyear)) +
scale_y_continuous("") +
opts(axis.title.y = theme_blank(), axis.text.y =  theme_blank())


###############################################################################
# Histogram of last digits
###############################################################################

lastdigits = as.integer(substr(as.character(genres$end),4,4))
qplot(lastdigits,geom="bar", xlab="last digit", main="Periodization ending year final digit")

###############################################################################
# Construct periodizations of the silver fork and gothic novels by identifying
# the period during which 95% of all novels in the category were published
# (the period must also include the peak publication years). Hopefully there
# will only be one such period for each genre.
###############################################################################

library(TeachingDemos) # for emp.hpd()

# gothic novels

gothic = read.csv2("datasets/gothic/maurice_lévy_le_roman_gothique_anglais_utf8.csv",header=T,sep="\t", stringsAsFactors=FALSE)
gothic$YEAR = as.integer(gothic$YEAR)
gothic$YEAR[is.na(gothic$YEAR)] = c(1797,1783,1803) # replace multi year [1] "1797-1805" "1783-1785" "1803-1804"
gothic_hpd_start = emp.hpd(gothic$YEAR,conf=.95)[1] + 1 # adjustment gets us slightly closer to 95%
gothic_hpd_end = emp.hpd(gothic$YEAR,conf=.95)[2]
sum(gothic$YEAR >= gothic_hpd_start & gothic$YEAR <= gothic_hpd_end) / length(gothic$YEAR)

ggplot(gothic,aes(x=YEAR)) + 
  geom_histogram(binwidth=1) +
  labs(x="Year", y="Novels published") +
  # NB: adjustment of geom_vline start so it includes the relevant year
  geom_vline(xintercept=c(gothic_hpd_start-1,gothic_hpd_end),color="red") + opts(title="95% period for gothic novels (n = 394)")

# silver fork novels

silverfork = read.csv2("datasets/silverfork/silver_fork_utf8.csv",header=T,sep="\t", stringsAsFactors=FALSE)
silverfork$YEAR = as.integer(silverfork$YEAR)
silverfork$YEAR[is.na(silverfork$YEAR)] = 1826 # "1826-7" gets turned into an NA
silverfork_hpd_start = emp.hpd(silverfork$YEAR,conf=.95)[1] + 2 # adjustment since we include the relevant year
silverfork_hpd_end = emp.hpd(silverfork$YEAR,conf=.95)[2]
# verify that this really is a 95% interval
sum(silverfork$YEAR >= silverfork_hpd_start & silverfork$YEAR <= silverfork_hpd_end) / length(silverfork$YEAR)

ggplot(silverfork,aes(x=YEAR)) + 
  geom_histogram(binwidth=1) +
  labs(x="Year", y="Novels published") +
  # NB: adjustment of geom_vline start so it includes the relevant year
  geom_vline(xintercept=c(silverfork_hpd_start-1,silverfork_hpd_end),color="red") + opts(title="95% period for silver fork novels (n = 99)")

###############################################################################
# Reproducing Cosma Shalizi simulated likelihood test with adjustments
# For the specifics of Shalizi's test see
# http://www.thevalve.org/go/valve/article/graphs_trees_materialism_fishing/
###############################################################################

genres <- read.csv("datasets/moretti_graphs/british_novelistic_genres_1740_1900.csv")
n = length(genres$start)
interarrivals = genres$start[2:n] - genres$start[1:n-1]
stopifnot(signif(mean(interarrivals),3) == 3.44)
theta = 1/mean(interarrivals)
log_likelihood_moretti = sum(dgeom(interarrivals,theta,log=T)) # works, I get Shalizi's result -103.9498

