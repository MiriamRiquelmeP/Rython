library(collapsibleTree)
data()
summary(Geography)
collapsibleTree(warpbreaks, c("tension","wool","breaks"), tooltip = "tension")
warpbreaks

