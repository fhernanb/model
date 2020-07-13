\dontrun{
# Using a table object
tbl1 <- with(Cars93, table(Type, AirBags))
plot_prof(x=tbl1, Row=TRUE)
plot_prof(x=tbl1, Row=FALSE, cex.names=0.9, cex=0.6)

# Using a xtabs object
tbl2 <- with(Cars93, xtabs(~ Type + AirBags))
plot_prof(x=tbl2, Row=TRUE, cex.names=1.8)
}