cdat1 <- comparative.data(data = r4.2.df, phy = a.r.tre5, names.col = "Species", na.omit = FALSE)

mod1 <- pgls(log(Brain_Mass/Body_Mass)~Egg_Care_Comp_F, data = cdat1, lambda = "ML",  bounds = list( lambda = c(0.01, 0.99)))
  summary(mod1)
  anova.pgls.fixed(mod1)
