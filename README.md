# Brain-Body Size Allometries Differ with Parental Care Across Vertebrates
This repository contains all materials relating to The connection between Brain Size and Patenal Care and Care Complexity in Vertebrates.
The publication is based on two seperate comprehensive databases, one inculding the brain size data for nearly 6500 vertebrate species (at the time of this publication).

## About
This study includes several analysis on the connection between brain size and parental care complexity. We used several statistical and visual methods in this study.
We studied larger classes of vertebrates seperatly and at once. In this study only Absolute Brain Mass and Relative Brain Mass were used.

## Example Codes
To visualize the connections we used `ggplot2` and `ggthemes` in all vertebrate classes.

```ruby
# Relative Brain Mass and Male Juvenile Care

ggplot(r.df,aes(x=Male_Juv_Care,y=log(Brain_Mass_g/Body_Mass_g)))
+geom_boxplot(size = 1, alpha = .2,aes(fill=r.df$Male_Juv_Care,color=r.df$Male_Juv_Care))
+theme_clean()
+scale_fill_manual(values=c("lightblue1","plum"))
+labs(x="Male Juvenile Care",y=expression("Brain Mass/Body Mass"^"Log10"))
+labs(color="Male Juvenile Care")+labs(fill="Male Juvenile Care")
+scale_color_manual(values=c("lightblue1","plum"))
```

To visualize our data on phylogenetic scale, we used the `caper`, `ggtree`, `ape` and `diversitree` R packages.

```ruby
# Phylogenetic tree for amphibians

ampbodyb <- subset(amp2,select=c(Species,Body_Mass,Brain_Mass))
ampbodyb$Brain_Mass=as.character(ampbodyb$Brain_Mass)
ampbodyb$Brain_Mass=as.numeric(ampbodyb$Brain_Mass)
ampbodybrain <- ampbodyb
ampbodybrain$divide=ampbodyb$Brain_Mass/ampbodyb$Body_Mass
ampbodybrain=subset(ampbodybrain,select=c(Species,divide))
ampbodybrain
ampbb <- ampbodybrain[complete.cases(ampbodybrain),]
ampbb.drop <- amp.lab[!amp.lab %in% ampbb$Species]
(ampbb.tre2=drop.tip(a.tre,ampbb.drop))
(ampbb.lab2=ampbb.tre2$tip.label)
(ampbb.sp=unique(ampbb$Species))
ampbb.t <- table(ampbb.lab2)
ampbb.t[ampbb.t>1]
ampbb.tsp <- table(ampbb.sp)
ampbb.tsp[ampbb.tsp>1]
(ampbb.tree=ampbb.sp[ampbb.sp %in% ampbb.lab2])
(ampbb.not=ampbb.sp[!(ampbb.sp %in% ampbb.lab2)])
(ampbb.not=ampbb.not[order(ampbb.not)])
ampbb3 <- ampbb[ampbb$Species %in% ampbb.tree,]
ampbb4 <- ampbb3[!duplicated(ampbb3),]
(ampbb.t=table(ampbb4$Species))
ampbb.t[ampbb.t>1]
ampbb.tre5 <- compute.brlen(ampbb.tre2,method="Grafen",power=0.5)
ampbb4.3 <- as.matrix(ampbb4)
row.names(ampbb4.3)=ampbb4$Species
ampbb4.3 <- as.data.frame(ampbb4.3)

#Write it out into a csv file

ampbodybraintree<-read.csv("AmpBodyBrainTree.csv",header=TRUE,row.names=1)
ampbodybraintree<-setNames(ampbodybraintree[,1],rownames(ampbodybraintree))
obj.ampbodybrain<-contMap(ampbb.tre5,ampbodybraintree,plot=FALSE)
obj.ampbodybrain<-setMap(obj.ampbodybrain,invert=TRUE)
plot(obj.ampbodybrain,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="Brain/Body Mass")
plot(obj.ampbodybrain,type="fan",outline=FALSE,fsize=c(0.6,1),legend=1,lwd=c(4,8),xlim=c(-
1.8,1.8),leg.txt="Brain/Body Mass")
bbtreeplot <- function(){plot(obj.ampbodybrain,fsize=c(0.4,1),outline=FALSE,lwd=c(3,7),leg.txt="Brain/Body 
Mass")}
bbfanplot <- function(){plot(obj.ampbodybrain,type="fan",outline=FALSE,fsize=c(0.6,1),legend=1,lwd=c(4,8),xli
m=c(-1.8,1.8),leg.txt="Brain/Body Mass")}
```

To visualize parental care complexity on the phylogeny we used `diversitree` R package.

```ruby
# Amphibian Care Complexity on Tree

r.df <- read_excel("Amphibians_brain_(R) - For Tree.xlsx")
r.df$Nest_Building_Comp_M  <- as.character(r.df$Nest_Building_Comp_M)
r.df$Nest_Building_Comp_F  <- as.character(r.df$Nest_Building_Comp_F)
r.df$Egg_Care_Comp_M  <- as.character(r.df$Egg_Care_Comp_M )
r.df$Egg_Care_Comp_F <- as.character(r.df$Egg_Care_Comp_F)
r.df$Juv_Care_Comp_M  <- as.character(r.df$Juv_Care_Comp_M )
r.df$Juv_Care_Comp_F <- as.character(r.df$Juv_Care_Comp_F)

r.df$Nest_Building_Comp_M  <- as.factor(r.df$Nest_Building_Comp_M)
r.df$Nest_Building_Comp_F  <- as.factor(r.df$Nest_Building_Comp_F)
r.df$Egg_Care_Comp_M  <- as.factor(r.df$Egg_Care_Comp_M )
r.df$Egg_Care_Comp_F <- as.factor(r.df$Egg_Care_Comp_F)
r.df$Juv_Care_Comp_M  <- as.factor(r.df$Juv_Care_Comp_M )
r.df$Juv_Care_Comp_F <- as.factor(r.df$Juv_Care_Comp_F)

r2.df<-r.df

r3.df=subset(r2.df, select= c(Species, Nest_Building_Comp_M, Nest_Building_Comp_F, Egg_Care_Comp_M,
                              Egg_Care_Comp_F, Juv_Care_Comp_M, Juv_Care_Comp_F))

(a.r.lab <- a.tre$tip.label)
(t.r <- table(a.r.lab))
t.r[t.r>1]
a.r.lab[order(a.r.lab)]
attributes(a.tre)

#drop missing species
drop.r<-a.r.lab[!a.r.lab %in% r3.df$Species]
(a.r.tre2 <- drop.tip(a.tre, drop.r))

#remove missing species from data frame
(a.r2.lab <- a.r.tre2$tip.label)

(sp.r <- unique(r3.df$Species))
t.r <- table(a.r2.lab)
t.r[t.r>1]
t.r.sp <- table(sp.r)
t.r.sp[t.r.sp>1]
(intree.r <- sp.r[sp.r %in% a.r2.lab])
(notintree.r <- sp.r[!(sp.r %in% a.r2.lab)])
(notintree.r <- notintree.r[order(notintree.r)])
r4.df <- r3.df[r3.df$Species %in% intree.r,]

r4.df$Nest_Building_Comp_M  <- as.character(r4.df$Nest_Building_Comp_M)
r4.df$Nest_Building_Comp_F  <- as.character(r4.df$Nest_Building_Comp_F)
r4.df$Egg_Care_Comp_M  <- as.character(r4.df$Egg_Care_Comp_M )
r4.df$Egg_Care_Comp_F <- as.character(r4.df$Egg_Care_Comp_F)
r4.df$Juv_Care_Comp_M  <- as.character(r4.df$Juv_Care_Comp_M )
r4.df$Juv_Care_Comp_F <- as.character(r4.df$Juv_Care_Comp_F)

r4.df$Nest_Building_Comp_M  <- as.numeric(r4.df$Nest_Building_Comp_M)
r4.df$Nest_Building_Comp_F  <- as.numeric(r4.df$Nest_Building_Comp_F)
r4.df$Egg_Care_Comp_M  <- as.numeric(r4.df$Egg_Care_Comp_M )
r4.df$Egg_Care_Comp_F <- as.numeric(r4.df$Egg_Care_Comp_F)
r4.df$Juv_Care_Comp_M  <- as.numeric(r4.df$Juv_Care_Comp_M )
r4.df$Juv_Care_Comp_F <- as.numeric(r4.df$Juv_Care_Comp_F)

#remove duplicates
(t.r <- table(r4.df$Species))
t.r[t.r>1]
r.df <- r.df[!r.df$Species %in% names(t.r[t.r>1]),]
names(r.df)

#branch length 
a.r.tre5 <- compute.brlen(a.r.tre2, method = "Grafen", power = 0.5)

row.names(r4.df) <- r4.df$Species


col.r2.tree <- list(Nest_Building_Comp_M =c("lightblue1","cyan"),
                  Egg_Care_Comp_M =c("lightblue1","lightskyblue","lightseagreen"),
                  Juv_Care_Comp_M =c("lightblue1","dodgerblue","deepskyblue4"), 
                  
                  Nest_Building_Comp_F =c("plum1","hotpink"),
                  Egg_Care_Comp_F=c("plum1","orange","tomato","indianred2","indianred3","maroon"),
                  Juv_Care_Comp_F=c("plum1","firebrick1","red","firebrick","darkred"))

#(clade.r <- sapply(a.r.tre5$tip.label, function(x) r4.df[r4.df$Species==x,]$Family))

trait.plot(a.r.tre5, dat=r4.df, 
           #class = as.character(clade.r), 
           cols=col.r2.tree, quiet=TRUE, w=1/15, margin=1/1.4,
           cex.lab = 0.001, cex.legend = 0.001)
```

To analyse our data we mostly used PGLS.

```ruby
# PGLS for Amphibian Relative Brain Mass and Egg Care in Females

cdat1 <- comparative.data(data = r4.2.df, phy = a.r.tre5, names.col = "Species", na.omit = FALSE)

mod1 <- pgls(log(Brain_Mass/Body_Mass)~Egg_Care_Comp_F, data = cdat1, lambda = "ML",  bounds = list( lambda = c(0.01, 0.99)))
  summary(mod1)
  anova.pgls.fixed(mod1)
```
```ruby
# PGLS for Amphibian Brain Mass and Nest Building in Males

pglmm_compare(log(Brain_Mass)~Nest_Building_Comp_M,
  family = "gaussian",
  data = r4.2.df,
  phy=a.r.tre5,
  REML = TRUE,
  optimizer = c("nelder-mead-nlopt", "bobyqa", "Nelder-Mead", "subplex"),
  add.obs.re = TRUE,
  verbose = FALSE,
  cpp = TRUE,
  bayes = FALSE,
  reltol = 10^-6,
  maxit = 500,
  tol.pql = 10^-6,
  maxit.pql = 200,
  marginal.summ = "mean", calc.DIC = FALSE,
  prior = "inla.default",
  prior_alpha = 0.1, prior_mu = 1,
  ML.init = FALSE, s2.init = 1, B.init = NULL)
```

## [Figures folder](Figures)
- this folder has PNG files which contain the figures that were shown in the supplementary files
- these figures were made with the R scrips shown above, and later expanded with original drawings by Mihály Mándi using [GIMP](https://www.gimp.org/)
- the figure were made with `ggplot2` and `ggthemes` R packages

## [Scripts folder](Scripts)
- this folder contains some of our R scripts, that we used in this study
- all scripts are in a separate file
- the scripts in the folder are examples, these codes were used in every class
- the sripts use several R packages like: `ggplot2`,`ggthemes`,`caper`,`phylolm`,`ape` and `diversitree`

## [Tables folder](Tables)
- this folder has several excel files, that were used in the original file and the supplements

## [Stats folder](Stats)
- this folder has several statistical results used in this study

## Database
The original dataset for Brain Size is in a seperate data paper for VerteBrainData.



