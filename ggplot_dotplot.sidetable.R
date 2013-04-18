##### Adapted from an example written by Matt Cooper
## Original version: 
##   http://mcfromnz.wordpress.com/2012/11/06/forest-plots-in-r-ggplot-with-side-table/

#### STEP 1:  Fit a regression (Example: Owls data from glmmADMB package) ####
## install.packages("glmmADMB", repos="http://r-forge.r-project.org")
library(glmmADMB)

Owls <- transform(Owls,
                  ArrivalTime.c = ArrivalTime - mean(ArrivalTime),
                  Nest = reorder(Nest, NegPerChick),
                  BroodSize.c = BroodSize - mean(BroodSize),
                  NCalls = SiblingNegotiation)

fitted.model1  <- glmmadmb(NCalls ~ (FoodTreatment+ArrivalTime.c)*SexParent + BroodSize.c + (1|Nest),
                           data=Owls,
                           zeroInflation=TRUE,
                           family="nbinom1")

fitted.model2  <- glmmadmb(NCalls ~ (FoodTreatment+ArrivalTime.c)*SexParent + BroodSize.c + (1|Nest),
                           data=Owls,
                           zeroInflation=TRUE,
                           family="nbinom2")


#### STEP 2: Extract the regression estimates from the fitted model ####

## Load utility function
library(devtools)
source_url("https://raw.github.com/davidhuh/plot_templates/master/formatforplot.R")

model1.table <- formatforplot(fitted.model1,
                              model.name="Negative Binomial (NB1)",
                              parm.names=c("Intercept","Food satiated","Arrival time",
                                           "Male parent", "Brood size","Food satiated × Male parent",
                                           "Arrival time × Male parent"))

model2.table <- formatforplot(fitted.model2,
                              model.name="Negative Binomial (NB2)",
                              parm.names=c("Intercept","Food satiated","Arrival time",
                                           "Male parent", "Brood size","Food satiated × Male parent",
                                           "Arrival time × Male parent"))


#### STEP 3: Create a dataset for the dot plot and side table

library(reshape2)

## Create dataset for the side table
sidetable <- rbind(c("NB1 Model","B","SE","p"),
                   model1.table[, c("param","beta.s","se.s","pval.s")],
                   NA,
                   c("NB2 Model","B","SE","p"),
                   model2.table[, c("param","beta.s","se.s","pval.s")])

# Reformat the side table data into long format
sidetable$row <- factor(rev(1:nrow(sidetable)))
sidetable.long <- melt(sidetable, value.name="cell", id="row")
sidetable.long$col <- factor(rep(1:4, each=nrow(sidetable)))

## Create dataset for the dot plot
plotdata <- rbind(NA,
                  model1.table[, c("beta","lower","upper")],
                  NA, NA,
                  model2.table[, c("beta","lower","upper")])

plotdata$row <- factor(rev(1:nrow(plotdata)))



#### STEP 4: Generate dot plot and side table in ggplot2

library(ggplot2)
library(grid)    # enable grid functionality

# utility function to format labels with consistent decimal places
fmt <- function(x) format(x, nsmall=1)

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

dotplot <- ggplot(plotdata, aes(beta, row)) +
  geom_point(size=3, shape=18) +
  geom_errorbarh(aes(xmax = upper, xmin = lower), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks = seq(-1.5,2.5,0.5), labels = fmt(seq(-1.5,2.5,0.5))) +
  labs(x="log(Rate ratio)", y="") +
  coord_cartesian(xlim=c(-1.75,2.75))

sidetable <- ggplot(sidetable.long, aes(x = col, y = row, label = cell)) +
  geom_text(size = 2.5, hjust=1, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(7.5,8.5))) +
  geom_hline(aes(yintercept=c(16.5,17.5))) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.text.x = element_text(colour="white"),#element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(colour="white"),#element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(-1,4.25))


#### STEP 5: Combine dot plot and side table

library(gridExtra)

png("ggplot_dotplot.sidetable.png", height=4, width=7.5, units="in", res=300)

grid.arrange(sidetable, dotplot,
             ncol=2, nrow=1, widths=c(0.6,0.4))

dev.off()
