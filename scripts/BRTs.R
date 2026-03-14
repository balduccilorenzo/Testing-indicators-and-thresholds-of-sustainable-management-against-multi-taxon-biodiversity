# Call library ----
library(tidyverse)
library(gbm)
library(ggBRT)
library(dismo) 
library(cowplot)
library(readxl)

# Call data ----
dataLI <- read_xlsx("data/dataLI.xlsx") #Lichens
dataVP <- read_xlsx("data/dataVP.xlsx") #Vascular Plants
dataDA <- read_xlsx("data/dataDA.xlsx") #Deadwood Arthropods
dataUA <- read_xlsx("data/dataUA.xlsx") #Understory Arthropods

# Data formatting ----
dataLI <- as.data.frame(dataLI)
dataVP <- as.data.frame(dataVP)
dataDA <- as.data.frame(dataDA)
dataUA <- as.data.frame(dataUA)

dataLI$siteID <- as.ordered(dataLI$siteID)
dataVP$siteID <- as.ordered(dataVP$siteID)
dataDA$siteID <- as.ordered(dataDA$siteID)
dataUA$siteID <- as.ordered(dataUA$siteID)

## 1. BRTs ----

### 1.1 Lichens ----

# 1.  tree.complexity = 2, bag.fraction = 0.75
set.seed(123)
br.LI.1 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),            # predictors
                    gbm.y = 1,                # response variables
                    family = "gaussian",       # distribution
                    tree.complexity = 2,      # tree complexity or number of nodes
                    # less complex trees bring to a greater number of produced trees, 
                    # a large number of trees usually means a good prediction
                    learning.rate = 0.0025,     # the contribution of each tree
                    ## if I decrease the lr i should increase the tree complexity to have the same number of trees
                    # low learning rates bring to a greater number of produced trees
                    bag.fraction = 0.75)       # stochasticity index:
#portion of data to select each time to build the trees 
#(0.5 means that at each interaction, 50% of the data is randomly selected)

#number of trees
br.LI.1$n.trees

# 2. tree.complexity = 3; bag.fraction = 0.75
set.seed(123)
br.LI.2 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),        
                    gbm.y = 1,               
                    family = "gaussian",      
                    tree.complexity = 3,     
                    learning.rate = 0.0025,     
                    bag.fraction = 0.75)

#number of trees
br.LI.2$n.trees


# 3. imposto tree.complexity = 5, bag.fraction = 0.75
set.seed(123)
br.LI.3 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),            # variabili esplicative
                    gbm.y = 1,                # varibaile risposta
                    family = "gaussian",     
                    tree.complexity = 5,     
                    learning.rate = 0.0025,     
                    bag.fraction = 0.75)

#number of trees
br.LI.3$n.trees

# 4. tree.complexity = 2, bag.fraction = 0.5
set.seed(123)
br.LI.4 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),           
                    gbm.y = 1,               
                    family = "gaussian",       
                    tree.complexity = 2,      
                    learning.rate = 0.0025,   
                    bag.fraction = 0.5)   

#number of trees
br.LI.4$n.trees

# 5. tree.complexity = 3, bag.fraction = 0.5
set.seed(123)
br.LI.5 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),            
                    gbm.y = 1,                
                    family = "gaussian",       
                    tree.complexity = 3,      
                    learning.rate = 0.0025,     
                    bag.fraction = 0.5)       
#number of trees
br.LI.5$n.trees

# 6. tree.complexity = 6, bag.fraction = 0.5
set.seed(123)
br.LI.6 <- gbm.step(data=dataLI, 
                    gbm.x = c(2:12),          
                    gbm.y = 1,               
                    family = "gaussian",       
                    tree.complexity = 3,      
                    learning.rate = 0.0025,   
                    bag.fraction = 0.5)     
#number of trees
br.LI.6$n.trees


# model comparisons (best is highest correlation mean)
res.cv.corr <- data.frame(br.LI.1$cv.statistics$correlation.mean,
                          br.LI.2$cv.statistics$correlation.mean,
                          br.LI.3$cv.statistics$correlation.mean,
                          br.LI.4$cv.statistics$correlation.mean,
                          br.LI.5$cv.statistics$correlation.mean,
                          br.LI.6$cv.statistics$correlation.mean)

res.cv.corr <- as.data.frame(t(res.cv.corr))
# the best is 2


#model properties
br.LI.2$n.trees
br.LI.2$self.statistics$mean.null
br.LI.2$self.statistics$mean.resid
br.LI.2$cv.statistics$correlation.mean
br.LI.2$cv.statistics$correlation.se
br.LI.2$self.statistics$correlation   # SS correlation
1-(br.LI.2$self.statistics$mean.resid/br.LI.2$self.statistics$mean.null)   # explained deviance


# plots
x <- summary(br.LI.2)
gbm.plot(br.LI.2, n.plots = 12, write.title= FALSE, main = " ", rug = T, smooth = TRUE, plot.layout=c(2,4), common.scale = T, cex.axis = 1.5, cex.lab = 1.5)




### 1.2 Vascular Plants ----

# 1. tree.complexity = 2, bag.fraction = 0.75
set.seed(123)
br.VP.1 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,                
                          family = "gaussian",       
                          tree.complexity = 2,      
                          learning.rate = 0.001,     
                          bag.fraction = 0.75)      

#number of trees
br.VP.1$n.trees

# 2. tree.complexity = 3; bag.fraction = 0.75

set.seed(123)
br.VP.2 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,                
                          family = "gaussian",      
                          tree.complexity = 3,     
                          learning.rate = 0.001,     
                          bag.fraction = 0.75)

#number of trees
br.VP.2$n.trees


# 3. tree.complexity = 5, bag.fraction = 0.75
set.seed(123)
br.VP.3 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,                           
                          family = "gaussian",     
                          tree.complexity = 5,     
                          learning.rate = 0.001,     
                          bag.fraction = 0.75)

#number of trees
br.VP.3$n.trees

# 4. tree.complexity = 2, bag.fraction = 0.5
set.seed(123)
br.VP.4 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,                
                          family = "gaussian",       
                          tree.complexity = 2,     
                          learning.rate = 0.001,     
                          bag.fraction = 0.5)     

#number of trees
br.VP.4$n.trees

# 5. tree.complexity = 3, bag.fraction = 0.5
set.seed(123)
br.VP.5 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,              
                          family = "gaussian",       
                          tree.complexity = 3,      
                          learning.rate = 0.001,    
                          bag.fraction = 0.5)     

#number of trees
br.VP.5$n.trees

# 6. tree.complexity = 6, bag.fraction = 0.5
set.seed(123)
br.VP.6 <- gbm.step(data=dataVP, 
                          gbm.x = c(2:12),            
                          gbm.y = 1,                
                          family = "gaussian",      
                          tree.complexity = 3,      
                          learning.rate = 0.001,    
                          bag.fraction = 0.5)       

#number of trees
br.VP.6$n.trees


# model comparisons (best is highest correlation mean)
res.cv.corr <- data.frame(br.VP.1$cv.statistics$correlation.mean,
                          br.VP.2$cv.statistics$correlation.mean,
                          br.VP.3$cv.statistics$correlation.mean,
                          br.VP.4$cv.statistics$correlation.mean,
                          br.VP.5$cv.statistics$correlation.mean,
                          br.VP.6$cv.statistics$correlation.mean)

res.cv.corr <- as.data.frame(t(res.cv.corr))
# 1 is the best


br.VP.1$n.trees
br.VP.1$self.statistics$mean.null
br.VP.1$self.statistics$mean.resid
br.VP.1$cv.statistics$correlation.mean
br.VP.1$cv.statistics$correlation.se
br.VP.1$self.statistics$correlation   # SS correlation
1-(br.VP.1$self.statistics$mean.resid/br.VP.1$self.statistics$mean.null)   # explained deviance


# 6. plots
x <- summary(br.VP.1)
gbm.plot(br.VP.1, n.plots = 12, write.title= FALSE, main = " ", rug = T, smooth = TRUE, plot.layout=c(2,4), common.scale = T, cex.axis = 1.5, cex.lab = 1.5)



### 1.3 Deadwood Arthropods ----

# 1. tree.complexity = 2, bag.fraction = 0.75
set.seed(123)
br.DA.R.1 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1,                
                      family = "poisson",       
                      tree.complexity = 2,      
                      learning.rate = 0.0025,     
                      bag.fraction = 0.75)      

#number of trees
br.DA.R.1$n.trees

# 2. tree.complexity = 3; bag.fraction = 0.75
set.seed(123)
br.DA.R.2 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1, 
                      family = "poisson",      
                      tree.complexity = 3,     
                      learning.rate = 0.001,     
                      bag.fraction = 0.75)

#number of trees
br.DA.R.2$n.trees


# 3. tree.complexity = 5, bag.fraction = 0.75
set.seed(123)
br.DA.R.3 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1,           
                      family = "poisson",     
                      tree.complexity = 5,     
                      learning.rate = 0.0025,     
                      bag.fraction = 0.75)

#number of trees
br.DA.R.3$n.trees

# 4. tree.complexity = 2, bag.fraction = 0.5
set.seed(123)
br.DA.R.4 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1, 
                      family = "poisson",       
                      tree.complexity = 2,      
                      learning.rate = 0.0025,    
                      bag.fraction = 0.5)       
#number of trees
br.DA.R.4$n.trees

# 5. tree.complexity = 3, bag.fraction = 0.5
set.seed(123)
br.DA.R.5 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1, 
                      family = "poisson",       
                      tree.complexity = 3,      
                      learning.rate = 0.0025,     
                      bag.fraction = 0.5)      

#number of trees
br.DA.R.5$n.trees

# 6.  tree.complexity = 6, bag.fraction = 0.5
set.seed(123)
br.DA.R.6 <- gbm.step(data=dataDA, 
                      gbm.x = c(2:7),            
                      gbm.y = 1, 
                      family = "poisson",       
                      tree.complexity = 3,      
                      learning.rate = 0.0025,     
                      bag.fraction = 0.5)       

#number of trees
br.DA.R.6$n.trees


#  model comparisons (best is highest correlation mean)
res.cv.corr <- data.frame(br.DA.R.1$cv.statistics$correlation.mean,
                          br.DA.R.2$cv.statistics$correlation.mean,
                          br.DA.R.3$cv.statistics$correlation.mean,
                          br.DA.R.4$cv.statistics$correlation.mean,
                          br.DA.R.5$cv.statistics$correlation.mean,
                          br.DA.R.6$cv.statistics$correlation.mean)

res.cv.corr <- as.data.frame(t(res.cv.corr))
# 3 is the best

br.DA.R.3$n.trees
br.DA.R.3$self.statistics$mean.null
br.DA.R.3$self.statistics$mean.resid
br.DA.R.3$cv.statistics$correlation.mean
br.DA.R.3$cv.statistics$correlation.se
br.DA.R.3$self.statistics$correlation   # SS correlation
1-(br.DA.R.3$self.statistics$mean.resid/br.DA.R.3$self.statistics$mean.null)   # explained deviance

# plots
x <- summary(br.DA.R.3)
gbm.plot(br.DA.R.3, n.plots = 12, write.title= FALSE, main = " ", rug = T, smooth = TRUE, plot.layout=c(2,4), common.scale = T, cex.axis = 1.5, cex.lab = 1.5)




### 1.4 Understory Arthropods ----

# 1. tree.complexity = 2, bag.fraction = 0.75
set.seed(123)
br.UA.1 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1,                
                    family = "poisson",       
                    tree.complexity = 2,      
                    learning.rate = 0.0025,     
                    bag.fraction = 0.75)      

#number of trees
br.UA.1$n.trees

# 2. tree.complexity = 3; bag.fraction = 0.75
set.seed(123)
br.UA.2 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1, 
                    family = "poisson",      
                    tree.complexity = 3,     
                    learning.rate = 0.0025,     
                    bag.fraction = 0.75)

#number of trees
br.UA.2$n.trees


# 3. tree.complexity = 5, bag.fraction = 0.75
set.seed(123)
br.UA.3 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1,             
                    family = "poisson",     
                    tree.complexity = 5,     
                    learning.rate = 0.0025,     
                    bag.fraction = 0.75)

#number of trees
br.UA.3$n.trees

# 4. tree.complexity = 2, bag.fraction = 0.5
set.seed(123)
br.UA.4 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1, 
                    family = "poisson",       
                    tree.complexity = 2,      
                    learning.rate = 0.0025,     
                    bag.fraction = 0.5)       

#number of trees
br.UA.4$n.trees

# 5. tree.complexity = 3, bag.fraction = 0.5
set.seed(123)
br.UA.5 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1, 
                    family = "poisson",      
                    tree.complexity = 3,      
                    learning.rate = 0.0025,     
                    bag.fraction = 0.5)       

#number of trees
br.UA.5$n.trees

# 6. tree.complexity = 6, bag.fraction = 0.5
set.seed(123)
br.UA.6 <- gbm.step(data=dataUA, 
                    gbm.x = c(2:14),            
                    gbm.y = 1, 
                    family = "poisson",       
                    tree.complexity = 3,      
                    learning.rate = 0.0025,     
                    bag.fraction = 0.5)       

#number of trees
br.UA.6$n.trees


# model comparisons (best is highest correlation mean)
res.cv.corr <- data.frame(br.UA.1$cv.statistics$correlation.mean,
                          br.UA.2$cv.statistics$correlation.mean,
                          br.UA.3$cv.statistics$correlation.mean,
                          br.UA.4$cv.statistics$correlation.mean,
                          br.UA.5$cv.statistics$correlation.mean,
                          br.UA.6$cv.statistics$correlation.mean)

res.cv.corr <- as.data.frame(t(res.cv.corr))
# 1 is the best

br.UA.1$n.trees
br.UA.1$self.statistics$mean.null
br.UA.1$self.statistics$mean.resid
br.UA.1$cv.statistics$correlation.mean
br.UA.1$cv.statistics$correlation.se
br.UA.1$self.statistics$correlation   # SS correlation
1-(br.UA.1$self.statistics$mean.resid/br.UA.1$self.statistics$mean.null)   # explained deviance



# 6. plots
x <- summary(br.UA.1)
gbm.plot(br.UA.1, n.plots = 12, write.title= FALSE, main = " ", rug = T, smooth = TRUE, plot.layout=c(2,4), common.scale = T, cex.axis = 1.5, cex.lab = 1.5)


## 2. Plots ----

# Custom color palette for 4 taxonomic groups (each with 13 levels)
color_palettes <- list(
  "VP" = c(
    "#1B5E20", "#2E7D32", "#388E3C", "#43A047", "#4CAF50", "#66BB6A", 
    "#81C784", "#A5D6A7", "#C8E6C9", "#A5D6A7", "#81C784", "#66BB6A", "#388E3C"
  ),
  "DA" = c(
    "#B71C1C", "#C62828", "#D32F2F", "#F44336", "#EF5350", "#E57373", 
    "#FFCDD2", "#FF8A80", "#F44336", "#E57373", "#EF5350", "#D32F2F", "#C62828"
  ),
  "LI" = c(
    "#0D47A1", "#1976D2", "#2196F3", "#42A5F5", "#64B5F6", "#90CAF9", 
    "#BBDEFB", "#E3F2FD", "#42A5F5", "#64B5F6", "#90CAF9", "#2196F3", "#0D47A1"
  ),
  "UA" = c(
    "#6A1B9A", "#8E24AA", "#9C27B0", "#AB47BC", "#BA68C8", "#CE93D8", 
    "#E1BEE7", "#F3E5F5", "#9C27B0", "#AB47BC", "#CE93D8", "#BA68C8", "#6A1B9A"
  )
)

# color_palettes <- list(
#   "BIRDS" = c(
#     "#00695C", "#00796B", "#00897B", "#26A69A", "#4DB6AC", "#80CBC4", 
#     "#004D40", "#00574B", "#1DE9B6", "#64FFDA", "#00BFA5"
#   ),
#   "BATS" = c(
#     "#FF8F00", "#FFA726", "#FFB74D", "#FFCC80", "#FFE0B2", "#FFF3E0", 
#     "#F57C00", "#E65100", "#FF9800", "#FFC107", "#FFCA28"
#   )
# )

### 2.1 Relative influence ----

# Create dataframe
#LI
rel_inf.br.LI <- as.data.frame(summary(br.LI.2))
rel_inf.br.LI <- rel_inf.br.LI %>% 
  pivot_longer(cols = "rel.inf", values_to = "percentage")
#DA
rel_inf.br.DA <- as.data.frame(summary(br.DA.R.3))
rel_inf.br.DA <- rel_inf.br.DA %>% 
  pivot_longer(cols = "rel.inf", values_to = "percentage")
#VP
rel_inf.br.VP <- as.data.frame(summary(br.VP.1))
rel_inf.br.VP <- rel_inf.br.VP %>% 
  pivot_longer(cols = "rel.inf", values_to = "percentage")
#UA
rel_inf.br.UA <- as.data.frame(summary(br.UA.1))
rel_inf.br.UA <- rel_inf.br.UA %>% 
  pivot_longer(cols = "rel.inf", values_to = "percentage")

#Change names of the variables
var_labels <- tibble::tibble(
  var = c("siteID", "Trems_Sum", "Trems_N_type",
          "BAXCD1", "BAXCD2", "BAXCD3", "BAXCD4", "BAXCD5", "BAXCD6",
          "IBP_TOT", "SC", "decay_class", "diam", "CC", "HC"),
  new_var = c("Site",
              "TreMs Abundance",
              "TreMs Diversity",
              "Basal Area 7.5 - 17.5",
              "Basal Area 17.5 - 27.5",
              "Basal Area 27.5 - 37.5",
              "Basal Area 37.5 - 47.5",
              "Basal Area 47.5 - 57.5",
              "Basal Area > 57.5",
              "IBP",
              "Shrub Cover",
              "Decay Class",
              "Diameter",
              "Canopy Cover",
              "Herb Cover"))

rel_inf.br.DA  <- rel_inf.br.DA  %>% left_join(var_labels, by = "var")
rel_inf.br.VP  <- rel_inf.br.VP  %>% left_join(var_labels, by = "var")
rel_inf.br.LI  <- rel_inf.br.LI  %>% left_join(var_labels, by = "var")
rel_inf.br.UA  <- rel_inf.br.UA  %>% left_join(var_labels, by = "var")

rel_inf.br.LI <- rel_inf.br.LI %>%
  mutate(new_var = factor(new_var, levels = rel_inf.br.LI$new_var[order(-rel_inf.br.LI$percentage)]))
rel_inf.br.DA <- rel_inf.br.DA %>%
  mutate(new_var = factor(new_var, levels = rel_inf.br.DA$new_var[order(-rel_inf.br.DA$percentage)]))
rel_inf.br.VP <- rel_inf.br.VP %>%
  mutate(new_var = factor(new_var, levels = rel_inf.br.VP$new_var[order(-rel_inf.br.VP$percentage)]))
rel_inf.br.UA <- rel_inf.br.UA %>%
  mutate(new_var = factor(new_var, levels = rel_inf.br.UA$new_var[order(-rel_inf.br.UA$percentage)]))




# Create the stacked bar plot with a single color scale
ggplot(rel_inf.br.DA, aes(x = new_var, y = percentage, fill = new_var)) +  # Single color scale (blue)
  geom_bar(stat = "identity", width = 0.7) +
  xlab(" ") + ylab("Relative Influence (%)") +
  theme_light() +
  scale_fill_manual(
    values = color_palettes$DA  # Use the custom color palette for the "UA" taxonomic group
  ) +
  theme(legend.position = 'bottom',
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  ggtitle("Taxon: Deadwood Arthropods")  # Title for each plot

ggsave("plots/RI_DA.svg",
       width = 8, height = 6,
       device = "svg")
ggsave("plots/RI_DA.png",
       width = 8, height = 6, dpi = 500)


# Create the stacked bar plot with a single color scale
ggplot(rel_inf.br.VP, aes(x = new_var, y = percentage, fill = new_var)) +  # Single color scale (blue)
  geom_bar(stat = "identity", width = 0.7) +
  xlab(" ") + ylab("Relative Influence (%)") +
  theme_light() +
  scale_fill_manual(
    values = color_palettes$VP  # Use the custom color palette for the "UA" taxonomic group
  ) +
  theme(legend.position = 'bottom',
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  ggtitle("Taxon: Vascular Plants")  # Title for each plot

ggsave("plots/RI_VP.svg",
       width = 8, height = 6,
       device = "svg")
ggsave("plots/RI_VP.png",
       width = 8, height = 6, dpi = 500)


# Create the stacked bar plot with a single color scale
ggplot(rel_inf.br.LI, aes(x = new_var, y = percentage, fill = new_var)) +  # Single color scale (blue)
  geom_bar(stat = "identity", width = 0.7) +
  xlab(" ") + ylab("Relative Influence (%)") +
  theme_light() +
  scale_fill_manual(
    values = color_palettes$LI  # Use the custom color palette for the "UA" taxonomic group
  ) +
  theme(legend.position = 'bottom',
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  ggtitle("Taxon: Lichens")  # Title for each plot

ggsave("plots/RI_LI.svg",
       width = 8, height = 6,
       device = "svg")
ggsave("plots/RI_LI.png",
       width = 8, height = 6, dpi = 500)


# Create the stacked bar plot with a single color scale
ggplot(rel_inf.br.UA, aes(x = new_var, y = percentage, fill = new_var)) +  # Single color scale (blue)
  geom_bar(stat = "identity", width = 0.7) +
  xlab(" ") + ylab("Relative Influence (%)") +
  theme_light() +
  scale_fill_manual(
    values = color_palettes$UA  # Use the custom color palette for the "UA" taxonomic group
  ) +
  theme(legend.position = 'bottom',
        legend.spacing.x = unit(0.5, 'cm'),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  ggtitle("Taxon: Understory Arthropods")  # Title for each plot

ggsave("plots/RI_UA.svg", 
       width = 8, height = 6,
       device = "svg")
ggsave("plots/RI_UA.png",
       width = 8, height = 6, dpi = 500)

### 2.2 Partial dependency plots ----

#### 2.2.1 Lichens ----

# Create PDP for the top 4 variables
PDP_LI_1 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 6.5, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(33.3%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_2 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 27.5 - 37.5
       (15.8%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_3 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Canopy Cover\n(11.8%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_4 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 7.5 - 17.5\n(9.4%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))


#Plot grid
PDP_LI <- plot_grid(PDP_LI_1, PDP_LI_2, PDP_LI_3, PDP_LI_4, ncol=4, scale=1) +
  coord_fixed(0.5)
ggdraw(PDP_LI)
ggsave("plots/PDP_LI.png", plot = PDP_LI, dpi = 500)
ggsave("plots/PDP_LI.svg", plot = PDP_LI,
       width = 8, height = 6,
       device = "svg")


#### 2.2.2 Vascular Plants ----

# Create PDP for the top 4 variables
PDP_VP_1 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 6.5, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(45.5%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_2 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Canopy Cover\n(26.9%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_3 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(8.9%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_4 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 7.5 - 17.5\n(6.4%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))


#Plot grid
PDP_VP <- plot_grid(PDP_VP_1, PDP_VP_2, PDP_VP_3, PDP_VP_4, ncol=4, scale=1) +
  coord_fixed(0.5)
ggdraw(PDP_VP)
ggsave("plots/PDP_VP.png", plot = PDP_VP, dpi = 500)

ggsave("plots/PDP_VP.svg", plot = PDP_VP,
       width = 8, height = 6,
       device = "svg")

#### 2.2.3 Deadwood Arthropods ----

# Create PDP for the top 4 variables
PDP_DA_1 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(31%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_2 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "IBP\n(25.4%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_3 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "Decay class\n(19.5%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_4 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Diversity\n(13.7%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))


#Plot grid
PDP_DA <- plot_grid(PDP_DA_1, PDP_DA_2, PDP_DA_3, PDP_DA_4, ncol=4, scale=1) +
  coord_fixed(0.5)
ggdraw(PDP_DA)
ggsave("plots/PDP_DA.png", plot = PDP_DA, dpi = 500)

ggsave("plots/PDP_DA.svg", plot = PDP_DA,
       width = 8, height = 6,
       device = "svg")



#### 2.2.4 Understory Arthropods ----

# Create PDP for the top 4 variables
PDP_UA_1 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 27.5 - 37.5\n(17.3%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_2 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "Shrub Cover\n(13.4%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_3 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(11.7%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_4 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(10.4%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))


#Plot grid
PDP_UA <- plot_grid(PDP_UA_1, PDP_UA_2, PDP_UA_3, PDP_UA_4, ncol=4, scale=1) +
  coord_fixed(0.5)
ggdraw(PDP_UA)
ggsave("plots/PDP_UA.png", plot = PDP_UA, dpi = 500)
ggsave("plots/PDP_UA.svg", plot = PDP_UA,
       width = 8, height = 6,
       device = "svg")


### 2.3 Full Partial dependency plots ----


#### 2.3.1 Lichens ----

# Create PDP for the top 4 variables
PDP_LI_1 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(33.3%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_2 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 27.5 - 37.5
       (15.8%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_3 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Canopy Cover\n(11.8%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_4 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 7.5 - 17.5\n(9.4%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_5 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 5) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(8.2%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_6 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 6) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 37.5 - 47.5\n(8.2%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_7 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 7) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 17.5 - 27.5\n(6%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_8 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 8) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "IBP\n(3.3%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_9 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 9) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Diversity\n(2.4%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_10 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 10) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 47.5 - 57.5\n(1.3%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))

PDP_LI_11 <- ggPD(br.LI.2, rug = T, col.line = "#56B4E9", smooth = T, col.smooth = "#56B4E9",
                  cex.line = 0.3, cex.smooth = 0.3, predictor = 11) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area > 57.5\n(0.3%)") +
  coord_cartesian(ylim = c(-0.3, 0.20))


#Plot grid
PDP_LI_full <- plot_grid(PDP_LI_1, PDP_LI_2, PDP_LI_3, PDP_LI_4,
                    PDP_LI_5, PDP_LI_6, PDP_LI_7, PDP_LI_8,
                    PDP_LI_9, PDP_LI_10, PDP_LI_11, ncol=3, scale=1) +
  coord_fixed(1)
ggdraw(PDP_LI_full)
ggsave("plots/PDP_LI_full.png", width = 12, height = 12,
       plot = PDP_LI_full, dpi = 500)
ggsave("plots/PDP_LI_full.svg", plot = PDP_LI_full, 
       device = "svg")


#### 2.3.2 Vascular Plants ----


# Create PDP for the top 4 variables
PDP_VP_1 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(45.5%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_2 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Canopy Cover\n(26.9%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_3 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(8.9%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_4 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 7.5 - 17.5\n(6.4%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_5 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 5) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 17.5 - 27.5\n(3.3%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_6 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 6) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 47.5 - 57.5\n(2.9%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_7 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 7) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 27.5 - 37.5\n(1.8%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_8 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 8) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 37.5 - 47.5\n(1.7%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_9 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 9) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "IBP\n(1.1%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_10 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 10) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area > 57.5\n(0.8%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

PDP_VP_11 <- ggPD(br.VP.1, rug = T, col.line = "#43A047", smooth = T, col.smooth = "#43A047",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 11) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Diversity\n(0.8%)") +
  coord_cartesian(ylim = c(-0.15, 0.05))

#Plot grid
PDP_VP_full <- plot_grid(PDP_VP_1, PDP_VP_2, PDP_VP_3, PDP_VP_4,
                         PDP_VP_5, PDP_VP_6, PDP_VP_7, PDP_VP_8,
                         PDP_VP_9, PDP_VP_10, PDP_VP_11, ncol=3, scale=1) +
  coord_fixed(1)
ggdraw(PDP_VP_full)
ggsave("plots/PDP_VP_full.png", width = 12, height = 12,
       plot = PDP_VP_full, dpi = 500)
ggsave("plots/PDP_VP_full.svg", plot = PDP_VP_full, 
       device = "svg")






#### 2.3.3 Deadwood Arthropods ----


# Create PDP for the top 4 variables
PDP_DA_1 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(31%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_2 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "IBP\n(25.4%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_3 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Decay class\n(19.5%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_4 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Diversity\n(13.7%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_5 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 5) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(6.5%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))

PDP_DA_6 <- ggPD(br.DA.R.3, rug = T, col.line = "#F44336", smooth = T, col.smooth = "#F44336",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Diameter\n(3.8%)") +
  coord_cartesian(ylim = c(-0.15, 0.12))


#Plot grid
PDP_DA_full <- plot_grid(PDP_DA_1, PDP_DA_2, PDP_DA_3, PDP_DA_4,
                         PDP_DA_5, PDP_DA_6,  ncol=3, scale=1) +
  coord_fixed(0.5)
ggdraw(PDP_DA_full)
ggsave("plots/PDP_DA_full.png", width = 12, height = 12,
       plot = PDP_DA_full, dpi = 500)
ggsave("plots/PDP_DA_full.svg", plot = PDP_DA_full, 
       device = "svg")


#### 2.3.4 Understory Arthropods ----

# Create PDP for the top 4 variables
PDP_UA_1 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 1) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 27.5 - 37.5\n(17.3%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_2 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 2) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Shrub Cover\n(13.4%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_3 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 3) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  scale_x_discrete(labels = function(x) substr(x, nchar(x) - 1, nchar(x))) + # Keep only last two letters
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 18)) +
  labs(x = "Site\n(11.7%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_4 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 4) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Abundance\n(10.4%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_5 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 5) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 7.5 - 17.5\n(8.8%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_6 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 6) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Herb Cover\n(8.1%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_7 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 7) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "IBP\n(6.2%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_8 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 8) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 37.5 - 47.5\n(5.6%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_9 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 9) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area 17.5 - 27.5\n(5.3%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))


PDP_UA_10 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                 cex.line = 0.3, cex.smooth = 0.3, predictor = 10) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Basal Area > 57.5\n(4.5%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_11 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                  cex.line = 0.3, cex.smooth = 0.3, predictor = 11) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "Canopy Cover\n(4.5%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

PDP_UA_12 <- ggPD(br.UA.1, rug = T, col.line = "#AB47BC", smooth = T, col.smooth = "#AB47BC",
                  cex.line = 0.3, cex.smooth = 0.3, predictor = 12) +
  theme() +
  labs(y = "Marginal effect", color = "Legend") + 
  scale_color_manual(values = colors) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.title.x = element_text(size = 18, hjust=0.5),
        plot.title = element_text(size = 18)) +
  labs(x = "TreMs Diversity\n(3.6%)") +
  coord_cartesian(ylim = c(-0.17, 0.17))

#Plot grid
PDP_UA_full <- plot_grid(PDP_UA_1, PDP_UA_2, PDP_UA_3, PDP_UA_4,
                         PDP_UA_5, PDP_UA_6, PDP_UA_7, PDP_UA_8,  
                         PDP_UA_9, PDP_UA_10, PDP_UA_11, PDP_UA_12, 
                         ncol=3, scale=1) +
  coord_fixed(1)
ggdraw(PDP_UA_full)
ggsave("plots/PDP_UA_full.png", width = 12, height = 12,
       plot = PDP_UA_full, dpi = 500)
ggsave("plots/PDP_UA_full.svg", plot = PDP_UA_full, 
       device = "svg")