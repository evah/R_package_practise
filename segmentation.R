
library(openxlsx)
library(rbokeh)
library(fpc)
library(tidyverse)
library(annalectr)
library(fpc)

needs(tidyverse, Rtsne, dbscan, cluster, proxy, rbokeh, feather, glmnet)

db <- src_dsdk()

mcd_ipsos_remote <- db %>% tbl("mcd_ipsos_ts_model_sample")

mcd_ipsos_remote %>%
  distinct(ak_user_id) %>%
  select(ak_user_id) ->
  mcd_people

mcd_people %>%
  mutate ( x = random() ) %>% 
  filter ( x < .01) %>%
  select(ak_user_id) %>%
  compute() ->
  mcd_people_sample

mcd_ipsos_remote %>%
  inner_join(mcd_people_sample) %>%
  select(ak_user_id, segment, tax_key) %>%
  collect(n = Inf) ->
  mcd_ipsos_local

mcd_ipsos_local %>%
  mutate(ak_user_id = as.character(ak_user_id),
         tax_key = as.factor(tax_key)) ->
  mcd_prepped

mcd_prepped %>%
  xtabs ( ~ ak_user_id + tax_key, data=., sparse=TRUE ) -> X

mcd_prepped %>%
  select ( ak_user_id, segment ) %>%
  distinct() -> y_var

data_frame ( ak_user_id=rownames(X) ) %>% 
  left_join ( y_var ) %>% 
  select ( segment ) %>% 
  unlist ( use.names=FALSE ) -> y 

Xy <- list()

Xy[[1]] <- X
Xy[[2]] <- y

# ---------  build model --------------


Alpha <- 0.50   # elastic net penalty ( 1=Lasso, 0=Ridge )
pMax <- 900

needs(glmnet)
m1 <- glmnet(X, y, family = "binomial", alpha = Alpha, pmax = pMax )
s = min(m1$lambda)
lowest_lambda_coefs <- coef(m1, s = min(m1$lambda))

plot(m1)
plot(m1, xvar = "dev", label = TRUE)

cvm1 <- cv.glmnet(X, y, family = "binomial", alpha = Alpha, pmax = pMax )
plot(cvm1)


#------------ extract -------

getModelVars <- function ( cvmodel, sParam ) {
  # get a list of reasonable model coefs
  var_coeffs <- coef(cvmodel, s = sParam )
  # drop intercept, make logical flags for selecting non-zero coefs
  varFlags <- (abs(var_coeffs[, 1]) > 0)[-1]
  tax_key <- names(varFlags)[varFlags]
  vars2use <- as.data.frame(tax_key)
  return(vars2use)
}

modvars1 <- getModelVars ( cvm1, "lambda.1se" )

###################### Eva
temp_modvars1<-modvars1 

modvars1<-read.csv("/Users/yiqi.huang/Projects/mcd_ts_subsegment/patrick_selected_var.txt", header=TRUE)
tax_id<-read.csv("/Users/yiqi.huang/Projects/mcd_ts_subsegment/tax_id", header=TRUE)
dict <- read.csv("/Users/yiqi.huang/Projects/mcd_ts_subsegment/June_2017_attributes.txt", sep="\t", header=T)
tax_dict<-cbind(tax_id, dict)

modvars1$tax_key <- as.character(modvars1$tax_key)
tax_dict$tax_key <- as.character(tax_dict$tax_key)

modvars1 %>%
  left_join(tax_dict) ->
  modvars1_dict

write.csv(modvars1_dict, file = "/Users/yiqi.huang/Projects/mcd_ts_subsegment/modvars1_dict", row.names = FALSE, sep="\t")
#######################Eva




#-----------------------------------

## extract only needed variables
mcd_ipsos_local %>%
  right_join(modvars1) ->
  for_crosstab

for_crosstab %>%
  xtabs ( ~ ak_user_id + tax_key, data=., sparse=TRUE ) -> 
  crosstab

m <- as(crosstab, "matrix")

# Change thisa
# Distance calculation takes some time too
distance <- dist(m, "cosine")

# Clustering
clustering <- hclust(distance, "ward.D2")
cluster_sizes <- list()
statistics = vector("list", length = 50)

# the loop takes some time
for (i in 2:50) {
  assignments <- cutree(clustering, i)
  stats = cluster.stats(distance, assignments, silhouette = T)  
  cluster_sizes[[i]] <- table(assignments)
  statistics[[i]] <- stats
}

map(statistics, ~ .$wb.ratio) %>% 
  unlist %>% 
  as.data.frame %>% 
  mutate(idx = row_number()) %>% 
  figure(600, 400, title = "average.within/average.between") %>% 
  ly_points("idx", ".", hover = c("Clusters" = "idx", "Score" = "."))

t = Rtsne(distance, is_distance = T, perplexity = 5, check_duplicates = F)

# clustering = load("./clustering")
clustering <- readRDS("./output/clustering_object.RDS")
groups <- cutree(clustering, 14)

t$Y %>% 
  as.data.frame %>% 
  mutate(cluster = as.factor(groups), 
         person_id = as.numeric(rownames(crosstab))) %>% 
  collect() %>% 
  figure(750, 500) %>% 
  ly_points(V1, V2, alpha = .5, color = cluster, 
            hover = c(person_id, cluster)) 
