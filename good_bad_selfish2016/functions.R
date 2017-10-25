#Test data frames for functions
# Situation <- data.frame(x1 = c(1,2,3,4,5,6,7,8), x2 = c(4,5,6,7,8,9,1,2))
# Disposition <- data.frame(y1 = c(1,1,1,1,2,2,2,2), y2 = c(3,3,3,3,4,4,4,4))
# id <- c("alex", "alex", "alex", "alex", "ziv", "ziv", "ziv", "ziv")
# dec_num <- c(1, 2, 3, 4, 1, 2, 3, 4)
# y <- as.factor(c(0,1,1,0,1,1,0,0))

getData <- function(testDeviants = 'false') {
  df <- read.csv("/Users/ziv/GDrive/research/hcl/coop-pred-ec/camera-code/unilateral-giving-data.csv")
  #df <- read.csv("unilateral_round2_dec_data_reshaped.csv")
  
    df <- subset(df,   
                 df$hclresid ==1 & 
                   df$attencheck ==2 &
                   df$sports_atc_3 == 1 &
                   df$q134 ==2 &
                   df$valid == TRUE) # add 4th comprehension check 
  
  
  trolleysraw <- df[,grep("jg", names(df))]
  trolley1 <- as.numeric(scale(trolleysraw$jghcon)*.65 + 
                           scale(trolleysraw$jglcon)*.46 + 
                           scale(trolleysraw$jgsoph)*.596)
  trolley2 <- as.numeric(-scale(trolleysraw$jgimp)*.92 + 
                           scale(trolleysraw$jglcon)*.36)
  
  trolleys <- data.frame(trolley1, trolley2)
  
  crtraw <- df[,grep("crt", names(df))]
  crt <- (crtraw$crt_age==4) + (crtraw$crt_print==10) + (crtraw$crt_mold==39)
  
  charstrenghts <- df[,grep("cs", names(df))]
  econ <- df[,grep("econ", names(df))]
  
  big5raw <- df[,grep("person", names(df))]
  
  E <- as.numeric(scale(-big5raw[,1] + big5raw[,6]))
  A <- as.numeric(scale(big5raw[,2] - big5raw[,7]))
  C <- as.numeric(scale(-big5raw[,3] + big5raw[,8]))
  N <- as.numeric(scale(-big5raw[,4] + big5raw[,9]))
  O <- as.numeric(scale(-big5raw[,5] + big5raw[,10]))
  
  big5 <- data.frame(O,C,E,A,N)
  
  personality <- df[,c("hcltrust", 
                       "hclthough",
                       "hcltstpep")]
  
  irrelevant <- df[,c("hclseat",
                      "rblueeye",
                      "rumbrella",
                      "rsiblings",
                      "rmilk")]
  
  df$hclgender = df$hclgender-1
  
  demos <- df[,c("hcleducat", 
                 "hcltstpep", 
                 "hclpoliti", 
                 "hclfiscal", 
                 "hclsocial", 
                 "hclgod",
                 "hclincome",
                 "hclage",
                 "hclgender",
                 "k")] #add discounting k to demographics
  
  discountraw <- df[,grep("dds",names(df))]
  
  experience <- df[,c("hcldividi",
                      "hclsurvey",
                      "hclturkre")]
  
  dec <- df$i
  
  raw.sit <- df[,c( "a", "b", "cost", "bc")]
  
  raw.sit <- within(raw.sit[,1:4], {
                      mult <- bc -1
                     Imult <- as.numeric(bc > 1)
#                     diff_start <- a-b
#                     diff_end <- ((a-cost) - (b+cost*bc))
                     delta_diff_coop <- (a-b)-( (a-cost)-(b+cost*bc))
#                     Idiff_start <- as.numeric((a-b)>0)
#                     Idiff_end <- as.numeric((((a-cost) - (b+cost*bc)))>0)
                     Idelta_diff_coop <- as.numeric(((a-b)-( (a-cost)-(b+cost*bc))) > 0)
                    }) #generate advantageous inequity indiciator variable
  
  set <- cbind(dec = dec-1,
               experience, 
               demos, 
               irrelevant, 
               big5, 
               personality,
               charstrenghts,
               econ,
               trolleys,
               raw.sit,
               time = df$t,
               state = as.factor(df$state),
               time.adj = as.factor(df$time.adj),
               id = df$mtid)
  
  set$hclsurvey <- NULL
set$time <- NULL
set$id <- droplevels(set$id)
set <- set[-which(set$id == 'A3LI11B9RGB73X')[21:40],]
set <- set[-which(set$id == 'A3N8MC9JZWS4WX')[21:40],]
set <- set[-which(set$id == 'A122MZOQT9A8XL')[21:40],]
#set <- set[which(set$id != 'A1C57DJF1HCR3Q'),]
  #set <- set[complete.cases(set),]
bad <- set$id[which(!complete.cases(set))]
set <- set[which(!set$id %in% bad),]
  
  set$random_number <- runif(n=dim(set)[1])
  
  set <- data.frame(set)
  
#   bad <- c(7453 ,7454 ,7455, 7456, 7457, 7458, 7459, 7460 ,7461, 7462, 7463, 7464, 7465 ,7466, 7467, 7468, 7469 ,7470 ,7471,
#            7453: 7471,
#            6656:6674)
#, 7806:7825, 6986:7005
#   set <- set[-bad,]
#   set$id <- droplevels(set$id)
#   set <- set[which(set$id != 'A3LI11B9RGB73X'),]
# set <- set[which(set$id != 'A3N8MC9JZWS4WX'),]
# set <- set[which(set$id != 'A122MZOQT9A8XL'),]
#set <- set[which(set$id != 'A1C57DJF1HCR3Q'),]


  set <- set %>%
    group_by(id) %>%
    arrange(random_number) %>%
    mutate(dec_num = c(1:n())) %>%
    ungroup()

  Disposition <- set %>%
    select(hcldividi, hclturkre, hcleducat,
           hclpoliti, hclfiscal,
           hclsocial, hclgod, hclincome, hclage,
           hclgender, O, C, E, A, N, hcltrust,
           hclthough, hcltstpep, cs1, cs2, cs3,
           cs4, cs5, cs6, cs7, cs8, cs9, cs10, cs11,
           cs12, cs13, cs14, cs15, cs16, cs17, cs18,
           cs19, cs20, cs22, cs23, cs24, riskecon_1,
           ambigecon_1, mazeecon, trolley1, trolley2, k)
  
  id <- set %>%
    select(id)
  
  Situation <- set %>%
    select(a, b, cost, mult,delta_diff_coop,Imult,Idelta_diff_coop)
  
  Sit2 <- set %>% 
    select(state, time.adj)
  
  decisions <- set %>%
    select(dec)
  
  dec_num <- set %>%
    select(dec_num)
  pca <- prcomp(Disposition[,19:41])
  factors <- pca$x[,1:4]
  colnames(factors) <- c("prosociality", "intellectualism", "self-determination", "self-control")
  d <- cbind(Disposition[,-c(19:41)], factors)
  names(d) <- lapply(names(d),function(x){gsub("-",'',x)})
  l <- list(d, Situation, id, decisions, dec_num, Sit2)
  return(l)
}

addSquares <- function(X) {
  X.new <- cbind(X, X^2)
  names(X.new) <- c(names(X), paste(names(X), "sqr", sep="."))
  return(X.new)
}

addSelfInteractions <- function(X) {
  s <- paste("~(", paste(names(X), collapse="+"), ")^2", sep="")
  X.new <- model.matrix(as.formula(s), X)
  X.new <- as.data.frame(X.new[,-1])
  return(X.new)
}

makeWithinMatrix <- function(X, id, self.interact = "true", fe.interact = "true", squares = "false") {
  if (self.interact == "true") {
    X <- addSelfInteractions(X)
  }
  
  if (squares == "true") {
    X <- addSquares(X)    
  }
  
  df <- data.frame(X, id)
  s <- paste("~(", paste(names(X), collapse="+"), "+as.factor(id))", sep="")
  
  if(fe.interact == "true") {
    s <- paste("~((", paste(names(X), collapse="+"), ")*as.factor(id))", sep="")
  }
  
  X.new <- model.matrix(as.formula(s), df)
  
  return(X.new)
}

makeBetweenMatrix <- function(X, Y = NULL, interact.within = "true", interact.between = "true", squares = "true") {
  if (is.null(Y) & interact.within == 'true'){
    X.new <- addSelfInteractions(X)
  } else if (is.null(Y)){
    X.new <- X
  }
  if (interact.within == "true" & interact.between == "false" & !is.null(Y)) {
    X <- addSelfInteractions(X)
    Y <- addSelfInteractions(Y)
    X.new <- cbind(X, Y)
  }
  
  if (interact.within == "true" & interact.between == "true" & !is.null(Y)) {
    Z <- cbind(X, Y)
    Z <- addSelfInteractions(Z)
    X.new <- Z
  }
  
  if (interact.within == "false" & interact.between == "true") {
    return("You wanted no within and yes between interactions. 
           That's stupid, we're not doing that.")
  }
  
  if (squares == "true") {
    X.new <- addSquares(X.new)
  }
  
  X.new <- model.matrix(~.,X.new)
  return(X.new)
}

getFoldIds <- function(id, dec_num, type = "within", nfolds = 2) {
  df <- data.frame(id, dec_num)
  
  if (type == "within") {
    min_decs <- df %>%
      group_by(id) %>%
      summarize(l = length(dec_num)) %>%
      ungroup() %>%
      summarise(min = min(l))
    
    if (min_decs$min < nfolds*2) {return("Not enough decisions for how many folds you want.")}
  
    foldids <- dec_num %% nfolds + 1
  }
  
  if (type == "between") {
    #Add errorchecking here.
    
    unique_ids <- unique(id)
    n_people <- dim(unique_ids)[1]
    
    id_to_number <- data.frame(id = unique_ids, id_fold = c(1:n_people))
    id_to_number$id_fold <- id_to_number$id_fold %% nfolds + 1
    to_id <- data.frame(id) %>%
      inner_join(id_to_number)
    foldids <- to_id$id_fold
  }
  
  return(foldids)
}

runWithinRegression <- function(y, Situation, id, dec_num,
                                self.interact = "true", fe.interact = "true", squares = "false",
                                alpha = 0, nfolds = 5) {
  
  print("Building matrix...")
  X <- makeWithinMatrix(Situation, id, self.interact, fe.interact, squares)
  
  fold_ids <- getFoldIds(id, dec_num, type = "within", nfolds)
  
  print("Generating model...")
  model <- cv.glmnet(y=y,
                     x=X,
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids,
                     family="binomial")
                     
  return(model)
}
find.firsts <- function(x) { match(unique(x), x) }

runBetweenRegression <- function(y, Disposition, Situation, id, dec_num,
                                self.interact = "true", interact.between = "true", squares = "false",
                                alpha = 0, nfolds = 5, checkCross = "false") {
  print("Generating matrices...")
  if (checkCross == "false"){
    print("computing between")
  X <- makeBetweenMatrix(Disposition, NULL, 
                         interact.within = self.interact,
                         interact.between = interact.between,
                         squares = squares)
  
  print("folding")
  fold_ids <- getFoldIds(id, dec_num, type = "between", nfolds)
  all_people = lapply(id[[1]], toString)
  A = unique(all_people)
  training_people <- sample(A, length(A)*2/3)
  totrain <- all_people %in% training_people
  } else {
    print("computing cross")
    X <- makeSingleMatrix(cbind(Disposition, Situation), interact.within = "true", squares = "true")
  totrain <- unlist(dec_num) %in% 1:15
  }
  
  #   #
  #   s <- sample(nrow(unique(id[,1])),nrow(unique(id[,1])), replace = TRUE)
  #   
  #   idz <- unique(unlist(id[,1]))
  #   bootstrap <- sample(x=(idz),size=length((idz )), replace=TRUE)
  #   
  #   count <- function(x){
  #     sum(bootstrap == x)
  #   }
  #   wtz <- map(count,unlist(id[,1]))
  #
  #TODO: weights shit
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain],
                     family="binomial")
  #   model <- my.cv.glmnet(y=y,
  #                      x=X,
  #                      alpha=alpha,
  #                      type.measure="auc",
  #                      foldid=fold_ids,
  #                      family="binomial",
  #                      id = string_id,
  #                      self.interact)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
  
  print("Generating model...")
  model <- cv.glmnet(y=y,
                     x=X,
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids,
                     family="binomial")
  
  return(model)
}

makeSingleMatrix <- function(X, interact.within = "true", squares = "true") {
  X.new <- X
  if(interact.within == "true"){
    X.new <- addSelfInteractions(X)  
  }

  if (squares == "true") {
    X.new <- addSquares(X.new)
  }
  
  X.new <- model.matrix(~.,X.new)
  return(X.new)
}


runSingleRegression <- function(y, X, id, dec_num,
                                 self.interact = "true", squares = "false",
                                 alpha = 0, nfolds = 5, type= "between") {
  print("Generating matrices...")
  
  X <- makeSingleMatrix(X, self.interact, squares)
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)

  
  print("Generating model...")
  model <- cv.glmnet(y=y,
                     x=X,
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids,
                     family="binomial")
  
  return(model)
}

bootstrapSingleRegression <- function(y, X, id, dec_num,
                                self.interact = "true", squares = "false",
                                alpha = 0, nfolds = 5, type= "between") {
  print("Generating matrices...")
  s <- sample(nrow(unique(id[,1])),nrow(unique(id[,1])), replace = TRUE)
  X <- makeSingleMatrix(X, self.interact, squares)
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  idz <- unique(unlist(id[,1]))
  bootstrap <- sample(x=(idz),size=length((idz )), replace=TRUE)
  
  count <- function(x){
    sum(bootstrap == x)
  }
  wtz <- map(count,unlist(id[,1]))
  print("Generating model...")
  model <- cv.glmnet(y=y,
                     x=X,
                     weights = wtz,
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids,
                     family="binomial")
  
  return(model)
}


getAUC <- function(glmnet){
  x = cbind(glmnet$lambda, glmnet$cvup)
  s = min(glmnet$lambda)
  auc = x[which(x[,1]==s),]
  return(auc[2])
}

latlong2state <- function(pointsDF) {
  #input in the form testPoints <- data.frame(x = c(-90, -120,-80.04), y = c(44, 44,  32.8688))
  # where x is longitude and y is lattitude
  require(sp)
  require(maps)
  require(maptools)
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


runRepRegression <- function(y, X, id, dec_num,
                               self.interact = "true", squares = "false",
                               alpha = 0, nfolds = 5, type= "between", testOrder = 'false') {
  
  print("Generating giver vector...")
  
  X <- generateSituationSetB(X)
  
  
#   X <-model.matrix(~(interactX*givers) )
fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }

  #   #
  #   s <- sample(nrow(unique(id[,1])),nrow(unique(id[,1])), replace = TRUE)
  #   
  #   idz <- unique(unlist(id[,1]))
  #   bootstrap <- sample(x=(idz),size=length((idz )), replace=TRUE)
  #   
  #   count <- function(x){
  #     sum(bootstrap == x)
  #   }
  #   wtz <- map(count,unlist(id[,1]))
  #
  #TODO: weights shit
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain],
                     family="binomial")
  #   model <- my.cv.glmnet(y=y,
  #                      x=X,
  #                      alpha=alpha,
  #                      type.measure="auc",
  #                      foldid=fold_ids,
  #                      family="binomial",
  #                      id = string_id,
  #                      self.interact)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}



generateSituationSetB <- function(X){
  makeSingleMatrix(data.frame(X), interact.within = "true", squares= "true")[,c(1:24,26:34)]
}


runFexSitRegression <- function(y, Situation, id, dec_num,
                                self.interact = "false", fe.interact = "true", squares = "false",
                                alpha = 0, nfolds = 5, testOrder ='false') {
  
  print("Building matrix...")
  X <- generateSituationSetB(Situation)
  df <- data.frame(X, id)
  
  
  if(fe.interact == "true") {
    s <- paste("~((", paste(colnames(df[,-ncol(df)]), collapse="+"), ")*as.factor(id))", sep="")
  }
  
  X <- model.matrix(as.formula(s), df)
  
  fold_ids <- getFoldIds(id, dec_num, type = "within", nfolds)
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids,
                     family="binomial")
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}


runFeRegression <- function(y, id, dec_num,alpha = 0, nfolds = 5) {
  
  df <- data.frame(id)
  X <- model.matrix(~as.factor(id),df)
  
  fold_ids <- getFoldIds(id, dec_num, type = "within", nfolds)
  totrain <- unlist(dec_num) %in% 1:15
  print("Generating model...")
  trainY <- y[totrain]
  trainX <- X[totrain,]
  model <- cv.glmnet(y=trainY,
                     x=trainX,
                     alpha=alpha,
                     type.measure="auc",
                     foldid=fold_ids[totrain,],
                     family="binomial")
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}

runZnZRegression <- function(y, X, id, dec_num,
                             self.interact = "true", squares = "false",
                             alpha = 0, nfolds = 5, type= "between", testOrder = 'false') {
  
  
  dummy <- c()
  for (i in 1:len(y)){
    if (i %% 20 == 0){
      front <- i-19
      back <- i -5
    } else {
    front <-20*floor(i/20)  + 1
    back <-    20*floor(i/20) + 15
    }
  dummy[i] <- as.numeric(sum(as.numeric(y[front:back])-1) >0 )
  } 
  
  #   X <-model.matrix(~(interactX*givers) )
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }
  X <- as.matrix(data.frame(dummy, v))

  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain],
                     family="binomial")

  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}

runGiverRegression <- function(y, X, id, k=1,K=NULL, dec_num, giverType,
                               self.interact = "true", squares = "false",
                               alpha = 0, nfolds = 5, type= "between",testOrder) {
  
  print("Generating giver vector...")
  
  if (giverType == "double"){
    giverz <- giverDouble(k,dec,id,dec_num)
    
    print("Generating matrices...")
    
    #X <- makeSingleMatrix(data.frame(X,giverz), interact.within = "true", squares= "false")
    X <- makeSingleMatrix(data.frame(X,giverz), interact.within = "true", squares= "false")
  } else if (giverType == "triple"){
    giverz <- giverTriple(k,K,dec,id,dec_num)
    high <- giverz[[1]]
    low <- giverz[[2]]
    
    print("Generating matrices...")
    
    X <- makeSingleMatrix(data.frame(X,low,high), interact.within = "true", squares= "false")
  }
  
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }
  #   #
  #   s <- sample(nrow(unique(id[,1])),nrow(unique(id[,1])), replace = TRUE)
  #   
  #   idz <- unique(unlist(id[,1]))
  #   bootstrap <- sample(x=(idz),size=length((idz )), replace=TRUE)
  #   
  #   count <- function(x){
  #     sum(bootstrap == x)
  #   }
  #   wtz <- map(count,unlist(id[,1]))
  #
  #TODO: weights shit
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain],
                     family="binomial")
  #   model <- my.cv.glmnet(y=y,
  #                      x=X,
  #                      alpha=alpha,
  #                      type.measure="auc",
  #                      foldid=fold_ids,
  #                      family="binomial",
  #                      id = string_id,
  #                      self.interact)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}

giverDouble <- function(k,y,id,dec_num){
  string_id <- unlist(lapply(id[[1]], toString))
  map_big <- data.frame(y,string_id, dec_num)
  
  give <- c()
  
  for (j in 1:length(y)){
    local.person <- string_id[j]
    num_give <- sum(as.numeric(map_big[which(string_id == local.person & dec_num <= 15),]$y)-1)
    give[j] <- as.numeric(num_give >= k)
  }
  give
}

giverTriple <- function(k,K,y,id,dec_num){
  string_id <- unlist(lapply(id[[1]], toString))
  map_big <- data.frame(y,string_id, dec_num)
  ng <- c()
  low <- c()
  high <- c()
  for (j in 1:length(y)){
    local.person <- string_id[j]
    num_give <- sum(as.numeric(map_big[which(string_id == local.person & dec_num <= 15),]$y)-1)
    ng[j] <- num_give
    high[j] <- as.numeric(num_give >= K)
    low[j] <- as.numeric(num_give < K & num_give >= k)
  }
  list(high,low, ng)
}






my.cv.glmnet <- function (x, y, id, self.interact = "true", offset = NULL, lambda = NULL, type.measure = "auc", nfolds = 10, foldid, 
                          grouped = TRUE, keep = FALSE, ...) 
{
  if (!is.null(lambda) && length(lambda) < 2) 
    stop("Need more than one value of lambda for cv.glmnet")
  N = nrow(x)
  weights = rep(1, N)
  
  y = drop(y)
  glmnet.call = match.call(expand.dots = TRUE)
  which = match(c("type.measure", "nfolds", "foldid", "grouped", 
                  "keep"), names(glmnet.call), F)
  if (any(which)) 
    glmnet.call = glmnet.call[-which]
  glmnet.call[[1]] = as.name("glmnet")
  map_big <- data.frame(y,id)
  
  giver_big <- c()
  
  for (j in 1:length(y)){
    local.person <- id[j]
    local.giver <- as.numeric(sum(as.numeric(map_big[which(id == local.person),]$y)-1) != 0)
    giver_big[j] <- local.giver
  }
  
  
  X.big <- makeSingleMatrix(data.frame(x,giver_big),interact.within = "true", squares = "false")
  glmnet.object = glmnet(x, y, weights = weights, offset = offset, 
                         lambda = lambda, ...)
  glmnet.object$call = glmnet.call
  is.offset = glmnet.object$offset
  if (inherits(glmnet.object, "multnet") && !glmnet.object$grouped) {
    nz = predict(glmnet.object, type = "nonzero")
    nz = sapply(nz, function(x) sapply(x, length))
    nz = ceiling(apply(nz, 1, median))
  }
  else nz = sapply(predict(glmnet.object, type = "nonzero"), 
                   length)
  if (missing(foldid)) 
    foldid = sample(rep(seq(nfolds), length = N))
  else nfolds = max(foldid)
  if (nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  outlist = as.list(seq(nfolds))
  
  for (i in seq(nfolds)) {
    which = foldid == i ###homeys in test set
    if (is.matrix(y)) 
      y_sub = y[!which, ]
    else y_sub = y[!which] #ysub are actual y's to be used
    if (is.offset) 
      offset_sub = as.matrix(offset)[!which, ]
    else offset_sub = NULL
    x_matrix = x[!which, , drop = FALSE]
    ids = id[!which]
    map <- data.frame(y_sub,ids)
    
    giver <- c()
    
    for (j in 1:length(y_sub)){
      local.person <- ids[j]
      local.giver <- as.numeric(sum(as.numeric(map[which(ids == local.person),]$y_sub)-1) != 0)
      giver[j] <- local.giver
    }
    
    
    X.fold <- makeSingleMatrix(data.frame(x_matrix,giver),interact.within = "true", squares = "false")
    
    outlist[[i]] = glmnet(X.fold, 
                          y_sub, lambda = lambda, offset = offset_sub, 
                          weights = weights[!which], ...)
    print(i)
  }
  
  fun = paste("cv", class(glmnet.object)[[1]], sep = ".")
  lambda = glmnet.object$lambda
  cvstuff = my.lognet(outlist, lambda, x, y,id, weights, offset, foldid, type.measure, grouped, keep)
  cvm = cvstuff$cvm
  cvsd = cvstuff$cvsd
  nas = is.na(cvsd)
  if (any(nas)) {
    lambda = lambda[!nas]
    cvm = cvm[!nas]
    cvsd = cvsd[!nas]
    nz = nz[!nas]
  }
  cvname = cvstuff$name
  out = list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + 
               cvsd, cvlo = cvm - cvsd, nzero = nz, name = cvname, glmnet.fit = glmnet.object)
  if (keep) 
    out = c(out, list(fit.preval = cvstuff$fit.preval, foldid = foldid))
  lamin = if (cvname == "AUC") 
    getmin(lambda, -cvm, cvsd)
  else getmin(lambda, cvm, cvsd)
  obj = c(out, as.list(lamin))
  class(obj) = "cv.glmnet"
  obj
}


buildGiverModel <- function(y, X, id, k=1,K=NULL, dec_num, giverType,
                            self.interact = "true", squares = "false",
                            alpha = 0, nfolds = 5, type= "between", interact = FALSE) {
  
  
  giverz <- giverTripleRedux(k,K,y,id,dec_num)
  low <- giverz[[1]]
  high <- giverz[[2]]
  sigma <-  giverz[[3]]
  
  print("Generating matrices...")
  
  if (interact){
    data.frame(makeSingleMatrix(data.frame(X), interact.within = "true", squares= "true"),low,high,sigma)
  } else{
    data.frame(X,low,high,sigma)
  }
}

giverTripleRedux <- function(k,K,y,id,dec_num){
  string_id <- unlist(lapply(id[[1]], toString))
  map_big <- data.frame(y,string_id, dec_num)
  
  low <- c()
  high <- c()
  sigma <- c()
  
  for (j in 1:length(y)){
    local.person <- string_id[j]
    num_give <- sum(as.numeric(map_big[which(string_id == local.person & dec_num < 15),]$y)-1)
    high[j] <- as.numeric(num_give >= K)
    low[j] <- as.numeric(num_give < K & num_give >= k)
    sigma[j] <- num_give
  }
  list(low,high,sigma)
}

runGiverRegressionRedux <- function(y, X, id, k=1,K=NULL, dec_num, giverType,
                                    self.interact = "true", squares = "false",
                                    alpha = 0, nfolds = 5, type= "within",testOrder = 'false') {
  
  print("Generating giver vector.X..")
  
  if (giverType == "double"){
    giverz <- giverDouble(k,y,id,dec_num)
    
    print("Generating matrices...")
    
    interact <- generateSituationSetB(X)
    X <- model.matrix(~(interact*giverz) )
  } else if (giverType == "triple"){
    
    giverz <- giverTriple(k,K,y,id,dec_num)
    high <- giverz[[1]]
    low <- giverz[[2]]
    ng <- giverz[[3]]
    View(ng)
    givers <- cbind(high,low)
    print("GeneratiXng matrices...")
    interactX <-generateSituationSetB(X)
    
    X <-model.matrix(~(interactX*givers) )
  }
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }
  
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain,],
                     family="binomial")
  #   model <- my.cv.glmnet(y=y,
  #                      x=X,
  #                      alpha=alpha,
  #                      type.measure="auc",
  #                      foldid=fold_ids,
  #                      family="binomial",
  #                      id = string_id,
  #                      self.interact)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}


runGiverRF <- function(y, X, id, k=1,K=NULL, dec_num, giverType,
                       self.interact = "true", squares = "false",
                       alpha = 0, nfolds = 5, type= "within",testOrder = 'false') {
  
  print("Generating giver vector.X..")
  
  if (giverType == "double"){
    giverz <- giverDouble(k,y,id,dec_num)
    
    print("Generating matrices...")
    
    interact <- generateSituationSetB(X)
    X <- model.matrix(~(interact*giverz) )
  } else if (giverType == "triple"){
    
    giverz <- giverTriple(k,K,y,id,dec_num)
    high <- giverz[[1]]
    low <- giverz[[2]]
    
    givers <- cbind(high,low)
    print("GeneratiXng matrices...")
    interactX <-generateSituationSetB(X)
    
    X <-model.matrix(~(interactX*givers) )
  }
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  
  if (testOrder == 'false'){
    totrain <- unlist(dec_num) %in% 1:15
  } else if (testOrder == 'true'){
    totrain <- unlist(dec_num) %in% 6:20
  }
  
  print("Generating model...")
  d <- data.frame(cbind(X,y)[totrain,])
  d$y <- as.factor(d$y)
  model <- randomForest(y ~ ., data=d, importance=TRUE,proximity=TRUE)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  s <- d[!totrain,]
  yhat <- predict(rf, data.frame(X))[!totrain]
  
  performance(prediction(as.numeric(yhat),as.numeric(ytest)),"auc")@y.values[[1]] 
}

runGiverRegression3D <- function(y, X, id, i,j,k, dec_num, giverType,
                                 self.interact = "true", squares = "false",
                                 alpha = 0, nfolds = 5, type= "within") {
  
  print("Generating giver vector.X..")
  
  
  giverz <- giver3D(i,j,k,y,id,dec_num)
  low <- giverz[[1]]
  med <-  giverz[[2]]
  high <- giverz[[3]]
  freq <- giverz[[4]]
  givers <- cbind(high,med,low)
  print("GeneratiXng matrices...")
  interactX <-generateSituationSetB(X)
  
  X <-model.matrix(~(interactX*givers) )
  
  
  fold_ids <- getFoldIds(id, dec_num, type = type, nfolds)
  
  
  totrain <- unlist(dec_num) %in% 1:15
  
  
  print("Generating model...")
  model <- cv.glmnet(y=y[totrain],
                     x=X[totrain,],
                     alpha=alpha,
                     #weights=wtz[totrain],
                     type.measure="auc",
                     foldid=fold_ids[totrain,],
                     family="binomial")
  #   model <- my.cv.glmnet(y=y,
  #                      x=X,
  #                      alpha=alpha,
  #                      type.measure="auc",
  #                      foldid=fold_ids,
  #                      family="binomial",
  #                      id = string_id,
  #                      self.interact)
  #   
  
  ytest <- y[!totrain]
  xtest <- X[!totrain,]
  return(list(model, ytest,xtest))
}


giver3D <- function(i,j,k,y,id,dec_num){
  id_map <- unlist(lapply(id[[1]], toString))
  string_iterator <- unique(id_map)
  map_big <- data.frame(y,id_map, dec_num)
  low <- c()
  med <- c()
  high <- c()
  freq <- c()
  for (z in 1:length(string_iterator)){
    local.person <- string_iterator[z]
    num_give <- sum(as.numeric(map_big[which(id_map == local.person & dec_num <= 15),]$y)-1)
    localLow <- as.numeric(num_give >= i & num_give < j )
    localMed <- as.numeric(num_give >= j & num_give < k )
    localHigh <- as.numeric(num_give >= k)
    low <- c(low, rep(localLow, 20))
    med <- c(med, rep(localMed, 20))
    high <- c(high, rep(localHigh, 20))
    freq <- c(freq, rep(num_give, 20))
  }
  list(low,med, high, freq)
}

iszero <- function(x){
  if (x==0){
    NA
  }
  else{
    x
  }
}