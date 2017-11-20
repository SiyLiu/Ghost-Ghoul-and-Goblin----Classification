## Functions
# Put multiple plots on one page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Cross-Validation for LDA, QDA

cv = function(num_fold,formula,data,cl,fun){
  grps = cut(1:nrow(data),num_fold,labels=FALSE)[sample(1:nrow(data))]
  pred = lapply(1:num_fold,
                function(i,formula,data){
                  omit = which(grps == i)
                  model.fun = function(...)fun(...)
                  z = model.fun(formula,data=data[-omit,])
                  predict(z,data[omit,])
                },formula,data)
  
  pred.res = unlist(lapply(pred,function(pp)pp$class))
  confusion.m = table(pred.res,cl[order(grps)])
  error = 1- sum(diag(confusion.m)/371)
  return(error)
  
}

# Cross Validation for Logistic
cv.logistic = function(num_fold,formula,data,cl,fun){
  grps = cut(1:nrow(data),num_fold,labels=FALSE)[sample(1:nrow(data))]
  model = function(i,formula,data){
    omit = which(grps == i)
    model.fun = function(...)fun(...)
    z = model.fun(formula,data=data[-omit,])
    predict(z,data[omit,])
  }
  pred = lapply(1:num_fold,model,formula,data)
  
  #pred.res = unlist(lapply(pred,function(pp)pp$class))
  confusion.m = table(unlist(pred),cl[order(grps)])
  error = 1- sum(diag(confusion.m)/371)
  return(error)
}

# Cross Validation for decision trees

cv.trees = function(num_fold,formula,data,cl,fun){
  grps = cut(1:nrow(data),num_fold,labels=FALSE)[sample(1:nrow(data))]
  model = function(i,formula,data){
    omit = which(grps == i)
    model.fun = function(...)fun(...)
    z = model.fun(formula,data=data[-omit,])
    predict(z,data[omit,],type = "class")
  }
  pred = lapply(1:10,model,formula,Monster)
  pred = unlist(pred)
  
  #pred.res = unlist(lapply(pred,function(pp)pp$class))
  confusion.m = table(pred,cl[order(grps)])
  error = 1- sum(diag(confusion.m)/371)
  return(error)
}

cv.prune.trees = function(num_fold,formula,data,cl,fun){
  grps = cut(1:371,10,labels=FALSE)[sample(1:371)]
  model = function(i){
    omit = which(grps == i)
    z = tree(formula=type~.-color,data=Monster[-omit,])
    cv.z<-cv.tree(z,FUN=prune.misclass,K=10) # 10 folds cv to fin the best # of nodes
    best = cv.z$size[which.min(cv.z$dev)]
    prune.z = prune.tree(z, best = best)
    predict(prune.z,Monster[omit,],type = "class")
  }
  pred = lapply(1:10,model)
  pred = unlist(pred)
  
  #pred.res = unlist(lapply(pred,function(pp)pp$class))
  confusion.m = table(pred,cl[order(grps)])
  error = 1- sum(diag(confusion.m)/371)
  return(error)
}

cv.svm= function(num_fold,formula,data,cl){
  grps = cut(1:nrow(data),num_fold,labels=FALSE)[sample(1:nrow(data))]
  model = function(i){
    omit = which(grps == i)
    z = svm(formula=type~.-color,data=Monster[-omit,], C = 1,kernel="linear")
    predict(z, Monster[omit,])
  }
  pred = lapply(1:10,model)
  pred = unlist(pred)
  
  #pred.res = unlist(lapply(pred,function(pp)pp$class))
  confusion.m = table(pred,cl[order(grps)])
  error = 1- sum(diag(confusion.m)/371)
  return(error)
}