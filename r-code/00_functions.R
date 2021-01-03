# install packages if not present
using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
# usage: using("RCurl","ggplot2","jsonlite","magrittr")






# https://hlplab.wordpress.com/2012/03/20/correlation-plot-matrices-using-the-ellipse-library/

my.plotcorr <- function (corr, outline = FALSE, col = "grey", upper.panel = c("ellipse", "number", "none"), lower.panel = c("ellipse", "number", "none"), diag = c("none", "ellipse", "number"), digits = 2, bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), cex = 0.75 * par("cex"), mar = 0.1 + c(2, 2, 4, 2), ...)
{
  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible()
}



#The helper function below will be used to calculate the mean and the standard deviation, for the variable of interest, in each group :

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the variable of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}


normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}



#Returns AUC from GLMs
# http://rstudio-pubs-static.s3.amazonaws.com/404104_5f4fc303426e4310bd8d43a45a7ad5f3.html
do_auc <- function(model, the_data){
  p <- predict(model, newdata=the_data, type="response")
  pr <- prediction(p, the_data$y)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  #print(plot(prf))
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return (auc)
}

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df)
}


scale01 <- function(x){(x-min(x))/(max(x)-min(x))}




# Max. combinations of environmental predictors
#
# Scripts allows to select environmental predictor based on
# subjective judgement using ecological expertise (interactive)
# Script takes correlation into account! (data.frames: predictors.cor -> df -> xy)
# combination is saved in "y"

# creation date: 19.04.2013



PredictorSelection<-function(predictors.cor){
  df<-cbind(row.names(predictors.cor),predictors.cor)
  colnames(df)<-c("pred","predcor")
  rownames(df)<-1:nrow(df)
  
  
  
  # SELECTION starts here:
  
  # get rid of factors!, sonst Wanrmeldung beim letzten loop:
  # Warnmeldung:
  # In `[<-.factor`(`*tmp*`, iseq, value = 
  # "rs13,rs265,bgl,fk_klasse,nfk_klasse,kak_klasse,kfa_klasse,natveg,kultpfla,fipu_a_ln,fipu_o_ln,fipu_s_ln") :
  #  invalid factor level, NAs generated
  
  xy<-data.frame(lapply(df,as.character),stringsAsFactors=FALSE)
  
  
  # SELECT variables with 0 correlation
  y<-as.vector(xy[xy[,2]==0,1])
  xy<-xy[-which(xy[,2]==0),] # delete selected variables
  
  
  gg<-NULL
  while(nrow(xy)>0){
    cat("\nLeft variables:",nrow(xy),"\n")
    print(as.vector(xy[,1]))
    #print(xy)
    
    x<-readline("\nSelect an additional environmental variable! ")
    while(!(x %in% xy[,1])){
      x<-readline("\nWrong spelling - please try again! ")
    }
    
    
    # STEP 1: add variable to selection
    y<-c(y,x)
    
    # STEP 2: delete correlated variables from dataframe
    x.cor1<-strsplit(as.vector(xy[which(xy[,1]==x),2]),"\\,") # select corr variables
    x.cor2<-which(xy[,1] %in% x.cor1[1][[1]]) # which rows?
    if(length(x.cor2)!=0){
      xy<-xy[-x.cor2,] # delete selected variables (their rows)
    }
    # STEP 3: delete variable x
    xy<-xy[-which(xy[,1] %in% x),]
    
    
    
    # Step 4: delete correlated variables from the "correlated variables column" of other variables
    if(nrow(xy)>0){
      for (i in 1:nrow(xy)){
        g<-strsplit(as.vector(xy[i,2]),"\\,")[[1]] # splitted string of correlated variables(column 2) belonging to variable i
        xy[i,2]<-paste(g[!(g %in% x.cor1[1][[1]])],collapse=",") # get 
        if(length(g[!(g %in% x.cor1[1][[1]])])==0){ # if variable has 0 correlated variables -> add it to y
          y<-c(y,as.vector(xy[i,1]))
          gg<-c(gg,which(xy[,1] %in% xy[i,1])) # record rownumber of variable to be dropped
        }
        
      }
      if (length(gg)>0){
        xy<-xy[-gg,]
        gg<-NULL}
    }
    
  }
  
  
  
  pred.uncor<-y
  print(pred.uncor)
  
  return(pred.uncor)
}





