# file stuff

fi <- function (filen) {
  info <- file.info(filen)[, c("mtime", "size"), drop = FALSE]
  names(info) <- c("modification_time", "size")
  info <- data.frame(path = filen, info, row.names = NULL)
  print(info)
}

.legdata_attach <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "This function requires: ", paste(missing, collapse = ", "),
      ". Install the missing package(s) and try again.",
      call. = FALSE
    )
  }
  suppressPackageStartupMessages(
    invisible(lapply(packages, library, character.only = TRUE))
  )
}

# mathy convenviencs

qu <- function (x,p=.025) {
    return(quantile(x,probs=p,na.rm=T))
}

seqrange <- function (x,steps=.005,ra=75) {
    return (seq(qu(x,(.5-ra/200)),qu(x,(.5+ra/200)),steps))
}


r<-function(x, dig=2) { return (round(x,dig)) }

plusminus <- function (x, nsd = 1) {
  return(c((mean(x,na.rm=T)-(nsd/2)*sd(x,na.rm=T)),(mean(x,na.rm=T)+(nsd/2)*sd(x,na.rm=T))))
}

minmax <- function (x) {
  return(c(min(x,na.rm=T),max(x,na.rm=T)))
}

# ptile <- function ( x, party ) {
# 
#     if (length(unique(party))==1) {
#       ecdf.d <- ecdf(x[party=="D"])
#       ecdf.r <- NULL
#     } else {
#       ecdf.d <- ecdf(x[party=="D"])
#       ecdf.r <- ecdf(x[party=="R"])
#     }
#     #ecdf. <- ecdf(x)
# 
#     perc = rep(NA,length(x))
#     for (k in 1:length(x)) {
#         if (party[k]=="R") {
#             perc[k]=ecdf.r(x[k])
#         } else {
#             perc[k]=ecdf.d(x[k])                    
#         }
#     }
#     
#     return(perc)
# 
# }

ptile <- function ( x, party ) {
  
  if (length(unique(party))==1) {
    ecdf.d <- ecdf(x[party=="D"])
    ecdf.r <- NULL
  } else {
    ecdf.d <- ecdf(x[party=="D"])
    ecdf.r <- ecdf(x[party=="R"])
  }
  #ecdf. <- ecdf(x)
  
  perc = rep(NA,length(x))
  for (k in 1:length(x)) {
    if (party[k]=="R") {
      perc[k]=ecdf.r(x[k])
    } else {
      perc[k]=ecdf.d(x[k])                    
    }
  }
  
  return(perc)
  
}


rmse = function(m, o){
  sqrt(mean((m - o)^2))
}


stdz <- function (x) {
    return((x-mean(x,na.rm=T))/sd(x,na.rm=T))
}

Mode <- function(x, na.rm=T) {
  ux <- unique(x)
  if (na.rm) { ux = na.omit(ux)[1]}
  ux[which.max(tabulate(match(x, ux)))]
}


signif.pct <- function() {
  env <- sys.frame(-3);
  paste("$^*$ significant at", evalq(lev,env)*100, "percent")
}

fold <- function(f, x, L) (for(e in L) x <- f(x, e))

objsize <- function (object) {
  format(lobstr::obj_size(object))
}
