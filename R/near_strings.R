#' Strings of Near Repeats
#'
#' Identifies cases that are nearby each other in space/time
#'
#' @param dat data frame 
#' @param id string for id variable in data frame (should be unique)
#' @param x string for variable that has the x coordinates
#' @param y string for variable that has the y coordinates
#' @param tim string for variable that has the time stamp (should be numeric or datetime)
#' @param DistThresh scaler for distance threshold (in whatever units x/y are in)
#' @param TimeThresh scaler for time threshold (in whatever units tim is in)
#'
#' @details This function returns strings of cases nearby in space and time. Useful for near-repeat analysis, or to
#' identify potentially duplicate cases. This particular function is memory safe, although uses loops and will be
#' approximately \eqn{O(n^2)} time (or more specifically `choose(n,2)`). Tests I have done 
#' [on my machine](https://andrewpwheeler.com/2017/04/12/identifying-near-repeat-crime-strings-in-r-or-python/) 
#' 5k rows take only ~10 seconds, but ~100k rows takes around 12 minutes with this code.
#' @returns
#' A data frame that contains the ids as row.names, and two columns:
#'  - `CompId`, a unique identifier that lets you collapse original cases together
#'  - `CompNum`, the number of linked cases inside of a component
#' @export
#' @examples
#' # Simplified example showing two clusters
#' s <- c(0,0,0,4,4)
#' ccheck <- c(1,1,1,2,2)
#' dat <- data.frame(x=1:5,y=0,
#'                   ti=s,
#'                   id=1:5)
#' res1 <- near_strings1(dat,'id','x','y','ti',2,1)
#' print(res1)
#' 
#' # If you want to see a larger example, from Dallas Data
#' bmv <- read.csv('https://dl.dropbox.com/s/bpfd3l4ueyhvp7z/TheftFromMV.csv?dl=0')
#' print(Sys.time()) 
#' BigStrings <- near_strings1(dat=bmv,id='incidentnu',x='xcoordinat',
#'                             y='ycoordinat',tim='DateInt',DistThresh=1000,TimeThresh=3)
#' print(Sys.time()) #takes around 10 seconds on my machine
#'
#' ## Not run, but takes around 12 minutes for me
#' #print(Sys.time())
#' #FullStrings <- near_strings1(dat=bmv,id='incidentnu',x='xcoordinat',
#' #                             y='ycoordinat',tim='DateInt',DistThresh=1000,TimeThresh=3)
#' #print(Sys.time())
#'
#' @seealso [near_strings2()], which uses kdtrees, so should be faster with larger data frames, although still may run out of memory, and is not 100% guaranteed to return all nearby strings.
#' @references
#' Wheeler, A. P., Riddell, J. R., & Haberman, C. P. (2021). Breaking the chain: How arrests reduce the probability of near repeat crimes. *Criminal Justice Review*, 46(2), 236-258.
near_strings1 <- function(dat,id,x,y,tim,DistThresh,TimeThresh){
  MyData <- dat[,c(id,x,y,tim)]
  # Double loop
  totrow <- nrow(MyData)
  mrow <- totrow - 1
  res <- vector('list',mrow)
  # Could do this for loop in parallel?
  for (i in 1:mrow){
      ne <- (i+1):totrow
      compare <- MyData[ne,]
      compare$id2 <- MyData[i,id]
      locx <- MyData[i,x]
      locy <- MyData[i,y]
      loct <- MyData[i,tim]
      # Get those within the time threshold first
      dit <- abs(compare[,tim] - loct)
      compare <- compare[dit < TimeThresh,]
      # Now do the distance calculations
      dx <- compare[,x] - locx
      dy <- compare[,y] - locy
      dsp <- sqrt(dx^2 + dy^2)
      cl <- compare[dsp < DistThresh,c(id,'id2'),drop=FALSE]
      res[[i]] <- cl
   }
   pa <- data.frame(do.call(rbind,res))
   #row.names(pa) <- 1:nrow(pa)
   G <- igraph::graph_from_data_frame(pa, directed = FALSE, vertices=MyData[,id])
   CompInfo <- igraph::components(G) #assigning the connected components
   return(data.frame(CompId=CompInfo$membership,CompNum=CompInfo$csize[CompInfo$membership]))
}

# Helper function to turn RANN output into edge list
pairs_nn2 <- function(nn2){
    ids <- nn2$nn.idx
    nr <- nrow(ids)
    max_col <- sum(colSums(ids) > 0)
    ids <- ids[,1:max_col,drop=FALSE]
    x1 <- rep(1:nr,max_col)
    x2 <- c(ids)
    dl <- data.frame(X1 = x1,X2 = x2)
    check <- (dl$X1 < dl$X2) & (dl$X2 > 0) 
    return(dl[check,])
}

#' Strings of Near Repeats using KDtrees
#'
#' Identifies cases that are nearby each other in space/time
#'
#' @param dat data frame 
#' @param id string for id variable in data frame (should be unique)
#' @param x string for variable that has the x coordinates
#' @param y string for variable that has the y coordinates
#' @param tim string for variable that has the time stamp (should be numeric or datetime)
#' @param DistThresh scaler for distance threshold (in whatever units x/y are in)
#' @param TimeThresh scaler for time threshold (in whatever units tim is in)
#' @param k, the k for the max number of neighbors to grab in the nn2 function in RANN package
#' @param eps, the nn2 function returns <=, so to return less (like `near_strings1()`), needs a small fudge factor
#'
#' @details This function returns strings of cases nearby in space and time. Useful for near-repeat analysis, or to
#' identify potentially duplicate cases. This particular function uses kdtrees (from the RANN library).
#' For very large data frames, this will run quite a bit faster than `near_strings1` (although still may run out of memory). 
#' And it is not 100% guaranteed to grab all of the pairs. Tests I have done 
#' [on my machine](https://andrewpwheeler.com/2017/04/12/identifying-near-repeat-crime-strings-in-r-or-python/) 
#' ~100k rows takes around 2 minutes with this code.
#' @returns
#' A data frame that contains the ids as row.names, and two columns:
#'  - `CompId`, a unique identifier that lets you collapse original cases together
#'  - `CompNum`, the number of linked cases inside of a component
#' @export
#' @examples
#' # Simplified example showing two clusters
#' s <- c(0,0,0,4,4)
#' ccheck <- c(1,1,1,2,2)
#' dat <- data.frame(x=1:5,y=0,
#'                   ti=s,
#'                   id=1:5)
#' res1 <- near_strings2(dat,'id','x','y','ti',2,1)
#' print(res1)
#' 
#' # If you want to see a larger example, from Dallas Data
#' bmv <- read.csv('https://dl.dropbox.com/s/bpfd3l4ueyhvp7z/TheftFromMV.csv?dl=0')
#' print(Sys.time()) 
#' BigStrings <- near_strings2(dat=bmv,id='incidentnu',x='xcoordinat',
#'                             y='ycoordinat',tim='DateInt',DistThresh=1000,TimeThresh=3)
#' print(Sys.time()) #very fast
#'
#' ## Not run, but takes around 2 minutes for me
#' #big_bmv <- rbind(bmv,bmv,bmv,bmv,bmv,bmv,bmv) #duplicating data so around 100k rows
#' #big_bmv$id <- 1:nrow(big_bmv)
#' #print(Sys.time())
#' #FullStrings <- near_strings2(dat=big_bmv,id='id',x='xcoordinat',
#' #                             y='ycoordinat',tim='DateInt',DistThresh=1000,TimeThresh=3)
#' #print(Sys.time()) #takes around 
#'
#' @seealso [near_strings1()], which uses loops but is guaranteed to get all pairs of cases and should be memory safe.
#' @references
#' Wheeler, A. P., Riddell, J. R., & Haberman, C. P. (2021). Breaking the chain: How arrests reduce the probability of near repeat crimes. *Criminal Justice Review*, 46(2), 236-258.
near_strings2 <- function(dat,id,x,y,tim,DistThresh,TimeThresh,k=300,eps=0.0001){
    MyData <- dat
    # min neighbors
    mk <- min(nrow(MyData),k)
    #KDtree for distance
    dist_tree <- RANN::nn2(MyData[,c(x,y)],k=mk,treetype="kd",
                           searchtype='radius',radius=DistThresh-eps)
    dist_p <- pairs_nn2(dist_tree)
    dist_p$X1 <- MyData[dist_p$X1,id]
    dist_p$X2 <- MyData[dist_p$X2,id]
    gd <- igraph::graph_from_data_frame(dist_p, directed = FALSE, vertices=MyData[,id])
    #KDtree for time
    dist_time <- RANN::nn2(MyData[,tim],k=mk,treetype="kd",searchtype='radius',radius=TimeThresh-eps)
    dist_t <- pairs_nn2(dist_time)
    dist_t$X1 <- MyData[dist_t$X1,id]
    dist_t$X2 <- MyData[dist_t$X2,id]
    gt <- igraph::graph_from_data_frame(dist_t, directed = FALSE, vertices=MyData[,id])
    #Combined Graph
    G <- igraph::intersection(gd,gt)
    CompInfo <- igraph::components(G) #assigning the connected components
    return(data.frame(CompId=CompInfo$membership,CompNum=CompInfo$csize[CompInfo$membership]))
}