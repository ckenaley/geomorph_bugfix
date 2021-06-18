#' @title Read single *.nts file containing landmark coordinates for a single specimen, avoiding the bugs of \code{geomorph::readland.nts}
#'
#' @description A function modified from \code{geomorph::readland.nts} to take a single .nts file and return an array that can be read by the \code{geomorph} package.
#' 
#' @param file Path to the input file
#' @return Modified from \code{geomorph::readland.nts} . . . Function returns a 3D array (p x k x n=1), where p is the number of landmark points, k is the number of landmark dimensions (2 or 3), and n is the number of specimens (one in this limited case). The third dimension of this array contains a name for the specimen which is obtained from the names in the *.nts file.
#' 
#' @seealso \code{geomorph::readland.nts}
#' 
#' @export
#' 
#' @examples	
#' 
#' #load a single nts file
#' 
#' file <- system.file("extdata", "echiostoma.nts", package = "geomorphcompanion")
#' nts.file <- readland.from1.nts(file=file)
#' print(nts.file)

readland.from1.nts <- function(file){
  ntsfile <- scan(file = file, what = "char", quote = "", sep = "\n", 
                  strip.white = TRUE, comment.char = "\"", quiet = TRUE)
  comment <- grep("'", ntsfile)
  if (length(comment) != 0) {
    ntsfile <- scan(file = file, what = "char", quote = "", 
                    sep = "\n", strip.white = TRUE, comment.char = "'", 
                    quiet = TRUE)
  }
  header <- unlist(strsplit(ntsfile[1], "\\s+"))
  if (header[1] != 1) {
    stop("NTS file not a rectangular matrix. First value in parameter line must be '1'.")
  }
  header <- casefold(header, upper = TRUE)
  dimval <- unlist(grep("DIM=", header))
  if (length(dimval) == 0) {
    stop("Header does not contain 'DIM=' designator.")
  }
  
  #this is screwy. digit.fixed returns no value with L in header and label is omitted from both scan operations above.
  #labval <- unlist(grep("L", header))
  #r.lab <- ifelse(is.element("2", labval) == TRUE, T, F)
  #c.lab <- ifelse(is.element("3", labval) == TRUE, T, F)
  

  header <- sub("L", "", header)
  header <- as.numeric(sub("DIM=", "", header))
  missdata <- ifelse(header[4] != 0, T, F)
  if (missdata == TRUE) {
    missval <- ifelse(dimval == 6, header[5], header[6])
  }
  
  #this is weird and doesn't match any description of NTS paramters
  #n <- header[2] #header[2] is p!
  #k <- header[dimval] #isn't k header 3?!?!
  #p <- header[3]/k
  
  #let's just change n to 1 (assume NTS file contains one speciemen) and k=DIM and p really does equal the poinst parameter in the header
  n <- header[1]
  k <- header[dimval]
  p <- header[2]
  
  
  tmp <- unlist(strsplit(ntsfile[-1], "\\s+"))
  # if (r.lab) {
  #   speclab <- tmp[1:n]
  #   tmp <- tmp[-(1:n)]
  # }else speclab <- NULL
  # if (c.lab) 
  #   tmp <- tmp[-(1:(p * k))]
  # if (missdata == TRUE) {
  #   tmp[grep(missval, as.integer(tmp))] <- NA
  # }
  
  speclab <- scan(file = file, what = "char",sep="\n",quiet = TRUE)[1]
  
  options(warn = -1)
  landdata <- matrix(as.numeric(tmp), ncol = k, byrow = TRUE)
  if (sum(which(is.na(landdata) == TRUE)) > 0) {
    cat("NOTE.  Missing data identified.")
  }
  

  coords <- aperm(array(t(landdata), c(k, p, n)), c(2, 1, 3))
  
  #specimen lable
  
  
  if (length(speclab) == 1) {
    dimnames(coords)[[3]] <- list(speclab)
  }else {
    dimnames(coords)[[3]] <- speclab
  }
  return(coords)
}

#' @title  Read multiple nts files.
#' 
#' @description  Read multiple nts (or .dta) files that reflect one specimen to obtain landmark coordinates and combine them into a single array. Avoiding the bugs of \code{geomorph::readland.nts}, about which \code{geomorph::readlmulti.nts} is wrapped

#' @param filelist A vector containing the file paths to all the .nts files to be compiled
#' 
#' @details This is a wrapper of \link{readland.from1.nts} to allow reading landmark coordinates, in 2D or 3D, from several nts (or .dta) files each containing one specimen, and compiling them into an array for proceeding with \code{geomorph} procedures.
#' 
#' @return Modified from \code{geomorph::readmulit.nts} . . . Function returns a 3D array (p x k x n), where p is the number of landmark points, k is the number of landmark dimensions (2 or 3), and n is the number of specimens. The third dimension of this array contains names for each specimen, which are retrieved from the nts specimen labels, if those are available. 
#'
#' @seealso \code{geomorph::readmulti.nts} 
#' 
#' @export
#' 
#' @examples
#' 
#' library(geomorph)
#' #load a two nts files
#' 
#' file1 <- system.file("extdata","echiostoma.nts",package = "geomorphcompanion")
#' file2 <- system.file("extdata","echiostoma2.nts",package = "geomorphcompanion")
#' files <- c(file1,file2)
#' nts.file <- readmulti.from1.nts(filelist=files)
#' print(nts.file)
#' 
readmulti.from1.nts <- function (filelist) 
{
  nts.list <- filelist
  file.ext <- substr(nts.list, nchar(nts.list) - 3, nchar(nts.list))
  if (!all(file.ext %in% c(".nts", ".NTS", ".dta", ".DTA"))) 
    stop("File list includes files in a format other than nts or dta, please ammend")
  dt.dims <- sapply(1:length(nts.list), function(x) 
    dim(readland.from1.nts(nts.list[x])),simplify = TRUE)
  p1 <- dt.dims[1, 1]
  k1 <- dt.dims[2, 1]
  n1 <- dt.dims[3, 1]
  if (any(dt.dims[1, ] != p1)) 
    stop("Input tps files include different numbers of landmarks, please correct")
  if (any(dt.dims[2, ] != k1)) 
    stop("Input tps files include landmarks in different dimensions (2D and 3D), please correct")
  all.lms <-list()   #use a list instead of array
  for (f in 1:length(nts.list)) {
    all.lms[[f]] <- readland.from1.nts(nts.list[f])
  }

  all.lms <- do.call(rbind,all.lms)
  all.lms <- geomorph::arrayspecs(all.lms, p1, k1)
  dimnames(all.lms)[[3]] <- basename(nts.list)
  
  return(all.lms)
}


