#' Read single *.nts file containing landmark coordinates for a set of specimens, avoiding the bugs of \linl{\code{readland.nts}}
#'
#' A function modified from \link{\code{readland.nts}} to take a single .nts file and return and array that can be read by the \link{\code{geomorph}} package.
#' 
#' @param infile Path to the input file
#' @return Modified from \link{\code{readland.nts}} . . . Function returns a 3D array (p x k x n=1), where p is the number of landmark points, k is the number of landmark dimensions (2 or 3), and n is the number of specimens (one in this limited case). The third dimension of this array contains a name for the specimen which is obtained from the names in the *.nts file.
#' 
#' @seealso \code{readland.nts}
#' @export
#' 
#' @examples	
#' 
#' #load an nts file
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
