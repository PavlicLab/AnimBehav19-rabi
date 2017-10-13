## ---- echo=FALSE, collapse=TRUE,comment=""-------------------------------
cat("     |   |            |  {5}\n     o   o            o   o\n     |   |     =>    {1} {3}\n     .   .           {2} {4}\n    /|\\ /|\\          /|\\ /|\\")


## ------------------------------------------------------------------------
alphabet <- 8      # the number of colors we have
total_length <- 5  # the number of positions we want band
redundancy <- 2    # how many bands we can lose but still ID perfectly

codes <- rabi::reed_solomon(total_length, redundancy, alphabet)
head(codes, n = 4L)


## ---- echo=FALSE, collapse=TRUE,comment=""-------------------------------
cat("     |  {5}           |  {5}           |  {5}\n     o   o            o   o            o   o\n     |  {3}   AND     |  {3}    =>     |  {3}\n    {1} {4}          {2} {4}          {?} {4}\n    /|\\ /|\\          /|\\ /|\\          /|\\ /|\\")


## ------------------------------------------------------------------------
codes <- rabi::reed_solomon(total_length = 5, redundancy = 2, alphabet = 8)
 #turn the list of codes into a matrix for ease of editing
codes <- t(do.call("cbind",codes))  
 #now only select the codes where {1} and {3} are not smaller than their lower partners {2} and {4}
codes <- codes[which(codes[,1] >= codes[,2] & codes[,3] >= codes[,4]), ]
 #turn it back into a list...
codes <- split(codes, 1:nrow(codes))
names(codes) <- NULL
head(codes, n = 4L)


## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

