###############################################################################
#
#       test the overall and local difference in spatial point patterns
#    
###############################################################################

diff.aspp <- function(null,
                      alternative,
                      method = "friedman",
                      nr_sq = 10, 
                      window,
                      title = "",
                      ribbon = TRUE,
                      cex.axis = 1,
                      cex.lab = 1,
                      col = c("blue", "white", "red"),
                      testonly = FALSE){
            if (!is(null, "list")) {
              stop("Input (null) is of wrong class.")
            }
            if (!is(alternative, "list")) {
              stop("Input (alternative) is of wrong class.")
            }
            if(length(null) != length(alternative)) {
              stop("Number of elements is different in data sets.")
            }
            if (!is(method, "character")) {
              stop("Input (method) is of wrong class.")
            }
            if (!is(method, "character")) {
              if (!(method %in% c("friedman",
                                  "wilcox"))) {
                stop("Input (method) ill-specified.")
              }
            } 
            if (!is(nr_sq, "numeric")) {
              stop("Input (nr_sq) is of wrong class.")
            }
            if (length(nr_sq) != 1) {
              stop("Input (nr_sq) is of wrong length.")
            }
            if (is.na(nr_sq)) {
              stop("Input (nr_sq) is not a positive integer.")
            }
            if (nr_sq < 0) {
              stop("Input (nr_sq) is not a positive integer.")
            }
            if (!is(title, "character")) {
              stop("Input (title) is of wrong class.")
            }
            if (!is(window, "owin")) {
              stop("Input (window) is of wrong class.")
            }
            if (!is(ribbon, "logical")) {
              stop("Input (ribbon) is of wrong class.")
            }
            if (!is(cex.axis, "numeric")) {
              stop("Input (cex.axis) is of wrong class.")
            }
            if (length(cex.axis) != 1) {
              stop("Input (cex.axis) is of wrong length.")
            }
            if (is.na(cex.axis)) {
              stop("Input (cex.axis) is not a positive integer.")
            }
            if (cex.axis < 0) {
              stop("Input (cex.axis) is not a positive integer.")
            }
            if (!is(cex.lab, "numeric")) {
              stop("Input (cex.lab) is of wrong class.")
            }
            if (length(cex.lab) != 1) {
              stop("Input (cex.lab) is of wrong length.")
            }
            if (is.na(cex.lab)) {
              stop("Input (cex.lab) is not a positive integer.")
            }
            if (cex.lab < 0) {
              stop("Input (cex.lab) is not a positive integer.")
            }
            if (!is(col, "character")) {
              stop("Input (col) is of wrong class.")
            }
            if (!is(testonly, "logical")) {
              stop("Input (testonly) is of wrong class.")
            }
            
            a <- b <- result <- numeric()
            for(i in 1:length(null)){
              a <- rbind(a, quadratcount(ppp(null[[i]]$X,
                                             null[[i]]$Y, 
                                             window = window), 
                                         nx = nr_sq, 
                                         ny = nr_sq)
              )
              b <- rbind(b, quadratcount(ppp(alternative[[i]]$X, 
                                             alternative[[i]]$Y, 
                                             window = window),
                                         nx = nr_sq,
                                         ny = nr_sq)
              )
            }
            for(i in 1:ncol(a)){
              if(method == "friedman"){
                result[i] <-  friedman.test(cbind(a[,i],
                                                  b[,i]))$p.value
              }
              if(method == "wilcox"){
                result[i] <-  wilcox.exact(a[,i],
                                           b[,i], 
                                           paired = TRUE,
                                           alternative = "greater", 
                                           exact = FALSE)$p.value
              }   
            }
            null_all <- do.call("rbind", null)
            alt_all <- do.call("rbind", alternative)
            
            t1 <- ppp(null_all$X, 
                      null_all$Y,
                      window = window
            )
            t2 <- ppp(alt_all$X,
                      alt_all$Y, 
                      window = window
            )
            k2 <- density(t2,
                          sigma = 15
            ) 
            if(!testonly){
              p <- plot(k2, 
                        las = 1,
                        main = title,
                        axes = TRUE,
                        box = FALSE,
                        ribbon = ribbon,
                        cex.axis = cex.axis,
                        cex.lab = cex.lab,
                        col = colorRampPalette(col)(n = 255)
              )
              contour(k2, 
                      add = TRUE
              )
            }
            Q1 <- quadratcount(t1,
                               nx = nr_sq,
                               ny = nr_sq
            )
            Q2 <- quadrat.test(t2,
                               nx = nr_sq,
                               ny = nr_sq
            )
            Q2$expected <- Q1
            #padj <- p.adjust(result, "fdr")
            result[is.nan(result)] <- 1
            Q2$residuals <- result #padj
            if(!testonly){
              p <- plot(Q2,
                        add = TRUE,
                        col = ifelse(result < 0.05, "lawngreen", 'white'),
                        cex = ifelse(result < 0.05, 1.3, 1.1)
              )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 1,
                    line = 2
              )
              mtext(expression(paste("Distance ", "(", mu, "m)")),
                    side = 2, 
                    line = 2
              )
              if(ribbon){
                mtext("Kernel Density", side = 4, line = 1, cex = cex.lab)
              }
            }
            treated = as.numeric(Q2$observed)
            untreated = as.numeric(Q2$expected)
            if(method == "friedman"){
              frid = friedman.test(cbind(treated, untreated))
              text(300, 450, 
                   paste("Friedman rank sum test:","\n p-value =", as.character(frid$p.value)),
                   cex = 1
              )
              if(!testonly){
                return(list(l1 = frid,
                            l1 = p))
              } else{
                return(frid)
              }
            }
            if(method == "wilcox"){
              wilc = wilcox.exact(treated,
                                  untreated,
                                  paired = TRUE,
                                  alternative = "greater",
                                  exact = FALSE
              )
              text(300, 450, 
                   paste("Friedman rank sum test:","\n p-value =", as.character(frid$p.value)),
                   cex = 1
              )
              if(!testonly){
                return(list(l1 = wilc,
                            l1 = p))
              } else{
                return(wilc)
              }
            }
}  
