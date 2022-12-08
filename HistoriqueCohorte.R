packages <- c("ggplot2", "viridis")

package.check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(x)
    library(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
  }
})
print("Package loaded succesfully!")


results <- read.csv("/home/guyllaume/Desktop/OptiFly/Résultat/expérience1.csv",header = TRUE)[1:10,1:9]


fill_date <- function(x){
  if(x[length(x)]=="TRUE"){
    x[is.na(x)] <- x[length(x)-1]
  }
    return(x)
}

filled_results <- as.data.frame(t(apply(results, MARGIN = 1, FUN = function(x) fill_date(x))))

for(i in 2:(ncol(filled_results)-1)){
  filled_results[,i] = as.Date(filled_results[,i], format = "%Y-%m-%d")
}

filled_results[is.na(filled_results)] <- Sys.Date()
temps <- data.frame(preponte = as.numeric(filled_results[,3]-filled_results[,2]),
           oeuf = as.numeric(filled_results[,4]-filled_results[,3]),
           neonate = as.numeric(filled_results[,5]-filled_results[,4]),
           larve = as.numeric(filled_results[,6]-filled_results[,5]),
           pupe = as.numeric(filled_results[,7]-filled_results[,6]),
           adulte = as.numeric(filled_results[,8]-filled_results[,7]))


filled_results <- cbind(filled_results[,1],temps,filled_results[9])
colnames(filled_results) <- c("couple",colnames(temps),"elim")

filled_results <- tidyr::gather(data = filled_results,key = "etat", value = "jour",2:7)

colorFill <- c("adulte"   = viridis(6)[1],
               "pupe"     = viridis(6)[2],
               "larve"    = viridis(6)[3],
               "neonate"  = viridis(6)[4],
               "oeuf"     = viridis(6)[5],
               "preponte" = viridis(6)[6])



plt <- ggplot(data = filled_results, aes(y=fl))
plt +
  geom_bar(aes(x=factor(couple,level = unique(couple)),
               y=jour,
               fill = factor(etat,
                             level = c("adulte",
                                       "pupe",
                                       "larve",
                                       "neonate",
                                       "oeuf",
                                       "preponte"))),
           stat="identity") +
  scale_y_continuous(limits = c(0, 60)) +
  labs(
    title = paste("Historique des cohortes"),
    y = "Nombre de jour",
    x = "Cohorte",
    fill = "Stade"
  ) +
  scale_fill_manual(breaks = c("adulte",
                               "pupe",
                               "larve",
                               "neonate",
                               "oeuf",
                               "preponte"),
                    values = magma(7))+
  coord_flip() +
  annotate("text", x=filled_results[filled_results$elim == TRUE,]$couple, y=5,
           label= "bold(Retiré)", parse = TRUE, col="darkred", size=7)
  
