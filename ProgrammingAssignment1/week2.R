##Return the mean of pollutant across all monitors mentioned as id. Files are in specdata folder.
pollutantmean <- function(directory, pollutant, id=1:332) {
        
        files <- list.files(directory, full.names = TRUE)
        final <- data.frame()
        for (i in id) {
            final <- rbind(final, read.csv(files[i]))
        }
  
        mean_final <- mean(final[,pollutant], na.rm = TRUE)
        mean_final
}

##generate a table with id and total number of complete case observations without NAs or NaNs
complete <- function(directory, id=1:332) {
                  
        files <- list.files(directory, full.names = TRUE)
        final <- data.frame()
        for (i in id) {
          temp <- read.csv(files[i])
          final <- rbind(final, data.frame(i, sum(complete.cases(temp))))
        }
      colnames(final) <- c("id", "nobs")  
      final
}

corr <- function(directory, threshold = 0) {
        files <- list.files(directory, full.names = TRUE)
        dat <- vector(mode = "numeric", length = 0)
        for (i in 1:length(files)) {
          temp <- read.csv(files[i])
          csum <- sum((!is.na(temp$sulfate)) & (!is.na(temp$nitrate)))
          if (csum > threshold) {
              #Extract data of niteate and sulfate and calculate correlation between them
            sul <- temp[which(!is.na(temp$sulfate)), ]
            nit <- sul[which(!is.na(sul$nitrate)), ]
            dat <- c(dat, cor(nit$sulfate, nit$nitrate))
          }
        }
      dat  
}

