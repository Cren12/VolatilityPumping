packages <- c('magrittr',
              'quantmod',
              'GAS',
              'depmixS4')

# +------------------------------------------------------------------
# | library and require load and attach add-on packages. Download and
# | install packages from CRAN-like repositories.
# +------------------------------------------------------------------

lapply(X = packages,
       FUN = function(package){
         if (!require(package = package,
                      character.only = TRUE))
         {
           install.packages(pkgs = package,
                            repos = "https://cloud.r-project.org")
           library(package = package,
                   character.only = TRUE)
         } else {
           library(package = package,
                   character.only = TRUE)    
         }
       })

# +------------------------------------------------------------------
# | Sys.setenv sets environment variables.
# +------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

# source()

# +------------------------------------------------------------------

GASmean <- function(
  ohlc
)
{
  price <- ohlc %>%
    rowMeans() %>%
    reclass(match.to = ohlc)
  dx <- Delt(price) ; dx[1] <- 0
  
  # +------------------------------------------------------------------
  # | Specify the conditional distribution, scaling mechanism and 
  # | time–varying parameters for univariate GAS models.
  # +------------------------------------------------------------------
  
  gas.spec <- UniGASSpec(GASPar = list(location = TRUE,
                                       scale = TRUE))
  
  # +------------------------------------------------------------------
  # | Estimate univariate GAS models by Maximum Likelihood.
  # +------------------------------------------------------------------
  
  gas.fit <- UniGASFit(GASSpec = gas.spec,
                       data = dx)
  
  # +------------------------------------------------------------------
  # | Extract conditional moments.
  # +------------------------------------------------------------------
  
  moments <- getMoments(object = gas.fit)
  
  moments[, 1] %>%
    last() %>%
    sign() %>%
    return()
}