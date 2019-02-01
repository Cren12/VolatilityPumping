packages <- c('TTR',
              'xts',
              'foreach',
              'magrittr')

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

VolatilitySlope <- function(
  ohlc # Object that is coercible to xts or matrix and contains Open-High-Low-Close prices
)
{
  vol.curve <- foreach(n = seq(from = 20, to = 200, by = 5), .combine = c) %do%
  {
    # +------------------------------------------------------------------
    # | The Yang and Zhang historical volatility estimator has minimum 
    # | estimation error, and is independent of drift and opening gaps.
    # | It can be interpreted as a weighted average of the Rogers and 
    # | Satchell estimator, the close-open volatility, and the open-close
    # | volatility.
    # +------------------------------------------------------------------
    
    vol.knot <- TTR::volatility(OHLC = ohlc,
                                n = n,
                                calc = 'yang.zhang',
                                mean0 = TRUE)
    
    vol.knot <- vol.knot %>%
      last() %>%
      as.numeric()
    names(vol.knot) <- n
    return(vol.knot)
  }
  
  # +------------------------------------------------------------------
  # | Fits a cubic smoothing spline to the supplied data.
  # +------------------------------------------------------------------
  
  vol.curve <- smooth.spline(x = as.numeric(names(vol.curve)),
                             y = vol.curve)
  
  # +------------------------------------------------------------------
  # | Perform cubic spline interpolation of given data points, 
  # | returning either a function performing the interpolation.
  # +------------------------------------------------------------------
  
  vol.curve.fn <- splinefun(x = vol.curve$x,
                            y = vol.curve$y)
  
  vol.curve.fn(vol.curve$x, deriv = 1) %>%
    mean(na.rm = TRUE) %>%
    return()
}