package <- c("quantmod",
             "devtools",
             "PerformanceAnalytics")

# +------------------------------------------------------------------
# | library and require load and attach add-on packages. Download and
# | install packages from CRAN-like repositories.
# +------------------------------------------------------------------

lapply(X = package,
       FUN = function(this.package){
         if (!require(package = this.package,
                      character.only = TRUE))
         {
           install.packages(pkgs = this.package,
                            repos = "https://cloud.r-project.org")
           library(package = this.package,
                   character.only = TRUE)
         } else {
           library(package = this.package,
                   character.only = TRUE)    
         }
       })
# install_github("braverock/FinancialInstrument")
# install_github("braverock/blotter")
# install_github("braverock/quantstrat")
# install_github("braverock/PerformanceAnalytics")
require(quantstrat)

# +------------------------------------------------------------------
# | Sys.setenv sets environment variables.
# +------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | source() causes R to accept its input from the named file or URL
# | or connection or expressions directly.
# +------------------------------------------------------------------

source('GASmean.R')
source('getSymbolsFromBloomberg.R')
source('HalfLogSize.R')
source('osVarSize.R')
source('WinDoPar.R')

# +------------------------------------------------------------------

Symbols <- c('TLT')

# +------------------------------------------------------------------
# | Functions to load and manage Symbols in specified environment. 
# | Called for its side-effect with env set to a valid environment 
# | and auto.assign=TRUE, getSymbols will load into the specified env
# | one object for each Symbol specified, with class defined by 
# | return.class.
# +------------------------------------------------------------------

getSymbols(Symbols = Symbols,
           from = Sys.Date() - 365 * 30)
# getSymbolsFromBloomberg(securities = Symbols,
#                         start.date = Sys.Date() - 365 * 30)

name <- 'Trading'
currency <- 'USD'
initEq <- 50000 * length(Symbols)
TxnFees <- 0
Sys.setenv(TZ = 'UTC')

# +------------------------------------------------------------------
# | Remove the order_book, account, and portfolio of given name.
# +------------------------------------------------------------------

rm.strat(name = name)

# +------------------------------------------------------------------
# | Constructs and initializes a portfolio object, which is used to
# | contain transactions, positions, and aggregate level values.
# +------------------------------------------------------------------

initPortf(name = name,
          symbols = Symbols,
          currency = currency)

# +------------------------------------------------------------------
# | Inputs portfolios: a list of portfolio object names to attach to
# | the account. initDate: date prior to the first close price given,
# | used to contain initial account equity and initial position.
# | initEq: initial equity or starting capital, default is 100,000.
# +------------------------------------------------------------------

initAcct(name = name,
         portfolios = name,
         initEq = initEq)

# +------------------------------------------------------------------
# | This function sets up the order container by portfolio.
# +------------------------------------------------------------------

initOrders(portfolio = name,
           symbols = Symbols)

# +------------------------------------------------------------------
# | All 'currency' instruments must be defined before instruments of
# | other types may be defined.
# +------------------------------------------------------------------

currency(primary_id = currency)

# +------------------------------------------------------------------
# | Variables passed in dots will be added to the strategy object, 
# | and may be used by initialization and wrapup functions, as well 
# | as indicators, signals, and rules.
# +------------------------------------------------------------------

strategy(name = name,
         store = TRUE)

for(primary_id in Symbols)
{
  stock(primary_id = primary_id,
        currency = currency)
}

# +------------------------------------------------------------------
# | Indicators are typically standard technical or statistical
# | analysis outputs, such as moving averages, bands, or pricing 
# | models.
# +------------------------------------------------------------------

add.indicator(strategy = name,
              name = 'WinDoPar',
              arguments = list(x = quote(Cl(mktdata)),
                               n = 25,
                               w = 'exp',
                               fun = PercRank),
              label = 'rank',
              store = TRUE)

# +------------------------------------------------------------------
# | This adds a signal definition to a strategy object.
# +------------------------------------------------------------------

# add.signal(strategy = name,
#            name = 'sigCrossover',
#            arguments = list(data = quote(mktdata),
#                             columns = c('sigma.fast', 'sigma.slow'),
#                             relationship = 'gte',
#                             cross = FALSE),
#            label = 'rebalance',
#            store = TRUE)
add.signal(strategy = name,
           name = 'sigThreshold',
           arguments = list(data = quote(mktdata),
                            column = 'Close',
                            relationship = 'gte',
                            threshold = 0,
                            cross = FALSE),
           label = 'rebalance',
           store = TRUE)

# +------------------------------------------------------------------
# | Rules will be processed in a very particular manner, so it bears
# | going over.
# +------------------------------------------------------------------

add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'rebalance',
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = 'market',
                          orderside = 'long',
                          replace = TRUE,
                          osFUN = osVarSize,
                          acct.name = name,
                          col.name = 'X1.rank',
                          TxnFees = TxnFees),
         label = 'rebalance.buy',
         type = 'enter',
         store = TRUE)
add.rule(strategy = name,
         name = 'ruleSignal',
         arguments = list(sigcol = 'rebalance',
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = 'market',
                          orderside = 'long',
                          replace = TRUE,
                          osFUN = osVarSize,
                          acct.name = name,
                          col.name = 'X1.rank',
                          TxnFees = TxnFees),
         label = 'rebalance.sell',
         type = 'exit',
         store = TRUE)
# add.rule(strategy = name,
#          name = 'ruleSignal',
#          arguments = list(sigcol = 'rebalance',
#                           sigval = TRUE,
#                           orderqty = 'all',
#                           ordertype = 'stoplimit', # stoplimit # stoptrailing
#                           orderside = 'long',
#                           orderset = 'stop',
#                           threshold = quote(-mktdata[timestamp, 'X1.sigma.fast']),
#                           tmult = TRUE,
#                           TxnFees = TxnFees,
#                           prefer = 'Low'),
#          label = 'rebalance.buy.chain',
#          type = 'chain',
#          parent = 'rebalance.buy',
#          store = TRUE)

# +------------------------------------------------------------------
# | This function is the wrapper that holds together the execution of
# | a strategy.
# +------------------------------------------------------------------

applyStrategy(strategy = name,
              portfolios = name)

# +------------------------------------------------------------------
# | The updatePortf function goes through each symbol and calculates
# | the PL for each period prices are available.
# +------------------------------------------------------------------

updatePortf(Portfolio = name,
            Symbols = Symbols)

# +------------------------------------------------------------------
# | Constructs the equity account calculations from the portfolio 
# | data and corresponding close prices.
# +------------------------------------------------------------------

updateAcct(name = name)

# +------------------------------------------------------------------
# | Calculates End.Eq and Net.Performance.
# +------------------------------------------------------------------

updateEndEq(Account = name)

for (symbol in Symbols)
{
  dev.new()
  
  # +------------------------------------------------------------------
  # | Produces a three-panel chart of time series charts that contains
  # | prices and transactions in the top panel, the resulting position
  # | in the second, and a cumulative profit-loss line chart in the 
  # | third.
  # +------------------------------------------------------------------
  
  try(chart.Posn(Portfolio = name,
                 Symbol = symbol))
}

# +------------------------------------------------------------------
# | This function (for now) calculates return on initial equity for 
# | each instrument in the portfolio or portfolios that make up an 
# | account. These columns will be additive to return on capital of 
# | each portfolio, or of the entire account.
# +------------------------------------------------------------------

R <- PortfReturns(Account = name)

R$Tot.DailyEqPl <- rowMeans(R)

# +------------------------------------------------------------------
# | Retrieves an account object from the .blotter environment. Useful
# | for local examination or charting, or storing interim results for
# | later reference.
# +------------------------------------------------------------------

account <- getAccount(Account = name)

dev.new()
plot(cumsum(account$summary$Realized.PL)[cumsum(account$summary$Realized.PL) != 0], main = 'Realized PL', lwd = 1)
dev.new()

# +------------------------------------------------------------------
# | Get a portfolio object conssting of either a nested list. 43
# | Portfolios in blotter are stored as a set of nested, hashed,
# | environments.
# +------------------------------------------------------------------

portfolio <- blotter::getPortfolio(name)
x <- na.omit(merge(Cl(get(Symbols)), portfolio$symbols[[1]]$txn$Pos.Avg.Cost)[-1, ])
plot.xts(x, legend.loc = 'topleft', lwd = 1, main = '')
dev.new()

# +------------------------------------------------------------------
# | For a set of returns, create a wealth index chart, bars for 
# | per-period performance, and underwater chart for drawdown.
# +------------------------------------------------------------------

charts.PerformanceSummary(R = R$Tot.DailyEqPl,
                          geometric = FALSE,
                          main = 'Trading performance')

# +------------------------------------------------------------------
# | This function calculates trade-level statistics on a symbol or 
# | symbols within a portfolio or portfolios.
# +------------------------------------------------------------------

as.data.frame(t(tradeStats(Portfolios = name)))
