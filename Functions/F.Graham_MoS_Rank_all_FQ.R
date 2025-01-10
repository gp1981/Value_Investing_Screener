## Author: Gabriele Picheo - 28 March 2019

# INPUT: ROC_EY
# OUTPUT: DF_all_Year_GrahamRank

# Note: Provide a script to calculate DataFrame of calcualtion and stock ranking (Graham No.) 
# for the all available FY in FinancialModellingPrep.com

Graham_MoS_Rank_all_FQ <- function(DF_all_FQ_Return)
{
  
  
  # 01 - Calculate Graham values (Book, Current, Cash) ----------------------
  
  
  DF <- DF_all_FQ_Return %>% 
    # mutate(Avg.Price.PExEPS=(as.numeric(PE.ratio)*as.numeric(EPS.Diluted))) %>% 
    mutate(Outstanding.Shares = as.numeric(marketCap)/(DF.Stock.Price)) %>% #<<-- TO BE CALCULATED with Revenue or cash per share (Check auxiliary script) 
    mutate(Net.Std.Book.Value = (as.numeric(totalAssets) - as.numeric(totalLiabilities))/as.numeric(Outstanding.Shares)) %>% 
    mutate(Net.Book.Value.per.Share.G = 
             (as.numeric(totalAssets)- as.numeric(goodwillAndIntangibleAssets) 
              - as.numeric(totalLiabilities))/as.numeric(Outstanding.Shares)) %>% 
    mutate(Net.Current.Asset.Value.per.Share.G = 
             (as.numeric(totalCurrentAssets) - as.numeric(totalLiabilities))/as.numeric(Outstanding.Shares)) %>% 
    mutate(Net.Cash.Asset.Value.per.Share.G = 
             (as.numeric(cashAndShortTermInvestments)-as.numeric(totalLiabilities))/as.numeric(Outstanding.Shares))
  
  # 02 - Calculate Graham liquidation values ----------------------
  
  
  DF <- DF %>% 
    mutate(Acid.ratio = as.numeric(cashAndShortTermInvestments)/as.numeric(totalCurrentLiabilities)) %>% 
    mutate(Quick.ratio = (as.numeric(cashAndShortTermInvestments)+as.numeric(netReceivables))/as.numeric(totalCurrentLiabilities)) %>% 
    mutate(Net.Current.Asset.over.Debt = (as.numeric(totalCurrentAssets)-as.numeric(totalCurrentLiabilities))/as.numeric(totalDebt)) %>% 
    mutate(Capital.Structure.E_over_D = as.numeric(totalStockholdersEquity)/as.numeric(totalDebt)) %>% 
    mutate(Liq.Receivable_75pc = as.numeric(netReceivables)*0.75) %>% 
    mutate(Liq.Inventory_50pc = as.numeric(inventory)*0.5) %>% 
    mutate(Liq.PPE_15pc = as.numeric(propertyPlantEquipmentNet)*0.15) %>% 
    mutate(Liq.Other.Asset_15pc = as.numeric(otherAssets)*0.15)
  
  DF <- DF %>% 
    mutate(Liq.Value.per.Share.G = ((as.numeric(cashAndShortTermInvestments) + 
                                       Liq.Receivable_75pc +
                                       Liq.Inventory_50pc +
                                       Liq.PPE_15pc +
                                       Liq.Other.Asset_15pc -as.numeric(totalLiabilities)
    )/as.numeric(Outstanding.Shares)))
  
  # 03 - Calculate Graham Margin of Safety ----------------------
  
  
  DF <- DF %>% 
    mutate(MoS = Liq.Value.per.Share.G - DF.Stock.Price) %>% 
    mutate(MoS_Percentage = (Liq.Value.per.Share.G - as.numeric(price))/as.numeric(price))
  
  DF <- DF %>% 
    select(Ticker,date,historical.date,quarter,historical.adjClose,DF.Stock.Price,
           Net.Std.Book.Value,Net.Book.Value.per.Share.G,Net.Current.Asset.Value.per.Share.G,
           Net.Cash.Asset.Value.per.Share.G, Liq.Value.per.Share.G, everything())
  
  
  
  return(DF)
}