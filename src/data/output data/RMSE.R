###--------------------------------------###
###-------------美股RMSE計算-------------
###--------------------------------------###

### 一、RMSE、病例數函式整理，若RMSE計算中病例數採log需做smoothing
### 二、計算各月各類別RMSE
### 三、彙整成為RMSE表格(dfRMSE)、病例數表格(casesCol)
### 四、繪製為圖表(分為病例數有取log與原始版本)

library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(ggplot2)


df <- as_tibble(readr::read_csv("SP500_USnewcases.csv")) %>%
    select(-X1)

View(df)


## 各月各類別 RMSE 函式
rmse_OneMonth <- function(df, X, yearX, monthX, logCases){
    
    # 取出該月資料並標準化
    df_OneMonth <- df%>%
        filter(year(Date) == yearX & month(Date) == monthX) %>%
        mutate(NewCases_smoothing = NewCases + 1)
    # 標準化
    standarX <- scale(df_OneMonth[3:13])
    df_OneMonth <- select(df_OneMonth, NewCases, NewCases_smoothing)
    
    df_OneMonth <- cbind(df_OneMonth, standarX)
    cases_OneMonth <- sum(df_OneMonth[["NewCases"]])
    # View(df_OneMonth)
    
    # 計算 RMSE，X 帶入各類別
    if (logCases == T){
        fmla_All <- "log(NewCases_smoothing) ~ "
    }else{
        fmla_All <- "NewCases ~ "
    }
    fmla <- as.formula(paste0(fmla_All, X))
    lm_OneMonth <- lm(fmla, data = df_OneMonth)
    rmse <- sum((lm_OneMonth$residuals) ** 2) / length(lm_OneMonth$residuals)
    
    return(rmse)
}

## 各月病例數函式
cases_OneMonth <- function(df, yearX, monthX){
    
    # 取出該月資料
    df_OneMonth <- df%>%
        filter(year(Date) == yearX & month(Date) == monthX)
    
    cases <- sum(df_OneMonth[["NewCases"]])
    
    return(cases)
}

## 計算各月各類別 RMSE

xAll <- c("Energy", "Materials", "Industrials", "Consumer.Discretionary",
          "Consumer.Staples", "Health.Care", "Financials",
          "Information.Technology", "Communication.Services", "Utilities", "Real.Estate")

dates <- c("2020/01", "2020/02", "2020/03", "2020/04", "2020/05", "2020/06",
           "2020/07", "2020/08", "2020/09", "2020/10", "2020/11", "2020/12",
           "2021/01", "2021/02", "2021/03", "2021/04")

# 表格第一欄是日期(月份)
dfRMSE <- data.frame(Date = dates)


for (k in xAll){
    
    # 紀錄同一類別的各月 RMSE
    rmseAll <- vector("numeric", length = 16)
    casesAll <- vector("numeric", length = 16)
    
    # 算 2020 年
    for (i in 1:12){
        rmse <- rmse_OneMonth(df, k, 2020, i, logCases = F)
        rmseAll[i] <- rmse
        if (k == "Real.Estate"){
            cases <- cases_OneMonth(df, 2020, i)
            casesAll[i] <- cases}}
    
    # 算 2021 年
    for (i in 1:4){
        rmse <- rmse_OneMonth(df, k, 2021, i, logCases = F)
        rmseAll[12 + i] <- rmse
        if (k == "Real.Estate"){
            cases <- cases_OneMonth(df, 2021, i)
            casesAll[12 + i] <- cases}}
    
    # 紀錄單月病例數
    if (k == "Real.Estate"){
            casesCol <- data.frame(Date = dates, Cases = casesAll)            }
            
    # 單一類別 RMSE 紀錄至 dfRMSE
    oneCol <- data.frame(rmseAll)
    dfRMSE <- cbind(dfRMSE, oneCol)
}

# 表格(dfRMSE)整理表頭名稱，轉成長表格
rowNames_dfRMSE <- c("Date", xAll)
colnames(dfRMSE) <- rowNames_dfRMSE
dfRMSE <- as_tibble(dfRMSE) %>%
    gather(key = Category, value = RMSE, 2:12)


## 圖表呈現
ggplot() + 
    geom_bar(data = casesCol, aes(x = Date, y = Cases * 100/0.3), stat = "identity") + 
    geom_point(data = dfRMSE, aes(x = Date, y = RMSE, group = Category, color = Category)) + 
    geom_line(data = dfRMSE, aes(x = Date, y = RMSE, group = Category, color = Category)) +
    # 副座標軸：Cases (調整刻度)
    scale_y_continuous(name = expression("RMSE"), sec.axis = sec_axis(~. *0.3/100, name = "Cases")) +
    labs(title = "11 Categories' RMSE in S&P500") + 
    theme_bw()


## 圖表呈現 (計算RMSE時Cases取log)
ggplot() + 
    geom_bar(data = casesCol, aes(x = Date, y = Cases * 3/10000000), stat = "identity") + 
    geom_point(data = dfRMSE, aes(x = Date, y = RMSE, group = Category, color = Category)) + 
    geom_line(data = dfRMSE, aes(x = Date, y = RMSE, group = Category, color = Category)) +
    # 副座標軸：Cases
    scale_y_continuous(name = expression("RMSE"), sec.axis = sec_axis(~. *10000000/3, name = "Cases")) +
    labs(title = "11 Categories' RMSE in S&P500 (log)") + 
    theme_bw()


##-----------------------------------##
##----------tseting testing----------##
##-----------------------------------##

## 散布圖 + lm線

ggplot(data = dfJan2021, aes(x = Energy, y = log(NewCases)))+
    geom_point() + 
    geom_smooth(method = lm)


## 找出 2021 1月的模型

dfJan2021 <- df%>%
    filter(year(Date) == 2021 & month(Date) == 1)

standarX <- scale(dfJan2021[3:13])

dfJan2021 <- select(dfJan2021, NewCases)

dfJan2021 <- cbind(dfJan2021, standarX)
    
View(dfJan2021)    

## 計算 RMSE
lmJan2021 <- lm(log(NewCases) ~ Energy, data = dfJan2021)
summary(lmJan2021)
sum((lmJan2021$residuals) ** 2) / length(lmJan2021$residuals)

lmJan2020 <- lm(log(NewCases) ~ Energy, data = dfJan2020)
summary(lmJan2020)
sum((lmJan2020$residuals) ** 2) / length(lmJan2020$residuals)
