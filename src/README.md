# 原始碼說明文件
- `data/`:有cvaw, stock, output data三個子資料夾

  - `cvaw/`: 情緒分析原始資料
    + `preprocess+cvaw.R`：處理文本情緒詞之程式碼
    + `ptt_covid19.csv`：covid-19板原始資料
    + `ptt_stock.csv`：stock板原始資料
    + `cvaw_covid.csv`：covid-19板情緒詞分數
    + `cvaw_stock.csv`：stock板情緒詞分數
  - `stock/`:股市與疫情分析相關原始資料
    + `BTC-Oil-Gold-COVID.csv`：比特幣、原油、黃金原始價格資料
    + `SP500_USnewcases.csv`：美國標普五百指數歷史資料
    + `SP500.csv`：美國標普五百指數十一大類股歷史資料
    + `TAIEX_7MA.csv`：台股各類股七日簡單均線原始數值
    + `TAIEX_ROC.csv`：台股各類股變動率原始數值
    + `TAIEX-COVID.csv`：台灣大盤指數對疫情歷史資料
    + `TAIEX.csv`：台股各類股指數原始數值
  - `output data/`:程式碼產出資料
    + `RMSE.R`：產出RMSE.csv之程式碼
    + `RMSE.csv`：S&P500類股之RMSE計算

- `Slides/`:產出G04_slides.pdf的 .Rmd及 .html檔案，及其 .css檔
- `Report/`:產出G04_report.pdf的程式碼.Rmd及 .html檔案，及其 .css檔
