# 原始碼說明文件
- `data/`:有cvaw, stock, output data三個子資料夾

  - `cvaw/`: 情緒分析原始資料
    + preprocess+cvaw.R：處理文本情緒詞之程式碼
    + ptt_covid19.csv：covid-19板原始資料
    + ptt_stock.csv：stock板原始資料
    + cvaw_covid.csv：covid-19板情緒詞分數
    + cvaw_stock.csv：stock板情緒詞分數
  - `stock/`:股市與疫情分析相關原始資料
  - `output data/`:程式碼產出資料
    + RMSE.R：產出RMSE.csv之程式碼
    + RMSE.csv：S&P500類股之RMSE計算

- `Slides/`:產出G04_slides.pdf的 .Rmd及 .html檔案，及其 .css檔
- `Report/`:產出G04_report.pdf的程式碼.Rmd及 .html檔案，及其 .css檔
