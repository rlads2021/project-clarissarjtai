###---------------------------------------###
###---------ptt斷詞與情緒分數計算---------
###---------------------------------------###

### 一、爬蟲：python requests 抓取 ptt 資料
### 二、資料清理：使用疫情相關詞彙篩選文本，並僅保留內文與時間，去除推文與固定格式之資料清理(聲明、格式要求)
### 三、斷詞：jiebaR, CKIP(python)
### 四、情緒分數：使用cvaw中文維度型情感詞典，計算每篇文章的情緒總分、平均情緒分數

library(tibble)
library(dplyr)
library(stringr)
library(jiebaR)
library(tidytext)
library(quanteda)


###--------------------------------------###
###------------COVID-19版處理------------
###--------------------------------------###

###只取新聞與疫苗文章，不使用公告、整理、LIVE、摘要

## 篩選新聞類別
df_covid_news <- as_tibble(readr::read_csv("ptt_covid19.csv")) %>%
    filter(str_detect(title, pattern = "新聞")) %>%
    select(content, date) %>%
    filter(!is.na(content)) %>%
    filter(!is.na(date))

View(df_covid_news)


## 清理新聞內文

# k = 內文篇數
for (k in 1:length(df_covid_news[[1]])){
    
    # 取第 k 篇第一欄(content)以換行段成 vector，同時建立存放清理後資料的 cleanVec
    strList <- unlist(strsplit(df_covid_news[[k,1]], "\n"))
    cleanVec = vector("character", length = length(strList))
    
    
    for (i in seq_along(strList)){
        
        # 檢查留言是否被刪除
        # if (str_detect(strList[[i]], pattern = "^(推|噓|→)") == T){
        #     print(k)
        #     }
        
        # 刪除一些不必要的內文，格式：以 XXX 開頭者存入 NA
        if (str_detect(strList[[i]], pattern = "^(完整標題|發稿單位|發稿時間|撰 稿 者|原文連結|推|噓|→|※|https|最後更新|最新更新|作者|標題|時間)") == F){
            cleanVec[i] <- strList[[i]]
        }else{
            cleanVec[i] <- NA
        }
    }
    
    # 刪掉空值(不必要內文)
    cleanVec <- na.omit(cleanVec)
    #合併內文，改寫 dataframe，去除換行字元(後續要斷詞)
    df_covid_news[[k,1]] <- paste(cleanVec, collapse = "")    
    
}

## 篩選疫苗類別
df_covid_vaccine <- as_tibble(readr::read_csv("ptt_covid19.csv")) %>%
    filter(str_detect(title, pattern = "疫苗")) %>%
    select(content, date) %>%
    filter(!is.na(content)) %>%
    filter(!is.na(date))

View(df_covid_vaccine)


## 清理疫苗內文

for (k in 1:length(df_covid_vaccine[[1]])){
    
    strList <- unlist(strsplit(df_covid_vaccine[[k,1]], "\n"))
    cleanVec = vector("character", length = length(strList))
    
    for (i in seq_along(strList)){
        
        if (str_detect(strList[[i]], pattern = "^(完整標題|發稿單位|發稿時間|撰 稿 者|原文連結|推|噓|→|※|https|最後更新|最新更新|施打時間|施打地點|施打廠商|施打心得|作者|標題|時間)") == F){
            cleanVec[i] <- strList[[i]]
        }else{
            cleanVec[i] <- NA
        }
    }
    
    cleanVec <- na.omit(cleanVec)
    
    df_covid_vaccine[[k,1]] <- paste(cleanVec, collapse = "\n")
    
}

## 刪除真實聲明(在內文最末)
for (k in 1:length(df_covid_vaccine[[1]])){
    
    strList <- unlist(strsplit(df_covid_vaccine[[k,1]], "\n"))
    cleanVec = vector("character", length = length(strList))
    declar <- F
    
    for (i in seq_along(strList)){
        
        # 開頭非真實聲明時
        if (str_detect(strList[[i]], pattern = "^我聲明上述資料均為真實") == F){
            # 檢查前面是否已出現過
            if (declar == F){
                cleanVec[i] <- strList[[i]]     # 未出現表示尚未到最後
            }else{
                cleanVec[i] <- NA     # 已出現表示後續接是真實聲明
            }
        
        # 出現真實聲明時    
        }else{
            declar <- T
            cleanVec[i] <- NA
        }
    }
    
    cleanVec <- na.omit(cleanVec)
    df_covid_vaccine[[k,1]] <- paste(cleanVec, collapse = "")
    
}

## 合併新聞與疫苗類別
df_covid_clean <- rbind(df_covid_news, df_covid_vaccine) %>%
    filter(!is.na(content)) %>%
    filter(!is.na(date)) %>%
    arrange(date)

df_covid_clean <- df_covid_clean[!duplicated(df_covid_clean$date), ]  

## 去除數字中的英文逗點，python在切割csv時才不會出錯
for(i in seq(length(df_covid_clean$content))){
    df_covid_clean$content[i] <- gsub(',', '', df_covid_clean$content[i])
}

View(df_covid_clean)    

## 清理過 covid-19 版存檔
write.csv(df_covid_clean, file = "covid_clean.csv", fileEncoding = "UTF-8")




###--------------------------------------###
###-------------斷詞 (jieba)-------------
###--------------------------------------###


df_covid_clean <- as_tibble(readr::read_csv("covid_clean.csv")) %>%
    rename(id = X1)

## 斷詞 covid_token_df
# 設定自定義字典
seg <- worker(user = "user_dict.txt")

# 建立空vector以儲存資料輸入dataframe
content_tokens_all <- vector("character", length = length(df_covid_clean[[1]]))

for (i in 1:length(df_covid_clean[[1]])){
    
    segged <- segment(df_covid_clean[[i,2]], seg)
    # 只取中文字
    segged <- segged[str_detect(segged, pattern = "[\u4e00-\u9FFF]")]
    content_tokens_all[i] <- paste0(segged, collapse = " ") #用空白格開斷詞
}

# 斷詞建立 df
covid_token_df <- data.frame(id = df_covid_clean[["id"]],
                      content = content_tokens_all, 
                      date = df_covid_clean[["date"]]) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))


## covid-19 斷詞存檔
write.csv(covid_token_df, file = "covid_token.csv", fileEncoding = "UTF-8")



###---------------------------------------###
###--------------斷詞 (CKIP)--------------
###---------------------------------------###

###--------------CKIP 完整版--------------

## 已在 python 使用 CKIP 斷詞完
# 只取 5/31 之前的日期
covid_CKIPtoken_df <- as_tibble(readr::read_csv("covid_CKIPtoken.csv")) %>%
    rename(id = X1) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))
    
View(covid_CKIPtoken_df)
# 重新存檔(使格式與 covid_token.csv 相同)
write.csv(covid_CKIPtoken_df, file = "covid_CKIPtoken.csv", fileEncoding = "UTF-8")


###--------------CKIP 取中文--------------
## 只取中文版本
content_tokens_all <- vector("character", length = length(covid_CKIPtoken_df[[1]]))

for (i in 1:length(covid_CKIPtoken_df[[1]])){
    
    segged <- unlist(strsplit(covid_CKIPtoken_df[[i,2]], split = " "))
    # 只取中文字
    segged <- segged[str_detect(segged, pattern = "[\u4e00-\u9FFF]")]
    content_tokens_all[i] <- paste0(segged, collapse = " ") #用空白格開斷詞
}

# 斷詞建立 df
covid_CKIP_nonEng_df <- data.frame(id = covid_CKIPtoken_df[["id"]],
                      content = content_tokens_all, 
                      date = covid_CKIPtoken_df[["date"]]) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))

View(covid_CKIP_nonEng_df)

## covid-19 CKIP 斷詞存檔(只取中文)
write.csv(covid_CKIP_nonEng_df, file = "covid_CKIP_nonEng.csv", fileEncoding = "UTF-8")



###-------------------------------------###
###------------- T F-I D F -------------
###-------------------------------------###


## tf-idf (記憶體不夠)

covid_token_df <- as_tibble(readr::read_csv("covid_token.csv"))

###--------------------------------------###
###---------------無法執行---------------###
covid_tidy_format <- covid_token_df %>%
    unnest_tokens(output = "word", input = "content", 
                token = "regex", pattern = " ") %>%
    count(id, word) %>% 
    cast_dfm(document = id,term = word, value = n) %>% 
    dfm_tfidf() %>% 
    convert(to = "data.frame") %>%
    write.csv(file = "covid_tfidf.csv", fileEncoding = "UTF-8")
###---------------無法執行---------------###
###--------------------------------------###

## 計算 idf
covid_doc_num <- length(df_covid_clean[[1]])

covid_tidy_format <- covid_token_df %>%
    unnest_tokens(output = "word", input = "content",
                token = "regex", pattern = " ") %>%
    group_by(id, word) %>%
    summarise(n = n()) %>%
    group_by(word) %>%
    summarise(word_in_doc = n()) %>%
    filter(word_in_doc > 2) %>%
    mutate(idf = log(covid_doc_num/word_in_doc))

wordAll <- covid_tidy_format[["word"]]
columnName <- c("id", wordAll)



###--------------------------------------###
###-------------STOCK 版處理-------------
###--------------------------------------###


## 建立疫情關鍵字 vector
covid_keywords <- read.table("covid_keywords.txt", encoding = "UTF-8") %>%
    unlist(use.names = F) %>%
    paste(collapse = "|")
covid_keywords <- paste0("(", covid_keywords, ")")

## 文本範圍選取
df_stock <- as_tibble(readr::read_csv("ptt_stock.csv")) %>%
    # 去除與當日股票無關的文
    filter(!str_detect(title, pattern = "(公告|問卷|情報|板務|置底|
                       盤後閒聊|盤中閒聊|加密貨幣|幣圈|比特幣|
                       Bitcoi|bitcoin|馬斯克|Live)")) %>%
    # 選擇與疫情相關之內文
    filter(str_detect(content, pattern = covid_keywords)) %>%
    select(content, date) %>%
    filter(!is.na(date))

   
## 清理股票版內文

for (k in seq(length(df_stock$content))){

    strVec <- unlist(strsplit(df_stock$content[k], "\n"))
    cleanVec = vector("character", length = length(strList))

    for (i in seq_along(strVec)){

        if (str_detect(strVec[i], pattern = "^(1.|2.|3.|原文標題|原文連結|發布時間|原文內容|推|噓|→|:|※|https|作者|標題|時間|【)") == F){
            
            if (str_detect(strVec[i], pattern = "(請勿刪減原文標題|請善用縮網址工具|請以原文網頁/報紙之發布)") == F){
                cleanVec[i] <- strVec[i]
            }else{
                cleanVec[i] <- NA
            }
        }else{
            cleanVec[i] <- NA
        }
    }
    cleanVec <- na.omit(cleanVec)

    df_stock$content[k] <- paste(cleanVec, collapse = "")
}

View(df_stock)


df_stock_clean <- df_stock %>%
    filter(!content == "") %>%
    arrange(date)
## 去除重複
df_stock_clean <- df_stock_clean[!duplicated(df_stock_clean$date), ]

## 去除數字中的英文逗點，python在切割csv時才不會出錯
for(i in seq(length(df_stock_clean$content))){
    df_stock_clean$content[i] <- gsub(',', '', df_stock_clean$content[i])
}

View(df_stock_clean)


write.csv(df_stock_clean, file = "stock_clean.csv", fileEncoding = "UTF-8")



###--------------------------------------###
###-------------斷詞 (jieba)-------------
###--------------------------------------###


df_stock_clean <- as_tibble(readr::read_csv("stock_clean.csv")) %>%
    rename(id = X1)

## 斷詞 covid_token_df
# 設定自定義字典
seg <- worker(user = "user_dict.txt")

# 建立空vector以儲存資料輸入dataframe
content_tokens_all <- vector("character", length = length(df_stock_clean$content))

for (i in 1:length(df_stock_clean[[1]])){
    
    segged <- segment(df_stock_clean[[i,2]], seg)
    # 只取中文字
    segged <- segged[str_detect(segged, pattern = "[\u4e00-\u9FFF]")]
    content_tokens_all[i] <- paste0(segged, collapse = " ") #用空白格開斷詞
}

# 斷詞建立 df
stock_token_df <- data.frame(id = df_stock_clean[["id"]],
                      content = content_tokens_all, 
                      date = df_stock_clean[["date"]]) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))


## stock 斷詞存檔
write.csv(stock_token_df, file = "stock_token.csv", fileEncoding = "UTF-8")


###---------------------------------------###
###--------------斷詞 (CKIP)--------------
###---------------------------------------###

###--------------CKIP 完整版--------------

## 已在 python 使用 CKIP 斷詞完
# 只取 5/31 之前的日期
stock_CKIPtoken_df <- as_tibble(readr::read_csv("stock_CKIPtoken.csv")) %>%
    rename(id = X1) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))
    
View(stock_CKIPtoken_df)
# 重新存檔(使格式與 stock_token.csv 相同)
write.csv(stock_CKIPtoken_df, file = "stock_CKIPtoken.csv", fileEncoding = "UTF-8")


###--------------CKIP 取中文--------------
## 只取中文版本
content_tokens_all <- vector("character", length = length(stock_CKIPtoken_df[[1]]))

for (i in 1:length(stock_CKIPtoken_df[[1]])){
    
    segged <- unlist(strsplit(stock_CKIPtoken_df[[i,2]], split = " "))
    # 只取中文字
    segged <- segged[str_detect(segged, pattern = "[\u4e00-\u9FFF]")]
    content_tokens_all[i] <- paste0(segged, collapse = " ") #用空白格開斷詞
}

# 斷詞建立 df
stock_CKIP_nonEng_df <- data.frame(id = stock_CKIPtoken_df[["id"]],
                      content = content_tokens_all, 
                      date = stock_CKIPtoken_df[["date"]]) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(between(date, as.Date("2020-01-01"), as.Date("2021-05-31")))

View(stock_CKIP_nonEng_df)

## stock CKIP 斷詞存檔(只取中文)
write.csv(stock_CKIP_nonEng_df, file = "stock_CKIP_nonEng.csv", fileEncoding = "UTF-8")



###---------------------------------------###
###----------------C V A W----------------
###---------------------------------------###

###------------CVAW COVID-19版------------

## 使用 CKIP tokens
cvaw <- as_tibble(readr::read_csv("cvaw.csv"))
View(cvaw)


## 切開成 token(每個token 一列)
covid_tidy_format <- covid_CKIP_nonEng_df %>%
    unnest_tokens(output = "word", input = "content",
                token = "regex", pattern = " ")

## 建立 vector 之後整理為 dataframe
valenceTotal_all <- vector("double", length = length(covid_CKIP_nonEng_df$content))
arousalTotal_all <- vector("double", length = length(covid_CKIP_nonEng_df$content))
docCVAW_Words_all <- vector("integer", length = length(covid_CKIP_nonEng_df$content))
# seq(length(covid_CKIP_nonEng_df$content))

# 在一篇文章中
for (k in seq(length(covid_CKIP_nonEng_df$content))){
    
    valenceTotal = 0
    arousalTotal = 0
    docCVAW_Words = 0
    
    # 只取該篇的 tokens
    doc_tokens <- covid_tidy_format %>%
        filter(id == k) %>%
        group_by(word) %>%
        summarise(n = n())
    
    # 每個CVAW情緒詞取出做計算
    for (i in seq(length(cvaw$Word))){
        
        # 情緒詞數量計算歸零
        cvawWord <- cvaw$Word[i]
        cvawValence <- cvaw$Valence_Mean[i] - 5
        cvawArousal <- cvaw$Arousal_Mean[i] - 5

        # 若此情緒詞在此文章中
        if (cvawWord %in% doc_tokens$word){
            
            doc_cvaw <- doc_tokens %>%
                filter(word == cvawWord)
            
            valenceTotal = valenceTotal + cvawValence * doc_cvaw$n
            arousalTotal = arousalTotal + cvawArousal * doc_cvaw$n
            docCVAW_Words = docCVAW_Words + doc_cvaw$n
            
            # print(c(cvawWord, cvawValence,cvawArousal))
            # print(valenceTotal)
            # print(arousalTotal)
            # print(docCVAW_Words)
        }
        
        valenceTotal_all[k] <- valenceTotal
        arousalTotal_all[k] <- arousalTotal
        docCVAW_Words_all[k] <- docCVAW_Words
    }
}

## 合併成 dataframe
cvaw_covid <- data.frame(id = covid_CKIP_nonEng_df$id,
                         Date = covid_CKIP_nonEng_df$date, 
                         Valence_Sum = valenceTotal_all, 
                         Arousal_Sum = arousalTotal_all, 
                         words_Num = docCVAW_Words_all) %>%
    # sum 取到小數第三位，avg 取到小數第六位
    mutate(Valence_Sum = round(Valence_Sum, 3)) %>%
    mutate(Arousal_Sum = round(Arousal_Sum, 3)) %>%
    # 若此篇文章無情緒詞，則 Valence_Avg, Arousal_Avg 為 0
    mutate(Valence_Avg = if_else(Valence_Sum == 0, 0, round(Valence_Sum / words_Num, 6))) %>%
    mutate(Arousal_Avg = if_else(Valence_Sum == 0, 0, round(Arousal_Sum / words_Num, 6)))
    

## cvaw_covid 存檔
write.csv(cvaw_covid, file = "cvaw_covid.csv", fileEncoding = "UTF-8")

###------------CVAW stock版------------


## 切開成 token(每個token 一列)
stock_tidy_format <- stock_CKIP_nonEng_df %>%
    unnest_tokens(output = "word", input = "content",
                token = "regex", pattern = " ")

## 建立 vector 之後整理為 dataframe
valenceTotal_all <- vector("double", length = length(stock_CKIP_nonEng_df$content))
arousalTotal_all <- vector("double", length = length(stock_CKIP_nonEng_df$content))
docCVAW_Words_all <- vector("integer", length = length(stock_CKIP_nonEng_df$content))
# seq(length(stock_CKIP_nonEng_df$content))

# 在一篇文章中
for (k in seq(length(stock_CKIP_nonEng_df$content))){
    
    valenceTotal = 0
    arousalTotal = 0
    docCVAW_Words = 0
    
    # 只取該篇的 tokens
    doc_tokens <- stock_tidy_format %>%
        # stock 版的 id 從 2 開始
        filter(id == k+1) %>%
        group_by(word) %>%
        summarise(n = n())
    
    # View(doc_tokens)
    
    # 每個CVAW情緒詞取出做計算
    for (i in seq(length(cvaw$Word))){
        
        # 情緒詞數量計算歸零
        cvawWord <- cvaw$Word[i]
        cvawValence <- cvaw$Valence_Mean[i] - 5
        cvawArousal <- cvaw$Arousal_Mean[i] - 5

        # 若此情緒詞在此文章中
        if (cvawWord %in% doc_tokens$word){
            
            doc_cvaw <- doc_tokens %>%
                filter(word == cvawWord)
            
            valenceTotal = valenceTotal + cvawValence * doc_cvaw$n
            arousalTotal = arousalTotal + cvawArousal * doc_cvaw$n
            docCVAW_Words = docCVAW_Words + doc_cvaw$n
            
            # print(c(cvawWord, cvawValence,cvawArousal))
            # print(valenceTotal)
            # print(arousalTotal)
            # print(docCVAW_Words)
        }
        
        valenceTotal_all[k] <- valenceTotal
        arousalTotal_all[k] <- arousalTotal
        docCVAW_Words_all[k] <- docCVAW_Words
    }
}

## 合併成 dataframe
cvaw_stock <- data.frame(id = stock_CKIP_nonEng_df$id,
                         Date = stock_CKIP_nonEng_df$date, 
                         Valence_Sum = valenceTotal_all, 
                         Arousal_Sum = arousalTotal_all, 
                         words_Num = docCVAW_Words_all) %>%
    # sum 取到小數第三位，avg 取到小數第六位
    mutate(Valence_Sum = round(Valence_Sum, 3)) %>%
    mutate(Arousal_Sum = round(Arousal_Sum, 3)) %>%
    # 若此篇文章無情緒詞，則 Valence_Avg, Arousal_Avg 為 0
    mutate(Valence_Avg = if_else(Valence_Sum == 0, 0, round(Valence_Sum / words_Num, 6))) %>%
    mutate(Arousal_Avg = if_else(Valence_Sum == 0, 0, round(Arousal_Sum / words_Num, 6)))
    

## cvaw_covid 存檔
write.csv(cvaw_stock, file = "cvaw_stock.csv", fileEncoding = "UTF-8")

