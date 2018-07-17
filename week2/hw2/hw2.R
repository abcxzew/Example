library(xml2)
library(tmcn)
library(rvest)
pttTestFunction <- function(URL, filename)
{
  #URL   = "https://www.ptt.cc/bbs/TWICE/index.html"
  html  = read_html(URL)
  title = html_nodes(html, "a")
  href  = html_attr(title, "href")
  data = data.frame(title = toUTF8(html_text(title)),
                    href = href)
  data = data[-c(298:303),]
  getContent <- function(x) {
    url  = paste0("https://www.ptt.cc", x)
    tag  = html_node(read_html(url), 'div#main-content.bbs-screen.bbs-content')
    text = toUTF8(html_text(tag))
  }
  #getContent(data$href[6900])
  allText = sapply(data$href, getContent)
  allText
  #out <- file(filename, "w", encoding="BIG-5") 
  write.table(allText, filename) 
  #close(out) 
}

id = c(298:303)
URL = paste0("https://www.ptt.cc/bbs/TWICE/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)

rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "※")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "看板")
docs <- tm_map(docs, toSpace, "作者")
docs <- tm_map(docs, toSpace, "發信站")
docs <- tm_map(docs, toSpace, "批踢踢實業坊")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "推")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "看")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "好")
docs <- tm_map(docs, toSpace, "啊")
docs <- tm_map(docs, toSpace, "阿")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "們")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "這次")
docs <- tm_map(docs, toSpace, "喔")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "想說")
docs <- tm_map(docs, toSpace, "要")
docs <- tm_map(docs, toSpace, "這")
docs <- tm_map(docs, toSpace, "沒")
docs <- tm_map(docs, toSpace, "什麼")
docs <- tm_map(docs, toSpace, "人")
docs <- tm_map(docs, toSpace, "由")
docs <- tm_map(docs, toSpace, "來")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "像")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "真")
docs <- tm_map(docs, toSpace, "嗎")
docs <- tm_map(docs, toSpace, "啦")
docs <- tm_map(docs, toSpace, "還")
docs <- tm_map(docs, toSpace, "已經")
docs <- tm_map(docs, toSpace, "如果")
docs <- tm_map(docs, toSpace, "吧")
docs <- tm_map(docs, toSpace, "張")
docs <- tm_map(docs, toSpace, "跟")
docs <- tm_map(docs, toSpace, "只")
docs <- tm_map(docs, toSpace, "個")
docs <- tm_map(docs, toSpace, "去")
docs <- tm_map(docs, toSpace, "到")
docs <- tm_map(docs, toSpace, "但")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
library(knitr)
kable(head(freqFrame), format = "markdown")

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.4),min.freq=0,max.words=110,
          random.order=FALSE, random.color=FALSE, 
          rot.per=.05, colors=brewer.pal(4, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
