library(httr)
library(jsonlite)
library(anytime)
library(ggplot2)
library(rvest)
library(scales)
library(data.table)
library(stringr)
library(wordcloud)
Sys.setlocale("LC_TIME", "C")

url <- "https://steamdb.info/api/GetGraph/?type=concurrent_max&appid=730"
data <- fromJSON(url)

start_date <- anydate(data$data$start)
dates <- c(start_date)
for (i in 1:(length(data$data$values) - 1)) {
  dates <- c(dates, start_date + i*data$data$step/86400)
}

player_counts <- data.frame(Date = dates, Count = data$data$values, stringsAsFactors = F)

options(scipen = 999)
ggplot(player_counts, aes(x = Date, y = Count)) + 
          geom_line(color = "red") + geom_area(fill="blue")


zz = url("https://steamdb.info/api/GetPriceHistory/?appid=578080&cc=us")
zz = fromJSON(zz)

price_history <- as.data.frame(zz$data$final)
colnames(price_history) <- c("Date", "Price")
price_history$Date = price_history$Date / 1000
#price_history$Date <- lapply(price_history$Date / 1000, anytime)
for (i in length(price_history$Date)) {
  price_history$Date = anytime(price_history$Date)
}

diffed_date <- price_history$Date - 1
price_history <- rbind(price_history, data.frame(Date = diffed_date, Price = shift(price_history$Price)))


ggplot(price_history, aes(x = price_history$Date, y = price_history$Price)) + 
  geom_point(color="red", size = 1) + geom_line() +
  labs(x = "Date", y = "Price", title = "Price Change History")


z <- read_html("https://steamdb.info/app/730/info/") %>%
  html_nodes(xpath = "//td[text()='store_tags']/following-sibling::td[1]/ul/*") %>%
  html_text() %>%
  str_remove_all(pattern = "\\d+:") %>%
  trimws()

layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Tags", col = "Blue")
wordcloud(words = z, freq = c(length(z):1), 
          scale=c(2,.5), random.order = F, main="Title")

countries = c("U.S. Dollar", "British Pound", "Euro", "Russian Ruble","Brazilian Real", "Japanese Yen"
              ,"Indonesian Rupiah","Malaysian Ringgit","Philippine Peso", "Singapore Dollar","Thai Baht", 
              "Vietnamese Dong","South Korean Won","Turkish Lira", "Ukrainian Hryvnia","Mexican Peso",
              "Canadian Dollar","New Zealand Dollar","Norwegian Krone","Polish Zloty","Swiss Franc",
              "Chinese Yuan Renminbi","Indian Rupee","Chilean Peso", "Peruvian Nuevo Sol",
              "Colombian Peso","South African Rand","Hong Kong Dollar","Taiwan Dollar","Saudi Riyal",
              "U.A.E. Dirham","Argentine Peso","Israeli New Shekel", "Kazakhstani Tenge", "Kuwaiti Dinar",
              "Qatari Riyal", "Costa Rican Colon", "Uruguayan Peso", "CIS - U.S. Dollar", "South Asia - U.S. Dollar")
currency_short = c("us","uk","eu","ru","br","jp","id","my","ph","sg","th","vn","kr","tr","ua","mx","ca","nz","no","pl","ch","cn","in"
                   ,"cl","pe","co","za","hk","tw","sa","ae","ar","il","kz","kw","qa","cr","uy","az","pk")

logo_small = read_html("https://steamdb.info/app/730/info/") %>%
  html_node(xpath = "//td[text()='logo_small']/following-sibling::td[1]/a") %>%
  html_attr(name = "href")


