library(dplyr)
library(tidyr)
library(reshape2)
library(readr)
library(png)

https://api.projectoxford.ai/emotion/v1.0

4ef1081e34874d358fd97dcbac907180

loadNamespace("httr")

apiUrl <- "https://api.projectoxford.ai/emotion/v1.0"
mybody <- upload_file(file.choose())
faceEMO <- httr::POST(
  url = apiUrl,
  httr::content_type('application/octet-stream'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = "4ef1081e34874d358fd97dcbac907180")),
  body = mybody,
  encode = 'json'
)
operationLocation <- httr::headers(faceEMO)[["operation-location"]]
while(TRUE){
  ret <- httr::GET(operationLocation,
                   httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
  con <- httr::content(ret)
  if(is.null(con$status)){
    warning("Connection Error, retry after 1 minute")
    Sys.sleep(60)
  } else if (con$status == "Running" | con$status == "Uploading"){
    cat(paste0("status ", con$status, "\n"))
    cat(paste0("progress ", con$progress, "\n"))
    Sys.sleep(60)
  } else {
    cat(paste0("status ", con$status, "\n"))
    break()
  }
}
df <- (con$processingResult %>% jsonlite::fromJSON())$fragments
df <- df[df$events!="NULL",]
df$events <- purrr::map(df$events, function(events){
  events %>% purrr::map(function(event){
    jsonlite::flatten(event)
  }) %>% bind_rows()
})
df <- df %>% unnest(events) 
df



microsoft_emo <- function(urlVideo, key) {
  loadNamespace("httr")
  apiUrl <- "https://api.projectoxford.ai/emotion/v1.0/recognizeInVideo?outputStyle=perFrame"
  mybody <- list(url = urlVideo)
  faceEMO <- httr::POST(
    url = apiUrl,
    httr::content_type('application/json'),
    httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
    body = mybody,
    encode = 'json'
  )
  operationLocation <- httr::headers(faceEMO)[["operation-location"]]
  while(TRUE){
    ret <- httr::GET(operationLocation,
                     httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
    con <- httr::content(ret)
    if(is.null(con$status)){
      warning("Connection Error, retry after 1 minute")
      Sys.sleep(60)
    } else if (con$status == "Running" | con$status == "Uploading"){
      cat(paste0("status ", con$status, "\n"))
      cat(paste0("progress ", con$progress, "\n"))
      Sys.sleep(60)
    } else {
      cat(paste0("status ", con$status, "\n"))
      break()
    }
  }
  df <- (con$processingResult %>% jsonlite::fromJSON())$fragments
  df <- df[df$events!="NULL",]
  df$events <- purrr::map(df$events, function(events){
    events %>% purrr::map(function(event){
      jsonlite::flatten(event)
    }) %>% bind_rows()
  })
  df <- df %>% unnest(events) 
  df
}
savik <- microsoft_emo('http://redirector.googlevideo.com/videoplayback?sparams=dur%2Cgcr%2Cid%2Cinitcwndbps%2Cip%2Cipbits%2Citag%2Clmt%2Cmime%2Cmm%2Cmn%2Cms%2Cmv%2Cnh%2Cpl%2Cratebypass%2Csource%2Cupn%2Cexpire&itag=18&expire=1480550274&mime=video%2Fmp4&source=youtube&pl=24&signature=BEE3BA8CD243B3B6EDD0F62B7C7C9F0AC1403310.2504955D18FA8CD1596CEC305C4F7B8E18182D8E&initcwndbps=2223750&ratebypass=yes&nh=IgpwZjAxLmFtczE1Kg0xNDkuMTQuMTQyLjk3&upn=9UndFgMv9bo&id=o-AF-IpQNjcE2vGdDwP15oKT6l2g44gkm4qVB3X8joZCip&gcr=it&key=yt6&ipbits=0&mv=m&mt=1480528450&ms=au&lmt=1478404053200748&ip=149.13.117.213&mn=sn-5hnednlr&mm=31&dur=12375.585&title=%D0%A8%D1%83%D1%81%D1%82%D0%B5%D1%80+LIVE+04.11.2016','da1c4e918a3c41e6b96b95c67d47dea5')

emo_dynamic <- function(lia,time) {
  lia2 <- melt(lia[c(1,4,9:16)], id.vars = c("start","id"))
  lia3 <- lia2 %>% group_by(start,variable,id) %>% summarise(value = mean(value))
  
  lia3$time <- lia3$start/max(lia3$start)*time
  lia3$variable <- ifelse(lia3$variable=="scores.neutral","нейтральність",ifelse(lia3$variable=="scores.happiness","радість",ifelse(lia3$variable=="scores.surprise","здивування",
                                                                              ifelse(lia3$variable=="scores.sadness","сум",
                                                                                     ifelse(lia3$variable=="scores.anger","злість",
                                                                                            ifelse(lia3$variable=="scores.disgust","відраза",
                                                                                                   ifelse(lia3$variable=="scores.fear","страх",ifelse(lia3$variable=="scores.contempt","зневага",NA))))))))
  ag <- lia3 %>% group_by(variable) %>% summarise(med=median(value))
  lia3 <- inner_join(lia3,ag, by="variable")
  ggplot(lia3,aes(time,value))  + geom_line() + facet_wrap(~variable) + geom_hline(aes(yintercept=med,group=variable),colour="red") +
    theme_minimal() + 
    labs(title="Динаміка емоцій") +
    theme(axis.line = element_blank(),
           axis.title.x = element_blank(),
           axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_blank())
}

emo_average <- function(lia) {
  lia2 <- melt(lia[c(1,4,9:16)], id.vars = c("start","id"))
  lia3 <- lia2 %>% group_by(start,variable,id) %>% summarise(value = mean(value))
  lia3$variable <- ifelse(lia3$variable=="scores.neutral","нейтральність",ifelse(lia3$variable=="scores.happiness","радість",ifelse(lia3$variable=="scores.surprise","здивування",
                                                                                                                                    ifelse(lia3$variable=="scores.sadness","сум",
                                                                                                                                           ifelse(lia3$variable=="scores.anger","злість",
                                                                                                                                                  ifelse(lia3$variable=="scores.disgust","відраза",
                                                                                                                                                         ifelse(lia3$variable=="scores.fear","страх",ifelse(lia3$variable=="scores.contempt","зневага",NA))))))))
  lia4 <- lia3 %>% group_by(variable) %>% summarise(value=mean(value))
  ggplot(lia4, aes(x="", y=value,fill=variable))+
    geom_bar(width = 1, stat = "identity") + 
    coord_polar("y", start=0) +
    theme_minimal() + 
    labs(title="Сумарні показники емоцій") +
    scale_fill_discrete(name = "") +
    theme(axis.line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_blank())
}

emo_text <- function(lia) {
  lia2 <- melt(lia[c(1,4,9:16)], id.vars = c("start","id"))
  lia3 <- lia2 %>% group_by(start,variable,id) %>% summarise(value = mean(value))
  lia3$time <- lia3$start/max(lia3$start)*time
  lia3$variable <- ifelse(lia3$variable=="scores.neutral","нейтральність",ifelse(lia3$variable=="scores.happiness","радість",ifelse(lia3$variable=="scores.surprise","здивування",
                                                                                                                                    ifelse(lia3$variable=="scores.sadness","сум",
                                                                                                                                           ifelse(lia3$variable=="scores.anger","злість",
                                                                                                                                                  ifelse(lia3$variable=="scores.disgust","відраза",
                                                                                                                                                         ifelse(lia3$variable=="scores.fear","страх",ifelse(lia3$variable=="scores.contempt","зневага",NA))))))))
  lia4 <- lia3 %>% group_by(variable) %>% summarise(value=mean(value))
  ag <- lia3 %>% group_by(variable) %>% summarise(med=median(value))
  lia3 %>% group_by(time) %>% summarise()
  
  
  paste0("У проаналізованому відео домінує емоція ",lia4$variable[lia4$value==max(lia4$value)],
         ". Її загальна частка становить ",round(max(lia4$value)*100,2),
         "%. У певні моменти часу домінували інші емоції. Наприклад,")
}

apiUrlVideo <- "https://api.projectoxford.ai/video/v1.0/trackface"
keyVideo <- '2c875d1a0d284da3b163b03956bf4502'

# Load httr package
loadNamespace("httr")
# Set an endpoint for Emotion in Video API with 'perFrame' output
apiUrl <- "https://api.projectoxford.ai/emotion/v1.0/recognizeInVideo?outputStyle=perFrame"
urlVideo <- 'http://r2---sn-uvujvg-qh3e.googlevideo.com/videoplayback?initcwndbps=1567500&mime=video%2Fmp4&id=o-ABpskzgkbnUsyKCel2BGY0oh0hcgGb3eaieF0kDKgxG9&mm=31&signature=5D60E7977016703A4CA6CB920DF6994287103D8C.8F822E3C2685B1E438BC1E8D85921A833E09EC5C&upn=f0lQd1RhzWA&mn=sn-uvujvg-qh3e&ms=au&source=youtube&mv=m&pcm2cms=yes&key=yt6&ip=1.179.194.41&lmt=1401835186773574&ipbits=0&itag=18&sparams=dur%2Cid%2Cinitcwndbps%2Cip%2Cipbits%2Citag%2Clmt%2Cmime%2Cmm%2Cmn%2Cms%2Cmv%2Cpcm2cms%2Cpl%2Cratebypass%2Csource%2Cupn%2Cexpire&ratebypass=yes&pl=26&dur=1047.382&pfsc=ltr&expire=1480560306&mt=1480538560&title=%D0%9E%D0%BB%D0%B5%D0%B3+%D0%9B%D1%8F%D1%88%D0%BA%D0%BE+%D0%B2+%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D1%96+%27%D0%A7%D0%BE%D1%80%D0%BD%D0%B5+%D0%B4%D0%B7%D0%B5%D1%80%D0%BA%D0%B0%D0%BB%D0%BE%27+%D0%BD%D0%B0+%D0%86%D0%BD%D1%82%D0%B5%D1%80%D1%96'

urlVideo <- 'https://www.dropbox.com/s/x6l3hmztkp7yy86/%D0%A8%D1%83%D1%81%D1%82%D0%B5%D1%80%20LIVE%2004.11.2016.mp4?dl=1'
mybody <- list(url = urlVideo)
# Request data from Microsoft AI
faceVideo <- httr::POST(
  url = apiUrlVideo,
  httr::content_type('application/json'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = keyVideo)),
  body = mybody,
  encode = 'json'
)
operationLocation <- httr::headers(faceVideo)[["operation-location"]]
while(TRUE){
  ret <- httr::GET(operationLocation,
                   httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = keyVideo)))
  con <- httr::content(ret)
  if(is.null(con$status)){
    warning("Connection Error, retry after 1 minute")
    Sys.sleep(60)
  } else if (con$status == "Running" | con$status == "Uploading"){
    cat(paste0("status ", con$status, "\n"))
    cat(paste0("progress ", con$progress, "\n"))
    Sys.sleep(60)
  } else {
    cat(paste0("status ", con$status, "\n"))
    break()
  }
}
video <- (con$processingResult %>% jsonlite::fromJSON())$fragments
# data$events is list of events that has data.frame column,
# so it has to be flatten in this loop
video <- video[video$events!="NULL",]
video$events <- purrr::map(video$events, function(events){
  events %>% purrr::map(function(event){
    jsonlite::flatten(event)
  }) %>% bind_rows()
})

video <- video %>% unnest(events) 



# Set your API key for Emotion API
key <- 'da1c4e918a3c41e6b96b95c67d47dea5'
  # Set URL for accessing to the video.
urlVideo <- 'https://www.dropbox.com/s/pl22mngnimrkbsb/%D0%9B%D1%8F%D1%88%D0%BA%D0%BE%20%D0%B8%20%20%D0%91%D0%BE%D0%B9%D0%BA%D0%BE%20%D0%BF%D0%BE%D0%B4%D1%80%D0%B0%D0%BB%D0%B8%D1%81%D1%8C%20%D0%B2%20%D0%A0%D0%B0%D0%B4%D0%B5.mp4?dl=1'

urlVideo <-'http://r6---sn-ipoxu-u2xz.googlevideo.com/videoplayback?initcwndbps=3297500&ipbits=0&pl=26&itag=18&ms=au&mt=1480319324&upn=E9zoeTEVu40&source=youtube&key=yt6&ratebypass=yes&lmt=1479706051658317&mv=m&sparams=dur%2Cid%2Cinitcwndbps%2Cip%2Cipbits%2Citag%2Clmt%2Cmime%2Cmm%2Cmn%2Cms%2Cmv%2Cpl%2Cratebypass%2Csource%2Cupn%2Cexpire&ip=118.161.72.105&mm=31&signature=2318932EBED988C8D0B119DC6A0208C7B4C72846.A16EA20C525577B94556A2E4047C1BEEBAC6E43F&expire=1480341180&mn=sn-ipoxu-u2xz&mime=video%2Fmp4&id=o-AN1BDleDpbgtwrThBgfXCfauV1YCVs4jDCnX_VNixRlB&dur=189.498&title=%D0%9B%D1%8F%D1%88%D0%BA%D0%BE-+%D0%9B%D0%B5%D1%89%D0%B5%D0%BD%D0%BA%D1%83+%D0%BD%D0%B0+%D1%80%D0%B5%D0%BC%D0%BE%D0%BD%D1%82+%D0%BA%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D1%80%D0%B8+%D0%BD%D0%B5+%D0%B2%D0%B8%D1%81%D1%82%D0%B0%D1%87%D0%B0%D1%94%2C+%D1%82%D0%BE%D0%BC%D1%83+%D0%B2%D1%96%D0%BD+%D0%BA%D0%B0%D0%B7%D0%BA%D0%B8+%D0%BF%D1%80%D0%BE+%D0%BC%D0%B5%D0%BD%D0%B5+%D1%80%D0%BE%D0%B7%D0%BF%D0%BE%D0%B2%D1%96%D0%B4%D0%B0%D1%94'

https://www.youtube.com/watch?v=AupSFn9eh08

mybody <- list(url = urlVideo)
# Request data from Microsoft AI
faceEMO <- httr::POST(
  url = apiUrl,
  httr::content_type('application/json'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json'
)
operationLocation <- httr::headers(faceEMO)[["operation-location"]]
while(TRUE){
  ret <- httr::GET(operationLocation,
                   httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
  con <- httr::content(ret)
  if(is.null(con$status)){
    warning("Connection Error, retry after 1 minute")
    Sys.sleep(60)
  } else if (con$status == "Running" | con$status == "Uploading"){
    cat(paste0("status ", con$status, "\n"))
    cat(paste0("progress ", con$progress, "\n"))
    Sys.sleep(60)
  } else {
    cat(paste0("status ", con$status, "\n"))
    break()
  }
}
liaskov <- (con$processingResult %>% jsonlite::fromJSON())$fragments
# data$events is list of events that has data.frame column,
# so it has to be flatten in this loop
liaskov2 <- liaskov[liaskov$events!="NULL",]
liaskov2$events <- purrr::map(liaskov2$events, function(events){
  events %>% purrr::map(function(event){
    jsonlite::flatten(event)
  }) %>% bind_rows()
})

liaskov2 <-liaskov2 %>% unnest(events) 


boiko2 <-boiko2 %>% unnest(events) 

ggplot(boiko2,aes(start,scores.neutral)) + geom_line() + facet_wrap(~id) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") 

urlVideo <- 'https://www.dropbox.com/s/3y5x02rahgt4v78/%D0%AF%D0%BA%D1%89%D0%BE%20%D1%81%D0%BA%D0%B0%D1%81%D1%83%D1%8E%D1%82%D1%8C%20%D0%BC%D0%BE%D1%80%D0%B0%D1%82%D0%BE%D1%80%D1%96%D0%B9%2C%20%D1%82%D0%BE%20%D0%9A%D0%BE%D1%81%D1%8E%D0%BA%20%D1%96%20%D0%91%D0%B0%D1%85%D0%BC%D0%B0%D1%82%D1%8E%D0%BA%20%D1%81%D0%BA%D1%83%D0%BF%D0%BB%D1%8F%D1%82%D1%8C%20%D1%83%D1%81%D1%8E%20%D0%B7%D0%B5%D0%BC%D0%BB%D1%8E%2C%20-%20%D0%9B%D1%8F%D1%88%D0%BA%D0%BE.mp4?dl=1'

mybody <- list(url = urlVideo)
# Request data from Microsoft AI
faceEMO <- httr::POST(
  url = apiUrl,
  httr::content_type('application/json'),
  httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json'
)
operationLocation <- httr::headers(faceEMO)[["operation-location"]]
while(TRUE){
  ret <- httr::GET(operationLocation,
                   httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)))
  con <- httr::content(ret)
  if(is.null(con$status)){
    warning("Connection Error, retry after 1 minute")
    Sys.sleep(60)
  } else if (con$status == "Running" | con$status == "Uploading"){
    cat(paste0("status ", con$status, "\n"))
    cat(paste0("progress ", con$progress, "\n"))
    Sys.sleep(60)
  } else {
    cat(paste0("status ", con$status, "\n"))
    break()
  }
}
liashko <- (con$processingResult %>% jsonlite::fromJSON())$fragments
# data$events is list of events that has data.frame column,
# so it has to be flatten in this loop

liashko$events <- purrr::map(liashko$events, function(events){
  events %>% purrr::map(function(event){
    jsonlite::flatten(event)
  }) %>% bind_rows()
})

liashko <- liashko %>% unnest(events)

library(reshape2)
liashko2 <- melt(liashko[c(1,9:16)], id.vars = "start")
liashko3 <- liashko2 %>% group_by(start,variable) %>% summarise(value = mean(value))

liashko3$time <- liashko3$start/90000

#Динаміка

ggplot(liashko3,aes(time,value)) + geom_line() + facet_wrap(~variable) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") 

#Середні

library(ggplot2)
liashko4 <- liashko3 %>% group_by(variable) %>% summarise(value=mean(value))
ggplot(liashko4, aes(x="", y=value,fill=variable))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

#Коливання

ggplot(liashko2, aes(x=variable, y=value)) + geom_boxplot() +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())


#Динаміка

ggplot(yula3,aes(time,value)) + geom_line() + facet_wrap(~variable) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") 

#Середні

library(ggplot2)
yula4 <- yula3 %>% group_by(variable) %>% summarise(value=mean(value))
ggplot(yula4, aes(x="", y=value,fill=variable))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

#Коливання

ggplot(yula3, aes(x=variable, y=value)) + geom_boxplot() +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

parubiy <- boiko2[boiko2$id==5,]
parubiy2 <- melt(parubiy[c(1,9:16)], id.vars = "start")
parubiy3 <- parubiy2 %>% group_by(variable) %>% summarise(value=mean(value))
parubiy2$time <- parubiy2$start/15925.85
parubiy4 <- parubiy2 %>% group_by(time,variable) %>% summarise(value=mean(value))

#Динаміка

ggplot(parubiy4,aes(time,value)) + geom_line() + facet_wrap(~variable) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") 

#Середні

ggplot(parubiy3, aes(x="", y=value,fill=variable))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

#Коливання

ggplot(parubiy4, aes(x=variable, y=value)) + geom_boxplot() +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())
