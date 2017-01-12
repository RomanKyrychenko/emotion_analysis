library(httr)

face_api_url = "https://api.projectoxford.ai/emotion/v1.0/recognize"

filenames <- list.files("folder_name", pattern="*.png", full.names=TRUE)
body <- lapply(filenames,upload_file)

a <- lapply(body,function (x) {
  a <- as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="5cfb1d6831e0478e8567a34660dea5d7")))))
  Sys.sleep(2.3)
  a
  })

b <- do.call("smartbind",a)

b$time <- c(1:length(body))

ggplot(b,aes(time,scores.happiness)) + geom_bar(stat = "identity", fill="black") + theme_classic()

ggplot(b,aes(time)) + 
  geom_point(aes(y=scores.happiness),color="#ff7f00", alpha=0.4) +
  geom_point(aes(y=scores.sadness),color="#8da0cb", alpha=0.4) +
  geom_point(aes(y=scores.surprise),color="#4daf4a", alpha=0.4) +
  geom_point(aes(y=scores.fear),color="#e78ac3", alpha=0.4) +
  geom_point(aes(y=scores.disgust),color="#ffff33", alpha=0.4) +
  geom_point(aes(y=scores.contempt),color="#e5c494", alpha=0.4) +
  geom_point(aes(y=scores.anger),color="#e41a1c", alpha=0.4) +
  scale_y_continuous(labels = percent) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none")

ggplot(b,aes(time)) + 
  geom_point(aes(y=scores.sadness),color="#8da0cb", alpha=1) +
  geom_point(aes(y=scores.surprise),color="#4daf4a", alpha=0.5) +
  geom_point(aes(y=scores.disgust),color="#ffff33", alpha=0.5) +
  geom_point(aes(y=scores.anger),color="#e41a1c", alpha=0.4) +
  scale_y_continuous(labels = percent) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="top")