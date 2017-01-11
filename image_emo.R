library(httr)

# Below is the URL having returnFaceLandmarks = true and returnFaceAttributes = age,gender,headPose,smile,facialHair,glasses

face_api_url = "https://api.projectoxford.ai/emotion/v1.0/recognize"

# Below is the image we are going to upload

body_image = upload_file(file.choose())

filenames <- list.files("~/Право на владу/Наталія Мосейчук", pattern="*.jpg", full.names=TRUE)
body_mosiychuk <- lapply(filenames,upload_file)

# Below is the POST methord (Adding Request headers using add_headers)


result = POST(face_api_url,
              body = body_mosiychuk[[1]],
              add_headers(.headers = c("Content-Type"="application/octet-stream",
                                       "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))

API_Output = content(result)

# Coverting Output into R Dataframe 

Output_Face_Attributes = as.data.frame(API_Output)

system.time(mosiychuk_df <- do.call("rbind", lapply(body_mosiychuk[371:390],function (x) {as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))})))


mosiychuk_all <- lapply(body_mosiychuk[1:40],function (x) {as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))
  Sys.sleep(5)})

ggplot(moseychuk,aes(time,scores.happiness)) + geom_bar(stat = "identity", fill="black") + theme_classic()

ggplot(moseychuk,aes(time)) + 
  geom_point(aes(y=scores.happiness),color="#ff7f00") +
  geom_point(aes(y=scores.sadness),color="#8da0cb") +
  geom_point(aes(y=scores.surprise),color="#4daf4a") +
  geom_point(aes(y=scores.fear),color="#e78ac3") +
  geom_point(aes(y=scores.disgust),color="#ffff33") +
  geom_point(aes(y=scores.contempt),color="#e5c494") +
  geom_point(aes(y=scores.anger),color="#e41a1c") +
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

library(qlcMatrix)
moseychuk$color <- rowMax(as.matrix(moseychuk[c(5:9,11,12)]))

moseychuk$emo <- ifelse(moseychuk$color==moseychuk$scores.anger,"Злість" ,ifelse(
  moseychuk$color==moseychuk$scores.contempt,"Зневага",ifelse(moseychuk$color==moseychuk$scores.disgust,"Відраза",
                                                  ifelse(moseychuk$color==moseychuk$scores.fear,"Страх",
                                                         ifelse(moseychuk$color==moseychuk$scores.happiness,"Радість",
                                                                ifelse(moseychuk$color==moseychuk$scores.sadness,"Сум","Здивування")
)))))

ggplot() +
  geom_linerange(aes(
    x=tl3$`Наталія Мосійчук`[c(1:48,51:89,91:147,151:264,271:283,291:305,311:329,331:347,351:428,431:454)], 
    y=0, ymin=0, ymax=1,color=moseychuk$emo)) +
  scale_color_manual(values=c("#4daf4a","#377eb8","#ff7f00","#e5c494"))+
  scale_x_continuous(breaks = c(900,1800,2700,3600,4500,5400)) + theme_classic()

ggplot(data.frame(unname(ms),names(ms)), aes(y=Freq,x=names.ms.,fill=names.ms.)) + 
  scale_fill_manual(values=c("#4daf4a","#e78ac3","#ff7f00","#8da0cb"))+
  geom_bar(stat="identity") + 
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none")+
  coord_flip()



body_image = upload_file(file.choose())

filenames <- list.files("~/Право на владу/Юрій Луценко", pattern="*.jpg", full.names=TRUE)
body_lutsenko <- lapply(filenames,upload_file)

test <- for (i in body_lutsenko[1:20]) {
  as.data.frame(
    content(
      POST(
        face_api_url, body = i,
        add_headers(.headers = c("Content-Type"="application/octet-stream",
                                 "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))
  Sys.sleep(6)
}

# Below is the POST methord (Adding Request headers using add_headers)


result = POST(face_api_url,
              body = body_mosiychuk[[1]],
              add_headers(.headers = c("Content-Type"="application/octet-stream",
                                       "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))

API_Output = content(result)

# Coverting Output into R Dataframe 

Output_Face_Attributes = as.data.frame(API_Output)

system.time(mosiychuk_df <- do.call("rbind", lapply(body_mosiychuk[371:390],function (x) {as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))})))


test <- lapply(body_lutsenko[1:10],function (x) {as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))
  Sys.sleep(2)
})

as.data.frame(
  content(
    POST(
      face_api_url, body = body_lutsenko[[1]],
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))

lutsenko1_20 <- do.call("rbind", lapply(body_lutsenko[1:20],function (x) {as.data.frame(
  content(
    POST(
      face_api_url, body = x,
      add_headers(.headers = c("Content-Type"="application/octet-stream",
                               "Ocp-Apim-Subscription-Key"="4ef1081e34874d358fd97dcbac907180")))))}))
