#Analisis Data Covid-19 Di DKI Jakarta

#Library yang digunakan
library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)

#Mengakses API covid.go.id dan Mengekstrak isi Respon
resp<-GET("https://data.covid19.go.id/public/api/update.json")
status_code(resp)
headers(resp)
cov_id_raw<-content(resp,as="parsed",simplifyVector=TRUE)
length(cov_id_raw)
names(cov_id_raw)
cov_id_update<-cov_id_raw$update
lapply(cov_id_update,names)

#Menganalisa Data
cov_id_update$penambahan$tanggal #tanggal pembaharuan data penambahan kasus
cov_id_update$penambahan$jumlah_sembuh #jumlah penambahan kasus sembuh
cov_id_update$penambahan$jumlah_meninggal #jumlah penambahan kasus meninggal
cov_id_update$total$jumlah_positif #jumlah total kasus positif hingga saat ini
cov_id_update$total$jumlah_meninggal #jumlah total kasus meninggal hingga saat ini

#Apa Kabar DKI Jakarta?
resp_jkt<-GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")
cov_jkt_raw<-content(resp_jkt, as="parsed",simplifyVector=TRUE)
names(cov_jkt_raw)
cov_jkt_raw$kasus_total #total kasus per tgl 01/10/20
cov_jkt_raw$meninggal_persen #persen meninggal per tgl 01/10/20
cov_jkt_raw$sembuh_persen #persen sembuh per tgl 01/10/20

#Memperoleh Informasi yang lebih lengkap
cov_jkt<-cov_jkt_raw$list_perkembangan
str(cov_jkt)
head(cov_jkt)

#Menjinakkan Data
new_cov_jkt<-
  cov_jkt %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru=KASUS,
    meninggal=MENINGGAL,
    sembuh=SEMBUH) %>%
  mutate(
    tanggal=as.POSIXct(tanggal/1000, origin="1970-01-01"),
    tanggal=as.Date(tanggal)
  )
str(new_cov_jkt)
head(new_cov_jkt)

#Visualisasi Kasus Harian Positif
ggplot(new_cov_jkt, aes(tanggal, kasus_baru)) +
  geom_col(fill="salmon")+
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Harian Positif COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position="plot")

#Visualisasi Kasus Harian Sembuh
ggplot(new_cov_jkt, aes(tanggal, sembuh)) +
  geom_col(fill ="magenta") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di DKI JAKARTA",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#Visualisasi Meninggal Harian
ggplot(new_cov_jkt, aes(tanggal, meninggal)) +
  geom_col(fill ="darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di DKI JAKARTA",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#Apakah pekan ini lebih baik?
cov_jkt_pekanan<-new_cov_jkt %>%
  count(tahun=year(tanggal),
        pekan_ke=week(tanggal),
        wt=kasus_baru,
        name="jumlah"
  )
glimpse(cov_jkt_pekanan)
View(cov_jkt_pekanan)

#Menjawab Pertanyaan
cov_jkt_pekanan<-
  cov_jkt_pekanan %>%
  mutate(
    jumlah_pekanlalu=dplyr::lag(jumlah, 1),
    jumlah_pekanlalu=ifelse(is.na(jumlah_pekanlalu),0,jumlah_pekanlalu),
    lebih_baik=jumlah<jumlah_pekanlalu
    
  )
glimpse(cov_jkt_pekanan)
head(cov_jkt_pekanan)

#Visualisasi Kasus Pekanan Positif dengan Bar Chart
ggplot(cov_jkt_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:40, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = "Pekan Ke-",
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di DKI JAKARTA",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#Pola dan Dinamika
cov_jkt_akumulasi <- new_cov_jkt %>%
  transmute(
    tanggal,
    akumulasi_aktif=cumsum(kasus_baru)-cumsum(sembuh)-cumsum(meninggal),
    akumulasi_sembuh=cumsum(sembuh),
    akumulasi_meninggal=cumsum(meninggal)
  )
tail(cov_jkt_akumulasi)
View(cov_jkt_akumulasi)
ggplot(data=cov_jkt_akumulasi,aes(x=tanggal, y=akumulasi_aktif))+
  geom_line() #Membuat Line Chart

#Kabar buruk dan kabar baik
#Transformasi Data
dim(cov_jkt_akumulasi)
cov_jkt_akumulasi_pivot<-cov_jkt_akumulasi %>%
  pivot_longer(
    cols=-tanggal,
    names_to = "kategori",
    names_prefix = "akumulasi_",
    values_to="jumlah"
  )
View(cov_jkt_akumulasi_pivot)

#Dinamika Kasus Covid Di DKI Jakarta
ggplot(cov_jkt_akumulasi_pivot,aes(tanggal,jumlah,colour=(kategori)))+
  geom_line(size=0.9)+
  scale_y_continuous(sec.axis = dup_axis(name=NULL))+
  scale_colour_manual(
    values=c(
      "aktif"="salmon",
      "meninggal"="darkslategray4",
      "sembuh"="magenta"
    ),
    labels=c("Positif Aktif","Meninggal","Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colours=NULL,
    title="Dinamika Kasus COVID-19 di DKI Jakarta",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust=0.5),
    legend.position = "top"
  )

head(cov_jkt_akumulasi_pivot)
