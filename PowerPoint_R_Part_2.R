# E-mail: asif.salam@hotmail.com
library(RDCOMClient)
library(plyr)
library(dplyr)
library(stringr)
library(rvest)

#IMDB's Caterine Frot page
Actor_url  <-"http://www.imdb.com/name/nm0634159/" #Philippe Noiret
# IMDB's Clint Eastwood page
Actor_url <- "http://www.imdb.com/name/nm0000142/" #Clint Eastwood

gender="male"

Actor_url  <- "http://www.imdb.com/name/nm0296594/" #Frot
gender="female"

#Set your local path here
local_path <- "."
local_folder<- "html"
local_file <- paste(local_path,local_folder,"Catherine Frot - IMDb.html",sep="/")
local_file <- paste(local_path,local_folder, "Philippe Noiret - IMDb.html",sep="/")
local_file <- paste(local_path,local_folder, "Clint Eastwood - IMDb.html",sep="/")
# Create the actor sub-directory
if (!file.exists(local_folder)) dir.create(local_folder)
#pre-download the url page manually into the local folder to test if reading works (even without internet)
test_page <- read_html(local_file)
Actor_page <- read_html(Actor_url) #anonymous, now only need change acthor URL
Actor_name<- (Actor_page %>% html_nodes('.itemprop')%>% html_text() %>% str_trim())[[1]]
#xpath="//*[contains(concat( "" "", @class, "" "" ), concat( "" "", ""itemprop"", "" "" ))]")
local_folder<-Actor_name
#film_selector <- ".filmo-row"
if(gender=="male"){
film_selector <- "#filmo-head-actor+ .filmo-category-section .filmo-row"} else {
film_selector <- "#filmo-head-actress+ .filmo-category-section .filmo-row"}
filmography <- Actor_page %>% html_nodes(film_selector)

# Remove "TV Series" & "TV Movie" from the data
filmography <- filmography[-grep("TV Movie|TV Series",html_text(filmography))]

# Create the films data frame
films <- NULL
films$year <- filmography %>% html_nodes("span") %>% html_text() %>% str_trim()%>% str_extract("\\d+") #eliminate rubbish characters in year
films$title <- filmography %>% html_nodes("b a") %>% html_text() # some titles are in italian. why?
films$url <- paste0("http://www.imdb.com",filmography %>% html_nodes("b a") %>% html_attr("href"))
films <- as.data.frame(films,stringsAsFactors=FALSE)

#Create an index so the dataframe can be sorted back.
films$index <- sprintf("%02d",seq_along(1:length(films$year)))

films$img_file <- paste0(local_folder,"/img",films$index,".jpg")

# Extract the character name and add to dataframe
# check exact xpath with Selectorgadget chrome extention #IMDB xpaths can change over time.
get_character <- function(film,filmography) {
    i <- as.integer(film$index)
    character_name <- filmography[[i]] %>% html_nodes("br+ a") %>% html_text()  #xpath=".//a[2]"
    if (length(character_name)==0) {
        character_name <- filmography[[i]] %>%
                            html_nodes(xpath="text()[preceding-sibling::br]") %>%
                            html_text() %>%
                            str_trim() %>%
                            str_replace("\n"," ")
    }
    return(character_name)
}

films$character_name <- daply(films,.(index),get_character,filmography) #had error: not same dimensions. because of xpath in get_character ".//a[2] then eror

# Loop through the films and download the poster image into the "img" subdirectory.
# If the poster is not found, flag the file name with 0.
for (i in 1:nrow(films)) {
    img_node <- read_html(films$url[i]) %>%
                html_nodes(xpath='//*[(@id = "title-overview-widget")]//img') #//td[@id="img_primary"]//img_primary
    if (length(img_node)==0) {
        films$img_file[i] <- "img00.png"
        cat(i,"th img file NOT FOUND: replacing by ",films$img_file[i],"\n")
    }
    else {
        img_link <- html_attr(img_node,"src")
        cat(i," :",films$img_file[i]," : ",img_link,"\n")
        download.file(img_link,films$img_file[i],method="internal",mode="wb")
    }
}

# Check which of the files were not found and download them manually
films$title[which(films$img_file=="img00.png")]
#none!

# These images don't exist.  Download manually if you want them and place them in files below
if(Actor_name="Clint Eastwood"){
  films[55,"img_file"] <- "img/img55.jpg"
  films[54,"img_file"] <- "img/img54.jpg"
  films[52,"img_file"] <- "img/img52.jpg"

  # Correct this title (appears with strange characters because of my locale)
  films[40,"title"] <- "Kelly's Heroes"
}
# Save the data frame
write.table(films,file=paste(local_path,local_folder,"films.tsv", sep="/"),append=FALSE,quote=TRUE,sep="\t",row.names=FALSE)
write.table(films,file=paste(local_path,local_folder,"_films.csv", sep="/"),append=FALSE,quote=TRUE,sep=",",row.names=FALSE)

#------------------------------------- Films dataframe done -------------------------------------#


# =====================================Create a dataframe for box office earnings data ========#
##  Get box office earnings data for the films
box_office_url <- paste("http://www.boxofficemojo.com/people/chart/?id=",gsub(" ","",Actor_name),".htm",sep="")
box_office_page <- read_html(box_office_url)
# Extract tables. The fourth table is the one we want, with adjusted box office returns
bo<- html_table(box_office_page, header=TRUE,fill=TRUE)[[4]] #should be same as following
bo <- box_office_page %>% html_table(header=TRUE,fill=TRUE) %>% (function(x) {x[[4]]})
# Clean up dataframe and correct formats
names(bo) <- c("bo_rank","title_name","studio","adjusted_gross","unadjusted_gross","release_date")
bo$adjusted_gross <- as.numeric(gsub("[\\$\\,]","",bo$adjusted_gross))
bo$unadjusted_gross <- as.numeric(gsub("[\\$\\,]","",bo$unadjusted_gross))
bo$release_date <- strptime(bo$release_date,"%m/%d/%y")
if(Actor_name="Clint Eastwood"){bo$release_date[32] <- strptime("1975-06-15",format("%Y-%m-%d"))}
#bo$release_date <- correct_date(box_office$release_date)

# Create a key for joining dataframes using the film title
bo$key <- tolower(gsub("[^[:alnum:]]", "", bo$title))
films$key <- tolower(gsub("[^[:alnum:]]", "", films$title))
if(Actor_name="Clint Eastwood"){bo$key[34] <- "therookie"}

# Create a dataframe for the box office gross for the movies
box_office <- left_join(select(bo,bo_rank,studio,adjusted_gross,key),select(films,year,title,index,key),by="key")

# Save the box_office data frame. twice, as tsv and as csv. note the csv when opened might produce problems with commas in titles.
write.table(box_office,file=paste(local_path,local_folder,"box_office.tsv", sep="/"),append=FALSE,quote=TRUE,sep="\t",row.names=FALSE)
write.table(box_office,file=paste(local_path,local_folder,"box_office.csv", sep="/"),append=FALSE,quote=TRUE,sep=",",row.names=FALSE)
