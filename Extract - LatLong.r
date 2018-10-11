######################
# AJUSTE NOME DE RUA #
######################

require(stringr)
require(magrittr)
require(rvest)

rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}


nome_rua = function(rua){
  rua %<>% str_to_upper()
  rua %<>% rm_accent(.)
  rua %<>% str_remove_all("N°")

  fim = str_locate_all(rua,"-|-")[[1]]
  if(sum(fim)!=0){
    bairro = str_trim(str_sub(rua,start = fim[nrow(fim),1]+1))
    rua = str_trim(str_sub(rua,start = 1,end=fim[1,1]-1))
  }else{bairro=NULL}
  
  rua %<>% str_replace_all("[[:punct:]]", " ")
  
  rua %<>% str_replace("^R ","RUA ")
  rua %<>% str_replace("^PC","PRACA")
  rua %<>% str_replace("CEL ","CORONEL ")
  rua %<>% str_replace("GR ","GRAO ")
  rua %<>% str_replace("DR ","DOUTOR ")
  rua %<>% str_replace("TV ","TRAVESSA ")
  rua %<>% str_replace("^AV ","AVENIDA ")
  rua %<>% str_replace("DQ ","DUQUE ")
  rua %<>% str_replace("JD ","JARDIM ")
  rua %<>% str_replace("QD ","QUADRA ")
  rua %<>% str_replace("SL ","SALA ")
  rua %<>% str_remove_all(.,"TERREO|BLOCO|BL|APT|APARTAMENTO")
  
  sala = str_locate(rua,"SALA|LOJA|QUADRA|QD")[1]
  if(!is.na(sala)){
    rua = str_trim(str_sub(rua,start = 1,end=sala-1))
  }
  
  rua = paste(rua,bairro)
  
  rua %<>% str_squish(.)
  
  return(rua)
}

#####################################################################


#GoogleKeyAPI_latlong - Key do google maps api

#Here_App_ID, Here_App_Code - Keys do here maps

GetLatLong = function(endereco="",GoogleKeyAPI="",app_id = "",app_code="" ,source="dsk"){
	require(rvest)
  require(stringr)
  
  if(source=="dsk"){
    link = paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?address=",endereco)
    
    link = gsub(" {2,}","",link)
    link = gsub(" ","+",link)
    
    try(download.file(link, destfile = paste0(tempdir(),"\\scrapedpage.json"), quiet=TRUE))
    
    if(file.exists(paste0(tempdir(),"\\scrapedpage.json"))){
      aux = jsonlite::fromJSON(paste0(tempdir(),"\\scrapedpage.json"))
    }else{
      return(data.frame(lat=NA,
                        lng=NA))
    }
    
    file.remove(paste0(tempdir(),"\\scrapedpage.json"))
    
    aux = jsonlite::fromJSON(paste0(tempdir(),"\\scrapedpage.json"))
    
    file.remove(paste0(tempdir(),"\\scrapedpage.json"))
    
    return(data.frame(lat=aux$results$geometry$location$lat,
                      lng=aux$results$geometry$location$lng))
  }else{
    if(source=="google"){
      link = ifelse(GoogleKeyAPI=="",
             paste0("https://maps.googleapis.com/maps/api/geocode/xml?address=",endereco),
             paste0("https://maps.googleapis.com/maps/api/geocode/xml?address=",endereco,"&key=",GoogleKeyAPI))
      
      link = gsub(" {2,}","",link)
      link = gsub(" ","+",link)
      
      try(download.file(link, destfile = paste0(tempdir(),"\\scrapedpage.html"), quiet=TRUE))
    
    	site = read_html(paste0(tempdir(),"\\scrapedpage.html"))
    	
    	file.remove(paste0(tempdir(),"\\scrapedpage.json"))
    	
    	if(html_text(html_node(site,"status"))=="ZERO_RESULTS"){
    	  return(data.frame(lat=NA,
    	                    lng=NA,
    	                    bairro="Endereço não encontrado"))
    	} else{
    	
    	if(html_text(html_node(site,"status"))=="OVER_QUERY_LIMIT"){
    	  return(data.frame(lat=NA,
    	                    lng=NA))#,
    	                    #bairro="Limite excedido"))
    	}
    	
    	else{
    
    	  aux = site %>% html_nodes("address_component")
    	  aux2=data.frame(name=NA,type=NA)
    	  
    	  
    	  for(i in 1:length(aux)[1]){
    	    aux2[i,] = list(aux[i] %>% html_node("long_name") %>% html_text(),aux[i] %>% html_node("type") %>% html_text())
    	  }
    	  
    	  return(data.frame(lat=as.numeric(html_text(html_node(site,"lat"))),
    	                    lng=as.numeric(html_text(html_node(site,"lng")))))
    	}
    	}
    }
    else{
      if(source == "here"){
        if(app_id == ""|app_code == ""){
          stop("Falta identificação")
          }else{
            
            link = paste0("https://geocoder.cit.api.here.com/6.2/geocode.json?searchtext=",endereco,
                          "&app_id=",Here_App_ID,"&app_code=",Here_App_Code,"&gen=8")
            link = gsub(" ","%20",link)
            
            try(download.file(link, destfile = paste0(tempdir(),"\\scrapedpage.json"), quiet=TRUE))
            
            if(file.exists(paste0(tempdir(),"\\scrapedpage.json"))){
              aux = jsonlite::fromJSON(paste0(tempdir(),"\\scrapedpage.json"))
            }else{
              return(data.frame(lat=NA,
                                lng=NA))
            }
            
            file.remove(paste0(tempdir(),"\\scrapedpage.json"))
            
            return(data.frame(lat=aux$Response$View$Result[[1]]$Location$DisplayPosition$Latitude[1],
                              lng=aux$Response$View$Result[[1]]$Location$DisplayPosition$Longitude[1]))
            
          }
      }else(stop("source error"))
    }
  }
}


########################################################################################


GetLatLongNG = function(cep){
  require(rvest)
  require(stringr)
  
  cep = cep %>% as.character()
  
  while(str_length(cep)<8){
    cep=paste0("0",cep)
  }
  
  link=paste0("http://www.qualocep.com/busca-cep/",cep)
  
  download.file(link, destfile = paste0(tempdir(),"\\scrap.html"), quiet=TRUE)
  
  site=read_html(paste0(tempdir(),"\\scrap.html"),encoding = "UTF-8")
  
  if(length(site)==0){
    return("Endereço não encontrado")
  }
  
  else{
  latlong=site %>% html_nodes(xpath = "/html/body/div[2]/div[8]/div[1]/h4") %>% html_text()
  
  if(length(latlong)==0){
    return("Endereço não encontrado")
  }
  else{
  #Latitude
  lat_ini=latlong %>% str_locate("Latitude: ")
  lat_ini=lat_ini[2]
  
  lat_fim=latlong %>% str_locate(" /")
  lat_fim=lat_fim[1]  
  
  lat=str_sub(latlong,lat_ini+1,lat_fim-1)[[1]]
  
  
  #Longitude
  long_ini=latlong %>% str_locate("Longitude: ")
  long_ini=long_ini[2]
  
  long=str_sub(latlong,long_ini+1)[[1]]
  
  bairro=site %>% html_table()
  
  bairro=unlist(bairro[[1]])[3]
  
  return(c(latitude=lat,lontitude=long,bairro = bairro))
  }
  }
}
