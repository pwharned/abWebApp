
##source('/Users/pharned/Documents/Arab_Barometer_Team/Wraps/Functions.R')

##for(i in c('survey', 'haven', 'stringr',"readxl","ggplot2", "purrr", 'dplyr', 'tidyr','extrafont')){suppressMessages(library(i,character.only = TRUE, quietly = T))}

# usage
packages <- c('survey', 'haven', 'stringr',"readxl","ggplot2", "purrr", 'dplyr', 'tidyr','extrafont')




paths = c(abv_en = "ABV_Crossectional_Data_Release_ENG.dta", abv_ara =  "ABV_Crosssectional_Data_Release_ARA.dta")


  labeling=read_xlsx(paste(getwd(), "/titles/titles.xlsx",sep = ""), sheet = 4)
  arabic_labeling = read_xlsx( paste(getwd(), "/titles/titles.xlsx",sep = ""), sheet = 5)
  source(paste(getwd(), '/AB_Colors.R', sep = ""))

abv_en = read_dta("ABV_Crossectional_Data_Release_ENG.dta")


`%nin%`= negate(`%in%`)

explore = function(dataframe){
  
  map(dataframe, var_lab)%>%
    set_names(names(dataframe))
}


abdesign = partial(svydesign, ids=~id, weights=~wt, strata = ~stratum, nest = TRUE)

find_variable = function(string, dataframe, names=FALSE){

    if(names == FALSE){
      position = grep(string, explore(dataframe), ignore.case = TRUE)
    }else{
      position = grep(string, names(explore(dataframe)), ignore.case = TRUE)
    }
    find_variable=c()
    for (i in seq_along(position)) {
      pos=position[[i]]
      name = explore(dataframe)[[pos]]
      variable = names(explore(dataframe))[[pos]]
      find_variable[[i]]=variable
      names(find_variable)[[i]]=name
    }
  
  
  remove(position, pos)
  return(unlist(find_variable))
}


recode_variable=function(dataframe, variable){
  vallab = val_lab(dataframe[[variable]]) 
  varlab = var_lab(dataframe[[variable]])
  for(i in seq_along(val_lab(dataframe[[variable]]))){
    j= names(val_lab(dataframe[[variable]]))[[i]]
    k= val_lab(dataframe[[variable]])[[i]]
    dataframe[[variable]] = ifelse(dataframe[[variable]]==k,j, dataframe[[variable]])
    val_lab(dataframe[[variable]]) = val_lab(dataframe[[variable]])
    var_lab(dataframe[[variable]]) = var_lab(dataframe[[variable]])
  }
  val_lab(dataframe[[variable]]) = vallab
  var_lab(dataframe[[variable]]) = varlab
  return(dataframe)
}

recode_country=function(dataframe){
  dataframe$Country=dataframe$country
  for(i in seq_along(val_lab(dataframe$country))){
    j= names(val_lab(dataframe$country))[[i]]
    k= val_lab(dataframe$country)[[i]]
    dataframe$Country = ifelse(dataframe$country==k,j, dataframe$Country)
    val_lab(dataframe$Country) = val_lab(dataframe$country)
    var_lab(dataframe$Country) = var_lab(dataframe$country)
  }
  return(dataframe)
}





title_function= function (variable, lang = "EN", number = FALSE){
  if(lang =="EN"){
    if(number == TRUE){
      title = paste(variable,str_wrap(c(labeling[[variable]][[1]]), width = 50), sep = "\n")
    }else{
      title = str_wrap(c(labeling[[variable]][[1]]), width = 50)
    }
  }
  else{
    title = str_wrap(arabic_labeling[[variable]][[1]],width = 60)
  }
  return(title)
}

short_title =function (variable){
  title = c(labeling[[variable]])
  paste(title[2], sep = "\n ")
}



subtitle_function= function (variable, lang = "EN"){
  if(lang =="EN"){
    title = c(Subtitles_Frame[[variable]])
    title = paste("% saying ", title, sep = "")
  }
  else{
    title = c(Subtitles_Frame[[variable]])
    title = paste(arabic_labeling['subtitle'], title, sep = " ")
  }
  
  return(title)
}


caption_function = function(variable){
  
  
  strwrap(paste(labeling[["caption"]][[1]], as.character(variable), sep = ". "   )   )
  
}


plotterizer = function(dataframe, x, y, fill = NA, pallette=NA, lang = "EN"){
  dataframe = select(dataframe,starts_with(x), starts_with(y))%>%
                       filter_at(vars(starts_with(x)), any_vars(.%nin%c(0,NA, NaN)))%>%
    filter_at(vars(starts_with(x)), any_vars(.<1))

  fill=dataframe[[fill]]
  legend_title=names(dataframe)[[2]]
  title1 = title_function(x, lang = lang)
  subtitle = subtitle_function(x, lang = lang)
  variable = dataframe[[x]]
  y= dataframe[[y]]
  if(length(fill)==0){
    plot=ggplot(dataframe, aes(reorder(y,variable), variable*100, fill=fill))+
      geom_bar(stat = "identity",position = position_dodge(width = 1), width = .8, fill=country_color)
  }else{
    plot=ggplot(dataframe, aes(reorder(y,variable), variable*100, fill=fill))+
      geom_bar(stat = "identity",position = position_dodge(width = 1), width = .8)
  }
  plot=plot+
    coord_flip()+
    geom_text(aes(label=round(variable*100), hjust = -.5),size=2.6, position = position_dodge(width = 1))+
    theme_bw()+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 11, hjust = 0),  
          plot.title = element_text(size=14, hjust=0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
          axis.text.y = element_text(angle = 45, size = 12),
          text = element_text(family = "Arial"),
          panel.border = element_rect(colour = "black", fill=NA, size=.6),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())+ylim(0,100)+
    labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5", fill=legend_title)+
    xlab("")+ 
    ylab("Percent")+ggtitle(title1, subtitle = subtitle)
  if(is.na(pallette)==TRUE){
    return(plot)
  }else{
    plot=plot+scale_fill_ab(pallette)
    return(plot)
  }
  
}




grouping_function=function(dataframe, x=NA, group=NA, varlist){
  
  print(x)
  dataframe%>%
    group_by(!!x)%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))%>%
    filter(!is.na(!!x))
}


restore_labels = function(dataframe1, dataframe2){
  for (i in seq_along(dataframe1)){
    if(names(dataframe1)[i]==names(dataframe2)[i]){
      print(names(dataframe2)[i])
      val_lab(dataframe2[i])=val_lab(dataframe1[i])
      var_lab(dataframe2[i])=var_lab(dataframe1[i])
    }
    
    
  }
  return(dataframe2)
  
}




country_grouping_function=function(dataframe, varlist){
  
  dataframe%>%
    group_by(country)%>%
    summarise_at(varlist,list(~round(weighted.mean(.*100, w=wt, na.rm = TRUE))))%>%
    mutate(Country = names(val_lab(dataframe$country)[val_lab(dataframe$country)%nin%c(16)])     )
}

individual_country_function = function(dataframe, varlist){
  
  dataframe%>%
    summarise_at(varlist,list(~weighted.mean(., w=wt, na.rm = TRUE)))
}



extrapolate_names = function(x, lang = "EN"){
  
  if(lang =="EN"){
    names(val_lab(abv_en[x]  ) [  val_lab(abv_en[x])%in%unique(abv_en[[x]])         ])
  }else{
    names(val_lab(abv_ara[x]  ) [  val_lab(abv_ara[x])%in%unique(abv_ara[[x]])         ])
  }
 
  
  
}




