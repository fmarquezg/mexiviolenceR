#' Collecting data from web
#'
#' This function allows you pull data once and format it in a user friendly way
#' 
#' @examples
#' collect_data()


collect_data <-function() {
  
  url<-'http://datosabiertos.segob.gob.mx/DatosAbiertos/SESNSP/IDVFC_NM_'
  data<-readr::read_csv(url,locale = readr::locale(encoding = "latin1"))
  #data<-readr::read_csv(url)
  df<-data
  
  df%>%dplyr::filter(`Tipo de delito`=='Homicidio', `Subtipo de delito`=='Homicidio doloso') -> hom
  hom %>%
    dplyr::rename(year = "Año")->hom
  
  hom%>%
    dplyr::select(year,Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre)%>%
    dplyr::group_by(year)%>%
    dplyr::summarise_all(funs(sum))%>%
    tidyr::gather(month_name,hom, Enero:Diciembre)%>%
    dplyr::mutate(
      month = case_when(
        month_name == 'Enero' ~ '01',
        month_name == 'Febrero' ~ '02',
        month_name == 'Marzo' ~ '03',
        month_name == 'Abril' ~ '04',
        month_name == 'Mayo' ~ '05',
        month_name == 'Junio' ~ '06',
        month_name == 'Julio' ~ '07',
        month_name == 'Agosto' ~ '08',
        month_name == 'Septiembre' ~ '09',
        month_name == 'Octubre' ~ '10',
        month_name == 'Noviembre' ~ '11',
        month_name == 'Diciembre' ~ '12'
      )
    )%>%
    dplyr::select(-month_name)%>%
    dplyr::mutate(date=paste0(year,'-',month,'-01'))%>%
    dplyr::select(-month)%>%
    dplyr::mutate(date=as.Date(date)) ->deaths
  return(deaths)
}