#' Get codes of Italian regions
#'
#' @return A data.frame with columns REGI_ID (integer ID), REGI_NAME_I (Italian name) and REGI_NAME_D (German name)
#' @examples
#' summary(pollnet_regions())
#' @importFrom jsonlite read_json
#' @import dplyr ggplot2
#' @importFrom igraph graph_from_data_frame plot.igraph
#' @export
pollnet_regions <- function() {
	Url <- "http://dati.retecivica.bz.it/services/POLLNET_REGIONS?format=json"
	read_json(Url, simplifyVector = TRUE)
}

#' Get description of the monitored pollen and spores
#'
#' @return A data.frame with columns
#' \describe{
#'   \item{PART_ID}{numeric code of the particle}
#'   \item{PARENT_ID}{numeric code of the group to which the particle is assigned}
#'   \item{PARENT_NAME_L}{Latin name of the group to which the particle is assigned}
#'   \item{PART_CODE}{short code (string)}
#'   \item{PART_NAME_I}{Italian name}
#'   \item{PART_NAME_D}{German name}
#'   \item{PART_NAME_E}{English name}
#'   \item{PART_NAME_F}{French name}
#'   \item{PART_NAME_L}{Latin name}
#'   \item{PART_LOW}{lower limit for the concentration class "low"}
#'   \item{PART_MIDDLE}{lower limit for the concentration class "middle"}
#'   \item{PART_HIGH}{lower limit for the concentration class "high"}
#' }
#' @examples
#' summary(pollnet_particles())
#' @export
pollnet_particles <- function() {
	Url <- "http://dati.retecivica.bz.it/services/POLLNET_PARTICLES?format=json"
	read_json(Url, simplifyVector = TRUE)
}

#' Synthetic plot of the particles' classification
#'
#' @param p Particles metadata (data.frame)
#' @param plot_others If TRUE, all classes are included
#' @examples
#' plot_pollnet_particles(plot_others = FALSE)
#' plot_pollnet_particles(plot_others = TRUE)
#' @export
plot_pollnet_particles <- function(p=pollnet_particles(),
				   plot_others=F) {
	p <- p[!is.na(p$PARENT_ID),]
	if(!plot_others) p <- p[p$PART_NAME_L!="Altri",]
	g <- graph_from_data_frame(p[,c("PARENT_NAME_L","PART_NAME_L")], directed=F)
	plot.igraph(g, vertex.shape="none", vertex.label.cex=0.7)
}

#' Get metadata of the monitoring stations
#'
#' @param regi_id Numeric code of the region. If NULL, all regions are considered
#' @return A data.frame with columns
#' \describe{
#'   \item{STAT_ID}{Numeric code}
#'   \item{STAT_CODE}{Short code (string)}
#'   \item{STAT_NAME_I}{Italian name}
#'   \item{STAT_NAME_D}{German name}
#'   \item{REGI_ID}{Numeric code of the region}
#'   \item{REGI_NAME_I}{Italian name of the region}
#'   \item{REGI_NAME_D}{German name of the region}
#'   \item{LATITUDE}{Latitude}
#'   \item{LONGITUDE}{Longitude}
#' }
#' @examples
#' summary(pollnet_stations(regi_id=13))
#' @export
pollnet_stations <- function(regi_id=NULL) {
	Url <- "http://dati.retecivica.bz.it/services/POLLNET_STATIONS?format=json"
	if(!is.null(regi_id)) Url <- paste0(Url,"&regi_id=",regi_id)
	read_json(Url, simplifyVector = TRUE)
}

#' Get data
#'
#' @param part_id Numeric code of the particle (pollen or spore)
#' @param from First day (string YYYY-MM-DD)
#' @param to Last day (string YYYY-MM-DD)
#' @param stat_id Numeric code of the station
#' @param verbose Logical. If TRUE, the URL is printed.
#' @return A data.frame with columns
#' \describe{
#'   \item{PART_SEQ}{progressive number of the particle}
#'   \item{PART_LEVEL}{hierarchy level}
#'   \item{PART_ID}{numeric code of the particle}
#'   \item{PART_NAME_L}{scientific name (Latin) of the particle}
#'   \item{PART_PARENT_NAME_L}{name of the group to which the particle is assigned}
#'   \item{REMA_CONCENTRATION}{average daily particle concentration}
#'   \item{REMA_DATE}{date}
#'   \item{STAT_ID}{numeric code assigned to the monitoring station}
#'   \item{STAT_CODE}{alphanumeric code of the monitoring station}
#'   \item{STAT_NAME_I}{Italian name of the monitoring station}
#'   \item{STAT_NAME_D}{German name of the monitoring station}
#' }
#' @examples
#' pollnet_data_station(part_id = 1352, from = "2018-01-01", to = "2018-01-10", stat_id = 84)
#' @export
#' @description \code{pollnet_data_station} returns data of a single particle type, of a single monitoring station (max 5000 rows)
pollnet_data_station <- function(part_id=NULL, from=NULL, to=NULL, stat_id, verbose=F) {
	Url <- "http://dati.retecivica.bz.it/services/POLLNET_REMARKS?format=json"
	if(!is.null(part_id)) Url <- paste0(Url,"&PART_ID=",part_id)
	if(!is.null(from))    Url <- paste0(Url,"&from=",   from   )
	if(!is.null(to))      Url <- paste0(Url,"&to=",     to     )
	Url <- paste0(Url,"&STAT_ID=",stat_id)
	if(verbose) cat(Url, sep="\n")
	read_json(Url, simplifyVector = TRUE) -> dat
	if(length(dat)>0 && nrow(dat)>0) {
		dat %>%
		dplyr::mutate(REMA_DATE=as.POSIXct(.data$REMA_DATE))-> dat
	} else {
		dat <- NULL
	}
	return(dat)
}

#' @rdname pollnet_data_station
#' @description \code{pollnet_data} returns data of all the monitoring stations
#' @param part_ids Vector of numeric codes of the particles (pollen and/or spore)
#' @param ... Additional parameters passed to pollnet_data_station
#' @export
pollnet_data <- function(part_ids, from, to, ...) {
	stat_ids <- pollnet_stations()$STAT_ID
	args <- expand.grid(part_id=part_ids, stat_id=stat_ids)
	p_ds <- function(x) pollnet_data_station(part_id=x[1],
						 stat_id=x[2],
						 from=from,
						 to=to,
						 verbose=F)
	Dat <- do.call(bind_rows,apply(X=args, FUN=p_ds, MARGIN=1))
	Dat <- left_join(Dat %>%
			 dplyr::select(-.data$STAT_CODE,-.data$STAT_NAME_I,-.data$STAT_NAME_D),
			 pollnet_stations())
	return(Dat)
}

#' Synthetic plot of the available pollen and spores
#'
#' @param from First day (string YYYY-MM-DD)
#' @param to Last day (string YYYY-MM-DD)
#' @examples
#' \dontrun{
#' p <- pollnet_overview(from="2019-03-01", to="2019-03-01")
#' print(p)#' ggsave(p, filename=paste0("overview_",from,"_",to,".pdf"), height=22,width=14)
#' }
#' @export
pollnet_overview <- function(from="2019-03-01", to="2019-06-01") {
	Dat <- pollnet_data(pollnet_particles()$PART_ID, from, to)
	Dat %>%
	 group_by(.data$PART_PARENT_NAME_L,
	          .data$PART_NAME_L,
	          .data$REGI_NAME_I,
	          .data$STAT_CODE) %>%
	 summarize(n=n()) %>%
	 ungroup() %>%
	 group_by(.data$PART_PARENT_NAME_L,
	          .data$PART_NAME_L,
	          .data$REGI_NAME_I) %>%
	 summarize(n_rows=sum(n), available=n()>0) -> dat
	ggplot(dat) +
	 geom_point(aes_string(x="REGI_NAME_I", y="PART_NAME_L", col="available"), shape=16) +
	 facet_wrap("PART_PARENT_NAME_L", scales="free") +
	 theme_bw() +
	 xlab("") +ylab("") +
	 theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) -> p
	return(p)
}

