# Грузить данные желательно в последние несколько дней месяца

#### LOADING SVNC FACT DATA ####
rm(list = ls())

# Loading libraries
library(RJDBC)
library(rJava)
library(tsauxfunc)
library(dplyr)

# setting working directory and loading cached data
setwd('C:/!zemskov/svnc_forecast/data_sources/')
load("SVNC_P_V_KOM_FACT.RData")
load("PIKES_FACT.RData")
load("VOLUMES_FACT.RData")

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_65/bin')
#options(java.parameters="-Xmx2g")

# Checking Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Create connection driver and open connection
## ORACLE:
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", 
                   classPath="C:/!zemskov/ORALIBS/ojdbc7.jar")
jdbcConnectionSVNC <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@//hx5-06.rosenergo.com:1521/svncgrey.rosenergo.com", "zemskov", "Lun2i2q")

# VERTICA:
verticaDriver <- JDBC(driverClass = "com.vertica.jdbc.Driver", 
                      classPath="C:/!zemskov/VERTICA_JDBC/vertica-jdbc-7.2.3-0.jar")
verticaConnection <- dbConnect(verticaDriver, 
                               "jdbc:vertica://vertica-red-srv.rosenergo.com:5433/DWH", 
                               "zemskov", "Lun2i2q")

# Testing connection with simple query
#current_date <- dbGetQuery(jdbcConnectionSVNC, "SELECT current_date FROM dual")

# Setting up some basic date variables to use queries later
history_begin_date <- "2011-01-01"
begin_date <- as.Date(cut(max(svnc_p_v_kom$TDATE) + 25, "month"))
end_date <- Sys.Date()
forecast_date <- as.Date(cut(end_date + 25, "month"))
begin_year <- cut(max(svnc_p_v_kom$TDATE) + 25, "year") # for KOM
end_year <- cut(forecast_date, "year")                  # for KOM

# deleting evaluated and 'not full month data'
retrieved_volumes <- retrieved_volumes[retrieved_volumes$TDATE 
                                       < as.Date(begin_date), ]
pikes <- pikes[pikes$TDATE < as.Date(begin_date), ]

svnc_p_v_kom <- svnc_p_v_kom[svnc_p_v_kom$TDATE < as.Date(begin_date), ]

# extract DGPC_GP-info
extractDPGGP <- function(connection, date) {
  message("extracting DPG_GP info ...")
  message("starting query ...")
  message("for ", begin_date)
  query <- 
    paste("
          select
            t1.pcode,
            t1.pname,
            t1.tcode,
            t1.pz,
            t1.region_code,
            t2.region_name
          from
            (select
    		      p.trader_code as pcode,
              p.short_name as pname,
              t.trader_code as tcode,
              t.price_zone_code as pz,
              t.region_code 
            from
              crmdb.trader t,
              crmdb.trader p
            where
              t.parent_object_id = p.trader_id
              and to_date('",date,"','yyyy.mm.dd') between t.begin_date and t.end_date 
              and t.trader_type = 100
              and nvl(t.is_fsk,0) = 0
              and t.is_guarantee_supply_co = 1
              and t.is_unpriced_zone ^= 1
              and nvl(t.price_zone_code, 0) in (1,2)
              and (t.is_spot_trader = 1
                or t.is_forem_trader = 1)
            order by
              pcode, region_code) t1
          left join
            (select
              region_code,
              region_name
            from
              crmdb.region) t2
            on
              t1.region_code = t2.region_code
            order by
              pcode,
              region_code
          ")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}
jdbcConnectionRIO <- dbConnect(jdbcDriver, 
    "jdbc:oracle:thin:@//x240-11.rosenergo.com:1521/frs9i2.rosenergo.com", 
    "zemskov", "Lun2i2q")
dpg_gp_info <- extractDPGGP(jdbcConnectionRIO, forecast_date)
save(file = 'C:/!zemskov/svnc_forecast/data_sources/DPG_GP_INFO.RData', dpg_gp_info)
load(file = 'C:/!zemskov/svnc_forecast/data_sources/DPG_GP_INFO.RData')
gp_region <- unique(select(dpg_gp_info, PCODE, REGION_CODE, REGION_NAME))
dbDisconnect(jdbcConnectionRIO)

# extract data with fact prices by dpg
extractSVNC_FACT <- function(connection, begin_date, end_date, ...) {
  message("extracting SVNC ...")
  message("starting query ...")
  message("from ", begin_date, " to ", end_date)
  query <- 
    paste("
          select 
            t1.*, 
            reg.region_name
          from(
          with table_period as
          (
            select s.begin_date, max(s.session_id) as session_id, s.final_calculation_flag
            from 
          (
            select 
              begin_date, 
              max(final_calculation_flag) as final_calculation_flag
            from 
              rep_nov.frs_pr_session
            where
              begin_date between to_date('",begin_date,"', 'yyyy-mm-dd') 
              and to_date('",end_date,"','yyyy-mm-dd')
            group by 
              begin_date
          ) ts,
            rep_nov.frs_pr_session s
          where 
            ts.begin_date = s.begin_date 
            and ts.final_calculation_flag = s.final_calculation_flag
          group by
            s.begin_date, s.final_calculation_flag
          order by
            begin_date
          )
          select
            s.session_id
            ,s.final_calculation_flag
            ,s.begin_date as tdate
            ,p.full_name as pname
            ,p.trader_code as pcode
            ,t.trader_code as tcode
            ,t.region_code as region_code
            ,substr(t.zsp_links, 1, 8) as zsp_code
            ,d.p_nc_unreg_avg * 1000	as P_nc_unreg_avg
            ,d.p_vc_unreg_avg * 1000	as P_vc_unreg_avg
          from
            table_period tp
            join rep_nov.frs_pr_session s 
              on TP.session_id = s.session_id and s.begin_date = tp.begin_date
            join rep_nov.app_session_list c 
              on TP.session_id = c.session_id
            join rep_nov.session_gtp_dim d 
              on TP.session_id = d.session_id
            left join rep_nov.session_gtp_dim sgd_1 
              on c.prev_session_id = sgd_1.session_id 
              and d.gtp_id = sgd_1.gtp_id,				  
            rep_nov.trader t,
            rep_nov.trader p
          where
            t.real_trader_id = d.gtp_id
            and t.parent_object_id = p.real_trader_id
            and t.trader_type = 100
            and s.begin_date between t.begin_date and t.end_date
            and s.begin_date between p.begin_date and p.end_date
            and s.final_calculation_flag = 1
          order by
            tdate, tcode ,final_calculation_flag
          ) t1
          left join 
            rep_nov.crm_region reg
          on 
            t1.region_code = reg.region_code")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}
# retrieving data from the database
retrieved_svnc_fact <- extractSVNC_FACT(jdbcConnectionSVNC, begin_date, end_date)
retrieved_svnc_fact <- retrieved_svnc_fact[, c("TDATE", "PNAME", "PCODE", "TCODE",
                        "REGION_CODE", "REGION_NAME", "ZSP_CODE", "P_VC_UNREG_AVG", "P_NC_UNREG_AVG")]
retrieved_svnc_fact$TDATE <- as.Date(retrieved_svnc_fact$TDATE)
#last_fact_date <- as.character(cut(as.Date(cut(Sys.Date(), "month")) - 1, "month"))
#DPG_list <- unique(filterActuals(retrieved_svnc_fact, "TDATE", "TCODE", last_fact_date)$TCODE)
#retrieved_svnc_fact <- retrieved_svnc_fact[which(retrieved_svnc_fact$TCODE %in% DPG_list), ]
# SAVE FACT DATA
write.csv(retrieved_svnc_fact, "C:/!zemskov/svnc_forecast/data_sources/fact_dec_nee_2016.csv")

# saving accumulated data
#save(retrieved_svnc_fact, file = "SVNC_FACT_N_EE.RData")
dbDisconnect(jdbcConnectionSVNC)

#### LOADING PIKES AND VOLUMES ####

# PIKES

# creating connection to MFORMRED
jdbcConnectionMFORM <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//lp-a6o11-42.rosenergo.com:1521/mformred.rosenergo.com", "zemskov", "Lun2i2q")
jdbcConnectionMINIWH <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@//lp-a6o11-161.rosenergo.com:1521/miniwh.rosenergo.com", "zemskov", "Lun2i2q")

# loading fact pikes values
extractPikesFact <- function(connection, begin_date, end_date, ...) {
  message("extracting pikes ...")
  message("starting query ...")
  message("from ", begin_date, " to ", end_date)
  query <- 
    paste("
            select 
              m.target_month as TDATE, 
              t.trader_code as TCODE,  
              t.price_zone_code as PZ, 
              m.p_fact as PIKE_FACT 
            from 
              mform.MFORM_gtp_m_arch m,
              mform.trader t
            where 
              m.end_ver = 999999999999999 and 
              m.calc_type = 2 
              and m.target_month between to_date('",begin_date,"','yyyy-mm-dd') 
                and to_date('",end_date,"','yyyy-mm-dd')
              and m.target_month between t.begin_date and t.end_date
              and m.gtp_id = t.real_trader_id
              and t.dpg_type = 1
            order by 
              m.target_month,  
              t.trader_code")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}

# evaluating pike values for the current month
evaluatePikes <-  function(connection, current_month, ...) {
  message("evaluating pikes ...")
  message("starting query ...")
  message("Evaluating pikes for the current month... ")
  query <- 
    paste("
          with dates_table as
				  (
            select
              trunc(TO_DATE ('",end_date,"', 'yyyy-mm-dd'),'month') as Begin_Date,
              to_date ('",end_date,"', 'yyyy-mm-dd') as End_Date
            from
              dual
          ),
          pikes as 
          (
          select
            ts.target_date  as tdate
            ,d.hour as hour
            ,t.trader_code as tcode
            ,t.price_zone_code as pz
            ,t.region_code as rcode
            ,d.volume / 1000 as pike
            ,sum(nvl(d.volume,0)/1000) over(partition by ts.target_date,
              t.region_code, d.hour) as region_pike
          from
            dates_table dt, 
            tst_wh.trade_session ts,
            tst_wh.wh_deal_data_hour d,
            tst_wh.wh_trader t
          where
          ts.target_date between dt.begin_Date and dt.End_Date
          and tst_wh.is_workday(ts.target_date) not in (0,2)
          and ts.trade_session_id = d.trade_session_id
          and ts.trade_session_id = t.trade_session_id
          and d.dpg_code = t.trader_code
          and d.deal_type = 3
          and d.direction = 1
          and d.volume > 0
          and nvl(t.is_fsk,0) = 0
          and nvl(t.is_unpriced_zone,0) = 0
          and nvl(t.is_gaes,0) = 0
          and (t.price_zone_code = 1 and d.hour between 7 and 22 
                or t.price_zone_code=2 and d.hour between 3 and 19)
          ),
          region_pikes as
          (
            select
              tdate,
              max(hour) as hour,
              rcode,
              region_pike
            from
            (
              select
              tdate,
              hour,
              rcode,
              region_pike,
              max(region_pike) over (partition by rcode, tdate) as max_region_pike
              from
                pikes  
              group by
                tdate, 
                hour, 
                rcode, 
                region_pike 
            )
            where
              region_pike = max_region_pike 
            group by
              tdate, 
              rcode, 
              region_pike
            order by
              rcode, 
              tdate, 
              hour
            )
          select
            trunc(tdate, 'month') as tdate,
            tcode as tcode,
            pz,
            round(avg(pike),3) as PIKE_FACT
          from(
            select
            t.tdate,
            t.tcode,
            t.pz,
            t.rcode as ,
            t.pike,
            t.pike / p.region_pike as k_R_i_t
          from
            pikes t,
            region_pikes p
          where
            t.tdate = p.tdate
            and t.rcode = p.rcode
            and t.hour = p.hour
            and substr(t.tcode,1,1) <> 'F'
          ) 
          group by
            trunc(tdate,'month'), 
            tcode,
            pz,
            rcode
          order by
            tcode, 
            tdate")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}

# executing queries
retrieved_pikes <- extractPikesFact(jdbcConnectionMFORM, begin_date, end_date)
retrieved_pikes$TDATE <- as.Date(retrieved_pikes$TDATE)
evaluated_pikes <- evaluatePikes(jdbcConnectionMINIWH, end_date)
evaluated_pikes$TDATE <- as.Date(evaluated_pikes$TDATE)

# Union of the fact and evaluated pikes
pikes_new <- rbind(retrieved_pikes, evaluated_pikes)
pikes <- rbind(pikes, pikes_new)

# saving accumulated data
save(pikes, file = "PIKES_FACT.RData")

#rm(evaluated_pikes)
#rm(retrieved_pikes)

#joining pikes to svnc
svnc_with_pikes <- merge(retrieved_svnc_fact, retrieved_pikes, 
                         by = c("TCODE", "TDATE"), all.x = TRUE, sort = TRUE)
rm(retrieved_pikes)
#save(svnc_with_pikes, file = "DATA_FOR_NN.RData")

# VOLUMES
extractVolumesFact <- function(connection, begin_date, end_date, ...) {
  message("extracting volumes ...")
  message("starting query ...")
  message("from ", begin_date, " to ", end_date)
  query <- 
    paste("
          with TRADE_SESSIONS as
        		(select 
              trade_session_id 
            from 
              ODS_002.trade_session
            where 
              target_date between to_date('",begin_date,"','yyyy-mm-dd')
              and to_date('",end_date,"','yyyy-mm-dd')
              and valid_to_dttm > sysdate()
            )
          select   
            trunc(ts.target_date, 'month')  as TDATE
            ,t.trader_code as TCODE
            ,nvl(sum(d.volume)/1000, 0) as VOLUME
          from 
            ODS_002.trade_session ts,
            ODS_002.wh_deal_data_hour d,
            ODS_002.wh_trader t                                                
          where
            ts.trade_session_id = d.trade_session_id
            and ts.trade_session_id = t.trade_session_id
            and d.dpg_code = t.trader_code
            and d.deal_type = 3
            and d.direction = 1
            and d.volume > 0
            and (nvl(t.fed_station::NUMERIC,0) = 0 
              or t.is_guarantee_supply_co = 1)
            and ts.target_date between to_date('",begin_date,"','yyyy-mm-dd')
            and to_date('",end_date,"','yyyy-mm-dd') 
            and nvl(t.is_fsk::NUMERIC,0) = 0
            and nvl(t.is_unpriced_zone::NUMERIC,0) = 0
            and nvl(t.region_code::NUMERIC, 0) > 0
            and nvl(t.oes::NUMERIC,0) > 0
            and ts.valid_to_dttm > sysdate()
				    and d.valid_to_dttm > sysdate()
            and t.valid_to_dttm > sysdate()
            and ts.trade_session_id in (select * from TRADE_SESSIONS)
            and d.trade_session_id in (select * from TRADE_SESSIONS)
            and t.trade_session_id in (select * from TRADE_SESSIONS)
          group by
             t.price_zone_code
            ,t.trader_code
            ,t.region_code
            ,t.is_guarantee_supply_co
            ,trunc(ts.target_date , 'month')
          order by
            tcode
            ,region_code
            ,tdate")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}

# executing query
retrieved_volumes_new <- extractVolumesFact(verticaConnection, begin_date, end_date)
retrieved_volumes_new$TDATE <- as.Date(retrieved_volumes_new$TDATE)

# Union of the newly retrieved and historical data
retrieved_volumes <- rbind(retrieved_volumes, retrieved_volumes_new) 
retrieved_volumes <- retrieved_volumes[order(retrieved_volumes$TCODE, retrieved_volumes$TDATE), ]

# saving accumulated data
#retrieved_volumes$TDATE <- as.Date(retrieved_volumes$TDATE)
save(retrieved_volumes, file = "VOLUMES_FACT.RData")
svnc_pikes_volumes <- merge(svnc_with_pikes, retrieved_volumes_new, 
                            by = c("TDATE", "TCODE"), all.x = TRUE, sort = TRUE)
rm(svnc_with_pikes)

#rm(retrieved_volumes)
#save(svnc_pikes_volumes, file = "svnc_pikes_volumes.RData")

#### KOM Prices by price zones (after 01.01.2016) ####
extractKOMPricesPZ <- function(connection, begin_year, end_year, ...) {
  message("extracting KOM prices ...")
  message("starting query ...")
  message("from ", begin_year, " to ", end_year)
  query <- 
    paste("
          select 
            target_date as TDATE,
            price_zone_code as PZ,
            price_kom
          from 
  				  mform.result_com_price_zone 
          where
            target_date between to_date('",begin_year,"','yyyy-mm-dd') 
            and to_date('",end_year,"','yyyy-mm-dd')
            and end_ver = 999999999999999
          ")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}
retrieved_KOM_PZ <- extractKOMPricesPZ(jdbcConnectionMFORM, begin_year, end_year)
retrieved_KOM_PZ$TDATE <- as.Date(retrieved_KOM_PZ$TDATE)
retrieved_KOM_PZ$YEAR <- format(retrieved_KOM_PZ$TDATE, "%Y")
retrieved_KOM_PZ <- retrieved_KOM_PZ[, -1]
save(retrieved_KOM_PZ, file = "KOM_PZ_PRICES.RData")

#### KOM Prices by ZSP (before 01.01.2016) ####
extractKOMPricesZSP <- function(connection, begin_date, end_date, ...) {
  message("starting query ...")
  message("from ", begin_date, " to ", end_date)
  query <- 
    paste("
          select 
            zsp_code,
            target_date as tdate,
            price_kom as zsp_price_kom
          from 
            mform.kom_result_zsp_dfr
          where
            end_ver = 999999999999999
            and target_date between to_date('",begin_date,"', 'yyyy-mm-dd') and  
            to_date('",end_date,"', 'yyyy-mm-dd')
            order by
            zsp_code, tdate
          ")
  cursor <- dbGetQuery(connection, query)
  message("query was executed successfuly")
  return (cursor)
}
retrieved_KOM_ZSP <- extractKOMPricesZSP(jdbcConnectionMFORM, history_begin_date, forecast_date)
retrieved_KOM_ZSP$TDATE <- as.Date(retrieved_KOM_ZSP$TDATE)
retrieved_KOM_ZSP$YEAR <- format(retrieved_KOM_ZSP$TDATE, "%Y")
retrieved_KOM_ZSP <- retrieved_KOM_ZSP[, -2]

#### Joining KOM Prices ####
svnc_pikes_volumes$YEAR <- format(svnc_pikes_volumes$TDATE, "%Y")
svnc_p_v_kom_new <- merge(svnc_pikes_volumes, retrieved_KOM_PZ, 
                      by = c("YEAR", "PZ"), all.x = TRUE, sort = FALSE)
svnc_p_v_kom_new <- merge(svnc_p_v_kom_new, retrieved_KOM_ZSP, by = c("YEAR", "ZSP_CODE"), 
                      all.x = TRUE, sort = FALSE)
svnc_p_v_kom_new$KOM_PRICE <- ifelse(is.na(svnc_p_v_kom_new$PRICE_KOM), svnc_p_v_kom_new$ZSP_PRICE_KOM, 
                       svnc_p_v_kom_new$PRICE_KOM)
svnc_p_v_kom_new <- svnc_p_v_kom_new[, -c(14, 15)]
svnc_p_v_kom <- rbind(svnc_p_v_kom, svnc_p_v_kom_new)
svnc_p_v_kom <- svnc_p_v_kom[order(svnc_p_v_kom$TDATE, svnc_p_v_kom$TCODE), ]
save(svnc_p_v_kom, file = "C:/!zemskov/svnc_forecast/data_sources/SVNC_P_V_KOM_FACT.RData")

#### Consume Price Index (CPI) ####
library(xlsx)
#CPI <- read.xlsx("CPI.xlsx", sheetName = "CPI", header = TRUE)
dataUrl <- paste("http://quote.rbc.ru/cgi-bin/macro/ind_export.cgi?bdate=",
                 form_begin,"&edate=",form_end,"&ind_id=174&c_id=1", sep = "")
cpi_data <- read.csv(dataUrl, sep = ";")




















#### TRY {AUTO-LOADER WITH AUTO-CLICKING} ####


# reading CPI data from quotes.rbc.ru
require(RCurl)
require(rvest)
require(httr)

# Proxy was set up with CNTLMlocalhost proxy
# CNTLM config file is in C:\Program Files\CNTLM directory
# commands are: "net start(stop) cntlm"
# Port, used to listen to - 8080
#opts <- list(
#  proxy         = "http://localhost", 
#  proxyusername = "i.zemskov", 
#  proxypassword = "GrandPik9", 
#  proxyport     = 8080
#)
#cpi_url <- getURL("http://quote.rbc.ru/macro/indicator/1/174.shtml", .opts = opts)
#cpi_table <- readHTMLTable(cpi_url, as.data.frame = TRUE)[[5]]

# Function, that submits dates to the html-form and clicks the export 
# setting the proxy configuration
set_config(use_proxy(url="http://localhost", 8080, "i.zemskov", "GrandPik9"))
# creating simulation of the session in an html browser
session <- html_session("http://quote.rbc.ru/macro/indicator/1/174.shtml")

# form with the date-picker
date_form <- html_form(session)[[3]]  

# dates for the date-picker control
form_begin <- "01.01.2011"            # maybe automate later...
form_end <- "01.01.2017"              # maybe automate later...

# setting input values to the form
# bdate and edate - names from the html code of the URL
date_form <- set_values(date_form, bdate = form_begin, edate = form_end)

# Appends element of a list to another without changing variable type of x
# build_url function uses the httr package and requires a variable of the url class
appendList <- function (x, val)
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}

# Simulating submit_form for GET requests
submit_geturl <- function (session, form)
{
  query <- rvest:::submit_request(form)
  query$method <- NULL
  query$encode <- NULL
  query$url <- NULL
  names(query) <- "query"
  
  relativeurl <- XML::getRelativeURL(form$url, session$url)
  basepath <- parse_url(relativeurl)
  
  fullpath <- appendList(basepath,query)
  fullpath <- build_url(fullpath)
  fullpath
}

# Submit form and get new url
session1 <- submit_geturl(session, date_form)



cpi_table<-read_html(session1, encoding = "windows-1251")%>%
  html_table(fill=TRUE)%>%
  .[[11]] # number of the table node in html document on quotes.rbc.ru

# Get the data
session2 <- follow_link(session1, css = "#more_9 li:nth-child(3) a")
reviews <- session2 %>% html_nodes(".description") %>% html_text()
reviews


load_session <- html_session(dataUrl)

