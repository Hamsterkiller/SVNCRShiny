library(rsconnect)

#Sys.setenv(http_proxy = "http://i.zemskov:GrandPik9@localhost:8080/")

library(RCurl)

options(RCurlOptions = list(proxy = "https://i.zemskov:GrandPik9@localhost:8080", 
                            ssl.verifypeer = FALSE))

setAccountInfo(name = "123a", token = "E6A47DF5869B6618EF9691E36B33F5F0", 
                          secret = "TRkD7LNpmgL9MvTbwcreHC3IXl11NlSw0elEbibS")

deployApp(appDir = "C:/!zemskov/svnc_forecast/shiny_app", appName = "svnc", 
                    account = "123a")
