require(HockeyModel)
HockeyModel::dailySummary()
require(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email
outMail = OutApp$CreateItem(0)
## configure  email parameter
outMail[["To"]] = "bulsinkp@gmail.com"
outMail[["subject"]] = "Daily Update Complete"
outMail[["body"]] = paste("Daily update of HockeyModel complete at", Sys.time(), ".")
## send it
outMail$Send()
