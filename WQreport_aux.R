#module load emacs/25.1.1 R/3.4.1 gdal/2.0.2 netcdf/4.3.3.1-v4 gcc/4.9.0 geos/3.5.0 hdf5/1.8.16 jags/3.4.0 xtide/2.15.1 texlive

library(dplyr)
library(tidyr)
library(xtable)
library(sp)
library(rgeos)
library(ggplot2)
library(gridExtra)
require(foreach)
require(doParallel)
#library(Cairo)

registerDoParallel(cores=10)

spatial = read.csv('../parameters/spatial.csv', strip.white=TRUE) %>%
    dplyr:::select(GBRMPA_Zone,Zone,Region,WaterBody)
measures = read.table('../parameters/measures.txt', strip.white=TRUE, sep=';', header=TRUE)



######################
## Table of sources ##
######################
glossery = read.table('../parameters/glossery.csv',  header=TRUE,sep='\t')
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(glossery$Term),glossery$Term) -1)[-1]
addtorow$pos[[3]] = nrow(glossery)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Glossery of important terms used throughout the report.}\\label{tab:glossery}\n",
    #"\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} p{60em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Term}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Description}}} & \n",
    "\\cmidrule{1-2} \n"
    ),
    "\\cline{1-2}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        #"\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
glossery %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) gsub('\\_','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/glossery.tex')


##############################################################
## Generate a table that lists the Regions and water bodies ##
##############################################################
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(spatial$WaterBody),spatial$WaterBody) -1)[-1]
addtorow$pos[[3]] = nrow(spatial)

addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "%\\begin{landscape}\n",
    "\\begin{table}[h]\\caption{Great Barrier Reef spatial Zones and associated Regions and Water bodies.}\\label{tab:spatial}\n",
    #"\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{25em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Spatial Reporting Zone}}} & \n",
    "%\\multicolumn{1}{l}{\\whiteHeader{{GBRMPA Zone}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Zone}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Region}}} & \n",
    "\\whiteHeader{{Water body}}\\\\ \n",
    "\\cmidrule{1-4} \n"
    ),
    "\\cline{1-4}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        #"\\end{center}\n",
        "\\end{table}\n",
        "%\\end{landscape}\n")
    )

writeLines(text=
spatial %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) gsub('\\_','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/spatial.tex')


######################
## Table of sources ##
######################
sources = read.csv('../parameters/sources.csv', strip.white=TRUE)
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(sources$Source),sources$Source) -1)[-1]
addtorow$pos[[3]] = nrow(sources)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Overview of used data sources.}\\label{tab:sources}\n",
    #"\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Source}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Custodian}}} & \n",
    "\\whiteHeader{{Description}}\\\\ \n",
    "\\cmidrule{1-3} \n"
    ),
    "\\cline{1-3}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        #"\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
sources %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) gsub('\\_','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/sources.tex')


##################################
## Generate a table of Measures ##
##################################
measures = read.table('../parameters/measures.txt', header=TRUE,strip.white=TRUE, sep=';') %>%
  mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
         UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
measures = measures %>% dplyr:::select(Indicator,Subindicator,Measure,Label,UnitsLabel) %>%
      mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
         UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(measures$Measure),measures$Measure) -1)[-1]
addtorow$pos[[3]] = (match(unique(measures$Subindicator),measures$Subindicator) -1)[-1]
addtorow$pos[[4]] = (match(unique(measures$Indicator),measures$Indicator) -1)
addtorow$pos[[5]] = nrow(measures)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Example of Water Quality Measure hierarchy specifying which Measures contribute to which Subindicators and which Subindicators contribute to which Indicators.}\\label{tab:measures}\n",
    #"\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    #"\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[0pt]} p{6em}}{}&{\\whiteHeader{{Indicator}}} & \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Indicator}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Subindicator}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Measure}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Label}}} & \n",
    "\\whiteHeader{{Units}}\\\\ \n",
    "\\cmidrule{1-5} \n"
    ),
    "\\cline{3-5}",
    "\\cline{2-5}",
    "\\cline{1-5}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        #"\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
measures %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow, only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) x, comment=FALSE, hline.after=-1),
con='tables/measures.tex')


insitu.measures = data.frame(Measure=c('Chlorophyll-a', 'Total Suspended Solids', 'Secchi Depth', 'NOx'),
                             AIMS_NAME = c('DRIFTCHL_UGPERL.wm', 'TSS_MGPERL.wm', 'SECCHI_DEPTH.wm', 'NOX.wm'),
                             Description=c('Chlorophyll-a (µg/L)',
                                           'Suspended solids (mg/L)',
                                           'Secchi depth (m)',
                                           'Nitrite and Nitrate measured by microanalyser (µM/L)'
                                           ),
                             Abbreviation=c('chl','nap','sd','NOx'),
                             Conversion = c('x1','x1','x1','x14'),
                             UnitsLabel = c('Chla (µgL⁻¹)','TSS (mgL⁻¹)','m','NOx (µgL⁻¹)')) %>%
    mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
           UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(insitu.measures$Measure),insitu.measures$Measure) -1)[-1]
addtorow$pos[[3]] = nrow(insitu.measures)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}\\caption{Measures collected in AIMS MMP insitu inshore water quality monitoring program.  NOx is the sum of NO$_2$ and NO$_3$.  Data used are annual means of depth weighted averages per site.}\\label{tab:insitu.measures}\n",
    "%\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{10em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} p{15em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Measure}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Variable}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Description}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Abbreviation}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Conversion}}} & \n",
    "\\whiteHeader{{Units}}\\\\ \n",
    "\\cmidrule{1-6} \n"
    ),
    "\\cline{1-6}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "%\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
insitu.measures %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) gsub('\\_','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/insitu.measures.tex')

## FLNTU logger measures table
flntu.measures = data.frame(Measure=c('Chlorophyll-a', 'NTU'),
                            AIMS_NAME = c('CHL_QA_AVG', 'NTU_QA_AVG'),
                            Description=c('Daily mean chlorophyll fluorescence',
                                           'Daily mean turbidity'
                                           ),
                            Abbreviation=c('chl','ntu'),
                            Conversion = c('CHL_QA_AVG x1','NTU_QA_AVG x1'),
                             UnitsLabel = c('Chla (µgL⁻¹)','NTU')) %>%
    mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
           UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(flntu.measures$Measure),flntu.measures$Measure) -1)[-1]
addtorow$pos[[3]] = nrow(flntu.measures)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Measures collected in AIMS MMP flntu inshore water quality monitoring program. Data used are daily means per site.}\\label{tab:flntu.measures}\n",
    "%\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{10em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} p{15em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Measure}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Variable}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Description}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Abbreviation}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Conversion}}} & \n",
    "\\whiteHeader{{Units}}\\\\ \n",
    "\\cmidrule{1-6} \n"
    ),
    "\\cline{1-6}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "%\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
flntu.measures %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) gsub('\\_','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/flntu.measures.tex')


## SATELLITE logger measures table
satellite.measures = data.frame(Measure=c('Chlorophyll-a', 'Non-Algal Particles','Secchi Depth'),
                                AIMS_NAME = c('Chl_{MIM}', 'Nap_{MIM}','SD_{MIM}'),
                                Description=c('Near surface concentration based on empirical relationship established between in situ measurements and blue-to-green band ratios',
                                           'Total suspended solids based on relationship established between in situ measurements and the absorption concentration of non-algal particles',
                                           'Secchi depth based on empirical relationship established between in situ measurements and estimated depth at which 10\\% of surface light still available'
                                           ),
                                Abbreviation=c('chl','nap','sd'),
                                Conversion = c('Chl_{MIM} x1','Nap_{MIM} x1','SD_{MIM} x1'),
                             UnitsLabel = c('Chla (µgL⁻¹)','TSS (mgL⁻¹)','m')) %>%
    mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
           UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(satellite.measures$Measure),satellite.measures$Measure) -1)[-1]
addtorow$pos[[3]] = nrow(satellite.measures)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Measures collected from MODIS satellite imaging. Data used are daily means per pixel. Variable and Description pertain to the eReefs source.  Conversion indicates the conversion applied on data to conform to threshold Units.  Abbreviation provides a consistent key accross data. MIM refers to the robust and scalable matrix inversion method used to handle the variability in optical properties of satellite imagery.}\\label{tab:satellite.measures}\n",
    "%\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{10em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} p{20em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Measure}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Variable}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Description}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Abbreviation}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Conversion}}} & \n",
    "\\whiteHeader{{Units}}\\\\ \n",
    "\\cmidrule{1-6} \n"
    ),
    "\\cline{1-6}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "%\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
satellite.measures %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,
                 sanitize.text.function=function(x) gsub('\\_[^0-9]','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/satellite.measures.tex')


## EREEFS logger measures table
ereefs.measures = data.frame(Measure=c('Chlorophyll-a', 'Non-Algal Particles','Secchi Depth','NOx'),
                             AIMS_NAME = c('Chl_{a}_sum', 'EFI','Kd_{490}','NO3'),
                             Description=c('Sum of Chlorophyll concentration of four microalgae types ($mg/m^3$)',
                                           'EFI = NAP and is the sum of Mud and Fine Sediment',
                                           'Kd\\_490 is calculated from the scattering and absorbing properties of all optical-active constituents, and includes the cosine zenith angle on vertical attenuation.',
                                           'Concentration of Nitrate. As Nitrite is not represented in the model, NO3 = $[NO^-_3] + [NO^-_2]$ ($mg/m^3$)'
                                           ),
                             Abbreviation=c('chl','nap','sd','NOx'),
                             Conversion = c('Chl_{a}_sum x1','EFI x1000','1/Kd_{490}','NO3 x1'),
                             UnitsLabel = c('Chla (µgL⁻¹)','TSS (mgL⁻¹)','m','NOx (µgL⁻¹)')) %>%
    mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
           UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = (match(unique(ereefs.measures$Measure),ereefs.measures$Measure) -1)[-1]
addtorow$pos[[3]] = nrow(ereefs.measures)
addtorow$command = c(paste(
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
    "\\LTcapwidth=\\linewidth\n",
    "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
    "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
    "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
    "\\begin{table}[h]\\caption{Measures collected from eReefs assimilated model. Data used are daily means per pixel. Variable and Description pertain to the eReefs source.  Conversion indicates the conversion applied on data to conform to threshold Units.  Abbreviation provides a consistent key accross data. }\\label{tab:ereefs.measures}\n",
    "%\\begin{center}\n",
    "\\scriptsize\n",
    "\\begin{tabular}{\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{10em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} p{20em}\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
    "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
    "}\n",
    "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} %top border\n",
    "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
    "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}l}{\\whiteHeader{{Measure}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Variable}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Description}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Abbreviation}}} & \n",
    "\\multicolumn{1}{l}{\\whiteHeader{{Conversion}}} & \n",
    "\\whiteHeader{{Units}}\\\\ \n",
    "\\cmidrule{1-6} \n"
    ),
    "\\cline{1-6}",
    paste(
        "\\bottomrule\n",
        "\\end{tabular}\n",
        "%\\end{center}\n",
        "\\end{table}")
    )
writeLines(text=
ereefs.measures %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow,only.contents=TRUE,include.colnames=FALSE,
                 sanitize.text.function=function(x) gsub('\\_[^0-9]','\\\\_',x), comment=FALSE, hline.after=-1),
con='tables/ereefs.measures.tex')










##########################################################
## Generate a map illustrating Regions and Water bodies ##
##########################################################
load(file='data/Polys.RData')
load(file='data/Polys.df.RData')
load(file='data/gbr_boundary.RData')
load(file='data/waterbodies.RData')
load(file='data/layers.RData')
qld.df = broom:::tidy(qld)
bbox = bbox(Polys)

plot.new()
centroids = as.data.frame(rgeos:::polygonsLabel(Polys,names(Polys), method='inpolygon', doPlot=FALSE))
colnames(centroids) = c('x','y')
centroids = centroids %>% mutate(GBRMPA_Zone=names(Polys)) %>% left_join(spatial) %>%
    mutate(Zone=paste0(WaterBody))
sitelabels = centroids %>% arrange(desc(y)) %>% group_by(Region) %>% mutate(cY=mean(y)) %>%
    mutate(yy=cY+seq(0.5,-0.5,len=4)) %>% ungroup %>%
    mutate(cX = ifelse(Region=='Cape York',150,
                ifelse(Region=='Wet Tropics',150,
                ifelse(Region=='Dry Tropics', 151,
                ifelse(Region=='Mackay Whitsunday', 146,
                ifelse(Region=='Fitzroy', 147,148))))),
           hjust = ifelse(Region=='Cape York',0,
                ifelse(Region=='Wet Tropics',0,
                ifelse(Region=='Dry Tropics', 0,
                ifelse(Region=='Mackay Whitsunday', 1,
                ifelse(Region=='Fitzroy', 1,1))))),
           ya = as.numeric(ifelse(Region=='Cape York',0,
                as.numeric(ifelse(Region=='Wet Tropics',0.5,
                as.numeric(ifelse(Region=='Dry Tropics', 0,
                as.numeric(ifelse(Region=='Mackay Whitsunday', 0.75,
                                  as.numeric(ifelse(Region=='Fitzroy', 0.75,-0.1))))))))))) %>%
    mutate(yy=yy+ya)


regionlabels = sitelabels %>% group_by(Region) %>% summarize(x=max(cX), y=max(yy)+0.5, hjust=unique(hjust))
g=ggplot(Polys.df, aes(y=lat, x=long)) +
    geom_polygon(data=qld.df, aes(y=lat, x=long,group=group), color='gray60',fill='gray90') +
    geom_polygon(aes(group=group), fill=NA,color='black')+
    geom_point(data=centroids, aes(y=y, x=x), pch=21,fill='#0000ff50', color='black', size=3) +
    geom_text(data=regionlabels, aes(y=y, x=x, label=Region, hjust=hjust), fontface='bold', size=6) +
    geom_text(data=sitelabels, aes(y=yy, x=cX, label=Zone,hjust=hjust), size=6)+
    geom_segment(data=sitelabels, aes(y=y, yend=yy, x=x, xend=cX), color='#0000ff50')+
    ggthemes:::theme_map() + theme(legend.position=c(0.02,0.5), panel.border=element_rect(fill=NA,color='black'))+
    coord_equal(ylim=bbox[2,],xlim=bbox[1,])

#ggsave('figures/Maps/Map_region_waterbody.pdf', g, width=10, height=10, units='in', device=cairo_pdf)
ggsave('figures/Maps/Map_region_waterbody.pdf', g, width=10, height=10, units='in')
ggsave('figures/Maps/Map_region_waterbody.png', g, width=10, height=10, units='in', dpi=300)

##############################
## Samples spatial temporal ##
##############################
data.heat = data %>% group_by(Region,WaterBody,GBRMPA_SITE_NAME, waterYear) %>%
    summarise(Latitude=mean(LATITUDE,na.rm=TRUE),Longitude=mean(LONGITUDE,na.rm=TRUE), Freq=length(unique(Date))) %>%
    ungroup %>% arrange(desc(Latitude)) %>%
    mutate(Region=factor(Region, levels=unique(Region)),GBRMPA_SITE_NAME=factor(GBRMPA_SITE_NAME, levels=rev(unique(GBRMPA_SITE_NAME)))) 
g=ggplot(data.heat, aes(y=GBRMPA_SITE_NAME, x=as.factor(waterYear), fill=as.factor(Freq))) +
    geom_tile() +
    scale_fill_brewer('Surveys',type='seq', palette=1)+
        scale_y_discrete('Site name')+
            scale_x_discrete('')+
                    facet_grid(Region~., scales='free',space='free')+
            theme_classic()+
        theme(axis.text.y=element_text(size=10),
              axis.ticks.y=element_blank(),
              panel.background=element_rect(fill=NA,color='black'),
              strip.background=element_blank(),
              strip.text=element_text(size=12))
ggsave('figures/Maps/Insitu_sites/Samples_spatial_temporal.pdf', g, width=10, height=10, units='in')  


#######################
## Niskin insitu map ##
#######################
load(file=paste0('../parameters/niskin.aims.reef.av.RData'))
niskin.all.reef = niskin.aims.reef.av %>%
    select(Date,LATITUDE,LONGITUDE,DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,NOx.wm,
           reef.alias,GBRMPA_group,GBRMPA_SITE_NAME,Water_Samples,
           GBRMPA_water_area,Region,Reg,Subregion,Subreg,Season,oldSamplingYear,waterYear,cwaterYear,financialYear,cfinancialYear) %>%
               mutate(Source = 'AIMS Niskin') %>%
                   gather(key=Measure, value=Value,-everything(),DRIFTCHL_UGPERL.wm,TSS_MGPERL.wm,SECCHI_DEPTH.wm,NOx.wm)

data = niskin.all.reef %>% dplyr:::filter(Measure %in% c("DRIFTCHL_UGPERL.wm","TSS_MGPERL.wm","SECCHI_DEPTH.wm","NOx.wm")) %>%
    dplyr:::select(Date,LATITUDE,LONGITUDE,reef.alias,GBRMPA_SITE_NAME,Subregion,Season,waterYear,Source,Measure,Value) %>%
    #left_join(wq.guidelines %>% dplyr:::filter(GL.Season=='Annual') %>%
    #dplyr:::select(GBRMPA_group,Measure,GL,DirectionOfFailure) %>% distinct()) %>%
    arrange(LATITUDE,LONGITUDE,Date) %>% mutate(Date=as.Date(Date)) %>%
        mutate(Measure=ifelse(Measure=='DRIFTCHL_UGPERL.wm','chl',ifelse(Measure=='TSS_MGPERL.wm', 'tss',ifelse(Measure=="SECCHI_DEPTH.wm",'secchi','NOx')))) %>%
                ungroup %>% arrange(desc(LATITUDE)) %>% mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias))) #%>%
## Note there was an error in Yongala 2016-02-15..
data[data$reef.alias=='Yongala' & data$Date==as.Date('2016-02-15'),'LONGITUDE'] = data[data$reef.alias=='Yongala' & data$Date==as.Date('2016-02-15'),'LONGITUDE'] + 4
data[data$reef.alias=='Bedarra' & data$Date==as.Date('2016-03-30'),'LONGITUDE'] = data[data$reef.alias=='Bedarra' & data$Date==as.Date('2016-03-30'),'LONGITUDE'] -2
data[data$reef.alias=='Dunk South' & data$Date==as.Date('2016-02-05'),'LONGITUDE'] = data[data$reef.alias=='Dunk South' & data$Date==as.Date('2016-02-05'),'LONGITUDE'] -2
# Since Fitzroy West comes up on the island (non-water), move it slightly north
data[data$reef.alias=='Fitzroy West','LATITUDE'] = max(data[data$reef.alias=='Fitzroy West','LATITUDE']) 

samples.insitu=data %>% group_by(reef.alias) %>% summarize(Lat=mean(LATITUDE,na.rm=TRUE),Long=mean(LONGITUDE,na.rm=TRUE)) %>%
    mutate(Y=seq(max(Polys.df$lat,na.rm=TRUE)-0.5,min(Polys.df$lat,na.rm=TRUE)+0.5, len=length(Lat)))

samples.insitu.sp = samples.insitu
coordinates(samples.insitu.sp) <- ~Long+Lat
wch=sp:::over(samples.insitu.sp,Polys)
samples.insitu = samples.insitu %>% mutate(GBRMPA_Zone=factor(names(Polys)[wch])) %>% left_join(spatial)

samples.insitu.sp = samples.insitu
coordinates(samples.insitu.sp) <- ~Long+Lat
wch=sp:::over(samples.insitu.sp,Polys)
samples.insitu = samples.insitu %>% mutate(GBRMPA_Zone=factor(names(Polys)[wch])) %>% left_join(spatial)
   # left_join(wq.guidelines %>% mutate(Zone=gsub(' ','_',Zone)))

## Put the spatial data into the in situ data
data = data %>% mutate(Measure=ifelse(Measure=='tss','nap',ifelse(Measure=='secchi','sd',Measure))) %>%
    left_join(samples.insitu) %>%
        ungroup %>% arrange(desc(Lat)) %>%
            mutate(reef.alias=factor(reef.alias, levels=unique(reef.alias)),Zone=factor(Zone, levels=unique(Zone)), Region=factor(Region, levels=unique(Region)), WaterBody=factor(WaterBody, levels=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore')))

qld.df = broom:::tidy(qld)
bbox = bbox(Polys)
samples.insitu=data %>% group_by(reef.alias,GBRMPA_SITE_NAME,Zone,Region,WaterBody) %>% summarize(Lat=mean(LATITUDE,na.rm=TRUE),Long=mean(LONGITUDE,na.rm=TRUE)) %>%
    ungroup %>% mutate(Y=seq(max(Polys.df$lat,na.rm=TRUE)-0.5,min(Polys.df$lat,na.rm=TRUE)+0.5, len=length(Lat))) %>%
    mutate(Site=paste0(GBRMPA_SITE_NAME,': ',reef.alias))

g=ggplot(Polys.df, aes(y=lat, x=long)) +
    geom_polygon(data=qld.df, aes(y=lat, x=long,group=group), color='gray60',fill='gray90') +
    geom_polygon(aes(group=group), fill=NA,color='black')+
    geom_point(data=samples.insitu, aes(y=Lat,x=Long),shape=16,col='red') +
    geom_segment(data=samples.insitu, aes(y=Lat,x=Long, yend=Y, xend=ifelse(Lat>-18, max(Polys.df$long)-4.5, min(Polys.df$long)+3.4), label=reef.alias), color='#ff000030') + 
geom_label(data=samples.insitu, aes(y=Y,x=ifelse(Lat>-18, max(Polys.df$long)-4.5, min(Polys.df$long)+3.4), label=Site, fill=WaterBody, hjust=ifelse(Lat> -18,0,1)),shape=16, show.legend=TRUE)+
                                        #               geom_label(data=samples.insitu, aes(y=Y,x=ifelse(Lat>-18, max(Polys.df$long)-4, min(Polys.df$long)+3), label=reef.alias, fill=Zone),shape=16)+
    scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
    ggthemes:::theme_map() + theme(legend.position=c(0.02,0.5), panel.border=element_rect(fill=NA,color='black'))+
    coord_equal(ylim=bbox[2,],xlim=bbox[1,])
ggsave('figures/Maps/Insitu_sites/Map_insitu1.pdf', g, width=10, height=10, units='in')


####################
## FLNTU temporal ##
####################
load(file='../parameters/flntu.all.RData')
flntu.ntu = flntu.all %>% dplyr:::select(-Region) %>%
    left_join(samples.insitu %>%
              dplyr:::select(GBRMPA_SITE_NAME,Zone,Region) %>% distinct) %>%
    ungroup %>%
    arrange(desc(LATITUDE)) %>%
    mutate(Region=factor(Region, levels=unique(Region))) %>%
    arrange(LATITUDE) %>%
    mutate(cSites=as.numeric(factor(GBRMPA_SITE_NAME, levels=unique(GBRMPA_SITE_NAME))),
           Sites=as.numeric(factor(GBRMPA_SITE_NAME, levels=unique(GBRMPA_SITE_NAME)))+ scales:::rescale(NTU_QA_AVG, to=c(0,0.9)))
gg1=ggplot(flntu.ntu, aes(y=Sites, x=Date, group=GBRMPA_SITE_NAME)) +
    geom_line(color='red') +
    facet_grid(Region ~., scales='free_y',space='free') +
    scale_y_continuous('GBRMPA site name',breaks=unique(flntu.ntu$cSites), labels=unique(flntu.ntu$GBRMPA_SITE_NAME)) +
    theme_bw() +
    scale_x_date(date_breaks = '1 years',date_labels = "%Y") +
    theme(strip.background=element_blank(), axis.title.x=element_blank(),
          axis.text.y=element_text(size=10),
          axis.ticks.y=element_blank(),
          panel.background=element_rect(fill=NA,color='black'),
          strip.text=element_text(size=12))

flntu.chl = flntu.all %>% dplyr:::select(-Region) %>%
    left_join(samples.insitu %>%
              dplyr:::select(GBRMPA_SITE_NAME,Zone,Region) %>% distinct) %>%
    ungroup %>%
    arrange(desc(LATITUDE)) %>%
    mutate(Region=factor(Region, levels=unique(Region))) %>%
    arrange(LATITUDE) %>%
    mutate(cSites=as.numeric(factor(GBRMPA_SITE_NAME, levels=unique(GBRMPA_SITE_NAME))),
           Sites=as.numeric(factor(GBRMPA_SITE_NAME, levels=unique(GBRMPA_SITE_NAME)))+ scales:::rescale(CHL_QA_AVG, to=c(0,0.9)))
gg2=ggplot(flntu.chl, aes(y=Sites, x=Date, group=GBRMPA_SITE_NAME)) +
    geom_line(color='darkgreen') +
    facet_grid(Region ~., scales='free_y',space='free') +
    scale_y_continuous('GBRMPA site name',breaks=unique(flntu.chl$cSites), labels=unique(flntu.chl$GBRMPA_SITE_NAME)) +
    theme_bw() +
    scale_x_date(date_breaks = '1 years',date_labels = "%Y") +
    theme(strip.background=element_blank(), axis.title.x=element_blank(),
          axis.text.y=element_text(size=10),
          axis.ticks.y=element_blank(),
          panel.background=element_rect(fill=NA,color='black'),
          strip.text=element_text(size=12))

#ggsave('figures/Exploratory_Data_Analysis/FLNTU/flntu_temporal.pdf',grid.arrange(gg1,gg2,nrow=2), width=10, height=12, units='in')  
ggsave('figures/Exploratory_Data_Analysis/FLNTU/flntu_temporal.png',grid.arrange(gg1,gg2,nrow=2), width=10, height=12, units='in', dpi=300)  





system("cp Tables/comp.all.sum_rmse.max.tex tables/comp.all.sum_rmse.max.tex")
system("cp Tables/comp.all.sum_mae.max.tex tables/comp.all.sum_mae.max.tex")
system("cp Tables/comp.all.sum_mpe.max.tex tables/comp.all.sum_mpe.max.tex")



## Copy figures to repo

system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Maps/Map_region_waterbody_small.pdf" "figures/Maps/Map_region_waterbody.pdf"'))
system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Maps/Insitu_sites/Map_insitu1_small.pdf" "figures/Maps/Insitu_sites/Map_insitu1.pdf"'))
system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/FLNTU/flntu_temporal_small.pdf" "figures/Exploratory_Data_Analysis/FLNTU/flntu_temporal.pdf"'))

## Spatial lookup
full.lookup = expand.grid(src=c('niskin','flntu','','eReefs','eReefs926'),
                          region=c('Cape York','Wet Tropics','Dry Tropics','Mackay Whitsunday','Fitzroy','Burnett Mary'),
                          waterbody=c('Enclosed Coastal','Open Coastal','Midshelf','Offshore'),
                          measure=c('chl','nap','sd','NOx')
                          ) %>%
    mutate(path=ifelse(src=='niskin', 'Insitu',
                ifelse(src=='flntu','FLNTU',
                ifelse(src=='','Satellite',
                ifelse(src=='eReefs','eReefs','eReefs926'))))) %>%
    mutate(Include=1,
           Include=ifelse(src %in% c('flntu','') & measure=='NOx',0,Include),
           Include=ifelse(src %in% c('niskin','flntu') & region %in% c('Cape York','Fitzroy','Burnett Mary'),0,Include),
           Include=ifelse(src %in% c('niskin','flntu') & waterbody %in% c('Offshore'),0,Include),
           Include=ifelse(src %in% c('niskin','flntu') & region %in% c('Mackay Whitsunday') & waterbody %in% c('Enclosed Coastal','Midshelf'),0,Include)
           )
full.lookup %>% head(20)
## Temporal Exploratory data analysis violin plots
lookup = data.frame(src=c('niskin','flntu','','eReefs','eReefs926'),
                    path=c('Insitu','FLNTU','Satellite','eReefs','eReefs926'))
ii=(full.lookup$region=='Wet Tropics' & full.lookup$waterbody=='Open Coastal') | (full.lookup$region=='Dry Tropics' & full.lookup$waterbody=='Midshelf')
which(ii)

#for (i in 1:nrow(full.lookup[which(ii),])) {
for (i in which(ii)) {
        src=full.lookup$src[i]
        path=full.lookup$path[i]
        r=full.lookup$region[i]
        w=full.lookup$waterbody[i]
        m=full.lookup$measure[i]
        inPath='../data/eda/'
        outPath=paste0('figures/Exploratory_Data_Analysis/',path,'/')
        inFile=paste0('eda.year.',m,'_',r,'__',w,'_',src,'_log')
        print(paste0(inPath,inFile))
        if (full.lookup$Include[i]==1) {
            system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,inFile,'_small.png" "',inPath,inFile,'.pdf"'))
            #system(paste0('convert -resize 100% "',inPath,inFile,'.pdf" "',outPath,inFile,'_small.pdf"'))
        }
}
i = i +1
full.lookup$Include[i]

## Annual EDA for main text
for (s in 1:nrow(lookup)) {
    for (m in c('chl','nap','sd','NOx')) {
        if (!(s %in% c(2,3) & m=='NOx')){
            ##system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_log_small.pdf" "../data/eda/eda.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_log.pdf"'))
            ##system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.year.month.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_log_small.pdf" "../data/eda/eda.year.month.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_log.pdf"'))

            ##system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_log_small.pdf" "../data/eda/eda.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_log.pdf"'))
            ##system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.year.month.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_log_small.pdf" "../data/eda/eda.year.month.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_log.pdf"'))
            ####system(paste0('convert -resize 100% "../data/eda/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logA.pdf" "figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logA_small.png"'))
            ####system(paste0('convert -resize 100% "../data/eda/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logA.pdf" "figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logA_small.png"'))
            ###system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logA_small.png" "../data/eda/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logA.pdf"'))
            ###system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logA_small.png" "../data/eda/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logA.pdf"'))
            ##system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logB_small.png" "../data/eda/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logB.pdf"'))
            ##system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logB_small.png" "../data/eda/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logB.pdf"'))

            system(paste0('convert -resize 100% "../data/eda/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logB.pdf" "figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Wet Tropics__Open Coastal_',lookup$src[s],'_logB_small.png"'))
            system(paste0('convert -resize 100% "../data/eda/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logB.pdf" "figures/Exploratory_Data_Analysis/',lookup$path[s],'/eda.spatial.year.',m,'_Dry Tropics__Midshelf_',lookup$src[s],'_logB_small.png"'))
        }
    }
}

#system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Analyses at AIMS niskin sites/Observations/Satellite_vs_Niskin_locations_5km_small.pdf" "Figures/Satellite_vs_Niskin_locations_5km.pdf"'))
#system(paste0('convert -resize 100% "Figures/Satellite_vs_Niskin_locations_5km.pdf" "figures/Analyses at AIMS niskin sites/Observations/Satellite_vs_Niskin_locations_5km_small.png"'))
#system(paste0('convert -resize 100% "Figures/eReefs_vs_Niskin_locations_5km.pdf" "figures/Analyses at AIMS niskin sites/Observations/eReefs_vs_Niskin_locations_5km_small.png"'))
system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Analyses at AIMS niskin sites/Observations/eReefs_vs_Niskin_locations_5km_small.png" "Figures/eReefs_vs_Niskin_locations_5km.pdf"'))
system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Analyses at AIMS niskin sites/Observations/Satellite_vs_Niskin_locations_5km_small.png" "Figures/Satellite_vs_Niskin_locations_5km.pdf"'))
            
for (m in c('chl','nap','sd','NOx')) {
    #system(paste0('convert -resize 100% "Figures/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural.pdf" "figures/Analyses at AIMS niskin sites/Observations/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural_small.png"'))
    system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Analyses at AIMS niskin sites/Observations/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural_small.png" "Figures/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural.pdf"'))
    system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -r200 -sOutputFile="figures/Analyses at AIMS niskin sites/Observations/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural_small.pdf" "Figures/',m,'_eReefs_vs_Satellite_vs_Niskin_.Radius_5_natural.pdf"'))
}


for (i in c('GL_0.1.R_1000','GL_10.R_1000','GL_100.R_1000','GL_1.R_10','GL_10.R_10')) {
    #system(paste0("cp 'Figures/sensitivity.Group_1.",i,".pdf' 'figures/Sensitivity of indices/sensitivity.Group_1.",i,"_small.pdf'"))
    system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Sensitivity of indices/sensitivity.Group_1.',i,'_small.pdf"  "Figures/sensitivity.Group_1.',i,'.pdf"'))
}

#Index violin plots
for (m in c('chl','nap','sd','NOx')[2]) {
    for (idx in c('.idx_Binary_year','.idx.year','.idx_fsMAMP_year','.idx_fsMAMP4_year')[c(1,3,4)]) {
        for (z in c('Wet Tropics__Open Coastal','Dry Tropics__Midshelf')) {
            print(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_',z,'_niskin_natural.pdf"'))
                                        #system(paste0('convert -resize 100% "../data/eda/eda',idx,'.chl_',z,'_niskin_log.pdf" "figures/Exploratory_Data_Analysis/Insitu/eda',idx,'.chl_',z,'_niskin_log_small.png"'))
                                        #system(paste0('cp "../data/eda/eda',idx,'.chl_',z,'_niskin_log.png" "figures/Exploratory_Data_Analysis/Insitu/eda',idx,'.chl_',z,'_niskin_log.png"'))
            system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/Insitu/eda',idx,'.',m,'_',z,'_niskin_natural_small.pdf"  "../data/eda/eda',idx,'.',m,'_',z,'_niskin_natural.pdf"'))
            system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/FLNTU/eda',idx,'.',m,'_',z,'_flntu_natural_small.pdf"  "../data/eda/eda',idx,'.',m,'_',z,'_flntu_natural.pdf"'))
            system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/Satellite/eda',idx,'.',m,'_',z,'__natural_small.pdf"  "../data/eda/eda',idx,'.',m,'_',z,'__natural.pdf"'))
            system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/eReefs/eda',idx,'.',m,'_',z,'_eReefs_natural_small.pdf"  "../data/eda/eda',idx,'.',m,'_',z,'_eReefs_natural.pdf"'))
            system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Exploratory_Data_Analysis/eReefs926/eda',idx,'.',m,'_',z,'_eReefs926_natural_small.pdf"  "../data/eda/eda',idx,'.',m,'_',z,'_eReefs926_natural.pdf"'))
        }
    }
}

## Spatial index maps
for (m in c('chl','nap','sd','NOx')) {
    for (idx in c('.idx.spatial.year','.idx_Binary_spatial.year','.idx_fsMAMP_spatial.year','.idx_fsMAMP4_spatial.year')[2:4]) {
        print(paste0('eda',idx,'.',m,'_Wet Tropics__Open Coastal_niskin_naturalA.pdf'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Wet Tropics__Open Coastal_niskin_naturalA.pdf" "figures/Exploratory_Data_Analysis/Insitu/eda',idx,'.',m,'_Wet Tropics__Open Coastal_niskin_naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Wet Tropics__Open Coastal__naturalA.pdf" "figures/Exploratory_Data_Analysis/Satellite/eda',idx,'.',m,'_Wet Tropics__Open Coastal__naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Wet Tropics__Open Coastal_eReefs_naturalA.pdf" "figures/Exploratory_Data_Analysis/eReefs/eda',idx,'.',m,'_Wet Tropics__Open Coastal_eReefs_naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Wet Tropics__Open Coastal_eReefs926_naturalA.pdf" "figures/Exploratory_Data_Analysis/eReefs926/eda',idx,'.',m,'_Wet Tropics__Open Coastal_eReefs926_naturalA_small.png"'))
        
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Dry Tropics__Midshelf_niskin_naturalA.pdf" "figures/Exploratory_Data_Analysis/Insitu/eda',idx,'.',m,'_Dry Tropics__Midshelf_niskin_naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Dry Tropics__Midshelf_flntu_naturalA.pdf" "figures/Exploratory_Data_Analysis/FLNTU/eda',idx,'.',m,'_Dry Tropics__Midshelf_flntu_naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Dry Tropics__Midshelf__naturalA.pdf" "figures/Exploratory_Data_Analysis/Satellite/eda',idx,'.',m,'_Dry Tropics__Midshelf__naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Dry Tropics__Midshelf_eReefs_naturalA.pdf" "figures/Exploratory_Data_Analysis/eReefs/eda',idx,'.',m,'_Dry Tropics__Midshelf_eReefs_naturalA_small.png"'))
        system(paste0('convert -resize 100% "../data/eda/eda',idx,'.',m,'_Dry Tropics__Midshelf_eReefs926_naturalA.pdf" "figures/Exploratory_Data_Analysis/eReefs926/eda',idx,'.',m,'_Dry Tropics__Midshelf_eReefs926_naturalA_small.png"'))
    }
}

lookup = data.frame(src=c('niskin','flntu','','eReefs','eReefs926'),
                    path=c('AIMS insitu','AIMS FLNTU','Satellite','eReefs','eReefs926'))
for (s in 1:nrow(lookup)) {
    #probably better to just copy the file
    #system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Compare indices/All_sources_',lookup$path[s],'.Annual_measure.chl_zone_small.pdf" "Figures/All_sources_',lookup$path[s],'.Annual_measure.chl_zone.pdf"'))
    system(paste0('cp "Figures/All_sources_',lookup$path[s],'.Annual_measure.chl_zone.pdf" "figures/Indices/Compare indices/All_sources_',lookup$path[s],'.Annual_measure.chl_zone_small.pdf"'))    
}

for (idx in c('fsMAMP')) {
    for (m in c('chl','nap','sd','NOx')) {
    #system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Compare sources/All_indicies_',idx,'.Annual_measure.chl_zone_small.pdf" "Figures/All_indicies_',idx,'.Annual_measure.chl_zone.pdf"'))
        system(paste0('cp "Figures/All_indicies_',idx,'.Annual_measure.',m,'_zone.pdf" "figures/Indices/Compare sources/All_indicies_',idx,'.Annual_measure.',m,'_zone_small.pdf"'))    
    }
}


## Generate a zip of spatial EDA figures



system(paste0('cp "Figures/simple_eReefs_fsMAMP.Annual_measure.zone_Grade_Uniform.pdf" "figures/Indices/Compare measures/simple_eReefs_fsMAMP.Annual_measure.zone_Grade_Uniform_small.pdf"'))
system(paste0('cp "Figures/All_excludes_index_fsMAMP_eReefs.Annual_indicator_zone_.pdf" "figures/Indices/Compare includes/All_excludes_index_fsMAMP_eReefs.Annual_indicator_zone_small.pdf"'))    

for (m in c('chl','nap','sd','NOx')) {
    system(paste0('convert -resize 150% "Figures/spatial_map__fsMAMP.Annual_measure.',m,'.site.pdf" "figures/Indices/Maps/Measurement level/Satellite/spatial_map__fsMAMP.Annual_measure.',m,'.site_small.png"'))
    system(paste0('convert -resize 150% "Figures/spatial_map_eReefs_fsMAMP.Annual_measure.',m,'.site.pdf" "figures/Indices/Maps/Measurement level/eReefs/spatial_map_eReefs_fsMAMP.Annual_measure.',m,'.site_small.png"'))
    system(paste0('convert -resize 150% "Figures/spatial_map_eReefs926_fsMAMP.Annual_measure.',m,'.site.pdf" "figures/Indices/Maps/Measurement level/eReefs926/spatial_map_eReefs_fsMAMP.Annual_measure.',m,'.site_small.png"'))
}

for (m in c('chl','nap','sd','NOx')) {
    system(paste0('cp "Figures/spatial_map__fsMAMP.Annual_measure.',m,'.site.png" "figures/Indices/Maps/Measurement level/Satellite/spatial_map__fsMAMP.Annual_measure.',m,'.site.png"'))
    system(paste0('cp "Figures/spatial_map_eReefs_fsMAMP.Annual_measure.',m,'.site.png" "figures/Indices/Maps/Measurement level/eReefs/spatial_map_eReefs_fsMAMP.Annual_measure.',m,'.site.png"'))
    system(paste0('cp "Figures/spatial_map_eReefs926_fsMAMP.Annual_measure.',m,'.site.png" "figures/Indices/Maps/Measurement level/eReefs926/spatial_map_eReefs926_fsMAMP.Annual_measure.',m,'.site.png"'))
                                        #system(paste0('cp "Figures/spatial_map__fsMAMP.Annual_measure.',m,'.site.pdf" "figures/Indices/Maps/Measurement level/Satellite/spatial_map__fsMAMP.Annual_measure.',m,'.site.pdf"'))
                                        #system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.3 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Maps/Measurement level/Satellite/spatial_map__fsMAMP.Annual_measure.',m,'.site.pdf"  "Figures/spatial_map__fsMAMP.Annual_measure.',m,'.site.pdf"'))
}

system(paste0('cp "Figures/simple_eReefs_fsMAMP.Annual_measure.zonenoNOxnonap_Grade_Uniform.pdf" "figures/Indices/Aggregations/eReefs/simple_eReefs_fsMAMP.Annual_measure.zonenoNOxnonap_Grade_Uniform.pdf"'))
for (gradetype in c('Uniform','MMP')) {
    for (src in c('eReefs')) {
        system(paste0('cp "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf" "figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))
        #system(paste0('convert -resize 150% "figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf" "figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.pdf"'))
    #system(paste0('convert -support -1 "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf" "figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.png"'))
        #system(paste0('gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dDEVICEWIDTHPOINTS=1152 -dDEVICEFIXEDHEIGHT=688 -dFIXEDMEDIA -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.pdf" "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))
        #system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.png" -r300 "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))
        #system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dQUIET -dBATCH -sOutputFile="figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.png" -r300 -g500x288 -dPDFFITPage "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))

        system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.png" "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))
        system(paste0('convert -density 300 -trim +repage -resize 605x "figures/Indices/Maps/Measurement level/',src,'/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'_small.png" "Figures/simple_map_',src,'_fsMAMP.Annual_measure_chl.zone_A_Grade_',gradetype,'.pdf"'))
    }
}


##============================================================================================================================
## Measurement/Site level maps - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (src in c('eReefs','eReefs926','Satellite')[1]) {
    for (GradeType in c('Uniform','MMP')[1]) {
        for (m in c('chl','nap','sd')[c(1,3)]) {
            inPath='Figures'
            outPath=paste0('figures/Indices/Maps/Measurement level/',src,'/')
            src1 = ifelse(src=='Satellite','',src)
        inFile=paste0('spatial_map_',src1,'_fsMAMP.Annual_measure.',m,'.site_Grade_',GradeType)
        print(paste0(inPath,'/',inFile))
        system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
        #system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
        }       
    }
}

## Subindicator/Site level maps - there should be a separate set of maps for each source/index/gradetype/exclusions
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {    
    for (src in c('eReefs','eReefs926')[1]) {
        for (GradeType in c('Uniform')) {
            for (m in c('Productivity','Water Clarity')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Subindicator level/',src,'/')
                
                inFile=paste0('spatial_map_',src,'_fsMAMP.Annual_subindicator.',m,'.site',excludes,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
                system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
                
            }
        }
    }
}

## Indicator/Site level maps - there should be a separate set of maps for each source/index/gradetype/exclusions
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {    
    for (src in c('eReefs','eReefs926')[1]) {
        for (GradeType in c('Uniform')) {
            inPath='Figures'
            outPath=paste0('figures/Indices/Maps/Indicator level/',src,'/')
            
            inFile=paste0('spatial_map_',src,'_fsMAMP.Annual_indicator.site',excludes,'_A_Grade_',GradeType)
            print(paste0(inPath,'/',inFile))
            system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
            system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
            
        }
    }
}
    
## Measurement/Zone level worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_measure.zone',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                                        #system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
                                        #system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
                
            }
        }
    }
}

## Measurement/Zone flat maps - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                for (m in c('chl','nap','sd')) {
                    inPath='Figures'
                    outPath=paste0('figures/Indices/Maps/Measurement level/',src,'/')
                    
                    inFile=paste0('simple_map_',src,'_fsMAMP.Annual_measure_',m,'.zone',excludes,'_',coastal,'_A_Grade_',GradeType)
                    print(paste0(inPath,'/',inFile))
                    system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
                    system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
                    
                }
            }
        }
    }
}

    

## Measurement/Zone mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Measurement level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_measure.zone',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}


## Subindicator/Zone worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_subindicator.zone',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}



## Subindicator/Zone flat maps - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                for (m in c('Productivity','Water Clarity')) {
                    inPath='Figures'
                    outPath=paste0('figures/Indices/Maps/Subindicator level/',src,'/')
                    
                    inFile=paste0('simple_map_',src,'_fsMAMP.Annual_subindicator_',m,'.zone',excludes,'_',coastal,'_A_Grade_',GradeType)
                    print(paste0(inPath,'/',inFile))
                    system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
                    system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
                    
                }
            }
        }
    }
}


## Subindicator/Zone mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Subindicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_subindicator.zone',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}
    
## Indicator/Zone worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_indicator.zone',excludes,'_',coastal,'_A_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Indicator/Zone flat maps - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform','MMP')[1]) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Indicator level/',src,'/')
                
                inFile=paste0('simple_map_',src,'_fsMAMP.Annual_indicator.zone',excludes,'_',coastal,'_A_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
                system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
            }
        }
    }
}


## Indicator/Zone mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Indicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_indicator.zone',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}


## Measure/Waterbody worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_measure.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Measure/Waterbody mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Measurement level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_measure.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}



## Subindicator/Waterbody worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_subindicator.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Subindicator/Waterbody mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Subindicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_subindicator.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Indicator/Waterbody worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_indicator.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Indicator/Waterbody mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[2]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Indicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_indicator.waterbody',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}



## Measure/GBR worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_measure.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Measure/GBR mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Measurement level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_measure.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}



## Subindicator/GBR worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_subindicator.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Subindicator/GBR mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Subindicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_subindicator.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Indicator/GBR worms - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Aggregations/',src,'/')
                
                inFile=paste0('simple_',src,'_fsMAMP.Annual_indicator.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}

## Indicator/GBR mosaic - there should be a separate set of maps for each source/index/gradetype
## RUN THIS AGAIN ONCE WE HAVE eReefs926 and MMP combinations
for (excludes in c('noNOxnonap','noNOx','noSDnoNOx')[1]) {
    for (coastal in c('with_enclosed_coastal', 'without_enclosed_coastal')[1]) {
        for (src in c('eReefs','eReefs926')[1]) {
            for (GradeType in c('Uniform')) {
                inPath='Figures'
                outPath=paste0('figures/Indices/Maps/Indicator level/',src,'/')
                
                inFile=paste0('mosaic_',src,'_fsMAMP.Annual_indicator.gbr',excludes,'_',coastal,'_Grade_',GradeType)
                print(paste0(inPath,'/',inFile))
                system(paste0('cp "',inPath,'/',inFile,'.pdf" "',outPath,'/',inFile,'.pdf"'))
                
            }
        }
    }
}
    


## All indices========================================================================================================================

## Compare sources
inPath='Figures'
outPath=paste0('figures/Indices/Compare sources/')

inFile=paste0('simple_map_',src,'_fsMAMP.Annual_subindicator_',m,'.zone',excludes,'_',coastal,'_A_Grade_',GradeType)
print(paste0(inPath,'/',inFile))
system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))


for (GradeType in c('Uniform','MMP')) {
    for (m in c('chl','nap','sd')) {
        inPath='Figures'
        outPath='figures/Indices/Maps/Measurement level/eReefs/'

        
        #inFile=paste0('simple_map_eReefs_fsMAMP.Annual_measure_',m,'.zonenoSDnoNOx_with_enclosed_coastal_A_Grade_',GradeType)
        inFile=paste0('simple_map_eReefs926_fsMAMP.Annual_measure_',m,'.zonenoNOx_with_enclosed_coastal_A_Grade_',GradeType)
        
                                        #system(paste0("cp 'Figures/simple_map_eReefs_fsMAMP.Annual_measure_chl.zonenoSDnoNOx_with_enclosed_coastal_A_Grade_MMP.pdf' 'figures/Indices/Maps/Measurement level/eReefs/simple_map_eReefs_fsMAMP.Annual_measure_chl.zonenoSDnoNOx_with_enclosed_coastal_A_Grade_MMP.pdf'"))
        system(paste0('gs -sDEVICE=pngalpha -dCompatibilityLevel=1.4 -dNOPAUSE -dQUIET -dBATCH -r300 -sOutputFile="',outPath,'/',inFile,'_small.png" "',inPath,'/',inFile,'.pdf"'))
        system(paste0('convert -density 300 -trim +repage "',outPath,'/',inFile,'_small.png" "',outPath,'/',inFile,'_small.png"'))
    }
}


## Beta distribution
library(tidyr)
library(dplyr)
library(ggplot2)
x = seq(0,1,len=1000)
b1 = dbeta(x, 0.5,0.5)
b2 = dbeta(x, 5,1)
b3 = dbeta(x, 1,3)
b4 = dbeta(x, 2,2)
b5 = dbeta(x, 2,5)
dat = data.frame(x, b1,b2,b3,b4,b5) %>% gather(key='Shape', value='Value', b1,b2,b3,b4,b5) 

g=ggplot(dat, aes(y=Value, x=x)) + geom_line(aes(color=Shape)) +ylim(c(0,5)) +
theme_classic() + theme(legend.position=c(0.5,1),legend.justification=c(0.5,1), axis.title.x=element_blank()) +
scale_y_continuous('Probability density function') +
scale_color_manual(name='',values=c('red','blue','green','purple','orange'),
labels=expression({alpha==0.5}*', '*{beta==0.5},
{alpha==5}*', '*{beta==1},
{alpha==1}*', '*{beta==3},
{alpha==2}*', '*{beta==2},
{alpha==2}*', '*{beta==5}
))
ggsave('figures/Diagrams/beta.pdf', g, width=4, height=3, units='in')


## Control charts
library(ggplot2)
library(gridExtra)
## Uniform
trafficLightColors = colorRampPalette(c('#ED1C24','#F47721','#F0C918','#B0D235','#00734D'))

dat = data.frame(Score = seq(0,1,length=1000))
g1=ggplot(dat, aes(y=1, x=Score)) +
    annotate(geom='rect',ymin=0, ymax=1,xmin=0, xmax=0.2, fill=trafficLightColors(5)[1])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.2, xmax=0.4, fill=trafficLightColors(5)[2])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.4, xmax=0.6, fill=trafficLightColors(5)[3])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.6, xmax=0.8, fill=trafficLightColors(5)[4])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.8, xmax=1, fill=trafficLightColors(5)[5])+
    annotate(geom='text', x=seq(0.1,0.9,by=0.2), y=0.5,label=rev(LETTERS[1:5]), size=20, color='white',fontface = "bold")+
    scale_x_continuous('', breaks=seq(0,1,by=0.2),expand=c(0,0)) +
    scale_y_continuous('', breaks=100,expand=c(0,0)) +
    theme_classic(14) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
          plot.margin=unit(c(0.1,1,0.5,1), 'lines')) +
    ggtitle('a) Uniform')

## MMP   
g2=ggplot(dat, aes(y=1, x=Score)) +
    annotate(geom='rect',ymin=0, ymax=1,xmin=0, xmax=0.25, fill=trafficLightColors(5)[1])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.25, xmax=0.5, fill=trafficLightColors(5)[2])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.5, xmax=0.5 + 1/3*0.5, fill=trafficLightColors(5)[3])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.5 + 1/3*0.5, xmax=0.5 + 2/3*0.5, fill=trafficLightColors(5)[4])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.5 + 2/3*0.5, xmax=1, fill=trafficLightColors(5)[5])+
    annotate(geom='text', x=c(0.25/2,0.5 - 0.25/2, 0.5 + 1/6*0.5, 0.5 + 3/6*0.5,0.5 + 5/6*0.5) , y=0.5,label=rev(LETTERS[1:5]), size=20, color='white',fontface = "bold")+
    scale_x_continuous('', breaks=c(0.25,0.5, 0.5 + 1/3*0.5, 0.5 + 2/3*0.5, 1),expand=c(0,0)) +
    scale_y_continuous('', breaks=100,expand=c(0,0)) +
    theme_classic(14) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
          plot.margin=unit(c(0.1,1,0.5,1), 'lines'))+
    ggtitle('b) AIMS Marine Monitoring Water Quality and Coral Report Cards')

## GHHP
g3=ggplot(dat, aes(y=1, x=Score)) +
    annotate(geom='rect',ymin=0, ymax=1,xmin=0, xmax=0.25, fill=trafficLightColors(5)[1])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.25, xmax=0.5, fill=trafficLightColors(5)[2])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.5, xmax=0.65, fill=trafficLightColors(5)[3])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.65, xmax=0.85, fill=trafficLightColors(5)[4])+
    annotate(geom='rect',ymin=0, ymax=1,xmin=0.85, xmax=1, fill=trafficLightColors(5)[5])+
    annotate(geom='text', x=c(0.25/2,0.5 - 0.25/2, mean(c(0.5,0.65)), mean(c(0.65,0.85)), mean(c(0.85,1))) , y=0.5,label=rev(LETTERS[1:5]), size=20, color='white',fontface = "bold")+
    scale_x_continuous('', breaks=c(0.25,0.5, 0.65,0.85, 1),expand=c(0,0)) +
    scale_y_continuous('', breaks=100,expand=c(0,0)) +
    theme_classic(14) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
          plot.margin=unit(c(0.1,1,0.5,1), 'lines'))+
    ggtitle('c) Gladstone Healthy Harbour Partnership Environmental Report Card')

## Great Lakes Council
g4=ggplot(dat, aes(y=1, x=Score)) +
    geom_segment(aes(yend=0, xend=Score, y=1-0.1, color=Score), show.legend=FALSE)+
    scale_color_gradientn(colors=trafficLightColors(5)) +
    annotate(geom='text', x=c(0.4/2,mean(c(0.4,0.56)), mean(c(0.56,0.73)), mean(c(0.73,0.93)), mean(c(0.93,1))) , y=0.45,label=rev(LETTERS[1:5]), size=18, color='white',fontface = "bold")+
    annotate(geom='segment', x=c(0,0.4,0.56,0.73,0.93,1), xend=c(0,0.4,0.56,0.73,0.93,1), y=0, yend=1.2) +
    annotate(geom='text', x=c(0.4/2,mean(c(0.4,0.56)), mean(c(0.56,0.73)), mean(c(0.73,0.93)), mean(c(0.93,1))), y=1.0, label=c('5%','15%','30%','30%','20%'),vjust=0) +
    scale_x_continuous('', breaks=c(0,0.4,0.56,0.73,0.93,1),expand=c(0,0)) +
    scale_y_continuous('', breaks=100,expand=c(0,0)) +
    theme_classic(14) +
    theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
          plot.margin=unit(c(0.1,1,0.5,1), 'lines'))+
    ggtitle('d) MidCoast Council Waterway and Catchment Report')

ggsave('figures/Diagrams/controlcharts.pdf', grid.arrange(g1,g2,g3,g4,ncol=1), width=10, height=7.5, units='in', device=cairo_pdf)


system('cp Tables/GradeTypeComparison.tex tables/GradeTypeComparison.tex')


## Guidlines table
guidelines = read.csv('../parameters/wq.guidelines.csv', strip.white=TRUE)
measures = read.table('../parameters/measures.txt', header=TRUE,strip.white=TRUE, sep=';') %>%
  mutate(UnitsLabel = gsub('.*\\((.*)\\).*','\\1',UnitsLabel),
         UnitsLabel = gsub('⁻¹','^{-1}',UnitsLabel))

GL=guidelines %>%
   mutate(
   Region=case_when(stringr:::str_detect(.$Zone,'Cape') ~ 'Cape York',
   				   stringr:::str_detect(.$Zone,'Wet Tropic') ~ 'Wet Tropics',
                    stringr:::str_detect(.$Zone,'Dry Tropics') ~ 'Dry Tropics',
                    stringr:::str_detect(.$Zone,'Mackay') ~ 'Mackay Whitsunday',
                    stringr:::str_detect(.$Zone,'Fitzroy') ~ 'Fitzroy',
                    stringr:::str_detect(.$Zone,'Burnett') ~ 'Burnett Mary'),
   waterBody=case_when(stringr:::str_detect(.$Zone,'Enclosed') ~ 'Enclosed Coastal',
                       stringr:::str_detect(.$Zone,'Open') ~ 'Open Coastal',
 					  stringr:::str_detect(.$Zone,'Midshelf') ~ 'Midshelf',
 					  stringr:::str_detect(.$Zone,'Offshore') ~ 'Offshore')				   
   ) %>% rename(GL=Annual.guideline, Justification=Justification.Source) %>%
   left_join(measures %>% dplyr:::select(Units,UnitsLabel) %>% distinct,by=c('Unit'='Units')) %>% dplyr:::select(-Unit) %>% rename(Unit=UnitsLabel) %>%
    dplyr:::select(Measure,Unit,waterBody,Region,GL,Wet,Dry,DirectionOfFailure,Justification) %>%
    dplyr:::filter(!is.na(Unit))
GL = GL %>% mutate(Justification=gsub('%','\\\\%',Justification))   
addtorow = list()
addtorow$pos = list()
addtorow$pos[[1]] = c(0)
addtorow$pos[[2]] = match(unique(GL$Measure),GL$Measure) -1
addtorow$pos[[3]] = match(unique(interaction(GL$Measure,GL$waterBody)),interaction(GL$Measure,GL$waterBody)) -1
addtorow$pos[[4]] = nrow(GL)
addtorow$command = c(paste(
   "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\n",
   "\\begin{landscape}\n",
   "\\LTcapwidth=\\linewidth\n",
   "\\setlength\\aboverulesep{0pt}\\setlength\\belowrulesep{0pt}\n",
   "\\setlength\\cmidrulekern{1pt}\\setlength\\cmidrulewidth{1pt}\n",
   "\\renewcommand\\arraystretch{1.2}\\setlength\\tabcolsep{5pt}\n",
   "\\scriptsize\n",
   "\\begin{longtable}{\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} c\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} r\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} r\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} r\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} c\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\vline} l\n",
   "!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]}\n",
   "}\\caption{Water Quality Threshold values for each Measure in each Zone (Region/Water Body).  Thresholds values are similar to annual Guideline values.  Wet and Dry represent Wet and Dry season thresholds respectively.  Direction of Failure indicates whether a values higher ('H') or lower ('L') than a Threshold would constitute an exceedence.  Range From and Range To represent Thresholds for Measures that have a range of optimum values (such as dissolved oxygen or pH).}\\label{tab:thresholds}\\\\\n",
 
   "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} \n",
   "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
   "\\arrayrulecolor[rgb]{0.53,0.62,0.74} \n",
   "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}}{}&\\multicolumn{1}{c}{}&\\multicolumn{1}{c}{}&\\multicolumn{1}{c}{}&\\multicolumn{3}{c}{\\whiteHeader{{Threshold}}}&\\multicolumn{1}{c}{\\whiteHeader{{Direction}}}&\\\\ \n",
   "\\addlinespace[-1pt] \n",
   "\\arrayrulecolor[rgb]{0.53,0.62,0.74}\\cmidrule(lr){1-4}\\cmidrule(lr){8-9} \n",
   "\\addlinespace[-1pt] \n",
   "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\cmidrule(lr){5-7} \n",
   "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
   "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}}{\\whiteHeader{{Measure}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Units}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Water Body}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Region}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Annual}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Dry}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Wet}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{of Faiure}}} & \n",
   "\\whiteHeader{{Justification}}\\\\ \n",
   "\\cmidrule{1-9} \n",
   "\\endfirsthead \n",
   "\\multicolumn{9}{l}{\\small\\textsl{...continued from previous page}}\\\\ \n",
   "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\specialrule{1pt}{0pt}{0pt} \n",
   "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
   "\\arrayrulecolor[rgb]{0.53,0.62,0.74} \n",
   "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}}{}&\\multicolumn{1}{c}{}&\\multicolumn{1}{c}{}&\\multicolumn{1}{c}{}&\\multicolumn{3}{c}{\\whiteHeader{{Threshold}}}&\\multicolumn{1}{c}{\\whiteHeader{{Direction}}}&\\\\ \n",
   "\\addlinespace[-1pt] \n",
   "\\arrayrulecolor[rgb]{0.53,0.62,0.74}\\cmidrule(lr){1-4}\\cmidrule(lr){8-9} \n",
   "\\addlinespace[-1pt] \n",
   "\\arrayrulecolor[rgb]{0.06,0.25,0.49}\\cmidrule(lr){5-7} \n",
   "\\rowcolor[rgb]{0.53,0.62,0.74} \n",
   "\\multicolumn{1}{!{\\color[rgb]{0.06,0.25,0.49}\\VRule[1pt]} p{6em}}{\\whiteHeader{{Measure}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Units}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Water Body}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Region}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Annual}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Dry}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{Wet}}} & \n",
   "\\multicolumn{1}{c}{\\whiteHeader{{of Faiure}}} & \n",
   "\\whiteHeader{{Justification}}\\\\ \n",
   "\\cmidrule{1-9} \n",
   "\\endhead \n",
   "\\hline \n",
   "\\endfoot \n",
   "\\bottomrule \n",
   "\\endlastfoot \n"
   ),
   "\\cline{1-9}",
   "\\cline{3-9}",
   paste(
       "\\end{longtable}\n",
       "\\end{landscape}")
   )

writeLines(text=
GL %>%
xtable %>% print(include.rownames=FALSE, add.to.row=addtorow, only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) x, comment=FALSE),
con='tables/guidelines.tex')

%print(include.rownames=FALSE, add.to.row=addtorow, only.contents=TRUE,include.colnames=FALSE,sanitize.text.function=function(x) x, comment=FALSE),

