suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(grid))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(stringi))

## Read data
stormData <- read.csv("./data/repdata_data_StormData.csv.bz2", 
                      stringsAsFactors = FALSE)

## Check data
Oldest <- min(as.POSIXct(stormData$BGN_DATE, tz = "", "%m/%d/%Y %H:%M:%S"))
Newest <- max(as.POSIXct(stormData$BGN_DATE, tz = "", "%m/%d/%Y %H:%M:%S"))

evtypes <- unique(stri_trans_toupper(stri_trim(stormData$EVTYPE)))
evtypes <- evtypes[order(evtypes)]
summary(evtypes)

exptypes <- unique(c(stri_trans_toupper(stri_trim(stormData$PROPDMGEXP)),
                     stri_trans_toupper(stri_trim(stormData$CROPDMGEXP))))
exptypes <- exptypes[order(exptypes)]
summary(exptypes)

## Impact on human health
## Define impact as number of fatalities + number of injuries
victims <- stormData[, c("REFNUM", "EVTYPE", "FATALITIES", "INJURIES")]
victims$VICTIMS <- victims$FATALITIES + victims$INJURIES
victims <- subset(victims, victims$VICTIMS > 0)
victims$EVTYPE <- stri_trans_toupper(stri_trim(victims$EVTYPE))
victims_byType <- aggregate(victims[c("FATALITIES", "INJURIES", "VICTIMS")], 
                            by = victims["EVTYPE"], FUN = sum)
victims_byType_all <- victims_byType[order(victims_byType$VICTIMS, 
                                           decreasing = TRUE),]
victims_byType_fatal <- victims_byType[order(victims_byType$FATALITIES,
                                             decreasing = TRUE),]
victims_byType_injur <- victims_byType[order(victims_byType$INJURIES, 
                                             decreasing = TRUE),]
head(victims_byType_all)
head(victims_byType_fatal)
head(victims_byType_injur)

##barplot(victims_byType_all$VICTIMS[5:1], 
##       names.arg = victims_byType_all$EVTYPE[5:1], 
##       horiz = TRUE, axes = TRUE, axisnames = TRUE, las = 1)

ggplot(victims_byType_all[1:5,], aes(y = VICTIMS, x = EVTYPE)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(limits = victims_byType_all$EVTYPE[1:5]) +
    ggtitle("Impact of Extreme Weather Events on Population Health\n Top 5 Event Types by Number of Victims") +
    theme(plot.title=element_text(size=rel(1.5), lineheight=1.0,
                                  face="bold")) +
    xlab("Weather Event Type") + ylab("Number of Victims (Fatalities + Injuries)")

## Economic impact
## Define impact as sum of property damage and crop damage
damage <- stormData[, c("REFNUM", "EVTYPE", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
damage <- subset(damage, damage$PROPDMG > 0 | damage$CROPDMG > 0)
damage$EVTYPE <- stri_trans_toupper(stri_trim(damage$EVTYPE))
damage$PROPDMGEXP <- stri_trans_toupper(damage$PROPDMGEXP)
damage$CROPDMGEXP <- stri_trans_toupper(damage$CROPDMGEXP)

exptypes <- unique(c(unique(damage$PROPDMGEXP), unique(damage$CROPDMGEXP)))
exptypes

histPropExp <- aggregate(damage$REFNUM, by = damage["PROPDMGEXP"], FUN = length)
histPropExp
histCropExp <- aggregate(damage$REFNUM, by = damage["CROPDMGEXP"], FUN = length)
histCropExp

getEXP_strict <- function(x) {
    if (x == "") {
        1
    }
    else {
        switch(x,
               K = 1000,
               M = 1000000,
               B = 1000000000,
               NA)
    }
}

damage$PROPEXP_strict <- sapply(damage$PROPDMGEXP, FUN = getEXP_strict, USE.NAMES = FALSE)
damage$CROPEXP_strict <- sapply(damage$CROPDMGEXP, FUN = getEXP_strict, USE.NAMES = FALSE)

getEXP_all <- function(x) {
    if (x == "") {
        1
    }
    else if (!is.na(suppressWarnings(as.integer(x)))) {
        10^as.integer(x)
    }
    else {
        switch(x,
               H = 100,
               K = 1000,
               M = 1000000,
               B = 1000000000,
               NA)
    }
}

damage$PROPEXP_all <- sapply(damage$PROPDMGEXP, FUN = getEXP_all, USE.NAMES = FALSE)
damage$CROPEXP_all <- sapply(damage$CROPDMGEXP, FUN = getEXP_all, USE.NAMES = FALSE)

damage$PROPDMG_strict <- mapply(function(x, y) {x * y}, 
                                damage$PROPDMG, damage$PROPEXP_strict, USE.NAMES = FALSE)
damage$PROPDMG_all <- mapply(function(x, y) {x * y}, 
                             damage$PROPDMG, damage$PROPEXP_all, USE.NAMES = FALSE)
damage$CROPDMG_strict <- mapply(function(x, y) {x * y}, 
                                damage$CROPDMG, damage$CROPEXP_strict, USE.NAMES = FALSE)
damage$CROPDMG_all <- mapply(function(x, y) {x * y}, 
                             damage$CROPDMG, damage$CROPEXP_all, USE.NAMES = FALSE)

damage$DMG_strict <- damage$PROPDMG_strict + damage$CROPDMG_strict
damage$DMG_all <- damage$PROPDMG_all + damage$CROPDMG_all

damage_byType <- aggregate(damage[c("PROPDMG_strict", "PROPDMG_all", "CROPDMG_strict", 
                                    "CROPDMG_all", "DMG_strict", "DMG_all")], 
                           by = damage["EVTYPE"], FUN = sum)

damage_byType_all <- damage_byType[order(damage_byType$DMG_all, decreasing = TRUE),]
damage_byType_strict <- damage_byType[order(damage_byType$DMG_strict, decreasing = TRUE),]
damage_byType_propAll <- damage_byType[order(damage_byType$PROPDMG_all, decreasing = TRUE),]
damage_byType_propStrict <- damage_byType[order(damage_byType$PROPDMG_strict, decreasing = TRUE),]
damage_byType_cropAll <- damage_byType[order(damage_byType$CROPDMG_all, decreasing = TRUE),]
damage_byType_cropStrict <- damage_byType[order(damage_byType$CROPDMG_strict, decreasing = TRUE),]
head(damage_byType_all)
head(damage_byType_strict)
head(damage_byType_propAll)
head(damage_byType_propStrict)
head(damage_byType_cropAll)
head(damage_byType_cropStrict)

ggplot(damage_byType_all[1:5,], aes(y = DMG_all, x = EVTYPE)) + 
    geom_bar(stat = "identity") + 
    scale_x_discrete(limits = damage_byType_all$EVTYPE[5:1]) + coord_flip()

