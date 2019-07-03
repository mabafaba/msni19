# Calculate the MSNI for 2019
msni19 <- function( education_lsg,
                    fsl_lsg,
                    health_lsg,
                    protection_lsg,
                    shelter_lsg,
                    wash_lsg,
                    capacity_gaps,
                    impact
                    ){


  # ensure proper input:

  inputs<-  list(

    education_lsg=education_lsg,
    fsl_lsg=fsl_lsg,
    health_lsg=health_lsg,
    protection_lsg=protection_lsg,
    shelter_lsg=shelter_lsg,
    wash_lsg=wash_lsg,
    capacity_gaps=capacity_gaps,
    impact = impact)

  factorerror<-function(name){
    paste(name,
          "must not be a factor. Use as.numeric() to convert _factor level indexes_ to numbers; use as.numeric(as.character()) to convert factors with numbers as labels ")

  }
  assertthat::assert_that(!is.factor(education_lsg),msg =factorerror(education_lsg))
  assertthat::assert_that(!is.factor(fsl_lsg),msg =factorerror(fsl_lsg))
  assertthat::assert_that(!is.factor(health_lsg),msg =factorerror(health_lsg))
  assertthat::assert_that(!is.factor(protection_lsg),msg =factorerror(protection_lsg))
  assertthat::assert_that(!is.factor(shelter_lsg),msg =factorerror(shelter_lsg))
  assertthat::assert_that(!is.factor(wash_lsg),msg =factorerror(wash_lsg))
  assertthat::assert_that(!is.factor(capacity_gaps),msg =factorerror(capacity_gaps))
  assertthat::assert_that(!is.factor(impact),msg =factorerror(capacity_gaps))

  assertthat::assert_that(typeof(education_lsg) %in% c("numeric","double"),message('education_lsg must be a numeric vector'))
  assertthat::assert_that(typeof(fsl_lsg) %in% c("numeric","double"),message('fsl_lsg must be numeric'))
  assertthat::assert_that(typeof(health_lsg) %in% c("numeric","double"),message('health_lsg must be a numeric vector'))
  assertthat::assert_that(typeof(protection_lsg) %in% c("numeric","double"),message('protection_lsg a numeric vector'))
  assertthat::assert_that(typeof(capacity_gaps) %in% c("numeric","double"),message('capacity_gaps must be a numeric vector'))
  assertthat::assert_that(typeof(wash_lsg) %in% c("numeric","double"),message('wash_lsg must be numeric'))
  assertthat::assert_that(typeof(impact) %in% c("numeric","double"),message('wash_lsg must be numeric'))

  inputs_numeric <- inputs %>% lapply(is.numeric) %>% unlist %>% .[!.] %>% names
  if(length(inputs_numeric)!=0){
    stop(paste("inputs must be numeric:", paste(inputs_numeric,collapse = ", ")))
  }

  inputs_range <-  inputs %>% lapply(function(x){all(x>=0) & all(x<=4)}) %>% unlist %>% .[!.] %>% names

  if(length(inputs_numeric)!=0){
    stop(paste("inputs are not in range 0-4:", paste(inputs_range,collapse = ", ")))
  }

  all_same_length <-  inputs %>% lapply(length) %>% unlist %>% (function(x){length(unique(x))==1})
  if(!all_same_length){stop("all inputs must be vectors of the same length")}


  # 1. start with largest from health, prot, shelter:
  msni <- pmax(health_lsg,protection_lsg,shelter_lsg)
  # 2. replace if impact if impact is lower:
  msni <- pmin(msni, impact)

  #' 3.  Replace with minimum score of any two combinations of
  #' health, prot, and shelter
  #' if higher than the previous score:
  largest_lsg_combo <- pmax(
    pmin(health_lsg,protection_lsg),
    pmin(health_lsg, shelter_lsg),
    pmin(shelter_lsg, protection_lsg)
  )

  msni <- pmax(msni, largest_lsg_combo)

  # If higher than current, replace with highest from wash, FSL or capacity gap
  msni<-pmax(msni, wash_lsg,fsl_lsg,capacity_gaps)

  # Notes: if now msni = 1 but health, protection or shelter >3, then 2
  where_notes_apply <- (msni == 1 & pmax(health_lsg,
                                     protection_lsg,
                                     shelter_lsg,
                                     education_lsg) >= 3)

  # Note 2: if now msni = 1 but education >=3, then 2
  msni[note_1_applies]<- 2

  #
  # done!
  msni
  }


