

/* Import the CSV */
proc import
    datafile = "C:\git\epa_water_stuff\data\results\weighted_tract_level_ucmr_contam_wide.csv"
    out = work.ucmr_contam
    dbms = csv
    replace;
    guessingrows = max;
run;


proc import
    datafile = "C:\git\epa_water_stuff\data\results\weighted_pws_level_ucmr_contam_wide.csv"
    out = work.ucmr_pws_contam
    dbms = csv
    replace;
    guessingrows = max;
run;


proc import
    datafile = "C:\git\epa_water_stuff\data\results\ucmr_tract_exposure_covariates.csv"
    out = work.ucmr_tract_fin
    dbms = csv
    replace;
    guessingrows = max;
run;


data ucmr_tract_fin;
    set ucmr_tract_fin;
    geoid_c = put(tract_geoid, z11.);  /* z11. pads to 11 digits with leading zeros */
    drop tract_geoid;
    rename geoid_c = tract_geoid;
run;

proc contents data = ucmr_tract_fin; run;

/* Check what we got */
proc contents data = work.ucmr_contam;
run;

/* Point a libref to the destination folder */
libname outlib "C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC";

/* Write the permanent dataset */
data outlib.weighted_tract_level_ucmr_contam;
    set work.ucmr_contam;
    where pws_coverage_proportion >= .5;
run;


data outlib.weighted_pws_level_ucmr_contam;
    set work.ucmr_pws_contam;
run;

data outlib.ucmr_tract_exposure_covariates;
    set work.ucmr_tract_fin;
run;


libname outlib clear;

proc print data = ucmr_tract_fin (obs = 100); run;


