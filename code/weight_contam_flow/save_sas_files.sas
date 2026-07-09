

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

/* Check what we got */
proc contents data = work.ucmr_contam;
run;

/* Point a libref to the destination folder */
libname outlib "C:/Users/js5466/Drexel University/De Roos,Anneclaire - UCMR_CodedData-PWS_ForMDAC";

/* Write the permanent dataset */
data outlib.weighted_tract_level_ucmr_contam;
    set work.ucmr_contam;
    where pws_coverage_proportion >= .1;
run;


data outlib.weighted_pws_level_ucmr_contam;
    set work.ucmr_pws_contam;
run;

libname outlib clear;


