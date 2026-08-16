* Written by R;
*  write.foreign(teens, "teenSNS.txt", "teenSNS.sas", package = "SAS") ;

PROC FORMAT;
value gender 
     1 = "F" 
     2 = "M" 
;

DATA  rdata ;
INFILE  "teenSNS.txt" 
     DSD 
     LRECL= 109 ;
INPUT
 gradyear
 gender
 age
 friends
 basketball
 football
 soccer
 softball
 volleyball
 swimming
 cheerleading
 baseball
 tennis
 sports
 cute
 sex
 sexy
 hot
 kissed
 dance
 band
 marching
 music
 rock
 god
 church
 jesus
 bible
 hair
 dress
 blonde
 mall
 shopping
 clothes
 hollister
 abercrombie
 die
 death
 drunk
 drugs
 female
 no_gender
;
FORMAT gender gender. ;
RUN;
