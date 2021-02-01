/*결과창(Result Viewer지우기*/
dm 'odsresults; clear';


proc import datafile = "C:\Users\GWANGRYUL\Desktop\스터디\카드데이터1\data04\result1.csv" dbms = csv out = result1 replace;
run;

/*Ward방법으로 군집분석*/
proc cluster data = result1 method =  ward outtree = result1out standard pseudo rsq;
var n avg;
id store_id;
copy n avg;
run;

/*나무구조그림 그리기, ncl= 군집의개수5개지정*/
proc tree data = result1out ncl=5 out=cluster_1;
id store_id;
run;
proc print data = cluster_1;
run;
/*데이터 정렬 및 병합*/
proc sort data = cluster_1;
by store_id;
run;
proc sort data = result1;
by store_id;
run;
data cluster_1d;
merge cluster_1 result1;
by store_id;
run;
proc sort data = cluster_1d;
by cluster;
run;
proc print data = cluster_1d;
run;

/*군집별 기초통계량*/
proc means data = cluster_1d;
by cluster;
run;



/*이후 cluster1d를 파일로 저장한후 R에서 log취해서 올거임(그래프 그리려고)*/

proc import datafile = "C:\Users\GWANGRYUL\Desktop\스터디\카드데이터1\data04\cluster1d_log.csv" dbms = csv out = cluster1d_log replace;
run;
proc gplot data = cluster1d_log;
plot n*avg = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= blue;
symbol5  i= none h=0.5 v=dot c= black;
run;





proc import datafile = "C:\Users\GWANGRYUL\Desktop\스터디\카드데이터1\data04\result2.csv" dbms = csv out = result2 replace;
run;

/*Ward방법으로 군집분석*/
proc cluster data = result2 method =  ward outtree = result2out standard pseudo rsq;
var n avg hour2_p -- hour22_p;
id store_id;
copy n avg hour2_p--hour22_p;
run;

/*나무구조그림 그리기, ncl= 군집의개수8개지정*/
proc tree data = result2out ncl=8 out=cluster_2;
id store_id;
run;
proc print data = cluster_2;
run;
/*데이터 정렬 및 병합*/
proc sort data = cluster_2;
by store_id;
run;
proc sort data = result2;
by store_id;
run;
data cluster_2d;
merge cluster_2 result2;
by store_id;
run;
proc sort data = cluster_2d;
by cluster;
run;
proc print data = cluster_2d;
run;

/*군집별 기초통계량*/
proc means data = cluster_2d;
by cluster;
run;



/*주성분분석*/
proc princomp data = cluster_2d out = prin2;
var n avg hour2_p -- hour22_p;
run;

/*2차원 산점도 -> 잘 안나와서 3차까지 필요할것 같음*/
proc gplot data = prin2;
plot prin2*prin1 = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= pupple;
symbol5  i= none h=0.5 v=dot c= blue;
run;







proc import datafile = "C:\Users\GWANGRYUL\Desktop\스터디\카드데이터1\data04\result3.csv" dbms = csv out = result3 replace;
run;

/*Ward방법으로 군집분석*/
proc cluster data = result3 method =  ward outtree = result3out standard pseudo rsq;
var n avg hour2_p -- hour22_p mean_install count_install_p ;
id store_id;
copy n avg hour2_p--hour22_p mean_install count_install_p;
run;

/*나무구조그림 그리기, ncl= 군집의개수9개지정*/
proc tree data = result3out ncl=9 out=cluster_3;
id store_id;
run;
proc print data = cluster_3;
run;
/*데이터 정렬 및 병합*/
proc sort data = cluster_3;
by store_id;
run;
proc sort data = result3;
by store_id;
run;
data cluster_3d;
merge cluster_3 result3;
by store_id;
run;
proc sort data = cluster_3d;
by cluster;
run;
proc print data = cluster_3d;
run;

/*군집별 기초통계량*/
proc means data = cluster_3d;
by cluster;
run;



/*주성분분석*/
proc princomp data = cluster_3d out = prin3;
var  n avg hour2_p -- hour22_p mean_install count_install_p ;
run;
/*2차원 산점도 -> 잘 안나와서 3차까지 필요할것 같음*/
proc gplot data = prin3;
plot prin2*prin1 = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= pupple;
symbol5  i= none h=0.5 v=dot c= blue;
run;
