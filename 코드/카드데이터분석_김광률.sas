/*���â(Result Viewer�����*/
dm 'odsresults; clear';


proc import datafile = "C:\Users\GWANGRYUL\Desktop\���͵�\ī�嵥����1\data04\result1.csv" dbms = csv out = result1 replace;
run;

/*Ward������� �����м�*/
proc cluster data = result1 method =  ward outtree = result1out standard pseudo rsq;
var n avg;
id store_id;
copy n avg;
run;

/*���������׸� �׸���, ncl= �����ǰ���5������*/
proc tree data = result1out ncl=5 out=cluster_1;
id store_id;
run;
proc print data = cluster_1;
run;
/*������ ���� �� ����*/
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

/*������ ������跮*/
proc means data = cluster_1d;
by cluster;
run;



/*���� cluster1d�� ���Ϸ� �������� R���� log���ؼ� �ð���(�׷��� �׸�����)*/

proc import datafile = "C:\Users\GWANGRYUL\Desktop\���͵�\ī�嵥����1\data04\cluster1d_log.csv" dbms = csv out = cluster1d_log replace;
run;
proc gplot data = cluster1d_log;
plot n*avg = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= blue;
symbol5  i= none h=0.5 v=dot c= black;
run;





proc import datafile = "C:\Users\GWANGRYUL\Desktop\���͵�\ī�嵥����1\data04\result2.csv" dbms = csv out = result2 replace;
run;

/*Ward������� �����м�*/
proc cluster data = result2 method =  ward outtree = result2out standard pseudo rsq;
var n avg hour2_p -- hour22_p;
id store_id;
copy n avg hour2_p--hour22_p;
run;

/*���������׸� �׸���, ncl= �����ǰ���8������*/
proc tree data = result2out ncl=8 out=cluster_2;
id store_id;
run;
proc print data = cluster_2;
run;
/*������ ���� �� ����*/
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

/*������ ������跮*/
proc means data = cluster_2d;
by cluster;
run;



/*�ּ��км�*/
proc princomp data = cluster_2d out = prin2;
var n avg hour2_p -- hour22_p;
run;

/*2���� ������ -> �� �ȳ��ͼ� 3������ �ʿ��Ұ� ����*/
proc gplot data = prin2;
plot prin2*prin1 = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= pupple;
symbol5  i= none h=0.5 v=dot c= blue;
run;







proc import datafile = "C:\Users\GWANGRYUL\Desktop\���͵�\ī�嵥����1\data04\result3.csv" dbms = csv out = result3 replace;
run;

/*Ward������� �����м�*/
proc cluster data = result3 method =  ward outtree = result3out standard pseudo rsq;
var n avg hour2_p -- hour22_p mean_install count_install_p ;
id store_id;
copy n avg hour2_p--hour22_p mean_install count_install_p;
run;

/*���������׸� �׸���, ncl= �����ǰ���9������*/
proc tree data = result3out ncl=9 out=cluster_3;
id store_id;
run;
proc print data = cluster_3;
run;
/*������ ���� �� ����*/
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

/*������ ������跮*/
proc means data = cluster_3d;
by cluster;
run;



/*�ּ��км�*/
proc princomp data = cluster_3d out = prin3;
var  n avg hour2_p -- hour22_p mean_install count_install_p ;
run;
/*2���� ������ -> �� �ȳ��ͼ� 3������ �ʿ��Ұ� ����*/
proc gplot data = prin3;
plot prin2*prin1 = cluster;
symbol1  i= none h=0.5 v=dot c= red;
symbol2  i= none h=0.5 v=dot c= orange;
symbol3  i= none h=0.5 v=dot c= yellow;
symbol4  i= none h=0.5 v=dot c= pupple;
symbol5  i= none h=0.5 v=dot c= blue;
run;
