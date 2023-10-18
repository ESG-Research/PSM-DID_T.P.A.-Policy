*生成did
replace treat=0 if treat==.
tab treat
*政策执行时间为2013年
gen time = (年份 >=2013) & !missing(年份) 
gen did=treat*time
*保留年份
keep if 年份 >=2001
*变量前处理
gen 金融支持=年末金融机构各项贷款余额/地区生产总值
gen lnpopu= ln(人口密度+1)
gen lnest=ln(固定资产合计+1)
gen ln公路运量=ln( 公路客运量+ 公路货运量+1)
zscore lnGDP 金融支持 lnpopu lnest ln公路运量
*被解释变量处理
zscore   地区生产总值增长率   第二产业增加值占GDP比重
gen lnGDP=ln( 地区生产总值+1)
zscore 产业增加值
gen Y=z_第一产业增加值+z_第三产业增加值+z_人均地区生产总值
zscore Y

*========================回归部分
*设定面板
xtset 行政区划代码 年份
*设定控制变量
global controls  z_金融支持 z_人口自然增长率 z_lnest z_ln公路运量  z_职工平均工资
*基准回归
reghdfe  z_Y did ,absorb( 行政区划代码 年份 )
est store a1
reghdfe  z_Y did $controls ,absorb( 行政区划代码 年份 )
est store a2
*esttab					
esttab a1 a2, replace b(%6.4f) t(%6.4f) nogap ar2 star(* 0.1 ** 0.05 *** 0.01) nogap s(r2_a N) 
*outreg2
outreg2 [a1 a2  ] using 基准回归.rtf,tstat replace e(r2_a) addtext(code FE, YES,Year FE, YES)

*平行趋势检验
gen policy = 年份 - 2013 
tab policy
replace policy = -7 if policy < -7
replace policy = 6 if policy > 6

forvalues i=7(-1)1{
	gen pre`i'=(policy==-`i')
}

gen current= (policy==0)

forvalues i=1(1)6{
	gen post`i'=(policy==`i')
}

drop pre1 
*将政策前第一期作为基准组
reghdfe  z_Y pre* current post* $controls ,absorb( 行政区划代码  )
est store a1
*esttab					
esttab a1 , replace b(%6.4f) t(%6.4f) nogap ar2 star(* 0.1 ** 0.05 *** 0.01) nogap s(r2_a N) 
*outreg2
outreg2 [a1   ] using 平行趋势检验.rtf,tstat replace e(r2_a) addtext(code FE, YES,Year FE, YES)

*平行趋势绘图
coefplot, baselevels ///
keep(pre* current post*) ///
vertical ///转置图形
coeflabels( pre7 = "-7" pre6 = "-6" pre5 = "-5" pre4 = "-4" pre3 = "-3" pre2 = "-2" pre1 = "-1" current = "0" post1 = "1" post2 = "2" post3 = "3"post4 = "4"  post5 = "5" post6 = "6") ///
yline(0,lcolor(edkblue*0.8)) ///加入y=0这条虚线
ylabel(-0.06(0.02)0.06) ///
xline(11, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
ylabel(,labsize(*0.75)) xlabel(,labsize(*0.75)) ///
ytitle("政策动态经济效应", size(small)) ///加入Y轴标题,大小small
xtitle("政策时点", size(small)) ///加入X轴标题，大小small 
addplot(line @b @at) ///增加点之间的连线
ciopts(lpattern(dash) recast(rcap) msize(medium)) ///CI为虚线上下封口
msymbol(circle_hollow) ///plot空心格式
scheme(s1mono)

*安慰剂检验
cap erase"simulations.dta",clear
permute did beta = _b[did] se = _se[did] df = e(df_r),reps(600)seed(100)saving("simulations.dta"):reghdfe  z_Y did $controls ,absorb( 行政区划代码 年份 )
* 回归系数
use "simulations.dta",clear
gen t_value = beta / se
gen p_value = 2 * ttail(df,abs(beta/se))
* 绘图 （P值和回归系数的密度函数图）
twoway (kdensity beta) (scatter p_value beta, msymbol(smcircle_hollow) mcolor(blue)), ///
title("Placebo Test") ///
title("Placebo Test") ///
xlabel(-0.2(0.1)0.2) ylabel(,angle(0)) ///
xline(0.169, lwidth(vthin) lp(shortdash)) xtitle("回归系数") ///
yline(0.1,lwidth(vthin) lp(dash)) ytitle(p value) ///
legend(label(1 "kdensity of estimates") label( 2 "p value")) ///
plotregion(style(none)) ///无边框
graphregion(color(white)) //白底

*异质性检验
tab 省份
gen 地区 = 1
replace 地区 = 2 if 省份 == "山西省" |省份 == "安徽省" |省份 == "江西省" |省份 == "河南省" |省份 == "湖北省" |省份 == "湖南省" |省份 == "吉林省" |省份 == "黑龙江省" 
replace 地区 = 3 if 省份 == "内蒙古自治区" |省份 == "重庆市" |省份 == "四川省" |省份 == "广西壮族自治区" |省份 == "贵州省" |省份 == "云南省" |省份 == "陕西省" |省份 == "青海省" |省份 == "宁夏回族自治区" |省份 == "甘肃省" |省份 == "西藏自治区" |省份 == "新疆维吾尔自治区" 
gen 中西部=1 if 地区 == 2
replace 中西部=0 if 地区 == 3

reghdfe  z_Y did $controls if  中西部 == 1  ,absorb( 行政区划代码 年份 )
est store a1
reghdfe  z_Y did $controls if  中西部 == 0 ,absorb( 行政区划代码 年份 )
est store a2
*esttab					
esttab a1 a2 , replace b(%6.4f) t(%6.4f) nogap ar2 star(* 0.1 ** 0.05 *** 0.01) nogap s(r2_a N) 
*outreg2
outreg2 [a1 a2  ] using 异质性检验-分中西部.rtf,tstat replace e(r2_a) addtext(code FE, YES,Year FE, YES)
*组件系数差异检验
bdiff, group(中西部) model (reghdfe  z_Y did $controls   ,absorb( 行政区划代码 年份 ))  reps(1000) detail

*psm
*设定控制变量
global controls  z_金融支持 z_人口自然增长率 z_lnest z_ln公路运量  z_职工平均工资
set seed 0001
gen tmp=runiform()  
sort tmp
psmatch2  did    $controls ,outcome(z_Y) logit neighbor(1) common ate ties  
* 匹配效果检验
pstest $controls , both gra

*psm-did
reghdfe  z_Y did $controls if _support==1 ,absorb( 行政区划代码 年份 )
est store a1
*esttab					
esttab a1  , replace b(%6.4f) t(%6.4f) nogap ar2 star(* 0.1 ** 0.05 *** 0.01) nogap s(r2_a N) 
*outreg2
outreg2 [a1   ] using psm-did.rtf,tstat replace e(r2_a) addtext(IND FE, YES,Year FE, YES,PRO FE, YES)

