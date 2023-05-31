* 安装apc 包
//ssc install apc

*糖尿病
clear

*更改工作路径
cd C:\Users\35111\Desktop\apc_ie
clear

//1.1  总体：SDI水平
*导入数据
import delimited "data_for_apc.csv"


*运行apc模型
apc_ie case, age(age)period(period)cohort(cohort)family(poisson)link(log)exposure(pop)




 