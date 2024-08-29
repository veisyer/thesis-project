*DATA MASTER
use "D:\College\Skripsi\Stata Progress\Panel\Versi HHID Fix.dta"



****************************VARIABLES*****************************
**# hhid14+07
gen hhid=hhid14+hhid07

**# Household Size
gen x=1
bys hhid Tahun : egen hhsize = sum(x)

**#pendidikan#**

*Tahun 
gen pendidikan  = .

**SD/MI
replace pendidikan  = 1 if dl06==02 & dl07==1    
replace pendidikan  = 2 if dl06==02 & dl07==2    
replace pendidikan  = 3 if dl06==02 & dl07==3    
replace pendidikan  = 4 if dl06==02 & dl07==4    
replace pendidikan  = 5 if dl06==02 & dl07==5    
replace pendidikan  = 6 if dl06==02 & dl07==6    
replace pendidikan  = 6 if dl06==02 & dl07==7    
replace pendidikan  = 1 if dl06==72 & dl07==1    
replace pendidikan  = 2 if dl06==72 & dl07==2    
replace pendidikan  = 3 if dl06==72 & dl07==3    
replace pendidikan  = 4 if dl06==72 & dl07==4    
replace pendidikan  = 5 if dl06==72 & dl07==5    
replace pendidikan  = 6 if dl06==72 & dl07==6    
replace pendidikan  = 6 if dl06==72 & dl07==7    
**SMP/MTs
replace pendidikan  = 7 if dl06==03 & dl07==1    
replace pendidikan  = 8 if dl06==03 & dl07==2    
replace pendidikan  = 9 if dl06==03 & dl07==3    
replace pendidikan  = 9 if dl06==03 & dl07==7    
replace pendidikan  = 7 if dl06==04 & dl07==1    
replace pendidikan  = 8 if dl06==04 & dl07==2    
replace pendidikan  = 9 if dl06==04 & dl07==3    
replace pendidikan  = 9 if dl06==04 & dl07==7    
replace pendidikan  = 7 if dl06==73 & dl07==1    
replace pendidikan  = 8 if dl06==73 & dl07==2    
replace pendidikan  = 9 if dl06==73 & dl07==3    
replace pendidikan  = 9 if dl06==73 & dl07==7    
**SMU/SMK/MA
replace pendidikan  = 10 if dl06==05 & dl07==1    
replace pendidikan  = 11 if dl06==05 & dl07==2    
replace pendidikan  = 12 if dl06==05 & dl07==3    
replace pendidikan  = 12 if dl06==05 & dl07==7    
replace pendidikan  = 10 if dl06==06 & dl07==1    
replace pendidikan  = 11 if dl06==06 & dl07==2    
replace pendidikan  = 12 if dl06==06 & dl07==3    
replace pendidikan  = 12 if dl06==06 & dl07==4    
replace pendidikan  = 12 if dl06==06 & dl07==7    
**S1
replace pendidikan  = 13 if dl06==61 & dl07==1    
replace pendidikan  = 14 if dl06==61 & dl07==2    
replace pendidikan  = 15 if dl06==61 & dl07==3    
replace pendidikan  = 15 if dl06==61 & dl07==4    
replace pendidikan  = 16 if dl06==61 & dl07==7    
replace pendidikan  = 13 if dl06==13 & dl07==1    
replace pendidikan  = 14 if dl06==13 & dl07==2    
replace pendidikan  = 15 if dl06==13 & dl07==3    
replace pendidikan  = 16 if dl06==13 & dl07==4    
replace pendidikan  = 16 if dl06==13 & dl07==7    
**S2
replace pendidikan  = 17 if dl06==62 & dl07==1    
replace pendidikan  = 18 if dl06==62 & dl07==2    
replace pendidikan  = 18 if dl06==62 & dl07==7    
replace pendidikan  = 18 if dl06==62    
**S3
replace pendidikan  = 22 if dl06==63 
gen pendidikananak = pendidikan if ar02b==3 | ar02b==4
bys hhid Tahun: egen pendidikantertinggi = max(pendidikananak)

**# makanan
egen makanan = rowtotal(ks02A ks02AA ks02B ks02BA ks02C ks02CA ks02D ks02DA ks02E ks02EA ks02F ks02FA ks02G ks02GA ks02H ks02HA ks02I ks02IA ks02IB ks02J ks02K ks02L ks02M ks02N ks02OA ks02OB ks02P ks02Q ks02R ks02S ks02T ks02U ks02V ks02W ks02X ks02Y ks02Z)
gen logmakanan = log(makanan)


**# Protein
egen protein = rowtotal(ks02K ks02L ks02M ks02N ks02OA ks02OB ks02P ks02Q)
gen logprotein = log(protein)
gen persenprotein = protein/makanan
*K = daging sapi, kambing, kerbau, dan sejenisnya
*L = daging ayam, bebek, dan sejenisnya
*M = ikan segar (ikan basah), kerang, udang, cumi-cumi, dan sejenisnya
*N = ikan asin, ikan asap
*OA = dendeng, abon, daging kaleng, sarden, dll
*OB = tahu, tempe, oncom
*P = telur
*Q = susu segar, susu kaleng, susu bubuk, dan sejenisnya

**# Serat
egen serat = rowtotal(ks02F ks02G ks02H)
gen logserat = log(serat)
gen persenserat = serat/makanan
*F = Kangkung, mentimun, bayam, sawi, tomat, kol, katuk, buncis, kacang panjang dan sejenisnya.
*G = Kacang-kacangan, seperti kacang tanah,kacang hijau,kacang merah,kacang kedelai, dan sejenisnya.
*H = Buah-buahan seperti pepaya, mangga, pisang dan sejenisnya.

**# HEI
*HEI
*adequacy
egen ikandaging = rowtotal(ks02K ks02L ks02M ks02N ks02OA)

foreach kecukupan in ks02F ks02H ks02Q ikandaging ks02G ks02Y {
	gen proporsi`kecukupan'=`kecukupan'/makanan
	xtile status`kecukupan' = proporsi`kecukupan', n(4)
	*kuarter 2 3 4 cukup, 1 gak
	gen quantilatas`kecukupan' = 0
	replace quantilatas`kecukupan' = 1 if status`kecukupan'==2 | status`kecukupan'==3 | status`kecukupan'==4
}


*moderation (kalori dari lemak ga diitung)
egen gula = rowtotal(ks02AA ks02W)
egen lemakjenuh = rowtotal(ks02K ks02Y ks02X ks02Q)
foreach batasan in ks02EA ks02FA gula ks02S lemakjenuh {
	gen proporsi`batasan'=`batasan'/makanan
	xtile status`batasan' = proporsi`batasan', n(4)
	*kuarter 1 2 3 masih ok, 4 over
	gen quantilbawah`batasan' = 0
	replace quantilbawah`batasan' = 1 if status`batasan'==1 | status`batasan'==2 | status`batasan'==3
}

egen HEI = rowtotal(quantil*)
egen HEIkecukupan = rowtotal(quantilatas*)
egen HEIbatasan = rowtotal(quantilbawah*)

**# Urban/Rural
recode sc05(2=0), gen (urban)
format %12.0f urban

**# PENDAPATAN (courtesy to Yusrin Amalia S.E. https://gist.github.com/pangudijr/41422bceb805f0233120438ca8fce3cc)
gen pendapatan =ar15b  
replace pendapatan =. if ar15b>999999990 
replace pendapatan =0 if ar15bx==6  
replace pendapatan =0 if ar15a==3   
*# pendapatan kepala rumah tangga	
gen pendapatankrt =pendapatan  if ar02b==1 
bys hhid Tahun : egen pendapatan_krt =max(pendapatankrt )
gen lnpendapatan_krt =ln(pendapatan_krt )
*# pendapatan per kapita + household size
bys hhid Tahun : egen total_pendapatanRT =sum(ar15b)
gen pendapatanperkapita  = total_pendapatanRT /hhsize 
gen lnpendapatanperkapita  = ln(pendapatanperkapita )
replace lnpendapatanperkapita  = pendapatanperkapita  if pendapatanperkapita ==0
*# pendapatan orang tua
gen pendapatankrt2 = pendapatan if ar02b==2
egen pendapatanortu =rowtotal(pendapatankrt pendapatankrt2)
bys hhid Tahun : egen pendapatan_ortu =sum(pendapatanortu)
gen lnpendapatan_ortu = log(pendapatan_ortu)

**# Total TK dalam keluarga
gen nontenagakerja = .
replace nontenagakerja = 1 if (pendapatan==0 & usia>=15 & usia<=65) | usia<15 | usia>65 
replace nontenagakerja = 0 if pendapatan>0 & usia>=15 & usia<=65 
bys hhid Tahun: egen nontenagakerja_dk = sum(nontenagakerja)


**# Family Income [HARUS DARI VERSI 4 MASING-MASING, REPLACE   KE   DULU]
bys hhid Tahun : egen pendapatan_RT  = sum(pendapatan )
gen lntotal_pendapatanRT  = log(total_pendapatanRT )

**# Status Ekonomi [HARUS DARI VERSI 4 MASING-MASING, REPLACE   KE   DULU]
xtile statusekon =total_pendapatanRT , n(10)
	foreach i of numlist 1/10{
	gen desil_`i' =0
	replace desil_`i' = 1 if statusekon==`i'
	}
	

**# Region Fixed Effect (Kota)
egen kota = rowtotal(sc020707 sc02_14_14)







***********************************REGRESSION*************************************

/*encode hhid, gen(HHID)

xtset HHID Tahun
*/
est clear

xtset HHID Tahun

///////////////////////////// Fixed Effect (NOTE: GA VCE(ROBUST) SOALNYA BUAT HAUSMAN)

**# PROTEIN
xtreg logprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe 
est store FE1C

xtreg persenprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe 
est store FE1F

**# SERAT
xtreg logserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe 
est store FE2C

xtreg persenserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe 
est store FE2F

**# HEI
xtreg HEI pendidikantertinggi lntotal_pendapatanRT urban nontenagakerja_dk i.Tahun i.kota, fe 
est store HEI


///////////////////////////// OLS

**# PROTEIN
reg logprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota
est store OLS1

reg persenprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota
est store OLS2

**# SERAT
reg logserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota
est store OLS3

reg persenserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota
est store OLS4

**# HEI
reg HEI pendidikantertinggi lntotal_pendapatanRT urban nontenagakerja_dk i.Tahun i.kota
est store HEIOLS




////////////////////////////////////
**# output
outreg2 [FE1C FE1F OLS1 OLS2] using protein.xls, keep(pendidikantertinggi lntotal_pendapatanRT) excel replace label dec(4)
outreg2 [FE2C FE2F OLS3 OLS4] using serat.xls, keep(pendidikantertinggi lntotal_pendapatanRT) excel replace label dec(4)
outreg2 [HEI HEIOLS] using HEI.xls, keep(pendidikantertinggi lntotal_pendapatanRT) excel replace label dec(4)


**# Regression RE
xtreg logprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, re
est store RE1C

xtreg persenprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, re
est store RE1F

xtreg logserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, re
est store RE2C

xtreg persenserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, re
est store RE2F

xtreg HEI pendidikantertinggi lntotal_pendapatanRT urban nontenagakerja_dk i.Tahun i.kota, re
est store HEIRE

**# Hausman (belum fix)
hausman FE1C RE1C
hausman FE1F RE1F
hausman FE2C RE2C
hausman FE2F RE2F
hausman HEI HEIRE


**# modified wald
xtreg logprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
xttest3
xtreg persenprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
xttest3
xtreg logserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
xttest3
xtreg persenserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
xttest3
xtreg HEI pendidikantertinggi lntotal_pendapatanRT urban nontenagakerja_dk i.Tahun i.kota, fe
xttest3

**# bp/cw
xtreg logprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
hettest
xtreg persenprotein pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
hettest
xtreg logserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
hettest
xtreg persenserat pendidikantertinggi lntotal_pendapatanRT hhsize urban nontenagakerja_dk i.Tahun i.kota, fe
hettest
xtreg HEI pendidikantertinggi lntotal_pendapatanRT urban nontenagakerja_dk i.Tahun i.kota, fe
hettest




  


