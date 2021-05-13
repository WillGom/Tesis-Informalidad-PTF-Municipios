

********************************************************************************
******************** CODGIO PARA CENSO ECONOMICO 2014 **************************
********************************************************************************
*Por: Williams de Jesús Gómez Cerino *******************************************
*Folio de investigación: 1895 **************************************************
********************************************************************************

*AQUI SE DEBE ESTABLECER LA CARPETA DE DIRECTORIO
*.....................................................

*AQUI SE DEBE CARGAR LA BASE
*............................

*PASANDO VARIABLES A MINUSCULAS
rename *, lower

*CONSERVANDO VARIABLES DE INTERES
keep e03 e04 e17 h101a h203a h020a i100a i200a j010a j203a k610a k620a ///
q000a j300a a131a k050a g111a

*RENOMBRANDO VARIABLES
rename e03 estado
rename e04 muni
rename e17 scian_6
rename h101a azul
rename h203a blanco
rename h020a familia
rename i100a outsour
rename i200a no_asa
rename j010a s_azul
rename j203a s_blanco
rename k610a g_outsour
rename k620a g_no_asa
rename q000a act_fijos
rename j300a g_seg_social
rename a131a v_agregado
rename k050a g_cap_rent
rename g111a anio_cre

*GENERANDO VARIABLES DE SUBINDUSTRIAS
tostring scian_6, replace
gen scian_5=substr(scian_6,1,5)
gen scian_4=substr(scian_6,1,4)
gen scian_3=substr(scian_6,1,3)
gen scian_2=substr(scian_6,1,2)

*CONSERVANDO INDUSTRIAS DE INTERES
keep if (scian_2=="43" | scian_2=="46" | scian_2=="31" | scian_2=="32") | ///
scian_2=="33" | scian_2=="51" | scian_2=="53" | scian_2=="54" | ///
scian_2=="55" | scian_2=="56" | scian_2=="61" | scian_2=="62" | ///
scian_2=="71" | scian_2=="72" | scian_2=="81")
drop if (scian_6=="312142" | scian_6=="312221" | scian_6=="321920" | ///
scian_6=="321999" | scian_6=="322122" | scian_6=="324110" | scian_6=="327213" | ///
scian_6=="327310" | scian_6=="334110" | scian_6=="512112" | scian_6=="513120" | ///
scian_6=="531113" | scian_6=="531114" | scian_6=="532411" | scian_6=="561110" | ///
scian_6=="611311" | scian_6=="622111" | scian_6=="711311" | scian_6=="713941" | ///
scian_6=="721111")
drop if scian_4="5613"
drop scian_6 scian_5 scian_4

*DEFINIENDO CONSTANTES
global R=0.1
global sigma=2

*VARIABLE DE PRODUCCION
gen PY_msi= v_agregado

*QUITANDO EMPRESAS CON PY_msi NO POSITIVO
drop if PY_msi<=0

*GENERANDO VARIABLE DE STOCK DE CAPITAL
gen K_msi= act_fijos+g_cap_rent/$R

*GENERANDO VARIABLE DE TRABAJADORES
gen trab_msi= azul+blanco+familia+no_asa+outsour

*GENERANDO VARIABLE DE SALARIO IMPUTADO
gen tot_trab=azul+blanco
gen tot_sal=s_azul+s_blanco
gen menor_10=0
replace menor_10=1 if trab_msi<=10
*Paso 1
bysort scian_3 estado menor_10: egen suma_trab1=sum(tot_trab)
bysort scian_3 estado menor_10: egen suma_sal1=sum(tot_sal)
gen imputado1=suma_sal1/suma_trab1
*Paso 2
bysort scian_3 menor_10 : egen suma_trab2=sum(tot_trab)
bysort scian_3 menor_10 : egen suma_sal2=sum(tot_sal)
gen imputado2=suma_sal2/suma_trab2
*Paso 3
bysort menor_10 : egen suma_trab3=sum(tot_trab)
bysort menor_10 : egen suma_sal3=sum(tot_sal)
gen imputado3=suma_sal3/suma_trab3
*Conjuntando
gen s_imputado=imputado1
replace s_imputado=imputado2 if s_imputado==.
replace s_imputado=imputado3 if s_imputado==.
drop tot_trab tot_sal menor_10 suma_trab* suma_sal* imputado*

*GENERANDO VARIABLE DE WAGE BILL
gen wL_msi= s_azul+s_blanco+g_outsour+g_no_asa
replace wL_msi=s_azul+s_blanco+s_imputado*(familia+outsour+no_asa) ///
if trab_msi<=10

*GUARDANDO BASE PARA FUTUROS CALCULOS
save base_general_14, replace



********************************************************************************
********************************************************************************
*********************** CALCULOS A NIVEL MUNICIPIO *****************************
********************************************************************************
********************************************************************************

********************************************************************************
****************** PRODUCTIVIDAD POR INDUSTRIAS A 2 DIGITOS ********************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*DEFINIENDO CONSTANTES
global R=0.1
global sigma=2

*ESTABLECIENDO VARIABLE DE DIGITOS DE SCIAN
gen industria=scian_2

*GENERANDO VARIABLE DE PARTICIPACION DEL CAPITAL
gen a_s=0.532192817
replace a_s=0.550378966 if scian_2=="43"
replace a_s=0.445297335 if scian_2=="46"
replace a_s=0.654405449 if scian_2=="51"
replace a_s=0.945397676 if scian_2=="53"
replace a_s=0.336800924 if scian_2=="54"
replace a_s=0.139881139 if scian_2=="55"
replace a_s=0.276808352 if scian_2=="56"
replace a_s=0.240317849 if scian_2=="61"
replace a_s=0.189028486 if scian_2=="62"
replace a_s=0.494388356 if scian_2=="71"
replace a_s=0.375928411 if scian_2=="72"
replace a_s=0.276227821 if scian_2=="81"

*GENERANDO VARIABLE DE AGRUPAMIENTO DE INDUSTRIAS POR MUNICIPIO
egen msi=group(industria estado muni)

*ELIMINANDO INDUSTRIAS CON MENOS DE 10 ESTABLECIMIENTOS EN EL MUNICIPIO
bysort msi : egen num_estab=count(PY_msi)
drop if num_estab<=10

*GENERANDO VARIABLE DE PRODUCTIVIDAD FISICA
gen A_msi=(PY_msi^($sigma/($sigma-1)))/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR DE PRODUCTIVIDAD
gen TFPR_msi=PY_msi/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL MUNICIPIO
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN MUNICIPIO
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL MUNICIPIO
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL MUNICIPIO
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*ELIMINANDO LOS PERCENTILES 1 Y 99
gen ln_TFPR_ms=log(TFPR_msi/TFPR_ms_barra)
gen ln_A_ms=log(A_msi/A_ms_barra)
bysort msi: egen orden1=rank(ln_TFPR_ms)
bysort msi: egen orden2=rank(ln_A_ms)
bysort msi: gen tot=_N
gen percen1=orden1/tot
gen percen2=orden2/tot
drop if (percen1<=0.01 | percen1>=0.99)
drop if (percen2<=0.01 | percen2>=0.99)

*QUITANDO VARIABLES QUE DEBEN VOLVER A CALCULARSE
drop wL_ms K_ms PY_ms TFPR_ms_barra A_ms_barra sumando_A_ms base_A_ms

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN MUNICIPIO (OTRA VEZ)
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*CALCULANDO PRODUCTIVIDAD DE LA INDUSTRIA EN EL MUNICIPIO
gen sumando_TFP_ms=(A_msi*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi : egen base_TPF_ms=sum(sumando_TFP_ms)
gen pseudo_kappa_ms=1/(PY_ms^(1/($sigma-1)))
gen TFP_ms_1=pseudo_kappa_ms*(base_TPF_ms^(1/($sigma-1)))
gen TFP_ms_levy=base_TPF_ms^(1/($sigma-1))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA ECONOMIA DEL MUNICIPIO
bysort estado muni: egen Y=sum(PY_msi)

*GENERANDO VARIABLE DE LA PARTICIPACION DE LA INDUSTRIA EN EL MUNICIPIO
gen theta_s=PY_ms/Y

*GENERANDO VARIABLE DE GANANCIAS EN LA PRODUCTIVDAD DEL MUNICIPIO
gen sumando_Y_Y_ef=((A_msi/A_ms_barra)*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi: egen pre_productando=sum(sumando_Y_Y_ef)
gen productando_Y_Y_ef=pre_productando^(theta_s/($sigma-1))
bysort msi : gen contador=_n
replace productando_Y_Y_ef=1 if contador!=1
gen ln_prod_Y_Y_ef=log(productando_Y_Y_ef)
bysort estado muni: egen ln_Y_Y_ef=sum(ln_prod_Y_Y_ef)
gen Y_Y_ef=100*((1/exp(ln_Y_Y_ef))-1)

*CALCULANDO PRODUCTIVIDAD (1) DEL MUNICIPIO
gen productando_TFP_1=TFP_ms_1^theta_s
replace productando_TFP_1=1 if contador!=1
gen ln_prod_TFP_1= log(productando_TFP_1)
bysort estado muni : egen ln_TFP_1=sum(ln_prod_TFP_1)
gen TFP_1=exp(ln_TFP_1)

*CALCULANDO PRODUCTIVIDAD (levy) DEL MUNICIPIO
gen productando_TFP_levy=TFP_ms_levy^theta_s
replace productando_TFP_levy=1 if contador!=1
gen ln_prod_TFP_levy= log(productando_TFP_levy)
bysort estado muni : egen ln_TFP_levy=sum(ln_prod_TFP_levy)
gen TFP_levy=exp(ln_TFP_levy)

*OBTENIENDO BASE FINAL
collapse (mean) Y_Y_ef TFP_1 TFP_levy (sum) PY_msi, by(estado muni)

*GUARDANDO BASE FINAL
save TFP_muni_2dig_14, replace


********************************************************************************
****************** PRODUCTIVIDAD POR INDUSTRIAS A 3 DIGITOS ********************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*DEFINIENDO CONSTANTES
global R=0.1
global sigma=2

*ESTABLECIENDO VARIABLE DE DIGITOS DE SCIAN
gen industria=scian_3

*VARIABLE DE PARTICIPACION DEL CAPITAL A 2 DIGITOS
gen a_s=0.532192817
replace a_s=0.550378966 if scian_2=="43"
replace a_s=0.445297335 if scian_2=="46"
replace a_s=0.654405449 if scian_2=="51"
replace a_s=0.945397676 if scian_2=="53"
replace a_s=0.336800924 if scian_2=="54"
replace a_s=0.139881139 if scian_2=="55"
replace a_s=0.276808352 if scian_2=="56"
replace a_s=0.240317849 if scian_2=="61"
replace a_s=0.189028486 if scian_2=="62"
replace a_s=0.494388356 if scian_2=="71"
replace a_s=0.375928411 if scian_2=="72"
replace a_s=0.276227821 if scian_2=="81"

*VARIABLE DE PARTICIPACION DEL CAPITAL A 3 DIGITOS
replace a_s=0.418835111 if scian_3=="321"
replace a_s=0.488834642 if scian_3=="327"
replace a_s=0.504589953 if scian_3=="331"
replace a_s=0.356129661 if scian_3=="332"
replace a_s=0.43932401 if scian_3=="333"
replace a_s=0.474947141 if scian_3=="334"
replace a_s=0.430427132 if scian_3=="335"
replace a_s=0.516829312 if scian_3=="336"
replace a_s=0.254696156 if scian_3=="337"
replace a_s=0.41291478 if scian_3=="339"
replace a_s=0.585869756 if scian_3=="312"
replace a_s=0.313825022 if scian_3=="313"
replace a_s=0.19768922 if scian_3=="316"
replace a_s=0.47879821 if scian_3=="322"
replace a_s=0.346833428 if scian_3=="323"
replace a_s=0.876488678 if scian_3=="324"
replace a_s=0.711642326 if scian_3=="325"
replace a_s=0.419703691 if scian_3=="326"
replace a_s=0.430831485 if scian_3=="468"
replace a_s=0.371588601 if scian_3=="461"
replace a_s=0.526751986 if scian_3=="511"
replace a_s=0.64092375 if scian_3=="512"
replace a_s=0.723081978 if scian_3=="515"
replace a_s=0.655542003 if scian_3=="518"
replace a_s=0.955944346 if scian_3=="531"
replace a_s=0.83000329 if scian_3=="533"
replace a_s=0.261680105 if scian_3=="561"
replace a_s=0.432926688 if scian_3=="562"
replace a_s=0.23627054 if scian_3=="621"
replace a_s=0.166855227 if scian_3=="622"
replace a_s=0.101318685 if scian_3=="623"
replace a_s=0.112891141 if scian_3=="624"
replace a_s=0.557067772 if scian_3=="711"
replace a_s=0.557067772 if scian_3=="712"
replace a_s=0.405034388 if scian_3=="713"
replace a_s=0.490473961 if scian_3=="721"
replace a_s=0.329506301 if scian_3=="722"

*GENERANDO VARIABLE DE AGRUPAMIENTO DE INDUSTRIAS POR MUNICIPIO
egen msi=group(industria estado muni)

*ELIMINANDO INDUSTRIAS CON MENOS DE 10 ESTABLECIMIENTOS EN EL MUNICIPIO
bysort msi : egen num_estab=count(PY_msi)
drop if num_estab<=10

*GENERANDO VARIABLE DE PRODUCTIVIDAD FISICA
gen A_msi=(PY_msi^($sigma/($sigma-1)))/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR DE PRODUCTIVIDAD
gen TFPR_msi=PY_msi/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL MUNICIPIO
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN MUNICIPIO
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL MUNICIPIO
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL MUNICIPIO
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*ELIMINANDO LOS PERCENTILES 1 Y 99
gen ln_TFPR_ms=log(TFPR_msi/TFPR_ms_barra)
gen ln_A_ms=log(A_msi/A_ms_barra)
bysort msi: egen orden1=rank(ln_TFPR_ms)
bysort msi: egen orden2=rank(ln_A_ms)
bysort msi: gen tot=_N
gen percen1=orden1/tot
gen percen2=orden2/tot
drop if (percen1<=0.01 | percen1>=0.99)
drop if (percen2<=0.01 | percen2>=0.99)

*QUITANDO VARIABLES QUE DEBEN VOLVER A CALCULARSE
drop wL_ms K_ms PY_ms TFPR_ms_barra A_ms_barra sumando_A_ms base_A_ms

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN MUNICIPIO (OTRA VEZ)
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL MUNICIPIO (OTRA VEZ)
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*CALCULANDO PRODUCTIVIDAD DE LA INDUSTRIA EN EL MUNICIPIO
gen sumando_TFP_ms=(A_msi*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi : egen base_TPF_ms=sum(sumando_TFP_ms)
gen pseudo_kappa_ms=1/(PY_ms^(1/($sigma-1)))
gen TFP_ms_1=pseudo_kappa_ms*(base_TPF_ms^(1/($sigma-1)))
gen TFP_ms_levy=base_TPF_ms^(1/($sigma-1))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA ECONOMIA DEL MUNICIPIO
bysort estado muni: egen Y=sum(PY_msi)

*GENERANDO VARIABLE DE LA PARTICIPACION DE LA INDUSTRIA EN EL MUNICIPIO
gen theta_s=PY_ms/Y

*GENERANDO VARIABLE DE GANANCIAS EN LA PRODUCTIVDAD DEL MUNICIPIO
gen sumando_Y_Y_ef=((A_msi/A_ms_barra)*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi: egen pre_productando=sum(sumando_Y_Y_ef)
gen productando_Y_Y_ef=pre_productando^(theta_s/($sigma-1))
bysort msi : gen contador=_n
replace productando_Y_Y_ef=1 if contador!=1
gen ln_prod_Y_Y_ef=log(productando_Y_Y_ef)
bysort estado muni: egen ln_Y_Y_ef=sum(ln_prod_Y_Y_ef)
gen Y_Y_ef=100*((1/exp(ln_Y_Y_ef))-1)

*CALCULANDO PRODUCTIVIDAD (1) DEL MUNICIPIO
gen productando_TFP_1=TFP_ms_1^theta_s
replace productando_TFP_1=1 if contador!=1
gen ln_prod_TFP_1= log(productando_TFP_1)
bysort estado muni : egen ln_TFP_1=sum(ln_prod_TFP_1)
gen TFP_1=exp(ln_TFP_1)

*CALCULANDO PRODUCTIVIDAD (levy) DEL MUNICIPIO
gen productando_TFP_levy=TFP_ms_levy^theta_s
replace productando_TFP_levy=1 if contador!=1
gen ln_prod_TFP_levy= log(productando_TFP_levy)
bysort estado muni : egen ln_TFP_levy=sum(ln_prod_TFP_levy)
gen TFP_levy=exp(ln_TFP_levy)

*OBTENIENDO BASE FINAL
collapse (mean) Y_Y_ef TFP_1 TFP_levy (sum) PY_msi, by(estado muni)

*GUARDANDO BASE FINAL
save TFP_muni_3dig_14, replace


********************************************************************************
************************ GENERANDO OTRAS VARIABLES *****************************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*VARIABLE DE INFORMALIDAD DEL MUNICIPIO
bysort estado muni : egen seg_social_m=sum(g_seg_social)
bysort estado muni : egen wL_m=sum(wL_msi)
gen infor_m=seg_social_m/wL_m

*VARIABLE DE SECTOR ASALARIADO DEL MUNICIPIO
gen asa_si=azul+blanco
bysort estado muni : egen asa_m=sum(asa_si)
bysort estado muni : egen trab_m=sum(trab_msi)
gen sector_asa_m=asa_m/trab_m

*VARIABLE DE TAMAÑO PROMEDIO DE LAS EMPRESAS EN EL MUNICIPIO
bysort estado muni : egen num_m=mean(trab_msi)

*VARIABLE DE LEGALIDAD DEL MUNICIPIO
gen g_asa_si=s_azul+s_blanco
bysort estado muni : egen g_asa_m=sum(g_asa_si)
gen legal_m=seg_social_m/g_asa_m

*VARIABLE DE EDAD PROMEDIO DE LAS EMPRESAS EN EL MUNICIPIO
gen edad_msi=2020-anio_cre
bysort estado muni : egen edad_m=mean(edad_msi)

*CONSERVANDO VARIABLES DE INTERES
collapse (mean) infor_m sector_asa_m num_m legal_m edad_m, by(estado muni)

*GUARDANDO BASE
save variables_muni_14, replace


********************************************************************************
********************************************************************************
************************* CALCULOS A NIVEL ESTADO ******************************
********************************************************************************
********************************************************************************

********************************************************************************
****************** PRODUCTIVIDAD POR INDUSTRIAS A 2 DIGITOS ********************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*DEFINIENDO CONSTANTES
global R=0.1
global sigma=2

*ESTABLECIENDO VARIABLE DE DIGITOS DE SCIAN
gen industria=scian_2

*GENERANDO VARIABLE DE PARTICIPACION DEL CAPITAL
gen a_s=0.532192817
replace a_s=0.550378966 if scian_2=="43"
replace a_s=0.445297335 if scian_2=="46"
replace a_s=0.654405449 if scian_2=="51"
replace a_s=0.945397676 if scian_2=="53"
replace a_s=0.336800924 if scian_2=="54"
replace a_s=0.139881139 if scian_2=="55"
replace a_s=0.276808352 if scian_2=="56"
replace a_s=0.240317849 if scian_2=="61"
replace a_s=0.189028486 if scian_2=="62"
replace a_s=0.494388356 if scian_2=="71"
replace a_s=0.375928411 if scian_2=="72"
replace a_s=0.276227821 if scian_2=="81"

*GENERANDO VARIABLE DE AGRUPAMIENTO DE INDUSTRIAS POR ESTADO
egen msi=group(industria estado)

*ELIMINANDO INDUSTRIAS CON MENOS DE 10 ESTABLECIMIENTOS EN EL ESTADO
bysort msi : egen num_estab=count(PY_msi)
drop if num_estab<=10

*GENERANDO VARIABLE DE PRODUCTIVIDAD FISICA
gen A_msi=(PY_msi^($sigma/($sigma-1)))/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR DE PRODUCTIVIDAD
gen TFPR_msi=PY_msi/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL ESTADO
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN ESTADO
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL ESTADO
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL ESTADO
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*ELIMINANDO LOS PERCENTILES 1 Y 99
gen ln_TFPR_ms=log(TFPR_msi/TFPR_ms_barra)
gen ln_A_ms=log(A_msi/A_ms_barra)
bysort msi: egen orden1=rank(ln_TFPR_ms)
bysort msi: egen orden2=rank(ln_A_ms)
bysort msi: gen tot=_N
gen percen1=orden1/tot
gen percen2=orden2/tot
drop if (percen1<=0.01 | percen1>=0.99)
drop if (percen2<=0.01 | percen2>=0.99)

*QUITANDO VARIABLES QUE DEBEN VOLVER A CALCULARSE
drop wL_ms K_ms PY_ms TFPR_ms_barra A_ms_barra sumando_A_ms base_A_ms

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN ESTADO (OTRA VEZ)
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*CALCULANDO PRODUCTIVIDAD DE LA INDUSTRIA EN EL ESTADO
gen sumando_TFP_ms=(A_msi*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi : egen base_TPF_ms=sum(sumando_TFP_ms)
gen pseudo_kappa_ms=1/(PY_ms^(1/($sigma-1)))
gen TFP_ms_1=pseudo_kappa_ms*(base_TPF_ms^(1/($sigma-1)))
gen TFP_ms_levy=base_TPF_ms^(1/($sigma-1))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA ECONOMIA DEL ESTADO
bysort estado : egen Y=sum(PY_msi)

*GENERANDO VARIABLE DE LA PARTICIPACION DE LA INDUSTRIA EN EL ESTADO
gen theta_s=PY_ms/Y

*GENERANDO VARIABLE DE GANANCIAS EN LA PRODUCTIVDAD DEL ESTADO
gen sumando_Y_Y_ef=((A_msi/A_ms_barra)*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi: egen pre_productando=sum(sumando_Y_Y_ef)
gen productando_Y_Y_ef=pre_productando^(theta_s/($sigma-1))
bysort msi : gen contador=_n
replace productando_Y_Y_ef=1 if contador!=1
gen ln_prod_Y_Y_ef=log(productando_Y_Y_ef)
bysort estado : egen ln_Y_Y_ef=sum(ln_prod_Y_Y_ef)
gen Y_Y_ef=100*((1/exp(ln_Y_Y_ef))-1)

*CALCULANDO PRODUCTIVIDAD (1) DEL ESTADO
gen productando_TFP_1=TFP_ms_1^theta_s
replace productando_TFP_1=1 if contador!=1
gen ln_prod_TFP_1= log(productando_TFP_1)
bysort estado : egen ln_TFP_1=sum(ln_prod_TFP_1)
gen TFP_1=exp(ln_TFP_1)

*CALCULANDO PRODUCTIVIDAD (levy) DEL ESTADO
gen productando_TFP_levy=TFP_ms_levy^theta_s
replace productando_TFP_levy=1 if contador!=1
gen ln_prod_TFP_levy= log(productando_TFP_levy)
bysort estado : egen ln_TFP_levy=sum(ln_prod_TFP_levy)
gen TFP_levy=exp(ln_TFP_levy)

*OBTENIENDO BASE FINAL
collapse (mean) Y_Y_ef TFP_1 TFP_levy (sum) PY_msi, by(estado)

*GUARDANDO BASE FINAL
save TFP_edo_2dig_14, replace


********************************************************************************
****************** PRODUCTIVIDAD POR INDUSTRIAS A 3 DIGITOS ********************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*DEFINIENDO CONSTANTES
global R=0.1
global sigma=2

*ESTABLECIENDO VARIABLE DE DIGITOS DE SCIAN
gen industria=scian_3

*VARIABLE DE PARTICIPACION DEL CAPITAL A 2 DIGITOS
gen a_s=0.532192817
replace a_s=0.550378966 if scian_2=="43"
replace a_s=0.445297335 if scian_2=="46"
replace a_s=0.654405449 if scian_2=="51"
replace a_s=0.945397676 if scian_2=="53"
replace a_s=0.336800924 if scian_2=="54"
replace a_s=0.139881139 if scian_2=="55"
replace a_s=0.276808352 if scian_2=="56"
replace a_s=0.240317849 if scian_2=="61"
replace a_s=0.189028486 if scian_2=="62"
replace a_s=0.494388356 if scian_2=="71"
replace a_s=0.375928411 if scian_2=="72"
replace a_s=0.276227821 if scian_2=="81"

*VARIABLE DE PARTICIPACION DEL CAPITAL A 3 DIGITOS
replace a_s=0.418835111 if scian_3=="321"
replace a_s=0.488834642 if scian_3=="327"
replace a_s=0.504589953 if scian_3=="331"
replace a_s=0.356129661 if scian_3=="332"
replace a_s=0.43932401 if scian_3=="333"
replace a_s=0.474947141 if scian_3=="334"
replace a_s=0.430427132 if scian_3=="335"
replace a_s=0.516829312 if scian_3=="336"
replace a_s=0.254696156 if scian_3=="337"
replace a_s=0.41291478 if scian_3=="339"
replace a_s=0.585869756 if scian_3=="312"
replace a_s=0.313825022 if scian_3=="313"
replace a_s=0.19768922 if scian_3=="316"
replace a_s=0.47879821 if scian_3=="322"
replace a_s=0.346833428 if scian_3=="323"
replace a_s=0.876488678 if scian_3=="324"
replace a_s=0.711642326 if scian_3=="325"
replace a_s=0.419703691 if scian_3=="326"
replace a_s=0.430831485 if scian_3=="468"
replace a_s=0.371588601 if scian_3=="461"
replace a_s=0.526751986 if scian_3=="511"
replace a_s=0.64092375 if scian_3=="512"
replace a_s=0.723081978 if scian_3=="515"
replace a_s=0.655542003 if scian_3=="518"
replace a_s=0.955944346 if scian_3=="531"
replace a_s=0.83000329 if scian_3=="533"
replace a_s=0.261680105 if scian_3=="561"
replace a_s=0.432926688 if scian_3=="562"
replace a_s=0.23627054 if scian_3=="621"
replace a_s=0.166855227 if scian_3=="622"
replace a_s=0.101318685 if scian_3=="623"
replace a_s=0.112891141 if scian_3=="624"
replace a_s=0.557067772 if scian_3=="711"
replace a_s=0.557067772 if scian_3=="712"
replace a_s=0.405034388 if scian_3=="713"
replace a_s=0.490473961 if scian_3=="721"
replace a_s=0.329506301 if scian_3=="722"

*GENERANDO VARIABLE DE AGRUPAMIENTO DE INDUSTRIAS POR ESTADO
egen msi=group(industria estado)

*ELIMINANDO INDUSTRIAS CON MENOS DE 10 ESTABLECIMIENTOS EN EL ESTADO
bysort msi : egen num_estab=count(PY_msi)
drop if num_estab<=10

*GENERANDO VARIABLE DE PRODUCTIVIDAD FISICA
gen A_msi=(PY_msi^($sigma/($sigma-1)))/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR DE PRODUCTIVIDAD
gen TFPR_msi=PY_msi/((K_msi^a_s)*(wL_msi^(1-a_s)))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL ESTADO
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN ESTADO
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL ESTADO
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL ESTADO
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*ELIMINANDO LOS PERCENTILES 1 Y 99
gen ln_TFPR_ms=log(TFPR_msi/TFPR_ms_barra)
gen ln_A_ms=log(A_msi/A_ms_barra)
bysort msi: egen orden1=rank(ln_TFPR_ms)
bysort msi: egen orden2=rank(ln_A_ms)
bysort msi: gen tot=_N
gen percen1=orden1/tot
gen percen2=orden2/tot
drop if (percen1<=0.01 | percen1>=0.99)
drop if (percen2<=0.01 | percen2>=0.99)

*QUITANDO VARIABLES QUE DEBEN VOLVER A CALCULARSE
drop wL_ms K_ms PY_ms TFPR_ms_barra A_ms_barra sumando_A_ms base_A_ms

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
bysort msi: egen PY_ms=sum(PY_msi)

*CALCULANDO TRABAJO Y CAPITAL POR INDUSTRIA EN ESTADO (OTRA VEZ)
bysort msi: egen K_ms=sum(K_msi)
bysort msi: egen wL_ms=sum(wL_msi)

*CALCULANDO PRODUCTIVDAD MEDIA DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
gen TFPR_ms_barra=PY_ms/((K_ms^a_s)*(wL_ms^(1-a_s)))

*CALCULANDO PRODUCTIVDAD FISICA MEDIA DE LA INDUSTRIA EN EL ESTADO (OTRA VEZ)
gen sumando_A_ms=A_msi^($sigma-1)
bysort msi: egen base_A_ms=sum(sumando_A_ms)
gen A_ms_barra=base_A_ms^(1/($sigma-1))

*CALCULANDO PRODUCTIVIDAD DE LA INDUSTRIA EN EL ESTADO
gen sumando_TFP_ms=(A_msi*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi : egen base_TPF_ms=sum(sumando_TFP_ms)
gen pseudo_kappa_ms=1/(PY_ms^(1/($sigma-1)))
gen TFP_ms_1=pseudo_kappa_ms*(base_TPF_ms^(1/($sigma-1)))
gen TFP_ms_levy=base_TPF_ms^(1/($sigma-1))

*GENERANDO VARIABLE DE VALOR AGREGADO DE LA ECONOMIA DEL ESTADO
bysort estado : egen Y=sum(PY_msi)

*GENERANDO VARIABLE DE LA PARTICIPACION DE LA INDUSTRIA EN EL ESTADO
gen theta_s=PY_ms/Y

*GENERANDO VARIABLE DE GANANCIAS EN LA PRODUCTIVDAD DEL ESTADO
gen sumando_Y_Y_ef=((A_msi/A_ms_barra)*(TFPR_ms_barra/TFPR_msi))^($sigma-1)
bysort msi: egen pre_productando=sum(sumando_Y_Y_ef)
gen productando_Y_Y_ef=pre_productando^(theta_s/($sigma-1))
bysort msi : gen contador=_n
replace productando_Y_Y_ef=1 if contador!=1
gen ln_prod_Y_Y_ef=log(productando_Y_Y_ef)
bysort estado : egen ln_Y_Y_ef=sum(ln_prod_Y_Y_ef)
gen Y_Y_ef=100*((1/exp(ln_Y_Y_ef))-1)

*CALCULANDO PRODUCTIVIDAD (1) DEL ESTADO
gen productando_TFP_1=TFP_ms_1^theta_s
replace productando_TFP_1=1 if contador!=1
gen ln_prod_TFP_1= log(productando_TFP_1)
bysort estado : egen ln_TFP_1=sum(ln_prod_TFP_1)
gen TFP_1=exp(ln_TFP_1)

*CALCULANDO PRODUCTIVIDAD (levy) DEL ESTADO
gen productando_TFP_levy=TFP_ms_levy^theta_s
replace productando_TFP_levy=1 if contador!=1
gen ln_prod_TFP_levy= log(productando_TFP_levy)
bysort estado : egen ln_TFP_levy=sum(ln_prod_TFP_levy)
gen TFP_levy=exp(ln_TFP_levy)

*OBTENIENDO BASE FINAL
collapse (mean) Y_Y_ef TFP_1 TFP_levy (sum) PY_msi, by(estado)

*GUARDANDO BASE FINAL
save TFP_edo_3dig_14, replace


********************************************************************************
************************ GENERANDO OTRAS VARIABLES *****************************
********************************************************************************

*CARGANDO BASE
use base_general_14, clear

*VARIABLE DE INFORMALIDAD DEL ESTADO
bysort estado : egen seg_social_m=sum(g_seg_social)
bysort estado : egen wL_m=sum(wL_msi)
gen infor_m=seg_social_m/wL_m

*VARIABLE DE SECTOR ASALARIADO DEL ESTADO
gen asa_si=azul+blanco
bysort estado : egen asa_m=sum(asa_si)
bysort estado : egen trab_m=sum(trab_msi)
gen sector_asa_m=asa_m/trab_m

*VARIABLE DE TAMAÑO PROMEDIO DE LAS EMPRESAS EN EL ESTADO
bysort estado : egen num_m=mean(trab_m)

*VARIABLE DE LEGALIDAD DEL ESTADO
gen g_asa_si=s_azul+s_blanco
bysort estado : egen g_asa_m=sum(g_asa_si)
gen legal_m=seg_social_m/g_asa_m

*VARIABLE DE EDAD PROMEDIO DE LAS EMPRESAS EN EL MUNICIPIO
gen edad_msi=2020-anio_cre
bysort estado : egen edad_m=mean(edad_msi)

*CONSERVANDO VARIABLES DE INTERES
collapse (mean) infor_m sector_asa_m num_m legal_m edad_m, by(estado)

*GUARDANDO BASE
save variables_edo_14, replace


********************************************************************************
*********************************** FIN ****************************************
********************************************************************************




