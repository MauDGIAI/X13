# Ingresos por Suministros de Bienes y Servicios al por Mayor - EMEC
series{
title='EMEC INGRESOS POR SUMINISTROS DE BIENES Y SERVICIOS AL POR MAYOR'
start=2008.01
period=12
file='D:/X13ARIMASEATS/temp/DATA.dat'
format=datevalue
decimals=3
precision=3
print=(a18)
save=(a1 b1 a18)}
transform{
function=log}
regression{
variables=(Tdnolpyear Lpyear Easter[3]
LS2020.Mar LS2020.Apr AO2020.Apr AO2020.May)
save=(td hol)}
arima{model=([2] 1 [3])(2 1 0)}
check{maxlag=24 savelog=lbq print=all save=(acf pcf)} spectrum{save=(spr sp0 sp1 sp2)} forecast{save=(fct)}
estimate{print=(armacmatrix roots)}
x11{seasonalma=(s3x3 s3x5 s3x5 s3x5 s3x5 s3x5
s3x3 s3x5 s3x9 s3x5 s3x5 s3x3)
sigmalim=(1.0, 2.5)
print=(e1 e2 e3)
save=(d8 d9 d10 d11 d12 d13 e1 e2 e3)}
# Modelo extraÃ­do del Banco de InformaciÃ³n EconÃ³mica INEGI
