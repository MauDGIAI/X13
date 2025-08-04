# Ingresos por Suministros de Bienes y Servicios al por Mayor - EMEC          

series{                                                                    
       title='EMEC INGRESOS POR SUMINISTROS DE BIENES Y SERVICIOS AL POR MAYOR'
       start=2008.01
       period=12                                                 
       file="Imay.dat"                                                         
       format=datevalue                                                        
       decimals=3
       precision=3                                                  
       print=(a18)
       save=(b1 a18)}                                              
transform{                                                                 
       function=log}                                               
regression{                                                               
       variables=(Tdnolpyear Lpyear Easter[3]
                  LS2020.Mar LS2020.Apr AO2020.Apr AO2020.May AO2020.Jun)
       save=(td hol)}                                                    
arima{model=([2] 1 [3])(2 1 0)}
check{maxlag=24 savelog=lbq print=all}
estimate{print=(armacmatrix roots)}
x11{seasonalma=(s3x3 s3x5 s3x5 s3x5 s3x5 s3x5
                s3x3 s3x5 s3x9 s3x5 s3x5 s3x3)
    sigmalim=(1.0, 2.5)
    print=(e1 e2 e3)
    save=(d10 d11 d12 d13 e1 e2 e3)}

# Modelo extraído del Banco de Información Económica INEGI