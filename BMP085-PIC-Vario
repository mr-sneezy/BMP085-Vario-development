'******************************************************************************
'MR.SNEEZY - test code for FrSky using Bosch BMP085 baro sensor.
'This version is for PIC 18F1320 
'
'ADD/Do   Check what the result of new and old pressure value when equal ?
'
'NOTES - 
'
'LAST ACTION - Add TriState logic drive for the FrSky AD input
'
'PIC 18F1320 port/pin alocations
'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'PortA.0/Pin 1 = Serial TX
'PortA.1/Pin 2 = LED
'PortB.0/Pin 8 = I2C SCL clock
'PortB.1/Pin 9 = I2C SDA data
'PortB.4/Pin 10 = Up LED
'PortB.5/Pin 11 = Down LED
'PortB.6/Pin 12 = AD_Out
'VDD Pin5 GND
'VSS Pin14 +3.3V
 
' -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
'Config Directive settings for MPASM (fuses) for 18F4620
 
@    __CONFIG  _CONFIG1H, _IESO_OFF_1H & _FSCM_OFF_1H & _INTIO2_OSC_1H
@    __CONFIG  _CONFIG2L, _PWRT_ON_2L & _BOR_OFF_2L & _BORV_27_2L
@    __CONFIG  _CONFIG3H, _MCLRE_OFF_3H
@    __CONFIG  _CONFIG4L, _DEBUG_OFF_4L & _LVP_OFF_4L & _STVR_ON_4L
@    __CONFIG  _CONFIG5L, _CP0_OFF_5L & _CP1_OFF_5L
@    __CONFIG  _CONFIG5H, _CPB_OFF_5H & _CPD_OFF_5H
@    __CONFIG  _CONFIG6L, _WRT0_OFF_6L & _WRT1_OFF_6L
@    __CONFIG  _CONFIG6H, _WRTC_OFF_6H & _WRTB_OFF_6H & _WRTD_OFF_6H
@   __CONFIG  _CONFIG7L, _EBTR0_OFF_7L & _EBTR1_OFF_7L
@    __CONFIG  _CONFIG7H, _EBTRB_OFF_7H


    DEFINE OSC 8 '8Mhz clock used.
     
' Define some constants if needed
        
' Software Defines (variables and pins)
    Cal_table   var word[11]         '11 word array to store calibration data
    lUpres      var long             'Long variable for Uncompensated Pressure
    lPres       var Long             'Long variable for Compensated Pressure
    lPres_Prev  var Long       
    lTemp_Var1  Var long             'Long temporary variable
    X1          var Long
    X2          var Long
    X3          var Long
    B3          var long
    B4          var long
    B5          var long
    B6          var Long
    B7          var long
    
    lAC1        var Long            'Long variables for cal values actually negative in my sensor
    lAC2        var long            'These are 'cast' from the Word vars into Long vars in code below
    lAC3        var Long 
    lAC4        var Long 
    lMB         var Long
    lMC         var Long
 
    bTemp_Var1  var byte             'Byte temp variable 
    wTemp_Var1  var Word	         'Word temp variable
    wTemp_Var2  var word             'Word temp variable
    lUTemp      var long             'Uncompensated temperature reading from sensor
    lCTemp      var Long             'Compensated (real) temperature x10 (1/10th of C) from sensor
    lStablePresValue var long
    lNewPresValue    var long
    TrendUpCount     var byte
    TrendDnCount     var byte
    
    i2c_Reg     var Byte             'variable for target i2c register address

    CPIN        var     PortB.0       ' I2C clock pin 
    DPIN        var     PortB.1       ' I2C data pin
    SO          Var     PortA.0       'Serial out pin
    LED         var     PortA.1       'Indicator LED, via 500ohm to +3.3V
    UP_LED      var     PortB.4       'Indicator LED, via 500ohm to GND
    DN_LED      var     PortB.5       'Indicator LED, via 500ohm to GND
    AD_OUT      var     PortB.6       'AD drive pin, tristate logic (switch to Input = HiZ)
    
    OSS         con     $3          'This value is the Over Sampling Setting for the BMP085
                                    '0 = minimum, 3 = maximum. Also change value in Read_pres if you alter OSS 
    Threshold   con     $4          'Originally set to 5 (set 4 for Renato)
    StepValue   con     $4

'Alias's for calibration data in the sensor to match the Bosch parameter list names
    AC1     var     Cal_table[0]        '
    AC2     var     Cal_table[1]        'BMP085 has 11 16bit values stored in EEPROM
    AC3     var     Cal_table[2]        'First byte is at $AA last at $BF, two bytes per cal value
    AC4     var     Cal_table[3]        'Lowbyte is MSB (e.g $AA), Highbyte is LSB (e.g. $AB)
    AC5     var     Cal_table[4]        '
    AC6     var     Cal_table[5]    
    B1      var     Cal_table[6]        'Warning - AC4, AC5, AC6 are UNSIGNED values, the rest are SIGNED
    B2      var     Cal_table[7]
    MB      var     Cal_table[8]
    MC      var     Cal_table[9]    
    MD      var     Cal_table[10]    
    
' Initialise Processor - check for each PIC type 
' -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    ADCON1 = %11111111     'Turn off all AD's     
    OSCCON = %01110000     'set INTRC to 8 MHZ
'    OSCCON = %01100000     'set INTRC to 4 MHZ
    
'    OSCCON =  %01100111     'set INTRC to 4 MHZ    
'    OSCCON = %01110111     'set INTRC to 8 MHZ
'    OSCTUNE = 0                      'OSC trim set to Null 

' Set initial state of port pins as Input or Output if needed
'    TRISA = %11111100    'Input(0 = output, 1 = Input)
'    TRISB = %11111100    '
'    TRISC = %11111111
'    TRISD = %11111110
'    TRISE = %11111111
    
' PIC initialization code
'        Low So      'Start low, or you get rubbish on the LCD at PIC boot up.
        Gosub Alive                             'Go prove the PIC is running via LED
                   
        Serout2 SO,16780,[$FE,$01]               ' Clear LCD & home LCD cursor.
        pause 10                                 ' wait for LCD to catch up
        Serout2 SO,16780,["   FrSky Vario    "]  ' Serial print 
        Serout2 SO,16780,[$FE,$C0]               ' Shift cursor to line2
        Serout2 SO,16780,[" Development Jig  "]  ' Serial print 
        Pause   2000
        
        i2c_Reg =$AA                            'Start address of the BMP085 calibration data
        I2CREAD DPIN,CPIN,$EF,I2C_REG,[STR Cal_table\11],cal_error  'Read 11 reversed words out of sensor

        AC1 = (AC1.lowbyte<<8) + AC1.highbyte   'swap MSB and LSB of each to use in PBP (un-reverse then)    
        AC2 = (AC2.lowbyte<<8) + AC2.highbyte   'device stores the MSB in the Low byte, LSB in the High byte
        AC3 = (AC3.lowbyte<<8) + AC3.highbyte   
        AC4 = (AC4.lowbyte<<8) + AC4.highbyte          
        AC5 = (AC5.lowbyte<<8) + AC5.highbyte
        AC6 = (AC6.lowbyte<<8) + AC6.highbyte
        B1 = (B1.lowbyte<<8) + B1.highbyte
        B2 = (B2.lowbyte<<8) + B2.highbyte
        MB = (MB.lowbyte<<8) + MB.highbyte
        MC = (MC.lowbyte<<8) + MC.highbyte
        MD = (MD.lowbyte<<8) + MD.highbyte 

'Cast (convert) signed PBP Word vars to signed PBP Long vars where needed by math routines below
        lAC1 = AC1                           'copy word to long   
        if AC1.15 then lAC1.HIGHWORD = $FFFF 'Check for negative, set top 16bits to all 1's if true 

        lAC2 = AC2                           'copy word to long   
        if AC2.15 then lAC2.HIGHWORD = $FFFF 'Check for negative, set top 16bits to all 1's if true 

        lAC3 = AC3                           'copy word to long
        if AC3.15 then lAC3.HIGHWORD = $FFFF 'Check for negative, set top 16bits to all 1's if true

        lMB = MB                            'copy word to long
        if MB.15 then lMB.HIGHWORD = $FFFF  'Check for negative, set top 16bits to all 1's if true

        lMC = MC                            'copy word to long
        if MC.15 then lMC.HIGHWORD = $FFFF  'Check for negative, set top 16bits to all 1's if true

'Cast (convert) UN-signed PBP Word var to UN-signed PBP Long var for math routines below
        lAC4 = AC4                          'copy word to long, both unsigned
                                
        Serout2 SO,16780,[$FE,$01]             ' Clear LCD & home LCD cursor. 
        Pause 10                              ' wait for LCD to catch up

'Main loop -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
Main:  
        Serout2 SO,16780,[$FE,$02]             'home LCD cursor, LCD not cleared.                           
         
        Gosub Read_temp                        'get Temp via I2C
        
''        Serout2 SO,16780,["UT=",SDEC lUtemp," "]       'Send Word size number to LCD
        
        lTemp_Var1 = 0                          'Clear the last pressure reading
        For bTemp_Var1 = 0 to 9                 'Start of 10x averaging routine
        Gosub Read_pres                         'get Long uncompensated pressure via I2C
        lTemp_Var1 = lTemp_Var1 + lUpres
        Next bTemp_Var1
        lUpres = lTemp_Var1 / 10                'finish of the 10x Averaging routine   

''        Serout2 SO,16780,["UP=",SDEC lUpres," "]       'Send Word size number to LCD

'Calculate temperature in 1/10ths of Deg C  from lUTemp     ' Note 2^15 = 32768 Dec or $8000
        X1 = ((lUtemp - AC6) * AC5) / $8000     'find X1. 
        X2 = (lMC * $800) / (X1 + MD)           'Find X2.  Note:- math rounding results in X2 being in error by 1 ?
        B5 =  X1 + X2                           'Find B5 from X1 and X2.
        lCTemp = (B5 + 8) / 16                  'Hey presto, lCTemp appears... 

'DISPLAY true temperature in C 
''        X1 = lCTemp / 10                         'find value above decimal point
''        Serout2 SO,16780,[$FE,$C0]               ' Shift cursor to line_2
''        Serout2 SO,16780,["Temp= ",DEC X1,"."]   'Send Word size number to LCD        
''        X1 = lCTemp // 10                        'Find decimal value
''        Serout2 SO,16780,[DEC X1,"  "]           'Send Word size number to LCD 
         
'Calculate pressure in Pascals from uncompensated pressure lUpres (1/100th's of hPa's)        
        B6 = b5 - 4000

        x1 = (b2 * (B6 * B6 / $1000)) / $800 
        x2 = (lac2 * B6) / $800
        x3 = x1 + x2
        B3 = ((lac1 * 4 + x3) << OSS + 2) / 4     'OSS = Over Sampling constant set above  
 
        x1 = (lac3 * b6) / $2000
        x2 = (b1 * (b6 * b6 / $1000)) / $10000
        x3 = ((x1 + x2) + 2) / 4
        B4 = (lac4 * (x3 + 32768)) / $8000        'Find B4, note lAC4 is an unsigned Long

        B7 = (lUPres - B3) * (50000 >> OSS)       'OSS = Over Sampling constant set above
 
        If B7 < $80000000 then                    'branch if value is above or below range
            lPres = (B7 * 2) / B4
            Else
            lPres = (B7 / B4) * 2
        Endif

        X1 = (lPres / 256) * (lPres / 256)
        X1 = (X1 * 3038) / $10000                 '$10000 = 2^16
        X2 = (-7357 * lPres) / $10000
        lPres = lPres + (X1 + X2 + 3791) / 16     'lPres is the true pressure in Pa

'DISPLAY true pressure in hPa
        X1 = lPres / 100                          'find value above decimal point
        Serout2 SO,16780,[$FE,$94]                'Shift cursor to line_3   
        Serout2 SO,16780,["hPa= ",DEC X1,"."]     'Send Word size number to LCD        
        X1 = lPres // 100                         'find value below decimal point
        Serout2 SO,16780,[DEC2 X1,"  "]            'Send Word size number to LCD         

'        pause 1000
        Toggle LED                                'flash the 'im alive' LED
        
        
'Find pressure trend (altitude increasing or decreasing) Subroutine by Andrew Tennent Esq
' Establish StableValue
'Get lPres (with averaging)

    if lStablePresValue = 0 then lStablePresValue = lpres
 
    If lpres > lStablePresValue then                    'higher pressure (sinking...)
        if (lPres - lStablePresValue) > StepValue then      'filter noise 
               TrendDnCount = TrendDnCount + 1          'trending upwards
                if TrendDnCount > Threshold then        'trend is large enough to act
                        Low AD_OUT                      'Set Tristate drive Low for Sinking
                        High DN_LED                     'Climbing indicator LED set ON
                        Low UP_LED                      'Sinking LED set OFF
                        lStablePresValue = lPres        '
                        TrendDnCount = 1                'seed the value upwards so we don't clear downward trend
                        TrendUpCount = 0                'clear the down trend
                endif
        else
                TrendDnCount = 0
        endif
    Else 
        if lPres < lStablePresValue then                'lower pressure (climbing)
            if lStablePresValue - lPres > StepValue then
                TrendUpCount = TrendUpCount + 1
                if TrendUpCount > Threshold then
                        High AD_OUT                     'Set Tristate drive High for climbing
                        High UP_LED                     'Sinking indicator LED set ON
                        Low DN_LED                      'Climbing LED set OFF
                        lStablePresValue = lPres
                        TrendDnCount = 0
                        TrendUpCount = 1
                endif
        else
                TrendUpCount = 0
           endif
        Endif
    Endif    

    If TrendUpCount = 0 and TrendDnCount = 0 then      'indicate that there is no trend now
            INPUT PORTA.0 ' Make AD_OUT pin an input to go HiZ state
            Low DN_LED
            LOW UP_LED
    Endif

        Serout2 SO,16780,[$FE,$C0]               ' Shift cursor to line_2
        Serout2 SO,16780,["UP=",DEC TrendUpCount,"  DN=",DEC TrendDnCount,"  "]   'Send Word size number to LCD    
   
   
        Goto main
        
'SUBROUTINES -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-      
Read_pres: 
        i2c_Reg = $F4                               '$F4 is the control register address
        I2CWRITE DPIN,CPIN,$EE,I2C_REG,[$F4]        ' Write $34+(oss << 6) to set pressure conversion 
        Pause 30                                    ' Delay 10ms after each write (30mS for HiRes results (oss=3))
        i2c_Reg = $F6                               '$F6 is the result register MSB
        I2CREAD DPIN,CPIN,$EF,I2C_REG,[lUpres],I2C_error  'Read pressure MSB, LSB, XLSB, $F9 ($F9 not actually wanted).
        lUpres = lUpres >> (16 - oss)               'remove $F9 from result (>>8), and left shift result back to 16 to 19 Bits (OSS value dependant)
                                                    'it's because PBP reads four bytes if [Var] is a long...
        return                                      'we only want top 19bits of the result.
              
Read_temp:
        i2c_Reg = $F4                               '$F4 is the control register address
        I2CWRITE DPIN,CPIN,$EE,I2C_REG,[$2E]        ' Write $2E to set temperature conversion 
        Pause 10                                    ' Delay 10ms after each write
        i2c_Reg = $F6                               '$F6 is the result register MSB
        I2CREAD DPIN,CPIN,$EF,I2C_REG,[wTemp_Var1],I2C_error  'Read temperature MSB, LSB.
        lUTemp = wTemp_Var1                          'copy word to long. Note BMP085 UT is NOT a signed value   
        return

'Prove it's alive
Alive:
        High LED                                     'flash LED routine
        For btemp_var1 = 10 to 110 step 10
        Low LED
        Pause bTemp_Var1
        High LED
        Pause bTemp_Var1
        Next bTemp_Var1
        
        Return

'trap and display I2C problems        
I2C_error:     
        Serout2 SO,16780,[$FE,$01]             ' Clear LCD & home LCD cursor. 
        Pause 10                              ' wait for LCD to catch up
        Serout2 SO,16780,["i2c bus read error"]       'no ACK from I2C device        
        pause 2000        
        Toggle LED
        Goto main
         
Cal_error:
        Serout2 SO,16780,[$FE,$01]             ' Clear LCD & home LCD cursor. 
        Pause 10 
        Serout2 SO,16780,["i2c cal read error "]       '        
        
End 
