'*******************************************************************************
'*    Description   : WS2000 Sensor Temperatur und Feuchte mit SHT31           *
'*    Revision      : 1.0                                                      *
'*    Controller    : ATmega48PA-PU                                            *
'*    Stromaufnahme : Powersave 1 µA, Durschnitt ca. 6 µA                                                 *
'*    Compiler      : BASCOM-AVR  2.0.8.3                                      *
'*    Author        : UB , 2020                                                *
'*    Web           : HTTP://WWW.Elektron-BBS.de                               *
'*******************************************************************************
$regfile = "m48pdef.dat"
$crystal = 1000000
$hwstack = 40
$swstack = 40
$framesize = 40
Stop Ac                                                                         ' Strom sparen
Stop Adc                                                                        ' Strom sparen

'****************************** Timer 2 ****************************************
Config Timer2 = Timer , Async = On , Prescale = 1024                            ' 8 Sekunden
Enable Timer2
Dim Count As Byte

'******************************** I2C ******************************************
$lib "i2c_twi.lbx"                                                              ' we do not use software emulated I2C but the TWI
Config Scl = Portc.5                                                            ' I2C Clock
Config Sda = Portc.4                                                            ' I2C Data
I2cinit                                                                         ' I2C-Bus initialisieren

'************************* ports for connection ********************************
Config Portb.1 = Output : Tx433 Alias Portb.1                                   ' Data Sendemodul 433 MHz

'*************************** ungenutze Ports ***********************************
Config Portb.0 = Input : Portb.0 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portb.2 = Input : Portb.2 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portb.3 = Input : Portb.3 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portb.4 = Input : Portb.4 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portb.5 = Input : Portb.5 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portc.0 = Input : Portc.0 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portc.1 = Input : Portc.1 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portc.2 = Input : Portc.2 = 1                                            ' Port als Eingang, Pullup eingeschaltet
Config Portc.3 = Input : Portc.3 = 1                                            ' Port als Eingang, Pullup eingeschaltet

'************************* Variablen TX 433 MHz ********************************
Dim Tx_dbl As Double                                                            ' 64 Bit Sendepuffer
Dim Tx_bit_nr As Byte                                                           ' Nummer zu sendendes Bit
Const Tx_bit_anzahl = 60                                                        ' Anzahl zu sendende Bits
Dim Tx_byte As Byte                                                             ' zu sendendes Byte
Dim Tx_byte_nr As Byte                                                          ' Nummer zu sendendes Byte
Const Tx_byte_anzahl = 8                                                        ' Anzahl zu sendende Bytes
Dim Check As Byte                                                               ' Prüfsumme XOR Typ bis Check muss 0 ergeben
Dim Ersumme As Byte                                                             ' Prüfsumme errechnet
Dim S_adresse_temp As Byte                                                      ' Sensoradresse Temperatur/Feuchte

'*************************** Variablen SHT31 ***********************************
Dim Temperatur As Single
Dim Feuchte As Single

'***************************** Temporär ****************************************
Dim U As Byte , X As Byte , Y As Byte , Z As Byte
Dim W As Word
Dim Ar(10) As Byte

Enable Interrupts                                                               ' Interrupts einschalten

'*************************** H A U P T P R O G R A M M *************************
Do
   Select Case Count
      Case 1                                                                    ' SHT31 Temperatur messen
         Timer2 = &HE0                                                          ' Timer auf 1 Sekunde bis Overflow
         'SHT31 Temperatur messen
         I2cstart
         I2cwbyte &H88                                                          ' address of the microcircuit-sensor SHT31
         I2cwbyte &H24                                                          ' MSB Clock stretching disabled
         I2cwbyte &H00                                                          ' LSB Repeatability high (12,5 mS)
         'I2cwbyte &H0B                                                          ' LSB Repeatability medium (4,5 mS)
         'I2cwbyte &H16                                                          ' LSB Repeatability low (2,5 mS)
         I2cstop                                                                ' end of the contact
      Case 2
         'Sensor Adresse von Jumperstellung übernehmen
         Config Portd = Input                                                   ' Port als Eingänge
         Portd = 255                                                            ' Pullups einschalten
         Nop                                                                    ' kurz warten (SYNC LATCH)
         X = Pind                                                               ' Jumperstellung übernehmen
         'Jumper gegen GND, gesteckt = Bit auf 0, offen = Bit auf 1
         'Pind.0 - Jumper 1 (K1) Adresse Temperatur/Feuchte
         'Pind.1 - Jumper 2 (K2) Adresse Temperatur/Feuchte
         'Pind.2 - Jumper 3 (K3) Adresse Temperatur/Feuchte
         S_adresse_temp = X And &B00000111                                      ' Adresse Temp-/Feuchtesensor (untere 3 Bit) übernehmen
         Portd = 0                                                              ' Pullups ausschalten
         Config Portd = Output                                                  ' Port als Ausgang
         Timer2 = S_adresse_temp * 16                                           ' 8 - 0 bis 3,5 Sekunden, je nach Adresse
         'SHT31 Temperaturwerte holen
         I2cstart
         I2cwbyte &H89                                                          ' reading the result of the measurement
         I2crbyte Ar(1) , Ack                                                   ' receiving MSB Temperature
         I2crbyte Ar(2) , Ack                                                   ' receiving LSB Temperature
         I2crbyte Ar(3) , Ack                                                   ' receiving CRC Temperature
         I2crbyte Ar(5) , Ack                                                   ' receiving MSB Humidity
         I2crbyte Ar(6) , Ack                                                   ' receiving LSB Humidity
         I2crbyte Ar(7) , Nack                                                  ' receiving CRC Humidity
         I2cstop                                                                ' end of the contact
         Gosub Sht31crc
         If Ar(3) = Ar(4) Then                                                  ' CRC OK
            W = Makeint(ar(2) , Ar(1))                                          ' Convert 2 byte in one word.
            Temperatur = W * 1750
            Temperatur = Temperatur / 65535
            Temperatur = Temperatur - 450
            Temperatur = Round(temperatur)
         End If
         Ar(1) = Ar(5)
         Ar(2) = Ar(6)
         Ar(3) = Ar(7)
         Gosub Sht31crc
         If Ar(7) = Ar(4) Then                                                  ' CRC OK
            W = Makeint(ar(6) , Ar(5))                                          ' Convert 2 byte in one word.
            Feuchte = W * 1000
            Feuchte = Feuchte / 65535
            Feuchte = Round(feuchte)
         End If

         If Temperatur >= -300 Then                                             ' Fehler abfangen
            If Feuchte >= 1 Then                                                ' Fehler abfangen
               Ar(1) = 1                                                        ' Sensortyp 1 - Thermo/Hygro (AS2000, ASH2000, S2000, S2001A, S2001IA, ASH2200, S300IA)
               Ar(2) = S_adresse_temp                                           ' Sensoradresse Temperatur übernehmen
               W = Temperatur                                                   ' Temperatur übernehmen
               If Temperatur < 0 Then
                  Ar(2).3 = 1                                                   ' Bit 20 Vorzeichen negativ
                  W = Abs(temperatur)                                           ' Temperaturwert umkehren
               End If
               Gosub Tx_wert                                                    ' Temperatur umrechnen
               Ar(3) = Z : Ar(4) = Y : Ar(5) = X                                ' Bit 22-35 Temperatur in Array übernehmen
               W = Feuchte                                                      ' Feuchte übernehmen
               Gosub Tx_wert                                                    ' Feuchte umrechnen
               Ar(6) = Z : Ar(7) = Y : Ar(8) = X                                ' Bit 37-50 Feuchte in Array übernehmen
               Gosub Tx_433_send                                                ' Daten senden
            End If
         End If
   End Select

   Incr Count
'   If Count >= 8 Then : Count = 0 : End If
   If Count >= 23 Then : Count = 0 : End If                                     ' 176 + 1 = 177 - Adresse * 0,5 Sekunden
   Config Powermode = Powersave
Loop

End

'************************** U N T E R P R O G R A M M E ************************
'senden dauert: 74,42 mS (Thermo/Hygro)
Tx_433_send:
   Tx_dbl = 0                                                                   ' alle Bits auf 0 setzen
   X = 10                                                                       ' 10 Bit Präambel
   Do                                                                           ' Bit 10 bis Anzahl Step 5 muß 1 sein
      Tx_dbl.x = 1                                                              ' Bit auf 1 setzen
      X = X + 5                                                                 ' Array beginnt bei 1
   Loop Until X > Tx_bit_anzahl
   'Bits übernehmen
   Check = 0                                                                    ' Checksumme zurück setzen
   Ersumme = 0                                                                  ' Prüfsumme zurück setzen
   Tx_byte_nr = 0
   Do                                                                           ' Sensortyp, Adresse und Werte senden
      Incr Tx_byte_nr                                                           ' beginnt mit 1
      Tx_byte = Ar(tx_byte_nr)                                                  ' Byte übernehmen
      Gosub Tx_433_byte                                                         ' Bit 0-3 übernehmen
   Loop Until Tx_byte_nr >= Tx_byte_anzahl                                      ' fertig
   Incr Tx_byte_nr                                                              ' nächstes Byte
   Tx_byte = Check                                                              ' Checkbyte übernehmen
   Gosub Tx_433_byte                                                            ' Byte übernehmen
   Ersumme = Ersumme + 5                                                        ' Prüfsumme errechnen
   Tx_byte = Ersumme And &B00001111                                             ' obere 4 Bit auf 0 setzen
   Incr Tx_byte_nr                                                              ' nächstes Byte
   Gosub Tx_433_byte                                                            ' Byte senden
   'Bits senden
   X = 0                                                                        ' Beginne mit Bit 0
   Do
      If Tx_dbl.x = 1 Then                                                      ' 1 senden
         Set Tx433                                                              ' Ausgang high
         Waitus 366                                                             ' 366 µS warten
         Reset Tx433                                                            ' Ausgang low
         'Waitus 854                                                             ' 854 µS warten
         Waitus 780                                                             ' 854 µS warten, Rest braucht Programm
      Else                                                                      ' 0 senden
         Set Tx433                                                              ' Ausgang high
         Waitus 854                                                             ' 854 µS warten
         Reset Tx433                                                            ' Ausgang low
         'Waitus 366                                                             ' 366 µS warten
         Waitus 304                                                             ' 366 µS warten, Rest braucht Programm
      End If
      Incr X                                                                    ' nächstes Bit
   Loop Until X > Tx_bit_anzahl                                                 ' Ende mit gesetzter Anzahl Bits
Return

Tx_433_byte:
   Tx_bit_nr = Tx_byte_nr * 5                                                   ' 5, 10, 15...
   Tx_bit_nr = Tx_bit_nr + 6                                                    ' 11, 16, 21...
   X = 0
   Do
      Tx_dbl.tx_bit_nr = Tx_byte.x                                              ' Bit aus Byte übernehmen
      Incr Tx_bit_nr : Incr X                                                   ' nächstes Bit
   Loop Until X >= 4                                                            ' 4 Bit
   Check = Check Xor Tx_byte                                                    ' Check
   Ersumme = Ersumme + Tx_byte                                                  ' Prüfsumme bilden
Return

Sht31crc:
   Loadadr Ar(1) , Z
   $asm
      LDI R16,$02
      SER R24
      LDI R22,$31
Sht31crc1:
      LD R25,Z+
      EOR R24,R25
      LDI R17,$08
Sht31crc2:
      LSL R24
      BRcc Sht31crc3
      EOR R24,R22
Sht31crc3:
      DEC R17
      BRNE Sht31crc2
      DEC R16
      BRNE Sht31crc1
      ADIW R30,1
      ST Z,R24
   $end Asm
Return

Tx_wert:
   U = 0
   While W >= 100                                                               ' solange Wert größer X
      W = W - 100                                                               ' Zehnerpotenz subtrahieren
      Incr U                                                                    ' dann Zaehler erhöhen
   Wend
   X = U                                                                        ' Hunderter
   U = 0
   While W >= 10                                                                ' solange Wert größer X
      W = W - 10                                                                ' Zehnerpotenz subtrahieren
      Incr U                                                                    ' dann Zaehler erhöhen
   Wend
   Y = U                                                                        ' Zehner
   Z = Low(w)                                                                   ' Einer
Return