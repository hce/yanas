module Example where

import AVMisc
import Types

airspace :: [Element]
airspace = [AC Aeroplane {
               acregistration="DEAAA",
               accallsign="DEAAA",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="C150",
               accategory=AC_A,
               acpcategory=AP_A,
               aclat=mkPos 49 55.2,
               aclon=mkPos 8 36.1,
               acspeed=90,
               acheading=3,
               acvspeed=0,
               actruealt=1300,
               actransponder=([SquawkA, SquawkC, SquawkS], 4447),
               acfrequency=Frequency 119900000,
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            AC Aeroplane {
               acregistration="DEAQA",
               accallsign="DEAQA",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="A210",
               accategory=AC_A,
               acpcategory=AP_A,
               aclat=mkPos' 49 46 54,
               aclon=mkPos' 8 32 29,
               acspeed=115,
               acheading=51,
               acvspeed=0,
               actruealt=1400,
               acfrequency=Frequency 118400000,
               actransponder=([SquawkA, SquawkC, SquawkS], 4441),
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            AC Aeroplane {
               acregistration="OEABC",
               accallsign="OEABC",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="C172",
               accategory=AC_A,
               acpcategory=AP_A,
               aclat=mkPos 50 0.3,
               aclon=mkPos 8 48.8,
               acspeed=115,
               acheading=40,
               acvspeed=0,
               actruealt=1300,
               acfrequency=Frequency 119150000,
               actransponder=([SquawkA, SquawkC, SquawkS], 7000),
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            AC Aeroplane {
               acregistration="DAIQP",
               accallsign="DLH123",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="A322",
               accategory=AC_C,
               acpcategory=AP_C,
               aclat=mkPos 50 8.2,
               aclon=mkPos 8 35.6,
               acspeed=180,
               acheading=252,
               acvspeed=0,
               actruealt=8200,
               acfrequency=Frequency 127275000,
               actransponder=([SquawkA, SquawkC, SquawkS], 1000),
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            AC Aeroplane {
               acregistration="DAIQQ",
               accallsign="DLH124",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="A322",
               accategory=AC_C,
               acpcategory=AP_C,
               aclat=mkPos' 49 55 16,
               aclon=mkPos 8 8,
               acspeed=180,
               acheading=252,
               acvspeed=0,
               actruealt=8200,
               acfrequency=Frequency 127275000,
               actransponder=([SquawkA, SquawkC, SquawkS], 1000),
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            AC Aeroplane {
               acregistration="N123AB",
               accallsign="AWE456",
               acicao="",
               acflightplan="",
               acequipment=[],
               actype="A332",
               accategory=AC_C,
               acpcategory=AP_C,
               aclat=mkPos' 49 55 16,
               aclon=mkPos' 8 30 23,
               acspeed=512,
               acheading=210,
               acvspeed=0,
               actruealt=8200,
               acfrequency=Frequency 127275000,
               actransponder=([SquawkA, SquawkC, SquawkS], 1000),
               acatccommands=[],
               acatcresponses=[],
               acturnrate=0,
               acturnto=0},
            BC Beacon {
              bcntype=VOR,
              bcnlon=mkPos' 8 38 14,
              bcnlat=mkPos' 50 3 13,
              bcnfreq=114200000,
              bcnname="Frankfurt",
              bcnid="FFM",
              bcnvar=1,
              bcninop=False,
              bcnrange=50
              },
            BC Beacon {
              bcntype=VOR,
              bcnlon=mkPos' 8 50 55,
              bcnlat=mkPos' 50 16 35,
              bcnfreq=110000000,
              bcnname="Metro",
              bcnid="MTR",
              bcnvar=1,
              bcninop=False,
              bcnrange=60
              },
            BC Beacon {
              bcntype=VOR,
              bcnlon=mkPos' 9 2 23,
              bcnlat=mkPos' 49 55 16,
              bcnfreq=115500000,
              bcnname="Charlie",
              bcnid="CHA",
              bcnvar=1,
              bcninop=False,
              bcnrange=60
              },
            BC Beacon {
              bcntype=VOR,
              bcnlon=mkPos' 8 32 29,
              bcnlat=mkPos' 49 46 54,
              bcnfreq=112200000,
              bcnname="Ried",
              bcnid="RID",
              bcnvar=1,
              bcninop=False,
              bcnrange=40
              },
            WP VFRRP {
              vfrlat=mkPos 49 52,
              vfrlon=mkPos 8 36.6,
              vfrcompulsory=True,
              vfrdesignation="Delta 1",
              vfrdesignationletter=Nothing,              
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 49 52.7,
              vfrlon=mkPos 8 32.8,
              vfrcompulsory=True,
              vfrdesignation="Juliet",
              vfrdesignationletter=Just 'J',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 49 53.6,
              vfrlon=mkPos 8 29.9,
              vfrcompulsory=True,
              vfrdesignation="Sierra",
              vfrdesignationletter=Just 'S',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 49 59,
              vfrlon=mkPos 8 36.1,
              vfrcompulsory=True,
              vfrdesignation="Lima",
              vfrdesignationletter=Just 'L',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 49 52.6,
              vfrlon=mkPos 8 43.4,
              vfrcompulsory=True,
              vfrdesignation="Kilo",
              vfrdesignationletter=Just 'K',
              vfrctr="CTR Frankfurt"               
              },
            WP VFRRP {
              vfrlat=mkPos 49 57.1,
              vfrlon=mkPos 8 50.2,
              vfrcompulsory=True,
              vfrdesignation="Echo",
              vfrdesignationletter=Just 'E',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 50 0.3,
              vfrlon=mkPos 8 48.8,
              vfrcompulsory=True,
              vfrdesignation="Tango",
              vfrdesignationletter=Just 'T',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 50 8.2,
              vfrlon=mkPos 8 35.6,
              vfrcompulsory=True,
              vfrdesignation="November",
              vfrdesignationletter=Just 'N',
              vfrctr="CTR Frankfurt"
              },
            WP VFRRP {
              vfrlat=mkPos 49 56.1,
              vfrlon=mkPos 8 21.3,
              vfrcompulsory=True,
              vfrdesignation="Romeo 1",
              vfrdesignationletter=Nothing,
              vfrctr="CTR Frankfurt"
              },
            Air Airspace {
              airClassification=AirspaceD,
              airFlags=[CTR],
              airVBottom=GND,
              airVTop=BelowA AirspaceC,
              airActive = HJ,
              airPolygone=[(mkPos 8 36.1, mkPos 49 54),
                           (mkPos 8 36.1, mkPos 49 59),
                           (mkPos 8 44.5, mkPos 50 02.7),
                           (mkPos 8 44.5, mkPos 49 54)]              
              },
            Air Airspace {
              airClassification=AirspaceD,
              airFlags=[CTR],
              airVBottom=GND,
              airVTop=BelowA AirspaceC,
              airActive=H24,
              airPolygone=[(mkPos 8 36.1, mkPos 49 54.7),
                           (mkPos 8 27, mkPos 49 54.7),
                           (mkPos 8 27, mkPos 49 58),
                           (mkPos 8 23.0, mkPos 49 57),
                           (mkPos 8 19.8, mkPos 50 06),
                           (mkPos 8 40, mkPos 50 07.5),
                           (mkPos 8 42.5, mkPos 50 07.5),
                           (mkPos 8 46, mkPos 50 3.5),
                           (mkPos 8 36.1, mkPos 49 59)]
              }
           ]

exampleFrequencies :: [(Frequency, Designation)]
exampleFrequencies = [(f 118400000, "Egelsbach Info"),
                      (f 119150000, "Langen Information"),
                      (f 119900000, "Frankfurt Tower"),
                      (f 127275000, "Frankfurt Director"),
                      (f 136500000, "Frankfurt Tower")]
  where
    f :: Int -> Frequency
    f = Frequency