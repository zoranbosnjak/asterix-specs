Asterix category 020 - Multilateration Target Reports
=====================================================
**category**: 020

**edition**: 1.9

**date**: 2015-03-25

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I020/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system from which the data are received

*Structure*:

    **I020/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I020/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I020/020 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the data as transmitted by a system.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I020/020/SSR**

    - 1 bit [``.``]

    - values:

        | 0: Non-Mode S 1090MHz multilateration
        | 1: No Non-Mode S 1090MHz multilat

    **I020/020/MS**

    - 1 bit [``.``]

    - values:

        | 0: Mode-S 1090 MHz multilateration
        | 1: No Mode-S 1090 MHz multilateration

    **I020/020/HF**

    - 1 bit [``.``]

    - values:

        | 0: HF multilateration
        | 1: No HF multilateration

    **I020/020/VDL4**

    - 1 bit [``.``]

    - values:

        | 0: VDL Mode 4 multilateration
        | 1: No VDL Mode 4 multilateration

    **I020/020/UAT**

    - 1 bit [``.``]

    - values:

        | 0: UAT multilateration
        | 1: No UAT multilateration

    **I020/020/DME**

    - 1 bit [``.``]

    - values:

        | 0: DME/TACAN multilateration
        | 1: No DME/TACAN multilateration

    **I020/020/OT**

    - 1 bit [``.``]

    - values:

        | 0: Other Technology Multilateration
        | 1: No Other Technology Multilateration

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I020/020/RAB**

    - 1 bit [``.``]

    - values:

        | 0: Report from target transponder
        | 1: Report from field monitor (element transponder)

    **I020/020/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Absence of SPI
        | 1: Special Position Identification

    **I020/020/CHN**

    - 1 bit [``.``]

    - values:

        | 0: Chain 1
        | 1: Chain 2

    **I020/020/GBS**

    - 1 bit [``.``]

    - values:

        | 0: Transponder Ground bit not set
        | 1: Transponder Ground bit set

    **I020/020/CRT**

    - 1 bit [``.``]

    - values:

        | 0: No Corrupted reply in multilateration
        | 1: Corrupted replies in multilateration

    **I020/020/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual target report
        | 1: Simulated target report

    **I020/020/TST**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test Target

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I020/030 - Warning/Error Conditions
***********************************

*Definition*: Warning/error conditions detected by a system for the target report involved.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I020/030/WE**

    - 7 bits [``.......``]

    - values:

        | 0: Not defined; never used
        | 1: Multipath Reply (Reflection)
        | 3: Split plot
        | 10: Phantom SSR plot
        | 11: Non-Matching Mode-3/A Code
        | 12: Mode C code / Mode S altitude code abnormal value compared to the track
        | 15: Transponder anomaly detected
        | 16: Duplicated or Illegal Mode S Aircraft Address
        | 17: Mode S error correction applied
        | 18: Undecodable Mode C code / Mode S altitude code

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. It has to be stressed that a series of one or more W/E conditions
       can be reported per target report.
    2. Data conveyed in this item are of secondary importance, and can
       generally also be derived from the processing of mandatory items.
    3. Definitions can be found in SUR.ET1.ST03.1000-STD-01-01 Radar
       Sensor Performance Analysis.
    4. The coding of Warning/Errors is kept consistent with category 048.

I020/041 - Position In WGS-84 Coordinates
*****************************************

*Definition*: Position of a target in WGS-84 Coordinates.

*Structure*:

    **I020/041/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 25
    - unit: "deg"
    - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I020/041/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 25
    - unit: "deg"
    - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg



I020/042 - Position in Cartesian Coordinates
********************************************

*Definition*: Calculated position in Cartesian Coordinates, in two’s complement
representation.

*Structure*:

    **I020/042/X** - *X-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m

    **I020/042/Y** - *Y-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m



I020/050 - Mode-2 Code in Octal Representation
**********************************************

*Definition*: Mode-2 code converted into octal representation.

*Structure*:

    **I020/050/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I020/050/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I020/050/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-2 code derived from the reply of the transponder
        | 1: Smoothed Mode-2 code as provided by a local tracker n

    **I020/050/(spare)**

    - 1 bit [``.``]

    **I020/050/MODE2** - *Mode-2 Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)



I020/055 - Mode-1 Code in Octal Representation
**********************************************

*Definition*: Mode-1 code converted into octal representation.

*Structure*:

    **I020/055/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I020/055/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I020/055/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-1 code derived from the reply of the transponder
        | 1: Smoothed Mode-1 code as provided by a local tracker

    **I020/055/MODE1** - *Mode-1 Code in Octal Representation*

    - 5 bits [``.....``]

    - raw value



I020/070 - Mode-3/A Code in Octal Representation
************************************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I020/070/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I020/070/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I020/070/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Mode-3/A code not extracted during the last update period

    **I020/070/(spare)**

    - 1 bit [``.``]

    **I020/070/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Notes:

    1. Bit 15 (G) is set to one when an error correction has been attempted.
    2. Bit 16 (V) is normally set to zero, but can exceptionally be set to
       one to indicate a non-validated Mode-3/A code (e.g. alert condition
       detected, but new Mode-3/A code not successfully extracted).

I020/090 - Flight Level in Binary Representation
************************************************

*Definition*: Flight Level (Mode S Altitude) converted into binary two's complement representation.

*Structure*:

    **I020/090/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I020/090/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I020/090/FL** - *Flight Level*

    - 14 bits [``..............``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL


Notes:

    1. When Mode C code / Mode S altitude code is present but not
       decodable, the “Undecodable Mode C code / Mode S altitude
       code” Warning/Error should be sent in I020/030.
    2. When local tracking is applied and the received Mode S altitude
       code corresponds to an abnormal value (i.e: the difference in
       altitude between the current and the previous plot exceeds a
       predefined system threshold), the “Mode C code / Mode S altitude
       code abnormal value compared to the track“ Warning/Error should
       be sent in I020/030.
    3. The value shall be within the range described by ICAO Annex 10
    4. For Mode S, bit 15 (G) is set to one when an error correction has
       been attempted.

I020/100 - Mode C Code
**********************

*Definition*: Mode-C height in Gray notation as received from the transponder together
with the confidence level for each reply bit as provided by a MSSR/Mode-S
station.

*Structure*:

    **I020/100/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I020/100/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I020/100/(spare)**

    - 2 bits [``..``]

    **I020/100/MODEC** - *Mode-C Reply in Gray Notation*

    - 12 bits [``............``]

    - raw value

    **I020/100/(spare)**

    - 4 bits [``....``]

    **I020/100/QC1** - *Quality Pulse C1*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I020/100/QA1** - *Quality Pulse A1*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I020/100/QC2** - *Quality Pulse C2*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I020/100/QA2** - *Quality Pulse A2*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I020/100/QC4** - *Quality Pulse C4*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I020/100/QA4** - *Quality Pulse A4*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I020/100/QB1** - *Quality Pulse B1*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I020/100/QD1** - *Quality Pulse D1*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1

    **I020/100/QB2** - *Quality Pulse B2*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I020/100/QD2** - *Quality Pulse D2*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I020/100/QB4** - *Quality Pulse B4*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I020/100/QD4** - *Quality Pulse D4*

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4


Notes:

    1. For Mode S, bit 31 (G) is set to one when an error correction
       has been attempted.
    2. For Mode S, D1 is also designated as Q, and is used to denote
       either 25ft or 100ft reporting.

I020/105 - Geometric Height (WGS-84)
************************************

*Definition*: Vertical distance between the target and the projection of its position
on the earth’s ellipsoid, as defined by WGS84, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft"
- LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
- value :math:`>= -204800` ft
- value :math:`<= 204800` ft



I020/110 - Measured Height (Local Cartesian Coordinates)
********************************************************

*Definition*: Height above local 2D co-ordinate system in reference to the MLT System
Reference Point as defined in item I019/610, in two’s complement form,
based on a direct measurement not related to barometric pressure.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft"
- LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
- value :math:`>= -204800` ft
- value :math:`<= 204800` ft



I020/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s



I020/161 - Track Number
***********************

*Definition*: An integer value representing a unique reference to a track record
within a particular track file.

*Structure*:

    **I020/161/(spare)**

    - 4 bits [``....``]

    **I020/161/TRN** - *Track Number*

    - 12 bits [``............``]

    - raw value



I020/170 - Track Status
***********************

*Definition*: Status of a track.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I020/170/CNF**

    - 1 bit [``.``]

    - values:

        | 0: Confirmed track
        | 1: Track in initiation phase

    **I020/170/TRE**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Last report for a track

    **I020/170/CST**

    - 1 bit [``.``]

    - values:

        | 0: Not extrapolated
        | 1: Extrapolated

    **I020/170/CDM**

    - 2 bits [``..``]

    - values:

        | 0: Maintaining
        | 1: Climbing
        | 2: Descending
        | 3: Invalid

    **I020/170/MAH**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Horizontal manoeuvre

    **I020/170/STH**

    - 1 bit [``.``]

    - values:

        | 0: Measured position
        | 1: Smoothed position

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I020/170/GHO**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Ghost track

    **I020/170/(spare)**

    - 6 bits [``......``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Bit-8 (GHO) is used to signal that the track is suspected to
       have been generated by a fake target.

I020/202 - Calculated Track Velocity in Cartesian Coordinates
*************************************************************

*Definition*: Calculated track velocity expressed in Cartesian Coordinates, in two’s
complement representation.

*Structure*:

    **I020/202/VX**

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s

    **I020/202/VY**

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s



I020/210 - Calculated Acceleration
**********************************

*Definition*: Calculated Acceleration of the target, in two’s complement form.

*Structure*:

    **I020/210/AX**

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2
    - value :math:`>= -31` m/s2
    - value :math:`<= 31` m/s2

    **I020/210/AY**

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2
    - value :math:`>= -31` m/s2
    - value :math:`<= 31` m/s2


Notes:
    1. Maximum value means "maximum value or above"

I020/220 - Target Address
*************************

*Definition*: Target address (ICAO 24-bit address) assigned uniquely to each Target.

*Structure*:

- 24 bits [``........................``]

- raw value



I020/230 - Communications/ACAS Capability and Flight Status
***********************************************************

*Definition*: Communications capability of the transponder, capability of the on-board
ACAS equipment and flight status.

*Structure*:

    **I020/230/COM** - *Communications Capability of the Transponder*

    - 3 bits [``...``]

    - values:

        | 0: No communications capability (surveillance only)
        | 1: Comm. A and Comm. B capability
        | 2: Comm. A, Comm. B and Uplink ELM
        | 3: Comm. A, Comm. B, Uplink ELM and Downlink ELM
        | 4: Level 5 Transponder capability
        | 5: Not assigned
        | 6: Not assigned
        | 7: Not assigned

    **I020/230/STAT** - *Flight Status*

    - 3 bits [``...``]

    - values:

        | 0: No alert, no SPI, aircraft airborne
        | 1: No alert, no SPI, aircraft on ground
        | 2: Alert, no SPI, aircraft airborne
        | 3: Alert, no SPI, aircraft on ground
        | 4: Alert, SPI, aircraft airborne or on ground
        | 5: No alert, SPI, aircraft airborne or on ground
        | 6: Not assigned
        | 7: Information not yet extracted

    **I020/230/(spare)**

    - 2 bits [``..``]

    **I020/230/MSSC** - *Mode-S Specific Service Capability*

    - 1 bit [``.``]

    - values:

        | 0: No
        | 1: Yes

    **I020/230/ARC** - *Altitude Reporting Capability*

    - 1 bit [``.``]

    - values:

        | 0: 100 ft resolution
        | 1: 25 ft resolution

    **I020/230/AIC** - *Aircraft Identification Capability*

    - 1 bit [``.``]

    - values:

        | 0: No
        | 1: Yes

    **I020/230/B1A** - *BDS 1,0 Bit 16*

    - 1 bit [``.``]

    - raw value

    **I020/230/B1B** - *BDS 1,0 Bits 37/40*

    - 4 bits [``....``]

    - raw value



I020/245 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters.

*Structure*:

    **I020/245/STI**

    - 2 bits [``..``]

    - values:

        | 0: Callsign or registration not downlinked from transponder
        | 1: Registration downlinked from transponder
        | 2: Callsign downlinked from transponder
        | 3: Not defined

    **I020/245/(spare)**

    - 6 bits [``......``]

    **I020/245/CHR** - *Characters 1-8 (coded on 6 Bits each) Defining Target Identification*

    - 48 bits [``................................................``]

    - ICAO string (6-bits per character)


Notes:

    1. See ICAO document Annex 10, Volume IV, section 3.1.2.9 for the
       coding rules.

I020/250 - Mode S MB Data
*************************

*Definition*: Mode S Comm B data as extracted from the aircraft transponder.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I020/250/MBDATA** - *56-bit Message Conveying Mode S Comm B Message Data*

        - 56 bits [``........................................................``]

        - raw value

        **I020/250/BDS1** - *Comm B Data Buffer Store 1 Address*

        - 4 bits [``....``]

        - raw value

        **I020/250/BDS2** - *Comm B Data Buffer Store 2 Address*

        - 4 bits [``....``]

        - raw value


Notes:

    1. For the transmission of BDS20, item I020/245 is used.
    2. For the transmission of BDS30, item I020/260 is used.

I020/260 - ACAS Resolution Advisory Report
******************************************

*Definition*: Currently active Resolution Advisory (RA), if any, generated by the
ACAS associated with the transponder transmitting the report and
threat identity data.

*Structure*:

- 56 bits [``........................................................``]

- raw value


Notes:

    Refer to ICAO Draft SARPs for ACAS for detailed explanations.

I020/300 - Vehicle Fleet Identification
***************************************

*Definition*: Vehicle fleet identification number.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Unknown
    | 1: ATC equipment maintenance
    | 2: Airport maintenance
    | 3: Fire
    | 4: Bird scarer
    | 5: Snow plough
    | 6: Runway sweeper
    | 7: Emergency
    | 8: Police
    | 9: Bus
    | 10: Tug (push/tow)
    | 11: Grass cutter
    | 12: Fuel
    | 13: Baggage
    | 14: Catering
    | 15: Aircraft maintenance
    | 16: Flyco (follow me)



I020/310 - Pre-programmed Message
*********************************

*Definition*: Number related to a pre-programmed message that can be transmitted by a vehicle.

*Structure*:

    **I020/310/TRB**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: In Trouble

    **I020/310/MSG**

    - 7 bits [``.......``]

    - values:

        | 1: Towing aircraft
        | 2: FOLLOW-ME operation
        | 3: Runway check
        | 4: Emergency operation (fire, medical...)
        | 5: Work in progress (maintenance, birds scarer, sweepers...)



I020/400 - Contributing Devices
*******************************

*Definition*: Overview of Receiver Units, which have contributed to the Target Detection.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I020/400/BIT1** - *TU1/RU1 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU1/RU1 has NOT contributed to the target detection
            | 1: TU1/RU1 has contributed to the target detection

        **I020/400/BIT2** - *TU2/RU2 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU2/RU2 has NOT contributed to the target detection
            | 1: TU2/RU2 has contributed to the target detection

        **I020/400/BIT3** - *TU3/RU3 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU3/RU3 has NOT contributed to the target detection
            | 1: TU3/RU3 has contributed to the target detection

        **I020/400/BIT4** - *TU4/RU4 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU4/RU4 has NOT contributed to the target detection
            | 1: TU4/RU4 has contributed to the target detection

        **I020/400/BIT5** - *TU5/RU5 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU5/RU5 has NOT contributed to the target detection
            | 1: TU5/RU5 has contributed to the target detection

        **I020/400/BIT6** - *TU6/RU6 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU6/RU6 has NOT contributed to the target detection
            | 1: TU6/RU6 has contributed to the target detection

        **I020/400/BIT7** - *TU7/RU7 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU7/RU7 has NOT contributed to the target detection
            | 1: TU7/RU7 has contributed to the target detection

        **I020/400/BIT8** - *TU8/RU8 Contribution*

        - 1 bit [``.``]

        - values:

            | 0: TU8/RU8 has NOT contributed to the target detection
            | 1: TU8/RU8 has contributed to the target detection


Note:

    In case of more than 8 devices connected to the system, the numbering
    of the field "RUx Contribution" follows the standard ASTERIX rule:
    bits are numbered from right to left.
    The example below shows the case of a maximum of 16 devices with
    devices 1, 7 and 14 contributing to the target:

    <TODO: add table>

I020/500 - Position Accuracy
****************************

*Definition*: Standard Deviation of Position

*Structure*:

Compound item (FX)

    **I020/500/DOP** - *DOP of Position*

        **I020/500/DOP/X** - *DOP (X-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - LSB = :math:`1 / {2^{2}}`  = :math:`1 / {4}`  :math:`\approx 0.25`

        **I020/500/DOP/Y** - *DOP (Y-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - LSB = :math:`1 / {2^{2}}`  = :math:`1 / {4}`  :math:`\approx 0.25`

        **I020/500/DOP/XY** - *DOP (Correlation XY)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - LSB = :math:`1 / {2^{2}}`  = :math:`1 / {4}`  :math:`\approx 0.25`

    **I020/500/SDP** - *Standard Deviation of Position*

        **I020/500/SDP/X** - *SDP (X-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m"
        - LSB = :math:`1 / {2^{2}}` m = :math:`1 / {4}` m :math:`\approx 0.25` m

        **I020/500/SDP/Y** - *SDP (Y-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m"
        - LSB = :math:`1 / {2^{2}}` m = :math:`1 / {4}` m :math:`\approx 0.25` m

        **I020/500/SDP/XY** - *SDP (Correlation XY)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - LSB = :math:`1 / {2^{2}}`  = :math:`1 / {4}`  :math:`\approx 0.25`

    **I020/500/SDH** - *Standard Deviation of Geometric Height (WGS 84)*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m


Note:

    1. There is now a new Item for the Position Accuracy defined in
       the Reserved Expansion Field (REF), more complete (includes a
       Standard Deviation of Position in WGS-84) and is based on a
       different calculation method (covariance instead of correlation).
       It is recommended to use the new definition. Nevertheless, Item
       I020/500 is kept in order to prevent a full incompatibility with
       previous releases of ASTERIX Cat. 020 already implemented.

I020/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I020/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 020
=========================================
- (1) ``I020/010`` - Data Source Identifier
- (2) ``I020/020`` - Target Report Descriptor
- (3) ``I020/140`` - Time of Day
- (4) ``I020/041`` - Position In WGS-84 Coordinates
- (5) ``I020/042`` - Position in Cartesian Coordinates
- (6) ``I020/161`` - Track Number
- (7) ``I020/170`` - Track Status
- ``(FX)`` - Field extension indicator
- (8) ``I020/070`` - Mode-3/A Code in Octal Representation
- (9) ``I020/202`` - Calculated Track Velocity in Cartesian Coordinates
- (10) ``I020/090`` - Flight Level in Binary Representation
- (11) ``I020/100`` - Mode C Code
- (12) ``I020/220`` - Target Address
- (13) ``I020/245`` - Target Identification
- (14) ``I020/110`` - Measured Height (Local Cartesian Coordinates)
- ``(FX)`` - Field extension indicator
- (15) ``I020/105`` - Geometric Height (WGS-84)
- (16) ``I020/210`` - Calculated Acceleration
- (17) ``I020/300`` - Vehicle Fleet Identification
- (18) ``I020/310`` - Pre-programmed Message
- (19) ``I020/500`` - Position Accuracy
- (20) ``I020/400`` - Contributing Devices
- (21) ``I020/250`` - Mode S MB Data
- ``(FX)`` - Field extension indicator
- (22) ``I020/230`` - Communications/ACAS Capability and Flight Status
- (23) ``I020/260`` - ACAS Resolution Advisory Report
- (24) ``I020/030`` - Warning/Error Conditions
- (25) ``I020/055`` - Mode-1 Code in Octal Representation
- (26) ``I020/050`` - Mode-2 Code in Octal Representation
- (27) ``I020/RE`` - Reserved Expansion Field
- (28) ``I020/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

