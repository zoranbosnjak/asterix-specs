Asterix category 021 - ADS-B Target Reports
===========================================
**category**: 021

**edition**: 0.25

**date**: 2005-03-01

Preamble
--------
Surveillance data exchange.
ADS-B Target Reports.

Description of standard data items
----------------------------------

I021/010 - Data Source Identification
*************************************

*Definition*: Identification of the ADS-B station providing information.

*Structure*:

    **I021/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I021/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note:
    - The up-to-date list of SACs is published on the EUROCONTROL
      ASTERIX Web Site
      (http://www.eurocontrol.int/services/system-area-code-list).

I021/020 - Emitter Category
***************************

*Definition*: Characteristics of the originating ADS-B unit.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Light aircraft <= 7000 kg
    | 2: Reserved
    | 3: 7000 kg < Medium aircraft < 136000 kg
    | 4: Reserved
    | 5: 136000 kg <= Heavy aircraft
    | 6: Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)
    | 7: Reserved
    | 8: Reserved
    | 9: Reserved
    | 10: Rotocraft
    | 11: Glider / sailplane
    | 12: Lighter-than-air
    | 13: Unmanned aerial vehicle
    | 14: Space / transatmospheric vehicle
    | 15: Ultralight / handglider / paraglider
    | 16: Parachutist / skydiver
    | 17: Reserved
    | 18: Reserved
    | 19: Reserved
    | 20: Surface emergency vehicle
    | 21: Surface service vehicle
    | 22: Fixed ground or tethered obstruction
    | 23: Reserved
    | 24: Reserved

I021/030 - Time of Day
**********************

*Definition*: Time of applicability (measurement) of the reported position, in the form of elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

The time of the day value is reset to zero at every midnight.

I021/032 - Time of Day Accuracy
*******************************

*Definition*: The maximum difference between the actual time of applicability of the reported position and the time reported in the Time of Day item (I021/030).

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^8` s :math:`\approx 3.91e-3` s

I021/040 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the data as transmitted by a system.

*Structure*:

    **I021/040/DCR** - *Differential Correction*

    - 1 bit [``.``]

    - values:

        | 0: No differential correction (ADS-B)
        | 1: Differential correction (ADS-B)

    **I021/040/GBS** - *Ground Bit Setting*

    - 1 bit [``.``]

    - values:

        | 0: Ground Bit not set
        | 1: Ground Bit set

    **I021/040/SIM** - *Simulated Target*

    - 1 bit [``.``]

    - values:

        | 0: Actual target report
        | 1: Simulated target report

    **I021/040/TST** - *Test Target*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test Target

    **I021/040/RAB** - *Report Type*

    - 1 bit [``.``]

    - values:

        | 0: Report from target transponder
        | 1: Report from field monitor (fixed transponder)

    **I021/040/SAA** - *Selected Altitude Available*

    - 1 bit [``.``]

    - values:

        | 0: Equipment capable to provide Selected Altitude
        | 1: Equipment not capable to provide Selected Altitude

    **I021/040/SPI** - *Special Position Identification*

    - 1 bit [``.``]

    - values:

        | 0: Absence of SPI
        | 1: Special Position Identification

    **I021/040/(spare)**

    - 1 bit [``.``]

    **I021/040/ATP** - *Address Type*

    - 3 bits [``...``]

    - values:

        | 0: Non unique address
        | 1: 24-Bit ICAO address
        | 2: Surface vehicle address
        | 3: Anonymous address
        | 4: Reserved for future use
        | 5: Reserved for future use
        | 6: Reserved for future use
        | 7: Reserved for future use

    **I021/040/ARC** - *Altitude Reporting Capability*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: 25 ft
        | 2: 100 ft

    **I021/040/(spare)**

    - 3 bits [``...``]

I021/080 - Target Address
*************************

*Definition*: Target address (emitter identifier) assigned uniquely to each target.

*Structure*:

- 24 bits [``........................``]

- raw value

I021/090 - Figure of Merit
**************************

*Definition*: ADS figure of merit (FOM) provided by the aircraft avionics.

*Structure*:

    **I021/090/AC** - *ACAS Capabilities*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: ACAS not operational
        | 2: ACAS operartional
        | 3: Invalid

    **I021/090/MN** - *Multiple Navigation Aids*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: Multiple Navigation not operational
        | 2: Multiple Navigation operartional
        | 3: Invalid

    **I021/090/DC** - *Differencial Correction*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: Differencial Correction
        | 2: NO Differencial Correction
        | 3: Invalid

    **I021/090/(spare)**

    - 6 bits [``......``]

    **I021/090/PA** - *Position Accuracy*

    - 4 bits [``....``]

    - signed quantity
    - LSB = :math:`1`

Note:
    bits-4/1 (PA) code the “Navigational Uncertainty Categories –
    Position” as described in the ADS-B MASPS [Ref. 3]

I021/095 - Velocity Accuracy
****************************

*Definition*: Velocity uncertainty category of the least accurate velocity

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    bits-8/1 code the “Navigational Uncertainty Categories – Velocity”
    as described in the ADS-B MASPS [Ref. 3]

I021/110 - Trajectory Intent
****************************

*Definition*: Reports indicating the 4D intended trajectory of the aircraft.

*Structure*:

Compound item (FX)

    **I021/110/TIS** - *Trajectory Intent Status*

    Extended item.

        **I021/110/TIS/NAV**

        - 1 bit [``.``]

        - values:

            | 0: Trajectory Intent Data is available for this aircraft
            | 1: Trajectory Intent Data is not available for this aircraft

        **I021/110/TIS/NVB**

        - 1 bit [``.``]

        - values:

            | 0: Trajectory Intent Data is valid
            | 1: Trajectory Intent Data is not valid

        **I021/110/TIS/(spare)**

        - 5 bits [``.....``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I021/110/TID** - *Trajectory Intent Data*

    Repetitive item, repetition factor 8 bits.

            **I021/110/TID/TCA**

            - 1 bit [``.``]

            - values:

                | 0: TCP number available
                | 1: TCP number not available

            **I021/110/TID/NC**

            - 1 bit [``.``]

            - values:

                | 0: TCP compliance
                | 1: TCP non-compliance

            **I021/110/TID/TCPN**

            Trajectory Change Point number

            - 6 bits [``......``]

            - raw value

            **I021/110/TID/ALT** - *Altitude in Two's Complement Form*

            - 16 bits [``................``]

            - signed quantity
            - unit: "ft"
            - LSB = :math:`10` ft
            - value :math:`>= -1500` ft
            - value :math:`<= 150000` ft

            **I021/110/TID/LAT** - *In WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -90` °
            - value :math:`<= 90` °

            **I021/110/TID/LON** - *In WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -180` °
            - value :math:`< 180` °

            **I021/110/TID/PT** - *Point Type*

            - 4 bits [``....``]

            - values:

                | 0: Unknown
                | 1: Fly by waypoint (LT)
                | 2: Fly over waypoint (LT)
                | 3: Hold pattern (LT)
                | 4: Procedure hold (LT)
                | 5: Procedure turn (LT)
                | 6: RF leg (LT)
                | 7: Top of climb (VT)
                | 8: Top of descent (VT)
                | 9: Start of level (VT)
                | 10: Cross-over altitude (VT)
                | 11: Transition altitude (VT)

            **I021/110/TID/TD**

            - 2 bits [``..``]

            - values:

                | 0: N/A
                | 1: Turn right
                | 2: Turn left
                | 3: No turn

            **I021/110/TID/TRA**

            Turn Radius Availability

            - 1 bit [``.``]

            - values:

                | 0: TTR not available
                | 1: TTR available

            **I021/110/TID/TOA**

            - 1 bit [``.``]

            - values:

                | 0: TOV available
                | 1: TOV not available

            **I021/110/TID/TOV** - *Time Over Point*

            - 24 bits [``........................``]

            - unsigned quantity
            - unit: "s"
            - LSB = :math:`1` s

            **I021/110/TID/TTR** - *TCP Turn Radius*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/100` NM :math:`\approx 1.00e-2` NM
            - value :math:`>= 0` NM
            - value :math:`<= 13107/20` NM

Notes:

    1. NC is set to one when the aircraft will not fly the path described
       by the TCP data.
    2. TCP numbers start from zero.
    3. LT = Lateral Type
    4. VT = Vertical Type
    5. TOV gives the estimated time before reaching the point. It is
       defined as the absolute time from midnight.
    6. TOV is meaningful only if TOA is set to 1.

I021/130 - Position in WGS-84 Co-ordinates
******************************************

*Definition*: Calculated Position in WGS-84 Co-ordinates with a resolution of 180/(2^25) degrees.

*Structure*:

    **I021/130/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I021/130/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

Notes:

    1. Positive longitude indicates East. Positive latitude indicates North.
    2. The LSB provides a resolution at least better than 0.6m.

I021/140 - Geometric Altitude
*****************************

*Definition*: Vertical distance between the target and the projection of its position
on the earth’s ellipsoid, as defined by WGS84, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "ft"
- LSB = :math:`25/2^2` ft :math:`\approx 6.25` ft
- value :math:`>= -1500` ft
- value :math:`< 150000` ft

Note:
    1. LSB is required to be less than 10 ft by ICAO.

I021/145 - Flight Level
***********************

*Definition*: Flight Level from barometric measurements,not QNH corrected, in two’s
complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "FL"
- LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL
- value :math:`>= -15` FL
- value :math:`< 1500` FL

I021/146 - Intermediate State Selected Altitude
***********************************************

*Definition*: The short-term vertical intent as described by either the FMS selected
altitude, the Altitude Control Panel Selected Altitude, or the current
aircraft altitude according to the aircraft's mode of flight.

*Structure*:

    **I021/146/SAS** - *Source Availability*

    - 1 bit [``.``]

    - values:

        | 0: No source information provided
        | 1: Source Information provided

    **I021/146/SRC** - *Source*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: Aircraft Altitude (Holding Altitude)
        | 2: MCP/FCU Selected Altitude
        | 3: FMS Selected Altitude

    **I021/146/ALT** - *Altitude*

    - 13 bits [``.............``]

    - signed quantity
    - unit: "ft"
    - LSB = :math:`25` ft
    - value :math:`>= -1300` ft
    - value :math:`< 100000` ft

I021/148 - Final State Selected Altitude
****************************************

*Definition*: The vertical intent value that corresponds with the ATC cleared altitude,
as derived from the Altitude Control Panel (MCP/FCU).

*Structure*:

    **I021/148/MV** - *Manage Vertical Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active
        | 1: Active

    **I021/148/AH** - *Altitude Hold Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active
        | 1: Active

    **I021/148/AM** - *Approach Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active
        | 1: Active

    **I021/148/ALT** - *Altitude*

    - 13 bits [``.............``]

    - signed quantity
    - unit: "ft"
    - LSB = :math:`25` ft
    - value :math:`>= -1300` ft
    - value :math:`< 100000` ft

I021/150 - Air Speed
********************

*Definition*: Calculated Air Speed (Element of Air Vector).

*Structure*:

    **I021/150/IM**

    - 1 bit [``.``]

    - values:

        | 0: Air Speed = IAS, LSB (Bit-1) = 2 -14 NM/s
        | 1: Air Speed = Mach, LSB (Bit-1) = 0.001

    **I021/150/AS** - *Air Speed (IAS or Mach)*

    - 15 bits [``...............``]

    * Depends on the value of ``150/IM``.
    * In case of ``150/IM == 0``:
        - unsigned quantity
        - unit: "NM/s"
        - LSB = :math:`1/2^14` NM/s :math:`\approx 6.10e-5` NM/s

    * In case of ``150/IM == 1``:
        - unsigned quantity
        - unit: "Mach"
        - LSB = :math:`1/1000` Mach :math:`\approx 1.00e-3` Mach

    * Default:
        - raw value

I021/151 - True Airspeed
************************

*Definition*: True Air Speed.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- unit: "kt"
- LSB = :math:`1` kt

I021/152 - Magnetic Heading
***************************

*Definition*: Magnetic Heading (Element of Air Vector).

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- unit: "°"
- LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

I021/155 - Barometric Vertical Rate
***********************************

*Definition*: Barometric Vertical Rate, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "ft/min"
- LSB = :math:`25/2^2` ft/min :math:`\approx 6.25` ft/min

I021/157 - Geometric Vertical Rate
**********************************

*Definition*: Geometric Vertical Rate, in two’s complement form, with reference to WGS-84.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "ft/min"
- LSB = :math:`25/2^2` ft/min :math:`\approx 6.25` ft/min

I021/160 - Ground Vector
************************

*Definition*: Ground Speed and Track Angle elements of Ground Vector.

*Structure*:

    **I021/160/GS** - *Ground Speed in Two's Complement Form Referenced to WGS84*

    - 16 bits [``................``]

    - signed quantity
    - unit: "NM/s"
    - LSB = :math:`1/2^14` NM/s :math:`\approx 6.10e-5` NM/s
    - value :math:`>= 0` NM/s
    - value :math:`< 2` NM/s

    **I021/160/TA** - *Track Angle*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

I021/165 - Rate Of Turn
***********************

*Definition*: Rate of Turn, in two’s complement form.

*Structure*:

Extended item.

    **I021/165/TI** - *Turn Indicator*

    - 2 bits [``..``]

    - values:

        | 0: Not available
        | 1: Left
        | 2: Right
        | 3: Straight

    **I021/165/(spare)**

    - 5 bits [``.....``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/165/ROT** - *Rate of Turn*

    - 7 bits [``.......``]

    - signed quantity
    - unit: "°/s"
    - LSB = :math:`1/2^2` °/s :math:`\approx 0.25` °/s
    - value :math:`<= 15` °/s

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Notes:
    1. A positive value represents a right turn, whereas a negative value
       represents a left turn.
    2. Value 15 means 15 °/s or above.

I021/170 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters, as reported
by the target.

*Structure*:

- 48 bits [``... 48 bits ...``]

- ICAO string (6-bits per character)

I021/200 - Target Status
************************

*Definition*: Status of the target

*Structure*:

- 8 bits [``........``]

- values:

    | 0: No emergency / not reported
    | 1: General emergency
    | 2: Lifeguard / medical
    | 3: Minimum fuel
    | 4: No communications
    | 5: Unlawful interference

I021/210 - Link Technology Indicator
************************************

*Definition*: Indication of which ADS link technology has been used to send the target
report.

*Structure*:

    **I021/210/(spare)**

    - 3 bits [``...``]

    **I021/210/DTI** - *Cockpit Display of Traffic Information*

    - 1 bit [``.``]

    - values:

        | 0: Unknown
        | 1: Aircraft equiped with CDTI

    **I021/210/MDS** - *Mode-S Extended Squitter*

    - 1 bit [``.``]

    - values:

        | 0: Not used
        | 1: Used

    **I021/210/UAT** - *UAT*

    - 1 bit [``.``]

    - values:

        | 0: Not used
        | 1: Used

    **I021/210/VDL** - *VDL Mode 4*

    - 1 bit [``.``]

    - values:

        | 0: Not used
        | 1: Used

    **I021/210/OTR** - *Other Technology*

    - 1 bit [``.``]

    - values:

        | 0: Not used
        | 1: Used

I021/220 - Met Information
**************************

*Definition*: Meteorological information.

*Structure*:

Compound item (FX)

    **I021/220/WS** - *Wind Speed*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "kt"
    - LSB = :math:`1` kt
    - value :math:`>= 0` kt
    - value :math:`<= 300` kt

    **I021/220/WD** - *Wind Direction*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`1` °
    - value :math:`>= 1` °
    - value :math:`<= 360` °

    **I021/220/TMP** - *Temperature*

    - 16 bits [``................``]

    - signed quantity
    - unit: "°C"
    - LSB = :math:`1/2^2` °C :math:`\approx 0.25` °C
    - value :math:`>= -100` °C
    - value :math:`<= 100` °C

    **I021/220/TRB** - *Turbulence*

    - 8 bits [``........``]

    - unsigned integer
    - value :math:`>= 0`
    - value :math:`<= 15`

I021/230 - Roll Angle
*********************

*Definition*: The roll angle, in two’s complement form, of an aircraft executing a turn.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "°"
- LSB = :math:`1/100` ° :math:`\approx 1.00e-2` °
- value :math:`>= -180` °
- value :math:`<= 180` °

Notes:
    1. Negative Value indicates “Left Wing Down”.
    2. Resolution provided by the technology “1090 MHz Extended Squitter”
       is 1 degree.

I021/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item (RE)

I021/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 021
=========================================
- (1) ``I021/010`` - Data Source Identification
- (2) ``I021/040`` - Target Report Descriptor
- (3) ``I021/030`` - Time of Day
- (4) ``I021/130`` - Position in WGS-84 Co-ordinates
- (5) ``I021/080`` - Target Address
- (6) ``I021/140`` - Geometric Altitude
- (7) ``I021/090`` - Figure of Merit
- ``(FX)`` - Field extension indicator
- (8) ``I021/210`` - Link Technology Indicator
- (9) ``I021/230`` - Roll Angle
- (10) ``I021/145`` - Flight Level
- (11) ``I021/150`` - Air Speed
- (12) ``I021/151`` - True Airspeed
- (13) ``I021/152`` - Magnetic Heading
- (14) ``I021/155`` - Barometric Vertical Rate
- ``(FX)`` - Field extension indicator
- (15) ``I021/157`` - Geometric Vertical Rate
- (16) ``I021/160`` - Ground Vector
- (17) ``I021/165`` - Rate Of Turn
- (18) ``I021/170`` - Target Identification
- (19) ``I021/095`` - Velocity Accuracy
- (20) ``I021/032`` - Time of Day Accuracy
- (21) ``I021/200`` - Target Status
- ``(FX)`` - Field extension indicator
- (22) ``I021/020`` - Emitter Category
- (23) ``I021/220`` - Met Information
- (24) ``I021/146`` - Intermediate State Selected Altitude
- (25) ``I021/148`` - Final State Selected Altitude
- (26) ``I021/110`` - Trajectory Intent
- (27) ``(spare)``
- (28) ``(spare)``
- ``(FX)`` - Field extension indicator
- (29) ``(spare)``
- (30) ``(spare)``
- (31) ``(spare)``
- (32) ``(spare)``
- (33) ``(spare)``
- (34) ``I021/RE`` - Reserved Expansion Field
- (35) ``I021/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator
