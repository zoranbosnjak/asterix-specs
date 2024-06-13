Asterix category 010 - Transmission of Monosensor Surface Movement Data
=======================================================================
**category**: 010

**edition**: 1.1

**date**: 2007-03-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I010/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages
at the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Target Report
    | 2: Start of Update Cycle
    | 3: Periodic Status Message
    | 4: Event-triggered Status Message

Notes:

    1. In applications where transactions of various types are exchanged,
       the Message Type Data Item facilitates the proper message handling
       at the receiver side.
    2. All Message Type values are reserved for common standard use.
    3. The list of items present for the four message types is defined in
       the following table.
       M stands for mandatory, O for optional, X for never present.

The list of items present for the four message types is defined in the following
table. M stands for mandatory, O for optional, X for never present. ::

    Item Type [001, 002, 003, 004]
              [Target Report, Start of Update Cycle, Periodic Status Message, Event Status Message]

    I010/000 Message Type M M M M
    I010/010 Data Source Identifier M M M M
    I010/020 Target Report Descriptor M X X X
    I010/040 Measured Position in Polar Coordinates O X X X
    I010/041 Position in WGS-84 Coordinates O X X X
    I010/042 Position in Cartesian Coordinates O X X X
    I010/060 Mode-3/A Code O X X X
    I010/090 Flight Level in Binary Representation O X X X
    I010/091 Measured Height O X X X
    I010/131 Amplitude of Primary Plot O X X X
    I010/140 Time of Day M M M M
    I010/161 Track Number O X X X
    I010/170 Track Status O X X X
    I010/200 Calculated Track Velocity in Polar Coordinates O X X X
    I010/202 Calculated Track Velocity in Cartesian Coordinates O X X X
    I010/210 Calculated Acceleration O X X X
    I010/220 Target Address O X X X
    I010/245 Target Identification O X X X
    I010/250 Mode S MB Data O X X X
    I010/270 Target Size & Orientation O X X X
    I010/280 Presence O X X X
    I010/300 Vehicle Fleet Identification O X X X
    I010/310 Pre-programmed Message O X X X
    I010/500 Standard Deviation of Position O X X X
    I010/550 System Status X O M M

I010/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system from which the data are received.

*Structure*:

    **I010/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I010/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

NOTE - The SAC is fixed to zero to indicate a data flow local to the airport.

I010/020 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the data as transmitted by a system.

*Structure*:

Extended item.

    **I010/020/TYP**

    - 3 bits [``...``]

    - values:

        | 0: SSR multilateration
        | 1: Mode S multilateration
        | 2: ADS-B
        | 3: PSR
        | 4: Magnetic Loop System
        | 5: HF multilateration
        | 6: Not defined
        | 7: Other types

    **I010/020/DCR**

    - 1 bit [``.``]

    - values:

        | 0: No differential correction (ADS-B)
        | 1: Differential correction (ADS-B)

    **I010/020/CHN**

    - 1 bit [``.``]

    - values:

        | 0: Chain 1
        | 1: Chain 2

    **I010/020/GBS**

    - 1 bit [``.``]

    - values:

        | 0: Transponder Ground bit not set
        | 1: Transponder Ground bit set

    **I010/020/CRT**

    - 1 bit [``.``]

    - values:

        | 0: No Corrupted reply in multilateration
        | 1: Corrupted replies in multilateration

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/020/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual target report
        | 1: Simulated target report

    **I010/020/TST**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test Target

    **I010/020/RAB**

    - 1 bit [``.``]

    - values:

        | 0: Report from target transponder
        | 1: Report from field monitor (fixed transponder)

    **I010/020/LOP**

    - 2 bits [``..``]

    - values:

        | 0: Undetermined
        | 1: Loop start
        | 2: Loop finish

    **I010/020/TOT**

    - 2 bits [``..``]

    - values:

        | 0: Undetermined
        | 1: Aircraft
        | 2: Ground vehicle
        | 3: Helicopter

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/020/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Absence of SPI
        | 1: Special Position Identification

    **I010/020/(spare)**

    - 6 bits [``......``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

I010/040 - Measured Position in Polar Co-ordinates
**************************************************

*Definition*: Measured position of a target in local polar co-ordinates.

*Structure*:

    **I010/040/RHO** - *RHO*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1` m
    - value :math:`<= 65536` m

    **I010/040/TH** - *Theta*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

I010/041 - Position in WGS-84 Co-ordinates
******************************************

*Definition*: Position of a target in WGS-84 Co-ordinates.

*Structure*:

    **I010/041/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I010/041/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

I010/042 - Position in Cartesian Co-ordinates
*********************************************

*Definition*: Position of a target in Cartesian co-ordinates, in two’s complement form.

*Structure*:

    **I010/042/X** - *X Coordinate*

    - 16 bits [``................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1` m
    - value :math:`>= -32768` m
    - value :math:`<= 32768` m

    **I010/042/Y** - *Y Coordinate*

    - 16 bits [``................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1` m
    - value :math:`>= -32768` m
    - value :math:`<= 32768` m

I010/060 - Mode-3/A Code in Octal Representation
************************************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I010/060/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I010/060/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I010/060/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Mode-3/A code not extracted during the last scan

    **I010/060/(spare)**

    - 1 bit [``.``]

    **I010/060/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)

Notes:

    1. Bit 15 has no meaning in the case of a smoothed Mode-3/A code
       and is set to 0 for a calculated track. For Mode S, it is set
       to one when an error correction has been attempted.
    2. For Mode S, bit 16 is normally set to zero, but can exceptionally
       be set to one to indicate a non-validated Mode-3/A code (e.g. alert
       condition detected, but new Mode-3/A code not successfully extracted).

I010/090 - Flight Level in Binary Representation
************************************************

*Definition*: Flight Level (Mode C / Mode S Altitude) converted into binary two's
complement representation.

*Structure*:

    **I010/090/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I010/090/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I010/090/FL** - *Flight Level*

    - 14 bits [``..............``]

    - signed quantity
    - unit: "FL"
    - LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL

Notes:

    1. The value shall be within the range described by ICAO Annex 10
    2. For Mode S, bit 15 (G) is set to one when an error correction has
       been attempted.

I010/091 - Measured Height
**************************

*Definition*: Height above local 2D co-ordinate reference system (two's complement)
based on direct measurements not related to barometric pressure.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "ft"
- LSB = :math:`25/2^2` ft :math:`\approx 6.25` ft
- value :math:`>= -204800` ft
- value :math:`<= 204800` ft

I010/131 - Amplitude of Primary Plot
************************************

*Definition*: Amplitude of Primary Plot.

*Structure*:

- 8 bits [``........``]

- raw value

Notes:

    - The value is radar-dependent, 0 being the minimum detectable level
      for that radar.

I010/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

Notes:

    - The time of day value is reset to zero each day at midnight.

I010/161 - Track Number
***********************

*Definition*: An integer value representing a unique reference to a track record
within a particular track file.

*Structure*:

    **I010/161/(spare)**

    - 4 bits [``....``]

    **I010/161/TRK** - *Track Number*

    - 12 bits [``............``]

    - raw value

I010/170 - Track Status
***********************

*Definition*: Status of track.

*Structure*:

Extended item.

    **I010/170/CNF**

    - 1 bit [``.``]

    - values:

        | 0: Confirmed track
        | 1: Track in initialisation phase

    **I010/170/TRE**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Last report for a track

    **I010/170/CST**

    - 2 bits [``..``]

    - values:

        | 0: No extrapolation
        | 1: Predictable extrapolation due to sensor refresh period (see NOTE)
        | 2: Predictable extrapolation in masked area
        | 3: Extrapolation due to unpredictable absence of detection

    **I010/170/MAH**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Horizontal manoeuvre

    **I010/170/TCC**

    - 1 bit [``.``]

    - values:

        | 0: Tracking performed in 'Sensor Plane', i.e. neither slant range correction nor projection was applied
        | 1: Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Sensor Site co-ordinates

    **I010/170/STH**

    - 1 bit [``.``]

    - values:

        | 0: Measured position
        | 1: Smoothed position

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/170/TOM**

    - 2 bits [``..``]

    - values:

        | 0: Unknown type of movement
        | 1: Taking-off
        | 2: Landing
        | 3: Other types of movement

    **I010/170/DOU**

    - 3 bits [``...``]

    - values:

        | 0: No doubt
        | 1: Doubtful correlation (undetermined reason)
        | 2: Doubtful correlation in clutter
        | 3: Loss of accuracy
        | 4: Loss of accuracy in clutter
        | 5: Unstable track
        | 6: Previously coasted

    **I010/170/MRS**

    - 2 bits [``..``]

    - values:

        | 0: Merge or split indication undetermined
        | 1: Track merged by association to plot
        | 2: Track merged by non-association to plot
        | 3: Split track

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/170/GHO**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Ghost track

    **I010/170/(spare)**

    - 6 bits [``......``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Notes:

    1. Some sensors are not be able to scan the whole coverage in one
       refresh period. Therefore, track extrapolation is performed in
       un-scanned sectors. CST is then set to 01.
    2. Bit-8 (GHO) is used to signal that the track is suspected to have
       been generated by a fake target.

I010/200 - Calculated Track Velocity in Polar Co-ordinates
**********************************************************

*Definition*: Calculated track velocity expressed in polar co-ordinates.

*Structure*:

    **I010/200/GSP** - *Ground Speed*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "NM/s"
    - LSB = :math:`1/2^14` NM/s :math:`\approx 6.10e-5` NM/s
    - value :math:`<= 2` NM/s

    **I010/200/TRA** - *Track Angle*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

I010/202 - Calculated Track Velocity in Cartesian Co-ordinates
**************************************************************

*Definition*: Calculated track velocity expressed in Cartesian co-ordinates, in two’s
complement representation.

*Structure*:

    **I010/202/VX** - *X Velocity*

    - 16 bits [``................``]

    - signed quantity
    - unit: "m/s"
    - LSB = :math:`1/2^4` m/s :math:`\approx 6.25e-2` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s

    **I010/202/VY** - *Y Velocity*

    - 16 bits [``................``]

    - signed quantity
    - unit: "m/s"
    - LSB = :math:`1/2^4` m/s :math:`\approx 6.25e-2` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s

I010/210 - Calculated Acceleration
**********************************

*Definition*: Calculated Acceleration of the target, in two’s complement form.

*Structure*:

    **I010/210/AX** - *X Acceleration*

    - 8 bits [``........``]

    - signed quantity
    - unit: "m/s²"
    - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
    - value :math:`>= -31` m/s²
    - value :math:`<= 31` m/s²

    **I010/210/AY** - *Y Acceleration*

    - 8 bits [``........``]

    - signed quantity
    - unit: "m/s²"
    - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
    - value :math:`>= -31` m/s²
    - value :math:`<= 31` m/s²

I010/220 - Target Address
*************************

*Definition*: Target address (24-bits address) assigned uniquely to each Target.

*Structure*:

- 24 bits [``........................``]

- raw value

I010/245 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters.

*Structure*:

    **I010/245/STI**

    - 2 bits [``..``]

    - values:

        | 0: Callsign or registration downlinked from transponder
        | 1: Callsign not downlinked from transponder
        | 2: Registration not downlinked from transponder

    **I010/245/(spare)**

    - 6 bits [``......``]

    **I010/245/CHR** - *Characters 1-8 (Coded on 6 Bits Each) Defining Target Identification*

    - 48 bits [``... 48 bits ...``]

    - ICAO string (6-bits per character)

Notes:

    - See ICAO document Annex 10, Volume I, Part I, section 3.8.2.9 for
      the coding rules.

I010/250 - Mode S MB Data
*************************

*Definition*: Mode S Comm B data as extracted from the aircraft transponder.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I010/250/MBDATA**

        56-bit message conveying Mode S Comm B message data

        - 56 bits [``... 56 bits ...``]

        - raw value

        **I010/250/BDS1**

        Comm B Data Buffer Store 1 Address

        - 4 bits [``....``]

        - raw value

        **I010/250/BDS2**

        Comm B Data Buffer Store 2 Address

        - 4 bits [``....``]

        - raw value

Notes:

    - For the transmission of BDS20, item 245 is used.

I010/270 - Target Size and Orientation
**************************************

*Definition*: Target size defined as length and width of the detected target, and orientation.

*Structure*:

Extended item.

    **I010/270/LENGTH** - *Length*

    - 7 bits [``.......``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/270/ORIENTATION** - *Orientation*

    - 7 bits [``.......``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^7` ° :math:`\approx 2.81` °

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I010/270/WIDTH** - *Width*

    - 7 bits [``.......``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Notes:

    - The orientation gives the direction which the aircraft nose is
      pointing, relative to the Geographical North.

I010/280 - Presence
*******************

*Definition*: Positions of all elementary presences constituting a plot.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I010/280/DRHO**

        Difference between the radial distance of the plot centre
        and that of the presence.

        - 8 bits [``........``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1` m
        - value :math:`>= -127` m
        - value :math:`<= 127` m

        **I010/280/DTHETA**

        Difference between the azimuth of the plot centre and that
        of the presence.

        - 8 bits [``........``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`3/20` ° :math:`\approx 0.15` °
        - value :math:`>= -381/20` °
        - value :math:`<= 381/20` °

I010/300 - Vehicle Fleet Identification
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

I010/310 - Pre-programmed Message
*********************************

*Definition*: Number related to a pre-programmed message that can be transmitted by a vehicle.

*Structure*:

    **I010/310/TRB**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: In Trouble

    **I010/310/MSG**

    - 7 bits [``.......``]

    - values:

        | 1: Towing aircraft
        | 2: “Follow me” operation
        | 3: Runway check
        | 4: Emergency operation (fire, medical...)
        | 5: Work in progress (maintenance, birds scarer, sweepers...)

I010/500 - Standard Deviation of Position
*****************************************

*Definition*: Standard Deviation of Position

*Structure*:

    **I010/500/DEVX** - *Standard Deviation of X Component*

    - 8 bits [``........``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2^2` m :math:`\approx 0.25` m

    **I010/500/DEVY** - *Standard Deviation of Y Component*

    - 8 bits [``........``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2^2` m :math:`\approx 0.25` m

    **I010/500/COVXY** - *Covariance in Two’s Complement Form*

    - 16 bits [``................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/2^2` m :math:`\approx 0.25` m

I010/550 - System Status
************************

*Definition*: Information concerning the configuration and status of a System.

*Structure*:

    **I010/550/NOGO** - *Operational Release Status of the System*

    - 2 bits [``..``]

    - values:

        | 0: Operational
        | 1: Degraded
        | 2: NOGO

    **I010/550/OVL** - *Overload Indicator*

    - 1 bit [``.``]

    - values:

        | 0: No overload
        | 1: Overload

    **I010/550/TSV** - *Time Source Validity*

    - 1 bit [``.``]

    - values:

        | 0: Valid
        | 1: Invalid

    **I010/550/DIV**

    - 1 bit [``.``]

    - values:

        | 0: Normal Operation
        | 1: Diversity degraded

    **I010/550/TTF**

    - 1 bit [``.``]

    - values:

        | 0: Test Target Operative
        | 1: Test Target Failure

    **I010/550/(spare)**

    - 2 bits [``..``]

Notes:

    - For a radar, bit-4 (DIV) is set to zero either when diversity is
      not used, or when diversity is used and operational.

I010/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item (RE)

I010/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 010
=========================================
- (1) ``I010/010`` - Data Source Identifier
- (2) ``I010/000`` - Message Type
- (3) ``I010/020`` - Target Report Descriptor
- (4) ``I010/140`` - Time of Day
- (5) ``I010/041`` - Position in WGS-84 Co-ordinates
- (6) ``I010/040`` - Measured Position in Polar Co-ordinates
- (7) ``I010/042`` - Position in Cartesian Co-ordinates
- ``(FX)`` - Field extension indicator
- (8) ``I010/200`` - Calculated Track Velocity in Polar Co-ordinates
- (9) ``I010/202`` - Calculated Track Velocity in Cartesian Co-ordinates
- (10) ``I010/161`` - Track Number
- (11) ``I010/170`` - Track Status
- (12) ``I010/060`` - Mode-3/A Code in Octal Representation
- (13) ``I010/220`` - Target Address
- (14) ``I010/245`` - Target Identification
- ``(FX)`` - Field extension indicator
- (15) ``I010/250`` - Mode S MB Data
- (16) ``I010/300`` - Vehicle Fleet Identification
- (17) ``I010/090`` - Flight Level in Binary Representation
- (18) ``I010/091`` - Measured Height
- (19) ``I010/270`` - Target Size and Orientation
- (20) ``I010/550`` - System Status
- (21) ``I010/310`` - Pre-programmed Message
- ``(FX)`` - Field extension indicator
- (22) ``I010/500`` - Standard Deviation of Position
- (23) ``I010/280`` - Presence
- (24) ``I010/131`` - Amplitude of Primary Plot
- (25) ``I010/210`` - Calculated Acceleration
- (26) ``(spare)``
- (27) ``I010/SP`` - Special Purpose Field
- (28) ``I010/RE`` - Reserved Expansion Field
- ``(FX)`` - Field extension indicator
