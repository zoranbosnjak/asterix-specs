Asterix category 011 - Transmission of A-SMGCS Data
===================================================
**category**: 011

**edition**: 1.3

**date**: 2020-05-11

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I011/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages
at the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Target reports, flight plan data and basic alerts
    | 2: Manual attachment of flight plan to track
    | 3: Manual detachment of flight plan to track
    | 4: Insertion of flight plan data
    | 5: Suppression of flight plan data
    | 6: Modification of flight plan data
    | 7: Holdbar status



I011/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I011/010/SAC** - *System Area Code Fixed to Zero*

    - 8 bits [``........``]

    - raw value

    **I011/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    The SAC is fixed to zero to indicate a data flow local to the airport.

I011/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value


Note:
    The service identification is allocated by the A-SMGCS

I011/041 - Position in WGS-84 Coordinates
*****************************************

*Definition*: Position of a target in WGS-84 Coordinates.

*Structure*:

    **I011/041/LAT** - *Latitude in WGS-84 in Two's Complement*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 31
    - unit: "deg"
    - LSB = :math:`180 / {2^{31}}` deg = :math:`180 / {2147483648}` deg :math:`\approx 8.381903171539307e-08` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I011/041/LON** - *Longitude in WGS-84 in Two's Complement*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 31
    - unit: "deg"
    - LSB = :math:`180 / {2^{31}}` deg = :math:`180 / {2147483648}` deg :math:`\approx 8.381903171539307e-08` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg



I011/042 - Calculated Position in Cartesian Co-ordinates
********************************************************

*Definition*: Calculated position of a target in Cartesian co-ordinates (two's complement form).

*Structure*:

    **I011/042/X** - *X-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m
    - value :math:`>= -32768` m
    - value :math:`<= 32768` m

    **I011/042/Y** - *Y-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m
    - value :math:`>= -32768` m
    - value :math:`<= 32768` m



I011/060 - Mode-3/A Code in Octal Representation
************************************************

*Definition*: Track Mode-3/A code converted into Octal Representation.

*Structure*:

    **I011/060/(spare)**

    - 4 bits [``....``]

    **I011/060/MOD3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)



I011/090 - Measured Flight Level
********************************

*Definition*: Last valid and credible flight level used to update the track, in two's complement representation.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 1
- fractional bits: 2
- unit: "FL"
- LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
- value :math:`>= -12` FL
- value :math:`<= 1500` FL


Note:
     The criteria to determine the credibility of the flight level are Tracker dependent.
     Credible means: within reasonable range of change with respect to the previous detection.

I011/092 - Calculated Track Geometric Altitude
**********************************************

*Definition*: Calculated geometric vertical distance above mean sea level, not related to barometric pressure.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft"
- LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
- value :math:`>= -1500` ft
- value :math:`<= 150000` ft


Note:
     The source of altitude is identified in bits (SRC) of item I011/170 Track Status.

I011/093 - Calculated Track Barometric Altitude
***********************************************

*Definition*: Calculated Barometric Altitude of the track.

*Structure*:

    **I011/093/QNH** - *QNH Correction Applied*

    - 1 bit [``.``]

    - values:

        | 0: No QNH Correction Applied
        | 1: QNH Correction Applied

    **I011/093/CTBA** - *Calculated Track Barometric Altitude*

    - 15 bits [``...............``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
    - value :math:`>= -15` FL
    - value :math:`<= 1500` FL



I011/140 - Time of Track Information
************************************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Note:
    The Time of Track Information value is reset to zero each day at midnight.

I011/161 - Track Number
***********************

*Definition*: Identification of a fusion track (single track number).

*Structure*:

    **I011/161/(spare)**

    - 1 bit [``.``]

    **I011/161/FTN** - *Fusion Track Number*

    - 15 bits [``...............``]

    - raw value



I011/170 - Track Status
***********************

*Definition*: Status of track.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I011/170/MON**

    - 1 bit [``.``]

    - values:

        | 0: Multisensor Track
        | 1: Monosensor Track

    **I011/170/GBS**

    - 1 bit [``.``]

    - values:

        | 0: Transponder Ground bit not set or unknown
        | 1: Transponder Ground bit set

    **I011/170/MRH**

    - 1 bit [``.``]

    - values:

        | 0: Barometric altitude (Mode C) more reliable
        | 1: Geometric altitude more reliable

    **I011/170/SRC**

    - 3 bits [``...``]

    - values:

        | 0: No source
        | 1: GPS
        | 2: 3d radar
        | 3: Triangulation
        | 4: Height from coverage
        | 5: Speed look-up table
        | 6: Default height
        | 7: Multilateration

    **I011/170/CNF**

    - 1 bit [``.``]

    - values:

        | 0: Confirmed track
        | 1: Tentative track

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I011/170/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual Track
        | 1: Simulated track

    **I011/170/TSE**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Track service end (i.e. last message transmitted to the user for the track)

    **I011/170/TSB**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Track service begin (i.e. first message transmitted to the user for the track)

    **I011/170/FRIFOE**

    - 2 bits [``..``]

    - values:

        | 0: No Mode 4 interrogationt
        | 1: Friendly target
        | 2: Unknown target
        | 3: No reply

    **I011/170/ME**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Military Emergency present in the last report received from a sensor capable of decoding this data

    **I011/170/MI**

    - 1 bit [``.``]

    - values:

        | 0: End of Data Item
        | 1: Military Identification present in the last report received from a sensor capable of decoding this data

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I011/170/AMA**

    - 1 bit [``.``]

    - values:

        | 0: Track not resulting from amalgamation process
        | 1: Track resulting from amalgamation process

    **I011/170/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: SPI present in the last report received from a sensor capable of decoding this data

    **I011/170/CST**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received track update is higher than system dependent threshold (coasting)

    **I011/170/FPC**

    - 1 bit [``.``]

    - values:

        | 0: Not flight-plan correlated
        | 1: Flight plan correlated

    **I011/170/AFF**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: ADS-B data inconsistent with other surveillance information

    **I011/170/(spare)**

    - 2 bits [``..``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I011/170/(spare)**

    - 1 bit [``.``]

    **I011/170/PSR**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received PSR track update is higher than system dependent threshold

    **I011/170/SSR**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received SSR track update is higher than system dependent threshold

    **I011/170/MDS**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received Mode S track update is higher than system dependent threshold

    **I011/170/ADS**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received ADS track update is higher than system dependent threshold

    **I011/170/SUC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Special Used Code (Mode A codes to be defined in the system to mark a track with special interest)

    **I011/170/AAC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Assigned Mode A Code Conflict (same individual Mode A Code assigned to another track)

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Track type and coasting can also be derived from Data Item I011/290 System Track Update Ages

I011/202 - Calculated Track Velocity in Cartesian Coordinates
*************************************************************

*Definition*: Calculated track velocity expressed in Cartesian co-ordinates.

*Structure*:

    **I011/202/VX** - *Vx*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s

    **I011/202/VY** - *Vy*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8192` m/s



I011/210 - Calculated Acceleration
**********************************

*Definition*: Calculated Acceleration of the target, in two's complement form.

*Structure*:

    **I011/210/AX** - *Ax*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2
    - value :math:`>= -31` m/s2
    - value :math:`<= 31` m/s2

    **I011/210/AY** - *Ay*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2
    - value :math:`>= -31` m/s2
    - value :math:`<= 31` m/s2



I011/215 - Calculated Rate Of Climb/Descent
*******************************************

*Definition*: Calculated rate of Climb/Descent of an aircraft, in two's complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft/min"
- LSB = :math:`25 / {2^{2}}` ft/min = :math:`25 / {4}` ft/min :math:`\approx 6.25` ft/min
- value :math:`>= -204800` ft/min
- value :math:`<= 204800` ft/min



I011/245 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters.

*Structure*:

    **I011/245/STI**

    - 2 bits [``..``]

    - values:

        | 0: Callsign or registration downlinked from transponder
        | 1: Callsign not downlinked from transponder
        | 2: Registration not downlinked from transponder

    **I011/245/(spare)**

    - 6 bits [``......``]

    **I011/245/TID** - *Target Identification*

    - 48 bits [``................................................``]

    - ICAO string (6-bits per character)


Note:
    Characters 1-8 (coded on 6 bits each) defining target identification

I011/270 - Target Size and Orientation
**************************************

*Definition*: Target size defined as length and with of the detected target, and orientation.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I011/270/LENGTH** - *Length*

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I011/270/ORIENTATION** - *Orientation*

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 7
    - unit: "deg"
    - LSB = :math:`360 / {2^{7}}` deg = :math:`360 / {128}` deg :math:`\approx 2.8125` deg

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I011/270/WIDTH** - *Width*

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:
    The orientation gives the direction to which the aircraft nose is pointing, relative to the Geographical North.

I011/290 - System Track Update Ages
***********************************

*Definition*: Ages of the last plot/local track, or the last valid mode-A/mode-C, used to update the system track.

*Structure*:

Compound item (FX)

    **I011/290/PSR** - *Age of the Last Primary Report Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/SSR** - *Age of the Last Secondary Report Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MDA** - *Age of the Last Valid Mode A Report Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MFL** - *Age of the Last Valid and Credible Mode C Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MDS** - *Age of the Last Mode S Report Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/ADS** - *Age of the Last ADS Report Used to Update the Track*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/ADB** - *Age of the Last ADS-B Report Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MD1** - *Age of the Last Valid Mode 1 Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MD2** - *Age of the Last Valid Mode 2 Used to Update the Track*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/LOP** - *Age of the Last Magentic Loop Detection*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/TRK** - *Actual Track Age Since First Occurrence*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s

    **I011/290/MUL** - *Age of the Last Multilateration Detection*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s


Note:
    The ages are counted from Data Item I011/140, Time Of Track
    Information, using the following formula:
    Age = Time of track information - Time of last (valid) update
    If the computed age is greater than the maximum value or if the
    data has never been received, then the corresponding subfield is not sent.

I011/300 - Vehicle Fleet Identification
***************************************

*Definition*: Vehicle fleet identification number.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Flyco (follow me)
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
    | 16: Unknown



I011/310 - Pre-programmed Message
*********************************

*Definition*: Number related to a pre-programmed message that can be transmitted by a vehicle.

*Structure*:

    **I011/310/TRB** - *In Trouble*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: In Trouble

    **I011/310/MSG** - *Message*

    - 7 bits [``.......``]

    - values:

        | 1: Towing aircraft
        | 2: FOLLOW-ME operation
        | 3: Runway check
        | 4: Emergency operation (fire, medical...)
        | 5: Work in progress (maintenance, birds scarer, sweepers...)



I011/380 - Mode-S / ADS-B Related Data
**************************************

*Definition*: Data specific to Mode-S ADS-B.

*Structure*:

Compound item (FX)

    **I011/380/MB** - *BDS*

    Repetitive item, repetition factor 8 bits.

        - 64 bits [``................................................................``]

        - BDS register with address

    **I011/380/ADR** - *24 Bits Aircraft Address*

    - 24 bits [``........................``]

    - raw value

    (empty subitem)

    **I011/380/COMACAS** - *Communications/ACAS Capability and Flight Status*

        **I011/380/COMACAS/COM** - *Communications Capability of the Transponder*

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

        **I011/380/COMACAS/STAT** - *Flight Status*

        - 4 bits [``....``]

        - values:

            | 0: No alert, no SPI, aircraft airborne
            | 1: No alert, no SPI, aircraft on ground
            | 2: Alert, no SPI, aircraft airborne
            | 3: Alert, no SPI, aircraft on ground
            | 4: Alert, SPI, aircraft airborne or on ground
            | 5: No alert, SPI, aircraft airborne or on ground
            | 6: General Emergency
            | 7: Lifeguard / medical
            | 8: Minimum fuel
            | 9: No communications
            | 10: Unlawful interference

        **I011/380/COMACAS/(spare)**

        - 1 bit [``.``]

        **I011/380/COMACAS/SSC** - *Specific Service Capability*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I011/380/COMACAS/ARC** - *Altitude Reporting Capability*

        - 1 bit [``.``]

        - values:

            | 0: 100 ft resolution
            | 1: 25 ft resolution

        **I011/380/COMACAS/AIC** - *Aircraft Identification Capability*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I011/380/COMACAS/B1A** - *BDS 1,0 Bit 16*

        - 1 bit [``.``]

        - raw value

        **I011/380/COMACAS/B1B** - *BDS 1,0 Bit 37/40*

        - 4 bits [``....``]

        - raw value

        **I011/380/COMACAS/AC** - *ACAS Operational*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I011/380/COMACAS/MN** - *Multiple Navigational Aids Operating*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I011/380/COMACAS/DC** - *Differential Correction*

        - 1 bit [``.``]

        - values:

            | 0: Yes
            | 1: No

        **I011/380/COMACAS/(spare)**

        - 5 bits [``.....``]

    (empty subitem)

    (empty subitem)

    (empty subitem)

    **I011/380/ACT** - *Aircraft Derived Aircraft Type*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I011/380/ECAT** - *Emitter Category*

    - 8 bits [``........``]

    - values:

        | 1: Light aircraft <= 7000 kg
        | 2: Reserved
        | 3: 7000 kg &lt; medium aircraft &lt; 136000 kg
        | 4: Reserved
        | 5: 136000 kg <= heavy aircraft
        | 6: Highly manoeuvrable (5g acceleration capability) and high speed (&gt;400 knots cruise)
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

    (empty subitem)

    **I011/380/AVTECH** - *Available Technologies*

        **I011/380/AVTECH/VDL** - *VDL Mode 4*

        - 1 bit [``.``]

        - values:

            | 0: VDL Mode 4 available
            | 1: VDL Mode 4 not available

        **I011/380/AVTECH/MDS** - *Mode S*

        - 1 bit [``.``]

        - values:

            | 0: Mode S available
            | 1: Mode S not available

        **I011/380/AVTECH/UAT** - *UAT*

        - 1 bit [``.``]

        - values:

            | 0: UAT available
            | 1: UAT not available

        **I011/380/AVTECH/(spare)**

        - 5 bits [``.....``]

    (empty subitem)



I011/390 - Flight Plan Related Data
***********************************

*Definition*: All flight plan related information.

*Structure*:

Compound item (FX)

    **I011/390/FPPSID** - *FPPS Identification Tag*

        **I011/390/FPPSID/SAC** - *System Area Code*

        - 8 bits [``........``]

        - raw value

        **I011/390/FPPSID/SIC** - *System Identity Code*

        - 8 bits [``........``]

        - raw value

    **I011/390/CSN** - *Callsign*

    - 56 bits [``........................................................``]

    - Ascii string (8-bits per character)

    **I011/390/IFPSFLIGHTID** - *IFPS_FLIGHT_ID*

        **I011/390/IFPSFLIGHTID/TYP** - *IFPS Flight ID Type*

        - 2 bits [``..``]

        - values:

            | 0: Plan number
            | 1: Unit 1 internal flight number
            | 2: Unit 2 internal flight number
            | 3: Unit 3 internal flight number

        **I011/390/IFPSFLIGHTID/(spare)**

        - 3 bits [``...``]

        **I011/390/IFPSFLIGHTID/NBR** - *IFPS Flight ID Number*

        - 27 bits [``...........................``]

        - raw value

    **I011/390/FLIGHTCAT** - *Flight Category*

        **I011/390/FLIGHTCAT/GATOAT** - *Flight Type*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: General Air Traffic
            | 2: Operational Air Traffic
            | 3: Not applicable

        **I011/390/FLIGHTCAT/FR1FR2** - *Flight Rules*

        - 2 bits [``..``]

        - values:

            | 0: Instrument Flight Rules
            | 1: Visual Flight Rules
            | 2: Not applicable
            | 3: Controlled Visual Flight Rules

        **I011/390/FLIGHTCAT/RVSM** - *RVSM*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Approved
            | 2: Exempt
            | 3: Not Approved

        **I011/390/FLIGHTCAT/HPR** - *Flight Priority*

        - 1 bit [``.``]

        - values:

            | 0: Normal Priority Flight
            | 1: High Priority Flight

        **I011/390/FLIGHTCAT/(spare)**

        - 1 bit [``.``]

    **I011/390/TOA** - *Type of Aircraft*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I011/390/WTC** - *Wake Turbulence Category*

    - 8 bits [``........``]

    - values:

        | 76: Light
        | 77: Medium
        | 72: Heavy
        | 74: Super

    **I011/390/ADEP** - *Departure Airport*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I011/390/ADES** - *Destination Airport*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I011/390/RWY** - *Runway Designation*

    - 24 bits [``........................``]

    - Ascii string (8-bits per character)

    **I011/390/CFL** - *Current Cleared Flight Level*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL

    **I011/390/CCP** - *Current Control Position*

        **I011/390/CCP/CENTRE** - *8-bit Group Identification Code*

        - 8 bits [``........``]

        - raw value

        **I011/390/CCP/POSITION** - *8-bit Control Position Identification Code*

        - 8 bits [``........``]

        - raw value

    **I011/390/TOD** - *Time of Departure*

    Repetitive item, repetition factor 8 bits.

            **I011/390/TOD/TYP** - *Time Type*

            - 5 bits [``.....``]

            - values:

                | 0: Scheduled off-block time
                | 1: Estimated off-block time
                | 2: Estimated take-off time
                | 3: Actual off-block time
                | 4: Predicted time at runway hold
                | 5: Actual time at runway hold
                | 6: Actual line-up time
                | 7: Actual take-off time
                | 8: Estimated time of arrival
                | 9: Predicted landing time
                | 10: Actual landing time
                | 11: Actual time off runway
                | 12: Predicted time to gate
                | 13: Actual on-block time

            **I011/390/TOD/DAY** - *Day*

            - 2 bits [``..``]

            - values:

                | 0: Today
                | 1: Yesterday
                | 2: Tomorrow

            **I011/390/TOD/(spare)**

            - 4 bits [``....``]

            **I011/390/TOD/HOR** - *Hours, from 0 to 23*

            - 5 bits [``.....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 23`

            **I011/390/TOD/(spare)**

            - 2 bits [``..``]

            **I011/390/TOD/MIN** - *Minutes, from 0 to 59*

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

            **I011/390/TOD/AVS** - *Seconds Available*

            - 1 bit [``.``]

            - values:

                | 0: Seconds available
                | 1: Seconds not available

            **I011/390/TOD/(spare)**

            - 1 bit [``.``]

            **I011/390/TOD/SEC** - *Seconds, from 0 to 59*

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

    **I011/390/AST** - *Aircraft Stand*

    - 48 bits [``................................................``]

    - Ascii string (8-bits per character)

    **I011/390/STS** - *Stand Status*

        **I011/390/STS/EMP** - *Stand Empty*

        - 2 bits [``..``]

        - values:

            | 0: Empty
            | 1: Occupied
            | 2: Unknown

        **I011/390/STS/AVL** - *Stand Available*

        - 2 bits [``..``]

        - values:

            | 0: Available
            | 1: Not available
            | 2: Unknown

        **I011/390/STS/(spare)**

        - 4 bits [``....``]



I011/430 - Phase of Flight
**************************

*Definition*: Current phase of the flight.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Unknown
    | 1: On stand
    | 2: Taxiing for departure
    | 3: Taxiing for arrival
    | 4: Runway for departure
    | 5: Runway for arrival
    | 6: Hold for departure
    | 7: Hold for arrival
    | 8: Push back
    | 9: On finals



I011/500 - Estimated Accuracies
*******************************

*Definition*: Overview of all important accuracies (standard deviations).

*Structure*:

Compound item (FX)

    **I011/500/APC** - *Estimated Accuracy Of Track Position (Cartesian)*

        **I011/500/APC/X** - *Estimated Accuracy of the Calculated Position of X Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m"
        - LSB = :math:`1 / {2^{2}}` m = :math:`1 / {4}` m :math:`\approx 0.25` m

        **I011/500/APC/Y** - *Estimated Accuracy of the Calculated Position of Y Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m"
        - LSB = :math:`1 / {2^{2}}` m = :math:`1 / {4}` m :math:`\approx 0.25` m

    **I011/500/APW** - *Estimated Accuracy Of Track Position (WGS84)*

        **I011/500/APW/LAT** - *APW Latitude Component Accuracy*

        - 16 bits [``................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 31
        - unit: "deg"
        - LSB = :math:`180 / {2^{31}}` deg = :math:`180 / {2147483648}` deg :math:`\approx 8.381903171539307e-08` deg

        **I011/500/APW/LON** - *APW Longitude Component Accuracy*

        - 16 bits [``................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 31
        - unit: "deg"
        - LSB = :math:`180 / {2^{31}}` deg = :math:`180 / {2147483648}` deg :math:`\approx 8.381903171539307e-08` deg

    **I011/500/ATH** - *Estimated Accuracy Of Track Height*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m

    **I011/500/AVC** - *Estimated Accuracy Of Track Velocity (Cartesian)*

        **I011/500/AVC/X** - *Estimated Accuracy of the Calculated Velocity of X Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 0.1
        - fractional bits: 0
        - unit: "m/s"
        - LSB = :math:`0.1` m/s

        **I011/500/AVC/Y** - *Estimated Accuracy of the Calculated Velocity of Y Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 0.1
        - fractional bits: 0
        - unit: "m/s"
        - LSB = :math:`0.1` m/s

    **I011/500/ARC** - *Estimated Accuracy Of Rate Of Climb / Descent*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "m/s"
    - LSB = :math:`0.1` m/s

    **I011/500/AAC** - *Estimated Accuracy Of Acceleration (Cartesian)*

        **I011/500/AAC/X** - *Estimated Accuracy Of Acceleration of X Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 0.01
        - fractional bits: 0
        - unit: "m/s2"
        - LSB = :math:`0.01` m/s2

        **I011/500/AAC/Y** - *Estimated Accuracy Of Acceleration of Y Component*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 0.01
        - fractional bits: 0
        - unit: "m/s2"
        - LSB = :math:`0.01` m/s2



I011/600 - Alert Messages
*************************

*Definition*: Alert involving the targets indicated in I011/605.

*Structure*:

    **I011/600/ACK** - *Alert Acknowleged*

    - 1 bit [``.``]

    - values:

        | 0: Alert acknowledged
        | 1: Alert not acknowledged

    **I011/600/SVR** - *Alert Severity*

    - 2 bits [``..``]

    - values:

        | 0: End fo alert
        | 1: Pre-alarm
        | 2: Severe alert

    **I011/600/(spare)**

    - 5 bits [``.....``]

    **I011/600/AT** - *Alert Type*

    - 8 bits [``........``]

    - raw value

    **I011/600/AN** - *Alert Number*

    - 8 bits [``........``]

    - raw value



I011/605 - Tracks in Alert
**************************

*Definition*: List of track numbers of the targets concerned by the alert described in I011/600.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I011/605/(spare)**

        - 4 bits [``....``]

        **I011/605/FTN** - *Fusion Track Number*

        - 12 bits [``............``]

        - raw value



I011/610 - Holdbar Status
*************************

*Definition*: Status of up to sixteen banks of twelve indicators.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I011/610/BKN** - *Bank Number*

        - 4 bits [``....``]

        - raw value

        **I011/610/I1** - *Indicator 1*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I2** - *Indicator 2*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I3** - *Indicator 3*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I4** - *Indicator 4*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I5** - *Indicator 5*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I6** - *Indicator 6*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I7** - *Indicator 7*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I8** - *Indicator 8*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I9** - *Indicator 9*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I10** - *Indicator 10*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I11** - *Indicator 11*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off

        **I011/610/I12** - *Indicator 12*

        - 1 bit [``.``]

        - values:

            | 0: Indicator on
            | 1: Indicator off



I011/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



I011/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



User Application Profile for Category 011
=========================================
- (1) ``I011/010`` - Data Source Identifier
- (2) ``I011/000`` - Message Type
- (3) ``I011/015`` - Service Identification
- (4) ``I011/140`` - Time of Track Information
- (5) ``I011/041`` - Position in WGS-84 Coordinates
- (6) ``I011/042`` - Calculated Position in Cartesian Co-ordinates
- (7) ``I011/202`` - Calculated Track Velocity in Cartesian Coordinates
- ``(FX)`` - Field extension indicator
- (8) ``I011/210`` - Calculated Acceleration
- (9) ``I011/060`` - Mode-3/A Code in Octal Representation
- (10) ``I011/245`` - Target Identification
- (11) ``I011/380`` - Mode-S / ADS-B Related Data
- (12) ``I011/161`` - Track Number
- (13) ``I011/170`` - Track Status
- (14) ``I011/290`` - System Track Update Ages
- ``(FX)`` - Field extension indicator
- (15) ``I011/430`` - Phase of Flight
- (16) ``I011/090`` - Measured Flight Level
- (17) ``I011/093`` - Calculated Track Barometric Altitude
- (18) ``I011/092`` - Calculated Track Geometric Altitude
- (19) ``I011/215`` - Calculated Rate Of Climb/Descent
- (20) ``I011/270`` - Target Size and Orientation
- (21) ``I011/390`` - Flight Plan Related Data
- ``(FX)`` - Field extension indicator
- (22) ``I011/300`` - Vehicle Fleet Identification
- (23) ``I011/310`` - Pre-programmed Message
- (24) ``I011/500`` - Estimated Accuracies
- (25) ``I011/600`` - Alert Messages
- (26) ``I011/605`` - Tracks in Alert
- (27) ``I011/610`` - Holdbar Status
- (28) ``I011/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator
- (29) ``I011/RE`` - Reserved Expansion Field

