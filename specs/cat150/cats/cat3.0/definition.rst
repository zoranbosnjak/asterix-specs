Asterix category 150 - MADAP Plan Server - Flight Data Message
==============================================================
**category**: 150

**edition**: 3.0

**date**: 2004-08-20

Preamble
--------
The main purpose of I150 messages is to distribute Flight Plan data to clients. However, other data
items can be sent using I150 as well. I150 messages can be divided into the following sub-categories:

- Flight Plan (Short & Complete)
- Start/End of cycle
- Correlation/De-correlation
- Conflict Alert

Description of standard data items
----------------------------------

I150/010 - Destination ID
*************************

*Definition*: Identification of the receiving centre.

*Structure*:

    **I150/010/CEN** - *Centre Identifier*

    - 8 bits [``........``]

    - raw value

    **I150/010/POS** - *Workstation Identifier*

    - 8 bits [``........``]

    - raw value

Translation:
    See Annex 1 Centre ID definition
Note:
    The Destination ID is irrelevant in CAT150 messages since the flight plan messages are sent to all 10 centres.
    Hence, the centre identifier is set to broadcast.

    The workstation identifier can be ignored.

I150/020 - Source ID
********************

*Definition*: Identification of the sending centre.

*Structure*:

    **I150/020/CEN** - *Centre Identifier*

    - 8 bits [``........``]

    - raw value

    **I150/020/POS** - *Workstation Identifier*

    - 8 bits [``........``]

    - raw value

Translation:
    See Destination ID.
Note:
    The Source ID centre identifier will define the flight plan source centre.

    The workstation identifier can be ignored.

I150/030 - Message Type
***********************

*Definition*: The event that triggered the message transmission.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Flight plan creation
    | 2: Flight plan modification
    | 3: Flight plan repetition
    | 4: Manual flight plan deletion
    | 5: Automatic flight plan deletion
    | 6: Flight is beyond extraction area boundary
    | 251: Short term conflict alert
    | 252: Correlations
    | 253: Decorrelations
    | 254: Start of background loop
    | 255: End of background loop

I150/040 - Plan Reference Number
********************************

*Definition*: Identification of the flight plan.

*Structure*:

- 16 bits [``................``]

- raw value

Note:
    See Plan and Track Numbers.

    The currently defined range for plan reference numbers used in MADAP is 0 .. 1999. Client systems
    should allow for a range of 0 .. 2047.

I150/050 - Callsign
*******************

*Definition*: Flight identity.

*Structure*:

- 56 bits [``... 56 bits ...``]

- Ascii string (8-bits per character)

I150/060 - Present Mode 3A
**************************

*Definition*: Actual transponder code mode 3A of the flight.

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Translation:
    Octal representation.

    - zzzz: no code availlable/assigned
    - dd00: code family
    - other: discrete code

    where:

    - z :== 'z'
    - 0 :== '0'
    - d :== '0' .. '7'

I150/070 - Next Mode 3A
***********************

*Definition*: Next transponder code mode 3A of the flight.

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Translation:
    See Present Mode 3A.

I150/080 - Departure Aerodrome
******************************

*Definition*: Identification of the flight’s departure aerodrome.

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Translation:
    - zzzz: no standard ICAO location identifier
    - other: unique ICAO location identifier

    where:

    - z :== 'z'

I150/090 - Destination Aerodrome
********************************

*Definition*: Identification of the flight’s destination aerodrome. 

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Translation:
    See Departure Aerodrome.

I150/100 - Type Flags
*********************

*Definition*: Type of flight and type of flight plan. 

*Structure*:

    **I150/100/GAT** - *General Air Traffic*

    - 1 bit [``.``]

    - raw value

    **I150/100/OAT** - *Operational Air Traffic*

    - 1 bit [``.``]

    - raw value

    **I150/100/(spare)**

    - 3 bits [``...``]

    **I150/100/CPL** - *Complete Flight Plan*

    - 1 bit [``.``]

    - raw value

    **I150/100/SPN** - *Short Flight Plan*

    - 1 bit [``.``]

    - raw value

    **I150/100/(spare)**

    - 1 bit [``.``]

I150/110 - Status Flags
***********************

*Definition*: Status of the flight. 

*Structure*:

    **I150/110/(spare)**

    - 1 bit [``.``]

    **I150/110/HLD** - *Aircraft is in Hold State*

    - 1 bit [``.``]

    - raw value

    **I150/110/RVQ** - *Aircraft is RVSM Equipped*

    - 1 bit [``.``]

    - raw value

    **I150/110/RVC** - *Aircraft is RVSM Capable*

    - 1 bit [``.``]

    - raw value

    **I150/110/RVX** - *Aircraft is RVSM Exempted*

    - 1 bit [``.``]

    - raw value

    **I150/110/(spare)**

    - 3 bits [``...``]

Note:
    If an aircraft is set in hold status then the ETO values are increased with 3 hours. 

    RVQ is set for:

    - GAT or GAT SPN: never
    - GAT or OAT CPL: if “W” filed in field 10a [Radio Communication, Navigation and Approach Aid Equipment] of the flightplan

    RVC is set for:

    - GAT or OAT SPN: on controller input or reception of ACT with “RVSM Capable”
    - GAT or OAT CPL (in decreasing priority):
        - Controller input or reception of ABI or ACT with “RVSM Capable”
        - “W” filed in field 10a [Radio Communication, Navigation and Approach Aid Equipment] and “1” filed in field 9a [number of aircraft] in the flightplan.

    RVX is set for:

    - GAT SPN: on controller input “RVSM Exempted”
    - OAT SPN: by default or on controller input “RVSM Exempted”
    - GAT or OAT CPL: if “O”, “M” or “A” filed in field 8b [type of flight] of the flightplan 

I150/120 - Aircraft Type
************************

*Definition*: Flight formation details. 

*Structure*:

    **I150/120/NOA** - *Number of Aircraft*

    - 16 bits [``................``]

    - Ascii string (8-bits per character)

    **I150/120/TOA** - *Type of Aircraft*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I150/120/WT** - *Wake Turbulence*

    - 8 bits [``........``]

    - Ascii string (8-bits per character)

I150/130 - Cleared Flight Level
*******************************

*Definition*: Cleared flight level from the sector that has the aircraft under control. 

*Structure*:

- 24 bits [``........................``]

- Ascii string (8-bits per character)

Translation:
    Cleared Flight Level is listed in FLs (100ft).
Note:
    The Cleared Flight Level corresponds to the “current Planned Flight Level”, valid for the sector which
    is currently in communications with the aircraft.
    
    Intermediate flight levels, temporarily assigned by controllers, are not distributed by MADAP. 

I150/140 - Route Points, Description
************************************

*Definition*: Route point descriptions. 

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/140/T** - *Route Point Type*

        - 8 bits [``........``]

        - values:

            | 1: P, point
            | 2: B, point with bearing and distance
            | 3: LS, latitude/longitude position short format
            | 4: LL, latitude/longitude position long format
            | 5: X, x/y co-ordinate position
            | 6: G, georeference position
            | 14: E, airport

        **I150/140/E** - *Route Point Description Element*

        - 88 bits [``... 88 bits ...``]

        - Ascii string (8-bits per character)

Translation:
    See layout table in specification document.
Note:
    For all route point items (140..180), present in a single message the count values are equal. Co4 ordinate (1), description (1), etc. form a single route point.
    
    The maximum number of route points is currently set to 28. 

I150/150 - Route Points, Coordinates
************************************

*Definition*: Route point co-ordinates.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/150/X** - *X Co-ordinate*

        - 16 bits [``................``]

        - signed quantity
        - unit: "NM"
        - LSB = :math:`1/2^6` NM :math:`\approx 1.56e-2` NM

        **I150/150/Y** - *Y Co-ordinate*

        - 16 bits [``................``]

        - signed quantity
        - unit: "NM"
        - LSB = :math:`1/2^6` NM :math:`\approx 1.56e-2` NM

Translation:
    Co-ordinate values are in [NM] as Cartesian offsets from 51°00’00”N, 008°00’00”E.

I150/151 - Route Points, Geographic Position
********************************************

*Definition*: Route point position in Lat. / Long. (WSG84).

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/151/LAT** - *Latitude in WGS.84 in Two's Complement Form*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I150/151/LON** - *Longitude in WGS.84 in Two's Complement Form*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

Note:
    This corresponds to an accuracy of at least 2.4 meters.

I150/160 - Route Points, Time
*****************************

*Definition*: Estimated times over route points.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/160/HH** - *Hours*

        - 16 bits [``................``]

        - Ascii string (8-bits per character)

        **I150/160/MM** - *Minutes*

        - 16 bits [``................``]

        - Ascii string (8-bits per character)

Translation:
    Times are specified in 24-hour format. I.e. ranging from 00:00 to 23:59.

I150/170 - Route Points, Flight Level
*************************************

*Definition*: Planned flight level over route point.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 24 bits [``........................``]

    - Ascii string (8-bits per character)

Translation:
    The planned flight levels are given in FLs (100 ft).
Note:
    All flight levels have the same value, equal to the “Current Planned Flight Level”. This is the Planned
    Flight Level valid for the sector which is in communication with the aircraft.

I150/171 - Route Points, Requested Flight Level
***********************************************

*Definition*: Requested flight level over route point.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 24 bits [``........................``]

    - Ascii string (8-bits per character)

Translation:
    The planned flight levels are given in FLs (100 ft).

I150/180 - Route Points, Speed
******************************

*Definition*: Filed true air speed over route point.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

Translation:
    The true airspeed is indicated in Knots [NM/h].

I150/190 - Controller ID
************************

*Definition*: Current control position in charge of the aircraft.

*Structure*:

- 16 bits [``................``]

- Ascii string (8-bits per character)

Translation:
    Single character controller ids are sent with a leading space character. This can be interpreted as a
    right aligned value.

I150/200 - Field 18
*******************

*Definition*: Field 18 free text information. Contains subfields, each starting with a 3 or 4 letter keyword followed by
forward slash; e.g. RMK/free text.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 8 bits [``........``]

    - Ascii string (8-bits per character)

I150/210 - Correlated Track Number
**********************************

*Definition*: The track number of the track that has been correlated to the flight plan.

*Structure*:

- 16 bits [``................``]

- raw value

Note:
    See Plan and Track Numbers.

I150/220 - Maximum Plan Count
*****************************

*Definition*: Maximum plan count is the maximum number of possible active plans.

*Structure*:

- 16 bits [``................``]

- unsigned integer

Note:
    The maximum number of active plans is fixed and, at present, set to 302.

I150/230 - Number of Plans
**************************

*Definition*: Number of plans that were sent during the last update cycle.

*Structure*:

- 16 bits [``................``]

- unsigned integer

Note:
    The number of extracted plans should be equal to the number of plans received between the start of
    cycle and end of cycle messages.

I150/240 - Newly Correlated Plans
*********************************

*Definition*: Array of correlated plan/track combinations valid in the Maastricht UAC Area of Interest.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/240/PLAN** - *Plan Number*

        - 16 bits [``................``]

        - raw value

        **I150/240/TRACK** - *Track Number*

        - 16 bits [``................``]

        - raw value

Note:
    See Plan and Track Numbers.

I150/250 - Newly De-correlated Plans
************************************

*Definition*: Array of de-correlated plans.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 16 bits [``................``]

    - raw value

Note:
    Contains an array of Plan Numbers.
    See Plan and Track Numbers.

I150/251 - Tracks in Conflict
*****************************

*Definition*: Array of conflicting track/track combinations.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I150/251/TRACK1** - *Track Number 1*

        - 16 bits [``................``]

        - raw value

        **I150/251/TRACK2** - *Track Number 2*

        - 16 bits [``................``]

        - raw value

Note:
    See Plan and Track Numbers.

User Application Profile for Category 150
=========================================
- (1) ``I150/010`` - Destination ID
- (2) ``I150/020`` - Source ID
- (3) ``I150/030`` - Message Type
- (4) ``I150/040`` - Plan Reference Number
- (5) ``I150/050`` - Callsign
- (6) ``I150/060`` - Present Mode 3A
- (7) ``I150/070`` - Next Mode 3A
- ``(FX)`` - Field extension indicator
- (8) ``I150/080`` - Departure Aerodrome
- (9) ``I150/090`` - Destination Aerodrome
- (10) ``I150/100`` - Type Flags
- (11) ``I150/110`` - Status Flags
- (12) ``I150/120`` - Aircraft Type
- (13) ``I150/130`` - Cleared Flight Level
- (14) ``I150/140`` - Route Points, Description
- ``(FX)`` - Field extension indicator
- (15) ``I150/150`` - Route Points, Coordinates
- (16) ``I150/160`` - Route Points, Time
- (17) ``I150/170`` - Route Points, Flight Level
- (18) ``I150/180`` - Route Points, Speed
- (19) ``I150/190`` - Controller ID
- (20) ``I150/200`` - Field 18
- (21) ``I150/210`` - Correlated Track Number
- ``(FX)`` - Field extension indicator
- (22) ``I150/220`` - Maximum Plan Count
- (23) ``I150/230`` - Number of Plans
- (24) ``I150/240`` - Newly Correlated Plans
- (25) ``I150/250`` - Newly De-correlated Plans
- (26) ``I150/251`` - Tracks in Conflict
- (27) ``I150/171`` - Route Points, Requested Flight Level
- (28) ``I150/151`` - Route Points, Geographic Position
- ``(FX)`` - Field extension indicator
