Asterix category 205 - Radio Direction Finder Reports
=====================================================
**category**: 205

**edition**: 1.0

**date**: 2020-03-17

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I205/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the
messages at the receiver side by further defining the type of
transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: System Position Report
    | 2: System Bearing Report
    | 3: System Position Report of conflicting transmission
    | 4: System Detection End Report
    | 5: Sensor Data Report

I205/010 - Data Source Identifier
*********************************

*Definition*: Identification of the Radio Direction Finder System or Sensor from which the 
report is received.

*Structure*:

    **I205/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I205/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note:
    The up-to-date list of SACs is published on the
    EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I205/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    The service identification is allocated by the system.

I205/030 - Time of Day
**********************

*Definition*: UTC time of transmission of this ASTERIX message.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

Note:
    The TOD value is reset to zero at every midnight UTC. 

I205/040 - Report Number
************************

*Definition*: Sequential and cyclic number of position detection reports.

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    The number is incremented if there is a new position or bearing detected, independent 
    from the respective channel. The report number shall also be incremented if, during 
    the ongoing position or bearing detection, the position or bearing value is deviating by 
    a system-defined threshold from the previous one. If the message type (I205/000) 
    changes to another Detection Report, the report number is incremented as well.
    to detect lost messages.
    

I205/050 - Position in WGS-84 Coordinates
*****************************************

*Definition*: Calculated position in WGS-84 Coordinates.

*Structure*:

    **I205/050/LAT** - *Latitude in WGS-84*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I205/050/LON** - *Longitude in WGS-84*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

Note:
    The LSB provides a resolution at least better than 0.6 meters.
    For reports of Message Type 001 and 003, the item shall contain the estimated 
    position of the transmitting aircraft.
    For reports of Message Type 002, the item shall contain the position of the bearing 
    starting point, i.e. the position of the respective RDF sensor.
    

I205/060 - Position in Cartesian Coordinates
********************************************

*Definition*: Calculated position in Cartesian Coordinates.

*Structure*:

    **I205/060/X** - *X-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m

    **I205/060/Y** - *Y-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m

Note:
    The Cartesian coordinates are relative to an agreed System Reference Point. The 
    System Reference Point may be communicated in ASTERIX category 025, item 600.
    For reports of Message Type 001 and 003, the item shall contain the estimated 
    position of the transmitting aircraft.
    For reports of Message Type 002, the item shall contain the position of the bearing 
    starting point, i.e. the position of the respective RDF sensor.
    

I205/070 - Local Bearing
************************

*Definition*: Bearing of the detected radio transmission, starting at the position contained in 
I205/050 and given relative to this position.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- unit: "°"
- LSB = :math:`1/100` ° :math:`\approx 1.00e-2` °
- value :math:`>= 0` °
- value :math:`<= 360` °

Note:
    The angle is given in degrees, in clock-wise notation, 
    starting with 0 degrees for the geographical North. 

I205/080 - System Bearing
*************************

*Definition*: Bearing of the detected radio transmission, starting at the position contained in 
I205/060, projected to the Cartesian Coordinate System relative to the System 
Reference Point (as used for I205/ 060).

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- unit: "°"
- LSB = :math:`1/100` ° :math:`\approx 1.00e-2` °
- value :math:`>= 0` °
- value :math:`<= 360` °

Note:
    The angle is given in degrees, in clock-wise notation, 
    starting with 0 degrees for the geographical North. 

I205/090 - Radio Channel Name
*****************************

*Definition*: Name of the channel the radio transmission is detected on.

*Structure*:

- 56 bits [``... 56 bits ...``]

- Ascii string (8-bits per character)

Note:
    NU1 till NU7 contain digits or a decimal point in ASCII representation, specifying the 
    name of the radio channel. Channel names that could be provided with less than 6 
    digits shall be filled with trailing zeroes (e.g. 121.100).
    This channel name is not identical with the actual physical frequency.
    

I205/100 - Quality of Measurement
*********************************

*Definition*: Quality of the measurement provided by the Radio Direction Finder system.

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    The actual meanings of the bits are application dependent.
    

I205/110 - Estimated Uncertainty
********************************

*Definition*: Uncertainty estimation of the RDF System. The transmitter is expected to be 
within the provided radius around the calculated position.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- unit: "m"
- LSB = :math:`100` m
- value :math:`>= 0` m
- value :math:`<= 25500` m

I205/120 - Contributing Sensors
*******************************

*Definition*: The identification of the RDF sensors that contributed to the detection of the radio 
transmitter.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 8 bits [``........``]

    - raw value

Note:
    The actual identification of the receivers is application dependent.
    

I205/130 - Conflicting Transmitter Position in WGS-84 Coordinates
*****************************************************************

*Definition*: Calculated position in WGS-84 Coordinates. This is the position of a second 
transmitter on the same frequency and overlapping in time with the transmitter 
position communicated in data item I205/050.

*Structure*:

    **I205/130/LAT** - *Latitude in WGS-84*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I205/130/LON** - *Longitude in WGS-84*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

Note:
    The LSB provides a resolution at least better than 0.6 meters.

I205/140 - Conflicting Transmitter Position in Cartesian Coordinates
********************************************************************

*Definition*: Calculated position in Cartesian Coordinates. This is the position of a second 
transmitter on the same frequency and overlapping in time with the transmitter 
position communicated in data item I205/060.

*Structure*:

    **I205/140/X** - *X-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m

    **I205/140/Y** - *Y-coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m
    - value :math:`>= -4194300` m
    - value :math:`<= 4194300` m

Note:
    The Cartesian coordinates are relative to an agreed System Reference Point. The 
    System Reference Point may be communicated in ASTERIX category 025, item 600.
    

I205/150 - Conflicting Transmitter Estimated Uncertainty
********************************************************

*Definition*: Range uncertainty estimation of the RDF System for the Conflicting Transmitter, 
i.e. a transmitter on the same frequency and with a timely overlapping 
transmission. The transmitter is estimated to be within the provided radius around 
the detected position.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- unit: "m"
- LSB = :math:`100` m
- value :math:`>= 0` m
- value :math:`<= 25500` m

I205/160 - Track Number
***********************

*Definition*: Unique identification of a track at the calculated RDF position.

*Structure*:

- 16 bits [``................``]

- raw value

I205/170 - Sensor Identification
********************************

*Definition*: Unique identification of an RDF sensor.

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    For Message Type 5 (Sensor Data Report) in I205/000, the item has to be used if 
    there is no unique SAC/SIC defined for each RDF Sensor.
    The actual identification number is application dependent
    

I205/180 - Signal Level
***********************

*Definition*: The level of the signal received by an RDF sensor.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "dBµV"
- LSB = :math:`1/100` dBµV :math:`\approx 1.00e-2` dBµV

I205/190 - Signal Quality
*************************

*Definition*: Relative quality of the received signal as estimated by the RDF sensor.

*Structure*:

- 8 bits [``........``]

- raw value

Note:
    255 corresponds to the best quality,
    0 to the worst quality

I205/200 - Signal Elevation
***************************

*Definition*: The elevation of the signal received by an RDF sensor.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "°"
- LSB = :math:`1/100` ° :math:`\approx 1.00e-2` °
- value :math:`>= -90` °
- value :math:`<= 90` °

I205/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 205
=========================================
- (1) ``I205/010`` - Data Source Identifier
- (2) ``I205/015`` - Service Identification
- (3) ``I205/000`` - Message Type
- (4) ``I205/030`` - Time of Day
- (5) ``I205/040`` - Report Number
- (6) ``I205/090`` - Radio Channel Name
- (7) ``I205/050`` - Position in WGS-84 Coordinates
- ``(FX)`` - Field extension indicator
- (8) ``I205/060`` - Position in Cartesian Coordinates
- (9) ``I205/070`` - Local Bearing
- (10) ``I205/080`` - System Bearing
- (11) ``I205/100`` - Quality of Measurement
- (12) ``I205/110`` - Estimated Uncertainty
- (13) ``I205/120`` - Contributing Sensors
- (14) ``I205/130`` - Conflicting Transmitter Position in WGS-84 Coordinates
- ``(FX)`` - Field extension indicator
- (15) ``I205/140`` - Conflicting Transmitter Position in Cartesian Coordinates
- (16) ``I205/150`` - Conflicting Transmitter Estimated Uncertainty
- (17) ``I205/160`` - Track Number
- (18) ``I205/170`` - Sensor Identification
- (19) ``I205/180`` - Signal Level
- (20) ``I205/190`` - Signal Quality
- (21) ``I205/200`` - Signal Elevation
- ``(FX)`` - Field extension indicator
- (22) ``I205/SP`` - Special Purpose Field
- (23) ``(spare)``
- (24) ``(spare)``
- (25) ``(spare)``
- (26) ``(spare)``
- (27) ``(spare)``
- (28) ``(spare)``
- ``(FX)`` - Field extension indicator
