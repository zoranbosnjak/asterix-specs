Asterix category 016 - Independent Non-Cooperative Surveillance System Configuration Reports
============================================================================================
**category**: 016

**edition**: 1.0

**date**: 2019-07-15

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I016/000 - Message Type
***********************

*Definition*: This data item conveys the message type.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: System Configuration
    | 2: Transmitter / Receiver Configuration

I016/010 - Data Source Identifier
*********************************

*Definition*: Identification of the Ground System from which the data is received.

*Structure*:

    **I016/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I016/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

NOTE - The up-to-date list of SACs is published on the EUROCONTROL
Web Site (http://www.eurocontrol.int/asterix).

NOTE - The SICs are allocated by the national authority responsible
for the surveillance infrastructure.

I016/015 - Service Identification
*********************************

*Definition*: Identifies the service being reported.

*Structure*:

- 8 bits [``........``]

- raw value

NOTE - The service identification is allocated by the system.

I016/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

NOTE - The time of day value is reset to zero each day at midnight.

I016/200 - System Configuration Reporting Period
************************************************

*Definition*: Data item to indicate the reporting period of the system
configuration messages.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1` s
- value :math:`> 1` s

NOTE - The item will be sent periodically (every SCRP) and each
time a value change occurs.

I016/300 - Pair Identification
******************************

*Definition*: The use of a pair identifier in this data item, that is common
with its counterpart in ASTERIX Category I015/400, enables direct
mapping from the INCS Target Report to the Transmitter/Receiver
Pair that contributed to the target report.
This is an identifier pointing to a measurement that was created
from a specific contributing pair where the Pair Identifier refers
to the index that details both the transmitter characteristics
(DVB-T, DAB, FM, dedicated etc) and the receiver characteristics.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I016/300/PID** - *Pair Identification*

        - 16 bits [``................``]

        - raw value

        **I016/300/TID** - *Transmitter Identification*

        - 16 bits [``................``]

        - raw value

        **I016/300/RID** - *Receiver Identification*

        - 16 bits [``................``]

        - raw value

I016/400 - Position of the System Reference Point
*************************************************

*Definition*: Position of the system reference point in WGS-84 coordinates.

*Structure*:

    **I016/400/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I016/400/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

NOTE - Positive longitude indicates East. Positive latitude
indicates North.

NOTE - I016/400 shall only be sent together with item I016/405
“Height of the System Reference Point”.

I016/405 - Height of System Reference Point
*******************************************

*Definition*: Height of the system reference point in Two’s Complement form.
The height shall use mean sea level as the zero reference level.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "m"
- LSB = :math:`1/2^2` m :math:`\approx 0.25` m
- value :math:`> -8192` m
- value :math:`< 8192` m

NOTE - I016/405 shall only be sent together with item I016/400
“Position of the System Reference Point”.

I016/410 - Transmitter Properties
*********************************

*Definition*: This item provides properties of a transmitter component.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I016/410/TID** - *Transmitter ID*

        - 16 bits [``................``]

        - raw value

        **I016/410/LAT** - *Latitude*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I016/410/LON** - *Longitude*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

        **I016/410/ALT** - *Altitude*

        - 16 bits [``................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2^2` m :math:`\approx 0.25` m
        - value :math:`> -8192` m
        - value :math:`< 8192` m

        **I016/410/TTO** - *Transmission Time Offset*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "ns"
        - LSB = :math:`2` ns

        **I016/410/(spare)**

        - 4 bits [``....``]

        **I016/410/ATO** - *Accuracy of Transmission Time Offset*

        - 20 bits [``....................``]

        - unsigned quantity
        - unit: "ns"
        - LSB = :math:`1` ns

        **I016/410/PCI** - *Parallel Transmitter Index*

        - 16 bits [``................``]

        - unsigned integer

NOTE - Regarding Transmitter Identification:
    a. Individual channels of a transmitter are considered as
    separate collocated transmitters.

    b. A Transmitter ID may be assigned to individual channels
    of a compound transmitter. i.e. a single multi-channel
    transmitter may be assigned several Tx ID.

    c. The Tx ID shall be used in a unique way for a specific
    SAC/SIC.

NOTE - Regarding Transmitter Latitude and Longitude and Altitude:
    a. The Tx Location and Altitude is the position of the
    component in WGS-84 coordinates. The vertical distance between
    the component and the projection of its position on the earth’s
    ellipsoid, as defined by WGS-84, in two’s complement form.

NOTE - Regarding Transmission Time Offset
    a. Time offset of transmitter compared to the reference
    transmitter within the single frequency network (SFN).

NOTE - Regarding Accuracy of Transmission Time Offset
    a. The Accuracy of the Transmission Time Offset is the Standard
    Deviation of the measurement of the transmission time offset
    between the transmitter channel compared to the reference
    transmitter within the single frequency network (SFN).

NOTE - Regarding Parallel Transmitter Index
    a. The Parallel Transmitter Index is the identification of the
    transmitter via index, which is sent in parallel.

    b. For referring to a one-octet index bits-16/9 shall be set
    to zero.

    c. This index shall be used in a unique way for a specific
    SAC/SIC.

    d. In a Single Frequency Network the parallel transmitter
    index is the index of the reference transmitter.

I016/420 - Receiver Properties
******************************

*Definition*: This item provides properties of the receiver component.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I016/420/RID** - *Receiver Component ID*

        - 16 bits [``................``]

        - raw value

        **I016/420/LAT** - *Latitude*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I016/420/LON** - *Longitude*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

        **I016/420/ALT** - *Altitude*

        - 16 bits [``................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2^2` m :math:`\approx 0.25` m
        - value :math:`> -8192` m
        - value :math:`< 8192` m

NOTE - Regarding the Receiver Identification
    a. The Rx ID shall be used in a unique way for a specific
    SAC/SIC.

NOTE - Regarding the Receiver Location and Altitude (WGS-84)
    a. The Rx Location and Altitude is the of the component
    in WGS-84 coordinates. The vertical distance between the
    component and the projection of its position on the earth’s
    ellipsoid, as defined by WGS-84, in two’s complement form.

I016/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 016
=========================================
- (1) ``I016/010`` - Data Source Identifier
- (2) ``I016/015`` - Service Identification
- (3) ``I016/000`` - Message Type
- (4) ``I016/140`` - Time of Day
- (5) ``I016/200`` - System Configuration Reporting Period
- (6) ``I016/300`` - Pair Identification
- (7) ``I016/400`` - Position of the System Reference Point
- ``(FX)`` - Field extension indicator
- (8) ``I016/405`` - Height of System Reference Point
- (9) ``I016/410`` - Transmitter Properties
- (10) ``I016/420`` - Receiver Properties
- (11) ``I016/SP`` - Special Purpose Field
- (12) ``(spare)``
- (13) ``(spare)``
- (14) ``(spare)``
- ``(FX)`` - Field extension indicator
