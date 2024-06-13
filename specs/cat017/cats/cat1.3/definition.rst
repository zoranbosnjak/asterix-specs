Asterix category 017 - Mode S Surveillance Coordination Function Messages
=========================================================================
**category**: 017

**edition**: 1.3

**date**: 2009-01-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I017/000 - Message Type
***********************

*Definition*: Definition of the type of message in the Surveillance
Coordination Network (SCN) environment

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Network information
    | 10: Track data
    | 20: Track data request
    | 21: Track data stop
    | 22: Cancel track data request
    | 23: Track data stop acknowledgement
    | 30: New Node / Change-over Initial or intermediate message segment
    | 31: New Node / Change-over Final or only message segment
    | 32: New Node / Change-over Initial or intermediate message segment reply
    | 33: New Node / Change-over Final or only message segment reply
    | 110: Move node to new cluster state;
    | 111: Move node to new cluster state acknowledgement

NOTE:
    - Message types 30 to 33 are specific to POEMS stations.

I017/010 - Data Source Identifier
*********************************

*Definition*: Identification of the source node for the SCN data

*Structure*:

    **I017/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I017/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

NOTE:
    - The up-to-date list of SACs is published on the Eurocontrol
      Web Site (http://www.eurocontrol.int/asterix).

I017/012 - Data Destination Identifier
**************************************

*Definition*: Identification of the destination node for the SCN data.

*Structure*:

    **I017/012/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I017/012/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

NOTE:
    - The up-to-date list of SACs is published on the Eurocontrol
      Web Site (http://www.eurocontrol.int).

I017/045 - Calculated Position in WGS-84 Coordinates
****************************************************

*Definition*: Calculated Position in WGS-84 Coordinates.

*Structure*:

    **I017/045/LAT** - *Latitude*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I017/045/LON** - *Longitude*

    - 24 bits [``........................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
    - value :math:`>= -180` °
    - value :math:`< 180` °

NOTE:
    - See Annex A for calculation details

I017/050 - Flight Level in Binary Representation
************************************************

*Definition*: Flight Level of the Aircraft

*Structure*:

    **I017/050/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I017/050/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code / Error correction applied

    **I017/050/ALT** - *Altitude*

    - 14 bits [``..............``]

    - unsigned quantity
    - unit: "FL"
    - LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL

NOTES:
    1. The value shall be within the range described by ICAO Annex
       10
    2. Bit-15 (G) is set to one when an error correction has been
       attempted
    3. In case of a track miss (coasted position) the flight level
       sent will be either he predicted flight level from the
       vertical tracking or the last measured flight level, if no
       vertical tracking is performed. Bit 7 (FLT) of I017/240
       (Track Status) indicates whether vertical tracking was
       performed or not.

I017/070 - Mode 3/A Code in Octal Representation
************************************************

*Definition*: Mode 3/A code converted into octal representation.

*Structure*:

    **I017/070/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I017/070/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I017/070/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Smoothed Mode-3/A code not extracted during the last scan

    **I017/070/(spare)**

    - 1 bit [``.``]

    **I017/070/MODE3A** - *Mode 3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)

NOTES:
    1. Bit 15 is set to one when an error correction has been
       attempted
    2. The data could be used to correlate tracks with non unique
       Mode S addresses

I017/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as Coordinated Universal Time
(UTC) time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s
- value :math:`< 86400` s

NOTE:
    - The time of day is reset to zero each day at midnight

I017/200 - Track Velocity in Polar Co-ordinates
***********************************************

*Definition*: Calculated track velocity expressed in polar co-ordinates. The
heading is the heading with respect to the geographical north at
the aircraft position. For clarification see annex A, paragraph5.

*Structure*:

    **I017/200/GSP** - *Calculated Groundspeed*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "NM/s"
    - LSB = :math:`1/2^14` NM/s :math:`\approx 6.10e-5` NM/s

    **I017/200/HDG** - *Calculated Heading*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

I017/210 - Mode S Address List
******************************

*Definition*: Repetitive Data Item starting with a one-octet Repetition Factor
followed by at least one Mode S Address of 3-octets length.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 24 bits [``........................``]

    - raw value

NOTE:
    - This data item shall be sent even if there is no Mode S
      Address. In this case it is reduced in length to one octet
      only (REP =0) with all bits set to zero.

I017/220 - Aircraft Address
***************************

*Definition*: Aircraft address (24-bits Mode S address) assigned uniquely to
each aircraft.

*Structure*:

- 24 bits [``........................``]

- raw value

I017/221 - Duplicate Address Reference Number (DRN)
***************************************************

*Definition*: A number uniquely identifying the aircraft in case the Mode-S
Address is not unique.

*Structure*:

- 16 bits [``................``]

- raw value

NOTE:
    1. The DRN shall be added to the Track Data message, if the
       radar node, which is sending the Track Data messages,
       detects two or more aircraft with the same mode-S address
       in its coverage. How the numbers are generated is determined
       by the sending station.
    2. The radar node receiving the Track Data Messages containing
       a DRN shall add this DRN in the corresponding ”Cancellation
       of Track Data” message.
    3. The DRN is used to associate the ”Cancellation of Track
       Data” message with the corresponding ”Track Data” messages.
    4. The cluster controller node will not use the DRN in the
       track data message, because there is no cancellation.

I017/230 - Transponder Capability
*********************************

*Definition*: Communications capability of the transponder received in the
All-Call reply when the aircraft is initially acquired.

*Structure*:

    **I017/230/CA** - *Communications Capability of the Transponder*

    - 3 bits [``...``]

    - values:

        | 0: No communications capability (surveillance only), no ability to set CA code 7 either airborne or on the ground
        | 1: Reserved
        | 2: Reserved
        | 3: Reserved
        | 4: At Least Comm. A and Comm. B capability and the ability to set CA code 7 and on the ground
        | 5: At Least Comm. A and Comm. B capability and the ability to set CA code 7 and airborne
        | 6: At Least Comm. A and Comm. B capability and the ability to set CA code 7 and either airborne or on the ground
        | 7: Signifies the DR field is not equal to 0 or the FS field equals 2, 3, 4 or 5 and either airborne or on the ground SI/II-capabilities of the Transponder

    **I017/230/SI** - *SI/II-capabilities of the Transponder*

    - 1 bit [``.``]

    - values:

        | 0: Transponder SI capable
        | 1: Transponder not SI capable

    **I017/230/(spare)**

    - 4 bits [``....``]

I017/240 - Track Status
***********************

*Definition*: Status of the track position

*Structure*:

    **I017/240/CST** - *Track Coasted*

    - 1 bit [``.``]

    - values:

        | 0: Measured position
        | 1: No measured position (coasted)

    **I017/240/FLT** - *Flight Level Tracking*

    - 1 bit [``.``]

    - values:

        | 0: Last Measured Flight Level
        | 1: Predicted Flight Level

    **I017/240/(spare)**

    - 6 bits [``......``]

NOTE:
    - This item shall not be sent when CST and FLT equal zero.

I017/350 - Cluster Station/Node List
************************************

*Definition*: List of stations/nodes stored in the known network topology
maintained by NMP. The topology to be reported is as defined
in the SCN ICD.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I017/350/SAC** - *System Area Code*

        - 8 bits [``........``]

        - raw value

        **I017/350/SIC** - *System Identification Code*

        - 8 bits [``........``]

        - raw value

NOTE:
    - The up-to-date list of SACs is published on the Eurocontrol
      Web Site (http://www.eurocontrol.int).

I017/360 - Cluster Controller Command State
*******************************************

*Definition*: Defines the current mode and state in which a cluster station,
the radar node taking part in the cluster, should be operating.

*Structure*:

- 8 bits [``........``]

- raw value

NOTE:
    - The Cluster Controller will use this field to select the
      state in which a cluster station should be operating and
      the cluster station will use this field to indicate to the
      cluster controller the adopted state.

I017/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 017
=========================================
- (1) ``I017/010`` - Data Source Identifier
- (2) ``I017/012`` - Data Destination Identifier
- (3) ``I017/000`` - Message Type
- (4) ``I017/350`` - Cluster Station/Node List
- (5) ``I017/220`` - Aircraft Address
- (6) ``I017/221`` - Duplicate Address Reference Number (DRN)
- (7) ``I017/140`` - Time of Day
- ``(FX)`` - Field extension indicator
- (8) ``I017/045`` - Calculated Position in WGS-84 Coordinates
- (9) ``I017/070`` - Mode 3/A Code in Octal Representation
- (10) ``I017/050`` - Flight Level in Binary Representation
- (11) ``I017/200`` - Track Velocity in Polar Co-ordinates
- (12) ``I017/230`` - Transponder Capability
- (13) ``I017/240`` - Track Status
- (14) ``I017/210`` - Mode S Address List
- ``(FX)`` - Field extension indicator
- (15) ``I017/360`` - Cluster Controller Command State
- (16) ``(spare)``
- (17) ``(spare)``
- (18) ``(spare)``
- (19) ``(spare)``
- (20) ``(spare)``
- (21) ``I017/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator
