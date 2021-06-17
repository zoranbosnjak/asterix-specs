Asterix category 002 - Transmission of Monoradar Service Messages
=================================================================
**category**: 002

**edition**: 1.0

**date**: 1997-11-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I002/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the
messages at the receiver side by further defining the type of
transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: North marker message
    | 2: Sector crossing message
    | 3: South marker message
    | 8: Activation of blind zone filtering
    | 9: Stop of blind zone filtering


Notes:
    1. In application where transactions of various types are exchanged, the
    Message Type Data Item facilitates the proper message handling at the
    receiver side.

    2. Message Type values 1-127 are reserved for common standard use,
    whereas the values 128-255 are application dependent.

I002/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system sending the data.

*Structure*:

    **I002/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I002/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Notes:
    1. The defined SACs are listed in Part 1, Table 2 [Ref. 2]
    2. The defined SICs are listed in Part 1, Annex B [Ref. 2]

I002/020 - Sector Number
************************

*Definition*: Eight most significant bits of the antenna azimuth defining a
particular azimuth sector.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- scaling factor: 360
- fractional bits: 8
- unit: "deg"
- LSB = :math:`360 / {2^{8}}` deg = :math:`360 / {256}` deg :math:`\approx 1.40625` deg


The use of the antenna azimuth as sector number has the
advantage of being independent of the number of sectors
implemented.

I002/030 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. The time of day value is reset to zero each day at midnight.
    2. For time management in radar transmission applications, refer to Part 1,
       paragraph 5.4 [ Ref.2].
    3. Data Item I002/030 can have various logical meanings. In a particular
       message, the logical meaning is implicit from its context (e.g. in a North
       marker message it represents the antenna North crossing time; in a
       sector message it represents the antenna sector crossing time).

I002/041 - Antenna Rotation Speed
*********************************

*Definition*: Antenna rotation period as measured between two
consecutive North crossings or as averaged during a period
of time.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s



I002/050 - Station Configuration Status
***************************************

*Definition*: Information concerning the use and status of some vital
hardware components of the radar system.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I002/050/SCS**

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Due to the diversity in hardware design and requirement of
present and future radar stations, it is felt impractical to attempt to
define the individual bits.

I002/060 - Station Processing Mode
**********************************

*Definition*: Details concerning the present status with respect to
processing parameters and options.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I002/060/SPM**

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


NOTES:
    1. Typical information conveyed within this Data Item includes inter alia
       type of polarisation in use, Moving Target Indicator (MTI) in use and/or
       definition of the range to which MTI is applied, presence of overload
       conditions and the type of load reduction measures in use.
    2. Only the structure of this Data Item is defined, no attempt is made to
       standardise its contents, in order not to hamper any application or future
       development.

I002/070 - Plot Count Values
****************************

*Definition*: Plot count values according to various plot categories, either
for the last full antenna scan or for the last sector processed.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I002/070/A** - *Aerial Identification*

        - 1 bit [``.``]

        - values:

            | 0: Counter for antenna 1
            | 1: Counter for antenna 2

        **I002/070/IDENT**

        - 5 bits [``.....``]

        - values:

            | 1: Sole primary plots
            | 2: Sole SSR plots
            | 3: Combined plots

        **I002/070/COUNTER**

        - 10 bits [``..........``]

        - unsigned integer



I002/080 - Warning/Error Conditions
***********************************

*Definition*: Warning/error conditions affecting the functioning of the
radar system itself.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I002/080/WE** - *W/E Value*

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


NOTE:
    Warning/error condition values 1-63 are reserved for common
    Standard use, whereas the values 64-127 are application
    dependent.

I002/090 - Collimation Error
****************************

*Definition*: Averaged difference in range and in azimuth for the primary
target position with respect to the SSR target position as
calculated by the radar station.

*Structure*:

    **I002/090/RE** - *Range Error*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM

    **I002/090/AE** - *Azimuth Error*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 360
    - fractional bits: 14
    - unit: "deg"
    - LSB = :math:`360 / {2^{14}}` deg = :math:`360 / {16384}` deg :math:`\approx 0.02197265625` deg


NOTES
    1. LSB of RE is calculated as :math:`2^{16-f}`.
    2. A default quantisation unit of 0.022° and a range between -2.8125° and
       +2.7905° is obtained for a value of f=2 .

I002/100 - Dynamic Window Type 1
********************************

*Definition*: Signals the activation of a certain selective filtering function
and in a polar coordinates system the respective
geographical areas.

*Structure*:

    **I002/100/RS** - *Rho Start*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`< 512` NM

    **I002/100/RE** - *Rho End*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`< 512` NM

    **I002/100/TS** - *Theta Start*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg

    **I002/100/TE** - *Theta End*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


The logical meaning of the polar window is defined by its context,
given by the Message Type (Data Item I002/000) in the record
concerned.

I002/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



I002/RFS - Random Field Sequencing
**********************************

*Definition*: Random Field Sequencing

*Structure*:

Explicit item



User Application Profile for Category 002
=========================================
- (1) ``I002/010`` - Data Source Identifier
- (2) ``I002/000`` - Message Type
- (3) ``I002/020`` - Sector Number
- (4) ``I002/030`` - Time of Day
- (5) ``I002/041`` - Antenna Rotation Speed
- (6) ``I002/050`` - Station Configuration Status
- (7) ``I002/060`` - Station Processing Mode
- ``(FX)`` - Field extension indicator
- (8) ``I002/070`` - Plot Count Values
- (9) ``I002/100`` - Dynamic Window Type 1
- (10) ``I002/090`` - Collimation Error
- (11) ``I002/080`` - Warning/Error Conditions
- (12) ``(spare)``
- (13) ``I002/SP`` - Special Purpose Field
- (14) ``I002/RFS`` - Random Field Sequencing
- ``(FX)`` - Field extension indicator

