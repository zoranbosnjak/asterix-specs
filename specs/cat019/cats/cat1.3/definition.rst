Asterix category 019 - Multilateration System Status Messages
=============================================================
**category**: 019

**edition**: 1.3

**date**: 2010-12-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I019/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the
messages at the receiver side by further defining the type of
information.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Start of Update Cycle
    | 2: Periodic Status Message
    | 3: Event-triggered Status Message


NOTES:
  1. In applications where data of various types is exchanged, the Message
     Type Data Item facilitates the proper message handling at the receiver
     side.
  2. All Message Type values are reserved for common standard use.

I019/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system from which the data is received.

*Structure*:

    **I019/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I019/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I019/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Note:

    The time of day value is reset to zero each day at midnight.

I019/550 - System Status
************************

*Definition*: Information concerning the configuration and status of a System.

*Structure*:

    **I019/550/NOGO** - *Operational Release Status of the System*

    - 2 bits [``..``]

    - values:

        | 0: Operational
        | 1: Degraded
        | 2: NOGO
        | 3: Undefined

    **I019/550/OVL** - *Overload Indicator*

    - 1 bit [``.``]

    - values:

        | 0: No overload
        | 1: Overload

    **I019/550/TSV** - *Time Source Validity*

    - 1 bit [``.``]

    - values:

        | 0: Valid
        | 1: Invalid

    **I019/550/TTF** - *Test Target*

    - 1 bit [``.``]

    - values:

        | 0: Test Target Operative
        | 1: Test Target Failure

    **I019/550/(spare)**

    - 3 bits [``...``]


Note:

    A time source is considered as valid when either externally synchronised
    or running on a local oscillator within the required accuracy of UTC.

I019/551 - Tracking Processor Detailed Status
*********************************************

*Definition*: Information concerning the configuration and status of the Tracking processors.

*Structure*:

    **I019/551/TP1A**

    - 1 bit [``.``]

    - values:

        | 0: Standby
        | 1: Exec

    **I019/551/TP1B**

    - 1 bit [``.``]

    - values:

        | 0: Faulted
        | 1: Good

    **I019/551/TP2A**

    - 1 bit [``.``]

    - values:

        | 0: Standby
        | 1: Exec

    **I019/551/TP2B**

    - 1 bit [``.``]

    - values:

        | 0: Faulted
        | 1: Good

    **I019/551/TP3A**

    - 1 bit [``.``]

    - values:

        | 0: Standby
        | 1: Exec

    **I019/551/TP3B**

    - 1 bit [``.``]

    - values:

        | 0: Faulted
        | 1: Good

    **I019/551/TP4A**

    - 1 bit [``.``]

    - values:

        | 0: Standby
        | 1: Exec

    **I019/551/TP4B**

    - 1 bit [``.``]

    - values:

        | 0: Faulted
        | 1: Good


Note:

    Both Bits of one TP set to zero means, that this TP is not used
    in the system.

I019/552 - Remote Sensor Detailed Status
****************************************

*Definition*: Information concerning the configuration and status of the Remote Sensors (RS)

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I019/552/RSI** - *8-bit Identification Number of RS*

        - 8 bits [``........``]

        - raw value

        **I019/552/(spare)**

        - 1 bit [``.``]

        **I019/552/RS1090** - *Receiver 1090 MHz*

        - 1 bit [``.``]

        - values:

            | 0: Not present
            | 1: Present

        **I019/552/TX1030** - *Transmitter 1030 MHz*

        - 1 bit [``.``]

        - values:

            | 0: Not present
            | 1: Present

        **I019/552/TX1090** - *Transmitter 1090 MHz*

        - 1 bit [``.``]

        - values:

            | 0: Not present
            | 1: Present

        **I019/552/RSS** - *RS Status*

        - 1 bit [``.``]

        - values:

            | 0: Faulted
            | 1: Good

        **I019/552/RSO** - *RS Operational*

        - 1 bit [``.``]

        - values:

            | 0: Offline
            | 1: Online

        **I019/552/(spare)**

        - 2 bits [``..``]



I019/553 - Reference Transponder Detailed Status
************************************************

*Definition*: Information concerning the configuration and status of the Reference Transponder.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I019/553/REFTR1** - *Ref Trans 1 Status*

    - 2 bits [``..``]

    - values:

        | 1: Warning
        | 2: Faulted
        | 3: Good

    **I019/553/(spare)**

    - 2 bits [``..``]

    **I019/553/REFTR2** - *Ref Trans 2 Status*

    - 2 bits [``..``]

    - values:

        | 1: Warning
        | 2: Faulted
        | 3: Good

    **I019/553/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I019/553/REFTR3** - *Ref Trans 3 Status*

    - 2 bits [``..``]

    - values:

        | 1: Warning
        | 2: Faulted
        | 3: Good

    **I019/553/(spare)**

    - 2 bits [``..``]

    **I019/553/REFTR4** - *Ref Trans 4 Status*

    - 2 bits [``..``]

    - values:

        | 1: Warning
        | 2: Faulted
        | 3: Good

    **I019/553/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I019/600 - Position of the MLT System Reference Point
*****************************************************

*Definition*: Position of the MLT reference point in WGS-84 Coordinates.

*Structure*:

    **I019/600/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 30
    - unit: "deg"
    - LSB = :math:`180 / {2^{30}}` deg = :math:`180 / {1073741824}` deg :math:`\approx 1.6763806343078613e-07` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I019/600/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 30
    - unit: "deg"
    - LSB = :math:`180 / {2^{30}}` deg = :math:`180 / {1073741824}` deg :math:`\approx 1.6763806343078613e-07` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg



I019/610 - Height of the MLT System Reference Point
***************************************************

*Definition*: Height of the MLT system reference point in two’s complement form.
The height shall use mean sea level as the zero reference level.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 1
- fractional bits: 2
- unit: "m"
- LSB = :math:`1 / {2^{2}}` m = :math:`1 / {4}` m :math:`\approx 0.25` m
- value :math:`>= -8192` m
- value :math:`<= 8192` m



I019/620 - WGS-84 Undulation
****************************

*Definition*: WGS-84 undulation value of the MLT system reference point, in meters.
Geoid undulation value is the difference between the ellipsoidal height
and the height above mean sea level

*Structure*:

- 8 bits [``........``]

- signed quantity
- scaling factor: 1
- fractional bits: 0
- unit: "m"
- LSB = :math:`1` m



I019/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I019/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 019
=========================================
- (1) ``I019/010`` - Data Source Identifier
- (2) ``I019/000`` - Message Type
- (3) ``I019/140`` - Time of Day
- (4) ``I019/550`` - System Status
- (5) ``I019/551`` - Tracking Processor Detailed Status
- (6) ``I019/552`` - Remote Sensor Detailed Status
- (7) ``I019/553`` - Reference Transponder Detailed Status
- ``(FX)`` - Field extension indicator
- (8) ``I019/600`` - Position of the MLT System Reference Point
- (9) ``I019/610`` - Height of the MLT System Reference Point
- (10) ``I019/620`` - WGS-84 Undulation
- (11) ``(spare)``
- (12) ``(spare)``
- (13) ``I019/RE`` - Reserved Expansion Field
- (14) ``I019/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

