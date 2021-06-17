Asterix category 009 - Composite Weather Reports
================================================
**category**: 009

**edition**: 2.1

**date**: 2014-10-22

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I009/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages
at the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 2: Cartesian vector
    | 253: Intermediate-update-step message
    | 254: Start-of-picture message
    | 255: End-of-picture message



I009/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I009/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I009/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    The defined SACs are on the EUROCONTROL ASTERIX website
    (www.eurocontrol.int/asterix)

I009/020 - Vector Qualifier
***************************

*Definition*: This Data Item defines the orientation of the following sequence
of Cartesian vectors, their intensity level and the relevant
coordinate system.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I009/020/ORG**

    - 1 bit [``.``]

    - values:

        | 0: Local Coordinates
        | 1: System Coordinates

    **I009/020/I** - *Intensity Level*

    - 3 bits [``...``]

    - unsigned integer

    **I009/020/S** - *Shading Orientation with Respect to North*

    - 3 bits [``...``]

    - values:

        | 0: 0°
        | 1: 22.5°
        | 2: 45°
        | 3: 67.5°
        | 4: 90°
        | 5: 112.5°
        | 6: 135°
        | 7: 157.5°

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


For polar vectors "S-bits" are meaningless and shall be set to zero.

I009/030 - Sequence of Cartesian Vectors
****************************************

*Definition*: Sequence of weather vectors in local or system Cartesian coordinates.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I009/030/X** - *X-coordinate*

        - 16 bits [``................``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 0
        - unit: "NM"
        - LSB = :math:`1` NM

            remark
                Adjust with scaling factor 'F'

        **I009/030/Y** - *Y-coordinate*

        - 16 bits [``................``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 0
        - unit: "NM"
        - LSB = :math:`1` NM

            remark
                Adjust with scaling factor 'F'

        **I009/030/L** - *Vector Length*

        - 16 bits [``................``]

        - unsigned integer

            remark
                Adjust with scaling factor 'F'


'F' shall be incorporated as a parameter in the SOP message.

I009/060 - Synchronisation/Control Signal
*****************************************

*Definition*: This Data Item provides the serial Step Number.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I009/060/SN** - *Step Number*

    - 6 bits [``......``]

    - unsigned integer

    **I009/060/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I009/070 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


The time of day value is reset to zero each day at midnight.

I009/080 - Processing Status
****************************

*Definition*: Processing status of the Track Server.

*Structure*:

Extended item with first part ``24 bits`` long and optional ``8 bits`` extends.

    **I009/080/F** - *Scaling Factor*

    - 5 bits [``.....``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - LSB = :math:`1`

    **I009/080/R** - *Current Reduction Stage in Use*

    - 3 bits [``...``]

    - raw value

    **I009/080/Q** - *Processing Parameters*

    - 15 bits [``...............``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I009/090 - Radar Configuration and Status
*****************************************

*Definition*: Current radar configuration and status of all operational radars.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I009/090/SAC** - *SAC of Radar Concerned*

        - 8 bits [``........``]

        - raw value

        **I009/090/SIC** - *SIC of Radar Concerned*

        - 8 bits [``........``]

        - raw value

        **I009/090/(spare)**

        - 3 bits [``...``]

        **I009/090/CP** - *Circular Polarisation*

        - 1 bit [``.``]

        - raw value

        **I009/090/WO** - *Weather Channel Overload*

        - 1 bit [``.``]

        - raw value

        **I009/090/R** - *Reduction Step in Use By Radar  Concerned*

        - 3 bits [``...``]

        - raw value



I009/100 - Vector Count
***********************

*Definition*: Total number of vectors defining a complete weather picture.

*Structure*:

- 16 bits [``................``]

- unsigned integer



User Application Profile for Category 009
=========================================
- (1) ``I009/010`` - Data Source Identifier
- (2) ``I009/000`` - Message Type
- (3) ``I009/020`` - Vector Qualifier
- (4) ``I009/030`` - Sequence of Cartesian Vectors
- (5) ``I009/060`` - Synchronisation/Control Signal
- (6) ``I009/070`` - Time of Day
- (7) ``I009/080`` - Processing Status
- ``(FX)`` - Field extension indicator
- (8) ``I009/090`` - Radar Configuration and Status
- (9) ``I009/100`` - Vector Count

