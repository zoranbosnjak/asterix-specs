Asterix category 008 - Monoradar Derived Weather Information
============================================================
**category**: 008

**edition**: 1.2

**date**: 2014-08-24

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I008/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages
at the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Polar vector
    | 2: Cartesian vector of start point/length
    | 3: Contour record
    | 4: Cartesian start point and end point vector
    | 254: SOP message
    | 255: EOP message



I008/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I008/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I008/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    The defined SACs are on the EUROCONTROL ASTERIX website
    (www.eurocontrol.int/asterix)

I008/020 - Vector Qualifier
***************************

*Definition*: Precipitation intensity level, shading orientation of the vectors
representing the precipitation area and coordinate system used.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I008/020/ORG**

    - 1 bit [``.``]

    - values:

        | 0: Local Coordinates
        | 1: System Coordinates

    **I008/020/I** - *Intensity Level*

    - 3 bits [``...``]

    - unsigned integer

    **I008/020/S** - *Shading Orientation with Respect to North*

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

    **I008/020/(spare)**

    - 5 bits [``.....``]

    **I008/020/TST**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test vector

    **I008/020/ER**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Error condition encountered

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:
    For polar vectors bits-4/2 are meaningless and are set to zero.

I008/034 - Sequence of Polar Vectors in SPF Notation
****************************************************

*Definition*: Sequence of weather vectors in local polar coordinates.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I008/034/STR** - *Start Range*

        - 8 bits [``........``]

        - raw value

        **I008/034/ENDR** - *End Range*

        - 8 bits [``........``]

        - raw value

        **I008/034/AZ** - *Azimuth*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 360
        - fractional bits: 16
        - unit: "deg"
        - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Note:
    f is a parameter of the SOP message.

I008/036 - Sequence of Cartesian Vectors in SPF Notation
********************************************************

*Definition*: Sequence of weather vectors, in the representation start point/length,
in local or in system cartesian coordinates.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I008/036/X** - *X-Component*

        - 8 bits [``........``]

        - raw value

        **I008/036/Y** - *Y-Component*

        - 8 bits [``........``]

        - raw value

        **I008/036/LENGTH** - *Length*

        - 8 bits [``........``]

        - raw value


Note:
    1. LSB of [X, Y, L] is calculated as :math:`2^{-6+F}`.
    2. F is a parameter of the SOP message.
    3. Negative values are expressed in 2's complement form, bit-24
       and bit-16 are set to 0 for positive values and 1 for negative
       values.

I008/038 - Sequence of Weather Vectors in SPF Notation
******************************************************

*Definition*: Sequence of weather vectors, in the representation start point/ end
point, in local or in system cartesian coordinates.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I008/038/X1** - *X1-Component*

        - 8 bits [``........``]

        - raw value

        **I008/038/Y1** - *Y1-Component*

        - 8 bits [``........``]

        - raw value

        **I008/038/X2** - *X2-Component*

        - 8 bits [``........``]

        - raw value

        **I008/038/Y2** - *Y2-Component*

        - 8 bits [``........``]

        - raw value


Note:
    1. LSB of [X1, Y1, X2, Y2] is calculated as :math:`2^{-6+f}`.
    2. f is a parameter of the SOP message.
    3. Negative values are expressed in 2's complement form, bits-32,
       24, 16 and 8 are set to 0 for positive values and 1 for negative
       values.

I008/040 - Contour Identifier
*****************************

*Definition*: Contour serial number together with the precipitation intensity levels
and the coordinates system used.

*Structure*:

    **I008/040/ORG**

    - 1 bit [``.``]

    - values:

        | 0: Local Coordinates
        | 1: System Coordinates

    **I008/040/I** - *Intensity Level*

    - 3 bits [``...``]

    - raw value

    **I008/040/(spare)**

    - 2 bits [``..``]

    **I008/040/FSTLST**

    - 2 bits [``..``]

    - values:

        | 0: Intermediate record of a contour
        | 1: Last record of a contour of at least two records
        | 2: First record of a contour of at least two records
        | 3: First and only record, fully defining a contour

    **I008/040/CSN** - *Contour Serial Number*

    - 8 bits [``........``]

    - raw value


Note:
    The Contour Serial Number provides an unambiguous identification
    for each contour record. Within one update cycle, a serial number
    shall never be assigned twice.

I008/050 - Sequence of Contour Points in SPF Notation
*****************************************************

*Definition*: Cartesian coordinates of a variable number of points defining a contour.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I008/050/X1**

        - 8 bits [``........``]

        - raw value

        **I008/050/Y1**

        - 8 bits [``........``]

        - raw value


Note:
    1. LSB of [X1, Y1] is calculated as :math:`2^{-6+f}`.
    2. f is a parameter of the SOP message.
    3. Negative values are expressed in 2's complement form, bit-16
       and bit-8 shall be set to 0 for positive values and 1 for
       negative values.

I008/090 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as Coordinated Universal Time (UTC) time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:
    1. The time of day value is reset to zero each day at midnight.
    2. For time management in radar transmission applications, refer
       to Part 1, paragraph 5.4 [Ref. 1].

I008/100 - Processing Status
****************************

*Definition*: Information concerning the scaling factor currently applied, current
reduction step in use, etc.

*Structure*:

Extended item with first part ``24 bits`` long and optional ``8 bits`` extends.

    **I008/100/F** - *Scaling Factor*

    - 5 bits [``.....``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - LSB = :math:`1`

    **I008/100/R** - *Current Reduction Stage in Use*

    - 3 bits [``...``]

    - raw value

    **I008/100/Q** - *Processing Parameters*

    - 15 bits [``...............``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:
    F: Scaling factor, negative values are represented in 2's complement
    form, bit-24 is set to 0 for positive values and 1 for negative values.
    R: Current reduction stage in use. Normal operation is indicated by a
    value of zero. The actual bit signification is application dependent.
    Q: Processing parameters. The actual bit signification isapplication dependent.

I008/110 - Station Configuration Status
***************************************

*Definition*: Information concerning the use and status of some vital hardware
components of a radar system .

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I008/110/DATA** - *Unspecified Data*

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:
    Due to the diversity in hardware design and requirements of present
    and future radar stations, it is felt impractical to attempt to
    define individual bits.

I008/120 - Total Number of Items Constituting One Weather Picture
*****************************************************************

*Definition*: Total number of vectors, respectively contour points, constituting
the total weather image, provided with the EOP message.

*Structure*:

- 16 bits [``................``]

- unsigned integer



I008/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



I008/RFS - Random Field Sequencing
**********************************

*Definition*: Random Field Sequencing

*Structure*:

Explicit item



User Application Profile for Category 008
=========================================
- (1) ``I008/010`` - Data Source Identifier
- (2) ``I008/000`` - Message Type
- (3) ``I008/020`` - Vector Qualifier
- (4) ``I008/036`` - Sequence of Cartesian Vectors in SPF Notation
- (5) ``I008/034`` - Sequence of Polar Vectors in SPF Notation
- (6) ``I008/040`` - Contour Identifier
- (7) ``I008/050`` - Sequence of Contour Points in SPF Notation
- ``(FX)`` - Field extension indicator
- (8) ``I008/090`` - Time of Day
- (9) ``I008/100`` - Processing Status
- (10) ``I008/110`` - Station Configuration Status
- (11) ``I008/120`` - Total Number of Items Constituting One Weather Picture
- (12) ``I008/038`` - Sequence of Weather Vectors in SPF Notation
- (13) ``I008/SP`` - Special Purpose Field
- (14) ``I008/RFS`` - Random Field Sequencing
- ``(FX)`` - Field extension indicator

