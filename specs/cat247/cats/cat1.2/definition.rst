Asterix category 247 - Version Number Exchange
==============================================
**category**: 247

**edition**: 1.2

**date**: 2008-02-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I247/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I247/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I247/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    The defined SACs are on the EUROCONTROL ASTERIX website
    (www.eurocontrol.int/asterix)

I247/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value


The service identification is allocated by the system.

I247/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


The time of day value is reset to zero each day at midnight.

I247/550 - Category Version Number Report
*****************************************

*Definition*: Version number of Categories used.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I247/550/CAT** - *Category*

        - 8 bits [``........``]

        - unsigned integer

        **I247/550/MAIN** - *Main Version Number*

        - 8 bits [``........``]

        - unsigned integer

        **I247/550/SUB** - *Sub Version Number*

        - 8 bits [``........``]

        - unsigned integer



I247/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I247/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 247
=========================================
- (1) ``I247/010`` - Data Source Identifier
- (2) ``I247/015`` - Service Identification
- (3) ``I247/140`` - Time of Day
- (4) ``I247/550`` - Category Version Number Report
- (5) ``(spare)``
- (6) ``I247/SP`` - Special Purpose Field
- (7) ``I247/RE`` - Reserved Expansion Field
- ``(FX)`` - Field extension indicator

