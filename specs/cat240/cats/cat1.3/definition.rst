Asterix category 240 - Radar Video Transmission
===============================================
**category**: 240

**edition**: 1.3

**date**: 2015-05-13

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I240/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages at
the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Video Summary message
    | 2: Video message

Notes:

    1. In applications where transactions of various types are exchanged,
       the Message Type Data Item facilitates the proper report handling
       at the receiver side.
    2. All Message Type values are reserved for common standard use.
    3. The list of items present for the two message types is defined in
       the following table.

    Table: Message Types ::

        Type Item       001         002
        I240/000        M           M
        I240/010        M           M
        I240/020        X           M
        I240/030        M           X
        I240/040        X           O(1)
        I240/041        X           O(1)
        I240/048        X           M
        I240/049        X           M
        I240/050        X           O(2)
        I240/051        X           O(2)
        I240/052        X           O(2)
        I240/140        O           O

    (1) - Either Item I240/040 or I240/041 shall be present in each Video Message
    (2) - Either Item I240/050 or I240/051 or I240/052 shall be present in each video
          message

I240/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system from which the data are received.

*Structure*:

    **I240/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I240/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note:
    - The up-to-date list of SACs is published on the EUROCONTROL Web Site
      (http://www.eurocontrol.int/asterix)

I240/020 - Video Record Header
******************************

*Definition*: Contains a message sequence identifier.

*Structure*:

- 32 bits [``................................``]

- unsigned integer

Note:
    - The Message Sequence Identifier is used by the receiving application
      to detect lost messages.

I240/030 - Video Summary
************************

*Definition*: Contains an ASCII string (free text to define stream meta data).

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 8 bits [``........``]

    - Ascii string (8-bits per character)

I240/040 - Video Header Nano
****************************

*Definition*: Defines a group of video cells corresponding to a video radial: all cells
have the same size in azimuth and range and are consecutive in range.

*Structure*:

    **I240/040/STARTAZ** - *Start Azimuth of the Cells Group*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`>= 0` °
    - value :math:`< 360` °

    **I240/040/ENDAZ** - *End Azimuth of the Cells Group*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`>= 0` °
    - value :math:`< 360` °

    **I240/040/STARTRG** - *Starting Range of the Cells Group, Expressed in Number of Cells*

    - 32 bits [``................................``]

    - unsigned integer

    **I240/040/CELLDUR** - *Video Cell Duration in Nano-seconds*

    - 32 bits [``................................``]

    - unsigned quantity
    - unit: "ns"
    - LSB = :math:`1` ns

I240/041 - Video Header Femto
*****************************

*Definition*: Defines a group of video cells corresponding to a video radial: all cells
have the same size in azimuth and range and are consecutive in range.

*Structure*:

    **I240/041/STARTAZ** - *Start Azimuth of the Cells Group*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`>= 0` °
    - value :math:`< 360` °

    **I240/041/ENDAZ** - *End Azimuth of the Cells Group*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`>= 0` °
    - value :math:`< 360` °

    **I240/041/STARTRG** - *Starting Range of the Cells Group, Expressed in Number of Cells*

    - 32 bits [``................................``]

    - unsigned integer

    **I240/041/CELLDUR** - *Video Cell Duration in Femto-seconds*

    - 32 bits [``................................``]

    - unsigned quantity
    - unit: "fs"
    - LSB = :math:`1` fs

I240/048 - Video Cells Resolution & Data Compression Indicator
**************************************************************

*Definition*: This Data Item defines the bit resolution used in the coding of the video
signal amplitude in all cells of the video group as well as an indicator
whether data compression has been applied.

*Structure*:

    **I240/048/C** - *Data Compression Indicator*

    - 1 bit [``.``]

    - values:

        | 0: No compression applied
        | 1: Compression applied

    **I240/048/(spare)**

    - 7 bits [``.......``]

    **I240/048/RES** - *Bit Resolution*

    - 8 bits [``........``]

    - values:

        | 1: Monobit Resolution (1 bit)
        | 2: Low Resolution (2 bits)
        | 3: Medium Resolution (4 bits)
        | 4: High Resolution (8 bits)
        | 5: Very High Resolution (16 bits)
        | 6: Ultra High Resolution (32 bits)

Note:
    - When the Data Compression Indicator (C) is set, shows that a data
      compression technique has been applied. The actual algorithm used
      and the related parameters have to be specified in a relevant ICD
      (Interface Control Document).

I240/049 - Video Octets & Video Cells Counters
**********************************************

*Definition*: This Data Item contains the number of “valid” octets (i.e. nonempty octets)
used in the coding of the video signal amplitude and the number of “valid”
cells in the video group.

*Structure*:

    **I240/049/NBVB** - *Number of 'valid' Octets*

    - 16 bits [``................``]

    - unsigned integer

    **I240/049/NBCELLS** - *Number of 'valid' Cells*

    - 24 bits [``........................``]

    - unsigned integer

I240/050 - Video Block Low Data Volume
**************************************

*Definition*: Contains a group of video cells corresponding to a video radial; all cells
have the same size in azimuth and range and are consecutive in range. This
item shall be used in cases where a low data volume, up to 1020 bytes, will
be transmitted.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 32 bits [``................................``]

    - raw value

Notes:

    1. The first cell in the block is always the closest to the sensor and
       the following cells are in increasing range order.
    2. To get the range in meters of the cell at position “NU_CELL” in the
       data stream, the following formula shall be used:
       D = CELL_DUR(in seconds) * (START_RG + NU_CELL - 1) * c/(2.)
       where c = 299 792 458 m/s: light celerity.

I240/051 - Video Block Medium Data Volume
*****************************************

*Definition*: Contains a group of video cells corresponding to a video radial; all cells
have the same size in azimuth and range and are consecutive in range. This
item shall be used in cases where a medium data volume, up to 16320 bytes,
will be transmitted.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 512 bits [``... 512 bits ...``]

    - raw value

Notes:

    1. The first cell in the block is always the closest to the sensor and
       the following cells are in increasing range order.
    2. To get the range in meters of the cell at position “NU_CELL” in the
       data stream, the following formula shall be used:
       D = CELL_DUR(in seconds) * (START_RG + NU_CELL - 1) * c/(2.)
       where c = 299 792 458 m/s: light celerity.

I240/052 - Video Block High Data Volume
***************************************

*Definition*: Contains a group of video cells corresponding to a video radial; all cells
have the same size in azimuth and range and are consecutive in range. This
item shall be used in cases where a high data volume, up to 65024 bytes,
will be transmitted.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 2048 bits [``... 2048 bits ...``]

    - raw value

Notes:

    1. The first cell in the block is always the closest to the sensor and
       the following cells are in increasing range order.
    2. The maximum value of REP that should be used is 254, in order to keep
       the maximum size of the field at 64kbytes.
    3. To get the range in meters of the cell at position “NU_CELL” in the
       data stream, the following formula shall be used:
       D = CELL_DUR(in seconds) * (START_RG + NU_CELL - 1) * c/(2.)
       where c = 299 792 458 m/s: light celerity.

I240/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

Note:
    - The time information, shall reflect the exact time of an event,
      expressed as a number of 1/128 s elapsed since last midnight.

I240/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item (RE)

I240/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 240
=========================================
- (1) ``I240/010`` - Data Source Identifier
- (2) ``I240/000`` - Message Type
- (3) ``I240/020`` - Video Record Header
- (4) ``I240/030`` - Video Summary
- (5) ``I240/040`` - Video Header Nano
- (6) ``I240/041`` - Video Header Femto
- (7) ``I240/048`` - Video Cells Resolution & Data Compression Indicator
- ``(FX)`` - Field extension indicator
- (8) ``I240/049`` - Video Octets & Video Cells Counters
- (9) ``I240/050`` - Video Block Low Data Volume
- (10) ``I240/051`` - Video Block Medium Data Volume
- (11) ``I240/052`` - Video Block High Data Volume
- (12) ``I240/140`` - Time of Day
- (13) ``I240/RE`` - Reserved Expansion Field
- (14) ``I240/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator
