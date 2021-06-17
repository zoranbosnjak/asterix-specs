Asterix category 034 - Transmission of Monoradar Service Messages
=================================================================
**category**: 034

**edition**: 1.27

**date**: 2007-05-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I034/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of
the messages at the receiver side by further defining
the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: North marker message
    | 2: Sector crossing message
    | 3: Geographical filtering message
    | 4: Jamming strobe message


Notes:

    1. In applications where transactions of various
       types are exchanged, the Message Type Data Item facilitates the
       proper message handling at the receiver side.
    2. All Message Type values are reserved for common standard use.
    3. The list of items present for the four message types is defined in
       the following table.

       M stands for mandatory, O for optional, X for never present.

       TODO: message types table

I034/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I034/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I034/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I034/020 - Sector Number
************************

*Definition*: Eight most significant bits of the antenna azimuth defining a particular azimuth sector.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- scaling factor: 360
- fractional bits: 8
- unit: "deg"
- LSB = :math:`360 / {2^{8}}` deg = :math:`360 / {256}` deg :math:`\approx 1.40625` deg



I034/030 - Time of Day
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

    - The time of day value is reset to zero each day at midnight.

I034/041 - Antenna Rotation Speed
*********************************

*Definition*: Antenna rotation period as measured between two consecutive North crossings
or as averaged during a period of time.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    - This item represents the antenna rotation period as measured by the
      radar station between two consecutive North crossings, or a calculated
      antenna rotation speed as averaged during a period of time, or during a
      number of antenna rotation scans.

I034/050 - System Configuration and Status
******************************************

*Definition*: Information concerning the configuration and status of a System.

*Structure*:

Compound item (FX)

    **I034/050/COM** - *Common Part*

        **I034/050/COM/NOGO** - *Operational Release Status of the System*

        - 1 bit [``.``]

        - values:

            | 0: System is released for operational use
            | 1: Operational use of System is inhibited, i.e. the data shall be discarded by an operational SDPS

        **I034/050/COM/RDPC** - *Radar Data Processor Chain Selection Status*

        - 1 bit [``.``]

        - values:

            | 0: RDPC-1 selected
            | 1: RDPC-2 selected

        **I034/050/COM/RDPR** - *Event to Signal a Reset/restart of the Selected Radar Data Processor Chain, I.e. Expect a New Assignment of Track Numbers*

        - 1 bit [``.``]

        - values:

            | 0: Default situation
            | 1: Reset of RDPC

        **I034/050/COM/OVLRDP** - *Radar Data Processor Overload Indicator*

        - 1 bit [``.``]

        - values:

            | 0: Default, no overload
            | 1: Overload in RDP

        **I034/050/COM/OVLXMT** - *Transmission Subsystem Overload Status*

        - 1 bit [``.``]

        - values:

            | 0: Default, no overload
            | 1: Overload in transmission subsystem

        **I034/050/COM/MSC** - *Monitoring System Connected Status*

        - 1 bit [``.``]

        - values:

            | 0: Monitoring system connected
            | 1: Monitoring system disconnected

        **I034/050/COM/TSV** - *Time Source Validity*

        - 1 bit [``.``]

        - values:

            | 0: Valid
            | 1: Invalid

        **I034/050/COM/(spare)**

        - 1 bit [``.``]

    (empty subitem)

    (empty subitem)

    **I034/050/PSR** - *Specific Status Information for a PSR Sensor*

        **I034/050/PSR/ANT** - *Selected Antenna*

        - 1 bit [``.``]

        - values:

            | 0: Antenna 1
            | 1: Antenna 2

        **I034/050/PSR/CHAB** - *Channel A/B Selection Status*

        - 2 bits [``..``]

        - values:

            | 0: No channel selected
            | 1: Channel A only selected
            | 2: Channel B only selected
            | 3: Diversity mode ; Channel A and B selected

        **I034/050/PSR/OVL** - *Overload Condition*

        - 1 bit [``.``]

        - values:

            | 0: No overload
            | 1: Overload

        **I034/050/PSR/MSC** - *Monitoring System Connected Status*

        - 1 bit [``.``]

        - values:

            | 0: Monitoring system connected
            | 1: Monitoring system disconnected

        **I034/050/PSR/(spare)**

        - 3 bits [``...``]

    **I034/050/SSR** - *Specific Status Information for a SSR Sensor*

        **I034/050/SSR/ANT** - *Selected Antenna*

        - 1 bit [``.``]

        - values:

            | 0: Antenna 1
            | 1: Antenna 2

        **I034/050/SSR/CHAB** - *Channel A/B Selection Status*

        - 2 bits [``..``]

        - values:

            | 0: No channel selected
            | 1: Channel A only selected
            | 2: Channel B only selected
            | 3: Invalid combination

        **I034/050/SSR/OVL** - *Overload Condition*

        - 1 bit [``.``]

        - values:

            | 0: No overload
            | 1: Overload

        **I034/050/SSR/MSC** - *Monitoring System Connected Status:*

        - 1 bit [``.``]

        - values:

            | 0: Monitoring system connected
            | 1: Monitoring system disconnected

        **I034/050/SSR/(spare)**

        - 3 bits [``...``]

    **I034/050/MDS** - *Specific Status Information for a Mode S Sensor*

        **I034/050/MDS/ANT** - *Selected Antenna*

        - 1 bit [``.``]

        - values:

            | 0: Antenna 1
            | 1: Antenna 2

        **I034/050/MDS/CHAB** - *Channel A/B Selection Status*

        - 2 bits [``..``]

        - values:

            | 0: No channel selected
            | 1: Channel A only selected
            | 2: Channel B only selected
            | 3: Illegal combination

        **I034/050/MDS/OVLSUR** - *Overload Condition*

        - 1 bit [``.``]

        - values:

            | 0: No overload
            | 1: Overload

        **I034/050/MDS/MSC** - *Monitoring System Connected Status:*

        - 1 bit [``.``]

        - values:

            | 0: Monitoring system connected
            | 1: Monitoring system disconnected

        **I034/050/MDS/SCF** - *Channel A/B Selection Status for Surveillance Co-ordination Function*

        - 1 bit [``.``]

        - values:

            | 0: Channel A in use
            | 1: Channel B in use

        **I034/050/MDS/DLF** - *Channel A/B Selection Status for Data Link Function*

        - 1 bit [``.``]

        - values:

            | 0: Channel A in use
            | 1: Channel B in use

        **I034/050/MDS/OVLSCF** - *Overload in Surveillance Co-ordination Function*

        - 1 bit [``.``]

        - values:

            | 0: No overload
            | 1: Overload

        **I034/050/MDS/OVLDLF** - *Overload in Data Link Function*

        - 1 bit [``.``]

        - values:

            | 0: No overload
            | 1: Overload

        **I034/050/MDS/(spare)**

        - 7 bits [``.......``]

    (empty subitem)



I034/060 - System Processing Mode
*********************************

*Definition*: Status concerning the processing options, in use during the last antenna
revolution, for the various Sensors, composing the System.

*Structure*:

Compound item (FX)

    **I034/060/COM** - *Common Part*

        **I034/060/COM/(spare)**

        - 1 bit [``.``]

        **I034/060/COM/REDRDP** - *Reduction Steps in Use for An Overload of the RDP*

        - 3 bits [``...``]

        - values:

            | 0: No reduction active
            | 1: Reduction step 1 active
            | 2: Reduction step 2 active
            | 3: Reduction step 3 active
            | 4: Reduction step 4 active
            | 5: Reduction step 5 active
            | 6: Reduction step 6 active
            | 7: Reduction step 7 active

        **I034/060/COM/REDXMT** - *Reduction Steps in Use for An Overload of the Transmission Subsystem*

        - 3 bits [``...``]

        - values:

            | 0: No reduction active
            | 1: Reduction step 1 active
            | 2: Reduction step 2 active
            | 3: Reduction step 3 active
            | 4: Reduction step 4 active
            | 5: Reduction step 5 active
            | 6: Reduction step 6 active
            | 7: Reduction step 7 active

        **I034/060/COM/(spare)**

        - 1 bit [``.``]

    (empty subitem)

    (empty subitem)

    **I034/060/PSR** - *Specific Processing Mode Information for a PSR Sensor*

        **I034/060/PSR/POL** - *Polarization in Use by PSR*

        - 1 bit [``.``]

        - values:

            | 0: Linear polarization
            | 1: Circular polarization

        **I034/060/PSR/REDRAP** - *Reduction Steps in Use as Result of An Overload Within the PSR Subsystem*

        - 3 bits [``...``]

        - values:

            | 0: No reduction active
            | 1: Reduction step 1 active
            | 2: Reduction step 2 active
            | 3: Reduction step 3 active
            | 4: Reduction step 4 active
            | 5: Reduction step 5 active
            | 6: Reduction step 6 active
            | 7: Reduction step 7 active

        **I034/060/PSR/STC** - *Sensitivity Time Control Map in Use*

        - 2 bits [``..``]

        - values:

            | 0: STC Map-1
            | 1: STC Map-2
            | 2: STC Map-3
            | 3: STC Map-4

        **I034/060/PSR/(spare)**

        - 2 bits [``..``]

    **I034/060/SSR** - *Specific Processing Mode Information for a SSR Sensor*

        **I034/060/SSR/REDRAD** - *Reduction Steps in Use as Result of An Overload Within the SSR Subsystem*

        - 3 bits [``...``]

        - values:

            | 0: No reduction active
            | 1: Reduction step 1 active
            | 2: Reduction step 2 active
            | 3: Reduction step 3 active
            | 4: Reduction step 4 active
            | 5: Reduction step 5 active
            | 6: Reduction step 6 active
            | 7: Reduction step 7 active

        **I034/060/SSR/(spare)**

        - 5 bits [``.....``]

    **I034/060/MDS** - *Specific Processing Mode Information for a Mode S Sensor*

        **I034/060/MDS/REDRAD** - *Reduction Steps in Use as Result of An Overload Within the Mode S Subsystem*

        - 3 bits [``...``]

        - values:

            | 0: No reduction active
            | 1: Reduction step 1 active
            | 2: Reduction step 2 active
            | 3: Reduction step 3 active
            | 4: Reduction step 4 active
            | 5: Reduction step 5 active
            | 6: Reduction step 6 active
            | 7: Reduction step 7 active

        **I034/060/MDS/CLU** - *Cluster State*

        - 1 bit [``.``]

        - values:

            | 0: Autonomous
            | 1: Not autonomous

        **I034/060/MDS/(spare)**

        - 4 bits [``....``]


Notes:

    - Applicable to all defined secondary subfields. The actual mapping
      between the up to seven data reduction steps and their associated
      data reduction measures is not subject to standardisation.

I034/070 - Message Count Values
*******************************

*Definition*: Message Count values, according the various types of messages, for the
last completed antenna revolution, counted between two North crossings

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I034/070/TYP** - *Type of Message Counter*

        - 5 bits [``.....``]

        - values:

            | 0: No detection (number of misses)
            | 1: Single PSR target reports
            | 2: Single SSR target reports (Non-Mode S)
            | 3: SSR+PSR target reports (Non-Mode S)
            | 4: Single All-Call target reports (Mode S)
            | 5: Single Roll-Call target reports (Mode S)
            | 6: All-Call + PSR (Mode S) target reports
            | 7: Roll-Call + PSR (Mode S) target reports
            | 8: Filter for Weather data
            | 9: Filter for Jamming Strobe
            | 10: Filter for PSR data
            | 11: Filter for SSR/Mode S data
            | 12: Filter for SSR/Mode S+PSR data
            | 13: Filter for Enhanced Surveillance data
            | 14: Filter for PSR+Enhanced Surveillance
            | 15: Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest
            | 16: Filter for PSR+Enhanced Surveillance + all SSR/Mode S data

        **I034/070/COUNT** - *COUNTER*

        - 11 bits [``...........``]

        - unsigned integer



I034/090 - Collimation Error
****************************

*Definition*: Averaged difference in range and in azimuth for the primary target position
with respect to the SSR target position as calculated by the radar station.

*Structure*:

    **I034/090/RNG** - *Range Error*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM

    **I034/090/AZM** - *Azimuth Error*

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 360
    - fractional bits: 14
    - unit: "deg"
    - LSB = :math:`360 / {2^{14}}` deg = :math:`360 / {16384}` deg :math:`\approx 0.02197265625` deg


Notes:

    - Negative values are coded in two’s complement form.

I034/100 - Generic Polar Window
*******************************

*Definition*: Geographical window defined in polar co-ordinates.

*Structure*:

    **I034/100/RHOST** - *Rho Start*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 8
    - unit: "NM"
    - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM
    - value :math:`<= 256` NM

    **I034/100/RHOEND** - *Rho End*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 8
    - unit: "NM"
    - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM
    - value :math:`<= 256` NM

    **I034/100/THETAST** - *Theta Start*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg

    **I034/100/THETAEND** - *Theta End*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg



I034/110 - Data Filter
**********************

*Definition*: Data Filter, which allows suppression of individual data types.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Invalid value
    | 1: Filter for Weather data
    | 2: Filter for Jamming Strobe
    | 3: Filter for PSR data
    | 4: Filter for SSR/Mode S data
    | 5: Filter for SSR/Mode S + PSR data
    | 6: Enhanced Surveillance data
    | 7: Filter for PSR+Enhanced Surveillance data
    | 8: Filter for PSR+Enhanced Surveillance + SSR/Mode S data not in Area of Prime Interest
    | 9: Filter for PSR+Enhanced Surveillance + all SSR/Mode S data


Notes:

    1. This Data Item is often used in conjunction with I034/100 and
       represents a Data Filter for a specific geographical subarea.
       A Data Source may have zero, one or multiple data filters active at any time.
    2. If I034/110 is not accompanied with I034/100, then the Data Filter
       is valid throughout the total area of coverage.

I034/120 - 3D-Position Of Data Source
*************************************

*Definition*: 3D-Position of Data Source in WGS 84 Co-ordinates

*Structure*:

    **I034/120/HGT** - *Height of Data Source*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m

    **I034/120/LAT** - *Latitude*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 23
    - unit: "deg"
    - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I034/120/LON** - *Longitude*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 23
    - unit: "deg"
    - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
    - value :math:`>= -180` deg
    - value :math:`<= 180` deg



I034/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I034/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 034
=========================================
- (1) ``I034/010`` - Data Source Identifier
- (2) ``I034/000`` - Message Type
- (3) ``I034/030`` - Time of Day
- (4) ``I034/020`` - Sector Number
- (5) ``I034/041`` - Antenna Rotation Speed
- (6) ``I034/050`` - System Configuration and Status
- (7) ``I034/060`` - System Processing Mode
- ``(FX)`` - Field extension indicator
- (8) ``I034/070`` - Message Count Values
- (9) ``I034/100`` - Generic Polar Window
- (10) ``I034/110`` - Data Filter
- (11) ``I034/120`` - 3D-Position Of Data Source
- (12) ``I034/090`` - Collimation Error
- (13) ``I034/RE`` - Reserved Expansion Field
- (14) ``I034/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

