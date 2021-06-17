Asterix category 001 - Transmission of Monoradar Data Target Reports
====================================================================
**category**: 001

**edition**: 1.2

**date**: 2011-08-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I001/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I001/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I001/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    1 The defined SACs are listed in Part 1, Table 2. [Ref.2 ]
    2 The defined SICs are listed in Part 1, Annex B. [Ref.2 ]

I001/020 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the radar data as transmitted by a radar station.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I001/020/TYP**

    - 1 bit [``.``]

    - values:

        | 0: Plot
        | 1: Track

    **I001/020/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual plot or track
        | 1: Simulated plot or track

    **I001/020/SSRPSR** - *Radar Detection in Last Antenna Scan*

    - 2 bits [``..``]

    - values:

        | 0: No detection
        | 1: Sole primary detection
        | 2: Sole secondary detection
        | 3: Combined primary and secondary detection

    **I001/020/ANT**

    - 1 bit [``.``]

    - values:

        | 0: Target report from antenna 1
        | 1: Target report from antenna 2

    **I001/020/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Special Position Identification

    **I001/020/RAB**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Plot or track from a fixed transponder

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I001/020/TST**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test target indicator

    **I001/020/DS1DS2** - *Radar Detection in Last Antenna Scan*

    - 2 bits [``..``]

    - values:

        | 0: Default
        | 1: Unlawful interference (code 7500)
        | 2: Radio-communication failure (code 7600)
        | 3: Emergency (code 7700)

    **I001/020/ME**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Military emergency

    **I001/020/MI**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Military identification

    **I001/020/(spare)**

    - 2 bits [``..``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:

    - Bit-7 (SIM) is used to identify a simulated target report as produced by a traffic simulator.

I001/030 - Warning/Error Conditions
***********************************

*Definition*: Warning/error conditions detected by a radar station for the target
report involved.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I001/030/WE** - *WEVALUE*

    - 7 bits [``.......``]

    - values:

        | 0: No warning nor error condition
        | 1: Garbled reply
        | 2: Reflection
        | 3: Sidelobe reply
        | 4: Split plot
        | 5: Second time around reply
        | 6: Angels
        | 7: Terrestrial vehicles
        | 64: Possible wrong code in Mode-3/A
        | 65: Possible wrong altitude information, transmitted when the Code C credibility check fails together with the Mode-C code in binary notation
        | 66: Possible phantom MSSR plot
        | 80: Fixed PSR plot
        | 81: Slow PSR plot
        | 82: Low quality PSR plot

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Warning/error condition values 0-63 are reserved for common
       standard use, whereas the values 64-127 are application dependent.

I001/040 - Measured Position in Polar Co-ordinates
**************************************************

*Definition*: Measured position of an aircraft in local polar co-ordinates.

*Structure*:

    **I001/040/RHO**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`<= 512` NM

    **I001/040/THETA**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Note:

    - When expressed in 16 bits, signed or unsigned azimuths have the same value.

I001/042 - Calculated Position in Cartesian Co-ordinates
********************************************************

*Definition*: Calculated position of an aircraft in Cartesian co-ordinates.

*Structure*:

    **I001/042/X** - *X-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 6
    - unit: "NM"
    - LSB = :math:`1 / {2^{6}}` NM = :math:`1 / {64}` NM :math:`\approx 0.015625` NM
    - value :math:`>= -512` NM
    - value :math:`<= 512` NM

    **I001/042/Y** - *Y-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 6
    - unit: "NM"
    - LSB = :math:`1 / {2^{6}}` NM = :math:`1 / {64}` NM :math:`\approx 0.015625` NM
    - value :math:`>= -512` NM
    - value :math:`<= 512` NM


Notes:

    1. LSB is calculated as :math:`2^{-6+f}`.
    2. A default quantisation unit of 1/64 NM is obtained for a value of f = 0.
    3. Negative values are expressed in 2's complement form, bit-32 and
       bit-16 shall be set to 0 for positive values and 1 for negative values.

I001/050 - Mode-2 Code in Octal Representation
**********************************************

*Definition*: Reply to Mode-2 interrogation.

*Structure*:

    **I001/050/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I001/050/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I001/050/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-2 code as derived from the reply of the transponder
        | 1: Smoothed Mode-2 code as provided by a local tracker

    **I001/050/(spare)**

    - 1 bit [``.``]

    **I001/050/MODE2** - *Mode-2 Code in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Notes:

    1. Smoothed Mode-2 data (bit-14 set to one) is used when the plot
       contains no Mode-2 code or the Mode-2 codes of the plot and track
       are different.
    2. Bits-16/15 have no meaning in the case of a smoothed Mode-2 and
       are set to 0 for a calculated track.

I001/060 - Mode-2 Code Confidence Indicator
*******************************************

*Definition*: Confidence level for each bit of a Mode-2 reply as provided by a monopulse SSR station.

*Structure*:

    **I001/060/(spare)**

    - 4 bits [``....``]

    **I001/060/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I001/060/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I001/060/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I001/060/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I001/060/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I001/060/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I001/060/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I001/060/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I001/060/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I001/060/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4

    **I001/060/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I001/060/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1


Note:
    - This Data Item is only transmitted if at least one pulse is of low quality.

I001/070 - Mode-3/A Code in Octal Representation
************************************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I001/070/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I001/070/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I001/070/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Smoothed Mode-3/A code as provided by a local tracker

    **I001/070/(spare)**

    - 1 bit [``.``]

    **I001/070/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Notes:

    1. The detector signals a garbled code (bit-15 set to one) when at
       least two replies are overlapping.
    2. Smoothed Mode-3/A data (bit-14 set to a one) are used in the case
       of the absence of Mode-3/A code information in the plot, or in the
       case of a difference between the plot and track Mode-3/A code information.
    3. Bits-16/15 have no meaning in the case of a smoothed Mode-3/A and
       are set to 0 for a calculated track.

I001/080 - Mode-3/A Code Confidence Indicator
*********************************************

*Definition*: Confidence level for each bit of a Mode-3/A reply as provided by a monopulse SSR station.

*Structure*:

    **I001/080/(spare)**

    - 4 bits [``....``]

    **I001/080/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I001/080/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I001/080/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I001/080/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I001/080/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I001/080/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I001/080/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I001/080/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I001/080/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I001/080/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4

    **I001/080/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I001/080/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1



I001/090 - Mode-C Code in Binary Representation
***********************************************

*Definition*: Mode-C height converted into binary representation.

*Structure*:

    **I001/090/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I001/090/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I001/090/HGT** - *Mode-C HEIGHT*

    - 14 bits [``..............``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL


Notes:

    1. The detector signals a garbled code when at least two replies are overlapping.
    2. The maximum height which can be represented is 204 775 ft.
       Practically the maximum valid value is 126 750 ft (refer to ICAO Annex 10).
    3. Negative values are expressed in 2's complement form, bit-14 is
       set to 0 for positive values and 1 for negative values.

I001/100 - Mode-C Code and Code Confidence Indicator
****************************************************

*Definition*: Mode-C height in Gray notation as received from the transponder together
with the confidence level for each reply bit as provided by a monopulse SSR station.

*Structure*:

    **I001/100/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I001/100/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I001/100/(spare)**

    - 2 bits [``..``]

    **I001/100/MODEC** - *Mode-C Reply in Gray Notation*

    - 12 bits [``............``]

    - raw value

    **I001/100/(spare)**

    - 4 bits [``....``]

    **I001/100/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I001/100/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I001/100/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I001/100/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I001/100/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I001/100/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I001/100/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I001/100/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1

    **I001/100/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I001/100/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I001/100/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I001/100/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4


Notes:

    1. This Data Item is only transmitted if at least one pulse is of low quality.
    2. The detector signals a garbled code when at least two replies are overlapping.

I001/120 - Measured Radial Doppler Speed
****************************************

*Definition*: Radial component of the ground speed as measured by means of Doppler filter banks in radar signal processors.

*Structure*:

- 8 bits [``........``]

- signed quantity
- scaling factor: 1
- fractional bits: 8
- unit: "NM/s"
- LSB = :math:`1 / {2^{8}}` NM/s = :math:`1 / {256}` NM/s :math:`\approx 0.00390625` NM/s


Notes:
    1. LSB is calculated as :math:`2^{-14+f}`.
    2. A default quantisation unit of 14.0625 kt and a maximum of +/- 1 800 kt
       is obtained for a value of f = 6.
    3. Negative values are expressed in 2's complement form, bit-8 is
       set to 0 for positive values and 1 for negative values.

I001/130 - Radar Plot Characteristics
*************************************

*Definition*: Additional information on the quality of the target report.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I001/130/IND** - *Indicator*

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:
    - The actual meaning of the bits is application dependent."

    - This Data Item may contain parameters such as plot runlength
      (primary and secondary), difference between primary and secondary
      derived azimuth, pulse amplitude, etc.

I001/131 - Received Power
*************************

*Definition*: Measurement of the received power.

*Structure*:

- 8 bits [``........``]

- signed quantity
- scaling factor: 1
- fractional bits: 0
- unit: "dBm"
- LSB = :math:`1` dBm


Notes:

    1. POWER is the measured value of the power received on the sum
       pattern for a plot.
    2. Negative values are expressed in 2's complement form, bit-8 is
       set to 0 for positive values and 1 for negative values.

I001/141 - Truncated Time of Day
********************************

*Definition*: Absolute time stamping expressed as Coordinated Universal Time (UTC) time.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. The exchange of this Data Item allows the easy derivation of the
       correct UTC time value, provided that the clocks at the data source
       and sink(s) are less than 512 seconds out of synchronisation.
       Special care has to be taken at the transition of an "all ones"
       value to an "all zeros" value (every 512 seconds).
    2. The time of day value is reset to 0 each day at midnight.
    3. For time management in radar transmission applications, refer to
       Part 1, paragraph 5.4 [Ref. 2].

I001/150 - Presence of X-Pulse
******************************

*Definition*: Presence of the X-Pulse for the various modes applied in the interrogation interlace pattern.

*Structure*:

    **I001/150/XA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: X-pulse received in Mode-3/A reply

    **I001/150/(spare)**

    - 1 bit [``.``]

    **I001/150/XC**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: X-pulse received in Mode-C reply

    **I001/150/(spare)**

    - 2 bits [``..``]

    **I001/150/X2**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: X-pulse received in Mode-2 reply

    **I001/150/(spare)**

    - 2 bits [``..``]


Note:

    - This Data Item is transmitted only if at least one X-pulse has been
      received in a Mode-A, Mode-2 or Mode-C reply.

I001/161 - Track Plot Number
****************************

*Definition*: An integer value representing a unique reference to a track/plot record within a particular track/plot file.

*Structure*:

- 16 bits [``................``]

- raw value


Note:

    - The differentiation between track and plot number is either implicit
      or is made via the Target Report Descriptor (Data Item I001/020).

I001/170 - Track Status
***********************

*Definition*: Status of track derived either from primary and/or secondary radar information.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I001/170/CON**

    - 1 bit [``.``]

    - values:

        | 0: Confirmed Track
        | 1: Track in initialisation phase

    **I001/170/RAD**

    - 1 bit [``.``]

    - values:

        | 0: Primary track
        | 1: SSR/Combined track

    **I001/170/MAN**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Aircraft manoeuvring

    **I001/170/DOU**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Doubtful plot to track association

    **I001/170/RDPC** - *Radar Data Processing Chain*

    - 1 bit [``.``]

    - values:

        | 0: RDP Chain 1
        | 1: RDP Chain 2

    **I001/170/(spare)**

    - 1 bit [``.``]

    **I001/170/GHO**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Ghost track

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I001/170/TRE**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Last report for a track

    **I001/170/(spare)**

    - 6 bits [``......``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Bit-2 (GHO) is used to signal that the track is suspected to have
       been generated by a fake target.
    2. Bit-4 (RDPC) is used to signal the discontinuity of the track numbers.

I001/200 - Calculated Track Velocity in Polar Co-ordinates
**********************************************************

*Definition*: Calculated track velocity expressed in polar co-ordinates.

*Structure*:

    **I001/200/GSP** - *Calculated Groundspeed*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 14
    - unit: "NM/s"
    - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s

    **I001/200/HDG** - *Calculated Heading*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg



I001/210 - Track Quality
************************

*Definition*: Relative track quality.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I001/210/QI** - *Relative Track Quality*

    - 7 bits [``.......``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:

    - Actual bit signification is application dependent.

I001/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



I001/RFS - Random Field Sequencing
**********************************

*Definition*: Random Field Sequencing

*Structure*:

Explicit item



User Application Profile for Category 001
=========================================
This category has multiple UAPs.

plot
----
- (1) ``I001/010`` - Data Source Identifier
- (2) ``I001/020`` - Target Report Descriptor
- (3) ``I001/040`` - Measured Position in Polar Co-ordinates
- (4) ``I001/070`` - Mode-3/A Code in Octal Representation
- (5) ``I001/090`` - Mode-C Code in Binary Representation
- (6) ``I001/130`` - Radar Plot Characteristics
- (7) ``I001/141`` - Truncated Time of Day
- ``(FX)`` - Field extension indicator
- (8) ``I001/050`` - Mode-2 Code in Octal Representation
- (9) ``I001/120`` - Measured Radial Doppler Speed
- (10) ``I001/131`` - Received Power
- (11) ``I001/080`` - Mode-3/A Code Confidence Indicator
- (12) ``I001/100`` - Mode-C Code and Code Confidence Indicator
- (13) ``I001/060`` - Mode-2 Code Confidence Indicator
- (14) ``I001/030`` - Warning/Error Conditions
- ``(FX)`` - Field extension indicator
- (15) ``I001/150`` - Presence of X-Pulse
- (16) ``(spare)``
- (17) ``(spare)``
- (18) ``(spare)``
- (19) ``(spare)``
- (20) ``I001/SP`` - Special Purpose Field
- (21) ``I001/RFS`` - Random Field Sequencing
- ``(FX)`` - Field extension indicator

track
-----
- (1) ``I001/010`` - Data Source Identifier
- (2) ``I001/020`` - Target Report Descriptor
- (3) ``I001/161`` - Track Plot Number
- (4) ``I001/040`` - Measured Position in Polar Co-ordinates
- (5) ``I001/042`` - Calculated Position in Cartesian Co-ordinates
- (6) ``I001/200`` - Calculated Track Velocity in Polar Co-ordinates
- (7) ``I001/070`` - Mode-3/A Code in Octal Representation
- ``(FX)`` - Field extension indicator
- (8) ``I001/090`` - Mode-C Code in Binary Representation
- (9) ``I001/141`` - Truncated Time of Day
- (10) ``I001/130`` - Radar Plot Characteristics
- (11) ``I001/131`` - Received Power
- (12) ``I001/120`` - Measured Radial Doppler Speed
- (13) ``I001/170`` - Track Status
- (14) ``I001/210`` - Track Quality
- ``(FX)`` - Field extension indicator
- (15) ``I001/050`` - Mode-2 Code in Octal Representation
- (16) ``I001/080`` - Mode-3/A Code Confidence Indicator
- (17) ``I001/100`` - Mode-C Code and Code Confidence Indicator
- (18) ``I001/060`` - Mode-2 Code Confidence Indicator
- (19) ``I001/030`` - Warning/Error Conditions
- (20) ``I001/SP`` - Special Purpose Field
- (21) ``I001/RFS`` - Random Field Sequencing
- ``(FX)`` - Field extension indicator
- (22) ``I001/150`` - Presence of X-Pulse

