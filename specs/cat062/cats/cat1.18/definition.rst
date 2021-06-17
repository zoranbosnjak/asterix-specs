Asterix category 062 - SDPS Track Messages
==========================================
**category**: 062

**edition**: 1.18

**date**: 2018-08-13

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I062/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system sending the data.

*Structure*:

    **I062/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I062/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I062/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value



I062/040 - Track Number
***********************

*Definition*: Identification of a track.

*Structure*:

- 16 bits [``................``]

- raw value



I062/060 - Track Mode 3/A Code
******************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I062/060/V** - *Validated*

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I062/060/G** - *Garbled*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I062/060/CH** - *Change in Mode 3/A*

    - 1 bit [``.``]

    - values:

        | 0: No change
        | 1: Mode 3/A has changed

    **I062/060/(spare)**

    - 1 bit [``.``]

    **I062/060/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)



I062/070 - Time Of Track Information
************************************

*Definition*: Absolute time stamping of the information provided
in the track message, in the form of elapsed time since
last mid night, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. This is the time of the track state vector.
    2. The time is reset to zero at every midnight.

I062/080 - Track Status
***********************

*Definition*: Status of a track.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I062/080/MON**

    - 1 bit [``.``]

    - values:

        | 0: Multisensor track
        | 1: Monosensor track

    **I062/080/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: SPI present in the last report received from a sensor capable of decoding this data

    **I062/080/MRH** - *Most Reliable Height*

    - 1 bit [``.``]

    - values:

        | 0: Barometric altitude (Mode C) more reliable
        | 1: Geometric altitude more reliable

    **I062/080/SRC** - *Source of Calculated Track Altitude for I062/130*

    - 3 bits [``...``]

    - values:

        | 0: No source
        | 1: GNSS
        | 2: 3D radar
        | 3: Triangulation
        | 4: Height from coverage
        | 5: Speed look-up table
        | 6: Default height
        | 7: Multilateration

    **I062/080/CNF**

    - 1 bit [``.``]

    - values:

        | 0: Confirmed track
        | 1: Tentative track

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/080/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual track
        | 1: Simulated track

    **I062/080/TSE**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Last message transmitted to the user for the track

    **I062/080/TSB**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: First message transmitted to the user for the track

    **I062/080/FPC**

    - 1 bit [``.``]

    - values:

        | 0: Not flight-plan correlated
        | 1: Flight plan correlated

    **I062/080/AFF**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: ADS-B data inconsistent with other surveillance information

    **I062/080/STP**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Slave Track Promotion

    **I062/080/KOS**

    - 1 bit [``.``]

    - values:

        | 0: Complementary service used
        | 1: Background service used

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/080/AMA**

    - 1 bit [``.``]

    - values:

        | 0: Track not resulting from amalgamation process
        | 1: Track resulting from amalgamation process

    **I062/080/MD4**

    - 2 bits [``..``]

    - values:

        | 0: No Mode 4 interrogation
        | 1: Friendly target
        | 2: Unknown target
        | 3: No reply

    **I062/080/ME**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Military Emergency present in the last report received from a sensor capable of decoding this data

    **I062/080/MI**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Military Identification present in the last report received from a sensor capable of decoding this data

    **I062/080/MD5**

    - 2 bits [``..``]

    - values:

        | 0: No Mode 5 interrogation
        | 1: Friendly target
        | 2: Unknown target
        | 3: No reply

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/080/CST**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received track update is higher than system dependent threshold (coasting)

    **I062/080/PSR**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received PSR track update is higher than system dependent threshold

    **I062/080/SSR**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received SSR track update is higher than system dependent threshold

    **I062/080/MDS**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received Mode S track update is higher than system dependent threshold

    **I062/080/ADS**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Age of the last received ADS-B track update is higher than system dependent threshold

    **I062/080/SUC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Special Used Code (Mode A codes to be defined in the system to mark a track with special interest)

    **I062/080/AAC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Assigned Mode A Code Conflict (same discrete Mode A Code assigned to another track)

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/080/SDS**

    - 2 bits [``..``]

    - values:

        | 0: Combined
        | 1: Co-operative only
        | 2: Non-Cooperative only
        | 3: Not defined

    **I062/080/EMS**

    - 3 bits [``...``]

    - values:

        | 0: No emergency
        | 1: General emergency
        | 2: Lifeguard / medical
        | 3: Minimum fuel
        | 4: No communications
        | 5: Unlawful interference
        | 6: Downed Aircraft
        | 7: Undefined

    **I062/080/PFT**

    - 1 bit [``.``]

    - values:

        | 0: No indication
        | 1: Potential False Track Indication

    **I062/080/FPLT**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Track created / updated with FPL data

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/080/DUPT**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Duplicate Mode 3/A Code

    **I062/080/DUPF**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Duplicate Flight Plan

    **I062/080/DUPM**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Duplicate Flight Plan due to manual correlation

    **I062/080/SFC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Surface target

    **I062/080/IDD**

    - 1 bit [``.``]

    - values:

        | 0: No indication
        | 1: Duplicate Flight-ID

    **I062/080/IEC**

    - 1 bit [``.``]

    - values:

        | 0: Default value
        | 1: Inconsistent Emergency Code

    **I062/080/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Track type and coasting can also be derived from I062/290 System Track Update Ages
    2. If the system supports the technology, default value (0) means that the technology was used to produce the report
    3. If the system does not support the technology, default value is meaningless.
    4. Bits (EMS): other than subfield #11 of data item I062/380, these
       bits allow the SDPS to set the emergency indication as derived from
       other sources than ADS-B (e.g. based on the Mode 3/A code).
    5. Bit 3 (PFT): with this flag an SDPS can indicate that internal processing
       points to the track being potentially false. Details on the internal
       processing are system dependent. In order to improve security on
       targets provided by ADS-B numerous validation functions have been
       developed in the ADS-B ground domain. If any of these validation
       functions show a potentially spoofed target, the PFT bit will be used to
       convey this information to the CWP. If and how this information is
       processed and displayed on the CWP is a local matter and not subject
       to the category 062 specification.
    6. Bit (FPLT): this bit - if set - indicates that the information contained
       in the target report has been updated by flight plan related data
       because no surveillance data was available for the target, or was
       created based on flight plan related data in areas with no
       surveillance.
    7. Bit (DUPT) is set to 1 if the correlation between the target report and a flight
       plan is not possible because the Mode 3/A code stated in the flight plan exists
       more than once in the surveillance data.
    8. Bit (DUPF) - if set to 1 - indicates that for a specific surveillance target more
       than one flight plan exists which makes correlation impossible.
    9. Bit (DUPM) is set to 1 if a target was correlated manually but also a regular
       flight plan exists.
    10. All tracks for which bits 8, 7 or 6 are set to 1 are marked on the CWP.
    11. Bit 5 (SFC) is set to 1 when the SDPS considers the target to be on the Surface
        (the actual meaning is implementation dependent – please refer to chapter 4.8
        above).
    12. Bit 4 (IDD) is set to 1 when the Flight ID is present more than once in the
        surveillance area.
    13. Bit 3 (IEC) is set to 1 when the comparison between various sources has
        revealed an inconsistency in the information contained about emergency codes.
    14. If I062/080 (MRH) indicates "Barometric altitude (Mode C) more
        reliable", and a calculated altitude is transmitted, it shall be transmitted
        using data item I062/135 “Calculated Track Barometric Altitude”.
    15. If I062/080 (MRH) indicates "Geometric altitude more reliable", and a
        calculated altitude is transmitted, it shall be transmitted using data item
        I062/130 “Calculated Track Geometric Altitude”. In this case the source
        for I062/130 is indicated by I062/080 (SRC).
    16. Data Items I062/130, I062/135, and I062/136 may be transmitted in
        parallel whenever the respective information is available. This is
        independent from the value transmitted on I062/080 (MRH).

I062/100 - Calculated Track Position (Cartesian)
************************************************

*Definition*: Calculated position in Cartesian co-ordinates with a resolution of
0.5m, in two's complement form.

*Structure*:

    **I062/100/X** - *X Coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m

    **I062/100/Y** - *Y Coordinate*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m



I062/105 - Calculated Position In WGS-84 Co-ordinates
*****************************************************

*Definition*: Calculated Position in WGS-84 Co-ordinates with a resolution of
:math:`180/2^{25}` degrees.

*Structure*:

    **I062/105/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 25
    - unit: "deg"
    - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I062/105/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 25
    - unit: "deg"
    - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg


Notes:

    - The LSB provides a resolution at least better than 0.6m.

I062/110 - Mode 5 Data Reports and Extended Mode 1 Code
*******************************************************

*Definition*: Mode 5 Data reports and Extended Mode 1 Code.

*Structure*:

Compound item (FX)

    **I062/110/SUM** - *Mode 5 Summary*

        **I062/110/SUM/M5**

        - 1 bit [``.``]

        - values:

            | 0: No Mode 5 interrogation
            | 1: Mode 5 interrogation

        **I062/110/SUM/ID**

        - 1 bit [``.``]

        - values:

            | 0: No authenticated Mode 5 ID reply
            | 1: Authenticated Mode 5 ID reply

        **I062/110/SUM/DA**

        - 1 bit [``.``]

        - values:

            | 0: No authenticated Mode 5 Data reply or Report
            | 1: Authenticated Mode 5 Data reply or Report (i.e any valid Mode 5 reply type other than ID)

        **I062/110/SUM/M1**

        - 1 bit [``.``]

        - values:

            | 0: Mode 1 code not present or not from Mode 5 reply
            | 1: Mode 1 code from Mode 5 reply

        **I062/110/SUM/M2**

        - 1 bit [``.``]

        - values:

            | 0: Mode 2 code not present or not from Mode 5 reply
            | 1: Mode 2 code from Mode 5 reply

        **I062/110/SUM/M3**

        - 1 bit [``.``]

        - values:

            | 0: Mode 3 code not present or not from Mode 5 reply
            | 1: Mode 3 code from Mode 5 reply

        **I062/110/SUM/MC**

        - 1 bit [``.``]

        - values:

            | 0: Mode C altitude code not present or not from Mode 5 reply
            | 1: Mode C altitude from Mode 5 reply

        **I062/110/SUM/X** - *X-pulse from Mode 5 Data Reply or Report*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no authenticated Data reply or Report received
            | 1: X-pulse set to one

    **I062/110/PMN** - *Mode 5 PIN/ National Origin/Mission Code*

        **I062/110/PMN/(spare)**

        - 2 bits [``..``]

        **I062/110/PMN/PIN** - *PIN Code*

        - 14 bits [``..............``]

        - raw value

        **I062/110/PMN/(spare)**

        - 3 bits [``...``]

        **I062/110/PMN/NAT** - *National Origin*

        - 5 bits [``.....``]

        - raw value

        **I062/110/PMN/(spare)**

        - 2 bits [``..``]

        **I062/110/PMN/MIS** - *Mission Code*

        - 6 bits [``......``]

        - raw value

    **I062/110/POS** - *Mode 5 Reported Position*

        **I062/110/POS/LAT** - *Latitude*

        - 24 bits [``........................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 23
        - unit: "deg"
        - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
        - value :math:`>= -90` deg
        - value :math:`<= 90` deg

        **I062/110/POS/LON** - *Longitude*

        - 24 bits [``........................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 23
        - unit: "deg"
        - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
        - value :math:`>= -180` deg
        - value :math:`< 180` deg

    **I062/110/GA** - *Mode 5 GNSS-derived Altitude*

        **I062/110/GA/(spare)**

        - 1 bit [``.``]

        **I062/110/GA/RES** - *Resolution with which the GNSS-derived Altitude (GA) is Reported*

        - 1 bit [``.``]

        - values:

            | 0: GA reported in 100 ft increments
            | 1: GA reported in 25 ft increments

        **I062/110/GA/GA** - *GNSS-derived Altitude of Target, Expressed as Height Above WGS 84 Ellipsoid*

        - 14 bits [``..............``]

        - signed quantity
        - scaling factor: 25
        - fractional bits: 0
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1000` ft

    **I062/110/EM1** - *Extended Mode 1 Code in Octal Representation*

        **I062/110/EM1/(spare)**

        - 4 bits [``....``]

        **I062/110/EM1/EM1** - *Extended Mode 1 Reply in Octal Representation*

        - 12 bits [``............``]

        - Octal string (3-bits per digit)

    **I062/110/TOS** - *Time Offset for POS and GA*

    Time Offset coded as a twos complement number with an LSB of 1/128 s. The time at which the Mode 5 Reported Position (Subfield #3) and Mode 5 GNSS-derived Altitude (Subfield #4) are valid is given by Time of Day (I048/140) plus Time Offset.

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "s"
    - LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s

    **I062/110/XP** - *X Pulse Presence*

        **I062/110/XP/(spare)**

        - 3 bits [``...``]

        **I062/110/XP/X5** - *X-pulse from Mode 5 Data Reply or Report*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no authenticated Data reply or Report received
            | 1: X-pulse set to one (present)

        **I062/110/XP/XC** - *X-pulse from Mode C Reply*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no Mode C reply
            | 1: X-pulse set to one (present)

        **I062/110/XP/X3** - *X-pulse from Mode 3/A Reply*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no Mode 3/A reply
            | 1: X-pulse set to one (present)

        **I062/110/XP/X2** - *X-pulse from Mode 2 Reply*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no Mode 2 reply
            | 1: X-pulse set to one (present)

        **I062/110/XP/X1** - *X-pulse from Mode 1 Reply*

        - 1 bit [``.``]

        - values:

            | 0: X-pulse set to zero or no Mode 1 reply
            | 1: X-pulse set to one (present)


Notes:

    1. The flags M2, M3, MC refer to the contents of data subitems I062/120,
       I062/060 and I062/135 respectively. The flag M1 refers to the contents
       of the Subfield #5 (Extended Mode 1 Code in Octal Representation).
    2. If an authenticated Mode 5 reply is received with the Emergency
       bit set, then the Military Emergency bit (ME) in Data Item I062/080,
       Track Status, shall be set.
    3. If an authenticated Mode 5 reply is received with the Identification
       of Position bit set, then the Special Position Identification
       bit (SPI) in Data Item I062/080, Track Status, shall be set.
    4. The resolution implied by the LSB is better than the resolution
       with which Mode 5 position reports are transmitted from aircraft
       transponders using currently defined formats.
    5. GA is coded as a 14-bit two's complement binary number with
       an LSB of 25 ft. irrespective of the setting of RES.
    6. The minimum value of GA that can be reported is -1000 ft.
    7. If Subfield #1 is present, the M1 bit in Subfield #1 indicates
       whether the Extended Mode 1 Code is from a Mode 5 reply or
       a Mode 1 reply. If Subfield #1 is not present, the Extended
       Mode 1 Code is from a Mode 1 reply.
    8. TOS shall be assumed to be zero if Subfield #6 is not present.

I062/120 - Track Mode 2 Code
****************************

*Definition*: Mode 2 code associated to the track

*Structure*:

    **I062/120/(spare)**

    - 4 bits [``....``]

    **I062/120/MODE2** - *Mode-2 Code in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)



I062/130 - Calculated Track Geometric Altitude
**********************************************

*Definition*: Vertical distance between the target and the projection of its position
on the earth's ellipsoid, as defined by WGS84, in two's complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft"
- LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
- value :math:`>= -1500` ft
- value :math:`<= 150000` ft


Notes:

    1. LSB is required to be less than 10 ft by ICAO
    2. The source of altitude is identified in bits (SRC) of item
       I062/080 Track Status.

I062/135 - Calculated Track Barometric Altitude
***********************************************

*Definition*: Calculated barometric altitude of the track, in two's complement form.

*Structure*:

    **I062/135/QNH**

    - 1 bit [``.``]

    - values:

        | 0: No QNH correction applied
        | 1: QNH correction applied

    **I062/135/CTB** - *Calculated Track Barometric Altitude*

    - 15 bits [``...............``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
    - value :math:`>= -15` FL
    - value :math:`<= 1500` FL


Notes:

    1) ICAO specifies a range between -10 FL and 1267 FL for Mode C

I062/136 - Measured Flight Level
********************************

*Definition*: Last valid and credible flight level used to update the track, in two's
complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 1
- fractional bits: 2
- unit: "FL"
- LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
- value :math:`>= -15` FL
- value :math:`<= 1500` FL


Notes:

    1. The criteria to determine the credibility of the flight level
       are Tracker dependent.
    2. Credible means: within reasonable range of change with respect
       to the previous detection.
    3. ICAO specifies a range between -10 FL and 1267 FL for Mode C.
    4. This item includes the barometric altitude received from ADS-B.

I062/185 - Calculated Track Velocity (Cartesian)
************************************************

*Definition*: Calculated track velocity expressed in Cartesian co-ordinates,in
two's complement form.

*Structure*:

    **I062/185/VX** - *Velocity (X-component)*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8191.75` m/s

    **I062/185/VY** - *Velocity (Y-component)*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s"
    - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s
    - value :math:`>= -8192` m/s
    - value :math:`<= 8191.75` m/s


Notes:

    - The y-axis points to the Geographical North at the location of
      the target.

I062/200 - Mode of Movement
***************************

*Definition*: Calculated Mode of Movement of a target.

*Structure*:

    **I062/200/TRANS** - *Transversal Acceleration*

    - 2 bits [``..``]

    - values:

        | 0: Constant course
        | 1: Right turn
        | 2: Left turn
        | 3: Undetermined

    **I062/200/LONG** - *Longitudinal Acceleration*

    - 2 bits [``..``]

    - values:

        | 0: Constant groundspeed
        | 1: Increasing groundspeed
        | 2: Decreasing groundspeed
        | 3: Undetermined

    **I062/200/VERT** - *Transversal Acceleration*

    - 2 bits [``..``]

    - values:

        | 0: Level
        | 1: Climb
        | 2: Descent
        | 3: Undetermined

    **I062/200/ADF** - *Altitude Discrepancy Flag*

    - 1 bit [``.``]

    - values:

        | 0: No altitude discrepancy
        | 1: Altitude discrepancy

    **I062/200/(spare)**

    - 1 bit [``.``]


Notes:

    - The ADF, if set, indicates that a difference has been detected
      in the altitude information derived from radar as compared to
      other technologies (such as ADS-B).

I062/210 - Calculated Acceleration (Cartesian)
**********************************************

*Definition*: Calculated Acceleration of the target expressed in Cartesian co-ordinates,
in two's complement form.

*Structure*:

    **I062/210/AX**

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2

    **I062/210/AY**

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "m/s2"
    - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2


Notes:

    1. The y-axis points to the Geographical North at the location of the target.
    2. Maximum value means maximum value or above.

I062/220 - Calculated Rate of Climb/Descent
*******************************************

*Definition*: Calculated rate of climb/descent of an aircraft in two's complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft/min"
- LSB = :math:`25 / {2^{2}}` ft/min = :math:`25 / {4}` ft/min :math:`\approx 6.25` ft/min


Notes:

    1. A positive value indicates a climb, whereas a negative value
       indicates a descent.

I062/245 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters.

*Structure*:

    **I062/245/STI**

    - 2 bits [``..``]

    - values:

        | 0: Callsign or registration downlinked from target
        | 1: Callsign not downlinked from target
        | 2: Registration not downlinked from target
        | 3: Invalid

    **I062/245/(spare)**

    - 6 bits [``......``]

    **I062/245/CHR** - *Characters 1-8 (Coded on 6 Bits Each) Defining Target Identification*

    - 48 bits [``................................................``]

    - ICAO string (6-bits per character)


Notes:

    1. For coding, see section 3.1.2.9 of [Ref.3]
    2. As the Callsign of the target can already be transmitted
       (thanks to I062/380 Subfield #2 if downlinked from the
       aircraft or thanks to I062/390 Subfield #2 if the target
       is correlated to a flight plan), and in order to avoid
       confusion at end user's side, this item SHALL not be used.

I062/270 - Target Size and Orientation
**************************************

*Definition*: Target size defined as length and width of the detected target, and orientation.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I062/270/LENGTH** - *Length*

    Length

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/270/ORIENTATION** - *Orientation*

    Length

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 7
    - unit: "deg"
    - LSB = :math:`360 / {2^{7}}` deg = :math:`360 / {128}` deg :math:`\approx 2.8125` deg

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/270/WIDTH** - *Width*

    Length

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "m"
    - LSB = :math:`1` m

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. The orientation gives the direction which the target nose is
       pointing to,relative to the Geographical North.
    2. When the length only is sent, the largest dimension is provided.

I062/290 - System Track Update Ages
***********************************

*Definition*: Ages of the last plot/local track/target report update for each sensor type.

*Structure*:

Compound item (FX)

    **I062/290/TRK** - *Track Age*

    Actual track age since occurence

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/PSR** - *PSR Age*

    Age of the last primary detection used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/SSR** - *SSR Age*

    Age of the last secondary detection used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/MDS** - *Mode S Age*

    Age of the last Mode S detection used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/ADS** - *ADS-C Age*

    Age of the last ADS-C report used to update the track

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 16383.75` s

    **I062/290/ES** - *ADS-B Extended Squitter Age*

    Age of the last 1090 Extended Squitter ADS-B report used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/VDL** - *ADS-B VDL Mode 4 Age*

    Age of the last VDL Mode 4 ADS-B report used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/UAT** - *ADS-B UAT Age*

    Age of the last UAT ADS-B report used to update the track

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/LOP** - *Loop Age*

    Age of the last magnetic loop detection

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/290/MLT** - *Multilateration Age*

    Age of the last MLT detection

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s


Notes:

    1. Except for Track Age, the ages are counted from Data Item I062/070,
       Time Of Track Information, using the following formula:
       Age = Time of track information - Time of last detection used
       to update the track
    2. The time of last detection is derived from monosensor category time of day
    3. If the data has never been received, then the corresponding
       subfield is not sent.
    4. Maximum value means maximum value or above.

I062/295 - Track Data Ages
**************************

*Definition*: Ages of the data provided.

*Structure*:

Compound item (FX)

    **I062/295/MFL** - *Measured Flight Level Age*

    Age of the last valid and credible Mode C code or barometric altitude from ADS-B used to update the track (I062/136).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MD1** - *Mode 1 Age*

    Age of the last valid and credible Mode 1 code used to update the track (I062/110).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MD2** - *Mode 2 Age*

    Age of the last valid and credible Mode 2 code used to update the track (I062/120).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MDA** - *Mode 3/A Age*

    Age of the last valid and credible Mode 3/A code used to update the track (I062/060).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MD4** - *Mode 4 Age*

    Age of the last valid and credible Mode 4 code used to update the track.

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MD5** - *Mode 5 Age*

    Age of the last valid and credible Mode 5 code used to update the track (I062/110).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MHG** - *Magnetic Heading Age*

    Age of the DAP "Magnetic Heading" in item 062/380 (Subfield #3).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/IAS** - *Indicated Airspeed / Mach Nb Age*

    Age of the DAP "Indicated Airspeed/Mach Number" in item 062/380 (Subfield #4).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/TAS** - *True Airspeed Age*

    Age of the DAP "True Airspeed" in item 062/380 (Subfield #5).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/SAL** - *Selected Altitude Age*

    Age of the DAP "Selected Altitude" in item 062/380 (Subfield #6).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/FSS** - *Final State Selected Altitude Age*

    Age of the DAP "Final State Selected Altitude Age" in item 062/380 (Subfield #7).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/TID** - *Trajectory Intent Age*

    Age of the DAP "Trajectory Intent" in item 062/380 (Subfield #8).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/COM** - *Communication/ACAS Capability and Flight Status Age*

    Age of the DAP "Communication/ACAS Capability and Flight Status" in item 062/380 (Subfield #10).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/SAB** - *Status Reported by ADS-B Age*

    Age of the DAP "Status Reported by ADS-B" in item 062/380 (Subfield #11).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/ACS** - *ACAS Resolution Advisory Report Age*

    Age of the DAP "ACAS Resolution Advisory Report" in item 062/380 (Subfield #12).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/BVR** - *Barometric Vertical Rate Age*

    Age of the DAP "Barometric Vertical Rate" in item 062/380 (Subfield #13).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/GVR** - *Geometrical Vertical Rate Age*

    Age of the DAP "Geometrical Vertical Rate" in item 062/380 (Subfield #14).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/RAN** - *Roll Angle Age*

    Age of the DAP "Roll Angle" in item 062/380 (Subfield #15).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/TAR** - *Track Angle Rate Age*

    Age of the DAP "Track Angle Rate" in item 062/380 (Subfield #16).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/TAN** - *Track Angle Age*

    Age of the DAP "Track Angle" in item 062/380 (Subfield #17).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/GSP** - *Ground Speed Age*

    Age of the DAP "Ground Speed" in item 062/380 (Subfield #18).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/VUN** - *Velocity Uncertainty Age*

    Age of the DAP "Velocity Uncertainty" in item 062/380 (Subfield #19).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MET** - *Meteorological Data Age*

    Age of the DAP "Meteorological Data" in item 062/380 (Subfield #20).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/EMC** - *Emitter Category Age*

    Age of the DAP "Emitter Category" in item 062/380 (Subfield #21).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/POS** - *Position Age*

    Age of the DAP "Position" in item 062/380 (Subfield #23).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/GAL** - *Geometric Altitude Age*

    Age of the DAP "Geometric Altitude" in item 062/380 (Subfield #24).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/PUN** - *Position Uncertainty Age*

    Age of the DAP "Position Uncertainty" in item 062/380 (Subfield #25).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MB** - *Mode S MB Data Age*

    Age of the DAP "Mode S MB Data" in item 062/380 (Subfield #22).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/IAR** - *Indicated Airspeed Data Age*

    Age of the DAP "Indicated Airspeed" in item 062/380 (Subfield #26).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/MAC** - *Mach Number Data Age*

    Age of the DAP "Mach Number" in item 062/380 (Subfield #27).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s

    **I062/295/BPS** - *Barometric Pressure Setting Data Age*

    Age of the DAP "Barometric Pressure Setting" in item 062/380 (Subfield #28).

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "s"
    - LSB = :math:`1 / {2^{2}}` s = :math:`1 / {4}` s :math:`\approx 0.25` s
    - value :math:`<= 63.75` s


Notes:

    1. Despite there are now two subfields (#29 and #30) reporting the ages
       of, respectively, the Indicated Airspeed track data and the Mach
       Number track data, the subfield #8 (and so its presence bit , bit-32) is
       kept free in order to prevent a full incompatibility with previous
       releases of ASTERIX Cat. 062 already implemented.
    2. In all the subfields, the age is the time delay since the value was
       measured

I062/300 - Vehicle Fleet Identification
***************************************

*Definition*: Vehicle fleet identification number.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Unknown
    | 1: ATC equipment maintenance
    | 2: Airport maintenance
    | 3: Fire
    | 4: Bird scarer
    | 5: Snow plough
    | 6: Runway sweeper
    | 7: Emergency
    | 8: Police
    | 9: Bus
    | 10: Tug (push/tow)
    | 11: Grass cutter
    | 12: Fuel
    | 13: Baggage
    | 14: Catering
    | 15: Aircraft maintenance
    | 16: Flyco (follow me)



I062/340 - Measured Information
*******************************

*Definition*: All measured data related to the last report used to update the track.
These data are not used for ADS-B.

*Structure*:

Compound item (FX)

    **I062/340/SID** - *Sensor Identification*

        **I062/340/SID/SAC** - *System Area Code*

        - 8 bits [``........``]

        - raw value

        **I062/340/SID/SIC** - *System Identification Code*

        - 8 bits [``........``]

        - raw value

    **I062/340/POS** - *Measured Position*

        **I062/340/POS/RHO** - *Measured Distance*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 8
        - unit: "NM"
        - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM
        - value :math:`<= 256` NM

        **I062/340/POS/THETA** - *Measured Azimuth*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 360
        - fractional bits: 16
        - unit: "deg"
        - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg

    **I062/340/HEIGHT** - *Measured 3-D Height*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 25
    - fractional bits: 0
    - unit: "ft"
    - LSB = :math:`25` ft

    **I062/340/MDC**

        **I062/340/MDC/V** - *Validated*

        - 1 bit [``.``]

        - values:

            | 0: Code validated
            | 1: Code not validated

        **I062/340/MDC/G** - *Garbled*

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Garbled code

        **I062/340/MDC/LMC** - *Last Measured Mode C Code*

        Last Measured Mode C Code, in two's complement form

        - 14 bits [``..............``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "FL"
        - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
        - value :math:`>= -12` FL
        - value :math:`<= 1270` FL

    **I062/340/MDA**

        **I062/340/MDA/V** - *Validated*

        - 1 bit [``.``]

        - values:

            | 0: Code validated
            | 1: Code not validated

        **I062/340/MDA/G** - *Garbled*

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Garbled code

        **I062/340/MDA/L**

        - 1 bit [``.``]

        - values:

            | 0: Mode 3/A code as derived from the reply of the transponder
            | 1: Mode 3/A code as provided by a sensor local tracker

        **I062/340/MDA/(spare)**

        - 1 bit [``.``]

        **I062/340/MDA/MODE3A** - *Mode-3/A Reply in Octal Representation*

        - 12 bits [``............``]

        - Octal string (3-bits per digit)

    **I062/340/TYP**

        **I062/340/TYP/TYP** - *Report Type*

        - 3 bits [``...``]

        - values:

            | 0: No detection
            | 1: Single PSR detection
            | 2: Single SSR detection
            | 3: SSR + PSR detection
            | 4: Single ModeS All-Call
            | 5: Single ModeS Roll-Call
            | 6: ModeS All-Call + PSR
            | 7: ModeS Roll-Call + PSR

        **I062/340/TYP/SIM**

        - 1 bit [``.``]

        - values:

            | 0: Actual target report
            | 1: Simulated target report

        **I062/340/TYP/RAB**

        - 1 bit [``.``]

        - values:

            | 0: Report from target transponder
            | 1: Report from field monitor (item transponder)

        **I062/340/TYP/TST**

        - 1 bit [``.``]

        - values:

            | 0: Real target report
            | 1: Test target report

        **I062/340/TYP/(spare)**

        - 2 bits [``..``]


Notes:

    1. In case of a plot, the measured bias-corrected polar co-ordinates;
    2. In case of a sensor local track, the measured bias-corrected
       polar co-ordinates of the plot associated to the track;
    3. In case of a local track without detection, the extrapolated
       bias-corrected polar co-ordinates.
    4. Smoothed MODE 3/A data (L = 1) will be used in case of absence of
       MODE 3/A code information in the plot or in case of difference
       between plot and sensor local track MODE 3/A code information.

I062/380 - Aircraft Derived Data
********************************

*Definition*: Data derived directly by the aircraft.

*Structure*:

Compound item (FX)

    **I062/380/ADR** - *Target Address*

    - 24 bits [``........................``]

    - raw value

    **I062/380/ID** - *Target Identification*

    Characters 1-8 (coded on 6 bits each) defining a target identification when flight plan is available or the registration marking when no flight plan is available. Coding rules are provided in [3] Section 3.1.2.9.1.2 and Table 3-9"

    - 48 bits [``................................................``]

    - ICAO string (6-bits per character)

    **I062/380/MHG** - *Magnetic Heading*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg

    **I062/380/IAS** - *Indicated Airspeed/Mach No*

        **I062/380/IAS/IM**

        - 1 bit [``.``]

        - values:

            | 0: Air Speed = IAS, LSB (Bit-1) = 2^-14 NM/s
            | 1: Air Speed = Mach, LSB (Bit-1) = 0.001

        **I062/380/IAS/IAS**

        - 15 bits [``...............``]

        * Content of this item depends on the value of item ``380/IAS/IM``.

            * In case of ``380/IAS/IM == 0``:
                - unsigned quantity
                - scaling factor: 1
                - fractional bits: 14
                - unit: "NM/s"
                - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s

            * In case of ``380/IAS/IM == 1``:
                - unsigned quantity
                - scaling factor: 0.001
                - fractional bits: 0
                - unit: "mach"
                - LSB = :math:`0.001` mach


    **I062/380/TAS** - *True Airspeed*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "kt"
    - LSB = :math:`1` kt
    - value :math:`>= 0` kt
    - value :math:`<= 2046` kt

    **I062/380/SAL** - *Selected Altitude*

        **I062/380/SAL/SAS**

        - 1 bit [``.``]

        - values:

            | 0: No source information provided
            | 1: Source information provided

        **I062/380/SAL/SRC**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Aircraft altitude
            | 2: FCU/MCP selected altitude
            | 3: FMS selected altitude

        **I062/380/SAL/ALT** - *Altitude in Two's Complement Form*

        - 13 bits [``.............``]

        - signed quantity
        - scaling factor: 25
        - fractional bits: 0
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1300` ft
        - value :math:`<= 100000` ft

    **I062/380/FSS** - *Final State Selected Altitude*

        **I062/380/FSS/MV** - *Manage Vertical Mode*

        Manage Vertical Mode

        - 1 bit [``.``]

        - values:

            | 0: Not active
            | 1: Active

        **I062/380/FSS/AH** - *Altitude Hold*

        Altitude Hold

        - 1 bit [``.``]

        - values:

            | 0: Not active
            | 1: Active

        **I062/380/FSS/AM** - *Approach Mode*

        Approach Mode

        - 1 bit [``.``]

        - values:

            | 0: Not active
            | 1: Active

        **I062/380/FSS/ALT** - *Altitude in Two's Complement Form*

        - 13 bits [``.............``]

        - signed quantity
        - scaling factor: 25
        - fractional bits: 0
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1300` ft
        - value :math:`<= 100000` ft

    **I062/380/TIS** - *Trajectory Intent Status*

    Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

        **I062/380/TIS/NAV** - *TID Available*

        - 1 bit [``.``]

        - values:

            | 0: Trajectory intent data is available for this aircraft
            | 1: Trajectory intent data is not available for this aircraft

        **I062/380/TIS/NVB** - *TID Valid*

        - 1 bit [``.``]

        - values:

            | 0: Trajectory intent data is valid
            | 1: Trajectory intent data is not valid

        **I062/380/TIS/(spare)**

        - 5 bits [``.....``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I062/380/TID** - *Trajectory Intent Data*

    Repetitive item, repetition factor 8 bits.

            **I062/380/TID/TCA** - *TCP Number Availability*

            - 1 bit [``.``]

            - values:

                | 0: TCP number available
                | 1: TCP number not available

            **I062/380/TID/NC** - *TCP Compliance*

            - 1 bit [``.``]

            - values:

                | 0: TCP compliance
                | 1: TCP non-compliance

            **I062/380/TID/TCPN** - *Trajectory Change Point Number*

            Trajectory change point number

            - 6 bits [``......``]

            - raw value

            **I062/380/TID/ALT** - *Altitude in Two's Complement Form*

            - 16 bits [``................``]

            - signed quantity
            - scaling factor: 10
            - fractional bits: 0
            - unit: "ft"
            - LSB = :math:`10` ft
            - value :math:`>= -1500` ft
            - value :math:`<= 150000` ft

            **I062/380/TID/LAT** - *Latitude in WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - scaling factor: 180
            - fractional bits: 23
            - unit: "deg"
            - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
            - value :math:`>= -90` deg
            - value :math:`<= 90` deg

            **I062/380/TID/LON** - *Longitude in WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - scaling factor: 180
            - fractional bits: 23
            - unit: "deg"
            - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
            - value :math:`>= -180` deg
            - value :math:`< 180` deg

            **I062/380/TID/PT** - *Point Type*

            - 4 bits [``....``]

            - values:

                | 0: Unknown
                | 1: Fly by waypoint (LT)
                | 2: Fly over waypoint (LT)
                | 3: Hold pattern (LT)
                | 4: Procedure hold (LT)
                | 5: Procedure turn (LT)
                | 6: RF leg (LT)
                | 7: Top of climb (VT)
                | 8: Top of descent (VT)
                | 9: Start of level (VT)
                | 10: Cross-over altitude (VT)
                | 11: Transition altitude (VT)

            **I062/380/TID/TD** - *Turn Direction*

            - 2 bits [``..``]

            - values:

                | 0: N/A
                | 1: Turn right
                | 2: Turn left
                | 3: No turn

            **I062/380/TID/TRA** - *Turn Radius Availability*

            Turn Radius Availability

            - 1 bit [``.``]

            - values:

                | 0: TTR not available
                | 1: TTR available

            **I062/380/TID/TOA** - *TOV Available*

            - 1 bit [``.``]

            - values:

                | 0: TOV available
                | 1: TOV not available

            **I062/380/TID/TOV** - *Time Over Point*

            - 24 bits [``........................``]

            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 0
            - unit: "s"
            - LSB = :math:`1` s

            **I062/380/TID/TTR** - *TCP Turn Radius*

            - 16 bits [``................``]

            - unsigned quantity
            - scaling factor: 0.01
            - fractional bits: 0
            - unit: "Nm"
            - LSB = :math:`0.01` Nm
            - value :math:`>= 0` Nm
            - value :math:`<= 655.35` Nm

    **I062/380/COM** - *Communications/ACAS Capability and Flight Status*

        **I062/380/COM/COM** - *Communications Capability of the Transponder*

        - 3 bits [``...``]

        - values:

            | 0: No communications capability (surveillance only)
            | 1: Comm. A and Comm. B capability
            | 2: Comm. A, Comm. B and Uplink ELM
            | 3: Comm. A, Comm. B, Uplink ELM and Downlink ELM
            | 4: Level 5 Transponder capability
            | 5: Not assigned
            | 6: Not assigned
            | 7: Not assigned

        **I062/380/COM/STAT** - *Flight Status*

        - 3 bits [``...``]

        - values:

            | 0: No alert, no SPI, aircraft airborne
            | 1: No alert, no SPI, aircraft on ground
            | 2: Alert, no SPI, aircraft airborne
            | 3: Alert, no SPI, aircraft on ground
            | 4: Alert, SPI, aircraft airborne or on ground
            | 5: No alert, SPI, aircraft airborne or on ground
            | 6: Not defined
            | 7: Unknown or not yet extracted

        **I062/380/COM/(spare)**

        - 2 bits [``..``]

        **I062/380/COM/SSC** - *Specific Service Capability*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I062/380/COM/ARC** - *Altitude Reporting Capability*

        - 1 bit [``.``]

        - values:

            | 0: 100 ft resolution
            | 1: 25 ft resolution

        **I062/380/COM/AIC** - *Aircraft Identification Capability*

        - 1 bit [``.``]

        - values:

            | 0: No
            | 1: Yes

        **I062/380/COM/B1A** - *BDS 1,0 Bit 16*

        - 1 bit [``.``]

        - raw value

        **I062/380/COM/B1B** - *BDS BDS 1,0 Bits 37/40*

        - 4 bits [``....``]

        - raw value

    **I062/380/SAB** - *Status Reported by ADS-B*

        **I062/380/SAB/AC** - *ACAS Status*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: ACAS not operational
            | 2: ACAS operational
            | 3: Invalid

        **I062/380/SAB/MN** - *Multiple Navigational Aids Status*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Multiple navigational aids not operating
            | 2: Multiple navigational aids operating
            | 3: Invalid

        **I062/380/SAB/DC** - *Differential Correction Status*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Differential correction
            | 2: No differential correction
            | 3: Invalid

        **I062/380/SAB/GBS** - *Ground Bit Set*

        - 1 bit [``.``]

        - values:

            | 0: Transponder ground bit not set or unknown
            | 1: Transponder Ground Bit set

        **I062/380/SAB/(spare)**

        - 6 bits [``......``]

        **I062/380/SAB/STAT** - *Flight Status*

        - 3 bits [``...``]

        - values:

            | 0: No emergency
            | 1: General emergency
            | 2: Lifeguard / medical
            | 3: Minimum fuel
            | 4: No communications
            | 5: Unlawful interference
            | 6: Downed Aircraft
            | 7: Unknown

    **I062/380/ACS** - *ACAS Resolution Advisory Report*

    Currently active Resolution Advisory (RA), if any, generated by the ACAS associated with the transponder transmitting the report and threat identity data. (MB Data) 56-bit message conveying Mode S Comm B message data of BDS Register 3,0

    - 56 bits [``........................................................``]

    - BDS register 30

    **I062/380/BVR** - *Barometric Vertical Rate*

    Barometric Vertical Rate in two's complement form

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "ft/min"
    - LSB = :math:`25 / {2^{2}}` ft/min = :math:`25 / {4}` ft/min :math:`\approx 6.25` ft/min

    **I062/380/GVR** - *Geometric Vertical Rate*

    Geometric Vertical Rate in two's complement form

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "ft/min"
    - LSB = :math:`25 / {2^{2}}` ft/min = :math:`25 / {4}` ft/min :math:`\approx 6.25` ft/min

    **I062/380/RAN** - *Roll Angle*

    Roll Angle in two's complement form

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 0.01
    - fractional bits: 0
    - unit: "deg"
    - LSB = :math:`0.01` deg
    - value :math:`>= -180` deg
    - value :math:`<= 180` deg

    **I062/380/TAR** - *Track Angle Rate*

        **I062/380/TAR/TI** - *Turn Indicator*

        - 2 bits [``..``]

        - values:

            | 0: Not available
            | 1: Left
            | 2: Right
            | 3: Straight

        **I062/380/TAR/(spare)**

        - 6 bits [``......``]

        **I062/380/TAR/ROT** - *Rate of Turn in Two's Complement Form*

        - 7 bits [``.......``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "deg/s"
        - LSB = :math:`1 / {2^{2}}` deg/s = :math:`1 / {4}` deg/s :math:`\approx 0.25` deg/s
        - value :math:`>= -15` deg/s
        - value :math:`<= 15` deg/s

        **I062/380/TAR/(spare)**

        - 1 bit [``.``]

    **I062/380/TAN** - *Track Angle*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg

    **I062/380/GS** - *Ground Speed*

    Ground Speed in Two's Complement Form Referenced to WGS84

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 14
    - unit: "NM/s"
    - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s
    - value :math:`>= -2` NM/s
    - value :math:`< 2` NM/s

    **I062/380/VUN** - *Velocity Uncertainty*

    - 8 bits [``........``]

    - raw value

    **I062/380/MET** - *Meteorological Data*

        **I062/380/MET/WS** - *Wind Speed Valid Flag*

        - 1 bit [``.``]

        - values:

            | 0: Not valid Wind Speed
            | 1: Valid Wind Speed

        **I062/380/MET/WD** - *Wind Direction Valid Flag*

        - 1 bit [``.``]

        - values:

            | 0: Not valid Wind Direction
            | 1: Valid Wind Direction

        **I062/380/MET/TMP** - *Temperature Valid Flag*

        - 1 bit [``.``]

        - values:

            | 0: Not valid Temperature
            | 1: Valid Temperature

        **I062/380/MET/TRB** - *Turbulence Valid Flag*

        - 1 bit [``.``]

        - values:

            | 0: Not valid Turbulence
            | 1: Valid Turbulence

        **I062/380/MET/(spare)**

        - 4 bits [``....``]

        **I062/380/MET/WSD** - *Wind Speed*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 0
        - unit: "kt"
        - LSB = :math:`1` kt
        - value :math:`>= 0` kt
        - value :math:`<= 300` kt

        **I062/380/MET/WDD** - *Wind Direction*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 0
        - unit: "deg"
        - LSB = :math:`1` deg
        - value :math:`>= 1` deg
        - value :math:`<= 360` deg

        **I062/380/MET/TMPD** - *Temperature in Degrees Celsius*

        - 16 bits [``................``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "degC"
        - LSB = :math:`1 / {2^{2}}` degC = :math:`1 / {4}` degC :math:`\approx 0.25` degC
        - value :math:`>= -100` degC
        - value :math:`<= 100` degC

        **I062/380/MET/TRBD** - *Turbulence*

        - 8 bits [``........``]

        - unsigned integer
        - value :math:`>= 0`
        - value :math:`<= 15`

    **I062/380/EMC** - *Emitter Category*

    - 8 bits [``........``]

    - values:

        | 1: Light aircraft =< 7000 kg
        | 2: Reserved
        | 3: 7000 kg < medium aircraft < 136000 kg
        | 4: Reserved
        | 5: 136000 kg <= heavy aircraft
        | 6: Highly manoeuvrable (5g acceleration capability) and high speed (>400 knots cruise)
        | 7: Reserved
        | 8: Reserved
        | 9: Reserved
        | 10: Rotocraft
        | 11: Glider / sailplane
        | 12: Lighter-than-air
        | 13: Unmanned aerial vehicle
        | 14: Space / transatmospheric vehicle
        | 15: Ultralight / handglider / paraglider
        | 16: Parachutist / skydiver
        | 17: Reserved
        | 18: Reserved
        | 19: Reserved
        | 20: Surface emergency vehicle
        | 21: Surface service vehicle
        | 22: Fixed ground or tethered obstruction
        | 23: Reserved
        | 24: Reserved

    **I062/380/POS** - *Position*

        **I062/380/POS/LAT** - *Latitude in WGS.84 in Two's Complement Form*

        - 24 bits [``........................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 23
        - unit: "deg"
        - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
        - value :math:`>= -90` deg
        - value :math:`<= 90` deg

        **I062/380/POS/LON** - *Longitude in WGS.84 in Two's Complement Form*

        - 24 bits [``........................``]

        - signed quantity
        - scaling factor: 180
        - fractional bits: 23
        - unit: "deg"
        - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
        - value :math:`>= -180` deg
        - value :math:`< 180` deg

        remark
            This corresponds to a resolution of at least 2.4 meters.

    **I062/380/GAL** - *Geometric Altitude*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "ft"
    - LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
    - value :math:`>= -1500` ft
    - value :math:`<= 150000` ft

    **I062/380/PUN** - *Position Uncertainty*

        **I062/380/PUN/(spare)**

        - 4 bits [``....``]

        **I062/380/PUN/PUN** - *Position Uncertainty*

        - 4 bits [``....``]

        - raw value

    **I062/380/MB** - *MODE S MB DATA*

    Repetitive item, repetition factor 8 bits.

        - 64 bits [``................................................................``]

        - BDS register with address

    **I062/380/IAR** - *Indicated Airspeed*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "kt"
    - LSB = :math:`1` kt
    - value :math:`>= 0` kt
    - value :math:`<= 1100` kt

    **I062/380/MAC** - *Mach Number*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 0.008
    - fractional bits: 0
    - unit: "Mach"
    - LSB = :math:`0.008` Mach
    - value :math:`>= 0` Mach
    - value :math:`<= 4.096` Mach

    **I062/380/BPS** - *Barometric Pressure Setting (derived from Mode S BDS 4,0)*

        **I062/380/BPS/(spare)**

        - 4 bits [``....``]

        **I062/380/BPS/BPS**

        - 12 bits [``............``]

        - unsigned quantity
        - scaling factor: 0.1
        - fractional bits: 0
        - unit: "mb"
        - LSB = :math:`0.1` mb
        - value :math:`>= 0` mb
        - value :math:`<= 409.5` mb


Notes:

    1. NC is set to one when the aircraft will not fly the path described
       by the TCP data.
    2. TCP numbers start from zero.
    3. LT = Lateral Type
    4. VT = Vertical Type
    5. TOV gives the estimated time before reaching the point. It is
       defined as the absolute time from midnight.
    6. TOV is meaningful only if TOA is set to 0
    7. Refer to ICAO Draft SARPs for ACAS for detailed explanations.
    8. A positive value represents a right turn, whereas a negative value
       represents a left turn.
    9. Value 15 means 15 degrees/s or above.
    10. Velocity uncertainty category of the least accurate velocity component
    11. Positive longitude indicates East. Positive latitude indicates North.
    12. LSB is required to be thinner than 10 ft by ICAO
    13. Only DAPs that can not be encoded into other subfields of this item
        should be sent using subfield #25
    14. BPS is the barometric pressure setting of the aircraft minus 800 mb.

I062/390 - Flight Plan Related Data
***********************************

*Definition*: All flight plan related information, provided by ground-based systems.

*Structure*:

Compound item (FX)

    **I062/390/TAG** - *FPPS Identification Tag*

        **I062/390/TAG/SAC** - *System Area Code*

        - 8 bits [``........``]

        - raw value

        **I062/390/TAG/SIC** - *System Identification Code*

        - 8 bits [``........``]

        - raw value

    **I062/390/CS** - *Callsign*

    - 56 bits [``........................................................``]

    - Ascii string (8-bits per character)

    **I062/390/IFI** - *IFPS_FLIGHT_ID*

        **I062/390/IFI/TYP**

        - 2 bits [``..``]

        - values:

            | 0: Plan Number
            | 1: Unit 1 internal flight number
            | 2: Unit 2 internal flight number
            | 3: Unit 3 internal flight number

        **I062/390/IFI/(spare)**

        - 3 bits [``...``]

        **I062/390/IFI/NBR** - *Number from 0 to 99 999 999*

        - 27 bits [``...........................``]

        - unsigned integer
        - value :math:`>= 0`
        - value :math:`<= 99999999`

    **I062/390/FCT** - *Flight Category*

        **I062/390/FCT/GATOAT**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: General Air Traffic
            | 2: Operational Air Traffic
            | 3: Not applicable

        **I062/390/FCT/FR1FR2**

        - 2 bits [``..``]

        - values:

            | 0: Instrument Flight Rules
            | 1: Visual Flight Rules
            | 2: Not applicable
            | 3: Controlled Visual Flight Rules

        **I062/390/FCT/RVSM**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Approved
            | 2: Exempt
            | 3: Not Approved

        **I062/390/FCT/HPR**

        - 1 bit [``.``]

        - values:

            | 0: Normal Priority Flight
            | 1: High Priority Flight

        **I062/390/FCT/(spare)**

        - 1 bit [``.``]

    **I062/390/TAC** - *Type of Aircraft*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I062/390/WTC** - *Wake Turbulence Category*

    - 8 bits [``........``]

    - Ascii string (8-bits per character)

    **I062/390/DEP** - *Departure Airport*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I062/390/DST** - *Destination Airport*

    - 32 bits [``................................``]

    - Ascii string (8-bits per character)

    **I062/390/RDS** - *Runway Designation*

        **I062/390/RDS/NU1** - *First Number*

        - 8 bits [``........``]

        - raw value

        **I062/390/RDS/NU2** - *Second Number*

        - 8 bits [``........``]

        - raw value

        **I062/390/RDS/LTR** - *Letter*

        - 8 bits [``........``]

        - Ascii string (8-bits per character)

    **I062/390/CFL** - *Current Cleared Flight Level*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL

    **I062/390/CTL** - *Current Control Position*

        **I062/390/CTL/CENTRE** - *8-bit Group Identification Code*

        - 8 bits [``........``]

        - raw value

        **I062/390/CTL/POSITION** - *8-bit Control Position Identification Code*

        - 8 bits [``........``]

        - raw value

    **I062/390/TOD** - *Time of Departure / Arrival*

    Repetitive item, repetition factor 8 bits.

            **I062/390/TOD/TYP**

            - 5 bits [``.....``]

            - values:

                | 0: Scheduled off-block time
                | 1: Estimated off-block time
                | 2: Estimated take-off time
                | 3: Actual off-block time
                | 4: Predicted time at runway hold
                | 5: Actual time at runway hold
                | 6: Actual line-up time
                | 7: Actual take-off time
                | 8: Estimated time of arrival
                | 9: Predicted landing time
                | 10: Actual landing time
                | 11: Actual time off runway
                | 12: Predicted time to gate
                | 13: Actual on-block time

            **I062/390/TOD/DAY**

            - 2 bits [``..``]

            - values:

                | 0: Today
                | 1: Yesterday
                | 2: Tomorrow
                | 3: Invalid

            **I062/390/TOD/(spare)**

            - 4 bits [``....``]

            **I062/390/TOD/HOR** - *Hours*

            - 5 bits [``.....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 23`

            **I062/390/TOD/(spare)**

            - 2 bits [``..``]

            **I062/390/TOD/MIN** - *Minutes*

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

            **I062/390/TOD/AVS** - *Seconds Available Flag*

            - 1 bit [``.``]

            - values:

                | 0: Seconds available
                | 1: Seconds not available

            **I062/390/TOD/(spare)**

            - 1 bit [``.``]

            **I062/390/TOD/SEC** - *Seconds*

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

    **I062/390/AST** - *Aircraft Stand*

    - 48 bits [``................................................``]

    - Ascii string (8-bits per character)

    **I062/390/STS** - *Stand Status*

        **I062/390/STS/EMP**

        - 2 bits [``..``]

        - values:

            | 0: Empty
            | 1: Occupied
            | 2: Unknown
            | 3: Invalid

        **I062/390/STS/AVL**

        - 2 bits [``..``]

        - values:

            | 0: Available
            | 1: Not available
            | 2: Unknown
            | 3: Invalid

        **I062/390/STS/(spare)**

        - 4 bits [``....``]

    **I062/390/STD** - *Standard Instrument Departure*

    - 56 bits [``........................................................``]

    - Ascii string (8-bits per character)

    **I062/390/STA** - *Standard Instrument Arrival*

    - 56 bits [``........................................................``]

    - Ascii string (8-bits per character)

    **I062/390/PEM** - *Pre-Emergency Mode 3/A*

        **I062/390/PEM/(spare)**

        - 3 bits [``...``]

        **I062/390/PEM/VA**

        - 1 bit [``.``]

        - values:

            | 0: No valid Mode 3/A available
            | 1: Valid Mode 3/A available

        **I062/390/PEM/MODE3A** - *Mode-3/A Reply in Octal Representation*

        - 12 bits [``............``]

        - Octal string (3-bits per digit)

    **I062/390/PEC** - *Pre-Emergency Callsign*

    - 56 bits [``........................................................``]

    - Ascii string (8-bits per character)


Notes:

    1. The up-to-date list of SACs is published on the Eurocontrol Web Site
       (http://www.eurocontrol.int).
    2. Each one of the seven Octets contains an ASCII Character.
       TheCallsign is always left adjusted. It contains up to seven
       upper-case alphanumeric characters, the remaining character
       positions (if any)are padded with space characters.
    3. Each one of the four Octets composing the type of an aircraft
       contains an ASCII Character (upper-case alphanumeric characters
       with trailing spaces).
    4. The types of aircraft are defined in [Ref.4]
    5. Each one of the four Octets composing the name of an airport
       contains an ASCII Character (upper case alphabetic).
    6. The Airport Names are indicated in the ICAO Location Indicators book.
    7. Each one of the four Octets composing the name of an airport
       contains an ASCII Character (upper case alphabetic).
    8. The Airport Names are indicated in the ICAO Location Indicators book.
    9. NU1, NU2 and LTR each contain an ASCII character
    10. For details refer to.[5] Section 5
    11. The centre and the control position identification codes have to be
        defined between communication partners.
    12. Estimated times are derived from flight plan systems. Predicted
        times are derived by the fusion system, based on surveillance
        data. For definitions, see [Ref.4]
    13. Each one of the six Octets contains an ASCII Character. The Aircraft
        Stand identification is always left adjusted. It contains up
        to six upper-case alphanumeric characters, the remaining character
        positions (if any) are padded with space characters.
    14. Each one of the seven Octets contains an ASCII Character.
        The SID is always left adjusted. It contains up to seven
        alphanumeric characters, the remaining character positions
        (if any) are padded with space characters.
    15. Each one of the seven Octets contains an ASCII Character.
        The STAR is always left adjusted. It contains up to seven
        alphanumeric characters, the remaining character positions
        (if any) are padded with space characters.
    16. This subfield is used only when the aircraft is transmitting
        an emergency Mode 3/A code
    17. If VA = 0, the content of bits 12/1 is meaningless
    18. Each one of the seven Octets contains an ASCII Character.
        The Callsign is always left adjusted. It contains up to seven
        upper-case alphanumeric characters, the remaining character
        positions (if any) are padded with space characters
    19. This subfield is used only when an emergency Mode 3/A is associated
        with the track (I062/390 Subfield #17)

I062/500 - Estimated Accuracies
*******************************

*Definition*: Overview of all important accuracies.

*Structure*:

Compound item (FX)

    **I062/500/APC** - *Estimated Accuracy Of Track Position (Cartesian)*

        **I062/500/APC/X** - *APC (X-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 1
        - unit: "m"
        - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m

        **I062/500/APC/Y** - *APC (Y-Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 1
        - unit: "m"
        - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m

    **I062/500/COV** - *XY Covariance Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "m"
    - LSB = :math:`1 / {2^{1}}` m = :math:`1 / {2}` m :math:`\approx 0.5` m

    **I062/500/APW** - *Estimated Accuracy Of Track Position (WGS-84)*

        **I062/500/APW/LAT** - *APW (Latitude Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 180
        - fractional bits: 25
        - unit: "deg"
        - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg

        **I062/500/APW/LON** - *APW (Longitude Component)*

        - 16 bits [``................``]

        - unsigned quantity
        - scaling factor: 180
        - fractional bits: 25
        - unit: "deg"
        - LSB = :math:`180 / {2^{25}}` deg = :math:`180 / {33554432}` deg :math:`\approx 5.364418029785156e-06` deg

    **I062/500/AGA** - *Estimated Accuracy Of Calculated Track Geometric Altitude*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "ft"
    - LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft

    **I062/500/ABA** - *Estimated Accuracy Of Calculated Track Barometric Altitude*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL

    **I062/500/ATV** - *Estimated Accuracy Of Track Velocity (Cartesian)*

        **I062/500/ATV/X** - *ATV (X-Component)*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m/s"
        - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s

        **I062/500/ATV/Y** - *ATV (Y-Component)*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m/s"
        - LSB = :math:`1 / {2^{2}}` m/s = :math:`1 / {4}` m/s :math:`\approx 0.25` m/s

    **I062/500/AA** - *Estimated Accuracy Of Acceleration (Cartesian)*

        **I062/500/AA/X** - *AA (X-Component)*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m/s2"
        - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2

        **I062/500/AA/Y** - *AA (Y-Component)*

        - 8 bits [``........``]

        - unsigned quantity
        - scaling factor: 1
        - fractional bits: 2
        - unit: "m/s2"
        - LSB = :math:`1 / {2^{2}}` m/s2 = :math:`1 / {4}` m/s2 :math:`\approx 0.25` m/s2

    **I062/500/ARC** - *Estimated Accuracy Of Rate Of Climb/Descent*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "ft/min"
    - LSB = :math:`25 / {2^{2}}` ft/min = :math:`25 / {4}` ft/min :math:`\approx 6.25` ft/min


Notes:

    1. Maximum value means maximum value or above.
    2. XY covariance component = sign {Cov(X,Y)} * sqrt {abs [Cov (X,Y)]}
    3. The maximum value for the (unsigned) XY covariance component is 16.383 km
    4. Maximum value means maximum value or above.
    5. Maximum value means maximum value or above.
    6. Maximum value means maximum value or above.
    7. Maximum value means maximum value or above.
    8. Maximum value means maximum value or above.
    9. Maximum value means maximum value or above.

I062/510 - Composed Track Number
********************************

*Definition*: Identification of a system track.

*Structure*:

Extended item with first part ``24 bits`` long and optional ``24 bits`` extends.

    **I062/510/MIDENT** - *Master System Unit Identification*

    - 8 bits [``........``]

    - raw value

    **I062/510/MTRACK** - *Master System Track Number*

    - 15 bits [``...............``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I062/510/SIDENT** - *Slave System Unit Identification*

    - 8 bits [``........``]

    - raw value

    **I062/510/STRACK** - *Slave System Track Number*

    - 15 bits [``...............``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    - The composed track number is used by co-operating units to uniquely
      identify a track. It consists of the unit identifier and system
      track number for each unit involved in the co-operation. The first
      unit identification identifies the unit that is responsible for the
      track amalgamation.

I062/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I062/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 062
=========================================
- (1) ``I062/010`` - Data Source Identifier
- (2) ``(spare)``
- (3) ``I062/015`` - Service Identification
- (4) ``I062/070`` - Time Of Track Information
- (5) ``I062/105`` - Calculated Position In WGS-84 Co-ordinates
- (6) ``I062/100`` - Calculated Track Position (Cartesian)
- (7) ``I062/185`` - Calculated Track Velocity (Cartesian)
- ``(FX)`` - Field extension indicator
- (8) ``I062/210`` - Calculated Acceleration (Cartesian)
- (9) ``I062/060`` - Track Mode 3/A Code
- (10) ``I062/245`` - Target Identification
- (11) ``I062/380`` - Aircraft Derived Data
- (12) ``I062/040`` - Track Number
- (13) ``I062/080`` - Track Status
- (14) ``I062/290`` - System Track Update Ages
- ``(FX)`` - Field extension indicator
- (15) ``I062/200`` - Mode of Movement
- (16) ``I062/295`` - Track Data Ages
- (17) ``I062/136`` - Measured Flight Level
- (18) ``I062/130`` - Calculated Track Geometric Altitude
- (19) ``I062/135`` - Calculated Track Barometric Altitude
- (20) ``I062/220`` - Calculated Rate of Climb/Descent
- (21) ``I062/390`` - Flight Plan Related Data
- ``(FX)`` - Field extension indicator
- (22) ``I062/270`` - Target Size and Orientation
- (23) ``I062/300`` - Vehicle Fleet Identification
- (24) ``I062/110`` - Mode 5 Data Reports and Extended Mode 1 Code
- (25) ``I062/120`` - Track Mode 2 Code
- (26) ``I062/510`` - Composed Track Number
- (27) ``I062/500`` - Estimated Accuracies
- (28) ``I062/340`` - Measured Information
- ``(FX)`` - Field extension indicator
- (29) ``(spare)``
- (30) ``(spare)``
- (31) ``(spare)``
- (32) ``(spare)``
- (33) ``(spare)``
- (34) ``I062/RE`` - Reserved Expansion Field
- (35) ``I062/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

