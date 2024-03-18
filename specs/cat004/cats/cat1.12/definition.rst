Asterix category 004 - Safety Net Messages
==========================================
**category**: 004

**edition**: 1.12

**date**: 2020-10-28

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I004/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the messages
at the receiver side by further defining the type of transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Alive Message (AM)
    | 2: Route Adherence Monitor Longitudinal Deviation (RAMLD)
    | 3: Route Adherence Monitor Heading Deviation (RAMHD)
    | 4: Minimum Safe Altitude Warning (MSAW)
    | 5: Area Proximity Warning (APW)
    | 6: Clearance Level Adherence Monitor (CLAM)
    | 7: Short Term Conflict Alert (STCA)
    | 8: Approach Path Monitor (APM)
    | 9: RIMCAS Arrival / Landing Monitor (ALM)
    | 10: RIMCAS Arrival / Departure Wrong Runway Alert (WRA)
    | 11: RIMCAS Arrival / Departure Opposite Traffic Alert (OTA)
    | 12: RIMCAS Departure Monitor (RDM)
    | 13: RIMCAS Runway / Taxiway Crossing Monitor (RCM)
    | 14: RIMCAS Taxiway Separation Monitor (TSM)
    | 15: RIMCAS Unauthorized Taxiway Movement Monitor(UTMM)
    | 16: RIMCAS Stop Bar Overrun Alert (SBOA)
    | 17: End Of Conflict (EOC)
    | 18: ACAS Resolution Advisory (ACASRA)
    | 19: Near Term Conflict Alert (NTCA)
    | 20: Downlinked Barometric Pressure Setting Monitor (DBPSM)
    | 21: Speed Adherence Monitor (SAM)
    | 22: Outside Controlled Airspace Tool (OCAT)
    | 23: Vertical Conflict Detection (VCD)
    | 24: Vertical Rate Adherence Monitor (VRAM)
    | 25: Cleared Heading Adherence Monitor (CHAM)
    | 26: Downlinked Selected Altitude Monitor (DSAM)
    | 27: Holding Adherence Monitor (HAM)
    | 28: Vertical Path Monitor (VPM)
    | 29: RIMCAS Taxiway Traffic Alert (TTA)
    | 30: RIMCAS Arrival/Departure Close Runway Alert (CRA)
    | 31: RIMCAS Arrival/Departure Aircraft Separation Monitor (ASM)
    | 32: RIMCAS ILS Area Violation Monitor (IAVM)
    | 33: Final Target Distance Indicator (FTD)
    | 34: Initial Target Distance Indicator (ITD)
    | 35: Wake Vortex Indicator Infringement Alert (IIA)
    | 36: Sequence Warning (SQW)
    | 37: Catch Up Warning (CUW)
    | 38: Conflicting ATC Clearances (CATC)
    | 39: No ATC Clearance (NOCLR)
    | 40: Aircraft Not Moving despite ATC Clearance (NOMOV)
    | 41: Aircraft leaving/entering the aerodrome area without proper handover (NOH)
    | 42: Wrong Runway or Taxiway Type (WRTY)
    | 43: Stand Occupied (STOCC)
    | 44: Ongoing Alert (ONGOING)
    | 97: Lost Track Warning (LTW)
    | 98: Holding Volume Infringement (HVI)
    | 99: Airspace Infringement Warning (AIW)

Notes:

    1. In applications where transactions of various types are exchanged,
       the Message Type Data Item facilitates the proper message handling
       at the receiver side.
    2. All Message Type values are reserved for common standard use.
    3. Message Types 33 to 37 have been designed for applications supporting
       the ATCO in the optimisation of separation during final approach.
       They provide information required to indicate to the ATCO the closest
       possible distance of a following aircraft in relation to a leading
       aircraft. This allows to make optimum use of the available runway capacity.
    4. The list of items present for the 31 types of messages is defined
       in the following 4 tables.
       M stands for mandatory, O for optional, X for never present. ::

        item 001 002 003 004 005 006 007 008
        I004/000 M M M M M M M M
        I004/010 M M M M M M M M
        I004/015 O O O O O O O O
        I004/020 M M M M M M M M
        I004/030 X M M M M M M M
        I004/035 X X X X X X M X
        I004/040 X M M M M M M M
        I004/045 X O O O O O O O
        I004/060 M X X X X X X X
        I004/070 X X X O O X O X
        I004/074 X M X X X X X X
        I004/075 X X M X X X X M
        I004/076 X X X X X O X O
        I004/100 X X X X M X X O
        I004/110 X O O O O O O O
        I004/120 X X X M M X M X
        I004/170 X O O O O O O O
        I004/171 X X X X X X O X
        I004/RE O O O O O O O O

        item 009 010 011 012 013 014 015 016
        I004/000 M M M M M M M M
        I004/010 M M M M M M M M
        I004/015 O O O O O O O O
        I004/020 M M M M M M M M
        I004/030 M M M M M M M M
        I004/035 M X M M M M O X
        I004/040 M M M M M M M M
        I004/045 O O O O O O O O
        I004/060 X X X X X X X X
        I004/070 O X O O O O O X
        I004/074 X X X X X X X X
        I004/075 X X X X X X X X
        I004/076 X X X X X X X X
        I004/100 M M M M M M M M
        I004/110 O O O O O O O O
        I004/120 M M M M M O O O
        I004/170 O O O O O O O O
        I004/171 O X O O O O O X
        I004/RE O O O O O O O O

        item 017 018 019 020 021 022 023 024
        I004/000 M M M M M M M M
        I004/010 M M M M M M M M
        I004/015 O O O O O O O O
        I004/020 M M M M M M M M
        I004/030 O X M M M M M M
        I004/035 O X M X X X M X
        I004/040 M M M M M M M M
        I004/045 O O O O O O O O
        I004/060 X X X X X X X X
        I004/070 X O O X X O O X
        I004/074 X X X X X X X X
        I004/075 X X X X X X X X
        I004/076 X X X X X X X O
        I004/100 X X X O O M M O
        I004/110 X X O O O O O O
        I004/120 X X O M O M M O
        I004/170 X M O O O O O O
        I004/171 X O O X X X O X
        I004/RE O M O O O O O O

        item 025 026 027 028 029 030 031 032
        I004/000 M M M M M M M M
        I004/010 M M M M M M M M
        I004/015 O O O O O O O O
        I004/020 M M M M M M M M
        I004/030 M M M M M M M M
        I004/035 X X X X O O M O
        I004/040 M M M M M M M M
        I004/045 O O O O O O O O
        I004/060 X X X X X X X X
        I004/070 X X X X O O O O
        I004/074 X X O X O X X X
        I004/075 X X X X O X X X
        I004/076 X O O O O X X X
        I004/100 O O O O O O O O
        I004/110 O O O O O O O O
        I004/120 O O O X O O O O
        I004/170 O O O O O O O O
        I004/171 X X X X O O O O
        I004/RE O O O O O O O O

        item 033 034 035 036 037 038 039 040
        I004/000 M M M M M M M M
        I004/010 M M M M M M M M
        I004/015 O O O O O O O O
        I004/020 M M M M M M M M
        I004/030 M M M M M M M M
        I004/035 M M M X O M X X
        I004/040 M M M M M M M M
        I004/045 O O O O O O O O
        I004/060 X X X X X X X X
        I004/070 O O M X X X X X
        I004/074 X X X X X X X X
        I004/075 X X X X X X X X
        I004/076 X X X X X X X X
        I004/100 O O O O O O O O
        I004/110 X X X X X O O O
        I004/120 M M O X X M M M
        I004/170 M M M M M O O O
        I004/171 M M M X M O X X
        I004/RE O O M O O O O O

        item 041 042 043 044 097 098 099
        I004/000 M M M M M M M
        I004/010 M M M M M M M
        I004/015 O O O O O O O
        I004/020 M M M M M M M
        I004/030 M M M M M M M
        I004/035 X X X O X X X
        I004/040 M M M M M M M
        I004/045 O O O O O O O
        I004/060 X X X X X X X
        I004/070 X X X X O O O
        I004/074 X X X X X X X
        I004/075 X X X X X X X
        I004/076 X X X X X X X
        I004/100 O M M M O O O
        I004/110 O O O O O O O
        I004/120 M O X X O O O
        I004/170 O O O O O O O
        I004/171 X X X X X X X
        I004/RE O O O O M O O

I004/010 - Data Source Identifier
*********************************

*Definition*: Identification of the Safety Nets server sending the message.

*Structure*:

    **I004/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I004/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note:
    - The up-to-date list of SACs is published on the EUROCONTROL Web
      Site (http://www.eurocontrol.int/asterix).

I004/015 - SDPS Identifier
**************************

*Definition*: Identification of the SDPS providing data to the safety nets server.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I004/015/SAC** - *System Area Code*

        - 8 bits [``........``]

        - raw value

        **I004/015/SIC** - *System Identification Code*

        - 8 bits [``........``]

        - raw value

Note:
    - The up-to-date list of SACs is published on the EUROCONTROL Web
    Site (http://www.eurocontrol.int/asterix).

I004/020 - Time of Message
**************************

*Definition*: Absolute time stamping of the message in the form of elapsed time since
last midnight

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

I004/030 - Track Number 1
*************************

*Definition*: Identification of a track number related to conflict

*Structure*:

- 16 bits [``................``]

- raw value

Notes:

    1. This is the track number of the first track involved in the
       conflict in case of an STCA or a RIMCA or the track involved
       in case of one of the other Safety Net functions.
    2. This track number is distributed in this field exactly as it was
       received from the Radar Processor Unit (identified by I004/015)
       and its range is depending on the range used by that unit.
    3. In case of Message Type = 33 (Final Target Distance Indicator - FTD)
       this represents the Track Number of the following aircraft.
    4. In case of Message Type = 34 (Initial Target Distance Indicator - ITD
       this represents the Track Number of the following aircraft.
    5. In case of Message Type = 35 (Wake Vortex Indicator Infringement
       Alert - IIA) this represents the track number of the following aircraft.
    6. In case of Message Type = 37 (Catch-Up Warning - CUW) this represents
       the track number of the following aircraft (i.e. the one catching up).

I004/035 - Track Number 2
*************************

*Definition*: Together with I004/030, this item defines the track pair in conflict.

*Structure*:

- 16 bits [``................``]

- raw value

Notes:

    1. This is the track number of the second track involved in the
       conflict in case of an STCA, a RIMCA, a NTCA, a VCD or in message
       types 33 to 35 and 37.
    2. For the other Safety Net functions, this item is not used.
    3. This track number is distributed in this field exactly as it was
       received from the Radar Processor Unit and its range is depending
       on the range used by that unit.
    4. In case of Message Type = 33 (Final Target Distance Indicator - FTD)
       this represents the Track Number of the leading aircraft
    5. In case of Message Type = 34 (Initial Target Distance Indicator - ITD
       this represents the Track Number of the leading aircraft.
    6. In case of Message Type = 35 (Wake Vortex Indicator Infringement
       Alert - IIA) this represents the track number of the leading aircraft.
    7. In case of Message Type = 37 (Catch-Up Warning - CUW) this represents
       the track number of the leading aircraft.
    8. In case of Message Type = 38 (Conflicting ATC Clearances - CATC)
       this represents the track number of the aircraft to which the first
       ATC Clearance was issued.

I004/040 - Alert Identifier
***************************

*Definition*: Identification of an alert (Alert number)

*Structure*:

- 16 bits [``................``]

- raw value

Notes:

    1. This item is the Alert Identification of the conflict in the system
    2. This number shall be assigned, by the Safety Net Server, for instance
       incrementally to every new alert and restart on zero after reaching
       the maximum value (65535)

I004/045 - Alert Status
***********************

*Definition*: Information concerning status of the alert

*Structure*:

    **I004/045/(spare)**

    - 4 bits [``....``]

    **I004/045/STAT** - *Status of the Alert*

    - 3 bits [``...``]

    - raw value

    **I004/045/(spare)**

    - 1 bit [``.``]

I004/060 - Safety Net Function and System Status
************************************************

*Definition*: Status of the Safety Nets functions handled by the system

*Structure*:

Extended item.

    **I004/060/MRVA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: MRVA function

    **I004/060/RAMLD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: RAMLD function

    **I004/060/RAMHD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: RAMHD function

    **I004/060/MSAW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: MSAW function

    **I004/060/APW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: APW function

    **I004/060/CLAM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: CLAM function

    **I004/060/STCA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: STCA function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/APM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: APM function

    **I004/060/RIMCA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: RIMCA function

    **I004/060/ACASRA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: ACAS RA function

    **I004/060/NTCA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: NTCA function

    **I004/060/DG**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: System degraded

    **I004/060/OF**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Overflow error

    **I004/060/OL**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Overload error

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/AIW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: AIW function

    **I004/060/PAIW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: PAIW function

    **I004/060/OCAT**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: OCAT function

    **I004/060/SAM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: SAM function

    **I004/060/VCD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: VCD function

    **I004/060/CHAM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: CHAM function

    **I004/060/DSAM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: DSAM function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/DBPSMARR**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: DBPSM ARR sub-function

    **I004/060/DBPSMDEP**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: DBPSM DEP sub-function

    **I004/060/DBPSMTL**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: DBPSM TL sub-function

    **I004/060/VRAMCRM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: VRAM CRM sub-function

    **I004/060/VRAMVTM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: VRAM VTM sub-function

    **I004/060/VRAMVRM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: VRAM VRM sub-function

    **I004/060/HAMHD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: HAM HD sub-function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/HAMRD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: HAM RD sub-function

    **I004/060/HAMVD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: HAM VD sub-function

    **I004/060/HVI**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: HVI function

    **I004/060/LTW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: LTW function

    **I004/060/VPM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: VPM function

    **I004/060/TTA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: TTA function

    **I004/060/CRA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: CRA function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/ASM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: ASM sub-function

    **I004/060/IAVM**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: IAVM sub-function

    **I004/060/FTD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: FTD Function

    **I004/060/ITD**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: ITD function

    **I004/060/IIA**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: IIA function

    **I004/060/SQW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: SQW function

    **I004/060/CUW**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: CUW function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I004/060/CATC**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: CATC function

    **I004/060/NOCLR**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: NOCLR sub-function

    **I004/060/NOMOV**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: NOMOV Function

    **I004/060/NOH**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: NOH function

    **I004/060/WRTY**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: WRTY function

    **I004/060/STOCC**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: STOCC function

    **I004/060/ONGOING**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: ONGOING function

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Notes:

    1. This item only sent in “alive messages” to describe the status
       of the Safety Net functions, handled by the system
    2. Value 0 means either that the function is not managed by the system
       or has failed.
    3. Value 1 means that the function is managed by the system and is running well
    4. “Overflow” is defined as a situation where the number of alerts
       in the system has exceeded the threshold for safe operation.
       Potential prioritization of the alerts may lead to a loss of information.
    5. “Overload” is defined as a system status in which the number of
       alerts does not allow for a reliable performance. A correct
       calculation and transmission cannot be guaranteed.
    6. “System degraded” means that information from one or more sensors is lost.

I004/070 - Conflict Timing and Separation
*****************************************

*Definition*: Information on Timing and Aircraft Separation

*Structure*:

Compound item (FX)

    **I004/070/TC** - *Time to Conflict*

    Time remaining to actual conflict situation

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

    **I004/070/TCA** - *Time to Closest Approach*

    Time to closest proximity between entities in conflict

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

    **I004/070/CHS** - *Current Horizontal Separation*

    Current horizontal separation

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m

    **I004/070/MHS** - *Estimated Minimum Horizontal Separation*

    Estimated minimum horizontal separation.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m

    **I004/070/CVS** - *Current Vertical Separation*

    Current vertical separation

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "ft"
    - LSB = :math:`25` ft

    **I004/070/MVS** - *Estimated Minimum Vertical Separation*

    Estimated Minimum Vertical Separation

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "ft"
    - LSB = :math:`25` ft

I004/074 - Longitudinal Deviation
*********************************

*Definition*: Longitudinal deviation for Route Adherence Monitoring, in two’s complement.

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "m"
- LSB = :math:`32` m

Note:
    - Longitudinal deviation will be positive if the aircraft is ahead of its
      planned position.
      Longitudinal deviation will be negative if the aircraft is behind its
      planned position.

I004/075 - Transversal Distance Deviation
*****************************************

*Definition*: Transversal distance deviation for Route Adherence Monitoring, in two’s complement.

*Structure*:

- 24 bits [``........................``]

- signed quantity
- unit: "m"
- LSB = :math:`1/2` m :math:`\approx 0.50` m

Note:
   - Deviation to the right of the track will be coded as a positive value.
     Deviation to the left of the track will be coded as a negative value

I004/076 - Vertical Deviation
*****************************

*Definition*: Vertical Deviation from planned altitude, in two’s complement

*Structure*:

- 16 bits [``................``]

- signed quantity
- unit: "ft"
- LSB = :math:`25` ft

Note:
    - Positive value if aircraft is above planned altitude
      Negative value if aircraft is below planned altitude

I004/100 - Area Definition
**************************

*Definition*: Name of the area involved in a Safety Net alarm

*Structure*:

Compound item (FX)

    **I004/100/AN** - *Area Name*

    Name of the area involved in a Safety Net alarm.
    Characters 1-8 (coded on 6 bits each) defining the name of the area.
    Coding rules are provided in [3]Section 3.1.2.9"

    - 48 bits [``... 48 bits ...``]

    - ICAO string (6-bits per character)

    **I004/100/CAN** - *Crossing Area Name*

    Name of Crossing Area Involved in RIMCA.
    Each octet is an ASCII character defining the name of the crossing
    area involved in a runway/taxiway crossing alert (message type 013)

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/100/RT1** - *Runway/Taxiway Designator 1*

    Designator of Runway/Taxiway 1 Involved in a RIMCA
    Each octet is an ASCII character defining the runway designator

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/100/RT2** - *Runway/Taxiway Designator 2*

    Designator of Runway/Taxiway 2 Involved in a RIMCA
    Each octet is an ASCII character defining the runway designator

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/100/SB** - *Stop Bar Designator*

    Designator of Stop-Bar Involved in RIMCA
    Each octet is an ASCII character defining the stop-bar involved
    in a stop-bar crossed alert (message type 016)

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/100/G** - *Gate Designator*

    Gate Designator (in 7 characters) of the approaching aircraft in
    a RIMCA or a STOCC message,
    Each octet is an ASCII character defining the gate for the
    approaching aircraft

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

Notes:

    1. The area name is always left adjusted. If needed, the remaining
       characters are filled with space character.
    2. The name of the crossing area is always left adjusted. If needed,
       the remaining characters are filled with space characters.
    3. The runway designator is always left adjusted. If needed, the
       remaining characters are filled with space characters.
       The runway is encoded as follows: Location indicator, runway
       direction, left or right.
       Example: EGLL09L means London Heathrow (EGLL), Runway 09
       (direction 090 degrees) left runway
    4. The runway designator is always left adjusted. If needed, the
       remaining characters are filled with space characters.
       The runway is encoded as follows: Location indicator, runway
       direction, left or right.
       Example: EGLL09L means London Heathrow (EGLL), Runway 09
       (direction 090 degrees) left runway
    5. The stop-bar designator is always left adjusted. If needed, the
       remaining characters are filled with space characters.
    6. The gate designator is always left adjusted. If needed, the
       remaining characters are filled with space character.

I004/110 - FDPS Sector Control Identification
*********************************************

*Definition*: Identification of a list of FDPS Sector Control Positions in charge of
the involved targets, as provided by the FDPS

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I004/110/CEN**

        Centre identification code

        - 8 bits [``........``]

        - raw value

        **I004/110/POS**

        Control position identification code

        - 8 bits [``........``]

        - raw value

Note:
    - The Centre identification code and the Control position identification
      code must be defined between the communication partners.

I004/120 - Conflict Characteristics
***********************************

*Definition*: Description of the Conflict Properties

*Structure*:

Compound item (FX)

    **I004/120/CN** - *Conflict Nature*

    Nature of the conflict expressed by a set of properties

    Extended item.

        **I004/120/CN/MAS** - *Conflict Location in Military Airspace*

        - 1 bit [``.``]

        - values:

            | 0: Conflict not predicted to occur in military airspace
            | 1: Conflict predicted to occur in military airspace

        **I004/120/CN/CAS** - *Conflict Location in Civil Airspace*

        - 1 bit [``.``]

        - values:

            | 0: Conflict not predicted to occur in civil airspace
            | 1: Conflict predicted to occur in civil airspace

        **I004/120/CN/FLD** - *Fast Lateral Divergence*

        - 1 bit [``.``]

        - values:

            | 0: Aircraft are not fast diverging laterally at current time
            | 1: Aircraft are fast diverging laterally at current time

        **I004/120/CN/FVD** - *Fast Vertical Divergence*

        - 1 bit [``.``]

        - values:

            | 0: Aircraft are not fast diverging vertically at current time
            | 1: Aircraft are fast diverging vertically at current time

        **I004/120/CN/TYPE** - *Type of Separation Infringement*

        - 1 bit [``.``]

        - values:

            | 0: Minor separation infringement
            | 1: Major separation infringement

        **I004/120/CN/CROSS** - *Crossing Test*

        - 1 bit [``.``]

        - values:

            | 0: Aircraft have not crossed at starting time of conflict
            | 1: Aircraft have crossed at starting time of conflict

        **I004/120/CN/DIV** - *Divergence Test*

        - 1 bit [``.``]

        - values:

            | 0: Aircraft are not diverging at starting time of conflict
            | 1: Aircraft are diverging at starting time of conflict

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I004/120/CN/RRC** - *Runway/Runway Crossing in RIMCAS*

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Runway/Runway Crossing

        **I004/120/CN/RTC** - *Runway/Taxiway Crossing in RIMCAS*

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Runway/Taxiway Crossing

        **I004/120/CN/MRVA**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 4 (MSAW) indicates MRVA

        **I004/120/CN/VRAMCRM**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 25 (VRAM) indicates CRM

        **I004/120/CN/VRAMVRM**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 25 (VRAM) indicates VRM

        **I004/120/CN/VRAMVTM**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 25 (VRAM) indicates VTM

        **I004/120/CN/HAMHD**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 29 (HAM) indicates HD

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I004/120/CN/HAMRD**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 29 (HAM) indicates RD

        **I004/120/CN/HAMVD**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 29 (HAM) indicates VD

        **I004/120/CN/DBPSMARR**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 20 (DBPSM) indicates ARR

        **I004/120/CN/DBPSMDEP**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 20 (DBPSM) indicates DEP

        **I004/120/CN/DBPSMTL**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 20 (DBPSM) indicates above TL

        **I004/120/CN/AIW**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Msg Type 99 (AIW) indicates pAIW Alert

        **I004/120/CN/(spare)**

        - 1 bit [``.``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I004/120/CC** - *Conflict Classification*

    Severity classification of the conflict

        **I004/120/CC/TID** - *Identification of Conflict Categories Definition Table*

        - 4 bits [``....``]

        - raw value

        **I004/120/CC/CPC** - *Conflict Properties Class*

        * Depends on the value of ``(000, 120/CC/TID)``.
        * In case of ``(000, 120/CC/TID) == (5, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: APW Low Severity
                | 1: APW Medium Severity
                | 2: APW High Severity

        * In case of ``(000, 120/CC/TID) == (7, 0)``:
            - 3 bits [``...``]

            - values:

                | 1: Major seperation infringement and not (crossed and diverging)
                | 2: Minor seperation infringement and not (crossed and diverging)
                | 3: Major seperation infringement and (crossed and diverging)
                | 4: Minor seperation infringement and (crossed and diverging)

        * In case of ``(000, 120/CC/TID) == (7, 1)``:
                **I004/120/CC/CPC/LPF** - *Linear Prediction Filter*

                - 1 bit [``.``]

                - values:

                    | 0: Filter not set
                    | 1: Filter set

                **I004/120/CC/CPC/CPF** - *Current Proximity Filter*

                - 1 bit [``.``]

                - values:

                    | 0: Filter not set
                    | 1: Filter set

                **I004/120/CC/CPC/MHF** - *Manoeuvre Hazard Filter*

                - 1 bit [``.``]

                - values:

                    | 0: Filter not set
                    | 1: Filter set

        * In case of ``(000, 120/CC/TID) == (9, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (10, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (11, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (12, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (13, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (14, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (15, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (16, 2)``:
                **I004/120/CC/CPC/RAS** - *RIMCAS Alert Stage*

                - 1 bit [``.``]

                - values:

                    | 0: Stage One Alert
                    | 1: Stage Two Alert

                **I004/120/CC/CPC/(spare)**

                - 2 bits [``..``]

        * In case of ``(000, 120/CC/TID) == (15, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: 2 aircraft, same taxiway, opposite direction
                | 1: Aircraft entering wrong direction
                | 2: Aircraft entering wrong taxiway
                | 3: Speed violation

        * In case of ``(000, 120/CC/TID) == (24, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: VRM Slow Climb
                | 1: VRM Slow Descent

        * In case of ``(000, 120/CC/TID) == (24, 2)``:
            - 3 bits [``...``]

            - values:

                | 0: VTM Fast Climb
                | 1: VTM Fast Descent

        * In case of ``(000, 120/CC/TID) == (26, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: Vertical manoeuvre deviation prior to reaching its expected level
                | 1: Vertical manoeuvre deviation past its expected level

        * In case of ``(000, 120/CC/TID) == (27, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: Slow Descent
                | 1: Fast Descent
                | 2: Slow Climb
                | 3: Fast Climb

        * In case of ``(000, 120/CC/TID) == (27, 2)``:
            - 3 bits [``...``]

            - values:

                | 0: Above
                | 1: Below

        * In case of ``(000, 120/CC/TID) == (33, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: Table - Single RWY Operation
                | 1: MRS - Single RWY Operation
                | 2: ROT - Single RWY Operation
                | 3: GAP - Single RWY Operation
                | 4: Table - Parallel RWY Operation
                | 5: MRS - Parallel RWY Operation
                | 6: ROT - Parallel RWY Operation
                | 7: GAP - Parallel RWY Operation

        * In case of ``(000, 120/CC/TID) == (34, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: Table - Single RWY Operation
                | 1: MRS - Single RWY Operation
                | 2: ROT - Single RWY Operation
                | 3: GAP - Single RWY Operation
                | 4: Table - Parallel RWY Operation
                | 5: MRS - Parallel RWY Operation
                | 6: ROT - Parallel RWY Operation
                | 7: GAP - Parallel RWY Operation

        * In case of ``(000, 120/CC/TID) == (35, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: End of Alert
                | 1: Planned Alert
                | 2: Alert on TABLE Indicator
                | 3: Alert on MRS Indicator
                | 4: Alert on ROT Indicator
                | 5: Alert on GAP Indicator

        * In case of ``(000, 120/CC/TID) == (38, 0)``:
            - 3 bits [``...``]

            - values:

                | 0: Line-Up vs. Line-Up
                | 1: Line-Up vs. Cross or Enter
                | 2: Line-Up vs. Take-Off
                | 3: Line-Up vs. Landing

        * In case of ``(000, 120/CC/TID) == (38, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: Cross or Enter  vs. Line-Up
                | 1: Cross or Enter  vs. Cross or Enter
                | 2: Cross or Enter  vs. Take-Off
                | 3: Cross or Enter  vs. Landing

        * In case of ``(000, 120/CC/TID) == (38, 2)``:
            - 3 bits [``...``]

            - values:

                | 0: Take-Off vs. Line-Up
                | 1: Take-Off vs. Cross or Enter
                | 2: Take-Off vs. Take-Off
                | 3: Take-Off vs. Landing

        * In case of ``(000, 120/CC/TID) == (38, 3)``:
            - 3 bits [``...``]

            - values:

                | 0: Landing vs. Line-Up
                | 1: Landing vs. Cross or Enter
                | 2: Landing vs. Take-Off
                | 3: Landing vs. Landing

        * In case of ``(000, 120/CC/TID) == (38, 4)``:
            - 3 bits [``...``]

            - values:

                | 0: Push-Back vs. Push-Back
                | 1: Push-Back vs. Taxi

        * In case of ``(000, 120/CC/TID) == (38, 5)``:
            - 3 bits [``...``]

            - values:

                | 0: Taxi vs. Push-Back
                | 1: TAxi vs. Taxi

        * In case of ``(000, 120/CC/TID) == (39, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: No Push-Back Clearance
                | 1: No Taxi Clearance
                | 2: No Line-Up Clearance
                | 3: No Crossing Clearance
                | 4: No Enter Clearance
                | 5: No Take-Off Clearance
                | 6: Landing Clearance

        * In case of ``(000, 120/CC/TID) == (40, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: After Push-Back Clearance
                | 1: After Taxi Clearance
                | 2: After Line-Up Clearance
                | 3: After Crossing Clearance
                | 4: After Enter Clearance
                | 5: After Take-Off Clearance
                | 6: Stationary on Runway
                | 7: Stationary on Taxiway

        * In case of ``(000, 120/CC/TID) == (41, 1)``:
            - 3 bits [``...``]

            - values:

                | 0: No contact (receiving ATSU)
                | 1: No transfer (leaving ATSU)

        * Default:
            - 3 bits [``...``]

            - raw value

        **I004/120/CC/CS** - *Conflict Severity*

        - 1 bit [``.``]

        - values:

            | 0: LOW
            | 1: HIGH

    **I004/120/CP** - *Conflict Probability*

    Probability of the conflict

    - 8 bits [``........``]

    - unsigned quantity
    - unit: "%"
    - LSB = :math:`1/2` % :math:`\approx 0.50` %

    **I004/120/CD** - *Conflict Duration*

    The duration of the conflict is the elapsed time since the declaration of the conflict.

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

Note:
    If no Table Id is defined for a message type, only the value of the CS
    bit may be of relevance. In that case, for this message type, Table Id
    and Conflict Properties are meaningless and shall be set to "0000"
    and "000" respectively.
    1. Additional conflict classes may be defined by introducing additional
       properties of a conflict.
    2. For FTD (Message Type = 033), ITD (Message Type = 034) and IIA
       (Message Type = 035) the following types of separation have been
       applied:
       Table: application of the values contained in the separation table
       according to the different wake vortex categories of the two aircraft.
       MRS: Minimum Radar Separation for the arrival runway
       ROT: Runway Occupancy Time – separation to achieve a specific ROT.
       GAP: separation based on a gap manually input by the ATCO

I004/170 - Aircraft Identification and Characteristics 1
********************************************************

*Definition*: Identification & Characteristics of Aircraft 1 Involved in the Conflict.

*Structure*:

Compound item (FX)

    **I004/170/AI1** - *Aircraft Identifier (in 7 Characters) of Aircraft 1 Involved in the Conflict*

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/170/M31** - *Mode 3/A Code Aircraft 1*

        **I004/170/M31/(spare)**

        - 4 bits [``....``]

        **I004/170/M31/MODE3A** - *Mode-3/A Code (Converted Into Octal Representation) of Aircraft 1 Involved in the Conflict*

        - 12 bits [``............``]

        - Octal string (3-bits per digit)

    **I004/170/CPW** - *Predicted Conflict Position Target 1 in WGS-84 Coordinates*

        **I004/170/CPW/LAT** - *In WGS-84 in Two’s Complement*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I004/170/CPW/LON** - *In WGS-84 in Two’s Complement*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

        **I004/170/CPW/ALT** - *Altitude of Predicted Conflict*

        - 16 bits [``................``]

        - signed quantity
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1500` ft
        - value :math:`<= 150000` ft

    **I004/170/CPC** - *Predicted Conflict Position for the Aircraft 1 Involved in the Conflict*

        **I004/170/CPC/X** - *Starting X-position of the Conflict*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m

        **I004/170/CPC/Y** - *Starting Y-position of the Conflict*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m

        **I004/170/CPC/Z** - *Starting Z-position of the Conflict*

        - 16 bits [``................``]

        - signed quantity
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1500` ft
        - value :math:`<= 150000` ft

    **I004/170/TT1** - *Time to Runway Threshold for First Approaching Aircraft in a RIMCA*

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

    **I004/170/DT1** - *Distance to Runway Threshold for Aircraft 1 Involved in a RIMCA*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m

    **I004/170/AC1** - *Characteristics of Aircraft 1 Involved in the Conflict*

    Extended item.

        **I004/170/AC1/GATOAT** - *Identification of Conflict Categories Definition Table*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: General Air Traffic
            | 2: Operational Air Traffic
            | 3: Not applicable

        **I004/170/AC1/FR1FR2** - *Flight Rules*

        - 2 bits [``..``]

        - values:

            | 0: Instrument Flight Rules
            | 1: Visual Flight rules
            | 2: Not applicable
            | 3: Controlled Visual Flight Rules

        **I004/170/AC1/RVSM**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Approved
            | 2: Exempt
            | 3: Not Approved

        **I004/170/AC1/HPR**

        - 1 bit [``.``]

        - values:

            | 0: Normal Priority Flight
            | 1: High Priority Flight

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I004/170/AC1/CDM** - *Climbing/Descending Mode*

        - 2 bits [``..``]

        - values:

            | 0: Maintaining
            | 1: Climbing
            | 2: Descending
            | 3: Invalid

        **I004/170/AC1/PRI**

        - 1 bit [``.``]

        - values:

            | 0: Non primary target
            | 1: Primary target

        **I004/170/AC1/GV**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Ground Vehicle

        **I004/170/AC1/(spare)**

        - 3 bits [``...``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I004/170/MS1** - *Aircraft Identification Downloaded from Aircraft 1 Involved in the Conflict If Equipped with a Mode-S Transponder*

    - 48 bits [``... 48 bits ...``]

    - Ascii string (8-bits per character)

    **I004/170/FP1** - *Number of the Flight Plan Correlated to Aircraft 1 Involved in the Conflict*

        **I004/170/FP1/(spare)**

        - 5 bits [``.....``]

        **I004/170/FP1/NBR**

        - 27 bits [``...........................``]

        - unsigned quantity
        - LSB = :math:`1`
        - value :math:`>= 0` 
        - value :math:`<= 99999999` 

    **I004/170/CF1** - *Cleared Flight Level for Aircraft 1 Involved in the Conflict*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "FL"
    - LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL

Notes:

    1. The aircraft identifier is always left adjusted. If needed, the
       remaining characters are filled with space character.
    2. For Message Type = 33 (Final Target Distance Indicator - FTD) this
       contains the aircraft identifier of the following aircraft.
    3. For Message Type = 34 (Initial Target Distance Indicator - ITD)
       this contains the aircraft identifier of the following aircraft.
    4. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
       this contains the aircraft identifier of the following aircraft.
    5. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
       aircraft identifier of the following aircraft (i.e. the one catching up).
    6. For Message Type = 33 (Final Target Distance Indicator - FTD) this
       contains the Mode 3/A Code of the following aircraft.
    7. For Message Type = 34 (Initial Target Distance Indicator - ITD)
       this contains the Mode 3/A Code of the following aircraft.
    8. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
       this contains the Mode 3/A Code of the following aircraft.
    9. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
       Mode 3/A code of the following aircraft (i.e. the one catching up).
    10. Altitude expressed in two’s complement.
    11. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        data item contains the position (in WGS-84) of the Separation
        Indicator presented to the ATCO. In this case bits 16/1 are meaningless.
    12. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this data item contains the position (in WGS-84) of the Separation
        Indicator presented to the ATCO. In this case bits 16/1 are meaningless.
    13. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this data item contains the position (in WGS-84) of the Separation
        Indicator presented to the ATCO. In this case bits 16/1 are meaningless.
    14. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        position (in WGS-84) of the Separation Indicator presented to the ATCO.
        In this case bits 16/1 are meaningless.
    15. Two’s complement fixed-point format.
    16. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        data item contains the position (in Cartesian Coordinates) of the
        Separation Indicator presented to the ATCO. In this case bits 16/1 are meaningless.
    17. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this data item contains the position (in Cartesian Coordinates)
        of the Separation Indicator presented to the ATCO. In this case bits
        16/1 are meaningless.
    18. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this data item contains the position (in Cartesian Coordinates) of the
        Separation Indicator presented to the ATCO. In this case bits 16/1 are meaningless.
    19. For Message Type = 37 (Catch-Up Warning - CUW) this data item
        contains the position (in Cartesian Coordinates) of the Separation
        Indicator presented to the ATCO. In this case bits 16/1 are meaningless.

    20. Time to Threshold expressed in Two’s Complement
    21. For Message Type = 33 (Final Target Distance Indicator - FTD) and
        for Message Type = 34 (Initial Target Distance Indicator - FTD)
        this data item contains the additional gap in spacing between two
        approaching aircraft as manually inserted by the ATCO. This could
        be used, for example, to increase the spacing between approaching
        aircraft in order to generate sufficient spacing to clear a departing aircraft.
    22. For Message Type = 33 (Final Target Distance Indicator - FTD)
        thiscontains the Aircraft Characteristics of the following aircraft.
    23. For Message Type = 34 (Initial Target Distance Indicator - FTD)
        this contains the Aircraft Characteristics of the following aircraft.
    24. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Aircraft Characteristics of the following aircraft.
    25. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Aircraft Characteristics of the following aircraft.
    26. For Message Type = 33 (Final Target Distance Indicator - FTD)
        this contains the Mode-S Identifier of the following aircraft.
    27. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this contains the Mode-S Identifier of the following aircraft.
    28. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Mode-S Identifier of the following aircraft.
    29. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Mode-S Identifier of the following aircraft.
    30. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Flight Plan Number of the following aircraft.
    31. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this contains the Flight Plan Number of the following aircraft.
    32. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Flight Plan Number of the following aircraft.
    33. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Flight Plan Number of the following aircraft.
    34. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Cleared Flight Level of the following aircraft.
    35. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this contains the Cleared Flight Level of the following aircraft.
    36. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Cleared Flight Level of the following aircraft.
    37. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Cleared Flight Level of the following aircraft.

I004/171 - Aircraft Identification and Characteristics 2
********************************************************

*Definition*: Identification & Characteristics of Aircraft 2 Involved in the Conflict.

*Structure*:

Compound item (FX)

    **I004/171/AI2** - *Aircraft Identifier (in 7 Characters) of Aircraft 2 Involved in the Conflict*

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I004/171/M32** - *Mode 3/A Code Aircraft 2*

        **I004/171/M32/(spare)**

        - 4 bits [``....``]

        **I004/171/M32/MODE3A** - *Mode-3/A Code (Converted Into Octal Representation) of Aircraft 2 Involved in the Conflict*

        - 12 bits [``............``]

        - Octal string (3-bits per digit)

    **I004/171/CPW** - *Predicted Conflict Position Target 2 in WGS-84 Coordinates*

        **I004/171/CPW/LAT** - *In WGS-84 in Two’s Complement*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I004/171/CPW/LON** - *In WGS-84 in Two’s Complement*

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^25` ° :math:`\approx 5.36e-6` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

        **I004/171/CPW/ALT** - *Altitude of Predicted Conflict*

        - 16 bits [``................``]

        - signed quantity
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1500` ft
        - value :math:`<= 150000` ft

    **I004/171/CPL** - *Predicted Conflict Position for the Aircraft 2 Involved in the Conflict*

        **I004/171/CPL/X** - *Starting X-position of the Conflict*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m

        **I004/171/CPL/Y** - *Starting Y-position of the Conflict*

        - 24 bits [``........................``]

        - signed quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m

        **I004/171/CPL/Z** - *Starting Z-position of the Conflict*

        - 16 bits [``................``]

        - signed quantity
        - unit: "ft"
        - LSB = :math:`25` ft
        - value :math:`>= -1500` ft
        - value :math:`<= 150000` ft

    **I004/171/TT2** - *Time to Runway Threshold for Second Approaching Aircraft in a RIMCA*

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

    **I004/171/DT2** - *Distance to Runway Threshold for Aircraft 2 Involved in a RIMCA*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/2` m :math:`\approx 0.50` m

    **I004/171/AC2** - *Characteristics of Aircraft 2 Involved in the Conflict*

    Extended item.

        **I004/171/AC2/GATOAT** - *Identification of Conflict Categories Definition Table*

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: General Air Traffic
            | 2: Operational Air Traffic
            | 3: Not applicable

        **I004/171/AC2/FR1FR2** - *Flight Rules*

        - 2 bits [``..``]

        - values:

            | 0: Instrument Flight Rules
            | 1: Visual Flight rules
            | 2: Not applicable
            | 3: Controlled Visual Flight Rules

        **I004/171/AC2/RVSM**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Approved
            | 2: Exempt
            | 3: Not Approved

        **I004/171/AC2/HPR**

        - 1 bit [``.``]

        - values:

            | 0: Normal Priority Flight
            | 1: High Priority Flight

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I004/171/AC2/CDM** - *Climbing/Descending Mode*

        - 2 bits [``..``]

        - values:

            | 0: Maintaining
            | 1: Climbing
            | 2: Descending
            | 3: Invalid

        **I004/171/AC2/PRI**

        - 1 bit [``.``]

        - values:

            | 0: Non primary target
            | 1: Primary target

        **I004/171/AC2/GV**

        - 1 bit [``.``]

        - values:

            | 0: Default
            | 1: Ground Vehicle

        **I004/171/AC2/(spare)**

        - 3 bits [``...``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I004/171/MS2** - *Aircraft Identification Downloaded From Aircraft 2 Involved in the Conflict If Eequipped With a Mode-S Transponder*

    - 48 bits [``... 48 bits ...``]

    - Ascii string (8-bits per character)

    **I004/171/FP2** - *Number of the Flight Plan Correlated to Aircraft 2 Involved in the Conflict*

        **I004/171/FP2/(spare)**

        - 5 bits [``.....``]

        **I004/171/FP2/NBR**

        - 27 bits [``...........................``]

        - unsigned quantity
        - LSB = :math:`1`
        - value :math:`>= 0` 
        - value :math:`<= 99999999` 

    **I004/171/CF2** - *Cleared Flight Level for Aircraft 2 Involved in the Conflict*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "FL"
    - LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL

Notes:

    1. The aircraft identifier is always left adjusted. If needed, the
       remaining characters are filled with space character.
    2. For Message Type = 33 (Final Target Distance Indicator - FTD) this
       contains the aircraft identifier of the leading aircraft.
    3. For Message Type = 34 (Initial Target Distance Indicator - ITD)
       this contains the aircraft identifier of the leading aircraft.
    4. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
       this contains the aircraft identifier of the leading aircraft.
    5. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
       aircraft identifier of the leading aircraft.
    6. For Message Type = 33 (Final Target Distance Indicator - FTD) this
       contains the Mode 3/A Code of the leading aircraft.
    7. For Message Type = 34 (Initial Target Distance Indicator - ITD)
       this contains the Mode 3/A Code of the leading aircraft.
    8. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
       this contains the Mode 3/A Code of the leading aircraft.
    9. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
       Mode 3/A code of the leading aircraft.
    10. Altitude expressed in two’s complement.
    11. FTwo’s complement.
    12. Time to Threshold expressed in Two’s Complement
    13. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Aircraft Characteristics of the leading aircraft.
    14. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        this contains the Aircraft Characteristics of the leading aircraft.
    15. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Aircraft Characteristics of the leading aircraft.
    16. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Aircraft Characteristics of the leading aircraft.
    17. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Mode-S Identifier of the leading aircraft.
    18. For Message Type = 34 (Initial Target Distance Indicator - ITD) this
        contains the Mode-S Identifier of the leading aircraft.
    19. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Mode-S Identifier of the leading aircraft.
    20. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Mode-S Identifier of the leading aircraft.
    21. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Flight Plan Number of the leading aircraft.
    22. For Message Type = 34 (Initial Target Distance Indicator - ITD) this
        contains the Flight Plan Number of the leading aircraft.
    23. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Flight Plan Number of the leading aircraft.
    24. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Flight Plan Number of the leading aircraft.
    25. The value shall be within the range described by ICAO Annex 10
    26. For Message Type = 33 (Final Target Distance Indicator - FTD) this
        contains the Cleared Flight Level of the leading aircraft.
    27. For Message Type = 34 (Initial Target Distance Indicator - ITD)
        thiscontains the Cleared Flight Level of the leading aircraft.
    28. For Message Type = 35 (Wake Vortex Indicator Infringement Alert - IIA)
        this contains the Cleared Flight Level of the leading aircraft.
    29. For Message Type = 37 (Catch-Up Warning - CUW) this contains the
        Cleared Flight Level of the leading aircraft.

I004/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item (RE)

I004/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 004
=========================================
- (1) ``I004/010`` - Data Source Identifier
- (2) ``I004/000`` - Message Type
- (3) ``I004/015`` - SDPS Identifier
- (4) ``I004/020`` - Time of Message
- (5) ``I004/040`` - Alert Identifier
- (6) ``I004/045`` - Alert Status
- (7) ``I004/060`` - Safety Net Function and System Status
- ``(FX)`` - Field extension indicator
- (8) ``I004/030`` - Track Number 1
- (9) ``I004/170`` - Aircraft Identification and Characteristics 1
- (10) ``I004/120`` - Conflict Characteristics
- (11) ``I004/070`` - Conflict Timing and Separation
- (12) ``I004/076`` - Vertical Deviation
- (13) ``I004/074`` - Longitudinal Deviation
- (14) ``I004/075`` - Transversal Distance Deviation
- ``(FX)`` - Field extension indicator
- (15) ``I004/100`` - Area Definition
- (16) ``I004/035`` - Track Number 2
- (17) ``I004/171`` - Aircraft Identification and Characteristics 2
- (18) ``I004/110`` - FDPS Sector Control Identification
- (19) ``(spare)``
- (20) ``I004/RE`` - Reserved Expansion Field
- (21) ``I004/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator
