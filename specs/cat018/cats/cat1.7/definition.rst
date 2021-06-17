Asterix category 018 - Mode S Datalink Function Messages
========================================================
**category**: 018

**edition**: 1.7

**date**: 2015-11-08

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I018/000 - Message Type
***********************

*Definition*: Allows identification of the message type.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: Associate_req
    | 1: Associate_resp
    | 2: Release_req
    | 3: Release_resp
    | 4: Abort_req
    | 5: Keep_alive
    | 16: Aircraft_report
    | 17: Aircraft_command
    | 18: II_code_change
    | 32: Uplink_packet
    | 33: Cancel_uplink_packet
    | 34: Uplink_packet_ack
    | 35: Downlink_packet
    | 38: Data_XON
    | 39: Data_XOFF
    | 48: Uplink_broadcast
    | 49: Cancel_uplink_broadcast
    | 50: Uplink_broadcast_ack
    | 52: Downlink_broadcast
    | 64: GICB_extraction
    | 65: Cancel_GICB_extraction
    | 66: GICB_extraction_ack
    | 67: GICB_response



I018/001 - Result
*****************

*Definition*: Indicates the status of a particular message together with additional information.

*Structure*:

    **I018/001/CAUSE** - *Cause*

    - 4 bits [``....``]

    - values:

        | 0: Accepted, the request is accepted and is under processing
        | 1: Rejected, the request has not been accepted
        | 2: Cancelled, the request has been cancelled
        | 3: Finished, the request has been accepted and successfully processed
        | 4: Delayed, the request processing is temporarily delayed but the request is still valid
        | 5: In Progress, the request is being successfully processed
        | 6: In Progress, the request is being successfully processed

    **I018/001/DIAG** - *Diagnostic*

    - 4 bits [``....``]

    - values:

        | 0: No diagnostic available
        | 1: Aircraft Exit
        | 2: Incorrect aircraft address
        | 3: Impossibility to process the message
        | 4: Insufficient or change in data link capability
        | 5: Invalid LV field
        | 6: Duplicate request number
        | 7: Unknown request number
        | 8: Timer T3 expiry
        | 9: Expiry of I/R delivery timer
        | 10: Uplink flow disabled by UC



I018/002 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as Co-ordinated Universal Time (UTC) time.

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
       to Part 1, paragraph 5.4.

I018/004 - II Code
******************

*Definition*: Indicates the interrogator’s current and previous II Code.

*Structure*:

    **I018/004/PREVIOUSII** - *Former II Code*

    - 4 bits [``....``]

    - raw value

    **I018/004/CURRENTII** - *Current II Code*

    - 4 bits [``....``]

    - raw value


Note:
    - The Previous II code shall be set to the Current II code value when there is no Previous II code available.

I018/005 - Mode S Address
*************************

*Definition*: Technical Mode S address used for identification of an aircraft, as defined in ICAO Annex 10.

*Structure*:

- 24 bits [``........................``]

- raw value



I018/006 - Mode S Address List
******************************

*Definition*: List of technical Mode S addresses.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 24 bits [``........................``]

    - raw value



I018/007 - Aircraft Data Link Command
*************************************

*Definition*: Command for the aircraft data link communications. It allows the
GDLP to enable or disable the uplink & downlink data flows for a
specified aircraft.

*Structure*:

    **I018/007/UM** - *Uplink Mask*

    - 1 bit [``.``]

    - values:

        | 0: UC shall be ignored
        | 1: UC shall be taken into account

    **I018/007/DM** - *Downlink Mask*

    - 1 bit [``.``]

    - values:

        | 0: DC shall be ignored
        | 1: DC shall be taken into account

    **I018/007/UC** - *Uplink Command*

    - 1 bit [``.``]

    - values:

        | 0: the uplink flow shall be enabled
        | 1: the uplink flow shall be stopped

    **I018/007/DC** - *Downlink Command*

    - 1 bit [``.``]

    - values:

        | 0: the downlink flow shall be enabled
        | 1: the downlink flow shall be stopped

    **I018/007/(spare)**

    - 4 bits [``....``]


Note:
    - This command applies to the interrogator's
      Current status (UCS/DCS) and does not affect the
      interrogator’s Default Status (see UDS/DDS in
      Data Item I018/008).

I018/008 - Aircraft Data Link Status
************************************

*Definition*: Status for the aircraft data link communications.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I018/008/UDS** - *Uplink Default Status*

    - 1 bit [``.``]

    - values:

        | 0: The interrogator is enabled to uplink frames
        | 1: The interrogator is disabled to uplink frames

    **I018/008/DDS** - *Downlink Default Status*

    - 1 bit [``.``]

    - values:

        | 0: The interrogator is enabled to extract frames
        | 1: The interrogator is disabled to extract frames

    **I018/008/UCS** - *Uplink Current Status*

    - 1 bit [``.``]

    - values:

        | 0: The interrogator is enabled to uplink frames
        | 1: The interrogator is disabled to uplink frames

    **I018/008/DCS** - *Downlink Current Status*

    - 1 bit [``.``]

    - values:

        | 0: The interrogator is enabled to extract frames
        | 1: The interrogator is disabled to extract frames

    **I018/008/(spare)**

    - 2 bits [``..``]

    **I018/008/EI** - *Exit Indication*

    - 1 bit [``.``]

    - values:

        | 0: The aircraft is in the Datalink coverage map of the interrogator
        | 1: The aircraft is not in the Datalink coverage map of the interrogator

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I018/008/IC** - *Interrogator Control*

    - 1 bit [``.``]

    - values:

        | 0: The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report could be changed using D_Data_link_command
        | 1: The interrogators current ability to uplink/downlink frames (UCS/DCS) and the content of the Aircraft_report cannot be changed using D_Data_link_command

    **I018/008/(spare)**

    - 6 bits [``......``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:
    1. The current status should never be more restrictive than the default status.
    2. IC is usually set 1when the interrogator is a member of a cluster with a
       decentralised data link responsibility protocol.
       IC is usually set to 0 when the interrogator is connected to a GDLP. IC
       settings shall comply with the rules defined in Ref.3.

I018/009 - Aircraft Data Link Report Request
********************************************

*Definition*: Request for an Aircraft_report message.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I018/009/SR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_Data_link_status
        | 1: The next Aircraft_report shall include D_Data_link_status

    **I018/009/AR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_COM
        | 1: The next Aircraft_report shall include D_COM

    **I018/009/ER**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_ECA
        | 1: The next Aircraft_report shall include D_ECA

    **I018/009/FR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_CQF
        | 1: The next Aircraft_report shall include D_CQF

    **I018/009/MR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_CQF_method
        | 1: The next Aircraft_report shall include D_CQF_method

    **I018/009/PR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_Polar_position
        | 1: The next Aircraft_report shall include D_Polar_position

    **I018/009/CR**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include D_Cartesian_position
        | 1: The next Aircraft_report shall include D_Cartesian_position

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I018/009/ID**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include Aircraft_ID
        | 1: The next Aircraft_report shall include Aircraft_ID

    **I018/009/MA**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include Mode_A
        | 1: The next Aircraft_report shall include Mode_A

    **I018/009/SP**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include Speed
        | 1: The next Aircraft_report shall include Speed

    **I018/009/HG**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include Height
        | 1: The next Aircraft_report shall include Height

    **I018/009/HD**

    - 1 bit [``.``]

    - values:

        | 0: The next Aircraft_report may not include Heading
        | 1: The next Aircraft_report shall include Heading

    **I018/009/(spare)**

    - 2 bits [``..``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Note:
    - This item indicates to the DLF which items to send in the next Aircraft_report
      messages (for a specified aircraft) through the use of flags. These flags
      concern D_Data_link_status, D_COM, D_ECA, D_CQF, D_CQF_method,
      D_Polar_position, D_Cartesian_position, D_Aircraft_ID, D_Mode_A,
      D_Speed, D_Height, D_Heading.

I018/010 - Transponder Communications Capability
************************************************

*Definition*: Transponder Communications Capability

*Structure*:

    **I018/010/(spare)**

    - 5 bits [``.....``]

    **I018/010/COM** - *Communications Capability of the Transponder*

    - 3 bits [``...``]

    - values:

        | 0: No communications capability (surveillance only)
        | 1: Comm. A and Comm. B capability
        | 2: Comm. A, Comm. B and Uplink ELM
        | 3: Comm. A, Comm. B and Uplink ELM and Downlink ELM
        | 4: Level 5 Transponder capability



I018/011 - Capability Report
****************************

*Definition*: Capability report as described in the Mode S subnetwork SARPs

*Structure*:

- 56 bits [``........................................................``]

- raw value



I018/012 - Aircraft Coverage Quality Factor
*******************************************

*Definition*: Coverage Quality Factor (CQF) of an aircraft (for a given interrogator).

*Structure*:

    **I018/012/FS** - *Flight Status*

    - 1 bit [``.``]

    - values:

        | 0: Aircraft is airborne
        | 1: Aircraft is on the ground

    **I018/012/CQF** - *Aircraft CQF*

    - 7 bits [``.......``]

    - values:

        | 0: The CQF calculation method is not supported
        | 1: The CQF is minimum
        | 126: The CQF is maximum
        | 127: The CQF is undefined according to the calculation method



I018/013 -  Aircraft CQF Calculation Method
*******************************************

*Definition*: Indicates which criteria to take into account when computing the CQF of an aircraft for an interrogator.

*Structure*:

- 8 bits [``........``]

- raw value



I018/014 - Aircraft Position in Polar Co-ordinates
**************************************************

*Definition*: Measured position of an aircraft in local polar co-ordinates.

*Structure*:

    **I018/014/RHO**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 8
    - unit: "NM"
    - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM
    - value :math:`< 256` NM

    **I018/014/THETA**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Note:
    - When expressed in 16 bits, signed or unsigned azimuths have the same value.

I018/015 - Aircraft Position in Cartesian Co-ordinates 
*******************************************************

*Definition*: Calculated position of an aircraft in Cartesian co-ordinates.

*Structure*:

    **I018/015/X** - *X-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`>= -256` NM
    - value :math:`<= 256` NM

    **I018/015/Y** - *Y-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`>= -256` NM
    - value :math:`<= 256` NM


Note:
    - Negative values are expressed in 2’s complement form, bit-32 and bit-16
      shall be set to 0 for positive values and 1 for negative values.

I018/016 - Packet Number
************************

*Definition*: Number used to correlate an uplink packet request and its associated acknowledgement.

*Structure*:

- 32 bits [``................................``]

- unsigned integer



I018/017 - Packet Number List
*****************************

*Definition*: List of numbers used to correlate an uplink packet request and their associated acknowledgements

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 32 bits [``................................``]

    - unsigned integer



I018/018 - Mode S Packet Properties
***********************************

*Definition*: Properties of an uplink Mode S packet, i.e. its internal priority and its capability to be
multiplexed or not, and its type (SVC, MSP or ROUTE).

*Structure*:

    **I018/018/(spare)**

    - 1 bit [``.``]

    **I018/018/PR** - *Mode S Packet Internal Priority*

    - 5 bits [``.....``]

    - unsigned integer

    **I018/018/PT** - *Packet Type*

    - 2 bits [``..``]

    - values:

        | 0: SVC packets
        | 1: MSP packets
        | 2: Route packets


Note:
    - The PT field is used to identify the ROUTE packets which have a higher
      priority than SVCs.  The PR field is used to describe the priority of SVCs as
      follows:

          0 = low
          1 = high

      For ROUTE and MSP packets the value of PR has no significance.  However,
      for the purpose of standardisation, it is recommended that for ROUTE
      packets, PR is set to 15 and for MSPs the PR should be set to 31.

I018/019 - Mode S Packet
************************

*Definition*: A Mode S packet as defined in the Mode S subnetwork SARPs.

*Structure*:

Explicit item



I018/020 - Broadcast Number
***************************

*Definition*: Number used to correlate an uplink broadcast request and its associated acknowledgement.

*Structure*:

- 32 bits [``................................``]

- unsigned integer



I018/021 - Broadcast Properties
*******************************

*Definition*: Properties of an uplink broadcast request (power, duration, coverage).

*Structure*:

    **I018/021/PRIORITY** - *Priority*

    - 4 bits [``....``]

    - unsigned integer

    **I018/021/POWER** - *Power*

    - 4 bits [``....``]

    - unsigned integer

    **I018/021/DURATION** - *Duration*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`1` s

    **I018/021/COVERAGE** - *Coverage*

    - 32 bits [``................................``]

    - raw value


Notes:
     1. A broadcast with a higher priority will temporarily delay a lower priority broadcast if
        necessary. The delayed broadcast will be resumed as soon as possible for its
        remaining time.
     2. The sectors are numbered from 1 to 32 clockwise, sector 1 being the first sector after the North.

I018/022 - Broadcast Prefix
***************************

*Definition*: Contents of the 32 first bits of an uplink broadcast interrogation.

*Structure*:

    **I018/022/(spare)**

    - 5 bits [``.....``]

    **I018/022/PREFIX** - *Prefix Field*

    - 27 bits [``...........................``]

    - raw value


Note:
    - The Mode S uplink broadcast interrogation will be made up of this
      D_Broadcast_prefix field followed by the D_Broadcast field and then by the
      Address/Parity field (in this order), as defined in ICAO Annex 10. In the
      interrogator, the 5 first bits of D_Broadcast_prefix will be replaced by ICAO
      UF field, as defined in Annex 10.

I018/023 - Uplink or Downlink Broadcast
***************************************

*Definition*: Broadcast message sent (MA field of the Comm-A frame) or received
(MB field of the Comm-B frame), conformant with the ICAO Manual on
Mode S Specific Services.

*Structure*:

- 56 bits [``........................................................``]

- raw value



I018/025 - GICB Number
**********************

*Definition*: Number used to correlate subsequent GICB messages (i.e. responses
and acknowledgements) with the original GICB request.

*Structure*:

- 32 bits [``................................``]

- unsigned integer



I018/027 - BDS Code
*******************

*Definition*: BDS code of the GICB to be extracted.

*Structure*:

- 8 bits [``........``]

- raw value



I018/028 - GICB Extraction Periodicity
**************************************

*Definition*: Periodicity of the GICB extractions.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 0
- unit: "s"
- LSB = :math:`1` s



I018/029 - GICB Extracted
*************************

*Definition*: GICB extracted message (MB field of the Comm-B frame). i.e., the
contents of a BDS register, conformant with the ICAO Manual on
Mode S Specific Services.

*Structure*:

- 56 bits [``........................................................``]

- BDS register (unknown)



I018/030 - GICB Properties
**************************

*Definition*: Properties of the GICB extractions.

*Structure*:

    **I018/030/PRIORITY** - *GICB Priority*

    - 5 bits [``.....``]

    - unsigned integer

    **I018/030/(spare)**

    - 3 bits [``...``]

    **I018/030/PC** - *Periodicity Constraint*

    - 1 bit [``.``]

    - values:

        | 0: The periodicity may not be strictly respected
        | 1: The periodicity shall be strictly respected

    **I018/030/AU** - *Asynchronous Update*

    - 1 bit [``.``]

    - values:

        | 0: GICB extractions should be sent only when required by the periodicity
        | 1: If a GICB extraction is done due to external conditions, an update will also be sent, even if it does not match the expected periodicity

    **I018/030/NE** - *Non Extraction*

    - 1 bit [``.``]

    - values:

        | 0: The GICB extraction is attempted according to the periodicity
        | 1: There will no GICB attempts

    **I018/030/RD** - *Reply Destination*

    - 2 bits [``..``]

    - values:

        | 0: The extracted GICB must be sent only on the Data Link line
        | 1: The extracted GICB must be sent only on the Surveillance line
        | 2: The extracted GICB must be sent both on the Data Link and on the Surveillance lines

    **I018/030/(spare)**

    - 3 bits [``...``]


Note:
    - The Non Extraction flag (NE) should be used only if the Asynchronous
      Update flag (AU) is set to true. It is specially reserved to the ACAS' RA
      extraction (asynchronous update without periodic extraction request).

I018/031 - Aircraft Identity
****************************

*Definition*: Identity of the aircraft extracted by a BDS 20 as described in ICAO Annex 10.

*Structure*:

- 48 bits [``................................................``]

- raw value



I018/032 - Aircraft Mode A
**************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I018/032/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I018/032/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I018/032/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Mode-3/A code not extracted during the last scan

    **I018/032/(spare)**

    - 1 bit [``.``]

    **I018/032/MOD3A**

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Note:
    - Bit 15 has no meaning in the case of a smoothed Mode-3/A code and is set
      to 0 for a calculated track. For Mode S, it is set to one when an error
      correction has been attempted.

I018/033 - Aircraft Height
**************************

*Definition*: Flight Level converted into binary representation.

*Structure*:

    **I018/033/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I018/033/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I018/033/FL** - *Flight Level*

    - 14 bits [``..............``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL


Notes:
    1. The value shall be within the range described by ICAO Annex 10
    2. For Mode S, bit 15 (G) is set to one when an error correction
       has been attempted.
    3. If Altitude is not extracted on the last scan, it is an implementation
       issue as to whether Altitude is output from track file, if at all.

I018/034 - Aircraft Speed
*************************

*Definition*: Tracker calculated Ground Speed of an aircraft.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 14
- unit: "NM/s"
- LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s



I018/035 - Aircraft Heading
***************************

*Definition*: Tracker calculated heading of an aircraft. . The heading is the heading with respect
to the geographical north at the aircraft position.

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 360
- fractional bits: 16
- unit: "deg"
- LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg



I018/036 - Data Source Identifier
*********************************

*Definition*: Identification of the source node for the GDLP/LU data

*Structure*:

    **I018/036/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I018/036/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I018/037 - Data Destination Identifier
**************************************

*Definition*: Identification of the destination node for the GDLP/LU data.

*Structure*:

    **I018/037/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I018/037/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

User Application Profile for Category 018
=========================================
- (1) ``I018/036`` - Data Source Identifier
- (2) ``I018/037`` - Data Destination Identifier
- (3) ``I018/000`` - Message Type
- (4) ``I018/001`` - Result
- (5) ``I018/005`` - Mode S Address
- (6) ``I018/016`` - Packet Number
- (7) ``I018/017`` - Packet Number List
- ``(FX)`` - Field extension indicator
- (8) ``I018/018`` - Mode S Packet Properties
- (9) ``I018/019`` - Mode S Packet
- (10) ``I018/028`` - GICB Extraction Periodicity
- (11) ``I018/030`` - GICB Properties
- (12) ``I018/025`` - GICB Number
- (13) ``I018/027`` - BDS Code
- (14) ``I018/029`` - GICB Extracted
- ``(FX)`` - Field extension indicator
- (15) ``I018/002`` - Time of Day
- (16) ``I018/006`` - Mode S Address List
- (17) ``I018/007`` - Aircraft Data Link Command
- (18) ``I018/008`` - Aircraft Data Link Status
- (19) ``I018/009`` - Aircraft Data Link Report Request
- (20) ``I018/010`` - Transponder Communications Capability
- (21) ``I018/011`` - Capability Report
- ``(FX)`` - Field extension indicator
- (22) ``I018/014`` - Aircraft Position in Polar Co-ordinates
- (23) ``I018/015`` - Aircraft Position in Cartesian Co-ordinates
- (24) ``I018/020`` - Broadcast Number
- (25) ``I018/021`` - Broadcast Properties
- (26) ``I018/022`` - Broadcast Prefix
- (27) ``I018/023`` - Uplink or Downlink Broadcast
- (28) ``I018/004`` - II Code
- ``(FX)`` - Field extension indicator
- (29) ``I018/031`` - Aircraft Identity
- (30) ``I018/032`` - Aircraft Mode A
- (31) ``I018/033`` - Aircraft Height
- (32) ``I018/034`` - Aircraft Speed
- (33) ``I018/035`` - Aircraft Heading
- (34) ``I018/012`` - Aircraft Coverage Quality Factor
- (35) ``I018/013`` -  Aircraft CQF Calculation Method
- ``(FX)`` - Field extension indicator

