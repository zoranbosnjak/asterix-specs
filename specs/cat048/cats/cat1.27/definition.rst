Asterix category 048 - Monoradar Target Reports
===============================================
**category**: 048

**edition**: 1.27

**date**: 2020-06-18

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I048/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data is received.

*Structure*:

    **I048/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I048/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I048/020 - Target Report Descriptor
***********************************

*Definition*: Type and properties of the target report.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I048/020/TYP**

    - 3 bits [``...``]

    - values:

        | 0: No detection
        | 1: Single PSR detection
        | 2: Single SSR detection
        | 3: SSR + PSR detection
        | 4: Single ModeS All-Call
        | 5: Single ModeS Roll-Call
        | 6: ModeS All-Call + PSR
        | 7: ModeS Roll-Call +PSR

    **I048/020/SIM**

    - 1 bit [``.``]

    - values:

        | 0: Actual target report
        | 1: Simulated target report

    **I048/020/RDP**

    - 1 bit [``.``]

    - values:

        | 0: Report from RDP Chain 1
        | 1: Report from RDP Chain 2

    **I048/020/SPI**

    - 1 bit [``.``]

    - values:

        | 0: Absence of SPI
        | 1: Special Position Identification

    **I048/020/RAB**

    - 1 bit [``.``]

    - values:

        | 0: Report from aircraft transponder
        | 1: Report from field monitor (fixed transponder)

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I048/020/TST**

    - 1 bit [``.``]

    - values:

        | 0: Real target report
        | 1: Test target report

    **I048/020/ERR**

    - 1 bit [``.``]

    - values:

        | 0: No Extended Range
        | 1: Extended Range present

    **I048/020/XPP**

    - 1 bit [``.``]

    - values:

        | 0: No X-Pulse present
        | 1: X-Pulse present

    **I048/020/ME**

    - 1 bit [``.``]

    - values:

        | 0: No military emergency
        | 1: Military emergency

    **I048/020/MI**

    - 1 bit [``.``]

    - values:

        | 0: No military identification
        | 1: Military identification

    **I048/020/FOEFRI**

    - 2 bits [``..``]

    - values:

        | 0: No Mode 4 interrogation
        | 1: Friendly target
        | 2: Unknown target
        | 3: No reply

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. For Mode S aircraft, the SPI information is also contained in I048/230.
    2. To bits 3/2 (FOE/FRI): IFF interrogators supporting a three level
       classification of the processing of the Mode 4 interrogation result
       shall encode the detailed response information in data item M4E of
       the Reserved Expansion Field of category 048. In this case the value
       for FOE/FRI in I048/020 shall be set to “00”.
       However, even those interrogators shall use I048/020 to encode the information “No reply”.
    3. To bit 6 (XPP): This bit shall always be set when the X-pulse has
       been extracted, independent from the Mode it was extracted with.
    4. To bit 7 (ERR): This bit set to “1” indicates that the range of the
       target is beyond the maximum range in data item I048/040.In this
       case – and this case only - the ERR Data Item in the Reserved
       Expansion Field shall provide the range value of the Measured
       Position in Polar Coordinates.

I048/030 - Warning/Error Conditions and Target Classification
*************************************************************

*Definition*: Warning/error conditions detected by a radar station for the target report
involved. Target Classification information for the target involved.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I048/030/CODE**

    - 7 bits [``.......``]

    - values:

        | 0: Not defined; never used
        | 1: Multipath Reply (Reflection)
        | 2: Reply due to sidelobe interrogation/reception
        | 3: Split plot
        | 4: Second time around reply
        | 5: Angel
        | 6: Slow moving target correlated with road infrastructure (terrestrial vehicle)
        | 7: Fixed PSR plot
        | 8: Slow PSR target
        | 9: Low quality PSR plot
        | 10: Phantom SSR plot
        | 11: Non-Matching Mode-3/A Code
        | 12: Mode C code / Mode S altitude code abnormal value compared to the track
        | 13: Target in Clutter Area
        | 14: Maximum Doppler Response in Zero Filter
        | 15: Transponder anomaly detected
        | 16: Duplicated or Illegal Mode S Aircraft Address
        | 17: Mode S error correction applied
        | 18: Undecodable Mode C code / Mode S altitude code
        | 19: Birds
        | 20: Flock of Birds
        | 21: Mode-1 was present in original reply
        | 22: Mode-2 was present in original reply
        | 23: Plot potentially caused by Wind Turbine
        | 24: Helicopter
        | 25: Maximum number of re-interrogations reached (surveillance information)
        | 26: Maximum number of re-interrogations reached (BDS Extractions)
        | 27: BDS Overlay Incoherence
        | 28: Potential BDS Swap Detected
        | 29: Track Update in the Zenithal Gap
        | 30: Mode S Track re-acquired
        | 31: Duplicated Mode 5 Pair NO/PIN detected

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. It has to be stressed that a series of one or more codes can
       be reported per target report.
    2. Data conveyed in this item are of secondary importance, and
       can generally also be derived from the processing of mandatory items.
    3. Definitions can be found in SUR.ET1.ST03.1000-STD-01-01 Radar
       Sensor Performance Analysis.
    4. Values 25 to 30 have been defined to comply with the updated
       European Mode S Specification (EMS) and to provide the possibility
       to report the following information:

           - Code 25: the maximum number of permitted re-interrogations to
             acquire the surveillance information has been reached;
           - Code 26: the maximum number of permitted re-interrogations to
             extract BDS Registers has been reached;
           - Code 27: inconsistency detected between the contents of the
             message and the BDS register overlayed;
           - Code 28: a BDS swap has been detected and the respective information
             has been discarded;
           - Code 29: the track has been updated while being in the zenithal
             gap (also referred to as “Cone of Silence”);
           - Code 30: the radar had lost track of an aircraft and subsequently
             re-acquired it.

I048/040 - Measured Position in Polar Co-ordinates
**************************************************

*Definition*: Measured position of an aircraft in local polar co-ordinates.

*Structure*:

    **I048/040/RHO**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 8
    - unit: "NM"
    - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM
    - value :math:`< 256` NM

    **I048/040/THETA**

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Notes:

    1. In case of no detection, the extrapolated position expressed in slant
       polar co-ordinates may be sent, except for a track cancellation message.
       No detection is signalled by the TYP field set to zero in I048/020
       Target Report Descriptor.
    2. This item represents the measured target position of the plot, even
       if associated with a track, for the present antenna scan. It is
       expressed in polar co-ordinates in the local reference system,
       centred on the radar station.
    3. In case of combined detection by a PSR and an SSR, then the SSR
       position is sent.
    4. For targets having a range beyond the maximum range the data item
       “Extended Range Report” has been added to the Reserved Expansion
       Field of category 048. The presence of this data item is indicated
       by the ERR bit set to one in data item I048/020, first extension.
       The ERR data item shall only be sent if the value of RHO is equal
       to or greater than 256NM.
       Please note that if this data item is used, the Encoding Rule to
       data item I048/040 still applies, meaning that the extra item in
       the Reserved Expansion Field shall be transmitted in addition to
       data item I048/040.
       If the Extended Range Report item in the Reserved Expansion Field
       is used, it is recommended to set the value of RHO in data item
       I048/040 to its maximum, meaning bits 32/17 all set to 1.

I048/042 - Calculated Position in Cartesian Co-ordinates
********************************************************

*Definition*: Calculated position of an aircraft in Cartesian co-ordinates.

*Structure*:

    **I048/042/X** - *X-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`>= -256` NM
    - value :math:`<= 256` NM

    **I048/042/Y** - *X-Component*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM
    - value :math:`>= -256` NM
    - value :math:`<= 256` NM



I048/050 - Mode-2 Code in Octal Representation
**********************************************

*Definition*: Reply to Mode-2 interrogation.

*Structure*:

    **I048/050/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I048/050/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I048/050/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-2 code as derived from the reply of the transponder
        | 1: Smoothed Mode-2 code as provided by a local tracker

    **I048/050/(spare)**

    - 1 bit [``.``]

    **I048/050/MODE2** - *Mode-2 Code in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Notes:

    - Bit 15 has no meaning in the case of a smoothed Mode-2 and is set
      to 0 for a calculated track.

I048/055 - Mode-1 Code in Octal Representation
**********************************************

*Definition*: Reply to Mode-1 interrogation.

*Structure*:

    **I048/055/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I048/055/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I048/055/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-1 code as derived from the reply of the transponder
        | 1: Smoothed Mode-1 code as provided by a local tracker

    **I048/055/MODE1** - *Mode-1 Code*

    - 5 bits [``.....``]

    - raw value


Notes:

    1. Bit 7 has no meaning in the case of a smoothed Mode-1 and is set
       to 0 for a calculated track.
    2. The values of the bits for V, G, L, A4, A2, A1, B2 and B1 shall be
       identical to the values of the corresponding bits in subfield #5
       of data item “MD5 – Mode 5 Reports” and in subfield #5 of data
       item “MD5 – Mode 5 Reports, New Format” in the Reserved Expansion Field.

I048/060 - Mode-2 Code Confidence Indicator
*******************************************

*Definition*: Confidence level for each bit of a Mode-2 reply as provided by a monopulse SSR station.

*Structure*:

    **I048/060/(spare)**

    - 4 bits [``....``]

    **I048/060/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I048/060/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I048/060/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I048/060/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I048/060/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I048/060/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I048/060/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I048/060/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I048/060/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I048/060/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4

    **I048/060/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I048/060/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1



I048/065 - Mode-1 Code Confidence Indicator
*******************************************

*Definition*: Confidence level for each bit of a Mode-1 reply as provided by a monopulse SSR station.

*Structure*:

    **I048/065/(spare)**

    - 3 bits [``...``]

    **I048/065/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I048/065/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I048/065/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I048/065/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I048/065/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1



I048/070 - Mode-3/A Code in Octal Representation
************************************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I048/070/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I048/070/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I048/070/L**

    - 1 bit [``.``]

    - values:

        | 0: Mode-3/A code derived from the reply of the transponder
        | 1: Mode-3/A code not extracted during the last scan

    **I048/070/(spare)**

    - 1 bit [``.``]

    **I048/070/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)


Notes:

    1. Bit 15 has no meaning in the case of a smoothed Mode-3/A code and
       is set to 0 for a calculated track. For Mode S, it is set to one
       when an error correction has been attempted.
    2. For Mode S, bit 16 is normally set to zero, but can exceptionally
       be set to one to indicate a non-validated Mode-3/A code (e.g. alert
       condition detected, but new Mode-3/A code not successfully extracted).

I048/080 - Mode-3/A Code Confidence Indicator
*********************************************

*Definition*: Confidence level for each bit of a Mode-3/A reply as provided by a monopulse SSR station.

*Structure*:

    **I048/080/(spare)**

    - 4 bits [``....``]

    **I048/080/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I048/080/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I048/080/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I048/080/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I048/080/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I048/080/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I048/080/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I048/080/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I048/080/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I048/080/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4

    **I048/080/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I048/080/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1



I048/090 - Flight Level in Binary Representation
************************************************

*Definition*: Flight Level converted into binary representation.

*Structure*:

    **I048/090/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I048/090/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I048/090/FL**

    - 14 bits [``..............``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "FL"
    - LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL


Notes:

    1. When Mode C code / Mode S altitude code is present but not decodable,
       the “Undecodable Mode C code / Mode S altitude code” Warning/Error
       should be sent in I048/030.
    2. When local tracking is applied and the received Mode C code / Mode S
       altitude code corresponds to an abnormal value (the variation with
       the previous plot is estimated too important by the tracker),
       the “Mode C code / Mode S altitude code abnormal value compared
       to the track“ Warning/Error should be sent in I048/030.
    3. The value shall be within the range described by ICAO Annex 10
    4. For Mode S, bit 15 (G) is set to one when an error correction has
       been attempted.

I048/100 - Mode-C Code and Code Confidence Indicator
****************************************************

*Definition*: Mode-C height in Gray notation as received from the transponder together
with the confidence level for each reply bit as provided by a MSSR/Mode S station.

*Structure*:

    **I048/100/V**

    - 1 bit [``.``]

    - values:

        | 0: Code validated
        | 1: Code not validated

    **I048/100/G**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Garbled code

    **I048/100/(spare)**

    - 2 bits [``..``]

    **I048/100/MODEC** - *Mode-C Reply in Gray Notation*

    - 12 bits [``............``]

    - raw value

    **I048/100/(spare)**

    - 4 bits [``....``]

    **I048/100/QC1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C1
        | 1: Low quality pulse C1

    **I048/100/QA1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A1
        | 1: Low quality pulse A1

    **I048/100/QC2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C2
        | 1: Low quality pulse C2

    **I048/100/QA2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A2
        | 1: Low quality pulse A2

    **I048/100/QC4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse C4
        | 1: Low quality pulse C4

    **I048/100/QA4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse A4
        | 1: Low quality pulse A4

    **I048/100/QB1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B1
        | 1: Low quality pulse B1

    **I048/100/QD1**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D1
        | 1: Low quality pulse D1

    **I048/100/QB2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B2
        | 1: Low quality pulse B2

    **I048/100/QD2**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D2
        | 1: Low quality pulse D2

    **I048/100/QB4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse B4
        | 1: Low quality pulse B4

    **I048/100/QD4**

    - 1 bit [``.``]

    - values:

        | 0: High quality pulse D4
        | 1: Low quality pulse D4


Notes:

    1. For Mode S, D1 is also designated as Q, and is used to denote either
       25ft or 100ft reporting.
    2. For Mode S, bit-31 (G) is set when an error correction has been attempted.

I048/110 - Height Measured by a 3D Radar
****************************************

*Definition*: Height of a target as measured by a 3D radar. The height shall use mean
sea level as the zero reference level.

*Structure*:

    **I048/110/(spare)**

    - 2 bits [``..``]

    **I048/110/3DH** - *3D Height, in Binary Notation. Negative Values Are Expressed in Two's Complement*

    - 14 bits [``..............``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 0
    - unit: "ft"
    - LSB = :math:`25` ft



I048/120 - Radial Doppler Speed
*******************************

*Definition*: Information on the Doppler Speed of the target report.

*Structure*:

Compound item (FX)

    **I048/120/CAL** - *Calculated Doppler Speed*

        **I048/120/CAL/D**

        - 1 bit [``.``]

        - values:

            | 0: Doppler speed is valid
            | 1: Doppler speed is doubtful

        **I048/120/CAL/(spare)**

        - 5 bits [``.....``]

        **I048/120/CAL/CAL** - *Calculated Doppler Speed, Coded in Two's Complement*

        - 10 bits [``..........``]

        - signed quantity
        - scaling factor: 1
        - fractional bits: 0
        - unit: "m/s"
        - LSB = :math:`1` m/s

    **I048/120/RDS** - *Raw Doppler Speed*

    Repetitive item, repetition factor 8 bits.

            **I048/120/RDS/DOP** - *Doppler Speed*

            - 16 bits [``................``]

            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 0
            - unit: "m/s"
            - LSB = :math:`1` m/s

            **I048/120/RDS/AMB** - *Ambiguity Range*

            - 16 bits [``................``]

            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 0
            - unit: "m/s"
            - LSB = :math:`1` m/s

            **I048/120/RDS/FRQ** - *Transmitter Frequency*

            - 16 bits [``................``]

            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 0
            - unit: "Mhz"
            - LSB = :math:`1` Mhz



I048/130 - Radar Plot Characteristics
*************************************

*Definition*: Additional information on the quality of the target report.

*Structure*:

Compound item (FX)

    **I048/130/SRL** - *SSR Plot Runlength*

    SSR plot runlength, expressed as a positive binary value.

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 13
    - unit: "dg"
    - LSB = :math:`360 / {2^{13}}` dg = :math:`360 / {8192}` dg :math:`\approx 0.0439453125` dg

    **I048/130/SRR** - *Number of Received Replies for (M)SSR*

    Number of Received Replies for (M)SSR

    - 8 bits [``........``]

    - unsigned integer

    **I048/130/SAM** - *Amplitude of (M)SSR Reply*

    Amplitude of (M)SSR Reply

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "dBm"
    - LSB = :math:`1` dBm

    **I048/130/PRL** - *Primary Plot Runlength*

    Primary Plot Runlength, expressed as positive binary value

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 13
    - unit: "dg"
    - LSB = :math:`360 / {2^{13}}` dg = :math:`360 / {8192}` dg :math:`\approx 0.0439453125` dg

    **I048/130/PAM** - *Amplitude of Primary Plot*

    Amplitude of Primary Plot

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "dBm"
    - LSB = :math:`1` dBm

    **I048/130/RPD** - *Difference in Range Between PSR and SSR Plot*

    Range (PSR-SSR)

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 8
    - unit: "NM"
    - LSB = :math:`1 / {2^{8}}` NM = :math:`1 / {256}` NM :math:`\approx 0.00390625` NM

    **I048/130/APD** - *Difference in Azimuth Between PSR and SSR Plot*

    Azimuth (PSR-SSR)

    - 8 bits [``........``]

    - signed quantity
    - scaling factor: 360
    - fractional bits: 14
    - unit: "dg"
    - LSB = :math:`360 / {2^{14}}` dg = :math:`360 / {16384}` dg :math:`\approx 0.02197265625` dg


Notes:

    1. The total range covered is therefore from 0 to 11.21 dg.
    2. Negative values are coded in two's complement form.
    3. The total range covered is therefore from 0 to 11.21 dg.
    4. Negative values are coded in two's complement form.
    5. Negative values are coded in two's complement form.
    6. The covered range difference is +/- 0.5 NM.
    7. Sending the maximum value means that the difference in range
       is equal or greater than the maximum value.
    8. Negative values are coded in two's complement form.
    9. The covered azimuth difference is +/-360/2 7 = +/- 2.8125 dg.
    10. Sending the maximum value means that the difference in range
        is equal or greater than the maximum value.

I048/140 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as Co-ordinated Universal Time (UTC).

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s
- value :math:`< 86400` s


Notes:

    1. The time of day value is reset to 0 each day at midnight.
    2. Every radar station using ASTERIX should be equipped with at least
       one synchronised time source

I048/161 - Track Number
***********************

*Definition*: An integer value representing a unique reference to a track record within
a particular track file.

*Structure*:

    **I048/161/(spare)**

    - 4 bits [``....``]

    **I048/161/TRN** - *Track Number*

    - 12 bits [``............``]

    - raw value



I048/170 - Track Status
***********************

*Definition*: Status of monoradar track (PSR and/or SSR updated).

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I048/170/CNF** - *Confirmed Vs. Tentative Track*

    - 1 bit [``.``]

    - values:

        | 0: Confirmed Track
        | 1: Tentative Track

    **I048/170/RAD** - *Type of Sensor(s) Maintaining Track*

    - 2 bits [``..``]

    - values:

        | 0: Combined Track
        | 1: PSR Track
        | 2: SSR/Mode S Track
        | 3: Invalid

    **I048/170/DOU** - *Signals Level of Confidence in Plot to Track Association Process*

    - 1 bit [``.``]

    - values:

        | 0: Normal confidence
        | 1: Low confidence in plot to track association

    **I048/170/MAH** - *Manoeuvre Detection in Horizontal Sense*

    - 1 bit [``.``]

    - values:

        | 0: No horizontal man.sensed
        | 1: Horizontal man. sensed

    **I048/170/CDM** - *Climbing / Descending Mode*

    - 2 bits [``..``]

    - values:

        | 0: Maintaining
        | 1: Climbing
        | 2: Descending
        | 3: Unknown

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I048/170/TRE** - *Signal for End_of_Track*

    - 1 bit [``.``]

    - values:

        | 0: Track still alive
        | 1: End of track lifetime(last report for this track)

    **I048/170/GHO** - *Ghost Vs. True Target*

    - 1 bit [``.``]

    - values:

        | 0: True target track
        | 1: Ghost target track

    **I048/170/SUP** - *Track Maintained with Track Information from Neighbouring Node B on the Cluster, or Network*

    - 1 bit [``.``]

    - values:

        | 0: No
        | 1: Yes

    **I048/170/TCC** - *Type of Plot Coordinate Transformation Mechanism:*

    - 1 bit [``.``]

    - values:

        | 0: Tracking performed in so-called 'Radar Plane', i.e. neither slant range correction nor stereographical projection was applied
        | 1: Slant range correction and a suitable projection technique are used to track in a 2D.reference plane, tangential to the earth model at the Radar Site co-ordinates

    **I048/170/(spare)**

    - 3 bits [``...``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I048/200 - Calculated Track Velocity in Polar Co-ordinates
**********************************************************

*Definition*: Calculated track velocity expressed in polar co-ordinates.

*Structure*:

    **I048/200/GSP** - *Calculated Groundspeed*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 14
    - unit: "NM/s"
    - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s

    **I048/200/HDG** - *Calculated Heading*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Notes:

    - The calculated heading is related to the geographical North at the
      aircraft position.

I048/210 - Track Quality
************************

*Definition*: Track quality in the form of a vector of standard deviations.

*Structure*:

    **I048/210/SIGX** - *Sigma (X)) Standard Deviation on the Horizontal Axis of the Local Grid System*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM

    **I048/210/SIGY** - *Sigma (Y)) Standard Deviation on the Vertical Axis of the Local Grid System*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM

    **I048/210/SIGV** - *Sigma (V)) Standard Deviation on the Groundspeed Within the Local Grid System*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 14
    - unit: "NM/s"
    - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s

    **I048/210/SIGH** - *Sigma (H)) Standard Deviation on the Heading Within the Local Grid System*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 12
    - unit: "deg"
    - LSB = :math:`360 / {2^{12}}` deg = :math:`360 / {4096}` deg :math:`\approx 0.087890625` deg


Notes:

    1. The standard deviation is per definition a positive value, hence
       the range covered is : 0<= Sigma(X)<2 NM
    2. The standard deviation is per definition a positive value, hence
       the range covered is : 0<= Sigma(Y)<2 NM
    3. The standard deviation is per definition a positive value, hence
       the range covered is: 0<=Sigma (V)<56.25 Kt
    4. The standard deviation is per definition a positive value; hence
       the range covered is: 0 <= sigma (H) < 22.5 degrees.

I048/220 - Aircraft Address
***************************

*Definition*: Aircraft address (24-bits Mode S address) assigned uniquely to each aircraft.

*Structure*:

- 24 bits [``........................``]

- raw value



I048/230 - Communications/ACAS Capability and Flight Status
***********************************************************

*Definition*: Communications capability of the transponder, capability of the on-board
ACAS equipment and flight status.

*Structure*:

    **I048/230/COM** - *Communications Capability of the Transponder*

    - 3 bits [``...``]

    - values:

        | 0: No communications capability (surveillance only)
        | 1: Comm. A and Comm. B capability
        | 2: Comm. A, Comm. B and Uplink ELM
        | 3: Comm. A, Comm. B, Uplink ELM and Downlink ELM
        | 4: Level 5 Transponder capability

    **I048/230/STAT** - *Flight Status*

    - 3 bits [``...``]

    - values:

        | 0: No alert, no SPI, aircraft airborne
        | 1: No alert, no SPI, aircraft on ground
        | 2: Alert, no SPI, aircraft airborne
        | 3: Alert, no SPI, aircraft on ground
        | 4: Alert, SPI, aircraft airborne or on ground
        | 5: No alert, SPI, aircraft airborne or on ground
        | 7: Unknown

    **I048/230/SI** - *SI/II Transponder Capability*

    - 1 bit [``.``]

    - values:

        | 0: SI-Code Capable
        | 1: II-Code Capable

    **I048/230/(spare)**

    - 1 bit [``.``]

    **I048/230/MSSC** - *Mode-S Specific Service Capability*

    - 1 bit [``.``]

    - values:

        | 0: No
        | 1: Yes

    **I048/230/ARC** - *Altitude Reporting Capability*

    - 1 bit [``.``]

    - values:

        | 0: 100 ft resolution
        | 1: 25 ft resolution

    **I048/230/AIC** - *Aircraft Identification Capability*

    - 1 bit [``.``]

    - values:

        | 0: No
        | 1: Yes

    **I048/230/B1A** - *BDS 1,0 Bit 16*

    - 1 bit [``.``]

    - raw value

    **I048/230/B1B** - *BDS 1,0 Bits 37/40*

    - 4 bits [``....``]

    - raw value



I048/240 - Aircraft Identification
**********************************

*Definition*: Aircraft identification (in 8 characters) obtained from an aircraft
equipped with a Mode S transponder.

*Structure*:

- 48 bits [``................................................``]

- ICAO string (6-bits per character)


Notes:

    - This data item contains the flight identification as available in
      the respective Mode S transponder registers.

I048/250 - Mode S MB Data
*************************

*Definition*: Mode S Comm B data as extracted from the aircraft transponder.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I048/250/MBDATA** - *Mode S Comm B Message Data*

        - 56 bits [``........................................................``]

        - raw value

        **I048/250/BDS1** - *Comm B Data Buffer Store 1 Address*

        - 4 bits [``....``]

        - raw value

        **I048/250/BDS2** - *Comm B Data Buffer Store 2 Address*

        - 4 bits [``....``]

        - raw value


Notes:

    1. For the transmission of BDS20, item 240 is used.
    2. For the transmission of BDS30, item 260 is used.
    3. In case of data extracted via Comm-B broadcast, all bits of fields
       BDS1 and BDS2 are set to 0; in case of data extracted via GICB
       requests, the fields BDS1 and BDS2 correspond to the GICB register number.

I048/260 - ACAS Resolution Advisory Report
******************************************

*Definition*: Currently active Resolution Advisory (RA), if any, generated by the ACAS
associated with the transponder transmitting the report and threat identity data.

*Structure*:

- 56 bits [``........................................................``]

- raw value


Notes:

    - Refer to ICAO Draft SARPs for ACAS for detailed explanations.

I048/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I048/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 048
=========================================
- (1) ``I048/010`` - Data Source Identifier
- (2) ``I048/140`` - Time of Day
- (3) ``I048/020`` - Target Report Descriptor
- (4) ``I048/040`` - Measured Position in Polar Co-ordinates
- (5) ``I048/070`` - Mode-3/A Code in Octal Representation
- (6) ``I048/090`` - Flight Level in Binary Representation
- (7) ``I048/130`` - Radar Plot Characteristics
- ``(FX)`` - Field extension indicator
- (8) ``I048/220`` - Aircraft Address
- (9) ``I048/240`` - Aircraft Identification
- (10) ``I048/250`` - Mode S MB Data
- (11) ``I048/161`` - Track Number
- (12) ``I048/042`` - Calculated Position in Cartesian Co-ordinates
- (13) ``I048/200`` - Calculated Track Velocity in Polar Co-ordinates
- (14) ``I048/170`` - Track Status
- ``(FX)`` - Field extension indicator
- (15) ``I048/210`` - Track Quality
- (16) ``I048/030`` - Warning/Error Conditions and Target Classification
- (17) ``I048/080`` - Mode-3/A Code Confidence Indicator
- (18) ``I048/100`` - Mode-C Code and Code Confidence Indicator
- (19) ``I048/110`` - Height Measured by a 3D Radar
- (20) ``I048/120`` - Radial Doppler Speed
- (21) ``I048/230`` - Communications/ACAS Capability and Flight Status
- ``(FX)`` - Field extension indicator
- (22) ``I048/260`` - ACAS Resolution Advisory Report
- (23) ``I048/055`` - Mode-1 Code in Octal Representation
- (24) ``I048/050`` - Mode-2 Code in Octal Representation
- (25) ``I048/065`` - Mode-1 Code Confidence Indicator
- (26) ``I048/060`` - Mode-2 Code Confidence Indicator
- (27) ``I048/SP`` - Special Purpose Field
- (28) ``I048/RE`` - Reserved Expansion Field
- ``(FX)`` - Field extension indicator

