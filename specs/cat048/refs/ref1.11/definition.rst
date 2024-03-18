Asterix expansion 048 - Monoradar Target Reports Appendix A: Reserved Expansion Field
=====================================================================================
**category**: 048

**edition**: 1.11

**date**: 2022-12-07

Description of asterix expansion
--------------------------------
Compound item (fspec=8 bits)

    **I048/MD5** - *Mode 5 Reports*

    Compound item (FX)

        **I048/MD5/SUM** - *Mode 5 Summary*

            **I048/MD5/SUM/M5**

            - 1 bit [``.``]

            - values:

                | 0: No Mode 5 interrogation
                | 1: Mode 5 interrogation

            **I048/MD5/SUM/ID**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 ID reply/report
                | 1: Authenticated Mode 5 ID reply/report

            **I048/MD5/SUM/DA**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 Data reply/report
                | 1: Authenticated Mode 5 Data reply/report (i.e any valid Mode 5 reply type other than ID)

            **I048/MD5/SUM/M1**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 code not present or not from Mode 5 reply/report
                | 1: Mode 1 code from Mode 5 reply/report

            **I048/MD5/SUM/M2**

            - 1 bit [``.``]

            - values:

                | 0: Mode 2 code not present or not from Mode 5 reply/report
                | 1: Mode 2 code from Mode 5 reply/report

            **I048/MD5/SUM/M3**

            - 1 bit [``.``]

            - values:

                | 0: Mode 3 code not present or not from Mode 5 reply/report
                | 1: Mode 3 code from Mode 5 reply/report

            **I048/MD5/SUM/MC**

            - 1 bit [``.``]

            - values:

                | 0: Mode C altitude not present or not from Mode 5 reply/report
                | 1: Mode C altitude from Mode 5 reply/report

            **I048/MD5/SUM/(spare)**

            - 1 bit [``.``]

            remark
                Notes:
                    1. The flags M2, M3, MC refer to the contents of data items I048/050, I048/070
                       and I048/090 respectively. The flag M1 refers to the contents of data item
                       I048/055, Mode 1 Code in Octal Representation, and to the contents of the
                       Subitem #5 (Extended Mode 1 Code in Octal Representation).
                    2. If an authenticated Mode 5 reply/report is received with the Emergency bit set,
                       then the Military Emergency bit (ME) in Data Item I048/020, Target Report
                       Descriptor, shall be set.
                    3. If an authenticated Mode 5 reply/report is received with the Identification of
                       Position bit set, then the Special Position Identification bit (SPI) in Data Item
                       I048/020, Target Report Descriptor, shall be set.

        **I048/MD5/PMN** - *PIN/ National Origin/Mission Code*

            **I048/MD5/PMN/(spare)**

            - 2 bits [``..``]

            **I048/MD5/PMN/PIN** - *PIN Code*

            - 14 bits [``..............``]

            - raw value

            **I048/MD5/PMN/(spare)**

            - 2 bits [``..``]

            **I048/MD5/PMN/NAV** - *Validity of NAT*

            - 1 bit [``.``]

            - values:

                | 0: National Origin is valid
                | 1: National Origin is invalid

            **I048/MD5/PMN/NAT** - *National Origin*

            - 5 bits [``.....``]

            - raw value

            **I048/MD5/PMN/(spare)**

            - 2 bits [``..``]

            **I048/MD5/PMN/MIS** - *Mission Code*

            - 6 bits [``......``]

            - raw value

            remark
                Note:
                    Bit 14 (NAV) is set to 1 if the value for National Origin is not known or invalid.
                    Under certain conditions PIN is available but NAT is not available. NAV then
                    indicates that the NAT field was not actively populated.

        **I048/MD5/POS** - *Mode 5 Reported Position*

            **I048/MD5/POS/LAT** - *Latitude in WGS 84*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -90/1` °
            - value :math:`<= 90/1` °

            **I048/MD5/POS/LON** - *Longitude in WGS 84*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -180/1` °
            - value :math:`<= 180/1` °

            remark
                Notes:
                    Latitude in WGS 84 is expressed as a 24-bit two’s complement number.
                    Range -90° ≤ latitude ≤ 90°. Sign convention: North is positive.
                    LSB = 180/223 degrees = 2.145767*10-05 degrees.

                    Longitude in WGS 84 is expressed as a 24-bit two’s complement number.
                    Range -180° ≤ longitude < 180°. Sign convention: East is positive.
                    LSB = 180/223 degrees = 2.145767*10-05 degrees.

                    The resolution implied by the LSB is better than the resolution with which Mode 5
                    position reports are transmitted from aircraft transponders using currently defined
                    formats.

        **I048/MD5/GA** - *Mode 5 GNSS-derived Altitude*

            **I048/MD5/GA/(spare)**

            - 1 bit [``.``]

            **I048/MD5/GA/RES**

            "Resolution with which the GNSS-derived Altitude (GA) is reported"

            - 1 bit [``.``]

            - values:

                | 0: GA reported in 100 ft increments
                | 1: GA reported in 25 ft increments

            **I048/MD5/GA/GA**

            "GNSS-derived Altitude of target, expressed as height above WGS 84 ellipsoid"

            - 14 bits [``..............``]

            - signed quantity
            - unit: "ft"
            - LSB = :math:`25` ft
            - value :math:`>= -1000` ft

        **I048/MD5/EM1** - *Extended Mode 1 Code in Octal Representation*

            **I048/MD5/EM1/V**

            - 1 bit [``.``]

            - values:

                | 0: Code not validated
                | 1: Code validated

            **I048/MD5/EM1/G**

            - 1 bit [``.``]

            - values:

                | 0: Default
                | 1: Garbled code

            **I048/MD5/EM1/L**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 Code derived from the reply of the transponder
                | 1: Mode 1 Code not extracted during the last scan

            **I048/MD5/EM1/(spare)**

            - 1 bit [``.``]

            **I048/MD5/EM1/EM1** - *Extended Mode 1 Code in Octal Representation*

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            remark
                Notes:
                    1. If Subitem #1 is present, the M1 bit in Subitem #1 indicates whether the
                       Extended Mode 1 Code is from a Mode 5 reply or a Mode 1 reply. If Subitem
                       #1 is not present, the Extended Mode 1 Code is from a Mode 1 reply.
                    2. For reasons of backwards compatibility the logic for the setting of the V-bit
                       was inverted compared to other similar data items.
                    3. The values of the bits for V, G, L, A4, A2, A1, B2 and B1 shall be identical to
                       the values of the corresponding bits in data item I048/055.

        **I048/MD5/TOS** - *Time Offset for POS and GA*

        - 8 bits [``........``]

        - unsigned quantity
        - unit: "s"
        - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

            remark
                Note:
                    TOS shall be assumed to be zero if Subitem #6 is not present.

        **I048/MD5/XP** - *X Pulse Presence*

            **I048/MD5/XP/(spare)**

            - 2 bits [``..``]

            **I048/MD5/XP/XP** - *X-pulse from Mode 5 PIN Reply/Report*

            - 1 bit [``.``]

            - values:

                | 0: X-Pulse not present
                | 1: X-pulse present

            **I048/MD5/XP/X5** - *X-pulse from Mode 5 Data Reply or Report*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no authenticated Data reply or Report received
                | 1: X-pulse set to one (present)

            **I048/MD5/XP/XC** - *X-pulse from Mode C Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode C reply
                | 1: X-pulse set to one (present)

            **I048/MD5/XP/X3** - *X-pulse from Mode 3/A Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 3/A reply
                | 1: X-pulse set to one (present)

            **I048/MD5/XP/X2** - *X-pulse from Mode 2 Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 2 reply
                | 1: X-pulse set to one (present)

            **I048/MD5/XP/X1** - *X-pulse from Mode 1 Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 1 reply
                | 1: X-pulse set to one (present)

            remark
                Note:
                    Within Mode 5 replies/reports, the X-Pulse can be set for the following cases:
                    1. In a combined Mode 1 and Mode 2 reply/report: in this case the X5 bit and the X2 bit
                       shall be set;
                    2. In a combined Mode 3 and Mode C reply/report: in this case the X5 bit and the X3 bit
                       shall be set;
                    3. In a Mode 5 PIN data reply/report: in this case the X5 bit and the XP bit shall be set.
                    The X1 bit and the XC bit are meaningless as in Mode 1 and Mode C replies/reports
                    the X Pulse is not defined. They are kept for compatibility reasons.

        remark
            Note:
                In 2011 NATO has modified the format of the National Origin information
                available in subitem 2 of the Mode 5 data item in this Reserved Expansion Field. The
                information for National Origin and Mission Code were combined into a 11-bit long
                field. In order to maintain backwards compatibility and to ease the use of the new
                layout, the original Mode 5 data item was copied and the layout of subitem #2
                adapted. The new layout is reflected in the data item M5N and shall be used by
                equipment prepared for the new National Origin system.
                Equipment certified to the previous encoding shall continue to use the data item MD5
                corresponding to the 5-bit National Origin / 6-bit Mission Code.

    **I048/M5N** - *Mode 5 Reports, New Format*

    Compound item (FX)

        **I048/M5N/SUM** - *Mode 5 Summary*

            **I048/M5N/SUM/M5**

            - 1 bit [``.``]

            - values:

                | 0: No Mode 5 interrogation
                | 1: Mode 5 interrogation

            **I048/M5N/SUM/ID**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 ID reply/report
                | 1: Authenticated Mode 5 ID reply/report

            **I048/M5N/SUM/DA**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 Data reply/report
                | 1: Authenticated Mode 5 Data reply/report (i.e any valid Mode 5 reply type other than ID)

            **I048/M5N/SUM/M1**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 code not present or not from Mode 5 reply/report
                | 1: Mode 1 code from Mode 5 reply/report

            **I048/M5N/SUM/M2**

            - 1 bit [``.``]

            - values:

                | 0: Mode 2 code not present or not from Mode 5 reply/report
                | 1: Mode 2 code from Mode 5 reply/report

            **I048/M5N/SUM/M3**

            - 1 bit [``.``]

            - values:

                | 0: Mode 3 code not present or not from Mode 5 reply/report
                | 1: Mode 3 code from Mode 5 reply/report

            **I048/M5N/SUM/MC**

            - 1 bit [``.``]

            - values:

                | 0: Mode C altitude not present or not from Mode 5 reply/report
                | 1: Mode C altitude from Mode 5 reply/report

            **I048/M5N/SUM/(spare)**

            - 1 bit [``.``]

            remark
                Notes:
                    4. The flags M2, M3, MC refer to the contents of data items I048/050, I048/070
                       and I048/090 respectively. The flag M1 refers to the contents of data item
                       I048/055, Mode 1 Code in Octal Representation, and to the contents of the
                       Subitem #5 (Extended Mode 1 Code in Octal Representation).
                    5. If an authenticated Mode 5 reply/report is received with the Emergency bit set,
                       then the Military Emergency bit (ME) in Data Item I048/020, Target Report
                       Descriptor, shall be set.
                    6. If an authenticated Mode 5 reply/report is received with the Identification of
                       Position bit set, then the Special Position Identification bit (SPI) in Data Item
                       I048/020, Target Report Descriptor, shall be set.

        **I048/M5N/PMN** - *PIN/ National Origin/Mission Code*

            **I048/M5N/PMN/(spare)**

            - 2 bits [``..``]

            **I048/M5N/PMN/PIN** - *PIN Code*

            - 14 bits [``..............``]

            - raw value

            **I048/M5N/PMN/(spare)**

            - 4 bits [``....``]

            **I048/M5N/PMN/NOV** - *Validity of NO*

            - 1 bit [``.``]

            - values:

                | 0: National Origin is valid
                | 1: National Origin is invalid

            **I048/M5N/PMN/NO** - *National Origin*

            - 11 bits [``...........``]

            - raw value

            remark
                Note:
                    Bit 12 (NOV) is set to 1 if the value for National Origin is not known or invalid.
                    Under certain conditions PIN is available but NO is not available. NOV then
                    indicates that the NO field was not actively populated.

        **I048/M5N/POS** - *Mode 5 Reported Position*

            **I048/M5N/POS/LAT** - *Latitude in WGS 84*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -90/1` °
            - value :math:`<= 90/1` °

            **I048/M5N/POS/LON** - *Longitude in WGS 84*

            - 24 bits [``........................``]

            - signed quantity
            - unit: "°"
            - LSB = :math:`180/2^23` ° :math:`\approx 2.15e-5` °
            - value :math:`>= -180/1` °
            - value :math:`<= 180/1` °

            remark
                Notes:
                    Latitude in WGS 84 is expressed as a 24-bit two’s complement number.
                    Range -90° ≤ latitude ≤ 90°. Sign convention: North is positive.
                    LSB = 180/223 degrees = 2.145767*10-05 degrees

                    Longitude in WGS 84 is expressed as a 24-bit two’s complement number.
                    Range -180° ≤ longitude < 180°. Sign convention: East is positive.
                    LSB = 180/223 degrees = 2.145767*10-05 degrees

                    The resolution implied by the LSB is better than the resolution with which Mode 5
                    position reports are transmitted from aircraft transponders using currently defined
                    formats.

        **I048/M5N/GA** - *Mode 5 GNSS-derived Altitude*

            **I048/M5N/GA/(spare)**

            - 1 bit [``.``]

            **I048/M5N/GA/RES**

            Resolution with which the GNSS-derived Altitude (GA) is reported

            - 1 bit [``.``]

            - values:

                | 0: GA reported in 100 ft increments
                | 1: GA reported in 25 ft increments

            **I048/M5N/GA/GA**

            GNSS-derived Altitude of target, expressed as height above WGS 84 ellipsoid

            - 14 bits [``..............``]

            - signed quantity
            - unit: "ft"
            - LSB = :math:`25` ft
            - value :math:`>= -1000` ft

        **I048/M5N/EM1** - *Extended Mode 1 Code in Octal Representation*

            **I048/M5N/EM1/V**

            - 1 bit [``.``]

            - values:

                | 0: Code not validated
                | 1: Code validated

            **I048/M5N/EM1/G**

            - 1 bit [``.``]

            - values:

                | 0: Default
                | 1: Garbled code

            **I048/M5N/EM1/L**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 Code derived from the reply of the transponder
                | 1: Mode 1 Code not extracted during the last scan

            **I048/M5N/EM1/(spare)**

            - 1 bit [``.``]

            **I048/M5N/EM1/EM1** - *Extended Mode 1 Code in Octal Representation*

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            remark
                Notes:
                    1. If Subitem #1 is present, the M1 bit in Subitem #1 indicates whether the
                       Extended Mode 1 Code is from a Mode 5 reply or a Mode 1 reply. If Subitem
                       #1 is not present, the Extended Mode 1 Code is from a Mode 1 reply.
                    2. For reasons of backwards compatibility the logic for the setting of the V-bit
                       was inverted compared to other similar data items.
                    3. The values of the bits for V, G, L, A4, A2, A1, B2 and B1 shall be identical to
                       the values of the corresponding bits in data item I048/055.

        **I048/M5N/TOS** - *Time Offset for POS and GA*

        - 8 bits [``........``]

        - unsigned quantity
        - unit: "s"
        - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

            remark
                Note:
                    TOS shall be assumed to be zero if Subitem #6 is not present.

        **I048/M5N/XP** - *X Pulse Presence*

            **I048/M5N/XP/(spare)**

            - 2 bits [``..``]

            **I048/M5N/XP/XP** - *X-pulse from Mode 5 PIN Reply/Report*

            - 1 bit [``.``]

            - values:

                | 0: X-Pulse not present
                | 1: X-pulse present

            **I048/M5N/XP/X5** - *X-pulse from Mode 5 Data Reply or Report*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no authenticated Data reply or Report received
                | 1: X-pulse set to one (present)

            **I048/M5N/XP/XC** - *X-pulse from Mode C Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode C reply
                | 1: X-pulse set to one (present)

            **I048/M5N/XP/X3** - *X-pulse from Mode 3/A Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 3/A reply
                | 1: X-pulse set to one (present)

            **I048/M5N/XP/X2** - *X-pulse from Mode 2 Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 2 reply
                | 1: X-pulse set to one (present)

            **I048/M5N/XP/X1** - *X-pulse from Mode 1 Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 1 reply
                | 1: X-pulse set to one (present)

            remark
                Note:
                    Within Mode 5 replies/reports, the X-Pulse can be set for the following cases:
                    1. In a combined Mode 1 and Mode 2 reply/report: in this case the X5 bit and the X2 bit
                       shall be set;
                    2. In a combined Mode 3 and Mode C reply/report: in this case the X5 bit and the X3 bit
                       shall be set;
                    3. In a Mode 5 PIN data reply/report: in this case the X5 bit and the XP bit shall be set.
                    The X1 bit and the XC bit are meaningless as in Mode 1 and Mode C replies/reports
                    the X Pulse is not defined. They are kept for compatibility reasons.

        **I048/M5N/FOM** - *Figure of Merit*

            **I048/M5N/FOM/(spare)**

            - 3 bits [``...``]

            **I048/M5N/FOM/FOM**

            Figure of Merit. Position Accuracy as extracted and provided by a Mode 5 transponder.

            - 5 bits [``.....``]

            - raw value

        remark
            Note:
                In 2011 NATO has modified the format of the National Origin information
                available in subitem 2 of the Mode 5 data item in this Reserved Expansion Field. The
                information for National Origin and Mission Code were combined into a 11-bit long
                field. In order to maintain backwards compatibility and to ease the use of the new
                layout, the original Mode 5 data item was copied and the layout of subitem #2
                adapted. The new layout is reflected in the data item M5N and shall be used by
                equipment prepared for the new National Origin system.
                Equipment certified to the previous encoding shall continue to use the data item MD5
                corresponding to the 5-bit National Origin / 6-bit Mission Code.

    **I048/M4E** - *Extended Mode 4 Report*

    Extended item.

        **I048/M4E/(spare)**

        - 5 bits [``.....``]

        **I048/M4E/FOEFRI** - *Indication Foe/Friend (Mode4)*

        - 2 bits [``..``]

        - values:

            | 0: No Mode 4 interrogation
            | 1: Possibly friendly target
            | 2: Probably friendly target
            | 3: Friendly target

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I048/RPC** - *Radar Plot Characteristics*

    Compound item (FX)

        **I048/RPC/SCO** - *Score*

        The score describes the number of raw responses used to create the plot.

        - 8 bits [``........``]

        - unsigned integer

        **I048/RPC/SRC** - *Signal/Clutter Ratio*

        The Signal / Clutter Ratio describes the difference in signal strength between the
        signal constituting the raw plot and the signal of the clutter.

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "dB"
        - LSB = :math:`1/10` dB :math:`\approx 0.10` dB
        - value :math:`>= 1/10` dB
        - value :math:`<= 2550` dB

        **I048/RPC/RW** - *Range Width*

        The Range Width defines the difference in range between the closest proximity to the
        radar of the raw response and the point farthest away from the radar.

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "NM"
        - LSB = :math:`1/2^8` NM :math:`\approx 3.91e-3` NM
        - value :math:`<= 256/1` NM

        **I048/RPC/AR** - *Ambiguous Range*

        The Ambiguous Range describes the Pulse Repetition Interval of the radar in range.

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "NM"
        - LSB = :math:`1/2^8` NM :math:`\approx 3.91e-3` NM
        - value :math:`<= 256/1` NM

    **I048/ERR** - *Extended Range Report*

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "NM"
    - LSB = :math:`1/2^8` NM :math:`\approx 3.91e-3` NM
    - value :math:`<= 65535` NM

        remark
            Notes:
                1. For radars with an operational range beyond 256 NM data item I048/040 is
                   insufficient. These radars may use this extension to provide the target position
                   equal to or beyond 256 NM. In such cases, data item I048/040 shall be
                   transmitted in addition to this extension. In this case it is recommended to
                   set bits 32/17 in data item I048/040 to “1”.
                2. The Encoding Rule for data item I048/040 still applies.
                3. This item represents the measured target position of the plot, even if
                   associated with a track, for the present antenna scan. It is expressed in polar
                   co-ordinates in the local reference system, centred on the radar station.
                4. In case of combined detection by a PSR and an SSR, then the SSR position is
                   sent.
                5. Before migrating an ASTERIX encoder to Edition 1.10 of this specification,
                   care has to be taken that receiving decoders allow the presence of the value
                   256 NM in the record. Systems applying a range check may otherwise
                   suppress the record.

    **I048/RTC** - *Radar Track Characteristics*

    Compound item (FX)

        **I048/RTC/PTL** - *Plot/Track Link*

        Providing link between a track and its associated plot.

            **I048/RTC/PTL/(spare)**

            - 3 bits [``...``]

            **I048/RTC/PTL/SCN** - *Track / SCN Association*

            - 1 bit [``.``]

            - values:

                | 0: Track is not associated with an SCN Plot
                | 1: Track is associated with an SCN Plot

            **I048/RTC/PTL/RC** - *Roll Call Component*

            - 1 bit [``.``]

            - values:

                | 0: Associated Plot does not contain a Roll Call component
                | 1: Associated Plot contains at least a Roll Call component

            **I048/RTC/PTL/AC** - *All Call Component*

            - 1 bit [``.``]

            - values:

                | 0: Associated Plot does not contain an All Call component
                | 1: Associated Plot contains at least an All Call component

            **I048/RTC/PTL/SSR** - *SSR Component*

            - 1 bit [``.``]

            - values:

                | 0: Associated Plot does not contain an SSR component
                | 1: Associated Plot contains at least an SSR component

            **I048/RTC/PTL/PSR** - *PSR Component*

            - 1 bit [``.``]

            - values:

                | 0: Associated Plot does not contain a PSR component
                | 1: Associated Plot contains at least a PSR component

            **I048/RTC/PTL/PLOTNR**

            Unique reference to the associated plot record

            - 16 bits [``................``]

            - raw value

            remark
                Notes:
                    1. (to bits-16/1): If SCN = 0, PLOTNR shall be set to 0.
                    2. (to bit-21): If SCN = 1, I048/020/SCN#VAL – if implemented - shall be set to “1”.

        **I048/RTC/ATL** - *ADS-B/Track Link*

        Providing link between a track and its associated ADS-B Report.

        Repetitive item, repetition factor 8 bits.

            - 16 bits [``................``]

            - raw value

            remark
                Note:
                    The presence of this information shall be communicated in Data Item
                    I048/020 by setting I048/020/ADSB#VAL – if implemented - = 1.

        **I048/RTC/TRN** - *Turn State*

        Turn State with probability with regards to track evolution hypothesis (Circular model).

        - 8 bits [``........``]

        - unsigned quantity
        - unit: "%"
        - LSB = :math:`1` %
        - value :math:`<= 100` %

        **I048/RTC/NPP** - *Next Predicted Position*

        Next predicted position for a track update at the next expected antenna rotation in
        reference to the current track update.

            **I048/RTC/NPP/PREDRHO** - *Predicted Range*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/2^7` NM :math:`\approx 7.81e-3` NM

            **I048/RTC/NPP/PREDTHETA** - *Predicted Azimuth*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "°"
            - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

            **I048/RTC/NPP/EVOLRHOSTART** - *Predicted Closest Range*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/2^7` NM :math:`\approx 7.81e-3` NM

            **I048/RTC/NPP/EVOLRHOEND** - *Predicted Largest Range*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/2^7` NM :math:`\approx 7.81e-3` NM

            **I048/RTC/NPP/EVOLTHETASTART** - *Predicted Smallest Azimuth*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "°"
            - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

            **I048/RTC/NPP/EVOLTHETAEND** - *Predicted Largest Azimuth*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "°"
            - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

            **I048/RTC/NPP/NOISERHOSTART** - *Predicted Closest Range*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/2^7` NM :math:`\approx 7.81e-3` NM

            **I048/RTC/NPP/NOISERHOEND** - *Predicted Largest Range*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "NM"
            - LSB = :math:`1/2^7` NM :math:`\approx 7.81e-3` NM

            **I048/RTC/NPP/NOISETHETASTART** - *Predicted Smallest Azimuth*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "°"
            - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

            **I048/RTC/NPP/NOISETHETAEND** - *Predicted Largest Azimuth*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "°"
            - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

            **I048/RTC/NPP/PREDTIME** - *Predicted Detection Time*

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "s"
            - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

            remark
                Notes:
                    1. When the area crosses North, THETASTART is larger than THETAEND.
                    2. Next detection = Time of Day of current track record + PREDTIME.

        **I048/RTC/DLK** - *Data Link Characteristics*

        Active message list for the aircraft for the current scan.

        Repetitive item, repetition factor 8 bits.

                **I048/RTC/DLK/TYPE**

                Type of Message Protocol

                - 4 bits [``....``]

                - values:

                    | 0: Surveillance Mode A (alert bit or periodic)
                    | 1: Comm-A
                    | 2: Ground Initiated Comm-B
                    | 3: Air Initiated Comm-B
                    | 4: Broadcast Comm-B
                    | 5: Comm-C
                    | 6: Comm-D
                    | 7: Reserved for future use
                    | 8: Reserved for future use
                    | 9: Reserved for future use
                    | 10: Reserved for future use
                    | 11: Reserved for future use
                    | 12: Reserved for future use
                    | 13: Reserved for future use
                    | 14: Reserved for future use
                    | 15: Reserved for future use

                **I048/RTC/DLK/ORIGIN**

                Frame Detection

                - 2 bits [``..``]

                - values:

                    | 0: From previous scan
                    | 1: New in current scan
                    | 2: Requested in the beam by transponder
                    | 3: Invalid ASTERIX value

                **I048/RTC/DLK/STATE**

                Frame state at aircraft release

                - 2 bits [``..``]

                - values:

                    | 0: In progress
                    | 1: Completed
                    | 2: Cancelled
                    | 3: Invalid ASTERIX value

        **I048/RTC/LCK** - *Lockout Characteristics*

        Lockout State and remaining Lockout Time

            **I048/RTC/LCK/LS** - *Lockout State*

            - 1 bit [``.``]

            - values:

                | 0: Target not locked out by this radar
                | 1: Target locked out by this radar

            **I048/RTC/LCK/LOCTIM** - *Lockout Time*

            - 15 bits [``...............``]

            - unsigned quantity
            - unit: "ms"
            - LSB = :math:`1` ms

        **I048/RTC/TC** - *Transition Code*

        Indication and Counter of Transition Codes for Modes 1, 2, and 3

            **I048/RTC/TC/(spare)**

            - 7 bits [``.......``]

            **I048/RTC/TC/TCOUNT1**

            Number of scans with transient Mode 1 Code

            - 4 bits [``....``]

            - unsigned integer

            **I048/RTC/TC/TCODE1**

            Transient Mode 1 Code

            - 5 bits [``.....``]

            - raw value

            **I048/RTC/TC/TCOUNT2**

            Number of scans with transient Mode 2 Code

            - 4 bits [``....``]

            - unsigned integer

            **I048/RTC/TC/TCODE2**

            Transient Mode 2 Code

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            **I048/RTC/TC/TCOUNT3**

            Number of scans with transient Mode 3 Code

            - 4 bits [``....``]

            - unsigned integer

            **I048/RTC/TC/TCODE3**

            Transient Mode 3 Code

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            remark
                Notes:
                    1. This item indicates a difference in the value for TCODEX between the code in
                       the track file and the code from the latest plot updating the track.
                    2. If TCOUNTX is set to 0 then TCODEX is meaningless and all bits shall be set
                       to 0.
                    3. The meaning of the individual bits in TCODEX is described in ICAO Annex 10
                       Volume 4 Chapter 3.1.1.6.2.

        **I048/RTC/TLC** - *Track Life Cycle*

        Acquisition Status of the Track and Track Life Cycle Counters

            **I048/RTC/TLC/ACQI**

            Acquisition Status Indicator

            - 2 bits [``..``]

            - values:

                | 0: Tentative Track with One Plot
                | 1: Tentative Track with at least Two Plots
                | 2: Pre-Confirmed Track
                | 3: Confirmed Track

            **I048/RTC/TLC/TRKUPDCTR**

            Track Update Counter

            - 14 bits [``..............``]

            - unsigned integer

            **I048/RTC/TLC/LASTTRKUPD**

            Time since last Track Update

            - 16 bits [``................``]

            - unsigned quantity
            - unit: "ms"
            - LSB = :math:`1` ms

            remark
                Notes:
                    1. When Subitem #8 is included, each element shall be properly populated.
                    2. The setting of bits 32/31 is implementation dependent and shall be described
                       in the ICD of the system generating the ASTERIX record.
                    3. The TRKUPDCTR is initiated with a value of 1 and it is incremented by 1 each
                       time a track is updated.
                    4. The LASTTRKUPD is set to 0 each time a track is updated.

        **I048/RTC/ASI** - *Adjacent Sensor Information*

        Adjacent Sensor information (received via SCN) for the respective Mode S address

        Repetitive item, repetition factor 8 bits.

                **I048/RTC/ASI/SACADJS** - *SAC of the Adjacent Sensor*

                - 8 bits [``........``]

                - raw value

                **I048/RTC/ASI/SICADJS** - *SIC of the Adjacent Sensor*

                - 8 bits [``........``]

                - raw value

                **I048/RTC/ASI/TIMEOFDAYSCN** - *Absolute Timestamp in UTC Provided by the SCN*

                - 16 bits [``................``]

                - unsigned quantity
                - unit: "s"
                - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

                **I048/RTC/ASI/DATAUSE** - *Use of Adjacent Sensor Data*

                - 7 bits [``.......``]

                - values:

                    | 0: Data used by Tracker
                    | 1: Data not used by Tracker
                    | 2: 2-127: Reserved for future use

                **I048/RTC/ASI/DRNA** - *DRN Availability*

                - 1 bit [``.``]

                - values:

                    | 0: DRN not available
                    | 1: DRN available

                **I048/RTC/ASI/DRN**

                Duplicate Address Reference Number uniquely identifying the aircraft in case of a duplicate Mode S Address

                - 16 bits [``................``]

                - raw value

        **I048/RTC/TES** - *Track Extrapolation Source*

        Source for the extrapolation of the track information

        - 8 bits [``........``]

        - values:

            | 0: Radar tracker calculation
            | 1: Integrated ADS-B
            | 2: External ADS-B
            | 3: SCN

        **I048/RTC/IR** - *Identity Requested*

        Information whether during latest scan the Mode 3/A Code was requested

            **I048/RTC/IR/IR**

            Identity Requested during latest scan

            - 1 bit [``.``]

            - values:

                | 0: Identity not requested
                | 1: Identity requested

            **I048/RTC/IR/M3A** - *Age of Mode 3/A Code (I048/070)*

            - 7 bits [``.......``]

            - unsigned quantity
            - unit: "s"
            - LSB = :math:`1` s

    **I048/CPC** - *Common and Plot Characteristics*

    Compound item (FX)

        **I048/CPC/PNB** - *Plot Number*

        Unique reference to a Plot Record

        - 16 bits [``................``]

        - raw value

        **I048/CPC/RPL** - *Replies/Plot Link*

        Link between a Plot and its Replies

        Repetitive item, repetition factor 8 bits.

                **I048/CPC/RPL/TYPE** - *Reply Type*

                - 8 bits [``........``]

                - values:

                    | 0: PSR Echo
                    | 1: SSR Reply
                    | 2: All Call Reply
                    | 3: Roll Call Reply

                **I048/CPC/RPL/REPLYNBR**

                Unique reference to a plot record

                - 16 bits [``................``]

                - raw value

        **I048/CPC/SNB** - *Scan Number*

        Scan Number

        - 8 bits [``........``]

        - unsigned integer

            remark
                Note:
                    The Scan Number ranges from 1 to 127 and is incremented when the radar
                    passes North. Once SCANNBR reached 127 it will restarted at 1 with the next
                    scan.

        **I048/CPC/DATE** - *Common and Plot Characteristics Date*

        Current Date in the form YYYYMMDD

            **I048/CPC/DATE/Y1**

            First digit of year

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            **I048/CPC/DATE/Y2**

            Second digit of year

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            **I048/CPC/DATE/Y3**

            Third digit of year

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            **I048/CPC/DATE/Y4**

            Fourth digit of year

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            **I048/CPC/DATE/M1**

            First digit of month

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 1`

            **I048/CPC/DATE/M2**

            Second digit of month

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            **I048/CPC/DATE/D1**

            First digit of day

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 3`

            **I048/CPC/DATE/D2**

            Second digit of day

            - 4 bits [``....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 10`

            remark
                Note:
                    The day is incremented at midnight UTC.

