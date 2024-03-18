Asterix expansion 021 - ADS-B Target Reports Expansion
======================================================
**category**: 021

**edition**: 1.5

**date**: 2021-12-22

Description of asterix expansion
--------------------------------
Compound item (fspec=8 bits)

    **I021/BPS** - *Barometric Pressure Setting*

        **I021/BPS/(spare)**

        - 4 bits [``....``]

        **I021/BPS/BPS** - *Barometric Pressure Setting*

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "hPa"
        - LSB = :math:`1/10` hPa :math:`\approx 0.10` hPa
        - value :math:`>= 0` hPa
        - value :math:`<= 819/2` hPa

        remark
            Notes:

                - BPS is the barometric pressure setting of the aircraft minus 800 hPa

                - A value of "0" indicates that in the aircraft a value of 800 hPa or
                  less has been selected.

                - A value of "409.5" indicates that in the aircraft a value of 1209.5
                  hPa or more has been selected.

    **I021/SH** - *Selected Heading*

        **I021/SH/(spare)**

        - 4 bits [``....``]

        **I021/SH/HDR** - *Horizontal Reference Direction*

        - 1 bit [``.``]

        - values:

            | 0: True North
            | 1: Magnetic North

        **I021/SH/STAT** - *Selected Heading Status*

        - 1 bit [``.``]

        - values:

            | 0: Data is either unavailable or invalid
            | 1: Data is available and valid

        **I021/SH/SH** - *Selected Heading*

        - 10 bits [``..........``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`45/2^6` ° :math:`\approx 0.70` °

        remark
            On many aircraft, the ADS-B Transmitting Subsystem receives
            Selected Heading from a Mode Control Panel / Flight Control Unit
            (MCP / FCU). Users of this data are cautioned that the Selected
            Heading value transmitted by the ADS-B Transmitting Subsystem
            does not necessarily reflect the true intention of the airplane during
            certain flight modes (e.g., during LNAV mode).

    **I021/NAV** - *Navigation Mode*

        **I021/NAV/AP** - *Autopilot*

        - 1 bit [``.``]

        - values:

            | 0: Autopilot not engaged
            | 1: Autopilot engaged

        **I021/NAV/VN** - *Vertical Navigation*

        - 1 bit [``.``]

        - values:

            | 0: Vertical Navigation not active
            | 1: Vertical Navigation active

        **I021/NAV/AH** - *Altitude Hold*

        - 1 bit [``.``]

        - values:

            | 0: Altitude Hold not engaged
            | 1: Altitude Hold engaged

        **I021/NAV/AM** - *Approach Mode*

        - 1 bit [``.``]

        - values:

            | 0: Approach Mode not active
            | 1: Approach Mode active

        **I021/NAV/MFM** - *Status of MCP/FCU Mode Bits*

            **I021/NAV/MFM/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/NAV/MFM/VAL** - *Value*

            - 1 bit [``.``]

            - values:

                | 0: MCP/FCU Mode Bits not populated
                | 1: MCP/FCU Mode Bits populated

        **I021/NAV/(spare)**

        - 2 bits [``..``]

        remark
            NOTE: 1: The status of the LNAV indication is contained in Data Item I021/200,
            bit-7. Please also consider Note 2 to Data Item I021/200.

            NOTE: 2: MFM (Status of MCP/FCU Mode Bits) is contained in the Target
            State and Status Message (Register 62 16, Format Type Code 29,
            Subtype Code 1) as defined in EUROCAE ED-102B/RTCA DO-260C
            Ref. [11] chapter 2.2.3.2.7.1.3.11. If set to “1”, MFM#VAL indicates that
            the bits for “AP”, “VN”, “AH”, “AM” (in I021/REF/NAV) and for LNAV (in
            I021/200) have been actively populated.

            NOTE: 3: If MFM#VAL = 0, AP, VN, AH, and AM (in I021/REF/NAV) shall be
            set to 0 and LNAV (in I021/200) shall be set to 1.

    **I021/GAO** - *GPS Antenna Offset*

    - 8 bits [``........``]

    - raw value

        remark
            The value of this field is copied from the respective bits 33-40 of
            Register 65 16 of Version 2 and Version 3 ADS-B Systems (as defined
            in I021/210) (Aircraft Operational Status - Surface). The detailed
            definition is contained in EUROCAE Document ED-102()/RTCA DO-
            260() Ref. [11].
            Bit-8 indicates the direction of the offset with a value of 0 indicating
            ‘left of centerline’ and a value of 1 indicating ‘right of centerline’.

    **I021/SGV** - *Surface Ground Vector*

    Extended item.

        **I021/SGV/STP**

        - 1 bit [``.``]

        - values:

            | 0: Aircraft has not stopped
            | 1: Aircraft has stopped

        **I021/SGV/HTS**

        - 1 bit [``.``]

        - values:

            | 0: Heading/Ground Track data is not valid
            | 1: Heading/Ground Track data is valid

        **I021/SGV/HTT**

        - 1 bit [``.``]

        - values:

            | 0: Heading data provided
            | 1: Ground Track provided

        **I021/SGV/HRD**

        - 1 bit [``.``]

        - values:

            | 0: True North
            | 1: Magnetic North

        **I021/SGV/GSS** - *Ground Speed*

        - 11 bits [``...........``]

        - unsigned quantity
        - unit: "kt"
        - LSB = :math:`1/2^3` kt :math:`\approx 0.12` kt

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/SGV/HGT** - *Heading/Ground Track Information*

        - 7 bits [``.......``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`45/2^4` ° :math:`\approx 2.81` °

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I021/STA** - *Aircraft Status*

    Extended item.

        **I021/STA/ES** - *ES IN Capability*

        - 1 bit [``.``]

        - values:

            | 0: Target is not 1090 ES IN capable
            | 1: Target is 1090 ES IN capable

        **I021/STA/UAT** - *UAT IN Capability*

        - 1 bit [``.``]

        - values:

            | 0: Target is not UAT IN capable
            | 1: Target is UAT IN capable

        **I021/STA/RCE** - *Reduced Capability Equipment*

            **I021/STA/RCE/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/RCE/VAL** - *Value*

            - 2 bits [``..``]

            - values:

                | 0: Not RCE
                | 1: TABS (see Note 2)
                | 2: Reserved for future use
                | 3: Other RCE

        **I021/STA/RRL** - *Reply Rate Limiting*

            **I021/STA/RRL/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/RRL/VAL** - *Value*

            - 1 bit [``.``]

            - values:

                | 0: Reply Rate Limiting is not active
                | 1: Reply Rate Limiting is active

            remark
                Notes:

                    1. The RCE information is taken from the Capability Class field in
                       the “Aircraft Operational Status Message (Register 65 16 )” as defined
                       in EUROCAE ED-102B/RTCA DO-260C (Chapter A.1.4.10.3 in Ref.[11]).
                    2. TABS is the “Traffic Awareness Beacon System” as defined by
                       ETSO-C199 / TSO-C199.
                    3. The RRL information is contained in the Operational Mode field
                       in the Airborne Operational Status Message, (Register 65_16, Bit 29).

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/STA/PS3** - *Priority Status for Version 3 ADS-B Systems*

            **I021/STA/PS3/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/PS3/VAL** - *Value*

            - 3 bits [``...``]

            - values:

                | 0: No emergency / not reported
                | 1: General emergency
                | 2: UAS/RPAS - Lost link
                | 3: Minimum fuel
                | 4: No communications
                | 5: Unlawful interference
                | 6: Aircraft in Distress
                | 7: Aircraft in Distress Manual Activation

        **I021/STA/TPW** - *Transmit Power*

            **I021/STA/TPW/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/TPW/VAL** - *Value*

            - 2 bits [``..``]

            - values:

                | 0: Unavailable, Unknown, or less than 70 W
                | 1: 70 W
                | 2: 125 W
                | 3: 200 W

            remark
                Notes:
                    1. For ADS-B Version 3 systems as defined in EUROCAE ED-
                       102B/RTCA DO-260C (Ref. [11]) the values have been re-defined.
                       PS3 shall be used exclusively for Version 3 ADS-B systems as
                       defined in I021/210/VN.
                    2. For ADS-B systems with a version number below 3, the PS
                       shall be encoded in Data Item I021/200/PS.
                       However, since values have been re-defined in ADS-B Version 3,
                       mapping is required to ensure that information is not lost. This
                       mapping shall be done according to the following table: ::

                         ADS-B Version 3 (PS3)           ADS-Version < 3 (I021/200 - PS)
                         0 (No Emergency/not reported)   0 (No Emergency/not reported)
                         1 (General emergency)           1 (General emergency)
                         2 (UAS/RPAS Lost Link)          4 (No communication)
                         3 (Minimum fuel)                3 (Minimum fuel)
                         4 (No communication)            4 (No communication)
                         5 (Unlawful interference)       5 (Unlawful interference)
                         6 (Aircraft in distress -       1 (General emergency)
                           automatic activation)
                         7 (Aircraft in distress -       1 (General emergency)
                           manual activation)

                    3. TPW#VAL is defined in EUROCAE ED-102B/DO-260C Ref. [11]
                       “Aircraft Operational Status Message (Register 65 16 )” Bits 17-18
                       and indicates the nearest minimum transmit power (in Watts) at the
                       antenna port. The nearest minimum setting in this field would be
                       rounded down from the actual design value. For example, if the
                       avionics is designed to transmit at 100W out of the antenna port, the
                       encoded value in this field would be for 70W (decimal 1).

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/STA/TSI** - *Transponder Side Indication*

            **I021/STA/TSI/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/TSI/VAL** - *Value*

            - 2 bits [``..``]

            - values:

                | 0: Unknown
                | 1: Transponder #1 (left/pilot side or single)
                | 2: Transponder #2 (right/co-pilot side)
                | 3: Transponder #3 (auxiliary or Backup)

        **I021/STA/MUO** - *Manned / Unmanned Operation*

            **I021/STA/MUO/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/MUO/VAL** - *Value*

            - 1 bit [``.``]

            - values:

                | 0: Manned Operation
                | 1: Unmanned Operation

        **I021/STA/RWC** - *Remain Well Clear Corrective Alert*

            **I021/STA/RWC/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/RWC/VAL** - *Value*

            - 1 bit [``.``]

            - values:

                | 0: RWC Corrective Alert not active
                | 1: RWC Corrective Alert active

            remark
                Notes:

                    1. This information is taken from the “Extended Squitter Aircraft
                       Status Message” Register 61 16 Bit 25 as defined in EUROCAE ED-
                       102B/RTCA DO-260C Ref. [11]. It is available for Version 3 ADS-B
                       Systems only (refer to I021/210/VN).
                    2. This information is taken from the Operational Mode field in the
                       “Aircraft Operational Status Message (Register 65 16 Bit 40)” as
                       defined in EUROCAE ED-102B/RTCA DO-260C (Chapter
                       A.1.4.10.4 in Ref. [11]). This information is available for Version 3
                       ADS-B systems only (refer to I021/210/VN).

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/STA/DAA** - *Detectand Avoid Capabilities*

            **I021/STA/DAA/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/DAA/VAL** - *Value*

            - 2 bits [``..``]

            - values:

                | 0: No RWC Capability
                | 1: RWC/RA/OCM Capability
                | 2: RWC/OCM Capability
                | 3: Invalid ASTERIX Value

        **I021/STA/DF17CA** - *Transponder Capability*

            **I021/STA/DF17CA/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/DF17CA/VAL** - *Value*

            - 3 bits [``...``]

            - raw value

            remark
                Notes:

                    1. This information is taken from the Capability Class field in the
                       “Aircraft Operational Status Message (Register 65 16 Bits 23-24)” as
                       defined in EUROCAE ED-102B/RTCA DO-260C (Chapter
                       A.1.4.10.3 in Ref. [11]). This information is available for Version 3
                       ADS-B systems only (refer to I021/210/VN).
                    2. The meaning of the individual values in “DAA” are described in
                       Chapter A.1.4.10.24 in EUROCAE ED-102B/RTCA DO-260C Ref.[11].
                    3. CA is transmitted in Downlink Format 17 messages. CA is defined
                       in EUROCAE ED-73F [14] chapter 3.18.4.5 and in RTCA DO-181F
                       [14] chapter 2.2.14.4.6 where further details on the meaning of this
                       Element are provided. Category 021 provides this Element as a
                       “store-and-forward” capability only.

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/STA/SVH** - *Sense Vertical & Horizontal*

            **I021/STA/SVH/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/SVH/VAL** - *Value*

            - 2 bits [``..``]

            - values:

                | 0: Vertical Only
                | 1: Horizontal Only
                | 2: Blended
                | 3: Vertical Only or Horizontal Only per intruder

        **I021/STA/CATC** - *CAS Type & Capability*

            **I021/STA/CATC/EP** - *Element Population Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element not populated
                | 1: Element populated

            **I021/STA/CATC/VAL** - *Value*

            - 3 bits [``...``]

            - values:

                | 0: Active CAS (TCAS II) or no CAS
                | 1: Active CAS (not TCAS II)
                | 2: Active CAS (not TCAS II) with OCM transmit capability
                | 3: Active CAS of Junior Status
                | 4: Passive CAS with 1030TCAS Resolution Message receive capability
                | 5: Passive CAS with only OCM receive capability
                | 6: Reserved for future use
                | 7: Reserved for future use

            remark
                Notes:

                    1. SVH is part of the CCCB (Collision Avoidance Coordination
                       Capability Bits) in the “Aircraft Operational Status Message”
                       Register 65_16 Bits 33-39 as defined in EUROCAE ED-102B/RTCA
                       DO-260C Ref. [11], Chapter 2.2.3.2.7.2.4.8.1. This information is
                       available for Version 3 ADS-B systems only (refer to I021/210/VN).
                    2. CATC is part of the CCCB (Collision Avoidance Coordination
                       Capability Bits) in the “Aircraft Operational Status Message”
                       Register 65_16 Bits 33-39 as defined in EUROCAE ED-102B/RTCA
                       DO-260C Ref. [11], Chapter 2.2.3.2.7.2.4.8.2. This information is
                       available for Version 3 ADS-B systems only (refer to I021/210/VN).

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        **I021/STA/TAO** - *Transponder Antenna Offset*

            **I021/STA/TAO/EP** - *Element Populated Bit*

            - 1 bit [``.``]

            - values:

                | 0: Element Not Populated
                | 1: Element Populated

            **I021/STA/TAO/VAL** - *Value*

            - 5 bits [``.....``]

            - values:

                | 0: No data
                | 1: 0 ≤ TAO ≤ 1
                | 2: 1 < TAO ≤ 2
                | 3: 2 < TAO ≤ 4
                | 4: 4 < TAO ≤ 6
                | 5: 6 < TAO ≤ 8
                | 6: 8 < TAO ≤ 10
                | 7: 10 < TAO ≤ 12
                | 8: 12 < TAO ≤ 14
                | 9: 14 < TAO ≤ 16
                | 10: 16 < TAO ≤ 18
                | 11: 18 < TAO ≤ 20
                | 12: 20 < TAO ≤ 22
                | 13: 22 < TAO ≤ 24
                | 14: 24 < TAO ≤ 26
                | 15: 26 < TAO ≤ 28
                | 16: 28 < TAO ≤ 30
                | 17: 30 < TAO ≤ 32
                | 18: 32 < TAO ≤ 34
                | 19: 34 < TAO ≤ 36
                | 20: 36 < TAO ≤ 38
                | 21: 38 < TAO ≤ 40
                | 22: 40 < TAO ≤ 42
                | 23: 42 < TAO ≤ 44
                | 24: 44 < TAO ≤ 46
                | 25: 46 < TAO ≤ 48
                | 26: 48 < TAO ≤ 50
                | 27: 50 < TAO ≤ 52
                | 28: 52 < TAO ≤ 54
                | 29: 54 < TAO ≤ 56
                | 30: 56 < TAO ≤ 58
                | 31: TAO > 58

            **I021/STA/TAO/(spare)**

            - 1 bit [``.``]

            remark
                Notes:

                    1. TAO is a one-to-one copy of Message Bits 68 to 72 of the ”Aircraft
                       Operational Status Message” (Register 65 16 ). The TAO is measured
                       along the longitudinal axis of the aircraft from the forward end.

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I021/TNH** - *True North Heading*

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °

        remark
            Magnetic Heading is defined in I021/152.

    **I021/MES** - *Military Extended Squitter*

    Compound item (FX)

        **I021/MES/SUM** - *Mode 5 Summary*

            **I021/MES/SUM/M5**

            - 1 bit [``.``]

            - values:

                | 0: No Mode 5 interrogation
                | 1: Mode 5 interrogation

            **I021/MES/SUM/ID**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 ID reply/report
                | 1: Authenticated Mode 5 ID reply/report

            **I021/MES/SUM/DA**

            - 1 bit [``.``]

            - values:

                | 0: No authenticated Mode 5 Data reply or Report
                | 1: Authenticated Mode 5 Data reply or Report (i.e any valid Mode 5 reply type other than ID)

            **I021/MES/SUM/M1**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 code not present or not from Mode 5 reply/report
                | 1: Mode 1 code from Mode 5 reply/report

            **I021/MES/SUM/M2**

            - 1 bit [``.``]

            - values:

                | 0: Mode 2 code not present or not from Mode 5 reply/report
                | 1: Mode 2 code from Mode 5 reply/report

            **I021/MES/SUM/M3**

            - 1 bit [``.``]

            - values:

                | 0: Mode 3 code not present or not from Mode 5 reply/report
                | 1: Mode 3 code from Mode 5 reply/report

            **I021/MES/SUM/MC**

            - 1 bit [``.``]

            - values:

                | 0: Flightlevel not present or not from Mode 5 reply/report
                | 1: Flightlevel from Mode 5 reply/report

            **I021/MES/SUM/PO**

            - 1 bit [``.``]

            - values:

                | 0: Position not from Mode 5 report (ADS-B report)
                | 1: Position from Mode 5 report

            remark
                Notes:

                    1. The flag M2 refers to the contents of Subfield #6 below, M3, MC refer
                       to the contents of data items I021/070 and I021/145 respectively. The
                       flag M1 refers to the contents of Subfield #3 below (Extended Mode 1
                       Code in Octal Representation).

                    2. If a Mode 5 reply/report is received with the Emergency bit set, then
                       the Military Emergency bit (ME) in Data Item I021/200, Target Status,
                       shall be set.

                    3. If a Mode 5 reply/report is received with the Identification of Position bit
                       set, then the Special Position Identification bit (SPI) in Data Item
                       I021/200, Target Status, shall be set.

                    4. If a Mode 5 report (ID or Data) is received and fullfill the autentication
                       criteria the corresponding authentication bit shall be set.

        **I021/MES/PNO** - *Mode 5 PIN / National Origin*

            **I021/MES/PNO/(spare)**

            - 2 bits [``..``]

            **I021/MES/PNO/PIN** - *PIN Code*

            - 14 bits [``..............``]

            - raw value

            **I021/MES/PNO/(spare)**

            - 5 bits [``.....``]

            **I021/MES/PNO/NO** - *National Origin Code*

            - 11 bits [``...........``]

            - raw value

        **I021/MES/EM1** - *Extended Mode 1 Code in Octal Representation*

            **I021/MES/EM1/V**

            - 1 bit [``.``]

            - values:

                | 0: Code validated
                | 1: Code not validated

            **I021/MES/EM1/(spare)**

            - 1 bit [``.``]

            **I021/MES/EM1/L**

            - 1 bit [``.``]

            - values:

                | 0: Mode 1 code as derived from the report of the transponder
                | 1: Smoothed Mode 1 code as provided by a local tracker

            **I021/MES/EM1/(spare)**

            - 1 bit [``.``]

            **I021/MES/EM1/EM1** - *Extended Mode 1 Code in Octal Representation*

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            remark
                Notes:

                    - Subfield #1 is present, the M1 bit in Subfield #1 indicates whether the
                      Extended Mode 1 Code is from a Mode 5 reply or a Mode 1 reply. If
                      Subfield #1 is not present, the Extended Mode 1 Code is from a Mode
                      1 reply.

                    - If Subfield #3 is not present the Mode 1 Code was not reported or all
                      Code Bits were equal to 0.

                    - The valid bit is set if the Code was only reported once for that target.

        **I021/MES/XP** - *X Pulse Presence*

            **I021/MES/XP/(spare)**

            - 2 bits [``..``]

            **I021/MES/XP/XP** - *X-pulse from Mode 5 PIN Reply/report*

            - 1 bit [``.``]

            - values:

                | 0: X-Pulse not present
                | 1: X-pulse present

            **I021/MES/XP/X5** - *X-pulse from Mode 5 Data Reply or Report*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no authenticated Data reply or Report received
                | 1: X-pulse set to one (present)

            **I021/MES/XP/XC** - *X-pulse from Mode C Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode C reply
                | 1: X-pulse set to one (present)

            **I021/MES/XP/X3** - *X-pulse from Mode 3/A Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 3/A reply
                | 1: X-pulse set to one (present)

            **I021/MES/XP/X2** - *X-pulse from Mode 2 Reply*

            - 1 bit [``.``]

            - values:

                | 0: 0 X-pulse set to zero or no Mode 2 reply
                | 1: X-pulse set to one (present)

            **I021/MES/XP/X1** - *X-pulse from Mode 1 Reply*

            - 1 bit [``.``]

            - values:

                | 0: X-pulse set to zero or no Mode 1 reply
                | 1: X-pulse set to one (present)

            remark
                Within Mode 5 reports, the X-Pulse can be set for the following cases:

                1. In a combined Mode 1 and Mode 2 report: in this case the X5 bit and the X2 bit
                shall be set;

                2. In a combined Mode 3 and Mode C report: in this case the X5 bit and the X3
                bit shall be set;

                3. In a Mode 5 PIN data report: in this case the X5 bit and the XP bit shall be set.
                The X1 bit and the XC bit are meaningless as in Mode 1 and Mode C
                replies/reports the X Pulse is not defined. They are kept for compatibility
                reasons.

        **I021/MES/FOM** - *Figure of Merit*

            **I021/MES/FOM/(spare)**

            - 3 bits [``...``]

            **I021/MES/FOM/FOM** - *Figure of Merit*

            - 5 bits [``.....``]

            - raw value

        **I021/MES/M2** - *Mode 2 Code in Octal Representation*

            **I021/MES/M2/V**

            - 1 bit [``.``]

            - values:

                | 0: Code validated
                | 1: Code not validated

            **I021/MES/M2/(spare)**

            - 1 bit [``.``]

            **I021/MES/M2/L**

            - 1 bit [``.``]

            - values:

                | 0: Mode-2 code as derived from the reply of the transponder
                | 1: Smoothed Mode-2 code as provided by a local tracker

            **I021/MES/M2/(spare)**

            - 1 bit [``.``]

            **I021/MES/M2/MODE2** - *Mode 2 Code in Octal Representation*

            - 12 bits [``............``]

            - Octal string (3-bits per digit)

            remark
                If Subfield 6 is not present the Mode 2 Code was no reported or all
                Code Bits were equal to 0.

        remark
            Notes:

                - The Reserved Expansion Field is optional. When used to transmit MES, it shall
                  be sent when the targets are represented by Mode 5 Level 2 reports.

                - The information contained in this data item is specific to
                  1090MHz Extended Squitter messages transmitted by military
                  aircraft (Mode 5 Level 2 squitter).

