Asterix expansion 062 - Coding rules for Reserved Expansion Field
=================================================================
**category**: 062

**edition**: 1.3

**date**: 2023-02-13

Description of asterix expansion
--------------------------------
Compound item (fspec=8 bits)

    **I062/CST** - *Contributing Sensors With Local Tracknumbers*

    Repetitive item, repetition factor 8 bits.

            **I062/CST/SAC** - *System Area Code*

            - 8 bits [``........``]

            - raw value

            **I062/CST/SIC** - *System Identification Code*

            - 8 bits [``........``]

            - raw value

            **I062/CST/(spare)**

            - 4 bits [``....``]

            **I062/CST/TYP**

            - 4 bits [``....``]

            - values:

                | 0: No detection
                | 1: Single PSR detection
                | 2: Single SSR detection
                | 3: SSR+PSR detection
                | 4: Single Mode S All-Call
                | 5: Single Mode S Roll-Call
                | 6: Mode S All-Call + PSR
                | 7: Mode S Roll-Call + PSR
                | 8: ADS-B
                | 9: WAM

            **I062/CST/LTN** - *Local Track Number*

            - 16 bits [``................``]

            - raw value

    **I062/CSN** - *Contributing Sensors No Local Tracknumbers*

    Repetitive item, repetition factor 8 bits.

            **I062/CSN/SAC** - *System Area Code*

            - 8 bits [``........``]

            - raw value

            **I062/CSN/SIC** - *System Identification Code*

            - 8 bits [``........``]

            - raw value

            **I062/CSN/(spare)**

            - 4 bits [``....``]

            **I062/CSN/TYP**

            - 4 bits [``....``]

            - values:

                | 0: No detection
                | 1: Single PSR detection
                | 2: Single SSR detection
                | 3: SSR+PSR detection
                | 4: Single Mode S All-Call
                | 5: Single Mode S Roll-Call
                | 6: Mode S All-Call + PSR
                | 7: Mode S Roll-Call + PSR
                | 8: ADS-B
                | 9: WAM

    **I062/TVS** - *Calculated Track Velocity Relative to System Reference Point*

        **I062/TVS/VX**

        - 16 bits [``................``]

        - signed quantity
        - unit: "m/s"
        - LSB = :math:`1/2^2` m/s :math:`\approx 0.25` m/s
        - value :math:`>= -8192` m/s
        - value :math:`<= 32767/4` m/s

        **I062/TVS/VY**

        - 16 bits [``................``]

        - signed quantity
        - unit: "m/s"
        - LSB = :math:`1/2^2` m/s :math:`\approx 0.25` m/s
        - value :math:`>= -8192` m/s
        - value :math:`<= 32767/4` m/s

        remark
            Note:
                The y-axis points to the Geographical North at the system reference
                point as available in the Reserved Expansion Field of category 065.

    **I062/STS** - *Supplementary Track Status*

    Extended item.

        **I062/STS/FDR** - *Flight Data Retained*

        - 1 bit [``.``]

        - values:

            | 0: Flight plan data from active FDPS
            | 1: Flight plan data retained from no longer active FDPS

        **I062/STS/LNAV** - *Lateral Navigation Mode*

            **I062/STS/LNAV/EP** - *LNAV Element Populated*

            - 1 bit [``.``]

            - values:

                | 0: LNAV not populated
                | 1: LNAV populated

            **I062/STS/LNAV/VAL** - *LNAV Mode*

            - 1 bit [``.``]

            - values:

                | 0: LNAV Mode Engaged
                | 1: LNAV Mode not Engaged

        **I062/STS/(spare)**

        - 4 bits [``....``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

        remark
            Note:
                The information of the Lateral Navigation Mode LNAV complements
                the already existing Navigation Modes in Data Item I062/380/SF#7.

    **I062/V3** - *ADS-B Version 3 Data*

    Compound item (FX)

        **I062/V3/PS3** - *Priority Status for Version 3 ADS-B Systems*

            **I062/V3/PS3/EP** - *Priority Status for Version 3 ADS-B Systems Populated*

            - 1 bit [``.``]

            - values:

                | 0: PS3 Element not populated
                | 1: PS3 Element populated

            **I062/V3/PS3/VAL** - *Priority Status for Version 3 ADS-B Systems*

            - 3 bits [``...``]

            - values:

                | 0: No emergency / not reported
                | 1: General emergency
                | 2: UAS/RPAS - Lost link
                | 3: Minimum fuel
                | 4: No communications
                | 5: Unlawful interference
                | 6: Aircraft in Distress Automatic Activation
                | 7: Aircraft in Distress Manual Activation

            **I062/V3/PS3/(spare)**

            - 4 bits [``....``]

            remark
                Notes:

                    1. The ADS-B Version Number is contained in Data Item I062/380/SF#11/VN.
                    2. Since in this edition of the REF I062/REF/PS3 is the only Element
                       in this Item, the Element Populated Bit strictly would not be necessary.
                       However, if in a future edition use is made of the Spare Bits, the Element
                       Populated Bit becomes important.
                    3. For ADS-B Version 3 systems as defined in ED-102B/DO-260C (Ref. [5],
                       as defined in the core Specification of Category 062), the values have been
                       re-defined. I062/REF/PS3 is to be used exclusively for Version 3 ADS-B systems
                       as defined in I062/380/SF#11/VN. For ADS-B systems with a version number below 3,
                       the PS shall be encoded in Data Item I062/380/SF#11/STAT.
                       However, since values have been re-defined in ADS-B Version 3, mapping is required
                       to ensure that information is not lost in systems not yet capable to decode this
                       Edition of Category 062. This mapping shall be done according to the following table: ::

                         ADS-B Version 3 (PS3)           ADS-Version < 3 (I062/380 - STAT)
                         0 (No Emergency/not reported)   0 (No Emergency/not reported)
                         1 (General emergency)           1 (General emergency)
                         2 (UAS/RPAS Lost Link)          4 (No communication)
                         3 (MInimum fuel)                3 (Minimum fuel)
                         4 (No communication)            4 (No communication)
                         5 (Unlawful interference)       5 (Unlawful interference)
                         6 (Aircraft in distress -       1 (General emergency)
                           automatic activation)
                         7 (Aircraft in distress -       1 (General emergency)
                           manual activation)

        **I062/V3/AS** - *Aircraft Status*

            **I062/V3/AS/RCE** - *Reduced Capability Equipment*

                **I062/V3/AS/RCE/EP** - *Element Populated Bit*

                - 1 bit [``.``]

                - values:

                    | 0: Element not populated
                    | 1: Element populated

                **I062/V3/AS/RCE/VAL** - *Value*

                - 2 bits [``..``]

                - values:

                    | 0: Not RCE
                    | 1: TABS
                    | 2: Reserved for future use
                    | 3: Other RCE

            **I062/V3/AS/RRL** - *Reply Rate Limiting*

                **I062/V3/AS/RRL/EP** - *Element Populated Bit*

                - 1 bit [``.``]

                - values:

                    | 0: Element not populated
                    | 1: Element populated

                **I062/V3/AS/RRL/VAL** - *Value*

                - 1 bit [``.``]

                - values:

                    | 0: Reply Rate Limiting is not active
                    | 1: Reply Rate Limiting is active

            **I062/V3/AS/TPW** - *Transmit Power*

                **I062/V3/AS/TPW/EP** - *Element Populated Bit*

                - 1 bit [``.``]

                - values:

                    | 0: Element not populated
                    | 1: Element populated

                **I062/V3/AS/TPW/VAL** - *Value*

                - 2 bits [``..``]

                - values:

                    | 0: Unavailable, Unknown, or less than 70 W
                    | 1: 70 W
                    | 2: 125 W
                    | 3: 200 W

            **I062/V3/AS/TSI** - *Transponder Side Indication*

                **I062/V3/AS/TSI/EP** - *Element Populated Bit*

                - 1 bit [``.``]

                - values:

                    | 0: Element not populated
                    | 1: Element populated

                **I062/V3/AS/TSI/VAL** - *Value*

                - 2 bits [``..``]

                - values:

                    | 0: Unknown
                    | 1: Transponder #1 (left/pilot side or single)
                    | 2: Transponder #2 (right/co-pilot side)
                    | 3: Transponder #3 (auxiliary or Back-up)

            **I062/V3/AS/TAO** - *Transponder Antenna Offset*

                **I062/V3/AS/TAO/EP** - *Element Populated Bit*

                - 1 bit [``.``]

                - values:

                    | 0: Element not populated
                    | 1: Element populated

                **I062/V3/AS/TAO/RE** - *Range Exceeded*

                - 1 bit [``.``]

                - values:

                    | 0: Value in defined range
                    | 1: Value exceeds defined range

                **I062/V3/AS/TAO/VAL** - *Value*

                - 6 bits [``......``]

                - raw value

            **I062/V3/AS/(spare)**

            - 5 bits [``.....``]

        remark
            Notes:

                1. TABS is the “Traffic Awareness Beacon System” as defined by ETSO-C199 / TSO-C199.
                2. PW contains the nearest minimum transmit power (in Watts) at the antenna port.
                   The nearest minimum setting in this field would be rounded down from the actual
                   design value. For example, if the avionics is designed to transmit at 100W out of
                   the antenna port, the encoded value in this field would be for 70W (decimal 1).
                3. Bit-12 shall be set to 1 when the aircraft transmits the maximum encodable value
                   (i.e. 31 representing a TAO greater than 58m). In this case TAO#VAL shall be set
                   to the maximum encodable TAO (i.e. 58m).
                4. The TAO is measured along the longitudinal axis of the aircraft from the forward end.

            UAS "UAS/RPAS Status"
                group
                    MUO "Manned / Unmanned Operation"
                        group
                            EP "Element Populated Bit"
                                element 1
                                    table
                                        0: Element not populated
                                        1: Element populated
                            VAL "Value"
                                element 1
                                    table:
                                        0: Manned Operation
                                        1: Unmanned Operation
                    DAA "Detect and Avoid Capabilities"
                        group
                            EP "Element Populated Bit"
                                element 1
                                    table
                                        0: Element not populated
                                        1: Element populated
                            VAL "Value"
                                element 2
                                    table
                                        0: No RWC Capability
                                        1: RWC/RA/OCM Capability
                                        2: RWC/OCM Capability
                                        3: Invalid ASTERIX Value
                    RWC "Remain Well Clear"
                        group
                            EP "Element Populated Bit"
                                element 1
                                    table
                                        0: Element not populated
                                        1: Element populated
                            VAL "Value"
                                element 1
                                    table
                                        0: RWC Corrective Alert not active
                                        1: RWC Corrective Alert active
                    spare 1
            CASS "Collision Avoidance System Status"
                group
                    SVH "Sense Vertical & Horizontal
                        group
                            EP "Element Populated Bit"
                                element 1
                                    table
                                        0: Element not populated
                                        1: Element populated
                            VAL "Value"
                                element 2
                                    table
                                        0: Vertical Only
                                        1: Horizontal Only
                                        2: Blended
                                        3: Vertical Only or Horizontal Only per intruder
                    CATC "CAS Type & Capability
                        group
                            EP "Element Populated Bit"
                                element 1
                                    table
                                        0: Element not populated
                                        1: Element populated
                            VAL "Value"
                                element 3
                                    table
                                        0: Active CAS (TCAS II) or no CAS
                                        1: Active CAS (not TCAS II)
                                        2: Active CAS (not TCAS II) with OCM transmit capability
                                        3: Active CAS of Junior Status
                                        4: Passive CAS with 1030 TCAS Resolution Message receive capability
                                        5: Passive CAS with only OCM receive capability
                                        6: Reserved for future use
                                        7: Reserved for future use
                    spare 1

