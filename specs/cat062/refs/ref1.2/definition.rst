Asterix expansion 062 - Coding rules for Reserved Expansion Field
=================================================================
**category**: 062

**edition**: 1.2

**date**: 2011-06-01

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

        **I062/STS/(spare)**

        - 6 bits [``......``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

