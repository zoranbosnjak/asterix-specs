Asterix category 021 - ADS-B Target Reports
===========================================
**category**: 021

**edition**: 2.4

**date**: 2015-06-15

Preamble
--------
Surveillance data exchange.
ADS-B Target Reports.

Description of standard data items
----------------------------------

I021/008 - Aircraft Operational Status
**************************************

*Definition*: Identification of the operational services available in the aircraft
while airborne.

*Structure*:

    **I021/008/RA** - *TCAS Resolution Advisory Active*

    - 1 bit [``.``]

    - values:

        | 0: TCAS II or ACAS RA not active
        | 1: TCAS RA active

    **I021/008/TC** - *Target Trajectory Change Report Capability*

    - 2 bits [``..``]

    - values:

        | 0: No capability for Trajectory Change Reports
        | 1: Support for TC+0 reports only
        | 2: Support for multiple TC reports
        | 3: Reserved

    **I021/008/TS** - *Target State Report Capability*

    - 1 bit [``.``]

    - values:

        | 0: No capability to support Target State Reports
        | 1: Capable of supporting target State Reports

    **I021/008/ARV** - *Air-Referenced Velocity Report Capability*

    - 1 bit [``.``]

    - values:

        | 0: No capability to generate ARV-reports
        | 1: Capable of generate ARV-reports

    **I021/008/CDTIA** - *Cockpit Display of Traffic Information Airborne*

    - 1 bit [``.``]

    - values:

        | 0: CDTI not operational
        | 1: CDTI operational

    **I021/008/NOTTCAS** - *TCAS System Status*

    - 1 bit [``.``]

    - values:

        | 0: TCAS operational
        | 1: TCAS not operational

    **I021/008/SA** - *Single Antenna*

    - 1 bit [``.``]

    - values:

        | 0: Antenna Diversity
        | 1: Single Antenna only


Note:
    - Additional Aircraft Status Information is available in the Reserved
      Expansion Field of Category 021.

I021/010 - Data Source Identification
*************************************

*Definition*: Identification of the ADS-B station providing information.

*Structure*:

    **I021/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I021/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the EUROCONTROL
      ASTERIX Web Site
      (http://www.eurocontrol.int/services/system-area-code-list).

I021/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value


Notes:

    1. The service identification is allocated by the system.
    2. The service identification is also available in item I023/015 [Ref. 3].

I021/016 - Service Management
*****************************

*Definition*: Identification of services offered by a ground station (identified by a SIC code).

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 1
- unit: "s"
- LSB = :math:`1 / {2^{1}}` s = :math:`1 / {2}` s :math:`\approx 0.5` s


Notes:

    1. This item contains the same information as item I023/101 in
       ASTERIX category 023 [Ref. 3]. Since not all service users
       receive category 023 data, this information has to be conveyed
       in category 021 as well.
    2. If this item is due to be sent according to the encoding rule
       above, it shall be sent with the next target report

I021/020 - Emitter Category
***************************

*Definition*: Characteristics of the originating ADS-B unit.

*Structure*:

- 8 bits [``........``]

- values:

    | 0: No ADS-B Emitter Category Information
    | 1: Light aircraft <= 15500 lbs
    | 2: 15500 lbs < small aircraft <75000 lbs
    | 3: 75000 lbs < medium a/c < 300000 lbs
    | 4: High Vortex Large
    | 5: 300000 lbs <= heavy aircraft
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
    | 23: Cluster obstacle
    | 24: Line obstacle



I021/040 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the data as transmitted by a system.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I021/040/ATP** - *Address Type*

    - 3 bits [``...``]

    - values:

        | 0: 24-Bit ICAO address
        | 1: Duplicate address
        | 2: Surface vehicle address
        | 3: Anonymous address
        | 4: Reserved for future use
        | 5: Reserved for future use
        | 6: Reserved for future use
        | 7: Reserved for future use

    **I021/040/ARC** - *Altitude Reporting Capability*

    - 2 bits [``..``]

    - values:

        | 0: 25 ft
        | 1: 100 ft
        | 2: Unknown
        | 3: Invalid

    **I021/040/RC** - *Range Check*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Range Check passed, CPR Validation pending

    **I021/040/RAB** - *Report Type*

    - 1 bit [``.``]

    - values:

        | 0: Report from target transponder
        | 1: Report from field monitor (fixed transponder)

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/040/DCR** - *Differential Correction*

    - 1 bit [``.``]

    - values:

        | 0: No differential correction (ADS-B)
        | 1: Differential correction (ADS-B)

    **I021/040/GBS** - *Ground Bit Setting*

    - 1 bit [``.``]

    - values:

        | 0: Ground Bit not set
        | 1: Ground Bit set

    **I021/040/SIM** - *Simulated Target*

    - 1 bit [``.``]

    - values:

        | 0: Actual target report
        | 1: Simulated target report

    **I021/040/TST** - *Test Target*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Test Target

    **I021/040/SAA** - *Selected Altitude Available*

    - 1 bit [``.``]

    - values:

        | 0: Equipment capable to provide Selected Altitude
        | 1: Equipment not capable to provide Selected Altitude

    **I021/040/CL** - *Confidence Level*

    - 2 bits [``..``]

    - values:

        | 0: Report valid
        | 1: Report suspect
        | 2: No information
        | 3: Reserved for future use

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/040/(spare)**

    - 1 bit [``.``]

    **I021/040/LLC** - *List Lookup Check*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: List Lookup failed (see note)

    **I021/040/IPC** - *Independent Position Check*

    - 1 bit [``.``]

    - values:

        | 0: Default (see note)
        | 1: Independent Position Check failed

    **I021/040/NOGO** - *No-go Bit Status*

    - 1 bit [``.``]

    - values:

        | 0: NOGO-bit not set
        | 1: NOGO-bit set

    **I021/040/CPR** - *Compact Position Reporting*

    - 1 bit [``.``]

    - values:

        | 0: CPR Validation correct
        | 1: CPR Validation failed

    **I021/040/LDPJ** - *Local Decoding Position Jump*

    - 1 bit [``.``]

    - values:

        | 0: LDPJ not detected
        | 1: LDPJ detected

    **I021/040/RCF** - *Range Check*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Range Check failed

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Bit 3 indicates that the position reported by the target is
       within a credible range from the ground station. The range
       check is followed by the CPR validation to ensure that global
       and local position decoding both indicate valid position
       information. Bit 3=1 indicates that the range check was done,
       but the CPR validation is not yet completed.
       Once CPR validation is completed, Bit 3 will be reset to 0.
    2. The second extension signals the reasons for which the report has
       been indicated as suspect (indication Confidence Level (CL) in the
       first extension).
    3. Bit 2 indicates that the Range Check failed, i.e. the target is
       reported outside the credible range for the Ground Station. For
       operational users such a target will be suppressed. In services
       used for monitoring the Ground Station, the target will be
       transmitted with bit 2 indicating the fault condition.
    4. Bit 6, if set to 1, indicates that the position reported by the
       target was validated by an independent means and a discrepancy
       was detected. If no independent position check is implemented,
       the default value “0” is to be used.
    5. Bit 5 represents the setting of the GO/NOGO-bit as defined in
       item I023/100 of category 023 [Ref. 3].
    6. Bit 7, if set to 1, indicates that a lookup in a Black-list/White-list
       failed, indicating that the target may be suspect

I021/070 - Mode 3/A Code in Octal Representation
************************************************

*Definition*: Mode-3/A code converted into octal representation.

*Structure*:

    **I021/070/(spare)**

    - 4 bits [``....``]

    **I021/070/MODE3A** - *Mode-3/A Reply in Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)



I021/071 - Time of Applicability for Position
*********************************************

*Definition*: Time of applicability of the reported position, in the form of elapsed
time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. The time of applicability value is reset to zero at every midnight.
    2. The time of applicability indicates the exact time at which the
       position transmitted in the target report is valid.

I021/072 - Time of Applicability for Velocity
*********************************************

*Definition*: Time of applicability (measurement) of the reported velocity, in the
form of elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. The time of the applicability value is reset to zero at every midnight.
    2. The time of applicability indicates the exact time at which the
       velocity information transmitted in the target report is valid.
    3. This item will not be available in some ADS-B technologies.

I021/073 - Time of Message Reception for Position
*************************************************

*Definition*: Time of reception of the latest position squitter in the Ground Station,
in the form of elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Note:
    - The time of message reception value is reset to zero at every midnight.

I021/074 - Time of Message Reception of Position-High Precision
***************************************************************

*Definition*: Time at which the latest ADS-B position information was received by
the ground station, expressed as fraction of the second of the UTC Time.

*Structure*:

    **I021/074/FSI** - *Full Second Indication*

    - 2 bits [``..``]

    - values:

        | 3: Reserved
        | 2: TOMRp whole seconds = (I021/073) Whole seconds - 1
        | 1: TOMRp whole seconds = (I021/073) Whole seconds + 1
        | 0: TOMRp whole seconds = (I021/073) Whole seconds

    **I021/074/TOMRP** - *Fractional Part of the Time of Message Reception for Position in the Ground Station*

    - 30 bits [``..............................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 30
    - unit: "s"
    - LSB = :math:`1 / {2^{30}}` s = :math:`1 / {1073741824}` s :math:`\approx 9.313225746154785e-10` s



I021/075 - Time of Message Reception for Velocity
*************************************************

*Definition*: Time of reception of the latest velocity squitter in the Ground Station,
in the form of elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Note:
    - The time of message reception value is reset to zero at every midnight.

I021/076 - Time of Message Reception of Velocity-High Precision
***************************************************************

*Definition*: Time at which the latest ADS-B velocity information was received by
the ground station, expressed as fraction of the second of the UTC Time.

*Structure*:

    **I021/076/FSI** - *Full Second Indication*

    - 2 bits [``..``]

    - values:

        | 3: Reserved
        | 2: TOMRp whole seconds = (I021/075) Whole seconds - 1
        | 1: TOMRp whole seconds = (I021/075) Whole seconds + 1
        | 0: TOMRp whole seconds = (I021/075) Whole seconds

    **I021/076/TOMRP** - *Fractional Part of the Time of Message Reception for Position in the Ground Station*

    - 30 bits [``..............................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 30
    - unit: "s"
    - LSB = :math:`1 / {2^{30}}` s = :math:`1 / {1073741824}` s :math:`\approx 9.313225746154785e-10` s



I021/077 - Time of ASTERIX Report Transmission
**********************************************

*Definition*: Time of the transmission of the ASTERIX category 021 report in the
form of elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Note:
    - The time of ASTERIX report transmission value is reset to zero at
      every midnight.

I021/080 - Target Address
*************************

*Definition*: Target address (emitter identifier) assigned uniquely to each target.

*Structure*:

- 24 bits [``........................``]

- raw value



I021/090 - Quality Indicators
*****************************

*Definition*: ADS-B quality indicators transmitted by a/c according to MOPS version.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I021/090/NUCRNACV** - *Navigation Uncertainty Category for Velocity NUCr or the Navigation Accuracy Category for Velocity NACv*

    - 3 bits [``...``]

    - raw value

    **I021/090/NUCPNIC** - *Navigation Uncertainty Category for Position NUCp or Navigation Integrity Category NIC*

    - 4 bits [``....``]

    - raw value

        remark
            Notes:
                1. Apart from the “PIC” item, all items are defined as per the
                   respective link technology protocol version (“MOPS version”,
                   see I021/210).
                2. The primary subfield is kept for backwards compatibility reasons.
                   Version 2 NIC-values shall be mapped accordingly. This is required
                   to ensure that downstream systems, which are not capable of
                   interpreting extensions 2 and 3 (because they use an ASTERIX

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/090/NICBARO** - *Navigation Integrity Category for Barometric Altitude*

    - 1 bit [``.``]

    - raw value

    **I021/090/SIL** - *Surveillance (version 1) or Source (version 2) Integrity Level*

    - 2 bits [``..``]

    - raw value

    **I021/090/NACP** - *Navigation Accuracy Category for Position*

    - 4 bits [``....``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/090/(spare)**

    - 2 bits [``..``]

    **I021/090/SILS** - *SIL-Supplement*

    - 1 bit [``.``]

    - values:

        | 0: Measured per flight-hour
        | 1: Measured per sample

    **I021/090/SDA** - *Horizontal Position System Design Assurance Level (as Defined in Version 2)*

    - 2 bits [``..``]

    - raw value

    **I021/090/GVA** - *Geometric Altitude Accuracy*

    - 2 bits [``..``]

    - raw value

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/090/PIC** - *Position Integrity Category*

    - 4 bits [``....``]

    - raw value

    **I021/090/(spare)**

    - 3 bits [``...``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Apart from the “PIC” item, all items are defined as per the
       respective link technology protocol version (“MOPS version”,
       see I021/210).
    2. The primary subfield is kept for backwards compatibility reasons.
       Version 2 NIC-values shall be mapped accordingly. This is required
       to ensure that downstream systems, which are not capable of
       interpreting extensions 2 and 3 (because they use an ASTERIX
       edition earlier than 2.0) still get the required information
    3. “Version 1” or “Version 2” refers to the MOPS version as defined
       in data item I021/210, bits 6/4
    4. “Version 2” refers to the MOPS version as defined in data item
       I021/210, bits 6/4
    5. PIC=0 is defined for completeness only. In this case the third
       extension shall not be generated.
    6. For ED102A/DO260B PIC values of 7 and 9, the NIC supplements
       for airborne messages (NIC supplements A/B) and surface messages
       (NIC supplements A/C) are listed.
       For ED102A/DO260B PIC=8, the NIC supplements A/B for airborne
       messages are listed.
       For DO260A PIC values of 7 and 8, the NIC supplement for airborne
       messages is shown in brackets.
       The aircraft air-ground status, and hence message type (airborne
       or surface), is derived from the GBS-bit in I021/040, 1 st extension.

I021/110 - Trajectory Intent
****************************

*Definition*: Reports indicating the 4D intended trajectory of the aircraft.

*Structure*:

Compound item (FX)

    **I021/110/TIS** - *Trajectory Intent Status*

    Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

        **I021/110/TIS/NAV**

        - 1 bit [``.``]

        - values:

            | 0: Trajectory Intent Data is available for this aircraft
            | 1: Trajectory Intent Data is not available for this aircraft

        **I021/110/TIS/NVB**

        - 1 bit [``.``]

        - values:

            | 0: Trajectory Intent Data is valid
            | 1: Trajectory Intent Data is not valid

        **I021/110/TIS/(spare)**

        - 5 bits [``.....``]

        ``(FX)``

        - extension bit

            | 0: End of data item
            | 1: Extension into next extent

    **I021/110/TID** - *Trajectory Intent Data*

    Repetitive item, repetition factor 8 bits.

            **I021/110/TID/TCA**

            - 1 bit [``.``]

            - values:

                | 0: TCP number available
                | 1: TCP number not available

            **I021/110/TID/NC**

            - 1 bit [``.``]

            - values:

                | 0: TCP compliance
                | 1: TCP non-compliance

            **I021/110/TID/TCPN**

            Trajectory Change Point number

            - 6 bits [``......``]

            - raw value

            **I021/110/TID/ALT** - *Altitude in Two's Complement Form*

            - 16 bits [``................``]

            - signed quantity
            - scaling factor: 10
            - fractional bits: 0
            - unit: "ft"
            - LSB = :math:`10` ft
            - value :math:`>= -1500` ft
            - value :math:`<= 150000` ft

            **I021/110/TID/LAT** - *In WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - scaling factor: 180
            - fractional bits: 23
            - unit: "deg"
            - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
            - value :math:`>= -90` deg
            - value :math:`<= 90` deg

            **I021/110/TID/LON** - *In WGS.84 in Two's Complement*

            - 24 bits [``........................``]

            - signed quantity
            - scaling factor: 180
            - fractional bits: 23
            - unit: "deg"
            - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
            - value :math:`>= -180` deg
            - value :math:`< 180` deg

            **I021/110/TID/PT** - *Point Type*

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

            **I021/110/TID/TD**

            - 2 bits [``..``]

            - values:

                | 0: N/A
                | 1: Turn right
                | 2: Turn left
                | 3: No turn

            **I021/110/TID/TRA**

            Turn Radius Availability

            - 1 bit [``.``]

            - values:

                | 0: TTR not available
                | 1: TTR available

            **I021/110/TID/TOA**

            - 1 bit [``.``]

            - values:

                | 0: TOV available
                | 1: TOV not available

            **I021/110/TID/TOV** - *Time Over Point*

            - 24 bits [``........................``]

            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 0
            - unit: "s"
            - LSB = :math:`1` s

            **I021/110/TID/TTR** - *TCP Turn Radius*

            - 16 bits [``................``]

            - unsigned quantity
            - scaling factor: 0.01
            - fractional bits: 0
            - unit: "Nm"
            - LSB = :math:`0.01` Nm
            - value :math:`>= 0` Nm
            - value :math:`<= 655.35` Nm


Notes:

    1. NC is set to one when the aircraft will not fly the path described
       by the TCP data.
    2. TCP numbers start from zero.
    3. LT = Lateral Type
    4. VT = Vertical Type
    5. TOV gives the estimated time before reaching the point. It is
       defined as the absolute time from midnight.
    6. TOV is meaningful only if TOA is set to 1.

I021/130 - Position in WGS-84 Co-ordinates
******************************************

*Definition*: Position in WGS-84 Co-ordinates.

*Structure*:

    **I021/130/LAT** - *Latitude*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 23
    - unit: "deg"
    - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I021/130/LON** - *Longitude*

    - 24 bits [``........................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 23
    - unit: "deg"
    - LSB = :math:`180 / {2^{23}}` deg = :math:`180 / {8388608}` deg :math:`\approx 2.1457672119140625e-05` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg


Notes:

    - Positive longitude indicates East. Positive latitude indicates North.

I021/131 - High-Resolution Position in WGS-84 Co-ordinates
**********************************************************

*Definition*: Position in WGS-84 Co-ordinates in high resolution.

*Structure*:

    **I021/131/LAT** - *Latitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 30
    - unit: "deg"
    - LSB = :math:`180 / {2^{30}}` deg = :math:`180 / {1073741824}` deg :math:`\approx 1.6763806343078613e-07` deg
    - value :math:`>= -90` deg
    - value :math:`<= 90` deg

    **I021/131/LON** - *Longitude*

    - 32 bits [``................................``]

    - signed quantity
    - scaling factor: 180
    - fractional bits: 30
    - unit: "deg"
    - LSB = :math:`180 / {2^{30}}` deg = :math:`180 / {1073741824}` deg :math:`\approx 1.6763806343078613e-07` deg
    - value :math:`>= -180` deg
    - value :math:`< 180` deg


Notes:

    - Positive longitude indicates East. Positive latitude indicates North.

I021/132 - Message Amplitude
****************************

*Definition*: Amplitude, in dBm, of ADS-B messages received by the ground station,
coded in two’s complement.

*Structure*:

- 8 bits [``........``]

- signed quantity
- scaling factor: 1
- fractional bits: 0
- unit: "dBm"
- LSB = :math:`1` dBm


Note:
    - The value gives the amplitude of the latest received squitter.

I021/140 - Geometric Height
***************************

*Definition*: Minimum height from a plane tangent to the earth’s ellipsoid, defined
by WGS-84, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 25
- fractional bits: 2
- unit: "ft"
- LSB = :math:`25 / {2^{2}}` ft = :math:`25 / {4}` ft :math:`\approx 6.25` ft
- value :math:`>= -1500` ft
- value :math:`< 150000` ft


Note:
    1. LSB is required to be less than 10 ft by ICAO.
    2. A value of ‘0111111111111111’ indicates that the aircraft transmits
       a “greater than” indication.

I021/145 - Flight Level
***********************

*Definition*: Flight Level from barometric measurements,not QNH corrected, in two’s
complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 1
- fractional bits: 2
- unit: "FL"
- LSB = :math:`1 / {2^{2}}` FL = :math:`1 / {4}` FL :math:`\approx 0.25` FL
- value :math:`>= -15` FL
- value :math:`< 1500` FL



I021/146 - Selected Altitude
****************************

*Definition*: The Selected Altitude as provided by the avionics and corresponding
either to the MCP/FCU Selected Altitude (the ATC cleared altitude
entered by the flight crew into the avionics) or to the FMS Selected Altitude.

*Structure*:

    **I021/146/SAS** - *Source Availability*

    - 1 bit [``.``]

    - values:

        | 0: No source information provided
        | 1: Source Information provided

    **I021/146/S** - *Source*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: Aircraft Altitude (Holding Altitude)
        | 2: MCP/FCU Selected Altitude
        | 3: FMS Selected Altitude

    **I021/146/ALT** - *Altitude*

    - 13 bits [``.............``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 0
    - unit: "ft"
    - LSB = :math:`25` ft
    - value :math:`>= -1300` ft
    - value :math:`< 100000` ft


Notes:

    1. The Selected Altitude provided in this field is not necessarily
       the “Target Altitude” as defined by ICAO.
    2. The value of “Source” (bits 15/14) indicating “unknown” or “Aircraft
       Altitude” is kept for backward compatibility as these indications are
       not provided by “version 2” systems as defined by data item I021/210,
       bits 6/4.
    3. Vertical mode indications supporting the determination of the
       nature of the Selected Altitude are provided in the Reserved
       Expansion Field in the subfield NAV.

I021/148 - Final State Selected Altitude
****************************************

*Definition*: The vertical intent value that corresponds with the ATC cleared altitude,
as derived from the Altitude Control Panel (MCP/FCU).

*Structure*:

    **I021/148/MV** - *Manage Vertical Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active or unknown
        | 1: Active

    **I021/148/AH** - *Altitude Hold Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active or unknown
        | 1: Active

    **I021/148/AM** - *Approach Mode*

    - 1 bit [``.``]

    - values:

        | 0: Not active or unknown
        | 1: Active

    **I021/148/ALT** - *Altitude*

    - 13 bits [``.............``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 0
    - unit: "ft"
    - LSB = :math:`25` ft
    - value :math:`>= -1300` ft
    - value :math:`< 100000` ft


Notes:

    - This item is kept for backward compatibility but shall not be used
      for “version 2” ADS-B systems (as defined by data item I021/210,
      bits 6/4) for which item 146 will be used to forward the MCP/FCU
      or the FMS selected altitude information. For “version 2” ADS-B
      systems, the vertical mode indications will be provided through
      the Reserved Expansion Field in the subfield NAV .

I021/150 - Air Speed
********************

*Definition*: Calculated Air Speed (Element of Air Vector).

*Structure*:

    **I021/150/IM**

    - 1 bit [``.``]

    - values:

        | 0: Air Speed = IAS, LSB (Bit-1) = 2 -14 NM/s
        | 1: Air Speed = Mach, LSB (Bit-1) = 0.001

    **I021/150/AS** - *Air Speed (IAS or Mach)*

    - 15 bits [``...............``]

    * Content of this item depends on the value of item ``150/IM``.

        * In case of ``150/IM == 0``:
            - unsigned quantity
            - scaling factor: 1
            - fractional bits: 14
            - unit: "NM/s"
            - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s

        * In case of ``150/IM == 1``:
            - unsigned quantity
            - scaling factor: 0.001
            - fractional bits: 0
            - unit: "mach"
            - LSB = :math:`0.001` mach




I021/151 - True Airspeed
************************

*Definition*: True Air Speed.

*Structure*:

    **I021/151/RE** - *Range Exceeded Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Value in defined range
        | 1: Value exceeds defined range

    **I021/151/TAS** - *True Air Speed*

    - 15 bits [``...............``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "kt"
    - LSB = :math:`1` kt


Notes:

    - The RE-Bit, if set, indicates that the value to be transmitted is
      beyond the range defined for this specific data item and the
      applied technology. In this case the True Air Speed contains the
      maximum value that can be downloaded from the aircraft avionics
      and the RE-bit indicates that the actual value is greater than the
      value contained in the field.

I021/152 - Magnetic Heading
***************************

*Definition*: Magnetic Heading (Element of Air Vector).

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- scaling factor: 360
- fractional bits: 16
- unit: "deg"
- LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Notes:

    - True North Heading is defined in the Reserved Expansion Field in
      the subfield TNH.

I021/155 - Barometric Vertical Rate
***********************************

*Definition*: Barometric Vertical Rate, in two’s complement form.

*Structure*:

    **I021/155/RE** - *Range Exceeded Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Value in defined range
        | 1: Value exceeds defined range

    **I021/155/BVR** - *Barometric Vertical Rate*

    - 15 bits [``...............``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "feet/min"
    - LSB = :math:`25 / {2^{2}}` feet/min = :math:`25 / {4}` feet/min :math:`\approx 6.25` feet/min


Notes:

    - The RE-Bit, if set, indicates that the value to be transmitted is
      beyond the range defined for this specific data item and the applied
      technology. In this case the Barometric Vertical Rate contains the
      maximum value that can be downloaded from the aircraft avionics and
      the RE-bit indicates that the actual value is greater than the value
      contained in the field.

I021/157 - Geometric Vertical Rate
**********************************

*Definition*: Geometric Vertical Rate, in two’s complement form, with reference to WGS-84.

*Structure*:

    **I021/157/RE** - *Range Exceeded Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Value in defined range
        | 1: Value exceeds defined range

    **I021/157/GVR** - *Geometric Vertical Rate*

    - 15 bits [``...............``]

    - signed quantity
    - scaling factor: 25
    - fractional bits: 2
    - unit: "feet/min"
    - LSB = :math:`25 / {2^{2}}` feet/min = :math:`25 / {4}` feet/min :math:`\approx 6.25` feet/min


Notes:

    - The RE-Bit, if set, indicates that the value to be transmitted is
      beyond the range defined for this specific data item and the applied
      technology. In this case the Geometric Vertical Rate contains the
      maximum value that can be downloaded from the aircraft avionics and
      the RE-bit indicates that the actual value is greater than the value
      contained in the field.

I021/160 - Airborne Ground Vector
*********************************

*Definition*: Ground Speed and Track Angle elements of Airborne Ground Vector.

*Structure*:

    **I021/160/RE** - *Range Exceeded Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Value in defined range
        | 1: Value exceeds defined range

    **I021/160/GS** - *Ground Speed Referenced to WGS-84*

    - 15 bits [``...............``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 14
    - unit: "NM/s"
    - LSB = :math:`1 / {2^{14}}` NM/s = :math:`1 / {16384}` NM/s :math:`\approx 6.103515625e-05` NM/s
    - value :math:`>= 0` NM/s
    - value :math:`< 2` NM/s

    **I021/160/TA** - *Track Angle Clockwise Reference to True North*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 360
    - fractional bits: 16
    - unit: "deg"
    - LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Notes:

    1. The RE-Bit, if set, indicates that the value to be transmitted is
       beyond the range defined for this specific data item and the applied
       technology. In this case the Ground Speed contains the maximum value
       that can be downloaded from the aircraft avionics and the RE-bit
       indicates that the actual value is greater than the value contained
       in the field.
    2. The Surface Ground Vector format is defined in the Reserved Expansion
       Field in the subfield SGV.

I021/161 - Track Number
***********************

*Definition*: An integer value representing a unique reference to a track record
within a particular track file.

*Structure*:

    **I021/161/(spare)**

    - 4 bits [``....``]

    **I021/161/TRNUM** - *Track Number*

    - 12 bits [``............``]

    - raw value



I021/165 - Track Angle Rate
***************************

*Definition*: Rate of Turn, in two’s complement form.

*Structure*:

    **I021/165/(spare)**

    - 6 bits [``......``]

    **I021/165/TAR** - *Track Angle Rate*

    - 10 bits [``..........``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 5
    - unit: "deg/s"
    - LSB = :math:`1 / {2^{5}}` deg/s = :math:`1 / {32}` deg/s :math:`\approx 0.03125` deg/s
    - value :math:`>= -16` deg/s
    - value :math:`<= 16` deg/s


Notes:

    1. A positive value represents a right turn, whereas a negative value
       represents a left turn.
    2. Maximum value means Maximum value or above.
    3. This item will not be transmitted for the technology 1090 MHz
       Extended Squitter.

I021/170 - Target Identification
********************************

*Definition*: Target (aircraft or vehicle) identification in 8 characters, as reported
by the target.

*Structure*:

- 48 bits [``................................................``]

- ICAO string (6-bits per character)



I021/200 - Target Status
************************

*Definition*: Status of the target

*Structure*:

    **I021/200/ICF** - *Intent Change Flag (see Note)*

    - 1 bit [``.``]

    - values:

        | 0: No intent change active
        | 1: Intent change flag raised

    **I021/200/LNAV** - *LNAV Mode*

    - 1 bit [``.``]

    - values:

        | 0: LNAV Mode engaged
        | 1: LNAV Mode not engaged

    **I021/200/ME** - *Military Emergency*

    - 1 bit [``.``]

    - values:

        | 0: No military emergency
        | 1: Military emergency

    **I021/200/PS** - *Priority Status*

    - 3 bits [``...``]

    - values:

        | 0: No emergency / not reported
        | 1: General emergency
        | 2: Lifeguard / medical emergency
        | 3: Minimum fuel
        | 4: No communications
        | 5: Unlawful interference
        | 6: DOWNED Aircraft

    **I021/200/SS** - *Surveillance Status*

    - 2 bits [``..``]

    - values:

        | 0: No condition reported
        | 1: Permanent Alert (Emergency condition)
        | 2: Temporary Alert (change in Mode 3/A Code other than emergency)
        | 3: SPI set


Notes:

    - Bit-8 (ICF), when set to “1” indicates that new information is
      available in the Mode S GICB registers 40, 41 or 42.

I021/210 - MOPS Version
***********************

*Definition*: Identification of the MOPS version used by a/c to supply ADS-B information.

*Structure*:

    **I021/210/(spare)**

    - 1 bit [``.``]

    **I021/210/VNS** - *Version Not Supported*

    - 1 bit [``.``]

    - values:

        | 0: The MOPS Version is supported by the GS
        | 1: The MOPS Version is not supported by the GS

    **I021/210/VN** - *Version Number*

    - 3 bits [``...``]

    - values:

        | 0: ED102/DO-260 [Ref. 8]
        | 1: DO-260A [Ref. 9]
        | 2: ED102A/DO-260B [Ref. 11]

    **I021/210/LTT** - *Link Technology Type*

    - 3 bits [``...``]

    - values:

        | 0: Other
        | 1: UAT
        | 2: 1090 ES
        | 3: VDL 4
        | 4: Not assigned
        | 5: Not assigned
        | 6: Not assigned
        | 7: Not assigned


Notes:

    - VN sub-field shall contain a value describing the MOPS used by each aircraft.
      The versions of other link technologies are assumed to be in line
      with the 1090 ES MOPS versions and the corresponding MASPS versions.

    - Bit 7 (VNS) when set to 1 indicates that the aircraft transmits a
      MOPS Version indication that is not supported by the Ground Station.
      However, since MOPS versions are supposed to be backwards compatible,
      the GS has attempted to interpret the message and achieved a credible
      result. The fact that the MOPS version received is not supported by
      the GS is submitted as additional information to subsequent processing
      systems.

I021/220 - Met Information
**************************

*Definition*: Meteorological information.

*Structure*:

Compound item (FX)

    **I021/220/WS** - *Wind Speed*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "kt"
    - LSB = :math:`1` kt
    - value :math:`>= 0` kt
    - value :math:`<= 300` kt

    **I021/220/WD** - *Wind Direction*

    - 16 bits [``................``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "deg"
    - LSB = :math:`1` deg
    - value :math:`>= 1` deg
    - value :math:`<= 360` deg

    **I021/220/TMP** - *Temperature*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 2
    - unit: "degC"
    - LSB = :math:`1 / {2^{2}}` degC = :math:`1 / {4}` degC :math:`\approx 0.25` degC
    - value :math:`>= -100` degC
    - value :math:`<= 100` degC

    **I021/220/TRB** - *Turbulence*

    - 8 bits [``........``]

    - unsigned integer
    - value :math:`>= 0`
    - value :math:`<= 15`



I021/230 - Roll Angle
*********************

*Definition*: The roll angle, in two’s complement form, of an aircraft executing a turn.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 0.01
- fractional bits: 0
- unit: "deg"
- LSB = :math:`0.01` deg
- value :math:`>= -180` deg
- value :math:`<= 180` deg


Notes:

    1. Negative Value indicates “Left Wing Down”.
    2. Resolution provided by the technology “1090 MHz Extended Squitter”
       is 1 degree.

I021/250 - Mode S MB Data
*************************

*Definition*: Mode S Comm B data as extracted from the aircraft transponder.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 64 bits [``................................................................``]

    - BDS register with address


Notes:

    1. For the transmission of BDS20, item 170 should be used.
    2. For the transmission of BDS30, item 260 is used.

I021/260 - ACAS Resolution Advisory Report
******************************************

*Definition*: Currently active Resolution Advisory (RA), if any, generated by the ACAS
associated with the transponder transmitting the RA message and threat
identity data.

*Structure*:

    **I021/260/TYP** - *Message Type (= 28 for 1090 ES, Version 2)*

    - 5 bits [``.....``]

    - raw value

    **I021/260/STYP** - *Message Sub-type (= 2 for 1090 ES, Version 2)*

    - 3 bits [``...``]

    - raw value

    **I021/260/ARA** - *Active Resolution Advisories*

    - 14 bits [``..............``]

    - raw value

    **I021/260/RAC** - *RAC (RA Complement) Record*

    - 4 bits [``....``]

    - raw value

    **I021/260/RAT** - *RA Terminated*

    - 1 bit [``.``]

    - raw value

    **I021/260/MTE** - *Multiple Threat Encounter*

    - 1 bit [``.``]

    - raw value

    **I021/260/TTI** - *Threat Type Indicator*

    - 2 bits [``..``]

    - raw value

    **I021/260/TID** - *Threat Identity Data*

    - 26 bits [``..........................``]

    - raw value


Notes:

    1. Version denotes the MOPS version as defined in I021/210, bits 6/4
    2. This data items copies the value of BDS register 6,1 for message
       type 28, subtype 2
    3. The “TYP” and “STYP” items are implementation (i.e. link technology)
       dependent.
    4. Refer to ICAO Annex 10 SARPs for detailed explanations [Ref. 10]

I021/271 - Surface Capabilities and Characteristics
***************************************************

*Definition*: Operational capabilities of the aircraft while on the ground.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I021/271/(spare)**

    - 2 bits [``..``]

    **I021/271/POA** - *Position Offset Applied*

    - 1 bit [``.``]

    - values:

        | 0: Position transmitted is not ADS-B position reference point
        | 1: Position transmitted is the ADS-B position reference point

    **I021/271/CDTIS** - *Cockpit Display of Traffic Information Surface*

    - 1 bit [``.``]

    - values:

        | 0: CDTI not operational
        | 1: CDTI operational

    **I021/271/B2LOW** - *Class B2 Transmit Power Less Than 70 Watts*

    - 1 bit [``.``]

    - values:

        | 0: >= 70 Watts
        | 1: < 70 Watts

    **I021/271/RAS** - *Receiving ATC Services*

    - 1 bit [``.``]

    - values:

        | 0: Aircraft not receiving ATC-services
        | 1: Aircraft receiving ATC services

    **I021/271/IDENT** - *Setting of IDENT Switch*

    - 1 bit [``.``]

    - values:

        | 0: IDENT switch not active
        | 1: IDENT switch active

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I021/271/LW** - *Length and Width of the Aircraft*

    - 4 bits [``....``]

    - raw value

    **I021/271/(spare)**

    - 3 bits [``...``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. Version 2 (as defined in I021/210, bits 6/4) data technology
       protocols encode “No Data or Unknown” with value 0. In this
       case data item I021/271, first extension is not generated.
    2. As of edition 2.2 the structure of this data item has been changed.
       Edition 2.2 is not backwards compatible with previous editions.

I021/295 - Data Ages
********************

*Definition*: Ages of the data provided.

*Structure*:

Compound item (FX)

    **I021/295/AOS** - *Aircraft Operational Status Age*

    Age of the information transmitted in item I021/008.

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TRD** - *Target Report Descriptor Age*

    Age of the Target Report Descriptor, item I021/040

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/M3A** - *Mode 3/A Age*

    Age of the Mode 3/A Code, item I021/070

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/QI** - *Quality Indicators Age*

    Age of the Quality Indicators, item I021/090

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TI1** - *Trajectory Intent Age*

    Age of the Trajectory Intent information, item I021/110

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/MAM** - *Message Amplitude Age*

    Age of the message amplitude, item I021/132

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/GH** - *Geometric Height Age*

    Age of the Geometric Height, item 021/140

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/FL** - *Flight Level Age*

    Age of the Flight Level, item I021/145

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/ISA** - *Intermediate State Selected Altitude Age*

    Age of the Intermediate State Selected Altitude, item I021/146

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/FSA** - *Final State Selected Altitude Age*

    Age of the Final State Selected Altitude, item I021/148

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/AS** - *Air Speed Age*

    Age of the Air Speed, item I021/150

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TAS** - *True Air Speed Age*

    Age of the True Air Speed, item I021/151

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/MH** - *Magnetic Heading Age*

    Age of the Magnetic Heading, item I021/152

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/BVR** - *Barometric Vertical Rate Age*

    Age of the Barometric Vertical Rate, item I021/155

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/GVR** - *Geometric Vertical Rate Age*

    Age of the Geometric Vertical Rate, item I021/157

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/GV** - *Ground Vector Age*

    Age of the Ground Vector, item I021/160

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TAR** - *Track Angle Rate Age*

    Age of the Track Angle Rate, item I021/165

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TI2** - *Target Identification Age*

    Age of the Target Identification, item I021/170

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/TS** - *Target Status Age*

    Age of the Target Status, item I021/200

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/MET** - *Met Information Age*

    Age of the Meteorological Information, item I021/220

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/ROA** - *Roll Angle Age*

    Age of the Roll Angle, item I021/230

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/ARA** - *ACAS Resolution Advisory Age*

    Age of the latest update of an active ACAS Resolution Advisory, item I021/260

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s

    **I021/295/SCC** - *Surface Capabilities and Characteristics Age*

    Age of the information on the surface capabilities and characteristics of the respective target, item I021/271

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 0.1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`0.1` s
    - value :math:`<= 25.5` s


Notes:

    - In all the subfields, the maximum value indicates “maximum value
      or above”.

I021/400 - Receiver ID
**********************

*Definition*: Designator of Ground Station in Distributed System.

*Structure*:

- 8 bits [``........``]

- raw value



I021/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I021/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 021
=========================================
- (1) ``I021/010`` - Data Source Identification
- (2) ``I021/040`` - Target Report Descriptor
- (3) ``I021/161`` - Track Number
- (4) ``I021/015`` - Service Identification
- (5) ``I021/071`` - Time of Applicability for Position
- (6) ``I021/130`` - Position in WGS-84 Co-ordinates
- (7) ``I021/131`` - High-Resolution Position in WGS-84 Co-ordinates
- ``(FX)`` - Field extension indicator
- (8) ``I021/072`` - Time of Applicability for Velocity
- (9) ``I021/150`` - Air Speed
- (10) ``I021/151`` - True Airspeed
- (11) ``I021/080`` - Target Address
- (12) ``I021/073`` - Time of Message Reception for Position
- (13) ``I021/074`` - Time of Message Reception of Position-High Precision
- (14) ``I021/075`` - Time of Message Reception for Velocity
- ``(FX)`` - Field extension indicator
- (15) ``I021/076`` - Time of Message Reception of Velocity-High Precision
- (16) ``I021/140`` - Geometric Height
- (17) ``I021/090`` - Quality Indicators
- (18) ``I021/210`` - MOPS Version
- (19) ``I021/070`` - Mode 3/A Code in Octal Representation
- (20) ``I021/230`` - Roll Angle
- (21) ``I021/145`` - Flight Level
- ``(FX)`` - Field extension indicator
- (22) ``I021/152`` - Magnetic Heading
- (23) ``I021/200`` - Target Status
- (24) ``I021/155`` - Barometric Vertical Rate
- (25) ``I021/157`` - Geometric Vertical Rate
- (26) ``I021/160`` - Airborne Ground Vector
- (27) ``I021/165`` - Track Angle Rate
- (28) ``I021/077`` - Time of ASTERIX Report Transmission
- ``(FX)`` - Field extension indicator
- (29) ``I021/170`` - Target Identification
- (30) ``I021/020`` - Emitter Category
- (31) ``I021/220`` - Met Information
- (32) ``I021/146`` - Selected Altitude
- (33) ``I021/148`` - Final State Selected Altitude
- (34) ``I021/110`` - Trajectory Intent
- (35) ``I021/016`` - Service Management
- ``(FX)`` - Field extension indicator
- (36) ``I021/008`` - Aircraft Operational Status
- (37) ``I021/271`` - Surface Capabilities and Characteristics
- (38) ``I021/132`` - Message Amplitude
- (39) ``I021/250`` - Mode S MB Data
- (40) ``I021/260`` - ACAS Resolution Advisory Report
- (41) ``I021/400`` - Receiver ID
- (42) ``I021/295`` - Data Ages
- ``(FX)`` - Field extension indicator
- (43) ``(spare)``
- (44) ``(spare)``
- (45) ``(spare)``
- (46) ``(spare)``
- (47) ``(spare)``
- (48) ``I021/RE`` - Reserved Expansion Field
- (49) ``I021/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

