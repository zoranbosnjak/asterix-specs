Asterix category 015 - Independent Non-Cooperative Surveillance System Target Reports
=====================================================================================
**category**: 015

**edition**: 1.2

**date**: 2024-05-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I015/000 - Message Type
***********************

*Definition*: This data item conveys the report type and whether the output is
periodically updated or asynchronous depending upon external events.

*Structure*:

    **I015/000/MT** - *Message Type*

    - 7 bits [``.......``]

    - values:

        | 1: Measurement Plot
        | 2: Measurement Track
        | 3: Sensor Centric Plot
        | 4: Sensor Centric Track
        | 5: Track End Message

    **I015/000/RG** - *Report Generation*

    - 1 bit [``.``]

    - values:

        | 0: Periodic Report
        | 1: Event Driven Report

Note 1: See Section 4.7 and ANNEX A for definitions of the Message
Types.

Note 2: Values 6 to 127 are reserved for future use

Note 3: Periodic Report: A periodic report is one transmitted
periodically with an independently configurable period.

Note 4: Event Driven Report: An Event Driven Report is one
generated in response to the occurrence of an external event such
as an RF echo off a target.

I015/010 - Data Source Identifier
*********************************

*Definition*: Identification of the sensor from which the data is received.

*Structure*:

    **I015/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I015/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note 1: The up-to-date list of SACs is published on the EUROCONTROL
Web Site (http://www.eurocontrol.int/asterix).

Note 2: The SICs are allocated by the national authority
responsible for the surveillance infrastructure.

Note 3: The SIC and SAC values shall be formatted as binary
unsigned integers.

I015/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value

Note 1: The Service Identification is allocated by the system.

Note 2: The SID value shall be formatted as binary unsigned
integers.

I015/020 - Target Report Descriptor
***********************************

*Definition*: Type and characteristics of the data as transmitted by a system.

*Structure*:

Extended item.

    **I015/020/MOMU** - *Mono-Static Target Report or Multi-Static Target Report*

    - 2 bits [``..``]

    - values:

        | 0: Mono-Static Sensor
        | 1: Multi-Static Sensor
        | 2: Other
        | 3: Unknown

    **I015/020/TTAX** - *Target Taxonomy*

    - 2 bits [``..``]

    - values:

        | 0: Actual Target Report
        | 1: Reference Target
        | 2: Synthetic Target
        | 3: Simulated / Replayed Target

    **I015/020/SCD** - *Scanning Direction*

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: Forward
        | 2: Backward
        | 3: Static

    **I015/020/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Note 1: The MoMu bit is used to indicate whether the target report
was constructed from a multi-static (including bi-static) or
mono-static sensor. Its setting dictates the interpretation of data
items I015/625 and I015/626. The meaning of the value “other” shall
be described in the system ICD.

Note 2: In this context, a Reference Target Report stems from
a non-aircraft target based on RF received externally to the system
boundary. This may be generated, for example, by an external RF
generator or a Permanent Echo or from a device, which is deployed
in line of sight of the sensor.

Note 3: A synthetic target is an internally generated diagnostic
signal prior to the generation of the ASTERIX Category 015 target
report. For example used to support test processes.

Note 4: This value is used to represent externally generated
targets or recorded data injected into the output data stream
of the INCS system e.g. for test or training purposes.

Note 5: This indication is used to inform about the scanning
direction of the system (e.g. left/right, up/down,
clockwise/anti-clockwise). It’s exact meaning is implementation
dependent and shall be described in the system ICD.

I015/030 - Warning/Error Conditions
***********************************

*Definition*: Warning/error conditions detected by a system for the target report
involved.

*Structure*:

Repetitive item with FX extension

    - 7 bits [``.......``]

    - raw value

Note 1: It has to be stressed that a series of one or more W/E
conditions can be reported per target report.

Note 2: The nature of the warning / error condition may differ
between sensor types and the declaration and use of such alerts
is driven by end user requirements.

Note 3: Potential applications could be to indicate that the target
report correlates with road infrastructure (terrestrial vehicles)
or a wind turbine or that it is a fixed or slow moving return or
originating from an area of high clutter. Such data items could
also be used to indicate the presence of interference – either
deliberate or accidental.

Note 4: The Warning/Error Condition Values from 1-31 are reserved
for designation by the ASTERIX Maintenance Group. System
implementers are free to use values of 32 and above. The allocation
of the remaining values of this data item shall be defined in a
local Interface Control Document.

Note 5: The value of “0” must not be assigned.

I015/050 - Update Period
************************

*Definition*: Period until next expected output of a target report for this
target.

*Structure*:

    **I015/050/(spare)**

    - 2 bits [``..``]

    **I015/050/UPD** - *Update Period*

    - 14 bits [``..............``]

    - unsigned quantity
    - unit: "s"
    - LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s
    - value :math:`<= 128` s

Note 1: It is not necessary that all targets detected by the INCS
sensor have target reports generated at the same update period.

Note 2: This data item indicates the period until the next expected
output of a target report for this target relative to the Time
of Applicability contained in data item I015/145

I015/145 - Time of Applicability
********************************

*Definition*: Absolute time stamping for applicability of the measured
information expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s
- value :math:`< 86400` s

Note 1: The Time of Applicability refers to the information
contained in data item I015/600, I015/601, I015/625, I015/626,
I015/627, I015/628 whichever is available. In case of a Track End
Message (Message Type = 5) it refers to the time at which the track
is terminated and the track number (data item I015/161) is released
for re-use.

Note 2: A distributed sensor, such as an MSPSR, may have multiple
elements that are each individually time stamped which are
consolidated in to a target report. Rather than provide details
of each time stamped message, this data item conveys the time
of applicability of position of the target report.

Note 3: The Time of Applicability value is reset to zero each day
at midnight.

Note 4: The Time of Applicability value shall be formatted as a
binary unsigned integer.

I015/161 - Track/Plot Number
****************************

*Definition*: An integer value representing a unique reference to a track/plot
record.

*Structure*:

- 16 bits [``................``]

- unsigned integer
- value :math:`<= 65535`

Note 1: Track numbers are required for Sensor and Measurement
Tracks. However, for Sensor and Measurement Plots the inclusion
of a track number is optional – depending upon whether the INCS
sensor has used tracking processing to reduce the false alarm
rates.

Note 2: The track number is allocated by the system.

Note 3: The track number value shall be formatted as binary
unsigned integers.

I015/170 - Track/Plot Status
****************************

*Definition*: Status of Track/Plot.

*Structure*:

Extended item.

    **I015/170/BIZ**

    - 1 bit [``.``]

    - values:

        | 0: Target not in Blind Zone
        | 1: Target in Blind Zone

    **I015/170/BAZ**

    - 1 bit [``.``]

    - values:

        | 0: Target not in Blanked Zone
        | 1: Target in Blanked Zone

    **I015/170/TUR**

    - 1 bit [``.``]

    - values:

        | 0: Track Alive
        | 1: Track Terminated by User Request

    **I015/170/(spare)**

    - 1 bit [``.``]

    **I015/170/CSTP** - *Coasted - Position*

    - 1 bit [``.``]

    - values:

        | 0: Not extrapolated
        | 1: Extrapolated

    **I015/170/CSTH** - *Coasted – Height*

    - 1 bit [``.``]

    - values:

        | 0: Not extrapolated
        | 1: Extrapolated

    **I015/170/CNF** - *Confirmed vs. Tentative Track*

    - 1 bit [``.``]

    - values:

        | 0: Confirmed Track
        | 1: Tentative Track

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Note 1: The indication for CSTP and/or CSTH applies only to data
items I015/600 and I015/605 respectively. In case one of these data
items is not present, CSTP and/or CSTH has no meaning.

Note 2: A coasted track is one for which the sensor detections have
been interrupted and whose position/height is being predicted based
on the previously received responses.

Note 3: The blind zone or blanked zone are predictable zones where
no detection is predicted.
If bit 5 is set and TTS = 1 then the track is coasted because it is
in a blind zone or sector blank zone.

Note 4: The indication TUR=1 shall be sent only with
Message Type = 5 “Track End Message”.

I015/270 - Target Size & Orientation
************************************

*Definition*: Data item containing the size and orientation information of the
target.

*Structure*:

Compound item (FX)

    **I015/270/LEN** - *Target Length*

    The target length is the longest dimension in the targets
    direction of motion

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`<= 13107/20` m

    **I015/270/WDT** - *Target Width*

    The target width is the longest dimension orthogonal to
    the targets direction of motion

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`<= 13107/20` m

    **I015/270/HGT** - *Target Height*

    The target height is the longest dimension in the vertical
    direction.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`<= 13107/20` m

    **I015/270/ORT** - *Target Orientation*

    The orientation gives the direction, which the target nose
    is pointing, relative to the Geographical North.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`<= 360` °

        remark
            Note: The orientation shall increment in a clockwise manner
            relative to Geographic North.

Note: If length and width cannot be clearly determined, the greater
value of the two shall be transmitted as length.

I015/300 - Object Classification
********************************

*Definition*: Classification result of the object detection.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I015/300/CLS** - *Classification*

        - 9 bits [``.........``]

        - unsigned integer

        **I015/300/PRB** - *Probability*

        - 7 bits [``.......``]

        - unsigned integer

Note 1: INCS processing may be able to provide an indication of the
nature of the target e.g. road vehicle or aircraft with the
potential for further discrimination in the type of the aircraft
e.g. two engine, fixed wing/helicopter etc.
Before including requirements for target classification it is
necessary to consider the operational manner in which such
information would be used and how/if such data would be made
available to the controllers.

Note 2: Target classification is attributing, with an acceptable
degree of confidence, a target report as having originated from
a specific object or target type e.g. fixed wing aircraft, flock
of birds etc. (It should be noted that the probabilities assigned
to a target do not necessarily need to add up to 100%)
The ability of an INCS sensor to classify the targets it detects
is dependent upon the systems capabilities and is driven by end
user requirements. The use of this optional data item is to be
agreed between parties such as the system manufacturer, the system
operator agency and the end user. The allocation of the CLS octet
is to be defined in a local Interface Control Document that shall
be agreed by both parties.

Note 3: If this functionality is implemented in the sensor, the
classification result (CLS), PRB and REP value shall be formatted
as a binary unsigned integer.

I015/400 - Measurement Identifier
*********************************

*Definition*: An identifier pointing to a measurement that was created from a
specific contributing Tx/Rx Pair where the Pair Identifier refers
to the index which details both the transmitter characteristics
(DVB-T, DAB, FM, dedicated etc.) and the receiver characteristics.
These are defined in ASTERIX Category 016 – Data Item I016/300).

*Structure*:

    **I015/400/PID** - *Pair Identifier*

    - 16 bits [``................``]

    - unsigned integer

    **I015/400/ON** - *Observation Number*

    - 24 bits [``........................``]

    - unsigned integer

Note 1: INCS sensors may achieve their operational requirements
based upon different techniques and technologies. Some may utilise
multiple transmitter stations or multiple receiver stations.
This data items provides the means for subsequent processing stages
to be able to analyse the target report data based upon the system
components that contributed to the formation of the target report.
See ANNEX A for further details.

Note 2: The Pair Identifier shall be defined in ASTERIX Category
016 (Data Item I016/300).

Note 3: The Observation Number is identifying an element of
‘raw data’ information. The sensor plots and sensor tracks are
usually composed of several

I015/480 - Associations
***********************

*Definition*: Information on which Measurement Identifiers contributed to the
Sensor Centric Plot / Sensor Centric Track.

*Structure*:

Repetitive item, repetition factor 8 bits.

    - 40 bits [``... 40 bits ...``]

    - raw value

I015/600 - Horizontal Position Information
******************************************

*Definition*: Data item containing the horizontal position information of the
target.

*Structure*:

Compound item (FX)

    **I015/600/P84** - *Horizontal Position in WGS-84 Coordinates*

    Position of a target in WGS-84 Coordinates.

        **I015/600/P84/LATITUDE**

        Latitude in WGS-84, in Two’s complement.

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -90` °
        - value :math:`< 90` °

        **I015/600/P84/LONGITUDE**

        Longitude in WGS-84, in Two’s complement.

        - 32 bits [``................................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^31` ° :math:`\approx 8.38e-8` °
        - value :math:`>= -180` °
        - value :math:`< 180` °

        remark
            Note: The LSB provides a resolution of 1cm. Positive
            longitude indicates East. Positive latitude indicates North.

    **I015/600/HPR** - *Horizontal Position Resolution*

    A horizontal 2D dimensional area (ellipse) within which the
    sensor is unable to resolve two separate targets.

        **I015/600/HPR/RSHPX**

        Horizontal position resolution of the target in
        target centric Cartesian coordinates (X-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m
        - value :math:`<= 32767` m

        **I015/600/HPR/RSHPY**

        Horizontal position resolution of the target in
        target centric Cartesian coordinates (Y-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`1/2` m :math:`\approx 0.50` m
        - value :math:`<= 32767` m

        **I015/600/HPR/CORSHPXY**

        Correlation of horizontal position resolution
        of X and Y components, in Two’s complement.

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/600/HPP** - *Horizontal Position Precision*

    The distribution of horizontal position random errors.

        **I015/600/HPP/SDHPX**

        Standard Deviation of horizontal position of the
        target in target centric Cartesian coordinates
        (X-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`1/2^2` m :math:`\approx 0.25` m
        - value :math:`<= 65535/4` m

        **I015/600/HPP/SDHPY**

        Standard Deviation of horizontal position of the
        target in target centric Cartesian coordinates
        (Y-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`1/2^2` m :math:`\approx 0.25` m
        - value :math:`<= 65535/4` m

        **I015/600/HPP/COSDHPXY**

        Correlation of standard deviation of horizontal
        position of X and Y components, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/601 - Geometric Height Information
***************************************

*Definition*: Data item containing the geometric height information of the target
in WGS 84 height above ellipsoid.

*Structure*:

Compound item (FX)

    **I015/601/GH** - *Geometric Height (WGS-84)*

    Vertical distance between the target and the projection
    of its position on the earth’s ellipsoid, as defined by
    WGS-84, in Two’s complement form.

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`>= -83286` m
    - value :math:`<= 83286` m

    **I015/601/RSGH** - *Geometric Height Resolution*

    Vertical distance within which the sensor is unable to
    resolve two separate targets.

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`<= 3355443/20` m

    **I015/601/SDGH** - *Geometric Height Precision*

    The distribution of random Geometric Height errors (see
    also the definition of Precision in the appendix).

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/100` m :math:`\approx 1.00e-2` m
    - value :math:`<= 3355443/20` m

    **I015/601/CI6** - *Confidence Interval for Geometric Height (67%)*

    A measure of the uncertainty within which 67% of geometric
    height measurements will be contained.

        **I015/601/CI6/UCI6**

        Upper confidence interval for Geometric Height (67%)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`16` m
        - value :math:`<= 65520` m

        **I015/601/CI6/LCI6**

        Lower confidence interval for Geometric Height (67%)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`16` m
        - value :math:`<= 65520` m

    **I015/601/CI9** - *Confidence Interval for Geometric Height (95%)*

    A measure of the certainty within which 95% of geometric
    height measurements will be contained.

        **I015/601/CI9/UCI9**

        Upper confidence interval for Geometric Height (95%)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`16` m
        - value :math:`<= 65520` m

        **I015/601/CI9/LCI9**

        Lower confidence interval for Geometric Height (95%)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`16` m
        - value :math:`<= 65520` m

    **I015/601/COGHHP** - *Correlation of Geometric Height and Horizontal Position*

    Correlation of Geometric Height converted into metres and
    Horizontal Position of X/Y-components.

        **I015/601/COGHHP/X**

        Correlation of Geometric Height converted into
        metres and Horizontal Position of (X-component),
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/601/COGHHP/Y**

        Correlation of Geometric Height converted into
        metres and Horizontal Position of (Y-component),
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/601/COGHHV** - *Correlation of Geometric Height and Horizontal Velocity*

    Correlation of Geometric Height converted into metres and
    Horizontal Velocity.

        **I015/601/COGHHV/X**

        Correlation of Geometric Height converted into
        metres and Horizontal Velocity of (X-component),
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/601/COGHHV/Y**

        Correlation of Geometric Height converted into
        metres and Horizontal Velocity of (Y-component),
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/601/COGHHA** - *Correlation of Geometric Height and Horizontal Acceleration*

    Correlation of Geometric Height converted into metres and
    Horizontal Acceleration of X/Y-components.

        **I015/601/COGHHA/X**

        Correlation of Geometric Height converted into
        metres and Horizontal Acceleration of
        (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/601/COGHHA/Y**

        Correlation of Geometric Height converted into
        metres and Horizontal Acceleration of
        (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/602 - Horizontal Velocity Information
******************************************

*Definition*: Magnitude of the Horizontal Velocity Vector.

*Structure*:

Compound item (FX)

    **I015/602/HV** - *Horizontal Velocity Vector*

    Horizontal velocity vector expressed in target centric
    Cartesian coordinates.

        **I015/602/HV/X**

        Horizontal Velocity (X-component), in Two’s
        complement

        - 20 bits [``....................``]

        - signed quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`>= -524287/100` m/s
        - value :math:`<= 524287/100` m/s

        **I015/602/HV/Y**

        Horizontal Velocity in (Y-component), in Two’s
        complement

        - 20 bits [``....................``]

        - signed quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`>= -524287/100` m/s
        - value :math:`<= 524287/100` m/s

    **I015/602/RSHV** - *Horizontal Velocity Resolution*

    Minimum difference in Horizontal Velocity at which a sensor
    system is able to distinguish two targets with otherwise
    identical parameters in range and angular domain (under
    ideal measurement).

        **I015/602/RSHV/X**

        Horizontal velocity resolution of the target in
        target centric Cartesian coordinates (X-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`<= 13107/20` m/s

        **I015/602/RSHV/Y**

        Horizontal velocity resolution of the target in
        target centric Cartesian coordinates (Y-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`<= 13107/20` m/s

        **I015/602/RSHV/CORSHVXY**

        Correlation of horizontal position resolution of
        X and Y components, in Two’s complement.

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        remark
            Note: The velocity resolution describes the capability
            of a sensor to be able to separate two closely located
            targets in the velocity domain. The velocity resolution
            for an INCS system depends on the signal integration time
            as well as the transmit frequency. A longer integration
            time or a higher transmit frequency leads to a better
            velocity resolution. Depending on the specific INCS
            application, a better velocity resolution may be more
            important than a high sensor refresh rate. The velocity
            resolution might also alleviate the limitations on the
            range resolution, in order to make small bandwidth
            applications possible.

    **I015/602/SDHV** - *Horizontal Velocity Precision*

    Root-mean-square (rms) error of the Horizontal Velocity
    estimate provided by a sensor system.

        **I015/602/SDHV/X**

        Standard Deviation of horizontal velocity
        (X-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`<= 13107/20` m/s

        **I015/602/SDHV/Y**

        Standard Deviation of horizontal velocity
        (Y-component)

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`<= 13107/20` m/s

        **I015/602/SDHV/COHVXY**

        Correlation of standard deviation of horizontal
        velocity of X and Y components, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/602/COHVHP** - *Correlation of Horizontal Velocity and Horizontal Position*

    Correlation of the errors associated with the estimates
    of Horizontal Velocity and Horizontal Position provided
    by a sensor system.

        **I015/602/COHVHP/COHVXHPX**

        Correlation of Horizontal Velocity (X-component)
        and Horizontal Position (X-component), in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/602/COHVHP/COHVXHPY**

        Correlation of Horizontal Velocity (X-component)
        and Horizontal Position (Y-component), in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/602/COHVHP/COHVYHPX**

        Correlation of Horizontal Velocity (Y-component)
        and Horizontal Position (X-component), in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/602/COHVHP/COHVYHPY**

        Correlation of Horizontal Velocity (Y-component)
        and Horizontal Position (Y-component), in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/603 - Horizontal Acceleration Information
**********************************************

*Definition*: Magnitude of the Horizontal Acceleration Vector.

*Structure*:

Compound item (FX)

    **I015/603/HA** - *Horizontal Acceleration Vector*

    Horizontal acceleration vector expressed in target centric
    local Cartesian coordinates, in Two’s complement
    representation.

        **I015/603/HA/X**

        Horizontal Acceleration (X-component), in Two’s
        complement

        - 12 bits [``............``]

        - signed quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
        - value :math:`>= -128` m/s²
        - value :math:`<= 128` m/s²

        **I015/603/HA/Y**

        Horizontal Acceleration (Y-component), in Two’s
        complement

        - 12 bits [``............``]

        - signed quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
        - value :math:`>= -128` m/s²
        - value :math:`<= 128` m/s²

    **I015/603/SDHA** - *Horizontal Acceleration Precision*

    Root-mean-square (rms) error of the Horizontal Acceleration
    estimate provided by a sensor system.

        **I015/603/SDHA/X**

        Standard Deviation of Horizontal Acceleration
        (X-component)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
        - value :math:`<= 4095/16` m/s²

        **I015/603/SDHA/Y**

        Standard Deviation of Horizontal Acceleration
        (Y-component)

        - 12 bits [``............``]

        - unsigned quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^4` m/s² :math:`\approx 6.25e-2` m/s²
        - value :math:`<= 4095/16` m/s²

        **I015/603/SDHA/COHAXY**

        Correlation of standard deviation of Horizontal
        Acceleration of X and Y components, in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/603/COHAHP** - *Correlation of Horizontal Acceleration and Horizontal Position*

    Correlation of the errors associated with the estimates
    of Horizontal Acceleration and Horizontal Position provided
    by a sensor system.

        **I015/603/COHAHP/COHAXHPX**

        Correlation of Horizontal Acceleration
        (X-component) and Horizontal Position
        (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHP/COHAXHPY**

        Correlation of Horizontal Acceleration
        (X-component) and Horizontal Position
        (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHP/COHAYHPX**

        Correlation of Horizontal Acceleration
        (Y-component) and Horizontal Position
        (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHP/COAYHPY**

        Correlation of Horizontal Acceleration
        (Y-component) and Horizontal Position
        (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/603/COHAHV** - *Correlation of Horizontal Acceleration and Horizontal Velocity*

    Correlation of the errors associated with the estimates
    of Horizontal Acceleration and Horizontal Velocity provided
    by a sensor system.

        **I015/603/COHAHV/COHAXHVX**

        Correlation of Horizontal Acceleration
        (X-component) and Horizontal Velocity
        (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHV/COHAXHVY**

        Correlation of Horizontal Acceleration
        (X-component) and Horizontal Velocity
        (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHV/COHAYHVX**

        Correlation of Horizontal Acceleration
        (Y-component) and Horizontal Velocity
        (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/603/COHAHV/COHAYHVY**

        Correlation of Horizontal Acceleration
        (Y-component) and Horizontal Velocity
        (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/604 - Vertical Velocity Information
****************************************

*Definition*: Vertical velocity as given by the rate of change of the Geometric
Height.

*Structure*:

Compound item (FX)

    **I015/604/VV** - *Vertical Velocity*

    Vertical velocity as given by the rate of change of the
    Geometric Height expressed in Two’s Complement.

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m/s"
    - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
    - value :math:`>= -8388607/100` m/s
    - value :math:`<= 8388607/100` m/s

        remark
            Note: Positive values indicates climbing target and
            negative values indicates descending target.

    **I015/604/RSVV** - *Vertical Velocity Resolution*

    Minimum difference in Vertical Velocity at which a sensor
    system is able to distinguish two targets with otherwise
    identical parameters in range and angular domain (under
    ideal measurement).

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m/s"
    - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
    - value :math:`<= 13107/20` m/s

    **I015/604/SDVV** - *Vertical Velocity Precision*

    Root-mean-square (rms) error of the Vertical Velocity
    estimate provided by a sensor system.

        **I015/604/SDVV/SDVV**

        Standard Deviation of Vertical Velocity

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
        - value :math:`<= 13107/20` m/s

        **I015/604/SDVV/COVVGH**

        Correlation of Vertical Velocity and Geometric
        Height, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/604/COVVHP** - *Correlation of Vertical Velocity and Horizontal Position*

    Correlation of the errors associated with the estimates
    of Vertical Velocity converted in to metres/sec and
    Horizontal Position provided by a sensor system.

        **I015/604/COVVHP/X**

        Correlation of Vertical Velocity and Horizontal
        Position (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/604/COVVHP/Y**

        Correlation of Vertical Velocity and Horizontal
        Position (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/604/COVVHV** - *Correlation of Vertical Velocity and Horizontal Velocity*

    Correlation of the errors associated with the estimates
    of Vertical Velocity converted in to metres/sec and
    Horizontal Velocity provided by a sensor system.

        **I015/604/COVVHV/X**

        Correlation of Vertical Velocity and Horizontal
        Velocity (X-component)), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/604/COVVHV/Y**

        Correlation of Vertical Velocity and Horizontal
        Velocity (Y-component)), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/604/COVVHA** - *Correlation of Vertical Velocity and Horizontal Acceleration*

    Correlation of the errors associated with the estimates
    of Vertical Velocity converted in to metres/sec and
    Horizontal Acceleration provided by a sensor system.

        **I015/604/COVVHA/X**

        Correlation of Vertical and Horizontal Acceleration
        (X-component)), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/604/COVVHA/Y**

        Correlation of Vertical Velocity and Horizontal
        Acceleration (Y-component)), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/605 - Vertical Velocity Information
****************************************

*Definition*: Compound data item, comprising a primary subfield of one-octet,
followed by one or more defined subfields.

*Structure*:

Compound item (FX)

    **I015/605/VA** - *Vertical Acceleration*

    Vertical acceleration information expressed in Two’s
    complement.

    - 16 bits [``................``]

    - signed quantity
    - unit: "m/s²"
    - LSB = :math:`1/100` m/s² :math:`\approx 1.00e-2` m/s²
    - value :math:`>= -32767/100` m/s²
    - value :math:`<= 32767/100` m/s²

        remark
            Note: Positive values indicates accelerating during climb
            or descent and negative values indicates deceleration
            during climb or descent.

    **I015/605/RSVA** - *Vertical Acceleration Precision*

    Root-mean-square (rms) error of the Vertical Acceleration
    estimate provided by a sensor system.

        **I015/605/RSVA/SDVA**

        Standard Deviation of Vertical Acceleration

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s²"
        - LSB = :math:`1/100` m/s² :math:`\approx 1.00e-2` m/s²
        - value :math:`<= 3355443/20` m/s²

        **I015/605/RSVA/COVAGH**

        Correlation of Vertical Acceleration and Geometric
        Height, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/605/RSVA/COVAVV**

        Correlation of Vertical Acceleration and Vertical
        Velocity, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/605/COVAHP** - *Correlation of Vertical Acceleration and Horizontal Position*

    Correlation of the errors associated with the estimates
    of Vertical Acceleration and Horizontal Position provided
    by a sensor system.

        **I015/605/COVAHP/X**

        Correlation of Vertical Acceleration and Horizontal
        Position (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/605/COVAHP/Y**

        Correlation of Vertical Acceleration and Horizontal
        Position (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/605/COVAHV** - *Correlation of Vertical Acceleration and Horizontal Velocity*

    Correlation of the errors associated with the estimates
    of Vertical Acceleration and Horizontal Velocity provided
    by a sensor system.

        **I015/605/COVAHV/X**

        Correlation of Vertical Acceleration and Horizontal
        Velocity (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/605/COVAHV/Y**

        Correlation of Vertical Acceleration and Horizontal
        Velocity (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/605/COVAHA** - *Correlation of Vertical Acceleration and Horizontal Acceleration*

    Correlation of the errors associated with the estimates
    of Vertical Acceleration and Horizontal Acceleration
    provided by a sensor system.

        **I015/605/COVAHA/X**

        Correlation of Vertical Acceleration and Horizontal
        Acceleration (X-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/605/COVAHA/Y**

        Correlation of Vertical Acceleration and Horizontal
        Acceleration (Y-component), in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/625 - Range Information
****************************

*Definition*: The targets range information is given relative to the sensor
reference point(s).

*Structure*:

Compound item (FX)

    **I015/625/R** - *Range*

    Measured range between a target object and a pre-defined
    point associated with the sensor system (e.g., for
    a mono-static radar system the phase centre of the antenna
    aperture) or measured bistatic range between a pre-defined
    point associated with the transmitter station, the target
    object position and a pre-defined point associated with
    the sensor system (e.g., for a bistatic radar system
    the phase centres of the transmitter and receiver antenna
    aperture).

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m"
    - LSB = :math:`1/10` m :math:`\approx 0.10` m
    - value :math:`>= -8388607/10` m
    - value :math:`<= 8388607/10` m

    **I015/625/RSR** - *Range Resolution*

    Minimum difference in Range at which a sensor system is
    able to distinguish two targets with otherwise identical
    parameters in velocity and angular domain (under ideal
    measurement).

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/10` m :math:`\approx 0.10` m
    - value :math:`<= 3355443/2` m

        remark
            Note: This may differ from the cell size applied within
            the Sensor.

    **I015/625/SDR** - *Range Precision*

    Root-mean-square (rms) error of the Range estimate provided
    by a sensor system.

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m"
    - LSB = :math:`1/10` m :math:`\approx 0.10` m
    - value :math:`<= 3355443/2` m

    **I015/625/RR** - *Range Rate*

    The range rate is derived from different range measurements.

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m/s"
    - LSB = :math:`1/10` m/s :math:`\approx 0.10` m/s
    - value :math:`>= -8388607/10` m/s
    - value :math:`<= 8388607/10` m/s

        remark
            Note: The range rate is the first derivative computed from
            the range. In contrast, the Doppler velocity in I015/626
            is actually measured through Doppler.

    **I015/625/RSRR** - *Range Rate Resolution*

    Minimum difference in Range Rate at which a sensor system
    is able to distinguish two targets with otherwise identical
    parameters in position and angular domain (under ideal
    measurement).

    - 24 bits [``........................``]

    - unsigned quantity
    - unit: "m/s"
    - LSB = :math:`1/10` m/s :math:`\approx 0.10` m/s
    - value :math:`<= 3355443/2` m/s

    **I015/625/SDRR** - *Range Rate Precision*

    Root-mean-square (rms) error of the Range Rate estimate
    provided by a sensor system.

        **I015/625/SDRR/SDRR**

        Standard Deviation of Range Rate

        - 24 bits [``........................``]

        - unsigned quantity
        - unit: "m/s"
        - LSB = :math:`1/10` m/s :math:`\approx 0.10` m/s
        - value :math:`<= 3355443/2` m/s

        **I015/625/SDRR/CORRR**

        Correlation of Range Rate and Range, in Two’s
        complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/625/RA** - *Range Acceleration*

    The range acceleration is derived from different range
    rates.

    - 16 bits [``................``]

    - signed quantity
    - unit: "m/s²"
    - LSB = :math:`1/2^6` m/s² :math:`\approx 1.56e-2` m/s²
    - value :math:`>= -512` m/s²
    - value :math:`<= 512` m/s²

    **I015/625/SDRA** - *Range Acceleration Precision*

    Root-mean-square (rms) error of the Range Acceleration
    determined by the sensor system.

        **I015/625/SDRA/SDRA**

        Standard Deviation of Range Acceleration

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^7` m/s² :math:`\approx 7.81e-3` m/s²
        - value :math:`<= 512` m/s²

        **I015/625/SDRA/CORAR**

        Correlation of Range Acceleration and Range,
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

        **I015/625/SDRA/CORARR**

        Correlation of Range Acceleration and Range Rate,
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

Note 1: Depending upon its design the manner in which the
positional data is declared by an INCS sensor may be expressed
in WGS-84 (I015/ 600) or sensor centric coordinate system based
upon the ‘system reference point’ of the sensor (I015/625).

Note 2: The optional ‘precision’ fields (in Data Items
I015/600 – I015/625) provide a measure of the accuracy the INCS
system has assigned to positional data contained in the target
report.
Such information can be used to improve the quality with which
the INCS target report data is integrated in to the subsequent
processing stages of the ATM infrastructure.
However a consideration of the sensor characteristics and
capabilities, the manner in which INCS data is used operationally,
the weighting assigned to INCS data within a multi-sensor tracker
and the credibility assigned to the covariance data items should
be made to support the decision of whether these optional
covariance data items are required or whether the basic data items
provide sufficient performance.
Whilst not an operational consideration the additional costs that
may be associated with the provision and use of such data items
should also be weighed against the potential performance benefits
that would be achieved through the inclusion of these Data Items
in performance specifications.

Note 3: If I015/020 MoMu indicates that the target report is
Bi-Static (MoMu =1) then the range information is the difference
between the path from the transmitter to target to the receiver
less the distance between the transmitter and receiver. In this
case, the reference points referred to above are the positions
of the transmitter and receiver.
If I015/020 MoMu indicates that the target report is Mono-Static
(MoMu =0) then the range information is the distance between
the sensor and the target. In this case, the reference point
referred to above is the position of the mono-static sensor.

Note 4: The meaning of range in Category 015 is significantly
broader than the traditional hence the different INCS working
principles. This is especially true for bi-static and multi-static
radars. For readability the field is still called range and not
mono-/bi-/multi-static range or pseudo-range.
For radar the measured range is calculated from time differences
of signals assumed to be transmitted/received at the sensor
reference point(s). As noted above for bi-static radars the
reference points are the positions of the transmitter and receiver.
Moreover multi-static radars may receive identical signals from
other transmitters than the assumed reference transmitter
(e.g. passive INCS which are using single frequency networks as
illuminators) and therefore may calculate negative values for
bi-static range measurement data.

I015/626 - Doppler Information
******************************

*Definition*: Doppler measurement of the Target.

*Structure*:

Compound item (FX)

    **I015/626/DV** - *Doppler Velocity*

    Radial velocity or bistatic velocity of a target object
    measured by a sensor system via a corresponding Doppler
    frequency shift.

    - 24 bits [``........................``]

    - signed quantity
    - unit: "m/s"
    - LSB = :math:`1/100` m/s :math:`\approx 1.00e-2` m/s
    - value :math:`>= -8388607/100` m/s
    - value :math:`<= 8388607/100` m/s

        remark
            Note: The radial velocity is the magnitude of the
            3-dimensional velocity vector (i.e., the time derivative
            of the 3-dimensional position vector) projected onto the
            line between target object and sensor. The bistatic
            velocity is the magnitude of the 3-dimensional velocity
            vector projected onto the line between transmitter station
            and target object plus the magnitude of the 3-dimensional
            velocity vector projected onto the line between target
            object and sensor.

    **I015/626/SDDV** - *Precision of Doppler Velocity*

    Root-mean-square (rms) error of the Doppler Velocity
    measured by the sensor system.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "m/s"
    - LSB = :math:`1/2^6` m/s :math:`\approx 1.56e-2` m/s
    - value :math:`<= 1024` m/s

    **I015/626/DA** - *Doppler Acceleration*

    Radial acceleration or bistatic acceleration of a target
    object measured by a sensor system via a corresponding
    Doppler frequency shift and a subsequent difference
    operation.

    - 16 bits [``................``]

    - signed quantity
    - unit: "m/s²"
    - LSB = :math:`1/2^6` m/s² :math:`\approx 1.56e-2` m/s²
    - value :math:`>= -512` m/s²
    - value :math:`<= 512` m/s²

        remark
            Note: The radial acceleration is the magnitude of the
            3-dimensional acceleration vector (i.e., the time
            derivative of the 3-dimensional velocity vector) projected
            onto the line between target object and sensor. The
            bistatic acceleration is the magnitude of the 3-dimensional
            acceleration vector projected onto the line between
            transmitter station and target object plus the magnitude
            of the 3-dimensional acceleration vector projected onto
            the line between target object and sensor.

    **I015/626/SDDA** - *Precision of Doppler Acceleration*

    Root-mean-square (rms) error of the Doppler Velocity
    measured by the sensor system.

        **I015/626/SDDA/SDDA**

        Standard Deviation of Doppler Acceleration

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m/s²"
        - LSB = :math:`1/2^6` m/s² :math:`\approx 1.56e-2` m/s²
        - value :math:`<= 1024` m/s²

        **I015/626/SDDA/CODADV**

        Correlation of Doppler Acceleration and Doppler
        Velocity, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODVR** - *Correlation of Doppler Velocity and Range*

    Correlation of Doppler Velocity and Range (e.g. bistatic
    range).

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODVRR** - *Correlation of Doppler Velocity and Range Rate*

    Correlation of Doppler Velocity and Range Rate.

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODVRA** - *Correlation of Doppler Velocity and Range Acceleration*

    Correlation of Doppler Velocity and Range (e.g. bistatic
    range).

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODAR** - *Correlation of Doppler Acceleration and Range*

    Correlation of Doppler Acceleration and Range (e.g.
    bistatic range).

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODARR** - *Correlation of Doppler Acceleration and Range Rate*

    Correlation of Doppler Acceleration and Range Rate.

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/626/CODARA** - *Correlation of Doppler Acceleration and Range Acceleration*

    Correlation of Doppler Acceleration and Range Acceleration.

    - 8 bits [``........``]

    - signed quantity
    - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

I015/627 - Azimuth Information
******************************

*Definition*: Azimuth information that is provided relative to the sensor
or component reference point.

*Structure*:

Compound item (FX)

    **I015/627/AZ** - *Azimuth*

    Target angle relative to geographic North in the local
    reference system centred on the sensor.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
    - value :math:`>= 360` °

        remark
            Note: The azimuth shall increment in a clockwise manner
            relative to geographic North.

    **I015/627/RSAZ** - *Azimuth Resolution*

    Minimum angle in order to separate targets by the sensor
    in the azimuth dimension.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`45/2^16` ° :math:`\approx 6.87e-4` °
    - value :math:`<= 45` °

    **I015/627/SDASZ** - *Standard Deviation of Azimuth*

    Estimated standard deviation of the azimuth angle.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`45/2^16` ° :math:`\approx 6.87e-4` °
    - value :math:`<= 45` °

    **I015/627/AZR** - *Azimuth Rate*

    Rate of change of the azimuth angle.

    - 16 bits [``................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^16` ° :math:`\approx 2.75e-3` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

    **I015/627/SDAZR** - *Standard Deviation of Azimuth Rate*

    Estimated standard deviation of the azimuth angle rate.

        **I015/627/SDAZR/SDAZR**

        Standard Deviation of Azimuth Rate

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`45/2^16` ° :math:`\approx 6.87e-4` °
        - value :math:`<= 45` °

        **I015/627/SDAZR/COAZRAZ**

        Correlation of Azimuth Rate and Azimuth, in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/627/AZEX** - *Azimuth Extent*

    Target size in the azimuth angle dimension. The target
    extends between start angle and end angle traversed
    clockwise.

        **I015/627/AZEX/S**

        Azimuth Extent Start

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
        - value :math:`<= 360` °

        **I015/627/AZEX/E**

        Azimuth Extent End

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
        - value :math:`<= 360` °

Note: The Sensor Reference Point is detailed in ASTERIX Category
016 – where there is also provision for including the reference
points for the transmitter(s) and receiver(s) that are used within
the sensor configuration. The Sensor Reference Point is also
contained in ASTERIX Category 025.

I015/628 - Elevation Information
********************************

*Definition*: Information related to the elevation angle provided by the sensor.
(Predominantly used by electro-optic sensors).

*Structure*:

Compound item (FX)

    **I015/628/EL** - *Elevation*

    The elevation shall be given with respect to the horizontal
    plane of the sensor expressed in Two’s Complement.

    - 16 bits [``................``]

    - signed quantity
    - unit: "°"
    - LSB = :math:`180/2^16` ° :math:`\approx 2.75e-3` °
    - value :math:`>= -90` °
    - value :math:`<= 90` °

        remark
            Note: The elevation shall be given with respect to the
            local WGS-84 tangential plane of the receiver dedicated
            by I015/400.
            Note: For targets above the horizontal plane the elevation
            angle is positive and for targets below negative.

    **I015/628/RSEL** - *Elevation Resolution*

    Minimum angle in order to separate targets by the sensor
    in the elevation dimension.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`45/2^16` ° :math:`\approx 6.87e-4` °
    - value :math:`<= 45` °

    **I015/628/SDEL** - *Standard Deviation of Elevation*

    Estimated standard deviation of the elevation angle.

    - 16 bits [``................``]

    - unsigned quantity
    - unit: "°"
    - LSB = :math:`45/2^16` ° :math:`\approx 6.87e-4` °
    - value :math:`<= 45` °

    **I015/628/ER** - *Elevation Rate*

    Rate of change of the elevation angle.

    - 16 bits [``................``]

    - signed quantity
    - unit: "°/s"
    - LSB = :math:`180/2^16` °/s :math:`\approx 2.75e-3` °/s
    - value :math:`>= -90` °/s
    - value :math:`<= 90` °/s

    **I015/628/SDER** - *Standard Deviation of Elevation Rate*

    Estimated standard deviation of the elevation angle rate.

        **I015/628/SDER/SDELR**

        Standard Deviation of Elevation Rate

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "°/s"
        - LSB = :math:`45/2^16` °/s :math:`\approx 6.87e-4` °/s

        **I015/628/SDER/COELREL**

        Correlation of Elevation Rate and Elevation,
        in Two’s complement

        - 8 bits [``........``]

        - signed quantity
        - LSB = :math:`1/2^7`  :math:`\approx 7.81e-3` 

    **I015/628/ELEX** - *Elevation Extent*

    Target size in the elevation angle dimension. The target
    extends between start angle and end angle.

        **I015/628/ELEX/S**

        Elevation Extent Start, in Two’s complement

        - 16 bits [``................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^16` ° :math:`\approx 2.75e-3` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

        **I015/628/ELEX/E**

        Elevation Extent End, in Two’s complement

        - 16 bits [``................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^16` ° :math:`\approx 2.75e-3` °
        - value :math:`>= -90` °
        - value :math:`<= 90` °

I015/630 - Path Quality
***********************

*Definition*: Measure characterising the signal quality associated with
a specific target echo signal.

*Structure*:

Compound item (FX)

    **I015/630/DPP** - *Direct Path - Power*

    Signal power measured for the direct signal received
    from a specific transmitter station.

    - 8 bits [``........``]

    - signed quantity
    - unit: "dB"
    - LSB = :math:`1` dB
    - value :math:`>= -128` dB
    - value :math:`<= 127` dB

    **I015/630/DPS** - *Direct Path - Signal to Noise Ratio (SNR)*

    Signal to noise ratio measured for the direct signal
    received from a specific transmitter station.

    - 8 bits [``........``]

    - signed quantity
    - unit: "dB"
    - LSB = :math:`1` dB
    - value :math:`>= -128` dB
    - value :math:`<= 127` dB

    **I015/630/RPP** - *Reflected Path - Power*

    Signal power measured for a specific target echo signal
    found within range-Doppler matrix (associated with
    a specific transmitter station).

        **I015/630/RPP/(spare)**

        - 7 bits [``.......``]

        **I015/630/RPP/RPP**

        Power of reflected path, in Two’s complement"

        - 9 bits [``.........``]

        - signed quantity
        - unit: "dB"
        - LSB = :math:`1` dB
        - value :math:`>= -256` dB
        - value :math:`<= 255` dB

    **I015/630/RPS** - *Reflected Path - Signal to Noise Ratio (SNR)*

    Signal to noise ratio measured for a specific target echo
    signal found within range-Doppler matrix (associated with
    a specific transmitter station).

    - 8 bits [``........``]

    - signed quantity
    - unit: "dB"
    - LSB = :math:`1` dB
    - value :math:`>= -128` dB
    - value :math:`<= 127` dB

Notes: Some INCS sensors may be capable of outputting an indication
of the signal quality based upon the received echo signal strength
for that target.
Before including the provision of such data items in the technical
specification, it is advised that the cost and operational benefits
of the availability of such data is assessed.

I015/631 - Contour (Azimuth, Elevation Angle, Range Extent)
***********************************************************

*Definition*: Azimuth, elevation angles and range extent of all elementary
presences constituting a plot.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I015/631/AZCON**

        Azimuth Contour

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "°"
        - LSB = :math:`360/2^16` ° :math:`\approx 5.49e-3` °
        - value :math:`>= 360` °

        **I015/631/ELCON**

        Elevation Contour, in Two’s complement

        - 16 bits [``................``]

        - signed quantity
        - unit: "°"
        - LSB = :math:`180/2^16` ° :math:`\approx 2.75e-3` °
        - value :math:`<= -90` °
        - value :math:`>= 90` °

        **I015/631/RGCONSTOP**

        Range Contour Stop

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`10000/2^16` m :math:`\approx 0.15` m

        **I015/631/RGCONSTART**

        Range Contour Start

        - 16 bits [``................``]

        - unsigned quantity
        - unit: "m"
        - LSB = :math:`10000/2^16` m :math:`\approx 0.15` m

Note 1: The azimuth shall increment in a clockwise manner relative
to geographic North centred at the System Reference Point.

Note 2: The elevation shall be given with respect to the local
WGS-84 tangential plane of the receiver dedicated by I015/400.

Note 3: If populated, the range contour requires a start and stop
point. The stop point is to be greater or equal than the start
point.

I015/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item (SP)

User Application Profile for Category 015
=========================================
- (1) ``I015/010`` - Data Source Identifier
- (2) ``I015/000`` - Message Type
- (3) ``I015/015`` - Service Identification
- (4) ``I015/020`` - Target Report Descriptor
- (5) ``I015/030`` - Warning/Error Conditions
- (6) ``I015/145`` - Time of Applicability
- (7) ``I015/161`` - Track/Plot Number
- ``(FX)`` - Field extension indicator
- (8) ``I015/170`` - Track/Plot Status
- (9) ``I015/050`` - Update Period
- (10) ``I015/270`` - Target Size & Orientation
- (11) ``I015/300`` - Object Classification
- (12) ``I015/400`` - Measurement Identifier
- (13) ``I015/600`` - Horizontal Position Information
- (14) ``I015/601`` - Geometric Height Information
- ``(FX)`` - Field extension indicator
- (15) ``I015/602`` - Horizontal Velocity Information
- (16) ``I015/603`` - Horizontal Acceleration Information
- (17) ``I015/604`` - Vertical Velocity Information
- (18) ``I015/605`` - Vertical Velocity Information
- (19) ``I015/480`` - Associations
- (20) ``I015/625`` - Range Information
- (21) ``I015/626`` - Doppler Information
- ``(FX)`` - Field extension indicator
- (22) ``I015/627`` - Azimuth Information
- (23) ``I015/628`` - Elevation Information
- (24) ``I015/630`` - Path Quality
- (25) ``I015/631`` - Contour (Azimuth, Elevation Angle, Range Extent)
- (26) ``I015/SP`` - Special Purpose Field
- (27) ``(spare)``
- (28) ``(spare)``
- ``(FX)`` - Field extension indicator
