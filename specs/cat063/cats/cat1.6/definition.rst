Asterix category 063 - Sensor Status Reports
============================================
**category**: 063

**edition**: 1.6

**date**: 2020-08-04

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I063/010 - Data Source Identifier
*********************************

*Definition*: Identification of the radar station from which the data are received.

*Structure*:

    **I063/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I063/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    The defined SACs are on the EUROCONTROL ASTERIX website
    (www.eurocontrol.int/asterix)

I063/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value


The service identification is allocated by the SDPS

I063/030 - Time of Message
**************************

*Definition*: Absolute time stamping of the message, in the form of elapsed time
since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


The time of the day value is reset to zero at every midnight.

I063/050 - Sensor Identifier
****************************

*Definition*: None

*Structure*:

    **I063/050/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I063/050/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


- The up-to-date list of SACs is published on the EUROCONTROL
  Web Site (http://www.eurocontrol.int/asterix).
- If the SAC/SIC refers to an SDPS used as input, the respective
  sensor status information will be transmitted using the Reserved
  Expansion Field.

I063/060 - Sensor Configuration and Status
******************************************

*Definition*: Configuration and status of the sensor

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I063/060/CON**

    - 2 bits [``..``]

    - values:

        | 0: Operational
        | 1: Degraded
        | 2: Initialization
        | 3: Not currently connected

    **I063/060/PSR**

    - 1 bit [``.``]

    - values:

        | 0: PSR GO
        | 1: PSR NOGO

    **I063/060/SSR**

    - 1 bit [``.``]

    - values:

        | 0: SSR GO
        | 1: SSR NOGO

    **I063/060/MDS**

    - 1 bit [``.``]

    - values:

        | 0: MDS GO
        | 1: MDS NOGO

    **I063/060/ADS**

    - 1 bit [``.``]

    - values:

        | 0: ADS GO
        | 1: ADS NOGO

    **I063/060/MLT**

    - 1 bit [``.``]

    - values:

        | 0: MLT GO
        | 1: MLT NOGO

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I063/060/OPS** - *Operational Release Status of the System*

    - 1 bit [``.``]

    - values:

        | 0: System is released for operational use
        | 1: Operational use of System is inhibited

    **I063/060/ODP** - *Data Processor Overload Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Default, no overload
        | 1: Overload in DP

    **I063/060/OXT** - *Transmission Subsystem Overload Status*

    - 1 bit [``.``]

    - values:

        | 0: Default, no overload
        | 1: Overload in transmission subsystem

    **I063/060/MSC** - *Monitoring System Connected Status*

    - 1 bit [``.``]

    - values:

        | 0: Monitoring system connected
        | 1: Monitoring system disconnected

    **I063/060/TSV** - *Time Source Validity*

    - 1 bit [``.``]

    - values:

        | 0: Valid
        | 1: Invalid

    **I063/060/NPW** - *No Plot Warning*

    - 1 bit [``.``]

    - values:

        | 0: Default (no meaning)
        | 1: No plots being received

    **I063/060/(spare)**

    - 1 bit [``.``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


1. GO/NOGO information from PSR, SSR, Mode S, ADS and MLT is derived
   from monosensor categories and has a meaning only for operational
   sensors, whereas (CON) is derived by the SDPS.
2. The information (OPS), (ODP), (OXT), (MSC) and (TSV) are only related to
   CNS/ATM Ground Station and are derived from monosensor category
   (ASTERIX Cat 023).

I063/070 - Time Stamping Bias
*****************************

*Definition*: Plot Time stamping bias, in two’s complement form

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 1
- fractional bits: 0
- unit: "ms"
- LSB = :math:`1` ms



I063/080 - SSR / Mode S Range Gain and Bias
*******************************************

*Definition*: SSR / Mode S Range Gain and Range Bias, in two’s complement form.

*Structure*:

    **I063/080/SRG** - *Mode S Range Gain*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 0.00001
    - fractional bits: 0
    - LSB = :math:`0.00001`

    **I063/080/SRB** - *Mode S Range Bias*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM


Note:

    The following formula is used to correct range:

    .. math::

        \rho_\mathrm{corrected} = \frac{\rho_\mathrm{measured} - range\_bias}{1 + range\_gain}

I063/081 - SSR Mode S Azimuth Bias
**********************************

*Definition*: SSR / Mode S Azimuth Bias, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 360
- fractional bits: 16
- unit: "deg"
- LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Note:

    The following formula is used to correct azimuth:

    .. math::

        \theta_\mathrm{corrected} = \theta_\mathrm{measured} - azimuth\_bias

I063/090 - PSR Range Gain and Bias
**********************************

*Definition*: PSR Range Gain and PSR Range Bias, in two’s complement form.

*Structure*:

    **I063/090/PRG** - *PSR Range Gain*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 0.00001
    - fractional bits: 0
    - LSB = :math:`0.00001`

    **I063/090/PRB** - *PSR Range Bias*

    - 16 bits [``................``]

    - signed quantity
    - scaling factor: 1
    - fractional bits: 7
    - unit: "NM"
    - LSB = :math:`1 / {2^{7}}` NM = :math:`1 / {128}` NM :math:`\approx 0.0078125` NM


Note:

    The following formula is used to correct range:

    .. math:

        \rho_\mathrm{corrected} = \frac{\rho_\mathrm{measured} - range\_bias}{1 + range\_gain}

I063/091 - PSR Azimuth Bias
***************************

*Definition*: PSR Azimuth Bias, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 360
- fractional bits: 16
- unit: "deg"
- LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg


Note:

    The following formula is used to correct azimuth:

    .. math::

        \theta_\mathrm{corrected} = \theta_\mathrm{measured} - azimuth\_bias

I063/092 - PSR Elevation Bias
*****************************

*Definition*: PSR Elevation Bias, in two’s complement form.

*Structure*:

- 16 bits [``................``]

- signed quantity
- scaling factor: 360
- fractional bits: 16
- unit: "deg"
- LSB = :math:`360 / {2^{16}}` deg = :math:`360 / {65536}` deg :math:`\approx 0.0054931640625` deg



I063/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I063/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 063
=========================================
- (1) ``I063/010`` - Data Source Identifier
- (2) ``I063/015`` - Service Identification
- (3) ``I063/030`` - Time of Message
- (4) ``I063/050`` - Sensor Identifier
- (5) ``I063/060`` - Sensor Configuration and Status
- (6) ``I063/070`` - Time Stamping Bias
- (7) ``I063/080`` - SSR / Mode S Range Gain and Bias
- ``(FX)`` - Field extension indicator
- (8) ``I063/081`` - SSR Mode S Azimuth Bias
- (9) ``I063/090`` - PSR Range Gain and Bias
- (10) ``I063/091`` - PSR Azimuth Bias
- (11) ``I063/092`` - PSR Elevation Bias
- (12) ``(spare)``
- (13) ``I063/RE`` - Reserved Expansion Field
- (14) ``I063/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

