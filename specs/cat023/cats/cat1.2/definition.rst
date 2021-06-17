Asterix category 023 - CNS/ATM Ground Station and Service Status Reports
========================================================================
**category**: 023

**edition**: 1.2

**date**: 2009-03-01

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I023/000 - Report Type
**********************

*Definition*: This Data Item allows for a more convenient handling of the
reports at the receiver side by further defining the type of
transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Ground station status report
    | 2: Service status report
    | 3: Service statistics report


NOTES:
    1. In applications where transactions of various types are exchanged, the
       Report Type Data Item facilitates the proper report handling at the
       receiver side.
    2. All Report Type values are reserved for common standard use.

I023/010 - Data Source Identifier
*********************************

*Definition*: Identification of the Ground Station from which the data is received.

*Structure*:

    **I023/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I023/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I023/015 - Service Type and Identification
******************************************

*Definition*: Identifies the type of service being reported.

*Structure*:

    **I023/015/SID** - *Service Identification*

    - 4 bits [``....``]

    - raw value

    **I023/015/STYP** - *Type of Service*

    - 4 bits [``....``]

    - values:

        | 1: ADS-B VDL4
        | 2: ADS-B Ext Squitter
        | 3: ADS-B UAT
        | 4: TIS-B VDL4
        | 5: TIS-B Ext Squitter
        | 6: TIS-B UAT
        | 7: FIS-B VDL4
        | 8: GRAS VDL4
        | 9: MLT


Note:
    - The service identification is allocated by the system.
    - The service identification is also available in item I021/015.

I023/070 - Time of Day
**********************

*Definition*: Absolute time stamping expressed as UTC time.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 7
- unit: "s"
- LSB = :math:`1 / {2^{7}}` s = :math:`1 / {128}` s :math:`\approx 0.0078125` s


Notes:

    1. The time of day value is reset to zero each day at midnight.

I023/100 - Ground Station Status
********************************

*Definition*: Information concerning the status of a Ground Station.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I023/100/NOGO** - *Operational Release Status of the Data*

    - 1 bit [``.``]

    - values:

        | 0: Data is released for operational use
        | 1: Data must not be used operationally

    **I023/100/ODP** - *Data Processor Overload Indicator*

    - 1 bit [``.``]

    - values:

        | 0: Default, no overload
        | 1: Overload in DP

    **I023/100/OXT** - *Ground Interface Data Communications Overload*

    - 1 bit [``.``]

    - values:

        | 0: Default, no overload
        | 1: Overload in transmission subsystem

    **I023/100/MSC** - *Monitoring System Connected Status*

    - 1 bit [``.``]

    - values:

        | 0: Monitoring system not connected or unknown
        | 1: Monitoring system connected

    **I023/100/TSV** - *Time Source Validity*

    - 1 bit [``.``]

    - values:

        | 0: Valid
        | 1: Invalid

    **I023/100/SPO** - *Indication of Spoofing Attack*

    - 1 bit [``.``]

    - values:

        | 0: No spoofing detected
        | 1: Potential spoofing attack

    **I023/100/RN** - *Renumbering Indication for Track ID*

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Track numbering has restarted

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I023/100/GSSP** - *Ground Station Status Reporting Period*

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`1` s
    - value :math:`>= 1` s
    - value :math:`<= 127` s

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent


Notes:

    1. A time source is considered as valid when either externally
       synchronised or running on a local oscillator within the
       required accuracy of UTC.
    2. Bit 8 (NOGO), when set to “1” indicates that the data transmitted
       by the GS is not released for operational use.
    3. Bit 2 indicates that the allocation of Track-IDs (Item I021/161)
       was re-started.

I023/101 - Service Configuration
********************************

*Definition*: Information concerning the configuration of a Service.

*Structure*:

Extended item with first part ``16 bits`` long and optional ``8 bits`` extends.

    **I023/101/RP** - *Report Period for Category 021 Reports*

    - 8 bits [``........``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 1
    - unit: "s"
    - LSB = :math:`1 / {2^{1}}` s = :math:`1 / {2}` s :math:`\approx 0.5` s

    **I023/101/SC** - *Service Class*

    - 3 bits [``...``]

    - values:

        | 0: No information
        | 1: NRA class
        | 2: Reserved for future use
        | 3: Reserved for future use
        | 4: Reserved for future use
        | 5: Reserved for future use
        | 6: Reserved for future use
        | 7: Reserved for future use

    **I023/101/(spare)**

    - 4 bits [``....``]

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

    **I023/101/SSRP** - *Service Status Reporting Period*

    - 7 bits [``.......``]

    - unsigned quantity
    - scaling factor: 1
    - fractional bits: 0
    - unit: "s"
    - LSB = :math:`1` s
    - value :math:`>= 1` s
    - value :math:`<= 127` s

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I023/110 - Service Status
*************************

*Definition*: Information concerning the status of the Service provided by a Ground Station.

*Structure*:

Extended item with first part ``8 bits`` long and optional ``8 bits`` extends.

    **I023/110/(spare)**

    - 4 bits [``....``]

    **I023/110/STAT** - *Status of the Service*

    - 3 bits [``...``]

    - values:

        | 0: Unknown
        | 1: Failed
        | 2: Disabled
        | 3: Degraded
        | 4: Normal
        | 5: Initialisation

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent



I023/120 - Service Statistics
*****************************

*Definition*: Statistics concerning the service. Provides counts of various ADS-B
message types that have been received since the report was last sent.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I023/120/TYPE** - *Type of Report Counter*

        - 8 bits [``........``]

        - values:

            | 0: Number of unknown messages received
            | 1: Number of too old messages received
            | 2: Number of failed message conversions
            | 3: Total Number of messages received
            | 4: Total Number of messages transmitted
            | 20: Number of TIS-B management messages received
            | 21: Number of Basic messages received
            | 22: Number of High Dynamic messages received
            | 23: Number of Full Position messages received
            | 24: Number of Basic Ground  messages received
            | 25: Number of TCP messages received
            | 26: Number of UTC time  messages received
            | 27: Number of Data messages received
            | 28: Number of High Resolution messages received
            | 29: Number of Aircraft Target Airborne messages received
            | 30: Number of Aircraft Target Ground messages received
            | 31: Number of Ground Vehicle Target messages received
            | 32: Number of 2 slots TCP messages received

        **I023/120/REF** - *Reference from which the Messages Are Countered*

        - 1 bit [``.``]

        - values:

            | 0: From midnight
            | 1: From the last report

        **I023/120/(spare)**

        - 7 bits [``.......``]

        **I023/120/CV** - *32-bit Counter Value*

        - 32 bits [``................................``]

        - raw value



I023/200 - Operational Range
****************************

*Definition*: Currently active operational range of the Ground Station.

*Structure*:

- 8 bits [``........``]

- unsigned quantity
- scaling factor: 1
- fractional bits: 0
- unit: "NM"
- LSB = :math:`1` NM


Notes:

    1. Maximum value indicates “maximum value or above”.

I023/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I023/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 023
=========================================
- (1) ``I023/010`` - Data Source Identifier
- (2) ``I023/000`` - Report Type
- (3) ``I023/015`` - Service Type and Identification
- (4) ``I023/070`` - Time of Day
- (5) ``I023/100`` - Ground Station Status
- (6) ``I023/101`` - Service Configuration
- (7) ``I023/200`` - Operational Range
- ``(FX)`` - Field extension indicator
- (8) ``I023/110`` - Service Status
- (9) ``I023/120`` - Service Statistics
- (10) ``(spare)``
- (11) ``(spare)``
- (12) ``(spare)``
- (13) ``I023/RE`` - Reserved Expansion Field
- (14) ``I023/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

