Asterix category 065 - SDPS Service Status Reports
==================================================
**category**: 065

**edition**: 1.5

**date**: 2020-06-18

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I065/000 - Message Type
***********************

*Definition*: This Data Item allows for a more convenient handling of the
messages at the receiver side by further defining the type of
transaction.

*Structure*:

- 8 bits [``........``]

- values:

    | 1: SDPS Status
    | 2: End of Batch
    | 3: Service Status Report


Notes:
    - In application where transactions of various types are exchanged, the
      Message Type Data Item facilitates the proper message handling at the
      receiver side.

I065/010 - Data Source Identifier
*********************************

*Definition*: Identification of the system sending the data.

*Structure*:

    **I065/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I065/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value


Note:
    - The up-to-date list of SACs is published on the
      EUROCONTROL Web Site (http://www.eurocontrol.int/asterix).

I065/015 - Service Identification
*********************************

*Definition*: Identification of the service provided to one or more users.

*Structure*:

- 8 bits [``........``]

- raw value


Note:
    - The service identification is allocated by the SDPS.

I065/020 - Batch Number
***********************

*Definition*: A number indicating the completion of a service for that batch of track
data, from 0 to N-1, N being the number of batches used to make
one complete processing cycle.

*Structure*:

- 8 bits [``........``]

- unsigned integer



I065/030 - Time of Message
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


Note:
    - The time is reset to zero at every midnight.

I065/040 - SDPS Configuration and Status
****************************************

*Definition*: Status of an SDPS.

*Structure*:

    **I065/040/NOGO**

    - 2 bits [``..``]

    - values:

        | 0: Operational
        | 1: Degraded
        | 2: Not currently connected
        | 3: Unknown

    **I065/040/OVL**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Overload

    **I065/040/TSV**

    - 1 bit [``.``]

    - values:

        | 0: Default
        | 1: Invalid Time Source

    **I065/040/PSS** - *Processing System Status*

    - 2 bits [``..``]

    - values:

        | 0: Not applicable
        | 1: SDPS-1 selected
        | 2: SDPS-2 selected
        | 3: SDPS-3 selected

    **I065/040/STTN** - *Track Re-numbering Indication*

    - 1 bit [``.``]

    - raw value

        remark
            The STTN bit indicates when track numbering has been restarted. The STTN bit
            is a toggle bit that changes its value whenever the SDPS system starts re-numbering
            its tracks. Re-numbering of the track takes place either after SDPS start-up (cold or hot
            start-up) or when a configuration set has been changed (please note that the latter
            case is ARTAS specific behaviour and may be different in another SDPS).

    **I065/040/(spare)**

    - 1 bit [``.``]



I065/050 - Service Status Report
********************************

*Definition*: Report sent by the SDPS related to a service

*Structure*:

- 8 bits [``........``]

- values:

    | 1: Service degradation
    | 2: Service degradation ended
    | 3: Main radar out of service
    | 4: Service interrupted by the operator
    | 5: Service interrupted due to contingency
    | 6: Ready for service restart after contingency
    | 7: Service ended by the operator
    | 8: Failure of user main radar
    | 9: Service restarted by the operator
    | 10: Main radar becoming operational
    | 11: Main radar becoming degraded
    | 12: Service continuity interrupted due to disconnection with adjacent unit
    | 13: Service continuity restarted
    | 14: Service synchronised on backup radar
    | 15: Service synchronised on main radar
    | 16: Main and backup radar, if any, failed



I065/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item



I065/SP - Special Purpose Field
*******************************

*Definition*: Special Purpose Field

*Structure*:

Explicit item



User Application Profile for Category 065
=========================================
- (1) ``I065/010`` - Data Source Identifier
- (2) ``I065/000`` - Message Type
- (3) ``I065/015`` - Service Identification
- (4) ``I065/030`` - Time of Message
- (5) ``I065/020`` - Batch Number
- (6) ``I065/040`` - SDPS Configuration and Status
- (7) ``I065/050`` - Service Status Report
- ``(FX)`` - Field extension indicator
- (8) ``(spare)``
- (9) ``(spare)``
- (10) ``(spare)``
- (11) ``(spare)``
- (12) ``(spare)``
- (13) ``I065/RE`` - Reserved Expansion Field
- (14) ``I065/SP`` - Special Purpose Field
- ``(FX)`` - Field extension indicator

