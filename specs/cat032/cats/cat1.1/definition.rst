Asterix category 032 - Miniplan Reports to an SDPS
==================================================
**category**: 032

**edition**: 1.1

**date**: 2020-12-11

Preamble
--------
Surveillance data exchange.

Description of standard data items
----------------------------------

I032/010 - Server Identification Tag
************************************

*Definition*: Identification of the Server of track information.

*Structure*:

    **I032/010/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I032/010/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Notes:

    1. The up-to-date list of SACs is published on the EUROCONTROL ASTERIX
       Web Site (http://www.eurocontrol.int/services/system-area-code-list).
    2. In case of message originating from an FPPS, the Server Identification
       Tag corresponds to the SDPS unit receiving the Miniplan.
    3. In case of message originating from a SDPS, the Server Identification
       Tag corresponds to the SDPS unit sending the Miniplan.

I032/015 - User Number
**********************

*Definition*: Identification of the User of the track data.

*Structure*:

- 16 bits [``................``]

- unsigned integer

Notes:

    1. The User numbers are predefined in the User registration data base
       of the SDPS Unit to which the User wants to connect.
    2. In case of message originating from an FPPS, the User Number
       corresponds to the FPPS one.
    3. In case of message originating from an SDPS, the User Number
       corresponds to the SDPS unit receiving the Miniplan.

I032/018 - Data Source Identification Tag
*****************************************

*Definition*: Identification of the data source (FPPS system) from which the information
contained in the message was initially originated.

*Structure*:

    **I032/018/SAC** - *System Area Code*

    - 8 bits [``........``]

    - raw value

    **I032/018/SIC** - *System Identification Code*

    - 8 bits [``........``]

    - raw value

Note:
    - The up-to-date list of SACs is published on the EUROCONTROL ASTERIX
      Web Site (http://www.eurocontrol.int/services/system-area-code-list).

I032/020 - Time of ASTERIX Report Generation
********************************************

*Definition*: Time of the generation of the ASTERIX category 032 report in the form of
elapsed time since last midnight, expressed as UTC.

*Structure*:

- 24 bits [``........................``]

- unsigned quantity
- unit: "s"
- LSB = :math:`1/2^7` s :math:`\approx 7.81e-3` s

Notes:

    1. The Time of ASTERIX Report Generation is reset to zero at every midnight.
    2. This time is determined at an application level (e.g. time at which
       a message is filled), and not at the communication level (i.e. not
       the time at which the data-block is sent).

I032/035 - Type of Message
**************************

*Definition*: This data item allows for a more convenient handling of the message at the
receiver side by further defining the type of transaction.

*Structure*:

    **I032/035/FAMILY**

    - 4 bits [``....``]

    - values:

        | 1: Information sent by an FPPS

    **I032/035/NATURE**

    - 4 bits [``....``]

    - values:

        | 1: Flight Plan to track initial correlation
        | 2: Miniplan update
        | 3: End of correlation
        | 4: Miniplan Cancellation
        | 5: Retained Miniplan

Note:
    - The composition of the messages is described in the following table. ::

        Data        Description         FPL to track Initial    End of correlation ($13),
        Ref Num                         Correlation ($11),      Miniplan cancellation ($14),
                                        Miniplan update ($12)   Retained Miniplan ($15)

        I032/010    Server id ...               M                       M
        I032/015    User Number                 O                       O
        I032/018    Data Source ...             M                       M
        I032/020    Time of ...                 M                       M
        I032/035    Type of Message             M                       M
        I032/040    Track Number            M from FPPS             M from FPPS
                                            X from SDPS             X from SDPS
        I032/050    Composed trknum...      M from SDPS             M from SDPS
                                            X from FPPS             X from FDPS
        I032/060    Track Mode 3/A              O                       X
        I032/400    Callsign                    O                       X
        I032/410    Plan Number                 O                       X
        I032/420    Flight Category             O                       X
        I032/430    Type of Aircraft            O                       X
        I032/435    Wake Turbulence ...         O                       X
        I032/440    Departure ...               O                       X
        I032/450    Destination ...             O                       X
        I032/460    Allocated SSR Codes         O                       X
        I032/480    Current Cleared FL...       O                       X
        I032/490    Current Control Pos...      O                       X
        I032/500    Supplementary FD...         O                       X

I032/040 - Track Number
***********************

*Definition*: Identification of a track (track number)

*Structure*:

- 16 bits [``................``]

- unsigned integer

I032/050 - Composed Track Number
********************************

*Definition*: Identification of a system track.

*Structure*:

Extended item.

    **I032/050/SUI** - *System Unit Identification*

    - 8 bits [``........``]

    - unsigned integer

    **I032/050/STN** - *System Track Number*

    - 15 bits [``...............``]

    - unsigned integer

    ``(FX)``

    - extension bit

        | 0: End of data item
        | 1: Extension into next extent

Notes:

    1. Each Track Number (i.e. either a Master or a Slave Track Number)
       is composed of a System Unit Identification (i.e. the identification
       of the SDPS unit processing the) together with the relevant System
       Track Number (i.e. the number of the track local to the SDPS Unit
       in question).
    2. The Composed Track Number is used by co-operating SDPS units to
       uniquely identify a track. It consists of the unit identifier and
       system track number for each unit involved in the co-operation.
       The first unit identification identifies the unit that is responsible
       for the track amalgamation.
    3. The Master Track Number and the possible extensions (Slave Tracks
       Numbers) are identically composed.

I032/060 - Track Mode 3/A
*************************

*Definition*: Mode 3/A code associated to the track

*Structure*:

    **I032/060/(spare)**

    - 4 bits [``....``]

    **I032/060/MODE3A** - *(Mode 3/A Code) 4 Digits, Octal Representation*

    - 12 bits [``............``]

    - Octal string (3-bits per digit)

I032/400 - Callsign
*******************

*Definition*: Callsign (in 7 characters) of an aircraft (provided in the Miniplan).

*Structure*:

- 56 bits [``... 56 bits ...``]

- Ascii string (8-bits per character)

Note:
    - Each one of the seven octets contains an ASCII Character. The Callsign
      is always left adjusted. It contains up to seven upper-case alphanumeric
      characters, the remaining character positions (if any) are padded with
      space characters.

I032/410 - Plan Number
**********************

*Definition*: The Plan Number is an integer value representing a unique reference to a
Flight-plan record within a particular FPPS.

*Structure*:

- 16 bits [``................``]

- unsigned integer

I032/420 - Flight Category
**************************

*Definition*: Flight Category.

*Structure*:

    **I032/420/GATOAT**

    - 2 bits [``..``]

    - values:

        | 0: Unknown
        | 1: General Air Traffic
        | 2: Operational Air Traffic
        | 3: Not applicable

    **I032/420/FR1FR2**

    - 2 bits [``..``]

    - values:

        | 0: Instrument Flight Rules
        | 1: Visual Flight rules
        | 2: Not applicable
        | 3: Controlled Visual Flight Rules

    **I032/420/SP3**

    - 1 bit [``.``]

    - raw value

    **I032/420/SP2**

    - 1 bit [``.``]

    - raw value

    **I032/420/SP1**

    - 1 bit [``.``]

    - raw value

    **I032/420/(spare)**

    - 1 bit [``.``]

Note:
    - The definition of the sub-categories is system dependent and shall
      be descried in the system ICD.

I032/430 - Type of Aircraft
***************************

*Definition*: Type of Aircraft.

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Notes:

    1. Each one of the four octets composing the type of aircraft contains
       an ASCII Character (upper-case alphabetic characters with trailing
       spaces).
    2. The types of aircraft are defined in the ICAO Document 4444.

I032/435 - Wake Turbulence Category
***********************************

*Definition*: Wake turbulence category of an aircraft.

*Structure*:

- 8 bits [``........``]

- values:

    | 76: Light
    | 77: Medium
    | 72: Heavy
    | 74: Super

I032/440 - Departure Aerodrome
******************************

*Definition*: Departure Aerodrome

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Notes:

    1. Each octet contains one ASCII Character (Upper Case Alphabetic)
    2. The Aerodrome Names are indicated in the ICAO Location Indicators book.

I032/450 - Destination Aerodrome
********************************

*Definition*: Departure Aerodrome

*Structure*:

- 32 bits [``................................``]

- Ascii string (8-bits per character)

Notes:

    1. Each octet contains one ASCII Character (Upper Case Alphabetic).
    2. The Aerodrome Names are indicated in the ICAO Location Indicators book [Ref. 5].

I032/460 - Allocated SSR Codes
******************************

*Definition*: List of successive SSR Codes allocated to a flight.

*Structure*:

Repetitive item, repetition factor 8 bits.

        **I032/460/(spare)**

        - 4 bits [``....``]

        **I032/460/OCT1** - *1st Octal Digit*

        - 3 bits [``...``]

        - raw value

        **I032/460/OCT2** - *2nd Octal Digit*

        - 3 bits [``...``]

        - raw value

        **I032/460/OCT3** - *3rd Octal Digit*

        - 3 bits [``...``]

        - raw value

        **I032/460/OCT4** - *4th Octal Digit*

        - 3 bits [``...``]

        - raw value

I032/480 - Current Cleared Flight Level
***************************************

*Definition*: Current Cleared Flight Level

*Structure*:

- 16 bits [``................``]

- unsigned quantity
- unit: "FL"
- LSB = :math:`1/2^2` FL :math:`\approx 0.25` FL
- value :math:`>= 0` FL
- value :math:`<= 1500` FL

I032/490 - Current Control Position
***********************************

*Definition*: Identification of the Control Position currently controlling a flight.

*Structure*:

    **I032/490/CEN** - *Centre*

    - 8 bits [``........``]

    - raw value

    **I032/490/POS** - *Position*

    - 8 bits [``........``]

    - raw value

Note:
    - The Centre and Control Position Identification Codes are implementation
      specific and have to be agreed upon between communication partners.

I032/500 - Supplementary Flight Data
************************************

*Definition*: Flight related data provided by ground based systems.

*Structure*:

Compound item (FX)

    **I032/500/IFI** - *IFPS FLIGHT ID*

        **I032/500/IFI/TYP**

        - 2 bits [``..``]

        - values:

            | 0: Plan Number
            | 1: Unit 1 internal flight number
            | 2: Unit 2 internal flight number
            | 3: Unit 3 internal flight number

        **I032/500/IFI/(spare)**

        - 3 bits [``...``]

        **I032/500/IFI/NBR**

        - 27 bits [``...........................``]

        - unsigned integer
        - value :math:`>= 0`
        - value :math:`<= 99999999`

    **I032/500/RVP** - *RVSM & Flight Priority*

        **I032/500/RVP/(spare)**

        - 5 bits [``.....``]

        **I032/500/RVP/RVSM**

        - 2 bits [``..``]

        - values:

            | 0: Unknown
            | 1: Approved
            | 2: Exempt
            | 3: Not approved

        **I032/500/RVP/HPR**

        - 1 bit [``.``]

        - values:

            | 0: Normal Priority Flight
            | 1: High Priority Flight

    **I032/500/RDS** - *Runway Designation*

        **I032/500/RDS/NU1** - *First Number*

        - 8 bits [``........``]

        - Ascii string (8-bits per character)

        **I032/500/RDS/NU2** - *Second Number*

        - 8 bits [``........``]

        - Ascii string (8-bits per character)

        **I032/500/RDS/LTR** - *Letter*

        - 8 bits [``........``]

        - Ascii string (8-bits per character)

    **I032/500/TOD** - *Time of Departure / Arrival*

    Repetitive item, repetition factor 8 bits.

            **I032/500/TOD/TYP**

            - 5 bits [``.....``]

            - values:

                | 0: Scheduled Off-Block Time
                | 1: Estimated Off-Block Time
                | 2: Estimated Take-Off Time
                | 3: Actual Off-Block Time
                | 4: Predicted Time at Runway Hold
                | 5: Actual Time at Runway Hold
                | 6: Actual Line-Up Time
                | 7: Actual Take-Off Time
                | 8: Estimated Time of Arrival
                | 9: Predicted Landing Time
                | 10: Actual Landing Time
                | 11: Actual Time off Runway
                | 12: Predicted Time to Gate
                | 13: Actual On-Block Time

            **I032/500/TOD/DAY**

            - 2 bits [``..``]

            - values:

                | 0: Today
                | 1: Yesterday
                | 2: Tomorrow
                | 3: Invalid

            **I032/500/TOD/(spare)**

            - 4 bits [``....``]

            **I032/500/TOD/HOR**

            - 5 bits [``.....``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 23`

            **I032/500/TOD/(spare)**

            - 2 bits [``..``]

            **I032/500/TOD/MIN**

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

            **I032/500/TOD/AVS**

            - 1 bit [``.``]

            - values:

                | 0: Seconds available
                | 1: Seconds not available

            **I032/500/TOD/(spare)**

            - 1 bit [``.``]

            **I032/500/TOD/SEC**

            - 6 bits [``......``]

            - unsigned integer
            - value :math:`>= 0`
            - value :math:`<= 59`

    **I032/500/AST** - *Aircraft Stand*

    - 48 bits [``... 48 bits ...``]

    - Ascii string (8-bits per character)

    **I032/500/STS** - *Stand Status*

        **I032/500/STS/EMP**

        - 2 bits [``..``]

        - values:

            | 0: Empty
            | 1: Occupied
            | 2: Unknown
            | 3: Invalid

        **I032/500/STS/AVL**

        - 2 bits [``..``]

        - values:

            | 0: Available
            | 1: Not available
            | 2: Unknown
            | 3: Invalid

        **I032/500/STS/(spare)**

        - 4 bits [``....``]

    **I032/500/SID** - *Standard Instrument Departure*

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

    **I032/500/STAR** - *Standard Instrument Arrival*

    - 56 bits [``... 56 bits ...``]

    - Ascii string (8-bits per character)

Notes:

    1. NU1, NU2 and LTR each contain an ASCII character (upper case alphabetic).
    2. For details refer to ICAO Annex 14 Chapter 5 [Ref. 4].
    3. Estimated times are derived from flight plan processing systems.
       Predicted times are derived by the fusion system based on surveillance
       data. For definitions see [Ref.4]
    4. Each one of the six Octets contains an ASCII Character. The Aircraft
       Stand identification is always left adjusted. It contains up to six
       upper-case alphanumeric characters, the remaining character positions
       (if any) are padded with space characters.
    5. Each one of the seven Octets contains an ASCII Character. The SID is
       always left adjusted. It contains up to seven alphanumeric characters,
       the remaining character positions (if any) are padded with space characters.
    6. Each one of the seven Octets contains an ASCII Character. The STAR
       is always left adjusted. It contains up to seven alphanumeric
       characters, the remaining character positions (if any) are padded
       with space characters.

I032/RE - Reserved Expansion Field
**********************************

*Definition*: Expansion

*Structure*:

Explicit item (RE)

User Application Profile for Category 032
=========================================
- (1) ``I032/010`` - Server Identification Tag
- (2) ``I032/015`` - User Number
- (3) ``I032/018`` - Data Source Identification Tag
- (4) ``I032/035`` - Type of Message
- (5) ``I032/020`` - Time of ASTERIX Report Generation
- (6) ``I032/040`` - Track Number
- (7) ``I032/050`` - Composed Track Number
- ``(FX)`` - Field extension indicator
- (8) ``I032/060`` - Track Mode 3/A
- (9) ``I032/400`` - Callsign
- (10) ``I032/410`` - Plan Number
- (11) ``I032/420`` - Flight Category
- (12) ``I032/440`` - Departure Aerodrome
- (13) ``I032/450`` - Destination Aerodrome
- (14) ``I032/480`` - Current Cleared Flight Level
- ``(FX)`` - Field extension indicator
- (15) ``I032/490`` - Current Control Position
- (16) ``I032/430`` - Type of Aircraft
- (17) ``I032/435`` - Wake Turbulence Category
- (18) ``I032/460`` - Allocated SSR Codes
- (19) ``I032/500`` - Supplementary Flight Data
- (20) ``(spare)``
- (21) ``I032/RE`` - Reserved Expansion Field
- ``(FX)`` - Field extension indicator
