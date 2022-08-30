{*******************************************************}
{                 MiTeC Common Routines                 }
{          MiTeC Windows Terminal Services API          }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}
{$DEFINE DYNAMIC_LINK}

unit MiTeC_WtsApi32;

interface

uses
  {$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}, MiTeC_Windows;

//   Windows Terminal Server public APIs
//
//   Copyright 1995-1999, Citrix Systems Inc.
//   Copyright (c) 1997-1999  Microsoft Corporation

//==============================================================================
// Defines
//==============================================================================

//
//  Specifies the current server
//

const
  INVALID_HANDLE = THandle(-1);

  WTS_CURRENT_SERVER        = THANDLE(0);
  {$EXTERNALSYM WTS_CURRENT_SERVER}
  WTS_CURRENT_SERVER_HANDLE = THANDLE(0);
  {$EXTERNALSYM WTS_CURRENT_SERVER_HANDLE}
  WTS_CURRENT_SERVER_NAME   = '';
  {$EXTERNALSYM WTS_CURRENT_SERVER_NAME}

//
//  Specifies the current session (SessionId)
//

  WTS_CURRENT_SESSION = DWORD(-1);
  {$EXTERNALSYM WTS_CURRENT_SESSION}
  WTS_ANY_SESSION = DWORD(-2);

//
//  Possible pResponse values from WTSSendMessage()
//

  IDTIMEOUT = 32000;
  {$EXTERNALSYM IDTIMEOUT}
  IDASYNC   = 32001;
  {$EXTERNALSYM IDASYNC}

//
//  Shutdown flags
//

  WTS_WSD_LOGOFF = $00000001;           // log off all users except
  {$EXTERNALSYM WTS_WSD_LOGOFF}         // current user; deletes
                                        // WinStations (a reboot is
                                        // required to recreate the
                                        // WinStations)
  WTS_WSD_SHUTDOWN = $00000002;         // shutdown system
  {$EXTERNALSYM WTS_WSD_SHUTDOWN}
  WTS_WSD_REBOOT   = $00000004;         // shutdown and reboot
  {$EXTERNALSYM WTS_WSD_REBOOT}
  WTS_WSD_POWEROFF = $00000008;         // shutdown and power off (on
  {$EXTERNALSYM WTS_WSD_POWEROFF}
                                        // machines that support power
                                        // off through software)
  WTS_WSD_FASTREBOOT = $00000010;       // reboot without logging users
  {$EXTERNALSYM WTS_WSD_FASTREBOOT}     // off or shutting down

  WTS_SESSION_REMOTE_CONTROL = $9;

  USERNAME_LENGTH = 20;
  CLIENTNAME_LENGTH = 20;
  CLIENTADDRESS_LENGTH = 30;
  DOMAIN_LENGTH = 17;


  AF_UNSPEC = 0; // unspecified
  AF_UNIX      = 1; // local to host (pipes, portals
  AF_INET      = 2; // internetwork: UDP, TCP, etc.
  AF_IMPLINK   = 3; // arpanet imp addresses
  AF_PUP       = 4; // pup protocols: e.g. BSP
  AF_CHAOS     = 5; // mit CHAOS protocols
  AF_NS        = 6; // XEROX NS protocols
  AF_IPX       = AF_NS; // IPX protocols: IPX, SPX, etc.
  AF_ISO       = 7; // ISO protocols
  AF_OSI       = AF_ISO; // OSI is ISO
  AF_ECMA      = 8; // european computer manufacturers
  AF_DATAKIT   = 9; // datakit protocols
  AF_CCITT     = 10; // CCITT protocols, X.25 etc
  AF_SNA       = 11; // IBM SNA
  AF_DECnet    = 12; // DECnet
  AF_DLI       = 13; // Direct data link interface
  AF_LAT       = 14; // LAT
  AF_HYLINK    = 15; // NSC Hyperchannel
  AF_APPLETALK = 16; // AppleTalk
  AF_NETBIOS   = 17; // NetBios-style addresses
  AF_VOICEVIEW = 18; // VoiceView
  AF_FIREFOX   = 19; // Protocols from Firefox
  AF_UNKNOWN1  = 20; // Somebody is using this!
  AF_BAN       = 21; // Banyan

  AF_INET6     = 23; // Internetwork Version 6


//==============================================================================
// WTS_CONNECTSTATE_CLASS - Session connect state
//==============================================================================

type
  _WTS_CONNECTSTATE_CLASS = (
    WTSActive,              // User logged on to WinStation
    WTSConnected,           // WinStation connected to client
    WTSConnectQuery,        // In the process of connecting to client
    WTSShadow,              // Shadowing another WinStation
    WTSDisconnected,        // WinStation logged on without client
    WTSIdle,                // Waiting for client to connect
    WTSListen,              // WinStation is listening for connection
    WTSReset,               // WinStation is being reset
    WTSDown,                // WinStation is down due to error
    WTSInit);               // WinStation in initialization
  {$EXTERNALSYM _WTS_CONNECTSTATE_CLASS}
  WTS_CONNECTSTATE_CLASS = _WTS_CONNECTSTATE_CLASS;
  {$EXTERNALSYM WTS_CONNECTSTATE_CLASS}
  TWtsConnectStateClass = WTS_CONNECTSTATE_CLASS;

//==============================================================================
// WTS_SERVER_INFO - returned by WTSEnumerateServers (version 1)
//==============================================================================

//
//  WTSEnumerateServers() returns two variables: pServerInfo and Count.
//  The latter is the number of WTS_SERVER_INFO structures contained in
//  the former.  In order to read each server, iterate i from 0 to
//  Count-1 and reference the server name as
//  pServerInfo[i].pServerName; for example:
//
//  for ( i=0; i < Count; i++ ) {
//      _tprintf( TEXT("%s "), pServerInfo[i].pServerName );
//  }
//
//  The memory returned looks like the following.  P is a pServerInfo
//  pointer, and D is the string data for that pServerInfo:
//
//  P1 P2 P3 P4 ... Pn D1 D2 D3 D4 ... Dn
//
//  This makes it easier to iterate the servers, using code MiTeCilar to
//  the above.
//

type
  PWTS_SERVER_INFOW = ^WTS_SERVER_INFOW;
  {$EXTERNALSYM PWTS_SERVER_INFOW}
  _WTS_SERVER_INFOW = record
    pServerName: LPWSTR; // server name
  end;
  {$EXTERNALSYM _WTS_SERVER_INFOW}
  WTS_SERVER_INFOW = _WTS_SERVER_INFOW;
  {$EXTERNALSYM WTS_SERVER_INFOW}
  TWtsServerInfoW = WTS_SERVER_INFOW;
  PWtsServerInfoW = PWTS_SERVER_INFOW;

  PWTS_SERVER_INFOA = ^WTS_SERVER_INFOA;
  {$EXTERNALSYM PWTS_SERVER_INFOA}
  _WTS_SERVER_INFOA = record
    pServerName: LPSTR; // server name
  end;
  {$EXTERNALSYM _WTS_SERVER_INFOA}
  WTS_SERVER_INFOA = _WTS_SERVER_INFOA;
  {$EXTERNALSYM WTS_SERVER_INFOA}
  TWtsServerInfoA = WTS_SERVER_INFOA;
  PWtsServerInfoA = PWTS_SERVER_INFOA;

{$IFDEF UNICODE}
  WTS_SERVER_INFO = WTS_SERVER_INFOW;
  {$EXTERNALSYM WTS_SERVER_INFO}
  PWTS_SERVER_INFO = PWTS_SERVER_INFOW;
  {$EXTERNALSYM PWTS_SERVER_INFO}
  TWtsServerInfo = TWtsServerInfoW;
  PWtsServerInfo = PWtsServerInfoW;
{$ELSE}
  WTS_SERVER_INFO = WTS_SERVER_INFOA;
  {$EXTERNALSYM WTS_SERVER_INFO}
  PWTS_SERVER_INFO = PWTS_SERVER_INFOA;
  {$EXTERNALSYM PWTS_SERVER_INFO}
  TWtsServerInfo = TWtsServerInfoA;
  PWtsServerInfo = PWtsServerInfoA;
{$ENDIF}

//==============================================================================
// WTS_SESSION_INFO - returned by WTSEnumerateSessions (version 1)
//==============================================================================

//
//  WTSEnumerateSessions() returns data in a MiTeCilar format to the above
//  WTSEnumerateServers().  It returns two variables: pSessionInfo and
//  Count.  The latter is the number of WTS_SESSION_INFO structures
//  contained in the former.  Iteration is MiTeCilar, except that there
//  are three parts to each entry, so it would look like this:
//
//  for ( i=0; i < Count; i++ ) {
//      _tprintf( TEXT("%-5u  %-20s  %u\n"),
//                pSessionInfo[i].SessionId,
//                pSessionInfo[i].pWinStationName,
//                pSessionInfo[i].State );
//  }
//
//  The memory returned is also segmented as the above, with all the
//  structures allocated at the start and the string data at the end.
//  We'll use S for the SessionId, P for the pWinStationName pointer
//  and D for the string data, and C for the connect State:
//
//  S1 P1 C1 S2 P2 C2 S3 P3 C3 S4 P4 C4 ... Sn Pn Cn D1 D2 D3 D4 ... Dn
//
//  As above, this makes it easier to iterate the sessions.
//

type
  PWTS_SESSION_INFOW = ^WTS_SESSION_INFOW;
  {$EXTERNALSYM PWTS_SESSION_INFOW}
  _WTS_SESSION_INFOW = record
    SessionId: DWORD;              // session id
    pWinStationName: LPWSTR;       // name of WinStation this session is connected to
    State: WTS_CONNECTSTATE_CLASS; // connection state (see enum)
  end;
  {$EXTERNALSYM _WTS_SESSION_INFOW}
  WTS_SESSION_INFOW = _WTS_SESSION_INFOW;
  {$EXTERNALSYM WTS_SESSION_INFOW}
  TWtsSessionInfoW = WTS_SESSION_INFOW;
  PWtsSessionInfoW = PWTS_SESSION_INFOW;

  PWTS_SESSION_INFOA = ^WTS_SESSION_INFOA;
  {$EXTERNALSYM PWTS_SESSION_INFOA}
  _WTS_SESSION_INFOA = record
    SessionId: DWORD;              // session id
    pWinStationName: LPSTR;        // name of WinStation this session is connected to
    State: WTS_CONNECTSTATE_CLASS; // connection state (see enum)
  end;
  {$EXTERNALSYM _WTS_SESSION_INFOA}
  WTS_SESSION_INFOA = _WTS_SESSION_INFOA;
  {$EXTERNALSYM WTS_SESSION_INFOA}
  TWtsSessionInfoA = WTS_SESSION_INFOA;
  PWtsSessionInfoA = PWTS_SESSION_INFOA;

{$IFDEF UNICODE}
  WTS_SESSION_INFO = WTS_SESSION_INFOW;
  PWTS_SESSION_INFO = PWTS_SESSION_INFOW;
  TWtsSessionInfo = TWtsSessionInfoW;
  PWtsSessionInfo = PWtsSessionInfoW;
{$ELSE}
  WTS_SESSION_INFO = WTS_SESSION_INFOA;
  PWTS_SESSION_INFO = PWTS_SESSION_INFOA;
  TWtsSessionInfo = TWtsSessionInfoA;
  PWtsSessionInfo = PWtsSessionInfoA;
{$ENDIF}

//==============================================================================
// WTS_PROCESS_INFO - returned by WTSEnumerateProcesses (version 1)
//==============================================================================

//
//  WTSEnumerateProcesses() also returns data MiTeCilar to
//  WTSEnumerateServers().  It returns two variables: pProcessInfo and
//  Count.  The latter is the number of WTS_PROCESS_INFO structures
//  contained in the former.  Iteration is MiTeCilar, except that there
//  are four parts to each entry, so it would look like this:
//
//  for ( i=0; i < Count; i++ ) {
//      GetUserNameFromSid( pProcessInfo[i].pUserSid, UserName,
//                          sizeof(UserName) );
//      _tprintf( TEXT("%-5u  %-20s  %-5u  %s\n"),
//              pProcessInfo[i].SessionId,
//              UserName,
//              pProcessInfo[i].ProcessId,
//              pProcessInfo[i].pProcessName );
//  }
//
//  The memory returned is also segmented as the above, with all the
//  structures allocated at the start and the string data at the end.
//  We'll use S for the SessionId, R for the ProcessId, P for the
//  pProcessName pointer and D for the string data, and U for pUserSid:
//
//  S1 R1 P1 U1 S2 R2 P2 U2 S3 R3 P3 U3 ... Sn Rn Pn Un D1 D2 D3 ... Dn
//
//  As above, this makes it easier to iterate the processes.
//

type
  PWTS_PROCESS_INFOW = ^WTS_PROCESS_INFOW;
  {$EXTERNALSYM PWTS_PROCESS_INFOW}
  _WTS_PROCESS_INFOW = record
    SessionId: DWORD;     // session id
    ProcessId: DWORD;     // process id
    pProcessName: LPWSTR; // name of process
    pUserSid: PSID;       // user's SID
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFOW}
  WTS_PROCESS_INFOW = _WTS_PROCESS_INFOW;
  {$EXTERNALSYM WTS_PROCESS_INFOW}
  TWtsProcessInfoW = WTS_PROCESS_INFOW;
  PWtsProcessInfoW = PWTS_PROCESS_INFOW;

  PWTS_PROCESS_INFOA = ^WTS_PROCESS_INFOA;
  {$EXTERNALSYM PWTS_PROCESS_INFOA}
  _WTS_PROCESS_INFOA = record
    SessionId: DWORD;    // session id
    ProcessId: DWORD;    // process id
    pProcessName: LPSTR; // name of process
    pUserSid: PSID;      // user's SID
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFOA}
  WTS_PROCESS_INFOA = _WTS_PROCESS_INFOA;
  {$EXTERNALSYM WTS_PROCESS_INFOA}
  TWtsProcessInfoA = WTS_PROCESS_INFOA;
  PWtsProcessInfoA = PWTS_PROCESS_INFOA;

{$IFDEF UNICODE}
  WTS_PROCESS_INFO = WTS_PROCESS_INFOW;
  {$EXTERNALSYM WTS_PROCESS_INFO}
  PWTS_PROCESS_INFO = PWTS_PROCESS_INFOW;
  {$EXTERNALSYM PWTS_PROCESS_INFO}
  TWtsProcessInfo = TWtsProcessInfoW;
  PWtsProcessInfo = PWtsProcessInfoW;
{$ELSE}
  WTS_PROCESS_INFO = WTS_PROCESS_INFOA;
  {$EXTERNALSYM WTS_PROCESS_INFO}
  PWTS_PROCESS_INFO = PWTS_PROCESS_INFOA;
  {$EXTERNALSYM PWTS_PROCESS_INFO}
  TWtsProcessInfo = TWtsProcessInfoA;
  PWtsProcessInfo = PWtsProcessInfoA;
{$ENDIF}

//==============================================================================
// WTS_PROCESS_INFO_EX - returned by WTSEnumerateProcessesEx
//==============================================================================

type
  PWTS_PROCESS_INFO_EXW = ^WTS_PROCESS_INFO_EXW;
  {$EXTERNALSYM PWTS_PROCESS_INFO_EXW}
  _WTS_PROCESS_INFO_EXW = record
    SessionId: DWORD;     // session id
    ProcessId: DWORD;     // process id
    pProcessName: LPWSTR; // name of process
    pUserSid: PSID;       // user's SID
    NumberOfThreads: DWORD;
    HandleCount: DWORD;
    PagefileUsage: DWORD;
    PeakPagefileUsage: DWORD;
    WorkingSetSize: DWORD;
    PeakWorkingSetSize: DWORD;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFO_EXW}
  WTS_PROCESS_INFO_EXW = _WTS_PROCESS_INFO_EXW;
  {$EXTERNALSYM WTS_PROCESS_INFO_EXW}
  TWtsProcessInfoExW = WTS_PROCESS_INFO_EXW;
  PWtsProcessInfoExW = PWTS_PROCESS_INFO_EXW;

  PWTS_PROCESS_INFO_EXA = ^WTS_PROCESS_INFO_EXA;
  {$EXTERNALSYM PWTS_PROCESS_INFO_EXA}
  _WTS_PROCESS_INFO_EXA = record
    SessionId: DWORD;    // session id
    ProcessId: DWORD;    // process id
    pProcessName: LPSTR; // name of process
    pUserSid: PSID;      // user's SID
    NumberOfThreads: DWORD;
    HandleCount: DWORD;
    PagefileUsage: DWORD;
    PeakPagefileUsage: DWORD;
    WorkingSetSize: DWORD;
    PeakWorkingSetSize: DWORD;
    UserTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
  end;
  {$EXTERNALSYM _WTS_PROCESS_INFO_EXA}
  WTS_PROCESS_INFO_EXA = _WTS_PROCESS_INFO_EXA;
  {$EXTERNALSYM WTS_PROCESS_INFO_EXA}
  TWtsProcessInfoExA = WTS_PROCESS_INFO_EXA;
  PWtsProcessInfoExA = PWTS_PROCESS_INFO_EXA;

{$IFDEF UNICODE}
  WTS_PROCESS_INFO_EX = WTS_PROCESS_INFO_EXW;
  {$EXTERNALSYM WTS_PROCESS_INFO_EX}
  PWTS_PROCESS_INFO_EX = PWTS_PROCESS_INFO_EXW;
  {$EXTERNALSYM PWTS_PROCESS_INFO_EX}
  TWtsProcessInfoEx = TWtsProcessInfoExW;
  PWtsProcessInfoEx = PWtsProcessInfoExW;
{$ELSE}
  WTS_PROCESS_INFO_EX = WTS_PROCESS_INFO_EXA;
  {$EXTERNALSYM WTS_PROCESS_INFO_EX}
  PWTS_PROCESS_INFO_EX = PWTS_PROCESS_INFO_EXA;
  {$EXTERNALSYM PWTS_PROCESS_INFO_EX}
  TWtsProcessInfoEx = TWtsProcessInfoExA;
  PWtsProcessInfoEx = PWtsProcessInfoExA;
{$ENDIF}


//==============================================================================
// WTS_INFO_CLASS - WTSQuerySessionInformation
// (See additional typedefs for more info on structures)
//==============================================================================

const
  WTS_PROTOCOL_TYPE_CONSOLE = 0; // Console
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_CONSOLE}
  WTS_PROTOCOL_TYPE_ICA     = 1; // ICA Protocol
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_ICA}
  WTS_PROTOCOL_TYPE_RDP     = 2; // RDP Protocol
  {$EXTERNALSYM WTS_PROTOCOL_TYPE_RDP}

type
  _WTS_INFO_CLASS = (
    WTSInitialProgram,
    WTSApplicationName,
    WTSWorkingDirectory,
    WTSOEMId,
    WTSSessionId,
    WTSUserName,
    WTSWinStationName,
    WTSDomainName,
    WTSConnectState,
    WTSClientBuildNumber,
    WTSClientName,
    WTSClientDirectory,
    WTSClientProductId,
    WTSClientHardwareId,
    WTSClientAddress,
    WTSClientDisplay,
    WTSClientProtocolType,
    WTSIdleTime,
    WTSLogonTime,
    WTSIncomingBytes,
    WTSOutgoingBytes,
    WTSIncomingFrames,
    WTSOutgoingFrames,
    WTSClientInfo,
    WTSSessionInfo,
    WTSSessionInfoEx,
    WTSConfigInfo,
    WTSValidationInfo,
    WTSSessionAddressV4,
    WTSIsRemoteSession
    );
  {$EXTERNALSYM _WTS_INFO_CLASS}
  WTS_INFO_CLASS = _WTS_INFO_CLASS;
  TWtsInfoClass = WTS_INFO_CLASS;

//==============================================================================
// WTSQuerySessionInformation - (WTSClientAddress)
//==============================================================================

type
  PWTS_CLIENT_ADDRESS = ^WTS_CLIENT_ADDRESS;
  {$EXTERNALSYM PWTS_CLIENT_ADDRESS}
  _WTS_CLIENT_ADDRESS = record
    AddressFamily: DWORD;           // AF_INET, AF_IPX, AF_NETBIOS, AF_UNSPEC
    Address: array [0..19] of BYTE; // client network address
  end;
  {$EXTERNALSYM _WTS_CLIENT_ADDRESS}
  WTS_CLIENT_ADDRESS = _WTS_CLIENT_ADDRESS;
  {$EXTERNALSYM WTS_CLIENT_ADDRESS}
  TWtsClientAddress = WTS_CLIENT_ADDRESS;
  PWtsClientAddress = PWTS_CLIENT_ADDRESS;

//==============================================================================
// WTSQuerySessionInformation - (WTSClientDisplay)
//==============================================================================

type
  PWTS_CLIENT_DISPLAY = ^WTS_CLIENT_DISPLAY;
  {$EXTERNALSYM PWTS_CLIENT_DISPLAY}
  _WTS_CLIENT_DISPLAY = record
    HorizontalResolution: DWORD; // horizontal dimensions, in pixels
    VerticalResolution: DWORD;   // vertical dimensions, in pixels
    ColorDepth: DWORD;           // 1=16, 2=256, 4=64K, 8=16M
  end;
  {$EXTERNALSYM _WTS_CLIENT_DISPLAY}
  WTS_CLIENT_DISPLAY = _WTS_CLIENT_DISPLAY;
  {$EXTERNALSYM WTS_CLIENT_DISPLAY}
  TWtsClientDisplay = WTS_CLIENT_DISPLAY;
  PWtsClientDisplay = PWTS_CLIENT_DISPLAY;

//==============================================================================
// WTS_CONFIG_CLASS - WTSQueryUserConfig/WTSSetUserConfig
//==============================================================================

type
  _WTS_CONFIG_CLASS = (
    //Initial program settings
    WTSUserConfigInitialProgram,         	// string returned/expected
    WTSUserConfigWorkingDirectory,       	// string returned/expected
    WTSUserConfigfInheritInitialProgram, 	// DWORD returned/expected
    //
    WTSUserConfigfAllowLogonTerminalServer, 	//DWORD returned/expected
    //Timeout settings
    WTSUserConfigTimeoutSettingsConnections, 	//DWORD returned/expected
    WTSUserConfigTimeoutSettingsDisconnections, //DWORD returned/expected
    WTSUserConfigTimeoutSettingsIdle, 	        //DWORD returned/expected
    //Client device settings
    WTSUserConfigfDeviceClientDrives,  		//DWORD returned/expected
    WTSUserConfigfDeviceClientPrinters,         //DWORD returned/expected
    WTSUserConfigfDeviceClientDefaultPrinter,   //DWORD returned/expected
    //Connection settings
    WTSUserConfigBrokenTimeoutSettings,         //DWORD returned/expected
    WTSUserConfigReconnectSettings,             //DWORD returned/expected
    //Modem settings
    WTSUserConfigModemCallbackSettings,         //DWORD returned/expected
    WTSUserConfigModemCallbackPhoneNumber,      // string returned/expected
    //Shadow settings
    WTSUserConfigShadowingSettings,             //DWORD returned/expected
    //User Profile settings
    WTSUserConfigTerminalServerProfilePath,     // string returned/expected
    //Terminal Server home directory
    WTSUserConfigTerminalServerHomeDir,         // string returned/expected
    WTSUserConfigTerminalServerHomeDirDrive,    // string returned/expected
    WTSUserConfigfTerminalServerRemoteHomeDir); // DWORD 0:LOCAL 1:REMOTE
  {$EXTERNALSYM _WTS_CONFIG_CLASS}
  WTS_CONFIG_CLASS = _WTS_CONFIG_CLASS;
  TWtsConfigClass = WTS_CONFIG_CLASS;

//==============================================================================
// WTSCLIENT - WTSQueryUserConfig/WTSClientInfo
//==============================================================================

  _WTSCLIENTW = record
    ClientName : array[0..CLIENTNAME_LENGTH { + 1 - 1}] of WCHAR;
    Domain : array[0..DOMAIN_LENGTH { + 1 - 1}] of WCHAR;
    UserName : array[0..USERNAME_LENGTH { + 1 - 1}] of WCHAR;
    WorkDirectory : array[0..MAX_PATH { + 1 - 1}] of WCHAR;
    InitialProgram : array[0..MAX_PATH { + 1 - 1}] of WCHAR;
    EncryptionLevel : BYTE;       // security level of encryption pd
    ClientAddressFamily : ULONG;
    ClientAddress : array[0..CLIENTADDRESS_LENGTH { + 1 - 1}] of USHORT;
    HRes : USHORT;
    VRes : USHORT;
    ColorDepth : USHORT;
    ClientDirectory : array[0..MAX_PATH { + 1 - 1}] of WCHAR;
    ClientBuildNumber : ULONG;
    ClientHardwareId : ULONG;    // client software serial number
    ClientProductId : USHORT;     // client software product id
    OutBufCountHost : USHORT;     // number of outbufs on host
    OutBufCountClient : USHORT;   // number of outbufs on client
    OutBufLength : USHORT;        // length of outbufs in bytes
    DeviceId : array[0..MAX_PATH { + 1 - 1}] of WCHAR;
  end;
  {$EXTERNALSYM _WTSCLIENTW}
  WTSCLIENTW = _WTSCLIENTW;
  {$EXTERNALSYM WTSCLIENTW}
  PWTSCLIENTW = ^WTSCLIENTW;
  {$EXTERNALSYM PWTSCLIENTW}
  TWtsClientW = WTSCLIENTW;


  _WTSCLIENTA = record
    ClientName : array[0..CLIENTNAME_LENGTH { + 1 - 1}] of AnsiChar;
    Domain : array[0..DOMAIN_LENGTH { + 1 - 1}] of AnsiChar;
    UserName : array[0..USERNAME_LENGTH { + 1 - 1}] of AnsiChar;
    WorkDirectory : array[0..MAX_PATH { + 1 - 1}] of AnsiChar;
    InitialProgram : array[0..MAX_PATH { + 1 - 1}] of AnsiChar;
    EncryptionLevel : BYTE;       // security level of encryption pd
    ClientAddressFamily : ULONG;
    ClientAddress : array[0..CLIENTADDRESS_LENGTH { + 1 - 1}] of USHORT;
    HRes : USHORT;
    VRes : USHORT;
    ColorDepth : USHORT;
    ClientDirectory : array[0..MAX_PATH { + 1 - 1}] of AnsiChar;
    ClientBuildNumber : ULONG;
    ClientHardwareId : ULONG;    // client software serial number
    ClientProductId : USHORT;     // client software product id
    OutBufCountHost : USHORT;     // number of outbufs on host
    OutBufCountClient : USHORT;   // number of outbufs on client
    OutBufLength : USHORT;        // length of outbufs in bytes
    DeviceId : array[0..MAX_PATH { + 1 - 1}] of AnsiChar;
  end;
  {$EXTERNALSYM _WTSCLIENTA}
  WTSCLIENTA = _WTSCLIENTA;
  {$EXTERNALSYM WTSCLIENTA}
  PWTSCLIENTA = ^WTSCLIENTA;
  {$EXTERNALSYM PWTSCLIENTA}
  TWtsClientA = WTSCLIENTA;

  {$IFDEF UNICODE}
    WTSCLIENT = WTSCLIENTW;
    {$EXTERNALSYM WTSCLIENT}
    PWTSCLIENT = PWTSCLIENTW;
    {$EXTERNALSYM PWTSCLIENT}
  {$ELSE}
    WTSCLIENT = WTSCLIENTA;
    {$EXTERNALSYM WTSCLIENT}
    PWTSCLIENT = PWTSCLIENTA;
    {$EXTERNALSYM PWTSCLIENT}
  {$ENDIF}
  TWtsClient = WTSCLIENT;



{$IFDEF FALSE}

// There we're remove in June 2001 PSDK (pre-release)

  PWTS_USER_CONFIG_SET_NWSERVERW = ^WTS_USER_CONFIG_SET_NWSERVERW;
  {$EXTERNALSYM PWTS_USER_CONFIG_SET_NWSERVERW}
  _WTS_USER_CONFIG_SET_NWSERVERW = record
    pNWServerName: LPWSTR;
    pNWDomainAdminName: LPWSTR;
    pNWDomainAdminPassword: LPWSTR;
  end;
  {$EXTERNALSYM _WTS_USER_CONFIG_SET_NWSERVERW}
  WTS_USER_CONFIG_SET_NWSERVERW = _WTS_USER_CONFIG_SET_NWSERVERW;
  {$EXTERNALSYM WTS_USER_CONFIG_SET_NWSERVERW}
  TWtsUserConfigSetNwserverW = WTS_USER_CONFIG_SET_NWSERVERW;
  PWtsUserConfigSetNwserverW = PWTS_USER_CONFIG_SET_NWSERVERW;

  PWTS_USER_CONFIG_SET_NWSERVERA = ^WTS_USER_CONFIG_SET_NWSERVERA;
  {$EXTERNALSYM PWTS_USER_CONFIG_SET_NWSERVERA}
  _WTS_USER_CONFIG_SET_NWSERVERA = record
    pNWServerName: LPSTR;
    pNWDomainAdminName: LPSTR;
    pNWDomainAdminPassword: LPSTR;
  end;
  {$EXTERNALSYM _WTS_USER_CONFIG_SET_NWSERVERA}
  WTS_USER_CONFIG_SET_NWSERVERA = _WTS_USER_CONFIG_SET_NWSERVERA;
  {$EXTERNALSYM WTS_USER_CONFIG_SET_NWSERVERA}
  TWtsUserConfigSetNwserverA = WTS_USER_CONFIG_SET_NWSERVERA;
  PWtsUserConfigSetNwserverA = PWTS_USER_CONFIG_SET_NWSERVERA;

{$IFDEF UNICODE}
  WTS_USER_CONFIG_SET_NWSERVER  = WTS_USER_CONFIG_SET_NWSERVERW;
  {$EXTERNALSYM WTS_USER_CONFIG_SET_NWSERVER}
  PWTS_USER_CONFIG_SET_NWSERVER = PWTS_USER_CONFIG_SET_NWSERVERW;
  {$EXTERNALSYM PWTS_USER_CONFIG_SET_NWSERVER}
  TWtsUserConfigSetNwserver = TWtsUserConfigSetNwserverW;
  PWtsUserConfigSetNwserver = PWtsUserConfigSetNwserverW;
{$ELSE}
  WTS_USER_CONFIG_SET_NWSERVER  = WTS_USER_CONFIG_SET_NWSERVERA;
  {$EXTERNALSYM WTS_USER_CONFIG_SET_NWSERVER}
  PWTS_USER_CONFIG_SET_NWSERVER = PWTS_USER_CONFIG_SET_NWSERVERA;
  {$EXTERNALSYM PWTS_USER_CONFIG_SET_NWSERVER}
  TWtsUserConfigSetNwserver = TWtsUserConfigSetNwserverA;
  PWtsUserConfigSetNwserver = PWtsUserConfigSetNwserverA;
{$ENDIF}
{$ENDIF}

//==============================================================================
// WTS_EVENT - Event flags for WTSWaitSystemEvent
//==============================================================================

const
  WTS_EVENT_NONE        = $00000000; // return no event
  {$EXTERNALSYM WTS_EVENT_NONE}
  WTS_EVENT_CREATE      = $00000001; // new WinStation created
  {$EXTERNALSYM WTS_EVENT_CREATE}
  WTS_EVENT_DELETE      = $00000002; // existing WinStation deleted
  {$EXTERNALSYM WTS_EVENT_DELETE}
  WTS_EVENT_RENAME      = $00000004; // existing WinStation renamed
  {$EXTERNALSYM WTS_EVENT_RENAME}
  WTS_EVENT_CONNECT     = $00000008; // WinStation connect to client
  {$EXTERNALSYM WTS_EVENT_CONNECT}
  WTS_EVENT_DISCONNECT  = $00000010; // WinStation logged on without client
  {$EXTERNALSYM WTS_EVENT_DISCONNECT}
  WTS_EVENT_LOGON       = $00000020; // user logged on to existing WinStation
  {$EXTERNALSYM WTS_EVENT_LOGON}
  WTS_EVENT_LOGOFF      = $00000040; // user logged off from existing WinStation
  {$EXTERNALSYM WTS_EVENT_LOGOFF}
  WTS_EVENT_STATECHANGE = $00000080; // WinStation state change
  {$EXTERNALSYM WTS_EVENT_STATECHANGE}
  WTS_EVENT_LICENSE     = $00000100; // license state change
  {$EXTERNALSYM WTS_EVENT_LICENSE}
  WTS_EVENT_ALL         = $7fffffff; // wait for all event types
  {$EXTERNALSYM WTS_EVENT_ALL}
  WTS_EVENT_FLUSH       = DWORD($80000000); // unblock all waiters
  {$EXTERNALSYM WTS_EVENT_FLUSH}

//==============================================================================
// WTS_VIRTUAL_CLASS - WTSVirtualChannelQuery
//==============================================================================

type
  _WTS_VIRTUAL_CLASS = (WTSVirtualClientData, WTSVirtualFileHandle);  
  {$EXTERNALSYM _WTS_VIRTUAL_CLASS}
  WTS_VIRTUAL_CLASS = _WTS_VIRTUAL_CLASS;
  {$EXTERNALSYM WTS_VIRTUAL_CLASS}
  TWtsVirtualClass = WTS_VIRTUAL_CLASS;

//==============================================================================
// Windows Terminal Server public APIs
//==============================================================================

function WTSEnumerateServersA(pDomainName: LPSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateServersA}
function WTSEnumerateServersW(pDomainName: LPWSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateServersW}

{$IFDEF UNICODE}
function WTSEnumerateServers(pDomainName: LPWSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateServers}
{$ELSE}
function WTSEnumerateServers(pDomainName: LPSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateServers}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSOpenServerA(pServerName: LPSTR): THANDLE; stdcall;
{$EXTERNALSYM WTSOpenServerA}
function WTSOpenServerW(pServerName: LPWSTR): THANDLE; stdcall;
{$EXTERNALSYM WTSOpenServerW}

{$IFDEF UNICODE}
function WTSOpenServer(pServerName: LPWSTR): THANDLE; stdcall;
{$EXTERNALSYM WTSOpenServer}
{$ELSE}
function WTSOpenServer(pServerName: LPSTR): THANDLE; stdcall;
{$EXTERNALSYM WTSOpenServer}
{$ENDIF}

//------------------------------------------------------------------------------

procedure WTSCloseServer(hServer: THANDLE); stdcall;
{$EXTERNALSYM WTSCloseServer}

//------------------------------------------------------------------------------

function WTSEnumerateSessionsA(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateSessionsA}
function WTSEnumerateSessionsW(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateSessionsW}

{$IFDEF UNICODE}
function WTSEnumerateSessions(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateSessions}
{$ELSE}
function WTSEnumerateSessions(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateSessions}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSEnumerateProcessesA(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesA}
function WTSEnumerateProcessesW(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesW}

{$IFDEF UNICODE}
function WTSEnumerateProcesses(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcesses}
{$ELSE}
function WTSEnumerateProcesses(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcesses}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSEnumerateProcessesExA(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesExA}
function WTSEnumerateProcessesExW(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesExW}

{$IFDEF UNICODE}
function WTSEnumerateProcessesEx(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXW; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesEx}
{$ELSE}
function WTSEnumerateProcessesEx(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXA; var pCount: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSEnumerateProcessesEx}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSTerminateProcess(hServer: THANDLE; ProcessId, ExitCode: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSTerminateProcess}

//------------------------------------------------------------------------------

function WTSQuerySessionInformationA(hServer: THANDLE; SessionId: DWORD;
  WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQuerySessionInformationA}
function WTSQuerySessionInformationW(hServer: THANDLE; SessionId: DWORD;
  WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQuerySessionInformationW}

{$IFDEF UNICODE}
function WTSQuerySessionInformation(hServer: THANDLE; SessionId: DWORD;
  WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQuerySessionInformation}
{$ELSE}
function WTSQuerySessionInformation(hServer: THANDLE; SessionId: DWORD;
  WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQuerySessionInformation}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSGetActiveConsoleSessionId: DWORD; stdcall;
{$EXTERNALSYM WTSGetActiveConsoleSessionId}

//------------------------------------------------------------------------------

function WTSQueryUserConfigA(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQueryUserConfigA}
function WTSQueryUserConfigW(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQueryUserConfigW}

{$IFDEF UNICODE}
function WTSQueryUserConfig(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQueryUserConfig}
{$ELSE}
function WTSQueryUserConfig(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSQueryUserConfig}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSSetUserConfigA(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPSTR; DataLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSSetUserConfigA}
function WTSSetUserConfigW(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPWSTR; DataLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSSetUserConfigW}

{$IFDEF UNICODE}
function WTSSetUserConfig(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPWSTR; DataLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSSetUserConfig}
{$ELSE}
function WTSSetUserConfig(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPSTR; DataLength: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSSetUserConfig}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSSendMessageA(hServer: THANDLE; SessionId: DWORD; pTitle: LPSTR;
  TitleLength: DWORD; pMessage: LPSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSSendMessageA}
function WTSSendMessageW(hServer: THANDLE; SessionId: DWORD; pTitle: LPWSTR;
  TitleLength: DWORD; pMessage: LPWSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSSendMessageW}

{$IFDEF UNICODE}
function WTSSendMessage(hServer: THANDLE; SessionId: DWORD; pTitle: LPWSTR;
  TitleLength: DWORD; pMessage: LPWSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSSendMessage}
{$ELSE}
function WTSSendMessage(hServer: THANDLE; SessionId: DWORD; pTitle: LPSTR;
  TitleLength: DWORD; pMessage: LPSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSSendMessage}
{$ENDIF}

//------------------------------------------------------------------------------

function WTSDisconnectSession(hServer: THANDLE; SessionId: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSDisconnectSession}

//------------------------------------------------------------------------------

function WTSLogoffSession(hServer: THANDLE; SessionId: DWORD; bWait: BOOL): BOOL; stdcall;
{$EXTERNALSYM WTSLogoffSession}

//------------------------------------------------------------------------------

function WTSShutdownSystem(hServer: THANDLE; ShutdownFlag: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSShutdownSystem}

//------------------------------------------------------------------------------

function WTSWaitSystemEvent(hServer: THANDLE; EventMask: DWORD;
  var pEventFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSWaitSystemEvent}

//------------------------------------------------------------------------------

function WTSVirtualChannelOpen(hServer: THANDLE; SessionId: DWORD;
  pVirtualName: LPSTR): THANDLE; stdcall;
{$EXTERNALSYM WTSVirtualChannelOpen}

function WTSVirtualChannelClose(hChannelHandle: THANDLE): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelClose}

function WTSVirtualChannelRead(hChannelHandle: THANDLE; TimeOut: ULONG;
  Buffer: PAnsiChar; BufferSize: ULONG; var pBytesRead: ULONG): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelRead}

function WTSVirtualChannelWrite(hChannelHandle: THANDLE; Buffer: PAnsiChar;
  Length: ULONG; var pBytesWritten: ULONG): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelWrite}

function WTSVirtualChannelPurgeInput(hChannelHandle: THANDLE): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelPurgeInput}

function WTSVirtualChannelPurgeOutput(hChannelHandle: THANDLE): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelPurgeOutput}

function WTSVirtualChannelQuery(hChannelHandle: THANDLE; VirtualClass: WTS_VIRTUAL_CLASS;
  ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSVirtualChannelQuery}

//------------------------------------------------------------------------------

procedure WTSFreeMemory(pMemory: Pointer); stdcall;
{$EXTERNALSYM WTSFreeMemory}

type
  WTS_TYPE_CLASS = (
    WTSTypeProcessInfoLevel0,
    WTSTypeProcessInfoLevel1,
    WTSTypeSessionInfoLevel1);

procedure WTSFreeMemoryEx(TypeClass: WTS_TYPE_CLASS; pMemory: Pointer; NumberOfEntries: ULONG); stdcall;
{$EXTERNALSYM WTSFreeMemoryEx}

// Flags for Console Notification

const
  NOTIFY_FOR_ALL_SESSIONS = 1;
  {$EXTERNALSYM NOTIFY_FOR_ALL_SESSIONS}
  NOTIFY_FOR_THIS_SESSION = 0;
  {$EXTERNALSYM NOTIFY_FOR_THIS_SESSION}

function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM WTSRegisterSessionNotification}

function WTSUnRegisterSessionNotification(hWnd: HWND): BOOL; stdcall;
{$EXTERNALSYM WTSUnRegisterSessionNotification}

function WTSQueryUserToken(SessionId: ULONG; var phToken: THANDLE): BOOL; stdcall;
{$EXTERNALSYM WTSQueryUserToken}

type
  PSessionInfoArray = ^TSessionInfoArray;
  TSessionInfoArray = array[0..0] of TWtsSessionInfo;

  PProcessInfoArray = ^TProcessInfoArray;
  TProcessInfoArray = array[0..0] of TWtsProcessInfo;

  PProcessInfoExArray = ^TProcessInfoExArray;
  TProcessInfoExArray = array[0..0] of TWtsProcessInfoEx;

const
  cConnectState: array[TWtsConnectStateClass] of string = (
    'WTSActive',         { User logged on to WinStation }
    'WTSConnected',      { WinStation connected to client }
    'WTSConnectQuery',   { In the process of connecting to client }
    'WTSShadow',         { Shadowing another WinStation }
    'WTSDisconnected',   { WinStation logged on without client }
    'WTSIdle',           { Waiting for client to connect }
    'WTSListen',         { WinStation is listening for connection }
    'WTSReset',          { WinStation is being reset }
    'WTSDown',           { WinStation is down due to error }
    'WTSInit');          { WinStation in initialization }

  cSessionProto: array[WTS_PROTOCOL_TYPE_CONSOLE..WTS_PROTOCOL_TYPE_RDP] of string =
                      ('Console','ICA','RDP');

var
  WTSAvailable: Boolean;

implementation

const
  wtsapi = 'wtsapi32.dll';

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        Exit;
    end;
    P:=GetProcAddress(ModuleHandle,PChar(ProcName));
    if not Assigned(P) then
      Exit;
  end;
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateServersA = function (pDomainName: LPSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOA; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateServersA: TWTSEnumerateServersA = nil;

function WTSEnumerateServersA;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateServersA), wtsapi, 'WTSEnumerateServersA');
  if Assigned(_WTSEnumerateServersA) then
    Result:=_WTSEnumerateServersA(pDomainName,Reserved,Version,ppServerInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateServersA; external wtsapi name 'WTSEnumerateServersA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateServersW = function(pDomainName: LPWSTR; Reserved, Version: DWORD;
  var ppServerInfo: PWTS_SERVER_INFOW; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateServersW: TWTSEnumerateServersW = nil;

function WTSEnumerateServersW;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateServersW), wtsapi, 'WTSEnumerateServersW');
  if Assigned(_WTSEnumerateServersW) then
    Result:=_WTSEnumerateServersW(pDomainName,Reserved,Version,ppServerInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateServersW; external wtsapi name 'WTSEnumerateServersW';
{$ENDIF DYNAMIC_LINK}

function WTSEnumerateServers;
begin
  {$IFDEF UNICODE}
  Result:=WTSEnumerateServersW(pDomainName,Reserved,Version,ppServerInfo,pCount);
  {$ELSE}
  Result:=WTSEnumerateServersA(pDomainName,Reserved,Version,ppServerInfo,pCount);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSOpenServerA = function(pServerName: LPSTR): THANDLE; stdcall;
var
  _WTSOpenServerA: TWTSOpenServerA = nil;

function WTSOpenServerA;
begin
  GetProcedureAddress(Pointer(@_WTSOpenServerA), wtsapi, 'WTSOpenServerA');
  if Assigned(_WTSOpenServerA) then
    Result:=_WTSOpenServerA(pServerName)
  else
    Result:=INVALID_HANDLE;
end;
{$ELSE}
function WTSOpenServerA; external wtsapi name 'WTSOpenServerA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSOpenServerW = function(pServerName: LPWSTR): THANDLE; stdcall;
var
  _WTSOpenServerW: TWTSOpenServerW;

function WTSOpenServerW;
begin
  GetProcedureAddress(Pointer(@_WTSOpenServerW), wtsapi, 'WTSOpenServerW');
  if Assigned(_WTSOpenServerW) then
    Result:=_WTSOpenServerW(pServerName)
  else
    Result:=INVALID_HANDLE;
end;
{$ELSE}
function WTSOpenServerW; external wtsapi name 'WTSOpenServerW';
{$ENDIF DYNAMIC_LINK}

function WTSOpenServer;
begin
  {$IFDEF UNICODE}
  Result:=WTSOpenServerW(pServerName)
  {$ELSE}
  Result:=WTSOpenServerA(pServerName)
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSCloseServer = procedure(hServer: THANDLE); stdcall;
var
  _WTSCloseServer: TWTSCloseServer = nil;

procedure WTSCloseServer;
begin
  GetProcedureAddress(Pointer(@_WTSCloseServer), wtsapi, 'WTSCloseServer');
  if Assigned(_WTSCloseServer) then
    _WTSCloseServer(hServer);
end;
{$ELSE}
procedure WTSCloseServer; external wtsapi name 'WTSCloseServer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateSessionsA = function(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOA; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateSessionsA: TWTSEnumerateSessionsA = nil;

function WTSEnumerateSessionsA;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateSessionsA), wtsapi, 'WTSEnumerateSessionsA');
  if Assigned(_WTSEnumerateSessionsA) then
    Result:=_WTSEnumerateSessionsA(hServer,Reserved,Version,ppSessionInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateSessionsA; external wtsapi name 'WTSEnumerateSessionsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateSessionsW = function(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateSessionsW: TWTSEnumerateSessionsW = nil;

function WTSEnumerateSessionsW;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateSessionsW), wtsapi, 'WTSEnumerateSessionsW');
  if Assigned(_WTSEnumerateSessionsW) then
    Result:=_WTSEnumerateSessionsW(hServer,Reserved,Version,ppSessionInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateSessionsW; external wtsapi name 'WTSEnumerateSessionsW';
{$ENDIF DYNAMIC_LINK}

function WTSEnumerateSessions;
begin
  {$IFDEF UNICODE}
  Result:=WTSEnumerateSessionsW(hServer,Reserved,Version,ppSessionInfo,pCount);
  {$ELSE}
  Result:=WTSEnumerateSessionsA(hServer,Reserved,Version,ppSessionInfo,pCount);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateProcessesA = function(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOA; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateProcessesA: TWTSEnumerateProcessesA = nil;

function WTSEnumerateProcessesA;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateProcessesA), wtsapi, 'WTSEnumerateProcessesA');
  if Assigned(_WTSEnumerateProcessesA) then
    Result:=_WTSEnumerateProcessesA(hServer,Reserved,Version,ppProcessInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateProcessesA; external wtsapi name 'WTSEnumerateProcessesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateProcessesW = function(hServer: THANDLE; Reserved: DWORD; Version: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFOW; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateProcessesW: TWTSEnumerateProcessesW = nil;

function WTSEnumerateProcessesW;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateProcessesW), wtsapi, 'WTSEnumerateProcessesW');
  if Assigned(_WTSEnumerateProcessesW) then
    Result:=_WTSEnumerateProcessesW(hServer,Reserved,Version,ppProcessInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateProcessesW; external wtsapi name 'WTSEnumerateProcessesW';
{$ENDIF DYNAMIC_LINK}

function WTSEnumerateProcesses;
begin
  {$IFDEF UNICODE}
  Result:=WTSEnumerateProcessesW(hServer,Reserved,Version,ppProcessInfo,pCount);
  {$ELSE}
  Result:=WTSEnumerateProcessesA(hServer,Reserved,Version,ppProcessInfo,pCount);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateProcessesExA = function(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXA; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateProcessesExA: TWTSEnumerateProcessesExA = nil;

function WTSEnumerateProcessesExA;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateProcessesExA), wtsapi, 'WTSEnumerateProcessesExA');
  if Assigned(_WTSEnumerateProcessesExA) then
    Result:=_WTSEnumerateProcessesExA(hServer,pLevel,SessionId,ppProcessInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateProcessesExA; external wtsapi name 'WTSEnumerateProcessesExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSEnumerateProcessesExW = function(hServer: THANDLE; var pLevel: DWORD; SessionId: DWORD;
  var ppProcessInfo: PWTS_PROCESS_INFO_EXW; var pCount: DWORD): BOOL; stdcall;
var
  _WTSEnumerateProcessesExW: TWTSEnumerateProcessesExW = nil;

function WTSEnumerateProcessesExW;
begin
  GetProcedureAddress(Pointer(@_WTSEnumerateProcessesExW), wtsapi, 'WTSEnumerateProcessesExW');
  if Assigned(_WTSEnumerateProcessesExW) then
    Result:=_WTSEnumerateProcessesExW(hServer,pLevel,SessionId,ppProcessInfo,pCount)
  else
    Result:=False;
end;
{$ELSE}
function WTSEnumerateProcessesExW; external wtsapi name 'WTSEnumerateProcessesExW';
{$ENDIF DYNAMIC_LINK}

function WTSEnumerateProcessesEx;
begin
  {$IFDEF UNICODE}
  Result:=WTSEnumerateProcessesExW(hServer,pLevel,SessionId,ppProcessInfo,pCount);
  {$ELSE}
  Result:=WTSEnumerateProcessesExA(hServer,pLevel,SessionId,ppProcessInfo,pCount);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSTerminateProcess = function(hServer: THANDLE; ProcessId, ExitCode: DWORD): BOOL; stdcall;
var
  _WTSTerminateProcess: TWTSTerminateProcess = nil;

function WTSTerminateProcess;
begin
  GetProcedureAddress(Pointer(@_WTSTerminateProcess), wtsapi, 'WTSTerminateProcess');
  if Assigned(_WTSTerminateProcess) then
    Result:=_WTSTerminateProcess(hServer,ProcessId,ExitCode)
  else
    Result:=False;
end;
{$ELSE}
function WTSTerminateProcess; external wtsapi name 'WTSTerminateProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSQuerySessionInformation = function(hServer: THANDLE; SessionId: DWORD;
  WTSInfoClass: WTS_INFO_CLASS; var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
var
  _WTSQuerySessionInformationA: TWTSQuerySessionInformation = nil;

function WTSQuerySessionInformationA;
begin
  GetProcedureAddress(Pointer(@_WTSQuerySessionInformationA), wtsapi, 'WTSQuerySessionInformationA');
  if Assigned(_WTSQuerySessionInformationA) then
    Result:=_WTSQuerySessionInformationA(hServer,SessionId,WTSInfoClass,ppBuffer,pBytesReturned)
  else
    Result:=False;
end;
{$ELSE}
function WTSQuerySessionInformationA; external wtsapi name 'WTSQuerySessionInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WTSQuerySessionInformationW: TWTSQuerySessionInformation = nil;

function WTSQuerySessionInformationW;
begin
  GetProcedureAddress(Pointer(@_WTSQuerySessionInformationW), wtsapi, 'WTSQuerySessionInformationW');
  if Assigned(_WTSQuerySessionInformationW) then
    Result:=_WTSQuerySessionInformationW(hServer,SessionId,WTSInfoClass,ppBuffer,pBytesReturned)
  else
    Result:=False;
end;
{$ELSE}
function WTSQuerySessionInformationW; external wtsapi name 'WTSQuerySessionInformationW';
{$ENDIF DYNAMIC_LINK}

function WTSQuerySessionInformation;
begin
  {$IFDEF UNICODE}
  Result:=WTSQuerySessionInformationW(hServer,SessionId,WTSInfoClass,ppBuffer,pBytesReturned);
  {$ELSE}
  Result:=WTSQuerySessionInformationA(hServer,SessionId,WTSInfoClass,ppBuffer,pBytesReturned);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSGetActiveConsoleSessionId = function: DWORD; stdcall;
var
  _WTSGetActiveConsoleSessionId: TWTSGetActiveConsoleSessionId = nil;

function WTSGetActiveConsoleSessionId;
begin
  GetProcedureAddress(Pointer(@_WTSGetActiveConsoleSessionId), kernel32, 'WTSGetActiveConsoleSessionId');
  if Assigned(_WTSGetActiveConsoleSessionId) then
    Result:=_WTSGetActiveConsoleSessionId
  else
    Result:=DWORD(-1);
end;
{$ELSE}
function WTSGetActiveConsoleSessionId; external kernel32 name 'WTSGetActiveConsoleSessionId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSQueryUserConfigA = function(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
var
  _WTSQueryUserConfigA: TWTSQueryUserConfigA = nil;

function WTSQueryUserConfigA;
begin
  GetProcedureAddress(Pointer(@_WTSQueryUserConfigA), wtsapi, 'WTSQueryUserConfigA');
  if Assigned(_WTSQueryUserConfigA) then
    Result:=_WTSQueryUserConfigA(pServerName,pUserName,WTSConfigClass,ppBuffer,pBytesReturned)
  else
    Result:=False;
end;
{$ELSE}
function WTSQueryUserConfigA; external wtsapi name 'WTSQueryUserConfigA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSQueryUserConfigW = function(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  var ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
var
  _WTSQueryUserConfigW: TWTSQueryUserConfigW = nil;

function WTSQueryUserConfigW;
begin
  GetProcedureAddress(Pointer(@_WTSQueryUserConfigW), wtsapi, 'WTSQueryUserConfigW');
  if Assigned(_WTSQueryUserConfigW) then
    Result:=_WTSQueryUserConfigW(pServerName,pUserName,WTSConfigClass,ppBuffer,pBytesReturned)
  else
    Result:=False;
end;
{$ELSE}
function WTSQueryUserConfigW; external wtsapi name 'WTSQueryUserConfigW';
{$ENDIF DYNAMIC_LINK}

function WTSQueryUserConfig;
begin
  {$IFDEF UNICODE}
  Result:=WTSQueryUserConfigW(pServerName,pUserName,WTSConfigClass,ppBuffer,pBytesReturned);
  {$ELSE}
  Result:=WTSQueryUserConfigA(pServerName,pUserName,WTSConfigClass,ppBuffer,pBytesReturned);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSSetUserConfigA = function(pServerName, pUserName: LPSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPSTR; DataLength: DWORD): BOOL; stdcall;
var
  _WTSSetUserConfigA: TWTSSetUserConfigA = nil;

function WTSSetUserConfigA;
begin
  GetProcedureAddress(Pointer(@_WTSSetUserConfigA), wtsapi, 'WTSSetUserConfigA');
  if Assigned(_WTSSetUserConfigA) then
    Result:=_WTSSetUserConfigA(pServerName,pUserName,WTSConfigClass,pBuffer,DataLength)
  else
    Result:=False;
end;
{$ELSE}
function WTSSetUserConfigA; external wtsapi name 'WTSSetUserConfigA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSSetUserConfigW = function(pServerName, pUserName: LPWSTR; WTSConfigClass: WTS_CONFIG_CLASS;
  pBuffer: LPWSTR; DataLength: DWORD): BOOL; stdcall;
var
  _WTSSetUserConfigW: TWTSSetUserConfigW = nil;

function WTSSetUserConfigW;
begin
  GetProcedureAddress(Pointer(@_WTSSetUserConfigW), wtsapi, 'WTSSetUserConfigW');
  if Assigned(_WTSSetUserConfigW) then
    Result:=_WTSSetUserConfigW(pServerName,pUserName,WTSConfigClass,pBuffer,DataLength)
  else
    Result:=False;
end;
{$ELSE}
function WTSSetUserConfigW; external wtsapi name 'WTSSetUserConfigW';
{$ENDIF DYNAMIC_LINK}

function WTSSetUserConfig;
begin
  {$IFDEF UNICODE}
  Result:=WTSSetUserConfigW(pServerName,pUserName,WTSConfigClass,pBuffer,DataLength);
  {$ELSE}
  Result:=WTSSetUserConfigA(pServerName,pUserName,WTSConfigClass,pBuffer,DataLength);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSSendMessageA = function(hServer: THANDLE; SessionId: DWORD; pTitle: LPSTR;
  TitleLength: DWORD; pMessage: LPSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
var
  _WTSSendMessageA: TWTSSendMessageA = nil;

function WTSSendMessageA;
begin
  GetProcedureAddress(Pointer(@_WTSSendMessageA), wtsapi, 'WTSSendMessageA');
  if Assigned(_WTSSendMessageA) then
    Result:=_WTSSendMessageA(hServer,SessionId,pTitle,TitleLength,pMessage,MessageLength,Style,Timeout,pResponse,bWait)
  else
    Result:=False;
end;
{$ELSE}
function WTSSendMessageA; external wtsapi name 'WTSSendMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSSendMessageW = function(hServer: THANDLE; SessionId: DWORD; pTitle: LPWSTR;
  TitleLength: DWORD; pMessage: LPWSTR; MessageLength: DWORD; Style: DWORD;
  Timeout: DWORD; var pResponse: DWORD; bWait: BOOL): BOOL; stdcall;
var
  _WTSSendMessageW: TWTSSendMessageW = nil;

function WTSSendMessageW;
begin
  GetProcedureAddress(Pointer(@_WTSSendMessageW), wtsapi, 'WTSSendMessageW');
  if Assigned(_WTSSendMessageW) then
    Result:=_WTSSendMessageW(hServer,SessionId,pTitle,TitleLength,pMessage,MessageLength,Style,Timeout,pResponse,bWait)
  else
    Result:=False;
end;
{$ELSE}
function WTSSendMessageW; external wtsapi name 'WTSSendMessageW';
{$ENDIF DYNAMIC_LINK}

function WTSSendMessage;
begin
  {$IFDEF UNICODE}
  Result:=WTSSendMessageW(hServer,SessionId,pTitle,TitleLength,pMessage,MessageLength,Style,Timeout,pResponse,bWait);
  {$ELSE}
  Result:=WTSSendMessageA(hServer,SessionId,pTitle,TitleLength,pMessage,MessageLength,Style,Timeout,pResponse,bWait);
  {$ENDIF}
end;

{$IFDEF DYNAMIC_LINK}
type
  TWTSDisconnectSession = function(hServer: THANDLE; SessionId: DWORD; bWait: BOOL): BOOL; stdcall;
var
  _WTSDisconnectSession: TWTSDisconnectSession = nil;

function WTSDisconnectSession;
begin
  GetProcedureAddress(Pointer(@_WTSDisconnectSession), wtsapi, 'WTSDisconnectSession');
  if Assigned(_WTSDisconnectSession) then
    Result:=_WTSDisconnectSession(hServer,SessionId,bWait)
  else
    Result:=False;
end;
{$ELSE}
function WTSDisconnectSession; external wtsapi name 'WTSDisconnectSession';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSLogoffSession = function(hServer: THANDLE; SessionId: DWORD; bWait: BOOL): BOOL; stdcall;
var
  _WTSLogoffSession: TWTSLogoffSession = nil;

function WTSLogoffSession;
begin
  GetProcedureAddress(Pointer(@_WTSLogoffSession), wtsapi, 'WTSLogoffSession');
  if Assigned(_WTSLogoffSession) then
    Result:=_WTSLogoffSession(hServer,SessionId,bWait)
  else
    Result:=False;
end;
{$ELSE}
function WTSLogoffSession; external wtsapi name 'WTSLogoffSession';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSShutdownSystem = function(hServer: THANDLE; ShutdownFlag: DWORD): BOOL; stdcall;
var
  _WTSShutdownSystem: TWTSShutdownSystem = nil;

function WTSShutdownSystem;
begin
  GetProcedureAddress(Pointer(@_WTSShutdownSystem), wtsapi, 'WTSShutdownSystem');
  if Assigned(_WTSShutdownSystem) then
    Result:=_WTSShutdownSystem(hServer,ShutdownFlag)
  else
    Result:=False;
end;
{$ELSE}
function WTSShutdownSystem; external wtsapi name 'WTSShutdownSystem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSWaitSystemEvent = function(hServer: THANDLE; EventMask: DWORD; var pEventFlags: DWORD): BOOL; stdcall;
var
  _WTSWaitSystemEvent: TWTSWaitSystemEvent = nil;

function WTSWaitSystemEvent;
begin
  GetProcedureAddress(Pointer(@_WTSWaitSystemEvent), wtsapi, 'WTSWaitSystemEvent');
  if Assigned(_WTSWaitSystemEvent) then
    Result:=_WTSWaitSystemEvent(hServer,EventMask,pEventFlags)
  else
    Result:=False;
end;
{$ELSE}
function WTSWaitSystemEvent; external wtsapi name 'WTSWaitSystemEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelOpen = function(hServer: THANDLE; SessionId: DWORD;
  pVirtualName: LPSTR): THANDLE; stdcall;
var
  _WTSVirtualChannelOpen: TWTSVirtualChannelOpen = nil;

function WTSVirtualChannelOpen;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelOpen), wtsapi, 'WTSVirtualChannelOpen');
  if Assigned(_WTSVirtualChannelOpen) then
    Result:=_WTSVirtualChannelOpen(hServer,SessionId,pVirtualName)
  else
    Result:=INVALID_HANDLE;
end;
{$ELSE}
function WTSVirtualChannelOpen; external wtsapi name 'WTSVirtualChannelOpen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelClose = function(hChannelHandle: THANDLE): BOOL; stdcall;
var
  _WTSVirtualChannelClose: TWTSVirtualChannelClose = nil;

function WTSVirtualChannelClose;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelClose), wtsapi, 'WTSVirtualChannelClose');
  if Assigned(_WTSVirtualChannelClose) then
    Result:=_WTSVirtualChannelClose(hChannelHandle)
  else
    Result:=False;
end;
{$ELSE}
function WTSVirtualChannelClose; external wtsapi name 'WTSVirtualChannelClose';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelRead = function(hChannelHandle: THANDLE; TimeOut: ULONG;
  Buffer: PAnsiChar; BufferSize: ULONG; var pBytesRead: ULONG): BOOL; stdcall;
var
  _WTSVirtualChannelRead: TWTSVirtualChannelRead = nil;

function WTSVirtualChannelRead;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelRead), wtsapi, 'WTSVirtualChannelRead');
  if Assigned(_WTSVirtualChannelRead) then
    Result:=_WTSVirtualChannelRead(hChannelHandle,TimeOut,Buffer,BufferSize,pBytesRead)
  else
    Result:=False;  
end;
{$ELSE}
function WTSVirtualChannelRead; external wtsapi name 'WTSVirtualChannelRead';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelWrite = function(hChannelHandle: THANDLE; Buffer: PAnsiChar;
  Length: ULONG; var pBytesWritten: ULONG): BOOL; stdcall;
var
  _WTSVirtualChannelWrite: TWTSVirtualChannelWrite = nil;

function WTSVirtualChannelWrite;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelWrite), wtsapi, 'WTSVirtualChannelWrite');
  if Assigned(_WTSVirtualChannelWrite) then
    Result:=_WTSVirtualChannelWrite(hChannelHandle,Buffer,Length,pBytesWritten)
  else
    Result:=False;
end;
{$ELSE}
function WTSVirtualChannelWrite; external wtsapi name 'WTSVirtualChannelWrite';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelPurgeInput = function(hChannelHandle: THANDLE): BOOL; stdcall;
var
  _WTSVirtualChannelPurgeInput: TWTSVirtualChannelPurgeInput = nil;

function WTSVirtualChannelPurgeInput;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelPurgeInput), wtsapi, 'WTSVirtualChannelPurgeInput');
  if Assigned(_WTSVirtualChannelPurgeInput) then
    Result:=_WTSVirtualChannelPurgeInput(hChannelHandle)
  else
    Result:=False;
end;
{$ELSE}
function WTSVirtualChannelPurgeInput; external wtsapi name 'WTSVirtualChannelPurgeInput';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelPurgeOutput = function(hChannelHandle: THANDLE): BOOL; stdcall;
var
  _WTSVirtualChannelPurgeOutput: TWTSVirtualChannelPurgeOutput = nil;

function WTSVirtualChannelPurgeOutput;
begin
  GetProcedureAddress(Pointer(@_WTSVirtualChannelPurgeOutput), wtsapi, 'WTSVirtualChannelPurgeOutput');
  if Assigned(_WTSVirtualChannelPurgeOutput) then
    Result:=_WTSVirtualChannelPurgeOutput(hChannelHandle)
  else
    Result:=False;
end;
{$ELSE}
function WTSVirtualChannelPurgeOutput; external wtsapi name 'WTSVirtualChannelPurgeOutput';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSVirtualChannelQuery = function(hChannelHandle: THANDLE; VirtualClass: WTS_VIRTUAL_CLASS;
  ppBuffer: Pointer; var pBytesReturned: DWORD): BOOL; stdcall;
var
  _WTSVirtualChannelQuery: TWTSVirtualChannelQuery = nil;

function WTSVirtualChannelQuery;
begin
  Result:=False;
  GetProcedureAddress(Pointer(@_WTSVirtualChannelQuery), wtsapi, 'WTSVirtualChannelQuery');
  if Assigned(_WTSVirtualChannelQuery) then
    Result:=_WTSVirtualChannelQuery(hChannelHandle,VirtualClass,ppBuffer,pBytesReturned)
  else
    Result:=Result;
end;
{$ELSE}
function WTSVirtualChannelQuery; external wtsapi name 'WTSVirtualChannelQuery';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSFreeMemory = procedure(pMemory: Pointer); stdcall;
var
  _WTSFreeMemory: TWTSFreeMemory = nil;

procedure WTSFreeMemory;
begin
  GetProcedureAddress(Pointer(@_WTSFreeMemory), wtsapi, 'WTSFreeMemory');
  if Assigned(_WTSFreeMemory) then
    _WTSFreeMemory(pMemory);
end;
{$ELSE}
procedure WTSFreeMemory; external wtsapi name 'WTSFreeMemory';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSFreeMemoryEx = procedure(TypeClass: WTS_TYPE_CLASS; pMemory: Pointer; NumberOfEntries: ULONG); stdcall;
var
  _WTSFreeMemoryEx: TWTSFreeMemoryEx = nil;

procedure WTSFreeMemoryEx;
begin
  GetProcedureAddress(Pointer(@_WTSFreeMemoryEx), wtsapi, 'WTSFreeMemoryEx');
  if Assigned(_WTSFreeMemoryEx) then
    _WTSFreeMemoryEx(TypeClass,pMemory,NumberOfEntries);
end;
{$ELSE}
procedure WTSFreeMemoryEx; external wtsapi name 'WTSFreeMemoryEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSRegisterSessionNotification = function(hWnd: HWND; dwFlags: DWORD): BOOL; stdcall;
var
  _WTSRegisterSessionNotification: TWTSRegisterSessionNotification = nil;

function WTSRegisterSessionNotification;
begin
  GetProcedureAddress(Pointer(@_WTSRegisterSessionNotification), wtsapi, 'WTSRegisterSessionNotification');
  if Assigned(_WTSRegisterSessionNotification) then
    Result:=_WTSRegisterSessionNotification(hWnd,dwFlags)
  else begin
    Result:=False;
    SetLastError(0);
  end;
end;
{$ELSE}
function WTSRegisterSessionNotification; external wtsapi name 'WTSRegisterSessionNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSUnRegisterSessionNotification = function(hWnd: HWND): BOOL; stdcall;
var
  _WTSUnRegisterSessionNotification: TWTSUnRegisterSessionNotification = nil;

function WTSUnRegisterSessionNotification;
begin
  GetProcedureAddress(Pointer(@_WTSUnRegisterSessionNotification), wtsapi, 'WTSUnRegisterSessionNotification');
  if Assigned(_WTSUnRegisterSessionNotification) then
    Result:=_WTSUnRegisterSessionNotification(hWnd)
  else
    Result:=False;
end;
{$ELSE}
function WTSUnRegisterSessionNotification; external wtsapi name 'WTSUnRegisterSessionNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
type
  TWTSQueryUserToken = function(SessionId: ULONG; var phToken: THANDLE): BOOL; stdcall;
var
  _WTSQueryUserToken: TWTSQueryUserToken = nil;

function WTSQueryUserToken;
begin
  GetProcedureAddress(Pointer(@_WTSQueryUserToken), wtsapi, 'WTSQueryUserToken');
  if Assigned(_WTSQueryUserToken) then
    Result:=_WTSQueryUserToken(SessionId,phToken)
  else
    Result:=False;
end;
{$ELSE}
function WTSQueryUserToken; external wtsapi name 'WTSQueryUserToken';
{$ENDIF DYNAMIC_LINK}


initialization
{$IFDEF DYNAMIC_LINK}
  GetProcedureAddress(Pointer(@_WTSEnumerateServersA), wtsapi, 'WTSEnumerateServersA');
  WTSAvailable:=Assigned(_WTSEnumerateServersA);
{$ELSE}
  WTSAvailable:=True;
{$ENDIF}
end.
