{*******************************************************}
{               MiTeC Common Routines                   }
{                   LAN Manager API                     }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_NetAPI32;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  NERR_SUCCESS = 0;
  NERR_BASE = 2100;
  NETERR_INCLUDED = 2102;    // The workstation driver is not installed.
  NERR_UnknownServer = 2103;    // The server could not be located.
  NERR_ShareMem = 2104; //An internal error occurred. The network cannot access a shared memory segment.
  NERR_NoNetworkResource = 2105;    // A network resource shortage occurred .
  NERR_RemoteOnly = 2106;    // This operation is not supported on workstations.
  NERR_DevNotRedirected = 2107;    // The device is not connected.
  NERR_ServerNotStarted = 2114;    // The Server service is not started.
  NERR_ItemNotFound = 2115;    // The queue is empty.
  NERR_UnknownDevDir = 2116;    // The device or directory does not exist.
  NERR_RedirectedPath = 2117;    // The operation is invalid on a redirected resource.
  NERR_DuplicateShare = 2118;    // The name has already been shared.
  NERR_NoRoom = 2119;    // The server is currently out of the requested resource.
  NERR_TooManyItems = 2121;    // Requested addition of items exceeds the maximum allowed.
  NERR_InvalidMaxUsers = 2122;    // The Peer service supports only two MiTeCultaneous users.
  NERR_BufTooSmall = 2123;    // The API return buffer is too small.
  NERR_RemoteErr = 2127;    // A remote API error occurred.
  NERR_LanmanIniError = 2131;    // An error occurred when opening or reading the configuration file.
  NERR_NetworkError = 2136;    // A general network error occurred.
  NERR_WkstaInconsistentState = 2137;  // The Workstation service is in an inconsistent state. Restart the computer before restarting the Workstation service.
  NERR_WkstaNotStarted = 2138;    // The Workstation service has not been started.
  NERR_BrowserNotStarted = 2139;    // The requested information is not available.
  NERR_InternalError = 2140;    // An internal Windows NT error occurred.
  NERR_BadTransactConfig = 2141;    // The server is not configured for transactions.
  NERR_InvalidAPI = 2142;    // The requested API is not supported on the remote server.
  NERR_BadEventName = 2143;    // The event name is invalid.
  NERR_DupNameReboot = 2144;  // The computer name already exists on the network. Change it and restart the computer.
  NERR_CfgCompNotFound = 2146;    // The specified component could not be found in the configuration information.
  NERR_CfgParamNotFound = 2147;    // The specified parameter could not be found in the configuration information.
  NERR_LineTooLong = 2149;    // A line in the configuration file is too long.
  NERR_QNotFound = 2150;    // The printer does not exist.
  NERR_JobNotFound = 2151;    // The print job does not exist.
  NERR_DestNotFound = 2152;    // The printer destination cannot be found.
  NERR_DestExists = 2153;    // The printer destination already exists.
  NERR_QExists = 2154;    // The printer queue already exists.
  NERR_QNoRoom = 2155;    // No more printers can be added.
  NERR_JobNoRoom = 2156;    // No more print jobs can be added.
  NERR_DestNoRoom = 2157;    // No more printer destinations can be added.
  NERR_DestIdle = 2158;    // This printer destination is idle and cannot accept control operations.
  NERR_DestInvalidOp = 2159;    // This printer destination request contains an invalid control function.
  NERR_ProcNoRespond = 2160;    // The print processor is not responding.
  NERR_SpoolerNotLoaded = 2161;    // The spooler is not running.
  NERR_DestInvalidState = 2162;    // This operation cannot be performed on the print destination in its current state.
  NERR_QInvalidState = 2163;    // This operation cannot be performed on the printer queue in its current state.
  NERR_JobInvalidState = 2164;    // This operation cannot be performed on the print job in its current state.
  NERR_SpoolNoMemory = 2165;    // A spooler memory allocation failure occurred.
  NERR_DriverNotFound = 2166;    // The device driver does not exist.
  NERR_DataTypeInvalid = 2167;    // The data type is not supported by the print processor.
  NERR_ProcNotFound = 2168;    // The print processor is not installed.
  NERR_ServiceTableLocked = 2180;    // The service database is locked.
  NERR_ServiceTableFull = 2181;    // The service table is full.
  NERR_ServiceInstalled = 2182;    // The requested service has already been started.
  NERR_ServiceEntryLocked = 2183;    // The service does not respond to control actions.
  NERR_ServiceNotInstalled = 2184;    // The service has not been started.
  NERR_BadServiceName = 2185;    // The service name is invalid.
  NERR_ServiceCtlTimeout = 2186;    // The service is not responding to the control function.
  NERR_ServiceCtlBusy = 2187;    // The service control is busy.
  NERR_BadServiceProgName = 2188;    // The configuration file contains an invalid service program name.
  NERR_ServiceNotCtrl = 2189;    // The service could not be controlled in its present state.
  NERR_ServiceKillProc = 2190;    // The service ended abnormally.
  NERR_ServiceCtlNotValid = 2191;    // The requested pause or stop is not valid for this service.
  NERR_NotInDispatchTbl = 2192;    // The service control dispatcher could not find the service name in the dispatch table.
  NERR_BadControlRecv = 2193;    // The service control dispatcher pipe read failed.
  NERR_ServiceNotStarting = 2194;    // A thread for the new service could not be created.
  NERR_AlreadyLoggedOn = 2200;    // This workstation is already logged on to the local-area network.
  NERR_NotLoggedOn = 2201;    // The workstation is not logged on to the local-area network.
  NERR_BadUsername = 2202;    // The user name or group name parameter is invalid.
  NERR_BadPassword = 2203;    // The password parameter is invalid.
  NERR_UnableToAddName_W = 2204;    // @W The logon processor did not add the message alias.
  NERR_UnableToAddName_F = 2205;    // The logon processor did not add the message alias.
  NERR_UnableToDelName_W = 2206;    // @W The logoff processor did not delete the message alias.
  NERR_UnableToDelName_F = 2207;    // The logoff processor did not delete the message alias.
  NERR_LogonsPaused = 2209;    // Network logons are paused.
  NERR_LogonServerConflict = 2210;    // A centralized logon-server conflict occurred.
  NERR_LogonNoUserPath = 2211;    // The server is configured without a valid user path.
  NERR_LogonScriptError = 2212;    // An error occurred while loading or running the logon script.
  NERR_StandaloneLogon = 2214;    // The logon server was not specified. Your computer will be logged on as STANDALONE.
  NERR_LogonServerNotFound = 2215;    // The logon server could not be found.
  NERR_LogonDomainExists = 2216;    // There is already a logon domain for this computer.
  NERR_NonValidatedLogon = 2217;    // The logon server could not validate the logon.
  NERR_ACFNotFound = 2219;    // The security database could not be found.
  NERR_GroupNotFound = 2220;    // The group name could not be found.
  NERR_UserNotFound = 2221;    // The user name could not be found.
  NERR_ResourceNotFound = 2222;    // The resource name could not be found.
  NERR_GroupExists = 2223;    // The group already exists.
  NERR_UserExists = 2224;    // The user account already exists.
  NERR_ResourceExists = 2225;    // The resource permission list already exists.
  NERR_NotPrimary = 2226;    // This operation is only allowed on the primary domain controller of the domain.
  NERR_ACFNotLoaded = 2227;    // The security database has not been started.
  NERR_ACFNoRoom = 2228;    // There are too many names in the user accounts database.
  NERR_ACFFileIOFail = 2229;    // A disk I/O failure occurred.
  NERR_ACFTooManyLists = 2230;    // The limit of 64 entries per resource was exceeded.
  NERR_UserLogon = 2231;    // Deleting a user with a session is not allowed.
  NERR_ACFNoParent = 2232;    // The parent directory could not be located.
  NERR_CanNotGrowSegment = 2233;    // Unable to add to the security database session cache segment.
  NERR_SpeGroupOp = 2234;    // This operation is not allowed on this special group.
  NERR_NotInCache = 2235;    // This user is not cached in user accounts database session cache.
  NERR_UserInGroup = 2236;    // The user already belongs to this group.
  NERR_UserNotInGroup = 2237;    // The user does not belong to this group.
  NERR_AccountUndefined = 2238;    // This user account is undefined.
  NERR_AccountExpired = 2239;    // This user account has expired.
  NERR_InvalidWorkstation = 2240;    // The user is not allowed to log on from this workstation.
  NERR_InvalidLogonHours = 2241;    // The user is not allowed to log on at this time.
  NERR_PasswordExpired = 2242;    // The password of this user has expired.
  NERR_PasswordCantChange = 2243;    // The password of this user cannot change.
  NERR_PasswordHistConflict = 2244;    // This password cannot be used now.
  NERR_PasswordTooShort = 2245;    // The password is shorter than required.
  NERR_PasswordTooRecent = 2246;    // The password of this user is too recent to change.
  NERR_InvalidDatabase = 2247;    // The security database is corrupted.
  NERR_DatabaseUpToDate = 2248;    // No updates are necessary to this replicant network/local security database.
  NERR_SyncRequired = 2249;    // This replicant database is outdated; synchronization is required.
  NERR_UseNotFound = 2250;    // The network connection could not be found.
  NERR_BadAsgType = 2251;    // This asg_type is invalid.
  NERR_DeviceIsShared = 2252;    // This device is currently being shared.
  NERR_NoComputerName = 2270;   // The computer name could not be added as a message alias. The name may already exist on the network.
  NERR_MsgAlreadyStarted = 2271;    // The Messenger service is already started.
  NERR_MsgInitFailed = 2272;    // The Messenger service failed to start.
  NERR_NameNotFound = 2273;    // The message alias could not be found on the network.
  NERR_AlreadyForwarded = 2274;    // This message alias has already been forwarded.
  NERR_AddForwarded = 2275;    // This message alias has been added but is still forwarded.
  NERR_AlreadyExists = 2276;    // This message alias already exists locally.
  NERR_TooManyNames = 2277;    // The maximum number of added message aliases has been exceeded.
  NERR_DelComputerName = 2278;    // The computer name could not be deleted.
  NERR_LocalForward = 2279;    // Messages cannot be forwarded back to the same workstation.
  NERR_GrpMsgProcessor = 2280;    // An error occurred in the domain message processor.
  NERR_PausedRemote = 2281;    // The message was sent, but the recipient has paused the Messenger service.
  NERR_BadReceive = 2282;    // The message was sent but not received.
  NERR_NameInUse = 2283;   // The message alias is currently in use. Try again later.
  NERR_MsgNotStarted = 2284;    // The Messenger service has not been started.
  NERR_NotLocalName = 2285;    // The name is not on the local computer.
  NERR_NoForwardName = 2286;    // The forwarded message alias could not be found on the network.
  NERR_RemoteFull = 2287;    // The message alias table on the remote station is full.
  NERR_NameNotForwarded = 2288;    // Messages for this alias are not currently being forwarded.
  NERR_TruncatedBroadcast = 2289;    // The broadcast message was truncated.
  NERR_InvalidDevice = 2294;    // This is an invalid device name.
  NERR_WriteFault = 2295;    // A write fault occurred.
  NERR_DuplicateName = 2297;    // A duplicate message alias exists on the network.
  NERR_DeleteLater = 2298;    // @W This message alias will be deleted later.
  NERR_IncompleteDel = 2299;    // The message alias was not successfully deleted from all networks.
  NERR_MultipleNets = 2300;    // This operation is not supported on computers with multiple networks.
  NERR_NetNameNotFound = 2310;    // This shared resource does not exist.
  NERR_DeviceNotShared = 2311;    // This device is not shared.
  NERR_ClientNameNotFound = 2312;    // A session does not exist with that computer name.
  NERR_FileIdNotFound = 2314;    // There is not an open file with that identification number.
  NERR_ExecFailure = 2315;    // A failure occurred when executing a remote administration command.
  NERR_TmpFile = 2316;    // A failure occurred when opening a remote temporary file.
  NERR_TooMuchData = 2317;    // The data returned from a remote administration command has been truncated to 64K.
  NERR_DeviceShareConflict = 2318;    // This device cannot be shared as both a spooled and a non-spooled resource.
  NERR_BrowserTableIncomplete = 2319;    // The information in the list of servers may be incorrect.
  NERR_NotLocalDomain = 2320;    // The computer is not active in this domain.
  NERR_IsDfsShare = 2321;    // The share must be removed from the Distributed File System before it can be deleted.
  NERR_DevInvalidOpCode = 2331;    // The operation is invalid for this device.
  NERR_DevNotFound = 2332;    // This device cannot be shared.
  NERR_DevNotOpen = 2333;    // This device was not open.
  NERR_BadQueueDevString = 2334;    // This device name list is invalid.
  NERR_BadQueuePriority = 2335;    // The queue priority is invalid.
  NERR_NoCommDevs = 2337;    // There are no shared communication devices.
  NERR_QueueNotFound = 2338;    // The queue you specified does not exist.
  NERR_BadDevString = 2340;    // This list of devices is invalid.
  NERR_BadDev = 2341;    // The requested device is invalid.
  NERR_InUseBySpooler = 2342;    // This device is already in use by the spooler.
  NERR_CommDevInUse = 2343;    // This device is already in use as a communication device.
  NERR_InvalidComputer = 2351;    // This computer name is invalid.
  NERR_MaxLenExceeded = 2354;    // The string and prefix specified are too long.
  NERR_BadComponent = 2356;    // This path component is invalid.
  NERR_CantType = 2357;    // Could not determine the type of input.
  NERR_TooManyEntries = 2362;    // The buffer for types is not big enough.
  NERR_ProfileFileTooBig = 2370;    // Profile files cannot exceed 64K.
  NERR_ProfileOffset = 2371;    // The start offset is out of range.
  NERR_ProfileCleanup = 2372;    // The system cannot delete current connections to network resources.
  NERR_ProfileUnknownCmd = 2373;    // The system was unable to parse the command line in this file.
  NERR_ProfileLoadErr = 2374;    // An error occurred while loading the profile file.
  NERR_ProfileSaveErr = 2375;    // @W Errors occurred while saving the profile file. The profile was partially saved.
  NERR_LogOverflow = 2377;    // Log file %1 is full.
  NERR_LogFileChanged = 2378;    // This log file has changed between reads.
  NERR_LogFileCorrupt = 2379;    // Log file %1 is corrupt.
  NERR_SourceIsDir = 2380;    // The source path cannot be a directory.
  NERR_BadSource = 2381;    // The source path is illegal.
  NERR_BadDest = 2382;    // The destination path is illegal.
  NERR_DifferentServers = 2383;    // The source and destination paths are on different servers.
  NERR_RunSrvPaused = 2385;    // The Run server you requested is paused.
  NERR_ErrCommRunSrv = 2389;    // An error occurred when communicating with a Run server.
  NERR_ErrorExecingGhost = 2391;    // An error occurred when starting a background process.
  NERR_ShareNotFound = 2392;    // The shared resource you are connected to could not be found.
  NERR_InvalidLana = 2400;    // The LAN adapter number is invalid.
  NERR_OpenFiles = 2401;    // There are open files on the connection.
  NERR_ActiveConns = 2402;    // Active connections still exist.
  NERR_BadPasswordCore = 2403;    // This share name or password is invalid.
  NERR_DevInUse = 2404;    // The device is being accessed by an active process.
  NERR_LocalDrive = 2405;    // The drive letter is in use locally.
  NERR_AlertExists = 2430;    // The specified client is already registered for the specified event.
  NERR_TooManyAlerts = 2431;    // The alert table is full.
  NERR_NoSuchAlert = 2432;    // An invalid or nonexistent alert name was raised.
  NERR_BadRecipient = 2433;    // The alert recipient is invalid.
  NERR_AcctLimitExceeded = 2434;    //A user's session with this server has been deleted
  NERR_InvalidLogSeek = 2440;    // The log file does not contain the requested record number.
  NERR_BadUasConfig = 2450;    // The user accounts database is not configured correctly.
  NERR_InvalidUASOp = 2451;    // This operation is not permitted when the Netlogon service is running.
  NERR_LastAdmin = 2452;    // This operation is not allowed on the last administrative account.
  NERR_DCNotFound = 2453;    // Could not find domain controller for this domain.
  NERR_LogonTrackingError = 2454;    // Could not set logon information for this user.
  NERR_NetlogonNotStarted = 2455;    // The Netlogon service has not been started.
  NERR_CanNotGrowUASFile = 2456;    // Unable to add to the user accounts database.
  NERR_TimeDiffAtDC = 2457;    // This server's clock is not synchronized with the primary domain controller's clock.
  NERR_PasswordMismatch = 2458;    // A password mismatch has been detected.
  NERR_NoSuchServer = 2460;    // The server identification does not specify a valid server.
  NERR_NoSuchSession = 2461;    // The session identification does not specify a valid session.
  NERR_NoSuchConnection = 2462;    // The connection identification does not specify a valid connection.
  NERR_TooManyServers = 2463;    // There is no space for another entry in the table of available servers.
  NERR_TooManySessions = 2464;    // The server has reached the maximum number of sessions it supports.
  NERR_TooManyConnections = 2465;    // The server has reached the maximum number of connections it supports.
  NERR_TooManyFiles = 2466;    // The server cannot open more files because it has reached its maximum number.
  NERR_NoAlternateServers = 2467;    // There are no alternate servers registered on this server.
  NERR_TryDownLevel = 2470;    // Try down-level (remote admin protocol) version of API instead.
  NERR_UPSDriverNotStarted = 2480;    // The UPS driver could not be accessed by the UPS service.
  NERR_UPSInvalidConfig = 2481;    // The UPS service is not configured correctly.
  NERR_UPSInvalidCommPort = 2482;    // The UPS service could not access the specified Comm Port.
  NERR_UPSSignalAsserted = 2483;  // The UPS indicated a line fail or low battery situation. Service not started.
  NERR_UPSShutdownFailed = 2484;    // The UPS service failed to perform a system shut down.
  NERR_BadDosRetCode = 2500;    //The program below returned an MS-DOS error code:
  NERR_ProgNeedsExtraMem = 2501;    //The program below needs more memory:
  NERR_BadDosFunction = 2502;    //The program below called an unsupported MS-DOS function:
  NERR_RemoteBootFailed = 2503;    // The workstation failed to boot.
  NERR_BadFileCheckSum = 2504;    // The file below is corrupt.
  NERR_NoRplBootSystem = 2505;    // No loader is specified in the boot-block definition file.
  NERR_RplLoadrNetBiosErr = 2506;    // NetBIOS returned an error: The NCB and SMB are dumped above.
  NERR_RplLoadrDiskErr = 2507;    // A disk I/O error occurred.
  NERR_ImageParamErr = 2508;    // Image parameter substitution failed.
  NERR_TooManyImageParams = 2509;    // Too many image parameters cross disk sector boundaries.
  NERR_NonDosFloppyUsed = 2510;    // The image was not generated from an MS-DOS diskette formatted with /S.
  NERR_RplBootRestart = 2511;    // Remote boot will be restarted later.
  NERR_RplSrvrCallFailed = 2512;    // The call to the Remoteboot server failed.
  NERR_CantConnectRplSrvr = 2513;    // Cannot connect to the Remoteboot server.
  NERR_CantOpenImageFile = 2514;    // Cannot open image file on the Remoteboot server.
  NERR_CallingRplSrvr = 2515;    // Connecting to the Remoteboot server..
  NERR_StartingRplBoot = 2516;    // Connecting to the Remoteboot server..
  NERR_RplBootServiceTerm = 2517;    // Remote boot service was stopped; check the error log for the cause of the problem.
  NERR_RplBootStartFailed = 2518;    // Remote boot startup failed; check the error log for the cause of the problem.
  NERR_RPL_CONNECTED = 2519;    // A second connection to a Remoteboot resource is not allowed.
  NERR_BrowserConfiguredToNotRun = 2550;    // The browser service was configured with MaintainServerList=No.
  NERR_RplNoAdaptersStarted = 2610;    // Service failed to start since none of the network adapters started with this service.
  NERR_RplBadRegistry = 2611;    // Service failed to start due to bad startup information in the registry.
  NERR_RplBadDatabase = 2612;    // Service failed to start because its database is absent or corrupt.
  NERR_RplRplfilesShare = 2613;    // Service failed to start because RPLFILES share is absent.
  NERR_RplNotRplServer = 2614;    // Service failed to start because RPLUSER group is absent.
  NERR_RplCannotEnum = 2615;    // Cannot enumerate service records.
  NERR_RplWkstaInfoCorrupted = 2616;    // Workstation record information has been corrupted.
  NERR_RplWkstaNotFound = 2617;    // Workstation record was not found.
  NERR_RplWkstaNameUnavailable = 2618;    // Workstation name is in use by some other workstation.
  NERR_RplProfileInfoCorrupted = 2619;    // Profile record information has been corrupted.
  NERR_RplProfileNotFound = 2620;    // Profile record was not found.
  NERR_RplProfileNameUnavailable = 2621;    // Profile name is in use by some other profile.
  NERR_RplProfileNotEmpty = 2622;    // There are workstations using this profile.
  NERR_RplConfigInfoCorrupted = 2623;    // Configuration record information has been corrupted.
  NERR_RplConfigNotFound = 2624;    // Configuration record was not found.
  NERR_RplAdapterInfoCorrupted = 2625;    // Adapter id record information has been corrupted.
  NERR_RplInternal = 2626;    // An internal service error has occurred.
  NERR_RplVendorInfoCorrupted = 2627;    // Vendor id record information has been corrupted.
  NERR_RplBootInfoCorrupted = 2628;    // Boot block record information has been corrupted.
  NERR_RplWkstaNeedsUserAcct = 2629;    // The user account for this workstation record is missing.
  NERR_RplNeedsRPLUSERAcct = 2630;    // The RPLUSER local group could not be found.
  NERR_RplBootNotFound = 2631;    // Boot block record was not found.
  NERR_RplIncompatibleProfile = 2632;    // Chosen profile is incompatible with this workstation.
  NERR_RplAdapterNameUnavailable = 2633;    // Chosen network adapter id is in use by some other workstation.
  NERR_RplConfigNotEmpty = 2634;    // There are profiles using this configuration.
  NERR_RplBootInUse = 2635;    // There are workstations, profiles, or configurations using this boot block.
  NERR_RplBackupDatabase = 2636;    // Service failed to backup Remoteboot database.
  NERR_RplAdapterNotFound = 2637;    // Adapter record was not found.
  NERR_RplVendorNotFound = 2638;    // Vendor record was not found.
  NERR_RplVendorNameUnavailable = 2639;    // Vendor name is in use by some other vendor record.
  NERR_RplBootNameUnavailable = 2640;    // (boot name, vendor id) is in use by some other boot block record.
  NERR_RplConfigNameUnavailable = 2641;    // Configuration name is in use by some other configuration.
  NERR_DfsInternalCorruption = 2660;    //The internal database maintained by the Dfs service is corrupt
  NERR_DfsVolumeDataCorrupt = 2661;    //One of the records in the internal Dfs database is corrupt
  NERR_DfsNoSuchVolume = 2662;    //There is no volume whose entry path matches the input Entry Path
  NERR_DfsVolumeAlreadyExists = 2663;    //A volume with the given name already exists
  NERR_DfsAlreadyShared = 2664;    //The server share specified is already shared in the Dfs
  NERR_DfsNoSuchShare = 2665;    //The indicated server share does not support the indicated Dfs volume
  NERR_DfsNotALeafVolume = 2666;    //The operation is not valid on a non-leaf volume
  NERR_DfsLeafVolume = 2667;    //The operation is not valid on a leaf volume
  NERR_DfsVolumeHasMultipleServers = 2668;    //The operation is ambiguous because the volume has multiple servers
  NERR_DfsCantCreateJunctionPoint = 2669;    //Unable to create a junction point
  NERR_DfsServerNotDfsAware = 2670;    //The server is not Dfs Aware
  NERR_DfsBadRenamePath = 2671;    //The specified rename target path is invalid
  NERR_DfsVolumeIsOffline = 2672;    //The specified Dfs volume is offline
  NERR_DfsNoSuchServer = 2673;    //The specified server is not a server for this volume
  NERR_DfsCyclicalName = 2674;    //A cycle in the Dfs name was detected
  NERR_DfsNotSupportedInServerDfs = 2675;    //The operation is not supported on a server-based Dfs
  NERR_DfsInternalError = 2690;    //Dfs internal error
  NERR_SetupAlreadyJoined = 2691;    // This machine is already joined to a domain.
  NERR_SetupNotJoined = 2692;    // This machine is not currently joined to a domain.
  NERR_SetupDomainController = 2693;    // This machine is a domain controller and cannot be unjoined from a domain.
  NERR_InvalidWeekDays    = 2000;
  NERR_InvalidMonthDays   = 2001;
  // The destination domain controller does not support
  // creating machine accounts in OUs.
  NERR_DefaultJoinRequired = (NERR_BASE + 594) ;
  // The specified workgroup name is invalid.
  NERR_InvalidWorkgroupName = (NERR_BASE + 595) ;
  // The specified computer name is incompatible with the
  // default language used on the domain controller.
  NERR_NameUsesIncompatibleCodePage = (NERR_BASE + 596) ;
  // The specified computer account could not be found.
  // Contact an administrator to verify the account is in the domain.
  // If the account has been deleted unjoin, reboot, and rejoin the domain.
  NERR_ComputerAccountNotFound = (NERR_BASE + 597) ;
  // This version of Windows cannot be joined to a domain.
  NERR_PersonalSku = (NERR_BASE + 598) ;
  // An attempt to resolve the DNS name of a DC in the domain being joined has failed.
  // Please verify this client is configured to reach a DNS server that can
  // resolve DNS names in the target domain.
  NERR_SetupCheckDNSConfig = (NERR_BASE + 599) ;

  SV_TYPE_WORKSTATION       = $00000001;
  SV_TYPE_SERVER            = $00000002;
  SV_TYPE_SQLSERVER         = $00000004;
  SV_TYPE_DOMAIN_CTRL       = $00000008;
  SV_TYPE_DOMAIN_BAKCTRL    = $00000010;
  SV_TYPE_TIME_SOURCE       = $00000020;
  SV_TYPE_AFP               = $00000040;
  SV_TYPE_NOVELL            = $00000080;
  SV_TYPE_DOMAIN_MEMBER     = $00000100;
  SV_TYPE_PRINTQ_SERVER     = $00000200;
  SV_TYPE_DIALIN_SERVER     = $00000400;
  SV_TYPE_XENIX_SERVER      = $00000800;
  SV_TYPE_NT                = $00001000;
  SV_TYPE_WFW               = $00002000;
  SV_TYPE_SERVER_MFPN       = $00004000;
  SV_TYPE_SERVER_NT         = $00008000;
  SV_TYPE_POTENTIAL_BROWSER = $00010000;
  SV_TYPE_BACKUP_BROWSER    = $00020000;
  SV_TYPE_MASTER_BROWSER    = $00040000;
  SV_TYPE_DOMAIN_MASTER     = $00080000;
  SV_TYPE_SERVER_OSF        = $00100000;
  SV_TYPE_SERVER_VMS        = $00200000;
  SV_TYPE_WINDOWS           = $00400000; // Windows95 and above
  SV_TYPE_DFS               = $00800000; // Root of a DFS tree
  SV_TYPE_CLUSTER_NT        = $01000000; // NT Cluster
  SV_TYPE_TERMINALSERVER    = $02000000; // Terminal Server(Hydra)
  SV_TYPE_CLUSTER_VS_NT     = $04000000; // NT Cluster Virtual Server Name
  SV_TYPE_DCE               = $10000000; // IBM DSS (Directory and Security Services) or equivalent
  SV_TYPE_ALTERNATE_XPORT   = $20000000; // return list for alternate transport
  SV_TYPE_LOCAL_LIST_ONLY   = $40000000; // Return local list only
  SV_TYPE_DOMAIN_ENUM       = DWORD($80000000);
  SV_TYPE_ALL               = DWORD($FFFFFFFF); // handy for NetServerEnum2
  SV_TYPE_SERVER_UNIX       = SV_TYPE_XENIX_SERVER;

  FILTER_TEMP_DUPLICATE_ACCOUNT     =  $0001;
  FILTER_NORMAL_ACCOUNT             =  $0002;
  FILTER_PROXY_ACCOUNT              =  $0004;
  FILTER_INTERDOMAIN_TRUST_ACCOUNT  =  $0008;
  FILTER_WORKSTATION_TRUST_ACCOUNT  =  $0010;
  FILTER_SERVER_TRUST_ACCOUNT       =  $0020;

  LM20_NNLEN = 12;
  SHPWLEN = 8;

  STYPE_DISKTREE = 0;
  STYPE_PRINTQ   = 1;
  STYPE_DEVICE   = 2;
  STYPE_IPC      = 3;

  STYPE_SPECIAL  = $80000000;
  STYPE_TEMPORARY = $40000000;

  ACCESS_NONE   = $00;
  ACCESS_READ   = $01;
  ACCESS_WRITE  = $02;
  ACCESS_CREATE = $04;
  ACCESS_EXEC   = $08;
  ACCESS_DELETE = $10;
  ACCESS_ATRIB  = $20;
  ACCESS_PERM   = $40;
  ACCESS_ALL    = ACCESS_READ or ACCESS_WRITE or
                  ACCESS_CREATE or ACCESS_EXEC or
                  ACCESS_DELETE or ACCESS_ATRIB or
                  ACCESS_PERM;

  LG_INCLUDE_INDIRECT = $0001;

  SESS_GUEST        = $00000001;
  SESS_NOENCRYPTION = $00000002;

  JOB_RUN_PERIODICALLY   = $01;    //  set if EVERY
  JOB_EXEC_ERROR         = $02;    //  set if error
  JOB_RUNS_TODAY         = $04;    //  set if today
  JOB_ADD_CURRENT_DATE   = $08;    // set if to add current date
  JOB_NONINTERACTIVE     = $10;    // set for noninteractive
  JOB_INPUT_FLAGS        = JOB_RUN_PERIODICALLY or JOB_ADD_CURRENT_DATE or JOB_NONINTERACTIVE;
  JOB_OUTPUT_FLAGS       = JOB_RUN_PERIODICALLY or JOB_EXEC_ERROR or JOB_RUNS_TODAY or JOB_NONINTERACTIVE;
  MAX_PREFERRED_LENGTH   = -1;

  UF_SCRIPT              = $0001;
  UF_ACCOUNTDISABLE      = $0002;
  UF_HOMEDIR_REQUIRED    = $0008;
  UF_LOCKOUT             = $0010;
  UF_PASSWD_NOTREQD      = $0020;
  UF_PASSWD_CANT_CHANGE  = $0040;

  UF_TEMP_DUPLICATE_ACCOUNT      = $0100;
  UF_NORMAL_ACCOUNT              = $0200;
  UF_INTERDOMAIN_TRUST_ACCOUNT   = $0800;
  UF_WORKSTATION_TRUST_ACCOUNT   = $1000;
  UF_SERVER_TRUST_ACCOUNT        = $2000;

  UF_MACHINE_ACCOUNT_MASK = UF_INTERDOMAIN_TRUST_ACCOUNT or
                            UF_WORKSTATION_TRUST_ACCOUNT or
                            UF_SERVER_TRUST_ACCOUNT;

  UF_ACCOUNT_TYPE_MASK = UF_TEMP_DUPLICATE_ACCOUNT or
                         UF_NORMAL_ACCOUNT or
                         UF_INTERDOMAIN_TRUST_ACCOUNT or
                         UF_WORKSTATION_TRUST_ACCOUNT or
                         UF_SERVER_TRUST_ACCOUNT;

  UF_DONT_EXPIRE_PASSWD = $10000;
  UF_MNS_LOGON_ACCOUNT  = $20000;


  UF_SETTABLE_BITS = UF_SCRIPT or
                     UF_ACCOUNTDISABLE or
                     UF_LOCKOUT or
                     UF_HOMEDIR_REQUIRED or
                     UF_PASSWD_NOTREQD or
                     UF_PASSWD_CANT_CHANGE or
                     UF_ACCOUNT_TYPE_MASK or
                     UF_DONT_EXPIRE_PASSWD or
                     UF_MNS_LOGON_ACCOUNT;

  USER_PRIV_MASK  = $3;
  USER_PRIV_GUEST = 0;
  USER_PRIV_USER  = 1;
  USER_PRIV_ADMIN = 2;

  PLATFORM_ID_DOS = 300;
  PLATFORM_ID_OS2 = 400;
  PLATFORM_ID_NT  = 500;
  PLATFORM_ID_OSF = 600;
  PLATFORM_ID_VMS = 700;

  USE_LOCAL_PARMNUM      = 1;
  USE_REMOTE_PARMNUM     = 2;
  USE_PASSWORD_PARMNUM   = 3;
  USE_ASGTYPE_PARMNUM    = 4;
  USE_USERNAME_PARMNUM   = 5;
  USE_DOMAINNAME_PARMNUM = 6;

  USE_OK       = 0;
  USE_PAUSED   = 1;
  USE_SESSLOST = 2;
  USE_DISCONN  = 2;
  USE_NETERR   = 3;
  USE_CONN     = 4;
  USE_RECONN   = 5;

  USE_WILDCARD = DWORD(-1);
  USE_DISKDEV  = 0;
  USE_SPOOLDEV = 1;
  USE_CHARDEV  = 2;
  USE_IPC      = 3;

  USE_NOFORCE            = 0;
  USE_FORCE              = 1;
  USE_LOTS_OF_FORCE      = 2;

  TIMEQ_FOREVER             = ULONG(-1);
  USER_MAXSTORAGE_UNLIMITED = ULONG(-1);
  USER_NO_LOGOFF            = ULONG(-1);
  UNITS_PER_DAY             = 24;
  UNITS_PER_WEEK            = UNITS_PER_DAY * 7;

type
  NET_API_STATUS = Cardinal;

  _NET_DISPLAY_USER = record
    usri1_name: LPWSTR;
    usri1_comment: LPWSTR;
    usri1_flags: Cardinal;
    usri1_full_name: LPWSTR;
    usri1_user_id: Cardinal;
    usri1_next_Index: Integer;
  end;

  PNET_DISPLAY_USER = ^NET_DISPLAY_USER;
  NET_DISPLAY_USER = _NET_DISPLAY_USER;

  _NET_DISPLAY_MACHINE = record
    usri2_name: LPWSTR;
    usri2_comment: LPWSTR;
    usri2_flags: Cardinal;
    usri2_user_id: Cardinal;
    usri2_next_Index: Integer;
  end;

  PNET_DISPLAY_MACHINE = ^NET_DISPLAY_MACHINE;
  NET_DISPLAY_MACHINE = _NET_DISPLAY_MACHINE;

  _NET_DISPLAY_GROUP = record
    grpi3_name: LPWSTR;
    grpi3_comment: LPWSTR;
    grpi3_group_id: Cardinal;
    grpi3_attributes: Cardinal;
    grpi3_next_Index: Integer;
  end;

  PNET_DISPLAY_GROUP = ^NET_DISPLAY_GROUP;
  NET_DISPLAY_GROUP = _NET_DISPLAY_GROUP;

  _STAT_WORKSTATION_0 = record
    stw0_start: Cardinal;
    stw0_numNCB_r: Cardinal;
    stw0_numNCB_s: Cardinal;
    stw0_numNCB_a: Cardinal;
    stw0_fiNCB_r: Cardinal;
    stw0_fiNCB_s: Cardinal;
    stw0_fiNCB_a: Cardinal;
    stw0_fcNCB_r: Cardinal;
    stw0_fcNCB_s: Cardinal;
    stw0_fcNCB_a: Cardinal;
    stw0_sesstart: Cardinal;
    stw0_sessfailcon: Cardinal;
    stw0_sessbroke: Cardinal;
    stw0_uses: Cardinal;
    stw0_usefail: Cardinal;
    stw0_autorec: Cardinal;
    stw0_bytessent_r_lo: Cardinal;
    stw0_bytessent_r_hi: Cardinal;
    stw0_bytesrcvd_r_lo: Cardinal;
    stw0_bytesrcvd_r_hi: Cardinal;
    stw0_bytessent_s_lo: Cardinal;
    stw0_bytessent_s_hi: Cardinal;
    stw0_bytesrcvd_s_lo: Cardinal;
    stw0_bytesrcvd_s_hi: Cardinal;
    stw0_bytessent_a_lo: Cardinal;
    stw0_bytessent_a_hi: Cardinal;
    stw0_bytesrcvd_a_lo: Cardinal;
    stw0_bytesrcvd_a_hi: Cardinal;
    stw0_reqbufneed: Cardinal;
    stw0_bigbufneed: Cardinal;
  end;

  PSTAT_WORKSTATION_0 = ^STAT_WORKSTATION_0;
  STAT_WORKSTATION_0 = _STAT_WORKSTATION_0;

  _STAT_SERVER_0 = record
    sts0_start: Cardinal;
    sts0_fopens: Cardinal;
    sts0_devopens: Cardinal;
    sts0_jobsqueued: Cardinal;
    sts0_sopens: Cardinal;
    sts0_stimedout: Cardinal;
    sts0_serrorout: Cardinal;
    sts0_pwerrors: Cardinal;
    sts0_permerrors: Cardinal;
    sts0_syserrors: Cardinal;
    sts0_bytessent_low: Cardinal;
    sts0_bytessent_high: Cardinal;
    sts0_bytesrcvd_low: Cardinal;
    sts0_bytesrcvd_high: Cardinal;
    sts0_avresponse: Cardinal;
    sts0_reqbufneed: Cardinal;
    sts0_bigbufneed: Cardinal;
  end;

  PSTAT_SERVER_0 = ^STAT_SERVER_0;
  STAT_SERVER_0 = _STAT_SERVER_0;

  _SERVER_TRANSPORT_INFO_1 = record
    svti1_numberofvcs: Cardinal;
    svti1_transportname: LPSTR;
    svti1_transportaddress: PBYTE;
    svti1_transportaddresslength: Cardinal;
    svti1_networkaddress: LPSTR;
    svti1_domain: LPSTR;
  end;

  PSERVER_TRANSPORT_INFO_1 = ^SERVER_TRANSPORT_INFO_1;
  SERVER_TRANSPORT_INFO_1 = _SERVER_TRANSPORT_INFO_1;

  _WKSTA_TRANSPORT_INFO_0 = record
    wkti0_quality_of_service: Cardinal;
    wkti0_number_of_vcs: Cardinal;
    wkti0_transport_name: LPWSTR;
    wkti0_transport_address: LPWSTR;
    wkti0_wan_ish: BOOL;
  end;

  PWKSTA_TRANSPORT_INFO_0 = ^WKSTA_TRANSPORT_INFO_0;
  WKSTA_TRANSPORT_INFO_0 = _WKSTA_TRANSPORT_INFO_0;

  _WKSTA_USER_INFO_0 = record
      wkui0_username :LPWSTR;
  end;

  PWKSTA_USER_INFO_0 = ^WKSTA_USER_INFO_0;
  WKSTA_USER_INFO_0 = ^_WKSTA_USER_INFO_0;

  _WKSTA_USER_INFO_1 = record
    wkui1_username: LPWSTR;
    wkui1_logon_domain: LPWSTR;
    wkui1_oth_domains: LPWSTR;
    wkui1_logon_server: LPWSTR;
  end;
  WKSTA_USER_INFO_1 = _WKSTA_USER_INFO_1;
  PWKSTA_USER_INFO_1 = ^_WKSTA_USER_INFO_1;
  LPWKSTA_USER_INFO_1 = ^_WKSTA_USER_INFO_1;


  _WKSTA_INFO_100 = record
     wksi100_platform_id : Cardinal;
     wksi100_computername : LPWSTR;
     wksi100_langroup : LPWSTR;
     wksi100_ver_major : Cardinal;
     wksi100_ver_minor : Cardinal;
  end;

  PWKSTA_INFO_100 = ^WKSTA_INFO_100;
  WKSTA_INFO_100 = _WKSTA_INFO_100;

  PWKSTA_INFO_102 = ^WKSTA_INFO_102;
  _WKSTA_INFO_102 = record
    wksi102_platform_id: DWORD;
    wksi102_computername: LPWSTR;
    wksi102_langroup: LPWSTR;
    wksi102_ver_major: DWORD;
    wksi102_ver_minor: DWORD;
    wksi102_lanroot: LPWSTR;
    wksi102_logged_on_users: DWORD;
  end;
  WKSTA_INFO_102 = _WKSTA_INFO_102;
  TWkstaInfo102 = WKSTA_INFO_102;
  PWkstaInfo102 = PWKSTA_INFO_102;

  _USER_INFO_1 = record
   usri1_name: PWideChar;
   usri1_password: PWideChar;
   usri1_password_age: Dword;
   usri1_priv: DWord;
   usri1_home_dir: PWideChar;
   usri1_comment: PWideChar;
   usri1_flags: DWord;
   usri1_script_path: PWideChar;
  end;
  PUSER_INFO_1 = ^USER_INFO_1;
  USER_INFO_1 = _USER_INFO_1;

  PUSER_INFO_10 = ^USER_INFO_10;
  _USER_INFO_10 = record
    usri10_name: LPWSTR;
    usri10_comment: LPWSTR;
    usri10_usr_comment: LPWSTR;
    usri10_full_name: LPWSTR;
  end;
  USER_INFO_10 = _USER_INFO_10;
  TUserInfo10 = USER_INFO_10;
  PUserInfo10 = PUSER_INFO_10;

  _USER_INFO_1008 = record
   usri1008_flags: DWord;
  end;
  PUSER_INFO_1008 = ^USER_INFO_1008;
  USER_INFO_1008 = _USER_INFO_1008;

  _USER_INFO_11 = record
    usri11_name: LPWSTR;
    usri11_comment: LPWSTR;
    usri11_usr_comment: LPWSTR;
    usri11_full_name: LPWSTR;
    usri11_priv: Cardinal;
    usri11_auth_flags: Cardinal;
    usri11_password_age: Cardinal;
    usri11_home_dir: LPWSTR;
    usri11_parms: LPWSTR;
    usri11_last_logon: Cardinal;
    usri11_last_logoff: Cardinal;
    usri11_bad_pw_count: Cardinal;
    usri11_num_logons: Cardinal;
    usri11_logon_server: LPWSTR;
    usri11_country_code: Cardinal;
    usri11_workstations: LPWSTR;
    usri11_max_storage: Cardinal;
    usri11_units_per_week: Cardinal;
    usri11_logon_hours: PBYTE;
    usri11_code_page: Cardinal;
  end;

  PUSER_INFO_3 = ^USER_INFO_3;
  USER_INFO_3 = record
    usri3_name: LPWSTR;
    usri3_password: LPWSTR;
    usri3_password_age: DWORD;
    usri3_priv: DWORD;
    usri3_home_dir: LPWSTR;
    usri3_comment: LPWSTR;
    usri3_flags: DWORD;
    usri3_script_path: LPWSTR;
    usri3_auth_flags: DWORD;
    usri3_full_name: LPWSTR;
    usri3_usr_comment: LPWSTR;
    usri3_parms: LPWSTR;
    usri3_workstations: LPWSTR;
    usri3_last_logon: DWORD;
    usri3_last_logoff: DWORD;
    usri3_acct_expires: DWORD;
    usri3_max_storage: DWORD;
    usri3_units_per_week: DWORD;
    usri3_logon_hours: PBYTE;
    usri3_bad_pw_count: DWORD;
    usri3_num_logons: DWORD;
    usri3_logon_server: LPWSTR;
    usri3_country_code: DWORD;
    usri3_code_page: DWORD;
    usri3_user_id: DWORD;
    usri3_primary_group_id: DWORD;
    usri3_profile: LPWSTR;
    usri3_home_dir_drive: LPWSTR;
    usri3_password_expired: DWORD;
  end;
  TUserInfo3 = USER_INFO_3;
  PUserInfo3 = PUSER_INFO_3;

  PUSER_INFO_4 = ^USER_INFO_4;
  USER_INFO_4 = record
    usri4_name: LPWSTR;
    usri4_password: LPWSTR;
    usri4_password_age: DWORD;
    usri4_priv: DWORD;
    usri4_home_dir: LPWSTR;
    usri4_comment: LPWSTR;
    usri4_flags: DWORD;
    usri4_script_path: LPWSTR;
    usri4_auth_flags: DWORD;
    usri4_full_name: LPWSTR;
    usri4_usr_comment: LPWSTR;
    usri4_parms: LPWSTR;
    usri4_workstations: LPWSTR;
    usri4_last_logon: DWORD;
    usri4_last_logoff: DWORD;
    usri4_acct_expires: DWORD;
    usri4_max_storage: DWORD;
    usri4_units_per_week: DWORD;
    usri4_logon_hours: PBYTE;
    usri4_bad_pw_count: DWORD;
    usri4_num_logons: DWORD;
    usri4_logon_server: LPWSTR;
    usri4_country_code: DWORD;
    usri4_code_page: DWORD;
    usri4_user_sid: PSID;
    usri4_primary_group_id: DWORD;
    usri4_profile: LPWSTR;
    usri4_home_dir_drive: LPWSTR;
    usri4_password_expired: DWORD;
  end;
  TUserInfo4 = USER_INFO_4;
  PUserInfo4 = PUSER_INFO_4;

  PUSER_INFO_11 = ^USER_INFO_11;
  USER_INFO_11 = _USER_INFO_11;

  _GROUP_USERS_INFO_0 = record
     grui0_name: LPWSTR;
   end;

   PGROUP_USERS_INFO_0 = ^GROUP_USERS_INFO_0;
   GROUP_USERS_INFO_0 = _GROUP_USERS_INFO_0;

  _GROUP_INFO_2 = record
    grpi2_name: LPWSTR;
    grpi2_comment: LPWSTR;
    grpi2_group_id: Cardinal;
    grpi2_attributes: Cardinal;
  end;

  PGROUP_INFO_2 = ^GROUP_INFO_2;
  GROUP_INFO_2 = _GROUP_INFO_2;

  _LOCALGROUP_INFO_0 = record
    lgrpi1_name: LPWSTR;
  end;

  PLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;
  LOCALGROUP_INFO_0 = _LOCALGROUP_INFO_0;

  _LOCALGROUP_INFO_1 = record
    lgrpi1_name: LPWSTR;
    lgrpi1_comment: LPWSTR;
  end;

  PLOCALGROUP_INFO_1 = ^LOCALGROUP_INFO_1;
  LOCALGROUP_INFO_1 = _LOCALGROUP_INFO_1;

  _LOCALGROUP_USERS_INFO_0 = record
     lgrui0_name: LPWSTR;
  end;

  PLOCALGROUP_USERS_INFO_0 = ^LOCALGROUP_USERS_INFO_0;
  LOCALGROUP_USERS_INFO_0 = _LOCALGROUP_USERS_INFO_0;

  _LOCALGROUP_MEMBERS_INFO_3 = record
    lgrmi3_domainandname: LPWSTR;
  end;

  PLOCALGROUP_MEMBERS_INFO_3 = ^LOCALGROUP_MEMBERS_INFO_3;
  LOCALGROUP_MEMBERS_INFO_3 = _LOCALGROUP_MEMBERS_INFO_3;

  PSHARE_INFO_0 = ^SHARE_INFO_0;
  _SHARE_INFO_0 = record
    shi0_netname: LPWSTR;
  end;
  SHARE_INFO_0 = _SHARE_INFO_0;
  TShareInfo0 = SHARE_INFO_0;
  PShareInfo0 = PSHARE_INFO_0;

  PSHARE_INFO_1 = ^SHARE_INFO_1;
  _SHARE_INFO_1 = record
    shi1_netname: LPWSTR;
    shi1_type: DWORD;
    shi1_remark: LPWSTR;
  end;
  SHARE_INFO_1 = _SHARE_INFO_1;
  TShareInfo1 = SHARE_INFO_1;
  PShareInfo1 = PSHARE_INFO_1;

  PSHARE_INFO_2 = ^SHARE_INFO_2;
  _SHARE_INFO_2 = record
    shi2_netname: LPWSTR;
    shi2_type: DWORD;
    shi2_remark: LPWSTR;
    shi2_permissions: DWORD;
    shi2_max_uses: DWORD;
    shi2_current_uses: DWORD;
    shi2_path: LPWSTR;
    shi2_passwd: LPWSTR;
  end;
  SHARE_INFO_2 = _SHARE_INFO_2;
  TShareInfo2 = SHARE_INFO_2;
  PShareInfo2 = PSHARE_INFO_2;

  _SHARE_INFO_502 = record
    shi502_netname: LPWSTR;
    shi502_type: Cardinal;
    shi502_remark: LPWSTR;
    shi502_permissions: Cardinal;
    shi502_max_uses: Cardinal;
    shi502_current_uses: Cardinal;
    shi502_path: LPWSTR;
    shi502_passwd: LPWSTR;
    shi502_reserved: Cardinal;
    shi502_security_descriptor: PSECURITY_DESCRIPTOR;
  end;

  PSHARE_INFO_502 = ^SHARE_INFO_502;
  SHARE_INFO_502 = _SHARE_INFO_502;


  _FILE_INFO_3 = record
    fi3_id: Cardinal;
    fi3_permissions: Cardinal;
    fi3_num_locks: Cardinal;
    fi3_pathname: LPWSTR;
    fi3_username: LPWSTR;
  end;

  PFILE_INFO_3 = ^FILE_INFO_3;
  FILE_INFO_3 = _FILE_INFO_3;

  _file_info_50 = record
    fi50_id: Cardinal;
    fi50_permissions: WORD;
    fi50_num_locks: WORD;
    fi50_pathname: PAnsiChar;
    fi50_username: PAnsiChar;
    fi50_sharename: PAnsiChar;
  end;

  pfile_info_50 = ^file_info_50;
  file_info_50 = _file_info_50;

  _SESSION_INFO_502 = record
    sesi502_cname: LPWSTR;
    sesi502_username: LPWSTR;
    sesi502_num_opens: Cardinal;
    sesi502_time: Cardinal;
    sesi502_idle_time: Cardinal;
    sesi502_user_flags: Cardinal;
    sesi502_cltype_name: LPWSTR;
    sesi502_transport: LPWSTR;
  end;

  PSESSION_INFO_502 = ^SESSION_INFO_502;
  SESSION_INFO_502 = _SESSION_INFO_502;

  _CONNECTION_INFO_1 = record
    coni1_id: Cardinal;
    coni1_type: Cardinal;
    coni1_num_opens: Cardinal;
    coni1_num_users: Cardinal;
    coni1_time: Cardinal;
    coni1_username: LPWSTR;
    coni1_netname: LPWSTR;
  end;

  PCONNECTION_INFO_1 = ^CONNECTION_INFO_1;
  CONNECTION_INFO_1 = _CONNECTION_INFO_1;

  _share_info_50 = record
    shi50_netname: array [0..LM20_NNLEN] of char;
    shi50_type: BYTE;
    shi50_flags: WORD;
    shi50_remark: PAnsiChar;
    shi50_path: PAnsiChar;
    shi50_rw_password: array[0..SHPWLEN] of char;
    shi50_ro_password: array[0..SHPWLEN] of char;
  end;

  pshare_info_50 = ^share_info_50;
  share_info_50 = _share_info_50;

  _session_info_50 = record
    sesi50_cname: PAnsiChar;
    sesi50_username: PAnsiChar;
    sesi50_key: Cardinal;
    sesi50_num_conns: WORD;
    sesi50_num_opens: WORD;
    sesi50_time: Cardinal;
    sesi50_idle_time: Cardinal;
    sesi50_protocol: PAnsiChar;
    pad1: PAnsiChar;
  end;

  psession_info_50 = ^session_info_50;
  session_info_50 = _session_info_50;

  PSESSION_INFO_1 = ^SESSION_INFO_1;
  _SESSION_INFO_1 = record
    sesi1_cname: LPWSTR; 
    sesi1_username: LPWSTR;
    sesi1_num_opens: DWORD;
    sesi1_time: DWORD;
    sesi1_idle_time: DWORD;
    sesi1_user_flags: DWORD;
  end;
  SESSION_INFO_1 = _SESSION_INFO_1;
  TSessionInfo1 = SESSION_INFO_1;
  PSessionInfo1 = PSESSION_INFO_1;

  PSESSION_INFO_10 = ^SESSION_INFO_10;
  _SESSION_INFO_10 = record
    sesi10_cname: LPWSTR;
    sesi10_username: LPWSTR;
    sesi10_time: DWORD;
    sesi10_idle_time: DWORD;
  end;
  SESSION_INFO_10 = _SESSION_INFO_10;
  TSessionInfo10 = SESSION_INFO_10;
  PSessionInfo10 = PSESSION_INFO_10;

  _connection_info_50 = record
    coni50_type: Word;
    coni50_num_opens: Word;
    coni50_time: Cardinal;
    coni50_netname: PAnsiChar;
    coni50_username: PAnsiChar;
  end;

  pconnection_info_50 = ^connection_info_50;
  connection_info_50 = _connection_info_50;

  _AT_ENUM = record
    JobId: Cardinal;
    JobTime: Cardinal;
    DaysOfMonth: Cardinal;
    DaysOfWeek: byte;
    flags: byte;
    Command: LPWSTR;
  end;

  PAT_ENUM = ^AT_ENUM;
  AT_ENUM = _AT_ENUM;

  _AT_INFO = record
    JobTime: Cardinal;
    DaysOfMonth: Cardinal;
    DaysOfWeek: byte;
    flags: byte;
    Command: LPWSTR;
  end;

  PAT_INFO = ^AT_INFO;
  AT_INFO = _AT_INFO;

  _USE_INFO_0 = record
    ui0_local: LPWSTR;
    ui0_remote: LPWSTR;
  end;
  USE_INFO_0 = _USE_INFO_0;
  PUSE_INFO_0 = ^USE_INFO_0;
  LPUSE_INFO_0 = ^USE_INFO_0;
  TUseInfo0 = USE_INFO_0;
  PUseInfo0 = PUSE_INFO_0;

  _USE_INFO_1 = record
    ui1_local: LPWSTR;
    ui1_remote: LPWSTR;
    ui1_password: LPWSTR;
    ui1_status: DWORD;
    ui1_asg_type: DWORD;
    ui1_refcount: DWORD;
    ui1_usecount: DWORD;
  end;
  USE_INFO_1 = _USE_INFO_1;
  PUSE_INFO_1 = ^USE_INFO_1;
  LPUSE_INFO_1 = ^USE_INFO_1;
  TUseInfo1 = USE_INFO_1;
  PUseInfo1 = PUSE_INFO_1;

  _USE_INFO_2 = record
    ui2_local: LPWSTR;
    ui2_remote: LPWSTR;
    ui2_password: LPWSTR;
    ui2_status: DWORD;
    ui2_asg_type: DWORD;
    ui2_refcount: DWORD;
    ui2_usecount: DWORD;
    ui2_username: LPWSTR;
    ui2_domainname: LPWSTR;
  end;
  USE_INFO_2 = _USE_INFO_2;
  PUSE_INFO_2 = ^USE_INFO_2;
  LPUSE_INFO_2 = ^USE_INFO_2;
  TUseInfo2 = USE_INFO_2;
  PUseInfo2 = PUSE_INFO_2;

  _USE_INFO_3 = record
    ui3_ui2: USE_INFO_2;
    ui3_flags: ULONG;
  end;
  USE_INFO_3 = _USE_INFO_3;
  PUSE_INFO_3 = ^USE_INFO_3;
  LPUSE_INFO_3 = ^USE_INFO_3;
  TUseInfo3 = USE_INFO_3;
  PUseInfo3 = PUSE_INFO_3;

  PSERVER_INFO_100 = ^SERVER_INFO_100;
  _SERVER_INFO_100 = record
    sv100_platform_id: DWORD;
    sv100_name: LPWSTR;
  end;
  SERVER_INFO_100 = _SERVER_INFO_100;
  TServerInfo100 = SERVER_INFO_100;
  PServerInfo100 = PSERVER_INFO_100;

  PSERVER_INFO_101 = ^SERVER_INFO_101;
  _SERVER_INFO_101 = record
    sv101_platform_id: DWORD;
    sv101_name: LPWSTR;
    sv101_version_major: DWORD;
    sv101_version_minor: DWORD;
    sv101_type: DWORD;
    sv101_comment: LPWSTR;
  end;
  SERVER_INFO_101 = _SERVER_INFO_101;
  TServerInfo101 = SERVER_INFO_101;
  PServerInfo101 = PSERVER_INFO_101;

  PSERVER_INFO_102 = ^SERVER_INFO_102;
  _SERVER_INFO_102 = record
    sv102_platform_id: DWORD;
    sv102_name: LPWSTR;
    sv102_version_major: DWORD;
    sv102_version_minor: DWORD;
    sv102_type: DWORD;
    sv102_comment: LPWSTR;
    sv102_users: DWORD;
    sv102_disc: Longint;
    sv102_hidden: BOOL;
    sv102_announce: DWORD;
    sv102_anndelta: DWORD;
    sv102_licenses: DWORD;
    sv102_userpath: LPWSTR;
  end;
  SERVER_INFO_102 = _SERVER_INFO_102;
  TServerInfo102 = SERVER_INFO_102;
  PServerInfo102 = PSERVER_INFO_102;

  _TIME_OF_DAY_INFO = record
    tod_elapsedt: DWORD;
    tod_msecs: DWORD;
    tod_hours: DWORD;
    tod_mins: DWORD;
    tod_secs: DWORD;
    tod_hunds: DWORD;
    tod_timezone: Longint;
    tod_tinterval: DWORD;
    tod_day: DWORD;
    tod_month: DWORD;
    tod_year: DWORD;
    tod_weekday: DWORD;
  end;
  TIME_OF_DAY_INFO = _TIME_OF_DAY_INFO;
  PTIME_OF_DAY_INFO = ^TIME_OF_DAY_INFO;
  TTimeOfDayInfo = TIME_OF_DAY_INFO;
  PTimeOfDayInfo = PTIME_OF_DAY_INFO;

  PDOMAIN_CONTROLLER_INFO = ^DOMAIN_CONTROLLER_INFO;
  _DOMAIN_CONTROLLER_INFO = record
    DomainControllerName: LPSTR;
    DomainControllerAddress: LPSTR;
    DomainControllerAddressType: ULONG;
    DomainGuid: TGUID;
    DomainName: LPSTR;
    DnsForestName: LPSTR;
    Flags: ULONG;
    DcSiteName: LPSTR;
    ClientSiteName: LPSTR;
  end;
  DOMAIN_CONTROLLER_INFO = _DOMAIN_CONTROLLER_INFO;
  TDomainControllerInfo = DOMAIN_CONTROLLER_INFO;
  PDomainControllerInfo = PDOMAIN_CONTROLLER_INFO;

  _NETSETUP_JOIN_STATUS = (
    NetSetupUnknownStatus,
    NetSetupUnjoined,
    NetSetupWorkgroupName,
    NetSetupDomainName
 );

  NETSETUP_JOIN_STATUS = _NETSETUP_JOIN_STATUS;
  PNETSETUP_JOIN_STATUS = ^_NETSETUP_JOIN_STATUS;
  TNetSetupJoinStatus = NETSETUP_JOIN_STATUS;
  PNetSetupJoinStatus = PNETSETUP_JOIN_STATUS;

function InitNETAPI: Boolean;
procedure FreeNETAPI;

type
  TNetQueryDisplayInformation = function(ServerName: LPCWSTR; Level: Cardinal;
                                    Index: Integer; EntriesRequested: Cardinal;
                                    PrefMaxLen: Cardinal;
                                    var ReturnedEntryCount: Cardinal;
                                    var SortedBuffer: Pointer): NET_API_STATUS; stdcall;

  TNetStatisticsGet = function (ServerName: LPWSTR; Service: LPWSTR; Level: Cardinal;
                          Options: Cardinal; var BufPtr: Pointer): NET_API_STATUS; stdcall;

  TNetWkstaGetInfo = function(ServerName: LPWSTR; Level: Cardinal;
                         var Bufptr: Pointer): NET_API_STATUS; stdcall;

  TNetWkstaUserEnum = function(ServerName: LPCWSTR;
                          Level: Cardinal;
                          BufPtr: Pointer;
                          PrefMaxLen: Cardinal;
                          EntriesRead: PDWORD;
                          TotalEntries: PDWORD;
                          ResumeHandle: PDWORD): NET_API_STATUS; stdcall;

  TNetWkstaUserGetInfo = function(Reserved: LPWSTR; Level: Cardinal;
                             var BufPtr: Pointer): NET_API_STATUS; stdcall;

  TNetWkstaTransportEnum = function(ServerName: LPWSTR; Level: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal;
                          var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetServerTransportEnum = function(ServerName: LPWSTR; Level: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal;
                          var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetUserEnum = function(ServerName: LPWSTR; Level: Cardinal; Filter: Cardinal;
                     var Bufptr: Pointer; PrefMaxLen: Cardinal;
                     var EntriesRead: Cardinal;
                     var TotalEntries: Cardinal;
                     var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetUserAdd = function (servername: PWideChar; level: DWORD; buf: Pointer; var parm_err: DWORD): Dword; stdcall;

  TNetUserDel = function (servername, username: PWideChar): Dword; stdcall;

  TNetUserGetInfo = function (ServerName: LPWSTR; UserName: LPWSTR;  Level: Cardinal;
                        var Bufptr: Pointer): NET_API_STATUS; stdcall;
  TNetUserSetInfo = function(servername, username: LPCWSTR; level: DWORD; buf: PBYTE; parm_err: LPDWORD): NET_API_STATUS; stdcall;

  TNetUserGetGroups = function(ServerName: LPWSTR; UserName: LPCWSTR; Level: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal): NET_API_STATUS; stdcall;

  TNetUserGetLocalGroups = function(ServerName: LPWSTR; UserName: LPCWSTR;
                          Level: Cardinal; Flags: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal): NET_API_STATUS; stdcall;

  TNetGroupEnum = function(ServerName: LPWSTR; Level: Cardinal;
                           var BufPtr: Pointer; PrefMaxLen: Cardinal;
                           var EntriesRead: Cardinal;
                           var TotalEntries: Cardinal;
                           var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetLocalGroupEnum = function(ServerName: LPWSTR; Level: Cardinal;
                           var BufPtr: Pointer; PrefMaxLen: Cardinal;
                           var EntriesRead: Cardinal;
                           var TotalEntries: Cardinal;
                           var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetGroupGetUsers = function(ServerName: LPWSTR; GroupName: LPCWSTR; Level: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal;
                          var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetLocalGroupGetMembers = function(ServerName: LPWSTR; GroupName: LPCWSTR; Level: Cardinal;
                          var BufPtr: Pointer; PrefMaxLen: Cardinal;
                          var EntriesRead: Cardinal;
                          var TotalEntries: Cardinal;
                          var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetLocalGroupAddMembers = function(Server, GroupName: PWideChar; Level:Cardinal;
                                      var MemsBuf; TotalEntries: Cardinal): Integer; stdcall;

  TNetServerEnum = function (ServerName: LPWSTR; Level: Cardinal;
                       var Bufptr: Pointer; PrefMaxLen: Cardinal;
                       var EntriesRead: Cardinal;
                       var TotalEntries: Cardinal;  ServerType: Cardinal;  Domain: LPWSTR;
                       var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetShareEnum = function(ServerName: LPWSTR; Level: Cardinal;
                      var BufPtr: Pointer; PrefMaxLength: Cardinal;
                      var EntriesRead: Cardinal;
                      var TotalEntries: Cardinal;
                      var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetShareDel = function(ServerName: LPWSTR; NetName: LPWSTR; Reserved: Cardinal): NET_API_STATUS; stdcall;

  TNetSessionEnum = function(ServerName: LPWSTR; UncClientName: LPWSTR; UserName: LPWSTR;
                      Level: Cardinal;
                      var BufPtr: Pointer; PrefMaxLength: Cardinal;
                      var EntriesRead: Cardinal;
                      var TotalEntries: Cardinal;
                      var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetSessionDel = function(ServerName: LPWSTR; UncClientName: LPWSTR; Username: LPWSTR): NET_API_STATUS; stdcall;

  TNetConnectionEnum = function(ServerName: LPWSTR; Qualifier: LPWSTR;
                      Level: Cardinal;
                      var BufPtr: Pointer; PrefMaxLength: Cardinal;
                      var EntriesRead: Cardinal;
                      var TotalEntries: Cardinal;
                      var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetFileEnum = function(ServerName: LPWSTR; BasePath: LPWSTR; UserName: LPWSTR;
                      Level: Cardinal;
                      var BufPtr: Pointer; PrefMaxLength: Cardinal;
                      var EntriesRead: Cardinal;
                      var TotalEntries: Cardinal;
                      var ResumeHandle: Cardinal): NET_API_STATUS; stdcall;

  TNetFileClose = function(ServerName: LPWSTR; FileID: Cardinal): NET_API_STATUS; stdcall;

  TNetScheduleJobAdd = function(ServerName : LPWSTR;
                           Buffer : pointer;
                           var JobID : Cardinal) : NET_API_STATUS; stdcall;
  TNetScheduleJobDel = function(ServerName : LPWSTR;
                           MinJobID,
                           MaxJobID : Cardinal) : NET_API_STATUS; stdcall;
  TNetScheduleJobEnum = function(ServerName : LPWSTR;
                             var buffer : pointer;
                             PrefMaximumLength : Cardinal;
                             var EntriesRead :Cardinal;
                             var TotalEntries :Cardinal;
                             var ResumeHandle : Cardinal) : NET_API_STATUS; stdcall;
  TNetScheduleJobGetInfo = function(ServerName : LPWSTR;
                               JobId : Cardinal;
                               var buffer : pointer) : NET_API_STATUS; stdcall;

  TNetApiBufferFree = function (buffer: Pointer): NET_API_STATUS; stdcall;

  TNetApiBufferReallocate = function(OldBuffer : Pointer;
                                NewByteCount : Cardinal;
                                var NewBuffer : Pointer) : NET_API_STATUS; stdcall;
  TNetApiBufferSize = function(buffer : Pointer;
                          var byteCount : Cardinal) : NET_API_STATUS; stdcall;

  TNetUseAdd = function(UncServerName: LPWSTR; Level: DWORD; Buf: Pointer; ParmError: LPDWORD): NET_API_STATUS; stdcall;

  TNetUseDel = function(UncServerName: LPWSTR; UseName: LPWSTR; ForceCond: DWORD): NET_API_STATUS; stdcall;

  TNetUseEnum = function(UncServerName: LPWSTR; Level: DWORD; var BufPtr: Pointer; PreferedMaximumSize: DWORD; EntriesRead: LPDWORD; TotalEntries: LPDWORD; ResumeHandle: LPDWORD): NET_API_STATUS; stdcall;

  TNetUseGetInfo = function(UncServerName: LPWSTR; UseName: LPWSTR; Level: DWORD; var BufPtr: Pointer): NET_API_STATUS; stdcall;

  TNetServerGetInfo = function(servername: LPWSTR; level: DWORD; var bufptr: Pointer): NET_API_STATUS stdcall;

  TNetGetDCName = function (servername, domainname: LPWSTR; var bufptr: Pointer): NET_API_STATUS; stdcall;

  TNetGetAnyDCName = function(servername, domainname: LPWSTR; var bufptr: Pointer): NET_API_STATUS; stdcall;

  TNetRemoteTOD = function(UncServerName: LPWSTR; var BufferPtr: Pointer): NET_API_STATUS; stdcall;

  TNetShareGetInfo = function(servername, netname: LPWSTR; level: DWORD; var bufptr: Pointer): NET_API_STATUS; stdcall;

  TNetShareCheck = function(servername, device: LPWSTR; typ: LPDWORD): NET_API_STATUS; stdcall;

  TNetLocalGroupDelMembers = function(servername, groupname: LPWSTR; level: DWORD; var buf; totalentries: DWORD): NET_API_STATUS; stdcall;

  TDsGetDcName = function(ComputerName, DomainName: LPCTSTR; DomainGuid: TGUID;
    SiteName: LPCTSTR; Flags: ULONG; var DomainControllerInfo: PDOMAIN_CONTROLLER_INFO): DWORD; stdcall;

  TDsGetSiteName = function(ComputerName: LPCTSTR; var SiteName: LPTSTR): DWORD; stdcall;

  TDsGetDcOpen = function(DnsName: PAnsiChar; OptionFlags: ULONG; SiteName: PAnsiChar; DomainGuid: PGUID; DnsForestName: PAnsiChar; DcFlags: ULONG; RetGetDcContext: PHandle): DWORD; stdcall;
  TDsGetDcNext = function(GetDcContextHandle: THandle; SocketAddresses: PULONG; SockAddresses: Pointer; DnsHostName: PAnsiChar): DWORD; stdcall;
  TDsGetDcClose = procedure(hGetDc: THandle); stdcall;

  TNetRenameMachineInDomain = function(lpServer, MachineName, lpAccount, Password: PWideChar; Options: Longint): LongInt stdcall;

  TNetGetJoinInformation = function(lpServer: PWideChar; out lpNameBuffer: PWideChar; BufferType: TNetSetupJoinStatus): NET_API_STATUS; stdcall;

var
  NetQueryDisplayInformation: TNetQueryDisplayInformation = nil;
  NetStatisticsGet: TNetStatisticsGet = nil;
  NetWkstaGetInfo: TNetWkstaGetInfo = nil;
  NetWkstaUserEnum: TNetWkstaUserEnum = nil;
  NetWkstaUserGetInfo: TNetWkstaUserGetInfo = nil;
  NetServerTransportEnum: TNetServerTransportEnum = nil;
  NetWkstaTransportEnum: TNetWkstaTransportEnum = nil;
  NetServerEnum: TNetServerEnum = nil;
  NetUserEnum: TNetUserEnum = nil;
  NetUserGetInfo: TNetUserGetInfo = nil;
  NetUserSetInfo: TNetUserSetInfo = nil;
  NetUserAdd: TNetUserAdd = nil;
  NetUserDel: TNetUserDel = nil;
  NetUserGetGroups: TNetUserGetGroups = nil;
  NetUserGetLocalGroups: TNetUserGetLocalGroups = nil;
  NetGroupEnum: TNetGroupEnum = nil;
  NetLocalGroupEnum: TNetLocalGroupEnum = nil;
  NetGroupGetUsers: TNetGroupGetUsers = nil;
  NetLocalGroupGetMembers: TNetLocalGroupGetMembers = nil;
  NetLocalGroupAddMembers: TNetLocalGroupAddMembers = nil;
  NetLocalGroupDelMembers: TNetLocalGroupDelMembers = nil;
  NetShareEnum: TNetShareEnum = nil;
  NetShareDel: TNetShareDel = nil;
  NetSessionEnum: TNetSessionEnum = nil;
  NetSessionDel: TNetSessionDel = nil;
  NetConnectionEnum: TNetConnectionEnum = nil;
  NetFileEnum: TNetFileEnum = nil;
  NetFileClose: TNetFileClose = nil;
  NetApiBufferFree: TNetApiBufferFree = nil;
  NetApiBufferReallocate: TNetApiBufferReallocate = nil;
  NetApiBufferSize: TNetApiBufferSize = nil;
  NetScheduleJobEnum: TNetScheduleJobEnum = nil;
  NetScheduleJobDel: TNetScheduleJobDel = nil;
  NetScheduleJobAdd: TNetScheduleJobAdd = nil;
  NetScheduleJobGetInfo: TNetScheduleJobGetInfo = nil;
  NetUseAdd: TNetUseAdd = nil;
  NetUseDel: TNetUseDel = nil;
  NetUseEnum: TNetUseEnum = nil;
  NetUseGetInfo: TNetUseGetInfo = nil;
  NetServerGetInfo: TNetServerGetInfo = nil;
  NetGetDCName: TNetGetDCName = nil;
  NetGetAnyDCName: TNetGetAnyDCName = nil;
  NetRemoteTOD: TNetRemoteTOD = nil;
  NetShareGetInfo: TNetShareGetInfo = nil;
  NetShareCheck: TNetShareCheck = nil;
  DsGetDcName: TDsGetDcName = nil;
  DsGetSiteName: TDsGetSiteName = nil;
  DsGetDcOpen: TDsGetDcOpen = nil;
  DsGetDcNext: TDsGetDcNext = nil;
  DsGetDcClose: TDsGetDcClose = nil;
  NetRenameMachineInDomain: TNetRenameMachineInDomain = nil;
  NetGetJoinInformation: TNetGetJoinInformation = nil;

function GetServerTypeFlagString(sv101_type: Cardinal): string;

implementation

const
  NETAPI_DLL = 'netapi32.dll';

var
  NETAPIHandle: THandle;
  UnloadNETAPI: Boolean;

function GetServerTypeFlagString(sv101_type: Cardinal): string;
begin
  Result:='';
  if sv101_type and SV_TYPE_WORKSTATION<>0 then Result:=Result+'WORKSTATION,';
  if sv101_type and SV_TYPE_SERVER<>0 then Result:=Result+'SERVER,';
  if sv101_type and SV_TYPE_SQLSERVER<>0 then Result:=Result+'SQLSERVER,';
  if sv101_type and SV_TYPE_DOMAIN_CTRL<>0 then Result:=Result+'DOMAIN_CTRL,';
  if sv101_type and SV_TYPE_DOMAIN_BAKCTRL<>0 then Result:=Result+'DOMAIN_BAKCTRL,';
  if sv101_type and SV_TYPE_TIME_SOURCE<>0 then Result:=Result+'TIME_SOURCE,';
  if sv101_type and SV_TYPE_AFP<>0 then Result:=Result+'AFP,';
  if sv101_type and SV_TYPE_NOVELL<>0 then Result:=Result+'NOVELL,';
  if sv101_type and SV_TYPE_DOMAIN_MEMBER<>0 then Result:=Result+'DOMAIN_MEMBER,';
  //if sv101_type and SV_TYPE_LOCAL_LIST_ONLY<>0 then Result:=Result+'Servers maintained by the browser,';
  if sv101_type and SV_TYPE_PRINTQ_SERVER<>0 then Result:=Result+'PRINTQ_SERVER,';
  if sv101_type and SV_TYPE_DIALIN_SERVER<>0 then Result:=Result+'DIALIN_SERVER,';
  if sv101_type and SV_TYPE_XENIX_SERVER<>0 then Result:=Result+'XENIX_SERVER,';
  if sv101_type and SV_TYPE_SERVER_MFPN<>0 then Result:=Result+'SERVER_MFPN,';
  if sv101_type and SV_TYPE_NT<>0 then Result:=Result+'NT,';
  if sv101_type and SV_TYPE_WFW<>0 then Result:=Result+'WFW,';
  if sv101_type and SV_TYPE_SERVER_MFPN<>0 then Result:=Result+'SERVER_MFPN,';
  if sv101_type and SV_TYPE_SERVER_NT<>0 then Result:=Result+'SERVER_NT,';
  if sv101_type and SV_TYPE_POTENTIAL_BROWSER<>0 then Result:=Result+'POTENTIAL_BROWSER,';
  if sv101_type and SV_TYPE_BACKUP_BROWSER<>0 then Result:=Result+'BACKUP_BROWSER,';
  if sv101_type and SV_TYPE_MASTER_BROWSER<>0 then Result:=Result+'MASTER_BROWSER,';
  if sv101_type and SV_TYPE_DOMAIN_MASTER<>0 then Result:=Result+'DOMAIN_MASTER,';
  if sv101_type and SV_TYPE_SERVER_OSF<>0 then Result:=Result+'SERVER_OSF,';
  if sv101_type and SV_TYPE_SERVER_VMS<>0 then Result:=Result+'SERVER_VMS,';
  if sv101_type and SV_TYPE_WINDOWS<>0 then Result:=Result+'WINDOWS,';
  if sv101_type and SV_TYPE_DFS<>0 then Result:=Result+'DFS,';
  if sv101_type and SV_TYPE_CLUSTER_NT<>0 then Result:=Result+'CLUSTER_NT,';
  if sv101_type and SV_TYPE_TERMINALSERVER<>0 then Result:=Result+'TERMINALSERVER,';
  if sv101_type and SV_TYPE_CLUSTER_VS_NT<>0 then Result:=Result+'CLUSTER_VS_NT,';
  if sv101_type and SV_TYPE_DCE<>0 then Result:=Result+'DCE,';
  if sv101_type and SV_TYPE_ALTERNATE_XPORT<>0 then Result:=Result+'ALTERNATE_XPORT,';
  if sv101_type and SV_TYPE_LOCAL_LIST_ONLY<>0 then Result:=Result+'LOCAL_LIST_ONLY,';
  if sv101_type and SV_TYPE_DOMAIN_ENUM<>0 then Result:=Result+'DOMAIN_ENUM,';
  SetLength(Result,Length(Result)-1);
end;

function InitNETAPI: Boolean;
begin
  NETAPIHandle:=GetModuleHandle(NETAPI_DLL);
  UnloadNetAPI:=NetAPIHandle=0;
  if NETAPIHandle=0 then
    NETAPIHandle:=loadlibrary(NETAPI_DLL);
  if NETAPIHandle<>0 then begin
    NetQueryDisplayInformation:=TNetQueryDisplayInformation(GetProcAddress(NETAPIHandle,PAnsiChar('NetQueryDisplayInformation')));
    NetStatisticsGet:=TNetStatisticsGet(GetProcAddress(NETAPIHandle,PAnsiChar('NetStatisticsGet')));
    NetWkstaGetInfo:=TNetWkstaGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetWkstaGetInfo')));
    NetWkstaUserGetInfo:=TNetWkstaUserGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetWkstaUserGetInfo')));
    NetUserEnum:=TNetUserEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserEnum')));
    NetWkstaUserEnum:=TNetWkstaUserEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetWkstaUserEnum')));
    NetWkstaTransportEnum:=TNetWkstaTransportEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetWkstaTransportEnum')));
    NetServerTransportEnum:=TNetServerTransportEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetServerTransportEnum')));
    NetUserGetInfo:=TNetUserGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserGetInfo')));
    NetUserSetInfo:=TNetUserSetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserSetInfo')));
    NetUserAdd:=TNetUserAdd(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserAdd')));
    NetUserDel:=TNetUserDel(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserDel')));
    NetUserGetGroups:=TNetUserGetGroups(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserGetGroups')));
    NetUserGetLocalGroups:=TNetUserGetLocalGroups(GetProcAddress(NETAPIHandle,PAnsiChar('NetUserGetLocalGroups')));
    NetGroupEnum:=TNetGroupEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetGroupEnum')));
    NetLocalGroupEnum:=TNetLocalGroupEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetLocalGroupEnum')));
    NetGroupGetUsers:=TNetGroupGetUsers(GetProcAddress(NETAPIHandle,PAnsiChar('NetGroupGetUsers')));
    NetLocalGroupGetMembers:=TNetLocalGroupGetMembers(GetProcAddress(NETAPIHandle,PAnsiChar('NetLocalGroupGetMembers')));
    NetLocalGroupAddMembers:=TNetLocalGroupAddMembers(GetProcAddress(NETAPIHandle,PAnsiChar('NetLocalGroupAddMembers')));
    NetLocalGroupDelMembers:=TNetLocalGroupDelMembers(GetProcAddress(NETAPIHandle,PAnsiChar('NetLocalGroupDelMembers')));
    NetServerEnum:=TNetServerEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetServerEnum')));
    NetShareEnum:=TNetShareEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetShareEnum')));
    NetShareDel:=TNetShareDel(GetProcAddress(NETAPIHandle,PAnsiChar('NetShareDel')));
    NetSessionEnum:=TNetSessionEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetSessionEnum')));
    NetSessionDel:=TNetSessionDel(GetProcAddress(NETAPIHandle,PAnsiChar('NetSessionDel')));
    NetConnectionEnum:=TNetConnectionEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetConnectionEnum')));
    NetFileEnum:=TNetFileEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetFileEnum')));
    NetFileClose:=TNetFileClose(GetProcAddress(NETAPIHandle,PAnsiChar('NetFileClose')));
    NetApiBufferFree:=TNetApiBufferFree(GetProcAddress(NETAPIHandle,PAnsiChar('NetApiBufferFree')));
    NetApiBufferReallocate:=TNetApiBufferReallocate(GetProcAddress(NETAPIHandle,PAnsiChar('NetApiBufferReallocate')));
    NetApiBufferSize:=TNetApiBufferSize(GetProcAddress(NETAPIHandle,PAnsiChar('NetApiBufferSize')));
    NetScheduleJobEnum:=TNetScheduleJobEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetScheduleJobEnum')));
    NetScheduleJobDel:=TNetScheduleJobDel(GetProcAddress(NETAPIHandle,PAnsiChar('NetScheduleJobDel')));
    NetScheduleJobAdd:=TNetScheduleJobAdd(GetProcAddress(NETAPIHandle,PAnsiChar('NetScheduleJobAdd')));
    NetScheduleJobGetInfo:=TNetScheduleJobGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetScheduleJobGetInfo')));
    NetUseAdd:=TNetUseAdd(GetProcAddress(NETAPIHandle,PAnsiChar('NetUseAdd')));
    NetUseDel:=TNetUseDel(GetProcAddress(NETAPIHandle,PAnsiChar('NetUseDel')));
    NetUseEnum:=TNetUseEnum(GetProcAddress(NETAPIHandle,PAnsiChar('NetUseEnum')));
    NetUseGetInfo:=TNetUseGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetUseGetInfo')));
    NetServerGetInfo:=TNetServerGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetServerGetInfo')));
    NetGetDCName:=TNetGetDCName(GetProcAddress(NETAPIHandle,PAnsiChar('NetGetDCName')));
    NetGetAnyDCName:=TNetGetAnyDCName(GetProcAddress(NETAPIHandle,PAnsiChar('NetGetAnyDCName')));
    NetRemoteTOD:=TNetRemoteTOD(GetProcAddress(NETAPIHandle,PAnsiChar('NetRemoteTOD')));
    NetShareGetInfo:=TNetShareGetInfo(GetProcAddress(NETAPIHandle,PAnsiChar('NetShareGetInfo')));
    NetShareCheck:=TNetShareCheck(GetProcAddress(NETAPIHandle,PAnsiChar('NetShareCheck')));
    DsGetDcName:=TDsGetDcName(GetProcAddress(NETAPIHandle,PAnsiChar('DsGetDcNameA')));
    DsGetSiteName:=TDsGetSiteName(GetProcAddress(NETAPIHandle,PAnsiChar('DsGetSiteNameA')));
    DsGetDcOpen:=TDsGetDcOpen(GetProcAddress(NETAPIHandle,PAnsiChar('DsGetDcOpenA')));
    DsGetDcNext:=TDsGetDcNext(GetProcAddress(NETAPIHandle,PAnsiChar('DsGetDcNextA')));
    DsGetDcClose:=TDsGetDcClose(GetProcAddress(NETAPIHandle,PAnsiChar('DsGetDcCloseW')));
    NetRenameMachineInDomain:=TNetRenameMachineInDomain(GetProcAddress(NETAPIHandle,PAnsiChar('NetRenameMachineInDomain')));
    NetGetJoinInformation:=TNetGetJoinInformation(GetProcAddress(NETAPIHandle,PAnsiChar('NetGetJoinInformation')));
  end;
  result:=(NETAPIHandle<>0) and Assigned(NetWkstaGetInfo);
end;

procedure FreeNETAPI;
begin
  if (NetAPIHandle<>0) and UnloadNetAPI then begin
    if not FreeLibrary(NetAPIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[NETAPI_DLL,GetModuleHandle(NETAPI_DLL)]))
    else
      NetAPIHandle:=0;
  end;
end;

initialization
  InitNetAPI;
finalization
  FreeNetAPI;
end.
