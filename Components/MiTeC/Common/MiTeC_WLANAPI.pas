{*******************************************************}
{                                                       }
{               MiTeC Common Routines                   }
{            Windows WLAN API interface                 }
{                                                       }
{         Copyright (c) 2013-2018 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}
{$DEFINE DYNAMIC_LINK}

unit MiTeC_WLANAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils
     {$ELSE}
     Windows, SysUtils
     {$ENDIF}
     ;

type
  PVOID = Pointer;
  PPVOID = ^PVOID;
  PPByte = ^PByte;

const
  DELETE       	= $00010000;
  READ_CONTROL 	= $00020000;

  STANDARD_RIGHTS_READ		= (READ_CONTROL);
  STANDARD_RIGHTS_WRITE		= (READ_CONTROL);
  STANDARD_RIGHTS_EXECUTE	= (READ_CONTROL);

  FILE_READ_DATA					= $0001; // file & pipe
  FILE_EXECUTE						= $0020;

  FILE_WRITE_DATA					= $0002;
  WRITE_DAC								= $00040000;

	DOT11_SSID_MAX_LENGTH = 32; //32 Bytes

type
  {$MINENUMSIZE 4}
	PDOT11_BSS_TYPE = ^TDOT11_BSS_TYPE;
	TDOT11_BSS_TYPE = (
    dot11_BSS_type_infrastructure = 1,
    dot11_BSS_type_independent = 2,
    dot11_BSS_type_any = 3);

  PDOT11_SSID = ^TDOT11_SSID;
  TDOT11_SSID = record
  	uSSIDLength: ulong;
    ucSSID: array[0..DOT11_SSID_MAX_LENGTH - 1] of uchar;
  end;

  {$MINENUMSIZE 4}
  PDOT11_AUTH_ALGORITHM = ^TDOT11_AUTH_ALGORITHM;
  TDOT11_AUTH_ALGORITHM = Cardinal;
const
	DOT11_AUTH_ALGO_80211_OPEN = 1;
  DOT11_AUTH_ALGO_80211_SHARED_KEY = 2;
  DOT11_AUTH_ALGO_WPA = 3;
  DOT11_AUTH_ALGO_WPA_PSK = 4;
  DOT11_AUTH_ALGO_WPA_NONE = 5;
  DOT11_AUTH_ALGO_RSNA = 6;
  DOT11_AUTH_ALGO_RSNA_PSK = 7;
  DOT11_AUTH_ALGO_IHV_START = $80000000;
  DOT11_AUTH_ALGO_IHV_END = $ffffffff;

type
	{$MINENUMSIZE 4}
	PDOT11_CIPHER_ALGORITHM = ^TDOT11_CIPHER_ALGORITHM;
	TDOT11_CIPHER_ALGORITHM = Cardinal;
const
  DOT11_CIPHER_ALGO_NONE = $00;
  DOT11_CIPHER_ALGO_WEP40 = $01;
  DOT11_CIPHER_ALGO_TKIP = $02;
  DOT11_CIPHER_ALGO_CCMP = $04;
  DOT11_CIPHER_ALGO_WEP104 = $05;
  DOT11_CIPHER_ALGO_WPA_USE_GROUP = $100;
  DOT11_CIPHER_ALGO_RSN_USE_GROUP = $100;
  DOT11_CIPHER_ALGO_WEP = $101;
  DOT11_CIPHER_ALGO_IHV_START = $80000000;
  DOT11_CIPHER_ALGO_IHV_END = $ffffffff;

type
  PDOT11_AUTH_CIPHER_PAIR = ^TDOT11_AUTH_CIPHER_PAIR;
  TDOT11_AUTH_CIPHER_PAIR = record
  	AuthAlgoId: TDOT11_AUTH_ALGORITHM;
    CipherAlgoId: TDOT11_CIPHER_ALGORITHM;
  end;

const
	L2_PROFILE_MAX_NAME_LENGTH 											= 256;

  L2_NOTIFICATION_SOURCE_NONE 										= 0;
  L2_NOTIFICATION_SOURCE_DOT3_AUTO_CONFIG        	= $00000001;
  L2_NOTIFICATION_SOURCE_SECURITY									= $00000002;
  L2_NOTIFICATION_SOURCE_ONEX											= $00000004;
  L2_NOTIFICATION_SOURCE_WLAN_ACM									= $00000008;
  L2_NOTIFICATION_SOURCE_WLAN_MSM									= $00000010;
  L2_NOTIFICATION_SOURCE_WLAN_SECURITY						= $00000020;
  L2_NOTIFICATION_SOURCE_WLAN_IHV									= $00000040;

  L2_NOTIFICATION_SOURCE_ALL											= $0000FFFF;
  L2_NOTIFICATION_CODE_PUBLIC_BEGIN								= $00000000;


  L2_REASON_CODE_GROUP_SIZE												= $10000;
  L2_REASON_CODE_GEN_BASE													= $10000;

  L2_REASON_CODE_DOT11_AC_BASE										=
  	(L2_REASON_CODE_GEN_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_DOT11_MSM_BASE										=
  	(L2_REASON_CODE_DOT11_AC_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_DOT11_SECURITY_BASE							=
  	(L2_REASON_CODE_DOT11_MSM_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_ONEX_BASE 												=
  	(L2_REASON_CODE_DOT11_SECURITY_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_DOT3_AC_BASE											=
  	(L2_REASON_CODE_ONEX_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_DOT3_MSM_BASE										=
  	(L2_REASON_CODE_DOT3_AC_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_PROFILE_BASE											=
  	(L2_REASON_CODE_DOT3_MSM_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_IHV_BASE													=
  	(L2_REASON_CODE_PROFILE_BASE + L2_REASON_CODE_GROUP_SIZE);

  L2_REASON_CODE_SUCCESS													= 0;

  L2_REASON_CODE_UNKNOWN													=
  	(L2_REASON_CODE_GEN_BASE + 1);

  L2_REASON_CODE_PROFILE_MISSING									= $00000001;

type
	PL2_NOTIFICATION_DATA = ^TL2_NOTIFICATION_DATA;
	TL2_NOTIFICATION_DATA = record
  	NotificationSource: DWORD;
    NotificationCode: DWORD;
    InterfaceGuid: TGUID;
    dwDataSize: DWORD;
    pData: PVOID;
  end;

const
	WLAN_API_VERSION 						= 1;
  WLAN_MAX_NAME_LENGTH 				= L2_PROFILE_MAX_NAME_LENGTH;

  //Profile Flags
  WLAN_PROFILE_GROUP_POLICY		= $00000001;
  WLAN_PROFILE_USER						= $00000002;

  WLAN_SET_EAPHOST_DATA_ALL_USERS = $00000001;

  WLAN_MAX_PHY_TYPE_NUMBER	= 8;

type
	PWLAN_PROFILE_INFO = ^TWLAN_PROFILE_INFO;
	TWLAN_PROFILE_INFO = record
  	strProfileName: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    dwFlags: DWORD;
  end;

  PDOT11_NETWORK = ^TDOT11_NETWORK;
  TDOT11_NETWORK = record
  	dot11Ssid: TDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE;
  end;

const
	DOT11_PSD_IE_MAX_DATA_SIZE 			= 220;		// 255 - 6 - 2 - FORMAT ID
  DOT11_PSD_IE_MAX_ENTRY_NUMBER		= 10;

type
	PWLAN_RAW_DATA = ^TWLAN_RAW_DATA;
	TWLAN_RAW_DATA = record
  	dwDataSize: DWORD;
    DataBlob: array[0..0] of Byte;
  end;

  PWLAN_RAW_DATA_LIST = ^TWLAN_RAW_DATA_LIST;
  PPWLAN_RAW_DATA_LIST = ^PWLAN_RAW_DATA_LIST;
  TWLAN_RAW_DATA_LIST = record
  	dwTotalSize: DWORD;
    dwNumberOfItems: DWORD;
    case Integer of
    	0: (dwDataOffset: DWORD);
      1: (dwDataSize: DWORD);
  end;

  {$MINENUMSIZE 4}
  PWLAN_CONNECTION_MODE = ^TWLAN_CONNECTION_MODE;
  TWLAN_CONNECTION_MODE = (
  	wlan_connection_mode_profile = 0,
    wlan_connection_mode_temporary_profile,
    wlan_connection_mode_discovery_secure,
    wlan_connection_mode_discovery_unsecure,
    wlan_connection_mode_auto,
    wlan_connection_mode_invalid);

  TWLAN_REASON_CODE = DWORD;
  PWLAN_REASON_CODE = ^TWLAN_REASON_CODE;

const
	WLAN_REASON_CODE_SUCCESS 						= L2_REASON_CODE_SUCCESS;
  WLAN_REASON_CODE_UNKNOWN						= L2_REASON_CODE_UNKNOWN;

  WLAN_REASON_CODE_RANGE_SIZE					= L2_REASON_CODE_GROUP_SIZE;
  WLAN_REASON_CODE_BASE								= L2_REASON_CODE_DOT11_AC_BASE;

  WLAN_REASON_CODE_AC_BASE						= L2_REASON_CODE_DOT11_AC_BASE;

  WLAN_REASON_CODE_AC_CONNECT_BASE		=
  	(WLAN_REASON_CODE_AC_BASE + WLAN_REASON_CODE_RANGE_SIZE div 2);

  WLAN_REASON_CODE_AC_END							=
  	(WLAN_REASON_CODE_AC_BASE + WLAN_REASON_CODE_RANGE_SIZE - 1);


  WLAN_REASON_CODE_PROFILE_BASE				= L2_REASON_CODE_PROFILE_BASE;

  WLAN_REASON_CODE_PROFILE_CONNECT_BASE =
  	(WLAN_REASON_CODE_PROFILE_BASE + WLAN_REASON_CODE_RANGE_SIZE div 2);

  WLAN_REASON_CODE_PROFILE_END				=
  	(WLAN_REASON_CODE_PROFILE_BASE + WLAN_REASON_CODE_RANGE_SIZE - 1);

  // range for MSM
	//
  WLAN_REASON_CODE_MSM_BASE 					= L2_REASON_CODE_DOT11_MSM_BASE;

  WLAN_REASON_CODE_MSM_CONNECT_BASE 	=
  	(WLAN_REASON_CODE_MSM_BASE + WLAN_REASON_CODE_RANGE_SIZE div 2);

  WLAN_REASON_CODE_MSM_END						=
  	(WLAN_REASON_CODE_MSM_BASE + WLAN_REASON_CODE_RANGE_SIZE - 1);

  // range for MSMSEC
	//
  WLAN_REASON_CODE_MSMSEC_BASE				=
  	L2_REASON_CODE_DOT11_SECURITY_BASE;

  WLAN_REASON_CODE_MSMSEC_CONNECT_BASE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + WLAN_REASON_CODE_RANGE_SIZE div 2);

  WLAN_REASON_CODE_MSMSEC_END					=
  	(WLAN_REASON_CODE_MSMSEC_BASE + WLAN_REASON_CODE_RANGE_SIZE - 1);

  // AC network incompatible reason codes
	//
  WLAN_REASON_CODE_NETWORK_NOT_COMPATIBLE =
  	(WLAN_REASON_CODE_AC_BASE + 1);
  WLAN_REASON_CODE_PROFILE_NOT_COMPATIBLE =
  	(WLAN_REASON_CODE_AC_BASE + 2);

  // AC connect reason code
	//
  WLAN_REASON_CODE_NO_AUTO_CONNECTION	=
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 1);
  WLAN_REASON_CODE_NOT_VISIBLE =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 2);
  WLAN_REASON_CODE_GP_DENIED =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 3);
  WLAN_REASON_CODE_USER_DENIED =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 4);
  WLAN_REASON_CODE_BSS_TYPE_NOT_ALLOWED =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 5);
  WLAN_REASON_CODE_IN_FAILED_LIST =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 6);
  WLAN_REASON_CODE_IN_BLOCKED_LIST =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 7);
  WLAN_REASON_CODE_SSID_LIST_TOO_LONG =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 8);
  WLAN_REASON_CODE_CONNECT_CALL_FAIL =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 9);
  WLAN_REASON_CODE_SCAN_CALL_FAIL =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 10);
  WLAN_REASON_CODE_NETWORK_NOT_AVAILABLE =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 11);
  WLAN_REASON_CODE_PROFILE_CHANGED_OR_DELETED =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 12);
  WLAN_REASON_CODE_KEY_MISMATCH =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 13);
  WLAN_REASON_CODE_USER_NOT_RESPOND =
  	(WLAN_REASON_CODE_AC_CONNECT_BASE + 14);

  // Profile validation errors
	//
  WLAN_REASON_CODE_INVALID_PROFILE_SCHEMA =
    (WLAN_REASON_CODE_PROFILE_BASE + 1);
  WLAN_REASON_CODE_PROFILE_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 2);
  WLAN_REASON_CODE_INVALID_PROFILE_NAME =
  	(WLAN_REASON_CODE_PROFILE_BASE + 3);
  WLAN_REASON_CODE_INVALID_PROFILE_TYPE =
    (WLAN_REASON_CODE_PROFILE_BASE + 4);
  WLAN_REASON_CODE_INVALID_PHY_TYPE =
  	(WLAN_REASON_CODE_PROFILE_BASE + 5);
  WLAN_REASON_CODE_MSM_SECURITY_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 6);
  WLAN_REASON_CODE_IHV_SECURITY_NOT_SUPPORTED =
  	(WLAN_REASON_CODE_PROFILE_BASE + 7);
  WLAN_REASON_CODE_IHV_OUI_MISMATCH =
  	(WLAN_REASON_CODE_PROFILE_BASE + 8);
  WLAN_REASON_CODE_IHV_OUI_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 9);
  WLAN_REASON_CODE_IHV_SETTINGS_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 10);
  WLAN_REASON_CODE_CONFLICT_SECURITY =
  	(WLAN_REASON_CODE_PROFILE_BASE + 11);
  WLAN_REASON_CODE_SECURITY_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 12);
  WLAN_REASON_CODE_INVALID_BSS_TYPE =
  	(WLAN_REASON_CODE_PROFILE_BASE + 13);
  WLAN_REASON_CODE_INVALID_ADHOC_CONNECTION_MODE =
  	(WLAN_REASON_CODE_PROFILE_BASE + 14);
  WLAN_REASON_CODE_NON_BROADCAST_SET_FOR_ADHOC =
  	(WLAN_REASON_CODE_PROFILE_BASE + 15);
  WLAN_REASON_CODE_AUTO_SWITCH_SET_FOR_ADHOC =
  	(WLAN_REASON_CODE_PROFILE_BASE + 16);
  WLAN_REASON_CODE_AUTO_SWITCH_SET_FOR_MANUAL_CONNECTION =
  	(WLAN_REASON_CODE_PROFILE_BASE + 17);
  WLAN_REASON_CODE_IHV_SECURITY_ONEX_MISSING =
  	(WLAN_REASON_CODE_PROFILE_BASE + 18);
  WLAN_REASON_CODE_PROFILE_SSID_INVALID =
  	(WLAN_REASON_CODE_PROFILE_BASE + 19);

  // MSM network incompatible reasons
	//
  WLAN_REASON_CODE_UNSUPPORTED_SECURITY_SET_BY_OS =
  	(WLAN_REASON_CODE_MSM_BASE + 1);
  WLAN_REASON_CODE_UNSUPPORTED_SECURITY_SET =
  	(WLAN_REASON_CODE_MSM_BASE + 2);
  WLAN_REASON_CODE_BSS_TYPE_UNMATCH =
  	(WLAN_REASON_CODE_MSM_BASE + 3);
  WLAN_REASON_CODE_PHY_TYPE_UNMATCH =
  	(WLAN_REASON_CODE_MSM_BASE + 4);
  WLAN_REASON_CODE_DATARATE_UNMATCH =
  	(WLAN_REASON_CODE_MSM_BASE + 5);

  // MSM connection failure reasons, to be defined
	// failure reason codes
	//
  WLAN_REASON_CODE_USER_CANCELLED =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 1);
  WLAN_REASON_CODE_ASSOCIATION_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 2);
  WLAN_REASON_CODE_ASSOCIATION_TIMEOUT =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 3);
  WLAN_REASON_CODE_PRE_SECURITY_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 4);
  WLAN_REASON_CODE_START_SECURITY_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 5);
  WLAN_REASON_CODE_SECURITY_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 6);
  WLAN_REASON_CODE_SECURITY_TIMEOUT =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 7);
  WLAN_REASON_CODE_ROAMING_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 8);
  WLAN_REASON_CODE_ROAMING_SECURITY_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 9);
  WLAN_REASON_CODE_ADHOC_SECURITY_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 10);
  WLAN_REASON_CODE_DRIVER_DISCONNECTED =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 11);
  WLAN_REASON_CODE_DRIVER_OPERATION_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 12);
  WLAN_REASON_CODE_IHV_NOT_AVAILABLE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 13);
  WLAN_REASON_CODE_IHV_NOT_RESPONDING =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 14);
  WLAN_REASON_CODE_DISCONNECT_TIMEOUT =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 15);
  WLAN_REASON_CODE_INTERNAL_FAILURE =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 16);
  WLAN_REASON_CODE_UI_REQUEST_TIMEOUT =
  	(WLAN_REASON_CODE_MSM_CONNECT_BASE + 17);

  // MSMSEC reason codes
	//
  WLAN_REASON_CODE_MSMSEC_MIN = WLAN_REASON_CODE_MSMSEC_BASE;
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_KEY_INDEX =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 1);
  WLAN_REASON_CODE_MSMSEC_PROFILE_PSK_PRESENT =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 2);
  WLAN_REASON_CODE_MSMSEC_PROFILE_KEY_LENGTH =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 3);
  WLAN_REASON_CODE_MSMSEC_PROFILE_PSK_LENGTH =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 4);
  WLAN_REASON_CODE_MSMSEC_PROFILE_NO_AUTH_CIPHER_SPECIFIED =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 5);
  WLAN_REASON_CODE_MSMSEC_PROFILE_TOO_MANY_AUTH_CIPHER_SPECIFIED =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 6);
  WLAN_REASON_CODE_MSMSEC_PROFILE_DUPLICATE_AUTH_CIPHER =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 7);
  WLAN_REASON_CODE_MSMSEC_PROFILE_RAWDATA_INVALID =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 8);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_AUTH_CIPHER =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 9);
  WLAN_REASON_CODE_MSMSEC_PROFILE_ONEX_DISABLED =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 10);
  WLAN_REASON_CODE_MSMSEC_PROFILE_ONEX_ENABLED =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 11);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_PMKCACHE_MODE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 12);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_PMKCACHE_SIZE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 13);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_PMKCACHE_TTL =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 14);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_PREAUTH_MODE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 15);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_PREAUTH_THROTTLE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 16);
  WLAN_REASON_CODE_MSMSEC_PROFILE_PREAUTH_ONLY_ENABLED =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 17);
  WLAN_REASON_CODE_MSMSEC_CAPABILITY_NETWORK =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 18);
  WLAN_REASON_CODE_MSMSEC_CAPABILITY_NIC =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 19);
  WLAN_REASON_CODE_MSMSEC_CAPABILITY_PROFILE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 20);
  WLAN_REASON_CODE_MSMSEC_CAPABILITY_DISCOVERY =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 21);
  WLAN_REASON_CODE_MSMSEC_PROFILE_PASSPHRASE_CHAR =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 22);
  WLAN_REASON_CODE_MSMSEC_PROFILE_KEYMATERIAL_CHAR =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 23);
  WLAN_REASON_CODE_MSMSEC_PROFILE_WRONG_KEYTYPE =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 24);
  WLAN_REASON_CODE_MSMSEC_MIXED_CELL =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 25);
  WLAN_REASON_CODE_MSMSEC_PROFILE_AUTH_TIMERS_INVALID =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 26);
  WLAN_REASON_CODE_MSMSEC_PROFILE_INVALID_GKEY_INTV =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 27);
  WLAN_REASON_CODE_MSMSEC_TRANSITION_NETWORK =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 28);
  WLAN_REASON_CODE_MSMSEC_PROFILE_KEY_UNMAPPED_CHAR =
  	(WLAN_REASON_CODE_MSMSEC_BASE + 29);


  WLAN_REASON_CODE_MSMSEC_UI_REQUEST_FAILURE =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 1);
  WLAN_REASON_CODE_MSMSEC_AUTH_START_TIMEOUT =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 2);
  WLAN_REASON_CODE_MSMSEC_AUTH_SUCCESS_TIMEOUT =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 3);
  WLAN_REASON_CODE_MSMSEC_KEY_START_TIMEOUT =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 4);
  WLAN_REASON_CODE_MSMSEC_KEY_SUCCESS_TIMEOUT =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 5);
  WLAN_REASON_CODE_MSMSEC_M3_MISSING_KEY_DATA =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 6);
  WLAN_REASON_CODE_MSMSEC_M3_MISSING_IE =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 7);
  WLAN_REASON_CODE_MSMSEC_M3_MISSING_GRP_KEY =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 8);
  WLAN_REASON_CODE_MSMSEC_PR_IE_MATCHING =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 9);
  WLAN_REASON_CODE_MSMSEC_SEC_IE_MATCHING =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 10);
  WLAN_REASON_CODE_MSMSEC_NO_PAIRWISE_KEY =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 11);
  WLAN_REASON_CODE_MSMSEC_G1_MISSING_KEY_DATA =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 12);
  WLAN_REASON_CODE_MSMSEC_G1_MISSING_GRP_KEY =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 13);
  WLAN_REASON_CODE_MSMSEC_PEER_INDICATED_INSECURE =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 14);
  WLAN_REASON_CODE_MSMSEC_NO_AUTHENTICATOR =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 15);
  WLAN_REASON_CODE_MSMSEC_NIC_FAILURE =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 16);
  WLAN_REASON_CODE_MSMSEC_CANCELLED =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 17);
  WLAN_REASON_CODE_MSMSEC_KEY_FORMAT =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 18);
  WLAN_REASON_CODE_MSMSEC_DOWNGRADE_DETECTED =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 19);
  WLAN_REASON_CODE_MSMSEC_PSK_MISMATCH_SUSPECTED =
  	(WLAN_REASON_CODE_MSMSEC_CONNECT_BASE + 20);

  WLAN_REASON_CODE_MSMSEC_MAX =	WLAN_REASON_CODE_MSMSEC_END;

type
	TWLAN_SIGNAL_QUALITY = ulong;
  PWLAN_SIGNAL_QUALITY = ^TWLAN_SIGNAL_QUALITY;

const
  WLAN_AVAILABLE_NETWORK_CONNECTED 		= $00000001;
  WLAN_AVAILABLE_NETWORK_HAS_PROFILE 	= $00000002;

  WLAN_AVAILABLE_NETWORK_INCLUDE_ALL_ADHOC_PROFILES 	= $00000001;
  WLAN_AVAILABLE_NETWORK_INCLUDE_ALL_MANUAL_HIDDEN_PROFILES	= $00000002;

type
	TDOT11_MAC_ADDRESS = array[0..5] of uchar;
  PDOT11_MAC_ADDRESS = ^TDOT11_MAC_ADDRESS;

	PNDIS_OBJECT_HEADER = ^TNDIS_OBJECT_HEADER;
  TNDIS_OBJECT_HEADER = packed record
  	aType: uchar;
    Revision: uchar;
    Size: Word;
  end;

  PDOT11_BSSID_LIST = ^TDOT11_BSSID_LIST;
  TDOT11_BSSID_LIST = record
  	//const DOT11_BSSID_LIST_REVISION_1 = 1;
    Header: TNDIS_OBJECT_HEADER;
    uNumOfEntries: ulong;
    uTotalNumOfEntries: ulong;
    BSSIDs: array[0..0] of TDOT11_MAC_ADDRESS;
  end;

  {$MINENUMSIZE 4}
  PDOT11_PHY_TYPE = ^TDOT11_PHY_TYPE;
  TDOT11_PHY_TYPE = Cardinal;
const
	DOT11_PHY_TYPE_UNKNOWN = 0;
  DOT11_PHY_TYPE_ANY = DOT11_PHY_TYPE_UNKNOWN;
  DOT11_PHY_TYPE_FHSS = 1;
  DOT11_PHY_TYPE_DSSS = 2;
  DOT11_PHY_TYPE_IRBASEBAND = 3;
  DOT11_PHY_TYPE_OFDM = 4;
  DOT11_PHY_TYPE_HRDSSS = 5;
  DOT11_PHY_TYPE_ERP = 6;
  DOT11_PHY_TYPE_HT = 7;
  DOT11_PHY_TYPE_VHT = 8;
  DOT11_PHY_TYPE_IHV_START = $80000000;
  DOT11_PHY_TYPE_IHV_END = $FFFFFFFF;

	DOT11_RATE_SET_MAX_LENGTH = 126;

type
	PDOT11_RATE_SET = ^TDOT11_RATE_SET;
	TDOT11_RATE_SET = record
  	uRateSetLength: ulong;
    ucRateSet: array[0..DOT11_RATE_SET_MAX_LENGTH - 1] of uchar;
  end;

  TDOT11_COUNTRY_OR_REGION_STRING = array[0..2] of uchar;
  PDOT11_COUNTRY_OR_REGION_STRING = ^TDOT11_COUNTRY_OR_REGION_STRING;

type
	PWLAN_RATE_SET = ^TWLAN_RATE_SET;
  TWLAN_RATE_SET = record
  	uRateSetLength: ulong;
    usRateSet: array[0..DOT11_RATE_SET_MAX_LENGTH - 1] of Word;
  end;

	PWLAN_AVAILABLE_NETWORK = ^TWLAN_AVAILABLE_NETWORK;
  TWLAN_AVAILABLE_NETWORK = record
    strProfileName: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    dot11Ssid: TDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE;
    uNumberOfBssids: ulong;
    bNetworkConnectable: Bool;
    wlanNotConnectableReason: TWLAN_REASON_CODE;
    uNumberOfPhyTypes: ulong;
    dot11PhyTypes: array[0..WLAN_MAX_PHY_TYPE_NUMBER -1] of TDOT11_PHY_TYPE;
    bMorePhyTypes: Bool;
    wlanSignalQuality: TWLAN_SIGNAL_QUALITY;
    bSecurityEnabled: Bool;
    dot11DefaultAuthAlgorithm: TDOT11_AUTH_ALGORITHM;
    dot11DefaultCipherAlgorithm: TDOT11_CIPHER_ALGORITHM;
    dwFlags: DWORD;
    dwReserved: DWORD;
  end;

  PWLAN_BSS_ENTRY = ^TWLAN_BSS_ENTRY;
  TWLAN_BSS_ENTRY = record
    dot11Ssid: TDOT11_SSID;
    uPhyId: ulong;
    dot11Bssid: TDOT11_MAC_ADDRESS;
    dot11BssType: TDOT11_BSS_TYPE;
    dot11BssPhyType: TDOT11_PHY_TYPE;
    lRssi: LongInt;
    uLinkQuality: ulong;
    bInRegDomain: Boolean;
    usBeaconPeriod: Word;
    ullTimestamp: TLargeInteger;
    ullHostTimestamp: TLargeInteger;
    usCapabilityInformation: Word;
    ulChCenterFrequency: ulong;
    wlanRateSet: TWLAN_RATE_SET;
    ulIeOffset: ulong;
    ulIeSize: ulong;
  end;

  PWLAN_BSS_LIST = ^TWLAN_BSS_LIST;
  PPWLAN_BSS_LIST	= ^PWLAN_BSS_LIST;
  TWLAN_BSS_LIST = record
    dwTotalSize: DWORD;
    dwNumberOfItems: DWORD;
    wlanBssEntries: array[0..0] of TWLAN_BSS_ENTRY;
  end;

  {$MINENUMSIZE 4}
  PWLAN_INTERFACE_STATE = ^TWLAN_INTERFACE_STATE;
  TWLAN_INTERFACE_STATE = (
  	wlan_interface_state_not_ready = 0,
    wlan_interface_state_connected,
    wlan_interface_state_ad_hoc_network_formed,
    wlan_interface_state_disconnecting,
    wlan_interface_state_disconnected,
    wlan_interface_state_associating,
    wlan_interface_state_discovering,
    wlan_interface_state_authenticating);

  PWLAN_INTERFACE_INFO = ^TWLAN_INTERFACE_INFO;
  TWLAN_INTERFACE_INFO = record
  	InterfaceGuid: TGUID;
    strInterfaceDescription: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    isState: TWLAN_INTERFACE_STATE;
  end;

  PWLAN_ASSOCIATION_ATTRIBUTES = ^TWLAN_ASSOCIATION_ATTRIBUTES;
  TWLAN_ASSOCIATION_ATTRIBUTES = record
    dot11Ssid: TDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE;
    dot11Bssid: TDOT11_MAC_ADDRESS;
    dot11PhyType: TDOT11_PHY_TYPE;
    uDot11PhyIndex: ulong;
    wlanSignalQuality: TWLAN_SIGNAL_QUALITY;
    ulRxRate: ulong;
    ulTxRate: ulong;
  end;

  PWLAN_SECURITY_ATTRIBUTES = ^TWLAN_SECURITY_ATTRIBUTES;
  TWLAN_SECURITY_ATTRIBUTES = record
  	bSecurityEnabled: Bool;
    bOneXEnabled : Bool;
    dot11AuthAlgorithm: TDOT11_AUTH_ALGORITHM;
    dot11CipherAlgorithm: TDOT11_CIPHER_ALGORITHM;
  end;


  PWLAN_CONNECTION_ATTRIBUTES = ^TWLAN_CONNECTION_ATTRIBUTES;
  TWLAN_CONNECTION_ATTRIBUTES =  record
  	isState: TWLAN_INTERFACE_STATE;
    wlanConnectionMode: TWLAN_CONNECTION_MODE;
    strProfileName: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    wlanAssociationAttributes: TWLAN_ASSOCIATION_ATTRIBUTES;
    wlanSecurityAttributes: TWLAN_SECURITY_ATTRIBUTES;
  end;

  {$MINENUMSIZE 4}
  PDOT11_RADIO_STATE = ^TDOT11_RADIO_STATE;
  TDOT11_RADIO_STATE = (
  	dot11_radio_state_unknown = 0,
    dot11_radio_state_on,
    dot11_radio_state_off);

const
	// the maximum number of PHYs supported by a NIC
	WLAN_MAX_PHY_INDEX = 63;

type
	PWLAN_PHY_RADIO_STATE = ^TWLAN_PHY_RADIO_STATE;
	TWLAN_PHY_RADIO_STATE = record
    dwPhyIndex: DWORD;
    dot11SoftwareRadioState: TDOT11_RADIO_STATE;
    dot11HardwareRadioState: TDOT11_RADIO_STATE;
  end;

  PWLAN_RADIO_STATE = ^TWLAN_RADIO_STATE;
  TWLAN_RADIO_STATE = record
  	dwNumberOfPhys: DWORD;
    PhyRadioState: array[0..WLAN_MAX_PHY_INDEX - 1] of TWLAN_PHY_RADIO_STATE;
  end;

  {$MINENUMSIZE 4}
  PWLAN_INTERFACE_TYPE = ^TWLAN_INTERFACE_TYPE;
  TWLAN_INTERFACE_TYPE = (
  	wlan_interface_type_emulated_802_11 = 0,
    wlan_interface_type_native_802_11,
    wlan_interface_type_invalid);

  PWLAN_INTERFACE_CAPABILITY = ^TWLAN_INTERFACE_CAPABILITY;
  PPWLAN_INTERFACE_CAPABILITY = ^PWLAN_INTERFACE_CAPABILITY;
  TWLAN_INTERFACE_CAPABILITY = record
  	interfaceType: TWLAN_INTERFACE_TYPE;
    bDot11DSupported: Bool;
    dwMaxDesiredSsidListSize: DWORD;
    dwMaxDesiredBssidListSize: DWORD;
    dwNumberOfSupportedPhys: DWORD;
    dot11PhyTypes: array[0..WLAN_MAX_PHY_INDEX - 1] of TDOT11_PHY_TYPE;
  end;

  PWLAN_AUTH_CIPHER_PAIR_LIST = ^TWLAN_AUTH_CIPHER_PAIR_LIST;
  TWLAN_AUTH_CIPHER_PAIR_LIST = record
  	pAuthCipherPairList: array[0..0] of TDOT11_AUTH_CIPHER_PAIR;
  end;

  PWLAN_COUNTRY_OR_REGION_STRING_LIST = ^TWLAN_COUNTRY_OR_REGION_STRING_LIST;
  TWLAN_COUNTRY_OR_REGION_STRING_LIST = record
  	pCountryOrRegionStringList: array[0..0] of TDOT11_COUNTRY_OR_REGION_STRING;
  end;

  PWLAN_PROFILE_INFO_LIST = ^TWLAN_PROFILE_INFO_LIST;
  PPWLAN_PROFILE_INFO_LIST = ^PWLAN_PROFILE_INFO_LIST;
  TWLAN_PROFILE_INFO_LIST = record
  	dwNumberOfItems: DWORD;
    dwIndex: DWORD;
    ProfileInfo: array[0..0] of TWLAN_PROFILE_INFO;
  end;

  PWLAN_AVAILABLE_NETWORK_LIST = ^TWLAN_AVAILABLE_NETWORK_LIST;
  PPWLAN_AVAILABLE_NETWORK_LIST = ^PWLAN_AVAILABLE_NETWORK_LIST;
  TWLAN_AVAILABLE_NETWORK_LIST = record
  	dwNumberOfItems: DWORD;
    dwIndex: DWORD;
    Network: array[0..0] of TWLAN_AVAILABLE_NETWORK;
  end;

  PWLAN_INTERFACE_INFO_LIST = ^TWLAN_INTERFACE_INFO_LIST;
  PPWLAN_INTERFACE_INFO_LIST = ^PWLAN_INTERFACE_INFO_LIST;
  TWLAN_INTERFACE_INFO_LIST = record
  	dwNumberOfItems: DWORD;
    dwIndex: DWORD;
    InterfaceInfo: array[0..0] of TWLAN_INTERFACE_INFO;
  end;

  PDOT11_NETWORK_LIST = ^TDOT11_NETWORK_LIST;
  PPDOT11_NETWORK_LIST = ^PDOT11_NETWORK_LIST;
  TDOT11_NETWORK_LIST = record
  	dwNumberOfItems: DWORD;
    dwIndex: DWORD;
    Network: array[0..0] of TDOT11_NETWORK;
  end;

  {$MINENUMSIZE 4}
  PWLAN_POWER_SETTING = ^TWLAN_POWER_SETTING;
  TWLAN_POWER_SETTING = (
  	wlan_power_setting_no_saving = 0,
    wlan_power_setting_low_saving,
    wlan_power_setting_medium_saving,
    wlan_power_setting_maximum_saving,
    wlan_power_setting_invalid);

const
	WLAN_CONNECTION_HIDDEN_NETWORK 		= $00000001;
  WLAN_CONNECTION_ADHOC_JOIN_ONLY 	= $00000002;

type
	PWLAN_CONNECTION_PARAMETERS = ^TWLAN_CONNECTION_PARAMETERS;
	TWLAN_CONNECTION_PARAMETERS = record
  	wlanConnectionMode: TWLAN_CONNECTION_MODE;
    strProfile: LPCTSTR;
    pDot11Ssid: PDOT11_SSID;
    pDesiredBssidList: PDOT11_BSSID_LIST;
    dot11BssType: TDOT11_BSS_TYPE;
    dwFlags: DWORD;
  end;

  PWLAN_MSM_NOTIFICATION_DATA = ^TWLAN_MSM_NOTIFICATION_DATA;
  TWLAN_MSM_NOTIFICATION_DATA = record
  	wlanConnectionMode: TWLAN_CONNECTION_MODE;
    strProfileName: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    dot11Ssid: TDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE;
    dot11MacAddr: TDOT11_MAC_ADDRESS;
    bSecurityEnabled: Bool;
    bFirstPeer: Bool;
    bLastPeer: Bool;
    wlanReasonCode: TWLAN_REASON_CODE;
  end;

  PWLAN_CONNECTION_NOTIFICATION_DATA = ^TWLAN_CONNECTION_NOTIFICATION_DATA;
  TWLAN_CONNECTION_NOTIFICATION_DATA = record
  	wlanConnectionMode: TWLAN_CONNECTION_MODE;
    strProfileName: array[0..WLAN_MAX_NAME_LENGTH - 1] of wchar;
    dot11Ssid: TDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE;
    bSecurityEnabled: Bool;
    wlanReasonCode: TWLAN_REASON_CODE;
    strProfileXml: array[0..0] of wchar;
  end;

const
	WLAN_NOTIFICATION_SOURCE_NONE = L2_NOTIFICATION_SOURCE_NONE;
  WLAN_NOTIFICATION_SOURCE_ALL 	= L2_NOTIFICATION_SOURCE_ALL;

  WLAN_NOTIFICATION_SOURCE_ACM	= L2_NOTIFICATION_SOURCE_WLAN_ACM;
  WLAN_NOTIFICATION_SOURCE_MSM	= L2_NOTIFICATION_SOURCE_WLAN_MSM;
  WLAN_NOTIFICATION_SOURCE_SECURITY	= L2_NOTIFICATION_SOURCE_WLAN_SECURITY;
  WLAN_NOTIFICATION_SOURCE_IHV	= L2_NOTIFICATION_SOURCE_WLAN_IHV;

type
	{$MINENUMSIZE 4}
	PWLAN_NOTIFICATION_ACM = ^TWLAN_NOTIFICATION_ACM;
	TWLAN_NOTIFICATION_ACM = (
  	wlan_notification_acm_start = L2_NOTIFICATION_CODE_PUBLIC_BEGIN,
    wlan_notification_acm_autoconf_enabled,
    wlan_notification_acm_autoconf_disabled,
    wlan_notification_acm_background_scan_enabled,
    wlan_notification_acm_background_scan_disabled,
    wlan_notification_acm_bss_type_change,
    wlan_notification_acm_power_setting_change,
    wlan_notification_acm_scan_complete,
    wlan_notification_acm_scan_fail,
    wlan_notification_acm_connection_start,
    wlan_notification_acm_connection_complete,
    wlan_notification_acm_connection_attempt_fail,
    wlan_notification_acm_filter_list_change,
    wlan_notification_acm_interface_arrival,
    wlan_notification_acm_interface_removal,
    wlan_notification_acm_profile_change,
    wlan_notification_acm_profile_name_change,
    wlan_notification_acm_profiles_exhausted,
    wlan_notification_acm_network_not_available,
    wlan_notification_acm_network_available,
    wlan_notification_acm_disconnecting,
    wlan_notification_acm_disconnected,
    wlan_notification_acm_end);

  {$MINENUMSIZE 4}
  PWLAN_NOTIFICATION_MSM = ^TWLAN_NOTIFICATION_MSM;
  TWLAN_NOTIFICATION_MSM = (
  	wlan_notification_msm_start = L2_NOTIFICATION_CODE_PUBLIC_BEGIN,
    wlan_notification_msm_associating,
    wlan_notification_msm_associated,
    wlan_notification_msm_authenticating,
    wlan_notification_msm_connected,
    wlan_notification_msm_roaming_start,
    wlan_notification_msm_roaming_end,
    wlan_notification_msm_radio_state_change,
    wlan_notification_msm_signal_quality_change,
    wlan_notification_msm_disassociating,
    wlan_notification_msm_disconnected,
    wlan_notification_msm_peer_join,
    wlan_notification_msm_peer_leave,
    wlan_notification_msm_end);

  {$MINENUMSIZE 4}
  PWLAN_NOTIFICATION_SECURITY = ^TWLAN_NOTIFICATION_SECURITY;
  TWLAN_NOTIFICATION_SECURITY = (
  	wlan_notification_security_start = L2_NOTIFICATION_CODE_PUBLIC_BEGIN,
    wlan_notification_security_end);

  TWLAN_NOTIFICATION_DATA = TL2_NOTIFICATION_DATA;
  PWLAN_NOTIFICATION_DATA = ^TWLAN_NOTIFICATION_DATA;

  TWLAN_NOTIFICATION_CALLBACK	= PVOID;
  PWLAN_NOTIFICATION_CALLBACK	= PVOID;

  {$MINENUMSIZE 4}
  PWLAN_OPCODE_VALUE_TYPE = ^TWLAN_OPCODE_VALUE_TYPE;
  TWLAN_OPCODE_VALUE_TYPE = (
  	wlan_opcode_value_type_query_only = 0,
    wlan_opcode_value_type_set_by_group_policy,
    wlan_opcode_value_type_set_by_user,
    wlan_opcode_value_type_invalid);

  {$MINENUMSIZE 4}
  PWLAN_INTF_OPCODE = ^TWLAN_INTF_OPCODE;
  TWLAN_INTF_OPCODE = (
  	wlan_intf_opcode_autoconf_start = $000000000,
    wlan_intf_opcode_autoconf_enabled,
    wlan_intf_opcode_background_scan_enabled,
    wlan_intf_opcode_media_streaming_mode,
    wlan_intf_opcode_radio_state,
    wlan_intf_opcode_bss_type,
    wlan_intf_opcode_interface_state,
    wlan_intf_opcode_current_connection,
    wlan_intf_opcode_channel_number,
    wlan_intf_opcode_supported_infrastructure_auth_cipher_pairs,
    wlan_intf_opcode_supported_adhoc_auth_cipher_pairs,
    wlan_intf_opcode_supported_country_or_region_string_list,
    wlan_intf_opcode_autoconf_end = $0fffffff,
    wlan_intf_opcode_msm_start = $10000100,
    wlan_intf_opcode_statistics,
    wlan_intf_opcode_rssi,
    wlan_intf_opcode_msm_end = $1fffffff,
    wlan_intf_opcode_security_start = $20010000,
    wlan_intf_opcode_security_end = $2fffffff,
    wlan_intf_opcode_ihv_start = $30000000,
    wlan_intf_opcode_ihv_end = $3fffffff);

  {$MINENUMSIZE 4}
  PWLAN_AUTOCONF_OPCODE = ^TWLAN_AUTOCONF_OPCODE;
  TWLAN_AUTOCONF_OPCODE = (
  	wlan_autoconf_opcode_start = 0,
    wlan_autoconf_opcode_show_denied_networks,
    wlan_autoconf_opcode_power_setting,
    wlan_autoconf_opcode_connect_with_all_user_profile_only,
    wlan_autoconf_opcode_end);

  {$MINENUMSIZE 4}
  PWLAN_IHV_CONTROL_TYPE = ^TWLAN_IHV_CONTROL_TYPE;
  TWLAN_IHV_CONTROL_TYPE = (
  	wlan_ihv_control_type_service,
    wlan_ihv_control_type_driver);

  {$MINENUMSIZE 4}
  PWLAN_FILTER_LIST_TYPE = ^TWLAN_FILTER_LIST_TYPE;
  TWLAN_FILTER_LIST_TYPE = (
  	wlan_filter_list_type_gp_permit,
    wlan_filter_list_type_gp_deny,
    wlan_filter_list_type_user_permit,
    wlan_filter_list_type_user_deny);

  PWLAN_PHY_FRAME_STATISTICS = ^TWLAN_PHY_FRAME_STATISTICS;
  TWLAN_PHY_FRAME_STATISTICS = record
  	ullTransmittedFrameCount: TLargeInteger;
    ullMulticastTransmittedFrameCount: TLargeInteger;
    ullFailedCount: TLargeInteger;
    ullRetryCount: TLargeInteger;
    ullMultipleRetryCount: TLargeInteger;
    ullMaxTXLifetimeExceededCount: TLargeInteger;
    ullTransmittedFragmentCount: TLargeInteger;
    ullRTSSuccessCount: TLargeInteger;
    ullRTSFailureCount: TLargeInteger;
    ullACKFailureCount: TLargeInteger;
    ullReceivedFrameCount: TLargeInteger;
    ullMulticastReceivedFrameCount: TLargeInteger;
    ullPromiscuousReceivedFrameCount: TLargeInteger;
    ullMaxRXLifetimeExceededCount: TLargeInteger;
    ullFrameDuplicateCount: TLargeInteger;
    ullReceivedFragmentCount: TLargeInteger;
    ullPromiscuousReceivedFragmentCount: TLargeInteger;
    ullFCSErrorCount: TLargeInteger;
  end;

  PWLAN_MAC_FRAME_STATISTICS = ^TWLAN_MAC_FRAME_STATISTICS;
  TWLAN_MAC_FRAME_STATISTICS = record
  	ullTransmittedFrameCount: TLargeInteger;
    ullReceivedFrameCount: TLargeInteger;
    ullWEPExcludedCount: TLargeInteger;
    ullTKIPLocalMICFailures: TLargeInteger;
    ullTKIPReplays: TLargeInteger;
    ullTKIPICVErrorCount: TLargeInteger;
    ullCCMPReplays: TLargeInteger;
    ullCCMPDecryptErrors: TLargeInteger;
    ullWEPUndecryptableCount: TLargeInteger;
    ullWEPICVErrorCount: TLargeInteger;
    ullDecryptSuccessCount: TLargeInteger;
    ullDecryptFailureCount: TLargeInteger;
  end;

  PWLAN_STATISTICS = ^TWLAN_STATISTICS;
  TWLAN_STATISTICS = record
  	ullFourWayHandshakeFailures: TLargeInteger;
    ullTKIPCounterMeasuresInvoked: TLargeInteger;
    ullReserved: TLargeInteger;
    MacUcastCounters: TWLAN_MAC_FRAME_STATISTICS;
    MacMcastCounters: TWLAN_MAC_FRAME_STATISTICS;
    dwNumberOfPhys: DWORD;
    PhyCounters: array[0..0] of TWLAN_PHY_FRAME_STATISTICS;
  end;

const
  WLAN_READ_ACCESS = (STANDARD_RIGHTS_READ or  FILE_READ_DATA);
  WLAN_EXECUTE_ACCESS = (WLAN_READ_ACCESS or
  	STANDARD_RIGHTS_EXECUTE or FILE_EXECUTE);
  WLAN_WRITE_ACCESS = (WLAN_READ_ACCESS or WLAN_EXECUTE_ACCESS or
  	STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or DELETE or
    WRITE_DAC);

type
	{$MINENUMSIZE 4}
	PWLAN_SECURABLE_OBJECT = ^TWLAN_SECURABLE_OBJECT;
  TWLAN_SECURABLE_OBJECT = (
  	wlan_secure_permit_list = 0,
    wlan_secure_deny_list,
    wlan_secure_ac_enabled,
    wlan_secure_bc_scan_enabled,
    wlan_secure_bss_type,
    wlan_secure_show_denied,
    wlan_secure_interface_properties,
    wlan_secure_ihv_control,
    wlan_secure_all_user_profiles_order,
    wlan_secure_sso,
    wlan_secure_add_new_all_user_profiles,
    wlan_secure_add_new_per_user_profiles,
    wlan_secure_manual_connect_single_user,
    wlan_secure_manual_connect_multi_user,
    wlan_secure_media_streaming_mode_enabled,
    WLAN_SECURABLE_OBJECT_COUNT);

const
	eapPropCipherSuiteNegotiation 						= $00000001;
  eapPropMutualAuth													= $00000002;
  eapPropIntegrity													= $00000004;
  eapPropReplayProtection										= $00000008;
  eapPropConfidentiality                 		= $00000010;
  eapPropKeyDerivation											= $00000020;
  eapPropKeyStrength64											= $00000040;
  eapPropKeyStrength128											= $00000080;
  eapPropKeyStrength256											= $00000100;
  eapPropKeyStrength512											= $00000200;
  eapPropKeyStrength1024										= $00000400;
  eapPropDictionaryAttackResistance					= $00000800;
  eapPropFastReconnect											= $00001000;
  eapPropCryptoBinding											= $00002000;
  eapPropSessionDependence									= $00004000;
  eapPropFragmentation											= $00008000;
  eapPropChannelBinding											= $00010000;
  eapPropNap																= $00020000;
  eapPropStandalone													= $00040000;
  eapPropMppeEncryption											= $00080000;
  eapPropTunnelMethod												= $00100000;
  eapPropSupportsConfig											= $00200000;
  eapPropReserved														= $80000000;

  EAP_VALUENAME_PROPERTIES									= 'Properties';

type
	EAP_SESSIONID = DWORD;

  TEAP_TYPE = record
  	atype: Byte;
    dwVendorId: DWORD;
    dwVendorType: DWORD;
  end;

  TEAP_METHOD_TYPE = record
  	eapType: TEAP_TYPE;
    dwAuthorId: DWORD;
  end;

  PEAP_METHOD_INFO = ^TEAP_METHOD_INFO;
  TEAP_METHOD_INFO = record
  	eaptype: TEAP_METHOD_TYPE;
    pwszAuthorName: LPWSTR;
    pwszFriendlyName: LPWSTR;
    eapProperties: DWORD;
    pInnerMethodInfo: PEAP_METHOD_INFO;
  end;

  TEAP_METHOD_INFO_ARRAY = record
    dwNumberOfMethods: DWORD;
    pEapMethods: PEAP_METHOD_INFO;
  end;

  TEAP_ERROR = record
  	dwWinError: DWORD;
    atype: TEAP_METHOD_TYPE;
    dwReasonCode: DWORD;
    rootCauseGuid: TGUID;
    repairGuid: TGUID;
    helpLinkGuid: TGUID;

    pRootCauseString: LPWSTR;
    pRepairString: LPWSTR;
  end;

const
	WLAN_UI_API_VERSION 					= 1;
  WLAN_UI_API_INITIAL_VERSION		= 1;

type
	PWL_DISPLAY_PAGES = ^TWL_DISPLAY_PAGES;
	TWL_DISPLAY_PAGES = (
  	WLConnectionPage,
		WLSecurityPage);

  TWLAN_HOSTED_NETWORK_STATE = (
    wlan_hosted_network_unavailable,
    wlan_hosted_network_idle,
    wlan_hosted_network_active);

  TWLAN_HOSTED_NETWORK_PEER_AUTH_STATE = (
    wlan_hosted_network_peer_state_invalid,
    wlan_hosted_network_peer_state_authenticated
  );

  TWLAN_HOSTED_NETWORK_PEER_STATE = record
    PeerMacAddress: TDOT11_MAC_ADDRESS;
    PeerAuthState: TWLAN_HOSTED_NETWORK_PEER_AUTH_STATE;
  end;

  PWLAN_HOSTED_NETWORK_STATUS = ^TWLAN_HOSTED_NETWORK_STATUS;
  PPWLAN_HOSTED_NETWORK_STATUS = ^PWLAN_HOSTED_NETWORK_STATUS;
  TWLAN_HOSTED_NETWORK_STATUS = record
    HostedNetworkState: TWLAN_HOSTED_NETWORK_STATE;
    IPDeviceID: TGUID;
    wlanHostedNetworkBSSID: TDOT11_MAC_ADDRESS;
    dot11PhyType: TDOT11_PHY_TYPE;
    ulChannelFrequency: ULONG;
    dwNumberOfPeers: DWORD;
    PeerList: array[0..0] of TWLAN_HOSTED_NETWORK_PEER_STATE;
  end;

const
	wlanapi = 'wlanapi.dll';

function WlanOpenHandle(dwClientVersion: DWORD; pReserved: PVOID;
  	pdwNegotiatedVersion: PWord; phClientHandle: PHandle): DWORD; stdcall;

function WlanCloseHandle(hClientHandle: THandle;
  	pReserved: PVOID): DWORD; stdcall;

function WlanEnumInterfaces(hClientHandle: THandle;
  	pReserved: PVOID; ppInterfaceList: PPWLAN_INTERFACE_INFO_LIST
    ): DWORD; stdcall;

function WlanGetAvailableNetworkList(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; dwFlags: DWORD; pReserved: PVOID;
    var pAvailableNetworkList: PWLAN_AVAILABLE_NETWORK_LIST): DWORD; stdcall;

function WlanHostedNetworkQueryStatus(hClientHandle: THandle;
                                      ppWlanHostedNetworkStatus: PPWLAN_HOSTED_NETWORK_STATUS;
                                      pvReserved: PVOID): DWORD; stdcall;

function WlanGetNetworkBssList(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; const pDot11Ssid: PDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE; bSecurityEnabled: BOOL;
    pReserved: PVOID; var ppWlanBssList: PWLAN_BSS_LIST): DWORD; stdcall;

function WlanFreeMemory(pMemory: PVOID): PVOID; stdcall;

function WlanScan(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; const pDot11Ssid: PDOT11_SSID;
    const pIeData: PWLAN_RAW_DATA; pReserved: PVOID): DWORD; stdcall;


{$IFNDEF DYNAMIC_LINK}

function WlanSetAutoConfigParameter(hClientHandle: THandle;
  	OpCode: TWLAN_AUTOCONF_OPCODE; dwDataSize: DWORD;
    const pData: PVOID; pReserved: PVOID): DWORD; stdcall;

function WlanQueryAutoConfigParameter(hClientHandle: THandle;
  	OpCode: TWLAN_AUTOCONF_OPCODE; pReserved: PVOID;
    pdwDataSize: PDWORD; ppData: PPVOID;
    pWlanOpcodeValueType: PWLAN_OPCODE_VALUE_TYPE): DWORD; stdcall;

function WlanGetInterfaceCapability(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; pReserved: PVOID;
    ppCapability: PPWLAN_INTERFACE_CAPABILITY): DWORD; stdcall;

function WlanSetInterface(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; OpCode: TWLAN_INTF_OPCODE;
    dwDataSize: DWORD; const pData: PVOID; pReserved: PVOID): DWORD; stdcall;

function WlanQueryInterface(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; OpCode: TWLAN_INTF_OPCODE;
    pReserved: PVOID; pdwDataSize: PDWORD; ppData: PPVOID;
    pWlanOpcodeValueType: PWLAN_OPCODE_VALUE_TYPE): DWORD; stdcall;

function WlanIhvControl(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; aType: TWLAN_IHV_CONTROL_TYPE;
    dwInBufferSize: DWORD; pInBuffer: pvoid; dwOutBufferSize: DWORD;
    pOutBuffer: PVOID): DWORD; stdcall;

function WlanConnect(hClientHandle: THandle; const pInterfaceGuid: PGUID;
  	const pConnectionParameters: PWLAN_CONNECTION_PARAMETERS;
    pReserved: PVOID): DWORD; stdcall;

function WlanDisconnect(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; pReserved: PVOID): DWORD; stdcall;

function WlanRegisterNotification(hClientHandle: THandle;
  	dwNotifSource: DWORD; bIgnoreDuplicate: Bool;
    funcCallback: TWLAN_NOTIFICATION_CALLBACK;
    pCallbackContext: PVOID; pReserved: PVOID;
    pdwPrevNotifSource: PDWORD): DWORD; stdcall;

function WlanGetProfile(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    pReserved: PVOID; pstrProfileXml: LPWSTR; pdwFlags: PDWORD;
    pdwGrantedAccess: PDWORD): DWORD; stdcall;

function WlanSetProfileEapUserData(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    eapType: TEAP_METHOD_TYPE; dwFlags: DWORD;
    dwEapUserDataSize: DWORD; const pbEapUserData: LPByte;
    pReserved: PVOID): DWORD; stdcall;

function WlanSetProfileEapXMLUserData(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    eapType: TEAP_METHOD_TYPE; dwFlags: DWORD;
    strEapXMLUserData: LPCWSTR; pReserved: PVOID): DWORD; stdcall;

function WlanSetProfile(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; dwFlags: DWORD; strProfileXml: LPCWSTR;
    strAllUserProfileSecurity: LPCWSTR;
    bOverwrite: Bool; pReserved: PVOID;
    pdwReasonCode: PDWORD): DWORD; stdcall;

function WlanDeleteProfile(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    pReserved: PVOID): DWORD; stdcall;

function WlanRenameProfile(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strOldProfileName: LPCWSTR;
    strNewProfileName: LPCWSTR; pReserved: PVOID): DWORD; stdcall;

function WlanGetProfileList(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; pReserved: PVOID;
    ppProfileList: PPWLAN_PROFILE_INFO_LIST): DWORD; stdcall;

function WlanSetProfileList(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; dwItems: DWORD;
    strProfileNames: LPCWSTR; pReserved: PVOID): DWORD; stdcall;

function WlanSetProfilePosition(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    dwPosition: DWORD; pReserved: PVOID): DWORD; stdcall;

function WlanSetProfileCustomUserData(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    dwDataSize: DWORD; const pData: LPByte;
    pReserved: PVOID): DWORD; stdcall;

function WlanGetProfileCustomUserData(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    pReserved: PVOID; pdwDataSize: PDWORD; ppData: PPByte): DWORD; stdcall;

function WlanSetFilterList(hClientHandle: THandle;
  	wlanFilterListType: TWLAN_FILTER_LIST_TYPE;
    const pNetworkList: PDOT11_NETWORK_LIST;
    pReserved: PVOID): DWORD; stdcall;

function WlanGetFilterList(hClientHandle: THandle;
  	wlanFilterListType: TWLAN_FILTER_LIST_TYPE;
    pReserved: PVOID; ppNetworkList: PPDOT11_NETWORK_LIST): DWORD; stdcall;

function WlanSetPsdIEDataList(hClientHandle: THandle; strFormat: LPCWSTR;
    const pPsdIEDataList: PWLAN_RAW_DATA_LIST;
    pReserved: pvoid): DWORD; stdcall;

function WlanSaveTemporaryProfile(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; strProfileName: LPCWSTR;
    strAllUserProfileSecurity: LPCWSTR; dwFlags: DWORD;
    bOverWrite: Bool; pReserved: PVOID): DWORD; stdcall;

function WlanExtractPsdIEDataList(hClientHandle: THandle;
  	dwIeDataSize: DWORD; const pRawIeData: PByte;
    strFormat: LPCWSTR; pReserved: PVOID;
    ppPsdIEDataList: PPWLAN_RAW_DATA_LIST): DWORD; stdcall;

function WlanReasonCodeToString(dwReasonCode: DWORD;
  	dwBufferSize: DWORD; pStringBuffer: PWChar;
    pReserved: PVOID): DWORD; stdcall;

function WlanAllocateMemory(dwMemorySize: DWORD): pvoid; stdcall;

function WlanSetSecuritySettings(hClientHandle: THandle;
  	SecurableObject: TWLAN_SECURABLE_OBJECT;
    strModifiedSDDL: LPCWSTR): DWORD; stdcall;

function WlanGetSecuritySettings(hClientHandle: THandle;
  	SecurableObject: TWLAN_SECURABLE_OBJECT;
    pstrCurrentSDDL: PLPWSTR; pdwGrantedAccess: PWORD): DWORD; stdcall;

function WlanUIEditProfile(dwClientVersion: DWORD;
  	wstrProfileName: LPCWSTR; pInterfaceGuid: PGUID;
    hWnd: HWND; wlStartPage: TWL_DISPLAY_PAGES;
    pReserved: PVOID; pWlanReasonCode: PWLAN_REASON_CODE): DWORD; stdcall;

{$ENDIF}

implementation

{$IFDEF DYNAMIC_LINK}

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

type
  TWlanOpenHandle = function(dwClientVersion: DWORD; pReserved: PVOID;
  	pdwNegotiatedVersion: PWord; phClientHandle: PHandle): DWORD; stdcall;
var
  _WlanOpenHandle: TWlanOpenHandle;

function WlanOpenHandle;
begin
  GetProcedureAddress(Pointer(@_WlanOpenHandle),wlanapi,'WlanOpenHandle');
  if Assigned(_WlanOpenHandle) then
    Result:=_WlanOpenHandle(dwClientVersion,pReserved,pdwNegotiatedVersion,phClientHandle)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanCloseHandle = function(hClientHandle: THandle; pReserved: PVOID): DWORD; stdcall;
var
  _WlanCloseHandle: TWlanCloseHandle;

function WlanCloseHandle;
begin
  GetProcedureAddress(Pointer(@_WlanCloseHandle),wlanapi,'WlanCloseHandle');
  if Assigned(_WlanCloseHandle) then
    Result:=_WlanCloseHandle(hClientHandle,pReserved)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanEnumInterfaces = function(hClientHandle: THandle; pReserved: PVOID;
                                 ppInterfaceList: PPWLAN_INTERFACE_INFO_LIST): DWORD; stdcall;
var
  _WlanEnumInterfaces: TWlanEnumInterfaces;

function WlanEnumInterfaces;
begin
  GetProcedureAddress(Pointer(@_WlanEnumInterfaces),wlanapi,'WlanEnumInterfaces');
  if Assigned(_WlanEnumInterfaces) then
    Result:=_WlanEnumInterfaces(hClientHandle,pReserved,ppInterfaceList)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanGetAvailableNetworkList = function(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; dwFlags: DWORD; pReserved: PVOID;
    var pAvailableNetworkList: PWLAN_AVAILABLE_NETWORK_LIST): DWORD; stdcall;
var
  _WlanGetAvailableNetworkList: TWlanGetAvailableNetworkList;

function WlanGetAvailableNetworkList;
begin
  GetProcedureAddress(Pointer(@_WlanGetAvailableNetworkList),wlanapi,'WlanGetAvailableNetworkList');
  if Assigned(_WlanGetAvailableNetworkList) then
    Result:=_WlanGetAvailableNetworkList(hClientHandle,pInterfaceGuid,dwFlags,pReserved,pAvailableNetworkList)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanHostedNetworkQueryStatus = function(hClientHandle: THandle;
                                      ppWlanHostedNetworkStatus: PPWLAN_HOSTED_NETWORK_STATUS;
                                      pvReserved: PVOID): DWORD; stdcall;
var
  _WlanHostedNetworkQueryStatus: TWlanHostedNetworkQueryStatus;

function WlanHostedNetworkQueryStatus;
begin
  GetProcedureAddress(Pointer(@_WlanHostedNetworkQueryStatus),wlanapi,'WlanHostedNetworkQueryStatus');
  if Assigned(_WlanHostedNetworkQueryStatus) then
    Result:=_WlanHostedNetworkQueryStatus(hClientHandle,ppWlanHostedNetworkStatus,pvReserved)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanGetNetworkBssList = function(hClientHandle: THandle;
  	const pInterfaceGuid: PGUID; const pDot11Ssid: PDOT11_SSID;
    dot11BssType: TDOT11_BSS_TYPE; bSecurityEnabled: BOOL;
    pReserved: PVOID; var ppWlanBssList: PWLAN_BSS_LIST): DWORD; stdcall;
var
  _WlanGetNetworkBssList: TWlanGetNetworkBssList;

function WlanGetNetworkBssList;
begin
  GetProcedureAddress(Pointer(@_WlanGetNetworkBssList),wlanapi,'WlanGetNetworkBssList');
  if Assigned(_WlanGetNetworkBssList) then
    Result:=_WlanGetNetworkBssList(hClientHandle,pInterfaceGuid,pDot11Ssid,dot11BssType,bSecurityEnabled,pReserved,ppWlanBssList)
  else
    Result:=ERROR_CALL_NOT_IMPLEMENTED;
end;

type
  TWlanFreeMemory = function(pMemory: PVOID): PVOID; stdcall;
var
  _WlanFreeMemory: TWlanFreeMemory;

function WlanFreeMemory;
begin
  GetProcedureAddress(Pointer(@_WlanFreeMemory),wlanapi,'WlanFreeMemory');
  if Assigned(_WlanFreeMemory) then
    Result:=_WlanFreeMemory(pMemory)
  else
    Result:=nil;
end;

type
  TWlanScan = function(hClientHandle: THandle;
                       const pInterfaceGuid: PGUID; const pDot11Ssid: PDOT11_SSID;
                       const pIeData: PWLAN_RAW_DATA; pReserved: PVOID): DWORD; stdcall;
var
  _WlanScan: TWlanScan;

function WlanScan;
begin
  GetProcedureAddress(Pointer(@_WlanScan),wlanapi,'WlanScan');
  if Assigned(_WlanScan) then
    Result:=_WlanScan(hClientHandle,pInterfaceGuid,pDot11Ssid,pIeData,pReserved)
  else
    Result:=ERROR_INVALID_HANDLE;
end;



{$ELSE}
function WlanOpenHandle;								external  wlanapi	name	'WlanOpenHandle';
function WlanCloseHandle;								external	wlanapi	name	'WlanCloseHandle';
function WlanEnumInterfaces; 						external	wlanapi	name	'WlanEnumInterfaces';
function WlanSetAutoConfigParameter;		external	wlanapi	name	'WlanSetAutoConfigParameter';
function WlanQueryAutoConfigParameter;  external	wlanapi	name	'WlanQueryAutoConfigParameter';
function WlanGetInterfaceCapability;		external	wlanapi	name	'WlanGetInterfaceCapability';
function WlanSetInterface;							external	wlanapi	name	'WlanSetInterface';
function WlanQueryInterface;						external	wlanapi	name	'WlanQueryInterface';
function WlanIhvControl;								external	wlanapi	name	'WlanIhvControl';
function WlanScan;											external	wlanapi	name	'WlanScan';
function WlanGetAvailableNetworkList;		external	wlanapi	name	'WlanGetAvailableNetworkList';
function WlanGetNetworkBssList;					external	wlanapi	name	'WlanGetNetworkBssList';
function WlanConnect;										external	wlanapi	name	'WlanConnect';
function WlanDisconnect;								external	wlanapi	name	'WlanDisconnect';
function WlanRegisterNotification;			external	wlanapi	name	'WlanRegisterNotification';
function WlanGetProfile;								external	wlanapi	name	'WlanGetProfile';
function WlanSetProfileEapUserData;			external	wlanapi	name	'WlanSetProfileEapUserData';
function WlanSetProfileEapXMLUserData;	external	wlanapi	name	'WlanSetProfileEapXMLUserData';
function WlanSetProfile;								external	wlanapi	name	'WlanSetProfile';
function WlanDeleteProfile;							external	wlanapi	name	'WlanDeleteProfile';
function WlanRenameProfile;							external	wlanapi	name	'WlanRenameProfile';
function WlanGetProfileList;						external	wlanapi	name	'WlanGetProfileList';
function WlanSetProfileList;						external	wlanapi	name	'WlanSetProfileList';
function WlanSetProfilePosition;				external	wlanapi	name	'WlanSetProfilePosition';
function WlanSetProfileCustomUserData;	external  wlanapi	name 	'WlanSetProfileCustomUserData';
function WlanGetProfileCustomUserData;	external	wlanapi	name	'WlanGetProfileCustomUserData';
function WlanSetFilterList;							external	wlanapi	name	'WlanSetFilterList';
function WlanGetFilterList;							external	wlanapi	name	'WlanGetFilterList';
function WlanSetPsdIEDataList;					external	wlanapi	name	'WlanSetPsdIEDataList';
function WlanSaveTemporaryProfile;			external	wlanapi	name	'WlanSaveTemporaryProfile';
function WlanExtractPsdIEDataList;			external	wlanapi	name	'WlanExtractPsdIEDataList';
function WlanReasonCodeToString;				external	wlanapi	name	'WlanReasonCodeToString';
function WlanAllocateMemory;						external	wlanapi	name	'WlanAllocateMemory';
function WlanFreeMemory;								external	wlanapi	name	'WlanFreeMemory';
function WlanSetSecuritySettings;				external	wlanapi	name	'WlanSetSecuritySettings';
function WlanGetSecuritySettings;				external	wlanapi	name	'WlanGetSecuritySettings';
function WlanUIEditProfile;							external	wlanapi	name	'WlanUIEditProfile';
function WlanHostedNetworkQueryStatus   external	wlanapi	name	'WlanHostedNetworkQueryStatus';
{$ENDIF}

end.
