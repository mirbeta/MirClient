{*******************************************************}
{               MiTeC Common Routines                   }
{          Native Bluetooth Enumerator API              }
{                                                       }
{         Copyright (c) 1997-2014 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_BTAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  BLUETOOTH_MAX_NAME_SIZE            = 248;
  {$EXTERNALSYM BLUETOOTH_MAX_NAME_SIZE}
  BLUETOOTH_MAX_PASSKEY_SIZE         = 16;
  {$EXTERNALSYM BLUETOOTH_MAX_PASSKEY_SIZE}
  BLUETOOTH_MAX_PASSKEY_BUFFER_SIZE  = BLUETOOTH_MAX_PASSKEY_SIZE + 1;
  {$EXTERNALSYM BLUETOOTH_MAX_PASSKEY_BUFFER_SIZE}

// ***************************************************************************
//
//  Sdp defs
//
// ***************************************************************************

type
  SDP_LARGE_INTEGER_16 = record
    LowPart: Int64;
    HighPart: Int64;
  end;
  {$EXTERNALSYM SDP_LARGE_INTEGER_16}
  PSDP_LARGE_INTEGER_16 = ^SDP_LARGE_INTEGER_16;
  {$EXTERNALSYM PSDP_LARGE_INTEGER_16}
  LPSDP_LARGE_INTEGER_16 = PSDP_LARGE_INTEGER_16;
  {$EXTERNALSYM LPSDP_LARGE_INTEGER_16}
  TSdpLargeInteger = SDP_LARGE_INTEGER_16;
  PSdpLargeInteger = PSDP_LARGE_INTEGER_16;

  SDP_ULARGE_INTEGER_16 = record
    LowPart: Int64;
    HighPart: Int64;
  end;
  {$EXTERNALSYM SDP_ULARGE_INTEGER_16}
  PSDP_ULARGE_INTEGER_16 = ^SDP_ULARGE_INTEGER_16;
  {$EXTERNALSYM PSDP_ULARGE_INTEGER_16}
  LPSDP_ULARGE_INTEGER_16 = PSDP_ULARGE_INTEGER_16;
  {$EXTERNALSYM LPSDP_ULARGE_INTEGER_16}
  TSdpULargeInteger16 = SDP_ULARGE_INTEGER_16;
  PSdpULargeInteger16 = PSDP_ULARGE_INTEGER_16;

  NodeContainerType = (NodeContainerTypeSequence, NodeContainerTypeAlternative);
  TNodeContainerType = NodeContainerType;

  SDP_ERROR = Word;
  {$EXTERNALSYM SDP_ERROR}
  PSDP_ERROR = ^SDP_ERROR;
  {$EXTERNALSYM PSDP_ERROR}
  TSdpError = SDP_ERROR;
  PSdpError = PSDP_ERROR;

type
  SDP_TYPE = DWORD;
  {$EXTERNALSYM SDP_TYPE}
  TSdpType = SDP_TYPE;

const
  SDP_TYPE_NIL = $00;
  {$EXTERNALSYM SDP_TYPE_NIL}
  SDP_TYPE_UINT = $01;
  {$EXTERNALSYM SDP_TYPE_UINT}
  SDP_TYPE_INT = $02;
  {$EXTERNALSYM SDP_TYPE_INT}
  SDP_TYPE_UUID = $03;
  {$EXTERNALSYM SDP_TYPE_UUID}
  SDP_TYPE_STRING = $04;
  {$EXTERNALSYM SDP_TYPE_STRING}
  SDP_TYPE_BOOLEAN = $05;
  {$EXTERNALSYM SDP_TYPE_BOOLEAN}
  SDP_TYPE_SEQUENCE = $06;
  {$EXTERNALSYM SDP_TYPE_SEQUENCE}
  SDP_TYPE_ALTERNATIVE = $07;
  {$EXTERNALSYM SDP_TYPE_ALTERNATIVE}
  SDP_TYPE_URL = $08;
  {$EXTERNALSYM SDP_TYPE_URL}
  // 9 - 31 are reserved
  SDP_TYPE_CONTAINER = $20;
  {$EXTERNALSYM SDP_TYPE_CONTAINER}

// allow for a little easier type checking / sizing for integers and UUIDs
// ((SDP_ST_XXX & 0xF0) >> 4) == SDP_TYPE_XXX
// size of the data (in bytes) is encoded as ((SDP_ST_XXX & 0xF0) >> 8)

type
  SDP_SPECIFICTYPE = DWORD;
  {$EXTERNALSYM SDP_SPECIFICTYPE}
  TSdpSpecificType = SDP_SPECIFICTYPE;

// ***************************************************************************
//
//  Bluetooth Address
//
// ***************************************************************************

type
  BTH_ADDR = Int64;
  {$EXTERNALSYM BTH_ADDR}

  _BLUETOOTH_ADDRESS = record
    case Integer of
      0: (ullLong: BTH_ADDR);       //  easier to compare again BLUETOOTH_NULL_ADDRESS
      1: (rgBytes: array [0..5] of Byte);   //  easier to format when broken out
  end;
  {$EXTERNALSYM _BLUETOOTH_ADDRESS}
  BLUETOOTH_ADDRESS = _BLUETOOTH_ADDRESS;
  {$EXTERNALSYM BLUETOOTH_ADDRESS}
  TBlueToothAddress = BLUETOOTH_ADDRESS;
  PBlueToothAddress = ^BLUETOOTH_ADDRESS;

const
  BLUETOOTH_NULL_ADDRESS: TBlueToothAddress = (ullLong: 0;);
  {$EXTERNALSYM BLUETOOTH_NULL_ADDRESS}


type
  _BLUETOOTH_FIND_RADIO_PARAMS = record
    dwSize: DWORD;             //  IN  sizeof this structure
  end;
  {$EXTERNALSYM _BLUETOOTH_FIND_RADIO_PARAMS}
  BLUETOOTH_FIND_RADIO_PARAMS = _BLUETOOTH_FIND_RADIO_PARAMS;
  {$EXTERNALSYM BLUETOOTH_FIND_RADIO_PARAMS}
  TBlueToothFindRadioParams = BLUETOOTH_FIND_RADIO_PARAMS;
  PBlueToothFindRadioParams = ^BLUETOOTH_FIND_RADIO_PARAMS;

  HBLUETOOTH_RADIO_FIND = THandle;
  {$EXTERNALSYM HBLUETOOTH_RADIO_FIND}


// ***************************************************************************
//
//  Radio Information
//
// ***************************************************************************

type
  _BLUETOOTH_RADIO_INFO = record
    dwSize: DWORD;                               // Size, in bytes, of this entire data structure
    address: BLUETOOTH_ADDRESS;                  // Address of the local radio
    szName: array [0..BLUETOOTH_MAX_NAME_SIZE - 1] of WideChar;    // Name of the local radio
    ulClassofDevice: ULONG;                      // Class of device for the local radio
    lmpSubversion: Word;                       // lmpSubversion, manufacturer specifc.
    manufacturer: Word;                        // Manufacturer of the radio, BTH_MFG_Xxx value.  For the most up to date
                                                // list, goto the Bluetooth specification website and get the Bluetooth
                                                // assigned numbers document.
  end;
  {$EXTERNALSYM _BLUETOOTH_RADIO_INFO}
  BLUETOOTH_RADIO_INFO = _BLUETOOTH_RADIO_INFO;
  {$EXTERNALSYM BLUETOOTH_RADIO_INFO}
  PBLUETOOTH_RADIO_INFO = ^BLUETOOTH_RADIO_INFO;
  {$EXTERNALSYM PBLUETOOTH_RADIO_INFO}
  TBlueToothRadioFind = BLUETOOTH_RADIO_INFO;
  PBlueToothRadioFind = PBLUETOOTH_RADIO_INFO;


// ***************************************************************************
//
//  Device Information Stuctures
//
// ***************************************************************************

type
  _BLUETOOTH_DEVICE_INFO = record
    dwSize: DWORD;                             //  size, in bytes, of this structure - must be the sizeof(BLUETOOTH_DEVICE_INFO)
    Address: BLUETOOTH_ADDRESS;                  //  Bluetooth address
    ulClassofDevice: ULONG;                    //  Bluetooth "Class of Device"
    fConnected: BOOL;                         //  Device connected/in use
    fRemembered: BOOL;                        //  Device remembered
    fAuthenticated: BOOL;                     //  Device authenticated/paired/bonded
    stLastSeen: SYSTEMTIME;                     //  Last time the device was seen
    stLastUsed: SYSTEMTIME;                     //  Last time the device was used for other than RNR, inquiry, or SDP
    szName: array [0..BLUETOOTH_MAX_NAME_SIZE - 1] of WideChar;  //  Name of the device
  end;
  {$EXTERNALSYM _BLUETOOTH_DEVICE_INFO}
  BLUETOOTH_DEVICE_INFO = _BLUETOOTH_DEVICE_INFO;
  {$EXTERNALSYM BLUETOOTH_DEVICE_INFO}
  PBLUETOOTH_DEVICE_INFO = ^BLUETOOTH_DEVICE_INFO;
  {$EXTERNALSYM PBLUETOOTH_DEVICE_INFO}
  TBlueToothDeviceInfo = BLUETOOTH_DEVICE_INFO;
  PBlueToothDeviceInfo = PBLUETOOTH_DEVICE_INFO;


type
  _BLUETOOTH_DEVICE_SEARCH_PARAMS = record
    dwSize: DWORD;                 //  IN  sizeof this structure

    fReturnAuthenticated: BOOL;   //  IN  return authenticated devices
    fReturnRemembered: BOOL;      //  IN  return remembered devices
    fReturnUnknown: BOOL;         //  IN  return unknown devices
    fReturnConnected: BOOL;       //  IN  return connected devices

    fIssueInquiry: BOOL;          //  IN  issue a new inquiry
    cTimeoutMultiplier: UCHAR;     //  IN  timeout for the inquiry

    hRadio: THandle;                 //  IN  handle to radio to enumerate - NULL == all radios will be searched
  end;
  {$EXTERNALSYM _BLUETOOTH_DEVICE_SEARCH_PARAMS}
  BLUETOOTH_DEVICE_SEARCH_PARAMS = _BLUETOOTH_DEVICE_SEARCH_PARAMS;
  {$EXTERNALSYM BLUETOOTH_DEVICE_SEARCH_PARAMS}
  PBLUETOOTH_DEVICE_SEARCH_PARAMS = ^BLUETOOTH_DEVICE_SEARCH_PARAMS;
  {$EXTERNALSYM PBLUETOOTH_DEVICE_SEARCH_PARAMS}
  TBlueToothDeviceSearchParams = BLUETOOTH_DEVICE_SEARCH_PARAMS;
  PBlueToothDeviceSearchParams = PBLUETOOTH_DEVICE_SEARCH_PARAMS;

  HBLUETOOTH_DEVICE_FIND = THandle;
  {$EXTERNALSYM HBLUETOOTH_DEVICE_FIND}


type
  _BLUETOOTH_COD_PAIRS = record
    ulCODMask: ULONG;                          //  ClassOfDevice mask to compare
    pcszDescription: LPWSTR;                    //  Descriptive string of mask
  end;
  {$EXTERNALSYM _BLUETOOTH_COD_PAIRS}
  BLUETOOTH_COD_PAIRS = _BLUETOOTH_COD_PAIRS;
  {$EXTERNALSYM BLUETOOTH_COD_PAIRS}
  TBlueToothCodPairs = BLUETOOTH_COD_PAIRS;
  PBlueToothCodPairs = ^BLUETOOTH_COD_PAIRS;

  PFN_DEVICE_CALLBACK = function(pvParam: Pointer; pDevice: PBLUETOOTH_DEVICE_INFO): BOOL; stdcall;
  {$EXTERNALSYM PFN_DEVICE_CALLBACK}

  _BLUETOOTH_SELECT_DEVICE_PARAMS = record
    dwSize: DWORD;                             //  IN  sizeof this structure

    cNumOfClasses: ULONG;                      //  IN  Number in prgClassOfDevice - if ZERO search for all devices
    prgClassOfDevices: PBlueToothCodPairs;    //  IN  Array of CODs to find.

    pszInfo: LPWSTR;                            //  IN  If not NULL, sets the "information" text

    hwndParent: HWND;                         //  IN  parent window - NULL == no parent

    fForceAuthentication: BOOL;               //  IN  If TRUE, authenication will be forced before returning
    fShowAuthenticated: BOOL;                 //  IN  If TRUE, authenticated devices will be shown in the picker
    fShowRemembered: BOOL;                    //  IN  If TRUE, remembered devices will be shown in the picker
    fShowUnknown: BOOL;                       //  IN  If TRUE, unknown devices that are not authenticated or "remember" will be shown.

    fAddNewDeviceWizard: BOOL;                //  IN  If TRUE, invokes the add new device wizard.
    fSkipServicesPage: BOOL;                  //  IN  If TRUE, skips the "Services" page in the wizard.

    pfnDeviceCallback: PFN_DEVICE_CALLBACK;      //  IN  If non-NULL, a callback that will be called for each device. If the
                                                //      the callback returns TRUE, the item will be added. If the callback is
                                                //      is FALSE, the item will not be shown.
    pvParam: Pointer;                            //  IN  Parameter to be passed to pfnDeviceCallback as the pvParam.

    cNumDevices: DWORD;                        //  IN  number calles wants - ZERO == no limit.
                                                //  OUT the number of devices returned.

    pDevices: PBLUETOOTH_DEVICE_INFO;           //  OUT pointer to an array for BLUETOOTH_DEVICE_INFOs.
                                                //      call BluetoothSelectDevicesFree() to free
  end;
  {$EXTERNALSYM _BLUETOOTH_SELECT_DEVICE_PARAMS}
  BLUETOOTH_SELECT_DEVICE_PARAMS = _BLUETOOTH_SELECT_DEVICE_PARAMS;
  {$EXTERNALSYM BLUETOOTH_SELECT_DEVICE_PARAMS}
  TBlueToothSelectDeviceParams = BLUETOOTH_SELECT_DEVICE_PARAMS;
  PBlueToothSelectDeviceParams = ^BLUETOOTH_SELECT_DEVICE_PARAMS;


// ***************************************************************************
//
//  Bluetooth Services
//
// ***************************************************************************

const
  BLUETOOTH_SERVICE_DISABLE  = $00;
  {$EXTERNALSYM BLUETOOTH_SERVICE_DISABLE}
  BLUETOOTH_SERVICE_ENABLE   = $01;
  {$EXTERNALSYM BLUETOOTH_SERVICE_ENABLE}
  BLUETOOTH_SERVICE_MASK     = BLUETOOTH_SERVICE_ENABLE or BLUETOOTH_SERVICE_DISABLE;
  {$EXTERNALSYM BLUETOOTH_SERVICE_MASK}

// ***************************************************************************
//
//  Authentication Registration
//
// ***************************************************************************

type
  HBLUETOOTH_AUTHENTICATION_REGISTRATION = THandle;
  {$EXTERNALSYM HBLUETOOTH_AUTHENTICATION_REGISTRATION}

  PFN_AUTHENTICATION_CALLBACK = function(pvParam: Pointer; pDevice: PBLUETOOTH_DEVICE_INFO): BOOL; stdcall;
  {$EXTERNALSYM PFN_AUTHENTICATION_CALLBACK}


// ***************************************************************************
//
//  SDP Parsing Functions
//
// ***************************************************************************

type
  TSpdElementDataString = record
    // raw string buffer, may not be encoded as ANSI, use
    // BluetoothSdpGetString to convert the value if it is described
    // by the base language attribute ID list
    value: PBYTE;
    // raw length of the string, may not be NULL terminuated
    length: ULONG;
  end;

  TSpdElementDataUrl = record
    value: PBYTE;
    length: ULONG;
  end;

  // type == SDP_TYPE_SEQUENCE
  TSpdElementDataSequence = record
    // raw sequence, starts at sequence element header
    value: PBYTE;
    // raw sequence length
    length: ULONG;
  end;

  // type == SDP_TYPE_ALTERNATIVE
  TSpdElementDataAlternative = record
    // raw alternative, starts at alternative element header
    value: PBYTE;
    // raw alternative length
    length: ULONG;
  end;

  _SDP_ELEMENT_DATA = record
    //
    // Enumeration of SDP element types.  Generic element types will have a
    // specificType value other then SDP_ST_NONE.  The generic types are:
    // o SDP_TYPE_UINT
    // o SDP_TYPE_INT
    // o SDP_TYPE_UUID
    //
    type_: SDP_TYPE;

    //
    // Specific types for the generic SDP element types.
    //
    specificType: SDP_SPECIFICTYPE;

    //
    // Union of all possible data types.  type and specificType will indicate
    // which field is valid.  For types which do not have a valid specificType,
    // specific type will be SDP_ST_NONE.
    //
    case Integer of
        // type == SDP_TYPE_INT
        0: (int128: SDP_LARGE_INTEGER_16);        // specificType == SDP_ST_INT128
        1: (int64: LONGLONG);                     // specificType == SDP_ST_INT64
        2: (int32: Integer);                         // specificType == SDP_ST_INT32
        3: (int16: SHORT);                        // specificType == SDP_ST_INT16
        4: (int8: Byte);                          // specificType == SDP_ST_INT8

        // type == SDP_TYPE_UINT
        5: (uint128: SDP_ULARGE_INTEGER_16);      // specificType == SDP_ST_UINT128
        6: (uint64: Int64);                   // specificType == SDP_ST_UINT64
        7: (uint32: ULONG);                       // specificType == SDP_ST_UINT32
        8: (uint16: Word);                      // specificType == SDP_ST_UINT16
        9: (uint8: UCHAR);                        // specificType == SDP_ST_UINT8

        // type == SDP_TYPE_BOOLEAN
        10: (booleanVal: UCHAR);

        // type == SDP_TYPE_UUID
        11: (uuid128: TGUID);                       // specificType == SDP_ST_UUID128
        12: (uuid32: ULONG);                       // specificType == SDP_ST_UUID32
        13: (uuid16: Word);                      // specificType == SDP_ST_UUID32

        // type == SDP_TYPE_STRING
        14: (string_: TSpdElementDataString);
        // type == SDP_TYPE_URL
        15: (url: TSpdElementDataUrl);

        // type == SDP_TYPE_SEQUENCE
        16: (sequence: TSpdElementDataSequence);

        // type == SDP_TYPE_ALTERNATIVE
        17: (alternative: TSpdElementDataAlternative);
  end;
  {$EXTERNALSYM _SDP_ELEMENT_DATA}
  SDP_ELEMENT_DATA = _SDP_ELEMENT_DATA;
  {$EXTERNALSYM SDP_ELEMENT_DATA}
  PSDP_ELEMENT_DATA = ^SDP_ELEMENT_DATA;
  {$EXTERNALSYM PSDP_ELEMENT_DATA}
  TSdpElementData = SDP_ELEMENT_DATA;
  PSdpElementData = PSDP_ELEMENT_DATA;

// ***************************************************************************
//
//  WM_DEVICE_CHANGE structures
//
// ***************************************************************************

  _BTH_HCI_EVENT_INFO = record
    //
    // Remote radio address which the HCI event is associated with
    //
    bthAddress: BTH_ADDR;

    //
    // HCI_CONNNECTION_TYPE_XXX value
    //
    connectionType: UCHAR;

    //
    // If != 0, then the underlying connection to the remote radio has just
    // been estrablished.  If == 0, then the underlying conneciton has just been
    // destroyed.
    //
    connected: UCHAR;
  end;
  {$EXTERNALSYM _BTH_HCI_EVENT_INFO}
  BTH_HCI_EVENT_INFO = _BTH_HCI_EVENT_INFO;
  {$EXTERNALSYM BTH_HCI_EVENT_INFO}
  PBTH_HCI_EVENT_INFO = ^BTH_HCI_EVENT_INFO;
  {$EXTERNALSYM PBTH_HCI_EVENT_INFO}
  TBthHciEventInfo = BTH_HCI_EVENT_INFO;
  PBthHciEventInfo = PBTH_HCI_EVENT_INFO;


type
  HBLUETOOTH_CONTAINER_ELEMENT = THandle;
  {$EXTERNALSYM HBLUETOOTH_CONTAINER_ELEMENT}


//
// These three fields correspond one to one with the triplets defined in the
// SDP specification for the language base attribute ID list.
//

type
  _SDP_STRING_TYPE_DATA = record
    //
    // How the string is encoded according to ISO 639:1988 (E/F): "Code
    // for the representation of names of languages".
    //
    encoding: Word;

    //
    // MIBE number from IANA database
    //
    mibeNum: Word;

    //
    // The base attribute where the string is to be found in the record
    //
    attributeId: Word;
  end;
  {$EXTERNALSYM _SDP_STRING_TYPE_DATA}
  SDP_STRING_TYPE_DATA = _SDP_STRING_TYPE_DATA;
  {$EXTERNALSYM SDP_STRING_TYPE_DATA}
  PSDP_STRING_TYPE_DATA = ^SDP_STRING_TYPE_DATA;
  {$EXTERNALSYM PSDP_STRING_TYPE_DATA}
  TSdpStringTypeData = SDP_STRING_TYPE_DATA;
  PSdpStringTypeData = PSDP_STRING_TYPE_DATA;


// ***************************************************************************
//
//  Raw Attribute  Enumeration
//
// ***************************************************************************

  PFN_BLUETOOTH_ENUM_ATTRIBUTES_CALLBACK = function(
    uAttribId: ULONG;
    pValueStream: PBYTE;
    cbStreamSize: ULONG;
    pvParam: Pointer): BOOL; stdcall;
  {$EXTERNALSYM PFN_BLUETOOTH_ENUM_ATTRIBUTES_CALLBACK}

  TBluetoothFindFirstRadio = function(const pbtfrp: PBlueToothFindRadioParams; var phRadio: THandle): HBLUETOOTH_RADIO_FIND; stdcall;
  TBluetoothFindNextRadio = function(hFind: HBLUETOOTH_RADIO_FIND; var phRadio: THandle): BOOL; stdcall;
  TBluetoothFindRadioClose = function(hFind: HBLUETOOTH_RADIO_FIND): BOOL; stdcall;
  TBluetoothSdpEnumAttributes = function(pSDPStream: PBYTE; cbStreamSize: ULONG; pfnCallback: PFN_BLUETOOTH_ENUM_ATTRIBUTES_CALLBACK; pvParam: Pointer): BOOL; stdcall;
  TBluetoothGetRadioInfo = function(hRadio: THandle; var pRadioInfo: BLUETOOTH_RADIO_INFO): DWORD; stdcall;
  TBluetoothEnumAttributes = function(pSDPStream: PBYTE; cbStreamSize: ULONG; pfnCallback: PFN_BLUETOOTH_ENUM_ATTRIBUTES_CALLBACK; pvParam: Pointer): BOOL;
  TBluetoothFindFirstDevice = function(const pbtsp: PBLUETOOTH_DEVICE_SEARCH_PARAMS; var pbtdi: BLUETOOTH_DEVICE_INFO): HBLUETOOTH_DEVICE_FIND; stdcall;
  TBluetoothFindNextDevice = function(hFind: HBLUETOOTH_DEVICE_FIND; var pbtdi: BLUETOOTH_DEVICE_INFO): BOOL; stdcall;
  TBluetoothFindDeviceClose = function(hFind: HBLUETOOTH_DEVICE_FIND): BOOL; stdcall;
  TBluetoothGetDeviceInfo = function(hRadio: THandle; var pbtdi: BLUETOOTH_DEVICE_INFO): DWORD; stdcall;
  TBluetoothUpdateDeviceRecord = function(var pbtdi: BLUETOOTH_DEVICE_INFO): DWORD; stdcall;
  TBluetoothRemoveDevice = function(var pAddress: BLUETOOTH_ADDRESS): DWORD; stdcall;
  TBluetoothSelectDevices = function(pbtsdp: PBlueToothSelectDeviceParams): BOOL; stdcall;
  TBluetoothSelectDevicesFree = function(pbtsdp: PBlueToothSelectDeviceParams): BOOL; stdcall;
  TBluetoothDisplayDeviceProperties = function(hwndParent: HWND; pbtdi: PBLUETOOTH_DEVICE_INFO): BOOL; stdcall;
  TBluetoothAuthenticateDevice = function(hwndParent: HWND; hRadio: THandle; pbtbi: PBLUETOOTH_DEVICE_INFO; pszPasskey: PWideChar; ulPasskeyLength: ULONG): DWORD; stdcall;
  TBluetoothAuthenticateMultipleDevices = function(hwndParent: HWND; hRadio: THandle; cDevices: DWORD; pbtdi: PBLUETOOTH_DEVICE_INFO): DWORD; stdcall;
  TBluetoothSetServiceState = function(hRadio: THandle; pbtdi: PBLUETOOTH_DEVICE_INFO; const pGuidService: TGUID; dwServiceFlags: DWORD): DWORD; stdcall;
  TBluetoothEnumerateInstalledServices = function(hRadio: THandle; pbtdi: PBLUETOOTH_DEVICE_INFO; var pcServices: DWORD; pGuidServices: PGUID): DWORD; stdcall;
  TBluetoothEnableDiscovery = function(hRadio: THandle; fEnabled: BOOL): BOOL; stdcall;
  TBluetoothIsDiscoverable = function(hRadio: THandle): BOOL; stdcall;
  TBluetoothEnableIncomingConnections = function(hRadio: THandle; fEnabled: BOOL): BOOL; stdcall;
  TBluetoothIsConnectable = function(hRadio: THandle): BOOL; stdcall;
  TBluetoothRegisterForAuthentication = function(pbtdi: PBLUETOOTH_DEVICE_INFO; var phRegHandle: HBLUETOOTH_AUTHENTICATION_REGISTRATION; pfnCallback: PFN_AUTHENTICATION_CALLBACK; pvParam: Pointer): DWORD; stdcall;
  TBluetoothUnregisterAuthentication = function(hRegHandle: HBLUETOOTH_AUTHENTICATION_REGISTRATION): BOOL; stdcall;
  TBluetoothSendAuthenticationResponse = function(hRadio: THandle; pbtdi: PBLUETOOTH_DEVICE_INFO; pszPasskey: LPWSTR): DWORD; stdcall;
  TBluetoothSdpGetElementData = function(pSdpStream: PBYTE; cbSdpStreamLength: ULONG; pData: PSDP_ELEMENT_DATA): DWORD; stdcall;
  TBluetoothSdpGetContainerElementData = function(pContainerStream: PBYTE; cbContainerLength: ULONG; var pElement: HBLUETOOTH_CONTAINER_ELEMENT; pData: PSDP_ELEMENT_DATA): DWORD; stdcall;
  TBluetoothSdpGetAttributeValue = function(pRecordStream: PBYTE; cbRecordLength: ULONG; usAttributeId: Word; pAttributeData: PSDP_ELEMENT_DATA): DWORD; stdcall;
  TBluetoothSdpGetString = function(pRecordStream: PBYTE; cbRecordLength: ULONG; pStringData: PSDP_STRING_TYPE_DATA; usStringOffset: Word; pszString: PWideChar; pcchStringLength: PULONG): DWORD; stdcall;

var
  BTHHandle: THandle;
  UnloadBTH: Boolean;
  BluetoothFindFirstRadio: TBluetoothFindFirstRadio;
  BluetoothFindNextRadio: TBluetoothFindNextRadio;
  BluetoothFindRadioClose: TBluetoothFindRadioClose;
  BluetoothGetRadioInfo: TBluetoothGetRadioInfo;
  BluetoothFindFirstDevice: TBluetoothFindFirstDevice;
  BluetoothFindNextDevice: TBluetoothFindNextDevice;
  BluetoothFindDeviceClose: TBluetoothFindDeviceClose;
  BluetoothGetDeviceInfo: TBluetoothGetDeviceInfo;
  BluetoothUpdateDeviceRecord: TBluetoothUpdateDeviceRecord;
  BluetoothRemoveDevice: TBluetoothRemoveDevice;
  BluetoothSelectDevices: TBluetoothSelectDevices;
  BluetoothSelectDevicesFree: TBluetoothSelectDevicesFree;
  BluetoothDisplayDeviceProperties: TBluetoothDisplayDeviceProperties;
  BluetoothAuthenticateDevice: TBluetoothAuthenticateDevice;
  BluetoothAuthenticateMultipleDevices: TBluetoothAuthenticateMultipleDevices;
  BluetoothSetServiceState: TBluetoothSetServiceState;
  BluetoothEnumerateInstalledServices: TBluetoothEnumerateInstalledServices;
  BluetoothEnableDiscovery: TBluetoothEnableDiscovery;
  BluetoothIsDiscoverable: TBluetoothIsDiscoverable;
  BluetoothEnableIncomingConnections: TBluetoothEnableIncomingConnections;
  BluetoothIsConnectable: TBluetoothIsConnectable;
  BluetoothRegisterForAuthentication: TBluetoothRegisterForAuthentication;
  BluetoothUnregisterAuthentication: TBluetoothUnregisterAuthentication;
  BluetoothSendAuthenticationResponse: TBluetoothSendAuthenticationResponse;
  BluetoothSdpGetElementData: TBluetoothSdpGetElementData;
  BluetoothSdpGetContainerElementData: TBluetoothSdpGetContainerElementData;
  BluetoothSdpGetAttributeValue: TBluetoothSdpGetAttributeValue;
  BluetoothSdpGetString: TBluetoothSdpGetString;
  BluetoothSdpEnumAttributes: TBluetoothSdpEnumAttributes;

function InitBTHAPI: boolean;
procedure FreeBTHAPI;

implementation

const
  btapi = 'irprops.cpl';

// (rom) MACRO implementation
function BluetoothEnumAttributes(pSDPStream: PBYTE; cbStreamSize: ULONG;
  pfnCallback: PFN_BLUETOOTH_ENUM_ATTRIBUTES_CALLBACK; pvParam: Pointer): BOOL;
begin
  Result := BluetoothSdpEnumAttributes(pSDPStream, cbStreamSize, pfnCallback, pvParam);
end;

function InitBTHAPI;
begin
  BTHHandle:=GetModuleHandle(btapi);
  UnloadBTH:=BTHHandle=0;
  if BTHHandle=0 then
    BTHHandle:=LoadLibrary(btapi);
  if BTHHandle<>0 then begin
    @BluetoothFindFirstRadio:=GetProcAddress(BTHHandle,'BluetoothFindFirstRadio');
    @BluetoothFindNextRadio:=GetProcAddress(BTHHandle,'BluetoothFindNextRadio');
    @BluetoothFindRadioClose:=GetProcAddress(BTHHandle,'BluetoothFindRadioClose');
    @BluetoothGetRadioInfo:=GetProcAddress(BTHHandle,'BluetoothGetRadioInfo');
    @BluetoothFindFirstDevice:=GetProcAddress(BTHHandle,'BluetoothFindFirstDevice');
    @BluetoothFindNextDevice:=GetProcAddress(BTHHandle,'BluetoothFindNextDevice');
    @BluetoothFindDeviceClose:=GetProcAddress(BTHHandle,'BluetoothFindDeviceClose');
    @BluetoothGetDeviceInfo:=GetProcAddress(BTHHandle,'BluetoothGetDeviceInfo');
    @BluetoothUpdateDeviceRecord:=GetProcAddress(BTHHandle,'BluetoothUpdateDeviceRecord');
    @BluetoothRemoveDevice:=GetProcAddress(BTHHandle,'BluetoothRemoveDevice');
    @BluetoothSelectDevices:=GetProcAddress(BTHHandle,'BluetoothSelectDevices');
    @BluetoothSelectDevicesFree:=GetProcAddress(BTHHandle,'BluetoothSelectDevicesFree');
    @BluetoothDisplayDeviceProperties:=GetProcAddress(BTHHandle,'BluetoothDisplayDeviceProperties');
    @BluetoothAuthenticateDevice:=GetProcAddress(BTHHandle,'BluetoothAuthenticateDevice');
    @BluetoothAuthenticateMultipleDevices:=GetProcAddress(BTHHandle,'BluetoothAuthenticateMultipleDevices');
    @BluetoothSetServiceState:=GetProcAddress(BTHHandle,'BluetoothSetServiceState');
    @BluetoothEnumerateInstalledServices:=GetProcAddress(BTHHandle,'BluetoothEnumerateInstalledServices');
    @BluetoothEnableDiscovery:=GetProcAddress(BTHHandle,'BluetoothEnableDiscovery');
    @BluetoothIsDiscoverable:=GetProcAddress(BTHHandle,'BluetoothIsDiscoverable');
    @BluetoothEnableIncomingConnections:=GetProcAddress(BTHHandle,'BluetoothEnableIncomingConnections');
    @BluetoothIsConnectable:=GetProcAddress(BTHHandle,'BluetoothIsConnectable');
    @BluetoothRegisterForAuthentication:=GetProcAddress(BTHHandle,'BluetoothRegisterForAuthentication');
    @BluetoothUnregisterAuthentication:=GetProcAddress(BTHHandle,'BluetoothUnregisterAuthentication');
    @BluetoothSendAuthenticationResponse:=GetProcAddress(BTHHandle,'BluetoothSendAuthenticationResponse');
    @BluetoothSdpGetElementData:=GetProcAddress(BTHHandle,'BluetoothSdpGetElementData');
    @BluetoothSdpGetContainerElementData:=GetProcAddress(BTHHandle,'BluetoothSdpGetContainerElementData');
    @BluetoothSdpGetAttributeValue:=GetProcAddress(BTHHandle,'BluetoothSdpGetAttributeValue');
    @BluetoothSdpGetString:=GetProcAddress(BTHHandle,'BluetoothSdpGetString');
    @BluetoothSdpEnumAttributes:=GetProcAddress(BTHHandle,'BluetoothSdpEnumAttributes');
  end;
  Result:=(BTHHandle<>0) and Assigned(BluetoothFindFirstRadio);
end;

procedure FreeBTHAPI;
begin
  if (BTHHandle<>0) and UnloadBTH then begin
    if not FreeLibrary(BTHHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[btapi,GetModuleHandle(btapi)]))
    else
      BTHHandle:=0;
  end;
end;


initialization
finalization
  FreeBTHAPI;
end.


