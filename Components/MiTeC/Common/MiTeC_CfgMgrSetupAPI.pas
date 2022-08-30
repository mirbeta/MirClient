{*******************************************************}
{                MiTeC Common Routines                  }
{              Config Manager Setup API                 }
{                                                       }
{          Copyright (c) 2009-2018 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_CfgMgrSetupApi;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows;
     {$ELSE}
     Windows;
     {$ENDIF}

const
  SetupApi = 'SetupApi.dll';
  cfgmgr = 'cfgmgr32.dll';

  GUID_DEVCLASS_APMSUPPORT: TGUID = '{D45B1C18-C8FA-11D1-9F77-0000F805F530}';
  GUID_DEVCLASS_COMPUTER: TGUID = '{4D36E966-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_DECODER: TGUID = '{6BDD1FC2-810F-11D0-BEC7-08002BE2092F}';
  GUID_DEVCLASS_VOLUMESNAPSHOT: TGUID = '{533C5B84-EC70-11D2-9505-00C04F79DEAF}';
  GUID_DEVCLASS_USB: TGUID = '{36FC9E60-C465-11CF-8056-444553540000}';
  GUID_DEVCLASS_LEGACYDRIVER: TGUID = '{8ECC055D-047F-11D1-A537-0000F8753ED1}';
  GUID_DEVCLASS_SOUND: TGUID = '{4d36e97c-e325-11ce-bfc1-08002be10318}'; //obsolete
  GUID_DEVCLASS_ADAPTER: TGUID = '{4d36e964-e325-11ce-bfc1-08002be10318}'; //obsolete

  GUID_DEVCLASS_BATTERY: TGUID = '{72631E54-78A4-11D0-BCF7-00AA00B7B32A}';
  GUID_DEVCLASS_BIOMETRIC: TGUID = '{53D29EF7-377C-4D14-864B-EB3A85769359}';
  GUID_DEVCLASS_BLUETOOTH: TGUID = '{E0CBF06C-CD8B-4647-BB8A-263B43F0F974}';
  GUID_DEVCLASS_CAMERA: TGUID = '{CA3E7AB9-B4C3-4AE6-8251-579EF933890F}';
  GUID_DEVCLASS_CDROM: TGUID = '{4D36E965-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_DISKDRIVE: TGUID = '{4D36E967-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_DISPLAY: TGUID = '{4D36E968-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_FDC: TGUID = '{4D36E969-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_FLOPPYDISK: TGUID = '{4D36E980-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_HDC: TGUID = '{4D36E96A-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_HIDCLASS: TGUID = '{745A17A0-74D3-11D0-B6FE-00A0C90F57DA}';
  GUID_DEVCLASS_DOT4: TGUID = '{48721B56-6795-11D2-B1A8-0080C72E74A2}';
  GUID_DEVCLASS_DOT4PRINT: TGUID = '{49CE6AC8-6F86-11D2-B1E5-0080C72E74A2}';
  GUID_DEVCLASS_61883: TGUID = '{7EBEFBC0-3200-11D2-B4C2-00A0C9697D07}';
  GUID_DEVCLASS_AVC: TGUID = '{C06FF265-AE09-48F0-812C-16753D7CBA83}';
  GUID_DEVCLASS_SBP2: TGUID = '{D48179BE-EC20-11D1-B6B8-00C04FA372A7}';
  GUID_DEVCLASS_1394: TGUID = '{6BDD1FC1-810F-11D0-BEC7-08002BE2092F}';
  GUID_DEVCLASS_IMAGE: TGUID = '{6BDD1FC6-810F-11D0-BEC7-08002BE2092F}';
  GUID_DEVCLASS_INFRARED: TGUID = '{6BDD1FC5-810F-11D0-BEC7-08002BE2092F}';
  GUID_DEVCLASS_KEYBOARD: TGUID = '{4D36E96B-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MEDIUMCHANGER: TGUID = '{CE5939AE-EBDE-11D0-B181-0000F8753EC4}';
  GUID_DEVCLASS_MTD: TGUID = '{4D36E970-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MODEM: TGUID = '{4D36E96D-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MONITOR: TGUID = '{4D36E96E-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MOUSE: TGUID = '{4D36E96F-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MULTIFUNCTION: TGUID = '{4D36E971-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MEDIA: TGUID = '{4D36E96C-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_MULTIPORTSERIAL: TGUID = '{50906CB8-BA12-11D1-BF5D-0000F805F530}';
  GUID_DEVCLASS_NET: TGUID = '{4D36E972-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_NETCLIENT: TGUID = '{4D36E973-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_NETSERVICE: TGUID = '{4D36E974-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_NETTRANS: TGUID = '{4D36E975-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_SECURITYACCELERATOR: TGUID = '{268C95A1-EDFE-11D3-95C3-0010DC4050A5}';
  GUID_DEVCLASS_PCMCIA: TGUID = '{4D36E977-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PORTS: TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PRINTER: TGUID = '{4D36E979-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PNPPRINTERS: TGUID = '{4658EE7E-F050-11D1-B6BD-00C04FA372A7}';
  GUID_DEVCLASS_PROCESSOR: TGUID = '{50127DC3-0F36-415E-A6CC-4CB3BE910B65}';
  GUID_DEVCLASS_SCSIADAPTER: TGUID = '{4D36E97B-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_SENSOR: TGUID = '{5175D334-C371-4806-B3BA-71FD53C9258D}';
  GUID_DEVCLASS_SMARTCARDREADER: TGUID = '{50DD5230-BA8A-11D1-BF5D-0000F805F530}';
  GUID_DEVCLASS_SOFTWARECOMPONENT: TGUID = '{5C4C3332-344D-483C-8739-259E934C9CC8}';
  GUID_DEVCLASS_VOLUME: TGUID = '{71A27CDD-812A-11D0-BEC7-08002BE2092F}';
  GUID_DEVCLASS_SYSTEM: TGUID = '{4D36E97D-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_TAPEDRIVE: TGUID = '{6D807884-7D21-11CF-801C-08002BE10318}';
  GUID_DEVCLASS_USBDEVICE: TGUID = '{88BAE032-5A81-49F0-BC3D-A4FF138216D6}';
  GUID_DEVCLASS_WCEUSBS: TGUID = '{25DBCE51-6C8F-4A72-8A6D-B54C2B4FC835}';
  GUID_DEVCLASS_WPD: TGUID = '{EEC5AD98-8080-425F-922A-DABF3DE3F69A}';
  GUID_DEVCLASS_SIDESHOW: TGUID = '{997B5D8D-C442-4F2E-BAF3-9C8E671E9E21}';

  GUID_DEVICE_MEMORY: TGUID = '{3FD0F03D-92E0-45FB-B75C-5ED8FFB01021}';
  GUID_DEVICE_PROCESSOR: TGUID = '{97FADB10-4E33-40AE-359C-8BEF029DBDD0}';
  GUID_BTHPORT_DEVICE_INTERFACE: TGUID = '{0850302A-B344-4FDA-9BE9-90576B8D46F0}';
  GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_DEVINTERFACE_USB_HOST_CONTROLLER: TGUID = '{3ABF6F2D-71C4-462A-8A92-1E6861E6AF27}';
  GUID_DEVINTERFACE_USB_HUB: TGUID = '{F18A0E88-C30C-11D0-8815-00A0C906BED8}';
  GUID_DEVINTERFACE_NET: TGUID = '{CAC88484-7515-4C03-82E6-71A87ABAC361}';
  GUID_DEVINTERFACE_KEYBOARD: TGUID = '{884B96C3-56EF-11D1-BC8C-00A0C91405DD}';
  GUID_DEVINTERFACE_MOUSE: TGUID = '{378DE44C-56EF-11D1-BC8C-00A0C91405DD}';
  GUID_DEVINTERFACE_DISPLAY_ADAPTER: TGUID = '{5B45201D-F2F2-4F3B-85BB-30FF1F953599}';
  GUID_DEVINTERFACE_MONITOR: TGUID = '{E6F07B5F-EE97-4A90-B076-33F57BF4EAA7}';
  GUID_DEVINTERFACE_DISK: TGUID = '{53F56307-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVINTERFACE_TAPE: TGUID = '{53F5630B-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVINTERFACE_CDROM: TGUID = '{53F56308-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVINTERFACE_WPD: TGUID = '{6AC27878-A6FA-4155-BA85-F98F491D4F33}';
  GUID_DEVINTERFACE_VOLUME: TGUID = '{53F5630D-B6BF-11D0-94F2-00A0C91EFB8B}';
  GUID_DEVINTERFACE_BLUETOOTH: TGUID = '{00F40965-E89D-4487-9890-87C3ABB211F4}';
  GUID_DEVINTERFACE_REMOVABLE_STORAGE: TGUID = '{F33FDC04-D1AC-4E8E-9A30-19BBD4B108AE}';
  GUID_DEVINTERFACE_SENSOR: TGUID = '{ba1bb692-9b7a-4833-9a1e-525ed134e7e2}';

type
  PDevPropKey = ^TDevPropKey;
  DEVPROPKEY = packed record
    fmtid: TGUID ;
    pid: Cardinal;
  end;
  TDevPropKey = DEVPROPKEY;

const
  ANYSIZE_ARRAY = 1;

  DIGCF_DEFAULT = $00000001;  // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT = $00000002;
  DIGCF_ALLCLASSES = $00000004;
  DIGCF_PROFILE = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

  SPDRP_DEVICEDESC = $00000000;  // DeviceDesc (R/W)
  SPDRP_HARDWAREID = $00000001;  // HardwareID (R/W)
  SPDRP_COMPATIBLEIDS = $00000002;  // CompatibleIDs (R/W)
  SPDRP_UNUSED0 = $00000003;  // unused
  SPDRP_SERVICE = $00000004;  // Service (R/W)
  SPDRP_UNUSED1 = $00000005;  // unused
  SPDRP_UNUSED2 = $00000006;  // unused
  SPDRP_CLASS = $00000007;  // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID = $00000008;  // ClassGUID (R/W)
  SPDRP_DRIVER = $00000009;  // Driver (R/W)
  SPDRP_CONFIGFLAGS = $0000000A;  // ConfigFlags (R/W)
  SPDRP_MFG = $0000000B;  // Mfg (R/W)
  SPDRP_FRIENDLYNAME = $0000000C;  // FriendlyName (R/W)
  SPDRP_LOCATION_INFORMATION = $0000000D;  // LocationInformation (R/W)
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E;  // PhysicalDeviceObjectName (R)
  SPDRP_CAPABILITIES = $0000000F;  // Capabilities (R)
  SPDRP_UI_NUMBER = $00000010;  // UiNumber (R)
  SPDRP_UPPERFILTERS = $00000011;  // UpperFilters (R/W)
  SPDRP_LOWERFILTERS = $00000012;  // LowerFilters (R/W)
  SPDRP_BUSTYPEGUID = $00000013;  // BusTypeGUID (R)
  SPDRP_LEGACYBUSTYPE = $00000014;  // LegacyBusType (R)
  SPDRP_BUSNUMBER = $00000015;  // BusNumber (R)
  SPDRP_ENUMERATOR_NAME = $00000016;  // Enumerator Name (R)
  SPDRP_SECURITY = $00000017;  // Security (R/W, binary form)
  SPDRP_SECURITY_SDS = $00000018;  // Security (W, SDS form)
  SPDRP_DEVTYPE = $00000019;  // Device Type (R/W)
  SPDRP_EXCLUSIVE = $0000001A;  // Device is exclusive-access (R/W)
  SPDRP_CHARACTERISTICS = $0000001B;  // Device Characteristics (R/W)
  SPDRP_ADDRESS = $0000001C;  // Device Address (R)
  SPDRP_UI_NUMBER_DESC_FORMAT = $0000001D;  // UiNumberDescFormat (R/W)
  SPDRP_DEVICE_POWER_DATA = $0000001E;  // Device Power Data (R)
  SPDRP_REMOVAL_POLICY = $0000001F;  // Removal Policy (R)
  SPDRP_REMOVAL_POLICY_HW_DEFAULT = $00000020;  // Hardware Removal Policy (R)
  SPDRP_REMOVAL_POLICY_OVERRIDE = $00000021;  // Removal Policy Override (RW)
  SPDRP_INSTALL_STATE = $00000022;  // Device Install State (R)
  SPDRP_LOCATION_PATHS = $00000023;  // Device Location Paths (R)
  SPDRP_MAXIMUM_PROPERTY = $00000024;  // Upper bound on ordinals

// Device properties
// These DEVPKEYs correspond to the SetupApi SPDRP_XXX device properties.
  DEVPKEY_Device_DeviceDesc: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 2);
  DEVPKEY_Device_HardwareIds: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 3);
  DEVPKEY_Device_CompatibleIds: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 4);
  DEVPKEY_Device_Service: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 6);
  DEVPKEY_Device_Class: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 9);
  DEVPKEY_Device_ClassGuid: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 10);
  DEVPKEY_Device_Driver: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 11);
  DEVPKEY_Device_ConfigFlags: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 12);
  DEVPKEY_Device_Manufacturer: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 13);
  DEVPKEY_Device_FriendlyName: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 14);
  DEVPKEY_Device_LocationInfo: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 15);
  DEVPKEY_Device_PDOName: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 16);
  DEVPKEY_Device_Capabilities: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 17);
  DEVPKEY_Device_UINumber: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 18);
  DEVPKEY_Device_UpperFilters: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 19);
  DEVPKEY_Device_LowerFilters: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 20);
  DEVPKEY_Device_BusTypeGuid: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 21);
  DEVPKEY_Device_LegacyBusType: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 22);
  DEVPKEY_Device_BusNumber: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 23);
  DEVPKEY_Device_EnumeratorName: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 24);
  DEVPKEY_Device_Security: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 25);
  DEVPKEY_Device_SecuritySDS: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 26);
  DEVPKEY_Device_DevType: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 27);
  DEVPKEY_Device_Exclusive: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 28);
  DEVPKEY_Device_Characteristics: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 29);
  DEVPKEY_Device_Address: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 30);
  DEVPKEY_Device_UINumberDescFormat: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 31);
  DEVPKEY_Device_PowerData: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 32);
  DEVPKEY_Device_RemovalPolicy: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 33);
  DEVPKEY_Device_RemovalPolicyDefault: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 34);
  DEVPKEY_Device_RemovalPolicyOverride: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 35);
  DEVPKEY_Device_InstallState: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 36);
  DEVPKEY_Device_LocationPaths: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 37);
  DEVPKEY_Device_BaseContainerId: TDEVPROPKEY = (fmtid: '{a45c254e-df1c-4efd-8020-67d146a850e0}'; pid: 38);

// Device and Device Interface property
// Common DEVPKEY used to retrieve the device instance id associated with devices and device interfaces.
  DEVPKEY_Device_InstanceId: TDEVPROPKEY = (fmtid: '{78c34fc8-104a-4aca-9ea4-524d52996e57}'; pid: 256);

// Device activity timestamp properties
  DEVPKEY_Device_InstallDate: TDEVPROPKEY = (fmtid: '{83da6326-97a6-4088-9453-a1923f573b29}'; pid: 100);   // DEVPROP_TYPE_FILETIME
  DEVPKEY_Device_FirstInstallDate: TDEVPROPKEY = (fmtid: '{83da6326-97a6-4088-9453-a1923f573b29}'; pid: 101);   // DEVPROP_TYPE_FILETIME
  DEVPKEY_Device_LastArrivalDate: TDEVPROPKEY = (fmtid: '{83da6326-97a6-4088-9453-a1923f573b29}'; pid: 102);   // DEVPROP_TYPE_FILETIME
  DEVPKEY_Device_LastRemovalDate: TDEVPROPKEY = (fmtid: '{83da6326-97a6-4088-9453-a1923f573b29}'; pid: 103);   // DEVPROP_TYPE_FILETIME

// Device driver properties
  DEVPKEY_Device_DriverDate: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 2);     // DEVPROP_TYPE_FILETIME
  DEVPKEY_Device_DriverVersion: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 3);     // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverDesc: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 4);    // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverInfPath: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 5);     // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverInfSection: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 6);     // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverInfSectionExt: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 7);     // DEVPROP_TYPE_STRING
  DEVPKEY_Device_MatchingDeviceId: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 8);    // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverProvider: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 9);    // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverPropPageProvider: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 10);   // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverCoInstallers: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 11);    // DEVPROP_TYPE_STRING_LIST
  DEVPKEY_Device_ResourcePickerTags: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 12);    // DEVPROP_TYPE_STRING
  DEVPKEY_Device_ResourcePickerExceptions: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 13);    // DEVPROP_TYPE_STRING
  DEVPKEY_Device_DriverRank: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 14);    // DEVPROP_TYPE_UINT32
  DEVPKEY_Device_DriverLogoLevel: TDEVPROPKEY = (fmtid: '{a8b865dd-2e3d-4094-ad97-e593a70c75d6}'; pid: 15);    // DEVPROP_TYPE_UINT32

// Device setup class properties
  DEVPKEY_DeviceClass_Name: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 2);      // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_ClassName: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 3);      // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_Icon: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 4);     // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_ClassInstaller: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 5);      // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_PropPageProvider: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 6);      // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_NoInstallClass: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 7);      // DEVPROP_TYPE_BOOLEAN
  DEVPKEY_DeviceClass_NoDisplayClass: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 8);      // DEVPROP_TYPE_BOOLEAN
  DEVPKEY_DeviceClass_SilentInstall: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 9);     // DEVPROP_TYPE_BOOLEAN
  DEVPKEY_DeviceClass_NoUseClass: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 10);     // DEVPROP_TYPE_BOOLEAN
  DEVPKEY_DeviceClass_DefaultService: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 11);     // DEVPROP_TYPE_STRING
  DEVPKEY_DeviceClass_IconPath: TDEVPROPKEY = (fmtid: '{259abffc-50a7-47ce-af08-68c9a7d73366}'; pid: 12);     // DEVPROP_TYPE_STRING_LIST

type
  {$if not defined(RAD5PLUS) and not defined(FPC)}
  ULONG_PTR = LongWord;
  {$ifend}

  HDEVINFO = THandle;

  PSPDevInfoData = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: ULONG_PTR;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  TSPDeviceInterfaceData = SP_DEVICE_INTERFACE_DATA;

const
  SPINT_ACTIVE  = $00000001;
  SPINT_DEFAULT = $00000002;
  SPINT_REMOVED = $00000004;

  POWER_SYSTEM_MAXIMUM = 7;

type
  TSPInterfaceDeviceData = TSPDeviceInterfaceData;
  PSPInterfaceDeviceData = PSPDeviceInterfaceData;

  TFiller = record
    {$IFDEF WIN64}
    Fill: array[0..1] of Byte;
    {$ENDIF WIN64}
  end;

  PSPDeviceInterfaceDetailDataA = ^TSPDeviceInterfaceDetailDataA;
  PSPDeviceInterfaceDetailDataW = ^TSPDeviceInterfaceDetailDataW;
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    DevicePath: array [0..ANYSIZE_ARRAY - 1] of AnsiChar;
    Filler: TFiller;
  end;

  SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    DevicePath: array [0..ANYSIZE_ARRAY - 1] of WideChar;
    Filler: TFiller;
  end;

  TSPDeviceInterfaceDetailDataA = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  TSPDeviceInterfaceDetailDataW = SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  {$IFDEF UNICODE}
  TSPDeviceInterfaceDetailData = TSPDeviceInterfaceDetailDataW;
  PSPDeviceInterfaceDetailData = PSPDeviceInterfaceDetailDataW;
  SP_DEVICE_INTERFACE_DETAIL_DATA = SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  {$ELSE}
  TSPDeviceInterfaceDetailData = TSPDeviceInterfaceDetailDataA;
  PSPDeviceInterfaceDetailData = PSPDeviceInterfaceDetailDataA;
  SP_DEVICE_INTERFACE_DETAIL_DATA = SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  {$ENDIF UNICODE}

  _DEVICE_POWER_STATE = (
    PowerDeviceUnspecified,
    PowerDeviceD0,
    PowerDeviceD1,
    PowerDeviceD2,
    PowerDeviceD3,
    PowerDeviceMaximum);
  DEVICE_POWER_STATE = _DEVICE_POWER_STATE;
  PDEVICE_POWER_STATE = ^DEVICE_POWER_STATE;
  TDevicePowerState = DEVICE_POWER_STATE;
  PDevicePowerState = PDEVICE_POWER_STATE;

  _SYSTEM_POWER_STATE = (
    PowerSystemUnspecified,
    PowerSystemWorking,
    PowerSystemSleeping1,
    PowerSystemSleeping2,
    PowerSystemSleeping3,
    PowerSystemHibernate,
    PowerSystemShutdown,
    PowerSystemMaximum);
  SYSTEM_POWER_STATE = _SYSTEM_POWER_STATE;
  PSYSTEM_POWER_STATE = ^SYSTEM_POWER_STATE;
  TSystemPowerState = SYSTEM_POWER_STATE;
  PSystemPowerState = PSYSTEM_POWER_STATE;

  TCMPowerData  = record
    PD_Size: DWORD;
    PD_MostRecentPowerState: DEVICE_POWER_STATE;
    PD_Capabilities,
    PD_D1Latency,
    PD_D2Latency,
    PD_D3Latency: ULONG;
    PD_PowerStateMapping: array [0..POWER_SYSTEM_MAXIMUM - 1] of DEVICE_POWER_STATE;
    PD_DeepestSystemWake: SYSTEM_POWER_STATE;
  end;

  PSPClassImageListData = ^TSPClassImageListData;
  SP_CLASSIMAGELIST_DATA = record
    cbSize: DWORD;
    ImageList: THandle;
    Reserved: ULONG_PTR;
  end;
  TSPClassImageListData = SP_CLASSIMAGELIST_DATA;


  function SetupDiGetClassDevsA(ClassGuid: PGUID; const Enumerator: PAnsiChar;
                                hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
                                external SetupApi name 'SetupDiGetClassDevsA';

  function SetupDiGetClassDevsW(ClassGuid: PGUID; const Enumerator: PWideChar;
                                hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
                                external SetupApi Name 'SetupDiGetClassDevsW';

  {$IFDEF UNICODE}
  function SetupDiGetClassDevs(ClassGuid: PGUID; const Enumerator: PWideChar;
                               hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
                               external SetupApi Name 'SetupDiGetClassDevsW';
  {$ELSE}
  function SetupDiGetClassDevs(ClassGuid: PGUID; const Enumerator: PAnsiChar;
                               hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
                               external SetupApi Name 'SetupDiGetClassDevsA';
  {$ENDIF}

  function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): LongBool; stdcall; external SetupApi;

  function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
                                       DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
                                       MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall;
                                       external SetupApi;

  function SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet: HDEVINFO;
                                            DeviceInterfaceData: PSPDeviceInterfaceData;
                                            DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
                                            DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
                                            Device: PSPDevInfoData): BOOL; stdcall;
                                            external SetupApi Name 'SetupDiGetDeviceInterfaceDetailA';

  function SetupDiGetDeviceInterfaceDetailW(DeviceInfoSet: HDEVINFO;
                                            DeviceInterfaceData: PSPDeviceInterfaceData;
                                            DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
                                            DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
                                            Device: PSPDevInfoData): BOOL; stdcall;
                                            external SetupApi Name 'SetupDiGetDeviceInterfaceDetailW';

  {$IFDEF UNICODE}
  function SetupDiGetDeviceInterfaceDetail(DeviceInfoSet: HDEVINFO;
                                           DeviceInterfaceData: PSPDeviceInterfaceData;
                                           DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataW;
                                           DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
                                           Device: PSPDevInfoData): BOOL; stdcall;
                                           external SetupApi Name 'SetupDiGetDeviceInterfaceDetailW';
  {$ELSE}
  function SetupDiGetDeviceInterfaceDetail(DeviceInfoSet: HDEVINFO;
                                           DeviceInterfaceData: PSPDeviceInterfaceData;
                                           DeviceInterfaceDetailData: PSPDeviceInterfaceDetailDataA;
                                           DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
                                           Device: PSPDevInfoData): BOOL; stdcall;
                                           external SetupApi Name 'SetupDiGetDeviceInterfaceDetailA';
  {$ENDIF}

  function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD;
                                 var DeviceInfoData: TSPDevInfoData): BOOL; stdcall;
                                 external SetupApi name 'SetupDiEnumDeviceInfo';

  function SetupDiGetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO;
                                             const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
                                             var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
                                             var RequiredSize: DWORD): BOOL; stdcall;
                                             external SetupApi name 'SetupDiGetDeviceRegistryPropertyA';

  function SetupDiGetDeviceRegistryPropertyW(DeviceInfoSet: HDEVINFO;
                                             const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
                                             var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
                                             var RequiredSize: DWORD): BOOL; stdcall;
                                             external SetupApi name 'SetupDiGetDeviceRegistryPropertyW';

  {$IFDEF UNICODE}
  function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO;
                                            const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
                                            var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
                                            var RequiredSize: DWORD): BOOL; stdcall;
                                            external SetupApi name 'SetupDiGetDeviceRegistryPropertyW';
  {$ELSE}
  function SetupDiGetDeviceRegistryProperty(DeviceInfoSet: HDEVINFO;
                                            const DeviceInfoData: TSPDevInfoData; Property_: DWORD;
                                            var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
                                            var RequiredSize: DWORD): BOOL; stdcall;
                                            external SetupApi name 'SetupDiGetDeviceRegistryPropertyA';
  {$ENDIF}

  {$IFDEF UNICODE}
  function SetupDiGetClassDescription(var ClassGuid: TGUID; ClassDescription: PChar;
                                     ClassDescriptionSize: DWORD; RequiredSize: PDWORD): LongBool; stdcall;
                                     external SetupApi name 'SetupDiGetClassDescriptionW';
  {$ELSE}
  function SetupDiGetClassDescription(var ClassGuid: TGUID; ClassDescription: PChar;
                                     ClassDescriptionSize: DWORD; RequiredSize: PDWORD): LongBool; stdcall;
                                     external SetupApi name 'SetupDiGetClassDescriptionA';
  {$ENDIF}

  {$IFDEF UNICODE}
  function SetupDiGetDeviceInstanceId(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData;
                                     DeviceInstanceId: PChar; DeviceInstanceIdSize: DWORD;
                                     RequiredSize: PDWORD): LongBool; stdcall;
                                     external SetupApi name 'SetupDiGetDeviceInstanceIdW';
  {$ELSE}
  function SetupDiGetDeviceInstanceId(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData;
                                     DeviceInstanceId: PChar; DeviceInstanceIdSize: DWORD;
                                     RequiredSize: PDWORD): LongBool; stdcall;
                                     external SetupApi name 'SetupDiGetDeviceInstanceIdA';
  {$ENDIF}

  {$IFDEF UNICODE}
  function SetupDiCreateDeviceInterface(DeviceInfoSet: HDEVINFO; var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
                                        const ReferenceString: PChar; CreationFlags: DWORD;
                                        DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
                                        external SetupApi name 'SetupDiCreateDeviceInterfaceW';
  {$ELSE}
  function SetupDiCreateDeviceInterface(DeviceInfoSet: HDEVINFO; var DeviceInfoData: TSPDevInfoData; var InterfaceClassGuid: TGUID;
                                        const ReferenceString: PChar; CreationFlags: DWORD;
                                        DeviceInterfaceData: PSPDeviceInterfaceData): BOOL; stdcall;
                                        external SetupApi name 'SetupDiCreateDeviceInterfaceA';
  {$ENDIF}

  function SetupDiLoadClassIcon(var ClassGuid: TGUID; var LargeIcon: HICON; var MiniIconIndex: integer): BOOL; stdcall; external SetupApi;

  function SetupDiGetClassImageList(var ClassImageListData: TSPClassImageListData): BOOL; stdcall; external SetupApi;

  function SetupDiGetClassImageIndex(var ClassImageListData: TSPClassImageListData; var ClassGuid: TGUID; var ImageIndex: Integer): BOOL; stdcall; external SetupApi;

  function SetupDiDestroyClassImageList(var ClassImageListData: TSPClassImageListData): BOOL; stdcall;  external SetupApi;

type
  TSetupDiGetDeviceProperty = function(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDEVINFODATA; const PropertyKey: PDEVPROPKEY;
                                    var PropertyType: Cardinal; PropertyBuffer:PBYTE; PropertyBufferSize:DWORD;
                                    RequiredSize:PDWORD; Flags:DWORD): BOOL; stdcall;

var
  SetupDiGetDeviceProperty: TSetupDiGetDeviceProperty = nil;

const
  CR_SUCCESS                  = $00000000;
  CR_DEFAULT                  = $00000001;
  CR_OUT_OF_MEMORY            = $00000002;
  CR_INVALID_POINTER          = $00000003;
  CR_INVALID_FLAG             = $00000004;
  CR_INVALID_DEVNODE          = $00000005;
  CR_INVALID_DEVINST          = CR_INVALID_DEVNODE;
  CR_INVALID_RES_DES          = $00000006;
  CR_INVALID_LOG_CONF         = $00000007;
  CR_INVALID_ARBITRATOR       = $00000008;
  CR_INVALID_NODELIST         = $00000009;
  CR_DEVNODE_HAS_REQS         = $0000000A;
  CR_DEVINST_HAS_REQS         = CR_DEVNODE_HAS_REQS;
  CR_INVALID_RESOURCEID       = $0000000B;
  CR_DLVXD_NOT_FOUND          = $0000000C;   // WIN 95 ONLY
  CR_NO_SUCH_DEVNODE          = $0000000D;
  CR_NO_SUCH_DEVINST          = CR_NO_SUCH_DEVNODE;
  CR_NO_MORE_LOG_CONF         = $0000000E;
  CR_NO_MORE_RES_DES          = $0000000F;
  CR_ALREADY_SUCH_DEVNODE     = $00000010;
  CR_ALREADY_SUCH_DEVINST     = CR_ALREADY_SUCH_DEVNODE;
  CR_INVALID_RANGE_LIST       = $00000011;
  CR_INVALID_RANGE            = $00000012;
  CR_FAILURE                  = $00000013;
  CR_NO_SUCH_LOGICAL_DEV      = $00000014;
  CR_CREATE_BLOCKED           = $00000015;
  CR_NOT_SYSTEM_VM            = $00000016;   // WIN 95 ONLY
  CR_REMOVE_VETOED            = $00000017;
  CR_APM_VETOED               = $00000018;
  CR_INVALID_LOAD_TYPE        = $00000019;
  CR_BUFFER_SMALL             = $0000001A;
  CR_NO_ARBITRATOR            = $0000001B;
  CR_NO_REGISTRY_HANDLE       = $0000001C;
  CR_REGISTRY_ERROR           = $0000001D;
  CR_INVALID_DEVICE_ID        = $0000001E;
  CR_INVALID_DATA             = $0000001F;
  CR_INVALID_API              = $00000020;
  CR_DEVLOADER_NOT_READY      = $00000021;
  CR_NEED_RESTART             = $00000022;
  CR_NO_MORE_HW_PROFILES      = $00000023;
  CR_DEVICE_NOT_THERE         = $00000024;
  CR_NO_SUCH_VALUE            = $00000025;
  CR_WRONG_TYPE               = $00000026;
  CR_INVALID_PRIORITY         = $00000027;
  CR_NOT_DISABLEABLE          = $00000028;
  CR_FREE_RESOURCES           = $00000029;
  CR_QUERY_VETOED             = $0000002A;
  CR_CANT_SHARE_IRQ           = $0000002B;
  CR_NO_DEPENDENT             = $0000002C;
  CR_SAME_RESOURCES           = $0000002D;
  CR_NO_SUCH_REGISTRY_KEY     = $0000002E;
  CR_INVALID_MACHINENAME      = $0000002F;   // NT ONLY
  CR_REMOTE_COMM_FAILURE      = $00000030;   // NT ONLY
  CR_MACHINE_UNAVAILABLE      = $00000031;   // NT ONLY
  CR_NO_CM_SERVICES           = $00000032;   // NT ONLY
  CR_ACCESS_DENIED            = $00000033;   // NT ONLY
  CR_CALL_NOT_IMPLEMENTED     = $00000034;
  CR_INVALID_PROPERTY         = $00000035;
  CR_DEVICE_INTERFACE_ACTIVE  = $00000036;
  CR_NO_SUCH_DEVICE_INTERFACE = $00000037;
  CR_INVALID_REFERENCE_STRING = $00000038;
  CR_INVALID_CONFLICT_LIST    = $00000039;
  CR_INVALID_INDEX            = $0000003A;
  CR_INVALID_STRUCTURE_SIZE   = $0000003B;
  NUM_CR_RESULTS              = $0000003C;

  PNP_VetoTypeUnknown          = 0;
  PNP_VetoLegacyDevice         = 1;
  PNP_VetoPendingClose         = 2;
  PNP_VetoWindowsApp           = 3;
  PNP_VetoWindowsService       = 4;
  PNP_VetoOutstandingOpen      = 5;
  PNP_VetoDevice               = 6;
  PNP_VetoDriver               = 7;
  PNP_VetoIllegalDeviceRequest = 8;
  PNP_VetoInsufficientPower    = 9;
  PNP_VetoNonDisableable       = 10;
  PNP_VetoLegacyDriver         = 11;
  PNP_VetoInsufficientRights   = 12;

type
  DEVINST = DWORD;
  CONFIGRET = DWORD;

  PPNP_VETO_TYPE = ^PNP_VETO_TYPE;
  PNP_VETO_TYPE = DWORD;

  function CM_Get_Device_ID_Size(var ulLen: ULONG;
  dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall; external cfgmgr name 'CM_Get_Device_ID_Size';

  function CM_Get_Device_IDA(dnDevInst: DEVINST; Buffer: PAnsiChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external cfgmgr name 'CM_Get_Device_IDA';
  function CM_Get_Device_IDW(dnDevInst: DEVINST; Buffer: PWideChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external cfgmgr name 'CM_Get_Device_IDW';
  {$IFDEF UNICODE}
  function CM_Get_Device_ID(dnDevInst: DEVINST; Buffer: PWideChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external cfgmgr name 'CM_Get_Device_IDW';
  {$ELSE}
  function CM_Get_Device_ID(dnDevInst: DEVINST; Buffer: PAnsiChar;
    BufferLen: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external cfgmgr name 'CM_Get_Device_IDA';
  {$ENDIF}

  function CM_Get_Parent(var dnDevInstParent: DEVINST;
    dnDevInst: DEVINST; ulFlags: ULONG): CONFIGRET; stdcall;
    external cfgmgr;

  function CM_Request_Device_EjectA(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external SetupApi;
  function CM_Request_Device_EjectW(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external SetupApi;
  {$IFDEF UNICODE}
  function CM_Request_Device_Eject(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PWideChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external SetupApi name 'CM_Request_Device_EjectW';
  {$ELSE}
  function CM_Request_Device_Eject(dnDevInst: DEVINST;
    pVetoType: PPNP_VETO_TYPE;     // OPTIONAL
    pszVetoName: PAnsiChar;        // OPTIONAL
    ulNameLength: ULONG; ulFlags: ULONG): CONFIGRET; stdcall; external SetupApi name 'CM_Request_Device_EjectA';
  {$ENDIF}

function GetBinary(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer; pData: Pointer; dwSize: DWORD): Boolean;
function GetDWORD(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): DWORD;
function GetGuid(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): TGUID;
function GetString(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): String;
function GetDeviceClassDescription(DeviceTypeGUID: TGUID): String;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.ComObj;
     {$ELSE}
     ComObj;
     {$ENDIF}

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

function ExtractMultiString(const Value: String): String;
var
  P: PChar;
begin
  P:=@Value[1];
  while P^ <> #0 do begin
    if Result<>'' then
      Result:=Result+', ';
    Result:=Result+P;
    Inc(P,lstrlen(P)+1);
  end;
end;

function GetBinary(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer; pData: Pointer; dwSize: DWORD): Boolean;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  dwRequiredSize:=0;
  dwPropertyRegDataType:=REG_BINARY;
  Result:=SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, pData, dwSize, dwRequiredSize);
end;

function GetDWORD(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): DWORD;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  Result:=0;
  dwRequiredSize:=4;
  dwPropertyRegDataType:=REG_DWORD;
  SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, @Result, dwRequiredSize, dwRequiredSize);
end;

function GetGuid(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): TGUID;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
  StringGUID: String;
begin
  ZeroMemory(@Result, SizeOf(TGUID));
  StringGUID:=GetString(ADeviceHandle,ADeviceData,APropertyCode);
  if StringGUID = '' then begin
    dwRequiredSize:=0;
    dwPropertyRegDataType:=REG_BINARY;
    SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, nil, 0, dwRequiredSize);
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
      SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, @Result, dwRequiredSize, dwRequiredSize);
  end else
    Result:=StringToGUID(StringGUID);
end;

function GetString(ADeviceHandle: HDEVINFO; ADeviceData: TSPDevInfoData; APropertyCode: Integer): String;
var
  dwPropertyRegDataType, dwRequiredSize: DWORD;
begin
  Result:='';
  dwRequiredSize:=0;
  dwPropertyRegDataType:=REG_SZ;
  SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, nil, 0, dwRequiredSize);
  if not (dwPropertyRegDataType in [REG_SZ, REG_MULTI_SZ]) then
    Exit;
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
    SetLength(Result, dwRequiredSize);
    SetupDiGetDeviceRegistryProperty(ADeviceHandle, ADeviceData, APropertyCode, dwPropertyRegDataType, @Result[1], dwRequiredSize, dwRequiredSize);
  end;
  case dwPropertyRegDataType of
    REG_SZ: Result:=PChar(Result);
    REG_MULTI_SZ: Result:=ExtractMultiString(Result);
  end;
end;

function GetDeviceClassDescription(DeviceTypeGUID: TGUID): String;
var
  dwRequiredSize: DWORD;
begin
  Result := '';
  dwRequiredSize := 0;
  SetupDiGetClassDescription(DeviceTypeGUID, nil, 0, @dwRequiredSize);
  if GetLastError = ERROR_INSUFFICIENT_BUFFER then begin
    SetLength(Result, dwRequiredSize);
    SetupDiGetClassDescription(DeviceTypeGUID, @Result[1], dwRequiredSize, @dwRequiredSize);
  end;
  Result := PChar(Result);
end;

initialization
  GetProcedureAddress(Pointer(@SetupDiGetDeviceProperty),setupapi,'SetupDiGetDevicePropertyW');
end.
