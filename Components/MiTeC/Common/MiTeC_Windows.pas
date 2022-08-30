{*******************************************************}
{                      MiTeC                            }
{              Windows declarations                     }
{                                                       }
{         Copyright (c) 2006-2018 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Windows;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils
     {$ELSE}
     Windows, SysUtils
     {$ENDIF};

const
  FILE_READ_DATA            = $0001; // file & pipe
  {$EXTERNALSYM FILE_READ_DATA}
  FILE_LIST_DIRECTORY       = $0001; // directory
  {$EXTERNALSYM FILE_LIST_DIRECTORY}

  FILE_WRITE_DATA           = $0002; // file & pipe
  {$EXTERNALSYM FILE_WRITE_DATA}
  FILE_ADD_FILE             = $0002; // directory
  {$EXTERNALSYM FILE_ADD_FILE}

  FILE_APPEND_DATA          = $0004; // file
  {$EXTERNALSYM FILE_APPEND_DATA}
  FILE_ADD_SUBDIRECTORY     = $0004; // directory
  {$EXTERNALSYM FILE_ADD_SUBDIRECTORY}
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  {$EXTERNALSYM FILE_CREATE_PIPE_INSTANCE}

  FILE_READ_EA = $0008; // file & directory
  {$EXTERNALSYM FILE_READ_EA}

  FILE_WRITE_EA = $0010; // file & directory
  {$EXTERNALSYM FILE_WRITE_EA}

  FILE_EXECUTE = $0020; // file
  {$EXTERNALSYM FILE_EXECUTE}
  FILE_TRAVERSE = $0020; // directory
  {$EXTERNALSYM FILE_TRAVERSE}

  FILE_DELETE_CHILD = $0040; // directory
  {$EXTERNALSYM FILE_DELETE_CHILD}

  FILE_READ_ATTRIBUTES = $0080; // all
  {$EXTERNALSYM FILE_READ_ATTRIBUTES}

  FILE_WRITE_ATTRIBUTES = $0100; // all
  {$EXTERNALSYM FILE_WRITE_ATTRIBUTES}

  FILE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  {$EXTERNALSYM FILE_ALL_ACCESS}

  FILE_GENERIC_READ = (STANDARD_RIGHTS_READ or FILE_READ_DATA or
    FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_READ}

  FILE_GENERIC_WRITE = (STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or
    FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_WRITE}

  FILE_GENERIC_EXECUTE = (STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or
    FILE_EXECUTE or SYNCHRONIZE);
  {$EXTERNALSYM FILE_GENERIC_EXECUTE}

  FILE_SHARE_READ                    = $00000001;
  {$EXTERNALSYM FILE_SHARE_READ}
  FILE_SHARE_WRITE                   = $00000002;
  {$EXTERNALSYM FILE_SHARE_WRITE}
  FILE_SHARE_DELETE                  = $00000004;
  {$EXTERNALSYM FILE_SHARE_DELETE}
  FILE_ATTRIBUTE_READONLY            = $00000001;
  {$EXTERNALSYM FILE_ATTRIBUTE_READONLY}
  FILE_ATTRIBUTE_HIDDEN              = $00000002;
  {$EXTERNALSYM FILE_ATTRIBUTE_HIDDEN}
  FILE_ATTRIBUTE_SYSTEM              = $00000004;
  {$EXTERNALSYM FILE_ATTRIBUTE_SYSTEM}
  FILE_ATTRIBUTE_DIRECTORY           = $00000010;
  {$EXTERNALSYM FILE_ATTRIBUTE_DIRECTORY}
  FILE_ATTRIBUTE_ARCHIVE             = $00000020;
  {$EXTERNALSYM FILE_ATTRIBUTE_ARCHIVE}
  FILE_ATTRIBUTE_DEVICE              = $00000040;
  {$EXTERNALSYM FILE_ATTRIBUTE_DEVICE}
  FILE_ATTRIBUTE_NORMAL              = $00000080;
  {$EXTERNALSYM FILE_ATTRIBUTE_NORMAL}
  FILE_ATTRIBUTE_TEMPORARY           = $00000100;
  {$EXTERNALSYM FILE_ATTRIBUTE_TEMPORARY}
  FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
  {$EXTERNALSYM FILE_ATTRIBUTE_SPARSE_FILE}
  FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
  {$EXTERNALSYM FILE_ATTRIBUTE_REPARSE_POINT}
  FILE_ATTRIBUTE_COMPRESSED          = $00000800;
  {$EXTERNALSYM FILE_ATTRIBUTE_COMPRESSED}
  FILE_ATTRIBUTE_OFFLINE             = $00001000;
  {$EXTERNALSYM FILE_ATTRIBUTE_OFFLINE}
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
  {$EXTERNALSYM FILE_ATTRIBUTE_NOT_CONTENT_INDEXED}
  FILE_ATTRIBUTE_ENCRYPTED           = $00004000;
  {$EXTERNALSYM FILE_ATTRIBUTE_ENCRYPTED}
  FILE_NOTIFY_CHANGE_FILE_NAME       = $00000001;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_FILE_NAME}
  FILE_NOTIFY_CHANGE_DIR_NAME        = $00000002;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_DIR_NAME}
  FILE_NOTIFY_CHANGE_ATTRIBUTES      = $00000004;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_ATTRIBUTES}
  FILE_NOTIFY_CHANGE_SIZE            = $00000008;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_SIZE}
  FILE_NOTIFY_CHANGE_LAST_WRITE      = $00000010;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_LAST_WRITE}
  FILE_NOTIFY_CHANGE_LAST_ACCESS     = $00000020;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_LAST_ACCESS}
  FILE_NOTIFY_CHANGE_CREATION        = $00000040;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_CREATION}
  FILE_NOTIFY_CHANGE_SECURITY        = $00000100;
  {$EXTERNALSYM FILE_NOTIFY_CHANGE_SECURITY}
  FILE_ACTION_ADDED                  = $00000001;
  {$EXTERNALSYM FILE_ACTION_ADDED}
  FILE_ACTION_REMOVED                = $00000002;
  {$EXTERNALSYM FILE_ACTION_REMOVED}
  FILE_ACTION_MODIFIED               = $00000003;
  {$EXTERNALSYM FILE_ACTION_MODIFIED}
  FILE_ACTION_RENAMED_OLD_NAME       = $00000004;
  {$EXTERNALSYM FILE_ACTION_RENAMED_OLD_NAME}
  FILE_ACTION_RENAMED_NEW_NAME       = $00000005;
  {$EXTERNALSYM FILE_ACTION_RENAMED_NEW_NAME}
  MAILSLOT_NO_MESSAGE                = DWORD(-1);
  {$EXTERNALSYM MAILSLOT_NO_MESSAGE}
  MAILSLOT_WAIT_FOREVER              = DWORD(-1);
  {$EXTERNALSYM MAILSLOT_WAIT_FOREVER}
  FILE_CASE_SENSITIVE_SEARCH         = $00000001;
  {$EXTERNALSYM FILE_CASE_SENSITIVE_SEARCH}
  FILE_CASE_PRESERVED_NAMES          = $00000002;
  {$EXTERNALSYM FILE_CASE_PRESERVED_NAMES}
  FILE_UNICODE_ON_DISK               = $00000004;
  {$EXTERNALSYM FILE_UNICODE_ON_DISK}
  FILE_PERSISTENT_ACLS               = $00000008;
  {$EXTERNALSYM FILE_PERSISTENT_ACLS}
  FILE_FILE_COMPRESSION              = $00000010;
  {$EXTERNALSYM FILE_FILE_COMPRESSION}
  FILE_VOLUME_QUOTAS                 = $00000020;
  {$EXTERNALSYM FILE_VOLUME_QUOTAS}
  FILE_SUPPORTS_SPARSE_FILES         = $00000040;
  {$EXTERNALSYM FILE_SUPPORTS_SPARSE_FILES}
  FILE_SUPPORTS_REPARSE_POINTS       = $00000080;
  {$EXTERNALSYM FILE_SUPPORTS_REPARSE_POINTS}
  FILE_SUPPORTS_REMOTE_STORAGE       = $00000100;
  {$EXTERNALSYM FILE_SUPPORTS_REMOTE_STORAGE}
  FILE_VOLUME_IS_COMPRESSED          = $00008000;
  {$EXTERNALSYM FILE_VOLUME_IS_COMPRESSED}
  FILE_SUPPORTS_OBJECT_IDS           = $00010000;
  {$EXTERNALSYM FILE_SUPPORTS_OBJECT_IDS}
  FILE_SUPPORTS_ENCRYPTION           = $00020000;
  {$EXTERNALSYM FILE_SUPPORTS_ENCRYPTION}
  FILE_NAMED_STREAMS                 = $00040000;
  {$EXTERNALSYM FILE_NAMED_STREAMS}
  FILE_READ_ONLY_VOLUME              = $00080000;
  {$EXTERNALSYM FILE_READ_ONLY_VOLUME}

  REG_QWORD                      = ( 11 ); // 64-bit number
  REG_QWORD_LITTLE_ENDIAN        = ( 11 ); // 64-bit number (same as REG_QWORD)

  {$IFDEF FPC}
  RESOURCEUSAGE_ATTACHED = $00000010;
  RESOURCEUSAGE_ALL = (RESOURCEUSAGE_CONNECTABLE or RESOURCEUSAGE_CONTAINER or RESOURCEUSAGE_ATTACHED);
  {$ENDIF}

  CAPTUREBLT = $40000000;

  {$IFNDEF D9PLUS}
  IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT      = 13;  { Delay Load Import Descriptors }
  IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR    = 14;  { COM Runtime descriptor }
  {$ENDIF}

  CACHE_FULLY_ASSOCIATIVE = $FF;

  SL_GEN_STATE_IS_GENUINE       = $00000000;
  SL_GEN_STATE_INVALID_LICENSE  = $00000001;
  SL_GEN_STATE_TAMPERED         = $00000002;
  SL_GEN_STATE_LAST             = $00000003;

  VER_NT_WORKSTATION       = $0000001;
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER            = $0000003;

  VER_SUITE_SMALLBUSINESS            = $00000001;
  VER_SUITE_ENTERPRISE               = $00000002;
  VER_SUITE_BACKOFFICE               = $00000004;
  VER_SUITE_COMMUNICATIONS           = $00000008;
  VER_SUITE_TERMINAL                 = $00000010;
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  VER_SUITE_EMBEDDEDNT               = $00000040;
  VER_SUITE_DATACENTER               = $00000080;
  VER_SUITE_SINGLEUSERTS             = $00000100;
  VER_SUITE_PERSONAL                 = $00000200;
  VER_SUITE_BLADE                    = $00000400;
  VER_SUITE_EMBEDDED_RESTRICTED      = $00000800;
  VER_SUITE_STORAGE_SERVER           = $00002000;
  VER_SUITE_COMPUTE_SERVER           = $00004000;
  VER_SUITE_WH_SERVER                = $00008000;
  VER_SUITENAME        = $0000040;
  VER_PRODUCT_TYPE     = $0000080;

  VER_EQUAL         = 1;
  VER_GREATER       = 2;
  VER_GREATER_EQUAL = 3;
  VER_LESS          = 4;
  VER_LESS_EQUAL    = 5;
  VER_AND           = 6;
  VER_OR            = 7;
  VER_CONDITION_MASK              = 7;
  VER_NUM_BITS_PER_CONDITION_MASK = 3;

  PRODUCT_BUSINESS                           = $00000006;
  PRODUCT_BUSINESS_N                         = $00000010;
  PRODUCT_CLUSTER_SERVER                     = $00000012;
  PRODUCT_DATACENTER_SERVER                  = $00000008;
  PRODUCT_DATACENTER_SERVER_CORE             = $0000000C;
  PRODUCT_DATACENTER_SERVER_CORE_V           = $00000027;
  PRODUCT_DATACENTER_SERVER_V                = $00000025;
  PRODUCT_SERVER_HYPER_CORE_V                = $00000040;
  PRODUCT_ENTERPRISE                         = $00000004;
  PRODUCT_ENTERPRISE_E                       = $00000046;
  PRODUCT_ENTERPRISE_N                       = $0000001B;
  PRODUCT_ENTERPRISE_SERVER                  = $0000000A;
  PRODUCT_ENTERPRISE_SERVER_CORE             = $0000000E;
  PRODUCT_ENTERPRISE_SERVER_CORE_V           = $00000029;
  PRODUCT_ENTERPRISE_SERVER_IA64             = $0000000F;
  PRODUCT_ENTERPRISE_SERVER_V                = $00000026;
  PRODUCT_HOME_BASIC                         = $00000002;
  PRODUCT_HOME_BASIC_E                       = $00000043;
  PRODUCT_HOME_BASIC_N                       = $00000005;
  PRODUCT_HOME_PREMIUM                       = $00000003;
  PRODUCT_HOME_PREMIUM_E                     = $00000044;
  PRODUCT_HOME_PREMIUM_N                     = $0000001A;
  PRODUCT_HYPERV                             = $0000002A;
  PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT   = $0000001E;
  PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING    = $00000020;
  PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY     = $0000001F;
  PRODUCT_PROFESSIONAL                       = $00000030;
  PRODUCT_PROFESSIONAL_E                     = $00000045;
  PRODUCT_PROFESSIONAL_N                     = $00000031;
  PRODUCT_SERVER_FOR_SMALLBUSINESS           = $00000018;
  PRODUCT_SERVER_FOR_SMALLBUSINESS_V         = $00000023;
  PRODUCT_SERVER_FOUNDATION                  = $00000021;
  PRODUCT_SMALLBUSINESS_SERVER               = $00000009;
  PRODUCT_STANDARD_SERVER                    = $00000007;
  PRODUCT_STANDARD_SERVER_CORE               = $0000000D;
  PRODUCT_STANDARD_SERVER_CORE_V             = $00000028;
  PRODUCT_STANDARD_SERVER_V                  = $00000024;
  PRODUCT_STARTER                            = $0000000B;
  PRODUCT_STARTER_E                          = $00000042;
  PRODUCT_STARTER_N                          = $0000002F;
  PRODUCT_STORAGE_ENTERPRISE_SERVER          = $00000017;
  PRODUCT_STORAGE_EXPRESS_SERVER             = $00000014;
  PRODUCT_STORAGE_STANDARD_SERVER            = $00000015;
  PRODUCT_STORAGE_WORKGROUP_SERVER           = $00000016;
  PRODUCT_UNDEFINED                          = $00000000;
  PRODUCT_ULTIMATE                           = $00000001;
  PRODUCT_ULTIMATE_E                         = $00000047;
  PRODUCT_ULTIMATE_N                         = $0000001C;
  PRODUCT_WEB_SERVER                         = $00000011;
  PRODUCT_WEB_SERVER_CORE                    = $0000001D;
  PRODUCT_CORE                               = $00000065; // Windows 10 Home
  PRODUCT_CORE_N                             = $00000062; // Windows 10 Home N
  PRODUCT_CORE_COUNTRYSPECIFIC               = $00000063; // Windows 10 Home China
  PRODUCT_CORE_SINGLELANGUAGE                = $00000064; // Windows 10 Home Single Language
  PRODUCT_MOBILE_CORE                        = $00000068; // Windows 10 Mobile
  PRODUCT_MOBILE_ENTERPRISE                  = $00000085; // Windows 10 Mobile Enterprise
  PRODUCT_EDUCATION                          = $00000079; // Windows 10 Education
  PRODUCT_EDUCATION_N                        = $0000007A; // Windows 10 Education N

  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  PROCESS_SUSPEND_RESUME = $0800;

  THREAD_SUSPEND_RESUME       = $0002;

  PROCESSOR_ARCHITECTURE_AMD64         = 9; //x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10; //WOW64
  PROCESSOR_ARCHITECTURE_IA64          = 6; //Intel Itanium Processor Family (IPF)
  PROCESSOR_ARCHITECTURE_INTEL         = 0; //x86
  PROCESSOR_ARCHITECTURE_UNKNOWN       = $ffff;

  SM_MEDIACENTER = 87;
  SM_SERVERR2 = 89;
  SM_STARTER = 88;
  SM_TABLETPC = 86;
  SM_REMOTESESSION = $1000;
  SM_SHUTTINGDOWN  = $2000;
  SM_REMOTECONTROL = $2001;

  KEY_WOW64_64KEY = $0100;

  CSIDL_APPDATA                   = $001a;
  CSIDL_COMMON_ALTSTARTUP         = $001e;
  CSIDL_COMMON_FAVORITES          = $001f;
  CSIDL_INTERNET_CACHE            = $0020;
  CSIDL_COOKIES                   = $0021;
  CSIDL_HISTORY                   = $0022;
  CSIDL_INTERNET                  = $0001;
  CSIDL_COMMON_DOCUMENTS          = $002E;
  CSIDL_COMMON_TEMPLATES          = $002D;
  CSIDL_MYMUSIC                   = $000D;
  CSIDL_MYDOCUMENTS               = $000C;
  CSIDL_FONTS                     = $0014;
  CSIDL_LOCAL_APPDATA             = $001C;
  CSIDL_COMMON_ADMINTOOLS         = $002F;
  CSIDL_ADMINTOOLS                = $0030;
  CSIDL_COMMON_APPDATA            = $0023;
  CSIDL_WINDOWS                   = $0024;
  CSIDL_SYSTEM                    = $0025;
  CSIDL_PROGRAM_FILES             = $0026;
  CSIDL_MYPICTURES                = $0027;
  CSIDL_PROFILE                   = $0028;
  CSIDL_PROGRAM_FILES_COMMON      = $002B;
  CSIDL_MYVIDEO                   = $000E;
  CSIDL_PERSONAL                  = $0005;
  CSIDL_COMMON_MUSIC              = $0035;
  CSIDL_COMMON_PICTURES           = $0036;
  CSIDL_COMMON_VIDEO              = $0037;
  CSIDL_CDBURN_AREA               = $003B;
  CSIDL_PROFILES                  = $003E;
  CSIDL_RECENT                    = $0008;

  FOLDERID_NetworkFolder: TGUID                  = '{D20BEEC4-5CA8-4905-AE3B-BF251EA09B53}';
  FOLDERID_ComputerFolder: TGUID                 = '{0AC0837C-BBF8-452A-850D-79D08E667CA7}';
  FOLDERID_InternetFolder: TGUID                 = '{4D9F7874-4E0C-4904-967B-40B0D20C3E4B}';
  FOLDERID_ControlPanelFolder: TGUID             = '{82A74AEB-AEB4-465C-A014-D097EE346D63}';
  FOLDERID_PrintersFolder: TGUID                 = '{76FC4E2D-D6AD-4519-A663-37BD56068185}';
  FOLDERID_SyncManagerFolder: TGUID              = '{43668BF8-C14E-49B2-97C9-747784D784B7}';
  FOLDERID_SyncSetupFolder: TGUID                = '{0F214138-B1D3-4A90-BBA9-27CBC0C5389A}';
  FOLDERID_ConflictFolder: TGUID                 = '{4BFEFB45-347D-4006-A5BE-AC0CB0567192}';
  FOLDERID_SyncResultsFolder: TGUID              = '{289A9A43-BE44-4057-A41B-587A76D7E7F9}';
  FOLDERID_RecycleBinFolder: TGUID               = '{B7534046-3ECB-4C18-BE4E-64CD4CB7D6AC}';
  FOLDERID_ConnectionsFolder: TGUID              = '{6F0CD92B-2E97-45D1-88FF-B0D186B8DEDD}';
  FOLDERID_Fonts: TGUID                          = '{FD228CB7-AE11-4AE3-864C-16F3910AB8FE}';
  FOLDERID_Desktop: TGUID                        = '{B4BFCC3A-DB2C-424C-B029-7FE99A87C641}';
  FOLDERID_Startup: TGUID                        = '{B97D20BB-F46A-4C97-BA10-5E3608430854}';
  FOLDERID_Programs: TGUID                       = '{A77F5D77-2E2B-44C3-A6A2-ABA601054A51}';
  FOLDERID_StartMenu: TGUID                      = '{625B53C3-AB48-4EC1-BA1F-A1EF4146FC19}';
  FOLDERID_Recent: TGUID                         = '{AE50C081-EBD2-438A-8655-8A092E34987A}';
  FOLDERID_SendTo: TGUID                         = '{8983036C-27C0-404B-8F08-102D10DCFD74}';
  FOLDERID_Documents: TGUID                      = '{FDD39AD0-238F-46AF-ADB4-6C85480369C7}';
  FOLDERID_Favorites: TGUID                      = '{1777F761-68AD-4D8A-87BD-30B759FA33DD}';
  FOLDERID_NetHood: TGUID                        = '{C5ABBF53-E17F-4121-8900-86626FC2C973}';
  FOLDERID_PrintHood: TGUID                      = '{9274BD8D-CFD1-41C3-B35E-B13F55A758F4}';
  FOLDERID_Templates: TGUID                      = '{A63293E8-664E-48DB-A079-DF759E0509F7}';
  FOLDERID_CommonStartup: TGUID                  = '{82A5EA35-D9CD-47C5-9629-E15D2F714E6E}';
  FOLDERID_CommonPrograms: TGUID                 = '{0139D44E-6AFE-49F2-8690-3DAFCAE6FFB8}';
  FOLDERID_CommonStartMenu: TGUID                = '{A4115719-D62E-491D-AA7C-E74B8BE3B067}';
  FOLDERID_PublicDesktop: TGUID                  = '{C4AA340D-F20F-4863-AFEF-F87EF2E6BA25}';
  FOLDERID_ProgramData: TGUID                    = '{62AB5D82-FDC1-4DC3-A9DD-070D1D495D97}';
  FOLDERID_CommonTemplates: TGUID                = '{B94237E7-57AC-4347-9151-B08C6C32D1F7}';
  FOLDERID_PublicDocuments: TGUID                = '{ED4824AF-DCE4-45A8-81E2-FC7965083634}';
  FOLDERID_RoamingAppData: TGUID                 = '{3EB685DB-65F9-4CF6-A03A-E3EF65729F3D}';
  FOLDERID_LocalAppData: TGUID                   = '{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}';
  FOLDERID_LocalAppDataLow: TGUID                = '{A520A1A4-1780-4FF6-BD18-167343C5AF16}';
  FOLDERID_InternetCache: TGUID                  = '{352481E8-33BE-4251-BA85-6007CAEDCF9D}';
  FOLDERID_Cookies: TGUID                        = '{2B0F765D-C0E9-4171-908E-08A611B84FF6}';
  FOLDERID_History: TGUID                        = '{D9DC8A3B-B784-432E-A781-5A1130A75963}';
  FOLDERID_System: TGUID                         = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}';
  FOLDERID_SystemX86: TGUID                      = '{D65231B0-B2F1-4857-A4CE-A8E7C6EA7D27}';
  FOLDERID_Windows: TGUID                        = '{F38BF404-1D43-42F2-9305-67DE0B28FC23}';
  FOLDERID_Profile: TGUID                        = '{5E6C858F-0E22-4760-9AFE-EA3317B67173}';
  FOLDERID_Pictures: TGUID                       = '{33E28130-4E1E-4676-835A-98395C3BC3BB}';
  FOLDERID_ProgramFilesX86: TGUID                = '{7C5A40EF-A0FB-4BFC-874A-C0F2E0B9FA8E}';
  FOLDERID_ProgramFilesCommonX86: TGUID          = '{DE974D24-D9C6-4D3E-BF91-F4455120B917}';
  FOLDERID_ProgramFilesX64: TGUID                = '{6D809377-6AF0-444B-8957-A3773F02200E}';
  FOLDERID_ProgramFilesCommonX64: TGUID          = '{6365D5A7-0F0D-45E5-87F6-0DA56B6A4F7D}';
  FOLDERID_ProgramFiles: TGUID                   = '{905E63B6-C1BF-494E-B29C-65B732D3D21A}';
  FOLDERID_ProgramFilesCommon: TGUID             = '{F7F1ED05-9F6D-47A2-AAAE-29D317C6F066}';
  FOLDERID_UserProgramFiles: TGUID               = '{5CD7AEE2-2219-4A67-B85D-6C9CE15660CB}';
  FOLDERID_UserProgramFilesCommon: TGUID         = '{BCBD3057-CA5C-4622-B42D-BC56DB0AE516}';
  FOLDERID_AdminTools: TGUID                     = '{724EF170-A42D-4FEF-9F26-B60E846FBA4F}';
  FOLDERID_CommonAdminTools: TGUID               = '{D0384E7D-BAC3-4797-8F14-CBA229B392B5}';
  FOLDERID_Music: TGUID                          = '{4BD8D571-6D19-48D3-BE97-422220080E43}';
  FOLDERID_Videos: TGUID                         = '{18989B1D-99B5-455B-841C-AB7C74E4DDFC}';
  FOLDERID_Ringtones: TGUID                      = '{C870044B-F49E-4126-A9C3-B52A1FF411E8}';
  FOLDERID_PublicPictures: TGUID                 = '{B6EBFB86-6907-413C-9AF7-4FC2ABF07CC5}';
  FOLDERID_PublicMusic: TGUID                    = '{3214FAB5-9757-4298-BB61-92A9DEAA44FF}';
  FOLDERID_PublicVideos: TGUID                   = '{2400183A-6185-49FB-A2D8-4A392A602BA3}';
  FOLDERID_PublicRingtones: TGUID                = '{E555AB60-153B-4D17-9F04-A5FE99FC15EC}';
  FOLDERID_ResourceDir: TGUID                    = '{8AD10C31-2ADB-4296-A8F7-E4701232C972}';
  FOLDERID_LocalizedResourcesDir: TGUID          = '{2A00375E-224C-49DE-B8D1-440DF7EF3DDC}';
  FOLDERID_CommonOEMLinks: TGUID                 = '{C1BAE2D0-10DF-4334-BEDD-7AA20B227A9D}';
  FOLDERID_CDBurning: TGUID                      = '{9E52AB10-F80D-49DF-ACB8-4330F5687855}';
  FOLDERID_UserProfiles: TGUID                   = '{0762D272-C50A-4BB0-A382-697DCD729B80}';
  FOLDERID_Playlists: TGUID                      = '{DE92C1C7-837F-4F69-A3BB-86E631204A23}';
  FOLDERID_SamplePlaylists: TGUID                = '{15CA69B3-30EE-49C1-ACE1-6B5EC372AFB5}';
  FOLDERID_SampleMusic: TGUID                    = '{B250C668-F57D-4EE1-A63C-290EE7D1AA1F}';
  FOLDERID_SamplePictures: TGUID                 = '{C4900540-2379-4C75-844B-64E6FAF8716B}';
  FOLDERID_SampleVideos: TGUID                   = '{859EAD94-2E85-48AD-A71A-0969CB56A6CD}';
  FOLDERID_PhotoAlbums: TGUID                    = '{69D2CF90-FC33-4FB7-9A0C-EBB0F0FCB43C}';
  FOLDERID_Public: TGUID                         = '{DFDF76A2-C82A-4D63-906A-5644AC457385}';
  FOLDERID_ChangeRemovePrograms: TGUID           = '{DF7266AC-9274-4867-8D55-3BD661DE872D}';
  FOLDERID_AppUpdates: TGUID                     = '{A305CE99-F527-492B-8B1A-7E76FA98D6E4}';
  FOLDERID_AddNewPrograms: TGUID                 = '{DE61D971-5EBC-4F02-A3A9-6C82895E5C04}';
  FOLDERID_Downloads: TGUID                      = '{374DE290-123F-4565-9164-39C4925E467B}';
  FOLDERID_PublicDownloads: TGUID                = '{3D644C9B-1FB8-4F30-9B45-F670235F79C0}';
  FOLDERID_SavedSearches: TGUID                  = '{7D1D3A04-DEBB-4115-95CF-2F29DA2920DA}';
  FOLDERID_QuickLaunch: TGUID                    = '{52A4F021-7B75-48A9-9F6B-4B87A210BC8F}';
  FOLDERID_Contacts: TGUID                       = '{56784854-C6CB-462B-8169-88E350ACB882}';
  FOLDERID_SidebarParts: TGUID                   = '{A75D362E-50FC-4FB7-AC2C-A8BEAA314493}';
  FOLDERID_SidebarDefaultParts: TGUID            = '{7B396E54-9EC5-4300-BE0A-2482EBAE1A26}';
  FOLDERID_PublicGameTasks: TGUID                = '{DEBF2536-E1A8-4C59-B6A2-414586476AEA}';
  FOLDERID_GameTasks: TGUID                      = '{054FAE61-4DD8-4787-80B6-090220C4B700}';
  FOLDERID_SavedGames: TGUID                     = '{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}';
  FOLDERID_Games: TGUID                          = '{CAC52C1A-B53D-4EDC-92D7-6B2E8AC19434}';
  FOLDERID_SEARCH_MAPI: TGUID                    = '{98EC0E18-2098-4D44-8644-66979315A281}';
  FOLDERID_SEARCH_CSC: TGUID                     = '{EE32E446-31CA-4ABA-814F-A5EBD2FD6D5E}';
  FOLDERID_Links: TGUID                          = '{BFB9D5E0-C6A9-404C-B2B2-AE6DB6AF4968}';
  FOLDERID_UsersFiles: TGUID                     = '{F3CE0F7C-4901-4ACC-8648-D5D44B04EF8F}';
  FOLDERID_UsersLibraries: TGUID                 = '{A302545D-DEFF-464B-ABE8-61C8648D939B}';
  FOLDERID_SearchHome: TGUID                     = '{190337D1-B8CA-4121-A639-6D472D16972A}';
  FOLDERID_OriginalImages: TGUID                 = '{2C36C0AA-5812-4B87-BFD0-4CD0DFB19B39}';
  FOLDERID_DocumentsLibrary: TGUID               = '{7B0DB17D-9CD2-4A93-9733-46CC89022E7C}';
  FOLDERID_MusicLibrary: TGUID                   = '{2112AB0A-C86A-4FFE-A368-0DE96E47012E}';
  FOLDERID_PicturesLibrary: TGUID                = '{A990AE9F-A03B-4E80-94BC-9912D7504104}';
  FOLDERID_VideosLibrary: TGUID                  = '{491E922F-5643-4AF4-A7EB-4E7A138D8174}';
  FOLDERID_RecordedTVLibrary: TGUID              = '{1A6FDBA2-F42D-4358-A798-B74D745926C5}';
  FOLDERID_HomeGroup: TGUID                      = '{52528A6B-B9E3-4ADD-B60D-588C2DBA842D}';
  FOLDERID_DeviceMetadataStore: TGUID            = '{5CE4A5E9-E4EB-479D-B89F-130C02886155}';
  FOLDERID_Libraries: TGUID                      = '{1B3EA5DC-B587-4786-B4EF-BD1DC332AEAE}';
  FOLDERID_PublicLibraries: TGUID                = '{48daf80b-e6cf-4f4e-b800-0e69d84ee384}';
  FOLDERID_UserPinned: TGUID                     = '{9E3995AB-1F9C-4F13-B827-48B24B6C7174}';
  FOLDERID_ImplicitAppShortcuts: TGUID           = '{BCB5256F-79F6-4CEE-B725-DC34E402FD46}';

  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  NORMAL_PRIORITY_CLASS = $00000020;

  THREAD_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF;
  THREAD_QUERY_INFORMATION = $0040;
  THREAD_QUERY_LIMITED_INFORMATION = $0800;
  THREAD_TERMINATE = $0001;

  SGUPP_DIRECTORY         = $1;
  SGUPP_DEFAULTDIRECTORY  = $2;
  SGUPP_CREATEPICTURESDIR = $80000000;


  FILE_NAME_NORMALIZED = 0;
  FILE_NAME_OPENED     = 8;

  VOLUME_NAME_DOS  = 0;
  VOLUME_NAME_GUID = 1;
  VOLUME_NAME_NONE = 4;
  VOLUME_NAME_NT   = 2;

  GR_GDIOBJECTS = 0;
  GR_GDIOBJECTS_PEAK = 2;
  GR_USEROBJECTS = 1;
  GR_USEROBJECTS_PEAK = 4;

  // NT defined privileges
  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME       = 'SeManageVolumePrivilege';

  // Group attributes
  SE_GROUP_MANDATORY          = ($00000001);
  SE_GROUP_ENABLED_BY_DEFAULT = ($00000002);
  SE_GROUP_ENABLED            = ($00000004);
  SE_GROUP_OWNER              = ($00000008);
  SE_GROUP_USE_FOR_DENY_ONLY  = ($00000010);
  SE_GROUP_INTEGRITY          = ($00000020);
  SE_GROUP_INTEGRITY_ENABLED  = ($00000040);
  SE_GROUP_LOGON_ID           = ($C0000000);
  SE_GROUP_RESOURCE           = ($20000000);

  LOGON_WITH_PROFILE = $00000001;

  RT_HTML       = MAKEINTRESOURCE(23);
  RT_MANIFEST   = MAKEINTRESOURCE(24);
  RT_REGISTRY   = MAKEINTRESOURCE(125);
  RT_TYPELIB    = MAKEINTRESOURCE(126);
  RT_AVI        = MAKEINTRESOURCE(127);

  rtCURSOR           = 1;
  rtBITMAP           = 2;
  rtICON             = 3;
  rtMENU             = 4;
  rtDIALOG           = 5;
  rtSTRING           = 6;
  rtFONTDIR          = 7;
  rtFONT             = 8;
  rtACCELERATOR      = 9;
  rtRCDATA           = 10;
  rtMESSAGETABLE     = 11;
  rtGROUP_CURSOR     = 12;
  rtGROUP_ICON       = 13;
  rtVERSION          = 16;
  rtDLGINCLUDE       = 17;
  rtPLUGPLAY         = 19;
  rtVXD              = 20;
  rtANICURSOR        = 21;
  rtANIICON          = 22;
  rtHTML             = 23;
  rtMANIFEST         = 24;
  rtREGISTRY         = 125;
  rtTYPELIB          = 126;
  rtAVI              = 127;

  cResources: array[0..23] of record
                                ID: PChar;
                                Name: string;
                              end = ((ID:RT_CURSOR; Name:'CURSOR'),
                                     (ID:RT_BITMAP; Name:'BITMAP'),
                                     (ID:RT_ICON; Name:'ICON'),
                                     (ID:RT_MENU; Name:'MENU'),
                                     (ID:RT_DIALOG; Name:'DIALOG'),
                                     (ID:RT_STRING; Name:'STRING'),
                                     (ID:RT_FONTDIR; Name:'FONTDIR'),
                                     (ID:RT_FONT; Name:'FONT'),
                                     (ID:RT_ACCELERATOR; Name:'ACCELERATOR'),
                                     (ID:RT_RCDATA; Name:'RCDATA'),
                                     (ID:RT_MESSAGETABLE; Name:'MESSAGETABLE'),
                                     (ID:RT_GROUP_CURSOR; Name:'GROUP_CURSOR'),
                                     (ID:RT_GROUP_ICON; Name:'GROUP_ICON'),
                                     (ID:RT_VERSION; Name:'VERSION'),
                                     (ID:RT_DLGINCLUDE; Name:'DLGINCLUDE'),
                                     (ID:RT_PLUGPLAY; Name:'PLUGPLAY'),
                                     (ID:RT_VXD; Name:'VXD'),
                                     (ID:RT_ANICURSOR; Name:'ANICURSOR'),
                                     (ID:RT_ANIICON; Name:'ANIICON'),
                                     (ID:RT_HTML; Name:'HTML'),
                                     (ID:RT_MANIFEST; Name:'MANIFEST'),
                                     (ID:RT_REGISTRY; Name:'REGISTRY'),
                                     (ID:RT_TYPELIB; Name:'TYPELIB'),
                                     (ID:RT_AVI; Name:'AVI'));

  IMAGE_SUBSYSTEM_WINDOWS_CE_GUI = 9;	//Windows CE
  IMAGE_SUBSYSTEM_EFI_APPLICATION =	10;	//An Extensible Firmware Interface (EFI) application
  IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER	= 11; //An EFI driver with boot services
  IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER = 12; //An EFI driver with run-time services
  IMAGE_SUBSYSTEM_EFI_ROM =	13;	//An EFI ROM image
  IMAGE_SUBSYSTEM_XBOX =14;	//XBOX

  IMAGE_FILE_MACHINE_I386 = $14c; //Intel 386
  IMAGE_FILE_MACHINE_R3000 = $162; //MIPS little-endian, 0x160 big-endian
  IMAGE_FILE_MACHINE_R4000 = $166; //MIPS little-endian
  IMAGE_FILE_MACHINE_R10000 = $168; //MIPS little-endian
  IMAGE_FILE_MACHINE_ALPHA = $184;  //Alpha_AXP
  IMAGE_FILE_MACHINE_POWERPC = $1F0; //IBM PowerPC Little-Endian
  IMAGE_FILE_MACHINE_IA64 = $0200; //Intel 64
  IMAGE_FILE_MACHINE_ALPHA64 = $0284; //Alpha_64
  IMAGE_FILE_MACHINE_AMD64 = $8664; //AMD64 (K8)
  IMAGE_FILE_MACHINE_AM33 = $1d3; //Matsushita AM33
  IMAGE_FILE_MACHINE_ARM = $1c0;  //ARM little endian
  IMAGE_FILE_MACHINE_ARMNT = $1c4; //ARMv7 (or higher) Thumb mode only
  IMAGE_FILE_MACHINE_ARM64 = $aa64; //ARMv8 in 64-bit mode
  IMAGE_FILE_MACHINE_EBC = $ebc;  //EFI byte code
  IMAGE_FILE_MACHINE_M32R = $9041; //Mitsubishi M32R little endian
  IMAGE_FILE_MACHINE_MIPS16 = $266; //MIPS16
  IMAGE_FILE_MACHINE_MIPSFPU = $366;  //MIPS with FPU
  IMAGE_FILE_MACHINE_MIPSFPU16 = $466; //MIPS16 with FPU
  IMAGE_FILE_MACHINE_POWERPCFP = $1f1; //Power PC with floating point support
  IMAGE_FILE_MACHINE_SH3 = $1a2; //Hitachi SH3
  IMAGE_FILE_MACHINE_SH3DSP = $1a3; //Hitachi SH3 DSP
  IMAGE_FILE_MACHINE_SH4 = $1a6; //Hitachi SH4
  IMAGE_FILE_MACHINE_SH5 = $1a8; //Hitachi SH5
  IMAGE_FILE_MACHINE_THUMB = $1c2; //ARM or Thumb (“interworking”)
  IMAGE_FILE_MACHINE_WCEMIPSV2 = $169; //MIPS little-endian WCE v2

  IMAGE_RESOURCE_NAME_IS_STRING    = Cardinal($80000000);
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = Cardinal($80000000);

  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $010B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $020B;

  IMAGE_ORDINAL_FLAG32 = Cardinal($80000000);
  IMAGE_ORDINAL_FLAG64 = int64($8000000000000000);

  IMAGE_DEBUG_TYPE_BORLAND    = 9;
  IMAGE_DEBUG_TYPE_RESERVED10 = 10;
  IMAGE_DEBUG_TYPE_CLSID      = 11;

  MNU_GRAYED         =$0001;   // 'GRAYED' keyword
  MNU_INACTIVE       =$0002;   // 'INACTIVE' keyword
  MNU_BITMAP         =$0004;   // 'BITMAP' keyword
  MNU_OWNERDRAW      =$0100;   // 'OWNERDRAW' keyword
  MNU_CHECKED        =$0008;   // 'CHECKED' keyword
  MNU_POPUP          =$0010;   // Used internally
  MNU_MENUBARBREAK   =$0020;   // 'MENUBARBREAK' keyword
  MNU_MENUBREAK      =$0040;   // 'MENUBREAK' keyword
  MNU_ENDMENU        =$0080;   // Used internally

  COMIMAGE_FLAGS_ILONLY               = $00000001;
  COMIMAGE_FLAGS_32BITREQUIRED        = $00000002;
  COMIMAGE_FLAGS_IL_LIBRARY           = $00000004;
  COMIMAGE_FLAGS_STRONGNAMESIGNED     = $00000008;
  COMIMAGE_FLAGS_TRACKDEBUGDATA       = $00010000;

  cMetadataTables: array[0..63] of string = ('00 - Module',
                                              '01 - TypeRef',
                                              '02 - TypeDef',
                                              '',
                                              '04 - Field',
                                              '',
                                              '06 - MethodDef',
                                              '',
                                              '08 - Param',
                                              '09 - InterfaceImpl',
                                              '10 - MemberRef',
                                              '11 - Constant',
                                              '12 - CustomAttribute',
                                              '13 - FieldMarshal',
                                              '14 - DeclSecurity',
                                              '15 - ClassLayout',
                                              '16 - FieldLayout',
                                              '17 - StandAloneSig',
                                              '18 - EventMap',
                                              '',
                                              '20 - Event',
                                              '21 - PropertyMap',
                                              '',
                                              '23 - Property',
                                              '24 - MethodSemantics',
                                              '25 - MethodImpl',
                                              '26 - ModuleRef',
                                              '27 - TypeSpec',
                                              '28 - ImplMap',
                                              '29 - FieldRVA',
                                              '',
                                              '',
                                              '32 - Assembly',
                                              '33 - AssemblyProcessor',
                                              '34 - AssemblyOS',
                                              '35 - AssemblyRef',
                                              '36 - AssemblyRefProcessor',
                                              '37 - AssemblyRefOS',
                                              '38 - File',
                                              '39 - ExportedType',
                                              '40 - ManifestResource',
                                              '41 - NestedClass',
                                               '','','','','','','','','','','',
                                               '','','','','','','','','','','');

type
  {$IFNDEF BDS3PLUS}
  PCardinal = ^Cardinal;
  {$ENDIF}
  NTSTATUS = Cardinal;
  PPointer = ^Pointer;
  PVOID = Pointer;
  USHORT = Word;
  LONG = Integer;
  TLUID = Int64;
  PLUID = ^TLUID;
  PBOOLEAN = ^Boolean;
  CCHAR = Byte;
  HANDLE = THandle;

  {$if not defined(RAD5PLUS) and not defined(FPC)}
  ULONGLONG = UInt64;
  TBytes = array of Byte;
  {$ifend}

  {$IFNDEF RAD9PLUS}
  PPVOID = ^PVOID;
  {$ENDIF}

  {$IFNDEF NATIVEINT}
  PNativeUInt = ^NativeUInt;
  NativeUInt = LongWord;
  NativeInt = LongInt;
  UINT_PTR = NativeUInt;
  INT_PTR = NativeInt;
  WPARAM = UINT_PTR;
  LPARAM = INT_PTR;
  LRESULT = INT_PTR;
  LONG_PTR = NativeInt;
  ULONG_PTR = NativeUInt;
  PULONG_PTR = ^ULONG_PTR;
  DWORD_PTR = ULONG_PTR;
  HHOOK = type UINT_PTR;
  HANDLE_PTR = type NativeUInt;
  SIZE_T = ULONG_PTR;
  SSIZE_T = LONG_PTR;
  ULONG64 = UInt64;
  PULONG64 = ^ULONG64;
  {$ENDIF}

  {$IFDEF FPC}
  SSIZE_T = LONG_PTR;
  {$ENDIF}

  LPBYTE = PBYTE;

  GUID = TGUID;
  LPGUID = ^GUID;
  CLSID = TGUID;

  LPVOID = Pointer;
  LPCVOID = Pointer;
  LPLPVOID = ^LPVOID;

  PSIZE_T = ^SIZE_T;
  PSSIZE_T = ^SSIZE_T;

  LPINT = PINT;

  PVOID64 = Pointer;

  LPLPSTR = ^LPSTR;
  LPLPCSTR = ^LPCSTR;
  LPLPCWSTR = ^LPCWSTR;
  LPLPWSTR = ^LPWSTR;
  LPLPCTSTR = ^LPCTSTR;

  LPFILETIME = PFILETIME;

  KAFFINITY = ULONG_PTR;

  {$IFNDEF D6PLUS}
  PCardinal = ^Cardinal;
  {$ENDIF}

  TImageOs2Header = record
    ne_magic: Word;        // Magic number
    ne_ver: Byte;          // Version number
    ne_rev: Byte;          // Revision number
    ne_enttab: Word;       // Offset of Entry Table
    ne_cbenttab: Word;     // Number of bytes in Entry Table
    ne_crc: cardinal;      // Checksum of whole file
    ne_progflags: byte;    // Program flags
    ne_appflags: byte;     // Application flags
    ne_autodata: byte;     // Automatic data segment index
    ne_heap: word;         // Initial heap allocation
    ne_stack: Word;        // Initial stack size
    ne_csip: cardinal;     // Initial CS:IP setting
    ne_sssp: cardinal;      // Initial SS:SP setting
    ne_cseg: Word;         // Count of file segments
    ne_cmod: Word;         // Entries in Module Reference Table
    ne_cbnrestab: Word;    // Size of non-resident name table
    ne_segtab: Word;       // Offset of Segment Table
    ne_rsrctab: Word;      // Offset of Resource Table
    ne_restab: Word;       // Offset of resident name table
    ne_modtab: Word;       // Offset of Module Reference Table
    ne_imptab: Word;       // Offset of Imported Names Table
    ne_nrestab: cardinal;   // Offset of Non-resident Names Table
    ne_cmovent: Word;      // Count of movable entries
    ne_align: Word;        // Segment alignment shift count
    ne_cres: Word;         // Count of resource segments
    ne_exetyp: Byte;       // Target Operating system
    ne_flagsothers: Byte;  // Other .EXE flags
    ne_pretthunks: Word;   // offset to return thunks
    ne_psegrefbytes: Word; // offset to segment ref. bytes
    ne_swaparea: Word;     // Minimum code swap area size
    ne_expver: Word;       // Expected Windows version number
  end;
  PImageOs2Header = ^TImageOs2Header;

  TImageVxdHeader = record
    e32_magic: Word;         // Magic number
    e32_border: Byte;        // The byte ordering for the VXD
    e32_worder: Byte;        // The word ordering for the VXD
    e32_level: cardinal;        // The EXE format level for now = 0
    e32_cpu: Word;           // The CPU type
    e32_os: Word;            // The OS type
    e32_ver: cardinal;          // Module version
    e32_mflags: cardinal;       // Module flags
    e32_mpages: cardinal;       // Module # pages
    e32_startobj: cardinal;     // Object # for instruction pointer
    e32_eip: cardinal;          // Extended instruction pointer
    e32_stackobj: cardinal;     // Object # for stack pointer
    e32_esp: cardinal;          // Extended stack pointer
    e32_pagesize: cardinal;     // VXD page size
    e32_lastpagesize: cardinal; // Last page size in VXD
    e32_fixupsize: cardinal;    // Fixup section size
    e32_fixupsum: cardinal;     // Fixup section checksum
    e32_ldrsize: cardinal;      // Loader section size
    e32_ldrsum: cardinal;       // Loader section checksum
    e32_objtab: cardinal;       // Object table offset
    e32_objcnt: cardinal;       // Number of objects in module
    e32_objmap: cardinal;       // Object page map offset
    e32_itermap: cardinal;      // Object iterated data map offset
    e32_rsrctab: cardinal;      // Offset of Resource Table
    e32_rsrccnt: cardinal;      // Number of resource entries
    e32_restab: cardinal;       // Offset of resident name table
    e32_enttab: cardinal;       // Offset of Entry Table
    e32_dirtab: cardinal;       // Offset of Module Directive Table
    e32_dircnt: cardinal;       // Number of module directives
    e32_fpagetab: cardinal;     // Offset of Fixup Page Table
    e32_frectab: cardinal;      // Offset of Fixup Record Table
    e32_impmod: cardinal;       // Offset of Import Module Name Table
    e32_impmodcnt: cardinal;    // Number of entries in Import Module Name Table
    e32_impproc: cardinal;      // Offset of Import Procedure Name Table
    e32_pagesum: cardinal;      // Offset of Per-Page Checksum Table
    e32_datapage: cardinal;     // Offset of Enumerated Data Pages
    e32_preload: cardinal;      // Number of preload pages
    e32_nrestab: cardinal;      // Offset of Non-resident Names Table
    e32_cbnrestab: cardinal;    // Size of Non-resident Name Table
    e32_nressum: cardinal;      // Non-resident Name Table Checksum
    e32_autodata: cardinal;     // Object # for automatic data object
    e32_debuginfo: cardinal;    // Offset of the debugging information
    e32_debuglen: cardinal;     // The length of the debugging info. in bytes
    e32_instpreload: cardinal;  // Number of instance pages in preload section of VXD file
    e32_instdemand: cardinal;   // Number of instance pages in demand load section of VXD file
    e32_heapsize: cardinal;     // Size of heap - for 16-bit apps
    e32_res3: array [0..11] of Byte;      // Reserved words
    e32_winresoff: cardinal;
    e32_winreslen: cardinal;
    e32_devid: Word;         // Device ID for VxD
    e32_ddkver: Word;        // DDK version for VxD
  end;
  PImageVxdHeader = ^TImageVxdHeader;

  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  TImageOptionalHeader64 = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Cardinal;
    SizeOfInitializedData: Cardinal;
    SizeOfUninitializedData: Cardinal;
    AddressOfEntryPoint: Cardinal;
    BaseOfCode: Cardinal;
    { NT additional fields. }
    ImageBase: Int64;
    SectionAlignment: Cardinal;
    FileAlignment: Cardinal;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: Cardinal;
    SizeOfImage: Cardinal;
    SizeOfHeaders: Cardinal;
    CheckSum: Cardinal;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
    LoaderFlags: Cardinal;
    NumberOfRvaAndSizes: Cardinal;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;

  PImageNtHeaders = ^TImageNtHeaders;
  TImageNtHeaders = packed record
    Signature: Cardinal;
    FileHeader: TImageFileHeader;
    Magic: Word;
  end;

  TImageLoadConfigDirectory32 = record
    Size: cardinal;
    TimeDateStamp: cardinal;
    MajorVersion: WORD;
    MinorVersion: WORD;
    GlobalFlagsClear: cardinal;
    GlobalFlagsSet: cardinal;
    CriticalSectionDefaultTimeout: cardinal;
    DeCommitFreeBlockThreshold: cardinal;
    DeCommitTotalFreeThreshold: cardinal;
    LockPrefixTable: cardinal;            // VA
    MaximumAllocationSize: cardinal;
    VirtualMemoryThreshold: cardinal;
    ProcessHeapFlags: cardinal;
    ProcessAffinityMask: cardinal;
    CSDVersion: WORD;
    Reserved1: WORD;
    EditList: cardinal;                   // VA
    SecurityCookie: cardinal;             // VA
    SEHandlerTable: cardinal;             // VA
    SEHandlerCount: cardinal;
  end;
  PImageLoadConfigDirectory32 = ^TImageLoadConfigDirectory32;

  TImageLoadConfigDirectory64 = record
    Size: Cardinal;
    TimeDateStamp: Cardinal;
    MajorVersion: WORD;
    MinorVersion: WORD;
    GlobalFlagsClear: Cardinal;
    GlobalFlagsSet: Cardinal;
    CriticalSectionDefaultTimeout: Cardinal;
    DeCommitFreeBlockThreshold: int64;
    DeCommitTotalFreeThreshold: int64;
    LockPrefixTable: int64;         // VA
    MaximumAllocationSize: int64;
    VirtualMemoryThreshold: int64;
    ProcessAffinityMask: int64;
    ProcessHeapFlags: Cardinal;
    CSDVersion: WORD;
    Reserved1: WORD;
    EditList: int64;                // VA
    SecurityCookie: int64;             // VA
    SEHandlerTable: int64;             // VA
    SEHandlerCount: int64;
  end;
  PImageLoadConfigDirectory64 = ^TImageLoadConfigDirectory64;

  TImageCOR20Header = record
    // Header versioning
    cb: Cardinal;
    MajorRuntimeVersion: word;
    MinorRuntimeVersion: word;
    // Symbol table and startup information
    MetaData: TImageDataDirectory;
    Flags: Cardinal;
// DDBLD - Added next section to replace following lin
// DDBLD - Still verifying, since not in NT SDK
// Cardinal EntryPointToken;

    // If COMIMAGE_FLAGS_NATIVE_ENTRYPOINT is not set, EntryPointToken represents a managed entrypoint.
    // If COMIMAGE_FLAGS_NATIVE_ENTRYPOINT is set, EntryPointRVA represents an RVA to a native entrypoint.
    EntryPointToken: cardinal;
    EntryPointRVA: cardinal;

// DDBLD - End of Added Area

    // Binding information
    Resources: TImageDataDirectory;
    StrongNameSignature: TImageDataDirectory;

    // Regular fixup and binding information
    CodeManagerTable: TImageDataDirectory;
    VTableFixups: TImageDataDirectory;
    ExportAddressTableJumps: TImageDataDirectory;

    // Precompiled image info (internal use only - set to zero)
    ManagedNativeHeader: TImageDataDirectory;
  end;
  PImageCOR20Header = ^TImageCOR20Header;

  TIIDUnion = record
    case Integer of
      0: (Characteristics: Cardinal);         // 0 for terminating null import descriptor
      1: (OriginalFirstThunk: Cardinal);      // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
  end;

  TImageImportDescriptor = record
    Union: TIIDUnion;
    TimeDateStamp: Cardinal;                  // 0 if not bound,
                                           // -1 if bound, and real date\time stamp
                                           //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                           // O.W. date/time stamp of DLL bound to (Old BIND)

    ForwarderChain: Cardinal;                 // -1 if no forwarders
    Name: Cardinal;
    FirstThunk: Cardinal;                     // RVA to IAT (if bound this IAT has actual addresses)
  end;
  PImageImportDescriptor = ^TImageImportDescriptor;

  TImageThunkData = record
    case Integer of
      0: (ForwarderString: Cardinal);   // PBYTE
      1: (Function_: Cardinal);         // PCardinal
      2: (Ordinal: Cardinal);
      3: (AddressOfData: Cardinal);     // PIMAGE_IMPORT_BY_NAME
  end;
  PImageThunkData = ^TImageThunkData;

  TImageThunkData64 = record
    case Integer of
      0: (ForwarderString: int64);   // PBYTE
      1: (Function_: int64);         // PCardinal
      2: (Ordinal: int64);
      3: (AddressOfData: int64);     // PIMAGE_IMPORT_BY_NAME
  end;
  PImageThunkData64 = ^TImageThunkData64;

  TImgDelayDescr = packed record
    grAttrs: Cardinal;                 // attributes
    szName: Cardinal;                  // pointer to dll name
    phmod: Cardinal;                  // address of module handle
    pIAT: Cardinal;          // address of the IAT
    pINT: Cardinal;          // address of the INT
    pBoundIAT: Cardinal;     // address of the optional bound IAT
    pUnloadIAT: Cardinal;    // address of optional copy of original IAT
    dwTimeStamp: Cardinal;             // 0 if not bound,
                                    // O.W. date/time stamp of DLL bound to (Old BIND)
  end;
  PImgDelayDescr = ^TImgDelayDescr;

  TImageBoundImportDescriptor = record
    TimeDateStamp: Cardinal;
    OffsetModuleName: Word;
    NumberOfModuleForwarderRefs: Word;
    // Array of zero or more IMAGE_BOUND_FORWARDER_REF follows
  end;
  PImageBoundImportDescriptor = ^TImageBoundImportDescriptor;

  TImageBoundForwarderRef = record
    TimeDateStamp: Cardinal;
    OffsetModuleName: Word;
    Reserved: Word;
  end;
  PImageBoundForwarderRef = ^TImageBoundForwarderRef;

  TImageImportByName = record
    Hint: Word;
    Name: array [0..0] of AnsiChar;
  end;
  PImageImportByName = ^TImageImportByName;

  TImageResourceDirectory = record
    Characteristics: cardinal;
    TimeDateStamp: cardinal;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
  end;
  PImageResourceDirectory = ^TImageResourceDirectory;

  TImageResourceDirectoryEntry = record
    case Integer of
      0: (
        // Cardinal NameOffset:31;
        // Cardinal NameIsString:1;
        NameOffset: Cardinal;
        OffsetToData: Cardinal
      );
      1: (
        Name: Cardinal;
        // Cardinal OffsetToDirectory:31;
        // Cardinal DataIsDirectory:1;
        OffsetToDirectory: Cardinal;
      );
      2: (
        Id: WORD;
      );
  end;
  PImageResourceDirectoryEntry = ^TImageResourceDirectoryEntry;

  TImageResourceDataEntry = record
    OffsetToData: Cardinal;
    Size: Cardinal;
    CodePage: Cardinal;
    Reserved: Cardinal;
  end;
  PImageResourceDataEntry = ^TImageResourceDataEntry;

  TImageResourceDirectoryString = record
    Length: Word;
    NameString: array [0..0] of CHAR;
  end;
  PImageResourceDirectoryString = ^TImageResourceDirectoryString;

  TImageResourceDirStringU = record
    Length: Word;
    NameString: array [0..0] of WCHAR;
  end;
  PImageResourceDirStringU = ^TImageResourceDirStringU;

  PAccelTableEntry = ^TAccelTableEntry;
  TAccelTableEntry = packed record
    fFlags: Word;
    wAnsi: Word;
    wId: Word;
    padding: Word;
  end;

  PMessageResourceBlock = ^TMessageResourceBlock;
  TMessageResourceBlock = packed record
    LowId: ULONG;
    HighId: ULONG;
    OffsetToEntries: ULONG;
  end;

  PMessageResourceData = ^TMessageResourceData;
  TMessageResourceData = packed record
    NumberOfBlocks: ULONG;
    // Blocks: array[0..0] of TMessageResourceBlock;
  end;

  PMessageResourceEntry = ^TMessageResourceEntry;
  TMessageResourceEntry = packed record
    Length: Word;
    Flags: Word;
    // Text: array[0..0] of Char;
  end;

  TImageFunctionEntry = record
    StartingAddress: cardinal;
    EndingAddress: cardinal;
    EndOfPrologue: cardinal;
  end;
  PImageFunctionEntry = ^TImageFunctionEntry;

  TImageFunctionEntry64 = record
    StartingAddress: int64;
    EndingAddress: int64;
    case Integer of
      0: (EndOfPrologue: int64);
      1: (UnwindInfoAddress: int64);
  end;
  PImageFunctionEntry64 = ^TImageFunctionEntry64;

  TImageTLSDirectory = record
    StartAddressOfRawData: cardinal;
    EndAddressOfRawData: cardinal;
    AddressOfIndex: cardinal;
    AddressOfCallbacks: Cardinal;
    SizeOfZeroFill: Cardinal;
    Characteristics: Cardinal;
  end;
  PImageTLSDirectory = ^TImageTLSDirectory;

  TImageTLSDirectory64 = record
    StartAddressOfRawData: int64;
    EndAddressOfRawData: int64;
    AddressOfIndex: int64;
    AddressOfCallbacks: int64;
    SizeOfZeroFill: Cardinal;
    Characteristics: Cardinal;
  end;
  PImageTLSDirectory64 = ^TImageTLSDirectory64;

  PDlgTemplateEx = ^TDlgTemplateEx;
  TDlgTemplateEx = packed record
    wDlgVer: word;           // use 1
    wSignature: word;        // use 0xFFFF
    dwHelpID: cardinal;         // Dialog's context help ID
    dwExtendedStyle: Cardinal;
    style: Cardinal;
    cdit: Word;
    x: SHORT;
    y: SHORT;
    cx: SHORT;
    cy: SHORT;
  end;

  PDlgItemTemplateEx = ^TDlgItemTemplateEx;
  TDlgItemTemplateEx = packed record
    dwHelpID: cardinal;
    dwExtendedStyle: Cardinal;
    style: Cardinal;
    x: SHORT;
    y: SHORT;
    cx: SHORT;
    cy: SHORT;
    id: Cardinal;
  end;

  {$IFNDEF RAD6PLUS}
  POSVersionInfoEx = ^TOSVersionInfoEx;
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: Cardinal;
    dwMajorVersion: Cardinal;
    dwMinorVersion: Cardinal;
    dwBuildNumber: Cardinal;
    dwPlatformId: Cardinal;
    szCSDVersion: array [0..127] of AnsiChar;
    wServicePackMajor: Word;
    wServicePackMinor: Word;
    wSuiteMask: Word;
    wProductType: Byte;
    wReserved: Byte;
  end;
  {$ENDIF}

  TMemoryStatusEx = record
    dwLength,
    dwMemoryLoad: Cardinal;
    ullTotalPhys,
    ullAvailPhys,
    ullTotalPageFile,
    ullAvailPageFile,
    ullTotalVirtual,
    ullAvailVirtual,
    ullAvailExtendedVirtual: int64;
  end;

  PMemoryStatusEx = ^TMemoryStatusEx;

  TOKEN_ELEVATION_TYPE = (TokenElevationTypePad0,
    TokenElevationTypeDefault, TokenElevationTypeFull,
    TokenElevationTypeLimited);

  SLID = TGUID;
  SL_GENUINE_STATE = Cardinal;

  TLogicalProcessorRelationship = (
    RelationProcessorCore = 0,
    RelationNumaNode = 1,
    RelationCache = 2,
    RelationProcessorPackage = 3,
    RelationGroup = 4,
    RelationAll = $FFFF
  );

  {$IFNDEF RAD9PLUS}
  TProcessorCacheType = (
    CacheUnified,
    CacheInstruction,
    CacheData,
    CacheTrace
  );

  TCacheDescriptor = record
    Level: Byte;
    Associativity: Byte;
    LineSize: Word;
    Size: DWORD;
    _Type: TProcessorCacheType;
  end;
  {$ENDIF}

  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;
  TSystemLogicalProcessorInformation = record
    ProcessorMask: ULONG_PTR;
    Relationship: TLogicalProcessorRelationship;
    case Integer of
      0: (Flags: Byte);
      1: (NodeNumber: DWORD);
      2: (Cache: TCacheDescriptor);
      3: (Reserved: array [0..1] of ULONGLONG);
  end;

  TFileInfoByHandleClass = (
    FileBasicInfo,
    FileStandardInfo,
    FileNameInfo,
    FileRenameInfo,
    FileDispositionInfo,
    FileAllocationInfo,
    FileEndOfFileInfo,
    FileStreamInfo,
    FileCompressionInfo,
    FileAttributeTagInfo,
    FileIdBothDirectoryInfo,
    FileIdBothDirectoryRestartInfo,
    FileIoPriorityHintInfo,
    FileRemoteProtocolInfo,
    FileFullDirectoryInfo,
    FileFullDirectoryRestartInfo,
    FileStorageInfo,
    FileAlignmentInfo,
    FileIdInfo,
    FileIdExtdDirectoryInfo,
    FileIdExtdDirectoryRestartInfo);

  MONITOR_DPI_TYPE = (
    MDT_EFFECTIVE_DPI = 0,
    MDT_ANGULAR_DPI = 1,
    MDT_RAW_DPI = 2,
    MDT_DEFAULT = MDT_EFFECTIVE_DPI);
  TMonitorDpiType = MONITOR_DPI_TYPE;

  PDisplayDeviceExA = ^TDisplayDeviceExA;
  PDisplayDeviceExW = ^TDisplayDeviceExW;
  PDisplayDeviceEx = PDisplayDeviceExW;
  _DISPLAY_DEVICEA = record
    cb: DWORD;
    DeviceName: array[0..31] of AnsiChar;
    DeviceString: array[0..127] of AnsiChar;
    StateFlags: DWORD;
    DeviceID: array[0..127] of AnsiChar;
    DeviceKey: array[0..127] of AnsiChar;
  end;
  _DISPLAY_DEVICEW = record
    cb: DWORD;
    DeviceName: array[0..31] of WideChar;
    DeviceString: array[0..127] of WideChar;
    StateFlags: DWORD;
    DeviceID: array[0..127] of WideChar;
    DeviceKey: array[0..127] of WideChar;
  end;
  _DISPLAY_DEVICE = _DISPLAY_DEVICEW;
  TDisplayDeviceExA = _DISPLAY_DEVICEA;
  TDisplayDeviceExW = _DISPLAY_DEVICEW;
  {$IFDEF UNICODE}
  TDisplayDeviceEx = TDisplayDeviceExW;
  {$ELSE}
  TDisplayDeviceEx = TDisplayDeviceExA;
  {$ENDIF}

const
  rsKernel = 'KERNEL32.DLL';
  rsShell = 'SHELL32.DLL';
  rsUSER32 = 'USER32.DLL';
  rsSLWGA = 'SLWGA.DLL';
  rsSHCORE = 'SHCORE.dll';
  rsIMAGEHLP = 'IMAGEHLP.DLL';
  rsSetProcessAffinityMask = 'SetProcessAffinityMask';
  rsGetProcessAffinityMask = 'GetProcessAffinityMask';
  rsOpenThread = 'OpenThread';
  rsGetNativeSystemInfo = 'GetNativeSystemInfo';
  rsIsWow64Process = 'IsWow64Process';
  rsVerSetConditionMask = 'VerSetConditionMask';
  rsVerifyVersionInfo = 'VerifyVersionInfo';
  rsProcessIdToSessionId = 'ProcessIdToSessionId';
  rsGetSystemTimes = 'GetSystemTimes';
  rsCreateProcessWithLogonW = 'CreateProcessWithLogonW';
  rsCreateProcessWithLogonA = 'CreateProcessWithLogonA';
  rsGetProductInfo = 'GetProductInfo';
  rsWPC_InstallState = 'WPC_InstallState';
  rsGlobalMemoryStatusEx = 'GlobalMemoryStatusEx';
  rsQueryFullProcessImageNameW = 'QueryFullProcessImageNameW';
  rsQueryFullProcessImageNameA = 'QueryFullProcessImageNameA';
  rsGetLongPathNameW = 'GetLongPathNameW';
  rsGetLongPathNameA = 'GetLongPathNameA';
  rsLockWorkStation= 'LockWorkStation';
  rsSLIsGenuineLocal = 'SLIsGenuineLocal';
  rsGhostWindowFromHungWindow = 'GhostWindowFromHungWindow';
  rsHungWindowFromGhostWindow = 'HungWindowFromGhostWindow';
  rsIsHungAppWindow = 'IsHungAppWindow';
  rsGetFinalPathNameByHandleW = 'GetFinalPathNameByHandleW';
  rsGetFinalPathNameByHandleA = 'GetFinalPathNameByHandleA';
  rsGetDpiForMonitor = 'GetDpiForMonitor';
  rsSHGetKnownFolderPath = 'SHGetKnownFolderPath';
  rsCheckSumMappedFile = 'CheckSumMappedFile';
  rsFindExecutable = 'FindExecutable'+{$IFDEF UNICODE}'W'{$ELSE}'A'{$ENDIF};

type
  TLoadLibraryA = function(lpLibFileName: PAnsiChar): HMODULE; stdcall;
  TGetProcAddress = function(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
  TFreeLibrary = function(hLibModule: HMODULE): BOOL; stdcall;

  TSetProcessAffinityMask = function(hProcess: THandle; dwProcessAffinityMask: DWORD_PTR): BOOL; stdcall;
  TGetProcessAffinityMask = function (hProcess: HANDLE; var lpProcessAffinityMask, lpSystemAffinityMask: DWORD_PTR): BOOL; stdcall;
  TOpenThread = function(dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwThreadId: DWORD): THANDLE; stdcall;
  TGetNativeSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
  TIsWow64Process =  function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  {$IFNDEF RAD14PLUS}
  TVerSetConditionMask = function(ConditionMask: int64; TypeMask: DWORD; Condition: BYTE): int64; stdcall;
  TVerifyVersionInfo = function(var lpVersionInformation: TOSVERSIONINFOEX; dwTypeMask: DWORD; dwlConditionMask: int64): BOOL; stdcall;
  {$ENDIF}
  TProcessIdToSessionId = function(ProcessID: Cardinal; var SessionID: Cardinal): Bool; stdcall;
  TGetSystemTimes = function(lpIdleTime, lpKernelTime, lpUserTime: PFILETIME): BOOL; stdcall;
  TGetProductInfo = function(dwOSMajorVersion,dwOSMinorVersion,dwSpMajorVersion,dwSpMinorVersion: DWORD;
                              var pdwReturnedProductType: DWORD): BOOL; stdcall;
  TWPC_InstallState = function(pdwState: PDWORD): HRESULT; stdcall;
  TGlobalMemoryStatusEx = function(lpBuffer: PMemoryStatusEx): BOOL; stdcall;
  {$IFDEF UNICODE}
  TCreateProcessWithLogon = function(lpUserName, lpDomain, lpPassword: PWChar;
                                      dwLogonFlags: DWord; lpApplicationName, lpCommandLine: PWChar;
                                      dwCreationFlags: DWord; lpEnvironment: Pointer; lpCurrentDirectory: PWChar;
                                      lpStartupInfo: PStartupInfo; lpProcessInfo: PProcessInformation): Boolean; stdcall;
  TGetLongPathName = function (lpszShortPath, lpszLongPath: PWideChar; cchBuffer: DWORD): DWORD; stdcall;
  TQueryFullProcessImageName = function(HProcess: THandle; dwFlags: DWORD; lpExeName: PWideChar; lpdwSize: PDWORD): integer; stdcall;
  TGetFinalPathNameByHandle = function (hFile: THandle; lpszFilePath: LPWSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;
  {$ELSE}
  TCreateProcessWithLogon = function(lpUserName, lpDomain, lpPassword: PAnsiChar;
                                      dwLogonFlags: DWord; lpApplicationName, lpCommandLine: PAnsiChar;
                                      dwCreationFlags: DWord; lpEnvironment: Pointer; lpCurrentDirectory: PAnsiChar;
                                      lpStartupInfo: PStartupInfo; lpProcessInfo: PProcessInformation): Boolean; stdcall;
  TGetLongPathName = function (lpszShortPath, lpszLongPath: PAnsiChar; cchBuffer: DWORD): DWORD; stdcall;
  TQueryFullProcessImageName = function(HProcess: THandle; dwFlags: DWORD; lpExeName: PAnsiChar; lpdwSize: PDWORD): integer; stdcall;
  TGetFinalPathNameByHandle = function(hFile: THandle; lpszFilePath: LPSTR; cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall;
  {$ENDIF}
  TGetFinalPathNameByHandleEx = function(hFile: THandle; FileInformationClass: TFileInfoByHandleClass; lpFileInformation: Pointer; dwBufferSize: DWORD): Boolean; stdcall;

  TLockWorkStation = function: boolean; stdcall;
  TSLIsGenuineLocal = function (var pAppId: SLID; var pGenuineState: SL_GENUINE_STATE; pUIOptions: Pointer): HRESULT; stdcall;

  TGhostWindowFromHungWindow =  function(AHWND: THandle): THandle; stdcall;
  THungWindowFromGhostWindow = function(AHWND: THandle): THandle; stdcall;
  TIsHungAppWindow = function (AHWND: THandle): Boolean; stdcall;

  TxpSHGetUserPicturePath = function(pUserOrPicName: PWideChar; sguppFlags: Cardinal; pwszPicPath: PWideChar): HRESULT;
  TSHGetUserPicturePath = function(pUserOrPicName: PWideChar; sguppFlags: Cardinal; pwszPicPath: PWideChar; picPathLen: Cardinal): HRESULT;
  TSHGetUserPicturePathEx = function(pwszUserOrPicName: PWideChar; sguppFlags: Cardinal; pwszDesiredSrcExt: PWideChar; pwszPicPath: PWideChar; picPathLen: Cardinal; pwszSrcPath: PWideChar; srcLen: Cardinal): HRESULT;

  TFindExecutable = function(lpFile: PChar;  lpDirectory: PChar; lpResult: PChar): integer; stdcall;

  TDisableProcessWindowsGhosting = function(): HRESULT;

  TGetLogicalProcessorInformation = function(Buffer: PSystemLogicalProcessorInformation; var ReturnLength: DWORD): BOOL; stdcall;

  {$IFNDEF FPC}
  TGetTickCount64 = function: uint64;
  {$ENDIF}

  TQueryProcessCycleTime = function(ProcessHandle: THandle; CycleTime: PULONG64): bool; stdcall;
  TQueryThreadCycleTime = function(ThreadHandle: THandle; CycleTime: PULONG64): bool; stdcall;

  TWow64DisableWow64FsRedirection = function(out OldValue: Pointer): bool; stdcall;
  TWow64RevertWow64FsRedirection = function(OldValue: Pointer): bool; stdcall;

  {$IFDEF FPC}
  TFNWndEnumProc = Pointer;
  TEnumWindows = function (lpEnumFunc: TFNWndEnumProc; lParam: LPARAM): BOOL; stdcall;
  {$ENDIF}

  TGetDpiForMonitor = function (hmonitor: THandle; dpiType: TMonitorDpiType; out dpiX: UINT; out dpiY: UINT): HRESULT; stdcall;

  {$IFNDEF RAD7PLUS}
  TSHGetKnownFolderPath = function (const rfid: TGUID; dwFlags: DWORD; hToken: THandle; var ppszPath: LPWSTR): HRESULT; stdcall;
  {$ENDIF}

  TGetGuiResources = function(hProcess: THandle; uiFlags: Cardinal): Cardinal; stdcall;

  TCheckSumMappedFile = function(BaseAddress: Pointer; FileLength: Cardinal; var HeaderSum: Cardinal; var CheckSum: Cardinal): PImageNtHeaders; stdcall;

  TSetThreadErrorMode = function(uNewMode: DWORD; var uOldMode: DWORD): BOOL; stdcall;

var
  SetProcessAffinityMask: TSetProcessAffinityMask;
  GetProcessAffinityMask: TGetProcessAffinityMask;
  OpenThread: TOpenThread;
  _GetNativeSystemInfo: TGetNativeSystemInfo;
  IsWow64Process: TIsWow64process;
  {$IFNDEF RAD14PLUS}
  VerSetConditionMask: TVerSetConditionMask;
  VerifyVersionInfo: TVerifyVersionInfo;
  {$ENDIF}
  ProcessIdToSessionId: TProcessIdToSessionId;
  GetSystemTimes: TGetSystemTimes;
  CreateProcessWithLogon: TCreateProcessWithLogon;
  GetProductInfo: TGetProductInfo;
  WPC_InstallState: TWPC_InstallState;
  GlobalMemoryStatusEx_: TGlobalMemoryStatusEx;
  GetLongPathName: TGetLongPathName;
  QueryFullProcessImageName: TQueryFullProcessImageName;
  LockWorkStation: TLockWorkStation;
  SLIsGenuineLocal: TSLIsGenuineLocal;
  GhostWindowFromHungWindow: TGhostWindowFromHungWindow = nil;
  HungWindowFromGhostWindow: THungWindowFromGhostWindow = nil;
  IsHungAppWindow: TIsHungAppWindow = nil;
  xpSHGetUserPicturePath: TxpSHGetUserPicturePath = nil;
  SHGetUserPicturePath: TSHGetUserPicturePath = nil;
  SHGetUserPicturePathEx: TSHGetUserPicturePathEx = nil;
  FindExecutable: TFindExecutable = nil;
  DisableProcessWindowsGhosting: TDisableProcessWindowsGhosting = nil;
  GetLogicalProcessorInformation: TGetLogicalProcessorInformation = nil;
  GetFinalPathNameByHandle: TGetFinalPathNameByHandle = nil;
  GetFileInformationByHandleEx: TGetFinalPathNameByHandleEx = nil;
  {$IFNDEF FPC}
  _GetTickCount64: TGetTickCount64 = nil;
  {$ENDIF}
  QueryThreadCycleTime: TQueryThreadCycleTime = nil;
  QueryProcessCycleTime: TQueryProcessCycleTime = nil;
  Wow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection = nil;
  Wow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection = nil;
  {$IFDEF FPC}
  EnumWindows: TEnumWindows = nil;
  {$ENDIF}
  GetDpiForMonitor: TGetDpiForMonitor = nil;
  {$IFNDEF RAD7PLUS}
  SHGetKnownFolderPath: TSHGetKnownFolderPath = nil;
  {$ENDIF}
  GetGuiResources: TGetGuiResources = nil;
  CheckSumMappedFile: TCheckSumMappedFile = nil;
  SetThreadErrorMode: TSetThreadErrorMode = nil;
  IsWow64: LongBool;

{$IFNDEF RAD6PLUS}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall; overload;
function GetVersionEx(var lpVersionInformation: TOSVersionInfo): BOOL; stdcall; overload;
{$ENDIF}
function ConvertSidToStringSid(Sid: PSID; var StringSid: {$IFDEF UNICODE}LPWSTR{$ELSE}LPTSTR{$ENDIF}): BOOL; stdcall;
{$IFNDEF FPC}
function GetTickCount64: UInt64;
{$ENDIF}
procedure GetNativeSystemInfo(var lpSystemInfo: TSystemInfo);

function EnumDisplayDevicesEx(Unused: Pointer; iDevNum: DWORD; var lpDisplayDevice: TDisplayDeviceEx; dwFlags: DWORD): BOOL; stdcall;
function EnumDisplayDevicesExA(Unused: Pointer; iDevNum: DWORD; var lpDisplayDevice: TDisplayDeviceExA; dwFlags: DWORD): BOOL; stdcall;
function EnumDisplayDevicesExW(Unused: Pointer; iDevNum: DWORD; var lpDisplayDevice: TDisplayDeviceExW; dwFlags: DWORD): BOOL; stdcall;

implementation

var
  Kernel32Handle, Shell32Handle, User32Handle, SLWGAHandle,
  SHCoreHandle,ImageHlpHandle: THandle;

{$IFNDEF RAD6PLUS}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall; external kernel32 name 'GetVersionExA';
function GetVersionEx(var lpVersionInformation: TOSVersionInfo): BOOL; stdcall; external kernel32 name 'GetVersionExA';
{$ENDIF}

{$IFDEF UNICODE}
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPWSTR): BOOL; stdcall; external 'advapi32.dll' name 'ConvertSidToStringSidW';
{$else}
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPTSTR): BOOL; stdcall; external 'advapi32.dll' name 'ConvertSidToStringSidA';
{$ENDIF}

{$IFNDEF FPC}
function GetTickCount64: UInt64;
begin
  if Assigned(_GetTickCount64) then
    Result:=_GetTickCount64
  else
    Result:=GetTickCount;
end;
{$ENDIF}

procedure GetNativeSystemInfo(var lpSystemInfo: TSystemInfo);
begin
  if Assigned(_GetNativeSystemInfo) then
    _GetNativeSystemInfo(lpSystemInfo)
  else
    GetSystemInfo(lpSystemInfo);
end;

{$IFDEF UNICODE}
function EnumDisplayDevicesEx; external user32 name 'EnumDisplayDevicesW';
{$ELSE}
function EnumDisplayDevicesEx; external user32 name 'EnumDisplayDevicesA';
{$ENDIF}
function EnumDisplayDevicesExA; external user32 name 'EnumDisplayDevicesA';
function EnumDisplayDevicesExW; external user32 name 'EnumDisplayDevicesW';

initialization
  SetProcessAffinityMask:=nil;
  GetProcessAffinityMask:=nil;

  Kernel32Handle:=GetModuleHandle(PChar(rsKernel));
  if Kernel32Handle=0 then
    Kernel32Handle:=LoadLibrary(PChar(rsKernel));
  if Kernel32Handle<>0 then begin
    SetProcessAffinityMask:=TSetProcessAffinityMask(GetProcAddress(Kernel32Handle,PChar(rsSetProcessAffinityMask)));
    GetProcessAffinityMask:=TGetProcessAffinityMask(GetProcAddress(Kernel32Handle,PChar(rsGetProcessAffinityMask)));
    OpenThread:=TOpenThread(GetProcAddress(Kernel32Handle,PChar(rsOpenThread)));
    _GetNativeSystemInfo:=TGetNativeSystemInfo(GetProcAddress(Kernel32Handle,PChar(rsGetNativeSystemInfo)));
    {$IFNDEF RAD14PLUS}
    VerSetConditionMask:=TVerSetConditionMask(GetProcAddress(Kernel32Handle,PChar(rsVerSetConditionMask)));
    VerifyVersionInfo:=TVerifyVersionInfo(GetProcAddress(Kernel32Handle,PChar(rsVerifyVersionInfo)));
    {$ENDIF}
    IsWOW64Process:=TIsWOW64Process(GetProcAddress(Kernel32Handle,PChar(rsIsWOW64Process)));
    ProcessIdToSessionId:=TProcessIdToSessionId(GetProcAddress(Kernel32Handle,PChar(rsProcessIdToSessionId)));
    GetSystemTimes:=TGetSystemTimes(GetProcAddress(Kernel32Handle,PChar(rsGetSystemTimes)));
    GetProductInfo:=TGetProductInfo(GetProcAddress(Kernel32Handle,PChar(rsGetProductInfo)));
    GlobalMemoryStatusEx_:=TGlobalMemoryStatusEx(GetProcAddress(Kernel32Handle,PChar(rsGlobalMemoryStatusEx)));
    {$IFDEF UNICODE}
    CreateProcessWithLogon:=TCreateProcessWithLogon(GetProcAddress(Kernel32Handle,PChar(rsCreateProcessWithLogonW)));
    GetLongPathName:=TGetLongPathName(GetProcAddress(Kernel32Handle,PChar(rsGetLongPathNameW)));
    QueryFullProcessImageName:=TQueryFullProcessImageName(GetProcAddress(Kernel32Handle,PChar(rsQueryFullProcessImageNameW)));
    GetFinalPathNameByHandle:=TGetFinalPathNameByHandle(GetProcAddress(Kernel32Handle,PChar(rsGetFinalPathNameByHandleW)));
    {$ELSE}
    GetLongPathName:=TGetLongPathName(GetProcAddress(Kernel32Handle,PChar(rsGetLongPathNameA)));
    CreateProcessWithLogon:=TCreateProcessWithLogon(GetProcAddress(Kernel32Handle,PChar(rsCreateProcessWithLogonA)));
    QueryFullProcessImageName:=TQueryFullProcessImageName(GetProcAddress(Kernel32Handle,PChar(rsQueryFullProcessImageNameA)));
    GetFinalPathNameByHandle:=TGetFinalPathNameByHandle(GetProcAddress(Kernel32Handle,PChar(rsGetFinalPathNameByHandleA)));
    {$ENDIF}
    GetFileInformationByHandleEx:=TGetFinalPathNameByHandleEx(GetProcAddress(Kernel32Handle,'GetFileInformationByHandleEx'));
    GetLogicalProcessorInformation:=TGetLogicalProcessorInformation(GetProcAddress(Kernel32Handle,'GetLogicalProcessorInformation'));
    {$IFNDEF FPC}
    _GetTickCount64:=TGetTickCount64(GetProcAddress(Kernel32Handle,'GetTickCount64'));
    {$ENDIF}
    QueryThreadCycleTime:=TQueryThreadCycleTime(GetProcAddress(Kernel32Handle,'QueryThreadCycleTime'));
    QueryProcessCycleTime:=TQueryProcessCycleTime(GetProcAddress(Kernel32Handle,'QueryProcessCycleTime'));
    Wow64DisableWow64FsRedirection:=TWow64DisableWow64FsRedirection(GetProcAddress(Kernel32Handle,'Wow64DisableWow64FsRedirection'));
    Wow64RevertWow64FsRedirection:=TWow64RevertWow64FsRedirection(GetProcAddress(Kernel32Handle,'Wow64RevertWow64FsRedirection'));
    SetThreadErrorMode:=TSetThreadErrorMode(GetProcAddress(Kernel32Handle,'SetThreadErrorMode'));
  end;

  User32Handle:=GetModuleHandle(PChar(rsUser32));
  if User32Handle=0 then
    User32Handle:=LoadLibrary(PChar(rsUser32));
  if User32Handle<>0 then begin
    LockWorkStation:=TLockWorkStation(GetProcAddress(User32Handle,PChar(rsLockWorkStation)));
    IsHungAppWindow:=TIsHungAppWindow(GetProcAddress(User32Handle,PChar(rsIsHungAppWindow)));
    HungWindowFromGhostWindow:=THungWindowFromGhostWindow(GetProcAddress(User32Handle,PChar(rsHungWindowFromGhostWindow)));
    GhostWindowFromHungWindow:=TGhostWindowFromHungWindow(GetProcAddress(User32Handle,PChar(rsGhostWindowFromHungWindow)));
    DisableProcessWindowsGhosting:=TDisableProcessWindowsGhosting(GetProcAddress(User32Handle,'DisableProcessWindowsGhosting'));
    {$IFDEF FPC}
    EnumWindows:=TEnumWindows(GetProcAddress(User32Handle,'EnumWindows'));
    {$ENDIF}
    GetGuiResources:=TGetGuiResources(GetProcAddress(User32Handle,'GetGuiResources'));
  end;

  ImageHlpHandle:=GetModuleHandle(PChar(rsImageHlp));
  if ImageHlpHandle=0 then
    ImageHlpHandle:=LoadLibrary(PChar(rsImageHlp));
  if ImageHlpHandle<>0 then begin
    CheckSumMappedFile:=TCheckSumMappedFile(GetProcAddress(ImageHlpHandle,PChar(rsCheckSumMappedFile)));
  end;

  SLWGAHandle:=GetModuleHandle(PChar(rsSLWGA));
  if SLWGAHandle=0 then
    SLWGAHandle:=LoadLibrary(PChar(rsSLWGA));
  if SLWGAHandle<>0 then begin
    SLIsGenuineLocal:=TSLIsGenuineLocal(GetProcAddress(SLWGAHandle,PChar(rsSLIsGenuineLocal)));
  end;

  SHCoreHandle:=GetModuleHandle(PChar(rsSHCORE));
  if SHCoreHandle=0 then
    SHCoreHandle:=LoadLibrary(PChar(rsSHCORE));
  if SHCoreHandle<>0 then begin
    GetDpiForMonitor:=TGetDpiForMonitor(GetProcAddress(SHCoreHandle,PChar(rsGetDpiForMonitor)));
  end;

  Shell32Handle:=GetModuleHandle(PChar(rsShell));
  if Shell32Handle=0 then
    Shell32Handle:=LoadLibrary(PChar(rsShell));
  if Shell32Handle<>0 then begin
    WPC_InstallState:=TWPC_InstallState(GetProcAddress(Shell32Handle,MAKEINTRESOURCE(859)));
    FindExecutable:=TFindExecutable(GetProcAddress(Shell32Handle,PChar(rsFindExecutable)));
    if Win32MajorVersion<6 then
      xpSHGetUserPicturePath:=TxpSHGetUserPicturePath(GetProcAddress(Shell32Handle,MAKEINTRESOURCE(233)))
    else begin
      SHGetUserPicturePath:=TSHGetUserPicturePath(GetProcAddress(Shell32Handle,MAKEINTRESOURCE(261)));
      SHGetUserPicturePathEx:=TSHGetUserPicturePathEx(GetProcAddress(Shell32Handle,MAKEINTRESOURCE(810)));
    end;
    {$IFNDEF RAD7PLUS}
    SHGetKnownFolderPath:=TSHGetKnownFolderPath(GetProcAddress(Shell32Handle,PChar(rsSHGetKnownFolderPath)));
    {$ENDIF}
  end;

  if Assigned(IsWow64Process) then
    IsWow64Process(GetCurrentProcess,IsWow64)
  else
    IsWow64:=False;
end.


