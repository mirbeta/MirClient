{*******************************************************************}
{ TWebUpdate component                                              }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1998 - 2015                                        }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WUpdate;

{$I TMSDEFS.INC}

{$DEFINE USEUAC}

{$IFDEF USEUAC}
{$R WUPDATE.RES}
{$ENDIF}

{$IFNDEF USEUAC}
{$R WUPDATENO_UAC.RES}
{$ENDIF}

{$IFDEF WIN64}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.a"'}
{$HPPEMIT ''}
{$ENDIF}

{$IFDEF WIN32}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$HPPEMIT '#pragma link "urlmon.lib"'}
{$HPPEMIT ''}
{$ENDIF}

{$IFDEF WIN32}
{$IFDEF ISDELPHI}
{$DEFINE USEURLMON}
{$ENDIF}
{$ENDIF}

{$R WUPDENG.RES}
// USE THIS RESOURCE FILE FOR FRENCH LANGUAGE         
// {$R WUPDFRE.RES}

{$IFDEF DELPHI7_LVL}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

{$DEFINE noTMSDEBUG}

uses
  StdCtrls, Messages, Classes, SysUtils, WinInet, Windows, Forms, Registry,
  IniFiles, Dialogs, ShellApi, Controls, LZExpand, CabFdi, WuCRC32, WuBase64,
  ComCtrls, ShlObj, WULogin, Variants
  {$IFNDEF USEURLMON}
  , ActiveX
  {$ENDIF}
  {$IFDEF DELPHIXE_LVL}
  , KnownFolders
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  {constants used for status}
  WebUpdateSuccess        = 0;
  WebUpdateAccessError    = 1;
  WebUpdateNotFound       = 2;
  WebUpdateInformation    = 3;
  WebUpdateNoNewVersion   = 4;
  WebUpdateNewVersion     = 5;
  WebUpdateHTTPStatus     = 6;
  WebUpdateHTMLDialog     = 7;
  WebUpdateCABError       = 8;
  WebUpdateSpawnFail      = 9;
  WebUpdateWrongSource    = 10;
  WebUpdateSignatureError = 11;
  WebUpdateWhatsNew       = 12;
  WebUpdateEUL            = 13;
  WebUpdateWhatsnewCancel = 14;
  WebUpdateEULACancel     = 15;
  WebUpdatePostConnectFail= 16;
  WebUpdatePostPostFail   = 17;
  WebUpdateExecAndWait    = 18;
  WebUpdateReplaceError   = 19;
  WebUpdateRenameError    = 19;
  WebUpdateDownloadError  = 20;
  WebUpdateUndefined      = $FF;

  {constants for errors}
  ErrControlFileNotFound  = 0;
  ErrUpdateFileNotFound   = 1;
  ErrUpdateFileZeroLen    = 2;
  ErrUpdateTargetEqual    = 3;
  ErrUpdateSignatureError = 4;
  ErrConnectError         = 5;
  ErrCannotDeleteFile     = 6;
  ErrCannotRenameFile     = 7;
  ErrCannotChangeDir      = 8;
  ErrDownloadError        = 9;
  ErrDownloadSizeWrong    = 10;
  ErrUndefined            = $FF;

  WU_FAILED             = -1;
  WU_SUCCESS            = 0;
  WU_NOCONNECTION       = 1;
  WU_INTERNETOPENFAILED = 2;
  WU_DIALUPFAILED       = 3;
  WU_FILENOTFOUND       = 4;
  WU_SIGNATUREFAILED    = 5;

  WU_DATEBASEDNEWVERSION        = 6;
  WU_NONEWVERSION               = 7;
  WU_UNCONDITIONALNEWVERSION    = 8;
  WU_FILESIZEBASEDNEWVERSION    = 9;
  WU_CHECKSUMBASEDNEWVERSION    = 10;
  WU_VERSIONINFOBASEDNEWVERSION = 11;
  WU_CUSTOMNEWVERSION           = 12;

  MAJ_VER = 2; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 12; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.6.4.0 : changed what's new & EULA memos to wordwrap = true
  //         : changed StartButton on wizard to disabled during version check
  //         : changed cursor on wizard to crHourGlass during version check
  // 1.6.5.0 : Hungarian, Swedish & Norwegian language support added
  //         : fix with version check added
  //         : logfile name customizable
  //         : updates with time info shown in wizard
  // 1.6.6.0 : Improvement in handling failed downloads
  // 1.6.7.0 : Trigger OnSuccess event from wizard fixed
  // 1.6.8.0 : French messages added (contributed by Pierre Yager)
  // 1.7.0.0 : OnBeforePost event added
  //         : WebUpdate.OnSuccess triggered when wizard is used after successful update
  //         : View in Notepad popup added in wizard for Whats New and EULA
  //         : Fixed issue with CAB extraction for CAB files with multi level hierarchies
  // 1.7.0.1 : Fixed issue with OnDownloadedWhatsNew, OnDownloadedEULA from wizard
  // 1.7.1.0 : New : support for Czech language added
  // 1.7.2.0 : Improved : added support for handling updates for readonly & hidden files
  // 1.7.3.0 : New : Polish language support
  // 1.7.4.0 : Improved : What's new information is downloaded when OnDownloadedWhatsnew is assigned
  //           from the DoVersionCheck function
  // 2.0.0.0 : New : support for Windows Vista
  //         : New : support for Delphi 2007 & C++Builder 2007
  //         : New : {doc} prefix
  //         : New : support for Hungarian, Czech, Swedish & Polish language added
  //         : New : parameter InitPath added to DoUpdate method to automatically initialize current directory to app EXE
  //         : Improved : logging
  //         : Various smaller improvements & fixes
  // 2.0.0.1 : Fixed : issue with targetdir during file download
  // 2.0.1.0 : New method NewWhatsNew(ShowDialog) method available to only show what's new info
  // 2.0.1.1 : Fixed : issue with overwriting files during updates
  // 2.0.1.2 : Fixed : memory leak in wizard
  // 2.0.2.0 : New : method WebUpdateWizard.CloseWizard added
  // 2.0.3.0 : Improved : handling of temp directory names with spaces
  // 2.0.3.1 : Fixed : issue with handling updates with _NEW extension on Windows Vista
  // 2.0.3.2 : Improved : automatic LIB reference in HPP file generation
  // 2.0.3.3 : Fixed : issue with command line parameters for app restart
  // 2.0.3.4 : Improved : cleanup of file WUPDATE.INI
  // 2.0.4.0 : New : property TimeOut added
  // 2.0.5.0 : New : event OnBeforeDownload triggered before getting "What's new" file.
  // 2.0.5.1 : Improved : french language version of wizard
  // 2.0.6.0 : New : Preselect=0|1 per file possible to control preselect in wizard checklistbox
  // 2.0.7.0 : New : status events when file after download cannot be deleted/renamed
  // 2.0.7.1 : Improved : will automatically handle URLs with '?' used
  // 2.0.7.2 : Improved : Fix for Vista UAC with runbefore/runafter commands
  // 2.0.8.0 : Improved : new status messages for changing FTP directory
  // 2.0.9.0 : Improved : error logging for FTP based updates
  // 2.0.9.1 : Fixed : issue with sub .INF file processing for Windows Vista
  // 2.0.9.2 : Improved : cancel handling
  // 2.0.10.0: New : check for Delphi 2009 / C++Builder 2009 for logging
  // 2.0.10.1: Fixed : issue with Windows Vista & Delphi 2009
  // 2.0.10.2: Improved : suppress messagebox for server connection problems when UpdateUpdate = wuuSilent
  // 2.0.10.3: Improved : Cancel handling from HandleActions stage
  // 2.0.11.0: New : property ForceUpdate added
  // 2.0.12.0: Improved : support to allow using custom what's new & EULA handling via wizard
  // 2.0.12.1: Fixed : issue with Cancel clearing when WUpdate restarts
  // 2.0.12.2: Fixed : issue in wizard button location when translated text is used
  // 2.0.13.0: New: built-in support for Turkish language in dialogs
  // 2.0.14.0: New: property ReceiveTimeOut added

  // 2.1.0.0 : New: UpdateBuilder
  //         : New: Support for rich text What's New & EULA files
  //         : New: Exposed billboard position, stretch, center in TWebUpdateWizard
  //         : Improved: handling of the WebUpdateWizard with silent restart

  // 2.1.0.1 : Improved : small change for URL parameter in OnFileNameFromURL event
  // 2.1.0.2 : Fixed : When UseWinTempDir is false and file is not a .CAB or .PAT file, keep file
  // 2.1.0.3 : Fixed : Issue with PostUpdateInfo with Unicode Delphi
  // 2.1.0.4 : Fixed : Issue with cancelling and restarting the wizard
  // 2.1.0.5 : Fixed : Wizard shows dummy description instead of blank text when no description is defined
  // 2.1.1.0 : New : Property ShowDownloadProgress added
  // 2.1.2.0 : Improved : Display of new date based version update in wizard
  //         : Changed : Default parameters for DoUpdate() / Wizard.Execute call
  // 2.1.3.0 : New : Added support to specify local application data folder for downloads
  //         : New : optional parameter in NewVersionAvailable to initialize path for this call
  // 2.1.3.1 : Fixed : Issue with DateSeparator/TimeSeparator in persisting last update timestamp to registry
  // 2.1.3.2 : Fixed : Get proper Program Files folder for 32bit apps on 64bit Windows
  // 2.1.3.3 : Improved : Handling of HTTP POST for secure sites
  // 2.1.3.4 : Improved : Handling of setting UseWinTempDir
  // 2.1.3.5 : Fixed : Issue with mandatory files where description is missing
  // 2.1.3.6 : Fixed : Issue with loading TXT EULAs in wizard
  // 2.1.4.0 : New : CertCheck property added to disable certificate checks
  // 2.1.4.1 : Fixed : Issue with sequence of status events
  // 2.1.5.0 : Improved : Internally use TMemIniFile instead of TIniFile for speed
  // 2.1.5.1 : Fixed : Issue with wizard form in Delphi 7
  // 2.2.0.0 : New : OnSetAppParamBefore, OnSetAppParamAfter events added
  //         : New : Possibility to turn off UAC prompt (registered version only)
  // 2.2.0.1 : Fixed : Issue with using TWebUpdate with UAC turned off
  // 2.2.0.2 : Fixed : Issue with error handling when disconnect happens during update download
  // 2.2.0.3 : Fixed : Issue with using TWebUpdate with UAC turned off
  // 2.2.0.4 : Improved : Handling of .INF files with mandatory file lists and missing description
  // 2.2.0.5 : Fixed : Issue with localization of download progress form
  // 2.2.0.6 : Fixed : Issue with CAB extraction in 64bit EXEs
  // 2.2.0.7 : Improved : Handling date based updates with specific machine date format settings
  // 2.2.1.0 : New : Greek version of wizard added
  // 2.2.2.0 : New : property AutoRestart added
  // 2.2.3.0 : New : Support for Catalan language added
  // 2.2.4.0 : New : Extra check & extra OnStatus message for downloads where filesize of download is incorrect
  //         : Improved : Progressbar positioning on progressform for use in apps with skinning
  // 2.2.4.1 : Improved : Handling updates when retrieving the checksum fails
  // 2.2.4.2 : Fixed : Issue with NewVersionAvailable function for unconditional updates
  // 2.2.4.3 : Improved : Handling of config files for use with services
  // 2.2.5.0 : New : Delphi XE5 & C++Builder XE5 support
  // 2.2.5.1 : Fixed : Issue with triggered OnDownloadEULA
  // 2.2.5.2 : Fixed : Issue for 64bit for C++
  // 2.2.5.3 : Fixed : Issue with checksum version comparison
  // 2.2.6.0 : New : APPDATA prefix support added for installing files in the common data folder
  //         : New : Public property HideURLInLogFile added
  //         : New : Support for Russian language
  // 2.2.7.0 : New : Delphi XE6 & C++Builder XE6 support
  // 2.2.7.1 : Improved : Handling of general update description
  // 2.2.8.0 : New : Delphi XE7 & C++Builder XE7 support
  // 2.2.8.1 : Fixed : Issue with checksum update verification
  // 2.2.9.0 : Improved : More accurate operating system version reporting
  // 2.2.9.1 : Fixed : Rare IO error case with handling logging
  // 2.2.9.2 : Fixed : Issue with VersionCheck = vcUpdateOnly
  // 2.2.10.0: New : Public property UACEnabled added
  // 2.2.11.0: New : Unmanisfested operating system version retrieval
  // 2.2.12.0: New : Delphi 10 Seattle & C++Builder 10 Seattle support

type
  TWebUpdateAuthentication = (waAlways, waAuto, waNever);

  TWebUpdateEvent = procedure(Sender: TObject) of object;

  TWebUpdateFileProgress = procedure(Sender: TObject; FileName:string; Pos,Size: Longint) of object;
  TWebUpdateProgress = procedure(Sender: TObject; Action:string) of object;
  TWebUpdateProgressCancel = procedure(Sender: TObject; var Cancel: Boolean) of object;

  TWebUpdateFileDownloaded = procedure(Sender: TObject; FileName:string) of object;

  TWebUpdateBeforePost = procedure(Sender: TObject;var AllowPost: Boolean) of object;
  TWebUpdateProcessPostResult = procedure(Sender: TObject;var AllowPostResult: Boolean) of object;

  TWebUpdateStatus = procedure(Sender: TObject;StatusStr: string; StatusCode,ErrCode: Integer) of object;
  TWebUpdateThreadDone = procedure(Sender: TObject) of object;
  TWebUpdateRestart = procedure(Sender: TObject; var Allow:Boolean) of object;

  TWebUpdateCustomValidate = procedure(Sender: TObject; Msg,Param:string; var Allow:Boolean) of object;
  TWebUpdateCustomProcess = procedure(Sender: TObject; Msg,Param:string) of object;

  TWebUpdateSetParams = procedure(Sender: TObject;var AppParams:string) of object;

  TWebUpdateFileVersionCheck = procedure(Sender: TObject; NewVersion,LocalVersion:string; var IsNew:Boolean) of object;

  TWebUpdateConvertPrefix = procedure(Sender: TObject;var Path: string) of object;

  TWebUpdateFileNameFromURL = procedure(Sender: TObject; URL: string; var FName: string) of object;

  TWebUpdateBeforeDownload = procedure(Sender: TObject; FileIdx: Integer;FileDescription:string; var URL: string) of object;

  TWebUpdateFileList = procedure(Sender: TObject;List: TStringList) of object;

  TWebUpdateConnect = (wucNoConnect,wucConnectPrompt,wucConnectSilent,wucConnectPromptHangup,wucConnectSilentHangup,wucNone);

  TWebUpdateUpdate = (wuuPromptOnce,wuuPromptAll,wuuSilent);

  TWebUpdateType = (ftpUpdate,httpUpdate,fileUpdate);

  TWebUpdateTextDownloaded = procedure(Sender: TObject; Text: TStrings; var Res: Integer) of object;

  TWebUpdateVersionCheck = (vcUpdateOnly, vcAlways);

  TWebUpdateCertCheck = (ccEnable, ccDisable);

  TLastURLReg = (lurLOCALUSER, lurLOCALMACHINE);

  TLastURLEntry = class(TPersistent)
  private
    FSave: Boolean;
    FKey : string;
    FSection : string;
    FRegRoot: TLastURLReg;
  public
  published
    property Save:Boolean read FSave write FSave default false;
    property Key:string read FKey write FKey;
    property Section:string read FSection write FSection;
    property RegRoot: TLastURLReg read FRegRoot write FRegRoot default lurLOCALUSER;
  end;

  TPostUpdateInfo = class(TPersistent)
  private
    FServer:string;
    FAction:string;
    FData:ansistring;
    FEnabled:Boolean;
    FPostResult:ansistring;
  public
    property PostResult:ansistring read fPostResult write fPostResult;
  published
    property Server:string read FServer write FServer;
    property Action:string read FAction write FAction;
    property Data:ansistring read FData write FData;
    property Enabled:Boolean read FEnabled write FEnabled;
  end;

  TWebUpdate = class;

  TInetThread = class(TThread)
  private
    WebUpdate: TWebUpdate;
    FInitPath: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebUpdate: TWebUpdate; InitPath: boolean);
  end;

  TFileDescription = class(TCollectionItem)
  private
    FCompressed: Boolean;
    FNewCheckSum: Integer;
    FNewSize: Integer;
    FURL: string;
    FTargetDir: string;
    FLocalVersion: string;
    FNewVersion: string;
    FDescription: string;
    FNewCustomVer: string;
    FFileSize: Integer;
    FNewDate: string;
    FNewTime: string;
    FMandatory: Boolean;
    FPreselect: Boolean;
    FHidden: Boolean;
    FSelected: Boolean;
  public
  published
    property URL: string read FURL write FURL;
    property NewVersion: string read FNewVersion write FNewVersion;
    property NewSize: Integer read FNewSize write FNewSize;
    property NewCheckSum: Integer read FNewCheckSum write FNewCheckSum;
    property NewCustomVer: string read FNewCustomVer write FNewCustomVer;
    property NewDate: string read FNewDate write FNewDate;
    property NewTime: string read FNewTime write FNewTime;
    property LocalVersion: string read FLocalVersion write FLocalVersion;
    property Description: string read FDescription write FDescription;
    property TargetDir: string read FTargetDir write FTargetDir;
    property Compressed: Boolean read FCompressed write FCompressed;
    property FileSize: Integer read FFileSize write FFileSize;
    property Mandatory: Boolean read FMandatory write FMandatory;
    property Preselect: Boolean read FPreselect write FPreselect;
    property Hidden: Boolean read FHidden write FHidden;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TFileList = class(TCollection)    
  private
    FActiveItem: Integer;
    function GetItem(Index: Integer): TFileDescription;
    procedure SetItem(Index: Integer; const Value: TFileDescription);
  public
    function GetItemClass: TCollectionItemClass; virtual;
    function Add: TFileDescription;
    function Insert(index: Integer): TFileDescription;
    property Items[Index: Integer]: TFileDescription read GetItem write SetItem; default;
    property ActiveItem: Integer read FActiveItem;
    function TotalSize: Integer;
    function CompletedSize: Integer;
  end;

  tfvi = record
    versionms: Integer;
    versionls: Integer;
  end;


  TWebUpdateUtility = class(TComponent)
  private
    FShowAnimation: Boolean;
    FShowWindow: Boolean;
    FStatusMessage: string;
    FStatusCaption: string;
  published
    property ShowWindow: Boolean read FShowWindow write FShowWindow;
    property ShowAnimation: Boolean read FShowAnimation write FShowAnimation;
    property StatusMessage: string read FStatusMessage write FStatusMessage;
    property StatusCaption: string read FStatusCaption write FStatusCaption;
  end;

  TProgressForm = class(TForm)
  private
    FProgressBar: TProgressBar;
    FLabel: TLabel;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    procedure CreateWnd; override;
    property ProgressBar: TProgressBar read FProgressBar;
    property FileLabel: TLabel read FLabel;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebUpdate = class(TComponent)
  private
    FForceUpdate : Boolean;
    FHint,FHintConnect: HInternet;
    FDialed: Boolean;
    FSaveUpdate: Boolean;
    FControlFileName: string;
    FFTPDirectory: string;
    FFTPPassive: Boolean;
    FHost: string;
    FPassword: string;
    FSignature: string;
    FSignatureCheck: Boolean;
    FExtractCAB: Boolean;
    FApplyPatch: Boolean;
    FPort: Integer;
    FProxy: string;
    FProxyUserID: string;
    FProxyPassword: string;
    FUpdateConnect: TWebUpdateConnect;
    FUpdateType: TWebUpdateType;
    FUpdateUpdate: TWebUpdateUpdate;
    FURL: string;
    FHideURLInLogFile: Boolean;
    FUserID: string;
    FDateFormat: string;
    FTimeFormat: string;
    FDateSeparator: Char;
    FTimeSeparator: Char;
    FPostUpdateInfo: TPostUpdateInfo;
    FLastURLEntry: TLastURLEntry;
    FWebUpdateFileProgress: TWebUpdateFileProgress;
    FWebUpdateFileDownloaded: TWebUpdateFileDownloaded;
    FWebUpdateFileVersionCheck: TWebUpdateFileVersionCheck;
    FWebUpdateProgress: TWebUpdateProgress;
    FWebUpdateProgressCancel: TWebUpdateProgressCancel;
    FWebUpdateStatus: TWebUpdateStatus;
    FThreadUpdateDone: TWebUpdateThreadDone;
    FAppRestart: TWebUpdateRestart;
    FAppDoClose: TWebUpdateEvent;
    FCustomValidate: TWebUpdateCustomValidate;
    FCustomProcess: TWebUpdateCustomProcess;
    FProcessPostResult: TWebUpdateProcessPostResult;
    FBeforePost: TWebUpdateBeforePost;
    FFileNameList: TWebUpdateFileList;
    FThreaded: Boolean;
    FAppName: string;
    FAppParam: string;
    FAppComps: string;
    FAppCompsIncluded: Boolean;
    FAppClose: Boolean;
    FSilentRestart: Boolean;
    FConvertPrefix: TWebUpdateConvertPrefix;
    FSetAppParams: TWebUpdateSetParams;
    FFTPDirSet: Boolean;
    FOnDownloadedWhatsNew: TWebUpdateTextDownloaded;
    FOnDownloadedEULA: TWebUpdateTextDownloaded;
    FOnBeforeFileDownload: TWebUpdateBeforeDownload;
    FUseCRC32: Boolean;
    FUseWinTempDir: Boolean;
    FUtility: TWebUpdateUtility;
    FOnFileNameFromURL: TWebUpdateFileNameFromURL;
    FTempDirectory: string;
    FKeepAlive: Boolean;
    FExistingConnection: Boolean;
    FLocalFileDateCheck : string;
    FNewVersionDate: TDateTime;
    FCurVersionDate: TDateTime;
    FCurVersionInfo: string;
    FNewVersionInfo: string;
    FFileList: TFileList;
    FUpdateDescription: string;
    FLogging: Boolean;
    FCancelled: Boolean;
    FVersionCheck: TWebUpdateVersionCheck;
    FOnSuccess: TNotifyEvent;
    FInProgress: Boolean;
    FKeepIntermediateFiles: Boolean;
    FAgent: string;
    FLanguageID: string;
    FLogFileName: string;
    FTimeOut: integer;
    FReceiveTimeOut: integer;
    FAuthenticate: TWebUpdateAuthentication;
    FAutoRestart: boolean;
    FProgressForm: TProgressForm;
    FShowDownloadProgress: Boolean;
    FCertCheck: TWebUpdateCertCheck;
    FSetAppParamsAfter: TWebUpdateSetParams;
    FSetAppParamsBefore: TWebUpdateSetParams;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetLogFileName(const Value: string);
    function GetUACEnabled: boolean;
  protected
    function GetTextFile(URL: string): TStringList;
    function GetStream(URL: string): TMemoryStream;
    function ExpandPath(tgt: string):string;
    function FileGetFile(url,tgt:string;uncompress:Boolean):Boolean;
    procedure ThreadDone(Sender: TObject);
    function  IsRTF(ms: TMemoryStream): Boolean;
    function WhatsNewDialog(ms: TMemoryStream):Integer; virtual;
    function EULADialog(ms: TMemoryStream):Integer; virtual;
    procedure DoStatus(id:Integer;param:string;statuscode,errorcode:Integer); virtual;
    procedure ExtractUpdateResource; virtual;
    procedure ExtractUtility; virtual;
    function CheckVersions(var fvn,fvl:string;fvs,fvc:Integer):Boolean; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function ControlValueToDate(d,t:string): TDateTime;
    function ExecAndWait(sCommandLine: string; Show, Animate: Boolean; Caption, Msg: string): Boolean;
    function GetAppNeedsRestart: Boolean;
    procedure HandleProgress(AFileName: string; FilePos, FileSize: Longint); virtual;
    procedure HandleCancel(var Cancel: Boolean); virtual;
    procedure SetTimeout(timeout: dword);
    procedure SetReceiveTimeOut(receivetimeout: dword);
    procedure DoSetAppParamBefore(var Param:string); virtual;
    procedure DoSetAppParamAfter(var Param:string); virtual;
  public
    procedure CustomProcess(fn:string); virtual;
    function URLtoFile(url:string):string;
    function URLToDomain(url:string):string;
    function URLGetFile(hfile:hinternet;url,tgt:string;uncompress:Boolean):Boolean;
    function MakeProxyUrl(url,proxyuser,proxypwd:string):string;
    procedure ShowHTMLDialog(s:string);
    procedure Error;
    procedure Cancel;
    function GetFileVersion(FileName: string):tfvi;
    procedure AddToLog(s: string);
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure DoUpdate; overload;
    procedure DoUpdate(InitPath: boolean); overload;
    procedure DoSuccess;
    procedure DoThreadUpdate; overload;
    procedure DoThreadUpdate(InitPath: boolean); overload;
    procedure DoPostUpdateInfo;
    procedure DoRestart; virtual;
    function StartConnection: Integer; virtual;
    function StopConnection: Integer; virtual;
    function FTPConnect: Integer; virtual;
    function GetControlFileURL: string; virtual;
    function GetControlFile: Integer; virtual;
    function DoVersionCheck: Integer; virtual;
    function HandleActions: Integer; virtual;
    function GetWhatsNew: TStringList; virtual;
    function GetWhatsNewStream: TMemoryStream; virtual;
    function GetEULA: TStringList; virtual;
    function GetEULAStream: TMemoryStream; virtual;
    function GetOSVersion: string; virtual;
    function GetIDE: string; virtual;
    function GetIEVersion: string; virtual;
    function GetInstalledIDEs: string; virtual;
    function GetFileDetails: Integer; virtual;
    function ProcessFileDetails: Integer; virtual;
    function GetFileUpdates: Integer; virtual;
    function UpdateActions: Integer;
    function Connected: Boolean;
    function ConnectionType: Integer;
    function NewVersionAvailable(InitPath: boolean = false): Boolean;
    function NewWhatsNew(showdialog: boolean = false): TStringList;
    procedure HangUp;
    procedure URLPut(url:string);
    function URLGet:string;
    procedure UpdateDatePut(dt:TDateTime);
    function UpdateDateGet:TDateTime;
    function WinTempDir: string;
    procedure ConvertPrefix(const prefix:string; var s:string); virtual;
    property ControlFileName: string read FControlFileName;
    property NewVersionDate: TDateTime read FNewVersionDate;
    property CurVersionDate: TDateTime read FCurVersionDate;
    property CurVersionInfo: string read FCurVersionInfo;
    property NewVersionInfo: string read FNewVersionInfo;
    property UpdateDescription: string read FUpdateDescription;
    property FileList: TFileList read FFileList;
    property Cancelled: Boolean read FCancelled;
    property InProgress: Boolean read FInProgress;
    property AppNeedsRestart: Boolean read GetAppNeedsRestart;
    property AppSilentRestart: Boolean read FSilentRestart;
    property HintConnect: HInternet read FHintConnect;
    property UACEnabled: boolean read GetUACEnabled;
   published
    property Agent: string read FAgent write FAgent;
    property ApplyPatch: Boolean read FApplyPatch write FApplyPatch default false;
    property Authenticate: TWebUpdateAuthentication read FAuthenticate write FAuthenticate default waNever;
    property AutoRestart: boolean read FAutoRestart write FAutoRestart default true;
    property CertCheck: TWebUpdateCertCheck read FCertCheck write FCertCheck default ccEnable;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DateSeparator: Char read FDateSeparator write FDateSeparator;
    property ExtractCAB: Boolean read FExtractCAB write FExtractCAB default false;
    property ExistingConnection: Boolean read FExistingConnection write FExistingConnection default false;
    property ForceUpdate : Boolean read FForceUPdate write FForceUpdate default false;
    property FTPDirectory: string read FFTPDirectory write FFTPDirectory;
    property FTPPassive: Boolean read FFTPPassive write FFTPPassive default false;
    property Host: string read FHost write FHost;
    property HTTPKeepAliveAuthentication: Boolean read FKeepAlive write FKeepAlive default false;
    property KeepIntermediateFiles: Boolean read FKeepIntermediateFiles write FKeepIntermediateFiles default false;
    property LanguageID: string read FLanguageID write FLanguageID;
    property Logging: Boolean read FLogging write FLogging default false;
    property LogFileName: string read FLogFileName write SetLogFileName;
    property LastURLEntry: TLastURLEntry read FLastURLEntry write FLastURLEntry;
    property Password: string read FPassword write FPassword;
    property Port: Integer read FPort write FPort default 21;
    property PostUpdateInfo: TPostUpdateInfo read FPostUpdateInfo write FPostUpdateInfo;
    property Proxy: string read FProxy write FProxy;
    property ProxyUserID: string read FProxyUserID write FProxyUserID;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
    property ShowDownloadProgress: boolean read FShowDownloadProgress write FShowDownloadProgress default false;
    property Signature: string read FSignature write FSignature;
    property SignatureCheck: Boolean read FSignatureCheck write FSignatureCheck default false;
    property TempDirectory: string read FTempDirectory write FTempDirectory;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
    property TimeOut: integer read FTimeOut write FTimeOut default 0;
    property ReceiveTimeOut: integer read FReceiveTimeOUt write FReceiveTimeOut default 0;
    property TimeSeparator: Char read FTimeSeparator write FTimeSeparator;
    property UpdateType: TWebUpdateType read FUpdateType write FUpdateType default httpUpdate;
    property UpdateConnect: TWebUpdateConnect read FUpdateConnect write FUpdateConnect default wucNoConnect;
    property UpdateUpdate: TWebUpdateUpdate read FUpdateUpdate write FUpdateUpdate default wuuPromptOnce;
    property URL: string read FURL write FURL;
    property HideURLInLogFile: Boolean read FHideURLInLogFile write FHideURLInLogFile default False;
    property UserID:string read FUserID write FUserID;
    property UseCRC32: Boolean read FUseCRC32 write FUseCRC32 default false;
    property UseWinTempDir: Boolean read FUseWinTempDir write FUseWinTempDir default true;
    property Utility: TWebUpdateUtility read FUtility write FUtility;
    property VersionCheck: TWebUpdateVersionCheck read FVersionCheck write FVersionCheck default vcUpdateOnly;
    property OnFileProgress:TWebUpdateFileProgress read FWebUpdateFileProgress
      write FWebUpdateFileProgress;
    property OnFileDownloaded:TWebUpdateFileDownloaded read FWebUpdateFileDownloaded
      write FWebUpdateFileDownloaded;
    property OnFileVersionCheck:TWebUpdateFileVersionCheck read FWebUpdateFileVersionCheck
      write FWebUpdateFileVersionCheck;
    property OnProcessPostResult:TWebUpdateProcessPostResult read FProcessPostResult
      write FProcessPostResult;
    property OnBeforePost: TWebUpdateBeforePost read FBeforePost write FBeforePost;
    property OnProgress:TWebUpdateProgress read FWebUpdateProgress
      write FWebUpdateProgress;
    property OnProgressCancel:TWebUpdateProgressCancel read FWebUpdateProgressCancel
      write FWebUpdateProgressCancel;
    property OnStatus:TWebUpdateStatus read FWebUpdateStatus
      write FWebUpdateStatus;
    property OnThreadUpdateDone:TWebUpdateThreadDone read FThreadUpdateDone
      write FThreadUpdateDone;
    property OnAppRestart:TWebUpdateRestart read FAppRestart
      write FAppRestart;
    property OnAppDoClose:TWebUpdateEvent read FAppDoClose
      write FAppDoClose;
    property OnBeforeFileDownload: TWebUpdateBeforeDownload read FOnBeforeFileDownload
      write FOnBeforeFileDownload;
    property OnCustomValidate:TWebUpdateCustomValidate read FCustomValidate
      write FCustomValidate;
    property OnCustomProcess:TWebUpdateCustomProcess read FCustomProcess
      write FCustomProcess;
    property OnGetFileList:TWebUpdateFileList read FFileNameList
      write FFileNameList;
    property OnConvertPrefix:TWebUpdateConvertPrefix read FConvertPrefix
      write FConvertPrefix;
    property OnSetAppParams: TWebUpdateSetParams read FSetAppParams
      write FSetAppParams;
    property OnSetAppParamAfter: TWebUpdateSetParams read FSetAppParamsAfter
      write FSetAppParamsAfter;
    property OnSetAppParamBefore: TWebUpdateSetParams read FSetAppParamsBefore
      write FSetAppParamsBefore;
    property OnDownloadedWhatsNew: TWebUpdateTextDownloaded read FOnDownloadedWhatsNew
      write FOnDownloadedWhatsNew;
    property OnDownloadedEULA: TWebUpdateTextDownloaded read FOnDownloadedEULA
      write FOnDownloadedEULA;
    property OnFileNameFromURL: TWebUpdateFileNameFromURL read FOnFileNameFromURL
      write FOnFileNameFromURL;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property Version: string read GetVersion write SetVersion;
  end;

function GetSizeOfFile(fn:string):Integer;
procedure HTMLDialog(pHandle: THandle; s:string);
function UserDocDir: string;

const
  winetdll = 'wininet.dll';

const
  D5 = '\Software\Borland\Delphi\5.0';
  D6 = '\Software\Borland\Delphi\6.0';
  D7 = '\Software\Borland\Delphi\7.0';
  D2005 = '\Software\Borland\BDS\3.0';
  D2006 = '\Software\Borland\BDS\4.0';
  D2007 = '\Software\Borland\BDS\5.0';
  D2009 = '\Software\CodeGear\BDS\6.0';
  D2010 = '\Software\CodeGear\BDS\7.0';
  D2011 = '\Software\Embarcadero\BDS\8.0';
  DXE2 = '\Software\Embarcadero\BDS\9.0';
  DXE3 = '\Software\Embarcadero\BDS\10.0';
  DXE4 = '\Software\Embarcadero\BDS\11.0';
  DXE5 = '\Software\Embarcadero\BDS\12.0';
  DXE6 = '\Software\Embarcadero\BDS\14.0';
  DXE7 = '\Software\Embarcadero\BDS\15.0';
  DXE8 = '\Software\Embarcadero\BDS\16.0';
  C5 = '\Software\Borland\C++Builder\5.0';
  C6 = '\Software\Borland\C++Builder\6.0';

implementation

{$IFDEF USEURLMON}
uses
  ActiveX, UrlMon, ComObj;
{$ENDIF}

const
  READBUFFERSIZE = 4096;

{$IFNDEF DELPHI2006_LVL}
  CSIDL_LOCAL_APPDATA = $001c;
{$ENDIF}
{$IFNDEF DELPHIXE_LVL}
  CSIDL_COMMON_APPDATA = $0023;
{$ENDIF}

{$IFDEF USEURLMON}
type
  TShowHTMLDialogFn = function( const hwndParent : HWND; const pmk : IMoniker;
                                const pvarArgIn : Variant; const pchOptions : POleStr;
                                var pvarArgOut : Variant ):HResult stdcall;
{$ENDIF}

var
  _logfilename: string;

{$I DELPHIXE.INC}

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

function TWebUpdate.GetIEVersion: string;
var
  Reg: TRegistry;
begin
  Result := '(not found)';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey ('\Software\Microsoft\Internet Explorer\Version Vector', False) then
      Result := Reg.ReadString('IE');
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

function IsPlatformInstalled(const Platform: string): Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.OpenKey(Platform, False);
    Reg.CloseKey;
  finally
    Reg.Free
  end;
end;

function TWebUpdate.GetIDE: string;
begin
  {$IFDEF VER180}
  {$IFDEF BCB}
  Result := 'C2006';
  {$ELSE}
  Result := 'D2006';
  {$ENDIF}
  {$ENDIF}
  
  {$IFDEF VER185}
  {$IFDEF BCB}
  Result := 'C2007';
  {$ELSE}
  Result := 'D2007';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER200}
  {$IFDEF BCB}
  Result := 'C2009';
  {$ELSE}
  Result := 'D2009';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER210}
  {$IFDEF BCB}
  Result := 'C2010';
  {$ELSE}
  Result := 'D2010';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER220}
  {$IFDEF BCB}
  Result := 'C2011';
  {$ELSE}
  Result := 'D2011';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER230}
  {$IFDEF BCB}
  Result := 'CXE2';
  {$ELSE}
  Result := 'DXE2';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER240}
  {$IFDEF BCB}
  Result := 'CXE3';
  {$ELSE}
  Result := 'DXE3';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER250}
  {$IFDEF BCB}
  Result := 'CXE4';
  {$ELSE}
  Result := 'DXE4';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER260}
  {$IFDEF BCB}
  Result := 'CXE5';
  {$ELSE}
  Result := 'DXE5';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER270}
  {$IFDEF BCB}
  Result := 'CXE6';
  {$ELSE}
  Result := 'DXE6';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER280}
  {$IFDEF BCB}
  Result := 'CXE7';
  {$ELSE}
  Result := 'DXE7';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER290}
  {$IFDEF BCB}
  Result := 'CXE8';
  {$ELSE}
  Result := 'DXE8';
  {$ENDIF}
  {$ENDIF}

  {$IFDEF VER170}
  Result := 'D2005';
  {$ENDIF}
  {$IFDEF VER150}
  Result := 'D7';
  {$ENDIF}
  {$IFDEF VER140}
  {$IFDEF BCB}
  Result := 'C6';
  {$ELSE}
  Result := 'D6';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF VER130}
  {$IFDEF BCB}
  Result := 'C5';
  {$ELSE}
  Result := 'D5';
  {$ENDIF}
  {$ENDIF}
end;

function TWebUpdate.GetInstalledIDEs: string;
var
  DevPlatforms: string;
begin
  DevPlatforms := '';

  if IsPlatformInstalled(D5) then
    DevPlatforms := DevPlatforms + 'D5';
  if IsPlatformInstalled(D6) then
    DevPlatforms := DevPlatforms + 'D6';
  if IsPlatformInstalled(D7) then
    DevPlatforms := DevPlatforms + 'D7';
  if IsPlatformInstalled(D2005) then
    DevPlatforms := DevPlatforms + 'D2005';
  if IsPlatformInstalled(D2006) then
    DevPlatforms := DevPlatforms + 'D2006';
  if IsPlatformInstalled(D2007) then
    DevPlatforms := DevPlatforms + 'D2007';
  if IsPlatformInstalled(D2009) then
    DevPlatforms := DevPlatforms + 'D2009';
  if IsPlatformInstalled(D2010) then
    DevPlatforms := DevPlatforms + 'D2010';
  if IsPlatformInstalled(D2011) then
    DevPlatforms := DevPlatforms + 'D2011';
  if IsPlatformInstalled(DXE2) then
    DevPlatforms := DevPlatforms + 'DXE2';
  if IsPlatformInstalled(DXE3) then
    DevPlatforms := DevPlatforms + 'DXE3';
  if IsPlatformInstalled(DXE4) then
    DevPlatforms := DevPlatforms + 'DXE4';
  if IsPlatformInstalled(DXE5) then
    DevPlatforms := DevPlatforms + 'DXE5';
  if IsPlatformInstalled(DXE6) then
    DevPlatforms := DevPlatforms + 'DXE6';

  if IsPlatformInstalled(C5) then
    DevPlatforms := DevPlatforms + 'C5';
  if IsPlatformInstalled(C6) then
    DevPlatforms := DevPlatforms + 'C6';

  Result := DevPlatforms;
end;

function TWebUpdate.GetOSVersion: string;
const
  SM_SERVERR2 = 89;
  VER_NT_WORKSTATION = $0000001;

{$IFDEF DELPHI_UNICODE}
type
  pfnRtlGetVersion = function(var RTL_OSVERSIONINFOEXW): DWORD; stdcall;
{$ENDIF}

type
  TOSVersionInfoEx = record
    dwOSVersionInfoSize:DWORD;
    dwMajorVersion:DWORD;
    dwMinorVersion:DWORD;
    dwBuildNumber:DWORD;
    dwPlatformId:DWORD;
    szCSDVersion: array[0..127] of Char;
    wServicePackMajor:WORD;
    wServicePackMinor:WORD;
    wSuiteMask:WORD;
    wProductType:BYTE;
    wReserved:BYTE;
  end;

var
  osVerInfo: TOSVersionInfoEx;
  majorVer, minorVer: Cardinal;
{$IFDEF DELPHI_UNICODE}
  ver: RTL_OSVERSIONINFOEXW;
  RtlGetVersion: pfnRtlGetVersion;


  procedure GetUnmanistedVersion(var majv,minv: cardinal);
  begin
    @RtlGetVersion := GetProcAddress(GetModuleHandle('ntdll.dll'), 'RtlGetVersion');
    if Assigned(RtlGetVersion) then
    begin
      ZeroMemory(@ver, SizeOf(ver));
      ver.dwOSVersionInfoSize := SizeOf(ver);

      if RtlGetVersion(ver) = 0 then
      begin
        majv := ver.dwMajorVersion;
        minv := ver.dwMinorVersion;
      end;
    end;
  end;
{$ENDIF}

begin
  Result := 'Unknown';
  { set operating system type flag }
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
  if GetVersionEx(Windows.POSVersionInfo(@osVerInfo)^) then
  begin
    majorVer := osVerInfo.dwMajorVersion;
    minorVer := osVerInfo.dwMinorVersion;

    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT: { Windows NT/2000 }
        begin
          if majorVer <= 4 then
            Result := 'Windows NT'
          else if (majorVer = 5) and (minorVer = 0) then
            Result := 'Windows 2000'
          else if (majorVer = 5) and (minorVer = 1) then
            Result := 'Windows XP'
          else if (majorVer = 5) and (minorVer = 2) then
            Result := 'Windows 2003'
          else if (majorVer = 6) and (minorVer = 0) then
          begin
            Result := 'Windows Vista';

            if osVerInfo.wProductType = VER_NT_WORKSTATION then
              Result := 'Windows Vista'
            else
              Result := 'Windows Server 2008';
          end
          else if (majorVer = 6) and (minorVer = 1) then
          begin
            if osVerInfo.wProductType = VER_NT_WORKSTATION then
               Result := 'Windows 7'
            else
               Result := 'Windows Server 2008R2';
          end
          else if (majorVer = 6) and (minorVer = 2) then
          begin
            // in case of unmanifested call
            {$IFDEF DELPHI_UNICODE}
            GetUnmanistedVersion(majorVer, minorVer);
            {$ENDIF}

            if (majorVer = 6) and (minorVer = 2) then
            begin
              if osVerInfo.wProductType = VER_NT_WORKSTATION then
                Result := 'Windows 8'
              else
                Result := 'Windows Server 2012'
            end;

            if (majorVer = 6) and (minorVer = 3) then
            begin
              if osVerInfo.wProductType = VER_NT_WORKSTATION then
                Result := 'Windows 8.1'
              else
                Result := 'Windows Server 2012R2'
            end;

            if (majorVer = 10) and (minorVer = 0) then
            begin
              if osVerInfo.wProductType = VER_NT_WORKSTATION then
                Result := 'Windows 10'
              else
                Result := 'Windows Server Preview'
            end;
          end
          else if (majorVer = 6) and (minorVer = 3) then
          begin
            if osVerInfo.wProductType = VER_NT_WORKSTATION then
              Result := 'Windows 8.1'
            else
              Result := 'Windows Server 2012R2'
          end
          // Windows 10 betas return this
          else if (majorVer = 6) and (minorVer = 4) then
          begin
            if osVerInfo.wProductType = VER_NT_WORKSTATION then
              Result := 'Windows 10'
            else
              Result := 'Windows Server Preview'
          end
          else
            Result := 'Unknown';
        end;
      VER_PLATFORM_WIN32_WINDOWS:  { Windows 9x/ME }
        begin
          if (majorVer = 4) and (minorVer = 0) then
            Result := 'Windows 95'
          else if (majorVer = 4) and (minorVer = 10) then
          begin
            if osVerInfo.szCSDVersion[1] = 'A' then
              Result := 'Windows 98 SE'
            else
              Result := 'Windows 98';
          end
          else if (majorVer = 4) and (minorVer = 90) then
            Result := 'Windows ME'
          else
            Result := 'Unknown';
        end;
      else
        Result := 'Unknown';
    end;
  end
  else
    Result := 'Unknown';
end;


function DirExists(const Name: string): Boolean;
var
  Code: Cardinal;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function CLFToLF(s:string):string;
begin
  while Pos('\n',s) > 0 do
  begin
    s := Copy(s,1,Pos('\n',s)-1)+#13+Copy(s,pos('\n',s)+2,Length(s));
  end;
  Result := s;
end;

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

function VersionToString(fvi: tfvi): string;
begin
  Result := inttostr(hiword(fvi.VersionMS))+'.'+inttostr(loword(fvi.VersionMS))+'.'+
    inttostr(hiword(fvi.VersionLS))+'.'+inttostr(loword(fvi.VersionLS));
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

// Either remove a trailing backslash or add '.' as needed to get a directory
function RemoveBackslash(S: string): string;
begin
  s := AddBackslash(s);
  if (Length(s) > 3) and (s[2] = ':') then
    Delete(s,Length(s),1)
  else
    s := s + '.';
  Result := s
end;

{$IFDEF DELPHIXE_LVL}
// This function retrieve the path of the special system directorys
function GetWinSpecialPath(const SpecialPathId: ShortInt; const KnownFolderId: TGUID): string;
var
  SpecialFolder: PItemIDList;
  SpecialPath: Array[0..(MAX_PATH + 1)*2] of Char;
  Path: PChar;  // = PWideChar in actually versions
  ErrCode: Longint;
begin
  Result:= '';
  case Win32Platform of
    // Windows 3.1: OS not supported
    VER_PLATFORM_WIN32s:
      ;
    // Windows 95...Windows ME: OS not supported
    VER_PLATFORM_WIN32_WINDOWS:
      ;
    // Windows NT family
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        // Windows NT 3.1...Windows NT 4.0: OS not supported
        3, 4:
          ;
        // Minimum supported: Windows 2000 Professional
        5:
          begin
            SpecialFolder:= nil;
            try
              ErrCode:= SHGetFolderLocation(0, SpecialPathId, 0, 0, SpecialFolder);
              if ErrCode =  ERROR_SUCCESS then
              begin
                // Converts an item identifier list to a file system path
                if SHGetPathFromIDList(SpecialFolder, SpecialPath) then
                  Result:= SpecialPath;
              end;
            finally
              CoTaskMemFree(SpecialFolder);
            end;
          end;
        // Minimum supported: Windows Vista
        6:
          begin
            Path:= nil;
            try
              ErrCode:= SHGetKnownFolderPath(KnownFolderId, 0, 0, Path);
              if ErrCode =  ERROR_SUCCESS then
                Result:= Path;
            finally
              CoTaskMemFree(Path);
            end;
          end
      end;
    // Windows CE: OS not supported
    VER_PLATFORM_WIN32_CE:
      ;
  end;
end;

function UserDocDir: string;
begin
  Result:= GetWinSpecialPath(CSIDL_PERSONAL, FOLDERID_Documents);
end;

function GetLocalAppData: string;
begin
  Result := GetWinSpecialPath(CSIDL_LOCAL_APPDATA, FOLDERID_LocalAppData);
end;

function GetCommonAppData: string;
begin
  Result := GetWinSpecialPath(CSIDL_COMMON_APPDATA, FOLDERID_ProgramData);
end;

{$ENDIF}


{$IFNDEF DELPHIXE_LVL}

procedure FreePidl( pidl: PItemIDList );
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
    allocator.Free(pidl);
end;


function UserDocDir: string;
var
  pidl: PItemIDList;
  Path: array [0..MAX_PATH-1] of char;
  buf: string;
  i: integer;
begin
  Result := '';

  if Succeeded(
       SHGetSpecialFolderLocation(Application.Handle, CSIDL_PERSONAL, pidl)
     ) then
  begin
    if SHGetPathFromIDList(pidl, Path) then
      Result := Strpas(path);
    FreePidl(pidl);
  end;

  if not DirectoryExists(Result) then
  begin
    SetLength(buf, MAX_PATH);
    i := GetTempPath(Length(buf), PChar(buf));
    SetLength(buf, i);
    Result := buf;
  end;
end;

function GetLocalAppData: string;
var
  r: Bool;
  path: array [0 .. Max_Path-1] of Char;
begin
  r := ShGetSpecialFolderPath(0, path, CSIDL_LOCAL_APPDATA, False);
  if not r then
    raise Exception.Create('Could not find Local Application Data folder location.');
  Result := path;
end;

function GetCommonAppData: string;
var
  r: Bool;
  path: array [0 .. Max_Path-1] of Char;
begin
  r := ShGetSpecialFolderPath(0, path, CSIDL_COMMON_APPDATA, False);
  if not r then
    raise Exception.Create('Could not find Common Application Data folder location.');
  Result := path;
end;
{$ENDIF}

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;
begin
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName), PChar(Result), BufSize);
  end
  else
    Result := '';
end;


function GetProgramFilesFolder: string;
begin
  // change for proper value for 32bit apps on 64bit Windows
  Result := GetEnvVarValue('ProgramFiles')
end;


procedure Log(s:string);
var
  tf:text;
  fn: string;
  FilePath: string;
  bSuccess: boolean;
begin
  if (pos('\',_logfilename) = 0) and (pos('/',_logfilename) = 0) and (pos(':',_logfilename) = 0) then
    fn := AddBackslash(UserDocDir) + _logfilename
  else
    fn := _logfilename;

  bSuccess := True;
  // Check directory first
  FilePath := ExtractFilePath(fn);
  if not DirectoryExists(FilePath) then
  begin
    {$i-}
    bSuccess := ForceDirectories(FilePath);
    {$i+}
  end;

  if bSuccess then
  begin
    AssignFile(tf,fn);
    try
      {$i-}
      Append(tf);
      {$i+}
      if IOResult <> 0 then
      begin
        {$i-}
        Rewrite(tf);
        {$i+}
        if IOResult <> 0 then
        begin
          bSuccess := false;
          Exit;
        end;
      end;
      Writeln(tf,s);
    finally
      if bSuccess then
        CloseFile(tf);
    end;
  end;
end;

function StringToVersion(fvn:string):tfvi;
var
  hw,lw:word;
  e:Integer;
  sep:string;

begin
  Result.VersionMS := -1;
  Result.VersionLS := -1;

  sep := '';
  if Pos('.',fvn) > 0 then sep := '.';
  if Pos(',',fvn) > 0 then sep := ',';

  if Pos(sep,fvn) > 0 then
  begin
    Val(Copy(fvn,1,Pos(sep,fvn)-1),hw,e);
    system.Delete(fvn,1,Pos(sep,fvn));

    if (Pos(sep,fvn) > 0) and (e = 0) then
    begin
      Val(copy(fvn,1,pos(sep,fvn)-1),lw,e);
      Result.VersionMS := makelong(lw,hw);
      system.Delete(fvn,1,pos(sep,fvn));
      if (Pos(sep,fvn) > 0) and (e = 0) then
      begin
        Val(Copy(fvn,1,Pos(sep,fvn)-1),hw,e);
        system.Delete(fvn,1,pos(sep,fvn));
        Val(fvn,lw,e);
        if e = 0 then
          Result.VersionLS := makelong(lw,hw);
      end
      else
      begin
        val(fvn, hw, e);
        Result.VersionLS := makelong(0,hw);
      end;
    end
    else
    begin
      val(fvn, lw, e);
      Result.versionMS := makelong(lw,hw);
      Result.versionLS := makelong(0,0);
    end;
  end
  else
  begin
    val(fvn, hw, e);
    if (e = 0) then
    begin
      Result.versionMS := makelong(0,hw);
      Result.versionLS := makelong(0,0);
    end;
  end;
end;

function TWebUpdate.WinTempDir: string;
var
  buf:string;
  i: integer;
begin
  if UseWinTempDir then
  begin
    SetLength(buf, MAX_PATH);
    i := GetTempPath(Length(buf), PChar(buf));
    SetLength(buf, i);
  end
  else
    buf := TempDirectory;

  Result := AddBackslash(buf);
end;

function TWebUpdate.GetAppNeedsRestart: Boolean;
begin
  Result := FAppClose and (not FCancelled) and FAppCompsIncluded;
end;

procedure TWebUpdate.Cancel;
begin
  FCancelled := true;
end;

procedure TWebUpdate.HandleProgress(AFileName: string; FilePos, FileSize: Longint);
var
  s:string;
begin
  if Assigned(FProgressForm) then
  begin
    FProgressForm.ProgressBar.Min := 0;
    FProgressForm.ProgressBar.Max := FileSize;
    FProgressForm.ProgressBar.Position := FilePos;
    //FProgressForm.FileLabel.Caption := 'Downloading: '+ ExtractFileName(AFileName);
    s := LoadStr(977);
    FProgressForm.FileLabel.Caption := Format(s,[ExtractFileName(AFileName)]);
    FProgressForm.Caption := LoadStr(978);
  end;

  if Assigned(OnFileProgress) then
    OnFileProgress(Self, AFileName, FilePos, FileSize);
end;


procedure TWebUpdate.HandleCancel(var Cancel: boolean);
begin
  if Assigned(OnProgressCancel) then
    OnProgressCancel(Self,Cancel);
  if Cancel then
    DoStatus(964,'',0,0);
end;

function TWebUpdate.ExecAndWait(sCommandLine: string; Show, Animate: Boolean; Caption, Msg: string): Boolean;
var
  tsi: TStartupInfo;
  tpi: TProcessInformation;
  dw: DWORD;
  form: TForm;
  anim: TAnimate;
  lbl: TStaticText;
begin
  form := nil;

  DoStatus(WebUpdateExecAndWait,'Exec: '+sCommandLine,WebUpdateExecAndWait,0);

  if Animate then
  begin
    form := TForm.Create(Application);
    form.Position := poScreenCenter;
    form.Width := 300;
    form.Height := 170;
    form.Caption := Caption;
    form.BorderStyle := bsDialog;
    form.BorderIcons := [];
    anim := TAnimate.Create(form);
    anim.Parent := form;
    anim.CommonAVI := aviCopyFiles;
    anim.Left := 10;
    anim.Top := 10;
    lbl := TStaticText.Create(form);
    lbl.Parent := form;
    lbl.Left := 10;
    lbl.Top := anim.Top + anim.Height + 8;
    lbl.Caption := Msg;
    form.Show;
    form.Repaint;
    anim.Active := true;
  end;

  Result := False;
  FillChar(tsi, SizeOf(TStartupInfo), 0);
  tsi.cb := SizeOf(TStartupInfo);

  if Show then
    TSI.wShowWindow := SW_SHOW
  else
    TSI.wShowWindow := SW_HIDE;

  TSI.dwFlags := StartF_USESHOWWINDOW;

  if CreateProcess(nil, PChar(sCommandLine), nil, nil, False,
     NORMAL_PRIORITY_CLASS, nil, nil, tsi, tpi) then
  begin
    if WaitForSingleObject(tpi.hProcess, INFINITE) = WAIT_OBJECT_0 then
    begin
      if GetExitCodeProcess(tpi.hProcess, dw) then
      begin
        if dw = 0 then
        begin
          Result := True;
        end
        else
        begin
          SetLastError(dw + $2000);
        end;
      end;
    end;
    dw := GetLastError;
    CloseHandle(tpi.hProcess);
    CloseHandle(tpi.hThread);
    SetLastError(dw);
  end
  else
    dw := GetLastError;

  if Assigned(form) then
    form.Free;
end;


function TWebUpdate.GetFileVersion(FileName: string): tfvi;
var
  FileHandle: dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of Char;
  buf: PChar;
  {$IFDEF TMSDEBUG}
  ErrorCode: Integer;
  {$ENDIF}
begin
  Result.VersionMS := -1;
  Result.VersionLS := -1;

  StrpCopy(querybuf,filename);
  l := GetFileVersionInfoSize(querybuf,filehandle);
  if l > 0 then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,filehandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if pvs^.dwSignature = $FEEF04BD then
      begin
        Result.VersionMS := pvs^.dwFileVersionMS;
        Result.VersionLS := pvs^.dwFileVersionLS;
      end;
    end;
    FreeMem(buf);
  end;

  {$IFDEF TMSDEBUG}
  if (l = 0) then
  begin
    ErrorCode := GetLastError;
    if ErrorCode <> 0 then
    begin
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
        nil,ErrorCode,0,querybuf,sizeof(querybuf),nil);
      ShowMessage(strpas(querybuf));
    end;
  end;
  {$ENDIF}
end;

function TWebUpdate.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TWebUpdate.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebUpdate.SetLogFileName(const Value: string);
begin
  FLogFileName := Value;
  _logfilename := Value;
end;

procedure TWebUpdate.SetVersion(const Value: string);
begin

end;

function DynaLink_InternetAutodial(dwFlags: DWORD; dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetAutodial:function(dwFlags: DWORD; dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetAutodial := GetProcAddress(Wininetdll,'InternetAutodial');
    if Assigned(Wininet_InternetAutodial) then
    begin
      Result := Wininet_InternetAutodial(dwFlags,dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;

function DynaLink_InternetAutodialHangup(dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetAutodialHangup:function(dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetAutodialHangup := GetProcAddress(WininetDLL,'InternetAutodialHangup');
    if Assigned(Wininet_InternetAutodialHangup) then
    begin
      Result := Wininet_InternetAutodialHangup(dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;

function DynaLink_InternetGetConnectedState(lpdwFlags: LPDWORD;dwReserved: DWORD): BOOL;
var
  WininetDLL: THandle;
  Wininet_InternetGetConnectedState:function(lpdwFlags: LPDWORD;dwReserved: DWORD): BOOL; stdcall;
begin
  Result := TRUE;
  WininetDLL := LoadLibrary(winetdll);
  if WininetDLL > 0 then
  begin
    @Wininet_InternetGetConnectedState := GetProcAddress(WininetDLL,'InternetGetConnectedState');
    if Assigned(Wininet_InternetGetConnectedState) then
    begin
      Result := Wininet_InternetGetConnectedState(lpdwFlags,dwReserved);
    end;
    FreeLibrary(WininetDLL);
  end;
end;

procedure HTMLDialog(pHandle: THandle; s:string);
{$IFDEF USEURLMON}
var
  ResURLStr : POleStr;
  pmk : IMoniker;
  InParam,OutParam: Variant;
  hInstHTML : THandle;
  ShowHTMLDialog: TShowHTMLDialogFn;
{$ENDIF}

begin
  {$IFDEF USEURLMON}
  @ShowHTMLDialog := nil;

  hInstHTML := LoadLibrary('MSHTML.DLL');
  if hInstHTML = 0 then
    Exit;

  if hInstHTML > 0 then
    @ShowHTMLDialog := GetProcAddress(hInstHTML,'ShowHTMLDialog');

  if @ShowHTMLDialog = nil then
    Exit;

  ResURLStr := StringToOleStr(s);
  OleCheck(CreateURLMoniker(nil,ResURLStr,pmk));
  SysFreeString(ResURLStr);
  TVarData(InParam).VType := varOleStr;
  TVarData(InParam).VOleStr := StringToOleStr('');
  OleCheck(ShowHTMLDialog(pHandle, pmk, InParam, nil, OutParam));
  InParam := Unassigned;
  OutParam := UnAssigned;
  FreeLibrary(hInstHTML);
  {$ENDIF}
end;

procedure TInetThread.Execute;
begin
  {$IFDEF DELPHI_UNICODE}
  WebUpdate.DoUpdate(FInitPath);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Synchronize(WebUpdate.DoUpdate);
  {$ENDIF}
end;

constructor TInetThread.Create(AWebUpdate: TWebUpdate; InitPath: boolean);
begin
  WebUpdate := AWebUpdate;
  FInitPath := InitPath;
  FreeOnTerminate := True;
  inherited Create(False);
end;


constructor TWebUpdate.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FLastURLEntry := TLastURLEntry.Create;
  FPostUpdateInfo := TPostUpdateInfo.Create;
  FLastURLEntry.Key := '';
  FPostUpdateInfo.Enabled := false;
  FPort := 21;
  FDateFormat := 'dd/mm/yyyy';
  FDateSeparator := '/';
  FTimeFormat := 'hh:nn';
  FTimeSeparator := ':';
  FThreaded := false;
  FAppClose := false;
  FAppName := '';
  FAppComps := '';
  FSignatureCheck := false;
  FSignature := 'WebUpdate';
  FSilentRestart := false;
  FFTPDirSet := False;
  FTempDirectory := '.';
  FHint := nil;
  FDialed := False;
  FInProgress := False;
  FFileList := TFileList.Create(TFileDescription);
  FAgent := 'TWebUpdate';
  FAuthenticate := waNever;
  FAutoRestart := true;
  LogFileName := 'WUPDATE.LOG';
  FHideURLInLogFile:= False;
  FUseWinTempDir := true;
  FUpdateType := httpUpdate;
  FVersionCheck := vcUpdateOnly;
  FUpdateUpdate := wuuPromptOnce;
  FUpdateConnect := wucNoConnect;
  FProgressForm := nil;
end;

destructor TWebUpdate.Destroy;
begin
  FLastURLEntry.Free;
  FPostUpdateInfo.Free;
  FFileList.Free;
  inherited Destroy;
end;

procedure TWebUpdate.AddToLog(s: string);
begin
  Log(s);
end;


procedure TWebUpdate.Error;
var
  Errorcode: dword;
  dwIntError,dwLength: dword;
  buf: array[0..1024] of char;

begin
  ErrorCode := GetLastError;
  if ErrorCode <> 0 then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
      pointer(GetModuleHandle(winetdll)),ErrorCode,0,buf,sizeof(buf),nil);

    if (ErrorCode = ERROR_INTERNET_EXTENDED_ERROR) then
    begin
      InternetGetLastResponseInfo(dwIntError,nil,dwLength);
      if dwLength > 0 then
      begin
        InternetGetLastResponseInfo(dwIntError,buf,dwLength);

        DoStatus(976,StrPas(buf),WebUpdateAccessError,ErrorCode);

        if not Assigned(OnStatus) and (UpdateUpdate <> wuuSilent) then
          Messagedlg(StrPas(buf),mtError,[mbok],0);
      end
    end
    else
    begin
      DoStatus(976, StrPas(buf),WebUpdateAccessError,ErrorCode);

      if not Assigned(OnStatus) and (UpdateUpdate <> wuuSilent) then
        Messagedlg(StrPas(buf),mtError,[mbok],0);
    end;
  end;
end;

function GetSizeOfFile(fn:string):Integer;
var
  fh:thandle;
  hisize:Integer;

begin
  Result := 0;

  fh := CreateFile(pchar(fn),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,0);

  if fh > 0 then
  begin
    Result := GetFileSize(fh,@hisize);
    CloseHandle(fh);
  end;
end;

function GetCheckSumOfFile(fn:string):Integer;
var
  fh: THandle;
  buf: array[0..READBUFFERSIZE-1] of Byte;
  NumRead,i : DWORD;
begin
  Result := -1;

  fh := CreateFile(pchar(fn),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,0);

  if fh > 0 then
  begin
    repeat
      if ReadFile(fh,buf,READBUFFERSIZE,numread, nil) then
        for i := 1 to numread do
          Result := Result + buf[i-1]
      else NumRead := 0;

    until NumRead <> READBUFFERSIZE;
    CloseHandle(fh);
  end;
end;

function TWebUpdate.CheckVersions(var fvn,fvl: string;fvs,fvc: Integer):Boolean;
var
  lfvi,nfvi: tfvi;
  lfs: Integer;
  haslocal: boolean;

begin
  Result := True;

  if (fvn = '') and (fvs = 0) and (fvc = -1) then
    Exit;

  if (fvl = '') and (VersionCheck = vcUpdateOnly) then
    Exit;

  haslocal := FileExists(fvl);

  if (not haslocal) and (VersionCheck = vcAlways) then
    Exit;

  Result := False;

  if (not haslocal) and (VersionCheck = vcUpdateOnly) then
    Exit;

  if fvc <> -1 then
  begin
    if FUseCRC32 then
      lfs := CRC32CheckSumOfFile(fvl)
    else
      lfs := GetCheckSumOfFile(fvl);

    if (lfs = 0) and haslocal then  // in case retrieving checksum fails
      lfs := fvc;

    Result := (fvc <> lfs) and ((lfs <> 0) or (VersionCheck = vcAlways));
    DoStatus(944,'Checksum compare of '+fvl+':'+inttostr(lfs)+':'+inttostr(fvc),WebUpdateInformation,0);
    Exit;
  end;

  if fvs > 0 then
  begin
    lfs := GetSizeOfFile(fvl);
    Result := (lfs <> fvs) and ((lfs > 0) or (VersionCheck = vcAlways));
    Exit;
  end;

  lfvi := GetFileVersion(fvl);
  nfvi := StringToVersion(fvn);

  DoStatus(944,'Version compare of '+fvl+':'+VersionToString(lfvi)+' with ' + fvn +':'+VersionToString(nfvi),WebUpdateInformation,0);

  if (nfvi.versionMS = -1) or (nfvi.versionLS = -1) then
    Exit;

  if ((lfvi.VersionMS = -1) or (lfvi.VersionLS = -1)) and (VersionCheck = vcUpdateOnly) then
    Exit;

  fvl := IntToStr(hiword(lfvi.VersionMS)) + '.' + IntToStr(loword(lfvi.VersionMS)) + '.' +
    IntToStr(hiword(lfvi.VersionLS)) + '.' + IntToStr(loword(lfvi.VersionLS));

  fvn := IntToStr(hiword(nfvi.VersionMS)) + '.' + IntToStr(loword(nfvi.VersionMS)) + '.' +
    IntToStr(hiword(nfvi.VersionLS)) + '.' + IntToStr(loword(nfvi.VersionLS));

  if (nfvi.versionMS > lfvi.versionMS) or
     ((nfvi.versionMS = lfvi.versionMS) and (nfvi.versionLS > lfvi.versionLS)) then
    Result := True;
end;

function ExpandFile(SrcName:ansistring): string;
var
  rbuf: TOFSTRUCT;
  src,dst: Integer;
  dstname: array[0..MAX_PATH] of ansichar;
begin
  src := lzOpenFileA(PAnsiChar(SrcName),rbuf,OF_READ);

  if GetExpandedNameA(PAnsiChar(SrcName),dstname) = 1  then
  begin
    Result := string(dstname);
    dst := lzopenfileA(dstname,rbuf,OF_CREATE);
    lzCopy(src,dst);
    lzClose(dst);
  end
  else
    Result := string(SrcName);
  lzClose(src);

  {$IFDEF DELPHI_UNICODE}
  DeleteFile(pwidechar(string(srcname)));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  DeleteFile(pchar(srcname));
  {$ENDIF}
end;


procedure TWebUpdate.ConvertPrefix(const prefix:string; var s:string);
var
  buf: array[0..MAX_PATH - 1] of Char;
begin
  if prefix = 'WIN' then
  begin
    GetWindowsDirectory(buf,sizeof(buf));
    s := StrPas(buf);
  end
  else
    if prefix = 'SYS' then
    begin
      GetSystemDirectory(buf, sizeof(buf));
      s := StrPas(buf);
    end
    else
      if prefix = 'APP' then
        s := ExtractFilePath(Application.ExeName)
      else
        if prefix = 'TMP' then
          s := WinTempDir
        else
          if prefix = 'DOC' then
            s := UserDocDir
          else
            if prefix = 'PF' then
              s := GetProgramFilesFolder
            else
              if prefix = 'LAD' then
                s := GetLocalAppData
              else
                if prefix = 'APPDATA' then
                  s:= GetCommonAppData;

  s := RemoveBackSlash(s);
end;

function TWebUpdate.ExpandPath(tgt:string):string;
var
  i,prefixbegin: Integer;
  s: string;
begin
  {call event handler for custom macro expansion}
  if Assigned(FConvertPrefix) then
    FConvertPrefix(self,tgt);

  {look for prefix and call ConvertPrefix if found}
  prefixbegin := 0;
  i := 1;
  while i <= Length(tgt) do
  begin
    if (prefixbegin = 0) and (tgt[i] = '{') then
      prefixbegin := i
    else
      if (prefixbegin <> 0) and (tgt[i] = '}') then
      begin
        s := '';
        ConvertPrefix(UpperCase(copy(tgt,prefixbegin + 1,i - prefixbegin - 1)),s);
        if s <> '' then
        begin
          Delete(tgt,prefixbegin,i-prefixbegin+1);
          Insert(s,tgt,prefixbegin);
          i := prefixbegin - 1; //restart the loop at beginning of new changes
        end;
        prefixbegin := 0;
      end;
    Inc(i);
  end;
  Result := tgt;
end;

function TWebUpdate.URLtoFile(url:string):string;
var
  Res: string;
begin
  Res := url;

  while Pos('/',Res) > 0 do
    Delete(Res,1,Pos('/',Res));

  while Pos('\',Res) > 0 do
    Delete(Res,1,Pos('\',Res));

  while Pos('?',Res) > 0 do
    Delete(Res,1,Pos('?',Res));

  if Assigned(FOnFileNameFromURL) then
    FOnFileNameFromURL(Self,url,Res);

  Result := Res;
end;

function TWebUpdate.URLtoDomain(url:string):string;
begin
  if pos('://',url) > 0 then
    delete(url,1, pos('://',url)+2);

  if pos('/',url) > 0 then
    delete(url,pos('/',url),length(url));

  if pos('\',url) > 0 then
    delete(url,pos('/',url),length(url));

  Result := url;
end;


function TWebUpdate.MakeProxyUrl(url,proxyuser,proxypwd:string):string;
begin
  Result := url;
  if (Pos('HTTP://',Uppercase(url)) = 1) and (ProxyUser <> '') then
  begin
    Delete(url,1,7);
    Result := 'http://' + ProxyUser + ':' + ProxyPwd + '@' + url;
  end;
end;

{$WARNINGS OFF}
procedure FileSetNotReadOnlyOrHidden(const MyFile : string);
var
  Attribute      : integer;
begin
  Attribute := FileGetAttr(MyFile);
  if Attribute <> -1 then
  begin
    FileSetAttr(MyFile, Attribute and not faReadOnly and not faHidden);
  end;
end;
{$WARNINGS ON}


// copy files over a LAN
function TWebUpdate.FileGetFile(url, tgt: string; uncompress:Boolean): Boolean;
const
  BlockSize = $4000;
type
  pBuf = ^tBuf;
  TBuf = array[1..BlockSize] of char;

var
  tmpname:array[0..MAX_PATH] of char;
  SourceSize: LongInt;
  source,target: tFileRec;
  numRead, numWritten: Integer;
  fbuf: pBuf;
  fsize: Integer;
  DoCancel,dwnloaderror:Boolean;

begin
  Result := False;

  FSize := 0;

  // if source file and target file are identical raises an error
  if url = tgt then
  begin
    DoStatus(912,url,WebUpdateWrongSource,ErrUpdateTargetEqual);
    Exit;
  end;

  // deny writing to the file while someone could be updating
  // tries to open the source file
  Source.Handle := FileOpen(url, fmShareDenyWrite);
  if Source.Handle = -1 then
  begin
    DoStatus(913,url,WebUpdateNotFound,ErrUpdateFileNotFound);
    Exit;
  end;

  // compute how many block are needed
  SourceSize := FileSeek(Source.handle, 0, 2);

  if SourceSize = -1 then
  begin
    FileClose(Source.Handle);
    DoStatus(914,url,WebUpdateWrongSource,ErrUpdateFileZeroLen);
    Exit;
  end;

  // set the handle to the file beginning
  FileSeek(Source.Handle, 0, 0);

  // tries to create the target file

  if UseWinTempDir then
    GetTempFilename(PChar(WinTempDir),'WUPD',0,tmpname)
  else
   GetTempFilename(PChar(FTempDirectory),'WUPD',0,tmpname);

  Target.handle := FileCreate(strpas(tmpname));

  if Target.Handle < 0 then
  begin
    FileClose(Source.Handle);
    Exit;
  end;

  DoCancel := false;
  DwnloadError := false;

  New(FBuf);

  // copy block
  repeat
    // reading
    numRead := FileRead(Source.Handle, FBuf^, sizeOf(FBuf^));

    if numRead < 0 then
      begin
        FileClose(Source.Handle);
        FileClose(Target.handle);
        Dispose(FBuf);
        dwnloaderror := true;
        // if Assigned(OnError) then OnError(self,errCopyReadFailure);
      end;

    // writing
    numWritten := FileWrite(Target.Handle, FBuf^, numRead);
    if numWritten < 0 then
      begin
        FileClose(Source.Handle);
        FileClose(Target.handle);
        dispose(FBuf);
        dwnloaderror := true;
        //if assigned(OnError) then OnError(self,errCopyWriteFailure);
      end;

    fsize := fsize + numWritten;

    HandleProgress(url,fsize,SourceSize);
    HandleCancel(DoCancel);

    Application.ProcessMessages;

  until (numRead = 0) or (numRead <> numWritten) or (DoCancel) or (dwnloaderror);

  FileClose(Source.Handle);
  FileClose(Target.handle);
  dispose(FBuf);

  // if Assigned(FOnCopyProgress) then FOnCopyProgress(Self, 100, 100);

  FCancelled := DoCancel;

  if DoCancel or dwnloaderror then
  begin
    sysutils.DeleteFile(strpas(TmpName))
  end
  else
  begin
    if FileExists(tgt) then
    begin
      if not sysutils.DeleteFile(tgt) then
      begin
        FileSetNotReadOnlyOrHidden(tgt);
        if not sysutils.DeleteFile(tgt) then
        begin
          Result := False;
          DoStatus(972,tgt,WebUpdateReplaceError,ErrCannotDeleteFile);
          Log('Unable to delete file '+ tgt);
          Exit;
        end;
      end;
    end;
    sysutils.RenameFile(strpas(TmpName),tgt);
    Result := True;
  end;
end;

function TWebUpdate.URLGetFile(hfile:hinternet;url,tgt:string;uncompress:Boolean):Boolean;
var
  buf: array[0..READBUFFERSIZE - 1] of char;
  szHeaders: string;
  szUser,szPassword: string;
  tmpname: array[0..MAX_PATH] of char;
  bufsize: dword;
  lf: file;
  fn,furl: string;
  fsize: dword;
  hintfile,findfile,hreq,hconn: hinternet;
  lpdwlen,lpdwidx,lpdword: dword;
  s: string;
  DoCancel,dwnloaderror: Boolean;
  lpFindFileData:_WIN32_FIND_DATA;
  OpenFlags: DWORD;
label
  RetryAfterLogin;
begin
  Result := False;

  furl := url;
  fn := UrlToFile(url);

  if FHideURLInLogFile then
    DoStatus(915,fn,WebUpdateInformation,0)
  else
    DoStatus(915,furl,WebUpdateInformation,0);

  if FUpdateType = FileUpdate then
  begin
    if ExtractFileName(tgt) = '' then
      fn := tgt + fn
    else
      fn := tgt;

    if FileGetFile(PChar(url),PChar(fn),false) then
    begin
      CustomProcess(fn);
      if Uncompress and (Pos('.CAB',Uppercase(fn)) = 0) then
        ExpandFile(ansistring(fn));
      if Uncompress and (Pos('.CAB',Uppercase(fn)) <> 0) then
      begin
        DoStatus(965,fn,WebUpdateInformation,0);
        CABExtract(fn,tgt);
        if not FKeepIntermediateFiles then
          DeleteFile(pchar(fn));
      end;

      Result := True;
    end
    else
      Result := False;
    Exit;
  end;

  url := MakeProxyURL(url,FProxyUserID,FProxyPassword);

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar(url));
  {$ENDIF}

  if FUpdateType = FTPUpdate then
  begin
    if (FFTPDirectory <> '') and not FFTPDirSet then
    begin
      s := ExpandPath(FFTPDirectory);
      
      DoStatus(974,s,WebUpdateInformation,0);
      
      if not FtpSetCurrentDirectory(hfile,pchar(s)) then
        DoStatus(975,s,WebUpdateInformation,ErrCannotChangeDir);

      FFTPDirSet := True;
    end;

   //retrieve filesize
   findfile := FtpFindFirstFile(hfile,pchar(fn),lpFindFileData, INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE,0);

   if findfile <> nil then
     InternetCloseHandle(findfile);

   lpdword := lpFindFileData.nFileSizeLow;

   hintfile := FtpOpenFile(hfile,pchar(fn),GENERIC_READ,FTP_TRANSFER_TYPE_BINARY,0);
  end
  else
  begin
    if Pos('HTTPS',UpperCase(URL)) > 0 then
    begin

      hConn := InternetConnect(hfile, pchar(url), INTERNET_DEFAULT_HTTPS_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);

      hReq := HttpOpenRequest(hConn, 'GET', '/', nil, nil, nil, INTERNET_FLAG_SECURE, 0);

      if Assigned(hreq) then
      begin
        lpdword := SizeOf(OpenFlags);
        // Get the current flags
        if (InternetQueryOption(hReq, INTERNET_OPTION_SECURITY_FLAGS, @OpenFlags, lpdword)) then
        begin
          // Add desired flags
          OpenFlags := OpenFlags or SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
          // Set new flags
          if not(InternetSetOption(hReq, INTERNET_OPTION_SECURITY_FLAGS, @OpenFlags, lpdword)) then
            // Get error code
            Error;
        end;
                    // Send the request
        if HttpSendRequest(hReq, nil, 0, nil, 0) then
        begin

        end;
      end;

    end;


    OpenFlags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_PRAGMA_NOCACHE;

    if FKeepAlive then
      OpenFlags := OpenFlags or INTERNET_FLAG_KEEP_CONNECTION;

    if FExistingConnection then
      OpenFlags := OpenFlags or INTERNET_FLAG_EXISTING_CONNECT;

    if FCertCheck = ccDisable then
      OpenFlags := OpenFlags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID or INTERNET_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_UNKNOWN_CA;

    if (Authenticate = waAlways) and ((UserID = '') or (Password = '')) then
    begin
      szUser := UserID;
      szPassword := Password;

      if not GetLogin('Connect to '+URLToDomain(url),szUser, szPassword) then
      begin
        DoStatus(956,'Authentication cancelled',0,0);
        Exit;
      end;
      UserID := szUser;
      Password := szPassword;
    end;

retryafterlogin:

    if (UserID <> '') or (Password <> '') then
    begin
      szHeaders := 'Authorization: Basic ' + Base64Encode(UserID+':'+Password) + #13#10#13#10;
    end
    else
      szHeaders := '';

    hintfile := InternetOpenURL(hfile,PChar(url),PChar(szHeaders),length(szHeaders),OpenFlags,0);
    lpdwlen := 4;
    lpdwidx := 0;

    if hintfile <> nil then
    begin
      HttpQueryInfo(hintfile,HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER ,@lpdword,lpdwlen,lpdwidx);

      if (lpdword = 401) and (Authenticate = waAuto) then
      begin
        szUser := UserID;
        szPassword := Password;
        if GetLogin('Connect to '+URLToDomain(url),szUser, szPassword) then
        begin
          UserID := szUser;
          Password := szPassword;
          InternetCloseHandle(hintfile);
          goto RetryAfterLogin;
        end
        else
        begin
          DoStatus(956,'Authentication cancelled',0,0);
          Exit;
        end;
      end;

      if lpdword >= 300 then
      begin
        DoStatus(916,IntToStr(lpdword),WebUpdateHTTPStatus,lpdword);
        Exit;
      end;
    end;
  end;

  if hintfile = nil then
  begin
    Error;
    DoStatus(917,furl,WebUpdateNotFound,ErrUpdateFileNotFound);
    {$IFDEF TMSDEBUG}
    outputdebugstring('could not open file');
    {$ENDIF}
    Exit;
  end;

  fn := tgt + URLtoFile(url);

  if UseWinTempDir then
    GetTempFilename(PChar(WinTempDir),'WUPD',0,tmpname)
  else
    GetTempFilename(PChar(FTempDirectory),'WUPD',0,tmpname);

  AssignFile(lf,tmpname);
  Rewrite(lf,1);

  bufsize := READBUFFERSIZE;

  if FUpdateType = HTTPUpdate then
  begin
    lpdword := 0;
    lpdwlen := 4;
    lpdwidx := 0;

    HttpQueryInfo(hintfile,HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER ,@lpdword,lpdwlen,lpdwidx);
  end;

  FSize := 0;

  // initialize with global cancel state in case cancel was called from other method
  DoCancel := FCancelled;
  dwnloaderror := False;

  while (bufsize > 0) and not DoCancel do
  begin
    Application.ProcessMessages;

    if not InternetReadFile(hintfile,@buf,READBUFFERSIZE,bufsize) then
    begin
      dwnloaderror := True;
      Break;
    end;
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('read from http = '+inttostr(bufsize)));
    {$ENDIF}
    if (bufsize > 0) and (bufsize <= READBUFFERSIZE) then
      Blockwrite(lf,buf,bufsize);

    FSize := FSize + bufsize;

    HandleProgress(fn,fsize,lpdword);

    DoCancel := FCancelled;

    HandleCancel(DoCancel);

    if (bufsize > 0) then
      Result := True;
  end;

  InternetCloseHandle(hintfile);

  CloseFile(lf);

  FCancelled := DoCancel;

  if (FUpdateType = HTTPUpdate) and (lpdword > 0) and not DwnloadError then
  begin
    DwnloadError := FSize <> lpdword;
    if DwnloadError then
      DoStatus(908, url, WebUpdateDownloadError,ErrDownloadSizeWrong);
  end;

  if DoCancel or DwnloadError then
  begin
    if not SysUtils.DeleteFile(strpas(tmpname)) then
      Log('Unable to delete file '+strpas(tmpname));
    Result := False;
  end
  else
  begin
    if FileExists(fn) then
    begin
      if not SysUtils.DeleteFile(fn) then
      begin
        FileSetNotReadOnlyOrHidden(fn);
        if not SysUtils.DeleteFile(fn) then
        begin
          Result := false;
          DoStatus(972,fn,WebUpdateReplaceError,ErrCannotDeleteFile);
          Log('Unable to delete file '+fn);
        end;
      end;
    end;

    if not SysUtils.RenameFile(strpas(tmpname),fn) then
    begin
      Result := false;
      DoStatus(973,tmpname+' to ' + fn,WebUpdateRenameError,ErrCannotRenameFile);
      Log('Unable to rename file '+strpas(tmpname)+' to '+fn);
    end;

    CustomProcess(fn);

    if UnCompress and (UpperCase(ExtractFileExt(fn)) <> '.CAB') then
      ExpandFile(ansistring(fn));

    if UnCompress and (UpperCase(ExtractFileExt(fn)) = '.CAB') then
    begin
      CABExtract(fn,tgt);
      if not FKeepIntermediateFiles then
        DeleteFile(pchar(fn));
    end;
  end;
end;

procedure TWebUpdate.DoSetAppParamAfter(var Param: string);
begin
  if Assigned(OnSetAppParamAfter) then
    OnSetAppParamAfter(Self, Param);
end;

procedure TWebUpdate.DoSetAppParamBefore(var Param: string);
begin
  if Assigned(OnSetAppParamBefore) then
    OnSetAppParamBefore(Self, Param);
end;

procedure TWebUpdate.DoStatus(id:Integer;param:string;statuscode,errorcode:Integer);
var
  s: string;
begin
  s := LoadStr(id);
  
  if Logging then
    Log(FormatDateTime('dd/mm/yyyy hh:nn:ss',Now)+' : ['+IntToStr(id)+'] '+Format(s,[param])+' (Status:'+IntToStr(StatusCode)+') (Error:'+IntToStr(ErrorCode)+')');

  if Assigned(OnStatus) then
    OnStatus(Self,Format(s,[param]),StatusCode,ErrorCode);
end;


procedure TWebUpdate.ThreadDone(Sender: TObject);
begin
  if Assigned(FThreadUpdateDone) then
    FThreadUpdateDone(self);
  FThreaded := False;

  DoStatus(960,'',0,0);

  if AppNeedsRestart then
  begin
    DoStatus(961,'',0,0);
    DoRestart;
  end;
end;

procedure TWebUpdate.DoPostUpdateInfo;
var
  hint,hconn,hreq: hinternet;
  hdr: ansistring;
  accept: array[0..28] of char;
  buf: array[0..READBUFFERSIZE-1] of ansichar;
  bufsize, flags: dword;
  i: Integer;

begin
  hdr := 'Content-Type: application/x-www-form-urlencoded';

  if FPostUpdateInfo.Server = '' then
    Exit;

  if FPostUpdateInfo.Action = '' then
    Exit;

  StrpCopy(accept,'text/*');

  hint := InternetOpen(PChar(Agent),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);

  hconn := InternetConnect(hint,pchar(FPostUpdateInfo.Server),INTERNET_DEFAULT_HTTP_PORT,nil,nil,INTERNET_SERVICE_HTTP,0,1);

  if hconn <> nil then
  begin
    flags := INTERNET_FLAG_IGNORE_CERT_CN_INVALID or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
    hreq := HttpOpenRequest(hconn,'POST',pchar(FPostUpdateInfo.Action),nil,nil,nil,flags,1);

    if hreq <> nil then
    begin
      FPostUpdateInfo.PostResult := '';

      if HttpSendRequestA(hreq,pansichar(hdr),length(hdr),pansichar(FPostUpdateInfo.Data),length(FPostUpdateInfo.Data)) then
      begin
        bufsize := READBUFFERSIZE;
        while bufsize > 0 do
        begin
          Application.ProcessMessages;
          if not InternetReadFile(hreq,@buf,READBUFFERSIZE,bufsize) then
            Break;
          if (bufsize > 0) and (bufsize <= READBUFFERSIZE) then
            for i := 0 to bufsize - 1 do
              FPostUpdateInfo.PostResult := FPostUpdateInfo.PostResult + buf[i];
        end;
      end;
      InternetCloseHandle(hreq);
    end
    else
      DoStatus(952,'',WebUpdatePostPostFail,ErrConnectError);

    InternetCloseHandle(hconn);
  end
  else
    DoStatus(951,'',WebUpdatePostConnectFail,ErrConnectError);

  InternetCloseHandle(hint);
end;

function TWebUpdate.GetTextFile(URL:string): TStringList;
var
  fn: string;
begin
  Result := nil;

  if not URLGetFile(FHintConnect,URL,WinTempDir,False) then
    Exit;

  fn := WinTempDir + URLToFile(URL);

  Result := TStringList.Create;
  Result.LoadFromFile(fn);
  DeleteFile(PChar(fn));
end;

function TWebUpdate.GetUACEnabled: boolean;
begin
  {$IFDEF USEUAC}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TWebUpdate.GetStream(URL:string): TMemoryStream;
var
  fn: string;
begin
  Result := nil;

  if not URLGetFile(FHintConnect,URL,WinTempDir,False) then
    Exit;

  fn := WinTempDir + URLToFile(URL);

  Result := TMemoryStream.Create;
  Result.LoadFromFile(fn);
  DeleteFile(PChar(fn));
end;


function TWebUpdate.GetWhatsNew: TStringList;
var
  IniFile: TMemIniFile;
  s: string;
begin
  Result := nil;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    s := IniFile.ReadString('whatsnew'+LanguageID,'file','');
    if s <> '' then
    begin
      if Assigned(FOnBeforeFileDownload) then
        FOnBeforeFileDownload(Self,0,s,s);

      DoStatus(937,'',WebUpdateWhatsNew,0);
      Result := GetTextFile(s);
    end;
  finally
    IniFile.Free;
  end;
end;

function TWebUpdate.GetWhatsNewStream: TMemoryStream;
var
  IniFile: TMemIniFile;
  s: string;
begin
  Result := nil;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    s := IniFile.ReadString('whatsnew'+LanguageID,'file','');
    if s <> '' then
    begin
      if Assigned(FOnBeforeFileDownload) then
        FOnBeforeFileDownload(Self,0,s,s);

      DoStatus(937,'',WebUpdateWhatsNew,0);
      Result := GetStream(s);
    end;
  finally
    IniFile.Free;
  end;
end;

function TWebUpdate.GetEULA: TStringList;
var
  IniFile: TMemIniFile;
  s: string;
begin
  Result := nil;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    s := IniFile.ReadString('eula'+LanguageID,'file','');
    if s <> '' then
    begin
      DoStatus(939,'',WebUpdateEUL,0);
      Result := GetTextFile(s);
    end;
  finally
    IniFile.Free;
  end;  
end;

function TWebUpdate.GetEULAStream: TMemoryStream;
var
  IniFile: TMemIniFile;
  s: string;
begin
  Result := nil;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    s := IniFile.ReadString('eula'+LanguageID,'file','');
    if s <> '' then
    begin
      DoStatus(939,'',WebUpdateEUL,0);
      Result := GetStream(s);
    end;
  finally
    IniFile.Free;
  end;
end;

function TWebUpdate.IsRTF(ms: TMemoryStream): boolean;
var
  s: ansistring;
begin
  SetLength(s, 10);
  ms.Position := 0;
  ms.Read(s[1], 5);
  Result := pos(ansistring('{\RTF'),Uppercase(string(s))) = 1;
  ms.Position := 0;
end;

function TWebUpdate.WhatsNewDialog(ms: TMemoryStream):Integer;
var
  form: TForm;
  memo: TMemo;
  rich: TRichEdit;
  ok,cancel: TButton;
  Res: Integer;
  Caption,okbtn,cancelbtn:string;
  IniFile: TMemIniFile;
  sl: TStringList;
  RTF: boolean;
  ctrlheight,ctrlwidth: integer;
  ctrltop, ctrlleft: integer;
begin
  Result := WU_SUCCESS;

  RTF := IsRTF(ms);

  if Assigned(FOnDownloadedWhatsNew) and not RTF then
  begin
    Res := mrOK;
    sl := TStringList.Create;
    sl.LoadFromStream(ms);

    FOnDownloadedWhatsNew(Self,sl, Res);
    if (Res <> mrOk) then
      Result := WU_FAILED;

    sl.Free;
    Exit;
  end;

  IniFile := TMemIniFile.Create(ControlFilename);

  caption := IniFile.ReadString('whatsnew','caption','');
  okbtn := IniFile.ReadString('whatsnew','okbtn','Ok');
  cancelbtn := IniFile.ReadString('whatsnew','cancelbtn','Cancel');

  if caption = '' then
    caption := 'What''s new';

  IniFile.Free;

  form := TForm.Create(Application);
  form.Position := poScreenCenter;

  form.Width := 400;
  form.Height := 400;
  form.Caption := Caption;
  form.BorderStyle := bsDialog;
  form.BorderIcons := [biSystemMenu];

  if RTF then
  begin
    rich := TRichEdit.Create(form);
    rich.Parent := form;
    rich.Top := 10;
    rich.Left := 10;
    rich.Width := form.ClientRect.Right - 20;
    rich.Height := form.ClientRect.Bottom - 60;
    rich.Lines.LoadFromStream(ms);
    rich.Readonly := true;
    rich.WordWrap := true;
    rich.ScrollBars := ssVertical;
    ctrlwidth := rich.Width;
    ctrlheight := rich.Height;
    ctrltop := rich.Top;
    ctrlleft := rich.Left;
  end
  else
  begin
    memo := TMemo.Create(form);
    memo.Parent := form;
    memo.Top := 10;
    memo.Left := 10;
    memo.Width := form.ClientRect.Right - 20;
    memo.Height := form.ClientRect.Bottom - 60;
    memo.Lines.LoadFromStream(ms);
    memo.Readonly := true;
    memo.WordWrap := true;
    memo.ScrollBars := ssVertical;
    ctrlwidth := memo.Width;
    ctrlheight := memo.Height;
    ctrltop := memo.Top;
    ctrlleft := memo.Left;
  end;

  ok := TButton.Create(form);
  ok.Parent := form;
  ok.Caption := okbtn;
  ok.Top := ctrlheight + ctrltop + 10;
  ok.Left := ctrlWidth + ctrlleft - ok.Width - ok.Width - 10;
  ok.TabOrder := 0;
  ok.ModalResult := mrOk;

  cancel := TButton.Create(form);
  cancel.Parent := form;
  cancel.Caption := cancelbtn;
  cancel.Top := ctrlheight + ctrltop + 10;
  cancel.Left := ctrlwidth + ctrlleft - ok.Width;
  cancel.ModalResult := mrCancel;
  cancel.TabOrder := 1;

  res := form.ShowModal;

  if res = mrCancel then
  begin
    DoStatus(938,'',WebUpdateWhatsNewCancel,0);
    Result := WU_FAILED;
  end;

  form.Free;
end;


function TWebUpdate.EULADialog(ms: TMemoryStream):Integer;
var
  form: TForm;
  memo: TMemo;
  rich: TRichEdit;
  ok: TButton;
  r1,r2: TRadioButton;
  Res: Integer;
  Caption,okbtn,accept,noaccept:string;
  IniFile: TMemIniFile;
  RTF: boolean;
  sl: TStringList;
  ctrlheight,ctrlwidth: integer;
  ctrltop, ctrlleft: integer;

begin
  Result := WU_SUCCESS;

  RTF := IsRTF(ms);

  // always trigger this event
  if Assigned(FOnDownloadedEULA) {and RTF} then
  begin
    sl := TStringList.Create;
    sl.LoadFromStream(ms);

    FOnDownloadedEULA(Self,sl,Res);
    if (Res <> mrOk) then
      Result := WU_FAILED;
    sl.Free;

    if RTF then
      Exit;
  end;

  IniFile := TMemIniFile.Create(ControlFileName);

  Caption := IniFile.ReadString('eula','caption','');
  okbtn := IniFile.ReadString('eula','okbtn','Ok');
  accept := IniFile.ReadString('eula','accept','I accept');
  noaccept := IniFile.ReadString('eula','notaccept','I do not accept');

  IniFile.Free;


  form := TForm.Create(application);
  form.Position := poScreenCenter;

  form.Width := 400;
  form.Height := 400;
  form.Caption := Caption;
  form.BorderStyle := bsDialog;
  form.BorderIcons := [biSystemMenu];

  if RTF then
  begin
    rich := TRichEdit.Create(form);
    rich.Parent := form;
    rich.Top := 10;
    rich.Left := 10;
    rich.Width := form.ClientRect.Right - 20;
    rich.Height := form.ClientRect.Bottom - 60;
    rich.Lines.LoadFromStream(ms);
    rich.Readonly := true;
    rich.WordWrap := true;
    rich.ScrollBars := ssVertical;
    ctrlwidth := rich.Width;
    ctrlheight := rich.Height;
    ctrltop := rich.Top;
    ctrlleft := rich.Left;
  end
  else
  begin
    memo := TMemo.Create(form);
    memo.Parent := form;
    memo.Top := 10;
    memo.Left := 10;
    memo.Width := form.ClientRect.Right - 20;
    memo.Height := form.ClientRect.Bottom - 60;
    memo.Lines.LoadFromStream(ms);
    memo.ReadOnly := True;
    memo.WordWrap := True;
    memo.ScrollBars := ssVertical;
    ctrlwidth := memo.Width;
    ctrlheight := memo.Height;
    ctrltop := memo.Top;
    ctrlleft := memo.Left;
  end;

  ok := TButton.Create(form);
  ok.Parent := form;
  ok.Caption := okbtn;
  ok.Top := ctrlheight + ctrltop + 10;
  ok.Left := ctrlwidth + ctrlleft - ok.Width;
  ok.TabOrder := 0;
  ok.ModalResult := mrOk;
  ok.Enabled := True;

  r1 := TRadioButton.Create(form);
  r1.Parent := form;
  r1.Left:=10;
  r1.Top := ctrlheight + ctrltop + 10;
  r1.Caption := Accept;
  r1.TabOrder := 1;

  r2 := TRadioButton.Create(form);
  r2.Parent := form;

  r2.Left := 20 + r1.Width;
  r2.Top := ctrlheight + ctrltop + 10;
  r2.Caption := NoAccept;
  r2.TabOrder := 2;
  r2.Checked := true;

  res := form.ShowModal;

  if not r1.Checked then
    Result := WU_FAILED;

  form.Free;
end;

procedure TWebUpdate.DoThreadUpdate(InitPath: boolean);
begin
  FThreaded := True;
  with TInetThread.Create(Self,InitPath) do
  begin
    OnTerminate := ThreadDone;
  end;
end;

procedure TWebUpdate.DoThreadUpdate;
begin
  DoThreadUpdate(true);
end;

function ForceDirectories(Dir: string): boolean;
begin
  Result := True;
  if Length(Dir) = 0 then Exit;
  if Copy(Dir,Length(Dir),1) = '\' then
    SetLength(Dir, Length(Dir) - 1);
  if (Length(Dir) < 3) or DirExists(Dir) or (ExtractFilePath(Dir) = Dir) then
    Exit;
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

procedure TWebUpdate.SetTimeout(timeout: dword);
var
  timos: dword;
begin
  timos := sizeof(timeout);
  InternetSetOption(FHint, INTERNET_OPTION_CONNECT_TIMEOUT, @timeout, timos);
end;

procedure TWebUpdate.SetReceiveTimeout(receivetimeout: dword);
var
  timos: dword;
begin
  timos := sizeof(receivetimeout);
  InternetSetOption(FHint, INTERNET_OPTION_RECEIVE_TIMEOUT, @receivetimeout, timos);
end;


function TWebUpdate.StartConnection: Integer;
var
  dwReserved: DWORD;
  dwFlags, dwFlagsSz: DWORD;

begin
  Result := WU_SUCCESS;
  dwReserved := 0;

  FHint := nil;
  FHintconnect := nil;

  {$IFDEF FREEWARE}
  ShowMessage('Starting application update with'#13#10'TWebUpdate © 1998-2012 by tmssoftware.com');
  {$ENDIF}

  if FUpdateType <> FileUpdate then
  begin
    DoStatus(918,'',WebUpdateInformation,0);

    if FUpdateConnect = wucNone then
    begin
      Result := WU_SUCCESS;
      Exit;
    end;

    if not Connected and (FUpdateConnect = wucNoConnect) then
    begin
      Result := WU_NOCONNECTION;
      Exit;
    end;

    if not Connected and (FUpdateConnect in [wucConnectPrompt,wucConnectPromptHangup]) then
    begin
      DoStatus(919,'',WebUpdateInformation,0);

      if not DynaLink_InternetAutodial(INTERNET_AUTODIAL_FORCE_ONLINE,dwReserved) then
      begin
        Result := WU_DIALUPFAILED;
        DoStatus(953,'',WebUpdateInformation,0);
        Exit;
      end;
      FDialed := True;
    end;

    if not Connected and (FUpdateConnect in [wucConnectSilent,wucConnectSilentHangup]) then
    begin
      DoStatus(920,'',WebUpdateInformation,0);
      if not DynaLink_InternetAutodial(INTERNET_AUTODIAL_FORCE_UNATTENDED,dwReserved) then
      begin
        Result := WU_DIALUPFAILED;
        DoStatus(953,'',WebUpdateInformation,0);
        Exit;
      end;
      FDialed := True;
    end;

    DoStatus(921,'',WebUpdateInformation,0);

    // ftp & http common code
    if FProxy = '' then
      FHint := InternetOpen(PChar(Agent),INTERNET_OPEN_TYPE_PRECONFIG {or INTERNET_FLAG_ASYNC},nil,nil,0)
    else
      FHint := InternetOpen(PChar(Agent),INTERNET_OPEN_TYPE_PROXY {or INTERNET_FLAG_ASYNC},pchar(FProxy),nil,0);

    { Fails on NT ???????
    if (fProxyUserID <>'') then
      InternetSetOption(hint,INTERNET_OPTION_PROXY_USERNAME,pchar(fProxyUserID),length(fProxyUserID));
    if (fProxyPassword <>'') then
      InternetSetOption(hint,INTERNET_OPTION_PROXY_PASSWORD,pchar(fProxyPassword),length(fProxyPassword));
    }

    if FHint = nil then
    begin
      DoStatus(922,'',WebUpdateAccessError,0);

      Result := WU_INTERNETOPENFAILED;
    end
    else
    begin
      if FTimeOut > 0 then
        SetTimeOut(FTimeOut);
      if FReceiveTimeOut > 0 then
        SetReceiveTimeOut(FReceiveTimeOut);

      if FCertCheck = ccDisable then
      begin
        dwFlags := SECURITY_FLAG_IGNORE_UNKNOWN_CA or SECURITY_FLAG_IGNORE_CERT_CN_INVALID or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
        dwFlagsSz := sizeof(dwFlags);
        InternetSetOption(FHint, INTERNET_OPTION_SECURITY_FLAGS, @dwflags, dwflagsSz);
      end;

      if FUpdateType = httpUpdate then
        FHintConnect := FHint;
      Result := WU_SUCCESS;
      FInProgress := True;
    end;
  end;
end;

function TWebUpdate.FTPConnect: Integer;
var
  ftpflag: Integer;
begin
  FFTPDirSet := False;

  DoStatus(923,fHost,WebUpdateInformation,0);

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  if (FUserID = '') or (FPassword = '') then
    FHintConnect := InternetConnect(FHint,pchar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
  else
    FHintconnect := InternetConnect(FHint,pchar(FHost),FPort,pchar(FUserID),pchar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);

  if FHintconnect <> nil then
    Result := WU_SUCCESS
  else
    Result := WU_FAILED;  
end;

function TWebUpdate.StopConnection: Integer;
var
  dwReserved: DWORD;
begin
  Result := WU_SUCCESS;
  dwReserved := 0;

  if (FUpdateType <> fileUpdate) then
  begin

    if (FUpdateType = ftpUpdate) and (FHintConnect <> nil) then
    begin
      InternetCloseHandle(FHintConnect);
      DoStatus(954,'',WebUpdateInformation,0);
      FHintConnect := nil;
    end;

    if FHint <> nil then
    begin
      InternetCloseHandle(FHint);
      DoStatus(949,'',WebUpdateInformation,0);
      FHint := nil;
    end;

    if FDialed and (FUpdateConnect in [wucConnectPromptHangup,wucConnectSilentHangup]) then
    begin
      DynaLink_InternetAutoDialHangup(dwReserved);
      DoStatus(955,'',WebUpdateInformation,0);
    end;

    if FDialed and (FUpdateConnect in [wucConnectPromptHangup,wucConnectSilentHangup]) then
      DynaLink_InternetAutoDialHangup(dwReserved);

    DoStatus(949,'',WebUpdateInformation,0);
  end;

  if ControlFileName <> '' then
    SysUtils.DeleteFile(ControlFileName);
  FInProgress := False;
end;

function TWebUpdate.GetControlFileURL: string;
begin
  Result := URLGet;
  if (Result = '') then
    Result := FURL;
  Result := ExpandPath(Result);
end;

function TWebUpdate.GetControlFile: Integer;
var
  ctrlURL: string;
  IniFile: TMemIniFile;

begin
  // get the .INF file here
  
  Result := WU_SUCCESS;
  FCancelled := False;

  ctrlURL := GetControlFileURL;

  if (ctrlURL = '') or not URLGetFile(FHintconnect,ctrlURL,WinTempDir,False) then
  begin
    DoStatus(925,'',WebUpdateNotFound,ErrControlFileNotFound);
    Result := WU_FILENOTFOUND;
    Exit;
  end;

  DoStatus(926,'',WebUpdateInformation,0);

  FControlFileName := WinTempDir + URLtoFile(ctrlURL);

  IniFile := TMemIniFile.Create(FControlFileName);

  if FSignatureCheck then
  begin
    if FSignature <> IniFile.ReadString('update','signature','') then
    begin
      DoStatus(927,'',WebUpdateSignatureError,ErrUpdateSignatureError);
      Result := WU_SIGNATUREFAILED;
    end;
  end;

  // get application related settings
  FAppClose := IniFile.ReadInteger('application','appupdate',0) = 1;

  FAppName := ExpandPath(IniFile.ReadString('application','appname',''));
  FAppParam := IniFile.ReadString('application','appparam','');

  if FAppName = '' then
    FAppClose := False;

  if Assigned(FSetAppParams) then
    FSetAppParams(Self,FAppParam);

  if FAppParam = '' then
    FAppParam := ' ';

  FAppComps := ExpandPath(IniFile.ReadString('application','appcomps',''));
  FSilentRestart := IniFile.ReadInteger('application','silentrestart',0)=1;

  IniFile.Free;
end;

function TWebUpdate.ControlValueToDate(d,t:string): TDateTime;
var
  oldfmt, s:string;
  oldsep: Char;
  dt: TDateTime;
begin
  dt := EncodeDate(1980,1,1);
  oldfmt := ShortDateFormat;
  {$IFNDEF DELPHIXE_LVL}
  oldsep := SysUtils.DateSeparator;
  {$ENDIF}
  {$IFDEF DELPHIXE_LVL}
  oldsep := FormatSettings.DateSeparator;
  {$ENDIF}
  try
    try
      {$IFNDEF DELPHIXE_LVL}
      Sysutils.ShortDateFormat := FDateFormat;
      Sysutils.DateSeparator := FDateSeparator;
      {$ENDIF}
      {$IFDEF DELPHIXE_LVL}
      FormatSettings.ShortDateFormat := FDateFormat;
      FormatSettings.DateSeparator := FDateSeparator;
      {$ENDIF}
      dt := StrToDate(d);
    except
      s := '';
    end;
  finally
    {$IFNDEF DELPHIXE_LVL}
    SysUtils.ShortDateFormat := oldfmt;
    SysUtils.DateSeparator := oldsep;
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    FormatSettings.ShortDateFormat := oldfmt;
    FormatSettings.DateSeparator := oldsep;
    {$ENDIF}
  end;

  if (t <> '') then
  begin
    oldfmt := ShortTimeFormat;
    {$IFNDEF DELPHIXE_LVL}
    oldsep := SysUtils.TimeSeparator;
    {$ENDIF}
    {$IFDEF DELPHIXE_LVL}
    oldsep := FormatSettings.TimeSeparator;
    {$ENDIF}
    try
      try
        {$IFNDEF DELPHIXE_LVL}
        Sysutils.ShortTimeFormat := FTimeFormat;
        Sysutils.TimeSeparator := FTimeSeparator;
        {$ENDIF}
        {$IFDEF DELPHIXE_LVL}
        FormatSettings.ShortTimeFormat := FTimeFormat;
        FormatSettings.TimeSeparator := FTimeSeparator;
        {$ENDIF}
        dt := dt + StrToTime(t);
      except
        s := '';
      end;
    finally
      {$IFNDEF DELPHIXE_LVL}
      SysUtils.ShortTimeFormat := oldfmt;
      SysUtils.TimeSeparator := oldsep;
      {$ENDIF}
      {$IFDEF DELPHIXE_LVL}
      FormatSettings.ShortTimeFormat := oldfmt;
      FormatSettings.TimeSeparator := oldsep;
      {$ENDIF}
    end;
  end;

  Result := dt;
end;

function TWebUpdate.DoVersionCheck: Integer;
var
  IniFile: TMemIniFile;
  fvl,fvn,descr:string;
  fvs,fvi:Integer;
  s: string;
  CustomVal: Boolean;

begin
  Result := WU_NONEWVERSION;

//  if GetControlFile <> WU_SUCCESS then // added code -> should call first GetControlFile, then DoVersionCheck
//    Exit;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    descr := IniFile.ReadString('update','descr','');
    FUpdateDescription := CLFToLF(descr);

    // check for date based update
    s := IniFile.ReadString('update','date','');
    if s <> '' then
    begin
      DoStatus(928,'',WebUpdateInformation,0);

      FNewVersionDate := ControlValueToDate(s,IniFile.ReadString('update','time',''));
      FLocalFileDateCheck := ExpandPath(IniFile.ReadString('update','localversion',''));

      FCurVersionDate := UpdateDateGet;

      if FCurVersionDate < FNewVersionDate then
      begin
        // SaveUpdate := True;
        // Updatedt := dt;
        DoStatus(929,DateToStr(FNewVersionDate),WebUpdateNewVersion,0);
        Result := WU_DATEBASEDNEWVERSION;
      end
      else
      begin
        DoStatus(930,'',WebUpdateNoNewVersion,0);
        Result := WU_NONEWVERSION;
      end;
    end;

    // check for unconditional update
    s := IniFile.ReadString('update','unconditional','');
    if (s <> '') and (Result = WU_NONEWVERSION) then
    begin
      Result := WU_UNCONDITIONALNEWVERSION;
    end;

    // check for file version based update
    if (Result = WU_NONEWVERSION) then
    begin
      fvn := IniFile.ReadString('update','newversion','');
      fvs := IniFile.ReadInteger('update','newsize',0);
      fvl := ExpandPath(IniFile.ReadString('update','localversion',''));
      fvi := IniFile.ReadInteger('update','newchecksum',-1);

      if ((fvn <> '') or (fvs > 0) or (fvi <> -1)) and (fvl <> '') then
      begin
        if (fvi <> -1) then
        begin
          Result := WU_CHECKSUMBASEDNEWVERSION;
          DoStatus(931,'',WebUpdateInformation,0);
        end
        else
        begin
          if (fvs > 0) then
          begin
            Result := WU_FILESIZEBASEDNEWVERSION;
            DoStatus(932,'',WebUpdateInformation,0)
          end
          else
          begin
            Result := WU_VERSIONINFOBASEDNEWVERSION;
            DoStatus(933,'',WebUpdateInformation,0);
          end;
        end;

        if not CheckVersions(fvn,fvl,fvs,fvi) then
        begin
          DoStatus(930,'',WebUpdateNoNewVersion,0);
          Result := WU_NONEWVERSION;
        end
        else
          DoStatus(929,fvn,WebUpdateNewVersion,0);

      end;

      FNewVersionInfo := fvn;
      FCurVersionInfo := fvl;
    end;

    // check for custom validation
    if Assigned(FCustomValidate) then
    begin
      fvl := IniFile.ReadString('custom','validatemsg','');
      fvn := IniFile.ReadString('custom','validateparam','');
      CustomVal := not (Result = WU_NONEWVERSION);
      FCustomValidate(self,fvl,fvn,customval);

      if not CustomVal then
        Result := WU_NONEWVERSION
      else
        Result := WU_CUSTOMNEWVERSION;
    end;
  finally
    IniFile.Free;
  end;  
end;

function TWebUpdate.HandleActions: Integer;
var
  s,p: string;
  fvl,fvn: string;
  IniFile: TMemIniFile;
  {$IFDEF DELPHI_UNICODE}
  sa: ansistring;
  {$ENDIF}
begin
  Result := WU_SUCCESS;

  IniFile := TMemIniFile.Create(ControlFilename);

  try
    s := CLFToLF(IniFile.ReadString('action','msg',''));
    if s <> '' then
    begin
      DoStatus(934,'',WebUpdateInformation,0);
      MessageDlg(s,mtinformation,[mbok],0);
    end;

    s := ExpandPath(IniFile.ReadString('action','htmldlg',''));
    if s <> '' then
    begin
      DoStatus(935,'',WebUpdateHTMLDialog,0);
      HTMLDialog(0,s);
    end;

    s := ExpandPath(IniFile.readstring('action','runbefore',''));
    p := IniFile.readstring('action','runbeforeparam','');

    DoSetAppParamBefore(p);

    if s <> '' then
    begin
      {$IFDEF DELPHI_UNICODE}
      sa := ansistring(s);
      if p <> '' then
        sa := sa + ' ' + ansistring(p);

      if IsVista then
        ShellExecute(0,'open',PChar(s), PChar(p), nil, SW_NORMAL)
      else
        WinExec(PAnsiChar(sa),sw_normal);
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if IsVista then
        ShellExecute(0,'open',PChar(s), PChar(p), nil, SW_NORMAL)
      else
      begin
        if p <> '' then
          s := s + ' ' + p;
        WinExec(PChar(s),SW_NORMAL);
      end;
      {$ENDIF}      
    end;

    if Assigned(FCustomProcess) then
    begin
      fvl := IniFile.ReadString('custom','processmsg','');
      fvn := IniFile.ReadString('custom','processparam','');
      FCustomProcess(self,fvl,fvn);
    end;

    s := ExpandPath(IniFile.readstring('action','showURL',''));
    if s <> '' then
    begin
      DoStatus(942,'',WebUpdateInformation,0);
      s := MakeProxyURL(s,fProxyUserID,fProxyPassword);
  
      if (ShellExecute(0, 'open', 'iexplore.exe',pchar('-new '+s),nil,SW_SHOWDEFAULT) <= 32) {was an error} then
        ShellExecute(0, 'open',pchar(s),nil,nil,SW_SHOWDEFAULT);
    end;

    s := CLFToLF(IniFile.ReadString('action','query',''));
    if s <> '' then
    begin
      DoStatus(936,'',WebUpdateInformation,0);
      if MessageDlg(s,mtinformation,[mbyes,mbno],0) <> mrYes then
      begin
        FAppClose := False;
        Result := WU_FAILED;
      end;
    end;

  finally
    IniFile.Free;
  end;

  if FCancelled then
    Result := WU_FAILED; 
end;

{
    lst := TStringlist.Create;
    if Assigned(FFileList) then
    begin
      j := StrToInt(s);
      for i := 1 to j do
        lst.Add(IniFile.ReadString('file'+inttostr(i),'descr',''));
      FFileNameList(Self,lst);
    end;

      if Assigned(FFileList) then
      begin
        fvn := lst.Strings[i - 1];
      end
      else
        fvn := 'ok';

}

function TWebUpdate.GetFileDetails: Integer;
var
  num,i: Integer;
  IniFile: TMemIniFile;
  filekey: string;
begin
  FFileList.Clear;

  IniFile := TMemIniFile.Create(ControlFilename);

  num := IniFile.ReadInteger('files','count',0);
  if num > 0 then
  begin
    DoStatus(943,'',WebUpdateInformation,0);

    for i := 1 to num do
    begin
      filekey := 'file'+IntToStr(i);
      with FFileList.Add do
      begin
        URL := ExpandPath(IniFile.ReadString(filekey,'url',''));
        LocalVersion := ExpandPath(IniFile.ReadString(filekey,'localversion',''));
        NewVersion := IniFile.ReadString(filekey,'newversion','');
        NewSize := IniFile.ReadInteger(filekey,'newsize',0);
        NewChecksum := IniFile.ReadInteger(filekey,'newchecksum',-1);
        NewCustomVer := IniFile.ReadString(filekey,'customversion','');
        NewDate := IniFile.ReadString(filekey,'date','');
        NewTime := IniFile.ReadString(filekey,'time','');
        Description := IniFile.ReadString(filekey,'descr','');
        TargetDir := AddBackslash(ExpandPath(IniFile.ReadString(filekey,'targetdir','')));
        Compressed := IniFile.ReadInteger(filekey,'compressed',0) = 1;
        FileSize := IniFile.ReadInteger(filekey,'filesize',0);
        Mandatory := IniFile.ReadInteger(filekey,'mandatory',0)=1;
        Preselect := IniFile.ReadInteger(filekey,'preselect',1)=1;
        Hidden := IniFile.ReadInteger(filekey,'hidden',0)=1;
      end;
    end;
  end;

  Result := num;
  IniFile.Free;
end;

function TWebUpdate.ProcessFileDetails: Integer;
var
  i: Integer;
  isNew: Boolean;
  fvn,fvl: string;
  lst: TStringList;
  NewDt,CurDt: TDateTime;
begin
  Result := WU_SUCCESS;
  i := 0;

  while i < FFileList.Count do
  begin
    fvn := FFileList.Items[i].NewVersion;
    fvl := FFileList.Items[i].LocalVersion;

    if FHideURLInLogFile then
      DoStatus(971,
        UrlToFile(FFileList.Items[i].URL) + '[' +fvl+
          ',nv='+FFileList.Items[i].NewVersion+
          ',nd='+FFileList.Items[i].NewDate+
          ',ns='+inttostr(FFileList.Items[i].NewSize)+
          ',nc='+inttostr(FFileList.Items[i].NewCheckSum)+']',0,0)
    else
      DoStatus(971,
        FFileList.Items[i].URL + '[' +fvl+
          ',nv='+FFileList.Items[i].NewVersion+
          ',nd='+FFileList.Items[i].NewDate+
          ',ns='+inttostr(FFileList.Items[i].NewSize)+
          ',nc='+inttostr(FFileList.Items[i].NewCheckSum)+']',0,0);

    if (fvl <> '') and
       (FFileList.Items[i].NewDate <> '') then
    begin
      NewDt := ControlValueToDate(FFileList.Items[i].NewDate,FFileList.Items[i].NewTime);

      if FileExists(fvl) then
      begin
        {$IFDEF DELPHI2006_LVL}
        FileAge(fvl, CurDt);
        {$ELSE}
        CurDt := SysUtils.FileDateToDateTime(SysUtils.FileAge(fvl));
        {$ENDIF}

        if CurDt >= NewDt then
          FFileList.Items[i].Free
        else
          inc(i);
      end
      else
        inc(i);
    end
    else
    begin
      if not CheckVersions(fvn,fvl,
                           FFileList.Items[i].NewSize,
                           FFileList.Items[i].NewCheckSum) then
      begin
        DoStatus(930,fvn +' removed from update list',0,0);
        FFileList.Items[i].Free
      end
      else
      begin
        isNew := True;
        if Assigned(FWebUpdateFileVersionCheck) and (FFileList.Items[i].NewCustomVer <> '') then
            FWebUpdateFileVersionCheck(Self,FFileList.Items[i].NewCustomVer, fvl ,IsNew);

        if not IsNew then
          FFileList.Items[i].Free
        else
          inc(i);
      end;
    end;
  end;

  if Assigned(FFileNameList) then
  begin
    lst := TStringlist.Create;

    for i := 1 to FFileList.Count do
      lst.Add(FFileList.Items[i - 1].Description);
    FFileNameList(Self,lst);

    i := 0;

    while (i < FFileList.Count) do
    begin
      if (i < lst.Count) and (lst.Strings[i] = '') then
      begin
        if not FFileList.Items[i].Mandatory then
          FFileList.Items[i].Free;
        lst.Delete(i);
      end
      else
        inc(i);
    end;

    lst.Free;
  end;

  if FFileList.Count = 0 then
    Result := WU_FAILED;
end;


function TWebUpdate.GetFileUpdates: Integer;
var
  i: Integer;
  UpdateMethod: TWebUpdateUpdate;
  FName, UFName: string;
  ShowUtil: Boolean;
  ShowAnim: Boolean;
  szCaption, szMsg: string;
  tgtdir: string;
  inAppComps: boolean;
  ures: boolean;
begin
  UpdateMethod := FUpdateUpdate;

  Result := WU_SUCCESS;

  for i := 1 to FFileList.Count do
  begin
    if (UpdateMethod = wuuSilent) or
       ((UpdateMethod = wuuPromptAll) and
       (MessageDlg(LoadStr(909) + UrlToFile(FFileList.Items[i - 1].URL),mtConfirmation,[mbYes,mbNo],0) = mrYes)) or
       ((UpdateMethod = wuuPromptOnce) and
       (MessageDlg(LoadStr(910),mtConfirmation,[mbYes,mbNo],0) = mrYes)) then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('download '+s));
      {$ENDIF}

      FName := FFileList.Items[i - 1].URL;
      UFName := URLToFile(FName);

      FFileList.FActiveItem := i - 1;

      if Assigned(FOnBeforeFileDownload) then
        FOnBeforeFileDownload(Self,i,FFileList.Items[i - 1].Description,FName);

      // create directory if required
      if (FFileList.Items[i - 1].TargetDir <> '') and not DirExists(FFileList.Items[i - 1].TargetDir) then
        ForceDirectories(FFileList.Items[i - 1].TargetDir);

      if UseWinTempDir then
        tgtdir := WinTempDir
      else
        tgtdir := FFileList.Items[i - 1].TargetDir;

      if Assigned(FProgressForm) then
        FProgressForm.Show;

      ures := URLGetFile(FHintconnect, Fname, tgtdir, false);

      if Assigned(FProgressForm) then
        FProgressForm.Hide;

      if not ures then
      begin
         Result := WU_FAILED;
         DoStatus(946,FName,WebUpdateNotFound,ErrDownloadError);
         FAppclose := False;
      end
      else
      begin
        inAppComps := false;

        if (IPos(UFName,FAppComps) > 0) then
        begin
          FAppCompsIncluded := True;
          inAppComps := True;
          DoStatus(962,UFName,0,0);
        end;

        if (UpperCase(ExtractFileExt(UFName)) <> '.CAB') and
           (UpperCase(ExtractFileExt(UFName)) <> '.PAT') and
           (FUseWinTempDir) and
           (FFileList.Items[i - 1].TargetDir <> '') and not inAppComps and
           (CompareStr(AddBackSlash(FFileList.Items[i - 1].TargetDir) + UFName, tgtdir + UFName) <> 0) then
        begin
          if FileExists(AddBackSlash(FFileList.Items[i - 1].TargetDir) + UFName) then
            SysUtils.DeleteFile(AddBackSlash(FFileList.Items[i - 1].TargetDir) + UFName);

          RenameFile(tgtdir + UFName, AddBackSlash(FFileList.Items[i - 1].TargetDir) + UFName);
        end;  

        if (UpperCase(ExtractFileExt(UFName)) <> '.CAB') and
          FExtractCAB and FFileList.Items[i - 1].Compressed and
          not inAppComps then
        begin
          RenameFile(tgtdir + UFName, UFName);
          ExpandFile(ansistring(UFName));
          if not FKeepIntermediateFiles then
            DeleteFile(PChar(UFName));
        end;

        if (UpperCase(ExtractFileExt(UFName)) = '.CAB') and
          FExtractCAB and FFileList.Items[i - 1].Compressed and
          not inAppComps then
        begin
          if CABExtract(AddBackSlash(tgtdir) + UFName, FFileList.Items[i - 1].TargetDir) = -1 then
            DoStatus(947,AddBackSlash(tgtdir) + UFName,WebUpdateCABError,0)
          else
          begin
            if not FKeepIntermediateFiles then
              DeleteFile(PChar(AddBackSlash(tgtdir) + UFName));
          end;
        end;

        if (UpperCase(ExtractFileExt(UFName)) = '.PAT') and
          FApplyPatch and (FFileList.Items[i - 1].LocalVersion <> '') and not inAppComps then
        begin
          if Assigned(FUtility) and FApplyPatch and not FileExists('patcher.exe') then
          begin
            ExtractUtility;
          end;

          ShowUtil := true;
          ShowAnim := false;
          szCaption := '';
          szMsg := '';

          if Assigned(FUtility) then
          begin
            ShowUtil := FUtility.ShowWindow;
            ShowAnim := FUtility.ShowAnimation;
            szCaption := FUtility.StatusCaption;
            szMsg := FUtility.StatusMessage;
          end;

          if FLogging then
            ExecAndWait('patcher.exe -v ' + UFName + ' ' + FFileList.Items[i - 1].LocalVersion + '>> WUPDATE.LOG',ShowUtil, ShowAnim, szCaption, szMsg)
          else
            ExecAndWait('patcher.exe ' + UFName + ' ' + FFileList.Items[i - 1].LocalVersion, ShowUtil, ShowAnim, szCaption, szMsg);
          
          if not FKeepIntermediateFiles then
            DeleteFile(PChar(FFileList.Items[i - 1].TargetDir + UFName));
        end;
      end;
      if UpdateMethod = wuuPromptOnce then
        UpdateMethod := wuuSilent;
    end
    else
    begin
      if UpdateMethod = wuuPromptOnce then
        Break;
    end;
  end;
end;

function TWebUpdate.UpdateActions: Integer;
var
  s,p: string;
  IniFile: TMemIniFile;
  CustomVal: Boolean;
{$IFDEF DELPHI_UNICODE}
  sa: ansistring;
{$ENDIF}
begin
  if FCancelled then
    Result := WU_FAILED
  else
  begin
    Result := WU_SUCCESS;
    // post update info
    if FPostUpdateInfo.Enabled then
    begin
      CustomVal := true;

      if Assigned(FBeforePost) then
        FBeforePost(Self, CustomVal);

      if CustomVal then
      begin
        DoPostUpdateInfo;
        CustomVal := True;
        if Assigned(FProcessPostResult) then
          FProcessPostResult(Self,CustomVal);
        if not CustomVal then
        begin
          Result := WU_FAILED;
          Exit;
        end;
      end;
    end;

    // save new updated date
    if FSaveUpdate then
      UpdateDatePut(NewVersionDate);

    IniFile := TMemIniFile.Create(ControlFilename);

    // save new updated URL
    s := IniFile.ReadString('action','updateURL','');
    if s <> '' then
    begin
      DoStatus(941,'',WebUpdateInformation,0);
      URLPut(s);
    end;

    // extract the patcher utility
    if Assigned(FUtility) and FApplyPatch then
      ExtractUtility;

    s := ExpandPath(IniFile.ReadString('action','runafter',''));
    p := IniFile.ReadString('action','runafterparam','');

    DoSetAppParamAfter(p);

    if s <> '' then
    begin
      {$IFDEF DELPHI_UNICODE}
      sa := ansistring(s);
      if p <> '' then
        sa := sa + ' ' + ansistring(p);

      if IsVista then
        ShellExecute(0,'open',PChar(s), PChar(p), nil, SW_NORMAL)
      else
        Winexec(PAnsiChar(sa),sw_normal);
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      if IsVista then
        ShellExecute(0,'open',PChar(s), PChar(p), nil, SW_NORMAL)
      else
      begin
        if p <> '' then
          s := s + ' ' + p;
        Winexec(PChar(s),sw_normal);
      end;
      {$ENDIF}
    end;

    IniFile.Free;

    DoStatus(948,'',WebUpdateInformation,0);
  end;  
end;

function TWebUpdate.NewVersionAvailable(InitPath: boolean = false): Boolean;
var
  sl: TStringList;
  Res: Integer;
  vc: Integer;
begin
  if InitPath then
  begin
    SetCurrentDir(ExtractFilePath(Application.EXEName));
  end;

  FCancelled := False;

  Result := false;
  if StartConnection = WU_SUCCESS then
  begin
    // handle connect different for FTP & HTTP
    if FUpdateType = ftpUpdate then
    begin
      if FTPConnect = WU_FAILED then
        Error;
    end;

    if (FHintConnect <> nil) or (FUpdateType = fileUpdate) then
    begin
      if GetControlFile = WU_SUCCESS then
      begin
        vc := DoVersionCheck;
        Result := vc <> WU_NONEWVERSION;

        if vc = WU_UNCONDITIONALNEWVERSION then
        begin
          if (GetFileDetails > 0) then
            ProcessFileDetails;
          Result := (FFileList.Count > 0);
        end;

        if Result and Assigned(FOnDownloadedWhatsNew) then
        begin
          sl := GetWhatsNew;
          if Assigned(sl) then
          begin
            Res := mrOK;
            FOnDownloadedWhatsNew(Self,sl, Res);
            sl.Free;
          end;
        end;
      end;
    end;
    StopConnection;
  end;
end;

function TWebUpdate.NewWhatsnew(ShowDialog: boolean = false): TStringlist;
var
  sl: TStringList;
  ms: TMemoryStream;
  Res: boolean;
  resi: integer;
  RTF: boolean;
begin
  Result := nil;

  if StartConnection = WU_SUCCESS then
  begin
    // handle connect different for FTP & HTTP
    if FUpdateType = ftpUpdate then
    begin
      if FTPConnect = WU_FAILED then
        Error;
    end;

    if (FHintConnect <> nil) or (FUpdateType = fileUpdate) then
    begin
      if GetControlFile = WU_SUCCESS then
      begin
        Res := DoVersionCheck <> WU_NONEWVERSION;
        if Res then
        begin
          ms := GetWhatsNewStream;

          if Assigned(ms) and (ms.Size > 0) then
          begin
            RTF := IsRTF(ms);

            if not RTF then
            begin
              sl := TStringList.Create;
              sl.LoadFromStream(ms);

              if Assigned(FOnDownloadedWhatsNew) and not ShowDialog then
                FOnDownloadedWhatsNew(Self, sl, resi);

              ms.Clear;
              sl.SaveToStream(ms);
              ms.Position := 0;
              Result := sl;
            end;

            if ShowDialog then
              WhatsNewDialog(ms);

            ms.Free;
          end;
        end;
      end;
    end;
    StopConnection;
  end;
end;


procedure TWebUpdate.DoSuccess;
begin
  if Assigned(OnSuccess) then
    OnSuccess(Self);
end;

procedure TWebUpdate.DoUpdate;
begin
  DoUpdate(True);
end;


procedure TWebUpdate.DoUpdate(InitPath: boolean);
var
  ms: TMemoryStream;
  res: Integer;
  Doit : Boolean; 

begin
  FCancelled := False;

  if InitPath then
  begin
    SetCurrentDir(ExtractFilePath(Application.EXEName));
  end;

  if FThreaded then
    DoStatus(968,'Threaded update',0,0)
  else
    DoStatus(968,'Non threaded update',0,0);

  DoStatus(966,GetOSVersion,0,0);
  DoStatus(967,GetIEVersion,0,0);
  DoStatus(969,GetIDE,0,0);
  DoStatus(970,GetInstalledIDEs,0,0);

  FAppCompsIncluded := False;
  FFTPDirSet := False;
  FControlFileName := '';

  DoStatus(963,Version,0,0);

  if StartConnection = WU_SUCCESS then
  begin
    // handle connect different for FTP & HTTP
    if FUpdateType = ftpUpdate then
    begin
      if FTPConnect = WU_FAILED then
        Error;
    end;

    if (FHintConnect <> nil) or (FUpdateType = fileUpdate) then
    begin
      if GetControlFile = WU_SUCCESS then
      begin
        DoIt := FForceUpdate; // CLM
        if not DoIT then
          DoIT := (DoVersionCheck <> WU_NONEWVERSION);

        if DoIt then
        begin
          if HandleActions = WU_SUCCESS then
          begin
            res := WU_SUCCESS;

            ms := GetWhatsNewStream;
            if Assigned(ms) then
            begin
              res := WhatsNewDialog(ms);
              ms.Free;
            end;

            ms := GetEULAStream;
            if Assigned(ms) then
            begin
              if (res = WU_SUCCESS) then
                res := EULADialog(ms);
              ms.Free;
            end;

            if (res = WU_SUCCESS) then
            begin
              if GetFileDetails > 0 then
              begin
                res := ProcessFileDetails;
                if res = WU_SUCCESS then
                begin
                  if FShowDownloadProgress then
                    FProgressForm := TProgressForm.CreateNew(Application);
                  try
                    res := GetFileUpdates;
                  finally
                    if FShowDownloadProgress then
                    begin
                      if Assigned(FProgressForm) then
                      begin
                        FProgressForm.Hide;
                        FreeAndNil(FProgressForm);
                      end;
                    end;
                  end;

                  UpdateActions;
                end;

                StopConnection;

                if (res = WU_SUCCESS) and not FCancelled then
                  DoSuccess;

                if AppNeedsRestart and not FThreaded then
                  DoRestart;
                Exit;
              end
              else
                FAppClose := False;
            end
            else
              FAppClose := False;
          end;
        end
        else
          FAppClose := False;
      end;
    end;
    StopConnection;
  end;

end;

procedure TWebUpdate.DoRestart;
var
  proch: dword;
  allow: Boolean;
  StartupInfo: TStartupInfo;
  //ProcessInfo: TProcessInformation;
  //StartErrorCode: dword;
  //TmpBuf:array[0..1024] of char;
  Cancelled: Boolean;
  curdir, dolog,curapp,tmpdir: string;
  commandline: string;
  inifile: TMemIniFile;
begin
  Cancelled := False;

  DoStatus(959,GetCurrentDir,0,0);

  HandleCancel(Cancelled);

  If Cancelled then Exit;

  Allow := True;
  if Assigned(FAppRestart) then
    FAppRestart(Self,Allow)
  else
   if not FSilentRestart then
     Allow := MessageDlg(LoadStr(911),mtConfirmation,[mbYes,mbNo],0) = mrYes;

  if Allow then
  begin
    ExtractUpdateResource;

    inifile := TMemIniFile.Create(AddBackSlash(UserDocDir) + 'WUPDATE.INI');
    inifile.WriteString('CONFIG','LOGFILENAME',_logfilename);
    inifile.Free;

    proch := GetCurrentProcessID;

    FillChar(StartupInfo, Sizeof(StartupInfo), #0);
    StartupInfo.cb := Sizeof(StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOWNORMAL;

    curdir := GetCurrentDir;

    if Logging then
      dolog := ' L '
    else
      doLog := ' X ';

    // no full path is specified for the app
    if pos(':',FAppname) = 0 then
      curapp := AddBackSlash(curdir) + FAppname
    else
      curapp := FAppName;

    if pos(' ',curapp) > 0 then
      curapp := '"' + curapp + '"';

    if not FAutoRestart then
      curapp := 'null';

    if (pos(' ',FAppParam) > 0) then
      FAppParam := '"'+ FAppParam + '"';

    curapp := curapp + ' ' + FAppParam;  

    if UseWinTempDir then
      tmpdir := WinTempDir
    else
      tmpdir := TempDirectory;

    //CommandLine := wintempdir + 'wusetup.exe ' + IntToStr(proch) + doLog + curapp + ' ' + tmpdir + ' ' + fappcomps;
    CommandLine := IntToStr(proch) + doLog + curapp + ' "' + tmpdir + '" ' + fappcomps;

    DoStatus(958,CommandLine,0,0);

    (*
    if not CreateProcess(nil, PChar(CommandLine),
      nil,
      nil,
      true,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, { creation flags }
      nil,                    { pointer to new environment block }
      nil,                    { pointer to current directory name, PChar}
      StartupInfo,            { pointer to STARTUPINFO }
      ProcessInfo) then
    begin
      StartErrorCode := GetLastError;
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,
        nil,
        StartErrorCode,
        0,
        TmpBuf,
        SizeOf(TmpBuf),     
        nil);

      DoStatus(950,strpas(TmpBuf),WebUpdateSpawnFail,StartErrorCode);
    end;
    *)
    {$IFDEF USEUAC}
    ShellExecute(0,'open',pchar(wintempdir + 'wusetup.exe'),pchar(commandline),nil,SW_SHOW);
    {$ENDIF}

    {$IFNDEF USEUAC}
    ShellExecute(0,'open',pchar(wintempdir + 'wurepl.exe'),pchar(commandline),nil,SW_SHOW);
    {$ENDIF}


    if Assigned(FAppDoClose) then
      FAppDoClose(self) else Application.Terminate;
  end;
end;

function TWebUpdate.URLGet: string;
var
  RegIniFile: TRegIniFile;
begin
  Result := '';
  if (FLastURLEntry.Key <> '') and
     (FLastURLEntry.Section <> '') and
     (FLastURLEntry.Save) then
  begin
    RegIniFile := TRegIniFile.Create(''{FLastURLEntry.key});
    if FLastURLEntry.RegRoot = lurLOCALMACHINE then
      RegIniFile.RootKey := HKEY_LOCAL_MACHINE;
    RegIniFile.OpenKey(FLastURLEntry.key, True {CanCreate});
    Result := RegIniFile.ReadString(FLastURLEntry.Section,'LastURL','');
    RegIniFile.Free;
  end;
end;

procedure TWebUpdate.URLPut(url: string);
var
  RegIniFile: TRegIniFile;
begin
  if (FLastURLEntry.Key <> '') and
    (FLastURLEntry.Section <> '') and
    (FLastURLEntry.Save) then
  begin
    RegIniFile := TRegIniFile.Create(''{FLastURLEntry.key});
    if FLastURLEntry.RegRoot = lurLOCALMACHINE then
      RegIniFile.RootKey := HKEY_LOCAL_MACHINE;
    RegIniFile.OpenKey(FLastURLEntry.key, True {CanCreate});      
    RegIniFile.WriteString(fLastURLEntry.Section,'LastURL',url);
    RegIniFile.Free;
  end;
end;

function TWebUpdate.UpdateDateGet: TDateTime;
var
  RegIniFile: TRegIniFile;
  DateStr, TimeStr,sdf: string;
begin
  Result := EncodeDate(1980,1,1);

  if (FLastURLEntry.Key <> '') and
     (FLastURLEntry.Section <> '') then
  begin
    RegIniFile := TRegIniFile.Create('');
    if FLastURLEntry.RegRoot = lurLOCALMACHINE then
      RegIniFile.RootKey := HKEY_LOCAL_MACHINE;

    sdf := ShortDateFormat;

    {$IFDEF DELPHIXE_LVL}
    FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
    {$ENDIF}
    {$IFNDEF DELPHIXE_LVL}
    ShortDateFormat := 'dd/mm/yyyy';
    {$ENDIF}

    RegIniFile.OpenKey(FLastURLEntry.key, True {CanCreate});
    DateStr := RegIniFile.ReadString(FLastURLEntry.Section,'LastDate','');
    TimeStr := RegIniFile.ReadString(FLastURLEntry.Section,'LastTime','');

    try
      if DateStr <> '' then
        Result := StrToDate(DateStr);

      if TimeStr <> '' then
        Result := Result + StrToTime(TimeStr);
    except
    end;

    {$IFDEF DELPHIXE_LVL}
    FormatSettings.ShortDateFormat := sdf;
    {$ENDIF}
    {$IFNDEF DELPHIXE_LVL}
    ShortDateFormat := sdf;
    {$ENDIF}

    FSaveUpdate := True;
    RegIniFile.Free;
  end
  else
  begin
    if FLocalFileDateCheck <> '' then
      if FileExists(FLocalFileDateCheck) then
      {$IFDEF DELPHI2006_LVL}
        FileAge(FLocalFileDateCheck, Result);
      {$ELSE}
        Result := SysUtils.FileDateToDateTime(SysUtils.FileAge(FLocalFileDateCheck));
      {$ENDIF}
  end;
end;

procedure TWebUpdate.UpdateDatePut(dt: TDateTime);
var
  RegIniFile:TRegIniFile;
  sdf: string;
begin
  if (FLastURLEntry.Key<>'') and
     (FLastURLEntry.Section<>'') and
     (FLastURLEntry.Save) then
  begin
    RegIniFile := TRegIniFile.Create('');
    if FLastURLEntry.RegRoot = lurLOCALMACHINE then
      RegIniFile.RootKey := HKEY_LOCAL_MACHINE;

    sdf := ShortDateFormat;

    {$IFDEF DELPHIXE_LVL}
    FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
    {$ENDIF}
    {$IFNDEF DELPHIXE_LVL}
    ShortDateFormat := 'dd/mm/yyyy';
    {$ENDIF}

    RegIniFile.OpenKey(FLastURLEntry.key, True {CanCreate});
    RegIniFile.WriteString(FLastURLEntry.Section,'LastDate',DateToStr(dt));

    if Frac(dt) <> 0 then
      RegIniFile.WriteString(FLastURLEntry.Section,'LastTime',TimeToStr(dt));

    {$IFDEF DELPHIXE_LVL}
    FormatSettings.ShortDateFormat := sdf;
    {$ENDIF}
    {$IFNDEF DELPHIXE_LVL}
    ShortDateFormat := sdf;
    {$ENDIF}

    RegIniFile.Free;
  end;
end;

function TWebUpdate.Connected:Boolean;
var
  dwFlags:dword;
  dwReserved:cardinal;
begin
  dwReserved:=0;

  if (Pos('127.0.0.1', URL) > 0) then
    Result := true
  else
    Result := DynaLink_InternetGetConnectedState(@dwFlags,dwReserved);
end;

function TWebUpdate.ConnectionType:Integer;
var
  dwFlags: dword;
  dwReserved: cardinal;
begin
  dwReserved := 0;
  if DynaLink_InternetGetConnectedState(@dwFlags,dwReserved) then
    Result := dwFlags
  else
    Result := -1;
end;

procedure TWebUpdate.ExtractUpdateResource;
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: dword;
  ptr: pointer;
  binfile: file of byte;
  srcname, dstname: string;
begin
  reshandle := FindResource(hinstance,'UPD',PChar(RT_RCDATA));
  hglobal := LoadResource(hinstance,reshandle);
  Ressize := SizeOfResource(hinstance,reshandle);
  ptr := LockResource(hglobal);

  srcname := wintempdir + 'wusetup.ex_';
  AssignFile(binfile,srcname);
  Rewrite(binfile);
  Blockwrite(binfile,ptr^,ressize);
  Closefile(binfile);
  dstname := ExpandFile(ansistring(srcname));

  {$IFNDEF USEUAC}
  if FileExists(wintempdir + 'wurepl.exe') then
    SysUtils.DeleteFile(wintempdir + 'wurepl.exe');

  RenameFile(dstname, wintempdir + 'wurepl.exe');
  {$ENDIF}
end;

procedure TWebUpdate.ExtractUtility;
var
  reshandle: THandle;
  hglobal: THandle;
  ressize: DWord;
  ptr: pointer;
  binfile: file of byte;
  srcname: string;
begin
  reshandle := FindResource(hinstance,'UTIL',PChar(RT_RCDATA));
  hglobal := LoadResource(hinstance,reshandle);
  Ressize := SizeOfResource(hinstance,reshandle);
  ptr := LockResource(hglobal);

  if UseWinTempDir then
    SrcName := wintempdir + 'patcher.exe'
  else
    SrcName := GetCurrentDir + '\patcher.exe';
    
  AssignFile(binfile,SrcName);
  Rewrite(binfile);
  Blockwrite(binfile,ptr^,ressize);
  Closefile(binfile);
end;


procedure TWebUpdate.CustomProcess(fn: string);
begin
  if Assigned(FWebUpdateFileDownloaded) then
    FWebUpdateFileDownloaded(Self,fn);
end;

procedure TWebUpdate.ShowHTMLDialog(s:string);

begin
  if Owner is TWinControl then
    HTMLDialog((Owner as TWinControl).Handle,s)

  else
    HTMLDialog(0, s);
end;

procedure TWebUpdate.HangUp;
var
  dwReserved: DWord;
begin
  if Connected then
  begin
    dwReserved := 0;
    Dynalink_InternetAutoDialHangup(dwReserved);
  end;
end;

procedure TWebUpdate.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FUtility) then
  begin
    FUtility := nil;
  end;
end;

{ TFileList }

function TFileList.Add: TFileDescription;
begin
   Result := TFileDescription(inherited Add);
end;

function TFileList.CompletedSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to ActiveItem do
    Result := Result + Items[i - 1].FileSize;
end;

function TFileList.GetItem(Index: Integer): TFileDescription;
begin
  Result := TFileDescription(inherited Items[Index]);
end;

function TFileList.GetItemClass: TCollectionItemClass;
begin
  Result := TFileDescription;
end;

function TFileList.Insert(index: Integer): TFileDescription;
begin
  Result := TFileDescription(inherited Insert(Index));
end;

procedure TFileList.SetItem(Index: Integer; const Value: TFileDescription);
begin
  inherited Items[Index] := Value;
end;

function TFileList.TotalSize: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Count do
    Result := Result + Items[i - 1].FileSize;
end;


{ TProgressForm }

constructor TProgressForm.CreateNew(AOwner: TComponent; Dummy: integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FProgressBar := TProgressBar.Create(Self);
  FLabel := TLabel.Create(Self);
  Width := 300;
  Height := 110;
end;

procedure TProgressForm.CreateWnd;
begin
  inherited;
  FProgressBar.Parent := Self;
  FLabel.Parent := Self;
  FLabel.Top := 10;
  FLabel.Left := 10;
  FProgressBar.Top := 40;
  FProgressBar.Left := 10;
  FProgressBar.Width := 264;
  {$IFDEF DELPHIXE_LVL}
  FProgressBar.Anchors := [akRight, akLeft, akTop];
  {$ENDIF}
  Caption := LoadStr(978);
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  BorderIcons := [];
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}



end.
