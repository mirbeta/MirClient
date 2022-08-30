{*******************************************************************}
{ TWEBCOPY component                                                }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2000-2015                                          }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit WebCopy;

{$IFDEF WIN32}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.lib"'}
{$HPPEMIT ''}
{$ENDIF}
{$IFDEF WIN64}
{$HPPEMIT ''}
{$HPPEMIT '#pragma link "wininet.a"'}
{$HPPEMIT ''}
{$ENDIF}

{$R WEBCOPY.RES}
{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  WinInet, ComCtrls, StdCtrls, ShellApi, WcBase64, WcLogin;

const
  errFilesIdentical = 0;
  errCannotOpenSourceFile = 1;
  errSourceFileZeroLength = 2;
  errCannotCreateTargetFile = 3;
  errCopyReadFailure = 4;
  errCopyWriteFailure = 5;
  errCannotConnect = 6;
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.5.0.0  : keep FTP connection between multiple ftp uploads / downloads
  //          : added authenticated HTTP download support
  // 1.5.0.1  : improved handling for keeping FTP connections active
  // 1.5.0.2  : graceful degrade when server cannot return file timestamp
  // 1.5.0.3  : fixed issue with FtpFindFirstFile and closing handle
  // 1.5.0.4  : fixed issue with KeepConnect and ftp change dir
  // 1.5.0.5  : fixed OnFileDateCheck event declaration
  // 1.6.0.0  : New wildchard match multiple file FTP upload/download
  // 1.6.0.1  : Fixed issue with wildchard match multiple file FTP upload/download
  // 1.6.1.0  : New property ShowDialogOnTop added
  // 1.7.0.0  : New : VCL.NET support added
  // 1.7.5.0  : New : wpFtpDelete type added
  //          : Fixed : issue with getting filesize for some FTP server types
  // 1.7.6.0  : New : event OnBeforeDialogShow added

  // 2.0.0.0  : New : capability to create multilevel folders on FTP server added
  //          : New : OnMultiFTPFileDone event added
  //          : New : property Agent added
  //          : New : OnCopyOverWrite event triggered for FTP upload when file already exists
  //          : New : Added capability to create unique filenames during download when file exists, ie. filename(x).ext
  //          : New : instruction wpFTPList added to get list of files on FTP server

  // 2.0.0.1  : Improved : handling of very large files for FTP download
  // 2.0.0.2  : Fixed : issue with creating partially existing directory paths on FTP server
  // 2.0.0.3  : Fixed : issue with putting multiple files in the same folder via FTP upload
  // 2.0.0.4  : Fixed : issue with retrieving FTP current directory with Delphi 2009
  // 2.0.1.0  : New : properties DlgShowCancel, DlgShowCloseBtn added
  // 2.1.0.0  : New : ShowElapsedTime capability added
  // 2.1.0.1  : Fixed : issue with wpFTPList and refreshing folders
  // 2.2.0.0  : New : Marquee style progressbar for HTTP servers unable to return file size (from Delphi 2009)
  //          : New : DisplayURL property added in TWebCopyItem to show URL in dialog different from real URL
  //          : Improved : Extraction of server name from URL
  // 2.2.1.0  : Improved : added support to upload readonly files
  // 2.2.1.1  : Fixed : Issue with getting file stamp from FTP servers
  // 2.2.2.0  : New : Exposed as public property RateStr, TimeLeftStr, ElapsedTimeStr
  // 2.2.3.0  : Improved : Handles the file:// specifier for file copy
  // 2.2.3.1  : Fixed : Issue with trying to upload via FTP files that do not exist
  // 2.3.0.0  : New : wpFtpTest protocol added
  // 2.3.0.1  : Fixed : Issue when server could not return filesize
  // 2.3.0.2  : Improved : Handling of Cancel
  // 2.4.0.0  : New : TimeOut property added
  //          : Fixed : Possible issue when programmatically calling CancelCopy
  // 2.4.0.1  : Fixed : Issue with wpMultiFtp and CopyNewerOnly = true
  // 2.4.0.2  : Fixed : Issue with ShowOpenFile & Delphi 7 or older Delphi versions
  // 2.4.0.3  : Fixed : Issue with auto-close progress dialogs
  // 2.4.1.0  : New : Properties DlgTimeSec, DlgTimeMin added to localize time units
  // 2.4.1.1  : Fixed : Rare issue with FTP download when file is removed from server after download
  // 2.4.1.2  : Fixed : Issue with rare FTP caching
  // 2.4.1.3  : Fixed : Issue with closing dialog
  // 2.4.2.0  : New : FTPMode property added
  // 2.4.2.1  : Fixed : Issue with handling very large files with FTP upload/download
  // 2.4.3.0  : New : Property AutoURLDecode added for HTTP downloads using URL encoded filenames


type
  TWebCopy = class;

  TWebCopyAuthentication = (waNever, waAlways, waAuto);

  TWCopyThread = class(TThread)
  private
    webcopy: TWebCopy;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebCopy: TWebCopy);
  end;

  TWebCopyURLNotFound = procedure(Sender:TObject;url:string) of object;
  TWebCopyError = procedure(Sender:TObject;ErrorCode:longint) of object;
  TWebCopyErrorInfo = procedure(Sender:TObject;ErrorCode:longint;ErrorInfo:string) of object;
  TWebCopyThreadDone = procedure(Sender:TObject) of object;
  TWebCopyCancel = procedure(Sender:TObject) of object;
  TWebCopyFileDone = procedure(Sender:TObject;idx:integer) of object;
  TWebCopyMultiFTPFileDone = procedure(Sender: TObject; Filename: string) of object;
  TWebCopyFileStart = procedure(Sender:TObject;idx:integer) of object;
  TWebRemoveFileDone = procedure(Sender:TObject;idx:integer) of object;
  TWebRemoveFileFailed = procedure(Sender:TObject;idx:integer) of object;
  TWebCopyFileDateCheck = procedure(Sender:TObject;idx: Integer;newdate: TDateTime;var allow:boolean) of object;
  TWebCopyConnectError = procedure(Sender: TObject) of object;
  TWebCopyProtocol = (wpHttp,wpFtp,wpFile,wpFtpUpload,wpHttpUpload,wpMultiFtp, wpMultiFtpUpload, wpFtpDelete, wpFtpList, wpFtpTest);
  TWebCopyHTTPCommand = (hcPost,hcPut);
  TWebCopyOverwrite = procedure(Sender:TObject;tgtfile:string;var allow:boolean) of object;
  TWebCopyProgress = procedure(Sender:TObject;fileidx:integer;size,totsize:DWORD) of object;
  TWebCopyNoNewFile = procedure(Sender:TObject;tgtfile:string;curdate,newdate:TDateTime) of object;
  TWebCopyBeforeDialogShow = procedure(Sender: TObject; ADialog: TForm) of object;


  TFileDetailItem = class(TCollectionItem)
  private
    FFileAttributes: DWORD;
    FFilename: string;
    FFileSizeLow: DWORD;
    FFileSizeHigh: DWORD;
    FFileDate: TFileTime;
  public
    property FileAttributes: DWORD read FFileAttributes write FFileAttributes;
    property Filename: string read FFilename write FFilename;
    property FileSizeLow: DWORD read FFileSizeLow write FFileSizeLow;
    property FileSizeHigh: DWORD read FFileSizeHigh write FFileSizeHigh;
    property FileDate: TFileTime read FFileDate write FFileDate;
  end;

  TFileDetailItems = class(TCollection)
  private
    function GetItem(Index: Integer): TFileDetailItem;
    procedure SetItem(Index: Integer; const Value: TFileDetailItem);
  published
  public
    constructor Create;
    function Add: TFileDetailItem;
    function Insert(Index: Integer): TFileDetailItem;
    property Items[Index: Integer]: TFileDetailItem read GetItem write SetItem; default;
  end;

  TFTPMode = (fmBINARY, fmASCII);

  TWebCopyItem = class(TCollectionItem)
  private
    FURL: string;
    FTargetDir: string;
    FTargetFilename: string;
    FProtocol: TWebCopyProtocol;
    FHTTPCommand: TWebCopyHTTPCommand;
    FFTPHost: string;
    FFTPUserID: string;
    FFTPPassword: string;
    FFTPPort: Integer;
    FActive: Boolean;
    FSuccess: Boolean;
    FCopyNewerOnly: Boolean;
    FFileDate: TDateTime;
    FNoNewVersion: Boolean;
    FNewFileDate: TDateTime;
    FAuthenticate: TWebCopyAuthentication;
    FHTTPUserID: string;
    FDisplayURL: string;
    FHTTPPassword: string;
    FFileSize: Cardinal;
    FCancelled: boolean;
    FFileList: TStringList;
    FFileDetailList: TFileDetailItems;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Success: Boolean read FSuccess;
    property Cancelled: Boolean read FCancelled;
    property NoNewVersion: Boolean read FNoNewVersion;
    property NewFileDate: TDateTime read FNewFileDate write FNewFileDate;
    property FileList: TStringList read FFileList;
    property FileSize: Cardinal read FFileSize write FFileSize;
    property FileDetails: TFileDetailItems read FFileDetailList;
  published
    property Active: Boolean read FActive write FActive default True;
    property Authenticate: TWebCopyAuthentication read FAuthenticate write FAuthenticate default waNever;
    property CopyNewerOnly: Boolean read FCopyNewerOnly write FCopyNewerOnly default False;
    property DisplayURL: string read FDisplayURL write FDisplayURL;
    property FileDate: TDateTime read FFileDate write FFileDate;
    property FTPHost: string read FFTPHost write FFTPHost;
    property FTPUserID: string read FFTPUserID write FFTPUserID;
    property FTPPassword: string read FFTPPassword write FFTPPassword;
    property FTPPort: Integer read FFTPPort write FFTPPort;
    property HTTPCommand: TWebCopyHTTPCommand read FHTTPCommand write FHTTPCommand;
    property Protocol: TWebCopyProtocol read FProtocol write FProtocol;
    property TargetDir: string read FTargetDir write FTargetDir;
    property TargetFilename: string read FTargetFilename write FTargetFilename;
    property URL: string read FURL write FURL;
    property HTTPUserID: string read FHTTPUserID write FHTTPUserID;
    property HTTPPassword: string read FHTTPPassword write FHTTPPassword;
  end;

  TWebCopyItems = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TWebCopyItem;
    procedure SetItem(Index: Integer; Value: TWebCopyItem);
    function GetActiveItems: integer;
    function GetSuccessCount: Integer;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TComponent);
    function Add: TWebCopyItem;
    function Insert(Index: Integer): TWebCopyItem;
    property Items[Index: Integer]: TWebCopyItem read GetItem write SetItem; default;
    property ActiveItems: Integer read GetActiveItems;
    property SuccessCount: Integer read GetSuccessCount;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TWebCopy = class(TComponent)
  private
    { Private declarations }
    FAlwaysOnTop: Boolean;
    FFileNum: Integer;
    FCancelled: Boolean;
    FForm: TForm;
    FFormClosed: Boolean;
    FProgress: TProgressBar;
    FFileLbl: TLabel;
    FSizeLbl: TLabel;
    FRateLbl: TLabel;
    FTimeLbl: TLabel;
    FElapsedTimeLbl: TLabel;
    FCancelBtn: TButton;
    FBtnShowFile,FBtnShowFolder: TButton;
    FAnim:TAnimate;
    FLastFile: string;
    FLastDir: string;
    FDlgCaption:string;
    FDlgUpload: string;
    FFileLabel: string;
    FProgressLabel: string;
    FRateLabel: string;
    FTimeLabel: string;
    FElapsedTimeLabel: string;
    FProxy: string;
    FProxyUserID: string;
    FProxyPassword: string;
    FTimeOut: integer;
    FItems: TWebCopyItems;
    FHinternet: hinternet;
    FOnError: TWebCopyError;
    FOnErrorInfo: TWebCopyErrorInfo;
    FOnCopyDone: TWebCopyThreadDone;
    FOnFileDone: TWebCopyFileDone;
    FOnFileStart: TWebCopyFileStart;
    FOnConnectError: TWebCopyConnectError;
    FOnURLNotFound: TWebCopyURLNotFound;
    FOnCopyCancel: TWebCopyCancel;
    FOnCopyOverwrite: TWebCopyOverwrite;
    FOnCopyProgress: TWebCopyProgress;
    FOnMultiFTPFileDone: TWebCopyMultiFTPFileDone;
    FOnBeforeDialogShow: TWebCopyBeforeDialogShow;
    FShowDialog: Boolean;
    FFileOfLabel: string;
    FFromServerLabel: string;
    FToServerLabel: string;
    FShowOpenFolder: Boolean;
    FShowOpenFile: Boolean;
    FFTPPassive: Boolean;
    FFTPMode: TFTPMode;
    FShowTime: Boolean;
    FShowElapsedTime: Boolean;
    FShowCompletion: Boolean;
    FShowServer: Boolean;
    FDlgCompleted: string;
    FDlgDwnload: string;
    FDlgCopying: string;
    FCopyCompleted: Boolean;
    FOnNoNewFile: TWebCopyNoNewFile;
    FDlgCancel: string;
    FDlgClose: string;
    FDlgOpenFolder: string;
    FDlgOpenFile: string;
    FDlgShowCancel: boolean;
    FDlgShowCloseBtn: boolean;
    FDlgTimeSec: string;
    FDlgTimeMin: string;
    FCancelIsClose: boolean;
    FKeepAlive: Boolean;
    FOnFileDateCheck: TWebCopyFileDateCheck;
    FShowFileName: Boolean;
    FFTPconnect: hinternet;
    FPrevConnect: string;
    FPrevTarget: string;
    FAgent: string;
    FRateStr: string;
    FTimeLeftStr: string;
    FElapsedTimeStr: string;
    FLogFileName: string;
    FCreateUniqueFilenames: Boolean;
    FAutoURLDecode: Boolean;
    FShowDialogOnTop: Boolean;
    FOnRemovedFile: TWebRemoveFileDone;
    FOnRemovedFileFailed: TWebRemoveFileFailed;
    function HttpGetFile(idx: Integer;url,displayurl,tgtdir,tgtfn:string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean;Auth: TWebCopyAuthentication; UserId, Password: string): Boolean;
    function HttpPutFile(url,tgtdir,tgtfn:string; HttpCommand: TWebCopyHTTPCommand): Boolean;
    function FileGetFile(idx: Integer;FUserid, FPassword, FHost: string; FPort: integer; url, tgtdir,tgtfn: string;
      UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean): Boolean;
    function FtpGetFile(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
      URL, TgtDir,TgtFn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean; KeepConnect, ShowFileInfo: Boolean): Boolean;
    function MultiFtpGetList(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
      URL, TgtDir,TgtFn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean; KeepConnect: Boolean): Boolean;
    function MultiFtpGetFile(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
      URL, TgtDir,TgtFn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean; KeepConnect: Boolean): Boolean;
    function MultiFtpPutFile(idx: Integer;fuserid,fpassword,FHost:string;FPort:Integer;
      URL, TgtDir,TgtFn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean; KeepConnect: Boolean): Boolean;
    function FtpPutFile(idx: Integer;fuserid,fpassword,FHost:string;FPort:Integer;
      URL, TgtDir,TgtFn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean; KeepConnect: Boolean): Boolean;
    function FtpRemoveFile(idx: Integer; FUserid, FPassword, FHost: string;
      FPort: Integer; URL, TgtDir, TgtFn: string; KeepConnect: Boolean): Boolean;
    function FtpTest(idx: Integer; FUserid, FPassword, FHost: string; FPort: integer): boolean;
    function MakeProxyUrl(url:string): string;
    function ExtractServer(url:string): string;
    function RemoveServer(url:string): string;
    procedure CreateForm;
    procedure ThreadDone(Sender: TObject);
    function NumInetItems: Integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure CancelClick(Sender:TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OpenFile(Sender:TObject);
    procedure OpenFolder(Sender:TObject);
    procedure Log(s: string);
  public
    { Public declarations }
    procedure Execute;
    procedure ThreadExecute;
    procedure ThreadExecAndWait;
    procedure DoCopy;
    procedure CancelCopy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ProgressForm: TForm read FForm;
    property ProgressBar: TProgressBar read FProgress;
    property ProgressFileLabel: TLabel read FFileLbl;
    property ProgressSizeLabel: TLabel read FSizeLbl;
    property ProgressRateLabel: TLabel read FRateLbl;
    property ElapsedTimeStr: string read FElapsedTimeStr;
    property TimeLeftStr: string read FTimeLeftStr;
    property RateStr: string read FRateStr;
    property FormClosed: Boolean read FFormClosed write FFormClosed;
    property LogfileName: string read FLogFileName write FLogFileName;
  published
    { Published declarations }
    property Agent: string read FAgent write FAgent;
    property AlwaysOnTop: Boolean read FAlwaysOnTop write FAlwaysOnTop default False;
    property AutoURLDecode: Boolean read FAutoURLDecode write FAutoURLDecode default False;
    property CreateUniqueFilenames: boolean read FCreateUniqueFilenames write FCreateUniqueFilenames default False;
    property DlgCancel: string read FDlgCancel write FDlgCancel;
    property DlgCaption: string read FDlgCaption write FDlgCaption;
    property DlgCompleted: string read FDlgCompleted write FDlgCompleted;
    property DlgClose: string read FDlgClose write FDlgClose;
    property DlgFileLabel: string read FFileLabel write FFileLabel;
    property DlgFileOfLabel: string read FFileOfLabel write FFileOfLabel;
    property DlgFromServerLabel: string read FFromServerLabel write FFromServerLabel;
    property DlgToServerLabel: string read FToServerLabel write FToServerLabel;
    property DlgProgressLabel: string read FProgressLabel write FProgressLabel;
    property DlgRateLabel: string read FRateLabel write FRateLabel;
    property DlgTimeLabel: string read FTimeLabel write FTimeLabel;
    property DlgElapsedTimeLabel: string read FElapsedTimeLabel write FElapsedTimeLabel;
    property DlgCopying: string read FDlgCopying write FDlgCopying;
    property DlgDwnload: string read FDlgDwnload write FDlgDwnload;
    property DlgUpload: string read FDlgUpload write FDlgUpload;
    property DlgOpenFile: string read FDlgOpenFile write FDlgOpenFile;
    property DlgOpenFolder: string read FDlgOpenFolder write FDlgOpenFolder;
    property DlgTimeSec: string read FDlgTimeSec write FDlgTimeSec;
    property DlgTimeMin: string read FDlgTimeMin write FDlgTimeMin;
    property FTPPassive: Boolean read FFTPPassive write FFTPPassive;
    property FTPMode: TFTPMode read FFTPMode write FFTPMode default fmBINARY;
    property HTTPKeepAliveAuthentication: Boolean read FKeepAlive write FKeepAlive default False;
    property Items: TWebCopyItems read FItems write FItems;
    property Proxy: string read FProxy write FProxy;
    property ProxyUserID: string read FProxyUserID write FProxyUserID;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
    property DlgShowCancel: boolean read FDlgShowCancel write FDlgShowCancel default True;
    property DlgShowCloseBtn: boolean read FDlgShowCloseBtn write FDlgShowCloseBtn default False;
    property ShowDialog: Boolean read FShowDialog write FShowDialog default True;
    property ShowDialogOnTop: Boolean read FShowDialogOnTop write FShowDialogOnTop default True;
    property ShowOpenFile: Boolean read FShowOpenFile write FShowOpenFile default False;
    property ShowOpenFolder: Boolean read FShowOpenFolder write FShowOpenFolder default False;
    property ShowCompletion: Boolean read FShowCompletion write FShowCompletion default False;
    property ShowElapsedTime: Boolean read FShowElapsedTime write FShowElapsedTime default False;
    property ShowFileName: Boolean read FShowFileName write FShowFileName default True;
    property ShowServer: Boolean read FShowServer write FShowServer default True;
    property ShowTime: Boolean read FShowTime write FShowTime default False;
    property Timeout: integer read FTimeout write FTimeout;
    property OnBeforeDialogShow: TWebCopyBeforeDialogShow read FOnBeforeDialogShow write FOnBeforeDialogShow;
    property OnConnectError: TWebCopyConnectError read FOnConnectError write FOnConnectError;
    property OnCopyCancel: TWebCopyCancel read FOnCopyCancel write FOnCopyCancel;
    property OnCopyDone: TWebCopyThreadDone read FOnCopyDone write FOnCopyDone;
    property OnCopyOverWrite: TWebCopyOverwrite read FOnCopyOverwrite write FOnCopyOverwrite;
    property OnCopyProgress: TWebCopyProgress read FOnCopyProgress write FOnCopyProgress;
    property OnError: TWebCopyError read FOnError write FOnError;
    property OnErrorInfo: TWebCopyErrorInfo read FOnErrorInfo write FOnErrorInfo;
    property OnFileDone: TWebCopyFileDone read FOnFileDone write FOnFileDone;
    property OnFileStart: TWebCopyFileStart read FOnFileStart write FOnFileStart;
    property OnFileDateCheck: TWebCopyFileDateCheck read FOnFileDateCheck write FOnFileDateCheck;
    property OnMultiFTPFileDone: TWebCopyMultiFTPFileDone read FOnMultiFTPFileDone write FOnMultiFTPFileDone;
    property OnURLNotFound: TWebCopyURLNotFound read FOnURLNotFound write FOnURLNotFound;
    property OnNoNewFileFound: TWebCopyNoNewFile read FOnNoNewFile write FOnNoNewFile;
    property OnRemovedFile : TWebRemoveFileDone read FOnRemovedFile write FOnRemovedFile;
    property OnRemovedFileFailed : TWebRemoveFileFailed read FOnRemovedFileFailed write FOnRemovedFileFailed;
    property Version: string read GetVersion write SetVersion;
  end;

function FileSizeFmt(Size: int64): string;
function FileSizeFmtSpeed(Size: int64):string;
function TimeFmt(ticks: DWORD; cursize,totsize: int64; TS,TM: string): string;

implementation

uses
  ActiveX, ShlObj;

const
  READBUFFERSIZE = 4096;

function URLDecode(const Url: string): string;
const
  HexDigit = '0123456789ABCDEF';
var
  i: Integer;
  res: string;
  ch1,ch2: integer;
begin
  res := '';

  i := 1;

  while i <= Length(Url) do
  begin
    if Url[i] = '%' then
    begin
      if i + 2 <= Length(Url) then
      begin
        ch1 := Pos(Url[i + 1], HexDigit);
        ch2 := Pos(Url[i + 2], HexDigit);

        if (ch1 > 0) and (ch2 > 0)  then
        begin
          dec(ch1);
          dec(ch2);
          ch1 := (ch1 shl 4) + ch2;
          res := res + Chr(ch1);
        end;
        inc(i,2);
      end;

    end
    else
      res := res + Url[i];

    inc(i);
  end;

  Result := res;
end;


function GetFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
  h: int64;
begin
  Result := -1;

  if not GetFileAttributesEx(PChar(aFileName), GetFileExInfoStandard, @info) then
    Exit;

  h := $80000000;
  h := h * 2;
  Result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh * h);
end;


{ TWebCopy helper functions }

function ForwardSlashes(s:string): string;
var
  i: integer;

begin
  for i := 1 to length(s) do
    if s[i] = '\' then s[i] := '/';
  Result := s;
end;


procedure FileTimeToDateTime(ft: _FileTime; var FTime: TDateTime);
var
  st: _SYSTEMTIME;
begin
  FileTimeToSystemTime(ft,st);

  FTime := EncodeDate(st.wYear,st.wMonth,st.wDay) +
    EncodeTime(st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
end;

procedure SystemTimeToDateTime(st: _SystemTime; var FTime: TDateTime);
begin
  if (st.wYear = 0) and (st.wMonth = 0) then
    FTime := Now
  else
    FTime := EncodeDate(st.wYear,st.wMonth,st.wDay) +
      EncodeTime(st.wHour,st.wMinute,st.wSecond,st.wMilliseconds);
end;


function URLtoFile(url:string): string;
begin
  while Pos('/',url) > 0 do
    Delete(url,1,Pos('/',url));
  while Pos('\',url) > 0 do
   Delete(url,1,pos('\',url));
  Result := url;
end;

function FileSizeFmt(Size: int64): string;
begin
  if size < 1000 then
    Result := IntToStr(Size) + ' bytes';
  if (Size >= 1000) and (Size < 1000000) then
    Result := Format('%.2f',[Size/1000])+' Kb';
  if Size >= 1000000 then
    Result := Format('%.2f',[Size/1000000])+' Mb';
end;

function FileSizeFmtSpeed(Size: int64): string;
begin
  if Size < 1000 then
    Result := IntToStr(Size)+' bytes/sec';
  if (Size >= 1000) and (Size < 1000000) then
    Result := Format('%.2f',[Size/1000])+' Kb/sec';
  if Size >= 1000000 then
    Result := Format('%.2f',[Size/1000000])+' Mb/sec';
end;

function TimeFmt(ticks: DWord; cursize,totsize: int64; TS,TM: string): string;
var
  secs,mins,hours: Integer;
  ratio: double;

begin
  ratio := ticks;

  if (cursize > 0) and (totsize > 0) and (totsize > cursize) then
  begin
    ratio := ratio * ((totsize - cursize) / cursize);
  end;

  ticks := Round(ratio);

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar(inttostr(ticks)+' : '+inttostr(cursize)+' : '+inttostr(totsize)));
  {$ENDIF}

  secs := round(ticks / 1000);
  mins := round(secs / 60);
  hours := round(mins / 60);

  if (mins = 0) and (hours = 0) then
  begin
    Result := IntToStr(secs)+ ' ' + TS +'.';
  end
  else
  begin
    if hours > 0 then
    begin
      Result := IntToStr(mins div 60) + 'h ' + IntToStr(mins mod 60);
    end
    else
    begin
      Result := IntToStr(mins) + ' ' + TM + '.';
    end;
  end;
end;

function AddFileToDir(Dir,fn:string):string;
begin
  if Length(Dir) > 0 then
  begin
    if Dir[Length(dir)] = '\' then
      Result := dir + fn
    else
      Result := dir + '\' + fn;
  end
  else
    Result := fn;
end;

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    Exit;
//    raise Exception.Create('Cannot create directory');

  if Dir[length(Dir)] = '\' then
    Delete(Dir,length(Dir),1);

  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;


function WinInetError(Err: Integer):string;
var
  buf:array[0..255] of Char;
  bufsize:integer;
  hwininet:thandle;
begin
  hwininet := GetModuleHandle('wininet.dll');
  bufsize := SizeOf(buf);
  FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,Pointer(hwininet),Err,0,buf,bufsize,nil);
  Result := StrPas(buf);
end;

{ TWebCopy }

procedure TWebCopy.CancelClick(Sender: TObject);
begin
  if Assigned(FOnCopyCancel) then
    FOnCopyCancel(self);
  FCancelled := True;
  FForm.Close;
end;

function TWebCopy.MakeProxyUrl(url: string):string;
begin
  Result := url;
  if (Pos('HTTP://',Uppercase(url)) = 1) and
     (FProxyUserID <> '') and (FProxyPassword <> '') then
  begin
    Delete(url,1,7);
    Result := 'http://' + FProxyUserID + ':' + FProxyPassword + '@' + url;
  end;
end;

function TWebCopy.ExtractServer(url:string): string;
var
  atpos,slpos: integer;
begin
  if Pos('://',UpperCase(url)) > 0 then
    Delete(url,1,Pos('://',url) + 2);

  atpos := Pos('@',UpperCase(url));
  slpos := Pos('/',UpperCase(url));

  if (atpos > 0) and (atpos < slpos) then
    Delete(url,1,Pos('@',url) + 1);

  if Pos('/',url) > 0 then
    url := Copy(url,1,Pos('/',url)-1);

  Result := url;
end;

function TWebCopy.RemoveServer(url:string): string;
begin
  if Pos('://',UpperCase(url)) > 0 then
    Delete(url,1,Pos('://',url) + 2);

  if Pos('@',UpperCase(url)) > 0 then
    Delete(url,1,Pos('@',url) + 1);

  if Pos('/',url) > 0 then
    Delete(url,1,Pos('/',url)-1);

  Result := url;
end;


function TWebCopy.HttpPutFile(url,tgtdir,tgtfn:string; HttpCommand: TWebCopyHTTPCommand): Boolean;
var
  buf:array[0..READBUFFERSIZE - 1] of char;
  bufsize:dword;
  lf: File;
  fn,furl,fsrvr:string;
  fsize,totsize:dword;
  hconnect:hinternet;
  hintfile:hinternet;
  lpdword:dword;
  tck:dword;
  bufferin:INTERNET_BUFFERS;
  ErrCode: Integer;
{$IFDEF TMSDEBUG}
  wsize: integer;
{$ENDIF}
  fm: word;

begin
  Result := False;

  FUrl := url;

  FAnim.ResID := 258;
  FAnim.ResHandle := hinstance;
  FAnim.Active  := True;

  fn := UrlToFile(url);

  url := MakeProxyURL(url);

  if FItems.Count > 1 then
    FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems)+' : '
  else
    FFileLbl.Caption := '';

  if ShowServer then
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + fn + ' ' + FFromServerLabel + ' '  + ExtractServer(url)
  else
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + fn;


  if tgtfn <> '' then
    fn := tgtfn
  else
    fn := URLtoFile(url);

  fn := AddFileToDir(tgtdir,fn);
  if not FileExists(fn) then
    Exit;

  Application.ProcessMessages;

  fsrvr := ExtractServer(url);
  url := RemoveServer(url);
  hconnect := InternetConnect(FHinternet,PChar(fsrvr),INTERNET_DEFAULT_HTTP_PORT,nil,nil,INTERNET_SERVICE_HTTP,0,0);

  if HttpCommand = hcPost then
    hintfile := HttpOpenRequest(HConnect,'POST',PChar(url),nil,nil,nil,INTERNET_FLAG_NO_CACHE_WRITE ,0)
  else
    hintfile := HttpOpenRequest(HConnect,'PUT',PChar(url),nil,nil,nil,INTERNET_FLAG_NO_CACHE_WRITE ,0);

  if hintfile = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to server ' + FSrvr);
    Exit;
  end;

  bufsize := READBUFFERSIZE;

  fm := FileMode;
  FileMode := 0; // openread mode

  if hintfile <> nil then
  begin
    AssignFile(lf,fn);
    Reset(lf,1);
    TotSize := FileSize(lf);

    bufsize := READBUFFERSIZE;
    FSize := 0;

    FillChar(bufferin, SizeOf(bufferin),0);
    bufferin.dwStructSize := SizeOf(INTERNET_BUFFERS);
    bufferin.dwBufferTotal := TotSize;

    if not HttpSendRequestEx(hintfile,@bufferin,nil,HSR_INITIATE,0) then
    begin
      ErrCode := GetLastError;
      CloseFile(lf);
      InternetCloseHandle(hintfile);
      InternetCloseHandle(hconnect);
      if Assigned(FOnError) then
        FOnError(Self, ErrCode);
      if Assigned(FOnErrorInfo) then
        FOnErrorInfo(Self,ErrCode, WinInetError(ErrCode));
      Exit;
    end;

    tck := GetTickCount;

    while (bufsize = READBUFFERSIZE) and not FCancelled do
    begin
      BlockRead(lf,buf,READBUFFERSIZE,bufsize);
      if not InternetWriteFile(hintfile,@buf,bufsize,lpdword) then
      begin
        ErrCode := GetLastError;
        if Assigned(FOnError) then
          FOnError(Self, ErrCode);
        if Assigned(FOnErrorInfo) then
          FOnErrorInfo(Self,ErrCode, WinInetError(ErrCode));
        Break;
      end;

      {$IFDEF TMSDEBUG}
      wsize := wsize + lpdword;
      outputdebugstring(PChar('bytes written = '+inttostr(lpdword)+' total  = '+inttostr(wsize)));
      {$ENDIF}

      FSize := FSize + bufsize;

      if lpdword > 0 then
      begin
        FProgress.Position := Round(100 * (FSize / lpdword));
        FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize) + ' ' + DlgFileOfLabel + ' ' + FileSizeFmt(TotSize);
        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar('bytes read = '+inttostr(FSize)+ ' of '+inttostr(totsize)));
        {$ENDIF}
      end
      else
        FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize);

      if FShowCompletion then
        FForm.Caption := DlgUpload + ' : ' + IntToStr(FProgress.Position) + '% ' + DlgCompleted;

      FRateStr := FileSizeFmtSpeed(Round(FSize/((GetTickCount-tck)+1)*1000));

      FRatelbl.Caption := FRateLabel + ' ' + FRateStr;

      FTimeLeftStr := TimeFmt(GetTickCount  - tck,FSize,TotSize, FDlgTimeSec, FDlgTimeMin);

      FTimeLbl.Caption := FTimeLabel + ' ' + FTimeLeftStr;

      FElapsedTimeStr := TimeFmt(GetTickCount-tck,0,TotSize, FDlgTimeSec, FDlgTimeMin);

      FElapsedTimeLbl.Caption := FElapsedTimeLabel + ' ' + FElapsedTimeStr;

      if Assigned(FOnCopyProgress) then
        FOnCopyProgress(Self,FFilenum,FSize,TotSize);

      Application.Processmessages;
    end;

    if not HttpEndRequest(hintfile,nil,0,0) then
    begin
      ErrCode := GetLastError;
      if Assigned(FOnError) then
        FOnError(Self, ErrCode);
      if Assigned(FOnErrorInfo) then
        FOnErrorInfo(Self,ErrCode, WinInetError(ErrCode));
    end;
    CloseFile(lf);

    InternetCloseHandle(hintfile);
    InternetCloseHandle(hconnect);
    FileMode := fm;
    Result := FSize = TotSize;
  end;
end;

function AddBackslash(const s: string): string;
begin
  if (Length(s) >= 1) and (s[Length(s)]<>'\') then
    Result := s + '\'
  else
    Result := s;
end;

Procedure FreePidl( pidl: PItemIDList );
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


procedure TWebCopy.Log(s: string);
var
  tf:text;

begin
  if FLogFileName = '' then
    Exit;

  AssignFile(tf,FLogFileName);
  {$i-}
  Append(tf);
  {$i+}
  if IOResult <> 0 then
  begin
    {$i-}
    Rewrite(tf);
    {$i+}
    if IOResult <> 0 then
      Exit;
  end;

  Writeln(tf,FormatDateTime('hh:nn dd/mm/yyyy : ',Now),s);
  CloseFile(tf);
end;

//-----------------------------
// download files over a HTTP
//-----------------------------
function URLtoDomain(url:string):string;
begin
  if pos('://',url) > 0 then
    delete(url,1, pos('://',url)+2);

  if pos('/',url) > 0 then
    delete(url,pos('/',url),length(url));

  if pos('\',url) > 0 then
    delete(url,pos('/',url),length(url));

  Result := url;
end;

function ExtractFileNameWithoutExt(FileName: string): string;
var
  fe: string;
begin                                                     
  fe := ExtractFileExt(FileName);

  delete(FileName, length(filename) - length(fe) + 1, length(fe));

  Result := FileName;
end;

function CreateUniqueFilename(FileName: string): string;
var
  i: integer;
  fn, fe: string;
  exists: boolean;
begin
  i := 1;
  fn := ExtractFileNameWithoutExt(FileName);
  fe := ExtractFileExt(FileName);

  repeat
    Result := fn + '('+inttostr(i)+')'+ fe;
    exists := FileExists(Result);
    inc(i);
  until not exists;
end;


function TWebCopy.HttpGetFile(idx:Integer; url,displayurl,tgtdir,tgtfn:string;
  UseDate: Boolean; var FileDate: TDateTime; var NoNew: Boolean; Auth: TWebCopyAuthentication; UserID, Password:string): Boolean;
var
  buf: array[0..READBUFFERSIZE-1] of char;
  bufsize: DWord;
  lf: file;
  displfn,fn,furl: string;
  fsize: int64;
  hintfile: hinternet;
  lpdwlen,lpdwidx,lpdword,tck: DWord;
  allow: Boolean;
  datebuf: _SYSTEMTIME;
  ftime: TDateTime;
  Flags: DWORD;
  szUser, szPassword, szHeaders:string;

label
  retryafterlogin;

begin
  Result := False;
  NoNew := False;
  FUrl := url;

  fn := UrlToFile(url);

  url := MakeProxyURL(url);

  if FItems.Count > 1 then
    FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  if DisplayURL <> '' then
    displfn := DisplayURL
  else
    displfn := fn;

  if ShowServer then
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + displfn + ' ' + FFromServerLabel + ' ' + ExtractServer(url)
  else
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + displfn;

  Application.Processmessages;

  Flags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE;
  if FKeepAlive then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;

  if (Auth = waAlways) and ((UserID = '') or (Password = '')) then
  begin
    szUser := UserID;
    szPassword := Password;

    if not GetLogin('Connect to '+URLToDomain(url),szUser, szPassword) then
      Exit;

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

  hintfile := InternetOpenURL(fhinternet,PChar(url),PChar(szHeaders),length(szHeaders),Flags,0);
  lpdwlen := 4;
  lpdwidx := 0;

  Application.ProcessMessages;

  if hintfile <> nil then
  begin
    HttpQueryInfo(hintfile,HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER ,@lpdword,lpdwlen,lpdwidx);

    if (lpdword = 401) and (Auth in [waAuto,waAlways]) then
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
        Exit;
    end;

    if lpdword >= 300 then
    begin
      InternetCloseHandle(hintfile);
      if Assigned(FOnError) then
        FOnError(Self,lpdword);

      if Assigned(FOnErrorInfo) then
        FOnErrorInfo(Self,lpdword,'Returned http error code '+IntToStr(lpdword));

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('Http status : '+inttostr(lpdword)));
      {$ENDIF}
      Exit;
    end;
  end;

  if hintfile = nil then
  begin
    if Assigned(FOnURLNotFound) then
      FOnURLNotFound(Self,url);
    {$IFDEF TMSDEBUG}
    outputdebugstring('could not open file');
    {$ENDIF}
    Exit;
  end;

  if UseDate or Assigned(FOnFileDateCheck) then
  begin
    lpdwlen := SizeOf(datebuf);
    lpdwidx := 0;
    HttpQueryInfo(hintfile,HTTP_QUERY_LAST_MODIFIED or HTTP_QUERY_FLAG_SYSTEMTIME,@datebuf,lpdwlen,lpdwidx);

    SystemTimeToDateTime(datebuf,FTime);

    {$IFDEF TMSDEBUG}
     outputdebugstring(pchar(formatdatetime('dd/mm/yyyy hh:nn:ss',ftime)));
    {$ENDIF}

    if Assigned(FOnFileDateCheck) then
    begin
      allow := true;
      FileDate := FTime;
      FOnFileDateCheck(Self,idx,ftime,allow);
      if not allow then
      begin
        InternetCloseHandle(hintfile);
        Exit;
      end;
    end
    else
      if ftime <= FileDate then
      begin
        NoNew := True;
        FileDate := FTime;

        InternetCloseHandle(hintfile);

        if Assigned(FOnNoNewFile) then
          FOnNoNewFile(Self,fn,FileDate,FTime);

        Exit;
      end;
    FileDate := FTime;
  end;

  if tgtfn <> '' then
    fn := tgtfn
  else
  begin
    fn := URLtoFile(url);
    if AutoURLDecode then
      fn := UrlDecode(fn);
  end;

  fn := AddFileToDir(tgtdir,fn);

  FLastFile := fn;
  FLastDir := tgtdir;

  if FileExists(fn) then
  begin
    Allow := True;
    if Assigned(FOnCopyOverwrite) then
      FOnCopyOverwrite(self,fn,Allow);

    if not Allow then
    begin
      InternetCloseHandle(hintfile);
      Exit;
    end;

    if CreateUniqueFilenames then
      fn := CreateUniqueFilename(fn);
  end;

  ForceDirectories(ExtractFilePath(fn));
  AssignFile(lf,fn);
  {$i-}
  Rewrite(lf,1);
  {$i+}
  if IOResult <> 0 then
  begin
    InternetCloseHandle(hintfile);

    if Assigned(FOnError) then
      FOnError(Self,errCannotCreateTargetFile);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotCreateTargetFile,'Cannot create file '+fn);

    Exit;
  end;

  bufsize := READBUFFERSIZE;

  lpdword:=0;
  lpdwlen:=4;
  lpdwidx:=0;

  HttpQueryInfo(hintfile,HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER ,@lpdword,lpdwlen,lpdwidx);

  if (lpdword = 0) and (FItems[idx].FFileSize > 0) then
    lpdword := FItems[idx].FFileSize;

  FSize := 0;

  tck := GetTickCount;
  bufsize := READBUFFERSIZE;

  {$IFDEF DELPHI_UNICODE}
  if lpdword > 0 then
    FProgress.Style := pbstNormal
  else
    FProgress.Style := pbstMarquee;
  {$ENDIF}

  while (bufsize > 0) and not FCancelled do
  begin
    Application.Processmessages;
    if not InternetReadFile(hintfile,@buf,READBUFFERSIZE,bufsize) then Break;

    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('read from http = '+inttostr(bufsize)));
    {$ENDIF}

    if (bufsize > 0) and (bufsize <= READBUFFERSIZE) then
      BlockWrite(lf,buf,bufsize);


    FSize := FSize + bufsize;

    if lpdword > 0 then
    begin
      FProgress.Position := Round(100 * (FSize / lpdword));
      FSizelbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize)+ ' ' +DlgFileOfLabel +' ' +FileSizeFmt(lpdword);
    end
    else
    begin
      FSizelbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize);
    end;


    if FShowCompletion then
      FForm.Caption := DlgDwnload + ' : ' + IntToStr(FProgress.Position) + '% ' + DlgCompleted;

    FRateStr := FileSizeFmtSpeed(Round(FSize/((GetTickCount-tck)+1)*1000));
    FRateLbl.Caption := FRateLabel + ' ' + FRateStr;

    FTimeLeftStr := TimeFmt(GetTickCount - tck,FSize,lpdword, FDlgTimeSec, FDlgTimeMin);
    FTimeLbl.Caption := FTimeLabel + ' ' + FTimeLeftStr;

    FElapsedTimeStr := TimeFmt(GetTickCount - tck,0,lpdword, FDlgTimeSec, FDlgTimeMin);
    FElapsedTimeLbl.Caption := FElapsedTimeLabel + ' ' + FElapsedTimeStr;

    if Assigned(FOnCopyProgress) then
      FOnCopyProgress(Self,FFilenum,FSize,lpdword);

    Application.Processmessages;

    if bufsize > 0 then
      Result := True;
  end;
  CloseFile(lf);

  if FCancelled then
    Deletefile(fn);

  InternetCloseHandle(hintfile);
end;

//-----------------------
// copy files over a LAN
//-----------------------
function TWebCopy.FileGetFile(idx: Integer;fuserid, fpassword, fhost: string; fPort: integer;
  url, tgtdir,tgtfn: string; UseDate: Boolean; var FileDate: TDateTime;var NoNew: Boolean): Boolean;

const
  BlockSize = $4000;
type
  pBuf = ^tBuf;
  TBuf = array[1..BlockSize] of char;
var
  SourceSize: LongInt;
  sour: tFileRec;
  targ: tFileRec;
  numRead, numWritten: integer;
  fbuf: pBuf;
  fsize: DWORD;
  tck: dword;
  NetResource: TNetResource;
  dwFlags: DWORD;
  source, dest: string;
  allow:boolean;
  crT,laT,lwT, llT: _FileTime;
  FTime: TDateTime;
  ratio: double;

label
  CleanExit;

begin
  NoNew := False;
  Result := False;

  FAnim.CommonAVI := aviCopyFiles;
  FAnim.Active  := True;

  if FItems.Count > 1 then
    FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  if FHost <> '' then
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + URLToFile(url) + ' '+FFromServerLabel+' ' + FHost
  else
    FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + URLToFile(url) + ' '+FFromServerLabel+' ' + ExtractFileDir(URL);


  if FHost <> '' then
  begin
    NetResource.dwType := RESOURCETYPE_DISK;
    NetResource.lpLocalName := '';
    NetResource.lpRemoteName := PChar(fHost);
    NetResource.lpProvider := '';
    dwFlags := 0;
    if WNetAddConnection2(NetResource, PChar(fpassword), PChar(fuserid), dwFlags) <> NO_ERROR then
    begin
      if Assigned(FOnConnectError) then
        FOnConnectError(self);
      goto CleanExit;
    end;
  end;

  FSize := 0;
  tck := GetTickCount;

  if FHost <> '' then
    Source := FHost + '\' + url
  else
    Source := url;

  if (pos('FILE://',UpperCase(Source)) = 1) then
    Delete(Source,1,7);


  if tgtfn <> '' then
    dest := tgtfn
  else
    dest := URLtoFile(url);

  dest := AddFileToDir(tgtdir,dest);

  FLastFile := dest;
  FLastDir := tgtdir;

   // if source file and target file are identical raises an error
  if source = dest then
  begin
    if Assigned(OnError) then
      OnError(Self,errFilesIdentical);
    if Assigned(OnErrorInfo) then
      OnErrorInfo(Self,errFilesIdentical,'Cannot copy identical files '+Source);
    goto CleanExit;
  end;
  new(FBuf);
  // tries to open the source file
  Sour.Handle := FileOpen(Source, fmOpenRead);
  if Sour.handle = -1 then
  begin
    Dispose(FBuf);
    if Assigned(OnError) then
       OnError(Self,errCannotOpenSourceFile);
    if Assigned(OnErrorInfo) then
       OnErrorInfo(Self,errCannotOpenSourceFile,'Cannot open source file '+Source);

    goto CleanExit;
  end;
  if UseDate or Assigned(FOnFileDateCheck) then
  begin
    GetFileTime(sour.Handle,@crT,@laT,@lwT);
    FileTimeToLocalFileTime(lwT, llT);
    FileTimeToDateTime(llT,FTime);
    if Assigned(OnFileDateCheck) then
    begin
      allow := true;
      FOnFileDateCheck(Self,idx,ftime,allow);
      if not Allow then
      begin
        Dispose(FBuf);
        FileClose(Sour.Handle);
        goto CleanExit;
      end;
    end
    else
      if FTime <= FileDate then
      begin
        NoNew := True;
        FileDate := FTime;
        Dispose(FBuf);
        FileClose(Sour.Handle);
        if Assigned(FOnNoNewFile) then
          FOnNoNewFile(Self,Source,FileDate,FTime);
        goto CleanExit;
      end;
    FileDate := FTime;
  end;

  // compute how many block are needed
  SourceSize := FileSeek(Sour.handle, 0, 2);
  if SourceSize = -1 then
  begin
    FileClose(Sour.Handle);
    Dispose(FBuf);
    if Assigned(OnError) then
      OnError(Self,errSourceFileZeroLength);
    if Assigned(OnErrorInfo) then
      OnErrorInfo(Self,errSourceFileZeroLength,'Zero length source file '+Source);

    goto CleanExit;
  end;

  // set the handle to the file beginning
  FileSeek(Sour.handle, 0, 0);

  if FileExists(dest) then
  begin
    Allow := True;
    if Assigned(FOnCopyOverwrite) then
      FOnCopyOverwrite(self,dest,allow);
    if not Allow then
    begin
      FileClose(Sour.Handle);
      Dispose(FBuf);
      goto CleanExit;
    end;

    if CreateUniqueFilenames then
      dest := CreateUniqueFilename(dest);
  end;

  ForceDirectories(ExtractFilePath(Dest));

  // tries to create the target file
  Targ.handle := FileCreate(Dest);
  if Targ.Handle < 0 then
  begin
    FileClose(Sour.Handle);
    Dispose(FBuf);
    if Assigned(OnError) then
      OnError(Self,errCannotCreateTargetFile);
    if Assigned(OnErrorInfo) then
      OnErrorInfo(Self,errCannotCreateTargetFile,'Cannot create target file ' + Dest);
    goto CleanExit;
  end;

  FCancelled := False;

  // copy block
  repeat
    // reading
    numRead := FileRead(Sour.Handle, FBuf^, sizeOf(FBuf^));
    if numRead < 0 then
    begin
      Dispose(FBuf);
      FileClose(Sour.Handle);
      FileClose(Targ.handle);
      if Assigned(OnError) then
        OnError(Self,errCopyReadFailure);
      if Assigned(OnErrorInfo) then
        OnErrorInfo(Self,errCopyReadFailure,'Error reading source file ' + Source);
      goto CleanExit;
    end;

    // writing
    numWritten := FileWrite(Targ.Handle, FBuf^, numRead);
    if numWritten < 0 then
    begin
      FileClose(Sour.Handle);
      FileClose(Targ.handle);
      Dispose(FBuf);
      if Assigned(OnError) then
        OnError(Self,errCopyWriteFailure);
      if Assigned(OnErrorInfo) then
        OnErrorInfo(Self,errCopyWriteFailure,'Error writing to destination file '+Dest);
      goto CleanExit;
    end;

    FSize := FSize + DWORD(numWritten);

    if SourceSize > 0 then
    begin
      ratio := FSize;
      ratio := ratio/SourceSize;
      FProgress.Position := Round(100 * ratio)
    end
    else
      FProgress.Position := 100;

    FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize) + ' ' + DlgFileOfLabel + ' ' + FileSizeFmt(Sourcesize);

    FRateStr := FileSizeFmtSpeed(Round(FSize / ((GetTickCount - tck) + 1) * 1000));

    FRateLbl.Caption := FRateLabel + ' ' + FRateStr;

    if FShowCompletion then
      FForm.Caption := DlgCopying + ' : ' + IntToStr(FProgress.Position) + '% ' + DlgCompleted;

    FTimeLeftStr := TimeFmt(GetTickCount-tck,FSize,SourceSize, FDlgTimeSec, FDlgTimeMin);

    FTimeLbl.Caption := FTimeLabel + ' ' + FTimeLeftStr;

    FElapsedTimeStr := TimeFmt(GetTickCount-tck,0,SourceSize, FDlgTimeSec, FDlgTimeMin);

    FElapsedTimeLbl.Caption := FElapsedTimeLabel + ' ' + FElapsedTimeStr;

    if Assigned(FOnCopyProgress) then
      FOnCopyProgress(Self,FFileNum,FSize,SourceSize);


    Application.ProcessMessages;

  until (numRead = 0) or (numRead <> numWritten) or FCancelled;

  FileClose(Sour.Handle);
  FileClose(Targ.handle);
  Dispose(FBuf);

  Result := true;

CleanExit:

  if FHost <> '' then
  begin
    if WNetCancelConnection2(PChar(fhost), CONNECT_UPDATE_PROFILE, true) <> NO_ERROR then
    begin
      if Assigned(FOnConnectError) then
        FOnConnectError(Self);
      Result := false;
    end;
  end;
end;

procedure TWebCopy.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TWebCopy.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := DlgShowCloseBtn or FCancelIsClose;
end;

function TWebCopy.MultiFtpGetList(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
  URL, TgtDir,TgtFn: string; UseDate: Boolean;var FileDate:TDateTime; var NoNew: Boolean; KeepConnect: boolean): Boolean;
var
  hintconnect: hinternet;
  hintfind: hinternet;
  ftpflag: Integer;
  fn,fdir: string;
  ffi: TWin32FindData;

  {$IFNDEF DELPHI_UNICODE}
  origdir: array[0..255] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  origdir: String;
  {$ENDIF}
  dirs: DWORD;
  chdir: boolean;
  i: integer;

begin
  Result := False;
  NoNew := False;
  KeepConnect := false;

  if FItems.Count > 1 then
    FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  if ShowServer then
    FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url) + ' ' + FFromServerLabel+' '+FHost
  else
    FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url);

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+FHost));
  outputdebugstring(pchar('userid:'+FUserID));
  outputdebugstring(pchar('pwd:'+FPassword));
  {$ENDIF}

  Application.ProcessMessages;

  //if (FPrevConnect = FHost + FUserID + FPassword) then
  //begin
  //  hintconnect := FFTPConnect;
  //end
  //else
  begin
    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    //FFTPConnect := hintconnect;
    //FPrevConnect := FHost + FUserID + FPassword;
  end;

  chdir := false;

  Application.ProcessMessages;

  if pos('\',url) > 0 then
    url := StringReplace(url, '\','/',[rfReplaceAll]);

  if (Pos('/',url) > 0) then
  begin
    FDir := url;
    while (FDir[Length(FDir)] <> '/') and (Length(FDir) > 0) do
      Delete(FDir,Length(FDir),1);

    if Length(FDir) > 0 then
    begin
      dirs := 255;
      {$IFDEF DELPHI_UNICODE}
      SetLength(origdir, dirs);
      FtpGetCurrentDirectory(hintconnect, PChar(origdir), dirs);
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      FtpGetCurrentDirectory(hintconnect, origdir, dirs);
      {$ENDIF}
      chdir := true;
      FtpSetCurrentDirectory(hintconnect,PChar(FDir));
    end;
  end;

  fn := URLtoFile(url);

  hintfind := FtpFindFirstFile(hintconnect,PChar(fn),ffi,INTERNET_FLAG_RELOAD,0);

  if Assigned(Items[idx].FFileList) then
    Items[idx].FFileList.Free;

  if Assigned(Items[idx].FFileDetailList) then
    Items[idx].FFileDetailList.Free;

  Items[idx].FFileList := TStringList.Create;
  Items[idx].FFileDetailList := TFileDetailItems.Create;

  if not (hintfind = nil) then
  begin
    Items[idx].FFileList.Add(strpas(ffi.cFileName));

    with Items[idx].FFileDetailList.Add do
    begin
      FileName := strpas(ffi.cFileName);
      FileAttributes := ffi.dwFileAttributes;
      FileSizeLow := ffi.nFileSizeLow;
      FileSizeHigh := ffi.nFileSizeHigh;
      FileDate := ffi.ftLastWriteTime;
    end;

    i := 0;
    while InternetFindNextFile(hintfind, @ffi) do
    begin
      Items[idx].FFileList.Add(strpas(ffi.cFileName));
      with Items[idx].FFileDetailList.Add do
      begin
        FileName := strpas(ffi.cFileName);
        FileAttributes := ffi.dwFileAttributes;
        FileSizeLow := ffi.nFileSizeLow;
        FileSizeHigh := ffi.nFileSizeHigh;
        FileDate := ffi.ftLastWriteTime;
      end;

      FFileLbl.Caption := IntToStr(i + 1) + ' ' + strpas(ffi.cFileName);
    end;

    InternetCloseHandle(hintfind);
  end;

  if not KeepConnect then
  begin
    InternetCloseHandle(hintconnect);
    FPrevConnect := '';
  end
  else
    if chdir then
    begin
      if OrigDir = '' then
        OrigDir := '/';

      {$IFDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,OrigDir);
      {$ENDIF}
    end;

  Result := True;
end;


function TWebCopy.MultiFtpGetFile(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
  URL, TgtDir,TgtFn: string; UseDate: Boolean;var FileDate:TDateTime; var NoNew: Boolean; KeepConnect: boolean): Boolean;
var
  hintconnect: hinternet;
  hintfind: hinternet;
  ftpflag: Integer;
  fn,fdir: string;
  ffi: TWin32FindData;
  {$IFNDEF DELPHI_UNICODE}
  origdir: array[0..255] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  origdir: String;
  {$ENDIF}
  dirs: DWORD;
  chdir: boolean;
  sl: TStringlist;
  i: integer;
  fdt: TDateTime;

begin
  Result := False;
  NoNew := False;
  KeepConnect := true;

  if FItems.Count > 1 then
    FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  if ShowServer then
    FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url) + ' ' + FFromServerLabel+' '+FHost
  else
    FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url);

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+FHost));
  outputdebugstring(pchar('userid:'+FUserID));
  outputdebugstring(pchar('pwd:'+FPassword));
  {$ENDIF}

  Application.ProcessMessages;

  //if (FPrevConnect = FHost + FUserID + FPassword) then
  //begin
  //  hintconnect := FFTPConnect;
  //end
  //else
  begin
    if (FUserID = '') or (FPassword = '') then
       hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    FFTPConnect := hintconnect;
    FPrevConnect := FHost + FUserID + FPassword;
  end;

  chdir := false;

  Application.ProcessMessages;

  if pos('\',url) > 0 then
    url := StringReplace(url, '\','/',[rfReplaceAll]);

  if (Pos('/',url) > 0) then
  begin
    FDir := url;
    while (FDir[Length(FDir)] <> '/') and (Length(FDir) > 0) do
      Delete(FDir,Length(FDir),1);

    if Length(FDir) > 0 then
    begin
      dirs := 255;
      {$IFDEF DELPHI_UNICODE}
      SetLength(origdir, dirs);
      FtpGetCurrentDirectory(hintconnect, PChar(origdir), dirs);
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      FtpGetCurrentDirectory(hintconnect, origdir, dirs);
      {$ENDIF}
      chdir := true;
      FtpSetCurrentDirectory(hintconnect,PChar(FDir));
    end;
  end;

  fn := URLtoFile(url);

  hintfind := FtpFindFirstFile(hintconnect,PChar(fn),ffi,0,0);

  sl := TStringList.Create;

  if not (hintfind = nil) then
  begin
    sl.Add(strpas(ffi.cFileName));
    while InternetFindNextFile(hintfind, @ffi) do
    begin
    sl.Add(strpas(ffi.cFileName));
    end;

    InternetCloseHandle(hintfind);
  end;

  for i := 0 to sl.Count - 1 do
  begin
    if sl.Count > 0 then
      FFileLbl.Caption := IntToStr(i + 1) + ' ' + FFileOfLabel + ' ' + IntToStr(sl.Count) + ' : ';

    if ShowServer then
      FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + sl[i] + ' ' + FFromServerLabel+' '+FHost
    else
      FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + sl[i];

    fdt := FileDate;

    FtpGetFile(i,FUserID, FPassword, FHost, FPort, sl.Strings[i], TgtDir, sl[i], UseDate, fdt, nonew, KeepConnect, false);

    if Assigned(OnMultiFTPFileDone) then
      OnMultiFTPFileDone(Self, sl.Strings[i]);
  end;

  sl.Free;

  if not KeepConnect then
  begin
    InternetCloseHandle(hintconnect);
    FPrevConnect := '';
  end
  else
    if chdir then
    begin
      if OrigDir = '' then
        OrigDir := '/';

      {$IFDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,OrigDir);
      {$ENDIF}
    end;

  Result := True;
end;

//---------------------------
// Get files from FTP server
//---------------------------
function TWebCopy.FtpGetFile(idx: Integer;FUserid,FPassword,FHost:string;FPort:Integer;
  URL, TgtDir,TgtFn: string; UseDate: Boolean;var FileDate:TDateTime; var NoNew: Boolean; KeepConnect, ShowFileInfo: boolean): Boolean;
var
  hintconnect: hinternet;
  hintfile: hinternet;
  hintfind: hinternet;
  ftpflag: Integer;
  buf: array[0..READBUFFERSIZE - 1] of Char;
  bufsize: DWord;
  lf: file;
  fn,fdir: string;
  tck: DWord;
  fsize,lpdword,h: int64;
  ffi: TWin32FindData;
  Allow: Boolean;
  FTime: TDateTime;
  {$IFNDEF DELPHI_UNICODE}
  origdir: array[0..255] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  origdir: String;
  {$ENDIF}
  dirs: DWORD;
  ftpmodeflag: DWORD;
  chdir,notfound: boolean;

begin
  Result := False;
  NoNew := False;
  FCancelled := False;

  if ShowFileInfo then
  begin
    if FItems.Count > 1 then
      FFileLbl.Caption := IntToStr(FFileNum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
    else
      FFileLbl.Caption := '';

    if ShowServer then
      FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url) + ' ' + FFromServerLabel+' '+FHost
    else
      FFileLbl.Caption := FFileLabel + ' ' + FFilelbl.caption + URLToFile(url);
  end;

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+FHost));
  outputdebugstring(pchar('userid:'+FUserID));
  outputdebugstring(pchar('pwd:'+FPassword));
  {$ENDIF}

  Application.ProcessMessages;

  if (FPrevConnect = FHost + FUserID + FPassword) then
  begin
    hintconnect := FFTPConnect;
  end
  else
  begin
    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,INTERNET_FLAG_DONT_CACHE)
    else
      hintconnect := InternetConnect(FHinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,INTERNET_FLAG_DONT_CACHE);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);

    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    FFTPConnect := hintconnect;
    FPrevConnect := FHost + FUserID + FPassword;
  end;

  chdir := false;

  Application.ProcessMessages;

  if Pos('/',url) > 0 then
  begin

    FDir := url;
    while (FDir[Length(FDir)] <> '/') and (Length(FDir) > 0) do
      Delete(FDir,Length(FDir),1);
    if Length(FDir) > 0 then
    begin
      dirs := 255;
      {$IFNDEF DELPHI_UNICODE}
      FtpGetCurrentDirectory(hintconnect, origdir, dirs);
      {$ENDIF}
      {$IFDEF DELPHI_UNICODE}
      SetLength(origdir, dirs);
      FtpGetCurrentDirectory(hintconnect, PChar(origdir), dirs);
      {$ENDIF}
      chdir := true;
      FtpSetCurrentDirectory(hintconnect,PChar(FDir));
    end;
 end;

  notfound := false;
  fn := URLtoFile(url);
  hintfind := FtpFindFirstFile(hintconnect,PChar(fn),ffi,0,0);

  if hintfind = nil then
  begin
    // file not found
    notfound := true;
    lpdword := 0;
  end
  else
  begin
    InternetCloseHandle(hintfind);

    h := $80000000;
    h := h * 2;
    lpdword := int64(ffi.nFileSizeHigh * h) + int64(ffi.nFileSizeLow);

    if UseDate or Assigned(FOnFileDateCheck) then
    begin
      FileTimeToDateTime(ffi.ftLastWriteTime, FTime);
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar(formatdatetime('dd/mm/yyyy hh:nn:ss',ftime)));
      {$ENDIF}
      if Assigned(FOnFileDateCheck) then
      begin
        Allow := true;
        FOnFileDateCheck(Self,idx,FTime,allow);
        FileDate := FTime;
        if not Allow then
        begin
          if not KeepConnect then
          begin
            InternetCloseHandle(hintconnect);
            FPrevConnect := '';
          end
          else
           if chdir then
           begin
             {$IFNDEF DELPHI_UNICODE}
             FtpSetCurrentDirectory(hintconnect,OrigDir);
             {$ENDIF}
             {$IFDEF DELPHI_UNICODE}
             FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
             {$ENDIF}
           end;

          Exit;
        end;
      end
      else
        if FTime <= FileDate then
        begin
          NoNew := True;
          FileDate := FTime;

          if not KeepConnect then
          begin
            InternetCloseHandle(hintconnect);
            FPrevConnect := '';
          end;

          if Assigned(FOnNoNewFile) then
            FOnNoNewFile(Self,fn,FileDate,FTime);
          Exit;
        end;
      FileDate := FTime;
    end;
  end;

  Application.ProcessMessages;

  if FTPMode = fmBINARY then
    ftpmodeflag := FTP_TRANSFER_TYPE_BINARY
  else
    ftpmodeflag := FTP_TRANSFER_TYPE_ASCII;

  hintfile := FtpOpenFile(hintconnect,PChar(fn),GENERIC_READ,ftpmodeflag or INTERNET_FLAG_RESYNCHRONIZE or INTERNET_FLAG_RELOAD or INTERNET_FLAG_DONT_CACHE,0);

  if (hintfile <> nil) and (lpdword = 0) then
    lpdword := FtpGetFileSize(hintfile, nil);

  Application.ProcessMessages;

  if (hintfile = nil) or notfound then
  begin
    if Assigned(FOnURLNotFound) then
      FOnURLNotFound(Self,url);
    if not KeepConnect then
    begin
      InternetCloseHandle(hintconnect);
      FPrevConnect := '';
    end
    else
      if chdir then
      begin
       {$IFNDEF DELPHI_UNICODE}
       FtpSetCurrentDirectory(hintconnect,OrigDir);
       {$ENDIF}
       {$IFDEF DELPHI_UNICODE}
        FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
       {$ENDIF}
      end;
    Exit;
  end;
  if tgtfn <> '' then
    fn := tgtfn
  else
    fn := URLtoFile(url);

  fn := AddFileToDir(tgtdir,fn);

  FLastFile := fn;
  FLastDir := tgtdir;

  if FileExists(fn) then
  begin
    Allow := True;
    if Assigned(FOnCopyOverwrite) then
      FOnCopyOverwrite(Self,fn,Allow);
    if not Allow then
    begin
      InternetCloseHandle(hintfile);

      if not KeepConnect then
      begin
        InternetCloseHandle(hintconnect);
        FPrevConnect := '';
      end
      else
        if chdir then
        begin
          if OrigDir = '' then
            OrigDir := '/';
          {$IFNDEF DELPHI_UNICODE}
          FtpSetCurrentDirectory(hintconnect,OrigDir);
          {$ENDIF}
          {$IFDEF DELPHI_UNICODE}
          FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
          {$ENDIF}
        end;

      Exit;
    end;
    if FCreateUniqueFilenames then
      fn := CreateUniqueFilename(fn);
  end;

  ForceDirectories(ExtractFilePath(fn));
  AssignFile(lf,fn);
  {$i-}
  Rewrite(lf,1);
  {$i+}
  if IOResult <> 0 then
  begin
    InternetCloseHandle(hintfile);

    if not KeepConnect then
    begin
      InternetCloseHandle(hintconnect);
      FPrevConnect := '';
    end;

    if Assigned(FOnError) then
      FOnError(Self,errCannotCreateTargetFile);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotCreateTargetFile,'Cannot create target file '+fn);
    Exit;
  end;

  bufsize := READBUFFERSIZE;

  FSize := 0;

  tck := GetTickCount;

  bufsize := READBUFFERSIZE;
  while (bufsize > 0) and not FCancelled  do
  begin
    Application.ProcessMessages;
    if not InternetReadFile(hintfile,@buf,READBUFFERSIZE,bufsize) then Break;
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('read from ftp = '+inttostr(bufsize)));
    {$ENDIF}
    if (bufsize > 0) and (bufsize <= READBUFFERSIZE) then
      BlockWrite(lf,buf,bufsize);
    FSize := FSize + bufsize;

    if lpdword > 0 then
    begin
      FProgress.Position := Round(100 * (FSize / lpdword));
      FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize) + ' ' + DlgFileOfLabel + ' '+ FileSizeFmt(lpdword);
    end
    else
      FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize);

    if FShowCompletion then
      FForm.Caption := DlgDwnload + ' : ' + IntToStr(FProgress.Position) + '% ' + DlgCompleted;

    FRateStr := FileSizeFmtSpeed(Round(FSize/((GetTickCount-tck)+1)*1000));
    FRateLbl.Caption := FRateLabel + ' ' + FRateStr;

    FTimeLeftStr := TimeFmt(GetTickCount-tck,FSize,lpdword, FDlgTimeSec, FDlgTimeMin);
    FTimeLbl.Caption := FTimeLabel + ' '+ FTimeLeftStr;

    FElapsedTimeStr := TimeFmt(GetTickCount-tck,0,lpdword, FDlgTimeSec, FDlgTimeMin);
    FElapsedTimeLbl.Caption := FElapsedTimeLabel + ' ' + FElapsedTimeStr;

    if Assigned(FOnCopyProgress) then
      FOnCopyProgress(Self,FFilenum,FSize,lpdword);

    Application.ProcessMessages;

  end;
  Closefile(lf);

  InternetCloseHandle(hintfile);

  if not KeepConnect then
  begin
    InternetCloseHandle(hintconnect);
    FPrevConnect := '';
  end
  else
    if chdir then
    begin
      {$IFNDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,OrigDir);
      {$ENDIF}
      {$IFDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
      {$ENDIF}
    end;

  Result := True;
end;

//--------------------------
// Put multiple files to FTP
//--------------------------
function TWebCopy.MultiFtpPutFile(idx: Integer; FUserid,FPassword,FHost:string;FPort: Integer;
  URL, TgtDir,TgtFn: string; UseDate: Boolean;var FileDate: TDateTime; var NoNew: Boolean; KeepConnect: Boolean): Boolean;
var
  hintconnect: hinternet;
  ftpflag: Integer;
  fn,ufn: string;
  SR: TSearchRec;
  fldr: string;
  fldrpart: string;

  {$IFNDEF DELPHI_UNICODE}
  origdir: array[0..255] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  origdir: String;
  {$ENDIF}
  dirs: DWORD;

begin
  NoNew := False;
  Result := False;
  FAnim.ResID := 258;
  FAnim.ResHandle := hinstance;
  FAnim.Active  := True;

  if FItems.Count>1 then
    FFileLbl.Caption := IntToStr(FFilenum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + URLToFile(url) + ' ' + FToServerLabel+' '+Fhost;

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  Application.ProcessMessages;
  //if (FPrevConnect = FHost + FUserID + FPassword) then
  //begin
  //  hintconnect := FFTPConnect;
  //end
  //else
  begin
    FPrevTarget := '';

    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    FFTPConnect := hintconnect;
    FPrevConnect := FHost + FUserID + FPassword;
  end;

  Application.ProcessMessages;

  begin
    if KeepConnect then
      FPrevTarget := tgtdir
    else
      FPrevTarget := '';

    dirs := 255;

    {$IFNDEF DELPHI_UNICODE}
    FtpGetCurrentDirectory(hintconnect, origdir, dirs);
    {$ENDIF}
    {$IFDEF DELPHI_UNICODE}
    SetLength(origdir, dirs);
    FtpGetCurrentDirectory(hintconnect, PChar(origdir), dirs);
    {$ENDIF}

    if not FtpSetCurrentDirectory(hintconnect,PChar(tgtdir)) then
    begin  // cannot set the full targetdir, split and try to set subfolder by subfolder
      fldr := ForwardSlashes(tgtdir);
      FPrevTarget := '';

      repeat
        if pos('/', fldr) > 0 then
        begin
          fldrpart := copy(fldr, 1, pos('/', fldr) - 1);
          delete(fldr, 1, pos('/', fldr));
        end
        else
        begin
          fldrpart := fldr;
          fldr := '';
        end;

        if (fldrpart <> '') then
        begin
          if not FtpSetCurrentDirectory(hintconnect,PChar(fldrpart)) then
          begin
            if not FtpCreateDirectory(hintconnect,PChar(fldrpart)) then
            begin
              if not KeepConnect then
              begin
                InternetCloseHandle(hintconnect);
                FPrevConnect := '';
              end;
              Exit;
            end
            else
              FtpSetCurrentDirectory(hintconnect,PChar(fldrpart));
          end;
        end;
      until (fldr = '');
    end;
  end;

  (*
  // change to directory / create it if used
  if (tgtdir <> '') and (tgtdir <> FPrevTarget) then
  begin
    if KeepConnect then
      FPrevTarget := tgtdir
    else
      FPrevTarget := '';

    if not FtpSetCurrentDirectory(hintconnect,PChar(tgtdir)) then
    begin
      if not FtpCreateDirectory(hintconnect,PChar(tgtdir)) then
      begin
        if not KeepConnect then
        begin
          InternetCloseHandle(hintconnect);
          FPrevConnect := '';
        end;
        Exit;
      end
      else
        FtpSetCurrentDirectory(hintconnect,PChar(tgtdir));
    end;
  end;
  *)

  Application.ProcessMessages;

  fn := URL;

  if tgtfn = '' then
    tgtfn := URLToFile(url);

  if FindFirst(fn,faAnyFile, SR) = 0 then
  begin
    repeat
      ufn := ExtractFileName(SR.Name);

      FTPPutFile(idx, FUserID, FPassword, FHost, FPort, ExtractFilePath(fn) + ufn, TgtDir, ufn, UseDate, FileDate, NoNew, true);

      if Assigned(OnMultiFTPFileDone) then
        OnMultiFTPFileDone(Self, SR.Name);

    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

  InternetCloseHandle(hintconnect);
  FPrevConnect := '';

  Result := True;
end;

//-------------------
// Put files to FTP
//-------------------
function TWebCopy.FtpPutFile(idx: Integer; FUserid,FPassword,FHost:string;FPort: Integer;
  URL, TgtDir,TgtFn: string; UseDate: Boolean;var FileDate: TDateTime; var NoNew: Boolean; KeepConnect: Boolean): Boolean;
var
  hintconnect: hinternet;
  hintfile: hinternet;
  ftpflag: Integer;
  buf: array[0..READBUFFERSIZE-1] of char;
  bufsize,numread: DWord;
  lf: file;
  fn: string;
  tck: DWord;
  FSize,lpdword: int64;
  ffi: TWin32FindData;
  FTime: TDateTime;
  Allow: Boolean;
  fldr: string;
  fldrpart: string;
  chdir: boolean;
  {$IFNDEF DELPHI_UNICODE}
  origdir: array[0..255] of char;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  origdir: String;
  {$ENDIF}
  dirs: DWORD;
  fm: word;
  ftpmodeflag: DWORD;
  openerr: boolean;

label
  QuitProc;

begin
  Log('start ftp upload');
  NoNew := False;
  Result := False;
  FAnim.ResID := 258;
  FAnim.ResHandle := hinstance;
  FAnim.Active  := True;
  FCancelled := False;


  if FItems.Count>1 then
    FFileLbl.Caption := IntToStr(FFilenum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + URLToFile(url) + ' ' + FToServerLabel+' '+Fhost;

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+fHost));
  outputdebugstring(pchar('userid:'+fUserID));
  outputdebugstring(pchar('pwd:'+fPassword));
  {$ENDIF}

  Application.ProcessMessages;
  if (FPrevConnect = FHost + FUserID + FPassword) then
  begin
    hintconnect := FFTPConnect;
  end
  else
  begin
    FPrevTarget := '';

    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    FFTPConnect := hintconnect;
    FPrevConnect := FHost + FUserID + FPassword;
  end;

  Log('connected to server:'+fhost);

  Application.ProcessMessages;

  chdir := false;

  // change to directory / create it if used
  if (tgtdir <> '') and (tgtdir <> FPrevTarget) then
  begin
    if KeepConnect then
      FPrevTarget := tgtdir
    else
      FPrevTarget := '';

    dirs := 255;

    {$IFNDEF DELPHI_UNICODE}
    FtpGetCurrentDirectory(hintconnect, origdir, dirs);
    {$ENDIF}
    {$IFDEF DELPHI_UNICODE}
    SetLength(origdir, dirs);
    FtpGetCurrentDirectory(hintconnect, PChar(origdir), dirs);
    {$ENDIF}

    //chdir := tgtdir <> '';

    if not FtpSetCurrentDirectory(hintconnect,PChar(tgtdir)) then
    begin  // cannot set the full targetdir, split and try to set subfolder by subfolder
      fldr := ForwardSlashes(tgtdir);
      FPrevTarget := '';

      repeat
        if pos('/', fldr) > 0 then
        begin
          fldrpart := copy(fldr, 1, pos('/', fldr) - 1);
          delete(fldr, 1, pos('/', fldr));
        end
        else
        begin
          fldrpart := fldr;
          fldr := '';
        end;

        if (fldrpart <> '') then
        begin
          if not FtpSetCurrentDirectory(hintconnect,PChar(fldrpart)) then
          begin
            if not FtpCreateDirectory(hintconnect,PChar(fldrpart)) then
            begin
              if not KeepConnect then
              begin
                InternetCloseHandle(hintconnect);
                FPrevConnect := '';
              end;
              Exit;
            end
            else
              FtpSetCurrentDirectory(hintconnect,PChar(fldrpart));
          end;
        end;
      until (fldr = '');
    end;
  end;

  Application.ProcessMessages;

  //fn:=URLtoFile(url);
  fn := URL;

  if tgtfn = '' then
    tgtfn := URLToFile(url);

  if UseDate or Assigned(FOnFileDateCheck) then
  begin
    if FtpFindFirstFile(hintconnect,PChar(tgtfn),ffi,0,0) <> nil then
  begin
      Allow := true;
      if Assigned(FOnCopyOverwrite) then
        FOnCopyOverwrite(Self,tgtfn,Allow);

      if not Allow then
        goto QuitProc;

      FileTimeToDateTime(ffi.ftLastWriteTime,FTime);

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar(formatdatetime('dd/mm/yyyy hh:nn:ss',ftime)));
      {$ENDIF}
      if Assigned(FOnFileDateCheck) then
      begin
        Allow := True;
        FOnFileDateCheck(Self,idx,FTime,Allow);
        FileDate := FTime;
        if not Allow then
          goto QuitProc;
      end
      else
        if FTime > FileDate then
        begin
          FileDate := FTime;
          NoNew := True;
          if Assigned(FOnNoNewFile) then
            FOnNoNewFile(Self,tgtfn,FileDate,FTime);
          goto QuitProc;
        end;
      FileDate := FTime;
    end;
  end;

  if FTPMode = fmBINARY then
    ftpmodeflag := FTP_TRANSFER_TYPE_BINARY
  else
    ftpmodeflag := FTP_TRANSFER_TYPE_ASCII;


  hintfile := FtpOpenFile(hintconnect,PChar(tgtfn),GENERIC_WRITE,ftpmodeflag,0);

  if hintfile = nil then
  begin
    if Assigned(FOnURLNotFound) then
      FOnURLNotFound(self,url);
    if not KeepConnect then
    begin
      InternetCloseHandle(hintconnect);
      FPrevConnect := '';
    end;
    Exit;
  end;

  Application.ProcessMessages;

  FLastFile := fn;
  FLastDir := tgtdir;

  openerr := false;

  // check if source file exists
  if not FileExists(fn) then
    openerr := true;

  // try to open source file non exclusively
  fm := FileMode;
  FileMode := 0;
  AssignFile(lf,fn);
  {$i-}
  Reset(lf,1);
  {$i+}
  if IOResult <> 0 then
    openerr := true;

  // file not found or failure to open source file
  if openerr then
  begin
    FileMode := fm;

    InternetCloseHandle(hintfile);
    if not KeepConnect then
    begin
      InternetCloseHandle(hintconnect);
      FPrevConnect := '';
    end;

    if Assigned(FOnURLNotFound) then
      FOnURLNotFound(self,fn);

    if Assigned(FOnError) then
      FOnError(Self,errCannotOpenSourceFile);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotOpenSourceFile,'Cannot open source file ' + fn);
    Exit;
  end;

  lpdword := GetFileSize(fn);

  bufsize := READBUFFERSIZE;
  FSize := 0;
  tck := GetTickCount;

  log('start to upload file:'+fn);
  log('filesize:'+inttostr(lpdword));

  while (bufsize > 0) and not FCancelled  do
  begin
    Application.ProcessMessages;
    BlockRead(lf,buf,bufsize,numread);
    if not InternetWriteFile(hintfile,@buf,numread,bufsize) then
      Break;
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar('write from ftp = '+IntToStr(bufsize)));
    {$ENDIF}

    FSize := FSize + bufsize;

    log('uploaded:'+inttostr(fsize));

    if (lpdword > 0) then
    begin
      FProgress.Position := Round(100 * (FSize/lpdword));
      FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize) + ' ' +DlgFileOfLabel + ' ' + FileSizeFmt(lpdword);
    end
    else
      FSizeLbl.Caption := FProgressLabel + ' ' + FileSizeFmt(FSize);


    if FShowCompletion then
      FForm.Caption := DlgUpload + ' : ' + IntToStr(FProgress.Position) + '% ' + DlgCompleted;

    FRateStr := FileSizeFmtSpeed(Round(FSize/((GetTickCount-tck)+1)*1000));
    FRateLbl.Caption := FRateLabel + ' ' + FRateStr;

    FTimeLeftStr := TimeFmt(GetTickCount-tck,FSize,lpdword, FDlgTimeSec, FDlgTimeMin);
    FTimeLbl.Caption := FTimeLabel + ' ' + FTimeLeftStr;

    FElapsedTimeStr := TimeFmt(GetTickCount-tck,0,lpdword, FDlgTimeSec, FDlgTimeMin);
    FElapsedTimeLbl.Caption := FElapsedTimeLabel + ' ' + FElapsedTimeStr;

    if Assigned(FOnCopyProgress) then
      FOnCopyProgress(Self,ffilenum,fsize,lpdword);

    Application.ProcessMessages;
  end;
  CloseFile(lf);
  InternetCloseHandle(hintfile);

  log('done upload:'+inttostr(fsize));

  FileMode := fm;


  Result := True;

QuitProc:
  if FCancelled then
    KeepConnect := false;

  if not KeepConnect then
  begin
    InternetCloseHandle(hintconnect);
    FPrevConnect := '';
  end
  else
  begin
    if chdir then
    begin
      if OrigDir = '' then
        OrigDir := '/';

      {$IFDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,PChar(OrigDir));
      {$ENDIF}
      {$IFNDEF DELPHI_UNICODE}
      FtpSetCurrentDirectory(hintconnect,OrigDir);
      {$ENDIF}
    end;

  end;

  Log('done ftp upload');
end;


function TWebCopy.FtpTest(idx: Integer; FUserid,FPassword,FHost:string;FPort: Integer): Boolean;
var
  hintconnect: hinternet;
  ftpflag: Integer;

begin
  Result := False;
  FAnim.ResID := 258;
  FAnim.ResHandle := hinstance;
  FAnim.Active  := True;

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+fHost));
  outputdebugstring(pchar('userid:'+fUserID));
  outputdebugstring(pchar('pwd:'+fPassword));
  {$ENDIF}

  Application.ProcessMessages;
  if (FPrevConnect = FHost + FUserID + FPassword) then
  begin
    hintconnect := FFTPConnect;
  end
  else
  begin
    FPrevTarget := '';
    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end
  else
  begin
    Result := true;
    InternetCloseHandle(hintconnect);
  end;
end;


//-------------------
// Remove files in FTP
//-------------------
function TWebCopy.FtpRemoveFile(idx: Integer; FUserid,FPassword,FHost:string;FPort: Integer;
  URL, TgtDir,TgtFn: string; KeepConnect: Boolean): Boolean;
var
  hintconnect: hinternet;
  ftpflag: Integer;
  fn: string;

begin
  Result := False;
  FAnim.ResID := 258;
  FAnim.ResHandle := hinstance;
  FAnim.Active  := True;

  if FItems.Count>1 then
    FFileLbl.Caption := IntToStr(FFilenum) + ' ' + FFileOfLabel + ' ' + IntToStr(FItems.ActiveItems) + ' : '
  else
    FFileLbl.Caption := '';

  FFileLbl.Caption := FFileLabel + ' ' + FFileLbl.Caption + URLToFile(url) + ' ' + FToServerLabel+' '+Fhost;

  if FFTPPassive then
    ftpflag := INTERNET_FLAG_PASSIVE
  else
    ftpflag := 0;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('host:'+fHost));
  outputdebugstring(pchar('userid:'+fUserID));
  outputdebugstring(pchar('pwd:'+fPassword));
  {$ENDIF}

  Application.ProcessMessages;
  if (FPrevConnect = FHost + FUserID + FPassword) then
  begin
    hintconnect := FFTPConnect;
  end
  else
  begin
    FPrevTarget := '';
    if (FUserID = '') or (FPassword = '') then
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,nil,nil,INTERNET_SERVICE_FTP,ftpflag,0)
    else
      hintconnect := InternetConnect(fhinternet,PChar(FHost),FPort,PChar(FUserID),PChar(FPassword),INTERNET_SERVICE_FTP,ftpflag,0);
  end;

  if hintconnect = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Self,errCannotConnect);
    if Assigned(FOnErrorInfo) then
      FOnErrorInfo(Self,errCannotConnect,'Cannot connect to FTP server ' + FHost);
    Exit;
  end;

  if KeepConnect then
  begin
    FFTPConnect := hintconnect;
    FPrevConnect := FHost + FUserID + FPassword;
  end;

  Application.ProcessMessages;

  // change to directory
  if (tgtdir <> '') and (tgtdir <> FPrevTarget) then
  begin
    if KeepConnect then
      FPrevTarget := tgtdir
    else
      FPrevTarget := '';

    // directory doesn't exist so can't remove the file
    if not FtpSetCurrentDirectory(hintconnect,PChar(tgtdir)) then
      exit;
  end;

    Application.ProcessMessages;

  //fn:=URLtoFile(url);
  fn := URL;

  if tgtfn = '' then
    tgtfn := URLToFile(url);

  if FtpDeleteFile(hintconnect, PChar(tgtfn)) then
    begin
    if Assigned(FonRemovedFile) then
      FonRemovedFile(Self, idx);
  end
  else
  begin
    if Assigned(FOnRemovedFileFailed) then
      FOnRemovedFileFailed(Self, idx);
  end;

  Application.ProcessMessages;

  if not KeepConnect then
  begin
    InternetCloseHandle(hintconnect);
    FPrevConnect := '';
  end;
  Result := True;
end;

constructor TWebCopy.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FItems := TWebCopyItems.Create(self);
  FFileLabel := 'File :';
  FProgressLabel := 'Progress :';
  FRateLabel := 'Transfer rate :';
  FTimeLabel := 'Estimated time left :';
  FElapsedTimeLabel := 'Elapsed time :';
  FDlgCaption := 'Downloading';
  FDlgUpload := 'Uploading';
  FDlgDwnload := 'Downloading';
  FDlgCopying := 'Copying';
  FDlgCompleted := 'completed';
  FShowDialog := True;
  FFromServerLabel := 'from';
  FToServerLabel := 'to';
  FFileOfLabel := 'of';
  FDlgCancel := 'Cancel';
  FDlgClose := 'Close';
  FDlgOpenFile := 'Open file';
  FDlgOpenFolder := 'Open folder';
  FDlgTimeSec := 'sec';
  FDlgTimeMin := 'minutes';
  FShowServer := True;
  FShowFileName := True;
  FShowDialogOnTop := True;
  FDlgShowCancel := True;
  FDlgShowCloseBtn := False;
  FAutoURLDecode := False;
  FAgent := 'WebCopy';
  FFTPMode := fmBINARY;
end;

destructor TWebCopy.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TWebCopy.DoCopy;
var
  i: Integer;
  ok: Boolean;
  NoNew: Boolean;
  FDate: TDateTime;
  KeepConn: Boolean;
  dwTimeOut: DWORD;

begin
  CreateForm;

  fhinternet := nil;

  if NumInetItems > 0 then
  begin
    if FProxy = '' then
      fhinternet := InternetOpen(PChar(Agent),INTERNET_OPEN_TYPE_PRECONFIG {or INTERNET_FLAG_ASYNC},nil,nil,INTERNET_FLAG_NO_CACHE_WRITE)
    else
      fhinternet := InternetOpen(PChar(Agent),INTERNET_OPEN_TYPE_PROXY {or INTERNET_FLAG_ASYNC},PChar(FProxy),nil, INTERNET_FLAG_NO_CACHE_WRITE);

    if FTimeOut > 0 then
    begin
      dwTimeout := FTimeOut * 1000;
      InternetSetOption(fhinternet, INTERNET_OPTION_CONNECT_TIMEOUT, @dwTimeOut, SizeOf(dwTimeOut));
      InternetSetOption(fhinternet, INTERNET_OPTION_SEND_TIMEOUT, @dwTimeOut, SizeOf(dwTimeOut));
      InternetSetOption(fhinternet, INTERNET_OPTION_RECEIVE_TIMEOUT, @dwTimeOut, SizeOf(dwTimeOut));
    end;
  end;

  FFileNum := 0;
  FPrevTarget := '';

  for i := 1 to FItems.Count do
  begin
    if FItems.Items[i - 1].Active then
    begin
      Inc(FFileNum);
      if Assigned(FOnFileStart) then
        FOnFileStart(Self,i - 1);

      with FItems.Items[i - 1] do
      begin
        ok := False;

        case Protocol of
        wpHttp,wpFtp,wpMultiFtp: FForm.Caption := FDlgDwnload + ' ...';
        wpFile: FForm.Caption := FDlgCopying + ' ...';
        wpHttpUpload,wpFtpUpload,wpMultiFtpUpload: FForm.Caption := FDlgUpload + ' ...';
        end;

        FDate := FileDate;

        KeepConn := false;

        if (i <= FItems.Count - 1) and (FItems.Count > 1) then
        begin
          if (FItems.Items[i].Protocol in [wpFtp, wpFtpUpload]) and (Protocol in [wpFtp, wpFtpUpload]) then
          begin
            KeepConn := (FTPHost + FTPUserID + FTPPassword = FItems.Items[i].FTPHost + FItems.Items[i].FTPUserID + FItems.Items[i].FTPPassword);
          end;
        end;

        case Protocol of
        wpHttp:
          ok := HttpGetFile(i - 1,url,FItems.Items[i - 1].DisplayUrl,targetdir,targetfilename,CopyNewerOnly,FDate,NoNew, Authenticate, HTTPUserID, HTTPPassword);
        wpHttpUpload:
          ok := HttpPutFile(url,targetdir,targetfilename,HTTPCommand);
        wpFile:
          ok := FileGetFile(i - 1,FTPUserID, FTPPassword, FTPHost, FTPPort, URL, Targetdir, TargetFilename, CopyNewerOnly, FDate, NoNew);
        wpFtp:
          ok := FtpGetFile(i - 1,FTPUserID, FTPPassword, FTPHost, FTPPort, URL, TargetDir, TargetFileName, CopyNewerOnly, FDate, NoNew, KeepConn, true);
        wpFtpUpload:
          ok := FtpPutFile(i - 1,FTPUserID, FTPPassword, FTPHost, FTPPort, URL, TargetDir,TargetFileName,CopyNewerOnly,FDate, NoNew, false);
        wpMultiFtp:
          ok := MultiFtpGetFile(i - 1,FTPUserID,FTPPassword,FTPHost,FTPPort,URL,TargetDir,TargetFileName,CopyNewerOnly,FDate, NoNew, KeepConn);
        wpFtpList:
          ok := MultiFtpGetList(i - 1,FTPUserID,FTPPassword,FTPHost,FTPPort,URL,TargetDir,TargetFileName,CopyNewerOnly,FDate, NoNew, KeepConn);
        wpMultiFtpUpload:
          ok := MultiFtpPutFile(i - 1,FTPUserID,FTPPassword,FTPHost,FTPPort,URL,TargetDir,TargetFileName,CopyNewerOnly,FDate, NoNew, KeepConn);
        wpFtpDelete:
          ok := FtpRemoveFile(i - 1,FTPUserID,FTPPassword,FTPHost,FTPPort,URL,TargetDir,TargetFileName,KeepConn);
        wpFtpTest:
          ok := FtpTest(i - 1,FTPUserID,FTPPassword,FTPHost,FTPPort);
        end;
      end;

      FItems.Items[i - 1].NewFileDate := FDate;
      FItems.Items[i - 1].FSuccess := ok and not FCancelled;
      FItems.Items[i - 1].FCancelled := FCancelled;
      FItems.Items[i - 1].FNoNewVersion := NoNew;

      if FCancelled then
        Break
      else
        if Assigned(FOnFileDone) then
          FOnFileDone(Self,i - 1);
    end;
  end;

  if Assigned(fhinternet) then
    InternetCloseHandle(fhinternet);

  if FShowOpenFile or FShowOpenFolder then
  begin
    if Assigned(FOnCopyDone) then
      FOnCopyDone(Self);

    FCancelBtn.Caption := FDlgClose;
    FCancelIsClose := true;
    FAnim.Active := False;
    if not FCancelled and FShowOpenFile then
      FBtnShowFile.Enabled := True;
    if not FCancelled and FShowOpenFolder then
      FBtnShowFolder.Enabled := True;
  end
  else
  begin
    FCancelIsClose := true;
    FForm.Close;
    Application.ProcessMessages;
    if Assigned(FOnCopyDone) then
      FOnCopyDone(Self);
  end;
end;


procedure TWebCopy.CancelCopy;
begin
  FCancelled := True;
end;

procedure TWebCopy.CreateForm;
var
  BtnTop: Integer;
  capdy: Integer;
  LblTop: Integer;
  delta: integer;
begin
  FForm := TForm.Create(self);
  FForm.Width := 300;

  capdy := GetSystemMetrics(SM_CYCAPTION);

  delta := 0;

  if FShowTime then
    delta := 20;

  if FShowElapsedTime then
    delta := delta + 20;

  FForm.Height := 188 + delta + capdy;
  if ShowFileName then
    BtnTop := 140 + delta
  else
    BtnTop := 120 + delta;


  FForm.Position := poScreenCenter;
  FForm.Borderstyle := bsDialog;
  FForm.OnCloseQuery := FormCloseQuery;
  FForm.OnClose := FormClose;

  if FShowDialogOnTop then
    FForm.FormStyle := fsStayOnTop;

  FFileLbl := TLabel.Create(FForm);
  FFileLbl.Parent := FForm;

  LblTop := 80;
  FFileLbl.Visible := ShowFileName;

  FFileLbl.Top := LblTop;
  FFileLbl.Left := 10;
  FFileLbl.Width := FForm.Width - 20;
  FFileLbl.Autosize := False;
  FFileLbl.Caption := FFileLabel;

  if not ShowFileName then
    LblTop := 60;

  FSizeLbl := TLabel.Create(FForm);
  FSizeLbl.Parent := FForm;
  FSizeLbl.Top := LblTop + 20;
  FSizeLbl.Left := 10;
  FSizeLbl.Caption := FProgressLabel;

  FRateLbl := TLabel.Create(FForm);
  FRateLbl.Parent := FForm;
  FRateLbl.Top := LblTop + 40;
  FRateLbl.Left := 10;
  FRateLbl.Caption := FRateLabel;

  FTimeLbl := TLabel.Create(FForm);
  FTimeLbl.Parent := FForm;
  FTimeLbl.Top := LblTop + 60;
  FTimeLbl.Left := 10;
  FTimeLbl.Caption := FTimeLabel;
  FTimeLbl.Visible := FShowTime;

  FElapsedTimeLbl := TLabel.Create(FForm);
  FElapsedTimeLbl.Parent := FForm;
  FElapsedTimeLbl.Top := LblTop + 80;
  FElapsedTimeLbl.Left := 10;
  FElapsedTimeLbl.Caption := FElapsedTimeLabel;
  FElapsedTimeLbl.Visible := FShowElapsedTime;

  FAnim := TAnimate.Create(FForm);
  FAnim.Parent := FForm;
  FAnim.Left := 10;
  FAnim.Top := 0;

  try
    FAnim.ResID := 257;
  except
  end;

  FAnim.ResHandle := hinstance;

  FAnim.Active  := True;

  FCancelBtn := TButton.Create(FForm);
  FCancelBtn.Parent := FForm;
  FCancelBtn.Top := BtnTop;
  FCancelBtn.Left := FForm.Width - FCancelbtn.width - 10;
  FCancelBtn.Caption := DlgCancel;
  FCancelBtn.OnClick  := CancelClick;
  FCancelBtn.Visible := DlgShowCancel;

  FCancelIsClose := False;

  if not DlgShowCancel then
    FForm.Height := FForm.Height - 20;

  if FShowOpenFolder then
  begin
    FBtnshowfolder := TButton.Create(FForm);
    FBtnshowfolder.Parent := FForm;
    FBtnshowfolder.Top := BtnTop;
    FBtnshowfolder.Left := FForm.Width - FCancelBtn.Width - FCancelBtn.Width - 10 - 10;
    FBtnshowfolder.Caption := FDlgOpenFolder;
    FBtnshowfolder.OnClick  := OpenFolder;
    FBtnshowfolder.Enabled := False;
  end;

  if FShowOpenFile then
  begin
    FBtnshowfile := TButton.Create(FForm);
    FBtnshowfile.Parent := FForm;
    FBtnshowfile.Top := BtnTop;
    FBtnshowfile.Left := FForm.Width - FCancelBtn.Width - FCancelBtn.Width - 10 - 10;
    if FShowOpenFolder then
      FBtnshowfile.left := FBtnshowfile.Left - FCancelBtn.Width - 10;
    FBtnshowfile.Caption := FDlgOpenFile;
    FBtnshowfile.OnClick  := OpenFile;
    FBtnshowfile.Enabled := False;
  end;

  FProgress := TProgressBar.Create(FForm);
  FProgress.Parent := FForm;
  FProgress.Top := 60;
  FProgress.Left := 10;
  FProgress.Height := 12;
  FProgress.Width := FForm.Width - 20;

  FFormClosed := false;

  if not DlgShowCloseBtn then
    FForm.BorderIcons := [];

  FForm.Caption := FDlgCaption + ' ...';
  FCancelled := False;

  if FShowDialog then
  begin
    if Assigned(OnBeforeDialogShow) then
      OnBeforeDialogShow(Self, FForm);

    FForm.Show;
    if FAlwaysOnTop then
      SetWindowPos(FForm.Handle,HWND_TOPMOST,FForm.Left,FForm.top,FForm.Width,FForm.height,0);
  end;

  FCancelled := False;
end;

procedure TWebCopy.Execute;
begin
  DoCopy;
end;


procedure TWebCopy.ThreadExecute;
begin
  with TWCopyThread.Create(Self) do
    OnTerminate := ThreadDone;
end;

procedure TWebCopy.ThreadDone(Sender:TObject);
begin
  FCopyCompleted := True;
end;

procedure TWebCopy.ThreadExecAndWait;
begin
   FCopyCompleted := False;
   ThreadExecute;
   while not FCopyCompleted do
     Application.ProcessMessages;
end;

procedure TWebCopy.OpenFile(sender: tobject);
begin
  ShellExecute(0,'open',PChar(FLastFile),nil,nil, SW_NORMAL);
  FForm.Close;
  FFormClosed := True;
end;

procedure TWebCopy.OpenFolder(sender: tobject);
begin
  ShellExecute(0,'open',PChar(FLastDir),nil,nil, SW_NORMAL);
  FForm.Close;
  FFormClosed:=true;
end;

function TWebCopy.NumInetItems: integer;
var
  i,j: Integer;
begin
  j := 0;
  for i := 1 to Items.Count do
  begin
    if Items.Items[i - 1].Active and (Items.Items[i - 1].Protocol <> wpFile) then
      Inc(j);
  end;
  result:=j;
end;

function TWebCopy.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TWebCopy.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TWebCopy.SetVersion(const Value: string);
begin

end;

{ TWebCopyItems }

function TWebCopyItems.Add: TWebCopyItem;
begin
  Result := TWebCopyItem(inherited Add);
end;

constructor TWebCopyItems.Create(aOwner:TComponent);
begin
  inherited Create(TWebCopyItem);
  FOwner := AOwner;
end;

function TWebCopyItems.GetActiveItems: integer;
var
  i,r: Integer;

begin
  r := 0;
  for i := 1 to Count do
  begin
    if Items[i - 1].Active then
      Inc(r);
  end;
  Result := r;
end;

function TWebCopyItems.GetItem(Index: Integer): TWebCopyItem;
begin
  Result := TWebCopyItem(inherited GetItem(Index));
end;

function TWebCopyItems.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TWebCopyItems.GetSuccessCount: Integer;
var
  i,r: Integer;
begin
  r := 0;
  for i := 1 to Count do
  begin
    if Items[i - 1].Success then
      Inc(r);
  end;
  Result := r;
end;

function TWebCopyItems.Insert(index: integer): TWebCopyItem;
begin
  {$IFDEF VER100}
  Result := TWebCopyItem(inherited Add);
  {$ELSE}
  Result := TWebCopyItem(inherited Insert(Index));
  {$ENDIF}
end;

procedure TWebCopyItems.SetItem(Index: Integer; Value: TWebCopyItem);
begin
  inherited SetItem(Index, Value);
end;

{ TWCopyThread }

constructor TWCopyThread.Create(AWebCopy: TWebCopy);
begin
  WebCopy := AWebCopy;
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TWCopyThread.Execute;
begin
  Synchronize(WebCopy.DoCopy);
end;

{ TWebCopyItem }

procedure TWebCopyItem.Assign(Source: TPersistent);
begin
  if (Source is TWebCopyItem) then
  begin
    FActive := (Source as TWebCopyItem).Active;
    FAuthenticate := (Source as TWebCopyItem).Authenticate;
    FCopyNewerOnly := (Source as TWebCopyItem).CopyNewerOnly;
    FDisplayURL := (Source as TWebCopyItem).DisplayURL;
    FFileDate := (Source as TWebCopyItem).FileDate;
    FFTPHost := (Source as TWebCopyItem).FTPHost;
    FFTPUserID := (Source as TWebCopyItem).FTPUserID;
    FFTPPassword := (Source as TWebCopyItem).FTPPassword;
    FFTPPort := (Source as TWebCopyItem).FTPPort;
    FHTTPCommand := (Source as TWebCopyItem).HTTPCommand;
    FProtocol := (Source as TWebCopyItem).Protocol;
    FTargetDir := (Source as TWebCopyItem).TargetDir;
    FTargetFilename := (Source as TWebCopyItem).TargetFilename;
    FURL := (Source as TWebCopyItem).URL;
    FHTTPUserID := (Source as TWebCopyItem).HTTPUserID;
    FHTTPPassword := (Source as TWebCopyItem).HTTPPassword;
  end;
end;

constructor TWebCopyItem.Create(Collection: TCollection);
begin
  inherited;
  FAuthenticate := waNever;
  FActive := True;
  FFTPPort := 21;
  FFileList := nil;
  FFileDetailList := nil;
  FFileSize := 0;
end;

destructor TWebCopyItem.Destroy;
begin
  if Assigned(FFileList) then
    FFileList.Free;
  if Assigned(FFileDetailList) then
    FFileDetailList.Free;

  inherited;
end;

{ TFileDetailItems }

function TFileDetailItems.Add: TFileDetailItem;
begin
  Result := TFileDetailItem(inherited Add);
end;

constructor TFileDetailItems.Create;
begin
  inherited Create(TFileDetailItem);
end;

function TFileDetailItems.GetItem(Index: Integer): TFileDetailItem;
begin
  Result := TFileDetailItem(inherited Items[Index]);
end;

function TFileDetailItems.Insert(Index: Integer): TFileDetailItem;
begin
  Result := TFileDetailItem(inherited Insert(Index));
end;

procedure TFileDetailItems.SetItem(Index: Integer;
  const Value: TFileDetailItem);
begin
  inherited Items[Index] := Value;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
