{*******************************************************}
{                                                       }
{           System Information Component                }
{           Disk Change Notify Component                }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_DiskMonitor;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}
     {$ELSE}
     Windows, SysUtils, Classes, Messages,{$IFDEF TRIAL}Dialogs,{$ENDIF}
     {$ENDIF}
     MSI_Defs;

const
  FILE_LIST_DIRECTORY   = $0001;

  {$IFDEF FPC}
  FILE_SHARE_READ                     = $00000001;
  FILE_SHARE_WRITE                    = $00000002;
  FILE_SHARE_DELETE                   = $00000004;
  FILE_ATTRIBUTE_READONLY             = $00000001;
  FILE_ATTRIBUTE_HIDDEN               = $00000002;
  FILE_ATTRIBUTE_SYSTEM               = $00000004;
  FILE_ATTRIBUTE_DIRECTORY            = $00000010;
  FILE_ATTRIBUTE_ARCHIVE              = $00000020;
  FILE_ATTRIBUTE_DEVICE               = $00000040;
  FILE_ATTRIBUTE_NORMAL               = $00000080;
  FILE_ATTRIBUTE_TEMPORARY            = $00000100;
  FILE_ATTRIBUTE_SPARSE_FILE          = $00000200;
  FILE_ATTRIBUTE_REPARSE_POINT        = $00000400;
  FILE_ATTRIBUTE_COMPRESSED           = $00000800;
  FILE_ATTRIBUTE_OFFLINE              = $00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = $00002000;
  FILE_ATTRIBUTE_ENCRYPTED            = $00004000;
  FILE_ATTRIBUTE_VIRTUAL              = $00010000;
  INVALID_FILE_ATTRIBUTES             = DWORD($FFFFFFFF);
  FILE_NOTIFY_CHANGE_FILE_NAME        = $00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME         = $00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES       = $00000004;
  FILE_NOTIFY_CHANGE_SIZE             = $00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE       = $00000010;
  FILE_NOTIFY_CHANGE_LAST_ACCESS      = $00000020;
  FILE_NOTIFY_CHANGE_CREATION         = $00000040;
  FILE_NOTIFY_CHANGE_SECURITY         = $00000100;
  FILE_ACTION_ADDED                   = $00000001;
  FILE_ACTION_REMOVED                 = $00000002;
  FILE_ACTION_MODIFIED                = $00000003;
  FILE_ACTION_RENAMED_OLD_NAME        = $00000004;
  FILE_ACTION_RENAMED_NEW_NAME        = $00000005;
  MAILSLOT_NO_MESSAGE                 = LongWord(-1);
  MAILSLOT_WAIT_FOREVER               = LongWord(-1);
  FILE_CASE_SENSITIVE_SEARCH          = $00000001;
  FILE_CASE_PRESERVED_NAMES           = $00000002;
  FILE_UNICODE_ON_DISK                = $00000004;
  FILE_PERSISTENT_ACLS                = $00000008;
  FILE_FILE_COMPRESSION               = $00000010;
  FILE_VOLUME_QUOTAS                  = $00000020;
  FILE_SUPPORTS_SPARSE_FILES          = $00000040;
  FILE_SUPPORTS_REPARSE_POINTS        = $00000080;
  FILE_SUPPORTS_REMOTE_STORAGE        = $00000100;
  FILE_VOLUME_IS_COMPRESSED           = $00008000;
  FILE_SUPPORTS_OBJECT_IDS            = $00010000;
  FILE_SUPPORTS_ENCRYPTION            = $00020000;
  FILE_NAMED_STREAMS                  = $00040000;
  FILE_READ_ONLY_VOLUME               = $00080000;
  FILE_SEQUENTIAL_WRITE_ONCE          = $00100000;
  FILE_SUPPORTS_TRANSACTIONS          = $00200000;
  FILE_SUPPORTS_HARD_LINKS            = $00400000;
  FILE_SUPPORTS_EXTENDED_ATTRIBUTES   = $00800000;
  FILE_SUPPORTS_OPEN_BY_FILE_ID       = $01000000;
  FILE_SUPPORTS_USN_JOURNAL           = $02000000;
  {$ENDIF}

type
  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: Cardinal;
    Action: Cardinal;
    FileNameLength: Cardinal;
    FileName: array[0..0] of WideChar;
  end;

  TWatchFilter = (wfFilename, wfDirname, wfAttrs, wfSize, wfLastWrite, wfLastAccess, wfCreation, wfSecurity);
  TWatchFilterSet = set of TWatchFilter;

  TWatchAction=(waAdd, waRemove, waModify, waRenameOld, waRenameNew);

  TWatchActionSet = set of TWatchAction;

  TEventChange = procedure(Sender: TObject; Action: TWatchAction; FileName: string) of object;

  TWatchThread = class(TThread)
  private
    FOwner: TComponent;
    procedure SendEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TComponent);
  end;

  TMiTeC_DiskMonitor = class(TComponent)
  private
    FWFS: TWatchFilterSet;
    FWAS: TWatchActionSet;
    FDir: string;
    FWT: TWatchThread;
    FEventChange: TEventChange;
    FActive: Boolean;
    FWatchSubtree: Boolean;
    FCompletionPort: {$IFDEF RAD10PLUS}NativeUInt{$ELSE}Integer{$ENDIF};
    FOverlapped: TOverlapped;
    FPOverlapped: POverlapped;
    FBytesWrite: Cardinal;
    FNotificationBuffer: array[0..4096] of Byte;
    FHandle: THandle;
    FFilter: Cardinal;
    FUC: string;
    procedure SetDir(const Value: string);
    procedure SetWAS(const Value: TWatchActionSet);
    procedure SetWatchSubtree(const Value: boolean);
    procedure SetWFS(const Value: TWatchFilterSet);
    procedure SetActive(Value: boolean);
  protected
    procedure DoChange;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: boolean read FActive write SetActive;
    property Directory: string read FDir write SetDir;
    property WatchSubtree: boolean read FWatchSubtree write SetWatchSubtree;
    property WatchFilter: TWatchFilterSet read FWFS write SetWFS;
    property WatchAction: TWatchActionSet read FWAS write SetWAS;
    property UserComment: string read FUC write FUC;
    property OnChange: TEventChange read FEventChange write FEventChange;
  end;

  {$IFDEF FPC}
  function ReadDirectoryChanges(hDirectory: THandle; lpBuffer: Pointer;
  nBufferLength: DWORD; bWatchSubtree: Bool; dwNotifyFilter: DWORD;
  lpBytesReturned: LPDWORD; lpOverlapped: POverlapped;
  lpCompletionRoutine: Pointer): BOOL; stdcall;
  {$ENDIF}

implementation

uses MSI_Common, MiTeC_StrUtils;

{$IFDEF FPC}
function ReadDirectoryChanges; external kernel32 name 'ReadDirectoryChangesW';
{$ENDIF}

{ TMiTeC_DiskMonitor }

constructor TMiTeC_DiskMonitor.Create(Owner: TComponent);
begin
  inherited;
  FPOverlapped:=@FOverlapped;
  FCompletionPort:=0;
  FDir:='c:\';
  FWatchSubtree:=True;
  FWFS:=[wfFilename,wfDirname,wfAttrs,wfSize,wfLastWrite,wfLastAccess,wfCreation,wfSecurity];
  FWAS:=[waAdd,waRemove,waModify,waRenameOld,waRenameNew];
  FWT:=nil;
  FActive:=False;
end;

destructor TMiTeC_DiskMonitor.Destroy;
begin
  Active:=False;
  if Assigned(FWT) then
    FWT.Terminate;
  inherited;
end;

procedure TMiTeC_DiskMonitor.DoChange;
var
  FileOpNotification: PFileNotifyInformation;
  Offset: Cardinal;
  ct: integer;
  s: string;
  wa: TWatchAction;
begin
  FileOpNotification:=@FNotificationBuffer[0];
  repeat
    Offset:=FileOpNotification^.NextEntryOffset;
    ct:=FileOpNotification^.Action;
    s:={$IFDEF FPC}WideToAnsi{$ENDIF}(WideCharLenToString(@(FileOpNotification^.FileName), FileOpNotification^.FileNameLength Div 2));
    PAnsiChar(FileOpNotification):=PAnsiChar(FileOpNotification)+Offset;
    wa:=waAdd;
    case ct of
      FILE_ACTION_ADDED: wa:=waAdd;
      FILE_ACTION_REMOVED: wa:=waRemove;
      FILE_ACTION_MODIFIED: wa:=waModify;
      FILE_ACTION_RENAMED_OLD_NAME: wa:=waRenameOld;
      FILE_ACTION_RENAMED_NEW_NAME: wa:=waRenameNew;
    end;
    if wa in FWAS then
      if Assigned(FEventChange) and (Length(s)>0) then
        FEventChange(Self,wa,s);
  until Offset=0;
end;

procedure TMiTeC_DiskMonitor.SetActive(Value: boolean);
begin
  if (FActive=Value) then
    Exit;
  FActive:=Value;
  if (csDesigning in self.ComponentState) then
    Exit;
  if FActive then begin
    {$IFDEF TRIAL}
    if not RunFromIDE then
      MessageDlg(Self.ClassName+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
    {$ENDIF}
    FFilter:=0;
    if wfFilename in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_FILE_NAME;
    if wfDirname in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_DIR_NAME;
    if wfAttrs in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_ATTRIBUTES;
    if wfSize in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_SIZE;
    if wfLastWrite in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_LAST_WRITE;
    if wfLastAccess in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_LAST_ACCESS;
    if wfCreation in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_CREATION;
    if wfSecurity in FWFS then
      FFilter:=FFilter or FILE_NOTIFY_CHANGE_SECURITY;
    FHandle:=CreateFile(PChar(FDir),FILE_LIST_DIRECTORY,FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,nil,OPEN_EXISTING,FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED,0);
    if Cardinal(FHandle)=INVALID_HANDLE_VALUE then
      Exit;
    FCompletionPort:=CreateIoCompletionPort(FHandle,0,Cardinal(Pointer(Self)),0);
    if FCompletionPort=0 then
      Exit;
    ZeroMemory(@FNotificationBuffer,SizeOf(FNotificationBuffer));
    if not ReadDirectoryChanges(FHandle,@FNotificationBuffer,SizeOf(FNotificationBuffer),FWatchSubtree,FFilter,@FBytesWrite,@FOverlapped,nil) then
      Exit;
    FWT:=TWatchThread.Create(self);
    FActive:=True;
  end else begin
    PostQueuedCompletionStatus(FCompletionPort,0,0,nil);
    CloseHandle(FCompletionPort);
    CloseHandle(FHandle);
    FWT.Terminate;
    FActive:=False;
  end;
end;

procedure TMiTeC_DiskMonitor.SetDir(const Value: string);
begin
  if FActive then
    raise Exception.Create('Cannot change property if component is active.');
  FDir:=Value;
end;

procedure TMiTeC_DiskMonitor.SetWAS(const Value: TWatchActionSet);
begin
  if FActive then
    raise Exception.Create('Cannot change property if component is active.');
  FWAS:=Value;
end;

procedure TMiTeC_DiskMonitor.SetWatchSubtree(const Value: boolean);
begin
  if FActive then
    raise Exception.Create('Cannot change property if component is active.');
  FWatchSubtree:=Value;
end;

procedure TMiTeC_DiskMonitor.SetWFS(const Value: TWatchFilterSet);
begin
  if FActive then
    raise Exception.Create('Cannot change property if component is active.');
  FWFS:=Value;
end;

{ TWatchThread }

constructor TWatchThread.Create;
begin
  inherited Create(False);
  FOwner:=Owner;
  FreeOnTerminate:=True;
  Priority:=tpTimeCritical;
end;

procedure TWatchThread.Execute;
var
  quit: boolean;
  p: TMiTeC_DiskMonitor;
  ck: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  n: Cardinal;
begin
  quit:=False;
  p:=TMiTeC_DiskMonitor(FOwner);
  while (not quit) do begin
    GetQueuedCompletionStatus(p.FCompletionPort,n,ck,p.FPOverlapped,INFINITE);
    if Terminated then
      quit:=True
    else if Assigned(p.FPOverlapped) then begin
      Synchronize(SendEvent);
      p.FBytesWrite:=0;
      ZeroMemory(@p.FNotificationBuffer,SizeOf(p.FNotificationBuffer));
      ReadDirectoryChanges(p.FHandle,@p.FNotificationBuffer,SizeOf(p.FNotificationBuffer),p.FWatchSubtree,p.FFilter,@p.FBytesWrite,@p.FOverlapped,nil);
    end;
  end;
end;

procedure TWatchThread.SendEvent;
begin
  TMiTeC_DiskMonitor(FOwner).DoChange;
end;

end.
