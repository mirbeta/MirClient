//  written by Philippe Wechsler 2008 - 2009
//                                                                                         
//  web: www.PhilippeWechsler.ch
//  mail: contact@PhilippeWechsler.ch
//
//  please see license.txt and documentation.pdf!
//
//  changes in 2.0
//   - major changes in code, 75% of the code is rewriten
//   - cleaner code
//   - AutoNextVolumeOnList
//   - ForceTestToEnd
//   - TOnRARFileProcessed
//   - TOnRARVolumeChanged
//   - support for wildchars (extract files)
//   - better unicode support and big files
//   - demo updated
//   - fixed some minor bugs
//
//  changes in 1.2 stable
//   - support for delphi 2009
//   - support for unicode filenames (see TRARFileItem.FileNameW)
//   - dll name + path is custom
//   - fixed a memory leak (thanks to Claes Enskär)
//   - some small improvements in the demo
//
//  changes in 1.1 stable
//   - fixed problem with mySelf pointer - you can use now multiple TRAR instances
//   - "SFX" in archive informations
//   - code better commented
//   - bugfixing in reading multivolumes
//
//  known bugs:
//   - Unicode support is only for d2009, but with older versions you can open, extract and
//     test archives.
//   - no support for Unicode archive names because unrar.dll does not support Unicode
//     filenames for multivolume parts! (workaround in 2.1)
//   - to extract only custom files you have to set the overall progress (ArchiveBytesTotal)
//     yourself. This is the sum of the uncompressed size of the selected files.
//     this issue will be fixed in 2.1

unit RAR;

interface

uses
  Classes, SysUtils, Windows, RAR_DLL, InIFiles, Masks;

type
  TRAROperation = (roLoadDLL, roOpenArchive, roCloseArchive, roListFiles, roExtract, roTest);

  //informations about the current progress
  TRARProgressInfo = record
    FileBytesDone: int64;
    FileBytesTotal: int64;
    FileName: WideString;
    ArchiveSizeDone: int64;
    ArchiveSizeTotal: int64;
  end;

  //informations about the current file
  TRARFileItem = record
    FileName: AnsiString;
    FileNameW: WideString;
    CompressedSize: int64;
    UnCompressedSize: int64;
    HostOS: AnsiString;
    CRC32: AnsiString;
    Attributes: Cardinal;
    Comment: AnsiString;
    Time: TDateTime;
    CompressionStrength: cardinal;
    ArchiverVersion: cardinal;
    Encrypted: boolean;
  end;

  //informations about existing or new item
  TRARReplaceData = record
    FileName: WideString;
    Size: int64;
    Time: TDateTime;
  end;

  //what to do when a file exists allready
  TRARReplaceAction = (rrCancel, rrOverwrite, rrSkip);

  TOnRARError = procedure(Sender: TObject; const ErrorCode: integer; const Operation: TRAROperation) of Object;
  TOnRARListFile = procedure(Sender: TObject; const FileInformation: TRARFileItem) of Object;
  TOnRARPasswordRequired = procedure(Sender: TObject; const HeaderPassword: boolean; const FileName: WideString; out NewPassword: AnsiString; out Cancel: boolean) of Object;
  TOnRARNextVolumeRequired = procedure(Sender: TObject; const requiredFileName: AnsiString; out newFileName: AnsiString; out Cancel: boolean) of Object;
  TOnRARProcess = procedure(Sender: TObject; const FileName: WideString; const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal, FileBytesDone: int64) of Object;
  TOnRARReplace = procedure(Sender: TObject; const ExistingData, NewData: TRARReplaceData; out Action: TRARReplaceAction) of object;
  TOnRARVolumeChanged = procedure(Sender: TObject; const NewVolumeName: AnsiString) of Object;
  TOnRARFileProcessed = procedure(Sender: TObject; const FileName: WideString; const Operation:TRAROperation; const Result:integer) of Object;

  //collected informations about the whole archive. These informations are only
  //correct if OpenFile() was successful
  TRARArchiveInformation = class(TPersistent)
  private
    fOpened: boolean;
    fFileName: AnsiString;
    fArchiverMajorVersion: Cardinal;
    fArchiverMinorVersion: Cardinal;
    fDictionarySize: int64;
    fEncryption: boolean;
    fSolid: boolean;
    fHostOS: AnsiString;
    fTotalFiles: integer;
    fCompressedSize: int64;
    fUnCompressedSize: int64;
    fHeaderEncrypted: boolean;
    fMultiVolume: boolean;
    fArchiveComment: boolean;
    fFileComment: boolean;
    fComment: AnsiString;
    fSigned: boolean;
    fLocked: boolean;
    fRecovery: boolean;
    fSFX: boolean;
    procedure Reset;
  protected
  public
  published
    property FileName: AnsiString read fFileName;
    property ArchiverMajorVersion: cardinal read fArchiverMajorVersion;
    property ArchiverMinorVersion: cardinal read fArchiverMinorVersion;
    property DictionarySize: int64 read fDictionarySize;
    property Encryption: boolean read fEncryption;
    property Solid: boolean read fSolid;
    property HostOS: AnsiString read fHostOS;
    property TotalFiles: integer read fTotalFiles;
    property CompressedSize: int64 read fCompressedSize;
    property UnCompressedSize: int64 read fUnCompressedSize;
    property HeaderEncrypted: boolean read fHeaderEncrypted;
    property MultiVolume: boolean read fMultiVolume;
    property ArchiveComment: boolean read fArchiveComment;
    property FileComment: boolean read fFileComment;
    property Comment: AnsiString read fComment;
    property Signed: boolean read fSigned;
    property Locked: boolean read fLocked;
    property Recovery: boolean read fRecovery;
    property SFX: boolean read fSFX;
  end;

  //the RAR Component itself
  TRAR = class(TComponent)
  private
    RARDLLInstance: THandle;
    RARArchiveInstance: Cardinal;
    HeaderData: TRARHeaderDataEx;
    ArchiveData: TRARArchiveDataEx;
    fArchiveInformation: TRARArchiveInformation;

    fAbort: Boolean;
    fListing: boolean;
    fPackedSizeMVVolume: int64;
    Password: AnsiString;
    FirstFileName: WideString;
    Comment: PAnsiChar;
    CommentResult: Cardinal;
    fProgressInfo: TRARProgressInfo;

    fReadMVToEnd: boolean;
    fAutoNextVolumeOnList: boolean;
    fForceTestToEnd:boolean;
    fDLLName: AnsiString;
    fOnError: TOnRARError;
    fOnListFile: TOnRARListFile;
    fOnPasswordRequired: TOnRARPasswordRequired;
    fOnNextVolumeRequired: TOnRARNextVolumeRequired;
    fOnProcess: TOnRARProcess;
    fOnReplace: TOnRARReplace;
    fOnVolumeChanged: TOnRARVolumeChanged;
    fOnFileProcessed: TOnRARFileProcessed;

    function OnUnRarCallBack(msg: Cardinal; UserData, P1, P2: LongInt): integer; stdcall;
    procedure DoError(ErrorCode: integer; Operation: TRAROperation);
    function getVersion: string;
    function LoadDLL: boolean;
    procedure UnloadDLL;
    function isDLLLoaded: boolean;
    function OpenArchive(Extract:boolean): boolean;
    function CloseArchive: boolean;
    function ReadArchiveInformation: boolean;
    function ListFiles(Mode:integer; Files:THashedStringList; Destination:WideString; RestoreFolder:boolean):boolean;
    procedure ProgressFile(EndOfArchive: boolean);
    function checkExtractFile(FileName:WideString; Files: THashedStringList): boolean;
    function getTotalFileSize(Files: THashedStringList): int64;
  protected
    RAROpenArchive: TRAROpenArchive;
    RAROpenArchiveEx: TRAROpenArchiveEx;
    RARCloseArchive: TRARCloseArchive;
    RARReadHeader: TRARReadHeader;
    RARReadHeaderEx: TRARReadHeaderEx;
    RARProcessFile: TRARProcessFile;
    RARProcessFileW: TRARProcessFileW;
    RARSetCallback: TRARSetCallback;
    RARSetChangeVolProc: TRARSetChangeVolProc;
    RARSetProcessDataProc: TRARSetProcessDataProc;
    RARSetPassword: TRARSetPassword;
    RARGetDllVersion: TRARGetDllVersion;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function OpenFile(FileName: AnsiString): boolean;
    function List: boolean;
    function Extract(Path: WideString; RestoreFolder: Boolean; Files: THashedStringList): boolean;
    function Test: boolean;
    procedure Abort;

    function isArchiveOpen: boolean;
    function GetDllVersion: integer;
  published
    property Version: string read getVersion;
    property ReadMultiVolumeToEnd: boolean read fReadMVToEnd write fReadMVToEnd;
    property AutoNextVolumeOnList: boolean read fAutoNextVolumeOnList write fAutoNextVolumeOnList;
    property ForceTestToEnd:boolean read fForceTestToEnd write fForceTestToEnd;
    property DllName: AnsiString read fDLLName write fDLLName;
    property ArchiveInformation: TRARArchiveInformation read fArchiveInformation;
    property OnError: TOnRARError read fOnError write fOnError;
    property OnListFile: TOnRARListFile read fOnListFile write fOnListFile;
    property OnPasswordRequired: TOnRARPasswordRequired read fOnPasswordRequired write fOnPasswordRequired;
    property OnNextVolumeRequired: TOnRARNextVolumeRequired read fOnNextVolumeRequired write fOnNextVolumeRequired;
    property OnProgress: TOnRARProcess read fOnProcess write fOnProcess;
    property OnReplace: TOnRARReplace read fOnReplace write fOnReplace;
    property OnVolumeChanged: TOnRARVolumeChanged read fOnVolumeChanged write fOnVolumeChanged;
    property OnFileProcessed: TOnRARFileProcessed read fOnFileProcessed write fOnFileProcessed;
  end;

implementation

const
  RARVERSION = '2.0';

function HandleRarCallBack(msg: Cardinal; UserData, P1, P2: LongInt): integer; stdcall;
begin
  try
    Result := TRAR(UserData).OnUnRarCallBack(msg, UserData, P1, P2);
  except
    Result := -1;
  end;
end;

function TRAR.OnUnRarCallBack(msg: Cardinal; UserData, P1, P2: LongInt): integer; stdcall;
var
  Password, FileName: AnsiString;
  PasswordFile: WideString;
  Cancel: Boolean;
begin
  Password := '';
  Cancel := False;
  Result := 0;

  case msg of
    UCM_CHANGEVOLUME: begin
        FileName := PAnsiChar(P1);
        case P2 of
          RAR_VOL_ASK:  begin
                          if (not fArchiveInformation.fOpened) and (not fReadMVToEnd) then
                            Result := -1
                          else begin
                            if fAutoNextVolumeOnList and fListing then
                              fAbort := True
                            else
                              if assigned(fOnNextVolumeRequired) then
                                fOnNextVolumeRequired(Self, PAnsiChar(P1), FileName, Cancel);

                            StrPCopy(PAnsiChar(P1), FileName);
                            if fAbort or Cancel then
                              Result := -1
                            else
                              Result := 0;
                          end;
                        end;
          RAR_VOL_NOTIFY: begin
                            if assigned(fOnVolumeChanged) then
                              fOnVolumeChanged(self, FileName);
                            Result := 0; //continue
                          end;
                        end;
        end;

    UCM_NEEDPASSWORD: begin
                        if not fArchiveInformation.fOpened then begin
                          fArchiveInformation.fHeaderEncrypted := True;
                          PasswordFile := WideString(fArchiveInformation.FileName);
                        end else
                          PasswordFile := fProgressInfo.FileName;
                        if PasswordFile = '' then
                          PasswordFile := FirstFileName;
                        if assigned(fOnPasswordRequired) then
                          fOnPasswordRequired(Self, not fArchiveInformation.fOpened, PasswordFile, Password, Cancel);
                        try
                          StrPCopy(Pointer(P1), Copy(Password, 1, P2));
                        finally
                          if fAbort or Cancel then
                            Result := -1
                        else
                          Result := 0;
                        end;
                      end;
    UCM_PROCESSDATA:  begin
                        //P1 points to data, length of data is p2
                        fProgressInfo.FileBytesDone := fProgressInfo.FileBytesDone + P2;
                        fProgressInfo.ArchiveSizeDone := fProgressInfo.ArchiveSizeDone + P2;
                        if assigned(fOnProcess) then
                          fOnProcess(Self, fProgressInfo.FileName,
                          fProgressInfo.ArchiveSizeTotal, fProgressInfo.ArchiveSizeDone,
                          fProgressInfo.FileBytesTotal, fProgressInfo.FileBytesDone);
                        if fAbort then
                          Result := -1
                        else
                          Result := 0;
                      end;
  end;
  if fAbort then
    Result := -1;
end;

constructor TRAR.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RARDLLInstance := 0;
  RARArchiveInstance := 0;
  fReadMVToEnd := True;
  fAutoNextVolumeOnList := True;
  fForceTestToEnd:=True;
  fListing := False;
  fDLLName := 'unrar.dll';
  FirstFileName := '';
  fArchiveInformation := TRARArchiveInformation.Create;
  fArchiveInformation.Reset;
end;

destructor TRAR.Destroy;
begin
  CloseArchive;
  if assigned(comment) then
    FreeMem(comment);
  UnLoadDLL;
  fArchiveInformation.Free;
  inherited Destroy;
end;

function TRAR.LoadDLL: boolean;
begin
  try
    RARDLLInstance := LoadLibraryA(PAnsiChar(fDLLName));
    if isDLLLoaded then begin
      @RAROpenArchive := GetProcAddress(RARDLLInstance, 'RAROpenArchive');
      @RAROpenArchiveEx := GetProcAddress(RARDLLInstance, 'RAROpenArchiveEx');
      @RARCloseArchive := GetProcAddress(RARDLLInstance, 'RARCloseArchive');
      @RARReadHeader := GetProcAddress(RARDLLInstance, 'RARReadHeader');
      @RARReadHeaderEx := GetProcAddress(RARDLLInstance, 'RARReadHeaderEx');
      @RARProcessFile := GetProcAddress(RARDLLInstance, 'RARProcessFile');
      @RARProcessFileW := GetProcAddress(RARDLLInstance, 'RARProcessFileW');
      @RARSetCallback := GetProcAddress(RARDLLInstance, 'RARSetCallback');
      @RARSetChangeVolProc := GetProcAddress(RARDLLInstance, 'RARSetChangeVolProc');
      @RARSetProcessDataProc := GetProcAddress(RARDLLInstance, 'RARSetProcessDataProc');
      @RARSetPassword := GetProcAddress(RARDLLInstance, 'RARSetPassword');
      @RARGetDllVersion := GetProcAddress(RARDLLInstance, 'RARGetDllVersion');
      if (@RAROpenArchive = nil) or (@RAROpenArchiveEx = nil) or (@RARCloseArchive = nil)
        or (@RARReadHeader = nil) or (@RARReadHeaderEx = nil) or (@RARProcessFile = nil) or (@RARProcessFileW = nil)
        or (@RARSetCallback = nil) or (@RARSetChangeVolProc = nil) or (@RARSetProcessDataProc = nil)
        or (@RARSetPassword = nil) or (@RARGetDllVersion = nil) then begin
        RARDLLInstance := 0;
        UnloadDLL;
      end;
      if isDLLLoaded and (RARGetDllVersion < MIN_RAR_VERSION) then
        MessageBox(0, 'please download the newest "unrar.dll" file. See www.rarlabs.com', 'error', 0);
    end;
    Result := isDLLLoaded;
  except
    Result := False;
  end;
end;

procedure TRAR.UnloadDLL;
begin
  try
    if isDLLLoaded then
      FreeLibrary(RARDLLInstance);
  finally
    RARDLLInstance := 0;
  end;
end;

function TRAR.isDLLLoaded: boolean;
begin
  Result := RARDLLInstance <> 0;
end;

procedure TRAR.DoError(ErrorCode: integer; Operation: TRAROperation);
begin
  if assigned(fOnError) then
    fOnError(Self, ErrorCode, Operation);
end;

//abort the current TRAROperation
procedure TRAR.Abort;
begin
  fAbort := True;
end;

function TRAR.OpenArchive(Extract:boolean): boolean;
begin
  Result := False;
  try
    Result := CloseArchive;
  except
    exit;
  end;
  if not Result then
    exit;
  try
    //prepare
    with fProgressInfo do begin
      FileBytesDone := 0;
      FileBytesTotal := 0;
      ArchiveSizeDone := 0;
      ArchiveSizeTotal := 0;
      FileName := '';
    end;
    with ArchiveData do begin
      OpenResult := RAR_SUCCESS;
      if Extract then
        OpenMode := RAR_OM_EXTRACT
      else
        if fReadMVToEnd then
          OpenMode := RAR_OM_LIST_INCSPLIT
        else
          OpenMode := RAR_OM_LIST;
      ArcName := PAnsiChar(fArchiveInformation.FileName);
      if not Assigned(Comment) then
        GetMem(Comment, MAX_RAR_COMMENTSIZE);
      CmtBuf := Comment;
      CmtBufSize := MAX_RAR_COMMENTSIZE;
      CmtSize := length(Comment);
      CmtState := CommentResult;
    end;

    //open
    RARArchiveInstance := RAROpenArchiveEx(@ArchiveData);
    //ArchiveHandle:=RAROpenArchive(@ArchiveData);
    //possible OpenResult values: RAR_SUCCESS, ERAR_NO_MEMORY, ERAR_BAD_DATA,
    //ERAR_BAD_ARCHIVE , ERAR_UNKNOWN_FORMAT, ERAR_EOPEN
    DoError(ArchiveData.OpenResult, roOpenArchive);
    Result := (ArchiveData.OpenResult = RAR_SUCCESS) and isArchiveOpen;
    if not Result then
      exit;
    try
      RARSetCallback(RARArchiveInstance, HandleRarCallBack, Integer(Self));
      if Password <> '' then
        RARSetPassword(RARArchiveInstance, PAnsiChar(Password));
    except
      Result := False;
    end;
  except
    Result := False;
  end;
end;

//returns if the archive is initialized or not
function TRAR.isArchiveOpen: boolean;
begin
  Result := FileExists(String(ArchiveInformation.FileName)) and (RARArchiveInstance <> 0);
end;

function TRAR.CloseArchive: boolean;
var
  State: integer;
begin
  if not isArchiveOpen then begin
    Result := True;
    exit;
  end;

  try
    State := RARCloseArchive(RARArchiveInstance);
    //possible: RAR_SUCCESS or ERAR_CLOSE
    if State = RAR_SUCCESS then
      RARArchiveInstance := 0;
    DoError(State, roCloseArchive);
    Result := (State = RAR_SUCCESS) and not isArchiveOpen;
  except
    Result := False;
  end;
end;

//Opens an archive. This is the first method you have to call!
function TRAR.OpenFile(FileName: AnsiString): boolean;
begin
  fAbort:=False;
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    DoError(ERAR_DLL_LOAD_ERROR, roLoadDLL);
    Result := False;
    Exit;
  end;

  fArchiveInformation.fOpened := False;
  fArchiveInformation.Reset;
  fArchiveInformation.fFileName := FileName;

  try
    Result := OpenArchive(False);
    if not Result then
      exit;
    Result := ReadArchiveInformation;
    if not Result then
      exit;
    Result := ListFiles(RAR_SKIP, nil, '', false);
    if not Result then
      exit;
  finally
    fArchiveInformation.fOpened := True;
    Result := CloseArchive;
  end;
end;

//List all files in the archive via OnListFile event. This function can be used
//for a refresh feature.
function TRAR.List: boolean;
begin
  Result := False;
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    DoError(ERAR_DLL_LOAD_ERROR, roLoadDLL);
    Result := False;
    Exit;
  end;
  
  try
    Result := OpenArchive(False);  //do not read the file data
    if not Result then
      exit;
    Result := ReadArchiveInformation;
    if not Result then
      exit;
    Result := ListFiles(RAR_SKIP,nil ,'' , false);
    if not Result then
      exit;
  finally
    fArchiveInformation.fOpened := Result;
    Result := CloseArchive and Result;
  end;
end;

//Extract files from the archive. To extract all files you can set the parameter
//"Files" to nil. You can also add wildchars to "Files". If a file is damaged and
//ForceTestToEnd is set, RAR Component will try to continue extracting. If
//ForceTestToEnd is not set, RAR Component will stop extracting if a file is
//damaged.
function TRAR.Extract(Path: WideString; RestoreFolder: boolean; Files: THashedStringList): boolean;
begin
  Result := False;
  fAbort := False;
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    DoError(ERAR_DLL_LOAD_ERROR, roLoadDLL);
    Result := False;
    Exit;
  end;

  if Path = '' then
    exit;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  
  try
    Result := OpenArchive(True); // read the file data to extract it
    if not Result then
      exit;
    Result := ReadArchiveInformation;
    if not Result then
      exit;
    Result := ListFiles(RAR_EXTRACT, files, PWideChar(Path), RestoreFolder);   //todo
    if not Result then
      exit;
  finally
    fArchiveInformation.fOpened := True;
    Result := CloseArchive and Result;
  end;
end;

//Tests if the archive is damaged or not. If ForceTestToEnd is set, all files
//will be tested. If ForceTestToEnd is not set and a damaged file was found,
//the function will return "False" without testing other files.
//OnFileProcessed event can be used to retrive the test result for the files.
function TRAR.Test: boolean;
begin
  Result := False;
  fAbort := False;
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    DoError(ERAR_DLL_LOAD_ERROR, roLoadDLL);
    Result := False;
    Exit;
  end;
  
  try
    Result := OpenArchive(True); // read the file data to test it
    if not Result then
      exit;
    Result := ReadArchiveInformation;
    if not Result then
      exit;
    Result := ListFiles(RAR_TEST, nil, '', false);
  finally
    fArchiveInformation.fOpened := True;
    Result := CloseArchive and Result;
  end;
end;

function TRAR.ReadArchiveInformation: boolean;
begin
  try
    //((ArchiveData.Flags and $00000100)=$00000100)=first volume
    //((ArchiveData.Flags and $00000001)=$00000001)=Volume attribute (archive volume)
    //((ArchiveData.Flags and $00000010)=$00000010)=New volume naming scheme ('volname.partN.rar')
    //set archive info
    if ((ArchiveData.Flags and $00000004) = $00000004) then
      fArchiveInformation.fLocked := True;
    if ((ArchiveData.Flags and $00000020) = $00000020) then
      fArchiveInformation.fSigned := True;
    if ((ArchiveData.Flags and $00000040) = $00000040) then
      fArchiveInformation.fRecovery := True;
    if ((ArchiveData.Flags and $00000008) = $00000008) then
      fArchiveInformation.fSolid := True;
    if ((ArchiveData.Flags and $00000002) = $00000002) then
      fArchiveInformation.fArchiveComment := True;
    if ((ArchiveData.Flags and $00000080) = $00000080) then
      fArchiveInformation.fHeaderEncrypted := True;
    fArchiveInformation.fSFX := isSFX(fArchiveInformation.FileName);

    //read archive comment
    case ArchiveData.CmtState of
      ERAR_COMMENTS_EXISTS: begin
                              fArchiveInformation.fComment := StrPas(Comment);
                              fArchiveInformation.fArchiveComment := True;
                            end;
      ERAR_NO_COMMENTS: begin
                          fArchiveInformation.fComment := '';
                          fArchiveInformation.fArchiveComment := False;
                        end;
      ERAR_NO_MEMORY: DoError(ERAR_NO_MEMORY, roOpenArchive);
      ERAR_BAD_DATA: DoError(ERAR_BAD_DATA, roOpenArchive);
      ERAR_UNKNOWN_FORMAT: DoError(ERAR_UNKNOWN_FORMAT, roOpenArchive);
      ERAR_SMALL_BUF: DoError(ERAR_SMALL_BUF, roOpenArchive);
    end;
    if (ArchiveData.CmtState <> ERAR_NO_COMMENTS) and (ArchiveData.CmtState <> ERAR_COMMENTS_EXISTS) then
      Result := False //error reading comment
    else
      Result := True;
  except
    Result := False;
  end;
end;

function TRAR.checkExtractFile(FileName:WideString; Files: THashedStringList): boolean;
var
  new, existing: TRARReplaceData;
  ft: _FILETIME;
  st: TSystemTime;
  action: TRARReplaceAction;
  i:integer;
begin
  //nil = accept all files, otherwise file must be specified in Files
  Result := (Files = nil) or (Files.IndexOf(HeaderData.FileNameW) >= 0);
  //check masks
  if not Result then
    for i := 0 to Files.Count - 1 do
      if MatchesMask(FileName, Files[i]) then
        Result := True;

  if not Result then
    exit;
  //check if file exists
  if not FileExists(FileName) then
    Result := True
  else
    begin
      new.FileName := FileName;
      new.Size := GetFileSize(FileName);
      new.Time := GetFileModifyDate(FileName);
      existing.FileName := FileName;
      existing.Size := CardToInt64(HeaderData.UnpSizeHigh, HeaderData.UnpSize);
        DosDateTimeToFileTime(HiWord(HeaderData.FileTime),
        LoWord(HeaderData.FileTime), ft);
      FileTimeToSystemTime(ft, st);
      existing.Time := SystemTimeToDateTime(st);
      action := rrSkip;
      if assigned(fOnReplace) then
        fOnReplace(self, existing, new, action);
      case action of
        rrCancel: begin
                    Abort;
                    Result := False;
                  end;
        rrOverwrite: Result := True;
        rrSkip: Result := False;
      end;
    end;
end;

function TRAR.getTotalFileSize(Files: THashedStringList): int64;
begin
  if Files = nil then
    Result := ArchiveInformation.UnCompressedSize
  else
    Result := ArchiveInformation.UnCompressedSize; //todo, will be fixed in 2.1
end;

function TRAR.ListFiles(Mode:integer; Files:THashedStringList; Destination: WideString;  RestoreFolder:boolean):boolean;
var
  State:integer;
  op:TRAROperation;
  TestResult:boolean;
  FileName:WideString;
  first: boolean;
begin
  fListing := True;
  Result := True;
  TestResult := True;
  State := RAR_SUCCESS;
  try
    fProgressInfo.ArchiveSizeTotal := getTotalFileSize(Files);
    first := True;
    while (State = RAR_SUCCESS) and Result and not fAbort do begin
      State := RARReadHeaderEx(RARArchiveInstance, @HeaderData);

      if first then
        FirstFileName := HeaderData.FileNameW;
      first := False;

      if State = ERAR_END_ARCHIVE then
        break;
      if State <> RAR_SUCCESS then
        Result := False;
      DoError(State, roListFiles);

      if Mode = RAR_SKIP then //only while listing archive
        ProgressFile(State = ERAR_EOPEN); //fOnListFile + writte data to farchiveInformation
      with fProgressInfo do begin
        FileName := HeaderData.FileNameW;
        FileBytesDone := 0;
        FileBytesTotal := CardToInt64(HeaderData.UnpSizeHigh, HeaderData.UnpSize);
      end;

      if not RestoreFolder then
        FileName := Destination + ExtractFileName(fProgressInfo.FileName)
      else
        FileName := Destination + fProgressInfo.FileName;

      case Mode of
        RAR_SKIP: op := roListFiles;
        RAR_TEST: op := roTest;
        RAR_EXTRACT: op := roExtract;
        else op := roListFiles;
      end;

      if Mode = RAR_EXTRACT then
        if checkExtractFile(FileName, Files) then begin
          State := RARProcessFileW(RARArchiveInstance, Mode, nil, PWideChar(FileName)); //extract file
          if assigned(fOnFileProcessed) then
            fOnFileProcessed(self, HeaderData.FileNameW, op, State);
        end else
          State := RARProcessFileW(RARArchiveInstance, RAR_SKIP, nil, nil) //skip file
      else begin
        State := RARProcessFileW(RARArchiveInstance, Mode, nil, nil); //test or skip
        if assigned(fOnFileProcessed) then
          fOnFileProcessed(self, HeaderData.FileNameW, op, State);
      end;

      if State <> RAR_SUCCESS then
        TestResult := False;

      if fForceTestToEnd then
        State := RAR_SUCCESS;

      if State <> RAR_SUCCESS then
        Result := False;
      DoError(State, roListFiles);
    end;
  except
    Result := False;
  end;
  fListing := False;
  Result := Result and TestResult;
end;

procedure TRAR.ProgressFile(EndOfArchive: boolean);
var
  FileItem: TRARFileItem;
  ft: _FILETIME;
  st: TSystemTime;
  OS: AnsiString;
begin
  //handle informations from multivolume
  if (fReadMVToEnd) and (not ((HeaderData.Flags and $00000001) = $00000001)) and //first part of the file
    (((HeaderData.Flags and $00000002) = $00000002)) then begin
      fPackedSizeMVVolume := CardToInt64(HeaderData.PackSizeHigh, HeaderData.PackSize);
      exit;
    end;
  if (fReadMVToEnd) and (((HeaderData.Flags and $00000001) = $00000001)) and //not last, not first part
    (((HeaderData.Flags and $00000002) = $00000002)) then begin
    fPackedSizeMVVolume := fPackedSizeMVVolume + CardToInt64(HeaderData.PackSizeHigh, HeaderData.PackSize);
    exit;
  end;
  if (fReadMVToEnd) and (((HeaderData.Flags and $00000001) = $00000001)) and //last part
    (not ((HeaderData.Flags and $00000002) = $00000002)) then begin
      fPackedSizeMVVolume := fPackedSizeMVVolume + CardToInt64(HeaderData.PackSizeHigh, HeaderData.PackSize);
      HeaderData.PackSizeHigh := Int64Rec(fPackedSizeMVVolume).Hi;
      HeaderData.PackSize := Int64Rec(fPackedSizeMVVolume).Lo;
    end;

  if (fReadMVToEnd) and ((HeaderData.Flags and $00000002) = $00000002) and EndOfArchive then //not last part
    exit;

  //update archive information
  if fArchiveInformation.fArchiverMajorVersion * 10 + fArchiveInformation.fArchiverMinorVersion < HeaderData.UnpVer then begin
    fArchiveInformation.fArchiverMinorVersion := HeaderData.UnpVer mod 10;
    fArchiveInformation.fArchiverMajorVersion := (HeaderData.UnpVer - fArchiveInformation.fArchiverMinorVersion) div 10;
  end;
  if ((HeaderData.Flags and $00000004) = $00000004) then
    fArchiveInformation.fEncryption := True;
  if ((HeaderData.Flags and $00000010) = $00000010) then
    fArchiveInformation.fSolid := True;
  OS := 'unknown';
  case HeaderData.HostOS of
    0: OS := 'DOS';
    1: OS := 'IBM OS/2';
    2: OS := 'Windows';
    3: OS := 'Unix';
  end;
  fArchiveInformation.fHostOS := OS;
  if (not ((HeaderData.Flags and $00000070) = $00000070)) and (HeaderData.FileAttr <> faDirectory) then begin //not a directory
    fArchiveInformation.fTotalFiles := fArchiveInformation.fTotalFiles + 1;
    case (HeaderData.Flags shl 24 shr 29) of
      0: fArchiveInformation.fDictionarySize := 65536;
      1: fArchiveInformation.fDictionarySize := 131072;
      2: fArchiveInformation.fDictionarySize := 262144;
      3: fArchiveInformation.fDictionarySize := 524288;
      4: fArchiveInformation.fDictionarySize := 1048576;
      5: fArchiveInformation.fDictionarySize := 2097152;
      6: fArchiveInformation.fDictionarySize := 4194304;
    end;
  end;
  fArchiveInformation.fCompressedSize := fArchiveInformation.fCompressedSize + CardToInt64(HeaderData.PackSizeHigh, HeaderData.PackSize);
  fArchiveInformation.fUnCompressedSize := fArchiveInformation.fUnCompressedSize + CardToInt64(HeaderData.UnpSizeHigh, HeaderData.UnpSize);
  if ((HeaderData.Flags and $00000001) = $00000001) or ((HeaderData.Flags and $00000002) = $00000002) then //file continued in last or next part
    fArchiveInformation.fMultiVolume := True;
  if HeaderData.CmtSize > 0 then
    fArchiveInformation.fFileComment := True;

  with FileItem do begin
    FileName := StrPas(HeaderData.FileName);
    FileNameW := HeaderData.FileNameW;
    CompressedSize := CardToInt64(HeaderData.PackSizeHigh, HeaderData.PackSize);
    UnCompressedSize := CardToInt64(HeaderData.UnpSizeHigh, HeaderData.UnpSize);
    HostOS := OS;
    CRC32 := AnsiString(Format('%x', [HeaderData.FileCRC]));
    Attributes := HeaderData.FileAttr;
    Comment := HeaderData.CmtBuf;
    DosDateTimeToFileTime(HiWord(HeaderData.FileTime),
      LoWord(HeaderData.FileTime),
      ft);
    FileTimeToSystemTime(ft, st);
    Time := SystemTimeToDateTime(st);
    CompressionStrength := HeaderData.Method;
    ArchiverVersion := HeaderData.UnpVer;
    Encrypted := ((HeaderData.Flags and $00000004) = $00000004);
  end;
  if assigned(fOnListFile) then
    fOnListFile(Self, FileItem);
end;

function TRAR.GetDllVersion: integer;
begin
  if not isDLLLoaded then
    LoadDLL;
  if not isDLLLoaded then begin
    DoError(ERAR_DLL_LOAD_ERROR, roLoadDLL);
    Result := 0;
    Exit;
  end;
  try
    Result := RARGetDllVersion;
  except
    Result := 0;
  end;
end;

function TRAR.getVersion: string;
begin
  result := RARVERSION;
end;

procedure TRARArchiveInformation.Reset;
begin
  fOpened := False;

  fFileName := '';
  fTotalFiles := 0;
  fArchiverMajorVersion := 0;
  fArchiverMinorVersion := 0;
  fDictionarySize := 0;
  fEncryption := False;
  fSolid := False;
  fHostOS := '';
  fTotalFiles := 0;
  fCompressedSize := 0;
  fUnCompressedSize := 0;
  fHeaderEncrypted := False;
  fMultiVolume := False;
  fArchiveComment := False;
  fFileComment := False;
  fComment := '';
  fLocked := False;
  fSigned := False;
  fRecovery := False;
  fSFX := False;
end;


end.
