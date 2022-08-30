{************************************************************************}
{ TCABFile component                                                     }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by TMS Software                                                }
{           copyright © 1999-2013                                        }
{           Email : info@tmssoftware.com                                 }
{           Web : http://www.tmssoftware.com                             }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit cabfiles;

{$I TMSDEFS.INC}

{$DEFINE noTMSDEBUG}

{$R CABFILES.RES}

interface

uses
  Windows, Classes, SysUtils;

const
    cabdll = 'cabinet.dll';

    FDIERROR_NONE = 0;
        // Description: No error
        // Cause:       Function was successfull.
        // Response:    Keep going!

    FDIERROR_CABINET_NOT_FOUND = 1;
        // Description: Cabinet not found
        // Cause:       Bad file name or path passed to FDICopy(), or returned
        //              to fdintNEXT_CABINET.
        // Response:    To prevent this error, validate the existence of the
        //              the cabinet *before* passing the path to FDI.

    FDIERROR_NOT_A_CABINET = 2;
        // Description: Cabinet file does not have the correct format
        // Cause:       File passed to to FDICopy(), or returned to
        //              fdintNEXT_CABINET, is too small to be a cabinet file,
        //              or does not have the cabinet signature in its first
        //              four bytes.
        // Response:    To prevent this error, call FDIIsCabinet() to check a
        //              cabinet before calling FDICopy() or returning the
        //              cabinet path to fdintNEXT_CABINET.

    FDIERROR_UNKNOWN_CABINET_VERSION = 3;
        // Description: Cabinet file has an unknown version number.
        // Cause:       File passed to to FDICopy(), or returned to
        //              fdintNEXT_CABINET, has what looks like a cabinet file
        //              header, but the version of the cabinet file format
        //              is not one understood by this version of FDI.  The
        //              erf.erfType field is filled in with the version number
        //              found in the cabinet file.
        // Response:    To prevent this error, call FDIIsCabinet() to check a
        //              cabinet before calling FDICopy() or returning the
        //              cabinet path to fdintNEXT_CABINET.

    FDIERROR_CORRUPT_CABINET = 4;
        // Description: Cabinet file is corrupt
        // Cause:       FDI returns this error any time it finds a problem
        //              with the logical format of a cabinet file, and any
        //              time one of the passed-in file I/O calls fails when
        //              operating on a cabinet (PFNOPEN, PFNSEEK, PFNREAD,
        //              or PFNCLOSE).  The client can distinguish these two
        //              cases based upon whether the last file I/O call
        //              failed or not.
        // Response:    Assuming this is not a real corruption problem in
        //              a cabinet file, the file I/O functions could attempt
        //              to do retries on failure (for example, if there is a
        //              temporary network connection problem).  If this does
        //              not work, and the file I/O call has to fail, then the
        //              FDI client will have to clean up and call the
        //              FDICopy() function again.

    FDIERROR_ALLOC_FAIL = 5;
        // Description: Could not allocate enough memory
        // Cause:       FDI tried to allocate memory with the PFNALLOC
        //              function, but it failed.
        // Response:    If possible, PFNALLOC should take whatever steps
        //              are possible to allocate the memory requested.  If
        //              memory is not immediately available, it might post a
        //              dialog asking the user to free memory, for example.
        //              Note that the bulk of FDI's memory allocations are
        //              made at FDICreate() time and when the first cabinet
        //              file is opened during FDICopy().

    FDIERROR_BAD_COMPR_TYPE = 6;
        // Description: Unknown compression type in a cabinet folder
        // Cause:       [Should never happen.]  A folder in a cabinet has an
        //              unknown compression type.  This is probably caused by
        //              a mismatch between the version of FCI.LIB used to
        //              create the cabinet and the FDI.LIB used to read the
        //              cabinet.
        // Response:    Abort.

    FDIERROR_MDI_FAIL = 7;
        // Description: Failure decompressing data from a cabinet file
        // Cause:       The decompressor found an error in the data coming
        //              from the file cabinet.  The cabinet file was corrupted.
        //              [11-Apr-1994 bens When checksuming is turned on, this
        //              error should never occur.]
        // Response:    Probably should abort; only other choice is to cleanup
        //              and call FDICopy() again, and hope there was some
        //              intermittent data error that will not reoccur.

    FDIERROR_TARGET_FILE = 8;
        // Description: Failure writing to target file
        // Cause:       FDI returns this error any time it gets an error back
        //              from one of the passed-in file I/O calls fails when
        //              writing to a file being extracted from a cabinet.
        // Response:    To avoid or minimize this error, the file I/O functions
        //              could attempt to avoid failing.  A common cause might
        //              be disk full -- in this case, the PFNWRITE function
        //              could have a check for free space, and put up a dialog
        //              asking the user to free some disk space.

    FDIERROR_RESERVE_MISMATCH = 9;
        // Description: Cabinets in a set do not have the same RESERVE sizes
        // Cause:       [Should never happen]. FDI requires that the sizes of
        //              the per-cabinet, per-folder, and per-data block
        //              RESERVE sections be consistent across all the cabinets
        //              in a set.
        // Response:    Abort.

    FDIERROR_WRONG_CABINET = 10;
        // Description: Cabinet returned on fdintNEXT_CABINET is incorrect
        // Cause:       NOTE: THIS ERROR IS NEVER RETURNED BY FDICopy()!
        //              Rather, FDICopy() keeps calling the fdintNEXT_CABINET
        //              callback until either the correct cabinet is specified,
        //              or you return ABORT.
        //              When FDICopy() is extracting a file that crosses a
        //              cabinet boundary, it calls fdintNEXT_CABINET to ask
        //              for the path to the next cabinet.  Not being very
        //              trusting, FDI then checks to make sure that the
        //              correct continuation cabinet was supplied!  It does
        //              this by checking the "setID" and "iCabinet" fields
        //              in the cabinet.  When MAKECAB.EXE creates a set of
        //              cabinets, it constructs the "setID" using the sum
        //              of the bytes of all the destination file names in
        //              the cabinet set.  FDI makes sure that the 16-bit
        //              setID of the continuation cabinet matches the
        //              cabinet file just processed.  FDI then checks that
        //              the cabinet number (iCabinet) is one more than the
        //              cabinet number for the cabinet just processed.
        // Response:    You need code in your fdintNEXT_CABINET (see below)
        //              handler to do retries if you get recalled with this
        //              error.  See the sample code (EXTRACT.C) to see how
        //              this should be handled.

    FDIERROR_USER_ABORT = 11;
        // Description: FDI aborted.
        // Cause:       An FDI callback returnd -1 (usually).
        // Response:    Up to client.

    FCIERR_NONE = 0;                // No error

    FCIERR_OPEN_SRC  =1 ;           // Failure opening file to be stored in cabinet
                            //  erf.erfTyp has C run-time *errno* value
    FCIERR_READ_SRC = 2;            // Failure reading file to be stored in cabinet
                            //  erf.erfTyp has C run-time *errno* value
    FCIERR_ALLOC_FAIL = 3;          // Out of memory in FCI

    FCIERR_TEMP_FILE = 4;           // Could not create a temporary file
                            //  erf.erfTyp has C run-time *errno* value
    FCIERR_BAD_COMPR_TYPE = 5;      // Unknown compression type

    FCIERR_CAB_FILE = 6;            // Could not create cabinet file
                            //  erf.erfTyp has C run-time *errno* value
    FCIERR_USER_ABORT = 7;          // Client requested abort

    FCIERR_MCI_FAIL = 8;            // Failure compressing data

    fdintCABINET_INFO = 0;              // General information about cabinet
    fdintPARTIAL_FILE = 1;              // First file in cabinet is continuation
    fdintCOPY_FILE = 2;                 // File to be copied
    fdintCLOSE_FILE_INFO = 3;           // close the file, set relevant info
    fdintNEXT_CABINET = 4;              // File continued to next cabinet
    fdintENUMERATE = 5;                 // Enumeration status

    CB_MAX_CHUNK        =      32768;
    CB_MAX_DISK         =  $7fffffff;
    CB_MAX_FILENAME     =        256;
    CB_MAX_CABINET_NAME =        256;
    CB_MAX_CAB_PATH     =        256;
    CB_MAX_DISK_NAME    =        256;

    FOLDER_THRESHOLD	= 900000;

    statusFile     = 0;   // Add File to Folder callback
    statusFolder   = 1;   // Add Folder to Cabinet callback
    statusCabinet  = 2;   // Write out a completed cabinet callback

    O_CREAT =    $100;  // create and open file
    O_TRUNC =    $200;  // open with truncation
    O_EXCL  =    $400;  // exclusive open

    MAJ_VER = 1; // Major version nr.
    MIN_VER = 5; // Minor version nr.
    REL_VER = 5; // Release nr.
    BLD_VER = 1; // Build nr.

    // version history
    // 1.4.0.2 : fix for directory hierarchy handling in uncompress
    // 1.4.0.3 : fixed issue for file timestamp handling
    // 1.5.0.0 : added support for Delphi 2007
    // 1.5.1.0 : added support to add files without faArchive attribute set
    // 1.5.1.1 : fixed issue with very long filenames
    // 1.5.1.2 : fixed issue with date/time zero in CAB file
    // 1.5.2.0 : fixed issues with Delphi 2009
    // 1.5.3.0 : Added support to handle files > 2GB
    // 1.5.3.1 : Fixed : Issue with progress event handler for files > 2GB
    // 1.5.4.0 : Improved : Handling for extract with subfolders
    // 1.5.5.0 : New : Public property ExtractRelative: boolean added
    // 1.5.5.1 : Fixed : Issue with change in ansistring handling with older Delphi versions

type

  ECABFileError = class(Exception);

  perf = ^terf;
  terf = record
          erfOper,erfType: Integer;
          fError:bool;
         end;

  PFDICABINETINFO = ^TFDICABINETINFO;
  TFDICABINETINFO = record
        cbCabinet: Longint;              // Total length of cabinet file
        cFolders: Smallint;              // Count of folders in cabinet
        cFiles: Smallint;                // Count of files in cabinet
        setID: Smallint;                 // Cabinet set ID
        iCabinet: Smallint;              // Cabinet number in set (0 based)
        fReserve: Integer;               // TRUE => RESERVE present in cabinet
        hasprev: Integer;                // TRUE => Cabinet is chained prev
        hasnext: Integer;                // TRUE => Cabinet is chained next
       end;

  PFDINOTIFICATION = ^TFDINOTIFICATION;
  TFDINOTIFICATION = record
    cb: Longint;
    psz1: PAnsiChar;
    psz2: PAnsiChar;
    psz3: PAnsiChar;                   // Points to a 256 character buffer
    pv: Pointer;                       // Value for client
    hf: Integer;
    date: Smallint;
    time: Smallint;
    attribs: Smallint;
    setID: Smallint;                   // Cabinet set ID
    iCabinet: Smallint;                // Cabinet number (0-based)
    iFolder: Smallint;                 // Folder number (0-based)
    FDIERROR: Integer;
  end;

  PCCAB = ^TCCAB;
  TCCAB = record
    cb: Longint;                  // size available for cabinet on this media
    cbFolderThresh: Longint;      // Thresshold for forcing a new Folder
    cbReserveCFHeader: Integer;   // Space to reserve in CFHEADER
    cbReserveCFFolder: Integer;   // Space to reserve in CFFOLDER
    cbReserveCFData: Integer;     // Space to reserve in CFDATA
    iCab: Integer;                // sequential numbers for cabinets
    iDisk: Integer;               // Disk number
    fFailOnIncompressible: BOOL;  // TRUE => Fail if a block is incompressible
    setID: Smallint;               // Cabinet set ID
    szDisk:array[0..CB_MAX_DISK_NAME - 1] of ansichar;    // current disk name
    szCab:array[0..CB_MAX_CABINET_NAME - 1] of ansichar;  // current cabinet name
    szCabPath:array[0..CB_MAX_CAB_PATH - 1] of ansichar;  // path for creating cabinet
  end;

  TCABFile = class;

  TProcessMode = (pmContents,pmExtractAll,pmExtractFile,pmExtractSelected);

  TCABFileEntry = class(TCollectionItem)
  private
    FName:string;
    FSize: Int64;
    FDate: TDateTime;
    FSelected: Boolean;
    FRelPath:string;
    FDateCorrect: boolean;
  public
    constructor Create(Collection:TCollection); override;
    destructor Destroy; override;
    property Size: Int64 read FSize write FSize;
    property Date: TDateTime read FDate write FDate;
    property DateCorrect: boolean read FDateCorrect write FDateCorrect;
    property Selected: Boolean read FSelected write FSelected;
  published
    property Name:string read FName write FName;
    property RelPath:string read FRelPath write FRelPath;
  end;

  TCABFileContents = class(TCollection)
  private
    FOwner: TCABFile;
    function GetItem(Index: Integer): TCABFileEntry;
    procedure SetItem(Index: Integer; Value: TCABFileEntry);
  public
    constructor Create(AOwner: TCABFile);
    procedure AddFolder(FileSpec: string);
    procedure AddRelFolder(FileSpec,RelativePath: string);
    procedure AddFiles(FileSpec,RelativePath:string;SubDirectory:Boolean = False);
    function Add:TCABFileEntry;
    function Insert(index: Integer): TCABFileEntry;
    property Items[Index: Integer]: TCABFileEntry read GetItem write SetItem;
    function IsInList(s: ansistring): Boolean;
    function IsSelected(s: ansistring): Boolean;
    procedure SelectAll;
    procedure SelectNone;
  protected
    function GetOwner: TPersistent; override;
  end;

  TCompressionType = (typNone,typMSZIP,typLZX);

  TCompressProgress = procedure(Sender:TObject;pos,tot: Int64) of object;

  TDeCompressProgress = procedure(Sender:TObject;FileName:string;pos,tot: Int64) of object;

  TOverWriteFile = procedure(Sender:TObject;FileName:string;var Allow: Boolean) of object;

  TLZXMemory = (lzxLowest,lzxLower,lzxLow,lzxMedium,lzxHigh,lzxHigher,lzxHighest);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCABFile = class(TComponent)
  private
    FCABFileContents: TCABFileContents;
    FCABFile: string;
    FExtractFile: string;
    FTargetPath: string;
    FTotSizeFiles: Int64;
    FTotSizeCompress: Int64;
    FTotSizeProgress: Int64;
    FTotFileSizeDecompress: Int64;
    FCurFileSizeDecompress: Int64;
    FCurFileNameDecompress: string;
    FProcessMode: TProcessMode;
    FCompressionType: TCompressionType;
    FLZXMemory: TLZXMemory;
    FOnCompressProgress: TCompressProgress;
    FOnDeCompressProgress: TDeCompressProgress;
    FOnOverWriteFile: TOverWriteFile;
    FExtractRelative: boolean;
    function GetCompressionRatio: Double;
    function DecompressCABFile: Integer;
    function CompressCABFile: Integer;
    procedure Error(err,errt: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer;
    procedure CompressProgress(pos,tot: Int64);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetContents;
    function ExtractAll: Boolean;
    function ExtractSelected: Boolean;

    function ExtractFile(const FName:string): Boolean;
    function Compress: Boolean;
    function MakeSFX: Boolean;
    function ConvertToSFX: Boolean;
    property CompressionRatio: Double read GetCompressionRatio;
    property CompressedSize: Int64 read FTotSizeCompress;
    property OriginalSize: Int64 read FTotSizeFiles;
    property ExtractRelative: boolean read FExtractRelative write FExtractRelative;
  published
    property CABFileContents: TCABFileContents read FCABFileContents write FCABFileContents;
    property CABFile: string read FCABFile write FCABFile;
    property CompressionType: TCompressionType read FCompressionType write FCompressionType;
    property LZXMemory: TLZXMemory read FLZXMemory write FLZXMemory;
    property TargetPath: string read FTargetPath write FTargetPath;
    property OnCompressProgress: TCompressProgress read FOnCompressProgress write FOnCompressProgress;
    property OnDecompressProgress: TDecompressProgress read FOnDecompressProgress write FOnDecompressProgress;
    property OnOverWriteFile: TOverWriteFile read FOnOverWriteFile write FOnOverWriteFile;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

{$IFDEF DELPHIXE4_LVL}
uses
  AnsiStrings;
{$ENDIF}

var
  doExtract: Boolean;
  cf: TCABFile;

function StrPasN(p: PAnsiChar): string;
begin
{$IFDEF DELPHIXE4_LVL}
  Result := string(AnsiStrings.StrPas(p));
{$ENDIF}
{$IFNDEF DELPHIXE4_LVL}
  Result := string(StrPas(p));
{$ENDIF}
end;

procedure StrCopyN(Dest,Source: PAnsiChar);
begin
{$IFDEF DELPHIXE4_LVL}
  AnsiStrings.StrCopy(Dest,Source);
{$ENDIF}
{$IFNDEF DELPHIXE4_LVL}
  StrCopy(Dest,Source);
{$ENDIF}
end;

procedure StrCatN(Dest,Source: PAnsiChar);
begin
{$IFDEF DELPHIXE4_LVL}
  AnsiStrings.StrCat(Dest,Source);
{$ENDIF}
{$IFNDEF DELPHIXE4_LVL}
  StrCat(Dest,Source);
{$ENDIF}
end;

function StrICompN(Src1,Src2: PAnsiChar): integer;
begin
{$IFDEF DELPHIXE4_LVL}
 Result := AnsiStrings.StrIComp(Src1,Src2);
{$ENDIF}
{$IFNDEF DELPHIXE4_LVL}
 Result := StrIComp(Src1,Src2);
{$ENDIF}

end;

{$IFNDEF DELPHI7_LVL}
function RemoveBackslash(dir: string):string;
var
  ch: char;
begin
  if (Length(dir)>0) then
  begin
    ch := dir[length(dir)];
    if (ch = '/') or (ch = '\') then
      delete(dir,length(dir),1);
  end;
  Result := dir;
end;


function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;


function ForceDir(Dir: string): Boolean;
begin
  Result := True;
  Dir := RemoveBackslash(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
  Result := ForceDir(ExtractFilePath(Dir)) and CreateDir(Dir);
end;


function DirExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ExtractSubDir(s:string):string;
var
  res: string;
begin
  res := '';
  while pos('/',s) > 0 do
  begin
    res := res + copy(s,1,pos('/',s));
    delete(s,1,pos('/',s));
  end;
  while pos('\',s) > 0 do
  begin
    res := res + copy(s,1,pos('\',s));
    delete(s,1,pos('\',s));
  end;
  Result := res;
end;

{$ENDIF}

procedure SysErr(Error: Integer);
var
  buf:array[0..1024] of char;
begin
  if (Error <> 0) then
  begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,Error,0,buf,SizeOf(buf),nil);
    Messagebox(0,buf,'Error',mb_ok or mb_iconexclamation);
  end;
end;

function FdiNotification(fdint: Integer; notif: PFDINOTIFICATION): Integer; cdecl;
var
  FileTime,LFileTime:TFileTime;
  SysTime: TSystemTime;
  subdir:string;
  ExtractPath:string;
  ExtractFName: ansistring;
  Allow: Boolean;
  {$IFDEF DELPHI_UNICODE}
  sa: ansistring;
  {$ENDIF}
  FileName: string;
  FilePath: string;
  TgtFile: string;
begin
  Result := 0;

  case fdint of
  fdintCABINET_INFO:
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring('cabinet info');
      {$ENDIF}
    end;
  fdintPARTIAL_FILE:
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring('partial file');
      {$ENDIF}
    end;
  fdintCOPY_FILE:
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring('copy file');
      outputdebugstringa(pansichar(notif^.psz1));
      {$ENDIF}

      Result := 0;

      if (TCABFile(notif^.pv).FProcessMode = pmContents) then
      begin
        with TCABFile(notif^.pv).CABFileContents.Add do
        begin
          Name := StrPasN(notif^.psz1);
          if (notif^.time = 0 ) and (notif^.date = 0 ) then
          begin
            Date := now;
            DateCorrect := false;
          end
          else
            Date := FileDateToDateTime(makelong(word(notif^.time),word(notif^.date)));
          Size := notif^.cb;
        end;
        Result := 0;
      end;                                                     

      ExtractFName := ansistring(TCABFile(notif^.pv).FExtractFile);
      if (TCABFile(notif^.pv).FProcessMode = pmExtractAll) or
         ((TCABFile(notif^.pv).FProcessMode = pmExtractFile) and
         (StrICompN(notif^.psz1,PAnsiChar(ExtractFName))=0)) or
         ((TCABFile(notif^.pv).FProcessMode = pmExtractSelected) and
         (TCABFile(notif^.pv).CABFileContents.IsSelected(ansistring(StrPasN(notif^.psz1))))) then
      begin
        {$IFNDEF DELPHI7_LVL}
        ExtractPath := TCABFile(notif^.pv).targetPath;

        if (ExtractPath = '') then
          SubDir := ExtractFileDir(ExpandFileName(string(strpas(notif^.psz1))))
        else
        begin
          if (ExtractPath[length(ExtractPath)]<>'\') then
            ExtractPath := ExtractPath+'\';
          subdir := ExtractPath + ExtractSubDir(Strpas(notif^.psz1));
        end;

        if not DirExists(subdir) then
          ForceDir(subdir);
        {$ENDIF} 

        {$IFDEF DELPHI7_LVL}
 
        ExtractPath := IncludeTrailingPathDelimiter(TCABFile(notif^.pv).targetPath);

        FileName := ExtractFileName(string(notif^.psz1));

        if  TCABFile(notif^.pv).ExtractRelative then
          TgtFile := StrPasN(notif^.psz1)
        else
          TgtFile := FileName;

        FilePath := ExtractFilePath(string(notif^.psz1));

        if (Length(FilePath) > 0) and (FilePath[1] = PathDelim) then
          Delete(FilePath, 1, 1);
        SubDir := ExtractPath + string(FilePath);

        { Create the directory if it does not exist. Note that ForceDirectories }
        { takes a fully-qualified path name, hence the ExpandFileName function. }
        { Also note that the call to DirectoryExists is not really necessary, }
        { because ForceDirectories will also do the checking for us. However, }
        { checking in advance is faster. }
        if not DirectoryExists(subdir) then
          ForceDirectories(ExpandFileName(subdir));
        {$ENDIF}

        Allow := True;

        if FileExists(ExtractPath + TgtFile) then
        begin
          if Assigned(TCABFile(notif^.pv).FOnOverWriteFile) then
            TCABFile(notif^.pv).FOnOverWriteFile(TCABFile(notif^.pv),ExtractPath + StrPasN(notif^.psz1),Allow);
          if Allow then DeleteFile(ExtractPath + TgtFile);
        end;

        if Allow then
        begin
          {$IFDEF DELPHI_UNICODE}
          sa := ansistring(ExtractPath + TgtFile);
          Result := _lcreat(PAnsiChar(sa),0);
          {$ENDIF}
          {$IFNDEF DELPHI_UNICODE}
          Result := _lcreat(PChar(ExtractPath + TgtFile),0);
          {$ENDIF}
          {$IFDEF TMSDEBUG}
          OutputDebugString(pchar('create file: '+ExtractPath + TgtFile));
          {$ENDIF}
        end;

        TCABFile(notif^.pv).FCurFileNameDecompress := FileName;
        TCABFile(notif^.pv).FCurFileSizeDecompress := 0;
        TCABFile(notif^.pv).FTotFileSizeDecompress := notif^.cb;

        cf := TCABFile(notif^.pv);
      end;
    end;
  fdintCLOSE_FILE_INFO:
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring('close file');
      {$ENDIF}
      if doExtract then
      begin
        if (notif^.time = 0 ) and (notif^.date = 0 ) then
          DateTimeToSystemTime(now,SysTime)
        else
          DateTimeToSystemTime(FileDateToDateTime(makelong(word(notif^.time),word(notif^.date))),SysTime);
        SystemTimeToFileTime(SysTime,FileTime);
        LocalFileTimeToFileTime(FileTime,LFileTime);
        SetFileTime(notif^.hf, nil, nil, @LFileTime);
        _lclose(notif^.hf);
      end;
      Result:=1;
    end;
  fdintNEXT_CABINET:
    begin
     {$IFDEF TMSDEBUG}
     outputdebugstring('next cabinet');
     {$ENDIF}
    end;
  fdintENUMERATE :
    begin
     {$IFDEF TMSDEBUG}
     outputdebugstring('enumerate');
     {$ENDIF}
    end;
  end;
end;

function StdFdiOpen (pszFile : PAnsiChar; pmode : Integer): Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstringa(pansichar('open call:'+pszFile));
  {$ENDIF}

  Result := _lopen(pszfile, pmode);
end;

function StdFdiRead (hf : Integer; memory : pointer; cb : integer) : integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('read call');
  {$ENDIF}
  Result := _lread(hf,memory,cb);
end;

function StdFdiWrite (hf : Integer; memory : pointer; cb : integer) : Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('write call ');
  {$ENDIF}
  Result := _lwrite(hf, memory, cb);

  SysErr(GetLastError);
    
  if Assigned(cf) then
   begin
    cf.FCurFileSizeDecompress := cf.FCurFileSizeDecompress + cb;

    if Assigned(cf.FOnDecompressProgress) then
      cf.FOnDecompressProgress(cf,cf.FCurFileNameDecompress,cf.FCurFileSizeDecompress,cf.FTotFileSizeDecompress);
   end;
end;

function StdFdiClose (hf : Integer) : Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('close call');
  {$ENDIF}
  Result := _lclose(hf);
  SysErr(GetLastError);
end;

function StdFdiSeek (hf : Integer; dist : Longint; seektype : Integer) : Longint; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('seek call');
  {$ENDIF}
  Result := _llseek(hf, dist, seektype);
  SysErr(GetLastError);
end;

function StdFdiAlloc(cb : longint) : pointer; cdecl;
{$IFDEF TMSDEBUG}
var
  i: Integer;
{$ENDIF}
begin
  GetMem(Result, cb);
  {$IFDEF TMSDEBUG}
  i := integer(result);
  outputdebugstring(pchar('alloc call '+format('%x : %d',[i,cb])));
  {$ENDIF}
end;

function StdFdiFree(memory : pointer) : Pointer; cdecl;
{$IFDEF TMSDEBUG}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF TMSDEBUG}
  i := integer(memory);
  outputdebugstring(pchar('free call '+format('%x',[i])));
  {$ENDIF}
  FreeMem(memory);
  Result:=nil;
end;

function StdFciOpen(pszFile : PAnsiChar; oflag : Integer; pmode : Integer;
  err : PInteger; pv : Pointer) : Integer; cdecl;
var
  //ReOpenBuff:TOFSTRUCT;
  SecAtrrs: TSecurityAttributes;
  fname: string;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('open call');
  if strpas(pszFile)='' then outputdebugstring('empty file name !!');
  outputdebugstringa(pansichar(pszFile));
  {$ENDIF}

  if (oflag and O_CREAT = O_CREAT) then
  begin
    FillChar(SecAtrrs, SizeOf(SecAtrrs), #0);
    SecAtrrs.nLength := SizeOf(SecAtrrs);
    SecAtrrs.lpSecurityDescriptor := nil;
    SecAtrrs.bInheritHandle := True;

    fname := StrPasN(pszfile);

    Result := CreateFile(PChar(fname),GENERIC_READ or GENERIC_WRITE,
      { access (read-write) mode }
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      { share mode } @SecAtrrs,                             { pointer to security attributes }
      OPEN_ALWAYS,                           { how to create }
      FILE_ATTRIBUTE_TEMPORARY,              { file attributes }
      0);

    //Result := OpenFile(pszFile,ReOpenBuff,(oflag and $3) or OF_CREATE);
  end
  else
  begin
    Result := _lopen(pszFile,oflag);
  end;

  Err^ := 0;
end;

function StdFciRead(hf : Integer; memory : pointer; cb : integer; err:pinteger; pv: Pointer) : integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('read call '+inttostr(hf)+':'+inttostr(cb)));
  {$ENDIF}
  Result := _lread(hf,memory,cb);
  if (Result = integer(HFILE_ERROR)) then
    SysErr(GetLastError);
  Err^ := 0;
end;

function StdFciWrite(hf : Integer; memory : pointer; cb : integer; err:pinteger; pv: Pointer) : integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('write call '+inttostr(hf)+':'+inttostr(cb)));
  {$ENDIF}
  Result := _lwrite(hf, memory, cb);
  if (Result = integer(HFILE_ERROR)) then
    SysErr(GetLastError);
  Err^ := 0;
end;

function StdFciClose(hf : Integer; err:pinteger; pv: Pointer) : Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('close call '+inttostr(hf)));
  {$ENDIF}
  Result := _lclose(hf);
  if (Result = integer(HFILE_ERROR)) then
    SysErr(GetLastError);
  Err^ := 0;
end;

function StdFciSeek(hf : Integer; dist : Longint; seektype : Integer; err:pinteger; pv: Pointer) : Longint; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('seek call');
  {$ENDIF}
  Result := _llseek(hf, dist, seektype);
  if (Result = Integer(HFILE_ERROR)) then
    SysErr(GetLastError);
  Err^ := 0;
end;

function StdFciDelete (pszFile : PAnsiChar; err : PInteger; pv : Pointer): Integer cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('delete call');
  outputdebugstringa(pszFile);
  {$ENDIF}
  DeleteFile(StrPasN(pszFile));
  Result := 0;
  Err^ := 0;
end;

function StdFciTemp(pszTempname: PAnsiChar; cbTempName: Integer; pv: Pointer):BOOL; cdecl;
var
  buf: array[0..MAX_PATH] of ansichar;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('tempfile call');
  {$ENDIF}

  GetTempPathA(sizeof(buf),buf);
  GetTempFileNameA(buf,'CAB',0,pszTempname);
  Result := TRUE;
end;

function StdFciFileDest(pCab:PCCAB;pszFile: PAnsiChar; cbFile: Integer; fContinuation:bool; pv: Pointer): Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('file dest call');
  {$ENDIF}
  Result := 0;
end;

function StdFciGetNextCab(pCab:PCCAB;cbPrevCab: Integer;pv: Pointer): Integer; cdecl;
var
  sa: ansistring;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('getnextcab call');
  {$ENDIF}
  sa := ansistring(inttostr(pcab^.icab));
  StrCopyN(pcab^.szCab,'FOO');
  StrCatN(pcab^.szCab,pansichar(sa));
  Result := 0;
end;

function StdFciProgress(typeStatus: Integer;cb1,cb2: Integer;pv: Pointer): Integer; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('fci status call');
  {$ENDIF}

  case typeStatus of
  statusFolder:
    begin
      Result := 0;
    end;
  statusCabinet:
    begin
      Result:=0;
    end;
  statusFile:
    begin
      //if (pv <> nil) then
      begin
        cf.FTotSizeCompress := cf.FTotSizeCompress + cb1;
        cf.FTotSizeProgress := cf.FTotSizeProgress + cb2;
        cf.CompressProgress(cf.FTotSizeProgress, cf.FTotSizeFiles);
        {
        TCabFile(pv).FTotSizeCompress := TCabFile(pv).FTotSizeCompress+cb1;
        TCabFile(pv).FTotSizeProgress := TCabFile(pv).FTotSizeProgress+cb2;
        TCabFile(pv).CompressProgress(TCabFile(pv).FTotSizeProgress,TCabFile(pv).FTotSizeFiles);
        }
      end;
      Result := 0;
    end
  else
    Result := -1;
  end;
end;

function StdFciOpenInfo(pszName : PAnsiChar; var pdate : word;
  var ptime : word; var pattribs: word; err : PInteger;
  pv : Pointer) : Integer; cdecl;
var
  handle: THandle;
  finfo: TBYHANDLEFILEINFORMATION;
  filetime: TFileTime;
  attrs: DWORD;
  fname: string;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('fci open info call');
  outputdebugstringa(pansichar(pszName));
  {$ENDIF}

  fname := StrPasN(pszName);

  handle := CreateFile(PChar(fname),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,0);

  if (handle = INVALID_HANDLE_VALUE) then
  begin
    {$IFDEF TMSDEBUG}
    outputdebugstringa(pansichar(pszName));
    outputdebugstring('invalid handle');
    {$ENDIF}
    Result := -1;
    Exit;
  end;

  if (GetFileInformationByHandle(handle,finfo) = FALSE) then
  begin
    CloseHandle(handle);
    {$IFDEF TMSDEBUG}
    outputdebugstringa(pansichar(pszName));
    outputdebugstring('info fails');
    {$ENDIF}
    Result:=-1;
    Exit;
  end;

  FileTimeToLocalFileTime(finfo.ftLastWriteTime,filetime);
  FileTimeToDosDateTime(filetime,pdate,ptime);

  attrs := GetFileAttributes(pchar(fName));

  pattribs :=  (attrs  AND (FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_SYSTEM or
                            FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_ARCHIVE));
  CloseHandle(handle);
  Err^ := 0;
  Result := _lopen(pszName,OF_READ);

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('open handle = '+inttostr(result)));
  {$ENDIF}
end;

function StdFciAlloc(cb : longint) : pointer; cdecl;
{$IFDEF TMSDEBUG}
var
  i: Integer;
{$ENDIF}
begin
  GetMem(Result, cb);
  {$IFDEF TMSDEBUG}
  i := integer(result);
  outputdebugstring(pchar('alloc call '+format('%x : %d',[i,cb])));
  {$ENDIF}
end;

function StdFciFree(memory : pointer) : Pointer; cdecl;
{$IFDEF TMSDEBUG}
var
  i: Integer;
{$ENDIF}
begin
  {$IFDEF TMSDEBUG}
  i:= Integer(Memory);
  outputdebugstring(pchar('free call '+format('%x',[i])));
  {$ENDIF}
  FreeMem(memory);
  Result := nil;
end;

{ TCABFileEntry }

constructor TCABFileEntry.Create(Collection: TCollection);
begin
  inherited;
  FDateCorrect := true;
end;

destructor TCABFileEntry.Destroy;
begin
  inherited;
end;

{ TCABFile }

constructor TCABFile.Create(AOwner: TComponent);
begin
  inherited;
  FCompressionType := typMSZIP;
  FCABFileContents := TCABFileContents.Create(self);
  FExtractRelative := true;
end;

destructor TCABFile.Destroy;
begin
  FCABFileContents.Free;
  inherited;
end;

function TCABFile.ExtractAll: Boolean;
begin
  FProcessMode := pmExtractAll;
  Result := DecompressCABFile = 0;
end;

function TCABFile.ExtractSelected: Boolean;
begin
  FProcessMode := pmExtractSelected;
  Result := DecompressCABFile = 0;
end;

function TCABFile.ExtractFile(const fName: string): Boolean;
begin
  FExtractFile := FName;
  FProcessMode := pmExtractFile;
  Result := DecompressCABFile = 0;
end;

procedure TCABFile.GetContents;
begin
  doExtract := False;
  FCABFileContents.Clear;
  FProcessMode := pmContents;
  DecompressCABFile;
end;

function TCABFile.Compress: Boolean;
begin
  Result := (CompressCABFile = 0);
end;

function TCABFile.ConvertToSFX: Boolean;
var
  ptr: pointer;
  binfile: file of byte;
  reshandle: THandle;
  sfxname: string;
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  Result := False;
  if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) then
  begin
    sfxname := ChangeFileExt(cabfile,'.EXE');

    Reshandle := FindResource(hinstance,'SFX',pchar(RT_RCDATA));
    ptr := LockResource(LoadResource(hinstance,reshandle));
    Assignfile(binfile,sfxname);
    Rewrite(binfile);
    Blockwrite(binfile,ptr^,SizeOfResource(hinstance,reshandle));
    Closefile(binfile);

    ResHandle := BeginUpdateResource(pchar(sfxname),TRUE);
    AssignFile(binfile,cabfile);
    Reset(binfile);
    GetMem(ptr,filesize(binfile));
    BlockRead(binfile,ptr^,filesize(binfile));
    UpdateResource(reshandle,RT_RCDATA,'SFX',0,ptr,filesize(binfile));
    EndUpdateResource(reshandle,false);
    FreeMem(ptr);
    CloseFile(binfile);
    DeleteFile(cabfile);
    Result  := True;
  end;
end;

function TCABFile.MakeSFX: Boolean;

begin
  result := Compress;
  ConvertToSFX;
end;

procedure TCABFile.Error(err,errt: Integer);
var
  s:string;
begin
  case err of
  FCIERR_NONE: s := 'No error';
  FCIERR_OPEN_SRC:s := 'Failure opening file to be stored in cabinet';
  FCIERR_READ_SRC:s := 'Failure reading file to be stored in cabinet';
  FCIERR_ALLOC_FAIL:s := 'Insufficient memory in FCI';
  FCIERR_TEMP_FILE:s := 'Could not create a temporary file';
  FCIERR_BAD_COMPR_TYPE:s := 'Unknown compression type';
  FCIERR_CAB_FILE:s := 'Could not create cabinet file';
  FCIERR_USER_ABORT:s := 'Client requested abort';
  FCIERR_MCI_FAIL:s := 'Failure compressing data';
  else s := 'Unknown error';
  end;
  raise ECABFileError.Create(s);
end;


function TCABFile.DecompressCABFile: Integer;
var
  hfdi:thandle;
  hf: Integer;
  IsCab:bool;
  cabinetDLL:THandle;
  cablib:thandle;
  cabname,cabpath:ansistring;
  erf:terf;
  FDICABINETINFO:TFDICABINETINFO;

  _FDICreate:function(pfnalloc,pfnfree,pfnopen,pfnread,pfnwrite,
                      pfnclose,pfnseek: Pointer;
                      cpuType: Integer;PERF:perf): thandle; cdecl;
  _FDIDestroy:function(hfdi:thandle): bool; cdecl;
  _FDIIsCabinet:function(hfdi:thandle; hf: Integer;PFDIINFO:PFDICABINETINFO):bool; cdecl;
  _FDICopy:function(hfdi:thandle;pszCabinet,pszCabPath: PAnsiChar;flags: Integer;
                    pfnfdin,pfnfdid: Pointer;pvUser: Pointer) : bool; cdecl;

begin
  Result := -1;

  {$IFDEF DELPHI_UNICODE}
  hf := CreateFile(PWideChar(FCABfile), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  //hf := _lopen(PAnsiChar(FCABfile),OF_READ);
  {$ENDIF}

  {$IFNDEF DELPHI_UNICODE}
  hf := _lopen(pchar(FCABfile),OF_READ);
  {$ENDIF}

  if (hf <> integer(HFILE_ERROR)) then
  {$IFDEF DELPHI_UNICODE}
    CloseHandle(hf)
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
    _lclose(hf)
  {$ENDIF}
  else
    Exit;

  cablib := LoadLibrary('cabinet.dll');
  if (cablib = 0) then Exit;

  try
    cabinetdll := GetModuleHandle('cabinet.dll');


    if CabinetDLL > 0 then
    begin
      {$IFDEF DELPHI_UNICODE}
      @_FDICreate := GetProcAddress(CabinetDLL,PAnsiChar('FDICreate'));
      @_FDIIsCabinet := GetProcAddress(CabinetDLL,PAnsiChar('FDIIsCabinet'));
      @_FDICopy := GetProcAddress(CabinetDLL,PAnsiChar('FDICopy'));
      @_FDIDestroy := GetProcAddress(CabinetDLL,PAnsiChar('FDIDestroy'));
      {$ENDIF}

      {$IFNDEF DELPHI_UNICODE}
      @_FDICreate := GetProcAddress(CabinetDLL,'FDICreate');
      @_FDIIsCabinet := GetProcAddress(CabinetDLL,'FDIIsCabinet');
      @_FDICopy := GetProcAddress(CabinetDLL,'FDICopy');
      @_FDIDestroy := GetProcAddress(CabinetDLL,'FDIDestroy');
      {$ENDIF}

      hfdi := _FDICreate(@StdFdiAlloc,@StdFdiFree,
                         @StdFdiOpen,@StdFdiRead,@StdFdiWrite,@StdFdiClose,@StdFdiSeek,1,@erf);

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar(fcabfile));
      {$ENDIF}

      doExtract := True;

      {$IFDEF DELPHI_UNICODE}
      hf := CreateFile(PWideChar(FCABFile), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
      IsCab := _FDIIsCabinet(hfdi,hf,@FDICABINETINFO);
      CloseHandle(hf);
      {$ENDIF}

      {$IFNDEF DELPHI_UNICODE}
      hf := _lopen(pchar(fcabfile),OF_READ);
      IsCab := _FDIIsCabinet(hfdi,hf,@FDICABINETINFO);
      _lclose(hf);
      {$ENDIF}

      if IsCab then
      begin
        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar('cabinet size = '+inttostr(FDICABINETINFO.cbCabinet)));
        outputdebugstring(pchar('cabinet folders = '+inttostr(FDICABINETINFO.cFolders)));
        outputdebugstring(pchar('cabinet files = '+inttostr(FDICABINETINFO.cFiles)));
        {$ENDIF}

        cabname := ansistring(ExtractFileName(cabfile));
        cabpath := ansistring(ExtractFilePath(ExpandFileName(cabfile)));

        {$IFDEF TMSDEBUG}
        outputdebugstringa(pansichar(cabname));
        outputdebugstringa(pansichar(cabpath));
        {$ENDIF}

        if _FDICopy(hfdi,pansichar(cabname),pansichar(cabpath),0,@FdiNotification,nil,self) then
          Result := 0;

      end;

      if not _FDIDestroy(hfdi) then
        Result := -1;
    end;
  finally
    FreeLibrary(cablib);
  end;
end;

function GetFileSize(filename:ansistring): Integer;
var
  f:file of byte;
  fs: TFileStream;
begin
  Result := 0;
  AssignFile(f,string(filename));
  {$i-}
  Reset(f);
  {$i+}
  if IOResult = 0 then
  begin
    Result := FileSize(f);
    Closefile(f);
  end;
  if Result = 0 then
  begin
    fs := TFileStream.Create(string(fileName), fmOpenRead or fmShareDenyNone);
    try
      Result := fs.size;
    finally
      fs.free;
    end;
  end;
end;


function TCABFile.CompressCABFile: Integer;
var
  hfci: THandle;
  cabinetDLL: THandle;
  cablib: THandle;
  cabname,cabpath: ansistring;
  cmpfile,cmpname: ansistring;
  erf: terf;
  ccab: tccab;
  i,ct: Integer;

  _FCICreate:function(PERF:perf;pfnfiledest,pfnalloc,pfnfree,pfnopen,pfnread,pfnwrite,
                      pfnclose,pfnseek,pfntemp,pfndelete: Pointer;
                      pccab:PCCAB;pv: Pointer): THandle; cdecl;

  _FCIAddFile:function(hfci:thandle;pszSourceFile,pszFileName: PAnsiChar; fExecute:bool;
                       pfngetnextcab,pfnprogress,pfnopeninfo: Pointer;typeCompress: Integer):bool; cdecl;

  _FCIFlushCabinet:function(hfci:thandle;fGetNextCab:bool;pfngetnextcab,pfnprogress: Pointer):bool; cdecl;

  _FCIFlushFolder:function(hfci:thandle;pfngetnextcab,pfnprogress: Pointer):bool; cdecl;

  _FCIDestroy:function(hfci:thandle): bool; cdecl;

begin
  Result := -1;

  cablib := LoadLibrary('cabinet.dll');
  if (cablib = 0) then Exit;

  try
    cabinetdll := GetModuleHandle('cabinet.dll');

    if CabinetDLL > 0 then
    begin
      FillChar(ccab, SizeOf(ccab), 0);
      ccab.cb := CB_MAX_DISK;
      ccab.cbFolderThresh := Folder_Threshold;
      ccab.FFailOnIncompressible := FALSE;
      ccab.setID := 12345;
      ccab.iCab := 1;
      ccab.iDisk := 0;

      cabname := ansistring(ExtractFileName(FCabFile));
      cabpath := ansistring(ExtractFilePath(FCabFile));

      StrCopyN(ccab.szDisk,'CAB');
      StrCopyN(ccab.szCab, PAnsiChar(cabname));
      StrCopyN(ccab.szCabPath, PAnsiChar(cabpath));
      {$IFDEF TMSDEBUG}
      outputdebugstringa(ccab.szCab);
      outputdebugstringa(ccab.szCabPath);
      {$ENDIF}

      {$IFDEF DELPHI_UNICODE}
      @_FCICreate := GetProcAddress(CabinetDLL,PAnsiChar('FCICreate'));
      @_FCIDestroy := GetProcAddress(CabinetDLL,PAnsiChar('FCIDestroy'));
      @_FCIAddFile := GetProcAddress(CabinetDLL,PAnsiChar('FCIAddFile'));
      @_FCIFlushCabinet := GetProcAddress(CabinetDLL,PAnsiChar('FCIFlushCabinet'));
      @_FCIFlushFolder := GetProcAddress(CabinetDLL,PAnsiChar('FCIFlushFolder'));
      {$ENDIF}

      {$IFNDEF DELPHI_UNICODE}
      @_FCICreate := GetProcAddress(CabinetDLL,'FCICreate');
      @_FCIDestroy := GetProcAddress(CabinetDLL,'FCIDestroy');
      @_FCIAddFile := GetProcAddress(CabinetDLL,'FCIAddFile');
      @_FCIFlushCabinet := GetProcAddress(CabinetDLL,'FCIFlushCabinet');
      @_FCIFlushFolder := GetProcAddress(CabinetDLL,'FCIFlushFolder');
      {$ENDIF}

      hfci := _FCICreate(@erf,@StdFciFileDest,@StdFciAlloc,@StdFciFree,
                         @StdFciOpen,@StdFciRead,@StdFciWrite,@StdFciClose,
                         @StdFciSeek,@StdFciDelete,@StdFciTemp,@ccab,nil);

      cf := Self;
      FTotSizeFiles := 0;
      FTotSizeCompress := 0;
      FTotSizeProgress := 0;

      for i := 1 to FCABFileContents.Count do
      begin
        cmpfile := ansistring(Expandfilename(fCABFileContents.Items[i - 1].Name));
        FTotSizeFiles := FTotSizeFiles + GetFileSize(cmpfile);
      end;

      for i := 1 to FCABFileContents.Count do
      begin
        cmpfile := ansistring(Expandfilename(FCABFileContents.Items[i - 1].Name));

        if (FCABFileContents.Items[i - 1].RelPath <> '') then
        begin
          cmpname := ansistring(FCABFileContents.Items[i - 1].RelPath);
          if cmpname[length(cmpname)]<>'\' then
            cmpname := cmpname + '\';
          cmpname := cmpname + ansistring(ExtractFileName(FCABFileContents.Items[i - 1].name));
        end
        else
          cmpname := ansistring(ExtractFileName(FCABFileContents.Items[i - 1].name));

        ct := $01;

        case FCompressionType of
        typNone:ct := 0;
        typMSZIP:ct := $01;
        typLZX:
          begin
            case FLZXMemory of
            lzxLowest:ct := $F03;
            lzxLower :ct := $1003;
            lzxLow   :ct := $1103;
            lzxMedium:ct := $1203;
            lzxHigh  :ct := $1303;
            lzxHigher:ct := $1403;
            lzxHighest:ct := $1503;
            end;
          end;
        end;


        if not _FCIAddFile(hfci,pansichar(cmpfile),pansichar(cmpname),FALSE,
                           @StdFciGetNextCab,@StdFciProgress,@StdFciOpenInfo,ct) then
                           Error(erf.erfOper,erf.erfType);
      end;
      {$IFDEF TMSDEBUG}
      outputdebugstringa(ccab.szCab);
      outputdebugstringa(ccab.szCabPath);
      outputdebugstring('flush cabinet file');
      {$ENDIF}

      if not _FCIFlushCabinet(hfci,FALSE,@StdFciGetNextCab,@StdFciProgress) then
        Error(erf.erfOper,erf.erfType);

      _FCIDestroy(hfci);

      Result := 0;
    end;
  finally
    FreeLibrary(cablib);
  end;
end;


procedure TCABFile.CompressProgress(pos, tot: int64);
begin
  if Assigned(fOnCompressProgress) then
    FOnCompressProgress(self,pos,tot);
end;

function TCABFile.GetCompressionRatio: double;
begin
  if FTotSizeFiles <> 0 then
    Result := 100 - (FTotSizeCompress/FTotSizeFiles) * 100
  else
    Result := 0;
end;

function TCABFile.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCABFile.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCABFile.SetVersion(const Value: string);
begin

end;

{ TCABFileContents }

constructor TCABFileContents.Create(aOwner: TCABFile);
begin
  inherited Create(TCABFileEntry);
  FOwner := AOwner;
end;

function TCABFileContents.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TCABFileContents.GetItem(Index: Integer): TCABFileEntry;
begin
  Result := TCABFileEntry(inherited GetItem(Index));
end;

procedure TCABFileContents.SetItem(Index: Integer;
  Value: TCABFileEntry);
begin
  inherited SetItem(Index, Value);
end;

function TCABFileContents.Add: TCABFileEntry;
begin
  Result := TCABFileEntry(inherited Add);
end;

function TCABFileContents.Insert(index: Integer):TCABFileEntry;
begin
  Result := TCABFileEntry(inherited Add);
end;


function TCABFileContents.IsInList(s: ansistring): boolean;
var
  i: Integer;
  sa: ansistring;
begin
  Result := False;
  for i := 1 to Count do
  begin
    sa := ansistring(Items[i - 1].Name);
    if StrICompN(PAnsiChar(s),PAnsiChar(sa)) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TCABFileContents.IsSelected(s: ansistring): boolean;
var
  i: Integer;
  sa: ansistring;
begin
  Result := False;
  for i := 1 to Count do
  begin
    sa := ansistring(Items[i - 1].Name);
    if (StrICompN(PAnsiChar(s),PAnsiChar(sa)) = 0) then
    begin
      Result := Items[i - 1].Selected;
      Break;
    end;
  end;
end;

procedure TCABFileContents.SelectAll;
var
  i: Integer;
begin
  for i := 1 to Count do
    Items[i-1].Selected := True;
end;

procedure TCABFileContents.SelectNone;
var
  i: Integer;
begin
  for i := 1 to Count do
    Items[i-1].Selected := False;
end;


procedure TCABFileContents.AddFolder(FileSpec: string);
begin
  AddRelFolder(FileSpec,'');
end;

{$WARNINGS OFF}
procedure TCABFileContents.AddRelFolder(FileSpec, RelativePath: string);
var
  SR: TSearchRec;
  FP: string;
begin
  FP := ExtractFilePath(FileSpec);
  if Length(FP) > 0 then
    if FP[Length(FP)] <> '\' then
      FP := FP + '\';

  if FindFirst(FileSpec,faAnyfile,SR) = 0 then
  begin
    //if SR.Attr and faArchive = faArchive then
      if SR.Attr and faDirectory <> faDirectory then
      with Add do
      begin
        Name := FP + SR.Name;
        RelPath := RelativePath;
      end;

    while FindNext(SR) = 0 do
    begin
      //if SR.Attr and faArchive = faArchive then
      if SR.Attr and faDirectory <> faDirectory then
        with Add do
        begin
          Name := FP + SR.Name;
          RelPath := RelativePath;
        end;
    end;
    FindClose(SR);
  end;
end;


procedure TCABFileContents.AddFiles(FileSpec,RelativePath:string;SubDirectory:Boolean = False);
var
  Path,FileName: string;
  SR: TSearchRec;
begin
  Path := ExtractFilePath(FileSpec);
  if (FindFirst(FileSpec,faAnyFile,SR)=0) then
  begin
    try
      repeat
        FileName := SR.Name;
        if (FileName <> '.') and (FileName <> '..') then
        begin
          if (SR.Attr and faDirectory) = faDirectory then
          begin
            if SubDirectory then
            {$IFDEF DELPHI6_LVL}
              AddFiles(Path + FileName + '\' + ExtractFileName(FileSpec),
                IncludeTrailingPathDelimiter(RelativePath) + FileName,SubDirectory);
            {$ELSE}
              AddFiles(Path + FileName + '\' + ExtractFileName(FileSpec),
                RelativePath + FileName,SubDirectory);
            {$ENDIF}
          end
          else
          if SR.Attr and faDirectory <> faDirectory then
          begin
            with Add do
            begin
              Name := Path + SR.Name;
              RelPath := RelativePath;
            end;
          end;
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;
{$WARNINGS ON}

initialization
  cf := nil;

end.
