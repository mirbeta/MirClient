{**************************************************************************}
{ CAB FDI procs                                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{           copyright © 1999 - 2012                                        }
{           Email : info@tmssoftware.com                                   }
{           Web : http://www.tmssoftware.com                               }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit cabfdi;

{$I TMSDEFS.INC}

{$DEFINE noTMSDEBUG}
{$R-}

interface

uses
  Windows, SysUtils;

const
    cabdll = 'cabinet.dll';

    FDIERROR_NONE = 0;
    FDIERROR_CABINET_NOT_FOUND = 1;
    FDIERROR_NOT_A_CABINET = 2;
    FDIERROR_UNKNOWN_CABINET_VERSION = 3;
    FDIERROR_CORRUPT_CABINET = 4;
    FDIERROR_ALLOC_FAIL = 5;
    FDIERROR_BAD_COMPR_TYPE = 6;
    FDIERROR_MDI_FAIL = 7;
    FDIERROR_TARGET_FILE = 8;
    FDIERROR_RESERVE_MISMATCH = 9;
    FDIERROR_WRONG_CABINET = 10;
    FDIERROR_USER_ABORT = 11;
    fdintCABINET_INFO = 0;              // General information about cabinet
    fdintPARTIAL_FILE = 1;              // First file in cabinet is continuation
    fdintCOPY_FILE = 2;                 // File to be copied
    fdintCLOSE_FILE_INFO = 3;           // close the file, set relevant info
    fdintNEXT_CABINET = 4;              // File continued to next cabinet
    fdintENUMERATE = 5;                 // Enumeration status


type
  perf = ^terf;
  terf = record
          erfOper,erfType:integer;
          fError:longbool;
         end;

  PFDICABINETINFO = ^TFDICABINETINFO;
  TFDICABINETINFO = record
        cbCabinet:longint;              // Total length of cabinet file
        cFolders:smallint;              // Count of folders in cabinet
        cFiles:smallint;                // Count of files in cabinet
        setID:smallint;                 // Cabinet set ID
        iCabinet:smallint;              // Cabinet number in set (0 based)
        fReserve:integer;               // TRUE => RESERVE present in cabinet
        hasprev:integer;                // TRUE => Cabinet is chained prev
        hasnext:integer;                // TRUE => Cabinet is chained next
       end;

  PFDINOTIFICATION = ^TFDINOTIFICATION;
  TFDINOTIFICATION = record
    cb: longint;
    psz1: pansichar;
    psz2: pansichar;
    psz3: pansichar;                   // Points to a 256 character buffer
    pv: pointer;                       // Value for client
    hf: cardinal;
    date: smallint;
    time: smallint;
    attribs: smallint;
    setID: smallint;                   // Cabinet set ID
    iCabinet: smallint;                // Cabinet number (0-based)
    iFolder: smallint;                 // Folder number (0-based)
    FDIERROR: integer;
  end;



function CabExtract(cabfile,targetdir:string):integer;

implementation


var
  doExtract:boolean;
  doRelative:boolean;
  doExtractPath:string;


function ExpandFileName(const FileName: pchar): string;
var
  FName: PChar;
  buffer: string;
  i: integer;
begin
  SetLength(buffer,MAX_PATH);
  i := GetFullPathName(FileName, MAX_PATH, PChar(Buffer), FName);
  SetLength(buffer,i);
  Result := buffer;
end;

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end;

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

function ExtractFileDir( FileName: string): string;
begin
  Result := '';
  while pos('\',filename)>0 do
  begin
    Result := Result + Copy(filename,1,pos('\',filename));
    system.Delete(filename,1,pos('\',filename));
  end;
  // remove trailing backslash
  if (Length(Result) > 0) then
    if (Result[length(Result)] = '\') then
      Result := Copy(Result,1,length(Result) - 1);
end;

function ExtractFileName( FileName: string): string;
begin
  while pos('\',filename)>0 do delete(filename,1,pos('\',filename));
  result:=filename;
end;

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := Integer(GetFileAttributes(PChar(Name)));
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


function FdiNotification(fdint:integer; notif :PFDINOTIFICATION):dword; cdecl;
var
  FileTime,LFileTime:TFileTime;
  SysTime: TSystemTime;
  subdir:string;
  sa: ansistring;
  s: string;

begin
  Result := 0;

  case fdint of
    fdintCABINET_INFO : begin
        {$IFDEF TMSDEBUG}
        outputdebugstring('cabinet info');
        {$ENDIF}
        end;
    fdintPARTIAL_FILE : begin
        {$IFDEF TMSDEBUG}
        outputdebugstring('partial file');
        {$ENDIF}
        end;
    fdintCOPY_FILE : begin

        {$IFDEF TMSDEBUG}
        outputdebugstring('copy file:');
        outputdebugstringa(pansichar(notif^.psz1));
        {$ENDIF}

        if doExtract then
         begin
          sa := ansistring(notif^.psz1);
          s := string(sa);
          if doRelative then
           subdir := extractfiledir(expandfilename(pchar(s)))
          else
           subdir := doExtractPath + ExtractFileDir(pchar(s));

          if not DirectoryExists(subdir) then
          begin
            ForceDir(subdir);
            //CreateDirectory(PChar(subdir), nil);
          end;

          sa := ansistring(DoExtractPath + s);
          result := _lcreat(pansichar(sa),0);
         end
        else
          result := 0; {skip}

      end;
    fdintCLOSE_FILE_INFO :
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

          //DosDateTimeToFileTime(Word(notif^.Date), Word(notif^.Time), FileTime);
          //SetFileTime(notif^.hf, nil, nil, @FileTime);

         _lclose(notif^.hf);
        end;

       Result:=1;
      end;
    fdintNEXT_CABINET :
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

function StdFdiOpen (pszFile : PAnsiChar; pmode : Integer): cardinal; cdecl;
{$IFDEF DELPHI_UNICODE}
var
  sa: ansistring;
{$ENDIF}
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('open call');
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  sa := ansistring(pszFile);
  Result:=_lopen(pansichar(sa), pmode);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Result:=_lopen(pszFile, pmode);
  {$ENDIF}
end;

function StdFdiRead (hf : cardinal; memory : pointer; cb : integer) : cardinal; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('read call');
  {$ENDIF}
  Result:=_lread(hf,memory,cb);
end;

function StdFdiWrite (hf : cardinal; memory : pointer; cb : integer) : cardinal; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('write call');
  {$ENDIF}
  Result:=_lwrite(hf, memory, cb);
end;

function StdFdiClose (hf : cardinal) : cardinal; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('close call');
  {$ENDIF}
  Result:=_lclose(hf);
end;

function StdFdiSeek (hf : cardinal; dist : cardinal; seektype : Integer) : cardinal; cdecl;
begin
  {$IFDEF TMSDEBUG}
  outputdebugstring('seek call');
  {$ENDIF}
  Result:=_llseek(hf, dist, seektype);
end;

function StdFdiAlloc (cb : longint) : pointer; cdecl;
begin
 GetMem(Result, cb);
end;

function StdFdiFree (memory : pointer) : Pointer; cdecl;
begin
  FreeMem(memory);
  Result:=nil;
end;

function CabExtract(cabfile,targetdir:string):integer;
var
  hfdi: THandle;
  hf: Integer;
  IsCab: bool;
  cabinetDLL: THandle;
  cablib: THandle;
  cabname, cabpath:ansistring;
  erf:terf;
  {$IFDEF DELPHI_UNICODE}
  sa: ansistring;
  {$ENDIF}
  FDICABINETINFO:TFDICABINETINFO;

  _FDICreate:function(pfnalloc,pfnfree,pfnopen,pfnread,pfnwrite,
                      pfnclose,pfnseek:pointer;
                      cpuType:integer;PERF:perf): thandle; cdecl;
  _FDIDestroy:function(hfdi:thandle): bool; cdecl;
  _FDIIsCabinet:function(hfdi:thandle; hf:integer;PFDIINFO:PFDICABINETINFO):bool; cdecl;
  _FDICopy:function(hfdi:THandle;pszCabinet,pszCabPath:pansichar;flags:integer;
                    pfnfdin,pfnfdid:pointer;pvUser:pointer) : bool; cdecl;

begin
  Result := -1;

  {$IFDEF DELPHI_UNICODE}
  sa := ansistring(cabfile);
  hf :=_lopen(pansichar(sa),OF_READ);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  hf :=_lopen(pchar(cabfile),OF_READ);
  {$ENDIF}
  
  if (hf <> integer(HFILE_ERROR)) then
    _lclose(hf)
  else
    Exit;

  cablib := LoadLibrary(cabdll);
  if (cablib = 0) then
    Exit;

  cabinetdll := GetModuleHandle(cabdll);

  if (CabinetDLL > 0) then
  begin
    @_FDICreate := GetProcAddress(CabinetDLL,PAnsiChar('FDICreate'));
    @_FDIIsCabinet := GetProcAddress(CabinetDLL,PAnsiChar('FDIIsCabinet'));
    @_FDICopy := GetProcAddress(CabinetDLL,PAnsiChar('FDICopy'));
    @_FDIDestroy := GetProcAddress(CabinetDLL,PAnsiChar('FDIDestroy'));

    doExtractPath := targetdir;

    hfdi := _FDICreate(@StdFdiAlloc,@StdFdiFree,
                     @StdFdiOpen,@StdFdiRead,@StdFdiWrite,@StdFdiClose,@StdFdiSeek,1,@erf);

    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar(cabfile));
    {$ENDIF}

    {$IFDEF DELPHI_UNICODE}
    sa := ansistring(cabfile);
    hf :=_lopen(pansichar(sa),OF_READ);
    {$ENDIF}
    {$IFNDEF DELPHI_UNICODE}
    hf :=_lopen(pchar(cabfile),OF_READ);
    {$ENDIF}
    
    IsCab :=_FDIIsCabinet(hfdi,hf,@FDICABINETINFO);
    _lclose(hf);

    if IsCab then
    begin
      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('cabinet size = '+inttostr(FDICABINETINFO.cbCabinet)));
      outputdebugstring(pchar('cabinet folders = '+inttostr(FDICABINETINFO.cFolders)));
      outputdebugstring(pchar('cabinet files = '+inttostr(FDICABINETINFO.cFolders)));
      {$ENDIF}

      doExtract := True;
      doRelative := Targetdir='';

      cabname := ansistring(ExtractFileName(cabfile));
      cabpath := ansistring(ExtractFileDir(expandfilename(pchar(cabfile)))+'\');

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar(cabname));
      outputdebugstring(pchar(cabpath));
      {$ENDIF}

      if _FDICopy(hfdi,pansichar(cabname),pansichar(cabpath),0,@FdiNotification,nil,nil) then
        Result := 0
      else
        Result := -1;
   end;

   {$IFDEF TMSDEBUG}
   if _FDIDestroy(hfdi) then
   begin
     Result := 0;
     outputdebugstring(' destroy ok ')
   end
   else
   begin
     Result := -1;
     outputdebugstring('destroy fail');
   end;
   {$ELSE}
   if _FDIDestroy(hfdi) and (result=0) then
     Result := 0
   else
     Result := -1;
   {$ENDIF}
  end
  else
    Result:=-1;

  FreeLibrary(cablib);
end;




end.
