//Based on Networks unit by Dimka Maslov

{*******************************************************}
{               MiTeC Common Routines                   }
{               Networks Neighbourhood                  }
{                                                       }
{          Copyright (c) 1997-2019 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Networks;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.ShellAPI, WinAPI.ShlObj,
     System.Win.ComObj, WinAPI.ActiveX;
     {$ELSE}
     Windows, SysUtils, Classes, ShellAPI, ShlObj, ActiveX, ComObj;
     {$ENDIF}

type
  ComputerFound = class (Exception);
  ECannotFindNetwork = class (Exception);

  PNetworkWorkgroups = ^TNetworkWorkgroups;
  TNetworkWorkgroup = record
    Name,
    Description: string;
    Data: PItemiDList;
    Workgroups: PNetworkWorkgroups;
  end;
  TNetworkWorkgroups = array of TNetworkWorkgroup;

  TNetworkNeighborhood = class (TObject)
  private
    FData: TNetworkWorkgroups;
    function CreatePIDL(Size: Integer): PItemIDList;
    procedure DisposePIDL(ID: PItemIDList);
    function NextPIDL(IDList: PItemIDList): PItemIDList;
    function GetPIDLSize(IDList: PItemIDList): Integer;
    function CopyPIDL(IDList: PItemIDList): PItemIDList;
    procedure StripLastID(IDList: PItemIDList);
    function GetPrevPIDL(PIDL: PItemIDList): PItemIDList;
    function GetDisplayName(ShellFolder: IShellFolder; PIDL: PItemIDList): String;
    function OriginFolder:  IShellFolder;
    function OriginFolderNT: IShellFolder;
    function OriginFolderVista: IShellFolder;
    function EnumObjects(ShellFolder: IShellFolder): IEnumIDList;
    procedure ParseFolder(Folder: IShellFolder; var Items: TNetworkWorkgroups; StorePIDLs: Boolean = False);

    function GetWorkgroupByName(AName: String): TNetworkWorkgroup;
    function GetCount: Integer;
    function GetWorkgroup(AIndex: integer): TNetworkWorkgroup;

    procedure NGClear(var AData: TNetworkWorkgroup);
  public
    { The Refresh procedure searches all accessible workgroups in a local area
      network. Before calling this procedure the hourglass cursor would be switched on,
      because this procedure takes a part of time depending on a network speed and the
      count of workgroups and computers in a local area network. This procedure runs
      in an object constructor and then should be runned to refresh lists}
    procedure Refresh;

    property Count: Integer read GetCount;
    property Workgroup[AIndex: integer]: TNetworkWorkgroup read GetWorkgroup;

    { The Workgroups property contains lists of all computers in a network departed
      by workgroups. To obtain list of computers of a workgroup by its number
      (not by name) use the inherited property Objects as following:
       Objects[Index] as TNetworkWorkgroup}
    property WorkgroupByName[Name: String]: TNetworkWorkgroup read GetWorkgroupByName;

    { The FindComputer function searches a computer by its name and returns the
      workgroup name where a computer is. This function returns an empty string
      if a computer not found}
    function FindComputer(Name: String): String;

    { The ListComputers procedure copies the list of all the computers in a network
      into a TStrings object}
    procedure ListComputers(Strings: TStrings);

    { The ListNetwork procedure copies the alphbetically sorted list of all the
     workroups and computers in a local area network. The Objects property of the
     target TStrings objects is used to distinguish a workgroup from a computer.
     Workgroups have 'TObject(1)' in the corresponding item of the Objects property,
     and computers have 'nil'}
    procedure ListNetwork(Strings: TStrings);

    function Add(const S: string): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Insert(Index: Integer; const S: string);
    constructor Create;
    destructor Destroy; override;
  end;

resourcestring
  SShellLinkReadError = 'Shortcut read error';
  SShellLinkWriteError = 'Shortsut write error';
  SShellLinkLoadError = 'Cannot load shortcut %s';
  SShellLinkSaveError = 'Cannot save shortcut %s';
  SShellLinkCreateError = 'Cannot initialize shortcut interface';
  SDynArrayIndexError = 'Array %s item index is out of bounds (%d)';
  SDynArrayCountError = 'Array items count is out of bounds (%d)';
  SSharedMemoryError = 'Cannot create file mapping object';
  SCannotInitTimer = 'Cannot initialize timer';
  SPrinterIndexError = 'Printer index is out of bounds (%d)';
  SIndicesOutOfRange = 'Matrix item indices is out of bounds [%d: %d]';
  SRowIndexOutOfRange = 'Matrix row index is out of bounds (%d)';
  SColIndexOutOfRange = 'Matrix col index is out of bounds (%d)';
  SNoAdminRights = 'No admin rights to continue the program';

  SFileError = 'Error %s file %s%s';
  SFileReading = 'reading';
  SFileWriting = 'writing';
  SFileError002 = ' - file not found';
  SFileError003 = ' - path not found';
  SFileError004 = ' - cannot open file';
  SFileError005 = ' - access denied';
  SFileError014 = ' - no enough memory';
  SFileError015 = ' - cannot find specified drive';
  SFileError017 = ' - cannot move file to another drive';
  SFileError019 = ' - write protected media';
  SFileError020 = ' - cannot find specified device';
  SFileError021 = ' - device is not ready';
  SFileError022 = ' - device cannot recognize command';
  SFileError025 = ' - specified area not found';
  SFileError026 = ' - drive access denied';
  SFileError027 = ' - sector not found';
  SFileError029 = ' - device write error';
  SFileError030 = ' - device read error';
  SFileError032 = ' - file is used by another application';
  SFileError036 = ' - too many open files';
  SFileError038 = ' - end of file reached';
  SFileError039 = ' - disk full';
  SFileError050 = ' - network request not supported';
  SFileError051 = ' - remote computer is inaccessible';
  SFileError052 = ' - indentical names found on network';
  SFileError053 = ' - network path not found';
  SFileError054 = ' - network busy';
  SFileError055 = ' - network resource or device is inaccessible';
  SFileError057 = ' - network card hardware error';
  SFileError058 = ' - server unable to perform operation';
  SFileError059 = ' - network error';
  SFileError064 = ' - inaccessible network name';
  SFileError065 = ' - network access denied';
  SFileError066 = ' - network resource type incorrectly specified';
  SFileError067 = ' - network name not found';
  SFileError070 = ' - network server shut down';
  SFileError082 = ' - cannot create file or folder';
  SFileError112 = ' - no enough disk free space';
  SFileError123 = ' - file name syntax error';
  SFileError161 = ' - path incorrectly specified';
  SFileError183 = ' - file already exists';

  SCannotSetSize = 'Unable to change the size of a file';

  SUnableToCompress = 'Cannot compress data';
  SUnableToDecompress = 'Cannot decompress data';

  SCannotFindNetwork = 'Cannot find network neiborhood';


implementation

uses MiTeC_StrUtils;

function GetComputerName: String;
var
 N: Cardinal;
 Buf: array [0..MAX_COMPUTERNAME_LENGTH + 1] of Char;
begin
 N:=SizeOf(Buf)-1;
 {$IFDEF RAD9PLUS}WinAPI.Windows{$ELSE}Windows{$ENDIF}.GetComputerName(Buf, N);
 Result:=PChar(@Buf[0]);
end;

{ TNetworkNeighborhood }

function TNetworkNeighborhood.Add(const S: string): Integer;
begin
  SetLength(FData,Length(FData)+1);
  Result:=High(FData);
  FData[Result].Name:=S;
end;

procedure TNetworkNeighborhood.Clear;
var
  i: Integer;
begin
  for i:=0 to High(FData) do
    NGClear(FData[i]);
  Finalize(FData);
end;

function TNetworkNeighborhood.CopyPIDL(IDList: PItemIDList): PItemIDList;
var
 Size: Integer;
begin
 Size := GetPIDLSize(IDList);
 Result := CreatePIDL(Size);
 if Assigned(Result) then CopyMemory(Result, IDList, Size);
end;

constructor TNetworkNeighborhood.Create;
begin

end;

function TNetworkNeighborhood.CreatePIDL(Size: Integer): PItemIDList;
var
 Malloc: IMalloc;
 HR: HResult;
begin
 Result := nil;
 HR := SHGetMalloc(Malloc);
 if Failed(HR) then  Exit;
 try
  Result := Malloc.Alloc(Size);
  if Assigned(Result) then FillChar(Result^, Size, 0);
 finally
 end;
end;

procedure TNetworkNeighborhood.Delete(Index: Integer);
var
  i: Integer;
begin
  for i:=Index to High(FData)-1 do
    FData[i]:=FData[i+1];
  SetLength(FData,High(FData));
end;

destructor TNetworkNeighborhood.Destroy;
begin
  Clear;
  inherited;
end;

procedure TNetworkNeighborhood.DisposePIDL(ID: PItemIDList);
var
 Malloc: IMalloc;
begin
 if ID = nil then Exit;
 OLECheck(SHGetMalloc(Malloc));
 Malloc.Free(ID);
end;

function TNetworkNeighborhood.EnumObjects(
  ShellFolder: IShellFolder): IEnumIDList;
const
 Flags = SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN;
begin
 ShellFolder.EnumObjects(0, Flags, Result);
end;

function TNetworkNeighborhood.FindComputer;
{var
 i, j: Integer;
 List: TNetworkWorkgroup;
 S: String;}
begin
{ Result:='';
 try
  for i:=0 to High(FData) do begin
   List:=Objects[i] as TNetworkWorkgroup;
   for j:=0 to List.Count - 1 do begin
    S:=List[j];
    CleanUp(S);
    if SameText(Name, S) then begin
     Result:=Strings[i];
     raise ComputerFound.Create('');
    end;
   end;
  end;
 except
  if not (ExceptObject is ComputerFound) then raise;
 end;}
end;

function TNetworkNeighborhood.GetCount: Integer;
begin
  Result:=Length(FData);
end;

function TNetworkNeighborhood.GetDisplayName;
var
 StrRet: TStrRet;
 P: PChar;
begin
 Result := '';
 ShellFolder.GetDisplayNameOf(PIDL, SHGDN_NORMAL, StrRet);
 case StrRet.uType of
  STRRET_CSTR: Result:=string(StrRet.cStr);
  STRRET_OFFSET: begin
   P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
   SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
  end;
  STRRET_WSTR: Result := StrRet.pOleStr;
 end;
end;

function TNetworkNeighborhood.GetPIDLSize(IDList: PItemIDList): Integer;
begin
 Result := 0;
 if Assigned(IDList) then begin
  Result := SizeOf(IDList^.mkid.cb);
  while IDList^.mkid.cb <> 0 do begin
   Result := Result + IDList^.mkid.cb;
   IDList := NextPIDL(IDList);
  end;
 end;
end;

function TNetworkNeighborhood.GetPrevPIDL(PIDL: PItemIDList): PItemIDList;
var
 Temp: PItemIDList;
begin
 Temp := CopyPIDL(PIDL);
 if Assigned(Temp) then StripLastID(Temp);
 if Temp.mkid.cb <> 0 then Result:=Temp else Result:=nil;
end;

function TNetworkNeighborhood.GetWorkgroup(AIndex: integer): TNetworkWorkgroup;
begin
  Result:=FData[AIndex];
end;

function TNetworkNeighborhood.GetWorkgroupByName;
var
  i: Integer;
begin
  Zeromemory(@Result,SizeOf(Result));
  for i:=0 to High(FData) do
    if SameText(FData[i].Name,AName) then begin
       Result:=FData[i];
       Exit;
    end;
end;

procedure TNetworkNeighborhood.Insert(Index: Integer; const S: string);
var
  i: Integer;
begin
  SetLength(FData,Length(FData)+1);
  for i:=High(FData)-1 downto Index do
    FData[i+1]:=FData[i];
  FData[Index].Description:='';
  FData[Index].Name:=S;
  if Assigned(FData[Index].Data) then
    Dispose(FData[Index].Data);
end;

procedure TNetworkNeighborhood.ListComputers(Strings: TStrings);
var
  i, j: integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for i:=0 to High(FData) do
      if Assigned(FData[i].Workgroups) then
         for j:=0 to High(FData[i].Workgroups^) do
           Strings.Add(FData[i].Workgroups^[j].Name);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TNetworkNeighborhood.ListNetwork(Strings: TStrings);
{var
 List: TStringList;
 i: Integer;}
begin
 {List:=TStringList.Create;
 try
  List.AddStrings(Self);
  for i:=0 to List.Count - 1 do List.Objects[i]:=TObject(1);
  for i:=0 to Count - 1 do begin
   List.AddStrings(Objects[i] as TStrings);
  end;
  for i:=Count to List.Count - 1 do List.Objects[i]:=nil;
  List.Sort;
  Strings.Assign(List);
 finally
  List.Free;
 end;}
end;

function TNetworkNeighborhood.NextPIDL(IDList: PItemIDList): PItemIDList;
begin
 Result := IDList;
 Inc(PChar(Result), IDList^.mkid.cb);
end;

procedure TNetworkNeighborhood.NGClear(var AData: TNetworkWorkgroup);
var
  i: Integer;
begin
  with AData do begin
    if Assigned(Workgroups) then begin
      for i:=0 to High(Workgroups^) do
        if Assigned(Workgroups^[i].Workgroups) then
          NGClear(Workgroups^[i]);
      Dispose(Workgroups);
    end;
    if Assigned(Data) then
      Dispose(Data);
  end;
end;

function TNetworkNeighborhood.OriginFolder: IShellFolder;
var
 Desktop: IShellFolder;
 S: AnsiString;
 P: PWideChar;
 Len, Flags: LongWord;
 Machine, Workgroup, Network: PItemIDList;
begin
 S:={$IFDEF UNICODE}WideToAnsi{$ENDIF}('\\'+GetComputerName);
 Len:=Length(S);
 P:=StringToOleStr(S);
 Flags:=0;
 SHGetDesktopFolder(Desktop);
 Desktop.ParseDisplayName(0, nil, P, Len, Machine, Flags);
 Workgroup:=GetPrevPIDL(Machine);
 try
   Network:=GetPrevPIDL(Workgroup);
  try
    Desktop.BindToObject(Network, nil, IShellFolder, Pointer(Result));
  finally
   DisposePIDL(Network);
  end;
 finally
  DisposePIDL(Workgroup);
 end;
end;

function TNetworkNeighborhood.OriginFolderNT: IShellFolder;
var
 Desktop: IShellFolder;
 S: AnsiString; W: WideString; P: PWideChar;
 Len, Flags: LongWord;
 Machine, Workgroup, Network: PItemIDList;
 NetShell: IShellFolder;
 Enum: IEnumIDList;
 ID: PItemIDList;
begin
 S:={$IFDEF UNICODE}WideToAnsi{$ENDIF}('\\'+GetComputerName);
 Len:=Length(S);
 W:=WideString(S);
 P:=PWideChar(W);
 SHGetDesktopFolder(Desktop);
 Desktop.ParseDisplayName(0, nil, P, Len, Machine, Flags);
 Workgroup:=GetPrevPIDL(Machine);
 Network:=GetPrevPIDL(Workgroup);
 Desktop.BindToObject(Network, nil, IShellFolder, NetShell);
 Enum:=EnumObjects(NetShell);
 Enum.Next(1, ID, Flags);
 NetShell.BindToObject(ID, nil, IShellFolder, Pointer(Result));
 DisposePIDL(Network);
 DisposePIDL(Workgroup);
end;

function TNetworkNeighborhood.OriginFolderVista: IShellFolder;
var
 Desktop: IShellFolder;
 Network: PItemIDList;
begin
  SHGetSpecialFolderLocation(GetDesktopWindow,CSIDL_NETWORK,Network);
  try
    SHGetDesktopFolder(Desktop);
    Desktop.BindToObject(Network, nil, IShellFolder, Pointer(Result));
  finally
    DisposePIDL(Network);
  end;
end;

procedure TNetworkNeighborhood.ParseFolder;
var
  ID: PItemiDList;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  s,d: string;
  Index,p: Integer;
begin
  Clear;
  try
   EnumList:=EnumObjects(Folder);
   if Assigned(EnumList) then begin
     while EnumList.Next(1, ID, NumIDs) = S_OK do begin
       S:=GetDisplayName(Folder, ID);
       D:='';
       p:=Pos('(',S);
       if p>0 then begin
         D:=Copy(s,1,p-2);
         S:=Copy(S,p+1,255);
         SetLength(S,Length(S)-1);
       end;
       Index:=Add(S);
       FData[Index].Description:=D;
       if StorePIDLs then begin
         new(Items[Index].Data);
         FData[Index].Data:=ID;
       end;
     end;
   end;
  finally
  end;
end;

procedure TNetworkNeighborhood.Refresh;
var
 Network: IShellFolder;
 Workgroup: IShellFolder;
 i: Integer;
begin
 try
   if (Win32MajorVersion<4) then
    Network:=OriginFolderNT
  else if (Win32MajorVersion>5) then
    Network:=OriginFolderVista
  else
    Network:=OriginFolder;
  if (Win32MajorVersion>5) then
    Self.Add('')
  else
    ParseFolder(Network, FData, True);
  for i:=0 to Count-1 do begin
    if (Win32MajorVersion>5) then begin
      new(FData[i].Workgroups);
      ParseFolder(Network, FData[i].Workgroups^, False)
    end else begin
      Network.BindToObject(FData[i].Data, nil, IShellFolder, Workgroup);
      new(FData[i].Workgroups);
      ParseFolder(Workgroup, FData[i].Workgroups^, False);
      Workgroup:=nil;
    end;
  end;
 except
  raise ECannotFindNetwork.Create(SCannotFindNetwork);
 end;
end;

procedure TNetworkNeighborhood.StripLastID(IDList: PItemIDList);
var
 MarkerID: PItemIDList;
begin
 MarkerID := IDList;
 if Assigned(IDList) then begin
  while IDList.mkid.cb <> 0 do begin
   MarkerID := IDList;
   IDList := NextPIDL(IDList);
  end;
  MarkerID.mkid.cb := 0;
 end;
end;

end.
