{*******************************************************}
{               MiTeC Common Routines                   }
{                  PE Version Info                      }
{                                                       }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_VersionInfo;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Classes, System.SysUtils,
     {$ELSE}
     Windows, Classes, SysUtils,
     {$ENDIF}
     MiTeC_Windows;

type
  {$IFNDEF RAD6PLUS}
  TBytes = array of Byte;
  {$ENDIF}
  
  TResItem = record
    Name: string;
    Lang: Cardinal;
    CodePage: Cardinal;
    RawDataSize: Cardinal;
  end;

  TResList = array of TResItem;

  TVersionHeader = packed record
    wLength: word;
    wValueLength: word;
    wType: word;
    Key: array[0..16] of WideChar;   // 'VS_VERSION_INFO'
    Version: TVSFixedFileInfo;
  end;
  PVersionHeader = ^TVersionHeader;

  TTableHeader = record
    wLength: word;
    wValueLength: word;
    wType: Word;
  end;

  TVersionStringValue = record
    Name: string;
    Value: string;
  end;

  TTranslation = record
    Lang,
    CodePage: Word;
  end;
  PTranslation = ^TTranslation;

  TStringFileInfo = record
    Translation: TTranslation;
    Values: array of TVersionStringValue;
  end;

  TStringVersionInfo = array of TStringFileInfo;

  TStringFileInfoTables = array of TTranslation;

  TVersionNumber = record
    Major,
    Minor,
    Release,
    Build: integer;
  end;

  TFileFlags = (ffDebug, ffInfoInferred, ffPatched, ffPreRelease, ffPrivateBuild, ffSpecialBuild);
  TVersionFileFlags = set of TFileFlags;

  TVersionInformation = class(TPersistent)
  private
    FHeader: PVersionHeader;
    FMFB: Pointer;
    FSize: Cardinal;
    FFilename: string;
    FModified: Boolean;
    FFVN: TVersionNumber;
    FPVN: TVersionNumber;
    FFF: TVersionFileFlags;
    FOS,FType: Cardinal;
    FValid: Boolean;
    FVI: TStringVersionInfo;
    FRL: TResList;
    FRMF: Boolean;
    procedure Init;
    function CreateResList: boolean;
    procedure ReadData;
    procedure SetFilename(const Value: string);
    procedure SetFVN(const Value: TVersionNumber);
    procedure SetPVN(const Value: TVersionNumber);
    procedure SetFF(const Value: TVersionFileFlags);
    function GetVerItem(ATranslation: TTranslation;
      AIndex: Integer): TVersionStringValue;
    function GetTrans(AIndex: Integer): TTranslation;
    function GetTransCount: Cardinal;
    function GetValue(ATranslation: TTranslation; const AName: string): string;
    function GetItemCount(ATranslation: TTranslation): Cardinal;
    function FindTranslation(ATranslation: TTranslation): Integer;
    procedure SetResLang(const Value: TTranslation);
    procedure PadStream(AStream: TStream);
    procedure SaveVersionHeader(AStream: TStream; ALength, AValueLength, AType: Word; const Aname: string; const AValue);
    function GetProperty(const Index: Integer): string;
    procedure SetProperty(const Index: Integer; const Value: string);
    function GetTransProp: TTranslation;
    procedure SetTransProp(const Value: TTranslation);
    function GetCount: Cardinal;
    function GetItem(AIndex: Integer): TVersionStringValue;
    function GetResLang: TTranslation;
  protected
    procedure SetTranslation(AIndex: Integer; ANew: TTranslation);
    procedure SetStringFileInfoValue(ATranslation: TTranslation; const AName: string; ANewValue: string); overload;

    property TranslationCount: Cardinal read GetTransCount;
    property Translations[AIndex: Integer]: TTranslation read GetTrans;

    property StringFileInfo[ATranslation: TTranslation; AIndex: Integer]: TVersionStringValue read GetVerItem;
    property StringFileInfoItemCount[ATranslation: TTranslation]: Cardinal read GetItemCount;
    property StringFileInfoValue[ATranslation: TTranslation; const AName: string]: string read GetValue;
  public
    constructor Create; overload;
    constructor Create(AMappedFileBase: Pointer); overload;
    destructor Destroy; override;
    procedure SetStringFileInfoValue(AIndex: Integer; const AName: string; ANewValue: string); overload;
    procedure RefreshData;
    procedure SaveData;
    procedure AddItem(const AName, AValue: string);

    property FileName: string read FFilename write SetFilename;
    property Valid: Boolean read FValid;
    property Modified: Boolean read FModified;
    property ResourceLanguage: TTranslation read GetResLang write SetResLang;

    property Translation: TTranslation read GetTransProp write SetTransProp;

    property ProductVersionNumber: TVersionNumber read FPVN write SetPVN;
    property FileVersionNumber: TVersionNumber read FFVN write SetFVN;
    property FileFlags: TVersionFileFlags read FFF write SetFF;
    property FileOS: Cardinal read FOS;
    property FileType: Cardinal read FType;

    property ItemCount: Cardinal read GetCount;
    property Items[AIndex: Integer]: TVersionStringValue read GetItem;

    property CompanyName: string Index 0 read GetProperty write SetProperty;
    property FileDescription: string Index 1 read GetProperty write SetProperty;
    property FileVersion: string Index 2 read GetProperty write SetProperty;
    property InternalName: string Index 3 read GetProperty write SetProperty;
    property LegalCopyright: string Index 4 read GetProperty write SetProperty;
    property LegalTrademarks: string Index 5 read GetProperty write SetProperty;
    property OriginalFilename: string Index 6 read GetProperty write SetProperty;
    property ProductName: string Index 7 read GetProperty write SetProperty;
    property ProductVersion: string Index 8 read GetProperty write SetProperty;
    property Comments: string Index 9 read GetProperty write SetProperty;
  end;

implementation

{ TVersionInformation }

constructor TVersionInformation.Create;
begin
  FRMF:=True;
  Init;
end;

procedure TVersionInformation.AddItem(const AName, AValue: string);
begin
  SetLength(FVI[0].Values,Length(FVI[0].Values)+1);
  FVI[0].Values[High(FVI[0].Values)].Name:=AName;
  FVI[0].Values[High(FVI[0].Values)].Value:=AValue;
end;

constructor TVersionInformation.Create(AMappedFileBase: Pointer);
begin
  FMFB:=AMappedFileBase;
  FRMF:=False;
  Init;
end;

function TVersionInformation.CreateResList: Boolean;
var
  fm,fh,flh: THandle;
  ErrorMode: Word;
  Is64: Boolean;
  HeadersAddress,HeadersSize: Cardinal;
  ImageNTHeaders: PImageNtHeaders;
  OptionalHeader: {$IFDEF RAD9PLUS}PImageOptionalHeader32{$ELSE}PImageOptionalHeader{$ENDIF};
  OptionalHeader64: PImageOptionalHeader64;

function GetDataDirectory(ADirectory: Word): TImagedataDirectory;
begin
  if Is64 then
    Result:=TImageDataDirectory(OptionalHeader64.DataDirectory[ADirectory])
  else
    Result:=TImageDataDirectory(OptionalHeader.DataDirectory[ADirectory])
end;

function RvaToVa(Rva: Cardinal; Correct: boolean = True): Pointer;
var
  i: Cardinal;
  sh: PImageSectionHeader;
  d: Cardinal;
begin
  d:=0;
  if Correct then begin
    for i:=0 to ImageNTHeaders.FileHeader.NumberOfSections-1 do begin
      sh:=PImageSectionHeader(HeadersAddress+HeadersSize+i*SizeOf(TImageSectionHeader));
      if ((rva>=sh.VirtualAddress) and (rva<sh.VirtualAddress+sh.Misc.VirtualSize)) then begin
        d:=sh.VirtualAddress-sh.PointerToRawData;
        Break;
      end;
    end;
  end;
  Result:=PAnsiChar(FMFB)+Rva-d;
end;

function ResOfsToRawData(Ofs: Cardinal): Cardinal;
begin
  Result:=(Ofs and $7FFFFFFF)+Cardinal(RvaToVA(GetDataDirectory(IMAGE_DIRECTORY_ENTRY_RESOURCE).VirtualAddress));
end;

function DirectoryEntryToData(Directory: Word): Pointer;
begin
  Result:=RvaToVA(GetDataDirectory(Directory).VirtualAddress);
end;

procedure ReadSubDir(AEntry: PImageResourceDirectoryEntry; AType: Cardinal; ATypename: string);
var
  i: Integer;
  s,n: string;
  rd: PImageResourceDirectory;
  e: PImageResourceDirectoryEntry;
  de: PImageResourceDataEntry;
begin
  if AEntry^.Name and IMAGE_RESOURCE_NAME_IS_STRING<>0 then begin
    with PImageResourceDirStringU(ResOfsToRawData(AEntry^.Name))^ do
      n:=WideCharLenToString(NameString,Length);
    SetLength(n,Length(n));
  end else
    n:=IntToStr(AEntry^.Name and $FFFF);
  rd:=PImageResourceDirectory(ResOfsToRawData(AEntry.OffsetToDirectory));
  e:=Pointer(Cardinal(rd)+SizeOf(TImageResourceDirectory));
  if e=AEntry then
    Exit;
  for i:=0 to rd.NumberOfNamedEntries+rd.NumberOfIdEntries-1 do begin
    if e^.OffsetToData and IMAGE_RESOURCE_DATA_IS_DIRECTORY<>0 then
      ReadSubDir(e,AType,ATypename)
    else begin
      de:=PImageResourceDataEntry(ResOfsToRawData(e^.OffsetToData));
      if e^.Name and IMAGE_RESOURCE_NAME_IS_STRING<>0 then begin
        with PImageResourceDirStringU(ResOfsToRawData(e^.Name))^ do
          s:=WideCharLenToString(NameString,Length);
        SetLength(s,Length(s));
      end else
        s:=IntToStr(e^.Name and $FFFF);
      if AType=rtVersion then begin
        SetLength(FRL,Length(FRL)+1);
        with FRL[High(FRL)] do begin
          Name:=n;
          Lang:=StrToIntDef(s,0);
          CodePage:=de.CodePage;
          RawDataSize:=de.Size;
        end;
      end;
    end;
    Inc(e);
  end;
end;

var
  rd: PImageResourceDirectory;
  re: PImageResourceDirectoryEntry;
  i: Integer;
  s: string;
begin
  Result:=False;
  fh:=0;
  fm:=0;
  if FRMF then
    FMFB:=nil;
  flh:=0;
  try
    if not Assigned(FMFB) then begin
      fh:=CreateFile(PChar(FFilename),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
      if (FH<>INVALID_HANDLE_VALUE) then begin
        fm:=CreateFileMapping(FH,nil,PAGE_READONLY,0,0,nil);
        if (FM<>0) then
          FMFB:=MapViewOfFile(FM,FILE_MAP_READ,0,0,0);
      end;
    end;

    ErrorMode:=SetErrorMode(SEM_FailCriticalErrors or SEM_NoOpenFileErrorBox);
    try
      try flh:=LoadLibraryEx(PChar(FFilename),0,DONT_RESOLVE_DLL_REFERENCES); except end;
    finally
      SetErrorMode(ErrorMode);
    end;

    if Assigned(FMFB) then
      try
        ImageNTHeaders:=PImageNtHeaders(Integer(FMFB)+PImageDosHeader(FMFB)^._lfanew);
        OptionalHeader:={$IFDEF RAD9PLUS}PImageOptionalHeader32{$ELSE}PImageOptionalHeader{$ENDIF}(Integer(FMFB)+PImageDosHeader(FMFB)^._lfanew+sizeof(TImageNTHeaders)-SizeOf(Word));
        OptionalHeader64:=PImageOptionalHeader64(Integer(FMFB)+PImageDosHeader(FMFB)^._lfanew+sizeof(TImageNTHeaders)-SizeOf(Word));
        Is64:=ImageNtHeaders^.Magic=IMAGE_NT_OPTIONAL_HDR64_MAGIC;

        HeadersAddress:=Cardinal(ImageNtHeaders);
        if Is64 then
          HeadersSize:=sizeof(TImageNTHeaders)-SizeOf(Word)+SizeOf(TImageOptionalHeader64)
        else
          HeadersSize:=sizeof(TImageNTHeaders)-SizeOf(Word)+SizeOf({$IFDEF RAD9PLUS}TImageOptionalHeader32{$ELSE}TImageOptionalHeader{$ENDIF});

        if GetDataDirectory(IMAGE_DIRECTORY_ENTRY_RESOURCE).Size=0 then
          Exit;
        rd:=PImageResourceDirectory(DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_RESOURCE));
        re:=Pointer(Cardinal(rd)+SizeOf(TImageResourceDirectory));
        for i:=0 to rd.NumberOfNamedEntries+rd.NumberOfIdEntries-1 do begin
          if re^.Name and IMAGE_RESOURCE_NAME_IS_STRING<>0 then begin
            with PImageResourceDirStringU(ResOfsToRawData(re^.Name))^ do
              s:=WideCharLenToString(NameString,Length);
            SetLength(s,Length(s));
          end else
            s:=IntToStr(re^.Name and $FFFF);
          ReadSubDir(re,re^.Name,s);
          Inc(re);
        end;
        Result:=True;
      except
      end;
  finally
    if FRMF then begin
      if Assigned(FMFB) then
        UnmapViewOfFile(FMFB);
      if fm>0 then
        CloseHandle(fm);
      if fh>0 then
        Closehandle(fh);
    end;
    if flh>0 then
      FreeLibrary(flh);
  end;
end;

destructor TVersionInformation.Destroy;
begin
  Init;
  inherited;
end;

function TVersionInformation.FindTranslation(
  ATranslation: TTranslation): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FVI) do
    if (FVI[i].Translation.Lang=ATranslation.Lang) and (FVI[i].Translation.CodePage=ATranslation.CodePage) then begin
      Result:=i;
      Break;
    end;
end;

function TVersionInformation.GetVerItem(ATranslation: TTranslation;
  AIndex: Integer): TVersionStringValue;
var
  i: Integer;
begin
  i:=FindTranslation(ATranslation);
  if i>-1 then
    Result:=FVI[i].Values[AIndex];
end;

function TVersionInformation.GetCount: Cardinal;
begin
  Result:=0;
  if Length(FVI)=0 then
    Exit;
  Result:=Length(FVI[0].Values);
end;

function TVersionInformation.GetItem(AIndex: Integer): TVersionStringValue;
begin
  if Length(FVI)=0 then
    Exit;
  Result:=FVI[0].Values[AIndex];
end;

function TVersionInformation.GetItemCount(ATranslation: TTranslation): Cardinal;
var
  i: Integer;
begin
  Result:=0;
  i:=FindTranslation(ATranslation);
  if i>-1 then
    Result:=Length(FVI[i].Values);
end;

function TVersionInformation.GetProperty(const Index: Integer): string;
var
  s: string;
begin
  Result:='';
  if Length(FVI)=0 then
    Exit;
  case Index of
    0: s:='CompanyName';
    1: s:='FileDescription';
    2: s:='FileVersion';
    3: s:='InternalName';
    4: s:='LegalCopyright';
    5: s:='LegalTrademarks';
    6: s:='OriginalFilename';
    7: s:='ProductName';
    8: s:='ProductVersion';
    9: s:='Comments';
  end;
  Result:=GetValue(FVI[0].Translation,s);
end;

function TVersionInformation.GetResLang: TTranslation;
begin
  Result.Lang:=0;
  Result.CodePage:=0;
  if Length(FRL)=0 then
    Exit;
  Result.Lang:=FRL[0].Lang;
  Result.CodePage:=FRL[0].CodePage;
end;

function TVersionInformation.GetTrans(AIndex: Integer): TTranslation;
begin
  if Length(FVI)=0 then
    Exit;
  Result:=FVI[AIndex].Translation;
end;

function TVersionInformation.GetTransCount: Cardinal;
begin
  Result:=Length(FVI);
end;

function TVersionInformation.GetTransProp: TTranslation;
begin
  Result.Lang:=0;
  Result.CodePage:=0;
  if Length(FVI)=0 then
    Exit;
  Result:=FVI[0].Translation;
end;

function TVersionInformation.GetValue(ATranslation: TTranslation;
  const AName: string): string;
var
  i,j: Integer;
begin
  Result:='';
  i:=FindTranslation(ATranslation);
  if i>-1 then
    for j:=0 to High(FVI[i].Values) do
      if SameText(FVI[i].Values[j].Name,AName) then begin
        Result:=FVI[i].Values[j].Value;
        Break;
      end;
end;

procedure TVersionInformation.Init;
var
  i: Integer;
begin
  FHeader:=nil;
  FSize:=0;
  FValid:=False;
  FModified:=False;
  FFF:=[];
  FOS:=0;
  FType:=0;
  ZeroMemory(@FPVN,SizeOf(FPVN));
  ZeroMemory(@FFVN,SizeOf(FFVN));
  for i:=0 to High(FVI) do
    Finalize(FVI[i].Values);
  Finalize(FVI);
  Finalize(FRL);
end;

procedure TVersionInformation.PadStream(AStream: TStream);
var
  w: Word;
begin
  w:=0;
  if AStream.Position mod 4<>0 then
    AStream.Write(w,4-(AStream.Position mod 4));
end;

procedure TVersionInformation.ReadData;
var
  f,i,j,l: Integer;
  vh,p,vl,sp,n: Cardinal;
  Buf,b: TBytes;
  lcp: PTranslation;
  ms: TMemoryStream;
  sh,lh,h: TTableHeader;
  nvfi: Boolean;
  s: string;

function ReadString: string;
var
  w: Word;
  s: string;
begin
  Result:='';
  w:=1;
  while (w<>0) and (ms.Position<ms.Size) do begin
    ms.read(w,SizeOf(w));
    if w<>0 then begin
      s:={$IFNDEF UNICODE}WideCharToString{$ENDIF}(PWideChar(@w));
      SetLength(s,1);
      Result:=Result+s;
    end;
  end;
  while (w=0) and (ms.Position<ms.Size) do
    ms.read(w,SizeOf(w));
  if ms.Position<ms.Size then
    ms.Seek(-SizeOf(w),soFromCurrent);
end;  

begin
  Init;

  CreateResList;

  FSize:=GetFileVersionInfoSize(PChar(FFilename),vh);
  if (FSize=0) then
    Exit;
  FHeader:=AllocMem(FSize);
  GetFileVersionInfo(PChar(FFilename),0,FSize,FHeader);
  if FHeader^.Version.dwSignature<>$FEEF04BD then
    Exit;
  SetLength(Buf,FSize);
  Move(FHeader^,Buf[0],FSize);
  FFVN.Major:=HiWord(FHeader^.Version.dwFileVersionMS);
  FFVN.Minor:=LoWord(FHeader^.Version.dwFileVersionMS);
  FFVN.Release:=HiWord(FHeader^.Version.dwFileVersionLS);
  FFVN.Build:=LoWord(FHeader^.Version.dwFileVersionLS);
  FPVN.Major:=HiWord(FHeader^.Version.dwProductVersionMS);
  FPVN.Minor:=LoWord(FHeader^.Version.dwProductVersionMS);
  FPVN.Release:=HiWord(FHeader^.Version.dwProductVersionLS);
  FPVN.Build:=LoWord(FHeader^.Version.dwProductVersionLS);

  FOS:=FHeader^.Version.dwFileOS;
  FType:=FHeader^.Version.dwFileType;

  f:=FHeader^.Version.dwFileFlags and FHeader^.Version.dwFileFlagsMask;
  if (f and VS_FF_DEBUG)<>0 then
    FFF:=FFF+[ffDebug];
  if (f and VS_FF_INFOINFERRED)<>0 then
    FFF:=FFF+[ffInfoInferred];
  if (f and VS_FF_PATCHED)<>0 then
    FFF:=FFF+[ffPatched];
  if (f and VS_FF_PRERELEASE)<>0 then
    FFF:=FFF+[ffPreRelease];
  if (f and VS_FF_PRIVATEBUILD)<>0 then
    FFF:=FFF+[ffPrivateBuild];
  if (f and VS_FF_SPECIALBUILD)<>0 then
    FFF:=FFF+[ffSpecialBuild];

  nvfi:=True;
  if VerQueryValue(FHeader,PChar('\VarFileInfo\Translation'),Pointer(lcp),n) then begin
    SetLength(FVI,n div SizeOf(TTranslation));
    for i:=0 to High(FVI) do begin
      FVI[i].Translation:=lcp^;
      lcp:=Pointer(PAnsiChar(lcp)+SizeOf(TTranslation));
    end;
    nvfi:=False;
  end;

  p:=SizeOf(TVersionHeader);
  ms:=TMemoryStream.Create;
  try
    ms.Write(Buf[p],FHeader.wLength-p);
    ms.Position:=0;
    ms.read(sh,SizeOf(sh));
    SetLength(b,Length('StringFileInfo')*SizeOf(WideChar)+SizeOf(WideChar));
    ms.Read(b[0],Length(b));
    if not WideSameText(PWideChar(b),'StringFileInfo') then begin
      ms.Seek(sh.wLength,soFromBeginning);
      ms.read(sh,SizeOf(sh));
      ms.Seek(Length('StringFileInfo')*SizeOf(WideChar)+SizeOf(Word),soCurrent);
    end;
    i:=0;
    sp:=ms.Position;
    if Length(FVI)>i then
      Finalize(FVI[i].Values);
     ms.Seek(sp,soFromBeginning);
     while ms.Position<sh.wLength do begin
       p:=ms.Position;
       ms.read(lh,SizeOf(lh));
       SetLength(b,9*SizeOf(WideChar));
       ms.read(b[0],Length(b));
       if nvfi or (i=Length(FVI)) then begin
         SetLength(FVI,Length(FVI)+1);
         i:=High(FVI);
         s:={$IFNDEF UNICODE}WideCharToString{$ENDIF}(PWideChar(b));
         SetLength(s,8);
         FVI[i].Translation.Lang:=StrToIntDef('$'+Copy(s,1,4),0);
         FVI[i].Translation.CodePage:=StrToIntDef('$'+Copy(s,5,4),0);
       end;
       while ms.Position-p<lh.wLength do begin
         SetLength(FVI[i].Values,Length(FVI[i].Values)+1);
         j:=High(FVI[i].Values);
         ms.read(h,SizeOf(h));
         l:=ms.Position;
         FVI[i].Values[j].Name:=ReadString;//{$IFNDEF UNICODE}WideCharToString{$ENDIF}(PWideChar(b));
         l:=ms.Position-l;
         vl:=h.wLength-SizeOf(h)-l;
         vl:=vl+vl mod 4;
         SetLength(b,vl);
         FillChar(b[0],Length(b),0);
         ms.read(b[0],Length(b));
         FVI[i].Values[j].Value:={$IFNDEF UNICODE}WideCharToString{$ENDIF}(PWideChar(b));
       end;
       Inc(i);
     end;
  finally
    ms.Free;
  end;

  FValid:=True;
end;

procedure TVersionInformation.RefreshData;
begin
  ReadData;
end;

procedure TVersionInformation.SaveData;
var
  i,j: Integer;
  vr,p,p1,v: Cardinal;
  w,n: Word;
  ms,ms1: TMemoryStream;
  ws: WideString;
begin
  if not Assigned(FHeader) then
    Exit;

  w:=0;
  ms:=TMemoryStream.Create;
  try
    SaveVersionHeader(ms,0,sizeof(TVSFixedFileInfo),0,'VS_VERSION_INFO',FHeader^.Version);

    for i:=0 to High(FVI) do begin
      ms1:=TMemoryStream.Create;
      try
        SaveVersionHeader(ms1,0,0,0,IntToHex(FVI[i].Translation.Lang,4)+IntToHex(FVI[i].Translation.CodePage,4),w);
        for j:=0 to High(FVI[i].Values) do begin
          PadStream(ms1);
          p:=ms1.Position;
          ws:=FVI[i].Values[j].Value;
          if ws='' then
            ws:=#0#0;
          SaveVersionHeader(ms1,0,Length(FVI[i].Values[j].Value)+1,1,FVI[i].Values[j].Name,ws[1]);
          n:=ms1.Size-p;
          ms1.Seek(p,soFromBeginning);
          ms1.Write(n,SizeOf(n));
          ms1.Seek(0,soFromEnd);
        end;
        ms1.Seek(0,soFromBeginning);
        n:=ms1.Size;
        ms1.Write(n,sizeof(n));
        PadStream (ms);
        p:=ms.Position;
        SaveVersionHeader(ms,0,0,0,'StringFileInfo',w);
        ms.Write(ms1.Memory^,ms1.size);
        n:=ms.Size-p;
      finally
        ms1.Free
      end;
      ms.Seek(p,soFromBeginning);
      ms.Write(n,sizeof(n));
      ms.Seek(0,soFromEnd);
    end;

    if Length(FVI)>0 then begin
      PadStream(ms);
      p:=ms.Position;
      SaveVersionHeader(ms,0,0,0,'VarFileInfo',w);
      PadStream(ms);
      p1:=ms.Position;
      SaveVersionHeader(ms,0,0,0,'Translation',w);
      for i:=0 to High(FVI) do begin
        v:=MAKELONG(FVI[i].Translation.Lang,FVI[i].Translation.CodePage);
        ms.Write(v,sizeof(v));
      end;

      n:=ms.Size-p1;
      ms.Seek(p1,soFromBeginning);
      ms.Write(n,SizeOf(n));
      n:=sizeof(Integer)*Length(FVI);
      ms.Write(n,SizeOf(n));

      n:=ms.Size-p;
      ms.Seek(p,soFromBeginning);
      ms.Write(n,SizeOf(n));
    end;

    ms.Seek(0,soFromBeginning);
    n:=ms.Size;
    ms.Write(n,SizeOf(n));
    ms.Seek(0,soFromEnd);

    vr:=BeginUpdateResource(PChar(FFilename),False);
    try
      UpdateResource(vr,RT_VERSION,MAKEINTRESOURCE(VS_VERSION_INFO),FRL[0].Lang,ms.Memory,ms.Size);
    finally
      EndUpdateResource(vr,False);
    end;

  finally
    ms.Free;
  end;
  FModified:=False;
end;

procedure TVersionInformation.SaveVersionHeader(AStream: TStream; ALength,
  AValueLength, AType: Word; const Aname: string; const AValue);
var
  ws: WideString;
  vl,nl: Word;
begin
  ws:=AName;
  AStream.Write(ALength,SizeOf(ALength));
  AStream.Write(AValueLength,SizeOf(AValueLength));
  AStream.Write(AType,sizeof(AType));
  nl:=(Length(AName)+1)*sizeof(WideChar);
  AStream.Write(ws[1],nl);
  PadStream(AStream);
  if AValueLength>0 then begin
    vl:=AValueLength;
    if AType=1 then
      vl:=vl*sizeof(WideChar);
    AStream.Write(AValue,vl);
  end;
end;

procedure TVersionInformation.SetFF(const Value: TVersionFileFlags);
var
  f: Cardinal;
begin
  if not Assigned(FHeader) then
    Exit;

  FFF:=Value;

  f:=0;
  if ffDebug in Value then
    f:=f or VS_FF_DEBUG;
  if ffInfoInferred in Value then
    f:=f or VS_FF_INFOINFERRED;
  if ffPatched in Value then
    f:=f or VS_FF_PATCHED;
  if ffPreRelease in Value then
    f:=f or VS_FF_PRERELEASE;
  if ffPrivateBuild in Value then
    f:=f or VS_FF_PRIVATEBUILD;
  if ffSpecialBuild in Value then
    f:=f or VS_FF_SPECIALBUILD;

  if (FHeader^.Version.dwFileFlags and FHeader^.Version.dwFileFlagsMask)<>f then
    FHeader^.Version.dwFileFlags:=(FHeader^.Version.dwFileFlags and not FHeader^.Version.dwFileFlagsMask) or f;

  FModified:=True;
end;

procedure TVersionInformation.SetFilename(const Value: string);
begin
  if SameText(FFilename,Value) then
    Exit;
  FFilename:=Value;
  ReadData;
end;

procedure TVersionInformation.SetFVN(const Value: TVersionNumber);
begin
  if not Assigned(FHeader) then
    Exit;

  FFVN:=Value;

  FHeader^.Version.dwFileVersionMS:=MAKELONG(FFVN.Minor,FFVN.Major);
  FHeader^.Version.dwFileVersionLS:=MAKELONG(FFVN.Build,FFVN.Release);

  FModified:=True;
end;

procedure TVersionInformation.SetProperty(const Index: Integer;
  const Value: string);
var
  s: string;
begin
  if Length(FVI)=0 then
    Exit;
  case Index of
    0: s:='CompanyName';
    1: s:='FileDescription';
    2: s:='FileVersion';
    3: s:='InternalName';
    4: s:='LegalCopyright';
    5: s:='LegalTrademarks';
    6: s:='OriginalFilename';
    7: s:='ProductName';
    8: s:='ProductVersion';
    9: s:='Comments';
  end;
  SetStringFileInfoValue(0,s,Value);
  FModified:=True;
end;

procedure TVersionInformation.SetPVN(const Value: TVersionNumber);
begin
  if not Assigned(FHeader) then
    Exit;

  FPVN:=Value;

  FHeader^.Version.dwProductVersionMS:=MAKELONG(FPVN.Minor,FPVN.Major);
  FHeader^.Version.dwProductVersionLS:=MAKELONG(FPVN.Build,FPVN.Release);

  FModified:=True;
end;

procedure TVersionInformation.SetResLang(const Value: TTranslation);
begin
  if Length(FRL)=0 then
    Exit;
  FRL[0].Lang:=Value.Lang;
  FRL[0].CodePage:=Value.CodePage;
  FModified:=True;
end;

procedure TVersionInformation.SetStringFileInfoValue(AIndex: Integer;
  const AName: string; ANewValue: string);
var
  i: Integer;
  f: Boolean;
begin
  if Length(FVI)=0 then
    Exit;
  f:=False;
  for i:=0 to High(FVI[AIndex].Values) do
    if SameText(FVI[AIndex].Values[i].Name,AName) then begin
      FVI[AIndex].Values[i].Value:=ANewValue;
      FModified:=True;
      f:=True;
      Break;
    end;
  if not f and (ANewValue<>'') then begin
    SetLength(FVI[AIndex].Values,Length(FVI[AIndex].Values)+1);
    FVI[AIndex].Values[High(FVI[AIndex].Values)].Name:=AName;
    FVI[AIndex].Values[High(FVI[AIndex].Values)].Value:=ANewValue;
    FModified:=True;
  end;
end;

procedure TVersionInformation.SetStringFileInfoValue(ATranslation: TTranslation;
  const AName: string; ANewValue: string);
var
  i,j: Integer;
begin
  if Length(FVI)=0 then
    Exit;
  i:=FindTranslation(ATranslation);
  if i>-1 then
    for j:=0 to High(FVI[i].Values) do
      if SameText(FVI[i].Values[j].Name,AName) then begin
        FVI[i].Values[j].Value:=ANewValue;
        FModified:=True;
        Break;
      end;
end;

procedure TVersionInformation.SetTranslation(AIndex: Integer;
  ANew: TTranslation);
begin
  if Length(FVI)=0 then
    Exit;
  FVI[AIndex].Translation:=ANew;
  FModified:=True;
end;

procedure TVersionInformation.SetTransProp(const Value: TTranslation);
begin
  if Length(FVI)=0 then
    Exit;
  FVI[0].Translation:=Value;
  FModified:=True;
end;

end.
