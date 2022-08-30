{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Direct Memory Access                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_DMA;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_Windows;


type
  PMemoryBuffer = ^TMemoryBuffer;
  TMemoryBuffer = array[0..65535] of AnsiChar;
  TArrayBuffer = array[0..254] of AnsiChar;

  TMiTeC_DMA = class(TMiTeC_Component)
  private
    FPS: Cardinal;
    FMemory: PAnsiChar;
    FStartAddress: int64;
    FSize: Cardinal;
    FEndAddress: int64;
    function GetAddressByteValue(Address: int64): Byte;
    function GetAddressDWORDValue(Address: int64): Cardinal;
    function GetAddressWordValue(Address: int64): Word;
    function GetAddressArrayValue(Address: int64; Length: Byte): TArrayBuffer;
    function GetAddressCharValue(Address: int64): AnsiChar;
    function GetAddressString(Address: int64; Index: Byte): string;
    function GetAddressQWORDValue(Address: int64): int64;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    destructor Destroy; override;
    procedure SaveToFile(const AFileName: string; ASignature: TBytes);
    procedure SaveToStream(S: TStream; ASignature: TBytes);

    procedure LoadFromStream(S: TStream);
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromMemory(AMemory: PAnsiChar; AStart,ASize: Cardinal);

    property CharValue[Address: Int64]: AnsiChar read GetAddressCharValue;
    property ByteValue[Address: Int64]: Byte read GetAddressByteValue;
    property WordValue[Address: int64]: Word read GetAddressWordValue;
    property DWORDValue[Address: int64]: Cardinal read GetAddressDWORDValue;
    property QWORDValue[Address: int64]: int64 read GetAddressQWORDValue;
    property ArrayValue[Address: int64; Length: Byte]: TArrayBuffer read GetAddressArrayValue;
    property StringValue[Address: int64; Index: Byte]: string read GetAddressString;

    function IsValidAddress(A: int64): Boolean;
    function FindSequence(StartAddr: int64; Sequence: string): Cardinal;

    property Memory: PAnsiChar read FMemory;
  published
    property StartAddress: int64 read FStartAddress write FStartAddress;
    property EndAddress: int64 read FEndAddress write FEndAddress;
    property MemorySize: Cardinal read FSize write FSize;
  end;

resourcestring
  rsCannotMap = 'Cannot map physical memory to process memory!';
  rsNotAdmin = 'Cannot access physical memory!'#13#10+
              '(You should be a member of local administrators)';

implementation

uses MiTeC_Routines, MiTeC_NativeAPI, MiTeC_NativeDefs;

{ TMiTeC_DMA }

procedure TMiTeC_DMA.SaveToFile(const AFileName: string; ASignature: TBytes);
var
  fs: TFileStream;
begin
  if not Assigned(FMemory) then
    Exit;
  fs:=TFileStream.Create(AFilename,fmCreate or fmShareExclusive);
  try
    if Length(ASignature)>0 then
      fs.WriteBuffer(ASignature[0],Length(ASignature));
    fs.WriteBuffer(FMemory[0],FSize);
  finally
    fs.Free;
  end;
end;

procedure TMiTeC_DMA.SaveToStream(S: TStream; ASignature: TBytes);
begin
  if not Assigned(FMemory) then
    Exit;
  S.Size:=0;
  S.Position:=0;
  if Length(ASignature)>0 then
    S.WriteBuffer(ASignature[0],Length(ASignature));
  S.WriteBuffer(FMemory[0],FSize);
end;

procedure TMiTeC_DMA.LoadFromStream(S: TStream);
var
  i: Cardinal;
begin
  i:=S.Size;
  FillChar(FMemory^,Length(FMemory),0);
  ReAllocMem(FMemory,i);
  S.ReadBuffer(FMemory[0],i);
  FStartAddress:=0;
  FSize:=i;
  SetDataAvail(True);
end;

procedure TMiTeC_DMA.LoadFromFile(const AFileName: string);
var
  fs: TStream;
begin
  fs:=TFileStream.Create(AFilename,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
    SetDataAvail(True);
  finally
    fs.Free;
  end;
end;

destructor TMiTeC_DMA.Destroy;
begin
  Reallocmem(FMemory,0);
  inherited;
end;

procedure TMiTeC_DMA.RefreshData;
const
  ObjPhysMem = '\Device\PhysicalMemory';
  ObjectName: TUnicodeString = (
    Length       : Length(ObjPhysMem) * SizeOf(WChar);
    MaximumLength: Length(ObjPhysMem) * SizeOf(WChar) + SizeOf(WChar);
    Buffer       : ObjPhysMem;
  );
  DesiredAccess: ACCESS_MASK = SECTION_MAP_READ;
  ObjectAttribs: TOBJECTATTRIBUTES =(
    Length                  : SizeOf(TOBJECTATTRIBUTES);
    RootDirectory           : 0;
    ObjectName              : @ObjectName;
    Attributes              : OBJ_CASE_INSENSITIVE;
    SecurityDescriptor      : nil;
    SecurityQualityOfService: nil;
  );


var
  SectionHandle: THandle;
  ViewOfPhysMem: Pointer;
  BlockStart: Cardinal;
  ok: Boolean;
begin
  Clear;
  ok:=False;
  ReallocMem(FMemory,FSize);
  InitNativeAPI;
  if (NtOpenSection(@SectionHandle,DesiredAccess,@ObjectAttribs)=STATUS_SUCCESS) then
    try
      BlockStart:=FStartAddress-(FStartAddress mod FPS);
      ViewOfPhysMem:=MapViewOfFile(SectionHandle,DesiredAccess,0,FStartAddress,FSize+FStartAddress-BlockStart);
      if Assigned(ViewOfPhysMem) then
        try
          ZeroMemory(FMemory,FSize);
          if FStartAddress<BlockStart then
            Move(PAnsiChar(ViewOfPhysMem)[FStartAddress],FMemory[0],FSize)
          else
            Move(PAnsiChar(ViewOfPhysMem)[FStartAddress-BlockStart],FMemory[0],FSize);
          //Move(ViewOfPhysMem^,FMemory[0],FSize);
          ok:=True;
        finally
          UnmapViewOfFile(ViewOfPhysMem);
        end;
    finally
      NtClose(SectionHandle);
    end;
  SetDataAvail(ok);
  if not ok then begin
    FMemory:=nil;
  end;
end;

procedure TMiTeC_DMA.Clear;
begin
  FMemory:=nil;
  FStartAddress:=0;
  FEndAddress:=0;
  FSize:=0;
end;

constructor TMiTeC_DMA.Create;
var
  SI: TSystemInfo;
begin
  inherited Create(AOwner);
  FillChar(SI,SizeOf(SI),0);
  GetSystemInfo(SI);
  FPS:=SI.dwPageSize;
end;

function TMiTeC_DMA.GetAddressArrayValue;
begin
  try
    if Address+Length>FStartAddress+FSize then
      Length:=FStartAddress+FSize-Address;
    if (Address>=FStartAddress) and (Address<=FStartAddress+FSize) then
      Move(FMemory[Address-FStartAddress],Result[0],Length)
    else
      FillChar(Result,SizeOf(Result),0);
  except
    FillChar(Result,SizeOf(Result),0);
  end;
end;

function TMiTeC_DMA.GetAddressByteValue(Address: int64): Byte;
begin
  Result:=0;
  if not Assigned(FMemory) then
    Exit;
  try
    if (Address>=FStartAddress) and (Address<=FStartAddress+FSize) then
      Result:=Ord(FMemory[Address-FStartAddress])
    else
      Result:=0;
  except
    Result:=0;
  end;
end;

function TMiTeC_DMA.GetAddressDWORDValue(Address: int64): Cardinal;
begin
  try
    if (Address>=FStartAddress) and (Address<=(FStartAddress+FSize)-SizeOf(Cardinal)) then
      Move(Fmemory[Address-FStartAddress],Result,SizeOf(Cardinal))
    else
      Result:=0;
  except
    Result:=0;
  end;
end;

function TMiTeC_DMA.GetAddressQWORDValue(Address: int64): int64;
begin
  try
    if (Address>=FStartAddress) and (Address<=(FStartAddress+FSize)-SizeOf(int64)) then
      Move(Fmemory[Address-FStartAddress],Result,SizeOf(Int64))
    else
      Result:=0;
  except
    Result:=0;
  end;
end;

function TMiTeC_DMA.GetAddressWordValue(Address: int64): Word;
begin
  try
    if (Address>=FStartAddress) and ((Address+SizeOf(WORD))<=(FStartAddress+FSize)) then
      Move(Fmemory[Address-FStartAddress],Result,SizeOf(WORD))
    else
      Result:=0;
  except
    Result:=0;
  end
end;

function TMiTeC_DMA.FindSequence(StartAddr: int64; Sequence: string): Cardinal;
var
  i,j,l: Integer;
  Buffer: TArrayBuffer;
  s: string;
  c: AnsiChar;
begin
  if not IsValidAddress(StartAddr) then
    StartAddr:=FStartAddress;
  Result:=0;
  Sequence:=UpperCase(Sequence);
  l:=Length(Sequence) div 2;
  c:=AnsiChar(Chr(StrToInt('$'+Copy(Sequence,1,2))));
  for i:=StartAddr to FStartAddress+FSize do begin
    if (FMemory[i]=c) then begin
      FillChar(Buffer,SizeOf(Buffer),0);
      Buffer:=ArrayValue[i,l];
      s:='';
      for j:=0 to l-1 do
        s:=s+IntToHex(Ord(Buffer[j]),2);
      if s=Sequence then begin
        Result:=i;
        Break;
      end;
    end;
  end;
end;

function TMiTeC_DMA.IsValidAddress(A: int64): Boolean;
begin
  Result:=(A>=FStartAddress) and (A<=FStartAddress+FSize);
end;

function TMiTeC_DMA.GetAddressCharValue(Address: int64): AnsiChar;
begin
  try
    if (Address>=FStartAddress) and (Address<=FStartAddress+FSize) then
      Result:=FMemory[Address-FStartAddress]
    else
      Result:=#0;
  except
    Result:=#0;
  end;
end;

function TMiTeC_DMA.GetAddressString(Address: int64; Index: Byte): string;
var
  i,l: Cardinal;
  s: string;
begin
  try
    s:='';
    l:=0;
    try
      for i:=1 to Index do begin
        s:=string(ArrayValue[Address+l,255]);
        l:=l+Cardinal(Length(s))+1;
        if s='' then
          Break;
      end;
      Result:=Trim(s);
    except
      s:='';
    end;
    Result:=s;
  except
    Result:='';
  end;
end;

procedure TMiTeC_DMA.LoadFromMemory;
begin
  ReAllocMem(FMemory,ASize);
  ZeroMemory(Fmemory,ASize);
  Move(AMemory[AStart],FMemory[0],ASize);
  SetDataAvail(True);
end;

end.

