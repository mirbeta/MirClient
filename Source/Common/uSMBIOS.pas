unit uSMBIOS;

interface

uses
  SysUtils, Windows, Classes;

type
  SMBiosTables = (
    BIOSInformation = 0,
    SystemInformation = 1,
    BaseBoardInformation = 2,
    EnclosureInformation = 3,
    ProcessorInformation = 4,
    MemoryControllerInformation = 5,
    MemoryModuleInformation = 6,
    CacheInformation = 7,
    PortConnectorInformation = 8,
    SystemSlotsInformation = 9,
    OnBoardDevicesInformation = 10,
    OEMStrings = 11,
    SystemConfigurationOptions = 12,
    BIOSLanguageInformation = 13,
    GroupAssociations = 14,
    SystemEventLog = 15,
    PhysicalMemoryArray = 16,
    MemoryDevice = 17,
    MemoryErrorInformation = 18,
    MemoryArrayMappedAddress = 19,
    MemoryDeviceMappedAddress = 20,
    EndofTable = 127);

  TSmBiosTableHeader = packed record
    TableType: Byte;
    Length: Byte;
    Handle: Word;
  end;

  TBiosInfo = packed record
    Header: TSmBiosTableHeader;
    Vendor: Byte;
    Version: Byte;
    StartingSegment: Word;
    ReleaseDate: Byte;
    BiosRomSize: Byte;
    Characteristics: Int64;
    ExtensionBytes: array[0..1] of Byte;
  end;

  TSysInfo = packed record
    Header: TSmBiosTableHeader;
    Manufacturer: Byte;
    ProductName: Byte;
    Version: Byte;
    SerialNumber: Byte;
    UUID: array[0..15] of Byte;
    WakeUpType: Byte;
  end;

  TBaseBoardInfo = packed record
    Header: TSmBiosTableHeader;
    Manufacturer: Byte;
    Product: Byte;
    Version: Byte;
    SerialNumber: Byte;
  end;

  TEnclosureInfo = packed record
    Header: TSmBiosTableHeader;
    Manufacturer: Byte;
    &Type: Byte;
    Version: Byte;
    SerialNumber: Byte;
    AssetTagNumber: Byte;
    BootUpState: Byte;
    PowerSupplyState: Byte;
    ThermalState: Byte;
    SecurityStatus: Byte;
    OEM_Defined: DWORD;
  end;

  TProcessorInfo = packed record
    Header: TSmBiosTableHeader;
    SocketDesignation: Byte;
    ProcessorType: Byte;
    ProcessorFamily: Byte;
    ProcessorManufacturer: Byte;
    ProcessorID: Int64;                 // QWORD;
    ProcessorVersion: Byte;
    Voltaje: Byte;
    ExternalClock: Word;
    MaxSpeed: Word;
    CurrentSpeed: Word;
    Status: Byte;
    ProcessorUpgrade: Byte;
    L1CacheHandler: Word;
    L2CacheHandler: Word;
    L3CacheHandler: Word;
    SerialNumber: Byte;
    AssetTag: Byte;
    PartNumber: Byte;
  end;

  TCacheInfo = packed record
    Header: TSmBiosTableHeader;
    SocketDesignation: Byte;
    CacheConfiguration: DWORD;
    MaximumCacheSize: Word;
    InstalledSize: Word;
    SupportedSRAMType: Word;
    CurrentSRAMType: Word;
    CacheSpeed: Byte;
    ErrorCorrectionType: Byte;
    SystemCacheType: Byte;
    Associativity: Byte;
  end;

  TSMBiosTableEntry = record
    Header: TSmBiosTableHeader;
    Index: Integer;
  end;
  TSMBiosTablesList = array of TSMBiosTableEntry;

  TSMBios = class
  private
    FSize: Integer;
    FBuffer: PByteArray;
    //FDataString: AnsiString;
    FBiosInfo: TBiosInfo;
    FSysInfo: TSysInfo;
    FBaseBoardInfo: TBaseBoardInfo;
    FEnclosureInfo: TEnclosureInfo;
    FProcessorInfo: TProcessorInfo;
    FBiosInfoIndex: Integer;
    FSysInfoIndex: Integer;
    FBaseBoardInfoIndex: Integer;
    FEnclosureInfoIndex: Integer;
    FProcessorInfoIndex: Integer;
    FDmiRevision: Integer;
    FSmbiosMajorVersion: Integer;
    FSmbiosMinorVersion: Integer;
    FSMBiosTablesList: TSMBiosTablesList;
    procedure LoadSMBIOS;
    procedure ReadSMBiosTables;
    function GetHasBiosInfo: Boolean;
    function GetHasSysInfo: Boolean;
    function GetHasBaseBoardInfo: Boolean;
    function GetHasEnclosureInfo: Boolean;
    function GetHasProcessorInfo: Boolean;
    function GetSMBiosTablesList: TSMBiosTablesList;
  public
    constructor Create;
    destructor Destroy; override;

    function SearchSMBiosTable(TableType: SMBiosTables): Integer;
    function GetSMBiosTableIndex(TableType: SMBiosTables): Integer;
    function GetSMBiosString(Entry, Index: Integer): AnsiString;

    property Size: Integer read FSize;
    property Buffer: PByteArray read FBuffer;
    //property DataString: AnsiString read FDataString;
    property DmiRevision: Integer read FDmiRevision;
    property SmbiosMajorVersion: Integer read FSmbiosMajorVersion;
    property SmbiosMinorVersion: Integer read FSmbiosMinorVersion;
    property SMBiosTablesList: TSMBiosTablesList read FSMBiosTablesList;

    property BiosInfo: TBiosInfo read FBiosInfo write FBiosInfo;
    property BiosInfoIndex: Integer read FBiosInfoIndex write FBiosInfoIndex;
    property HasBiosInfo: Boolean read GetHasBiosInfo;
    property SysInfo: TSysInfo read FSysInfo write FSysInfo;
    property SysInfoIndex: Integer read FSysInfoIndex write FSysInfoIndex;
    property HasSysInfo: Boolean read GetHasSysInfo;
    property BaseBoardInfo: TBaseBoardInfo read FBaseBoardInfo write FBaseBoardInfo;
    property BaseBoardInfoIndex: Integer read FBaseBoardInfoIndex write FBaseBoardInfoIndex;
    property HasBaseBoardInfo: Boolean read GetHasBaseBoardInfo;
    property EnclosureInfo: TEnclosureInfo read FEnclosureInfo write FEnclosureInfo;
    property EnclosureInfoIndex: Integer read FEnclosureInfoIndex write FEnclosureInfoIndex;
    property HasEnclosureInfo: Boolean read GetHasEnclosureInfo;
    property ProcessorInfo: TProcessorInfo read FProcessorInfo write FProcessorInfo;
    property ProcessorInfoIndex: Integer read FProcessorInfoIndex write FProcessorInfoIndex;
    property HasProcessorInfo: Boolean read GetHasProcessorInfo;
  end;

function GetHWID(): string;

implementation

uses
  ComObj, ActiveX, Variants, uDiskSN;

{ TSMBios }

constructor TSMBios.Create;
begin
  inherited;
  FBuffer := nil;
  FSMBiosTablesList := nil;
  LoadSMBIOS;
  ReadSMBiosTables;
end;

destructor TSMBios.Destroy;
begin
  if Assigned(FBuffer) and (FSize > 0) then
    FreeMem(FBuffer);

  if Assigned(FSMBiosTablesList) then
    SetLength(FSMBiosTablesList, 0);
  FSMBiosTablesList := nil;

  inherited;
end;

function TSMBios.GetHasBaseBoardInfo: Boolean;
begin
  Result := FBaseBoardInfoIndex >= 0;
end;

function TSMBios.GetHasBiosInfo: Boolean;
begin
  Result := FBiosInfoIndex >= 0;
end;

function TSMBios.GetHasEnclosureInfo: Boolean;
begin
  Result := FEnclosureInfoIndex >= 0;
end;

function TSMBios.GetHasProcessorInfo: Boolean;
begin
  Result := FProcessorInfoIndex >= 0;
end;

function TSMBios.GetHasSysInfo: Boolean;
begin
  Result := FSysInfoIndex >= 0;
end;

function TSMBios.SearchSMBiosTable(TableType: SMBiosTables): Integer;
var
  Index                     : Integer;
  Header                    : TSmBiosTableHeader;
begin
  Index := 0;
  repeat
    Move(Buffer[Index], Header, SizeOf(Header));

    if Header.TableType = Ord(TableType) then
      Break
    else begin
      Inc(Index, Header.Length);
      if Index + 1 > FSize then begin
        Index := -1;
        Break;
      end;

      while not ((Buffer[Index] = 0) and (Buffer[Index + 1] = 0)) do
        if Index + 1 > FSize then begin
          Index := -1;
          Break;
        end else
          Inc(Index);

      Inc(Index, 2);
    end;
  until (Index > FSize);
  Result := Index;
end;

function TSMBios.GetSMBiosString(Entry, Index: Integer): AnsiString;
var
  i                         : Integer;
  p                         : PAnsiChar;
begin
  Result := '';
  for i := 1 to Index do begin
    p := PAnsiChar(@Buffer[Entry]);
    if i = Index then begin
      Result := p;
      Break;
    end else
      Inc(Entry, StrLen(p) + 1);
  end;
end;

function TSMBios.GetSMBiosTableIndex(TableType: SMBiosTables): Integer;
var
  i                         : Integer;
begin
  Result := -1;
  for i := 0 to Length(FSMBiosTablesList) - 1 do
    if FSMBiosTablesList[i].Header.TableType = Ord(TableType) then begin
      Result := FSMBiosTablesList[i].Index;
      Break;
    end;
end;

function TSMBios.GetSMBiosTablesList: TSMBiosTablesList;
var
  Index                     : Integer;
  Header                    : TSmBiosTableHeader;
  Entry                     : TSMBiosTableEntry;
begin
  Result := FSMBiosTablesList;
  Index := 0;
  repeat
    Move(Buffer[Index], Header, SizeOf(Header));
    Entry.Header := Header;
    Entry.Index := Index;
    SetLength(FSMBiosTablesList, Length(FSMBiosTablesList) + 1);
    FSMBiosTablesList[High(FSMBiosTablesList)] := Entry;

    if Header.TableType = Ord(EndofTable) then
      Break;

    Inc(Index, Header.Length);          // + 1);
    if Index + 1 > FSize then
      Break;

    while not ((Buffer[Index] = 0) and (Buffer[Index + 1] = 0)) do
      if Index + 1 > FSize then
        Break
      else
        Inc(Index);

    Inc(Index, 2);
  until (Index > FSize);
end;

procedure TSMBios.LoadSMBIOS;
const
  wbemFlagForwardOnly       = $00000020;
var
  FSWbemLocator             : OLEVariant;
  FWMIService               : OLEVariant;
  FWbemObjectSet            : OLEVariant;
  FWbemObject               : OLEVariant;
  oEnum                     : IEnumvariant;
  iValue                    : LongWord;
  vArray                    : variant;
  Value                     : Integer;
  i                         : Integer;
  szTemp1, szTemp2, szTemp3, szTemp4: string;
begin
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  szTemp1 := ('localhost');
  szTemp2 := ('root\WMI');
  FWMIService := FSWbemLocator.ConnectServer(szTemp1, szTemp2, '', '');
  szTemp3 := ('SELECT * FROM MSSmBios_RawSMBiosTables');
  szTemp4 := ('WQL');
  FWbemObjectSet := FWMIService.ExecQuery(szTemp3, szTemp4, wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
  if oEnum.Next(1, FWbemObject, iValue) = 0 then begin
    FSize := FWbemObject.Size;
    GetMem(FBuffer, FSize);

    FDmiRevision := FWbemObject.DmiRevision;
    FSmbiosMajorVersion := FWbemObject.SmbiosMajorVersion;
    FSmbiosMinorVersion := FWbemObject.SmbiosMinorVersion;

    vArray := FWbemObject.SMBiosData;

    if (VarType(vArray) and VarArray) <> 0 then
      for i := VarArrayLowBound(vArray, 1) to VarArrayHighBound(vArray, 1) do begin
        Value := vArray[i];
        Buffer[i] := Value;
        {if Value in [$20..$7E] then
          FDataString := FDataString + AnsiString(Chr(Value))
        else
          FDataString := FDataString + '.';}
      end;

    GetSMBiosTablesList();
    FWbemObject := Unassigned;
  end;
end;

procedure TSMBios.ReadSMBiosTables;
begin
  FBiosInfoIndex := GetSMBiosTableIndex(BIOSInformation);
  if FBiosInfoIndex >= 0 then
    Move(Buffer[FBiosInfoIndex], FBiosInfo, SizeOf(FBiosInfo));

  FSysInfoIndex := GetSMBiosTableIndex(SystemInformation);
  if FSysInfoIndex >= 0 then
    Move(Buffer[FSysInfoIndex], FSysInfo, SizeOf(FSysInfo));

  FBaseBoardInfoIndex := GetSMBiosTableIndex(BaseBoardInformation);
  if FBaseBoardInfoIndex >= 0 then
    Move(Buffer[FBaseBoardInfoIndex], FBaseBoardInfo, SizeOf(FBaseBoardInfo));

  FEnclosureInfoIndex := GetSMBiosTableIndex(EnclosureInformation);
  if FEnclosureInfoIndex >= 0 then
    Move(Buffer[FEnclosureInfoIndex], FEnclosureInfo, SizeOf(FEnclosureInfo));

  FProcessorInfoIndex := GetSMBiosTableIndex(ProcessorInformation);
  if FProcessorInfoIndex >= 0 then
    Move(Buffer[FProcessorInfoIndex], FProcessorInfo, SizeOf(FProcessorInfo));
end;

{procedure GetMSSmBios_RawSMBiosTablesInfo;
var
  SMBios                    : TSMBios;
  UUID                      : array[0..31] of CHAR;
  Entry                     : TSMBiosTableEntry;
begin
  SMBios := TSMBios.Create;
  try
    with SMBios do begin
      Writeln(Format('%d SMBios tables found', [Length(SMBiosTablesList)]));
      Writeln('');

      if HasBiosInfo then begin
        Writeln('Bios Information');
        Writeln('Vendor        ' + GetSMBiosString(BiosInfoIndex + BiosInfo.Header.Length, BiosInfo.Vendor));
        Writeln('Version       ' + GetSMBiosString(BiosInfoIndex + BiosInfo.Header.Length, BiosInfo.Version));
        Writeln('Start Segment ' + IntToHex(BiosInfo.StartingSegment, 4));
        Writeln('ReleaseDate   ' + GetSMBiosString(BiosInfoIndex + BiosInfo.Header.Length, BiosInfo.ReleaseDate));
        Writeln(Format('Bios Rom Size %d k', [64 * (BiosInfo.BiosRomSize + 1)]));
        Writeln('');
      end;

      if HasSysInfo then begin
        Writeln('System Information');
        Writeln('Manufacter    ' + GetSMBiosString(SysInfoIndex + SysInfo.Header.Length, SysInfo.Manufacturer));
        Writeln('Product Name  ' + GetSMBiosString(SysInfoIndex + SysInfo.Header.Length, SysInfo.ProductName));
        Writeln('Version       ' + GetSMBiosString(SysInfoIndex + SysInfo.Header.Length, SysInfo.Version));
        Writeln('Serial Number ' + GetSMBiosString(SysInfoIndex + SysInfo.Header.Length, SysInfo.SerialNumber));
        BinToHex(@SysInfo.UUID, UUID, SizeOf(SysInfo.UUID));
        Writeln('UUID          ' + UUID);

        Writeln('');
      end;

      if HasBaseBoardInfo then begin
        Writeln('BaseBoard Information');
        Writeln('Manufacter    ' + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.Manufacturer));
        Writeln('Product       ' + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.Product));
        Writeln('Version       ' + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.Version));
        Writeln('Serial Number ' + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.SerialNumber));
        Writeln('');
      end;

      if HasEnclosureInfo then begin
        Writeln('Enclosure Information');
        Writeln('Manufacter    ' + GetSMBiosString(EnclosureInfoIndex + EnclosureInfo.Header.Length, EnclosureInfo.Manufacturer));
        Writeln('Version       ' + GetSMBiosString(EnclosureInfoIndex + EnclosureInfo.Header.Length, EnclosureInfo.Version));
        Writeln('Serial Number ' + GetSMBiosString(EnclosureInfoIndex + EnclosureInfo.Header.Length, EnclosureInfo.SerialNumber));
        Writeln('Asset Tag Number ' + GetSMBiosString(EnclosureInfoIndex + EnclosureInfo.Header.Length, EnclosureInfo.AssetTagNumber));
        Writeln('');
      end;

      if HasProcessorInfo then begin
        Writeln('Processor Information');
        Writeln('Socket Designation     ' + GetSMBiosString(ProcessorInfoIndex + ProcessorInfo.Header.Length, ProcessorInfo.SocketDesignation));
        Writeln('Processor Manufacturer ' + GetSMBiosString(ProcessorInfoIndex + ProcessorInfo.Header.Length, ProcessorInfo.ProcessorManufacturer));
        Writeln('Serial Number          ' + GetSMBiosString(ProcessorInfoIndex + ProcessorInfo.Header.Length, ProcessorInfo.SerialNumber));
        Writeln('Asset Tag              ' + GetSMBiosString(ProcessorInfoIndex + ProcessorInfo.Header.Length, ProcessorInfo.AssetTag));
        Writeln('Part Number            ' + GetSMBiosString(ProcessorInfoIndex + ProcessorInfo.Header.Length, ProcessorInfo.PartNumber));
        Writeln('');
      end;
    end;
  finally
    SMBios.Free;
  end;
end;}

function GetHWID(): string;
var
  SMBios                    : TSMBios;
  UUID                      : array[0..31] of CHAR;
  Entry                     : TSMBiosTableEntry;
begin
  Result := GetIdeSerialNumber(0);
  try
    CoInitialize(nil);
    try
      SMBios := TSMBios.Create;
      try
        with SMBios do begin
          if HasBiosInfo then begin
            Result := Result + GetSMBiosString(BiosInfoIndex + BiosInfo.Header.Length, BiosInfo.Version);
          end;
          if HasBaseBoardInfo then begin
            Result := Result + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.Product);
            Result := Result + GetSMBiosString(BaseBoardInfoIndex + BaseBoardInfo.Header.Length, BaseBoardInfo.SerialNumber);
          end;
        end;
      finally
        SMBios.Free;
      end;
    finally
      CoUninitialize;
    end;
  except
    //on E: EOleException do
    //  Writeln(Format('EOleException %s %x', [E.Message, E.ErrorCode]));
    //on E: Exception do
    //  Writeln(E.Classname, ':', E.Message);
  end;
end;

end.

