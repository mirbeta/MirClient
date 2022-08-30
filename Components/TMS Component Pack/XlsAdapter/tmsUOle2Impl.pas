unit tmsUOle2Impl;
{$INCLUDE FLEXCEL.INC}

{$R+}
interface
uses Classes, tmsUFlxMessages, SysUtils, tmsXlsMessages, Contnrs,
     {$IFDEF FLX_GENERICS} Generics.Collections, {$ENDIF}
     tmsUXlsProtect;

{$IFDEF DELPHIXE4UP}
const
  soFromBeginning = TSeekOrigin.soBeginning;
  soFromCurrent = TSeekOrigin.soCurrent;
  soFromEnd = TSeekOrigin.soEnd;
{$ENDIF}


type
  STGTY = (
    STGTY_INVALID = 0,
    STGTY_STORAGE = 1,
    STGTY_STREAM = 2,
    STGTY_LOCKBYTES = 3,
    STGTY_PROPERTY = 4,
    STGTY_ROOT = 5
  );

  DECOLOR = (
    DECOLOR_RED = 0,
    DECOLOR_BLACK = 1
  );

  {$IFDEF FLX_GENERICS}
  UInt32List = TList<UInt32>;
  {$ELSE}
  UInt32List = class
  private
    FList: TList;
    function GetItems(const i: integer): UInt32;
    procedure SetItems(const i: integer; const Value: UInt32);

    function GetCapacity: Int32;
    procedure SetCapacity(const Value: Int32);
    function GetCount: Int32;

  protected
    property Capacity: Int32 read GetCapacity write SetCapacity;

  public
    constructor Create;
    destructor Destroy; override;

    property Items[const i: integer]: UInt32 read GetItems write SetItems; default;
    procedure Add(const Item: UInt32);
    property Count: Int32 read GetCount;
  end;
  {$ENDIF}

  TOneDirEntry = class
  public
    Name: UTF16String;
    LeftSid: Int32;
    RightSid: Int32;
    ChildSid: Int32;

    Deleted: boolean;
    Color: DECOLOR;

    constructor Create(const aName: UTF16String; const aLeftSid, aRightSid, aChildSid: Int32; const aColor: DECOLOR);
  end;

  {$IFDEF FLX_GENERICS}
  TDirEntryList = TObjectList<TOneDirEntry>;
  {$ELSE}
  TDirEntryList = class
  private
    FList: TObjectList;
    function GetItems(const i: integer): TOneDirEntry;
    procedure SetItems(const i: integer; const Value: TOneDirEntry);
    function GetCount: Int32;

  protected
  public
    constructor Create;
    destructor Destroy; override;

    property Items[const i: integer]: TOneDirEntry read GetItems write SetItems; default;
    procedure Add(const Item: TOneDirEntry);
    property Count: Int32 read GetCount;
  end;
  {$ENDIF}

  StringArray = Array of UTF16String;

  /// <summary>
  /// Header sector. It has a fixed size of 512 bytes.
  /// On this implementation, we don't save the first 109 DIF entries, as they will be saved by the DIF Sector.
  /// </summary>
  TOle2Header = class
  private
    FileSignature: ByteArray;
    class function CompareArray(const a1: ByteArray; const a2: ByteArray; const length: Int32): Boolean;
    function Get_FuSectorShift(): Int32;
    function Get_FSectorSize(): UInt32;
    function Get_uMiniSectorShift(): Int32;
    function Get_MiniSectorSize(): UInt32;
    function Get_csectFat(): UInt32;
    procedure Set_csectFat(const value: UInt32);
    function Get_sectDirStart(): UInt32;
    procedure Set_sectDirStart(const value: UInt32);
    function Get_FulMiniSectorCutoff(): UInt32;
    function Get_sectMiniFatStart(): UInt32;
    procedure Set_sectMiniFatStart(const value: UInt32);
    function Get_csectMiniFat(): UInt32;
    procedure Set_csectMiniFat(const value: UInt32);
    function Get_sectDifStart(): UInt32;
    procedure Set_sectDifStart(const value: UInt32);
    function Get_csectDif(): UInt32;
    procedure Set_csectDif(const value: UInt32);
    property FuSectorShift: Int32 read Get_FuSectorShift;
    property FSectorSize: UInt32 read Get_FSectorSize;

  public
    NotXls97: Boolean;
    Data: ByteArray;
    StartOfs: Int64;
    SectorSize: UInt32;
    uSectorShift: Int32;
    ulMiniSectorCutoff: UInt32;

    /// <summary>
    /// Creates the Header reading the data from a stream.
    /// </summary>
    /// <param name="aStream"></param>
    /// <param name="AvoidExceptions"></param>
    constructor Create(const aStream: TStream; const AvoidExceptions: Boolean);

    procedure Save(const aStream: TStream);
    function uDIFEntryShift(): Int32;
    function SectToStPos(const Sect: Int64): Int64;overload;
    function SectToStPos(const Sect: Int64; const Ofs: Int64): Int64;overload;
    property uMiniSectorShift: Int32 read Get_uMiniSectorShift;
    property MiniSectorSize: UInt32 read Get_MiniSectorSize;
    property csectFat: UInt32 read Get_csectFat write Set_csectFat;
    property sectDirStart: UInt32 read Get_sectDirStart write Set_sectDirStart;
    property FulMiniSectorCutoff: UInt32 read Get_FulMiniSectorCutoff;
    property sectMiniFatStart: UInt32 read Get_sectMiniFatStart write Set_sectMiniFatStart;
    property csectMiniFat: UInt32 read Get_csectMiniFat write Set_csectMiniFat;
    property sectDifStart: UInt32 read Get_sectDifStart write Set_sectDifStart;
    property csectDif: UInt32 read Get_csectDif write Set_csectDif;
  end;


  /// <summary>
  /// FAT Table stored as a list of ints.
  /// </summary>
  TOle2FAT = class (UInt32List)
  private
    Header: TOle2Header;
    LastFindSectorOfs: Int64;
    LastFindSectorStart: Int64;
    LastFindSectorRes: Int64;

    /// <summary>
    /// Use Create() to create an instance. This way we avoid calling virtual methods on a constructor.
    /// </summary>
    constructor Create(); overload;
    procedure LoadDifSector(const data: ByteArray; const inipos: UInt32; const endpos: UInt32; const aStream: TStream);
    procedure LoadFatSector(const data: ByteArray);

  public

    /// <summary>
    /// Creates a FAT integer list from the data on a stream.
    /// We have to read the DIF to access the actual FAT sectors.
    /// </summary>
    /// <param name="aHeader">The header record</param>
    /// <param name="aStream">Stream to read the FAT. When null, an Empty FAT will be created.</param>
    constructor Create(const aHeader: TOle2Header; const aStream: TStream); overload;
    destructor Destroy; override;
    function uFATEntryShift(): Int32;
    function GetNextSector(const Sect: Int64): Int64;
    function FindSector(const StartSect: Int64; const SectOfs: Int64): Int64;
  end;


  /// <summary>
  /// MINIFAT Table stored as a list of ints.
  /// </summary>
  TOle2MiniFAT = class (UInt32List)
  private
    Header: TOle2Header;

    /// <summary>
    /// Use Create() to create an instance. This way we avoid calling virtual methods on a constructor.
    /// </summary>
    constructor Create(); overload;
    procedure LoadMiniFatSector(const data: ByteArray);

  public

    /// <summary>
    /// Creates a MiniFAT integer list from the data on a stream.
    /// </summary>
    /// <param name="aHeader"></param>
    /// <param name="aStream"></param>
    /// <param name="aFAT"></param>
    constructor Create(const aHeader: TOle2Header; const aStream: TStream; const aFAT: TOle2FAT); overload;
    function GetNextSector(const Sect: Int64): Int64;
    function FindSector(const StartSect: Int64; const SectOfs: Int64): Int64;
  end;


  /// <summary>
  /// A semi-sector containing 1 Directory entry.
  /// </summary>
  TOle2Directory = class
  private
    Data: ByteArray;
    function Get_NameSize(): Int32;
    procedure Set_NameSize(const value: Int32);
    function Get_Name(): UTF16String;
    procedure Set_Name(const value: UTF16String);
    function Get_ObjType(): STGTY;
    procedure Set_ObjType(const value: STGTY);
    function Get_SectStart(): Int64;
    procedure Set_SectStart(const value: Int64);
    function Get_xulSize(): Int64;
    procedure Set_xulSize(const value: Int64);

  public
    ulSize: Int64;
    constructor Create(const aData: ByteArray);
    procedure Save(const aStream: TStream);
    class function GetNameSize(const Data: ByteArray; const StartPos: Int32): Int32;
    class function GetName(const Data: ByteArray; const StartPos: Int32): UTF16String;
    class function GetType(const Data: ByteArray; const StartPos: Int32): STGTY;
    class function GetSectStart(const Data: ByteArray; const StartPos: Int32): Int64;
    class procedure SetSectStart(const Data: ByteArray; const StartPos: Int32; const value: Int64);
    class function GetSize(const Data: ByteArray; const StartPos: Int32): Int64;
    class procedure SetSize(const Data: ByteArray; const StartPos: Int32; const value: Int64);
    class procedure Clear(const Data: ByteArray; const StartPos: Int32);
    class function GetLeftSid(const Data: ByteArray; const StartPos: Int32): Int32;
    class procedure SetLeftSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
    class function GetRightSid(const Data: ByteArray; const StartPos: Int32): Int32;
    class procedure SetRightSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
    class function GetChildSid(const Data: ByteArray; const StartPos: Int32): Int32;
    class procedure SetChildSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
    class function GetColor(const Data: ByteArray; const StartPos: Int32): DECOLOR;
    class procedure SetColor(const Data: ByteArray; const StartPos: Int32; const value: DECOLOR);
    property NameSize: Int32 read Get_NameSize write Set_NameSize;
    property Name: UTF16String read Get_Name write Set_Name;
    property ObjType: STGTY read Get_ObjType write Set_ObjType;
    property SectStart: Int64 read Get_SectStart write Set_SectStart;
    property xulSize: Int64 read Get_xulSize write Set_xulSize;
  end;

  TSectorBuffer = class
  private
    Data: ByteArray;
    Changed: Boolean;
    FSectorId: Int64;
    Header: TOle2Header;
    DataStream: TStream;
  public
    constructor Create(const aHeader: TOle2Header; const aStream: TStream);
    procedure Load(const SectNo: Int64);
    procedure Save();
    procedure Read(const aBuffer: ByteArray; const BufferPos: Int64; out nRead: Int64; const StartPos: Int64; const Count: Int64; const SectorSize: Int64);
    procedure ReadMem(var aBuffer; const BufferPos: Int64; out nRead: Int64; const StartPos: Int64; const Count: Int64; const SectorSize: Int64);
    property SectorId: Int64 read FSectorId;
  end;


  /// <summary>
  /// Class encapsulating an OLE2 file. FAT is kept in memory, data is read/written from/to disk.
  /// </summary>
  TOle2File = class
  private
    FStream: TStream;
    Header: TOle2Header;
    FAT: TOle2FAT;
    MiniFAT: TOle2MiniFAT;
    SectorBuffer: TSectorBuffer;
    ROOT: TOle2Directory;
    FEncryption: TEncryptionData;
    TOle2FileStr: UTF16String;
    disposed: Boolean;
    DIR: TOle2Directory;
    StreamPos: Int64;
    PreparedForWrite: Boolean;
    DIRStartPos: Int64;
    procedure MarkDeleted(const i: Int32; const Result: TDirEntryList; const Level: Int32);
    class procedure DeleteNode(const Result: TDirEntryList; var ParentLeaf: Int32);
    procedure FixNode(const Result: TDirEntryList; var ParentNode: Int32);
    function ReadDirs(const DeletedStorages: StringArray; var PaintItBlack: Boolean): TDirEntryList;
    procedure FinishStream();
    function Get_Length(): Int64;
    function Get_Position(): Int64;
    function Get_Eof(): Boolean;
    function Get_FileName(): UTF16String;

  public
    NotXls97: Boolean;

    /// <summary>
    /// Opens an EXISTING OLE2 Stream. There is no support for creating a new one, you can only modify existing ones.
    /// </summary>
    /// <param name="aStream">The stream with the data</param>
    constructor Create(const aStream: TStream);overload;

    /// <summary>
    /// Opens an EXISTING OLE2 Stream, without throwing an exception if it is a wrong file. (On this case the error is logged into the Notxls97 variable)
    /// There is no support for creating a new one, you can only modify existing ones.
    /// </summary>
    /// <param name="aStream">The stream with the data</param>
    /// <param name="AvoidExceptions">If true, no Exception will be raised when the file is not OLE2.</param>
    constructor Create(const aStream: TStream; const AvoidExceptions: Boolean);overload;

    destructor Destroy; override;

    procedure Close();
    function FindDir(const DirName: UTF16String): TOle2Directory;
    function FindRoot(): TOle2Directory;
    procedure SelectStream(const StreamName: UTF16String);
    function NextEof(const Count: Int32): Boolean;

    procedure ReadMem(var aBuffer; const Count: Int32);
    /// <summary>
    /// Writes to the stream sequentially. No seek or read allowed while writing.
    /// </summary>
    /// <param name="Buffer">The data.</param>
    /// <param name="Count">number of bytes to write.</param>
    procedure WriteRawMem(const Buffer; const Count: Int32);overload;
    procedure WriteMem(const Buffer; const Count: Int32);overload;

    procedure Read(const aBuffer: ByteArray; const Count: Int32);
    procedure WriteRaw(const Buffer: ByteArray; const Count: Int32);overload;
    procedure Write(Buffer: ByteArray; const Count: Int32);overload;


    procedure WriteRaw(const Buffer: ByteArray; const StartPos: Int32; const Count: Int32);overload;
    procedure WriteHeader(const Id: UInt16; const Len: UInt16);
    procedure Write(Buffer: ByteArray; const StartPos: Int32; const Count: Int32);overload;
    procedure Write16(Buffer: UInt16);
    procedure Write32(Buffer: UInt32);
    class function FindString(const s: UTF16String; const list: StringArray): Boolean;

    /// <summary>
    /// Only seeks forward, no reverse. Not really optimized either, don't use in heavy places.
    /// </summary>
    /// <param name="Offset"></param>
    /// <returns></returns>
    procedure SeekForward(const Offset: Int64);

    /// <summary>
    /// This method copies the contents of the ole stream to a new one, and clears the OStreamName
    /// stream, leaving it ready to be written.
    /// </summary>
    /// <param name="OutStream">The new Stream where we will write the data</param>
    /// <param name="OStreamName">Ole Stream Name (tipically "Workbook") that we will clear to write the new data.</param>
    /// <param name="DeleteStorages">Storages we are not going to copy to the new one. Used to remove macros.</param>
    procedure PrepareForWrite(const OutStream: TStream; const OStreamName: UTF16String; const DeleteStorages: StringArray);
    property Encryption: TEncryptionData read FEncryption;
    property Length: Int64 read Get_Length;
    property Position: Int64 read Get_Position;
    property Eof: Boolean read Get_Eof;
    property FileName: UTF16String read Get_FileName;
  end;


implementation
{.$region 'Constants'}
const
    TOle2Header_HeaderSize = 512;
   (*
    * OLE2 File format implementation.
	  * This file is used by UOLE2Stream to provide an uniform access layer to OLE2 Compound documents.
	  * Honoring flexcel tradition, this file is targeted to be "one" api to modify, instead of "two" apis, one for read and one for write.
	  *)

  //This is fixed on the header sector.
    TOle2Header_DifsInHeader: Int32  = 109;
    TOle2Header_DifEntries: Int32  = 436;  //Difs don't really belong here.
    TOle2Header_ENDOFCHAIN: UInt32  = 4294967294;
    TOle2Header_DIFSECT: UInt32  = 4294967292;
    TOle2Header_FATSECT: UInt32  = 4294967293;
    TOle2Header_FREESECT: UInt32 = 4294967295;
    TOle2Directory_DirectorySize = 128;
{.$endregion}

procedure StreamRead(const aStream: TStream; const aData: ByteArray; const IniOfs, Count: integer; ThrowOnEOF: boolean);
var
  BytesRead: integer;
  i: integer;
begin
  if (ThrowOnEOF) then aStream.ReadBuffer(aData[IniOfs], Count)
  else
  begin
    BytesRead := aStream.Read(aData[IniOfs], Count);
    if BytesRead < Count then
    begin
      for i := BytesRead to Count - 1 do aData[IniOfs + i] := 0;
    end;
  end;
end;


{.$region 'TOle2Header'}
{ TOle2Header }
constructor TOle2Header.Create(const aStream: TStream; const AvoidExceptions: Boolean);
begin
  inherited Create;
  //Initializations
  SetLength (FileSignature, 8);
  FileSignature[0] := 208;
  FileSignature[1] := 207;
  FileSignature[2] := 17;
  FileSignature[3] := 224;
  FileSignature[4] := 161;
  FileSignature[5] := 177;
  FileSignature[6] := 26;
  FileSignature[7] := 225;

  StartOfs := aStream.Position;
  SetLength (Data, TOle2Header_HeaderSize - TOle2Header_DifEntries);
  FillChar(Data[0], Length(Data), 0);
  if (aStream.Size - StartOfs) < Length(Data) then
  begin
    if AvoidExceptions then
    begin
      NotXls97 := true;
      exit;
    end;

    raise Exception.CreateFmt(ErrFileIsNotXLS,['']);
  end;

  StreamRead(aStream, Data, 0, Length(Data), false);
  if not CompareArray(Data, FileSignature, Length(FileSignature)) then
  begin
    if AvoidExceptions then
    begin
      NotXls97 := true;
      exit;
    end;

    raise Exception.CreateFmt(ErrFileIsNotXLS,['']);
  end;

  uSectorShift := FuSectorShift;
  SectorSize := FSectorSize;
  ulMiniSectorCutoff := FulMiniSectorCutoff;
end;

procedure TOle2Header.Save(const aStream: TStream);
begin
  aStream.WriteBuffer(Data[0], Length(Data));
end;

class function TOle2Header.CompareArray(const a1: ByteArray; const a2: ByteArray; const length: Int32): Boolean;
begin
  Result := CompareMem(@a1[0], @a2[0], length)
end;

function TOle2Header.uDIFEntryShift(): Int32;
begin
  Result := uSectorShift - 2;
end;

function TOle2Header.SectToStPos(const Sect: Int64): Int64;
begin
  Result := ((Sect shl uSectorShift) + TOle2Header_HeaderSize) + StartOfs;
end;

function TOle2Header.SectToStPos(const Sect: Int64; const Ofs: Int64): Int64;
begin
  Result := ((Sect shl uSectorShift) + TOle2Header_HeaderSize) + Ofs;
end;

function TOle2Header.Get_FuSectorShift(): Int32;
begin  //UInt16 has a bug with mono
  Result := UInt16((@Data[30])^);
end;

function TOle2Header.Get_FSectorSize(): UInt32;
begin
  Result := UInt32(1) shl FuSectorShift;
end;

function TOle2Header.Get_uMiniSectorShift(): Int32;
begin
  Result := UInt16((@Data[32])^);
end;

function TOle2Header.Get_MiniSectorSize(): UInt32;
begin
  Result := UInt32(1) shl uMiniSectorShift;
end;

function TOle2Header.Get_csectFat(): UInt32;
begin
  Result := UInt32((@Data[44])^);
end;

procedure TOle2Header.Set_csectFat(const value: UInt32);
begin
  System.Move(value, Data[44], SizeOf(value));
end;

function TOle2Header.Get_sectDirStart(): UInt32;
begin
  Result := UInt32((@Data[48])^);
end;

procedure TOle2Header.Set_sectDirStart(const value: UInt32);
begin
  System.Move(value, Data[48], SizeOf(value));
end;

function TOle2Header.Get_FulMiniSectorCutoff(): UInt32;
begin
  Result := UInt32((@Data[56])^);
end;

function TOle2Header.Get_sectMiniFatStart(): UInt32;
begin
  Result := UInt32((@Data[60])^);
end;

procedure TOle2Header.Set_sectMiniFatStart(const value: UInt32);
begin
  System.Move(value, Data[60], SizeOf(value));
end;

function TOle2Header.Get_csectMiniFat(): UInt32;
begin
  Result := UInt32((@Data[64])^);
end;

procedure TOle2Header.Set_csectMiniFat(const value: UInt32);
begin
  System.Move(value, Data[64], SizeOf(value));
end;

function TOle2Header.Get_sectDifStart(): UInt32;
begin
  Result := UInt32((@Data[68])^);
end;

procedure TOle2Header.Set_sectDifStart(const value: UInt32);
begin
  System.Move(value, Data[68], SizeOf(value));
end;

function TOle2Header.Get_csectDif(): UInt32;
begin
  Result := UInt32((@Data[72])^);
end;

procedure TOle2Header.Set_csectDif(const value: UInt32);
begin
  System.Move(value, Data[72], SizeOf(value));
end;

{.$endregion}
{.$region 'TOle2FAT'}
{ TOle2FAT }
constructor TOle2FAT.Create();
begin
  inherited Create;
  //Initializations
  LastFindSectorOfs := -1;
  LastFindSectorStart := -1;
  LastFindSectorRes := 0;

end;

constructor TOle2FAT.Create(const aHeader: TOle2Header; const aStream: TStream);
var
  DifSect0: ByteArray;
  DifPos: UInt32;
  DifSect: ByteArray;
  i: UInt32;
begin
  Create;
  Header := aHeader;
  if aStream <> nil then
  begin
    Capacity := (Int32(aHeader.csectFat shl (uFATEntryShift and 31)) + TOle2Header_DifsInHeader) + 16;  //This is, number of fat sectors*(SectorSize/4)+109+ extra_just_in_case
    SetLength (DifSect0, TOle2Header_DifEntries);
    FillChar(DifSect0[0], Length(DifSect0), 0);
    aStream.Seek((Header.StartOfs + TOle2Header_HeaderSize) - TOle2Header_DifEntries, soFromBeginning);
    StreamRead(aStream, DifSect0, 0, Length(DifSect0), false);
    LoadDifSector(DifSect0, 0, TOle2Header_DifEntries, aStream);  //First 109 DIF records are on the header.

     //if there are Dif sectors, load them.
    DifPos := Header.sectDifStart;
    SetLength (DifSect, Header.SectorSize);
    FillChar(DifSect[0], Length(DifSect), 0);
    for i := 1 to Header.csectDif do
    begin
      if DifPos = TOle2Header_ENDOFCHAIN then
        raise Exception.Create(ErrExcelInvalid);

      aStream.Seek(Header.SectToStPos(DifPos), soFromBeginning);
      StreamRead(aStream, DifSect, 0, Length(DifSect), false);
      LoadDifSector(DifSect, 0, Header.SectorSize - 4, aStream);
      DifPos := UInt32((@DifSect[Int32(Header.SectorSize) - 4])^);
    end;

  end;

 //Some sanity checks
 //not really... sometimes it is not. if (DifPos!=TOle2Header.ENDOFCHAIN) throw new IOException(XlsMessages.GetString(XlsErr.ErrExcelInvalid));
end;

destructor TOle2FAT.Destroy;
begin
  inherited;
end;

function TOle2FAT.uFATEntryShift(): Int32;
begin
  Result := Header.uSectorShift - 2;
end;

function TOle2FAT.GetNextSector(const Sect: Int64): Int64;
begin
  Result := (Self[Int32(Sect)]);
end;

function TOle2FAT.FindSector(const StartSect: Int64; const SectOfs: Int64): Int64;
var
  NewSect: Int64;
  RealSectOfs: Int64;
  i: Int32;
begin
  NewSect := StartSect;
  RealSectOfs := SectOfs;
  if (LastFindSectorStart = StartSect) and (SectOfs >= LastFindSectorOfs) then  //Optimization for sequential read.
  begin
    NewSect := LastFindSectorRes;
    RealSectOfs:= RealSectOfs - LastFindSectorOfs;
  end;

  for i := 0 to RealSectOfs - 1 do
  begin
    NewSect := (Self[Int32(NewSect)]);
  end;

  LastFindSectorStart := StartSect;
  LastFindSectorOfs := SectOfs;
  LastFindSectorRes := NewSect;
  Result := NewSect;
end;

procedure TOle2FAT.LoadDifSector(const data: ByteArray; const inipos: UInt32; const endpos: UInt32; const aStream: TStream);
var
  FatSect: ByteArray;
  FatEntries: Int32;
  i: UInt32;
  FatId: UInt32;
  k: Int32;
begin
  SetLength (FatSect, Header.SectorSize);
  FillChar(FatSect[0], Length(FatSect), 0);
  FatEntries := 1 shl uFATEntryShift;
  i := inipos;
  while i < endpos do
  try
    FatId := UInt32((@data[i])^);
    if FatId = TOle2Header_ENDOFCHAIN then
      exit;

    if FatId = TOle2Header_FREESECT then
    begin
       //We have to keep track of the FAT position.
      for k := 0 to FatEntries - 1 do
        Add(TOle2Header_FREESECT);

      continue;
    end;

    aStream.Seek(Header.SectToStPos(FatId), soFromBeginning);
    StreamRead(aStream, FatSect, 0, Length(FatSect), false);
    LoadFatSector(FatSect);
  finally
    i:= i + 4;
  end;

end;

procedure TOle2FAT.LoadFatSector(const data: ByteArray);
var
  HeaderSectorSize: UInt32;
  i: Int64;
  Sect: UInt32;
begin
  HeaderSectorSize := Header.SectorSize;
  i := 0;
  while i < HeaderSectorSize do
  try
    Sect := UInt32((@data[i])^);
     //No, we have to load it the same. if (Sect== TOle2Header.FREESECT) continue;
    Add(Sect);
  finally
    i:= i + 4;
  end;

end;

{.$endregion}
{.$region 'TOle2MiniFAT'}
{ TOle2MiniFAT }
constructor TOle2MiniFAT.Create();
begin
  inherited Create;
end;

constructor TOle2MiniFAT.Create(const aHeader: TOle2Header; const aStream: TStream; const aFAT: TOle2FAT);
var
  MiniFatSect: ByteArray;
  MiniFatPos: Int64;
  i: UInt32;
begin
  Create;
  Header := aHeader;
  Capacity := Int32(aHeader.csectMiniFat shl ((aHeader.uSectorShift - 2) and 31)) + 16;  //This is, number of minifat sectors*(SectorSize/4)+ extra_just_in_case
  SetLength (MiniFatSect, aHeader.SectorSize);
  FillChar(MiniFatSect[0], Length(MiniFatSect), 0);
  MiniFatPos := aHeader.sectMiniFatStart;
  for i := 1 to aHeader.csectMiniFat do
  begin
    if MiniFatPos = TOle2Header_ENDOFCHAIN then
      raise Exception.Create(ErrExcelInvalid);

    aStream.Seek(aHeader.SectToStPos(MiniFatPos), soFromBeginning);
    StreamRead(aStream, MiniFatSect, 0, Length(MiniFatSect), false);
    LoadMiniFatSector(MiniFatSect);
    MiniFatPos := aFAT.GetNextSector(MiniFatPos);
  end;

end;

function TOle2MiniFAT.GetNextSector(const Sect: Int64): Int64;
begin
  Result := Self[Int32(Sect)];
end;

function TOle2MiniFAT.FindSector(const StartSect: Int64; const SectOfs: Int64): Int64;
var
  NewSect: Int64;
  i: Int32;
begin
  NewSect := StartSect;
  for i := 0 to SectOfs - 1 do
  begin
    NewSect := Self[Int32(NewSect)];
  end;

  Result := NewSect;
end;

procedure TOle2MiniFAT.LoadMiniFatSector(const data: ByteArray);
var
  i: Int32;
  Sect: UInt32;
begin
  i := 0;
  while i < Int32(Header.SectorSize) do
  begin
    Sect := UInt32((@data[i])^);
     //NO!  Has to be loaded anyway. if (Sect== TOle2Header.FREESECT)continue;
    Add(Sect);

    i:= i + 4;
  end;

end;

{.$endregion}
{.$region 'TOle2Directory'}
{ TOle2Directory }
constructor TOle2Directory.Create(const aData: ByteArray);
begin
  inherited Create;
  Data := aData;
  ulSize := xulSize;
end;

procedure TOle2Directory.Save(const aStream: TStream);
begin
  xulSize := ulSize;
  aStream.WriteBuffer(Data[0], Length(Data));
end;

class function TOle2Directory.GetNameSize(const Data: ByteArray; const StartPos: Int32): Int32;
var
  nl: Int32;
begin
  nl := Data[64 + StartPos];
  if (nl < 2) or (nl > 64) then
    Result := 0 else
    Result := nl - 2;

end;

class function TOle2Directory.GetName(const Data: ByteArray; const StartPos: Int32): UTF16String;
begin
{$IFDEF DELPHI2008UP}
  Result := TEncoding.Unicode.GetString(TBytes(Data), StartPos, GetNameSize(Data, StartPos));
{$ELSE}
  SetLength(Result, GetNameSize(Data, StartPos) div 2);
  if Length(Result) > 0 then
  begin
    System.Move(Data[StartPos], Result[1], Length(Result)*2);
  end;
{$ENDIF}
end;

class function TOle2Directory.GetType(const Data: ByteArray; const StartPos: Int32): STGTY;
begin
  Result := STGTY(Data[66 + StartPos]);
end;

class function TOle2Directory.GetSectStart(const Data: ByteArray; const StartPos: Int32): Int64;
begin
  {$R-}
  begin  //return BitConverter.ToUInt32(Data, 0x0074+StartPos);
    begin Result := UInt32(((Data[116 + StartPos] + (Data[117 + StartPos] shl 8)) + (Data[118 + StartPos] shl 16)) + (Data[119 + StartPos] shl 24)); exit; end;
  end;
  {$INCLUDE FLEXCEL.INC}
end;

class procedure TOle2Directory.SetSectStart(const Data: ByteArray; const StartPos: Int32; const value: Int64);
var
  tPos: Int32;
begin
  {$R-}
  begin  //BitConverter.GetBytes((UInt32)value).CopyTo(Data,0x0074+StartPos);
    tPos := 116 + StartPos;
    Data[tPos] := Byte(value);
    Data[tPos + 1] := Byte(value shr 8);
    Data[tPos + 2] := Byte(value shr 16);
    Data[tPos + 3] := Byte(value shr 24);
  end;
  {$INCLUDE FLEXCEL.INC}
end;

class function TOle2Directory.GetSize(const Data: ByteArray; const StartPos: Int32): Int64;
begin
  {$R-}
  begin  // return BitConverter.ToUInt32(Data, 0x0078+StartPos);
    begin Result := UInt32(((Data[120 + StartPos] + (Data[121 + StartPos] shl 8)) + (Data[122 + StartPos] shl 16)) + (Data[123 + StartPos] shl 24)); exit; end;
  end;
  {$INCLUDE FLEXCEL.INC}
end;

class procedure TOle2Directory.SetSize(const Data: ByteArray; const StartPos: Int32; const value: Int64);
var
  tPos: Int32;
begin
  {$R-}
  begin  //BitConverter.GetBytes((UInt32)value).CopyTo(Data,0x0078+StartPos);
    tPos := 120 + StartPos;
    Data[tPos] := Byte(value);
    Data[tPos + 1] := Byte(value shr 8);
    Data[tPos + 2] := Byte(value shr 16);
    Data[tPos + 3] := Byte(value shr 24);
  end;
  {$INCLUDE FLEXCEL.INC}
end;

class procedure TOle2Directory.Clear(const Data: ByteArray; const StartPos: Int32);
begin
  FillChar(Data[StartPos], ((64 + 2) //Clear name and name length.
                            + 1) //StgType invalid
                            + 1, 0);//DeColor

   //Data[StartPos+64+2]=0;
  FillChar(Data[StartPos + 68], 4, 1);  //Left Sibling
  FillChar(Data[StartPos + 72], 4, 1);  //Right Sibling
  FillChar(Data[StartPos + 76], 4, 1);  //Child Sibling
  FillChar(Data[StartPos + 80], TOle2Directory_DirectorySize - 80, 0);  //All else
end;

class function TOle2Directory.GetLeftSid(const Data: ByteArray; const StartPos: Int32): Int32;
begin
  Result := Int32((@Data[68 + StartPos])^);
end;

class procedure TOle2Directory.SetLeftSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
begin
  System.Move(value, Data[68 + StartPos], SizeOf(value));
end;

class function TOle2Directory.GetRightSid(const Data: ByteArray; const StartPos: Int32): Int32;
begin
  Result := Int32((@Data[72 + StartPos])^);
end;

class procedure TOle2Directory.SetRightSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
begin
  System.Move(value, Data[72 + StartPos], SizeOf(value));
end;

class function TOle2Directory.GetChildSid(const Data: ByteArray; const StartPos: Int32): Int32;
begin
  Result := Int32((@Data[76 + StartPos])^);
end;

class procedure TOle2Directory.SetChildSid(const Data: ByteArray; const StartPos: Int32; const value: Int32);
begin
  System.Move(value, Data[76 + StartPos], SizeOf(value));
end;

class function TOle2Directory.GetColor(const Data: ByteArray; const StartPos: Int32): DECOLOR;
begin
  Result := DECOLOR(Data[67 + StartPos]);
end;

class procedure TOle2Directory.SetColor(const Data: ByteArray; const StartPos: Int32; const value: DECOLOR);
begin
  Data[67 + StartPos] := Byte(value);
end;

function TOle2Directory.Get_NameSize(): Int32;
begin
  Result := GetNameSize(Data, 0);
end;

procedure TOle2Directory.Set_NameSize(const value: Int32);
begin
  if (value < 0) or (value > 62) then
    raise Exception.CreateFmt(ErrTooManyEntries, [value, 62]);

  Data[64] := Byte(value + 2);
end;

function TOle2Directory.Get_Name(): UTF16String;
begin
  Result := GetName(Data, 0);
end;

procedure TOle2Directory.Set_Name(const value: UTF16String);
var
  aValue: UTF16String;
  i: integer;
  len: Integer;
begin
  SetLength(aValue, 32);
  len := 32;
  if Length(Value) < len then len := Length(Value);
  if len > 0 then System.Move(value[1], aValue[1], len * 2);
  for i  := len + 1 to 32 do aValue[i] := #0;

  NameSize := len * 2;
{$IFDEF DELPHI2008UP}
  TEncoding.Unicode.GetBytes(aValue, 1, len, TBytes(Data), 0);
{$ELSE}
  System.Move(aValue[1], Data[0], len);
{$ENDIF}
end;

function TOle2Directory.Get_ObjType(): STGTY;
begin
  Result := GetType(Data, 0);
end;

procedure TOle2Directory.Set_ObjType(const value: STGTY);
begin
  Data[66] := Byte(value);
end;

function TOle2Directory.Get_SectStart(): Int64;
begin
  Result := GetSectStart(Data, 0);
end;

procedure TOle2Directory.Set_SectStart(const value: Int64);
begin
  SetSectStart(Data, 0, value);
end;

function TOle2Directory.Get_xulSize(): Int64;
begin
  Result := GetSize(Data, 0);
end;

procedure TOle2Directory.Set_xulSize(const value: Int64);
begin
  SetSize(Data, 0, value);
end;

{.$endregion}
{.$region 'TSectorBuffer'}
{ TSectorBuffer }
constructor TSectorBuffer.Create(const aHeader: TOle2Header; const aStream: TStream);
begin
  inherited Create;
  //Initializations
  Changed := false;
  FSectorId := -1;
  
  Header := aHeader;
  DataStream := aStream;
  SetLength (Data, Header.SectorSize);
  FillChar(Data[0], Length(Data), 0);
  Changed := false;
  FSectorId := -1;
end;

procedure TSectorBuffer.Load(const SectNo: Int64);
begin
  if Changed then
    Save;
  
  if SectNo = FSectorId then
    exit;
  
  DataStream.Seek(Header.SectToStPos(SectNo), soFromBeginning);
  FSectorId := -1;  //It is invalid until we read the data.
  StreamRead(DataStream, Data, 0, Length(Data), false);
  FSectorId := SectNo;
end;

procedure TSectorBuffer.Save();
begin
  if Changed then
  begin
    DataStream.Seek(Header.SectToStPos(FSectorId), soFromBeginning);
    DataStream.WriteBuffer(Data[0], Length(Data));
    Changed := false;
  end;

end;

procedure TSectorBuffer.Read(const aBuffer: ByteArray; const BufferPos: Int64; out nRead: Int64; const StartPos: Int64; const Count: Int64; const SectorSize: Int64);
begin
  if Count > (SectorSize - StartPos) then
    nRead := SectorSize - StartPos else
    nRead := Count;

  System.Move(Data[Int32(StartPos)], aBuffer[Int32(BufferPos)], Int32(nRead));  //The (int) are to be compatible with CF
end;

procedure TSectorBuffer.ReadMem(var aBuffer; const BufferPos: Int64; out nRead: Int64; const StartPos: Int64; const Count: Int64; const SectorSize: Int64);
var
  MemBuffer: PArrayOfByte;
begin
  if Count > (SectorSize - StartPos) then
  nRead := SectorSize - StartPos else
  nRead := Count;

  MemBuffer := PArrayOfByte(@aBuffer);
  System.Move(Data[Int32(StartPos)], Membuffer[Int32(BufferPos)], Int32(nRead));  //The (int) are to be compatible with CF
end;

//--------------------------------------------------------------------------------------------------------------------------//{.$endregion}

{.$region 'TOle2File'}
{ TOle2File }
constructor TOle2File.Create(const aStream: TStream);
begin
  //private TOle2DirList DirList;
  Create(aStream, false);
end;

constructor TOle2File.Create(const aStream: TStream; const AvoidExceptions: Boolean);
var
  StreamPosition: Int64;
begin
  inherited Create;
  //Initializations
  TOle2FileStr := 'TOle2File';
  disposed := false;
  PreparedForWrite := false;
  DIRStartPos := -1;
  
  FStream := aStream;
  StreamPosition := aStream.Position;
  Header := TOle2Header.Create(FStream, AvoidExceptions);
  if Header.NotXls97 then
  begin
    NotXls97 := true;
    FStream.Position := StreamPosition;
    exit;
  end;
  
  FAT := TOle2FAT.Create(Header, FStream);
  MiniFAT := TOle2MiniFAT.Create(Header, FStream, FAT);
  ROOT := FindRoot;
  SectorBuffer := TSectorBuffer.Create(Header, FStream);
  FEncryption := TEncryptionData.Create('', nil, nil);
end;

procedure TOle2File.Close();
begin
  Destroy;
end;

destructor TOle2File.Destroy;
begin
  try try try try try try try
  FinishStream;
  finally
    FreeAndNil(Header);
  end;
  finally
    FreeAndNil(MiniFat);
  end;
  finally
    FreeAndNil(FAT);
  end;
  finally
    FreeAndNil(SectorBuffer);
  end;
  finally
    FreeAndNil(FEncryption);
  end;
  finally
    FreeAndNil(DIR);
  end;
  finally
    FreeAndNil(ROOT);
  end;
  inherited;
end;

function TOle2File.FindDir(const DirName: UTF16String): TOle2Directory;
var
  Data: ByteArray;
  DirSect: Int64;
  k: UInt32;
  nd: ByteArray;
begin
  Result := nil;
  SetLength (Data, Header.SectorSize);
  FillChar(Data[0], System.Length(Data), 0);
  DirSect := Header.sectDirStart;
  while DirSect <> TOle2Header_ENDOFCHAIN do
  begin
    begin
      FStream.Seek(Header.SectToStPos(DirSect), soFromBeginning);
      StreamRead(FStream, Data, 0, System.Length(Data), false);
      k := 0;
      while k < Header.SectorSize do
      try
        if  SameText(TOle2Directory.GetName(Data, k), DirName) then
        begin
          SetLength (nd, TOle2Directory_DirectorySize);
          FillChar(nd[0], System.Length(nd), 0);
          System.Move(Data[k], nd[0], System.Length(nd));
          begin Result := TOle2Directory.Create(nd); exit; end;
        end;

      finally
        k:= k + TOle2Directory_DirectorySize;
      end;

      DirSect := FAT.GetNextSector(DirSect);
    end;
  end;
end;

function TOle2File.FindRoot(): TOle2Directory;
var
  Data: ByteArray;
  DirSect: Int64;
begin
  SetLength (Data, TOle2Directory_DirectorySize);
  FillChar(Data[0], System.Length(Data), 0);
  DirSect := Header.sectDirStart;
  FStream.Seek(Header.SectToStPos(DirSect), soFromBeginning);
  StreamRead(FStream, Data, 0, System.Length(Data), false);
  Result := TOle2Directory.Create(Data);
end;

procedure TOle2File.SelectStream(const StreamName: UTF16String);
begin
  if PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, [StreamName]);

  FreeAndNil(DIR);  
  DIR := FindDir(StreamName);
  if DIR = nil then
    raise Exception.CreateFmt(ErrFileIsNotXLS,[FileName]);

   //XlsMessages.ThrowException(XlsErr.ErrInvalidStream, StreamName);
  StreamPos := 0;
end;

function TOle2File.NextEof(const Count: Int32): Boolean;
begin
  if PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  Result := (StreamPos + Count) >= Length;
end;

procedure TOle2File.ReadMem(var aBuffer; const Count: Int32);
var
  DIRulSize: Int64;
  MiniSectsOn1Sect: Int64;
  MiniFatSectorOfs: Int64;
  ActualMiniFatSector: Int64;
  SectorOfs: Int64;
  MiniStreamSector: Int64;
  nRead: Int64;
  TotalRead: Int64;
  MiniOffset: Int64;
  MiniStart: Int64;
  ActualSector: Int64;
begin
  if Count = 0 then  // this is needed to avoid reading into a free record.
    exit;

  if DIR = nil then  //No stream selected
    exit;

  if PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  DIRulSize := DIR.ulSize;
  if (StreamPos + Count) > DIRulSize then  //Reading past the end of the stream.
    raise Exception.CreateFmt(ErrEofReached, [(StreamPos + Count) - DIRulSize]);

  if DIRulSize < Header.ulMiniSectorCutoff then
  begin
    MiniSectsOn1Sect := 1 shl (Header.uSectorShift - Header.uMiniSectorShift);  //Read from the MiniFat.
    MiniFatSectorOfs := StreamPos shr Header.uMiniSectorShift;  //Find the minifat Sector number we have to read
    ActualMiniFatSector := MiniFAT.FindSector(DIR.SectStart, MiniFatSectorOfs);
    SectorOfs := ActualMiniFatSector shr (Header.uSectorShift - Header.uMiniSectorShift);  //Now, find this minifat sector into the MiniStream
  // MiniFAT/8
    MiniStreamSector := FAT.FindSector(ROOT.SectStart, SectorOfs);
    nRead := 0;
    TotalRead := 0;
    while TotalRead < Count do
    begin
      begin
        SectorBuffer.Load(MiniStreamSector);
        MiniOffset := (ActualMiniFatSector mod MiniSectsOn1Sect) shl Header.uMiniSectorShift;
        MiniStart := (StreamPos mod Header.MiniSectorSize) + MiniOffset;
        SectorBuffer.ReadMem(aBuffer, TotalRead, nRead, MiniStart, Count - TotalRead, Header.MiniSectorSize + MiniOffset);
        StreamPos:= StreamPos + nRead;
        TotalRead:= TotalRead + nRead;
        if TotalRead < Count then
        begin
          ActualMiniFatSector := MiniFAT.GetNextSector(ActualMiniFatSector);
          SectorOfs := ActualMiniFatSector shr (Header.uSectorShift - Header.uMiniSectorShift);  // MiniFAT/8
          MiniStreamSector := FAT.FindSector(ROOT.SectStart, SectorOfs);
        end;

      end;
    end;
  end
  else
  begin
    SectorOfs := StreamPos shr Header.uSectorShift;  //Read from a normal sector
    ActualSector := FAT.FindSector(DIR.SectStart, SectorOfs);
    nRead := 0;
    TotalRead := 0;
    while TotalRead < Count do
    begin
      begin
        SectorBuffer.Load(ActualSector);
        SectorBuffer.ReadMem(aBuffer, TotalRead, nRead, StreamPos mod Header.SectorSize, Count - TotalRead, Header.SectorSize);
        StreamPos:= StreamPos + nRead;
        if TotalRead < Count then
        begin
          TotalRead:= TotalRead + nRead;
          ActualSector := FAT.GetNextSector(ActualSector);
        end;

      end;
    end;
  end;

end;

procedure TOle2File.Read(const aBuffer: ByteArray; const Count: Int32);
begin
  if System.Length(aBuffer) = 0 then  // this is needed to avoid reading into a free record.
    exit;

  ReadMem(aBuffer[0], Count);
end;

procedure TOle2File.WriteRawMem(const Buffer; const Count: Int32);
begin
  if DIR = nil then  //No stream selected
    exit;

  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  FStream.WriteBuffer(Buffer, Count);
  DIR.ulSize:= DIR.ulSize + Count;
end;

procedure TOle2File.WriteRaw(const Buffer: ByteArray; const Count: Int32);
begin
  if DIR = nil then  //No stream selected
    exit;

  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  FStream.WriteBuffer(Buffer[0], Count);
  DIR.ulSize:= DIR.ulSize + Count;
end;

procedure TOle2File.WriteMem(const Buffer; const Count: Int32);
begin
  //Missing Encryption
  WriteRawMem(Buffer, Count);
end;

procedure TOle2File.Write(Buffer: ByteArray; const Count: Int32);
begin
  if (FEncryption <> nil) and (FEncryption.Engine <> nil) then
    Buffer := FEncryption.Engine.Encode(Buffer, Position, 0, System.Length(Buffer), FEncryption.ActualRecordLen);

  WriteRaw(Buffer, Count);
end;

procedure TOle2File.WriteRaw(const Buffer: ByteArray; const StartPos: Int32; const Count: Int32);
begin
  if DIR = nil then  //No stream selected
    exit;

  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  FStream.WriteBuffer(Buffer[StartPos], Count);
  DIR.ulSize:= DIR.ulSize + Count;
end;

procedure TOle2File.WriteHeader(const Id: UInt16; const Len: UInt16);
var
  Header: ByteArray;
begin
  SetLength (Header, 4);
  System.Move(Id, Header[0], 2);
  System.Move(Len, Header[0], 2);

  WriteRaw(Header, 0, System.Length(Header));
  FEncryption.ActualRecordLen := Len;
end;

procedure TOle2File.Write(Buffer: ByteArray; const StartPos: Int32; const Count: Int32);
begin
  if DIR = nil then  //No stream selected
    exit;

  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  if (FEncryption <> nil) and (FEncryption.Engine <> nil) then
    Buffer := FEncryption.Engine.Encode(Buffer, Position, StartPos, Count, FEncryption.ActualRecordLen);

  FStream.WriteBuffer(Buffer[StartPos], Count);
  DIR.ulSize:= DIR.ulSize + Count;
end;

procedure TOle2File.Write16(Buffer: UInt16);
begin
  if DIR = nil then  //No stream selected
    exit;
  
  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);
  
  if (FEncryption <> nil) and (FEncryption.Engine <> nil) then
    Buffer := FEncryption.Engine.Encode(Buffer, Position, FEncryption.ActualRecordLen);

  FStream.WriteBuffer(Buffer, 2);
  DIR.ulSize:= DIR.ulSize + 2;
end;

procedure TOle2File.Write32(Buffer: UInt32);
begin
  if (FEncryption <> nil) and (FEncryption.Engine <> nil) then
    Buffer := FEncryption.Engine.Encode(Buffer, Position, FEncryption.ActualRecordLen);

  if DIR = nil then  //No stream selected
    exit;

  if not PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  FStream.WriteBuffer(Buffer, 4);
  DIR.ulSize:= DIR.ulSize + 4;
end;

class function TOle2File.FindString(const s: UTF16String; const list: StringArray): Boolean;
var
  i: Int32;
begin
  for i := 0 to System.Length(list) - 1 do
    if list[i] = s then
      begin Result := true; exit; end;


  Result := false;
end;

procedure TOle2File.SeekForward(const Offset: Int64);
var
  Tmp: ByteArray;
begin
  if Position > Offset then
    raise Exception.Create(ErrInvalidPropertySector);

  if Offset > Position then
  begin
    SetLength (Tmp, Offset - Position);
    FillChar(Tmp[0], System.Length(Tmp), 0);
    Read(Tmp, System.Length(Tmp));
  end;
  
end;

procedure TOle2File.MarkDeleted(const i: Int32; const Result: TDirEntryList; const Level: Int32);
begin
  if Result[i].Deleted then
    exit;
  
  Result[i].Deleted := true;
  if Result[i].ChildSid >= 0 then
    MarkDeleted(Result[i].ChildSid, Result, Level + 1);
  
  if (Level > 0) and (Result[i].LeftSid >= 0) then
    MarkDeleted(Result[i].LeftSid, Result, Level + 1);
  
  if (Level > 0) and (Result[i].RightSid >= 0) then
    MarkDeleted(Result[i].RightSid, Result, Level + 1);
  
end;

class procedure TOle2File.DeleteNode(const Result: TDirEntryList; var ParentLeaf: Int32);
var
  NextNode: Int32;
  PreviousNode: Int32;
begin
  if (Result[ParentLeaf].LeftSid < 0) and (Result[ParentLeaf].RightSid < 0) then  //It is a final node
  begin
    ParentLeaf := -1;
    exit;
  end;
  
  if Result[ParentLeaf].LeftSid < 0 then  //Only right branch.
  begin
    ParentLeaf := Result[ParentLeaf].RightSid;
    exit;
  end;
  
  if Result[ParentLeaf].RightSid < 0 then  //Only left branch.
  begin
    ParentLeaf := Result[ParentLeaf].LeftSid;
    exit;
  end;
  
  NextNode := Result[ParentLeaf].RightSid;  //Leaf has both branchs.
  //Relabel the node as its successor and delete the successor
  //--------------------------------------------------------------//
  //Example: Delete node 3 here
  //           10
  //        3       
  //     2      6     
  //          4    7
  //            5    
  // We need to relabel 4 as 3, and hang 5 from 6.
  //--------------------------------------------------------------//
  //Find the next node. (once to the right and then always left)
  PreviousNode := -1;
  while Result[NextNode].LeftSid >= 0 do
  begin
    begin
      PreviousNode := NextNode;
      NextNode := Result[NextNode].LeftSid;
    end;
  end;

   //Rename it.
  Result[NextNode].LeftSid := Result[ParentLeaf].LeftSid;  //LeftSid is always-1, we are at the left end.
  if PreviousNode >= 0 then  //If parentNode=-1, we are at the first node (6 on the example) and we don't have to fix the right part.
  begin
    if Result[NextNode].RightSid >= 0 then
      Result[PreviousNode].LeftSid := Result[NextNode].RightSid else
      Result[PreviousNode].LeftSid := -1;
    
    Result[NextNode].RightSid := Result[ParentLeaf].RightSid;
  end;
  
  ParentLeaf := NextNode;
end;

procedure TOle2File.FixNode(const Result: TDirEntryList; var ParentNode: Int32);
begin
  while (ParentNode > 0) and Result[ParentNode].Deleted do
  begin
    DeleteNode(Result, ParentNode)end;
  if ParentNode < 0 then
    exit;
  
  if Result[ParentNode].LeftSid >= 0 then
    FixNode(Result, Result[ParentNode].LeftSid);
  
  if Result[ParentNode].RightSid >= 0 then
    FixNode(Result, Result[ParentNode].RightSid);
  
  if Result[ParentNode].ChildSid >= 0 then
    FixNode(Result, Result[ParentNode].ChildSid);
  
end;

function TOle2File.ReadDirs(const DeletedStorages: StringArray; var PaintItBlack: Boolean): TDirEntryList;
var
  DirSect: Int64;
  DirSector: ByteArray;
  i: Int32;
  FakeParent: Int32;
begin
  Result := TDirEntryList.Create;
  try
    DirSect := Header.sectDirStart;
    SetLength (DirSector, Header.SectorSize);
    FillChar(DirSector[0], System.Length(DirSector), 0);
    while DirSect <> TOle2Header_ENDOFCHAIN do
    begin
      begin
        FStream.Seek(Header.SectToStPos(DirSect), soFromBeginning);
         //Read the whole sector, tipically 4 DIR entries.
        StreamRead(FStream, DirSector, 0, System.Length(DirSector), false);
        i := 0;
        while i < System.Length(DirSector) do
        try
          Result.Add(TOneDirEntry.Create(TOle2Directory.GetName(DirSector, i), TOle2Directory.GetLeftSid(DirSector, i), TOle2Directory.GetRightSid(DirSector, i), TOle2Directory.GetChildSid(DirSector, i), TOle2Directory.GetColor(DirSector, i)));
        finally
          i:= i + TOle2Directory_DirectorySize;
        end;
      
        DirSect := FAT.GetNextSector(DirSect);
      end;
    end;
     // Tag deleted storages and its children.
    for i := 1 to Result.Count - 1 do  //Skip 0, we can't delete root.
    begin
      if FindString(Result[i].Name, DeletedStorages) then
      begin
        MarkDeleted(i, Result, 0);
        if Result[i].Color = DECOLOR_BLACK then
          PaintItBlack := true;
      
      end;
    
    end;
  
    FakeParent := 0;  //Now that we know the deletes, delete the nodes from the red/black tree.
    FixNode(Result, FakeParent);
    Assert(FakeParent = 0, 'Can''t delete root');
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TOle2File.PrepareForWrite(const OutStream: TStream; const OStreamName: UTF16String; const DeleteStorages: StringArray);
var
  DIRStartOfs: Int32;
  NewFat: TOle2FAT;
  DirSect: Int64;
  LastDirPos: Int32;
  IniPos: Int64;
  DirSector: ByteArray;
  DataSector: ByteArray;
  PaintItBlack: Boolean;
  DirEntries: TDirEntryList;
  CurrentDirPos: Int32;
  i: Int32;
  SType: STGTY;
  StreamSect: Int64;
  StreamSize: Int64;
  bRead: Int64;
  nd: ByteArray;
  LastMiniFatPos: Int32;
  MiniFatSect: Int64;
begin
  if PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);
  
  DIRStartPos := -1;
  DIRStartOfs := -1;
  NewFat := TOle2FAT.Create(Header, nil);
  try
    LastDirPos := -1;
    IniPos := OutStream.Position;
    SetLength (DirSector, Header.SectorSize);
    FillChar(DirSector[0], System.Length(DirSector), 0);
    SetLength (DataSector, Header.SectorSize);
    FillChar(DataSector[0], System.Length(DataSector), 0);
    FreeAndNil(DIR);
    PaintItBlack := false;  //We are not going to mess with red/black things. If a recolor on the tree is needed, we will paint it all black.
    DirEntries := nil;
    try

      if System.Length(DeleteStorages) > 0 then  //Find all the storages and streams to delete, and patch the others not to point to them.
        DirEntries := ReadDirs(DeleteStorages, PaintItBlack);
  
      DirSect := Header.sectDirStart;
      OutStream.Seek(Header.SectToStPos(0, IniPos), soFromBeginning);  //Advance to the first sector.
      CurrentDirPos := 0;
       //Copy the Dir tree and their asociated streams. If stream is OStreamName, set its size to 0.
      while DirSect <> TOle2Header_ENDOFCHAIN do
      begin
        begin
          FStream.Seek(Header.SectToStPos(DirSect), soFromBeginning);
           //Read the whole sector, tipically 4 DIR entries.
          StreamRead(FStream, DirSector, 0, System.Length(DirSector), false);
          i := 0;
          while i < System.Length(DirSector) do
          try
            SType := TOle2Directory.GetType(DirSector, i);
            if PaintItBlack then
              TOle2Directory.SetColor(DirSector, i, DECOLOR_BLACK);
        
            if (DirEntries <> nil) and not DirEntries[CurrentDirPos].Deleted then
            begin
               //Fix the tree.
              TOle2Directory.SetLeftSid(DirSector, i, DirEntries[CurrentDirPos].LeftSid);
              TOle2Directory.SetRightSid(DirSector, i, DirEntries[CurrentDirPos].RightSid);
              TOle2Directory.SetChildSid(DirSector, i, DirEntries[CurrentDirPos].ChildSid);
            end;
        
            if (DirEntries <> nil) and DirEntries[CurrentDirPos].Deleted then
            begin
              TOle2Directory.Clear(DirSector, i);
            end
            else
              if ((SType = STGTY_STREAM) and ((TOle2Directory.GetSize(DirSector, i) >= Header.ulMiniSectorCutoff) or (SameText(TOle2Directory.GetName(DirSector, i), OStreamName)))) or (SType = STGTY_ROOT) then

                 //When ROOT, the stream is the MiniStream.  When Sectors reference the ministream, the data is not copied, as the whole ministream was copied with root.
                if not SameText(TOle2Directory.GetName(DirSector, i), OStreamName) then
                begin
                  StreamSect := TOle2Directory.GetSectStart(DirSector, i);  //Arrange FAT
                  StreamSize := TOle2Directory.GetSize(DirSector, i);
                  bRead := 0;
                  if StreamSect <> TOle2Header_ENDOFCHAIN then
                    TOle2Directory.SetSectStart(DirSector, i, NewFat.Count);

                  while (StreamSect <> TOle2Header_ENDOFCHAIN) and (bRead < StreamSize) do
                  begin
                    begin
                       //Copy old Sector to New sector
                      FStream.Seek(Header.SectToStPos(StreamSect), soFromBeginning);
                      StreamRead(FStream, DataSector, 0, System.Length(DataSector), false);
                      OutStream.WriteBuffer(DataSector[0], System.Length(DataSector));
                      Assert((OutStream.Position - IniPos) = (Header.SectToStPos(NewFat.Count + 1) - Header.StartOfs), 'New Stream IO Error');
                       //Update The Fat
                      StreamSect := FAT.GetNextSector(StreamSect);
                      bRead:= bRead + Header.SectorSize;
                      if (StreamSect <> TOle2Header_ENDOFCHAIN) and (bRead < StreamSize) then
                        NewFat.Add(UInt32(NewFat.Count) + 1) else
                        NewFat.Add(TOle2Header_ENDOFCHAIN);

                    end;
                  end;
                end
                else
                begin
                  TOle2Directory.SetSectStart(DirSector, i, TOle2Header_ENDOFCHAIN);
                  TOle2Directory.SetSize(DirSector, i, 0);
                  SetLength (nd, TOle2Directory_DirectorySize);
                  System.Move(DirSector[i], nd[0], System.Length(nd));
                  DIR := TOle2Directory.Create(nd);
                  DIRStartOfs := i;
                end;

            Inc(CurrentDirPos);
          finally
            i:= i + TOle2Directory_DirectorySize;
          end;


           //Save the DIR Sector
          if DIRStartOfs >= 0 then
          begin
            DIRStartPos := OutStream.Position + DIRStartOfs;  //We must save the position here, just before writing the sector.
            DIRStartOfs := -1;
          end;

          OutStream.WriteBuffer(DirSector[0], System.Length(DirSector));
           //Add a new entry on the FAT for the new DIR sector.
          NewFat.Add(TOle2Header_ENDOFCHAIN);
          if LastDirPos > 0 then  //Chain the last FAT DIR point to this.
            NewFat[LastDirPos] := UInt32(NewFat.Count) - 1 else
            Header.sectDirStart := UInt32(NewFat.Count) - 1;
      
          DirSect := FAT.GetNextSector(DirSect);
          LastDirPos := NewFat.Count - 1;
        end;
      end;
    finally
      FreeAndNil(DirEntries);
    end;
    if DIR = nil then
      raise Exception.CreateFmt(ErrInvalidStream, ['']);
  
    LastMiniFatPos := -1;  //Copy the MiniFat
    MiniFatSect := Header.sectMiniFatStart;
    while MiniFatSect <> TOle2Header_ENDOFCHAIN do
    begin
      begin
        FStream.Seek(Header.SectToStPos(MiniFatSect), soFromBeginning);
         //Read the whole sector, tipically 128 MiniFat entries.
        StreamRead(FStream, DataSector, 0, System.Length(DataSector), false);
        OutStream.WriteBuffer(DataSector[0], System.Length(DataSector));
        NewFat.Add(TOle2Header_ENDOFCHAIN);
        if LastMiniFatPos > 0 then  //Chain the last FAT MiniFat point to this.
          NewFat[LastMiniFatPos] := UInt32(NewFat.Count) - 1 else
          Header.sectMiniFatStart := UInt32(NewFat.Count) - 1;
      
        MiniFatSect := FAT.GetNextSector(MiniFatSect);
        LastMiniFatPos := NewFat.Count - 1;
      end;
    end;
  except
    FreeAndNil(NewFat);
    raise;
  end;

   //Switch to the new Stream.
  FreeAndNil(FAT); 
  FAT := NewFat;
   //MiniFat stays the same.

  FStream := OutStream;
  Header.StartOfs := IniPos;
  FreeAndNil(SectorBuffer);
  SectorBuffer := TSectorBuffer.Create(Header, FStream);
  FreeAndNil(ROOT);  //No need for it when writing.
  PreparedForWrite := true;
  DIR.SectStart := NewFat.Count;
  DIR.ulSize := 0;
end;

procedure TOle2File.FinishStream();
var
  Data: ByteArray;
  OldDifSectorCount: Int64;
  DifSectorCount: Int64;
  FATSectorCount: Int64;
  FATEntryCount0: Int64;
  FatEntryDelta: Int64;
  OldFatEntryDelta: Int64;
  DifInHeader: ByteArray;
  StartDif: Int64;
  StartFat: Int64;
  f: Int32;
  i: Int32;
  DifSectorData: ByteArray;
  k: Int32;
  SectEnd: Int32;
  OneByte: byte;
  FourBytes: UInt32;
begin
  if not PreparedForWrite then
    exit;
  

   //Ensure Workbook has at least 4096 bytes, so it doesn't go to the MiniStream.
  if DIR.ulSize < Header.ulMiniSectorCutoff then
  begin
    SetLength (Data, Header.ulMiniSectorCutoff - DIR.ulSize);  //Filled with 0.
    FillChar(Data[0], System.Length(Data), 0);
    WriteRaw(Data, 0, System.Length(Data));
  end;
  

   //Fill the rest of the sector with 0s. 
  if (DIR.ulSize mod Header.SectorSize) > 0 then
  begin
    SetLength (Data, Header.SectorSize - (DIR.ulSize mod Header.SectorSize));  //Filled with 0.
    FillChar(Data[0], System.Length(Data), 0);
    WriteRaw(Data, 0, System.Length(Data));
    DIR.ulSize:= DIR.ulSize - System.Length(Data);  //This does not count to the final stream size
  end;
  
  //Fix Header. Fat count &sect, dif count & sect.  //Minifat and Dir are already fixed.
  DifSectorCount := 0;
  repeat  //Iterate to get the real dif/fat count. Adding a dif sector might a fat sector, so it might add another dif... luckily this will converge really fast.
  //Also, the fat sectors should be included on the fat count. If it weren't discrete, it would be a nice 3 x equation.
    FATEntryCount0 := ((FAT.Count + 1) + (((DIR.ulSize - 1) shr Header.uSectorShift) + 1)) + DifSectorCount;
    FatEntryDelta := ((FATEntryCount0 - 1) shr FAT.uFATEntryShift) + 1;  //first guess

    repeat
      OldFatEntryDelta := FatEntryDelta;
      FatEntryDelta := (((FATEntryCount0 + FatEntryDelta) - 1) shr FAT.uFATEntryShift) + 1;
     (* This converges, because FatEntryDelta>=OldFatEntryDelta
      * To prove Fed[n+1]>=Fed[n], lets begin... (n=0): Fed[0]=0 <= Fed[1]=(0+FEC0)/128.
      * (n=k):  if Fed[k]>=Fed[k-1] -> (n=k+1):  Fed[k+1]=(FEC0+Fed[n])/128
      * As Fed[n]>=Fed[n-1], FEC0>0 ->  (FEC0+Fed[n])/128)>=(FEC0+Fed[n-1])/128  ->
      *
      * Fed[n+1]>=(FEC0+Fed[n-1])/128=Fed[n]  ;-)
      *)
    until not (FatEntryDelta <> OldFatEntryDelta);



    FATSectorCount := (((FATEntryCount0 + FatEntryDelta) - 1) shr FAT.uFATEntryShift) + 1;
    OldDifSectorCount := DifSectorCount;
    if FATSectorCount > TOle2Header_DifsInHeader then
      DifSectorCount := ((FATSectorCount - TOle2Header_DifsInHeader - 1) div (Header.SectorSize div 4 - 1)) + 1;  //The last diff entry is a pointer to the new diff sector, so we have 127 slots, not 128.

  until not (OldDifSectorCount <> DifSectorCount);
  Header.csectFat := UInt32(FATSectorCount);
  Header.csectDif := UInt32(DifSectorCount);
  if DifSectorCount > 0 then
    Header.sectDifStart := UInt32((FAT.Count + 1) + (((DIR.ulSize - 1) shr Header.uSectorShift) + 1)) else
    Header.sectDifStart := TOle2Header_ENDOFCHAIN;


   //Save DIR
  FStream.Seek(DIRStartPos, soFromBeginning);
  DIR.Save(FStream);

   //Save Header.
  FStream.Seek(Header.StartOfs, soFromBeginning);
  Header.Save(FStream);

   //Save DIF in header
  SetLength (DifInHeader, TOle2Header_DifEntries);
  FillChar(DifInHeader[0], System.Length(DifInHeader), 0);
  StartDif := (FAT.Count + 1) + (((DIR.ulSize - 1) shr Header.uSectorShift) + 1);
  StartFat := StartDif + DifSectorCount;

  if FATSectorCount <= TOle2Header_DifsInHeader then
    f := Int32(FATSectorCount) else
    f := TOle2Header_DifsInHeader;
  
  for i := 0 to f - 1 do
  begin
    FourBytes := UInt32(StartFat + i);
    System.Move(FourBytes, DifInHeader[i shl 2], 4);
  end;

   //Docs say there should be an endofchain at the last slot of the last dif sector, but there is none. Only unused sectors (FFFF)
  for i := f shl 2 to System.Length(DifInHeader) - 1 do
    DifInHeader[i] := 255;

  FStream.WriteBuffer(DifInHeader[0], System.Length(DifInHeader));
  SetLength (DifSectorData, Header.SectorSize);
  FillChar(DifSectorData[0], System.Length(DifSectorData), 0);
   //Save DIF Sectors.
  FStream.Seek(Header.SectToStPos(StartDif), soFromBeginning);
  for k := 0 to DifSectorCount - 1 do
  begin
    SectEnd := Int32(Header.SectorSize) - 4;
    if k = (DifSectorCount - 1) then
      SectEnd := Int32(((FATSectorCount - TOle2Header_DifsInHeader - 1) mod (Header.SectorSize div 4 - 1) + 1) shl 2);

    i := 0;
    while i < SectEnd do
    begin
      FourBytes := UInt32(((StartFat + (i shr 2)) + TOle2Header_DifsInHeader) + Int64(k) * (Header.SectorSize div 4 - 1));
      System.Move(FourBytes, DifSectorData[i], 4);
      i:= i + 4;
    end;

    i := SectEnd;
    while i < Int32(Header.SectorSize - 4) do
    begin
      System.Move(UInt32(TOle2Header_FREESECT), DifSectorData[i], 4);
      i:= i + 4;
    end;

    if k = (DifSectorCount - 1) then
      System.Move(UInt32(TOle2Header_FREESECT), DifSectorData[Header.SectorSize - 4], 4) else
      begin
        FourBytes := UInt32((StartDif + k) + 1);
        System.Move(FourBytes, DifSectorData[Header.SectorSize - 4], 4);
      end;

    FStream.WriteBuffer(DifSectorData[0], System.Length(DifSectorData));
  end;


   //Write FAT for unmodified storages/streams
  for k := 0 to FAT.Count - 1 do
  begin
    FourBytes := UInt32(FAT[k]);
    FStream.WriteBuffer(FourBytes, 4);
  end;

   //Write Stream FAT
  for k := 0 to ((DIR.ulSize - 1) shr Header.uSectorShift) + 1 - 1 do
  begin
    FourBytes := UInt32((FAT.Count + k) + 1);
    FStream.WriteBuffer(FourBytes, 4);
  end;

  FStream.WriteBuffer(UInt32(TOle2Header_ENDOFCHAIN), 4);

   //Write DIF FAT
  for k := 0 to DifSectorCount - 1 do
    FStream.WriteBuffer(UInt32(TOle2Header_DIFSECT), 4);

   //Write FAT FAT
  for k := 0 to FATSectorCount - 1 do
    FStream.WriteBuffer(UInt32(TOle2Header_FATSECT), 4);

   //Fill FAT sector with FF
  OneByte := $FF;
  for k := Int32(((StartFat + FATSectorCount) shl 2) mod Header.SectorSize) to Header.SectorSize - 1 do
    FStream.WriteBuffer(OneByte, 1);
end;

function TOle2File.Get_Length(): Int64;
begin
  if DIR = nil then
    Result := 0 else
    Result := DIR.ulSize;

end;

function TOle2File.Get_Position(): Int64;
begin
  if PreparedForWrite then
    begin Result := DIR.ulSize; exit; end;

  Result := StreamPos;
end;

function TOle2File.Get_Eof(): Boolean;
begin
  if PreparedForWrite then
    raise Exception.CreateFmt(ErrInvalidStream, ['']);

  Result := StreamPos >= Length;
end;

function TOle2File.Get_FileName(): UTF16String;
begin
{$IFDEF FLX_FILESTREAM_HAS_FILENAME}
  if (FStream is TFileStream) then
    Result := (FStream as TFileStream).FileName else Result := '';
{$ELSE}
  Result := '';
{$ENDIF}
end;

{.$endregion}
{ UInt32List }


{$IFNDEF FLX_GENERICS}
procedure UInt32List.Add(const Item: UInt32);
begin
  FList.Add(Pointer(Item));
end;

constructor UInt32List.Create;
begin
  inherited;
  FList := TList.Create;
end;


destructor UInt32List.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;


function UInt32List.GetCapacity: Int32;
begin
  Result := FList.Capacity;
end;


function UInt32List.GetCount: Int32;
begin
  Result := FList.Count;
end;

function UInt32List.GetItems(const i: integer): UInt32;
begin
  Result := UInt32(FList[i]);
end;

procedure UInt32List.SetCapacity(const Value: Int32);
begin
  FList.Capacity := Value;
end;

procedure UInt32List.SetItems(const i: integer; const Value: UInt32);
begin
  FList[i] := Pointer(Value);
end;
{$ENDIF}

constructor TOneDirEntry.Create(const aName: UTF16String; const aLeftSid, aRightSid, aChildSid: Int32; const aColor: DECOLOR);
begin
  Name:=aName;
  LeftSid:=aLeftSid;
  RightSid:=aRightSid;
  ChildSid:=aChildSid;
  Deleted:=false;
  Color:=aColor;
end;


{$IFNDEF FLX_GENERICS}
{ TDirEntryList }

procedure TDirEntryList.Add(const Item: TOneDirEntry);
begin
  FList.Add(Item);
end;


constructor TDirEntryList.Create;
begin
  FList.Create;
  FList.OwnsObjects := true;
end;


destructor TDirEntryList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;


function TDirEntryList.GetCount: Int32;
begin
  Result := FList.Count;
end;


function TDirEntryList.GetItems(const i: integer): TOneDirEntry;
begin
  Result := FList[i] as TOneDirEntry;
end;


procedure TDirEntryList.SetItems(const i: integer; const Value: TOneDirEntry);
begin
  FList[i] := Value;
end;
{$ENDIF}
end.
