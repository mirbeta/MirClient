unit tmsUXlsProtect;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses tmsUFlxMessages;
type
  TEncryptionEngine = class;

  /// <summary>
  /// Holds an encryption engine and a password. Engine has to be created on demand (and is polymorphical) so we need to store password in another place.
  /// </summary>
  TEncryptionData = class
  public
    ReadPassword: UTF16String;
    Engine: TEncryptionEngine;
    ActualRecordLen: Int32;
    constructor Create(const aReadPassword: UTF16String; const aOnPassword: TObject; const aXls: TObject);
    function TotalSize(): Int32;
  end;


  /// <summary>
  /// Base for all encrtyption engines.
  /// </summary>
  TEncryptionEngine = class
  protected
    constructor Create();

  public
    function CheckHash(const Password: UTF16String): Boolean;virtual; abstract;
    function Decode(const Data: ByteArray; const StreamPosition: Int64; const StartPos: Int32; const Count: Int32; const RecordLen: Int32): ByteArray;virtual; abstract;
    function Encode(const Data; const StreamPosition: Int64; const StartPos: Int32; const Count: Int32; const RecordLen: Int32): ByteArray;overload; virtual; abstract;
    function Encode(const Data: UInt16; const StreamPosition: Int64; const RecordLen: Int32): UInt16;overload; virtual; abstract;
    function Encode(const Data: UInt32; const StreamPosition: Int64; const RecordLen: Int32): UInt32;overload; virtual; abstract;
    function GetFilePassRecord(): ByteArray;virtual; abstract;
    function GetFilePassRecordLen(): Int32;virtual; abstract;
  end;

implementation
{ TEncryptionData }
constructor TEncryptionData.Create(const aReadPassword: UTF16String; const aOnPassword: TObject; const aXls: TObject);
begin
  inherited Create;
  ReadPassword := aReadPassword;
end;

function TEncryptionData.TotalSize(): Int32;
begin
  if Engine = nil then
    begin Result := 0; exit; end;
  
  Result := Engine.GetFilePassRecordLen;
end;

{ TEncryptionEngine }
constructor TEncryptionEngine.Create();
begin
  inherited Create;
end;

end.
