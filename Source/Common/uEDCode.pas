unit uEDCode;

interface
  uses Classes, SysUtils;

procedure EncodeStream(InStream, OutStream: TStream; const Key: String);
procedure DecodeStream(InStream, OutStream: TStream; const Key: String);
function EncodeString(const Source, Key: String): AnsiString;
function DecodeString(const Source: AnsiString; const Key: String): String;
function EncodeSource(const Source: String): AnsiString;
function DecodeSource(const Source: AnsiString): String;
function EncodeData(const Source; const Size: Integer; const Key: String): AnsiString; overload;
procedure DecodeData(const Source: AnsiString; var Dest; const Size: Integer; const Key: String); overload;
procedure EncodeData(const Source; const BufSize: Integer; const Key: String; var Dest: PAnsiChar; var Size: Integer); overload;
procedure DecodeData(const Source; const BufSize: Integer; var Dest: PAnsiChar; var Size: Integer; const Key: String); overload;
procedure EncodeMemory(Source: PAnsiChar; Dest: TMemoryStream; const Size: Integer; const Key: String);
procedure DecodeMemory(Source: TMemoryStream; Dest: PAnsiChar; const Size: Integer; const Key: String);
function EncodeSourceData(const Source; const Size: Integer): AnsiString;
procedure DecodeSourceData(const Source: AnsiString; var Dest; const Size: Integer);
function DESEncodeString(const Source, Key: String): AnsiString;
function DESDecodeString(const Source: AnsiString; const Key: String): String;
function MakeCRC32(CRC: Cardinal; Data: Pointer; DataSize: Cardinal): Cardinal;

implementation
  uses uTPLb_Codec, uTPLb_CryptographicLibrary, uTPLb_StreamUtils, uTPLb_Constants;

procedure EncodeStream(InStream, OutStream: TStream; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.Password :=  Key;
    ACode.EncryptStream(InStream, OutStream);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

procedure DecodeStream(InStream, OutStream: TStream; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.Password :=  Key;
    ACode.DecryptStream(OutStream, InStream);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

function EncodeString(const Source, Key: String): AnsiString;
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.Password :=  Key;
    ACode.EncryptString(Source, Result);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

function DecodeString(const Source: AnsiString; const Key: String): String;
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.Password :=  Key;
    ACode.DecryptString(Result, Source);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

function DESEncodeString(const Source, Key: String): AnsiString;
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  TripleDES_ProgId;
    ACode.ChainModeId  :=  ECB_ProgId;
    ACode.Password :=  Key;
    ACode.EncryptString(Source, Result);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

function DESDecodeString(const Source: AnsiString; const Key: String): String;
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  TripleDES_ProgId;
    ACode.ChainModeId  :=  ECB_ProgId;
    ACode.Password :=  Key;
    ACode.DecryptString(Result, Source);
  finally
    FreeAndNil(ACode);
    FreeAndNil(ALibrary);
  end;
end;

function EncodeSource(const Source: String): AnsiString;
begin

  Result  :=  EncodeString(Source, '68B3BEC3-3E37-4F02-9132-C378EB844C97-0228AE12-23CC-410E-9D5C-C8A92322C1D3'+
                                   'F4AF841D-13E6-4FE9-8BD4-834602A24ECA-4C4DBF5E-FAF6-4A82-9295-DA450A4A44A3'+
                                   'BB6EBA67-45F4-49EA-A67B-85A986871B6C-2DA4F723-0342-44A4-ABAF-5378403003A1');

end;

function DecodeSource(const Source: AnsiString): String;
begin

  Result  :=  DecodeString(Source, '68B3BEC3-3E37-4F02-9132-C378EB844C97-0228AE12-23CC-410E-9D5C-C8A92322C1D3'+
                                   'F4AF841D-13E6-4FE9-8BD4-834602A24ECA-4C4DBF5E-FAF6-4A82-9295-DA450A4A44A3'+
                                   'BB6EBA67-45F4-49EA-A67B-85A986871B6C-2DA4F723-0342-44A4-ABAF-5378403003A1');

end;

function EncodeData(const Source; const Size: Integer; const Key: String): AnsiString;
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
  AStream: TStream;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  AStream   :=  TMemoryStream.Create;
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_EncryptMemory(AStream);
    ACode.EncryptMemory(Source, Size);
    ACode.End_EncryptMemory;
    Result  :=  Stream_to_Base64(AStream);
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
    FreeAndNil(AStream);
  end;
end;

procedure DecodeData(const Source: AnsiString; var Dest; const Size: Integer; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
  AStream, ACiphertext: TMemoryStream;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  AStream   :=  TMemoryStream.Create;
  ACiphertext :=  TMemoryStream.Create;
  try
    Base64_to_stream(Source, ACiphertext);
    ACiphertext.Seek(0, soBeginning);
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_DecryptMemory(AStream);
    ACode.DecryptMemory(ACiphertext.Memory^, ACiphertext.Size);
    ACode.End_DecryptMemory;
    AStream.Seek(0, soBeginning);
    AStream.ReadBuffer(Dest, Size);
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
    FreeAndNil(AStream);
    FreeAndNil(ACiphertext);
  end;
end;

procedure EncodeData(const Source; const BufSize: Integer; const Key: String; var Dest: PAnsiChar; var Size: Integer);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
  AStream: TStream;
begin
  ACode := TCodeC.Create(nil);
  ALibrary := TCryptographicLibrary.Create(nil);
  AStream := TMemoryStream.Create;
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_EncryptMemory(AStream);
    ACode.EncryptMemory(Source, BufSize);
    ACode.End_EncryptMemory;
    Size := AStream.Size;
    GetMem(Dest, Size);
    AStream.Position := 0;
    AStream.ReadBuffer(Dest^, Size);
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
    FreeAndNil(AStream);
  end;
end;

procedure DecodeData(const Source; const BufSize: Integer; var Dest: PAnsiChar; var Size: Integer; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
  AStream, ACiphertext: TMemoryStream;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  AStream   :=  TMemoryStream.Create;
  ACiphertext :=  TMemoryStream.Create;
  try
    ACiphertext.WriteBuffer(Source, BufSize);
    ACiphertext.Seek(0, soBeginning);
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_DecryptMemory(AStream);
    ACode.DecryptMemory(ACiphertext.Memory^, ACiphertext.Size);
    ACode.End_DecryptMemory;
    AStream.Seek(0, soBeginning);
    Size := AStream.Size;
    GetMem(Dest, Size);
    AStream.Position := 0;
    AStream.ReadBuffer(Dest^, Size);
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
    FreeAndNil(AStream);
    FreeAndNil(ACiphertext);
  end;
end;

procedure EncodeMemory(Source: PAnsiChar; Dest: TMemoryStream; const Size: Integer; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  try
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_EncryptMemory(Dest);
    ACode.EncryptMemory(Source^, Size);
    ACode.End_EncryptMemory;
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
  end;
end;

procedure DecodeMemory(Source: TMemoryStream; Dest: PAnsiChar; const Size: Integer; const Key: String);
var
  ACode: TCodeC;
  ALibrary: TCryptographicLibrary;
  AStream: TMemoryStream;
begin
  ACode     :=  TCodeC.Create(nil);
  ALibrary  :=  TCryptographicLibrary.Create(nil);
  AStream   :=  TMemoryStream.Create;
  try
    Source.Seek(0, soBeginning);
    ACode.CryptoLibrary  :=  ALibrary;
    ACode.StreamCipherId := BlockCipher_ProgId;
    ACode.BlockCipherId  :=  Blowfish_ProgId;
    ACode.ChainModeId  :=  CBC_ProgId;
    ACode.AsymetricKeySizeInBits := 1024;
    ACode.Password :=  Key;
    ACode.Begin_DecryptMemory(AStream);
    ACode.DecryptMemory(Source.Memory^, Source.Size);
    ACode.End_DecryptMemory;
    AStream.Seek(0, soBeginning);
    AStream.ReadBuffer(Dest^, Size);
  finally
    FreeAndNil(ALibrary);
    FreeAndNil(ACode);
    FreeAndNil(AStream);
  end;
end;

function EncodeSourceData(const Source; const Size: Integer): AnsiString;
begin
  Result  :=  EncodeData(Source, Size, 'E3894C0D-F0DA-4DA6-828B-4E17FAB36B87-5A96ADC0-E444-4D5F-A3B7-8D424727BC56');

end;

procedure DecodeSourceData(const Source: AnsiString; var Dest; const Size: Integer);
begin

  DecodeData(Source, Dest, Size, 'E3894C0D-F0DA-4DA6-828B-4E17FAB36B87-5A96ADC0-E444-4D5F-A3B7-8D424727BC56');

end;

function MakeCRC32(CRC: Cardinal; Data: Pointer; DataSize: Cardinal): Cardinal;
const
  crc_table: array [0 .. 255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $76DC419, $706AF48F, $E963A535, $9E6495A3, $EDB8832, $79DCB8A4, $E0D5E91E,
    $97D2D988, $9B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856,
    $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD,
    $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5,
    $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $1DB7106, $98D220BC,
    $EFD5102A, $71B18589, $6B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2, $F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $86D3D2D, $91646C97, $E6635C01, $6B6B51F4,
    $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3,
    $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73,
    $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822,
    $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $3B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $4DB2615, $73DC1683, $E3630B12,
    $94643B84, $D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671,
    $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1,
    $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0,
    $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0,
    $EC63F226, $756AA39C, $26D930A, $9C0906A9, $EB0E363F, $72076785, $5005713, $95BF4A82, $E2B87A14, $7BB12BAE, $CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7,
    $BDBDF21, $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF,
    $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66,
    $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF, $B3667A2E,
    $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

var
  i: Integer;
begin
  Result := CRC;
  for i := 0 to DataSize - 1 do
    Result := ((Result shr 8) and $00FFFFFF) xor crc_table[(Result xor pByteArray(Data)[i]) and $000000FF];
end;

end.
