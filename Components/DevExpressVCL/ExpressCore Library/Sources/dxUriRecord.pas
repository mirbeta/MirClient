{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCoreLibrary                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORELIBRARY AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxUriRecord;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxGenerics;

type
  { TdxURI }

  TdxURI = record
  strict private type
  {$REGION 'private type'}
    TSchemeType = (HTTP, HTTPS, WS, WSS, FTP, &FILE, GOPHER,
      MAILTO, NEWS, NNTP, UUID, TELNET, NET, PIPE, LDAP, UNKNOWN);
    TKind = (Unknown, Absolute, Relative);
  {$ENDREGION}
  strict private const
  {$REGION 'private const'}
    DefaultPorts: array[TSchemeType] of string = ('80', '443', '80', '443', '21', '', '70',
      '25', '', '119', '', '23', '808', '', '389', '');
    KnownSchemes: array[TSchemeType] of string = ('http', 'https', 'ws', 'wss', 'ftp', 'file', 'gopher',
      'mailto', 'news', 'nntp', 'uuid', 'telnet', 'net', 'pipe', 'ldap', '');
    InvalidChars = '<>:"\/|?';
  {$ENDREGION}
  strict private
    FIsLocalFile: Boolean;
    FBookmark: string;
    FHost: string;
    FKind: TKind;
    FLocalPath: string;
    FParams: string;
    FPassword: string;
    FPath: string;
    FPort: string;
    FProtocol: string;
    FSchemeType: TSchemeType;
    FUserName: string;

    class function GetSchemeType(var P: PChar; ALength: Integer; out AType: TSchemeType): Boolean; static;
    class function CheckKnownSchemes(P: PChar; ALength: Integer; out AType: TSchemeType): Boolean; static;
    class function IsAsciiLetter(ACharacter: Char): Boolean; static;
    class function IsAbsolute(const AUri: string): Boolean; static;
    class function IsLWS(ACh: Char): Boolean; static;
    class function IsBookmarkDelimiter(ACh: Char): Boolean; static;
    class function IsPathDelimiter(ACh: Char): Boolean; static;
    class function IsQueryDelimiter(ACh: Char): Boolean; static;

    procedure CalculateHostAndPort(var P: PChar);
    procedure CalculateUserNameAndPassword(var P: PChar);
    function CalculatePath(var P: PChar): string;
    function CalculateRelativeKind(const S: string): TKind;
    procedure CalculateRelativeUri(var P: PChar);
    procedure CalculateTarget(var P: PChar);
    procedure CalculateBookmark(var P: PChar);
    procedure CalculateParams(var P: PChar);
    procedure CalculateLocalPath;

    procedure Reset;
    procedure ResetTarget;

    function GetPathFromParts(AParts: TdxStringList): string;
    function GetSplitPathParts(var P: PChar): TdxStringList;
    procedure NormalizePathParts(AParts: TdxStringList);

    function HasBookmark: Boolean;
    function HasParams: Boolean;
    function IsBasedUri: Boolean;
    procedure SkipPathDelimiters(var P: PChar);

    function GetAbsoluteUri: string;
    function GetIsAbsoluteUri: Boolean;
    function GetIsFileScheme: Boolean;
    function GetIsRelativeUri: Boolean;
    function GetIsSecurityUri: Boolean;
    function GetIsWebScheme: Boolean;
    function GetScheme: string;
    function GetURI: string;

    procedure SetUri(const AURI: string);
  public
    constructor Create(const AUri: string); overload;
    constructor Create(const AAbsoluteUri, ARelativeUri: string); overload;
    constructor Create(const ABaseUri: TdxURI; ARelativeUri: string); overload;

    class function TryCreateAbsoluteUri(const AUriString: string; out AUri: TdxUri): Boolean; static;
    class function TryCreateRelativeUri(const AUriString: string; out AUri: TdxUri): Boolean; static;

    class function EscapeDataString(const AUriString: string): string; static;
    class function UnescapeDataString(const AUriString: string): string; static;

    property AbsoluteUri: string read GetAbsoluteUri;
    property Bookmark : string read FBookmark;
    property Host: string read FHost;
    property IsAbsoluteUri: Boolean read GetIsAbsoluteUri;
    property IsFileScheme: Boolean read GetIsFileScheme;
    property IsRelativeUri: Boolean read GetIsRelativeUri;
    property IsSecurityUri: Boolean read GetIsSecurityUri;
    property IsWebScheme: Boolean read GetIsWebScheme;
    property LocalPath: string read FLocalPath;
    property Params: string read FParams;
    property Password: string read FPassword;
    property Path: string read FPath;
    property Port: string read FPort;
    property Protocol: string read FProtocol;
    property Scheme: string read GetScheme;
    property URI: string read GetURI;
    property Username: string read FUserName;
  end;

implementation

uses
  StrUtils, Character, Math,
  dxCore,
  dxStringHelper;

type

  TdxUnescapeMode = class sealed
  public const
  {$REGION 'public const'}
    CopyOnly = 0;
    Escape = 1;
    Unescape = 2;
    EscapeUnescape = 3;
    V1ToStringFlag = 4;
    UnescapeAll = 8;
    UnescapeAllOrThrow = 24;
  {$ENDREGION}
  end;

  TdxUriFormatException = class(Exception);

  { TdxUriHelper }

  TdxUriHelper = class
  public const
  {$REGION 'public const'}
    HexUpperChars: array[0..$0F] of Char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
    DummyChar = #$FFFF;
    MaxAsciiCharsReallocate   = 40;
    MaxUnicodeCharsReallocate = 40;
    MaxUTF_8BytesPerUnicodeChar  = 4;
    EncodedCharsPerByte = 3;
    RFC2396ReservedMarks = ';/?:@&=+$,';
    RFC3986ReservedMarks = ':/?#[]@!$&''()*+,;=';
    RFC2396UnreservedMarks = '-_.!~*''()';
    RFC3986UnreservedMarks = '-._~';
  {$ENDREGION}
  strict private
    class function CheckIriUnicodeRange(AUnicode: Char; AIsQuery: Boolean): Boolean; overload; static;
    class function CheckIriUnicodeRange(AHighSurr, ALowSurr: Char; var ASurrogatePair: Boolean; AIsQuery: Boolean): Boolean; overload; static;
    class procedure EscapeAsciiChar(ACh: Char; var ATo: TCharArray; var APos: Integer); static;
    class function EscapedAscii(ADigit: Char; ANext: Char): Char; static;
    class function EscapeString(const AInput: string; AStart: Integer;
      AEnd: Integer; const ADest: TCharArray; var ADestPos: Integer;
      AIsUriString: Boolean; AForce1, AForce2, ARsvd: Char): TCharArray; static;
    class function EnsureDestinationSize(APStr: PChar; const ADest: TCharArray;
      ACurrentInputPos: Integer; ACharsToAdd, AMinReallocateChars: Integer; var ADestPos: Integer;
      APrevInputPos: Integer): TCharArray; static;
    class function IsAsciiLetter(C: Char): Boolean; static;
    class function IsAsciiLetterOrDigit(C: Char): Boolean; static;
    class function IsBidiControlCharacter(ACh: Char): Boolean; static;
    class function IsNotSafeForUnescape(ACh: Char): Boolean; static;
    class function IsReservedUnreservedOrHash(C: Char): Boolean; static;
    class function IsUnreserved(C: Char): Boolean; static;
    class procedure MatchUTF8Sequence(APDest: PChar; var ADest: TCharArray; var ADestOffset: Integer;
      const AUnescapedChars: TCharArray; ACharCount: Integer; const ABytes: TBytes;
      AByteCount: Integer; AIsQuery: Boolean); static;
    class function UnescapeString(APStr: PChar; AStart, AEnd: Integer;
      var ADest: TCharArray; var ADestPosition: Integer; ARsvd1, ARsvd2, ARsvd3: Char;
      AUnescapeMode: Cardinal; AIsQuery: Boolean): TCharArray; static;
  public
    class function CheckForUnicode(const AData: string): Boolean; static;
    class function EscapeDataString(const AStringToEscape: string): string; static;
    class function UnescapeDataString(const AStringToUnescape: string): string; static;
  end;

{ TdxUriHelper }

class function TdxUriHelper.CheckIriUnicodeRange(AUnicode: Char; AIsQuery: Boolean): Boolean;
begin
  Result :=
     ((AUnicode >= #$00A0) and (AUnicode <= #$D7FF)) or
     ((AUnicode >= #$F900) and (AUnicode <= #$FDCF)) or
     ((AUnicode >= #$FDF0) and (AUnicode <= #$FFEF)) or
     (AIsQuery and (AUnicode >= #$E000) and (AUnicode <= #$F8FF));
end;

class function TdxUriHelper.CheckIriUnicodeRange(AHighSurr, ALowSurr: Char; var ASurrogatePair: Boolean; AIsQuery: Boolean): Boolean;

  function InRange(AValue, AMin, AMax: Cardinal): Boolean;
  begin
    Result := (AValue >= AMin) and (AValue <= AMax);
  end;

var
  AChars: Cardinal;
begin
  Result := False;
  ASurrogatePair := False;
  Assert({$IFDEF DELPHIXE4}AHighSurr.IsHighSurrogate{$ELSE}TCharacter.IsHighSurrogate(AHighSurr){$ENDIF});
  if {$IFDEF DELPHIXE4}Char.IsSurrogatePair(AHighSurr, ALowSurr){$ELSE}TCharacter.IsSurrogatePair(AHighSurr, ALowSurr){$ENDIF} then
  begin
    ASurrogatePair := True;
    LongRec(AChars).Lo := Ord(ALowSurr);
    LongRec(AChars).Hi := Ord(AHighSurr);
    if  InRange(AChars, $D800DC00, $D83FDFFD) or
        InRange(AChars, $D840DC00, $D87FDFFD) or
        InRange(AChars, $D880DC00, $D8BFDFFD) or
        InRange(AChars, $D8C0DC00, $D8FFDFFD) or
        InRange(AChars, $D900DC00, $D93FDFFD) or
        InRange(AChars, $D940DC00, $D97FDFFD) or
        InRange(AChars, $D980DC00, $D9BFDFFD) or
        InRange(AChars, $D9C0DC00, $D9FFDFFD) or
        InRange(AChars, $DA00DC00, $DA3FDFFD) or
        InRange(AChars, $DA40DC00, $DA7FDFFD) or
        InRange(AChars, $DA80DC00, $DABFDFFD) or
        InRange(AChars, $DAC0DC00, $DAFFDFFD) or
        InRange(AChars, $DB00DC00, $DB3FDFFD) or
        InRange(AChars, $DB44DC00, $DB7FDFFD) or
        (AIsQuery and (InRange(AChars, $DB80DC00, $DBBFDFFD) or
        InRange(AChars, $DBC0DC00, $DBFFDFFD))) then
      Result := True;
  end;
end;

class procedure TdxUriHelper.EscapeAsciiChar(ACh: Char; var ATo: TCharArray; var APos: Integer);
begin
  ATo[APos] := '%';
  Inc(APos);
  ATo[APos] := HexUpperChars[(Ord(ACh) and $F0) shr 4];
  Inc(APos);
  ATo[APos] := HexUpperChars[Ord(ACh) and $0F];
  Inc(APos);
end;

class function TdxUriHelper.EscapedAscii(ADigit: Char; ANext: Char): Char;
var
  H, L: Integer;
begin
  if not CharInSet(ADigit, ['0'..'9', 'A'..'F', 'a'..'f']) then
    Exit(#$FFFF);

  if (ADigit <= '9') then
    H := Ord(ADigit) - Ord('0')
  else if ADigit <= 'F' then
    H := Ord(ADigit) - Ord('A') + 10
  else
    H := Ord(ADigit) - Ord('a') + 10;

  if not CharInSet(ANext, ['0'..'9', 'A'..'F', 'a'..'f']) then
    Exit(#$FFFF);

  if (ANext <= '9') then
    L := Ord(ANext) - Ord('0')
  else if ANext <= 'F' then
    L := Ord(ANext) - Ord('A') + 10
  else
    L := Ord(ANext) - Ord('a') + 10;

  Result := Char((H shl 4) + L);
end;

class function TdxUriHelper.EscapeString(const AInput: string; AStart: Integer;
  AEnd: Integer; const ADest: TCharArray; var ADestPos: Integer;
  AIsUriString: Boolean; AForce1, AForce2, ARsvd: Char): TCharArray;
var
  I, APrevInputPos: Integer;
  ABytes: TBytes;
  APStr: PChar;
  ACh: Char;
  AMaxSize, ACount, ANumberOfBytes: Integer;
begin
  Result := ADest;
  I := AStart;
  APrevInputPos := AStart;
  SetLength(ABytes, MaxUnicodeCharsReallocate * MaxUTF_8BytesPerUnicodeChar);
  APStr := PChar(AInput);
  while I < AEnd do
  begin
    ACh := (APStr + I)^;

    if ACh > #$7F then
    begin
      AMaxSize := Min(AEnd - I, MaxUnicodeCharsReallocate - 1);

      ACount := 1;
      while (ACount < AMaxSize) and ((APStr + I + ACount)^ > #$7F) do
        Inc(ACount);

      if ((APStr + I + ACount - 1)^ >= #$D800) and ((APStr + I + ACount - 1)^ <= #$DBFF) then
      begin
        if (ACount = 1) or (ACount = AEnd - I) then
          raise TdxUriFormatException.Create('net_uri_BadString');
        Inc(ACount);
      end;

      Result := EnsureDestinationSize(APStr, Result, I,
        ACount * MaxUTF_8BytesPerUnicodeChar * EncodedCharsPerByte,
        MaxUnicodeCharsReallocate * MaxUTF_8BytesPerUnicodeChar * EncodedCharsPerByte,
        ADestPos, APrevInputPos);

      ANumberOfBytes := TEncoding.UTF8.GetBytes(APStr, I, ACount, ABytes,
        MaxUnicodeCharsReallocate * MaxUTF_8BytesPerUnicodeChar);

      if ANumberOfBytes = 0 then
        raise TdxUriFormatException.Create('net_uri_BadString');

      Inc(I, (ACount - 1));

      while ACount < ANumberOfBytes do
      begin
        EscapeAsciiChar(Char(ABytes[ACount]), Result, ADestPos);
        Inc(ACount);
      end;

      APrevInputPos := I + 1;
    end
    else
      if (ACh = '%') and (ARsvd = '%') then
      begin
        Result := EnsureDestinationSize(APStr, Result, I, EncodedCharsPerByte, MaxAsciiCharsReallocate * EncodedCharsPerByte, ADestPos, APrevInputPos);
        if (I + 2 < AEnd) and (EscapedAscii(APStr[I + 1], APStr[I + 2]) <> DummyChar) then
        begin
          Result[ADestPos] := '%';
          Result[ADestPos + 1] := APStr[I + 1];
          Result[ADestPos + 2] := APStr[I + 2];
          Inc(ADestPos, 2);
          Inc(I, 2);
        end
        else
        begin
          EscapeAsciiChar('%', Result, ADestPos);
        end;
        APrevInputPos := I + 1;
      end
      else
        if (ACh = AForce1) or (ACh = AForce2) then
        begin
          Result := EnsureDestinationSize(APStr, Result, I, EncodedCharsPerByte, MaxAsciiCharsReallocate * EncodedCharsPerByte, ADestPos, APrevInputPos);
          EscapeAsciiChar(ACh, Result, ADestPos);
          APrevInputPos := I + 1;
        end
        else
          if (ACh <> ARsvd) and
            ((AIsUriString and not IsReservedUnreservedOrHash(ACh)) or
            (not AIsUriString and not IsUnreserved(ACh))) then
          begin
            Result := EnsureDestinationSize(APStr, Result, I, EncodedCharsPerByte, MaxAsciiCharsReallocate * EncodedCharsPerByte, ADestPos, APrevInputPos);
            EscapeAsciiChar(ACh, Result, ADestPos);
            APrevInputPos := I + 1;
          end;
    Inc(I);
  end;

  if APrevInputPos <> I then
  begin
    if (APrevInputPos <> AStart) or (Result <> nil) then
      Result := EnsureDestinationSize(APStr, Result, I, 0, 0, ADestPos, APrevInputPos);
  end;
end;

class function TdxUriHelper.EnsureDestinationSize(APStr: PChar; const ADest: TCharArray;
  ACurrentInputPos: Integer; ACharsToAdd, AMinReallocateChars: Integer; var ADestPos: Integer;
  APrevInputPos: Integer): TCharArray;
begin
  if (ADest = nil) or (Length(ADest) < ADestPos + (ACurrentInputPos - APrevInputPos) + ACharsToAdd) then
  begin
    SetLength(Result, ADestPos + (ACurrentInputPos - APrevInputPos) + AMinReallocateChars);
    if (ADest <> nil) and (ADestPos <> 0) then
      Move(ADest[0], Result[0], ADestPos * 2);
  end
  else
    Result := ADest;

  while APrevInputPos <> ACurrentInputPos do
  begin
    Result[ADestPos] := APStr[APrevInputPos];
    Inc(ADestPos);
    Inc(APrevInputPos);
  end;
end;

class function TdxUriHelper.CheckForUnicode(const AData: string): Boolean;
var
  AChars: TCharArray;
  ACount, I: Integer;
begin
  SetLength(AChars, Length(AData));
  ACount := 0;
  AChars := TdxUriHelper.UnescapeString(PChar(AData), 0, Length(AData), AChars, ACount,
    #$FFFF, #$FFFF, #$FFFF, TdxUnescapeMode.Unescape or TdxUnescapeMode.UnescapeAll, False);
  I := 0;
  while I < ACount do
  begin
    if AChars[I] > #$7F then
      Exit(True);
    Inc(I);
  end;
  Result := False;
end;

class function TdxUriHelper.IsAsciiLetter(C: Char): Boolean;
begin
  Result := CharInSet(C, ['a'..'z']) or CharInSet(C, ['A'..'Z']);
end;

class function TdxUriHelper.IsAsciiLetterOrDigit(C: Char): Boolean;
begin
  Result := IsAsciiLetter(C) or
    CharInSet(C, ['0'..'9']);
end;

class function TdxUriHelper.IsBidiControlCharacter(ACh: Char): Boolean;
begin
  Result :=
    (ACh = #$200E) or // LRM
    (ACh = #$200F) or // RLM
    (ACh = #$202A) or // LRE
    (ACh = #$202B) or // RLE
    (ACh = #$202C) or // PDF
    (ACh = #$202D) or // LRO
    (ACh = #$202E);   // RLO
end;

class function TdxUriHelper.IsNotSafeForUnescape(ACh: Char): Boolean;
begin
  if (ACh <= #31) or ((ACh >= #$007F) and (ACh <= #$009F)) then
    Result := True
  else
    Result :=
      (((ACh >= ';') and (ACh <= '@')) and (ACh <> '>') and (ACh <> '<')) or
      ((ACh >= '#') and (ACh <= '&')) or
      CharInSet(ACh, ['+',',','/','\']);
end;

class function TdxUriHelper.IsReservedUnreservedOrHash(C: Char): Boolean;
begin
  if IsUnreserved(C) then
    Exit(True);
  Result := Pos(C, RFC3986ReservedMarks) > 0;
end;

class function TdxUriHelper.IsUnreserved(C: Char): Boolean;
begin
  if IsAsciiLetterOrDigit(C) then
    Exit(True);
  Result := Pos(C, RFC3986UnreservedMarks) > 0;
end;

class procedure TdxUriHelper.MatchUTF8Sequence(APDest: PChar; var ADest: TCharArray; var ADestOffset: Integer;
  const AUnescapedChars: TCharArray; ACharCount: Integer; const ABytes: TBytes; AByteCount: Integer;
  AIsQuery: Boolean);
var
  ACount, J, AEncodedBytesLength, K, L: Integer;
  AIsHighSurr, AInIriRange, ASurrPair, AAllBytesMatch: Boolean;
  AEncodedBytes: TBytes;
begin
  ACount := 0;
  J := 0;
  while J < ACharCount do
  begin
    AIsHighSurr := {$IFDEF DELPHIXE4}AUnescapedChars[J].IsHighSurrogate{$ELSE}TCharacter.IsHighSurrogate(AUnescapedChars[J]){$ENDIF};
  {$IFDEF DELPHIXE}
    AEncodedBytes := TEncoding.UTF8.GetBytes(AUnescapedChars, J, 1 + Ord(AIsHighSurr));
    AEncodedBytesLength := Length(AEncodedBytes);
  {$ELSE}
    AEncodedBytesLength := TEncoding.UTF8.GetByteCount(AUnescapedChars, J, 1 + Ord(AIsHighSurr));
    SetLength(AEncodedBytes, AEncodedBytesLength);
    TEncoding.UTF8.GetBytes(AUnescapedChars, J, 1 + Ord(AIsHighSurr), AEncodedBytes, 0);
  {$ENDIF}
    if not AIsHighSurr then
      AInIriRange := CheckIriUnicodeRange(AUnescapedChars[J], AIsQuery)
    else
    begin
      ASurrPair := False;
      AInIriRange := CheckIriUnicodeRange(AUnescapedChars[J], AUnescapedChars[J + 1], ASurrPair, AIsQuery);
    end;

    while True do
    begin
      while ABytes[ACount] <> AEncodedBytes[0] do
      begin
        Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
        EscapeAsciiChar(Char(ABytes[ACount]), ADest, ADestOffset);
        Inc(ACount);
      end;
      AAllBytesMatch := True;
      K := 0;
      while K < AEncodedBytesLength do
      begin
        if ABytes[ACount + K] <> AEncodedBytes[K] then
        begin
          AAllBytesMatch := False;
          Break;
        end;
        Inc(K);
      end;

      if AAllBytesMatch then
      begin
        Inc(ACount, AEncodedBytesLength);
        if not AInIriRange then
        begin
          L := 0;
          while L < Length(AEncodedBytes) do
          begin
            Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
            EscapeAsciiChar(Char(AEncodedBytes[L]), ADest, ADestOffset);
            Inc(L);
          end;
        end
        else
          if not IsBidiControlCharacter(AUnescapedChars[J]) then
          begin
            Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
            APDest[ADestOffset] := AUnescapedChars[J];
            Inc(ADestOffset);
            if AIsHighSurr then
            begin
              Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
              APDest[ADestOffset] := AUnescapedChars[J + 1];
              Inc(ADestOffset);
            end;
          end;
        Break;
      end
      else
      begin
        L := 0;
        while L < K do
        begin
          Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
          EscapeAsciiChar(Char(ABytes[ACount]), ADest, ADestOffset);
          Inc(ACount);
          Inc(L);
        end;
      end;
    end;

    if AIsHighSurr then
      Inc(J);
    Inc(J);
  end;

  while ACount < AByteCount do
  begin
    Assert(Length(ADest) > ADestOffset, 'Buffer overrun detected');
    EscapeAsciiChar(Char(ABytes[ACount]), ADest, ADestOffset);
    Inc(ACount);
  end;
end;

class function TdxUriHelper.UnescapeString(APStr: PChar; AStart, AEnd: Integer; var ADest: TCharArray;
  var ADestPosition: Integer; ARsvd1, ARsvd2, ARsvd3: Char; AUnescapeMode: Cardinal; AIsQuery: Boolean): TCharArray;
label
  dest_fixed_loop_break;
var
  ABytes: TBytes;
  AEscapedReallocations: Byte;
  AEscapeReserved: Boolean;
  ANext, I, AByteCount, ACharCount: Integer;
  APDest: PChar;
  ACh: Char;
  ANewDest, AUnescapedChars: TCharArray;
begin
  ABytes := nil;
  AEscapedReallocations := 0;
  AEscapeReserved := False;
  ANext := AStart;
  while True do
  begin
    APDest := @ADest[0];
    if (AUnescapeMode and TdxUnescapeMode.EscapeUnescape) = TdxUnescapeMode.CopyOnly then
    begin
      while AStart < AEnd do
      begin
        APDest[ADestPosition] := APStr[AStart];
        Inc(ADestPosition);
        Inc(AStart);
      end;
      Exit(ADest);
    end;

    while True do
    begin
      ACh := #$0000;
      while ANext < AEnd do
      begin
        ACh := APStr[ANext];
        if ACh = '%' then
        begin
          if (AUnescapeMode and TdxUnescapeMode.Unescape) = 0 then
            AEscapeReserved := True
          else if ANext + 2 < AEnd then
          begin
            ACh := EscapedAscii(APStr[ANext + 1], APStr[ANext + 2]);
            if AUnescapeMode >= TdxUnescapeMode.UnescapeAll then
            begin
              if ACh = #$FFFF then
              begin
                if AUnescapeMode >= TdxUnescapeMode.UnescapeAllOrThrow then
                  raise TdxUriFormatException.Create('net_uri_BadString');

                Inc(ANext);
                Continue;
              end;
            end
            else if ACh = #$FFFF then
            begin
              if (AUnescapeMode and TdxUnescapeMode.Escape) <> 0 then
                AEscapeReserved := True
              else
              begin
                Inc(ANext);
                Continue;
              end;
            end
            else if ACh = '%' then
            begin
              Inc(ANext, 3);
              Continue;
            end
            else if (ACh = ARsvd1) or (ACh = ARsvd2) or (ACh = ARsvd3) then
            begin
              Inc(ANext, 3);
              Continue;
            end
            else if ((AUnescapeMode and TdxUnescapeMode.V1ToStringFlag) = 0) and IsNotSafeForUnescape(ACh) then
            begin
              Inc(ANext, 3);
              Continue;
            end
            else if ((ACh <= #$009F) and IsNotSafeForUnescape(ACh)) or ((ACh > #$009F) and not CheckIriUnicodeRange(ACh, AIsQuery)) then
            begin
              Inc(ANext, 3);
              Continue;
            end;
            Break;
          end
          else
            if AUnescapeMode >= TdxUnescapeMode.UnescapeAll then
            begin
              if AUnescapeMode >= TdxUnescapeMode.UnescapeAllOrThrow then
                raise TdxUriFormatException.Create('net_uri_BadString');

              Inc(ANext);
              Continue;
            end
            else
              AEscapeReserved := True;

          Break;
        end
        else if (AUnescapeMode and (TdxUnescapeMode.Unescape or TdxUnescapeMode.UnescapeAll)) <> (TdxUnescapeMode.Unescape or TdxUnescapeMode.UnescapeAll) then
        begin
          if (AUnescapeMode and TdxUnescapeMode.Escape) <> 0 then
          begin
            if (ACh = ARsvd1) or (ACh = ARsvd2) or (ACh = ARsvd3) then
            begin
              AEscapeReserved := True;
              Break;
            end
            else
              if ((AUnescapeMode and TdxUnescapeMode.V1ToStringFlag) = 0) and
                ((ACh <= #31) or (((ACh >= #$007F) and (ACh <= #$009F)))) then
              begin
                AEscapeReserved := True;
                Break;
              end;
          end;
        end;
        Inc(ANext);
      end;
      while AStart < ANext do
      begin
        APDest[ADestPosition] := APStr[AStart];
        Inc(ADestPosition);
        Inc(AStart);
      end;

      if ANext <> AEnd then
      begin
        if AEscapeReserved then
        begin
          if AEscapedReallocations = 0 then
          begin
            AEscapedReallocations := 30;
            SetLength(ANewDest, Length(ADest) + AEscapedReallocations * 3);
            begin
              I := 0;
              while I < ADestPosition do
              begin
                ANewDest[I] := APDest[I];
                Inc(I);
              end;
            end;
            Break;
          end
          else
          begin
            Dec(AEscapedReallocations);
            EscapeAsciiChar(APStr[ANext], ADest, ADestPosition);
            AEscapeReserved := False;
            Inc(ANext);
            AStart := ANext;
            Continue;
          end;
        end;
        if ACh <= #$7F then
        begin
          ADest[ADestPosition] := ACh;
          Inc(ADestPosition);
          Inc(ANext, 3);
          AStart := ANext;
          Continue;
        end;
        AByteCount := 1;
        if ABytes = nil then
          SetLength(ABytes, AEnd - ANext);

        ABytes[0] := Byte(ACh);
        Inc(ANext, 3);
        while ANext < AEnd do
        begin
          ACh := APStr[ANext];
          if (ACh <> '%') or (ANext + 2 >= AEnd) then
            Break;
          ACh := EscapedAscii(APStr[ANext + 1], APStr[ANext + 2]);
          if ACh = #$FFFF then
            Break
          else if ACh < #$0080 then
            Break
          else
          begin
            ABytes[AByteCount] := Byte(ACh);
            Inc(AByteCount);
            Inc(ANext, 3);
          end;
        end;
        SetLength(AUnescapedChars, Length(ABytes));
        ACharCount := TEncoding.UTF8.GetChars(ABytes, 0, AByteCount, AUnescapedChars, 0);
        AStart := ANext;
        MatchUTF8Sequence(APDest, ADest, ADestPosition, AUnescapedChars, ACharCount, ABytes, AByteCount, AIsQuery);
      end;

      if ANext = AEnd then
        Exit(ADest);
    end;
  end;

  Result := ADest;
end;

class function TdxUriHelper.UnescapeDataString(const AStringToUnescape: string): string;
var
  P: PChar;
  APosition: Integer;
  AUnescapeMode: Cardinal;
  ADest: TCharArray;
begin
  if AStringToUnescape = '' then
    Exit('');

  P := PChar(AStringToUnescape);
  APosition := 0;
  while APosition < Length(AStringToUnescape) do
  begin
    if P[APosition] = '%' then
      Break;
    Inc(APosition);
  end;

  if APosition = Length(AStringToUnescape) then
    Exit(AStringToUnescape);

  AUnescapeMode := TdxUnescapeMode.Unescape or TdxUnescapeMode.UnescapeAll;
  APosition := 0;
  SetLength(ADest, Length(AStringToUnescape));
  ADest := TdxUriHelper.UnescapeString(PChar(AStringToUnescape), 0, Length(AStringToUnescape), ADest,
    APosition, #$FFFF, #$FFFF, #$FFFF, AUnescapeMode, False);
  SetString(Result, PChar(@ADest[0]), APosition);
end;

class function TdxUriHelper.EscapeDataString(const AStringToEscape: string): string;
var
  APosition: Integer;
  ADest: TCharArray;
begin
  if AStringToEscape = '' then
    Exit('');

  APosition := 0;
  ADest := EscapeString(AStringToEscape, 0, Length(AStringToEscape), nil, APosition, False,
    #$FFFF, #$FFFF, #$FFFF);
  if ADest = nil then
    Exit(AStringToEscape);
  SetString(Result, PChar(@ADest[0]), APosition);
end;

{ TdxURI }

constructor TdxURI.Create(const AURI: string);
begin
  Reset;
  SetUri(AURI);
end;

constructor TdxURI.Create(const AAbsoluteUri, ARelativeUri: string);
var
  P: PChar;
  ARelativeKind: TKind;
begin
  if IsAbsolute(ARelativeUri) then
  begin
    Create(ARelativeUri);
    Exit;
  end;
  Create(AAbsoluteUri);
  if not IsBasedUri then
    Exit;
  ARelativeKind := CalculateRelativeKind(ARelativeUri);
  if ARelativeKind = TKind.Relative then
  begin
    P := PChar(ARelativeUri);
    CalculateRelativeUri(P);
    CalculateTarget(P);
  end;
end;

constructor TdxURI.Create(const ABaseUri: TdxURI; ARelativeUri: string);
begin
  Create(ABaseUri.AbsoluteUri, ARelativeUri);
end;

procedure TdxURI.SkipPathDelimiters(var P: PChar);
begin
  while IsPathDelimiter(P^) do
    Inc(P);
end;

function TdxURI.GetPathFromParts(AParts: TdxStringList): string;
begin
  NormalizePathParts(AParts);
  if AParts.Count <= 1 then
    Result := '/'
  else
    Result := '';
  Result := Result + TdxStringHelper.Join('/', AParts.ToArray);
end;

function TdxURI.GetSplitPathParts(var P: PChar): TdxStringList;
var
  S: string;
  AHasSeparator: Boolean;
begin
  Result := TdxStringList.Create;
  S := '';
  AHasSeparator := False;
  while P^ <> #0 do
  begin
    if IsPathDelimiter(P^) then
    begin
      if S <> '.' then
        Result.Add(S);
      S := '';
      Inc(P);
      AHasSeparator := True;
      Continue;
    end;
    AHasSeparator := False;
    if IsQueryDelimiter(P^) or IsBookmarkDelimiter(P^) then
    begin
      if S <> '' then
        Result.Add(S);
      Exit;
    end;

    S := S + P^;
    Inc(P);
  end;

  if AHasSeparator or (S <> '') then
    Result.Add(S);
end;

procedure TdxURI.NormalizePathParts(AParts: TdxStringList);
var
  I: Integer;
begin
  I := 0;
  while I < AParts.Count do
  begin
    if AParts[I] = '..' then
    begin
      AParts.Delete(I);
      if I > 1 then
      begin
        AParts.Delete(I - 1);
        Dec(I);
      end
      else
        if I = 1 then
          AParts[0] := '';
    end
    else
      Inc(I);
  end;
end;

class function TdxURI.GetSchemeType(var P: PChar; ALength: Integer; out AType: TSchemeType): Boolean;
var
  AIndex, AEnd: Integer;
  C: Char;
begin
  Result := False;
  AIndex := 0;
  while (AIndex < ALength) and IsLWS(P[AIndex]) do
    Inc(AIndex);

  AEnd := AIndex;
  while (AEnd < ALength) and (P[AEnd] <> ':') do
    Inc(AEnd);

  if ((AEnd <> ALength) and (AEnd >= AIndex + 2)) and CheckKnownSchemes(P + AIndex, AEnd - AIndex, AType) then
  begin
    Inc(P, AEnd + 1);
    Exit(True);
  end;

  if (AIndex + 2 >= ALength) or (AEnd = AIndex) then
    Exit;

  Inc(P, AIndex);
  C := P[1];
  if (C = ':') or (C = '|') then
  begin
    if IsAsciiLetter(P[0]) then
    begin
      C := P[2];
      if (C = '\') or (C = '/') then
      begin
        AType := TSchemeType.&FILE;
        Result := True;
      end;
    end;
  end
  else
  begin
    C := P[0];
    if (C = '/') or (C = '\') then
    begin
      C := P[1];
      if (C = '\') or (C = '/') then
      begin
        AType := TSchemeType.&FILE;
        Result := True;
      end;
    end;
  end;
end;

class function TdxURI.CheckKnownSchemes(P: PChar; ALength: Integer; out AType: TSchemeType): Boolean;
var
  S: string;
  I: TSchemeType;
begin
  S := LowerCase(Copy(P, 1, ALength));
  for I := Low(TSchemeType) to High(TSchemeType) do
  begin
    if S = KnownSchemes[I] then
    begin
      AType := I;
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TdxURI.IsAsciiLetter(ACharacter: Char): Boolean;
begin
  Result := (((ACharacter >= 'a') and (ACharacter <= 'z'))) or
    (((ACharacter >= 'A') and (ACharacter <= 'Z')));
end;

class function TdxURI.IsAbsolute(const AUri: string): Boolean;
var
  AType: TSchemeType;
  P: PChar;
begin
  P := PChar(AUri);
  Result := GetSchemeType(P, Length(AUri), AType) and (AType <> TSchemeType.UNKNOWN);
end;

class function TdxURI.IsLWS(ACh: Char): Boolean;
begin
  Result := CharInSet(ACh, [' ', #10, #13, #9]);
end;

class function TdxURI.IsBookmarkDelimiter(ACh: Char): Boolean;
begin
  Result := ACh = '#';
end;

class function TdxURI.IsPathDelimiter(ACh: Char): Boolean;
begin
  Result := CharInSet(ACh, ['/', '\']);
end;

class function TdxURI.IsQueryDelimiter(ACh: Char): Boolean;
begin
  Result := ACh = '?';
end;

procedure TdxURI.CalculateHostAndPort(var P: PChar);
var
  APos: Integer;
begin
  if IsRelativeUri then
    Exit;
  FIsLocalFile := False;
  if FSchemeType = TSchemeType.&FILE then
    FIsLocalFile := IsAsciiLetter(P^) and CharInSet(P[1], [':', '|']) and IsPathDelimiter(P[2]);

  if FIsLocalFile then
  begin
    FHost := '';
  end
  else
  begin
    APos := TdxStringHelper.IndexOfAny(P, ['\', '/']);
    if APos = -1 then
      APos := Length(P);
    FHost := Copy(P, 1, APos);
    Inc(P, APos);
    APos := TdxStringHelper.IndexOf(FHost, ':');
    if APos > 0 then
    begin
      FPort := Copy(FHost, APos + 2, Length(FHost));
      Delete(FHost, APos + 1, Length(FHost));
    end;
  end;
end;

procedure TdxURI.CalculateLocalPath;
begin
  if FSchemeType <> TSchemeType.&FILE then
    FLocalPath := Path
  else
  begin
    FLocalPath := Path;
    if not FIsLocalFile then
      FLocalPath := Format('\\%s', [Host]) + FLocalPath;
    FLocalPath := StringReplace(FLocalPath, '/', '\', [rfReplaceAll]);
  end;
  FLocalPath := UnescapeDataString(FLocalPath);
end;

procedure TdxURI.Reset;
begin
  FHost := '';
  FLocalPath := '';
  FPassword := '';
  FPath := '';
  FPort := '';
  FProtocol := '';
  FUserName := '';
  ResetTarget;
end;

procedure TdxURI.ResetTarget;
begin
  FBookmark := '';
  FParams := '';
end;

procedure TdxURI.CalculateUserNameAndPassword(var P: PChar);
begin
end;

function TdxURI.CalculatePath(var P: PChar): string;
var
  AParts: TdxStringList;
begin
  if P^ = #0 then
  begin
    Result := '/';
    Exit;
  end;

  AParts := GetSplitPathParts(P);
  try
    Result := GetPathFromParts(AParts);
  finally
    AParts.Free;
  end;
end;

function TdxURI.CalculateRelativeKind(const S: string): TKind;
var
  P, ASaveP: PChar;
begin
  Result := TKind.Unknown;
  P := PChar(S);
  ASaveP := P;
  if P^ <> #0 then
  begin
    repeat
      if P^ = '.' then
      begin
        Inc(P);
        if P^ = '.' then
          Inc(P);
        if IsPathDelimiter(P^) then
          Continue
        else
          Break;
      end;
      if IsPathDelimiter(P^) then
      begin
        SkipPathDelimiters(P);
        if P^ = '.' then
          Continue;
      end;
      Inc(P);
      Result := TKind.Relative;
    until (Result <> TKind.Unknown) or (P^ = #0);
    if (Result = TKind.Unknown) and (P^ = #0) and (P <> ASaveP) then
      Result := TKind.Relative;
  end;
end;

procedure TdxURI.CalculateRelativeUri(var P: PChar);
var
  AParts, AAbsolutePathParts: TdxStringList;
  AAbsolutePath: PChar;
begin
  if IsPathDelimiter(P^) then
  begin
    FPath := CalculatePath(P);
    Exit;
  end;

  AParts := GetSplitPathParts(P);
  try
    AAbsolutePath := PChar(Path);
    AAbsolutePathParts := GetSplitPathParts(AAbsolutePath);
    try
      if AAbsolutePathParts.Count > 0 then
        AAbsolutePathParts.Delete(AAbsolutePathParts.Count - 1);
      AParts.InsertRange(0, AAbsolutePathParts);
    finally
      AAbsolutePathParts.Free;
    end;
    FPath := GetPathFromParts(AParts);
  finally
    AParts.Free;
  end;
end;

procedure TdxURI.CalculateTarget(var P: PChar);
begin
  if FKind <> TKind.Unknown then
  begin
    CalculateBookmark(P);
    CalculateParams(P);
  end;
  CalculateLocalPath;
end;

procedure TdxURI.CalculateBookmark(var P: PChar);
var
  APos: Integer;
begin
  if IsBookmarkDelimiter(P^) then
  begin
    Inc(P);
    APos := TdxStringHelper.IndexOf(P, '?');
    if APos > 0 then
    begin
      FBookmark := Copy(P, 1, APos);
      Inc(P, APos);
    end;
  end;
end;

procedure TdxURI.CalculateParams(var P: PChar);
begin
  if IsQueryDelimiter(P^) then
  begin
    Inc(P);
    FParams := P;
  end;
end;

procedure TdxURI.SetUri(const AURI: string);
var
  S: PChar;
begin
  FKind := TKind.Unknown;
  S := PChar(AURI);
  if not TdxURI.GetSchemeType(S, Length(AURI), FSchemeType) then
    FSchemeType := TSchemeType.UNKNOWN;
  FProtocol := KnownSchemes[FSchemeType];

  if FSchemeType <> TSchemeType.UNKNOWN then
    FKind := TKind.Absolute
  else
    FKind := CalculateRelativeKind(S);

  SkipPathDelimiters(S);
  CalculateUserNameAndPassword(S);
  CalculateHostAndPort(S);
  FPath := CalculatePath(S);
  CalculateTarget(S);
end;

function TdxURI.HasBookmark: Boolean;
begin
  Result := Trim(Bookmark) <> ''
end;

function TdxURI.HasParams: Boolean;
begin
  Result := Trim(Params) <> ''
end;

function TdxURI.IsBasedUri: Boolean;
begin
  Result := IsAbsoluteUri and not HasBookmark and not HasParams;
end;

class function TdxURI.TryCreateAbsoluteUri(const AUriString: string;
  out AUri: TdxUri): Boolean;
var
  AResult: TdxUri;
begin
  AResult := TdxUri.Create(AUriString);
  Result := AResult.IsAbsoluteUri;
  if Result then
    AUri := AResult;
end;

class function TdxURI.TryCreateRelativeUri(const AUriString: string;
  out AUri: TdxUri): Boolean;
var
  AResult: TdxUri;
begin
  AResult := TdxUri.Create(AUriString);
  Result := AResult.IsRelativeUri;
  if Result then
    AUri := AResult;
end;

class function TdxURI.EscapeDataString(const AUriString: string): string;
begin
  Result := TdxUriHelper.EscapeDataString(AUriString);
end;

class function TdxURI.UnescapeDataString(const AUriString: string): string;
begin
  Result := TdxUriHelper.UnescapeDataString(AUriString);
end;

function TdxURI.GetAbsoluteUri: string;
begin
  if IsAbsoluteUri then
    Result := URI
  else
    Result := '';
end;

function TdxURI.GetIsAbsoluteUri: Boolean;
begin
  Result := FKind = TKind.Absolute;
end;

function TdxURI.GetIsFileScheme: Boolean;
begin
  Result := (FSchemeType = TSchemeType.&FILE) and (Length(FParams) = 0);
end;

function TdxURI.GetIsRelativeUri: Boolean;
begin
  Result := FKind = TKind.Relative;
end;

function TdxURI.GetIsSecurityUri: Boolean;
begin
  Result := FSchemeType = TSchemeType.HTTPS;
end;

function TdxURI.GetIsWebScheme: Boolean;
begin
  Result := FSchemeType in [TSchemeType.HTTP, TSchemeType.HTTPS, TSchemeType.FTP];
end;

function TdxURI.GetURI: string;
begin
  Result := '';
  if IsAbsoluteUri then
    Result := FProtocol + '://';
  if FIsLocalFile then
    Result := Result + '/';
  if FUserName <> '' then
  begin
    Result := Result + FUserName;
    if FPassword <> '' then
      Result := Result + ':' + FPassword;
    Result := Result + '@';
  end;
  Result := Result + FHost;
  if FPort <> '' then
    if FPort <> DefaultPorts[FSchemeType] then
      Result := Result + ':' + FPort;
  Result := Result + FPath;
  if HasBookmark then
    Result := Result + '#' + FBookmark;
  if HasParams then
    Result := Result + '?' + FParams;
end;

function TdxURI.GetScheme: string;
begin
  Result := Protocol;
end;

end.
