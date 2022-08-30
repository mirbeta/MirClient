{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxPDFFontEncoding;

{$I cxVer.inc}

interface

uses
  SysUtils, Types, Generics.Defaults, Generics.Collections, dxPDFBase, dxPDFTypes, dxPDFCore, dxPDFCharacterMapping,
  dxFontFile;

type
  TdxPDFSimpleFontEncoding = class;
  TdxPDFSimpleFontEncodingClass = class of TdxPDFSimpleFontEncoding;

  { TdxPDFSimpleFontEncoding }

  TdxPDFSimpleFontEncoding = class(TdxPDFCustomEncoding)
  strict private
    FDifferences: TDictionary<Integer, string>;

    procedure ReadDifferences(ADictionary: TdxPDFDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function CreateEncoding(const ABaseFont, AEncodingName: string): TdxPDFSimpleFontEncoding; static;

    function GetGlyphName(ACode: Byte): string;
    function GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData; override;
    function ShouldUseEmbeddedFontEncoding: Boolean; override;

    property Differences: TDictionary<Integer, string> read FDifferences;
  end;

  { TdxPDFMacRomanEncoding }

  TdxPDFMacRomanEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    class function GetTypeName: string; override;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  end;

  { TdxPDFSymbolEncoding }

  TdxPDFSymbolEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    class function GetTypeName: string; override;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  end;

  { TdxPDFStandartEncoding }

  TdxPDFStandartEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    class function GetTypeName: string; override;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  end;

  { TdxPDFDocEncoding }

  TdxPDFDocEncoding = class(TdxPDFStandartEncoding)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFWinAnsiEncoding }

  TdxPDFWinAnsiEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    class function GetTypeName: string; override;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  end;

  { TdxPDFZapfDingbatsEncoding }

  TdxPDFZapfDingbatsEncoding = class(TdxPDFSimpleFontEncoding)
  protected
    class function GetTypeName: string; override;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; override;
  end;

  { TdxPDFUnicodeConverter }

  TdxPDFUnicodeConverter = class
  public
    class function GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding; ACode: Word): Word; overload;
    class function GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding; AGlyphCodes: TDictionary<string, Word>;
      ACode: Word): Word; overload;
  end;

  { TdxPDFCompositeFontEncoding }

  TdxPDFCompositeFontEncoding = class(TdxPDFCustomEncoding)
  public
    class function Parse(ARepository: TdxPDFCustomRepository; ASourceObject: TdxPDFBase): TdxPDFCompositeFontEncoding;
    function IsVertical: Boolean; virtual;
    function ShouldUseEmbeddedFontEncoding: Boolean; override;
  end;

  { TdxPDFIdentityEncoding }

  TdxPDFCustomIdentityEncoding = class(TdxPDFCompositeFontEncoding)
  public
    function GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData; override;
  end;

  { TdxPDFHorizontalIdentityEncoding }

  TdxPDFHorizontalIdentityEncoding = class(TdxPDFCustomIdentityEncoding)
  protected
    class function GetTypeName: string; override;
  end;

  { TdxPDFVerticalIdentityEncoding }

  TdxPDFVerticalIdentityEncoding = class(TdxPDFCustomIdentityEncoding)
  protected
    class function GetTypeName: string; override;
  public
    function IsVertical: Boolean; override;
  end;

  { TdxPDFPredefinedCompositeFontEncoding }

  TdxPDFPredefinedCompositeFontEncoding = class(TdxPDFCompositeFontEncoding)
  strict private
    FHorizontalNames: TList<string>;
    FVerticalNames: TList<string>;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;

    property HorizontalNames: TList<string>read FHorizontalNames;
    property VerticalNames: TList<string>read FVerticalNames;
  public
    function GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData; override;
    function IsVertical: Boolean; override;
  end;

  { TdxPDFCustomCompositeFontEncoding }

  TdxPDFCustomCompositeFontEncoding = class(TdxPDFCompositeFontEncoding)
  strict private
    FBaseEncoding: TdxPDFCompositeFontEncoding;
    FCharacterMapping: TdxPDFCharacterMapping;
    FName: string;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; const AData: TBytes); reintroduce;
  public
    function GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData; override;
  end;

implementation

uses
  Variants, dxCore, dxCoreClasses, dxPDFUtils;

type
  TdxFontFileUnicodeConverterAccess = class(TdxFontFileUnicodeConverter);
  TdxPDFCustomCompositeFontEncodingAccess = class(TdxPDFCustomCompositeFontEncoding);
  TdxPDFObjectAccess = class(TdxPDFObject);

{ TdxPDFCustomCompositeFontEncoding }

function TdxPDFCustomCompositeFontEncoding.GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
begin
  Result := FCharacterMapping.GetStringData(ABytes, AGlyphOffsets);
end;

procedure TdxPDFCustomCompositeFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FBaseEncoding := nil;
  FCharacterMapping := nil;
end;

procedure TdxPDFCustomCompositeFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FCharacterMapping);
  FreeAndNil(FBaseEncoding);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomCompositeFontEncoding.Read(ADictionary: TdxPDFReaderDictionary;
  const AData: TBytes);
var
  AUseCMap: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FName := ADictionary.GetString('CMapName');
    if ADictionary.TryGetObject('UseCMap', AUseCMap) then
      FBaseEncoding := TdxPDFCompositeFontEncoding.Parse(Repository, AUseCMap);
    FCharacterMapping := TdxPDFCMapStreamParser.Parse(Repository, AData);
  end;
end;

{ TdxPDFSimpleFontEncoding }

class function TdxPDFSimpleFontEncoding.CreateEncoding(const ABaseFont, AEncodingName: string): TdxPDFSimpleFontEncoding;
var
  AClass: TdxPDFObjectClass;
begin
  if AEncodingName <> '' then
    AClass := dxPDFGetDocumentObjectClass(AEncodingName)
  else
     if ABaseFont = TdxPDFMacRomanEncoding.GetTypeName then
       AClass := TdxPDFMacRomanEncoding
     else
       if ABaseFont = TdxPDFZapfDingbatsEncoding.GetTypeName then
         AClass := TdxPDFZapfDingbatsEncoding
       else
         AClass := TdxPDFPredefinedCompositeFontEncoding;
  Result := AClass.Create(nil) as TdxPDFSimpleFontEncoding;
end;

function TdxPDFSimpleFontEncoding.GetGlyphName(ACode: Byte): string;
begin
  if not FDifferences.TryGetValue(ACode, Result) then
    if not FontFileEncoding.Dictionary.TryGetValue(ACode, Result) then
      Result := TdxGlyphNames._notdef;
end;

function TdxPDFSimpleFontEncoding.GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
var
  I: Integer;
begin
  SetLength(Result.CharacterCodes, Length(ABytes));
  SetLength(Result.Glyphs, Length(ABytes));
  for I := 0 to Length(ABytes) - 1 do
  begin
    Result.Glyphs[I] := Word(ABytes[I]);
    SetLength(Result.CharacterCodes[I], 1);
    Result.CharacterCodes[I][0] := ABytes[I];
  end;
  TdxPDFUtils.AddData(AGlyphOffsets, Result.Offsets);

  if Length(AGlyphOffsets) = 0 then
    for I := 0 to Length(ABytes) + 1 do
      TdxPDFUtils.AddValue(0, Result.Offsets);
end;

function TdxPDFSimpleFontEncoding.ShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := GetTypeName = TdxPDFStandartEncoding.GetTypeName;
end;

procedure TdxPDFSimpleFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FDifferences := TDictionary<Integer, string>.Create;
end;

procedure TdxPDFSimpleFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FDifferences);
  inherited DestroySubClasses;
end;

procedure TdxPDFSimpleFontEncoding.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadDifferences(ADictionary);
end;

procedure TdxPDFSimpleFontEncoding.ReadDifferences(ADictionary: TdxPDFDictionary);
var
  I, ACode: Integer;
  AArray: TdxPDFArray;
  ADifference: TdxPDFBase;
begin
  FDifferences.Clear;
  if ADictionary <> nil then
  begin
    AArray := ADictionary.GetArray(TdxPDFKeywords.Differences);
    ACode := 0;
    if (AArray <> nil) and (AArray.Count > 0) then
      for I := 0 to AArray.Count - 1 do
      begin
        ADifference := AArray[I] as TdxPDFBase;
        if not (ADifference.ObjectType in [otName, otString]) then
          ACode := TdxPDFInteger(ADifference).Value
        else
        begin
          FDifferences.AddOrSetValue(ACode, TdxPDFName(ADifference).Value);
          Inc(ACode);
        end;
      end;
  end;
end;

{ TdxPDFMacRomanEncoding }

class function TdxPDFMacRomanEncoding.GetTypeName: string;
begin
  Result := 'MacRomanEncoding';
end;

function TdxPDFMacRomanEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileMacRomanEncoding;
end;

{ TdxPDFSymbolEncoding }

class function TdxPDFSymbolEncoding.GetTypeName: string;
begin
  Result := 'Symbol';
end;

function TdxPDFSymbolEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileSymbolEncoding;
end;

{ TdxPDFStandartEncoding }

class function TdxPDFStandartEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.StandartEncoding;
end;

function TdxPDFStandartEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileStandardEncoding;
end;

{ TdxPDFDocEncoding }

class function TdxPDFDocEncoding.GetTypeName: string;
begin
  Result := 'PDFDocEncoding';
end;

{ TdxPDFWinAnsiEncoding }

class function TdxPDFWinAnsiEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.WinAnsiEncoding;
end;

function TdxPDFWinAnsiEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileWinAnsiEncoding;
end;

{ TdxPDFZapfDingbatsEncoding }

class function TdxPDFZapfDingbatsEncoding.GetTypeName: string;
begin
  Result := 'ZapfDingbats';
end;

function TdxPDFZapfDingbatsEncoding.GetFontFileEncodingClass: TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileZapfDingbatsEncoding;
end;

{ TdxPDFUnicodeConverter }

class function TdxPDFUnicodeConverter.GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding; ACode: Word): Word;
begin
  Result := GetGlyphCode(AEncoding, TdxFontFileUnicodeConverterAccess(dxFontFileUnicodeConverter).GlyphCodes, ACode);
end;

class function TdxPDFUnicodeConverter.GetGlyphCode(AEncoding: TdxPDFSimpleFontEncoding;
  AGlyphCodes: TDictionary<string, Word>; ACode: Word): Word;
var
  AGlyphName: string;
begin
  AGlyphName := AEncoding.GetGlyphName(ACode);
  if not AGlyphCodes.TryGetValue(AGlyphName, Result) then
    if not dxFontFileUnicodeConverter.FindCode(AGlyphName, Result) then
      if not ((AGlyphName = TdxGlyphNames.Zdotaccent) and AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, Result)) then
        if not ((AGlyphName = TdxGlyphNames.LowerZdotaccent) and AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, Result)) then
          Result := ACode;
end;

{ TdxPDFCustomIdentityEncoding }

function TdxPDFCustomIdentityEncoding.GetStringData(const ABytes: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
var
  ALength, I, AByteIndex: Integer;
  AHighByte, ALowByte: Byte;
  AIsEmptyOffsets: Boolean;
begin
  ALength := Length(ABytes) div 2;
  SetLength(Result.Glyphs, ALength);
  SetLength(Result.CharacterCodes, ALength);
  SetLength(Result.Offsets, ALength + 1);
  AIsEmptyOffsets := Length(AGlyphOffsets) = 0;
  AByteIndex := 0;
  for I := 0 to ALength - 1 do
  begin
    if not AIsEmptyOffsets then
      Result.Offsets[I] := AGlyphOffsets[AByteIndex];
    AHighByte := ABytes[AByteIndex];
    ALowByte := ABytes[AByteIndex + 1];
    Inc(AByteIndex, 2);
    Result.Glyphs[I] := AHighByte shl 8 + ALowByte;
    SetLength(Result.CharacterCodes[I], 2);
    Result.CharacterCodes[I][0] := AHighByte;
    Result.CharacterCodes[I][1] := ALowByte;
  end;
end;

{ TdxPDFCompositeFontEncoding }

class function TdxPDFCompositeFontEncoding.Parse(ARepository: TdxPDFCustomRepository;
  ASourceObject: TdxPDFBase): TdxPDFCompositeFontEncoding;
var
  AName: string;
  AStream: TdxPDFStream;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if ASourceObject <> nil then
    case ASourceObject.ObjectType of
      otDictionary, otStream:
        begin
          if ASourceObject.ObjectType = otStream then
          begin
            AStream := TdxPDFStream(ASourceObject);
            ADictionary := AStream.Dictionary as TdxPDFReaderDictionary;
          end
          else
          begin
            ADictionary := ASourceObject as TdxPDFReaderDictionary;
            AStream := ADictionary.StreamRef;
          end;
          if ADictionary <> nil then
          begin
            Result := TdxPDFCustomCompositeFontEncoding.Create(nil);
            TdxPDFCustomCompositeFontEncodingAccess(Result).Read(ADictionary, AStream.UncompressedData);
          end;
        end;
      otName, otString:
        begin
          AName := TdxPDFName(ASourceObject).Value;
          if AName = TdxPDFVerticalIdentityEncoding.GetTypeName then
            Result := TdxPDFVerticalIdentityEncoding.Create(nil)
          else
            if AName = TdxPDFHorizontalIdentityEncoding.GetTypeName then
              Result := TdxPDFHorizontalIdentityEncoding.Create(nil)
            else
              Result := TdxPDFPredefinedCompositeFontEncoding.Create(nil) as TdxPDFCompositeFontEncoding
        end;
    end
  else
    Result := TdxPDFHorizontalIdentityEncoding.Create(nil);
end;

function TdxPDFCompositeFontEncoding.IsVertical: Boolean;
begin
  Result := False;
end;

function TdxPDFCompositeFontEncoding.ShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := False;
end;

{ TdxPDFVerticalIdentityEncoding }

function TdxPDFVerticalIdentityEncoding.IsVertical: Boolean;
begin
  Result := True;
end;

class function TdxPDFVerticalIdentityEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.IdentityV;
end;

{ TdxPDFPredefinedCompositeFontEncoding }

function TdxPDFPredefinedCompositeFontEncoding.GetStringData(const ABytes: TBytes;
  var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
var
  I, ALength: Integer;
  AByteIndex: Integer;
begin
  ALength := Length(ABytes) div 2;
  SetLength(Result.CharacterCodes, ALength);
  SetLength(Result.Glyphs, ALength);
  SetLength(Result.Offsets, ALength + 1);
  for I := 0 to ALength do
    Result.Offsets[I] := 0;

  AByteIndex := 0;
  if Length(AGlyphOffsets) > 0 then
    for I := 0 to ALength - 1 do
      Result.Offsets[I] := AGlyphOffsets[AByteIndex];
end;

function TdxPDFPredefinedCompositeFontEncoding.IsVertical: Boolean;
begin
  Result := True;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FHorizontalNames := TList<string>.Create;
  FVerticalNames := TList<string>.Create;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.DestroySubClasses;
begin
  FreeAndNil(FVerticalNames);
  FreeAndNil(FHorizontalNames);
  inherited DestroySubClasses;
end;

procedure TdxPDFPredefinedCompositeFontEncoding.Initialize;
begin
  FHorizontalNames.Add('GB-EUC-H');
  FHorizontalNames.Add('GBpc-EUC-H');
  FHorizontalNames.Add('GBK-EUC-H');
  FHorizontalNames.Add('GBKp-EUC-H');
  FHorizontalNames.Add('GBK2K-H');
  FHorizontalNames.Add('UniGB-UCS2-H');
  FHorizontalNames.Add('UniGB-UTF16-H');
  FHorizontalNames.Add('B5pc-H');
  FHorizontalNames.Add('HKscs-B5-H');
  FHorizontalNames.Add('ETen-B5-H');
  FHorizontalNames.Add('ETenms-B5-H');
  FHorizontalNames.Add('CNS-EUC-H');
  FHorizontalNames.Add('UniCNS-UCS2-H');
  FHorizontalNames.Add('UniCNS-UTF16-H');
  FHorizontalNames.Add('83pv-RKSJ-H');
  FHorizontalNames.Add('90ms-RKSJ-H');
  FHorizontalNames.Add('90msp-RKSJ-H');
  FHorizontalNames.Add('90pv-RKSJ-H');
  FHorizontalNames.Add('Add-RKSJ-H');
  FHorizontalNames.Add('EUC-H');
  FHorizontalNames.Add('Ext-RKSJ-H');
  FHorizontalNames.Add('H');
  FHorizontalNames.Add('UniJIS-UCS2-H');
  FHorizontalNames.Add('UniJIS-UCS2-HW-H');
  FHorizontalNames.Add('UniJIS-UTF16-H');
  FHorizontalNames.Add('KSC-EUC-H');
  FHorizontalNames.Add('KSCms-UHC-H');
  FHorizontalNames.Add('KSCms-UHC-HW-H');
  FHorizontalNames.Add('KSCpc-EUC-H');
  FHorizontalNames.Add('UniKS-UCS2-H');
  FHorizontalNames.Add('UniKS-UTF16-H');

  FVerticalNames.Add('GB-EUC-V');
  FVerticalNames.Add('GBpc-EUC-V');
  FVerticalNames.Add('GBK-EUC-V');
  FVerticalNames.Add('GBKp-EUC-V');
  FVerticalNames.Add('GBK2K-V');
  FVerticalNames.Add('UniGB-UCS2-V');
  FVerticalNames.Add('UniGB-UTF16-V');
  FVerticalNames.Add('B5pc-V');
  FVerticalNames.Add('HKscs-B5-V');
  FVerticalNames.Add('ETen-B5-V');
  FVerticalNames.Add('ETenms-B5-V');
  FVerticalNames.Add('CNS-EUC-V');
  FVerticalNames.Add('UniCNS-UCS2-V');
  FVerticalNames.Add('UniCNS-UTF16-V');
  FVerticalNames.Add('90ms-RKSJ-V');
  FVerticalNames.Add('90msp-RKSJ-V');
  FVerticalNames.Add('Add-RKSJ-V');
  FVerticalNames.Add('EUC-V');
  FVerticalNames.Add('Ext-RKSJ-V');
  FVerticalNames.Add('V');
  FVerticalNames.Add('UniJIS-UCS2-V');
  FVerticalNames.Add('UniJIS-UCS2-HW-V');
  FVerticalNames.Add('UniJIS-UTF16-V');
  FVerticalNames.Add('KSC-EUC-V');
  FVerticalNames.Add('KSCms-UHC-V');
  FVerticalNames.Add('KSCms-UHC-HW-V');
  FVerticalNames.Add('UniKS-UCS2-V');
  FVerticalNames.Add('UniKS-UTF16-V');
end;

{ TdxPDFHorizontalIdentityEncoding }

class function TdxPDFHorizontalIdentityEncoding.GetTypeName: string;
begin
  Result := TdxPDFKeywords.IdentityH;
end;

procedure dxPDFRegisterPredefinedCompositeFontEncodings;

  procedure RegisterEncodingNames(ANames: TList<string>);
  var
    AName: string;
  begin
    for AName in ANames do
      dxPDFRegisterDocumentObjectClass(AName, TdxPDFPredefinedCompositeFontEncoding);
  end;

var
  AEncodingNames: TList<string>;
  ATempEncoding: TdxPDFPredefinedCompositeFontEncoding;
begin
  ATempEncoding := TdxPDFPredefinedCompositeFontEncoding.Create(nil);
  try
    AEncodingNames := ATempEncoding.HorizontalNames;
    RegisterEncodingNames(AEncodingNames);
    AEncodingNames := ATempEncoding.VerticalNames;
    RegisterEncodingNames(AEncodingNames);
  finally
    ATempEncoding.Free;
  end;
end;

procedure dxPDFUnregisterPredefinedCompositeFontEncodings;

  procedure UnregisterEncodingNames(ANames: TList<string>);
  var
    AName: string;
  begin
    for AName in ANames do
      dxPDFUnregisterDocumentObjectClass(AName, TdxPDFPredefinedCompositeFontEncoding);
  end;

var
  AEncodingNames: TList<string>;
  ATempEncoding: TdxPDFPredefinedCompositeFontEncoding;
begin
  ATempEncoding := TdxPDFPredefinedCompositeFontEncoding.Create(nil);
  try
    AEncodingNames := ATempEncoding.VerticalNames;
    UnregisterEncodingNames(AEncodingNames);
    AEncodingNames := ATempEncoding.HorizontalNames;
    UnregisterEncodingNames(AEncodingNames);
  finally
    ATempEncoding.Free;
  end;
end;

initialization
  dxPDFRegisterDocumentObjectClass(TdxPDFMacRomanEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFStandartEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFWinAnsiEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFSymbolEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFZapfDingbatsEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFHorizontalIdentityEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFVerticalIdentityEncoding);
  dxPDFRegisterDocumentObjectClass(TdxPDFDocEncoding);
  dxPDFRegisterPredefinedCompositeFontEncodings;

finalization
  dxPDFUnregisterPredefinedCompositeFontEncodings;
  dxPDFUnregisterDocumentObjectClass(TdxPDFDocEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFVerticalIdentityEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFHorizontalIdentityEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFZapfDingbatsEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFSymbolEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFWinAnsiEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFStandartEncoding);
  dxPDFUnregisterDocumentObjectClass(TdxPDFMacRomanEncoding);

end.

