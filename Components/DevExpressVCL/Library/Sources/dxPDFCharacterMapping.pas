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

unit dxPDFCharacterMapping;

{$I cxVer.inc}

interface

uses
  SysUtils, Types, Generics.Defaults, Generics.Collections, dxCore, dxPDFBase, dxPDFTypes, dxPDFParser;

type
  TdxPDFGlyphMappingFlags = (mfNone = 0, mfUseKerning = 1, mfDirectionRightToLeft = 2);

  { TdxPDFCharacterMappingSearchResult }

  TdxPDFCharacterMappingSearchResult = record
    CodeLength: Integer;
    Value: string;
  end;

  { TdxPDFCharacterMappingTreeNode }

  TdxPDFCharacterMappingTreeNode = class
  protected
    function CancelSearching: Boolean; virtual;
    function FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult; virtual; abstract;
    procedure AddCode(const ACode: TBytes; APosition: Integer; const AValue: string); virtual;
  public
    constructor Create; virtual;
  end;

  { TdxPDFCharacterMappingTreeLeaf }

  TdxPDFCharacterMappingTreeLeaf = class(TdxPDFCharacterMappingTreeNode)
  strict private
    FValue: string;
  strict protected
    function CancelSearching: Boolean; override;
    function FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult; override;
  public
    constructor Create(const AValue: string); reintroduce;
  end;

  { TdxPDFCharacterMappingTreeBranch }

  TdxPDFCharacterMappingTreeBranch = class(TdxPDFCharacterMappingTreeNode)
  strict private
    FNodes: TObjectDictionary<Byte, TdxPDFCharacterMappingTreeNode>;
  strict protected
    function FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult; override;
    procedure AddCode(const ACode: TBytes; APosition: Integer; const AValue: string); override;

    property Nodes: TObjectDictionary<Byte, TdxPDFCharacterMappingTreeNode> read FNodes;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TdxPDFCMapRecord }

  TdxPDFCMapRecord = class
  strict private
    FCode: TBytes;
    FValue: string;
  public
    constructor Create(const ACode: TBytes; const AValue: string);
    destructor Destroy; override;

    property Code: TBytes read FCode;
    property Value: string read FValue;
  end;

  { TdxPDFCharacterMapping }

  TdxPDFCharacterMapping = class
  strict private
    FMaxCodeLength: Integer;
    FTree: TdxPDFCharacterMappingTreeBranch;
    procedure PopulateTree(ACMapTable: TList<TdxPDFCMapRecord>);
  public
    constructor Create(ACMapTable: TList<TdxPDFCMapRecord>; AMaxByteCount: Integer);
    destructor Destroy; override;

    class function CreateCharacterMappingData(ACharMap: TDictionary<Integer, string>; const AFontName: string): TBytes;

    function GetStringData(const AData: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
    function FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult;
    function MapCode(const ACode: TBytes): string;
  end;

  { TdxPDFCMapStreamParser }

  TdxPDFCMapStreamParser = class(TdxPDFStructureParser)
  strict private
    FMaxByteCount: Integer;

    function CheckCode(const ALastCode: TBytes; var ACode: TBytes): Boolean;
    function ReadCode: TBytes;
    function ToUnicode(const ABytes: TBytes): string;
    procedure PopulateCMapTable(ACMapTable: TObjectList<TdxPDFCMapRecord>);
    procedure ReadChars(ACMapTable: TObjectList<TdxPDFCMapRecord>; ACount: Integer);
    procedure ReadRange(ACMapTable: TObjectList<TdxPDFCMapRecord>; var ACode, ALastCode: TBytes);
    procedure ReadRangeCodes(var ACode, ALastCode: TBytes);
    procedure ReadRanges(ACMapTable: TObjectList<TdxPDFCMapRecord>; ACount: Integer);
  strict protected
    function CanContinueTokenReading: Boolean; override;
    function ReadHexadecimalString(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFString; override;
    function ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase; override;

    function DoRead: TdxPDFCharacterMapping;
  public
    class function Parse(ARepository: TdxPDFCustomRepository; const AData: TBytes): TdxPDFCharacterMapping;
    function Read(const AData: TBytes): TdxPDFCharacterMapping;
  end;

  { TdxPDFCharacterCache }

  TdxPDFCharacterCache = class
  strict private
    FHasAddedCharacters: Boolean;
    FMapping: TDictionary<Integer, string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterString(AToUnicode: TDictionary<Integer, string>);

    property HasAddedCharacters: Boolean read FHasAddedCharacters write FHasAddedCharacters;
    property Mapping: TDictionary<Integer, string> read FMapping;
  end;

  { TdxPDFGlyph }

  TdxPDFGlyph = record
  strict private
    FIndex: Integer;
    FOffset: Double;
    FWidth: Double;
    function GetActualWidth: Double;
  public
    class function Create(AIndex: Integer; AWidth, AOffset: Double): TdxPDFGlyph; static;

    property ActualWidth: Double read GetActualWidth;
    property &Index: Integer read FIndex;
    property Offset: Double read FOffset;
    property Width: Double read FWidth;
  end;

  TdxPDFGlyphList = class(TList<TdxPDFGlyph>);

  { TdxPDFGlyphRun }

  TdxPDFGlyphRun = class
  strict private
    FGlyphs: TdxPDFGlyphList;
    FWidth: Double;
    function GetGlyphOffsets: TDoubleDynArray;
    function GetEmpty: Boolean;
  protected
    function GetByteCountPerGlyph: Integer; virtual; abstract;
    function GetTextData: TBytes; virtual; abstract;

    property ByteCountPerGlyph: Integer read GetByteCountPerGlyph;
  public
    constructor Create; overload;
    constructor Create(AGlyphs: TdxPDFGlyphList); overload;
    destructor Destroy; override;

    function CreateCompatible: TdxPDFGlyphRun; virtual; abstract;

    procedure Append(AGlyph: TdxPDFGlyph); overload;
    procedure Append(AGlyphRun: TdxPDFGlyphRun); overload;
    procedure Prepend(AGlyph: TdxPDFGlyph); overload;
    procedure Prepend(AGlyphRun: TdxPDFGlyphRun; ASpaceGlyphIndex: Integer); overload;
    procedure RemoveLast;

    property GlyphOffsets: TDoubleDynArray read GetGlyphOffsets;
    property Glyphs: TdxPDFGlyphList read FGlyphs;
    property Empty: Boolean read GetEmpty;
    property TextData: TBytes read GetTextData;
    property Width: Double read FWidth;
  end;

  TdxPDFGlyphRunList = class(TList<TdxPDFGlyphRun>);

  { TdxPDFSimpleFontGlyphRun }

  TdxPDFSimpleFontGlyphRun = class(TdxPDFGlyphRun)
  protected
    function GetByteCountPerGlyph: Integer; override;
    function GetTextData: TBytes; override;
  public
    function CreateCompatible: TdxPDFGlyphRun; override;
  end;

  { TdxPDFCompositeFontGlyphRun }

  TdxPDFCompositeFontGlyphRun = class(TdxPDFGlyphRun)
  protected
    function GetByteCountPerGlyph: Integer; override;
    function GetTextData: TBytes; override;
  public
    function CreateCompatible: TdxPDFGlyphRun; override;
  end;

  TdxPDFCompositeFontGlyphRun2 = class(TdxPDFCompositeFontGlyphRun);

  { TdxPDFGlyphMappingResult }

  TdxPDFGlyphMappingResult = record
  strict private
    FGlyphRun: TdxPDFGlyphRun;
    FMapping: TDictionary<Integer, string>;
  public
    class function Create(AGlyphRun: TdxPDFGlyphRun): TdxPDFGlyphMappingResult; overload; static;
    class function Create(AGlyphRun: TdxPDFGlyphRun;
      AMapping: TDictionary<Integer, string>): TdxPDFGlyphMappingResult; overload; static;

    property GlyphRun: TdxPDFGlyphRun read FGlyphRun;
    property Mapping: TDictionary<Integer, string> read FMapping;
  end;

  { TdxPDFGlyphPlacementInfo }

  TdxPDFGlyphPlacementInfo = record
  strict private
    FGlyphIndices: TIntegerDynArray;
    FGlyphPositions: TIntegerDynArray;
    FOrder: TIntegerDynArray;
    FScaleFactor: Double;
    function GetIsEmptyOrInvalid: Boolean;
  public
    class function Create(const AGlyphIndices, AGlyphPositions, AOrder: TIntegerDynArray;
      ADPI, AFontSize: Double): TdxPDFGlyphPlacementInfo; static;

    property GlyphIndices: TIntegerDynArray read FGlyphIndices;
    property GlyphPositions: TIntegerDynArray read FGlyphPositions;
    property IsEmptyOrInvalid: Boolean read GetIsEmptyOrInvalid;
    property Order: TIntegerDynArray read FOrder;
    property ScaleFactor: Double read FScaleFactor;
  end;

implementation

uses
  Classes, Variants, Math, dxPDFUtils, dxPDFFont, dxPDFFontUtils, dxFontFile;

function dxPDFCharacterMappingSearchResult(AValue: string; ACodeLength: Integer): TdxPDFCharacterMappingSearchResult;
begin
  Result.CodeLength := ACodeLength;
  Result.Value := AValue;
end;

{ TdxPDFCharacterMappingTreeNode }

constructor TdxPDFCharacterMappingTreeNode.Create;
begin
  inherited Create;
end;

function TdxPDFCharacterMappingTreeNode.CancelSearching: Boolean;
begin
  Result := False;
end;

procedure TdxPDFCharacterMappingTreeNode.AddCode(const ACode: TBytes; APosition: Integer; const AValue: string);
begin
// do nothing
end;

{ TdxPDFCharacterMappingTreeLeaf }

constructor TdxPDFCharacterMappingTreeLeaf.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TdxPDFCharacterMappingTreeLeaf.CancelSearching: Boolean;
begin
  Result := True;
end;

function TdxPDFCharacterMappingTreeLeaf.FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult;
begin
  Result := dxPDFCharacterMappingSearchResult(FValue, 0);
end;

{ TdxPDFCharacterMappingTreeBranch }

constructor TdxPDFCharacterMappingTreeBranch.Create;
begin
  inherited Create;
  FNodes := TObjectDictionary<Byte, TdxPDFCharacterMappingTreeNode>.Create([doOwnsValues]);
end;

destructor TdxPDFCharacterMappingTreeBranch.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

function TdxPDFCharacterMappingTreeBranch.FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult;
var
  ANode: TdxPDFCharacterMappingTreeNode;
  ASearchResult: TdxPDFCharacterMappingSearchResult;
begin
  if (Length(ACode) - APosition > 0) and Nodes.TryGetValue(ACode[APosition], ANode) then
  begin
    ASearchResult := ANode.FindCode(ACode, APosition + 1);
    Exit(dxPDFCharacterMappingSearchResult(ASearchResult.Value, ASearchResult.CodeLength + 1));
  end;
  Result := dxPDFCharacterMappingSearchResult('', 0);
end;

procedure TdxPDFCharacterMappingTreeBranch.AddCode(const ACode: TBytes; APosition: Integer; const AValue: string);
var
  AFirstByte: Byte;
  ANode: TdxPDFCharacterMappingTreeBranch;
  ACodeLength: Integer;
  ACurrentNode: TdxPDFCharacterMappingTreeNode;
begin
  ACodeLength := Length(ACode) - APosition;
  if ACodeLength > 0 then
  begin
    AFirstByte := ACode[APosition];
    if Nodes.TryGetValue(AFirstByte, ACurrentNode) then
    begin
      if not ACurrentNode.CancelSearching then
        if ACodeLength = 1 then
          Nodes.Add(AFirstByte, TdxPDFCharacterMappingTreeLeaf.Create(AValue))
        else
          ACurrentNode.AddCode(ACode, APosition + 1, AValue);
    end
    else
      if ACodeLength = 1 then
        Nodes.Add(AFirstByte, TdxPDFCharacterMappingTreeLeaf.Create(AValue))
      else
      begin
        ANode := TdxPDFCharacterMappingTreeBranch.Create;
        ANode.AddCode(ACode, APosition + 1, AValue);
        Nodes.Add(AFirstByte, ANode);
      end;
  end;
end;

{ TdxPDFCMapRecord }

constructor TdxPDFCMapRecord.Create(const ACode: TBytes; const AValue: string);
begin
  inherited Create;
  TdxPDFUtils.AddData(ACode, FCode);
  FValue := AValue;
end;

destructor TdxPDFCMapRecord.Destroy;
begin
  SetLength(FCode, 0);
  inherited Destroy;
end;

{ TdxPDFCharacterMapping }

constructor TdxPDFCharacterMapping.Create(ACMapTable: TList<TdxPDFCMapRecord>; AMaxByteCount: Integer);
begin
  inherited Create;
  FTree := TdxPDFCharacterMappingTreeBranch.Create;
  PopulateTree(ACMapTable);
end;

destructor TdxPDFCharacterMapping.Destroy;
begin
  FreeAndNil(FTree);
  inherited Destroy;
end;

class function TdxPDFCharacterMapping.CreateCharacterMappingData(ACharMap: TDictionary<Integer, string>;
  const AFontName: string): TBytes;
var
  AStream: TBytesStream;
  AWriter: TStreamWriter;
  AChars, ARanges: TList<TPair<Integer, string>>;
  APair: TPair<Integer, string>;
  AGlyph: string;
  C: Char;
  ABytes: TBytes;
  S: string;
begin
  AStream := TBytesStream.Create;
  try
    AWriter := TStreamWriter.Create(AStream);
    try
      AWriter.WriteLine('/CIDInit /ProcSet findresource begin');
      AWriter.WriteLine('12 dict begin');
      AWriter.WriteLine('begincmap');
      AWriter.WriteLine('/CIDSystemInfo');
      AWriter.WriteLine('<<');
      AWriter.WriteLine('/Registry (Adobe)');
      AWriter.WriteLine('/Ordering (' + AFontName + ')');
      AWriter.WriteLine('/Supplement 0');
      AWriter.WriteLine('>> def');
      AWriter.WriteLine('/CMapName /' + AFontName + ' def');
      AWriter.WriteLine('/CMapType 2 def');
      AWriter.WriteLine('1 begincodespacerange');
      AWriter.WriteLine('<0000> <FFFF>');
      AWriter.WriteLine('endcodespacerange');
      AChars := TList<TPair<Integer, string>>.Create;
      ARanges := TList<TPair<Integer, string>>.Create;
      try
        if ACharMap.Count > 0 then
        begin
          for APair in ACharMap do
            if Length(APair.Value) = 1 then
              AChars.Add(APair)
            else
              ARanges.Add(APair);
          if AChars.Count > 0 then
          begin
            AWriter.Write(IntToStr(AChars.Count));
            AWriter.WriteLine(' beginbfchar');
            for APair in AChars do
            begin
              ABytes := TEncoding.Unicode.GetBytes(APair.Value);
              S := '<' + IntToHex(APair.Key, 4) + '> <' + IntToHex(ABytes[1], 2) +  IntToHex(ABytes[0], 2) + '>';
              AWriter.WriteLine(S);
            end;
            AWriter.WriteLine('endbfchar');
          end;
          if ARanges.Count > 0 then
          begin
            AWriter.Write(ARanges.Count);
            AWriter.WriteLine(' beginbfrange');
            for APair in ARanges do
            begin
              AGlyph := IntToHex(APair.Key, 4);
              AWriter.Write('<' + AGlyph + '><' + AGlyph + '><');
              for C in APair.Value do
              begin
                ABytes := TEncoding.Unicode.GetBytes(C);
                AWriter.Write(IntToHex(ABytes[0], 4));
              end;
              AWriter.WriteLine('>');
            end;
            AWriter.WriteLine('endbfrange');
          end;
        end;
      finally
        AChars.Free;
        ARanges.Free;
      end;
      AWriter.WriteLine('endcmap');
      AWriter.WriteLine('CMapName currentdict /CMap defineresource pop');
      AWriter.WriteLine('end');
      AWriter.WriteLine('end');
    finally
      AWriter.Free;
    end;
    Exit(AStream.Bytes);
  finally
    AStream.Free;
  end;
end;

function TdxPDFCharacterMapping.GetStringData(const AData: TBytes; var AGlyphOffsets: TDoubleDynArray): TdxPDFStringData;
var
  ACodes: TList<TBytes>;
  AActualOffsets: TDoubleDynArray;
  AResult: TList<SmallInt>;
  ALength, APosition, ACodeLength: Integer;
  AFindResult: TdxPDFCharacterMappingSearchResult;
  ACode: TBytes;
  I: Integer;
begin
  ACodes := TList<TBytes>.Create;

  AResult := TList<SmallInt>.Create;
  try

    ALength := Length(AData);

    APosition := 0;
    while APosition < ALength do
    begin
      AFindResult := FindCode(AData, APosition);
      ACodeLength := AFindResult.CodeLength;
      if ACodeLength = 0 then
      begin
        AResult.Add(SmallInt(32));
        ACodeLength := 1;
      end
      else
        if dxWideIsNumeric(AFindResult.Value[1]) and (Length(AFindResult.Value) > 1) then
          AResult.Add(StrToInt(AFindResult.Value))
        else
          AResult.Add(SmallInt(AFindResult.Value[1]));

      SetLength(ACode, ACodeLength);
      TdxPDFUtils.CopyData(AData, APosition, ACode, 0, ACodeLength);

      ACodes.Add(ACode);
      if (AGlyphOffsets <> nil) and (Length(AGlyphOffsets) > 0) then
        TdxPDFUtils.AddValue(AGlyphOffsets[APosition], AActualOffsets);
      Inc(APosition, ACodeLength);
    end;

    if (AGlyphOffsets <> nil) and (Length(AGlyphOffsets) > 0) then
    begin
      TdxPDFUtils.AddValue(0, AActualOffsets);
      SetLength(Result.Offsets, 0);
      TdxPDFUtils.AddData(AActualOffsets, Result.Offsets);
    end
    else
    begin
      SetLength(Result.Offsets, 0);
      for I := 0 to AResult.Count do
        TdxPDFUtils.AddValue(0, Result.Offsets);
    end;

    SetLength(AGlyphOffsets, 0);
    TdxPDFUtils.AddData(Result.Offsets, AGlyphOffsets);

    SetLength(Result.CharacterCodes, ACodes.Count);
    for I := 0 to ACodes.Count - 1 do
      Result.CharacterCodes[I] := ACodes[I];

    SetLength(Result.Glyphs, AResult.Count);
    for I := 0 to AResult.Count - 1 do
      Result.Glyphs[I] := AResult[I];
  finally
    ACodes.Free;
    AResult.Free;
  end;
end;

function TdxPDFCharacterMapping.FindCode(const ACode: TBytes; APosition: Integer): TdxPDFCharacterMappingSearchResult;
var
  ACodeLength: Integer;
  ANewCode: TBytes;
begin
  Result := FTree.FindCode(ACode, APosition);
  ACodeLength := Length(ACode) - APosition;
  if (Result.CodeLength < 0) and (ACodeLength < FMaxCodeLength) then
  begin
    SetLength(ANewCode, ACodeLength + 1);
    cxCopyData(@ACode[0], @ANewCode[0], APosition, 1, ACodeLength * SizeOf(Byte));
    Result := FindCode(ANewCode, 0);
    Result := dxPDFCharacterMappingSearchResult(Result.Value, ACodeLength);
  end;
end;

function TdxPDFCharacterMapping.MapCode(const ACode: TBytes): string;
var
  ACharCount, ALength: Integer;
  ASearchResult: TdxPDFCharacterMappingSearchResult;
begin
  ASearchResult := FindCode(ACode, 0);
  if ASearchResult.CodeLength > 0 then
    Result := ASearchResult.Value
  else
  begin
    Result := '';
    ALength := Length(ACode);
    if (ALength > 0) and (ACode[0] <> 0) then
    begin
      ACharCount := TEncoding.BigEndianUnicode.GetCharCount(ACode, 0, ALength);
      if ACharCount > 0 then
        Result := TdxPDFUtils.ConvertToStr(ACode, ALength);
    end;
  end;
end;

procedure TdxPDFCharacterMapping.PopulateTree(ACMapTable: TList<TdxPDFCMapRecord>);
var
  I: Integer;
begin
  for I := 0 to ACMapTable.Count - 1 do
  begin
    FMaxCodeLength := Max(FMaxCodeLength, Length(ACMapTable[I].Code));
    FTree.AddCode(ACMapTable[I].Code, 0, ACMapTable[I].Value);
  end;
end;

{ TdxPDFCMapStreamParser }

class function TdxPDFCMapStreamParser.Parse(ARepository: TdxPDFCustomRepository; const AData: TBytes): TdxPDFCharacterMapping;
var
  AParser: TdxPDFCMapStreamParser;
begin
  AParser := TdxPDFCMapStreamParser.Create(ARepository);
  try
    try
      Result := AParser.Read(AData);
    except
      Result := nil;
    end;
  finally
    AParser.Free;
  end;
end;

function TdxPDFCMapStreamParser.Read(const AData: TBytes): TdxPDFCharacterMapping;
begin
  Data := AData;
  Result := DoRead;
end;

function TdxPDFCMapStreamParser.CanContinueTokenReading: Boolean;
begin
  Result := inherited CanContinueTokenReading and (Current <> TdxPDFDefinedSymbols.StartObject);
end;

function TdxPDFCMapStreamParser.ReadHexadecimalString(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFString;
var
  AData: TBytes;
  ASymbol: Byte;
begin
  Result := nil;
  if ((Current = TdxPDFDefinedSymbols.StartObject) and ReadNext) then
  begin
    while SkipSpaces do
    begin
      if Current = TdxPDFDefinedSymbols.EndObject then
      begin
        ReadNext;
        Result := TdxPDFString.Create(TdxPDFUtils.BytesToStr(AData));
        Break;
      end;
      ASymbol := GetHexDigit;
      if not ReadNext then
        TdxPDFUtils.Abort;
      ASymbol := ASymbol * 16;
      if Current = TdxPDFDefinedSymbols.EndObject then
        TdxPDFUtils.AddByte(ASymbol, AData)
      else
      begin
        TdxPDFUtils.AddByte(ASymbol + GetHexDigit, AData);
        if not ReadNext then
          TdxPDFUtils.Abort;
      end;
    end;
  end
  else
    TdxPDFUtils.RaiseTestException;
end;

function TdxPDFCMapStreamParser.ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase;
var
  ADictionaryOrStream: TdxPDFBase;
begin
  ADictionaryOrStream := DoReadDictionaryOrStream;
  if ADictionaryOrStream = nil then
    Result := TdxPDFString.Create(DoReadToken)
  else
    Result := ADictionaryOrStream;
end;

procedure TdxPDFCMapStreamParser.ReadChars(ACMapTable: TObjectList<TdxPDFCMapRecord>; ACount: Integer);
var
  I: Integer;
  AValue: TdxPDFBase;
  ACode, AUnicodeCharacters: TBytes;
begin
  for I := 0 to ACount - 1 do
  begin
    ACode := ReadCode;
    AValue := DoReadObject;
    if AValue <> nil then
    begin
      case AValue.ObjectType of
        otString:
          if TdxPDFString(AValue).Value = '' then
            ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, ''))
          else
          begin
            AUnicodeCharacters := TdxPDFUtils.StrToByteArray(TdxPDFString(AValue).Value);
            ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, ToUnicode(AUnicodeCharacters)));
          end;
        otInteger:
          ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, IntToStr(TdxPDFInteger(AValue).Value)));
      end;
      dxPDFFreeObject(AValue);
    end;
  end;
end;

procedure TdxPDFCMapStreamParser.ReadRange(ACMapTable: TObjectList<TdxPDFCMapRecord>; var ACode, ALastCode: TBytes);
var
  I, AValue: Integer;
  ARange: TdxPDFBase;
  AUnicode: string;
  AUnicodeRangeStart: TBytes;
  AUnicodeRangeValue: Char;
begin
  ARange := DoReadObject;
  if ARange <> nil then
  begin
    case ARange.ObjectType of
      otArray:
        begin
          I := 0;
          repeat
            if I >= TdxPDFArray(ARange).Count then
              TdxPDFUtils.Abort;
            AUnicodeRangeStart := TdxPDFUtils.StrToByteArray((TdxPDFArray(ARange)[I] as TdxPDFString).Value);
            Inc(I);
            ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, ToUnicode(AUnicodeRangeStart)));
          until not CheckCode(ALastCode, ACode);
         end;
      otInteger:
          if TdxPDFUtils.IsIntegerValid(TdxPDFInteger(ARange).Value) then
          begin
            AValue := TdxPDFInteger(ARange).Value;
            repeat
              ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, Char(AValue)));
              Inc(AValue);
            until not CheckCode(ALastCode, ACode);
          end;
      otString:
        begin
          AUnicode := ToUnicode(TdxPDFUtils.StrToByteArray(TdxPDFString(ARange).Value));
          AUnicodeRangeValue := AUnicode[1];
          repeat
            ACMapTable.Add(TdxPDFCMapRecord.Create(ACode, AUnicodeRangeValue));
            Inc(AUnicodeRangeValue);
          until not CheckCode(ALastCode, ACode);
        end;
    end;
    dxPDFFreeObject(ARange);
  end;
end;

procedure TdxPDFCMapStreamParser.ReadRangeCodes(var ACode, ALastCode: TBytes);

  procedure CalculateRangeCode(ACodeLength, ALastCodeLength, ALengthDifference: Integer; var ACode: TBytes);
  var
    ATempCode: TBytes;
  begin
    SetLength(ATempCode, ALastCodeLength);
    cxCopyData(@ACode[0], @ATempCode[0], 0, -ALengthDifference, ACodeLength * SizeOf(Byte));
    SetLength(ACode, 0);
    ACode := ATempCode;
  end;

var
  ACodeLength, ALastCodeLength, ADelta: Integer;
begin
  ACode := ReadCode;
  ALastCode := ReadCode;
  ACodeLength := Length(ACode);
  ALastCodeLength := Length(ALastCode);
  ADelta := ACodeLength - ALastCodeLength;
  if ADelta < 0 then
    CalculateRangeCode(ACodeLength, ALastCodeLength, ADelta, ACode)
  else
    if ADelta > 0 then
      CalculateRangeCode(ALastCodeLength, ACodeLength, -ADelta, ALastCode);
end;

procedure TdxPDFCMapStreamParser.ReadRanges(ACMapTable: TObjectList<TdxPDFCMapRecord>; ACount: Integer);
var
  I: Integer;
  ACode, ALastCode: TBytes;
begin
  SetLength(ACode, 0);
  SetLength(ALastCode, 0);
  for I := 0 to ACount - 1 do
  begin
    ReadRangeCodes(ACode, ALastCode);
    ReadRange(ACMapTable, ACode, ALastCode);
  end;
  SetLength(ACode, 0);
  SetLength(ALastCode, 0);
end;

procedure TdxPDFCMapStreamParser.PopulateCMapTable(ACMapTable: TObjectList<TdxPDFCMapRecord>);
var
  ARecordCount: Integer;
  AToken: TdxPDFString;
  AValue: TdxPDFBase;
begin
  ARecordCount := 0;
  while SkipSpaces do
  begin
    AValue := DoReadObject;
    if AValue <> nil then
      try
        case AValue.ObjectType of
          otString:
            begin
              AToken := AValue as TdxPDFString;
              if AToken.Value = 'endcmap' then
                Break
              else
                if (AToken.Value = 'beginbfchar') or (AToken.Value = 'begincidchar') then
                  ReadChars(ACMapTable, ARecordCount)
                else
                  if (AToken.Value = 'beginbfrange') or (AToken.Value = 'begincidrange') then
                    ReadRanges(ACMapTable, ARecordCount);
            end;
          otInteger:
            ARecordCount := TdxPDFInteger(AValue).Value;
        end;
      finally
        dxPDFFreeObject(AValue);
      end;
  end;
end;

function TdxPDFCMapStreamParser.DoRead: TdxPDFCharacterMapping;
var
  ACMapTable: TObjectList<TdxPDFCMapRecord>;
begin
  ACMapTable := TObjectList<TdxPDFCMapRecord>.Create;
  try
    PopulateCMapTable(ACMapTable);
    Result := TdxPDFCharacterMapping.Create(ACMapTable, FMaxByteCount);
  finally
    ACMapTable.Free;
  end;
end;

function TdxPDFCMapStreamParser.ToUnicode(const ABytes: TBytes): string;
var
  AArray: TCharArray;
  ACount, I, AIndex: Integer;
  AIsCountOdd: Boolean;
begin
  Result := '';
  if ABytes <> nil then
  begin
    ACount := Length(ABytes);
    AIsCountOdd := ACount mod 2 <> 0;
    ACount := IfThen(AIsCountOdd, (ACount + 1) div 2, ACount div 2);
    AIndex := 0;
    for I := 0 to ACount - 1 do
    begin
      if AIsCountOdd and (I = 0) then
        TdxPDFUtils.AddChar(Char(ABytes[AIndex]), AArray)
      else
      begin
        TdxPDFUtils.AddChar(Char(ABytes[AIndex] shl 8 + ABytes[AIndex + 1]), AArray);
        Inc(AIndex);
      end;
      Inc(AIndex);
    end;
    for I := 0 to Length(AArray) - 1 do
      Result := Result + AArray[I];
  end
  else
    TdxPDFUtils.Abort;
end;

function TdxPDFCMapStreamParser.ReadCode: TBytes;
var
  AValue, ACount, AByteCount, I: Integer;
begin
  AValue := 0;
  ACount := 0;
  SkipSpaces;
  if Current = TdxPDFDefinedSymbols.StartObject then
  begin
    ReadNext;
    while Current <> TdxPDFDefinedSymbols.EndObject do
    begin
      if not IsWhiteSpace then
      begin
        AValue := AValue shl 4 + GetHexDigit;
        Inc(ACount);
      end;
      ReadNext;
    end;
  end;
  ReadNext;
  AByteCount := Ceil(ACount / 2.0);
  FMaxByteCount := Max(AByteCount, FMaxByteCount);
  SetLength(Result, AByteCount);
  for I := AByteCount - 1 downto 0 do
  begin
    Result[I] := AValue and $FF;
    AValue := AValue shr 8;
  end;
end;

function TdxPDFCMapStreamParser.CheckCode(const ALastCode: TBytes; var ACode: TBytes): Boolean;
var
  ALength, I, AIndex: Integer;
  B: Byte;
begin
  Result := False;
  ALength := Length(ACode);
  for I := 0 to ALength - 1 do
    if ACode[I] <> ALastCode[I] then
    begin
      Result := True;
      Break;
    end;
  if Result then
  begin
    for AIndex := ALength - 1 downto 0 do
    begin
      B := ACode[AIndex];
      if B = $FF then
        ACode[AIndex] := 0
      else
      begin
        ACode[AIndex] := Byte((B + 1));
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TdxPDFCharacterCache }

constructor TdxPDFCharacterCache.Create;
begin
  inherited Create;
  FHasAddedCharacters := True;
  FMapping := TDictionary<Integer, string>.Create;
end;

destructor TdxPDFCharacterCache.Destroy;
begin
  FreeAndNil(FMapping);
  inherited Destroy;
end;

procedure TdxPDFCharacterCache.RegisterString(AToUnicode: TDictionary<Integer, string>);
var
  ACode: Integer;
  APair: TPair<Integer, string>;
begin
  for APair in AToUnicode do
  begin
    ACode := APair.Key;
    if not FMapping.ContainsKey(ACode) then
    begin
      FMapping.Add(ACode, APair.Value);
      FHasAddedCharacters := True;
    end;
  end;
end;

{ TdxPDFGlyph }

class function TdxPDFGlyph.Create(AIndex: Integer; AWidth: Double; AOffset: Double): TdxPDFGlyph;
begin
  Result.FIndex := AIndex;
  Result.FWidth := AWidth;
  Result.FOffset := AOffset;
end;

function TdxPDFGlyph.GetActualWidth: Double;
begin
  Result := FWidth - FOffset;
end;

{ TdxPDFGlyphRun }

constructor TdxPDFGlyphRun.Create;
begin
  inherited Create;
  FGlyphs := TdxPDFGlyphList.Create;
  FWidth := 0;
end;

constructor TdxPDFGlyphRun.Create(AGlyphs: TdxPDFGlyphList);
var
  AGlyph: TdxPDFGlyph;
begin
  inherited Create;
  FGlyphs := AGlyphs;
  for AGlyph in AGlyphs do
    FWidth := FWidth + AGlyph.ActualWidth;
end;

destructor TdxPDFGlyphRun.Destroy;
begin
  FreeAndNil(FGlyphs);
  inherited Destroy;
end;

procedure TdxPDFGlyphRun.Append(AGlyph: TdxPDFGlyph);
begin
  FGlyphs.Add(AGlyph);
  FWidth := FWidth + AGlyph.ActualWidth;
end;

procedure TdxPDFGlyphRun.Append(AGlyphRun: TdxPDFGlyphRun);
var
  I: Integer;
begin
  for I := 0 to AGlyphRun.Glyphs.Count - 1 do
    Append(AGlyphRun.Glyphs[I]);
end;

procedure TdxPDFGlyphRun.Prepend(AGlyph: TdxPDFGlyph);
begin
  FGlyphs.Insert(0, AGlyph);
  FWidth := FWidth + AGlyph.ActualWidth;
end;

procedure TdxPDFGlyphRun.Prepend(AGlyphRun: TdxPDFGlyphRun; ASpaceGlyphIndex: Integer);
var
  J, I: Integer;
  AGlyph: TdxPDFGlyph;
begin
  J := 0;
  for I := AGlyphRun.Glyphs.Count - 1 downto 0 do
  begin
    AGlyph := AGlyphRun.Glyphs[I];
    if AGlyph.Index <> ASpaceGlyphIndex then
    begin
      FGlyphs.Insert(0, AGlyph);
      Inc(J);
    end
    else
      FGlyphs.Insert(J, AGlyph);
    FWidth := FWidth + AGlyph.ActualWidth;
  end;
end;

procedure TdxPDFGlyphRun.RemoveLast;
var
  ALastPos: Integer;
begin
  if FGlyphs.Count > 0 then
  begin
    ALastPos := FGlyphs.Count - 1;
    FWidth := FWidth - FGlyphs[ALastPos].ActualWidth;
    FGlyphs.Delete(ALastPos);
  end;
end;

function TdxPDFGlyphRun.GetGlyphOffsets: TDoubleDynArray;
var
  ACount, AByteCount, I, K: Integer;
begin
  SetLength(Result, 0);
  ACount := FGlyphs.Count;
  if ACount >= 1 then
  begin
    AByteCount := ByteCountPerGlyph;
    SetLength(Result, (ACount + 1) * AByteCount);
    K := 0;
    for I := 0 to ACount - 1 do
    begin
      Result[K] := FGlyphs[I].Offset;
      Inc(K, AByteCount);
    end;
  end;
end;

function TdxPDFGlyphRun.GetEmpty: Boolean;
begin
  Result := FGlyphs.Count = 0;
end;

{ TdxPDFSimpleFontGlyphRun }

function TdxPDFSimpleFontGlyphRun.CreateCompatible: TdxPDFGlyphRun;
begin
  Result := TdxPDFSimpleFontGlyphRun.Create;
end;

function TdxPDFSimpleFontGlyphRun.GetByteCountPerGlyph: Integer;
begin
  Result := 1;
end;

function TdxPDFSimpleFontGlyphRun.GetTextData: TBytes;
var
  I: Integer;
  ACharacters: TCharArray;
begin
  SetLength(ACharacters, Glyphs.Count);
  for I := 0 to Glyphs.Count - 1 do
    ACharacters[I] := Char(Glyphs[I].Index);
  Result := TEncoding.GetEncoding(1252).GetBytes(ACharacters);
end;

{ TdxPDFCompositeFontGlyphRun }

function TdxPDFCompositeFontGlyphRun.CreateCompatible: TdxPDFGlyphRun;
begin
  Result := TdxPDFCompositeFontGlyphRun.Create;
end;

function TdxPDFCompositeFontGlyphRun.GetByteCountPerGlyph: Integer;
begin
  Result := 2;
end;

function TdxPDFCompositeFontGlyphRun.GetTextData: TBytes;
var
  AByteCount, I, APosition, AGlyphIndex, B: Integer;
begin
  SetLength(Result, 0);
  if Glyphs.Count >= 1 then
  begin
    AByteCount := ByteCountPerGlyph;
    SetLength(Result, Glyphs.Count * AByteCount);
    APosition := 0;
    for I := 0 to Glyphs.Count - 1 do
    begin
      AGlyphIndex := Glyphs[I].Index;
      for B := AByteCount - 1 downto 0 do
      begin
        Result[APosition] := Byte((AGlyphIndex shr (8 * B)) and $FF);
        Inc(APosition);
      end;
    end;
  end;
end;

{ TdxPDFGlyphMappingResult }

class function TdxPDFGlyphMappingResult.Create(AGlyphRun: TdxPDFGlyphRun;
  AMapping: TDictionary<Integer, string>): TdxPDFGlyphMappingResult;
begin
  Result.FGlyphRun := AGlyphRun;
  Result.FMapping := AMapping;
end;

class function TdxPDFGlyphMappingResult.Create(AGlyphRun: TdxPDFGlyphRun): TdxPDFGlyphMappingResult;
begin
  Result := Create(AGlyphRun, nil);
end;

{ TdxPDFGlyphPlacementInfo }

class function TdxPDFGlyphPlacementInfo.Create(const AGlyphIndices, AGlyphPositions, AOrder: TIntegerDynArray;
  ADPI, AFontSize: Double): TdxPDFGlyphPlacementInfo;
begin
  Result.FGlyphIndices := AGlyphIndices;
  Result.FGlyphPositions := AGlyphPositions;
  Result.FOrder := AOrder;
  Result.FScaleFactor := ADPI / 72 * AFontSize / 1000;
end;

function TdxPDFGlyphPlacementInfo.GetIsEmptyOrInvalid: Boolean;
begin
  Result := (FGlyphIndices = nil) or (FGlyphPositions = nil) or (FOrder = nil);
end;

end.

