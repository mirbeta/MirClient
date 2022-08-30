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

unit dxPDFStreamFilter;

{$I cxVer.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, dxPDFBase, dxPDFTypes, dxJBIG2;

type
  TdxPDFFilterPredictor = (fpNoPrediction = 1, fpTiffPredictor = 2, fpPngNonePrediction = 10, fpPngSubPrediction = 11,
    fpPngUpPrediction = 12, fpPngAveragePrediction = 13, fpPngPaethPrediction = 14, fpPngOptimumPrediction = 15);

  { TdxPDFUnknownStreamFilter }

  TdxPDFUnknownStreamFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFPredictableStreamFilter }

  TdxPDFPredictableStreamFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FColors: Integer;
    FColumns: Integer;
    FBitsPerComponent: Integer;
    FPredictor: TdxPDFFilterPredictor;

    procedure ApplyTIFFPrediction(const AData: TBytes; out Result: TBytes);
    procedure ApplyPNGPrediction(const AData: TBytes; out Result: TBytes);
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    function PerformDecode(const AData: TBytes): TBytes; virtual; abstract;
    procedure Initialize(ADecodeParameters: TObject); override;
    procedure DoInitialize(ADictionary: TdxPDFDictionary); virtual;
  end;

  { TdxPDFFlateStreamFilter }

  TdxPDFFlateStreamFilter = class(TdxPDFPredictableStreamFilter)
  protected
    function PerformDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    function Encode(const AData: TBytes): TBytes;
  end;

  { TdxPDFLZWStreamFilter }

  TdxPDFLZWStreamFilter = class(TdxPDFPredictableStreamFilter)
  strict protected
    FEarlyChangeValue: Boolean;
  protected
    function PerformDecode(const AData: TBytes): TBytes; override;
    procedure DoInitialize(ADictionary: TdxPDFDictionary); override;
    procedure Initialize(ADecodeParameters: TObject); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFASCIIHexDecodeFilter }

  TdxPDFASCIIHexDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private const
    FZero = Byte('0');
    FOne = Byte('1');
    FTwo = Byte('2');
    FThree = Byte('3');
    FFour = Byte('4');
    FFive = Byte('5');
    FSix = Byte('6');
    FSeven = Byte('7');
    FEight = Byte('8');
    FNine = Byte('9');
    FA = Byte('a');
    FB = Byte('b');
    FC = Byte('c');
    FD = Byte('d');
    FE = Byte('e');
    FF = Byte('f');
    FCapitalA = Byte('A');
    FCapitalB = Byte('B');
    FCapitalC = Byte('C');
    FCapitalD = Byte('D');
    FCapitalE = Byte('E');
    FCapitalF = Byte('F');
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFDCTDecodeFilter }

  TdxPDFDCTDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private type
  {$REGION 'Private helper types'}
    TDCTDecodeResult = record
      Data: TBytes;
      Stride: Integer;
    end;
    TDecoder = class
    strict private
      FImageData: TBytes;
      FImageWidth: Integer;
      FImageHeight: Integer;
    protected
      function DoDecode: TDCTDecodeResult;
      procedure Initialize(const AData: TBytes; AWidth, AHeight: Integer);
    public
      class function Decode(const AData: TBytes; AWidth, AHeight: Integer): TDCTDecodeResult; static;
    end;
  {$ENDREGION}
  strict private
    FColorTransform: Boolean;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TObject); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    function DecodeImageData(const AData: TBytes;
      AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData; override;

    property ColorTransform: Boolean read FColorTransform;
  end;

  { TdxPDFJPXDecodeFilter }

  TdxPDFJPXDecodeFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
    function DecodeImageData(const AData: TBytes;
      AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData; override;
  end;

  { TdxPDFASCII85DecodeFilter }

  TdxPDFASCII85DecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FGroup: TBytes;
    FGroupSize: Integer;
    FIndex: Integer;
    FMaxValue: Int64;
    FMultiplier1: Int64;
    FMultiplier2: Int64;
    FMultiplier3: Int64;
    FMultiplier4: Int64;
    function CheckEncodedData(AData: Byte): Boolean;
    function ConvertCurrentGroup: Int64;
    procedure DecodeGroup(AGroupSize: Integer; var AResult: TBytes);
    procedure ProcessFinalGroup(var AResult: TBytes);
    procedure ProcessGroup(AData: Byte; var AResult: TBytes);
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TObject); override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxPDFRunLengthDecodeFilter }

  TdxPDFRunLengthDecodeFilter = class(TdxPDFCustomStreamFilter)
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  TdxPDFCCITTFaxEncodingScheme = (fesTwoDimensional, fesOneDimensional, fesMixed);

  { TdxPDFCCITTFaxDecodeFilter }

  TdxPDFCCITTFaxDecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FEncodingScheme: TdxPDFCCITTFaxEncodingScheme;
    FTwoDimensionalLineCount: Integer;
    FEndOfLine: Boolean;
    FEncodedByteAlign: Boolean;
    FColumns: Integer;
    FRows: Integer;
    FEndOfBlock: Boolean;
    FBlackIs1: Boolean;
    FDamagedRowsBeforeError: Integer;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TObject); override;

    property EncodingScheme: TdxPDFCCITTFaxEncodingScheme read FEncodingScheme;
    property TwoDimensionalLineCount: Integer read FTwoDimensionalLineCount;
    property EndOfLine: Boolean read FEndOfLine;
    property EncodedByteAlign: Boolean read FEncodedByteAlign;
    property Columns: Integer read FColumns;
    property Rows: Integer read FRows;
    property EndOfBlock: Boolean read FEndOfBlock;
    property BlackIs1: Boolean read FBlackIs1;
    property DamagedRowsBeforeError: Integer read FDamagedRowsBeforeError;
  public
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

  { TdxJBIG2GlobalSegments }

  TdxJBIG2GlobalSegments = class(TdxPDFReferencedObject)
  strict private
    FData: TBytes;
  public
    constructor Create(const AData: TBytes);

    class function Parse(ARepository: TdxPDFCustomRepository; AValue: TdxPDFBase): TdxJBIG2GlobalSegments; static;

    property Data: TBytes read FData;
  end;

  { TdxPDFJBIG2DecodeFilter }

  TdxPDFJBIG2DecodeFilter = class(TdxPDFCustomStreamFilter)
  strict private
    FGlobalSegments: TdxJBIG2GlobalSegments;
  protected
    function DoDecode(const AData: TBytes): TBytes; override;
    procedure Initialize(ADecodeParameters: TObject); override;

    property GlobalSegments: TdxJBIG2GlobalSegments read FGlobalSegments;
  public
    destructor Destroy; override;
    class function GetName: string; override;
    class function GetShortName: string; override;
  end;

function dxPDFCreateStreamFilter(const AFilterName: string; ADecodeParameters: TObject): TdxPDFCustomStreamFilter;

implementation

uses
  Math, Classes, Character, ZLib, dxCore, dxGDIPlusClasses, dxGDIPlusAPI, dxJPX, dxPDFCore, dxPDFUtils;

type
  TdxPDFCustomStreamFilterClass = class of TdxPDFCustomStreamFilter;
  TdxPDFPngPrediction = (ppNone = 0, ppSub = 1, ppUp = 2, ppAverage = 3, ppPaeth = 4);

  { TdxPDFStreamFilterFactory }

  TdxPDFStreamFilterFactory = class(TdxPDFFactory<TdxPDFCustomStreamFilterClass>)
  public
    function CreateFilter(const AFilterName: string; ADecodeParameters: TObject): TdxPDFCustomStreamFilter;
    procedure RegisterFilter(AFilterClass: TdxPDFCustomStreamFilterClass);
    procedure UnregisterFilter(AFilterClass: TdxPDFCustomStreamFilterClass);
  end;

  { TdxLZWDecoder }

  TdxLZWDecoder = class
  strict private
    FClearTable: Integer;
    FCurrentDictionarySize: Integer;
    FCurrentDictionaryEntryLength: Integer;
    FCurrentDictionaryMaxEntryLength: Integer;
    FCurrentPosition: Integer;
    FCurrentSymbol: Byte;
    FCurrentSequenceLength: Integer;
    FData: TBytes;
    FDataSize: Integer;
    FDictionary: TDictionary<Integer, TBytes>;
    FEarlyChangeValue: Boolean;
    FEndOfData: Integer;
    FInitialSequenceLength: Integer;
    FRemainBits: Integer;
    FNextCode: Integer;

    function DoDecode: TBytes;
    procedure CalculateNextCode;
    procedure InitializeDictionaryParameters;
    procedure InitializeParameters(const AData: TBytes; AInitialSequenceLength: Integer);
    procedure PopulateDictionary(const ACurrentSequence, ANextSequence: TBytes);
    procedure PopulateNextSequence(const ACurrentSequence: TBytes; var ANextSequence: TBytes);
  public
    constructor Create;
    destructor Destroy; override;

    function Decode(const AData: TBytes; AInitialSequenceLength: Integer; AEarlyChangeValue: Boolean = False): TBytes; overload;
  end;

  { TdxPDFPngPredictor }

  TdxPDFPngPredictor = class
  strict private
    function AveragePredictor(AElement: Byte; APixelIndex, ARowIndex: Integer): Byte; inline;
    function PathPredictor(AElement: Byte; APixelIndex, ARowIndex: Integer): Byte;
    function SubPredictor(AElement: Byte; APixelIndex: Integer): Byte; inline;
    function UpPredictor(AElement: Byte; ARowIndex: Integer): Byte; inline;
  protected
    FActualData: TBytes;
    FBytesPerPixel: Integer;
    FData: TBytes;
    FPredictor: TdxPDFFilterPredictor;
    FPreviousPixels: TBytes;
    FPreviousRow: TBytes;
    FRowCount: Integer;
    FRowSize: Integer;
    FTopLeftPixels: TBytes;

    procedure Calculate;
    procedure CalculateActualElement(APrediction: TdxPDFPngPrediction; ARowIndex, APixelIndex: Integer; AElement: Byte;
      out AActualElement: Byte);
    procedure Initialize(const AData: TBytes; APredictor: TdxPDFFilterPredictor;
      AColors, AColumns, ABitsPerComponent: Integer);
    procedure Finalize;
    procedure PopulateData(AActualDataElement: Byte; ARowIndex: Integer; var AActualDataIndex, APixelIndex: Integer);
  public
    function Decode(const AData: TBytes; APredictor: TdxPDFFilterPredictor;
      AColors, AColumns, ABitsPerComponent: Integer): TBytes; virtual;
  end;

  { TdxPDFTIFFPrediction }

  TdxPDFTIFFPrediction = class(TdxPDFPngPredictor)
  public
    function Decode(const AData: TBytes; APredictor: TdxPDFFilterPredictor;
      AColors, AColumns, ABitsPerComponent: Integer): TBytes; override;
  end;

  { TdxPDFHuffmanTreeNode }

  TdxPDFHuffmanTreeNode = class(TObject);

  { TdxPDFHuffmanTreeLeaf }

  TdxPDFHuffmanTreeLeaf = class(TdxPDFHuffmanTreeNode)
  strict private
    FRunLength: Integer;
  public
    constructor Create(ARunLength: Integer);

    property RunLength: Integer read FRunLength;
  end;

  { TdxPDFHuffmanTreeBranch }

  TdxPDFHuffmanTreeBranch = class(TdxPDFHuffmanTreeNode)
  strict private
    FZero: TdxPDFHuffmanTreeNode;
    FOne: TdxPDFHuffmanTreeNode;
  public
    destructor Destroy; override;

    procedure Fill(const ASequence: string; ARunLength: Integer); overload;
    procedure Fill(ADictionary: TDictionary<string, Integer>); overload;

    property Zero: TdxPDFHuffmanTreeNode read FZero;
    property One: TdxPDFHuffmanTreeNode read FOne;
  end;

  TdxPDFCCITTFaxCodingMode = (fcmPass, fcmHorizontal, fcmVertical0, fcmVerticalRight1, fcmVerticalRight2,
    fcmVerticalRight3, fcmVerticalLeft1, fcmVerticalLeft2, fcmVerticalLeft3, fcmEndOfData);

  { TdxPDFCCITTFaxDecoder }

  TdxPDFCCITTFaxDecoder = class
  strict private
    FWhiteRunLengths: TDictionary<string, Integer>;
    FBlackRunLengths: TDictionary<string, Integer>;
    FCommonRunLengths: TDictionary<string, Integer>;
    FWhiteTree: TdxPDFHuffmanTreeBranch;
    FBlackTree: TdxPDFHuffmanTreeBranch;

    FData: TBytes;
    FLength: Integer;
    FBlackIs1: Boolean;
    FAlignEncodedBytes: Boolean;
    FTwoDimensionalLineCount: Integer;
    FColumns: Integer;
    FSize: Integer;
    FResult: TBytes;
    FLineSize: Integer;
    FReferenceLine: TBytes;
    FDecodingLine: TBytes;
    FCurrentPosition: Integer;
    FCurrentByte: Byte;
    FCurrentByteOffset: Integer;
    FIsBlack: Boolean;

    FA0: Integer;
    FA1: Integer;
    FB1: Integer;
    FB2: Integer;

    function IsCompleted: Boolean;
    function IsEOF: Boolean;
    function DecodeGroup3Line: Boolean;
    function DecodeGroup4: Integer;
    function FillByte(B: Byte; AStart, AEnd: Integer; ABlack: Boolean): Byte;
    function FillDecodingLine(AA0, AA1: Integer; ABlack: Boolean): Boolean;
    function Finish: Boolean;
    function FindRunningLengthPart(ABranch: TdxPDFHuffmanTreeBranch): Integer;
    function FindRunningLength(ABranch: TdxPDFHuffmanTreeBranch): Integer;
    function FindB(AStartPosition: Integer; AIsWhite: Boolean): Integer;
    function ReadBit: Boolean; inline;
    function ReadMode: TdxPDFCCITTFaxCodingMode; inline;
    procedure AccumulateResult;
    procedure DecodeGroup3;
    procedure DecodeGroup3TwoDimensional;
    procedure InitializeLengths;
    procedure InitializeBlackRunLengths;
    procedure InitializeCommonRunLengths;
    procedure InitializeWhiteRunLengths;
    procedure NextLine; inline;
    procedure MoveNextByte;
    procedure ReadEOL; inline;
  public
    constructor Create;
    destructor Destroy; override;

    function Decode(AFilter: TdxPDFCCITTFaxDecodeFilter; const AData: TBytes): TBytes;
  end;

var
  dxgPDFStreamFilterFactory: TdxPDFStreamFilterFactory;

function dxPDFStreamFilterFactory: TdxPDFStreamFilterFactory;
begin
  if dxgPDFStreamFilterFactory = nil then
    dxgPDFStreamFilterFactory := TdxPDFStreamFilterFactory.Create;
  Result := dxgPDFStreamFilterFactory;
end;

function dxPDFCreateStreamFilter(const AFilterName: string; ADecodeParameters: TObject): TdxPDFCustomStreamFilter;
begin
  Result := dxPDFStreamFilterFactory.CreateFilter(AFilterName, ADecodeParameters);
end;

{ TdxPDFStreamFilterFactory }

function TdxPDFStreamFilterFactory.CreateFilter(const AFilterName: string;
  ADecodeParameters: TObject): TdxPDFCustomStreamFilter;
var
  AFilterClass: TdxPDFCustomStreamFilterClass;
begin
  if TryGetClass(AFilterName, AFilterClass) then
    Result := AFilterClass.Create(ADecodeParameters)
  else
    Result := TdxPDFUnknownStreamFilter.Create(ADecodeParameters);
end;

procedure TdxPDFStreamFilterFactory.RegisterFilter(AFilterClass: TdxPDFCustomStreamFilterClass);
begin
  if not ContainsKey(AFilterClass.GetName) then
  begin
    Register(AFilterClass.GetName, AFilterClass);
    if AFilterClass.GetName <> AFilterClass.GetShortName then
      Register(AFilterClass.GetShortName, AFilterClass);
  end;
end;

procedure TdxPDFStreamFilterFactory.UnregisterFilter(AFilterClass: TdxPDFCustomStreamFilterClass);
begin
  UnregisterClass(AFilterClass.GetName);
  UnregisterClass(AFilterClass.GetShortName);
end;

{ TdxLZWDecoder }

constructor TdxLZWDecoder.Create;
begin
  inherited Create;
  FDictionary := TDictionary<Integer, TBytes>.Create;
end;

destructor TdxLZWDecoder.Destroy;
begin
  SetLength(FData, 0);
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxLZWDecoder.Decode(const AData: TBytes; AInitialSequenceLength: Integer; AEarlyChangeValue: Boolean = False): TBytes;
begin
  if (AInitialSequenceLength >= 3) and (AInitialSequenceLength <= 9) and (Length(AData) > 0) then
  begin
    FEarlyChangeValue := AEarlyChangeValue;
    InitializeParameters(AData, AInitialSequenceLength);
    Result := DoDecode;
  end
  else
    Result := AData;
end;

function TdxLZWDecoder.DoDecode: TBytes;
var
  ACurrentSequence, ANextSequence: TBytes;
begin
  SetLength(ACurrentSequence, 0);
  CalculateNextCode;
  while FNextCode <> FEndOfData do
  begin
    if FNextCode = FClearTable then
    begin
      InitializeDictionaryParameters;
      SetLength(ACurrentSequence, 0);
    end
    else
    begin
      PopulateNextSequence(ACurrentSequence, ANextSequence);
      TdxPDFUtils.AddData(ANextSequence, Result);
      PopulateDictionary(ACurrentSequence, ANextSequence);
      ACurrentSequence := ANextSequence;
    end;
    CalculateNextCode;
  end;
end;

procedure TdxLZWDecoder.CalculateNextCode;
var
  ABitsToRead: Integer;
begin
  FNextCode := 0;
  ABitsToRead := FCurrentDictionaryEntryLength - FRemainBits;
  while ABitsToRead > 0 do
  begin
    Inc(FNextCode, FCurrentSymbol shl ABitsToRead);
    Inc(FCurrentPosition);
    if FCurrentPosition >= FDataSize then
    begin
      FNextCode := FEndOfData;
      Exit;
    end;
    FCurrentSymbol := FData[FCurrentPosition];
    FRemainBits := 8;
    Dec(ABitsToRead, FRemainBits);
  end;
  FRemainBits := -ABitsToRead;
  Inc(FNextCode, FCurrentSymbol shr FRemainBits);
  FCurrentSymbol := Byte(FCurrentSymbol and (1 shl FRemainBits - 1));
end;

procedure TdxLZWDecoder.InitializeDictionaryParameters;
begin
  FCurrentDictionaryEntryLength := FInitialSequenceLength;
  FCurrentDictionaryMaxEntryLength := (1 shl FInitialSequenceLength) - 1;
  FCurrentDictionarySize := FEndOfData + 1;
  FDictionary.Clear;
end;

procedure TdxLZWDecoder.InitializeParameters(const AData: TBytes; AInitialSequenceLength: Integer);
begin
  TdxPDFUtils.AddData(AData, FData);
  FInitialSequenceLength := AInitialSequenceLength;
  FCurrentPosition := 0;
  FRemainBits := 8;
  FDataSize := Length(AData);
  FCurrentSymbol := FData[FCurrentPosition];
  FClearTable := 1 shl (AInitialSequenceLength - 1);
  FEndOfData := FClearTable + 1;
  InitializeDictionaryParameters;
end;

procedure TdxLZWDecoder.PopulateDictionary(const ACurrentSequence, ANextSequence: TBytes);

  procedure EnsureEntryLength;
  begin
    if (FCurrentDictionarySize = FCurrentDictionaryMaxEntryLength) and (FCurrentDictionaryEntryLength < 12) then
    begin
      Inc(FCurrentDictionaryEntryLength);
      FCurrentDictionaryMaxEntryLength := ((FCurrentDictionaryMaxEntryLength + 1) shl 1) - 1;
    end;
  end;

  procedure AddEntry(const ADictionaryEntry: TBytes);
  begin
    FDictionary.Add(FCurrentDictionarySize, ADictionaryEntry);
    Inc(FCurrentDictionarySize);
  end;

var
  ADictionaryEntry: TBytes;
begin
  if FCurrentSequenceLength > 0 then
  begin
    SetLength(ADictionaryEntry, FCurrentSequenceLength + 1);
    TdxPDFUtils.CopyData(ACurrentSequence, 0, ADictionaryEntry, 0, Length(ACurrentSequence));
    ADictionaryEntry[FCurrentSequenceLength] := ANextSequence[0];
    if FEarlyChangeValue then
    begin
      AddEntry(ADictionaryEntry);
      EnsureEntryLength;
    end
    else
    begin
      EnsureEntryLength;
      AddEntry(ADictionaryEntry);
    end;
  end;
end;

procedure TdxLZWDecoder.PopulateNextSequence(const ACurrentSequence: TBytes; var ANextSequence: TBytes);
begin
  FCurrentSequenceLength := Length(ACurrentSequence);
  SetLength(ANextSequence, 0);
  if FNextCode < FClearTable then
  begin
    SetLength(ANextSequence, 1);
    ANextSequence[0] := FNextCode;
  end
  else if FNextCode < FCurrentDictionarySize then
    ANextSequence := FDictionary[FNextCode]
  else
  begin
    SetLength(ANextSequence, FCurrentSequenceLength + 1);
    TdxPDFUtils.CopyData(ACurrentSequence, 0, ANextSequence, 0, Length(ACurrentSequence));
    ANextSequence[FCurrentSequenceLength] := ACurrentSequence[0];
  end;
end;

{ TdxPDFPngPredictor }

function TdxPDFPngPredictor.Decode(const AData: TBytes; APredictor: TdxPDFFilterPredictor; AColors, AColumns: Integer;
  ABitsPerComponent: Integer): TBytes;
begin
  Initialize(AData, APredictor, AColors, AColumns, ABitsPerComponent);
  Calculate;
  Result := FActualData;
  Finalize;
end;

function TdxPDFPngPredictor.AveragePredictor(AElement: Byte; APixelIndex, ARowIndex: Integer): Byte;
begin
  Result := (FPreviousPixels[APixelIndex] + FPreviousRow[ARowIndex]) div 2 + AElement;
end;

function TdxPDFPngPredictor.PathPredictor(AElement: Byte; APixelIndex, ARowIndex: Integer): Byte;
var
  ALeft, ATop, ATopLeft: Byte;
  APrediction, ADistanceToLeft, ADistanceToTop, ADistanceToTopLeft: Integer;
begin
  ALeft := FPreviousPixels[APixelIndex];
  ATop := FPreviousRow[ARowIndex];
  ATopLeft := FTopLeftPixels[APixelIndex];
  APrediction := ALeft + ATop - ATopLeft;
  ADistanceToLeft := Abs(APrediction - ALeft);
  ADistanceToTop := Abs(APrediction - ATop);
  ADistanceToTopLeft := Abs(APrediction - ATopLeft);
  if ADistanceToLeft <= ADistanceToTop then
    if ADistanceToLeft <= ADistanceToTopLeft then
      Result := ALeft
    else
      Result := ATopLeft
  else
    if ADistanceToTop <= ADistanceToTopLeft then
      Result := ATop
    else
      Result := ATopLeft;
  Result := Result + AElement;
end;

function TdxPDFPngPredictor.SubPredictor(AElement: Byte; APixelIndex: Integer): Byte;
begin
  Result := FPreviousPixels[APixelIndex] + AElement;
end;

function TdxPDFPngPredictor.UpPredictor(AElement: Byte; ARowIndex: Integer): Byte;
begin
  Result := FPreviousRow[ARowIndex] + AElement;
end;

procedure TdxPDFPngPredictor.Calculate;
var
  AActualDataElement: Byte;
  ARow, ADataIndex, AActualDataIndex, ARowIndex, APixelIndex: Integer;
  APrediction: TdxPDFPngPrediction;
begin
  ADataIndex := 0;
  AActualDataIndex := 0;
  for ARow := 0 to FRowCount - 1 do
  begin
    TdxPDFUtils.ClearData(FPreviousPixels);
    TdxPDFUtils.ClearData(FTopLeftPixels);
    APrediction := TdxPDFPngPrediction(FData[ADataIndex]);
    Inc(ADataIndex);
    if ADataIndex < Length(FData) then
    begin
      APixelIndex := 0;
      for ARowIndex := 0 to FRowSize - 1 do
      begin
        if FPredictor = fpTiffPredictor then
        begin
          SetLength(FActualData, 1);
          FActualData[0] := 1;
          Break;
        end
        else
        begin
          CalculateActualElement(APrediction, ARowIndex, APixelIndex, FData[ADataIndex], AActualDataElement);
          PopulateData(AActualDataElement, ARowIndex, AActualDataIndex, APixelIndex);
          Inc(ADataIndex);
        end;
        if ADataIndex >= Length(FData) then
          Break;
      end;
    end
    else
      Break;
  end;
end;

procedure TdxPDFPngPredictor.CalculateActualElement(APrediction: TdxPDFPngPrediction; ARowIndex, APixelIndex: Integer;
  AElement: Byte; out AActualElement: Byte);
begin
  AActualElement := AElement;
  case FPredictor of
    fpPngUpPrediction:
      if APrediction = ppUp then
        AActualElement := UpPredictor(AElement, ARowIndex);
    fpPngNonePrediction, fpPngOptimumPrediction, fpPngPaethPrediction:
      case APrediction of
        ppSub:
          AActualElement := SubPredictor(AElement, APixelIndex);
        ppUp:
          AActualElement := UpPredictor(AElement, ARowIndex);
        ppAverage:
          AActualElement := AveragePredictor(AElement, APixelIndex, ARowIndex);
        ppPaeth:
          AActualElement := PathPredictor(AElement, APixelIndex, ARowIndex);
      end;
  end;
end;

procedure TdxPDFPngPredictor.Initialize(const AData: TBytes; APredictor: TdxPDFFilterPredictor;
  AColors, AColumns, ABitsPerComponent: Integer);
var
  ABitsFactor, ARowComponents: Integer;
begin
  FData := AData;
  FPredictor := APredictor;
  FBytesPerPixel := Max(ABitsPerComponent * AColors div 8, 1);

  if ABitsPerComponent = 16 then
    FRowSize := AColumns * AColors * 2
  else
  begin
    ABitsFactor := 8 div ABitsPerComponent;
    ARowComponents := AColumns * AColors;
    FRowSize := ARowComponents div ABitsFactor;
    if ARowComponents mod ABitsFactor <> 0 then
      Inc(FRowSize);
  end;
  FRowCount := Length(FData) div (FRowSize + 1);
  if Length(FData) mod (FRowSize + 1) <> 0 then
    Inc(FRowCount);

  SetLength(FActualData, FRowSize * FRowCount);
  SetLength(FPreviousRow, FRowSize);
  SetLength(FPreviousPixels, FBytesPerPixel);
  SetLength(FTopLeftPixels, FBytesPerPixel);
end;

procedure TdxPDFPngPredictor.Finalize;
begin
  SetLength(FTopLeftPixels, 0);
  SetLength(FPreviousPixels, 0);
  SetLength(FPreviousRow, 0);
  SetLength(FActualData, 0);
end;

procedure TdxPDFPngPredictor.PopulateData(AActualDataElement: Byte; ARowIndex: Integer;
  var AActualDataIndex, APixelIndex: Integer);
begin
  FTopLeftPixels[APixelIndex] := FPreviousRow[ARowIndex];
  FPreviousRow[ARowIndex] := AActualDataElement;
  FActualData[AActualDataIndex] := AActualDataElement;
  FPreviousPixels[APixelIndex] := AActualDataElement;
  Inc(AActualDataIndex);
  Inc(APixelIndex);
  if APixelIndex = FBytesPerPixel then
    APixelIndex := 0;
end;

{ TdxPDFTIFFPrediction }

function TdxPDFTIFFPrediction.Decode(const AData: TBytes; APredictor: TdxPDFFilterPredictor; AColors, AColumns,
  ABitsPerComponent: Integer): TBytes;
var
  APreviousPixel: TBytes;
  ADataElement, AActualDataElement: Byte;
  ADataLength, ARow, ASourceIndex, AResultIndex, AComponent, APixelIndex: Integer;
begin
  Initialize(AData, APredictor, AColors, AColumns, ABitsPerComponent);
  ADataLength := Length(AData);
  SetLength(Result, ADataLength);
  SetLength(APreviousPixel, FBytesPerPixel);
  ASourceIndex := 0;
  AResultIndex := 0;
  for ARow := 0 to FRowCount - 1 do
  begin
    TdxPDFUtils.ClearData(APreviousPixel);
    APixelIndex := 0;
    for AComponent := 0 to FRowSize - 1 do
    begin
      ADataElement := AData[ASourceIndex];
      Inc(ASourceIndex);
      AActualDataElement := Byte((APreviousPixel[APixelIndex] + ADataElement));
      Result[AResultIndex] := AActualDataElement;
      Inc(AResultIndex);
      APreviousPixel[APixelIndex] := AActualDataElement;
      Inc(APixelIndex);
      if APixelIndex = FBytesPerPixel then
        APixelIndex := 0;
      if ASourceIndex >= ADataLength then
        Exit;
    end;
  end;
end;

{ TdxPDFHuffmanTreeLeaf }

constructor TdxPDFHuffmanTreeLeaf.Create(ARunLength: Integer);
begin
  inherited Create;
  FRunLength := ARunLength;
end;

{ TdxPDFHuffmanTreeBranch }

destructor TdxPDFHuffmanTreeBranch.Destroy;
begin
  FreeAndNil(FOne);
  FreeAndNil(FZero);
  inherited Destroy;
end;

procedure TdxPDFHuffmanTreeBranch.Fill(const ASequence: string; ARunLength: Integer);
var
  AIsOne: Boolean;
  ANewSequence: string;
  ABranch: TdxPDFHuffmanTreeBranch;
begin
  ABranch := nil;
  AIsOne := False;
  if ASequence = '' then
    TdxPDFUtils.Abort;
  case ASequence[1] of
    '0':
      AIsOne := False;
    '1':
      AIsOne := True;
    else
      TdxPDFUtils.Abort;
  end;
  ANewSequence := Copy(ASequence, 2, MaxInt);

  if ANewSequence = '' then
    if AIsOne then
    begin
      if FOne <> nil then
        TdxPDFUtils.Abort;
      FOne := TdxPDFHuffmanTreeLeaf.Create(ARunLength);
    end
    else
    begin
      if FZero <> nil then
        TdxPDFUtils.Abort;
      FZero := TdxPDFHuffmanTreeLeaf.Create(ARunLength);
    end
  else
  begin
    if AIsOne then
      if FOne = nil then
      begin
        ABranch := TdxPDFHuffmanTreeBranch.Create;
        FOne := ABranch;
      end
      else
      begin
        ABranch := FOne as TdxPDFHuffmanTreeBranch;
        if ABranch = nil then
          TdxPDFUtils.Abort;
      end
    else
      if FZero = nil then
      begin
        if ABRanch <> nil then
          ABranch.Free;
        ABranch := TdxPDFHuffmanTreeBranch.Create;
        FZero := ABranch;
      end
      else
      begin
        ABranch := FZero as TdxPDFHuffmanTreeBranch;
        if ABranch = nil then
          TdxPDFUtils.Abort;
      end;
    ABranch.Fill(ANewSequence, ARunLength);
  end;
end;

procedure TdxPDFHuffmanTreeBranch.Fill(ADictionary: TDictionary<string, Integer>);
var
  APair: TPair<string, Integer>;
begin
  for APair in ADictionary do
    Fill(APair.Key, APair.Value);
end;

{ TdxPDFCCITTFaxDecoder }

constructor TdxPDFCCITTFaxDecoder.Create;
begin
  inherited Create;
  FCurrentByteOffset := 7;
end;

destructor TdxPDFCCITTFaxDecoder.Destroy;
begin
  SetLength(FResult, 0);
  FreeAndNil(FBlackTree);
  FreeAndNil(FWhiteTree);
  FreeAndNil(FCommonRunLengths);
  FreeAndNil(FBlackRunLengths);
  FreeAndNil(FWhiteRunLengths);
  inherited Destroy;
end;

function TdxPDFCCITTFaxDecoder.Decode(AFilter: TdxPDFCCITTFaxDecodeFilter; const AData: TBytes): TBytes;
begin
  FData := AData;
  FLength := Length(AData);
  FBlackIs1 := AFilter.BlackIs1;
  FAlignEncodedBytes := AFilter.EncodedByteAlign;
  FTwoDimensionalLineCount := AFilter.TwoDimensionalLineCount;
  FColumns := AFilter.Columns;

  FSize := FColumns div 8;
  if FColumns mod 8 <> 0 then
    Inc(FSize);
  FSize := FSize * AFilter.Rows;

  SetLength(FResult, 0);

  FLineSize := FColumns div 8;
  if FColumns mod 8 > 0 then
    Inc(FLineSize);
  SetLength(FReferenceLine, FLineSize);
  SetLength(FDecodingLine, FLineSize);
  if FLength > 0 then
    FCurrentByte := AData[0];
  FB1 := FColumns;
  FB2 := FColumns;

  InitializeLengths;

  FWhiteTree := TdxPDFHuffmanTreeBranch.Create;
  FWhiteTree.Fill(FWhiteRunLengths);
	FWhiteTree.Fill(FCommonRunLengths);

  FBlackTree := TdxPDFHuffmanTreeBranch.Create;
	FBlackTree.Fill(FBlackRunLengths);
	FBlackTree.Fill(FCommonRunLengths);

  if FLength > 0 then
    case AFilter.EncodingScheme of
      fesTwoDimensional:
        DecodeGroup4;
      fesOneDimensional:
        DecodeGroup3;
      else
        DecodeGroup3TwoDimensional;
    end;
  SetLength(Result, Length(FResult));
  TdxPDFUtils.CopyData(FResult, 0, Result, 0, Length(FResult));
  FResult := TdxByteArray.Resize(FResult, 0);
end;

function TdxPDFCCITTFaxDecoder.IsEOF: Boolean;
begin
  Result := FCurrentPosition >= FLength;
end;

function TdxPDFCCITTFaxDecoder.FillByte(B: Byte; AStart, AEnd: Integer; ABlack: Boolean): Byte;
var
  AMask: Byte;
begin
  AMask := ($FF shr AStart) and ($FF shl (8 - AEnd));
  if ABlack then
    Result := B and ($FF xor AMask)
  else
    Result := B or AMask;
end;

function TdxPDFCCITTFaxDecoder.FillDecodingLine(AA0, AA1: Integer; ABlack: Boolean): Boolean;
var
  AStartByteIndex, AEndByteIndex, I: Integer;
begin
  Result := True;
  if (AA0 <> 0) or (AA1 <> 0) then
  begin
    if (AA1 <= AA0) or (AA1 > FColumns) then
      Exit(False);
    AStartByteIndex := AA0 div 8;
    AEndByteIndex := AA1 div 8;
    if AStartByteIndex = AEndByteIndex then
      FDecodingLine[AStartByteIndex] := FillByte(FDecodingLine[AStartByteIndex], AA0 mod 8, AA1 mod 8, ABlack)
    else
    begin
      FDecodingLine[AStartByteIndex] := FillByte(FDecodingLine[AStartByteIndex], AA0 mod 8, 8, ABlack);
      for I := AStartByteIndex + 1 to AEndByteIndex - 1 do
        if ABlack then
          FDecodingLine[I] := 0
        else
          FDecodingLine[I] := $FF;
      if AEndByteIndex < FLineSize then
        FDecodingLine[AEndByteIndex] := FillByte(FDecodingLine[AEndByteIndex], 0, AA1 mod 8, ABlack);
    end;
  end;
end;

function TdxPDFCCITTFaxDecoder.Finish: Boolean;
begin
  AccumulateResult;
  FA0 := 0;
  FIsBlack := False;
  Result := not IsCompleted;
end;

function TdxPDFCCITTFaxDecoder.IsCompleted: Boolean;
begin
  if FSize = 0 then
    Result := IsEOF
  else
    Result := Length(FResult) >= FSize;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeLengths;
begin
  InitializeWhiteRunLengths;
  InitializeBlackRunLengths;
  InitializeCommonRunLengths;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeBlackRunLengths;
begin
  FBlackRunLengths := TDictionary<string, Integer>.Create;
  FBlackRunLengths.Add('0000110111', 0);
  FBlackRunLengths.Add('010', 1);
  FBlackRunLengths.Add('11', 2);
  FBlackRunLengths.Add('10', 3);
  FBlackRunLengths.Add('011', 4);
  FBlackRunLengths.Add('0011', 5);
  FBlackRunLengths.Add('0010', 6);
  FBlackRunLengths.Add('00011', 7);
  FBlackRunLengths.Add('000101', 8);
  FBlackRunLengths.Add('000100', 9);
  FBlackRunLengths.Add('0000100', 10);
  FBlackRunLengths.Add('0000101', 11);
  FBlackRunLengths.Add('0000111', 12);
  FBlackRunLengths.Add('00000100', 13);
  FBlackRunLengths.Add('00000111', 14);
  FBlackRunLengths.Add('000011000', 15);
  FBlackRunLengths.Add('0000010111', 16);
  FBlackRunLengths.Add('0000011000', 17);
  FBlackRunLengths.Add('0000001000', 18);
  FBlackRunLengths.Add('00001100111', 19);
  FBlackRunLengths.Add('00001101000', 20);
  FBlackRunLengths.Add('00001101100', 21);
  FBlackRunLengths.Add('00000110111', 22);
  FBlackRunLengths.Add('00000101000', 23);
  FBlackRunLengths.Add('00000010111', 24);
  FBlackRunLengths.Add('00000011000', 25);
  FBlackRunLengths.Add('000011001010', 26);
  FBlackRunLengths.Add('000011001011', 27);
  FBlackRunLengths.Add('000011001100', 28);
  FBlackRunLengths.Add('000011001101', 29);
  FBlackRunLengths.Add('000001101000', 30);
  FBlackRunLengths.Add('000001101001', 31);
  FBlackRunLengths.Add('000001101010', 32);
  FBlackRunLengths.Add('000001101011', 33);
  FBlackRunLengths.Add('000011010010', 34);
  FBlackRunLengths.Add('000011010011', 35);
  FBlackRunLengths.Add('000011010100', 36);
  FBlackRunLengths.Add('000011010101', 37);
  FBlackRunLengths.Add('000011010110', 38);
  FBlackRunLengths.Add('000011010111', 39);
  FBlackRunLengths.Add('000001101100', 40);
  FBlackRunLengths.Add('000001101101', 41);
  FBlackRunLengths.Add('000011011010', 42);
  FBlackRunLengths.Add('000011011011', 43);
  FBlackRunLengths.Add('000001010100', 44);
  FBlackRunLengths.Add('000001010101', 45);
  FBlackRunLengths.Add('000001010110', 46);
  FBlackRunLengths.Add('000001010111', 47);
  FBlackRunLengths.Add('000001100100', 48);
  FBlackRunLengths.Add('000001100101', 49);
  FBlackRunLengths.Add('000001010010', 50);
  FBlackRunLengths.Add('000001010011', 51);
  FBlackRunLengths.Add('000000100100', 52);
  FBlackRunLengths.Add('000000110111', 53);
  FBlackRunLengths.Add('000000111000', 54);
  FBlackRunLengths.Add('000000100111', 55);
  FBlackRunLengths.Add('000000101000', 56);
  FBlackRunLengths.Add('000001011000', 57);
  FBlackRunLengths.Add('000001011001', 58);
  FBlackRunLengths.Add('000000101011', 59);
  FBlackRunLengths.Add('000000101100', 60);
  FBlackRunLengths.Add('000001011010', 61);
  FBlackRunLengths.Add('000001100110', 62);
  FBlackRunLengths.Add('000001100111', 63);
  FBlackRunLengths.Add('0000001111', 64);
  FBlackRunLengths.Add('000011001000', 128);
  FBlackRunLengths.Add('000011001001', 192);
  FBlackRunLengths.Add('000001011011', 256);
  FBlackRunLengths.Add('000000110011', 320);
  FBlackRunLengths.Add('000000110100', 384);
  FBlackRunLengths.Add('000000110101', 448);
  FBlackRunLengths.Add('0000001101100', 512);
  FBlackRunLengths.Add('0000001101101', 576);
  FBlackRunLengths.Add('0000001001010', 640);
  FBlackRunLengths.Add('0000001001011', 704);
  FBlackRunLengths.Add('0000001001100', 768);
  FBlackRunLengths.Add('0000001001101', 832);
  FBlackRunLengths.Add('0000001110010', 896);
  FBlackRunLengths.Add('0000001110011', 960);
  FBlackRunLengths.Add('0000001110100', 1024);
  FBlackRunLengths.Add('0000001110101', 1088);
  FBlackRunLengths.Add('0000001110110', 1152);
  FBlackRunLengths.Add('0000001110111', 1216);
  FBlackRunLengths.Add('0000001010010', 1280);
  FBlackRunLengths.Add('0000001010011', 1344);
  FBlackRunLengths.Add('0000001010100', 1408);
  FBlackRunLengths.Add('0000001010101', 1472);
  FBlackRunLengths.Add('0000001011010', 1536);
  FBlackRunLengths.Add('0000001011011', 1600);
  FBlackRunLengths.Add('0000001100100', 1664);
  FBlackRunLengths.Add('0000001100101', 1728);
  FBlackRunLengths.Add('000000000001', -1);
  FBlackRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeCommonRunLengths;
begin
  FCommonRunLengths := TDictionary<string, Integer>.Create;
  FCommonRunLengths.Add('00000001000', 1792);
  FCommonRunLengths.Add('00000001100', 1856);
  FCommonRunLengths.Add('00000001101', 1920);
  FCommonRunLengths.Add('000000010010', 1984);
  FCommonRunLengths.Add('000000010011', 2048);
  FCommonRunLengths.Add('000000010100', 2112);
  FCommonRunLengths.Add('000000010101', 2176);
  FCommonRunLengths.Add('000000010110', 2240);
  FCommonRunLengths.Add('000000010111', 2304);
  FCommonRunLengths.Add('000000011100', 2368);
  FCommonRunLengths.Add('000000011101', 2432);
  FCommonRunLengths.Add('000000011110', 2496);
  FCommonRunLengths.Add('000000011111', 2560);
  FCommonRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.InitializeWhiteRunLengths;
begin
  FWhiteRunLengths := TDictionary<string, Integer>.Create;
  FWhiteRunLengths.Add('00110101', 0);
  FWhiteRunLengths.Add('000111', 1);
  FWhiteRunLengths.Add('0111', 2);
  FWhiteRunLengths.Add('1000', 3);
  FWhiteRunLengths.Add('1011', 4);
  FWhiteRunLengths.Add('1100', 5);
  FWhiteRunLengths.Add('1110', 6);
  FWhiteRunLengths.Add('1111', 7);
  FWhiteRunLengths.Add('10011', 8);
  FWhiteRunLengths.Add('10100', 9);
  FWhiteRunLengths.Add('00111', 10);
  FWhiteRunLengths.Add('01000', 11);
  FWhiteRunLengths.Add('001000', 12);
  FWhiteRunLengths.Add('000011', 13);
  FWhiteRunLengths.Add('110100', 14);
  FWhiteRunLengths.Add('110101', 15);
  FWhiteRunLengths.Add('101010', 16);
  FWhiteRunLengths.Add('101011', 17);
  FWhiteRunLengths.Add('0100111', 18);
  FWhiteRunLengths.Add('0001100', 19);
  FWhiteRunLengths.Add('0001000', 20);
  FWhiteRunLengths.Add('0010111', 21);
  FWhiteRunLengths.Add('0000011', 22);
  FWhiteRunLengths.Add('0000100', 23);
  FWhiteRunLengths.Add('0101000', 24);
  FWhiteRunLengths.Add('0101011', 25);
  FWhiteRunLengths.Add('0010011', 26);
  FWhiteRunLengths.Add('0100100', 27);
  FWhiteRunLengths.Add('0011000', 28);
  FWhiteRunLengths.Add('00000010', 29);
  FWhiteRunLengths.Add('00000011', 30);
  FWhiteRunLengths.Add('00011010', 31);
  FWhiteRunLengths.Add('00011011', 32);
  FWhiteRunLengths.Add('00010010', 33);
  FWhiteRunLengths.Add('00010011', 34);
  FWhiteRunLengths.Add('00010100', 35);
  FWhiteRunLengths.Add('00010101', 36);
  FWhiteRunLengths.Add('00010110', 37);
  FWhiteRunLengths.Add('00010111', 38);
  FWhiteRunLengths.Add('00101000', 39);
  FWhiteRunLengths.Add('00101001', 40);
  FWhiteRunLengths.Add('00101010', 41);
  FWhiteRunLengths.Add('00101011', 42);
  FWhiteRunLengths.Add('00101100', 43);
  FWhiteRunLengths.Add('00101101', 44);
  FWhiteRunLengths.Add('00000100', 45);
  FWhiteRunLengths.Add('00000101', 46);
  FWhiteRunLengths.Add('00001010', 47);
  FWhiteRunLengths.Add('00001011', 48);
  FWhiteRunLengths.Add('01010010', 49);
  FWhiteRunLengths.Add('01010011', 50);
  FWhiteRunLengths.Add('01010100', 51);
  FWhiteRunLengths.Add('01010101', 52);
  FWhiteRunLengths.Add('00100100', 53);
  FWhiteRunLengths.Add('00100101', 54);
  FWhiteRunLengths.Add('01011000', 55);
  FWhiteRunLengths.Add('01011001', 56);
  FWhiteRunLengths.Add('01011010', 57);
  FWhiteRunLengths.Add('01011011', 58);
  FWhiteRunLengths.Add('01001010', 59);
  FWhiteRunLengths.Add('01001011', 60);
  FWhiteRunLengths.Add('00110010', 61);
  FWhiteRunLengths.Add('00110011', 62);
  FWhiteRunLengths.Add('00110100', 63);
  FWhiteRunLengths.Add('11011', 64);
  FWhiteRunLengths.Add('10010', 128);
  FWhiteRunLengths.Add('010111', 192);
  FWhiteRunLengths.Add('0110111', 256);
  FWhiteRunLengths.Add('00110110', 320);
  FWhiteRunLengths.Add('00110111', 384);
  FWhiteRunLengths.Add('01100100', 448);
  FWhiteRunLengths.Add('01100101', 512);
  FWhiteRunLengths.Add('01101000', 576);
  FWhiteRunLengths.Add('01100111', 640);
  FWhiteRunLengths.Add('011001100', 704);
  FWhiteRunLengths.Add('011001101', 768);
  FWhiteRunLengths.Add('011010010', 832);
  FWhiteRunLengths.Add('011010011', 896);
  FWhiteRunLengths.Add('011010100', 960);
  FWhiteRunLengths.Add('011010101', 1024);
  FWhiteRunLengths.Add('011010110', 1088);
  FWhiteRunLengths.Add('011010111', 1152);
  FWhiteRunLengths.Add('011011000', 1216);
  FWhiteRunLengths.Add('011011001', 1280);
  FWhiteRunLengths.Add('011011010', 1344);
  FWhiteRunLengths.Add('011011011', 1408);
  FWhiteRunLengths.Add('010011000', 1472);
  FWhiteRunLengths.Add('010011001', 1536);
  FWhiteRunLengths.Add('010011010', 1600);
  FWhiteRunLengths.Add('011000', 1664);
  FWhiteRunLengths.Add('010011011', 1728);
  FWhiteRunLengths.Add('000000000001', -1);
  FWhiteRunLengths.TrimExcess;
end;

procedure TdxPDFCCITTFaxDecoder.MoveNextByte;
begin
  Inc(FCurrentPosition);
  if FCurrentPosition < FLength then
    FCurrentByte := FData[FCurrentPosition];
  FCurrentByteOffset := 7;
end;

function TdxPDFCCITTFaxDecoder.ReadBit: Boolean;
begin
  Result := IsEOF;
  if not Result then
  begin
    Result := ((FCurrentByte shr FCurrentByteOffset) and 1) = 1;
    Dec(FCurrentByteOffset);
    if FCurrentByteOffset < 0 then
      MoveNextByte;
  end;
end;

function TdxPDFCCITTFaxDecoder.ReadMode: TdxPDFCCITTFaxCodingMode;
var
  AWordLength: Integer;
begin
  AWordLength := 1;
  while not ReadBit do
  begin
    Inc(AWordLength);
    if IsEOF then
      Exit(fcmEndOfData);
  end;
  case AWordLength of
    1:
      Result := fcmVertical0;
    2:
      if ReadBit then
        Result := fcmVerticalRight1
      else
        Result := fcmVerticalLeft1;
    3:
      Result := fcmHorizontal;
    4:
      Result := fcmPass;
    5:
      if ReadBit then
        Result := fcmVerticalRight2
      else
        Result := fcmVerticalLeft2;
    6:
      if ReadBit then
        Result := fcmVerticalRight3
      else
        Result := fcmVerticalLeft3;
    else
      Result := fcmEndOfData;
  end;
end;

function TdxPDFCCITTFaxDecoder.FindRunningLengthPart(ABranch: TdxPDFHuffmanTreeBranch): Integer;
var
  I, J: Integer;
  ANextBit: Boolean;
  ANextNode: TdxPDFHuffmanTreeNode;
begin
  Result := -1;
  I := 0;
  try
    while FCurrentPosition < FLength do
    begin
      ANextBit := ReadBit;
      if ANextBit then
        ANextNode := ABranch.One
      else
        ANextNode := ABranch.Zero;
      if ANextNode = nil then
      begin
        if ANextBit then
          Exit(-1);
        for J := I to 9 do
          if ReadBit then
            Exit(-1);
        while not ReadBit do;
        Exit(-1);
      end;
      if ANextNode is TdxPDFHuffmanTreeLeaf then
        Exit(TdxPDFHuffmanTreeLeaf(ANextNode).RunLength)
      else
        if not (ANextNode is TdxPDFHuffmanTreeBranch) then
          Exit(-1)
        else
          ABranch := TdxPDFHuffmanTreeBranch(ANextNode);
      Inc(I);
    end;
  except
    Exit(-1);
  end;
end;

function TdxPDFCCITTFaxDecoder.FindRunningLength(ABranch: TdxPDFHuffmanTreeBranch): Integer;
var
  ACode, AResult: Integer;
begin
  ACode := FindRunningLengthPart(ABranch);
  if ACode < 64 then
    Exit(ACode);
  AResult := ACode;
  while ACode = 2560 do
  begin
    ACode := FindRunningLengthPart(ABranch);
    Inc(AResult, ACode);
  end;
  if ACode >= 64 then
  begin
    ACode := FindRunningLengthPart(ABranch);
    Inc(AResult, ACode);
  end;
  Result := AResult;
end;

function TdxPDFCCITTFaxDecoder.FindB(AStartPosition: Integer; AIsWhite: Boolean): Integer;
var
  AResult, ABytePosition, AByteOffset, I: Integer;
  B: Byte;
begin
  if AStartPosition = FColumns then
    Exit(FColumns);
  AResult := AStartPosition;
  ABytePosition := AStartPosition div 8;
  AByteOffset := AStartPosition mod 8;
  B := FReferenceLine[ABytePosition] shl AByteOffset;
  for I := 0 to FColumns - 1 do
  begin
    if ((B and $80) = $80) = AIsWhite then
      Exit(AResult);
    Inc(AByteOffset);
    if AByteOffset = 8 then
    begin
      Inc(ABytePosition);
      if ABytePosition = FLineSize then
        Exit(FColumns);
      B := FReferenceLine[ABytePosition];
      AByteOffset := 0;
    end
    else
      B := B shl 1;
    Inc(AResult);
  end;
  Result := FColumns;
end;

procedure TdxPDFCCITTFaxDecoder.NextLine;
var
  ATemp: TBytes;
begin
  ATemp := FReferenceLine;
  FReferenceLine := FDecodingLine;
  FDecodingLine := ATemp;
  FIsBlack := False;
  FA0 := 0;
  FB1 := FindB(0, False);
  FB2 := FindB(FB1, True);
end;

procedure TdxPDFCCITTFaxDecoder.ReadEOL;
var
  AWordLength: Integer;
begin
  AWordLength := 1;
  while not ReadBit do
    Inc(AWordLength);
  if AWordLength < 12 then
    TdxPDFUtils.Abort;
  NextLine;
end;

procedure TdxPDFCCITTFaxDecoder.AccumulateResult;
var
  I: Integer;
begin
  if FBlackIs1 then
  begin
    for I := 0 to FLineSize - 1 do
      FReferenceLine[I] := FDecodingLine[I] xor $FF;
    TdxPDFUtils.AddData(FReferenceLine, FResult);
  end
  else
    TdxPDFUtils.AddData(FDecodingLine, FResult);
  if FAlignEncodedBytes and (FCurrentByteOffset < 7) then
    MoveNextByte;
end;

function TdxPDFCCITTFaxDecoder.DecodeGroup3Line: Boolean;
var
  ATree: TdxPDFHuffmanTreeBranch;
  ARunningLength, I: Integer;
begin
  while True do
  begin
    if FIsBlack then
      ATree := FBlackTree
    else
      ATree := FWhiteTree;
    ARunningLength := FindRunningLength(ATree);
    if ARunningLength < 0 then
    begin
      ARunningLength := FindRunningLength(ATree);
      if ARunningLength < 0 then
      begin
        for I := 0 to 4 - 1 do
          if FindRunningLength(ATree) > 0 then
            TdxPDFUtils.Abort;
        Exit(False);
      end;
    end;
    FA1 := FA0 + ARunningLength;
    Result := FillDecodingLine(FA0, FA1, FIsBlack);
    FA0 := FA1;
    FIsBlack := not FIsBlack;
    if not Result then
      Exit(Finish)
    else
      if FA0 = FColumns then
        Exit(Finish);
  end;
end;

function TdxPDFCCITTFaxDecoder.DecodeGroup4: Integer;
var
  AA2, ALinesCount, AStartingLength, ATerminatingLength: Integer;
  AMode: TdxPDFCCITTFaxCodingMode;
  AStartingTree, ATerminatingTree: TdxPDFHuffmanTreeBranch;
begin
  ALinesCount := 0;

  while True do
  begin
    AMode := ReadMode;
    case AMode of
      fcmEndOfData:
         if FA0 <> 0 then
           TdxPDFUtils.Abort
         else
           Exit(ALinesCount);
      fcmPass:
        begin
          if not FillDecodingLine(FA0, FB2, FIsBlack) then
            TdxPDFUtils.Abort;
          FIsBlack := not FIsBlack;
          FA0 := FB2;
        end;
      fcmHorizontal:
        begin
          if FIsBlack then
          begin
            AStartingTree := FBlackTree;
            ATerminatingTree := FWhiteTree;
          end
          else
          begin
            AStartingTree := FWhiteTree;
            ATerminatingTree := FBlackTree;
          end;
          AStartingLength := FindRunningLength(AStartingTree);
          if AStartingLength < 0 then
            TdxPDFUtils.Abort;
          ATerminatingLength := FindRunningLength(ATerminatingTree);
          if ATerminatingLength < 0 then
            TdxPDFUtils.Abort;
          FA1 := FA0 + AStartingLength;
          AA2 := FA1 + ATerminatingLength;
          if AStartingLength > 0 then
            if not FillDecodingLine(FA0, FA1, FIsBlack) then
              TdxPDFUtils.Abort;
          if ATerminatingLength > 0 then
            if not FillDecodingLine(FA1, AA2, not FIsBlack) then
              TdxPDFUtils.Abort;
          FIsBlack := not FIsBlack;
          FA0 := AA2;
        end;
      else
        case AMode of
          fcmVertical0:
            FA1 := FB1;
          fcmVerticalRight1:
            FA1 := FB1 + 1;
          fcmVerticalRight2:
            FA1 := FB1 + 2;
          fcmVerticalRight3:
            FA1 := FB1 + 3;
          fcmVerticalLeft1:
            FA1 := FB1 - 1;
          fcmVerticalLeft2:
            FA1 := FB1 - 2;
          fcmVerticalLeft3:
            FA1 := FB1 - 3;
        end;
        if not FillDecodingLine(FA0, FA1, FIsBlack) then
          TdxPDFUtils.Abort;
        FA0 := FA1;
    end;
    if FA0 = FColumns then
    begin
      AccumulateResult;
      Inc(ALinesCount);
      if IsCompleted and (FCurrentPosition >= FLength - 1) then
        Exit(ALinesCount);
      NextLine;
    end
    else
    begin
      FIsBlack := not FIsBlack;
      FB1 := FindB(FindB(FA0, not FIsBlack), FIsBlack);
      FB2 := FindB(FB1, not FIsBlack);
    end;
  end;
end;

procedure TdxPDFCCITTFaxDecoder.DecodeGroup3;
begin
  while DecodeGroup3Line do ;
end;

procedure TdxPDFCCITTFaxDecoder.DecodeGroup3TwoDimensional;
var
  ARemainTwoDimensionalLineCount: Integer;
begin
  ReadEOL;
  if not ReadBit then
    TdxPDFUtils.Abort;
  if DecodeGroup3Line then
  begin
    ReadEOL;
    ARemainTwoDimensionalLineCount := FTwoDimensionalLineCount;
    while not IsCompleted do
      if ReadBit then
      begin
        if ARemainTwoDimensionalLineCount > 0 then
          TdxPDFUtils.Abort;
        if not DecodeGroup3Line then
          Break;
        ARemainTwoDimensionalLineCount := FTwoDimensionalLineCount;
        ReadEOL;
      end
      else
        ARemainTwoDimensionalLineCount := Max(0, ARemainTwoDimensionalLineCount - DecodeGroup4);
    if ARemainTwoDimensionalLineCount > 0 then
      TdxPDFUtils.Abort;
  end;
end;

{ TdxPDFUnknownStreamFilter }

class function TdxPDFUnknownStreamFilter.GetName: string;
begin
  Result := '';
end;

class function TdxPDFUnknownStreamFilter.GetShortName: string;
begin
  Result := '_';
end;

function TdxPDFUnknownStreamFilter.DoDecode(const AData: TBytes): TBytes;
begin
  Result := AData;
end;

{ TdxPDFPredictableStreamFilter }

function TdxPDFPredictableStreamFilter.DoDecode(const AData: TBytes): TBytes;
var
  APerformedData: TBytes;
begin
  APerformedData := PerformDecode(AData);
  case FPredictor of
    fpNoPrediction:
      Result := APerformedData;
    fpTiffPredictor:
      ApplyTIFFPrediction(APerformedData, Result);
  else
    ApplyPNGPrediction(APerformedData, Result);
  end;
end;

procedure TdxPDFPredictableStreamFilter.Initialize(ADecodeParameters: TObject);
begin
  inherited Initialize(ADecodeParameters);
  FBitsPerComponent := 8;
  FColors := 1;
  FColumns := 1;
  FPredictor := fpNoPrediction;
  if (ADecodeParameters <> nil) and ((ADecodeParameters as TdxPDFBase).ObjectType = otDictionary) then
    DoInitialize(TdxPDFDictionary(ADecodeParameters));
end;

procedure TdxPDFPredictableStreamFilter.DoInitialize(ADictionary: TdxPDFDictionary);

  procedure SetParameter(ADictionary: TdxPDFDictionary; const AParameterKey: string; out AParameter: Integer);
  begin
    if ADictionary.Contains(AParameterKey) then
      AParameter := ADictionary.GetInteger(AParameterKey)
  end;

const
  sdxColorsDictionaryKey = 'Colors';
  sdxColumnsDictionaryKey = 'Columns';
  sdxPredictorDictionaryKey = 'Predictor';
begin
  SetParameter(ADictionary, TdxPDFKeywords.BitsPerComponent, FBitsPerComponent);
  SetParameter(ADictionary, sdxColorsDictionaryKey, FColors);
  SetParameter(ADictionary, sdxColumnsDictionaryKey, FColumns);
  FPredictor := TdxPDFFilterPredictor(ADictionary.GetInteger(sdxPredictorDictionaryKey, 1));
end;

procedure TdxPDFPredictableStreamFilter.ApplyTIFFPrediction(const AData: TBytes; out Result: TBytes);
var
  APredictor: TdxPDFTIFFPrediction;
begin
  APredictor := TdxPDFTIFFPrediction.Create;
  try
    Result := APredictor.Decode(AData, FPredictor, FColors, FColumns, FBitsPerComponent);
  finally
    APredictor.Free;
  end;
end;

procedure TdxPDFPredictableStreamFilter.ApplyPNGPrediction(const AData: TBytes; out Result: TBytes);
var
  APredictor: TdxPDFPngPredictor;
begin
  APredictor := TdxPDFPngPredictor.Create;
  try
    Result := APredictor.Decode(AData, FPredictor, FColors, FColumns, FBitsPerComponent);
  finally
    APredictor.Free;
  end;
end;

{ TdxPDFFlateStreamFilter }

class function TdxPDFFlateStreamFilter.GetName: string;
begin
  Result := 'FlateDecode';
end;

class function TdxPDFFlateStreamFilter.GetShortName: string;
begin
  Result := 'Fl';
end;

function TdxPDFFlateStreamFilter.Encode(const AData: TBytes): TBytes;
var
  AOutput: TBytesStream;
  ACompressionStream: TZCompressionStream;
begin
  AOutput := TBytesStream.Create;
  try
    ACompressionStream := TZCompressionStream.Create(AOutput);
    try
      ACompressionStream.WriteBuffer(AData[0], Length(AData));
    finally
      ACompressionStream.Free;
    end;
    SetLength(Result, AOutput.Size);
    AOutput.Position := 0;
    AOutput.ReadBuffer(Result[0], AOutput.Size);
  finally
    AOutput.Free;
  end;
end;

function TdxPDFFlateStreamFilter.PerformDecode(const AData: TBytes): TBytes;
var
  ABuffer: TBytes;
  ACount: Integer;
  ADecompressionStream: TZDecompressionStream;
  AInput: TBytesStream;
  APrevStreamPosition: Int64;
begin
  AInput := TBytesStream.Create(AData);
  try
    ADecompressionStream := TDecompressionStream.Create(AInput);
    try
      while True do
      begin
        SetLength(ABuffer, 256);
        APrevStreamPosition := ADecompressionStream.Position;
        try
          ACount := ADecompressionStream.Read(ABuffer[0], 256);
        except
          ACount := ADecompressionStream.Position - APrevStreamPosition;
        end;
        if ACount = 256 then
          TdxPDFUtils.AddData(ABuffer, Result)
        else
        begin
          if ACount <> 0 then
          begin
            SetLength(ABuffer, ACount);
            TdxPDFUtils.AddData(ABuffer, Result);
          end;
          Break;
        end;
      end;
    finally
      ADecompressionStream.Free;
    end;
  finally
    AInput.Free;
  end;
end;

{ TdxPDFLZWStreamFilter }

class function TdxPDFLZWStreamFilter.GetName: string;
begin
  Result := 'LZWDecode';
end;

class function TdxPDFLZWStreamFilter.GetShortName: string;
begin
  Result := 'LZW';
end;

function TdxPDFLZWStreamFilter.PerformDecode(const AData: TBytes): TBytes;
var
  ADecoder: TdxLZWDecoder;
begin
  ADecoder := TdxLZWDecoder.Create;
  try
    Result := ADecoder.Decode(AData, 9, FEarlyChangeValue);
  finally
    ADecoder.Free;
  end;
end;

procedure TdxPDFLZWStreamFilter.DoInitialize(ADictionary: TdxPDFDictionary);
begin
  FEarlyChangeValue := ADictionary.GetBoolean('EarlyChange', True);
end;

procedure TdxPDFLZWStreamFilter.Initialize(ADecodeParameters: TObject);
begin
  FEarlyChangeValue := True;
  inherited Initialize(ADecodeParameters);
end;

{ TdxPDFASCIIHexDecodeFilter }

class function TdxPDFASCIIHexDecodeFilter.GetName: string;
begin
  Result := 'ASCIIHexDecode';
end;

class function TdxPDFASCIIHexDecodeFilter.GetShortName: string;
begin
  Result := 'AHx';
end;

function TdxPDFASCIIHexDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  AHigh: Boolean;
  ADecoded, AElement, ADigit: Byte;
begin
  SetLength(Result, 0);
  AHigh := True;
  ADecoded := 0;
  ADigit := 0;
  for AElement in AData do
  begin
    if not TdxPDFUtils.IsWhiteSpace(AElement) then
    begin
      case AElement of
        FZero, FOne, FTwo, FThree, FFour, FFive, FSix, FSeven, FEight, FNine:
          ADigit := AElement - FZero;
        FA, FB, FC, FD, FE, FF:
          ADigit := AElement - FA + TdxPDFDefinedSymbols.LineFeed;
        FCapitalA, FCapitalB, FCapitalC, FCapitalD, FCapitalE, FCapitalF:
          ADigit := AElement - FCapitalA + TdxPDFDefinedSymbols.LineFeed;
        TdxPDFDefinedSymbols.EndObject:
          if not AHigh then
            TdxPDFUtils.AddByte(ADecoded, Result)
          else
            Break;
      else
        TdxPDFUtils.RaiseTestException;
        Break;
      end;

      if AHigh then
        ADecoded := ADigit shl 4
      else
        TdxPDFUtils.AddByte(ADecoded + ADigit, Result);
      AHigh := not AHigh;
    end;
  end;
end;

{ TdxPDFDCTDecodeFilter }

function TdxPDFDCTDecodeFilter.DecodeImageData(const AData: TBytes;
  AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData;

  procedure PrepareData(out AActualData: TBytes);
  var
    I, ADataLength: Integer;
  begin
    AActualData := AData;
    ADataLength := Length(AData);
    for I := 0 to ADataLength - 1 do
      if not TdxPDFUtils.IsWhiteSpace(AData[I]) then
      begin
        if I <> 0 then
          TdxPDFUtils.CopyData(AData, I, AActualData, 0, ADataLength - I);
        Break;
      end;
  end;

var
  AColumnIndex, ARowIndex, AStartIndex, ACurrentSourceIndex, ACurrentDestinationIndex, I: Integer;
  AActualData, ADecodedData: TBytes;
  ADecodeResult: TDCTDecodeResult;
begin
  PrepareData(AActualData);
  ADecodeResult := TDecoder.Decode(AActualData, AWidth, AHeight);
  AStartIndex := 0;
  ACurrentDestinationIndex := 0;
  SetLength(ADecodedData, AWidth * AHeight * AComponentCount);
  for ARowIndex := 0 to AHeight - 1 do
  begin
    ACurrentSourceIndex := AStartIndex;
    for AColumnIndex := 0 to AWidth - 1 do
      if AComponentCount = 4 then
        for I := 0 to 4 - 1 do
        begin
          ADecodedData[ACurrentDestinationIndex] := 255 - ADecodeResult.Data[ACurrentSourceIndex];
          Inc(ACurrentSourceIndex);
          Inc(ACurrentDestinationIndex);
        end
      else
      begin
        SetLength(AActualData, AComponentCount);
        TdxPDFUtils.CopyData(ADecodeResult.Data, ACurrentSourceIndex, AActualData, 0, AComponentCount);
        Inc(ACurrentSourceIndex, AComponentCount);
        for I := AComponentCount - 1 downto 0 do
        begin
          ADecodedData[ACurrentDestinationIndex] := AActualData[I];
          Inc(ACurrentDestinationIndex);
        end;
        SetLength(AActualData, 0);
      end;
    Inc(AStartIndex, ADecodeResult.Stride);
  end;
  Result.Data := ADecodedData;
  Result.PixelFormat := pfUnknown;
end;

class function TdxPDFDCTDecodeFilter.GetName: string;
begin
  Result := 'DCTDecode';
end;

class function TdxPDFDCTDecodeFilter.GetShortName: string;
begin
  Result := 'DCT';
end;

function TdxPDFDCTDecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  Result := AData;
end;

procedure TdxPDFDCTDecodeFilter.Initialize(ADecodeParameters: TObject);
var
  AColorTransformValue: Integer;
begin
  inherited Initialize(ADecodeParameters);
  if not (ADecodeParameters is TdxPDFNull) and (ADecodeParameters <> nil) and
    (ADecodeParameters is TdxPDFDictionary) then
  begin
    AColorTransformValue := (ADecodeParameters as TdxPDFDictionary).GetInteger('ColorTransform');
    if TdxPDFUtils.IsIntegerValid(AColorTransformValue) then
      case AColorTransformValue of
        0:
          FColorTransform := False;
        1:
          FColorTransform := True;
      else
        TdxPDFUtils.Abort;
      end;
  end;
end;

class function TdxPDFDCTDecodeFilter.TDecoder.Decode(const AData: TBytes; AWidth, AHeight: Integer): TDCTDecodeResult;
var
  ADecoder: TDecoder;
begin
  ADecoder := TDecoder.Create;
  try
    ADecoder.Initialize(AData, AWidth, AHeight);
    Result := ADecoder.DoDecode;
  finally
    ADecoder.Free;
  end;
end;

function TdxPDFDCTDecodeFilter.TDecoder.DoDecode: TDCTDecodeResult;

  function CreateBitmapStream(const AData: TBytes): TMemoryStream;
  begin
    Result := TMemoryStream.Create;
    Result.WriteBuffer(AData[0], Length(AData));
    Result.Position := 0;
  end;

var
  AImage: TdxGPImage;
  ABitmapData: TBitmapData;
  ABitmapStream: TMemoryStream;
  ABitmapFormat: TdxGpPixelFormat;
  ARect: TdxGpRect;
begin
  ARect := MakeRect(0, 0, FImageWidth, FImageHeight);
  ABitmapStream := CreateBitmapStream(FImageData);
  try
    AImage := TdxGPImage.CreateFromStream(ABitmapStream);
    try
      GdipCheck(GdipGetImagePixelFormat(AImage.Handle, ABitmapFormat));
      GdipCheck(GdipBitmapLockBits(AImage.Handle, @ARect, 1, ABitmapFormat, @ABitmapData));
      Result.Stride := ABitmapData.Stride;
      try
        SetLength(Result.Data, Result.Stride * FImageHeight);
        cxCopyData(ABitmapData.Scan0, @Result.Data[0], Length(Result.Data));
      finally
        GdipCheck(GdipBitmapUnlockBits(AImage.Handle, @ABitmapData));
      end;
    finally
      AImage.Free;
    end;
  finally
    ABitmapStream.Free;
  end;
end;

procedure TdxPDFDCTDecodeFilter.TDecoder.Initialize(const AData: TBytes; AWidth, AHeight: Integer);
begin
  FImageData := AData;
  FImageWidth := AWidth;
  FImageHeight := AHeight;
end;

{ TdxPDFJPXDecodeFilter }

class function TdxPDFJPXDecodeFilter.GetName: string;
begin
  Result := 'JPXDecode';
end;

class function TdxPDFJPXDecodeFilter.GetShortName: string;
begin
  Result := 'JPX';
end;

function TdxPDFJPXDecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  TdxPDFUtils.Abort;
end;

function TdxPDFJPXDecodeFilter.DecodeImageData(const AData: TBytes;
  AWidth, AHeight, AComponentCount: Integer): TdxPDFDocumentDecodedImageData;
begin
  Result.Data := TdxJPXImage.Decode(AData, AComponentCount);
  if AComponentCount = 1 then
    Result.PixelFormat := pfGray8bit
  else
    Result.PixelFormat := pfArgb24bpp;
end;

{ TdxPDFASCII85DecodeFilter }

class function TdxPDFASCII85DecodeFilter.GetName: string;
begin
  Result := 'ASCII85Decode';
end;

class function TdxPDFASCII85DecodeFilter.GetShortName: string;
begin
  Result := 'A85';
end;

function TdxPDFASCII85DecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  ADataLength, APosition, I : Integer;
  ANext: Byte;
begin
  Result := AData;
  ADataLength := Length(AData);
  if ADataLength > 0 then
  begin
    SetLength(FGroup, FGroupSize);
    SetLength(Result, 0);
    APosition := 0;
    while APosition < ADataLength do
    begin
      ANext := AData[APosition];
      Inc(APosition);
      if not TdxPDFUtils.IsWhiteSpace(ANext) then
        case ANext of
          Byte('~'):
            begin
              while APosition < ADataLength do
              begin
                ANext := AData[APosition];
                Inc(APosition);
                if ANext = TdxPDFDefinedSymbols.EndObject then
                  Break;
                if TdxPDFUtils.IsWhiteSpace(ANext) then
                  TdxPDFUtils.RaiseTestException;
              end;
              Break;
            end;
          Byte('z'):
              if FIndex = 0 then
                for I := 0 to 3 do
                  TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.Null, Result)
              else
                TdxPDFUtils.RaiseTestException;
        else
          ProcessGroup(ANext, Result);
        end;
    end;
    ProcessFinalGroup(Result);
  end;
  SetLength(FGroup, 0);
end;

procedure TdxPDFASCII85DecodeFilter.Initialize(ADecodeParameters: TObject);
begin
  inherited Initialize(ADecodeParameters);
  FMaxValue := (Int64(1) shl 32) - 1;
  FMultiplier1 := 85;
  FMultiplier2 := FMultiplier1 * FMultiplier1;
  FMultiplier3 := FMultiplier2 * FMultiplier1;
  FMultiplier4 := FMultiplier3 * FMultiplier1;
  FIndex := 0;
  FGroupSize := 5;
end;

function TdxPDFASCII85DecodeFilter.CheckEncodedData(AData: Byte): Boolean;
begin
  Result := (AData >= TdxPDFDefinedSymbols.ExclamationMark) and (AData <= Byte('u'));
end;

function TdxPDFASCII85DecodeFilter.ConvertCurrentGroup: Int64;
begin
  Result :=
    (FGroup[0] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier4 +
    (FGroup[1] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier3 +
    (FGroup[2] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier2 +
    (FGroup[3] - TdxPDFDefinedSymbols.ExclamationMark) * FMultiplier1 +
     FGroup[4] - TdxPDFDefinedSymbols.ExclamationMark;
end;

procedure TdxPDFASCII85DecodeFilter.DecodeGroup(AGroupSize: Integer; var AResult: TBytes);
var
  AValue: Int64;
begin
  AValue := Max(Min(ConvertCurrentGroup, FMaxValue), 0);
  TdxPDFUtils.AddByte((AValue and $FF000000) shr 24, AResult);
  Dec(AGroupSize);
  if AGroupSize > 0 then
  begin
    Dec(AGroupSize);
    TdxPDFUtils.AddByte((AValue and $FF0000) shr 16, AResult);
    if AGroupSize > 0 then
    begin
      Dec(AGroupSize);
      TdxPDFUtils.AddByte((AValue and $FF00) shr 8, AResult);
      if AGroupSize > 0 then
        TdxPDFUtils.AddByte(AValue and $FF, AResult);
    end;
  end;
  FIndex := 0;
end;

procedure TdxPDFASCII85DecodeFilter.ProcessFinalGroup(var AResult: TBytes);
var
  I: Integer;
begin
  if FIndex > 0 then
  begin
    for I := FIndex to FGroupSize - 1 do
      FGroup[I] := Byte('u');
    DecodeGroup(FIndex - 1, AResult);
  end;
end;

procedure TdxPDFASCII85DecodeFilter.ProcessGroup(AData: Byte; var AResult: TBytes);
begin
  if CheckEncodedData(AData) then
  begin
    FGroup[FIndex] := AData;
    Inc(FIndex);
    if FIndex = FGroupSize then
      DecodeGroup(4, AResult);
  end;
end;

{ TdxPDFRunLengthDecodeFilter }

class function TdxPDFRunLengthDecodeFilter.GetName: string;
begin
  Result := 'RunLengthDecode';
end;

class function TdxPDFRunLengthDecodeFilter.GetShortName: string;
begin
  Result := 'RL';
end;

function TdxPDFRunLengthDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  B: Byte;
  I, ACount, AState: Integer;
begin
  SetLength(Result, 0);
  AState := 1;
  ACount := 0;
  for B in AData do
    case AState of
      1:
        if B <> 128 then
        begin
          if B <= 127 then
          begin
            AState := 2;
            ACount := B + 1;
          end
          else
          begin
            AState := 3;
            ACount := 257 - B;
          end
        end
        else
          Break;
      2:
        begin
          TdxPDFUtils.AddByte(B, Result);
          Dec(ACount);
          if ACount = 0 then
            AState := 1;
        end;
      3:
        begin
          AState := 1;
          for I := 0 to ACount - 1 do
            TdxPDFUtils.AddByte(B, Result);
        end;
  end;
end;

{ TdxPDFCCITTFaxDecodeFilter }

class function TdxPDFCCITTFaxDecodeFilter.GetName: string;
begin
  Result := 'CCITTFaxDecode';
end;

class function TdxPDFCCITTFaxDecodeFilter.GetShortName: string;
begin
  Result := 'CCF';
end;

function TdxPDFCCITTFaxDecodeFilter.DoDecode(const AData: TBytes): TBytes;
var
  ADecoder: TdxPDFCCITTFaxDecoder;
begin
  ADecoder := TdxPDFCCITTFaxDecoder.Create;
  try
    Result := ADecoder.Decode(Self, AData);
  finally
    ADecoder.Free;
  end;
end;

procedure TdxPDFCCITTFaxDecodeFilter.Initialize(ADecodeParameters: TObject);
var
  K: Integer;
  ADefaultColumns: Integer;
  AParameters: TdxPDFDictionary;
begin
  inherited Initialize(ADecodeParameters);
  ADefaultColumns := 1728;
  FColumns := ADefaultColumns;
  FEndOfBlock := True;

  AParameters := ADecodeParameters as TdxPDFDictionary;
  if AParameters = nil then
    FEncodingScheme := fesOneDimensional
  else
  begin
    K := AParameters.GetInteger('K', 0);
    if K < 0 then
      FEncodingScheme := fesTwoDimensional
    else
      if K = 0 then
        FEncodingScheme := fesOneDimensional
      else
      begin
        FEncodingScheme := fesMixed;
        FTwoDimensionalLineCount := K - 1;
      end;
    FEndOfLine := AParameters.GetBoolean('EndOfLine');
    FEncodedByteAlign := AParameters.GetBoolean('EncodedByteAlign');
    FColumns := AParameters.GetInteger('Columns', ADefaultColumns);
    FRows := AParameters.GetInteger('Rows', 0);
    FEndOfBlock := AParameters.GetBoolean('EndOfBlock', True);
    FBlackIs1 := AParameters.GetBoolean('BlackIs1');
    FDamagedRowsBeforeError := AParameters.GetInteger('DamagedRowsBeforeError', 0);
    if (FColumns <= 0) or (FRows < 0) or (FDamagedRowsBeforeError < 0) then
      TdxPDFUtils.RaiseTestException;
  end;
end;

{ TdxJBIG2GlobalSegments }

constructor TdxJBIG2GlobalSegments.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
end;

class function TdxJBIG2GlobalSegments.Parse(ARepository: TdxPDFCustomRepository; AValue: TdxPDFBase): TdxJBIG2GlobalSegments;
begin
  Result := nil;
  if AValue.ObjectType = otIndirectReference then
    Result := TdxJBIG2GlobalSegments.Parse(ARepository, ARepository.GetStream(TdxPDFReference(AValue).Number))
  else
    if AValue.ObjectType <> otStream then
      TdxPDFUtils.Abort
    else
      Result := TdxJBIG2GlobalSegments.Create(TdxPDFStream(AValue).UncompressedData);
end;

{ TdxPDFJBIG2DecodeFilter }

destructor TdxPDFJBIG2DecodeFilter.Destroy;
begin
  FreeAndNil(FGlobalSegments);
  inherited Destroy;
end;

class function TdxPDFJBIG2DecodeFilter.GetName: string;
begin
  Result := 'JBIG2Decode'
end;

class function TdxPDFJBIG2DecodeFilter.GetShortName: string;
begin
  Result := 'JBIG2Decode';
end;

function TdxPDFJBIG2DecodeFilter.DoDecode(const AData: TBytes): TBytes;
begin
  if FGlobalSegments = nil then
    Result := TdxJBIG2Image.Decode(AData, nil)
  else
    Result := TdxJBIG2Image.Decode(AData, FGlobalSegments.Data);
end;

procedure TdxPDFJBIG2DecodeFilter.Initialize(ADecodeParameters: TObject);
var
  AParameters: TdxPDFReaderDictionary;
  AReference: TdxPDFBase;
begin
  inherited Initialize(ADecodeParameters);
  if ADecodeParameters <> nil then
  begin
    AParameters := ADecodeParameters as TdxPDFReaderDictionary;
    AReference := AParameters.GetObject('JBIG2Globals') as TdxPDFBase;
    if (AReference <> nil) and (AReference.ObjectType <> otIndirectReference) then
      FGlobalSegments := TdxJBIG2GlobalSegments.Parse(AParameters.Repository, AReference);
  end;
end;

initialization
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFUnknownStreamFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFFlateStreamFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFLZWStreamFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFASCIIHexDecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFASCII85DecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFRunLengthDecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFDCTDecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFCCITTFaxDecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFJBIG2DecodeFilter);
  dxPDFStreamFilterFactory.RegisterFilter(TdxPDFJPXDecodeFilter);

finalization
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFJPXDecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFJBIG2DecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFCCITTFaxDecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFDCTDecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFRunLengthDecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFASCII85DecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFASCIIHexDecodeFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFLZWStreamFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFFlateStreamFilter);
  dxPDFStreamFilterFactory.UnregisterFilter(TdxPDFUnknownStreamFilter);
  FreeAndNil(dxgPDFStreamFilterFactory);

end.

