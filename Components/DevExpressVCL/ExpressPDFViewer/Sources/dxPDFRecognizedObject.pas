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

unit dxPDFRecognizedObject;

interface

uses
  Types, SysUtils, Graphics, Generics.Defaults, Generics.Collections, cxGeometry, dxPDFBase, dxPDFTypes, dxPDFText;

const
  // HitTests bits
  hcImage  = 2;

type
  { TdxPDFRecognizedObject }

  TdxPDFRecognizedObject = class(TdxPDFReferencedObject, IUnknown)
  protected
    function GetHitCode: Integer; virtual; abstract;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
  public
    property HitCode: Integer read GetHitCode;
  end;
  TdxPDFRecognizedObjectList<T: TdxPDFRecognizedObject> = class(TdxPDFObjectList<T>);

  { TdxPDFImage }

  TdxPDFImage = class(TdxPDFRecognizedObject)
  strict private
    FBounds: TdxRectF;
    FBitmap: Graphics.TBitmap;
    FDocumentImage: TdxPDFReferencedObject;

    function GetBitmap: Graphics.TBitmap;
    procedure SetDocumentImage(const AValue: TdxPDFReferencedObject);
  protected
    function GetHitCode: Integer; override;
    procedure SetBounds(const ABounds: TdxRectF);

    property Bounds: TdxRectF read FBounds;
    property DocumentImage: TdxPDFReferencedObject read FDocumentImage write SetDocumentImage;
  public
    constructor Create;
    destructor Destroy; override;

    property Bitmap: Graphics.TBitmap read GetBitmap;
  end;
  TdxPDFImageList = class(TdxPDFRecognizedObjectList<TdxPDFImage>);

  { TdxPDFTextObject }

  TdxPDFTextObject = class(TdxPDFRecognizedObject)
  strict protected
    FBounds: TdxPDFOrientedRect;
    FText: string;
  protected
    function GetHitCode: Integer; override;
    function GetText: string; virtual;

    function GetBounds: TdxPDFOrientedRect; virtual;
    function IsTextEmpty: Boolean;

    property Bounds: TdxPDFOrientedRect read GetBounds;
  public
    constructor Create; overload;
    constructor Create(const ABounds: TdxPDFOrientedRect; const AText: string); overload;

    property Text: string read GetText;
  end;
  TdxPDFTextObjectList<T: TdxPDFTextObject> = class(TdxPDFRecognizedObjectList<T>);

  { TdxPDFTextWordPart }

  TdxPDFTextWordPart = class(TdxPDFTextObject)
  strict private
    FCharacters: TdxPDFTextCharacters;
    FWordEnded: Boolean;
    FWordIndex: Integer;
    FWrapOffset: Integer;

    function GetEndWordPosition: Integer;
    function GetNextWrapOffset: Integer;
  protected
    function GetText: string; override;

    property EndWordPosition: Integer read GetEndWordPosition;
    property NextWrapOffset: Integer read GetNextWrapOffset;
    property WordEnded: Boolean read FWordEnded;
    property WrapOffset: Integer read FWrapOffset;
  public
    constructor Create(const ABounds: TdxPDFOrientedRect; const ACharacters: TdxPDFTextCharacters;
      AWrapOffset: Integer; AWordEnded: Boolean);

    function IsEmpty: Boolean;
    function Same(AWordIndex, AOffset: Integer): Boolean;

    property Characters: TdxPDFTextCharacters read FCharacters;
    property WordIndex: Integer read FWordIndex write FWordIndex;
  end;
  TdxPDFTextWordPartList = class(TdxPDFTextObjectList<TdxPDFTextWordPart>);

  { TdxPDFTextWord }

  TdxPDFTextWord = class(TdxPDFTextObject)
  strict private
    FBoundsList: TdxPDFOrientedRectList;
    FCharacters: TdxPDFTextCharacters;
    FIndex: Integer;
    FWordPartList: TdxPDFTextWordPartList;

    function GetCharacters: TdxPDFTextCharacters;
    function GetBoundsList: TdxPDFOrientedRectList;
    procedure SetIndex(const AValue: Integer);
  protected
    function GetBounds: TdxPDFOrientedRect; override;
    function GetText: string; override;

    property BoundsList: TdxPDFOrientedRectList read GetBoundsList;
    property Characters: TdxPDFTextCharacters read GetCharacters;
    property PartList: TdxPDFTextWordPartList read FWordPartList;
  public
    constructor Create(AParts: TdxPDFTextWordPartList);
    destructor Destroy; override;

    function Overlap(AWord: TdxPDFTextWord): Boolean;

    property Index: Integer read FIndex write SetIndex;
  end;
  TdxPDFTextWordList = class(TdxPDFTextObjectList<TdxPDFTextWord>);

  { TdxPDFTextLine }

  TdxPDFTextLine = class(TdxPDFTextObject)
  strict private
    FWordList: TdxPDFTextWordList;
    FWordPartList: TdxPDFTextWordPartList;
  protected
    function GetBounds: TdxPDFOrientedRect; override;
    function GetHitCode: Integer; override;
    function GetText: string; override;

    function GetLineRange: TdxPDFPageTextRange;
    function GetPosition(const APosition: TdxPDFPosition): TdxPDFTextPosition; overload;
    function GetPosition(APageIndex: Integer; const P: TdxPointF): TdxPDFTextPosition; overload;
    function GetRange(APageIndex: Integer; const R: TdxRectF): TdxPDFPageTextRange;

    function GetHighlight(AStartWordIndex, AStartOffset, AEndWordIndex, AEndOffset: Integer;
      ASplitBounds: Boolean; ARectIndex: Integer): TdxPDFOrientedRect; overload;
    function GetHighlight(AStartWordIndex, AStartOffset: Integer;
      ASplitBounds: Boolean; ARectIndex: Integer): TdxPDFOrientedRect; overload;
    function GetHighlights(AStartWordIndex, AStartOffset, AEndWordIndex, AEndOffset: Integer;
      ASplitBounds: Boolean): TdxPDFOrientedRectList; overload;
    function GetHighlights(AStartWordIndex, AStartOffset: Integer;
      ASplitBounds: Boolean): TdxPDFOrientedRectList; overload;

    function PositionInLine(AWordIndex: Integer; AOffset: Integer): Boolean;
    procedure PopulateWordList(AWordList: TdxPDFTextWordList);
    procedure PopulateWordPartList(AWordPartList: TdxPDFTextWordPartList);

    property WordList: TdxPDFTextWordList read FWordList;
    property WordPartList: TdxPDFTextWordPartList read FWordPartList;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TdxPDFTextLineList }

  TdxPDFTextLineList = class(TdxPDFTextObjectList<TdxPDFTextLine>)
  strict private
    FWordList: TdxPDFTextWordList;

    function CreateWordList: TdxPDFTextWordList;
    function GetWordList: TdxPDFTextWordList;
  protected
    property WordList: TdxPDFTextWordList read GetWordList;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(const APosition: TdxPDFPosition; const ATextExpansionFactor: TdxPointF; out ALine: TdxPDFTextLine): Boolean;
  end;

  { TdxPDFTextBlock }

  TdxPDFTextBlock = class
  strict private
    FCharacters: TdxPDFTextCharacters;
    FFontData: TdxPDFFontData;
    FFontDescent: Single;
    FFontHeight: Single;
    FCharacterSpacing: Single;
    FLocation: TdxPointF;
    FAngle: Single;
    FCharLocations: TArray<TdxPointF>;
  public
    constructor Create(AData: TdxPDFStringData; AFontData: TdxPDFFontData; AState: TdxPDFTextState; ATextWidthFactor,
      ATextHeightFactor: Single; const AGlyphOffsets: TDoubleDynArray; ALocation: TdxPointF; AAngle: Single);

    function CalculateCharacterWidth(ACharacter: Integer; AHorizontalFontFactor: Single): Single;
    function GetActualCharacter(const ACharacter: TBytes): string;

    property Angle: Single read FAngle;
    property Characters: TdxPDFTextCharacters read FCharacters;
    property CharacterSpacing: Single read FCharacterSpacing;
    property CharLocations: TArray<TdxPointF> read FCharLocations;
    property FontData: TdxPDFFontData read FFontData;
    property FontDescent: Single read FFontDescent;
    property FontHeight: Single read FFontHeight;
    property Location: TdxPointF read FLocation;
  end;

  { TdxPDFTextParserState }

  TdxPDFTextParserState = class
  strict private
    FActualRTL: Boolean;
    FBlocks: TList<TdxPDFTextBlock>;
    FPageCropBox: TdxRectF;
    FPreviousCharacterBlock: TdxPDFTextBlock;
    FCurrentCharacterBlock: TdxPDFTextBlock;
    FPreviousCharacter: TdxPDFTextCharacter;
    FCurrentCharacter: TdxPDFTextCharacter;
    FCurrentCharLocation: TdxPointF;
    FCurrentWordLeftBoundary: TdxPointF;
    FCurrentWordRightBoundary: TdxPointF;
    FPreviousCharLocation: TdxPointF;
    FCharacterIndex: Integer;
    FBlockIndex: Integer;
    FIsFinished: Boolean;
    FIsRTL: Boolean;
    FIsLineStart: Boolean;
    FIsSpace: Boolean;
    FIsSeparator: Boolean;
    FIsWrap: Boolean;
    FIsCJKWord: Boolean;

    function IsIndex: Boolean;
    function CheckHorizontalPosition: Boolean;
    function CheckVerticalPosition: Boolean;
  public
    constructor Create(ABlocks: TList<TdxPDFTextBlock>; APageCropBox: TdxRectF);

    function MoveToNextCharacter: Boolean;
    function MoveToNextBlock: Boolean;
    procedure MoveNext;

    property CurrentCharacterBlock: TdxPDFTextBlock read FCurrentCharacterBlock;
    property CurrentCharacter: TdxPDFTextCharacter read FCurrentCharacter;
    property CurrentCharLocation: TdxPointF read FCurrentCharLocation;
    property CurrentWordLeftBoundary: TdxPointF read FCurrentWordLeftBoundary write FCurrentWordLeftBoundary;
    property CurrentWordRightBoundary: TdxPointF read FCurrentWordRightBoundary write FCurrentWordRightBoundary;
    property PreviousCharacter: TdxPDFTextCharacter read FPreviousCharacter;
    property PreviousCharacterBlock: TdxPDFTextBlock read FPreviousCharacterBlock;
    property PreviousCharLocation: TdxPointF read FPreviousCharLocation;
    property IsCJKWord: Boolean read FIsCJKWord;
    property IsLineStart: Boolean read FIsLineStart;
    property IsFinished: Boolean read FIsFinished;
    property IsRTL: Boolean read FIsRTL;
    property IsSeparator: Boolean read FIsSeparator;
    property IsSpace: Boolean read FIsSpace;
    property IsWrap: Boolean read FIsWrap;
    property PageCropBox: TdxRectF read FPageCropBox;
  end;

  { TdxPDFPageWordBuilder }

  TdxPDFPageWordBuilder = class
  strict private
    FParserState: TdxPDFTextParserState;
    FCharacters: TdxPDFTextCharacters;
    FWordPartList: TdxPDFTextWordPartList;
    FWordMinX: Single;
    FWordMaxX: Single;
    FWordMinY: Single;
    FWordMaxY: Single;
    FWordAngle: Single;
    FWrapOffset: Integer;
  protected
    function BuildWord(ALineXOffset: Single): TdxPDFTextWord;
    procedure ClearWordPartData;
    procedure ProcessChar;
  public
    constructor Create(AParserState: TdxPDFTextParserState);
    destructor Destroy; override;
  end;

  { TdxPDFPageTextLineBuilder }

  TdxPDFPageTextLineBuilder = class
  strict private
    FParserState: TdxPDFTextParserState;
    FWordBuilder: TdxPDFPageWordBuilder;
    FWordIndex: Integer;
    FWordList: TdxPDFTextWordList;
    FWordPartList: TdxPDFTextWordPartList;
    FLineXOffset: Single;
    function OverlapsWithPreviousWords(AWord: TdxPDFTextWord): Boolean;
    procedure AddTextLine(ATextLines: TdxPDFTextLineList; AWordList: TdxPDFTextWordList; APartList: TdxPDFTextWordPartList);
    procedure Clear;
    procedure EnumerateWordsAndFillParts;
    procedure RecreateWordList;
    procedure RecreateWordPartList;
  public
    constructor Create(AParserState: TdxPDFTextParserState);
    destructor Destroy; override;

    procedure Populate(ATextLines: TdxPDFTextLineList);
  end;

  { TdxPDFRecognizedTextUtils }

  TdxPDFRecognizedTextUtils = class(TdxPDFBaseTextUtils)
  strict protected
    class procedure Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition; AWordPart: TdxPDFTextWordPart); overload;
    class procedure Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition; ALine: TdxPDFTextLine); overload;
    class procedure Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition; ALines: TdxPDFTextLineList); overload;
    class procedure AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart); overload;
    class procedure AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart;  AStartOffset: Integer); overload;
    class procedure AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart;   AStartOffset, AEndOffset: Integer); overload;
  public
    class function ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition; AWordPart: TdxPDFTextWordPart): string; overload;
    class function ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition; ALine: TdxPDFTextLine): string; overload;
    class function ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition; ALines: TdxPDFTextLineList): string; overload;
  end;

implementation

uses
  Math, Classes, dxCore, dxPDFCore, dxPDFCharacterMapping, dxPDFFont, dxPDFFontEncoding, dxPDFUtils;

type
  TdxPDFFontDataAccess = class(TdxPDFFontData);

{ TdxPDFRecognizedObject }

function TdxPDFRecognizedObject._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TdxPDFRecognizedObject._Release: Integer; stdcall;
begin
  Result := -1;
end;

function TdxPDFRecognizedObject.QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TdxPDFImage }

constructor TdxPDFImage.Create;
begin
  inherited Create;
  FBounds := dxNullRectF;
  FBitmap := nil;
  DocumentImage := nil;
end;

destructor TdxPDFImage.Destroy;
begin
  DocumentImage := nil;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TdxPDFImage.GetHitCode: Integer;
begin
  Result := hcImage;
end;

procedure TdxPDFImage.SetBounds(const ABounds: TdxRectF);
begin
  FBounds := ABounds;
end;

function TdxPDFImage.GetBitmap: Graphics.TBitmap;
begin
  if FBitmap = nil then
    FBitmap := (DocumentImage as TdxPDFDocumentImage).GetAsBitmap;
  Result := FBitmap;
end;

procedure TdxPDFImage.SetDocumentImage(const AValue: TdxPDFReferencedObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDocumentImage));
end;

{ TdxPDFTextObject }

constructor TdxPDFTextObject.Create;
begin
  Create(TdxPDFOrientedRect.Create, '');
end;

constructor TdxPDFTextObject.Create(const ABounds: TdxPDFOrientedRect; const AText: string);
begin
  inherited Create;
  FBounds := ABounds;
  FText := AText;
end;

function TdxPDFTextObject.GetHitCode: Integer;
begin
  Result := hcText;
end;

function TdxPDFTextObject.GetText: string;
begin
  Result := FText;
end;

function TdxPDFTextObject.GetBounds: TdxPDFOrientedRect;
begin
  Result := FBounds;
end;

function TdxPDFTextObject.IsTextEmpty: Boolean;
begin
  Result := FText = '';
end;

{ TdxPDFTextWordPart }

constructor TdxPDFTextWordPart.Create(const ABounds: TdxPDFOrientedRect; const ACharacters: TdxPDFTextCharacters;
  AWrapOffset: Integer; AWordEnded: Boolean);
begin
  inherited Create(ABounds, Text);
  FCharacters := ACharacters;
  FWrapOffset := AWrapOffset;
  FWordEnded := AWordEnded;
end;

function TdxPDFTextWordPart.GetText: string;
var
  I: Integer;
  ABuilder: TdxBiDiStringBuilder;
begin
  if IsTextEmpty then
  begin
    ABuilder := TdxBiDiStringBuilder.Create;
    try
      for I := Low(FCharacters) to High(FCharacters) do
        ABuilder.Append(FCharacters[I].Text);
      FText := ABuilder.EndCurrentLineAndGetString;
    finally
      ABuilder.Free;
    end;
  end;
  Result := FText;
end;

function TdxPDFTextWordPart.IsEmpty: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(FCharacters) to High(FCharacters) do
  begin
    Result := Length(FCharacters[I].Text) = 0;
    if not Result then
      Break;
  end;
end;

function TdxPDFTextWordPart.Same(AWordIndex, AOffset: Integer): Boolean;
begin
  Result := (WordIndex = AWordIndex) and (WrapOffset <= AOffset) and (AOffset <= NextWrapOffset);
end;

function TdxPDFTextWordPart.GetNextWrapOffset: Integer;
begin
  Result := WrapOffset + Length(Text);
end;

function TdxPDFTextWordPart.GetEndWordPosition: Integer;
var
  APosition: Integer;
begin
  APosition := NextWrapOffset;
  if WordEnded then
    Result := APosition
  else
    Result := APosition - 1;
end;

{ TdxPDFTextWord }

constructor TdxPDFTextWord.Create(AParts: TdxPDFTextWordPartList);
var
  APart: TdxPDFTextWordPart;
begin
  inherited Create;
  FCharacters := nil;
  FWordPartList := TdxPDFTextWordPartList.Create;
  for APart in AParts do
    FWordPartList.Add(TdxPDFTextWordPart.Create(APart.Bounds, APart.Characters, APart.WrapOffset, APart.WordEnded));
end;

destructor TdxPDFTextWord.Destroy;
begin
  FreeAndNil(FWordPartList);
  FreeAndNil(FBoundsList);
  inherited Destroy;
end;

function TdxPDFTextWord.Overlap(AWord: TdxPDFTextWord): Boolean;
var
  I, L: Integer;
begin
  L := Length(Characters);
  Result := L = Length(AWord.Characters);
  if Result then
    for I := 0 to L - 1 do
    begin
      Result := Characters[I].Text = AWord.Characters[I].Text;
      if not Result then
        Break;
    end;
  if Result then
    for I := 0 to BoundsList.Count - 1 do
    begin
      Result := BoundsList[I].Overlap(AWord.BoundsList[I]);
      if not Result then
        Break;
    end;
end;

function TdxPDFTextWord.GetBounds: TdxPDFOrientedRect;
var
  R: TdxPDFOrientedRect;
  ABounds: TdxRectF;
begin
  ABounds := dxNullRectF;
  for R in BoundsList do
    ABounds := cxRectUnion(ABounds, R.Rect);
  Result := TdxPDFOrientedRect.Create(ABounds, 0);
end;

function TdxPDFTextWord.GetText: string;
var
  APartCount, I: Integer;
  APartText: string;
begin
  if IsTextEmpty then
  begin
    APartCount := FWordPartList.Count - 1;
    for I := 0 to APartCount - 1 do
    begin
      APartText := FWordPartList[I].Text;
      FText := FText + Copy(APartText, 1, Length(APartText) - 1);
    end;
    FText := FText + FWordPartList[APartCount].Text;
  end;
  Result := FText;
end;

function TdxPDFTextWord.GetCharacters: TdxPDFTextCharacters;

  function Concatenate(const AB1, AB2: TdxPDFTextCharacters): TdxPDFTextCharacters;
  begin
    if AB1 = nil then
      Exit(AB2);
    if AB2 = nil then
      Exit(AB1);
    SetLength(Result, Length(AB1) + Length(AB2));
    Move(AB1[0], Result[0], Length(AB1));
    Move(AB2[0], Result[Length(AB1)], Length(AB2));
  end;

var
  I: Integer;
begin
  if FCharacters = nil then
    for I := 0 to FWordPartList.Count - 1 do
      FCharacters := Concatenate(FCharacters, FWordPartList[I].Characters);
  Result := FCharacters;
end;

function TdxPDFTextWord.GetBoundsList: TdxPDFOrientedRectList;
var
  APart: TdxPDFTextWordPart;
begin
  if FBoundsList = nil then
  begin
    FBoundsList := TdxPDFOrientedRectList.Create;
    for APart in FWordPartList do
      FBoundsList.Add(APart.Bounds);
  end;
  Result := FBoundsList;
end;

procedure TdxPDFTextWord.SetIndex(const AValue: Integer);
var
  APart: TdxPDFTextWordPart;
begin
  FIndex := AValue;
  for APart in FWordPartList do
    APart.WordIndex := FIndex;
end;

{ TdxPDFTextLine }

function CompareWord(AWord1, AWord2: Pointer): Integer;

  function GetRotatedTopLeft(R: TdxPDFOrientedRect): TdxPointF;
  begin
    Result := TdxPDFRecognizedTextUtils.RotatePoint(R.TopLeft, R.Angle);
  end;

var
  Left, Right: TdxPDFTextWord;
begin
  Result := 0;
  if AWord1 <> AWord2 then
  begin
    Left := TdxPDFTextWord(AWord1);
    Right := TdxPDFTextWord(AWord2);
    Result := IfThen(GetRotatedTopLeft(Left.BoundsList[0]).X < GetRotatedTopLeft(Right.BoundsList[0]).X, -1, 1);
  end;
end;

constructor TdxPDFTextLine.Create;
begin
  inherited Create;
  FWordList := TdxPDFTextWordList.Create;
  FWordPartList := TdxPDFTextWordPartList.Create;
end;

destructor TdxPDFTextLine.Destroy;
begin
  FreeAndNil(FWordPartList);
  FreeAndNil(FWordList);
  inherited Destroy;
end;

function TdxPDFTextLine.GetBounds: TdxPDFOrientedRect;
var
  ARectList: TdxPDFOrientedRectList;
begin
  if not FBounds.IsValid then
  begin
    ARectList := GetHighlights(-1, 0, FWordPartList.Count - 1, -1, False);
    try
      FBounds := ARectList[0];
    finally
      ARectList.Free;
    end;
  end;
  Result := FBounds;
end;

function TdxPDFTextLine.GetHitCode: Integer;
begin
  Result := hcText;
end;

function TdxPDFTextLine.GetText: string;
var
  ARange: TdxPDFPageTextRange;
begin
  if IsTextEmpty then
  begin
    ARange := GetLineRange;
    FText := TdxPDFRecognizedTextUtils.ConvertToString(ARange.StartPosition, ARange.EndPosition, Self);
  end;
  Result := inherited GetText;
end;

function TdxPDFTextLine.GetLineRange: TdxPDFPageTextRange;
begin
  if WordPartList.Count > 0 then
    Result := TdxPDFPageTextRange.Create(-1, WordPartList[0].WordIndex, 0, WordPartList.Last.WordIndex,
      Length(WordPartList.Last.Characters))
  else
    Result := TdxPDFPageTextRange.Invalid;
end;

function TdxPDFTextLine.GetPosition(const APosition: TdxPDFPosition): TdxPDFTextPosition;
begin
  Result := GetPosition(APosition.PageIndex, APosition.Point)
end;

function TdxPDFTextLine.GetPosition(APageIndex: Integer; const P: TdxPointF): TdxPDFTextPosition;
var
  J: Integer;
  AAngle, ARotatedLeft, X: Single;
  ABounds, R: TdxPDFOrientedRect;
  APart, ANearestPart: TdxPDFTextWordPart;
begin
  if (FWordPartList = nil) or (FWordPartList.Count < 1) then
    Exit(TdxPDFTextPosition.Invalid);

  ANearestPart := nil;
  AAngle := FWordPartList[0].Bounds.Angle;
  for APart in FWordPartList do
  begin
    ARotatedLeft := TdxPDFRecognizedTextUtils.RotatePoint(APart.Bounds.TopLeft, -AAngle).X;
    X := TdxPDFRecognizedTextUtils.RotatePoint(P, -AAngle).X;
    if ARotatedLeft + APart.Bounds.Width < X then
      ANearestPart := APart
    else
    begin
      if ANearestPart = nil then
        R := TdxPDFOrientedRect.Create
      else
        R := ANearestPart.Bounds;
      if (ANearestPart = nil) or APart.Bounds.PtInRect(P) or
        (Abs(TdxPDFRecognizedTextUtils.RotatePoint(R.TopLeft, -AAngle).X + R.Width - X) >= Abs(ARotatedLeft - X)) then
        ANearestPart := APart;

      for J := 0 to Length(ANearestPart.Characters) - 1 do
      begin
        ABounds := ANearestPart.Characters[J].Bounds;
        if TdxPDFRecognizedTextUtils.RotatePoint(ABounds.TopLeft, -AAngle).X + ABounds.Width / 2 >= TdxPDFRecognizedTextUtils.RotatePoint(P, -AAngle).X then
          Exit(TdxPDFTextPosition.Create(APageIndex, ANearestPart.WordIndex, J + ANearestPart.WrapOffset));
      end;
    end;
  end;
  Result := TdxPDFTextPosition.Create(APageIndex, ANearestPart.WordIndex, ANearestPart.WrapOffset + Length(ANearestPart.Characters));
end;

function TdxPDFTextLine.GetRange(APageIndex: Integer; const R: TdxRectF): TdxPDFPageTextRange;
var
  I: Integer;
  APart: TdxPDFTextWordPart;
  AStart, AEnd: TdxPDFPageTextPosition;
begin
  AStart := TdxPDFPageTextPosition.Invalid;
  AEnd := TdxPDFPageTextPosition.Invalid;
  Result := TdxPDFPageTextRange.Invalid;
  for APart in FWordPartList do
    for I := 0 to Length(APart.Characters) - 1 do
    begin
      if TdxPDFUtils.Intersects(R, APart.Characters[I].Bounds.RotatedRect) then
      begin
        if not AStart.IsValid then
          AStart := TdxPDFPageTextPosition.Create(APart.WordIndex, I);
        AEnd := TdxPDFPageTextPosition.Create(APart.WordIndex, I + 1);
      end;
    end;
  if AStart.IsValid and AEnd.IsValid then
    Result := TdxPDFPageTextRange.Create(APageIndex, AStart, AEnd);
end;

function TdxPDFTextLine.GetHighlight(AStartWordIndex, AStartOffset, AEndWordIndex, AEndOffset: Integer;
  ASplitBounds: Boolean; ARectIndex: Integer): TdxPDFOrientedRect;
var
  AList: TdxPDFOrientedRectList;
begin
  AList := GetHighlights(AStartWordIndex, AStartOffset, AEndWordIndex, AEndOffset, ASplitBounds);
  try
    Result := AList[ARectIndex];
  finally
    AList.Free;
  end;
end;

function TdxPDFTextLine.GetHighlight(AStartWordIndex, AStartOffset: Integer; ASplitBounds: Boolean;
  ARectIndex: Integer): TdxPDFOrientedRect;
begin
  Result := GetHighlight(AStartWordIndex, AStartOffset, FWordPartList.Count - 1, -1, ASplitBounds, ARectIndex);
end;

function TdxPDFTextLine.GetHighlights(AStartWordIndex, AStartOffset, AEndWordIndex, AEndOffset: Integer;
  ASplitBounds: Boolean): TdxPDFOrientedRectList;
var
  AStartCharacters: TdxPDFTextCharacters;
  I, AStart, AEnd, J: Integer;
  AStartOnEndOfWord: Boolean;
  AStartCharRect, ACharRect: TdxPDFOrientedRect;
  AAngle, ALeft, ATop, ARight, ABottom: Single;
  ATopLeft, ACharTopLeft: TdxPointF;
  AStoredStartOffset: Integer;
begin
  if AStartWordIndex = -1 then
    AStartCharacters := FWordPartList[0].Characters
  else
    AStartCharacters := FWordPartList[AStartWordIndex].Characters;

  if AStartOffset > Length(AStartCharacters) then
    Exit(nil);
  Result := TdxPDFOrientedRectList.Create;

  AStoredStartOffset := AStartOffset;
  AStartOnEndOfWord := AStartOffset = Length(AStartCharacters);
  if AStartOnEndOfWord then
    Dec(AStartOffset);
  AStartCharRect := AStartCharacters[AStartOffset].Bounds;

  AAngle := AStartCharRect.Angle;
  ATopLeft := TdxPDFRecognizedTextUtils.RotatePoint(AStartCharRect.TopLeft, -AAngle);
  ALeft := IfThen(AStartOnEndOfWord, ATopLeft.X + AStartCharRect.Width, ATopLeft.X);
  ATop := ATopLeft.Y;
  ARight := ALeft;
  ABottom := ATop - AStartCharRect.Height;

  if AStartWordIndex = -1 then
    Inc(AStartWordIndex);
  for I := AStartWordIndex to AEndWordIndex do
  begin
    AStart := IfThen(I = AStartWordIndex, AStoredStartOffset, 0);
    AEnd := IfThen((I = AEndWordIndex) and (AEndOffset <> -1), AEndOffset, Length(FWordPartList[I].Characters));

    if ASplitBounds and ((AStart = 0) or (AStart < AEnd)) then
    begin
      ACharRect := FWordPartList[I].Characters[AStart].Bounds;
      ACharTopLeft := TdxPDFRecognizedTextUtils.RotatePoint(ACharRect.TopLeft, -AAngle);
      if ACharTopLeft.X > ARight + 2 * ACharRect.Width then
      begin
        Result.Add(TdxPDFOrientedRect.Create(TdxPDFRecognizedTextUtils.RotatePoint(dxPointF(ALeft, ATop), AAngle),
          ARight - ALeft, ATop - ABottom, AAngle));
        ALeft := ACharTopLeft.X;
        ATop := ACharTopLeft.Y;
        ARight := ALeft;
        ABottom := ATop - ACharRect.Height;
      end;
    end;
    if AEnd = 0 then
      ARight := TdxPDFUtils.Max(ARight, TdxPDFRecognizedTextUtils.RotatePoint(FWordPartList[I].Characters[0].Bounds.TopLeft, -AAngle).X)
    else
      for J := AStart to AEnd - 1 do
      begin
        ACharRect := FWordPartList[I].Characters[J].Bounds;
        ACharTopLeft := TdxPDFRecognizedTextUtils.RotatePoint(ACharRect.TopLeft, -AAngle);
        ALeft := TdxPDFUtils.Min(ACharTopLeft.X, ALeft);
        ATop := TdxPDFUtils.Max(ACharTopLeft.Y, ATop);
        ARight := TdxPDFUtils.Max(ARight, ACharTopLeft.X + ACharRect.Width);
        ABottom := TdxPDFUtils.Min(ABottom, ACharTopLeft.Y - ACharRect.Height);
      end;
  end;
  Result.Add(TdxPDFOrientedRect.Create(TdxPDFRecognizedTextUtils.RotatePoint(dxPointF(ALeft, ATop), AAngle),
    ARight - ALeft, ATop - ABottom, AAngle));
end;

function TdxPDFTextLine.GetHighlights(AStartWordIndex, AStartOffset: Integer;
  ASplitBounds: Boolean): TdxPDFOrientedRectList;
begin
  Result := GetHighlights(AStartWordIndex, AStartOffset, FWordPartList.Count - 1, -1, ASplitBounds);
end;

function TdxPDFTextLine.PositionInLine(AWordIndex: Integer; AOffset: Integer): Boolean;
var
  I, ACharCount, AWrapOffset: Integer;
begin
  for I := 0 to FWordPartList.Count - 1 do
  begin
    ACharCount := Length(FWordPartList[I].Characters);
    if AWordIndex = FWordPartList[I].WordIndex then
    begin
      if I = 0 then
      begin
        AWrapOffset := FWordPartList[I].WrapOffset;
        Exit((AWrapOffset <= AOffset) and (AOffset - AWrapOffset <= ACharCount));
      end
      else
        Exit(AOffset <= ACharCount);
    end;
  end;
  Result := False;
end;

procedure TdxPDFTextLine.PopulateWordList(AWordList: TdxPDFTextWordList);
var
  I: Integer;
  AList: TList;
begin
  FWordList.Clear;
  if AWordList <> nil then
  begin
    AList := TList.Create;
    try
      for I := 0 to AWordList.Count - 1 do
        AList.Add(AWordList[I]);
      AList.SortList(CompareWord);
      for I := 0 to AList.Count - 1 do
        FWordList.Add(AList[I]);
    finally
      AList.Free;
    end;
  end;
end;

procedure TdxPDFTextLine.PopulateWordPartList(AWordPartList: TdxPDFTextWordPartList);
begin
  FWordPartList.Clear;
  if AWordPartList <> nil then
    FWordPartList.AddRange(AWordPartList);
end;

{ TdxPDFTextLineList }

constructor TdxPDFTextLineList.Create;
begin
  inherited Create;
  FWordList := nil
end;

destructor TdxPDFTextLineList.Destroy;
begin
  FreeAndNil(FWordList);
  inherited;
end;

function TdxPDFTextLineList.Find(const APosition: TdxPDFPosition; const ATextExpansionFactor: TdxPointF;
  out ALine: TdxPDFTextLine): Boolean;
var
  ACurrentLine: TdxPDFTextLine;
  ABounds: TdxPDFOrientedRect;
  APart: TdxPDFTextWordPart;
  I: Integer;
begin
  ALine := nil;
  if APosition.PageIndex >= 0 then
    for I := 0 to Count - 1 do
    begin
      ACurrentLine := Items[I];
      ABounds := ACurrentLine.Bounds;
      if ABounds.PtInRect(APosition.Point, ATextExpansionFactor.X) then
        for APart in ACurrentLine.WordPartList do
          if APart.Bounds.PtInRect(APosition.Point, ATextExpansionFactor.X) then
          begin
            ALine := ACurrentLine;
            Exit(True);
          end;
      if ABounds.PtInRect(APosition.Point, ATextExpansionFactor.X, ATextExpansionFactor.Y) then
        for APart in ACurrentLine.WordPartList do
          if APart.Bounds.PtInRect(APosition.Point, ATextExpansionFactor.X, ATextExpansionFactor.Y) then
            ALine := ACurrentLine;
    end;
  Result := ALine <> nil;
end;

function TdxPDFTextLineList.CreateWordList: TdxPDFTextWordList;
var
  I, J: Integer;
begin
  Result := TdxPDFTextWordList.Create;
  for J := 0 to Count - 1 do
    for I := 0 to Items[J].WordList.Count - 1 do
      Result.Add(Items[J].WordList[I]);
end;

function TdxPDFTextLineList.GetWordList: TdxPDFTextWordList;
begin
  if FWordList = nil then
    FWordList := CreateWordList;
  Result := FWordList;
end;

{ TdxPDFTextBlock }

constructor TdxPDFTextBlock.Create(AData: TdxPDFStringData; AFontData: TdxPDFFontData; AState: TdxPDFTextState;
  ATextWidthFactor, ATextHeightFactor: Single; const AGlyphOffsets: TDoubleDynArray; ALocation: TdxPointF; AAngle: Single);
var
  ABounds: TdxPDFOrientedRect;
  ALength, I, AOffset, ACount: Integer;
  AFont: IdxPDFFont;
  ARotatedLocation, ATopLeft: TdxPointF;
  X, Y, ACos, ASin, AGlyphOffset, AHeight: Single;
  AAbsoluteFontSize, AHorizontalScaling, AUnscaledUnitsFactor: Double;
  AHorizontalFontFactor, AVerticalFontFactor, ACharacterWidth: Double;
begin
  FFontData := AFontData;
  AFont := TdxPDFFontDataAccess(FFontData).Font as TdxPDFCustomFont;
  AAbsoluteFontSize := AState.AbsoluteFontSize * ATextHeightFactor;
  AHorizontalScaling := AState.HorizontalScaling / 100;
  AUnscaledUnitsFactor := AHorizontalScaling * ATextWidthFactor;
  AHorizontalFontFactor := AState.FontSize * AFont.GetWidthFactor * AUnscaledUnitsFactor;
  AVerticalFontFactor := AAbsoluteFontSize * AFont.GetHeightFactor;
  FCharacterSpacing := AState.CharacterSpacing * AUnscaledUnitsFactor;
  FFontDescent := TdxPDFFontDataAccess(AFontData).Descent * AVerticalFontFactor;
  FFontHeight := TdxPDFFontDataAccess(AFontData).Height * AVerticalFontFactor;
  FLocation := ALocation;
  FAngle := AAngle;
  X := ALocation.X;
  Y := ALocation.Y;
  ALength := Length(AGlyphOffsets);
  SetLength(FCharLocations, ALength);
  FCharLocations[0] := dxPointF(X, Y);
  ACos := Cos(AAngle);
  ASin := Sin(AAngle);
  AOffset := 0;
  for I := 1 to ALength - 1 do
  begin
    AGlyphOffset := AGlyphOffsets[AOffset];
    Inc(AOffset);
    FCharLocations[I] := dxPointF(X + AGlyphOffset * ACos, Y + AGlyphOffset * ASin);
  end;
  ACount := Length(AData.CharacterCodes);
  AHeight := FontHeight + FontDescent;
  SetLength(FCharacters, ACount);
  for I := 0 to ACount - 1 do
  begin
    ARotatedLocation := TdxPDFRecognizedTextUtils.RotatePoint(FCharLocations[I], -AAngle);
    ACharacterWidth := CalculateCharacterWidth(AData.Glyphs[I], AHorizontalFontFactor);
    ATopLeft := TdxPDFRecognizedTextUtils.RotatePoint(dxPointF(ARotatedLocation.X, ARotatedLocation.Y + AHeight), AAngle);
    ABounds := TdxPDFOrientedRect.Create(ATopLeft, ACharacterWidth, FontHeight, AAngle);
    FCharacters[I].Initialize(ABounds, GetActualCharacter(AData.CharacterCodes[I]), AAbsoluteFontSize);
  end;
end;

function TdxPDFTextBlock.CalculateCharacterWidth(ACharacter: Integer; AHorizontalFontFactor: Single): Single;
var
  AIndex: Integer;
begin
  AIndex := ACharacter - TdxPDFFontDataAccess(FFontData).FirstChar;
  if (TdxPDFFontDataAccess(FFontData).Widths = nil) or (AIndex < 0) or ((TdxPDFFontDataAccess(FFontData).Widths <> nil)
    and (AIndex > TdxPDFFontDataAccess(FFontData).Widths.Count - 1)) then
    Result := TdxPDFFontDataAccess(FontData).MissingWidth
  else
    Result := TdxPDFFontDataAccess(FontData).Widths[AIndex];
  Result := Result * AHorizontalFontFactor;
end;

function TdxPDFTextBlock.GetActualCharacter(const ACharacter: TBytes): string;
var
  AToUnicode: TdxPDFCharacterMapping;
  AFont: TdxPDFSimpleFont;
  ACode: Word;
  ACodeLength, I: Integer;
begin
  AToUnicode := TdxPDFFontDataAccess(FFontData).CharacterMapping;
  if AToUnicode <> nil then
    Exit(AToUnicode.MapCode(ACharacter));
  if  TdxPDFFontDataAccess(FontData).Font is TdxPDFSimpleFont then
    AFont := TdxPDFSimpleFont(TdxPDFFontDataAccess(FFontData).Font)
  else
    AFont := nil;
  if AFont = nil then
    Exit(TdxPDFUtils.ConvertToStr(ACharacter, Length(ACharacter)));
  ACode := 0;
  ACodeLength := Length(ACharacter);
  for I := 0 to ACodeLength - 1 do
    ACode := Word((ACode shl 8) + ACharacter[I]);
  Result := Char(TdxPDFUnicodeConverter.GetGlyphCode(AFont.Encoding as TdxPDFSimpleFontEncoding, ACode));
end;

{ TdxPDFTextParserState }

constructor TdxPDFTextParserState.Create(ABlocks: TList<TdxPDFTextBlock>; APageCropBox: TdxRectF);
begin
  inherited Create;
  FActualRTL := False;
  FBlocks := ABlocks;
  FPageCropBox := APageCropBox;
  FCurrentCharacterBlock := ABlocks[0];
  FPreviousCharacterBlock := FCurrentCharacterBlock;
  FCurrentCharacter := FCurrentCharacterBlock.Characters[0];
  FPreviousCharacter := FCurrentCharacter;
  FCurrentCharLocation := FCurrentCharacterBlock.CharLocations[0];
  FPreviousCharLocation := FCurrentCharLocation;
end;

function TdxPDFTextParserState.IsIndex: Boolean;
var
  AOverlapFactor, AIndexHeightRatio, ABlock1FontHeight, ABlock2FontHeight: Double;
  ABlock1Angle, ABlock2Angle, ARowBlockHeight, AIndexBlockHeight, ARowBlockAngle: Double;
  AIndexBlockAngle, AOverlapValue, AActualOverlap: Double;
  ABlock1TopLeft, ABlock2TopLeft, ARowBlockTopLeft, AIndexBlockTopLeft: TdxPointF;
  ARotatedRowBlockTopLeft, ARotatedIndexBlockTopLeft: TdxPointF;
begin
  AOverlapFactor := 0.35;
  AIndexHeightRatio := 1.3;
  ABlock1FontHeight := FPreviousCharacterBlock.FontHeight;
  ABlock2FontHeight := FCurrentCharacterBlock.FontHeight;
  ABlock1TopLeft := FPreviousCharacter.Bounds.TopLeft;
  ABlock2TopLeft := FCurrentCharacter.Bounds.TopLeft;
  ABlock1Angle := FPreviousCharacterBlock.Angle;
  ABlock2Angle := FCurrentCharacterBlock.Angle;
  if (ABlock1FontHeight / AIndexHeightRatio > ABlock2FontHeight) or (ABlock2FontHeight / AIndexHeightRatio > ABlock1FontHeight) then
  begin
    ARowBlockTopLeft := ABlock1TopLeft;
    AIndexBlockTopLeft := ABlock2TopLeft;
    ARowBlockHeight := ABlock1FontHeight;
    AIndexBlockHeight := ABlock2FontHeight;
    ARowBlockAngle := ABlock1Angle;
    AIndexBlockAngle := ABlock2Angle;
    if ABlock1FontHeight < ABlock2FontHeight then
    begin
      ARowBlockTopLeft := ABlock2TopLeft;
      AIndexBlockTopLeft := ABlock1TopLeft;
      ARowBlockHeight := ABlock2FontHeight;
      AIndexBlockHeight := ABlock1FontHeight;
      ARowBlockAngle := ABlock2Angle;
      AIndexBlockAngle := ABlock1Angle;
    end;
    AOverlapValue := AIndexBlockHeight * AOverlapFactor;
    ARotatedRowBlockTopLeft := TdxPDFRecognizedTextUtils.RotatePoint(ARowBlockTopLeft, -ARowBlockAngle);
    ARotatedIndexBlockTopLeft := TdxPDFRecognizedTextUtils.RotatePoint(AIndexBlockTopLeft, -AIndexBlockAngle);
    AActualOverlap := TdxPDFUtils.Min(ARotatedRowBlockTopLeft.Y, ARotatedIndexBlockTopLeft.Y) -
      TdxPDFUtils.Max(ARotatedRowBlockTopLeft.Y - ARowBlockHeight, ARotatedIndexBlockTopLeft.Y - AIndexBlockHeight);
    Exit(AActualOverlap >= AOverlapValue);
  end;
  Result := False;
end;

function TdxPDFTextParserState.CheckHorizontalPosition: Boolean;
var
  ADistanceFactor, AAvgWidth, AAngle, ADistanceFromLeft, ADistanceFromRight, ADistance: Double;
  ACurrentCharRectangle, APreviousCharRectangle: TdxPDFOrientedRect;
  AWordLeft, AWordRight, APreviousCharPosition: TdxPointF;
begin
  ADistanceFactor := 0.3;
  ACurrentCharRectangle := FCurrentCharacter.Bounds;
  APreviousCharRectangle := FPreviousCharacter.Bounds;
  AAvgWidth := ADistanceFactor * (ACurrentCharRectangle.Width + APreviousCharRectangle.Width) / 2;
  if FIsRtl then
  begin
    AWordLeft := FCurrentWordLeftBoundary;
    AWordRight := FCurrentWordRightBoundary;
    AAngle := ACurrentCharRectangle.Angle;
    ADistanceFromLeft := TdxPDFRecognizedTextUtils.GetOrientedDistance(ACurrentCharRectangle.TopRight, AWordLeft, AAngle);
    ADistanceFromRight := TdxPDFRecognizedTextUtils.GetOrientedDistance(AWordRight, ACurrentCharRectangle.TopLeft, AAngle);
    Result := (ADistanceFromLeft > AAvgWidth) or (ADistanceFromRight > AAvgWidth);
  end
  else
  begin
    APreviousCharPosition := FPreviousCharLocation;
    ADistance := TdxPDFRecognizedTextUtils.GetOrientedDistance(FPreviousCharLocation, FCurrentCharLocation, FPreviousCharacterBlock.Angle) -
      APreviousCharRectangle.Width;
    if ADistance < -0.1 then
      Result := Abs(ADistance) > APreviousCharRectangle.Width + AAvgWidth
    else
      Result := ADistance > AAvgWidth;
    if Result then
    begin
      Result := FPreviousCharacterBlock.CharacterSpacing = 0;
      if not Result then
         Result := Abs(ADistance / FPreviousCharacterBlock.CharacterSpacing) > (1 + ADistanceFactor);
    end;
  end;
end;

function TdxPDFTextParserState.CheckVerticalPosition: Boolean;
var
  ALineOverlapFactor, APreviousBlockAngle, ACurrentBlockAngle, AVerticalDifference, ABlockFontHeight: Double;
  APreviousBlockLocation, ACurrentBlockLocation: TdxPointF;
begin
  ALineOverlapFactor := 0.7;
  APreviousBlockLocation := FPreviousCharacterBlock.Location;
  ACurrentBlockLocation := FCurrentCharacterBlock.Location;
  APreviousBlockAngle := FPreviousCharacterBlock.Angle;
  ACurrentBlockAngle := FCurrentCharacterBlock.Angle;
  AVerticalDifference := Abs(TdxPDFRecognizedTextUtils.RotatePoint(APreviousBlockLocation, -APreviousBlockAngle).Y -
    TdxPDFRecognizedTextUtils.RotatePoint(ACurrentBlockLocation, -ACurrentBlockAngle).Y);
  ABlockFontHeight := TdxPDFUtils.Min(FCurrentCharacterBlock.FontHeight, FPreviousCharacterBlock.FontHeight);
  Result := (AVerticalDifference > ABlockFontHeight * (1 - ALineOverlapFactor)) and not IsIndex;
end;

function TdxPDFTextParserState.MoveToNextCharacter: Boolean;
begin
  FPreviousCharacter := FCurrentCharacter;
  FPreviousCharLocation := FCurrentCharLocation;
  Inc(FCharacterIndex);
  if FCharacterIndex >= Length(FCurrentCharacterBlock.Characters) then
    Exit(False);
  FPreviousCharacterBlock := FCurrentCharacterBlock;
  FCurrentCharacter := FCurrentCharacterBlock.Characters[FCharacterIndex];
  FCurrentCharLocation := FCurrentCharacterBlock.CharLocations[FCharacterIndex];
  Result := True;
end;

function TdxPDFTextParserState.MoveToNextBlock: Boolean;
begin
  FPreviousCharacterBlock := FCurrentCharacterBlock;
  Inc(FBlockIndex);
  if FBlockIndex >= FBlocks.Count then
    Exit(False);
  FCurrentCharacterBlock := FBlocks[FBlockIndex];
  FCharacterIndex := 0;
  FPreviousCharacter := FCurrentCharacter;
  FPreviousCharLocation := FCurrentCharLocation;
  FCurrentCharLocation := FCurrentCharacterBlock.CharLocations[FCharacterIndex];
  FCurrentCharacter := FCurrentCharacterBlock.Characters[FCharacterIndex];
  Result := True;
end;

procedure TdxPDFTextParserState.MoveNext;
var
  ACurrentCharUnicode, APrevCharUnicode: string;
  APreviousBlockCharacters: TdxPDFTextCharacters;
begin
  if not MoveToNextCharacter then
    if not MoveToNextBlock then
    begin
      FIsFinished := True;
      Exit;
    end;
  ACurrentCharUnicode := FCurrentCharacter.Text;
  APrevCharUnicode := FPreviousCharacter.Text;

  FIsRtl := TdxPDFRecognizedTextUtils.HasRTLMarker(ACurrentCharUnicode) or TdxPDFRecognizedTextUtils.HasRTLMarker(APrevCharUnicode);
  FIsCJKWord := TdxPDFRecognizedTextUtils.HasCJKMarker(ACurrentCharUnicode);
  FIsLineStart := (FCurrentCharacterBlock.Angle <> FPreviousCharacterBlock.Angle) or CheckVerticalPosition;
  FIsSpace := FIsLineStart or TdxPDFRecognizedTextUtils.IsWhitespace(ACurrentCharUnicode) or CheckHorizontalPosition;
  FIsSeparator := TdxPDFRecognizedTextUtils.IsSeparator(APrevCharUnicode);
  APreviousBlockCharacters := FPreviousCharacterBlock.Characters;
  FIsWrap := FIsLineStart and TdxPDFRecognizedTextUtils.IsWrapSymbol(APreviousBlockCharacters[Length(APreviousBlockCharacters) - 1].Text) and
    (FCurrentCharacterBlock.Location.Y <= FPreviousCharacterBlock.Location.Y);
end;

{ TdxPDFPageWordBuilder }

constructor TdxPDFPageWordBuilder.Create(AParserState: TdxPDFTextParserState);
begin
  inherited Create;
  FParserState := AParserState;
end;

destructor TdxPDFPageWordBuilder.Destroy;
begin
  FreeAndNil(FWordPartList);
  inherited Destroy;
end;

function TdxPDFPageWordBuilder.BuildWord(ALineXOffset: Single): TdxPDFTextWord;
var
  APartRectangle: TdxPDFOrientedRect;
  APartText: string;
  AWordWrap: Boolean;
  AWordPart: TdxPDFTextWordPart;
begin
  ClearWordPartData;
  FWrapOffset := 0;
  FreeAndNil(FWordPartList);
  FWordPartList := TdxPDFTextWordPartList.Create;
  Result := nil;
  while Result = nil do
  begin
    ProcessChar;
    FParserState.MoveNext;
    if FParserState.IsSpace or FParserState.IsSeparator or FParserState.IsFinished or FParserState.IsCJKWord then
    begin
      APartRectangle := TdxPDFOrientedRect.Create(TdxPDFRecognizedTextUtils.RotatePoint(dxPointF(FWordMinX, FWordMaxY), FWordAngle), FWordMaxX - FWordMinX, FWordMaxY - FWordMinY, FWordAngle);
      AWordWrap := FParserState.IsWrap and (ALineXOffset <= APartRectangle.Left);
      AWordPart := TdxPDFTextWordPart.Create(APartRectangle, FCharacters, FWrapOffset, not AWordWrap and (FParserState.IsSpace or FParserState.IsFinished));
      if not AWordPart.IsEmpty then
      begin
        FWordPartList.Add(AWordPart);
        if (AWordWrap and (Length(AWordPart.Text) > 1)) and not FParserState.IsFinished then
        begin
          FWrapOffset := FWrapOffset + Length(APartText);
          ClearWordPartData;
        end
        else
          Result := TdxPDFTextWord.Create(FWordPartList);
      end
      else
      begin
        AWordPart.Free;
        Break;
      end;
    end
  end;
end;

procedure TdxPDFPageWordBuilder.ClearWordPartData;
begin
  SetLength(FCharacters, 0);
  FWordMinX := 0;
  FWordMaxX := 0;
  FWordMinY := 0;
  FWordMaxY := 0;
  FWordAngle := 0;
end;

procedure TdxPDFPageWordBuilder.ProcessChar;
var
  L: Integer;
  ACurrentChar: TdxPDFTextCharacter;
  AUnicodeData: string;
  ABlock: TdxPDFTextBlock;
  ABlockAngle, AMinY, AMaxY, ACharWidth: Single;
  ACharLocation: TdxPointF;
  ACharBox: TdxRectF;
begin
  ACurrentChar := FParserState.CurrentCharacter;
  AUnicodeData := ACurrentChar.Text;
  if ((AUnicodeData <> #$A0) and (AUnicodeData <> ' ')) and (AUnicodeData <> #9) then
  begin
    ABlock := FParserState.CurrentCharacterBlock;
    ABlockAngle := ABlock.Angle;
    ACharLocation := TdxPDFRecognizedTextUtils.RotatePoint(FParserState.CurrentCharLocation, -ABlockAngle);
    AMinY := TdxPDFRecognizedTextUtils.RotatePoint(ABlock.Location, -ABlockAngle).Y + ABlock.FontDescent;
    AMaxY := AMinY + ABlock.FontHeight;
    if Length(FCharacters) = 0 then
    begin
      FWordMinX := ACharLocation.X;
      FWordMinY := AMinY;
      FWordMaxY := AMaxY;
      FWordAngle := ABlockAngle;
    end
    else
    begin
      FWordMinY := Math.Min(FWordMinY, AMinY);
      FWordMaxY := Math.Max(FWordMaxY, AMaxY);
    end;
    ACharWidth := ACurrentChar.Bounds.RotatedRect.Width;
    if ACharWidth <> 0 then
    begin
      ACharBox := ACurrentChar.Bounds.RotatedRect;
      if not TdxPDFUtils.IsRectEmpty(ACharBox) and TdxPDFUtils.Intersects(FParserState.PageCropBox, ACharBox) then
      begin
        FWordMaxX := Max(FWordMaxX, ACharLocation.X + ACharWidth);
        FWordMinX := Min(ACharLocation.X, FWordMinX);
        L := Length(FCharacters);
        SetLength(FCharacters, L + 1);
        FCharacters[L] := ACurrentChar;
      end;
    end;
    FParserState.CurrentWordLeftBoundary := dxPointF(FWordMinX, FWordMaxX);
    FParserState.CurrentWordRightBoundary := dxPointF(FWordMaxX, FWordMaxY);
  end;
end;

{ TdxPDFPageTextLineBuilder }

constructor TdxPDFPageTextLineBuilder.Create(AParserState: TdxPDFTextParserState);
begin
  inherited Create;
  FWordIndex := 1;
  FLineXOffset := MinSingle;
  FParserState := AParserState;

  FWordList := TdxPDFTextWordList.Create;
  FWordPartList := TdxPDFTextWordPartList.Create;
  FWordBuilder := TdxPDFPageWordBuilder.Create(AParserState);
end;

destructor TdxPDFPageTextLineBuilder.Destroy;
begin
  FreeAndNil(FWordBuilder);
  FreeAndNil(FWordPartList);
  FreeAndNil(FWordList);
  inherited Destroy;
end;

procedure TdxPDFPageTextLineBuilder.Populate(ATextLines: TdxPDFTextLineList);
var
  ANextWord: TdxPDFTextWord;
  AWordParts: TdxPDFTextWordPartList;
  ATempWordParts: TdxPDFTextWordPartList;
  APartsCount: Integer;
  I: Integer;
begin
  while True do
  begin
    ANextWord := FWordBuilder.BuildWord(FLineXOffset);
    if ANextWord <> nil then
    begin
      if not OverlapsWithPreviousWords(ANextWord) then
      begin
        FWordList.Add(ANextWord);
        AWordParts := ANextWord.PartList;
        FLineXOffset := Math.Max(FLineXOffset, AWordParts[0].Bounds.Right);
        APartsCount := AWordParts.Count;
        if APartsCount > 1 then
        begin
          EnumerateWordsAndFillParts;
          AddTextLine(ATextLines, FWordList, FWordPartList);
          for I := 1 to APartsCount - 2 do
          begin
            ATempWordParts := TdxPDFTextWordPartList.Create;
            ATempWordParts.Add(AWordParts[I]);
            AddTextLine(ATextLines, nil, ATempWordParts);
          end;
          Clear;
          FWordPartList.Add( AWordParts[APartsCount - 1]);
          FLineXOffset := MinSingle;
        end;
      end
      else
        ANextWord.Free;
    end;
    if (FParserState.IsLineStart) or (FParserState.IsFinished) then
    begin
      if (FWordList.Count <> 0) or (FWordPartList.Count <> 0) then
      begin
        EnumerateWordsAndFillParts;
        AddTextLine(ATextLines, FWordList, FWordPartList);
        if FParserState.IsFinished then
          Break
        else
        begin
          Clear;
          Continue;
        end;
      end;
      if FParserState.IsFinished then
        Break;
      FLineXOffset := MinSingle;
    end;
  end;
end;

function TdxPDFPageTextLineBuilder.OverlapsWithPreviousWords(AWord: TdxPDFTextWord): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FWordList.Count - 1 do
    if FWordList[I].Overlap(AWord) then
      Exit(True);
end;

procedure TdxPDFPageTextLineBuilder.AddTextLine(ATextLines: TdxPDFTextLineList; AWordList: TdxPDFTextWordList;
  APartList: TdxPDFTextWordPartList);
var
  ALine: TdxPDFTextLine;
begin
  ALine := TdxPDFTextLine.Create;
  ALine.PopulateWordList(AWordList);
  ALine.PopulateWordPartList(APartList);
  ATextLines.Add(ALine);
end;

procedure TdxPDFPageTextLineBuilder.Clear;
begin
  RecreateWordList;
  RecreateWordPartList;
end;

procedure TdxPDFPageTextLineBuilder.EnumerateWordsAndFillParts;
var
  I: Integer;
begin
  for I := 0 to FWordList.Count - 1 do
  begin
    FWordList[I].Index := FWordIndex;
    FWordPartList.Add(FWordList[I].PartList[0]);
    Inc(FWordIndex);
  end;
end;

procedure TdxPDFPageTextLineBuilder.RecreateWordList;
begin
  FreeAndNil(FWordList);
  FWordList := TdxPDFTextWordList.Create;
end;

procedure TdxPDFPageTextLineBuilder.RecreateWordPartList;
begin
  FreeAndNil(FWordPartList);
  FWordPartList := TdxPDFTextWordPartList.Create;
end;

{ TdxPDFRecognizedTextUtils }

class procedure TdxPDFRecognizedTextUtils.AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart);
begin
  AppendWordPart(ABuilder, APart, 0, Length(APart.Characters));
end;

class procedure TdxPDFRecognizedTextUtils.AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart;
  AStartOffset: Integer);
begin
  AppendWordPart(ABuilder, APart, AStartOffset, Length(APart.Characters));
end;

class procedure TdxPDFRecognizedTextUtils.AppendWordPart(ABuilder: TdxBiDiStringBuilder; APart: TdxPDFTextWordPart;
  AStartOffset, AEndOffset: Integer);
var
  I: Integer;
begin
  for I := AStartOffset to AEndOffset - 1 do
    ABuilder.Append(APart.Characters[I].Text);
end;

class procedure TdxPDFRecognizedTextUtils.Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition;
  AWordPart: TdxPDFTextWordPart);
var
  AWordIndex, APartLength, AStartOffset, AEndOffset: Integer;
  AWithoutStartWord, AWithoutEndWord: Boolean;
begin
  AWithoutStartWord := AStart.WordIndex = 0;
  AWithoutEndWord := AEnd.WordIndex = 0;
  AWordIndex := AWordPart.WordIndex;
  APartLength := Length(AWordPart.Text);

  AStartOffset := Max(AStart.Offset - AWordPart.WrapOffset, 0);
  AEndOffset := Min(AEnd.Offset - AWordPart.WrapOffset, APartLength);

  if AWithoutStartWord and AWithoutEndWord or AWithoutStartWord and (AWordIndex < AEnd.WordIndex) or  AWithoutEndWord and
  (AWordIndex > AStart.WordIndex) or (AWordIndex > AStart.WordIndex) and (AWordIndex < AEnd.WordIndex) then
  begin
    AppendWordPart(ABuilder, AWordPart);
    if AWordPart.WordEnded then
      ABuilder.Append(' ');
  end
  else
    if (AWordIndex = AEnd.WordIndex) then
    begin
      if AWordIndex = AStart.WordIndex then
      begin
        if (AStart.Offset = 0) and (AEnd.Offset < 0) then
          AppendWordPart(ABuilder, AWordPart)
        else
          if (AStartOffset <= APartLength) and (AEndOffset >= 0) then
            AppendWordPart(ABuilder, AWordPart, AStartOffset, AEndOffset);
      end
      else
        if AWithoutStartWord or (AWordIndex > AStart.WordIndex) then
        begin
          if AEnd.Offset < 0 then
            AppendWordPart(ABuilder, AWordPart)
          else
            if AEndOffset >= 0 then
              AppendWordPart(ABuilder, AWordPart, 0, AEndOffset);
        end;
    end
    else
      if (AWithoutEndWord or (AWordIndex < AEnd.WordIndex)) and (AWordIndex = AStart.WordIndex) and (AStartOffset <= APartLength) then
      begin
        AppendWordPart(ABuilder, AWordPart, AStartOffset);
        if AWordPart.WordEnded then
          ABuilder.Append(' ');
      end;
end;

class function TdxPDFRecognizedTextUtils.ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition;
  AWordPart: TdxPDFTextWordPart): string;
var
  ABuilder: TdxBiDiStringBuilder;
begin
  ABuilder := TdxBiDiStringBuilder.Create;
  try
    Append(ABuilder, AStart, AEnd, AWordPart);
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class procedure TdxPDFRecognizedTextUtils.Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition;
  ALine: TdxPDFTextLine);
var
  ALastWordIndex: Integer;
  ANeedNewLine: Boolean;
  APart: TdxPDFTextWordPart;
begin
  ANeedNewLine := False;
  for APart in ALine.WordPartList do
    try
      ANeedNewLine := (APart.WordIndex = AEnd.WordIndex) and ((AEnd.Offset = Length(APart.Characters)) or (AEnd.Offset < 0));
      Append(ABuilder, AStart, AEnd, APart);
    except
    end;
  ALastWordIndex := ALine.WordPartList.Last.WordIndex;
  if not ABuilder.Empty and (ALastWordIndex >= AStart.WordIndex) and
    ((ALastWordIndex < AEnd.WordIndex) or ANeedNewLine or (AEnd.WordIndex = 0)) then
    ABuilder.AppendLine;
end;

class procedure TdxPDFRecognizedTextUtils.Append(ABuilder: TdxBiDiStringBuilder; const AStart, AEnd: TdxPDFPageTextPosition;
  ALines: TdxPDFTextLineList);
var
  ALine: TdxPDFTextLine;
begin
  for ALine in ALines do
    Append(ABuilder, AStart, AEnd, ALine);
end;

class function TdxPDFRecognizedTextUtils.ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition; ALine: TdxPDFTextLine): string;
var
  ABuilder: TdxBiDiStringBuilder;
  AWordPart: TdxPDFTextWordPart;
begin
  ABuilder := TdxBiDiStringBuilder.Create;
  try
    for AWordPart in ALine.WordPartList do
      try
        Append(ABuilder, AStart, AEnd, AWordPart);
      except
      end;
    Result := ABuilder.EndCurrentLineAndGetString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxPDFRecognizedTextUtils.ConvertToString(const AStart, AEnd: TdxPDFPageTextPosition; ALines: TdxPDFTextLineList): string;
var
  ABuilder: TdxBiDiStringBuilder;
begin
  ABuilder := TdxBiDiStringBuilder.Create;
  try
    Append(ABuilder, AStart, AEnd, ALines);
    Result := ABuilder.EndCurrentLineAndGetString;
  finally
    ABuilder.Free;
  end;
end;

end.
