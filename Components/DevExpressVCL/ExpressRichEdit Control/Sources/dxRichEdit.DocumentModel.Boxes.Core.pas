{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.DocumentModel.Boxes.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}
{.$DEFINE DXLOGGING}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxHash, cxGeometry,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.Core;

type
  TdxBoxInfo = class;

  { IdxObjectMeasurer }

  IdxObjectMeasurer = interface
    procedure MeasureText(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo);
    function MeasureRectangularObject(const AObject: IdxRectangularObject): TSize;
    procedure BeginTextMeasure;
    procedure EndTextMeasure;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; const AText: string; AFontInfo: TdxFontInfo; AMaxWidth: Integer): Boolean;
  end;

  { TdxTextViewInfo }

  TdxTextViewInfo = class
  strict private
    FSize: TSize;
  public
    property Size: TSize read FSize write FSize;
  end;

  { TdxTextViewInfoCache }

  TdxTextViewInfoCache = class
  {$REGION 'internal types'}
  strict protected type
    TItem = class
    strict private
      FHash: Cardinal;
      FText: string;
      FFontInfo: TdxFontInfo;
      FTextViewInfo: TdxTextViewInfo;
      FNext: TItem;
    public
      constructor Create(const AText: string; AFontInfo: TdxFontInfo; ATextInfo: TdxTextViewInfo);
      destructor Destroy; override;
      function Equals(AEntry: TItem): Boolean; reintroduce; overload;
      function Equals(AHash: Cardinal; const AText: string; AFontInfo: TdxFontInfo): Boolean; reintroduce; overload;
      class function CalcHash(const AText: string; AFontInfo: TdxFontInfo): Cardinal; static;

      property Hash: Cardinal read FHash;
      property Text: string read FText;
      property FontInfo: TdxFontInfo read FFontInfo;
      property TextViewInfo: TdxTextViewInfo read FTextViewInfo;
      property Next: TItem read FNext write FNext;
    end;
    TItemArray = array of TItem;
  {$ENDREGION}
  strict private
    FCount: Integer;
    FTable: TItemArray;
    FTableSize: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure AddTextViewInfo(const AText: string; AFontInfo: TdxFontInfo; ATextInfo: TdxTextViewInfo);
    function TryGetTextViewInfo(const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo;

    property Count: Integer read FCount;
  end;

  { TdxBoxBase }

  TdxBoxBase = class(TdxReferencedObject)
  strict protected
    FBounds: TRect;
    function GetActualSizeBounds: TRect; virtual;
    function GetDetailsLevel: TdxDocumentLayoutDetailsLevel; virtual; abstract;
    function GetEndPos: TdxFormatterPosition; virtual; abstract;
    function GetHitTestAccuracy: TdxHitTestAccuracy; virtual; abstract;
    function GetStartPos: TdxFormatterPosition; virtual; abstract;
    procedure SetEndPos(const Value: TdxFormatterPosition); virtual; abstract;
    procedure SetStartPos(const Value: TdxFormatterPosition); virtual; abstract;
  public
    function GetFirstFormatterPosition: TdxFormatterPosition; virtual; abstract;
    function GetFirstPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; virtual; abstract;
    function GetLastFormatterPosition: TdxFormatterPosition; virtual; abstract;
    function GetLastPosition(APieceTable: TdxCustomPieceTable): TdxDocumentModelPosition; virtual; abstract;
    procedure MoveVertically(ADeltaY: Integer); virtual;

    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; virtual;
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; virtual; abstract;

    function IsLineBreak: Boolean; virtual; abstract;
    function IsSpaceBox: Boolean; virtual; abstract;
    function IsNotWhiteSpaceBox: Boolean; virtual; abstract;
    function IsVisible: Boolean; virtual; abstract;
    function IsInlinePicture: Boolean; virtual;
    function IsHyperlinkSupported: Boolean; virtual;

    property ActualSizeBounds: TRect read GetActualSizeBounds;
    property Bounds: TRect read FBounds write FBounds;
    property DetailsLevel: TdxDocumentLayoutDetailsLevel read GetDetailsLevel;
    property EndPos: TdxFormatterPosition read GetEndPos write SetEndPos;
    property HitTestAccuracy: TdxHitTestAccuracy read GetHitTestAccuracy;
    property StartPos: TdxFormatterPosition read GetStartPos write SetStartPos;
  end;
  TdxBoxListBase = class(TdxReferencedObjectList<TdxBoxBase>);

  { TdxBoxBaseComparer }

  TdxBoxBaseComparer = class abstract(TdxComparer<TdxBoxBase>);

  { TdxHighlightArea }

  TdxHighlightArea = record
  strict private
    FBounds: TRect;
    FColor: TdxAlphaColor;
    FIsNull: Boolean;
  private
    function GetBounds: TRect;
    class function GetNull: TdxHighlightArea; static;
  public
    constructor Create(AColor: TdxAlphaColor; const ABounds: TRect);

    procedure MoveVertically(ADeltaY: Integer);

    property Bounds: TRect read GetBounds;
    property Color: TdxAlphaColor read FColor;
    property IsNull: Boolean read FIsNull;
    class property Null: TdxHighlightArea read GetNull;
  end;

  { TdxHighlightAreaCollection }

  TdxHighlightAreaCollection = class(TList<TdxHighlightArea>)
  public
    procedure MoveVertically(ADeltaY: Integer);
  end;

  { TdxRowBoxRange }

  TdxRowBoxRange = class
  strict private
    FFirstBoxIndex: Integer;
    FLastBoxIndex: Integer;
    FBounds: TRect;
  public
    constructor Create(AFirstBoxIndex, ALastBoxIndex: Integer; const ABounds: TRect);

    property FirstBoxIndex: Integer read FFirstBoxIndex write FFirstBoxIndex;
    property LastBoxIndex: Integer read FLastBoxIndex write FLastBoxIndex;
    property Bounds: TRect read FBounds write FBounds;
  end;
  TdxRowBoxRangeCollection = class sealed(TdxObjectList<TdxRowBoxRange>);

  { TdxBoxInfo }

  TdxBoxInfo = class
  strict private
    FIteratorResult: TdxParagraphIteratorResult;
    FStartPos: TdxFormatterPosition;
    FEndPos: TdxFormatterPosition;
    FSize: TSize;
    FBox: TdxBoxBase;
  protected
    function GetForceUpdateCurrentRowHeight: Boolean; virtual;
  public
    function GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase; virtual;
    function GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo; virtual;

    property StartPos: TdxFormatterPosition read FStartPos write FStartPos;
    property EndPos: TdxFormatterPosition read FEndPos write FEndPos;
    property IteratorResult: TdxParagraphIteratorResult read FIteratorResult write FIteratorResult;
    property Size: TSize read FSize write FSize;
    property ForceUpdateCurrentRowHeight: Boolean read GetForceUpdateCurrentRowHeight;
    property Box: TdxBoxBase read FBox write FBox;
  end;

  { TdxBoxComparable }

  TdxBoxComparable = class(TcxIUnknownObject, IdxComparable<TdxBoxBase>)
  public
    function CompareTo(const Value: TdxBoxBase): Integer; virtual; abstract;
  end;

  { TdxBoxAndPointXComparable }

  TdxBoxAndPointXComparable = class(TdxBoxComparable)
  strict private
    FX: Integer;
  public
    constructor Create(const P: TPoint);
    function CompareTo(const Value: TdxBoxBase): Integer; override; final;

    property X: Integer read FX;
  end;

  { TdxBoxAndPointYComparable }

  TdxBoxAndPointYComparable = class(TdxBoxComparable)
  strict private
    FY: Integer;
  public
    constructor Create(const P: TPoint);
    function CompareTo(const Value: TdxBoxBase): Integer; override; final;

    property Y: Integer read FY;
  end;

  { TdxBoxLogPositionComparable }

  TdxBoxLogPositionComparable = class abstract(TdxBoxComparable)
  strict private
    FPieceTable: TdxCustomPieceTable;
    FLogPosition: TdxDocumentLogPosition;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition);

    property PieceTable: TdxCustomPieceTable read FPieceTable;
    property LogPosition: TdxDocumentLogPosition read FLogPosition;
  end;

  { TdxBoxAndLogPositionComparable }

  TdxBoxAndLogPositionComparable = class(TdxBoxLogPositionComparable)
  public
    function CompareTo(const Value: TdxBoxBase): Integer; override; final;
  end;

  { TdxBoxAndFormatterPositionComparable }

  TdxBoxAndFormatterPositionComparable = class(TdxBoxComparable)
  strict private
    FFormatterPosition: TdxFormatterPosition;
  public
    constructor Create(const AFormatterPosition: TdxFormatterPosition);
    function CompareTo(const Value: TdxBoxBase): Integer; override;

    property FormatterPosition: TdxFormatterPosition read FFormatterPosition;
  end;

  { TdxBoxStartAndLogPositionComparable }

  TdxBoxStartAndLogPositionComparable = class(TdxBoxLogPositionComparable)
  public
    function CompareTo(const Value: TdxBoxBase): Integer; override; final;
  end;

implementation

uses
  Math, RTLConsts,
  dxTypeHelpers,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter;

{ TdxTextViewInfoCache.TItem }

constructor TdxTextViewInfoCache.TItem.Create(const AText: string; AFontInfo: TdxFontInfo;
  ATextInfo: TdxTextViewInfo);
begin
  inherited Create;
  FHash := CalcHash(AText, AFontInfo);
  FText := AText;
  FFontInfo := AFontInfo;
  FTextViewInfo := ATextInfo;
end;

destructor TdxTextViewInfoCache.TItem.Destroy;
begin
  FTextViewInfo.Free;
  inherited Destroy;
end;

const
  DefTableSize = 33703;

class function TdxTextViewInfoCache.TItem.CalcHash(const AText: string; AFontInfo: TdxFontInfo): Cardinal;
var
  C: NativeUint;
begin
  C := NativeUInt(AFontInfo) shr 3;
  C := ((C shl 5) + C) and $FFFFFFFF;
  Result := TdxStringHash.Murmur2(AText) xor C;
end;

function TdxTextViewInfoCache.TItem.Equals(AEntry: TItem): Boolean;
begin
  Result := (Hash = AEntry.Hash) and (FontInfo = AEntry.FontInfo) and (Text = AEntry.Text);
end;


function TdxTextViewInfoCache.TItem.Equals(AHash: Cardinal; const AText: string; AFontInfo: TdxFontInfo): Boolean;
begin
  Result := (Hash = AHash) and (FontInfo = AFontInfo) and (Text = AText);
end;

{ TdxTextViewInfoCache }

constructor TdxTextViewInfoCache.Create;
begin
  inherited Create;
  FTableSize := DefTableSize;
  SetLength(FTable, FTableSize);
end;

destructor TdxTextViewInfoCache.Destroy;
begin
  Clear;
  FTable := nil;
  inherited Destroy;
end;

procedure TdxTextViewInfoCache.AddTextViewInfo(const AText: string; AFontInfo: TdxFontInfo; ATextInfo: TdxTextViewInfo);
var
  AEntry, ATestEntry: TItem;
  AIndex: Cardinal;
begin
  AEntry := TItem.Create(AText, AFontInfo, ATextInfo);
  AIndex := AEntry.Hash mod FTableSize;
  ATestEntry := FTable[AIndex];
  if ATestEntry = nil then
  begin
    FTable[AIndex] := AEntry;
    Inc(FCount);
    Exit;
  end;

  repeat
    if ATestEntry.Next = nil then
    begin
      ATestEntry.Next := AEntry;
      Inc(FCount);
      Break;
    end;
    ATestEntry := ATestEntry.Next;
  until False;
end;

function TdxTextViewInfoCache.TryGetTextViewInfo(const AText: string; AFontInfo: TdxFontInfo): TdxTextViewInfo;
var
  AEntry: TItem;
  AIndex: Integer;
  AHash: Cardinal;
begin
  AHash := TItem.CalcHash(AText, AFontInfo);
  AIndex := AHash mod FTableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
    Exit(nil);

  repeat
    if AEntry.Equals(AHash, AText, AFontInfo) then
      Exit(AEntry.TextViewInfo);

    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(nil);
  until False;
end;

procedure TdxTextViewInfoCache.Clear;
var
  I: Integer;
  AEntry, ATemp: TItem;
begin
  for I := 0 to FTableSize - 1 do
  begin
    AEntry := FTable[I];
;
    while AEntry <> nil do
    begin
      ATemp := AEntry;
      AEntry := ATemp.Next;
      ATemp.Free;
    end;
    FTable[I] := nil;
  end;
  FCount := 0;
end;

{ TdxBoxBase }

function TdxBoxBase.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
begin
  Result := APieceTable.DocumentModel.FontCache[GetRun(APieceTable).FontCacheIndex];
end;

function TdxBoxBase.IsHyperlinkSupported: Boolean;
begin
  Result := True;
end;

function TdxBoxBase.IsInlinePicture: Boolean;
begin
  Result := False;
end;

procedure TdxBoxBase.MoveVertically(ADeltaY: Integer);
begin
  FBounds.Offset(0, ADeltaY);
end;

function TdxBoxBase.GetActualSizeBounds: TRect;
begin
  Result := FBounds;
end;


{ TdxHighlightArea }

constructor TdxHighlightArea.Create(AColor: TdxAlphaColor; const ABounds: TRect);
begin
  FBounds := ABounds;
  FColor := AColor;
  FIsNull := False;
end;

function TdxHighlightArea.GetBounds: TRect;
begin
  Result := FBounds;
end;

class function TdxHighlightArea.GetNull: TdxHighlightArea;
begin
  Result.FIsNull := True;
end;

procedure TdxHighlightArea.MoveVertically(ADeltaY: Integer);
begin
  FBounds.Offset(0, ADeltaY);
end;

{ TdxHighlightAreaCollection }

procedure TdxHighlightAreaCollection.MoveVertically(ADeltaY: Integer);
var
  I: Integer;
  AItem: TdxHighlightArea;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    AItem.MoveVertically(ADeltaY);
    Items[I] := AItem;
  end;
end;

{ TdxRowBoxRange }

constructor TdxRowBoxRange.Create(AFirstBoxIndex, ALastBoxIndex: Integer; const ABounds: TRect);
begin
  inherited Create;
  FFirstBoxIndex := AFirstBoxIndex;
  FLastBoxIndex := ALastBoxIndex;
  FBounds := ABounds;
end;

{ TdxBoxInfo }

function TdxBoxInfo.GetFontInfo(APieceTable: TdxCustomPieceTable): TdxFontInfo;
begin
  Result := APieceTable.DocumentModel.FontCache[GetRun(APieceTable).FontCacheIndex];
end;

function TdxBoxInfo.GetForceUpdateCurrentRowHeight: Boolean;
begin
  Result := False;
end;

function TdxBoxInfo.GetRun(APieceTable: TdxCustomPieceTable): TdxRunBase;
begin
  Result := APieceTable.Runs[StartPos.RunIndex];
end;

{ TdxBoxAndPointXComparable }

constructor TdxBoxAndPointXComparable.Create(const P: TPoint);
begin
  inherited Create;
  FX := P.X;
end;

function TdxBoxAndPointXComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  R: TRect;
begin
  R := Value.Bounds;
  if X < R.Left then
    Result := 1
  else
    if X >= R.Right then
      Result := -1
    else
      Result := 0;
end;

{ TdxBoxAndPointYComparable }

constructor TdxBoxAndPointYComparable.Create(const P: TPoint);
begin
  inherited Create;
  FY := P.Y;
end;

function TdxBoxAndPointYComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  R: TRect;
begin
  R := Value.Bounds;
  if Y < R.Top then
    Result := 1
  else
    if Y >= R.Bottom then
      Result := -1
    else
      Result := 0;
end;

{ TdxBoxLogPositionComparable }

constructor TdxBoxLogPositionComparable.Create(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FLogPosition := ALogPosition;
end;

{ TdxBoxAndLogPositionComparable }

function TdxBoxAndLogPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AFirstPos, ALastPos: TdxDocumentModelPosition;
begin
  AFirstPos := Value.GetFirstPosition(PieceTable);
  if LogPosition < AFirstPos.LogPosition then
    Result := 1
  else
  begin
    if LogPosition > AFirstPos.LogPosition then
    begin
      ALastPos := Value.GetLastPosition(PieceTable);
      if LogPosition <= ALastPos.LogPosition then
        Result := 0
      else
        Result := -1;
    end
    else
      Result := 0;
  end;
end;

{ TdxBoxAndFormatterPositionComparable }

constructor TdxBoxAndFormatterPositionComparable.Create(const AFormatterPosition: TdxFormatterPosition);
begin
  inherited Create;
  FFormatterPosition := AFormatterPosition;
end;

function TdxBoxAndFormatterPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AFirstPos, ALastPos: TdxFormatterPosition;
begin
  AFirstPos := Value.GetFirstFormatterPosition;
  if FFormatterPosition < AFirstPos then
    Result := 1
  else
    if FFormatterPosition > AFirstPos then
    begin
      ALastPos := Value.GetLastFormatterPosition;
      if FFormatterPosition <= ALastPos then
        Result := 0
      else
        Result := -1;
    end
    else
      Result := 0;
end;

{ TdxBoxStartAndLogPositionComparable }

function TdxBoxStartAndLogPositionComparable.CompareTo(const Value: TdxBoxBase): Integer;
var
  AFirstPos: TdxDocumentModelPosition;
begin
  AFirstPos := Value.GetFirstPosition(PieceTable);
  Result := AFirstPos.LogPosition - LogPosition;
end;


end.
