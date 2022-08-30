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

unit dxRichEdit.Api.Shapes;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Graphics,
  dxCore, dxCoreClasses, dxCoreGraphics, cxGeometry,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable;

type
  TdxNativeShape = class;

  { TdxNativeTextBoxSubDocument }

  TdxNativeTextBoxSubDocument = class(TdxNativeSubDocument)
  strict private
    procedure ThrowInvalidOperationException;
  public
    function InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape; override;
    function InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape; override;
  end;

  { TdxNativeTextBox }

  TdxNativeTextBox = class(TInterfacedObject, IdxRichEditTextBox)
  strict private
    FDocument: TdxNativeSubDocument;
    FContent: TdxTextBoxFloatingObjectContent;
    FTextBoxContentDocument: IdxRichEditSubDocument;
    function GetProperties: TdxTextBoxProperties;

    function GetDocument: IdxRichEditSubDocument;
    function GetHeightRule: TdxRichEditTextBoxSizeRule;
    function GetMarginBottom: Single;
    function GetMarginLeft: Single;
    function GetMarginRight: Single;
    function GetMarginTop: Single;
    procedure SetHeightRule(const Value: TdxRichEditTextBoxSizeRule);
    procedure SetMarginBottom(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginTop(const Value: Single);
  public
    constructor Create(ADocument: TdxNativeSubDocument;
      AContent: TdxTextBoxFloatingObjectContent);

    property Properties: TdxTextBoxProperties read GetProperties;

    property Document: IdxRichEditSubDocument read GetDocument;
    property HeightRule: TdxRichEditTextBoxSizeRule read GetHeightRule write SetHeightRule;
    property MarginBottom: Single read GetMarginBottom write SetMarginBottom;
    property MarginLeft: Single read GetMarginLeft write SetMarginLeft;
    property MarginRight: Single read GetMarginRight write SetMarginRight;
    property MarginTop: Single read GetMarginTop write SetMarginTop;
  end;

  { TdxNativeShapeFill }

  TdxNativeShapeFill = class(TInterfacedObject, IdxRichEditShapeFill)
  strict private
    FShape: TdxNativeShape;
    function GetModelShape: TdxShape;
    function GetColor: TdxAlphaColor;
    procedure SetColor(const Value: TdxAlphaColor);
  protected
    property ModelShape: TdxShape read GetModelShape;
  public
    constructor Create(AShape: TdxNativeShape);

    property Color: TdxAlphaColor read GetColor write SetColor;
  end;

  { TdxNativeShapeLine }

  TdxNativeShapeLine = class(TInterfacedObject, IdxRichEditShapeLine)
  strict private
    FShape: TdxNativeShape;
    function GetModelShape: TdxShape;
    function GetColor: TdxAlphaColor;
    function GetThickness: Single;
    procedure SetColor(const Value: TdxAlphaColor);
    procedure SetThickness(const Value: Single);
  protected
    property ModelShape: TdxShape read GetModelShape;
  public
    constructor Create(AShape: TdxNativeShape);

    property Color: TdxAlphaColor read GetColor write SetColor;
    property Thickness: Single read GetThickness write SetThickness;
  end;

  { TdxNativeShape }

  TdxNativeShape = class(TInterfacedObject, IdxRichEditShape)
  strict private
    FDocument: TdxNativeSubDocument;
    FRange: IdxRichEditDocumentRange;
    FTextBox: IdxRichEditTextBox;
    FFill: IdxRichEditShapeFill;
    FLine: IdxRichEditShapeLine;

    function GetAnchorRun: TdxFloatingObjectAnchorRun;
    function GetShape: TdxShape;
    function GetProperties: TdxFloatingObjectProperties;
    function GetRectangleScalableObject: IdxRectangularScalableObject;
    function GetUnitConverter: TdxDocumentModelUnitConverter;

    function GetFill: IdxRichEditShapeFill;
    function GetHorizontalAlignment: TdxRichEditShapeHorizontalAlignment;
    function GetLine: IdxRichEditShapeLine;
    function GetLockAspectRatio: Boolean;
    function GetMarginBottom: Single;
    function GetMarginLeft: Single;
    function GetMarginRight: Single;
    function GetMarginTop: Single;
    function GetName: string;
    function GetOffset: TdxPointF;
    function GetOriginalSize: TdxSizeF;
    function GetPicture: TdxOfficeImage;
    function GetPictureUri: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetRelativeHorizontalPosition: TdxRichEditShapeRelativeHorizontalPosition;
    function GetRelativeVerticalPosition: TdxRichEditShapeRelativeVerticalPosition;
    function GetRotationAngle: Single;
    function GetScaleX: Single;
    function GetScaleY: Single;
    function GetSize: TdxSizeF;
    function GetTextBox: IdxRichEditTextBox;
    function GetTextWrapping: TdxRichEditTextWrappingType;
    function GetVerticalAlignment: TdxRichEditShapeVerticalAlignment;
    function GetZOrder: Integer;
    procedure SetHorizontalAlignment(const Value: TdxRichEditShapeHorizontalAlignment);
    procedure SetLockAspectRatio(const Value: Boolean);
    procedure SetMarginBottom(const Value: Single);
    procedure SetMarginLeft(const Value: Single);
    procedure SetMarginRight(const Value: Single);
    procedure SetMarginTop(const Value: Single);
    procedure SetName(const Value: string);
    procedure SetOffset(const Value: TdxPointF);
    procedure SetPictureUri(const Value: string);
    procedure SetRelativeHorizontalPosition(const Value: TdxRichEditShapeRelativeHorizontalPosition);
    procedure SetRelativeVerticalPosition(const Value: TdxRichEditShapeRelativeVerticalPosition);
    procedure SetRotationAngle(const Value: Single);
    procedure SetScaleX(const Value: Single);
    procedure SetScaleY(const Value: Single);
    procedure SetSize(const Value: TdxSizeF);
    procedure SetTextWrapping(const Value: TdxRichEditTextWrappingType);
    procedure SetVerticalAlignment(const Value: TdxRichEditShapeVerticalAlignment);
    procedure SetZOrder(const Value: Integer);
  public
    constructor Create(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex);

    class function TryCreate(ADocument: TdxNativeSubDocument; ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): IdxRichEditShape; static;
    class function CreateUnsafe(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex): IdxRichEditShape; static;

    property AnchorRun: TdxFloatingObjectAnchorRun read GetAnchorRun;
    property Shape: TdxShape read GetShape;
    property Properties: TdxFloatingObjectProperties read GetProperties;
    property RectangleScalableObject: IdxRectangularScalableObject read GetRectangleScalableObject;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;

    property Fill: IdxRichEditShapeFill read GetFill;
    property HorizontalAlignment: TdxRichEditShapeHorizontalAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
    property Line: IdxRichEditShapeLine read GetLine;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property MarginBottom: Single read GetMarginBottom write SetMarginBottom;
    property MarginLeft: Single read GetMarginLeft write SetMarginLeft;
    property MarginRight: Single read GetMarginRight write SetMarginRight;
    property MarginTop: Single read GetMarginTop write SetMarginTop;
    property Name: string read GetName write SetName;
    property Offset: TdxPointF read GetOffset write SetOffset;
    property OriginalSize: TdxSizeF read GetOriginalSize;
    property Picture: TdxOfficeImage read GetPicture;
    property PictureUri: string read GetPictureUri write SetPictureUri;
    property Range: IdxRichEditDocumentRange read GetRange;
    property RelativeHorizontalPosition: TdxRichEditShapeRelativeHorizontalPosition read GetRelativeHorizontalPosition write SetRelativeHorizontalPosition;
    property RelativeVerticalPosition: TdxRichEditShapeRelativeVerticalPosition read GetRelativeVerticalPosition write SetRelativeVerticalPosition;
    property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property ScaleX: Single read GetScaleX write SetScaleX;
    property ScaleY: Single read GetScaleY write SetScaleY;
    property Size: TdxSizeF read GetSize write SetSize;
    property TextBox: IdxRichEditTextBox read GetTextBox;
    property TextWrapping: TdxRichEditTextWrappingType read GetTextWrapping write SetTextWrapping;
    property VerticalAlignment: TdxRichEditShapeVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property ZOrder: Integer read GetZOrder write SetZOrder;
  end;

  { TdxNativeShapeCollection }

  TdxNativeShapeCollection = class(TInterfacedObject,
    IdxRichEditShapeCollection,
    IdxRichEditReadOnlyShapeCollection)
  strict private
    type
      TProcessShapeProc = reference to function(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex): Boolean;
  strict private
    FDocument: TdxNativeSubDocument;
    FRange: IdxRichEditDocumentRange;

    function CanProcessShape(const AShape: IdxRichEditShape): Boolean;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    procedure ProcessShapes(const Action: TProcessShapeProc);

    function GetCount: Integer;
    function GetItem(const Name: string): IdxRichEditShape; overload;
    function GetItem(Index: Integer): IdxRichEditShape; overload;
  protected
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  public
    constructor Create(ADocument: TdxNativeSubDocument);

    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection;

    function InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape;
    function InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;

    property Count: Integer read GetCount;
    property Items[const Name: string]: IdxRichEditShape read GetItem; default;
  end;

implementation

uses
  Math,
  dxTypeHelpers,
  dxRichEdit.Api.Images,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxNativeTextBoxSubDocument }

function TdxNativeTextBoxSubDocument.InsertPicture(const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;
begin
  ThrowInvalidOperationException;
  Result := nil;
end;

function TdxNativeTextBoxSubDocument.InsertTextBox(const APos: IdxRichEditDocumentPosition): IdxRichEditShape;
begin
  ThrowInvalidOperationException;
  Result := nil;
end;

procedure TdxNativeTextBoxSubDocument.ThrowInvalidOperationException;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionCannotInsertShapeIntoTextBox));
end;

{ TdxNativeTextBox }

constructor TdxNativeTextBox.Create(ADocument: TdxNativeSubDocument;
  AContent: TdxTextBoxFloatingObjectContent);
begin
  inherited Create;
  FDocument := ADocument;
  FContent := AContent;
  FTextBoxContentDocument := TdxNativeTextBoxSubDocument.Create(TdxPieceTable(FContent.TextBox.PieceTable),
    FDocument.DocumentServer);
end;

function TdxNativeTextBox.GetDocument: IdxRichEditSubDocument;
begin
  Result := FTextBoxContentDocument;
end;

function TdxNativeTextBox.GetProperties: TdxTextBoxProperties;
begin
  Result := FContent.TextBoxProperties;
end;

function TdxNativeTextBox.GetMarginTop: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.TopMargin);
end;

procedure TdxNativeTextBox.SetMarginTop(const Value: Single);
begin
  Properties.TopMargin := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeTextBox.GetMarginBottom: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.BottomMargin);
end;

procedure TdxNativeTextBox.SetMarginBottom(const Value: Single);
begin
  Properties.BottomMargin := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeTextBox.GetMarginLeft: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.LeftMargin);
end;

procedure TdxNativeTextBox.SetMarginLeft(const Value: Single);
begin
  Properties.LeftMargin := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeTextBox.GetMarginRight: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.RightMargin);
end;

procedure TdxNativeTextBox.SetMarginRight(const Value: Single);
begin
  Properties.RightMargin := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeTextBox.GetHeightRule: TdxRichEditTextBoxSizeRule;
begin
  if Properties.ResizeShapeToFitText then
    Result := TdxRichEditTextBoxSizeRule.Auto
  else
    Result := TdxRichEditTextBoxSizeRule.Exact;
end;

procedure TdxNativeTextBox.SetHeightRule(const Value: TdxRichEditTextBoxSizeRule);
begin
  Properties.ResizeShapeToFitText := Value = TdxRichEditTextBoxSizeRule.Auto;
end;

{ TdxNativeShapeFill }

constructor TdxNativeShapeFill.Create(AShape: TdxNativeShape);
begin
  inherited Create;
  FShape := AShape;
end;

function TdxNativeShapeFill.GetModelShape: TdxShape;
begin
  Result := FShape.Shape;
end;

function TdxNativeShapeFill.GetColor: TdxAlphaColor;
begin
  Result := ModelShape.FillColor;
end;

procedure TdxNativeShapeFill.SetColor(const Value: TdxAlphaColor);
begin
  ModelShape.FillColor := Value;
end;

{ TdxNativeShapeLine }

constructor TdxNativeShapeLine.Create(AShape: TdxNativeShape);
begin
  inherited Create;
  FShape := AShape;
end;

function TdxNativeShapeLine.GetModelShape: TdxShape;
begin
  Result := FShape.Shape;
end;

function TdxNativeShapeLine.GetColor: TdxAlphaColor;
begin
  Result := ModelShape.OutlineColor;
end;

procedure TdxNativeShapeLine.SetColor(const Value: TdxAlphaColor);
begin
  ModelShape.OutlineColor := Value;
end;

function TdxNativeShapeLine.GetThickness: Single;
begin
  Result := FShape.UnitConverter.ModelUnitsToPointsF(ModelShape.OutlineWidth);
end;

procedure TdxNativeShapeLine.SetThickness(const Value: Single);
begin
  ModelShape.OutlineWidth := Integer(Round(FShape.UnitConverter.PointsToModelUnitsF(Value)));
end;

{ TdxNativeShape }

constructor TdxNativeShape.Create(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex);
var
  AStart, AEnd: TdxDocumentModelPosition;
begin
  inherited Create;
  FDocument := ADocument;
  AStart := TdxDocumentModelPosition.FromRunStart(ADocument.PieceTable, ARunIndex);
  AEnd := AStart;
  AEnd.LogPosition := AStart.LogPosition + 1;
  FRange := TdxNativeDocumentRange.Create(ADocument, AStart, AEnd);
end;

class function TdxNativeShape.TryCreate(ADocument: TdxNativeSubDocument;
  ARun: TdxTextRunBase; ARunIndex: TdxRunIndex): IdxRichEditShape;
var
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARun);
  if AAnchorRun <> nil then
    Result := TdxNativeShape.Create(ADocument, ARunIndex)
  else
    Result := nil;
end;

class function TdxNativeShape.CreateUnsafe(ADocument: TdxNativeSubDocument; ARunIndex: TdxRunIndex): IdxRichEditShape;
begin
  Result := TdxNativeShape.Create(ADocument, ARunIndex);
end;

function TdxNativeShape.GetAnchorRun: TdxFloatingObjectAnchorRun;
begin
  Result := TdxFloatingObjectAnchorRun(FDocument.PieceTable.Runs[TdxNativeDocumentPosition(FRange.Start).Position.RunIndex]);
end;

function TdxNativeShape.GetShape: TdxShape;
begin
  Result := AnchorRun.Shape;
end;

function TdxNativeShape.GetProperties: TdxFloatingObjectProperties;
begin
  Result := AnchorRun.FloatingObjectProperties;
end;

function TdxNativeShape.GetRectangleScalableObject: IdxRectangularScalableObject;
begin
  Result := FDocument.PieceTable.Runs[TdxNativeDocumentPosition(FRange.Start).Position.RunIndex] as IdxRectangularScalableObject;
end;

function TdxNativeShape.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := FDocument.PieceTable.DocumentModel.UnitConverter;
end;

function TdxNativeShape.GetTextWrapping: TdxRichEditTextWrappingType;
begin
  case Properties.TextWrapType of
    TdxFloatingObjectTextWrapType.None:
      if Properties.IsBehindDoc then
        Result := TdxRichEditTextWrappingType.BehindText
      else
        Result := TdxRichEditTextWrappingType.InFrontOfText;
    TdxFloatingObjectTextWrapType.TopAndBottom:
      Result := TdxRichEditTextWrappingType.TopAndBottom;
    TdxFloatingObjectTextWrapType.Tight:
      Result := TdxRichEditTextWrappingType.Tight;
    TdxFloatingObjectTextWrapType.Through:
      Result := TdxRichEditTextWrappingType.Through;
    TdxFloatingObjectTextWrapType.Square:
      Result := TdxRichEditTextWrappingType.Square;
    else
      Result := TdxRichEditTextWrappingType.InFrontOfText;
  end;
end;

procedure TdxNativeShape.SetTextWrapping(const Value: TdxRichEditTextWrappingType);
begin
  Properties.IsBehindDoc := False;
  case Value of
    TdxRichEditTextWrappingType.Square:
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.Square;
    TdxRichEditTextWrappingType.Tight:
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.Tight;
    TdxRichEditTextWrappingType.Through:
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.Through;
    TdxRichEditTextWrappingType.TopAndBottom:
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.TopAndBottom;
    TdxRichEditTextWrappingType.BehindText:
      begin
        Properties.TextWrapType := TdxFloatingObjectTextWrapType.None;
        Properties.IsBehindDoc := True;
      end;
    TdxRichEditTextWrappingType.InFrontOfText:
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.None;
    else
      Properties.TextWrapType := TdxFloatingObjectTextWrapType.None;
  end;
end;

function TdxNativeShape.GetName: string;
begin
  Result := AnchorRun.Name;
end;

procedure TdxNativeShape.SetName(const Value: string);
begin
  AnchorRun.Name := Value;
end;

function TdxNativeShape.GetScaleX: Single;
begin
  Result := RectangleScalableObject.ScaleX / 100.0;
end;

procedure TdxNativeShape.SetScaleX(const Value: Single);
begin
  RectangleScalableObject.ScaleX := Value * 100.0;
end;

function TdxNativeShape.GetScaleY: Single;
begin
  Result := RectangleScalableObject.ScaleY / 100.0;
end;

procedure TdxNativeShape.SetScaleY(const Value: Single);
begin
  RectangleScalableObject.ScaleY := Value * 100.0;
end;

function TdxNativeShape.GetRotationAngle: Single;
begin
  Result := UnitConverter.ModelUnitsToDegreeF(Shape.Rotation);
end;

procedure TdxNativeShape.SetRotationAngle(const Value: Single);
begin
  Shape.Rotation := UnitConverter.DegreeToModelUnits(Value);
end;

function TdxNativeShape.GetOffset: TdxPointF;
var
  APoint: TPoint;
begin
  APoint := Properties.Offset;
  Result := TdxPointF.Create(FDocument.ModelUnitsToUnits(APoint.X), FDocument.ModelUnitsToUnits(APoint.Y));
end;

procedure TdxNativeShape.SetOffset(const Value: TdxPointF);
var
  X, Y: Integer;
begin
  X := FDocument.UnitsToModelUnits(Value.X);
  if Abs(X) < 1 then
    X := 1;
  Y := FDocument.UnitsToModelUnits(Value.Y);
  if Abs(Y) < 1 then
    Y := 1;
  Properties.Offset := TPoint.Create(X, Y);
end;

function TdxNativeShape.GetOriginalSize: TdxSizeF;
var
  AOriginalSize: TSize;
begin
  AOriginalSize := RectangleScalableObject.OriginalSize;
  Result := TdxSizeF.Create(FDocument.ModelUnitsToUnits(AOriginalSize.Width), FDocument.ModelUnitsToUnits(AOriginalSize.Height));
end;

function TdxNativeShape.GetSize: TdxSizeF;
var
  AActualSize: TdxSizeF;
begin
  AActualSize := Properties.ActualSize.ToSizeF;
  Result := TdxSizeF.Create(FDocument.ModelUnitsToUnitsF(AActualSize.Width), FDocument.ModelUnitsToUnitsF(AActualSize.Height));
end;

procedure TdxNativeShape.SetSize(const Value: TdxSizeF);
var
  AWidth, AHeight: Integer;
begin
  AWidth := Max(1, FDocument.UnitsToModelUnits(Value.Width));
  AHeight := Max(1, FDocument.UnitsToModelUnits(Value.Height));
  Properties.ActualSize := TSize.Create(AWidth, AHeight);
end;

function TdxNativeShape.GetHorizontalAlignment: TdxRichEditShapeHorizontalAlignment;
begin
  Result := Properties.HorizontalPositionAlignment;
end;

procedure TdxNativeShape.SetHorizontalAlignment(const Value: TdxRichEditShapeHorizontalAlignment);
begin
  Properties.HorizontalPositionAlignment := Value;
end;

function TdxNativeShape.GetVerticalAlignment: TdxRichEditShapeVerticalAlignment;
begin
  Result := Properties.VerticalPositionAlignment;
end;

procedure TdxNativeShape.SetVerticalAlignment(const Value: TdxRichEditShapeVerticalAlignment);
begin
  Properties.VerticalPositionAlignment := Value;
end;

function TdxNativeShape.GetRelativeHorizontalPosition: TdxRichEditShapeRelativeHorizontalPosition;
begin
  Result := Properties.HorizontalPositionType;
end;

procedure TdxNativeShape.SetRelativeHorizontalPosition(const Value: TdxRichEditShapeRelativeHorizontalPosition);
begin
  Properties.HorizontalPositionType := Value;
end;

function TdxNativeShape.GetRelativeVerticalPosition: TdxRichEditShapeRelativeVerticalPosition;
begin
  Result := Properties.VerticalPositionType;
end;

procedure TdxNativeShape.SetRelativeVerticalPosition(const Value: TdxRichEditShapeRelativeVerticalPosition);
begin
  Properties.VerticalPositionType := Value;
end;

function TdxNativeShape.GetZOrder: Integer;
begin
  Result := Properties.ZOrder;
end;

procedure TdxNativeShape.SetZOrder(const Value: Integer);
begin
  Properties.ZOrder := Value;
end;

function TdxNativeShape.GetLockAspectRatio: Boolean;
begin
  Result := Properties.LockAspectRatio;
end;

procedure TdxNativeShape.SetLockAspectRatio(const Value: Boolean);
begin
  Properties.LockAspectRatio := Value;
end;

function TdxNativeShape.GetMarginTop: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.TopDistance);
end;

procedure TdxNativeShape.SetMarginTop(const Value: Single);
begin
  Properties.TopDistance := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeShape.GetMarginBottom: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.BottomDistance);
end;

procedure TdxNativeShape.SetMarginBottom(const Value: Single);
begin
  Properties.BottomDistance := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeShape.GetMarginLeft: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.LeftDistance);
end;

procedure TdxNativeShape.SetMarginLeft(const Value: Single);
begin
  Properties.LeftDistance := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeShape.GetMarginRight: Single;
begin
  Result := FDocument.ModelUnitsToUnitsF(Properties.RightDistance);
end;

procedure TdxNativeShape.SetMarginRight(const Value: Single);
begin
  Properties.RightDistance := FDocument.UnitsToModelUnits(Value);
end;

function TdxNativeShape.GetPicture: TdxOfficeImage;
var
  AContent: TdxPictureFloatingObjectContent;
begin
  AContent := Safe<TdxPictureFloatingObjectContent>.Cast(AnchorRun.Content);
  if AContent = nil then
    Exit(nil);
  Result := AContent.Image.Image;
  Result.EnsureLoadComplete;
end;

function TdxNativeShape.GetTextBox: IdxRichEditTextBox;
var
  AContent: TdxTextBoxFloatingObjectContent;
begin
  if FTextBox <> nil then
    Exit(FTextBox);

  AContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(AnchorRun.Content);
  if AContent = nil then
    Exit(nil);

  FTextBox := TdxNativeTextBox.Create(FDocument, AContent);
  Result := FTextBox;
end;

function TdxNativeShape.GetPictureUri: string;
var
  AImage: TdxOfficeImage;
begin
  AImage := Picture;
  if AImage <> nil then
    Result := AImage.Uri
  else
    Result := '';
end;

function TdxNativeShape.GetRange: IdxRichEditDocumentRange;
begin
  Result := FRange;
end;

procedure TdxNativeShape.SetPictureUri(const Value: string);
var
  AImage: TdxOfficeImage;
begin
  AImage := Picture;
  if AImage <> nil then
    AImage.Uri := Value;
end;

function TdxNativeShape.GetFill: IdxRichEditShapeFill;
begin
  if FFill <> nil then
    Exit(FFill);

  FFill := TdxNativeShapeFill.Create(Self);
  Result := FFill;
end;

function TdxNativeShape.GetLine: IdxRichEditShapeLine;
begin
  if FLine <> nil then
    Exit(FLine);
  FLine := TdxNativeShapeLine.Create(Self);
  Result := FLine;
end;

{ TdxNativeShapeCollection }

constructor TdxNativeShapeCollection.Create(ADocument: TdxNativeSubDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TdxNativeShapeCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyShapeCollection;
var
  AResult: TdxNativeShapeCollection;
begin
  AResult := TdxNativeShapeCollection.Create(FDocument);
  AResult.FRange := ARange;
  Result := AResult;
end;

function TdxNativeShapeCollection.CanProcessShape(const AShape: IdxRichEditShape): Boolean;
begin
  if AShape = nil then
    Exit(False);
  Result := (FRange = nil) or FRange.Contains(AShape.Range.Start);
end;

function TdxNativeShapeCollection.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxNativeShapeCollection.GetPieceTable: TdxPieceTable;
begin
  Result := FDocument.PieceTable;
end;

procedure TdxNativeShapeCollection.ProcessShapes(const Action: TProcessShapeProc);
var
  ARuns: TdxTextRunCollection;
  ARun: TdxFloatingObjectAnchorRun;
  ACount, I: Integer;
begin
  ARuns := PieceTable.Runs;
  ACount := ARuns.Count;
  for I := 0 to ACount - 1 do
  begin
    ARun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARuns[I]);
    if ARun <> nil then
    begin
      if not Action(ARun, I) then
        Break;
    end;
  end;
end;

function TdxNativeShapeCollection.GetCount: Integer;
var
  AResult: Integer;
begin
  AResult := 0;
  ProcessShapes(function(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex): Boolean
    var
      AShape: IdxRichEditShape;
    begin
      AShape := TdxNativeShape.TryCreate(FDocument, ARun, ARunIndex);
      if AShape = nil then
        Exit(False);
      if CanProcessShape(AShape) then
        Inc(AResult);
      Result := True;
    end);
  Result := AResult;
end;

function TdxNativeShapeCollection.GetItem(const Name: string): IdxRichEditShape;
var
  AResult: IdxRichEditShape;
begin
  AResult := nil;
  ProcessShapes(function(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex): Boolean
    var
      AShape: IdxRichEditShape;
    begin
      Result := False;
      AShape := TdxNativeShape.TryCreate(FDocument, ARun, ARunIndex);
      if AShape = nil then
        Exit;
      if CanProcessShape(AShape) then
      begin
        if AShape.Name = Name then
          AResult := AShape;
        Result := AResult = nil;
      end;
    end);
  Result := AResult;
end;

function TdxNativeShapeCollection.GetItem(Index: Integer): IdxRichEditShape;
var
  I: Integer;
  AResult: IdxRichEditShape;
begin
  I := 0;
  AResult := nil;
  ProcessShapes(function(ARun: TdxFloatingObjectAnchorRun; ARunIndex: TdxRunIndex): Boolean
    var
      AShape: IdxRichEditShape;
    begin
      Result := False;
      AShape := TdxNativeShape.TryCreate(FDocument, ARun, ARunIndex);
      if AShape = nil then
        Exit;
      if CanProcessShape(AShape) then
      begin
        if I = Index then
          AResult := AShape
        else
          Inc(I);
        Result := AResult = nil;
      end;
    end);
  Result := AResult;
end;

function TdxNativeShapeCollection.InsertPicture(
  const APos: IdxRichEditDocumentPosition; AImage: TGraphic): IdxRichEditShape;
var
  AOfficeImage: TdxOfficeImage;
  AIsAddedOfficeImage: Boolean;
  AReference: TdxOfficeImageReference;
  ARunInfo: TdxRunInfo;
  ALogPosition: TdxDocumentLogPosition;
  ARun: TdxFloatingObjectAnchorRun;
  AContent: TdxPictureFloatingObjectContent;
  ASize: TSize;
begin
  if AImage = nil then
    Exit(nil);
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);

  ARunInfo := TdxRunInfo.Create(PieceTable);
  try
    ALogPosition := FDocument.NormalizeLogPosition(APos.LogPosition);

    DocumentModel.BeginUpdate;
    try
      PieceTable.InsertFloatingObjectAnchor(ALogPosition);

      PieceTable.CalculateRunInfoStart(ALogPosition, ARunInfo);
      ARun := TdxFloatingObjectAnchorRun(PieceTable.Runs[ARunInfo.Start.RunIndex]);
      AIsAddedOfficeImage := False;
      AOfficeImage := TdxNativeDocumentImage.CreateImage(AImage);
      try
        AReference := DocumentModel.CreateImage(AOfficeImage);
        try
          AIsAddedOfficeImage := AReference.Image = AOfficeImage;
          AContent := TdxPictureFloatingObjectContent.Create(ARun, AReference);
          ARun.SetContent(AContent);
          ASize := DocumentModel.UnitConverter.TwipsToModelUnits(AReference.SizeInTwips);
          ARun.FloatingObjectProperties.ActualSize := ASize;
          AContent.SetOriginalSize(ASize);
          Result := TdxNativeShape.CreateUnsafe(FDocument, ARunInfo.Start.RunIndex);
        finally
          AReference.Free;
        end;
      finally
        if not AIsAddedOfficeImage then
          AOfficeImage.Free;
      end;
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    ARunInfo.Free;
  end;
end;

function TdxNativeShapeCollection.InsertTextBox(
  const APos: IdxRichEditDocumentPosition): IdxRichEditShape;
var
  ARunInfo: TdxRunInfo;
  ALogPosition: TdxDocumentLogPosition;
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContentType: TdxTextBoxContentType;
  AContent: TdxTextBoxFloatingObjectContent;
  ASize: TSize;
begin
  FDocument.CheckValid;
  FDocument.CheckDocumentPosition(APos);

  ARunInfo := TdxRunInfo.Create(PieceTable);
  try
    ALogPosition := FDocument.NormalizeLogPosition(APos.LogPosition);
    DocumentModel.BeginUpdate;
    try
      PieceTable.InsertFloatingObjectAnchor(ALogPosition);
      PieceTable.CalculateRunInfoStart(ALogPosition, ARunInfo);
      ARun := TdxFloatingObjectAnchorRun(PieceTable.Runs[ARunInfo.Start.RunIndex]);
      ATextBoxContentType := TdxTextBoxContentType.Create(DocumentModel);
      AContent := TdxTextBoxFloatingObjectContent.Create(ARun, ATextBoxContentType);
      ARun.SetContent(AContent);
      ASize := DocumentModel.UnitConverter.TwipsToModelUnits(TSize.Create(3 * 1440, 2 * 1440));
      ARun.FloatingObjectProperties.ActualSize := ASize;
      Result := TdxNativeShape.CreateUnsafe(FDocument, ARunInfo.Start.RunIndex);
    finally
      DocumentModel.EndUpdate;
    end;
  finally
    ARunInfo.Free;
  end;
end;

end.
