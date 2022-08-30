{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetSelection;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Dialogs, Messages, Controls, Generics.Defaults, Generics.Collections,
  cxGeometry, dxCore, cxGraphics, cxControls, cxEdit, cxTextEdit, cxRichEdit,
  dxSpreadSheetCore, dxSpreadSheetGraphics, cxClasses, dxCoreClasses;

const
  dxSpreadSheetCornetHitTestZoneSize = 8;

type

  { TdxSpreadSheetCustomTableViewDragAndDropObject }

  TdxSpreadSheetCustomTableViewDragAndDropObject = class(TdxSpreadSheetCustomDragAndDropObject)
  strict private
    function GetView: TdxSpreadSheetTableView;
  protected
    procedure GetCellAtAbsolutePoint(const P: TPoint; out ARowIndex, AColumnIndex: Integer); virtual;
  public
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighterItem }

  TdxSpreadSheetTableViewReferenceHighlighterItem = class
  public
    Bounds: TRect;
    CellsArea: TRect;
    PositionInText: Integer;
    TextLength: Integer;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighter }

  TdxSpreadSheetTableViewReferenceHighlighter = class(TdxSpreadSheetCustomReferenceHighlighter)
  strict private
    FReferenceColors: TDictionary<TRect, TColor>;
    FReferences: TObjectList<TdxSpreadSheetTableViewReferenceHighlighterItem>;

    function GetColor(Index: Integer): TColor;
    function GetView: TdxSpreadSheetTableView;
  protected const
    FrameHitTestZoneSize = 6;
  protected
    FColorPalette: TList<TColor>;

    function CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetAreaBounds(const AArea: TRect): TRect;
    function GetCell: TdxSpreadSheetCell; virtual; abstract;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function GetSpreadSheet: TdxCustomSpreadSheet; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure Calculate(const AEdit: TcxCustomEdit); override;
    procedure Calculate(const AFormulaText: string); override;
    procedure CalculateColors;
    procedure Clear; override;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
    procedure HighlightReferences(AEdit: TcxCustomRichEdit); override;
    procedure PrepareColorPalette; override;
    //
    property Cell: TdxSpreadSheetCell read GetCell;
    property Colors[Index: Integer]: TColor read GetColor;
    property ReferenceColors: TDictionary<TRect, TColor> read FReferenceColors;
    property References: TObjectList<TdxSpreadSheetTableViewReferenceHighlighterItem> read FReferences;
    property View: TdxSpreadSheetTableView read GetView;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject }

  TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject = class(TdxSpreadSheetCustomTableViewDragAndDropObject)
  strict private
    FCursor: TCursor;
    FPrevCellArea: TRect;

    function GetContentOrigin: TPoint;
    function GetEdit: TcxCustomEdit;
    function GetHighlighter: TdxSpreadSheetTableViewReferenceHighlighter;
  protected
    FBounds: TRect;
    FCapturePoint: TPoint;
    FContentRect: TRect;
    FReference: TdxSpreadSheetTableViewReferenceHighlighterItem;
    FReferenceIndex: Integer;

    procedure AdjustRowAndColumnIndexesByMergedCells(var ARowIndex, AColumnIndex: Integer);
    procedure BeforeBeginDragAndDrop; override;
    procedure BeginDragAndDrop; override;
    function CheckForContentRect(const P: TPoint): TPoint;
    procedure CheckRowAndColumnIndexes(var ARowIndex, AColumnIndex: Integer); virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetAbsoluteCellBounds(const ARowIndex, AColumnIndex: Integer): TRect;
    procedure GetCellAtAbsolutePoint(const P: TPoint; out ARowIndex, AColumnIndex: Integer); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    procedure InitializeReference; virtual;
    function TranslateCoords(const P: TPoint): TPoint; override;
    procedure UpdateReference(AReference: TRect; AForce: Boolean = False);
  public
    property CapturePoint: TPoint read FCapturePoint;
    property ContentOrigin: TPoint read GetContentOrigin;
    property ContentRect: TRect read FContentRect;
    property Edit: TcxCustomEdit read GetEdit;
    property Highlighter: TdxSpreadSheetTableViewReferenceHighlighter read GetHighlighter;
    property Reference: TdxSpreadSheetTableViewReferenceHighlighterItem read FReference;
    property ReferenceIndex: Integer read FReferenceIndex;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject }

  TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject = class(
    TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject)
  strict private
    FColumnDelta: Integer;
    FRowDelta: Integer;
  protected
    procedure BeginDragAndDrop; override;
    procedure CheckRowAndColumnIndexes(var ARowIndex, AColumnIndex: Integer); override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject }

  TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject = class(
    TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject)
  strict private
    FCellsArea: TRect;
  protected
    FCorner: TdxCorner;

    procedure AdjustRowAndColumnIndexes(const P: TPoint; var ARowIndex, AColumnIndex: Integer); virtual;
    procedure BeforeBeginDragAndDrop; override;
    function CalculateReference(ARowIndex, AColumnIndex: Integer): TRect;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    //
    property Corner: TdxCorner read FCorner;
  end;

  { TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject }

  TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject = class(
    TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject)
  protected
    procedure AdjustRowAndColumnIndexes(const P: TPoint; var ARowIndex, AColumnIndex: Integer); override;
    procedure BeforeBeginDragAndDrop; override;
    procedure BeginDragAndDrop; override;
    procedure InitializeReference; override;
  end;

  { TdxSpreadSheetTableViewEditingCellReferenceHighlighter }

  TdxSpreadSheetTableViewEditingCellReferenceHighlighter = class(TdxSpreadSheetTableViewReferenceHighlighter)
  strict private
    function GetEditingController: TdxSpreadSheetTableViewEditingController;
  protected
    function GetCell: TdxSpreadSheetCell; override;
  public
    property EditingController: TdxSpreadSheetTableViewEditingController read GetEditingController;
  end;

  { TdxSpreadSheetTableViewCustomSelectionDragAndDropObject }

  TdxSpreadSheetTableViewCustomSelectionDragAndDropObject = class(TdxSpreadSheetCustomTableViewDragAndDropObject)
  strict private
    FCapturedCell: TPoint;

    function GetClipboardArea: TdxSpreadSheetClipboardArea;
    function GetOriginalArea: TRect;
    function GetTargetArea: TRect;
  protected
    procedure ApplyChanges; virtual; abstract;
    procedure HistoryActionBegin(AActionClass: TdxSpreadSheetHistoryActionClass);
    procedure HistoryActionEnd;
    function HasDataInArea(const AArea: TRect): Boolean;
    procedure UpdateSelection(const R: TRect);

    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetImmediateStart: Boolean; override;
    function GetSelectionArea: TRect; virtual;
  public
    function DoProcessKeyDown(var Message: TWMKey): Boolean; override;

    property CapturedCell: TPoint read FCapturedCell;
    property ClipboardArea: TdxSpreadSheetClipboardArea read GetClipboardArea;
    property OriginalArea: TRect read GetOriginalArea;
    property TargetArea: TRect read GetTargetArea;
  end;

  { TdxSpreadSheetTableViewAutoFillDragAndDropObject }

  TdxSpreadSheetTableViewAutoFillDragAndDropObject = class(TdxSpreadSheetTableViewCustomSelectionDragAndDropObject)
  protected
    FDirection: TcxDirection;

    procedure ApplyChanges; override;
    function CalculateTargetSelection(ARow, AColumn: Integer): TRect;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  public
    class function CalculateDirection(const ASourceArea, ATargetArea: TRect): TcxDirection; overload;
    class function CalculateDirection(const ASourceArea: TRect; ARow, AColumn: Integer): TcxDirection; overload;
  end;

  { TdxSpreadSheetTableViewMoveSelectionDragAndDropObject }

  TdxSpreadSheetTableViewMoveSelectionDragAndDropObject = class(TdxSpreadSheetTableViewCustomSelectionDragAndDropObject)
  protected
    procedure ApplyChanges; override;
    procedure CheckCanApplyChanges(AMoveMode: Boolean);
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
  end;

  { TdxSpreadSheetTableViewCustomSelectionViewInfo }

  TdxSpreadSheetTableViewCustomSelectionViewInfo = class(TdxSpreadSheetViewAbstractCellViewInfo)
  protected
    function GetPopupMenuClass(AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass; override;
    function GetView: TdxSpreadSheetCustomView; override;
  public
    procedure Invalidate; override;
  end;

  { TdxSpreadSheetTableViewSelectionFrameViewInfo }

  TdxSpreadSheetTableViewSelectionFrameViewInfo = class(TdxSpreadSheetTableViewCustomSelectionViewInfo)
  protected
    function CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean; override;
    function CanFillData: Boolean; virtual;
    function CanMoveSelection: Boolean; virtual;
    function GetDragAndDropObjectClass(AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass; override;
    function InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean; override;
  public
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;
    function GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor; override;
  end;

  { TdxSpreadSheetTableViewCustomPrintAreaViewInfo }

  TdxSpreadSheetTableViewCustomPrintAreaViewInfo = class(TdxSpreadSheetTableViewCustomSelectionViewInfo)
  protected
    procedure DrawCore(ACanvas: TcxCanvas; const R: TRect; AStage: TdxSpreadSheetDrawingStage); virtual; abstract;
    function GetBackgroundColor: TColor;
    function GetBorderSize: Integer; virtual;
    function GetBorderStyle: TdxSpreadSheetCellBorderStyle; virtual;
    function GetColor: TColor; virtual;
  public
    procedure Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage); override;
    //
    property BackgroundColor: TColor read GetBackgroundColor;
    property BorderSize: Integer read GetBorderSize;
    property BorderStyle: TdxSpreadSheetCellBorderStyle read GetBorderStyle;
    property Color: TColor read GetColor;
  end;

  { TdxSpreadSheetTableViewPageBreakViewInfo }

  TdxSpreadSheetTableViewPageBreakViewInfo = class(TdxSpreadSheetTableViewCustomPrintAreaViewInfo)
  strict private
    function GetHorizontal: Boolean;
  protected
    procedure DrawCore(ACanvas: TcxCanvas; const R: TRect; AStage: TdxSpreadSheetDrawingStage); override;
  public
    procedure CalculateBounds; override;
    //
    property Horizontal: Boolean read GetHorizontal;
  end;

  { TdxSpreadSheetTableViewPrintAreaViewInfo }

  TdxSpreadSheetTableViewPrintAreaViewInfo = class(TdxSpreadSheetTableViewCustomPrintAreaViewInfo)
  protected
    procedure CalculateBounds; override;
  public
    procedure DrawCore(ACanvas: TcxCanvas; const R: TRect; AStage: TdxSpreadSheetDrawingStage); override;
  end;

implementation

uses
  SysUtils, Math,
  dxSpreadSheetFormulas, dxSpreadSheetUtils, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, Forms,
  dxTypeHelpers, dxHashUtils, dxSpreadSheetCoreHelpers, cxLibraryConsts, dxSpreadSheetAutoFilling,
  dxSpreadSheetCoreFormulas, dxSpreadSheetCoreStrs, cxLookAndFeelPainters, dxSpreadSheetPopupMenu;

type
  TcxCustomRichEditAccess = class(TcxCustomRichEdit);
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCustomHitTestAccess = class(TdxSpreadSheetCustomHitTest);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableRowsAccess = class(TdxSpreadSheetTableRows);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewInfoAccess = class(TdxSpreadSheetTableViewInfo);
  TdxSpreadSheetTableViewControllerAccess = class(TdxSpreadSheetTableViewController);
  TdxSpreadSheetTableViewEditingControllerAccess = class(TdxSpreadSheetTableViewEditingController);
  TWinControlAccess = class(TWinControl);

{ TdxSpreadSheetCustomTableViewDragAndDropObject }

procedure TdxSpreadSheetCustomTableViewDragAndDropObject.GetCellAtAbsolutePoint(
  const P: TPoint; out ARowIndex, AColumnIndex: Integer);
begin
  if not TdxSpreadSheetTableViewAccess(View).GetCellAtAbsolutePoint(P, ARowIndex, AColumnIndex) then
  begin
    AColumnIndex := -1;
    ARowIndex := -1;
  end;
end;

function TdxSpreadSheetCustomTableViewDragAndDropObject.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(inherited View);
end;

{ TdxSpreadSheetTableViewReferenceHighlighter }

constructor TdxSpreadSheetTableViewReferenceHighlighter.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FColorPalette := TList<TColor>.Create;
  FReferences := TObjectList<TdxSpreadSheetTableViewReferenceHighlighterItem>.Create;
  FReferenceColors := TDictionary<TRect, TColor>.Create;
end;

destructor TdxSpreadSheetTableViewReferenceHighlighter.Destroy;
begin
  FreeAndNil(FReferences);
  FreeAndNil(FColorPalette);
  FreeAndNil(FReferenceColors);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.Calculate(const AEdit: TcxCustomEdit);
begin
  if AEdit.HandleAllocated and not TWinControlAccess(AEdit.InnerControl).FInImeComposition then
  begin
    if FColorPalette.Count = 0 then
      PrepareColorPalette;
    Calculate(TdxSpreadSheetTableViewEditingController.GetEditingText(AEdit));
    if AEdit is TcxCustomRichEdit then
      HighlightReferences(TcxCustomRichEdit(AEdit));
  end;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.Calculate(const AFormulaText: string);
var
  AFormattedText: TdxSpreadSheetFormulaFormattedText;
  AHighlightedReferenceIndex: Integer;
  AIndex: Integer;
  AItem: TdxSpreadSheetTableViewReferenceHighlighterItem;
  AReferences: TdxRectList;
  I: Integer;
begin
  References.Clear;
  if Cell <> nil then
  begin
    AReferences := TdxRectList.Create;
    try
      if TdxSpreadSheetFormulaReferencesHelper.GetReferences(Cell, AFormulaText, AReferences, AFormattedText) then
      try
        for I := 0 to AReferences.Count - 1 do
          AReferences[I] := cxRectAdjust(AReferences[I]);

        for I := 0 to AReferences.Count - 1 do
          if dxSpreadSheetIsValidArea(AReferences[I]) then
          begin
            AItem := TdxSpreadSheetTableViewReferenceHighlighterItem.Create;
            AItem.CellsArea := AReferences[I];
            AItem.Bounds := GetAreaBounds(AItem.CellsArea);
            References.Add(AItem);
          end;

        AIndex := 0;
        AHighlightedReferenceIndex := 0;
        for I := 0 to AFormattedText.Runs.Count - 2 do
          if AFormattedText.Runs[I].Tag <> 0 then
          begin
            if dxSpreadSheetIsValidArea(AReferences[AHighlightedReferenceIndex]) then
            begin
              References[AIndex].PositionInText := AFormattedText.Runs[I].StartIndex;
              References[AIndex].TextLength := AFormattedText.Runs[I + 1].StartIndex - AFormattedText.Runs[I].StartIndex;
              Inc(AIndex);
            end;
            Inc(AHighlightedReferenceIndex);
          end;

        CalculateColors;
      finally
        AFormattedText.Free;
      end;
    finally
      AReferences.Free;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.CalculateColors;
var
  I: Integer;
begin
  ReferenceColors.Clear;
  for I := 0 to References.Count - 1 do
    ReferenceColors.AddOrSetValue(References[I].CellsArea, Colors[ReferenceColors.Keys.Count]);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.Clear;
begin
  ReferenceColors.Clear;
  References.Clear;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  case HiWord(AHitTest.HitObjectData) of
    Ord(coTopLeft), Ord(coBottomRight):
      Result := crSizeNWSE;
    Ord(coTopRight), Ord(coBottomLeft):
      Result := crSizeNESW;
  else
    Result := crSizeAll;
  end;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.HighlightReferences(AEdit: TcxCustomRichEdit);

  procedure ApplyColor(const AItem: TdxSpreadSheetTableViewReferenceHighlighterItem);
  begin
    AEdit.SelStart := AItem.PositionInText - 1;
    AEdit.SelLength := AItem.TextLength;
    AEdit.SelAttributes.Color := ReferenceColors.Items[AItem.CellsArea];
  end;

var
  ASavedSelLength: Integer;
  ASavedSelStart: Integer;
  I: Integer;
begin
  SendMessage(AEdit.Handle, WM_SETREDRAW, 0, 0);
  try
    AEdit.LockChangeEvents(True, False);
    ASavedSelStart := AEdit.SelStart;
    ASavedSelLength := AEdit.SelLength;
    try
      AEdit.SelectAll;
      AEdit.SelAttributes.Color := TcxCustomRichEditAccess(AEdit).ActiveStyle.TextColor;
      for I := 0 to References.Count - 1 do
        ApplyColor(References[I]);
    finally
      AEdit.SelStart := ASavedSelStart;
      AEdit.SelLength := ASavedSelLength;
      AEdit.LockChangeEvents(False, False);
    end;
  finally
    SendMessage(AEdit.Handle, WM_SETREDRAW, 1, 1);
    AEdit.InvalidateWithChildren;
  end;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.PrepareColorPalette;
var
  AValue: TdxHSL;
  I: Integer;
begin
  FColorPalette.Clear;
  AValue := TdxColorSpaceConverter.ColorToHSL(View.SpreadSheet.LookAndFeel.Painter.SpreadSheetSelectionColor);
  AValue.L := Min(Max(AValue.L, 0.4), 0.8);
  AValue.S := Max(AValue.S, 0.4);

  FColorPalette.Capacity := Length(dxSpreadSheetSelectionColors);
  for I := 0 to Length(dxSpreadSheetSelectionColors) - 1 do
  begin
    AValue.H := TdxColorSpaceConverter.ColorToHSL(dxSpreadSheetSelectionColors[I]).H;
    FColorPalette.Add(TdxColorSpaceConverter.HSLToColor(AValue));
  end;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean;
begin
  Result := (AStage = dsSecond) and (References.Count > 0);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighter.DoDraw(ACanvas: TcxCanvas);
var
  ABackgroundColor: TColor;
  AReference: TdxSpreadSheetTableViewReferenceHighlighterItem;
  I: Integer;
begin
  ACanvas.SaveClipRegion;
  try
    ABackgroundColor := TdxSpreadSheetTableViewAccess(View).ViewInfo.ContentParams.Color;
    for I := 0 to References.Count - 1 do
    begin
      AReference := References[I];
      TdxSpreadSheetSelectionHelper.Draw(ACanvas, AReference.Bounds,
        ReferenceColors.Items[AReference.CellsArea], ABackgroundColor, [ssseCorners], [coTopLeft..coBottomRight]);
    end;
    for I := 0 to References.Count - 1 do
    begin
      AReference := References[I];
      TdxSpreadSheetSelectionHelper.Draw(ACanvas, AReference.Bounds,
        ReferenceColors.Items[AReference.CellsArea], ABackgroundColor, [ssseFrame, ssseBackground], []);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetAreaBounds(const AArea: TRect): TRect;
begin
  Result := TdxSpreadSheetTableViewInfoAccess(TdxSpreadSheetTableViewAccess(View).ViewInfo).GetAreaBounds(AArea);
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetDragAndDropObjectClass(
  AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  case HiWord(AHitTest.HitObjectData) of
    Ord(coTopLeft), Ord(coBottomRight), Ord(coTopRight), Ord(coBottomLeft):
      Result := TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject;
  else
    Result := TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject;
  end;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;

  procedure SetupHitTest(AData: Integer);
  begin
    TdxSpreadSheetCustomHitTestAccess(AHitTest).HitObject := Self;
    TdxSpreadSheetCustomHitTestAccess(AHitTest).HitObjectData := AData;
    TdxSpreadSheetCustomHitTestAccess(AHitTest).SetHitCode(hcResizeArea, True);
  end;

var
  ACorner: TdxCorner;
  ACornerBounds: TRect;
  ARect: TRect;
  I: Integer;
begin
  Result := False;
  for I := 0 to References.Count - 1 do
  begin
    ARect := References[I].Bounds;

    for ACorner := Low(TdxCorner) to High(TdxCorner) do
    begin
      ACornerBounds := TdxSpreadSheetSelectionHelper.GetCornerBounds(ARect, ACorner);
      ACornerBounds := cxRectCenter(ACornerBounds, dxSpreadSheetCornetHitTestZoneSize, dxSpreadSheetCornetHitTestZoneSize);
      if PtInRect(ACornerBounds, AHitTest.ActualHitPoint) then
      begin
        SetupHitTest(MakeLong(I, Ord(ACorner)));
        Exit(True);
      end;
    end;

    if PtInRect(cxRectInflate(ARect, FrameHitTestZoneSize div 2), AHitTest.ActualHitPoint) and not
      PtInRect(cxRectInflate(ARect, -FrameHitTestZoneSize div 2), AHitTest.ActualHitPoint) then
    begin
      SetupHitTest(MakeLong(I, MaxWord));
      Exit(True);
    end;
  end;
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetColor(Index: Integer): TColor;
begin
  Result := FColorPalette[Index mod FColorPalette.Count];
end;

function TdxSpreadSheetTableViewReferenceHighlighter.GetView: TdxSpreadSheetTableView;
begin
  Result := TdxSpreadSheetTableView(Owner);
end;

{ TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject }

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.AdjustRowAndColumnIndexesByMergedCells(
  var ARowIndex, AColumnIndex: Integer);
begin
  with View.MergedCells.ExpandArea(AColumnIndex, ARowIndex) do
  begin
    AColumnIndex := Left;
    ARowIndex := Top;
  end;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.BeforeBeginDragAndDrop;
begin
  inherited BeforeBeginDragAndDrop;
  InitializeReference;
  FContentRect := cxNullRect;
  FContentRect.BottomRight := GetAbsoluteCellBounds(dxSpreadSheetMaxRowIndex, dxSpreadSheetMaxColumnIndex).BottomRight;
  FCursor := HitTest.HitObject.GetCursor(HitTest);
  FPrevCellArea := Reference.CellsArea;
  FBounds := Reference.Bounds;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FCapturePoint := CurMousePos;
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.CheckForContentRect(const P: TPoint): TPoint;
begin
  Result.X := Max(ContentRect.Left, Min(ContentRect.Right, P.X));
  Result.Y := Max(ContentRect.Top, Min(ContentRect.Bottom, P.Y));
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.CheckRowAndColumnIndexes(
  var ARowIndex, AColumnIndex: Integer);
begin
  AColumnIndex := Max(Min(AColumnIndex, dxSpreadSheetMaxColumnIndex), 0);
  ARowIndex := Max(Min(ARowIndex, dxSpreadSheetMaxRowIndex), 0);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  TdxSpreadSheetTableViewControllerAccess(View.Controller).CheckScrollArea(cxPointScale(GetClientCursorPos, 100, ZoomFactor));
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  if not Accepted then
    UpdateReference(FPrevCellArea);
  inherited EndDragAndDrop(Accepted);
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetAbsoluteCellBounds(
  const ARowIndex, AColumnIndex: Integer): TRect;
begin
  Result := TdxSpreadSheetTableViewAccess(View).GetAbsoluteCellBounds(ARowIndex, AColumnIndex, False);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetCellAtAbsolutePoint(
  const P: TPoint; out ARowIndex, AColumnIndex: Integer);
begin
  inherited GetCellAtAbsolutePoint(P, ARowIndex, AColumnIndex);
  AdjustRowAndColumnIndexesByMergedCells(ARowIndex, AColumnIndex);
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetContentOrigin: TPoint;
begin
  Result := TdxSpreadSheetTableViewAccess(View).GetContentOrigin;
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := FCursor
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.InitializeReference;
begin
  FReferenceIndex := LoWord(HitTest.HitObjectData);
  FReference := Highlighter.References[ReferenceIndex];
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.TranslateCoords(const P: TPoint): TPoint;
begin
  Result := cxPointOffset(inherited TranslateCoords(P), ContentOrigin);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.UpdateReference(
  AReference: TRect; AForce: Boolean = False);
var
  AFormulaText: string;
  AIndexDelta: Integer;
  AReferenceAsString: string;
  I: Integer;
begin
  AReference := cxRectAdjust(AReference);
  if AForce or not cxRectIsEqual(AReference, Reference.CellsArea) then
  begin
    AReferenceAsString := dxReferenceToString(AReference,
      Control.OptionsView.R1C1Reference, Highlighter.Cell.RowIndex, Highlighter.Cell.ColumnIndex);

    AIndexDelta := Length(AReferenceAsString) - Reference.TextLength;
    AFormulaText := TdxSpreadSheetTableViewEditingController.GetEditingText(Edit);
    AFormulaText := Copy(AFormulaText, 1, Reference.PositionInText - 1) + AReferenceAsString +
      Copy(AFormulaText, Reference.PositionInText + Reference.TextLength, MaxInt);
    Reference.CellsArea := AReference;
    Reference.Bounds := Highlighter.GetAreaBounds(AReference);
    Reference.TextLength := Length(AReferenceAsString);

    Highlighter.CalculateColors;

    if AIndexDelta <> 0 then
    begin
      for I := ReferenceIndex + 1 to Highlighter.References.Count - 1 do
        Inc(Highlighter.References[I].PositionInText, AIndexDelta);
    end;

    Edit.LockChangeEvents(True, False);
    try
      Edit.EditValue := AFormulaText;
      if Edit is TcxCustomTextEdit then
        TcxCustomTextEdit(Edit).SelStart := Reference.PositionInText + Reference.TextLength - 1;
      if Edit is TcxCustomRichEdit then
        Highlighter.HighlightReferences(TcxCustomRichEdit(Edit));
    finally
      Edit.LockChangeEvents(False, False);
      TdxSpreadSheetTableViewAccess(View).DoEditChanged;
      TdxSpreadSheetTableViewEditingControllerAccess(TdxSpreadSheetTableViewAccess(View).EditingController).UpdateEditPosition;
    end;

    View.Invalidate;
  end;
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetEdit: TcxCustomEdit;
begin
  Result := TdxSpreadSheetTableViewAccess(View).EditingController.Edit;
end;

function TdxSpreadSheetTableViewReferenceHighlighterCustomDragAndDropObject.GetHighlighter: TdxSpreadSheetTableViewReferenceHighlighter;
begin
  Result := TdxSpreadSheetTableViewAccess(View).ViewInfo.ReferenceHighlighter as TdxSpreadSheetTableViewReferenceHighlighter;
end;

{ TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject }

procedure TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject.BeginDragAndDrop;
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
begin
  inherited BeginDragAndDrop;
  GetCellAtAbsolutePoint(CapturePoint, ARowIndex, AColumnIndex);

  ARowIndex := Max(Reference.CellsArea.Top, Min(Reference.CellsArea.Bottom, ARowIndex));
  AColumnIndex := Max(Reference.CellsArea.Left, Min(Reference.CellsArea.Right, AColumnIndex));

  FColumnDelta := Reference.CellsArea.Left - AColumnIndex;
  FRowDelta := Reference.CellsArea.Top - ARowIndex;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject.CheckRowAndColumnIndexes(
  var ARowIndex, AColumnIndex: Integer);
begin
  inherited CheckRowAndColumnIndexes(ARowIndex, AColumnIndex);
  ARowIndex := Min(ARowIndex, dxSpreadSheetMaxRowCount - dxSpreadSheetAreaHeight(Reference.CellsArea));
  AColumnIndex := Min(AColumnIndex, dxSpreadSheetMaxColumnCount - dxSpreadSheetAreaWidth(Reference.CellsArea));
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterMoveDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
begin
  Accepted := True;
  GetCellAtAbsolutePoint(P, ARowIndex, AColumnIndex);
  Inc(ARowIndex, FRowDelta);
  Inc(AColumnIndex, FColumnDelta);
  CheckRowAndColumnIndexes(ARowIndex, AColumnIndex);
  UpdateReference(cxRectSetOrigin(Reference.CellsArea, Point(AColumnIndex, ARowIndex)));
  inherited DragAndDrop(P, Accepted);
end;

{ TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject }

procedure TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject.AdjustRowAndColumnIndexes(
  const P: TPoint; var ARowIndex, AColumnIndex: Integer);

  procedure AdjustIndex(var AIndex: Integer; X, ACenterX: Integer; AIsForwardCorner: Boolean);
  begin
    if AIsForwardCorner then
    begin
      if X < ACenterX then
        Dec(AIndex);
    end
    else
      if X > ACenterX then
        Inc(AIndex);
  end;

var
  ACellCenter: TPoint;
begin
  ACellCenter := cxRectCenter(GetAbsoluteCellBounds(ARowIndex, AColumnIndex));
  AdjustIndex(AColumnIndex, P.X, ACellCenter.X, (Corner in [coTopRight, coBottomRight]) = (FCellsArea.Width >= 0));
  AdjustIndex(ARowIndex, P.Y, ACellCenter.Y, (Corner in [coBottomLeft, coBottomRight]) = (FCellsArea.Height >= 0));
  AdjustRowAndColumnIndexesByMergedCells(ARowIndex, AColumnIndex);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject.BeforeBeginDragAndDrop;
begin
  inherited BeforeBeginDragAndDrop;
  FCorner := TdxCorner(HiWord(HitTest.HitObjectData));
  FCellsArea := Reference.CellsArea;
end;

function TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject.CalculateReference(ARowIndex, AColumnIndex: Integer): TRect;
const
  HorzMirror: array[TdxCorner] of TdxCorner = (coBottomLeft, coBottomRight, coTopLeft, coTopRight);
  VertMirror: array[TdxCorner] of TdxCorner = (coTopRight, coTopLeft, coBottomRight, coBottomLeft);
begin
  Result := FCellsArea;
  if Corner in [coTopLeft, coTopRight] then
    Result.Top := ARowIndex;
  if Corner in [coTopLeft, coBottomLeft] then
    Result.Left := AColumnIndex;
  if Corner in [coTopRight, coBottomRight] then
    Result.Right := AColumnIndex;
  if Corner in [coBottomLeft, coBottomRight] then
    Result.Bottom := ARowIndex;

  if Result.Left > Result.Right then
    FCorner := VertMirror[Corner];
  if Result.Top > Result.Bottom then
    FCorner := HorzMirror[Corner];

  Result := dxSpreadSheetGetRealArea(Result);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterResizeDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
begin
  Accepted := True;
  GetCellAtAbsolutePoint(P, ARowIndex, AColumnIndex);
  AdjustRowAndColumnIndexes(P, ARowIndex, AColumnIndex);
  CheckRowAndColumnIndexes(ARowIndex, AColumnIndex);
  FCellsArea := CalculateReference(ARowIndex, AColumnIndex);
  UpdateReference(FCellsArea);
  inherited DragAndDrop(P, Accepted);
end;

{ TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject }

procedure TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject.AdjustRowAndColumnIndexes(
  const P: TPoint; var ARowIndex, AColumnIndex: Integer);
begin
  // do nothing
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject.BeforeBeginDragAndDrop;
begin
  inherited BeforeBeginDragAndDrop;
  FCorner := coBottomRight;
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  UpdateReference(Reference.CellsArea, True);
end;

procedure TdxSpreadSheetTableViewReferenceHighlighterAddAreaDragAndDropObject.InitializeReference;
var
  AColumn: Integer;
  ACursorPos: Integer;
  AIndex: Integer;
  AItem: TdxSpreadSheetTableViewReferenceHighlighterItem;
  ARow: Integer;
begin
  ACursorPos := (Edit as TcxCustomTextEdit).SelStart + 1;

  AIndex := Highlighter.References.Count - 1;
  while (AIndex >= 0) and (Highlighter.References[AIndex].PositionInText >= ACursorPos) do
    Dec(AIndex);

  if AIndex >= 0 then
  begin
    AItem := Highlighter.References[AIndex];
    if InRange(ACursorPos, AItem.PositionInText, AItem.PositionInText + AItem.TextLength - 1) then
      ACursorPos := AItem.PositionInText;
  end;

  View.CellAtPoint(HitTest.ActualHitPoint, ARow, AColumn);
  if (AColumn < 0) or (ARow < 0) then
    raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInternal), [ClassName]);
  AdjustRowAndColumnIndexesByMergedCells(ARow, AColumn);

  FReference := TdxSpreadSheetTableViewReferenceHighlighterItem.Create;
  FReference.PositionInText := ACursorPos;
  FReference.CellsArea := Bounds(AColumn, ARow, 0, 0);
  FReference.Bounds := Highlighter.GetAreaBounds(FReference.CellsArea);
  FReferenceIndex := AIndex + 1;
  Highlighter.References.Insert(FReferenceIndex, FReference);
end;

{ TdxSpreadSheetTableViewEditingCellReferenceHighlighter }

function TdxSpreadSheetTableViewEditingCellReferenceHighlighter.GetCell: TdxSpreadSheetCell;
begin
  if EditingController.IsEditing then
    Result := EditingController.Cell
  else
    Result := nil;
end;

function TdxSpreadSheetTableViewEditingCellReferenceHighlighter.GetEditingController: TdxSpreadSheetTableViewEditingController;
begin
  Result := TdxSpreadSheetTableViewAccess(View).EditingController;
end;

{ TdxSpreadSheetTableViewCustomSelectionDragAndDropObject }

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.DoProcessKeyDown(var Message: TWMKey): Boolean;
begin
  case Message.CharCode of
    VK_MENU, VK_LMENU, VK_RMENU,
    VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
    VK_CONTROL, VK_LCONTROL, VK_RCONTROL:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.HistoryActionBegin(
  AActionClass: TdxSpreadSheetHistoryActionClass);
var
  ASavedTargetArea: TRect;
begin
  ASavedTargetArea := TargetArea;
  UpdateSelection(OriginalArea);
  View.History.BeginAction(AActionClass);
  UpdateSelection(ASavedTargetArea);
end;

procedure TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.HistoryActionEnd;
begin
  View.History.EndAction;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.HasDataInArea(const AArea: TRect): Boolean;
var
  AResult: Boolean;
begin
  AResult := False;
  View.ForEachCell(AArea,
    procedure (ACell: TdxSpreadSheetCell)
    begin
      AResult := AResult or not ACell.IsEmpty;
    end);
  Result := AResult;
end;

procedure TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.UpdateSelection(const R: TRect);
begin
  View.Selection.Clear;
  View.Selection.Add(R);
end;

procedure TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.BeginDragAndDrop;
var
  R: TRect;
begin
  inherited BeginDragAndDrop;
  R := GetSelectionArea;
  GetCellAtAbsolutePoint(CurMousePos, FCapturedCell.Y, FCapturedCell.X);
  FCapturedCell.X := Max(Min(FCapturedCell.X, R.Right), R.Left);
  FCapturedCell.Y := Max(Min(FCapturedCell.Y, R.Bottom), R.Top);
  ClipboardArea.Initialize(View, R);
end;

procedure TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  try
    if Accepted and not cxRectIsEqual(OriginalArea, TargetArea) then
    try
      ApplyChanges;
    except
      UpdateSelection(OriginalArea);
      raise;
    end
    else
      UpdateSelection(OriginalArea);
  finally
    ClipboardArea.Reset;
    inherited EndDragAndDrop(Accepted);
  end;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.GetSelectionArea: TRect;
begin
  Result := View.Selection.Area;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.GetClipboardArea: TdxSpreadSheetClipboardArea;
begin
  Result := TdxCustomSpreadSheetAccess(Control).ClipboardArea;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.GetOriginalArea: TRect;
begin
  Result := ClipboardArea.Area;
end;

function TdxSpreadSheetTableViewCustomSelectionDragAndDropObject.GetTargetArea: TRect;
begin
  Result := View.Selection.Area;
end;

{ TdxSpreadSheetTableViewAutoFillDragAndDropObject }

class function TdxSpreadSheetTableViewAutoFillDragAndDropObject.CalculateDirection(
  const ASourceArea, ATargetArea: TRect): TcxDirection;
begin
  if cxPointIsEqual(ASourceArea.BottomRight, ATargetArea.BottomRight) then
    Result := CalculateDirection(ASourceArea, ATargetArea.Top, ATargetArea.Left)
  else
    if cxPointIsEqual(ASourceArea.TopLeft, ATargetArea.TopLeft) then
      Result := CalculateDirection(ASourceArea, ATargetArea.Bottom, ATargetArea.Right)
    else
      Result := dirNone;
end;

class function TdxSpreadSheetTableViewAutoFillDragAndDropObject.CalculateDirection(
  const ASourceArea: TRect; ARow, AColumn: Integer): TcxDirection;

  function GetDelta(AValue, ALeft, ARight: Integer): Integer;
  begin
    if AValue < ALeft  then
      Result := ALeft - AValue
    else
      if AValue > ARight then
        Result := AValue - ARight
      else
        Result := 0;
  end;

var
  H, V: Integer;
begin
  H := GetDelta(AColumn, ASourceArea.Left, ASourceArea.Right);
  V := GetDelta(ARow, ASourceArea.Top, ASourceArea.Bottom);
  if (H = 0) and (V = 0) then
    Result := dirNone
  else
    if H >= V then
    begin
      if AColumn < ASourceArea.Left then
        Result := dirLeft
      else
        Result := dirRight
    end
    else
      if ARow < ASourceArea.Top then
        Result := dirUp
      else
        Result := dirDown
end;

procedure TdxSpreadSheetTableViewAutoFillDragAndDropObject.ApplyChanges;
begin
  HistoryActionBegin(TdxSpreadSheetHistoryAutoFillAction);
  try
    View.FillData(OriginalArea, TargetArea);
  finally
    HistoryActionEnd;
  end;
end;

function TdxSpreadSheetTableViewAutoFillDragAndDropObject.CalculateTargetSelection(ARow, AColumn: Integer): TRect;
begin
  Result := OriginalArea;
  case FDirection of
    dirLeft:
      Result.Left := AColumn;
    dirRight:
      Result.Right := AColumn;
    dirUp:
      Result.Top := ARow;
    dirDown:
      Result.Bottom := ARow;
  else
    Result.BottomRight := cxPoint(AColumn, ARow);
  end;
end;

procedure TdxSpreadSheetTableViewAutoFillDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  CheckScrollArea(GetClientCursorPos);
  View.HitTest.Calculate(GetClientCursorPos);

  if View.HitTest.HitAtCell then
  begin
    FDirection := CalculateDirection(OriginalArea,
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Row,
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Column);
    UpdateSelection(CalculateTargetSelection(
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Row,
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Column));
  end;

  Accepted := FDirection <> dirNone;
  inherited DragAndDrop(P, Accepted);
end;

function TdxSpreadSheetTableViewAutoFillDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := crdxSpreadSheetCross;
end;

{ TdxSpreadSheetTableViewMoveSelectionDragAndDropObject }

procedure TdxSpreadSheetTableViewMoveSelectionDragAndDropObject.ApplyChanges;
var
  AHelper: TdxSpreadSheetTableViewMoveCellsModificationHelper;
  AMoveMode: Boolean;
  AStream: TMemoryStream;
begin
  if not dxSpreadSheetIsValidArea(TargetArea) then
    Exit;

  AMoveMode := not (ssCtrl in KeyboardStateToShiftState);
  CheckCanApplyChanges(AMoveMode);

  HistoryActionBegin(TdxSpreadSheetHistoryDragAndDropAction);
  try
    if AMoveMode then
    begin
      AHelper := TdxSpreadSheetTableViewMoveCellsModificationHelper.Create(View);
      try
        AHelper.Process(OriginalArea, TargetArea.TopLeft);
      finally
        AHelper.Free;
      end;
    end
    else
    begin
      AStream := TMemoryStream.Create;
      try
        View.CopyCellDataToStream(OriginalArea, AStream);
        AStream.Position := 0;
        View.PasteCellDataFromStream(TargetArea.TopLeft, AStream);
      finally
        AStream.Free;
      end;
    end;
  finally
    HistoryActionEnd;
  end;
end;

procedure TdxSpreadSheetTableViewMoveSelectionDragAndDropObject.CheckCanApplyChanges(AMoveMode: Boolean);

  procedure CheckArea(const AArea: TRect);
  begin
    TdxSpreadSheetTableViewAccess(View).CheckProtection(cmmClear, AArea);
  end;

  procedure CheckSelection;
  begin
    if (View.Controller.FocusedContainer <> nil) or (View.Selection.Count <> 1) then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorInvalidSelection));
  end;

begin
  CheckSelection;
  CheckArea(TargetArea);
  if AMoveMode then
    CheckArea(OriginalArea);
  if HasDataInArea(TargetArea) then
  begin
    if MessageDlg(cxGetResourceString(@sdxReplaceCellsDataConfirmation), mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
      Abort;
  end;
end;

procedure TdxSpreadSheetTableViewMoveSelectionDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AArea: TRect;
  ADelta: TPoint;
begin
  CheckScrollArea(GetClientCursorPos);
  View.HitTest.Calculate(GetClientCursorPos);
  Accepted := View.HitTest.HitAtCell;

  if Accepted then
  begin
    ADelta := cxPoint(
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Column - CapturedCell.X,
      TdxSpreadSheetTableViewCellViewInfo(View.HitTest.HitObject).Row - CapturedCell.Y);
    if dxSpreadSheetIsEntireRow(OriginalArea) then
      ADelta.X := 0;
    if dxSpreadSheetIsEntireColumn(OriginalArea) then
      ADelta.Y := 0;

    AArea := cxRectOffset(OriginalArea, ADelta);
    if not dxSpreadSheetIsEntireRowOrColumn(AArea) then
      AArea := View.MergedCells.ExpandArea(AArea);
    AArea := cxRectBounds(AArea.Left, AArea.Top, cxRectWidth(OriginalArea), cxRectHeight(OriginalArea));
    UpdateSelection(AArea);
  end;

  inherited DragAndDrop(P, Accepted);
end;

function TdxSpreadSheetTableViewMoveSelectionDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := crSizeAll;
end;

{ TdxSpreadSheetTableViewCustomSelectionViewInfo }

function TdxSpreadSheetTableViewCustomSelectionViewInfo.GetPopupMenuClass(
  AHitTest: TdxSpreadSheetCustomHitTest): TComponentClass;
begin
  Result := TdxSpreadSheetBuiltInTableViewPopupMenu;
end;

function TdxSpreadSheetTableViewCustomSelectionViewInfo.GetView: TdxSpreadSheetCustomView;
begin
  Result := TdxSpreadSheetTableView(Owner);
end;

procedure TdxSpreadSheetTableViewCustomSelectionViewInfo.Invalidate;
begin
  InvalidateRect(cxRectInflate(Bounds, dxSpreadSheetSelectionThickness));
end;

{ TdxSpreadSheetTableViewSelectionFrameViewInfo }

procedure TdxSpreadSheetTableViewSelectionFrameViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
begin
  ACanvas.SaveClipRegion;
  try
    TdxSpreadSheetSelectionHelper.Draw(ACanvas, Bounds,
      TdxSpreadSheetTableViewAccess(View).ViewInfo.SelectionParams.Color,
      TdxSpreadSheetTableViewAccess(View).ViewInfo.ContentParams.Color,
      [ssseFrame, ssseCorners], [coBottomRight]);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.GetCursor(AHitTest: TdxSpreadSheetCustomHitTest): TCursor;
begin
  if (AHitTest.HitObjectData = 0) and CanMoveSelection then
    Result := crSizeAll
  else
    if (AHitTest.HitObjectData <> 0) and CanFillData then
      Result := crdxSpreadSheetCross
    else
      Result := inherited GetCursor(AHitTest);
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.CanDraw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage): Boolean;
begin
  Result := inherited CanDraw(ACanvas, AStage) and TdxSpreadSheetTableViewAccess(View).ViewInfo.CanDrawCellSelection;
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.CanFillData: Boolean;
begin
  Result := TdxSpreadSheetTableViewAccess(View).CanFillData and SpreadSheet.OptionsBehavior.DragFilling;
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.CanMoveSelection: Boolean;
begin
  Result := SpreadSheet.OptionsBehavior.DragMoving and SpreadSheet.OptionsBehavior.Editing and
    (TdxSpreadSheetTableViewAccess(View).Selection.Count = 1);
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.GetDragAndDropObjectClass(
  AHitTest: TdxSpreadSheetCustomHitTest): TcxDragAndDropObjectClass;
begin
  if (AHitTest.HitObjectData = 0) and CanMoveSelection then
    Result := TdxSpreadSheetTableViewMoveSelectionDragAndDropObject
  else
    if (AHitTest.HitObjectData <> 0) and CanFillData then
      Result := TdxSpreadSheetTableViewAutoFillDragAndDropObject
    else
      Result := nil;
end;

function TdxSpreadSheetTableViewSelectionFrameViewInfo.InitHitTest(AHitTest: TdxSpreadSheetCustomHitTest): Boolean;
var
  ACornerBounds: TRect;
begin
  Result := False;
  if TdxSpreadSheetTableViewAccess(View).ViewInfo.CanDrawCellSelection then
  begin
    Result := TdxSpreadSheetSelectionHelper.IsInFrame(Bounds, AHitTest.ActualHitPoint);
    if Result then
    begin
      ACornerBounds := TdxSpreadSheetSelectionHelper.GetCornerBounds(Bounds, coBottomRight);
      ACornerBounds := cxRectCenter(ACornerBounds, dxSpreadSheetCornetHitTestZoneSize, dxSpreadSheetCornetHitTestZoneSize);
      TdxSpreadSheetCustomHitTestAccess(AHitTest).HitObject := Self;
      TdxSpreadSheetCustomHitTestAccess(AHitTest).HitObjectData := Ord(PtInRect(ACornerBounds, AHitTest.ActualHitPoint));
      TdxSpreadSheetCustomHitTestAccess(AHitTest).SetHitCode(hcSelectionFrame, True);
    end;
  end;
end;

{ TdxSpreadSheetTableViewCustomPrintAreaViewInfo }

procedure TdxSpreadSheetTableViewCustomPrintAreaViewInfo.Draw(ACanvas: TcxCanvas; AStage: TdxSpreadSheetDrawingStage);
var
  ABounds: TRect;
  APrevForm: TXForm;
begin
  ABounds := Bounds;
  dxSpreadSheetResetZoomFactor(ACanvas, ABounds, APrevForm);
  try
    DrawCore(ACanvas, ABounds, AStage);
  finally
    dxSpreadSheetRestoreZoomFactor(ACanvas, APrevForm);
  end;
end;

function TdxSpreadSheetTableViewCustomPrintAreaViewInfo.GetBackgroundColor: TColor;
begin
  Result := TdxSpreadSheetTableViewAccess(View).ViewInfo.ContentParams.Color;
end;

function TdxSpreadSheetTableViewCustomPrintAreaViewInfo.GetBorderSize: Integer;
begin
  Result := dxSpreadSheetBorderStyleThickness[BorderStyle];
end;

function TdxSpreadSheetTableViewCustomPrintAreaViewInfo.GetColor: TColor;
begin
  Result := cxGetActualColor(SpreadSheet.OptionsView.PrintAreaColor, LookAndFeelPainter.SpreadSheetContentTextColor);
end;

function TdxSpreadSheetTableViewCustomPrintAreaViewInfo.GetBorderStyle: TdxSpreadSheetCellBorderStyle;
begin
  Result := sscbsDashed;
end;

{ TdxSpreadSheetTableViewPageBreakViewInfo }

procedure TdxSpreadSheetTableViewPageBreakViewInfo.CalculateBounds;
begin
  inherited;
  if Horizontal then
    FBounds := cxRectCenterVertically(FBounds, BorderSize)
  else
    FBounds := cxRectCenterHorizontally(FBounds, BorderSize);
end;

procedure TdxSpreadSheetTableViewPageBreakViewInfo.DrawCore(
  ACanvas: TcxCanvas; const R: TRect; AStage: TdxSpreadSheetDrawingStage);
begin
  dxSpreadSheetDrawBorder(ACanvas, R, Color, BackgroundColor, BorderStyle, Horizontal);
end;

function TdxSpreadSheetTableViewPageBreakViewInfo.GetHorizontal: Boolean;
begin
  Result := cxRectWidth(FBounds) > cxRectHeight(FBounds);
end;

{ TdxSpreadSheetTableViewPrintAreaViewInfo }

procedure TdxSpreadSheetTableViewPrintAreaViewInfo.CalculateBounds;
begin
  inherited;
  FBounds := cxRectOffset(FBounds, -1, -1);
end;

procedure TdxSpreadSheetTableViewPrintAreaViewInfo.DrawCore(
  ACanvas: TcxCanvas; const R: TRect; AStage: TdxSpreadSheetDrawingStage);
begin
  dxSpreadSheetDrawBorders(ACanvas, R, Color, BackgroundColor, BorderStyle);
end;

end.
