{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressVerticalGrid                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSVERTICALGRID AND ALL           }
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
unit cxVGridViewInfo;

{$I cxVer.inc}

interface

uses
  Types, Classes, Graphics, Windows, Controls, cxClasses,
  cxGraphics, cxVGrid, cxVGridUtils, cxEdit, cxInplaceContainer, cxStyles;

type
  { TcxCategoryRowHeaderInfo }

  TcxCategoryRowHeaderInfo = class(TcxCustomRowHeaderInfo)
  private
    function GetRow: TcxCategoryRow;
  protected
    procedure AddBottomHorzLine(const R: TRect); override;
    procedure AddRightVertLine(const R: TRect); override;
    procedure CalcRowCaptionsInfo; override;
    procedure CalcViewParams(AAllowFocus: Boolean); override;
    procedure DoCalcExpandButton; override;
    function GetCaptionViewParams: TcxViewParams; override;
    function GetButtonColor: TColor; override;
    function GetFocusRect: TRect; override;
    function IncreaseBoundsByLastVertLine: Boolean; override;
    function LeftViewPoint: Integer; override;
  public
    property Row: TcxCategoryRow read GetRow;
  end;

  { TcxCategoryRowViewInfo }

  TcxCategoryRowViewInfo = class(TcxCustomRowViewInfo)
  private
    function GetRow: TcxCategoryRow;
  protected
    procedure CalcRowHeaderInfo(ANextRow: TcxCustomRow); override;
  public
    property Row: TcxCategoryRow read GetRow;
  end;

  { TcxEditorRowHeaderInfo }

  TcxEditorRowHeaderInfo = class(TcxCustomRowHeaderInfo)
  private
    function GetRow: TcxEditorRow;
  public
    property Row: TcxEditorRow read GetRow;
  end;

  { TcxEditorRowViewInfo }

  TcxEditorRowViewInfo = class(TcxCustomRowViewInfo)
  private
    function GetRow: TcxEditorRow;
  protected
    procedure CalcValuesInfo; override;
    function GetValuesHeight(ABandWidth: Integer; AViewInfo: TcxvgCustomViewInfo): Integer; override;
  public
    property Row: TcxEditorRow read GetRow;
  end;

  { TcxMultiEditorRowHeaderInfo }

  TcxMultiEditorRowHeaderInfo = class(TcxCustomRowHeaderInfo)
  private
		FSeparatorRects: TcxRectList;
		FSeparatorInfo: TSeparatorInfo;
    function GetRow: TcxMultiEditorRow;
  protected
    procedure CalcRowCaptionsInfo; override;
    procedure CalcSeparatorRects(AHeaderCells: TcxRectList); virtual;
    procedure CalcSeparatorWidth(ASeparatorWidth: Integer); virtual;
    procedure CalcSeparatorStyle; virtual;
    procedure Clear; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    constructor Create(ARow: TcxCustomRow); override;
    destructor Destroy; override;
    property Row: TcxMultiEditorRow read GetRow;
		property SeparatorInfo: TSeparatorInfo read FSeparatorInfo;
    property SeparatorRects: TcxRectList read FSeparatorRects;
  end;

  { TcxMultiEditorRowViewInfo }

  TcxMultiEditorRowViewInfo = class(TcxCustomRowViewInfo)
  private
    FSeparatorRects: TcxRectList;
    function GetHeaderInfo: TcxMultiEditorRowHeaderInfo;
    function GetRow: TcxMultiEditorRow;
    function GetSeparatorRects: TcxRectList;
  protected
    procedure AddSeparatorRects(AHeader: Boolean; ASepRects: TcxRectList;
      ASeparatorKind: TSeparatorKind; AValueCells: TcxRectList;
      const ABounds: TRect);
    procedure CalcValuesInfo; override;
    procedure CheckEmptyValuesInfo; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetValuesHeight(ABandWidth: Integer; AViewInfo: TcxvgCustomViewInfo): Integer; override;
  public
    constructor Create(ARow: TcxCustomRow); override;
    destructor Destroy; override;
    class function GetCellRects(ARow: TcxMultiEditorRow; const R: TRect;
      ASepWidth: Integer): TcxRectList;
    property HeaderInfo: TcxMultiEditorRowHeaderInfo read GetHeaderInfo;
    property Row: TcxMultiEditorRow read GetRow;
    property SeparatorRects: TcxRectList read GetSeparatorRects;
  end;

  { TcxInspectorViewInfo }

  TcxInspectorViewInfo = class(TcxvgCustomViewInfo)
  protected
    procedure AddEmptyRects; override;
    procedure CheckMaxRowHeaderWidth(var Value: Integer; AValueMinWidth: Integer); override;
    function GetViewValueWidth: Integer; override;
    function GetVisibleValueCount: Integer; override;
  end;

  { TcxBandsViewInfo }

  TcxBandsViewInfo = class(TcxvgCustomViewInfo)
  private
    FLock: Boolean;
    function GetAutoScaleBands: Boolean;
    function GetBandIndentWidth(ABandCount: Integer): Integer;
    function GetBandsInterval: Integer;
    procedure UpdateBandsWidth;
  protected
    procedure AddEmptyRects; override;
    procedure CalcBandRects; override;
    procedure CalcBandWidth; override;
    procedure CalculateBandsInfo; override;
    function CanAddRowToBand(const ARowRect, ABandRect: TRect; ABandRowIndex: Integer): Boolean; override;
    procedure CheckMaxRowHeaderWidth(var Value: Integer; AValueMinWidth: Integer); override;
    procedure CreateBand(ABandHeight, ABandWidth: Integer); override;
    function GetBandSizeableRect(const ABandRect: TRect): TRect; override;
    function GetMinViewBandWidth: Integer;
    function GetRowAutoHeight(ARow: TcxCustomRow): Boolean; override;
    function GetViewBandWidth: Integer; override;
    function GetViewValueWidth: Integer; override;
    function GetVisibleValueCount: Integer; override;
    procedure SetDividerPos(APos: Integer); override;

    property AutoScaleBands: Boolean read GetAutoScaleBands;
    property BandsIndent: Integer read GetBandsInterval;
  end;

  { TcxMultiRecordViewInfo }

  TcxMultiRecordViewInfo = class(TcxvgCustomViewInfo)
  private
		FCommonWidth: Integer;
    FRecordsInterval: Integer;
    FVisibleValueCount: Integer;
    function CalcVisibleValueCount: Integer;
    function GetRecordCount: Integer;
    function GetVerticalGrid: TcxVirtualVerticalGrid;
  protected
    procedure AddBandRowsLines(const R: TRect); override;
    procedure AddEmptyRects; override;
    procedure AddRightValueSide(ARowViewInfo: TcxCustomRowViewInfo; const R: TRect; ALast: Boolean); override;
    procedure CalcBandWidth; override;
    procedure CalcLayoutGeneral; override;
    function CanUpdateRecord(ARecordIndex: Integer): Boolean; override;
    procedure Clear; override;
    function GetBandSizeableRect(const ABandRect: TRect): TRect; override;
    function GetPixelScrollContentSize: Integer; override;
    function GetLTRValueRect(AValueIndex: Integer; ARowViewInfo: TcxCustomRowViewInfo): TRect; override;
    function GetVisibleValueCount: Integer; override;
    procedure Reset; override;
    procedure SetDividerPos(APos: Integer); override;
    property RecordCount: Integer read GetRecordCount;
  public
    constructor Create(AOwner: TcxEditingControl); override;
    function GetDefaultGridModeBufferCount: Integer; override;

    property RecordsInterval: Integer read FRecordsInterval;
    property VerticalGrid: TcxVirtualVerticalGrid read GetVerticalGrid;
  end;

implementation

uses
  SysUtils, Math, cxControls, cxGeometry, cxEditUtils, Forms;

type
  TcxvgCustomViewInfoAccess = class(TcxvgCustomViewInfo);

{ TcxCategoryRowHeaderInfo }

procedure TcxCategoryRowHeaderInfo.AddBottomHorzLine(const R: TRect);
begin
  with ViewInfo, R do
    if HorzLineWidth > 0 then
      Self.LinesInfo.Add(Left - VertLineWidth, Bottom,
        Right - Left + 2 * VertLineWidth, HorzLineWidth, HorzLineBrush);
end;

procedure TcxCategoryRowHeaderInfo.AddRightVertLine(const R: TRect);
begin
  with ViewInfo, Row.ViewInfo.RowRect do
    if VertLineWidth > 0 then
      Self.LinesInfo.Add(Right, Top, VertLineWidth,
        Bottom - Top + HorzLineWidth, BandBorderColor);
end;

procedure TcxCategoryRowHeaderInfo.CalcRowCaptionsInfo;
begin
  if ViewInfo.UseCategoryExplorerStyle and not cxRectIsEmpty(ButtonRect) then
    FHeaderCellsRect.Right := FButtonRect.Left;
  inherited CalcRowCaptionsInfo;
end;

procedure TcxCategoryRowHeaderInfo.CalcViewParams(AAllowFocus: Boolean);
begin
  FIndentViewParams := Row.Styles.GetHeaderParams(Row);
  ViewParams := IndentViewParams;
end;

procedure TcxCategoryRowHeaderInfo.DoCalcExpandButton;
begin
  if Row.HasVisibleChildren then
    if ViewInfo.UseCategoryExplorerStyle then
    begin
      FButtonRect := cxRectCenter(cxRect(HeaderRect.Right - ViewInfo.ExplorerButtonSize.cx - 2,
        HeaderRect.Top, HeaderRect.Right, HeaderRect.Bottom), ViewInfo.ExplorerButtonSize.cx, ViewInfo.ExplorerButtonSize.cy);
      FButtonAreaBounds := cxRectCenter(cxRect(HeaderRect.Right - ViewInfo.ExplorerButtonAreaSize.cx - 2,
        HeaderRect.Top, HeaderRect.Right, HeaderRect.Bottom), ViewInfo.ExplorerButtonAreaSize.cx, ViewInfo.ExplorerButtonAreaSize.cy);
    end
    else
      inherited DoCalcExpandButton;
end;

function TcxCategoryRowHeaderInfo.GetCaptionViewParams: TcxViewParams;
begin
  Result := ViewParams;
end;

function TcxCategoryRowHeaderInfo.GetButtonColor: TColor;
begin
  if PaintStyle = psDotNet then
    Result := ViewParams.Color
  else
    Result := inherited GetButtonColor;
end;

function TcxCategoryRowHeaderInfo.GetFocusRect: TRect;
begin
  Result := ViewInfo.CalcHelper.GetCategoryFocusRect(Self);
end;

function TcxCategoryRowHeaderInfo.IncreaseBoundsByLastVertLine: Boolean;
begin
  Result := True;
end;

function TcxCategoryRowHeaderInfo.LeftViewPoint: Integer;
begin
  if PaintStyle = psDotNet then
    Result := IndentBounds.Right
  else
    Result := inherited LeftViewPoint;
end;

function TcxCategoryRowHeaderInfo.GetRow: TcxCategoryRow;
begin
  Result := TcxCategoryRow(FRow);
end;

{ TcxCategoryRowViewInfo }

procedure TcxCategoryRowViewInfo.CalcRowHeaderInfo(ANextRow: TcxCustomRow);
begin
  HeaderInfo.HeaderRect := RowRect;
  with ValuesRect do
    ValuesRect := cxNullRect;
  inherited CalcRowHeaderInfo(ANextRow);
end;

function TcxCategoryRowViewInfo.GetRow: TcxCategoryRow;
begin
  Result := TcxCategoryRow(inherited Row);
end;

{ TcxEditorRowHeaderInfo }

function TcxEditorRowHeaderInfo.GetRow: TcxEditorRow;
begin
  Result := TcxEditorRow(FRow);
end;

{ TcxEditorRowViewInfo }

procedure TcxEditorRowViewInfo.CalcValuesInfo;
var
  ABottomLineNeeded: Boolean;
  ANextRow: TcxCustomRow;
  I, ACount: Integer;
  R, ALRTClipRect: TRect;
begin
  inherited CalcValuesInfo;
  ACount := ViewInfo.VisibleValueCount;
  for I := 0 to ACount - 1 do
  begin
    R := TcxvgCustomViewInfoAccess(ViewInfo).GetLTRValueRect(I, Self);
    ANextRow := VerticalGrid.NextVisibleRow(Row);
    ABottomLineNeeded := ViewInfo.CalcHelper.IsBottomLineNeeded(ANextRow);
    ALRTClipRect := ViewInfo.ClipRect;
    if NeedLTRBounds then
      ALRTClipRect := TdxRightToLeftLayoutConverter.ConvertRect(ALRTClipRect, RightToLeftConversionClientBounds);
    AddRectValueLines(R, (I = ACount - 1) or (R.Right >= ALRTClipRect.Right), ABottomLineNeeded);
    Assert(I <= ValuesInfo.Count, 'Wrong value index');
    ViewInfo.CalcEditCell(R, ValuesInfo[I])
  end;
end;

function TcxEditorRowViewInfo.GetValuesHeight(ABandWidth: Integer;
  AViewInfo: TcxvgCustomViewInfo): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ValuesInfo.Count - 1 do
    Result := Max(Result, ValuesInfo[I].GetHeight(AViewInfo.ViewValueWidth));
end;

function TcxEditorRowViewInfo.GetRow: TcxEditorRow;
begin
  Result := TcxEditorRow(inherited Row);
end;

{ TcxMultiEditorRowHeaderInfo }

constructor TcxMultiEditorRowHeaderInfo.Create(ARow: TcxCustomRow);
begin
  inherited Create(ARow);
  FSeparatorRects := TcxRectList.Create;
end;

destructor TcxMultiEditorRowHeaderInfo.Destroy;
begin
  FSeparatorRects.Free;
  inherited Destroy;
end;

procedure TcxMultiEditorRowHeaderInfo.CalcRowCaptionsInfo;
var
  I: Integer;
  R: TRect;
  ARects: TcxRectList;
  ACaptionInfo: TcxRowCaptionInfo;
begin
  CalcSeparatorWidth(ViewInfo.DividerWidth);
  CalcSeparatorStyle;
  ARects := TcxMultiEditorRowViewInfo.GetCellRects(Row, HeaderCellsRect, FSeparatorInfo.Width);
  if ARects <> nil then
  try
    for I := 0 to ARects.Count - 1 do
    begin
      R := ARects[I];
      if R.Left < HeaderCellsRect.Right then
      begin
        ACaptionInfo := CalcCaptionInfo(Row.Properties.Editors[I], R);
        ACaptionInfo.RowCellIndex := I;
        CaptionsInfo.Add(ACaptionInfo);
      end;
    end;
		CalcSeparatorRects(ARects);
  finally
    ARects.Free;
  end;
end;

procedure TcxMultiEditorRowHeaderInfo.CalcSeparatorRects(
  AHeaderCells: TcxRectList);
begin
  TcxMultiEditorRowViewInfo(Row.ViewInfo).AddSeparatorRects(
    True, SeparatorRects, SeparatorInfo.Kind, AHeaderCells, HeaderCellsRect);
end;

procedure TcxMultiEditorRowHeaderInfo.CalcSeparatorWidth(
  ASeparatorWidth: Integer);
begin
  with FSeparatorInfo do
  begin
    Kind := Row.Properties.SeparatorKind;
    Caption := Row.Properties.SeparatorString;
    if Kind = skVertLine then
      Width := ASeparatorWidth
    else
      Width := cxTextWidth(Self.ViewParams.Font, SeparatorInfo.Caption + '0');
  end;
end;

procedure TcxMultiEditorRowHeaderInfo.CalcSeparatorStyle;
begin
  FSeparatorInfo.ViewParams := ViewParams;
  with Row.Properties do
    FSeparatorInfo.TextFlags := GetTextAligmentFlags(taCenter, SeparatorAlignmentVert);
end;

procedure TcxMultiEditorRowHeaderInfo.Clear;
begin
  inherited Clear;
  FSeparatorRects.Clear;
end;

procedure TcxMultiEditorRowHeaderInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FSeparatorRects.RightToLeftConversion(AClientBounds);
end;

function TcxMultiEditorRowHeaderInfo.GetRow: TcxMultiEditorRow;
begin
  Result := TcxMultiEditorRow(FRow);
end;

{ TcxMultiEditorRowViewInfo }

constructor TcxMultiEditorRowViewInfo.Create(ARow: TcxCustomRow);
begin
  inherited Create(ARow);
  FSeparatorRects := TcxRectList.Create;
end;

destructor TcxMultiEditorRowViewInfo.Destroy;
begin
  FreeAndNil(FSeparatorRects);
  inherited Destroy;
end;

class function TcxMultiEditorRowViewInfo.GetCellRects(ARow: TcxMultiEditorRow;
  const R: TRect; ASepWidth: Integer): TcxRectList;
var
  I, ACount, AMinWidth: Integer;
begin
  Result := nil;
  AMinWidth := ARow.VerticalGrid.OptionsView.RowHeaderMinWidth;
  ACount := ARow.Properties.Editors.Count;
  with TRectScaler.Create do
  try
    for I := 0 to ACount - 1 do
      Add(ARow.Properties.Editors[I].Width, AMinWidth, cxSetValue(I = ACount - 1, 0, ASepWidth));
    if ARow.Properties.Fixed then
      CalcRect(R)
    else
      ScaleRect(R);
    if ScaledRects.Count > 0 then
    begin
      Result := TcxRectList.Create;
      Result.Assign(ScaledRects);
    end;
  finally
    Free;
  end;
end;

procedure TcxMultiEditorRowViewInfo.AddSeparatorRects(
  AHeader: Boolean; ASepRects: TcxRectList; ASeparatorKind: TSeparatorKind;
  AValueCells: TcxRectList; const ABounds: TRect);
var
  I: Integer;
  R: TRect;
begin
  for I := 0 to AValueCells.Count - 2 do
  begin
    with AValueCells[I] do
      if ASeparatorKind = skString then
      begin
        R := cxRect(Right, Top, AValueCells[I + 1].Left, Bottom);
        if cxRectIntersect(R, R, ABounds) then
          ASepRects.Add(R)
      end
      else
      begin
        R := cxRect(Right - ViewInfo.DividerWidth, Top, Right, Bottom);
        if cxRectIntersect(R, R, ABounds) then
        begin
          if AHeader then
            ViewInfo.CalcHelper.AddDivider(HeaderInfo.LinesInfo, R, HeaderInfo.ViewParams.Color, True)
          else
          begin
            if not ViewInfo.CalcHelper.IsBottomLineNeeded(VerticalGrid.NextVisibleRow(Row)) then
              Dec(R.Bottom);
            if R.Left > HeaderInfo.HeaderRect.Right then
              ViewInfo.CalcHelper.AddDivider(ViewInfo.LinesInfo, R, HeaderInfo.ViewParams.Color, True);
          end;
        end;
      end;
  end;
end;

procedure TcxMultiEditorRowViewInfo.CalcValuesInfo;
var
  I, J, ASepWidth, ACount, AValuesCount, AEditContainerCount: Integer;
  ABottomLineNeeded: Boolean;
  ANextRow: TcxCustomRow;
  ARects: TcxRectList;
  ARowValueInfo: TcxRowValueInfo;
  ASepKind: TSeparatorKind;
  AValueRect, R: TRect;
begin
  inherited CalcValuesInfo;
  SeparatorRects.Clear;
  with HeaderInfo.SeparatorInfo do
  begin
    ASepWidth := Width;
    ASepKind := Kind;
  end;
  with ViewInfo do
  begin
    AValuesCount := VisibleValueCount;
    AEditContainerCount := GetEditContainerCount;
    for I := 0 to AValuesCount - 1 do
    begin
      AValueRect := TcxvgCustomViewInfoAccess(ViewInfo).GetLTRValueRect(I, Self);
      ARects := GetCellRects(Row, AValueRect, ASepWidth);
      ANextRow := VerticalGrid.NextVisibleRow(Row);
      ABottomLineNeeded := CalcHelper.IsBottomLineNeeded(ANextRow);
      if ARects <> nil then
      begin
        ACount := Min(ARects.Count, AEditContainerCount);
        try
          for J := 0 to ACount - 1 do
          begin
            R := ARects[J];
            if not cxRectIsEmpty(R) then
            begin
              ARowValueInfo := ValuesInfo[I * AEditContainerCount + J];
              CalcEditCell(R, ARowValueInfo);
            end;
          end;
          AddSeparatorRects(False, SeparatorRects, ASepKind, ARects, AValueRect);
        finally
          ARects.Free;
        end;
      end;
      AddRectValueLines(AValueRect, I = AValuesCount - 1, ABottomLineNeeded);
    end;
  end;
  CheckEmptyValuesInfo;
end;

procedure TcxMultiEditorRowViewInfo.CheckEmptyValuesInfo;
begin
  if (Row.Properties.Editors.Count = 0) and (cxRectWidth(ValuesRect) > 0) then
  begin
    HeaderInfo.LinesInfo.Add(RowRect, HeaderInfo.ViewParams.Color);
    AddRectValueLines(ValuesRect, True, True);
  end;
end;

procedure TcxMultiEditorRowViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  SeparatorRects.RightToLeftConversion(AClientBounds);
end;

function TcxMultiEditorRowViewInfo.GetValuesHeight(ABandWidth: Integer;
  AViewInfo: TcxvgCustomViewInfo): Integer;
var
  I, J, ASepWidth, ACount, AValuesCount: Integer;
  AValueRect, R: TRect;
  ARects: TcxRectList;
  ARowValueInfo: TcxRowValueInfo;
begin
  Result := -1;
  ASepWidth := HeaderInfo.SeparatorInfo.Width;
  with AViewInfo do
  begin
    AValuesCount := VisibleValueCount;
    for I := 0 to AValuesCount - 1 do
    begin
      AValueRect := TcxvgCustomViewInfoAccess(AViewInfo).GetLTRValueRect(I, Self);
      ARects := GetCellRects(Row, AValueRect, ASepWidth);
      if ARects <> nil then
      begin
        ACount := Min(ARects.Count, GetEditContainerCount);
        try
          for J := 0 to ACount - 1 do
          begin
            R := ARects[J];
            if not cxRectIsEmpty(R) then
            begin
              ARowValueInfo := ValuesInfo[I * GetEditContainerCount + J];
              Result := Max(Result, ARowValueInfo.GetHeight(R.Right - R.Left));
            end;
          end;
        finally
          ARects.Free;
        end;
      end;
    end;
  end;
end;

function TcxMultiEditorRowViewInfo.GetHeaderInfo: TcxMultiEditorRowHeaderInfo;
begin
  Result := TcxMultiEditorRowHeaderInfo(inherited HeaderInfo);
end;

function TcxMultiEditorRowViewInfo.GetRow: TcxMultiEditorRow;
begin
  Result := TcxMultiEditorRow(inherited Row);
end;

function TcxMultiEditorRowViewInfo.GetSeparatorRects: TcxRectList;
begin
  Result := FSeparatorRects;
end;

{ TcxInspectorViewInfo }

procedure TcxInspectorViewInfo.AddEmptyRects;
var
  ABandRect: TRect;
begin
  ABandRect := ViewRects.BandRects[0];
  if ABandRect.Right < ClientRect.Right then
    ViewRects.EmptyRects.Add(cxRect(ABandRect.Right, ABandRect.Top,
      ClientRect.Right, ClientRect.Bottom));
end;

procedure TcxInspectorViewInfo.CheckMaxRowHeaderWidth(var Value: Integer;
  AValueMinWidth: Integer);
begin
  if ViewBandWidth > 0 then
    Value := Min(Value, ViewBandWidth - Max(AValueMinWidth, 4));
end;

function TcxInspectorViewInfo.GetViewValueWidth: Integer;
begin
  Result := ViewBandWidth - ViewHeaderWidth - DividerWidth - 2 * VertLineWidth;
end;

function TcxInspectorViewInfo.GetVisibleValueCount: Integer;
begin
  Result := 1;
end;

{ TcxBandsViewInfo }

procedure TcxBandsViewInfo.AddEmptyRects;
var
  I, AHeight: Integer;
  ABand: TRect;
begin
  AHeight := ClientRect.Bottom - ClientRect.Top;
  with ABand, ViewRects do
  begin
    if BandsIndent > 0 then
      for I := 0 to BandRects.Count - 2 do
      begin
        ABand := BandRects[I];
        EmptyRects.Add(cxRectBounds(Right, Top, BandsIndent, AHeight));
      end;
    // check last band
    ABand := BandRects[BandRects.Count - 1];
    if Right < ClientRect.Right then
      //suppose non-autoWidth mode
      EmptyRects.Add(cxRectBounds(Right, Top, ClientRect.Right - Right, AHeight));
  end;
end;

procedure TcxBandsViewInfo.CalcBandRects;
var
  I: Integer;
begin
  with Scroller do
    for I := LeftVisibleBand to BandsInfo.Count - 1 do
      CreateBand(BandsInfo[I].BandHeight, 40);
  UpdateBandsWidth;
end;

procedure TcxBandsViewInfo.CalcBandWidth;
var
  ASaveWidth: Integer;
  APos: Double;
begin
  ViewHeaderWidth := GetViewHeaderWidth;
  if not AutoScaleBands then
  begin
    ViewValueWidth := GetViewValueWidth;
    ViewBandWidth := GetViewBandWidth;
  end
  else
  begin
    ASaveWidth := ViewBandWidth;
    ViewBandWidth := GetViewBandWidth;
    if ShowHeaders and (ASaveWidth <> ViewBandWidth) and (ASaveWidth <> 0) and not LockDividerPos then
    begin
      APos := ViewHeaderWidth / ASaveWidth;
      SetDividerPos(Round(ViewBandWidth * APos));
      ViewHeaderWidth := GetViewHeaderWidth;
    end;
  end;
end;

procedure TcxBandsViewInfo.CalculateBandsInfo;
begin
  Reset;
  ClearValuesInfo;
  CalcRowsHeight;
  if AutoScaleBands then
    Scroller.RecalcBandsInfo;
  CalcBandWidth;
  if not AutoScaleBands then
    Scroller.RecalcBandsInfo;
end;

function TcxBandsViewInfo.CanAddRowToBand(const ARowRect, ABandRect: TRect;
  ABandRowIndex: Integer): Boolean;
begin
  Result := (ARowRect.Bottom <= ABandRect.Bottom) or (ABandRowIndex = 0);
end;

procedure TcxBandsViewInfo.CheckMaxRowHeaderWidth(var Value: Integer;
  AValueMinWidth: Integer);
begin
  Value := Min(Value, ViewBandWidth - AValueMinWidth);
end;

procedure TcxBandsViewInfo.CreateBand(ABandHeight, ABandWidth: Integer);
var
  ABandIndex, ALeft: Integer;
  R: TRect;
begin
  ABandIndex := ViewRects.BandRects.Count;
  if ABandIndex = 0 then
    ALeft := ClientRect.Left
  else
    ALeft := ViewRects.BandRects[ABandIndex - 1].Right;
  R := cxRectBounds(ALeft, ClientRect.Top, ABandWidth,
    Min(ABandHeight, cxRectHeight(ClientRect)));
  ViewRects.BandRects.Add(R);
  if R.Bottom < ClientRect.Bottom then
    ViewRects.EmptyRects.Add(cxRectBounds(R.Left, R.Bottom, ABandIndex,
      cxRectHeight(ClientRect) - cxRectHeight(R)));
end;

function TcxBandsViewInfo.GetBandSizeableRect(const ABandRect: TRect): TRect;
begin
  if VerticalGrid.OptionsView.AutoScaleBands then
    Result := cxNullRect
  else
    Result := inherited GetBandSizeableRect(ABandRect);
end;

function TcxBandsViewInfo.GetMinViewBandWidth: Integer;
begin
  Result := GetViewMinHeaderWidth + VerticalGrid.OptionsView.ValueMinWidth
end;

function TcxBandsViewInfo.GetRowAutoHeight(ARow: TcxCustomRow): Boolean;
begin
  with VerticalGrid.OptionsView do
    Result := CellAutoHeight and not AutoScaleBands and
      ARow.Options.CanAutoHeight;
end;

function TcxBandsViewInfo.GetViewBandWidth: Integer;
var
  ABandCount: Integer;
begin
  if not AutoScaleBands then
    Result := ViewHeaderWidth + DividerWidth + ViewValueWidth + VertLineWidth * 2
  else
  begin
    Result := cxRectWidth(ClientRect);
    ABandCount := Scroller.BandsInfo.Count;
    if ABandCount > 0 then
      Result := (Result - GetBandIndentWidth(ABandCount)) div ABandCount;
    Result := Max(Result, GetMinViewBandWidth);
  end;
end;

function TcxBandsViewInfo.GetViewValueWidth: Integer;
begin
  Result := VerticalGrid.OptionsView.ValueWidth
end;

function TcxBandsViewInfo.GetVisibleValueCount: Integer;
begin
  Result := 1;
end;

procedure TcxBandsViewInfo.SetDividerPos(APos: Integer);
begin
  FLock := True;
  try
    if AutoScaleBands then
      inherited SetDividerPos(APos)
    else
    begin
      if APos > ClipRect.Right - 4 then
        APos := ClipRect.Right - 4;
      VerticalGrid.OptionsView.RowHeaderWidth := APos;
    end;
  finally
    FLock := False;
  end;
end;

function TcxBandsViewInfo.GetAutoScaleBands: Boolean;
begin
  Result := VerticalGrid.OptionsView.AutoScaleBands;
end;

function TcxBandsViewInfo.GetBandIndentWidth(ABandCount: Integer): Integer;
begin
  if ABandCount > 1 then
    Result := (ABandCount - 1) * BandsIndent
  else
    Result := 0;
end;

function TcxBandsViewInfo.GetBandsInterval: Integer;
begin
  Result := VerticalGrid.OptionsView.BandsInterval;
end;

procedure TcxBandsViewInfo.UpdateBandsWidth;
var
  I, ACurLeft: Integer;
  R, B: TRect;
begin
  ACurLeft := ClientRect.Left;
  with ViewRects do
  begin
    for I := 0 to BandRects.Count - 1 do
    begin
      R := BandRects[I];
      R.Left := ACurLeft;
      R.Right := R.Left + ViewBandWidth;
      Inc(ACurLeft, ViewBandWidth + BandsIndent);
      if AutoScaleBands then
      begin
        if I = BandRects.Count - 1 then
        begin
          Dec(ACurLeft, BandsIndent);
          Inc(R.Right, ClientRect.Right - ACurLeft);
        end;
      end;
      BandRects[I] := R;
    end;
    for I := 0 to EmptyRects.Count - 1 do
    begin
      R := EmptyRects[I];
      B := BandRects[R.Right - R.Left];
      R.Left := B.Left;
      R.Right := B.Right;
      EmptyRects[I] := R;
    end;
  end;
end;

{ TcxMultiRecordViewInfo }

constructor TcxMultiRecordViewInfo.Create(AOwner: TcxEditingControl);
begin
  inherited Create(AOwner);
  FVisibleValueCount := -1;
end;

function TcxMultiRecordViewInfo.GetDefaultGridModeBufferCount: Integer;
var
  AValuesWidth, AFullValueWidth: Integer;
begin
  AValuesWidth := GetPixelScrollContentSize - Scroller.PixelScrollRecordOffset;
  if AValuesWidth <= 0 then
  begin
    Result := 1;
    Exit;
  end;
  Inc(AValuesWidth, RecordsInterval);
  AFullValueWidth := ViewValueWidth + VertLineWidth + RecordsInterval;
  Result := RoundDiv(AValuesWidth, AFullValueWidth);
end;

function TcxMultiRecordViewInfo.GetLTRValueRect(AValueIndex: Integer;
  ARowViewInfo: TcxCustomRowViewInfo): TRect;
var
  ALeft: Integer;
begin
  with ARowViewInfo.RowRect do
  begin
    ALeft := Left + ViewHeaderWidth + DividerWidth +
      AValueIndex * (ViewValueWidth + cxSetValue(RecordsInterval = 0, VertLineWidth,
      RecordsInterval + 2 * VertLineWidth));
    Result := cxRectBounds(ALeft + Scroller.PixelScrollRecordOffset, Top, ViewValueWidth, Bottom - Top);
  end;
end;

procedure TcxMultiRecordViewInfo.AddBandRowsLines(const R: TRect);
var
  ASize: TSize;
  ABandRect: TRect;
  I, ALeft, ATop, AHeight: Integer;
  ARowInfo: TcxCustomRowViewInfo;

  procedure AddRecordInterval(ALast: Boolean);
  var
    I: Integer;
    R: TRect;
  begin
    if AHeight > 0 then
      for I := 0 to VisibleValueCount - 2 do
      begin
        if (HorzLineWidth > 0) and not ALast then
          LinesInfo.Add(ALeft, ATop + AHeight, RecordsInterval, HorzLineWidth,
            BandBorderColor);
        R := cxRectBounds(ALeft, ATop, RecordsInterval, AHeight);
        if ALast then Inc(R.Bottom, HorzLineWidth);
        ViewRects.EmptyRects.Add(R);
        if VertLineWidth > 0 then
          LinesInfo.Add(R.Right, R.Top, VertLineWidth, AHeight + HorzLineWidth,
            BandBorderColor);
        Inc(ALeft, ViewValueWidth + RecordsInterval + 2 * VertLineWidth);
      end;
  end;

begin
  if RecordsInterval = 0 then
  begin
    inherited AddBandRowsLines(R);
    Exit;
  end;
  ABandRect := R;
  if RecordsInterval > 0 then
  begin
    ALeft := ABandRect.Left + FCommonWidth + Scroller.PixelScrollRecordOffset;
    ATop := ClientRect.Top;
    AHeight := 0;
    for I := 0 to RowsViewInfo.Count - 1 do
    begin
      ARowInfo := RowsViewInfo[I];
      if not (ARowInfo is TcxCategoryRowViewInfo) then
        Inc(AHeight, cxRectHeight(ARowInfo.RowRect) + HorzLineWidth)
      else
      begin
        AddRecordInterval(False);
        AHeight := 0;
        ATop := ARowInfo.RowRect.Bottom;
        ALeft := ABandRect.Left + 2 * VertLineWidth + DividerWidth +
          ViewHeaderWidth + ViewValueWidth + Scroller.PixelScrollRecordOffset;
      end;
    end;
    AddRecordInterval(True);
  end;
  if (VertLineWidth > 0) then
    with R do
      LinesInfo.Add(Left, Top - HorzLineWidth, VertLineWidth, Bottom - Top,
        BandBorderColor);
  ALeft := R.Left + Scroller.PixelScrollRecordOffset;
  ATop  := R.Top;
  ASize := cxSize(FCommonWidth, R.Bottom - R.Top);
  while ALeft < R.Right do
  begin
    if ALeft + ASize.cx > R.Right then ASize.cx := R.Right - ALeft;
    with cxRectBounds(ALeft, ATop, ASize.cx, ASize.cy) do
    begin
      if HorzLineWidth > 0 then
      begin
        LinesInfo.Add(Left, Top, Right - Left, HorzLineWidth, BandBorderColor);
        LinesInfo.Add(Left, Bottom - HorzLineWidth, Right - Left, HorzLineWidth, BandBorderColor);
      end;
    end;
    Inc(ALeft, RecordsInterval + ASize.cx);
    ASize.cx := 2 * VertLineWidth + ViewValueWidth;
  end;
end;

procedure TcxMultiRecordViewInfo.AddEmptyRects;
var
  ABandRect: TRect;
begin
  ABandRect := ViewRects.BandRects[0];
  if ABandRect.Right < ClientRect.Right then
    ViewRects.EmptyRects.Add(cxRect(ABandRect.Right, ABandRect.Top,
      ClientRect.Right, ClientRect.Bottom));
end;

procedure TcxMultiRecordViewInfo.AddRightValueSide(ARowViewInfo: TcxCustomRowViewInfo;
  const R: TRect; ALast: Boolean);
begin
  inherited AddRightValueSide(ARowViewInfo, R, ALast or (FRecordsInterval > 0));
end;

procedure TcxMultiRecordViewInfo.CalcBandWidth;
var
  AWidth: Integer;
  AMaxBandWidth: Integer;
begin
  ViewHeaderWidth := GetViewHeaderWidth;
  ViewValueWidth := VerticalGrid.OptionsView.ValueWidth;
  AWidth := ViewHeaderWidth + 2 * VertLineWidth + DividerWidth + ViewValueWidth;
  FCommonWidth := AWidth;
  if RecordCount > 1 then
  begin
    Inc(AWidth, (RecordCount - Scroller.LeftVisibleRecord - 1) *
      (ViewValueWidth + cxSetValue(RecordsInterval = 0, VertLineWidth,
       RecordsInterval + 2 * VertLineWidth)));
    AWidth := AWidth + Scroller.PixelScrollRecordOffset;
  end;
  AMaxBandWidth := cxRectWidth(ClientRect);
  ViewBandWidth := Min(AWidth,  AMaxBandWidth);
end;

procedure TcxMultiRecordViewInfo.CalcLayoutGeneral;
begin
  FRecordsInterval := VerticalGrid.OptionsView.RecordsInterval;
  inherited CalcLayoutGeneral;
end;

function TcxMultiRecordViewInfo.CanUpdateRecord(ARecordIndex: Integer): Boolean;
begin
  Result := ARecordIndex >= 0;
end;

procedure TcxMultiRecordViewInfo.Clear;
begin
  inherited Clear;
  FVisibleValueCount := -1;
  FCommonWidth := 0
end;

function TcxMultiRecordViewInfo.GetBandSizeableRect(
  const ABandRect: TRect): TRect;
var
  ALeft: Integer;
begin
  if VerticalGrid.LayoutStyle <> lsMultiRecordView then
    Result := inherited GetBandSizeableRect(ABandRect)
  else
    with ClientRect do
    begin
      if not IsRightToLeftConverted then
      begin
        ALeft := ABandRect.Left + ViewHeaderWidth + DividerWidth + ViewValueWidth + VertLineWidth;
        Result := cxRect(ALeft - 1, Top, ALeft + 2, Bottom);
      end
      else
      begin
        ALeft := ABandRect.Right - ViewHeaderWidth - DividerWidth - ViewValueWidth - VertLineWidth;
        Result := cxRect(ALeft - 2, Top, ALeft + 1, Bottom);
      end
    end;
end;

function TcxMultiRecordViewInfo.GetPixelScrollContentSize: Integer;
begin
  Result := cxRectWidth(ClientRect) - ViewHeaderWidth - DividerWidth - VertLineWidth;
end;

function TcxMultiRecordViewInfo.GetVisibleValueCount: Integer;
begin
  if FVisibleValueCount = -1 then
    FVisibleValueCount := CalcVisibleValueCount;
  Result := FVisibleValueCount;
end;

procedure TcxMultiRecordViewInfo.Reset;
begin
  inherited Reset;
  FVisibleValueCount := -1;
end;

procedure TcxMultiRecordViewInfo.SetDividerPos(APos: Integer);
begin
  if APos > ClipRect.Right - 4 then APos := ClipRect.Right - 4;
  VerticalGrid.OptionsView.RowHeaderWidth := APos;
end;

function TcxMultiRecordViewInfo.CalcVisibleValueCount: Integer;
begin
  Result := 0;
  if cxRectIsEmpty(ClientRect) then Exit;
  CalcBandWidth;
  Result := Max(1, Min(GetDefaultGridModeBufferCount,
    RecordCount - Scroller.LeftVisibleRecord));
end;

function TcxMultiRecordViewInfo.GetRecordCount: Integer;
begin
  Result := VerticalGrid.RecordCount;
end;

function TcxMultiRecordViewInfo.GetVerticalGrid: TcxVirtualVerticalGrid;
begin
  Result := TcxVirtualVerticalGrid(inherited VerticalGrid);
end;

end.
