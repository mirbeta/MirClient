{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonGroupLayoutCalculator;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Contnrs, dxBar, dxRibbon, cxGeometry;

type
  { TdxRibbonGroupLayoutCalculator }

  // RLC - RibbonLayoutCalculator
  TRLCItemControlInfo = class
  public
    ViewInfo: IdxBarItemControlViewInfo;
    OneRowItemsBefore: Integer;
    OneRowItemsAfter: Integer;
  end;

  TSequenceForWholeGroupHeightReducing = class(TObjectList)
  private
    function GetItem(Index: Integer): TRLCItemControlInfo;
  public
    property Items[Index: Integer]: TRLCItemControlInfo read GetItem; default;
  end;

  TdxRibbonGroupLayoutCalculator = class(TInterfacedObject, IdxRibbonGroupLayoutCalculator)
  strict private
    FColumnCount: Integer;
    FGroupRowCount: Integer;
    FGroupRowHeight: Integer;
    FItemControlColumns: array of Integer;
    FItemControlIndexForOneRowHeightReducing: Integer;
    FItemControlSeparators: array of Integer;
    FReducingSequences: TObjectList;
    FScaleFactor: TdxScaleFactor;
    X: Integer;

    procedure AlignOneRowHeightItemControl(AItemControlViewInfo: IdxBarItemControlViewInfo; AColumnWidth: Integer);
    procedure AlignOneRowHeightItemControls(AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, AColumnWidth: Integer);
    procedure CheckDistanceBetweenTwoLastColumns(AGroupViewInfo: IdxRibbonGroupViewInfo; ALastItemControlIndex: Integer);
    procedure DistributeOneRowHeightItemControlsOnGroupHeight(AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, ALastIndex: Integer);
    function GetGroupHeight: Integer;
    procedure IncrementColumnCount(AGroupViewInfo: IdxRibbonGroupViewInfo; ALastItemControlIndex: Integer);
    procedure OffsetContent(AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstItemControlIndex, ALastItemControlIndex, AOffset: Integer);
    procedure PlaceOneRowHeightItemControls(AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, ALastIndex: Integer; ADistributeOnGroupHeight: Boolean);
    procedure PlaceSeparator(AGroupViewInfo: IdxRibbonGroupViewInfo; AItemControlIndex: Integer);
    procedure PlaceWholeGroupHeightItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo; AIndex: Integer);
    function ReduceOneRowHeightItemControlsWithText(AGroupViewInfo: IdxRibbonGroupViewInfo; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
    function TryReduceToSmallIconWithText(ASequence: TSequenceForWholeGroupHeightReducing): Boolean;
    function ReduceToSmallIconWithText(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
    function ReduceToLargeControlOnly(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
  protected
    property GroupRowCount: Integer read FGroupRowCount;
    property GroupRowHeight: Integer read FGroupRowHeight;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
  public
    constructor Create(AGroupRowHeight, AGroupRowCount: Integer; AScaleFactor: TdxScaleFactor);
    destructor Destroy; override;
    // IdxRibbonGroupLayoutCalculator
    procedure CalcInit(AGroupViewInfo: IdxRibbonGroupViewInfo);
    procedure CalcLayout(AGroupViewInfo: IdxRibbonGroupViewInfo);
    function CollapseMultiColumnItemControls(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
    function DecreaseMultiColumnItemControlsColumnCount(AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
    function Reduce(AGroupViewInfo: IdxRibbonGroupViewInfo; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
    procedure ReduceInit(AGroupViewInfo: IdxRibbonGroupViewInfo);
  end;

procedure CalcItemControlsRealPositionInButtonGroup(AItemControlViewInfos: TList);

implementation

uses
  SysUtils, cxClasses, cxGraphics, Math;

const
  dxRibbonItemSeparatorTopOffset = 5;
  dxRibbonItemSeparatorBottomOffset = 6;
  dxRibbonItemSeparatorAreaWidth = 8;

function GetItemControlCurrentWidth(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Integer; forward;
function IsNewColumnItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload; forward;
function IsNewColumnItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload; forward;
function IsPartOfHorizontalBox(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer; ACanBeginHorizontalBox: Boolean): Boolean; forward;
function IsWholeGroupHeightItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload; forward;
function IsWholeGroupHeightItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload; forward;
function IsWholeGroupHeightOnlyItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload; forward;
function IsWholeGroupHeightOnlyItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload; forward;

{ TSequences }

procedure CalcItemControlsRealPositionInButtonGroup(AItemControlViewInfos: TList);

  function GetItemControlViewInfo(AItemControlIndex: Integer): IdxBarItemControlViewInfo;
  begin
    Result := IdxBarItemControlViewInfo(AItemControlViewInfos[AItemControlIndex]);
  end;

  function GetItemControlPositionInButtonGroup(
    AItemControlViewInfo: IdxBarItemControlViewInfo): TdxBarButtonGroupPosition;
  begin
    Result := AItemControlViewInfo.GetPositionInButtonGroup;
    if IsWholeGroupHeightOnlyItemControl(AItemControlViewInfo) then
      Result := bgpNone;
  end;

  function NextItemControlIsMemberOfButtonGroup(AItemControlIndex: Integer): Boolean;
  var
    AItemControlViewInfo: IdxBarItemControlViewInfo;
  begin
    Result := AItemControlIndex + 1 < AItemControlViewInfos.Count;
    if Result then
    begin
      AItemControlViewInfo := GetItemControlViewInfo(AItemControlIndex + 1);
      Result := (GetItemControlPositionInButtonGroup(AItemControlViewInfo) = bgpMember) and
        not AItemControlViewInfo.HasSeparator;
    end;
  end;

  function GetItemControlRealPositionInButtonGroup(
    AItemControlIndex: Integer): TdxBarButtonGroupRealPosition;
  var
    AItemControlViewInfo: IdxBarItemControlViewInfo;
  begin
    case GetItemControlPositionInButtonGroup(GetItemControlViewInfo(AItemControlIndex)) of
      bgpStart:
        if NextItemControlIsMemberOfButtonGroup(AItemControlIndex) then
          Result := bgrpStart
        else
          Result := bgrpSingle;
      bgpMember:
        begin
          Result := bgrpNone;
          if not (GetItemControlViewInfo(AItemControlIndex).HasSeparator) and
            (AItemControlIndex > 0) then
          begin
            AItemControlViewInfo := GetItemControlViewInfo(AItemControlIndex - 1);
            if AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpStart, bgrpMember] then
            begin
              if NextItemControlIsMemberOfButtonGroup(AItemControlIndex) then
                Result := bgrpMember
              else
                Result := bgrpFinish;
            end;
          end;
        end;
    else
      Result := bgrpNone;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to AItemControlViewInfos.Count - 1 do
    GetItemControlViewInfo(I).SetRealPositionInButtonGroup(
      GetItemControlRealPositionInButtonGroup(I));
end;

function GetItemControlCurrentWidth(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Integer;
begin
  Result := AItemControlViewInfo.GetWidth(AItemControlViewInfo.GetViewLevel);
end;

function IsNewColumnItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload;
begin
  Result := AItemControlViewInfo.HasSeparator or
    (AItemControlViewInfo.GetPosition = ipBeginsNewColumn) and
    (AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpNone, bgrpStart, bgrpSingle]);
end;

function IsNewColumnItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload;
begin
  Result := IsNewColumnItemControl(
    AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex));
end;

function IsPartOfHorizontalBox(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer; ACanBeginHorizontalBox: Boolean): Boolean;
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  ARealPositionInButtonGroup: TdxBarButtonGroupRealPosition;
begin
  ARealPositionInButtonGroup := AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex).GetRealPositionInButtonGroup;
  if (ARealPositionInButtonGroup in [bgrpMember, bgrpFinish]) or
    ACanBeginHorizontalBox and (ARealPositionInButtonGroup = bgrpStart) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
  if ACanBeginHorizontalBox and (AItemControlIndex + 1 < AGroupViewInfo.GetItemControlCount) then
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex + 1);
    Result := not IsNewColumnItemControl(AItemControlViewInfo) and
      (AItemControlViewInfo.GetPosition = ipContinuesRow);
  end;
  if not Result then
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex);
    Result := (AItemControlViewInfo.GetPosition = ipContinuesRow) and
      (AItemControlIndex > 0) and not IsNewColumnItemControl(AItemControlViewInfo);
  end;
end;

function IsWholeGroupHeightItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload;
var
  AMultiColumnItemControlViewInfo: IdxBarMultiColumnItemControlViewInfo;
begin
  Result := AItemControlViewInfo.IsMultiColumnItemControl(True, AMultiColumnItemControlViewInfo) or
    (AItemControlViewInfo.GetViewLevel in [ivlLargeIconWithText, ivlLargeControlOnly]);
end;

function IsWholeGroupHeightItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload;
begin
  Result := IsWholeGroupHeightItemControl(
    AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex));
end;

function IsWholeGroupHeightOnlyItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo): Boolean; overload;
var
  AMultiColumnItemControlViewInfo: IdxBarMultiColumnItemControlViewInfo;
begin
  Result := AItemControlViewInfo.IsMultiColumnItemControl(True, AMultiColumnItemControlViewInfo) or
    (AItemControlViewInfo.GetAllowedViewLevels - [ivlLargeIconWithText, ivlLargeControlOnly] = []);
end;

function IsWholeGroupHeightOnlyItemControl(AGroupViewInfo: IdxRibbonGroupViewInfo;
  AItemControlIndex: Integer): Boolean; overload;
begin
  Result := IsWholeGroupHeightOnlyItemControl(AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex));
end;

{ TSequenceForWholeGroupHeightReducing }

function TSequenceForWholeGroupHeightReducing.GetItem(Index: Integer): TRLCItemControlInfo;
begin
  Result := TRLCItemControlInfo(inherited Items[Index]);
end;

{ TdxRibbonGroupLayoutCalculator }

constructor TdxRibbonGroupLayoutCalculator.Create(
  AGroupRowHeight, AGroupRowCount: Integer; AScaleFactor: TdxScaleFactor);
begin
  inherited Create;
  FScaleFactor := AScaleFactor;
  FGroupRowCount := AGroupRowCount;
  FGroupRowHeight := AGroupRowHeight;
  FReducingSequences := TObjectList.Create;
end;

destructor TdxRibbonGroupLayoutCalculator.Destroy;
begin
  FreeAndNil(FReducingSequences);
  inherited Destroy;
end;

// IdxRibbonGroupLayoutCalculator
procedure TdxRibbonGroupLayoutCalculator.CalcInit(AGroupViewInfo: IdxRibbonGroupViewInfo);
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AItemControlViewInfos: TList;
  AMultiColumnItemControlViewInfo: IdxBarMultiColumnItemControlViewInfo;
  I: Integer;
begin
  AItemControlViewInfos := TList.Create;
  try
    for I := 0 to AGroupViewInfo.GetItemControlCount - 1 do
      AItemControlViewInfos.Add(Pointer(AGroupViewInfo.GetItemControlViewInfo(I)));
    CalcItemControlsRealPositionInButtonGroup(AItemControlViewInfos);
  finally
    AItemControlViewInfos.Free;
  end;

  for I := 0 to AGroupViewInfo.GetItemControlCount - 1 do
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    AItemControlViewInfo.SetBounds(cxEmptyRect);
    if AItemControlViewInfo.GetRealPositionInButtonGroup = bgrpNone then
      AItemControlViewInfo.SetViewLevel(GetMaxViewLevel(AItemControlViewInfo.GetAllowedViewLevels))
    else
      AItemControlViewInfo.SetViewLevel(AItemControlViewInfo.GetViewLevelForButtonGroup);
    if AItemControlViewInfo.IsMultiColumnItemControl(False, AMultiColumnItemControlViewInfo) then
    begin
      AMultiColumnItemControlViewInfo.SetCollapsed(False);
      AMultiColumnItemControlViewInfo.SetColumnCount(AMultiColumnItemControlViewInfo.GetMaxColumnCount);
    end;
  end;
end;

procedure TdxRibbonGroupLayoutCalculator.CalcLayout(AGroupViewInfo: IdxRibbonGroupViewInfo);

  procedure CheckGroupMinContentWidth;
  var
    AMinContentWidth: Integer;
  begin
    AMinContentWidth := AGroupViewInfo.GetMinContentWidth;
    if AMinContentWidth < ScaleFactor.Apply(dxRibbonGroupMinContentWidth) then
      AMinContentWidth := ScaleFactor.Apply(dxRibbonGroupMinContentWidth);
    if X < AMinContentWidth then
    begin
      if X <> 0 then
        OffsetContent(AGroupViewInfo, 0, AGroupViewInfo.GetItemControlCount - 1, (AMinContentWidth - X) div 2);
      X := AMinContentWidth;
    end;
  end;

  procedure CalcLayoutFinalize;
  var
    I: Integer;
  begin
    for I := 0 to AGroupViewInfo.GetItemControlCount - 1 do
      AGroupViewInfo.GetItemControlViewInfo(I).CalculateFinalize;
  end;

var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  I, J: Integer;
begin
  AGroupViewInfo.DeleteSeparators;
  FColumnCount := -1;
  X := 0;
  SetLength(FItemControlColumns, AGroupViewInfo.GetItemControlCount);
  SetLength(FItemControlSeparators, AGroupViewInfo.GetItemControlCount);
  for I := 0 to High(FItemControlSeparators) do
    FItemControlSeparators[I] := -1;
  I := 0;
  while I < AGroupViewInfo.GetItemControlCount do
  begin
    J := I;
    while J < AGroupViewInfo.GetItemControlCount do
    begin
      AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(J);
      if IsWholeGroupHeightItemControl(AItemControlViewInfo) or
        (J > I) and IsNewColumnItemControl(AItemControlViewInfo) then
          Break;
      Inc(J);
    end;
    if I < J then
    begin
      IncrementColumnCount(AGroupViewInfo, I - 1);
      PlaceOneRowHeightItemControls(AGroupViewInfo, I, J - 1, True);
    end;
    I := J;
    while (I < AGroupViewInfo.GetItemControlCount) and
      IsWholeGroupHeightItemControl(AGroupViewInfo.GetItemControlViewInfo(I)) do
    begin
      IncrementColumnCount(AGroupViewInfo, I - 1);
      PlaceWholeGroupHeightItemControl(AGroupViewInfo, I);
      Inc(I);
    end;
  end;
  CheckDistanceBetweenTwoLastColumns(AGroupViewInfo, AGroupViewInfo.GetItemControlCount - 1);
  with AGroupViewInfo.GetOffsetsInfo do
  begin
    OffsetContent(AGroupViewInfo, 0, AGroupViewInfo.GetItemControlCount - 1, ContentLeftOffset);
    Inc(X, ContentLeftOffset + ContentRightOffset);
  end;
  CheckGroupMinContentWidth;
  AGroupViewInfo.SetContentSize(Size(X, GetGroupHeight));
  CalcLayoutFinalize;
end;

function TdxRibbonGroupLayoutCalculator.CollapseMultiColumnItemControls(
  AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AMultiColumnItemControlViewInfo: IdxBarMultiColumnItemControlViewInfo;
  I: Integer;
begin
  Result := False;
  if AGroupViewInfo.GetItemControlCount = 0 then
    Exit;
  for I := AGroupViewInfo.GetItemControlCount - 1 downto 0 do
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    if AItemControlViewInfo.IsMultiColumnItemControl(True, AMultiColumnItemControlViewInfo) and
      AMultiColumnItemControlViewInfo.CanCollapse then
    begin
      Result := True;
      AMultiColumnItemControlViewInfo.SetCollapsed(True);
      Break;
    end;
  end;
  if Result then
    CalcLayout(AGroupViewInfo);
end;

function TdxRibbonGroupLayoutCalculator.DecreaseMultiColumnItemControlsColumnCount(
  AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AMultiColumnItemControlViewInfo: IdxBarMultiColumnItemControlViewInfo;
  I: Integer;
begin
  Result := False;
  if AGroupViewInfo.GetItemControlCount = 0 then
    Exit;
  for I := AGroupViewInfo.GetItemControlCount - 1 downto 0 do
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    if AItemControlViewInfo.IsMultiColumnItemControl(True, AMultiColumnItemControlViewInfo) and
      (AMultiColumnItemControlViewInfo.GetColumnCount > AMultiColumnItemControlViewInfo.GetMinColumnCount) then
    begin
      Result := True;
      AMultiColumnItemControlViewInfo.SetColumnCount(AMultiColumnItemControlViewInfo.GetColumnCount - 1);
      Break;
    end;
  end;
  if Result then
    CalcLayout(AGroupViewInfo);
end;

function TdxRibbonGroupLayoutCalculator.Reduce(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;
begin
  if AGroupViewInfo.GetItemControlCount = 0 then
    Result := False
  else
  begin
    if AUpToViewLevel = ivlLargeControlOnly then
      Result := ReduceToLargeControlOnly(AGroupViewInfo)
    else
      if AUpToViewLevel = ivlSmallIconWithText then
        Result := ReduceToSmallIconWithText(AGroupViewInfo)
      else
      begin
        Result := ReduceOneRowHeightItemControlsWithText(AGroupViewInfo, AUpToViewLevel);
        if not Result then
          FItemControlIndexForOneRowHeightReducing := AGroupViewInfo.GetItemControlCount - 1;
      end;
  end;
end;

procedure TdxRibbonGroupLayoutCalculator.ReduceInit(
  AGroupViewInfo: IdxRibbonGroupViewInfo);

  procedure AddElementToSequence(ASequence: TSequenceForWholeGroupHeightReducing; AIndex, AOneRowItemCount: Integer);
  var
    AInfo: TRLCItemControlInfo;
  begin
    AInfo := TRLCItemControlInfo.Create;
    ASequence.Add(AInfo);
    AInfo.ViewInfo := AGroupViewInfo.GetItemControlViewInfo(AIndex);
    AInfo.OneRowItemsBefore := AOneRowItemCount;
    AInfo.OneRowItemsAfter := 0;
  end;

  function IsCollapseQueueBroken(AIndex: Integer): Boolean;
  begin
    Result :=
      IsWholeGroupHeightOnlyItemControl(AGroupViewInfo, AIndex) or
      IsNewColumnItemControl(AGroupViewInfo, AIndex) or
      (IsPartOfHorizontalBox(AGroupViewInfo, AIndex, True) or IsPartOfHorizontalBox(AGroupViewInfo, AIndex, False)) and IsWholeGroupHeightItemControl(AGroupViewInfo, AIndex);
  end;

  function AddSequence: TSequenceForWholeGroupHeightReducing;
  begin
    Result := TSequenceForWholeGroupHeightReducing.Create;
    FReducingSequences.Add(Result)
  end;

var
  I, AOneRowItemCount: Integer;
  ASequence: TSequenceForWholeGroupHeightReducing;
begin
  ASequence := AddSequence;
  AOneRowItemCount := 0;
  for I := 0 to AGroupViewInfo.GetItemControlCount - 1 do
  begin
    if IsCollapseQueueBroken(I) then
    begin
      if ASequence.Count > 0 then
        ASequence[ASequence.Count - 1].OneRowItemsAfter := AOneRowItemCount;
      ASequence := AddSequence;
      AOneRowItemCount := 0;
      if IsWholeGroupHeightOnlyItemControl(AGroupViewInfo, I) then
        Continue;
    end;
    if IsWholeGroupHeightItemControl(AGroupViewInfo, I) then
    begin
      if ASequence.Count > 0 then
        ASequence[ASequence.Count - 1].OneRowItemsAfter := AOneRowItemCount;
      AddElementToSequence(ASequence, I, AOneRowItemCount);
      AOneRowItemCount := 0;
      Continue;
    end;
    Inc(AOneRowItemCount);
  end;
  if ASequence.Count > 0 then
    ASequence[ASequence.Count - 1].OneRowItemsAfter := AOneRowItemCount;
  FItemControlIndexForOneRowHeightReducing := AGroupViewInfo.GetItemControlCount - 1;
end;

procedure TdxRibbonGroupLayoutCalculator.AlignOneRowHeightItemControl(
  AItemControlViewInfo: IdxBarItemControlViewInfo; AColumnWidth: Integer);
var
  R: TRect;
begin
  R := AItemControlViewInfo.GetBounds;
  case AItemControlViewInfo.GetAlign of
    iaCenter:
      OffsetRect(R, (AColumnWidth - cxRectWidth(R)) div 2, 0);
    iaRight:
      OffsetRect(R, AColumnWidth - cxRectWidth(R), 0);
    iaClient:
      R.Right := R.Left + AColumnWidth;
  end;
  AItemControlViewInfo.SetBounds(R);
end;

procedure TdxRibbonGroupLayoutCalculator.AlignOneRowHeightItemControls(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, AColumnWidth: Integer);

  function SkipRowWithSeveralItemControls: Boolean;
  var
    AColumn, ARow: Integer;
  begin
    Result := False;
    AColumn := FItemControlColumns[AFirstIndex];
    ARow := AGroupViewInfo.GetItemControlViewInfo(AFirstIndex).GetRow;
    while (AFirstIndex + 1 < AGroupViewInfo.GetItemControlCount) and
      (FItemControlColumns[AFirstIndex + 1] = AColumn) and
      (AGroupViewInfo.GetItemControlViewInfo(AFirstIndex + 1).GetRow = ARow) do
    begin
      Result := True;
      Inc(AFirstIndex);
    end;
    if Result then
      Inc(AFirstIndex);
  end;

var
  AColumn: Integer;
begin
  AColumn := FItemControlColumns[AFirstIndex];
  while (AFirstIndex < AGroupViewInfo.GetItemControlCount) and
    (FItemControlColumns[AFirstIndex] = AColumn) do
  begin
    if SkipRowWithSeveralItemControls then // TODO by first ItemControl
      Continue;
    AlignOneRowHeightItemControl(AGroupViewInfo.GetItemControlViewInfo(AFirstIndex), AColumnWidth);
    Inc(AFirstIndex);
  end;
end;

procedure TdxRibbonGroupLayoutCalculator.CheckDistanceBetweenTwoLastColumns(
  AGroupViewInfo: IdxRibbonGroupViewInfo; ALastItemControlIndex: Integer);

  function GetColumnFirstItemControlIndex(AColumnLastItemControlIndex: Integer): Integer;
  var
    AColumn: Integer;
  begin
    AColumn := FItemControlColumns[AColumnLastItemControlIndex];
    Result := AColumnLastItemControlIndex;
    while (Result > 0) and (FItemControlColumns[Result - 1] = AColumn) do
      Dec(Result);
  end;

  procedure GetMaxBoundInfoForItemControls(
    AFirstItemControlIndex, ALastItemControlIndex: Integer;
    out AMaxRightBound, AMaxButtonGroupRightBound: Integer);
  var
    AItemControlViewInfo: IdxBarItemControlViewInfo;
    I: Integer;
    R: TRect;
  begin
    AMaxRightBound := -1;
    AMaxButtonGroupRightBound := -1;
    for I := AFirstItemControlIndex to ALastItemControlIndex do
    begin
      AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
      R := AItemControlViewInfo.GetBounds;
      AMaxRightBound := Max(AMaxRightBound, R.Right);
      if AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpFinish, bgrpSingle] then
        AMaxButtonGroupRightBound := Max(AMaxButtonGroupRightBound, R.Right);
    end;
  end;

  procedure GetMinBoundInfoForItemControls(
    AFirstItemControlIndex, ALastItemControlIndex: Integer;
    out AMinLeftBound, AMinButtonGroupLeftBound: Integer);
  var
    AItemControlViewInfo: IdxBarItemControlViewInfo;
    I: Integer;
  begin
    AMinLeftBound := AGroupViewInfo.GetItemControlViewInfo(AFirstItemControlIndex).GetBounds.Left;
    AMinButtonGroupLeftBound := cxMaxRectSize;
    for I := AFirstItemControlIndex to ALastItemControlIndex do
    begin
      AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
      if AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpStart, bgrpSingle] then
        AMinButtonGroupLeftBound := Min(AMinButtonGroupLeftBound,
          AItemControlViewInfo.GetBounds.Left);
    end;
  end;

var
  AFirstColumnFirstItemControlIndex, AFirstColumnLastItemControlIndex: Integer;
  ASecondColumnFirstItemControlIndex, ASecondColumnLastItemControlIndex: Integer;
  AFirstColumnMaxItemControlRightBound, AFirstColumnMaxButtonGroupRightBound: Integer;
  ASecondColumnMinItemControlLeftBound, ASecondColumnMinButtonGroupLeftBound: Integer;
  AButtonGroupOffset, AOffset, AOffset1, AOffset2: Integer;
begin
  if FColumnCount < 1 then
    Exit;
  ASecondColumnLastItemControlIndex := ALastItemControlIndex;
  ASecondColumnFirstItemControlIndex := GetColumnFirstItemControlIndex(
    ASecondColumnLastItemControlIndex);
  if AGroupViewInfo.GetItemControlViewInfo(ASecondColumnFirstItemControlIndex).HasSeparator then
    Exit;
  AFirstColumnLastItemControlIndex := ASecondColumnFirstItemControlIndex - 1;
  AFirstColumnFirstItemControlIndex := GetColumnFirstItemControlIndex(
    AFirstColumnLastItemControlIndex);
  GetMaxBoundInfoForItemControls(AFirstColumnFirstItemControlIndex, AFirstColumnLastItemControlIndex,
    AFirstColumnMaxItemControlRightBound, AFirstColumnMaxButtonGroupRightBound);
  GetMinBoundInfoForItemControls(ASecondColumnFirstItemControlIndex, ASecondColumnLastItemControlIndex,
    ASecondColumnMinItemControlLeftBound, ASecondColumnMinButtonGroupLeftBound);

  AButtonGroupOffset := AGroupViewInfo.GetOffsetsInfo.ButtonGroupOffset;
  AOffset1 := 0;
  if (AFirstColumnMaxButtonGroupRightBound <> -1) and
    (ASecondColumnMinItemControlLeftBound - AFirstColumnMaxButtonGroupRightBound < AButtonGroupOffset) then
      AOffset1 := AButtonGroupOffset - (ASecondColumnMinItemControlLeftBound - AFirstColumnMaxButtonGroupRightBound);
  AOffset2 := 0;
  if (ASecondColumnMinButtonGroupLeftBound <> cxMaxRectSize) and
    (ASecondColumnMinButtonGroupLeftBound - AFirstColumnMaxItemControlRightBound < AButtonGroupOffset) then
      AOffset2 := AButtonGroupOffset - (ASecondColumnMinButtonGroupLeftBound - AFirstColumnMaxItemControlRightBound);
  AOffset := Max(AOffset1, AOffset2);
  if AOffset <> 0 then
  begin
    OffsetContent(AGroupViewInfo, ASecondColumnFirstItemControlIndex, ASecondColumnLastItemControlIndex, AOffset);
    Inc(X, AOffset);
  end;
end;

procedure TdxRibbonGroupLayoutCalculator.DistributeOneRowHeightItemControlsOnGroupHeight(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, ALastIndex: Integer);
var
  AColumn, AIndex, ARowCount, I: Integer;
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  R: TRect;
begin
  AColumn := FItemControlColumns[AFirstIndex];
  AIndex := AFirstIndex + 1;
  while (AIndex <= ALastIndex) and (FItemControlColumns[AIndex] = AColumn) do
    Inc(AIndex);
  Dec(AIndex);
  ARowCount := AGroupViewInfo.GetItemControlViewInfo(AIndex).GetRow + 1;
  if ARowCount < GroupRowCount then
    for I := AFirstIndex to AIndex do
    begin
      AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
      R := AItemControlViewInfo.GetBounds;
      R.Top := GetGroupHeight - ((GetGroupHeight - GroupRowHeight * ARowCount) div (ARowCount + 1) + GroupRowHeight) * (ARowCount - AGroupViewInfo.GetItemControlViewInfo(I).GetRow);
      R.Bottom := R.Top + GroupRowHeight;
      AItemControlViewInfo.SetBounds(R);
    end;
  for I := AFirstIndex to AIndex do
    AGroupViewInfo.GetItemControlViewInfo(I).SetColumnRowCount(ARowCount);
  Inc(AIndex);
  if AIndex <= ALastIndex then
    DistributeOneRowHeightItemControlsOnGroupHeight(AGroupViewInfo, AIndex, ALastIndex);
end;

function TdxRibbonGroupLayoutCalculator.GetGroupHeight: Integer;
begin
  Result := GroupRowHeight * GroupRowCount;
end;

procedure TdxRibbonGroupLayoutCalculator.IncrementColumnCount(
  AGroupViewInfo: IdxRibbonGroupViewInfo; ALastItemControlIndex: Integer);
begin
  CheckDistanceBetweenTwoLastColumns(AGroupViewInfo, ALastItemControlIndex);
  Inc(FColumnCount);
end;

procedure TdxRibbonGroupLayoutCalculator.OffsetContent(
  AGroupViewInfo: IdxRibbonGroupViewInfo;
  AFirstItemControlIndex, ALastItemControlIndex, AOffset: Integer);
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  ASeparatorInfo: TdxBarItemSeparatorInfo;
  I: Integer;
  R: TRect;
begin
  for I := AFirstItemControlIndex to ALastItemControlIndex do
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    R := AItemControlViewInfo.GetBounds;
    OffsetRect(R, AOffset, 0);
    AItemControlViewInfo.SetBounds(R);
    if FItemControlSeparators[I] <> -1 then
    begin
      ASeparatorInfo := AGroupViewInfo.GetSeparatorInfo(FItemControlSeparators[I]);
      OffsetRect(ASeparatorInfo.Bounds, AOffset, 0);
      AGroupViewInfo.SetSeparatorInfo(FItemControlSeparators[I], ASeparatorInfo);
    end;
  end;
end;

procedure TdxRibbonGroupLayoutCalculator.PlaceOneRowHeightItemControls(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AFirstIndex, ALastIndex: Integer;
  ADistributeOnGroupHeight: Boolean);
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AColumnFirstItemControlIndex, AItemControlWidth, AMaxRowX, ARow, ARowX, I: Integer;
  ADistributedControlIndex: Integer;
begin
  if AGroupViewInfo.GetItemControlViewInfo(AFirstIndex).HasSeparator then
    PlaceSeparator(AGroupViewInfo, AFirstIndex);
  ARow := 0;
  AMaxRowX := X;
  ARowX := X;
  AColumnFirstItemControlIndex := AFirstIndex;
  for I := AFirstIndex to ALastIndex do
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    if I > AFirstIndex then
      if (AItemControlViewInfo.GetPosition = ipBeginsNewRow) and
        (AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpNone, bgrpStart, bgrpSingle]) then
      begin
        Inc(ARow);
        if ARow < GroupRowCount then
          ARowX := X
        else
        begin
          AlignOneRowHeightItemControls(AGroupViewInfo, AColumnFirstItemControlIndex, AMaxRowX - X);
          AColumnFirstItemControlIndex := I;
          ARow := 0;
          X := AMaxRowX;
          IncrementColumnCount(AGroupViewInfo, I - 1);
          ARowX := X;
          AMaxRowX := X;
        end;
      end;
    AItemControlWidth := GetItemControlCurrentWidth(AItemControlViewInfo);
    FItemControlColumns[I] := FColumnCount;
    AItemControlViewInfo.SetRow(ARow);
    if not (AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpMember, bgrpFinish]) and
      (I > AFirstIndex)  and (FItemControlColumns[I - 1] = FItemControlColumns[I]) and (AGroupViewInfo.GetItemControlViewInfo(I - 1).GetRow = ARow) then
    begin
      if (AItemControlViewInfo.GetRealPositionInButtonGroup in [bgrpStart, bgrpSingle]) or
        (AItemControlViewInfo.GetRealPositionInButtonGroup = bgrpNone) and (AGroupViewInfo.GetItemControlViewInfo(I - 1).GetRealPositionInButtonGroup in [bgrpFinish, bgrpSingle]) then
          Inc(ARowX, AGroupViewInfo.GetOffsetsInfo.ButtonGroupOffset);
    end;
    AItemControlViewInfo.SetBounds(Rect(ARowX, ARow * GroupRowHeight, ARowX + AItemControlWidth, (ARow + 1) * GroupRowHeight));
    Inc(ARowX, AItemControlWidth);
    if ARowX > AMaxRowX then
      AMaxRowX := ARowX;
  end;
  AlignOneRowHeightItemControls(AGroupViewInfo, AColumnFirstItemControlIndex, AMaxRowX - X);
  X := AMaxRowX;

  ADistributedControlIndex := AFirstIndex;
  for I := AFirstIndex to ALastIndex do
    if AGroupViewInfo.GetItemControlViewInfo(I).IsPrimaryForDistribution then
    begin
      ADistributedControlIndex := I;
      Break;
    end;

  ADistributeOnGroupHeight := AGroupViewInfo.GetItemControlViewInfo(ADistributedControlIndex).GetDistributed;
  for I := AFirstIndex to ALastIndex do
    AGroupViewInfo.GetItemControlViewInfo(I).SetDistributed(ADistributeOnGroupHeight);
  if ADistributeOnGroupHeight then
    DistributeOneRowHeightItemControlsOnGroupHeight(AGroupViewInfo, AFirstIndex, ALastIndex);
end;

procedure TdxRibbonGroupLayoutCalculator.PlaceSeparator(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AItemControlIndex: Integer);
var
  ASeparatorInfo: TdxBarItemSeparatorInfo;
begin
  ASeparatorInfo.Bounds := Rect(X, dxRibbonItemSeparatorTopOffset,
    X + dxRibbonItemSeparatorAreaWidth,
    GetGroupHeight - dxRibbonItemSeparatorBottomOffset);
  ASeparatorInfo.Kind := skVertical;
  AGroupViewInfo.AddSeparator(ASeparatorInfo);
  Inc(X, dxRibbonItemSeparatorAreaWidth);
  FItemControlSeparators[AItemControlIndex] := AGroupViewInfo.GetSeparatorCount - 1;
end;

procedure TdxRibbonGroupLayoutCalculator.PlaceWholeGroupHeightItemControl(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AIndex: Integer);
var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  AItemControlWidth: Integer;
begin
  AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AIndex);
  if AItemControlViewInfo.HasSeparator then
    PlaceSeparator(AGroupViewInfo, AIndex);
  AItemControlWidth := GetItemControlCurrentWidth(AItemControlViewInfo);
  AItemControlViewInfo.SetBounds(Rect(X, 0, X + AItemControlWidth, GetGroupHeight));
  FItemControlColumns[AIndex] := FColumnCount;
  AItemControlViewInfo.SetRow(0); // ???
  Inc(X, AItemControlWidth);
end;

function TdxRibbonGroupLayoutCalculator.ReduceOneRowHeightItemControlsWithText(
  AGroupViewInfo: IdxRibbonGroupViewInfo; AUpToViewLevel: TdxBarItemRealViewLevel): Boolean;

  function CanReduce(AItemControlIndex: Integer): Boolean;
  var
    AItemControlViewInfo: IdxBarItemControlViewInfo;
  begin
    AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AItemControlIndex);
    Result := (AItemControlViewInfo.GetViewLevel < AUpToViewLevel) and
      (AUpToViewLevel in AItemControlViewInfo.GetAllowedViewLevels);
  end;

var
  AColumn, AIndex, ARow, I: Integer;
  AColumnWidthAfterReducing, AColumnWidthBeforeReducing, ARowWidthAfterReducing, ARowWidthBeforeReducing: Integer;
  AItemControlsToBeReduced: TInterfaceList;
  AItemControlViewInfo: IdxBarItemControlViewInfo;
begin
  Result := False;
  AIndex := FItemControlIndexForOneRowHeightReducing;
  while (AIndex >= 0) and IsWholeGroupHeightItemControl(AGroupViewInfo, AIndex) do
    Dec(AIndex);
  FItemControlIndexForOneRowHeightReducing := AIndex;
  if AIndex = -1 then
    Exit;
  AColumn := FItemControlColumns[AIndex];
  AColumnWidthBeforeReducing := 0;
  AColumnWidthAfterReducing := 0;
  AItemControlsToBeReduced := TInterfaceList.Create;
  try
    while (AIndex >= 0) and (FItemControlColumns[AIndex] = AColumn) do
    begin
      if IsPartOfHorizontalBox(AGroupViewInfo, AIndex, True) then
      begin
        AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AIndex);
        ARowWidthBeforeReducing := GetItemControlCurrentWidth(AItemControlViewInfo);
        ARow := AItemControlViewInfo.GetRow;
        Dec(AIndex);
        while (AIndex >= 0) and (FItemControlColumns[AIndex] = AColumn) and
          (AGroupViewInfo.GetItemControlViewInfo(AIndex).GetRow = ARow) do
        begin
          AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AIndex);
          Inc(ARowWidthBeforeReducing, GetItemControlCurrentWidth(AItemControlViewInfo));
          Dec(AIndex);
        end;
        ARowWidthAfterReducing := ARowWidthBeforeReducing;
      end
      else
      begin
        AItemControlViewInfo := AGroupViewInfo.GetItemControlViewInfo(AIndex);
        ARowWidthBeforeReducing := GetItemControlCurrentWidth(AItemControlViewInfo);
        if CanReduce(AIndex) then
        begin
          ARowWidthAfterReducing := AItemControlViewInfo.GetWidth(AUpToViewLevel);
          AItemControlsToBeReduced.Add(AItemControlViewInfo);
        end
        else
          ARowWidthAfterReducing := ARowWidthBeforeReducing;
        Dec(AIndex);
      end;
      if ARowWidthBeforeReducing > AColumnWidthBeforeReducing then
        AColumnWidthBeforeReducing := ARowWidthBeforeReducing;
      if ARowWidthAfterReducing > AColumnWidthAfterReducing then
        AColumnWidthAfterReducing := ARowWidthAfterReducing;
    end;
    for I := 0 to AItemControlsToBeReduced.Count - 1 do
    begin
      AItemControlViewInfo := IdxBarItemControlViewInfo(AItemControlsToBeReduced[I]);
      if (AItemControlViewInfo.GetWidth(AUpToViewLevel) < AColumnWidthBeforeReducing) and
        (GetItemControlCurrentWidth(AItemControlViewInfo) > AColumnWidthAfterReducing) then
      begin
        AItemControlViewInfo.SetViewLevel(AUpToViewLevel);
        Result := True;
      end;
    end;
    FItemControlIndexForOneRowHeightReducing := AIndex;
    if not Result then
      Result := ReduceOneRowHeightItemControlsWithText(AGroupViewInfo, AUpToViewLevel)
    else
      CalcLayout(AGroupViewInfo);
  finally
    AItemControlsToBeReduced.Free;
  end;
end;

function TdxRibbonGroupLayoutCalculator.TryReduceToSmallIconWithText(
  ASequence: TSequenceForWholeGroupHeightReducing): Boolean;

  function GetFreeCellCount(AOneRowItemCount: Integer): Integer;
  begin
    Result := (GroupRowCount - AOneRowItemCount mod GroupRowCount) mod GroupRowCount;
  end;

  function CanCollapse(const AItem: TRLCItemControlInfo): Boolean;
  var
    AFreeCellCountAfterReducing, AFreeCellCountBeforeReducing: Integer;
  begin
    if (AItem.OneRowItemsBefore = 0) and (ASequence[ASequence.Count - 1].ViewInfo = AItem.ViewInfo) and (ASequence.Count > 1) then
      Result := True
    else
    begin
      AFreeCellCountBeforeReducing := GetFreeCellCount(AItem.OneRowItemsBefore) +
        GetFreeCellCount(AItem.OneRowItemsAfter);
      AFreeCellCountAfterReducing := GetFreeCellCount(AItem.OneRowItemsBefore + 1 + AItem.OneRowItemsAfter);
      Result := AFreeCellCountAfterReducing <= AFreeCellCountBeforeReducing;
    end;
  end;

  procedure SetNextViewLevel(AItemControlViewInfo: IdxBarItemControlViewInfo; ALevel: TdxBarItemRealViewLevel);
  begin
    AItemControlViewInfo.SetViewLevel(GetNextViewLevel(AItemControlViewInfo.GetAllowedViewLevels, ALevel));
  end;

var
  AItemControlViewInfo: IdxBarItemControlViewInfo;
  APrevOneRowItemsAfter: Integer;
  AIndex: Integer;
begin
  AIndex := ASequence.Count - 1;
  Result := False;
  if AIndex >= 0 then
  begin
    APrevOneRowItemsAfter := 0;
    while (AIndex >= 0) and (APrevOneRowItemsAfter = 0) and CanCollapse(ASequence[AIndex]) do
    begin
      Result := True;
      AItemControlViewInfo := ASequence[AIndex].ViewInfo;
      SetNextViewLevel(AItemControlViewInfo, ivlLargeControlOnly);
      if AIndex > 0 then
      begin
        APrevOneRowItemsAfter := ASequence[AIndex - 1].OneRowItemsAfter;
        Inc(ASequence[AIndex - 1].OneRowItemsAfter, ASequence[AIndex].OneRowItemsAfter + 1);
      end;
      Dec(AIndex);
    end;
    ASequence.SetCount(AIndex+1);
  end;
end;

function TdxRibbonGroupLayoutCalculator.ReduceToSmallIconWithText(
  AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
var
  AIndex: Integer;
begin
  AIndex := FReducingSequences.Count - 1;
  Result := False;
  while not Result and (AIndex >= 0) do
  begin
    Result := TryReduceToSmallIconWithText(FReducingSequences[AIndex] as TSequenceForWholeGroupHeightReducing);
    if not Result then
    begin
      FReducingSequences.Delete(AIndex);
      Dec(AIndex);
    end
    else
      CalcLayout(AGroupViewInfo);
  end;
end;

function TdxRibbonGroupLayoutCalculator.ReduceToLargeControlOnly(
  AGroupViewInfo: IdxRibbonGroupViewInfo): Boolean;
var
  I: Integer;
  AViewInfo: IdxBarItemControlViewInfo;
begin
  Result := false;
  for I := AGroupViewInfo.GetItemControlCount - 1 downto 0 do
  begin
    AViewInfo := AGroupViewInfo.GetItemControlViewInfo(I);
    Result := (AViewInfo.GetViewLevel = ivlLargeIconWithText) and (ivlLargeControlOnly in AViewInfo.GetAllowedViewLevels);
    if Result then
    begin
      AViewInfo.SetViewLevel(ivlLargeControlOnly);
      CalcLayout(AGroupViewInfo);
      Break;
    end;
  end;
end;

end.
