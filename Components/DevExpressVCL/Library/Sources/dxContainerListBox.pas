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

unit dxContainerListBox;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGeometry, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, ComCtrls, cxClasses, cxListBox,
  Generics.Defaults, Generics.Collections;

type
  TdxControlItems = TcxComponentList;

  TdxCustomContainerListBox = class(TdxCustomListBox)
  public const
    FocusRectangleMargin = 1;
    IndentFromEdges = 1;
    SelectRectangleMargin = 2;
    SelectRectangleThickness = 2;
    ItemPadding = 10;
  private
    FColumnCount: Integer;
    FColumnWidth: Integer;
    FSelectRect: TPoint;
    FControlItems: TdxControlItems;
    FRowCount: Integer;
    FSelectedIndex: Integer;
    FIsMouseClickInItems: Boolean;
    FOnSelectedIndexChanged: TNotifyEvent;
    procedure CalculateRowCount;
    procedure ControlChange(Sender: TObject; AComponent: TComponent; AAction: TListNotification);
    procedure SetColumnCount(const AValue: Integer);
    procedure SetSelectedIndex(const Value: Integer);
    procedure UpdateItems;
    procedure UpdateControlItems;
    procedure UpdateSelectedRectCoordinate(AValue: Integer);
    procedure UpdateSelectIndexAfterMouseDown(const APos: TPoint);
  protected
    procedure ScaleFactorChanged; override;
    procedure CalculateColumnWidth;
    procedure DrawControl(const AControl: TWinControl; const APos: TPoint);
    procedure DrawFocusRectangle;
    procedure DrawSelectionRectangle;
    procedure DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    procedure DoLayoutChanged; override;
    procedure DoNumberingListBoxSelectedIndexChanged;
    procedure FocusEnter; override;
    procedure FocusLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    property SelectRect: TPoint read FSelectRect;
    property RowCount: Integer read FRowCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddControlItem(AControl: TWinControl);
    property ColumnCount: Integer read FColumnCount write SetColumnCount;
    property ColumnWidth: Integer read FColumnWidth;
    property ControlItems: TdxControlItems read FControlItems;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property IsMouseClickInItems: Boolean read FIsMouseClickInItems;
    property OnSelectedIndexChanged: TNotifyEvent read FOnSelectedIndexChanged write FOnSelectedIndexChanged;
  end;

  TdxContainerListBox = class(TdxCustomContainerListBox)
  published
    property Align;
    property Anchors;
    property OnResize;
  end;

implementation

uses
  Contnrs, Math, Character, dxTypeHelpers;

{ TdxCustomContainerListBox }

constructor TdxCustomContainerListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlItems := TdxControlItems.Create;
  FControlItems.OnNotify := ControlChange;
  FSelectRect.X := IndentFromEdges;
  FSelectRect.Y := IndentFromEdges;
  FColumnCount := 1;
end;

destructor TdxCustomContainerListBox.Destroy;
begin
  FreeAndNil(FControlItems);
  inherited Destroy;
end;

procedure TdxCustomContainerListBox.AddControlItem(AControl: TWinControl);
begin
  FControlItems.Add(AControl);
  AControl.Visible := False;
end;

procedure TdxCustomContainerListBox.CalculateRowCount;
begin
  if ColumnCount = 0 then
    Exit;
  FRowCount := ItemsAreaRect.Height div ItemHeight;
end;

procedure TdxCustomContainerListBox.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  UpdateSelectedRectCoordinate(FSelectedIndex);
end;

procedure TdxCustomContainerListBox.CalculateColumnWidth;
begin
  if ColumnCount = 0 then
    Exit;
  FColumnWidth := ItemsAreaRect.Width div ColumnCount;
end;

procedure TdxCustomContainerListBox.ControlChange(Sender: TObject; AComponent: TComponent; AAction: TListNotification);
begin
  if AAction = lnAdded then
  begin
    TWinControl(AComponent).Parent := Self;
    TWinControl(AComponent).SetBounds(0, 0, ColumnWidth - 2 * ItemPadding, ItemHeight - 2 * ItemPadding);
  end;
  UpdateItems;
end;

procedure TdxCustomContainerListBox.DoLayoutChanged;
begin
  if FControlItems = nil then
    Exit;
  inherited DoLayoutChanged;
  CalculateColumnWidth;
  CalculateRowCount;
  UpdateControlItems;
end;

procedure TdxCustomContainerListBox.DoNumberingListBoxSelectedIndexChanged;
begin
  if Assigned(OnSelectedIndexChanged) then
    OnSelectedIndexChanged(Self);
end;

procedure TdxCustomContainerListBox.DrawControl(const AControl: TWinControl; const APos: TPoint);
var
  ANewPos: TPoint;
begin
  Canvas.FrameRect(cxRectBounds(APos, ColumnWidth - IndentFromEdges, ItemHeight - IndentFromEdges), clBlack);
  ANewPos := APos;
  ANewPos.Offset(ItemPadding, ItemPadding);
  cxPaintTo(AControl, Canvas, ANewPos, AControl.ClientRect);
end;

procedure TdxCustomContainerListBox.DrawFocusRectangle;
var
  ABoundsWidth, ABoundsHeight: Integer;
  ABounds: TRect;
begin
  ABoundsWidth := ColumnWidth - (FocusRectangleMargin * 2 + 1);
  ABoundsHeight := ItemHeight - (FocusRectangleMargin * 2 + 1);
  ABounds := TRect.CreateSize(SelectRect.X + FocusRectangleMargin, SelectRect.Y + FocusRectangleMargin, ABoundsWidth, ABoundsHeight);
  Canvas.DrawFocusRect(ABounds);
end;

procedure TdxCustomContainerListBox.DrawItem(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  I: Integer;
  APos: TPoint;
  AIndex: Integer;
begin
  APos := R.TopLeft;
  AIndex := AItem.Index * ColumnCount;
  for I := 0 to ColumnCount - 1 do
    if AIndex < ControlItems.Count then
    begin
      DrawControl(TWinControl(ControlItems[AIndex]), APos);
      Inc(APos.X, ColumnWidth);
      Inc(AIndex);
    end;
  DrawSelectionRectangle;
end;

procedure TdxCustomContainerListBox.DrawSelectionRectangle;
var
  ASelectRectWidth, ASelectRectHeight: Integer;
  ADrawRect: TRect;
begin
  ASelectRectWidth := ColumnWidth - (SelectRectangleMargin * 2) - 1;
  ASelectRectHeight := ItemHeight - (SelectRectangleMargin * 2) - 1;
  ADrawRect := TRect.CreateSize(SelectRect.X + SelectRectangleMargin, SelectRect.Y + SelectRectangleMargin, ASelectRectWidth, ASelectRectHeight);
  Canvas.FrameRect(ADrawRect, clHighlight, SelectRectangleThickness);
end;

procedure TdxCustomContainerListBox.FocusEnter;
begin
  inherited FocusEnter;
  Refresh;
end;

procedure TdxCustomContainerListBox.FocusLeave;
begin
  inherited FocusLeave;
  Refresh;
end;

procedure TdxCustomContainerListBox.KeyDown(var Key: Word; Shift: TShiftState);

  procedure KeyDownPageUp;
  var
    ASelectedRow: Integer;
  begin
    if TopIndex = 0 then
      Exit;
    ASelectedRow := ItemIndex - TopIndex;
    if TopIndex - RowCount >= 0 then
    begin
      Dec(FSelectedIndex, RowCount * ColumnCount);
      TopIndex := TopIndex - RowCount;
    end
    else
    begin
      Dec(FSelectedIndex, TopIndex * ColumnCount);
      TopIndex := 0;
    end;
    ItemIndex := TopIndex - ASelectedRow;
    DoNumberingListBoxSelectedIndexChanged;
    Refresh;
  end;

  procedure KeyDownPageDown;
  var
    AValue: Integer;
    ANewSelectedIndex: Integer;
    ADeltaScroll: Integer;
    ABottomBorder: Integer;
    ADeltaColumn: Integer;
  begin
    if Items.Count < RowCount then
      Exit;
    AValue := RowCount * ColumnCount;
    ANewSelectedIndex := SelectedIndex + AValue;
    ADeltaScroll := RowCount;
    ABottomBorder := TopIndex + 2 * RowCount;
    if ABottomBorder > Items.Count then
    begin
      ADeltaColumn := ABottomBorder - Items.Count;
      Dec(ANewSelectedIndex, ADeltaColumn * ColumnCount);
      Dec(ADeltaScroll, ADeltaColumn);
    end;
    if (ANewSelectedIndex < ControlItems.Count) then
    begin
      TopIndex := TopIndex + ADeltaScroll;
      ItemIndex := ItemIndex + ADeltaScroll;
      FSelectedIndex := ANewSelectedIndex;
      DoNumberingListBoxSelectedIndexChanged;
    end;
    Refresh;
  end;

  procedure KeyDownEnd;
  begin
    SelectedIndex := ControlItems.Count - 1;
  end;

  procedure KeyDownHome;
  begin
    SelectedIndex := 0;
  end;

  procedure KeyDownLeft;
  begin
    if SelectedIndex = 0 then
      Exit;
    if (SelectRect.X > IndentFromEdges) then
      Dec(FSelectRect.X, ColumnWidth)
    else
    begin
      if SelectRect.Y = indentFromEdges then
        TopIndex := TopIndex - 1
      else
        Dec(FSelectRect.Y, ItemHeight);
      FSelectRect.X := (ColumnCount - 1) * ColumnWidth + IndentFromEdges;
    end;
    Dec(FSelectedIndex);
    DoNumberingListBoxSelectedIndexChanged;
    Refresh;
  end;

  procedure KeyDownUp;
  var
    AIndex: Integer;
  begin
    AIndex := SelectedIndex - ColumnCount;
    if AIndex < 0 then
      Exit;
    if (SelectRect.Y = IndentFromEdges) then
      TopIndex := TopIndex - 1
    else
      Dec(FSelectRect.Y, ItemHeight);
    Dec(FSelectedIndex, ColumnCount);
    DoNumberingListBoxSelectedIndexChanged;
    Refresh;
  end;

  procedure KeyDownRight;
  begin
    if SelectedIndex = ControlItems.Count - 1 then
      Exit;
    if FSelectRect.X < ColumnWidth * (ColumnCount - 1) then
       inc(FSelectRect.X, ColumnWidth)
    else
    begin
      if SelectRect.Y >= ColumnWidth * (RowCount - 1) then
        TopIndex := TopIndex + 1
      else
        Inc(FSelectRect.Y, ItemHeight);
      ItemIndex := ItemIndex + 1;
      FSelectRect.X := IndentFromEdges;
    end;
    Inc(FSelectedIndex);
    DoNumberingListBoxSelectedIndexChanged;
    Refresh;
  end;

  procedure KeyDownDown;
  var
    ANewSelectedIndex: Integer;
  begin
    ANewSelectedIndex := SelectedIndex + ColumnCount;
    if ANewSelectedIndex >= ControlItems.Count then
      Exit;
    if FSelectRect.Y >= ItemHeight * (RowCount - 1) then
      TopIndex := TopIndex + 1
    else
      Inc(FSelectRect.Y, ItemHeight);
    FSelectedIndex := ANewSelectedIndex;
    DoNumberingListBoxSelectedIndexChanged;
    Refresh;
  end;

begin
  case Key of
  VK_PRIOR:
    KeyDownPageUp;
  VK_NEXT:
    KeyDownPageDown;
  VK_END:
    KeyDownEnd;
  VK_HOME:
    KeyDownHome;
  VK_LEFT:
    KeyDownLeft;
  VK_UP:
    KeyDownUp;
  VK_RIGHT:
    KeyDownRight;
  VK_DOWN:
    KeyDownDown;
  else
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TdxCustomContainerListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  UpdateSelectIndexAfterMouseDown(Point(X, Y));
end;

procedure TdxCustomContainerListBox.Resize;
begin
  inherited Resize;
  UpdateItems;
  CalculateColumnWidth;
  CalculateRowCount;
  UpdateControlItems;
end;

procedure TdxCustomContainerListBox.SetColumnCount(const AValue: Integer);
begin
  if FColumnCount <> AValue then
  begin
    FColumnCount := AValue;
    UpdateItems;
  end;
end;

procedure TdxCustomContainerListBox.SetSelectedIndex(const Value: Integer);
begin
  FSelectedIndex := Value;
  Assert(not((FSelectedIndex < -1) or (FSelectedIndex > FControlItems.Count - 1)));
  UpdateSelectedRectCoordinate(Value);
  DoNumberingListBoxSelectedIndexChanged;
  Refresh;
end;

procedure TdxCustomContainerListBox.UpdateControlItems;
var
  AControl: TWinControl;
  AWidth, AHeight, I: Integer;
begin
  AWidth := ColumnWidth - 2 * ItemPadding;
  AHeight := ItemHeight - 2 * ItemPadding;
  for I := 0 to FControlItems.Count - 1 do
  begin
    AControl := TWinControl(FControlItems[I]);
    AControl.Width := AWidth;
    AControl.Height := AHeight;
  end;
end;

procedure TdxCustomContainerListBox.UpdateItems;
var
  I, N: Integer;
begin
  if IsDestroying then
    Exit;
  N := (FControlItems.Count + ColumnCount - 1) div ColumnCount;
  if N = Items.Count then
    Exit;
  Items.BeginUpdate;
  Items.Clear;
  for I := 0 to N - 1 do
    Items.Add('');
  Items.EndUpdate;
end;

procedure TdxCustomContainerListBox.UpdateSelectedRectCoordinate(AValue: Integer);
begin
  if ControlItems.Count = 0 then
    Exit;
  if AValue = -1 then
  begin
    FSelectRect.X := -ColumnWidth;
    FSelectRect.Y := -ItemHeight;
    Exit;
  end;
  ItemIndex := AValue div ColumnCount;
  if ItemIndex < TopIndex then
    TopIndex := ItemIndex;
  if ItemIndex >= RowCount then
    TopIndex := ItemIndex - RowCount + 1;
  FSelectRect.X := (AValue mod ColumnCount) * ColumnWidth + IndentFromEdges;
  FSelectRect.Y := (ItemIndex - TopIndex) * ItemHeight + IndentFromEdges;
end;

procedure TdxCustomContainerListBox.UpdateSelectIndexAfterMouseDown(const APos: TPoint);
var
  I: Integer;
  ASelPos: TPoint;
  ARect: TRect;
  AVisibleItemIndex: Integer;
  ANumberSelectedColumn: Integer;
begin
  FIsMouseClickInItems := False;
  ASelPos.X := IndentFromEdges;
  ASelPos.Y := IndentFromEdges - ItemHeight;
  AVisibleItemIndex := TopIndex * ColumnCount;
  for I := AVisibleItemIndex to Min(ControlItems.Count - 1, AVisibleItemIndex + ColumnCount * RowCount) do
  begin
    if I mod ColumnCount = 0 then
    begin
      ASelPos.X := IndentFromEdges;
      Inc(ASelPos.Y, ItemHeight);
    end;
    ARect := TRect.CreateSize(ASelPos, ColumnWidth, ItemHeight);
    if PtInRect(ARect, APos) then
    begin
      FSelectRect := ASelPos;
      ANumberSelectedColumn := I mod ColumnCount;
      FSelectedIndex := ItemIndex * ColumnCount + ANumberSelectedColumn;
      FIsMouseClickInItems := true;
      DoNumberingListBoxSelectedIndexChanged;
      Refresh;
      Break;
    end;
    Inc(ASelPos.X, ColumnWidth);
  end;
end;

end.
