{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxAutoCompleteWindow;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Messages, Graphics, Forms, Controls, ImgList, Menus,
  dxCore, dxCoreClasses, dxMessages, cxClasses, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, cxControls, cxContainer, cxListBox;

const
  dxAutoCompleteWindowBorderSize = 1;
  dxAutoCompleteWindowDefaultDropDownRows = 16;

type
  { TcxTextEditAutoCompleteInnerListBoxSizeGrip }

  TdxAutoCompleteInnerListBoxSizeGrip = class(TcxSizeGrip)
  protected
    procedure Paint; override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  end;

  { TdxCustomAutoCompleteInnerListBox }

  TdxCustomAutoCompleteInnerListBox = class(TdxCustomListBox)
  strict private
    FCircularKeyboardNavigation: Boolean;
    FHighlightSearchText: Boolean;
    FPrevMouseCursor: TPoint;

    FOnSelectItem: TNotifyEvent;

    procedure DoSelectItem;
    function GetSizeGripSize: TSize;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure DoLayoutChanged; override;

    procedure DrawItemText(const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState); override;
    function GetTextFlags: Integer; override;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    function ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean; override;
    // ScrollBars
    function AllowTouchScrollUIMode: Boolean; override;
    function GetSizeGripBounds: TRect; override;
    function GetSizeGripClass: TcxSizeGripClass; override;
    function GetVScrollBarBounds: TRect; override;
    procedure HideTouchScrollUIDirectly; override;
    function IsSizeGripVisible: Boolean; override;
    procedure InitScrollBarsParameters; override;
  public
    procedure AfterConstruction; override;
    function PointInSizeGrip(const P: TPoint): Boolean;
    //
    property CircularKeyboardNavigation: Boolean read FCircularKeyboardNavigation write FCircularKeyboardNavigation;
    property HighlightSearchText: Boolean read FHighlightSearchText write FHighlightSearchText;
    //
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  end;

  { TdxCustomAutoCompleteWindow }

  TdxCustomAutoCompleteWindow = class(TcxCustomPopupWindow)
  strict private
    FDisplayRowsCount: Integer;
    FInnerListBox: TdxCustomAutoCompleteInnerListBox;
    FLookAndFeel: TcxLookAndFeel;
    FPreviouslyEnteredText: string;

    FOnSelectItem: TNotifyEvent;
    FOnStoreSize: TNotifyEvent;

    function GetHighlightSearchText: Boolean;
    function GetItemsFont: TFont;
    function GetSearchText: string;
    function GetSelectedObject: TObject;
    function GetSelectedText: string;
    procedure SelectItemHandler(Sender: TObject);
    procedure SetHighlightSearchText(const Value: Boolean);
    procedure SetItemsFont(const Value: TFont);
    procedure SetSearchText(const Value: string);
    procedure SetSelectedObject(const Value: TObject);
    procedure SetSelectedText(const AValue: string);
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CalculateConstaints; virtual;
    function CalculateSize: TSize; override;
    function CreateInnerListBox: TdxCustomAutoCompleteInnerListBox; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoSelectItem; virtual;
    procedure DoStoreAutoCompleteWindowCustomizedSize; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure Paint; override;
    //
    property InnerListBox: TdxCustomAutoCompleteInnerListBox read FInnerListBox;
    property PreviouslyEnteredText: string read FPreviouslyEnteredText;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;
    function HasSelectedItem: Boolean;
    procedure Populate(AList: TStrings); virtual;
    procedure Popup(AFocusedControl: TWinControl); override;
    procedure ProcessNavigationKey(var Key: Word; Shift: TShiftState);
    procedure SetSize(AWidth, AHeight: Integer); overload;
    procedure SetSize(const ASize: TSize); overload;
    //
    property DisplayRowsCount: Integer read FDisplayRowsCount write FDisplayRowsCount;
    property HighlightSearchText: Boolean read GetHighlightSearchText write SetHighlightSearchText;
    property ItemsFont: TFont read GetItemsFont write SetItemsFont;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
    property SearchText: string read GetSearchText write SetSearchText;
    property SelectedObject: TObject read GetSelectedObject write SetSelectedObject;
    property SelectedText: string read GetSelectedText write SetSelectedText;
    //
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
    property OnStoreSize: TNotifyEvent read FOnStoreSize write FOnStoreSize;
  end;

implementation

uses
  cxDrawTextUtils, cxGeometry, Math;

{ TdxAutoCompleteInnerListBoxSizeGrip }

procedure TdxAutoCompleteInnerListBoxSizeGrip.Paint;
const
  ACorner: array [Boolean] of TdxCorner = (coBottomRight, coBottomLeft);
begin
  inherited Paint;
  cxPaintCanvas.BeginPaint(Canvas);
  try
    LookAndFeel.Painter.DrawSizeGrip(cxPaintCanvas, ClientRect, clDefault, ACorner[UseRightToLeftScrollBar]);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxAutoCompleteInnerListBoxSizeGrip.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

{ TdxCustomAutoCompleteInnerListBox }

procedure TdxCustomAutoCompleteInnerListBox.AfterConstruction;
begin
  inherited;
  CircularKeyboardNavigation := True;
end;

procedure TdxCustomAutoCompleteInnerListBox.DoLayoutChanged;
begin
  ItemHeight := CalculateItemHeight;
  inherited DoLayoutChanged;
end;

procedure TdxCustomAutoCompleteInnerListBox.DrawItemText(
  const R: TRect; AItem: TdxCustomListBoxItem; AState: TcxButtonState);
var
  ABounds: TRect;
  ASelStart: Integer;
begin
  if HighlightSearchText then
  begin
    ABounds := R;
    ASelStart := Pos(UpperCase(SearchText), UpperCase(AItem.Caption)) - 1;
    cxTextOut(Canvas.Canvas, AItem.Caption, ABounds, GetTextFlags, ASelStart, Length(SearchText), Font,
      LookAndFeelPainter.DefaultSelectionColor, LookAndFeelPainter.DefaultSelectionTextColor,
      0, 0, 0, GetTextColor(AItem, AState))
  end
  else
    inherited;
end;

function TdxCustomAutoCompleteInnerListBox.GetTextFlags: Integer;
begin
  if HighlightSearchText then
  begin
    Result := CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE or CXTO_END_ELLIPSIS;
    if UseRightToLeftReading then
      Result := Result or CXTO_RIGHT or CXTO_RTLREADING
    else
      Result := Result or CXTO_LEFT;
  end
  else
    Result := inherited;
end;

procedure TdxCustomAutoCompleteInnerListBox.DoSelectItem;
begin
  if Assigned(OnSelectItem) then OnSelectItem(Self);
end;

procedure TdxCustomAutoCompleteInnerListBox.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbVertical, 0, Items.Count - 1, 1, PageSize, TopIndex, True, False);
  SetScrollBarInfo(sbHorizontal, 0,  -1, 1, 1, 1, True, True);
end;

function TdxCustomAutoCompleteInnerListBox.IsSizeGripVisible: Boolean;
begin
  Result := True;
end;

procedure TdxCustomAutoCompleteInnerListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if not cxPointIsEqual(FPrevMouseCursor, Point(X, Y)) then
  begin
    FPrevMouseCursor := Point(X, Y);
    ItemIndex := ItemAtPos(FPrevMouseCursor);
  end;
end;

procedure TdxCustomAutoCompleteInnerListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ItemIndex <> -1 then
    DoSelectItem;
end;

function TdxCustomAutoCompleteInnerListBox.PointInSizeGrip(const P: TPoint): Boolean;
begin
  Result := ControlAtPos(P, False, True) = SizeGrip;
end;

function TdxCustomAutoCompleteInnerListBox.ProcessNavigationKey(var Key: Word; Shift: TShiftState): Boolean;

  function ValidateIndex(AIndex: Integer): Integer;
  begin
    if FCircularKeyboardNavigation then
    begin
      if AIndex < -1 then
        Result := Items.Count - 1
      else if AIndex >= Items.Count then
        Result := -1
      else
        Result := AIndex;
    end
    else
      Result := Min(Max(AIndex, 0), Items.Count - 1);
  end;

begin
  Result := True;
  case Key of
    VK_RETURN:
      DoSelectItem;
    VK_UP:
      ItemIndex := ValidateIndex(ItemIndex - 1);
    VK_DOWN:
      ItemIndex := ValidateIndex(ItemIndex + 1);
    else
      Result := inherited ProcessNavigationKey(Key, Shift);
  end;
end;

function TdxCustomAutoCompleteInnerListBox.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TdxCustomAutoCompleteInnerListBox.GetSizeGripBounds: TRect;
var
  ASize: TSize;
begin
  ASize := GetSizeGripSize;

  Result := ClientBounds;
  if IsPopupScrollBars then
    Result.Left := Result.Right - ASize.cx
  else
  begin
    Result.Left := Result.Right;
    Result.Right := Result.Left + ASize.cx;
  end;
  Result.Top := Result.Bottom - ASize.cy;

  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
end;

function TdxCustomAutoCompleteInnerListBox.GetSizeGripClass: TcxSizeGripClass;
begin
  Result := TdxAutoCompleteInnerListBoxSizeGrip;
end;

function TdxCustomAutoCompleteInnerListBox.GetSizeGripSize: TSize;
begin
  Result := Size(GetScrollBarSize.cx, GetScrollBarSize.cx);
end;

function TdxCustomAutoCompleteInnerListBox.GetVScrollBarBounds: TRect;
begin
  Result := ClientBounds;
  if IsPopupScrollBars then
    Result.Left := Result.Right - VScrollBar.Width
  else
  begin
    Result.Left := Result.Right;
    Result.Right := Result.Left + VScrollBar.Width;
  end;
  if IsSizeGripVisible then
    Dec(Result.Bottom, GetSizeGripSize.cy);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ClientBounds);
end;

procedure TdxCustomAutoCompleteInnerListBox.HideTouchScrollUIDirectly;
begin
  VScrollBar.Visible := False;
end;

procedure TdxCustomAutoCompleteInnerListBox.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if PointInSizeGrip(ScreenToClient(SmallPointToPoint(Message.Pos))) then
    Message.Result := HTTRANSPARENT
  else
    inherited;
end;

{ TdxCustomAutoCompleteWindow }

constructor TdxCustomAutoCompleteWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  OwnerParent := AOwnerControl;
  FDisplayRowsCount := dxAutoCompleteWindowDefaultDropDownRows;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FInnerListBox := CreateInnerListBox;
  FInnerListBox.Align := alClient;
  FInnerListBox.Parent := Self;
  FInnerListBox.BorderStyle := cxcbsNone;
  FInnerListBox.LookAndFeel.MasterLookAndFeel := FLookAndFeel;
  FInnerListBox.OnSelectItem := SelectItemHandler;
  CaptureFocus := False;
  UpdateScaleFactor;
end;

destructor TdxCustomAutoCompleteWindow.Destroy;
begin
  FreeAndNil(FInnerListBox);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TdxCustomAutoCompleteWindow.Populate(AList: TStrings);
var
  I: Integer;
begin
  FPreviouslyEnteredText := SearchText;
  InnerListBox.BeginUpdate;
  try
    InnerListBox.Clear;
    for I := 0 to AList.Count - 1 do
      InnerListBox.AddItem(AList[I], AList.Objects[I]);
    SelectedText := PreviouslyEnteredText;
  finally
    InnerListBox.EndUpdate;
  end;
end;

procedure TdxCustomAutoCompleteWindow.AdjustClientRect(var Rect: TRect);
begin
  InflateRect(Rect,
    -dxAutoCompleteWindowBorderSize,
    -dxAutoCompleteWindowBorderSize);
end;

procedure TdxCustomAutoCompleteWindow.CalculateConstaints;
begin
  Constraints.MinWidth := 2 * dxAutoCompleteWindowBorderSize + GetSystemMetrics(SM_CXVSCROLL);
  Constraints.MinHeight := Constraints.MinWidth;
end;

function TdxCustomAutoCompleteWindow.CalculateSize: TSize;
begin
  if Adjustable then
  begin
    InnerListBox.LayoutChanged;
    Result.cx := cxRectWidth(OwnerBounds);
    Result.cy := InnerListBox.CalculateContentSize(DisplayRowsCount).cy + 2 * dxAutoCompleteWindowBorderSize;
  end
  else
    Result := cxNullSize;
end;

function TdxCustomAutoCompleteWindow.CreateInnerListBox: TdxCustomAutoCompleteInnerListBox;
begin
  Result := TdxCustomAutoCompleteInnerListBox.Create(nil);
end;

procedure TdxCustomAutoCompleteWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if LookAndFeel.NativeStyle and IsWinVistaOrLater then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

procedure TdxCustomAutoCompleteWindow.DoSelectItem;
begin
  dxCallNotify(FOnSelectItem, Self);
end;

procedure TdxCustomAutoCompleteWindow.DoStoreAutoCompleteWindowCustomizedSize;
begin
  dxCallNotify(FOnStoreSize, Self);
end;

procedure TdxCustomAutoCompleteWindow.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  Invalidate;
end;

procedure TdxCustomAutoCompleteWindow.Paint;
begin
  Canvas.FrameRect(ClientRect, LookAndFeel.Painter.GetContainerBorderColor(False));
end;

procedure TdxCustomAutoCompleteWindow.Popup(AFocusedControl: TWinControl);
var
  P: TPoint;
  ASize: TSize;
begin
  CalculateConstaints;
  if not Visible then
    inherited Popup(AFocusedControl)
  else
    if Adjustable then
    begin
      ASize := CalculateSize;
      P := CalculatePosition(ASize);
      CorrectBoundsWithDesktopWorkArea(P, ASize);
      SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
    end;
end;

procedure TdxCustomAutoCompleteWindow.ProcessNavigationKey(var Key: Word; Shift: TShiftState);
begin
  InnerListBox.ProcessNavigationKey(Key, Shift);
end;

procedure TdxCustomAutoCompleteWindow.SelectItemHandler(Sender: TObject);
begin
  DoSelectItem;
end;

procedure TdxCustomAutoCompleteWindow.WMExitSizeMove(var Message: TMessage);
begin
  inherited;
  DoStoreAutoCompleteWindowCustomizedSize;
end;

procedure TdxCustomAutoCompleteWindow.WMNCHitTest(var Message: TWMNCHitTest);
const
  AHitTest: array [Boolean] of Integer = (HTBOTTOMRIGHT, HTBOTTOMLEFT);
begin
  if InnerListBox.PointInSizeGrip(InnerListBox.ScreenToClient(SmallPointToPoint(Message.Pos))) then
    Message.Result := AHitTest[UseRightToLeftScrollBar]
  else
    inherited;
end;

function TdxCustomAutoCompleteWindow.GetHighlightSearchText: Boolean;
begin
  Result := InnerListBox.HighlightSearchText;
end;

function TdxCustomAutoCompleteWindow.GetItemsFont: TFont;
begin
  Result := InnerListBox.Font;
end;

function TdxCustomAutoCompleteWindow.GetSearchText: string;
begin
  Result := InnerListBox.SearchText;
end;

function TdxCustomAutoCompleteWindow.GetSelectedObject: TObject;
begin
  Result := InnerListBox.Items[InnerListBox.ItemIndex].Data;
end;

function TdxCustomAutoCompleteWindow.GetSelectedText: string;
begin
  if InnerListBox.ItemIndex < 0 then
    Result := PreviouslyEnteredText
  else
    Result := InnerListBox.Items[InnerListBox.ItemIndex].Caption;
end;

function TdxCustomAutoCompleteWindow.HasSelectedItem: Boolean;
begin
  Result := InnerListBox.ItemIndex > -1;
end;

procedure TdxCustomAutoCompleteWindow.SetHighlightSearchText(const Value: Boolean);
begin
  InnerListBox.HighlightSearchText := Value;
end;

procedure TdxCustomAutoCompleteWindow.SetItemsFont(const Value: TFont);
begin
  InnerListBox.Font := Value;
end;

procedure TdxCustomAutoCompleteWindow.SetSearchText(const Value: string);
begin
  InnerListBox.SearchText := Value;
end;

procedure TdxCustomAutoCompleteWindow.SetSelectedObject(const Value: TObject);
begin
  InnerListBox.ItemIndex := InnerListBox.Items.IndexOfObject(Value);
end;

procedure TdxCustomAutoCompleteWindow.SetSelectedText(const AValue: string);
begin
  InnerListBox.ItemIndex := InnerListBox.Items.IndexOfCaption(AValue, False);
end;

procedure TdxCustomAutoCompleteWindow.SetSize(AWidth, AHeight: Integer);
begin
  SetBounds(Left, Top, AWidth, AHeight);
end;

procedure TdxCustomAutoCompleteWindow.SetSize(const ASize: TSize);
begin
  SetSize(ASize.cx, ASize.cy);
end;

end.
