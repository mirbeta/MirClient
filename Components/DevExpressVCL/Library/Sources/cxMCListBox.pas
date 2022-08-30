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

unit cxMCListBox;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, ExtCtrls, Forms, Graphics, ImgList, Messages, Types,
  StdCtrls, SysUtils,
  dxCore, cxClasses, cxContainer, cxControls, cxDataUtils,
  cxEdit, cxExtEditConsts, cxExtEditUtils, cxGraphics, cxHeader, cxLookAndFeelPainters,
  cxLookAndFeels, cxScrollBar, cxCustomListBox;

type
  TcxMCInnerHeader = class;
  TcxMCListBox = class;
  TcxMCInnerListBoxContainer = class;

  { TcxMCInnerHeader }

  TcxMCInnerHeader = class(TcxHeader, IcxContainerInnerControl)
  private
    FContainer: TcxContainer;

    function GetControlContainer: TcxContainer;
    function GetControl: TWinControl;
    function GetContainer: TcxMCListBox;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure AdjustSize; override;
    procedure Click; override;
    function IsInnerControl: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateHeight;
    //
    property Container: TcxMCListBox read GetContainer;
  public
    constructor Create(AOwner: TComponent); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

  { TcxMCInnerListBox }

  TcxMCInnerListBox = class(TcxCustomInnerListBox)
  strict private
    FItems: TStrings;
    FMCListBox: TcxMCListBox;
    FVScrollBarVisible: Boolean;

    function GetMCListBox: TcxMCListBox;
    function IsVScrollBarVisible: Boolean;
    procedure ItemsChanged(Sender: TStrings; AStartIndex, AEndIndex: Integer);
    procedure SetMCListBox(Value: TcxMCListBox);
    procedure SetItems(Value: TStrings);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure Click; override;
    procedure RecalcItemRects(AStartIndex: Integer = -1; AEndIndex: Integer = -1); virtual;
    procedure DrawLine(X, Y1, Y2: Integer); virtual;
    procedure DrawLines; virtual;
    procedure FullRepaint; virtual;
    function GetRootContainer: TcxContainer; override;

    property MCListBox: TcxMCListBox read GetMCListBox write SetMCListBox;
    property VScrollBarVisible: Boolean read FVScrollBarVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanFocus: Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property Items: TStrings read FItems write SetItems;
  end;

  { TcxMCInnerListBoxContainer }

  TcxMCInnerListBoxContainer = class(TcxCustomEditContainer)
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure DoScrollUIModeChanged; override;
    function GetBorderExtent: TRect; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function NeedsScrollBars: Boolean; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure WndProc(var Message: TMessage); override;
  end;

  { TcxMCInnerPanel }

  TcxMCInnerPanel = class(TcxControl)
  strict private
    function GetMCListBox: TcxMCListBox;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure BoundsChanged; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Paint; override;
    //
    property MCListBox: TcxMCListBox read GetMCListBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TcxMCListBox }

  TcxMCListBox = class(TcxCustomListBox)
  private
    FAlignment: TAlignment;
    FColumnLineColor: TColor;
    FDelimiter: Char;
    FInnerHeader: TcxMCInnerHeader;
    FInnerHeaderSectionRectsWithoutScrollbar: TRects;
    FInnerHeaderSectionRectsWithScrollbar: TRects;
    FInnerListBoxContainer: TcxMCInnerListBoxContainer;
    FInnerPanel: TcxMCInnerPanel;
    FInternalFlagCreatedHeader: Boolean;
    FMultiLines: Boolean;
    FOverflowEmptyColumn: Boolean;
    FOverLoadList: TStringList;
    FSavedHScroll: TScrollEvent;
    FSavedIndex: Integer;
    FShowColumnLines: Boolean;
    FShowEndEllipsis: Boolean;
    FShowHeader: Boolean;
    // InternalFlags
    FIsInternalPaint: Boolean;
    FIsSorting: Boolean;

    function CalcCellTextRect(AApproximateRect: TRect; AItemIndex, AColumnIndex: Integer): TRect;
    procedure DrawCellTextEx(var ARect: TRect; AFlags, AItemIndex, AColumnIndex: Integer);
    procedure DrawCellText(ARect: TRect; AItemIndex, AColumnIndex: Integer);
    function GetCellRect(AItemIndex, AColumnIndex, ATop, ABottom: Integer; AVScrollBarVisible: Boolean): TRect;
    function GetCellTextRect(AItemIndex, AColumnIndex, ATop, ABottom: Integer; AVScrollBarVisible: Boolean): TRect;
    function GetDelimiter: Char;

    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
    function GetHeaderSectionRect(AIndex: Integer; AVScrollBarVisible: Boolean): TRect;
    function GetHeaderSections: TcxHeaderSections;
    procedure SetHeaderSections(Value: TcxHeaderSections);
    procedure SectionEndResizeHandler(HeaderControl: TcxCustomHeader; Section: TcxHeaderSection);
    procedure SectionTrackHandler(HeaderControl: TcxCustomHeader; Section: TcxHeaderSection; Width: Integer; State: TcxSectionTrackState);
    procedure SetMultiLines(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetShowEndEllipsis(Value: Boolean);
    procedure SetDelimiter(Value: Char);
    function GetHeaderDragReorder: Boolean;
    procedure SetHeaderDragReorder(Value: Boolean);
    procedure SetShowColumnLines(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);
    procedure SetColumnLineColor(Value: TColor);
    procedure SetOverflowEmptyColumn(Value: Boolean);
    procedure SectionsChangeHandler(Sender: TObject);
    procedure HScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure SectionEndDragHandler(Sender: TObject);
    procedure DrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure MeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    function GetExtendedSelect: Boolean;
    function GetItems: TStrings;
    function GetMultiSelect: Boolean;
    function GetSelCount: Integer;
    function GetSorted: Boolean;
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetItems(Value: TStrings);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    function GetInnerListBox: TcxMCInnerListBox;
  protected
    procedure AdjustInnerControl; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CreateWnd; override;
    function IsInternalControl(AControl: TControl): Boolean; override;
    procedure Loaded; override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure DoSetSize; override;
    procedure ScaleFactorChanged; override;
    // changes
    procedure CursorChanged; override;
    procedure FontChanged; override;
    procedure ItemsChanged;


    procedure AdjustChildsPosition;
    procedure CalcHeaderSectionRects;
    function CalcItemHeight(AIndex: Integer; AVScrollBarVisible: Boolean): Integer; virtual;
    function DoCreateInnerListBox: TcxCustomInnerListBox; override;
    function GetTextPart(AItemIndex, AColumnIndex: Integer): string;
    procedure FullRepaint;
    function GetInnerListBoxTop: Integer; override;
    function GetItemText(AItemIndex: Integer): string; override;
    function IndexOf(const S: string): Integer; override;
    procedure InitializeInnerListBox; override;
    procedure SectionSortChangedHandler(Sender: TObject; const Section: TcxHeaderSection; const ASortOrder: TcxHeaderSortOrder); virtual;
    procedure SetItemHeight(Value: Integer); override;

    property InnerHeader: TcxMCInnerHeader read FInnerHeader;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Focused: Boolean; override;
    procedure GetTabOrderList(List: TList); override;
    procedure AddItem(AItem: string; AObject: TObject);
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelected;
    function ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    function ItemVisible(Index: Integer): Boolean;
    procedure SelectAll;
    procedure CopySelection(ADestination: TCustomListControl);
    procedure MoveSelection(ADestination: TCustomListControl);

    property Count;
    property InnerListBox: TcxMCInnerListBox read GetInnerListBox;
    property ItemIndex;
    property SelCount: Integer read GetSelCount;
    property Selected;
    property TopIndex;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoComplete;
    property AutoCompleteDelay;
    property Anchors;
    property BiDiMode;
    property ColumnLineColor: TColor read FColumnLineColor write SetColumnLineColor default clBtnShadow;
    property Constraints;
    property Delimiter: Char read GetDelimiter write SetDelimiter default #59;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect: Boolean read GetExtendedSelect write SetExtendedSelect default True;
    property HeaderDragReorder: Boolean read GetHeaderDragReorder write SetHeaderDragReorder default False;
    property HeaderSections: TcxHeaderSections read GetHeaderSections write SetHeaderSections;
    property Images: TCustomImageList read GetImages write SetImages;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight default 16;
    property Items: TStrings read GetItems write SetItems;
    property MultiLines: Boolean read FMultiLines write SetMultiLines default False;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
    property OverflowEmptyColumn: Boolean read FOverflowEmptyColumn write SetOverflowEmptyColumn default True;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnLines: Boolean read FShowColumnLines write SetShowColumnLines default True;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write SetShowEndEllipsis default True;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property ShowHint;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Variants, Math, cxVariants, dxThemeConsts, dxThemeManager, dxUxTheme, cxGeometry;

type
  TcxHeaderSectionsAccess = class(TcxHeaderSections);
  TMCStringList = class(TStringList)
  private
    Sections: TcxHeaderSectionsAccess;
    Delimiter: Char;

    function Compare(AIndex1, AIndex2: Integer): Integer;
  public
    function GetComparedString(AIndex: Integer; ASection: TcxHeaderSection): string;
    procedure CustomSort(Compare: TStringListSortCompare); override;
  end;

  TStringsChangeEvent = procedure(Sender: TStrings;
    AStartIndex, AEndIndex: Integer) of object;

  TcxMCListBoxStrings = class(TStrings)
  private
    FStorage: TStrings;
    FUpdating: Boolean;
    FOnChange: TStringsChangeEvent;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure Changed(AStartIndex: Integer = -1;
      AEndIndex: Integer = -1); virtual;
    property Storage: TStrings read FStorage;
  public
    constructor Create(AStorage: TStrings); virtual;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    property OnChange: TStringsChangeEvent read FOnChange write FOnChange;
  end;

function ListCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := (List as TMCStringList).Compare(Index1, Index2);
end;

function TMCStringList.GetComparedString(AIndex: Integer; ASection: TcxHeaderSection): string;
begin
  Result := GetWord(ASection.DataIndex+1, Strings[AIndex], Delimiter);
end;

procedure TMCStringList.CustomSort(Compare: TStringListSortCompare);
begin
  inherited CustomSort(ListCompare);
end;

function TMCStringList.Compare(AIndex1, AIndex2: Integer): Integer;
var
  s1, s2: string;
  ASection: TcxHeaderSection;
  ASortedIndex: Integer;
begin
  Result := 0;
  ASortedIndex := 0;
  repeat
    ASection := Sections.GetSortedSection(ASortedIndex);
    if ASection = nil then
      Break;
    ASortedIndex := ASection.Index + 1;
    s1 := GetComparedString(AIndex1, ASection);
    s2 := GetComparedString(AIndex2, ASection);
    if ASection.SortOrder = soAscending then
      Result := AnsiCompareText(s1, s2)
    else
      Result := AnsiCompareText(s2, s1);
  until Result <> 0;
end;

{ TcxMCListBoxStrings }

constructor TcxMCListBoxStrings.Create(AStorage: TStrings);
begin
  inherited Create;
  FStorage := AStorage;
end;

procedure TcxMCListBoxStrings.Clear;
begin
  Storage.Clear;
end;

procedure TcxMCListBoxStrings.Delete(Index: Integer);
begin
  Storage.Delete(Index);
  Changed;
end;

procedure TcxMCListBoxStrings.Exchange(Index1, Index2: Integer);
begin
  Storage.Exchange(Index1, Index2);
end;

function TcxMCListBoxStrings.IndexOf(const S: string): Integer;
begin
  Result := Storage.IndexOf(S);
end;

procedure TcxMCListBoxStrings.Insert(Index: Integer; const S: string);
begin
  Storage.Insert(Index, S);
  Changed(Index, Count - 1);
end;

procedure TcxMCListBoxStrings.Move(CurIndex, NewIndex: Integer);
begin
  Storage.Move(CurIndex, NewIndex);
end;

function TcxMCListBoxStrings.Get(Index: Integer): string;
begin
  Result := Storage[Index];
end;

function TcxMCListBoxStrings.GetCount: Integer;
begin
  Result := Storage.Count;
end;

function TcxMCListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := Storage.Objects[Index];
end;

procedure TcxMCListBoxStrings.Put(Index: Integer; const S: string);
begin
  Storage[Index] := S;
end;

procedure TcxMCListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  Storage.Objects[Index] := AObject;
end;

procedure TcxMCListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if Updating then
    Storage.BeginUpdate
  else
    Storage.EndUpdate;
  if not Updating then
    Changed;
end;

procedure TcxMCListBoxStrings.Changed(AStartIndex: Integer = -1;
  AEndIndex: Integer = -1);
begin
  if not FUpdating and Assigned(FOnChange) then
    FOnChange(Self, AStartIndex, AEndIndex);
end;

{ TcxMCInnerListBox }

constructor TcxMCInnerListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TcxMCListBoxStrings.Create(inherited Items);
  TcxMCListBoxStrings(FItems).OnChange := ItemsChanged;
end;

destructor TcxMCInnerListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TcxMCInnerListBox.CanFocus: Boolean;
begin
  Result := MCListBox.CanFocus;
end;

function TcxMCInnerListBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    MCListBox.FDataBinding.ExecuteAction(Action);
end;

function TcxMCInnerListBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    MCListBox.FDataBinding.UpdateAction(Action);
end;

procedure TcxMCInnerListBox.Click;
begin
  if MCListBox.DataBinding.SetEditMode then
  begin
    inherited Click;
    Container.SetScrollBarsParameters;
  end;
end;

procedure TcxMCInnerListBox.FullRepaint;
begin
  cxInvalidateRect(Self, GetControlRect(Self), True);
end;

procedure TcxMCInnerListBox.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TcxMCInnerListBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if MCListBox.ShowColumnLines then
    DrawLines;
end;

procedure TcxMCInnerListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  RecalcItemRects;
end;

procedure TcxMCInnerListBox.RecalcItemRects(AStartIndex: Integer = -1; AEndIndex: Integer = -1);
var
  I: Integer;
begin
  MCListBox.CalcHeaderSectionRects;

  if (AStartIndex = -1) and (AEndIndex = -1) then
  begin
    AStartIndex := 0;
    AEndIndex := Items.Count - 1;
  end;

  if FVScrollBarVisible <> IsVScrollBarVisible then
  begin
    FVScrollBarVisible := not FVScrollBarVisible;
    MCListBox.AdjustChildsPosition;
  end;

  for I := AStartIndex to AEndIndex do
    Perform(LB_SETITEMHEIGHT, I, MCListBox.CalcItemHeight(I, FVScrollBarVisible));
end;

function TcxMCInnerListBox.GetMCListBox: TcxMCListBox;
begin
  Result := FMCListBox;
end;

function TcxMCInnerListBox.IsVScrollBarVisible: Boolean;
var
  AItemsHeight: Integer;
  I: Integer;
begin
  AItemsHeight := 0;
  for I := 0 to Items.Count - 1 do
  begin
    Inc(AItemsHeight, MCListBox.CalcItemHeight(I, False));
    if AItemsHeight > Height then
      Break;
  end;
  Result := AItemsHeight > Height;
end;

procedure TcxMCInnerListBox.ItemsChanged(Sender: TStrings; AStartIndex, AEndIndex: Integer);
begin
  RecalcItemRects(AStartIndex, AEndIndex);
  MCListBox.ItemsChanged;
end;

procedure TcxMCInnerListBox.SetMCListBox(Value: TcxMCListBox);
begin
  FMCListBox := Value;
end;

procedure TcxMCInnerListBox.DrawLine(X, Y1, Y2: Integer);
begin
  Dec(X, MCListBox.InnerHeader.GetSplitLineOffset);
  Canvas.Pen.Color := MCListBox.ColumnLineColor;
  Canvas.Pen.Width := 1;
  Canvas.MoveTo(X, Y1);
  Canvas.LineTo(X, Y2);
end;

procedure TcxMCInnerListBox.DrawLines;
var
  AColumnLineTop, I: Integer;
  ALastItemRect: TRect;
begin
  if Items.Count > 0 then
  begin
    ALastItemRect := ItemRect(Items.Count - 1);
    if ALastItemRect.Bottom > Height then
      Exit
    else
      AColumnLineTop := ALastItemRect.Bottom;
  end
  else
    AColumnLineTop := 0;

  for I := 0 to MCListBox.HeaderSections.Count - 1 do
  begin
    if UseRightToLeftAlignment then
      DrawLine(MCListBox.HeaderSections[I].Left, AColumnLineTop, Height)
    else
      DrawLine(MCListBox.HeaderSections[I].Right, AColumnLineTop, Height);
  end;
end;

function TcxMCInnerListBox.GetRootContainer: TcxContainer;
begin
  Result := FMCListBox;
end;

{ TcxMCInnerHeader }

constructor TcxMCInnerHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  TabStop := False;
end;

function TcxMCInnerHeader.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    Container.FDataBinding.ExecuteAction(Action);
end;

function TcxMCInnerHeader.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    Container.FDataBinding.UpdateAction(Action);
end;

procedure TcxMCInnerHeader.AdjustSize;
begin
  inherited AdjustSize;
  FitToClientWidth;
end;

procedure TcxMCInnerHeader.Click;
begin
  if Container.DataBinding.SetEditMode then
    inherited Click;
end;

function TcxMCInnerHeader.IsInnerControl: Boolean;
begin
  Result := True;
end;

procedure TcxMCInnerHeader.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if HandleAllocated then
    UpdateHeight;
end;

procedure TcxMCInnerHeader.Notification(AComponent: TComponent; Operation: TOperation);
var
  AHeightCalculatingNeeded: Boolean;
begin
  AHeightCalculatingNeeded := (Operation = opRemove) and (AComponent = Images);
  inherited Notification(AComponent, Operation);
  if AHeightCalculatingNeeded then
    UpdateHeight;
end;

procedure TcxMCInnerHeader.UpdateHeight;
var
  APrevHeight: Integer;
begin
  APrevHeight := Height;
  if Container.ShowHeader then
    Height := GetAutoHeight
  else
    Height := 0;
  if Height <> APrevHeight then
  begin
    Container.AdjustChildsPosition;
    Container.InnerListBox.RecalcItemRects;
  end;
end;

function TcxMCInnerHeader.GetControlContainer: TcxContainer;
begin
  Result := FContainer;
end;

function TcxMCInnerHeader.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxMCInnerHeader.GetContainer: TcxMCListBox;
begin
  Result := TcxMCListBox(FContainer);
end;

procedure TcxMCInnerHeader.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateHeight;
end;

procedure TcxMCInnerHeader.WMSetFocus(var Message: TWMSetFocus);
begin
  Container.InnerListBox.SetFocus;
end;

{ TcxMCInnerListBoxContainer }

function TcxMCInnerListBoxContainer.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TcxMCInnerListBoxContainer.DoScrollUIModeChanged;
begin
  if InnerControl <> nil then
  begin
    (InnerControl as TcxMCInnerListBox).MCListBox.CalcHeaderSectionRects;
    (InnerControl as TcxMCInnerListBox).RecreateWnd;
  end;
end;

function TcxMCInnerListBoxContainer.GetBorderExtent: TRect;
begin
  Result := cxEmptyRect
end;

function TcxMCInnerListBoxContainer.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TcxMCInnerListBoxContainer.NeedsScrollBars: Boolean;
begin
  Result := True;
end;

procedure TcxMCInnerListBoxContainer.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if Enabled then
    (InnerControl as TcxMCInnerListBox).Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxMCInnerListBoxContainer.WndProc(var Message: TMessage);
begin
  if InnerControl <> nil then
    case Message.Msg of
      LB_ADDSTRING .. LB_MSGMAX:
        begin
          with Message do
            Result := SendMessage(InnerControl.Handle, Msg, WParam, LParam);
          Exit;
        end;
    end;
  inherited WndProc(Message);
//  if (InnerControl <> nil) and (Message.Msg = WM_COMMAND) and (Message.WParamHi = LBN_SELCHANGE) then
//    (InnerControl as TcxMCInnerListBox).SetExternalScrollBarsParameters;
end;

{ TcxMCInnerPanel }

constructor TcxMCInnerPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  TabStop := False;
end;

procedure TcxMCInnerPanel.BoundsChanged;
begin
  inherited BoundsChanged;
  MCListBox.AdjustChildsPosition;
  MCListBox.InnerListBox.RecalcItemRects;
end;

procedure TcxMCInnerPanel.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  Invalidate;
end;

procedure TcxMCInnerPanel.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if UseRightToLeftScrollBar then
    R := Rect(0, 0, MCListBox.InnerHeader.Left, MCListBox.InnerHeader.Height)
  else
    R := Rect(MCListBox.InnerHeader.Width, 0, ClientWidth, MCListBox.InnerHeader.Height);
  InflateRect(R, 10, 0);
  LookAndFeel.Painter.DrawScaledHeaderControlSection(Canvas, R, cxEmptyRect, [],
    [bTop, bBottom], cxbsNormal, taLeftJustify, vaTop, False, False, '', Font,
    clNone, clBtnFace, ScaleFactor);
end;

function TcxMCInnerPanel.GetMCListBox: TcxMCListBox;
begin
  Result := Parent as TcxMCListBox;
end;

procedure TcxMCInnerPanel.WMSetFocus(var Message: TWMSetFocus);
var
  AInnerListBox: TcxMCInnerListBox;
begin
  AInnerListBox := MCListBox.InnerListBox;
  if AInnerListBox.CanFocus then
    AInnerListBox.SetFocus;
end;

{ TcxMCListBox }

constructor TcxMCListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlignment := taLeftJustify;
  FShowEndEllipsis := True;
  FDelimiter := ';';
  FOverflowEmptyColumn := True;
  FShowColumnLines := True;
  FColumnLineColor := clBtnShadow;
  FSavedIndex := -1;
  FShowHeader := True;
  Width := 121;
  Height := 97;
  FOverLoadList := TStringList.Create;

  FInnerPanel := TcxMCInnerPanel.Create(Self);
  FInnerPanel.Parent := Self;
  FInnerPanel.Align := alClient;
  FInnerPanel.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;

  FInnerHeader := TcxMCInnerHeader.Create(FInnerPanel);
  InnerControl := FInnerPanel;
  FInnerHeader.Color := clBtnFace;
  FInnerHeader.ParentFont := True;
  FInnerHeader.Parent := FInnerPanel;
  FInnerHeader.FContainer := Self;
  FInnerHeader.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
  FInnerHeader.OnSectionEndResize := SectionEndResizeHandler;
  FInnerHeader.OnSectionTrack := SectionTrackHandler;
  FInnerHeader.OnSectionsChange := SectionsChangeHandler;
  FInnerHeader.OnSectionChangedSortOrder := SectionSortChangedHandler;
  FInnerHeader.OnSectionEndDrag := SectionEndDragHandler;
  FInnerHeader.AllowSort := True;
  FInnerHeader.ResizeUpdate := False;

  FInnerListBoxContainer := TcxMCInnerListBoxContainer.Create(FInnerPanel);
  FInnerListBoxContainer.ParentColor := True;
  FInnerListBoxContainer.Parent := FInnerPanel;
  FInnerListBoxContainer.Cursor := Cursor;
  FInnerListBoxContainer.Style.LookAndFeel.MasterLookAndFeel := Style.LookAndFeel;
  CreateInnerListBox;
  FInnerListBoxContainer.InnerControl := InnerListBox;
  DataBinding.VisualControl := InnerListBox;
end;

destructor TcxMCListBox.Destroy;
begin
  FreeAndNil(FOverLoadList);
  inherited Destroy;
end;

function TcxMCListBox.Focused: Boolean;
begin
  Result := inherited Focused or InnerListBox.Focused or FInnerHeader.Focused;
end;

procedure TcxMCListBox.AddItem(AItem: string; AObject: TObject);
begin
  Items.AddObject(AItem, AObject);
end;

procedure TcxMCListBox.Clear;
begin
  Items.Clear;
end;

procedure TcxMCListBox.ClearSelection;
begin
  InnerListBox.ClearSelection;
end;

procedure TcxMCListBox.DeleteSelected;
begin
  InnerListBox.DeleteSelected;
end;

function TcxMCListBox.ItemAtPos(const APos: TPoint; AExisting: Boolean): Integer;
var
  P: TPoint;
begin
  P := cxParentToClient(InnerListBox, APos, Self);
  Result := InnerListBox.ItemAtPos(P, AExisting);
end;

function TcxMCListBox.ItemRect(Index: Integer): TRect;
begin
  Result := InnerListBox.ItemRect(Index);
end;

function TcxMCListBox.ItemVisible(Index: Integer): Boolean;
begin
  Result := InnerListBox.ItemVisible(Index);
end;

procedure TcxMCListBox.SelectAll;
begin
  InnerListBox.SelectAll;
end;

procedure TcxMCListBox.CopySelection(ADestination: TCustomListControl);
begin
  InnerListBox.CopySelection(ADestination);
end;

procedure TcxMCListBox.MoveSelection(ADestination: TCustomListControl);
begin
  InnerListBox.MoveSelection(ADestination);
end;

procedure TcxMCListBox.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  AdjustInnerControl;
end;

procedure TcxMCListBox.CreateWnd;
var
  FSection: TcxHeaderSection;
begin
  inherited;
  if not FInternalFlagCreatedHeader then
  begin
    FInternalFlagCreatedHeader := True;
    if (FInnerHeader.Sections.Count = 0) then
    begin
      FSection := FInnerHeader.Sections.Add;
      FSection.Text := 'Section #1';
      FSection.Width := FInnerHeader.Canvas.TextWidth(FSection.Text) + 4;
    end;
  end;
end;

procedure TcxMCListBox.CursorChanged;
begin
  inherited CursorChanged;
  if InnerListBox <> nil then
    InnerListBox.Cursor := Cursor;
end;

procedure TcxMCListBox.FontChanged;
begin
  inherited;
  FullRepaint;
end;

procedure TcxMCListBox.ItemsChanged;
begin
  if HeaderSections.FirstSortedSection <> nil then
    InnerHeader.OnSectionChangedSortOrder(FInnerHeader, HeaderSections.FirstSortedSection, HeaderSections.FirstSortedSection.SortOrder);
end;

procedure TcxMCListBox.AdjustInnerControl;
var
  AFont: TFont;
begin
  FInnerHeader.Font := VisibleFont;
  FInnerListBoxContainer.Style.Color := ViewInfo.BackgroundColor;
  AFont := TFont.Create;
  try
    AFont.Assign(Style.GetVisibleFont);
    AFont.Color := VisibleFont.Color;
    FInnerListBoxContainer.Style.Font := AFont;
  finally
    AFont.Free;
  end;
end;

procedure TcxMCListBox.GetTabOrderList(List: TList);
var
  AActiveControl: TWinControl;
begin
  AActiveControl := GetParentForm(Self).ActiveControl;
  if (AActiveControl <> Self) and CanFocus and (InnerListBox = AActiveControl) then
  begin
    List.Add(InnerListBox);
    List.Remove(Self);
  end;
end;

function TcxMCListBox.IsInternalControl(AControl: TControl): Boolean;
begin
  if InnerListBox = nil then
    Result := True
  else
    Result := (AControl = InnerListBox.HScrollBar) or (AControl = InnerListBox.VScrollBar);
  Result := Result or inherited IsInternalControl(AControl);
end;

procedure TcxMCListBox.Loaded;
begin
  inherited;
  FontChanged;
end;

procedure TcxMCListBox.SetDragMode(Value: TDragMode);
begin
  inherited;
  InnerListBox.DragMode := Value;
end;

procedure TcxMCListBox.DoSetSize;
var
  ANewHeight: Integer;
  APrevBoundsRect: TRect;
begin
  if IsLoading then
    Exit;
  APrevBoundsRect := InnerListBox.BoundsRect;
  try
    if not IntegralHeight or (Align in [alLeft, alRight, alClient]) then
    begin
      inherited;
      Exit;
    end;
    ANewHeight := Height;
    GetOptimalHeight(ANewHeight);
    if Height >= (FInnerHeader.Height + 2) then
      Height := ANewHeight
    else
      Height := FInnerHeader.Height + 2;
    inherited;
  finally
    if not EqualRect(APrevBoundsRect, InnerListBox.BoundsRect) and InnerListBox.HandleAllocated then
      dxMessagesController.KillMessages(InnerListBox.Handle, WM_MOUSEMOVE);
  end;
end;

function TcxMCListBox.CalcCellTextRect(AApproximateRect: TRect; AItemIndex, AColumnIndex: Integer): TRect;
begin
  Result := AApproximateRect;
  if MultiLines then
    DrawCellTextEx(Result, DT_CALCRECT or DT_NOPREFIX or DT_VCENTER, AItemIndex, AColumnIndex);
end;

procedure TcxMCListBox.DrawCellTextEx(var ARect: TRect; AFlags, AItemIndex, AColumnIndex: Integer);

  function GetDrawTextParams: TDrawTextParams;
  begin
    Result.cbSize := SizeOf(TDrawTextParams);
    Result.iTabLength := TabWidth;
    Result.iLeftMargin := 0;
    Result.iRightMargin := 0;
  end;

  function GetTextFlag(const AStartFlag: Longint): Longint;
  const
    ShowEndEllipsisArray: array[Boolean] of Integer = (0, DT_END_ELLIPSIS);
    WordWrapArray: array[Boolean] of Integer = (0, DT_WORDBREAK);
  begin
    Result := AStartFlag or
      SystemAlignmentsHorz[HeaderSections[AColumnIndex].Alignment] or
      WordWrapArray[MultiLines] or ShowEndEllipsisArray[ShowEndEllipsis];
    if not MultiLines then
      Result := Result or DT_SINGLELINE;
    if InnerListBox.TabWidth > 0 then
      Result := Result or DT_EXPANDTABS or DT_TABSTOP;
    Result := DrawTextBiDiModeFlags(Result);
  end;

var
  ADrawTextParams: TDrawTextParams;
  AText: string;
begin
  ADrawTextParams := GetDrawTextParams;
  AText := GetTextPart(AItemIndex, AColumnIndex);
  DrawTextEx(InnerListBox.Canvas.Handle, PChar(AText),
    Length(AText), ARect, GetTextFlag(AFlags), @ADrawTextParams);
end;

procedure TcxMCListBox.DrawCellText(ARect: TRect; AItemIndex, AColumnIndex: Integer);
begin
  DrawCellTextEx(ARect, DT_NOPREFIX or DT_VCENTER, AItemIndex, AColumnIndex);
end;

function TcxMCListBox.GetCellRect(AItemIndex, AColumnIndex, ATop, ABottom: Integer; AVScrollBarVisible: Boolean): TRect;
var
  I: Integer;
  AHeaderSectionRect: TRect;
begin
  AHeaderSectionRect := GetHeaderSectionRect(AColumnIndex, AVScrollBarVisible);
  Result := Rect(AHeaderSectionRect.Left + 2, ATop, AHeaderSectionRect.Right - 2, ABottom);
  if OverflowEmptyColumn and (HeaderSections.Items[AColumnIndex].Alignment = taLeftJustify) then
    for I := AColumnIndex + 1 to HeaderSections.Count - 1 do
      if GetTextPart(AItemIndex, I) = '' then
        if not UseRightToLeftAlignment then
          Result.Right := Result.Right + cxRectWidth(GetHeaderSectionRect(I, AVScrollBarVisible))
        else
          Result.Left := Result.Left - cxRectWidth(GetHeaderSectionRect(I, AVScrollBarVisible))
      else
        Break;
end;

function TcxMCListBox.GetCellTextRect(AItemIndex, AColumnIndex,
  ATop, ABottom: Integer; AVScrollBarVisible: Boolean): TRect;
begin
  Result := CalcCellTextRect(GetCellRect(AItemIndex, AColumnIndex, ATop, ABottom, AVScrollBarVisible),
    AItemIndex, AColumnIndex);
end;

function TcxMCListBox.GetDelimiter: Char;
begin
  Result := FDelimiter;
end;

function TcxMCListBox.GetImages: TCustomImageList;
begin
  Result := FInnerHeader.Images;
end;

procedure TcxMCListBox.SetImages(Value: TCustomImageList);
begin
  FInnerHeader.Images := Value;
  FInnerHeader.UpdateHeight;
end;

function TcxMCListBox.GetHeaderSectionRect(AIndex: Integer; AVScrollBarVisible: Boolean): TRect;
begin
  if not FInnerListBoxContainer.IsPopupScrollBars and AVScrollBarVisible then
    Result := FInnerHeaderSectionRectsWithScrollbar[AIndex]
  else
    Result := FInnerHeaderSectionRectsWithoutScrollbar[AIndex];
end;

function TcxMCListBox.GetHeaderSections: TcxHeaderSections;
begin
  Result := FInnerHeader.Sections;
end;

procedure TcxMCListBox.SetHeaderSections(Value: TcxHeaderSections);
begin
  FInnerHeader.Sections := Value;
end;

procedure TcxMCListBox.SetAlignment(Value: TAlignment);
var
  I: Integer;
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    for I := 0 to Pred(HeaderSections.Count) do
      HeaderSections[I].Alignment := FAlignment;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SetMultiLines(Value: Boolean);
begin
  if FMultiLines <> Value then
  begin
    FMultiLines := Value;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SetShowEndEllipsis(Value: Boolean);
begin
  if FShowEndEllipsis <> Value then
  begin
    FShowEndEllipsis := Value;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SetDelimiter(Value: Char);
begin
  if FDelimiter <> Value then
  begin
    FDelimiter := Value;
    FullRepaint;
  end;
end;

function TcxMCListBox.GetHeaderDragReorder: Boolean;
begin
  Result := FInnerHeader.DragReorder;
end;

procedure TcxMCListBox.SetHeaderDragReorder(Value: Boolean);
begin
  FInnerHeader.DragReorder := Value;
end;

procedure TcxMCListBox.SetShowColumnLines(Value: Boolean);
begin
  if FShowColumnLines <> Value then
  begin
    FShowColumnLines := Value;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SetShowHeader(Value: Boolean);
begin
  if Value <> FShowHeader then
  begin
    FShowHeader := Value;
    FInnerHeader.UpdateHeight;
  end;
end;

procedure TcxMCListBox.SetColumnLineColor(Value: TColor);
begin
  if FColumnLineColor <> Value then
  begin
    FColumnLineColor := Value;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SetOverflowEmptyColumn(Value: Boolean);
begin
  if FOverflowEmptyColumn <> Value then
  begin
    FOverflowEmptyColumn := Value;
    FullRepaint;
  end;
end;

procedure TcxMCListBox.SectionEndResizeHandler(HeaderControl: TcxCustomHeader; Section: TcxHeaderSection);
begin
  FullRepaint;
end;

procedure TcxMCListBox.SectionsChangeHandler(Sender: TObject);
begin
  CalcHeaderSectionRects;
  FullRepaint;
end;

procedure TcxMCListBox.HScrollHandler(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FSavedHScroll) then
    FSavedHScroll(Sender, ScrollCode, ScrollPos);
end;

procedure TcxMCListBox.ScaleFactorChanged;
begin
  inherited;
  cxRecreateControlWnd(InnerListBox);
end;

procedure TcxMCListBox.SectionEndDragHandler(Sender: TObject);
begin
  InnerListBox.Invalidate;
end;

procedure TcxMCListBox.SectionTrackHandler(HeaderControl: TcxCustomHeader;
  Section: TcxHeaderSection; Width: Integer; State: TcxSectionTrackState);
begin
  if (State = tsTrackEnd) then
    FullRepaint;
end;

procedure TcxMCListBox.FullRepaint;
begin
  Canvas.Lock;
  try
    if Count = 0 then
      InnerListBox.FullRepaint
    else
    begin
      FSavedIndex := ItemIndex;
      FIsInternalPaint := True;
      InnerListBox.RecalcItemRects;
      InnerListBox.FullRepaint;
      InnerListBox.ItemIndex := FSavedIndex;
    end;
  finally
    FIsInternalPaint := False;
    Canvas.Unlock;
  end;
end;

procedure TcxMCListBox.SectionSortChangedHandler(Sender: TObject;
  const Section: TcxHeaderSection; const ASortOrder: TcxHeaderSortOrder);
var
  TmpList: TMCStringList;
begin
  if (ASortOrder = soNone) or FIsSorting then
    Exit;

  TmpList := TMCStringList.Create;
  FIsSorting := True;
  try
    Items.BeginUpdate;
    try
      TmpList.Assign(Items);
      try
        TmpList.Delimiter := FDelimiter;
        TmpList.Sections := TcxHeaderSectionsAccess(HeaderSections);
        TmpList.Sort;
      finally
        Items.Assign(TmpList);
      end;
    finally
      Items.EndUpdate;
    end;
  finally
    FIsSorting := False;
    TmpList.Free;
  end;
end;

function TcxMCListBox.GetTextPart(AItemIndex, AColumnIndex: Integer): string;
var
  APartIndex: Integer;
begin
  APartIndex := HeaderSections[AColumnIndex].DataIndex;
  if APartIndex < 0 then
    Result := ''
  else
    Result := GetWord(APartIndex + 1, Items[AItemIndex], FDelimiter);
end;

procedure TcxMCListBox.DrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

  procedure AdjustCanvasColors;
  var
    ABackgroundColor, ATextColor: TColor;
  begin
    if (not FIsInternalPaint and (odSelected in State)) or
     (FIsInternalPaint and (FSavedIndex = Index)) then
    begin
      ABackgroundColor := LookAndFeelPainter.DefaultSelectionColor;
      ATextColor := LookAndFeelPainter.DefaultSelectionTextColor;
    end
    else
    begin
      ABackgroundColor := GetBackgroundColor;
      ATextColor := VisibleFont.Color;
    end;
    InnerListBox.Canvas.Brush.Color := ABackgroundColor;
    InnerListBox.Canvas.Font.Color := ATextColor;
  end;

  procedure DrawColumnSectionText(AColumnIndex: Integer);
  var
    ATextRect: TRect;
  begin
    ATextRect := GetCellRect(Index, AColumnIndex, ARect.Top, ARect.Bottom, InnerListBox.VScrollBarVisible);
    DrawCellText(ATextRect, Index, AColumnIndex);
    InnerListBox.Canvas.ExcludeClipRect(ATextRect);
  end;

  procedure DrawColumnSectionLine(AColumnIndex: Integer);
  var
    R: TRect;
    X: Integer;
  begin
    R := GetHeaderSectionRect(AColumnIndex, InnerListBox.VScrollBarVisible);
    if UseRightToLeftAlignment then
      X := R.Left
    else
      X := R.Right;
    InnerListBox.DrawLine(X, ARect.Top, ARect.Bottom);
  end;

var
  I: Integer;
begin
  AdjustCanvasColors;
  InnerListBox.Canvas.FillRect(ARect);
  for I := 0 to HeaderSections.Count - 1 do
  begin
    DrawColumnSectionText(I);
    if ShowColumnLines then
      DrawColumnSectionLine(I);
  end;
end;

procedure TcxMCListBox.MeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  Height := CalcItemHeight(Index, InnerListBox.VScrollBarVisible);
end;

procedure TcxMCListBox.AdjustChildsPosition;
var
  R, AParentRect: TRect;
begin
  R := cxRect(0, 0, FInnerPanel.ClientWidth, InnerHeader.Height);
  if InnerListBox.VScrollBarVisible then
  begin
    AParentRect := R;
    Dec(R.Right, FInnerListBoxContainer.GetVScrollBarDefaultAreaWidth);
    if UseRightToLeftScrollBar then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, AParentRect);
  end;
  InnerHeader.BoundsRect := R;
  FInnerListBoxContainer.SetBounds(
    0, InnerHeader.Height, FInnerPanel.ClientWidth, FInnerPanel.ClientHeight - InnerHeader.Height);
end;

procedure TcxMCListBox.CalcHeaderSectionRects;

  procedure InternalCalcHeaderSectionRects(AHeaderWidth: Integer; out ASectionRects: TRects);
  var
    ASectionWidths: TcxHeaderSectionWidths;
    I: Integer;
  begin
    InnerHeader.CalcSectionWidths(AHeaderWidth, ASectionWidths);
    SetLength(ASectionRects, InnerHeader.Sections.Count);
    for I := 0 to InnerHeader.Sections.Count - 1 do
      ASectionRects[I] := InnerHeader.GetSectionRectBySectionWidths(AHeaderWidth, ASectionWidths, I);
  end;

begin
  InternalCalcHeaderSectionRects(FInnerPanel.ClientWidth, FInnerHeaderSectionRectsWithoutScrollbar);
  InternalCalcHeaderSectionRects(FInnerPanel.ClientWidth - FInnerListBoxContainer.GetVScrollBarDefaultAreaWidth,
    FInnerHeaderSectionRectsWithScrollbar);
end;

function TcxMCListBox.CalcItemHeight(AIndex: Integer; AVScrollBarVisible: Boolean): Integer;
var
  I, ATextHeight: Integer;
  ACalcRect: TRect;
begin
  Result := ItemHeight;
  ATextHeight := cxTextHeight(InnerListBox.Font);
  Result := Max(Result, ATextHeight);
  for I := 0 to HeaderSections.Count - 1 do
  begin
    ACalcRect := GetCellTextRect(AIndex, I, 0, Result, AVScrollBarVisible);
    Result := Max(Result, cxRectHeight(ACalcRect));
  end;
end;

function TcxMCListBox.DoCreateInnerListBox: TcxCustomInnerListBox;
begin
  Result := TcxMCInnerListBox.Create(FInnerListBoxContainer)
end;

function TcxMCListBox.GetExtendedSelect: Boolean;
begin
  Result := InnerListBox.ExtendedSelect;
end;

function TcxMCListBox.GetItems: TStrings;
begin
  Result := InnerListBox.Items;
end;

function TcxMCListBox.GetMultiSelect: Boolean;
begin
  Result := InnerListBox.MultiSelect;
end;

function TcxMCListBox.GetSelCount: Integer;
begin
  Result := InnerListBox.SelCount;
end;

function TcxMCListBox.GetSorted: Boolean;
begin
  Result := InnerListBox.Sorted;
end;

procedure TcxMCListBox.SetExtendedSelect(Value: Boolean);
begin
  InnerListBox.ExtendedSelect := Value;
end;

procedure TcxMCListBox.SetItemHeight(Value: Integer);
begin
  inherited;
  FullRepaint;
end;

procedure TcxMCListBox.SetItems(Value: TStrings);
begin
  InnerListBox.Items.Assign(Value);
  DataChange;
end;

procedure TcxMCListBox.SetMultiSelect(Value: Boolean);
begin
  InnerListBox.MultiSelect := Value;
end;

procedure TcxMCListBox.SetSorted(Value: Boolean);
begin
  InnerListBox.Sorted := Value;
end;

function TcxMCListBox.GetInnerListBox: TcxMCInnerListBox;
begin
  Result := inherited InnerListBox as TcxMCInnerListBox;
end;

function TcxMCListBox.GetInnerListBoxTop: Integer;
begin
  Result := FInnerHeader.Height;
end;

function TcxMCListBox.GetItemText(AItemIndex: Integer): string;
begin
  Result := Items[AItemIndex];
end;

function TcxMCListBox.IndexOf(const S: string): Integer;
begin
  Result := Items.IndexOf(S);
end;

procedure TcxMCListBox.InitializeInnerListBox;
begin
  InnerListBox.Container := FInnerListBoxContainer;
  InnerListBox.Parent := FInnerListBoxContainer;
  InnerListBox.MCListBox := Self;
  InnerListBox.Style := lbOwnerDrawVariable;
  InnerListBox.OnMeasureItem := MeasureItem;
  InnerListBox.OnDrawItem := DrawItem;
  FSavedHScroll := InnerListBox.HScrollBar.OnScroll;
  InnerListBox.HScrollBar.OnScroll := HScrollHandler;
end;

end.
