{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxDateNavigator;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Classes, Controls, Graphics, Math,
  dxCore, cxContainer, cxGeometry,
  cxControls, cxSchedulerUtils, cxSchedulerStorage, cxSchedulerCustomControls,
  cxSchedulerDateNavigator, cxSchedulerDayView, cxDateUtils, cxLookAndFeelPainters;

type
  TcxCustomDateNavigator = class;

  { TcxInnerDateNavigator }

  TcxInnerDateNavigator = class(TcxCustomScheduler, IUnknown, IcxContainerInnerControl)
  private
    FPrevDate: TDateTime;
    FPrevCopyDragDrop: Boolean;
    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    FSelAnchor, FSelStart: Integer;
    FContainer: TcxCustomDateNavigator;
    procedure CalcLayout; override;
    function CanDrag(X, Y: Integer): Boolean; override;
    function CanSelectPeriod: Boolean; override;
    procedure CheckSplittersVisibilityChanging; override;
    procedure Click; override;
    function CreateDefaultView: TcxSchedulerCustomView; override;
    function CreateDateNavigator: TcxSchedulerCustomDateNavigator; override;
    procedure DateNavigatorSelectionChanged; override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetOnShowDateHint: TcxSchedulerShowDateHintEvent; override;
    function HitAtDate(X, Y: Integer): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetSelection(ADelta: Integer);
    procedure SetSelectionDays(ADate: Integer; ACheckEnd: Boolean);
    property Container: TcxCustomDateNavigator read FContainer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CanFocus: Boolean; override;
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  { TcxCustomDateNavigator }

  TcxCustomDateNavigator = class(TcxContainer, IUnknown, IcxSchedulerStorageListener,
                                 IcxExternalDateNavigatorListener)
  private
    FInnerDateNavigator: TcxInnerDateNavigator;
    FIsExitProcessing: Boolean;
    FLockUpdate: Integer;
    FScheduler: TcxCustomScheduler;
    FOnShowDateHint: TcxSchedulerShowDateHintEvent;
    FStorage: TcxCustomSchedulerStorage;
    FUnlimitedSelection: Boolean;
    function GetActualStartOfWeek: TDay;
    function GetBorderStyle: TcxControlBorderStyle;
    function GetColCount: Integer;
    function GetCustomDrawBackground: TcxSchedulerDateNavigatorCustomDrawBackgroundEvent;
    function GetCustomDrawDayCaption: TcxSchedulerDateNavigatorCustomDrawDayCaptionEvent;
    function GetCustomDrawDayNumber: TcxSchedulerDateNavigatorCustomDrawDayNumberEvent;
    function GetCustomDrawContent: TcxSchedulerDateNavigatorCustomDrawContentEvent;
    function GetCustomDrawHeader: TcxSchedulerDateNavigatorCustomDrawHeaderEvent;
    function GetDate: TDateTime;
    function GetDateNavigator: TcxSchedulerDateNavigator;
    function GetEventDays: TcxSchedulerDateList;
    function GetFirstDate: TDateTime;
    function GetFirstWeekOfYear: TcxFirstWeekOfYear;
    function GetFont: TFont;
    function GetHitTest: TcxSchedulerDateNavigatorHitTest;
    function GetHolidayColor: TColor;
    function GetLastDate: TDateTime;
    function GetPeriodChanged: TcxSchedulerPeriodChangedEvent;
    function GetRealFirstDate: TDateTime;
    function GetRealLastDate: TDateTime;
    function GetRowCount: Integer;
    function GetSelectedDays: TcxSchedulerDateList;
    function GetSelectionChanged: TcxSchedulerPeriodChangedEvent;
    function GetSelectionIsWeeks: Boolean;
    function GetSelectPeriod: Boolean;
    function GetShowDatesContainingEventsInBold: Boolean;
    function GetShowDatesContainingHolidaysInColor: Boolean;
    function GetShowWeekNumbers: Boolean;
    function GetStartOfWeek: TcxStartOfWeek;
    function GetStyles: TcxSchedulerDateNavigatorStyles;
    procedure SetBorderStyle(AValue: TcxControlBorderStyle);
    procedure SetColCount(AValue: Integer);
    procedure SetCustomDrawBackground(AValue: TcxSchedulerDateNavigatorCustomDrawBackgroundEvent);
    procedure SetCustomDrawDayCaption(AValue: TcxSchedulerDateNavigatorCustomDrawDayCaptionEvent);
    procedure SetCustomDrawDayNumber(AValue: TcxSchedulerDateNavigatorCustomDrawDayNumberEvent);
    procedure SetCustomDrawContent(AValue: TcxSchedulerDateNavigatorCustomDrawContentEvent);
    procedure SetCustomDrawHeader(AValue: TcxSchedulerDateNavigatorCustomDrawHeaderEvent);
    procedure SetDate(AValue: TDateTime);
    procedure SetFirstDate(AValue: TDateTime);
    procedure SetFirstWeekOfYear(AValue: TcxFirstWeekOfYear);
    procedure SetFont(AValue: TFont);
    procedure SetHolidayColor(AValue: TColor);
    procedure SetPeriodChanged(AValue: TcxSchedulerPeriodChangedEvent);
    procedure SetRowCount(AValue: Integer);
    procedure SetScheduler(AValue: TcxCustomScheduler);
    procedure SetSelectionChanged(AValue: TcxSchedulerPeriodChangedEvent);
    procedure SetSelectionIsWeeks(AValue: Boolean);
    procedure SetSelectPeriod(AValue: Boolean);
    procedure SetShowDatesContainingEventsInBold(AValue: Boolean);
    procedure SetShowDatesContainingHolidaysInColor(AValue: Boolean);
    procedure SetShowWeekNumbers(AValue: Boolean);
    procedure SetStartOfWeek(AValue: TcxStartOfWeek);
    procedure SetStorage(AValue: TcxCustomSchedulerStorage);
    procedure SetStyles(AValue: TcxSchedulerDateNavigatorStyles);
    procedure SetUnlimitedSelection(AValue: Boolean);

    procedure IcxExternalDateNavigatorListener.StorageChanged = SchedulerStorageChanged;
  protected
    function CanDrag(X, Y: Integer): Boolean; override;
    function CanSelectPeriod: Boolean;
    function CreateInnerDateNavigator: TcxInnerDateNavigator; virtual;
    procedure DateNavigatorSelectionChanged; virtual;
    procedure DoExit; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActualStorage: TcxCustomSchedulerStorage;
    procedure Loaded; override;
    function NeedsScrollBars: Boolean; override;
    procedure UpdateStorage;

    // IcxExternalDateNavigatorListener
    procedure SchedulerStorageChanged;
    procedure SchedulerChanged;
    procedure SchedulerRemoved;

    // IcxSchedulerStorageListener
    procedure StorageChanged(Sender: TObject); virtual;
    procedure StorageRemoved(Sender: TObject); virtual;

    property BorderStyle: TcxControlBorderStyle read GetBorderStyle write SetBorderStyle default cxcbsDefault;
    property ColCount: Integer read GetColCount write SetColCount default 1;
    property DateNavigator: TcxSchedulerDateNavigator read GetDateNavigator;
    property DragMode default dmAutomatic;
    property EventDays: TcxSchedulerDateList read GetEventDays;
    property FirstWeekOfYear: TcxFirstWeekOfYear
      read GetFirstWeekOfYear write SetFirstWeekOfYear default fwySystem;
    property Font: TFont read GetFont write SetFont;
    property HitTest: TcxSchedulerDateNavigatorHitTest read GetHitTest;
    property HolidayColor: TColor read GetHolidayColor write SetHolidayColor default clRed;
    property ParentFont default False;
    property RowCount: Integer read GetRowCount write SetRowCount default 1;
    property Scheduler: TcxCustomScheduler read FScheduler write SetScheduler;
    property SelectedDays: TcxSchedulerDateList read GetSelectedDays;
    property ShowDatesContainingEventsInBold: Boolean
      read GetShowDatesContainingEventsInBold write SetShowDatesContainingEventsInBold default True;
    property ShowDatesContainingHolidaysInColor: Boolean
      read GetShowDatesContainingHolidaysInColor write SetShowDatesContainingHolidaysInColor default False;
    property ShowWeekNumbers: Boolean read GetShowWeekNumbers write SetShowWeekNumbers default True;
    property StartOfWeek: TcxStartOfWeek read GetStartOfWeek write SetStartOfWeek default swSystem;
    property Storage: TcxCustomSchedulerStorage read FStorage write SetStorage;
    property Styles: TcxSchedulerDateNavigatorStyles read GetStyles write SetStyles;

    property OnPeriodChanged: TcxSchedulerPeriodChangedEvent read GetPeriodChanged write SetPeriodChanged;
    property OnSelectionChanged: TcxSchedulerPeriodChangedEvent read GetSelectionChanged write SetSelectionChanged;
    property OnShowDateHint: TcxSchedulerShowDateHintEvent read FOnShowDateHint write FOnShowDateHint;
    property OnCustomDrawBackground: TcxSchedulerDateNavigatorCustomDrawBackgroundEvent
      read GetCustomDrawBackground write SetCustomDrawBackground;
    property OnCustomDrawContent: TcxSchedulerDateNavigatorCustomDrawContentEvent
      read GetCustomDrawContent write SetCustomDrawContent;
    property OnCustomDrawDayNumber: TcxSchedulerDateNavigatorCustomDrawDayNumberEvent
      read GetCustomDrawDayNumber write SetCustomDrawDayNumber;
    property OnCustomDrawDayCaption: TcxSchedulerDateNavigatorCustomDrawDayCaptionEvent
      read GetCustomDrawDayCaption write SetCustomDrawDayCaption;
    property OnCustomDrawHeader: TcxSchedulerDateNavigatorCustomDrawHeaderEvent
      read GetCustomDrawHeader write SetCustomDrawHeader;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function CalcSize(AColCount, ARowCount: Integer): TRect;
    procedure LayoutChanged;
    procedure SetFocus; override;
    property ActualStartOfWeek: TDay read GetActualStartOfWeek;
    property Date: TDateTime read GetDate write SetDate;
    property FirstDate: TDateTime read GetFirstDate write SetFirstDate;
    property InnerDateNavigator: TcxInnerDateNavigator read FInnerDateNavigator;
    property LastDate: TDateTime read GetLastDate;
    property RealFirstDate: TDateTime read GetRealFirstDate;
    property RealLastDate: TDateTime read GetRealLastDate;
    property SelectionIsWeeks: Boolean read GetSelectionIsWeeks write SetSelectionIsWeeks;
    property SelectPeriod: Boolean read GetSelectPeriod write SetSelectPeriod default True;
    property UnlimitedSelection: Boolean read FUnlimitedSelection write SetUnlimitedSelection default False;
  end;

  { TcxDateNavigator }

  TcxDateNavigator = class(TcxCustomDateNavigator)
  public
    property ColCount;
    property EventDays;
    property HitTest;
    property RowCount;
    property SelectedDays;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FirstWeekOfYear;
    property Font;
    property HolidayColor;
    property LookAndFeel;
    property ParentFont;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property Scheduler;
    property SelectPeriod;
    property ShowDatesContainingEventsInBold;
    property ShowDatesContainingHolidaysInColor;
    property ShowWeekNumbers;
    property StartOfWeek;
    property Storage;
    property Styles;
    property TabOrder;
    property TabStop;
    property UnlimitedSelection;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnCustomDrawBackground;
    property OnCustomDrawContent;
    property OnCustomDrawDayCaption;
    property OnCustomDrawDayNumber;
    property OnCustomDrawHeader;
    property OnPeriodChanged;
    property OnSelectionChanged;
    property OnShowDateHint;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  SysUtils, cxClasses, cxEdit, cxScrollBar, DateUtils;

type
  TcxDateNavigatorAccess = class(TcxSchedulerDateNavigator);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);

function DateCompare(AItem1, AItem2: Pointer): Integer;
begin
  Result := TdxNativeInt(AItem2) - TdxNativeInt(AItem1);
end;

{ TcxInnerDateNavigator }

constructor TcxInnerDateNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelAnchor := NullDate;
  CurrentView.CanShow := False;
  ControlBox.Visible := False;
  BorderStyle := cxcbsDefault;
  ControlStyle := ControlStyle + [csDoubleClicks];
  ParentColor := False;
  OptionsView.ShowEventsWithoutResource := True;
  OptionsView.GroupingKind := gkNone;
  EventOperations.ReadOnly := True;
end;

procedure TcxInnerDateNavigator.DragDrop(Source: TObject; X, Y: Integer);
begin
  try
    if Container <> nil then
    begin
      Container.DragDrop(Source, Left + X, Top + Y);
      if Container.DragMode <> dmAutomatic then Exit;
    end;
    if (Source is TcxSchedulerDragObject) and HitAtDate(X, Y) then
      TcxSchedulerDragObject(Source).DropToDateNavigator(DateNavigator);
  finally
    TcxDateNavigatorAccess(DateNavigator).Controller.DragDate := NullDate;
  end;
end;

function TcxInnerDateNavigator.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action);
end;

function TcxInnerDateNavigator.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action);
end;

function TcxInnerDateNavigator.CanFocus: Boolean;
begin
  Result := Container.CanFocus;
end;

procedure TcxInnerDateNavigator.CalcLayout;
begin
  DateNavigator.Bounds := ClientBounds;
end;

function TcxInnerDateNavigator.CanDrag(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TcxInnerDateNavigator.CanSelectPeriod: Boolean;
begin
  if Container <> nil then
    Result := Container.CanSelectPeriod
  else
    Result := True;
end;

procedure TcxInnerDateNavigator.CheckSplittersVisibilityChanging;
begin
end;

procedure TcxInnerDateNavigator.Click;
begin
  inherited Click;
  if Container <> nil then
    Container.Click;
end;

function TcxInnerDateNavigator.CreateDefaultView: TcxSchedulerCustomView;
begin
  Result := TcxSchedulerDayView.Create(Self);
end;

function TcxInnerDateNavigator.CreateDateNavigator: TcxSchedulerCustomDateNavigator;
begin
  Result := TcxSchedulerDateNavigator.Create(Self);
end;

procedure TcxInnerDateNavigator.DateNavigatorSelectionChanged;
begin
  if Container <> nil then
  begin
    Container.DateNavigatorSelectionChanged;
    if Container.Scheduler <> nil then
      inherited DateNavigatorSelectionChanged;
  end;
end;

procedure TcxInnerDateNavigator.DblClick;
begin
  inherited DblClick;
  if Container <> nil then
    Container.DblClick;
end;

function TcxInnerDateNavigator.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (Container <> nil) and Container.DoMouseWheel(Shift,
    WheelDelta, MousePos);
  if not Result then
    inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TcxInnerDateNavigator.DoStartDrag(var DragObject: TDragObject);
begin
  FPrevDate := NullDate;
  FPrevCopyDragDrop := IsCopyDragDrop;
  inherited;
end;

procedure TcxInnerDateNavigator.DragCanceled;
begin
  TcxDateNavigatorAccess(DateNavigator).Controller.DragDate := NullDate;
  Invalidate;
end;

procedure TcxInnerDateNavigator.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  AAccept, ACopyDragDrop: Boolean;
  ADate: TDateTime;
begin
  AAccept := HitAtDate(X, Y);
  if AAccept and (Source is TcxSchedulerDragObject) then
  begin
    ADate := TcxSchedulerDateNavigator(DateNavigator).HitTest.Time;
    ACopyDragDrop := IsCopyDragDrop;
    if (ADate <> FPrevDate) or (ACopyDragDrop <> FPrevCopyDragDrop) then
    begin
      TcxSchedulerDragObject(Source).CalculateConflictsForDateNavigator(DateNavigator);
      FPrevDate := ADate;
      FPrevCopyDragDrop := ACopyDragDrop;
    end;
    AAccept := not TcxSchedulerDragObject(Source).HasConflicts;
  end;
  if Container <> nil then
    Container.DragOver(Source, Left + X, Top + Y, State, Accept);
  if Container.DragMode = dmAutomatic then
  begin
    if not Accept and AAccept and Assigned(Container.OnDragOver) then
      AAccept := False;
    Accept := AAccept or Accept;
  end;
  UpdateDateNavigatorDragging(State <> dsDragLeave);
end;

function TcxInnerDateNavigator.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := GetAsyncKeyState(VK_MENU) < 0;
end;

function TcxInnerDateNavigator.GetOnShowDateHint: TcxSchedulerShowDateHintEvent;
begin
  Result := nil;
  if FContainer <> nil then
  begin
    if Assigned(FContainer.FOnShowDateHint) then
      Result := FContainer.FOnShowDateHint
    else
      if FContainer.Scheduler <> nil then
        Result := TcxCustomSchedulerAccess(FContainer.Scheduler).OnShowDateHint;
  end;
end;

function TcxInnerDateNavigator.HitAtDate(X, Y: Integer): Boolean;
begin
  with TcxSchedulerDateNavigator(DateNavigator).HitTest do
  begin
    HitPoint := Point(X, Y);
    Result := HitAtTime;
  end;
end;

procedure TcxInnerDateNavigator.KeyDown(var Key: Word; Shift: TShiftState);
const
  AShift: array[Boolean] of Integer = (1, -1);
var
  AKey: Word;
begin
  if Container <> nil then
    Container.KeyDown(Key, Shift);
  if Key <> 0 then
    inherited KeyDown(Key, Shift);
  if (ssShift in Shift) and (Key in [VK_LEFT..VK_DOWN]) and (FSelAnchor = NullDate) then
  begin
    FSelStart := Trunc(SelectedDays[0]);
    FSelAnchor := FSelStart;
  end;
  case Key of
    VK_LEFT, VK_RIGHT:
    begin
      AKey := Key;
      if UseRightToLeftAlignment then
        AKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(AKey);
      if (ssShift in Shift) and TcxDateNavigatorAccess(DateNavigator).SelectPeriod then
        SetSelectionDays(FSelAnchor - AShift[AKey = VK_RIGHT], AKey = VK_RIGHT)
      else
        SetSelection(AShift[AKey = VK_RIGHT]);
      Invalidate;
    end;
    VK_UP, VK_DOWN:
    begin
      if (ssShift in Shift) and TcxDateNavigatorAccess(DateNavigator).SelectPeriod then
        SetSelectionDays(FSelAnchor - AShift[Key = VK_DOWN] * 7, Key = VK_DOWN)
      else
        SetSelection(AShift[Key = VK_DOWN] * 7);
      Invalidate;
    end;
  end;
  Container.LayoutChanged;
end;

procedure TcxInnerDateNavigator.KeyPress(var Key: Char);
begin
  if Key = Char(VK_TAB) then
    Key := #0;
  if Container <> nil then
    Container.KeyPress(Key);
  if Word(Key) = VK_RETURN then
    Key := #0;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TcxInnerDateNavigator.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_TAB then
    Key := 0;
  if Container <> nil then
    Container.KeyUp(Key, Shift);
  if Key <> 0 then
    inherited KeyUp(Key, Shift);
end;

procedure TcxInnerDateNavigator.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelAnchor := NullDate;
  inherited MouseDown(Button, Shift, X, Y);
  if Container <> nil then
    with Container do
    begin
      InnerControlMouseDown := True;
      try
        MouseDown(Button, Shift, X + Self.Left, Y + Self.Top);
      finally
        InnerControlMouseDown := False;
      end;
    end;
end;

procedure TcxInnerDateNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if Container <> nil then
    Container.MouseMove(Shift, X + Left, Y + Top);
end;

procedure TcxInnerDateNavigator.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Container <> nil then
    Container.MouseUp(Button, Shift, X + Left, Y + Top);
end;

procedure TcxInnerDateNavigator.SetSelection(ADelta: Integer);
begin
  if FSelAnchor <> NullDate then
  begin
    SelectedDays.Clear;
    SelectedDays.Add(FSelAnchor - ADelta);
    FSelAnchor := NullDate;
    TcxDateNavigatorAccess(DateNavigator).UpdateSelection;
  end
  else
    TcxDateNavigatorAccess(DateNavigator).ScrollSelection(ADelta)
end;

procedure TcxInnerDateNavigator.SetSelectionDays(
  ADate: Integer; ACheckEnd: Boolean);

  function WeekOf(ADate: TDateTime): Word;
  var
    AYear, ADay: Word;
  begin
    DecodeDateWeek(ADate, AYear, Result, ADay);
  end;

var
  I: Integer;
begin
  if not Container.UnlimitedSelection then
  begin
    if (TcxDateNavigatorAccess(DateNavigator).SelectionIsWeeks and
      (WeekOf(Max(FSelStart, ADate)) - WeekOf(Min(FSelStart, ADate)) > cxMaxWeekCount)) or
      (not TcxDateNavigatorAccess(DateNavigator).SelectionIsWeeks and
      (Max(FSelStart, ADate) - Min(FSelStart, ADate) > 14)) then Exit;
  end;
  FSelAnchor := ADate;
  SelectedDays.Clear;
  for I := Min(FSelStart, FSelAnchor) to Max(FSelStart, FSelAnchor) do
    SelectedDays.Add(I);
  TcxDateNavigatorAccess(DateNavigator).UpdateSelection;
  if (SelectedDays[SelectedDays.Count - 1] = ADate) then
    while (ADate > TcxDateNavigatorAccess(DateNavigator).LastDate) do
       TcxDateNavigatorAccess(DateNavigator).FirstDate := TcxDateNavigatorAccess(DateNavigator).FirstDate + 31;
  TcxDateNavigatorAccess(DateNavigator).DoSelectionChangedEvent;
end;

procedure TcxInnerDateNavigator.WndProc(var Message: TMessage);
begin
  if (Container <> nil) and Container.InnerControlMenuHandler(Message) then
    Exit;

 { if Container <> nil then
    if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
      (Container.DragMode = dmAutomatic) and not Container.IsDesigning then
    begin
      Container.BeginAutoDrag;
      Exit;
    end;}

  inherited WndProc(Message);
end;

function TcxInnerDateNavigator.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxInnerDateNavigator.GetControlContainer: TcxContainer;
begin
  Result := FContainer;
end;

procedure TcxInnerDateNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if Container <> nil then
    with Message do
    begin
      Result := Result or DLGC_WANTCHARS;
      if GetKeyState(VK_CONTROL) >= 0 then
        Result := Result or DLGC_WANTTAB;
    end;
end;

procedure TcxInnerDateNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (Container <> nil) and not Container.IsDestroying then
    Container.FocusChanged;
end;

procedure TcxInnerDateNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if (Container <> nil) and not Container.IsDestroying and not(csDestroying in ComponentState)
      and (Message.FocusedWnd <> Container.Handle) then
    Container.FocusChanged;
end;

{ TcxCustomDateNavigator }

constructor TcxCustomDateNavigator.Create(AOwner: TComponent);
var
  ABounds: TRect;
begin
  inherited Create(AOwner);
  DragMode := dmAutomatic;
  ParentFont := False;
  LookAndFeel.MasterLookAndFeel := nil;
  FInnerDateNavigator := CreateInnerDateNavigator;
  with FInnerDateNavigator do
  begin
    FContainer := Self;
    LookAndFeel.MasterLookAndFeel := Self.LookAndFeel;
    Parent := Self;
  end;
  InnerControl := FInnerDateNavigator;
  ABounds := CalcSize(1, 1);
  Width := cxRectWidth(ABounds);
  Height := cxRectHeight(ABounds);
  Style.Font := FInnerDateNavigator.Font;
  Style.BorderStyle := cbsNone;
  Style.HotTrack := False;
  Style.TransparentBorder := False;
end;

destructor TcxCustomDateNavigator.Destroy;
begin
  Storage := nil;
  Scheduler := nil;
  FreeAndNil(FInnerDateNavigator);
  inherited Destroy;
end;

function TcxCustomDateNavigator.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action);
end;

function TcxCustomDateNavigator.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action);
end;

function TcxCustomDateNavigator.CalcSize(AColCount, ARowCount: Integer): TRect;
begin
  with TcxDateNavigatorAccess(DateNavigator) do
  begin
    DoLayoutChanged;
    Result := Rect(0, 0, GetMonthSize.CX * AColCount, GetMonthSize.CY * ARowCount);
  end;
  with GetBorderExtent do
  begin
    Inc(Result.Right, Left + Right);
    Inc(Result.Bottom, Bottom + Top);
  end;
end;

procedure TcxCustomDateNavigator.LayoutChanged;
begin
  FInnerDateNavigator.LayoutChanged;
end;

procedure TcxCustomDateNavigator.SetFocus;
begin
  if not IsDesigning then
    inherited SetFocus;
end;

function TcxCustomDateNavigator.CanDrag(X, Y: Integer): Boolean;
begin
  Result := False;
end;

function TcxCustomDateNavigator.CanSelectPeriod: Boolean;
begin
  if Scheduler <> nil then
    Result := TcxDateNavigatorAccess(FScheduler.DateNavigator).CanMultiSelect
  else
    Result := True;
end;

function TcxCustomDateNavigator.CreateInnerDateNavigator: TcxInnerDateNavigator;
begin
  Result := TcxInnerDateNavigator.Create(Self);
end;

procedure TcxCustomDateNavigator.DateNavigatorSelectionChanged;
begin
  if Assigned(FScheduler) and (FLockUpdate = 0) then
  begin
    Inc(FLockUpdate);
    try
      TcxSchedulerDateNavigator(FScheduler.DateNavigator).AssignProperties(
        TcxSchedulerDateNavigator(FInnerDateNavigator.DateNavigator));
    finally
      TcxDateNavigatorAccess(FScheduler.DateNavigator).MakeSelectionVisible;
      Dec(FLockUpdate);
    end;
  end
  else
    FInnerDateNavigator.FullRefresh;
end;

procedure TcxCustomDateNavigator.DoExit;
begin
  if IsDestroying or FIsExitProcessing then
    Exit;
  FIsExitProcessing := True;
  try
    inherited DoExit;
  finally
    FIsExitProcessing := False;
  end;
end;

procedure TcxCustomDateNavigator.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  with FInnerDateNavigator do DefineProperties(Filer);
end;

function TcxCustomDateNavigator.GetActualStorage: TcxCustomSchedulerStorage;
begin
  Result := FStorage;
  if Scheduler <> nil then
    Result := Scheduler.Storage;
end;

procedure TcxCustomDateNavigator.Loaded;
begin
  inherited Loaded;
  FInnerDateNavigator.Loaded;
end;

function TcxCustomDateNavigator.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TcxCustomDateNavigator.UpdateStorage;
begin
  FInnerDateNavigator.Storage := GetActualStorage;
  UnlimitedSelection := UnlimitedSelection;
end;

procedure TcxCustomDateNavigator.SchedulerStorageChanged;
begin
  UpdateStorage;
end;

procedure TcxCustomDateNavigator.SchedulerChanged;
begin
  if FLockUpdate <> 0 then Exit;
  Inc(FLockUpdate);
  try
    TcxSchedulerDateNavigator(InnerDateNavigator.DateNavigator
      ).AssignProperties(TcxSchedulerDateNavigator(FScheduler.DateNavigator));
    InnerDateNavigator.FullRefresh;
  finally
    TcxDateNavigatorAccess(InnerDateNavigator.DateNavigator).MakeSelectionVisible;
    Dec(FLockUpdate);
    Update;
  end;
end;

procedure TcxCustomDateNavigator.SchedulerRemoved;
begin
  FScheduler := nil;
  UpdateStorage;
end;

procedure TcxCustomDateNavigator.StorageChanged(Sender: TObject);
begin
end;

procedure TcxCustomDateNavigator.StorageRemoved(Sender: TObject);
begin
  FStorage := nil;
end;

function TcxCustomDateNavigator.GetActualStartOfWeek: TDay;
begin
  Result := FInnerDateNavigator.OptionsView.ActualStartOfWeek;
end;

function TcxCustomDateNavigator.GetBorderStyle: TcxControlBorderStyle;
begin
  Result := FInnerDateNavigator.BorderStyle;
end;

function TcxCustomDateNavigator.GetColCount: Integer;
begin
  Result := DateNavigator.ColCount;
end;

function TcxCustomDateNavigator.GetCustomDrawBackground: TcxSchedulerDateNavigatorCustomDrawBackgroundEvent;
begin
  Result := DateNavigator.OnCustomDrawBackground;
end;

function TcxCustomDateNavigator.GetCustomDrawDayCaption: TcxSchedulerDateNavigatorCustomDrawDayCaptionEvent;
begin
  Result := DateNavigator.OnCustomDrawDayCaption;
end;

function TcxCustomDateNavigator.GetCustomDrawDayNumber: TcxSchedulerDateNavigatorCustomDrawDayNumberEvent;
begin
  Result := DateNavigator.OnCustomDrawDayNumber;
end;

function TcxCustomDateNavigator.GetCustomDrawContent: TcxSchedulerDateNavigatorCustomDrawContentEvent;
begin
  Result := DateNavigator.OnCustomDrawContent;
end;

function TcxCustomDateNavigator.GetCustomDrawHeader: TcxSchedulerDateNavigatorCustomDrawHeaderEvent;
begin
  Result := DateNavigator.OnCustomDrawHeader;
end;

function TcxCustomDateNavigator.GetDate: TDateTime;
begin
  Result := DateNavigator.Date;
end;

function TcxCustomDateNavigator.GetDateNavigator: TcxSchedulerDateNavigator;
begin
  Result := TcxSchedulerDateNavigator(FInnerDateNavigator.DateNavigator);
end;

function TcxCustomDateNavigator.GetEventDays: TcxSchedulerDateList;
begin
  Result := FInnerDateNavigator.EventDays;
end;

function TcxCustomDateNavigator.GetFirstDate: TDateTime;
begin
  Result := DateNavigator.FirstDate;
end;

function TcxCustomDateNavigator.GetFirstWeekOfYear: TcxFirstWeekOfYear;
begin
  Result := DateNavigator.FirstWeekOfYear;
end;

function TcxCustomDateNavigator.GetFont: TFont;
begin
  Result := Style.Font;
end;

function TcxCustomDateNavigator.GetHitTest: TcxSchedulerDateNavigatorHitTest;
begin
  Result := DateNavigator.HitTest;
end;

function TcxCustomDateNavigator.GetHolidayColor: TColor;
begin
  Result := DateNavigator.HolidayColor;
end;

function TcxCustomDateNavigator.GetLastDate: TDateTime;
begin
  Result := DateNavigator.LastDate;
end;

function TcxCustomDateNavigator.GetPeriodChanged: TcxSchedulerPeriodChangedEvent;
begin
  Result := DateNavigator.OnPeriodChanged;
end;

function TcxCustomDateNavigator.GetRealFirstDate: TDateTime;
begin
  Result := DateNavigator.RealFirstDate;
end;

function TcxCustomDateNavigator.GetRealLastDate: TDateTime;
begin
  Result := DateNavigator.RealLastDate;
end;

function TcxCustomDateNavigator.GetRowCount: Integer;
begin
  Result := DateNavigator.RowCount;
end;

function TcxCustomDateNavigator.GetSelectionChanged: TcxSchedulerPeriodChangedEvent;
begin
  Result := DateNavigator.OnSelectionChanged;
end;

function TcxCustomDateNavigator.GetSelectionIsWeeks: Boolean;
begin
  Result := DateNavigator.SelectionIsWeeks;
end;

function TcxCustomDateNavigator.GetSelectPeriod: Boolean;
begin
  Result := DateNavigator.SelectPeriod;
end;

function TcxCustomDateNavigator.GetShowDatesContainingEventsInBold: Boolean;
begin
  Result := DateNavigator.ShowDatesContainingEventsInBold;
end;

function TcxCustomDateNavigator.GetShowDatesContainingHolidaysInColor: Boolean;
begin
  Result := DateNavigator.ShowDatesContainingHolidaysInColor;
end;

function TcxCustomDateNavigator.GetSelectedDays: TcxSchedulerDateList;
begin
  Result := FInnerDateNavigator.SelectedDays;
end;

function TcxCustomDateNavigator.GetShowWeekNumbers: Boolean;
begin
  Result := DateNavigator.ShowWeekNumbers;
end;

function TcxCustomDateNavigator.GetStartOfWeek: TcxStartOfWeek;
begin
  Result := FInnerDateNavigator.OptionsView.StartOfWeek;
end;

function TcxCustomDateNavigator.GetStyles: TcxSchedulerDateNavigatorStyles;
begin
  Result := DateNavigator.Styles;
end;

procedure TcxCustomDateNavigator.SetBorderStyle(AValue: TcxControlBorderStyle);
begin
  FInnerDateNavigator.BorderStyle := AValue;
end;

procedure TcxCustomDateNavigator.SetColCount(AValue: Integer);
begin
  DateNavigator.ColCount := AValue;
end;

procedure TcxCustomDateNavigator.SetCustomDrawBackground(
  AValue: TcxSchedulerDateNavigatorCustomDrawBackgroundEvent);
begin
  DateNavigator.OnCustomDrawBackground := AValue;
end;

procedure TcxCustomDateNavigator.SetCustomDrawDayCaption(
  AValue: TcxSchedulerDateNavigatorCustomDrawDayCaptionEvent);
begin
  DateNavigator.OnCustomDrawDayCaption := AValue;
end;

procedure TcxCustomDateNavigator.SetCustomDrawDayNumber(
  AValue: TcxSchedulerDateNavigatorCustomDrawDayNumberEvent);
begin
  DateNavigator.OnCustomDrawDayNumber := AValue;
end;

procedure TcxCustomDateNavigator.SetCustomDrawContent(
  AValue: TcxSchedulerDateNavigatorCustomDrawContentEvent);
begin
  DateNavigator.OnCustomDrawContent := AValue;
end;

procedure TcxCustomDateNavigator.SetCustomDrawHeader(
  AValue: TcxSchedulerDateNavigatorCustomDrawHeaderEvent);
begin
  DateNavigator.OnCustomDrawHeader := AValue;
end;

procedure TcxCustomDateNavigator.SetDate(AValue: TDateTime);
begin
  DateNavigator.Date := AValue;
  DateNavigatorSelectionChanged;
end;

procedure TcxCustomDateNavigator.SetFirstDate(AValue: TDateTime);
begin
  DateNavigator.FirstDate := AValue;
end;

procedure TcxCustomDateNavigator.SetFirstWeekOfYear(AValue: TcxFirstWeekOfYear);
begin
  DateNavigator.FirstWeekOfYear := AValue;
end;

procedure TcxCustomDateNavigator.SetFont(AValue: TFont);
begin
  Style.Font.Assign(AValue);
end;

procedure TcxCustomDateNavigator.SetHolidayColor(AValue: TColor);
begin
  DateNavigator.HolidayColor := AValue;
end;

procedure TcxCustomDateNavigator.SetPeriodChanged(
  AValue: TcxSchedulerPeriodChangedEvent);
begin
  DateNavigator.OnPeriodChanged := AValue;
end;

procedure TcxCustomDateNavigator.SetRowCount(AValue: Integer);
begin
  DateNavigator.RowCount := AValue;
end;

procedure TcxCustomDateNavigator.SetScheduler(
  AValue: TcxCustomScheduler);
begin
  if AValue = FScheduler then Exit;
  if FScheduler <> nil then
    TcxInnerDateNavigator(FScheduler).RemoveListener(Self);
  FScheduler := AValue;
  if FScheduler <> nil then
  begin
    TcxInnerDateNavigator(FScheduler).AddListener(Self);
    LookAndFeel.MasterLookAndFeel := TcxInnerDateNavigator(FScheduler).LookAndFeel;
    SchedulerChanged;
  end
  else
    LookAndFeel.MasterLookAndFeel := nil;
  UpdateStorage;
//  DateNavigatorSelectionChanged;
end;

procedure TcxCustomDateNavigator.SetSelectionChanged(
  AValue: TcxSchedulerPeriodChangedEvent);
begin
  DateNavigator.OnSelectionChanged := AValue;
end;

procedure TcxCustomDateNavigator.SetSelectionIsWeeks(AValue: Boolean);
begin
  DateNavigator.SelectionIsWeeks := AValue;
end;

procedure TcxCustomDateNavigator.SetSelectPeriod(AValue: Boolean);
begin
  DateNavigator.SelectPeriod := AValue;
end;

procedure TcxCustomDateNavigator.SetShowDatesContainingEventsInBold(
  AValue: Boolean);
begin
  DateNavigator.ShowDatesContainingEventsInBold := AValue;
end;

procedure TcxCustomDateNavigator.SetShowDatesContainingHolidaysInColor(AValue: Boolean);
begin
  DateNavigator.ShowDatesContainingHolidaysInColor := AValue;
end;

procedure TcxCustomDateNavigator.SetShowWeekNumbers(AValue: Boolean);
begin
  DateNavigator.ShowWeekNumbers := AValue;
end;

procedure TcxCustomDateNavigator.SetStartOfWeek(
  AValue: TcxStartOfWeek);
begin
  FInnerDateNavigator.OptionsView.StartOfWeek := AValue;
end;

procedure TcxCustomDateNavigator.SetStorage(
  AValue: TcxCustomSchedulerStorage);
begin
  if FStorage <> AValue then
  begin
    if FStorage <> nil then
      FStorage.RemoveListener(Self);
    FStorage := AValue;
    if FStorage <> nil then
      FStorage.AddListener(Self);
  end;
  UpdateStorage;
end;

procedure TcxCustomDateNavigator.SetStyles(
  AValue: TcxSchedulerDateNavigatorStyles);
begin
  DateNavigator.Styles := AValue;
end;

procedure TcxCustomDateNavigator.SetUnlimitedSelection(AValue: Boolean);
begin
  FUnlimitedSelection := AValue;
  TcxDateNavigatorAccess(DateNavigator).UnlimitedSelection := AValue and (Scheduler = nil);
end;

end.
