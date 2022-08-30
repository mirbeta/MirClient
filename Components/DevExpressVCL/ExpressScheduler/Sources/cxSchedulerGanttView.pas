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

unit cxSchedulerGanttView;

{$I cxVer.inc}

interface

uses
  Types, Forms, Windows, SysUtils, StdCtrls, Classes, Math, Controls, Graphics,
  cxScrollBar, cxLookAndFeelPainters, cxSchedulerCustomControls, cxSchedulerStorage,
  cxSchedulerCustomResourceView, cxSchedulerUtils, cxSchedulerStrs, cxDateUtils,
  cxClasses, cxDrawTextUtils, cxSchedulerTimeGridView, cxGraphics, cxGeometry,
  cxControls, dxCore;

const
  // HitTest constants
  htcLink = $15;
  htcExpandButton = $17;

type
  TcxSchedulerGanttView = class;
  TcxSchedulerGanttViewController = class;
  TcxSchedulerGanttViewViewInfo = class;

  TcxSchedulerGanttViewEventStyle = (esDefault, esProgress);

  { TcxSchedulerGanttViewEventLayoutBuilder }

  TcxSchedulerGanttViewEventLayoutBuilder = class(TcxSchedulerEventLayoutBuilder)
  public
    procedure Calculate(const ASchedulerViewStyle: TcxSchedulerViewStyle = svsClassic); override;
  end;

  { TcxSchedulerGanttViewLinkViewInfo }

  TcxSchedulerGanttViewLinkLines = TRects;

  TcxSchedulerGanttViewLinkViewInfo = class(TcxSchedulerCustomViewInfoItem)
  private
    FArrowDirection: TcxArrowDirection;
    FArrowRect: TRect;
    FEventItemLink: TcxSchedulerEventItemLink;
    FFinishEventViewInfo: TcxSchedulerEventCellViewInfo;
    FHint: string;
    FLines: TcxSchedulerGanttViewLinkLines;
    FStartEventViewInfo: TcxSchedulerEventCellViewInfo;
    FViewInfo: TcxSchedulerGanttViewViewInfo;
    function GetArrowDirection(const AArrowStart, AArrowFinish: TPoint): TcxArrowDirection;
    function GetIsHot: Boolean;
    function GetRelation: TcxSchedulerEventRelation;
    procedure InitializeFinishParams(var AStartRect, AFinishRect: TRect; out P1, P2: TPoint);
    procedure InitializeStartParams(var AStartRect: TRect; out P1, P2: TPoint);
  protected
    function GetHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest; var ABreak: Boolean): Boolean; override;
    procedure AfterCustomDraw(ACanvas: TcxCanvas); override;
    procedure BeforeCustomDraw(ACanvas: TcxCanvas); override;
    procedure Calculate(AStartRect: TRect; AFinishRect: TRect); virtual;
    procedure CalculateArrowRect(const AArrowStart, AArrowFinish: TPoint); virtual;
    procedure CalculateBounds; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure SetClipRegion(ACanvas: TcxCanvas);

    property FinishEventViewInfo: TcxSchedulerEventCellViewInfo read FFinishEventViewInfo;
    property StartEventViewInfo: TcxSchedulerEventCellViewInfo read FStartEventViewInfo;
  public
    procedure Initialize(AViewInfo: TcxSchedulerGanttViewViewInfo;
      AStartEventViewInfo, AFinishEventViewInfo: TcxSchedulerEventCellViewInfo;
      AEventItemLink: TcxSchedulerEventItemLink); virtual;
    procedure Draw(ACanvas: TcxCanvas); override;

    property ArrowDirection: TcxArrowDirection read FArrowDirection;
    property ArrowRect: TRect read FArrowRect;
    property EventItemLink: TcxSchedulerEventItemLink read FEventItemLink;
    property Hint: string read FHint;
    property IsHot: Boolean read GetIsHot;
    property Lines: TcxSchedulerGanttViewLinkLines read FLines;
    property Relation: TcxSchedulerEventRelation read GetRelation;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read FViewInfo;
  end;

  { TcxSchedulerGanttViewExpandButtonViewInfo }

  TcxSchedulerGanttViewExpandButtonViewInfo = class(TcxSchedulerCustomViewInfoItem)
  private
    FEvent: TcxSchedulerControlEvent;
    FExpanded: Boolean;
  protected
    procedure DoClick;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
    property Event: TcxSchedulerControlEvent read FEvent;
    property Expanded: Boolean read FExpanded;
  end;

  { TcxSchedulerGanttViewTotalProgressViewInfo }

  TcxSchedulerGanttViewTotalProgressViewInfo = class
  private
    FContentArea: TRect;
    FDrawAsPolyline: Boolean;
    FLineColor: TColor;
    FPoints: array of TPoint;
    FProgressNodes: array of TPoint;
    FTimeLineRect: TRect;
    FViewInfo: TcxSchedulerGanttViewViewInfo;
    function FindUncompletedEventBeforeLine(const ALine: TRect;
      AResource: TcxSchedulerStorageResourceItem; var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
    function GetTimeLineApproximateRect: TRect;
    function GetTimeLineRect: TRect;
    function GetUncompletedEventPoint(const ALine: TRect; var P: TPoint;
      AResource: TcxSchedulerStorageResourceItem): Boolean;
  protected
    function GetScaleFactor: TdxScaleFactor;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AViewInfo: TcxSchedulerGanttViewViewInfo); virtual;
    procedure Calculate; virtual;
    procedure CalculatePolyline;
    procedure Draw(ACanvas: TcxCanvas);
    procedure DrawProgressNodes(ACanvas: TcxCanvas);
    property ContentArea: TRect read FContentArea;
    property DrawAsPolyline: Boolean read FDrawAsPolyline;
    property LineColor: TColor read FLineColor;
    property TimeLineRect: TRect read FTimeLineRect;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read FViewInfo;
  end;

  { TcxSchedulerGanttViewViewInfo }

  TcxSchedulerGanttViewViewInfo = class(TcxSchedulerTimeGridViewViewInfo)
  private
    FExpandButtons: TcxSchedulerViewInfoCellList;
    FHotLink: TcxSchedulerGanttViewLinkViewInfo;
    FLinks: TcxSchedulerViewInfoCellList;
    FTotalProgressViewInfo: TcxSchedulerGanttViewTotalProgressViewInfo;
    FVisibleEventEnumerationList: TcxObjectList;
    FVisibleEventWithoutResourceCount: Integer;
    function GetGanttView: TcxSchedulerGanttView;
    function GetIsHotTrackEnabled: Boolean;
    function GetLinkViewParams: TcxViewParams;
    function GetVisibleEventEnumeration(AIndex: Integer): TcxSchedulerResourceViewInfo;
    procedure SetHotLink(AHotLink: TcxSchedulerGanttViewLinkViewInfo);
  protected
    procedure AddEventForCalculation(ABuilder: TcxSchedulerEventLayoutBuilder;
      AEvent: TcxSchedulerControlEvent; AResource: TcxSchedulerStorageResourceItem; AResourceIndex: Integer); override;
    procedure CalculateExpandButtonViewInfos;
    procedure CheckEventVisibility(ACell: TcxSchedulerEventCellViewInfo; AIndex: Integer); override;
    procedure Clear; override;
    function CreateEventViewData(AEvent: TcxSchedulerControlEvent;
      const ABounds: TRect; const AStart: TDateTime; const AFinish: TDateTime;
      AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData; override;
    function CreateLayoutBuilder: TcxSchedulerEventLayoutBuilder; override;
    function CreateLinkItemViewInfo(AEventViewInfo: TcxSchedulerEventCellViewInfo;
      AEventItemLink: TcxSchedulerEventItemLink;
      out ALinkItem: TcxSchedulerGanttViewLinkViewInfo): Boolean;
    procedure DoCalculate; override;
    function FindEventViewInfoBySource(AEvent: TcxSchedulerEvent;
      const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem;
      var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
    function GetEventBounds(AEvent: TcxSchedulerControlEvent; AResource: TcxSchedulerStorageResourceItem; out AEventRect: TRect): Boolean;
    function GetEventForResourceCount(AResourceIndex: Integer; out AResourceID: Variant): Integer; override;
    function GetEventWithoutResourceCount: Integer; override;
    function GetIndentBetweenLines: Integer; override;
    function GetNeedShowCurrentTime: Boolean; override;
    function HasVisibleEvents: Boolean; override;
    function IsGanttTaskVisible(AEvent: TcxSchedulerEvent): Boolean; virtual;
    function IsGroup(AEvent: TcxSchedulerControlEvent): Boolean; override;
    function IsMilestone(AEvent: TcxSchedulerControlEvent): Boolean; override;
    function IsTaskLinksExpanded(ATaskLinks: TcxSchedulerEventLinks): Boolean;
    function NavigationButtonOffset(AKind: TcxSchedulerContentNavigationButtonKind;
      AResourceIndex: Integer): Integer; override;
    // Links
    procedure CreateLinks; virtual;
    procedure RecreateLinks; virtual;
    property ExpandButtons: TcxSchedulerViewInfoCellList read FExpandButtons;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    function IsEventExpanded(AEvent: TcxSchedulerEvent): Boolean; override;

    property HotLink: TcxSchedulerGanttViewLinkViewInfo read FHotLink write SetHotLink;
    property Links: TcxSchedulerViewInfoCellList read FLinks;
    property LinkViewParams: TcxViewParams read GetLinkViewParams;
    property TotalProgressViewInfo: TcxSchedulerGanttViewTotalProgressViewInfo read FTotalProgressViewInfo;
    property View: TcxSchedulerGanttView read GetGanttView;
    property VisibleEventEnumerationList[AIndex: Integer]: TcxSchedulerResourceViewInfo read GetVisibleEventEnumeration;
  end;

  { TcxSchedulerGanttViewPainter }

  TcxSchedulerGanttViewPainter = class(TcxSchedulerTimeGridViewPainter)
  private
    function GetView: TcxSchedulerGanttView;
    function GetViewInfo: TcxSchedulerGanttViewViewInfo;
    procedure DoDrawExpandButton(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
    procedure DoDrawLinkItem(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
  public
    procedure Paint; override;
    property View: TcxSchedulerGanttView read GetView;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerGanttViewHitTest }

  TcxSchedulerGanttViewHitTest = class(TcxSchedulerTimeGridViewHitTest)
  private
    function GetHitAtEventDraggingArea: Boolean;
    function GetHitExpandButton: TcxSchedulerGanttViewExpandButtonViewInfo;
    function GetHitLink: TcxSchedulerGanttViewLinkViewInfo;
    function GetView: TcxSchedulerGanttView;
  protected
    FCalcEventDraggingArea: Boolean;
    FEventDraggingArea: TRect;
    function CanResizeEvent(AEvent: TcxSchedulerControlEvent): Boolean; override;

    property View: TcxSchedulerGanttView read GetView;
    property HitExpandButton: TcxSchedulerGanttViewExpandButtonViewInfo read GetHitExpandButton;
    property HitLink: TcxSchedulerGanttViewLinkViewInfo read GetHitLink;
  public
    property HitAtEventDraggingArea: Boolean read GetHitAtEventDraggingArea;
    property HitAtExpandButton: Boolean index htcExpandButton read GetBitState;
    property HitAtLink: Boolean index htcLink read GetBitState;
  end;

  { TcxGanttDragEventHelper }

  TcxSchedulerEventDragMode = (sedmNone, sedmMoveTo, sedmTaskLinkTo);

  TcxSchedulerGanttDragObject = class(TcxSchedulerDragObject)
  private
    FDragGroupedTaskLinks: Boolean;
    function GetController: TcxSchedulerGanttViewController;
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    property Controller: TcxSchedulerGanttViewController read GetController;
  public
    constructor Create(AControl: TControl); override;
    property DragGroupedTaskLinks: Boolean read FDragGroupedTaskLinks write FDragGroupedTaskLinks;
  end;

  TcxGanttDragEventHelper = class(TcxTimeGridDragEventHelper)
  private
    FHitEvent: TcxSchedulerControlEvent;
    function GetController: TcxSchedulerGanttViewController;
    function GetDragObject: TcxSchedulerGanttDragObject;
    function GetHitTest: TcxSchedulerGanttViewHitTest;
    function GetViewInfo: TcxSchedulerGanttViewViewInfo;
  protected
    procedure ApplyChanges; override;
    procedure ApplyTaskLinks; virtual;
    function CanDragTo(ADeltaTime: TDateTime): Boolean; virtual;
    procedure CalculateConflicts; override;
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); override;
    procedure EndDrag(Accepted: Boolean); override;
    procedure InternalApplyTaskLink(AEvent: TcxSchedulerControlEvent; ALinkedEvent: TcxSchedulerEvent); virtual;
    function IsAtOrigin: Boolean; override;
    procedure PrepareGroupedTaskLinks; virtual;
    procedure UpdateViewClonesTime; override;

    property Controller: TcxSchedulerGanttViewController read GetController;
    property DragObject: TcxSchedulerGanttDragObject read GetDragObject;
    property HitTest: TcxSchedulerGanttViewHitTest read GetHitTest;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read GetViewInfo;
  end;

  { TcxGanttEventSizing }

  TcxGanttEventSizing = class(TcxTimeGridEventSizing)
  protected
    function CanResize: Boolean; virtual;
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); override;
    function IsValidTime: Boolean; override;
  end;

  { TcxSchedulerGanttViewController }

  TcxSchedulerGanttViewController = class(TcxSchedulerTimeGridViewController)
  private
    FEventDragMode: TcxSchedulerEventDragMode;
    FNeedEraseDraggingLink: Boolean;
    FTimerScrollBarKind: TScrollBarKind;
    FDragEventClipRect: TRect;
    procedure CalcDragEventClipRect;
    function GetDragEventHelper: TcxGanttDragEventHelper;
    function GetEventHintData(var AHint: string): Boolean;
    function GetHitTest: TcxSchedulerGanttViewHitTest;
    function GetLinkHintData(var AHint: string): Boolean;
    function GetView: TcxSchedulerGanttView;
    function GetViewInfo: TcxSchedulerGanttViewViewInfo;
    function InternalPaintDraggingLink(const AStart, AFinish: TPoint): Boolean;
    procedure SetEventDragMode(AValue: TcxSchedulerEventDragMode);
  protected
    FPrevDragPoint: TPoint;
    FStartDragPoint: TPoint;
    procedure CheckScrolling(const APos: TPoint); override;
    procedure CheckTaskLinkDrawOnScroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode);
    function ConsiderHiddenEvents: Boolean; override;
    function CreateDragEventHelper: TcxDragEventHelper; override;
    function CreateResizeEventHelper: TcxEventSizingHelper; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    procedure EraseDraggingLink; virtual;
    function GetDragEventClipRect: TRect; virtual;
    function GetStartDragPoint: TPoint;
    procedure InitTimer(AllowStart: Boolean; AScrollCode: TScrollCode); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OnTimer(Sender: TObject); override;
    procedure PaintDraggingLink(const P: TPoint); virtual;
    procedure Scrolling(AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    function ShowHint: Boolean; override;
    procedure StartDrag(var DragObject: TDragObject); override;
    procedure UpdateDraggingLink;
    procedure UpdateEventDragMode;

    property DragEventHelper: TcxGanttDragEventHelper read GetDragEventHelper;
    property EventDragMode: TcxSchedulerEventDragMode read FEventDragMode write SetEventDragMode;
    property HitTest: TcxSchedulerGanttViewHitTest read GetHitTest;
    property View: TcxSchedulerGanttView read GetView;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerGanttView }

  TcxSchedulerShowTaskDependencyEditorEvent = procedure (Sender: TcxCustomScheduler;
    ALink: TcxSchedulerEventItemLink; var AHandled: Boolean) of object;
  TcxSchedulerStartLinkEventEvent = procedure (Sender: TcxCustomScheduler;
    AEvent: TcxSchedulerControlEvent; var AAllow: Boolean) of object;
  TcxSchedulerEndLinkEventEvent = procedure (Sender: TcxCustomScheduler;
    ALinkEvent: TcxSchedulerControlEvent; var AAllow: Boolean) of object;


  TcxSchedulerGanttView = class(TcxSchedulerTimeGridView)
  private
    FEventsStyle: TcxSchedulerGanttViewEventStyle;
    FLinkLineColor: TColor;
    FShowExpandButtons: Boolean;
    FShowTotalProgressLine: Boolean;
    FTotalProgressLineColor: TColor;
    FOnEndLinkEvent: TcxSchedulerEndLinkEventEvent;
    FOnShowTaskDependencyEditor: TcxSchedulerShowTaskDependencyEditorEvent;
    FOnStartLinkEvent: TcxSchedulerStartLinkEventEvent;
    function GetController: TcxSchedulerGanttViewController;
    function GetHitTest: TcxSchedulerGanttViewHitTest;
    function GetViewInfo: TcxSchedulerGanttViewViewInfo;
    procedure SetEventsStyle(AValue: TcxSchedulerGanttViewEventStyle);
    procedure SetLinkLineColor(AColor: TColor);
    procedure SetShowExpandButtons(AValue: Boolean);
    procedure SetShowTotalProgressLine(AValue: Boolean);
    procedure SetTotalProgressLineColor(AValue: TColor);
  protected
    function CanEventEdit(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean; override;
    function CheckEventsVisibility: Boolean; override;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreatePainter: TcxSchedulerSubControlPainter; override;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    function DoEndLinkEvent(ALink: TcxSchedulerControlEvent): Boolean; virtual;
    function DoShowTaskDependencyEditor(ALink: TcxSchedulerEventItemLink): Boolean; virtual;
    function DoStartLinkEvent(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function GetDragObjectClass: TDragControlObjectClass; override;
    function GetEventHintText(AEvent: TcxSchedulerControlEvent): string; override;
    function IsInplaceEditingEnabled: Boolean; override;
    function ShowTaskComplete: Boolean; override;
    procedure ShowTaskDependencyEditor(ALink: TcxSchedulerEventItemLink); virtual;
    procedure ValidateContentPopupMenuItems(var AItems: TcxSchedulerContentPopupMenuItems); override;

    property Controller: TcxSchedulerGanttViewController read GetController;
    property EventDetailInfo;
    property ViewInfo: TcxSchedulerGanttViewViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    property HitTest: TcxSchedulerGanttViewHitTest read GetHitTest;
  published
    property TreeBrowser;
    property EventsStyle: TcxSchedulerGanttViewEventStyle read FEventsStyle write SetEventsStyle default esDefault;
    property LinkLineColor: TColor read FLinkLineColor write SetLinkLineColor default clNavy;
    property ShowExpandButtons: Boolean read FShowExpandButtons write SetShowExpandButtons default False;
    property ShowTotalProgressLine: Boolean read FShowTotalProgressLine write SetShowTotalProgressLine default False;
    property TotalProgressLineColor: TColor read FTotalProgressLineColor write SetTotalProgressLineColor default clRed;
    property OnEndLinkEvent: TcxSchedulerEndLinkEventEvent read FOnEndLinkEvent write FOnEndLinkEvent;
    property OnShowTaskDependencyEditor: TcxSchedulerShowTaskDependencyEditorEvent read FOnShowTaskDependencyEditor write FOnShowTaskDependencyEditor;
    property OnStartLinkEvent: TcxSchedulerStartLinkEventEvent read FOnStartLinkEvent write FOnStartLinkEvent;
  end;

implementation

uses
  cxLibraryConsts, cxSchedulerDialogs, cxVariants, dxTypeHelpers;

const
  LinkLineOffset = 5;
  ProgressNodeRadius = 4;

type
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);
  TcxSchedulerEventAccess = class(TcxSchedulerEvent);
  TcxSchedulerControlEventAccess = class(TcxSchedulerControlEvent);
  TcxSchedulerEventCellViewInfoAccess = class(TcxSchedulerEventCellViewInfo);
  TcxSchedulerHintControllerAccess = class(TcxSchedulerHintController);
  TcxSchedulerTimeGridResourceScrollAccess = class(TcxSchedulerTimeGridResourceScroll);
  TcxSchedulerCustomDateNavigatorAccess = class(TcxSchedulerCustomDateNavigator);

function cxGanttViewCompareEventPlaces(AEventPlace1: TcxSchedulerEventPlace;
  AEventPlace2: TcxSchedulerEventPlace): Integer;

    function CheckParentGroup(AParentEvent, AChildrenEvent: TcxSchedulerEvent): Boolean;
    begin
      Result := TcxSchedulerEventAccess(AParentEvent).IsParentGroupForEvent(AChildrenEvent);
    end;

    function CompareByParentGroups(AEvent1, AEvent2: TcxSchedulerControlEvent): Integer;
    begin
      if CheckParentGroup(AEvent1, AEvent2) then
        Result := -1
      else
        if CheckParentGroup(AEvent2, AEvent1) then
          Result := 1
        else
          Result := 0;
    end;

    function CheckClone(AEvent1, AEvent2: TcxSchedulerControlEvent;
      var ASign: Integer): Boolean;
    begin
      if AEvent1.IsClone and (AEvent1.Source = AEvent2) then
        ASign := 1
      else
        if AEvent2.IsClone and (AEvent2.Source = AEvent1) then
          ASign := -1
        else
          ASign := 0;
      Result := ASign <> 0;
    end;

var
  AEvent1: TcxSchedulerControlEvent;
  AEvent2: TcxSchedulerControlEvent;
  ATaskIndex1, ATaskIndex2: Integer;
begin
  Result := TdxNativeInt(AEventPlace1.Resource) - TdxNativeInt(AEventPlace2.Resource);
  if Result = 0 then
  begin
    AEvent1 := TcxSchedulerControlEvent(AEventPlace1.Event);
    AEvent2 := TcxSchedulerControlEvent(AEventPlace2.Event);
    if AEvent1 = AEvent2 then
      Exit;
    Result := Byte(AEvent1.Source = nil) - Byte(AEvent2.Source = nil);
    if Result <> 0 then
      Exit;
    Result := CompareByParentGroups(AEvent1, AEvent2);
    if Result <> 0 then
      Exit;
    if AEvent1.Pattern <> nil then
      ATaskIndex1 := TcxSchedulerControlEvent(AEvent1.Pattern).TaskIndex
    else
      ATaskIndex1 := AEvent1.TaskIndex;
    if AEvent2.Pattern <> nil then
      ATaskIndex2 := TcxSchedulerControlEvent(AEvent2.Pattern).TaskIndex
    else
      ATaskIndex2 := AEvent2.TaskIndex;
    Result := ATaskIndex1 - ATaskIndex2;
    if Result <> 0 then
    begin
      if ATaskIndex1 = -1 then
        Result := 1;
      if ATaskIndex2 = -1 then
        Result := -1;
    end
    else
      if not CheckClone(AEvent1, AEvent2, Result) and (ATaskIndex1 = -1) then
        Result := AEvent1.Index - AEvent2.Index;
  end;
end;

{ TcxSchedulerGanttView }

constructor TcxSchedulerGanttView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FLinkLineColor := clNavy;
  FTotalProgressLineColor := clRed;
end;

function TcxSchedulerGanttView.CanEventEdit(AEvent: TcxSchedulerControlEvent; AInplace: Boolean): Boolean;
begin
  Result := inherited CanEventEdit(AEvent, AInplace) and
    (not AInplace or not (ViewInfo.IsMilestone(AEvent) or ViewInfo.IsGroup(AEvent)));
end;

function TcxSchedulerGanttView.CheckEventsVisibility: Boolean;
begin
  Result := False;
end;

function TcxSchedulerGanttView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerGanttViewController.Create(Self);
end;

function TcxSchedulerGanttView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerGanttViewHitTest.Create(Self);
end;

function TcxSchedulerGanttView.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerGanttViewPainter.Create(Self);
end;

function TcxSchedulerGanttView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerGanttViewViewInfo.Create(Self);
end;

function TcxSchedulerGanttView.DoEndLinkEvent(ALink: TcxSchedulerControlEvent): Boolean;
begin
  Result := True;
  if Assigned(FOnEndLinkEvent) then
    FOnEndLinkEvent(Scheduler, ALink, Result);
end;

function TcxSchedulerGanttView.DoShowTaskDependencyEditor(
  ALink: TcxSchedulerEventItemLink): Boolean;
begin
  Result := False;
  if Assigned(OnShowTaskDependencyEditor) then
    OnShowTaskDependencyEditor(Scheduler, ALink, Result);
end;

function TcxSchedulerGanttView.DoStartLinkEvent(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := True;
  if Assigned(FOnStartLinkEvent) then
    FOnStartLinkEvent(Scheduler, AEvent, Result);
end;

function TcxSchedulerGanttView.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := TcxSchedulerGanttDragObject;
end;

function TcxSchedulerGanttView.GetEventHintText(
  AEvent: TcxSchedulerControlEvent): string;
begin
  Result := Format(cxGetResourceString(@scxGanttEventHint), [AEvent.Caption,
    AEvent.TaskComplete, cxDateToStr(AEvent.Start), cxDateToStr(AEvent.Finish)]);
end;

function TcxSchedulerGanttView.IsInplaceEditingEnabled: Boolean;
begin
  Result := EventsStyle <> esProgress;
end;

function TcxSchedulerGanttView.ShowTaskComplete: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerGanttView.ShowTaskDependencyEditor(
  ALink: TcxSchedulerEventItemLink);
begin
  if not DoShowTaskDependencyEditor(ALink) then
    cxShowTaskDependencyEditor(ALink, GetSchedulerLookAndFeel(True), Scheduler);
end;

procedure TcxSchedulerGanttView.ValidateContentPopupMenuItems(
  var AItems: TcxSchedulerContentPopupMenuItems);
begin
  if not (Scales.MinorUnit in [suHour, suDay]) then
    Exclude(AItems, cpmiToday);
  AItems := AItems - [cpmiGoToDate, cpmiGoToThisDay];
end;

function TcxSchedulerGanttView.GetController: TcxSchedulerGanttViewController;
begin
  Result := TcxSchedulerGanttViewController(inherited Controller);
end;

function TcxSchedulerGanttView.GetHitTest: TcxSchedulerGanttViewHitTest;
begin
  Result := TcxSchedulerGanttViewHitTest(inherited HitTest);
end;

function TcxSchedulerGanttView.GetViewInfo: TcxSchedulerGanttViewViewInfo;
begin
  Result := TcxSchedulerGanttViewViewInfo(inherited ViewInfo);
end;

procedure TcxSchedulerGanttView.SetEventsStyle(AValue: TcxSchedulerGanttViewEventStyle);
begin
  if AValue <> EventsStyle then
  begin
    FEventsStyle := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerGanttView.SetLinkLineColor(AColor: TColor);
begin
  if AColor <> LinkLineColor then
  begin
    FLinkLineColor := AColor;
    Changed;
  end;
end;

procedure TcxSchedulerGanttView.SetShowExpandButtons(AValue: Boolean);
begin
  if AValue <> ShowExpandButtons then
  begin
    FShowExpandButtons := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerGanttView.SetShowTotalProgressLine(AValue: Boolean);
begin
  if ShowTotalProgressLine <> AValue then
  begin
    FShowTotalProgressLine := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerGanttView.SetTotalProgressLineColor(AValue: TColor);
begin
  if TotalProgressLineColor <> AValue then
  begin
    FTotalProgressLineColor := AValue;
    Changed;
  end;
end;

{ TcxSchedulerGanttViewLinkItemViewInfo }

procedure TcxSchedulerGanttViewLinkViewInfo.Initialize(
  AViewInfo: TcxSchedulerGanttViewViewInfo;
  AStartEventViewInfo, AFinishEventViewInfo: TcxSchedulerEventCellViewInfo;
  AEventItemLink: TcxSchedulerEventItemLink);
begin
  FViewInfo := AViewInfo;
  FFinishEventViewInfo := AFinishEventViewInfo;
  FStartEventViewInfo := AStartEventViewInfo;
  FEventItemLink := AEventItemLink;
  FHint := Format(cxGetResourceString(@scxLinkHint), [
    cxGetResourceString(sEventRelations[Integer(Relation)]),
    cxGetResourceString(sEventRelationsShort[Integer(Relation)]),
    StartEventViewInfo.Caption, FinishEventViewInfo.Caption]);
  Calculate(AStartEventViewInfo.Bounds, AFinishEventViewInfo.Bounds);
  CalculateBounds;
end;

procedure TcxSchedulerGanttViewLinkViewInfo.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  // do nothing
end;

procedure TcxSchedulerGanttViewLinkViewInfo.BeforeCustomDraw(ACanvas: TcxCanvas);
begin
  // do nothing
end;

procedure TcxSchedulerGanttViewLinkViewInfo.Calculate(AStartRect, AFinishRect: TRect);

  function GetValidRect(const P1, P2: TPoint): TRect;
  begin
    if P1.X = P2.X then
      Result := Rect(P1.X, Min(P1.Y, P2.Y), P1.X + 1, Max(P1.Y, P2.Y) + 1)
    else
      Result := Rect(Min(P1.X, P2.X), P2.Y, Max(P1.X, P2.X) + 1, P2.Y + 1);
  end;

  procedure AddLine(const P1, P2: TPoint; var ALineIndex: Integer);
  begin
    if ALineIndex >= Length(FLines) then
      SetLength(FLines, ALineIndex + 1);
    FLines[ALineIndex] := GetValidRect(P1, P2);
    Inc(ALineIndex);
  end;

  procedure InitializePoints(var AStartPoint, AFinishPoint: TPoint; var ALineIndex: Integer);
  var
    AOrigStartRect: TRect;
    P: TPoint;
  begin
    AOrigStartRect := AStartRect;
    InitializeStartParams(AStartRect, P, AStartPoint);
    AddLine(P, AStartPoint, ALineIndex);
    InitializeFinishParams(AOrigStartRect, AFinishRect, P, AFinishPoint);
    AddLine(AFinishPoint, P, ALineIndex);
    CalculateArrowRect(AFinishPoint, P);
  end;

  function GetNextPoint(var APoint: TPoint; AFinishPoint: TPoint;
    AXOffset: Integer; var ALineIndex: Integer): Boolean;
  var
    P: TPoint;
    R: TRect;
  begin
    Result := True;
    P := APoint;
    P.X := APoint.X + AXOffset;
    if cxRectIntersect(R, AStartRect, GetValidRect(APoint, P)) and (cxRectWidth(R) > 1) then
    begin
      P := APoint;
      Result := P.Y <> AFinishPoint.Y;
      if Relation in [trFinishToFinish, trStartToStart] then
        P.Y := AFinishPoint.Y
      else
        if AFinishPoint.Y > P.Y then
          P.Y := AStartRect.Bottom + 1
        else
          P.Y := AStartRect.Top - 1;
    end;
    AddLine(APoint, P, ALineIndex);
    APoint := P;
  end;

var
  AFinishPoint: TPoint;
  ALineIndex: Integer;
  P: TPoint;
begin
  ALineIndex := 0;
  SetLength(FLines, 10);
  InitializePoints(P, AFinishPoint, ALineIndex);
  while GetNextPoint(P, AFinishPoint, AFinishPoint.X - P.X, ALineIndex) and
    (P.X <> AFinishPoint.X) do ;
  AddLine(P, AFinishPoint, ALineIndex);
  SetLength(FLines, ALineIndex);
end;

procedure TcxSchedulerGanttViewLinkViewInfo.CalculateArrowRect(const AArrowStart, AArrowFinish: TPoint);
begin
  FArrowDirection := GetArrowDirection(AArrowStart, AArrowFinish);
  case ArrowDirection of
    adUp:
      FArrowRect := Rect(AArrowStart.X - ScaleFactor.Apply(LinkLineOffset - 1), AArrowFinish.Y,
        AArrowStart.X + ScaleFactor.Apply(LinkLineOffset), AArrowStart.Y - ScaleFactor.Apply(LinkLineOffset));
    adDown:
      FArrowRect := Rect(AArrowStart.X - ScaleFactor.Apply(LinkLineOffset - 1),
        AArrowStart.Y + ScaleFactor.Apply(LinkLineOffset), AArrowStart.X + ScaleFactor.Apply(LinkLineOffset),
        AArrowFinish.Y);
    adLeft:
      FArrowRect := Rect(AArrowFinish.X, AArrowStart.Y - ScaleFactor.Apply(LinkLineOffset),
        AArrowStart.X - ScaleFactor.Apply(LinkLineOffset), AArrowFinish.Y + ScaleFactor.Apply(LinkLineOffset));
    adRight:
      FArrowRect := Rect(AArrowStart.X + ScaleFactor.Apply(LinkLineOffset),
        AArrowFinish.Y - ScaleFactor.Apply(LinkLineOffset), AArrowFinish.X, AArrowFinish.Y + ScaleFactor.Apply(LinkLineOffset));
  end;
end;

procedure TcxSchedulerGanttViewLinkViewInfo.CalculateBounds;
var
  R: TRect;
  K, I: Integer;
  ALines: TcxSchedulerGanttViewLinkLines;
begin
  FClipRect := ViewInfo.GetEventClipRect(StartEventViewInfo);
  K := 0;
  SetLength(ALines, Length(FLines));
  for I := 0 to High(Lines) do
    if cxRectIntersect(R, FLines[I], FClipRect) then
    begin
      ALines[K] := R;
      Inc(K);
    end;
  SetLength(ALines, K);
  FLines := ALines;
  R := ArrowRect;
  for I := 0 to High(Lines) do
    R := cxRectUnion(R, Lines[I]);
  InflateRect(R, 3, 3); // todo: valid redraw after hot link
  FBounds := R;
  FVisible := not cxRectIsEmpty(FClipRect);
end;

procedure TcxSchedulerGanttViewLinkViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FArrowRect := TdxRightToLeftLayoutConverter.ConvertRect(FArrowRect, AClientBounds);
  FArrowDirection := TdxRightToLeftLayoutConverter.ConvertArrowDirection(FArrowDirection);
  for I := 0 to High(FLines) do
    FLines[I] := TdxRightToLeftLayoutConverter.ConvertRect(FLines[I], AClientBounds);
end;

function TcxSchedulerGanttViewLinkViewInfo.GetArrowDirection(
  const AArrowStart, AArrowFinish: TPoint): TcxArrowDirection;
begin
  if AArrowStart.X = AArrowFinish.X then
    if AArrowStart.Y > AArrowFinish.Y then
      Result := adUp
    else
      Result := adDown
  else
    if AArrowStart.X > AArrowFinish.X then
      Result := adLeft
    else
      Result := adRight;
end;

procedure TcxSchedulerGanttViewLinkViewInfo.InitializeFinishParams(
  var AStartRect, AFinishRect: TRect; out P1, P2: TPoint);

  procedure GetToStartRelationParams(var R: TRect; out P1, P2: TPoint);
  begin
    P1 := cxPoint(R.Left, cxRectCenter(R).Y);
    InflateRect(R, 2 * ScaleFactor.Apply(LinkLineOffset), 0);
    P2 := cxPoint(R.Left, cxRectCenter(R).Y);
  end;

  procedure GetToFinishRelationParams(var R: TRect; out P1, P2: TPoint);
  begin
    P1 := cxPoint(R.Right, cxRectCenter(R).Y);
    InflateRect(R, 2 * ScaleFactor.Apply(LinkLineOffset), 0);
    P2 := cxPoint(R.Right, cxRectCenter(R).Y);
  end;

  procedure GetFinishToStartRelationParams(var R: TRect; out P1, P2: TPoint; ATargetX: Integer);
  begin
    ATargetX := Max(ATargetX, AStartRect.Right + ScaleFactor.Apply(LinkLineOffset));
    if ATargetX >= R.Right then
      GetToStartRelationParams(R, P1, P2)
    else
      if AStartRect.Top >= R.Top then
      begin
        P1 := cxPoint(ATargetX, AFinishRect.Bottom);
        InflateRect(R, 0, 2 * ScaleFactor.Apply(LinkLineOffset));
        P2 := cxPoint(ATargetX, AFinishRect.Bottom);
      end
      else
      begin
        P1 := cxPoint(ATargetX, AFinishRect.Top);
        InflateRect(R, 0, 2 * ScaleFactor.Apply(LinkLineOffset));
        P2 := cxPoint(ATargetX, AFinishRect.Top);
      end;
  end;

begin
  case Relation of
    trStartToStart:
      GetToStartRelationParams(AFinishRect, P1, P2);
    trFinishToFinish, trStartToFinish:
      GetToFinishRelationParams(AFinishRect, P1, P2);
    trFinishToStart:
      if ViewInfo.IsMilestone(FinishEventViewInfo.Event) then
        GetFinishToStartRelationParams(AFinishRect, P1, P2, cxRectCenter(AFinishRect).X)
      else
        GetFinishToStartRelationParams(AFinishRect, P1, P2, AFinishRect.Left);
  end;
end;

function TcxSchedulerGanttViewLinkViewInfo.GetHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest; var ABreak: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FLines) - 1 do
  begin
    Result := PtInRect(cxRectInflate(FLines[I], 1, 1), AHitTest.HitPoint);
    if Result then
    begin
      with TcxSchedulerGanttViewHitTest(AHitTest) do
      begin
        SetBitState(htcLink, True);
        FHitObject := Self;
        ABreak := True;
      end;
      Break;
    end;
  end;
end;

function TcxSchedulerGanttViewLinkViewInfo.GetIsHot: Boolean;
begin
  Result := ViewInfo.HotLink = Self;
end;

procedure TcxSchedulerGanttViewLinkViewInfo.InitializeStartParams(
  var AStartRect: TRect; out P1, P2: TPoint);

  function GetStartPoint(const R: TRect): TPoint;
  begin
    Result.Y := cxRectCenter(R).Y;
    if Relation in [trStartToStart, trStartToFinish] then
      Result.X := R.Left
    else
      Result.X := R.Right;
  end;

begin
  P1 := GetStartPoint(AStartRect);
  InflateRect(AStartRect, ScaleFactor.Apply(LinkLineOffset), ScaleFactor.Apply(LinkLineOffset));
  P2 := GetStartPoint(AStartRect);
end;

function TcxSchedulerGanttViewLinkViewInfo.GetRelation: TcxSchedulerEventRelation;
begin
  Result := EventItemLink.Relation;
end;

procedure TcxSchedulerGanttViewLinkViewInfo.SetClipRegion(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  ACanvas.SetClipRegion(TcxRegion.Create(ClipRect), roIntersect);
  for I := 0 to ViewInfo.NavigationButtons.Count - 1 do
    with ViewInfo.NavigationButtons.Items[I] do
      ACanvas.SetClipRegion(TcxRegion.Create(Bounds), roSubtract);
  ACanvas.ExcludeClipRect(StartEventViewInfo.Bounds);
  ACanvas.ExcludeClipRect(FinishEventViewInfo.Bounds);
end;

procedure TcxSchedulerGanttViewLinkViewInfo.Draw(ACanvas: TcxCanvas);

  function CheckRect(const R: TRect): TRect;
  begin
    Result := R;
    if IsHot then
      InflateRect(Result, 1, 1);
  end;

var
  AArrowPoints: TcxArrowPoints;
  I: Integer;
begin
  ACanvas.SaveState;
  try
    ACanvas.Brush.Style := bsSolid;
    ACanvas.SetBrushColor(ViewParams.Color);
    ACanvas.Pen.Color := ViewParams.Color;
    SetClipRegion(ACanvas);
    cxLookAndFeelPaintersManager.GetPainter(lfsStandard).CalculateArrowPoints(
      CheckRect(ArrowRect), AArrowPoints, ArrowDirection, True,
      ScaleFactor.Apply(LinkLineOffset) + Byte(IsHot) * 2);
    ACanvas.Polygon(AArrowPoints);
    for I := 0 to Length(FLines) - 1 do
      ACanvas.FillRect(CheckRect(FLines[I]));
  finally
    ACanvas.RestoreState;
  end;
end;

{ TcxSchedulerGanttViewPainter }

procedure TcxSchedulerGanttViewPainter.DoDrawExpandButton(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  // nothing to do
end;

procedure TcxSchedulerGanttViewPainter.DoDrawLinkItem(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  // nothing to do
end;

function TcxSchedulerGanttViewPainter.GetView: TcxSchedulerGanttView;
begin
  Result := inherited View as TcxSchedulerGanttView;
end;

function TcxSchedulerGanttViewPainter.GetViewInfo: TcxSchedulerGanttViewViewInfo;
begin
  Result := TcxSchedulerGanttViewViewInfo(inherited ViewInfo);
end;

procedure TcxSchedulerGanttViewPainter.Paint;
begin
  inherited Paint;
  ViewInfo.Links.Draw(Canvas, DoDrawLinkItem);
  ViewInfo.ExpandButtons.Draw(Canvas, DoDrawExpandButton);
  if View.ShowTotalProgressLine then
    ViewInfo.TotalProgressViewInfo.Draw(Canvas);
  View.Controller.UpdateDraggingLink;
end;

{ TcxSchedulerGanttViewHitTest }

function TcxSchedulerGanttViewHitTest.CanResizeEvent(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := inherited CanResizeEvent(AEvent) and not HitAtExpandButton;
end;

function TcxSchedulerGanttViewHitTest.GetHitAtEventDraggingArea: Boolean;

  function GetEventDraggingArea: TRect;
  var
    ADragEvent: TcxSchedulerControlEvent;
    R: TRect;
  begin
    Result := cxNullRect;
    ADragEvent := View.Controller.DragEvent;
    if (View.Controller.DragEventHelper = nil) or (ADragEvent = nil) or (ADragEvent.Source = nil) or
        (View = nil) or (View.ViewInfo = nil) then
      Exit;
    if View.ViewInfo.GetEventBounds(ADragEvent, Resource, R) then
    begin
      Result := View.ViewInfo.Bounds;
      Result.Top := R.Top;
      Result.Bottom := R.Bottom;
      FCalcEventDraggingArea := True;
    end;
  end;

begin
  if not FCalcEventDraggingArea then
    FEventDraggingArea := GetEventDraggingArea;
  Result := PtInRect(FEventDraggingArea, HitPoint);
end;

function TcxSchedulerGanttViewHitTest.GetHitExpandButton: TcxSchedulerGanttViewExpandButtonViewInfo;
begin
  if FHitObject is TcxSchedulerGanttViewExpandButtonViewInfo then
    Result := TcxSchedulerGanttViewExpandButtonViewInfo(FHitObject)
  else
    Result := nil
end;

function TcxSchedulerGanttViewHitTest.GetHitLink: TcxSchedulerGanttViewLinkViewInfo;
begin
  if FHitObject is TcxSchedulerGanttViewLinkViewInfo then
    Result := TcxSchedulerGanttViewLinkViewInfo(FHitObject)
  else
    Result := nil;
end;

function TcxSchedulerGanttViewHitTest.GetView: TcxSchedulerGanttView;
begin
  Result := TcxSchedulerGanttView(inherited Owner);
end;

{ TcxSchedulerGanttDragObject }

constructor TcxSchedulerGanttDragObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  DragGroupedTaskLinks := True;
end;

function TcxSchedulerGanttDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
const
  ACursorMap: array [Boolean] of TCursor = (crNoDrop, crTaskLink);
begin
  if Controller.EventDragMode <> sedmTaskLinkTo then
    Result := inherited GetDragCursor(Accepted, X, Y)
  else
    Result := ACursorMap[Accepted];
end;

function TcxSchedulerGanttDragObject.GetController: TcxSchedulerGanttViewController;
begin
  Result := TcxSchedulerGanttViewController(DragEventHelper.Controller);
end;

{ TcxGanttDragEventHelper }

procedure TcxGanttDragEventHelper.ApplyChanges;
begin
  if Controller.EventDragMode <> sedmTaskLinkTo then
    inherited ApplyChanges
  else
    ApplyTaskLinks;
end;

function CreateCustom(APattern: TcxSchedulerEvent; ADate: TDateTime): TcxSchedulerEvent;
begin
  Result := APattern.GetOccurrence(ADate);
  if Result <> nil then
  begin
    Result.EventType := etCustom;
    Result.Post;
  end;
end;

procedure TcxGanttDragEventHelper.ApplyTaskLinks;
var
  I: Integer;
  AHitEventPattern: TcxSchedulerEvent;
  AHitEventDate: TDateTime;
  ALinkedEvent: TcxSchedulerEvent;
begin
  Scheduler.Storage.BeginUpdate;
  try
    if (FHitEvent <> nil) and (FHitEvent.Source <> nil) and
      not FHitEvent.IsClone and (Scheduler.Storage <> nil) then
    begin
      if FHitEvent.EventType = etOccurrence then
      begin
        AHitEventDate := FHitEvent.Start;
        AHitEventPattern := FHitEvent.Pattern;
        ALinkedEvent := CreateCustom(AHitEventPattern, AHitEventDate);
      end
      else
        ALinkedEvent := FHitEvent.Source;

      for I := 0 to Clones.Count - 1 do
        InternalApplyTaskLink(TcxSchedulerControlEvent(TcxSchedulerControlEvent(Clones[I]).Source), ALinkedEvent);
    end;
  finally
    Events.CancelClones;
    SetSelection;
    Scheduler.Storage.EndUpdate;
  end;
end;

function TcxGanttDragEventHelper.CanDragTo(ADeltaTime: TDateTime): Boolean;

  function EventInClones(AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Clones.Count - 1 do
    begin
      if AEvent = TcxSchedulerControlEvent(Clones[I].Source).Source then
      begin
        Result := Clones[I];
        Break;
      end;
    end;
  end;

  function CanMoving(AClone: TcxSchedulerControlEvent): Boolean;
  var
    ANewTime: TDateTime;
  begin
    Result := AClone.TaskStatus = tsComplete;
    if not Result then
    begin
      ANewTime := AClone.Source.Start - ADeltaTime;
      Result := TcxSchedulerEventAccess(AClone).CanMoveTo(ANewTime);
    end;
  end;

var
  I: Integer;
begin
  Result := True;
  for I := 0 to Clones.Count - 1 do
  begin
    Result := Result and CanMoving(Clones[I]);
    if not Result then Break;
  end;
end;

procedure TcxGanttDragEventHelper.CalculateConflicts;
begin
  if Controller.EventDragMode = sedmTaskLinkTo then
    FHasConflicts := False
  else
    inherited CalculateConflicts;
end;

procedure TcxGanttDragEventHelper.DragOver(const P: TPoint;
  State: TDragState; var Accepted: Boolean);
var
  ADeltaTime: TDateTime;
begin
  if Controller.EventDragMode <> sedmTaskLinkTo then
  begin
    inherited DragOver(P, State, Accepted);
    case Destination of
      dodView:
        ADeltaTime := Controller.StartDragHitTime - HitTest.Time;
      dodDateNavigator:
        ADeltaTime := Controller.StartDragHitTime - TcxSchedulerCustomDateNavigatorAccess(DateNavigator).HitTest.Time;
    else
      ADeltaTime := MinDateTime;
    end;
    Accepted := (Accepted or HitTest.HitAtExpandButton or HitTest.HitAtLink) and CanDragTo(ADeltaTime);
  end
  else
    Accepted := Controller.DragEvent.IsResourceEvent(Controller.HitTest.Resource, True) and
      ((Controller.HitTest.Event = nil) or
      (not Controller.HitTest.Event.IsClone and Controller.DragEvent.Source.CanLink(Controller.HitTest.Event.Source) and
        Controller.DragEvent.Source.CanLink(Controller.HitTest.Event)));
end;

procedure TcxGanttDragEventHelper.EndDrag(Accepted: Boolean);
begin
  if Accepted and (Controller.EventDragMode = sedmTaskLinkTo) then
  begin
    FHitEvent := HitTest.Event;
    Accepted := FHitEvent <> nil;
  end;
  inherited EndDrag(Accepted);
end;

procedure TcxGanttDragEventHelper.InternalApplyTaskLink(AEvent: TcxSchedulerControlEvent; ALinkedEvent: TcxSchedulerEvent);
var
  ADragEvent: TcxSchedulerEvent;
  ADragEventPattern: TcxSchedulerEvent;
  ADragEventDate: TDateTime;
  ADragEventType: TcxEventType;
begin
  if AEvent.Source.CanLink(ALinkedEvent) and
    Controller.View.DoEndLinkEvent(FHitEvent) then
  begin
    ADragEvent := AEvent;
    ADragEventType := AEvent.EventType;
    if ADragEventType = etOccurrence then
    begin
      ADragEventDate := AEvent.Start;
      ADragEventPattern := ADragEvent.Pattern;
      ADragEvent := CreateCustom(ADragEventPattern, ADragEventDate);
    end;
    if ADragEvent <> nil then
      ADragEvent.TaskLinks.Add(ALinkedEvent, trFinishToStart);
  end;
end;

function TcxGanttDragEventHelper.IsAtOrigin: Boolean;
begin
  Result := inherited IsAtOrigin and (Controller.EventDragMode <> sedmTaskLinkTo);
end;

procedure TcxGanttDragEventHelper.PrepareGroupedTaskLinks;

  function GetControlEvent(ASource: TcxSchedulerEvent): TcxSchedulerControlEvent;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Events.Count - 1 do
      if Events[I].Source = ASource then
      begin
        Result := Events[I];
        Break;
      end;
  end;

  procedure AddClones(ALinksHeaderEvent: TcxSchedulerEvent);
  var
    I: Integer;
    AEvent: TcxSchedulerControlEvent;
  begin
    for I := 0 to ALinksHeaderEvent.TaskLinks.Count - 1 do
    begin
      if (ALinksHeaderEvent.TaskLinks[I].Link = nil) or
        (ALinksHeaderEvent.TaskLinks[I].Link.TaskLinkOwners.Count > 1) then Continue;
      AEvent := GetControlEvent(ALinksHeaderEvent.TaskLinks[I].Link);
      if AEvent = nil then Continue;
      Events.Selection.Add(AEvent, [ssCtrl]);
      AddClones(ALinksHeaderEvent.TaskLinks[I].Link);
    end;
  end;

var
  I: Integer;
begin
  if not DragObject.DragGroupedTaskLinks then Exit;
  for I := 0 to DragObject.DragEvents.Count - 1 do
    if not ViewInfo.IsTaskLinksExpanded(DragObject.DragEvents[I].TaskLinks) then
      AddClones(DragObject.DragEvents[I]);
  Events.CreateClones;
end;

procedure TcxGanttDragEventHelper.UpdateViewClonesTime;
var
  I: Integer;
  ADelta: TDateTime;
begin
  ADelta := Controller.StartDragHitTime - HitTest.Time;
  if not CanDragTo(ADelta) then
    Exit;
  for I := 0 to Clones.Count - 1 do
    with TcxSchedulerControlEventAccess(Clones[I]) do
      InternalMoveTo(Source.Start - ADelta);
  CorrectAllDayEventProperty;
end;

function TcxGanttDragEventHelper.GetController: TcxSchedulerGanttViewController;
begin
  Result := TcxSchedulerGanttViewController(inherited Controller);
end;

function TcxGanttDragEventHelper.GetDragObject: TcxSchedulerGanttDragObject;
begin
  Result := TcxSchedulerGanttDragObject(inherited DragObject);
end;

function TcxGanttDragEventHelper.GetHitTest: TcxSchedulerGanttViewHitTest;
begin
  Result := TcxSchedulerGanttViewHitTest(inherited HitTest);
end;

function TcxGanttDragEventHelper.GetViewInfo: TcxSchedulerGanttViewViewInfo;
begin
  Result := TcxSchedulerGanttViewViewInfo(inherited ViewInfo);
end;

{ TcxGanttEventSizing }

function TcxGanttEventSizing.CanResize: Boolean;
var
  AStartTime, AFinishTime: TDateTime;
begin
  Result := not Event.IsGroup or not Event.HasChildren;
  if Result then
  begin
    TcxSchedulerControlEvent(Event).GetValidTaskTimeRange(AStartTime, AFinishTime);
    Result := (HitTest.Time >= AStartTime) and (HitTest.Time < AFinishTime);
  end;
end;

procedure TcxGanttEventSizing.DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean);
begin
  inherited DragOver(P, State, Accepted);
  FHasConflicts := not Accepted;
end;

function TcxGanttEventSizing.IsValidTime: Boolean;
begin
  Result := inherited IsValidTime and CanResize;
end;

{ TcxSchedulerGanttViewController }

procedure TcxSchedulerGanttViewController.CheckScrolling(const APos: TPoint);
var
  ACanScroll: Boolean;
  AScrollCode: TScrollCode;
  R: TRect;
begin
  if DragEvent = nil then
  begin
    inherited CheckScrolling(APos);
    Exit;
  end;
  R := GetDragEventClipRect;
  FPos := APos;
  ACanScroll := True;
  AScrollCode := TScrollCode(Timer.Tag);
  if APos.X < (R.Left + cxScrollZoneSize) then
  begin
    AScrollCode := scLineUp;
    R.Right := R.Left + cxScrollZoneSize;
    FTimerScrollBarKind := sbHorizontal;
  end
  else
  begin
    if APos.X >= (R.Right - cxScrollZoneSize) then
    begin
      AScrollCode := scLineDown;
      R.Left := R.Right - cxScrollZoneSize;
      FTimerScrollBarKind := sbHorizontal;
    end
    else
    begin
      if APos.Y >= (R.Bottom - cxScrollZoneSize) then
      begin
        AScrollCode := scLineDown;
        R.Top := R.Bottom - cxScrollZoneSize;
        FTimerScrollBarKind := sbVertical;
      end
      else
      begin
        if APos.Y < (R.Top + cxScrollZoneSize) then
        begin
          AScrollCode := scLineUp;
          R.Bottom := R.Top + cxScrollZoneSize;
          FTimerScrollBarKind := sbVertical;
        end
        else
          ACanScroll := False;
      end;
    end;
  end;
  ACanScroll := ACanScroll and cxRectPtIn(R, APos);
  if (ACanScroll <> Timer.Enabled) or (Integer(AScrollCode) <> Timer.Tag) then
    InitTimer(ACanScroll, AScrollCode);
end;

procedure TcxSchedulerGanttViewController.CheckTaskLinkDrawOnScroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode);
var
  ADelta: Integer;
begin
  ADelta := 0;
  case AScrollCode of
    scLineDown, scPageDown:
      ADelta := -1;
    scLineUp, scPageUp:
      ADelta := 1;
  end;
  if ADelta <> 0 then
  begin
    case AScrollBarKind of
      sbHorizontal:
        begin
          ADelta := ADelta * ViewInfo.FColumnWidth;
          Inc(FStartDragPoint.X, ADelta);
        end;
      sbVertical:
        begin
          ADelta := ADelta * (ViewInfo.FEventRowHeight);
          Inc(FStartDragPoint.Y, ADelta);
        end;
    end;
  end;
end;

function TcxSchedulerGanttViewController.ConsiderHiddenEvents: Boolean;
begin
  Result := False;
end;

function TcxSchedulerGanttViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxGanttDragEventHelper.Create(Scheduler);
end;

function TcxSchedulerGanttViewController.CreateResizeEventHelper: TcxEventSizingHelper;
begin
  Result := TcxGanttEventSizing.Create(Scheduler);
end;

procedure TcxSchedulerGanttViewController.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  ADragMode: TcxSchedulerEventDragMode;
begin
  ADragMode := EventDragMode;
  if Assigned(DragEventHelper) then
  begin
    EraseDraggingLink;
    UpdateEventDragMode;
  end;
  inherited DragOver(Source, X, Y, State, Accept);
  if ADragMode = sedmTaskLinkTo then
    PaintDraggingLink(HitTest.HitPoint);
end;

procedure TcxSchedulerGanttViewController.EndDrag(Target: TObject; X, Y: Integer);
begin
  inherited EndDrag(Target, X, Y);
  EraseDraggingLink;
  EventDragMode := sedmNone;
  HitTest.FCalcEventDraggingArea := False;
  HitTest.FEventDraggingArea := cxNullRect;
end;

procedure TcxSchedulerGanttViewController.EraseDraggingLink;
begin
  if not FNeedEraseDraggingLink then
    Exit;
  InternalPaintDraggingLink(GetStartDragPoint, FPrevDragPoint);
  FNeedEraseDraggingLink := False;
  FPrevDragPoint := GetStartDragPoint;
end;

function TcxSchedulerGanttViewController.GetDragEventClipRect: TRect;
begin
  Result := FDragEventClipRect;
end;

function TcxSchedulerGanttViewController.GetStartDragPoint: TPoint;
begin
  CalcDragEventClipRect;
  Result := cxPointOffset(FStartDragPoint, GetDragEventClipRect.TopLeft);
end;

function TcxSchedulerGanttViewController.GetLinkHintData(var AHint: string): Boolean;
begin
  Result := HitTest.HitAtLink and (HitTest.HitLink <> nil);
  if Result then
    AHint := HitTest.HitLink.Hint;
end;

procedure TcxSchedulerGanttViewController.InitTimer(AllowStart: Boolean; AScrollCode: TScrollCode);
begin
  inherited InitTimer(AllowStart, AScrollCode);
  if not AllowStart then
    FPrevDragPoint := GetStartDragPoint;
end;

procedure TcxSchedulerGanttViewController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  CalcDragEventClipRect;
  FStartDragPoint := cxPointOffset(HitTest.HitPoint, GetDragEventClipRect.TopLeft, False);
  if (Button = mbLeft) and HitTest.HitAtExpandButton then
    HitTest.HitExpandButton.DoClick
  else
    if (Button = mbLeft) and HitTest.HitAtLink then
      View.ShowTaskDependencyEditor(HitTest.HitLink.FEventItemLink)
    else
      inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerGanttViewController.OnTimer(Sender: TObject);
begin
  inherited OnTimer(Sender);
end;

procedure TcxSchedulerGanttViewController.PaintDraggingLink(const P: TPoint);
begin
  if EventDragMode <> sedmTaskLinkTo then Exit;
  FNeedEraseDraggingLink := InternalPaintDraggingLink(GetStartDragPoint, P);
  FPrevDragPoint := P;
end;

procedure TcxSchedulerGanttViewController.Scrolling(AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  ADelta: Integer;
  AResourceIndex: Integer;
  ALastScrollPos: Integer;
begin
  if FTimerScrollBarKind = sbHorizontal then
  begin
    CheckTaskLinkDrawOnScroll(FTimerScrollBarKind, AScrollCode);
    View.Scroll(FTimerScrollBarKind, AScrollCode, AScrollPos);
  end
  else
  begin
    if DragEvent = nil then Exit;
    ADelta := 0;
    case AScrollCode of
      scLineUp:
        ADelta := -1;
      scLineDown:
        ADelta := 1;
    end;
    if ViewInfo.ResourceCount = 0 then
      AResourceIndex := 0
    else
      AResourceIndex := DragEvent.GetResourceItem.Index - Scheduler.FirstVisibleResourceIndex;
    if AResourceIndex >= 0 then
    begin
      ALastScrollPos := AScrollPos;
      AScrollPos := -TcxSchedulerTimeGridResourceScrollAccess(ViewInfo.ResourceViewShift[AResourceIndex]).FShift + ADelta;
      if (AScrollPos >= 0) and (AScrollPos <= ViewInfo.GetResourceScrollBarMax(AResourceIndex)) then
      begin
        ALastScrollPos := ViewInfo.ResourceViewShift[AResourceIndex].ScrollBar.Position;
        ViewInfo.DoResourceVerticalScroll(ViewInfo.ResourceViewShift[AResourceIndex].ScrollBar as TObject, AScrollCode, AScrollPos);
        if ALastScrollPos <> ViewInfo.ResourceViewShift[AResourceIndex].ScrollBar.Position then
          CheckTaskLinkDrawOnScroll(FTimerScrollBarKind, AScrollCode);
      end
      else
        AScrollPos := ALastScrollPos;
    end;
  end;
end;

function TcxSchedulerGanttViewController.ShowHint: Boolean;
var
  AHintText: string;
begin
  Result := GetLinkHintData(AHintText) or ((ViewInfo.ViewStyle = svsClassic) and GetEventHintData(AHintText));
  if Result then
  begin
    if not SameText(TcxSchedulerHintControllerAccess(HintController).FHintText,
      AHintText)
    then
      InternalHideHint;
    InternalShowHint(GetMouseCursorPos, AHintText);
  end
    else
    Result := inherited ShowHint;
end;

procedure TcxSchedulerGanttViewController.StartDrag(var DragObject: TDragObject);
begin
  FEventDragMode := sedmNone;
  FPrevDragPoint := GetStartDragPoint;
  HitTest.FCalcEventDraggingArea := False;
  HitTest.FEventDraggingArea := cxNullRect;
  CalcDragEventClipRect;
  inherited StartDrag(DragObject);
  DragEventHelper.PrepareGroupedTaskLinks;
end;

procedure TcxSchedulerGanttViewController.UpdateDraggingLink;
begin
  FNeedEraseDraggingLink := False;
  if EventDragMode <> sedmTaskLinkTo then Exit;
  CalcDragEventClipRect;
  PaintDraggingLink(FPrevDragPoint);
end;

procedure TcxSchedulerGanttViewController.UpdateEventDragMode;
begin
  if (EventDragMode = sedmNone) and not DragEventHelper.IsAtOrigin then
    EventDragMode := sedmMoveTo;
  if (EventDragMode <> sedmTaskLinkTo) and not IsCopyDragDrop and
    ((not HitTest.HitAtEventDraggingArea and ((EventDragMode = sedmNone) or
    (HitTest.HitAtEvent and (HitTest.Event.Source <> DragEvent)))) or
    (DragEvent.IsGroup and DragEvent.HasChildren)) then
    EventDragMode := sedmTaskLinkTo;
end;

procedure TcxSchedulerGanttViewController.CalcDragEventClipRect;
var
  AViewInfo: TcxSchedulerEventCellViewInfo;
begin
  if (DragEvent <> nil) and
      ViewInfo.FindEventViewInfoBySource(DragEvent.Source, dxDateOf(DragEvent.Start), HitTest.Resource, AViewInfo) then
    FDragEventClipRect := ViewInfo.GetEventClipRect(AViewInfo)
  else
    FDragEventClipRect := cxEmptyRect;
end;

function TcxSchedulerGanttViewController.GetDragEventHelper: TcxGanttDragEventHelper;
begin
  Result := TcxGanttDragEventHelper(inherited DragEventHelper);
end;

function TcxSchedulerGanttViewController.GetEventHintData(var AHint: string): Boolean;
var
  AEvent: TcxSchedulerControlEvent;
begin
  Result := (EventDragMode = sedmNone) and HitTest.HitAtEvent;
  if Result then
  begin
    AEvent := HitTest.Event;
    AHint := View.GetEventHintText(AEvent);
    TcxCustomSchedulerAccess(Scheduler).GetEventUserHintText(AEvent, AHint);
  end;
end;

function TcxSchedulerGanttViewController.GetHitTest: TcxSchedulerGanttViewHitTest;
begin
  Result := View.HitTest;
end;

function TcxSchedulerGanttViewController.GetView: TcxSchedulerGanttView;
begin
  Result := TcxSchedulerGanttView(inherited View);
end;

function TcxSchedulerGanttViewController.GetViewInfo: TcxSchedulerGanttViewViewInfo;
begin
  Result := View.ViewInfo;
end;

function TcxSchedulerGanttViewController.InternalPaintDraggingLink(const AStart, AFinish: TPoint): Boolean;
begin
  Result := (AStart.X <> AFinish.X) or (AStart.Y <> AFinish.Y);
  if not Result then
    Exit;
  with View.GetControlCanvas do
  begin
    SaveState;
    try
      WindowOrg := cxPoint(-View.Left, -View.Top);
      SetClipRegion(TcxRegion.Create(GetDragEventClipRect), roSet);
      Pen.Mode := pmNotXor;
      Pen.Style := psSolid;
      Pen.Color := View.LinkLineColor;
      MoveTo(AStart.X, AStart.Y);
      LineTo(AFinish.X, AFinish.Y);
    finally
      RestoreState;
    end;
  end;
end;

procedure TcxSchedulerGanttViewController.SetEventDragMode(AValue: TcxSchedulerEventDragMode);
var
  I: Integer;
begin
  if AValue = FEventDragMode then Exit;
  if AValue = sedmTaskLinkTo then
  begin
    if not View.DoStartLinkEvent(DragEvent) then Exit;
    for I := 0 to DragEventHelper.Clones.Count -1 do
      with DragEventHelper.Clones[I] do
      begin
        MoveTo(Source.Start);
        ResourceID := Source.ResourceID;
      end;
    View.Refresh;
    FPrevDragPoint := GetStartDragPoint;
  end;
  FEventDragMode := AValue;
end;

{ TcxSchedulerGanttViewEventLayoutBuilder }

procedure TcxSchedulerGanttViewEventLayoutBuilder.Calculate(
  const ASchedulerViewStyle: TcxSchedulerViewStyle = svsClassic);

  function FindStartOfRecurrence(AEventPlace: TcxSchedulerEventPlace;
    ACheckEventPlaceCount: Integer; out AIndex: Integer): Boolean;
  var
    AMin: Integer;
    APlace: TcxSchedulerEventPlace;
    I: Integer;
  begin
    Result := False;
    if AEventPlace.Event.Pattern <> nil then
    begin
      AMin := MAXINT;
      for I := 0 to ACheckEventPlaceCount - 1 do
      begin
        APlace := EventPlaces[I];
        if (APlace.Resource = AEventPlace.Resource) and
          (APlace.Event.Pattern = AEventPlace.Event.Pattern) and
          (APlace.Event.RecurrenceIndex < AMin) and
          (((I > 0) and (APlace.LineStart >= 0)) or (I = 0)) then
        begin
          AMin := APlace.Event.RecurrenceIndex;
          Result := APlace <> AEventPlace;
          if Result then
            AIndex := APlace.LineStart;
        end;
      end;
    end;
  end;

  procedure CheckEventPlaceResource(AEventPlace: TcxSchedulerEventPlace;
    var ALastResource, APlaceIndex: Integer);
  begin
    if ALastResource <> Integer(AEventPlace.Resource) then
    begin
      ALastResource := Integer(AEventPlace.Resource);
      APlaceIndex := 0;
    end;
  end;

  function GetTaskIndex(AEventPlace: TcxSchedulerEventPlace;
    ACheckEventPlaceCount: Integer; var APlaceIndex: Integer): Integer;
  begin
    if not FindStartOfRecurrence(AEventPlace, ACheckEventPlaceCount, APlaceIndex) then
      APlaceIndex := Max(AEventPlace.Event.TaskIndex, APlaceIndex);
    Result := APlaceIndex;
    Inc(APlaceIndex);
  end;

var
  AEventPlace: TcxSchedulerEventPlace;
  APlaceIndex: Integer;
  ALastResource: Integer;
  I: Integer;
begin
  APlaceIndex := 0;
  ALastResource := -1;
  EventPlacesList.Sort(TListSortCompare(@cxGanttViewCompareEventPlaces));
  for I := 0 to EventPlaceCount - 1 do
  begin
    AEventPlace := EventPlaces[I];
    CheckEventPlaceResource(AEventPlace, ALastResource, APlaceIndex);
    AEventPlace.LineStart := GetTaskIndex(AEventPlace, I + 1, APlaceIndex);
    AEventPlace.LineFinish := AEventPlace.LineStart;
  end;
end;

{ TcxSchedulerGanttViewExpandButtonViewInfo }

procedure TcxSchedulerGanttViewExpandButtonViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Painter.DrawScaledExpandButton(ACanvas, Bounds, Expanded, ScaleFactor);
end;

procedure TcxSchedulerGanttViewExpandButtonViewInfo.DoClick;
begin
  with Event.TaskLinks do
    Expanded := not Expanded;
end;

procedure TcxSchedulerGanttViewExpandButtonViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  TcxSchedulerGanttViewHitTest(AHitTest).SetBitState(htcExpandButton, True);
end;

{ TcxSchedulerGanttViewTotalProgressViewInfo }

constructor TcxSchedulerGanttViewTotalProgressViewInfo.Create(
  AViewInfo: TcxSchedulerGanttViewViewInfo);
begin
  FViewInfo := AViewInfo;
end;

procedure TcxSchedulerGanttViewTotalProgressViewInfo.Calculate;
begin
  FDrawAsPolyline := ViewInfo.View.EventsStyle = esProgress;
  FLineColor := ViewInfo.View.TotalProgressLineColor;
  FTimeLineRect := GetTimeLineRect;
  FContentArea := ViewInfo.Bounds;
  Inc(FContentArea.Left, ViewInfo.ResourceHeaderWidth);
  if DrawAsPolyline then
    CalculatePolyline;
end;

procedure TcxSchedulerGanttViewTotalProgressViewInfo.CalculatePolyline;
var
  ANodeIndex: Integer;
  APointIndex: Integer;
  AVisibleLineCount: Integer;
  I: Integer;
  R: TRect;

  procedure AddPoint(X, Y: Integer);
  begin
    FPoints[APointIndex] := Point(X, Y);
    Inc(APointIndex);
  end;

  procedure AddPointsByResource(const ALine, ABounds: TRect;
    AResource: TcxSchedulerStorageResourceItem);
  var
    APoint: TPoint;
    ATop: Integer;
  begin
    ATop := ABounds.Top;
    while ATop < ABounds.Bottom do
    begin
      AddPoint(ALine.Left, ATop);
      Inc(ATop, ViewInfo.IndentBetweenLines);
      if GetUncompletedEventPoint(Rect(ALine.Left, ATop, ALine.Right,
        ATop + ViewInfo.ContentLineHeight), APoint, AResource)
      then
      begin
        AddPoint(ALine.Left, ATop);
        AddPoint(APoint.X, ATop + ViewInfo.ContentLineHeight div 2);
        FProgressNodes[ANodeIndex] := Point(APoint.X,
          ATop + ViewInfo.ContentLineHeight div 2);
        Inc(ANodeIndex);
      end;
      Inc(ATop, ViewInfo.ContentLineHeight);
    end;
    AddPoint(ALine.Left, ABounds.Bottom);
  end;

begin
  APointIndex := 0;
  ANodeIndex := 0;
  AVisibleLineCount := (1 + cxRectHeight(ViewInfo.Bounds) div ViewInfo.FEventRowHeight);
  SetLength(FPoints, 3 * AVisibleLineCount);
  SetLength(FProgressNodes, AVisibleLineCount);
  AddPoint(TimeLineRect.Left, TimeLineRect.Top);
  if ViewInfo.ResourceCount = 0 then
  begin
    R := ViewInfo.Bounds;
    R.Top := ViewInfo.ScalesHeight;
    AddPointsByResource(TimeLineRect, R, nil);
  end
  else
    for I := 0 to ViewInfo.ResourceCount - 1 do
      AddPointsByResource(TimeLineRect, ViewInfo.ResourceHeaderCells.Items[I + 1].Bounds,
        ViewInfo.Resources[I].ResourceItem);
  SetLength(FPoints, APointIndex);
  SetLength(FProgressNodes, ANodeIndex);
end;

procedure TcxSchedulerGanttViewTotalProgressViewInfo.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ContentArea);
    for I := 0 to ViewInfo.NavigationButtons.Count - 1 do
      ACanvas.ExcludeClipRect(ViewInfo.NavigationButtons.Items[I].Bounds);
    for I := 0 to ViewInfo.GroupSeparatorCells.Count - 1 do
      ACanvas.ExcludeClipRect(ViewInfo.GroupSeparatorCells.Items[I].Bounds);

    if DrawAsPolyline then
    begin
      ACanvas.Pen.Color := LineColor;
      ACanvas.Polyline(FPoints);
      DrawProgressNodes(ACanvas);
    end
    else
      ACanvas.FillRect(TimeLineRect, LineColor);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxSchedulerGanttViewTotalProgressViewInfo.DrawProgressNodes(
  ACanvas: TcxCanvas);
var
  I: Integer;
  R: TRect;
begin
  ACanvas.Brush.Color := LineColor;
  for I := 0 to High(FProgressNodes) do
  begin
    R.TopLeft := FProgressNodes[I];
    R.BottomRight := FProgressNodes[I];
    InflateRect(R, ScaleFactor.Apply(ProgressNodeRadius), ScaleFactor.Apply(ProgressNodeRadius));
    ACanvas.Canvas.Ellipse(R);
  end;
end;

function TcxSchedulerGanttViewTotalProgressViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := FViewInfo.ScaleFactor;
end;

function TcxSchedulerGanttViewTotalProgressViewInfo.FindUncompletedEventBeforeLine(
  const ALine: TRect; AResource: TcxSchedulerStorageResourceItem;
  var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
var
  AEventViewInfo: TcxSchedulerEventCellViewInfo;
  I: Integer;
  R: TRect;
begin
  Result := False;
  for I := 0 to ViewInfo.EventCells.Count - 1 do
  begin
    AEventViewInfo := TcxSchedulerEventCellViewInfo(ViewInfo.EventCells.Items[I]);
    R := AEventViewInfo.Bounds;
    if (AResource = nil) or (AResource = AEventViewInfo.ResourceItem) then
      if (R.Left <= ALine.Left) and (R.Top >= ALine.Top) and (R.Bottom <= ALine.Bottom) then
        if (AEventViewInfo.Event.TaskComplete < 100) and not AEventViewInfo.Hidden then
        begin
          AViewInfo := AEventViewInfo;
          Result := True;
          Break; // todo: need find nearest event (not first!)
        end;
  end;
end;

function TcxSchedulerGanttViewTotalProgressViewInfo.GetTimeLineApproximateRect: TRect;
var
  AVisibleStart: TDateTime;
begin
  AVisibleStart := ViewInfo.View.VisibleStart;
  if Now > AVisibleStart then
  begin
    Result := ViewInfo.Bounds;
    Inc(Result.Left, ViewInfo.ResourceHeaderWidth);
    Inc(Result.Left, Trunc((Now - AVisibleStart) *
      (Result.Right - Result.Left) / (ViewInfo.FLastVisibleTime - AVisibleStart)));
    Result.Right := Result.Left;
  end
  else
    Result := cxEmptyRect;
end;

function TcxSchedulerGanttViewTotalProgressViewInfo.GetTimeLineRect: TRect;
var
  ACell: TcxSchedulerTimeGridSelectionBarCell;
  AItem: TcxSchedulerCustomViewInfoItem;
  I: Integer;
begin
  Result := cxEmptyRect;
  for I := 0 to ViewInfo.TimeLineCells.Count - 1 do
  begin
    AItem := ViewInfo.TimeLineCells.Items[I];
    if AItem is TcxSchedulerTimeGridSelectionBarCell then
    begin
      ACell := TcxSchedulerTimeGridSelectionBarCell(AItem);
      if ACell.IsCurrentTimeCell then
      begin
        Result := ACell.TimeLineRect;
        Break;
      end;
    end;
  end;
  if IsRectEmpty(Result) then
    Result := GetTimeLineApproximateRect;
end;

function TcxSchedulerGanttViewTotalProgressViewInfo.GetUncompletedEventPoint(
  const ALine: TRect; var P: TPoint;
  AResource: TcxSchedulerStorageResourceItem): Boolean;
var
  AEventViewInfo: TcxSchedulerEventCellViewInfo;
  R: TRect;
begin
  Result := FindUncompletedEventBeforeLine(ALine, AResource, AEventViewInfo);
  if Result then
  begin
    R := cxRectContent(AEventViewInfo.Bounds,
      ViewInfo.View.ExternalPainter.Painter.SchedulerEventProgressOffsets);
    P := Point(Min(ALine.Left, R.Left + Trunc((R.Right - R.Left) *
      AEventViewInfo.Event.TaskComplete / 100)), (ALine.Top + ALine.Bottom) div 2);
  end;
end;

{ TcxSchedulerGanttViewViewInfo }

constructor TcxSchedulerGanttViewViewInfo.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FExpandButtons := TcxSchedulerViewInfoCellList.Create;
  FLinks := TcxSchedulerViewInfoCellList.Create;
  FTotalProgressViewInfo := TcxSchedulerGanttViewTotalProgressViewInfo.Create(Self);
  FVisibleEventEnumerationList := TcxObjectList.Create;
  FCells.Add(FExpandButtons);
  FCells.Add(FLinks);
end;

destructor TcxSchedulerGanttViewViewInfo.Destroy;
begin
  FreeAndNil(FTotalProgressViewInfo);
  FreeAndNil(FVisibleEventEnumerationList);
  inherited Destroy;
end;

procedure TcxSchedulerGanttViewViewInfo.AddEventForCalculation(
  ABuilder: TcxSchedulerEventLayoutBuilder; AEvent: TcxSchedulerControlEvent;
  AResource: TcxSchedulerStorageResourceItem; AResourceIndex: Integer);
begin
  if ((AEvent.Source = nil) or IsEventExpanded(AEvent.Source)) and IsGanttTaskVisible(AEvent) then
    inherited AddEventForCalculation(ABuilder, AEvent, AResource, AResourceIndex);
end;

procedure TcxSchedulerGanttViewViewInfo.CalculateExpandButtonViewInfos;

  function CalculateButtonBounds(const AEventRect: TRect): TRect;
  var
    ASize: Integer;
  begin
    ASize := LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor);
    Result.Left := AEventRect.Right - ASize;
    Result.Top := (AEventRect.Top + AEventRect.Bottom - ASize) div 2;
    Result.Bottom := Result.Top + ASize;
    Result.Right := AEventRect.Right;
    OffsetRect(Result, 3, 0);
  end;

var
  AButtonViewInfo: TcxSchedulerGanttViewExpandButtonViewInfo;
  AEventCell: TcxSchedulerEventCellViewInfo;
  I: Integer;
begin
  if not FShowLinks then Exit;
  FExpandButtons.Clear;
  if View.ShowExpandButtons then
    for I := 0 to EventCells.Count - 1 do
    begin
      AEventCell := TcxSchedulerEventCellViewInfo(EventCells.Items[I]);
      if AEventCell.Event.TaskLinks = nil then Continue;
      if not AEventCell.Hidden and (AEventCell.Event.NonExceptionLinkCount > 0) then
      begin
        CreateCellInstance(TcxSchedulerGanttViewExpandButtonViewInfo,
          CalculateButtonBounds(AEventCell.Bounds), LinkViewParams, UseRightToLeftAlignment, AButtonViewInfo);
        AButtonViewInfo.FExpanded := IsTaskLinksExpanded(AEventCell.Event.TaskLinks);
        AButtonViewInfo.FEvent := AEventCell.Event;
        FExpandButtons.Add(AButtonViewInfo);
      end;
    end;
end;

procedure TcxSchedulerGanttViewViewInfo.CalculateHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  if (View.Controller.DragEventHelper <> nil) or
      ((View.Controller.DragAndDropObject <> nil) and (View.Controller.DragAndDropObject.SizingHelper <> nil)) or
      not (ExpandButtons.CalculateHitTest(AHitTest) or Links.CalculateHitTest(AHitTest)) then
    inherited CalculateHitTest(AHitTest);
  if TcxSchedulerGanttViewHitTest(AHitTest).HitExpandButton <> nil then
    FEventCells.CalculateHitTest(AHitTest);
  if GetIsHotTrackEnabled then
    HotLink := TcxSchedulerGanttViewHitTest(AHitTest).HitLink
  else
    HotLink := nil;
end;

function TcxSchedulerGanttViewViewInfo.IsEventExpanded(AEvent: TcxSchedulerEvent): Boolean;
var
  I: Integer;
begin
  Result := not View.ShowExpandButtons;
  if not Result then
  begin
    Result := (AEvent.TaskLinkOwners = nil) or (AEvent.TaskLinkOwners.Count = 0);
    if not Result then
    begin
      for I := 0 to AEvent.TaskLinkOwners.Count - 1 do
      begin
        Result := IsTaskLinksExpanded(AEvent.TaskLinkOwners[I].TaskLinks) and IsEventExpanded(AEvent.TaskLinkOwners[I]);
        if Result then
          Break;
      end;
    end;
    Result := Result and ((AEvent.ParentGroup = nil) or IsEventExpanded(AEvent.ParentGroup));
  end;
end;

procedure TcxSchedulerGanttViewViewInfo.CheckEventVisibility(
  ACell: TcxSchedulerEventCellViewInfo; AIndex: Integer);
begin
  if not ACell.Hidden and not ((ACell.Event.Source = nil) or
    IsEventExpanded(ACell.Event.Source)) then
  begin
    ACell.Free;
    FEventCells.Delete(AIndex);
  end;
end;

procedure TcxSchedulerGanttViewViewInfo.Clear;
begin
  inherited Clear;
  FVisibleEventEnumerationList.Clear;
  FVisibleEventWithoutResourceCount := 0;
end;

function TcxSchedulerGanttViewViewInfo.CreateEventViewData(
  AEvent: TcxSchedulerControlEvent; const ABounds: TRect; const AStart: TDateTime;
  const AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;

  procedure IncreaseVisibleEventCounter;
  var
    AResourceIndex: Integer;
    AResourceFound: Boolean;
  begin
    AResourceFound := False;
    for AResourceIndex := 0 to ResourceCount - 1 do
      if AEvent.IsSharedWithResource(VisibleEventEnumerationList[AResourceIndex].ResourceID) then
      begin
        VisibleEventEnumerationList[AResourceIndex].VisibleEventCount :=
          VisibleEventEnumerationList[AResourceIndex].VisibleEventCount + 1;
        AResourceFound := True;
      end;
    if not AResourceFound then
      Inc(FVisibleEventWithoutResourceCount);
  end;

begin
  Result := inherited CreateEventViewData(AEvent, ABounds, AStart, AFinish, AResource);
  Result.DrawAsProgress := View.EventsStyle = esProgress;
  Result.IsGroup := IsGroup(AEvent);
  Result.IsMilestone := IsMilestone(AEvent);
  if (AEvent.Start < FLastVisibleTime) and
    (AEvent.Finish > View.VisibleStart) then
    IncreaseVisibleEventCounter;
end;

function TcxSchedulerGanttViewViewInfo.CreateLayoutBuilder: TcxSchedulerEventLayoutBuilder;
begin
  Result := TcxSchedulerGanttViewEventLayoutBuilder.Create;
end;

function TcxSchedulerGanttViewViewInfo.CreateLinkItemViewInfo(
  AEventViewInfo: TcxSchedulerEventCellViewInfo;
  AEventItemLink: TcxSchedulerEventItemLink;
  out ALinkItem: TcxSchedulerGanttViewLinkViewInfo): Boolean;
var
  AFinishViewInfo: TcxSchedulerEventCellViewInfo;
begin
  Result := AEventItemLink.Link <> nil;
  if Result then
  begin
    Result := FindEventViewInfoBySource(AEventItemLink.Link, AEventItemLink.Link.Start,
      AEventViewInfo.ResourceItem, AFinishViewInfo);
  if Result then
    begin
      CreateCellInstance(TcxSchedulerGanttViewLinkViewInfo, AEventViewInfo.Bounds,
        Bounds, LinkViewParams, UseRightToLeftAlignment, ALinkItem);
      ALinkItem.Initialize(Self, AEventViewInfo, AFinishViewInfo, AEventItemLink);
    end;
  end;
end;

procedure TcxSchedulerGanttViewViewInfo.CreateLinks;
var
  AEventViewInfo: TcxSchedulerEventCellViewInfo;
  ALinkItem: TcxSchedulerGanttViewLinkViewInfo;
  ALinkIndex: Integer;
  ATaskLinks: TcxSchedulerEventLinks;
  I: Integer;
begin
  if not FShowLinks then Exit;
  for I := 0 to EventCells.Count - 1 do
  begin
    AEventViewInfo := TcxSchedulerEventCellViewInfo(EventCells.Items[I]);
    ATaskLinks := AEventViewInfo.Event.TaskLinks;
    if (ATaskLinks = nil) or not IsTaskLinksExpanded(ATaskLinks) then Continue;
    for ALinkIndex := 0 to ATaskLinks.Count - 1 do
      if CreateLinkItemViewInfo(AEventViewInfo, ATaskLinks.ItemLinks[ALinkIndex],
        ALinkItem)
      then
        FLinks.Add(ALinkItem);
  end;
end;

procedure TcxSchedulerGanttViewViewInfo.DoCalculate;
begin
  Adapter.GetPageResources(FVisibleEventEnumerationList);
  inherited DoCalculate;
  CalculateExpandButtonViewInfos;
  if View.ShowTotalProgressLine then
    TotalProgressViewInfo.Calculate;
  RecreateLinks;
end;

function TcxSchedulerGanttViewViewInfo.FindEventViewInfoBySource(
  AEvent: TcxSchedulerEvent; const ADate: TDateTime;
  AResource: TcxSchedulerStorageResourceItem;
  var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
var
  ATempViewInfo: TcxSchedulerEventCellViewInfo;
  I: Integer;
begin
  AViewInfo := nil;
  for I := 0 to EventCells.Count - 1 do
  begin
    ATempViewInfo := TcxSchedulerEventCellViewInfo(EventCells.Items[I]);
    if ATempViewInfo.Event.IsOrigin(AEvent) then
      if (AResource = nil) or (ATempViewInfo.ResourceItem = AResource) then
      begin
        AViewInfo := ATempViewInfo;
        with TcxSchedulerEventCellViewInfoAccess(ATempViewInfo) do
          if (ADate = NullDate) or ((dxDateOf(ContentStart) = ADate) or
            (IsHeaderEvent and (ADate >= dxDateOf(ContentStart)) and
            (ADate < dxDateOf(ContentFinish))))
          then
            Break;
    end;
  end;
  Result := AViewInfo <> nil;
end;

function TcxSchedulerGanttViewViewInfo.GetEventBounds(AEvent: TcxSchedulerControlEvent; AResource: TcxSchedulerStorageResourceItem; out AEventRect: TRect): Boolean;
var
  I: Integer;
  ATempViewInfo: TcxSchedulerEventCellViewInfo;
begin
  Result := False;
  for I := 0 to EventCells.Count - 1 do
  begin
    ATempViewInfo := TcxSchedulerEventCellViewInfo(EventCells.Items[I]);
    Result := (ATempViewInfo.Event = AEvent) and (ATempViewInfo.ResourceItem = AResource);
    if Result then
    begin
      AEventRect := ATempViewInfo.Bounds;
      Break;
    end;
  end;
end;

function TcxSchedulerGanttViewViewInfo.GetEventForResourceCount(
  AResourceIndex: Integer; out AResourceID: Variant): Integer;
var
  I: Integer;
  AFound: Boolean;
begin
  Result := inherited GetEventForResourceCount(AResourceIndex, AResourceID);
  AFound := False;
  I := 0;
  while (I < ResourceCount) and not AFound do
  begin
    if VarEquals(VisibleEventEnumerationList[I].ResourceID, AResourceID) then
    begin
      Result := VisibleEventEnumerationList[I].VisibleEventCount;
      AFound := True;
    end;
    Inc(I);
  end;
end;

function TcxSchedulerGanttViewViewInfo.GetEventWithoutResourceCount: Integer;
begin
  Result := FVisibleEventWithoutResourceCount;
end;

function TcxSchedulerGanttViewViewInfo.GetGanttView: TcxSchedulerGanttView;
begin
  Result := TcxSchedulerGanttView(inherited View);
end;

function TcxSchedulerGanttViewViewInfo.GetIndentBetweenLines: Integer;
begin
  Result := 3 * ScaleFactor.Apply(LinkLineOffset);
end;

function TcxSchedulerGanttViewViewInfo.GetNeedShowCurrentTime: Boolean;
begin
  Result := not View.ShowTotalProgressLine;
end;

function TcxSchedulerGanttViewViewInfo.GetIsHotTrackEnabled: Boolean;
begin
  Result := View.Scheduler.OptionsBehavior.HotTrack and (View.Controller.EventDragMode = sedmNone);
end;

function TcxSchedulerGanttViewViewInfo.GetLinkViewParams: TcxViewParams;
begin
  Result.Bitmap := nil;
  Result.Color := View.LinkLineColor;
  Result.Font := Scheduler.Font;
  Result.TextColor := View.LinkLineColor;
end;

function TcxSchedulerGanttViewViewInfo.GetVisibleEventEnumeration(
  AIndex: Integer): TcxSchedulerResourceViewInfo;
begin
  if (FVisibleEventEnumerationList <> nil) and
    (AIndex < FVisibleEventEnumerationList.Count) then
    Result := TcxSchedulerResourceViewInfo(FVisibleEventEnumerationList[AIndex])
  else
    Result := nil;
end;

function TcxSchedulerGanttViewViewInfo.HasVisibleEvents: Boolean;
var
  AResourceIndex, AEventCount: Integer;
begin
  AEventCount := 0;
  for AResourceIndex := 0 to ResourceCount - 1 do
    Inc(AEventCount, VisibleEventEnumerationList[AResourceIndex].VisibleEventCount);
  Result := AEventCount + GetEventWithoutResourceCount <> 0;
end;

function TcxSchedulerGanttViewViewInfo.IsGanttTaskVisible(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  AEvent := TcxSchedulerControlEvent(AEvent).Source;
  Result := (AEvent = nil) or (AEvent.ParentGroup = nil);
  if not Result then
  begin
    while (AEvent.ParentGroup <> nil) and AEvent.ParentGroup.GroupItems.Expanded and (AEvent <> AEvent.ParentGroup) do
      AEvent := AEvent.ParentGroup;
    Result := AEvent.ParentGroup = nil;
  end;
end;

function TcxSchedulerGanttViewViewInfo.IsGroup(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := AEvent.IsGroup;
end;

function TcxSchedulerGanttViewViewInfo.IsMilestone(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := AEvent.Duration = 0;
end;

function TcxSchedulerGanttViewViewInfo.IsTaskLinksExpanded(
  ATaskLinks: TcxSchedulerEventLinks): Boolean;
begin
  Result := not View.ShowExpandButtons or ATaskLinks.Expanded;
end;

function TcxSchedulerGanttViewViewInfo.NavigationButtonOffset(
  AKind: TcxSchedulerContentNavigationButtonKind;
  AResourceIndex: Integer): Integer;
begin
  AResourceIndex := Max(AResourceIndex, 0);
  if not TcxCustomSchedulerAccess(Scheduler).IsPopupScrollBars and (AKind = nbkNext) and
    ResourceViewShift[AResourceIndex].ScrollBar.Visible then
    Result := -ResourceViewShift[AResourceIndex].ScrollBar.Width
  else
    Result := 0;
end;

procedure TcxSchedulerGanttViewViewInfo.RecreateLinks;
begin
  Links.Clear;
  CreateLinks;
end;

procedure TcxSchedulerGanttViewViewInfo.SetHotLink(
  AHotLink: TcxSchedulerGanttViewLinkViewInfo);

  function GetRefreshRect(AOldLink, ANewLink: TcxSchedulerGanttViewLinkViewInfo): TRect;
  begin
    if ANewLink = nil then
      Result := AOldLink.ClipRect
    else
      Result := ANewLink.ClipRect;
  end;

var
  AOldLink: TcxSchedulerGanttViewLinkViewInfo;
begin
  if HotLink <> AHotLink then
  begin
    AOldLink := HotLink;
    FHotLink := AHotLink;
    View.RepaintRect(GetRefreshRect(AOldLink, HotLink));
  end;
end;

end.
