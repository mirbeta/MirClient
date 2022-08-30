{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars extended items                               }
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

unit dxBarExtItems;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, CommCtrl, Classes, Controls, Forms, Graphics,
  Dialogs, StdCtrls, ComCtrls, ImgList, SysUtils,
  dxBar, dxBarAccessibility, dxCommon, dxBarSkinConsts, cxControls, dxCore, dxForms, cxDateUtils, cxGeometry;

type
  TdxBarStatic = class(TdxBarItem)
  private
    FAlignment: TAlignment;
    FAllowClick: Boolean;
    FBorderStyle: TdxBarStaticBorderStyle;
    FHeight: Integer;
    FLeftIndent: Integer;
    FRightIndent: Integer;
    FWidth: Integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBorderStyle(Value: TdxBarStaticBorderStyle);
    procedure SetSizeValue(Index: Integer; Value: Integer);
  protected
    function CanClicked: Boolean; override;
    function HasAccel(AItemLink: TdxBarItemLink): Boolean; override;
    function IsStyleColorSupported: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AllowClick: Boolean read FAllowClick write FAllowClick default False;
    property BorderStyle: TdxBarStaticBorderStyle read FBorderStyle write SetBorderStyle
      default sbsNone;
    property Glyph;
    property Height: Integer index 4 read FHeight write SetSizeValue default 0;
    property ImageIndex;
    property LeftIndent: Integer index 1 read FLeftIndent write SetSizeValue default 0;
    property RightIndent: Integer index 2 read FRightIndent write SetSizeValue default 0;
    property ShowCaption default True;
    property Width: Integer index 3 read FWidth write SetSizeValue default 0;
    property OnClick;
  end;

  TdxBarStaticControl = class(TdxBarCustomStaticControl)
  private
    function GetBorderStyle: TdxBarStaticBorderStyle;
    function GetBorderOffsets: TRect;
    function GetItem: TdxBarStatic;
    function GetSizeValue(Index: Integer): Integer;
  protected
    procedure CalcDrawParams(AFull: Boolean = True); override;
    function CanClicked: Boolean; override;
    function CanHaveZeroSize: Boolean; virtual;
    function CanMouseSelect: Boolean; override;
    procedure CaptionChanged; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    procedure DrawInterior(ARect: TRect); override;
    function GetAlignment: TAlignment; virtual;
    function GetAutoHeight(ADefaultButtonSize: Integer): Integer;
    function GetAutoWidth(ADefaultButtonSize: Integer): Integer;
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    function InternalGetDefaultHeight: Integer; override;
    function InternalGetDefaultWidth: Integer; override;
    function CanDestroyOnClick: Boolean; override;

    property Alignment: TAlignment read GetAlignment;
    property BorderStyle: TdxBarStaticBorderStyle read GetBorderStyle;
    property BorderOffsets: TRect read GetBorderOffsets;
    property Height: Integer index 4 read GetSizeValue;
    property LeftIndent: Integer index 1 read GetSizeValue;
    property RightIndent: Integer index 2 read GetSizeValue;
    property Width: Integer index 3 read GetSizeValue;
  public
    property Item: TdxBarStatic read GetItem;
  end;

  TdxBarColorCombo = class(TdxBarCustomCombo, IdxLocalizerListener)
  private
    FAutoColor: TColor;
    FAutoColorText: string;
    FColor: TColor;
    FCustomColorText: string;
    FExchangeColor: TColor;
    FHasExchangeColor: Boolean;
    FInRefreshColorNames: Boolean;
    FIsAutoColorTextAssigned: Boolean;
    FIsCustomColorTextAssigned: Boolean;
    FSettingColor: Boolean;
    FShowAutoColor: Boolean;
    FShowCustomColorButton: Boolean;

    function GetCurColor: TColor;
    procedure SetAutoColor(Value: TColor);
    procedure SetAutoColorText(Value: string);
    procedure SetColor(Value: TColor);
    procedure SetCurColor(Value: TColor);
    procedure SetCustomColorText(Value: string);
    procedure SetShowAutoColor(Value: Boolean);
    procedure SetShowCustomColorButton(Value: Boolean);

    procedure CreateItemsList;
    function DefaultAutoColorText: string;
    function DefaultCustomColorText: string;
    function GetColorByIndex(AIndex: Integer): TColor;
    function GetIndexOfColor(AColor: TColor): Integer;
    function IsAutoColorTextStored: Boolean;
    function IsCustomColorTextStored: Boolean;
    function IsDropDownCountStored: Boolean;
  protected
    procedure Change; override;
    procedure DrawItem(APainter: TdxBarPainter; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState); override;
    procedure MeasureItem(AIndex: Integer; var AHeight: Integer); override;
    procedure MeasureItemWidth(AIndex: Integer; var AWidth: Integer); override;
    property ExchangeColor: TColor read FExchangeColor;
    property HasExchangeColor: Boolean read FHasExchangeColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick; override;
    procedure TranslationChanged;
    procedure RefreshColorNames;
    property CurColor: TColor read GetCurColor write SetCurColor;
  published
    property AutoColor: TColor read FAutoColor write SetAutoColor default clWindowText;
    property AutoColorText: string read FAutoColorText write SetAutoColorText
      stored IsAutoColorTextStored;
    property Color: TColor read FColor write SetColor;
    property CustomColorText: string read FCustomColorText write SetCustomColorText
      stored IsCustomColorTextStored;
    property DropDownCount stored IsDropDownCountStored;
    property ShowAutoColor: Boolean read FShowAutoColor write SetShowAutoColor default False;
    property ShowCustomColorButton: Boolean read FShowCustomColorButton
      write SetShowCustomColorButton default False;
    property ShowEditor default False;
    property Text stored False;
  end;

  TdxBarColorComboControl = class(TdxBarComboControl)
  private
    function GetDrawParams: TdxBarColorComboControlDrawParams;
    function GetItem: TdxBarColorCombo;
  protected
    procedure CalcDrawParams(AFull: Boolean = True); override;
    procedure CalcParts; override;
    procedure CorrectFrameRect(var ARect: TRect); override;
    function DrawSelected: Boolean; override;
    procedure DrawTextField; override;
    function GetDrawParamsClass: TdxBarItemControlDrawParamsClass; override;
    function GetPartCount: Integer; override;
    procedure PressedChanged; override;
    procedure WndProc(var Message: TMessage); override;
    property DrawParams: TdxBarColorComboControlDrawParams read GetDrawParams;
  public
    property Item: TdxBarColorCombo read GetItem;
  end;

  TdxBarFontNameCombo = class(TdxBarCustomCombo)
  protected
    procedure DrawItem(APainter: TdxBarPainter; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState); override;
    procedure LoadFontNames;
    procedure MeasureItemWidth(AIndex: Integer; var AWidth: Integer); override;
    procedure SetText(Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoClick; override;
  published
    property ShowEditor default False;
  end;

  TdxBarCalendarStyle = (cs3D, csFlat, csUltraFlat);

  TdxBarCustomCalendar = class(TCustomControl)
  private
    FDragDate: TDateTime;
    FFirstDate: TDateTime;
    FSelStart: TDateTime;
    FSelFinish: TDateTime;
    FStyle: TdxBarCalendarStyle;

    FOnDateTimeChanged: TNotifyEvent;

    function GetFlat: Boolean;
    function GetUltraFlat: Boolean;
    procedure SetStyle(Value: TdxBarCalendarStyle);

    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    function GetStyle: TdxBarCalendarStyle; virtual;
    function GetRealFirstDate: TDateTime; virtual;
    function GetRealLastDate: TDateTime; virtual;
    function GetLastDate: TDateTime; virtual; abstract;
    function GetSelStart: TDateTime; virtual;
    function GetSelFinish: TDateTime; virtual;
    procedure SetFirstDate(Value: TDateTime); virtual;
    procedure SetSelStart(Value: TDateTime); virtual;
    procedure SetSelFinish(Value: TDateTime); virtual;

    procedure CancelAll; dynamic;
    procedure CheckFirstDate; virtual; abstract;
    procedure DoDateTimeChanged; dynamic;
    procedure DoInternalSelectPeriod(ADate: TDateTime);
    function PosToDateTime(P: TPoint): TDateTime; virtual; abstract;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    property Flat: Boolean read GetFlat;
    property UltraFlat: Boolean read GetUltraFlat;

    property RealFirstDate: TDateTime read GetRealFirstDate;
    property RealLastDate: TDateTime read GetRealLastDate;
  public
    constructor Create(AOwner: TComponent); override;

    property FirstDate: TDateTime read FFirstDate write SetFirstDate;
    property LastDate: TDateTime read GetLastDate;
    property SelStart: TDateTime read GetSelStart write SetSelStart;
    property SelFinish: TDateTime read GetSelFinish write SetSelFinish;
    property Style: TdxBarCalendarStyle read GetStyle write SetStyle;
    property OnDateTimeChanged: TNotifyEvent read FOnDateTimeChanged
      write FOnDateTimeChanged;
  end;

  TdxBarDateCombo = class;

  TdxBarDateNavigator = class(TdxBarCustomCalendar)
  private
    FCombo: TdxBarDateCombo;
    FColCount: Integer;
    FRowCount: Integer;
    FColWidth, FSideWidth,
    FRowHeight, FHeaderHeight, FDaysOfWeekHeight: Integer;
    FTodayButtonWidth, FClearButtonWidth, FButtonsOffset, FButtonsHeight,
    FButtonsRegionHeight: Integer;
    FListBox: TWinControl;
    FListBoxDelta: Integer;
    FTimer: UINT;
    FTodayButtonActive, FTodayButtonPressed: Boolean;
    FClearButtonActive, FClearButtonPressed: Boolean;

    procedure CheckSelection(MarginDate: TDateTime);
    function ColOfDate(ADate: TDateTime): Integer;
    function GetHeaderRect: TRect;
    function GetInternalRect: TRect;
    function GetLeftArrowRect: TRect;
    function GetPainter: TdxBarPainter;
    function GetRightArrowRect: TRect;
    function GetMonthNameRect: TRect;
    function GetTodayButtonRect: TRect;
    function GetClearButtonRect: TRect;
    function GetShowButtonsArea: Boolean;
    procedure FreeTimer;
    procedure RepaintTodayButton;
    procedure RepaintClearButton;

    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    function GetStyle: TdxBarCalendarStyle; override;
    function GetRealFirstDate: TDateTime; override;
    function GetRealLastDate: TDateTime; override;
    function GetLastDate: TDateTime; override;
    procedure SetFirstDate(Value: TDateTime); override;
    procedure SetSelFinish(Value: TDateTime); override;
    procedure StepToPast;
    procedure StepToFuture;
    procedure CancelAll; override;
    procedure CheckFirstDate; override;
    procedure DeactivateAll;
    function PosToDateTime(P: TPoint): TDateTime; override;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;

    property Painter: TdxBarPainter read GetPainter;
    property ShowButtonsArea: Boolean read GetShowButtonsArea;
  public
    IsPopup: Boolean;
    ShowTodayButton, ShowClearButton: Boolean;
    constructor Create(AOwner: TComponent); override;
    procedure SetSize;
    function GetWidth: Integer;
    function GetHeight: Integer;
  end;

  TdxBarDateOnStart = (bdsToday, bdsNullDate, bdsCustom);

  TdxBarDateCombo = class(TCustomdxBarCombo)
  private
    FDateOnStart: TdxBarDateOnStart;
    FDatePopup, FDateNavigator: TdxBarDateNavigator;
    FDateEdit: TEdit;
    FForm: TForm;
    FInternalUpdate: Boolean;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FShowDayText: Boolean;
    FShowTodayButton: Boolean;
    FShowClearButton: Boolean;
    function GetCurDate: TDateTime;
    function GetDate: TDateTime;
    procedure SetCurDate(Value: TDateTime);
    procedure SetDate(Value: TDateTime);
    procedure DateChanged(Sender: TObject);
    procedure DialogClick(Sender: TObject);
    procedure DialogDateChanged(Sender: TObject);
    procedure DialogDateEditChange(Sender: TObject);
    function GetDateOfText(AText: string): TDateTime;
    function GetDateText(ADate: TDateTime): string;
    function IsMinDateStored: Boolean;
    function IsMaxDateStored: Boolean;
    function IsTextStored: Boolean;
    procedure SetDateOnStart(Value: TdxBarDateOnStart);
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
    procedure SetShowDayText(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure CheckDateOnStart;
    procedure CheckRange;
    function CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean; override;
    procedure CloseUp; override;
    procedure DropDown(X, Y: Integer); override;
    function GetDropDownWindow: HWND; override;
    procedure ResetDate;
    procedure SetText(Value: string); override;

    property DatePopup: TdxBarDateNavigator read FDatePopup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CheckDate(ADate: TDateTime): TDateTime;
    procedure DoClick; override;

    property CurDate: TDateTime read GetCurDate write SetCurDate;
    property Date: TDateTime read GetDate write SetDate;
  published
    property DateOnStart: TdxBarDateOnStart read FDateOnStart write SetDateOnStart default bdsToday;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored IsMaxDateStored;
    property MinDate: TDateTime read FMinDate write SetMinDate stored IsMinDateStored;
    property ShowTodayButton: Boolean read FShowTodayButton write FShowTodayButton default True;
    property ShowClearButton: Boolean read FShowClearButton write FShowClearButton default True;
    property ShowDayText: Boolean read FShowDayText write SetShowDayText default True;
    property Text stored IsTextStored;
  end;

  TdxBarDateComboControl = class(TCustomdxBarComboControl)
  private
    function GetDate: TDateTime;
    function GetItem: TdxBarDateCombo;
    procedure SetDate(const Value: TDateTime);
  protected
    procedure WndProc(var Message: TMessage); override;
    property Date: TDateTime read GetDate write SetDate;
  public
    property Item: TdxBarDateCombo read GetItem;
  end;

  TdxBarTreeViewCombo = class;

  TdxBarTreeView = class(TCustomTreeView)
  private
    FCloseButtonRect, FGripRect: TRect;
    FCloseButtonIsTracking: Boolean;
    FCombo: TdxBarTreeViewCombo;
    FCorner: TdxCorner;
    FMouseAboveCloseButton: Boolean;
    FMouseDownHitTestInfo: THitTests;
    function FindNode(const AText: string): TTreeNode;
    function GetPainter: TdxBarPainter;
    procedure SaveAndHide;
    procedure TVMSetImageList(var Message: TMessage); message TVM_SETIMAGELIST;
    procedure TVMSetItem(var Message: TMessage); message TVM_SETITEM;
    procedure WMCaptureChanged(var Message: TMessage); message WM_CAPTURECHANGED;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
  protected
    procedure Change(Node: TTreeNode); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateSizeGripCorner(ADropDownPosition: TPoint);
    property Painter: TdxBarPainter read GetPainter;
  public
    IsPopup: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    property Items;
    property OnDeletion;
    property Combo: TdxBarTreeViewCombo read FCombo;
  end;

  TdxBarTreeViewComboCanSelectNodeEvent = procedure(Sender: TdxBarTreeViewCombo;
    Node: TTreeNode; var CanSelect: Boolean) of object;

  TdxBarTreeViewCombo = class(TCustomdxBarCombo)
  private
    FAllowResizing: Boolean;
    FButtonOk, FButtonCancel: TButton;
    FChooseByDblClick: Boolean;
    FForm: TForm;
    FFormTreeView, FTreeView: TdxBarTreeView;
    FFullExpand: Boolean;
    FInSelectedNodeChanged: Boolean;
    FLoadedText: string;
    FSelectedNode: TTreeNode;
    FShowImageInEdit: Boolean;
    FOnCanSelectNode: TdxBarTreeViewComboCanSelectNodeEvent;

    function GetDropDownHeight: Integer;
    function GetDropDownWidth: Integer;
    function GetImages: TCustomImageList;
    function GetIndent: Integer;
    function GetItems: TTreeNodes;
    function GetShowButtons: Boolean;
    function GetShowLines: Boolean;
    function GetShowRoot: Boolean;
    function GetSortType: TSortType;
    function GetStateImages: TCustomImageList;
    function GetOnExpanded: TTVExpandedEvent;
    function GetOnExpanding: TTVExpandingEvent;
    function GetOnChanging: TTVChangingEvent;
    function GetOnCollapsed: TTVExpandedEvent;
    function GetOnCollapsing: TTVCollapsingEvent;
    function GetOnCompare: TTVCompareEvent;
    function GetOnGetImageIndex: TTVExpandedEvent;
    function GetOnGetSelectedIndex: TTVExpandedEvent;
    function GetOnTreeViewChange: TTVChangedEvent;

    procedure SetDropDownHeight(Value: Integer);
    procedure SetDropDownWidth(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetIndent(Value: Integer);
    procedure SetItems(Value: TTreeNodes);
    procedure SetSelectedNode(Value: TTreeNode);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowImageInEdit(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetOnExpanded(Value: TTVExpandedEvent);
    procedure SetOnExpanding(Value: TTVExpandingEvent);
    procedure SetOnChanging(Value: TTVChangingEvent);
    procedure SetOnCollapsed(Value: TTVExpandedEvent);
    procedure SetOnCollapsing(Value: TTVCollapsingEvent);
    procedure SetOnCompare(Value: TTVCompareEvent);
    procedure SetOnGetImageIndex(Value: TTVExpandedEvent);
    procedure SetOnGetSelectedIndex(Value: TTVExpandedEvent);
    procedure SetOnTreeViewChange(Value: TTVChangedEvent);

    procedure FormSize(Sender: TObject);
  protected
    procedure CheckDropDownPoint(var X, Y: Integer); override;
    function CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean; override;
    function DoCanSelectNode: Boolean;
    procedure DoSelectedNodeChanged; virtual;
    procedure DrawInterior(ABarEditControl: TdxBarEditControl; ACanvas: TCanvas;
      R: TRect; ItemLink: TdxBarItemLink); override;
    procedure DropDown(X, Y: Integer); override;
    function GetDropDownWindow: HWND; override;
    function HasImageInEdit: Boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetText(Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick; override;

    property SelectedNode: TTreeNode read FSelectedNode write SetSelectedNode;
    property TreeView: TdxBarTreeView read FTreeView;
  published
    property AllowResizing: Boolean read FAllowResizing write FAllowResizing default True;
    property ChooseByDblClick: Boolean read FChooseByDblClick write FChooseByDblClick default True;
    property DropDownHeight: Integer read GetDropDownHeight write SetDropDownHeight default 200;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth default 150;
    property FullExpand: Boolean read FFullExpand write FFullExpand default False;
    property Images: TCustomImageList read GetImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent;
    property Items: TTreeNodes read GetItems write SetItems;
    property ShowButtons: Boolean read GetShowButtons write SetShowButtons;
    property ShowEditor default False;
    property ShowImageInEdit: Boolean read FShowImageInEdit write SetShowImageInEdit
      default True;
    property ShowLines: Boolean read GetShowLines write SetShowLines;
    property ShowRoot: Boolean read GetShowRoot write SetShowRoot;
    property SortType: TSortType read GetSortType write SetSortType;
    property StateImages: TCustomImageList
      read GetStateImages write SetStateImages;

    property OnExpanded: TTVExpandedEvent read GetOnExpanded write SetOnExpanded;
    property OnExpanding: TTVExpandingEvent read GetOnExpanding write SetOnExpanding;
    property OnCanSelectNode: TdxBarTreeViewComboCanSelectNodeEvent read FOnCanSelectNode
      write FOnCanSelectNode;
    property OnChanging: TTVChangingEvent read GetOnChanging write SetOnChanging;
    property OnCollapsed: TTVExpandedEvent read GetOnCollapsed write SetOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read GetOnCollapsing write SetOnCollapsing;
    property OnCompare: TTVCompareEvent read GetOnCompare write SetOnCompare;
    property OnGetImageIndex: TTVExpandedEvent read GetOnGetImageIndex write SetOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read GetOnGetSelectedIndex write SetOnGetSelectedIndex;
    property OnTreeViewChange: TTVChangedEvent read GetOnTreeViewChange write SetOnTreeViewChange;
  end;

  TdxBarTreeViewComboControl = class(TCustomdxBarComboControl)
  private
    function GetItem: TdxBarTreeViewCombo;
  protected
    function GetDefaultHeight: Integer; override;
    procedure SetFocused(Value: Boolean); override;
  public
    property Item: TdxBarTreeViewCombo read GetItem;
  end;

  TdxBarImageCombo = class(TdxBarCustomCombo)
  private
    FDialogListBox: TListBox;
    FForm: TForm;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FShowText: Boolean;

    function GetImageIndexes(Index: Integer): Integer;
    procedure SetImageIndexes(Index: Integer; Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowText(Value: Boolean);
    procedure ImageListChange(Sender: TObject);
    procedure ReadImageIndexes(Reader: TReader);
    procedure WriteImageIndexes(Writer: TWriter);

    procedure DialogListBoxDblClick(Sender: TObject);
    procedure DialogListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure DialogListBoxMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawItem(APainter: TdxBarPainter; AIndex: Integer; ARect: TRect;
      AState: TOwnerDrawState); override;
    procedure ImagesChanged; virtual;
    procedure MeasureItem(AIndex: Integer; var AHeight: Integer); override;
    procedure MeasureItemWidth(AIndex: Integer; var AWidth: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick; override;
    property ImageIndexes[Index: Integer]: Integer read GetImageIndexes write SetImageIndexes;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Items;
    property ShowEditor default False;
    property Sorted;
    property ItemIndex;  // loading after all
    property ShowText: Boolean read FShowText write SetShowText default True;
  end;

  TdxBarImageComboControl = class(TdxBarComboControl)
  protected
    function GetDefaultHeight: Integer; override;
    procedure ImagesChanged; virtual;
  end;

  TdxBarToolbarsListItem = class(TCustomdxBarSubItem)
  protected
    function InternalCanMergeWith(AItem: TdxBarItem): Boolean; override;
  published
    //TCustomdxBarSubItem
    property BarSize;
    property Detachable;
    property DetachingBar;
    property Glyph;
    property ImageIndex;
    property LargeGlyph;
    property LargeImageIndex;
    property ShowCaption default True;
    property OnClick;
    property OnDetaching;
    property OnPaintBar;
  end;

  TdxBarToolbarsListItemControl = class(TdxBarSubItemControl)
  protected
    procedure CreateSubMenuControl; override;
  end;

  TdxBarSpinEdit = class;
  TdxBarSpinEditValueType = (svtInteger, svtFloat);
  TdxBarSpinEditPrefixPlace = (ppStart, ppEnd);
  TdxBarSpinEditButtonClickEvent = procedure(Sender: TdxBarSpinEdit;
    Button: TdxBarSpinEditButton) of object;

  TdxBarSpinEdit = class(TdxBarEdit)
  private
    FIncrement: Extended;
    FMaxValue: Extended;
    FMinValue: Extended;
    FPrefix: string;
    FPrefixPlace: TdxBarSpinEditPrefixPlace;
    FValueType: TdxBarSpinEditValueType;
    FOnButtonClick: TdxBarSpinEditButtonClickEvent;

    function GetCurValue: Extended;
    function GetIntCurValue: Integer;
    function GetIntValue: Integer;
    function GetValue: Extended;
    procedure SetCurValue(Value: Extended);
    procedure SetIncrement(Value: Extended);
    procedure SetIntCurValue(Value: Integer);
    procedure SetIntValue(Value: Integer);
    procedure SetMaxValue(Value: Extended);
    procedure SetMinValue(Value: Extended);
    procedure SetPrefix(const Value: string);
    procedure SetPrefixPlace(Value: TdxBarSpinEditPrefixPlace);
    procedure SetValue(Value: Extended);
    procedure SetValueType(Value: TdxBarSpinEditValueType);

    function IsIncrementStored: Boolean;
    function IsMaxValueStored: Boolean;
    function IsMinValueStored: Boolean;
    function IsValueStored: Boolean;

    procedure AddPrefix(var Text: string);
    procedure RemovePrefix(var Text: string);
  protected
    function CheckRange: Boolean;
    procedure DoButtonClick(Button: TdxBarSpinEditButton);
    function GetCheckedValue(Value: Extended): Extended;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure PrepareValue(var Value: Extended);
    function TextToValue(Text: string): Extended;
    procedure SetText(Value: string); override;
    function ValueToText(Value: Extended): string;
  public
    constructor Create(AOwner: TComponent); override;
    property CurValue: Extended read GetCurValue write SetCurValue;
    property IntCurValue: Integer read GetIntCurValue write SetIntCurValue;
    property IntValue: Integer read GetIntValue write SetIntValue;
  published
    property ValueType: TdxBarSpinEditValueType read FValueType write SetValueType
      default svtInteger;  // must be loaded before all
    property Increment: Extended read FIncrement write SetIncrement
      stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxValueStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinValueStored;
    property Prefix: string read FPrefix write SetPrefix;
    property PrefixPlace: TdxBarSpinEditPrefixPlace read FPrefixPlace write SetPrefixPlace
      default ppEnd;
    property Text stored False;
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property OnButtonClick: TdxBarSpinEditButtonClickEvent read FOnButtonClick
      write FOnButtonClick;
  end;

  TdxBarSpinEditControl = class(TdxBarEditControl)
  private
    FActiveButtonIndex: Integer;
    FPressedButtonIndex: Integer;
    FTimerID: UINT;
    function GetActiveButton: TdxBarSpinEditButton;
    function GetButtonPressed: Boolean;
    function GetDrawParams: TdxBarSpinEditDrawParams;
    function GetItem: TdxBarSpinEdit;
    procedure SetActiveButtonIndex(Value: Integer);
  protected
    procedure BreakProcess;
    procedure CalcDrawParams(AFull: Boolean = True); override;
    procedure CalcParts; override;
    procedure CorrectFrameRect(var ARect: TRect); override;
    procedure DrawTextField; override;
    function GetDrawParamsClass: TdxBarItemControlDrawParamsClass; override;
    function GetPartCount: Integer; override;
    procedure KillFocus(AHandle: THandle); override;
    procedure WndProc(var Message: TMessage); override;
    property ActiveButton: TdxBarSpinEditButton read GetActiveButton;
    property ActiveButtonIndex: Integer read FActiveButtonIndex write SetActiveButtonIndex;
    property ButtonPressed: Boolean read GetButtonPressed;
    property DrawParams: TdxBarSpinEditDrawParams read GetDrawParams;
  public
    property Item: TdxBarSpinEdit read GetItem;
  end;

  TdxBarControlContainerItem = class(TdxBarItem)
  strict private
    FControl: TControl;
    FIsControlRecreating: Boolean;
    FPlace: TdxCustomForm;
    FPrevControlSize: TPoint;
    FWindowProcObject: TcxWindowProcLinkedObject;

    function GetControlVisible: Boolean;
    function GetInPlaceControl: Boolean;
    procedure SetControl(Value: TControl);
    procedure ControlWndProc(var Message: TMessage);
    function IsControlAssigned(AControl: TControl): Boolean;
    procedure SaveControlSize;
    procedure SetControlVisible(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;

    function CanClicked: Boolean; override;
    function GetHidden: Boolean; override;
    function HasAccel(AItemLink: TdxBarItemLink): Boolean; override;
    procedure HideControl(AControl: TdxBarItemControl); override;
    function NeedToBeHidden: Boolean; override;

    property ControlVisible: Boolean read GetControlVisible write SetControlVisible;
    property InPlaceControl: Boolean read GetInPlaceControl;
    property Place: TdxCustomForm read FPlace;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Control: TControl read FControl write SetControl;
  end;

  { TdxBarControlContainerControl }

  TdxBarControlContainerControl = class(TdxBarItemControl)
  strict private
    FInPlaceControl: Boolean;
    FPlacedControl: Boolean;

    function GetControlDPI: Integer;
    function GetItem: TdxBarControlContainerItem;
    function GetPlace: TdxCustomForm;
    procedure InternalPaint;
    procedure PlaceControl;
  protected
    procedure BeforeDestroyParentHandle; override;
    procedure CalcParts; override;
    function CanClicked: Boolean; override;
    function CanSelect: Boolean; override;
    function CanMouseSelect: Boolean; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    function GetControl: TControl; override;
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetFocused: Boolean; override;
    function GetPossibleViewLevels: TdxBarItemViewLevels; override;
    function CanDestroyOnClick: Boolean; override;
    function IsShowingControl: Boolean;
    function NeedCaptureMouse: Boolean; override;
    function NeedUpdateWhenResize: Boolean; override;
    procedure RealVisibleChanging(AVisible: Boolean); override;
    function ShowsControl: Boolean;

    property ControlDPI: Integer read GetControlDPI;
    property InPlaceControl: Boolean read FInPlaceControl;
    property Place: TdxCustomForm read GetPlace;
  public
    destructor Destroy; override;
    function HasWindow: Boolean; override;
    //
    property Item: TdxBarControlContainerItem read GetItem;
  end;

  TdxBarProgressItem = class(TdxBarStatic)
  private
    FColor: TColor;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FSmooth: Boolean;
    FStep: Integer;
    procedure SetColor(Value: TColor);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetSmooth(Value: Boolean);
    procedure SetStep(Value: Integer);
  protected
    procedure UpdateBar;
    procedure UpdateDifference;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(AMin, AMax: Integer);
    procedure StepBy(Delta: Integer);
    procedure StepIt;
  published
    property BorderStyle default sbsLowered;
    property Color: TColor read FColor write SetColor default clDefault;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition default 0;
    property Smooth: Boolean read FSmooth write SetSmooth default False;
    property Step: Integer read FStep write SetStep default 10;
  end;

  TdxBarProgressControl = class(TdxBarStaticControl)
  private
    FPrevPos: Integer;
    function GetDrawParams: TdxBarProgressControlDrawParams;
    function GetItem: TdxBarProgressItem;
    function CalculateBandDrawPosition(const ABandRect: TRect): Integer;
  protected
    function BarBrushColor: TColorRef; virtual;
    function BarHeight: Integer;
    function BarRect: TRect;
    function BarWidth: Integer;
    procedure CalcDrawParams(AFull: Boolean); override;
    function CanHaveZeroSize: Boolean; override;
    procedure DrawInterior(ARect: TRect); override;
    function GetAlignment: TAlignment; override;
    function GetDrawParamsClass: TdxBarItemControlDrawParamsClass; override;
    function GetPossibleViewLevels: TdxBarItemViewLevels; override;
    function InternalGetDefaultHeight: Integer; override;
    function InternalGetDefaultWidth: Integer; override;
    procedure UpdateBar;
    procedure UpdateDifference;
    property DrawParams: TdxBarProgressControlDrawParams read GetDrawParams;
  public
    property Item: TdxBarProgressItem read GetItem;
  end;

  TdxBarMRUListItem = class(TdxBarListItem)
  private
    FMaxItemCount: Integer;
    FRemoveItemOnClick: Boolean;
    procedure SetMaxItemCount(Value: Integer);
  protected
    procedure CheckItemCount;
    function GetDisplayText(const AText: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DirectClick; override;
    procedure AddItem(const S: string; AObject: TObject);
    procedure RemoveItem(const S: string; AObject: TObject);
  published
    property MaxItemCount: Integer read FMaxItemCount write SetMaxItemCount default 5;
    property RemoveItemOnClick: Boolean read FRemoveItemOnClick
      write FRemoveItemOnClick default False;
  end;

  TdxBarInPlaceSubItem = class;

  TdxBarInPlaceSubItemEvent =
    procedure(Sender: TdxBarInPlaceSubItem; Link: TdxBarItemLink) of object;

  TdxBarInPlaceSubItem = class(TdxBarContainerItem)
  private
    FExpanded: Boolean;
    FExpandedChanging: Boolean;
    FKeepBeginGroupWhileExpanded: Boolean;
    FOnAfterExpand: TdxBarInPlaceSubItemEvent;
    FOnBeforeCollapse: TdxBarInPlaceSubItemEvent;
    procedure SetExpanded(Value: Boolean);
  protected
    procedure DeleteListedItemLinks(ALinkData: TObject); override;
    function HideWhenRun: Boolean; override;
    function InternalActuallyVisible: Boolean; override;
    function InternalCanMergeWith(AItem: TdxBarItem): Boolean; override;
    procedure PopulateListedItemLinks(AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer); override;
    procedure PrepareListedItemLinks; override;

    procedure ChangeNextItemLinkBeginGroup(ALink: TdxBarItemLink; Value: Boolean);
    procedure DoAfterExpand(ALink: TdxBarItemLink); dynamic;
    procedure DoBeforeCollapse(ALink: TdxBarItemLink); dynamic;
  published
    property Expanded: Boolean read FExpanded write SetExpanded default False;
    property KeepBeginGroupWhileExpanded: Boolean read FKeepBeginGroupWhileExpanded
      write FKeepBeginGroupWhileExpanded;
    property OnAfterExpand: TdxBarInPlaceSubItemEvent read FOnAfterExpand
      write FOnAfterExpand;
    property OnBeforeCollapse: TdxBarInPlaceSubItemEvent read FOnBeforeCollapse
      write FOnBeforeCollapse;
  end;

  TdxBarInPlaceSubItemControl = class(TdxBarContainerItemControl)
  private
    function GetDrawParams: TdxBarInPlaceSubItemControlDrawParams;
    function GetItem: TdxBarInPlaceSubItem;
  protected
    procedure CalcDrawParams(AFull: Boolean = False); override;
    procedure ControlClick(AByMouse: Boolean; AKey: Char = #0); override;
    procedure DblClick; override;
    procedure DoPaint(ARect: TRect; PaintType: TdxBarPaintType); override;
    function GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass; override;
    function GetDefaultHeight: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetDrawParamsClass: TdxBarItemControlDrawParamsClass; override;
    function HasSubMenu: Boolean; override;
    function IsExpandable: Boolean; override;
    function IsInvertTextColor: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function WantsKey(Key: Word): Boolean; override;

    property DrawParams: TdxBarInPlaceSubItemControlDrawParams read GetDrawParams;
  public
    property Item: TdxBarInPlaceSubItem read GetItem;
  end;

  { TdxBarInPlaceSubItemControlAccessibilityHelper }

  TdxBarInPlaceSubItemControlAccessibilityHelper = class(TdxBarSubItemControlAccessibilityHelper)
  protected
    function ShowDropDownWindow: Boolean; override;
  end;

// Use cxSetResourceString instead global variable
// for example, old code:
//   sdxBarDatePopupToday := ...
// new code:
//   cxSetResourceString(@dxSBAR_DATETODAY, ...);
function sdxBarDatePopupToday: string;
function sdxBarDatePopupClear: string;

function dxBarColorDialog: TColorDialog;
function dxBarFontDialog: TFontDialog;

implementation

{$R dxBarExtItems.res}

uses
  Math, ActiveX, Printers, dxBarStrs,
  cxClasses, cxGraphics, cxFormats, cxLookAndFeels, dxDPIAwareUtils;

const
  Colors: array[0..15] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clGray,
     clSilver, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
  ADateNavigatorTime = 170;
  ProgressBarDefaultWidth = 150;
  ProgressBarIndent = 2;

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TBarManagerAccess = class(TdxBarManager);
  TCustomdxBarControlAccess = class(TCustomdxBarControl);

  TPlaceForm = class(TdxCustomForm,
    IdxPlaceForm,
    IdxSkinSupport,
    IdxSkinSupport2)
  strict private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FBarItemControl: TdxBarItemControl;
    // IdxSkinSupport2
    function IsSkinnable: Boolean;
  public
    procedure AfterConstruction; override;
  end;

var
  FColorDialog: TColorDialog;
  FFontDialog: TFontDialog;
  FTrueTypeFontBitmap, FNonTrueTypeFontBitmap: TBitmap;

function sdxBarDatePopupToday: string;
begin
  Result := cxGetResourceString(@dxSBAR_DATETODAY);
end;

function sdxBarDatePopupClear: string;
begin
  Result := cxGetResourceString(@dxSBAR_DATECLEAR);
end;

procedure TPlaceForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  APrevWindowOrg: TPoint;
  R: TRect;
begin
  if FBarItemControl = nil then
    FillRect(Message.DC, ClientRect, Brush.Handle)
  else
  begin
    R := ClientRect;
    BeforeDrawBackground(Self, FBarItemControl.Parent, Message.DC, R, APrevWindowOrg);
    try
      cxPaintCanvas.BeginPaint(Message.DC);
      try
        FBarItemControl.Painter.DrawItemBackground(FBarItemControl, cxPaintCanvas, R, Brush.Handle);
      finally
        cxPaintCanvas.EndPaint;
      end;
    finally
      AfterDrawBackground(Message.DC, APrevWindowOrg);
    end;
  end;
  Message.Result := 1;
end;

procedure TPlaceForm.AfterConstruction;
begin
  inherited;
  Scaled := False;
  RightToLeftLayout := bFalse;
end;

function TPlaceForm.IsSkinnable: Boolean;
begin
  Result := False;
end;

{ TdxBarStatic }

constructor TdxBarStatic.Create(AOwner: TComponent);
begin
  inherited;
  FAlignment := taCenter;
  FShowCaption := True;
end;

procedure TdxBarStatic.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if not IsLoading then Update;
  end;
end;

procedure TdxBarStatic.SetBorderStyle(Value: TdxBarStaticBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if not IsLoading then
      if (Width = 0) or (Height = 0) then
        UpdateEx
      else
        Update;
  end;
end;

procedure TdxBarStatic.SetSizeValue(Index: Integer; Value: Integer);
var
  PrevValue: Integer;
begin
  if Value < 0 then Value := 0;
  case Index of
    1: PrevValue := FLeftIndent;
    2: PrevValue := FRightIndent;
    3: PrevValue := FWidth;
    4: PrevValue := FHeight;
  else
    PrevValue := 0;
  end;
  if PrevValue <> Value then
  begin
    case Index of
      1: FLeftIndent := Value;
      2: FRightIndent := Value;
      3: FWidth := Value;
      4: FHeight := Value;
    end;
    if not IsLoading then UpdateEx;
  end;
end;

function TdxBarStatic.CanClicked: Boolean;
begin
  Result := FAllowClick;
end;

function TdxBarStatic.HasAccel(AItemLink: TdxBarItemLink): Boolean;
begin
  Result := False;
end;

function TdxBarStatic.IsStyleColorSupported: Boolean;
begin
  Result := True;
end;

{ TdxBarStaticControl }

function TdxBarStaticControl.GetBorderStyle: TdxBarStaticBorderStyle;
begin
  Result := Item.BorderStyle;
end;

function TdxBarStaticControl.GetBorderOffsets: TRect;
begin
  Result := Painter.StaticControlGetBorderOffsets(Parent, BorderStyle);
end;

function TdxBarStaticControl.GetItem: TdxBarStatic;
begin
  Result := TdxBarStatic(ItemLink.Item);
end;

function TdxBarStaticControl.GetSizeValue(Index: Integer): Integer;
begin
  case Index of
    1: Result := Item.LeftIndent;
    2: Result := Item.RightIndent;
    3: Result := Item.Width;
    4: Result := Item.Height;
  else
    Result := 0;
  end;
end;

function TdxBarStaticControl.CanHaveZeroSize: Boolean;
begin
  Result := False;
end;

procedure TdxBarStaticControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited;
  if AFull then
  begin
    DrawParams.Alignment := Alignment;
    DrawParams.AllowCenter := True;
    DrawParams.BorderOffsets := BorderOffsets;
    DrawParams.BorderStyle := BorderStyle;
  end;
end;

function TdxBarStaticControl.CanClicked: Boolean;
begin
  Result := Item.AllowClick;
end;

function TdxBarStaticControl.CanMouseSelect: Boolean;
begin
  Result := not IsSelectionForbidden;
end;

procedure TdxBarStaticControl.CaptionChanged;
begin
  if Width = 0 then
    inherited
  else
    Repaint;
end;

procedure TdxBarStaticControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  Painter.DrawStaticLikeControl(DrawParams, ARect, cxRect(LeftIndent, LeftIndent, RightIndent, RightIndent));
end;

procedure TdxBarStaticControl.DrawInterior(ARect: TRect);
begin
  Painter.DrawStaticGlyphAndCaption(DrawParams, ARect);
end;

function TdxBarStaticControl.GetAlignment: TAlignment;
begin
  Result := Item.Alignment;
end;

function TdxBarStaticControl.GetAutoHeight(ADefaultButtonSize: Integer): Integer;
begin
  if Parent.Kind = bkSubMenu then
    Result := GetTextSize
  else
  begin
    if cpText in DrawParams.ViewStructure then
    begin
      if cpIcon in DrawParams.ViewStructure then
        Result := Max(ADefaultButtonSize, GetTextSize - 3)
      else
        Result := GetTextSize - 3;
    end
    else
    begin
      if (cpIcon in DrawParams.ViewStructure) or not CanHaveZeroSize then
        Result := ADefaultButtonSize
      else
        Result := GetTextSize - 3;
    end;
  end;
  Inc(Result, BorderOffsets.Top);
  Inc(Result, BorderOffsets.Bottom);
end;

function TdxBarStaticControl.GetAutoWidth(ADefaultButtonSize: Integer): Integer;
begin
  if cpText in DrawParams.ViewStructure then
  begin
    if (cpIcon in DrawParams.ViewStructure) then
      if DrawParams.PaintType <> ptMenu then
        Result := ADefaultButtonSize + 4 + GetCaptionWidth
      else
        Result := Painter.SubMenuControlGetItemTextIndent(DrawParams) + GetCaptionWidth + 4
    else
      Result := GetCaptionWidth + Parent.TextSize div 2;
  end
  else
  begin
    if (cpIcon in DrawParams.ViewStructure) or not CanHaveZeroSize then
      Result := ADefaultButtonSize
    else
      Result := 0;
  end;

  Inc(Result, LeftIndent + RightIndent + BorderOffsets.Left + BorderOffsets.Right);
end;

function TdxBarStaticControl.GetDefaultHeight: Integer;
begin
  if IsRotated then
    Result := InternalGetDefaultWidth
  else
    Result := InternalGetDefaultHeight;
end;

function TdxBarStaticControl.GetDefaultWidth: Integer;
begin
  if IsRotated then
    Result := InternalGetDefaultHeight
  else
    Result := InternalGetDefaultWidth;
end;

function TdxBarStaticControl.InternalGetDefaultHeight: Integer;
begin
  if Height = 0 then
    Result := GetAutoHeight(GetRotationDependentHeight(DrawParams.DefaultButtonSize))
  else
    Result := Height;
end;

function TdxBarStaticControl.InternalGetDefaultWidth: Integer;
begin
  if Width = 0 then
    Result := GetAutoWidth(GetRotationDependentWidth(DrawParams.DefaultButtonSize))
  else
    Result := Width;
end;

function TdxBarStaticControl.CanDestroyOnClick: Boolean;
begin
  Result := Item.AllowClick;
end;

{ TdxBarColorCombo }

constructor TdxBarColorCombo.Create(AOwner: TComponent);
begin
  inherited;
  FAutoColor := clWindowText;
  FAutoColorText := DefaultAutoColorText;
  FCustomColorText := DefaultCustomColorText;
  DropDownCount := 16;
  Glyph.LoadFromResource(HInstance, 'DXBARCOLORCOMBO', RT_BITMAP);
  CreateItemsList;
  ItemIndex := 0;
  ShowEditor := False;
  dxResourceStringsRepository.AddListener(Self);
end;

destructor TdxBarColorCombo.Destroy;
begin
  dxResourceStringsRepository.RemoveListener(Self);
  inherited;
end;

function TdxBarColorCombo.GetCurColor: TColor;
begin
  Result := GetColorByIndex(CurItemIndex);
end;

procedure TdxBarColorCombo.SetAutoColor(Value: TColor);
begin
  if FAutoColor <> Value then
  begin
    FAutoColor := Value;
    if FShowAutoColor then
    begin
      FSettingColor := True;
      try
        ItemIndex := GetIndexOfColor(FColor);
      finally
        FSettingColor := False;
        Update;
      end;
    end;
  end;
end;

procedure TdxBarColorCombo.SetAutoColorText(Value: string);
begin
  if FAutoColorText <> Value then
  begin
    FAutoColorText := Value;
    FIsAutoColorTextAssigned := Value <> DefaultAutoColorText;
    if FShowAutoColor then
    begin
      Items[0] := Value;
      Update;
    end;
  end;
end;

procedure TdxBarColorCombo.SetColor(Value: TColor);
var
  AIndex: Integer;
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FSettingColor := True;
    try
      AIndex := GetIndexOfColor(FColor);
      if ItemIndex = AIndex then
      begin
        Update;
        Change;
      end
      else ItemIndex := AIndex;
    finally
      FSettingColor := False;
    end;
  end;
end;

procedure TdxBarColorCombo.SetCurColor(Value: TColor);
begin
  if CurColor <> Value then
    CurItemIndex := GetIndexOfColor(Value);
end;

procedure TdxBarColorCombo.SetCustomColorText(Value: string);
begin
  if FCustomColorText <> Value then
  begin
    FCustomColorText := Value;
    FIsCustomColorTextAssigned := Value <> DefaultCustomColorText;
    Update;
  end;
end;

procedure TdxBarColorCombo.SetShowAutoColor(Value: Boolean);
begin
  if FShowAutoColor <> Value then
  begin
    FShowAutoColor := Value;
    FSettingColor := True;
    try
      if Value then Items.Insert(0, FAutoColorText)
      else Items.Delete(0);
      if DropDownCount = Byte(not Value) + 16 then
        DropDownCount := Byte(Value) + 16;
      ItemIndex := GetIndexOfColor(FColor);
    finally
      FSettingColor := False;
      Update;
    end;
  end;
end;

procedure TdxBarColorCombo.SetShowCustomColorButton(Value: Boolean);
begin
  if FShowCustomColorButton <> Value then
  begin
    FShowCustomColorButton := Value;
    Recalculate;
  end;
end;

procedure TdxBarColorCombo.CreateItemsList;
begin
  with Items do
  begin
    Clear;
    if FShowAutoColor then Add(FAutoColorText);
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_0));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_1));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_2));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_3));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_4));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_5));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_6));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_7));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_8));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_9));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_10));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_11));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_12));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_13));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_14));
    Add(cxGetResourceString(@dxSBAR_COLOR_STR_15));
  end;
end;

function TdxBarColorCombo.DefaultAutoColorText: string;
begin
  Result := cxGetResourceString(@dxSBAR_COLORAUTOTEXT);
end;

function TdxBarColorCombo.DefaultCustomColorText: string;
begin
  Result := cxGetResourceString(@dxSBAR_COLORCUSTOMTEXT);
end;

function TdxBarColorCombo.GetColorByIndex(AIndex: Integer): TColor;
begin
  if (0 <= AIndex) and (AIndex < 16 + Byte(FShowAutoColor)) then
    if FShowAutoColor and (AIndex = 0) then
      Result := FAutoColor
    else
      Result := Colors[AIndex - Byte(FShowAutoColor)]
  else Result := FColor;
end;

function TdxBarColorCombo.GetIndexOfColor(AColor: TColor): Integer;
begin
  if FShowAutoColor and (AColor = FAutoColor) then
    Result := 0
  else
  begin
    AColor := ColorToRGB(AColor);
    for Result := Low(Colors) + Byte(FShowAutoColor) to High(Colors) + Byte(FShowAutoColor) do
      if ColorToRGB(Colors[Result - Byte(FShowAutoColor)]) = AColor then Exit;
    Result := -1;
  end;
end;

function TdxBarColorCombo.IsAutoColorTextStored: Boolean;
begin
  Result := FIsAutoColorTextAssigned;
end;

function TdxBarColorCombo.IsCustomColorTextStored: Boolean;
begin
  Result := FIsCustomColorTextAssigned;
end;

function TdxBarColorCombo.IsDropDownCountStored: Boolean;
begin
  Result := DropDownCount <> Byte(FShowAutoColor) + 16;
end;

procedure TdxBarColorCombo.Change;
begin
  if not FSettingColor then
    FColor := GetColorByIndex(ItemIndex);
  if not FInRefreshColorNames then
    inherited;
end;

procedure TdxBarColorCombo.DrawItem(APainter: TdxBarPainter;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  PrevBrushColor: TColor;
  S: string;
  R: TRect;
  HasIndent: Boolean;
begin
  if Assigned(OnDrawItem) then
    inherited
  else
    with Canvas, ARect do
    begin
      PrevBrushColor := Brush.Color;
      if AIndex = -1 then
        S := FCustomColorText
      else
        S := Items[AIndex];
      R := ARect;
      HasIndent := (AIndex <> -1) or (FColor >= 0) or (FColor <= clInfoBk);
      if HasIndent then Inc(R.Left, 30);
      FillRect(R);
      TextOut(R.Left + 1, (Top + Bottom - TextHeight(S)) div 2, S);
      if HasIndent then
      begin
        R.Right := R.Left;
        R.Left := Left;
        FrameRect(R);
        InflateRect(R, -1, -1);
        Brush.Color := clBtnShadow;
        FrameRect(R);
        InflateRect(R, -1, -1);
        Brush.Color := GetColorByIndex(AIndex);
        FillRect(R);
      end;
      Brush.Color := PrevBrushColor;
      if odFocused in AState then Windows.DrawFocusRect(Handle, ARect); // for hiding focus rect
    end;
end;

procedure TdxBarColorCombo.MeasureItem(AIndex: Integer; var AHeight: Integer);
begin
  if Assigned(OnMeasureItem) then
    inherited MeasureItem(AIndex, AHeight)
  else
    AHeight := 2 + Canvas.TextHeight('0') + 2;
end;

procedure TdxBarColorCombo.MeasureItemWidth(AIndex: Integer; var AWidth: Integer);
begin
  inherited;
  Inc(AWidth, 30);
end;

procedure TdxBarColorCombo.DoClick;
begin
  try
    inherited;
    if not Assigned(OnClick) and not ReadOnly then
      with dxBarColorDialog do
      begin
        if FHasExchangeColor then
          Color := FExchangeColor
        else
          Color := Self.Color;
        if Execute then Self.Color := Color;
      end;
  finally
    FHasExchangeColor := False;
  end;
end;

procedure TdxBarColorCombo.TranslationChanged;
begin
  if not FIsAutoColorTextAssigned then
    FAutoColorText := DefaultAutoColorText;
  if not FIsCustomColorTextAssigned then
    FCustomColorText := DefaultCustomColorText;
  RefreshColorNames;
end;

procedure TdxBarColorCombo.RefreshColorNames;
var
  APrevItemIndex: Integer;
begin
  APrevItemIndex := ItemIndex;
  FInRefreshColorNames := True;
  try
    CreateItemsList;
    ItemIndex := APrevItemIndex;
  finally
    FInRefreshColorNames := False;
  end;
  Update;
end;

{ TdxBarColorComboControl }

function TdxBarColorComboControl.GetDrawParams: TdxBarColorComboControlDrawParams;
begin
  Result := TdxBarColorComboControlDrawParams(FDrawParams);
end;

function TdxBarColorComboControl.GetItem: TdxBarColorCombo;
begin
  Result := TdxBarColorCombo(ItemLink.Item);
end;

procedure TdxBarColorComboControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited;
  if AFull then
    DrawParams.IsPressed := Pressed;
  DrawParams.IsShowCustomColorButton := Item.ShowCustomColorButton;
end;

procedure TdxBarColorComboControl.CalcParts;
begin
  inherited;
  Painter.CalculateColorComboParts(DrawParams, FParts, FAreaParts);
end;

procedure TdxBarColorComboControl.CorrectFrameRect(var ARect: TRect);
begin
  inherited;
  Painter.ColorComboCorrectFrameRect(DrawParams, ARect);
end;

function TdxBarColorComboControl.DrawSelected: Boolean;
begin
  Result := inherited DrawSelected or Pressed;
end;

procedure TdxBarColorComboControl.DrawTextField;

  procedure DrawCustomButton;
  var
    AIndentsRect, AAdjacentZoneRect: TRect;
  begin
    if Painter.IsCustomColorButtonVisible(DrawParams) then
    begin
      AIndentsRect := Painter.GetCustomColorButtonIndents(DrawParams.PaintType);
      AAdjacentZoneRect := cxRectInflate(FParts[clcpCustomColorButton],
        AIndentsRect.Left, AIndentsRect.Top, AIndentsRect.Right, AIndentsRect.Bottom);

      // TODO: Regions
      Canvas.SaveClipRegion;
      Canvas.SetClipRegion(TcxRegion.Create(AAdjacentZoneRect), roSet);
      Painter.ColorComboDrawCustomButtonAdjacentZone(DrawParams, AAdjacentZoneRect);
      Canvas.RestoreClipRegion;

      Painter.ColorComboDrawCustomButton(DrawParams, FParts[clcpCustomColorButton]);
    end;
  end;

begin
  inherited;
  DrawCustomButton;
end;

function TdxBarColorComboControl.GetDrawParamsClass: TdxBarItemControlDrawParamsClass;
begin
  Result := TdxBarColorComboControlDrawParams;
end;

function TdxBarColorComboControl.GetPartCount: Integer;
begin
  Result := inherited GetPartCount + 1;
end;

procedure TdxBarColorComboControl.PressedChanged;
begin
  Repaint;
  if Pressed then
    DroppedDown := False;
end;

procedure TdxBarColorComboControl.WndProc(var Message: TMessage);
var
  AAllowPressed: Boolean;
  ARealItemLink: TdxBarItemLink;
  AItem: TdxBarColorCombo;
  ALinkSelf: TcxObjectLink;
begin
  with Message do
    if (Msg = WM_LBUTTONDOWN) and (FHotPartIndex = clcpCustomColorButton) or
      (Msg = WM_KEYDOWN) and (wParam = VK_RETURN) and (GetKeyState(VK_CONTROL) < 0) then
    begin
      AItem := Item;
      AItem.FHasExchangeColor := True;
      AItem.FExchangeColor := AItem.CurColor;
      AAllowPressed := CanVisuallyPressed;
      ARealItemLink := ItemLink.RealItemLink;
      if ARealItemLink <> nil then
        ARealItemLink.BringToTopInRecentList(True);
      if AAllowPressed then
        Pressed := True
      else
        Parent.HideAll;

      ALinkSelf := cxAddObjectLink(Self);
      try
        try
          AItem.DirectClick;
        finally
          if (ALinkSelf.Ref <> nil) and AAllowPressed then
            Pressed := False;
        end;
      finally
        cxRemoveObjectLink(ALinkSelf);
      end;
    end
    else
      inherited;
end;

{ TdxBarFontNameCombo }

constructor TdxBarFontNameCombo.Create(AOwner: TComponent);
begin
  inherited;
  DropDownCount := 12;
  Glyph.LoadFromResource(HInstance, 'DXBARFONTNAMECOMBO', RT_BITMAP);
  ShowEditor := False;
  Sorted := True;
  LoadFontNames;
  Width := 160;
end;

procedure TdxBarFontNameCombo.DrawItem(APainter: TdxBarPainter;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  W, H: Integer;
  CurrentBitmap: TBitmap;
  R: TRect;
  S: string;
begin
  if Assigned(OnDrawItem) or (AIndex = -1) then
    inherited
  else
    with Canvas, ARect do
    begin
      if Boolean(Items.Objects[AIndex]) then
        CurrentBitmap := FTrueTypeFontBitmap
      else
        CurrentBitmap := FNonTrueTypeFontBitmap;
      W := CurrentBitmap.Width;
      H := CurrentBitmap.Height;
      R := Bounds(Left, (Top + Bottom - H) div 2, W, H);
      TransparentDraw(Handle, Brush.Handle, ARect, R, CurrentBitmap, nil, -1, clNone,
        True, False, False, False, False, False, False, False{Faded}, True);

      S := Items[AIndex];
      TextOut(R.Right + 2, (Top + Bottom - TextHeight(S)) div 2, S);
      if odFocused in AState then Windows.DrawFocusRect(Handle, ARect); // for hiding focus rect
    end;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).AddObject(LogFont.lfFaceName, Pointer(FontType and TRUETYPE_FONTTYPE <> 0));
  Result := 1;
end;

procedure TdxBarFontNameCombo.LoadFontNames;
var
  DC: HDC;
  AIsWindowDC: Boolean;

  procedure EnumFonts(DC: HDC);
  var
    LogFont: TLogFont;
  begin
    with LogFont do
    begin
      lfCharset := DEFAULT_CHARSET;
      lfFaceName := '';
      lfPitchAndFamily := 0;
    end;
    EnumFontFamiliesEx(DC, LogFont, @EnumFontsProc, LPARAM(Items), 0);
  end;

begin
  if Printer.Printers.Count = 0 then
    DC := 0
  else
    try
      DC := Printer.Handle;
    except
      DC := 0;
    end;
  AIsWindowDC := DC = 0;
  if AIsWindowDC then DC := GetDC(0);
  try
    EnumFonts(DC);
  finally
    if AIsWindowDC then ReleaseDC(0, DC);
  end;
end;

procedure TdxBarFontNameCombo.MeasureItemWidth(AIndex: Integer; var AWidth: Integer);
begin
  inherited;
  Inc(AWidth, FTrueTypeFontBitmap.Width + 1);
end;

procedure TdxBarFontNameCombo.SetText(Value: string);
var
  AIndex: Integer;
begin
  if CurItemLink <> nil then
  begin
    AIndex := GetNearestItemIndex(Value);
    if (AIndex = -1) and (Value <> '') then Exit;
    if AIndex > -1 then Value := Items[AIndex];
  end;
  inherited;
end;

procedure TdxBarFontNameCombo.DoClick;
begin
  inherited;
  if not Assigned(OnClick) and not ReadOnly then
    with dxBarFontDialog do
    begin
      Font.Name := Text;
      if Execute then Text := Font.Name;
    end;
end;

function dxBarColorDialog: TColorDialog;
begin
  if FColorDialog = nil then
    FColorDialog := TColorDialog.Create(nil);
  Result := FColorDialog;
end;

function dxBarFontDialog: TFontDialog;
begin
  if FFontDialog = nil then
    FFontDialog := TFontDialog.Create(nil);
  Result := FFontDialog;
end;

{ TAMonthListBox }

type
  TAMonthListBox = class(TCustomControl)
  private
    FTopDate: TDateTime;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FItems: TStrings;
    FTimer: UINT;
    FTimerId: UINT;

    procedure FreeTimer;

    function GetDate: TDateTime;
    procedure SetItemIndex(Value: Integer);
    procedure SetTopDate(Value: TDateTime);

    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    property ItemHeight: Integer read FItemHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Items: TStrings read FItems;
    property TopDate: TDateTime read FTopDate write SetTopDate;
  public
    constructor Create(AOwner: TComponent); override;

    property Date: TDateTime read GetDate;
  end;

constructor TAMonthListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTopDate := NullDate;
end;

procedure TAMonthListBox.FreeTimer;
begin
  dxKillTimer(Handle, FTimerId);
end;

function TAMonthListBox.GetDate: TDateTime;
var
  Year, Month, Day: Word;
begin
  if ItemIndex = -1 then Result := NullDate
  else
  begin
    DecodeDate(TopDate, Year, Month, Day);
    dxChangeMonth(Year, Month, ItemIndex);
    Result := EncodeDate(Year, Month, 1);
  end;
end;

procedure TAMonthListBox.SetItemIndex(Value: Integer);
var
  PrevItemIndex: Integer;

  procedure InvalidateItemRect(Index: Integer);
  var
    R: TRect;
  begin
    if Index = -1 then Exit;
    with R do
    begin
      Left := 0;
      Top := Index * ItemHeight;
      Right := ClientWidth;
      Bottom := Top + ItemHeight;
    end;
    InvalidateRect(Handle, @R, False);
  end;

begin
  if FItemIndex <> Value then
  begin
    PrevItemIndex := FItemIndex;
    FItemIndex := Value;
    InvalidateItemRect(PrevItemIndex);
    InvalidateItemRect(FItemIndex);
  end;
end;

procedure TAMonthListBox.SetTopDate(Value: TDateTime);
begin
  if FTopDate <> Value then
  begin
    FTopDate := Value;
    Repaint;
  end;
end;

procedure TAMonthListBox.WMDestroy(var Message: TMessage);
begin
  FreeTimer;
  inherited;
end;

procedure TAMonthListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAMonthListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Font);
  with TdxBarDateNavigator(Parent) do
  begin
    FItemHeight := FHeaderHeight - 2;
    Self.Width := 2 * GetSystemMetrics(SM_CXBORDER) + 6 * FColWidth;
    Self.Height := 2 * GetSystemMetrics(SM_CYBORDER) + 7 * ItemHeight;
  end;
end;

procedure TAMonthListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOPMOST;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure AMonthListBoxTimerProc(Wnd: HWND; Msg: UINT; idEvent: UINT;
  Time: DWORD); stdcall;
var
  AControl: TAMonthListBox;
  Year, Month, Day: Word;
begin
  AControl := TAMonthListBox(FindControl(Wnd));
  with AControl do
  begin
    DecodeDate(TopDate, Year, Month, Day);
    dxChangeMonth(Year, Month, 2 * Integer(idEvent > 5) - 1);
    TopDate := EncodeDate(Year, Month, 1);
  end;
end;

procedure TAMonthListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  Times: array[1..4] of UINT = (500, 250, 100, 50);
var
  Delta, Sign: Integer;
  NewTimerId: UINT;
begin
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    FreeTimer;
    ItemIndex := Y div ItemHeight;
  end
  else
  begin
    ItemIndex := -1;
    if Y < 0 then Delta := Y
    else
      if Y >= ClientHeight then
        Delta := 1 + Y - ClientHeight
      else Exit;
    Sign := Delta div Abs(Delta);
    NewTimerId := Sign + Delta div 12;
    if Abs(NewTimerId) > 4 then
      NewTimerId := Sign * 4;
    NewTimerId := NewTimerId + 5;
    if (FTimer = 0) or (NewTimerId <> FTimerId) then
    begin
      FreeTimer;
      FTimerId := NewTimerId;
      FTimer := SetTimer(Handle, FTimerId, Times[Abs(FTimerId - 5)],
        @AMonthListBoxTimerProc);
    end;
  end;
end;

procedure TAMonthListBox.Paint;

  function GetItemColor(ASelected: Boolean): TColor;
  begin
    if ASelected then
      Result := clWindowText
    else
      Result := Color;
  end;

//const
//  Colors: array[Boolean] of TColor = (clWindow, clWindowText);
var
  ADay, AMonth, AYear: Word;
  ASelected: Boolean;
  I: Integer;
  R: TRect;
  S: string;
begin
  DecodeDate(TopDate, AYear, AMonth, ADay);
  with R do
  begin
    Left := 0;
    Top := 0;
    Right := ClientWidth;
    Bottom := ItemHeight;
  end;
  for I := 0 to 6 do
  begin
    ASelected := I = ItemIndex;
    with Canvas do
    begin
      Font.Color := GetItemColor(not ASelected);
      Brush.Color := GetItemColor(ASelected);
      Windows.FillRect(Handle, R, Brush.Handle);
      S := dxFormatSettings.LongMonthNames[AMonth] + ' ' + IntToStr(AYear);
      cxDrawText(Handle, S, R, DT_SINGLELINE or DT_NOCLIP or DT_CENTER or DT_VCENTER);
    end;
    dxIncMonth(AYear, AMonth);
    OffsetRect(R, 0, ItemHeight);
  end;
end;

{ TdxBarCustomCalendar }

constructor TdxBarCustomCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csDoubleClicks];
  FFirstDate := Date;
  FSelStart := FFirstDate;
  FSelFinish := FSelStart;
  FStyle := csFlat;
end;

function TdxBarCustomCalendar.GetFlat: Boolean;
begin
  Result := FStyle <> cs3D;
end;

function TdxBarCustomCalendar.GetUltraFlat: Boolean;
begin
  Result := Style = csUltraFlat;
end;

procedure TdxBarCustomCalendar.SetStyle(Value: TdxBarCalendarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TdxBarCustomCalendar.WMCancelMode(var Message: TMessage);
begin
  inherited;
  CancelAll;
end;

procedure TdxBarCustomCalendar.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  with Message do
    if (lParam <> 0) and (HWND(lParam) <> Handle) then CancelAll;
end;

procedure TdxBarCustomCalendar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TdxBarCustomCalendar.GetStyle: TdxBarCalendarStyle;
begin
  Result := FStyle;
end;

function TdxBarCustomCalendar.GetRealFirstDate: TDateTime;
begin
  Result := FirstDate;
end;

function TdxBarCustomCalendar.GetRealLastDate: TDateTime;
begin
  Result := LastDate;
end;

function TdxBarCustomCalendar.GetSelStart: TDateTime;
begin
  if (FSelStart < FSelFinish) or (FSelFinish = NullDate) then
    Result := FSelStart
  else
    Result := FSelFinish;
end;

function TdxBarCustomCalendar.GetSelFinish: TDateTime;
begin
  if FSelStart < FSelFinish then
    Result := FSelFinish
  else
    Result := FSelStart;
end;

procedure TdxBarCustomCalendar.SetFirstDate(Value: TDateTime);
begin
  if FFirstDate <> Value then
  begin
    FFirstDate := Value;
  end;
end;

procedure TdxBarCustomCalendar.SetSelStart(Value: TDateTime);
begin
  FSelStart := Value;
  FSelFinish := NullDate;
  SelFinish := Value;
end;

procedure TdxBarCustomCalendar.SetSelFinish(Value: TDateTime);
var
  OldSelFinish: TDateTime;
begin
  if FSelFinish <> Value then
  begin
    CheckFirstDate;
    OldSelFinish := FSelFinish;
    FSelFinish := Value;
    if FSelFinish <> OldSelFinish then
    begin
      CheckFirstDate;
      Repaint;
    end;
  end;
end;

procedure TdxBarCustomCalendar.CancelAll;
begin
  SendMessage(Handle, WM_LBUTTONUP, 0, dxPointToLParam(cxInvalidPoint));
end;

procedure TdxBarCustomCalendar.DoDateTimeChanged;
begin
  if Assigned(FOnDateTimeChanged) then FOnDateTimeChanged(Self);
end;

procedure TdxBarCustomCalendar.DoInternalSelectPeriod(ADate: TDateTime);
var
  PrevSelFinish: TDateTime;
begin
  if (SelFinish <> ADate) and (ADate <> NullDate) then
  begin
    PrevSelFinish := FSelFinish;
    SelFinish := ADate;
    if FSelFinish = PrevSelFinish then Repaint;
  end;
end;

procedure TdxBarCustomCalendar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_CLIPCHILDREN;
end;

procedure TdxBarCustomCalendar.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, CM_FONTCHANGED, 0, 0);
end;

procedure TdxBarCustomCalendar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ADate: TDateTime;
begin
  if ssDouble in Shift then Exit;
  inherited MouseDown(Button, Shift, X, Y);
  ADate := PosToDateTime(Point(X, Y));
  if Button = mbLeft then
  begin
    FDragDate := SelStart;
    if ADate <> NullDate then SelStart := ADate;
  end;
end;

procedure TdxBarCustomCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ADate: TDateTime;
begin
  ADate := NullDate;
  if (ssLeft in Shift) and (GetCapture = Handle) then
    ADate := PosToDateTime(Point(X, Y));
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and (GetCapture = Handle) then
    if ADate <> NullDate then SelFinish := ADate
    else
      if not PtInRect(ClientRect, Point(X, Y)) then
        DoInternalSelectPeriod(FDragDate);
  Update;
end;

{ TdxBarDateNavigator }

constructor TdxBarDateNavigator.Create(AOwner: TComponent);
var
  Year, Month, Day: Word;
begin
  inherited Create(AOwner);
  Visible := False;
  DecodeDate(FFirstDate, Year, Month, Day);
  FFirstDate := EncodeDate(Year, Month, 1);
  Width := 20;
  Height := 20;
  FColCount := 1;
  FRowCount := 1;
  ShowTodayButton := True;
end;

procedure TdxBarDateNavigator.CheckSelection(MarginDate: TDateTime);
begin
  Repaint;
end;

function TdxBarDateNavigator.ColOfDate(ADate: TDateTime): Integer;
begin
  Result := dxDayOfWeekOffset(ADate);
end;

function TdxBarDateNavigator.GetHeaderRect: TRect;
begin
  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := ClientWidth;
    Bottom := Top + FHeaderHeight;
  end;
end;

function TdxBarDateNavigator.GetInternalRect: TRect;
begin
  with Result do
  begin
    Left := 0;
    Top := FHeaderHeight + Byte(not Flat);
    Right := ClientWidth;
    Bottom := Top + FDaysOfWeekHeight + 6 * FRowHeight + 1;
  end;
end;

function TdxBarDateNavigator.GetLeftArrowRect: TRect;
begin
  SetRect(Result, 1, 1, FColWidth - 1, FHeaderHeight - 1);
end;

function TdxBarDateNavigator.GetPainter: TdxBarPainter;
begin
  if IsPopup and (FCombo.CurItemLink <> nil) then
    Result := FCombo.CurItemLink.Control.Painter
  else
    Result := FCombo.BarManager.DefaultPainter;
end;

function TdxBarDateNavigator.GetRightArrowRect: TRect;
begin
  SetRect(Result, ClientWidth - FColWidth, 1,
    ClientWidth - 1 - Byte(not Flat), FHeaderHeight - 1);
end;

function TdxBarDateNavigator.GetMonthNameRect: TRect;
begin
  Result := GetInternalRect;
  with Result do
  begin
    Inc(Left, FColWidth);
    Dec(Right, FColWidth + Byte(not Flat));
    Bottom := Top - Byte(not Flat) - 1;
    Top := Bottom - (FHeaderHeight - 2);
  end;
end;

function TdxBarDateNavigator.GetTodayButtonRect: TRect;
begin
  Result :=
    Bounds(
      (ClientWidth - FTodayButtonWidth - Byte(ShowClearButton) * FClearButtonWidth) div
       (3 - Byte(not ShowClearButton)),
      ClientHeight - FButtonsRegionHeight + FButtonsOffset,
      FTodayButtonWidth, FButtonsHeight);
end;

function TdxBarDateNavigator.GetClearButtonRect: TRect;
begin
  Result :=
    Bounds(ClientWidth - FClearButtonWidth -
      (ClientWidth - Byte(ShowTodayButton) * FTodayButtonWidth - FClearButtonWidth) div
       (3 - Byte(not ShowTodayButton)),
      ClientHeight - FButtonsRegionHeight + FButtonsOffset,
      FClearButtonWidth, FButtonsHeight);
end;

function TdxBarDateNavigator.GetShowButtonsArea: Boolean;
begin
  Result := ShowTodayButton or ShowClearButton;
end;

procedure TdxBarDateNavigator.FreeTimer;
begin
  dxKillTimer(Handle, FTimer);
end;

procedure TdxBarDateNavigator.RepaintTodayButton;
begin
  cxInvalidateRect(Handle, GetTodayButtonRect, False);
end;

procedure TdxBarDateNavigator.RepaintClearButton;
begin
  cxInvalidateRect(Handle, GetClearButtonRect, False);
end;

procedure TdxBarDateNavigator.WMDestroy(var Message: TMessage);
begin
  FreeTimer;
  inherited;
end;

procedure TdxBarDateNavigator.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  if Flat then
    InflateRect(Message.CalcSize_Params^.rgrc[0], -1, -1);
end;

procedure TdxBarDateNavigator.WMNCPaint(var Message: TWMNCPaint);
var
  R, CR: TRect;
  Delta: Integer;
  DC: HDC;
begin
  inherited;
  if Flat then
  begin
    R := cxGetWindowRect(Handle);
    OffsetRect(R, -R.Left, -R.Top);
    DC := GetWindowDC(Handle);
    if Style = csFlat then
    begin
      Windows.GetClientRect(Handle, CR);
      Delta := (R.Right - CR.Right) div 2 - 1;
      InflateRect(R, -Delta, -Delta);
      DrawEdge(DC, R, BDR_SUNKENOUTER, BF_RECT);
    end
    else
      FrameRect(DC, R, GetSysColorBrush(COLOR_BTNSHADOW));
    ReleaseDC(Handle, DC);
  end;
end;

procedure TdxBarDateNavigator.WMSize(var Message: TWMSize);
begin
  inherited;
  SetSize;
end;

procedure TdxBarDateNavigator.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font.Assign(Font);
  FColWidth := 3 * Canvas.TextWidth('0');
  FSideWidth := 2 * Canvas.TextWidth('0');
  FRowHeight := Canvas.TextHeight('0') + 2;
  FHeaderHeight := FRowHeight + 2 + Byte(Flat);
  FDaysOfWeekHeight := FRowHeight + 1;
  FTodayButtonWidth := Canvas.TextWidth(sdxBarDatePopupToday) +
    FColWidth;
  FClearButtonWidth := Canvas.TextWidth(sdxBarDatePopupClear) +
    FColWidth;
  FButtonsOffset := Font.Size div 2;
  FButtonsHeight := MulDiv(Font.Size, 5, 2);
  FButtonsRegionHeight := FButtonsOffset + FButtonsHeight +
    Font.Size * 3 div 4;
  SendMessage(Handle, WM_SIZE, 0, 0);
end;

function TdxBarDateNavigator.GetStyle: TdxBarCalendarStyle;
begin
  Result := inherited GetStyle;
  if (Result = csFlat) and Painter.IsDateNavigatorFlat then
    Result := csUltraFlat;
end;

function TdxBarDateNavigator.GetRealFirstDate: TDateTime;
var
  ACol: Integer;
begin
  Result := FirstDate;
  ACol := ColOfDate(FirstDate);
  if ACol = 0 then
    Result := Result - 7
  else
    Result := Result - ACol;
end;

function TdxBarDateNavigator.GetRealLastDate: TDateTime;
var
  Year, Month, Day: Word;
  ACol: Integer;
begin
  Result := LastDate;
  DecodeDate(Result, Year, Month, Day);
  ACol := ColOfDate(EncodeDate(Year, Month, 1));
  Result := Result + 6 * 7 - DaysPerMonth(Year, Month) - ACol;
  if ACol = 0 then Result := Result - 7;
end;

function TdxBarDateNavigator.GetLastDate: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FirstDate, Year, Month, Day);
  Result := EncodeDate(Year, Month, DaysPerMonth(Year, Month));
end;

procedure TdxBarDateNavigator.SetFirstDate(Value: TDateTime);
begin
  Value := dxDateOf(Value) - (dxGetDateElement(Value, deDay) - 1);
  inherited SetFirstDate(Value);
end;

procedure TdxBarDateNavigator.SetSelFinish(Value: TDateTime);
begin
  if FSelFinish <> Value then
  begin
    FSelStart := Value;
    inherited SetSelFinish(Value);
  end;
end;

procedure TdxBarDateNavigator.StepToPast;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FirstDate, Year, Month, Day);
  dxDecMonth(Year, Month);
  FirstDate := EncodeDate(Year, Month, 1);
  if SelStart > LastDate then
    CheckSelection(LastDate)
  else
    Repaint;
end;

procedure TdxBarDateNavigator.StepToFuture;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FirstDate, Year, Month, Day);
  dxIncMonth(Year, Month);
  FirstDate := EncodeDate(Year, Month, 1);
  if SelStart < FirstDate then
    CheckSelection(FirstDate)
  else
    Repaint;
end;

procedure TdxBarDateNavigator.CancelAll;
begin
  inherited;
  DeactivateAll;
end;

procedure TdxBarDateNavigator.CheckFirstDate;
var
  Year, Month, Day: Word;
begin
  if FSelStart < RealFirstDate then
  begin
    DecodeDate(FSelStart, Year, Month, Day);
    dxChangeMonth(Year, Month, -1{(ColCount * RowCount - 1)});
    FirstDate := EncodeDate(Year, Month, CheckDay(Year, Month, Day));
  end;
  if FSelStart > RealLastDate then
    FirstDate := dxDateOf(FSelStart);
end;

procedure TdxBarDateNavigator.DeactivateAll;
begin
  FreeTimer;
  dxFreeAndNil(FListBox);
  FTodayButtonActive := False;
  FClearButtonActive := False;
end;

function TdxBarDateNavigator.PosToDateTime(P: TPoint): TDateTime;
var
  ACol, ARow, X, Y: Integer;
  R: TRect;
  Year, Month, Day, AYear, AMonth: Word;
  ADate: TDateTime;
begin
  if PtInRect(ClientRect, P) then
  begin
    ACol := P.X div ClientWidth;
    ARow := P.Y div ClientHeight;
    R := GetInternalRect;
    with R do
    begin
      Inc(Top, FDaysOfWeekHeight);
      Inc(Left, FSideWidth);
      Dec(Right, FSideWidth);
      Bottom := Top + 6 * FRowHeight;
      if PtInRect(R, P) then
      begin
        Dec(P.X, Left);
        Dec(P.Y, Top);
        X := P.X div FColWidth;
        Y := P.Y div FRowHeight;
        DecodeDate(FirstDate, Year, Month, Day);
        dxChangeMonth(Year, Month, ARow + ACol);
        ADate := EncodeDate(Year, Month, 1);
        Result := ADate - ColOfDate(ADate) + Y * 7 + X;
        if (ACol + ARow = 0) and (ColOfDate(FirstDate) = 0) then
          Result := Result - 7;
        DecodeDate(Result, AYear, AMonth, Day);
        if ((Result < ADate) and (ACol + ARow > 0)) or
          ((Result >= ADate + DaysPerMonth(Year, Month)) and
           not ((ACol = 0) and (ARow = 0))) then
          Result := NullDate;
      end
      else
        Result := NullDate;
    end;
  end
  else
    Result := NullDate;
end;

procedure TdxBarDateNavigator.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if IsPopup then
    begin
      Style := WS_CHILD or Byte(not UltraFlat) * WS_DLGFRAME;
      ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    end;
    if not Flat then
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

procedure TdxBarDateNavigator.CreateWnd;
begin
  inherited CreateWnd;

  if FCombo.CurItemLink <> nil then
    FCombo.InternalInitDropDownWindow(Self)
  else
  begin
    Color := clWindow;
    Font.Color := clWindowText;
  end;
  Canvas.Font := Font;

  if IsPopup then
  begin
    Windows.SetParent(Handle, 0);
    CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TdxBarDateNavigator.DblClick;
var
  P: TPoint;
begin
  inherited;
  GetCursorPos(P);
  Windows.ScreenToClient(Handle, P);
  if not IsPopup and (PosToDateTime(P) <> NullDate) then
    FCombo.FForm.ModalResult := mrOk;
end;

procedure TdxBarDateNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  AYear, AMonth, ADay: Word;

  procedure MoveByMonth(AForward: Boolean);
  begin
    DecodeDate(SelStart, AYear, AMonth, ADay);
    if AForward then
      dxIncMonth(AYear, AMonth)
    else
      dxDecMonth(AYear, AMonth);
    ADay := CheckDay(AYear, AMonth, ADay);
    SelStart := EncodeDate(AYear, AMonth, ADay);
  end;

begin
  inherited KeyDown(Key, Shift);
  if IsPopup then
    case Key of
      VK_RETURN:
        if FListBox = nil then DoDateTimeChanged;
      VK_LEFT: SelStart := SelStart - 1;
      VK_RIGHT: SelStart := SelStart + 1;
      VK_UP: SelStart := SelStart - 7;
      VK_DOWN: SelStart := SelStart + 7;
      VK_HOME:
        if Shift = [ssCtrl] then
          SelStart := SelStart - (dxGetDateElement(SelStart, deDay) - 1)
        else
          SelStart := SelStart - ColOfDate(SelStart);
      VK_END:
        if Shift = [ssCtrl] then
        begin
          DecodeDate(SelStart, AYear, AMonth, ADay);
          SelStart := SelStart + (DaysPerMonth(AYear, AMonth) - ADay)
        end
        else
          SelStart := SelStart + (6 - ColOfDate(SelStart));
      VK_PRIOR: MoveByMonth(False);
      VK_NEXT: MoveByMonth(True)
    end;
end;

procedure ADateNavigatorTimerProc(Wnd: HWND; Msg: UINT; idEvent: UINT;
  Time: DWORD); stdcall;
var
  AControl: TdxBarDateNavigator;
  P: TPoint;
begin
  AControl := TdxBarDateNavigator(FindControl(Wnd));
  GetCursorPos(P);
  P := AControl.ScreenToClient(P);
  with AControl do
    case idEvent of
      1: if PtInRect(GetLeftArrowRect, P) then StepToPast;
      2: if PtInRect(GetRightArrowRect, P) then StepToFuture;
    end;
end;

procedure TdxBarDateNavigator.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  Year, Month, Day: Word;
  R: TRect;
begin
  if (Button = mbLeft) and IsPopup then
    if ShowTodayButton and PtInRect(GetTodayButtonRect, Point(X, Y)) then
    begin
      FTodayButtonActive := True;
      FTodayButtonPressed := True;
      RepaintTodayButton;
      Exit;
    end
    else
      if ShowClearButton and PtInRect(GetClearButtonRect, Point(X, Y)) then
      begin
        FClearButtonActive := True;
        FClearButtonPressed := True;
        RepaintClearButton;
        Exit;
      end
      else
        if ShowButtonsArea and (Y >= ClientHeight - FButtonsRegionHeight) then
          Exit;
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    Col := X div ClientWidth;
    Row := Y div ClientHeight;
    if PtInRect(GetMonthNameRect, Point(X, Y)) then
    begin  // show month's list box
      FListBoxDelta := Row + Col;
      FListBox := TAMonthListBox.Create(Self);
      FListBox.Visible := False;
      FListBox.Parent := Self;
      DecodeDate(FirstDate, Year, Month, Day);
      dxChangeMonth(Year, Month, FListBoxDelta - 3);
      R := dxMapWindowRect(Handle, 0, GetMonthNameRect);
      with TAMonthListBox(FListBox) do
      begin
        Font.Assign(Self.Font);
        SendMessage(Handle, CM_FONTCHANGED, 0, 0);
        TopDate := EncodeDate(Year, Month, 1);
        Left := (R.Left + R.Right - Width) div 2;
        Top := (R.Top + R.Bottom) div 2 - Height div 2;
        ShowWindow(Handle, SW_SHOWNOACTIVATE);
      end;
    end
    else
      if PtInRect(GetLeftArrowRect, Point(X, Y)) then
      begin  // shift by month to past
        StepToPast;
        if FTimer = 0 then
          FTimer := SetTimer(Handle, 1, ADateNavigatorTime,
            @ADateNavigatorTimerProc);
      end
      else
        if PtInRect(GetRightArrowRect, Point(X, Y)) then
        begin  // shift by month to future
          StepToFuture;
          if FTimer = 0 then
            FTimer := SetTimer(Handle, 2, ADateNavigatorTime,
              @ADateNavigatorTimerProc);
        end;
  end;
end;

procedure TdxBarDateNavigator.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FTimer > 0 then Exit;
  if FListBox <> nil then
  begin
    P := dxMapWindowPoint(Handle, FListBox.Handle, Point(X, Y));
    TAMonthListBox(FListBox).MouseMove(Shift, P.X, P.Y);
    Exit;
  end;
  if FTodayButtonActive then
  begin
    if FTodayButtonPressed <> PtInRect(GetTodayButtonRect, Point(X, Y)) then
    begin
      FTodayButtonPressed := not FTodayButtonPressed;
      RepaintTodayButton;
    end;
    Exit;
  end;
  if FClearButtonActive then
  begin
    if FClearButtonPressed <> PtInRect(GetClearButtonRect, Point(X, Y)) then
    begin
      FClearButtonPressed := not FClearButtonPressed;
      RepaintClearButton;
    end;
    Exit;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxBarDateNavigator.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ADate: TDateTime;
  Year, Month, Day: Word;
  P: TPoint;
begin
  if FTimer > 0 then
  begin
    FreeTimer;
    Exit;
  end;
  if FListBox <> nil then
  begin
    ADate := TAMonthListBox(FListBox).Date;
    dxFreeAndNil(FListBox);
    if ADate <> NullDate then
    begin
      DecodeDate(ADate, Year, Month, Day);
      dxChangeMonth(Year, Month, -FListBoxDelta);
      FirstDate := EncodeDate(Year, Month, 1);
      if SelStart < FirstDate then
        CheckSelection(FirstDate)
      else
        if SelStart > LastDate then
          CheckSelection(LastDate)
        else
          Repaint;
    end;
    Exit;
  end;
  if FTodayButtonActive then
  begin
    FTodayButtonActive := False;
    if FTodayButtonPressed then
      SelStart := Date
    else
      Exit;
  end;
  if FClearButtonActive then
  begin
    FClearButtonActive := False;
    if FClearButtonPressed then
      SelStart := NullDate
    else
      Exit;
  end;
  inherited MouseUp(Button, Shift, X, Y);
  if not (ssDouble in Shift) then
  begin
    P := Point(X, Y);
    if PtInRect(ClientRect, P) and
      ((PosToDateTime(P) <> NullDate) or
       ShowTodayButton and PtInRect(GetTodayButtonRect, P) or
       ShowClearButton and PtInRect(GetClearButtonRect, P)) then
      DoDateTimeChanged
    else
      DoInternalSelectPeriod(FDragDate);
  end;
end;

procedure TdxBarDateNavigator.Paint;

  procedure DrawArrow(const R: TRect; ALeftArrow: Boolean);
  var
    X, ASign: Integer;
    P: array[1..3] of TPoint;
    AArrowHeight: Integer;
  begin
    AArrowHeight := cxRectHeight(R) div 2;
    if not Odd(AArrowHeight) then
      Inc(AArrowHeight);

    if ALeftArrow then
      X := R.Left - 1
    else
      X := R.Right;
    ASign := 2 * Byte(ALeftArrow) - 1;
    P[1] := Point(X + ASign * (FSideWidth - 1), (R.Top + R.Bottom - AArrowHeight) div 2);
    P[2] := Point(P[1].X, P[1].Y + AArrowHeight - 1);
    P[3] := Point(P[1].X - ASign * AArrowHeight div 2, P[1].Y + AArrowHeight div 2);
    cxPaintCanvas.Pen.Color := clBtnText;
    cxPaintCanvas.Brush.Color := clBtnText;
    cxPaintCanvas.Polygon(P);
  end;

  procedure DrawHeader;
  var
    S: string;
    R: TRect;
    AMonth, AYear, ADay: Word;
  begin
    DecodeDate(FirstDate, AYear, AMonth, ADay);

    R := GetHeaderRect;
    cxPaintCanvas.FillRect(R, Painter.DateNavigatorHeaderColor);
    cxPaintCanvas.FrameRect(R, clBtnShadow, 1, [bBottom]);

    DrawArrow(R, True);
    DrawArrow(R, False);
    S := dxFormatSettings.LongMonthNames[AMonth] + ' ' + IntToStr(AYear);
    cxDrawText(cxPaintCanvas, S, R, DT_VCENTER or DT_CENTER or DT_SINGLELINE, clBtnText);
  end;

  procedure DrawDayLetters(const R: TRect);
  var
    ALettersRect, ALineRect : TRect;
    I: TDxDayOfWeek;
    J: TDay;
    S : string;
  begin
    ALineRect := cxRectInflate(R, -FSideWidth, 0);
    ALineRect.Bottom := ALineRect.Top + FDaysOfWeekHeight - 2;
    cxPaintCanvas.FrameRect(AlineRect, clBtnShadow, 1, [bBottom]);

    ALettersRect := ALineRect;
    ALettersRect.Right := ALettersRect.Left;

    for I := Low(TdxDayOfWeek) to High(TdxDayOfWeek) do
    begin
      ALettersRect.Left := ALettersRect.Right;
      ALettersRect.Right := ALettersRect.Left + FColWidth;
      J := dxGetDayOfWeek(dxGetStartOfWeek, I);
      S := cxGetDayOfWeekName(J, cxPaintCanvas.Font.Charset);
      cxDrawText(cxPaintCanvas, S, ALettersRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE, clBtnText);
    end;
  end;

  procedure DrawDaysGrid(const R: TRect);

    function GetDayBrushColor(ASelected: Boolean): TColor;
    begin
      if not ASelected then
        Result := Color
      else
        if Flat then
          Result := clBtnFace
        else
          Result := clHighlight;
    end;

  const
    FontColors: array[Boolean] of TColor = (clWindowText, clHighlightText);
  var
    ADate, AFirstDayOfMonth: TDateTime;
    I, J, AFirstDayOfWeek: Integer;
    ASelected: Boolean;
    S: string;
    AYear, AMonth, ADay: Word;
    ATextRect: TRect;
    ACurrentDate, ALastDate: TDateTime;
  begin
    ACurrentDate := Date;
    ALastDate := LastDate;
    DecodeDate(FirstDate, AYear, AMonth, ADay);
    AFirstDayOfMonth := EncodeDate(AYear, AMonth, 1) - 1;
    AFirstDayOfWeek := 1 - ColOfDate(AFirstDayOfMonth + 1);
    if (AFirstDayOfWeek = 1) then
      Dec(AFirstDayOfWeek, 7);

    for I := 0 to 6 do
      for J := 0 to 5 do
      begin
        ADate := AFirstDayOfMonth + AFirstDayOfWeek + J * 7 + I;
        ASelected := (ADate >= SelStart) and (ADate <= SelFinish);

        ATextRect.Left := R.Left + FSideWidth + I * FColWidth;
        ATextRect.Top := R.Top + FDaysOfWeekHeight + J * FRowHeight;
        ATextRect.Right := ATextRect.Left + FColWidth;
        ATextRect.Bottom := ATextRect.Top + FRowHeight;

        if ADate = ACurrentDate then
        begin
          cxPaintCanvas.FrameRect(ATextRect, clMaroon);
          InflateRect(ATextRect, -1, -1);
        end;

        if ASelected and UltraFlat then
          cxPaintCanvas.Brush.Color := TBarManagerAccess(FCombo.BarManager).FlatToolbarsSelColor
        else
          cxPaintCanvas.Brush.Color := GetDayBrushColor(ASelected);
        cxPaintCanvas.FillRect(ATextRect);

        if not ASelected and ((ADate < FirstDate) or (ADate > ALastDate)) then
          cxPaintCanvas.Font.Color := clGrayText
        else
          cxPaintCanvas.Font.Color := FontColors[ASelected and not UltraFlat];
        S := IntToStr(dxGetDateElement(ADate, deDay));
        cxDrawText(cxPaintCanvas, S, ATextRect, DT_VCENTER or DT_CENTER or DT_SINGLELINE);
      end;
  end;

  procedure DrawMonthGrid;
  var
    R: TRect;
  begin
    R := GetInternalRect;

    DrawDayLetters(R);
    DrawDaysGrid(R);
  end;

  procedure DrawButton(R: TRect; ACaption: string; Pressed: Boolean);
  begin
    Painter.DateNavigatorDrawButton(FCombo, Canvas.Handle, R, ACaption, Pressed, dxSystemScaleFactor);
  end;

begin
  cxPaintCanvas.BeginPaint(Canvas);
  try
    cxPaintCanvas.FillRect(ClientRect, Color);

    DrawHeader;
    DrawMonthGrid;

    if IsPopup and ShowButtonsArea then
    begin
      cxPaintCanvas.FrameRect(cxRectInflate(ClientRect, -FSideWidth, -FButtonsRegionHeight - 1), clBtnShadow, 1, [bBottom]);
      if ShowTodayButton then
        DrawButton(GetTodayButtonRect, sdxBarDatePopupToday, FTodayButtonActive and FTodayButtonPressed);
      if ShowClearButton then
        DrawButton(GetClearButtonRect, sdxBarDatePopupClear, FClearButtonActive and FClearButtonPressed);
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarDateNavigator.SetSize;
begin
  Width := GetWidth;
  Height := GetHeight;
end;

function TdxBarDateNavigator.GetWidth: Integer;
var
  WR, CR: TRect;
begin
  WR := cxGetWindowRect(Handle);
  OffsetRect(WR, -WR.Left, -WR.Top);
  CR := cxGetClientRect(Handle);
  Result := WR.Right - CR.Right + 2 * FSideWidth + 7 * FColWidth;
end;

function TdxBarDateNavigator.GetHeight: Integer;
var
  WR, CR: TRect;
begin
  WR := cxGetWindowRect(Handle);
  OffsetRect(WR, -WR.Left, -WR.Top);
  CR := cxGetClientRect(Handle);
  Result := WR.Bottom - CR.Bottom +
    FHeaderHeight + Byte(not Flat) + FDaysOfWeekHeight + 6 * FRowHeight + 1;
  if IsPopup and ShowButtonsArea then
    Inc(Result, FButtonsRegionHeight);
end;

{ TdxBarDateCombo }

constructor TdxBarDateCombo.Create(AOwner: TComponent);
begin
  inherited;
  Glyph.LoadFromResource(HInstance, 'DXBARDATECOMBO', RT_BITMAP);
  FShowTodayButton := True;
  FShowClearButton := True;
  FInternalUpdate := True;
  try
    Date := SysUtils.Date;
  finally
    FInternalUpdate := False;
  end;
  FDatePopup := TdxBarDateNavigator.Create(Self);
  with FDatePopup do
  begin
    FCombo := Self;
    IsPopup := True;
  end;
  FShowDayText := True;
end;

destructor TdxBarDateCombo.Destroy;
begin
  FreeAndNil(FDatePopup);
  inherited;
end;

function TdxBarDateCombo.GetCurDate: TDateTime;
begin
  Result := GetDateOfText(CurText);
end;

function TdxBarDateCombo.GetDate: TDateTime;
begin
  Result := GetDateOfText(Text);
end;

procedure TdxBarDateCombo.SetCurDate(Value: TDateTime);
begin
  CurText := GetDateText(CheckDate(Value));
end;

procedure TdxBarDateCombo.SetDate(Value: TDateTime);
begin
  Text := GetDateText(CheckDate(Value));
end;

procedure TdxBarDateCombo.DateChanged(Sender: TObject);
begin
  if (CurItemLink <> nil) and (CurItemLink.RealItemLink <> nil) then
  begin
    CurItemLink.RealItemLink.BringToTopInRecentList(True);
    BarManager.HideAll;
  end;
  Date := TdxBarDateNavigator(Sender).SelStart;
end;

procedure TdxBarDateCombo.DialogClick(Sender: TObject);
begin
  case TWinControl(Sender).Tag of
    1: FDateNavigator.SelStart := SysUtils.Date;
    2: FDateNavigator.SelStart := NullDate;
  end;
  DialogDateChanged(nil);
end;

procedure TdxBarDateCombo.DialogDateChanged(Sender: TObject);
begin
  FDateEdit.Text := GetDateText(FDateNavigator.SelStart);
end;

procedure TdxBarDateCombo.DialogDateEditChange(Sender: TObject);
var
  ADate: TDateTime;
begin
  ADate := GetDateOfText(FDateEdit.Text);
  if (ADate <> NullDate) or (FDateEdit.Text = '') then
    FDateNavigator.SelStart := ADate;
end;

function TdxBarDateCombo.GetDateOfText(AText: string): TDateTime;

  function IsStringWithDayText(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to 7 do
      if Pos(dxFormatSettings.ShortDayNames[I] + ' ', S) <> 0 then
      begin
        Result := True;
        Break;
      end;
  end;

var
  P: Integer;
begin
  if IsStringWithDayText(AText) then
  begin
    P := Pos(' ', AText);
    if P > 0 then Delete(AText, 1, P);
  end;
  Result := cxTextToDateTime(AText);
end;

function TdxBarDateCombo.GetDateText(ADate: TDateTime): string;
begin
  if ADate = NullDate then
    Result := ''
  else
  begin
    if ShowDayText then
      Result := FormatDateTime('ddd ', ADate)
    else
      Result := '';
    Result := Result + cxDateTimeToText(ADate);
  end;
end;

function TdxBarDateCombo.IsMinDateStored: Boolean;
begin
  Result := FMinDate <> 0;
end;

function TdxBarDateCombo.IsMaxDateStored: Boolean;
begin
  Result := FMaxDate <> 0;
end;

function TdxBarDateCombo.IsTextStored: Boolean;
begin
  Result := FDateOnStart = bdsCustom;
end;

procedure TdxBarDateCombo.SetDateOnStart(Value: TdxBarDateOnStart);
begin
  if FDateOnStart <> Value then
  begin
    FDateOnStart := Value;
    CheckDateOnStart;
  end;
end;

procedure TdxBarDateCombo.SetMinDate(Value: TDateTime);
begin
  Value := Min(Value, FMaxDate);
  if FMinDate <> Value then
  begin
    FMinDate := Value;
    CheckRange;
  end;
end;

procedure TdxBarDateCombo.SetMaxDate(Value: TDateTime);
begin
  Value := Max(Value, FMinDate);
  if FMaxDate <> Value then
  begin
    FMaxDate := Value;
    CheckRange;
  end;
end;

procedure TdxBarDateCombo.SetShowDayText(Value: Boolean);
begin
  if FShowDayText <> Value then
  begin
    FShowDayText := Value;
    ResetDate;
  end;
end;

procedure TdxBarDateCombo.Loaded;
begin
  CheckDateOnStart;
  inherited Loaded;
end;

procedure TdxBarDateCombo.CheckDateOnStart;
begin
  FInternalUpdate := True;
  try
    case DateOnStart of
      bdsToday:
        Date := SysUtils.Date;
      bdsNullDate:
        Date := NullDate;
      bdsCustom:
        Date := Date;
    end;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TdxBarDateCombo.CheckRange;
var
  ADate: TDateTime;
begin
  ADate := CheckDate(Date);
  if Date <> ADate then
    Date := ADate;
end;

function TdxBarDateCombo.CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = VK_RETURN) or
    inherited CheckKeyForDropDownWindow(Key, Shift);
end;

procedure TdxBarDateCombo.CloseUp;
begin
  with FDatePopup do
    if IsWindowVisible(Handle) then
    begin
      if GetCapture = Handle then ReleaseCapture;
      DeactivateAll;
    end;
  inherited;
  FDatePopup.Parent := nil;
end;

procedure TdxBarDateCombo.DropDown(X, Y: Integer);
var
  ADate: TDateTime;
begin
  with FDatePopup do
  begin
    ADate := Date;
    if ADate = NullDate then ADate := SysUtils.Date;
    FirstDate := ADate;
    SelStart := ADate;
    OnDateTimeChanged := DateChanged;
    ShowTodayButton := Self.ShowTodayButton;
    ShowClearButton := Self.ShowClearButton;
    Parent := CurItemLink.Control.Parent;  //BarManager.MainForm;
  end;
  inherited;
end;

function TdxBarDateCombo.GetDropDownWindow: HWND;
begin
  Result := inherited GetDropDownWindow;
  if Result = 0 then Result := FDatePopup.Handle;
end;

procedure TdxBarDateCombo.ResetDate;
begin
  FInternalUpdate := True;
  try
    Date := Date; // reset
  finally
    FInternalUpdate := False;
  end;
end;

procedure TdxBarDateCombo.SetText(Value: string);
begin
  Value := GetDateText(CheckDate(GetDateOfText(Value)));
  if not FInternalUpdate then
    FDateOnStart := bdsCustom;
  inherited;
end;

function TdxBarDateCombo.CheckDate(ADate: TDateTime): TDateTime;
begin
  if (MinDate <> 0) or (MaxDate <> 0) then
    Result := Min(Max(ADate, MinDate), MaxDate)
  else
    Result := ADate;
end;

procedure TdxBarDateCombo.DoClick;
var
  ButtonOk, ButtonCancel, ButtonToday, ButtonClear: TButton;
  W, H, D: Integer;
begin
  inherited;
  if not Assigned(OnClick) and not ReadOnly then
  begin
    FForm := TForm.Create(nil);
    with FForm do
    begin
      BorderStyle := bsDialog;
      Caption := cxGetResourceString(@dxSBAR_DATEDIALOGCAPTION);
      Font := BarManager.Font;
      Position := poScreenCenter;

      FDateEdit := TEdit.Create(FForm);
      with FDateEdit do
      begin
        Parent := FForm;
        OnChange := DialogDateEditChange;
        HandleNeeded;
      end;
      FDateNavigator := TdxBarDateNavigator.Create(FForm);
      with FDateNavigator do
      begin
        Style := cs3D;
        FCombo := Self;
        Parent := FForm;
        Visible := True;
        OnDateTimeChanged := DialogDateChanged;
        HandleNeeded;
      end;
      ButtonOk := TButton.Create(FForm);
      with ButtonOk do
      begin
        Caption := cxGetResourceString(@dxSBAR_DIALOGOK);
        Default := True;
        ModalResult := mrOk;
        Parent := FForm;
      end;
      ButtonCancel := TButton.Create(FForm);
      with ButtonCancel do
      begin
        Caption := cxGetResourceString(@dxSBAR_DIALOGCANCEL);
        Cancel := True;
        ModalResult := mrCancel;
        Parent := FForm;
      end;
      if ShowTodayButton then
      begin
        ButtonToday := TButton.Create(FForm);
        with ButtonToday do
        begin
          Caption := sdxBarDatePopupToday;
          Parent := FForm;
          Tag := 1;
          OnClick := DialogClick;
        end;
      end
      else
        ButtonToday := nil;
      if ShowClearButton then
      begin
        ButtonClear := TButton.Create(FForm);
        with ButtonClear do
        begin
          Caption := sdxBarDatePopupClear;
          Parent := FForm;
          Tag := 2;
          OnClick := DialogClick;
        end;
      end
      else
        ButtonClear := nil;

      W := MulDiv(FDateNavigator.FTodayButtonWidth, 3, 2);
      H := MulDiv(FDateNavigator.FButtonsHeight, 7, 6);
      D := FDateNavigator.FButtonsHeight div 4;

      ClientWidth := D + FDateNavigator.Width + D + W + D;
      ClientHeight := D + FDateEdit.Height + D + FDateNavigator.Height + D;
      FDateEdit.SetBounds(D, D, FDateNavigator.Width, FDateEdit.Height);
      FDateNavigator.SetBounds(D, FDateEdit.Top + FDateEdit.Height + D, 0, 0);
      ButtonOk.SetBounds(FDateEdit.Left + FDateEdit.Width + D, D, W, H);
      ButtonCancel.SetBounds(ButtonOk.Left, ButtonOk.Top + ButtonOk.Height + D, W, H);
      if ButtonToday <> nil then
        ButtonToday.SetBounds(ButtonOk.Left, ClientHeight - D - H - D - H, W, H);
      if ButtonClear <> nil then
        ButtonClear.SetBounds(ButtonOk.Left, ClientHeight - D - H, W, H);

      FDateEdit.Text := GetDateText(Date);
      if ShowModal = mrOk then
        Date := GetDateOfText(FDateEdit.Text);
      Free;
    end;
  end;
end;

{ TdxBarDateComboControl }

function TdxBarDateComboControl.GetDate: TDateTime;
begin
  Result := Item.GetDateOfText(Text);
end;

function TdxBarDateComboControl.GetItem: TdxBarDateCombo;
begin
  Result := TdxBarDateCombo(ItemLink.Item);
end;

procedure TdxBarDateComboControl.SetDate(const Value: TDateTime);
begin
  Text := Item.GetDateText(Value);
end;

procedure TdxBarDateComboControl.WndProc(var Message: TMessage);
begin
  with Message do
    if Msg = WM_CHAR then
      case wParam of
        Ord('+'):
          begin
            if Date <> NullDate then Date := Date + 1;
            wParam := 0;
          end;
        Ord('-'):
          begin
            if Date <> NullDate then Date := Date - 1;
            wParam := 0;
          end;
      end;
  inherited;
end;

{ TdxBarTreeView }

constructor TdxBarTreeView.Create(AOwner: TComponent);
begin
  inherited;
  Visible := False;
  ReadOnly := True;
  SetBounds(0, 0, 150, 200);
end;

destructor TdxBarTreeView.Destroy;

  procedure FreeNode(ANode: TTreeNode);
  var
    I: Integer;
  begin
    for I := 0 to ANode.Count - 1 do
      FreeNode(ANode[0]);
    ANode.Free;
  end;

begin
  while Items.Count <> 0 do
    FreeNode(Items.GetFirstNode);
  inherited;
end;

function TdxBarTreeView.FindNode(const AText: string): TTreeNode;
var
  ANode: TTreeNode;

  function FindOne(ARootNode: TTreeNode): TTreeNode;
  var
    ANode: TTreeNode;
  begin
    if AnsiCompareText(AText, ARootNode.Text) = 0 then Result := ARootNode
    else
    begin
      Result := nil;
      ANode := ARootNode.GetFirstChild;
      while ANode <> nil do
      begin
        Result := FindOne(ANode);
        if Result <> nil then Exit;
        ANode := ARootNode.GetNextChild(ANode);
      end;
    end;
  end;

begin
  Result := nil;
  with Items do
  begin
    ANode := GetFirstNode;
    while ANode <> nil do
    begin
      Result := FindOne(ANode);
      if Result <> nil then Break;
      ANode := ANode.GetNext;
    end;
  end;
end;

function TdxBarTreeView.GetPainter: TdxBarPainter;
begin
  if IsPopup and (FCombo.CurItemLink <> nil) then
    Result := FCombo.CurItemLink.Control.Painter
  else
    Result := FCombo.BarManager.DefaultPainter;
end;

procedure TdxBarTreeView.SaveAndHide;
begin
  if (Selected <> nil) and FCombo.DoCanSelectNode then
    if IsPopup then
    begin
      with FCombo do
      begin
        if (CurItemLink <> nil) and (CurItemLink.RealItemLink <> nil) then
          CurItemLink.RealItemLink.BringToTopInRecentList(True);
        BarManager.HideAll;
      end;
      FCombo.SelectedNode := Selected;
    end
    else
    begin
      FCombo.Text := Selected.Text;
      FCombo.FForm.ModalResult := mrOk;
    end;
end;

procedure TdxBarTreeView.TVMSetImageList(var Message: TMessage);
begin
  inherited;
  if IsPopup then FCombo.UpdateEx;//DoImageListChanged;
end;

procedure TdxBarTreeView.TVMSetItem(var Message: TMessage);
begin
  inherited;
  if not (csDestroying in ComponentState) and
    (FCombo.SelectedNode <> nil) and
    (PTVItem(Message.lParam)^.hitem = FCombo.SelectedNode.ItemId) then
    FCombo.DoSelectedNodeChanged;
end;

procedure TdxBarTreeView.WMCaptureChanged(var Message: TMessage);
begin
  inherited;
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    FMouseAboveCloseButton := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarTreeView.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  Message.MinMaxInfo^.ptMinTrackSize := Point(100, 100);
end;

procedure TdxBarTreeView.WMLButtonDown(var Message: TWMLButtonDown);
begin
  FMouseDownHitTestInfo := GetHitTestInfoAt(Message.XPos, Message.YPos);
  inherited;
end;

procedure TdxBarTreeView.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  if FCloseButtonIsTracking then
  begin
    FCloseButtonIsTracking := False;
    ReleaseCapture;
    if FMouseAboveCloseButton then
      FCombo.BarManager.HideAll
    else
      SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarTreeView.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if IsPopup then
    Painter.SysPanelCalcSize(Message.CalcSize_Params^.rgrc[0], FGripRect, FCloseButtonRect,
      FCorner, FCombo, FCombo.AllowResizing, TBarManagerAccess(FCombo.BarManager).ScaleFactor);
  inherited;
end;

procedure TdxBarTreeView.WMNCHitTest(var Message: TWMNCHitTest);
var
  PrevMouseAboveCloseButton: Boolean;
begin
  inherited;
  with Message do
    if PtInRect(FGripRect, SmallPointToPoint(Pos)) then
      Result := GetHitTestByCorner(FCorner)
    else
    begin
      PrevMouseAboveCloseButton := FMouseAboveCloseButton;
      FMouseAboveCloseButton := (GetTopWindow(0) = Handle) and
        ((GetCapture = 0) or FCloseButtonIsTracking) and
        PtInRect(FCloseButtonRect, SmallPointToPoint(Pos));
      if FMouseAboveCloseButton then Result := HTBORDER;
      if PrevMouseAboveCloseButton <> FMouseAboveCloseButton then
        SendMessage(Handle, WM_NCPAINT, 0, 0);
    end;
end;

procedure TdxBarTreeView.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  inherited;
  if FMouseAboveCloseButton then
  begin
    FCloseButtonIsTracking := True;
    SetCapture(Handle);
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarTreeView.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if IsPopup then
    Painter.SysPanelDraw(Handle, FCombo.AllowResizing,
      FMouseAboveCloseButton, FCloseButtonIsTracking, FCloseButtonRect,
      FGripRect, FCorner, TBarManagerAccess(FCombo.BarManager).ScaleFactor);
end;

procedure TdxBarTreeView.WMSysColorChange(var Message: TWMSysColorChange);
begin
  inherited;
  RecreateWnd;
end;

procedure TdxBarTreeView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseAboveCloseButton then
  begin
    FMouseAboveCloseButton := False;
    SendMessage(Handle, WM_NCPAINT, 0, 0);
  end;
end;

procedure TdxBarTreeView.CNNotify(var Message: TWMNotify);
begin
  case Message.NMHdr^.code of
    TVN_DELETEITEM:
      if FCombo.SelectedNode <> nil then
        with PNMTreeView(Pointer(Message.NMHdr))^ do
          if itemOld.hItem = FCombo.SelectedNode.ItemId then
            FCombo.FSelectedNode := nil;
  end;
  inherited;
end;

procedure TdxBarTreeView.Change(Node: TTreeNode);
begin
  inherited;
  if (FCombo.FocusedItemLink <> nil) and IsPopup and (Node <> nil) then
    FCombo.CurText := Node.Text;
end;

procedure TdxBarTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    if csDesigning in FCombo.ComponentState then
      Style := Style and not WS_CHILD or WS_POPUP;
    if IsPopup then
    begin
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
      WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    end;
  end;
end;

procedure TdxBarTreeView.CreateWnd;
begin
  inherited;
  if IsPopup then
  begin
    Windows.SetParent(Handle, 0);
    CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TdxBarTreeView.DblClick;
var
  P: TPoint;
begin
  inherited;
  if FCombo.ChooseByDblClick then
  begin
    GetCursorPos(P);
    Windows.ScreenToClient(Handle, P);
    if GetHitTestInfoAt(P.X, P.Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> [] then
      SaveAndHide;
  end;
end;

procedure TdxBarTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then SaveAndHide;
end;

procedure TdxBarTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not FCombo.ChooseByDblClick and (FMouseDownHitTestInfo <> [htOnButton]) and
    (GetHitTestInfoAt(X, Y) * [htOnItem, htOnIcon, htOnLabel, htOnStateIcon] <> []) then
    SaveAndHide;
end;

procedure TdxBarTreeView.UpdateSizeGripCorner(ADropDownPosition: TPoint);
var
  AEditRect, ADropDownRect: TRect;
begin
  if IsPopup and (Combo.CurItemLink <> nil) and (Combo.CurItemLink.Control <> nil) then
  begin
    AEditRect := Combo.CurItemLink.ItemRect;
    AEditRect.TopLeft := Combo.CurItemLink.Control.Parent.ClientToScreen(AEditRect.TopLeft);
    AEditRect.BottomRight := Combo.CurItemLink.Control.Parent.ClientToScreen(AEditRect.BottomRight);
    ADropDownRect := cxRectBounds(ADropDownPosition.X, ADropDownPosition.Y, Width, Height);
    FCorner := GetCornerForRects(AEditRect, ADropDownRect);
  end;
end;

procedure TdxBarTreeView.SetFocus;
begin
end;

{ TdxBarTreeViewCombo }

constructor TdxBarTreeViewCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Glyph.LoadFromResource(HInstance, 'DXBARTREEVIEWCOMBO', RT_BITMAP);
  ShowEditor := False;
  FAllowResizing := True;
  FChooseByDblClick := True;
  FShowImageInEdit := True;
  FTreeView := TdxBarTreeView.Create(Self);
  FTreeView.IsPopup := True;
  FTreeView.FCombo := Self;
  if not (csDesigning in ComponentState) then
    FTreeView.Parent := BarManager.Owner;
end;

destructor TdxBarTreeViewCombo.Destroy;
begin
  FreeAndNil(FTreeView);
  inherited;
end;

function TdxBarTreeViewCombo.GetDropDownHeight: Integer;
begin
  Result := FTreeView.Height;
end;

function TdxBarTreeViewCombo.GetDropDownWidth: Integer;
begin
  Result := FTreeView.Width;
end;

function TdxBarTreeViewCombo.GetImages: TCustomImageList;
begin
  Result := FTreeView.Images;
end;

function TdxBarTreeViewCombo.GetIndent: Integer;
begin
  Result := FTreeView.Indent;
end;

function TdxBarTreeViewCombo.GetItems: TTreeNodes;
begin
  Result := FTreeView.Items;
end;

function TdxBarTreeViewCombo.GetShowButtons: Boolean;
begin
  Result := FTreeView.ShowButtons;
end;

function TdxBarTreeViewCombo.GetShowLines: Boolean;
begin
  Result := FTreeView.ShowLines;
end;

function TdxBarTreeViewCombo.GetShowRoot: Boolean;
begin
  Result := FTreeView.ShowRoot;
end;

function TdxBarTreeViewCombo.GetSortType: TSortType;
begin
  Result := FTreeView.SortType;
end;

function TdxBarTreeViewCombo.GetStateImages: TCustomImageList;
begin
  Result := FTreeView.StateImages;
end;

function TdxBarTreeViewCombo.GetOnExpanded: TTVExpandedEvent;
begin
  Result := FTreeView.OnExpanded;
end;

function TdxBarTreeViewCombo.GetOnExpanding: TTVExpandingEvent;
begin
  Result := FTreeView.OnExpanding;
end;

function TdxBarTreeViewCombo.GetOnChanging: TTVChangingEvent;
begin
  Result := FTreeView.OnChanging;
end;

function TdxBarTreeViewCombo.GetOnCollapsed: TTVExpandedEvent;
begin
  Result := FTreeView.OnCollapsed;
end;

function TdxBarTreeViewCombo.GetOnCollapsing: TTVCollapsingEvent;
begin
  Result := FTreeView.OnCollapsing;
end;

function TdxBarTreeViewCombo.GetOnCompare: TTVCompareEvent;
begin
  Result := FTreeView.OnCompare;
end;

function TdxBarTreeViewCombo.GetOnGetImageIndex: TTVExpandedEvent;
begin
  Result := FTreeView.OnGetImageIndex;
end;

function TdxBarTreeViewCombo.GetOnGetSelectedIndex: TTVExpandedEvent;
begin
  Result := FTreeView.OnGetSelectedIndex;
end;

function TdxBarTreeViewCombo.GetOnTreeViewChange: TTVChangedEvent;
begin
  Result := FTreeView.OnChange;
end;

procedure TdxBarTreeViewCombo.SetDropDownHeight(Value: Integer);
begin
  if Value < 100 then Value := 100;
  FTreeView.Height := Value;
end;

procedure TdxBarTreeViewCombo.SetDropDownWidth(Value: Integer);
begin
  if Value < 100 then Value := 100;
  FTreeView.Width := Value;
end;

procedure TdxBarTreeViewCombo.SetImages(Value: TCustomImageList);
begin
  FTreeView.Images := Value;
end;

procedure TdxBarTreeViewCombo.SetIndent(Value: Integer);
begin
  FTreeView.Indent := Value;
end;

procedure TdxBarTreeViewCombo.SetItems(Value: TTreeNodes);
begin
  FTreeView.Items := Value;
end;

procedure TdxBarTreeViewCombo.SetSelectedNode(Value: TTreeNode);
begin
  if FSelectedNode <> Value then
  begin
    FSelectedNode := Value;
    DoSelectedNodeChanged;
  end;
end;

procedure TdxBarTreeViewCombo.SetShowButtons(Value: Boolean );
begin
  FTreeView.ShowButtons := Value;
end;

procedure TdxBarTreeViewCombo.SetShowImageInEdit(Value: Boolean);
begin
  if FShowImageInEdit <> Value then
  begin
    FShowImageInEdit := Value;
    if (Images <> nil) or (StateImages <> nil) then UpdateEx;
  end;
end;

procedure TdxBarTreeViewCombo.SetShowLines(Value: Boolean);
begin
  FTreeView.ShowLines := Value;
end;

procedure TdxBarTreeViewCombo.SetShowRoot(Value: Boolean);
begin
  FTreeView.ShowRoot := Value;
end;

procedure TdxBarTreeViewCombo.SetSortType(Value: TSortType);
begin
  FTreeView.SortType := Value;
end;

procedure TdxBarTreeViewCombo.SetStateImages(Value: TCustomImageList);
begin
  FTreeView.StateImages := Value;
end;

procedure TdxBarTreeViewCombo.SetOnExpanded(Value: TTVExpandedEvent);
begin
  FTreeView.OnExpanded := Value;
end;

procedure TdxBarTreeViewCombo.SetOnExpanding(Value: TTVExpandingEvent);
begin
  FTreeView.OnExpanding := Value;
end;

procedure TdxBarTreeViewCombo.SetOnChanging(Value: TTVChangingEvent);
begin
  FTreeView.OnChanging := Value;
end;

procedure TdxBarTreeViewCombo.SetOnCollapsed(Value: TTVExpandedEvent);
begin
  FTreeView.OnCollapsed := Value;
end;

procedure TdxBarTreeViewCombo.SetOnCollapsing(Value: TTVCollapsingEvent);
begin
  FTreeView.OnCollapsing := Value;
end;

procedure TdxBarTreeViewCombo.SetOnCompare(Value: TTVCompareEvent);
begin
  FTreeView.OnCompare := Value;
end;

procedure TdxBarTreeViewCombo.SetOnGetImageIndex(Value: TTVExpandedEvent);
begin
  FTreeView.OnGetImageIndex := Value;
end;

procedure TdxBarTreeViewCombo.SetOnGetSelectedIndex(Value: TTVExpandedEvent);
begin
  FTreeView.OnGetSelectedIndex := Value;
end;

procedure TdxBarTreeViewCombo.SetOnTreeViewChange(Value: TTVChangedEvent);
begin
  FTreeView.OnChange := Value;
end;

procedure TdxBarTreeViewCombo.FormSize(Sender: TObject);
var
  H, W, D: Integer;
begin
  W := 12 * FForm.Canvas.TextWidth('0');
  H := MulDiv(FForm.Canvas.TextHeight('0'), 5, 3);
  D := H div 4;

  with FFormTreeView do
  begin
    Left := D;
    Top := D;
    Width := FForm.ClientWidth - (D + D + W + D);
    Height := FForm.ClientHeight - (D + D);
  end;
  FButtonOk.SetBounds(FForm.ClientWidth - D - W, D, W, H);
  FButtonCancel.SetBounds(FButtonOk.Left, FButtonOk.Top + FButtonOk.Height + D, W, H);
end;

procedure TdxBarTreeViewCombo.CheckDropDownPoint(var X, Y: Integer);
begin
  inherited CheckDropDownPoint(X, Y);
  TreeView.UpdateSizeGripCorner(cxPoint(X, Y));
end;

function TdxBarTreeViewCombo.CheckKeyForDropDownWindow(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Key = VK_RETURN) or inherited CheckKeyForDropDownWindow(Key, Shift);
end;

function TdxBarTreeViewCombo.DoCanSelectNode: Boolean;
begin
  Result := True;
  if Assigned(FOnCanSelectNode) then
    FOnCanSelectNode(Self, TreeView.Selected, Result);
end;

procedure TdxBarTreeViewCombo.DoSelectedNodeChanged;
var
  AText: string;
begin
  FInSelectedNodeChanged := True;
  try
    if SelectedNode = nil then
      AText := ''
    else
      AText := SelectedNode.Text;
    if Text = AText then
      Change
    else
      Text := AText;
    Update;
  finally
    FInSelectedNodeChanged := False;
  end;
end;

procedure TdxBarTreeViewCombo.DrawInterior(ABarEditControl: TdxBarEditControl; ACanvas: TCanvas;
  R: TRect; ItemLink: TdxBarItemLink);
var
  DC: HDC;
  ANode: TTreeNode;
  AIndex: Integer;
  S: string;
begin
  if not HasImageInEdit then
    inherited
  else
  begin
    if FocusedItemLink = ItemLink then
      ANode := FTreeView.Selected
    else
      ANode := SelectedNode;
    DC := ACanvas.Handle;
    FillRect(DC, R, ACanvas.Brush.Handle);
    with R do
    begin
      Inc(Left);
      if (StateImages <> nil) and (ANode <> nil) and
        (0 <= ANode.StateIndex) and (ANode.StateIndex < StateImages.Count) then
        with StateImages do
        begin
          Draw(ACanvas, Left, (Top + Bottom - Height) div 2, ANode.StateIndex);
          Inc(Left, Width);
        end;
      if Images <> nil then
        with Images do
        begin
          if ANode = nil then
            AIndex := -1
          else
            if (0 <= ANode.SelectedIndex) and (ANode.SelectedIndex < Count) then
              AIndex := ANode.SelectedIndex
            else
              if (0 <= ANode.ImageIndex) and (ANode.ImageIndex < Count) then
                AIndex := ANode.ImageIndex
              else
                AIndex := -1;
          if AIndex <> -1 then
            Draw(ACanvas, Left, (Top + Bottom - Height) div 2, AIndex);
          Inc(Left, Width + 3);
        end;
      if FocusedItemLink <> nil then
        S := CurText
      else
        S := Text;
      //Canvas.TextOut(Left + 2, (Top + Bottom - Canvas.TextHeight(S)) div 2, S);
      Inc(Left, 2);
      Dec(Right, 2);
      cxDrawText(DC, S, R, DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER);
    end;
  end;
end;

procedure TdxBarTreeViewCombo.DropDown(X, Y: Integer);
begin
  InternalInitDropDownWindow(FTreeView);
  if FFullExpand then FTreeView.FullExpand;
  if CurText <> Text then
    FTreeView.Selected := FTreeView.FindNode(CurText);
  if (FTreeView.Selected = nil) and (FTreeView.Items.Count > 0) then
    with FTreeView.Items[0] do
    begin
      Focused := True;
      MakeVisible;
    end;
  if FTreeView.Selected <> nil then
    FTreeView.Selected.MakeVisible;
  inherited;
end;

function TdxBarTreeViewCombo.GetDropDownWindow: HWND;
begin
  Result := inherited GetDropDownWindow;
  if Result = 0 then Result := FTreeView.Handle;
end;

function TdxBarTreeViewCombo.HasImageInEdit: Boolean;
begin
  Result := FShowImageInEdit and ((Images <> nil) or (StateImages <> nil));
end;

procedure TdxBarTreeViewCombo.Loaded;
begin
  inherited;
  Text := FLoadedText;
end;

procedure TdxBarTreeViewCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FTreeView then FTreeView := nil;
end;

procedure TdxBarTreeViewCombo.SetText(Value: string);
begin
  if Text <> Value then
    if csLoading in ComponentState then FLoadedText := Value
    else
    begin
      if not FInSelectedNodeChanged then
        FSelectedNode := FTreeView.FindNode(Value);
      inherited;
    end;
end;

procedure TdxBarTreeViewCombo.DoClick;
var
  W, H, D, I, J: Integer;
begin
  inherited;
  if Assigned(OnClick) or ReadOnly then Exit;

  FForm := TForm.Create(nil);
  with FForm do
  begin
    if FAllowResizing then
      BorderIcons := []
    else
      BorderStyle := bsDialog;
    Caption := cxGetResourceString(@dxSBAR_TREEVIEWDIALOGCAPTION);
    Font := BarManager.Font;
    Position := poScreenCenter;

    FFormTreeView := TdxBarTreeView.Create(FForm);
    with FFormTreeView do
    begin
      FCombo := Self;
      Visible := True;
      Parent := FForm;

      Images := FTreeView.Images;
      Indent := FTreeView.Indent;
      Items.Assign(FTreeView.Items);
      ShowButtons := FTreeView.ShowButtons;
      ShowLines := FTreeView.ShowLines;
      ShowRoot := FTreeView.ShowRoot;
      SortType := FTreeView.SortType;
      StateImages := FTreeView.StateImages;

      HandleNeeded;
      while (ClientHeight <> FTreeView.ClientHeight) or
        (ClientWidth <> FTreeView.ClientWidth) do
      begin
        ClientHeight := FTreeView.ClientHeight;
        ClientWidth := FTreeView.ClientWidth;
      end;
    end;
    FButtonOk := TButton.Create(FForm);
    with FButtonOk do
    begin
      Caption := cxGetResourceString(@dxSBAR_DIALOGOK);
      Default := True;
      ModalResult := mrOk;
      Parent := FForm;
    end;
    FButtonCancel := TButton.Create(FForm);
    with FButtonCancel do
    begin
      Caption := cxGetResourceString(@dxSBAR_DIALOGCANCEL);
      Cancel := True;
      ModalResult := mrCancel;
      Parent := FForm;
    end;

    Canvas.Font := Font;
    W := 12 * Canvas.TextWidth('0');
    H := MulDiv(Canvas.TextHeight('0'), 5, 3);
    D := H div 4;

    with FFormTreeView do
    begin
      Left := D;
      Top := D;
    end;
    FButtonOk.SetBounds(FFormTreeView.BoundsRect.Right + D, D, W, H);
    FButtonCancel.SetBounds(FButtonOk.Left, FButtonOk.Top + FButtonOk.Height + D, W, H);
    I := D + FFormTreeView.Width + D + W + D;
    J := D + FFormTreeView.Height + D;
    while (I <> ClientWidth) or (J <> ClientHeight) do
    begin
      ClientWidth := I;
      ClientHeight := J;
    end;

    OnResize := FormSize;
    if FFullExpand then FFormTreeView.FullExpand;
    FFormTreeView.Selected := FFormTreeView.FindNode(Text);
    if (FFormTreeView.Selected = nil) and (FFormTreeView.Items.Count > 0) then
      with FFormTreeView.Items[0] do
      begin
        Focused := True;
        MakeVisible;
      end;
    if (ShowModal = mrOk) and (FFormTreeView.Selected <> nil) then
      Text := FFormTreeView.Selected.Text;
    while (FTreeView.ClientHeight <> FFormTreeView.ClientHeight) or
      (FTreeView.ClientWidth <> FFormTreeView.ClientWidth) do
    begin
      FTreeView.ClientHeight := FFormTreeView.ClientHeight;
      FTreeView.ClientWidth := FFormTreeView.ClientWidth;
    end;
    Free;
  end;
end;

{ TdxBarTreeViewComboControl }

function TdxBarTreeViewComboControl.GetItem: TdxBarTreeViewCombo;
begin
  Result := TdxBarTreeViewCombo(ItemLink.Item);
end;

function TdxBarTreeViewComboControl.GetDefaultHeight: Integer;
var
  AItem: TdxBarTreeViewCombo;
  Value: Integer;
begin
  Result := inherited GetDefaultHeight;
  AItem := Item;
  if not Parent.IsVertical and AItem.HasImageInEdit then
  begin
    if AItem.Images = nil then
      Value := 0
    else
      Value := AItem.Images.Height;
    if (AItem.StateImages <> nil) and (AItem.StateImages.Height > Value) then
      Value := AItem.StateImages.Height;
    Value := 2 + 1 + Value + 1 + 2;
    if Value > Result then Result := Value;
  end;
end;

procedure TdxBarTreeViewComboControl.SetFocused(Value: Boolean);
begin
  inherited;
  if Value then
    with Item do
      if SelectedNode = nil then
        TreeView.Selected := TreeView.FindNode(Text)
      else
        TreeView.Selected := SelectedNode;
end;

{ TdxBarImageCombo }

constructor TdxBarImageCombo.Create(AOwner: TComponent);
begin
  inherited;
  Glyph.LoadFromResource(HInstance, 'DXBARIMAGECOMBO', RT_BITMAP);
  ShowEditor := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FShowText := True;
end;

destructor TdxBarImageCombo.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  inherited;
end;

function TdxBarImageCombo.GetImageIndexes(Index: Integer): Integer;
begin
  Result := Integer(Items.Objects[Index]) - 1;
end;

procedure TdxBarImageCombo.SetImageIndexes(Index: Integer; Value: Integer);
begin
  Items.Objects[Index] := TObject(Value + 1);
  if Index = ItemIndex then Update;
end;

procedure TdxBarImageCombo.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TdxBarImageCombo.SetShowText(Value: Boolean);
begin
  if FShowText <> Value then
  begin
    FShowText := Value;
    Update;
  end
end;

procedure TdxBarImageCombo.ImageListChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then ImagesChanged;
end;

procedure TdxBarImageCombo.ReadImageIndexes(Reader: TReader);
var
  I: Integer;
begin
  Reader.ReadListBegin;
  for I := 0 to Items.Count - 1 do
    if Reader.EndOfList then Break
    else
      ImageIndexes[I] := Reader.ReadInteger;
  Reader.ReadListEnd;
end;

procedure TdxBarImageCombo.WriteImageIndexes(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Items.Count - 1 do
    Writer.WriteInteger(ImageIndexes[I]);
  Writer.WriteListEnd;
end;

procedure TdxBarImageCombo.DialogListBoxDblClick(Sender: TObject);
begin
  FForm.ModalResult := mrOk;
end;

procedure TdxBarImageCombo.DialogListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  FCanvas := FDialogListBox.Canvas;
  DrawItem(ClickItemLink.Control.Painter, Index, Rect, State);
  FCanvas := nil;
end;

procedure TdxBarImageCombo.DialogListBoxMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
begin
  FCanvas := FDialogListBox.Canvas;
  MeasureItem(Index, Height);
  FCanvas := nil;
end;

procedure TdxBarImageCombo.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ImageIndexes', ReadImageIndexes, WriteImageIndexes, True);
end;

procedure TdxBarImageCombo.DrawItem(APainter: TdxBarPainter;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  OriginX, AImageIndex: Integer;
  R: TRect;
  S: string;
begin
  if Assigned(OnDrawItem) or (Images = nil) then
    inherited
  else
    with Canvas, ARect do
    begin
      FillRect(ARect);
      with Images do
      begin
        if FShowText then
          OriginX := Left + 1
        else
          OriginX := (Left + Right - Width) div 2;
        R := Bounds(OriginX, (Top + Bottom - Height) div 2, Width, Height);
        if AIndex <> -1 then
        begin
          AImageIndex := ImageIndexes[AIndex];
          if (0 <= AImageIndex) and (AImageIndex < Count) then
            Draw(Canvas, R.Left, R.Top, AImageIndex)
          else
            if FocusedItemLink = nil then R.Right := R.Left;
        end
        else
          if FocusedItemLink = nil then R.Right := R.Left;
      end;
      if FShowText then
      begin
        if AIndex = -1 then
          S := Text
        else
          S := Items[AIndex];
        TextOut(R.Right + 2, (Top + Bottom - TextHeight(S)) div 2, S);
      end;
      if odFocused in AState then Windows.DrawFocusRect(Handle, ARect); // for hiding focus rect
    end;
end;

procedure TdxBarImageCombo.ImagesChanged;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Links[I].Control is TdxBarImageComboControl then
      TdxBarImageComboControl(Links[I].Control).ImagesChanged;
end;

procedure TdxBarImageCombo.MeasureItem(AIndex: Integer; var AHeight: Integer);
begin
  if Assigned(OnMeasureItem) then inherited
  else
  begin
    inherited;
    if (Images <> nil) and (1 + Images.Height + 1 > AHeight) then
      AHeight := 1 + Images.Height + 1;
  end;
end;

procedure TdxBarImageCombo.MeasureItemWidth(AIndex: Integer; var AWidth: Integer);
begin
  inherited;
  if Images <> nil then
  begin
    if not FShowText then AWidth := 0;
    Inc(AWidth, 1 + Images.Width + 1);
  end;
end;

procedure TdxBarImageCombo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

procedure TdxBarImageCombo.DoClick;
var
  W, H, D, C: Integer;
  FButtonOk, FButtonCancel: TButton;
begin
  inherited;
  if Assigned(OnClick) or ReadOnly then Exit;

  FForm := TForm.Create(nil);
  with FForm do
  begin
    BorderStyle := bsDialog;
    Caption := cxGetResourceString(@dxSBAR_IMAGEDIALOGCAPTION);
    Font := BarManager.Font;
    Position := poScreenCenter;

    Canvas.Font := Font;
    W := 12 * Canvas.TextWidth('0');
    H := MulDiv(Canvas.TextHeight('0'), 5, 3);
    D := H div 4;

    FDialogListBox := TListBox.Create(FForm);
    with FDialogListBox do
    begin
      Parent := FForm;
      Items.Assign(Self.Items);
      FCanvas := Canvas;
      if Items.Count < DropDownCount then C := Items.Count
      else C := DropDownCount;
      ClientHeight := ItemsHeight[0] * C;
      if Height < H + D + H then Height := H + D + H;
      ClientWidth := GetDropDownWidth - (2 + 2);
      FCanvas := nil;
      Style := lbOwnerDrawVariable;
      OnDblClick := DialogListBoxDblClick;
      OnDrawItem := DialogListBoxDrawItem;
      OnMeasureItem := DialogListBoxMeasureItem;
    end;
    FButtonOk := TButton.Create(FForm);
    with FButtonOk do
    begin
      Caption := cxGetResourceString(@dxSBAR_DIALOGOK);
      Default := True;
      ModalResult := mrOk;
      Parent := FForm;
    end;
    FButtonCancel := TButton.Create(FForm);
    with FButtonCancel do
    begin
      Caption := cxGetResourceString(@dxSBAR_DIALOGCANCEL);
      Cancel := True;
      ModalResult := mrCancel;
      Parent := FForm;
    end;

    ClientWidth := D + FDialogListBox.Width + D + W + D;
    ClientHeight := D + FDialogListBox.Height + D;
    with FDialogListBox do
    begin
      Left := D;
      Top := D;
    end;
    FButtonOk.SetBounds(ClientWidth - D - W, D, W, H);
    FButtonCancel.SetBounds(FButtonOk.Left, FButtonOk.Top + FButtonOk.Height + D, W, H);

    FDialogListBox.ItemIndex := ItemIndex;
    if ShowModal = mrOk then ItemIndex := FDialogListBox.ItemIndex;
    Free;
  end;
end;

{ TdxBarImageComboControl }

function TdxBarImageComboControl.GetDefaultHeight: Integer;
var
  AItem: TdxBarImageCombo;
  Value: Integer;
begin
  Result := inherited GetDefaultHeight;
  AItem := TdxBarImageCombo(Item);
  if not Parent.IsVertical and (AItem.Images <> nil) then
  begin
    Value := 2 + 1 + AItem.Images.Height + 1 + 2;
    if Value > Result then Result := Value;
  end;
end;

procedure TdxBarImageComboControl.ImagesChanged;
begin
  Parent.RepaintBar;
end;

{ TdxBarToolbarsListItem }

function TdxBarToolbarsListItem.InternalCanMergeWith(AItem: TdxBarItem): Boolean;
begin
  Result := AItem is TdxBarToolbarsListItem;
end;

{ TdxBarToolbarsListItemControl }

procedure TdxBarToolbarsListItemControl.CreateSubMenuControl;
begin
  if BarManager.IsCustomizing then Exit;
  BarDesignController.InitToolBarPopup(Item.ItemLinks);
  Item.ItemLinks.BarControl := TdxBarSubMenuControl.Create(BarManager);
  SubMenuControl.ItemLinks := Item.ItemLinks;
end;

{ TdxBarSpinEdit }

constructor TdxBarSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  FIncrement := 1;
  FPrefixPlace := ppEnd;
  Text := '0';
end;

function TdxBarSpinEdit.GetCurValue: Extended;
begin
  Result := TextToValue(CurText);
end;

function TdxBarSpinEdit.GetIntCurValue: Integer;
begin
  Result := Trunc(CurValue);
end;

function TdxBarSpinEdit.GetIntValue: Integer;
begin
  Result := Trunc(Value);
end;

function TdxBarSpinEdit.GetValue: Extended;
begin
  Result := TextToValue(Text);
end;

procedure TdxBarSpinEdit.SetCurValue(Value: Extended);
begin
  Value := GetCheckedValue(Value);
  if CurValue <> Value then CurText := ValueToText(Value);
end;

procedure TdxBarSpinEdit.SetIncrement(Value: Extended);
begin
  PrepareValue(Value);
  case FValueType of
    svtInteger:
      if Value < 1 then Value := 1;
    svtFloat:
      if Value <= 0 then Value := 1;
  end;
  FIncrement := Value;
end;

procedure TdxBarSpinEdit.SetIntCurValue(Value: Integer);
begin
  CurValue := Value;
end;

procedure TdxBarSpinEdit.SetIntValue(Value: Integer);
begin
  Self.Value := Value;
end;

procedure TdxBarSpinEdit.SetMaxValue(Value: Extended);
begin
  PrepareValue(Value);
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    if FMinValue > FMaxValue then FMinValue := FMaxValue;
    Self.Value := GetCheckedValue(Self.Value);
    CurValue := GetCheckedValue(CurValue);
  end;
end;

procedure TdxBarSpinEdit.SetMinValue(Value: Extended);
begin
  PrepareValue(Value);
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    if FMaxValue < FMinValue then FMaxValue := FMinValue;
    Self.Value := GetCheckedValue(Self.Value);
    CurValue := GetCheckedValue(CurValue);
  end;
end;

procedure TdxBarSpinEdit.SetPrefix(const Value: string);
var
  AValue: Extended;
begin
  AValue := Self.Value;
  if FPrefix <> Value then
  begin
    FPrefix := Value;
    Text := ValueToText(AValue);
  end;
end;

procedure TdxBarSpinEdit.SetPrefixPlace(Value: TdxBarSpinEditPrefixPlace);
begin
  if FPrefixPlace <> Value then
  begin
    FPrefixPlace := Value;
    Text := ValueToText(Self.Value);
  end;
end;

procedure TdxBarSpinEdit.SetValue(Value: Extended);
begin
  Value := GetCheckedValue(Value);
  if Self.Value <> Value then Text := ValueToText(Value);
end;

procedure TdxBarSpinEdit.SetValueType(Value: TdxBarSpinEditValueType);
var
  PrevValue, PrevCurValue: Extended;
begin
  if FValueType <> Value then
  begin
    PrevValue := Self.Value;
    PrevCurValue := Self.CurValue;
    FValueType := Value;
    if Value = svtInteger then
    begin
      Increment := Increment;
      MinValue := MinValue;
      MaxValue := MaxValue;
      Self.Value := PrevValue;
      CurValue := PrevCurValue;
    end;
  end;
end;

function TdxBarSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1;
end;

function TdxBarSpinEdit.IsMaxValueStored: Boolean;
begin
  Result := FMaxValue <> 0;
end;

function TdxBarSpinEdit.IsMinValueStored: Boolean;
begin
  Result := FMinValue <> 0;
end;

function TdxBarSpinEdit.IsValueStored: Boolean;
begin
  Result := Value <> 0;
end;

procedure TdxBarSpinEdit.AddPrefix(var Text: string);
begin
  if FPrefixPlace = ppEnd then
    Text := Text + FPrefix
  else
    Text := FPrefix + Text;
end;

procedure TdxBarSpinEdit.RemovePrefix(var Text: string);
var
  P: Integer;
begin
  P := Pos(FPrefix, Text);
  if P <> 0 then Delete(Text, P, Length(FPrefix));
end;

function TdxBarSpinEdit.CheckRange: Boolean;
begin
  Result := (FMinValue <> FMaxValue) or (FMinValue <> 0);
end;

procedure TdxBarSpinEdit.DoButtonClick(Button: TdxBarSpinEditButton);
begin
  case Button of
    sbUp: CurValue := CurValue + Increment;
    sbDown: CurValue := CurValue - Increment;
  end;
  if Assigned(FOnButtonClick) then FOnButtonClick(Self, Button);
end;

function TdxBarSpinEdit.GetCheckedValue(Value: Extended): Extended;
begin
  Result := Value;
  PrepareValue(Result);
  if CheckRange then
  begin
    if Result < FMinValue then Result := FMinValue;
    if Result > FMaxValue then Result := FMaxValue;
  end;
end;

procedure TdxBarSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
const
  Buttons: array[Boolean] of Integer = (secButtonDown, secButtonUp);
var
  Control: TdxBarSpinEditControl;
begin
  inherited;
  if (Key in [VK_UP, VK_DOWN]) and (FocusedItemLink <> nil) and not ReadOnly then
  begin
    Control := TdxBarSpinEditControl(FocusedItemLink.Control);
    if Control.FTimerID = 0 then
    begin
      Control.ActiveButtonIndex := Buttons[Key = VK_UP];
      DoButtonClick(Control.ActiveButton);
    end;
    Key := 0;
  end;
end;

procedure TdxBarSpinEdit.KeyPress(var Key: Char);
var
  AKeySet: set of AnsiChar;
begin
  inherited;
  AKeySet := [Chr(VK_BACK), ^C, ^V, ^X, '0'..'9'];
  if FMinValue < 0 then
    Include(AKeySet, '-');
  if FValueType = svtFloat then
    AKeySet := AKeySet + [dxFormatSettings.DecimalSeparator];
  if not dxCharInSet(Key, AKeySet) then
    Key := #0;
end;

procedure TdxBarSpinEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FocusedItemLink <> nil then
    with TdxBarSpinEditControl(FocusedItemLink.Control) do
      if (Key in [VK_UP, VK_DOWN]) and (FTimerID = 0) then
        ActiveButtonIndex := - 1;
end;

procedure TdxBarSpinEdit.PrepareValue(var Value: Extended);
begin
  if FValueType = svtInteger then Value := Trunc(Value);
end;

function TdxBarSpinEdit.TextToValue(Text: string): Extended;
begin
  RemovePrefix(Text);
  try
    if FValueType = svtInteger then
      Result := StrToInt(Text)
    else
      Result := StrToFloat(Text);
  except
    on EConvertError do Result := FMinValue;
  end;
end;

procedure TdxBarSpinEdit.SetText(Value: string);
begin
  RemovePrefix(Value);
  try
    if FValueType = svtInteger then
      StrToInt(Value)
    else
      StrToFloat(Value);
  except
    on EConvertError do Exit;
  end;
  inherited SetText(ValueToText(GetCheckedValue(TextToValue(Value))));
end;

function TdxBarSpinEdit.ValueToText(Value: Extended): string;
begin
  if FValueType = svtInteger then
    Result := IntToStr(Trunc(Value))
  else
    Result := FloatToStr(Value);
  AddPrefix(Result);
end;

{ TdxBarSpinEditControl }

function TdxBarSpinEditControl.GetActiveButton: TdxBarSpinEditButton;
begin
  case FActiveButtonIndex of
    secButtonUp: Result := sbUp;
    secButtonDown: Result := sbDown
  else
    Result := sbNone;
  end;
end;

function TdxBarSpinEditControl.GetButtonPressed: Boolean;
begin
  Result := ActiveButtonIndex in [secButtonUp, secButtonDown];
end;

function TdxBarSpinEditControl.GetDrawParams: TdxBarSpinEditDrawParams;
begin
  Result := TdxBarSpinEditDrawParams(FDrawParams);
end;

function TdxBarSpinEditControl.GetItem: TdxBarSpinEdit;
begin
  Result := TdxBarSpinEdit(ItemLink.Item);
end;

procedure TdxBarSpinEditControl.SetActiveButtonIndex(Value: Integer);
begin
  if Item.ReadOnly then
    FActiveButtonIndex := - 1
  else
    if FActiveButtonIndex <> Value then
    begin
      FActiveButtonIndex  := Value;
      Repaint;
    end;
end;

procedure TdxBarSpinEditControl.BreakProcess;
begin
  if GetCapture = Handle then ReleaseCapture;
  ActiveButtonIndex := - 1;
  if FTimerID <> 0 then
    dxKillTimer(Handle, FTimerID);
end;

procedure TdxBarSpinEditControl.CalcDrawParams(AFull: Boolean = True);
begin
  inherited CalcDrawParams(AFull);
  if AFull then
  begin
    DrawParams.ActiveButtonIndex := ActiveButtonIndex;
    DrawParams.ArrowSize := Painter.GetSpinEditArrowSize(cxRectHeight(ItemBounds));
    DrawParams.IsPressed := Pressed;
  end;
end;

procedure TdxBarSpinEditControl.CalcParts;
begin
  inherited;
  Painter.CalculateSpinEditParts(DrawParams, FParts, FAreaParts);
end;

procedure TdxBarSpinEditControl.CorrectFrameRect(var ARect: TRect);
begin
  Painter.SpinEditCorrectFrameRect(DrawParams, ARect);
end;

procedure TdxBarSpinEditControl.DrawTextField;

  function IsSpinButtonsVisible: Boolean;
  begin
    Result := cxRectWidth(FParts[secButtonUp]) <> 0
  end;

  procedure DrawButton(AButtonIndex: Integer);
  begin
    Painter.SpinEditControlDrawButton(DrawParams, FParts[AButtonIndex], AButtonIndex);
  end;

var
  AIndentsRect, AAdjacentZoneRect: TRect;
begin
  if IsSpinButtonsVisible then
  begin
    AIndentsRect := Painter.GetSpinEditButtonIndents(DrawParams.PaintType);
    AAdjacentZoneRect := cxRectUnion(FParts[secButtonUp], FParts[secButtonDown]);
    AAdjacentZoneRect := cxRectInflate(AAdjacentZoneRect, AIndentsRect);
    Canvas.SetClipRegion(TcxRegion.Create(AAdjacentZoneRect), roAdd);
    Canvas.SetClipRegion(TcxRegion.Create(ViewInfo.Bounds), roIntersect);
    Painter.SpinEditControlDrawButtonsAdjacentZone(DrawParams, AAdjacentZoneRect);
    DrawButton(secButtonUp);
    DrawButton(secButtonDown);
    Canvas.SetClipRegion(TcxRegion.Create(AAdjacentZoneRect), roSubtract);
  end;

  inherited;
end;

function TdxBarSpinEditControl.GetDrawParamsClass: TdxBarItemControlDrawParamsClass;
begin
  Result := TdxBarSpinEditDrawParams;
end;

function TdxBarSpinEditControl.GetPartCount: Integer;
begin
  Result := inherited GetPartCount + 2;
end;

procedure TdxBarSpinEditControl.KillFocus(AHandle: THandle);
begin
  if ButtonPressed then
    BreakProcess;
  inherited;
end;

procedure TdxBarSpinEditControl.WndProc(var Message: TMessage);
var
  ALinkSelf: TcxObjectLink;
begin
  inherited;
  with Message do
    case Msg of
      WM_MOUSEWHEEL:
        if not Item.ReadOnly then
          if SmallInt(HiWord(TWMMouse(Message).Keys)) > 0 then
            Item.DoButtonClick(sbUp)
          else
            Item.DoButtonClick(sbDown);
      WM_CAPTURECHANGED:
        if FTimerID <> 0 then BreakProcess;
      {WM_LBUTTONDBLCLK, }WM_LBUTTONDOWN:
        if not ButtonPressed then
        begin
          FPressedButtonIndex := FHotPartIndex;
          ActiveButtonIndex := FHotPartIndex;
          if ButtonPressed then
          begin
            SetCapture(Handle);
            ALinkSelf := cxAddObjectLink(Self);
            try
              Item.DoButtonClick(ActiveButton);
              if (ALinkSelf.Ref <> nil) and (GetCapture = Handle) then
                FTimerID := SetTimer(Handle, 1, GetDoubleClickTime - 100, nil);
            finally
              cxRemoveObjectLink(ALinkSelf);
            end;
          end;
        end;
      WM_LBUTTONUP:
        begin
          FPressedButtonIndex := -1;
          if FTimerID <> 0 then BreakProcess;
        end;
      WM_MOUSEMOVE:
        if (FTimerID <> 0) then
        begin
          CheckHotTrack(ClientToParent(SmallPointToPoint(TWMMouse(Message).Pos)));
          if FHotPartIndex <> FPressedButtonIndex then
            ActiveButtonIndex := -1
          else
            ActiveButtonIndex := FHotPartIndex;
        end;
      WM_TIMER:
        case wParam of
          1: begin
               KillTimer(Handle, FTimerID);
               FTimerID := SetTimer(Handle, 2, 100, nil);
             end;
          2: if ButtonPressed then
               Item.DoButtonClick(ActiveButton);
        end;
    end;
end;

{ TdxBarControlContainerItem }

constructor TdxBarControlContainerItem.Create(AOwner: TComponent);
begin
  inherited;
  FPlace := TPlaceForm.CreateNew(nil);
  FPlace.BorderStyle := bsNone;
end;

destructor TdxBarControlContainerItem.Destroy;
begin
  Control := nil;
  FreeAndNil(FPlace);
  inherited;
end;

function TdxBarControlContainerItem.GetControlVisible: Boolean;
begin
  Result := (FPlace <> nil) and FPlace.Visible;
end;

function TdxBarControlContainerItem.GetInPlaceControl: Boolean;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
  begin
    Result := (Links[I].Control <> nil) and
      TdxBarControlContainerControl(Links[I].Control).InPlaceControl;
    if Result then Exit;
  end;
  Result := False;
end;

procedure TdxBarControlContainerItem.SetControl(Value: TControl);
begin
  if (Value <> nil) and IsControlAssigned(Value) then
    raise EdxException.Create(dxSBAR_CANTASSIGNCONTROL);
  if FControl <> Value then
  begin
    if FControl <> nil then
    begin
      cxWindowProcController.Remove(FWindowProcObject);
      if not (csDestroying in FControl.ComponentState) and
        not BarManager.Designing then
        FControl.Parent := nil;
    end;
    FControl := Value;
    if FControl <> nil then
    begin
      FControl.FreeNotification(Self);
      FWindowProcObject := cxWindowProcController.Add(FControl, ControlWndProc);
      SaveControlSize;
    end
    else
      Place.Hide;
    UpdateEx;
  end;
end;

procedure TdxBarControlContainerItem.ControlWndProc(var Message: TMessage);

  procedure Default;
  begin
    FWindowProcObject.DefaultProc(Message);
  end;

  function IsSizeChanged: Boolean;
  begin
    Result := (Control.Width <> FPrevControlSize.X) or (Control.Height <> FPrevControlSize.Y);
  end;

begin
  if Message.Msg = CM_RECREATEWND then
  begin
    FIsControlRecreating := True;
    Default;
    FIsControlRecreating := False;
  end
  else
  begin
    Default;
    if (Message.Msg = WM_SIZE) or (Message.Msg = WM_WINDOWPOSCHANGED) and (Message.lParam = 0) then
    begin
      if not (FIsControlRecreating or InPlaceControl) and IsSizeChanged then
      begin
        SaveControlSize;
        UpdateEx;
      end;
    end;
  end;
end;

function TdxBarControlContainerItem.IsControlAssigned(AControl: TControl): Boolean;
var
  I: Integer;
  ABarItem: TdxBarItem;
begin
  Result := True;
  for I := 0 to BarManager.ItemCount - 1 do
  begin
    ABarItem := BarManager.Items[I];
    if (ABarItem is TdxBarControlContainerItem) and (ABarItem <> Self) and
      (TdxBarControlContainerItem(ABarItem).Control = AControl) then Exit;
  end;
  Result := False;
end;

procedure TdxBarControlContainerItem.SaveControlSize;
begin
  FPrevControlSize := Point(Control.Width, Control.Height);
end;

procedure TdxBarControlContainerItem.SetControlVisible(Value: Boolean);
begin
  if ControlVisible <> Value then
  begin
    if FPlace <> nil then
      FPlace.Visible := Value;
  end;
end;

procedure TdxBarControlContainerItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TdxBarControlContainerItem.SetName(const NewName: TComponentName);
begin
  inherited;
  if (Control = nil) and not IsLoading then
    UpdateEx;
end;

function TdxBarControlContainerItem.CanClicked: Boolean;
begin
  Result := False;
end;

function TdxBarControlContainerItem.GetHidden: Boolean;
begin
  Result := True;
end;

function TdxBarControlContainerItem.HasAccel(AItemLink: TdxBarItemLink): Boolean;
begin
  Result := (AItemLink.Control <> nil) and (AItemLink.Control.Parent.Kind = bkBarQuickControl);
end;

procedure TdxBarControlContainerItem.HideControl(AControl: TdxBarItemControl);
begin
  if TdxBarControlContainerControl(AControl).ShowsControl then
  begin
    FPlace.Visible := False;
    FPlace.Enabled := False;
    FPlace.ParentWindow := 0;
    FPlace.Enabled := True;
  end;
end;

function TdxBarControlContainerItem.NeedToBeHidden: Boolean;
begin
  Result := True;
end;

{ TdxBarControlContainerControl }

destructor TdxBarControlContainerControl.Destroy;
begin
  Item.HideControl(Self);
  inherited Destroy;
end;

function TdxBarControlContainerControl.HasWindow: Boolean;
begin
  Result := Control is TWinControl;
end;

function TdxBarControlContainerControl.GetControlDPI: Integer;
var
  AIntf: IdxScaleFactor;
begin
  if Supports(Control, IdxScaleFactor, AIntf) then
    Result := AIntf.Value.TargetDPI
  else
    if (Place <> nil) and FPlacedControl then
      Result := TPlaceForm(Place).ScaleFactor.TargetDPI
    else
      Result := dxGetCurrentDPI(Control);
end;

function TdxBarControlContainerControl.GetItem: TdxBarControlContainerItem;
begin
  Result := TdxBarControlContainerItem(ItemLink.Item);
end;

function TdxBarControlContainerControl.GetPlace: TdxCustomForm;
begin
  Result := Item.Place;
end;

procedure TdxBarControlContainerControl.InternalPaint;
begin
  PlaceControl;
  Place.Invalidate;
  Control.Invalidate;
end;

procedure TdxBarControlContainerControl.PlaceControl;
begin
  if ShowsControl then
  begin
    if FInPlaceControl or Item.InPlaceControl then
      Exit;

    FInPlaceControl := True;
    try
      Place.ScaleForPPI(ScaleFactor.Apply(dxDefaultDPI));
      if not IsRectEmpty(ItemBounds) or (Item.LinkCount = 1) then
      begin
        Place.Font := TControlAccess(Control).Font;
        Place.ParentWindow := Parent.Handle;
      end;
      TPlaceForm(Place).Position := poDesigned;
      if not IsRectEmpty(ItemBounds) or IsShowingControl then
        Place.BoundsRect := ItemBounds;
      Control.Parent := Place;
      if not IsRectEmpty(ItemBounds) then
      begin
        Control.BoundsRect := Control.Parent.ClientRect;
        Control.Visible := True;
      end;
      Place.Brush.Color := cxGetBrushData(Parent.BkBrush).lbColor;
      TPlaceForm(Place).FBarItemControl := Self;
      Place.Visible := True;
      FPlacedControl := True;
    finally
      FInPlaceControl := False;
    end;
  end;
end;

procedure TdxBarControlContainerControl.BeforeDestroyParentHandle;
begin
  inherited;
  if IsShowingControl and (Control is TWinControl) then
  begin
    Control.Parent := nil;
    Place.Visible := False;   // work-around for the controls that
    Place.ParentWindow := 0;  // don't check HandleAllocated
  end;
end;

procedure TdxBarControlContainerControl.CalcParts;
begin
  inherited CalcParts;
  if IsRectEmpty(ItemBounds) and (Place <> nil) then
    Place.Visible := False;
end;

function TdxBarControlContainerControl.CanClicked: Boolean;
begin
  Result := Parent.Kind = bkBarQuickControl;
end;

function TdxBarControlContainerControl.CanSelect: Boolean;
begin
  if Parent.Kind = bkBarQuickControl then
    Result := True
  else
    Result := BarManager.Designing;
end;

function TdxBarControlContainerControl.CanMouseSelect: Boolean;
begin
  Result := not IsSelectionForbidden;
end;

procedure TdxBarControlContainerControl.DoPaint(ARect: TRect;
  PaintType: TdxBarPaintType);
const
  Borders: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  R: TRect;
  DC: HDC;
  ABrush: HBRUSH;
  PrevBkColor: COLORREF;
  S: string;
  APressed: Boolean;
begin
  R := ARect;
  DC := Canvas.Handle;
  if Parent.Kind = bkBarQuickControl then
  begin
    APressed := DrawSelected and Parent.IsActive and MousePressed;
    Painter.DrawGlyph(Self, DC, R, cxEmptyRect, PaintType, False, DrawSelected, False, APressed, False, False, False, False);
    if IconAssigned(GetViewSize) then
      Inc(R.Left, FDrawParams.DefaultButtonSize.cx)
    else
      Inc(R.Left, Parent.Font.Size div 2);
    Painter.OffsetEllipsisBounds(APressed, R);
    Painter.DrawItemText(Self, DC, Caption, R, DT_LEFT, Enabled, False, False, False, False);
  end
  else
    if (Control = nil) or BarManager.Designing then
    begin
      FrameRect(DC, R, GetSysColorBrush(COLOR_BTNSHADOW));
      InflateRect(R, -1, -1);

      ABrush := CreateHatchBrush(HS_BDIAGONAL, GetSysColor(COLOR_BTNSHADOW));

      PrevBkColor := SetBkColor(DC, cxGetBrushData(Parent.BkBrush).lbColor);
      FillRect(DC, R, ABrush);
      SetBkColor(DC, PrevBkColor);
      DeleteObject(ABrush);

      if Control = nil then
        S := Item.Name
      else
        S := cxGetResourceString(@dxSBAR_PLACEFORCONTROL) + Control.Name;
      Painter.DrawItemText(Self, DC, S, R, DT_CENTER, True, False, PaintType = ptVert, True, False);
    end
    else
      InternalPaint;
end;

function TdxBarControlContainerControl.GetControl: TControl;
begin
  if Item <> nil then
    Result := Item.Control
  else
    Result := nil;
end;

function TdxBarControlContainerControl.GetDefaultHeight: Integer;
begin
  if (Control = nil) or (Parent.Kind = bkBarQuickControl) then
  begin
    if Parent.Kind = bkSubMenu then
      Result := GetTextSize
    else
      Result := Max(GetTextSize, FDrawParams.DefaultButtonSize.cy);
  end
  else
    Result := ScaleFactor.Apply(Control.Height, ControlDPI, dxDefaultDPI);
end;

function TdxBarControlContainerControl.GetDefaultWidth: Integer;
begin
  if (Control = nil) or (Parent.Kind = bkBarQuickControl) then
  begin
    if Parent.Kind = bkSubMenu then
      Result := 2 * Parent.TextSize + 3 + GetTextWidth(GetTextOf(Item.Name)) + 3 + Painter.ContainerControlSubMenuOffset
    else
      if Parent.Kind = bkBarQuickControl then
      begin
        if IconAssigned(FDrawParams.ViewSize) then
          Result := GetCaptionWidth + FDrawParams.DefaultButtonSize.cx + 4
        else
          Result := GetCaptionWidth + Parent.Font.Size;
      end
      else
        Result := GetTextWidth(GetTextOf(Item.Name)) + Canvas.Font.Size
  end
  else
    Result := ScaleFactor.Apply(Control.Width, ControlDPI, dxDefaultDPI);
end;

function TdxBarControlContainerControl.GetFocused: Boolean;
begin
  Result := (Control is TWinControl) and TWinControl(Control).Focused;
end;

function TdxBarControlContainerControl.GetPossibleViewLevels: TdxBarItemViewLevels;
begin
  Result := inherited GetPossibleViewLevels;
  if (Control <> nil) and (Control.Height > Parent.Height div 2) then
    Include(Result, ivlLargeControlOnly);
end;

function TdxBarControlContainerControl.CanDestroyOnClick: Boolean;
begin
  Result := Parent.Kind = bkBarQuickControl;
end;

function TdxBarControlContainerControl.IsShowingControl: Boolean;
begin
  Result := (Control <> nil) and (Place.ParentWindow = Parent.Handle);
end;

function TdxBarControlContainerControl.NeedCaptureMouse: Boolean;
begin
  Result := Parent.Kind = bkBarQuickControl;
end;

function TdxBarControlContainerControl.NeedUpdateWhenResize: Boolean;
begin
  Result := Item.Align = iaClient;
end;

procedure TdxBarControlContainerControl.RealVisibleChanging(AVisible: Boolean);
begin
  if FPlacedControl then
    Item.ControlVisible := AVisible;
end;

function TdxBarControlContainerControl.ShowsControl: Boolean;
begin
  Result := (Parent.Kind <> bkBarQuickControl) and (Control <> nil) and not BarManager.Designing;
end;

{ TdxBarProgressItem }

constructor TdxBarProgressItem.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := sbsLowered;
  FColor := clDefault;
  FMax := 100;
  FStep := 10;
end;

procedure TdxBarProgressItem.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateBar;
  end;
end;

procedure TdxBarProgressItem.SetMax(Value: Integer);
var
  AMin, AMax: Integer;
begin
  if FMax <> Value then
  begin
    AMin := FMin;
    AMax := Value;
    if AMin > AMax then AMin := AMax;
    SetParams(AMin, AMax);
  end;
end;

procedure TdxBarProgressItem.SetMin(Value: Integer);
var
  AMin, AMax: Integer;
begin
  if FMin <> Value then
  begin
    AMin := Value;
    AMax := FMax;
    if AMax < AMin then AMax := AMin;
    SetParams(AMin, AMax);
  end;
end;

procedure TdxBarProgressItem.SetPosition(Value: Integer);
begin
  if Value < FMin then Value := FMin;
  if Value > FMax then Value := FMax;
  if FPosition <> Value then
  begin
    FPosition := Value;
    UpdateDifference;
  end;
end;

procedure TdxBarProgressItem.SetSmooth(Value: Boolean);
begin
  if FSmooth <> Value then
  begin
    FSmooth := Value;
    UpdateBar;
  end;
end;

procedure TdxBarProgressItem.SetStep(Value: Integer);
begin
  FStep := Value;
end;

procedure TdxBarProgressItem.UpdateBar;
var
  I: Integer;
begin
  if not IsLoading then
    for I := 0 to LinkCount - 1 do
      if Links[I].Control <> nil then
        TdxBarProgressControl(Links[I].Control).UpdateBar;
end;

procedure TdxBarProgressItem.UpdateDifference;
var
  I: Integer;
begin
  if not IsLoading then
    for I := 0 to LinkCount - 1 do
      if Links[I].Control <> nil then
        TdxBarProgressControl(Links[I].Control).UpdateDifference;
end;

procedure TdxBarProgressItem.SetParams(AMin, AMax: Integer);
begin
  if (FMin <> AMin) or (FMax <> AMax) then
  begin
    FMin := AMin;
    FMax := AMax;
    if IsLoading then Exit;
    if FMin > FMax then FMin := FMax;
    if FPosition < FMin then
      Position := FMin
    else
      if FPosition > FMax then
        Position := FMax
      else
        UpdateBar;
  end;
end;

procedure TdxBarProgressItem.StepBy(Delta: Integer);
begin
  Position := Position + Delta;
end;

procedure TdxBarProgressItem.StepIt;
begin
  if FPosition + FStep > FMax then
    Position := FPosition + FStep - FMax
  else
    if FPosition + FStep < FMin then
      Position := FMax - (FMin - (FPosition + FStep))
    else
      Position := Position + Step;
end;

{ TdxBarProgressControl }

function TdxBarProgressControl.GetDrawParams: TdxBarProgressControlDrawParams;
begin
  Result := TdxBarProgressControlDrawParams(FDrawParams);
end;

function TdxBarProgressControl.GetItem: TdxBarProgressItem;
begin
  Result := TdxBarProgressItem(ItemLink.Item);
end;

function TdxBarProgressControl.CalculateBandDrawPosition(const ABandRect: TRect): Integer;
var
  ALength: Integer;
begin
  if DrawParams.PaintType = ptVert then
    ALength := cxRectHeight(ABandRect)
  else
    ALength := cxRectWidth(ABandRect);
  Result := MulDiv(ALength, Item.Position - Item.Min, Item.Max - Item.Min);
end;

function TdxBarProgressControl.BarBrushColor: TColorRef;
begin
  if Item.Color = clDefault then
    Result := Painter.ProgressControlBarBrushColor
  else
    Result := ColorToRGB(Item.Color);
end;

function TdxBarProgressControl.BarHeight: Integer;
var
  AHeight: Integer;
begin
  if Parent.IsVertical then
    AHeight := cxRectWidth(ItemBounds)
  else
    AHeight := cxRectHeight(ItemBounds);
  Result := MulDiv(AHeight - (BorderOffsets.Top + BorderOffsets.Bottom), 2, 3);
  if Odd(AHeight) <> Odd(Result) then
    Inc(Result);
end;

function TdxBarProgressControl.BarRect: TRect;
var
  W, H, RightOffset: Integer;
begin
  W := BarWidth;
  H := BarHeight;
  RightOffset := W + ProgressBarIndent + BorderOffsets.Right + RightIndent;
  with ItemBounds do
    if Parent.IsVertical then
      Result := Bounds((Left + Right - H) div 2, Bottom - RightOffset, H, W)
    else
      Result := Bounds(Right - RightOffset, (Top + Bottom - H) div 2, W, H);
end;

function TdxBarProgressControl.BarWidth: Integer;

  function GetProgressBarIndent: Integer;
  begin
    Result := ProgressBarIndent;
    if not(cpText in DrawParams.ViewStructure) then
      Result := Result * 2;
  end;

var
  AWidth: Integer;
begin
  if (Width = 0) and (Align <> iaClient) then
    Result := ProgressBarDefaultWidth
  else
  begin
    AWidth := GetRotationDependentWidth(cxRectSize(ItemBounds));
    if (Parent.Kind <> bkSubMenu) and (Align <> iaClient) then
      AWidth := Width;

    Result := AWidth - GetAutoWidth(GetRotationDependentWidth(DrawParams.DefaultButtonSize)) - GetProgressBarIndent;
    if Result < 0 then Result := 0;
  end;
end;

procedure TdxBarProgressControl.CalcDrawParams(AFull: Boolean);
begin
  inherited;
  if AFull then
  begin
    DrawParams.AllowCenter := False;
    DrawParams.Max := Item.Max;
    DrawParams.Min := Item.Min;
    DrawParams.Position := Item.Position;
    DrawParams.Smooth := Item.Smooth;
  end;
end;

function TdxBarProgressControl.CanHaveZeroSize: Boolean;
begin
  Result := True;
end;

procedure TdxBarProgressControl.DrawInterior(ARect: TRect);
var
  BarR: TRect;
  AIndent: Integer;
begin
  BarR := BarRect;

  Canvas.SaveDC;
  try
    Canvas.SetClipRegion(TcxRegion.Create(BarR), roSubtract);
    AIndent := Painter.ProgressControlIndent(DrawParams);

    if AIndent <> 0 then
      with ARect do
        Painter.DrawBackground(Self, Canvas.Handle, Rect(Left, Top, Left + AIndent, Bottom), Parent.BkBrush, False);
    Inc(ARect.Left, AIndent);
    inherited;
  finally
    Canvas.RestoreDC;
  end;

  if not IsRectEmpty(BarR) then
    Painter.ProgressControlDrawBar(DrawParams, BarR, BarBrushColor);

  if Painter.CanUpdateBarPartly then
    FPrevPos := CalculateBandDrawPosition(BarR);
end;

function TdxBarProgressControl.GetAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TdxBarProgressControl.GetDrawParamsClass: TdxBarItemControlDrawParamsClass;
begin
  Result := TdxBarProgressControlDrawParams;
end;

function TdxBarProgressControl.GetPossibleViewLevels: TdxBarItemViewLevels;
begin
  Result := inherited GetPossibleViewLevels + [ivlControlOnly];
end;

function TdxBarProgressControl.InternalGetDefaultHeight: Integer;
var
  ABarHeight: Integer;
begin
  Result := inherited InternalGetDefaultHeight;
  ABarHeight := Painter.ProgressControlBarHeight(Self) + BorderOffsets.Top + BorderOffsets.Bottom + 2;
  if Result < ABarHeight then
    Result := ABarHeight;
end;

function TdxBarProgressControl.InternalGetDefaultWidth: Integer;
begin
  Result := inherited InternalGetDefaultWidth;
  if Width = 0 then
    Inc(Result, (Byte(not (cpText in DrawParams.ViewStructure)) + 1) * ProgressBarIndent +
      ProgressBarDefaultWidth);
  Inc(Result, Painter.ProgressControlIndent(DrawParams));
end;

procedure TdxBarProgressControl.UpdateBar;
var
  R: TRect;
begin
  R := BarRect;
  InvalidateRect(Parent.Handle, @R, False);
end;

procedure TdxBarProgressControl.UpdateDifference;
var
  R: TRect;
  ACurPos: Integer;
  ABeginPos, AEndPos: Integer;
begin
  if Painter.CanUpdateBarPartly then
  begin
    R := BarRect;
    ACurPos := CalculateBandDrawPosition(R);
    if FPrevPos <> ACurPos then
    begin
      if FPrevPos > ACurPos then
      begin
        ABeginPos := ACurPos;
        AEndPos := FPrevPos;
      end
      else
      begin
        ABeginPos := FPrevPos;
        AEndPos := ACurPos;
      end;
      if DrawParams.PaintType = ptVert then
      begin
        R.Bottom := R.Top + AEndPos;
        Inc(R.Top, ABeginPos);
      end
      else
      begin
        R.Right := R.Left + AEndPos;
        Inc(R.Left, ABeginPos);
      end;
      InvalidateRect(Parent.Handle, @R, False);
    end;
  end
  else
    UpdateBar;
end;

{ TdxBarMRUListItem }

constructor TdxBarMRUListItem.Create(AOwner: TComponent);
begin
  inherited;
  FMaxItemCount := 5;
end;

procedure TdxBarMRUListItem.SetMaxItemCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMaxItemCount <> Value then
  begin
    FMaxItemCount := Value;
    CheckItemCount;
  end;
end;

procedure TdxBarMRUListItem.CheckItemCount;
var
  I: Integer;
begin
  if FMaxItemCount = 0 then Exit;
  for I := Items.Count - 1 downto FMaxItemCount do
    Items.Delete(I);
end;

function TdxBarMRUListItem.GetDisplayText(const AText: string): string;
begin
  Result := cxGetStringAdjustedToWidth(0, 0, inherited GetDisplayText(AText), 300, mstPathEllipsis);
end;

procedure TdxBarMRUListItem.DirectClick;
begin
  inherited;
  if ((CurItemLink = nil) or (CurItemLink.Item <> Self)) and
    not BarManager.IsCustomizing and FRemoveItemOnClick then
  begin
    RemoveItem(Items[ItemIndex], nil);
    ItemIndex := -1;
  end;
end;

procedure TdxBarMRUListItem.AddItem(const S: string; AObject: TObject);
var
  I: Integer;
begin
  I := Items.IndexOf(S);
  if (I = -1) and (AObject <> nil) then
    I := Items.IndexOfObject(AObject);
  if I = -1 then
  begin
    Items.InsertObject(0, S, AObject);
    CheckItemCount;
  end
  else
    Items.Move(I, 0);
end;

procedure TdxBarMRUListItem.RemoveItem(const S: string; AObject: TObject);
var
  I: Integer;
begin
  with Items do
  begin
    if S <> '' then
      I := IndexOf(S)
    else
      I := IndexOfObject(AObject);
    if I <> -1 then Delete(I);
  end;
end;

{ TdxBarInPlaceSubItem }

procedure TdxBarInPlaceSubItem.SetExpanded(Value: Boolean);
var
  AList: TList;
  I: Integer;
begin
  if FExpanded <> Value then
  begin
    if not Value then
      for I := 0 to LinkCount - 1 do
        DoBeforeCollapse(Links[I]);
    FExpanded := Value;
    if not IsLoading then
    begin
      AList := TList.Create;
      FExpandedChanging := True;
      try
        for I := 0 to LinkCount - 1 do
          with Links[I] do
            if (Control <> nil) and (Control.Parent is TdxBarSubMenuControl) and
              (AList.IndexOf(Control.Parent) = -1) then
            begin
              AList.Add(Control.Parent);
              Control.Parent.RepaintBar;
            end;
      finally
        FExpandedChanging := False;
        AList.Free;
      end;
    end;
    if Value then
      for I := 0 to LinkCount - 1 do
        DoAfterExpand(Links[I]);
  end;
end;

procedure TdxBarInPlaceSubItem.DeleteListedItemLinks(ALinkData: TObject);
begin
  if FExpanded and not FExpandedChanging or
     not FExpanded and FExpandedChanging then
    inherited;
end;

function TdxBarInPlaceSubItem.HideWhenRun: Boolean;
begin
  Result := False;
end;

function TdxBarInPlaceSubItem.InternalActuallyVisible: Boolean;
begin
  Result := True;
end;

function TdxBarInPlaceSubItem.InternalCanMergeWith(AItem: TdxBarItem): Boolean;
begin
  Result := AItem is TdxBarInPlaceSubItem;
end;

procedure TdxBarInPlaceSubItem.PopulateListedItemLinks(
  AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer);
begin
  if FExpanded then
    inherited PopulateListedItemLinks(AItemLinks, ALinkData, AIndex);
end;

procedure TdxBarInPlaceSubItem.PrepareListedItemLinks;
begin
  if FExpanded then
    inherited PrepareListedItemLinks;
end;

procedure TdxBarInPlaceSubItem.ChangeNextItemLinkBeginGroup(ALink: TdxBarItemLink;
  Value: Boolean);
var
  NextItemLinkIndex: Integer;
begin
  if FKeepBeginGroupWhileExpanded then
    with ALink.Owner do
      if not (BarControl is TdxBarControl) then
      begin
        NextItemLinkIndex := ALink.VisibleIndex + 1;
        if not BarManager.Designing then
          Inc(NextItemLinkIndex, ItemLinks.VisibleItemCount);
        if NextItemLinkIndex <= VisibleItemCount - 1 then
          VisibleItems[NextItemLinkIndex].BeginGroup := Value;
      end;
end;

procedure TdxBarInPlaceSubItem.DoAfterExpand(ALink: TdxBarItemLink);
begin
  if Assigned(FOnAfterExpand) then FOnAfterExpand(Self, ALink);
  ChangeNextItemLinkBeginGroup(ALink, True);
end;

procedure TdxBarInPlaceSubItem.DoBeforeCollapse(ALink: TdxBarItemLink);
begin
  if Assigned(FOnBeforeCollapse) then FOnBeforeCollapse(Self, ALink);
  ChangeNextItemLinkBeginGroup(ALink, False);
end;

{ TdxBarInPlaceSubItemControl }

function TdxBarInPlaceSubItemControl.GetDrawParams: TdxBarInPlaceSubItemControlDrawParams;
begin
  Result := TdxBarInPlaceSubItemControlDrawParams(FDrawParams);
end;

function TdxBarInPlaceSubItemControl.GetItem: TdxBarInPlaceSubItem;
begin
  Result := TdxBarInPlaceSubItem(ItemLink.Item);
end;

procedure TdxBarInPlaceSubItemControl.CalcDrawParams(AFull: Boolean = False);
begin
  inherited CalcDrawParams(AFull);
  if AFull then
    DrawParams.IsExpanded := Item.Expanded;
  if DrawParams.PaintType = ptMenu then
    DrawParams.ArrowSize.cx := Painter.InPlaceSubItemGetArrowWidth(DrawParams);
end;

procedure TdxBarInPlaceSubItemControl.ControlClick(AByMouse: Boolean; AKey: Char = #0);
var
  AOriginalItemLink: TdxBarItemLink;
  AParent: TdxBarSubMenuControl;
  ASelectedItemLinkIndex: Integer;
begin
  inherited;
  if Parent is TdxBarSubMenuControl then
  begin
    AParent := SubMenuParent;
    AOriginalItemLink := ItemLink.OriginalItemLink;
    if IsSelected then
      ASelectedItemLinkIndex := ItemLink.Index
    else
      ASelectedItemLinkIndex := -1;
    with Item do
    begin
      DirectClick;
      Expanded := not Expanded;
    end;
    if ASelectedItemLinkIndex <> -1 then
      with TCustomdxBarControlAccess(AParent) do
        if (ASelectedItemLinkIndex < ItemLinks.VisibleItemCount) and
          (ItemLinks[ASelectedItemLinkIndex].OriginalItemLink = AOriginalItemLink) then
          SelectedControl := ItemLinks[ASelectedItemLinkIndex].Control;
  end;
end;

procedure TdxBarInPlaceSubItemControl.DblClick;
begin
  if Enabled then ControlClick(True);
end;

procedure TdxBarInPlaceSubItemControl.DoPaint(ARect: TRect; PaintType: TdxBarPaintType);
begin
  if PaintType = ptMenu then
    Painter.InPlaceSubItemControlDrawInMenu(DrawParams, ARect)
  else
    inherited;
end;

function TdxBarInPlaceSubItemControl.GetAccessibilityHelperClass: TdxBarAccessibilityHelperClass;
begin
  Result := TdxBarInPlaceSubItemControlAccessibilityHelper;
end;

function TdxBarInPlaceSubItemControl.GetDefaultHeight: Integer;
begin
  Result := inherited GetDefaultHeight;
  if Parent is TdxBarSubMenuControl then Inc(Result);
end;

function TdxBarInPlaceSubItemControl.GetDefaultWidth: Integer;
begin
  if Parent is TdxBarSubMenuControl then
    Result := Painter.InPlaceSubItemGetTextIndent + GetCaptionWidth + DrawParams.ArrowSize.cx
  else
    Result := inherited GetDefaultWidth;
end;

function TdxBarInPlaceSubItemControl.GetDrawParamsClass: TdxBarItemControlDrawParamsClass;
begin
  Result := TdxBarInPlaceSubItemControlDrawParams;
end;

function TdxBarInPlaceSubItemControl.HasSubMenu: Boolean;
begin
  Result := not (Parent is TdxBarSubMenuControl);
end;

function TdxBarInPlaceSubItemControl.IsExpandable: Boolean;
begin
  Result := not (Parent is TdxBarSubMenuControl) and inherited IsExpandable;
end;

function TdxBarInPlaceSubItemControl.IsInvertTextColor: Boolean;
begin
  Result := Parent is TdxBarSubMenuControl;
end;

procedure TdxBarInPlaceSubItemControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_RIGHT:
      ControlClick(False, Char(Key));
  end;
end;

function TdxBarInPlaceSubItemControl.WantsKey(Key: Word): Boolean;
begin
  if Parent is TdxBarSubMenuControl then
    Result :=
      (Key = VK_LEFT) and Item.Expanded or
      (Key = VK_RIGHT) and not Item.Expanded
  else
    Result := inherited WantsKey(Key);
end;

{ TdxBarInPlaceSubItemControlAccessibilityHelper }

function TdxBarInPlaceSubItemControlAccessibilityHelper.ShowDropDownWindow: Boolean;
begin
  Result := inherited ShowDropDownWindow or (ItemControl.Parent.Kind = bkSubMenu);
end;

initialization
  FTrueTypeFontBitmap := TBitmap.Create;
  FTrueTypeFontBitmap.LoadFromResourceName(HInstance, 'DXBARTRUETYPEFONT');
  FNonTrueTypeFontBitmap := TBitmap.Create;
  FNonTrueTypeFontBitmap.LoadFromResourceName(HInstance, 'DXBARNONTRUETYPEFONT');

  dxBarRegisterItem(TdxBarStatic, TdxBarStaticControl, True);
  dxBarRegisterItem(TdxBarColorCombo, TdxBarColorComboControl, True);
  dxBarRegisterItem(TdxBarFontNameCombo, TdxBarComboControl, True);
  dxBarRegisterItem(TdxBarDateCombo, TdxBarDateComboControl, True);
  dxBarRegisterItem(TdxBarTreeViewCombo, TdxBarTreeViewComboControl, True);
  dxBarRegisterItem(TdxBarImageCombo, TdxBarImageComboControl, True);
  dxBarRegisterItem(TdxBarToolbarsListItem, TdxBarToolbarsListItemControl, True);
  dxBarRegisterItem(TdxBarSpinEdit, TdxBarSpinEditControl, True);
  dxBarRegisterItem(TdxBarControlContainerItem, TdxBarControlContainerControl, True);
  dxBarRegisterItem(TdxBarProgressItem, TdxBarProgressControl, True);
  dxBarRegisterItem(TdxBarMRUListItem, TdxBarContainerItemControl, True);
  dxBarRegisterItem(TdxBarInPlaceSubItem, TdxBarInPlaceSubItemControl, True);

finalization
  dxBarUnregisterItem(TdxBarStatic);
  dxBarUnregisterItem(TdxBarColorCombo);
  dxBarUnregisterItem(TdxBarFontNameCombo);
  dxBarUnregisterItem(TdxBarDateCombo);
  dxBarUnregisterItem(TdxBarTreeViewCombo);
  dxBarUnregisterItem(TdxBarImageCombo);
  dxBarUnregisterItem(TdxBarToolbarsListItem);
  dxBarUnregisterItem(TdxBarSpinEdit);
  dxBarUnregisterItem(TdxBarControlContainerItem);
  dxBarUnregisterItem(TdxBarProgressItem);
  dxBarUnregisterItem(TdxBarMRUListItem);
  dxBarUnregisterItem(TdxBarInPlaceSubItem);

  FreeAndNil(FFontDialog);
  FreeAndNil(FColorDialog);
  FreeAndNil(FNonTrueTypeFontBitmap);
  FreeAndNil(FTrueTypeFontBitmap);

end.
