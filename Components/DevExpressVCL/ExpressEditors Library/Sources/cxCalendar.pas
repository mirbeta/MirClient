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

unit cxCalendar;

{$I cxVer.inc}

interface

{.$Define Colorized}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, ComCtrls, Types, Variants, DateUtils,
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Clipbrd, RTLConsts,
  dxCore, dxCoreClasses, cxClasses, cxGraphics, cxControls, cxContainer, cxDataStorage, cxDataUtils, cxGeometry,
  cxEdit, cxDropDownEdit, cxTextEdit, cxMaskEdit, cxButtons, cxDateUtils, dxCalendarUtils, dxGdiPlusClasses,
  cxEditConsts, cxFormats, cxTimeEdit, cxFilterControlUtils, cxLookAndFeels, cxLookAndFeelPainters, dxAnimation,
  dxFading, dxDateTimeWheelPicker;

const
  // hit test constants
  cchtNone                      = 0;
  cchtMonthHeader               = 1;
  cchtYearHeader                = 2;
  cchtDateHeaderText            = 3;
  cchtDateHeaderArrow           = 4;
  cchtDateTableCell             = 5;
  cchtHeader                    = 6;
  cchtBackground                = 7;

type
  TCalendarButton = (btnClear, btnNow, btnToday, btnOk, btnCancel);
  TDateButton = btnClear..btnToday;
  TDateButtons = set of TDateButton;

  TcxCalendarArrow = (caPrevMonth, caNextMonth, caPrevYear, caNextYear, caPrevDecade, caNextDecade,
    caPrevCentury, caNextCentury);
  TcxCalendarKind = (ckDate, ckDateTime);
  TcxCustomCalendar = class;
  TcxCalendarController = class;
  TcxCalendarHitTest = class;
  TcxCalendarPainter = class;
  TcxCalendarElementViewInfo = class;
  TcxCalendarViewInfo = class;
  TcxCalendarClassicViewInfo = class;
  TcxCalendarModernDateViewInfo = class;
  TcxCustomDateEdit = class;

  { TcxClock }

  TcxClock = class(TcxControl, IdxSkinSupport)
  strict private
    FTime: TTime;

    function GetBackgroundColor: TColor;
    procedure SetTime(Value: TTime);
  protected
    function CanAutoSize(var ANewWidth, ANewHeight: Integer): Boolean; override;
    procedure Paint; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    //
    property BackgroundColor: TColor read GetBackgroundColor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property AutoSize;
    property Color;
    property LookAndFeel;
    property ParentColor;
    property Time: TTime read FTime write SetTime;
    property Transparent;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TcxMonthListBox - Bug in Delphi5: Must be in the interface part }

  TcxMonthListBox = class(TcxCustomPopupWindow)
  private
    FCurrentDate: TcxDateTime;
    FOrigin: TPoint;
    FTopMonthDelta: Integer;
    FItemHeight: Integer;
    FItemIndex: Integer;
    FItemCount: Integer;
    FSign: Integer;
    FShowYears: Boolean;
    FTimer: TcxTimer;
    procedure DoTimer(Sender: TObject);
    function GetCalendar: TcxCustomCalendar;
    function GetCalendarTable: TcxCustomCalendarTable;
    function GetCalendarViewInfo: TcxCalendarClassicViewInfo;
    function GetDate: TDateTime;
    procedure SetItemIndex(Value: Integer);
    procedure SetShowYears(const Value: Boolean);
    procedure SetTopMonthDelta(Value: Integer);
  protected
    function CalculatePosition(const ASize: TSize): TPoint; override;
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShowed; override;
    procedure FontChanged;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property Calendar: TcxCustomCalendar read GetCalendar;
    property CalendarTable: TcxCustomCalendarTable read GetCalendarTable;
    property ItemHeight: Integer read FItemHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ShowYears: Boolean read FShowYears write SetShowYears;
    property TopMonthDelta: Integer read FTopMonthDelta write SetTopMonthDelta;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;
    procedure CloseUp; override;
    procedure Popup(AFocusedControl: TWinControl); override;
    property Date: TDateTime read GetDate;
  end;

  TcxCalendarElementFadingHelper = class(TdxFadingObjectHelper)
  private
    FOwner: TcxCalendarElementViewInfo;
  protected
    // IdxFadingObject
    function CanFade: Boolean; override;
    procedure DrawFadeImage; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage: TcxBitmap); override;
  public
    constructor Create(AOwner: TcxCalendarElementViewInfo);
    property Owner: TcxCalendarElementViewInfo read FOwner;
  end;

  { TcxCustomCalendar }

  TcxCalendarElementViewInfo = class
  private
    FBounds: TRect;
    FCalendar: TcxCustomCalendar;
    FHandleNeededElements: TdxFastObjectList;
    FFadingHelper: TcxCalendarElementFadingHelper;
    FIsRightToLeftConverted: Boolean;
    FPainter: TcxCalendarPainter;
    FSize: TSize;
    FState: TcxCalendarElementState;
    FVisibleElements: TdxFastObjectList;
    FOnClick: TNotifyEvent;
    function GetController: TcxCalendarController;
    function GetFont: TFont;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetViewInfo: TcxCalendarViewInfo;
    function GetVisibleElementsCount: Integer;
    procedure SetState(const Value: TcxCalendarElementState);
  protected
    procedure Add(AElement: TcxCalendarElementViewInfo);
    procedure AddHandleNeededElement(AElement: TcxCalendarElementViewInfo);
    procedure AddVisibleElements; virtual;
    procedure CalculateBounds; virtual;
    procedure CalculateChildSizes; virtual;
    procedure CalculateSize; virtual;
    procedure ClearVisibleElements; virtual;
    procedure Click; virtual;
    procedure CreateElements; virtual;
    procedure DblClick; virtual;
    procedure DestroyElements; virtual;
    function DoCalculateSize: TSize; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetForbiddenStates: TcxCalendarElementStates; virtual;
    function GetHitTest(AHitTest: TObject): Boolean; virtual;
    function GetHitTestIndex: Integer; virtual;
    procedure InitControls; virtual;
    procedure Initialize; virtual;
    procedure InitializeVisibleElements; virtual;
    procedure Invalidate; virtual;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure PaintChildren(ACanvas: TcxCanvas); virtual;
    function PtInElement(const APoint: TPoint): Boolean; virtual;
    procedure Remove(AElement: TcxCalendarElementViewInfo);
    procedure RightToLeftConversion(const ABounds: TRect);
    procedure SetHitTest(AHitTest: TObject); virtual;
    procedure TranslationChanged; virtual;
    // Fading
    function CanFade: Boolean; virtual;
    procedure DrawFadeImage(ACanvas: TcxCanvas); virtual;
    procedure DrawFadePart(ACanvas: TcxCanvas); virtual;
    procedure UpdateFader(const Value: TcxCalendarElementState); virtual;

    property Calendar: TcxCustomCalendar read FCalendar;
    property Controller: TcxCalendarController read GetController;
    property FadingHelper: TcxCalendarElementFadingHelper read FFadingHelper;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
    property Painter: TcxCalendarPainter read FPainter;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property ViewInfo: TcxCalendarViewInfo read GetViewInfo;
    property VisibleElements: TdxFastObjectList read FVisibleElements;
    property VisibleElementsCount: Integer read GetVisibleElementsCount;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(ACalendar: TcxCustomCalendar); virtual;
    destructor Destroy; override;

    property Bounds: TRect read FBounds write FBounds;
    property Font: TFont read GetFont;
    property Size: TSize read FSize;
    property State: TcxCalendarElementState read FState write SetState;
  end;

  TcxCalendarArrowViewInfo = class(TcxCalendarElementViewInfo)
  private
    FDirection: TcxArrowDirection;
  protected
    procedure DoAction; virtual;
    function DoCalculateSize: TSize; override;
    function GetArrowType: TcxCalendarArrow; virtual;
    function GetHitTestIndex: Integer; override;
    function GetRealDirection: TcxArrowDirection;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
  public
    constructor Create(ACalendar: TcxCustomCalendar; ADirection: TcxArrowDirection); reintroduce; virtual;
    property Direction: TcxArrowDirection read FDirection;
  end;

  TcxCalendarModernArrowViewInfo = class(TcxCalendarArrowViewInfo)
  protected
    function GetArrowType: TcxCalendarArrow; override;
  end;

  TcxCalendarDateCellViewInfo = class(TcxCalendarElementViewInfo)
  private
    FColumn: Integer;
    FHorzAlignment: TAlignment;
    FIndex: Integer;
    FRow: Integer;
    FTextRect: TRect;
    FValue: Double;
    function GetBrushColor(AStates: TcxCalendarElementStates): TColor;
  protected
    procedure CalculateBounds; override;
    function CalculateTextRect: TRect; virtual;
    function DoCalculateSize: TSize; override;
    function DoCalculateValue: Double; virtual;
    procedure DoCustomDraw(ACanvas: TcxCanvas); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetContentOffset: TRect; virtual;
    function GetDrawStates: TcxCalendarElementStates; virtual;
    function GetHitTestIndex: Integer; override;
    function GetMinSize: TSize; virtual;
    function GetText: string; virtual;
    function GetRealHorzAlignment: TAlignment;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    function GetSelectDate: TDateTime;
    procedure Initialize; override;
    function IsEnabled: Boolean; virtual;
    function IsSelected: Boolean; virtual;
    procedure Select; virtual;
    procedure SetPosition(X, Y, AIndex: Integer);
    // Fading
    function CanFade: Boolean; override;
    procedure DrawFadeImage(ACanvas: TcxCanvas); override;
    procedure UpdateFader(const Value: TcxCalendarElementState); override;

    property Column: Integer read FColumn;
    property HorzAlignment: TAlignment read GetRealHorzAlignment;
    property Index: Integer read FIndex;
    property Row: Integer read FRow;
    property TextRect: TRect read FTextRect;
    property Value: Double read FValue;
  end;

  TcxCalendarDayCellViewInfo = class(TcxCalendarDateCellViewInfo)
  private
    FDate: TDateTime;
  protected
    function CalculateTextRect: TRect; override;
    procedure DblClick; override;
    function DoCalculateValue: Double; override;
    procedure DoCustomDraw(ACanvas: TcxCanvas); override;
    function GetDrawStates: TcxCalendarElementStates; override;
    function GetForbiddenStates: TcxCalendarElementStates; override;
    function GetMinSize: TSize; override;
    function GetText: string; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
    function IsSelected: Boolean; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TcxCalendarModernDayCellViewInfo = class(TcxCalendarDayCellViewInfo)
  protected
    function CalculateTextRect: TRect; override;
    function GetContentOffset: TRect; override;
    function GetForbiddenStates: TcxCalendarElementStates; override;
    function GetMinSize: TSize; override;
  end;

  TcxCalendarModernLargeCellViewInfo = class(TcxCalendarDateCellViewInfo)
  protected
    function CalculateTextRect: TRect; override;
    function IsEnabled: Boolean; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TcxCalendarModernMonthCellViewInfo = class(TcxCalendarModernLargeCellViewInfo)
  private
    FMonth: Word;
  protected
    function DoCalculateSize: TSize; override;
    function DoCalculateValue: Double; override;
    function GetMinSize: TSize; override;
    function GetText: string; override;
    function IsEnabled: Boolean; override;
    function IsSelected: Boolean; override;
  end;

  TcxCalendarModernYearCellViewInfo = class(TcxCalendarModernLargeCellViewInfo)
  private
    FYear: Word;
  protected
    function DoCalculateSize: TSize; override;
    function DoCalculateValue: Double; override;
    function GetMinSize: TSize; override;
    function GetText: string; override;
    function IsSelected: Boolean; override;
  end;

  TcxCalendarModernDecadeCellViewInfo = class(TcxCalendarModernLargeCellViewInfo)
  private
    FEndYear: Word;
    FStartYear: Word;
  protected
    function DoCalculateSize: TSize; override;
    function DoCalculateValue: Double; override;
    function GetMinSize: TSize; override;
    function GetSelectedYear: Word;
    function GetText: string; override;
    function IsSelected: Boolean; override;
  end;

  TcxCalendarDateTableViewInfo = class(TcxCalendarElementViewInfo)
  private
    FDateCells: TdxFastObjectList;
    FCellSize: TSize;
    function GetDateCells(Index: Integer): TcxCalendarDateCellViewInfo;
  protected
    procedure AddVisibleElements; override;
    function CreateDateCell: TcxCalendarDateCellViewInfo; virtual;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    function DoCalculateSize: TSize; override;
    function IsDateEnabled(ADate: TDateTime): Boolean; virtual;
    function GetMinSize: TSize; virtual;
    function GetSelectedCell: TcxCalendarDateCellViewInfo; virtual;
    function GetTableSize: TSize; virtual;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure Paint(ACanvas: TcxCanvas); override;

    // Navigation
    function GetDateInNextRow(ASelectDate: TDateTime): TDateTime; virtual;
    function GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime; virtual;
    function GetFirstDate(ASelectDate: TDateTime): TDateTime; virtual;
    function GetFirstDateInRow(ASelectDate: TDateTime): TDateTime; virtual;
    function GetLastDate(ASelectDate: TDateTime): TDateTime; virtual;
    function GetLastDateInRow(ASelectDate: TDateTime): TDateTime; virtual;
    function GetNextDate(ADate: TDateTime): TDateTime; virtual;
    function GetNextPageDate(ADate: TDateTime): TDateTime; virtual;
    function GetPreviousDate(ADate: TDateTime): TDateTime; virtual;
    function GetPreviousPageDate(ADate: TDateTime): TDateTime; virtual;

    property CellSize: TSize read FCellSize write FCellSize;
    property DateCells [Index: Integer]: TcxCalendarDateCellViewInfo read GetDateCells;
  end;

  TcxCalendarMonthTableViewInfo = class(TcxCalendarDateTableViewInfo)
  private
    FWeekNumberWidth: Integer;
    FDaysOfWeekHeight: Integer;
  protected
    procedure CalculateBounds; override;
    function CreateDateCell: TcxCalendarDateCellViewInfo; override;
    procedure DrawDelimeters(ACanvas: TcxCanvas); virtual;
    procedure DrawWeekDays(ACanvas: TcxCanvas); virtual;
    procedure DrawWeekNumbers(ACanvas: TcxCanvas); virtual;
    function GetDayOfWeekName(ADay: TDay; AFontCharset: TFontCharset): string; virtual;
    function GetMinSize: TSize; override;
    function GetNextWeekDayTextRect(const ATextRect: TRect): TRect;
    function GetWeekNumbersRegionWidth: Integer;
    procedure InitializeVisibleElements; override;
    function IsDateEnabled(ADate: TDateTime): Boolean; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    // Navigation
    function GetDateInNextRow(ASelectDate: TDateTime): TDateTime; override;
    function GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDate(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDate(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetNextDate(ADate: TDateTime): TDateTime; override;
    function GetNextPageDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousPageDate(ADate: TDateTime): TDateTime; override;
  end;

  TcxCalendarModernMonthTableViewInfo = class(TcxCalendarMonthTableViewInfo)
  protected
    procedure CalculateBounds; override;
    function CreateDateCell: TcxCalendarDateCellViewInfo; override;
    procedure DrawDelimeters(ACanvas: TcxCanvas); override;
    procedure DrawWeekDays(ACanvas: TcxCanvas); override;
    function GetDayOfWeekName(ADay: TDay; AFontCharset: TFontCharset): string; override;
  end;

  TcxCalendarModernLargeCellTableViewInfo = class(TcxCalendarDateTableViewInfo)
  protected
    procedure CalculateBounds; override;
    procedure CalculateChildSizes; override;
    function DoCalculateSize: TSize; override;
    function GetTableSize: TSize; override;
  end;

  TcxCalendarModernYearTableViewInfo = class(TcxCalendarModernLargeCellTableViewInfo)
  protected
    function CreateDateCell: TcxCalendarDateCellViewInfo; override;
    function IsDateEnabled(ADate: TDateTime): Boolean; override;

    // Navigation
    function GetDateInNextRow(ASelectDate: TDateTime): TDateTime; override;
    function GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDate(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDate(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetNextDate(ADate: TDateTime): TDateTime; override;
    function GetNextPageDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousPageDate(ADate: TDateTime): TDateTime; override;
  end;

  TcxCalendarModernDecadeTableViewInfo = class(TcxCalendarModernLargeCellTableViewInfo)
  protected
    function CreateDateCell: TcxCalendarDateCellViewInfo; override;
    function IsDateEnabled(ADate: TDateTime): Boolean; override;

    // Navigation
    function GetDateInNextRow(ASelectDate: TDateTime): TDateTime; override;
    function GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDate(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDate(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetNextDate(ADate: TDateTime): TDateTime; override;
    function GetNextPageDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousPageDate(ADate: TDateTime): TDateTime; override;
  end;

  TcxCalendarModernCenturyTableViewInfo = class(TcxCalendarModernLargeCellTableViewInfo)
  protected
    function CreateDateCell: TcxCalendarDateCellViewInfo; override;
    function IsDateEnabled(ADate: TDateTime): Boolean; override;

    // Navigation
    function GetDateInNextRow(ASelectDate: TDateTime): TDateTime; override;
    function GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDate(ASelectDate: TDateTime): TDateTime; override;
    function GetFirstDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDate(ASelectDate: TDateTime): TDateTime; override;
    function GetLastDateInRow(ASelectDate: TDateTime): TDateTime; override;
    function GetNextDate(ADate: TDateTime): TDateTime; override;
    function GetNextPageDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousDate(ADate: TDateTime): TDateTime; override;
    function GetPreviousPageDate(ADate: TDateTime): TDateTime; override;
  end;

  TcxCalendarDateHeaderCellViewInfo = class(TcxCalendarElementViewInfo)
  private
    FTextRect: TRect;
  protected
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetDrawStates: TcxCalendarElementStates; virtual;
    function GetHitTestIndex: Integer; override;
    function GetText: string; virtual;
    function IsEnabled: Boolean; virtual;
    procedure Paint(ACanvas: TcxCanvas); override;
    property TextRect: TRect read FTextRect;
  end;

  TcxCalendarYearDateHeaderCellViewInfo = class(TcxCalendarDateHeaderCellViewInfo)
  protected
    function GetText: string; override;
  end;

  TcxCalendarMonthDateHeaderCellViewInfo = class(TcxCalendarDateHeaderCellViewInfo)
  protected
    function GetText: string; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TcxCalendarModernDateHeaderCellViewInfo = class(TcxCalendarDateHeaderCellViewInfo)
  private
    FText: string;
  protected
    function CanFade: Boolean; override;
    procedure Click; override;
    procedure DrawFadeImage(ACanvas: TcxCanvas); override;
    function GetText: string; override;
    procedure Initialize; override;
    function IsEnabled: Boolean; override;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TcxCalendarDateHeaderViewInfo = class(TcxCalendarElementViewInfo)
  private
    FArrowsViewInfo: array [adLeft..adRight] of TcxCalendarArrowViewInfo;
    FTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateBounds; override;
    function CreateArrowViewInfo(ADirection: TcxArrowDirection): TcxCalendarArrowViewInfo; virtual;
    procedure CreateElements; override;
    function CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo; virtual;
    procedure DestroyElements; override;
    property TextCellViewInfo: TcxCalendarDateHeaderCellViewInfo read FTextCellViewInfo;
  end;

  TcxCalendarMonthDateHeaderViewInfo = class(TcxCalendarDateHeaderViewInfo)
  protected
    function GetHitTestIndex: Integer; override;
  end;

  TcxCalendarModernMonthDateHeaderViewInfo = class(TcxCalendarMonthDateHeaderViewInfo)
  protected
    function CreateArrowViewInfo(ADirection: TcxArrowDirection): TcxCalendarArrowViewInfo; override;
    function CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo; override;
  end;

  TcxCalendarYearDateHeaderViewInfo = class(TcxCalendarDateHeaderViewInfo)
  protected
    function CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo; override;
    function GetHitTestIndex: Integer; override;
  end;

  TcxCalendarDateHeadersViewInfo = class(TcxCalendarElementViewInfo)
  private
    FMonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateBounds; override;
    procedure CreateElements; override;
    function CreateMonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo; virtual;
    procedure DestroyElements; override;
    procedure DoCalculateBounds; virtual;
    function DoCalculateSize: TSize; override;
  public
    property MonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo read FMonthHeaderViewInfo;
  end;

  TcxCalendarClassicDateHeadersViewInfo = class(TcxCalendarDateHeadersViewInfo)
  private
    FYearHeaderViewInfo: TcxCalendarYearDateHeaderViewInfo;

    procedure CalculateMaxYearAndMonthNameLengths(var AMaxYearNameWidth, AMaxMonthNameWidth: Integer);
  protected
    procedure AddVisibleElements; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    procedure DoCalculateBounds; override;
    function IsYearHeaderVisible: Boolean;
    procedure Paint(ACanvas: TcxCanvas); override;
  public
    property YearHeaderViewInfo: TcxCalendarYearDateHeaderViewInfo read FYearHeaderViewInfo;
  end;

  TcxCalendarModernDateHeadersViewInfo = class(TcxCalendarDateHeadersViewInfo)
  protected
    function CreateMonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo; override;
  end;

  TcxCalendarDateViewInfo = class(TcxCalendarElementViewInfo)
  private
    FHeadersViewInfo: TcxCalendarDateHeadersViewInfo;
    FMonthTableViewInfo: TcxCalendarMonthTableViewInfo;
    FSideWidth: Integer;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateBounds; override;
    procedure CalculateHeadersBounds; virtual;
    procedure CalculateTableBounds; virtual;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    function DoCalculateSize: TSize; override;
    function GetTableOffsets: TRect; virtual;
    procedure Initialize; override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure SynchronizeFirstDate; virtual;
    procedure SynchronizeSelectDate; virtual;
  public
    procedure ChangeFirstDate(Value: Double; AAnimated: Boolean); virtual;
    procedure ChangeSelectDate(AValue: Double; ARepositionVisibleDates: Boolean;
      AAnimated: Boolean); virtual;

    property MonthTableViewInfo: TcxCalendarMonthTableViewInfo read FMonthTableViewInfo;
    property HeadersViewInfo: TcxCalendarDateHeadersViewInfo read FHeadersViewInfo;
  end;

  TcxCalendarModernDateViewStyle = (cvdsMonth, cvdsYear, cvdsDecade, cvdsCentury);

  TcxCalendarAnimatedImageState = record
    Alpha: Byte;
    Scale: TdxSizeF;
    Offset: TdxPointF;
  end;

  TcxCalendarAnimatedImageInfo = class
  private
    FImage: TdxSmartImage;
    FStartState: TcxCalendarAnimatedImageState;
    FEndState: TcxCalendarAnimatedImageState;

    FDestBounds: TRect;
    FSourceBounds: TRect;
    FClipRect: TRect;

    FCurrentDestBounds: TRect;
    FCurrentAlphaValue: Byte;

    FScaleCoefficient: TdxSizeF;
    FOffsetCoefficient: TdxPointF;
    FAlphaCoefficient: Double;

    procedure SetImage(const Value: TdxSmartImage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Calculate(APosition: Integer);
    procedure Initialize(ALength: Integer);
    procedure Paint(AGpCanvas: TdxGPCanvas);

    property SourceBounds: TRect read FSourceBounds write FSourceBounds;
    property DestBounds: TRect read FDestBounds write FDestBounds;

    property ClipRect: TRect read FClipRect write FClipRect;
    property Image: TdxSmartImage read FImage write SetImage;
    property StartState: TcxCalendarAnimatedImageState read FStartState write FStartState;
    property EndState: TcxCalendarAnimatedImageState read FEndState write FEndState;
  end;

  TcxCalendarModernDateViewAnimationController = class
  private
    FAnimation: TdxAnimationTransition;
    FDateViewInfo: TcxCalendarModernDateViewInfo;
    FImageInfos: TdxFastObjectList;
    FIsActive: Boolean;
    procedure DoOnAnimation(Sender: TdxAnimationTransition;
      var APosition: Integer; var AFinished: Boolean);
    function GetImageInfo(AIndex: Integer): TcxCalendarAnimatedImageInfo;
  protected
    property ImageInfo[AIndex: Integer]: TcxCalendarAnimatedImageInfo read GetImageInfo;
  public
    constructor Create(ADateViewInfo: TcxCalendarModernDateViewInfo);
    destructor Destroy; override;
    function AddImage: TcxCalendarAnimatedImageInfo;
    procedure ClearImages;
    procedure DrawImages(AGpCanvas: TdxGPCanvas);
    function IsActive: Boolean;
    procedure Start;
  end;

  TcxCalendarModernDateViewInfo = class(TcxCalendarDateViewInfo)
  private
    FAnimationController: TcxCalendarModernDateViewAnimationController;
    FStartImage, FEndImage: TdxSmartImage;
    FPrevSelectCellBounds: TRect;
    FNextSelectCellBounds: TRect;
    FDirection: TcxDirection;

    FMaxTableSize: TSize;
    FTableAreaBounds: TRect;
    FCenturyTableViewInfo: TcxCalendarModernCenturyTableViewInfo;
    FDecadeTableViewInfo: TcxCalendarModernDecadeTableViewInfo;
    FYearTableViewInfo: TcxCalendarModernYearTableViewInfo;
    FViewStyle: TcxCalendarModernDateViewStyle;
    procedure AddHeaderAnimationImages;
    procedure AddNextDateAnimationImages;
    procedure AddNextViewAnimationImages;
    function GetAnimationBounds: TRect;
    function GetCurrentTableViewInfo: TcxCalendarDateTableViewInfo;
    function GetTableViewInfo(AViewStyle: TcxCalendarModernDateViewStyle): TcxCalendarDateTableViewInfo;
    procedure DoChangeDateAnimation;
    procedure DoChangeViewStyleAnimation;
    procedure InternalSetViewStyle(AValue: TcxCalendarModernDateViewStyle);
    procedure Recalculate;
    procedure RecreateImage(var AImage: TdxSmartImage);
    procedure SetViewStyle(Value: TcxCalendarModernDateViewStyle);
  protected
    procedure AddVisibleElements; override;
    procedure CalculateChildSizes; override;
    procedure CalculateTableBounds; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    function DoCalculateSize: TSize; override;
    function GetHorizontalAnimationDirection(ARight: Boolean): TcxDirection;
    function GetTableOffsets: TRect; override;
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure SynchronizeSelectDate; override;

    procedure NextViewStyle;
    procedure PreviousViewStyle;

    property MaxTableSize: TSize read FMaxTableSize;
    property ViewStyle: TcxCalendarModernDateViewStyle read FViewStyle write SetViewStyle;
  public
    constructor Create(ACalendar: TcxCustomCalendar); override;
    destructor Destroy; override;
    procedure ChangeFirstDate(Value: Double; AAnimated: Boolean); override;
    procedure ChangeViewStyle(AValue: TcxCalendarModernDateViewStyle; AAnimated: Boolean);
    procedure ChangeSelectDate(AValue: Double; ARepositionVisibleDates: Boolean;
      AAnimated: Boolean); override;
    property TableViewInfo [AView: TcxCalendarModernDateViewStyle]: TcxCalendarDateTableViewInfo read GetTableViewInfo;
  end;

  TcxCalendarHeaderViewInfo = class(TcxCalendarElementViewInfo)
  private
    FTextOffsets: TRect;
    FTextRect: TRect;
  protected
    procedure CalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure DrawFadeImage(ACanvas: TcxCanvas); override;
    function GetText: string; virtual;
    procedure Paint(ACanvas: TcxCanvas); override;
    property TextOffsets: TRect read FTextOffsets;
    property TextRect: TRect read FTextRect;
  end;

  TcxCalendarModernHeaderViewInfo = class(TcxCalendarHeaderViewInfo)
  protected
    procedure CalculateBounds; override;
    function CanFade: Boolean; override;
    procedure Click; override;
    function GetHitTestIndex: Integer; override;
    function GetText: string; override;
  end;

  TcxCalendarClockViewInfo = class(TcxCalendarElementViewInfo)
  private
    FClock: TcxClock;
    FClockSize: TSize;
  protected
    procedure CalculateBounds; override;
    procedure ClearVisibleElements; override;
    function DoCalculateSize: TSize; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure InitControls; override;
    procedure InitializeVisibleElements; override;
    property ClockSize: TSize read FClockSize write FClockSize;
  public
    constructor Create(ACalendar: TcxCustomCalendar); override;
    destructor Destroy; override;
  end;

  TcxCalendarTimeEditViewInfo = class(TcxCalendarElementViewInfo)
  private
    FTimeEdit: TcxTimeEdit;
    function GetTimeEditWidth: Integer;
  protected
    procedure CalculateBounds; override;
    procedure ClearVisibleElements; override;
    function DoCalculateSize: TSize; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure InitControls; override;
    procedure InitializeVisibleElements; override;
  public
    constructor Create(ACalendar: TcxCustomCalendar); override;
    destructor Destroy; override;
  end;

  TcxCalendarTimeViewInfo = class(TcxCalendarElementViewInfo)
  private
    FClockInfo: TcxCalendarClockViewInfo;
    FHeight: Integer;
    FTimeEditInfo: TcxCalendarTimeEditViewInfo;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateChildSizes; override;
    procedure CalculateBounds; override;
    procedure ClearVisibleElements; override;
    function CreateClockViewInfo: TcxCalendarClockViewInfo; virtual;
    procedure CreateElements; override;
    function CreateTimeEditViewInfo: TcxCalendarTimeEditViewInfo; virtual;
    procedure DestroyElements; override;
    procedure DoCalculateBounds; virtual;
    function DoCalculateSize: TSize; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property Height: Integer read FHeight write FHeight;
    property TimeEditInfo: TcxCalendarTimeEditViewInfo read FTimeEditInfo;
  end;

  TcxCalendarModernClockViewInfo = class(TcxCalendarClockViewInfo)
  protected
    function DoCalculateSize: TSize; override;
  public
    constructor Create(ACalendar: TcxCustomCalendar); override;
  end;

  TcxCalendarModernTimeViewInfo = class(TcxCalendarTimeViewInfo)
  protected
    function CreateClockViewInfo: TcxCalendarClockViewInfo; override;
    procedure DoCalculateBounds; override;
    function DoCalculateSize: TSize; override;
  end;

  TcxCalendarButtonViewInfo = class(TcxCalendarElementViewInfo)
  private
    FButton: TcxButton;
    FType: TCalendarButton;
  protected
    procedure CalculateBounds; override;
    procedure ClearVisibleElements; override;
    function DoCalculateSize: TSize; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetCaptionWidth: Integer;
    procedure InitControls; override;
    procedure InitializeVisibleElements; override;
    procedure UpdateCaption;
  public
    constructor Create(ACalendar: TcxCustomCalendar; AType: TCalendarButton); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TcxCalendarButtonsViewInfo = class(TcxCalendarElementViewInfo)
  private
    FButtons: array [TCalendarButton] of TcxCalendarButtonViewInfo;
    FMaxButtonWidth: Integer;
    procedure UpdateCalendarButtonCaptions;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateBounds; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    procedure DoCalculateBounds; virtual;
    function DoCalculateSize: TSize; override;
    function IsNative: Boolean;
    procedure Paint(ACanvas: TcxCanvas); override;
    procedure TranslationChanged; override;
  end;

  TcxCalendarModernButtonsViewInfo = class(TcxCalendarButtonsViewInfo)
  protected
    procedure DoCalculateBounds; override;
    function DoCalculateSize: TSize; override;
    function GetButtonsOffset: Integer; virtual;
    function GetHorzAlign: TAlignment; virtual;
    procedure Paint(ACanvas: TcxCanvas); override;
    property HorzAlign: TAlignment read GetHorzAlign;
  end;

  TcxCalendarTouchUIButtonsViewInfo = class(TcxCalendarModernButtonsViewInfo)
  protected
    procedure DoCalculateBounds; override;
    function DoCalculateSize: TSize; override;
    function GetHorzAlign: TAlignment; override;
  end;

  TcxCalendarViewInfo = class(TcxCalendarElementViewInfo)
  private
    // elements
    FButtonsViewInfo: TcxCalendarButtonsViewInfo;
    FHeaderViewInfo: TcxCalendarHeaderViewInfo;
    FDateViewInfo: TcxCalendarDateViewInfo;
    FTimeViewInfo: TcxCalendarTimeViewInfo;

    FColWidth: Integer;
    FRowHeight: Integer;

    FButtonsHeight: Integer;
    FButtonsOffset: Integer;
    FButtonsRegionHeight: Integer;

    FHeaderHeight: Integer;

    FSpaceWidth: Integer;
    function GetHeaderBorderWidth: Integer;
    function GetFont: TFont;
    function GetKind: TcxCalendarKind;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function IsButtonsVisible: Boolean;
    function IsTimeVisible: Boolean;
  protected
    procedure AddVisibleElements; override;
    procedure CalculateButtonsAreaBounds; virtual;
    procedure CalculateChildSizes; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    function DoCalculateSize: TSize; override;
    function GetHitTestIndex: Integer; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    // elements
    function CreateButtonsViewInfo: TcxCalendarButtonsViewInfo; virtual;
    function CreateHeaderViewInfo: TcxCalendarHeaderViewInfo; virtual;
    function CreateDateViewInfo: TcxCalendarDateViewInfo; virtual;
    function CreateTimeViewInfo: TcxCalendarTimeViewInfo; virtual;

    function AllowHidePopupOnMouseUp: Boolean; virtual;
    procedure DoCalculateBounds; virtual;
    procedure DoSetFirstDate(Value: Double; AAnimated: Boolean); virtual;
    procedure DoSetSelectDate(AValue: Double; ARepositionVisibleDates: Boolean;AAnimated: Boolean); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    function GetArrowSize: TSize; virtual;
    function GetButtonSpace: Integer; virtual;
    function GetDateInfoOffsets: TRect; virtual;
    function GetMonthDateHeaderText: string; virtual;
    function HasBackground: Boolean;
    function IsButtonsTabStop: Boolean; virtual;
    function IsButtonVisible(AType: TCalendarButton): Boolean; virtual;
    function IsDateHeaderTransparent: Boolean; virtual;
    function IsHeaderVisible: Boolean; virtual;
    function IsYearDateHeaderVisible: Boolean; virtual;
    procedure Reset; virtual;
    procedure SetFirstDate(Value: Double; AAnimated: Boolean); virtual;
    procedure SetSelectDate(AValue: Double; ARepositionVisibleDates: Boolean; AAnimated: Boolean); virtual;
    procedure TimeChanged(Sender: TObject);

    property HeaderHeight: Integer read FHeaderHeight;
    property Font: TFont read GetFont;
    property Kind: TcxCalendarKind read GetKind;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    procedure CalculateBounds; override;
    procedure Initialize; override;
    procedure Paint(ACanvas: TcxCanvas); override;

    property DateViewInfo: TcxCalendarDateViewInfo read FDateViewInfo;
    property HeaderViewInfo: TcxCalendarHeaderViewInfo read FHeaderViewInfo;
  end;

  TcxCalendarClassicViewInfo = class(TcxCalendarViewInfo)
  private
    FMonthListBox: TcxMonthListBox;
    procedure CreateMonthListBox;
  protected
    // elements
    function CreateButtonsViewInfo: TcxCalendarButtonsViewInfo; override;
    function CreateHeaderViewInfo: TcxCalendarHeaderViewInfo; override;
    function CreateDateViewInfo: TcxCalendarDateViewInfo; override;
    function CreateTimeViewInfo: TcxCalendarTimeViewInfo; override;

    function GetMonthListBoxOwnerBounds: TRect; virtual;
  public
    constructor Create(ACalendar: TcxCustomCalendar); override;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

  TcxCalendarModernViewInfo = class(TcxCalendarViewInfo)
  private
    function GetDateViewInfo: TcxCalendarModernDateViewInfo;
    function GetHeaderViewInfo: TcxCalendarModernHeaderViewInfo;
  protected
    // elements
    function CreateButtonsViewInfo: TcxCalendarButtonsViewInfo; override;
    function CreateHeaderViewInfo: TcxCalendarHeaderViewInfo; override;
    function CreateDateViewInfo: TcxCalendarDateViewInfo; override;
    function CreateTimeViewInfo: TcxCalendarTimeViewInfo; override;

    function AllowHidePopupOnMouseUp: Boolean; override;
    function CanFade: Boolean; override;
    procedure DoCalculateBounds; override;
    function DoCalculateSize: TSize; override;
    function GetArrowSize: TSize; override;
    function GetDateInfoOffsets: TRect; override;
    function GetDateTimeInfoOffset: Integer; virtual;
    function GetHeaderOffset: Integer; virtual;
    function GetMonthDateHeaderText: string; override;
    function IsDateHeaderTransparent: Boolean; override;
    function IsHeaderVisible: Boolean; override;
    function IsYearDateHeaderVisible: Boolean; override;
    procedure Reset; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
    property DateViewInfo: TcxCalendarModernDateViewInfo read GetDateViewInfo;
    property HeaderViewInfo: TcxCalendarModernHeaderViewInfo read GetHeaderViewInfo;
  end;

  TcxCalendarTouchUIViewInfo = class(TcxCalendarViewInfo)
  strict private
    FDateTimeWheelPicker: TdxDateTimeWheelPicker;

    procedure DatePickerValueChanged(ASender: TObject);
    function GetDateTime: Double;
  protected
    procedure AddVisibleElements; override;
    function AllowHidePopupOnMouseUp: Boolean; override;
    function CreateButtonsViewInfo: TcxCalendarButtonsViewInfo; override;
    procedure CreateElements; override;
    procedure DestroyElements; override;
    procedure DoCalculateBounds; override;
    function DoCalculateSize: TSize; override;
    procedure DoSetFirstDate(Value: Double; AAnimated: Boolean); override;
    procedure DoSetSelectDate(AValue: Double; ARepositionVisibleDates: Boolean; AAnimated: Boolean); override;
    procedure InitControls; override;
    procedure InitializeVisibleElements; override;
    function IsButtonsTabStop: Boolean; override;
    function IsButtonVisible(AType: TCalendarButton): Boolean; override;
    function IsHeaderVisible: Boolean; override;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  TcxCalendarHitTest = class
  private
    FController: TcxCalendarController;
    FHitObject: TcxCalendarElementViewInfo;
    FFlags: Int64;
    FHitPoint: TPoint;
    function GetBitState(AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TcxCalendarController);
    procedure Clear;
    procedure Recalculate;
    procedure SetBitState(AIndex: Integer; AValue: Boolean);
    procedure Calculate(const APoint: TPoint); virtual;

    property HitAtBackground: Boolean index cchtBackground read GetBitState;
    property HitAtDateHeaderArrow: Boolean index cchtDateHeaderArrow read GetBitState;
    property HitAtDateTableCell: Boolean index cchtDateTableCell read GetBitState;
    property HitAtHeader: Boolean index cchtHeader read GetBitState;
    property HitAtMonthDateHeader: Boolean index cchtMonthHeader read GetBitState;
    property HitAtYearDateHeader: Boolean index cchtYearHeader read GetBitState;
    property HitObject: TcxCalendarElementViewInfo read FHitObject write FHitObject;
    property HitPoint: TPoint read FHitPoint;
  end;

  TcxCalendarController = class
  private
    FHitTest: TcxCalendarHitTest;
    FHotElement: TcxCalendarElementViewInfo;
    FCalendar: TcxCustomCalendar;
    FPressedElement: TcxCalendarElementViewInfo;
    FTimer: TcxTimer;
    procedure CheckHotElement(AShift: TShiftState; const APoint: TPoint);
    procedure DoScrollArrow(Sender: TObject);
    function GetViewInfo: TcxCalendarViewInfo;
    procedure SetHotElement(AValue: TcxCalendarElementViewInfo);
    procedure SetPressedElement(AValue: TcxCalendarElementViewInfo);
  protected
    function CreateHitTestController: TcxCalendarHitTest; virtual;

    function DoCanFocus: Boolean; virtual;
    procedure DoDblClick; virtual;
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; virtual;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; virtual;

    property HotElement: TcxCalendarElementViewInfo read FHotElement write SetHotElement;
    property PressedElement: TcxCalendarElementViewInfo read FPressedElement write SetPressedElement;
  public
    constructor Create(ACalendar: TcxCustomCalendar);
    destructor Destroy; override;

    function CanFocus: Boolean;
    procedure DblClick;
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); virtual;
    procedure KeyDown(var AKey: Word; AShift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeave;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean;

    property HitTest: TcxCalendarHitTest read FHitTest;
    property ViewInfo: TcxCalendarViewInfo read GetViewInfo;
  end;

  TcxCalendarClassicController = class(TcxCalendarController)
  private
    function GetViewInfo: TcxCalendarClassicViewInfo;
  protected
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); override;
    property ViewInfo: TcxCalendarClassicViewInfo read GetViewInfo;
  end;

  TcxCalendarModernController = class(TcxCalendarController)
  private
    function GetViewInfo: TcxCalendarModernViewInfo;
  protected
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    property ViewInfo: TcxCalendarModernViewInfo read GetViewInfo;
  end;

  TcxCalendarTouchUIController = class(TcxCalendarController)
  protected
    function DoCanFocus: Boolean; override;
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  end;

  TcxCalendarPainter = class
  private
    FCalendar: TcxCustomCalendar;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure DrawArrow(ACanvas: TcxCanvas; AViewInfo: TcxCalendarArrowViewInfo); virtual;
    procedure DrawButtonsBackground(ACanvas: TcxCanvas; AViewInfo: TcxCalendarButtonsViewInfo); virtual;
    procedure DrawDateCellText(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo); virtual;
    procedure DrawDateCellBackground(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo); virtual;
    procedure DrawDateCellSelection(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo); virtual;
    procedure DrawDateHeaderCell(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateHeaderCellViewInfo); virtual;
    procedure DrawDateHeaderCellText(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateHeaderCellViewInfo); virtual;
    procedure DrawHeader(ACanvas: TcxCanvas; AViewInfo: TcxCalendarHeaderViewInfo); virtual;
    procedure DrawHeaderText(ACanvas: TcxCanvas; AViewInfo: TcxCalendarHeaderViewInfo); virtual;
    function GetCellTextColor(ADrawStates: TcxCalendarElementStates): TColor; virtual;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  public
    constructor Create(ACalendar: TcxCustomCalendar); virtual;
  end;

  TcxCalendarModernPainter = class(TcxCalendarPainter)
  protected
    procedure DrawArrow(ACanvas: TcxCanvas; AViewInfo: TcxCalendarArrowViewInfo); override;
    procedure DrawDateCellBackground(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo); override;
    procedure DrawDateCellSelection(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo); override;
    procedure DrawDateHeaderCell(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateHeaderCellViewInfo); override;
    procedure DrawDateHeaderCellText(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateHeaderCellViewInfo); override;
    procedure DrawHeader(ACanvas: TcxCanvas; AViewInfo: TcxCalendarHeaderViewInfo); override;
    procedure DrawHeaderText(ACanvas: TcxCanvas; AViewInfo: TcxCalendarHeaderViewInfo); override;
    function GetCellTextColor(ADrawStates: TcxCalendarElementStates): TColor; override;
  end;

  TcxCalendarTouchUIPainter = class(TcxCalendarPainter)
  end;

  TcxCalendarView = (cavDefault, cavClassic, cavModern, cavTouchUI);

  TcxCalendarGetDayOfWeekStateEvent = procedure (Sender: TObject;
    ADayOfWeek: TDay; AState: TCustomDrawState; AFont: TFont; var ABackgroundColor: TColor) of object;

  TcxCalendarGetDayStateEvent = procedure (Sender: TObject;
    ADate: TDateTime; AState: TCustomDrawState; AFont: TFont; var ABackgroundColor: TColor) of object;

  TcxCustomCalendar = class(TcxControl, IcxMouseTrackingCaller, IcxMouseTrackingCaller2, IcxMouseTrackingCaller3)
  private
    FAnimation: Boolean;
    FArrowsForYear: Boolean;
    FCalendarButtons: TDateButtons;
    FController: TcxCalendarController;
    FFirstDate: Double;
    FFlat: Boolean;
    FIsViewInfoDirty: Boolean;
    FIsSizeCalculated: Boolean;
    FKind: TcxCalendarKind;
    FLockCount: Integer;
    FMaxDate: Double;
    FMinDate: Double;
    FPainter: TcxCalendarPainter;
    FRealView: TcxCalendarView;
    FSelectDate: Double;
    FShowOnlyValidDates: Boolean;
    FShowToday: Boolean;
    FTime: TTime;
    FTimeFormat: TcxTimeEditTimeFormat;
    FUse24HourFormat: Boolean;
    FView: TcxCalendarView;
    FViewInfo: TcxCalendarViewInfo;
    FWeekNumbers: Boolean;
    FYearsInMonthList: Boolean;
    FOnDateTimeChanged: TNotifyEvent;
    FOnGetDayOfWeekState: TcxCalendarGetDayOfWeekStateEvent;
    FOnGetDayState: TcxCalendarGetDayStateEvent;
    procedure ButtonClick(Sender: TObject);
    function CanShowDate(ADate: Double): Boolean;
    function CanShowMonth(ADate: Double): Boolean;
    function CanShowYear(ADate: Double): Boolean;
    procedure CheckChanges;
    procedure CheckRealView;
    procedure CreateSubclasses;
    procedure CorrectHeaderTextRect(var R: TRect);
    procedure DestroySubclasses;
    procedure DoDateTimeChanged;
    function GetBackgroundColor: TColor;
    function GetCalendarTable: TcxCustomCalendarTable;
    function GetContainerBorderColor: TColor;
    function GetDateFromCell(X, Y: Integer): Double;
    function GetDateHeaderFrameColor: TColor;
    function GetDateTimeHeaderFrameColor: TColor;
    function GetHeaderColor: TColor;
    function GetHeaderOffset: TRect;
    function IsDateInRange(ADate: Double): Boolean;
    function IsMonthInRange(ADate: Double): Boolean;
    function IsYearInRange(ADate: Double): Boolean;
    procedure RecreateView;
    procedure SetArrowsForYear(Value: Boolean);
    procedure SetCalendarButtons(Value: TDateButtons);
    procedure SetFlat(Value: Boolean);
    procedure SetKind(Value: TcxCalendarKind);
    procedure SetMaxDate(Value: Double);
    procedure SetMinDate(Value: Double);
    procedure SetShowToday(const Value: Boolean);
    procedure SetTime(const Value: TTime);
    procedure SetTimeFormat(Value: TcxTimeEditTimeFormat);
    procedure SetUse24HourFormat(Value: Boolean);
    procedure SetView(const Value: TcxCalendarView);
    procedure SetWeekNumbers(Value: Boolean);
  protected
    procedure DblClick; override;
    procedure FocusChanged; override;
    procedure FontChanged; override;
    function HasBackground: Boolean; override;
    procedure InitControl; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function IsPopup: Boolean; virtual;
    function IsShowOnlyValidDates: Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Paint; override;
    procedure Calculate; virtual;
    function CalculateNextDate(ADate: Double; AArrow: TcxCalendarArrow): Double;
    function CanAnimate: Boolean;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure Changed;
    function CreateController: TcxCalendarController; virtual;
    function CreatePainter: TcxCalendarPainter; virtual;
    function CreateViewInfo: TcxCalendarViewInfo; virtual;
    procedure DoStep(AArrow: TcxCalendarArrow);
    function GetButtonsRegionOffset: Integer; virtual;
    function GetLastDate: Double; virtual;
    function GetMonthCalendarOffset: TPoint; virtual;
    function GetRealFirstDate: Double; virtual;
    function GetRealLastDate: Double; virtual;
    function GetRealView: TcxCalendarView; virtual;
    function GetSize: TSize; virtual;
    procedure Hide(AReason: TcxEditCloseUpReason);
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); virtual;
    procedure InternalSetFirstDate(Value: Double);
    procedure InternalSetSelectDate(Value: Double); overload;
    procedure InternalSetSelectDate(Value: Double; ARepositionVisibleDates: Boolean); overload;
    procedure InternalSetTime(const Value: TTime);
    function IsLocked: Boolean;
    procedure SetFirstDate(Value: Double); virtual;
    procedure SetSelectDate(Value: Double); virtual;
    procedure StepToFuture;
    procedure StepToPast;

    // IcxMouseTrackingCaller
    procedure MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller2.MouseLeave = MouseTrackingMouseLeave;
    procedure IcxMouseTrackingCaller3.MouseLeave = MouseTrackingMouseLeave;
    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    // IcxMouseTrackingCaller3
    function IsCaptureMouse: Boolean;

    property BackgroundColor: TColor read GetBackgroundColor;
    property ContainerBorderColor: TColor read GetContainerBorderColor;
    //
    property CalendarTable: TcxCustomCalendarTable read GetCalendarTable;
    property Controller: TcxCalendarController read FController;
    property ShowOnlyValidDates: Boolean read FShowOnlyValidDates write FShowOnlyValidDates;
    property MaxDate: Double read FMaxDate write SetMaxDate;
    property MinDate: Double read FMinDate write SetMinDate;
    property Painter: TcxCalendarPainter read FPainter;
    property Time: TTime read FTime write SetTime;
    property ViewInfo: TcxCalendarViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    function CanFocus: Boolean; override;
    procedure EndUpdate;

    // IdxLocalizerListener
    procedure TranslationChanged; override;

    property Animation: Boolean read FAnimation write FAnimation default True;
    property ArrowsForYear: Boolean read FArrowsForYear write SetArrowsForYear default True;
    property CalendarButtons: TDateButtons read FCalendarButtons write SetCalendarButtons;
    property FirstDate: Double read FFirstDate write SetFirstDate;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Kind: TcxCalendarKind read FKind write SetKind default ckDate;
    property SelectDate: Double read FSelectDate write SetSelectDate;
    property ShowToday: Boolean read FShowToday write SetShowToday;
    property TimeFormat: TcxTimeEditTimeFormat read FTimeFormat write SetTimeFormat default tfHourMinSec;
    property View: TcxCalendarView read FView write SetView;
    property Use24HourFormat: Boolean read FUse24HourFormat write SetUse24HourFormat default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
    property YearsInMonthList: Boolean read FYearsInMonthList write FYearsInMonthList default True;
    property OnDateTimeChanged: TNotifyEvent read FOnDateTimeChanged write FOnDateTimeChanged;
    property OnGetDayOfWeekState: TcxCalendarGetDayOfWeekStateEvent read FOnGetDayOfWeekState write FOnGetDayOfWeekState;
    property OnGetDayState: TcxCalendarGetDayStateEvent read FOnGetDayState write FOnGetDayState;
  end;

  { TcxPopupCalendar }

  TcxPopupCalendar = class(TcxCustomCalendar)
  private
    FEdit: TcxCustomDateEdit;
    FOnHidePopup: TcxEditClosePopupEvent;
  protected
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); override;
    procedure Initialize;
    function IsPopup: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    property Edit: TcxCustomDateEdit read FEdit write FEdit;
    property OnHidePopup: TcxEditClosePopupEvent read FOnHidePopup write FOnHidePopup;
  end;

  TcxPopupCalendarClass = class of TcxPopupCalendar;

  { TcxDateEditPropertiesValues }

  TcxDateEditPropertiesValues = class(TcxTextEditPropertiesValues)
  private
    FDateButtons: Boolean;
    FInputKind: Boolean;
    function GetMaxDate: Boolean;
    function GetMinDate: Boolean;
    function IsMaxDateStored: Boolean;
    function IsMinDateStored: Boolean;
    procedure SetDateButtons(Value: Boolean);
    procedure SetInputKind(Value: Boolean);
    procedure SetMaxDate(Value: Boolean);
    procedure SetMinDate(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; override;
  published
    property DateButtons: Boolean read FDateButtons write SetDateButtons
      stored False;
    property InputKind: Boolean read FInputKind write SetInputKind stored False;
    property MaxDate: Boolean read GetMaxDate write SetMaxDate stored IsMaxDateStored;
    property MinDate: Boolean read GetMinDate write SetMinDate stored IsMinDateStored;
  end;

  { TcxCustomDateEditProperties }

  TDateOnError = (deNoChange, deToday, deNull);
  TcxInputKind = (ikStandard, ikMask, ikRegExpr);

  TcxCustomDateEditProperties = class(TcxCustomPopupEditProperties)
  private
    FAnimation: Boolean;
    FArrowsForYear: Boolean;
    FDateButtons: TDateButtons;
    FDateOnError: TDateOnError;
    FInputKind: TcxInputKind;
    FKind: TcxCalendarKind;
    FSaveTime: Boolean;
    FShowOnlyValidDates: Boolean;
    FShowTime: Boolean;
    FShowToday: Boolean;
    FView: TcxCalendarView;
    FWeekNumbers: Boolean;
    FYearsInMonthList: Boolean;
    FOnGetDayOfWeekState: TcxCalendarGetDayOfWeekStateEvent;
    FOnGetDayState: TcxCalendarGetDayStateEvent;
    procedure BuildEditMask;
    function GetAssignedValues: TcxDateEditPropertiesValues;
    function GetDateButtons: TDateButtons;
    function GetDefaultDateButtons: TDateButtons;
    function GetDefaultInputKind: TcxInputKind;
    function GetInputKind: TcxInputKind;
    function GetMaxDate: TDateTime;
    function GetMinDate: TDateTime;
    procedure CalendarGetDayOfWeekStateEventHandler(Sender: TObject; ADayOfWeek: TDay; AState: TCustomDrawState; AFont: TFont;
      var ABackgroundColor: TColor);
    procedure CalendarGetDayStateEventHandler(Sender: TObject;
      ADate: TDateTime; AState: TCustomDrawState; AFont: TFont; var ABackgroundColor: TColor);
    function IsDateButtonsStored: Boolean;
    function IsInputKindStored: Boolean;
    function NeedShowTime(ADate: TDateTime; AIsFocused: Boolean): Boolean;
    procedure SetAnimation(Value: Boolean);
    procedure SetArrowsForYear(Value: Boolean);
    procedure SetAssignedValues(Value: TcxDateEditPropertiesValues);
    procedure SetDateButtons(Value: TDateButtons);
    procedure SetDateOnError(Value: TDateOnError);
    procedure SetInputKind(Value: TcxInputKind);
    procedure SetKind(Value: TcxCalendarKind);
    procedure SetMaxDate(Value: TDateTime);
    procedure SetMinDate(Value: TDateTime);
    procedure SetSaveTime(Value: Boolean);
    procedure SetShowTime(Value: Boolean);
    procedure SetShowToday(const Value: Boolean);
    procedure SetView(const Value: TcxCalendarView);
    procedure SetWeekNumbers(Value: Boolean);
    procedure SetYearsInMonthList(Value: Boolean);
  protected
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure DoChanged; override;
    function GetAlwaysPostEditValue: Boolean; override;
    class function GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass; override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function GetModeClass(AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass; override;
    class function GetPopupWindowClass: TcxCustomEditPopupWindowClass; override;
    function IsEditValueEmpty(const AEditValue: TcxEditValue): Boolean; override;
    function IsEditValueNumeric: Boolean; override;
    function IsValueBoundDefined(ABound: TcxEditValueBound): Boolean; override;
    function IsValueBoundsDefined: Boolean; override;
    function PopupWindowAcceptsAnySize: Boolean; override;
    function GetEmptyDisplayValue(AEditFocused: Boolean): string;
    function GetStandardMaskBlank(APos: Integer): Char; virtual;
    function GetTimeZoneInfo(APos: Integer;
      out AInfo: TcxTimeEditZoneInfo): Boolean; virtual;
    procedure InternalPrepareEditValue(ADisplayValue: string; out AEditValue: TcxEditValue);
    property AssignedValues: TcxDateEditPropertiesValues read GetAssignedValues
      write SetAssignedValues;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function IsDisplayValueValid(var DisplayValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    function IsValueFormattedByProperties: Boolean; override;
    procedure DoPrepareDisplayValue(const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var AError: Boolean; AEdit: TcxCustomEdit); override;
    // !!!
    property Animation: Boolean read FAnimation write SetAnimation default True;
    property ArrowsForYear: Boolean read FArrowsForYear write SetArrowsForYear default True;
    property DateButtons: TDateButtons read GetDateButtons write SetDateButtons stored IsDateButtonsStored;
    property DateOnError: TDateOnError read FDateOnError write SetDateOnError default deNoChange;
    property ImmediateDropDownWhenKeyPressed default False;
    property InputKind: TcxInputKind read GetInputKind write SetInputKind stored IsInputKindStored;
    property Kind: TcxCalendarKind read FKind write SetKind default ckDate;
    property MaxDate: TDateTime read GetMaxDate write SetMaxDate;
    property MinDate: TDateTime read GetMinDate write SetMinDate;
    property SaveTime: Boolean read FSaveTime write SetSaveTime default True;
    property ShowOnlyValidDates: Boolean read FShowOnlyValidDates write FShowOnlyValidDates default False;
    property View: TcxCalendarView read FView write SetView default cavDefault;
    property ShowTime: Boolean read FShowTime write SetShowTime default True;
    property ShowToday: Boolean read FShowToday write SetShowToday default True;
    property WeekNumbers: Boolean read FWeekNumbers write SetWeekNumbers default False;
    property YearsInMonthList: Boolean read FYearsInMonthList write SetYearsInMonthList default True;
    property OnGetDayOfWeekState: TcxCalendarGetDayOfWeekStateEvent read FOnGetDayOfWeekState write FOnGetDayOfWeekState;
    property OnGetDayState: TcxCalendarGetDayStateEvent read FOnGetDayState write FOnGetDayState;
  end;

  { TcxDateEditProperties }

  TcxDateEditProperties = class(TcxCustomDateEditProperties)
  published
    property Alignment;
    property Animation;
    property ArrowsForYear;
    property AssignedValues;
    property AutoSelect;
    property ButtonGlyph;
    property ClearKey;
    property DateButtons;
    property DateOnError;
    property DisplayFormat;
    property EditFormat;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property InputKind;
    property Kind;
    property MaxDate;
    property MinDate;
    property Nullstring;
    property PostPopupValueOnTab;
    property ReadOnly;
    property SaveTime;
    property ShowOnlyValidDates;
    property ShowTime;
    property ShowToday;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property View;
    property WeekNumbers;
    property YearsInMonthList;
    property OnChange;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnGetDayOfWeekState;
    property OnGetDayState;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;
  end;

  { TcxDateEditPopupWindow }

  TcxDateEditPopupWindow = class(TcxPopupEditPopupWindow)
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function IsPopupCalendarKey(Key: Word; Shift: TShiftState): Boolean; virtual;
  public
    constructor Create(AOwnerControl: TWinControl); override;
  end;

  { TcxDateEditMaskStandardMode }

  TcxDateEditMaskStandardMode = class(TcxMaskEditStandardMode)
  protected
    function GetBlank(APos: Integer): Char; override;
  end;

  { TcxCustomDateEdit }

  TcxCustomDateEdit = class(TcxCustomPopupEdit)
  private
    FDateDropDown: TDateTime;
    FSavedTime: TDateTime;

    function GetActiveProperties: TcxCustomDateEditProperties;
    function GetCurrentDate: TDateTime;
    function GetProperties: TcxCustomDateEditProperties;
    procedure SetProperties(Value: TcxCustomDateEditProperties);
  protected
    FCalendar: TcxPopupCalendar;

    function CanSynchronizeModeText: Boolean; override;
    procedure CheckEditorValueBounds; override;
    procedure CreatePopupWindow; override;
    procedure DropDown; override;
    procedure Initialize; override;
    procedure InitializePopupWindow; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function InternalGetText: string; override;
    function InternalGetValueToValidate: TcxEditValue; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    function InternalSetText(const Value: string): Boolean; override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function IsCharValidForPos(var AChar: Char; APos: Integer): Boolean; override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PopupWindowShowed(Sender: TObject); override;
    procedure SetupPopupWindow; override;
    procedure UpdateTextFormatting; override;

    procedure CreateCalendar; virtual;
    procedure DateChange(Sender: TObject); virtual;
    function GetCalendarClass: TcxPopupCalendarClass; virtual;
    function GetDate: TDateTime; virtual;
    function GetDateFromStr(const S: string): TDateTime;
    function GetRecognizableDisplayValue(ADate: TDateTime): TcxEditValue; virtual;
    procedure SetDate(Value: TDateTime); virtual;

    property Calendar: TcxPopupCalendar read FCalendar;
  public
    destructor Destroy; override;

    procedure Clear; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue;
      out EditValue: TcxEditValue; AEditFocused: Boolean); override;

    property ActiveProperties: TcxCustomDateEditProperties read GetActiveProperties;
    property CurrentDate: TDateTime read GetCurrentDate;
    property Date: TDateTime read GetDate write SetDate stored False;
    property Properties: TcxCustomDateEditProperties read GetProperties write SetProperties;
  end;

  { TcxDateEdit }

  TcxDateEdit = class(TcxCustomDateEdit)
  private
    function GetActiveProperties: TcxDateEditProperties;
    function GetProperties: TcxDateEditProperties;
    procedure SetProperties(Value: TcxDateEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxDateEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property Date;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxDateEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop default True;
    property TextHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
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
    property OnStartDrag;
    property OnStartDock;
  end;

  { TcxFilterDateEditHelper }

  TcxFilterDateEditHelper = class(TcxFilterDropDownEditHelper)
  public
    class function GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType; override;
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

function VarIsNullDate(const AValue: Variant): Boolean;

const
  cxCalendarUseClassicViewAsDefault: Boolean = False;

implementation

uses
  Math,
  dxOffice11, dxThemeConsts, dxThemeManager, dxUxTheme,
  cxEditPaintUtils, cxEditUtils,
  cxVariants, dxDPIAwareUtils;

type
  TDelimiterOffset = record
    Left, Right: Integer;
  end;

  TcxEditAccess = class(TcxCustomEdit);

const
  cxEditTimeFormatA: array [TcxTimeEditTimeFormat, Boolean] of string = (
    ('hh:nn:ss ampm', 'hh:nn:ss'),
    ('hh:nn ampm', 'hh:nn'),
    ('hh ampm', 'hh')
  );
  DateNavigatorTime = 200;
  Office11HeaderOffset = 2;

  WeekNumbersDelimiterOffset: TDelimiterOffset = (Left: 3; Right: 1);
  WeekNumbersDelimiterWidth = 1;

function GetCalendarRealView(AView: TcxCalendarView): TcxCalendarView;
begin
  if AView <> cavDefault then
    Result := AView
  else
    if cxCalendarUseClassicViewAsDefault then
      Result := cavClassic
    else
      if cxIsTouchModeEnabled then
        Result := cavTouchUI
      else
        Result := cavModern;
end;

function CalendarElementStateToCustomDrawState(AStates: TcxCalendarElementStates): TCustomDrawState;
var
  AState: TcxCalendarElementState;
begin
  Result := [];
  for AState := Low(AState) to High(AState) do
    if AState in AStates then
      case AState of
        cesHot:
          Include(Result, cdsHot);
        cesPressed:
          Include(Result, cdsSelected);
        cesSelected:
          Include(Result, cdsSelected);
        cesFocused:
          Include(Result, cdsFocused);
        cesMarked:
          Include(Result, cdsMarked);
        cesDisabled:
          Include(Result, cdsGrayed);
      else //cesNormal
        Include(Result, cdsDefault);
      end;
end;

function VarIsNullDate(const AValue: Variant): Boolean;
begin
  Result := (VarIsDate(AValue) or VarIsNumericEx(AValue)) and
    (Integer(AValue) = NullDate);
end;

function cxEncodeDate(AYear, AMonth, ADay: Word): Double;
begin
  if (AYear < dxMinYear) or (AYear > dxMaxYear) or
    (AMonth < 1) or (AMonth > 12) or
    (ADay < 1) or (ADay > DaysInAMonth(AYear, AMonth)) then
    Result := InvalidDate
  else
    Result := EncodeDate(AYear, AMonth, ADay);
end;

procedure GetTimeFormat(const ADateTimeFormatInfo: TcxDateTimeFormatInfo;
  out ATimeFormat: TcxTimeEditTimeFormat; out AUse24HourFormat: Boolean);

  function GetFormatInfoItemIndex(
    AItemKind: TcxDateTimeFormatItemKind): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to ADateTimeFormatInfo.ItemCount - 1 do
      if ADateTimeFormatInfo.Items[I].Kind = AItemKind then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AFormatInfoItemIndex: Integer;
begin
  if GetFormatInfoItemIndex(dtikSec) <> -1 then
    ATimeFormat := tfHourMinSec
  else if GetFormatInfoItemIndex(dtikMin) <> -1 then
    ATimeFormat := tfHourMin
  else if GetFormatInfoItemIndex(dtikHour) <> -1 then
    ATimeFormat := tfHour
  else
    ATimeFormat := tfHourMinSec;

  AFormatInfoItemIndex := GetFormatInfoItemIndex(dtikHour);
  if AFormatInfoItemIndex <> -1 then
    AUse24HourFormat := Copy(ADateTimeFormatInfo.Items[AFormatInfoItemIndex].Data, 1, 2) = '24'
  else
    AUse24HourFormat := False;
end;

function GetDateNavigatorContentColor(APainter: TcxCustomLookAndFeelPainter; AColor: TColor): TColor;
begin
  Result := APainter.DefaultDateNavigatorContentColor;
  if (Result = clDefault) or (AColor <> clWindow) then
    Result := AColor;
end;

function GetFontSize(AFont: TFont): Integer;
begin
  if AFont.Height <> 0 then
    Result := Abs(AFont.Height)
  else
    Result := cxTextHeight(AFont);

  Result := MulDiv(Result, 72, dxDefaultDPI);
end;

function dxChangeDay(ADate: TDateTime; ADay: Word): TDateTime;
var
  AcxDate: TcxDateTime;
begin
  AcxDate := cxGetLocalCalendar.FromDateTime(ADate);
  AcxDate.Day := CheckDay(AcxDate.Year, AcxDate.Month, ADay);
  Result := cxGetLocalCalendar.ToDateTime(AcxDate);
end;

function dxChangeMonth(ADate: TDateTime; AMonth: Word): TDateTime;
var
  AcxDate: TcxDateTime;
begin
  AcxDate := cxGetLocalCalendar.FromDateTime(ADate);
  AcxDate.Month := AMonth;
  AcxDate.Day := CheckDay(AcxDate.Year, AcxDate.Month, AcxDate.Day);
  Result := cxGetLocalCalendar.ToDateTime(AcxDate);
end;

function dxChangeYear(ADate: TDateTime; AYear: Word): TDateTime;
var
  AcxDate: TcxDateTime;
begin
  AcxDate := cxGetLocalCalendar.FromDateTime(ADate);
  AcxDate.Year := AYear;
  AcxDate.Day := CheckDay(AcxDate.Year, AcxDate.Month, AcxDate.Day);
  Result := cxGetLocalCalendar.ToDateTime(AcxDate);
end;

function dxChangeDecade(ADate: TDateTime; AStartYear: Word): TDateTime;
var
  AcxDate: TcxDateTime;
begin
  AcxDate := cxGetLocalCalendar.FromDateTime(ADate);
  AcxDate.Year := AStartYear + AcxDate.Year mod 10;
  AcxDate.Day := CheckDay(AcxDate.Year, AcxDate.Month, AcxDate.Day);
  Result := cxGetLocalCalendar.ToDateTime(AcxDate);
end;

{ TcxClock }

constructor TcxClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(Left, Top, 130, 130);
end;

function TcxClock.CanAutoSize(var ANewWidth, ANewHeight: Integer): Boolean;
var
  ASize: TSize;
begin
  ASize := LookAndFeelPainter.ScaledClockSize(ScaleFactor);
  ANewHeight := ASize.cy;
  ANewWidth := ASize.cx;
  Result := True;
end;

procedure TcxClock.Paint;
begin
  if Transparent then
    cxDrawTransparentControlBackground(Self, Canvas, ClientRect);
  LookAndFeelPainter.DrawScaledClock(Canvas, ClientRect, TDateTime(FTime), BackgroundColor, ScaleFactor);
end;

function TcxClock.GetBackgroundColor: TColor;
begin
{$ifdef Colorized}
  Result := clRed;
{$else}
  if Transparent then
    Result := clNone
  else
    Result := GetDateNavigatorContentColor(LookAndFeelPainter, Color);
{$endif}
end;

procedure TcxClock.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TcxClock.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Transparent then
    Invalidate;
end;

procedure TcxClock.SetTime(Value: TTime);
begin
  Value := Frac(Value);
  if Value <> FTime then
  begin
    FTime := Value;
    Invalidate;
  end;
end;

{ TcxMonthListBox }

constructor TcxMonthListBox.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  ControlStyle := [csCaptureMouse, csOpaque];
  FTimer := cxCreateTimer(DoTimer, 200, False);
  Adjustable := False;
  BorderStyle := pbsFlat;
end;

destructor TcxMonthListBox.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TcxMonthListBox.CloseUp;
var
  ADate: TDateTime;
begin
  if GetCaptureControl = Self then
    SetCaptureControl(nil);
  if not Visible then
    Exit;
  inherited CloseUp;
  FTimer.Enabled := False;
  if ShowYears then
  begin
    ADate := GetDate;
    if ADate <> NullDate then
      Calendar.SetFirstDate(ADate);
  end;
end;

procedure TcxMonthListBox.Popup(AFocusedControl: TWinControl);

  function GetTopItemCount(const ACurrentDate: TcxDateTime): Integer;
  var
    ADate: TcxDateTime;
    APrevDate: TDateTime;
  begin
    if ShowYears then
      Result := 3
    else
      Result := ACurrentDate.Month - 1;

    APrevDate := CalendarTable.AddMonths(ACurrentDate, -Result);
    ADate := CalendarTable.FromDateTime(APrevDate);
    while (Result > 0) and (not Calendar.CanShowMonth(CalendarTable.ToDateTime(ADate)) or
      not CalendarTable.IsValidYear(ADate.Era, ADate.Year) or
      (APrevDate = CalendarTable.AddMonths(FCurrentDate, 1 - Result))) do
    begin
      ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(ADate, 1));
      Dec(Result);
    end;
  end;

  function GetItemCount(const ACurrentDate: TcxDateTime; ATopItemsCount: Cardinal): Cardinal;
  var
    ADate: TcxDateTime;
    AMaxItemCount: Cardinal;
  begin
    if ShowYears then
      AMaxItemCount := 4 + Abs(ATopItemsCount)
    else
      AMaxItemCount := CalendarTable.GetMonthsInYear(ACurrentDate.Year) - ACurrentDate.Month + ATopItemsCount + 1;

    ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(ACurrentDate, -ATopItemsCount));
    Result := 0;
    while (Result < AMaxItemCount) and Calendar.CanShowMonth(CalendarTable.ToDateTime(ADate)) and
      CalendarTable.IsValidYear(ADate.Era, ADate.Year) do
    begin
      ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(ADate, 1));
      Inc(Result);
    end;
  end;

var
  R: TRect;
begin
  FCurrentDate := CalendarTable.FromDateTime(Calendar.FirstDate);
  TopMonthDelta := -GetTopItemCount(FCurrentDate);
  FItemCount := GetItemCount(FCurrentDate, -TopMonthDelta);
  BiDiMode := Calendar.BiDiMode;
  if FItemCount > 1 then
  begin
    Font := Calendar.Font;
    FontChanged;
    R := GetCalendarViewInfo.GetMonthListBoxOwnerBounds;
    R.TopLeft := Calendar.ClientToScreen(R.TopLeft);
    R.BottomRight := Calendar.ClientToScreen(R.BottomRight);
    FOrigin.X := R.Left + (R.Right - R.Left - Self.Width) div 2;
    if ShowYears then
      FOrigin.Y := (R.Top + R.Bottom) div 2 + TopMonthDelta * FItemHeight - FItemHeight div 2
    else
      FOrigin.Y := (R.Top + R.Bottom) div 2 - Self.Height div 2;

    FItemIndex := -1;
    inherited Popup(AFocusedControl);
  end;
end;

procedure TcxMonthListBox.DoTimer(Sender: TObject);
var
  ATopMonthDelta: Integer;
  ADateMin, ADateMax: Double;
begin
  ATopMonthDelta := TopMonthDelta + FSign;
  ADateMin := CalendarTable.AddMonths(FCurrentDate, ATopMonthDelta);
  ADateMax := CalendarTable.AddMonths(FCurrentDate, FItemCount + ATopMonthDelta - 1);
  if Calendar.CanShowMonth(ADateMin) and Calendar.CanShowMonth(ADateMax) then
    TopMonthDelta := ATopMonthDelta;
end;

function TcxMonthListBox.GetCalendar: TcxCustomCalendar;
begin
  Result := OwnerControl as TcxCustomCalendar;
end;

function TcxMonthListBox.GetCalendarTable: TcxCustomCalendarTable;
begin
  Result := Calendar.CalendarTable;
end;

function TcxMonthListBox.GetCalendarViewInfo: TcxCalendarClassicViewInfo;
begin
  Result := Calendar.ViewInfo as TcxCalendarClassicViewInfo;
end;

function TcxMonthListBox.GetDate: TDateTime;
begin
  if ItemIndex = -1 then
    Result := NullDate
  else
    Result := dxChangeDay(CalendarTable.AddMonths(FCurrentDate, TopMonthDelta + ItemIndex), 1);
end;

procedure TcxMonthListBox.SetItemIndex(Value: Integer);

  procedure InvalidateItemRect(AIndex: Integer);
  var
    R: TRect;
  begin
    if AIndex <> -1 then
    begin
      R.Left := BorderWidths[bLeft];
      R.Top := AIndex * ItemHeight + BorderWidths[bTop];
      R.Right := Width - BorderWidths[bRight];
      R.Bottom := R.Top + ItemHeight;
      cxInvalidateRect(Handle, R, False);
    end;
  end;

var
  APrevItemIndex: Integer;
begin
  if not HandleAllocated then Exit;
  if FItemIndex <> Value then
  begin
    if FItemIndex <> Value then
    begin
      begin
        APrevItemIndex := FItemIndex;
        FItemIndex := Value;
        InvalidateItemRect(APrevItemIndex);
        InvalidateItemRect(FItemIndex);
      end
    end;
  end;
end;

procedure TcxMonthListBox.SetShowYears(const Value: Boolean);
begin
  FShowYears := Value;
  if FShowYears then
    ControlStyle := ControlStyle - [csClickEvents]
  else
    ControlStyle := ControlStyle + [csClickEvents];
end;

procedure TcxMonthListBox.SetTopMonthDelta(Value: Integer);
begin
  if FTopMonthDelta <> Value then
  begin
    FTopMonthDelta := Value;
    Repaint;
  end;
end;

function TcxMonthListBox.CalculatePosition(const ASize: TSize): TPoint;
begin
  Result := FOrigin;
end;

procedure TcxMonthListBox.Click;
var
  ADate: TcxDateTime;
begin
  inherited Click;
  CloseUp;
  ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(FCurrentDate, TopMonthDelta));
  if ItemIndex <> -1 then
    Calendar.SetFirstDate(CalendarTable.AddMonths(CalendarTable.ToDateTime(ADate), ItemIndex));
end;

procedure TcxMonthListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
end;

procedure TcxMonthListBox.DoShowed;
begin
  if ShowYears then
    SetCaptureControl(Self)
  else
    SetCaptureControl(nil);
end;

procedure TcxMonthListBox.FontChanged;
begin
  Canvas.Font := Font;
  with Calendar do
  begin
    FItemHeight := FViewInfo.FHeaderHeight - 2;
    Self.Width := 6 * FViewInfo.FColWidth + 2;
    Self.Height := FItemCount * FItemHeight + 2;
  end;
end;

procedure TcxMonthListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if ShowYears then
    CloseUp;
end;

procedure TcxMonthListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  Times: array[0..3] of UINT = (500, 250, 100, 50);
var
  Delta: Integer;
  Interval: Integer;
begin
  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    FTimer.Enabled := False;
    ItemIndex := Y div ItemHeight;
  end
  else
  begin
    ItemIndex := -1;
    if Y < 0 then Delta := Y
    else
      if Y >= ClientHeight then
        Delta := 1 + Y - ClientHeight
      else
      begin
        FTimer.Enabled := False;
        Exit;
      end;
    FSign := Delta div Abs(Delta);
    Interval := Abs(Delta) div ItemHeight;
    if Interval > 3 then Interval := 3;
    if not FTimer.Enabled or (Times[Interval] <> FTimer.Interval) then
    begin
      FTimer.Interval := Times[Interval];
      FTimer.Enabled := True;
    end;
  end;
end;

procedure TcxMonthListBox.Paint;

  function GetItemColor(ASelected: Boolean): TColor;
  begin
    if ASelected then
      Result := clWindowText
    else
      Result := Calendar.Color;
  end;

var
  ASelected: Boolean;
  R: TRect;
  I: Integer;
  S: string;
  ADate: TcxDateTime;
  AConvertDate: TDateTime;
begin
  Canvas.FrameRect(GetControlRect(Self), clBlack);
  R := Rect(1, 1, Width - 1, ItemHeight + 1);
  ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(FCurrentDate, TopMonthDelta));
  for I := 0 to FItemCount - 1 do
  begin
    ASelected := I = ItemIndex;
    Canvas.Font.Color := GetItemColor(not ASelected);
    Canvas.Brush.Color := GetItemColor(ASelected);
    Canvas.FillRect(R);
    if CalendarTable.IsValidYear(ADate.Era, ADate.Year) then
    begin
      AConvertDate := CalendarTable.ToDateTime(ADate);
      if ShowYears then
        S := cxGetLocalMonthYear(AConvertDate, CalendarTable)
      else
        S := cxGetLocalMonthName(CalendarTable.ToDateTime(ADate), CalendarTable);
      Canvas.DrawText(S, R, cxAlignCenter or cxSingleLine, True);
    end;
    ADate := CalendarTable.FromDateTime(CalendarTable.AddMonths(ADate, 1));
    OffsetRect(R, 0, ItemHeight);
  end;
end;

{ TcxCalendarElementFadingHelper }

constructor TcxCalendarElementFadingHelper.Create(
  AOwner: TcxCalendarElementViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxCalendarElementFadingHelper.CanFade: Boolean;
begin
  Result := Owner.CanFade;
end;

procedure TcxCalendarElementFadingHelper.DrawFadeImage;
begin
  Owner.Invalidate;
end;

procedure TcxCalendarElementFadingHelper.GetFadingImages(out AFadeOutImage,
  AFadeInImage: TcxBitmap);

  function PrepareFadingImage(AState: TcxCalendarElementState): TcxBitmap32;
  var
    APrevState: TcxCalendarElementState;
  begin
    Result := TcxBitmap32.CreateSize(Owner.Bounds, True);
    Result.Canvas.Lock;
    Result.cxCanvas.WindowOrg := Owner.Bounds.TopLeft;
    APrevState := Owner.State;
    Owner.FState := AState;
    Owner.DrawFadeImage(Result.cxCanvas);
    Owner.FState := APrevState;
    Result.Canvas.Unlock;
  end;

begin
  AFadeOutImage := PrepareFadingImage(cesNormal);
  AFadeInImage := PrepareFadingImage(cesHot);
end;

{ TcxCalendarElementViewInfo }

constructor TcxCalendarElementViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create;
  FCalendar := ACalendar;
  FPainter := FCalendar.Painter;
  FHandleNeededElements := TdxFastObjectList.Create(False);
  CreateElements;
  FVisibleElements := TdxFastObjectList.Create(False);
  FFadingHelper := TcxCalendarElementFadingHelper.Create(Self);
end;

destructor TcxCalendarElementViewInfo.Destroy;
begin
  FreeAndNil(FFadingHelper);
  FreeAndNil(FVisibleElements);
  DestroyElements;
  FreeAndNil(FHandleNeededElements);
  inherited Destroy;
end;

procedure TcxCalendarElementViewInfo.Add(AElement: TcxCalendarElementViewInfo);
begin
  FVisibleElements.Add(AElement);
end;

procedure TcxCalendarElementViewInfo.AddHandleNeededElement(AElement: TcxCalendarElementViewInfo);
begin
  FHandleNeededElements.Add(AElement);
end;

procedure TcxCalendarElementViewInfo.AddVisibleElements;
begin
end;

procedure TcxCalendarElementViewInfo.CalculateBounds;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).CalculateBounds;
end;

procedure TcxCalendarElementViewInfo.CalculateChildSizes;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).CalculateSize;
end;

procedure TcxCalendarElementViewInfo.CalculateSize;
begin
  CalculateChildSizes;
  FSize := DoCalculateSize;
end;

procedure TcxCalendarElementViewInfo.ClearVisibleElements;
var
  I: Integer;
begin
  FIsRightToLeftConverted := False;
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).ClearVisibleElements;
  FVisibleElements.Clear;
end;

procedure TcxCalendarElementViewInfo.Click;
begin
  dxCallNotify(FOnClick, Self);
end;

procedure TcxCalendarElementViewInfo.CreateElements;
begin
end;

procedure TcxCalendarElementViewInfo.DblClick;
begin
end;

procedure TcxCalendarElementViewInfo.DestroyElements;
begin
end;

function TcxCalendarElementViewInfo.DoCalculateSize: TSize;
begin
  Result := cxNullSize;
end;

procedure TcxCalendarElementViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
end;

function TcxCalendarElementViewInfo.GetForbiddenStates: TcxCalendarElementStates;
begin
  Result := [];
end;

function TcxCalendarElementViewInfo.GetHitTest(AHitTest: TObject): Boolean;
var
  I: Integer;
begin
  Result := (GetHitTestIndex <> cchtNone) and PtInElement((AHitTest as TcxCalendarHitTest).HitPoint);
  if Result then
    SetHitTest(AHitTest);
  for I := FVisibleElements.Count - 1 downto 0 do
    if (FVisibleElements[I] as TcxCalendarElementViewInfo).GetHitTest(AHitTest) then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCalendarElementViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtNone;
end;

procedure TcxCalendarElementViewInfo.InitControls;
var
  I: Integer;
begin
  for I := 0 to FHandleNeededElements.Count - 1 do
    (FHandleNeededElements[I] as TcxCalendarElementViewInfo).InitControls;
end;

procedure TcxCalendarElementViewInfo.Initialize;
begin
  ClearVisibleElements;
  AddVisibleElements;
  InitializeVisibleElements;
end;

procedure TcxCalendarElementViewInfo.InitializeVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).Initialize;
end;

procedure TcxCalendarElementViewInfo.Invalidate;
begin
  Calendar.InvalidateRect(Bounds, True);
end;

procedure TcxCalendarElementViewInfo.KeyDown(var AKey: Word; AShift: TShiftState);
begin
end;

procedure TcxCalendarElementViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TcxCalendarElementViewInfo.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
end;

procedure TcxCalendarElementViewInfo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TcxCalendarElementViewInfo.Paint(ACanvas: TcxCanvas);
begin
  PaintChildren(ACanvas);
end;

procedure TcxCalendarElementViewInfo.PaintChildren(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).Paint(ACanvas);
end;

function TcxCalendarElementViewInfo.PtInElement(const APoint: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, APoint);
end;

procedure TcxCalendarElementViewInfo.Remove(AElement: TcxCalendarElementViewInfo);
begin
  FVisibleElements.Remove(AElement);
end;

procedure TcxCalendarElementViewInfo.RightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    for I := 0 to VisibleElements.Count - 1 do
      (VisibleElements[I] as TcxCalendarElementViewInfo).RightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TcxCalendarElementViewInfo.SetHitTest(AHitTest: TObject);
var
  ACalendarHitTest: TcxCalendarHitTest;
begin
  ACalendarHitTest := AHitTest as TcxCalendarHitTest;
  ACalendarHitTest.SetBitState(GetHitTestIndex, True);
  ACalendarHitTest.HitObject := Self;
end;

procedure TcxCalendarElementViewInfo.TranslationChanged;
var
  I: Integer;
begin
  for I := 0 to FVisibleElements.Count - 1 do
    (FVisibleElements[I] as TcxCalendarElementViewInfo).TranslationChanged;
end;

function TcxCalendarElementViewInfo.CanFade: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarElementViewInfo.DrawFadeImage(ACanvas: TcxCanvas);
begin
end;

procedure TcxCalendarElementViewInfo.DrawFadePart(ACanvas: TcxCanvas);
begin
  if not CanFade or not dxFader.DrawFadeImage(FadingHelper, ACanvas.Handle, Bounds) then
    DrawFadeImage(ACanvas);
end;

procedure TcxCalendarElementViewInfo.UpdateFader(const Value: TcxCalendarElementState);
begin
  if (Value = cesHot) and (FState = cesNormal) then
    FadingHelper.FadeIn
  else
    if (Value = cesNormal) and (FState = cesHot) then
      FadingHelper.FadeOut
    else
      FadingHelper.StopFading;
end;

function TcxCalendarElementViewInfo.GetController: TcxCalendarController;
begin
  Result := Calendar.Controller;
end;

function TcxCalendarElementViewInfo.GetFont: TFont;
begin
  Result := ViewInfo.Font;
end;

function TcxCalendarElementViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := ViewInfo.LookAndFeelPainter;
end;

function TcxCalendarElementViewInfo.GetViewInfo: TcxCalendarViewInfo;
begin
  Result := FCalendar.ViewInfo;
end;

function TcxCalendarElementViewInfo.GetVisibleElementsCount: Integer;
begin
  Result := FVisibleElements.Count;
end;

procedure TcxCalendarElementViewInfo.SetState(
  const Value: TcxCalendarElementState);
begin
  if FState <> Value then
  begin
    if CanFade then
      UpdateFader(Value);
    FState := Value;
    Invalidate;
  end;
end;

{ TcxCalendarArrowViewInfo }

constructor TcxCalendarArrowViewInfo.Create(
  ACalendar: TcxCustomCalendar; ADirection: TcxArrowDirection);
begin
  inherited Create(ACalendar);
  FDirection := ADirection;
end;

procedure TcxCalendarArrowViewInfo.DoAction;
begin
  Calendar.DoStep(GetArrowType);
end;

function TcxCalendarArrowViewInfo.DoCalculateSize: TSize;
begin
  Result := ViewInfo.GetArrowSize;
end;

function TcxCalendarArrowViewInfo.GetArrowType: TcxCalendarArrow;
begin
  if Controller.HitTest.HitAtMonthDateHeader then
  begin
    if FDirection = adLeft then
      Result := caPrevMonth
    else
      Result := caNextMonth
  end
  else
  begin
    if FDirection = adLeft then
      Result := caPrevYear
    else
      Result := caNextYear
  end;
end;

function TcxCalendarArrowViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtDateHeaderArrow;
end;

function TcxCalendarArrowViewInfo.GetRealDirection: TcxArrowDirection;
begin
  Result := FDirection;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertArrowDirection(Result);
end;

procedure TcxCalendarArrowViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoAction;
  Controller.FTimer.Enabled := True;
end;

procedure TcxCalendarArrowViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawArrow(ACanvas, Self);
end;

{ TcxCalendarModernArrowViewInfo }

function TcxCalendarModernArrowViewInfo.GetArrowType: TcxCalendarArrow;
const
  AAction: array [TcxCalendarModernDateViewStyle, adLeft..adRight] of TcxCalendarArrow =
    ((caPrevMonth, caNextMonth), (caPrevYear, caNextYear),
     (caPrevDecade, caNextDecade), (caPrevCentury, caNextCentury));
begin
  Result := AAction[(ViewInfo.FDateViewInfo as TcxCalendarModernDateViewInfo).ViewStyle, FDirection];
end;

{ TcxCalendarDateCellViewInfo }

procedure TcxCalendarDateCellViewInfo.CalculateBounds;
begin
  FTextRect := CalculateTextRect;
  inherited CalculateBounds;
end;

function TcxCalendarDateCellViewInfo.CalculateTextRect: TRect;
begin
  Result := Bounds;
end;

function TcxCalendarDateCellViewInfo.DoCalculateSize: TSize;
begin
  Result := GetMinSize;
end;

function TcxCalendarDateCellViewInfo.DoCalculateValue: Double;
begin
  Result := InvalidDate;
end;

procedure TcxCalendarDateCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas);
begin
end;

procedure TcxCalendarDateCellViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, ABounds);
end;

function TcxCalendarDateCellViewInfo.GetContentOffset: TRect;
begin
  Result := Rect(4, 4, 4, 4);
  //Result := Rect(2, 2, 2, 2);
end;

function TcxCalendarDateCellViewInfo.GetDrawStates: TcxCalendarElementStates;
begin
  Result := [];
  if State = cesHot then
    Include(Result, cesHot);
  if IsSelected then
  begin
    if Calendar.Focused then
      Include(Result, cesFocused);
    Include(Result, cesSelected);
  end;
  if not IsEnabled then
    Include(Result, cesDisabled);
  Result := Result - GetForbiddenStates;
end;

function TcxCalendarDateCellViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtDateTableCell;
end;

function TcxCalendarDateCellViewInfo.GetMinSize: TSize;
begin
  Result := cxNullSize;
end;

function TcxCalendarDateCellViewInfo.GetText: string;
begin
  Result := '';
end;

function TcxCalendarDateCellViewInfo.GetRealHorzAlignment: TAlignment;
begin
  Result := FHorzAlignment;
  if IsRightToLeftConverted then
    ChangeBiDiModeAlignment(Result);
end;

procedure TcxCalendarDateCellViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Select;
end;

procedure TcxCalendarDateCellViewInfo.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if ssLeft in Shift then
    Select;
end;

procedure TcxCalendarDateCellViewInfo.Paint(ACanvas: TcxCanvas);
var
  AStates: TcxCalendarElementStates;
begin
  if Calendar.CanShowDate(Value) and cxGetLocalCalendar.IsValidDate(Value) then
  begin
    ACanvas.SaveState;
    try
      ACanvas.Font := Font;
      AStates := GetDrawStates;
      ACanvas.Font.Color := Painter.GetCellTextColor(AStates);
      ACanvas.Brush.Color := GetBrushColor(AStates);
      DoCustomDraw(ACanvas);
      Painter.DrawDateCellBackground(ACanvas, Self);
      DrawFadePart(ACanvas);
      Painter.DrawDateCellText(ACanvas, Self);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

procedure TcxCalendarDateCellViewInfo.Initialize;
begin
  inherited;
  FValue := DoCalculateValue;
end;

function TcxCalendarDateCellViewInfo.IsEnabled: Boolean;
begin
  Result := False;
end;

function TcxCalendarDateCellViewInfo.IsSelected: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarDateCellViewInfo.Select;
begin
  Calendar.InternalSetSelectDate(Value, False);
end;

procedure TcxCalendarDateCellViewInfo.SetPosition(X, Y, AIndex: Integer);
begin
  FColumn := X;
  FRow := Y;
  FIndex := AIndex;
end;

function TcxCalendarDateCellViewInfo.CanFade: Boolean;
begin
  Result := ViewInfo.CanFade;
end;

procedure TcxCalendarDateCellViewInfo.DrawFadeImage(ACanvas: TcxCanvas);
begin
  Painter.DrawDateCellSelection(ACanvas, Self);
end;

procedure TcxCalendarDateCellViewInfo.UpdateFader(const Value: TcxCalendarElementState);
begin
  if IsSelected then
    FadingHelper.StopFading
  else
    inherited UpdateFader(Value);
end;

function TcxCalendarDateCellViewInfo.GetSelectDate: TDateTime;
begin
  Result := Calendar.SelectDate;
  if Result = NullDate then
    Result := Date;
end;

function TcxCalendarDateCellViewInfo.GetBrushColor(
  AStates: TcxCalendarElementStates): TColor;
begin
  if cesSelected in AStates then
    Result := LookAndFeelPainter.DefaultDateNavigatorSelectionColor
  else
    Result := Calendar.BackgroundColor;
end;

{ TcxCalendarDayCellViewInfo }

function TcxCalendarDayCellViewInfo.CalculateTextRect: TRect;
var
  ASize: TSize;
begin
  ASize := cxTextExtent(Font, GetText);
  Result := cxRectCenter(Bounds, 3 * cxTextWidth(Font, '0'), ASize.cy);
  Result.Right := Result.Right - 3;
  Result.Left := Result.Right - ASize.cx;
end;

procedure TcxCalendarDayCellViewInfo.DblClick;
begin
  Calendar.SelectDate := FDate;
  Calendar.DoDateTimeChanged;
  Calendar.Hide(crEnter);
end;

function TcxCalendarDayCellViewInfo.DoCalculateValue: Double;
begin
  FDate := Calendar.GetDateFromCell(Column, Row);
  Result := FDate;
end;

procedure TcxCalendarDayCellViewInfo.DoCustomDraw(ACanvas: TcxCanvas);
var
  AColor: TColor;
  AStates: TcxCalendarElementStates;
begin
  AStates := GetDrawStates;
  AColor := ACanvas.Brush.Color;
  if Assigned(Calendar.FOnGetDayOfWeekState) then
    Calendar.FOnGetDayOfWeekState(Calendar, dxDayOfWeek(FDate),
      CalendarElementStateToCustomDrawState(AStates), ACanvas.Font, AColor);
  if Assigned(Calendar.FOnGetDayState) then
    Calendar.FOnGetDayState(Calendar, FDate,
      CalendarElementStateToCustomDrawState(AStates), ACanvas.Font, AColor);
  if ACanvas.Brush.Color <> AColor then
    ACanvas.FillRect(Bounds, AColor);
end;

function TcxCalendarDayCellViewInfo.GetDrawStates: TcxCalendarElementStates;
begin
  Result := inherited GetDrawStates;
  if FDate = Date then
    Include(Result, cesMarked)
  else
    Include(Result, cesNormal);
end;

function TcxCalendarDayCellViewInfo.GetForbiddenStates: TcxCalendarElementStates;
begin
  Result := [cesHot, cesFocused];
end;

function TcxCalendarDayCellViewInfo.GetMinSize: TSize;
begin
  Result := cxSize(ViewInfo.FColWidth, ViewInfo.FRowHeight);
end;

function TcxCalendarDayCellViewInfo.GetText: string;
begin
  Result := cxDayNumberToLocalFormatStr(FDate);
end;

procedure TcxCalendarDayCellViewInfo.Initialize;
begin
  inherited Initialize;
  FHorzAlignment := taRightJustify;
end;

function TcxCalendarDayCellViewInfo.IsEnabled: Boolean;
begin
  Result := InRange(FDate, Calendar.FirstDate, Calendar.GetLastDate);
end;

function TcxCalendarDayCellViewInfo.IsSelected: Boolean;
begin
  Result := FDate = GetSelectDate;
end;

procedure TcxCalendarDayCellViewInfo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsSelected then
  begin
    if ViewInfo.AllowHidePopupOnMouseUp then
    begin
      Calendar.DoDateTimeChanged;
      Calendar.Hide(crEnter);
    end
    else
      Calendar.FirstDate := Calendar.SelectDate;
  end;
end;

{ TcxCalendarModernDayCellViewInfo }

function TcxCalendarModernDayCellViewInfo.CalculateTextRect: TRect;
begin
  Result := cxRectCenter(Bounds, cxTextSize(Font, '00'));
end;

function TcxCalendarModernDayCellViewInfo.GetContentOffset: TRect;
begin
  Result := Rect(2, 1, 2, 1);
end;

function TcxCalendarModernDayCellViewInfo.GetForbiddenStates: TcxCalendarElementStates;
begin
  Result := [];
end;

function TcxCalendarModernDayCellViewInfo.GetMinSize: TSize;
var
  ARect: TRect;
begin
  ARect := cxRect(cxTextSize(Font, 'Wed'));
  ARect := cxRectInflate(ARect, GetContentOffset);
  Result := cxSize(ARect);
  dxAdjustToTouchableSize(Result, Calendar.ScaleFactor);
end;

{ TcxCalendarModernLargeCellViewInfo }

function TcxCalendarModernLargeCellViewInfo.CalculateTextRect: TRect;
begin
  Result := cxRectCenter(Bounds, cxTextSize(Font, GetText));
end;

function TcxCalendarModernLargeCellViewInfo.IsEnabled: Boolean;
begin
  Result := InRange(Index, 1, 10);
end;

procedure TcxCalendarModernLargeCellViewInfo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (ViewInfo.FDateViewInfo as TcxCalendarModernDateViewInfo).PreviousViewStyle;
end;

{ TcxCalendarMonthCellViewInfo }

function TcxCalendarModernMonthCellViewInfo.DoCalculateSize: TSize;
begin
  Result := (ViewInfo as TcxCalendarModernViewInfo).DateViewInfo.TableViewInfo[cvdsYear].CellSize;
end;

function TcxCalendarModernMonthCellViewInfo.DoCalculateValue: Double;
begin
  FMonth := Index + 1;
  Result := dxChangeMonth(Calendar.SelectDate, FMonth);
end;

function TcxCalendarModernMonthCellViewInfo.GetMinSize: TSize;
var
  ARect: TRect;
begin
  ARect := cxRect(cxTextSize(Font, 'Feb'));
  ARect := cxRectInflate(ARect, GetContentOffset);
  Result := cxSize(ARect);
  dxAdjustToTouchableSize(Result, Calendar.ScaleFactor);
end;

function TcxCalendarModernMonthCellViewInfo.GetText: string;
begin
  Result := dxFormatSettings.ShortMonthNames[FMonth];
end;

function TcxCalendarModernMonthCellViewInfo.IsEnabled: Boolean;
begin
  Result := True;
end;

function TcxCalendarModernMonthCellViewInfo.IsSelected: Boolean;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(GetSelectDate, AYear, AMonth, ADay);
  Result := FMonth = AMonth;
end;

{ TcxCalendarYearCellViewInfo }

function TcxCalendarModernYearCellViewInfo.DoCalculateSize: TSize;
begin
  Result := (ViewInfo as TcxCalendarModernViewInfo).DateViewInfo.TableViewInfo[cvdsDecade].CellSize;
end;

function TcxCalendarModernYearCellViewInfo.DoCalculateValue: Double;
begin
  FYear := cxGetLocalCalendar.GetYear(Calendar.SelectDate) div 10 * 10 - 1 + Word(Index);
  if cxGetLocalCalendar.IsValidYear(FYear) then
    Result := dxChangeYear(Calendar.SelectDate, FYear)
  else
    Result := InvalidDate;
end;

function TcxCalendarModernYearCellViewInfo.GetMinSize: TSize;
var
  ARect: TRect;
begin
  ARect := cxRect(cxTextSize(Font, '9999'));
  ARect := cxRectInflate(ARect, GetContentOffset);
  Result := cxSize(ARect);
  dxAdjustToTouchableSize(Result, Calendar.ScaleFactor);
end;

function TcxCalendarModernYearCellViewInfo.GetText: string;
begin
  Result := cxGetLocalYear(EncodeDate(FYear, 1, 1), cxGetLocalCalendar);
end;

function TcxCalendarModernYearCellViewInfo.IsSelected: Boolean;
begin
  Result := cxGetLocalCalendar.GetYear(Calendar.SelectDate) = FYear;
end;

{ TcxCalendarDecadeCellViewInfo }

function TcxCalendarModernDecadeCellViewInfo.DoCalculateSize: TSize;
begin
  Result := (ViewInfo as TcxCalendarModernViewInfo).DateViewInfo.TableViewInfo[cvdsCentury].CellSize;
end;

function TcxCalendarModernDecadeCellViewInfo.DoCalculateValue: Double;
begin
  FStartYear := GetSelectedYear div 100 * 100 - 10 + Index * 10;
  FEndYear := FStartYear + 9;
  if cxGetLocalCalendar.IsValidYear(FStartYear) then
    Result := dxChangeDecade(Calendar.SelectDate, FStartYear)
  else
    Result := InvalidDate;
end;

function TcxCalendarModernDecadeCellViewInfo.GetMinSize: TSize;
var
  ARect: TRect;
begin
  ARect := cxRect(cxTextSize(Font, '9999-'));
  ARect.Bottom := ARect.Bottom * 2;
  ARect := cxRectInflate(ARect, GetContentOffset);
  Result := cxSize(ARect);
  dxAdjustToTouchableSize(Result, Calendar.ScaleFactor);
end;

function TcxCalendarModernDecadeCellViewInfo.GetSelectedYear: Word;
begin
  Result := cxGetLocalCalendar.GetYear(Calendar.SelectDate);
end;

function TcxCalendarModernDecadeCellViewInfo.GetText: string;

  function GetYearName: string;
  var
    AStartDate, AEndDate: TDateTime;
  begin
    AStartDate := EncodeDate(FStartYear, 1, 1);
    AEndDate := EncodeDate(FEndYear, 1, 1);
    Result := cxGetLocalYear(AStartDate, cxGetLocalCalendar) + '-' + dxCRLF +
      cxGetLocalYear(AEndDate, cxGetLocalCalendar);
  end;

begin
  Result := GetYearName;
end;

function TcxCalendarModernDecadeCellViewInfo.IsSelected: Boolean;
begin
  Result := InRange(GetSelectedYear, FStartYear, FEndYear);
end;

{ TcxCalendarDateTableViewInfo }

procedure TcxCalendarDateTableViewInfo.AddVisibleElements;
var
  I: Integer;
begin
  for I := 0 to FDateCells.Count - 1 do
    Add(DateCells[I]);
end;

procedure TcxCalendarDateTableViewInfo.CreateElements;
var
  X: Integer;
  Y: Integer;
  ADateCell: TcxCalendarDateCellViewInfo;
begin
  FDateCells := TdxFastObjectList.Create;
  for Y := 0 to GetTableSize.cy - 1 do
    for X := 0 to GetTableSize.cx - 1 do
    begin
      ADateCell := CreateDateCell;
      ADateCell.SetPosition(X, Y, X + Y * GetTableSize.cx);
      FDateCells.Add(ADateCell);
    end;
end;

function TcxCalendarDateTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := nil;
end;

procedure TcxCalendarDateTableViewInfo.DestroyElements;
begin
  FreeAndNil(FDateCells);
end;

function TcxCalendarDateTableViewInfo.DoCalculateSize: TSize;
begin
  Result := GetMinSize;
end;

function TcxCalendarDateTableViewInfo.IsDateEnabled(ADate: TDateTime): Boolean;
begin
  Result := False;
end;

function TcxCalendarDateTableViewInfo.GetMinSize: TSize;
begin
  Result := cxSize(GetTableSize.cx * DateCells[0].GetMinSize.cx,
    GetTableSize.cy * DateCells[0].GetMinSize.cy);
end;

function TcxCalendarDateTableViewInfo.GetSelectedCell: TcxCalendarDateCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FDateCells.Count - 1 do
  begin
    if DateCells[I].IsSelected then
    begin
      Result := DateCells[I];
      Break;
    end;
  end;
end;

function TcxCalendarDateTableViewInfo.GetTableSize: TSize;
begin
  Result := cxSize(7, 6);
end;

procedure TcxCalendarDateTableViewInfo.KeyDown(var AKey: Word;
  AShift: TShiftState);
var
  ASelectDate: TDateTime;
begin
  ASelectDate := Calendar.SelectDate;
  if IsRightToLeftConverted then
    AKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(AKey);
  case AKey of
    VK_LEFT:
      ASelectDate := GetPreviousDate(ASelectDate);
    VK_RIGHT:
      ASelectDate := GetNextDate(ASelectDate);
    VK_UP:
      if AShift <> [ssAlt] then
        ASelectDate := GetDateInPreviousRow(ASelectDate);
    VK_DOWN:
      if AShift <> [ssAlt] then
        ASelectDate := GetDateInNextRow(ASelectDate);
    VK_HOME:
      if AShift = [ssCtrl] then
        ASelectDate := GetFirstDate(ASelectDate)
      else
        ASelectDate := GetFirstDateInRow(ASelectDate);
    VK_END:
      if AShift = [ssCtrl] then
        ASelectDate := GetLastDate(ASelectDate)
      else
        ASelectDate := GetLastDateInRow(ASelectDate);
    VK_PRIOR:
      ASelectDate := GetPreviousPageDate(ASelectDate);
    VK_NEXT:
      ASelectDate := GetNextPageDate(ASelectDate);
  end;
  Calendar.SelectDate := ASelectDate;
end;

procedure TcxCalendarDateTableViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited;
end;

function TcxCalendarDateTableViewInfo.GetDateInNextRow(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetDateInPreviousRow(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetFirstDate(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetFirstDateInRow(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetLastDate(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetLastDateInRow(ASelectDate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetNextDate(
  ADate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetNextPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetPreviousDate(
  ADate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetPreviousPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := NullDate;
end;

function TcxCalendarDateTableViewInfo.GetDateCells(
  Index: Integer): TcxCalendarDateCellViewInfo;
begin
  Result := FDateCells[Index] as TcxCalendarDateCellViewInfo;
end;

{ TcxCalendarDayTableViewInfo }

procedure TcxCalendarMonthTableViewInfo.CalculateBounds;
var
  I: Integer;
  ACellRect: TRect;
  ACellPos: TPoint;
begin
  for I := 0 to FDateCells.Count - 1 do
  begin
    ACellRect := cxRect(DateCells[I].Size);
    ACellPos := Point(Bounds.Left + GetWeekNumbersRegionWidth +
      DateCells[I].Column * DateCells[I].Size.cx,
      Bounds.Top + FDaysOfWeekHeight + DateCells[I].Row * DateCells[I].Size.cy);
    DateCells[I].Bounds := cxRectSetOrigin(ACellRect, ACellPos);
  end;
  inherited CalculateBounds;
end;

function TcxCalendarMonthTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := TcxCalendarDayCellViewInfo.Create(Calendar);
end;

procedure TcxCalendarMonthTableViewInfo.DrawDelimeters(ACanvas: TcxCanvas);
var
  ASeparator2Color: TColor;
  ATextRect: TRect;
  AWeekNumbersDelimiterPos: Integer;
  P1, P2: TPoint;
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := Calendar.BackgroundColor;

  ASeparator2Color := LookAndFeelPainter.DefaultDateNavigatorSeparator2Color;
  if (ASeparator2Color = clNone) or (ASeparator2Color = clDefault) then
    ASeparator2Color := Calendar.BackgroundColor;

  if not IsRightToLeftConverted then
  begin
    ATextRect.Left := Bounds.Left + GetWeekNumbersRegionWidth;
    ATextRect.Right := Bounds.Right;
  end
  else
  begin
    ATextRect.Left := Bounds.Left;
    ATextRect.Right := Bounds.Right - GetWeekNumbersRegionWidth;
  end;
  ATextRect.Top := Bounds.Top;
  ATextRect.Bottom := ATextRect.Top + FDaysOfWeekHeight - 2;
  ACanvas.FillRect(Rect(ATextRect.Left - 8, ATextRect.Top, ATextRect.Left, ATextRect.Bottom + 2));
  ACanvas.FillRect(Rect(ATextRect.Right, ATextRect.Top, ATextRect.Right + 8, ATextRect.Bottom + 2));

  InternalPolyLine(ACanvas, [Point(ATextRect.Left, ATextRect.Bottom), Point(ATextRect.Right - 1, ATextRect.Bottom)],
    LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
  InternalPolyLine(ACanvas, [Point(ATextRect.Left, ATextRect.Bottom + 1),
    Point(ATextRect.Right - 1, ATextRect.Bottom + 1)], ASeparator2Color, True);

  if not ViewInfo.IsTimeVisible and ViewInfo.IsButtonsVisible then
  begin
    P1 := Point(ATextRect.Left, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
    P2 := Point(ATextRect.Right - 1, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
    InternalPolyLine(ACanvas, [P1, P2], clBtnShadow, True);
    ACanvas.ExcludeClipRect(cxRectBounds(P1, P2.X - P1.X + 1, 1));
  end;

  if Calendar.WeekNumbers then
  begin
    ACanvas.Brush.Color := Calendar.BackgroundColor;
    if not IsRightToLeftConverted then
    begin
      AWeekNumbersDelimiterPos := Bounds.Left + FWeekNumberWidth + WeekNumbersDelimiterOffset.Left;
      ACanvas.FillRect(Rect(Bounds.Left, ATextRect.Top, AWeekNumbersDelimiterPos, Bounds.Bottom));
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos, Bounds.Top),
        Point(AWeekNumbersDelimiterPos, ATextRect.Bottom - 1)], Calendar.BackgroundColor, True);
      if (ViewInfo.Kind = ckDate) and ViewInfo.IsButtonsVisible then
      begin
        P1 := Point(AWeekNumbersDelimiterPos, ATextRect.Bottom);
        P2 := Point(AWeekNumbersDelimiterPos, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
        InternalPolyLine(ACanvas, [P1, P2], clBtnShadow, True);
        P1 := P2;
        P2 := Point(AWeekNumbersDelimiterPos + 1, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
        InternalPolyLine(ACanvas, [P2, P2], clBtnShadow, True);
        ACanvas.ExcludeClipRect(cxRectBounds(P1, P2.X - P1.X + 1, 1));
      end
      else
      begin
        InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos, ATextRect.Bottom),
          Point(AWeekNumbersDelimiterPos, Bounds.Bottom - 3)],
          LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
        InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos, Bounds.Bottom - 2),
          Point(AWeekNumbersDelimiterPos, Bounds.Bottom - 2)], Calendar.BackgroundColor, True);
      end;
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos + 1, ATextRect.Bottom),
        Point(AWeekNumbersDelimiterPos + 1, ATextRect.Bottom)],
        LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos + 1, ATextRect.Bottom + 1),
        Point(AWeekNumbersDelimiterPos + 1, ATextRect.Bottom + 1)], ASeparator2Color, True);
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos + 1, ATextRect.Bottom + 2),
        Point(AWeekNumbersDelimiterPos + 1, Bounds.Bottom - 1)], Calendar.BackgroundColor, True);
    end
    else
    begin
      AWeekNumbersDelimiterPos := Bounds.Right - FWeekNumberWidth - WeekNumbersDelimiterOffset.Left;
      ACanvas.Brush.Color := Calendar.BackgroundColor;
      ACanvas.FillRect(Rect(AWeekNumbersDelimiterPos, ATextRect.Top, Bounds.Right, Bounds.Bottom));
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos, Bounds.Top),
        Point(AWeekNumbersDelimiterPos, ATextRect.Bottom - 1)], Calendar.BackgroundColor, True);
      if (ViewInfo.Kind = ckDate) and ViewInfo.IsButtonsVisible then
      begin
        P1 := Point(AWeekNumbersDelimiterPos - 1, ATextRect.Bottom);
        P2 := Point(AWeekNumbersDelimiterPos - 1, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
        InternalPolyLine(ACanvas, [P1, P2], clBtnShadow, True);
        P1 := P2;
        P2 := Point(AWeekNumbersDelimiterPos - 2, Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight - 1);
        InternalPolyLine(ACanvas, [P2, P2], clBtnShadow, True);
        ACanvas.ExcludeClipRect(cxRectBounds(P2, P1.X - P2.X + 1, 1));
      end
      else
      begin
        InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos - 1, ATextRect.Bottom),
          Point(AWeekNumbersDelimiterPos - 1, Bounds.Bottom - 3)],
          LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
        InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos - 1, Bounds.Bottom - 2),
          Point(AWeekNumbersDelimiterPos - 1, Bounds.Bottom - 2)], Calendar.BackgroundColor, True);
      end;
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos - 2, ATextRect.Bottom),
        Point(AWeekNumbersDelimiterPos - 2, ATextRect.Bottom)],
        LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos - 2, ATextRect.Bottom + 1),
        Point(AWeekNumbersDelimiterPos - 2, ATextRect.Bottom + 1)], ASeparator2Color, True);
      InternalPolyLine(ACanvas, [Point(AWeekNumbersDelimiterPos - 2, ATextRect.Bottom + 2),
        Point(AWeekNumbersDelimiterPos - 2, Bounds.Bottom - 1)], Calendar.BackgroundColor, True);
    end;
  end;
end;

procedure TrueTextRect(ACanvas: TCanvas; R: TRect; X, Y: Integer;
  const Text: WideString);
begin
  ACanvas.TextRect(R, X, Y, Text);
end;

procedure TcxCalendarMonthTableViewInfo.DrawWeekDays(ACanvas: TcxCanvas);
var
  ATextRect: TRect;
  I, ALeft: Integer;
  J: TDay;
  AColor: TColor;
  S: string;
  ASize: TSize;
begin
  ACanvas.Font := Font;
  ACanvas.Brush.Color := Calendar.BackgroundColor;
  if not IsRightToLeftConverted then
  begin
    ATextRect.Left := Bounds.Left + GetWeekNumbersRegionWidth;
    ATextRect.Right := ATextRect.Left;
  end
  else
  begin
    ATextRect.Right := Bounds.Right - GetWeekNumbersRegionWidth;
    ATextRect.Left := ATextRect.Right;
  end;
  ATextRect.Top := Bounds.Top;
  ATextRect.Bottom := ATextRect.Top + FDaysOfWeekHeight - 2;

  ATextRect.Right := ATextRect.Left;
  ACanvas.Font.Color := LookAndFeelPainter.DefaultDateNavigatorTextColor;
  for I := Low(TdxDayOfWeek) to High(TdxDayOfWeek) do
  begin
    ATextRect := GetNextWeekDayTextRect(ATextRect);
    J := dxGetDayOfWeek(dxGetStartOfWeek, I);

    ACanvas.SaveState;
    try
      if Assigned(Calendar.FOnGetDayOfWeekState) then
      begin
        AColor := ACanvas.Brush.Color;
        Calendar.FOnGetDayOfWeekState(Calendar, J, [cdsDefault], ACanvas.Font, AColor);
        ACanvas.Brush.Color := AColor;
      end;

      S := GetDayOfWeekName(J, ACanvas.Font.Charset);
      ASize := ACanvas.TextExtent(S);
      if not IsRightToLeftConverted then
        ALeft := ATextRect.Right - 3 - ASize.cx
      else
        ALeft := ATextRect.Left + 4;
      TrueTextRect(ACanvas.Canvas, ATextRect, ALeft, (ATextRect.Top + ATextRect.Bottom - ASize.cy) div 2, S);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

procedure TcxCalendarMonthTableViewInfo.DrawWeekNumbers(ACanvas: TcxCanvas);
const
  AHorzAlign: array[Boolean] of Integer = (cxAlignRight, cxAlignLeft);
var
  I: Integer;
  R: TRect;
begin
  if not Calendar.WeekNumbers then
    Exit;

  ACanvas.Brush.Color := Calendar.BackgroundColor;
  ACanvas.Font := Font;
  ACanvas.Font.Height := MulDiv(ACanvas.Font.Height, 2, 3);
  ACanvas.Font.Color := LookAndFeelPainter.DefaultDateNavigatorTextColor;
  for I := 0 to 5 do
  begin
    if not cxIsDateValid(Calendar.GetDateFromCell(0, I)) then
      Continue;
    if not IsRightToLeftConverted then
    begin
      R.Left := Bounds.Left;
      R.Right := R.Left + FWeekNumberWidth;
    end
    else
    begin
      R.Right := Bounds.Right;
      R.Left := R.Right - FWeekNumberWidth;
    end;
    R.Top := Bounds.Top + FDaysOfWeekHeight + DateCells[0].Size.cy * I;
    R.Bottom := R.Top + DateCells[0].Size.cy;

    ACanvas.DrawTexT(IntToStr(cxGetLocalCalendar.GetWeekNumber(Calendar.GetDateFromCell(0, I),
      TDay(cxFormatController.StartOfWeek), cxFormatController.FirstWeekOfYear)),
      R, AHorzAlign[IsRightToLeftConverted] or cxAlignVCenter);
  end;
  ACanvas.Font := Font;
end;

function TcxCalendarMonthTableViewInfo.GetDayOfWeekName(ADay: TDay;
  AFontCharset: TFontCharset): string;
begin
  Result := cxGetDayOfWeekName(ADay, AFontCharset);
end;

function TcxCalendarMonthTableViewInfo.GetMinSize: TSize;
begin
  FDaysOfWeekHeight := DateCells[0].GetMinSize.cy + 1;
  Result := inherited GetMinSize;
  Inc(Result.cx, GetWeekNumbersRegionWidth);
  Inc(Result.cy, FDaysOfWeekHeight);
end;

function TcxCalendarMonthTableViewInfo.GetWeekNumbersRegionWidth: Integer;
begin
  if Calendar.WeekNumbers then
    Result := FWeekNumberWidth + WeekNumbersDelimiterOffset.Left +
      WeekNumbersDelimiterWidth + WeekNumbersDelimiterOffset.Right
  else
    Result := 0;
end;

procedure TcxCalendarMonthTableViewInfo.InitializeVisibleElements;
begin
  inherited InitializeVisibleElements;
  FWeekNumberWidth := cxTextWidth(Font, '99', MulDiv(Font.Size, 2, 3));
end;

function TcxCalendarMonthTableViewInfo.IsDateEnabled(ADate: TDateTime): Boolean;
begin
  Result := (ADate >= Calendar.FirstDate) and (ADate <= Calendar.GetLastDate);
end;

procedure TcxCalendarMonthTableViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited Paint(ACanvas);
  DrawDelimeters(ACanvas);
  DrawWeekDays(ACanvas);
  DrawWeekNumbers(ACanvas);
  ACanvas.ExcludeClipRect(Bounds);
end;

function TcxCalendarMonthTableViewInfo.GetDateInNextRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := ASelectDate + GetTableSize.cx;
end;

function TcxCalendarMonthTableViewInfo.GetDateInPreviousRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := ASelectDate - GetTableSize.cx;
end;

function TcxCalendarMonthTableViewInfo.GetFirstDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeDay(ASelectDate, 1);
end;

function TcxCalendarMonthTableViewInfo.GetFirstDateInRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := ASelectDate - dxDayOfWeekOffset(ASelectDate);
end;

function TcxCalendarMonthTableViewInfo.GetLastDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxGetEndDateOfMonth(ASelectDate, True);
end;

function TcxCalendarMonthTableViewInfo.GetLastDateInRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := ASelectDate + (6 - dxDayOfWeekOffset(ASelectDate));
end;

function TcxCalendarMonthTableViewInfo.GetNextDate(ADate: TDateTime): TDateTime;
begin
  Result := ADate + 1;
end;

function TcxCalendarMonthTableViewInfo.GetNextPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ADate, 1);
end;

function TcxCalendarMonthTableViewInfo.GetNextWeekDayTextRect(const ATextRect: TRect): TRect;
begin
  Result := ATextRect;
  if not IsRightToLeftConverted then
  begin
    Result.Left := Result.Right;
    Result.Right := Result.Left + DateCells[0].Size.cx;
  end
  else
  begin
    Result.Right := Result.Left;
    Result.Left := Result.Right - DateCells[0].Size.cx;
  end;
end;

function TcxCalendarMonthTableViewInfo.GetPreviousDate(
  ADate: TDateTime): TDateTime;
begin
  Result := ADate - 1;
end;

function TcxCalendarMonthTableViewInfo.GetPreviousPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ADate, -1);
end;

{ TcxCalendarModernMonthTableViewInfo }

procedure TcxCalendarModernMonthTableViewInfo.CalculateBounds;
begin
  inherited CalculateBounds;

end;

function TcxCalendarModernMonthTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := TcxCalendarModernDayCellViewInfo.Create(Calendar);
end;

procedure TcxCalendarModernMonthTableViewInfo.DrawDelimeters(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R.Left := Bounds.Left;
  if not Calendar.WeekNumbers then
    Inc(R.Left, GetWeekNumbersRegionWidth);
  R.Right := Bounds.Right;
  R.Top := Bounds.Top + FDaysOfWeekHeight - 2;
  R.Bottom := R.Top + 1;
  ACanvas.FillRect(R, $F5F5F5);//$E3E3E3
  if Calendar.WeekNumbers then
  begin
    R.Left := Bounds.Left + FWeekNumberWidth + WeekNumbersDelimiterOffset.Left;
    R.Right := R.Left + WeekNumbersDelimiterWidth;
    R.Top := DateCells[0].Bounds.Top;
    R.Bottom := Bounds.Bottom;
    if IsRightToLeftConverted then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, Bounds);
    ACanvas.FillRect(R, $F5F5F5);
  end;
end;

procedure TcxCalendarModernMonthTableViewInfo.DrawWeekDays(ACanvas: TcxCanvas);
var
  ATextRect: TRect;
  I: Integer;
  J: TDay;
  AColor: TColor;
  S: string;
begin
  ACanvas.Font := Font;
  ACanvas.Brush.Color := Calendar.BackgroundColor;
  if not IsRightToLeftConverted then
  begin
    ATextRect.Left := DateCells[0].Bounds.Left;// Bounds.Left + GetWeekNumbersRegionWidth;
    ATextRect.Right := ATextRect.Left;
  end
  else
  begin
    ATextRect.Right := DateCells[0].Bounds.Right;
    ATextRect.Left := ATextRect.Right;
  end;
  ATextRect.Top := Bounds.Top;
  ATextRect.Bottom := ATextRect.Top + FDaysOfWeekHeight - 2;

  ATextRect.Right := ATextRect.Left;
  ACanvas.Font.Color := LookAndFeelPainter.DefaultDateNavigatorTextColor;
  for I := Low(TdxDayOfWeek) to High(TdxDayOfWeek) do
  begin
    ATextRect := GetNextWeekDayTextRect(ATextRect);
    J := dxGetDayOfWeek(dxGetStartOfWeek, I);

    ACanvas.SaveState;
    try
      if Assigned(Calendar.FOnGetDayOfWeekState) then
      begin
        AColor := ACanvas.Brush.Color;
        Calendar.FOnGetDayOfWeekState(Calendar, J, [cdsDefault], ACanvas.Font, AColor);
        ACanvas.Brush.Color := AColor;
      end;

      S := GetDayOfWeekName(J, ACanvas.Font.Charset);
      cxDrawText(ACanvas, S, ATextRect, DT_WORDBREAK or DT_EDITCONTROL or DT_CENTER);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

function TcxCalendarModernMonthTableViewInfo.GetDayOfWeekName(ADay: TDay;
  AFontCharset: TFontCharset): string;
begin
  Result := FormatDateTime('ddd', Ord(ADay) + 1);
end;

{ TcxCalendarModernLargeCellTableViewInfo }

procedure TcxCalendarModernLargeCellTableViewInfo.CalculateBounds;
var
  I: Integer;
  ACellRect: TRect;
  ACellPos: TPoint;
begin
  for I := 0 to FDateCells.Count - 1 do
  begin
    ACellRect := cxRect(DateCells[I].Size);
    ACellPos := Point(Bounds.Left + DateCells[I].Column * DateCells[I].Size.cx,
      Bounds.Top + DateCells[I].Row * DateCells[I].Size.cy);
    DateCells[I].Bounds := cxRectSetOrigin(ACellRect, ACellPos);
  end;
  inherited CalculateBounds;
end;

procedure TcxCalendarModernLargeCellTableViewInfo.CalculateChildSizes;
var
  AMaxSize: TSize;
  AMinSize: TSize;
begin
  AMaxSize := (ViewInfo as TcxCalendarModernViewInfo).DateViewInfo.MaxTableSize;
  AMinSize := GetMinSize;
  FCellSize := DateCells[0].GetMinSize;
  if AMinSize.cx < AMaxSize.cx then
    FCellSize.cx := DateCells[0].GetMinSize.cx + (AMaxSize.cx - AMinSize.cx) div GetTableSize.cx;
  if AMinSize.cy < AMaxSize.cy then
    FCellSize.cy := DateCells[0].GetMinSize.cy + (AMaxSize.cy - AMinSize.cy) div GetTableSize.cy;
  if FCellSize.cy > FCellSize.cx then
    FCellSize.cy := FCellSize.cx;
  inherited CalculateChildSizes;
end;

function TcxCalendarModernLargeCellTableViewInfo.DoCalculateSize: TSize;
begin
  Result := cxSize(GetTableSize.cx * CellSize.cx,
    GetTableSize.cy * CellSize.cy);
end;

function TcxCalendarModernLargeCellTableViewInfo.GetTableSize: TSize;
begin
  Result := cxSize(4, 3);
end;

{ TcxCalendarModernYearTableViewInfo }

function TcxCalendarModernYearTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := TcxCalendarModernMonthCellViewInfo.Create(Calendar);
end;

function TcxCalendarModernYearTableViewInfo.IsDateEnabled(
  ADate: TDateTime): Boolean;
begin
  Result := cxGetLocalCalendar.GetYear(Calendar.FirstDate) = cxGetLocalCalendar.GetYear(ADate);
end;

function TcxCalendarModernYearTableViewInfo.GetDateInNextRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ASelectDate, GetTableSize.cx);
end;

function TcxCalendarModernYearTableViewInfo.GetDateInPreviousRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ASelectDate, -GetTableSize.cx);
end;

function TcxCalendarModernYearTableViewInfo.GetFirstDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeMonth(ASelectDate, 1);
end;

function TcxCalendarModernYearTableViewInfo.GetFirstDateInRow(
  ASelectDate: TDateTime): TDateTime;
var
  AMonth: Word;
begin
  AMonth := dxGetDateElement(ASelectDate, deMonth);
  AMonth := (AMonth - 1) div GetTableSize.cx * GetTableSize.cx + 1;
 // AMonth := AMonth + 1 - AMonth mod (GetTableSize.cx + 1);
  Result := dxChangeMonth(ASelectDate, AMonth);
end;

function TcxCalendarModernYearTableViewInfo.GetLastDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeMonth(ASelectDate, 12);
end;

function TcxCalendarModernYearTableViewInfo.GetLastDateInRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := GetFirstDateInRow(ASelectDate) + 3;
end;

function TcxCalendarModernYearTableViewInfo.GetNextDate(ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ADate, 1);
end;

function TcxCalendarModernYearTableViewInfo.GetNextPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, 1);
end;

function TcxCalendarModernYearTableViewInfo.GetPreviousDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddMonths(ADate, -1);
end;

function TcxCalendarModernYearTableViewInfo.GetPreviousPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, -1);
end;

{ TcxCalendarModernDecadeTableViewInfo }

function TcxCalendarModernDecadeTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := TcxCalendarModernYearCellViewInfo.Create(Calendar);
end;

function TcxCalendarModernDecadeTableViewInfo.IsDateEnabled(
  ADate: TDateTime): Boolean;
begin
  Result := cxGetLocalCalendar.GetYear(Calendar.FirstDate) div 10 = cxGetLocalCalendar.GetYear(ADate) div 10;
end;

function TcxCalendarModernDecadeTableViewInfo.GetDateInNextRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ASelectDate, GetTableSize.cx);
end;

function TcxCalendarModernDecadeTableViewInfo.GetDateInPreviousRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ASelectDate, -GetTableSize.cx);
end;

function TcxCalendarModernDecadeTableViewInfo.GetFirstDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeYear(ASelectDate, cxGetLocalCalendar.GetYear(ASelectDate) div 10 * 10);
end;

function TcxCalendarModernDecadeTableViewInfo.GetFirstDateInRow(
  ASelectDate: TDateTime): TDateTime;
var
  AYear, AStartYear: Word;
  AIndex: Integer;
begin
  AYear := dxGetDateElement(ASelectDate, deYear);
  AStartYear := AYear div 10 * 10;
  AIndex := AYear - AStartYear + 1;
  AIndex := AIndex div GetTableSize.cx * GetTableSize.cx;
  AYear := AStartYear + AIndex - 1;
  Result := dxChangeYear(ASelectDate, AYear);
end;

function TcxCalendarModernDecadeTableViewInfo.GetLastDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeYear(ASelectDate, cxGetLocalCalendar.GetYear(ASelectDate) div 10 * 10 + 9);
end;

function TcxCalendarModernDecadeTableViewInfo.GetLastDateInRow(
  ASelectDate: TDateTime): TDateTime;
var
  AYear, AStartYear: Word;
  AIndex: Integer;
begin
  AYear := dxGetDateElement(ASelectDate, deYear);
  AStartYear := AYear div 10 * 10;
  AIndex := AYear - AStartYear + 1;
  AIndex := AIndex div GetTableSize.cx * GetTableSize.cx;
  AYear := AStartYear + AIndex - 1 + 3;
  Result := dxChangeYear(ASelectDate, AYear);
end;

function TcxCalendarModernDecadeTableViewInfo.GetNextDate(ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, 1);
end;

function TcxCalendarModernDecadeTableViewInfo.GetNextPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, 10);
end;

function TcxCalendarModernDecadeTableViewInfo.GetPreviousDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, -1);
end;

function TcxCalendarModernDecadeTableViewInfo.GetPreviousPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, -10);
end;

{ TcxCalendarModernCenturyTableViewInfo }

function TcxCalendarModernCenturyTableViewInfo.CreateDateCell: TcxCalendarDateCellViewInfo;
begin
  Result := TcxCalendarModernDecadeCellViewInfo.Create(Calendar);
end;

function TcxCalendarModernCenturyTableViewInfo.IsDateEnabled(
  ADate: TDateTime): Boolean;
begin
  Result := cxGetLocalCalendar.GetYear(Calendar.FirstDate) div 100 = cxGetLocalCalendar.GetYear(ADate) div 100;
end;

function TcxCalendarModernCenturyTableViewInfo.GetDateInNextRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ASelectDate, GetTableSize.cx * 10);
end;

function TcxCalendarModernCenturyTableViewInfo.GetDateInPreviousRow(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ASelectDate, -GetTableSize.cx * 10);
end;

function TcxCalendarModernCenturyTableViewInfo.GetFirstDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeDecade(ASelectDate, cxGetLocalCalendar.GetYear(ASelectDate) div 100 * 100);
end;

function TcxCalendarModernCenturyTableViewInfo.GetFirstDateInRow(
  ASelectDate: TDateTime): TDateTime;
var
  AYear, AStartYear, AStartDecadeYear: Word;
  AIndex: Integer;
begin
  AYear := dxGetDateElement(ASelectDate, deYear);
  AStartYear := AYear div 100 * 100;
  AStartDecadeYear := AYear div 10 * 10;
  AIndex := (AStartDecadeYear - AStartYear) div 10 + 1;
  AIndex := AIndex div GetTableSize.cx * GetTableSize.cx;
  AStartDecadeYear := AStartYear + AIndex * 10 - 10;
  Result := dxChangeDecade(ASelectDate, AStartDecadeYear);
end;

function TcxCalendarModernCenturyTableViewInfo.GetLastDate(
  ASelectDate: TDateTime): TDateTime;
begin
  Result := dxChangeDecade(ASelectDate, cxGetLocalCalendar.GetYear(ASelectDate) div 100 * 100 + 90);
end;

function TcxCalendarModernCenturyTableViewInfo.GetLastDateInRow(
  ASelectDate: TDateTime): TDateTime;
var
  AYear, AStartYear, AStartDecadeYear: Word;
  AIndex: Integer;
begin
  AYear := dxGetDateElement(ASelectDate, deYear);
  AStartYear := AYear div 100 * 100;
  AStartDecadeYear := AYear div 10 * 10;
  AIndex := (AStartDecadeYear - AStartYear) div 10 + 1;
  AIndex := AIndex div GetTableSize.cx * GetTableSize.cx;
  AStartDecadeYear := AStartYear + (AIndex + 3) * 10 - 10;
  Result := dxChangeDecade(ASelectDate, AStartDecadeYear);
end;

function TcxCalendarModernCenturyTableViewInfo.GetNextDate(ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, 10);
end;

function TcxCalendarModernCenturyTableViewInfo.GetNextPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, 100);
end;

function TcxCalendarModernCenturyTableViewInfo.GetPreviousDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, -10);
end;

function TcxCalendarModernCenturyTableViewInfo.GetPreviousPageDate(
  ADate: TDateTime): TDateTime;
begin
  Result := cxGetLocalCalendar.AddYears(ADate, -100);
end;

{ TcxCalendarHeaderCellViewInfo }

procedure TcxCalendarDateHeaderCellViewInfo.CalculateBounds;
begin
  FTextRect := cxRectCenter(Bounds, Size);
  inherited CalculateBounds;
end;

function TcxCalendarDateHeaderCellViewInfo.DoCalculateSize: TSize;
begin
  Result := cxTextSize(Font, GetText);
end;

procedure TcxCalendarDateHeaderCellViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, ABounds);
end;

function TcxCalendarDateHeaderCellViewInfo.GetDrawStates: TcxCalendarElementStates;
begin
  Result := [State];
  if not IsEnabled then
    Include(Result, cesDisabled);
end;

function TcxCalendarDateHeaderCellViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtDateHeaderText;
end;

function TcxCalendarDateHeaderCellViewInfo.GetText: string;
begin
  Result := '';
end;

function TcxCalendarDateHeaderCellViewInfo.IsEnabled: Boolean;
begin
  Result := True;
end;

procedure TcxCalendarDateHeaderCellViewInfo.Paint(ACanvas: TcxCanvas);
begin
  Painter.DrawDateHeaderCell(ACanvas, Self);
end;

{ TcxCalendarYearHeaderCellViewInfo }

function TcxCalendarYearDateHeaderCellViewInfo.GetText: string;
begin
  Result := cxGetLocalYear(Calendar.FirstDate, cxGetLocalCalendar);
end;

{ TcxCalendarMonthDateHeaderCellViewInfo }

function TcxCalendarMonthDateHeaderCellViewInfo.GetText: string;
begin
  Result := ViewInfo.GetMonthDateHeaderText;
end;

procedure TcxCalendarMonthDateHeaderCellViewInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Controller.HitTest.HitAtMonthDateHeader then
    (ViewInfo as TcxCalendarClassicViewInfo).FMonthListBox.Popup(Calendar);
end;

{ TcxCalendarModernMonthDateHeaderCellViewInfo }

function TcxCalendarModernDateHeaderCellViewInfo.CanFade: Boolean;
begin
  Result := ViewInfo.CanFade;
end;

procedure TcxCalendarModernDateHeaderCellViewInfo.Click;
begin
  inherited Click;
  (ViewInfo.FDateViewInfo as TcxCalendarModernDateViewInfo).NextViewStyle;
end;

procedure TcxCalendarModernDateHeaderCellViewInfo.DrawFadeImage(
  ACanvas: TcxCanvas);
begin
  Painter.DrawDateHeaderCell(ACanvas, Self);
end;

function TcxCalendarModernDateHeaderCellViewInfo.GetText: string;
begin
  Result := FText;
end;

procedure TcxCalendarModernDateHeaderCellViewInfo.Initialize;
begin
  FText := ViewInfo.GetMonthDateHeaderText;
  inherited Initialize;
end;

function TcxCalendarModernDateHeaderCellViewInfo.IsEnabled: Boolean;
begin
  Result := (ViewInfo.DateViewInfo as TcxCalendarModernDateViewInfo).ViewStyle <> cvdsCentury;
end;

procedure TcxCalendarModernDateHeaderCellViewInfo.Paint(ACanvas: TcxCanvas);
begin
  DrawFadePart(ACanvas);
  Painter.DrawDateHeaderCellText(ACanvas, Self);
end;

{ TcxCalendarDateHeaderViewInfo }

procedure TcxCalendarDateHeaderViewInfo.CalculateBounds;
var
  ABorderWidth: Integer;
  R, R1: TRect;
  AArrowSize: TSize;
begin
  ABorderWidth := ViewInfo.GetHeaderBorderWidth;
  AArrowSize := FArrowsViewInfo[adLeft].Size;

  R1 := cxRectSetSize(Bounds, AArrowSize);

  FArrowsViewInfo[adLeft].Bounds := cxRectOffsetVert(R1, ABorderWidth);
  FArrowsViewInfo[adRight].Bounds := cxRectOffsetHorz(FArrowsViewInfo[adLeft].Bounds, cxRectWidth(Bounds) - AArrowSize.cx);
  R := Bounds;
  R.Left := FArrowsViewInfo[adLeft].Bounds.Right;
  R.Right := FArrowsViewInfo[adRight].Bounds.Left;
  FTextCellViewInfo.Bounds := R;
  inherited CalculateBounds;
end;

function TcxCalendarDateHeaderViewInfo.CreateArrowViewInfo(
  ADirection: TcxArrowDirection): TcxCalendarArrowViewInfo;
begin
  Result := TcxCalendarArrowViewInfo.Create(Calendar, ADirection);
end;

procedure TcxCalendarDateHeaderViewInfo.CreateElements;
begin
  FArrowsViewInfo[adLeft] := CreateArrowViewInfo(adLeft);
  FArrowsViewInfo[adRight] := CreateArrowViewInfo(adRight);
  FTextCellViewInfo := CreateTextCellViewInfo;
end;

function TcxCalendarDateHeaderViewInfo.CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo;
begin
  Result := TcxCalendarMonthDateHeaderCellViewInfo.Create(Calendar);
end;

procedure TcxCalendarDateHeaderViewInfo.DestroyElements;
begin
  FreeAndNil(FTextCellViewInfo);
  FreeAndNil(FArrowsViewInfo[adRight]);
  FreeAndNil(FArrowsViewInfo[adLeft]);
end;

procedure TcxCalendarDateHeaderViewInfo.AddVisibleElements;
begin
  Add(FArrowsViewInfo[adLeft]);
  Add(FArrowsViewInfo[adRight]);
  Add(FTextCellViewInfo);
end;

{ TcxCalendarMonthDateHeaderViewInfo }

function TcxCalendarMonthDateHeaderViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtMonthHeader;
end;

{ TcxCalendarModernMonthDateHeaderViewInfo }

function TcxCalendarModernMonthDateHeaderViewInfo.CreateArrowViewInfo(
  ADirection: TcxArrowDirection): TcxCalendarArrowViewInfo;
begin
  Result := TcxCalendarModernArrowViewInfo.Create(Calendar, ADirection);
end;

function TcxCalendarModernMonthDateHeaderViewInfo.CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo;
begin
  Result := TcxCalendarModernDateHeaderCellViewInfo.Create(Calendar);
end;

{ TcxCalendarYearDateHeaderViewInfo }

function TcxCalendarYearDateHeaderViewInfo.CreateTextCellViewInfo: TcxCalendarDateHeaderCellViewInfo;
begin
  Result := TcxCalendarYearDateHeaderCellViewInfo.Create(Calendar);
end;

function TcxCalendarYearDateHeaderViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtYearHeader;
end;

{ TcxCalendarDateHeadersViewInfo }

procedure TcxCalendarDateHeadersViewInfo.AddVisibleElements;
begin
  Add(FMonthHeaderViewInfo);
end;

procedure TcxCalendarDateHeadersViewInfo.CalculateBounds;
begin
  DoCalculateBounds;
  inherited CalculateBounds;
end;

procedure TcxCalendarDateHeadersViewInfo.CreateElements;
begin
  FMonthHeaderViewInfo := CreateMonthHeaderViewInfo;
end;

function TcxCalendarDateHeadersViewInfo.CreateMonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo;
begin
  Result := TcxCalendarMonthDateHeaderViewInfo.Create(Calendar);
end;

procedure TcxCalendarDateHeadersViewInfo.DestroyElements;
begin
  FreeAndNil(FMonthHeaderViewInfo);
end;

procedure TcxCalendarDateHeadersViewInfo.DoCalculateBounds;
var
  ABounds: TRect;
  AHeaderOffset: TRect;
begin
  AHeaderOffset := Calendar.GetHeaderOffset;
  ABounds := Bounds;
  Inc(ABounds.Left, AHeaderOffset.Left);
  Dec(ABounds.Right, AHeaderOffset.Right);
  MonthHeaderViewInfo.Bounds := ABounds;
end;

function TcxCalendarDateHeadersViewInfo.DoCalculateSize: TSize;
begin
  Result := cxSize(0, ViewInfo.FHeaderHeight);
end;

{ TcxCalendarClassicDateHeadersViewInfo }

procedure TcxCalendarClassicDateHeadersViewInfo.CreateElements;
begin
  inherited CreateElements;
  FYearHeaderViewInfo := TcxCalendarYearDateHeaderViewInfo.Create(Calendar);
end;

procedure TcxCalendarClassicDateHeadersViewInfo.DestroyElements;
begin
  FreeAndNil(FYearHeaderViewInfo);
  inherited DestroyElements;
end;

procedure TcxCalendarClassicDateHeadersViewInfo.DoCalculateBounds;
var
  ABorderWidth: Integer;
  AHeaderOffset: TRect;
  R: TRect;
  AArrowSize: TSize;

  AMaxMonthNameWidth, ASpaceWidth: Integer;
  AMaxYearNameWidth: Integer;
  AMonthRegionWidth: Integer;
  ADateRegionWidth: Integer;
  ABounds: TRect;
begin
  ABorderWidth := ViewInfo.GetHeaderBorderWidth;
  AHeaderOffset := Calendar.GetHeaderOffset;
  AArrowSize := ViewInfo.GetArrowSize;
  ADateRegionWidth := cxRectWidth(Bounds);
  ABounds := Bounds;
  Inc(ABounds.Left, AHeaderOffset.Left);
  Dec(ABounds.Right, AHeaderOffset.Right);
  if IsYearHeaderVisible then
  begin
    CalculateMaxYearAndMonthNameLengths(AMaxYearNameWidth, AMaxMonthNameWidth);
    ASpaceWidth := ADateRegionWidth - ABorderWidth * 2 - AHeaderOffset.Left -
      AHeaderOffset.Right - AMaxMonthNameWidth - AMaxYearNameWidth - 4 * AArrowSize.cx;
    AMonthRegionWidth := AMaxMonthNameWidth + ASpaceWidth * AMaxMonthNameWidth div
      (AMaxMonthNameWidth + AMaxYearNameWidth);

    R := ABounds;
    Inc(R.Left, ABorderWidth);
    R.Right := R.Left + AMonthRegionWidth + AArrowSize.cx * 2;
    MonthHeaderViewInfo.Bounds := R;

    R := ABounds;
    R.Left := MonthHeaderViewInfo.Bounds.Right;
    Dec(R.Right, ABorderWidth);
    YearHeaderViewInfo.Bounds := R;
  end
  else
    MonthHeaderViewInfo.Bounds := ABounds;
end;

function TcxCalendarClassicDateHeadersViewInfo.IsYearHeaderVisible: Boolean;
begin
  Result := ViewInfo.IsYearDateHeaderVisible;
end;

procedure TcxCalendarClassicDateHeadersViewInfo.Paint(ACanvas: TcxCanvas);
const
  HeaderBorders: array[Boolean] of TcxBorders = ([bBottom], cxBordersAll);
var
  AIsTransparent: Boolean;
  ASkinPainter: TcxCustomLookAndFeelPainter;
begin
  AIsTransparent := ViewInfo.IsDateHeaderTransparent;
  ASkinPainter := Calendar.LookAndFeel.SkinPainter;
  if ASkinPainter <> nil then
  begin
    ASkinPainter.DrawScaledHeader(ACanvas, Bounds, cxEmptyRect, [],
      HeaderBorders[ViewInfo.Kind = ckDateTime], cxbsNormal, taCenter, vaCenter, False,
      False, '', Font, 0, 0, Calendar.ScaleFactor);
  end
  else
    if LookAndFeelPainter.LookAndFeelStyle = lfsNative then
      DrawThemeBackground(OpenTheme(totHeader), ACanvas.Handle, HP_HEADERITEMLEFT, HIS_NORMAL, Bounds)
    else
      if not AIsTransparent then
        ACanvas.FillRect(Bounds, Calendar.GetHeaderColor);

  inherited Paint(ACanvas);

  ACanvas.Font.Color  := LookAndFeelPainter.DefaultHeaderTextColor;
  ACanvas.Brush.Color := Calendar.GetHeaderColor;

  if not AIsTransparent then
    if not Calendar.FFlat then
      ACanvas.DrawEdge(Bounds, False, False, cxBordersAll)
    else
      if ViewInfo.Kind = ckDateTime then
        ACanvas.FrameRect(Bounds, Calendar.GetDateTimeHeaderFrameColor)
      else
        if LookAndFeelPainter.LookAndFeelStyle = lfsNative then
          ACanvas.FrameRect(Rect(Bounds.Left, Bounds.Top - Office11HeaderOffset,
            Bounds.Right, Bounds.Bottom + Office11HeaderOffset - 1),
            Calendar.BackgroundColor, Office11HeaderOffset)
        else
          InternalPolyLine(ACanvas, [Point(Bounds.Left, Bounds.Bottom - 1),
            Point(Bounds.Right - 1, Bounds.Bottom - 1)],
            Calendar.GetDateHeaderFrameColor, True);
  ACanvas.ExcludeClipRect(Bounds);
end;

procedure TcxCalendarClassicDateHeadersViewInfo.AddVisibleElements;
begin
  inherited AddVisibleElements;
  if IsYearHeaderVisible then
    Add(FYearHeaderViewInfo);
end;

procedure TcxCalendarClassicDateHeadersViewInfo.CalculateMaxYearAndMonthNameLengths(
  var AMaxYearNameWidth, AMaxMonthNameWidth: Integer);
var
  I: Integer;
  AMonthNameWidth: Integer;
  AYearNameWidth: Integer;
  AConvertDate: TcxDateTime;
begin
  AMaxMonthNameWidth := 0;
  AMaxYearNameWidth := 0;
  AConvertDate := Calendar.CalendarTable.FromDateTime(Calendar.FirstDate);
  cxScreenCanvas.Font := Font;
  try
    for I := 1 to cxGetLocalCalendar.GetMonthsInYear(AConvertDate.Era, AConvertDate.Year) do
    begin
      AMonthNameWidth := cxScreenCanvas.TextWidth(cxGetLocalMonthName(cxGetLocalCalendar.AddMonths(Calendar.FirstDate, I), cxGetLocalCalendar));
      if AMonthNameWidth > AMaxMonthNameWidth then
        AMaxMonthNameWidth := AMonthNameWidth;
      AYearNameWidth := cxScreenCanvas.TextWidth(cxGetLocalYear(cxGetLocalCalendar.AddYears(Calendar.FirstDate, I), cxGetLocalCalendar));
      AMaxYearNameWidth := Max(AMaxYearNameWidth, AYearNameWidth);
    end;
  finally
    cxScreenCanvas.Dormant;
  end;
end;

{ TcxCalendarModernDateHeadersViewInfo }

function TcxCalendarModernDateHeadersViewInfo.CreateMonthHeaderViewInfo: TcxCalendarMonthDateHeaderViewInfo;
begin
  Result := TcxCalendarModernMonthDateHeaderViewInfo.Create(Calendar);
end;

{ TcxCalendarDateViewInfo }

procedure TcxCalendarDateViewInfo.ChangeFirstDate(Value: Double;
  AAnimated: Boolean);
begin
  Calendar.InternalSetFirstDate(Value);
  SynchronizeSelectDate;
  Calendar.Calculate;
  Calendar.Invalidate;
end;

procedure TcxCalendarDateViewInfo.ChangeSelectDate(AValue: Double;
  ARepositionVisibleDates, AAnimated: Boolean);
begin
  Calendar.InternalSetSelectDate(AValue);
  if ARepositionVisibleDates and (AValue <> NullDate) then
    Calendar.FirstDate := dxDateOf(AValue);
  if ViewInfo.IsTimeVisible then
    Calendar.Repaint
  else
    Invalidate;
end;

procedure TcxCalendarDateViewInfo.AddVisibleElements;
begin
  Add(FMonthTableViewInfo);
  Add(FHeadersViewInfo);
end;

procedure TcxCalendarDateViewInfo.CalculateBounds;
begin
  CalculateHeadersBounds;
  CalculateTableBounds;
  inherited CalculateBounds;
end;

procedure TcxCalendarDateViewInfo.CalculateHeadersBounds;
var
  R: TRect;
begin
  R := Bounds;
  R.Bottom := R.Top + FHeadersViewInfo.Size.cy;
  R.Right := R.Left + Size.cx;
  HeadersViewInfo.Bounds := R;
end;

procedure TcxCalendarDateViewInfo.CalculateTableBounds;
var
  R: TRect;
begin
  R := Bounds;
  R.TopLeft := cxPoint(Bounds.Left + GetTableOffsets.Left, FHeadersViewInfo.Bounds.Bottom + GetTableOffsets.Top);
  FMonthTableViewInfo.Bounds := cxRectSetSize(R, FMonthTableViewInfo.GetMinSize);
end;

procedure TcxCalendarDateViewInfo.CreateElements;
begin
  FHeadersViewInfo := TcxCalendarClassicDateHeadersViewInfo.Create(Calendar);
  FMonthTableViewInfo := TcxCalendarMonthTableViewInfo.Create(Calendar);
end;

procedure TcxCalendarDateViewInfo.DestroyElements;
begin
  FreeAndNil(FMonthTableViewInfo);
  FreeAndNil(FHeadersViewInfo);
end;

function TcxCalendarDateViewInfo.DoCalculateSize: TSize;
begin
  Result.cx := GetTableOffsets.Left + GetTableOffsets.Right +
    FMonthTableViewInfo.GetMinSize.cx;
  Result.cy := FHeadersViewInfo.Size.cy + GetTableOffsets.Top + GetTableOffsets.Bottom +
    FMonthTableViewInfo.GetMinSize.cy;
end;

function TcxCalendarDateViewInfo.GetTableOffsets: TRect;
begin
  Result := cxRect(FSideWidth, 1, FSideWidth, 0);
end;

procedure TcxCalendarDateViewInfo.Initialize;
begin
  inherited;
  FSideWidth := 2 * cxTextWidth(Font, '0');
end;

procedure TcxCalendarDateViewInfo.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  FMonthTableViewInfo.KeyDown(AKey, AShift);
end;

procedure TcxCalendarDateViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited;
end;

procedure TcxCalendarDateViewInfo.SynchronizeFirstDate;
begin
  Calendar.InternalSetFirstDate(dxChangeDay(Calendar.SelectDate, 1));
end;

procedure TcxCalendarDateViewInfo.SynchronizeSelectDate;
begin
end;

{ TcxCalendarAnimatedImageInfo }

constructor TcxCalendarAnimatedImageInfo.Create;
begin
  inherited Create;
  FImage := TdxSmartImage.Create;
end;

destructor TcxCalendarAnimatedImageInfo.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TcxCalendarAnimatedImageInfo.Calculate(APosition: Integer);
var
  AScale: TdxSizeF;
  AOffset: TdxPointF;
  ASize: TSize;
begin
  AScale := dxSizeF(
    StartState.Scale.cx  + FScaleCoefficient.cx * APosition,
    StartState.Scale.cy  + FScaleCoefficient.cy * APosition);
  AOffset := dxPointF(
    StartState.Offset.X + FOffsetCoefficient.X * APosition,
    StartState.Offset.Y + FOffsetCoefficient.Y * APosition);
  ASize := cxSize(DestBounds);
  ASize := cxSize(Trunc(ASize.cx * AScale.cx), Trunc(ASize.cy * AScale.cy));

  FCurrentDestBounds := cxRectOffset(DestBounds, cxPoint(AOffset));
  FCurrentDestBounds := cxRectCenter(FCurrentDestBounds, ASize);
  FCurrentAlphaValue := Trunc(StartState.Alpha + FAlphaCoefficient * APosition);
end;

procedure TcxCalendarAnimatedImageInfo.Initialize(ALength: Integer);
begin
  FScaleCoefficient := dxSizeF(
    (EndState.Scale.cx - StartState.Scale.cx) / ALength,
    (EndState.Scale.cy - StartState.Scale.cy) / ALength);
  FOffsetCoefficient := dxPointF(
    (EndState.Offset.X - StartState.Offset.X) / ALength,
    (EndState.Offset.Y - StartState.Offset.Y) / ALength);
  FAlphaCoefficient := (EndState.Alpha - StartState.Alpha) / ALength;
end;

procedure TcxCalendarAnimatedImageInfo.Paint(AGpCanvas: TdxGPCanvas);
begin
  AGpCanvas.SaveClipRegion;
  AGpCanvas.SetClipRect(FClipRect, gmIntersect);
  AGpCanvas.Draw(FImage, FCurrentDestBounds, SourceBounds, FCurrentAlphaValue);
  AGpCanvas.RestoreClipRegion;
end;

procedure TcxCalendarAnimatedImageInfo.SetImage(const Value: TdxSmartImage);
begin
  FImage.Assign(Value);
end;

{ TcxCalendarModernDateViewAnimationController }

constructor TcxCalendarModernDateViewAnimationController.Create(
  ADateViewInfo: TcxCalendarModernDateViewInfo);
begin
  inherited Create;
  FDateViewInfo := ADateViewInfo;
  FImageInfos := TdxFastObjectList.Create;
end;

destructor TcxCalendarModernDateViewAnimationController.Destroy;
begin
  FreeAndNil(FImageInfos);
  FreeAndNil(FAnimation);
  inherited Destroy;
end;

function TcxCalendarModernDateViewAnimationController.AddImage: TcxCalendarAnimatedImageInfo;
begin
  Result := TcxCalendarAnimatedImageInfo.Create;
  FImageInfos.Add(Result);
end;

procedure TcxCalendarModernDateViewAnimationController.ClearImages;
begin
  FImageInfos.Clear;
end;

procedure TcxCalendarModernDateViewAnimationController.DrawImages(
  AGpCanvas: TdxGPCanvas);
var
  I: Integer;
begin
  for I := 0 to FImageInfos.Count -1 do
    ImageInfo[I].Paint(AGpCanvas);
end;

function TcxCalendarModernDateViewAnimationController.IsActive: Boolean;
begin
  Result := FIsActive;
end;

procedure TcxCalendarModernDateViewAnimationController.Start;
var
  ALength: Integer;
  I: Integer;
begin
  FreeAndNil(FAnimation);
  ALength := 255;
  FAnimation := TdxAnimationTransition.Create(255, ateLinear, ALength);
  FAnimation.FreeOnTerminate := False;
  FAnimation.OnAnimate := DoOnAnimation;
  for I := 0 to FImageInfos.Count -1 do
    ImageInfo[I].Initialize(ALength);
  FIsActive := True;
  try
    FAnimation.ImmediateAnimation;
  finally
    FIsActive := False;
  end;
end;

procedure TcxCalendarModernDateViewAnimationController.DoOnAnimation(
  Sender: TdxAnimationTransition; var APosition: Integer;
  var AFinished: Boolean);
var
  I: Integer;
begin
  for I := 0 to FImageInfos.Count -1 do
    ImageInfo[I].Calculate(APosition);
  FDateViewInfo.Invalidate;
  FDateViewInfo.Calendar.Update;
end;

function TcxCalendarModernDateViewAnimationController.GetImageInfo(
  AIndex: Integer): TcxCalendarAnimatedImageInfo;
begin
  Result := FImageInfos[AIndex] as TcxCalendarAnimatedImageInfo;
end;

{ TcxCalendarModernDateViewInfo }

constructor TcxCalendarModernDateViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited;
  FAnimationController := TcxCalendarModernDateViewAnimationController.Create(Self);
end;

destructor TcxCalendarModernDateViewInfo.Destroy;
begin
  FreeAndNil(FStartImage);
  FreeAndNil(FEndImage);
  FreeAndNil(FAnimationController);
  inherited;
end;

procedure TcxCalendarModernDateViewInfo.ChangeFirstDate(Value: Double;
  AAnimated: Boolean);

  procedure InternalChangeFirstDate(AValue: Double);
  begin
    Calendar.InternalSetFirstDate(AValue);
    SynchronizeSelectDate;
  end;

begin
  if AAnimated then
  begin
    if GetCurrentTableViewInfo.IsDateEnabled(Value) then
      InternalChangeFirstDate(Value)
    else
    begin
      FDirection := GetHorizontalAnimationDirection(Value < Calendar.FirstDate);
      RecreateImage(FStartImage);
      InternalChangeFirstDate(Value);
      Recalculate;
      RecreateImage(FEndImage);
      DoChangeDateAnimation;
    end;
    Invalidate;
  end
  else
    inherited ChangeFirstDate(Value, AAnimated);
end;

procedure TcxCalendarModernDateViewInfo.ChangeViewStyle(
  AValue: TcxCalendarModernDateViewStyle; AAnimated: Boolean);
begin
  if (AValue <> FViewStyle) and
    (AValue >= Low(FViewStyle)) and (AValue <= High(FViewStyle)) then
  begin
    if not AAnimated then
      InternalSetViewStyle(AValue)
    else
    begin
      RecreateImage(FStartImage);
      InternalSetViewStyle(AValue);
      RecreateImage(FEndImage);
      DoChangeViewStyleAnimation;
    end;
    Invalidate;
  end;
end;

procedure TcxCalendarModernDateViewInfo.ChangeSelectDate(AValue: Double;
  ARepositionVisibleDates: Boolean; AAnimated: Boolean);

  procedure InternalChangeSelectDate(AValue: Double);
  begin
    Calendar.InternalSetSelectDate(AValue);
    SynchronizeFirstDate;
  end;

begin
  if ARepositionVisibleDates and AAnimated then
  begin
    if GetCurrentTableViewInfo.IsDateEnabled(AValue) then
      InternalChangeSelectDate(AValue)
    else
    begin
      FDirection := GetHorizontalAnimationDirection(AValue < Calendar.SelectDate);
      RecreateImage(FStartImage);
      InternalChangeSelectDate(AValue);
      Recalculate;
      RecreateImage(FEndImage);
      DoChangeDateAnimation;
    end;
    Invalidate;
  end
  else
    inherited ChangeSelectDate(AValue, ARepositionVisibleDates, AAnimated);
end;

procedure TcxCalendarModernDateViewInfo.AddVisibleElements;
begin
  Add(GetCurrentTableViewInfo);
  Add(FHeadersViewInfo);
end;

procedure TcxCalendarModernDateViewInfo.CalculateChildSizes;
begin
  FMaxTableSize.cx := Max(FMonthTableViewInfo.GetMinSize.cx,
    Max(FYearTableViewInfo.GetMinSize.cx, Max(FDecadeTableViewInfo.GetMinSize.cx, FCenturyTableViewInfo.GetMinSize.cx)));
  FMaxTableSize.cy := Max(FMonthTableViewInfo.GetMinSize.cy,
    Max(FYearTableViewInfo.GetMinSize.cy, Max(FDecadeTableViewInfo.GetMinSize.cy, FCenturyTableViewInfo.GetMinSize.cy)));
  inherited CalculateChildSizes;
end;

procedure TcxCalendarModernDateViewInfo.CalculateTableBounds;
var
  R: TRect;
begin
  R := Bounds;
  R.TopLeft := cxPoint(Bounds.Left + GetTableOffsets.Left, FHeadersViewInfo.Bounds.Bottom + GetTableOffsets.Top);
  FTableAreaBounds := R;
  FMonthTableViewInfo.Bounds := cxRectCenter(R, FMonthTableViewInfo.Size);
  FYearTableViewInfo.Bounds := cxRectCenter(R, FYearTableViewInfo.Size);
  FDecadeTableViewInfo.Bounds := cxRectCenter(R, FDecadeTableViewInfo.Size);
  FCenturyTableViewInfo.Bounds := cxRectCenter(R, FCenturyTableViewInfo.Size);
end;

procedure TcxCalendarModernDateViewInfo.SynchronizeSelectDate;
var
  ASelectDate: TcxDateTime;
begin
  ASelectDate := cxGetLocalCalendar.FromDateTime(Calendar.SelectDate);
  Calendar.InternalSetSelectDate(dxChangeDay(Calendar.FirstDate, ASelectDate.Day));
end;

procedure TcxCalendarModernDateViewInfo.CreateElements;
begin
  FHeadersViewInfo := TcxCalendarModernDateHeadersViewInfo.Create(Calendar);
  FMonthTableViewInfo := TcxCalendarModernMonthTableViewInfo.Create(Calendar);
  FDecadeTableViewInfo := TcxCalendarModernDecadeTableViewInfo.Create(Calendar);
  FYearTableViewInfo := TcxCalendarModernYearTableViewInfo.Create(Calendar);
  FCenturyTableViewInfo := TcxCalendarModernCenturyTableViewInfo.Create(Calendar);
end;

procedure TcxCalendarModernDateViewInfo.DestroyElements;
begin
  FreeAndNil(FCenturyTableViewInfo);
  FreeAndNil(FYearTableViewInfo);
  FreeAndNil(FDecadeTableViewInfo);
  inherited DestroyElements;
end;

function TcxCalendarModernDateViewInfo.DoCalculateSize: TSize;
begin
  Result.cx := GetTableOffsets.Left + GetTableOffsets.Right + FMaxTableSize.cx;
  Result.cy := FHeadersViewInfo.Size.cy + GetTableOffsets.Top + GetTableOffsets.Bottom +
    FMaxTableSize.cy;
end;

function TcxCalendarModernDateViewInfo.GetHorizontalAnimationDirection(ARight: Boolean): TcxDirection;
begin
  if ARight then
    Result := dirRight
  else
    Result := dirLeft;
  if IsRightToLeftConverted then
     Result := TdxRightToLeftLayoutConverter.ConvertDirection(Result);
end;

function TcxCalendarModernDateViewInfo.GetTableOffsets: TRect;
begin
  Result := cxRect(0, 7, 0, 0);
end;

procedure TcxCalendarModernDateViewInfo.KeyDown(var AKey: Word;
  AShift: TShiftState);
begin
  if (AShift = [ssCtrl]) and (AKey in [VK_UP, VK_DOWN]) then
    if AKey = VK_UP then
      NextViewStyle
    else
      PreviousViewStyle
  else
    if AKey in [VK_SPACE, VK_RETURN] then
      PreviousViewStyle
    else
      GetCurrentTableViewInfo.KeyDown(AKey, AShift);
end;

procedure TcxCalendarModernDateViewInfo.Paint(ACanvas: TcxCanvas);
var
  AGpCanvas: TdxGPGraphics;
begin
{$ifdef Colorized}
  ACanvas.FillRect(Bounds, clBlue);
{$endif}
  if FAnimationController.IsActive then
  begin
    AGPCanvas := dxGpBeginPaint(ACanvas.Handle, Bounds);
    try
      AGPCanvas.SmoothingMode := smAntiAlias;
      FAnimationController.DrawImages(AGpCanvas);
    finally
      dxGpEndPaint(AGPCanvas);
    end;
    ACanvas.ExcludeClipRect(GetAnimationBounds);
    ACanvas.ExcludeClipRect(HeadersViewInfo.MonthHeaderViewInfo.TextCellViewInfo.Bounds);
    FHeadersViewInfo.Paint(ACanvas);
  end
  else
    inherited;
end;

procedure TcxCalendarModernDateViewInfo.NextViewStyle;
begin
  ViewStyle := Succ(FViewStyle);
end;

procedure TcxCalendarModernDateViewInfo.PreviousViewStyle;
begin
  ViewStyle := Pred(FViewStyle);
end;

procedure TcxCalendarModernDateViewInfo.DoChangeDateAnimation;
begin
  FAnimationController.ClearImages;
  AddNextDateAnimationImages;
  AddHeaderAnimationImages;
  FAnimationController.Start;
end;

procedure TcxCalendarModernDateViewInfo.DoChangeViewStyleAnimation;
begin
  FAnimationController.ClearImages;
  AddNextViewAnimationImages;
  AddHeaderAnimationImages;
  FAnimationController.Start;
end;

procedure TcxCalendarModernDateViewInfo.InternalSetViewStyle(AValue: TcxCalendarModernDateViewStyle);
begin
  if AValue > FViewStyle then
    FDirection := dirUp
  else
    FDirection := dirDown;
  FPrevSelectCellBounds := GetCurrentTableViewInfo.GetSelectedCell.Bounds;
  FViewStyle := AValue;
  SynchronizeFirstDate;
  Recalculate;
  FNextSelectCellBounds := GetCurrentTableViewInfo.GetSelectedCell.Bounds;
end;

procedure TcxCalendarModernDateViewInfo.Recalculate;
begin
  Initialize;
  CalculateSize;
  CalculateBounds;
  if Calendar.UseRightToLeftAlignment then
    RightToLeftConversion(Bounds);
end;

procedure TcxCalendarModernDateViewInfo.RecreateImage(var AImage: TdxSmartImage);
var
  ABitmap: TcxBitmap;
  ACanvas: TcxCanvas;
begin
  ABitmap := TcxBitmap.CreateSize(Bounds);
  try
    ACanvas := ABitmap.cxCanvas;
    ACanvas.WindowOrg := Bounds.TopLeft;
    ViewInfo.Paint(ACanvas);
    FreeAndNil(AImage);
    AImage := TdxSmartImage.CreateFromBitmap(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TcxCalendarModernDateViewInfo.AddHeaderAnimationImages;
var
  ASourceRect, ADestRect: TRect;
  AImageInfo: TcxCalendarAnimatedImageInfo;
  AState: TcxCalendarAnimatedImageState;
begin
  ADestRect := HeadersViewInfo.MonthHeaderViewInfo.FTextCellViewInfo.Bounds;
  ASourceRect := cxRectOffset(ADestRect, Bounds.TopLeft, False);

  AState.Scale := dxSizeF(1, 1);
  AState.Offset := dxPointF(0, 0);

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.Image := FStartImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := ADestRect;
  AState.Alpha := 255;
  AImageInfo.StartState := AState;
  AState.Alpha := 0;
  AImageInfo.EndState := AState;

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.Image := FEndImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := ADestRect;
  AState.Alpha := 0;
  AImageInfo.StartState := AState;
  AState.Alpha := 255;
  AImageInfo.EndState := AState;
end;

procedure TcxCalendarModernDateViewInfo.AddNextDateAnimationImages;
var
  ASourceRect, ADestRect: TRect;
  AImageInfo: TcxCalendarAnimatedImageInfo;
  AState: TcxCalendarAnimatedImageState;
  AHorzOffset: Integer;
begin
  ADestRect := FTableAreaBounds;
  ASourceRect := cxRectOffset(ADestRect, Bounds.TopLeft, False);
  AHorzOffset := cxRectWidth(ADestRect);

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.Image := FStartImage;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := ADestRect;

  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  AState.Offset := dxPointF(0, 0);
  AImageInfo.StartState := AState;
  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  if FDirection = dirRight then
    AState.Offset := dxPointF(AHorzOffset, 0)
  else
    AState.Offset := dxPointF(-AHorzOffset, 0);
  AImageInfo.EndState := AState;

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.Image := FEndImage;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := ADestRect;

  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  if FDirection = dirRight then
    AState.Offset := dxPointF(-AHorzOffset, 0)
  else
    AState.Offset := dxPointF(AHorzOffset, 0);
  AImageInfo.StartState := AState;
  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  AState.Offset := dxPointF(0, 0);
  AImageInfo.EndState := AState;
end;

procedure TcxCalendarModernDateViewInfo.AddNextViewAnimationImages;

  function GetRectCenterOffset(const ARect1, ARect2: TRect): TPoint;
  var
    APos1, APos2: TPoint;
  begin
    APos1 := cxRectCenter(ARect1);
    APos2 := cxRectCenter(ARect2);
    Result := cxPoint(APos2.X - APos1.X, APos2.Y - APos1.Y);
  end;

var
  AImageInfo: TcxCalendarAnimatedImageInfo;
  AState: TcxCalendarAnimatedImageState;
  AOffset: TPoint;
  AScale: TdxSizeF;
  ASourceRect, ADestRect: TRect;
begin
  ADestRect := FTableAreaBounds;
  ASourceRect := cxRectOffset(ADestRect, Bounds.TopLeft, False);

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.Image := FStartImage;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := ADestRect;

  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  AState.Offset := dxPointF(0, 0);
  AImageInfo.StartState := AState;
  AState.Alpha := 0;
  if FDirection = dirUp then
  begin
    AState.Scale := dxSizeF(cxRectWidth(FNextSelectCellBounds)/cxRectWidth(FTableAreaBounds),
      cxRectHeight(FNextSelectCellBounds)/cxRectHeight(FTableAreaBounds));
    AState.Offset := dxPointF(GetRectCenterOffset(FTableAreaBounds, FNextSelectCellBounds));
  end
  else
  begin
    AScale := dxSizeF(cxRectWidth(FTableAreaBounds)/cxRectWidth(FPrevSelectCellBounds),
      cxRectHeight(FTableAreaBounds)/cxRectHeight(FPrevSelectCellBounds));
    AState.Scale := AScale;
    AOffset := GetRectCenterOffset(FPrevSelectCellBounds, FTableAreaBounds);
    AState.Offset := dxPointF(AOffset.X * AScale.cx, AOffset.Y * AScale.cy);
  end;
  AImageInfo.EndState := AState;

  AImageInfo := FAnimationController.AddImage;
  AImageInfo.Image := FEndImage;
  AImageInfo.ClipRect := ADestRect;
  AImageInfo.SourceBounds := ASourceRect;
  AImageInfo.DestBounds := FTableAreaBounds;

  AState.Alpha := 0;
  if FDirection = dirUp then
  begin
    AState.Scale := dxSizeF(cxRectWidth(FTableAreaBounds)/cxRectWidth(FNextSelectCellBounds),
      cxRectHeight(FTableAreaBounds)/cxRectHeight(FNextSelectCellBounds));
    AOffset := GetRectCenterOffset(FNextSelectCellBounds, FTableAreaBounds);
    AState.Offset := dxPointF(AOffset.X * AState.Scale.cx, AOffset.Y * AState.Scale.cy);
  end
  else
  begin
    AState.Scale := dxSizeF(cxRectWidth(FPrevSelectCellBounds)/cxRectWidth(FTableAreaBounds),
      cxRectHeight(FPrevSelectCellBounds)/cxRectHeight(FTableAreaBounds));
    AState.Offset := dxPointF(GetRectCenterOffset(FTableAreaBounds, FPrevSelectCellBounds));
  end;
  AImageInfo.StartState := AState;
  AState.Alpha := 255;
  AState.Scale := dxSizeF(1, 1);
  AState.Offset := dxPointF(0, 0);
  AImageInfo.EndState := AState;
end;

function TcxCalendarModernDateViewInfo.GetAnimationBounds: TRect;
begin
  Result := FTableAreaBounds;
end;

function TcxCalendarModernDateViewInfo.GetCurrentTableViewInfo: TcxCalendarDateTableViewInfo;
begin
  Result := GetTableViewInfo(FViewStyle);
end;

function TcxCalendarModernDateViewInfo.GetTableViewInfo(AViewStyle: TcxCalendarModernDateViewStyle): TcxCalendarDateTableViewInfo;
begin
  case AViewStyle of
    cvdsMonth:
      Result := FMonthTableViewInfo;
    cvdsYear:
      Result := FYearTableViewInfo;
    cvdsDecade:
      Result := FDecadeTableViewInfo;
  else
    // cvdsCentury
    Result := FCenturyTableViewInfo;
  end;
end;

procedure TcxCalendarModernDateViewInfo.SetViewStyle(
  Value: TcxCalendarModernDateViewStyle);
begin
  ChangeViewStyle(Value, Calendar.CanAnimate);
end;

{ TcxCalendarHeaderViewInfo }

procedure TcxCalendarHeaderViewInfo.CalculateBounds;
begin
  FTextRect := cxRectCenter(Bounds, Size);
  inherited CalculateBounds;
end;

function TcxCalendarHeaderViewInfo.DoCalculateSize: TSize;
begin
  Result := cxTextSize(Font, GetText);
end;

procedure TcxCalendarHeaderViewInfo.DrawFadeImage(ACanvas: TcxCanvas);
begin
  Painter.DrawHeader(ACanvas, Self);
end;

function TcxCalendarHeaderViewInfo.GetText: string;
begin
  Result := cxDateToLocalFormatStr(Calendar.SelectDate +
    cxSign(Calendar.SelectDate) * Calendar.Time);
end;

procedure TcxCalendarHeaderViewInfo.Paint(ACanvas: TcxCanvas);
begin
  DrawFadePart(ACanvas);
  Painter.DrawHeaderText(ACanvas, Self);
end;

{ TcxCalendarModernHeaderViewInfo }

procedure TcxCalendarModernHeaderViewInfo.CalculateBounds;
begin
  inherited CalculateBounds;
  FTextOffsets := LookAndFeelPainter.GetModernCalendarHeaderTextOffsets;
end;

function TcxCalendarModernHeaderViewInfo.CanFade: Boolean;
begin
  Result := ViewInfo.CanFade;
end;

procedure TcxCalendarModernHeaderViewInfo.Click;
begin
  inherited Click;
  if dxGetMonthNumber(Calendar.SelectDate) <> dxGetMonthNumber(Date) then
    (ViewInfo.DateViewInfo as TcxCalendarModernDateViewInfo).ChangeViewStyle(cvdsMonth, False);
  ViewInfo.SetSelectDate(Date, True,
    Calendar.CanAnimate and ((ViewInfo.DateViewInfo as TcxCalendarModernDateViewInfo).ViewStyle = cvdsMonth));
  (ViewInfo.DateViewInfo as TcxCalendarModernDateViewInfo).ViewStyle := cvdsMonth;
end;

function TcxCalendarModernHeaderViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtHeader;
end;

function TcxCalendarModernHeaderViewInfo.GetText: string;
begin
  Result := DateToLongDateStr(Date);
end;

{ TcxCalendarClockViewInfo }

constructor TcxCalendarClockViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create(ACalendar);
  FClock := TcxClock.Create(nil);
  FClock.TabStop := False;
  FClock.Visible := False;
  FClock.LookAndFeel.MasterLookAndFeel := Calendar.LookAndFeel;
end;

destructor TcxCalendarClockViewInfo.Destroy;
begin
  FreeAndNil(FClock);
  inherited;
end;

function TcxCalendarClockViewInfo.DoCalculateSize: TSize;
begin
  Result := FClockSize;
end;

procedure TcxCalendarClockViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FClock.BoundsRect := TdxRightToLeftLayoutConverter.ConvertRect(FClock.BoundsRect, ABounds);
end;

procedure TcxCalendarClockViewInfo.CalculateBounds;
begin
  FClock.BoundsRect := Bounds;
  inherited;
end;

procedure TcxCalendarClockViewInfo.ClearVisibleElements;
begin
  FIsRightToLeftConverted := False;
  FClock.Visible := False;
end;

procedure TcxCalendarClockViewInfo.InitControls;
begin
  inherited InitControls;
  FClock.Parent := Calendar;
end;

procedure TcxCalendarClockViewInfo.InitializeVisibleElements;
begin
  FClock.BiDiMode := Calendar.BiDiMode;
  FClock.Visible := True;
end;

{ TcxCalendarTimeEditViewInfo }

constructor TcxCalendarTimeEditViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create(ACalendar);
  FTimeEdit := TcxTimeEdit.Create(nil);
  FTimeEdit.Visible := False;
  TcxEditAccess(FTimeEdit).SupportsTouchMode := True;
  FTimeEdit.ActiveProperties.Circular := True;
  FTimeEdit.Style.LookAndFeel.MasterLookAndFeel := Calendar.LookAndFeel;
end;

destructor TcxCalendarTimeEditViewInfo.Destroy;
begin
  FreeAndNil(FTimeEdit);
  inherited;
end;

procedure TcxCalendarTimeEditViewInfo.CalculateBounds;
begin
  FTimeEdit.BoundsRect := Bounds;
  FTimeEdit.SelStart := 0;
  inherited;
end;

procedure TcxCalendarTimeEditViewInfo.ClearVisibleElements;
begin
  FTimeEdit.ActiveProperties.OnChange := nil;
  FTimeEdit.Visible := False;
  FIsRightToLeftConverted := False;
end;

function TcxCalendarTimeEditViewInfo.DoCalculateSize: TSize;
begin
  Result.cx := GetTimeEditWidth;
  Result.cy := FTimeEdit.Height;
end;

procedure TcxCalendarTimeEditViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FTimeEdit.BoundsRect := TdxRightToLeftLayoutConverter.ConvertRect(FTimeEdit.BoundsRect, ABounds);
end;

procedure TcxCalendarTimeEditViewInfo.InitializeVisibleElements;
begin
  FTimeEdit.BiDiMode := Calendar.BiDiMode;
  FTimeEdit.Visible := True;
  FTimeEdit.TabStop := True;
  FTimeEdit.TabOrder := 0;
  FTimeEdit.ActiveProperties.TimeFormat := Calendar.TimeFormat;
  FTimeEdit.ActiveProperties.Use24HourFormat := Calendar.Use24HourFormat;
  FTimeEdit.ActiveProperties.OnChange := ViewInfo.TimeChanged;
  FTimeEdit.Time := Calendar.Time;
end;

function TcxCalendarTimeEditViewInfo.GetTimeEditWidth: Integer;
var
  AEditSizeProperties: TcxEditSizeProperties;
begin
  AEditSizeProperties := cxDefaultEditSizeProperties;
  AEditSizeProperties.MaxLineCount := 1;
  cxScreenCanvas.Font := Font;
  Result := FTimeEdit.ActiveProperties.GetEditSize(cxScreenCanvas, FTimeEdit.Style,
    True, 0, AEditSizeProperties).cx + cxScreenCanvas.TextWidth('0');
  cxScreenCanvas.Dormant;
end;

procedure TcxCalendarTimeEditViewInfo.InitControls;
begin
  inherited InitControls;
  FTimeEdit.Parent := Calendar;
end;

{ TcxCalendarTimeViewInfo }

procedure TcxCalendarTimeViewInfo.AddVisibleElements;
begin
  Add(FTimeEditInfo);
  Add(FClockInfo);
end;

procedure TcxCalendarTimeViewInfo.CalculateChildSizes;
var
  AButtonVOffset: Integer;
  AClockSize: Integer;
begin
  FTimeEditInfo.CalculateSize;
  AButtonVOffset := (ViewInfo.FButtonsRegionHeight - 1 - ViewInfo.FButtonsHeight) div 2;
  AClockSize := Height - FTimeEditInfo.Size.cy - AButtonVOffset * 3;
  FClockInfo.ClockSize := cxSize(AClockSize, AClockSize);
  FClockInfo.CalculateSize;
end;

procedure TcxCalendarTimeViewInfo.CalculateBounds;
begin
  DoCalculateBounds;
  inherited;
end;

procedure TcxCalendarTimeViewInfo.ClearVisibleElements;
begin
  inherited;
  FClockInfo.ClearVisibleElements;
end;

function TcxCalendarTimeViewInfo.CreateClockViewInfo: TcxCalendarClockViewInfo;
begin
  Result := TcxCalendarClockViewInfo.Create(Calendar);
end;

procedure TcxCalendarTimeViewInfo.CreateElements;
begin
  FTimeEditInfo := CreateTimeEditViewInfo;
  AddHandleNeededElement(FTimeEditInfo);
  FClockInfo := CreateClockViewInfo;
  AddHandleNeededElement(FClockInfo);
end;

function TcxCalendarTimeViewInfo.CreateTimeEditViewInfo: TcxCalendarTimeEditViewInfo;
begin
  Result := TcxCalendarTimeEditViewInfo.Create(Calendar);
end;

procedure TcxCalendarTimeViewInfo.DestroyElements;
begin
  FreeAndNil(FClockInfo);
  FreeAndNil(FTimeEditInfo);
end;

procedure TcxCalendarTimeViewInfo.DoCalculateBounds;
var
  AButtonVOffset: Integer;
  R: TRect;
begin
  AButtonVOffset := (ViewInfo.FButtonsRegionHeight - 1 - ViewInfo.FButtonsHeight) div 2;

  R.Left := Bounds.Left + AButtonVOffset;
  R.Top := Bounds.Top + AButtonVOffset;
  FClockInfo.Bounds := cxRectSetSize(R, FClockInfo.Size);

  R.Left := Bounds.Left + (FClockInfo.Size.cx + AButtonVOffset * 2) div 2 -
    FTimeEditInfo.Size.cx div 2;
  R.Top := FClockInfo.Bounds.Bottom + AButtonVOffset;
  R := cxRectSetSize(R, FTimeEditInfo.Size);
  FTimeEditInfo.Bounds := R;
end;

function TcxCalendarTimeViewInfo.DoCalculateSize: TSize;
var
  AButtonVOffset: Integer;
begin
  Result.cx := Max(FClockInfo.Size.cx, FTimeEditInfo.Size.cx);
  AButtonVOffset := (ViewInfo.FButtonsRegionHeight - 1 - ViewInfo.FButtonsHeight) div 2;
  Inc(Result.cx, AButtonVOffset * 2);
  Result.cy := FClockInfo.Size.cy + FTimeEditInfo.Size.cy + AButtonVOffset;
end;

procedure TcxCalendarTimeViewInfo.Paint(ACanvas: TcxCanvas);
begin
{$ifdef Colorized}
  ACanvas.FillRect(Bounds, clGreen);
{$endif}
  inherited;
end;

{ TcxCalendarModernClockViewInfo }

constructor TcxCalendarModernClockViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited;
  FClock.AutoSize := True;
end;

function TcxCalendarModernClockViewInfo.DoCalculateSize: TSize;
begin
  Result := cxSize(FClock.Width, FClock.Height);
end;

{ TcxCalendarModernTimeViewInfo }

function TcxCalendarModernTimeViewInfo.CreateClockViewInfo: TcxCalendarClockViewInfo;
begin
  Result := TcxCalendarModernClockViewInfo.Create(Calendar);
end;

procedure TcxCalendarModernTimeViewInfo.DoCalculateBounds;
var
  R: TRect;
  ATimeEditOffset: Integer;
begin
  R := Bounds;
  ATimeEditOffset := (ViewInfo.FButtonsRegionHeight - FTimeEditInfo.Size.cy) div 2;
  R.Bottom := Bounds.Bottom - ATimeEditOffset;
  R.Top := R.Bottom - FTimeEditInfo.Size.cy;
  R := cxRectCenterHorizontally(R, FTimeEditInfo.Size.cx);
  FTimeEditInfo.Bounds := R;

  R := Bounds;
  R.Bottom := FTimeEditInfo.Bounds.Top - ATimeEditOffset;
  FClockInfo.Bounds := cxRectCenter(R, FClockInfo.Size);
end;

function TcxCalendarModernTimeViewInfo.DoCalculateSize: TSize;
begin
  Result.cx := Max(FClockInfo.Size.cx, FTimeEditInfo.Size.cx);
  Result.cy := FClockInfo.Size.cy + ViewInfo.FButtonsRegionHeight;
end;

{ TcxCalendarButtonViewInfo }

constructor TcxCalendarButtonViewInfo.Create(ACalendar: TcxCustomCalendar; AType: TCalendarButton);
begin
  inherited Create(ACalendar);
  FType := AType;
  FButton := TcxButton.Create(nil);
  FButton.Visible := False;
  FButton.UseSystemPaint := False;
  FButton.OnClick := Calendar.ButtonClick;
  FButton.Tag := Integer(AType);
  FButton.LookAndFeel.MasterLookAndFeel := Calendar.LookAndFeel;
  if AType = btnOk then
    FButton.Default := True
  else
    if AType = btnCancel then
      FButton.Cancel := True;
end;

destructor TcxCalendarButtonViewInfo.Destroy;
begin
  FreeAndNil(FButton);
  inherited;
end;

procedure TcxCalendarButtonViewInfo.CalculateBounds;
begin
  FButton.BoundsRect := Bounds;
  inherited;
end;

procedure TcxCalendarButtonViewInfo.ClearVisibleElements;
begin
  inherited;
  FIsRightToLeftConverted := False;
  FButton.Visible := False;
end;

function TcxCalendarButtonViewInfo.DoCalculateSize: TSize;
begin
  Result.cx := GetCaptionWidth;
  Result.cy := ViewInfo.FButtonsHeight;
end;

procedure TcxCalendarButtonViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FButton.BoundsRect := TdxRightToLeftLayoutConverter.ConvertRect(FButton.BoundsRect, ABounds);
end;

function TcxCalendarButtonViewInfo.GetCaptionWidth: Integer;
begin
  Result := cxTextWidth(Font, FButton.Caption);
end;

procedure TcxCalendarButtonViewInfo.InitControls;
begin
  inherited InitControls;
  FButton.Parent := Calendar;
end;

procedure TcxCalendarButtonViewInfo.InitializeVisibleElements;
const
  ATabOrder: array [TCalendarButton] of Integer = (3, 2, 1, 4, 5);
begin
  FButton.BiDiMode := Calendar.BiDiMode;
  FButton.Visible := True;
  FButton.TabStop := ViewInfo.IsButtonsTabStop;
  FButton.TabOrder := ATabOrder[FType];
  FButton.Enabled := (FType in [btnClear, btnOk, btnCancel]) or Calendar.CanShowDate(Date);
  if FType = btnOk then
    FButton.Default := True
end;

procedure TcxCalendarButtonViewInfo.UpdateCaption;
var
  ACaption: string;
begin
  case FType of
    btnToday:
      ACaption := cxGetResourceString(@cxSDatePopupToday);
    btnNow:
      ACaption := cxGetResourceString(@cxSDatePopupNow);
    btnClear:
      ACaption := cxGetResourceString(@cxSDatePopupClear);
    btnCancel:
      ACaption := cxGetResourceString(@cxSDatePopupCancel);
  else // btnOk
    ACaption := cxGetResourceString(@cxSDatePopupOK);
  end;
  FButton.Caption := ACaption;
end;

{ TcxCalendarButtonsViewInfo }

procedure TcxCalendarButtonsViewInfo.AddVisibleElements;
const
  AButtonType: array [Ord(btnClear)..Ord(High(TCalendarButton))] of TCalendarButton =
    (btnToday, btnNow, btnClear, btnOk, btnCancel);
var
  I: Integer;
begin
  for I := 0 to High(AButtonType) do
  begin
    if ViewInfo.IsButtonVisible(AButtonType[I]) then
      Add(FButtons[AButtonType[I]]);
  end;
end;

procedure TcxCalendarButtonsViewInfo.CalculateBounds;
begin
  DoCalculateBounds;
  inherited CalculateBounds;
end;

procedure TcxCalendarButtonsViewInfo.DestroyElements;
var
  AButtonType: TCalendarButton;
begin
  for AButtonType := Low(TCalendarButton) to High(TCalendarButton) do
    FreeAndNil(FButtons[AButtonType]);
end;

procedure TcxCalendarButtonsViewInfo.DoCalculateBounds;

  function GetTodayButtonRect: TRect;
  begin
    Result := cxRectBounds(
      (Calendar.Width - FMaxButtonWidth - Byte(VisibleElementsCount = 2) * FMaxButtonWidth) div
       (3 - Byte(VisibleElementsCount = 1)),
      Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight + ViewInfo.FButtonsOffset,
      FMaxButtonWidth + 1, ViewInfo.FButtonsHeight);
  end;

  function GetClearButtonRect: TRect;
  begin
    Result := cxRectBounds(
      Calendar.Width - FMaxButtonWidth -
      (Calendar.Width - Byte(VisibleElementsCount = 2) * FMaxButtonWidth - FMaxButtonWidth) div
       (3 - Byte(VisibleElementsCount = 1)),
      Calendar.ClientHeight - ViewInfo.FButtonsRegionHeight + ViewInfo.FButtonsOffset,
      FMaxButtonWidth + 1, ViewInfo.FButtonsHeight);
  end;

  procedure SetButtonsPosition;
  var
    AButtonLeft, AButtonsOffset, AButtonsTop, I: Integer;
  begin
    AButtonsTop := Calendar.Height - ViewInfo.FButtonsRegionHeight + 1 +
      (ViewInfo.FButtonsRegionHeight - 1 - ViewInfo.FButtonsHeight) div 2;
    AButtonsOffset := MulDiv(GetFontSize(Font), 5, 4);

      (VisibleElements[VisibleElementsCount - 1] as TcxCalendarElementViewInfo).Bounds := cxRectBounds(
        Bounds.Right - AButtonsOffset - FMaxButtonWidth - 1, AButtonsTop, FMaxButtonWidth + 1, ViewInfo.FButtonsHeight);
      if VisibleElementsCount > 1 then
        if VisibleElementsCount = 2 then
          (VisibleElements[0] as TcxCalendarElementViewInfo).Bounds := cxRectBounds(
            (VisibleElements[1] as TcxCalendarElementViewInfo).Bounds.Left - AButtonsOffset -
            FMaxButtonWidth - 1, AButtonsTop, FMaxButtonWidth + 1, ViewInfo.FButtonsHeight)
        else
        begin
          AButtonLeft := AButtonsOffset;
          for I := 0 to VisibleElementsCount - 2 do
          begin
            (VisibleElements[I] as TcxCalendarElementViewInfo).Bounds := cxRectBounds(AButtonLeft, AButtonsTop,
              FMaxButtonWidth + 1, ViewInfo.FButtonsHeight);
            Inc(AButtonLeft, AButtonsOffset + FMaxButtonWidth + 1);
          end;
        end;
  end;

begin
  if not ViewInfo.IsTimeVisible then
  begin
    FButtons[btnToday].Bounds := GetTodayButtonRect;
    FButtons[btnClear].Bounds := GetClearButtonRect;
  end
  else
    SetButtonsPosition;
end;

function TcxCalendarButtonsViewInfo.DoCalculateSize: TSize;
var
  AType: TCalendarButton;
begin
  FMaxButtonWidth := 0;
  for AType := btnClear to btnOk do
    FMaxButtonWidth := Max(FMaxButtonWidth, FButtons[AType].GetCaptionWidth);
  Inc(FMaxButtonWidth, ViewInfo.FColWidth);
  Result.cx := (FMaxButtonWidth + ViewInfo.GetButtonSpace) * VisibleElementsCount + ViewInfo.GetButtonSpace;
  Result.cy := ViewInfo.FButtonsRegionHeight;
end;

function TcxCalendarButtonsViewInfo.IsNative: Boolean;
begin
  Result := FButtons[btnToday].FButton.LookAndFeel.NativeStyle and AreVisualStylesAvailable;
end;

procedure TcxCalendarButtonsViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited;
  if ViewInfo.IsTimeVisible then
    Painter.DrawButtonsBackground(ACanvas, Self);
end;

procedure TcxCalendarButtonsViewInfo.TranslationChanged;
begin
  UpdateCalendarButtonCaptions;
end;

procedure TcxCalendarButtonsViewInfo.CreateElements;
var
  AButtonType: TCalendarButton;
begin
  for AButtonType := Low(TCalendarButton) to High(TCalendarButton) do
  begin
    FButtons[AButtonType] := TcxCalendarButtonViewInfo.Create(Calendar, AButtonType);
    AddHandleNeededElement(FButtons[AButtonType]);
  end;
  UpdateCalendarButtonCaptions;
end;

procedure TcxCalendarButtonsViewInfo.UpdateCalendarButtonCaptions;
var
  AButtonType: TCalendarButton;
begin
  for AButtonType := Low(TCalendarButton) to High(TCalendarButton) do
    FButtons[AButtonType].UpdateCaption;
end;

{ TcxCalendarModernButtonsViewInfo }

procedure TcxCalendarModernButtonsViewInfo.DoCalculateBounds;
var
  I: Integer;
  AButton: TcxCalendarButtonViewInfo;
  R: TRect;
  ABounds: TRect;
  AButtonsSize: Integer;
begin
  ABounds := cxRectInflate(Bounds, -GetButtonsOffset, 0);
  AButtonsSize := Size.cx - 2 * GetButtonsOffset;
  case HorzAlign of
    taRightJustify:
      ABounds.Left := ABounds.Right - AButtonsSize;
    taCenter:
      ABounds := cxRectCenterHorizontally(ABounds, AButtonsSize)
  else // taLeftJustify
    ABounds.Right := ABounds.Left + AButtonsSize;
  end;
  //ABounds
  R := cxRectCenterVertically(ABounds,  ViewInfo.FButtonsHeight);
  R.Left := R.Right - FMaxButtonWidth;
  for I := VisibleElementsCount - 1 downto 0 do
  begin
    AButton := (VisibleElements[I] as TcxCalendarButtonViewInfo);
    AButton.Bounds := R;
    R := cxRectOffsetHorz(R, -cxRectWidth(R) - GetButtonsOffset);
  end;
end;

function TcxCalendarModernButtonsViewInfo.DoCalculateSize: TSize;
var
  AType: TCalendarButton;
begin
  FMaxButtonWidth := 0;
  for AType := Low(TCalendarButton) to High(TCalendarButton) do
    FMaxButtonWidth := Max(FMaxButtonWidth, FButtons[AType].GetCaptionWidth);
  Inc(FMaxButtonWidth, ViewInfo.FColWidth);
  Result.cx := (FMaxButtonWidth + GetButtonsOffset) * VisibleElementsCount + GetButtonsOffset;
  Result.cy := ViewInfo.FButtonsRegionHeight;
end;

function TcxCalendarModernButtonsViewInfo.GetButtonsOffset: Integer;
begin
  Result := GetFontSize(Font);
end;

function TcxCalendarModernButtonsViewInfo.GetHorzAlign: TAlignment;
begin
  Result := taCenter;
end;

procedure TcxCalendarModernButtonsViewInfo.Paint(ACanvas: TcxCanvas);
begin
{$ifdef Colorized}
  ACanvas.FillRect(Bounds, clMaroon);
{$endif}
  PaintChildren(ACanvas);
end;

{ TcxCalendarTouchUIButtonsViewInfo }

procedure TcxCalendarTouchUIButtonsViewInfo.DoCalculateBounds;
var
  I: Integer;
  AButton: TcxCalendarButtonViewInfo;
begin
  inherited DoCalculateBounds;
  for I := 0 to VisibleElementsCount - 1 do
  begin
    AButton := (VisibleElements[I] as TcxCalendarButtonViewInfo);
    if not (AButton.FType in [btnOk, btnCancel])  then
      AButton.Bounds := cxRectOffsetHorz(AButton.Bounds, - GetButtonsOffset);
  end;
end;

function TcxCalendarTouchUIButtonsViewInfo.DoCalculateSize: TSize;
begin
  Result := inherited DoCalculateSize;
  if VisibleElementsCount > 2 then
    Inc(Result.cx, GetButtonsOffset);
end;

function TcxCalendarTouchUIButtonsViewInfo.GetHorzAlign: TAlignment;
begin
  Result := taRightJustify;
end;

{ TcxCalendarViewInfo }

procedure TcxCalendarViewInfo.CalculateBounds;
begin
  FBounds := Calendar.Bounds;
  DoCalculateBounds;
  inherited CalculateBounds;
end;

procedure TcxCalendarViewInfo.Initialize;
begin
  cxScreenCanvas.Font := Font;
  try
    FColWidth := 3 * cxScreenCanvas.TextWidth('0');
    dxAdjustToTouchableSize(FColWidth, Calendar.ScaleFactor);
    FRowHeight := cxScreenCanvas.TextHeight('0') + 2;
    dxAdjustToTouchableSize(FRowHeight, Calendar.ScaleFactor);
    FHeaderHeight := FRowHeight + 3;
    if (Kind = ckDate) and (FCalendar.LookAndFeelPainter.LookAndFeelStyle = lfsOffice11) then
      Dec(FHeaderHeight);

    FButtonsOffset := GetFontSize(Font) div 2;
    FButtonsHeight := MulDiv(cxTextHeight(cxScreenCanvas.Handle), 20, 13) + 1;
    dxAdjustToTouchableSize(FButtonsHeight, Calendar.ScaleFactor);
  finally
    cxScreenCanvas.Dormant;
  end;

  if IsTimeVisible then
  begin
    FButtonsRegionHeight := MulDiv(GetFontSize(Font), 5, 4) + 1;
    if not Odd(FButtonsRegionHeight) then
      Inc(FButtonsRegionHeight);
    Inc(FButtonsRegionHeight, FButtonsHeight);
  end
  else
    FButtonsRegionHeight := FButtonsOffset + (FButtonsHeight - 1) + MulDiv(GetFontSize(Font), 3, 4);

  inherited Initialize;
end;

procedure TcxCalendarViewInfo.Paint(ACanvas: TcxCanvas);

  procedure DrawDateTimeRegionsDelimiter;
  var
    X: Integer;
  begin
    X := DateViewInfo.Bounds.Right + FColWidth div 2 + FSpaceWidth;
    InternalPolyLine(ACanvas, [
      Point(X, DateViewInfo.MonthTableViewInfo.Bounds.Top),
      Point(X, FTimeViewInfo.TimeEditInfo.Bounds.Bottom - 1)],
      LookAndFeelPainter.DefaultDateNavigatorSeparator1Color, True);
  end;

begin
  inherited Paint(ACanvas);
  DrawBackground(ACanvas);
  if IsTimeVisible then
    DrawDateTimeRegionsDelimiter;
end;

procedure TcxCalendarViewInfo.AddVisibleElements;
begin
  if IsHeaderVisible then
    Add(FHeaderViewInfo);
  Add(FDateViewInfo);
  if IsTimeVisible then
    Add(FTimeViewInfo);
  if IsButtonsVisible then
    Add(FButtonsViewInfo);
end;

procedure TcxCalendarViewInfo.CalculateButtonsAreaBounds;
var
  R: TRect;
begin
  if IsButtonsVisible then
  begin
    R := Bounds;
    R.Top := FDateViewInfo.Bounds.Bottom + FCalendar.GetButtonsRegionOffset;
    FButtonsViewInfo.Bounds := R;
  end
  else
    FButtonsViewInfo.Bounds := cxNullRect;
end;

procedure TcxCalendarViewInfo.CalculateChildSizes;
begin
  if IsTimeVisible then
    Remove(FTimeViewInfo);
  inherited CalculateChildSizes;
  if IsTimeVisible and (FTimeViewInfo <> nil) then
    Add(FTimeViewInfo);
end;

procedure TcxCalendarViewInfo.CreateElements;
begin
  FHeaderViewInfo := CreateHeaderViewInfo;
  FDateViewInfo := CreateDateViewInfo;
  FTimeViewInfo := CreateTimeViewInfo;
  if FTimeViewInfo <> nil then
    AddHandleNeededElement(FTimeViewInfo);
  FButtonsViewInfo := CreateButtonsViewInfo;
  if FButtonsViewInfo <> nil then
    AddHandleNeededElement(FButtonsViewInfo);
end;

procedure TcxCalendarViewInfo.DestroyElements;
begin
  FreeAndNil(FButtonsViewInfo);
  FreeAndNil(FTimeViewInfo);
  FreeAndNil(FDateViewInfo);
  FreeAndNil(FHeaderViewInfo);
end;

function TcxCalendarViewInfo.DoCalculateSize: TSize;
begin
  Result.cy := DateViewInfo.Size.cy;
  if IsButtonsVisible then
    Result.cy := Result.cy + FButtonsViewInfo.Size.cy;
  if IsHeaderVisible then
    Result.cy := Result.cy + FHeaderHeight + FCalendar.GetMonthCalendarOffset.Y +
      FCalendar.GetButtonsRegionOffset
  else
    Result.cy := Result.cy + 1;
  Result.cy := Result.cy + FCalendar.GetHeaderOffset.Top;

  Result.cx := DateViewInfo.Size.cx + GetDateInfoOffsets.Left + GetDateInfoOffsets.Right;
  if IsTimeVisible then
  begin
    FTimeViewInfo.Height := Result.cy - FButtonsViewInfo.Size.cy - FHeaderHeight - Calendar.GetMonthCalendarOffset.Y;
    FTimeViewInfo.CalculateSize;
    Result.cx := Result.cx + FCalendar.GetMonthCalendarOffset.X * 2 + FColWidth +
      FTimeViewInfo.Size.cx;
  end;

  if IsButtonsVisible and (Result.cx < FButtonsViewInfo.Size.cx) then
  begin
    FSpaceWidth := (FButtonsViewInfo.Size.cx - Result.cx) div 2;
    if IsTimeVisible then
      FSpaceWidth := FSpaceWidth div 2;
    Result.cx := FButtonsViewInfo.Size.cx;
  end
  else
    FSpaceWidth := 0;
end;

function TcxCalendarViewInfo.GetHitTestIndex: Integer;
begin
  Result := cchtBackground;
end;

procedure TcxCalendarViewInfo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if AllowHidePopupOnMouseUp and
    not Calendar.IsShowOnlyValidDates then
    Calendar.Hide(crEnter);
end;

function TcxCalendarViewInfo.CreateButtonsViewInfo: TcxCalendarButtonsViewInfo;
begin
  Result := nil;
end;

function TcxCalendarViewInfo.CreateDateViewInfo: TcxCalendarDateViewInfo;
begin
  Result := nil;
end;

function TcxCalendarViewInfo.CreateHeaderViewInfo: TcxCalendarHeaderViewInfo;
begin
  Result := nil;
end;

function TcxCalendarViewInfo.CreateTimeViewInfo: TcxCalendarTimeViewInfo;
begin
  Result := nil;
end;

function TcxCalendarViewInfo.AllowHidePopupOnMouseUp: Boolean;
begin
  Result := not IsTimeVisible;
end;

procedure TcxCalendarViewInfo.DoCalculateBounds;

  function GetCurrentDateRegion: TRect;
  begin
    Result := Bounds;
    Result.Bottom := FHeaderHeight;
    if LookAndFeelPainter.LookAndFeelStyle = lfsOffice11 then
    begin
      Inc(Result.Left, Office11HeaderOffset);
      Inc(Result.Top, Office11HeaderOffset);
      Dec(Result.Right, Office11HeaderOffset);
    end;
  end;

  function GetMonthCalendarPosition: TPoint;
  begin
    Result := cxPoint(FSpaceWidth + GetDateInfoOffsets.Left, FHeaderViewInfo.Bounds.Bottom);
    if IsTimeVisible then
      Inc(Result.X, FCalendar.GetMonthCalendarOffset.X);
    if IsHeaderVisible then
      Inc(Result.Y, FCalendar.GetMonthCalendarOffset.Y);
  end;

var
  R: TRect;
begin
  if IsHeaderVisible then
    FHeaderViewInfo.Bounds := GetCurrentDateRegion
  else
    FHeaderViewInfo.Bounds := cxNullRect;

  R := Bounds;
  R.TopLeft := GetMonthCalendarPosition;
  Inc(R.Top, Calendar.GetHeaderOffset.Top);
  FDateViewInfo.Bounds := cxRectSetSize(R, FDateViewInfo.Size);

  if IsTimeVisible then
  begin
    R := Bounds;
    R.Top := FDateViewInfo.Bounds.Top;
    R.Left := R.Right - GetMonthCalendarPosition.X - FTimeViewInfo.Size.cx;
    R.Bottom := R.Top + FTimeViewInfo.Size.cy;
    FTimeViewInfo.Bounds := R;
  end
  else
    FTimeViewInfo.Bounds := cxNullRect;

  CalculateButtonsAreaBounds;
end;

procedure TcxCalendarViewInfo.DoSetFirstDate(Value: Double; AAnimated: Boolean);
begin
  DateViewInfo.ChangeFirstDate(Value, AAnimated);
end;

procedure TcxCalendarViewInfo.DoSetSelectDate(AValue: Double;
  ARepositionVisibleDates, AAnimated: Boolean);
begin
  FDateViewInfo.ChangeSelectDate(AValue, ARepositionVisibleDates, AAnimated);
end;

procedure TcxCalendarViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  ACanvas.FillRect(Bounds, FCalendar.BackgroundColor);
end;

function TcxCalendarViewInfo.GetArrowSize: TSize;
begin
  Result := cxSize(FColWidth - 2, FHeaderHeight - 1 - GetHeaderBorderWidth);
end;

function TcxCalendarViewInfo.GetButtonSpace: Integer;
begin
  Result := FColWidth;
end;

function TcxCalendarViewInfo.GetDateInfoOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TcxCalendarViewInfo.GetMonthDateHeaderText: string;
begin
  if IsYearDateHeaderVisible then
    Result := cxGetLocalMonthName(FCalendar.FirstDate, cxGetLocalCalendar)
  else
    Result := cxGetLocalMonthYear(FCalendar.FirstDate, cxGetLocalCalendar);
end;

function TcxCalendarViewInfo.HasBackground: Boolean;
begin
  Result := FButtonsViewInfo.IsNative;
end;

function TcxCalendarViewInfo.IsButtonVisible(AType: TCalendarButton): Boolean;
begin
  case AType of
    btnOk:
      Result := ViewInfo.IsTimeVisible;
    btnClear, btnToday:
      Result := AType in Calendar.CalendarButtons;
    btnCancel:
      Result := False;
  else // btnNow
    Result := ViewInfo.IsTimeVisible and (AType in Calendar.CalendarButtons);
  end;
end;

function TcxCalendarViewInfo.IsDateHeaderTransparent: Boolean;
begin
  Result := LookAndFeelPainter.LookAndFeelStyle in [lfsSkin, lfsNative];
end;

function TcxCalendarViewInfo.IsHeaderVisible: Boolean;
begin
  Result := IsTimeVisible;
end;

function TcxCalendarViewInfo.IsYearDateHeaderVisible: Boolean;
begin
  Result := FCalendar.ArrowsForYear;
end;

procedure TcxCalendarViewInfo.Reset;
begin
end;

procedure TcxCalendarViewInfo.SetFirstDate(Value: Double; AAnimated: Boolean);
begin
  if cxIsDateValid(Value) then
  begin
    Value := dxChangeDay(Value, 1);
    if Calendar.FirstDate <> Value then
      DoSetFirstDate(Value, AAnimated);
  end;
end;

procedure TcxCalendarViewInfo.SetSelectDate(AValue: Double;
  ARepositionVisibleDates: Boolean; AAnimated: Boolean);
begin
  if (Calendar.SelectDate <> AValue) and Calendar.CanShowDate(AValue) and cxIsDateValid(AValue) then
    DoSetSelectDate(AValue, ARepositionVisibleDates, AAnimated);
end;

procedure TcxCalendarViewInfo.TimeChanged(Sender: TObject);
var
  ATime: TTime;
begin
  ATime := (Sender as TcxTimeEdit).Time;
  FTimeViewInfo.FClockInfo.FClock.Time := ATime;
  Calendar.InternalSetTime(ATime);
  HeaderViewInfo.Invalidate;
end;

function TcxCalendarViewInfo.GetFont: TFont;
begin
  Result := FCalendar.Font;
end;

function TcxCalendarViewInfo.GetHeaderBorderWidth: Integer;
begin
  if IsTimeVisible then
    Result := 1
  else
    Result := Integer(not FCalendar.FFlat);
end;

function TcxCalendarViewInfo.GetKind: TcxCalendarKind;
begin
  Result := FCalendar.Kind;
end;

function TcxCalendarViewInfo.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FCalendar.LookAndFeelPainter;
end;

function TcxCalendarViewInfo.IsButtonsTabStop: Boolean;
begin
  Result := IsTimeVisible;
end;

function TcxCalendarViewInfo.IsButtonsVisible: Boolean;
var
  AType: TCalendarButton;
begin
  Result := False;
  for AType := Low(TCalendarButton) to High(TCalendarButton) do
    if IsButtonVisible(AType) then
    begin
      Result := True;
      Break;
    end;
end;

function TcxCalendarViewInfo.IsTimeVisible: Boolean;
begin
  Result := Kind = ckDateTime;
end;

{ TcxCalendarClassicViewInfo }

constructor TcxCalendarClassicViewInfo.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create(ACalendar);
  CreateMonthListBox;
end;

destructor TcxCalendarClassicViewInfo.Destroy;
begin
  FreeAndNil(FMonthListBox);
  inherited Destroy;
end;

procedure TcxCalendarClassicViewInfo.Initialize;
begin
  inherited Initialize;
  FMonthListBox.ShowYears := not Calendar.ArrowsForYear or Calendar.YearsInMonthList;
end;

function TcxCalendarClassicViewInfo.GetMonthListBoxOwnerBounds: TRect;
begin
  Result := DateViewInfo.HeadersViewInfo.MonthHeaderViewInfo.TextCellViewInfo.Bounds;
end;

function TcxCalendarClassicViewInfo.CreateButtonsViewInfo: TcxCalendarButtonsViewInfo;
begin
  Result := TcxCalendarButtonsViewInfo.Create(Calendar);
end;

function TcxCalendarClassicViewInfo.CreateDateViewInfo: TcxCalendarDateViewInfo;
begin
  Result := TcxCalendarDateViewInfo.Create(Calendar);
end;

function TcxCalendarClassicViewInfo.CreateTimeViewInfo: TcxCalendarTimeViewInfo;
begin
  Result := TcxCalendarTimeViewInfo.Create(Calendar);
end;

function TcxCalendarClassicViewInfo.CreateHeaderViewInfo: TcxCalendarHeaderViewInfo;
begin
  Result := TcxCalendarHeaderViewInfo.Create(Calendar);
end;

procedure TcxCalendarClassicViewInfo.CreateMonthListBox;
begin
  FMonthListBox := TcxMonthListBox.Create(Calendar);
  FMonthListBox.CaptureFocus := False;
  FMonthListBox.IsTopMost := True;
  FMonthListBox.OwnerParent := Calendar;
end;

{ TcxCalendarModernViewInfo }

function TcxCalendarModernViewInfo.CreateButtonsViewInfo: TcxCalendarButtonsViewInfo;
begin
  Result := TcxCalendarModernButtonsViewInfo.Create(Calendar);
end;

function TcxCalendarModernViewInfo.CreateDateViewInfo: TcxCalendarDateViewInfo;
begin
  Result := TcxCalendarModernDateViewInfo.Create(Calendar);
end;

function TcxCalendarModernViewInfo.CreateHeaderViewInfo: TcxCalendarHeaderViewInfo;
begin
  Result := TcxCalendarModernHeaderViewInfo.Create(Calendar);
end;

function TcxCalendarModernViewInfo.CreateTimeViewInfo: TcxCalendarTimeViewInfo;
begin
  Result := TcxCalendarModernTimeViewInfo.Create(Calendar);
end;

function TcxCalendarModernViewInfo.AllowHidePopupOnMouseUp: Boolean;
begin
  Result := inherited AllowHidePopupOnMouseUp and (DateViewInfo.ViewStyle = cvdsMonth);
end;

function TcxCalendarModernViewInfo.CanFade: Boolean;
begin
  Result := Calendar.LookAndFeel.SkinPainter <> nil;
end;

procedure TcxCalendarModernViewInfo.DoCalculateBounds;
var
  R: TRect;
begin
  FBounds := Calendar.Bounds;

  if IsHeaderVisible then
  begin
    R := Bounds;
    R.Top := GetHeaderOffset;
    R.Bottom := R.Top + FHeaderHeight;
    FHeaderViewInfo.Bounds := R;
  end
  else
    FHeaderViewInfo.Bounds := cxNullRect;

  R := Bounds;
  if FDateViewInfo.Size.cx < FButtonsViewInfo.Size.cx then
    R.Left := GetDateInfoOffsets.Left + (FButtonsViewInfo.Size.cx - FDateViewInfo.Size.cx) div 2
  else
    R.Left := GetDateInfoOffsets.Left;
  R.Top := FHeaderViewInfo.Bounds.Bottom + GetDateInfoOffsets.Top;
  FDateViewInfo.Bounds := cxRectSetSize(R, FDateViewInfo.Size);

  if IsButtonsVisible then
  begin
    R := Bounds;
    R.Top := R.Bottom - FButtonsRegionHeight;
    if FButtonsViewInfo.Size.cx < FDateViewInfo.Size.cx then
    begin
      R.Left := FDateViewInfo.Bounds.Left;
      R.Right := FDateViewInfo.Bounds.Right;
    end
    else
    begin
      R.Left := GetDateInfoOffsets.Left;
      R.Right := R.Left + FButtonsViewInfo.Size.cx;
    end;

    FButtonsViewInfo.Bounds := R;
  end
  else
    FButtonsViewInfo.Bounds := cxNullRect;

  if IsTimeVisible then
  begin
    R := Bounds;
    R.Top := FDateViewInfo.Bounds.Top;
    Dec(R.Right, GetDateInfoOffsets.Right);
    R.Left := R.Right - FTimeViewInfo.Size.cx;
    FTimeViewInfo.Bounds := R;
  end
  else
    FTimeViewInfo.Bounds := cxNullRect;
end;

function TcxCalendarModernViewInfo.DoCalculateSize: TSize;
begin
  FTimeViewInfo.CalculateSize;

  Result.cy := DateViewInfo.Size.cy + GetDateInfoOffsets.Top;

  if IsButtonsVisible then
    Result.cy := Result.cy + FButtonsViewInfo.Size.cy
  else
    Result.cy := Result.cy + GetDateInfoOffsets.Bottom;

  if IsTimeVisible then
    Result.cy := Max(Result.cy, FTimeViewInfo.Size.cy + GetDateInfoOffsets.Top);

  if IsHeaderVisible then
    Result.cy := Result.cy + FHeaderHeight + GetHeaderOffset;

  Result.cx := DateViewInfo.Size.cx;

  if IsButtonsVisible and (Result.cx < FButtonsViewInfo.Size.cx) then
    Result.cx := FButtonsViewInfo.Size.cx;

  Inc(Result.cx, GetDateInfoOffsets.Left + GetDateInfoOffsets.Right);

  if IsTimeVisible then
    Result.cx := Result.cx + GetDateTimeInfoOffset + FTimeViewInfo.Size.cx;
end;

function TcxCalendarModernViewInfo.GetArrowSize: TSize;
var
  ASize: Integer;
begin
  ASize := FHeaderHeight - 1 - GetHeaderBorderWidth;
  Result := cxSize(ASize, ASize);
end;

function TcxCalendarModernViewInfo.GetDateInfoOffsets: TRect;
var
  AInterval: Integer;
begin
  if IsTimeVisible then
    AInterval := MulDiv(GetFontSize(Font), 7, 4)
  else
    AInterval := MulDiv(GetFontSize(Font), 3, 8);
  Result := Rect(AInterval, GetFontSize(Font), AInterval, AInterval);
end;

function TcxCalendarModernViewInfo.GetDateTimeInfoOffset: Integer;
begin
  Result :=  MulDiv(GetFontSize(Font), 7, 4);
end;

function TcxCalendarModernViewInfo.GetHeaderOffset: Integer;
begin
  Result := GetFontSize(Font);
end;

function TcxCalendarModernViewInfo.GetMonthDateHeaderText: string;

  function GetYearsGroup(ADate: TDateTime; AGroupInterval: Integer): string;
  var
    AStartGroupYear, AEndGroupYear, AYear: Word;
    AStartDate, AEndDate: TDateTime;
  begin
    AYear := cxGetLocalCalendar.GetYear(ADate);
    AStartGroupYear := AYear div AGroupInterval * AGroupInterval;
    AEndGroupYear := AStartGroupYear + AGroupInterval - 1;
    AStartDate := EncodeDate(AStartGroupYear, 1, 1);
    AEndDate := EncodeDate(AEndGroupYear, 1, 1);
    Result := Format('%s-%s', [cxGetLocalYear(AStartDate, cxGetLocalCalendar),
      cxGetLocalYear(AEndDate, cxGetLocalCalendar)]);
  end;

begin
  case DateViewInfo.ViewStyle of
    cvdsMonth: Result := cxGetLocalMonthYear(FCalendar.SelectDate, cxGetLocalCalendar);
    cvdsYear: Result := cxGetLocalYear(FCalendar.SelectDate, cxGetLocalCalendar);
    cvdsDecade: Result := GetYearsGroup(FCalendar.SelectDate, 10);
  else // cvdsCentury
    Result := GetYearsGroup(FCalendar.SelectDate, 100);
  end;
end;

function TcxCalendarModernViewInfo.IsDateHeaderTransparent: Boolean;
begin
  Result := True;
end;

function TcxCalendarModernViewInfo.IsHeaderVisible: Boolean;
begin
  Result := Calendar.ShowToday;
end;

function TcxCalendarModernViewInfo.IsYearDateHeaderVisible: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarModernViewInfo.Paint(ACanvas: TcxCanvas);
begin
  DrawBackground(ACanvas);
  PaintChildren(ACanvas);
end;

procedure TcxCalendarModernViewInfo.Reset;
begin
  inherited Reset;
  DateViewInfo.FViewStyle := cvdsMonth;
end;

function TcxCalendarModernViewInfo.GetDateViewInfo: TcxCalendarModernDateViewInfo;
begin
  Result := inherited DateViewInfo as TcxCalendarModernDateViewInfo;
end;

function TcxCalendarModernViewInfo.GetHeaderViewInfo: TcxCalendarModernHeaderViewInfo;
begin
  Result := inherited HeaderViewInfo as TcxCalendarModernHeaderViewInfo;
end;

{ TcxCalendarTouchUIViewInfo }

procedure TcxCalendarTouchUIViewInfo.AddVisibleElements;
begin
  if IsButtonsVisible then
    Add(FButtonsViewInfo);
end;

function TcxCalendarTouchUIViewInfo.AllowHidePopupOnMouseUp: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarTouchUIViewInfo.Paint(ACanvas: TcxCanvas);
begin
  DrawBackground(ACanvas);
  PaintChildren(ACanvas);
end;

function TcxCalendarTouchUIViewInfo.CreateButtonsViewInfo: TcxCalendarButtonsViewInfo;
begin
  Result := TcxCalendarTouchUIButtonsViewInfo.Create(Calendar);
end;

procedure TcxCalendarTouchUIViewInfo.CreateElements;
begin
  inherited CreateElements;
  FDateTimeWheelPicker := TdxDateTimeWheelPicker.Create(nil);
  FDateTimeWheelPicker.Properties.OnEditValueChanged := DatePickerValueChanged;
end;

procedure TcxCalendarTouchUIViewInfo.DestroyElements;
begin
  FreeAndNil(FDateTimeWheelPicker);
  inherited DestroyElements;
end;

procedure TcxCalendarTouchUIViewInfo.DoCalculateBounds;
var
  R: TRect;
begin
  if IsButtonsVisible then
  begin
    R := Bounds;
    R.Top := FDateTimeWheelPicker.BoundsRect.Bottom;
    FButtonsViewInfo.Bounds := R;
    if FDateTimeWheelPicker.Width < FButtonsViewInfo.Size.cx then
    begin
      FDateTimeWheelPicker.AutoSize := False;
      FDateTimeWheelPicker.Width := Calendar.Width;
    end;
  end
  else
    FButtonsViewInfo.Bounds := cxNullRect;
end;

function TcxCalendarTouchUIViewInfo.DoCalculateSize: TSize;
begin
  FDateTimeWheelPicker.HandleNeeded;
  Result.cx := FDateTimeWheelPicker.Width;
  Result.cy := FDateTimeWheelPicker.Height;
  if IsButtonsVisible then
  begin
    Result.cx := Max(FButtonsViewInfo.Size.cx, Result.cx);
    Result.cy := FButtonsViewInfo.Size.cy + Result.cy;
  end;
end;

procedure TcxCalendarTouchUIViewInfo.DoSetFirstDate(Value: Double; AAnimated: Boolean);
begin
  Calendar.InternalSetFirstDate(Value);
end;

procedure TcxCalendarTouchUIViewInfo.DoSetSelectDate(
  AValue: Double; ARepositionVisibleDates: Boolean; AAnimated: Boolean);
begin
  Calendar.InternalSetSelectDate(AValue);
end;

procedure TcxCalendarTouchUIViewInfo.InitializeVisibleElements;
var
  AWheelKinds: TdxDateTimePickerWheels;
begin
  inherited InitializeVisibleElements;
  AWheelKinds := [pwYear..pwDay];
  if IsTimeVisible then
  begin
    case Calendar.TimeFormat of
      tfHourMinSec:
        AWheelKinds := AWheelKinds + [pwHour..pwSecond];
      tfHourMin:
        AWheelKinds := AWheelKinds + [pwHour, pwMinute]
    else //tfHour
      AWheelKinds := AWheelKinds + [pwHour];
    end;
  end;
  FDateTimeWheelPicker.BiDiMode := Calendar.BiDiMode;
  FDateTimeWheelPicker.Style.LookAndFeel.MasterLookAndFeel := Calendar.LookAndFeel;
  if Calendar.LookAndFeel.NativeStyle then
    FDateTimeWheelPicker.Style.BorderStyle := ebsNone
  else
  begin
    FDateTimeWheelPicker.Style.AssignedValues := FDateTimeWheelPicker.Style.AssignedValues - [csvBorderStyle];
    FDateTimeWheelPicker.Style.Edges := [bBottom];
  end;

  FDateTimeWheelPicker.Properties.Wheels := AWheelKinds;
  FDateTimeWheelPicker.Properties.Use24HourFormat := Calendar.Use24HourFormat;
  FDateTimeWheelPicker.EditValue := GetDateTime;
  if Calendar.IsShowOnlyValidDates then
  begin
    FDateTimeWheelPicker.Properties.MinDate := Calendar.MinDate;
    FDateTimeWheelPicker.Properties.MaxDate := Calendar.MaxDate;
  end
  else
  begin
    FDateTimeWheelPicker.Properties.MinDate := 0;
    FDateTimeWheelPicker.Properties.MaxDate := 0;
  end;
  FDateTimeWheelPicker.AutoSize := True;
  FDateTimeWheelPicker.TabStop := True;
  FDateTimeWheelPicker.TabOrder := 0;
end;

function TcxCalendarTouchUIViewInfo.IsButtonsTabStop: Boolean;
begin
  Result := True;
end;

function TcxCalendarTouchUIViewInfo.IsButtonVisible(AType: TCalendarButton): Boolean;
begin
  Result := (AType in [btnOk, btnCancel]) or inherited IsButtonVisible(AType);
end;

function TcxCalendarTouchUIViewInfo.IsHeaderVisible: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarTouchUIViewInfo.DatePickerValueChanged(ASender: TObject);
begin
  FCalendar.InternalSetSelectDate(dxDateOf(FDateTimeWheelPicker.EditValue));
  if IsTimeVisible then
    FCalendar.InternalSetTime(dxTimeOf(FDateTimeWheelPicker.EditValue));
end;

function TcxCalendarTouchUIViewInfo.GetDateTime: Double;
begin
  Result := Calendar.SelectDate;
  if IsTimeVisible then
    Result := dxDateOf(Result) + cxSign(Result) * dxTimeOf(TDateTime(Calendar.Time));
end;

procedure TcxCalendarTouchUIViewInfo.InitControls;
begin
  inherited InitControls;
  FDateTimeWheelPicker.Parent := Calendar;
  FDateTimeWheelPicker.AutoSize := True;
end;

{ TcxCalendarHitTest }

constructor TcxCalendarHitTest.Create(AOwner: TcxCalendarController);
begin
  inherited Create;
  FController := AOwner;
end;

procedure TcxCalendarHitTest.Clear;
begin
  FFlags := 0;
  FHitObject := nil;
end;

procedure TcxCalendarHitTest.Recalculate;
begin
  Clear;
  FController.ViewInfo.GetHitTest(Self);
end;

procedure TcxCalendarHitTest.SetBitState(AIndex: Integer; AValue: Boolean);
begin
  if AValue then
    FFlags := FFlags or (1 shl AIndex)
  else
    FFlags := FFlags and not (1 shl AIndex);
end;

procedure TcxCalendarHitTest.Calculate(const APoint: TPoint);
begin
  FHitPoint := APoint;
  Recalculate;
end;

function TcxCalendarHitTest.GetBitState(AIndex: Integer): Boolean;
begin
  Result := (FFlags and (1 shl AIndex)) <> 0;
end;

{ TcxCalendarController }

constructor TcxCalendarController.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create;
  FCalendar := ACalendar;
  FHitTest := CreateHitTestController;
  FTimer := cxCreateTimer(DoScrollArrow, DateNavigatorTime, False);
end;

destructor TcxCalendarController.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FHitTest);
  inherited;
end;

function TcxCalendarController.CanFocus: Boolean;
begin
  Result := DoCanFocus;
end;

procedure TcxCalendarController.DblClick;
begin
  DoDblClick;
end;

procedure TcxCalendarController.HidePopup(Sender: TcxControl;
  AReason: TcxEditCloseUpReason);
begin
  FTimer.Enabled := False;
end;

procedure TcxCalendarController.KeyDown(var AKey: Word; AShift: TShiftState);
begin
  DoKeyDown(AKey, AShift);
end;

procedure TcxCalendarController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TcxCalendarController.MouseLeave;
begin
  DoMouseLeave;
end;

procedure TcxCalendarController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(Shift, X, Y);
end;

procedure TcxCalendarController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, X, Y);
end;

function TcxCalendarController.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TcxCalendarController.CreateHitTestController: TcxCalendarHitTest;
begin
  Result := TcxCalendarHitTest.Create(Self);
end;

function TcxCalendarController.DoCanFocus: Boolean;
begin
  Result := True;
end;

procedure TcxCalendarController.DoDblClick;
begin
  if ViewInfo.IsTimeVisible and (HotElement <> nil) then
    HotElement.DblClick;
end;

procedure TcxCalendarController.DoKeyDown(var AKey: Word; AShift: TShiftState);
begin
  ViewInfo.DateViewInfo.KeyDown(AKey, AShift);
end;

procedure TcxCalendarController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    Exit;
  FHitTest.Calculate(cxPoint(X, Y));
  PressedElement := FHitTest.HitObject;
  if PressedElement <> nil then
    PressedElement.MouseDown(Button, Shift, X, Y);
end;

procedure TcxCalendarController.DoMouseLeave;
begin
  CheckHotElement([], cxInvisiblePoint);
  PressedElement := nil;
end;

procedure TcxCalendarController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  CheckHotElement(Shift, cxPoint(X, Y));
  if HotElement <> nil then
    HotElement.MouseMove(Shift, X, Y);
end;

procedure TcxCalendarController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APreviousPressedElement: TcxCalendarElementViewInfo;
begin
  FHitTest.Calculate(cxPoint(X, Y));
  APreviousPressedElement := PressedElement;
  PressedElement := nil;

  if (HotElement <> nil) and (HotElement = APreviousPressedElement) then
    HotElement.Click;

  if FTimer.Enabled then
  begin
    FTimer.Enabled := False;
    Exit;
  end;

  if HotElement <> nil then
    HotElement.MouseUp(Button, Shift, X, Y);
end;

function TcxCalendarController.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  AArrowMap: array[Boolean, Boolean] of TcxCalendarArrow =
    ((caPrevMonth, caNextMonth), (caPrevYear, caNextYear));
begin
  Result := True;
  FCalendar.DoStep(AArrowMap[HitTest.HitAtYearDateHeader, WheelDelta < 0]);
end;

function TcxCalendarController.IsMouseWheelHandleNeeded(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := True;
end;

procedure TcxCalendarController.CheckHotElement(AShift: TShiftState;
  const APoint: TPoint);
begin
  FHitTest.Calculate(APoint);
  HotElement := FHitTest.HitObject;
end;

procedure TcxCalendarController.DoScrollArrow(Sender: TObject);
begin
  if HitTest.HitAtDateHeaderArrow then
    (HitTest.HitObject as TcxCalendarArrowViewInfo).DoAction;
end;

function TcxCalendarController.GetViewInfo: TcxCalendarViewInfo;
begin
  Result := FCalendar.ViewInfo;
end;

procedure TcxCalendarController.SetHotElement(
  AValue: TcxCalendarElementViewInfo);
begin
  if FHotElement <> AValue then
  begin
    if (FHotElement <> nil) then
      if FHotElement = FPressedElement then
        FHotElement.State := cesPressed
      else
        FHotElement.State := cesNormal;
    FHotElement := AValue;
    if (FHotElement <> nil) then
      if FHotElement = FPressedElement then
        FHotElement.State := cesPressed
      else
        FHotElement.State := cesHot;
  end;
end;

procedure TcxCalendarController.SetPressedElement(
  AValue: TcxCalendarElementViewInfo);
begin
  if FPressedElement <> AValue then
  begin
    if (FPressedElement <> nil) and
      (FPressedElement.State <> cesSelected) then
      if FPressedElement = FHotElement then
        FPressedElement.State := cesHot
      else
        FPressedElement.State := cesNormal;
    FPressedElement := AValue;
    if (FPressedElement <> nil) and
      (FPressedElement.State <> cesSelected) then
      FPressedElement.State := cesPressed;
  end;
end;

{ TcxCalendarClassicController }

procedure TcxCalendarClassicController.HidePopup(Sender: TcxControl;
  AReason: TcxEditCloseUpReason);
begin
  inherited HidePopup(Sender, AReason);
  ViewInfo.FMonthListBox.FTimer.Enabled := False;
  if ViewInfo.FMonthListBox.Showing then ViewInfo.FMonthListBox.CloseUp;
end;

procedure TcxCalendarClassicController.DoKeyDown(var AKey: Word;
  AShift: TShiftState);
begin
  inherited DoKeyDown(AKey, AShift);
  if FCalendar.IsPopup then
    case AKey of
      VK_ESCAPE:
        if not ViewInfo.FMonthListBox.ShowYears and ViewInfo.FMonthListBox.IsVisible then
          ViewInfo.FMonthListBox.CloseUp
        else
          FCalendar.Hide(crCancel);
      VK_RETURN:
        if not ViewInfo.FMonthListBox.Showing then
        begin
          FCalendar.DoDateTimeChanged;
          FCalendar.Hide(crEnter);
        end;
    end;
end;

function TcxCalendarClassicController.IsMouseWheelHandleNeeded(
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := not ViewInfo.FMonthListBox.IsVisible;
  if Result then
  begin
    HitTest.Calculate(FCalendar.ScreenToClient(MousePos));
    Result := HitTest.HitAtMonthDateHeader or
      HitTest.HitAtYearDateHeader;
  end;
end;

function TcxCalendarClassicController.GetViewInfo: TcxCalendarClassicViewInfo;
begin
  Result := inherited ViewInfo as TcxCalendarClassicViewInfo;
end;

{ TcxCalendarModernController }

procedure TcxCalendarModernController.DoKeyDown(var AKey: Word;
  AShift: TShiftState);
var
  AViewStyle: TcxCalendarModernDateViewStyle;
begin
  AViewStyle := ViewInfo.DateViewInfo.ViewStyle;
  inherited DoKeyDown(AKey, AShift);
  if FCalendar.IsPopup then
    case AKey of
      VK_ESCAPE:
        FCalendar.Hide(crCancel);
      VK_RETURN:
        if (ViewInfo.DateViewInfo.ViewStyle = cvdsMonth) and
          (AViewStyle = cvdsMonth) then
        begin
          FCalendar.DoDateTimeChanged;
          FCalendar.Hide(crEnter);
        end;
    end;
end;

function TcxCalendarModernController.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  AAction: array [TcxCalendarModernDateViewStyle, Boolean] of TcxCalendarArrow =
    ((caPrevMonth, caNextMonth), (caPrevYear, caNextYear),
     (caPrevDecade, caNextDecade), (caPrevCentury, caNextCentury));
begin
  Result := True;
  if ssCtrl in Shift then
    if WheelDelta < 0 then
      ViewInfo.DateViewInfo.NextViewStyle
    else
      ViewInfo.DateViewInfo.PreviousViewStyle
  else
    FCalendar.DoStep(AAction[ViewInfo.DateViewInfo.ViewStyle, WheelDelta < 0]);
end;

function TcxCalendarModernController.IsMouseWheelHandleNeeded(
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := FCalendar.Focused;
end;

function TcxCalendarModernController.GetViewInfo: TcxCalendarModernViewInfo;
begin
  Result := inherited ViewInfo as TcxCalendarModernViewInfo;
end;

{ TcxCalendarTouchUIController }

function TcxCalendarTouchUIController.DoCanFocus: Boolean;
begin
  Result := False;
end;

procedure TcxCalendarTouchUIController.DoKeyDown(var AKey: Word;
  AShift: TShiftState);
begin
  if AKey = VK_ESCAPE then
    FCalendar.Hide(crCancel);
end;

function TcxCalendarTouchUIController.IsMouseWheelHandleNeeded(
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
end;

{ TcxCalendarPainter }

constructor TcxCalendarPainter.Create(ACalendar: TcxCustomCalendar);
begin
  inherited Create;
  FCalendar := ACalendar;
end;

procedure TcxCalendarPainter.DrawArrow(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarArrowViewInfo);
var
  P: TcxArrowPoints;
  R: TRect;
  AIsTransparent: Boolean;
begin
  R := AViewInfo.Bounds;
  AIsTransparent := AViewInfo.ViewInfo.IsDateHeaderTransparent;
  if not AIsTransparent then
  begin
    if FCalendar.FFlat and (FCalendar.Kind = ckDate) then
      InternalPolyLine(ACanvas, [Point(R.Left, R.Top), Point(R.Right - 1, R.Top)],
        FCalendar.GetHeaderColor, True);
    InternalPolyLine(ACanvas, [Point(R.Left, R.Bottom - 1),
      Point(R.Right - 1, R.Bottom - 1)], FCalendar.GetHeaderColor, True);
    if FCalendar.FFlat and (FCalendar.Kind = ckDate) and (LookAndFeelPainter.LookAndFeelStyle <> lfsOffice11) then
      Inc(R.Top);
    Dec(R.Bottom);
  end;

  if not AIsTransparent then
    ACanvas.FillRect(R, FCalendar.GetHeaderColor);
  LookAndFeelPainter.CalculateArrowPoints(R, P, AViewInfo.GetRealDirection, False);
  ACanvas.Brush.Color := LookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(False);
  ACanvas.Pen.Color := LookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(False);
  ACanvas.Polygon(P);

  ACanvas.ExcludeClipRect(AViewInfo.Bounds);
end;

procedure TcxCalendarPainter.DrawButtonsBackground(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarButtonsViewInfo);
var
  R, R1: TRect;
begin
  R := AViewInfo.Bounds;
  R1 := R;
  InternalPolyLine(ACanvas, [R1.TopLeft, Point(R1.Right - 1, R1.Top)], FCalendar.ContainerBorderColor, True);
  Inc(R1.Top);
  if not FCalendar.FFlat then
  begin
    ACanvas.DrawEdge(R1, False, False, cxBordersAll);
    InflateRect(R1, -1, -1);
  end;
  ACanvas.FillRect(R1, FCalendar.BackgroundColor);
  ACanvas.ExcludeClipRect(R);
end;

procedure TcxCalendarPainter.DrawDateCellBackground(ACanvas: TcxCanvas; AViewInfo: TcxCalendarDateCellViewInfo);
begin
  ACanvas.SaveState;
  ACanvas.FillRect(AViewInfo.Bounds);
  ACanvas.RestoreState;
end;

procedure TcxCalendarPainter.DrawDateCellSelection(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateCellViewInfo);
begin
  LookAndFeelPainter.DrawCalendarDateCellSelection(ACanvas, AViewInfo.Bounds, AViewInfo.GetDrawStates);
end;

procedure TcxCalendarPainter.DrawDateCellText(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateCellViewInfo);
var
  AFormat: Cardinal;
  ARect: TRect;
  ASize: TSize;
begin
  AFormat := {DT_WORDBREAK or} SystemAlignmentsHorz[AViewInfo.HorzAlignment] or DT_EDITCONTROL;
  ARect := AViewInfo.TextRect;
  if ACanvas.Font.Style <> AViewInfo.Font.Style then
  begin
    ASize := ACanvas.TextExtent(AViewInfo.GetText);
    ARect.Left := ARect.Right - ASize.cx;
  end;
  cxDrawText(ACanvas, AViewInfo.GetText, ARect, AFormat);
end;

procedure TcxCalendarPainter.DrawDateHeaderCell(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateHeaderCellViewInfo);
var
  R: TRect;
begin
  ACanvas.Font.Color  := LookAndFeelPainter.DefaultHeaderTextColor;
  ACanvas.Brush.Color := FCalendar.GetHeaderColor;

  if AViewInfo.ViewInfo.IsDateHeaderTransparent then
    ACanvas.Brush.Style := bsClear;
  ACanvas.Font.Color := LookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(AViewInfo.State = cesHot);
  R := AViewInfo.Bounds;
  FCalendar.CorrectHeaderTextRect(R);
  cxDrawText(ACanvas, AViewInfo.GetText, R, DT_CENTER + DT_VCENTER + DT_SINGLELINE + DT_WORDBREAK);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TcxCalendarPainter.DrawDateHeaderCellText(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateHeaderCellViewInfo);
begin
end;

procedure TcxCalendarPainter.DrawHeader(ACanvas: TcxCanvas; AViewInfo: TcxCalendarHeaderViewInfo);
var
  R: TRect;
begin
  R := AViewInfo.Bounds;
  LookAndFeelPainter.DrawDateNavigatorDateHeader(ACanvas, R);
  ACanvas.Font := AViewInfo.Font;
  ACanvas.Font.Color := LookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(False);
  ACanvas.Brush.Style := bsClear;
  cxDrawText(ACanvas, AViewInfo.GetText, R, DT_CENTER + DT_VCENTER + DT_SINGLELINE + DT_WORDBREAK);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.ExcludeClipRect(AViewInfo.Bounds);
end;

procedure TcxCalendarPainter.DrawHeaderText(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarHeaderViewInfo);
begin
end;

function TcxCalendarPainter.GetCellTextColor(
  ADrawStates: TcxCalendarElementStates): TColor;
begin
  if cesSelected in ADrawStates then
    Result := LookAndFeelPainter.DefaultDateNavigatorSelectionTextColor
  else
    if cesDisabled in ADrawStates then
      Result := LookAndFeelPainter.DefaultDateNavigatorInactiveTextColor
    else
      if cesMarked in ADrawStates then
        Result := LookAndFeelPainter.DefaultDateNavigatorTodayTextColor
      else
        Result := LookAndFeelPainter.DefaultDateNavigatorTextColor
end;

function TcxCalendarPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FCalendar.LookAndFeelPainter;
end;

{ TcxModernCalendarPainter }

procedure TcxCalendarModernPainter.DrawArrow(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarArrowViewInfo);
begin
  LookAndFeelPainter.DrawScaledModernCalendarArrow(ACanvas,
    AViewInfo.Bounds, AViewInfo.GetRealDirection, AViewInfo.State, FCalendar.ScaleFactor);
end;

procedure TcxCalendarModernPainter.DrawDateCellBackground(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateCellViewInfo);
begin
end;

procedure TcxCalendarModernPainter.DrawDateCellSelection(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateCellViewInfo);
var
  AStates: TcxCalendarElementStates;
begin
  AStates := AViewInfo.GetDrawStates;
  if [cesSelected, cesHot, cesMarked] * AStates <> [] then
    LookAndFeelPainter.DrawModernCalendarDateCellSelection(ACanvas, AViewInfo.Bounds, AStates);
end;

procedure TcxCalendarModernPainter.DrawDateHeaderCell(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateHeaderCellViewInfo);
begin
  ACanvas.Font := AViewInfo.Font;
  LookAndFeelPainter.DrawModernCalendarDateHeaderSelection(ACanvas,
    cxRectInflate((AViewInfo.TextRect), LookAndFeelPainter.GetModernCalendarHeaderTextOffsets), AViewInfo.GetDrawStates);
end;

procedure TcxCalendarModernPainter.DrawDateHeaderCellText(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarDateHeaderCellViewInfo);
begin
  ACanvas.Font := AViewInfo.Font;
  ACanvas.Font.Color := LookAndFeelPainter.GetModernCalendarDateHeaderTextColor(AViewInfo.GetDrawStates);
  cxDrawText(ACanvas, AViewInfo.GetText, AViewInfo.TextRect, DT_WORDBREAK or DT_EDITCONTROL);
end;

procedure TcxCalendarModernPainter.DrawHeader(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarHeaderViewInfo);
begin
{$ifdef colorized}
  ACanvas.FillRect(AViewInfo.Bounds, clGray);
{$endif}
  ACanvas.Font := AViewInfo.Font;
  LookAndFeelPainter.DrawModernCalendarHeaderSelection(ACanvas, cxRectInflate(AViewInfo.TextRect, AViewInfo.TextOffsets),
    [AViewInfo.State]);
end;

procedure TcxCalendarModernPainter.DrawHeaderText(ACanvas: TcxCanvas;
  AViewInfo: TcxCalendarHeaderViewInfo);
begin
  ACanvas.Font.Color := LookAndFeelPainter.GetModernCalendarHeaderTextColor([AViewInfo.State]);
  cxDrawText(ACanvas, AViewInfo.GetText, AViewInfo.TextRect, DT_WORDBREAK or DT_EDITCONTROL);
end;

function TcxCalendarModernPainter.GetCellTextColor(
  ADrawStates: TcxCalendarElementStates): TColor;
begin
  Result := LookAndFeelPainter.GetModernCalendarCellTextColor(ADrawStates);
end;

{ TcxCustomCalendar }

constructor TcxCustomCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsViewInfoDirty := True;
  ControlStyle := [csCaptureMouse, csOpaque];
  FRealView := GetRealView;
  RecreateView;
  FAnimation := True;
  FArrowsForYear := True;
  FSelectDate := Date;
  FFirstDate := dxChangeDay(FSelectDate, 1);
  Keys := [kAll, kArrows, kChars, kTab];
  FKind := ckDate;
  SetCalendarButtons([btnClear, btnToday]);
  FFlat := True;
  FYearsInMonthList := True;
end;

destructor TcxCustomCalendar.Destroy;
begin
  EndMouseTracking(Self);
  DestroySubclasses;
  inherited Destroy;
end;

procedure TcxCustomCalendar.BeginUpdate;
begin
  Inc(FLockCount);
end;

function TcxCustomCalendar.CanFocus: Boolean;
begin
  Result := FController.CanFocus;
end;

procedure TcxCustomCalendar.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

procedure TcxCustomCalendar.TranslationChanged;
begin
  FViewInfo.TranslationChanged;
  Changed;
end;

procedure TcxCustomCalendar.ButtonClick(Sender: TObject);
var
  ADate: TDateTime;
begin
  case TCalendarButton(Integer(TcxButton(Sender).Tag)) of
    btnNow: ADate := Now;
    btnToday: ADate := Date + cxSign(Date) * Time;
    btnClear: ADate := NullDate;
    btnCancel:
     begin
       Hide(crCancel);
       Exit;
     end
  else
    ADate := SelectDate + cxSign(SelectDate) * Time;
  end;
 // FClock.Time := TTime(dxTimeOf(ADate));
  Time := TTime(dxTimeOf(ADate));
  InternalSetSelectDate(ADate);
  DoDateTimeChanged;
  Hide(crEnter);
end;

procedure TcxCustomCalendar.CreateSubclasses;
begin
  FPainter := CreatePainter;
  FViewInfo := CreateViewInfo;
  FController := CreateController;
end;

procedure TcxCustomCalendar.CorrectHeaderTextRect(var R: TRect);
begin
    if Kind = ckDateTime then
      Inc(R.Top)
    else
      Inc(R.Top, Integer(not Flat));
    Dec(R.Bottom);
end;

procedure TcxCustomCalendar.DestroySubclasses;
begin
  FreeAndNil(FController);
  FreeAndNil(FViewInfo);
  FreeAndNil(FPainter);
end;

procedure TcxCustomCalendar.DoDateTimeChanged;
begin
  if Assigned(FOnDateTimeChanged) then FOnDateTimeChanged(Self);
end;

function TcxCustomCalendar.GetBackgroundColor: TColor;
begin
  Result := GetDateNavigatorContentColor(LookAndFeelPainter, Color);
end;

function TcxCustomCalendar.GetCalendarTable: TcxCustomCalendarTable;
begin
  Result := cxGetLocalCalendar;
end;

function TcxCustomCalendar.GetContainerBorderColor: TColor;
begin
  Result := LookAndFeel.Painter.GetContainerBorderColor(False);
end;

function TcxCustomCalendar.GetDateHeaderFrameColor: TColor;
begin
  if LookAndFeelPainter.LookAndFeelStyle = lfsNative then
    Result := Color
  else
    Result := clBtnText;
end;

function TcxCustomCalendar.GetDateFromCell(X, Y: Integer): Double;
begin
  Result := FirstDate - dxDayOfWeekOffset(FirstDate) + Y * 7 + X;
  if (dxDayOfWeekOffset(FirstDate) = 0) and (FirstDate > MinDateTime) then
    Result := Result - 7;
end;

function TcxCustomCalendar.GetDateTimeHeaderFrameColor: TColor;
begin
  if LookAndFeelPainter.LookAndFeelStyle = lfsOffice11 then
    Result := Color
  else
    Result := clBtnShadow;
end;

function TcxCustomCalendar.GetHeaderColor: TColor;
begin
  Result := LookAndFeelPainter.DefaultDateNavigatorHeaderColor;
end;

function TcxCustomCalendar.GetHeaderOffset: TRect;
begin
  if (Kind = ckDate) and (LookAndFeelPainter.LookAndFeelStyle = lfsOffice11) then
    Result := Rect(Office11HeaderOffset, Office11HeaderOffset, Office11HeaderOffset, 0)
  else
    Result := cxEmptyRect;
end;

function TcxCustomCalendar.IsDateInRange(ADate: Double): Boolean;
begin
  Result := cxIsDateValid(ADate) and (ADate >= MinDate) and (ADate <= MaxDate);
end;

procedure TcxCustomCalendar.RecreateView;
begin
  DestroySubclasses;
  CreateSubclasses;
  if HandleAllocated then
    ViewInfo.InitControls;
end;

procedure TcxCustomCalendar.SetArrowsForYear(Value: Boolean);
begin
  if Value <> FArrowsForYear then
  begin
    FArrowsForYear := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetCalendarButtons(Value: TDateButtons);
begin
  if Value <> FCalendarButtons then
  begin
    FCalendarButtons := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetKind(Value: TcxCalendarKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Changed;
    if Value = ckDate then
    begin
      ControlStyle := ControlStyle - [csDoubleClicks, csClickEvents];
      Keys := Keys + [kTab];
    end
    else
    begin
      ControlStyle := ControlStyle + [csDoubleClicks, csClickEvents];
      Keys := Keys - [kTab];
    end;
  end;
end;

procedure TcxCustomCalendar.SetMaxDate(Value: Double);
begin
  if Value <> FMaxDate then
  begin
    FMaxDate := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetMinDate(Value: Double);
begin
  if Value <> FMinDate then
  begin
    FMinDate := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetShowToday(const Value: Boolean);
begin
  if FShowToday <> Value then
  begin
    FShowToday := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetTime(const Value: TTime);
begin
  if FTime <> Value then
  begin
    InternalSetTime(Value);
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetTimeFormat(Value: TcxTimeEditTimeFormat);
begin
  if Value <> TimeFormat then
  begin
    FTimeFormat := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetUse24HourFormat(Value: Boolean);
begin
  if Value <> Use24HourFormat then
  begin
    FUse24HourFormat := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.SetView(const Value: TcxCalendarView);
begin
  if FView <> Value then
  begin
    FView := Value;
    CheckRealView;
  end;
end;

procedure TcxCustomCalendar.SetWeekNumbers(Value: Boolean);
begin
  if Value <> FWeekNumbers then
  begin
    FWeekNumbers := Value;
    Changed;
  end;
end;

procedure TcxCustomCalendar.FocusChanged;
begin
  inherited FocusChanged;
  Invalidate;
end;

procedure TcxCustomCalendar.FontChanged;
begin
  inherited FontChanged;
  Changed;
end;

procedure TcxCustomCalendar.DblClick;
begin
  inherited DblClick;
  FController.DblClick;
end;

function TcxCustomCalendar.HasBackground: Boolean;
begin
  if FViewInfo <> nil then
    Result := FViewInfo.HasBackground
  else
    Result := False;
end;

procedure TcxCustomCalendar.InitControl;
begin
  inherited InitControl;
  ViewInfo.InitControls;
  FontChanged;
end;

function TcxCustomCalendar.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
  if not Result and FController.IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) then
    FController.MouseWheel(Shift, WheelDelta, MousePos);
end;

function TcxCustomCalendar.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TcxCustomCalendar.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) or
    FController.IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos);
end;

procedure TcxCustomCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FController.KeyDown(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TcxCustomCalendar.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  CheckRealView;
end;

procedure TcxCustomCalendar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FController.MouseDown(Button, Shift, X, Y);
end;

procedure TcxCustomCalendar.MouseEnter(AControl: TControl);
begin
  inherited MouseEnter(AControl);
 // CheckHotTrack;
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxCustomCalendar.MouseLeave(AControl: TControl);
begin
  inherited MouseLeave(AControl);
  FController.MouseLeave;
  EndMouseTracking(Self);
end;

procedure TcxCustomCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  BeginMouseTracking(Self, GetControlRect(Self), Self);
  FController.MouseMove(Shift, X, Y);
end;

procedure TcxCustomCalendar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FController.MouseUp(Button, Shift, X, Y);
end;

function TcxCustomCalendar.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TcxCustomCalendar.Paint;
begin
  FViewInfo.Paint(Canvas);
end;

procedure TcxCustomCalendar.Calculate;
begin
  FViewInfo.Initialize;
  FViewInfo.CalculateSize;
  FIsSizeCalculated := True;
  SetBounds(Left, Top, FViewInfo.Size.cx, FViewInfo.Size.cy);
  FViewInfo.CalculateBounds;
  if UseRightToLeftAlignment then
    FViewInfo.RightToLeftConversion(Bounds);
end;

function TcxCustomCalendar.CalculateNextDate(ADate: Double; AArrow: TcxCalendarArrow): Double;
begin
  case AArrow of
    caPrevMonth:
      Result := CalendarTable.AddMonths(ADate, -1);
    caNextMonth:
      Result := CalendarTable.AddMonths(ADate, 1);
    caPrevYear:
      Result := CalendarTable.AddYears(ADate, -1);
    caNextYear:
      Result := CalendarTable.AddYears(ADate, 1);
    caPrevDecade:
      Result := CalendarTable.AddYears(ADate, -10);
    caNextDecade:
      Result := CalendarTable.AddYears(ADate, 10);
    caPrevCentury:
      Result := CalendarTable.AddYears(ADate, -100);
    caNextCentury:
      Result := CalendarTable.AddYears(ADate, 100);
  else
    Result := NullDate;
  end;
end;

function TcxCustomCalendar.CanAnimate: Boolean;
begin
  Result := not IsDesigning and not IsLocked and Animation;
end;

function TcxCustomCalendar.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := FIsSizeCalculated;
  if Result then
  begin
    NewWidth := FViewInfo.Size.cx;
    NewHeight := FViewInfo.Size.cy;
  end;
end;

procedure TcxCustomCalendar.Changed;
begin
  FIsViewInfoDirty := True;
  FIsSizeCalculated := False;
  if not IsLocked then
  begin
    Calculate;
    Invalidate;
    FIsViewInfoDirty := False;
  end;
end;

function TcxCustomCalendar.CanShowDate(ADate: Double): Boolean;
begin
  Result := not IsShowOnlyValidDates or IsDateInRange(ADate);
end;

function TcxCustomCalendar.CanShowMonth(ADate: Double): Boolean;
begin
  Result := not IsShowOnlyValidDates or IsMonthInRange(ADate);
end;

function TcxCustomCalendar.CanShowYear(ADate: Double): Boolean;
begin
  Result := not IsShowOnlyValidDates or IsYearInRange(ADate);
end;

procedure TcxCustomCalendar.CheckChanges;
begin
  if FIsViewInfoDirty then
    Changed;
end;

procedure TcxCustomCalendar.CheckRealView;
var
  AView: TcxCalendarView;
begin
  AView := GetRealView;
  if FRealView <> AView then
  begin
    FRealView := AView;
    RecreateView;
    Changed;
  end;
end;

function TcxCustomCalendar.CreateController: TcxCalendarController;
begin
  case FRealView of
    cavClassic:
      Result := TcxCalendarClassicController.Create(Self);
    cavTouchUI:
      Result := TcxCalendarTouchUIController.Create(Self)
  else // cavModern
    Result := TcxCalendarModernController.Create(Self);
  end;
end;

function TcxCustomCalendar.CreatePainter: TcxCalendarPainter;
begin
  case FRealView of
    cavClassic:
      Result := TcxCalendarPainter.Create(Self);
    cavTouchUI:
      Result := TcxCalendarTouchUIPainter.Create(Self)
  else // cavModern
    Result := TcxCalendarModernPainter.Create(Self);
  end;
end;

function TcxCustomCalendar.CreateViewInfo: TcxCalendarViewInfo;
begin
  case FRealView of
    cavClassic:
      Result := TcxCalendarClassicViewInfo.Create(Self);
    cavTouchUI:
      Result := TcxCalendarTouchUIViewInfo.Create(Self)
  else // cavModern
    Result := TcxCalendarModernViewInfo.Create(Self);
  end;
end;

procedure TcxCustomCalendar.DoStep(AArrow: TcxCalendarArrow);
var
  AFirstDate: Double;
begin
  FirstDate := InvalidDate;
  AFirstDate := CalculateNextDate(FirstDate, AArrow);
  if IsShowOnlyValidDates then
  begin
    if CanShowYear(AFirstDate) then
      FirstDate := Min(Max(AFirstDate, MinDate), MaxDate);
  end
  else
    FirstDate := AFirstDate;
end;

function TcxCustomCalendar.GetButtonsRegionOffset: Integer;
begin
  Result := GetFontSize(Font) div 2;
end;

function TcxCustomCalendar.GetLastDate: Double;
begin
  Result := dxGetEndDateOfMonth(FirstDate, True);
end;

function TcxCustomCalendar.GetMonthCalendarOffset: TPoint;
begin
  Result.X := MulDiv(GetFontSize(Font), 3, 4);
  Result.Y := Result.X;
end;

function TcxCustomCalendar.GetRealFirstDate: Double;
var
  ACol: Integer;
begin
  Result := FirstDate;
  ACol := dxDayOfWeekOffset(FirstDate);
  if ACol = 0 then
    Result := Result - 7
  else
    Result := Result - ACol;
end;

function TcxCustomCalendar.GetRealLastDate: Double;
var
  Year, Month, Day: Word;
  ACol: Integer;
begin
  Result := GetLastDate;
  DecodeDate(Result, Year, Month, Day);
  ACol := dxDayOfWeekOffset(EncodeDate(Year, Month, 1));
  Result := Result + 6 * 7 - DaysPerMonth(Year, Month) - ACol;
  if ACol = 0 then Result := Result - 7;
end;

function TcxCustomCalendar.GetRealView: TcxCalendarView;
begin
  Result := GetCalendarRealView(FView);
end;

function TcxCustomCalendar.GetSize: TSize;
begin
  Result := FViewInfo.Size;
end;

procedure TcxCustomCalendar.Hide(AReason: TcxEditCloseUpReason);
begin
  HidePopup(Self, AReason);
end;

procedure TcxCustomCalendar.HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  FController.HidePopup(Sender, AReason);
end;

procedure TcxCustomCalendar.InternalSetFirstDate(Value: Double);
begin
  FFirstDate := Value;
end;

procedure TcxCustomCalendar.InternalSetSelectDate(Value: Double);
begin
  FSelectDate := Value;
end;

procedure TcxCustomCalendar.InternalSetSelectDate(Value: Double;
  ARepositionVisibleDates: Boolean);
begin
  FViewInfo.SetSelectDate(Value, ARepositionVisibleDates, CanAnimate);
end;

procedure TcxCustomCalendar.InternalSetTime(const Value: TTime);
begin
  FTime := Value;
end;

function TcxCustomCalendar.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsLoading or IsDestroying or not HandleAllocated;
end;

function TcxCustomCalendar.IsMonthInRange(ADate: Double): Boolean;
begin
  Result := (CalendarTable.GetLastDayOfMonth(ADate) >= MinDate) and
    (CalendarTable.GetFirstDayOfMonth(ADate) <= MaxDate);
end;

function TcxCustomCalendar.IsPopup: Boolean;
begin
  Result := False;
end;

function TcxCustomCalendar.IsShowOnlyValidDates: Boolean;
begin
  Result := ShowOnlyValidDates and (MinDate <> MaxDate)
end;

function TcxCustomCalendar.IsYearInRange(ADate: Double): Boolean;
begin
  Result := (CalendarTable.GetLastDayOfYear(ADate) >= MinDate) and
    (CalendarTable.GetFirstDayOfYear(ADate) <= MaxDate);
end;

procedure TcxCustomCalendar.SetFirstDate(Value: Double);
begin
  FViewInfo.SetFirstDate(Value, CanAnimate);
end;

procedure TcxCustomCalendar.SetSelectDate(Value: Double);
begin
  InternalSetSelectDate(Value, True);
end;

procedure TcxCustomCalendar.StepToFuture;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FirstDate, Year, Month, Day);
  dxIncMonth(Year, Month);
  FirstDate := EncodeDate(Year, Month, 1);
end;

procedure TcxCustomCalendar.StepToPast;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FirstDate, Year, Month, Day);
  dxDecMonth(Year, Month);
  FirstDate := EncodeDate(Year, Month, 1);
end;

// IcxMouseTrackingCaller
procedure TcxCustomCalendar.MouseTrackingMouseLeave;
begin
  FController.MouseLeave;
  EndMouseTracking(Self);
end;
// IcxMouseTrackingCaller 2
function TcxCustomCalendar.PtInCaller(const P: TPoint): Boolean;
begin
  Result := cxRectPtIn(Bounds, P);
end;
// IcxMouseTrackingCaller 3
function TcxCustomCalendar.IsCaptureMouse: Boolean;
begin
  Result := MouseCapture;
end;

{ TcxPopupCalendar }

procedure TcxPopupCalendar.HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  inherited HidePopup(Sender, AReason);
  if Assigned(FOnHidePopup) then FOnHidePopup(Self, AReason);
end;

procedure TcxPopupCalendar.Initialize;
begin
  FViewInfo.Reset;
end;

function TcxPopupCalendar.IsPopup: Boolean;
begin
  Result := True;
end;

procedure TcxPopupCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_F4:
      if not (ssAlt in Shift) then
        Hide(crClose);
    VK_UP, VK_DOWN:
      if Shift = [ssAlt] then
        Hide(crClose);
    VK_TAB:
      if TcxCustomDateEditProperties(Edit.ActiveProperties).Kind = ckDate then
      begin
        if Edit.ActiveProperties.PostPopupValueOnTab then
          DoDateTimeChanged;
        Edit.DoEditKeyDown(Key, Shift);
      end;
  end;
end;

procedure TcxPopupCalendar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
//  if FTimer.Enabled then Exit;
//  if (ssLeft in Shift) and not FController.HitTest.HitAtBackground then
//  begin
//    FSelectDate := TcxCustomDateEdit(Edit).Date;
//    FViewInfo.FDateViewInfo.Invalidate;
//    FViewInfo.FHeaderViewInfo.Invalidate;
//  end;
end;

{ TcxDateEditPropertiesValues }

procedure TcxDateEditPropertiesValues.Assign(Source: TPersistent);
begin
  if Source is TcxDateEditPropertiesValues then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with TcxDateEditPropertiesValues(Source) do
      begin
        Self.DateButtons := DateButtons;
        Self.InputKind := InputKind;
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxDateEditPropertiesValues.RestoreDefaults;
begin
  BeginUpdate;
  try
    inherited RestoreDefaults;
    DateButtons := False;
    InputKind := False;
  finally
    EndUpdate;
  end;
end;

function TcxDateEditPropertiesValues.GetMaxDate: Boolean;
begin
  Result := MaxValue;
end;

function TcxDateEditPropertiesValues.GetMinDate: Boolean;
begin
  Result := MinValue;
end;

function TcxDateEditPropertiesValues.IsMaxDateStored: Boolean;
begin
  Result := IsMaxValueStored;
end;

function TcxDateEditPropertiesValues.IsMinDateStored: Boolean;
begin
  Result := IsMinValueStored;
end;

procedure TcxDateEditPropertiesValues.SetDateButtons(Value: Boolean);
begin
  if Value <> FDateButtons then
  begin
    FDateButtons := Value;
    Changed;
  end;
end;

procedure TcxDateEditPropertiesValues.SetInputKind(Value: Boolean);
begin
  if Value <> FInputKind then
  begin
    FInputKind := Value;
    Changed;
  end;
end;

procedure TcxDateEditPropertiesValues.SetMaxDate(Value: Boolean);
begin
  MaxValue := Value;
end;

procedure TcxDateEditPropertiesValues.SetMinDate(Value: Boolean);
begin
  MinValue := Value;
end;

{ TcxCustomDateEditProperties }

constructor TcxCustomDateEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAnimation := True;
  FArrowsForYear := True;
  FDateOnError := deNoChange;
  FShowOnlyValidDates := False;
  FShowToday := True;
  CaseInsensitive := True;
  FKind := ckDate;
  FSaveTime := True;
  FShowTime := True;
  FYearsInMonthList := True;
  FView := cavDefault;
  ImmediateDropDownWhenKeyPressed := False;
  PopupSizeable := False;
  BuildEditMask;
end;

procedure TcxCustomDateEditProperties.BuildEditMask;
begin
  LockUpdate(True);
  try
    if EditFormat = '' then
    case InputKind of
      ikMask:
        begin
          MaskKind := emkStandard;
          if Kind = ckDateTime then
            EditMask := cxFormatController.StandardDateTimeEditMask
          else
            EditMask := cxFormatController.StandardDateEditMask;
        end;
      ikRegExpr:
        begin
          MaskKind := emkRegExprEx;
          if Kind = ckDateTime then
            EditMask := cxFormatController.RegExprDateTimeEditMask
          else
            EditMask := cxFormatController.RegExprDateEditMask;
        end;
      else
        EditMask := '';
    end
    else
    begin
      MaskKind := emkRegExprEx;
      EditMask := cxFormatController.RegExprCustomDateEditMask(EditFormat);
    end;
  finally
    LockUpdate(False);
  end;
end;

function TcxCustomDateEditProperties.GetAssignedValues: TcxDateEditPropertiesValues;
begin
  Result := TcxDateEditPropertiesValues(FAssignedValues);
end;

function TcxCustomDateEditProperties.GetDateButtons: TDateButtons;
begin
  if AssignedValues.DateButtons then
    Result := FDateButtons
  else
    Result := GetDefaultDateButtons;
end;

function TcxCustomDateEditProperties.GetDefaultDateButtons: TDateButtons;
begin
  Result := [btnClear];
  if Kind = ckDateTime then
    Include(Result, btnNow)
  else
    if GetCalendarRealView(View) = cavClassic then
      Include(Result, btnToday);
end;

function TcxCustomDateEditProperties.GetDefaultInputKind: TcxInputKind;
begin
  if Kind = ckDate then
    Result := ikRegExpr
  else
    Result := ikMask;
end;

function TcxCustomDateEditProperties.GetInputKind: TcxInputKind;
begin
  if not cxIsGregorianCalendar then
    Result := ikStandard
  else
  begin
    if AssignedValues.EditFormat and (Length(EditMask) > 0) then
      Result := ikRegExpr
    else
    begin
      if AssignedValues.InputKind then
        Result := FInputKind
      else
        Result := GetDefaultInputKind;
    end;
  end;
end;

function TcxCustomDateEditProperties.GetMaxDate: TDateTime;
begin
  Result := inherited MaxValue;
end;

function TcxCustomDateEditProperties.GetMinDate: TDateTime;
begin
  Result := inherited MinValue;
end;

procedure TcxCustomDateEditProperties.CalendarGetDayOfWeekStateEventHandler(Sender: TObject; ADayOfWeek: TDay; AState: TCustomDrawState; AFont: TFont;
  var ABackgroundColor: TColor);
begin
  if Assigned(FOnGetDayOfWeekState) then
    FOnGetDayOfWeekState(Owner, ADayOfWeek, AState, AFont, ABackgroundColor);
end;

procedure TcxCustomDateEditProperties.CalendarGetDayStateEventHandler(Sender: TObject;
  ADate: TDateTime; AState: TCustomDrawState; AFont: TFont; var ABackgroundColor: TColor);
begin
  if Assigned(FOnGetDayState) then
    FOnGetDayState(Owner, ADate, AState, AFont, ABackgroundColor);
end;

function TcxCustomDateEditProperties.IsDateButtonsStored: Boolean;
begin
  Result := AssignedValues.DateButtons;
end;

function TcxCustomDateEditProperties.IsInputKindStored: Boolean;
begin
  Result := AssignedValues.InputKind;
end;

function TcxCustomDateEditProperties.NeedShowTime(ADate: TDateTime;
  AIsFocused: Boolean): Boolean;
begin
  if AIsFocused then
  begin
    if Kind = ckDateTime then
      Result := not((dxTimeOf(ADate) = 0) and (InputKind <> ikMask))
    else
      Result := ShowTime and (dxTimeOf(ADate) <> 0) and (InputKind = ikStandard);
  end
  else
    Result := not((Kind = ckDate) and not ShowTime);
end;

procedure TcxCustomDateEditProperties.SetAnimation(Value: Boolean);
begin
  if Value <> FAnimation then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetArrowsForYear(Value: Boolean);
begin
  if Value <> FArrowsForYear then
  begin
    FArrowsForYear := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetAssignedValues(
  Value: TcxDateEditPropertiesValues);
begin
  FAssignedValues.Assign(Value);
end;

procedure TcxCustomDateEditProperties.SetDateButtons(Value: TDateButtons);
begin
  if AssignedValues.DateButtons and (Value = FDateButtons) then
    Exit;

  AssignedValues.FDateButtons := True;
  FDateButtons := Value;
  Changed;
end;

procedure TcxCustomDateEditProperties.SetDateOnError(Value: TDateOnError);
begin
  if Value <> FDateOnError then
  begin
    FDateOnError := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetInputKind(Value: TcxInputKind);
begin
  if AssignedValues.InputKind and (Value = FInputKind) then
    Exit;
  AssignedValues.FInputKind := True;
  FInputKind := Value;
  Changed;
end;

procedure TcxCustomDateEditProperties.SetKind(Value: TcxCalendarKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetMaxDate(Value: TDateTime);
begin
  inherited MaxValue := Value;
end;

procedure TcxCustomDateEditProperties.SetMinDate(Value: TDateTime);
begin
  inherited MinValue := Value;
end;

procedure TcxCustomDateEditProperties.SetSaveTime(Value: Boolean);
begin
  if Value <> FSaveTime then
  begin
    FSaveTime := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetShowTime(Value: Boolean);
begin
  if Value <> FShowTime then
  begin
    FShowTime := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetShowToday(const Value: Boolean);
begin
  if Value <> FShowToday then
  begin
    FShowToday := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetView(const Value: TcxCalendarView);
begin
  if Value <> FView then
  begin
    FView := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetWeekNumbers(Value: Boolean);
begin
  if Value <> FWeekNumbers then
  begin
    FWeekNumbers := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.SetYearsInMonthList(Value: Boolean);
begin
  if Value <> FYearsInMonthList then
  begin
    FYearsInMonthList := Value;
    Changed;
  end;
end;

procedure TcxCustomDateEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomDateEditProperties then
    with TcxCustomDateEditProperties(AProperties) do
    begin
      Self.Animation := Animation;
      Self.ArrowsForYear := ArrowsForYear;

      Self.AssignedValues.DateButtons := False;
      if AssignedValues.DateButtons then
        Self.DateButtons := DateButtons;

      Self.DateOnError := DateOnError;

      Self.AssignedValues.InputKind := False;
      if AssignedValues.InputKind then
        Self.InputKind := InputKind;

      Self.Kind := Kind;
      Self.SaveTime := SaveTime;
      Self.ShowTime := ShowTime;
      Self.ShowToday := ShowToday;
      Self.View := View;
      Self.WeekNumbers := WeekNumbers;
      Self.YearsInMonthList := YearsInMonthList;

      Self.AssignedValues.MaxDate := False;
      if AssignedValues.MaxDate then
        Self.MaxDate := MaxDate;
      Self.AssignedValues.MinDate := False;
      if AssignedValues.MinDate then
        Self.MinDate := MinDate;
      Self.ShowOnlyValidDates := ShowOnlyValidDates;

      Self.OnGetDayOfWeekState := OnGetDayOfWeekState;
      Self.OnGetDayState := OnGetDayState;
    end;
end;

procedure TcxCustomDateEditProperties.DoChanged;
begin
  BeginUpdate;
  try
    BuildEditMask;
  finally
    EndUpdate(False);
  end;
  inherited;
end;

function TcxCustomDateEditProperties.GetAlwaysPostEditValue: Boolean;
begin
  Result := True;
end;

class function TcxCustomDateEditProperties.GetAssignedValuesClass: TcxCustomEditPropertiesValuesClass;
begin
  Result := TcxDateEditPropertiesValues;
end;

function TcxCustomDateEditProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [dfoSupports, dfoNoCurrencyValue];
end;

function TcxCustomDateEditProperties.GetModeClass(
  AMaskKind: TcxEditMaskKind): TcxMaskEditCustomModeClass;
begin
  if AMaskKind = emkStandard then
    Result := TcxDateEditMaskStandardMode
  else
    Result := inherited GetModeClass(AMaskKind);
end;

class function TcxCustomDateEditProperties.GetPopupWindowClass: TcxCustomEditPopupWindowClass;
begin
  Result := TcxDateEditPopupWindow;
end;

function TcxCustomDateEditProperties.IsEditValueEmpty(const AEditValue: TcxEditValue): Boolean;
begin
  Result := VarIsNull(AEditValue) or VarIsNullDate(AEditValue);
end;

function TcxCustomDateEditProperties.IsEditValueNumeric: Boolean;
begin
  Result := True;
end;

function TcxCustomDateEditProperties.IsValueBoundDefined(
  ABound: TcxEditValueBound): Boolean;
begin
  if (MinValue <> 0) and (MaxValue <> 0) then
    Result := MinValue < MaxValue
  else
    if ABound = evbMin then
      Result := MinValue <> 0
    else
      Result := MaxValue <> 0;
end;

function TcxCustomDateEditProperties.IsValueBoundsDefined: Boolean;
begin
  if (MinValue <> 0) and (MaxValue <> 0) then
    Result := MinValue < MaxValue
  else
    Result := (MinValue <> 0) or (MaxValue <> 0);
end;

function TcxCustomDateEditProperties.PopupWindowAcceptsAnySize: Boolean;
begin
  Result := False;
end;

function TcxCustomDateEditProperties.GetEmptyDisplayValue(
  AEditFocused: Boolean): string;
var
  ATimeStringLength: Integer;
  S: string;
begin
  Result := GetEmptyString;
  if AEditFocused and (Kind = ckDateTime) and (InputKind = ikMask) and
    (CompareText(EditMask, cxFormatController.StandardDateTimeEditMask) = 0) and
    not cxFormatController.AssignedStandardDateTimeEditMask then
  begin
    S := DateTimeToTextEx(0, True, True);
    ATimeStringLength := cxFormatController.GetDateTimeStandardMaskStringLength(
      cxFormatController.TimeFormatInfo);
    Delete(Result, Length(Result) - ATimeStringLength + 1, ATimeStringLength);
    Result := Result + Copy(S, Length(S) - ATimeStringLength + 1, ATimeStringLength);
  end;
end;

function TcxCustomDateEditProperties.GetStandardMaskBlank(APos: Integer): Char;
var
  ATimeZoneInfo: TcxTimeEditZoneInfo;
begin
  if not GetTimeZoneInfo(APos, ATimeZoneInfo) then
    Result := ' '
  else
    if ATimeZoneInfo.Kind = tzTimeSuffix then
      Result := #0
    else
      Result := '0';
end;

function TcxCustomDateEditProperties.GetTimeZoneInfo(APos: Integer;
  out AInfo: TcxTimeEditZoneInfo): Boolean;
const
  ATimeZoneKindMap: array[TcxDateTimeFormatItemKind] of TcxTimeEditZoneKind =
    (tzHour, tzHour, tzHour, tzHour, tzHour, tzMin, tzSec, tzHour, tzTimeSuffix,
      tzHour, tzHour);
var
  AItemInfo: TcxDateTimeFormatItemInfo;
begin
  Result := False;
  if (Kind <> ckDateTime) or (InputKind <> ikMask) or
    cxFormatController.AssignedStandardDateTimeEditMask then
      Exit;
  Result := cxFormatController.GetDateTimeFormatItemStandardMaskInfo(
    cxFormatController.DateTimeFormatInfo, APos, AItemInfo);
  Result := Result and (AItemInfo.Kind in [dtikHour, dtikMin, dtikSec, dtikTimeSuffix]);
  if Result then
  begin
    AInfo.Kind := ATimeZoneKindMap[AItemInfo.Kind];
    AInfo.Start := AItemInfo.ItemZoneStart;
    AInfo.Length := AItemInfo.ItemZoneLength;
    AInfo.TimeSuffixKind := AItemInfo.TimeSuffixKind;
    AInfo.Use24HourFormat := not cxFormatController.DateTimeFormatInfo.DefinedItems[dtikTimeSuffix];
  end;
end;

procedure TcxCustomDateEditProperties.InternalPrepareEditValue(ADisplayValue: string; out AEditValue: TcxEditValue);
var
  ADate: TDateTime;
begin
  AEditValue := Null;

  if not InternalCompareString(ADisplayValue, GetEmptyDisplayValue(True), True) then
  begin
    if TextToDateEx(ADisplayValue, ADate, EditFormat) then
      AEditValue := ADate
    else
      if DateOnError = deToday then
        if Kind = ckDate then
          AEditValue := SysUtils.Date
        else
          AEditValue := SysUtils.Now;
  end;
end;

class function TcxCustomDateEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxDateEdit;
end;

function TcxCustomDateEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := GetValueEditorEditValueSource(AEditFocused);
end;

function TcxCustomDateEditProperties.IsDisplayValueValid(var DisplayValue: TcxEditValue;
  AEditFocused: Boolean): Boolean;
var
  ADate: TDateTime;
begin
  // TODO optional symbols
  if InputKind = ikStandard then
    Result := TextToDateEx(VarToStr(DisplayValue), ADate)
  else
    Result := inherited IsDisplayValueValid(DisplayValue, AEditFocused);
end;

function TcxCustomDateEditProperties.IsEditValueValid(
  var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
var
  AValue: TcxEditValue;
begin
  if VarIsStr(EditValue) then
  begin
    try
      InternalPrepareEditValue(EditValue, AValue);
      Result := not VarIsNull(AValue) and not VarIsNullDate(AValue);
    except
      Result := False;
    end
  end
  else
    Result := VarIsSoftNull(EditValue) or VarIsDate(EditValue) or VarIsNumericEx(EditValue);
end;

function TcxCustomDateEditProperties.IsValueFormattedByProperties: Boolean;
begin
  Result := AssignedValues.DisplayFormat;
end;

procedure TcxCustomDateEditProperties.DoPrepareDisplayValue(
  const AEditValue: TcxEditValue; var ADisplayValue: TcxEditValue;
  AEditFocused: Boolean);

  function InternalPrepareDisplayValue(AEditValue: Variant): string;
  var
    AValue: TcxEditValue;
  begin
    if VarIsSoftNull(AEditValue) or VarIsNullDate(AEditValue) then
      Result := GetEmptyDisplayValue(AEditFocused)
    else
      if not(VarIsStr(AEditValue) or VarIsDate(AEditValue) or VarIsNumericEx(AEditValue)) then
        raise EConvertError.CreateFmt(cxGetResourceString(@cxSEditDateConvertError), [])
      else
      begin
        if VarIsStr(AEditValue) then
        begin
          InternalPrepareEditValue(AEditValue, AValue);
          if VarIsNull(AValue) or VarIsNullDate(AValue) then
            raise EConvertError.CreateFmt(cxGetResourceString(@cxSEditDateConvertError), [])
        end
        else
          AValue := AEditValue;
        if not NeedShowTime(AValue, AEditFocused) then
          AValue := dxDateOf(AValue);

        if AEditFocused then
          if EditFormat <> '' then
            DateTimeToString(Result, EditFormat, AValue)
          else
            Result := DateTimeToTextEx(AValue, InputKind = ikMask, Kind = ckDateTime)
        else
          if DisplayFormat <> '' then
            DateTimeToString(Result, DisplayFormat, AValue)
          else
            Result := DateTimeToTextEx(AValue, False);

        Result := TrimRight(Result);
      end;
  end;

begin
  ADisplayValue := InternalPrepareDisplayValue(AEditValue);
  if AEditFocused and IsMasked and (EditFormat = '') then
    inherited DoPrepareDisplayValue(ADisplayValue, ADisplayValue, AEditFocused);
end;

procedure TcxCustomDateEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption;
  var AError: Boolean; AEdit: TcxCustomEdit);

  function DateToDisplayValue(var ADate: TDateTime): TcxEditValue;
  begin
    Result := DateTimeToTextEx(ADate, InputKind = ikMask, Kind = ckDateTime);
    Result := TrimRight(Result);
  end;

  procedure ConvertToDate(const ADisplayValue: TcxEditValue;
    out ADate: TDateTime; out AError: Boolean);
  begin
    AError := not TextToDateEx(ADisplayValue, ADate, EditFormat);
  end;

  procedure CorrectError(var ADate: TDateTime; var AError: Boolean);
  begin
    if DateOnError = deNoChange then
      Exit;
    case DateOnError of
      deToday:
        if Kind = ckDate then
          ADate := SysUtils.Date
        else
          ADate := SysUtils.Now;
      deNull:
        ADate := NullDate;
    end;
    AError := False;
  end;

  function GetDate(var ADisplayValue: TcxEditValue; out AError: Boolean;
    out AErrorText: TCaption): TDateTime;
  // AError
  // NullDate - from CorrectError
  // ADisplayValue = GetEmptyString
  // Result
  var
    AIsUserErrorDisplayValue: Boolean;
  begin
    AError := False;
    AErrorText := cxGetResourceString(@cxSDateError);
    AIsUserErrorDisplayValue := False;
    try
      try
        if ADisplayValue <> GetEmptyDisplayValue(True) then
          ConvertToDate(ADisplayValue, Result, AError);
        if TcxCustomDateEdit(AEdit).IsOnValidateEventAssigned then
        begin
          if (ADisplayValue <> GetEmptyDisplayValue(True)) and not AError then
            ADisplayValue := DateToDisplayValue(Result(*, False*));
          DoValidate(ADisplayValue, AErrorText, AError, AEdit,
            AIsUserErrorDisplayValue);
          if not AError and (ADisplayValue <> GetEmptyDisplayValue(True)) then
            ConvertToDate(ADisplayValue, Result, AError);
        end
        else
          if AError then
            CorrectError(Result, AError);
      except
        AError := True;
      end;
    finally
      if AError and not AIsUserErrorDisplayValue then
        ADisplayValue := TcxCustomDateEdit(AEdit).DisplayText;
    end;
  end;

  procedure ValidateEmptyDisplayValue(AIsNullDate: Boolean);

    function SaveSavedTime: Boolean;
    begin
      Result := not AIsNullDate and (Kind = ckDate) and
        (TcxCustomDateEdit(AEdit).FSavedTime <> 0) and SaveTime;
    end;

  var
    ADateEdit: TcxCustomDateEdit;
    AEditValueChanged: Boolean;
  begin
    ADateEdit := TcxCustomDateEdit(AEdit);
    if SaveSavedTime then
    begin
      AEditValueChanged := not InternalVarEqualsExact(
        ADateEdit.FEditValue, ADateEdit.FSavedTime);
      ADateEdit.FEditValue := ADateEdit.FSavedTime;
      ADisplayValue := DateToDisplayValue(ADateEdit.FSavedTime);
    end
    else
    begin
      AEditValueChanged := not VarIsNull(ADateEdit.FEditValue);
      ADateEdit.FEditValue := Null;
      ADateEdit.FSavedTime := 0;
      ADisplayValue := GetEmptyDisplayValue(True);
    end;
    if AEditValueChanged then
      ADateEdit.DoEditValueChanged;
  end;

  procedure CheckDate(ADate: TDateTime; var AErrorText: TCaption;
    var AError: Boolean);
  begin
    if IsValueBoundsDefined then
      CheckEditValueBounds(ADate, AErrorText, AError, AEdit);
  end;

var
  ADate: TDateTime;
  ADateEdit: TcxCustomDateEdit;
begin
// TODO
//  if (TimeFormat = tfHour) and Use24HourFormat then
//    ADisplayValue := ADisplayValue + ':00';
  ADate := GetDate(ADisplayValue, AError, AErrorText);
  if AError then
    Exit;

  ADateEdit := TcxCustomDateEdit(AEdit);
  try
    try
      if (ADate = NullDate) or (ADisplayValue = GetEmptyDisplayValue(True)) then
        ValidateEmptyDisplayValue(ADate = NullDate)
      else
      begin
        CheckDate(ADate, AErrorText, AError);
        if AError then
          Exit;
        // support for time in the SmartInput

        if Kind = ckDate then
          if dxTimeOf(ADate) <> 0 then
            ADateEdit.FSavedTime := dxTimeOf(ADate)
          else
            if SaveTime then
            begin
              if ADate >= 0 then
                ADate := ADate + ADateEdit.FSavedTime
              else
                ADate := ADate - ADateEdit.FSavedTime;
            end
            else
              ADateEdit.FSavedTime := 0;
        ADisplayValue := ADate;
      end;
    except
      AError := True;
    end;
  finally
    if AError then
      ADisplayValue := TcxCustomDateEdit(AEdit).DisplayText;
  end;
end;

{ TcxDateEditPopupWindow }

constructor TcxDateEditPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  KeyPreview := True;
end;

procedure TcxDateEditPopupWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if IsPopupCalendarKey(Key, Shift) then
    TcxCustomDateEdit(Edit).Calendar.KeyDown(Key, Shift);
end;

function TcxDateEditPopupWindow.IsPopupCalendarKey(Key: Word;
  Shift: TShiftState): Boolean;

  function CanEscape: Boolean;
  var
    AContainer: TcxContainer;
  begin
    Result := ActiveControl is TcxButton;
    if not Result then
    begin
      AContainer := GetcxContainer(ActiveControl);
      Result := (AContainer is TcxTimeEdit) and not TcxTimeEdit(AContainer).ModifiedAfterEnter;
    end;
  end;

begin
  Result := (Key = VK_ESCAPE) and CanEscape or
  ((Key = VK_UP) or (Key = VK_DOWN)) and (ssAlt in Shift) or
  (Key = VK_F4) and not (ssAlt in Shift);
end;

{ TcxDateEditMaskStandardMode }

function TcxDateEditMaskStandardMode.GetBlank(APos: Integer): Char;
begin
  Result := TcxCustomDateEditProperties(Properties).GetStandardMaskBlank(APos);
end;

{ TcxCustomDateEdit }

destructor TcxCustomDateEdit.Destroy;
begin
  FreeAndNil(FCalendar);
  inherited Destroy;
end;

procedure TcxCustomDateEdit.Clear;
begin
  Date := NullDate;
end;

procedure TcxCustomDateEdit.DateChange(Sender: TObject);
var
  ADate: Double;
  ADisplayValue: TcxEditValue;
begin
  BeginUserAction;
  try
    FCloseUpReason := crEnter;
    ADate := Calendar.SelectDate;
    if ActiveProperties.Kind = ckDateTime then
      ADate := dxDateOf(ADate) + cxSign(ADate) * dxTimeOf(TDateTime(Calendar.Time))
    else
      if ActiveProperties.SaveTime and (ADate <> NullDate) then
        if ADate >= 0 then
          ADate := ADate + dxTimeOf(FDateDropDown)
        else
          ADate := ADate - dxTimeOf(FDateDropDown);
    ADisplayValue := GetRecognizableDisplayValue(ADate);
    if ((Date <> ADate) or not InternalCompareString(DisplayText, VarToStr(ADisplayValue), True)) and
      DoEditing then
    begin
      LockChangeEvents(True);
      try
        Date := ADate;
        ModifiedAfterEnter := True;
        SetInternalDisplayValue(ADisplayValue);
        SelectAll;
      finally
        LockChangeEvents(False);
      end;
    end;
  finally
    EndUserAction;
  end;
end;

function TcxCustomDateEdit.GetActiveProperties: TcxCustomDateEditProperties;
begin
  Result := inherited ActiveProperties as TcxCustomDateEditProperties;
end;

function TcxCustomDateEdit.GetCurrentDate: TDateTime;
begin
  if Focused and not IsEditValidated and ModifiedAfterEnter then
    Result := GetDateFromStr(DisplayText)
  else
    Result := Date;
end;

function TcxCustomDateEdit.GetProperties: TcxCustomDateEditProperties;
begin
  Result := TcxCustomDateEditProperties(inherited Properties);
end;

function TcxCustomDateEdit.GetRecognizableDisplayValue(
  ADate: TDateTime): TcxEditValue;

  function NeedDisplayValueCorrection(ADate: TDateTime): Boolean;
  var
    ADisplayValue, AEditValue: TcxEditValue;
  begin
    Result := False;
    if (ActiveProperties.InputKind = ikStandard) and (ADate <> NullDate) then
    begin
      PrepareDisplayValue(ADate, ADisplayValue, True);
      PrepareEditValue(ADisplayValue, AEditValue, True);
      Result := dxDateOf(ADate) <> dxDateOf(AEditValue);
    end;
  end;

begin
  if NeedDisplayValueCorrection(ADate) then
  begin
    if not ActiveProperties.NeedShowTime(ADate, True) then
      ADate := dxDateOf(ADate);
    Result := DateTimeToTextEx(ADate, False, ActiveProperties.Kind = ckDateTime, True);
    Result := TrimRight(Result);
  end
  else
    PrepareDisplayValue(ADate, Result, True);
end;

procedure TcxCustomDateEdit.SetProperties(Value: TcxCustomDateEditProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomDateEdit.CanSynchronizeModeText: Boolean;
begin
  Result := Focused or IsEditValidating;
end;

procedure TcxCustomDateEdit.CheckEditorValueBounds;
begin
  if Date = NullDate then
    Exit;
  BeginUserAction;
  try
    if ActiveProperties.IsValueBoundDefined(evbMin) and
      (Date < ActiveProperties.MinValue) then
        Date := ActiveProperties.MinValue
    else
      if ActiveProperties.IsValueBoundDefined(evbMax) and
        (Date > ActiveProperties.MaxValue) then
          Date := ActiveProperties.MaxValue;
  finally
    EndUserAction;
  end;
end;

procedure TcxCustomDateEdit.CreatePopupWindow;
begin
  inherited;
  PopupWindow.ModalMode := False;
end;

(*procedure TcxCustomDateEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);

  function IsArrowIncrementPosition: Boolean;
  begin
    Result := False;
    if (ActiveProperties.Kind <> ckDateTime) or (ActiveProperties.InputKind <> ikMask) then
      Exit;
    Result := SelStart + SelLength >= 11;
  end;

  procedure Increment(AButton: TcxSpinEditButton);
  var
    ATimeZone: TcxTimeEditZone;
  begin
    ATimeZone := GetTimeZone;
  end;

var
  AButton: TcxSpinEditButton;
begin
  if ((Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_NEXT) or (Key = VK_PRIOR)){ and
    not (ActiveProperties.UseCtrlIncrement and not (ssCtrl in Shift))} and
    IsArrowIncrementPosition then
  begin
    if not DataBinding.Modified and not DoEditing then
      Exit;
    case Key of
      VK_UP:
        AButton := sebForward;
      VK_DOWN:
        AButton := sebBackward;
      VK_PRIOR:
        AButton := sebFastForward;
      else
        AButton := sebFastBackward;
    end;
    Increment(AButton);
    DoAfterKeyDown(Key, Shift);
    Key := 0;
  end;
  if Key <> 0 then
    inherited DoEditKeyDown(Key, Shift);
end;*)

procedure TcxCustomDateEdit.DropDown;
begin
  if Calendar = nil then
    CreateCalendar;
  ActiveProperties.PopupControl := Calendar;
  inherited DropDown;
end;

procedure TcxCustomDateEdit.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csSetCaption];
  FDateDropDown := NullDate;
end;

procedure TcxCustomDateEdit.InitializePopupWindow;

  function GetSelectDate: TDateTime;
  begin
    if FDateDropDown = NullDate then
      Result := SysUtils.Date
    else
      Result := FDateDropDown;
    if ActiveProperties.ShowOnlyValidDates then
      Result := Min(Max(Result, Calendar.MinDate), Calendar.MaxDate);
  end;

var
  ADate: TDateTime;
  ATimeFormat: TcxTimeEditTimeFormat;
  AUse24HourFormat: Boolean;
begin
  inherited InitializePopupWindow;
  Calendar.BeginUpdate;
  try
    Calendar.HandleNeeded;
    Calendar.Initialize;
    Calendar.Color := ActiveStyle.Color;
    Calendar.Flat := PopupControlsLookAndFeel.Kind in [lfFlat, lfUltraFlat, lfOffice11];
    Calendar.CalendarButtons := ActiveProperties.DateButtons;
    Calendar.OnDateTimeChanged := nil;
    Calendar.ShowOnlyValidDates := ActiveProperties.ShowOnlyValidDates;
    if ActiveProperties.AssignedValues.MaxDate and (ActiveProperties.MaxDate <> NullDate) then
      Calendar.MaxDate := ActiveProperties.MaxDate
    else
      Calendar.MaxDate := MaxDateTime;

    if ActiveProperties.AssignedValues.MinDate and (ActiveProperties.MinDate <> NullDate) then
      Calendar.MinDate := ActiveProperties.MinDate
    else
      Calendar.MinDate := MinDateTime;

    FDateDropDown := CurrentDate;
    ADate := GetSelectDate;
    Calendar.MaxDate := dxDateOf(Calendar.MaxDate);
    Calendar.MinDate := dxDateOf(Calendar.MinDate);
    if ActiveProperties.Kind = ckDateTime then
      Calendar.Time := TTime(dxTimeOf(ADate));

    ADate := dxDateOf(ADate);
    Calendar.FirstDate := ADate;
    Calendar.SelectDate := ADate;

    Calendar.Font := VisibleFont;
    Calendar.Font.Color := clBtnText;
   // Calendar.SetSize; // force recalculate size  //#ch from first version why?
    Calendar.OnDateTimeChanged := DateChange;
    Calendar.ArrowsForYear := ActiveProperties.ArrowsForYear;
    Calendar.Kind := ActiveProperties.Kind;
    cxCalendar.GetTimeFormat(cxFormatController.DateTimeFormatInfo, ATimeFormat, AUse24HourFormat);
    Calendar.ShowToday := ActiveProperties.ShowToday;
    Calendar.TimeFormat := ATimeFormat;
    Calendar.Use24HourFormat := AUse24HourFormat;
    Calendar.WeekNumbers := ActiveProperties.WeekNumbers;
    Calendar.YearsInMonthList := ActiveProperties.YearsInMonthList;
    Calendar.View := ActiveProperties.View;
    Calendar.Animation := ActiveProperties.Animation;
  finally
    Calendar.EndUpdate;
  end;
end;

function TcxCustomDateEdit.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Text, Result, True);
end;

function TcxCustomDateEdit.InternalGetText: string;
begin
  Result := DisplayText;
end;

function TcxCustomDateEdit.InternalGetValueToValidate: TcxEditValue;
begin
  PrepareDisplayValue(EditValue, Result, True);
end;

procedure TcxCustomDateEdit.InternalSetEditValue(const Value: TcxEditValue;
  AValidateEditValue: Boolean);

  procedure SaveTime;
  var
    ADate: TDateTime;
  begin
    if not ActiveProperties.SaveTime or not(VarIsStr(Value) or VarIsNumericEx(Value) or VarIsDate(Value)) then
      FSavedTime := 0
    else
      if VarIsStr(Value) then
      begin
        cxStrToDateTime(Value, not cxFormatController.UseDelphiDateTimeFormats, ADate);
        if ADate = NullDate then
          FSavedTime := 0
        else
          FSavedTime := dxTimeOf(ADate);
      end
      else
        FSavedTime := dxTimeOf(Value);
  end;

begin
  if IsDestroying then
    Exit;
  if ActiveProperties.Kind = ckDate then
    SaveTime;
  inherited InternalSetEditValue(Value, AValidateEditValue);
end;

function TcxCustomDateEdit.InternalSetText(const Value: string): Boolean;
begin
  Result := SetDisplayText(Value);
end;

procedure TcxCustomDateEdit.InternalValidateDisplayValue(
  const ADisplayValue: TcxEditValue);
begin
  if VarIsStr(ADisplayValue) then
    inherited InternalValidateDisplayValue(ADisplayValue)
  else
  begin
    SaveModified;
    try
      InternalEditValue := ADisplayValue;
    finally
      RestoreModified;
    end;
  end;
end;

function TcxCustomDateEdit.IsCharValidForPos(var AChar: Char;
  APos: Integer): Boolean;
var
  ATimeZoneInfo: TcxTimeEditZoneInfo;
begin
  if not ActiveProperties.GetTimeZoneInfo(APos, ATimeZoneInfo) then
    Result := True
  else
    Result := IsCharValidForTimeEdit(Self, AChar, APos, ATimeZoneInfo);
end;

procedure TcxCustomDateEdit.PopupWindowClosed(Sender: TObject);
begin
  inherited PopupWindowClosed(Sender);
//  if Calendar <> nil then
//    Calendar.CheckHotTrack;
end;

procedure TcxCustomDateEdit.PopupWindowShowed(Sender: TObject);
begin
  inherited PopupWindowShowed(Sender);
  Calendar.CheckChanges;
//  Calendar.CheckHotTrack;
end;

procedure TcxCustomDateEdit.UpdateTextFormatting;
begin
end;

procedure TcxCustomDateEdit.CreateCalendar;
begin
  FCalendar := GetCalendarClass.Create(Self);
  FCalendar.FEdit := Self;
  FCalendar.Parent := PopupWindow;
  FCalendar.OnHidePopup := HidePopup;
  FCalendar.LookAndFeel.MasterLookAndFeel := PopupControlsLookAndFeel;
  FCalendar.OnGetDayState := ActiveProperties.CalendarGetDayStateEventHandler;
  FCalendar.OnGetDayOfWeekState := ActiveProperties.CalendarGetDayOfWeekStateEventHandler;
end;

function TcxCustomDateEdit.GetCalendarClass: TcxPopupCalendarClass;
begin
  Result := TcxPopupCalendar;
end;

function TcxCustomDateEdit.GetDate: TDateTime;
begin
  if VarIsNull(EditValue) then
    Result := NullDate
  else if VarIsNumericEx(EditValue) or VarIsDate(EditValue) then
    Result := EditValue
  else if VarIsStr(EditValue) then
    Result := GetDateFromStr(EditValue)
  else
    Result := NullDate;
end;

function TcxCustomDateEdit.GetDateFromStr(const S: string): TDateTime;
var
  AValue: TcxEditValue;
begin
  PrepareEditValue(S, AValue, Focused);
  if VarIsNull(AValue) then
    Result := NullDate
  else
    Result := AValue;
end;

procedure TcxCustomDateEdit.SetDate(Value: TDateTime);
begin
  if Value = NullDate then
    InternalEditValue := Null
  else
  begin
    if ActiveProperties.Kind = ckDate then
      if dxTimeOf(Value) = 0 then
        if ActiveProperties.SaveTime then
          Value := Int(Value) + cxSign(Value) * FSavedTime
        else
          Value := Int(Value);
    InternalEditValue := Value;
  end;
end;

procedure TcxCustomDateEdit.SetupPopupWindow;
begin
  inherited SetupPopupWindow;
  if ViewInfo.UseSkins then
    Calendar.LookAndFeel.SkinPainter := ViewInfo.Painter
  else
    Calendar.LookAndFeel.SkinPainter := nil;
end;

class function TcxCustomDateEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomDateEditProperties;
end;

procedure TcxCustomDateEdit.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
var
  ATempValue: TDateTime;
begin
  ActiveProperties.InternalPrepareEditValue(ADisplayValue, EditValue);
  if not VarIsNull(EditValue) and not VarIsNullDate(EditValue) and
    (ActiveProperties.Kind = ckDate) and ActiveProperties.SaveTime then
  begin
    ATempValue := Int(EditValue) + cxSign(EditValue) * FSavedTime;
    EditValue := ATempValue;
  end;
end;

{ TcxDateEdit }

class function TcxDateEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties;
end;

function TcxDateEdit.GetActiveProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(InternalGetActiveProperties);
end;

function TcxDateEdit.GetProperties: TcxDateEditProperties;
begin
  Result := TcxDateEditProperties(inherited Properties);
end;

procedure TcxDateEdit.SetProperties(Value: TcxDateEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterDateEditHelper }

class function TcxFilterDateEditHelper.GetFilterDataType(AValueTypeClass: TcxValueTypeClass): TcxFilterDataType;
begin
  Result := fdtDate;
end;

class function TcxFilterDateEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxDateEdit;
end;

class function TcxFilterDateEditHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoLess, fcoLessEqual, fcoGreater,
    fcoGreaterEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoBetween, fcoNotBetween, fcoInList, fcoNotInList,
      fcoYesterday, fcoToday, fcoTomorrow,
      fcoLast7Days, fcoLastWeek, fcoLast14Days, fcoLastTwoWeeks, fcoLast30Days, fcoLastMonth, fcoLastYear, fcoInPast,
      fcoThisWeek, fcoThisMonth, fcoThisYear,
      fcoNext7Days, fcoNextWeek, fcoNext14Days, fcoNextTwoWeeks, fcoNext30Days, fcoNextMonth, fcoNextYear, fcoInFuture];
end;

class procedure TcxFilterDateEditHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  with TcxCustomDateEditProperties(AProperties) do
  begin
    DateButtons := [btnToday, btnClear];
    DateOnError := deNull;
    InputKind := ikRegExpr;
    SaveTime := True;
  end;
end;

initialization
  GetRegisteredEditProperties.Register(TcxDateEditProperties, scxSEditRepositoryDateItem);
  FilterEditsController.Register(TcxDateEditProperties, TcxFilterDateEditHelper);

finalization
  FilterEditsController.Unregister(TcxDateEditProperties, TcxFilterDateEditHelper);

end.

