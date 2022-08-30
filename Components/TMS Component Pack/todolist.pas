{*************************************************************************}
{ TTodoList component                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ Copyright © 2001-2013                                                   }
{ Email : info@tmssoftware.com                                            }
{ Web : http://www.tmssoftware.com                                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit TodoList;

{                 
  With the USE_PLANNERDATEPICKER definition disabled, the component uses the
  Win32 date picker component for date editing. It's colors can be set using
  the EditColors.DefaultDateEditor property.

  With the USE_PLANNERDATEPICKER definition enabled, the component has a
  CalendarType property. The programmer can choose to use the Win32 date
  picker, or to use the TMS date picker.

  The colors of the TMS date picker can be set using the
  EditColors.PlannerDateEditor property.

  So, for editing the colors of the Win32 date picker, we use the
  EditColors.DefaultDateEditor property, and for editing the colors of
  the TMS date picker, we use the EditColors.PlannerDateEditor property.
}

//{$DEFINE USE_PLANNERDATEPICKER}

{$I TMSDEFS.INC}

{$R TODOLIST.RES}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  Forms, Spin, ComCtrls, Math, Mask, Buttons, ComObj, AdvStyleIF, ToDoXPVS,
  Clipbrd, Types
{$IFDEF USE_PLANNERDATEPICKER}
  , PlannerCal, PlannerDatePicker
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
  , Character
{$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  CHECKBOXSIZE = 14;

{$IFDEF DELPHI_UNICODE}
  STRSIZE = 2;
{$ENDIF}
{$IFNDEF DELPHI_UNICODE}
  STRSIZE = 1;
{$ENDIF}

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'May, 2013'; // Month version

  // version history
  // 1.2.3.0 : added MaxLength property in TodoItem
  //         : ImportFromCSV improved
  //         : ClearListOnImport property added
  //         : OnItemRightClick event added
  // 1.2.3.1 : Sort issue with category column fixed
  // 1.2.4.0 : Whidbey style added
  // 1.2.4.1 : Fixed issue with TotalTimeSuffix
  // 1.2.4.2 : Fixed issue with OnItemRightClick for empty todolist
  // 1.3.0.0 : New Style interface
  //         : New Office 2007 Luna & Obsidian styles
  // 1.3.0.1 : Fixed issue for SetFocus
  // 1.3.1.0 : New support for Office 2007 silver style added
  // 1.3.2.0 : New exposed Anchors property
  // 1.3.2.1 : Fixed issue with Item.Select for multiselect mode
  // 1.3.2.2 : Fixed issue with sorting on tdStatus, tdTotalTime
  //         : Improved : initial completiondate in inplace editor
  // 1.3.2.3 : Fixed issue with parent visibility change during edit
  //         : Fixed issue with Notes/Priority editor displaying first time
  // 1.3.2.4 : Fixed issue with focus leave when editing stops
  // 1.4.0.0 : New: Terminal, Windows Vista & Windows 7 styles
  //         : Fixed : issue with save & load files with Delphi 2009
  // 1.5.0.0 : New : Built in support for Office 2010 colors
  // 1.5.1.0 : New : Windows 8, Office 2013 styles added
  // 1.5.2.0 : New : Windows 10, Office 2016 styles added


var
  CF_TODOITEM: word;

type
  TTodoListBox = class;
  TCustomTodoList = class;

  TTodoData = (tdSubject, tdCompletion, tdNotes, tdPriority, tdDueDate, tdStatus,
    tdImage, tdComplete, tdTotalTime, tdCompletionDate, tdCreationDate,
    tdResource, tdHandle, tdProject, tdCategory);

  TCheckType = (ctCheckBox, ctCheckMark, ctGlyph);

  TSortDirection = (sdAscending, sdDescending);

  TCompleteClick = procedure(Sender: TObject; ItemIndex: Integer) of object;

  TTodoDateTimePicker = class(TDateTimePicker)
  private
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
  published
  end;

  { TCompleteCheck }

  TCompleteCheck = class(TPersistent)
  private
    FUnCompletedGlyph: TBitmap;
    FCompletedGlyph: TBitmap;
    FCheckType: TCheckType;
    FOnChange: TNotifyEvent;
    procedure SetCheckType(const Value: TCheckType);
    procedure SetCompletedGlyph(const Value: TBitmap);
    procedure SetUnCompletedGlyph(const Value: TBitmap);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property CompletedGlyph: TBitmap read FCompletedGlyph write SetCompletedGlyph;
    property UnCompletedGlyph: TBitmap read FUnCompletedGlyph write SetUnCompletedGlyph;
    property CheckType: TCheckType read FCheckType write SetCheckType;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTodoListStyle = (esOffice2003Blue, esOffice2003Silver, esOffice2003Olive, esOffice2003Classic, esOffice2007Luna, esOffice2007Obsidian, esWindowsXP, esWhidbey, esCustom, esOffice2007Silver,
                    esWindowsVista, esWindows7, esTerminal, esOffice2010Blue, esOffice2010Silver, esOffice2010Black,
                    esWindows8, esOffice2013White, esOffice2013LightGray, esOffice2013Gray,
                    esWindows10, esOffice2016White, esOffice2016Gray, esOffice2016Black);

  { TProgressLook }

  TProgressLook = class(TPersistent)
  private
    FUnCompleteFontColor: TColor;
    FCompleteColor: TColor;
    FUnCompleteColor: TColor;
    FCompleteFontColor: TColor;
    FOnChange: TNotifyEvent;
    FStacked: Boolean;
    FShowPercentage: Boolean;
    FShowBorder: Boolean;
    FCompletionSmooth: Boolean;
    FShowGradient: Boolean;
    FLevel2Perc: Integer;
    FLevel1Perc: Integer;
    FSteps: Integer;
    FLevel3Color: TColor;
    FLevel1Color: TColor;
    FLevel0Color: TColor;
    FLevel3ColorTo: TColor;
    FLevel2ColorTo: TColor;
    FLevel0ColorTo: TColor;
    FLevel1ColorTo: TColor;
    FBorderColor: TColor;
    FLevel2Color: TColor;
    procedure SetCompleteColor(const Value: TColor);
    procedure SetCompleteFontColor(const Value: TColor);
    procedure SetUnCompleteColor(const Value: TColor);
    procedure SetUnCompleteFontColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetCompletionSmooth(const Value: Boolean);
    procedure SetLevel0Color(const Value: TColor);
    procedure SetLevel0ColorTo(const Value: TColor);
    procedure SetLevel1Color(const Value: TColor);
    procedure SetLevel1ColorTo(const Value: TColor);
    procedure SetLevel1Perc(const Value: Integer);
    procedure SetLevel2Color(const Value: TColor);
    procedure SetLevel2ColorTo(const Value: TColor);
    procedure SetLevel2Perc(const Value: Integer);
    procedure SetLevel3Color(const Value: TColor);
    procedure SetLevel3ColorTo(const Value: TColor);
    procedure SetShowBorder(const Value: Boolean);
    procedure SetShowGradient(const Value: Boolean);
    procedure SetShowPercentage(const Value: Boolean);
    procedure SetStacked(const Value: Boolean);
    procedure SetSteps(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
  published
    property CompleteColor: TColor read FCompleteColor write SetCompleteColor;
    property CompleteFontColor: TColor read FCompleteFontColor write SetCompleteFontColor;
    property UnCompleteColor: TColor read FUnCompleteColor write SetUnCompleteColor;
    property UnCompleteFontColor: TColor read FUnCompleteFontColor write SetUnCompleteFontColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Level0Color: TColor read FLevel0Color write SetLevel0Color;
    property Level0ColorTo: TColor read FLevel0ColorTo write SetLevel0ColorTo;
    property Level1Color: TColor read FLevel1Color write SetLevel1Color;
    property Level1ColorTo: TColor read FLevel1ColorTo write SetLevel1ColorTo;
    property Level2Color: TColor read FLevel2Color write SetLevel2Color;
    property Level2ColorTo: TColor read FLevel2ColorTo write SetLevel2ColorTo;
    property Level3Color: TColor read FLevel3Color write SetLevel3Color;
    property Level3ColorTo: TColor read FLevel3ColorTo write SetLevel3ColorTo;
    property Level1Perc: Integer read FLevel1Perc write SetLevel1Perc;
    property Level2Perc: Integer read FLevel2Perc write SetLevel2Perc;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property ShowBorder: Boolean read FShowBorder write SetShowBorder;
    property Stacked: Boolean read FStacked write SetStacked;
    property ShowPercentage: Boolean read FShowPercentage write SetShowPercentage;
    property CompletionSmooth: Boolean read FCompletionSmooth write SetCompletionSmooth;
    property ShowGradient: Boolean read FShowGradient write SetShowGradient;
    property Steps: Integer read FSteps write SetSteps;
  end;

  TTodoColumnItem = class(TCollectionItem)
  private
    FWidth: Integer;
    FAlignment: TAlignment;
    FFont: TFont;
    FColor: TColor;
    FTodoData: TTodoData;
    FCaption: string;
    FOnChange: TNotifyEvent;
    FEditable: Boolean;
    FTag: Boolean;
    FImageIndex: Integer;
    FMaxLength: Integer;
    procedure SetWidth(const value: Integer);
    procedure SetAlignment(const value: tAlignment);
    procedure SetFont(const value: TFont);
    procedure SetColor(const value: TColor);
    procedure SetTodoData(const Value: TTodoData);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alignment: TAlignment read fAlignment write SetAlignment;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Editable: Boolean read FEditable write FEditable; // Can this column be edited in-place by the user?
    property Font: TFont read FFont write SetFont;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Tag: Boolean read FTag write FTag;
    property TodoData: TTodoData read FTodoData write SetTodoData;
    property Width: Integer read FWidth write SetWidth;
    property MaxLength: Integer read FMaxLength write FMaxLength;
  end;

  TTodoColumnCollection = class(TCollection)
  private
    FOwner: TTodoListBox;
    function GetItem(Index: Integer): TTodoColumnItem;
    procedure SetItem(Index: Integer; const Value: TTodoColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TTodoListBox);
    procedure Swap(Item1, Item2: TTodoColumnItem);
    function Add: TTodoColumnItem;
    function Insert(Index: Integer): TTodoColumnItem;
    property Items[Index: Integer]: TTodoColumnItem read GetItem write SetItem; default;
    function GetOwner: TPersistent; override;
  end;

  TTodoStatus = (tsNotStarted, tsCompleted, tsInProgress, tsDeferred);
  TTodoPriority = (tpLowest, tpLow, tpNormal, tpHigh, tpHighest);
  EConversionFunctionException = class(Exception);

  TStatusStrings = class(TPersistent)
  private
    FOwner: TCustomTodoList;
    FStatusStrings: array[Low(TTodoStatus)..High(TTodoStatus)] of string;
    function GetStringD: string;
    procedure SetStringD(const Value: string);
    function GetStringC: string;
    procedure SetStringC(const Value: string);
    function GetStringI: string;
    procedure SetStringI(const Value: string);
    function GetStringN: string;
    procedure SetStringN(const Value: string);
    function GetString(Index: TTodoStatus): string;
    procedure SetString(Index: TTodoStatus; const Value: string);
  protected
    property Owner: TCustomTodoList read FOwner;
  public
    constructor Create(AOwner: TCustomTodoList);
    property Items[Index: TTodoStatus]: string read GetString write SetString; default;
  published
    property Deferred: string read GetStringD write SetStringD;
    property NotStarted: string read GetStringN write SetStringN;
    property Completed: string read GetStringC write SetStringC;
    property InProgress: string read GetStringI write SetStringI;
  end;

  TPriorityStrings = class(TPersistent)
  private
    FOwner: TCustomTodoList;
    FPriorityStrings: array[Low(TTodoPriority)..High(TTodoPriority)] of string;
    function GetStringH: string;
    function GetStringHS: string;
    function GetStringL: string;
    function GetStringLS: string;
    function GetStringN: string;
    procedure SetStringH(const Value: string);
    procedure SetStringHS(const Value: string);
    procedure SetStringL(const Value: string);
    procedure SetStringLS(const Value: string);
    procedure SetStringN(const Value: string);
    function GetString(Index: TTodoPriority): string;
    procedure SetString(Index: TTodoPriority; const Value: string);
  protected
    property Owner: TCustomTodoList read FOwner;
  public
    constructor Create(AOwner: TCustomTodoList);
    property Items[Index: TTodoPriority]: string read GetString write SetString; default;
  published
    property Lowest: string read GetStringLS write SetStringLS;
    property Low: string read GetStringL write SetStringL;
    property Normal: string read GetStringN write SetStringN;
    property High: string read GetStringH write SetStringH;
    property Highest: string read GetStringHS write SetStringHS;
  end;

  TEditColors = class;

  TBackForeColors = class(TPersistent)
  private
    FColorControl: TWinControl;
    FOwner: TEditColors;
    procedure SetFontColor(const Value: TColor);
    procedure SetBackColor(const Value: TColor);
    function GetBackColor: TColor;
    function GetFontColor: TColor;
  public
    constructor Create(AOwner: TEditColors; AColorControl: TWinControl);
    property ColorControl: TWinControl read FColorControl write FColorControl;
    property Owner: TEditColors read FOwner;
  published
    property FontColor: TColor read GetFontColor write SetFontColor;
    property BackColor: TColor read GetBackColor write SetBackColor;
  end;

  TDatePickerColors = class(TPersistent)
  private
    FOwner: TEditColors;
    FColorControl: TTodoDateTimePicker;
    function GetBackColor: TColor;
    function GetFontColor: TColor;
    procedure SetBackColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    function GetCalColors: TMonthCalColors;
    procedure SetCalColors(const Value: TMonthCalColors);
  public
    constructor Create(AOwner: TEditColors; AColorControl: TTodoDateTimePicker);
    property Owner: TEditColors read FOwner;
  published
    property BackColor: TColor read GetBackColor write SetBackColor;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property CalColors: TMonthCalColors read GetCalColors write SetCalColors;
  end;

{$IFDEF USE_PLANNERDATEPICKER}
  TPlannerDatePickerColors = class;

  TCalendarColors = class(TPersistent)
  private
    FOwner: TPlannerDatePickerColors;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetEventDayColor: TColor;
    procedure SetEventDayColor(const Value: TColor);
    function GetEventMarkerColor: TColor;
    procedure SetEventMarkerColor(const Value: TColor);
    function GetFocusColor: TColor;
    procedure SetFocusColor(const Value: TColor);
    function GetHeaderColor: TColor;
    procedure SetHeaderColor(const Value: TColor);
    function GetInactiveColor: TColor;
    procedure SetInactiveColor(const Value: TColor);
    function GetSelectColor: TColor;
    procedure SetSelectColor(const Value: TColor);
    function GetSelectFontColor: TColor;
    procedure SetSelectFontColor(const Value: TColor);
    function GetTextColor: TColor;
    procedure SetTextColor(const Value: TColor);
    function GetWeekendColor: TColor;
    procedure SetWeekendColor(const Value: TColor);
    function GetGradientDirection: TGradientDirection;
    function GetGradientEndColor: TColor;
    function GetGradientStartColor: TColor;
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure SetGradientEndColor(const Value: TColor);
    procedure SetGradientStartColor(const Value: TColor);
  public
    constructor Create(AOwner: TPlannerDatePickerColors);
    property Owner: TPlannerDatePickerColors read FOwner;
  published
    property Color: TColor read GetColor write SetColor;
    property EventDayColor: TColor read GetEventDayColor write SetEventDayColor;
    property EventMarkerColor: TColor read GetEventMarkerColor write SetEventMarkerColor;
    property FocusColor: TColor read GetFocusColor write SetFocusColor;
    property GradientDirection: TGradientDirection read GetGradientDirection write SetGradientDirection;
    property GradientEndColor: TColor read GetGradientEndColor write SetGradientEndColor;
    property GradientStartColor: TColor read GetGradientStartColor write SetGradientStartColor;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property InactiveColor: TColor read GetInactiveColor write SetInactiveColor;
    property SelectColor: TColor read GetSelectColor write SetSelectColor;
    property SelectFontColor: TColor read GetSelectFontColor write SetSelectFontColor;
    property TextColor: TColor read GetTextColor write SetTextColor;
    property WeekendColor: TColor read GetWeekendColor write SetWeekendColor;
  end;

  TPlannerDatePickerColors = class(TPersistent)
  private
    FOwner: TEditColors;
    FColorControl: TPlannerDatePicker;
    FCalendarColors: TCalendarColors;
    function GetBackColor: TColor;
    function GetFontColor: TColor;
    procedure SetBackColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
  public
    constructor Create(AOwner: TEditColors; AColorControl: TPlannerDatePicker);
    destructor Destroy; override;
    property Owner: TEditColors read FOwner;
  published
    property BackColor: TColor read GetBackColor write SetBackColor;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property CalendarColors: TCalendarColors read FCalendarColors write FCalendarColors;
  end;
{$ENDIF}

  TEditColors = class(TPersistent)
  private
    FOwner: TCustomTodoList;
    FStringEditor: TBackForeColors;
    FMemoEditor: TBackForeColors;
    FIntegerEditor: TBackForeColors;
    FPriorityEditor: TBackForeColors;
    FStatusEditor: TBackForeColors;
{$IFDEF USE_PLANNERDATEPICKER}
    FPlannerDateEditor: TPlannerDatePickerColors;
{$ENDIF}
    FDefaultDateEditor: TDatePickerColors;
  public
    property Owner: TCustomTodoList read FOwner;
    constructor Create(AOwner: TCustomTodoList);
    destructor Destroy; override;
  published
    property StringEditor: TBackForeColors read FStringEditor write FStringEditor;
    property MemoEditor: TBackForeColors read FMemoEditor write FMemoEditor;
    property IntegerEditor: TBackForeColors read FIntegerEditor write FIntegerEditor;
    property PriorityEditor: TBackForeColors read FPriorityEditor write FPriorityEditor;
    property StatusEditor: TBackForeColors read FStatusEditor write FStatusEditor;
{$IFDEF USE_PLANNERDATEPICKER}
    property PlannerDateEditor: TPlannerDatePickerColors read FPlannerDateEditor write FPlannerDateEditor;
    property DefaultDateEditor: TDatePickerColors read FDefaultDateEditor write FDefaultDateEditor;
{$ELSE}
    property DateEditor: TDatePickerColors read FDefaultDateEditor write FDefaultDateEditor;
{$ENDIF}
  end;

  { Some functions which work with this: TodoStatusToString,
  TodoStatusFromString, TodoStatusCommaText.

  The same naming convention for TodoPriority: TodoPriorityToString,
  TodoPriorityFromString, TodoPriorityCommaText

  If any of these functions fail, they raise an EConversionFunctionException.
  }

  TInplaceSpinEdit = class(TSpinEdit)
  private
    FTodoList: TTodoListBox;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

  TTimeSpinBtnState = set of (tbFocusRect, tbAllowTimer);
  TTimerSpinButton = class(TSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeSpinBtnState;
    FHasMouse: Boolean;
    procedure TimerExpired(Sender: TObject);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TimeBtnState: TTimeSpinBtnState read FTimeBtnState write FTimeBtnState;
  end;

  TSpinSpeedButtons = class(TWinControl)
  private
    FUpButton: TTimerSpinButton; //TSpeedButton;
    FDownButton: TTimerSpinButton; //TSpeedButton;
    FOnUpBtnClick: TNotifyEvent;
    FOnDownBtnClick: TNotifyEvent;
    procedure AdjustSize(var W, H: Integer); reintroduce;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property OnUpBtnClick: TNotifyEvent read FOnUpBtnClick write FOnUpBtnClick;
    property OnDownBtnClick: TNotifyEvent read FOnDownBtnClick write FOnDownBtnClick;
  end;

  TInplaceFloatSpinEdit = class(TCustomMaskEdit)
  private
    FTodoList: TTodoListBox;
    FUpDownBtns: TSpinSpeedButtons;
    FPrecision: integer;
    FIncrementFloat: double;
    FDecSep: Char;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure SetEditRect;
    procedure SetPrecision(const Value: integer);
    function GetFloatValue: double;
    procedure SetFloatValue(const Value: double);
    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
  protected
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(var Key: Char): Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Precision: integer read FPrecision write SetPrecision;
    property FloatValue: double read GetFloatValue write SetFloatValue;
    property IncrementFloat: double read FIncrementFloat write FIncrementFloat;
    property DecSep: Char read fDecSep write fDecSep;
    property OnExit;
  end;

  TInplaceListBox = class(TListBox)
  private
    FOldItemIndex: Integer;
    FOnSelected: TNotifyEvent;
    FMouseDown: Boolean;
    FTodoList: TTodoListBox;
    function GetItemIndexEx: Integer;
    procedure SetItemIndexEx(const Value: Integer);
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnSelected: TNotifyEvent read FOnSelected write FOnSelected;
    property ItemIndex: Integer read GetItemIndexEx write SetItemIndexEx;
  end;

  TInplaceODListBox = class(TInplaceListBox)
  private
    FImageList: TImageList;
    FTodoList: TTodoListBox;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ImageList: TImageList read FImageList write FImageList;
  end;

  TInplaceMemo = class(TMemo)
  private
    FOldText: TStringList;
    FTodoList: TTodoListBox;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OrigLines: TStringList read FOldText;
  end;

  TInplaceEdit = class(TEdit)
  private
    FOldText: string;
    FTodoList: TTodoListBox;
    FNumericOnly: Boolean;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property NumericOnly: Boolean read FNumericOnly write FNumericOnly;
  published
    property Text: string read GetText write SetText;
  end;

  TCompletion = 0..100;

  TTodoItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FNotes: TStringList;
    FTag: Integer;
    FTotalTime: double;
    FSubject: string;
    FCompletion: TCompletion;
    FDueDate: TDateTime;
    FPriority: TTodoPriority;
    FStatus: TTodoStatus;
    FOnChange: TNotifyEvent;
    FComplete: Boolean;
    FCreationDate: TDateTime;
    FCompletionDate: TDateTime;
    FResource: string;
    FDBKey: string;
    FProject: string;
    FCategory: string;
    procedure SetImageIndex(const value: Integer);
    procedure StringsChanged(sender: TObject);
    procedure SetCompletion(const Value: TCompletion);
    procedure SetDueDate(const Value: TDateTime);
    procedure SetNotes(const Value: TStringList);
    procedure SetPriority(const Value: TTodoPriority);
    procedure SetStatus(const Value: TTodoStatus);
    procedure SetSubject(const Value: string);
    procedure SetTotalTime(const Value: double);
    procedure SetComplete(const Value: Boolean);
    procedure SetCompletionDate(const Value: TDateTime);
    procedure SetCreationDate(const Value: TDateTime);
    procedure SetResource(const Value: string);
    function GetNotesLine: string;
    procedure SetProject(const Value: string);
    procedure SetCategory(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed; virtual;
    procedure Select;
    procedure UnSelect;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    property DBKey: string read FDBKey write FDBKey;
    property NotesLine: string read GetNotesLine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Category: string read FCategory write SetCategory;
    property Complete: Boolean read FComplete write SetComplete;
    property Completion: TCompletion read FCompletion write SetCompletion;
    property CompletionDate: TDateTime read FCompletionDate write SetCompletionDate;
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    property DueDate: TDateTime read FDueDate write SetDueDate;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Notes: TStringList read FNotes write SetNotes;
    property Priority: TTodoPriority read FPriority write SetPriority;
    property Project: string read FProject write SetProject;
    property Resource: string read FResource write SetResource;
    property Status: TTodoStatus read FStatus write SetStatus;
    property Subject: string read FSubject write SetSubject;
    property Tag: Integer read FTag write FTag;
    property TotalTime: double read FTotalTime write SetTotalTime;

  end;

  TTodoItemCollection = class(TCollection)
  private
    FOwner: TTodoListBox;
    function GetItem(Index: Integer): TTodoItem;
    procedure SetItem(Index: Integer; const Value: TTodoItem);

    procedure PasteItem(Position: Boolean);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure ItemChanged(Index: Integer);
  public
    constructor Create(AOwner: TTodoListBox);
    function GetItemClass: TCollectionItemClass; virtual;
    procedure Changed; virtual;
    function Add: TTodoItem;
    function Insert(Index: Integer): TTodoItem;
    function GetOwner: TPersistent; override;
    function IndexOf(s: string): TPoint;
    function IndexInTodoOf(col: Integer; s: string): Integer;
    function IndexInRowOf(row: Integer; s: string): Integer;

    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;

    property Items[Index: Integer]: TTodoItem read GetItem write SetItem; default;
  end;

  TTodoItemIO = class(TComponent)
    FItem: TTodoItem;
  public
    constructor CreateItem(AOwner: TTodoItemCollection);
    destructor Destroy; override;
  published
    property Item: TTodoItem read FItem write FItem;
  end;

  TGridLineStyle = (glsNone, glsItems, glsAlways);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTodoListBox = class(TCustomListBox)
  private
    // Variables that represent the current editing state
    ActiveEditor: TWinControl; // current active editor, or nil if no active editor
    EditedColumn: TTodoColumnItem;
    EditedItem: TTodoItem;

    { If ActiveEditor<>nil, then the Edited* variables represent the current
    edited column and item. }
    // The actual editor objects.
    StringEditor: TInplaceEdit;
    FNumericOnlyEditor: TInplaceEdit;
    StringListEditor: TInplaceMemo;
{$IFDEF USE_PLANNERDATEPICKER}
    PlannerDateEditor: TPlannerDatePicker;
{$ENDIF}
    DefaultDateEditor: TTodoDateTimePicker;
    IntegerEditor: TInplaceSpinEdit;
    FFloatEditor: TInplaceFloatSpinEdit;
    PriorityEditor: TInplaceODListBox;
    StatusEditor: TInplaceListBox;
    EditorParent: TForm;
    { end of in-place editor object }
    FOwner: TCustomTodoList;
    FImages: TImageList;
    FPriorityImageList: TImageList;
    FTodoColumns: TTodoColumnCollection;
    FTodoItems: TTodoItemCollection;
    FGridLines: TGridLineStyle;
    FGridLineColor: TColor;
    FItemIndex: Integer;
    FUpdateCount: Integer;
    FSortTodo: Integer;
    FSortedEx: Boolean;
    FLookupIncr: Boolean;
    FLookupTodo: Integer;
    FLookup: string;
    FDateFormat: string;
    FColumnsChanged: TNotifyEvent;
    FPreview: Boolean;
    FPreviewFont: TFont;
    FCompletionFont: TFont;
    FProgressLook: TProgressLook;
    FShowSelection: Boolean;
    FPriorityFont: TFont;
    FEditable: Boolean;
    FEditSelectAll: Boolean;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FFocusColumn: Integer;
    FOnSelectItem: TNotifyEvent;
    FSelectionColorTo: TColor;
    FActiveColumnColor: TColor;
    FUseTab: Boolean;
    FShowPriorityText: Boolean;
    FIsWinXP: Boolean;
    FOnHorizontalScroll: TNotifyEvent;
    fScrollHorizontal: Boolean;
    FActiveItemColorTo: TColor;
    FActiveItemColor: TColor;
    FMouseDownNotDone: Boolean;
    FHandleGlyph: TBitmap;
    FPreviewColorTo: TColor;
    FPreviewColor: TColor;
    FStretchLastColumn: Boolean;
    FTotalTimeSuffix: string;
    procedure EditorOnExit(Sender: TObject); { This procedure is executed
    when the in-place editors lose focus. The task of this procedure is to
    transfer the edited data from the editor to the TodoListBox, then to
    hide the editor. }
    procedure EditorParentOnDeactivate(Sender: TObject); { This procedure is
    executed when the EditorParent deactivates. This calls EditorOnExit, in
    order to deactivate the active in-place editor. }

    procedure SetImages(const Value: TImageList);
    procedure SetItemIndexEx(const Value: Integer);
    function GetItemIndexEx: Integer;
    procedure SetGridLines(const Value: TGridLineStyle);
    procedure SynchItems;
    procedure SynchColumns;
    {
    function GetTodoItems(i, j: Integer): String;
    procedure SetTodoItems(i, j: Integer; const Value: String);
    }
    function GetSortedEx: Boolean;
    procedure SetSortedEx(const Value: Boolean);
    procedure SetDateFormat(const Value: string);
    procedure SetPreview(const Value: Boolean);
    procedure SetCompletionFont(const Value: TFont);
    procedure SetPreviewFont(const Value: TFont);
    procedure SetProgressLook(const Value: TProgressLook);
    procedure ProgressLookChanged(Sender: TObject);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetPriorityFont(const Value: TFont);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMHScroll(var WMScroll: TWMScroll); message WM_HSCROLL;
    procedure WMLButtonUp(var Message: TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
//    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
//    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;
    procedure CMHintShow(var M: TCMHintShow); message CM_HINTSHOW;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    function XYToColItem(const X, Y: Integer; var ColIdx, ItemIdx: Integer; var R: TRect): Boolean;
    procedure ColItemRect(ColIdx, ItemIdx: Integer; var R: TRect);
    function ClickedOnNotes(const CalcItem: Boolean; const P: TPoint; out ItemIdx: Integer; out R: TRect): Boolean;
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure StartEdit(Index, Column: Integer; FromMouse: Boolean; Msg: TMessage; X, Y: Integer; ch: Char);
    procedure AdvanceEdit;
    procedure SetSelectionColorTo(const Value: TColor);
    procedure SetActiveColumnColor(const Value: TColor);
    procedure SetShowPriorityText(const Value: Boolean);
    procedure SetScrollHorizontal(const Value: Boolean);
    function MaxHorizontalExtent: integer;
    procedure InputFromCSV(FileName: string; insertmode: Boolean);
    procedure OutputToCSV(FileName: string; appendmode: Boolean);
    procedure SetActiveItemColor(const Value: TColor);
    procedure SetActiveItemColorTo(const Value: TColor);
    procedure SetHandleGlyph(const Value: TBitmap);
    procedure SetGridLineColor(const Value: TColor);
    procedure SetPreviewColor(const Value: TColor);
    procedure SetPreviewColorTo(const Value: TColor);
    procedure SetStretchLastColumn(const Value: Boolean);
    function GetControlWidth: Integer;
  protected
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure WndProc(var Message: TMessage); override;
    //procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure UpdateHScrollExtent(maxextent: Integer);
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property Items;
    procedure DoEnter; override;
    procedure SendClickMessage(Msg: TMessage; X, Y: Integer);
    procedure SetEditorFont(Editor: TWinControl; Font: TFont);
    procedure EditNotesInPreviewArea(Idx: Integer; R: TRect; Msg: TMessage; X, Y: Integer);
    procedure ShowEditor(Editor: TWinControl; R: TRect; UseSeparateParent: boolean);
    procedure RepaintItem(Index: Integer);
    procedure SetHorizontalScrollBar;
    procedure DrawGrid(ACanvas: TCanvas);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property Text;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property ActiveColumnColor: TColor read FActiveColumnColor write SetActiveColumnColor;
    property ActiveItemColor: TColor read FActiveItemColor write SetActiveItemColor;
    property ActiveItemColorTo: TColor read FActiveItemColorTo write SetActiveItemColorTo;
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property BorderStyle;
    property Color;
    property CompletionFont: TFont read FCompletionFont write SetCompletionFont;
    property Cursor;
    property Ctl3D;
    property DateFormat: string read FDateFormat write SetDateFormat;
    property DragCursor;
    property DragMode;
    property Editable: Boolean read FEditable write FEditable;
    property EditSelectAll: Boolean read FEditSelectAll write FEditSelectAll; // If false, the caret will be put in the location the user clicked. If true, the whole subject text will be selected on user click.
    property Enabled;
    property Font;
    property GridLines: TGridLineStyle read FGridLines write SetGridLines default glsAlways;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor;
    property HandleGlyph: TBitmap read FHandleGlyph write SetHandleGlyph;
    property Images: TImageList read FImages write SetImages;
    property IntegralHeight;
    property ItemHeight;
    property ItemIndex: Integer read GetItemIndexEx write SetItemIndexEx;
    property LookupIncr: Boolean read fLookupIncr write fLookupIncr;
    property LookupTodo: Integer read fLookupTodo write fLookupTodo;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Preview: Boolean read FPreview write SetPreview;
    property PreviewFont: TFont read FPreviewFont write SetPreviewFont;
    property PreviewColor: TColor read FPreviewColor write SetPreviewColor default clNone;
    property PreviewColorTo: TColor read FPreviewColorTo write SetPreviewColorTo default clNone;    
    property PriorityFont: TFont read FPriorityFont write SetPriorityFont;
    property ProgressLook: TProgressLook read FProgressLook write SetProgressLook;
    property ScrollHorizontal: Boolean read fScrollHorizontal write SetScrollHorizontal;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionColorTo: TColor read FSelectionColorTo write SetSelectionColorTo;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor;
    property ShowHint;
    property ShowPriorityText: Boolean read FShowPriorityText write SetShowPriorityText;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection;
    property SortTodo: Integer read FSortTodo write FSortTodo;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property StretchLastColumn: Boolean read FStretchLastColumn write SetStretchLastColumn default True;
    property TabOrder;
    property TabStop;
    property TodoColumns: TTodoColumnCollection read FTodoColumns write FTodoColumns;
    property TodoItems: TTodoItemCollection read FTodoItems write FTodoItems;
    property UseTab: Boolean read FUseTab write FUseTab;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnStartDock;
    property OnEndDock;
    property OnContextPopup;
    property OnColumnsChanged: TNotifyEvent read FColumnsChanged write FColumnsChanged;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
    property TotalTimeSuffix: string read FTotalTimeSuffix write FTotalTimeSuffix;
  end;

  TImagePosition = (ipLeft, ipRight);
  TVAlignment = (vtaCenter, vtaTop, vtaBottom);
  THeaderOrientation = (hoHorizontal, hoVertical);

  THeaderClickEvent = procedure(Sender: TObject; SectionIndex: Integer) of object;
  THeaderDragDropEvent = procedure(Sender: TObject; FromSection, ToSection: Integer) of object;

  TTodoHeader = class(THeader)
  private
    FOffset: Integer;
    FLeftPos: Integer;
    FAlignment: TAlignment;
    FVAlignment: TVAlignment;
    FColor: TColor;
    FLineColor: TColor;
    FFlat: Boolean;
    FImageList: TImageList;
    FInplaceEdit: TMemo;
    FImagePosition: TImagePosition;
    FOnClick: THeaderClickEvent;
    FOnRightClick: THeaderClickEvent;
    FOnDragDrop: THeaderDragDropEvent;
    FOrientation: THeaderOrientation;
    FSectionDragDrop: Boolean;
    FDragging: Boolean;
    FDragStart: Integer;
    FEditSection: Integer;
    FEditWidth: Integer;
    FOnSectionEditEnd: THeaderClickEvent;
    FOnSectionEditStart: THeaderClickEvent;
    FSectionEdit: Boolean;
    FItemHeight: Integer;
    FTextHeight: Integer;
    FSortedSection: Integer;
    FSortDirection: TSortDirection;
    FSortShow: Boolean;
    FOwner: TCustomTodoList;
    FHScroll: integer;
    FColorTo: TColor;
    FActiveColorTo: TColor;
    FActiveColor: TColor;
    FActiveColumn: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetColor(const Value: TColor);
    procedure SetImageList(const Value: TImageList);
    procedure SetImagePosition(const Value: TImagePosition);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Message: TWMLButtonDown); message WM_RBUTTONDOWN;
    procedure SetOrientation(const Value: THeaderOrientation);
    procedure SetFlat(const Value: Boolean);
    procedure SetLineColor(const Value: TColor);
    procedure SetVAlignment(const Value: TVAlignment);
    procedure InplaceExit(Sender: TObject);
    procedure SetItemHeight(const Value: Integer);
    procedure SetTextHeight(const Value: Integer);
    procedure SetSortDirection(const Value: TSortDirection);
    procedure SetSortedSection(const Value: Integer);
    procedure DrawSortIndicator(Canvas: TCanvas; X, Y: Integer);
    procedure OwnOnDragDrop(Sender: TObject; FromSection, ToSection: Integer);
    procedure SetColorTo(const Value: TColor);
    procedure SetActiveColor(const Value: TColor);
    procedure SetActiveColorTo(const Value: TColor);
    procedure SetActiveColumn(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    function XYToSection(X, Y: Integer): Integer;
    function GetSectionRect(X: Integer): TRect;
    procedure Paint; override;

    procedure HorizontalScroll(X: integer);
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ActiveColumn: Integer read FActiveColumn write SetActiveColumn;
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
    property ActiveColorTo: TColor read FActiveColorTo write SetActiveColorTo;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Flat: Boolean read FFlat write SetFlat;
    property Images: TImageList read FImageList write SetImageList;
    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property TextHeight: Integer read FTextHeight write SetTextHeight;
    property LineColor: TColor read FLineColor write SetLineColor;
    property SectionDragDrop: Boolean read FSectionDragDrop write FSectionDragDrop;
    property SectionEdit: Boolean read FSectionEdit write FSectionEdit;
    property SortedSection: Integer read FSortedSection write SetSortedSection;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
    property SortShow: Boolean read FSortShow write FSortShow;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment;
    property Orientation: THeaderOrientation read FOrientation write SetOrientation default hoHorizontal;
    property OnClick: THeaderClickEvent read FOnClick write FOnClick;
    property OnDragDrop: THeaderDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnRightClick: THeaderClickEvent read FOnRightClick write FOnRightClick;
    property OnSectionEditStart: THeaderClickEvent read FOnSectionEditStart write FOnSectionEditStart;
    property OnSectionEditEnd: THeaderClickEvent read FOnSectionEditEnd write FOnSectionEditEnd;
  end;

  TStatusToStringEvent = procedure(Sender: TObject; AValue: TTodoStatus; var AString: string) of object;
  TStringToStatusEvent = procedure(Sender: TObject; AString: string; var AValue: TTodoStatus) of object;
  TPriorityToStringEvent = procedure(Sender: TObject; AValue: TTodoPriority; var AString: string) of object;
  TStringToPriorityEvent = procedure(Sender: TObject; AString: string; var AValue: TTodoPriority) of object;

  TTodoItemEvent = procedure(Sender: TObject; ATodoItem: TTodoItem; var Allow: Boolean) of object;

  TTodoItemSelectEvent = procedure(Sender: TObject; ATodoItem: TTodoItem) of object;

  TListHeaderEvent = procedure(Sender: TObject; Column: Integer) of object;
  TOnHeaderDragDropEvent = procedure(Sender: TObject; FromCol, ToCol: Integer) of object;

{$IFDEF USE_PLANNERDATEPICKER}
  TCalendarType = (tcDefaultCalendar, tcPlannerCalendar);
{$ENDIF}

  TCustomTodoList = class(TCustomControl, ITMSStyle) { TCustomTodoList is made of a TTodoHeader and a TTodoListBox bellow it. }
  private
{$IFDEF USE_PLANNERDATEPICKER}
    FCalendarType: TCalendarType;
{$ENDIF}
    FTodoHeader: TTodoHeader;
    FTodoListBox: TTodoListBox;
    FBorderStyle: TBorderStyle;
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnPriorityToString: TPriorityToStringEvent;
    FOnStatusToString: TStatusToStringEvent;
    FOnStringToPriority: TStringToPriorityEvent;
    FOnStringToStatus: TStringToStatusEvent;
    FPreviewHeight: Integer;
    FItemHeight: Integer;
    FStatusStrings: TStatusStrings;
    FPriorityStrings: TPriorityStrings;
    FEditColors: TEditColors;
    FCompleteCheck: TCompleteCheck;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDragDrop: TDragDropEvent;
    FOnEndDrag: TEndDragEvent;
    FOnDragOver: TDragOverEvent;
    FOnStartDrag: TStartDragEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FSorted: Boolean;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;
    FNewIdx: Integer;
    FOnHeaderRightClick: TListHeaderEvent;
    FOnHeaderClick: TListHeaderEvent;
    FAutoInsertItem: Boolean;
    FAutoDeleteItem: Boolean;
    FOnItemInsert: TTodoItemEvent;
    FOnItemDelete: TTodoItemEvent;
    FOnEditStart: TNotifyEvent;
    FOnEditDone: TNotifyEvent;
    FOnItemSelect: TTodoItemSelectEvent;
    FOnItemRightClick: TTodoItemSelectEvent;
    FCompletionGraphic: Boolean;
    FHintShowFullText: Boolean;
    FHeaderDragDrop: Boolean;
    FOnHeaderDragDrop: TOnHeaderDragDropEvent;
    FAutoAdvanceEdit: Boolean;
    FStatusListWidth: Integer;
    FPriorityListWidth: Integer;
    FNullDate: string;
    FOnCompleteClick: TCompleteClick;
    FCategory: TStringList;
    FLook: TTodoListStyle;
    FAutoThemeAdapt: Boolean;
    FTotalTimeSuffix: string;
    procedure NCPaintProc;
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetTodoItems: TTodoItemCollection;
    procedure SetTodoItems(const Value: TTodoItemCollection);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetTodoColumns: TTodoColumnCollection;
    procedure SetTodoColumns(const Value: TTodoColumnCollection);
    procedure SectionSized(Sender: TObject; SectionIdx, SectionWidth: Integer);
    function GetDateFormat: string;
    procedure SetDateFormat(const Value: string);
    procedure SetImages(const Value: TImageList);
    function GetImages: TImageList;
    function GetGridLines: TGridLineStyle;
    procedure SetGridLines(const Value: TGridLineStyle);
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    function GetPreview: Boolean;
    procedure SetPreview(const Value: Boolean);
    function GetCompletionFont: TFont;
    function GetPreviewFont: TFont;
    procedure SetCompletionFont(const Value: TFont);
    procedure SetPreviewFont(const Value: TFont);
    function GetProgressLook: TProgressLook;
    procedure SetProgressLook(const Value: TProgressLook);
    {
    function XYToColItem(const X,Y: Integer; var ColIdx,ItemIdx: Integer; var R:TRect): Boolean;
    }
    function GetPriorityFont: TFont;
    procedure SetPriorityFont(const Value: TFont);
    procedure SetEditable(const Value: boolean);
    function GetEditable: boolean;
    function GetSelectAllInSubjectEdit: Boolean;
    procedure SetSelectAllInSubjectEdit(const Value: Boolean);
    function GetShowSelection: Boolean;
    procedure SetShowSelection(const Value: Boolean);
    procedure SetPreviewHeight(const Value: Integer);
    function GetSelectionColor: TColor;
    function GetSelectionFontColor: TColor;
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionFontColor(const Value: TColor);
    procedure SetCompleteCheck(const Value: TCompleteCheck);
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragModeEx(const Value: TDragMode);
    procedure SetSortColumn(const Value: Integer);
    procedure SetSortDirection(const Value: TSortDirection);
    procedure SetSorted(const Value: Boolean);
    function GetSelected: TTodoItem;
    function GetHeaderFont: TFont;
    procedure SetHeaderFont(const Value: TFont);
    function GetEditColumn: Integer;
    procedure SetEditColumn(const Value: Integer);
    function GetTabStopEx: Boolean;
    procedure SetTabStopEx(const Value: Boolean);
    function GetTabOrderEx: Integer;
    procedure SetTabOrderEx(const Value: Integer);
    procedure SetCompletionGraphic(const Value: Boolean);
    procedure SetHeaderDragDrop(const Value: Boolean);
    procedure SetHintShowFullText(const Value: Boolean);
    procedure SetNullDate(const Value: string);
    function GetEditItem: TTodoItem;
    function GetSelectionColorTo: TColor;
    procedure SetSelectionColorTo(const Value: TColor);
    function GetActiveColumnColor: TColor;
    procedure SetActiveColumnColor(const Value: TColor);
    function GetUseTab: Boolean;
    procedure SetUseTab(const Value: Boolean);
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    function GetSelCount: integer;
    function GetItemSelected(Index: Integer): Boolean;
    procedure SetItemSelected(Index: Integer; const Value: Boolean);
    function GetShowPriorityText: Boolean;
    procedure SetShowPriorityText(const Value: Boolean);

    procedure LisBoxHorizontalScroll(Sender: TObject);
    procedure SetCategory(const Value: TStringList);
    function GetActiveItemColor: TColor;
    function GetActiveItemColorTo: TColor;
    procedure SetActiveItemColor(const Value: TColor);
    procedure SetActiveItemColorTo(const Value: TColor);
    function GetScrollHorizontal: Boolean;
    procedure SetScrollHorizontal(const Value: Boolean);
    function GetHeaderColor: TColor;
    function GetHeaderColorTo: TColor;
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderColorTo(const Value: TColor);
    function GetHandleGlyph: TBitmap;
    procedure SetHandleGlyph(const Value: TBitmap);
    procedure SetHeaderImages(const Value: TImageList);
    function GetHeaderImages: TImageList;
    function GetHeaderHeight: Integer;
    procedure SetHeaderHeight(const Value: Integer);
    function GetGridLineColor: TColor;
    procedure SetGridLineColor(const Value: TColor);
    function GetPreviewColor: TColor;
    function GetPreviewColorTo: TColor;
    procedure SetPreviewColor(const Value: TColor);
    procedure SetPreviewColorTo(const Value: TColor);
    function GetStretchLastColumn: Boolean;
    procedure SetStretchLastColumn(const Value: Boolean);
    function GetHeaderActiveColor: TColor;
    function GetHeaderActiveColorTo: TColor;
    procedure SetHeaderActiveColor(const Value: TColor);
    procedure SetHeaderActiveColorTo(const Value: TColor);
    procedure SetLook(const Value: TTodoListStyle);
    procedure SetComponentStyle(AStyle: TTMSStyle);    
    procedure ThemeAdapt;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTotalTimeSuffix(const Value: string);

  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;

    { The procedures bellow (ListXXX) are event handlers for the member
    TTodoListBox object. Their only purpose is to call the TCustomTodoList event
    handlers. }
    procedure ListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListKeyPress(Sender: TObject; var Key: Char);
    procedure ListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure ListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListEnter(Sender: TObject);
    procedure ListExit(Sender: TObject);
    procedure ListSelect(Sender: TObject);

    procedure HeaderClick(Sender: TObject; Section: Integer);
    procedure HeaderRightClick(Sender: TObject; Section: Integer);

    procedure CheckChanged(Sender: TObject);
    function CompareItems(A, B: Integer): Integer;
    procedure SwapItems(A, B: Integer);
    procedure QuickSortItems(Left, Right: Integer);
    function AllowAutoDelete(ATodoItem: TTodoItem): Boolean; virtual;
    function AllowAutoInsert(ATodoItem: TTodoItem): Boolean; virtual;
    procedure ItemSelect(ATodoItem: TTodoItem); virtual;
    procedure EditDone(Data: TTodoData; EditItem: TTodoItem); virtual;
    procedure CompleteClick(ItemIndex: Integer);
    procedure EditStart; virtual;
    property TodoListBox: TTodoListBox read FTodoListBox;
    procedure ColumnsChanged(Sender: TObject);
    function FormatDateTimeEx(Format: string; dt: TDateTime): string;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetVersionNr: Integer; virtual;
    function GetVersionString:string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetFocus; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function StatusToString(AValue: TTodoStatus): string; virtual;
    function StringToStatus(AValue: string): TTodoStatus; virtual;
    function PriorityToString(AValue: TTodoPriority): string; virtual;
    function StringToPriority(AValue: string): TTodoPriority; virtual;
    function StatusCommaText: string; virtual;
    function PriorityCommaText: string; virtual;
    procedure SaveToStream(S: TStream);
    procedure LoadFromStream(S: TStream);
    procedure ImportFromCSV(FileName: string); virtual;
    procedure ExportToCSV(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveColumns(FileName: string);
    procedure LoadColumns(FileName: string);
    procedure Sort;
    procedure HideEditor;
    procedure ShowEditor(Index, Column: Integer);
    procedure SelectAll;
    property VersionNr: Integer read GetVersionNr;
    property VersionString: string read GetVersionString;
    property List: TTodoListBox read FTodoListBox;
    property Selected: TTodoItem read GetSelected;
    property ItemSelected[Index: Integer]: Boolean read GetItemSelected write SetItemSelected;
    property EditColumn: Integer read GetEditColumn write SetEditColumn;
    property EditItem: TTodoItem read GetEditItem;
    procedure AddColumn(Data: TTodoData; ACaption: string);
    procedure RemoveColumn(Data: TTodoData);
    property ActiveColumnColor: TColor read GetActiveColumnColor write SetActiveColumnColor;
    property ActiveItemColor: TColor read GetActiveItemColor write SetActiveItemColor;
    property ActiveItemColorTo: TColor read GetActiveItemColorTo write SetActiveItemColorTo;
    property Align;
    property AutoAdvanceEdit: Boolean read FAutoAdvanceEdit write FAutoAdvanceEdit;
    property AutoInsertItem: Boolean read FAutoInsertItem write FAutoInsertItem default True;
    property AutoDeleteItem: Boolean read FAutoDeleteItem write FAutoDeleteItem default True;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Category: TStringList read FCategory write SetCategory;
{$IFDEF USE_PLANNERDATEPICKER}
    property CalendarType: TCalendarType read FCalendarType write FCalendarType default tcPlannerCalendar;
{$ENDIF}
    property Color: TColor read GetColor write SetColor;
    property Columns: TTodoColumnCollection read GetTodoColumns write SetTodoColumns;
    property CompleteCheck: TCompleteCheck read FCompleteCheck write SetCompleteCheck;
    property CompletionFont: TFont read GetCompletionFont write SetCompletionFont;
    property CompletionGraphic: Boolean read FCompletionGraphic write SetCompletionGraphic default True;
    property Cursor;
    property DateFormat: string read GetDateFormat write SetDateFormat;
    property DragCursor: TCursor read GetDragCursor write SetDragCursor;
    property DragMode: TDragMode read GetDragMode write SetDragModeEx;
    property DragKind: TDragKind read GetDragKind write SetDragKind;
    property Editable: boolean read GetEditable write SetEditable;
    property EditColors: TEditColors read FEditColors write FEditColors;
    property EditSelectAll: Boolean read GetSelectAllInSubjectEdit
      write SetSelectAllInSubjectEdit; // If false, the caret will be put in the location the user clicked. If true, the whole subject text will be selected on user click.
    property Font: TFont read GetFont write SetFont;
    property GridLines: TGridLineStyle read GetGridLines write SetGridLines default glsAlways;
    property GridLineColor: TColor read GetGridLineColor write SetGridLineColor;
    property HandleGlyph: TBitmap read GetHandleGlyph write SetHandleGlyph;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor;
    property HeaderColorTo: TColor read GetHeaderColorTo write SetHeaderColorTo;
    property HeaderActiveColor: TColor read GetHeaderActiveColor write SetHeaderActiveColor;
    property HeaderActiveColorTo: TColor read GetHeaderActiveColorTo write SetHeaderActiveColorTo;
    property HeaderDragDrop: Boolean read FHeaderDragDrop write SetHeaderDragDrop default False;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property HeaderHeight: Integer read GetHeaderHeight write SetHeaderHeight;
    property HeaderImages: TImageList read GetHeaderImages write SetHeaderImages;
    property HintShowFullText: Boolean read FHintShowFullText write SetHintShowFullText default False;
    property Images: TImageList read GetImages write SetImages;
    property Items: TTodoItemCollection read GetTodoItems write SetTodoItems;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property Look: TTodoListStyle read FLook write SetLook default esOffice2003Blue;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property NullDate: string read FNullDate write SetNullDate;
    property Preview: Boolean read GetPreview write SetPreview;
    property PreviewFont: TFont read GetPreviewFont write SetPreviewFont;
    property PreviewColor: TColor read GetPreviewColor write SetPreviewColor;
    property PreviewColorTo: TColor read GetPreviewColorTo write SetPreviewColorTo;
    property PreviewHeight: Integer read FPreviewHeight write SetPreviewHeight;
    property PriorityFont: TFont read GetPriorityFont write SetPriorityFont;
    property PriorityStrings: TPriorityStrings read FPriorityStrings write FPriorityStrings;
    property PriorityListWidth: Integer read FPriorityListWidth write FPriorityListWidth;
    property ProgressLook: TProgressLook read GetProgressLook write SetProgressLook;
    property ScrollHorizontal: Boolean read GetScrollHorizontal write SetScrollHorizontal;
    property SelCount: integer read GetSelCount;
    property SelectionColor: TColor read GetSelectionColor write SetSelectionColor;
    property SelectionColorTo: TColor read GetSelectionColorTo write SetSelectionColorTo;
    property SelectionFontColor: TColor read GetSelectionFontColor write SetSelectionFontColor;
    property ShowPriorityText: Boolean read GetShowPriorityText write SetShowPriorityText;
    property ShowSelection: Boolean read GetShowSelection write SetShowSelection;
    property Sorted: Boolean read FSorted write SetSorted;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    property StatusStrings: TStatusStrings read FStatusStrings write FStatusStrings;
    property StatusListWidth: Integer read FStatusListWidth write FStatusListWidth;
    property StretchLastColumn: Boolean read GetStretchLastColumn write SetStretchLastColumn;
    property TabStop: Boolean read GetTabStopEx write SetTabStopEx;
    property TabOrder: Integer read GetTabOrderEx write SetTabOrderEx;
    property UseTab: Boolean read GetUseTab write SetUseTab;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCompleteClick: TCompleteClick read FOnCompleteClick write FOnCompleteClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop: TDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnDragOver: TDragOverEvent read FOnDragOver write FOnDragOver;
    property OnEditDone: TNotifyEvent read FOnEditDone write FOnEditDone;
    property OnEditStart: TNotifyEvent read FOnEditStart write FOnEditStart;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnHeaderClick: TListHeaderEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderDragDrop: TOnHeaderDragDropEvent read FOnHeaderDragDrop write FOnHeaderDragDrop;
    property OnHeaderRightClick: TListHeaderEvent read FOnHeaderRightClick write FOnHeaderRightClick;
    property OnItemDelete: TTodoItemEvent read FOnItemDelete write FOnItemDelete;
    property OnItemInsert: TTodoItemEvent read FOnItemInsert write FOnItemInsert;
    property OnItemSelect: TTodoItemSelectEvent read FOnItemSelect write FOnItemSelect;
    property OnItemRightClick: TTodoItemSelectEvent read FOnItemRightClick write FOnItemRightClick;
    property OnStartDrag: TStartDragEvent read FOnStartDrag write FOnStartDrag;
    property OnEndDrag: TEndDragEvent read FOnEndDrag write FOnEndDrag;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnStatusToString: TStatusToStringEvent read FOnStatusToString
      write FOnStatusToString;
    property OnStringToStatus: TStringToStatusEvent read FOnStringToStatus
      write FOnStringToStatus;
    property OnPriorityToString: TPriorityToStringEvent read FOnPriorityToString
      write FOnPriorityToString;
    property OnStringToPriority: TStringToPriorityEvent read FOnStringToPriority
      write FOnStringToPriority;
    property Version: string read GetVersion write SetVersion;
    property TotalTimeSuffix: string read FTotalTimeSuffix write SetTotalTimeSuffix;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTodoList = class(TCustomTodoList)
  published
    property ActiveColumnColor;
    property ActiveItemColor;
    property ActiveItemColorTo;
    property Align;
    property Anchors;
    property AutoAdvanceEdit;
    property AutoInsertItem;
    property AutoDeleteItem;
    property AutoThemeAdapt;
    property BorderStyle;
    property Category;
{$IFDEF USE_PLANNERDATEPICKER}
    property CalendarType;
{$ENDIF}
    property Color;
    property Columns;
    property CompleteCheck;
    property CompletionFont;
    property CompletionGraphic;
    property Cursor;
    property DateFormat;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Editable;
    property EditColors;
    property EditSelectAll;
    property Font;
    property GridLines;
    property GridLineColor;
    property HandleGlyph;
    property HeaderActiveColor;
    property HeaderActiveColorTo;    
    property HeaderColor;
    property HeaderColorTo;    
    property HeaderDragDrop;
    property HeaderFont;
    property HeaderHeight;
    property HeaderImages;
    property HintShowFullText;
    property Images;
    property ItemHeight;
    property Items;
    property Look;
    property NullDate;
    property MultiSelect;
    property PopupMenu;
    property Preview;
    property PreviewFont;
    property PreviewColor;
    property PreviewColorTo;    
    property PreviewHeight;
    property PriorityFont;
    property PriorityStrings;
    property PriorityListWidth;
    property ProgressLook;
    property ScrollHorizontal;
    property SelCount;
    property SelectionColor;
    property SelectionColorTo;
    property SelectionFontColor;
    property ShowPriorityText;
    property ShowSelection;
    property Sorted;
    property SortDirection;
    property SortColumn;
    property StatusStrings;
    property StatusListWidth;
    property StretchLastColumn;
    property TabOrder;
    property TabStop;
    property UseTab;
    property OnClick;
    property OnCompleteClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditDone;
    property OnEditStart;
    property OnEnter;
    property OnExit;
    property OnHeaderClick;
    property OnHeaderDragDrop;
    property OnHeaderRightClick;
    property OnItemDelete;
    property OnItemInsert;
    property OnItemSelect;
    property OnItemRightClick;
    property OnStartDrag;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnStatusToString;
    property OnStringToStatus;
    property OnPriorityToString;
    property OnStringToPriority;
    property Version;
    property TotalTimeSuffix;
  end;

  TGaugeOrientation = (goHorizontal, goVertical);
  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
  end;

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer; Settings: TGaugeSettings);

function ReadInteger(S: TStream): Integer;
function ReadDouble(S: TStream): Double;
function ReadWord(S: TStream): Word;
function ReadString(S: TStream; Size: Integer): string;
function ReadDateTime(S: TStream): TDateTime;
function ReadByte(S: TStream): Byte;
function ReadBoolean(S: TStream): Boolean;
procedure WriteInteger(S: TStream; Value: Integer);
procedure SaveProperty(S: TStream; ID: Byte; Buffer: Pointer; Size: Word);

function VarCharPos(ch: Char; const s: string; var Res: Integer): Integer;
function SinglePos(p: char; s: string; var sp: Integer): Integer;
function DoubleToSingleChar(ch: Char; const s: string): string;
procedure CSVToLineFeeds(var s: string);


implementation

uses
  ShellApi, CommCtrl, Dialogs, ImgList;

const
  LINEFEED = #13;

  const
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);


{$I DELPHIXE.INC}

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;


function CurrentXPTheme:XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
                                cchMaxNameChars: Integer;
                                pszColorBuff: PWideChar;
                                cchMaxColorChars: Integer;
                                pszSizeBuff: PWideChar;
                                cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;

begin
  hThemeLib := 0;
  Result := xpNone;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib,'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib,'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            OleCheck(GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255));
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;




//------------------------------------------------------------------------------

function VarPos(su, s: string; var Respos: Integer): Integer;
begin
  Respos := Pos(su, s);
  Result := Respos;
end;

function CharPos(ch: Char; const s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      Result := i;
      Break;
    end;
end;

procedure CSVToLineFeeds(var s: string);
var
  res: string;
  i: Integer;
begin
  if CharPos(#10, s) = 0 then
    Exit;
  Res := '';
  for i := 1 to Length(s) do
    if s[i] = #10 then
      Res := Res + #13#10
    else
      Res := Res + s[i];
  s := Res;
end;

function DoubleToSingleChar(ch: Char; const s: string): string;
var
  Res: string;
  i: Integer;
begin
  if (s = '') or (CharPos(ch, s) = 0) then
  begin
    DoubleToSingleChar := s;
    Exit;
  end;

  res := '';
  i := 1;

  repeat
    if s[i] <> ch then
      Res := Res + s[i]
    else
      if ((s[i] = ch) and (s[i + 1] = ch)) then
      begin
        Inc(i);
        Res := Res + s[i];
      end;
    Inc(i);
  until (i > Length(s));

  DoubleToSingleChar := Res;

  {
  res := s[1];
  for i := 2 to Length(s) do
  begin
   if s[i] <> ch then
     Res := Res + s[i]
   else
     if ((s[i] = ch) and (s[i - 1] <> ch)) then
       Res := Res + s[i];
  end;
  DoubleToSingleChar := Res;
  }
end;

function SinglePos(p: char; s: string; var sp: Integer): Integer;
var
  i: Integer;
  QuoteCount: Integer;
begin
  i := 1;
  QuoteCount := 0;
  while i <= Length(s) do
  begin
    if s[i] = p then
    begin
      if i < Length(s) then
        Inc(QuoteCount)
      else
        if i = Length(s) then
        begin
          Result := i;
          sp := i;
          Exit;
        end;
    end
    else
    begin
      if (Odd(QuoteCount)) then
      begin
        Result := i - 1;
        sp := i - 1;
        Exit;
      end
      else
        QuoteCount := 0;
    end;
    Inc(i);
  end;
  Result := 0;
  sp := 0;
end;

function VarCharPos(ch: Char; const s: string; var Res: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      Res := i;
      Result := i;
      Break;
    end;
end;

procedure LineFeedsToCSV(var s: string);
var
  vp: Integer;
begin
  while VarPos(#13#10, s, vp) > 0 do
    Delete(s, vp, 2);
  s := '"' + s + '"';
end;

function LinesInText(s: string; multiline: boolean): Integer;
var
  vp: Integer;
begin
  Result := 1;
  if not Multiline then Exit;
  while VarPos(LINEFEED, s, vp) > 0 do
  begin
    Inc(Result);
    Delete(s, 1, vp);
  end;
end;

function CSVQuotes(const s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    Result := Result + s[i];
    if s[i] = '"' then
      Result := Result + '"';
  end;
end;

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//-------------------------------------------------------------------- DrawGauge
{
procedure DivideInSegment(Canvas: TCanvas; R: TRect; Position: integer);
var
  i: integer;
  r1: TRect;
begin
  r1.Left := 10;
  r1.Top := 1;
  r1.Right := 12;
  r1.Bottom := R.Bottom + 2;
  for i := 0 to R.Right div 9 do
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(r1);
    r1.Left := r1.Left + 9;
    r1.Right := r1.Left + 2;
  end;
end;
}

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  NumberOfBlock: Integer;
  i: Integer;
  EmptyWidth: integer;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := not (Settings.Orientation = goHorizontal);

  if (Settings.ShowPercentage) then
    Percentage := Position;

  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  
  WidthBar := WidthBar - 2;

  if (Position > 0) then
  begin
    if (Settings.Stacked) then
    begin
      if (Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / 100) * WidthBar)
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart;
        RectL.Bottom := R.Bottom - 1;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;
        DrawGradient(Canvas, Settings.Level0ColorTo,
          Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color,
          Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc -
            Settings.Level1Perc) / 100))
        else
        begin
          WidthPart := Round(WidthBar * ((Position -
            Settings.Level1Perc) / 100));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
          RectM.Right := WidthPart + RectM.Left
        else
          RectM.Top := RectM.Bottom - WidthPart;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;
          DrawGradient(Canvas, Settings.Level1ColorTo,
            Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas,
            Settings.Level1Color, Settings.Level1ColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position -
              Settings.Level2Perc) / 100));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := RectR.Left + WidthPart;
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := RectR.Bottom - WidthPart;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div
                2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo,
              Settings.Level3Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level3Color,
              Settings.Level3ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);
        end;
      end;
    end
    else
    begin
      if (Position < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (Position < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (Position < 100) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Position * WidthBar) / 100) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;
              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor,
              Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo,
              Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar) / 100) div 9) -
            1;
          EmptyWidth := Round((Position * WidthBar) / 100) mod 9;

          for i := 0 to NumberOfBlock - 1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth > 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 1);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - (EmptyWidth - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar) / 100) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position *
                WidthBar) / 100) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position *
                WidthBar) / 100) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Round((Position / 100) * WidthBar);

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 1;
          RectL.Top := R.Top + 1;
          RectL.Right := RectL.Left + WidthPart;
          RectL.Bottom := R.Bottom - 1;
        end
        else
        begin
          RectL.Left := r.Left + 1;
          RectL.Bottom := R.Bottom - 1;
          RectL.Top := RectL.Bottom - WidthPart;
          RectL.Right := r.Right - 1;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor,
            Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);


        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}

        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom
          - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2
          , IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) -
        (Canvas.TextWidth(IntToStr(Percentage) + '%') div 2) + r.Left, r.Top +
        ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(Percentage) +
        '%') div 2, IntToStr(Percentage) + '%');
    end;
  end;

  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;


//------------------------------------------------------------------------------


procedure SaveProperty(S: TStream; ID: Byte; Buffer: Pointer; Size: Word);
begin
  S.Write(ID, 1);
  S.Write(Size, 2);
  S.Write(Buffer^, Size);
end;


procedure WriteInteger(S: TStream; Value: Integer);
begin
  S.Write(Value, 4);
end;

function ReadBoolean(S: TStream): Boolean;
begin
  S.Read(Result, 1);
end;

function ReadByte(S: TStream): Byte;
begin
  S.Read(Result, 1);
end;

function ReadDateTime(S: TStream): TDateTime;
begin
  S.Read(Result, 8);
end;

function ReadString(S: TStream; Size: Integer): string;
begin
  SetLength(Result, Size div STRSIZE);
  S.Read(Result[1], Size);
end;

function ReadWord(S: TStream): Word;
begin
  S.Read(Result, 2);
end;

function ReadInteger(S: TStream): Integer;
begin
  S.Read(Result, 4);
end;

function ReadDouble(S: TStream): Double;
begin
  S.Read(Result, 8);
end;

{ Calculates the border withs of a WinControl. }

procedure WinControlBorderWidths(WinControl: TWinControl;
  out LeftBorderWidth, RightBorderWidth, TopBorderWidth, BottomBorderWidth: integer);
var
  WindowRect: TRect;
  ClientOrigin: TPoint;
begin
  // Put window rect, client origin into local variables
  GetWindowRect(WinControl.Handle, WindowRect);
  ClientOrigin := WinControl.ClientOrigin;

  LeftBorderWidth := ClientOrigin.X - WindowRect.Left;
  TopBorderWidth := ClientOrigin.Y - WindowRect.Top;

  RightBorderWidth := WindowRect.Right - (ClientOrigin.X + WinControl.ClientWidth);
  BottomBorderWidth := WindowRect.Bottom - (ClientOrigin.Y + WinControl.ClientHeight);
end;


function AlignToFlag(alignment: TAlignment): dword;
begin
  case Alignment of
    taLeftJustify: Result := DT_LEFT;
    taRightJustify: Result := DT_RIGHT;
    taCenter: Result := DT_CENTER;
  else Result := DT_LEFT;
  end;
end;

procedure TTodoListBox.CNCommand(var Message: TWMCommand);
begin
  inherited;
  if Message.NotifyCode = LBN_SELCHANGE then
  begin
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
    //DrawGrid();      
  end;
end;

procedure TTodoListBox.WMHScroll(var WMScroll: TWMScroll);
begin
  if ActiveEditor <> nil then
    EditorOnExit(Self);

  inherited;

  if (WMScroll.ScrollCode <> SB_ENDSCROLL) then
  begin
    if Assigned(FOnHorizontalScroll) then
      FOnHorizontalScroll(Self);
  end;

  if (GetScrollPos(Handle, SB_HORZ) = 0) then
    Invalidate;
end;

function TTodoListBox.XYToColItem(const X, Y: Integer; var ColIdx,
  ItemIdx: Integer; var R: TRect): Boolean;
begin
  ItemIdx := SendMessage(Handle, LB_ITEMFROMPOINT, 0, MakeLong(X, Y));

  Result := ItemIdx >= 0;

  if Result then
  begin
    SendMessage(Handle, LB_GETITEMRECT, ItemIdx, LParam(@R));

    R.Bottom := R.Top + FOwner.ItemHeight - 1;

    R.Left := R.Left - GetScrollPos(Handle, SB_HORZ); // New

    Result := False;
    ColIdx := 0;

    while ColIdx < TodoColumns.Count do
    begin
      R.Right := R.Left + TodoColumns.Items[ColIdx].Width;

      if not ScrollHorizontal and (ColIdx = TodoColumns.Count - 1) and (r.Right < Width) and StretchLastColumn then
      begin
        // if visible items < total items
        // subtract scrollbar size
        r.Right := GetControlWidth;
      end;

      if (X >= R.Left) and (X < R.Right) and (Y <= R.Bottom) then
      begin
        Result := True;
        Break;
      end
      else
        R.Left := R.Right;

      Inc(ColIdx);
    end;
  end;
end;

procedure TTodoListBox.ColItemRect(ColIdx, ItemIdx: Integer; var R: TRect);
var
  j: Integer;
begin
  SendMessage(Handle, LB_GETITEMRECT, ItemIdx, LParam(@R));

  R.Bottom := R.Top + FOwner.ItemHeight - 1;

  R.Left := R.Left - GetScrollPos(Handle, SB_HORZ); // New

  j := 0;

  while (j < TodoColumns.Count) do
  begin
    R.Right := R.Left + TodoColumns.Items[j].Width;

    if (j = ColIdx) then
    begin
      if not ScrollHorizontal and (j = TodoColumns.Count - 1) and (r.Right < Width) and StretchLastColumn then
      begin
        r.Right := GetControlWidth;
      end;
      Break
    end
    else
      R.Left := R.Right;

    Inc(j);
  end;
end;


procedure TTodoListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r, pr, R2, R3: TRect;
  su: string;
  Align: DWORD;
  col: Integer;
  IHeight: Integer;
  IIndent: Integer;
  PaintGradient: Boolean;
  dx: integer;
  ACanvas: TCanvas;
  dc: HDC;
  CurDate: TDate;
  LB: TLogBrush;
  HPen,HOldPen: THandle;
  brect: TRect;

  procedure DrawCheck(R: TRect; State: Boolean; Alignment: TAlignment);
  var
    DrawState: Integer;
    DrawRect: TRect;
    Bmp: TBitmap;
    HTheme: THandle;
  begin
    if State then
      DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED or DFCS_FLAT
    else
      DrawState := DFCS_BUTTONCHECK or DFCS_FLAT;

    case Alignment of
      taLeftJustify:
        begin
          DrawRect.Left := R.Left + 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
      taCenter:
        begin
          DrawRect.Left := R.Left + ((R.Right - R.Left - CHECKBOXSIZE) shr 1) + 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
      taRightJustify:
        begin
          DrawRect.Left := R.Right - CHECKBOXSIZE - 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
    end;

    case FOwner.CompleteCheck.CheckType of
      ctCheckBox:
        begin
          if FIsWinXP then
          begin
            if IsThemeActive then
            begin
              HTheme := OpenThemeData(Parent.Handle, 'button');
              if State then
                DrawThemeBackground(HTheme, ACanvas.Handle, BP_CHECKBOX, CBS_CHECKEDNORMAL, @DrawRect, nil)
              else
                DrawThemeBackground(HTheme, ACanvas.Handle, BP_CHECKBOX, CBS_UNCHECKEDNORMAL, @DrawRect, nil);
              CloseThemeData(HTheme);
              Exit;
            end;
          end;
          DrawFrameControl(ACanvas.Handle, DrawRect, DFC_BUTTON, DrawState);
        end;
      ctCheckMark:
        begin
          Bmp := TBitmap.Create;
          try
            if State then
              Bmp.LoadFromResourceName(hinstance, 'TMSTODO02')
            else
              Bmp.LoadFromResourceName(hinstance, 'TMSTODO03');

            Bmp.TransparentMode := tmAuto;
            Bmp.Transparent := True;

            ACanvas.Draw(DrawRect.Left, DrawRect.Top, Bmp);

            bmp.FreeImage;
          finally
            Bmp.Free;
          end;
        end;
      ctGlyph:
        begin

          if State and not FOwner.FCompleteCheck.FCompletedGlyph.Empty then
          begin
            FOwner.FCompleteCheck.FCompletedGlyph.Transparent := true;
            ACanvas.Draw(DrawRect.Left, DrawRect.Top, FOwner.FCompleteCheck.FCompletedGlyph);
          end;

          if not State and not FOwner.FCompleteCheck.FUnCompletedGlyph.Empty then
          begin
            FOwner.FCompleteCheck.FUnCompletedGlyph.Transparent := true;
            ACanvas.Draw(DrawRect.Left, DrawRect.Top, FOwner.FCompleteCheck.FUnCompletedGlyph);
          end;
        end;
    end;
  end;

  procedure DrawHandle(R: TRect; Alignment: TAlignment);
  var
    Bmp: TBitmap;
    DrawRect: TRect;
  begin
    case Alignment of
      taLeftJustify:
        begin
          DrawRect.Left := R.Left + 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
      taCenter:
        begin
          DrawRect.Left := R.Left + ((R.Right - R.Left - CHECKBOXSIZE) shr 1) + 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
      taRightJustify:
        begin
          DrawRect.Left := R.Right - CHECKBOXSIZE - 1;
          DrawRect.Top := R.Top + 1;
          DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
          DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;
        end;
    end;

    if not HandleGlyph.Empty then
      ACanvas.Draw(DrawRect.Left, DrawRect.Top, HandleGlyph)
    else
    begin
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromResourceName(hinstance, 'TMSTODO02');
        Bmp.TransparentMode := tmAuto;
        Bmp.Transparent := True;
        ACanvas.Draw(DrawRect.Left, DrawRect.Top, Bmp);
        Bmp.FreeImage;
      finally
        Bmp.Free;
      end;
    end;
  end;

  procedure DrawCompletion(R: TRect; Completion: Integer; OldStyle: Boolean);
  var
    SrcColor: TColor;
    SrcRect, TgtRect: TRect;
    W, H: Integer;
    Txt: string;
    FS: TFontStyles;
    Settings: TGaugeSettings;

  begin
    inflaterect(r,-1,-1);
    if OldStyle then
    begin
      SrcColor := ACanvas.Brush.Color;
      ACanvas.Brush.Color := Color;
      ACanvas.Brush.Color := FProgressLook.CompleteColor;
      ACanvas.Pen.Color := FProgressLook.CompleteColor;
      ACanvas.Font.Color := FProgressLook.CompleteFontColor;
      FS := ACanvas.Font.Style;
      ACanvas.Font.Style := [];
      InflateRect(R, -2, -2);
      SrcRect := R;
      W := R.Right - R.Left;
      H := R.Bottom - R.Top;
      Txt := IntToStr(Completion) + '%';
      SrcRect.Right := SrcRect.Left + Round(W * Completion / 100);
      TgtRect.Left := R.Left + ((W - ACanvas.Textwidth(Txt)) shr 1);
      TgtRect.Top := R.Top + ((H - ACanvas.Textheight(Txt)) shr 1);
      ACanvas.TextRect(SrcRect, TgtRect.Left, TgtRect.Top, Txt);

      ACanvas.Brush.Color := FProgressLook.UnCompleteColor;
      ACanvas.Pen.Color := FProgressLook.UnCompleteColor;
      ACanvas.Font.Color := FProgressLook.UnCompleteFontColor;

      SrcRect.Left := SrcRect.Right;
      SrcRect.Right := R.Right;
      ACanvas.TexTRect(SrcRect, TgtRect.Left, TgtRect.Top, Txt);

      ACanvas.Brush.Color := SrcColor;
      ACanvas.Pen.Color := SrcColor;
      Inflaterect(R, 1, 1);
      ACanvas.FrameRect(R);
      Inflaterect(R, 1, 1);
      ACanvas.FrameRect(R);
      ACanvas.Font.Style := FS;
    end
    else
    begin
      Settings.Level0Color := FProgressLook.Level0Color;
      Settings.Level0ColorTo := FProgressLook.Level0ColorTo;
      Settings.Level1Color := FProgressLook.Level1Color;
      Settings.Level1ColorTo := FProgressLook.Level1ColorTo;
      Settings.Level2Color := FProgressLook.Level2Color;
      Settings.Level2ColorTo := FProgressLook.Level2ColorTo;
      Settings.Level3Color := FProgressLook.Level3Color;
      Settings.Level3ColorTo := FProgressLook.Level3ColorTo;
      Settings.Level1Perc := FProgressLook.Level1Perc;
      Settings.Level2Perc := FProgressLook.Level2Perc;
      Settings.ShowBorder := FProgressLook.ShowBorder;
      Settings.Stacked := FProgressLook.Stacked;
      Settings.ShowPercentage := FProgressLook.ShowPercentage;
      Settings.CompletionSmooth := FProgressLook.CompletionSmooth;
      Settings.ShowGradient := FProgressLook.ShowGradient;
      Settings.Font  := ACanvas.Font;
      Settings.Font.Color := FProgressLook.FCompleteFontColor;
      Settings.Orientation := goHorizontal;
      Settings.Steps := FProgressLook.Steps;

      if FProgressLook.UnCompleteColor <> clNone then
        Settings.BackgroundColor := FProgressLook.UnCompleteColor
      else
        Settings.BackgroundColor := ACanvas.Brush.Color;

      DrawGauge(ACanvas, R, Completion, Settings);
    end;
  end;

begin
  if (Index < 0) or (Index >= TodoItems.Count) then
    Exit;

  CurDate := Date;

  dx := 0;

  if FScrollHorizontal then
    dx := GetScrollPos(Handle, SB_HORZ);


  r := Rect;
(*
  bmp := TBitmap.Create;
  bmp.Width := r.Right - r.Left;

  if Index = TodoItems.Count - 1 then
    bmp.Height := Height
  else
    bmp.Height := r.Bottom - r.Top;

  ACanvas := bmp.Canvas;

  r.Left := 0;
  r.Top := 0;
  r.Right := bmp.Width;
  r.Bottom := bmp.Height;
*)
  brect := r;



  dc := GetDC(Handle);
  ACanvas := TCanvas.Create;

  if dx = 0 then
    ACanvas.handle := Canvas.Handle
  else
    ACanvas.Handle := dc;


  ACanvas.Font.Assign(Font);

  if Index = TodoItems.Count - 1 then
  begin
    r.Bottom := Height;
  end;

  ACanvas.Pen.Color := Color;
  ACanvas.Brush.Color := Color;
  ACanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
  DrawGrid(ACanvas);

  // Rect is the rectangle covering the entire row.

  r.Left := r.Left - dx;

  IHeight := FOwner.ItemHeight;

  SetBkMode(ACanvas.Handle, TRANSPARENT);

  for Col := 1 to TodoColumns.Count do
  begin
    r.Right := r.Left + TodoColumns.Items[Col - 1].Width;

    if not ScrollHorizontal and (Col = TodoColumns.Count) and (r.Right < Width) and StretchLastColumn then
    begin
      r.Right := GetControlWidth;
    end;

    { At the end of this for loop there is an r.Left := r.Right }
    PaintGradient := false;

    if TodoItems.Items[Index].Complete then
      ACanvas.Font.Assign(FCompletionFont)
    else
    begin
      if TodoItems.Items[Index].Priority in [tpHighest, tpHigh] then
        ACanvas.Font.Assign(FPriorityFont)
      else
        ACanvas.Font.Assign(TodoColumns.Items[Col - 1].Font);
    end;

    if (odSelected in State) and FShowSelection then
    begin
      ACanvas.Brush.Color := FSelectionColor;
      ACanvas.Font.Color := FSelectionFontColor;
      if FSelectionColorTo <> clNone then
        PaintGradient := true;
    end
    else
    begin
      ACanvas.Brush.Color := TodoColumns.Items[Col - 1].Color;
    end;

    if ((FFocusColumn = Col - 1) and (GetFocus = Handle)) and
      (odSelected in State) and FOwner.Editable then
    begin
      ACanvas.Brush.Color := FActiveColumnColor; //Color;
      ACanvas.Font.Color := Font.Color;
      PaintGradient := false;
    end;

    case TodoColumns.Items[Col - 1].Alignment of
      taLeftJustify: Align := DT_LEFT;
      taCenter: Align := DT_CENTER;
      taRightJustify: Align := DT_RIGHT;
    else
      Align := DT_LEFT;
    end;

    ACanvas.Pen.Color := ACanvas.Brush.Color;

    if Col = TodoColumns.Count then
    begin
      R2 := BRect;
      R2.Left := r.Left;
      R2.Bottom := Rect.Top + FOwner.FItemHeight - 1;

      if PaintGradient then
      begin
        DrawGradient(ACanvas, SelectionColor, SelectionColorTo, 16, R2, false);
        ACanvas.Brush.Style := bsClear;
      end
      else
        ACanvas.Rectangle(r.Left, Rect.Top, Rect.Right, Rect.Top + FOwner.FItemHeight);
    end
    else
    begin
      R2 := r;
      R2.Top := BRect.Top;
      R2.Bottom := BRect.Top + FOwner.FItemHeight - 1;

      if PaintGradient then
      begin
        DrawGradient(ACanvas, SelectionColor, SelectionColorTo, 16, R2, false);
        ACanvas.Brush.Style := bsClear;
      end
      else
        ACanvas.Rectangle(r.Left, Rect.Top, r.Right, Rect.Top + FOwner.FItemHeight);
    end;

    if (TodoColumns.Items[Col - 1].TodoData <> tdCompletion) then
      OffsetRect(r, 1, 1);

    if (FGridLines <> glsNone) then
      OffsetRect(r, 1, 1);

    r.Bottom := r.Top + IHeight;

    if ((Int(TodoItems.Items[Index].CreationDate) = CurDate) or (Int(TodoItems.Items[Index].DueDate) = CurDate) or ((Int(TodoItems.Items[Index].CreationDate) <= CurDate) and (Int(TodoItems.Items[Index].DueDate) >= CurDate)))
      and (FActiveItemColor <> clNone) and (not (odSelected in State)) then
    begin
      DrawGradient(ACanvas, FActiveItemColor, FActiveItemColorTo, 16, r, false);
      ACanvas.Brush.Style := bsClear;
    end;

    case TodoColumns.Items[Col - 1].TodoData of

      tdSubject:
        begin
          su := TodoItems.Items[Index].Subject;
          DrawTextEx(ACanvas.Handle, Pchar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;
      tdResource:
        begin
          su := TodoItems.Items[Index].Resource;
          DrawTextEx(ACanvas.Handle, Pchar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdNotes:
        begin
          su := TodoItems.Items[Index].NotesLine;
          DrawTextEx(ACanvas.Handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdDueDate:
        begin
          su := FOwner.FormatDateTimeEx(FDateFormat, TodoItems.Items[Index].DueDate);
          DrawTextEx(ACanvas.Handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdCreationDate:
        begin
          su := FOwner.FormatDateTimeEx(FDateFormat, TodoItems.Items[Index].CreationDate);
          DrawTextEx(ACanvas.handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdCompletionDate:
        begin
          su := FOwner.FormatDateTimeEx(FDateFormat, TodoItems.Items[Index].CompletionDate);
          DrawTextEx(ACanvas.handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdTotalTime:
        begin
          su := floatToStr(TodoItems.Items[Index].TotalTime) + FTotalTimeSuffix;
          DrawTextEx(ACanvas.handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdStatus:
        begin
          su := FOwner.StatusToString(TodoItems.Items[Index].Status);
          DrawTextEx(ACanvas.Handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdProject:
        begin
          su := TodoItems.Items[Index].Project;
          DrawTextEx(ACanvas.Handle, Pchar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;

      tdImage:
        begin
          if Assigned(FImages) and (TodoItems.Items[Index].ImageIndex >= 0) then
          begin
            FImages.Draw(ACanvas, r.Left, r.Top, TodoItems.Items[Index].ImageIndex);
          end;
        end;

      tdComplete:
        begin
          DrawCheck(r, TodoItems.Items[Index].Complete, TodoColumns.Items[Col - 1].Alignment);
        end;

      tdHandle:
        begin
          DrawHandle(r, TodoColumns.Items[Col - 1].Alignment);
        end;

      tdCompletion:
        begin
          if FOwner.CompletionGraphic then
          begin
            if TodoItems.Items[Index].Complete then
              DrawCompletion(r, 100, false)
            else
              DrawCompletion(r, TodoItems.Items[Index].Completion, false)
          end else
          begin
            if TodoItems.Items[Index].Complete
              then su := '100%'
            else su := IntToStr(TodoItems.Items[Index].Completion) + '%';
            DrawTextEx(ACanvas.Handle, Pchar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
          end;
        end;

      tdPriority:
        begin
          IIndent := 0;
          case Align of
            DT_CENTER: IIndent := Max(0, r.Right - r.Left - FPriorityImageList.Width) div 2;
            DT_LEFT: IIndent := 0;
            DT_RIGHT: IIndent := Max(0, r.Right - r.Left - FPriorityImageList.Width);
          end;

          case TodoItems.Items[Index].Priority of
            tpLowest: FPriorityImageList.Draw(ACanvas, r.Left + IIndent, r.Top, 4, True);
            tpLow: FPriorityImageList.Draw(ACanvas, r.Left + IIndent, r.Top, 3, True);
            tpNormal: FPriorityImageList.Draw(ACanvas, r.Left + IIndent, r.Top, 2, True);
            tpHigh: FPriorityImageList.Draw(ACanvas, r.Left + IIndent, r.Top, 1, True);
            tpHighest: FPriorityImageList.Draw(ACanvas, r.Left + IIndent, r.Top, 0, True);
          end;

          R3 := r;
          if Align = DT_RIGHT then
            R3.Right := R3.Right - FPriorityImageList.Width - 2
          else
            R3.Left := r.Left + IIndent + FPriorityImageList.Width + 2;
          if FShowPriorityText then
            DrawTextEx(ACanvas.Handle, Pchar(FOwner.PriorityToString(TodoItems.Items[Index].Priority)), Length(FOwner.PriorityToString(TodoItems.Items[Index].Priority)), r3, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;
      tdCategory:
        begin
          su := TodoItems.Items[Index].Category;
          DrawTextEx(ACanvas.Handle, PChar(su), Length(su), r, Align or DT_END_ELLIPSIS or DT_SINGLELINE, nil);
        end;
    end;


    if (FFocusColumn = Col - 1) and (GetFocus = Handle) and (ItemIndex = Index) then
    begin
      // DrawFocusRect(Canvas.Handle,R);
    end;

    if (FGridLines <> glsNone) then
      OffsetRect(r, -1, -1);

    if (TodoColumns.Items[Col - 1].TodoData <> tdCompletion) then
      OffsetRect(r, -1, -1);
    r.Left := r.Right;
  end;


  if FPreview then
  begin
    SetBkMode(ACanvas.Handle, TRANSPARENT);
    ACanvas.Font.Assign(FPreviewFont);

    if (odSelected in State) and FShowSelection then
      ACanvas.Font.Color := SelectionFontColor;

    ACanvas.Brush.Color := Color;

    if (odSelected in State) and FShowSelection then
      ACanvas.Brush.Color := SelectionColor;

    if (PreviewColor <> clNone) and not ((odSelected in State) and FShowSelection)  then
      ACanvas.Brush.Color := PreviewColor;

    ACanvas.Pen.Color := ACanvas.Brush.Color;

    su := TodoItems.Items[Index].Notes.Text;
    pr := Rect;
    pr.Top := Rect.Top + IHeight;

    if (PreviewColor <> clNone) and not ((odSelected in State) and FShowSelection) then
      DrawGradient(ACanvas, PreviewColor, PreviewColorTo, 16, pr, False)
    else
      ACanvas.Rectangle(pr.Left, pr.Top, pr.Right, pr.Bottom);

    SetBKMode(ACanvas.Handle,TRANSPARENT);

    pr.Top := pr.Top + 1;
    DrawTextEx(ACanvas.Handle, Pchar(su), length(su), pr, DT_TOP or DT_WORDBREAK, nil);
  end;

  PaintGradient := false;
  if (odSelected in State) and FShowSelection then
  begin
    ACanvas.Brush.Color := FSelectionColor;
    if SelectionColorTo <> clNone then
      PaintGradient := true;
  end
  else
  begin
    ACanvas.Brush.Color := Color;
  end;

  r.Right := Rect.Right;

  ACanvas.Pen.Color := ACanvas.Brush.Color;
  if PaintGradient then
  begin
    R2 := r;
    R2.Bottom := R2.Bottom - 1;
    DrawGradient(ACanvas, SelectionColor, SelectionColorTo, 16, R2, false);
  end
  else
    ACanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);

  if ((Int(TodoItems.Items[Index].CreationDate) = CurDate) or (Int(TodoItems.Items[Index].DueDate) = CurDate) or ((Int(TodoItems.Items[Index].CreationDate) <= CurDate) and (Int(TodoItems.Items[Index].DueDate) >= CurDate)))
    and (FActiveItemColor <> clNone) and (not (odSelected in State)) then
  begin
    DrawGradient(ACanvas, FActiveItemColor, FActiveItemColorTo, 16, r, false);
    ACanvas.Brush.Style := bsClear;
  end;

  if (FGridLines <> glsNone) then
  begin
    ACanvas.Pen.Color := FGridLineColor;

    lb.lbColor := ColorToRGB(FGridLineColor);
    lb.lbStyle := bs_Solid;

    HPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE,1,lb,0,nil);
    HOldPen := SelectObject(ACanvas.Handle, HPen);

    Windows.MoveToEx(ACanvas.Handle, Rect.Left, Rect.Bottom - 1,nil);
    Windows.LineTo(ACanvas.Handle, Rect.Right, Rect.Bottom - 1);

    r.Left := Rect.Left - dx;

    if not Preview then
    begin
      for Col := 1 to FTodoColumns.Count - 1 do
      begin
        r.Left := r.Left + FTodoColumns.Items[Col - 1].Width;
        Windows.MoveToEx(ACanvas.Handle, r.Left, Rect.Top,nil);
        Windows.LineTo(ACanvas.Handle, r.Left, Rect.Bottom);
      end;
    end;
    DeleteObject(SelectObject(ACanvas.Handle,HOldPen));
  end;

(*
  dc := GetDC(Handle);
  ACanvas := TCanvas.Create;

  if dx = 0 then
    ACanvas.handle := Canvas.Handle
  else
    ACanvas.Handle := dc;

  ACanvas.Draw(Rect.Left, Rect.Top, bmp);
  bmp.Free;
*)

  ACanvas.Free;
  ReleaseDC(handle, dc);
end;


procedure TTodoListBox.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TTodoListBox.SetHandleGlyph(const Value: TBitmap);
begin
  FHandleGlyph.Assign(Value);
end;


procedure TTodoListBox.EditorParentOnDeactivate(Sender: TObject);
begin
  EditorOnExit(ActiveEditor);
end;

constructor TTodoListBox.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
begin
  inherited Create(AOwner);

  // FOwner := AOwner as TCustomTodoList; // Yields 'invalid class typecast' when component put on a form
  FOwner := AOwner as TCustomTodoList;

  Style := lbOwnerDrawFixed;
  FTodoColumns := TTodoColumnCollection.Create(self);
  FTodoItems := TTodoItemCollection.Create(self);
  FUpdateCount := 0;
  FDateFormat := ShortDateFormat;
  //DoubleBuffered := True;

  FCompletionFont := TFont.Create;
  FCompletionFont.Style := FCompletionFont.Style + [fsStrikeOut];
  FCompletionFont.Color := clGray;

  FPriorityFont := TFont.Create;
  FPriorityFont.Color := clRed;
  FPriorityFont.OnChange := ProgressLookChanged;

  FPreviewFont := TFont.Create;
  FPreviewFont.Color := clBlue;
  FPreviewFont.OnChange := ProgressLookChanged;
  FPreviewColor := clNone;
  FPreviewColorTo := clNone;

  FProgressLook := TProgressLook.Create;
  FProgressLook.OnChange := ProgressLookChanged;

  FShowSelection := True;
  FStretchLastColumn := True;

  FEditable := True; // TODO: default to false
  FEditSelectAll := False;

  // Create the components used as in-place editors
  ActiveEditor := nil;

  EditorParent := TForm.Create(self);
  EditorParent.Visible := False;
//  EditorParent.Parent := Self;
  EditorParent.BorderStyle := bsNone;
  EditorParent.OnDeactivate := EditorParentOnDeactivate;

  StringEditor := TInplaceEdit.Create(Self);
  StringEditor.Visible := False;
  StringEditor.BorderStyle := bsNone;

  FNumericOnlyEditor := TInplaceEdit.Create(Self);
  FNumericOnlyEditor.NumericOnly := true;
  FNumericOnlyEditor.Visible := false;
  FNumericOnlyEditor.BorderStyle := bsNone;

  IntegerEditor := TInplaceSpinEdit.Create(Self);
  IntegerEditor.Visible := False;
  StringListEditor := TInplaceMemo.Create(self);
  StringListEditor.Visible := False;
  StringListEditor.Ctl3D := False;

  FFloatEditor := TInplaceFloatSpinEdit.Create(self);
  FFloatEditor.Visible := False;
{$IFDEF USE_PLANNERDATEPICKER}
  PlannerDateEditor := TPlannerDatePicker.Create(Self);
  PlannerDateEditor.EditorEnabled := True;
  PlannerDateEditor.Visible := False;
{$ENDIF}

  DefaultDateEditor := TTodoDateTimePicker.Create(Self);
  DefaultDateEditor.Visible := False;
  if Assigned(FOwner) then DefaultDateEditor.Parent := FOwner;

  PriorityEditor := TInplaceODListBox.Create(Self);
  PriorityEditor.Visible := False;
  PriorityEditor.Ctl3D := False;

  StatusEditor := TInplaceListBox.Create(Self);
  StatusEditor.Visible := False;
  StatusEditor.Ctl3D := False;

  // Assign the in-place editors OnExit event
  StringEditor.OnExit := EditorOnExit;
  IntegerEditor.OnExit := EditorOnExit;
  StringListEditor.OnExit := EditorOnExit;
  FNumericOnlyEditor.OnExit := EditorOnExit;
  FFloatEditor.OnExit := EditorOnExit;
{$IFDEF USE_PLANNERDATEPICKER}
  PlannerDateEditor.OnExit := EditorOnExit;
{$ENDIF}
  DefaultDateEditor.OnExit := EditorOnExit;
  PriorityEditor.OnExit := EditorOnExit;
  PriorityEditor.OnSelected := EditorOnExit;
  StatusEditor.OnExit := EditorOnExit;
  StatusEditor.OnSelected := EditorOnExit;

  FPriorityImageList := TImageList.Create(Self);
//  FPriorityImageList.GetResource(rtBitmap,'TMSTODO01',12,[],RGB(255,255,255));
  FPriorityImageList.GetInstRes(HInstance, rtBitmap, 'TMSTODO01', 12, [], RGB(255, 255, 255));

  FScrollHorizontal := false;

  FGridLineColor := clSilver;

  FSelectionColor := clHighLight;
  FSelectionColorTo := clNone;
  FSelectionFontColor := clHighLightText;

  FActiveColumnColor := clWhite;
  FUseTab := false;
  FShowPriorityText := true;

  FActiveItemColor := clNone;
  FActiveItemColorTo := clNone;

  FHandleGlyph := TBitmap.Create;

  GridLines := glsAlways;  

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  FIsWinXP := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));

  FTotalTimeSuffix := 'h';
end;


destructor TTodoListBox.Destroy;
begin
  FCompletionFont.Free;
  FPreviewFont.Free;
  FPriorityFont.Free;
  FTodoColumns.Free;
  FTodoItems.Free;
  FProgressLook.Free;
  FPriorityImageList.Free;
  FNumericOnlyEditor.Free;
  FHandleGlyph.Free;
  inherited Destroy;
end;

procedure TTodoListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  Res: Integer;
  Canvas: TCanvas;
begin
  Height := 40;
  if (Index >= 0) then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(Handle);
    Res := Canvas.TextHeight('gh') + 4; {some overlap on fonts}
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
    SendMessage(Handle, CB_SETITEMHEIGHT, Index, Res);
  end
  else
    Res := 20;

  Height := Res;
end;

procedure TTodoListBox.WndProc(var Message: TMessage);
begin
  inherited;

  if (Message.msg = LB_DELETESTRING) or
    (Message.msg = LB_RESETCONTENT) then
  begin
    if FScrollHorizontal and (FUpdateCount = 0) then
      UpdateHScrollExtent(0);
  end;
end;
{
procedure TTodoListBox.WMSize(var Msg: TWMSize);
begin
  if FScrollHorizontal then
    UpdateHScrollExtent(0);
end;
}

procedure TTodoListBox.UpdateHScrollExtent(maxextent: Integer);
var
  max, w: Integer;
  r: TRect;
begin
  if (FUpdateCount > 0) or (FTodoColumns.Count <= 0) then
    Exit;

  if {(Items.Count <= 0) or }(FScrollHorizontal = False) then
  begin
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    SendMessage(Handle, WM_HSCROLL, SB_TOP, 0);
    Exit;
  end;

  if MaxExtent > 0 then
    Max := MaxExtent
  else
    Max := MaxHorizontalExtent;

  SendMessage(self.Handle, LB_GETITEMRECT, 0, LParam(@r));

  w := r.Right - r.Left;

  inc(FUpdateCount);
  if Max > w then
  begin
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, Max, 0);
  end
  else
  begin
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    SendMessage(Handle, WM_HSCROLL, SB_TOP, 0);
    ShowScrollBar(Handle, SB_HORZ, False);
  end;

  dec(FUpdateCount);
end;

procedure TTodoListBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

function TTodoListBox.GetItemIndexEx: Integer;
begin
  Result := SendMessage(Self.Handle, LB_GETCURSEL, 0, 0);
end;

procedure TTodoListBox.SetItemIndexEx(const Value: Integer);
var
  OldIndex: integer;
begin
  OldIndex := FItemIndex;
  FItemIndex := Value;
  if MultiSelect then
  begin
    SendMessage(Handle, LB_SELITEMRANGE, Value, MakeLParam(Value, Value));
  end;
  if FItemIndex <> OldIndex then
    SendMessage(Handle, LB_SETCURSEL, value, 0)
  else
    RepaintItem(Value);
end;

procedure TTodoListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  inherited;
end;

procedure TTodoListBox.SynchColumns;
begin
  if Assigned(FColumnsChanged) then
    FColumnsChanged(self);
end;

procedure TTodoListBox.SetProgressLook(const Value: TProgressLook);
begin
  FProgressLook.Assign(Value);
end;

procedure TTodoListBox.ProgressLookChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TTodoListBox.SetGridLines(const Value: TGridLineStyle);
begin
  FGridLines := Value;
  Invalidate;
end;

procedure TTodoListBox.SetGridLineColor(const Value: TColor);
begin
  FGridLineColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SynchItems;
var
  OldIdx: Integer;

begin
  OldIdx := ItemIndex;
  if (csLoading in ComponentState) then Exit;
  if FUpdateCount > 0 then Exit;

  //synchronize nr. of listboxitems with collection
  while (Items.Count > FTodoItems.Count) do
    Items.Delete(Items.Count - 1);

  while (Items.Count < FTodoItems.Count) do
    Items.Add('');

  if (ItemIndex = -1) and (Items.Count > 0) and (OldIdx <> -1) then
  begin
    if OldIdx < Items.Count then
      ItemIndex := OldIdx
    else
      ItemIndex := Items.Count - 1;
  end;
end;

procedure TTodoListBox.Loaded;
begin
  inherited;
  SynchItems;
  ItemIndex := FItemIndex;
end;

procedure TTodoListBox.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TTodoListBox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then SynchItems;
  end;
end;

procedure TTodoListBox.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
  Invalidate;
end;

{
function TTodoListBox.GetTodoItems(i, j: Integer): String;
begin
  if (i >= Items.Count) then raise Exception.Create('Item index out of range');

//  for k := 1 to j do
//   if fTodos.Items[k-1].TodoData<>ctText then dec(j);

  Result := GetTodo(succ(j), Items[i]);
end;

procedure TTodoListBox.SetTodoItems(i, j: Integer;
  const Value: String);
var
  s,n,l: String;
  k: Integer;

begin
  if (i >= Items.Count) then raise Exception.Create('Item index out of range');

//  for k := 1 to j do
//   if fTodos.Items[k-1].TodoType<>ctText then dec(j);
  inc(j);

  s := self.Items[i];
  k := 0;
  n := '';
  repeat
   if n <> '' then n := n + '|';
   l := GetTodoString(s);
   if (k <> j) then
     n := n + l
   else
     n := n + Value;

   inc(k);
  until (k > j);

  if (s <> '') then
   begin

    n := n + '|' + s;
   end;

  Items[i] := n;
end;
}

function TTodoListBox.GetSortedEx: Boolean;
begin
  Result := FSortedEx;
end;

procedure TTodoListBox.SetShowSelection(const Value: Boolean);
begin
  if FShowSelection <> Value then
  begin
    FShowSelection := Value;
    Invalidate;
  end;
end;

procedure TTodoListBox.SetSortedEx(const Value: Boolean);
begin
  FSortedEx := Value;
end;

procedure TTodoListBox.DoEnter;
begin
  inherited;
  FLookup := '';
end;

procedure TTodoListBox.KeyPress(var Key: Char);
var
  Msg: TMessage;
begin
  {$IFNDEF DELPHI_UNICODE}
  if (Key in ['A'..'z', '0'..'9']) and FOwner.Editable then
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  {$IFDEF DELPHIXE4_LVL}
  if Key.IsLetterOrDigit and FOwner.Editable then
  {$ENDIF}
  {$IFNDEF DELPHIXE4_LVL}
  if character.IsLetterOrDigit(Key) and FOwner.Editable then
  {$ENDIF}
  {$ENDIF}
  begin
    if (FFocusColumn >= 0) and (FOwner.Columns[FFocusColumn].TodoData = tdComplete) then
    begin
      inherited;
      Exit;
    end;
      
    StartEdit(ItemIndex, FFocusColumn, False, Msg, 0, 0, Key);
  end;  
  inherited;
end;

procedure TTodoListBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  i: Integer;
  s: string;
  Msg: TMessage;
  ATodoItem: TTodoItem;
  DelItemCount: integer;

  function Max(a, b: Integer): Integer;
  begin
    if (a > b) then Result := a else Result := b;
  end;

begin
  if key in [VK_LEFT, VK_RIGHT] then
  begin
    if Key = VK_LEFT then
      if FFocusColumn > 0 then
      begin
        FFocusColumn := FFocusColumn - 1;
        FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
        ItemIndex := ItemIndex;
      end;

    if Key = VK_RIGHT then
      if FFocusColumn < FOwner.Columns.Count - 1 then
      begin
        FFocusColumn := FFocusColumn + 1;
        FOwner.FTodoHeader.ActiveColumn := FFocusColumn;        
        ItemIndex := ItemIndex;
      end;

    Key := 0;
  end;

  inherited;

  if Key = VK_TAB then
  begin
    if Shift = [ssShift] then
    begin
      if FFocusColumn > 0 then
      begin
        FFocusColumn := FFocusColumn - 1;
        FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
        ItemIndex := ItemIndex;
      end;
    end
    else
    begin
      if FFocusColumn < FOwner.Columns.Count - 1 then
      begin
        FFocusColumn := FFocusColumn + 1;
        FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
        ItemIndex := ItemIndex;
      end;
    end;
    //StartEdit(ItemIndex,FFocusColumn,False,Msg,0,0,#0);
  end;

  if (Key = VK_F2) or (Key = VK_SPACE) then
  begin
    StartEdit(ItemIndex, FFocusColumn, False, Msg, 0, 0, #0);
  end;

  if (Key = VK_INSERT) and FOwner.AutoInsertItem then
  begin
    ATodoItem := FOwner.Items.Add;

    ATodoItem.CreationDate := Now;
    ATodoItem.DueDate := Now + 1;
    ATodoItem.CompletionDate := ATodoItem.DueDate;

    if FOwner.AllowAutoInsert(ATodoItem) then
    begin
      ATodoItem.Select;

      for i := 1 to FOwner.Columns.Count do
        if FOwner.Columns[i - 1].TodoData = tdSubject then
        begin
          FFocusColumn := i - 1;
          FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
        end;            

      StartEdit(ItemIndex, FFocusColumn, False, Msg, 0, 0, #0);
    end
    else
    begin
      ATodoItem.Free;
    end;
  end;

  if (Key = VK_DELETE) and FOwner.AutoDeleteItem then
  begin
    if not MultiSelect then
    begin
      ATodoItem := FOwner.Items[ItemIndex];

      if FOwner.AllowAutoDelete(ATodoItem) then
        FOwner.Items[ItemIndex].Free;
    end
    else
    begin
      i := 0;
      DelItemCount := 0;
      while ((i < Items.Count) and (DelItemCount < SelCount)) do
      begin
        if selected[i] then
        begin
          ATodoItem := FOwner.Items[i];

          if FOwner.AllowAutoDelete(ATodoItem) then
            FOwner.Items[i].Free;
          inc(DelItemCount);
        end
        else
          inc(i);
      end;
    end;
  end;

  if key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME, VK_END, VK_ESCAPE] then
  begin
    FLookup := '';
    Exit;
  end;

  if (ssCtrl in Shift) then
  begin
    if key = 67 {C} then
    begin
      TodoItems.CopyToClipboard;
    end;

    if key = 86 {V} then
    begin
      TodoItems.PasteFromClipboard;
    end;

    if Key = 88 {X} then
    begin
      TodoItems.CutToClipboard;
    end;
  end;

  if (key = VK_BACK) and (Length(FLookup) > 0) then
    Delete(FLookup, Length(FLookup), 1)
  else
    if not FLookupIncr then fLookup := chr(key) else
      if (key > 31) and (key <= 255) then FLookup := FLookup + chr(key);

  if (ItemIndex >= 0) or (FLookupIncr) then
  begin
    for i := Max(1, ItemIndex + 1) to Items.Count do
    begin
      // s := TodoItems[i-1,fLookupTodo];
      if (s <> '') then
        if (pos(UpperCase(FLookup), uppercase(s)) = 1) then
        begin
          ItemIndex := i - 1;
//        Invalidate;
          Exit;
        end;
    end;
  end;

  for i := 1 to Items.Count do
  begin
    //s := TodoItems[i-1,fLookupTodo];

    if (s <> '') then
      if (pos(uppercase(fLookup), uppercase(s)) = 1) then
      begin
        ItemIndex := i - 1;
        Exit;
      end;
  end;

  if FLookupIncr then
  begin
    fLookup := chr(key);
    for i := 1 to Items.Count do
    begin
      //s := TodoItems[i-1,fLookupTodo];
      if (s <> '') then
        if (pos(uppercase(fLookup), uppercase(s)) = 1) then
        begin
          ItemIndex := i - 1;
          Exit;
        end;
    end;
  end;
end;

procedure TTodoListBox.SetPreview(const Value: Boolean);
begin
  FPreview := Value;
  Invalidate;
end;

procedure TTodoListBox.SetCompletionFont(const Value: TFont);
begin
  FCompletionFont.Assign(Value);
  Invalidate;
end;

procedure TTodoListBox.SetPreviewFont(const Value: TFont);
begin
  FPreviewFont.Assign(Value);
  Invalidate;
end;

procedure TTodoListBox.SetPriorityFont(const Value: TFont);
begin
  FPriorityFont.Assign(Value);
  Invalidate;
end;

procedure TTodoListBox.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SetSelectionColorTo(const Value: TColor);
begin
  FSelectionColorTo := Value;
  Invalidate;
end;

procedure TTodoListBox.SetSelectionFontColor(const Value: TColor);
begin
  FSelectionFontColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SetActiveColumnColor(const Value: TColor);
begin
  FActiveColumnColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SetActiveItemColor(const Value: TColor);
begin
  FActiveItemColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SetActiveItemColorTo(const Value: TColor);
begin
  FActiveItemColorTo := Value;
  Invalidate;
end;

procedure TTodoListBox.SetShowPriorityText(const Value: Boolean);
begin
  FShowPriorityText := Value;
  Invalidate;
end;

procedure TTodoListBox.SetScrollHorizontal(const Value: Boolean);
begin
  if FScrollHorizontal <> Value then
  begin
    FScrollHorizontal := Value;
    UpdateHScrollExtent(0);
  end;
end;

procedure TTodoListBox.SetStretchLastColumn(const Value: Boolean);
begin
  FStretchLastColumn := Value;
  Invalidate;
end;

function TTodoListBox.GetControlWidth: Integer;
begin
  Result := Width;
  if (Items.Count * ItemHeight > Height) then
    Result := Result - GetSystemMetrics(SM_CXHSCROLL);
end;

function TTodoListBox.MaxHorizontalExtent: integer;
var
  i: integer;
begin
  Result := 0;
  if FTodoColumns.Count <= 0 then
    exit;
  for i := 0 to TodoColumns.Count - 1 do
  begin
    Result := Result + TodoColumns.Items[i].Width;
  end;
end;

procedure TTodoListBox.InputFromCSV(FileName: string; insertmode: Boolean);
var
  buffer, celltext: string;
  s: Integer;
  f: TextFile;
  delimiterpos, quotepos: Integer;
  lr: TStringList;
  ColCount, dif: integer;
begin
  ColCount := 15;
  TodoItems.Clear;
  dif := 1;
  AssignFile(f, FileName);
{$I-}
  Reset(f);
{$I+}
  if (IOResult <> 0) then
    raise Exception.Create('Cannot open file ' + FileName);


  lr := TStringList.Create;

  // do intelligent estimate of the separator
  Reset(f);

  while not Eof(f) do
  begin
    ReadLn(f, buffer);
    lr.CommaText := Buffer;

    s := 1; //StrtCol;

    while VarCharPos(#0 {FDelimiter}, Buffer, DelimiterPos) > 0 do
    begin
      if Buffer[1] = '"' then
      begin
        Delete(buffer, 1, 1); //delete first quote from buffer

        if SinglePos('"', Buffer, QuotePos) > 0 then //search for next single quote
        begin
          CellText := Copy(buffer, 1, QuotePos - 1);
          CellText := DoubleToSingleChar('"', CellText);
          Delete(buffer, 1, QuotePos);
        end
        else
          CellText := '';
        VarCharPos(#0 {FDelimiter}, buffer, DelimiterPos);
      end
      else
      begin
        CellText := Copy(buffer, 1, DelimiterPos - 1);
        CellText := DoubleToSingleChar('"', CellText);
      end;

     { if JavaCSV then
        JavaToLineFeeds(CellText)
      else  }
      CSVToLineFeeds(CellText);

      Delete(buffer, 1, DelimiterPos);

      Inc(s);
      if s >= ColCount then
        ColCount := s;
    end;

    if Length(Buffer) > 0 then
    begin
      if Buffer[1] = '"' then
        Delete(buffer, 1, 1);
      if Length(Buffer) > 0 then
      begin
        if Buffer[Length(Buffer)] = '"' then
          Delete(Buffer, Length(Buffer), 1);
      end;

      CellText := DoubleToSingleChar('"', Buffer);
     {
      if JavaCSV then
        JavaToLineFeeds(CellText)
      else  }
      CSVToLineFeeds(CellText);


      Inc(s);
      if s > ColCount then
        ColCount := s;
    end;

    with FTodoItems.Add do
    begin
      Subject := lr[0];
      Resource := lr[1];
      Notes.text := lr[2]; //WriteToCSV(CellText);
      Status := FOwner.StringToStatus(lr[2 + dif]);
      Project := lr[3 + dif];
      Category := lr[4 + dif];
      Complete := Lr[5 + dif] = 'True';
      Completion := StrtoInt(lr[6 + dif]);
      CompletionDate := StrToDate(lr[7 + dif]);
      CreationDate := StrToDate(lr[8 + dif]);
      DueDate := StrToDate(lr[9 + dif]);
      TotalTime := StrToFloat(lr[10 + dif]);
      Tag := Strtoint(lr[11 + dif]);
      ImageIndex := StrToInt(lr[12 + dif]);
      Priority := FOwner.StringToPriority(lr[13 + dif]);
    end;

  end;

  CloseFile(f);
  lr.Free;
  UpdateHScrollExtent(0)
end;

procedure TTodoListBox.OutputToCSV(FileName: string; appendmode: Boolean);
var
  f: TextFile;
  s: Integer;
  CellText: string;
  Delim: Char;
  dblquotes: Boolean;

  procedure WriteToCSV(S: string);
  begin
    S := CSVQuotes(S);
    {
    if FOemConvert then
      StringToOem(CellText);
    }
    dblquotes := false;

    if ({(FAlwaysQuotes) or }((Pos(Delim, S) = 0) and (Pos('"', S) > 0))) then
    begin
      S := '"' + S + '"';
      dblquotes := true;
    end;

    if S = '' then
    begin
     { if JavaCSV then
        CellText := '^'
      else
        if QuoteEmptyCells then
          CellText := '""';  }
    end;

    if (Pos(Delim, S) > 0) or (LinesInText(CellText, true) > 1) then
    begin
     { if JavaCSV then
        LinefeedstoJava(CellText)
      else      }
      begin
       // if not dblquotes then
        LinefeedsToCSV(S)
       // else
         // LinefeedsToCSVNQ(CellText);
      end;
    end;
    Write(f, S);
    write(f, Delim);
  end;

begin

  if true {FDelimiter = #0} then
    Delim := ',' {
  else
    Delim := FDelimiter};

  AssignFile(f, FileName);

  if AppendMode then
  begin
{$I-}
    Reset(f);
{$I+}
    if IOResult <> 0 then
    begin
{$I-}
      Rewrite(f);
{$I+}
      if IOResult <> 0 then raise Exception.Create('Cannot Create file ' + FileName);
    end
    else
      Append(f);
  end
  else
  begin
{$I-}
    Rewrite(f);
{$I+}
    if IOResult <> 0 then raise Exception.Create('Cannot Create file ' + FileName);
  end;

  for s := 0 {SaveStartCol} to TodoItems.Count - 1 {SaveEndCol + n} do
  begin

    CellText := TodoItems.Items[s].Notes.Text;
    WriteToCSV(TodoItems.Items[s].Subject);
    WriteToCSV(TodoItems.Items[s].Resource);
    WriteToCSV(CellText);
    WriteToCSV(FOwner.StatusToString(TodoItems.Items[s].Status));
    WriteToCSV(TodoItems.Items[s].Project);
    WriteToCSV(TodoItems.Items[s].Category);
    if TodoItems.Items[s].Complete then
      WriteToCSV('True')
    else
      WriteToCSV('False');
    WriteToCSV(Inttostr(TodoItems.Items[s].Completion));
    WriteToCSV(DateToStr(TodoItems.Items[s].CompletionDate));
    WriteToCSV(DateToStr(TodoItems.Items[s].CreationDate));
    WriteToCSV(DateToStr(TodoItems.Items[s].DueDate));
    WriteToCSV(FloatToStr(TodoItems.Items[s].TotalTime));
    WriteToCSV(inttostr(TodoItems.Items[s].Tag));
    WriteToCSV(Inttostr(TodoItems.Items[s].ImageIndex));
    WriteToCSV(FOwner.PriorityToString(TodoItems.Items[s].Priority));

    Writeln(f);
  end;
  CloseFile(f);
end;

{ TTodoColumnItem }

procedure TTodoColumnItem.Assign(Source: TPersistent);
begin
  if Source is TTodoColumnItem then
  begin
    Alignment := TTodoColumnItem(Source).Alignment;
    Caption := TTodoColumnItem(Source).Caption;
    Color := TTodoColumnItem(Source).Color;
    Editable := TTodoColumnItem(Source).Editable;
    Font.Assign(TTodoColumnItem(Source).Font);
    TodoData := TTodoColumnItem(Source).TodoData;
    Width := TTodoColumnItem(Source).Width;
  end;
end;

constructor TTodoColumnItem.Create(Collection: TCollection);
var
  AOwner: TTodoListBox;
begin
  inherited;
  FFont := TFont.Create;
  FWidth := 100;
  FColor := clWindow;
  FEditable := True;
  FImageIndex := -1;
  AOwner := TTodoColumnCollection(Collection).FOwner;

  FFont.Assign(AOwner.Font);
  if AOwner.HandleAllocated then
  begin
    AOwner.SynchColumns;
    AOwner.SetHorizontalScrollBar;
  end;
end;

destructor TTodoColumnItem.Destroy;
var
  AOwner: TTodoListBox;
begin
  AOwner := TTodoColumnCollection(Collection).FOwner;
  if AOwner.HandleAllocated then
  begin
    AOwner.SynchColumns;
    AOwner.SetHorizontalScrollBar;
  end;
  FFont.Free;
  inherited;
end;

function TTodoColumnItem.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := 'Column ' + IntToStr(Index);

  case TodoData of
    tdSubject: Result := Result + ' (Subject)';
    tdCompletion: Result := Result + ' (Completion)';
    tdNotes: Result := Result + ' (Notes)';
    tdPriority: Result := Result + ' (Priority)';
    tdDueDate: Result := Result + ' (Due Date)';
    tdStatus: Result := Result + ' (Status)';
    tdImage: Result := Result + ' (Image)';
    tdComplete: Result := Result + ' (Complete)';
    tdTotalTime: Result := Result + ' (Total time)';
    tdCompletionDate: Result := Result + ' (Completion date)';
    tdCreationDate: Result := Result + ' (Creation date)';
    tdResource: Result := Result + ' (Resource)';
    tdHandle: Result := Result + ' (Handle)';
    tdProject: Result := Result + ' (Project)';
    tdCategory: Result := Result + ' (Category)';
  end;

end;

procedure TTodoColumnItem.SetAlignment(const value: tAlignment);
begin
  FAlignment := Value;
  Changed;
end;

procedure TTodoColumnItem.SetColor(const value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TTodoColumnItem.SetTodoData(const Value: TTodoData);
begin
  FTodoData := Value;
  if FTodoData = tdHandle then
    FEditable := False;
  Changed;
end;

procedure TTodoColumnItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TTodoColumnItem.SetWidth(const Value: Integer);
var
  AOwner: TTodoListBox;
begin
  FWidth := Value;
  Changed;
  AOwner := TTodoColumnCollection(Collection).FOwner;
  if AOwner.HandleAllocated then
  begin
    AOwner.SynchColumns;
    AOwner.SetHorizontalScrollBar;
  end;
end;

procedure TTodoColumnItem.SetCaption(const Value: string);
var
  AOwner: TTodoListBox;
begin
  FCaption := Value;
  Changed;
  AOwner := TTodoColumnCollection(Collection).FOwner;
  if AOwner.HandleAllocated then AOwner.SynchColumns;
end;

procedure TTodoColumnItem.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  TTodoColumnCollection(Collection).Changed;
end;

procedure TTodoColumnItem.LoadFromStream(S: TStream);
var Count, i, ID, Size: Integer;
begin
  Count := ReadInteger(S);
  for i := 1 to Count do
  begin
    ID := ReadByte(S);
    Size := ReadWord(S);
    case ID of
      1: Width := ReadInteger(S);
      2: Caption := ReadString(S, Size);
      3: TodoData := TTodoData(ReadByte(S));
    end
  end;
end;

procedure TTodoColumnItem.SaveToStream(S: TStream);
begin
  // the number of properties i'm saving
  WriteInteger(S, 3);
  SaveProperty(S, 1, @FWidth, 4);
  SaveProperty(S, 2, @FCaption[1], Length(FCaption) * STRSIZE);
  SaveProperty(S, 3, @FTodoData, 1);

end;

procedure TTodoColumnItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed;
end;

{ TTodoColumnCollection }

function TTodoColumnCollection.Add: TTodoColumnItem;
begin
  Result := TTodoColumnItem(inherited Add);
end;

constructor TTodoColumnCollection.Create(AOwner: TTodoListBox);
begin
  inherited Create(TTodoColumnItem);
  FOwner := AOwner;
end;


function TTodoColumnCollection.GetItem(Index: Integer): TTodoColumnItem;
begin
  Result := TTodoColumnItem(inherited Items[index]);
end;

function TTodoColumnCollection.GetOwner: tPersistent;
begin
  Result := FOwner;
end;

function TTodoColumnCollection.Insert(Index: Integer): TTodoColumnItem;
begin
  Result := TTodoColumnItem(inherited Insert(index));
end;

procedure TTodoColumnCollection.SetItem(Index: Integer;
  const Value: TTodoColumnItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TTodoColumnCollection.Swap(Item1, Item2: TTodoColumnItem);
var
  ti: TTodoColumnItem;
begin
  ti := TTodoColumnItem.Create(Self);
  ti.Assign(Item1);
  Item1.Assign(Item2);
  Item2.Assign(ti);
  ti.Free;
end;

procedure TTodoColumnCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if FOwner.HandleAllocated then
  begin
    FOwner.Invalidate;
    FOwner.SynchColumns;
  end;
end;


{ TTodoItemCollection }

function TTodoItemCollection.Add: TTodoItem;
begin
  Result := TTodoItem(inherited Add);
end;

constructor TTodoItemCollection.Create(aOwner: TTodoListBox);
begin
  inherited Create(TTodoItem);
  FOwner := AOwner;
end;

function TTodoItemCollection.GetItem(Index: Integer): TTodoItem;
begin
  Result := TTodoItem(inherited Items[index]);
end;

function TTodoItemCollection.GetOwner: tPersistent;
begin
  Result := FOwner;
end;


function TTodoItemCollection.Insert(index: Integer): TTodoItem;
begin
  Result := TTodoItem(inherited Insert(index));
end;

function TTodoItemCollection.IndexInTodoOf(col: Integer;
  s: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Count do
  begin
    if Items[i - 1].Notes.Count > col then
      if Items[i - 1].Notes[col] = s then
      begin
        Result := i - 1;
        Break;
      end;
  end;
end;

function TTodoItemCollection.IndexInRowOf(row: Integer;
  s: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  if (Count > Row) then
    for i := 1 to Items[row].Notes.Count do
    begin
      if Items[row].Notes[i - 1] = s then
      begin
        Result := i - 1;
        Break;
      end;
    end;
end;


function TTodoItemCollection.IndexOf(s: string): tpoint;
var
  i, j: Integer;
begin
  Result := Point(-1, -1);
  for i := 1 to Count do
  begin
    for j := 1 to Items[i - 1].Notes.Count do
      if Items[i - 1].Notes[j - 1] = s then
      begin
        Result.y := i - 1;
        Result.x := j - 1;
        Break;
      end;
  end;
end;

procedure TTodoItemCollection.SetItem(Index: Integer;
  const Value: TTodoItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TTodoItemCollection.Update(Item: TCollectionItem);
begin
  inherited;
end;

procedure TTodoItemCollection.Changed;
begin
  FOwner.Invalidate;
end;

procedure TTodoItemCollection.ItemChanged(Index: Integer);
begin
  if (Index >= 0) and (Index < FOwner.Items.Count) then
  begin
    FOwner.RepaintItem(Index)
  end;
end;

function TTodoItemCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TTodoItem;
end;

procedure TTodoItemCollection.CopyToClipboard;
var
  Clipboard: TClipboard;
  TodoItemIO: TTodoItemIO;
  MemStream: TMemoryStream;
  Data: THandle;
  DataPtr: Pointer;
  Selected: TTodoItem;
begin
  if FOwner.ItemIndex < 0 then
    exit;

  Clipboard := TClipboard.Create;
  Selected := Items[FOwner.ItemIndex];

  if Assigned(Selected) then
  begin
   { Selected.ItemBegin := Selected.ItemBegin;
    Selected.ItemEnd := Selected.ItemEnd;
    Selected.DBTag := MakeLong(Selected.ItemBeginPrecis, Selected.ItemEndPrecis);
    }
    TodoItemIO := TTodoItemIO.CreateItem(Self);
    TodoItemIO.Item.Assign(Selected);

    MemStream := TMemoryStream.Create;
    try
      MemStream.WriteComponent(TodoItemIO);

      Clipboard.Open;
      try
        Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, MemStream.Size);
        try
          DataPtr := GlobalLock(Data);
          try
            Move(MemStream.Memory^, DataPtr^, MemStream.Size);
            Clipboard.Clear;
            SetClipboardData(CF_TODOITEM, Data);
          finally
            GlobalUnlock(Data);
          end;
        except
          GlobalFree(Data);
          raise;
        end;
      finally
        Clipboard.Close;
      end;
    finally
      MemStream.Free;
    end;
    TodoItemIO.Free;
  end;
  Clipboard.Free;
end;

procedure TTodoItemCollection.CutToClipboard;
begin
  CopyToClipboard;
  if (FOwner.ItemIndex >= 0) then
    FOwner.TodoItems.Delete(FOwner.ItemIndex); // FOwner.FreeItem(Selected);
end;

procedure TTodoItemCollection.PasteFromClipboard;
begin
  PasteItem(False);
end;

procedure TTodoItemCollection.PasteItem(Position: Boolean);
var
  Clipboard: TClipboard;
  TodoItemIO: TTodoItemIO;
  MemStream: TMemoryStream;
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard := TClipboard.Create;

  Clipboard.Open;

  Data := 0;

  try
    Data := GetClipboardData(CF_TODOITEM);
    if Data = 0 then
      Exit;

    DataPtr := GlobalLock(Data);
    if DataPtr = nil then
      Exit;
    try
      MemStream := TMemoryStream.Create;
      MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
      MemStream.Position := 0;
      TodoItemIO := TTodoItemIO.CreateItem(Self);
      try
        MemStream.ReadComponent(TodoItemIO);
        with TodoItemIO.Item do
         { if Position then
          begin
            ItemBegin := FOwner.SelItemBegin;
            ItemEnd := FOwner.SelItemEnd;
            ItemPos := FOwner.SelPosition;
          end
          else
          begin
            ItemBeginPrecis := LoWord(DBTag);
            ItemEndPrecis := HiWord(DBTag);
          end;
         }
          FOwner.TodoItems.Add.Assign(TodoItemIO.Item); // .CreateItem.Assign(TodoItemIO.Item);

      finally
        TodoItemIO.Free;
        MemStream.Free;
      end;
    finally

    end;
  finally
    GlobalUnlock(Data);
  end;

  Clipboard.Close;
  Clipboard.Free;
end;

{ TTodoItem }

procedure TTodoItem.Assign(Source: TPersistent);
begin
  if Source is TTodoItem then
  begin
    ImageIndex := TTodoItem(Source).ImageIndex;
    Notes.Assign(TTodoItem(Source).Notes);
    TTodoItemCollection(collection).FOwner.SynchItems;
    Subject := TTodoItem(Source).Subject;
    Category := TTodoItem(Source).Category;
    Complete := TTodoItem(Source).Complete;
    Completion := TTodoItem(Source).Completion;
    CompletionDate := TTodoItem(Source).CompletionDate;
    CreationDate := TTodoItem(Source).CreationDate;
    DueDate := TTodoItem(Source).DueDate;
    Priority := TTodoItem(Source).Priority;
    Project := TTodoItem(Source).Project;
    Resource := TTodoItem(Source).Resource;
    Status := TTodoItem(Source).Status;
    Tag := TTodoItem(Source).Tag;
    TotalTime := TTodoItem(Source).TotalTime;
  end;
end;

constructor TTodoItem.Create(Collection: TCollection);
var
  AOwner: TTodoListBox;
begin
  inherited;
  FNotes := TStringList.Create;
  FImageIndex := -1;
  FNotes.OnChange := StringsChanged;


  AOwner := TTodoItemCollection(Collection).FOwner;
  if AOwner.HandleAllocated then
    AOwner.SynchItems;
end;

destructor TTodoItem.Destroy;
var
  AOwner: TTodoListBox;
begin
  AOwner := TTodoItemCollection(Collection).FOwner;
  FNotes.Free;
  inherited;
  if AOwner.HandleAllocated then
    AOwner.SynchItems;
end;

function TTodoItem.GetDisplayName: string;
begin
  if Subject <> '' then
    Result := Subject
  else
    Result := 'Item' + IntToStr(Index);
end;

procedure TTodoItem.Changed;
begin
  {
  if Assigned(FOnChange) then
    FOnChange(Self);
  }
  TTodoItemCollection(Collection).ItemChanged(Index);
end;

procedure TTodoItem.SetCompletion(const Value: TCompletion);
begin
  if FCompletion <> Value then
  begin
    FCompletion := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetDueDate(const Value: TDateTime);
begin
  if FDueDate <> Value then
  begin
    FDueDate := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := value;
    Changed;
  end;
end;


procedure TTodoItem.SetNotes(const Value: TStringList);
begin
  FNotes.Assign(Value);
  Changed;
end;

procedure TTodoItem.SetPriority(const Value: TTodoPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetStatus(const Value: TTodoStatus);
begin
  if FStatus <> Value then
  begin
    FStatus := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetSubject(const Value: string);
begin
  if FSubject <> Value then
  begin
    FSubject := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetTotalTime(const Value: double);
begin
  if FTotalTime <> Value then
  begin
    FTotalTime := Value;
    Changed;
  end;
end;

procedure TTodoItem.StringsChanged(sender: TObject);
var
  Idx: Integer;
begin
  Idx := TTodoItemCollection(Collection).FOwner.ItemIndex;
  TTodoItemCollection(Collection).FOwner.SynchItems;
  TTodoItemCollection(Collection).FOwner.ItemIndex := Idx;
end;

procedure TTodoItem.SetComplete(const Value: Boolean);
begin
  if FComplete <> Value then
  begin
    FComplete := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetCompletionDate(const Value: TDateTime);
begin
  if FCompletionDate <> Value then
  begin
    FCompletionDate := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetCreationDate(const Value: TDateTime);
begin
  if FCreationDate <> Value then
  begin
    FCreationDate := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetResource(const Value: string);
begin
  if FResource <> Value then
  begin
    FResource := Value;
    Changed;
  end;
end;

procedure TTodoItem.Select;
begin
  with (Collection as TTodoItemCollection).FOwner as TTodoListBox do
  begin
    ItemIndex := Self.Index;
    if MultiSelect then
      Selected[self.Index] := true;
  end;
end;

procedure TTodoItem.UnSelect;
begin
  with (Collection as TTodoItemCollection).FOwner as TTodoListBox do
  begin

    if MultiSelect then
      Selected[self.Index] := false
    else
      ItemIndex := -1;    
  end;
end;


function TTodoItem.GetNotesLine: string;
begin
  if Notes.Count > 0 then
    Result := Notes.Strings[0]
  else
    Result := '';
end;

procedure TTodoItem.SaveToStream(S: TStream);
var
  STemp: string;
begin
// the number of properties i'm saving
  WriteInteger(S, 15); //14

// save the properties
  STemp := Notes.Text;
  SaveProperty(S, 1, @FComplete, 1);
  SaveProperty(S, 2, @FCompletion, 1);
  SaveProperty(S, 3, @FCompletionDate, 8);
  SaveProperty(S, 4, @FCreationDate, 8);
  SaveProperty(S, 5, @FDueDate, 8);
  SaveProperty(S, 6, @FImageIndex, 4);
  SaveProperty(S, 7, @STemp[1], Length(STemp) * STRSIZE);
  SaveProperty(S, 8, @FPriority, 1);
  SaveProperty(S, 9, @FResource[1], Length(FResource) * STRSIZE);
  SaveProperty(S, 10, @FStatus, 1);
  SaveProperty(S, 11, @FSubject[1], Length(FSubject) * STRSIZE);
  SaveProperty(S, 12, @FTag, 4);
  SaveProperty(S, 13, @FTotalTime, 8);
  SaveProperty(S, 14, @FProject[1], Length(FProject) * STRSIZE);
  SaveProperty(S, 15, @FCategory[1], Length(FCategory) * STRSIZE);
end;

procedure TTodoItem.LoadFromStream(S: TStream);
var Count, i, ID, Size: Integer;
begin
  Count := ReadInteger(S);
  for i := 1 to Count do
  begin
    ID := ReadByte(S);
    Size := ReadWord(S);
    case ID of
      1: Complete := ReadBoolean(S);
      2: Completion := ReadByte(S);
      3: CompletionDate := ReadDateTime(S);
      4: CreationDate := ReadDateTime(S);
      5: DueDate := ReadDateTime(S);
      6: ImageIndex := ReadInteger(S);
      7: Notes.Text := ReadString(S, Size);
      8: Priority := TTodoPriority(ReadByte(S));
      9: Resource := ReadString(S, Size);
      10: Status := TTodoStatus(ReadByte(S));
      11: Subject := ReadString(S, Size);
      12: Tag := ReadInteger(S);
      13: TotalTime := ReadDouble(S);
      14: Project := ReadString(S, Size);
      15: Category := ReadString(S, Size);
    else S.Seek(Size, soFromCurrent);
    end
  end;
end;

procedure TTodoItem.SetProject(const Value: string);
begin
  if FProject <> Value then
  begin
    FProject := Value;
    Changed;
  end;
end;

procedure TTodoItem.SetCategory(const Value: string);
begin
  if FCategory <> Value then
  begin
    FCategory := Value;
    Changed;
  end;
end;

{ TTodoHeader }

constructor TTodoHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner as TCustomTodoList;
  FColor := clBtnFace;
  FColorTo := clWhite;
  FActiveColor := clNone;
  FActiveColorTo := clNone;
  FLeftPos := 0;
  FTextHeight := 16;
  FItemHeight := 16;

  if not (csDesigning in ComponentState) then
  begin
    FInplaceEdit := TMemo.Create(Self);
    FInplaceEdit.Parent := Self;
    FInplaceEdit.Visible := False;
    FInplaceEdit.OnExit := InplaceExit;
  end;

  FOnDragDrop := OwnOnDragDrop;
  FHScroll := 0;
end;

destructor TTodoHeader.Destroy;
begin
  if not (csDesigning in ComponentState) then
    FInplaceEdit.Free;
  inherited;
end;

procedure TTodoHeader.InplaceExit(Sender: TObject);
begin
  Sections[FEditSection] := FInplaceEdit.Text;
  SectionWidth[FEditSection] := FEditWidth;
  FInplaceEdit.Visible := False;
end;

procedure TTodoHeader.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TTodoHeader.SetVAlignment(const Value: TVAlignment);
begin
  FVAlignment := Value;
  Invalidate;
end;

procedure TTodoHeader.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TTodoHeader.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
  Invalidate;
end;

procedure TTodoHeader.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  Invalidate;
end;

procedure TTodoHeader.SetOrientation(const Value: THeaderOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

procedure TTodoHeader.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Invalidate;
end;

procedure TTodoHeader.SetImagePosition(const Value: TImagePosition);
begin
  FImagePosition := Value;
  Invalidate;
end;

function TTodoHeader.GetSectionRect(X: Integer): TRect;
var
  Offset, SectionIndex: Integer;
begin
  Offset := 0;
  for SectionIndex := 0 to X - 1 do
    Offset := Offset + SectionWidth[SectionIndex];

  if FOrientation = hoHorizontal then
  begin
    Result.Left := Offset;
    Result.Right := Offset + SectionWidth[X];
    Result.Top := 0;
    Result.Bottom := Self.Height;
  end
  else
  begin
    Result.Left := 0;
    Result.Right := Self.Height;
    Result.Top := Offset;
    Result.Bottom := Offset + SectionWidth[X];
  end;
end;

function TTodoHeader.XYToSection(X, Y: Integer): Integer;
var
  Ofs, SectionIndex: Integer;
begin
  Ofs := 0;
  SectionIndex := 0;
  if FOrientation = hoHorizontal then
  begin
    while (Ofs < X) and (SectionIndex < Sections.Count) do
    begin
      Ofs := Ofs + SectionWidth[SectionIndex];
      Inc(SectionIndex);
    end;
    Dec(SectionIndex);
  end
  else
  begin
    while (Ofs < Y) and (SectionIndex < Sections.Count) do
    begin
      Ofs := Ofs + SectionWidth[SectionIndex];
      Inc(SectionIndex);
    end;
    Dec(SectionIndex);
  end;
  Result := SectionIndex;
end;

procedure TTodoHeader.DrawSortIndicator(Canvas: TCanvas; x, y: Integer);
var
  left, vpos: Integer;
begin
  left := x;
  vpos := y;

  if FSortDirection = sdDescending then
  begin
  {draw a full Colored triangle}
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Left + 4, vpos - 4);
    Canvas.LineTo(Left, vpos + 4);
    Canvas.pen.Color := clGray;
    Canvas.LineTo(Left - 4, vpos - 4);
    Canvas.LineTo(Left + 4, vpos - 4);
    Canvas.pen.Color := clBlack;
  end
  else
  begin
    Canvas.pen.Color := clWhite;
    Canvas.pen.Width := 1;
    Canvas.MoveTo(Left - 4, vpos + 4);
    Canvas.LineTo(Left + 4, vpos + 4);
    Canvas.LineTo(Left, vpos - 4);
    Canvas.pen.Color := clGray;
    Canvas.LineTo(Left - 4, vpos + 4);
    Canvas.pen.Color := clBlack;
  end;
end;


procedure TTodoHeader.Paint;
var
  SectionIndex, w, AIdx: Integer;
  s: string;
  r: TRect;
  pr: TRect;
  HorizontalAlign: Word;
  VerticalAlign: Word;
  AllPainted: Boolean;
  DoDraw: Boolean;
  ImgIdx: Integer;
  clr,clrto: TColor;
begin
  with Canvas do
  begin
    Font := Self.Font;
    
    Brush.Color := FColor;

    if FLineColor = clNone then
      Pen.Color := FColor
    else
      Pen.Color := FLineColor;

    Pen.Width := 1;
    SectionIndex := 0;
    if (Orientation = hoHorizontal) then
      r := Rect(0, 0, 0, ClientHeight)
    else
      r := Rect(0, 0, ClientWidth, 0);

    w := 0;
    s := '';
    ImgIdx := -1;

    HorizontalAlign := AlignToFlag(FAlignment);
    VerticalAlign := 0;

    AllPainted := False;
    repeat
      if SectionIndex < Sections.Count then
      begin
        w := SectionWidth[SectionIndex];

        case FVAlignment of
          vtaTop: VerticalAlign := DT_TOP;
          vtaCenter: VerticalAlign := DT_VCENTER;
          vtaBottom: VerticalAlign := DT_BOTTOM;
        else
          VerticalAlign := DT_TOP;
        end;

        if FOffset = 1 then
        begin
          if (SectionIndex < Sections.Count - 1 - FLeftPos) and (SectionIndex > 0) then
          begin
            AIdx := SectionIndex + FLeftPos;
            s := Sections[AIdx];
            ImgIdx := Integer(Sections.Objects[AIdx]);
          end
          else
          begin
            s := '';
          end;
        end
        else
        begin
          if (SectionIndex < Sections.Count - FLeftPos) then
            AIdx := SectionIndex + 1 + FLeftPos - 1
          else
            AIdx := 0;

          s := Sections[AIdx];
          ImgIdx := Integer(Sections.Objects[AIdx]);
        end;
        Inc(SectionIndex);
      end;

      if (Orientation = hoHorizontal) then
      begin
        r.Left := r.Right;
        Inc(r.Right, w);
        if (ClientWidth - r.Right < 2) or (SectionIndex = Sections.Count) then
        begin
          r.Right := ClientWidth;
          AllPainted := True;
        end;
      end
      else
      begin
        r.Top := r.Bottom;
        Inc(r.Bottom, w);

        if (ClientHeight - r.Bottom < 2) or (SectionIndex = Sections.Count) then
        begin
          r.Bottom := ClientHeight;
          AllPainted := True;
        end;
      end;

      pr := r;

      if (SectionIndex - 1 = ActiveColumn) and (ActiveColumn >= 0) then
      begin
        clr := FActiveColor;
        clrto := FActiveColorTo;
        Brush.Color := clr;
      end
      else
      begin
        clr := FColor;
        clrto := FColorTo;
      end;

      if (clrto <> clNone) then
        DrawGradient(Canvas, clr, clrto, 16, r, false)
      else
        FillRect(r);

      DoDraw := True;

      SetBKMode(Canvas.Handle, TRANSPARENT);

    {
    if Assigned(TPlanner(Owner).FOnPlannerHeaderDraw) then
    begin
      Font := Self.Font;
      Brush.Color := FColor;
      Pen.Color := FLineColor;
      Pen.Width := 1;
      TPlanner(Owner).FOnPlannerHeaderDraw(TPlanner(Owner), Canvas, r, AIdx,
        DoDraw);
    end;
    }

      if DoDraw then
      begin
        InflateRect(pr, -4, -2);

        if Assigned(FImageList) and (ImgIdx <> -1) then
        begin
          if (ImgIdx < FImageList.Count) then
          begin
            FImageList.Draw(Canvas, pr.Left, pr.Top, ImgIdx);
            pr.Left := pr.Left + FImageList.Width + 2;
          end;
        end;

      {
      if Assigned(FImageList) and (FImageList.Count + 1 + FOffset - FLeftPos >
        SectionIndex) and (SectionIndex > FOffset)
        and (SectionIndex <= Sections.Count - 1 - FLeftPos) then
      begin
        if FImagePosition = ipLeft then
        begin
          FImageList.Draw(Canvas, pr.Left, pr.Top, SectionIndex - 1 - FOffset +
            FLeftPos);
          pr.Left := pr.Left + FImageList.Width;
        end
        else
        begin
          pr.Right := pr.Right - FImageList.Width;
          FImageList.Draw(Canvas, pr.Right, pr.Top, SectionIndex - 1 - FOffset);
        end;
      end;

      s := CLFToLF(s);
      }

        if Pos(#13, s) = 0 then
          VerticalAlign := VerticalAlign or DT_SINGLELINE
        else
          VerticalAlign := 0;

        pr.Bottom := pr.Top + FTextHeight;

        if (SectionIndex - 1 = SortedSection) and FSortShow then
          pr.Right := pr.Right - 16;

        DrawText(Canvas.Handle, PChar(s), Length(s), pr, DT_NOPREFIX or
          DT_END_ELLIPSIS or HorizontalAlign or VerticalAlign);

        if (SectionIndex - 1 = SortedSection) and FSortShow then
        begin
          DrawText(Canvas.Handle, PChar(s), Length(s), pr, DT_NOPREFIX or
            DT_END_ELLIPSIS or HorizontalAlign or VerticalAlign or DT_CALCRECT);
          if (pr.Right > pr.Left) then
          begin
            if (r.Right > pr.Right) then
              DrawSortIndicator(Canvas, r.Right - 8, pr.Top + 6)
            else
              DrawSortIndicator(Canvas, pr.Right + 8, pr.Top + 6)
          end;
        end;

        if not FFlat then
        begin
          DrawEdge(Canvas.Handle, r, BDR_RAISEDINNER, BF_TOPLEFT);
          DrawEdge(Canvas.Handle, r, BDR_RAISEDINNER, BF_BOTTOMRIGHT);
        end
        else
        begin
          if (SectionIndex > 1) and (Orientation = hoHorizontal) and (FLineColor <> clNone) then
          begin
            Canvas.MoveTo(r.Left + 1, r.Top);
            Canvas.LineTo(r.Left + 1, r.Bottom);
          end;
          if (SectionIndex > 1) and (Orientation = hoVertical) and (FLineColor <> clNone) then
          begin
            Canvas.MoveTo(r.Left, r.Top + 2);
            Canvas.LineTo(r.Right, r.Top + 2);
          end;
        end;

      {
      with (Owner as TPlanner) do
      begin
        APlannerItem := Items.HeaderFirst(SectionIndex - 2 + FLeftPos);

        while Assigned(APlannerItem) do
        begin
          pr.Left := r.Left + 2;
          pr.Right := r.Right - 2;
          pr.Top := pr.Bottom;
          pr.Bottom := pr.Bottom + FItemHeight;

          APlannerItem.FRepainted := False;
          FGrid.PaintItemCol(Self.Canvas, pr, APlannerItem, False);
          APlannerItem := Items.HeaderNext(SectionIndex - 2 + FLeftPos);
        end;
      end;
      }

        Font := Self.Font;
        Brush.Color := FColor;
        Pen.Color := FLineColor;
        Pen.Width := 1;
      end;

    until (AllPainted);
  end;
end;

procedure TTodoHeader.WMLButtonDown(var Message: TWMLButtonDown);
var
  r: TRect;
  Column: Integer;
begin
  if FSectionDragDrop and not FDragging then
  begin
    FDragStart := XYToSection(Message.XPos, Message.YPos);

    if (FDragStart >= FOffset) then
    begin
      FDragging := True;
      Self.Cursor := crDrag;
      SetCapture(Self.Handle);
    end;
  end;

  if FSectionEdit and not (csDesigning in ComponentState) then
  begin
    if FInplaceEdit.Visible then
    begin
      Sections[FEditSection] := FInplaceEdit.Text;
      SectionWidth[FEditSection] := FEditWidth;
    end;

    FEditSection := XYToSection(Message.xpos, Message.ypos);
    r := GetSectionRect(FEditSection);

    InflateRect(r, -2, -2);
    OffsetRect(r, 2, 2);

    FInplaceEdit.Top := r.Top;
    FInplaceEdit.Left := r.Left;
    FInplaceEdit.Width := r.Right - r.Left;
    FInplaceEdit.Height := r.Bottom - r.Top;
    FInplaceEdit.Color := Self.Color;
    FInplaceEdit.Font.Assign(Self.Font);
    FInplaceEdit.Text := Self.Sections[FEditSection];
    FInplaceEdit.BorderStyle := bsNone;
    FInplaceEdit.Visible := True;
    FInplaceEdit.SelectAll;

    FEditWidth := SectionWidth[FEditSection];

    FInplaceEdit.SetFocus;
  end;

  inherited;

  Column := XYToSection(Message.XPos, Message.YPos);

  with Owner as TCustomTodoList do
  begin
    r := GetSectionRect(Column);

    HideEditor;

    if (Column >= 0) and (Column < Columns.Count) and
      (Abs(Message.Xpos - r.Left) > 4) and (Abs(Message.XPos - r.Right) > 4) then
    begin
      if Columns[Column].TodoData <> tdHandle then
      begin
        if SortColumn = Column then
        begin
          if SortDirection = sdAscending then
            SortDirection := sdDescending
          else
            SortDirection := sdAscending;
        end
        else
        begin
          SortColumn := Column;
        end;
      end;
    end;
  end;

  if Assigned(FOnClick) then
    FOnClick(Self, Column);
end;

procedure TTodoHeader.WMLButtonUp(var Message: TWMLButtonUp);
var
  FDragStop: Integer;
begin
  inherited;

  if Assigned(FOnClick) then
    FOnClick(Self, XYToSection(Message.xpos, Message.ypos));

  if FSectionDragDrop and FDragging then
  begin
    FDragging := False;
    Self.Cursor := crDefault;
    ReleaseCapture;
    if Assigned(FOnDragDrop) then
    begin
      FDragStop := XYToSection(Message.xpos, Message.ypos);

      if (FDragStop >= FOffset) and (FDragStop <> FDragStart) then
        FOnDragDrop(Self, FDragStart, FDragStop);
    end;
  end;

end;

procedure TTodoHeader.WMRButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if Assigned(FOnRightClick) then
    FOnRightClick(Self, XYToSection(Message.xpos, Message.ypos));
end;

procedure TTodoHeader.SetItemHeight(const Value: Integer);
begin
  FItemHeight := Value;
  Invalidate;
end;

procedure TTodoHeader.SetTextHeight(const Value: Integer);
begin
  FTextHeight := Value;
  Invalidate;
end;


procedure TTodoHeader.SetSortDirection(const Value: TSortDirection);
begin
  FSortDirection := Value;
  Invalidate;
end;

procedure TTodoHeader.SetSortedSection(const Value: Integer);
begin
  FSortedSection := Value;
  Invalidate;
end;

procedure TTodoHeader.OwnOnDragDrop(Sender: TObject; FromSection,
  ToSection: Integer);
begin
  FOwner.Columns.Items[FromSection].Index := ToSection;
  FOwner.ColumnsChanged(FOwner);
  if Assigned(FOwner.FOnHeaderDragDrop)
    then FOwner.FOnHeaderDragDrop(FOwner, FromSection, ToSection);
end;

procedure TTodoHeader.HorizontalScroll(X: integer);
begin
  FHScroll := -X;
  Self.SectionWidth[0] := SectionWidth[0] + FHScroll;
  Invalidate;
end;

procedure TTodoHeader.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TTodoHeader.SetActiveColor(const Value: TColor);
begin
  FActiveColor := Value;
  Invalidate;
end;

procedure TTodoHeader.SetActiveColorTo(const Value: TColor);
begin
  FActiveColorTo := Value;
  Invalidate;  
end;

procedure TTodoHeader.SetActiveColumn(const Value: Integer);
begin
  FActiveColumn := Value;
  Invalidate;
end;

{ TCustomTodoList }

procedure TCustomTodoList.CheckChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomTodoList.ColumnsChanged(Sender: TObject);
var
  Col: Integer;
begin
  if (csLoading in ComponentState) then
    Exit;
    
  with FTodoHeader do
  begin
    while (Sections.Count > FTodoListBox.TodoColumns.Count) do
      Sections.Delete(Sections.Count - 1);

    while (Sections.Count < FTodoListBox.TodoColumns.Count) do
      Sections.Add('');

    for Col := 1 to Sections.Count do
    begin
      Sections[Col - 1] := FTodoListBox.TodoColumns.Items[Col - 1].Caption;
      Sections.Objects[Col - 1] := TObject(FTodoListBox.TodoColumns.Items[Col - 1].ImageIndex);
      SectionWidth[Col - 1] := FTodoListBox.TodoColumns.Items[Col - 1].Width;
    end;
  end;

  if (FTodoHeader.Sections.count > 0) then
    FTodoHeader.HorizontalScroll(GetScrollPos(FTodoListBox.Handle, SB_HORZ));
end;

constructor TCustomTodoList.Create(AOwner: TComponent);
begin
  inherited;
  FTodoListBox := TTodoListBox.Create(Self);
  FTodoHeader := TTodoHeader.Create(Self);
  FStatusStrings := TStatusStrings.Create(Self);
  FPriorityStrings := TPriorityStrings.Create(Self);
  FEditColors := TEditColors.Create(Self);
  FCategory := TStringList.Create;
  FCompleteCheck := TCompleteCheck.Create;
  FCompleteCheck.OnChange := CheckChanged;
{$IFDEF USE_PLANNERDATEPICKER}
  FCalendarType := tcPlannerCalendar;
{$ENDIF}

  FTodoHeader.Parent := self;
  FTodoHeader.Visible := True;
  FTodoHeader.BorderStyle := bsNone;
  FTodoHeader.Height := 22;
  FTodoHeader.Align := alTop;
  FTodoHeader.OnSized := SectionSized;
  FTodoHeader.OnClick := HeaderClick;
  FTodoHeader.OnRightClick := HeaderRightClick;

  FTodoListBox.Parent := Self;
  FTodoListBox.Visible := True;
  FTodoListBox.Align := alClient;
  FTodoListBox.BorderStyle := bsNone;
  FTodoListBox.OnColumnsChanged := ColumnsChanged;
  FTodoListBox.ParentShowHint := False;

  FTodoListBox.OnMouseMove := ListMouseMove;
  FTodoListBox.OnMouseDown := ListMouseDown;
  FTodoListBox.OnMouseUp := ListMouseUp;

  FTodoListBox.OnKeyDown := ListKeyDown;
  FTodoListBox.OnKeyUp := ListKeyUp;
  FTodoListBox.OnKeyPress := ListKeyPress;

  FTodoListBox.OnClick := ListClick;
  FTodoListBox.OnDblClick := ListDblClick;

  FTodoListBox.OnDragDrop := ListDragDrop;
  FTodoListBox.OnDragOver := ListDragOver;
  FTodoListBox.OnStartDrag := ListStartDrag;
  FTodoListBox.OnEndDrag := ListEndDrag;

  FTodoListBox.OnEnter := ListEnter;
  FTodoListBox.OnExit := ListExit;
  FTodoListBox.OnSelectItem := ListSelect;
  FTodoListBox.OnHorizontalScroll := LisBoxHorizontalScroll;

  with Columns.Add do
  begin
    TodoData := tdHandle;
    Width := 32;
  end;
  with Columns.Add do
  begin
    TodoData := tdSubject;
    Caption := 'Subject';
  end;
  with Columns.Add do
  begin
    TodoData := tdCompletion;
    Caption := 'Completion';
  end;

  FNullDate := 'None';

  TabStop := False;
  ItemHeight := 22;
  DoubleBuffered := true;

  Width := 250;
  Height := 200;
  FStatusListWidth := -1;
  FPriorityListWidth := -1;
  BorderStyle := bsSingle;

  FAutoInsertItem := True;
  FAutoDeleteItem := True;

  FCompletionGraphic := True;
  FHintShowFullText := False;
  HeaderDragDrop := False;
  Look := esOffice2003Blue;
  GridLines := glsAlways;

  FTotalTimeSuffix := 'h';
end;

procedure TCustomTodoList.CreateWnd;
begin
  inherited;
  if (csDesigning in ComponentState) then
    FTodoListBox.SynchColumns;
end;

procedure TCustomTodoList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FBorderStyle = bsSingle then
    Params.Style := Params.Style or WS_BORDER
  else
    Params.Style := Params.Style and not WS_BORDER;
end;

destructor TCustomTodoList.Destroy;
begin
  FTodoListBox.Free;
  FTodoHeader.Free;
  FTodoHeader := nil;
  FStatusStrings.Free;
  FPriorityStrings.Free;
  FEditColors.Free;
  FCategory.Free;
  FCompleteCheck.Free;
  inherited;
end;

function TCustomTodoList.GetColor: TColor;
begin
  Result := FTodoListBox.Color;
end;

function TCustomTodoList.GetCompletionFont: TFont;
begin
  Result := FTodoListBox.CompletionFont;
end;

function TCustomTodoList.GetDateFormat: string;
begin
  Result := FTodoListBox.DateFormat;
end;

function TCustomTodoList.GetEditable: boolean;
begin
  result := FTodoListBox.Editable;
end;

function TCustomTodoList.GetFont: TFont;
begin
  Result := FTodoListBox.Font;
end;

function TCustomTodoList.GetGridLines: TGridLineStyle;
begin
  Result := FTodoListBox.GridLines;
end;

function TCustomTodoList.GetImages: TImageList;
begin
  Result := FTodoListBox.Images;
end;

function TCustomTodoList.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

function TCustomTodoList.GetPreview: Boolean;
begin
  Result := FTodoListBox.Preview;
end;

function TCustomTodoList.GetPreviewFont: TFont;
begin
  Result := FTodoListBox.PreviewFont;
end;

function TCustomTodoList.GetPriorityFont: TFont;
begin
  Result := FTodoListBox.PriorityFont;
end;

function TCustomTodoList.GetProgressLook: TProgressLook;
begin
  Result := FTodoListBox.ProgressLook;
end;

function TCustomTodoList.GetSelectAllInSubjectEdit: Boolean;
begin
  Result := FTodoListBox.EditSelectAll;
end;

function TCustomTodoList.GetSelectionColor: TColor;
begin
  Result := FTodoListBox.SelectionColor;
end;

function TCustomTodoList.GetSelectionFontColor: TColor;
begin
  Result := FTodoListBox.SelectionFontColor;
end;

function TCustomTodoList.GetShowSelection: Boolean;
begin
  Result := FTodoListBox.ShowSelection;
end;

function TCustomTodoList.GetTodoColumns: TTodoColumnCollection;
begin
  Result := FTodoListBox.TodoColumns;
end;

function TCustomTodoList.GetTodoItems: TTodoItemCollection;
begin
  Result := FTodoListBox.TodoItems;
end;

procedure TCustomTodoList.ListClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCustomTodoList.ListDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TCustomTodoList.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCustomTodoList.ListKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TCustomTodoList.ListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure TCustomTodoList.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  r: TRect;
  ColIdx,ItemIdx: Integer;
  ATodoItem: TTodoItem;
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);

  if (Button = mbRight) and Assigned(FOnItemRightClick) then
  begin
    if FTodoListBox.XYToColItem(X,Y,ColIdx,ItemIdx,r) then
    begin
      //ATodoItem := Items[FTodoListBox.ItemIndex];
      if ItemIdx < Items.Count then
      begin
        ATodoItem := Items[ItemIdx];
        FOnItemRightClick(Self, ATodoItem);
      end;
    end;
  end;
end;

procedure TCustomTodoList.ListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCustomTodoList.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCustomTodoList.ListDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(FOnDragDrop) then
    FOnDragDrop(Self, Source, X, Y);
end;

procedure TCustomTodoList.ListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TCustomTodoList.ListEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEndDrag) then
    FOnEndDrag(Self, Target, X, Y);
end;

procedure TCustomTodoList.ListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if Assigned(FOnStartDrag) then
    FOnStartDrag(Self, DragObject);
end;

procedure TCustomTodoList.Loaded;
begin
  inherited;
  FTodoListBox.SynchItems;
  ColumnsChanged(FTodoListBox);
  if FAutoThemeAdapt then
    ThemeAdapt;
end;

procedure TCustomTodoList.SetFocus;
begin
  FTodoListBox.SetFocus;
end;

function TCustomTodoList.PriorityCommaText: string;
var i: TTodoPriority;
begin
  for i := Low(TTodoPriority) to High(TTodoPriority) do
    Result := Result + ',"' + PriorityStrings[i] + '"';
  Delete(Result, 1, 1);
//  Result := '"Lowest", "Low", "Normal", "High", "Highest"';
end;

function TCustomTodoList.PriorityToString(AValue: TTodoPriority): string;
begin
//  case AValue of
//  tpLowest: Result := 'Lowest';
//  tpLow: Result := 'Low';
//  tpNormal: Result := 'Normal';
//  tpHigh: Result := 'High';
//  tpHighest: Result := 'Highest';
//  else
//    EConversionFunctionException.Create('TodoPriorityToString error. Unknown priority passed as parameter to the function.');
//  end;

  Result := PriorityStrings[AValue];

  if Assigned(FOnPriorityToString) then
    FOnPriorityToString(Self, AValue, Result);
end;

procedure TCustomTodoList.SectionSized(Sender: TObject; SectionIdx,
  SectionWidth: Integer);
begin
  if FTodoListBox.TodoColumns.Count > SectionIdx then
  begin
    FTodoListBox.TodoColumns.Items[SectionIdx].Width := SectionWidth;
  //SetScrollPos(FTodoListBox.Handle,SB_HORZ, 0, false);
  //LisBoxHorizontalScroll(self);
  end;
end;

procedure TCustomTodoList.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomTodoList.SetColor(const Value: TColor);
begin
  FTodoListBox.Color := Value;
end;

procedure TCustomTodoList.SetCompleteCheck(const Value: TCompleteCheck);
begin
  FCompleteCheck.Assign(Value);
end;

procedure TCustomTodoList.SetCompletionFont(const Value: TFont);
begin
  FTodoListBox.CompletionFont.Assign(Value);
end;

procedure TCustomTodoList.SetDateFormat(const Value: string);
begin
  FTodoListBox.DateFormat := Value;
end;

procedure TCustomTodoList.SetEditable(const Value: boolean);
begin
  FTodoListBox.Editable := Value;
end;

procedure TCustomTodoList.SetFont(const Value: TFont);
begin
  FTodoListBox.Font.Assign(Value);
end;

procedure TCustomTodoList.SetGridLines(const Value: TGridLineStyle);
begin
  FTodoListBox.GridLines := Value;
end;

procedure TCustomTodoList.SetImages(const Value: TImageList);
begin
  FTodoListBox.Images := Value;
end;

procedure TCustomTodoList.SetItemHeight(const Value: Integer);
begin
  if Preview then
  begin
    FTodoListBox.ItemHeight := Value + PreviewHeight;
    FItemHeight := FTodoListBox.ItemHeight - PreviewHeight;
  end
  else
  begin
    FTodoListBox.ItemHeight := Value;
    FItemHeight := FTodoListBox.ItemHeight;
  end;
{ Why not just a FItemHeight := Value; at the end of the Set method? Because
the TCustomListBox ItemHeight property refuses to take all values. For
example, if I assign to 0, it will remain set to the default value.
The FItemHeight setting code above exists in order to propagate this
behaviour to the TCustomTodoList level. }
end;

procedure TCustomTodoList.SetPreview(const Value: Boolean);
begin
  FTodoListBox.Preview := Value;
  SetItemHeight(ItemHeight);
end;

procedure TCustomTodoList.SetPreviewFont(const Value: TFont);
begin
  FTodoListBox.PreviewFont.Assign(Value);
end;

procedure TCustomTodoList.SetPreviewHeight(const Value: Integer);
begin
  FPreviewHeight := Value;
  SetItemHeight(ItemHeight);
end;

procedure TCustomTodoList.SetPriorityFont(const Value: TFont);
begin
  FTodoListBox.PriorityFont.Assign(Value);
end;

procedure TCustomTodoList.SetProgressLook(const Value: TProgressLook);
begin
  FTodoListBox.ProgressLook.Assign(Value);
end;

procedure TCustomTodoList.SetSelectAllInSubjectEdit(const Value: Boolean);
begin
  FTodoListBox.EditSelectAll := Value;
end;

procedure TCustomTodoList.SetSelectionColor(const Value: TColor);
begin
  FTodoListBox.SelectionColor := Value;
end;

procedure TCustomTodoList.SetSelectionFontColor(const Value: TColor);
begin
  FTodoListBox.SelectionFontColor := Value;
end;

procedure TCustomTodoList.SetShowSelection(const Value: Boolean);
begin
  FTodoListBox.ShowSelection := Value;
end;

procedure TCustomTodoList.SetTodoColumns(const Value: TTodoColumnCollection);
begin
  FTodoListBox.TodoColumns.Assign(Value);
end;

procedure TCustomTodoList.SetTodoItems(const Value: TTodoItemCollection);
begin
  FTodoListBox.TodoItems.Assign(Value);
end;

function TCustomTodoList.StatusCommaText: string;
var i: TTodoStatus;
begin
  for i := Low(TTodoStatus) to High(TTodoStatus) do
    Result := Result + ',"' + StatusStrings[i] + '"';
  Delete(Result, 1, 1);
//  Result:='"Not started", "In progress", "Completed", "Deferred"';
end;

function TCustomTodoList.StatusToString(AValue: TTodoStatus): string;
begin
  Result := StatusStrings[AValue];

  if Assigned(FOnStatusToString) then
    FOnStatusToString(Self, AValue, Result);
end;

function TCustomTodoList.StringToPriority(AValue: string): TTodoPriority;
var
  i: TTodoPriority;
  lFound: Boolean;
begin
  AValue := LowerCase(AValue);

  lFound := False;
  for i := Low(TTodoPriority) to High(TTodoPriority) do
    if LowerCase(PriorityStrings[i]) = AValue then begin
      lFound := True;
      Result := i;
    end;

  if not lFound then
    raise EConversionFunctionException.Create('TodoPriorityFromString error. The parameter "' + AValue + '" is not a valid priority string.');

  if Assigned(FOnStringToPriority) then
    FOnStringToPriority(Self, AValue, Result);
end;

function TCustomTodoList.StringToStatus(AValue: string): TTodoStatus;
var
  i: TTodoStatus;
  lFound: Boolean;
begin
  lFound := False;
  AValue := LowerCase(AValue);

  for i := Low(TTodoStatus) to High(TTodoStatus) do
    if LowerCase(StatusStrings[i]) = AValue then
    begin
      lFound := True;
      Result := i;
    end;

  if not lFound then
    raise EConversionFunctionException.Create('TodoStatusFromString error. The parameter "' + AValue + '" is not a valid status string.');

  if Assigned(FOnStringToStatus) then
    FOnStringToStatus(Self, AValue, Result);
end;

function TCustomTodoList.CompareItems(A, B: Integer): Integer;
var
  Item1, Item2: TTodoItem;

begin
  Item1 := Items.Items[A];
  Item2 := Items.Items[B];

  Result := 0;

  case Columns.Items[FSortColumn].TodoData of
    tdComplete:
      begin

        if Item1.Complete and not Item2.Complete then
          Result := 1;

        if not Item1.Complete and Item2.Complete then
          Result := -1;
      end;
    tdCompletion:
      begin
        if Item1.Completion > Item2.Completion then
          Result := 1 else
          if Item1.Completion < Item2.Completion then
            Result := -1;
      end;
    tdCompletionDate:
      begin
        if Item1.CompletionDate > Item2.CompletionDate then
          Result := 1 else
          if Item1.CompletionDate < Item2.CompletionDate then
            Result := -1;
      end;
    tdCreationDate:
      begin
        if Item1.CreationDate > Item2.CreationDate then
          Result := 1 else
          if Item1.CreationDate < Item2.CreationDate then
            Result := -1;
      end;
    tdDueDate:
      begin
        if Item1.DueDate > Item2.DueDate then
          Result := 1 else
          if Item1.DueDate < Item2.DueDate then
            Result := -1;
      end;
    tdPriority:
      begin
        if Item1.Priority > Item2.Priority then
          Result := 1 else
          if Item1.Priority < Item2.Priority then
            Result := -1;
      end;
    tdStatus:
      begin
        if Item1.Status > Item2.Status then
          Result := 1 else
          if Item1.Status < Item2.Status then
            Result := -1;
      end;
    tdTotalTime:
      begin
        if Item1.TotalTime > Item2.TotalTime then
          Result := 1 else
            if Item1.TotalTime < Item2.TotalTime then
              Result := -1;
      end;
    tdSubject:
      begin
        Result := AnsiStrComp(PChar(Item1.Subject), PChar(Item2.Subject));
      end;
    tdCategory:
      begin
        Result := AnsiStrComp(PChar(Item1.Category), PChar(Item2.Category));
      end;
    tdResource:
      begin
        Result := AnsiStrComp(PChar(Item1.Resource), PChar(Item2.Resource));
      end;
    tdNotes:
      begin
        Result := AnsiStrComp(PChar(Item1.Notes.Text), PChar(Item2.Notes.Text));
      end;
    tdProject:
      begin
        Result := AnsiStrComp(PChar(Item1.Project), PChar(Item2.Project));
      end;
    tdImage:
      if Item1.ImageIndex > Item2.ImageIndex then
        Result := 1 else
        if Item1.ImageIndex < Item2.ImageIndex then
          Result := -1;
  end;

  if FSortDirection = sdDescending then
    Result := Result * -1;
end;

procedure TCustomTodoList.SwapItems(A, B: Integer);
var
  SI: TTodoItem;
  SK: string;
begin
  SI := Items.Add;
  SI.Assign(Items.Items[A]);
  SK := Items.Items[A].DBKey; // public property
  Items.Items[A].Assign(Items.Items[B]);
  Items.Items[A].DBKey := Items.Items[B].DBKey;
  Items.Items[B].Assign(SI);
  Items.Items[B].DBKey := SK;
  SI.Free;
end;

procedure TCustomTodoList.QuickSortItems(Left, Right: Integer);
var
  i, j: Integer;
  Mid: Integer;

begin
  i := Left;
  j := Right;

// get middle item here
  Mid := (Left + Right) shr 1;

  repeat
    while (CompareItems(Mid, i) > 0) and (i < Right) do Inc(i);
    while (CompareItems(Mid, j) < 0) and (j > Left) do Dec(j);
    if i <= j then
    begin
      if i <> j then
      begin
        if CompareItems(i, j) <> 0 then
        begin
          if i = FNewIdx then
            FNewIdx := j
          else
            if j = FNewIdx then
              FNewIdx := i;

          SwapItems(i, j);
        end;
      end;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if (Left < j) then
    QuicksortItems(Left, j);
  if (i < Right) then
    QuickSortItems(i, right);
end;


procedure TCustomTodoList.Sort;
begin
  FNewIdx := FTodoListBox.ItemIndex;

  if Items.Count > 1 then
    QuickSortItems(0, Items.Count - 1);

  FTodoListBox.ItemIndex := FNewIdx;

  ListSelect(Self);
end;

procedure TCustomTodoList.ListEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TCustomTodoList.ListExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

function TCustomTodoList.GetDragCursor: TCursor;
begin
  Result := FTodoListBox.DragCursor;
end;

function TCustomTodoList.GetDragKind: TDragKind;
begin
  Result := FTodoListBox.DragKind;
end;

function TCustomTodoList.GetDragMode: TDragMode;
begin
  Result := FTodoListBox.DragMode;
end;

procedure TCustomTodoList.SetDragCursor(const Value: TCursor);
begin
  FTodoListBox.DragCursor := Value;
end;

procedure TCustomTodoList.SetDragKind(const Value: TDragKind);
begin
  FTodoListBox.DragKind := Value;
end;

procedure TCustomTodoList.SetDragModeEx(const Value: TDragMode);
begin
  FTodoListBox.DragMode := Value;
end;

procedure TCustomTodoList.SetSortColumn(const Value: Integer);
begin
  if FSortColumn <> Value then
  begin
    FSortColumn := Value;
    FTodoHeader.SortedSection := Value;
    if FSorted and (FSortColumn >= 0) then
      Sort;
  end;
end;

procedure TCustomTodoList.SetSortDirection(const Value: TSortDirection);
begin
  if FSortDirection <> Value then
  begin
    FSortDirection := Value;
    FTodoHeader.SortDirection := Value;
    if FSorted then Sort;
  end;
end;

procedure TCustomTodoList.SetSorted(const Value: Boolean);
begin
  FSorted := Value;
  FTodoHeader.SortShow := Value;
  FTodoHeader.Invalidate;

  if (FSorted = True) and (SortColumn >= 0) and (SortColumn < Columns.Count) then
    Sort;
end;

procedure TCustomTodoList.HeaderClick(Sender: TObject; Section: Integer);
begin
  if Assigned(FOnHeaderClick) then
    FOnHeaderClick(Self, Section);
end;

procedure TCustomTodoList.HeaderRightClick(Sender: TObject; Section: Integer);
begin
  if Assigned(FOnHeaderRightClick) then
    FOnHeaderRightClick(Self, Section);
end;

procedure TCustomTodoList.HideEditor;
begin
  FTodoListBox.EditorOnExit(Self);
end;

procedure TCustomTodoList.ShowEditor(Index, Column: Integer);
var
  Msg: TMessage;
begin
  FTodoListBox.StartEdit(Index, Column, False, Msg, 0, 0, #0);
end;

function TCustomTodoList.GetSelected: TTodoItem;
begin
  Result := nil;
  if FTodoListBox.ItemIndex >= 0 then
    Result := Items[FTodoListBox.ItemIndex];
end;

function TCustomTodoList.GetHeaderFont: TFont;
begin
  Result := FTodoHeader.Font;
end;

procedure TCustomTodoList.SetHeaderFont(const Value: TFont);
begin
  FTodoHeader.Font.Assign(Value);
end;

function TCustomTodoList.AllowAutoDelete(ATodoItem: TTodoItem): Boolean;
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnItemDelete) then
    FOnItemDelete(Self, ATodoItem, Allow);

  Result := Allow;
end;

function TCustomTodoList.AllowAutoInsert(ATodoItem: TTodoItem): Boolean;
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnItemInsert) then
    FOnItemInsert(Self, ATodoItem, Allow);
  Result := Allow;
end;

procedure TCustomTodoList.EditDone;
begin
  if Assigned(FOnEditDone) then
    FOnEditDone(Self);
end;

procedure TCustomTodoList.EditStart;
begin
  if Assigned(FOnEditStart) then
    FOnEditStart(Self);
end;

function TCustomTodoList.GetEditColumn: Integer;
begin
  Result := FTodoListBox.FFocusColumn;
end;

procedure TCustomTodoList.SetEditColumn(const Value: Integer);
begin
  FTodoListBox.FFocusColumn := Value;
end;

procedure TCustomTodoList.ListSelect(Sender: TObject);
var
  ATodoItem: TTodoItem;
begin
  if FTodoListBox.ItemIndex >= 0 then
    ATodoItem := Items[FTodoListBox.ItemIndex]
  else
    ATodoItem := nil;

  ItemSelect(ATodoItem);
end;

procedure TCustomTodoList.ItemSelect(ATodoItem: TTodoItem);
begin
  if Assigned(FOnItemSelect) then
    FOnItemSelect(Self, ATodoItem);
end;

procedure TCustomTodoList.AddColumn(Data: TTodoData; ACaption: string);
begin
  with Columns.Add do
  begin
    TodoData := Data;
    Caption := ACaption;
  end
end;

procedure TCustomTodoList.RemoveColumn(Data: TTodoData);
var
  i: Integer;
begin
  for i := 1 to Columns.Count do
  begin
    if Columns[i - 1].TodoData = Data then
    begin
      Columns[i - 1].Free;
      Break;
    end;
  end;
end;

function TCustomTodoList.GetTabStopEx: Boolean;
begin
  Result := FTodoListBox.TabStop;
end;

procedure TCustomTodoList.SetTabStopEx(const Value: Boolean);
begin
  inherited TabStop := False;
  FTodoListBox.TabStop := Value;
end;

function TCustomTodoList.GetTabOrderEx: Integer;
begin
  Result := FTodoListBox.TabOrder;
end;

procedure TCustomTodoList.SetTabOrderEx(const Value: Integer);
begin
  FTodoListBox.TabOrder := Value;
end;

procedure TCustomTodoList.SaveToStream(S: TStream);
var i: Integer;
begin
  i := Items.Count;
  S.Write(i, 4);
  for i := 0 to Items.Count - 1 do Items[i].SaveToStream(S);
end;

procedure TCustomTodoList.LoadFromStream(S: TStream);
var
  i, Count: Integer;
begin
  Items.Clear;
  if S.Size > 0 then
  begin
    S.Read(Count, 4);
    for i := 1 to Count do
      Items.Add.LoadFromStream(S);
    SortColumn := -1;
  end;
end;

procedure TCustomTodoList.LoadFromFile(FileName: string);
var S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomTodoList.SaveToFile(FileName: string);
var S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomTodoList.LoadColumns(FileName: string);
var
  S: TFileStream;
  Count, i: Integer;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Columns.Clear;
    Count := ReadInteger(S);
    for i := 1 to Count do Columns.Add.LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomTodoList.SaveColumns(FileName: string);
var
  S: TFileStream;
  i: Integer;
begin
  S := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    WriteInteger(S, Columns.Count);
    for i := 0 to Columns.Count - 1 do Columns[i].SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TCustomTodoList.SetCompletionGraphic(const Value: Boolean);
begin
  FCompletionGraphic := Value;
  Invalidate;
end;

procedure TCustomTodoList.SetHeaderDragDrop(const Value: Boolean);
begin
  FHeaderDragDrop := Value;
  FTodoHeader.SectionDragDrop := Value;
end;

procedure TCustomTodoList.SetHintShowFullText(const Value: Boolean);
begin
  FHintShowFullText := Value;
  FTodoListBox.ShowHint := Value;
end;

procedure TCustomTodoList.NCPaintProc;

var
  DC: HDC;
  ARect: TRect;
  Canvas: TCanvas;
begin
  if BorderStyle = bsNone then Exit;

  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;

    GetWindowRect(Handle, ARect);

    if (Parent is TWinControl) then
    begin
      Canvas.Pen.Color := clGray;
      Canvas.MoveTo(0, Height);
      Canvas.LineTo(0, 0);
      Canvas.LineTo(Width - 1, 0);
      Canvas.LineTo(Width - 1, Height - 1);
      Canvas.LineTo(0, Height - 1);
    end;

    Canvas.Free;
  finally

    ReleaseDC(Handle, DC);
  end;
end;

procedure TCustomTodoList.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_NCPAINT then
    NCPaintProc;
    
  if (Message.Msg = WM_THEMECHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  if (Message.Msg = CM_SYSFONTCHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

end;

procedure TCustomTodoList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if Assigned(Parent) then
    if Parent.HandleAllocated then
      NCPaintProc;
end;

function TCustomTodoList.FormatDateTimeEx(Format: string;
  dt: TDateTime): string;
begin
  Result := FormatDateTime(Format, dt);

  if (dt = 0) and (FNullDate <> '') then
    Result := FNullDate;
end;

procedure TCustomTodoList.SetNullDate(const Value: string);
begin
  FNullDate := Value;
  Invalidate;
end;

function TCustomTodoList.GetEditItem: TTodoItem;
begin
  Result := List.EditedItem;
end;

procedure TCustomTodoList.CompleteClick(ItemIndex: Integer);
begin
  if Assigned(FOnCompleteClick) then
    FOnCompleteClick(Self, ItemIndex);
end;

function TCustomTodoList.GetSelectionColorTo: TColor;
begin
  Result := FTodoListBox.SelectionColorTo;
end;

procedure TCustomTodoList.SetSelectionColorTo(const Value: TColor);
begin
  FTodoListBox.SelectionColorTo := Value;
end;

function TCustomTodoList.GetActiveColumnColor: TColor;
begin
  Result := FTodoListBox.ActiveColumnColor;
end;

procedure TCustomTodoList.SetActiveColumnColor(const Value: TColor);
begin
  FTodoListBox.ActiveColumnColor := Value;
end;

function TCustomTodoList.GetUseTab: Boolean;
begin
  Result := FTodoListBox.UseTab;
end;

procedure TCustomTodoList.SetUseTab(const Value: Boolean);
begin
  FTodoListBox.UseTab := Value;
end;

function TCustomTodoList.GetMultiSelect: Boolean;
begin
  Result := FTodoListBox.MultiSelect;
end;

procedure TCustomTodoList.SetMultiSelect(const Value: Boolean);
begin
  FTodoListBox.MultiSelect := Value;
end;

function TCustomTodoList.GetSelCount: integer;
begin
  Result := FTodoListBox.SelCount;
end;

function TCustomTodoList.GetItemSelected(Index: Integer): Boolean;
begin
  Result := FTodoListBox.Selected[Index];
end;

procedure TCustomTodoList.SetItemSelected(Index: Integer;
  const Value: Boolean);
begin
  FTodoListBox.Selected[Index] := Value;
end;

procedure TCustomTodoList.SelectAll;
{$IFNDEF DELPHI6_LVL}
var
  i: Integer;
{$ENDIF}  
begin
  {$IFDEF DELPHI6_LVL}
  FTodoListBox.SelectAll;
  {$ENDIF}
  {$IFNDEF DELPHI6_LVL}
  for i := 1 to FTodoListBox.Items.Count do
    FTodoListBox.Selected[i - 1] := True;
  {$ENDIF}  
end;

function TCustomTodoList.GetShowPriorityText: Boolean;
begin
  Result := FTodoListBox.ShowPriorityText;
end;

procedure TCustomTodoList.SetShowPriorityText(const Value: Boolean);
begin
  FTodoListBox.ShowPriorityText := Value;
end;

procedure TCustomTodoList.LisBoxHorizontalScroll(Sender: TObject);
begin
//Showmessage(inttostr(GetScrollPos(FTodoListBox.Handle,SB_HORZ)));
  ColumnsChanged(self);
//FTodoHeader.HorizontalScroll(GetScrollPos(FTodoListBox.Handle,SB_HORZ));
end;

procedure TCustomTodoList.SetCategory(const Value: TStringList);
begin
  FCategory.Assign(Value);
end;

procedure TCustomTodoList.ExportToCSV(FileName: string);
begin
  FTodoListBox.OutputToCSV(FileName, false);
end;

procedure TCustomTodoList.ImportFromCSV(FileName: string);
begin
  FTodoListBox.InputFromCSV(FileName, false);
end;

function TCustomTodoList.GetActiveItemColor: TColor;
begin
  Result := FTodoListBox.ActiveItemColor;
end;

procedure TCustomTodoList.SetActiveItemColor(const Value: TColor);
begin
  FTodoListBox.ActiveItemColor := Value;
end;

function TCustomTodoList.GetActiveItemColorTo: TColor;
begin
  Result := FTodoListBox.ActiveItemColorTo;
end;

procedure TCustomTodoList.SetActiveItemColorTo(const Value: TColor);
begin
  FTodoListBox.ActiveItemColorTo := Value;
end;

function TCustomTodoList.GetScrollHorizontal: Boolean;
begin
  Result := FTodoListBox.ScrollHorizontal;
end;

procedure TCustomTodoList.SetScrollHorizontal(const Value: Boolean);
begin
  FTodoListBox.ScrollHorizontal := Value;
end;

function TCustomTodoList.GetHeaderColor: TColor;
begin
  Result := FTodoHeader.Color;
end;

function TCustomTodoList.GetHeaderColorTo: TColor;
begin
  Result := FTodoHeader.ColorTo;
end;

procedure TCustomTodoList.SetHeaderColor(const Value: TColor);
begin
  FTodoHeader.Color := Value;
end;

procedure TCustomTodoList.SetHeaderColorTo(const Value: TColor);
begin
  FTodoHeader.ColorTo := Value;
end;

function TCustomTodoList.GetHandleGlyph: TBitmap;
begin
  Result := FTodoListBox.HandleGlyph;
end;

procedure TCustomTodoList.SetHandleGlyph(const Value: TBitmap);
begin
  FTodoListBox.HandleGlyph := Value;
end;

procedure TCustomTodoList.SetHeaderImages(const Value: TImageList);
begin
  FTodoHeader.Images := Value;
  Invalidate;
end;

procedure TCustomTodoList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = HeaderImages) then
    HeaderImages := nil;
  inherited;
end;

function TCustomTodoList.GetHeaderImages: TImageList;
begin
   if Assigned(FTodoHeader) then
     Result := FTodoHeader.Images
   else
     Result := nil;  
end;

function TCustomTodoList.GetHeaderHeight: Integer;
begin
  Result := FTodoHeader.Height;
end;

procedure TCustomTodoList.SetHeaderHeight(const Value: Integer);
begin
  FTodoHeader.Height := Value;
end;

function TCustomTodoList.GetGridLineColor: TColor;
begin
  Result := FTodoListBox.GridLineColor;
end;

procedure TCustomTodoList.SetGridLineColor(const Value: TColor);
begin
  FTodoListBox.GridLineColor := Value;
end;

function TCustomTodoList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TCustomTodoList.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;

function TCustomTodoList.GetPreviewColor: TColor;
begin
  Result := FTodoListBox.PreviewColor;
end;

function TCustomTodoList.GetPreviewColorTo: TColor;
begin
  Result := FTodoListBox.PreviewColorTo;
end;

procedure TCustomTodoList.SetPreviewColor(const Value: TColor);
begin
  FTodoListBox.PreviewColor := Value;
end;

procedure TCustomTodoList.SetPreviewColorTo(const Value: TColor);
begin
  FTodoListBox.PreviewColorTo := Value;
end;

function TCustomTodoList.GetStretchLastColumn: Boolean;
begin
  Result := FTodoListBox.StretchLastColumn;
end;

procedure TCustomTodoList.SetStretchLastColumn(const Value: Boolean);
begin
  FTodoListBox.StretchLastColumn := Value;
end;

function TCustomTodoList.GetHeaderActiveColor: TColor;
begin
  Result := FTodoHeader.ActiveColor;
end;

function TCustomTodoList.GetHeaderActiveColorTo: TColor;
begin
  Result := FTodoHeader.ActiveColorTo;
end;

procedure TCustomTodoList.SetHeaderActiveColor(const Value: TColor);
begin
  FTodoHeader.ActiveColor := Value;
end;

procedure TCustomTodoList.SetHeaderActiveColorTo(const Value: TColor);
begin
  FTodoHeader.ActiveColorTo := Value;
end;

procedure TCustomTodoList.SetComponentStyle(AStyle: TTMSStyle);
begin
  Look := TTodoListStyle(AStyle);
end;

procedure TCustomTodoList.SetLook(const Value: TTodoListStyle);
begin
  FLook := Value;
  PreviewColor := clNone;
  PreviewColorTo := clNone;
  PreviewFont.Color := clBlack;
  ActiveItemColor := clNone;
  ActiveItemColorTo := clNone;
  SelectionColor := clHighLight;
  SelectionColorTo := clNone;

  case FLook of
  esOffice2003Blue:
    begin
      HeaderColor := $FCE1CB;
      HeaderColorTo := $E0A57D;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;
    end;
  esOffice2003Olive:
    begin
      HeaderColor := $CFF0EA;
      HeaderColorTo := $8CC0B1;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;
    end;
  esOffice2003Silver:
    begin
      HeaderColor := $ECE2E1;
      HeaderColorTo := $B39698;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;
    end;
  esOffice2003Classic:
    begin
      HeaderColor := clWhite;
      HeaderColorTo := $ccd4d8;
      HeaderActiveColor := $d8d5d4;
      HeaderActiveColorTo := $d8d5d4;
    end;
  esOffice2007Luna:
    begin
      HeaderColor := clWhite;
      HeaderColorTo := $FFD2AF;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;

      PreviewColor := clWhite;
      PreviewColorTo := $fff6ec;
      PreviewFont.Color := $808080;
      ActiveItemColor := $e8c8b3;
      ActiveItemColorTo := $e8c8b3;
      SelectionColor := $7a4c29;
      SelectionColorTo := $7a4c29;
    end;
  esOffice2007Obsidian:
    begin
      HeaderColor := $F2F1F0;
      HeaderColorTo := $C9C2BD;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;

      PreviewColor := clWhite;
      PreviewColorTo := $fff6ec;
      PreviewFont.Color := $808080;
      ActiveItemColor := $e8c8b3;
      ActiveItemColorTo := $e8c8b3;
      SelectionColor := $7a4c29;
      SelectionColorTo := $7a4c29;
    end;
  esOffice2007Silver:
    begin
      HeaderColor := clWhite;
      HeaderColorTo := $DCD7D4;
      HeaderActiveColor := $94E6FB;
      HeaderActiveColorTo := $1595EE;

      PreviewColor := clWhite;
      PreviewColorTo := $fff6ec;
      PreviewFont.Color := $808080;
      ActiveItemColor := $e8c8b3;
      ActiveItemColorTo := $e8c8b3;
      SelectionColor := $7a4c29;
      SelectionColorTo := $7a4c29;
    end;
  esWindowsXP:
    begin
      HeaderColor := clBtnFace;
      HeaderColorTo := clBtnFace;
      HeaderActiveColor := clBtnFace;
      HeaderActiveColorTo := clBtnFace;
    end;
  esWhidbey:
    begin
      HeaderColor := $EBEEEF;//$808080;
      HeaderColorTo := $7E9898;//$808080;
      HeaderActiveColor := $94E6FB;//$D8D5D4;
      HeaderActiveColorTo := $1595EE;//$D8D5D4;
    end;
   esWindowsVista:
    begin
      HeaderColor := $FFFDF9;
      HeaderColorTo := $FFFAF0;
      HeaderActiveColor := $FEF9F0;
      HeaderActiveColorTo := $FDF0D7;
    end;
   esWindows7:
    begin
      HeaderColor := $FDFBFA;
      HeaderColorTo := $FDF3EB;
      HeaderActiveColor := $FCEBDC;
      HeaderActiveColorTo := $FCDBC1;
    end;
   esTerminal:
    begin
      HeaderColor := clBtnFace;
      HeaderColorTo := clBtnFace;
      HeaderActiveColor := clHighLight;
      HeaderActiveColorTo := clHighLight;
    end;
   esOffice2010Blue:
    begin
      HeaderColor := $FDF6EF;
      HeaderColorTo := $F0DAC7;
      HeaderActiveColor := $7BEEFF;
      HeaderActiveColorTo := $6CD0FF;

      SelectionColor := $6CD0FF;
      SelectionColorTo := $6CD0FF;
    end;
   esOffice2010Silver:
    begin
      HeaderColor := $FFFFFF;
      HeaderColorTo := $EDE5E0;
      HeaderActiveColor := $7BEEFF;
      HeaderActiveColorTo := $6CD0FF;

      SelectionColor := $6CD0FF;
      SelectionColorTo := $6CD0FF;
    end;
   esOffice2010Black:
    begin
      HeaderColor := $BFBFBF;
      HeaderColorTo := $919191;
      HeaderActiveColor := $7BEEFF;
      HeaderActiveColorTo := $6CD0FF;

      SelectionColor := $6CD0FF;
      SelectionColorTo := $6CD0FF;
    end;
  esWindows8, esWindows10:
    begin
      HeaderActiveColor := $F7F6F5;
      HeaderActiveColorTo := $F7F6F5;
      HeaderColor := $F7F6F5;
      HeaderColorTo := $F7F6F5;

      SelectionColor := $DAA026;
      SelectionColorTo := $DAA026;
    end;
  esOffice2013White:
    begin
      HeaderActiveColor := clWhite;
      HeaderActiveColorTo := clWhite;
      HeaderColor := clWhite;
      HeaderColorTo := clWhite;

      SelectionColor := $FF9933;
      SelectionColorTo := $FF9933;
    end;
  esOffice2013LightGray:
    begin
      HeaderActiveColor := $FAFAFA;
      HeaderActiveColorTo := $FAFAFA;
      HeaderColor := $FAFAFA;
      HeaderColorTo := $FAFAFA;

      SelectionColor := $FF9933;
      SelectionColorTo := $FF9933;
    end;
  esOffice2013Gray:
    begin
      HeaderActiveColor := $F3F3F3;
      HeaderActiveColorTo := $F3F3F3;
      HeaderColor := $F3F3F3;
      HeaderColorTo := $F3F3F3;

      SelectionColor := $FF9933;
      SelectionColorTo := $FF9933;
    end;
  esOffice2016White:
    begin
      HeaderActiveColor := clWhite;
      HeaderActiveColorTo := clWhite;
      HeaderColor := clWhite;
      HeaderColorTo := clWhite;

      SelectionColor := $E3BDA3;
      SelectionColorTo := $E3BDA3;
    end;
  esOffice2016Gray:
    begin
      HeaderActiveColor := $444444;
      HeaderActiveColorTo := $444444;
      HeaderColor := $444444;
      HeaderColorTo := $444444;

      SelectionColor := $E3BDA3;
      SelectionColorTo := $E3BDA3;
    end;
  esOffice2016Black:
    begin
      HeaderActiveColor := $444444;
      HeaderActiveColorTo := $444444;
      HeaderColor := $444444;
      HeaderColorTo := $444444;

      SelectionColor := $444444;
      SelectionColorTo := $444444;
    end;

  end;
end;

procedure TCustomTodoList.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
  xpBlue: Look := esOffice2003Blue;
  xpGreen: Look := esOffice2003Olive;
  xpGray: Look := esOffice2003Silver;
  else
    Look := esOffice2003Classic;
  end;
end;

function TCustomToDoList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TCustomToDoList.SetVersion(const Value: string);
begin

end;

procedure TCustomTodoList.SetTotalTimeSuffix(const Value: string);
begin
  FTotalTimeSuffix := Value;
  FTodoListBox.TotalTimeSuffix := Value;
end;

{ TProgressLook }

procedure TProgressLook.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TProgressLook.Create;
begin
  inherited;
  
  FCompleteColor := clRed;
  FCompleteFontColor := clBlue;
  FUnCompleteColor := clNone;
  FUnCompleteFontColor := clWindowText;

  FLevel0Color := clLime;
  FLevel0ColorTo := $00E1FFE1;
  FLevel1Color := clYellow;
  FLevel1ColorTo := $00CAFFFF;
  FLevel2Color := $0053A9FF;
  FLevel2ColorTo := $00A8D3FF;
  FLevel3Color := clRed;
  FLevel3ColorTo := $00CACAFF;

  FLevel1Perc := 70;
  FLevel2Perc := 90;

  FBorderColor := clBlack;
  FShowBorder := false;
  FStacked := false;
  FShowPercentage := true;
  FCompletionSmooth := true;
  FShowGradient := true;
  FSteps := 11;
end;

procedure TProgressLook.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetCompleteColor(const Value: TColor);
begin
  if FCompleteColor <> Value then
  begin
    FCompleteColor := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetCompleteFontColor(const Value: TColor);
begin
  if FCompleteFontColor <> Value then
  begin
    FCompleteFontColor := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetCompletionSmooth(const Value: Boolean);
begin
  if FCompletionSmooth <> Value then
  begin
    FCompletionSmooth := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel0Color(const Value: TColor);
begin
  if FLevel0Color <> Value then
  begin
    FLevel0Color := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel0ColorTo(const Value: TColor);
begin
  if FLevel0ColorTo <> Value then
  begin
    FLevel0ColorTo := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel1Color(const Value: TColor);
begin
  if FLevel1Color <> Value then
  begin
    FLevel1Color := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel1ColorTo(const Value: TColor);
begin
  if FLevel1ColorTo <> Value then
  begin
    FLevel1ColorTo := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel1Perc(const Value: Integer);
begin
  if FLevel1Perc <> Value then
  begin
    FLevel1Perc := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel2Color(const Value: TColor);
begin
  if FLevel2Color <> Value then
  begin
    FLevel2Color := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel2ColorTo(const Value: TColor);
begin
  if FLevel2ColorTo <> Value then
  begin
    FLevel2ColorTo := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel2Perc(const Value: Integer);
begin
  if FLevel2Perc <> Value then
  begin
    FLevel2Perc := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel3Color(const Value: TColor);
begin
  if FLevel3Color <> Value then
  begin
    FLevel3Color := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetLevel3ColorTo(const Value: TColor);
begin
  if FLevel3ColorTo <> Value then
  begin
    FLevel3ColorTo := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetShowBorder(const Value: Boolean);
begin
  if FShowBorder <> Value then
  begin
    FShowBorder := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetShowGradient(const Value: Boolean);
begin
  if FShowGradient <> Value then
  begin
    FShowGradient := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetShowPercentage(const Value: Boolean);
begin
  if FShowPercentage <> Value then
  begin
    FShowPercentage := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetStacked(const Value: Boolean);
begin
  if FStacked <> Value then
  begin
    FStacked := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetSteps(const Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetUnCompleteColor(const Value: TColor);
begin
  if FUnCompleteColor <> Value then
  begin
    FUnCompleteColor := Value;
    Changed;
  end;
end;

procedure TProgressLook.SetUnCompleteFontColor(const Value: TColor);
begin
  if FUnCompleteFontColor <> Value then
  begin
    FUnCompleteFontColor := Value;
    Changed;
  end;
end;

procedure TTodoListBox.EditorOnExit(Sender: TObject);
var
  Date: TDateTime;
  TodoData: TTodoData;
begin
  if ActiveEditor = nil then
    Exit;
    
{ This event handler executes when an in-place editor component loses focus.
  The task of this event handler is to transfer the data to the TodoListBox, then
  to make the editor disappear. }

  if Assigned(EditedColumn)
    then TodoData := EditedColumn.TodoData
  else TodoData := tdNotes;

  if ActiveEditor = StringEditor then // string
  begin
    if TodoData = tdSubject then
      EditedItem.Subject := StringEditor.Text;

    if TodoData = tdResource then
      EditedItem.Resource := StringEditor.Text;

    if TodoData = tdProject then
      EditedItem.Project := StringEditor.Text;
  end
{$IFDEF USE_PLANNERDATEPICKER}
  else if ActiveEditor = PlannerDateEditor then // date
  begin
    if (PlannerDateEditor.Text = '') then
      Date := 0
    else
      Date := PlannerDateEditor.Calendar.Date;

    if TodoData = tdCompletionDate then
      EditedItem.CompletionDate := Date
    else if TodoData = tdCreationDate then
      EditedItem.CreationDate := Date
    else if TodoData = tdDueDate then
      EditedItem.DueDate := Date;
  end
{$ENDIF}
  else if ActiveEditor = DefaultDateEditor then // date
  begin
    Date := DefaultDateEditor.Date;

    if TodoData = tdCompletionDate then
      EditedItem.CompletionDate := Date
    else if TodoData = tdCreationDate then
      EditedItem.CreationDate := Date
    else if TodoData = tdDueDate then
      EditedItem.DueDate := Date;
  end
  else if ActiveEditor = IntegerEditor then // integer
  begin
  {
  if TodoData = tdTotalTime then
    EditedItem.TotalTime := IntegerEditor.Value
  else if TodoData = tdCompletion then
  begin
    EditedItem.Completion := Min(Max(IntegerEditor.Value, 0), 100);
    EditedItem.Complete := EditedItem.Completion = 100;
  end; }
  end
  else if ActiveEditor = StringListEditor then // string list
  begin
    EditedItem.Notes.Text := StringListEditor.Lines.Text;
  end
  else if ActiveEditor = PriorityEditor then // priority
  begin
    if TodoData = tdPriority then
      EditedItem.Priority := FOwner.StringToPriority(PriorityEditor.Items[PriorityEditor.ItemIndex]);
  end
  else if ActiveEditor = StatusEditor then
  begin
    if TodoData = tdStatus then
      EditedItem.Status := FOwner.StringToStatus(StatusEditor.Items[StatusEditor.ItemIndex]);
    if ToDoData = tdCategory then
    begin
      if (StatusEditor.ItemIndex >= 0) and (StatusEditor.ItemIndex < StatusEditor.Items.Count) then
        EditedItem.Category := StatusEditor.Items[StatusEditor.ItemIndex]
      else
        EditedItem.Category := '';
    end;
  end
  else if ActiveEditor = FNumericOnlyEditor then
  begin
    if TodoData = tdCompletion then
    begin
      EditedItem.Completion := Min(Max(strtoint(FNumericOnlyEditor.Text), 0), 100);
      EditedItem.Complete := EditedItem.Completion = 100;
    end;
  end
  else if ActiveEditor = FFloatEditor then
  begin
    EditedItem.TotalTime := FFloatEditor.FloatValue;
  //FFloatEditor.Value:=0;
  end;

  if ActiveEditor.Parent = EditorParent then
    ActiveEditor.Parent.Hide
  else
    ActiveEditor.Hide;
    
  ActiveEditor := nil;


  FOwner.EditDone(TodoData, EditedItem);

  if Assigned(FOwner.Parent) then
  begin
//    if FOwner.Parent.Visible then
//      SetFocus;
  end;
end;

constructor TInplaceListBox.Create(AOwner: TComponent);
begin
  inherited;
  FMouseDown := False;
  FTodoList := TTodoListBox(AOwner);
end;

function TInplaceListBox.GetItemIndexEx: Integer;
begin
  Result := inherited ItemIndex;
end;

procedure TInplaceListBox.SetItemIndexEx(const Value: Integer);
begin
  FOldItemIndex := Value;
  inherited ItemIndex := Value;
end;

procedure TInplaceListBox.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then
  begin
    if Assigned(FOnSelected) then
      FOnSelected(Self);
    FTodoList.AdvanceEdit;
  end;

  if Key = #27 then
  begin
    ItemIndex := FOldItemIndex;
    if Assigned(FOnSelected) then
      FOnSelected(Self);
  end;
end;

procedure TInplaceListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := True;
end;

procedure TInplaceListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FMouseDown then
    if Assigned(FOnSelected) then
      FOnSelected(Self);

  FMouseDown := False;
end;

procedure TInplaceListBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  if Msg.CharCode = VK_TAB then
  begin
    PostMessage(Handle, WM_CHAR, ord(#13), 0);
  end;
  inherited;
end;

{ TInplaceODListBox }

constructor TInplaceODListBox.Create(AOwner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawFixed;
  FTodoList := TTodoListBox(AOwner);
end;

procedure TInplaceODListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  Rect.Left := Rect.Left + 16;
  inherited DrawItem(Index, Rect, State);

  if Assigned(ImageList) then
  begin
    Rect.Left := Rect.Left - 16;

    if odSelected in State then
      Canvas.Brush.Color := clHighLight
    else
      Canvas.Brush.Color := clWindow;

    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Left + 16, Rect.Bottom);

    ImageList.DrawingStyle := dsTransparent;
    ImageList.Draw(Canvas, Rect.Left, Rect.Top, 4 - Index, True);
  end;
end;

{ TInplaceEdit }

constructor TInplaceEdit.Create(AOwner: TComponent);
begin
  inherited;
  FTodoList := TTodoListBox(AOwner);
  FNumericOnly := false;
end;

procedure TInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or ES_MULTILINE;
end;

function TInplaceEdit.GetText: string;
begin
  Result := inherited Text;
end;

procedure TInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F4 then
    DoExit;
end;

procedure TInplaceEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #27 then
  begin
    Text := FOldText;
    DoExit;
  end;

  if FNumericOnly then
  begin
    if not ((IsNumChar(Key)) or (Key = chr(VK_BACK)) or (key = #13)) then
      Key := #0;
  //Exit;
  end;

  if Key = #13 then
  begin
    DoExit;
    FTodoList.AdvanceEdit;
  end;
end;

procedure TInplaceEdit.SetText(const Value: string);
begin
  inherited Text := Value;
  FOldText := Value;
end;

procedure ComboBoxDropDown(ComboBoxHandle: THandle; Down: boolean = true);
begin
  if Down then
    SendMessage(ComboBoxHandle, CB_SHOWDROPDOWN, 1, 0)
  else
    SendMessage(ComboBoxHandle, CB_SHOWDROPDOWN, 0, 0);
end;

procedure TTodoListBox.WMVScroll(var Message: TWMScroll);
begin
  if ActiveEditor <> nil then
    EditorOnExit(Self);
  inherited;
end;

procedure SetFontButKeepFontColor(Destination, Source: TFont);
var SaveColor: TColor;
begin
  SaveColor := Destination.Color;
  Destination.Assign(Source);
  Destination.Color := SaveColor;
end;

procedure TTodoListBox.SetEditorFont(Editor: TWinControl; Font: TFont);
begin
  if Editor = StatusEditor then
    SetFontButKeepFontColor(StatusEditor.Font, Font)
  else if Editor = PriorityEditor then
    SetFontButKeepFontColor(PriorityEditor.Font, Font)
{$IFDEF USE_PLANNERDATEPICKER}
  else if Editor = PlannerDateEditor then
    SetFontButKeepFontColor(PlannerDateEditor.Font, Font)
{$ENDIF}
  else if Editor = DefaultDateEditor then
    SetFontButKeepFontColor(DefaultDateEditor.Font, Font)
  else if Editor = IntegerEditor then
    SetFontButKeepFontColor(IntegerEditor.Font, Font)
  else if Editor = StringListEditor then
    SetFontButKeepFontColor(StringListEditor.Font, Font)
  else if Editor = StringEditor then
    SetFontButKeepFontColor(StringEditor.Font, Font);
end;


procedure SetControlRect(Control: TControl; Rect: TRect);
begin
  Control.Top := Rect.Top;
  Control.Left := Rect.Left;
  Control.Width := Rect.Right - Rect.Left;
  Control.Height := Rect.Bottom - Rect.Top;
end;

procedure TTodoListBox.ShowEditor(Editor: TWinControl; R: TRect; UseSeparateParent: Boolean);
begin
  ActiveEditor := Editor;

  if UseSeparateParent then
  begin
  // Empty the content of the form
    while EditorParent.ControlCount <> 0 do
      EditorParent.Controls[EditorParent.ControlCount - 1].Parent := nil;

  // Set size
    ActiveEditor.Parent := EditorParent;
    ActiveEditor.Left := 0; ActiveEditor.Top := 0;
    ActiveEditor.Width := R.Right - R.Left;
    ActiveEditor.Height := R.Bottom - R.Top;

  // Put the editor parent in the correct position

    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);

    {$IFDEF DELPHI9_LVL}
    SetControlRect(EditorParent, Rect(0,0,0,0));
    {$ELSE}
    SetControlRect(EditorParent, R);
    {$ENDIF}
  end
  else
  begin
    ActiveEditor.Parent := Self;
    SetControlRect(ActiveEditor, R);
  end;

  SetEditorFont(ActiveEditor, EditedColumn.Font);
  ActiveEditor.Visible := True;
  if UseSeparateParent then
    EditorParent.Visible := True;

  {$IFDEF DELPHI9_LVL}
  SetControlRect(EditorParent, R);
  {$ENDIF}

  ActiveEditor.SetFocus;
end;

procedure PopulateListBoxEditor(ListBoxEditor: TListBox; CommaText: string);
var
  R: TRect;
begin
{ In the constructor, I create TListBox editors. For example, the
PriorityEditor is of type TListBox, and it's parent is the TodoListBox.
When I access the Items property of TListBox, TListBox checks to see if
the final parent of the TListBox is a TCustomForm. It finds it is not
(because the parent is not set yet), and raises an exception. That is
why I use this workaround. }
  if ListBoxEditor.Tag = 0 then
  begin
    ListBoxEditor.Items.CommaText := CommaText;
    R := ListBoxEditor.ItemRect(ListBoxEditor.Items.Count - 1);
    ListBoxEditor.Tag := R.Bottom;
  end;
  ListBoxEditor.ClientHeight := ListBoxEditor.Tag;
  ListBoxEditor.Parent.Height := ListBoxEditor.Height;
end;

procedure TTodoListBox.SendClickMessage(Msg: TMessage; X, Y: Integer);
var
  P: TPoint;
  MMsg: TWMLButtonDown absolute Msg;
begin
// Calculate screen coordinates
//  P.X := Message.XPos;
//  P.Y := Message.YPos;

  P := Point(X, Y);

  P := ClientToScreen(P);
  P := ActiveEditor.ScreenToClient(P);

  MMsg.XPos := P.X;
  MMsg.YPos := P.Y;
//  ActiveEditor.Perform(WM_LBUTTONDOWN, RawMessage.WParam, Msg.LParam);
//  ActiveEditor.Perform(WM_LBUTTONUP, RawMessage.WParam, Msg.LParam);
  ActiveEditor.Perform(WM_LBUTTONDOWN, Msg.WParam, Msg.LParam);
  ActiveEditor.Perform(WM_LBUTTONUP, Msg.WParam, Msg.LParam);

end;

procedure TTodoListBox.EditNotesInPreviewArea(Idx: Integer; R: TRect; Msg: TMessage; X, Y: Integer);
begin
  EditedItem := TodoItems.Items[Idx];

  if ActiveEditor <> nil then
    EditorOnExit(ActiveEditor);

// ShowEditor function does not apply here, because of the possibility
// that the "Notes" column might be missing
  ActiveEditor := StringListEditor;

  StringListEditor.Parent := FOwner;
  StringListEditor.BorderStyle := bsNone;

  with StringListEditor do
  begin
    SetFontButKeepFontColor(Font, TCustomTodoList(Self.Parent).PreviewFont);
    Lines.Assign(EditedItem.Notes);
    OrigLines.Assign(EditedItem.Notes);

  { Here I could substract the top border height and the left border
  width, in order to put the string list editor in the correct position.
  However, if the TodoList has left=0, this makes all components on the
  form shift to the right. So I have to adjust preview drawing in order to
  achieve the same effect. }
    Top := Self.Top + R.Top + Self.FOwner.ItemHeight + 1;
    Left := Self.Left + R.Left;

    Height := TCustomTodoList(Self.Parent).PreviewHeight;
    Width := R.Right - R.Left;

    Visible := True;
    
    SendClickMessage(Msg, X, Y);

    if FEditSelectAll then
      SelectAll;
  end;
end;

procedure TTodoListBox.AdvanceEdit;
begin
  if (FFocusColumn < FOwner.Columns.Count) and FOwner.AutoAdvanceEdit then
  begin
    FFocusColumn := FFocusColumn + 1;
    FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
    PostMessage(Self.Handle, WM_KEYDOWN, VK_F2, 0);
  end;
end;


procedure TTodoListBox.StartEdit(Index, Column: Integer; FromMouse: Boolean; Msg: TMessage; X, Y: Integer; ch: Char);
var
  R: TRect;
  P: TPoint;
  NewCompletion: Integer;
  TodoData: TTodoData;
  OldIdx: Integer;
  maxLength: Integer;
begin

  if not FOwner.Editable then
    Exit;

  if (Index < 0) or (Index >= Items.Count) then
    Exit;

  if Index <> ItemIndex then
  begin
    ItemIndex := Index;
    FOwner.ListSelect(Self);
  end;

  if (Column >= 0) and (Column < TodoColumns.Count) then
  begin
    if not TodoColumns.Items[Column].Editable then
      Exit;
  end
  else
    if not FOwner.Preview then
      Exit;

  FOwner.EditStart;

  ColItemRect(Column, Index, R);

  R.Bottom := R.Top + FOwner.ItemHeight;

  InflateRect(R, -1, -1);

  P := Point(X, Y);

// If there is an active editor,
  if ActiveEditor <> nil then
    EditorOnExit(ActiveEditor);

// Assign current column and item fields
  EditedColumn := nil;
  if (Column >= 0) and (Column < TodoColumns.Count) then
  begin
    EditedColumn := TodoColumns.Items[Column];
    TodoData := EditedColumn.TodoData;
    maxLength := EditedColumn.MaxLength;
  end
  else
  begin
    TodoData := tdNotes;
    maxLength := 0;
  end;

  EditedItem := TodoItems.Items[Index];
  StringEditor.MaxLength := maxLength;
  StringListEditor.MaxLength := maxLength;

  case TodoData of
    tdSubject:
      begin
        if ch <> #0 then
          StringEditor.Text := ch
        else
          StringEditor.Text := EditedItem.Subject;

        StringEditor.Font.Assign(TodoColumns.Items[Column].Font);
        ShowEditor(StringEditor, R, False);

        if FEditSelectAll and (ch = #0) then
          StringEditor.SelectAll // Select all the text in editor. This call may not be needed.
        else
          if FromMouse then
            SendClickMessage(Msg, X, Y)
          else
            StringEditor.SelStart := Length(StringEditor.Text);
      end;
    tdResource:
      begin
        if ch <> #0 then
          StringEditor.Text := ch
        else
          StringEditor.Text := EditedItem.Resource;

        StringEditor.Font.Assign(TodoColumns.Items[Column].Font);

        ShowEditor(StringEditor, R, False);
        if FEditSelectAll and (ch = #0) then
          StringEditor.SelectAll // Select all the text in editor. This call may not be needed.
        else
          if FromMouse then
            SendClickMessage(Msg, X, Y)
          else
            StringEditor.SelStart := Length(StringEditor.Text);

      end;

    tdProject:
      begin
        if ch <> #0 then
          StringEditor.Text := ch
        else
          StringEditor.Text := EditedItem.Project;

        StringEditor.Font.Assign(TodoColumns.Items[Column].Font);

        ShowEditor(StringEditor, R, False);
        if FEditSelectAll and (ch = #0) then
          StringEditor.SelectAll // Select all the text in editor. This call may not be needed.
        else
          if FromMouse then
            SendClickMessage(Msg, X, Y)
          else
            StringEditor.SelStart := Length(StringEditor.Text);
      end;

    tdNotes:
      if FOwner.Preview then
      begin
        StringListEditor.Font.Assign(PreviewFont);
        OldIdx := Index;
        ClickedOnNotes(False, P, Index, R); // Calculate correct rectangle
        EditNotesInPreviewArea(OldIdx, R, Msg, X, Y);
      end
      else
      begin
        ShowEditor(StringListEditor, R, True);
        StringListEditor.Font.Assign(TodoColumns.Items[Column].Font);
        StringListEditor.BorderStyle := bsSingle;
        StringListEditor.Lines.Assign(EditedItem.Notes);
        StringListEditor.OrigLines.Assign(EditedItem.Notes);

        if (StringListEditor.Lines.Text = '') and (ch <> #0) then
        begin
          StringListEditor.Lines.Text := ch;
          StringListEditor.SelStart := 1;
        end;

        StringListEditor.Height := StringListEditor.Width;
        EditorParent.Height := StringListEditor.Height;
      end;

    tdTotalTime:
      begin
        FFloatEditor.FloatValue := EditedItem.TotalTime;
        ShowEditor(FFloatEditor, R, False);
        FFloatEditor.Font.Assign(TodoColumns.Items[Column].Font);
      end;

    tdDueDate:
      begin
{$IFDEF USE_PLANNERDATEPICKER}
        if TCustomTodoList(Parent).CalendarType = tcPlannerCalendar then
        begin

            PlannerDateEditor.Calendar.Date := EditedItem.DueDate;

          if (EditedItem.DueDate = 0) then
            PlannerDateEditor.Text := ''
          else
            PlannerDateEditor.Text := DateToStr(EditedItem.DueDate);
          PlannerDateEditor.BorderStyle := bsNone;
          ShowEditor(PlannerDateEditor, R, False);
          PlannerDateEditor.DropDown;
        end else
        begin
{$ENDIF}
          if EditedItem.DueDate <> 0 then
            DefaultDateEditor.Date := EditedItem.DueDate
          else
            DefaultDateEditor.Date := Now;
            ShowEditor(DefaultDateEditor, R, False);
            DefaultDateEditor.Font.Assign(TodoColumns.Items[Column].Font);

          if FromMouse then
            SendClickMessage(Msg, X, Y);
//        ResendThisClickMessageToActiveEditor;
{$IFDEF USE_PLANNERDATEPICKER}
        end;
{$ENDIF}
      end;

    tdCompletionDate:
      begin
{$IFDEF USE_PLANNERDATEPICKER}
        if TCustomTodoList(Parent).CalendarType = tcPlannerCalendar then
        begin
          PlannerDateEditor.Calendar.Date :=  Now;

          if EditedItem.CompletionDate = 0 then
            PlannerDateEditor.Text := ''
          else
            PlannerDateEditor.Text := DateToStr(EditedItem.CompletionDate);
          PlannerDateEditor.BorderStyle := bsNone;
          ShowEditor(PlannerDateEditor, R, False);
          PlannerDateEditor.DropDown;
        end else
        begin
{$ENDIF}
          if (DefaultDateEditor.Date <> 0) and (EditedItem.CompletionDate <> 0) then
            DefaultDateEditor.Date := EditedItem.CompletionDate
          else
            DefaultDateEditor.Date := Now;
          ShowEditor(DefaultDateEditor, R, False);
          DefaultDateEditor.Font.Assign(TodoColumns.Items[Column].Font);
{$IFDEF USE_PLANNERDATEPICKER}
        end;
{$ENDIF}
      end;

    tdCreationDate:
      begin
{$IFDEF USE_PLANNERDATEPICKER}
        if TCustomTodoList(Parent).CalendarType = tcPlannerCalendar then
        begin
            PlannerDateEditor.Calendar.Date := EditedItem.CreationDate;

          if EditedItem.CreationDate = 0 then
            PlannerDateEditor.Text := ''
          else
            PlannerDateEditor.Text := DateToStr(EditedItem.CreationDate);
          PlannerDateEditor.BorderStyle := bsNone;
          ShowEditor(PlannerDateEditor, R, False);
          PlannerDateEditor.DropDown;
        end else
        begin
{$ENDIF}
          if EditedItem.CreationDate <> 0 then
            DefaultDateEditor.Date := EditedItem.CreationDate
          else
            DefaultDateEditor.Date := Now;
          ShowEditor(DefaultDateEditor, R, False);
          DefaultDateEditor.Font.Assign(TodoColumns.Items[Column].Font);
          ComboBoxDropDown(DefaultDateEditor.Handle);
{$IFDEF USE_PLANNERDATEPICKER}
        end;
{$ENDIF}
      end;

    tdPriority:
      begin
        if (TCustomTodoList(Parent).FPriorityListWidth > 0) then
          R.Right := R.Left + TCustomTodoList(Parent).FPriorityListWidth;

        ShowEditor(PriorityEditor, R, True);
        PopulateListBoxEditor(PriorityEditor, FOwner.PriorityCommaText); // must be always called after ShowEditor
        PriorityEditor.ItemIndex := PriorityEditor.Items.IndexOf(FOwner.PriorityToString(EditedItem.Priority));
        PriorityEditor.Ctl3D := False;
        PriorityEditor.Font.Assign(TodoColumns.Items[Column].Font);
        PriorityEditor.ImageList := FPriorityImageList;
      end;

    tdStatus:
      begin
        if (TCustomTodoList(Parent).FStatusListWidth > 0) then
          R.Right := R.Left + TCustomTodoList(Parent).FStatusListWidth;

        ShowEditor(StatusEditor, R, True);
        StatusEditor.Tag := 0;
        PopulateListBoxEditor(StatusEditor, FOwner.StatusCommaText);
        StatusEditor.ItemIndex := StatusEditor.Items.IndexOf(FOwner.StatusToString(EditedItem.Status));
        StatusEditor.Font.Assign(TodoColumns.Items[Column].Font);
        StatusEditor.Ctl3D := False;
      end;

    tdCategory:
      begin
        if (TCustomTodoList(Parent).FStatusListWidth > 0) then
          R.Right := R.Left + TCustomTodoList(Parent).FStatusListWidth;
        ShowEditor(StatusEditor, R, True);
        StatusEditor.Tag := 0;
        PopulateListBoxEditor(StatusEditor, FOwner.Category.CommaText);
        StatusEditor.ItemIndex := StatusEditor.Items.IndexOf(EditedItem.Category);
        StatusEditor.Font.Assign(TodoColumns.Items[Column].Font);
        StatusEditor.Ctl3D := False;
      end;

// Items which don't have a specialized editor
    tdComplete:
      begin
        TodoItems.Items[Index].Complete :=
          not TodoItems.Items[Index].Complete;

        FOwner.CompleteClick(Index);

        FOwner.EditDone(TodoData, TodoItems.Items[Index]);
      end;

    tdCompletion:
      if FOwner.CompletionGraphic then
      begin
        if FromMouse then
        begin
          if (X < R.Right - 1) then
          begin
            NewCompletion := 100 * (X - R.Left) div (R.Right - R.Left - 2);
            TodoItems.Items[Index].Complete := NewCompletion = 100;
            TodoItems.Items[Index].Completion := NewCompletion;

            if NewCompletion = 100 then
              FOwner.CompleteClick(Index);

            FOwner.EditDone(TodoData, TodoItems.Items[Index]);
          end;
        end
        else
        begin
          FNumericOnlyEditor.Text := inttostr(TodoItems.Items[Index].Completion);
          FNumericOnlyEditor.Font.Assign(TodoColumns.Items[Column].Font);
          R.Top := R.Top + 1;
          R.Bottom := R.Bottom - 1;
          ShowEditor(FNumericOnlyEditor, R, False);
          if FEditSelectAll and (ch = #0) then
            FNumericOnlyEditor.SelectAll // Select all the text in editor. This call may not be needed.
          else
            if FromMouse then
              SendClickMessage(Msg, X, Y)
            else
              FNumericOnlyEditor.SelStart := Length(FNumericOnlyEditor.Text);
          R.Top := R.Top - 2;
          R.Bottom := R.Bottom + 2;
        end;
      end
      else
      begin
        IntegerEditor.Value := EditedItem.Completion;
        ShowEditor(IntegerEditor, R, False);
        IntegerEditor.Font.Assign(TodoColumns.Items[Column].Font);
      end;
  end;
end;

procedure TTodoListBox.RepaintItem(Index: Integer);
var
  r: TRect;
begin
  SendMessage(Handle, LB_GETITEMRECT, Index, LParam(@r));
  InvalidateRect(Handle, @r, False);
end;

procedure TTodoListBox.SetHorizontalScrollBar;
begin
  UpdateHScrollExtent(0);
end;

procedure TTodoListBox.DrawGrid(ACanvas: TCanvas);
var
  lb: TLogBrush;
  x,y, Col, w: Integer;
  HPen, HOldPen: THandle;
begin
  if (FGridLines = glsAlways) then
  begin
    if FScrollHorizontal then
      w := SendMessage(Handle, LB_GETHORIZONTALEXTENT,0,0)
    else
      w := Width;

    lb.lbColor := ColorToRGB(FGridLineColor);
    lb.lbStyle := bs_Solid;

    HPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE,1,lb,0,nil);
    HOldPen := SelectObject(ACanvas.Handle, HPen);

    y := ItemHeight;
    while (y < Height) do
    begin
      Windows.MoveToEx(ACanvas.Handle, 0, y - 1,nil);
      Windows.LineTo(ACanvas.Handle, w, y - 1);
      y := y + ItemHeight;
    end;

    if not Preview then
    begin
      if FScrollHorizontal then
        x := - GetScrollPos(Handle, SB_HORZ)
      else
        x := 0;
      for Col := 1 to FTodoColumns.Count - 1 do
      begin
        x := x + FTodoColumns.Items[Col - 1].Width;
        Windows.MoveToEx(ACanvas.Handle, x, 0,nil);
        Windows.LineTo(ACanvas.Handle, x, Height);
      end;
    end;

    DeleteObject(SelectObject(ACanvas.Handle,HOldPen));
  end;
end;


{
procedure TTodoListBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  DrawGrid;
end;

procedure TTodoListBox.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  message.Result := 0;
end;
}

procedure TTodoListBox.WMLButtonUp(var Message: TWMLButtonDown);
var
  R: TRect;
  P: TPoint;
  EditedColIdx, EditedItemIdx: Integer;
  RawMessage: TMessage absolute Message;
begin
  if FMouseDownNotDone then
  begin
    if FOwner.DragMode = dmAutomatic then
      EndDrag(False);
    inherited;
    Exit;
  end;

  if not FEditable then
  begin
    inherited;
    Exit;
  end;

  P.X := Message.XPos;
  P.Y := Message.YPos;

  if XYToColItem(P.X, P.Y, EditedColIdx, EditedItemIdx, R) and
    (EditedItemIdx < TodoItems.Count) then
  begin
    if FFocusColumn <> EditedColIdx then
    begin
      FFocusColumn := EditedColIdx;
      FOwner.FTodoHeader.ActiveColumn := FFocusColumn;
      RepaintItem(EditedItemIdx);
    end;

    if not TodoColumns.Items[EditedColIdx].Editable then
    begin
      inherited;
      Exit;
    end;
    if FOwner.DragMode = dmAutomatic then
      EndDrag(False);

    inherited;

    if Message.Keys <> 8 then
      StartEdit(EditedItemIdx, EditedColIdx, True, RawMessage, Message.XPos, Message.YPos, #0);
  end
  else
    inherited;

  if FOwner.Preview and ClickedOnNotes(True, P, EditedItemIdx, R) then
  begin
    if FOwner.DragMode = dmAutomatic then
      EndDrag(False);
    EditNotesInPreviewArea(EditedItemIdx, R, RawMessage, Message.XPos, Message.YPos);
  end;
end;

{ Even if the result is false, it calculates the preview rectangle of the clicked item. }
function TTodoListBox.ClickedOnNotes(const CalcItem: Boolean; const P: TPoint; out ItemIdx: Integer; out R: TRect): Boolean;
var
  TopBorderWidth, LeftBorderWidth, RightBorderWidth, BottomBorderWidth: integer;
begin
  if CalcItem then
    ItemIdx := ItemAtPos(P, True);

  if ItemIdx = -1 then
    Result := False
  else
  begin
    SendMessage(Handle, LB_GETITEMRECT, ItemIdx, LParam(@R));
    Result := (P.Y > R.Top + FOwner.ItemHeight) and (P.Y < R.Bottom);

  // Take into account listbox border width
    WinControlBorderWidths(self, LeftBorderWidth, RightBorderWidth, TopBorderWidth, BottomBorderWidth);
    R.Top := R.Top + TopBorderWidth;
    R.Bottom := R.Bottom + TopBorderWidth;
    R.Left := R.Left + LeftBorderWidth;
    R.Right := R.Right + LeftBorderWidth;
  end
end;


procedure TTodoListBox.SetPreviewColor(const Value: TColor);
begin
  FPreviewColor := Value;
  Invalidate;
end;

procedure TTodoListBox.SetPreviewColorTo(const Value: TColor);
begin
  FPreviewColorTo := Value;
  Invalidate;
end;


{ TStatusStrings }

constructor TStatusStrings.Create(AOwner: TCustomTodoList);
begin
  inherited Create;

  FOwner := AOwner;
  FStatusStrings[tsDeferred] := 'Deferred';
  FStatusStrings[tsNotStarted] := 'Not started';
  FStatusStrings[tsCompleted] := 'Completed';
  FStatusStrings[tsInProgress] := 'In progress';
end;

function TStatusStrings.GetString(Index: TTodoStatus): string;
begin
  Result := FStatusStrings[Index];
end;

function TStatusStrings.GetStringC: string;
begin
  Result := FStatusStrings[tsCompleted];
end;

function TStatusStrings.GetStringD: string;
begin
  Result := FStatusStrings[tsDeferred];
end;

function TStatusStrings.GetStringI: string;
begin
  Result := FStatusStrings[tsInProgress];
end;

function TStatusStrings.GetStringN: string;
begin
  Result := FStatusStrings[tsNotStarted];
end;

procedure TStatusStrings.SetString(Index: TTodoStatus;
  const Value: string);
begin
  FStatusStrings[Index] := Value;
  FOwner.Invalidate;
end;

procedure TStatusStrings.SetStringC(const Value: string);
begin
  FStatusStrings[tsCompleted] := Value;
  FOwner.Invalidate;
end;

procedure TStatusStrings.SetStringD(const Value: string);
begin
  FStatusStrings[tsDeferred] := Value;
  FOwner.Invalidate;
end;

procedure TStatusStrings.SetStringI(const Value: string);
begin
  FStatusStrings[tsInProgress] := Value;
  FOwner.Invalidate;
end;

procedure TStatusStrings.SetStringN(const Value: string);
begin
  FStatusStrings[tsNotStarted] := Value;
  FOwner.Invalidate;
end;

{ TPriorityStrings }

constructor TPriorityStrings.Create(AOwner: TCustomTodoList);
begin
  inherited Create;

  FOwner := AOwner;
  FPriorityStrings[tpLowest] := 'Lowest';
  FPriorityStrings[tpLow] := 'Low';
  FPriorityStrings[tpNormal] := 'Normal';
  FPriorityStrings[tpHigh] := 'High';
  FPriorityStrings[tpHighest] := 'Highest';
end;

function TPriorityStrings.GetString(Index: TTodoPriority): string;
begin
  Result := FPriorityStrings[Index];
end;

function TPriorityStrings.GetStringH: string;
begin
  Result := FPriorityStrings[tpHigh];
end;

function TPriorityStrings.GetStringHS: string;
begin
  Result := FPriorityStrings[tpHighest];
end;

function TPriorityStrings.GetStringL: string;
begin
  Result := FPriorityStrings[tpLow];
end;

function TPriorityStrings.GetStringLS: string;
begin
  Result := FPriorityStrings[tpLowest];
end;

function TPriorityStrings.GetStringN: string;
begin
  Result := FPriorityStrings[tpNormal];
end;

procedure TPriorityStrings.SetString(Index: TTodoPriority;
  const Value: string);
begin
  FPriorityStrings[Index] := Value;
  FOwner.Invalidate;
end;

procedure TPriorityStrings.SetStringH(const Value: string);
begin
  FPriorityStrings[tpHigh] := Value;
  FOwner.Invalidate;
end;

procedure TPriorityStrings.SetStringHS(const Value: string);
begin
  FPriorityStrings[tpHighest] := Value;
  FOwner.Invalidate;
end;

procedure TPriorityStrings.SetStringL(const Value: string);
begin
  FPriorityStrings[tpLow] := Value;
  FOwner.Invalidate;
end;

procedure TPriorityStrings.SetStringLS(const Value: string);
begin
  FPriorityStrings[tpLowest] := Value;
  FOwner.Invalidate;
end;

procedure TPriorityStrings.SetStringN(const Value: string);
begin
  FPriorityStrings[tpNormal] := Value;
  FOwner.Invalidate;
end;

{ TEditColors }

constructor TEditColors.Create(AOwner: TCustomTodoList);
begin
  inherited Create;
  FOwner := AOwner;
  FStringEditor := TBackForeColors.Create(Self, FOwner.FTodoListBox.StringEditor);
  FMemoEditor := TBackForeColors.Create(Self, FOwner.FTodoListBox.StringListEditor);
  FIntegerEditor := TBackForeColors.Create(Self, FOwner.FTodoListBox.IntegerEditor);
  FPriorityEditor := TBackForeColors.Create(Self, FOwner.FTodoListBox.PriorityEditor);
  FStatusEditor := TBackForeColors.Create(Self, FOwner.FTodoListBox.StatusEditor);
{$IFDEF USE_PLANNERDATEPICKER}
  FPlannerDateEditor := TPlannerDatePickerColors.Create(Self, TPlannerDatePicker(FOwner.FTodoListBox.PlannerDateEditor));
{$ENDIF}
  FDefaultDateEditor := TDatePickerColors.Create(Self, TTodoDateTimePicker(FOwner.FTodoListBox.DefaultDateEditor));
end;

destructor TEditColors.Destroy;
begin
  FStringEditor.Free;
  FMemoEditor.Free;
  FIntegerEditor.Free;
  FPriorityEditor.Free;
  FStatusEditor.Free;
{$IFDEF USE_PLANNERDATEPICKER}
  FPlannerDateEditor.Free;
{$ENDIF}
  FDefaultDateEditor.Free;
  inherited;
end;

{ TBackForeColors }

constructor TBackForeColors.Create(AOwner: TEditColors;
  AColorControl: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
  FColorControl := AColorControl;
end;

function TBackForeColors.GetBackColor: TColor;
begin
  if FColorControl is TInPlaceEdit
    then Result := TInPlaceEdit(FColorControl).Color
  else if FColorControl is TInplaceMemo
    then Result := TInplaceMemo(FColorControl).Color
  else if FColorControl is TSpinEdit
    then Result := TSpinEdit(FColorControl).Color
  else if FColorControl is TInPlaceODListBox
    then Result := TInPlaceODListBox(FColorControl).Color
  else if FColorControl is TInPlaceListBox
    then Result := TInPlaceListBox(FColorControl).Color
  else raise Exception.Create('TEditColors.GetBackColor: unknown control class.');
end;

function TBackForeColors.GetFontColor: TColor;
begin
  if FColorControl is TInPlaceEdit
    then Result := TInPlaceEdit(FColorControl).Font.Color
  else if FColorControl is TInplaceMemo
    then Result := TInplaceMemo(FColorControl).Font.Color
  else if FColorControl is TSpinEdit
    then Result := TSpinEdit(FColorControl).Font.Color
  else if FColorControl is TInPlaceODListBox
    then Result := TInPlaceODListBox(FColorControl).Font.Color
  else if FColorControl is TInPlaceListBox
    then Result := TInPlaceListBox(FColorControl).Font.Color
  else raise Exception.Create('TEditColors.GetFontColor: unknown control class.');
end;

procedure TBackForeColors.SetBackColor(const Value: TColor);
begin
  if FColorControl is TInPlaceEdit
    then TInPlaceEdit(FColorControl).Color := Value
  else if FColorControl is TInplaceMemo
    then TInplaceMemo(FColorControl).Color := Value
  else if FColorControl is TSpinEdit
    then TSpinEdit(FColorControl).Color := Value
  else if FColorControl is TInPlaceODListBox
    then TInPlaceODListBox(FColorControl).Color := Value
  else if FColorControl is TInPlaceListBox
    then TInPlaceListBox(FColorControl).Color := Value
  else raise Exception.Create('TEditColors.SetBackColor: unknown control class.');
end;

procedure TBackForeColors.SetFontColor(const Value: TColor);
begin
  if FColorControl is TInPlaceEdit
    then TInPlaceEdit(FColorControl).Font.Color := Value
  else if FColorControl is TInplaceMemo
    then TInplaceMemo(FColorControl).Font.Color := Value
  else if FColorControl is TSpinEdit
    then TSpinEdit(FColorControl).Font.Color := Value
  else if FColorControl is TInPlaceODListBox
    then TInPlaceODListBox(FColorControl).Font.Color := Value
  else if FColorControl is TInPlaceListBox
    then TInPlaceListBox(FColorControl).Font.Color := Value
  else raise Exception.Create('TEditColors.SetFontColor: unknown control class.');
end;

{ TDatePickerColors }

constructor TDatePickerColors.Create(AOwner: TEditColors; AColorControl: TTodoDateTimePicker);
begin
  inherited Create;
  FOwner := AOwner;
  FColorControl := AColorControl;
end;

function TDatePickerColors.GetBackColor: TColor;
begin
  Result := FColorControl.Color;
end;

function TDatePickerColors.GetCalColors: TMonthCalColors;
begin
  Result := FColorControl.CalColors;
end;

function TDatePickerColors.GetFontColor: TColor;
begin
  Result := FColorControl.Font.Color;
end;

procedure TDatePickerColors.SetBackColor(const Value: TColor);
begin
  FColorControl.Color := Value;
end;

procedure TDatePickerColors.SetCalColors(const Value: TMonthCalColors);
begin
  FColorControl.CalColors.Assign(Value);
end;

procedure TDatePickerColors.SetFontColor(const Value: TColor);
begin
  FColorControl.Font.Color := Value;
end;

{$IFDEF USE_PLANNERDATEPICKER}

{ TPlannerDatePickerColors }

constructor TPlannerDatePickerColors.Create(AOwner: TEditColors; AColorControl: TPlannerDatePicker);
begin
  inherited Create;
  FOwner := AOwner;
  FColorControl := AColorControl;
  FCalendarColors := TCalendarColors.Create(Self);
end;

function TPlannerDatePickerColors.GetBackColor: TColor;
begin
  Result := FColorControl.Color;
end;

function TPlannerDatePickerColors.GetFontColor: TColor;
begin
  Result := FColorControl.Font.Color;
end;

procedure TPlannerDatePickerColors.SetBackColor(const Value: TColor);
begin
  FColorControl.Color := Value;
end;

procedure TPlannerDatePickerColors.SetFontColor(const Value: TColor);
begin
  FColorControl.Font.Color := Value;
end;

destructor TPlannerDatePickerColors.Destroy;
begin
  FCalendarColors.Free;
  inherited;
end;

constructor TCalendarColors.Create(AOwner: TPlannerDatePickerColors);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TCalendarColors.GetColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.Color;
end;

function TCalendarColors.GetEventDayColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.EventDayColor;
end;

function TCalendarColors.GetEventMarkerColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.EventMarkerColor;
end;

function TCalendarColors.GetFocusColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.FocusColor;
end;

function TCalendarColors.GetGradientDirection: TGradientDirection;
begin
  Result := FOwner.FColorControl.Calendar.GradientDirection;
end;

function TCalendarColors.GetGradientEndColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.GradientEndColor;
end;

function TCalendarColors.GetGradientStartColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.GradientStartColor;
end;

function TCalendarColors.GetHeaderColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.HeaderColor;
end;

function TCalendarColors.GetInactiveColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.InactiveColor;
end;

function TCalendarColors.GetSelectColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.SelectColor;
end;

function TCalendarColors.GetSelectFontColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.SelectFontColor;
end;

function TCalendarColors.GetTextColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.TextColor;
end;

function TCalendarColors.GetWeekendColor: TColor;
begin
  Result := FOwner.FColorControl.Calendar.WeekendColor;
end;

procedure TCalendarColors.SetColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.Color := Value;
end;

procedure TCalendarColors.SetEventDayColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.EventDayColor := Value;
end;

procedure TCalendarColors.SetEventMarkerColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.EventMarkerColor := Value;
end;

procedure TCalendarColors.SetFocusColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.FocusColor := Value;
end;

procedure TCalendarColors.SetGradientDirection(
  const Value: TGradientDirection);
begin
  FOwner.FColorControl.Calendar.GradientDirection := Value;
end;

procedure TCalendarColors.SetGradientEndColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.GradientEndColor := Value;
end;

procedure TCalendarColors.SetGradientStartColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.GradientStartColor := Value;
end;

procedure TCalendarColors.SetHeaderColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.HeaderColor := Value;
end;

procedure TCalendarColors.SetInactiveColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.InactiveColor := Value;
end;

procedure TCalendarColors.SetSelectColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.SelectColor := Value;
end;

procedure TCalendarColors.SetSelectFontColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.SelectFontColor := Value;
end;

procedure TCalendarColors.SetTextColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.TextColor := Value;
end;

procedure TCalendarColors.SetWeekendColor(const Value: TColor);
begin
  FOwner.FColorControl.Calendar.WeekendColor := Value;
end;

{$ENDIF}



{ TCompleteCheck }

procedure TCompleteCheck.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TCompleteCheck.Create;
begin
  FCompletedGlyph := TBitmap.Create;
  FUnCompletedGlyph := TBitmap.Create;
end;

destructor TCompleteCheck.Destroy;
begin
  FCompletedGlyph.Free;
  FUnCompletedGlyph.Free;
  inherited;
end;

procedure TCompleteCheck.SetCheckType(const Value: TCheckType);
begin
  FCheckType := Value;
  Changed;
end;

procedure TCompleteCheck.SetCompletedGlyph(const Value: TBitmap);
begin
  FCompletedGlyph.Assign(Value);
  Changed;
end;

procedure TCompleteCheck.SetUnCompletedGlyph(const Value: TBitmap);
begin
  FUnCompletedGlyph.Assign(Value);
  Changed;
end;

{ TTodoDateTimePicker }

procedure TTodoDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    Parent.SetFocus;
  if Key = VK_TAB then
  begin
    PostMessage(Handle, WM_KEYDOWN, ord(#27), 0);
  end;
  inherited;
end;

{ TInplaceMemo }

procedure TInplaceMemo.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  if Msg.CharCode = VK_ESCAPE then
  begin
    Lines.Assign(FOldText);
    DoExit;
  end;
  if Msg.CharCode = VK_TAB then
  begin
    PostMessage(Handle, WM_CHAR, ord(#27), 0);
  end;

  inherited;
end;

constructor TInplaceMemo.Create(AOwner: TComponent);
begin
  inherited;
  FTodoList := TTodoListBox(AOwner);
  FOldText := TStringList.Create;
end;

destructor TInplaceMemo.Destroy;
begin
  FOldText.Free;
  inherited;
end;

procedure TInplaceMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_F4 then
    DoExit;
end;

procedure TInplaceMemo.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #27 then
  begin
    DoExit;
    FTodoList.AdvanceEdit;
  end;
end;


procedure TTodoListBox.CMHintShow(var M: TCMHintShow);
var
  ColIdx, ItemIdx: Integer;
  R: TRect;
  Text: string;
  Canvas: TCanvas;
begin
  if not FOwner.HintShowFullText then
  begin
    inherited;
    Exit;
  end;

// get the item underneath the mouse cursor
  if XYToColItem(M.HintInfo.CursorPos.X, M.HintInfo.CursorPos.Y, ColIdx, ItemIdx, R) and
    (ItemIdx < TodoItems.Count) then
  begin
  // fetch the text that should appear untrimmed in the column
    Text := '';
    with TodoItems.Items[ItemIdx] do
      case TodoColumns.Items[ColIdx].TodoData of
        tdSubject: Text := Subject;
        tdCompletion: if not FOwner.CompletionGraphic then Text := IntToStr(Completion);
        tdNotes: if not FOwner.Preview then Text := Notes.Text;
        tdPriority: Text := FOwner.PriorityToString(Priority);
        tdDueDate: Text := FormatDateTime(FDateFormat, DueDate);
        tdStatus: Text := FOwner.StatusStrings[Status];
        tdTotalTime: Text := FloatToStr(TotalTime) + FTotalTimeSuffix;
        tdCompletionDate: Text := FormatDateTime(FDateFormat, CompletionDate);
        tdCreationDate: Text := FormatDateTime(FDateFormat, CreationDate);
        tdResource: Text := Resource;
        tdProject: Text := Project;
        tdCategory: Text := Category;
      end;

  // see if the text fits the column, and if not, show the hint
    if Length(Trim(Text)) > 0 then
      with M.HintInfo^ do
      begin
        Canvas := TCanvas.Create;
        Canvas.Handle := GetDC(0);
        try
          Canvas.Font := TodoColumns.Items[ColIdx].Font;
          if Canvas.TextWidth(Text) > R.Right - R.Left then
          begin
            HintStr := Text;
            R.TopLeft := ClientToScreen(R.TopLeft);
            R.BottomRight := ClientToScreen(R.BottomRight);
            HintPos.Y := R.Top;
            HintPos.X := R.Left;
            CursorRect := R;
            HintMaxWidth := FOwner.ClientWidth;
          end;
        finally
          ReleaseDC(0, Canvas.Handle);
          Canvas.Free;
        end;
      end;
  end;
end;

procedure TTodoListBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (Msg.CharCode = VK_TAB) and (FUseTab) then
    Msg.Result := 1;
end;


{ TInplaceSpinEdit }


constructor TInplaceSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  FTodoList := TTodoListBox(AOwner);
end;

procedure TInplaceSpinEdit.WMChar(var Msg: TWMChar);
begin
  if Msg.CharCode = VK_RETURN then
    Msg.CharCode := 0;

  inherited;
end;

procedure TInplaceSpinEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  if Msg.CharCode = VK_RETURN then
  begin
    DoExit;
    FTodoList.AdvanceEdit;
    Msg.CharCode := 0;
  end;
  inherited;
end;

procedure TTodoListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  ColIdx, ItemIdx: Integer;
  R: TRect;
begin
  FMouseDownNotDone := true;

  XYToColItem(Message.XPos , Message.YPos, ColIdx, ItemIdx, R);

  if (ItemIdx >= 0) and (ColIdx >= 0) and (ColIdx < FOwner.Columns.Count) then
  begin
    if FOwner.Columns[ColIdx].TodoData = tdHandle then
      FMouseDownNotDone := false;
  end;

  inherited;
  FMouseDownNotDone := false;
//  DrawGrid;
end;

{ TInplaceFloatSpinEdit }

constructor TInplaceFloatSpinEdit.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
  FTodoList := TTodoListBox(AOwner);

  FUpDownBtns := TSpinSpeedButtons.Create(Self);
  FUpDownBtns.Visible := True;
  FUpDownBtns.Parent := Self;
  FUpDownBtns.OnUpBtnClick := UpClick;
  FUpDownBtns.OnDownBtnClick := DownClick;

  FIncrementFloat := 1.0;
  Text := '0';
  FDecSep := DecimalSeparator;
end;

destructor TInplaceFloatSpinEdit.destroy;
begin
  FUpDownBtns.Free;
  inherited;
end;

function TInplaceFloatSpinEdit.GetFloatValue: double;
begin
  try
    Result := StrToFloat(Text);
  except
    Result := 0;
  end;
end;

function TInplaceFloatSpinEdit.IsValidChar(var Key: Char): Boolean;
begin
//  Result := (Key = '.' {DecimalSeparator}) or (Key in ['+', '-', '0'..'9']) or ((Key < #32) and (Key <> Chr(VK_RETURN)));
   Result := (Key = FDecSep) or (IsNumChar(Key)) or (Key = '+') or (Key = '-') or ((Key < #32) and (Key <> Chr(VK_RETURN)));

(*
if (FSpinType = sptFloat) and not (key in [chr(vk_escape),chr(vk_return),chr(vk_back)]) then
begin
  if (key = ThousandSeparator) then
    key := DateSeparator;
*)
//  if (key = '.' {DecimalSeparator}) and ((Pos('.' {DecimalSeparator}, self.Text) > 0) { or (FPrecision = 0)}) then
//    Result := False;

  if (key = FDecSep) and ((Pos(FDecSep, self.Text) > 0) { or (FPrecision = 0)}) then
    Result := False;

(*
  dp := Pos(DecimalSeparator,self.Text);

  if (FPrecision > 0) and (dp > 0) and (selstart >= dp) and (sellength = 0) then
  begin
    if (Length(self.Text) >= dp + FPrecision) then
      Result := False;
  end;
end;
*)
end;

procedure TInplaceFloatSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TInplaceFloatSpinEdit.SetFloatValue(const Value: double);
begin
  if FPrecision < 0 then
    Text := FloatToStrF(Value, ffGeneral, 4, 4)
  else
    if FPrecision = 0 then
      Text := FloatToStr(Value)
    else
      Text := FloatToStrF(Value, ffFixed, 15, FPrecision);
end;

procedure TInplaceFloatSpinEdit.SetPrecision(const Value: integer);
begin
  FPrecision := Value;
  FloatValue := GetFloatValue;
end;

procedure TInplaceFloatSpinEdit.UpClick(Sender: TObject);
begin
  FloatValue := FloatValue + FIncrementFloat;
end;

procedure TInplaceFloatSpinEdit.DownClick(Sender: TObject);
begin
  FloatValue := FloatValue - FIncrementFloat;
end;

procedure TInplaceFloatSpinEdit.WMChar(var Msg: TWMChar);
begin
  if Msg.CharCode = VK_RETURN then
    Msg.CharCode := 0;

  inherited;
end;

procedure TInplaceFloatSpinEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  if Msg.CharCode = VK_RETURN then
  begin
    DoExit;
    FTodoList.AdvanceEdit;
    Msg.CharCode := 0;
  end;

  case Msg.CharCode of
    VK_UP:
      begin
        UpClick(self);
      end;
    VK_DOWN:
      begin
        DownClick(self);
      end;
  end;
  inherited;
end;

procedure TInplaceFloatSpinEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  if (FUpDownBtns <> nil) then
    FUpDownBtns.SetBounds(Width - FUpDownBtns.Width - 5, 0, FUpDownBtns.Width, Height - 2);
  SetEditRect;
end;

procedure TInplaceFloatSpinEdit.SetEditRect;
var
  Loc: TRect;
  Dist: integer;
begin
  if BorderStyle = bsNone then
    Dist := 2
  else
    Dist := 0;
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FUpDownBtns.Width - 2 - Dist;
  Loc.Top := Dist;
  Loc.Left := Dist;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));
end;

procedure TInplaceFloatSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TInplaceFloatSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;


{ TSpinSpeedButton }

constructor TSpinSpeedButtons.create(aOwner: TComponent);
begin
  inherited Create(AOwner);
//ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
//  [csFramed, csOpaque];
  FUpButton := {TSpeedButton} TTimerSpinButton.Create(self);
  FUpButton.OnClick := UpBtnClick;
  FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'SPINUPBTN');
  FUpButton.NumGlyphs := 1;
  FUpButton.Visible := True;
  FUpButton.Enabled := True;
  FUpButton.Parent := Self;
  FUpButton.Height := 12;
  FUpButton.width := 15;

  FDownButton := {TSpeedButton} TTimerSpinButton.Create(self);
  FDownButton.OnClick := DownBtnClick;
  FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'SPINDOWNBTN');
  FUpButton.NumGlyphs := 1;
  FDownButton.Visible := True;
  FDownButton.Enabled := True;
  FDownButton.Parent := Self;
  FDownButton.Height := 12;
  FDownButton.Width := 15;

  Width := 15;
  Height := 25;
end;

procedure TSpinSpeedButtons.AdjustSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds(0, 0, W, H div 2);
  FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height);
end;

procedure TSpinSpeedButtons.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

{ check for minimum size }
  W := Width;
  H := Height;
  AdjustSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TSpinSpeedButtons.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TSpinSpeedButtons.DownBtnClick(Sender: TObject);
begin
  if Assigned(FOnDownBtnClick) then
    FOnDownBtnClick(Self);
end;

procedure TSpinSpeedButtons.UpBtnClick(Sender: TObject);
begin
  if Assigned(FOnUpBtnClick) then
    FOnUpBtnClick(Self);
end;


{TTimerSpinButton}

destructor TTimerSpinButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TTimerSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
  end;

  InvalidateRect(parent.handle, nil, true);
end;

procedure TTimerSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;

  InvalidateRect(parent.handle, nil, true);
end;

procedure TTimerSpinButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TTimerSpinButton.Paint;
begin
  inherited Paint;
end;

constructor TTimerSpinButton.Create(AOwner: TComponent);
begin
  inherited;
  FHasMouse := False;
  TimeBtnState := [tbAllowTimer];
end;

procedure TTimerSpinButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHasMouse := True;
  Invalidate;
end;

procedure TTimerSpinButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHasMouse := False;
  Invalidate;
end;

{ TTodoItemIO }

constructor TTodoItemIO.CreateItem(AOwner: TTodoItemCollection);
begin
  inherited Create(nil);
  FItem := TTodoItem(AOwner.GetItemClass.Create(AOwner));
end;

destructor TTodoItemIO.Destroy;
begin
  FItem.Free;
  inherited;
end;




initialization

  CF_TODOITEM := RegisterClipboardFormat('TTodo Item');

end.
