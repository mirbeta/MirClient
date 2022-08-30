{**************************************************************************}
{ TAdvListBox component                                                    }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2013 - 2015                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvListBox;

{$I TMSDEFS.INC}

{$DEFINE HILIGHT}

interface

uses
  Classes, Controls, Windows, Messages, SysUtils, StdCtrls, AdvEdit,
  AdvMultiButtonEdit, ImgList, Dialogs, Forms, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed: Issue with use of checklist type on classic desktop
  // v1.0.0.2 : Fixed: ItemIndex now returns the real index in the Items[] collection when filtering is enabled
  // v1.0.0.3 : Fixed: Issue with Items.Clear()
  // v1.0.1.0 : New : Exposed public function GetItem to access items regardless of filtering
  // v1.0.2.0 : New : Added programmatic access to filtercondition
  // v1.1.0.0 : New : OnInsertItem event added
  //          : New : SearchOptions.Position added
  //          : New : InsertOptions.Position added
  //          : New : GetItemAtXY() added
  //          : New : SearchOptions.TextHint added
  //          : New : InsertOptions.TextHint added
  // v1.1.1.0 : New : SearchOptions.Flat added
  //          : New : InsertOptions.Flat added
  // v1.1.2.0 : New : Items.IndexOf() function added
  //          : Fixed : Issue with accessing ItemIndex in filtered list
  // v1.1.3.0 : New : OnItemHint event added
  // v1.1.3.1 : Improved : Doing filtering via entering search text automatically sets filter button in down state
  //          : Fixed : Issue with GetItemAtXY
  // v1.1.4.0 : New : Made SearchEdit / InsertEdit public properties
  // v1.1.4.1 : Fixed : Issue with SearchOptions.ShowFind setting
  //          : Fixed : Issue with ESC key handling in search edit
  // v1.1.5.0 : New : Method Items.Move() added
  //          : Fixed : Issue with default color setting
  // v1.1.5.1 : Fixed : Issue with associating TAdvListBox with Label.FocusControl

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TVisualListBox = class;

  TListBoxItem = class(TCollectionItem)
  private
    FTag: integer;
    FText: string;
    FChecked: boolean;
    FImageIndex: integer;
    FHasCheckBox: boolean;
    FObject: TObject;
    FFilterIndex: integer;
    FFiltered: boolean;
    procedure SetChecked(const Value: boolean);
    procedure SetHasCheckBox(const Value: boolean);
    procedure SetImageIndex(const Value: integer);
    procedure SetText(const Value: string);
  protected
    property FilterIndex: integer read FFilterIndex write FFilterIndex;
    property Filtered: boolean read FFiltered write FFiltered;
  public
    constructor Create(Collection: TCollection); override;
    property HasCheckBox: boolean read FHasCheckBox write SetHasCheckBox;
    procedure Assign(Source: TPersistent); override;
    property ItemObject: TObject read FObject write FObject;
  published
    property Checked: boolean read FChecked write SetChecked default false;
    property Text: string read FText write SetText;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Tag: integer read FTag write FTag default 0;
  end;

  TIntList = class;

  TListBoxItems = class(TCollection)
  private
    FOnChange: TNotifyEvent;
    function GetItems(Index: integer): TListBoxItem;
    procedure SetItems(Index: integer; const Value: TListBoxItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: integer]: TListBoxItem read GetItems write SetItems; default;
    function Add: TListBoxItem;
    function Insert(Index: integer): TListBoxItem;
    function IndexOf(Value: string): integer;
    procedure Move(FromIndex, ToIndex: integer);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvListBoxStyle = (stList, stCheckList);

  TFilterMethod = (fmStartsWith, fmEndsWith, fmContains, fmNotContains, fmEqual, fmNotEqual);

  TSearchStyle = (ssFilter, ssHighlight, ssGoto);

  TInputPosition = (ipTop, ipBottom);

  TSearchOptions = class(TPersistent)
  private
    FStyle: TSearchStyle;
    FOnChange: TNotifyEvent;
    FShowClose: boolean;
    FAutoFind: boolean;
    FShowFind: boolean;
    FVisible: boolean;
    FHintFind: string;
    FHintClose: string;
    FFilterMethod: TFilterMethod;
    FEditColor: TColor;
    FCaseSensitive: boolean;
    FPosition: TInputPosition;
    FOnAlignmentChange: TNotifyEvent;
    FTextHint: string;
    FFlat: boolean;
    procedure SetStyle(const Value: TSearchStyle);
    procedure SetVisible(const Value: boolean);
    procedure SetShowClose(const Value: boolean);
    procedure SetHintClose(const Value: string);
    procedure SetHintFind(const Value: string);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetEditColor(const Value: TColor);
    procedure SetPosition(const Value: TInputPosition);
    procedure SetTextHint(const Value: string);
    procedure SetFlat(const Value: boolean);
    procedure SetShowFind(const Value: boolean);
  protected
    procedure Changed;
    procedure AlignmentChanged;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoFind: boolean read FAutoFind write FAutoFind default false;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive default false;
    property EditColor: TColor read FEditColor write SetEditColor default clWindow;
    property HintClose: string read FHintClose write SetHintClose;
    property HintFind: string read FHintFind write SetHintFind;
    property FilterMethod: TFilterMethod read FFilterMethod write SetFilterMethod default fmStartsWith;
    property Flat: boolean read FFlat write SetFlat default true;
    property Position: TInputPosition read FPosition write SetPosition default ipTop;
    property ShowClose: boolean read FShowClose write SetShowClose default false;
    property ShowFind: boolean read FShowFind write SetShowFind default true;
    property Style: TSearchStyle read FStyle write SetStyle;
    property TextHint: string read FTextHint write SetTextHint;
    property Visible: boolean read FVisible write SetVisible default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAlignmentChange: TNotifyEvent read FOnAlignmentChange write FOnAlignmentChange;
  end;

  TInsertOptions = class(TPersistent)
  private
    FVisible: boolean;
    FOnChange: TNotifyEvent;
    FShowClose: boolean;
    FInsertAndClear: boolean;
    FHintClose: string;
    FAutoInsert: boolean;
    FEditColor: TColor;
    FHintInsert: string;
    FPosition: TInputPosition;
    FOnAlignmentChange: TNotifyEvent;
    FTextHint: string;
    FFlat: boolean;
    procedure SetVisible(const Value: boolean);
    procedure SetShowClose(const Value: boolean);
    procedure SetHintClose(const Value: string);
    procedure SetEditColor(const Value: TColor);
    procedure SetHintInsert(const Value: string);
    procedure SetPosition(const Value: TInputPosition);
    procedure SetTextHint(const Value: string);
    procedure SetFlat(const Value: boolean);
  protected
    procedure Changed;
    procedure AlignmentChanged;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoInsert: boolean read FAutoInsert write FAutoInsert default False;
    property EditColor: TColor read FEditColor write SetEditColor default clWindow;
    property Flat: boolean read FFlat write SetFlat default true;
    property InsertAndClear: boolean read FInsertAndClear write FInsertAndClear default false;
    property HintClose: string read FHintClose write SetHintClose;
    property HintInsert: string read FHintInsert write SetHintInsert;
    property Position: TInputPosition read FPosition write SetPosition default ipTop;
    property ShowClose: boolean read FShowClose write SetShowClose default false;
    property TextHint: string read FTextHint write SetTextHint;
    property Visible: boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAlignmentChange: TNotifyEvent read FOnAlignmentChange write FOnAlignmentChange;
  end;

  TCheckBoxClickEvent = procedure(Sender: TObject; Index: integer) of object;

  TListBoxItemEvent = procedure(Sender: TObject; Item: TListBoxItem) of object;

  TListBoxItemHintEvent = procedure(Sender: TObject; Item: TListBoxItem; Index: integer; var HintStr: string) of object;

  TAdvListBox = class(TCustomControl)
  private
    FListBox: TVisualListBox;
    FSearchEdit: TAdvMultiButtonEdit;
    FInsertEdit: TAdvMultiButtonEdit;
    FImages: TCustomImageList;
    FItems: TListBoxItems;
    FStyle: TAdvListBoxStyle;
    FFilterItems: integer;
    FFilterCondition: string;
    FFilterActive: boolean;
    FFilterIndexes: TIntList;
    FSearchOptions: TSearchOptions;
    FInsertOptions: TInsertOptions;
    FItemHeight: integer;
    FSearchShortCut: TShortCut;
    FInsertShortCut: TShortCut;
    FAutoCompleteDelay: integer;
    FAutoComplete: boolean;
    FBorderStyle: TBorderStyle;
    FMultiSelect: boolean;
    FListColor: TColor;
    FOnListDblClick: TNotifyEvent;
    FOnListClick: TNotifyEvent;
    FOnSearchEditDblClick: TNotifyEvent;
    FOnSearchEditChange: TNotifyEvent;
    FOnSearchEditClick: TNotifyEvent;
    FOnInsertEditDblClick: TNotifyEvent;
    FOnInsertEditChange: TNotifyEvent;
    FOnInsertEditClick: TNotifyEvent;
    FOnSearchEditKeyDown: TKeyEvent;
    FOnSearchEditKeyUp: TKeyEvent;
    FOnInsertEditKeyPress: TKeyPressEvent;
    FOnInsertEditKeyDown: TKeyEvent;
    FOnInsertEditKeyUp: TKeyEvent;
    FOnSearchEditKeyPress: TKeyPressEvent;
    FOnListKeyPress: TKeyPressEvent;
    FOnListKeyDown: TKeyEvent;
    FOnListKeyUp: TKeyEvent;
    FOnListCheckBoxClick: TCheckBoxClickEvent;
    FOnSearchClose: TNotifyEvent;
    FOnSearchClick: TNotifyEvent;
    FOnInsertClose: TNotifyEvent;
    FOnInsertClick: TNotifyEvent;
    FOnSearchShow: TNotifyEvent;
    FOnInsertShow: TNotifyEvent;
    FOnInsertItem: TListBoxItemEvent;
    FOnListMouseDown: TMouseEvent;
    FOnListMouseMove: TMouseMoveEvent;
    FOnListMouseUp: TMouseEvent;
    FOnItemHint: TListBoxItemHintEvent;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetItems(const Value: TListBoxItems);
    procedure SetStyle(const Value: TAdvListBoxStyle);
    procedure SetFilterActive(const Value: boolean);
    procedure SetInsertOptions(const Value: TInsertOptions);
    procedure SetSearchOptions(const Value: TSearchOptions);
    procedure SetItemHeight(const Value: integer);
    function GetVersion: string;
    procedure SetAutoComplete(const Value: boolean);
    procedure SetAutoCompleteDelay(const Value: integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    procedure SetMultiSelect(const Value: boolean);
    procedure SetListColor(const Value: TColor);
    function GetSelected(Index: integer): boolean;
    procedure SetSelected(Index: integer; const Value: boolean);
    procedure SetFilterCondition(const Value: string);
    function GetDragModeEx: TDragMode;
    procedure SetDragModeEx(const Value: TDragMode);
  protected
    procedure UpdateItems;
    procedure UpdateAlignment;
    function CreateItems: TListBoxItems; virtual;
    procedure CreateWnd; override;
    procedure Loaded; override;
    function BuildCondition(st: TFilterMethod; s: string): string;
    procedure DoFilter; virtual;
    procedure AlignmentChanged(Sender: TObject); virtual;
    procedure ItemsChanged(Sender: TObject); virtual;
    procedure SearchChanged(Sender: TObject); virtual;
    procedure SearchEditChanged(Sender: TObject); virtual;
    procedure SearchFindClick(Sender: TObject); virtual;
    procedure SearchCloseClick(Sender: TObject); virtual;
    procedure SearchShow; virtual;
    procedure InsertChanged(Sender: TObject); virtual;
    procedure InsertAddClick(Sender: TObject); virtual;
    procedure InsertCloseClick(Sender: TObject); virtual;
    procedure InsertShow; virtual;
    procedure InsertEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure InsertEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure InsertEditKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure SearchEditClick(Sender: TObject); virtual;
    procedure SearchEditDblClick(Sender: TObject); virtual;
    procedure InsertEditChanged(Sender: TObject); virtual;
    procedure InsertEditClick(Sender: TObject); virtual;
    procedure InsertEditDblClick(Sender: TObject); virtual;
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char); virtual;

    procedure ListClick(Sender: TObject); virtual;
    procedure ListDblClick(Sender: TObject); virtual;
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure ListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure ListKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure ListCheckBoxClick(Sender: TObject; Index: integer); virtual;
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ListMouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer); virtual;
    procedure ListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure HandleShortCut(Key: TWMKey);
    procedure HandleSearch(Filter: boolean);
    procedure DoInsertItem(Value: string);
    procedure DoItemHint(AItem: TListBoxItem; Index: integer; var HintStr: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemCount: integer;
    function GetVersionNr: integer;
    property FilterCondition: string read FFilterCondition write SetFilterCondition;
    property FilterActive: boolean read FFilterActive write SetFilterActive;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    function GetItem(Index: integer): TListBoxItem;
    function GetItemAtXY(X,Y: integer): TListBoxItem;
    property Selected[Index: integer]: boolean read GetSelected write SetSelected;
    property SearchEdit: TAdvMultiButtonEdit read FSearchEdit;
    property InsertEdit: TAdvMultiButtonEdit read FInsertEdit;
  published
    property Align;
    property Anchors;
    property AutoComplete: boolean read FAutoComplete write SetAutoComplete default true;
    property AutoCompleteDelay: integer read FAutoCompleteDelay write SetAutoCompleteDelay default 500;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color default clBtnFace;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode: TDragMode read GetDragModeEx write SetDragModeEx default dmManual;
    property Enabled;
    property Font;
    property Hint;
    property Images: TCustomImageList read FImages write SetImages;
    property InsertShortCut: TShortCut read FInsertShortCut write FInsertShortCut default 0;
    property InsertOptions: TInsertOptions read FInsertOptions write SetInsertOptions;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 15;
    property Items: TListBoxItems read FItems write SetItems;
    property ListColor: TColor read FListColor write SetListColor default clWindow;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default false;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property SearchShortCut: TShortCut read FSearchShortCut write FSearchShortCut default 0;
    property SearchOptions: TSearchOptions read FSearchOptions write SetSearchOptions;
    property ShowHint;
    property Style: TAdvListBoxStyle read FStyle write SetStyle;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHIXE_LVL}
    property Touch;
    {$ENDIF}
    property Version: string read GetVersion;
    property Visible;
    property OnContextPopup;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnStartDock;
    property OnStartDrag;

    property OnItemHint: TListBoxItemHintEvent read FOnItemHint write FOnItemHint;

    property OnListClick: TNotifyEvent read FOnListClick write FOnListClick;
    property OnListDblClick: TNotifyEvent read FOnListDblClick write FOnListDblClick;
    property OnListKeyDown: TKeyEvent read FOnListKeyDown write FOnListKeyDown;
    property OnListKeyUp: TKeyEvent read FOnListKeyUp write FOnListKeyUp;
    property OnListKeyPress: TKeyPressEvent read FOnListKeyPress write FOnListKeyPress;
    property OnListCheckBoxClick: TCheckBoxClickEvent read FOnListCheckBoxClick write FOnListCheckBoxClick;
    property OnListMouseDown: TMouseEvent read FOnListMouseDown write FOnListMouseDown;
    property OnListMouseMove: TMouseMoveEvent read FOnListMouseMove write FOnListMouseMove;
    property OnListMouseUp: TMouseEvent read FOnListMouseUp write FOnListMouseUp;

    property OnSearchClick: TNotifyEvent read FOnSearchClick write FOnSearchClick;
    property OnSearchClose: TNotifyEvent read FOnSearchClose write FOnSearchClose;
    property OnSearchShow: TNotifyEvent read FOnSearchShow write FOnSearchShow;
    property OnSearchEditChange: TNotifyEvent read FOnSearchEditChange write FOnSearchEditChange;
    property OnSearchEditClick: TNotifyEvent read FOnSearchEditClick write FOnSearchEditClick;
    property OnSearchEditDblClick: TNotifyEvent read FOnSearchEditDblClick write FOnSearchEditDblClick;
    property OnSearchEditKeyDown: TKeyEvent read FOnSearchEditKeyDown write FOnSearchEditKeyDown;
    property OnSearchEditKeyUp: TKeyEvent read FOnSearchEditKeyUp write FOnSearchEditKeyUp;
    property OnSearchEditKeyPress: TKeyPressEvent read FOnSearchEditKeyPress write FOnSearchEditKeyPress;

    property OnInsertItem: TListBoxItemEvent read FOnInsertItem write FOnInsertItem;
    property OnInsertClick: TNotifyEvent read FOnInsertClick write FOnInsertClick;
    property OnInsertClose: TNotifyEvent read FOnInsertClose write FOnInsertClose;
    property OnInsertShow: TNotifyEvent read FOnInsertShow write FOnInsertShow;
    property OnInsertEditChange: TNotifyEvent read FOnInsertEditChange write FOnInsertEditChange;
    property OnInsertEditClick: TNotifyEvent read FOnInsertEditClick write FOnInsertEditClick;
    property OnInsertEditDblClick: TNotifyEvent read FOnInsertEditDblClick write FOnInsertEditDblClick;
    property OnInsertEditKeyDown: TKeyEvent read FOnInsertEditKeyDown write FOnInsertEditKeyDown;
    property OnInsertEditKeyUp: TKeyEvent read FOnInsertEditKeyUp write FOnInsertEditKeyUp;
    property OnInsertEditKeyPress: TKeyPressEvent read FOnInsertEditKeyPress write FOnInsertEditKeyPress;

  end;

  TControlStyle = (csClassic, csThemed);


  TVisualListBox = class(TListBox)
  private
    FIsThemed: boolean;
    FOwner: TAdvListBox;
    chk: integer;
    FOnCheckBoxClick: TCheckBoxClickEvent;
    FHighlightText: string;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawCheck(R: TRect; State,Enabled,Grayed: Boolean; ControlStyle: TControlStyle);
    procedure DoCheckBoxClick(Index: integer); virtual;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property HighlightText: string read FHighlightText write FHighlightText;
  published
    property OnCheckBoxClick: TCheckBoxClickEvent read FOnCheckBoxClick write FOnCheckBoxClick;
  end;

  TIntList = class(TList)
  private
    FOnChange: TNotifyEvent;
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer):Integer;
    function GetStrValue: string;
    procedure SetStrValue(const Value: string);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    procedure DeleteValue(Value: Integer);
    function HasValue(Value: Integer): Boolean;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: Integer);
    procedure Insert(Index,Value: Integer);
    procedure Delete(Index: Integer);
    property StrValue: string read GetStrValue write SetStrValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses
  uxTheme, ShellAPI, ActiveX, CommCtrl, PictureContainer, Math, Menus;

const
  CHECKBOXSIZE = 16;

{$I HTMLENGO.PAS}

{$I DELPHIXE.INC}

{ TAdvListBox }

procedure TAdvListBox.ListCheckBoxClick(Sender: TObject; Index: integer);
var
  li: TListBoxItem;
begin
  li := GetItem(Index);
  li.Checked := not li.Checked;
  FListBox.Invalidate;

  if Assigned(OnListCheckBoxClick) then
    OnListCheckBoxClick(Self, Index);
end;

procedure TAdvListBox.CMColorChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvListBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FListBox.Enabled := Enabled;
end;

procedure TAdvListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FListBox.Font.Assign(Font);
  FSearchEdit.Font.Assign(Font);
  FInsertEdit.Font.Assign(Font);
end;

constructor TAdvListBox.Create(AOwner: TComponent);
begin
  inherited;
  FListBox := TVisualListBox.Create(Self);
  FListBox.Style := lbOwnerDrawVariable;
  FListBox.OnCheckBoxClick := ListCheckBoxClick;
  FListBox.OnClick := ListClick;
  FListBox.OnDblClick := ListDblClick;
  FListBox.OnKeyDown := ListKeyDown;
  FListBox.OnKeyUp := ListKeyUp;
  FListBox.OnKeyPress := ListKeyPress;
  FListBox.OnMouseDown := ListMouseDown;
  FListBox.OnMouseUp := ListMouseUp;
  FListBox.OnMouseMove := ListMouseMove;
  FListBox.OnDragOver := ListDragOver;
  FListBox.OnDragDrop := ListDragDrop;
  FSearchEdit := TAdvMultiButtonEdit.Create(Self);
  FSearchEdit.OnChange := SearchEditChanged;
  FSearchEdit.Buttons.Add.Style := bsFind;
  FSearchEdit.Buttons[0].Flat := true;
  FSearchEdit.OnClickFind := SearchFindClick;
  FSearchEdit.OnClickClose := SearchCloseClick;
  FSearchEdit.OnKeyDown := SearchEditKeyDown;
  FSearchEdit.OnKeyUp := SearchEditKeyUp;
  FSearchEdit.OnKeyPress := SearchEditKeyPress;
  FSearchEdit.OnClick := SearchEditClick;
  FSearchEdit.OnDblClick := SearchEditDblClick;
  FInsertEdit := TAdvMultiButtonEdit.Create(Self);
  FInsertEdit.Buttons.Add.Style := bsAdd;
  FInsertEdit.Buttons[0].Flat := true;
  FInsertEdit.OnChange := InsertEditChanged;
  FInsertEdit.OnClickAdd := InsertAddClick;
  FInsertEdit.OnClickClose := InsertCloseClick;
  FInsertEdit.OnKeyDown := InsertEditKeyDown;
  FInsertEdit.OnKeyUp := InsertEditKeyUp;
  FInsertEdit.OnKeyPress := InsertEditKeyPress;
  FInsertEdit.OnClick := InsertEditClick;
  FInsertEdit.OnDblClick := InsertEditDblClick;
  Width := 200;
  Height := 300;
  FItemHeight := 15;
  FListBox.ItemHeight := FItemHeight;
  FItems := CreateItems;
  FItems.OnChange := ItemsChanged;
  DoubleBuffered := true;
  FSearchOptions := TSearchOptions.Create;
  FSearchOptions.OnChange := SearchChanged;
  FSearchOptions.OnAlignmentChange := AlignmentChanged;
  FInsertOptions := TInsertOptions.Create;
  FInsertOptions.OnChange := InsertChanged;
  FInsertOptions.OnAlignmentChange := AlignmentChanged;
  FAutoComplete := true;
  FAutoCompleteDelay := 500;
  FBorderStyle := bsSingle;
  Color := clBtnFace;
  FListColor := clWindow;
  FMultiSelect := false;
end;

function TAdvListBox.CreateItems: TListBoxItems;
begin
  Result := TListBoxItems.Create;
end;

procedure TAdvListBox.CreateWnd;
begin
  inherited;
  FSearchEdit.Parent := Self;
  FInsertEdit.Parent := Self;

  UpdateAlignment;

  FSearchEdit.Margins.Left := 0;
  FSearchEdit.Margins.Top := 0;
  FSearchEdit.Margins.Bottom := 2;
  FSearchEdit.AlignWithMargins := true;
  FInsertEdit.Margins.Left := 0;
  FInsertEdit.Margins.Top := 0;
  FInsertEdit.Margins.Bottom := 2;
  FInsertEdit.AlignWithMargins := true;

  FListBox.Parent := Self;
  FListBox.Align := alClient;

  FSearchEdit.TabOrder := 0;
  FSearchEdit.TabStop := true;
  FInsertEdit.TabOrder := 1;
  FInsertEdit.TabStop := true;
  FListBox.TabOrder := 2;
  FListBox.TabStop := true;

  FFilterIndexes := TIntList.Create;

  UpdateItems;
end;

destructor TAdvListBox.Destroy;
begin
  FItems.Free;
  FFilterIndexes.Free;
  FSearchOptions.Free;
  FInsertOptions.Free;
  inherited;
end;

function StripThousandSep(ps: pchar):string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  s := strpas(ps);
  for i := 1 to Length(s) do
  begin
    if s[i] = DecimalSeparator then
      Result := Result + '.' else
      if s[i] <> ThousandSeparator then
        Result := Result + s[i];
  end;
end;


function IsDate(s:string;var dt:TDateTime):boolean;
var
  su, ts: string;
  da,mo,ye,ho,mi,se: word;
  err: Integer;
  dp,mp,yp,vp: Integer;
begin
  Result := False;

  ts := '';

  su := UpperCase(shortdateformat);
  dp := pos('D',su);
  mp := pos('M',su);
  yp := pos('Y',su);

  da := 0;
  mo := 0;
  ye := 0;
  ho := 0;
  mi := 0;
  se := 0;

  if VarPos(DateSeparator,s,vp)>0 then
  begin
    su := Copy(s,1,vp - 1);

    if (dp<mp) and
       (dp<yp) then
       val(su,da,err)
    else
    if (mp<dp) and
       (mp<yp) then
       val(su,mo,err)
    else
    if (yp<mp) and
       (yp<dp) then
       val(su,ye,err);

    if err<>0 then Exit;
    Delete(s,1,vp);

    if VarPos(DateSeparator,s,vp)>0 then
    begin
      su := Copy(s,1,vp - 1);

      if ((dp>mp) and (dp<yp)) or
         ((dp>yp) and (dp<mp)) then
         val(su,da,err)
      else
      if ((mp>dp) and (mp<yp)) or
         ((mp>yp) and (mp<dp)) then
         val(su,mo,err)
      else
      if ((yp>mp) and (yp<dp)) or
         ((yp>dp) and (yp<mp)) then
         val(su,ye,err);

      if err<>0 then Exit;
      Delete(s,1,vp);

      s := Trim(s);

      if VarPos(' ',s, vp) > 0 then  // there is space to separate date & time
      begin
        ts := copy(s, vp, length(s));
        s := copy(s, 1, vp - 1);
      end;

      if (dp>mp) and
         (dp>yp) then
         val(s,da,err)
      else
      if (mp>dp) and
         (mp>yp) then
         val(s,mo,err)
      else
      if (yp>mp) and
         (yp>dp) then
         val(s,ye,err);

      if err<>0 then Exit;
      if (da>31) then Exit;
      if (mo>12) then Exit;

      if (ts <> '') then  // there is a time part
      begin
        if VarPos(TimeSeparator,ts,vp)>0 then
        begin
          su := Copy(ts,1,vp - 1); // hour part
          val(su,ho,err);

          if (err <> 0) then Exit;
          if (ho > 23) then Exit;

          Delete(ts,1,vp);

          if VarPos(TimeSeparator,ts,vp)>0 then // there is a second part
          begin
            su := Copy(ts,1,vp - 1); // minute part
            val(su,mi,err);

            if err <> 0 then Exit;
            Delete(ts,1,vp);

            val(ts,se,err);  // second part
            if (err <> 0) then Exit;
            if (se > 60) then Exit;
          end
          else
          begin
            val(su,mi,err); // minute part
            if (err <> 0) then Exit;
          end;

          if (mi > 59) then Exit;

          Result := true;
        end;
      end
      else
        Result := True;

      try
        dt := EncodeDate(ye,mo,da) + EncodeTime(ho,mi,se,0);
      except
        Result := False;
      end;
    end;
  end;
end;



function Matches(s0a,s1a: PChar): Boolean;
const
  larger = '>';
  smaller = '<';
  logand  = '&';
  logor   = '^';
  asterix = '*';
  qmark = '?';
  negation = '!';
  null = #0;

var
  matching:boolean;
  done:boolean;
  len:longint;
  lastchar:char;
  s0,s1,s2,s3:pchar;
  oksmaller,oklarger,negflag:boolean;
  compstr:array[0..255] of char;
  flag1,flag2,flag3:boolean;
  equal:boolean;
  n1,n2:double;
  code1,code2:Integer;
  dt1,dt2:TDateTime;
  q: integer;
begin
  oksmaller := True;
  oklarger := True;
  flag1 := False;
  flag2 := False;
  flag3 := False;
  negflag := False;
  equal := False;

  { [<>] string [&|] [<>] string }

  // do larger than or larger than or equal
  s2 := StrPos(s0a,larger);
  if s2 <> nil then
  begin
    inc(s2);
    if (s2^ = '=') then
    begin
      Equal := True;
      inc(s2);
    end;

    while (s2^ = ' ') do
      inc(s2);

    s3 := s2;
    len := 0;

    lastchar := #0;

    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|')))  do
    begin
      if (s2^= '"') then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);

      if (s2^= ' ') and odd(q) then // skip space if between quotes
      begin
        lastchar := s2^;
        inc(s2);
      end;
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    StrLCopy(compstr,s3,len);

    Val(StripThousandSep(s1a),n1,code1);
    Val(StripThousandSep(compstr),n2,code2);

    if IsDate(compstr,dt2) then code2 := 1;
    if IsDate(s1a,dt1) then code1 := 1;

    if (code1 = 0) and (code2 = 0) then {both are numeric types}
    begin
      if equal then
        oklarger := n1 >= n2
      else
        oklarger := n1 > n2;
    end
    else
    begin
      if IsDate(StrPas(compstr),dt2) and IsDate(StrPas(s1a),dt1) then
      begin
        if equal then
         oklarger := dt1 >= dt2
        else
         oklarger := dt1 > dt2;
      end
      else
      begin
        if equal then
         oklarger := (strlcomp(compstr,s1a,255)<=0)
        else
         oklarger := (strlcomp(compstr,s1a,255)<0);
      end;
    end;
    flag1 := True;
  end;

  equal := False;

  // do smaller than or smaller than or equal
  s2 := strpos(s0a,smaller);
  if (s2 <> nil) then
  begin
    inc(s2);
    if (s2^ = '=') then
      begin
       equal := True;
       inc(s2);
      end;

    lastchar := #0;

    while (s2^=' ') do inc(s2);
    s3 := s2;
    len := 0;
    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|'))) do
    begin
      if s2^ = '"' then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);

    Val(StripThousandSep(s1a),n1,code1);
    Val(StripThousandSep(compstr),n2,code2);
    if IsDate(compstr,dt2) then code2 := 1;
    if IsDate(s1a,dt1) then code1 := 1;

    if (code1 = 0) and (code2 = 0) then // both are numeric types
     begin
      if equal then
       oksmaller := n1 <= n2
      else
       oksmaller := n1 < n2;
     end
    else
     begin
      // check for dates here ?
      if IsDate(strpas(compstr),dt2) and IsDate(strpas(s1a),dt1) then
       begin
        if equal then
         oksmaller := dt1 <= dt2
        else
         oksmaller := dt1 < dt2;
       end
      else
       begin
        if equal then
          oksmaller := (strlcomp(compstr,s1a,255)>=0)
        else
          oksmaller := (strlcomp(compstr,s1a,255)>0);
       end;
     end;

    flag2 := True;
  end;

  s2 := strpos(s0a,negation);

  if (s2 <> nil) then
  begin
    inc(s2);
    while (s2^=' ') do
      inc(s2);
    s3 := s2;
    len := 0;

    lastchar := #0;
    q := 0;

    while (s2^ <> ' ') and (s2^ <> NULL) and (odd(q) or ((s2^ <> '&') and (s2^ <> '|'))) do
    begin
      if (s2^ = '"') then
        inc(q);

      if (len = 0) and (s2^ = '"') then
        inc(s3)
      else
        inc(len);

      lastchar := s2^;
      inc(s2);
    end;

    if (len > 0) and (lastchar = '"') then
      dec(len);

    strlcopy(compstr,s3,len);
    flag3 := True;
  end;

  if (flag3) then
  begin
    if strpos(s0a,larger) = nil then
      flag1 := flag3;
    if strpos(s0a,smaller) = nil then
      flag2 := flag3;
  end;

  if (strpos(s0a,logor) <> nil) then
    if flag1 or flag2 then
    begin
      matches := oksmaller or oklarger;
      Exit;
    end;

  if (strpos(s0a,logand)<>nil) then
    if flag1 and flag2 then
    begin
      matches := oksmaller and oklarger;
      Exit;
    end;

  if ((strpos(s0a,larger) <> nil) and (oklarger)) or
     ((strpos(s0a,smaller) <> nil) and (oksmaller)) then
  begin
    matches := True;
    Exit;
  end;

  s0 := s0a;
  s1 := s1a;

  matching := True;

  done := (s0^ = NULL) and (s1^ = NULL);

  while not done and matching do
  begin
    case s0^ of
    qmark:
      begin
        matching := s1^ <> NULL;
        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    negation:
      begin
        negflag:=True;
        inc(s0);
      end;
    '"':
      begin
        inc(s0);
      end;
    (*
    '\':
      begin
        inc(s0);
        matching := s0^ = s1^;

        if matching then
        begin
          inc(s0);
          inc(s1);
        end;
      end;
    *)
    asterix:
      begin
        repeat
          inc(s0)
        until (s0^ <> asterix);
        len := strlen(s1);
        inc(s1,len);
        matching := matches(s0,s1);
        while (len >= 0) and not matching do
        begin
         dec(s1);
         dec(len);
         matching := Matches(s0,s1);
       end;
       if matching then
       begin
         s0 := strend(s0);
         s1 := strend(s1);
       end;
     end;
   else
     begin
       matching := s0^ = s1^;

       if matching then
       begin
         inc(s0);
         inc(s1);
       end;
     end;
   end;

   Done := (s0^ = NULL) and (s1^ = NULL);
  end;

  if negflag then
    Matches := not matching
  else
    Matches := matching;
end;


function MatchStr(s1,s2:string;DoCase:Boolean):Boolean;
begin
  if DoCase then
    MatchStr := Matches(PChar(s1),PChar(s2))
  else
    MatchStr := Matches(PChar(AnsiUpperCase(s1)),PChar(AnsiUpperCase(s2)));
end;

procedure TAdvListBox.AlignmentChanged(Sender: TObject);
begin
  UpdateAlignment;
end;

function TAdvListBox.BuildCondition(st: TFilterMethod; s: string): string;
begin
  if Pos(' ',s) > 0 then
    s := '"' + s + '"';

  case st of
  fmEqual: Result := s;
  fmStartsWith: Result := s + '*';
  fmEndsWith: Result := '*' + s;
  fmContains: Result := '*' + s + '*';
  fmNotContains: Result := '!*' + s + '*';
  fmNotEqual: Result := '!' + s;
  end;
end;

procedure TAdvListBox.DoFilter;
var
  i: integer;
  f: boolean;
  szFilter: string;

begin
  FListBox.ItemIndex := -1;
  FFilterItems := 0;

  FFilterIndexes.Clear;

  szFilter := BuildCondition(SearchOptions.FilterMethod, FFilterCondition);

  for i := 0 to Items.Count - 1 do
  begin
    f := false;
    if MatchStr(szFilter, Items[i].Text, false) then
    begin
      inc(FFilterItems);
      f := true;
      FFilterIndexes.Add(i);
    end;

    Items[i].Filtered := f;
    Items[i].FilterIndex := FFilterItems - 1;
  end;
end;


procedure TAdvListBox.DoInsertItem(Value: string);
var
  li: TListBoxItem;
begin
  li := Items.Add;
  li.ImageIndex := -1;
  li.Text := Value;
  FListBox.ItemIndex := FListBox.Items.Count - 1;
  if InsertOptions.InsertAndClear then
    FInsertEdit.Text := '';

  if Assigned(OnInsertItem) then
  begin
    OnInsertItem(Self, li);
    Invalidate;
  end;
end;

procedure TAdvListBox.DoItemHint(AItem: TListBoxItem; Index: integer;
  var HintStr: string);
begin
  if Assigned(OnItemHint) then
    OnItemHint(Self, AItem, Index, HintStr);
end;

function TAdvListBox.GetDragModeEx: TDragMode;
begin
  Result := inherited DragMode;
end;

function TAdvListBox.GetItem(Index: integer): TListBoxItem;
begin
  if FFilterActive then
  begin
    if Index >= FFilterIndexes.Count  then
      Result := nil
    else
      Result := Items[FFilterIndexes[Index]];
  end
  else
    if Index < Items.Count then
      Result := Items[Index]
    else
      Result := nil;
end;

function TAdvListBox.GetItemAtXY(X, Y: integer): TListBoxItem;
var
  idx: integer;
begin
  Result := nil;
  idx := SendMessage(FListbox.Handle, LB_ITEMFROMPOINT,0, MakeLParam(X,Y));
  if idx >= 0 then
    Result := GetItem(idx);
end;

function TAdvListBox.GetItemIndex: integer;
begin
  Result := -1;
  if Assigned(FListBox) then
  begin
    if FFilterActive then
    begin
      if (FListBox.ItemIndex = -1) then
        Result := -1
      else
        Result := FFilterIndexes[FListBox.ItemIndex];
    end
    else
      Result := FListBox.ItemIndex;
  end;
end;

function TAdvListBox.GetSelected(Index: integer): boolean;
begin
  Result := FListBox.Selected[Index];
end;

function TAdvListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvListBox.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvListBox.HandleSearch(Filter: boolean);
var
  i,j: integer;
  notlast, found: boolean;
  srch: string;
begin
  if (FSearchEdit.Text <> '') and Filter then
  begin
    case SearchOptions.Style of
    ssFilter:
      begin
        FilterCondition := FSearchEdit.Text;
        FilterActive := false;
        FilterActive := true;
      end;
    ssHighlight:
      begin
        FListBox.HighlightText := FSearchEdit.Text;
        FListBox.Invalidate;
      end;
    ssGoto:
      begin
        j := FListBox.ItemIndex;
        notlast := false;

        srch := BuildCondition(SearchOptions.FilterMethod, FSearchEdit.Text);

        if (j >= 0) then
        begin
          if not MatchStr(srch, Items[j].Text, SearchOptions.CaseSensitive) then
          begin
            inc(j);
            notlast := true;
          end
          else
            j := 0;
        end
        else
          j := 0;

        found := false;

        for i := j to Items.Count - 1 do
        begin
          if MatchStr(srch, Items[i].Text, SearchOptions.CaseSensitive) then
          begin
            FListBox.ItemIndex := i;
            found := true;
            Break;
          end;
        end;

        if not found and notlast then
        begin
          for i := 0 to Items.Count - 1 do
          begin
            if MatchStr(srch, Items[i].Text, SearchOptions.CaseSensitive) then
            begin
              FListBox.ItemIndex := i;
              Break;
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    FilterActive := false;
    FListBox.HighlightText := '';
    FListBox.Invalidate;
  end;
end;

procedure TAdvListBox.HandleShortCut(Key: TWMKey);
begin
  if ShortCutFromMessage(Key) = SearchShortCut then
  begin
    SearchShow;

    // force visible
    SearchOptions.Visible := false;
    SearchOptions.Visible := true;
    FSearchEdit.SetFocus;
  end;

  if ShortCutFromMessage(Key) = InsertShortCut then
  begin
    InsertShow;

    // force visible
    InsertOptions.Visible := false;
    InsertOptions.Visible := true;
    FInsertEdit.SetFocus;
  end;
end;

procedure TAdvListBox.InsertAddClick(Sender: TObject);
begin
  if Assigned(OnInsertClick) then
    OnInsertClick(Self);

  if (FInsertEdit.Text <> '') then
  begin
    DoInsertItem(FInsertEdit.Text);
  end;
end;

procedure TAdvListBox.InsertChanged(Sender: TObject);
var
  b: TEditButton;
begin

  FInsertEdit.Visible := InsertOptions.Visible;

  b := FInsertEdit.Buttons.FindButton(bsClose);

  if InsertOptions.ShowClose and not Assigned(b) then
  begin
    b := FInsertEdit.Buttons.Add;
    b.Style := bsClose;
    b.Flat := InsertOptions.Flat;
  end;

  if not InsertOptions.ShowClose and Assigned(b) then
  begin
    b.Free;
  end;

  b := FInsertEdit.Buttons.FindButton(bsClose);
  if Assigned(b) then
  begin
    b.Hint := InsertOptions.HintClose;
  end;

  b := FInsertEdit.Buttons.FindButton(bsAdd);
  if Assigned(b) then
    b.Hint := InsertOptions.HintInsert;

  FInsertEdit.EditColor := InsertOptions.EditColor;
  FInsertEdit.Edit.EmptyText := InsertOptions.TextHint;
end;

procedure TAdvListBox.InsertCloseClick(Sender: TObject);
begin
  if Assigned(OnInsertClose) then
    OnInsertClose(Self);
  FListBox.SetFocus;
end;

procedure TAdvListBox.InsertEditChanged(Sender: TObject);
begin
  if Assigned(OnInsertEditChange) then
    OnInsertEditChange(Self);
end;

procedure TAdvListBox.InsertEditClick(Sender: TObject);
begin
  if Assigned(OnInsertEditClick) then
    OnInsertEditClick(Self);
end;

procedure TAdvListBox.InsertEditDblClick(Sender: TObject);
begin
  if Assigned(OnInsertEditDblClick) then
    OnInsertEditDblClick(Self);
end;

procedure TAdvListBox.InsertEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnInsertEditKeyPress) then
    OnInsertEditKeyPress(Self, Key);
end;

procedure TAdvListBox.InsertEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnInsertEditKeyUp) then
    OnInsertEditKeyUp(Self, Key, Shift);
end;

procedure TAdvListBox.InsertShow;
begin
  if Assigned(OnInsertShow) then
    OnInsertShow(Self);
end;

procedure TAdvListBox.InsertEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnInsertEditKeyDown) then
    OnInsertEditKeyDown(Self, Key, Shift);

  if (Key = VK_RETURN) and InsertOptions.AutoInsert and (FInsertEdit.Text <> '') then
  begin
    DoInsertItem(FInsertEdit.Text);
  end;
end;

function TAdvListBox.ItemCount: integer;
begin
  if FFilterActive then
    Result := FFilterItems
  else
    Result := Items.Count;
end;

procedure TAdvListBox.ItemsChanged(Sender: TObject);
begin
  UpdateItems;
  Invalidate;
end;

procedure TAdvListBox.ListClick(Sender: TObject);
begin
  if Assigned(OnListClick) then
    OnListClick(Self);
end;

procedure TAdvListBox.ListDblClick(Sender: TObject);
begin
  if Assigned(OnListDblClick) then
    OnListDblClick(Self);
end;

procedure TAdvListBox.ListDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X,Y);
end;

procedure TAdvListBox.ListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TAdvListBox.ListEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(OnEndDrag) then
    OnEndDrag(Self, Target, X, Y);
end;

procedure TAdvListBox.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnListKeyDown) then
    OnListKeyDown(Self, Key, Shift);
end;

procedure TAdvListBox.ListKeyPress(Sender: TObject; var Key: Char);
begin
  if Ord(Key) = VK_SPACE then
  begin
    Key := #0;
  end
  else
    if Assigned(OnListKeyPress) then
      OnListKeyPress(Self, Key);
end;

procedure TAdvListBox.ListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnListKeyUp) then
    OnListKeyUp(Self, Key, Shift);
end;

procedure TAdvListBox.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnListMouseDown) then
    OnListMouseDown(Self, Button, Shift, X, Y);
end;

procedure TAdvListBox.ListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(OnListMouseMove) then
    OnListMouseMove(Self, Shift, X, Y);
end;

procedure TAdvListBox.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnListMouseUp) then
    OnListMouseUp(Self, Button, Shift, X, Y);
end;

procedure TAdvListBox.ListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if Assigned(OnStartDrag) then
    OnStartDrag(Self, DragObject);
end;

procedure TAdvListBox.Loaded;
begin
  inherited;
  SearchChanged(Self);
  InsertChanged(Self);
end;

procedure TAdvListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TAdvListBox.SearchChanged(Sender: TObject);
var
  b: TEditButton;
begin
  FSearchEdit.Visible := SearchOptions.Visible;

  b := FSearchEdit.Buttons.FindButton(bsClose);

  if SearchOptions.ShowClose and not Assigned(b) then
  begin
    b := FSearchEdit.Buttons.Add;
    b.Style := bsClose;
    b.Flat := SearchOptions.Flat;
  end;

  if Assigned(b) then
    b.Hint := SearchOptions.HintClose;

  if not SearchOptions.ShowClose and Assigned(b) then
  begin
    b.Free;
  end;

  b := FSearchEdit.Buttons.FindButton(bsFind);

  if SearchOptions.ShowFind and not Assigned(b) then
  begin
    b := FSearchEdit.Buttons.Add;
    b.Style := bsFind;
    b.Flat := SearchOptions.Flat;
  end;

  if Assigned(b) then
    b.Hint := SearchOptions.HintFind;

  if not SearchOptions.ShowFind and Assigned(b) then
  begin
    b.Free;
  end;

  FSearchEdit.EditColor := SearchOptions.EditColor;

  FSearchEdit.Edit.EmptyText := SearchOptions.TextHint;

  if SearchOptions.Style <> ssHighlight then
    FListBox.HighlightText := '';
end;

procedure TAdvListBox.SearchCloseClick(Sender: TObject);
begin
  if Assigned(OnSearchClose) then
    OnSearchClose(Self);

  FListBox.SetFocus;
end;

procedure TAdvListBox.SearchEditChanged(Sender: TObject);
var
  eb: TEditButton;
begin
  if Assigned(OnSearchEditChange) then
    OnSearchEditChange(Self);
  if SearchOptions.AutoFind then
  begin
    HandleSearch(true);
    if SearchOptions.Style = ssFilter then
    begin
      eb := FSearchEdit.Buttons.FindButton(bsFind);
      if Assigned(eb) then
      begin
        eb.Button.AllowAllUp := false;
        eb.Button.GroupIndex := 1;
        eb.Button.Down := true;
      end;
    end;
  end;
end;

procedure TAdvListBox.SearchEditClick(Sender: TObject);
begin
  if Assigned(OnSearchEditClick) then
    OnSearchEditClick(Self);
end;

procedure TAdvListBox.SearchEditDblClick(Sender: TObject);
begin
  if Assigned(OnSearchEditDblClick) then
    OnSearchEditDblClick(Self);
end;

procedure TAdvListBox.SearchEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnSearchEditKeyDown) then
    OnSearchEditKeyDown(Self,Key,Shift);

  if Key = VK_ESCAPE then
  begin
    if (SearchOptions.Style = ssFilter) and (SearchOptions.AutoFind) then
    begin
      SearchEdit.Text := '';
    end;
  end;

end;

procedure TAdvListBox.SearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(OnSearchEditKeyPress) then
    OnSearchEditKeyPress(Self,Key);
end;

procedure TAdvListBox.SearchEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(OnSearchEditKeyUp) then
    OnSearchEditKeyUp(Self,Key,Shift);
end;

procedure TAdvListBox.SearchFindClick(Sender: TObject);
begin
  if Assigned(OnSearchClick) then
    OnSearchClick(Self);

  if SearchOptions.Style = ssFilter then
  begin
    if FSearchEdit.Buttons.FindButton(bsFind).Button.Down then
    begin
      FSearchEdit.Buttons.FindButton(bsFind).Button.AllowAllUp := true;
      FSearchEdit.Buttons.FindButton(bsFind).Button.GroupIndex := 0;
      FSearchEdit.Buttons.FindButton(bsFind).Button.Down := false;
      HandleSearch(false);
    end
    else
    begin
      if (FSearchEdit.Text <> '') then
      begin
        FSearchEdit.Buttons.FindButton(bsFind).Button.AllowAllUp := false;
        FSearchEdit.Buttons.FindButton(bsFind).Button.GroupIndex := 1;
        FSearchEdit.Buttons.FindButton(bsFind).Button.Down := true;
        HandleSearch(true);
      end;
    end;
  end
  else
    HandleSearch(true);
end;

procedure TAdvListBox.SearchShow;
begin
  if Assigned(OnSearchShow) then
    OnSearchShow(Self);
end;

procedure TAdvListBox.SetAutoComplete(const Value: boolean);
begin
  FAutoComplete := Value;
  if Assigned(FListBox) then
    FListBox.AutoComplete := Value;
end;

procedure TAdvListBox.SetAutoCompleteDelay(const Value: integer);
begin
  FAutoCompleteDelay := Value;
  if Assigned(FListBox) then
    FListBox.AutoCompleteDelay := Value;
end;

procedure TAdvListBox.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  if Assigned(FListBox) then
    FListBox.BorderStyle := Value;
end;

procedure TAdvListBox.SetDragModeEx(const Value: TDragMode);
begin
  inherited DragMode := Value;
  FListBox.DragMode := Value;
end;

procedure TAdvListBox.SetFilterActive(const Value: boolean);
begin
  if (FFilterActive <> Value) then
  begin
    FFilterActive := Value;

    if FFilterActive then
      DoFilter;

    UpdateItems;
    Invalidate;
  end;
end;

procedure TAdvListBox.SetFilterCondition(const Value: string);
begin
  FFilterCondition := Value;
  DoFilter;
  UpdateItems;
  Invalidate;
  FSearchEdit.Text := FFilterCondition;
end;

procedure TAdvListBox.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

procedure TAdvListBox.SetInsertOptions(const Value: TInsertOptions);
begin
  FInsertOptions.Assign(Value);
end;

procedure TAdvListBox.SetItemHeight(const Value: integer);
begin
  if (FItemHeight <> Value) then
  begin
    FListBox.ItemHeight := Value;
    FItemHeight := Value;
    Invalidate;
  end;
end;

procedure TAdvListBox.SetItemIndex(const Value: integer);
begin
  if Assigned(FListBox) then
    FListBox.ItemIndex := Value;
end;

procedure TAdvListBox.SetItems(const Value: TListBoxItems);
begin
  FItems := Value;
end;

procedure TAdvListBox.SetListColor(const Value: TColor);
begin
  FListColor := Value;
  if Assigned(FListBox) then
    FListBox.Color := Value;
end;

procedure TAdvListBox.SetMultiSelect(const Value: boolean);
begin
  FMultiSelect := Value;
  if Assigned(FListBox) then
    FListBox.MultiSelect := Value;
end;

procedure TAdvListBox.SetSearchOptions(const Value: TSearchOptions);
begin
  FSearchOptions.Assign(Value);
end;

procedure TAdvListBox.SetSelected(Index: integer; const Value: boolean);
begin
  FListbox.Selected[Index] := Value;
end;

procedure TAdvListBox.SetStyle(const Value: TAdvListBoxStyle);
begin
  FStyle := Value;
  Invalidate;
end;

procedure TAdvListBox.UpdateAlignment;
begin
  if SearchOptions.Position = ipTop then
  begin
    FSearchEdit.Top := 1;
    FSearchEdit.Align := alTop;
  end
  else
  begin
    FSearchEdit.Top := Height - 3;
    FSearchEdit.Align := alBottom;
  end;


  if InsertOptions.Position = ipTop then
  begin
    FInsertEdit.Top := 10;
    FInsertEdit.Align := alTop;
  end
  else
  begin
    FInsertEdit.Top := Height - 10;
    FInsertEdit.Align := alBottom;
  end;
end;

procedure TAdvListBox.UpdateItems;
begin
  while FListBox.Count > ItemCount do
  begin
    FListBox.Items.Delete(0);
  end;

  while FListBox.Count < ItemCount do
  begin
    FListBox.Items.Add(' ');
  end;
end;

procedure TAdvListBox.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  HandleShortCut(Msg);
end;

procedure TAdvListBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  FListBox.SetFocus;
end;

{ TListBoxItem }

procedure TListBoxItem.Assign(Source: TPersistent);
begin
  if (Source is TListBoxItem) then
  begin
    FChecked := (Source as TListBoxItem).Checked;
    FImageIndex := (Source as TListBoxItem).ImageIndex;
    FText := (Source as TListBoxItem).Text;
    FTag := (Source as TListBoxItem).Tag;
    FObject := (Source as TListBoxItem).FObject;
    FHasCheckBox := (Source as TListBoxItem).HasCheckBox;
  end;
end;

constructor TListBoxItem.Create(Collection: TCollection);
begin
  inherited;
  FImageIndex := -1;
  FTag := 0;
  FChecked := false;
end;

procedure TListBoxItem.SetChecked(const Value: boolean);
begin
  FChecked := Value;
end;

procedure TListBoxItem.SetHasCheckBox(const Value: boolean);
begin
  FHasCheckBox := Value;
end;

procedure TListBoxItem.SetImageIndex(const Value: integer);
begin
  FImageIndex := Value;
end;

procedure TListBoxItem.SetText(const Value: string);
begin
  FText := Value;
end;


{ TListBoxItems }

function TListBoxItems.Add: TListBoxItem;
begin
  Result := TListBoxItem(inherited Add);
end;

constructor TListBoxItems.Create;
begin
  inherited Create(TListBoxItem);
end;

destructor TListBoxItems.Destroy;
begin
  inherited;
end;

function TListBoxItems.GetItems(Index: integer): TListBoxItem;
begin
  Result := TListBoxItem(inherited Items[Index]);
end;

function TListBoxItems.IndexOf(Value: string): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    if Value = Items[i].Text then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TListBoxItems.Insert(Index: integer): TListBoxItem;
begin
  Result := TListBoxItem(inherited Insert(Index));
end;

procedure TListBoxItems.Move(FromIndex, ToIndex: integer);
var
  s: string;
  FTag, FImageIndex: integer;
  FChecked: Boolean;
  li: TListBoxItem;
  FObject: TObject;
begin
  if FromIndex >= Count then
    raise Exception.Create('Invalid item index');
  if ToIndex >= Count then
    raise Exception.Create('Invalid item index');

  BeginUpdate;

  try
    s := Items[FromIndex].Text;
    FTag := Items[FromIndex].Tag;
    FImageIndex := Items[FromIndex].ImageIndex;
    FChecked := Items[FromIndex].Checked;
    FObject := Items[FromIndex].ItemObject;

    Delete(FromIndex);

    li := Insert(ToIndex);
    li.Text := s;
    li.Tag := FTag;
    li.ImageIndex := FImageIndex;
    li.Checked := FChecked;
    li.ItemObject := FObject;
  finally
    EndUpdate;
  end;
end;

procedure TListBoxItems.SetItems(Index: integer; const Value: TListBoxItem);
begin
  inherited Items[Index] := Value;
end;

procedure TListBoxItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;


{ TVisualListBox }

procedure TVisualListBox.CMHintShow(var Msg: TMessage);
var
  hi: PHintInfo;
  Idx: Integer;
  li: TListBoxItem;
  s: string;
  R: TRect;
begin
  hi := PHintInfo(Msg.LParam);

  s := FOwner.Hint;

  inherited;

  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(Hi^.CursorPos.X,Hi^.CursorPos.Y));

  SendMessage(Handle,LB_GETITEMRECT,Idx,LParam(@R));

  if PtInRect(R,Point(Hi^.CursorPos.X,Hi^.CursorPos.Y)) then
  begin
    li := FOwner.GetItem(Idx);
    if Assigned(li) then
      FOwner.DoItemHint(li,idx,s);
  end;

  hi^.HintStr := s;
end;

constructor TVisualListBox.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := TAdvListBox(AOwner);
end;

procedure TVisualListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  li: TListBoxItem;
  a,s,fa: string;
  xs,ys,hl,ml: integer;
  hr: TRect;
begin
  if (odSelected in State) then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font.Color := Font.Color;
  end;

  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(Rect);

  Rect.Left := Rect.Left + 2;

  li := FOwner.GetItem(Index);

  if not Assigned(li) then
    Exit;

  if FOwner.Style = stCheckList then
  begin
    if FIsThemed and IsThemeActive then
      DrawCheck(Rect, li.Checked, True, False, csThemed)
    else
      DrawCheck(Rect, li.Checked, True, False, csClassic);

    Rect.Left := Rect.Left + CHECKBOXSIZE;
  end;

  if Assigned(FOwner.Images) and (li.ImageIndex >= 0) then
  begin
    FOwner.Images.Draw(Canvas, Rect.Left, Rect.Top, li.ImageIndex);
    Rect.Left := Rect.Left + FOwner.Images.Width;
  end;

  Rect.Left := Rect.Left + 2;

  s := li.Text;

  if (FHighlightText <> '') then
    s := Hilight(s, FHighlightText,'hi',FOwner.SearchOptions.CaseSensitive);

  Rect.Top := Rect.Top - 2;
  HTMLDrawEx(Canvas, s, Rect, FOwner.Images, -1, -1,-1,-1,2,false,false,false,
   (odSelected in State),false,false,false,1.0,clBlue,clNone, clNone, clGray, a,s,fa,xs,ys,hl,ml,hr, nil, nil, 2);
end;

procedure TVisualListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOwner.Images) then
    Height := Max(FOwner.FItemHeight, FOwner.Images.Height + 4)
  else
    Height := FOwner.FItemHeight;
end;

procedure TVisualListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  chk := -1;
  if (FOwner.Style = stCheckList) and (x < CHECKBOXSIZE) then
  begin
    chk := SendMessage(Handle, LB_ITEMFROMPOINT,0,MakeLParam(X,Y));
  end;
end;

procedure TVisualListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (FOwner.Style = stCheckList) and (x < CHECKBOXSIZE) then
  begin
    if SendMessage(Handle, LB_ITEMFROMPOINT,0,MakeLParam(X,Y)) = chk then
    begin
      DoCheckBoxClick(chk);
      chk := -1;
    end;
  end;
end;

procedure TVisualListBox.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
  FOwner.HandleShortCut(Msg);
end;

procedure TVisualListBox.CreateWnd;
var
  VerInfo: TOSVersioninfo;
  FIsWinXP,FIsComCtl6: boolean;
  i: integer;
begin
  inherited;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);
  FIsThemed := FIsComCtl6 and FIsWinXP;
end;

procedure TVisualListBox.DoCheckBoxClick(Index: integer);
begin
  if Assigned(OnCheckBoxClick) then
    OnCheckBoxClick(Self, Index);
end;

procedure TVisualListBox.DrawCheck(R: TRect; State,Enabled,Grayed: Boolean; ControlStyle: TControlStyle);
var
  DrawState: Integer;
  DrawRect: TRect;
  HTheme: THandle;

begin
  r := Rect(R.Left, R.Top, R.Left + CHECKBOXSIZE, R.Top + CHECKBOXSIZE);

  case ControlStyle of
  csClassic:
    begin
      if State then
        DrawState := DFCS_BUTTONCHECK or DFCS_CHECKED
      else
        DrawState := DFCS_BUTTONCHECK;

      if Grayed then
        DrawState := DrawState or DFCS_BUTTON3STATE or DFCS_CHECKED;

      if not Enabled then
        DrawState := DrawState or DFCS_INACTIVE;

      DrawRect.Left := R.Left + (R.Right - R.Left - CHECKBOXSIZE) div 2;
      DrawRect.Top:= R.Top + (R.Bottom - R.Top - CHECKBOXSIZE) div 2;
      DrawRect.Right := DrawRect.Left + CHECKBOXSIZE;
      DrawRect.Bottom := DrawRect.Top + CHECKBOXSIZE;

      DrawFrameControl(Canvas.Handle,DrawRect,DFC_BUTTON,DrawState);
    end;
  csThemed:
    begin
      begin
        HTheme := OpenThemeData(Self.Handle,'button');

       if Grayed then
        begin
          if Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,r,nil);
        end
        else
        begin
          if State then
          begin
            if Enabled then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,r,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,r,nil);
          end
          else
          begin
            if Enabled then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,r,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,r,nil);
          end;
        end;

        CloseThemeData(HTheme);
      end;
    end;

  end;
end;


{ TIntList }

constructor TIntList.Create;
begin
  inherited Create;
end;

procedure TIntList.SetInteger(Index:Integer;Value:Integer);
begin
  inherited Items[Index] := Pointer(Value);
  DoChange;
end;

function TIntList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntList.DeleteValue(Value: Integer);
var
  i: integer;
begin
  i := IndexOf(Pointer(Value));

  if i <> -1 then
    Delete(i);
end;

procedure TIntList.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TIntList.HasValue(Value: Integer): Boolean;
begin
  Result := IndexOf(Pointer(Value)) <> -1;
end;


procedure TIntList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));

  DoChange;
end;

procedure TIntList.Delete(Index: Integer);
begin
  inherited Delete(Index);

  DoChange;
end;

function TIntList.GetStrValue: string;
var
  i: integer;
begin
  for i := 1 to Count do
    if i = 1 then
      Result:= IntToStr(Items[i - 1])
    else
      Result := Result + ',' + IntToStr(Items[i - 1]);
end;

procedure TIntList.SetStrValue(const Value: string);
var
  sl:TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.CommaText := Value;
  Clear;
  for i := 1 to sl.Count do
   Add(StrToInt(sl.Strings[i - 1]));
  sl.Free;
end;

procedure TIntList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;

{ TSearchOptions }

procedure TSearchOptions.AlignmentChanged;
begin
  if Assigned(OnAlignmentChange) then
    OnAlignmentChange(Self);
end;

procedure TSearchOptions.Assign(Source: TPersistent);
begin
  if (Source is TSearchOptions) then
  begin
    FStyle := (Source as TSearchOptions).Style;
    FShowClose := (Source as TSearchOptions).ShowClose;
    FShowFind := (Source as TSearchOptions).ShowFind;
    FAutoFind := (Source as TSearchOptions).AutoFind;
    FFilterMethod := (Source as TSearchOptions).FilterMethod;
    FEditColor := (Source as TSearchOptions).EditColor;
    FHintFind := (Source as TSearchOptions).HintFind;
    FHintClose := (Source as TSearchOptions).HintClose;
    FCaseSensitive := (Source as TSearchOptions).CaseSensitive;
    FPosition := (Source as TSearchOptions).Position;
    FTextHint := (Source as TSearchOptions).TextHint;
    FFlat := (Source as TSearchOptions).Flat;
  end;
end;

procedure TSearchOptions.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TSearchOptions.Create;
begin
  inherited;
  FStyle := ssFilter;
  FVisible := true;
  FEditColor := clWindow;
  FPosition := ipTop;
  FFlat := true;
  FShowFind := true;
end;

procedure TSearchOptions.SetEditColor(const Value: TColor);
begin
  if (FEditColor <> Value) then
  begin
    FEditColor := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetFilterMethod(const Value: TFilterMethod);
begin
  FFilterMethod := Value;
end;

procedure TSearchOptions.SetFlat(const Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetHintClose(const Value: string);
begin
  if (FHintClose <> Value) then
  begin
    FHintClose := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetHintFind(const Value: string);
begin
  if (FHintFind <> Value) then
  begin
    FHintFind := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetPosition(const Value: TInputPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    AlignmentChanged;
  end;
end;

procedure TSearchOptions.SetShowClose(const Value: boolean);
begin
  if (FShowClose <> Value) then
  begin
    FShowClose := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetShowFind(const Value: boolean);
begin
  if (FShowFind <> Value) then
  begin
    FShowFind := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetStyle(const Value: TSearchStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetTextHint(const Value: string);
begin
  if (FTextHint <> Value) then
  begin
    FTextHint := Value;
    Changed;
  end;
end;

procedure TSearchOptions.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TInsertOptions }

procedure TInsertOptions.AlignmentChanged;
begin
  if Assigned(OnAlignmentChange) then
    OnAlignmentChange(Self);
end;

procedure TInsertOptions.Assign(Source: TPersistent);
begin
  if (Source is TInsertOptions) then
  begin
    FVisible := (Source as TInsertOptions).Visible;
    FShowClose := (Source as TInsertOptions).ShowClose;
    FInsertAndClear := (Source as TInsertOptions).InsertAndClear;
    FAutoInsert := (Source as TInsertOptions).AutoInsert;
    FEditColor := (Source as TInsertOptions).EditColor;
    FHintClose := (Source as TInsertOptions).HintClose;
    FHintInsert := (Source as TInsertOptions).HintInsert;
    FPosition := (Source as TInsertOptions).Position;
    FTextHint := (Source as TInsertOptions).TextHint;
    FFlat := (Source as TInsertOptions).Flat;
  end;
end;

procedure TInsertOptions.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TInsertOptions.Create;
begin
  inherited;
  FVisible := false;
  FEditColor := clWindow;
  FPosition := ipTop;
  FFlat := true;
end;

procedure TInsertOptions.SetEditColor(const Value: TColor);
begin
  if (FEditColor <> Value) then
  begin
    FEditColor := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetFlat(const Value: boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetHintClose(const Value: string);
begin
  if (FHintClose <> Value) then
  begin
    FHintClose := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetHintInsert(const Value: string);
begin
  if (FHintInsert <> Value) then
  begin
    FHintInsert := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetPosition(const Value: TInputPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    AlignmentChanged;
  end;
end;

procedure TInsertOptions.SetShowClose(const Value: boolean);
begin
  if (FShowClose <> Value) then
  begin
    FShowClose := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetTextHint(const Value: string);
begin
  if (FTextHint <> Value) then
  begin
    FTextHint := Value;
    Changed;
  end;
end;

procedure TInsertOptions.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

end.
