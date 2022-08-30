{ **************************************************************************}
{ TAdvMultiColumnDropdown components                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvMultiColumnDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ImgList, Grids, Forms,
  SysUtils, AdvDropDown, AdvUtil, AdvStyleIF, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TAdvMultiColumnDropDown = class;

  TColumnType = (ctText, ctImage);

  TGetCellTextEvent = procedure(Sender: TObject; Column, Row: Integer;
    var Text: string) of object;
  TDrawCellEvent = procedure(Sender: TObject; Column, Row: Integer;
    ACanvas: TCanvas; ARect: TRect) of object;
  TGetCellPropEvent = procedure(Sender: TObject; Column, Row: Integer;
    Font: TFont; var AColor, AColorTo: TColor) of object;

  TDropDownColumn = class(TCollectionItem)
  private
    FWordwrap: Boolean;
    FWidth: Integer;
    FAutoSize: Boolean;
    FHeader: string;
    FAlignment: TAlignment;
    FColor: TColor;
    FColumnType: TColumnType;
    FFont: TFont;
    FEllipsis: boolean;
    procedure SetFont(const Value: TFont);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Width: Integer read FWidth write FWidth default 80;
    property Color: TColor read FColor write FColor default clWhite;
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property Ellipsis: boolean read FEllipsis write FEllipsis default False;
    property Font: TFont read FFont write SetFont;
    property ColumnType: TColumnType read FColumnType write FColumnType default ctText;
    // for ctImage, ImageIndex is used
    property Header: string read FHeader write FHeader; // (column header text)
    property Wordwrap: Boolean read FWordwrap write FWordwrap default False;
    // (allow wordwrap)
    property AutoSize: Boolean read FAutoSize write FAutoSize default False;
    // column width adapts to size of text
  end;

  TDropDownColumns = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TDropDownColumn;
    procedure SetItem(Index: Integer; const Value: TDropDownColumn);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TDropDownColumn read GetItem write SetItem; default;
    function Add: TDropDownColumn;
    function Insert(Index: Integer): TDropDownColumn;
    function GetOwner: TPersistent; override;
{$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

  TDropDownItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FText: TStringList;
    FTag: integer;
    procedure SetText(const Value: TStringList);
  protected
    // property Enabled: Boolean read FEnabled write FEnabled default True;  // sgg:
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: Integer read FImageIndex write FImageIndex default - 1;
    property Text: TStringList read FText write SetText;
    property Tag: integer read FTag write FTag;
  end;

  TDropDownItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TDropDownItem;
    procedure SetItem(Index: Integer; const Value: TDropDownItem);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TDropDownItem read GetItem write SetItem; default;
    function Add: TDropDownItem;
    function Insert(Index: Integer): TDropDownItem;
    function GetOwner: TPersistent; override;
    function IndexInColumn(Column: integer; Value: string): integer;
{$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvColGrid = class(TStringGrid)
  private
    FAdvDropDown: TAdvMultiColumnDropDown;
    FColumns: TDropDownColumns;
    FHeaderColor: TColor;
    FHeaderColorTo: TColor;
    FLineColor: TColor;
    FImages: TCustomImageList;
    FOnDrawCell: TDrawCellEvent;
    FOnGetCellProp: TGetCellPropEvent;
    FOnGetCellText: TGetCellTextEvent;
    FHeaderHeight: Integer;
    FHeaderFont: TFont;
    FFixedLineColor: TColor;
    FGridLineWidth: Integer;
    FOnSelect: TNotifyEvent;
    FOldRow: Integer;
    FMouseWheelSelection: Boolean;
    FConfirmSelection: Boolean;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure SetHeaderColor(const Value: TColor);
    procedure SetHeaderColorTo(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetColumns(const Value: TDropDownColumns);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetAdvDropDown(const Value: TAdvMultiColumnDropDown);
    function CellRect(c, r: Integer): TRect;
    procedure SetGridLineWidth(const Value: Integer);
  protected
    procedure DoExit; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;  AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SelectionChanged; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure AutoSizeColumn(ACol: Integer);
    procedure AutoSizeColumns;
    procedure DoWordWrap;
    procedure Initialize(DropDown: TAdvMultiColumnDropDown);
    function GetDesiredWidth: Integer;
    property AdvDropDown: TAdvMultiColumnDropDown read FAdvDropDown write SetAdvDropDown;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Columns: TDropDownColumns read FColumns write SetColumns;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    // (color of lines between columns, clNone = no line)
    property FixedLineColor: TColor read FFixedLineColor write FFixedLineColor;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor;
    // gradient start color of header
    property HeaderColorTo: TColor read FHeaderColorTo write SetHeaderColorTo;
    // gradient end color of header (when clNone, solid color is used)
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property Images: TCustomImageList read FImages write SetImages;

    property OnGetCellText: TGetCellTextEvent read FOnGetCellText write FOnGetCellText;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnGetCellProp: TGetCellPropEvent read FOnGetCellProp write FOnGetCellProp;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMultiColumnDropDown = class(TAdvCustomDropDown)
  private
    FShowing: Boolean;
    FColumnSizing: Boolean;
    FLookupColumn: Integer;
    FHeaderColorTo: TColor;
    FHeaderColor: TColor;
    FLineColor: TColor;
    FItems: TDropDownItems;
    FColumns: TDropDownColumns;
    FItemIndex: Integer;
    FHeaderHeight: Integer;
    FHeaderFont: TFont;
    FAdvColGrid: TAdvColGrid;
    FFixedLineColor: TColor;
    workmode: Boolean;
    FLookupStr: string;
    FOldValue: string;
    FReturnIsTab: Boolean;
    FLookupItems: TStringList;
    FMatchCase: Boolean;
    FMatchStart: Boolean;
    FItemChange: Boolean;
    FItemIdx: Integer;
    FItemSel: Integer;
    FOnSelect: TNotifyEvent;
    FOnDropDownDrawItem: TDrawCellEvent;
    FOnDropDownGetItemProp: TGetCellPropEvent;
    FOnDropDownGetItemText: TGetCellTextEvent;
    FOldItemIndex: Integer;
    FDropDownAutoWidth: Boolean;
    FColumnSizeWithDropDown: Boolean;
    FSelectionTextColor: TColor;
    FDropDownRowHeight: Integer;
    FLookupEntry: string;
    FCaseSensitive: Boolean;
    FOrgIndex: integer;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure OnColGridSelectCell(Sender: TObject; ACol, ARow: Longint;
      var CanSelect: Boolean);
    procedure OnColGridSelect(Sender: TObject);
    procedure OnColGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    function GetRowCount: Integer;
    procedure SetLookupColumn(const Value: Integer);
    procedure SetItems(const Value: TDropDownItems);
    procedure SetColumns(const Value: TDropDownColumns);
    function GetTextEx: string;
    procedure SetItemIndex(const Value: Integer);
    procedure SetTextEx(const Value: string);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure UpdateLookupList;
    procedure SyncColWidths;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Change; override;

    procedure CreateDropDownForm; override;
    procedure AdaptDropDownSize(var AHeight: Integer); override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure DoShowDropDown; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure SetCenterControl; override;
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: Char); override;
    procedure OnDropDownSizing; override;
    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;

    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure SelectNextPage;
    procedure SelectPrevPage;
    function LookupInColumn(value: string; var lookup: string; var row: integer): boolean;
    function DoKeyLookup(ch: Char): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property RowCount: Integer read GetRowCount;
    // number of rows in the dropdown
    property Text: string read GetTextEx write SetTextEx;
    // holds control.Items[ItemIndex].Text[LookupColumn]
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab;
    property MatchCase: Boolean read FMatchCase write FMatchCase default False;
    property MatchStart: Boolean read FMatchStart write FMatchStart default true;
    function GetItemIndex(value: string): integer;
  published
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default false;
    property DropDownAutoWidth: Boolean read FDropDownAutoWidth write FDropDownAutoWidth default true;
    property DropDownRowHeight: Integer read FDropDownRowHeight write FDropDownRowHeight default 22;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    // (color of lines between columns, clNone = no line)
    property FixedLineColor: TColor read FFixedLineColor write FFixedLineColor;
    property ColumnSizing: Boolean read FColumnSizing write FColumnSizing default False;
    // when true, columns are sizeable at runtime
    property ColumnSizeWithDropDown: Boolean read FColumnSizeWithDropDown write
      FColumnSizeWithDropDown default true;
    property Columns: TDropDownColumns read FColumns write SetColumns;
    property HeaderColor: TColor read FHeaderColor write FHeaderColor;
    // gradient start color of header
    property HeaderColorTo: TColor read FHeaderColorTo write FHeaderColorTo;
    // gradient end color of header (when clNone, solid color is used)
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property LookupColumn: Integer read FLookupColumn write SetLookupColumn default 0; // column used to show text of selected item in edit control and column in which lookup while typing is performed, ie. type 'M' , 'e' , 'r' and the item in LookupColumn in the dropdown will become selected.
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor default clBlack;
    property Items: TDropDownItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;

    property Align;
    {$IFDEF DELPHIXE_LVL}
    property Alignment;
    {$ENDIF}
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property DisabledBorder;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownSizeable;
    property EditorEnabled;
    property Enabled;
    property Font;
    property DropDownButtonGlyph;
    property Images;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property ReadOnly;
    property SelectionColor;
    property SelectionColorTo;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
{$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;

    property OnDropDownGetItemText: TGetCellTextEvent read FOnDropDownGetItemText write
      FOnDropDownGetItemText;
    property OnDropDownDrawItem: TDrawCellEvent read FOnDropDownDrawItem write
      FOnDropDownDrawItem;
    property OnDropDownGetItemProp: TGetCellPropEvent read FOnDropDownGetItemProp write
      FOnDropDownGetItemProp;
  end;

implementation

uses
  StdCtrls;

// ------------------------------------------------------------------------------

function upstr(s: string; docase: Boolean): string;
begin
  if docase then
    Result := s
  else
    Result := AnsiUpperCase(s);
end;

// ----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor;
  Steps: Integer; r: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    r.Right := r.Right - 1
  else
    r.Bottom := r.Bottom - 1;

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

  rstepr := diffr / Steps;
  rstepg := diffg / Steps;
  rstepb := diffb / Steps;

  if Direction then
    rstepw := (r.Right - r.Left) / Steps
  else
    rstepw := (r.Bottom - r.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(r.Left + stepw, r.Top, r.Left + stepw + Round(rstepw) + 1,
          r.Bottom)
      else
        Rectangle(r.Left, r.Top + stepw, r.Right, r.Top + stepw + Round(rstepw)
            + 1);
    end;
  end;
end;

// ------------------------------------------------------------------------------

{ TDropDownColumn }

procedure TDropDownColumn.Assign(Source: TPersistent);
begin
  if (Source is TDropDownColumn) then
  begin
    FWordwrap := (Source as TDropDownColumn).Wordwrap;
    FWidth := (Source as TDropDownColumn).Width;
    FHeader := (Source as TDropDownColumn).Header;
    FAlignment := (Source as TDropDownColumn).Alignment;
    FColor := (Source as TDropDownColumn).Color;
    FColumnType := (Source as TDropDownColumn).ColumnType;
    FFont.Assign((Source as TDropDownColumn).Font);
    FEllipsis := (Source as TDropDownColumn).Ellipsis;
    AutoSize := (Source as TDropDownColumn).AutoSize;
  end
  else
    inherited Assign(Source);
end;

// ------------------------------------------------------------------------------

constructor TDropDownColumn.Create(Collection: TCollection);
begin
  inherited;
  FWordwrap := False;
  FWidth := 80;
  FAutoSize := False;
  FHeader := '';
  FAlignment := taLeftJustify;
  FColor := clWhite;
  FColumnType := ctText;
  FFont := TFont.Create;
end;

// ------------------------------------------------------------------------------

destructor TDropDownColumn.Destroy;
begin
  FFont.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TDropDownColumn.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

// ------------------------------------------------------------------------------

{ TDropDownColumns }

function TDropDownColumns.Add: TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Add);
end;

// ------------------------------------------------------------------------------

constructor TDropDownColumns.Create(AOwner: TPersistent);
begin
  inherited Create(TDropDownColumn);
  FMyOwner := AOwner;
end;

// ------------------------------------------------------------------------------

function TDropDownColumns.GetItem(Index: Integer): TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Items[Index]);
end;

// ------------------------------------------------------------------------------

function TDropDownColumns.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

// ------------------------------------------------------------------------------

function TDropDownColumns.Insert(Index: Integer): TDropDownColumn;
begin
  Result := TDropDownColumn( inherited Insert(Index));
end;

// ------------------------------------------------------------------------------

procedure TDropDownColumns.SetItem(Index: Integer;
  const Value: TDropDownColumn);
begin
  inherited Items[Index] := Value;
end;

// ------------------------------------------------------------------------------

{ TAdvMultiColumnDropDown }

constructor TAdvMultiColumnDropDown.Create(AOwner: TComponent);
begin
  FHeaderFont := TFont.Create;

  inherited;

  FColumnSizing := False;
  FLookupColumn := 0;
  FHeaderColorTo := clGray;
  FHeaderColor := clWhite;
  FHeaderHeight := 25;
  FLineColor := clSilver;
  FItems := TDropDownItems.Create(Self);
  FColumns := TDropDownColumns.Create(Self);
  DropDownColor := clWhite;
  DropDownHeight := 200;
  FItemIndex := -1;
  FOldItemIndex := FItemIndex;

  FLookupItems := TStringList.Create;
  workmode := true;
  FLookupStr := '';
  FMatchCase := False;
  FMatchStart := true;
  AutoSelect := true;
  FItemChange := False;
  FDropDownAutoWidth := true;
  SelectionColorStyle := scOffice2007;
  FSelectionTextColor := clBlack;
  FColumnSizeWithDropDown := true;
  FDropDownRowHeight := 22;
end;

// ------------------------------------------------------------------------------

destructor TAdvMultiColumnDropDown.Destroy;
begin
  FItems.Free;
  FColumns.Free;
  FHeaderFont.Free;
  FLookupItems.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

function TAdvMultiColumnDropDown.GetItemIndex(value: string): integer;
var
  i:integer;

begin
  Result := -1;

  for i := 0 to Items.Count - 1 do
  begin
    if (value = Items[i].Text.Strings[FLookupColumn]) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvMultiColumnDropDown.GetRowCount: Integer;
begin
  Result := Items.Count;
end;

// ------------------------------------------------------------------------------

function TAdvMultiColumnDropDown.GetTextEx: string;
begin
  Result := inherited Text;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.HandleMouseWheelDown;
begin
  inherited;
  SelectNext;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.HandleMouseWheelUp;
begin
  inherited;
  SelectPrevious;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.CreateDropDownForm;
begin
  inherited;

  if not Assigned(FAdvColGrid) then
  begin
    FAdvColGrid := TAdvColGrid.Create(Self);
    FAdvColGrid.Parent := FDropDownForm;
  end;

  FAdvColGrid.AdvDropDown := Self;
  FAdvColGrid.Left := 0;
  FAdvColGrid.Top := 0;
  FAdvColGrid.Height := 180;
  FAdvColGrid.Color := DropDownColor;
  FAdvColGrid.DoubleBuffered := true;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if (Source is TAdvMultiColumnDropDown) then
  begin
    FCaseSensitive := (Source as TAdvMultiColumnDropDown).CaseSensitive;
    FDropDownAutoWidth := (Source as TAdvMultiColumnDropDown).DropDownAutoWidth;
    FDropDownRowHeight := (Source as TAdvMultiColumnDropDown).DropDownRowHeight;
    FLineColor  := (Source as TAdvMultiColumnDropDown).LineColor;
    FFixedLineColor := (Source as TAdvMultiColumnDropDown).FixedLineColor;
    FColumnSizing := (Source as TAdvMultiColumnDropDown).ColumnSizing;
    FColumnSizeWithDropDown := (Source as TAdvMultiColumnDropDown).ColumnSizeWithDropDown;
    FColumns.Assign((Source as TAdvMultiColumnDropDown).Columns);
    FHeaderColor  := (Source as TAdvMultiColumnDropDown).HeaderColor;
    FHeaderColorTo  := (Source as TAdvMultiColumnDropDown).HeaderColorTo;
    FHeaderHeight  := (Source as TAdvMultiColumnDropDown).HeaderHeight;
    FHeaderFont.Assign((Source as TAdvMultiColumnDropDown).Font);
    FLookupColumn := (Source as TAdvMultiColumnDropDown).LookupColumn;
    FSelectionTextColor := (Source as TAdvMultiColumnDropDown).SelectionTextColor;
    FItems.Assign((Source as TAdvMultiColumnDropDown).Items);
    FItemIndex := (Source as TAdvMultiColumnDropDown).ItemIndex;
  end;
end;

procedure TAdvMultiColumnDropDown.AdaptDropDownSize(var AHeight: integer);
var
  h,hdelta: integer;

begin
  if DropDownSizeable then
    Exit;

  h := AHeight;
  hdelta := 0;

  if DropDownHeader.Visible then
  begin
    h := h - DropDownHeader.Height;
    hdelta := DropDownHeader.Height;
  end;

  if DropDownFooter.Visible then
  begin
    h := h - DropDownFooter.Height;
    hdelta := hdelta + DropDownFooter.Height;
  end;

  h := h - HeaderHeight - 2;
  hdelta := hdelta + HeaderHeight + 2;

  h := h div (DropDownRowHeight );

  // h = nr. of items that can be displayed , round to integral number here
  AHeight := hdelta + h * (DropDownRowHeight );
end;

procedure TAdvMultiColumnDropDown.BeforeDropDown;
begin
  inherited;

  if Assigned(FDropDownForm) then
    FDropDownForm.CancelOnDeActivate := False;

  FShowing := true;

  if Assigned(FAdvColGrid) then
  begin
    FAdvColGrid.Initialize(Self);
    FAdvColGrid.Options := FAdvColGrid.Options + [goThumbTracking];
    FAdvColGrid.Color := DropDownColor;
    FAdvColGrid.OnSelectCell := OnColGridSelectCell;
    FAdvColGrid.OnSelect := OnColGridSelect;
    FAdvColGrid.OnKeyDown := OnColGridKeyDown;
    if ItemIndex + 1 >= FAdvColGrid.FixedRows then
      FAdvColGrid.Row := ItemIndex + 1
    else
      FAdvColGrid.Row := 1;
  end;

  FOldItemIndex := ItemIndex;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnHideDropDown;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.DoHideDropDown(Canceled: Boolean);
begin
  if not FDropDownForm.Visible then
  begin
    inherited;
    Exit;
  end;

  inherited;

  if not Canceled then
  begin
    if Assigned(FAdvColGrid) and FAdvColGrid.FConfirmSelection then
      ItemIndex := FAdvColGrid.Row - 1;
    FOldItemIndex := ItemIndex;
  end
  else
  begin
    if (ItemIndex <> FOldItemIndex) then
      ItemIndex := FOldItemIndex;
  end;
end;

procedure TAdvMultiColumnDropDown.DoShowDropDown;
var
  R: TRect;
begin
  if not Enabled or ReadOnly then
    Exit;

  FOrgIndex := ItemIndex;

  inherited;

  SyncColWidths;

  if (FAdvColGrid.RowCount > 1) then
  begin
    if ItemIndex + 1 > 0 then
      FAdvColGrid.Row := ItemIndex + 1
    else
      FAdvColGrid.Row := 1;
  end;

  R := FAdvColGrid.CellRect(FAdvColGrid.FixedCols, FAdvColGrid.Row);

  if (R.Left <= 0) and (R.Top <= 0) then
  begin
    FAdvColGrid.TopRow := FAdvColGrid.Row - FAdvColGrid.VisibleRowCount + 1;
  end;

  FShowing := false;
end;

procedure TAdvMultiColumnDropDown.EndUpdate;
begin
  UpdateLookupList;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectFirst;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := 0;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectLast;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := Items.Count - 1;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectNext;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 1) < Items.Count) then
      ItemIndex := ItemIndex + 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectNextPage;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 10) < Items.Count) then
      ItemIndex := ItemIndex + 10
    else
      ItemIndex := Items.Count - 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectPrevious;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 1) >= 0) then
      ItemIndex := ItemIndex - 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SelectPrevPage;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 10) >= 0) then
      ItemIndex := ItemIndex - 10
    else
      ItemIndex := 0;
  end;
end;

function TAdvMultiColumnDropDown.LookupInColumn(value: string; var lookup: string;
  var row: integer): boolean;
var
  i: integer;
  s: string;
begin
  Result := false;

  Row := -1;
  lookup := '';

  if not FCaseSensitive then
    value := AnsiUppercase(value);

  if Assigned(FAdvColGrid) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      s := Items[i].Text[FlookupColumn];
      if not FCaseSensitive then
        s := AnsiUpperCase(s);

      if pos(value, s) = 1 then
      begin
        lookup := Items[i].Text[FLookupColumn];
        row := i;
        Result := true;
        break;
      end;
    end;
  end;
end;

function TAdvMultiColumnDropDown.DoKeyLookup(ch: Char): integer;
var
  s,res: string;
  row: integer;
begin
  Result := -1;

  if (ch <> #8) and (ch <> #9) then
  begin
    FLookupEntry := FLookupEntry + ch;
  end
  else
  begin
    if (ch = #8) then
    begin
      res := Text;
      if (SelLength > 0) then
      begin
        Delete(res, SelStart, SelLength);
        Text := res;
        SelStart := Length(Text);
      end
      else
      begin
        Delete(FLookupEntry, length(FLookupEntry) , 1);
        Text := FLookupEntry;
        SelStart  := Length(FLookupEntry);
      end;
      Exit;
    end;
  end;

  s := FLookupEntry;

  if LookupInColumn(s, res, row) then
  begin
    FItemIndex := row;
    Text := res;
    Result := row;
    SelStart := Length(FLookupEntry);
    SelLength := Length(res) - Length(FLookupEntry);
  end
  else
  begin
    s := ch;
    if LookupInColumn(s, res, row) then
    begin
      FItemIndex := row;
      Text := res;
      Result := row;
      FLookupEntry := s
    end
    else
      FLookupEntry := '';
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetCenterControl;
begin
  Control := FAdvColGrid;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetColumns(const Value: TDropDownColumns);
begin
  FColumns.Assign(Value);
end;

procedure TAdvMultiColumnDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  case AStyle of
    tsOffice2003Blue:
      begin
        HeaderColor := $D68759;
        HeaderColorTo := $933803;
        HeaderFont.Color := clWhite;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $962D00;
        FixedLineColor := $962D00;
      end;
    tsOffice2003Silver:
      begin
        HeaderColor := $BDA4A5;
        HeaderColorTo := $957475;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $947C7C;
        FixedLineColor := $947C7C;
      end;
    tsOffice2003Olive:
      begin
        HeaderColor := $82C0AF;
        HeaderColorTo := $447A63;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $588060;
        FixedLineColor := $588060;
      end;
    tsOffice2003Classic:
      begin
        HeaderColor := $808080;
        HeaderColorTo := $808080;
        SelectionColorStyle := scOffice2007;
        SelectionTextColor := clBlack;
        LineColor := $808080;
        FixedLineColor := $808080;
      end;
    tsOffice2007Luna:
      begin
        HeaderColor := $FFEFE3;
        HeaderColorTo := $FFD2AF;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $723708;
        SelectionTextColor := clBlack;
        LineColor := $00FFD2AF;
        FixedLineColor := $00FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        HeaderColor := $F2F1F0;
        HeaderColorTo := $C9C2BD;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $433C37;
        SelectionTextColor := clBlack;
        LineColor := $5C534C;
        FixedLineColor := $5C534C;
      end;
    tsWindowsXP:
      begin
        FixedLineColor := clBlack;
        LineColor := clSilver;
        HeaderColor := clBtnFace;
        HeaderColorTo := clBtnFace;
        SelectionColorStyle := scCustom;
        SelectionColor := clHighLight;
        SelectionColorTo := clHighLight;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clHighlightText;
      end;
    tsWhidbey:
      begin
        HeaderColor := $EBEEEF;
        HeaderColorTo := $7E9898;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := clWhite;
        SelectionTextColor := clBlack;
        FixedLineColor := $962D00;
        LineColor := $962D00;
      end;
    tsOffice2007Silver:
      begin
        HeaderColor := $F8F7F6;
        HeaderColorTo := $E8E0DB;
        SelectionColorStyle := scOffice2007;
        HeaderFont.Color := $8B4215;
        SelectionTextColor := clBlack;
        FixedLineColor := $74706F;
        LineColor := $74706F;
      end;
    tsWindowsVista:
      begin
        HeaderColor := $FFFDF9;
        HeaderColorTo := $FFFAF0;
        SelectionColorStyle := scWindowsVista;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
        FixedLineColor := $FCF2DA;
        LineColor := $FCF2DA;
      end;
    tsWindows7:
      begin
        HeaderColor := $FDFBFA;
        HeaderColorTo := $FDF3EB;
        SelectionColorStyle := scWindows7;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
        FixedLineColor := $FBD6B8;
        LineColor := $FBD6B8;
      end;
    tsTerminal:
      begin
        FixedLineColor := clGray;
        LineColor := clGray;
        HeaderColor := clBtnFace;
        HeaderColorTo := clBtnFace;
        SelectionColorStyle := scCustom;
        SelectionColor := clHighLight;
        SelectionColorTo := clHighLight;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clHighLightText;
      end;
      tsOffice2010Blue:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $EDDBCD;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $5B391E;
        SelectionTextColor := clBlack;
      end;
        tsOffice2010Silver:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $EDE9E5;
        HeaderColorTo := clNone;
        SelectionColorStyle :=  scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $5B391E;
        SelectionTextColor := clBlack;
      end;
        tsOffice2010Black:
      begin
        FixedLineColor := $EEDDCF;
        LineColor := $EEDDCF;
        HeaderColor := $828282;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $6CD0FF;
        SelectionColorTo := $6CD0FF;
        HeaderFont.Color := $D7D7D6;
        SelectionTextColor := clBlack;
      end;
      tsWindows8, tsWindows10:
      begin
        FixedLineColor := $E4E3E2;
        LineColor := $E4E3E2;
        HeaderColor := $F7F6F5;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $F7E0C9;
        SelectionColorTo := $F7E0C9;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013White:
      begin
        FixedLineColor := $D4D4D4;
        LineColor := $D4D4D4;
        HeaderColor := clWhite;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013LightGray:
      begin
        FixedLineColor := $C6C6C6;
        LineColor := $C6C6C6;
        HeaderColor := $F6F6F6;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2013Gray:
      begin
        FixedLineColor := $ABABAB;
        LineColor := $ABABAB;
        HeaderColor := $E5E5E5;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $FCE2C8;
        SelectionColorTo := $FCE2C8;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016White:
      begin
        FixedLineColor := $D4D4D4;
        LineColor := $D4D4D4;
        HeaderColor := clWhite;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $E3BDA3;
        SelectionColorTo := $E3BDA3;
        HeaderFont.Color := clBlack;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016Gray:
      begin
        FixedLineColor := $444444;
        LineColor := $444444;
        HeaderColor := $444444;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $B2B2B2;
        SelectionColorTo := $B2B2B2;
        HeaderFont.Color := $F0F0F0;
        SelectionTextColor := clBlack;
      end;
      tsOffice2016Black:
      begin
        FixedLineColor := $6A6A6A;
        LineColor := $6A6A6A;
        HeaderColor := $444444;
        HeaderColorTo := clNone;
        SelectionColorStyle := scCustom;
        SelectionColor := $444444;
        SelectionColorTo := $444444;
        HeaderFont.Color := clWhite;
        SelectionTextColor := clWhite;
      end;

  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetItemIndex(const Value: Integer);
begin
  if (Value < FItems.Count) then
  begin
    FItemIndex := Value;
    if (FItemIndex >= 0) and (FItemIndex < FItems.Count) and
      (FLookupColumn >= 0) then
    begin
      if (FLookupColumn < Items[FItemIndex].Text.Count) then
        Text := Items[FItemIndex].Text[FLookupColumn]
      else
        Text := '';
    end
    else
      Text := '';
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetItems(const Value: TDropDownItems);
begin
  FItems.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetLookupColumn(const Value: Integer);
begin
  FLookupItems.Clear;
  FLookupColumn := Value;
  UpdateLookupList;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetSelectionColorStyle(const Value: TSelectionColorStyle);
begin
  case Value of
    scOffice2007:
      begin
        LineColor := $F1EDEB;
        FixedLineColor := $D1BBA4;
        SelectionColor := $5EC1F1;
      end;
    scWindowsVista:
      begin
        SelectionColor := $00EACAB6;
        // SelectionTextColor := clBlack;
        LineColor := $ECECF0;
        FixedLineColor := $D4D2D1;
      end;
    scWindows7:
      begin
        SelectionColor := $00EACAB6;
        // SelectionTextColor := clBlack;
        LineColor := $ECECF0;
        FixedLineColor := $D4D2D1;
      end;
  end;

  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetTextEx(const Value: string);
begin
  inherited Text := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.UpdateDropDownSize;
var
  w: Integer;
begin
  inherited;
  if DropDownAutoWidth and Assigned(FDropDownForm) then
  begin
    w := FAdvColGrid.GetDesiredWidth;
    FAdvColGrid.Align := alNone;
    FAdvColGrid.Width := w;
    FDropDownForm.Width := FAdvColGrid.Width + DropDownBorderWidth * 2;
    FAdvColGrid.Align := alClient;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnColGridSelectCell
  (Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if Assigned(FAdvColGrid) then
  begin
    if not FAdvColGrid.FMouseWheelSelection and not FShowing then
    begin
      ItemIndex := ARow - 1;
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnDropDownControlKeyDown
  (var Key: Word; Shift: TShiftState);
begin
  if not(Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;

  if (Key = VK_TAB) then
  begin
    Key := 0;
    HideDropDown(false);
    SelectAll;
    Exit;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnDropDownControlKeyPress(var Key: char);
var
  IsAlt: Boolean;
  r: Integer;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  if (not(Integer(Key) in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_HOME, VK_END, VK_RETURN, VK_ESCAPE]) or IsAlt) and
     (not (goEditing in FAdvColGrid.Options)) then
  begin
    r := DoKeyLookup(key);

    if r <> -1 then
    begin
      if (r + FAdvColGrid.FixedRows > FAdvColGrid.TopRow + FAdvColGrid.VisibleRowCount) or
         (r + FAdvColGrid.FixedRows < FAdvColGrid.TopRow) then
        FAdvColGrid.TopRow := r + FAdvColGrid.FixedRows;

      FAdvColGrid.Row := r + FAdvColGrid.FixedRows;
      FAdvColGrid.Col := 0;
    end
    else
    begin
      //FAdvColGrid.TopRow := FAdvColGrid.FixedRows;
      //FAdvColGrid.Row := FAdvColGrid.FixedRows;
      FLookupEntry := '';
    end;
  end
  else
    FLookupEntry := '';
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnDropDownControlKeyUp
  (var Key: Word; Shift: TShiftState);
begin
  if not(Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;
end;

procedure TAdvMultiColumnDropDown.SyncColWidths;
var
  r, d: double;
  tw, aw: Integer;
  i: Integer;
begin
  if not FColumnSizeWithDropDown then
    Exit;

  FAdvColGrid.ScrollBars := ssVertical;
  if Columns.Count > 0 then
  begin
    tw := 1;
    for i := 0 to Columns.Count - 1 do
    begin
      tw := tw + Columns[i].Width;
    end;

    // calculate ratio to size
    if FAdvColGrid.VisibleRowCount < FAdvColGrid.RowCount - FAdvColGrid.FixedRows then
    begin
      aw := FDropDownForm.Width - GetSystemMetrics(SM_CXVSCROLL);
    end
    else
      aw := FDropDownForm.Width;

    r := aw / tw;

    d := 0;

    for i := 0 to FAdvColGrid.ColCount - 1 do
    begin
      d := d + (r * Columns[i].Width) - Round(r * Columns[i].Width);

      if (d > 1) then
      begin
        FAdvColGrid.ColWidths[i] := Round(r * Columns[i].Width) + 1;
        d := d - 1;
      end
      else
        FAdvColGrid.ColWidths[i] := Round(r * Columns[i].Width);
    end;
  end;
end;

procedure TAdvMultiColumnDropDown.OnDropDownSizing;
begin
  inherited;
  // size columns proportionally
  SyncColWidths;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnColGridSelect(Sender: TObject);
begin
  if Assigned(FAdvColGrid) then
  begin
    if FAdvColGrid.FConfirmSelection then
      ItemIndex := FAdvColGrid.Row - 1;

    DoHideDropDown(False);

    if Assigned(OnSelect) then
      OnSelect(Self);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (FLookupColumn < 0) or (Items.Count = 0) then
  begin
    inherited;
    Exit;
  end;

  case Key of
    VK_ESCAPE, VK_BACK, VK_DELETE:
      workmode := False;
    VK_RETURN:
      begin
        if not ReadOnly and (FLookupItems.IndexOf(Text) <> -1) then
        begin
          Text := FLookupItems.Strings[FLookupItems.IndexOf(Text)];
          ItemIndex := FLookupItems.IndexOf(Text);
          Change;
        end;
        if FReturnIsTab then
        begin
          PostMessage(Handle, WM_KEYDOWN, VK_TAB, 0);
          Key := 0;
        end;
      end;
  else
    workmode := true;
  end;

  inherited KeyDown(Key, Shift);

  if (Key = VK_ESCAPE) then
    ItemIndex := FOrgIndex; //FLookupItems.IndexOf(Text);
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.KeyPress(var Key: char);
begin
  if (Key = #13) and FReturnIsTab then
    Key := #0
  else
    inherited KeyPress(Key);
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.KeyUp(var Key: Word; Shift: TShiftState);
var
  c,c1,newt,usrstr: string;
  autoadd: string;
  i: integer;
begin
  if not((Key = VK_RETURN) and (FReturnIsTab)) then
    inherited KeyUp(Key, Shift);

  if (Key = VK_BACK) then
  begin
    inherited KeyUp(Key, Shift);
    Exit;
  end;

  if Text <> FOldValue then
    Modified := true
  else
    Modified := False;

  NewT := Copy(Text, 1, SelStart);
  c1 := upstr(Text, FCaseSensitive);
  c := Copy(c1, 1, SelStart);

  if (FLookupItems.Count > 0) then
  begin
    for i := 0 to FLookupItems.Count - 1 do
    begin
      if pos(c, upstr(FLookupItems.Strings[i], FCaseSensitive)) = 1 then
      begin
        UsrStr := Copy(Text, 1, length(c));
        AutoAdd := Copy(FLookupItems.Strings[i], length(c) + 1, 255);

        // if Assigned(FAutoComplete) then
        // FAutoComplete(self, UsrStr, AutoAdd, i);

        FItemIndex := i;
        FItemIdx := i;
        FItemSel := length(c);
        FItemChange := true;
        Text := UsrStr + AutoAdd;

        SendMessage(Handle, EM_SETSEL, length(c), length(Text));
        Exit;
      end;
    end;

    if (NewT <> '') then
    begin
      Text := NewT;
      SendMessage(Handle, EM_SETSEL, length(Text), length(NewT));
    end;
  end;

  if FItemChange then
  begin
    ItemIndex := FItemIdx;
    SendMessage(Handle, CB_SETEDITSEL, 0, makelong(FItemSel, length(Text)));
    FItemChange := False;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.UpdateLookupList;
var
  i: Integer;
begin
  if (FLookupColumn < 0) or (Items.Count = 0) then
    Exit;

  FLookupItems.Clear;
  for i := 0 to Items.Count - 1 do
  begin
    if (FLookupColumn < Items[i].Text.Count) then
      FLookupItems.Add(Items[i].Text[FLookupColumn])
    else
      FLookupItems.Add('');
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.WMKeyDown(var Msg: TWMKeydown);
var
  IsAlt: Boolean;
begin
  inherited;

  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);
  if Enabled and not IsAlt and not ReadOnly then
  begin
    case Msg.CharCode of
      VK_UP:
        begin
          if not ReadOnly then
            SelectPrevious;
        end;
      VK_DOWN:
        begin
          if not ReadOnly then
            SelectNext;
        end;
      VK_HOME:
        begin
          if not EditorEnabled then
            SelectFirst;
        end;
      VK_END:
        begin
          if not EditorEnabled then
            SelectLast;
        end;
      VK_PRIOR:
        begin
          if not ReadOnly then
            SelectPrevPage;
        end;
      VK_NEXT:
        begin
          if not ReadOnly then
            SelectNextPage;
        end;
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.Loaded;
begin
  inherited;
  UpdateLookupList;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.BeginUpdate;
begin
  //
end;

procedure TAdvMultiColumnDropDown.Change;
var
  c, c1, NewT: string;
  i: Integer;
  UsrStr, AutoAdd: string;
begin
  FItemChange := False;

  inherited Change;

  if csDesigning in ComponentState then
    Exit;

  if not workmode then
    Exit;

  Exit;

  if Text <> FOldValue then
    Modified := true
  else
    Modified := False;

  NewT := Copy(Text, 1, SelStart);
  c1 := upstr(Text, FCaseSensitive);
  c := Copy(c1, 1, SelStart);

  if (FLookupItems.Count > 0) then
  begin
    for i := 0 to FLookupItems.Count - 1 do
    begin
      if pos(c, upstr(FLookupItems.Strings[i], FCaseSensitive)) = 1 then
      begin
        UsrStr := Copy(Text, 1, length(c));
        AutoAdd := Copy(FLookupItems.Strings[i], length(c) + 1, 255);

        FItemIndex := i;
        FItemIdx := i;
        FItemSel := length(c);
        FItemChange := true;
        Text := UsrStr + AutoAdd;

        SendMessage(Handle, EM_SETSEL, length(c), length(Text));
        Exit;
      end;
    end;

    if (NewT <> '') then
    begin
      Text := NewT;
      SendMessage(Handle, EM_SETSEL, length(Text), length(NewT));
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvMultiColumnDropDown.OnColGridKeyDown
  (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    vk_escape:
      DoHideDropDown(true);
    vk_return:
      DoHideDropDown(False);
  end;
end;

// ------------------------------------------------------------------------------

{ TDropDownItem }

procedure TDropDownItem.Assign(Source: TPersistent);
begin
  if (Source is TDropDownItem) then
  begin
    FImageIndex := (Source as TDropDownItem).ImageIndex;
    FText.Assign((Source as TDropDownItem).Text);
    FTag :=  (Source as TDropDownItem).Tag;
  end;
end;

// ------------------------------------------------------------------------------

constructor TDropDownItem.Create(Collection: TCollection);
begin
  inherited;
  FText := TStringList.Create;
  FImageIndex := -1;
end;

// ------------------------------------------------------------------------------

destructor TDropDownItem.Destroy;
begin
  FText.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TDropDownItem.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
end;

// ------------------------------------------------------------------------------

{ TDropDownItems }

function TDropDownItems.Add: TDropDownItem;
begin
  Result := TDropDownItem( inherited Add);
end;

// ------------------------------------------------------------------------------

constructor TDropDownItems.Create(AOwner: TPersistent);
begin
  inherited Create(TDropDownItem);
  FMyOwner := AOwner;
end;

// ------------------------------------------------------------------------------

function TDropDownItems.GetItem(Index: Integer): TDropDownItem;
begin
  Result := TDropDownItem( inherited Items[Index]);
end;

// ------------------------------------------------------------------------------

function TDropDownItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

// ------------------------------------------------------------------------------

function TDropDownItems.IndexInColumn(Column: integer; Value: string): integer;
var
  i: integer;
  itm: TDropDownItem;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    itm := Items[i];
    if itm.FText.Count > Column then
    begin
      if itm.FText.Strings[Column] = Value then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------

function TDropDownItems.Insert(Index: Integer): TDropDownItem;
begin
  Result := TDropDownItem( inherited Insert(Index));
end;

// ------------------------------------------------------------------------------

procedure TDropDownItems.SetItem(Index: Integer; const Value: TDropDownItem);
begin
  inherited Items[Index] := Value;
end;

// ------------------------------------------------------------------------------

{ TAdvColGrid }

constructor TAdvColGrid.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TDropDownColumns.Create(Self);
  FHeaderColorTo := clGray;
  FHeaderColor := clWhite;
  FHeaderFont := TFont.Create;
  FLineColor := clGray;
  FixedRows := 1;
  FixedCols := 0;
  FHeaderHeight := 25;
  Options := Options + [goRowSelect, goHorzLine, goTabs] - [goDrawFocusSelected, goRangeSelect];
  DefaultDrawing := False;
  ParentCtl3D := False;
  Ctl3D := False;
  GridLineWidth := 0;
  BorderStyle := bsNone;
  FGridLineWidth := 1;
  FOldRow := Row;
end;

// ------------------------------------------------------------------------------

destructor TAdvColGrid.Destroy;
begin
  FColumns.Free;
  FHeaderFont.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.WMKeyDown(var Msg: TWMKeydown);
begin
  if (Msg.CharCode = VK_TAB) then
  begin
    inherited;
    // if Assigned(AdvDropDown) and IsWindowVisible(AdvDropDown.Handle) then
    // PostMessage(AdvDropDown.Handle, WM_KEYDOWN, VK_TAB, 0);
    Exit;
  end;

  if (Msg.CharCode = vk_escape) or (Msg.CharCode = VK_F4) or
    (((Msg.CharCode = VK_UP) or (Msg.CharCode = VK_DOWN)) and
      (GetKeyState(VK_MENU) and $8000 = $8000)) then
  begin
    // if (msg.Charcode = VK_ESCAPE) then
    // ParentEdit.CancelChanges;
    // PostMessage((Parent as TForm).Handle,WM_CLOSE,0,0);
  end;

  inherited;
end;

procedure TAdvColGrid.WMLButtonDown(var Msg: TWMLButtonDown);
var
  LastCellClicked: boolean;
  X,Y: integer;

begin
  MouseToCell(Msg.XPos,Msg.YPos,X,Y);
  LastCellClicked := (Y = TopRow + VisibleRowCount) or (X = LeftCol + VisibleColCount) or ((X = LeftCol) and (LeftCol > FixedCols));

  inherited;

  if (FGridState = gsSelecting) and LastCellClicked then
  begin
    FGridState := gsnormal;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FConfirmSelection := true;
  if ((Key in [VK_UP, VK_DOWN]) and (GetKeyState(VK_MENU) and $8000 = $8000))
    then
  begin
    if Assigned(FAdvDropDown) then
      SendMessage(FAdvDropDown.Handle, WM_KEYDOWN, Key, 0);
    Exit;
  end;

  inherited;

  if (Key = vk_return) then
    SelectionChanged;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.DoExit;
begin
  inherited;
  // if Visible then
  // ParentEdit.HideGridList;
end;

// ------------------------------------------------------------------------------

function TAdvColGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint)
  : Boolean;
begin
  FMouseWheelSelection := true;
  FConfirmSelection := False;
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  FMouseWheelSelection := False;
end;

// ------------------------------------------------------------------------------

function TAdvColGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint)
  : Boolean;
begin
  FMouseWheelSelection := true;
  FConfirmSelection := False;
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  FMouseWheelSelection := False;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTTAB or DLGC_WANTARROWS;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
const
  Ellips: array[Boolean] of DWORD = (0, DT_END_ELLIPSIS);
  WordWraps: array[Boolean] of DWORD = (DT_SINGLELINE,
    DT_WORDBREAK or DT_EDITCONTROL);
  Alignments: array [TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);

var
  part: TGradientpart;
  DTSTYLE: DWORD;
  clr: TColor;
  Aln: TAlignment;
  ww,el: Boolean;
  cs: string;
  GLW, ImgIdx, Y: Integer;
  TxtR, R1: TRect;

begin
  if Assigned(AdvDropDown) and Assigned(AdvDropDown.OnDropDownDrawItem) then
  begin
    AdvDropDown.OnDropDownDrawItem(AdvDropDown, ACol, ARow, Canvas, ARect);
    Exit;
  end;

  if (ACol = FixedCols) then
    part := gpLeft
  else if (ACol = ColCount - 1) then
    part := gpRight
  else
    part := gpMiddle;

  ARect := CellRect(ACol, ARow);

  if (ACol = ColCount - 1) and (ACol = FixedCols) then
    part := gpFull;

  ImgIdx := -1;

  if Arow >= 0 then
    cs := Cells[ACol, ARow]
  else
    cs := '';

  if (ACol < Columns.Count) and (ACol >= FixedCols) and (Columns.Count > 0) then
  begin
    clr := Columns[ACol].Color;
    Aln := Columns[ACol].Alignment;
    ww := Columns[ACol].Wordwrap;
    el := Columns[ACol].Ellipsis;
    Canvas.Font.Assign(Columns[ACol].Font);

    if Assigned(AdvDropDown) and (ARow > 0) and
      (ARow - 1 < AdvDropDown.Items.Count) then
    begin
      if (Columns[ACol].ColumnType = ctImage) then
        ImgIdx := AdvDropDown.Items[ARow - 1].ImageIndex;
      if (ACol < AdvDropDown.Items[ARow - 1].Text.Count) then
        cs := AdvDropDown.Items[ARow - 1].Text[ACol];
    end;
  end
  else
  begin
    clr := Color;
    Aln := taLeftJustify;
    ww := False;
    el := False;
    Canvas.Font.Assign(Self.HeaderFont);
  end;

  if Assigned(AdvDropDown) and Assigned(AdvDropDown.OnDropDownGetItemText) then
    AdvDropDown.OnDropDownGetItemText(AdvDropDown, ACol, ARow, cs);

  // if Assigned(AdvDropDown) and Assigned(AdvDropDown.OnDropDownGetItemText) then  // TODO:
  // AdvDropDown.OnDropDownGetItemProp(AdvDropDown, ACol, ARow, Canvas.Font, Clr, Clr);

  // --- Draw Background
  if (gdFixed in AState) then
  begin

    cs := '';
    if Assigned(AdvDropDown) and (ACol < Columns.Count) then
    begin
      ImgIdx := -1;
      cs := Columns[ACol].Header;
    end;

    if (HeaderColor <> clNone) and (HeaderColorTo <> clNone) then
    begin
      if Assigned(AdvDropDown) then
        AdvDropDown.DrawGradientBackground
          (Canvas, HeaderColor, HeaderColorTo, 80, ARect, gdVertical)
      else
        DrawGradient(Canvas, HeaderColor, HeaderColorTo, 80, ARect, False);
    end
    else
    begin
      Canvas.Brush.Color := HeaderColor;
      Canvas.FillRect(ARect);
    end;
  end
  else if (gdSelected in AState) then
  begin
    if Assigned(AdvDropDown) then
    begin
      R1 := ARect;
      // R1.Right := R1.Right + 1;
      // R1.Bottom := R1.Bottom + 1;
      // R1.Top := R1.Top - 1;
      AdvDropDown.DrawSelectionBackground(Canvas, R1, clr, part);
    end
    else
    begin
      Canvas.Brush.Color := clActiveCaption;
      Canvas.FillRect(ARect);
    end;
  end
  else
  begin
    Canvas.Brush.Color := clr;
    Canvas.Pen.Color := clr;
    Canvas.Pen.Width := 1;
    // Canvas.FillRect(ARect);
    Canvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end;

  // --- Draw lines
  // Canvas.Pen.Color := LineColor;  // $F1EDEB;
  // Canvas.Pen.Width := 1;
  if gdFixed in AState then
  begin
    Canvas.Pen.Color := FixedLineColor;
    Canvas.Pen.Width := 1;
    GLW := 1;
  end
  else
  begin
    if FGridLineWidth > 0 then
    begin
      Canvas.Pen.Color := LineColor;
    end;

    Canvas.Pen.Width := FGridLineWidth;
    GLW := (FGridLineWidth + 1) shr 1;
  end;

  // Draw grid borders
  if ((goHorzLine in Options) and not(gdFixed in AState)) or
    ((goFixedHorzLine in Options) and (gdFixed in AState)) then
  begin
    Canvas.MoveTo(ARect.Left - GLW + 1, ARect.Bottom - GLW);
    Canvas.LineTo(ARect.Right - GLW + 1, ARect.Bottom - GLW);
  end;

  if ((goVertLine in Options) and not(gdFixed in AState)) or
    ((goFixedVertLine in Options) and (gdFixed in AState)) then
  begin
    if UseRightToLeftAlignment then
    begin
      Canvas.MoveTo(ARect.Right - GLW + 1, ARect.Bottom - GLW);
      Canvas.LineTo(ARect.Right - GLW + 1, ARect.Top - GLW);
    end
    else
    begin
      Canvas.MoveTo(ARect.Right - GLW, ARect.Bottom - GLW);
      Canvas.LineTo(ARect.Right - GLW, ARect.Top - GLW);
    end;
  end;

  Inflaterect(ARect, -FGridLineWidth, -FGridLineWidth);

  TxtR := ARect;
  if Assigned(AdvDropDown) and Assigned(AdvDropDown.Images) and (ImgIdx >= 0)
    then
  begin
    Y := ARect.Top;
    if (AdvDropDown.Images.Height < ARect.Bottom - ARect.Top) then
      Y := ARect.Top + ((ARect.Bottom - ARect.Top) - AdvDropDown.Images.Height)
        div 2;
    AdvDropDown.Images.Draw(Canvas, ARect.Left + 2, Y, ImgIdx);
    TxtR.Left := TxtR.Left + AdvDropDown.Images.Width + 4;
  end
  else
    TxtR.Left := TxtR.Left + 2;

  if (cs <> '') then
  begin
    DTSTYLE := DT_EXPANDTABS or DT_NOPREFIX or WordWraps[ww] or Alignments[Aln]
      or DT_VCENTER or Ellips[el];

    SetBkMode(Canvas.Handle, TRANSPARENT);

    if (gdSelected in AState) then
      Canvas.Font.Color := AdvDropDown.SelectionTextColor;

    if (gdFixed in AState) then
      Canvas.Font.Assign(HeaderFont);

    DrawText(Canvas.Handle, PChar(cs), length(cs), TxtR, DTSTYLE);
  end;

end;


// ------------------------------------------------------------------------------

function TAdvColGrid.GetDesiredWidth: Integer;
var
  i: Integer;
begin
  Result := BorderWidth * 2;
  for i := 0 to ColCount - 1 do
    Result := Result + ColWidths[i];

  if (VisibleRowCount + FixedRows <> RowCount) then
    Result := Result + GetSystemMetrics(SM_CXVSCROLL);
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.AutoSizeColumn(ACol: Integer);
var
  s: string;
  r, ImgIndx, cw, w: Integer;
  Rt: TRect;
begin
  if (ACol >= 0) and (ACol < Columns.Count) and (Columns[ACol].AutoSize)
    and Assigned(Parent) and (Parent is TDropDownForm) then
  begin
    TDropDownForm(Parent).Canvas.Font.Assign(Columns[ACol].Font);
    ImgIndx := -1;
    w := 0;
    for r := 1 to RowCount - 1 do
    begin
      cw := 0;
      if Assigned(AdvDropDown) then
      begin
        if (ACol < AdvDropDown.Items[r - 1].Text.Count) then
        begin
          s := AdvDropDown.Items[r - 1].Text[ACol];
          if (Columns[ACol].ColumnType = ctImage) then
            ImgIndx := AdvDropDown.Items[r - 1].ImageIndex;
        end
        else
          s := '';
      end
      else
      begin
        s := Cells[ACol, r];
        ImgIndx := -1;
      end;

      if (ImgIndx >= 0) and Assigned(Images) then
      begin
        cw := cw + Images.Width + 4;
      end;

      if (s <> '') then
      begin // using Parent's canvas to control the abnormal behavior of grid
        Rt := Rect(0, 0, 1000, 500);
        DrawText(TDropDownForm(Parent).Canvas.Handle, PChar(s), length(s), Rt,
          DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
        cw := cw + Rt.Right + 6;
      end;

      w := Max(w, cw);
    end;

    Columns[ACol].Width := w;
    ColWidths[ACol] := w;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.DoWordWrap;
var
  s: string;
  r, c, ImgIndx, ch, h: Integer;
  Rt: TRect;
  ww: Boolean;
begin
  ww := False;
  for c := 0 to Columns.Count - 1 do
  begin
    if Columns[c].Wordwrap then
    begin
      ww := true;
      Break;
    end;
  end;

  if not ww or not Assigned(Parent) or not(Parent is TDropDownForm) then
  // no word wrap required
    Exit;

  for r := FixedRows to RowCount - 1 do
  begin
    h := 24;
    for c := 0 to ColCount - 1 do
    begin
      ch := 0;
      if Columns[c].Wordwrap then
      begin
        ImgIndx := -1;
        if Assigned(AdvDropDown) then
        begin
          if (c < AdvDropDown.Items[r - 1].Text.Count) then
          begin
            s := AdvDropDown.Items[r - 1].Text[c];
            if (Columns[c].ColumnType = ctImage) then
              ImgIndx := AdvDropDown.Items[r - 1].ImageIndex;
          end
          else
            s := '';
        end
        else
        begin
          s := Cells[c, r - 1];
          ImgIndx := -1;
        end;

        if (ImgIndx >= 0) and Assigned(Images) then
        begin
          ch := ch + Images.Height + 6;
        end;

        if (s <> '') then
        begin // using Parent's canvas to control the abnormal behavior of grid
          TDropDownForm(Parent).Canvas.Font.Assign(Columns[c].Font);
          Rt := Rect(0, 0, ColWidths[c], 500);
          DrawText(TDropDownForm(Parent).Canvas.Handle, PChar(s), length(s),
            Rt, DT_CALCRECT or DT_LEFT or DT_Top or DT_WORDBREAK);
          ch := Max(ch, Rt.Bottom + 6);
        end;
        h := Max(h, ch);
      end;
    end;


    RowHeights[r] := h;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.AutoSizeColumns;
var
  i: Integer;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    if Columns[i].AutoSize then
      AutoSizeColumn(i);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.Initialize(DropDown: TAdvMultiColumnDropDown);
var
  i: Integer;

begin
  if Assigned(DropDown) then
  begin
    AdvDropDown := DropDown;

    Columns.Assign(AdvDropDown.Columns);
    ColCount := Columns.Count;
    RowCount := AdvDropDown.Items.Count + 1;

    if RowCount > 1 then
    begin
      FixedRows := 1; // ensure there is a fixed row
      Row := 1;
      RowCount := AdvDropDown.Items.Count + 1;
    end;

    for i := 0 to AdvDropDown.Columns.Count - 1 do
    begin
      ColWidths[i] := Columns[i].Width;
    end;

    LineColor := AdvDropDown.LineColor;
    FixedLineColor := AdvDropDown.FixedLineColor;
    HeaderColor := AdvDropDown.HeaderColor;
    HeaderColorTo := AdvDropDown.HeaderColorTo;
    HeaderHeight := AdvDropDown.HeaderHeight;
    HeaderFont.Assign(AdvDropDown.HeaderFont);
    Images := AdvDropDown.Images;


    if AdvDropDown.ColumnSizing then
      Options := Options + [goColSizing]
    else
      Options := Options - [goColSizing];

    AutoSizeColumns;

    DefaultRowHeight := AdvDropDown.DropDownRowHeight;

    DoWordWrap;

    RowHeights[0] := Max(1,AdvDropDown.HeaderHeight);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderColor(const Value: TColor);
begin
  FHeaderColor := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderColorTo(const Value: TColor);
begin
  FHeaderColorTo := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not(csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetColumns(const Value: TDropDownColumns);
begin
  FColumns.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetHeaderHeight(const Value: Integer);
begin
  FHeaderHeight := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetAdvDropDown(const Value: TAdvMultiColumnDropDown);
begin
  FAdvDropDown := Value;
end;

// ------------------------------------------------------------------------------

function TAdvColGrid.CellRect(c, r: Integer): TRect;
var
  R1: TRect;
  rh: integer;
begin
  Result := Rect(0,0,0,0);

  if (r >= 0) and (c >= 0) then
  begin
    R1 := inherited CellRect(c, r);
    rh := RowHeights[r];
    Result := Rect(R1.Left, R1.Top, R1.Left + ColWidths[c], R1.Top + rh);
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SetGridLineWidth(const Value: Integer);
begin
  FGridLineWidth := Value;
  inherited GridLineWidth := 0;
  Invalidate;
end;

// ------------------------------------------------------------------------------

function TAdvColGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  c, r: Longint;
begin
  inherited;
  MouseToCell(X, Y, c, r);
  if (r >= FixedRows) and (r < RowCount) then
    SelectionChanged;
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.SelectionChanged;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

// ------------------------------------------------------------------------------

procedure TAdvColGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FConfirmSelection := true;
  FOldRow := Row;
  inherited;
end;

// ------------------------------------------------------------------------------

end.
