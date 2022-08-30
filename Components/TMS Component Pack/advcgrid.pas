{**************************************************************************}
{ TADVCOLUMNGRID component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1996 - 2015                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

{$I TMSDEFS.INC}

unit AdvCGrid;

interface

uses
  BaseGrid, AdvGrid, Classes, Grids, Graphics, SysUtils, Windows, StdCtrls,
  Controls, Menus, Messages, Dialogs, AdvObj, Types;

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 4; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Nov, 2015'; // Release date

  // version history
  // 2.8.4.1 : changing in grouping to save & restore column setting oi grouped column
  // 2.8.7.0 : fixed issue with fixed rows color when grid.Flat = true

  // 3.0.0.0 : Version built on TAdvStringGrid v3.0
  //         : OnAfterColumnMoved event added
  // 3.0.0.1 : Fixed issue with column headers during column insert & delete
  // 3.0.0.3 : Version compatible with base class TAdvStringGrid v3.0.0.3
  // 3.0.0.4 : Fixed issue with column updates for hidden columns
  // 3.0.0.5 : Improved design-time load time
  // 3.0.0.6 : Fixed column moving for grid with hidden columns
  // 3.0.0.7 : Implemented workaround for using grid.Cells[x,0] instead of grid.Columns[x].Header
  // 3.0.0.8 : Improved LoadColumnPositions method for empty INI files
  //         : New DropDownCount property in Columns
  // 3.0.0.9 : Fixed issue with hidden columns and fixed cell control
  // 3.0.0.10: Added property PrintBorderCustom: boolean in ColumnItem
  // 3.0.0.11: Fixed issue with design time PrintFont setting
  //         : Fixed issue with direct cell font properties use during printing
  //         : Fixed issue with per cell alignment control
  // 3.0.1.0 : Added property PrintFontCustom: boolean in ColumnItem
  // 3.0.1.1 : Fixed issue with AdvanceOnEnter & hidden columns
  // 3.0.1.2 : Fixed issue with LoadColumnPosition
  // 3.1.0.0 : Compatibility release with TAdvStringGrid v3.4
  // 3.1.0.1 : Fix for Bands color handling & cell property settings
  // 3.1.0.2 : Fixed issue with InsertCols & column headers
  // 3.1.0.3 : Fixed issue with default color on fixed cells
  // 3.1.0.4 : Fixed issue with bands & checkbox colors
  // 3.1.0.5 : Fixed issue with header alignment for merged cells
  // 3.1.0.6 : Implemented workaround for C++Builder 2007 bug
  // 3.1.0.7 : Fixed issue with grid.Colors[col,row] on cells with images,checkbox,..
  // 3.1.1.0 : ChangeScale method implemented
  // 3.1.1.1 : Fixed: issue with fixed cell color for fixed cells with nodes
  // 3.1.1.2 : Fixed: issue with merged cell printing
  //         : Fixed: issue with filter initialization
  // 3.1.1.3 : Fixed: issue with column FloatFormat property
  // 3.1.1.4 : Fixed: issue with OnComboChange with specific MouseAction settings
  // 3.1.2.0 : Improved : Handling of SaveColPositions / LoadColPositions
  // 3.1.2.1 : Fixed : Issue with ChangeScale at design time
  // 3.1.2.2 : Fixed : Issue with filter list initialization
  // 3.1.2.3 : Fixed : Issue with grid.Columns[].EditLength for regular inplace editor
  // 3.1.2.4 : Fixed : Issue with Columns[].DropDownCount
  // 3.1.2.5 : Fixed : Issue with header font setting & column moving
  // 3.1.3.0 : Improved : Floats[] property setter uses Columns[].FloatFormat when defined
  // 3.1.3.1 : Fixed : Rare issue with column width changing during form closing
  // 3.1.3.2 : Fixed : Issue with using grid.ComboIndex()
  // 3.1.3.3 : Fixed : Issue with FilterDropDownAuto = true
  // 3.1.3.4 : Fixed : Issue at design time when comboboxes are used
  // 3.1.3.5 : Fixed : Issue with CheckAllCheck column header checkboxes
  // 3.1.3.6 : Fixed : Issue with Columns[i].MaxSize when using hidden columns
  // 3.1.3.7 : Improved : Use of column alignment with checkbox columns
  // 3.1.3.8 : Fixed : Issue with column hiding & using Columns[].ReadOnly
  // 3.1.3.9 : Fixed : Issue with MultiColumnCombobox editlink
  // 3.1.4.0 : Improved : Support for high DPI

type
  TAdvColumnGrid = class;

  TColumnPopupEvent = procedure(Sender: TObject; ACol, ARow: Integer; PopupMenu: TPopupMenu) of object;

  TAfterColumnMoved = procedure(Sender: TObject; FromIndex, ToIndex: Integer) of object;

  TColumnPopupType = (cpFixedCellsRClick,cpFixedCellsLClick,
    cpNormalCellsRClick,cpNormalCellsLClick,cpAllCellsRClick,cpAllCellsLClick);

  TGridColumnItem = class(TCollectionItem)
  private
    FWidth: Integer;
    FAlignment: TAlignment;
    FColumnHeader: string;
    FSortStyle: TSortStyle;
    FSortPrefix: string;
    FSortSuffix: string;
    FEditMask: string;
    FEditLength: Integer;
    FEditLink: TEditLink;
    FFont: TFont;
    FColor: TColor;
    FEditorType: TEditorType;
    FFixed: Boolean;
    FReadOnly: Boolean;
    FComboItems: TStringList;
    FDropDownCount : Integer;
    FSpinMin: Integer;
    FSpinMax: Integer;
    FSpinStep: Integer;
    FPassword: Boolean;
    FPrintFont: TFont;
    FPrintColor: TColor;
    FBorders: TCellBorders;
    FBorderPen: TPen;
    FPrintBorders: TCellBorders;
    FPrintBorderPen: TPen;
    FTag: Integer;
    FDefIdx: Integer;
    FName: string;
    FCheckFalse: string;
    FCheckTrue: string;
    FShowBands: Boolean;
    FFilter: TStringList;
    FMaxSize: Integer;
    FMinSize: Integer;
    FAutoMinSize: Integer;
    FAutoMaxSize: Integer;
    FColumnPopupType: TColumnPopupType;
    FColumnPopup: TPopupMenu;
    FFilterCaseSensitive: Boolean;
    FFloatFormat: string;
    FHeaderAlignment: TAlignment;
    FHeaderFont: TFont;
    FPrintBorderCustom: boolean;
    FPrintFontCustom: boolean;
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetColumnHeader(const Value:string);
    procedure SetFont(const Value:TFont);
    procedure SetColor(const Value:TColor);
    procedure SetFixed(const Value: Boolean);
    procedure SetPassword(const Value: Boolean);
    procedure SetComboItems(const Value: TStringList);
    procedure FontChanged(Sender: TObject);
    procedure PenChanged(Sender: TObject);
    procedure SetBorders(const Value: TCellBorders);
    procedure SetBorderPen(const Value: TPen);
    function GetRows(idx: Integer): string;
    procedure SetRows(idx: Integer; const Value: string);
    function GetDates(idx: Integer): TDateTime;
    function GetFloats(idx: Integer): Double;
    function GetInts(idx: Integer): Integer;
    procedure SetDates(idx: Integer; const Value: TDateTime);
    procedure SetFloats(idx: Integer; const Value: Double);
    procedure SetInts(idx: Integer; const Value: Integer);
    function GetTimes(idx: Integer): TDateTime;
    procedure SetTimes(idx: Integer; const Value: TDateTime);
    procedure SetEditorType(const Value: TEditorType);
    procedure SetShowBands(const Value: Boolean);
    procedure SetFilter(const Value: TStringList);
    procedure FilterChanged(Sender: TObject);
    procedure SetFloatFormat(const Value: string);
    procedure SetHeaderAlignment(const Value: TAlignment);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetPrintFont(const Value: TFont);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignVisuals(Source: TPersistent); 
    property DefIdx: Integer read FDefIdx write FDefIdx;
    property Rows[idx: Integer]: string read GetRows write SetRows;
    property Ints[idx: Integer]: Integer read GetInts write SetInts;
    property Floats[idx: Integer]: Double read GetFloats write SetFloats;
    property Dates[idx: Integer]: TDateTime read GetDates write SetDates;
    property Times[idx: Integer]: TDateTime read GetTimes write SetTimes;
  published
    property AutoMinSize: Integer read FAutoMinSize write FAutoMinSize;
    property AutoMaxSize: Integer read FAutoMaxSize write FAutoMaxSize;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Borders: TCellBorders read FBorders write SetBorders;
    property BorderPen: TPen read FBorderPen write SetBorderPen;
    property CheckFalse: string read FCheckFalse write FCheckFalse;
    property CheckTrue: string read FCheckTrue write FCheckTrue;
    property Color: TColor read FColor write SetColor;
    property ColumnPopup: TPopupMenu read FColumnPopup write FColumnPopup;
    property ColumnPopupType: TColumnPopupType read FColumnPopupType write FColumnPopupType;
    property ComboItems: TStringList read FComboItems write SetComboItems;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
    property EditLength: Integer read FEditLength write FEditLength;
    property EditLink: TEditLink read FEditLink write FEditLink;
    property EditMask: string read FEditMask write FEditMask;
    property Editor: TEditorType read FEditorType write SetEditorType;
    property Filter: TStringList read FFilter write SetFilter;
    property FilterCaseSensitive: Boolean read FFilterCaseSensitive write FFilterCaseSensitive;
    property Fixed: Boolean read FFixed write SetFixed;
    property FloatFormat: string read FFloatFormat write SetFloatFormat;
    property Font: TFont read FFont write SetFont;
    property Header: string read FColumnHeader write SetColumnHeader;
    property HeaderAlignment: TAlignment read FHeaderAlignment write SetHeaderAlignment;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property MinSize: Integer read FMinSize write FMinSize;
    property MaxSize: Integer read FMaxSize write FMaxSize;
    property Name: string read FName write FName;
    property Password: Boolean read FPassword write SetPassword;
    property PrintBorderCustom: boolean read FPrintBorderCustom write FPrintBorderCustom default True;
    property PrintBorders: TCellBorders read FPrintBorders write FPrintBorders;
    property PrintBorderPen: TPen read fPrintBorderPen write FPrintBorderPen;
    property PrintColor: TColor read FPrintColor write FPrintColor;
    property PrintFontCustom: boolean read FPrintFontCustom write FPrintFontCustom default True;
    property PrintFont: TFont read FPrintFont write SetPrintFont;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property ShowBands: Boolean read FShowBands write SetShowBands;
    property SortStyle: TSortStyle read FSortStyle write FSortStyle;
    property SortPrefix: string read FSortPrefix write FSortPrefix;
    property SortSuffix: string read FSortSuffix write FSortSuffix;
    property SpinMax: Integer read FSpinMax write FSpinMax;
    property SpinMin: Integer read FSpinMin write FSpinMin;
    property SpinStep: Integer read FSpinStep write FSpinStep;
    property Tag: Integer read FTag write FTag;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGridColumnCollection = class(TCollection)
  private
    FOwner: TAdvColumnGrid;
    FNoRecursiveUpdate: Boolean;
    function GetItem(Index: Integer): TGridColumnItem;
    procedure SetItem(Index: Integer; const Value: TGridColumnItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function GetItemClass: TCollectionItemClass; virtual;
    function Add: TGridColumnItem;
    function Insert(index: Integer): TGridColumnItem;
    property Items[Index: Integer]: TGridColumnItem read GetItem write SetItem; default;
    constructor Create(AOwner: TAdvColumnGrid);
    function GetOwner: TPersistent; override;
    procedure SetOrganization;
    procedure ResetOrganization;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvColumnGrid = class(TAdvStringGrid)
  private
    FColumnCollection: TGridColumnCollection;
    FSaveCol: TGridColumnItem;
    FColMoving: Boolean;
    FCellGraphic: TCellGraphic;
    FAutoFilterDisplay: Boolean;
    FOnColumnPopup: TColumnPopupEvent;
    FOnAfterColumnMoved: TAfterColumnMoved;
    procedure SetColumnCollection(const Value: TGridColumnCollection);
    function GetColumnCollection: TGridColumnCollection;
    function GetColCount: integer;
    procedure SetColCount(const Value: integer);
    procedure SynchHeaders;
    //procedure SynchWidths;
    procedure SynchColumns;
    //procedure SynchColMove;
    function GetColumnByName(AValue: string): TGridColumnItem;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; override;
    procedure CellsLoaded; override;
    procedure Loaded; override;
    procedure FilterSelect(Sender: TObject; ItemIndex: Integer); override;
    function GetFilter(ACol: Integer; Disp: boolean = false): Boolean; override;
    procedure UpdateColSize(ACol: Integer; var NewWidth: Integer); override;
    procedure UpdateAutoColSize(ACol: Integer; var NewWidth: Integer); override;
    procedure UpdateColHeaders; override;
    function HasColumnsProp: boolean; override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditLimit: Integer; override;
    function GetCellType(ACol,ARow: Integer): TCellType; override;
    function GetCellGraphic(ACol,ARow: Integer): TCellGraphic; override;
    procedure GetCellColor(ACol,ARow: Integer;AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    procedure GetCellPrintColor(ACol,ARow: Integer;AState: TGridDrawState; ABrush: TBrush; AFont: TFont); override;
    procedure GetCellBorder(ACol,ARow: Integer; APen:TPen;var borders:TCellBorders); override;
    procedure GetCellPrintBorder(ACol,ARow: Integer; APen:TPen;var borders:TCellBorders); override;
    procedure GetCellAlign(ACol,ARow: Integer;var HAlign:TAlignment;var VAlign: TVAlignment); override;
    procedure GetColFormat(ACol: Integer;var ASortStyle:TSortStyle;var aPrefix,aSuffix:string); override;
    procedure GetCellEditor(ACol,ARow: Integer;var AEditor:TEditorType); override;
    function GetCellFloatFormat(ACol,ARow: Integer): string; override;
    function HasCombo(ACol,ARow: Integer; AEditor: TEditorType = edNone): Boolean; override;
    procedure GetCellFixed(ACol,ARow: Integer;var IsFixed: Boolean); override;
    procedure GetCellReadOnly(ACol,ARow: Integer;var IsReadOnly: Boolean); override;
    procedure GetCellPassword(ACol,ARow: Integer;var IsPassword: Boolean); override;
    function GetFormattedCell(ACol,ARow: Integer): string; override;
    function GetCheckTrue(ACol,ARow: Integer): string; override;
    function GetCheckFalse(ACol,ARow: Integer): string; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function CreateColumns: TGridColumnCollection; virtual;
    procedure GetDefaultProps(ACol,ARow: Integer; AFont: TFont; ABrush: TBrush; var AColorTo,AMirrorColor,AMirrorColorTo: TColor;
      var HA: TAlignment; var VA: TVAlignment; var WW: boolean; var GD: TCellGradientDirection); override;
    procedure ChangeScale(M, D: Integer); override;
    procedure DoGetEditorProp(ACol,ARow: integer; EditLink: TEditLink); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; override;
    function GetVersionString:string; override;
    procedure SaveColumnsToStream(st: TStream);
    procedure LoadColumnsFromStream(st: TStream);
    property ColumnByName[AValue:string]: TGridColumnItem read GetColumnByName;
    procedure SaveColumnPositions(Key,Section: string);
    procedure LoadColumnPositions(Key,Section: string);
    procedure SaveColPositions; override;
    procedure LoadColPositions; override;
    procedure RemoveCols(ColIndex, CCount: Integer); override;
    procedure InsertCols(ColIndex, CCount: Integer); override;
    procedure Group(Colindex: Integer); override;
    procedure UnGroup; override;
  published
    property AutoFilterDisplay: Boolean read FAutoFilterDisplay write FAutoFilterDisplay default False;
    property Columns: TGridColumnCollection read GetColumnCollection write SetColumnCollection;
    property ColCount: Integer read GetColCount write SetColCount;
    property OnAfterColumnMoved: TAfterColumnMoved read FOnAfterColumnMoved write FOnAfterColumnMoved;
    property OnColumnPopup: TColumnPopupEvent read FOnColumnPopup write FOnColumnPopup;
  end;

  TAdvColumnGridIO = class(TComponent)
  private
    FItems: TGridColumnCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TGridColumnCollection read FItems write FItems;
  end;


implementation

uses
  IniFiles, AdvUtil;

{ TGridColumnItem }

constructor TGridColumnItem.Create(Collection:TCollection);
begin
  inherited;
  FWidth := 50;
  FFont := TFont.Create;
  FPrintFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FBorderPen := TPen.Create;
  FBorderPen.Width := 1;
  FBorderPen.Color := clSilver;

  FPrintBorders := [cbTop,cbLeft,cbRight,cbBottom];
  FPrintBorderPen := TPen.Create;
  FPrintBorderPen.Width := 1;
  FPrintBorderPen.Color := clBlack;

  FPrintBorderCustom := True;
  FPrintFontCustom := True;

  if Assigned(Collection) then
  begin
    FFont.Assign((TGridColumnCollection(Collection).FOwner).Font);
    FHeaderFont.Assign((TGridColumnCollection(Collection).FOwner).Font);
    FPrintFont.Assign((TGridColumnCollection(Collection).FOwner).Font);
    FColor := TGridColumnCollection(Collection).FOwner.Color;
    FFont.OnChange := FontChanged;
    FHeaderFont.OnChange := FontChanged;
    FBorderPen.OnChange := PenChanged;
  end;

  FPrintColor := clWhite;
  FComboItems := TStringList.Create;
  FDropDownCount := 8;
  FCheckTrue := 'Y';
  FCheckFalse := 'N';
  FSpinStep := 1;

  FMinSize := 0;
  FMaxSize := 0;
  FFilter := TStringList.Create;
  if Assigned(Collection) then
    FFilter.OnChange := FilterChanged;
end;

destructor TGridColumnItem.Destroy;
begin
  FFilter.Free;
  FFont.Free;
  FHeaderFont.Free;
  FPrintFont.Free;
  FComboItems.Free;
  FBorderPen.Free;
  FPrintBorderPen.Free;
  inherited Destroy;
end;

procedure TGridColumnItem.FontChanged(Sender:TObject);
begin
  TGridColumnCollection(Collection).Update(self);
end;

procedure TGridColumnItem.PenChanged(Sender:TObject);
begin
  TGridColumnCollection(Collection).Update(self);
end;

procedure TGridColumnItem.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  TGridColumnCollection(Collection).FOwner.AllColWidths[Index] := FWidth;
end;

function TGridColumnItem.GetWidth: Integer;
begin
  if Assigned(Collection) then
    Result := TGridColumnCollection(Collection).FOwner.AllColWidths[Index]
  else
    Result := 0;
end;

procedure TGridColumnItem.SetAlignment(const Value:TAlignment);
begin
  FAlignment := Value;
  TGridColumnCollection(Collection).FOwner.Invalidate;
end;

procedure TGridColumnItem.SetColumnHeader(const Value:string);
var
  i: Integer;
begin
  FColumnHeader := Value;

  if (TGridColumnCollection(Collection).FOwner.FixedRows > 0) and
    not TGridColumnCollection(Collection).FOwner.FColMoving then
  begin
    TGridColumnCollection(Collection).FOwner.Cells[Index,0] := Value;
    TGridColumnCollection(Collection).Update(Self);
  end;

  TGridColumnCollection(Collection).FOwner.ColumnHeaders.Clear;

  for i := 1 to TGridColumnCollection(Collection).Count do
  begin
    TGridColumnCollection(Collection).FOwner.ColumnHeaders.Add(
       TGridColumnCollection(Collection).Items[i - 1].Header);
  end;

end;

procedure TGridColumnItem.SetFont(const value:TFont);
begin
  FFont.Assign(value);
  TGridColumnCollection(Collection).Update(Self);
end;

procedure TGridColumnItem.SetColor(const Value:TColor);
begin
  FColor := Value;
  TGridColumnCollection(Collection).Update(Self);
end;

procedure TGridColumnItem.SetShowBands(const Value: Boolean);
begin
  FShowBands := Value;
  TGridColumnCollection(Collection).Update(Self);  
end;

procedure TGridColumnItem.SetFixed(const Value: Boolean);
begin
  FFixed := Value;
  TGridColumnCollection(Collection).Update(Self);
end;

procedure TGridColumnItem.Assign(Source: TPersistent);
begin
  if Source is TGridColumnItem then
  begin
    FAlignment := TGridColumnItem(Source).Alignment;
    FHeaderAlignment := TGridColumnItem(Source).HeaderAlignment;
    FBorderPen.Assign(TGridColumnItem(Source).BorderPen);
    FBorders := TGridColumnItem(Source).Borders;
    FCheckFalse := TGridColumnItem(Source).CheckFalse;
    FCheckTrue := TGridColumnItem(Source).CheckTrue;
    FColor := TGridColumnItem(Source).Color;
    FComboItems.Assign(TGridColumnItem(Source).ComboItems);
    FDropDownCount := TGridColumnItem(Source).DropDownCount;
    FEditLength := TGridColumnItem(Source).EditLength;
    FEditLink := TGridColumnItem(Source).EditLink;
    FEditMask := TGridColumnItem(Source).EditMask;
    FEditorType := TGridColumnItem(Source).Editor;
    FFixed := TGridColumnItem(Source).Fixed;
    FFont.Assign(TGridColumnItem(Source).Font);
    FHeaderFont.Assign(TGridColumnItem(Source).HeaderFont);
    FColumnHeader := TGridColumnItem(Source).Header;
    FName := TGridColumnItem(Source).Name;
    FPassword := TGridColumnItem(Source).Password;
    FPrintBorderPen.Assign(TGridColumnItem(Source).PrintBorderPen);
    FPrintBorders := TGridColumnItem(Source).PrintBorders;
    FPrintColor := TGridColumnItem(Source).PrintColor;
    FPrintFont.Assign(TGridColumnItem(Source).PrintFont);
    FReadOnly := TGridColumnItem(Source).ReadOnly;
    FSortPrefix := TGridColumnItem(Source).SortPrefix;
    FSortStyle := TGridColumnItem(Source).SortStyle;
    FSortSuffix := TGridColumnItem(Source).SortSuffix;
    FSpinMax := TGridColumnItem(Source).SpinMax;
    FSpinMin := TGridColumnItem(Source).SpinMin;
    FSpinStep := TGridColumnItem(Source).SpinStep;
    FColumnPopup := TGridColumnItem(Source).ColumnPopup;
    FTag := TGridColumnItem(Source).Tag;
    FWidth := TGridColumnItem(Source).Width;
    FShowBands := TGridColumnItem(Source).ShowBands;
    FMinSize := TGridColumnItem(Source).MinSize;
    FMaxSize := TGridColumnItem(Source).MaxSize;
    FAutoMinSize := TGridColumnItem(Source).AutoMinSize;
    FAutoMaxSize := TGridColumnItem(Source).AutoMaxSize;
    FFilter.Assign(TGridColumnItem(Source).Filter);
    FFilterCaseSensitive := TGridColumnItem(Source).FilterCaseSensitive;
    FDefIdx := TGridColumnItem(Source).DefIdx;
    FFloatFormat := TGridColumnItem(Source).FloatFormat;
    FSortStyle := TGridColumnItem(Source).SortStyle;
    FSortPrefix := TGridColumnItem(Source).SortPrefix;
    FSortSuffix := TGridColumnItem(Source).SortSuffix;        
  end;
end;

procedure TGridColumnItem.AssignVisuals(Source: TPersistent);
begin
  if Source is TGridColumnItem then
  begin
    FAlignment := TGridColumnItem(Source).Alignment;
    FHeaderAlignment := TGridColumnItem(Source).HeaderAlignment;
    FBorderPen.Assign(TGridColumnItem(Source).BorderPen);
    FBorders := TGridColumnItem(Source).Borders;
    FCheckFalse := TGridColumnItem(Source).CheckFalse;
    FCheckTrue := TGridColumnItem(Source).CheckTrue;
    FColor := (Collection as TGridColumnCollection).FOwner.Color;
    FEditLength := TGridColumnItem(Source).EditLength;
    FEditLink := TGridColumnItem(Source).EditLink;
    FEditMask := TGridColumnItem(Source).EditMask;
    FEditorType := TGridColumnItem(Source).Editor;
    FFixed := TGridColumnItem(Source).Fixed;
    FFont.Assign((Collection as TGridColumnCollection).FOwner.Font);
    FPassword := TGridColumnItem(Source).Password;
    FPrintBorderPen.Assign(TGridColumnItem(Source).PrintBorderPen);
    FPrintBorders := TGridColumnItem(Source).PrintBorders;
    FPrintColor := TGridColumnItem(Source).PrintColor;
    FPrintFont.Assign(TGridColumnItem(Source).PrintFont);
    FReadOnly := TGridColumnItem(Source).ReadOnly;
    FSortPrefix := TGridColumnItem(Source).SortPrefix;
    FSortStyle := TGridColumnItem(Source).SortStyle;
    FSortSuffix := TGridColumnItem(Source).SortSuffix;
    FSpinMax := TGridColumnItem(Source).SpinMax;
    FSpinMin := TGridColumnItem(Source).SpinMin;
    FSpinStep := TGridColumnItem(Source).SpinStep;
    FColumnPopup := TGridColumnItem(Source).ColumnPopup;
    FWidth := TGridColumnItem(Source).Width;
    FShowBands := TGridColumnItem(Source).ShowBands;
    FMinSize := TGridColumnItem(Source).MinSize;
    FMaxSize := TGridColumnItem(Source).MaxSize;
    FAutoMinSize := TGridColumnItem(Source).AutoMinSize;
    FAutoMaxSize := TGridColumnItem(Source).AutoMaxSize;
    FFilter.Assign(TGridColumnItem(Source).Filter);
    FFilterCaseSensitive := TGridColumnItem(Source).FilterCaseSensitive;
    FDefIdx := TGridColumnItem(Source).DefIdx;
    FFloatFormat := TGridColumnItem(Source).FloatFormat;
    FSortStyle := TGridColumnItem(Source).SortStyle;
    FSortPrefix := TGridColumnItem(Source).SortPrefix;
    FSortSuffix := TGridColumnItem(Source).SortSuffix;        
  end;
end;


function TGridColumnItem.GetDisplayName: string;
begin
  if Name = '' then
    Result := 'Column ' + Inttostr(Index)
  else
    Result := 'Col '+ IntToStr(Index) + ': ' + Name;
end;

procedure TGridColumnItem.SetComboItems(const Value: TStringList);
begin
  FComboItems.Assign(Value);
end;

procedure TGridColumnItem.SetPassword(const Value: Boolean);
begin
  FPassword := Value;
  TGridColumnCollection(Collection).Update(Self);
end;

procedure TGridColumnItem.SetBorderPen(const Value: TPen);
begin
  FBorderPen := Value;
  TGridColumnCollection(Collection).Update(Self);
end;

procedure TGridColumnItem.SetBorders(const Value: TCellBorders);
begin
  FBorders := Value;
  TGridColumnCollection(Collection).Update(Self);
end;

function TGridColumnItem.GetRows(idx: integer): string;
begin
  Result := (Collection as TGridColumnCollection).FOwner.Cells[Index,idx];
end;

procedure TGridColumnItem.SetRows(idx: integer; const Value: string);
begin
  (Collection as TGridColumnCollection).FOwner.Cells[Index,idx] := Value;
end;

function TGridColumnItem.GetDates(idx: Integer): TDateTime;
begin
  Result := (Collection as TGridColumnCollection).FOwner.Dates[Index,idx];
end;

function TGridColumnItem.GetFloats(idx: Integer): Double;
begin
  Result := (Collection as TGridColumnCollection).FOwner.Floats[Index,idx];
end;

function TGridColumnItem.GetInts(idx: Integer): Integer;
begin
  Result := (Collection as TGridColumnCollection).FOwner.Ints[Index,idx];
end;

procedure TGridColumnItem.SetDates(idx: Integer; const Value: TDateTime);
begin
  (Collection as TGridColumnCollection).FOwner.Dates[Index,idx] := Value;
end;

procedure TGridColumnItem.SetFloats(idx: Integer; const Value: Double);
begin
  (Collection as TGridColumnCollection).FOwner.Floats[Index,idx] := Value;
end;

procedure TGridColumnItem.SetInts(idx: Integer; const Value: Integer);
begin
  (Collection as TGridColumnCollection).FOwner.Ints[Index,idx] := Value;
end;

function TGridColumnItem.GetTimes(idx: Integer): TDateTime;
begin
  Result := (Collection as TGridColumnCollection).FOwner.Times[Index,idx];
end;

procedure TGridColumnItem.SetTimes(idx: Integer; const Value: TDateTime);
begin
  (Collection as TGridColumnCollection).FOwner.Times[Index,idx] := Value;
end;

procedure TGridColumnItem.SetEditorType(const Value: TEditorType);
var
  UpdateFlg: Boolean;
begin
  if FEditorType <> Value then
  begin
    UpdateFlg := (FEditorType = edDataCheckBox) or (Value = edDataCheckBox);
    FEditorType := Value;
    if UpdateFlg then
      (Collection as TGridColumnCollection).FOwner.Invalidate;
  end;
end;


procedure TGridColumnItem.SetFilter(const Value: TStringList);
begin
  FFilter.Assign(Value);
end;

procedure TGridColumnItem.FilterChanged(Sender: TObject);
begin
  (Collection as TGridColumnCollection).FOwner.Invalidate;
end;

procedure TGridColumnItem.SetFloatFormat(const Value: string);
begin
  FFloatFormat := Value;
  (Collection as TGridColumnCollection).FOwner.Invalidate;
end;

procedure TGridColumnItem.SetHeaderAlignment(const Value: TAlignment);
begin
  FHeaderAlignment := Value;
  (Collection as TGridColumnCollection).FOwner.Invalidate;
end;

procedure TGridColumnItem.SetPrintFont(const Value: TFont);
begin
  FPrintFont.Assign(Value);
end;

procedure TGridColumnItem.SetHeaderFont(const Value: TFont);
begin
  FHeaderFont.Assign(Value);
  (Collection as TGridColumnCollection).FOwner.Invalidate;
end;

{ TGridColumnCollection }

function TGridColumnCollection.Add: TGridColumnItem;
begin
  Result := TGridColumnItem(inherited Add);

  if Count > 1 then
    Result.AssignVisuals(Items[Count - 2]);
end;

constructor TGridColumnCollection.Create(AOwner:TAdvColumnGrid);
begin
  inherited Create(GetItemClass);
  FOwner := AOwner;
  FNoRecursiveUpdate := False;
end;

function TGridColumnCollection.GetItem(Index: Integer): TGridColumnItem;
begin
  Result := TGridColumnItem(inherited GetItem(Index));
end;


function TGridColumnCollection.GetItemClass: TCollectionItemClass;
begin
  Result := TGridColumnItem;
end;

function TGridColumnCollection.GetOwner:TPersistent;
begin
  Result := FOwner;
end;

function TGridColumnCollection.Insert(Index: Integer): TGridColumnItem;
begin
  Result := TGridColumnItem(inherited Insert(Index));
end;

procedure TGridColumnCollection.ResetOrganization;
var
  i: Integer;
  rst: Boolean;
begin
  rst := False;
  while not rst do
  begin
    rst := True;
    for i := 1 to Count do
    begin
      if i - 1 > Items[i - 1].DefIdx then
      begin
        rst := False;
        FOwner.MoveColumn(i - 1,Items[i - 1].DefIdx);
      end;
    end;
  end;
end;

procedure TGridColumnCollection.SetItem(Index: Integer;
  const Value: TGridColumnItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridColumnCollection.SetOrganization;
var
  i: Integer;
begin
  FNoRecursiveUpdate := True;
  FOwner.SynchColumns;
  FNoRecursiveUpdate := False;

  for i := 1 to Count do
    Items[i - 1].DefIdx := i - 1;

{$IFDEF TMSDEBUG}
  for i := 1 to Count do
    outputdebugstring(pchar(inttostr(Items[i-1].DefIdx)));
{$ENDIF}
end;

procedure TGridColumnCollection.Update(Item:TCollectionItem);
var
  VisCols: Integer;
begin
  inherited Update(Item);

  if UpdateCount > 0 then
   Exit;

  //reflect changes
  if (not FNoRecursiveUpdate) and not (csLoading in FOwner.ComponentState) then
  begin
    VisCols := Count - FOwner.NumHiddenColumns;
    //if (csDesigning in fOwner.ComponentState) then
    if (VisCols <> FOwner.ColCount) and (VisCols > FOwner.FixedCols) then
      FOwner.ColCount := VisCols;

    // always synch the headers
    FOwner.SynchHeaders;
    FOwner.Invalidate;
  end;
end;

{ TAdvColumnGrid }

procedure TAdvColumnGrid.ChangeScale(M, D: Integer);
var
  j : integer;
begin
  inherited;

  if not (csDesigning in ComponentState) and (M <> D) then
  begin
    for j := 0 to ColCount - 1 do
    begin
      Columns[j].Font.Height := MulDiv(Columns[j].Font.Height, M, D);
      Columns[j].HeaderFont.Height := MulDiv(Columns[j].HeaderFont.Height, M, D);
      Columns[j].Width := MulDiv(Columns[j].Width, M, D);
    end;
  end;
end;

procedure TAdvColumnGrid.ColumnMoved(FromIndex, ToIndex: Integer);
var
  CN: TGridColumnItem;
  tr: Integer;
  RFI,RTI: Integer;
begin
  FColumnCollection.FNoRecursiveUpdate := True;

//  SynchColMove;

  tr := TopRow;

  RFI := RealColIndex(FromIndex);
  RTI := RealColIndex(ToIndex);

  {
  outputdebugstring(pchar('move from : ' + inttostr(rfi)+' to :'+inttostr(rti)));
  outputdebugstring(pchar('col header from : ' + FColumnCollection.Items[RFI].Header));
  outputdebugstring(pchar('col header to : ' + FColumnCollection.Items[RTI].Header));
  }

  inherited;

  BeginUpdate;
  FColMoving := True;
  TGridColumnItem(FColumnCollection.Add).Assign(TGridColumnItem(FColumnCollection.Items[RFI]));
  TGridColumnItem(FColumnCollection.Items[RFI]).Free;
  CN := TGridColumnItem(FColumnCollection.Insert(RTI));
  CN.Assign(FColumnCollection.Items[FColumnCollection.Count - 1]);
  FColumnCollection.Items[FColumnCollection.Count - 1].Free;
  TopRow := tr;
  FColMoving := False;
  EndUpdate;
  FColumnCollection.FNoRecursiveUpdate := False;
  //HideSelection;

  if Assigned(FOnAfterColumnMoved) then
    FOnAfterColumnMoved(Self, RFI, RTI);
end;

procedure TAdvColumnGrid.ColWidthsChanged;
var
  i,rc: Integer;
begin
  inherited;

  if (csLoading in ComponentState) or (csDestroying in ComponentState) then
    Exit;

  for i := 1 to ColCount do
  begin
    rc := RealColIndex(i - 1);
    if (rc < FColumnCollection.Count) then
      TGridColumnItem(FColumnCollection.Items[rc]).FWidth := ColWidths[i - 1];
  end;
end;


constructor TAdvColumnGrid.Create(AOwner: TComponent);
begin
  inherited;
  FColumnCollection := CreateColumns;
  FColumnCollection.FNoRecursiveUpdate := True;
  SynchColumns;
  FColumnCollection.FNoRecursiveUpdate := False;
  FColMoving := False;
  FCellGraphic := TCellGraphic.Create;
  FSaveCol := TGridColumnItem.Create(nil);
end;

destructor TAdvColumnGrid.Destroy;
begin
  FColumnCollection.Free;
  FCellGraphic.Free;
  FSaveCol.Free;
  inherited;
end;

procedure TAdvColumnGrid.DoGetEditorProp(ACol, ARow: integer;
  EditLink: TEditLink);
begin
  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin

      if not (csDesigning in ComponentState) and (TGridColumnItem(FColumnCollection.Items[ACol]).Editor in [edComboEdit, edComboList])
        and Assigned(ComboBox) then
      begin
        ComboBox.Items.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).ComboItems);
        ComboBox.DropDownCount := TGridColumnItem(FColumnCollection.Items[ACol]).DropDownCount;
      end;

      if not (csDesigning in ComponentState)
        and (TGridColumnItem(FColumnCollection.Items[ACol]).Editor in [edSpinEdit, edFloatSpinEdit, edTimeSpinEdit, edDateSpinEdit])
        and Assigned(SpinEdit) then
      begin
        SpinEdit.MinValue := TGridColumnItem(FColumnCollection.Items[ACol]).SpinMin;
        SpinEdit.MaxValue := TGridColumnItem(FColumnCollection.Items[ACol]).SpinMax;
        SpinEdit.Increment := TGridColumnItem(FColumnCollection.Items[ACol]).SpinStep;
      end;

      self.EditLink := TGridColumnItem(FColumnCollection.Items[ACol]).EditLink;
      ControlLook.DropDownCount := TGridColumnItem(FColumnCollection.Items[ACol]).DropDownCount;
    end;
  end;

  inherited;
end;

procedure TAdvColumnGrid.GetCellAlign(ACol, ARow: integer;
  var HAlign: TAlignment; var VAlign: TVAlignment);
begin
  if FColumnCollection.Count > ACol then
  begin
    if ARow < FixedRows then
      HAlign := TGridColumnItem(FColumnCollection.Items[ACol]).HeaderAlignment
    else
      HAlign := TGridColumnItem(FColumnCollection.Items[ACol]).Alignment;

    if HasCellProperties(ACol,ARow) then
    begin
      HAlign := CellProperties[ACol,ARow].Alignment
    end;
  end;

  inherited;
end;

procedure TAdvColumnGrid.GetCellBorder(ACol, ARow: integer; APen: TPen;
  var borders: TCellBorders);
begin
  if FColumnCollection.Count > Acol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      Borders := TGridColumnItem(FColumnCollection.Items[ACol]).Borders;
      APen.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).BorderPen);
    end;
  end;
  inherited;
end;

procedure TAdvColumnGrid.GetCellPrintBorder(ACol, ARow: integer; APen: TPen;
  var borders: TCellBorders);
begin
  if FColumnCollection.Count > Acol then
  begin
    if TGridColumnItem(FColumnCollection.Items[ACol]).PrintBorderCustom then
    begin
      if (ACol >= FixedCols) and (Arow >= FixedRows) then
      begin
        Borders := TGridColumnItem(FColumnCollection.Items[ACol]).PrintBorders;
        APen.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).PrintBorderPen);
      end;
    end;
  end;
  inherited;
end;

procedure TAdvColumnGrid.GetCellPrintColor(ACol, ARow: integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
var
  Clr: TColor;
  cp: TCellProperties;
begin
  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) and
       (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
       (ARow < RowCount - FixedFooters) then
    begin
      if TGridColumnItem(FColumnCollection.Items[ACol]).Fixed then
        ABrush.Color := FixedColor
      else
      begin
        if not (TGridColumnItem(FColumnCollection.Items[ACol]).ShowBands and Bands.Active and Bands.Print) then
          ABrush.Color := TGridColumnItem(FColumnCollection.Items[ACol]).PrintColor;

        if HasCellProperties(ACol, ARow) then
        begin
          Clr := CellProperties[ACol,ARow].BrushColor;
          if Clr <> clNone then
            ABrush.Color := Clr;
        end;
      end;

      if TGridColumnItem(FColumnCollection.Items[ACol]).PrintFontCustom then
      begin
        AFont.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).PrintFont);
        if HasCellProperties(ACol,ARow) then
        begin
          cp := CellProperties[ACol, ARow];

          if cp.FontColor <> clNone then
            AFont.Color := cp.FontColor;

          if cp.FontStyle <> [] then
            AFont.Style := cp.FontStyle;

          if cp.FontSize <> 0 then
            AFont.Size := cp.FontSize;

          if cp.FontName <> '' then
            AFont.Name := cp.FontName;
        end;
      end
      else
        AFont.Assign(PrintSettings.Font);

    end
    else
    begin
      AFont.Assign(PrintSettings.FixedFont);
    end;
  end;
  inherited;
end;

procedure TAdvColumnGrid.GetCellColor(ACol, ARow: integer;
  AState: TGridDrawState; ABrush: TBrush; AFont: TFont);
var
  Clr: TColor;
begin
  if FColumnCollection.Count > Acol then
  begin
    if (ACol >= FixedCols) and
       (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
       (ARow < RowCount - FixedFooters) then
    begin
      if TGridColumnItem(FColumnCollection.Items[ACol]).Fixed then
        ABrush.Color := FixedColor
      else
        if not (TGridColumnItem(FColumnCollection.Items[ACol]).ShowBands and Bands.Active) and (ARow >= FixedRows) then
          ABrush.Color := TGridColumnItem(FColumnCollection.Items[ACol]).Color;

      if ARow < FixedRows then
        AFont.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).HeaderFont)
      else
        AFont.Assign(TGridColumnItem(FColumnCollection.Items[ACol]).Font);

      if HasCellProperties(ACol, ARow) then
      begin
        Clr := CellProperties[ACol,ARow].BrushColor;
        if Clr <> clNone then
          ABrush.Color := Clr;

        Clr := CellProperties[ACol,ARow].FontColor;
        if Clr <> clNone then
          AFont.Color := Clr;

      end;
    end;
  end;

  inherited;
end;

function TAdvColumnGrid.HasColumnsProp: boolean;
begin
  Result := true;
end;

function TAdvColumnGrid.HasCombo(ACol,ARow: Integer; AEditor: TEditorType = edNone): Boolean;
begin
  Result := False;
  if FColumnCollection.Count > ACol then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      Result := (TGridColumnItem(FColumnCollection.Items[acol]).Editor in [edComboEdit,edComboList]);
    end;
  end;
  if Assigned(OnHasComboBox) then
    OnHasComboBox(Self,ACol,ARow,Result);
end;

procedure TAdvColumnGrid.GetCellEditor(ACol, ARow: integer;
  var AEditor: TEditorType);
begin

  if (FColumnCollection.Count > ACol) and (ACol >= 0) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      AEditor := TGridColumnItem(FColumnCollection.Items[ACol]).Editor;

      if not (csDesigning in ComponentState) and Assigned(ComboBox) then
      begin
        EditLink := TGridColumnItem(FColumnCollection.Items[ACol]).EditLink;
      end;
    end;
    if Columns[ACol].FloatFormat <> '' then
      FloatFormat := Columns[ACol].FloatFormat;
  end;

  inherited;
end;

procedure TAdvColumnGrid.GetCellFixed(ACol, ARow: integer;
  var IsFixed: boolean);
begin
  if FColumnCollection.Count > RealColIndex(Acol) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) then
    begin
      IsFixed := TGridColumnItem(FColumnCollection.Items[RealColIndex(ACol)]).Fixed;
    end;
  end;
  inherited;
end;

function TAdvColumnGrid.GetCellFloatFormat(ACol, ARow: Integer): string;
begin
  Result := FloatFormat;

  if (ACol < Columns.Count) and (Columns[ACol].FloatFormat <> '') then
    Result := Columns[ACol].FloatFormat;
end;

procedure TAdvColumnGrid.GetCellPassword(ACol, ARow: integer;
  var IsPassword: boolean);
begin
  if FColumnCollection.Count > Acol then
  begin
    IsPassword := TGridColumnItem(FColumnCollection.Items[ACol]).Password;
  end;
  inherited;
end;

procedure TAdvColumnGrid.GetCellReadOnly(ACol, ARow: integer;
  var IsReadOnly: boolean);
var
  BC: TPoint;
begin
  IsReadOnly := False;


  if not (csLoading in ComponentState) then
  begin
    if FColumnCollection.Count > ACol then
    begin
      if (ACol >= FixedCols) and (ARow >= FixedRows) then
      begin
        IsReadOnly := not TGridColumnItem(FColumnCollection.Items[ACol]).ReadOnly and
                      not TGridColumnItem(FColumnCollection.Items[ACol]).Fixed;
      end;
    end;
  end;

  if (ARow < FixedRows) and MouseActions.CheckAllCheck then
  begin
    if HasCheckBox(ACol, ARow) then
      IsReadOnly := not TGridColumnItem(FColumnCollection.Items[ACol]).ReadOnly;
  end;

  BC := BaseCell(ACol,ARow);

  if HasCellProperties(ACol,ARow) and IsReadOnly then
  begin
    IsReadOnly := not (ReadOnly[ACol,ARow]);
  end;

  if Assigned(OnCanEditCell) then
    OnCanEditCell(Self,BC.Y,BC.X,IsReadOnly);
end;

function TAdvColumnGrid.GetColCount: integer;
begin
  Result := inherited ColCount;
end;

procedure TAdvColumnGrid.GetColFormat(ACol: integer;
  var ASortStyle: TSortStyle; var aPrefix, aSuffix: string);
begin
  if FColumnCollection.Count > ACol then
  begin
    ASortStyle := TGridColumnItem(FColumnCollection.Items[ACol]).SortStyle;
    APrefix := TGridColumnItem(FColumnCollection.Items[ACol]).SortPrefix;
    ASuffix := TGridColumnItem(FColumnCollection.Items[ACol]).SortSuffix;
  end;
  inherited;
end;

function TAdvColumnGrid.GetEditLimit: Integer;
begin
  Result := inherited GetEditLimit;
  if FColumnCollection.Count > Col then
  begin
    if Col >= FixedCols then
    begin
      Result := TGridColumnItem(FColumnCollection.Items[RealColIndex(Col)]).EditLength;
    end;
  end;
end;

function TAdvColumnGrid.GetEditMask(ACol, ARow: Integer): string;
var
  msk: string;
begin
  if FColumnCollection.Count > Acol then
  begin
    if ACol >= FixedCols then
    begin
      Result := TGridColumnItem(FColumnCollection.Items[RealColIndex(ACol)]).EditMask;
    end;
  end;
  msk := inherited GetEditMask(ACol,ARow);
  if msk <> '' then
    Result := msk;
end;

function TAdvColumnGrid.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvColumnGrid.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;

procedure TAdvColumnGrid.Loaded;
begin
  inherited;
  SynchColumns;
  Columns.SetOrganization;
end;

procedure TAdvColumnGrid.SetColCount(const Value: integer);
begin
  inherited ColCount := Value;
  FColumnCollection.FNoRecursiveUpdate := True;
  SynchColumns;
  FColumnCollection.FNoRecursiveUpdate := False;
end;

procedure TAdvColumnGrid.SetColumnCollection(const value :TGridColumnCollection);
begin
  FColumnCollection.Assign(Value);
end;

function TAdvColumnGrid.GetColumnCollection: TGridColumnCollection;
begin
  Result := FColumnCollection;
end;

procedure TAdvColumnGrid.SynchHeaders;
var
  i: integer;
begin
  if FixedRows > 0 then
  begin
    ColumnHeaders.Clear;

    for i := 1 to FColumnCollection.Count do
    begin
      //j := RealColIndex(i - 1);
      //if (j  < FColumnCollection.Count) then

      if (FColumnCollection.Items[i - 1].FColumnHeader = '') and (Cells[i - 1,0] <> '') then
        FColumnCollection.Items[i - 1].FColumnHeader := Cells[i - 1, 0];

      ColumnHeaders.Add(FColumnCollection.Items[i - 1].FColumnHeader);
      {
      FColumnCollection.Items[j].FColumnHeader := Cells[j,0];
      }
    end;
  end;
end;

{
procedure TAdvColumnGrid.SynchWidths;
var
  i,j: integer;
begin
  if FixedRows > 0 then
  begin
    for i := 1 to FColumnCollection.Count do
    begin
      j := RealColIndex(i - 1);

      ColWidths[j] := FColumnCollection.Items[j].Width;
    end;
  end;
end;
}

(*
procedure TAdvColumnGrid.SynchColMove;
var
  i,j: Integer;
begin
  while FColumnCollection.Count < ColCount + NumHiddenColumns do
  begin
    FColumnCollection.Add;
  end;

  while FColumnCollection.Count > ColCount + NumHiddenColumns do
  begin
    FColumnCollection.Items[FColumnCollection.Count - 1].Free;
  end;

  if csDesigning in ComponentState then
    for i := 1 to ColCount do
      Cells[i - 1,FixedRows] := 'Column ' + IntToStr(i - 1);

  if FixedRows > 0 then
  begin
    {
    for i := 1 to ColCount do
    begin
      j := RealColIndex(i - 1);
      FColumnCollection.Items[j].FColumnHeader := Cells[j,0];
    end;
    }
    ColumnHeaders.Clear;
    for i := 1 to ColCount do
    begin
      j := RealColIndex(i - 1);
      ColumnHeaders.Add(FColumnCollection.Items[j].FColumnHeader);
    end;
  end;
end;
*)

procedure TAdvColumnGrid.SynchColumns;
var
  i: Integer;
begin
  while FColumnCollection.Count < ColCount + NumHiddenColumns do
  begin
    FColumnCollection.Add;
  end;

  while FColumnCollection.Count > ColCount + NumHiddenColumns do
  begin
    FColumnCollection.Items[FColumnCollection.Count - 1].Free;
  end;

  if csDesigning in ComponentState then
    for i := 1 to ColCount do
      Cells[i - 1,FixedRows] := 'Column ' + IntToStr(i - 1);

  if FixedRows > 0 then
  begin
    {
    ColumnHeaders.Clear;
    for i := 1 to ColCount do
    begin
      j := RealColIndex(i - 1);
      FColumnCollection.Items[j].FColumnHeader := Cells[j,0];
    end;
    }
  end;
end;



procedure TAdvColumnGrid.SaveColumnsToStream(st: TStream);
var
  gcio: TAdvColumnGridIO;
begin
  gcio := TAdvColumnGridIO.Create(self);
  gcio.Items.FNorecursiveUpdate := True;
  gcio.Items.Assign(self.Columns);
  st.Writecomponent(gcio);
  gcio.Free;
end;

procedure TAdvColumnGrid.LoadColumnsFromStream(st: TStream);
var
  gcio: TAdvColumnGridIO;
begin
  Columns.FNoRecursiveUpdate := True;

  gcio := TAdvColumnGridIO.Create(Self);
  gcio.Items.FNorecursiveUpdate := True;
  st.ReadComponent(gcio);
  Self.Columns.Assign(gcio.Items);
  gcio.Free;
  Columns.FNoRecursiveUpdate := False;
  Invalidate;
end;



procedure TAdvColumnGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent is TEditLink) then
      for i := 1 to Columns.Count do
        if Columns.Items[i - 1].EditLink = AComponent then
          Columns.Items[i - 1].EditLink := nil;

    if (AOperation = opRemove) and (AComponent is TPopupMenu) then
      for i := 1 to Columns.Count do
        if Columns.Items[i - 1].ColumnPopup = AComponent then
          Columns.Items[i - 1].ColumnPopup := nil;
  end;
end;

function TAdvColumnGrid.GetColumnByName(AValue: string): TGridColumnItem;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while i < Columns.Count do
  begin
    if Columns.Items[i].Name = AValue then
    begin
      Result := Columns.Items[i];
      Break;
    end;
    inc(i);
  end;
end;

procedure TAdvColumnGrid.GetDefaultProps(ACol, ARow: Integer; AFont: TFont;
  ABrush: TBrush; var AColorTo,AMirrorColor,AMirrorColorTo: TColor; var HA: TAlignment; var VA: TVAlignment;
  var WW: boolean; var GD: TCellGradientDirection);
begin
  if ACol < Columns.Count then
  begin
    AFont.Assign(Columns[ACol].Font);
    ABrush.Color := Columns[ACol].Color;
    AColorTo := clNone;

    if ARow < FixedRows then
      HA := Columns[ACol].HeaderAlignment
    else
      HA := Columns[ACol].Alignment;

    VA := VAlignment;
    WW := WordWrap;
    GD := GradientVertical;

    if IsFixed(Acol,ARow) then
    begin
      ABrush.Color := clNone;
      AColorTo := clNone;
    end;

    if Bands.Active and Columns[ACol].ShowBands then
    begin
      ABrush.Color := clNone;
      AColorTo := clNone;
      {
      if Bands.TotalLength > 0 then
      begin
        if (((ARow - FixedRows) mod Bands.TotalLength) < Bands.PrimaryLength) then
          ABrush.Color := Bands.PrimaryColor
        else
          ABrush.Color := Bands.SecondaryColor;
      end;
      }
    end;
  end
  else
    inherited;
end;

procedure TAdvColumnGrid.CellsLoaded;
begin
  inherited;
  Columns.FNoRecursiveUpdate := True;
  SynchColumns;
  Columns.FNoRecursiveUpdate := False;
end;

function TAdvColumnGrid.GetCellGraphic(ACol, ARow: Integer): TCellGraphic;
begin
  Result := inherited GetCellGraphic(ACol, ARow);

  if (csDestroying in ComponentState) then
    Exit;
  if not Assigned(FColumnCollection) then
    Exit;

  if (FColumnCollection.Count > Acol) and (Result = nil) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) and
       (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
       (ARow < RowCount - FixedFooters) then
    begin
      if TGridColumnItem(FColumnCollection.Items[ACol]).Editor = edDataCheckBox then
      begin
        FCellGraphic.CellType := ctVirtCheckBox;
        FCellGraphic.CellTransparent := ControlLook.ControlStyle = csFlat;
        Result := FCellGraphic;
      end;
    end;
  end;
end;

function TAdvColumnGrid.GetCellType(ACol, ARow: Integer): TCellType;

begin
  Result := inherited GetCellType(ACol, ARow);

  if (csDestroying in ComponentState) then
    Exit;
    
  if Assigned(FColumnCollection) then
  begin
    if (FColumnCollection.Count > Acol) and (Result = ctEmpty) then
    begin
      if (ACol >= FixedCols) and (Arow >= FixedRows) and
         (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
         (ARow < RowCount - FixedFooters) then
      begin
        if TGridColumnItem(FColumnCollection.Items[ACol]).Editor = edDataCheckBox then
        begin
          Result := ctDataCheckBox;
        end;
      end;
    end;
  end;
end;

function TAdvColumnGrid.GetCheckFalse(ACol,ARow: Integer): string;
begin
  Result := inherited GetCheckFalse(ACol,ARow);

  if (FColumnCollection.Count > ACol) then
  begin
    if (ACol >= FixedCols) and (ARow >= FixedRows) and
       (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
       (ARow < RowCount - FixedFooters) then
    begin
      Result := TGridColumnItem(FColumnCollection.Items[ACol]).CheckFalse;
    end;
  end;
end;

function TAdvColumnGrid.GetCheckTrue(ACol,ARow: Integer): string;
begin
  Result := inherited GetCheckTrue(ACol,ARow);

  if (FColumnCollection.Count > Acol) then
  begin
    if (ACol >= FixedCols) and (Arow >= FixedRows) and
       (ACol < ColCount - FixedRightCols + NumHiddenColumns) and
       (ARow < RowCount - FixedFooters) then
    begin
      Result := TGridColumnItem(FColumnCollection.Items[ACol]).CheckTrue;
    end;
  end;
end;

procedure TAdvColumnGrid.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R,C: Integer;
  CR: TRect;
  GC: TGridColumnItem;
  PT: TPoint;

begin
  inherited;
  MouseToCell(X,Y,C,R);
(*
  RC := RealColIndex(C);
  if (R = 0) and (RC <> FilterColumn) then
  begin
    CR := CellRect(C,R);

    if (X > CR.Right - 16) and (Y < CR.Top + 19) and
       (X < CR.Right - 3) and (FColumnCollection.Items[RC].Filter.Count > 0) then
    begin
      FilterColumn := RC;
      DropList.Color := FilterDropDown.Color;
      DropList.Font.Assign(FilterDropDown.Font);
      DropList.Width := FilterDropDown.Width;
      DropList.Height := FilterDropDown.Height;

      if FilterDropDown.ColumnWidth then
        DropList.Width := CR.Right - CR.Left;

      DropList.Top := CR.Top + 16;
      DropList.Left := CR.Left;
      DropList.Parent := Self;
      DropList.Visible := True;
      DropList.SetFocus;
      DropList.Ctl3D := False;
      DropList.Items.Clear;

      for I := 1 to FColumnCollection.Items[RC].Filter.Count do
      begin
        VP := Pos('=', FColumnCollection.Items[RC].Filter.Strings[I - 1]);
        if VP > 0 then
          DropList.Items.Add(Copy(FColumnCollection.Items[RC].Filter.Strings[I - 1],1,VP - 1))
        else
          DropList.Items.Add(FColumnCollection.Items[RC].Filter.Strings[I - 1]);
        if I = 1 then
          DropList.ItemIndex := 0;
      end;
    end
    else
    begin
      if DropList.Visible then
        DropList.Visible := false;
      FilterColumn := -1;
    end;
  end
  else
  begin
    if DropList.Visible then
      DropList.Visible := false;
    FilterColumn := -1;
  end;
  *)

  if (R <> -1) and (C <> -1) and (C < FColumnCollection.Count) then
  begin
    GC := FColumnCollection.Items[C];
    CR := CellRect(C,R);

    PT := ClientToScreen(Point(CR.Left,CR.Bottom));

    if (Button = mbLeft) and Assigned(GC.ColumnPopup) and
       (GC.ColumnPopupType in [cpFixedCellsLClick,cpNormalCellsLClick,cpAllCellsLClick]) then
    begin
      if Assigned(FOnColumnPopup) then
        FOnColumnPopup(Self,C,R,GC.ColumnPopup);

      if (R < FixedRows) and (GC.ColumnPopupType in [cpFixedCellsLClick,cpAllCellsLClick]) then
      begin
        GC.ColumnPopup.Popup(PT.X,PT.Y);
      end;

      if (R >= FixedRows) and (GC.ColumnPopupType in [cpNormalCellsLClick,cpAllCellsLClick]) then
      begin
        GC.ColumnPopup.Popup(PT.X,PT.Y);
      end;
    end;

    if (Button = mbRight) and Assigned(GC.ColumnPopup) and
       (GC.ColumnPopupType in [cpFixedCellsRClick,cpNormalCellsRClick,cpAllCellsRClick]) then
    begin
      if Assigned(FOnColumnPopup) then
        FOnColumnPopup(Self,C,R,GC.ColumnPopup);

      if (R < FixedRows) and (GC.ColumnPopupType in [cpFixedCellsRClick,cpAllCellsRClick]) then
      begin
        GC.ColumnPopup.Popup(PT.X,PT.Y);
      end;

      if (R >= FixedRows) and (GC.ColumnPopupType in [cpNormalCellsRClick,cpAllCellsRClick]) then
      begin
        GC.ColumnPopup.Popup(PT.X,PT.Y);
      end;
    end;
  end;
end;

function TAdvColumnGrid.GetFilter(ACol: Integer; Disp: boolean = false): Boolean;
var
  i: integer;
  filterString : string;
  filterIndex  : integer;
begin
  FilterList.Sorted := false;

  inherited GetFilter(ACol);

  Result := FilterList.Count >  0;

  if not FilterDropDownAuto then
  begin
    if (FColumnCollection.Count > ACol) then
    begin
      Result := FColumnCollection.Items[ACol].Filter.Count > 0;
      if Result then
      begin
        FilterList.Clear;
        for i := 0 to FColumnCollection.Items[ACol].Filter.Count - 1 do
        begin
          //DropList.Items.Add(FColumnCollection.Items[ACol].Filter.Strings[i]);
          filterString := FColumnCollection.Items[ACol].Filter.Strings[i];
          filterIndex  := Pos('=', filterString );
          if filterIndex > 0 then
            filterString := Copy( filterString, 1, filterIndex - 1 );

          FilterList.Add(filterString);
        end;
      end;
    end
    else
      Result := False;
  end;
end;

procedure TAdvColumnGrid.UpdateColSize(ACol: Integer;
  var NewWidth: Integer);
var
  RCol: integer;
begin
  RCol := RealColIndex(ACol);

  if (FColumnCollection.Count > RCol) and (RCol >= 0) then
  begin
    if (FColumnCollection.Items[RCol].MinSize > 0) then
    begin
      if NewWidth < FColumnCollection.Items[RCol].MinSize then
        NewWidth := FColumnCollection.Items[RCol].MinSize;
    end;

    if (FColumnCollection.Items[RCol].MaxSize > 0) then
    begin
      if NewWidth > FColumnCollection.Items[RCol].MaxSize then
        NewWidth := FColumnCollection.Items[RCol].MaxSize;
    end;
  end;
  inherited;

end;

procedure TAdvColumnGrid.UpdateAutoColSize(ACol: Integer;
  var NewWidth: Integer);
var
  RCol: integer;
begin
  RCol := RealColIndex(ACol);

  if (FColumnCollection.Count > RCol) and (RCol >= 0) then
  begin
    if (FColumnCollection.Items[RCol].AutoMinSize > 0) then
    begin
      if NewWidth < FColumnCollection.Items[RCol].AutoMinSize then
        NewWidth := FColumnCollection.Items[RCol].AutoMinSize;
    end;

    if (FColumnCollection.Items[RCol].AutoMaxSize > 0) then
    begin
      if NewWidth > FColumnCollection.Items[RCol].AutoMaxSize then
        NewWidth := FColumnCollection.Items[RCol].AutoMaxSize;
    end;
  end;
  inherited;
end;

procedure TAdvColumnGrid.FilterSelect(Sender: TObject; ItemIndex: Integer);
var
  F,FN: string;
begin
  if (FilterColumn >= 0) and (FilterColumn < FColumnCollection.Count) then
  begin
    GetFilter(FilterColumn);

    if (ItemIndex >= 0) and (ItemIndex < FilterList.Count) then
    begin
      F := FilterList.Strings[ItemIndex];

      if Pos('=',F) > 0 then
        FN := Copy(F,1,Pos('=',F)-1)
      else
        FN := F;

      if FAutoFilterDisplay then
        FColumnCollection.Items[FilterColumn].Header := FN;

      if AutoFilterUpdate then
        FilterActive := False;

      if Pos('=',F) > 0 then
        F := Copy(F,Pos('=',F)+1,Length(F));

      if Assigned(OnFilterSelect) then
        OnFilterSelect(Self,FilterColumn,ItemIndex,FN,F);

      Filter.ColumnFilter[FilterColumn].Condition := F;
      Filter.ColumnFilter[FilterColumn].CaseSensitive :=
        FColumnCollection.Items[FilterColumn].FilterCaseSensitive;

      if AutoFilterUpdate then
      begin
        FilterActive := True;

        if not ((RowCount = FixedRows) or ((RowCount = 1) and FixedRowAlways)) then
          Row := FixedRows;

        DoFilterDone;
      end;
    end;
  end;

  FilterColumn := -1;
end;

procedure TAdvColumnGrid.UpdateColHeaders;
var
  i: Integer;
begin
  for i := 1 to Columns.Count do
  begin
    if i < ColumnHeaders.Count then
    begin
      Columns.Items[i - 1].FColumnHeader := ColumnHeaders.Strings[i - 1];
    end;
  end;
end;

procedure TAdvColumnGrid.LoadColPositions;
begin
  if ColumnSize.Location = clRegistry then
    inherited LoadColPositions
  else
    LoadColumnPositions(ColumnSize.Key, ColumnSize.Section);
end;

procedure TAdvColumnGrid.LoadColumnPositions(Key, Section: string);
var
  IniFile: TIniFile;
  i,j: Integer;
  il: TIntList;
begin

  IniFile := TIniFile.Create(Key);

  // skip when there are invalid entries in the INI file
  for i := 1 to Columns.Count do
  begin
    if IniFile.ReadInteger(Section,'CP'+IntToStr(i - 1),-1) = - 1 then
      Exit;
  end;

  ResetColumnOrder;

  il := TIntList.Create(-1,-1);

  for i := 1 to Columns.Count do
  begin
    j := IniFile.ReadInteger(Section,'CP'+IntToStr(i - 1),0);
    Columns[j].DefIdx := i - 1;
    il.Add(j);
  end;
  IniFile.Free;

  Columns.ResetOrganization;

  for i := 0 to Columns.Count - 1 do
  begin
    Columns[i].DefIdx := il.Items[i];
  end;

  il.Free;
end;

procedure TAdvColumnGrid.SaveColPositions;
begin
  if ColumnSize.Location = clRegistry then
    inherited SaveColPositions
  else
    SaveColumnPositions(ColumnSize.Key, ColumnSize.Section);
end;

procedure TAdvColumnGrid.SaveColumnPositions(Key, Section: string);
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(Key);

  for i := 1 to Columns.Count do
  begin
    IniFile.WriteInteger(Section,'CP'+IntToStr(i - 1),Columns[i - 1].DefIdx);
  end;
  IniFile.Free;
end;

function TAdvColumnGrid.CreateColumns: TGridColumnCollection;
begin
  Result := TGridColumnCollection.Create(Self);
end;

function TAdvColumnGrid.GetFormattedCell(ACol, ARow: Integer): string;
var
  fmt: string;
  IsFloat: Boolean;
begin
  if (FColumnCollection.Count > ACol) then
  begin
    fmt := TGridColumnItem(FColumnCollection.Items[ACol]).FloatFormat;

    IsFloat := IsType(Cells[ACol,ARow]) in [atNumeric,atFloat];

    if Assigned(OnGetFloatFormat) then
      OnGetFloatFormat(Self,ACol,ARow,IsFloat,Fmt);

    if (fmt <> '') and IsFloat then
    begin
      Result := Format(fmt,[Floats[ACol,ARow]]);
    end
    else
      Result := Cells[ACol,ARow];

  end
  else
    Result := inherited GetFormattedCell(ACol,ARow);
end;

procedure TAdvColumnGrid.InsertCols(ColIndex, CCount: Integer);
var
  i: integer;
begin
  inherited;

  Columns.FNoRecursiveUpdate := true;
  
  for i := 1 to CCount do
    Columns.Insert(ColIndex);

  Columns.FNoRecursiveUpdate := false;
end;

procedure TAdvColumnGrid.RemoveCols(ColIndex, CCount: Integer);
var
  i: integer;
begin
  inherited;
  for i := 1 to CCount do
    Columns.Delete(ColIndex);
end;

procedure TAdvColumnGrid.Group(Colindex: Integer);
begin
  // save grouping column info
  FSaveCol.Assign(Columns[ColIndex]);
  inherited;
end;

procedure TAdvColumnGrid.UnGroup;
var
  grp: Integer;
begin
  grp := self.GroupColumn;

  inherited;
  if grp <> -1 then
    Columns[grp].Assign(FSaveCol);
end;

{ TAdvColumnGridIO }

constructor TAdvColumnGridIO.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TGridColumnCollection.Create(AOwner as TAdvColumnGrid);
end;

destructor TAdvColumnGridIO.Destroy;
begin
  FItems.Free;
  inherited;
end;



initialization
  {$IFDEF ISDELPHI}
  try
    Classes.RegisterClass(TAdvColumnGrid);
    Classes.RegisterClass(TGridColumnItem);
    Classes.RegisterClass(TGridColumnCollection);
  except
  end;  
  {$ENDIF}
end.
