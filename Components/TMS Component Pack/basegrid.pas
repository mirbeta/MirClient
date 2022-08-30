{***************************************************************************}
{ TBaseGrid component                                                       }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1996-2015                                          }
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

unit BaseGrid;

{$I TMSDEFS.INC}

{$DEFINE TMSUNICODE}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls,
  Grids, AdvObj
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

type
  TBaseGrid = class;

  TColumnCalcType = (acNONE,acSUM,acAVG,acCOUNT,acMIN,acMAX,acSPREAD,acCUSTOM,acDISTINCT,acSTDDEV);

  {$IFDEF DELPHIXE2_LVL}
  TIObjType = NativeUInt;
  {$ENDIF}

  {$IFNDEF DELPHIXE2_LVL}
  TIObjType = Integer;
  {$ENDIF}

  { TCellProperties }

  TCellProperties = class(TPersistent)
  private
    FOwnerGrid: TBaseGrid;
    FOwnerCol: Integer;
    FOwnerRow: Integer;
    FObject: TIObjType;
    FGraphicObject: TObject;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor;
    FBrushColorTo: TColor;
    FFontSize: Integer;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FCellSpanY: integer;
    FCellSpanX: integer;
    FBorderWidth: Integer;
    FBorderColor: TColor;
    FAlignment: TAlignment;
    FVAlignment: TVAlignment;
    FPaintID: Integer;
    FWordWrap: Boolean;
    FIsBaseCell: Boolean;
    FCalcType: TColumnCalcType;
    FCalcObject: TObject;
    FNodeLevel: Integer;
    FEditor: TEditorType;
    FFontName: string;
    FReadOnly: Boolean;
    FGradientDirection: TCellGradientDirection;
    FControl: TControl;
    FMergeCollaps: Boolean;
    procedure SetBrushColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetWordWrap(const Value: Boolean);
    function GetBaseCell(c, r: Integer): TPoint;
    procedure SetBrushColorTo(const Value: TColor);
    function GetCellSpanY: Integer;
  protected
    property BaseCell[c,r: Integer]: TPoint read GetBaseCell;
    property PaintID: Integer read FPaintID write FPaintID;
    property GraphicObject: TObject read FGraphicObject write FGraphicObject;
  public
    property CalcType: TColumnCalcType read FCalcType write FCalcType;
    property CalcObject: TObject read FCalcObject write FCalcObject;
    constructor Create(AOwner: TBaseGrid; ACol, ARow:integer);
    procedure Assign(Source: TPersistent); override;
    property MergeCollaps: boolean read FMergeCollaps write FMergeCollaps;
  published
    property IsBaseCell: Boolean read FIsBaseCell write FIsBaseCell;
    property CellSpanX: Integer read FCellSpanX write FCellSpanX;
    property CellSpanY: Integer read GetCellSpanY write FCellSpanY;
    property OwnerCol: Integer read FOwnerCol write FOwnerCol stored False;
    property OwnerRow: Integer read FOwnerRow write FOwnerRow stored False;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderWidth: integer read FBorderWidth write FBorderWidth;
    property BrushColor: TColor read FBrushColor write SetBrushColor;
    property BrushColorTo: TColor read FBrushColorTo write SetBrushColorTo;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property CellObject: TIObjType read FObject write FObject;
    property FontColor: TColor read FFontColor write FFontColor;
    property FontName: string read FFontName write FFontName;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property Editor: TEditorType read FEditor write FEditor;
    property GradientDirection: TCellGradientDirection read FGradientDirection write FGradientDirection;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property VAlignment: TVAlignment read FValignment write SetVAlignment;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property NodeLevel: Integer read FNodeLevel write FNodeLevel;
    property Control: TControl read FControl write FControl;
  end;

  TGetDisplWideTextEvent = procedure(Sender: TObject; ACol,ARow: Integer; var Value: widestring) of object;

  TUndoRedoItem = class(TCollectionItem)
  private
    FRow: Integer;
    FCol: Integer;
    FValue: string;
    FOrigValue: string;
    FSequenceStart: boolean;
    FSequenceStop: boolean;
  public
  published
    property Value: string read FValue write FValue;
    property OrigValue: string read FOrigValue write FOrigValue;
    property Col: Integer read FCol write FCol;
    property Row: Integer read FRow write FRow;
    property SequenceStart:boolean read FSequenceStart write FSequenceStart;
    property SequenceStop:boolean read FSequenceStop write FSequenceStop;
  end;

  TUndoRedoCollection = class(TCollection)
  private
  public
    constructor Create;
  published
  end;

  TAdvGridUndoRedoEvent = procedure(Sender: TObject; Grid: TBaseGrid; Col,Row: integer; OrigValue,NewValue: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridUndoRedo = class(TComponent)
  private
    FItems: TUndoRedoCollection;
    FGrid: TBaseGrid;
    FLevel: Integer;
    FMaxLevel: Integer;
    FOnUndo: TAdvGridUndoRedoEvent;
    FOnRedo: TAdvGridUndoRedoEvent;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterChange(ACol,ARow: Integer; OldValue, NewValue: string); virtual;
    procedure StartSequence; virtual;
    procedure StopSequence; virtual;
    property Level: Integer read FLevel write FLevel;
    property Items: TUndoRedoCollection read FItems write FItems;
    property Grid: TBaseGrid read FGrid write FGrid;
    procedure Undo;
    procedure Redo;
    procedure Reset;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
  published
    property MaxLevel: Integer read FMaxLevel write FMaxLevel;
    property OnUndo: TAdvGridUndoRedoEvent read FOnUndo write FOnUndo;
    property OnRedo: TAdvGridUndoRedoEvent read FOnRedo write FOnRedo;
  end;

  TGridSelectionStyle = (ssDefault, ssVista, ssWin7, ssOffice2007);

  THoverRowCell = (hcNormal, hcFixed, hcSelected);

  THoverRowCells = set of THoverRowCell;


  TCustomCellProps = record
    AState: TGridDrawState;
    Print: boolean;
    Select: boolean;
    Remap: boolean;
    ABrush: TBrush;
    AColorTo: TColor;
    AMirrorColor: TColor;
    AMirrorColorTo: TColor;
    AFont: TFont;
    HA: TAlignment;
    VA: TVAlignment;
    WW: Boolean;
    GD: TCellGradientDirection
   end; 

  TBaseGrid = class(TObjStringGrid)
  private
    { Private declarations }
    //FCustomCellProps: TCustomCellProps;
    FDefaultDrawing: Boolean;
    FGridLineWidth: Integer;
    FGridLineColor: TColor;
    FGridFixedLineColor: TColor;
    FPaintID: Integer;
    FCustomSelect: Boolean;
    FOnGetDisplText: TGetDisplTextEvent;
    FOnGetDisplWideText: TGetDisplWideTextEvent;
    FHideLastRow: Boolean;
    FNormalRowCount: Integer;
    FUndoRedo: TAdvGridUndoRedo;
    FWordWrap: boolean;
    FActiveRowShow: Boolean;
    FActiveRowColor: TColor;
    FActiveRowColorTo: TColor;
    FActiveRowMirrorColor: TColor;
    FActiveRowMirrorColorTo: TColor;
    FSelectionStyle: TGridSelectionStyle;
    FDefFont: TFont;
    FDefBrush: TBrush;
    function GetDefaultDrawing: boolean;
    procedure SetDefaultDrawing(const Value: boolean);
    function GetGridLineWidth: integer;
    procedure SetGridLineWidth(const Value: integer);
    procedure SetGridLineColor(const Value: TColor);
    procedure SetGridFixedLineColor(const Value: TColor);
    procedure SetObjectEx(c, r: integer; const Value: TObject);
    function GetObjectEx(c, r: integer): TObject;
    function GetCellEx(c, r: integer): String;
    procedure SetCellEx(c, r: integer; const Value: String);
    function GetGraphicObjectEx(c, r: Integer): TObject;
    procedure SetGraphicObjectEx(c, r: Integer; const Value: TObject);
    procedure RepaintFixedMergedCols;
    procedure RepaintFixedMergedRows;
    function GetGridObject(c, r: Integer): TObject;
    procedure SetGridObject(c, r: Integer; const Value: TObject);
    function GetGridCell(c, r: Integer): string;
    procedure SetGridCell(c, r: Integer; const Value: string);
    procedure SetUndoRedo(const Value: TAdvGridUndoRedo);
    procedure SetActiveRowColor(const Value: TColor);
    procedure SetActiveRowColorTo(const Value: TColor);
    procedure SetActiveRowMirrorColor(const Value: TColor);
    procedure SetActiveRowMirrorColorTo(const Value: TColor);
    procedure SetSelectionStyle(const Value: TGridSelectionStyle);
    procedure SetActiveRowShow(const Value: Boolean);
    //function GetRowsEx(r: integer): TStrings;
    //procedure SetRowsEx(r: integer; const Value: TStrings);
  protected
    FHoverRowIdx: integer;
    FHoverRow: boolean;
    FHoverRowCells: THoverRowCells;
    FHoverRowColor: TColor;
    FHoverRowColorTo: TColor;
    FShowSel: Boolean;
    FHasCellProps: Boolean;
    FMergeCount: integer;
    FEditCellSet: boolean;
    function GetCPGraphicObject(cp: TCellProperties): TObject;
    { Protected declarations }
    {$IFNDEF DELPHI6_LVL}
    procedure SetGridOrientation(RightToLeftOrientation: Boolean);
    {$ENDIF}
    procedure CopyRows(ARow1, ARow2: Integer);
    procedure CopyCols(ACol1, ACol2: Integer);
    procedure NilRow(ARow: Integer);
    procedure NilCol(ACol: Integer);
    procedure NilCell(ACol,ARow: Integer);
    procedure SelectBaseCell;
    function IsFixed(ACol, ARow: Integer): Boolean; virtual;
    function IsSelected(ACol, ARow: Integer): Boolean;
    function NodeIndent(ARow: Integer): Integer; virtual;
    function HasNodes: Boolean; virtual;
    function FixedColsWidth: Integer;
    function FixedRowsHeight: Integer;
    procedure TopLeftChanged; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawGridCell(Canvas: TCanvas; ACol, ARow:longint; ARect:TRect; AState:TGridDrawState); virtual;
    function HasCellProperties(ACol, ARow:integer): Boolean;
    procedure ClearProps;
    procedure ClearPropCell(ACol,ARow: Integer);
    procedure ClearPropRow(ARow: Integer);
    procedure ClearPropRect(ACol1,ARow1,ACol2,ARow2: Integer);
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
    procedure FloatFooterUpdate; virtual;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    function IsBaseCellEx(ACol,ARow: Integer;var MCol,MRow: Integer): Boolean;
    function GetCellRect(c, r: Integer): TRect;
    property PaintID: Integer read FPaintID;
    property CustomSelect: Boolean read FCustomSelect write FCustomSelect;
    //property RowProperties[r:integer]: TCellProperties read GetRowProperties write SetRowProperties;
    //property ColumnProperties[c:integer]: TCellProperties read GetColProperties write SetColProperties;
    property HideLastRow: Boolean read FHideLastRow write FHideLastRow;
    property NormalRowCount: Integer read FNormalRowCount write FNormalRowCount;
    property GridObjects[c,r: Integer]: TObject read GetGridObject write SetGridObject;
    property EditCellSet: boolean read FEditCellSet;
    procedure GetDisplText(c,r: Integer; var Value: string); virtual;
    procedure GetDisplWideText(c,r: Integer; var Value: widestring); virtual;
    procedure GetDefaultProps(ACol,ARow: Integer; AFont: TFont; ABrush: TBrush; var AColorTo, AMirrorColor, AMirrorColorTo: TColor;
      var HA: TAlignment; var VA: TVAlignment; var WW: boolean; var GD: TCellGradientDirection); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ChangeScale(M, D: Integer); override;
    procedure CellTextChange(ACol,ARow: integer); virtual;
    //property CustomCellProps: TCustomCellProps read FCustomCellProps;
    function HasCustomCellBorder:boolean; virtual;
    procedure PaintCellInt(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState; Internal: boolean = true);
    function UseVCLStyles: boolean; virtual;
    function CreateCellProperties(Col,Row: integer): TCellProperties;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintCell(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    procedure PaintCellExt(ACanvas: TCanvas; ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
    function GetCellProperties(c, r: integer): TCellProperties;
    procedure SetCellProperties(c, r: integer; const Value: TCellProperties);
    property CellProperties[c,r: Integer]: TCellProperties read GetCellProperties write SetCellProperties;
    property SelectionStyle: TGridSelectionStyle read FSelectionStyle write SetSelectionStyle;
    procedure GetVisualProperties(ACol,ARow: Integer; var AState: TGridDrawState; Print, Select, Remap: Boolean;
      ABrush: TBrush; var AColorTo, AMirrorColor, AMirrorColorTo: TColor; AFont: TFont; var HA: TAlignment; var VA: TVAlignment; var WW: Boolean;
      var GD: TCellGradientDirection); virtual;
    function CellRect(c,r:Integer): TRect;
    function CellSize(c,r: Integer): TPoint;
    function RealColIndex(ACol: Integer): Integer; virtual;
    function DisplColIndex(ACol: Integer): Integer; virtual;
    function IsBaseCell(ACol,ARow: Integer): Boolean;
    function IsMergedCell(ACol,ARow: Integer): Boolean;
    function IsMergedNonBaseCell(ACol,ARow: Integer): Boolean;
    function IsXMergedCell(ACol,ARow: Integer): Boolean;
    function IsYMergedCell(ACol,ARow: Integer): Boolean;
    function BaseCell(ACol,ARow: Integer): TPoint;
    function FullCell(c,r: Integer): TRect;
    function CellSpan(ACol,ARow: Integer): TPoint;
    function CellExt(ACol,ARow: Integer): TPoint;
    function MinRowSpan(ARow: Integer): Integer;
    function MaxRowSpan(ARow: Integer): Integer;
    function RowSpanIdentical(ARow1,ARow2: Integer): Boolean;
    function ColSpanIdentical(ACol1,ACol2: Integer): Boolean;
    procedure MergeCells(c,r,spanx,spany: Integer); virtual;
    procedure SplitCells(c,r: Integer); virtual;
    procedure RepaintCell(c,r: Integer);
    function TotalColCount: Integer; virtual;
    property ActiveRowColorTo: TColor read FActiveRowColorTo write SetActiveRowColorTo default clNone;
    property ActiveRowMirrorColor: TColor read FActiveRowMirrorColor write SetActiveRowMirrorColor default clNone;
    property ActiveRowMirrorColorTo: TColor read FActiveRowMirrorColorTo write SetActiveRowMirrorColorTo default clNone;

    property Objects[c,r: Integer]: TObject read GetObjectEx write SetObjectEx;
    property GraphicObjects[c,r: Integer]: TObject read GetGraphicObjectEx write SetGraphicObjectEx;
    property Cells[c,r: Integer]: String read GetCellEx write SetCellEx;
    property GridCells[c,r: Integer]: string read GetGridCell write SetGridCell;
    property WordWrap: boolean read FWordWrap write FWordWrap;
    //property Rows[r: Integer]: TStrings read GetRowsEx write SetRowsEx;
  published
    { Published declarations }
    property ActiveRowShow: Boolean read FActiveRowShow write SetActiveRowShow default False;
    property ActiveRowColor: TColor read FActiveRowColor write SetActiveRowColor default clInfoBk;
    property DefaultDrawing: Boolean read GetDefaultDrawing write SetDefaultDrawing default False;
    property GridLineWidth: Integer read GetGridLineWidth write SetGridLineWidth default 1;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor default clSilver;
    property GridFixedLineColor: TColor read FGridFixedLineColor write SetGridFixedLineColor default clGray;
    property HoverRow: boolean read FHoverRow write FHoverRow default false;
    property HoverRowColor: TColor read FHoverRowColor write FHoverRowColor default clInfoBk;
    property HoverRowColorTo: TColor read FHoverRowColorTo write FHoverRowColorTo default clNone;
    property HoverRowCells: THoverRowCells read FHoverRowCells write FHoverRowCells default [];
    property OnGetDisplText: TGetDisplTextEvent read FOnGetDisplText write FOnGetDisplText;
    property OnGetDisplWideText: TGetDisplWideTextEvent read FOnGetDisplWideText write FOnGetDisplWideText;
    property UndoRedo: TAdvGridUndoRedo read FUndoRedo write SetUndoRedo;
  end;

implementation

uses
  AdvUtil;

const
  GRADIENT_DIRECTION = false;

{ TBaseGrid }

constructor TBaseGrid.Create(AOwner: TComponent);
begin
  inherited;

  GridLineWidth := 1;
  FGridLineColor := clSilver;
  FGridFixedLineColor := clGray;
  DefaultDrawing := False;
  CustomSelect := True;
  FHideLastRow := False;
  FActiveRowColor := clInfoBk;
  FActiveRowShow := false;
  FWordWrap := true;
  FPaintID := 1;
  FShowSel := true;
  FMergeCount := 0;
  FHoverRow := false;
  FHoverRowIdx := -1;
  FHoverRowColor := clInfoBk;
  FHoverRowColorTo := clNone;
  FActiveRowColorTo := clNone;
  FActiveRowMirrorColor := clNone;
  FActiveRowMirrorColorTo := clNone;
  FHoverRowCells := [hcNormal, hcSelected];
  FDefFont := TFont.Create;
  FDefBrush := TBrush.Create;
end;

destructor TBaseGrid.Destroy;
begin
  FDefFont.Free;
  FDefBrush.Free;
  ClearProps;
  inherited;
end;

procedure TBaseGrid.Paint;
begin
  inherited;
  inc(FPaintID);
end;

procedure TBaseGrid.DrawGridCell(Canvas:TCanvas; ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
begin
  DrawText(Canvas.Handle,PChar(Cells[ACol,ARow]),Length(Cells[ACol,ARow]),ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

procedure TBaseGrid.PaintCell(ACanvas: TCanvas; ACol: Integer; ARow: Integer; ARect: TRect; AState: TGridDrawState);
begin
  PaintCellInt(ACanvas, ACol, ARow, ARect, AState, true);
end;

procedure TBaseGrid.PaintCellExt(ACanvas: TCanvas; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
begin
  PaintCellInt(ACanvas, ACol, ARow, ARect, AState, false);
end;

procedure TBaseGrid.PaintCellInt(ACanvas: TCanvas; ACol: Integer; ARow: Integer; ARect: TRect; AState: TGridDrawState; internal: boolean = true);
var
  hrgn: THandle;
  CrL,CrT: Integer;
  OutofBounds: Boolean;
  MCol,MRow: Integer;
  HA: TAlignment;
  VA: TVAlignment;
  WW: Boolean;
  GLW: Integer;
  AColorTo,AMirrorColor,AMirrorColorTo: TColor;
  GD: TCellGradientDirection;
  mode: TGradientPart;
  ls,rs,ms,rc: integer;
  SR: TRect;
  dosel,dofix: boolean;

begin
  hrgn := 0;
  // Leave drawing of the last row to the floating last fixed row
  if (ARow = RowCount - 1) and FHideLastRow then
  begin
    ACanvas.Brush.Color := Color;
    ACanvas.Pen.Color := Color;
    ACanvas.Rectangle(ARect.Left,ARect.Top, ARect.Right ,ARect.Bottom);
    Exit;
  end;

  // compensate for hidden cols
  rc := RealColIndex(ACol);

  // Redirect painting to base cell
  if not IsBaseCellEx(rc,ARow,MCol,MRow) then
  begin
    if (CellProperties[MCol,MRow].PaintID <> FPaintID) then
    begin
      CellProperties[MCol,MRow].PaintID := FPaintID;
      PaintCell(ACanvas,MCol,MRow,ARect,AState);
    end;
    Exit;
  end;

  if IsFixed(ACol,ARow) and internal then
    AState := AState + [gdFixed] - [gdSelected];

  if Internal then
    ARect := CellRect(ACol,ARow);

  CrL := 0;
  CrT := 0;

  if (ACol >= FixedCols) then
    CrL := FixedColsWidth;

  if (ARow >= FixedRows) then
    CrT := FixedRowsHeight;

  OutOfBounds := ((ACol >= FixedCols) and (FixedCols > 0) and (ARect.Left < CrL)) or
                 ((ARow >= FixedRows) and (FixedRows > 0) and (ARect.Top < CrT));

  if OutOfBounds then
  begin
    hrgn := CreateRectRgn(CrL, CrT, Width,Height);
    SelectClipRgn(ACanvas.Handle, hrgn);
  end;

  ACanvas.Pen.Width := 1;
  ACanvas.Font.Assign(Font);

  // virtual method to get cell colors
  GetVisualProperties(ACol,ARow,AState,False,True,True,ACanvas.Brush,AColorTo,AMirrorColor,AMirrorColorTo,ACanvas.Font,HA,VA,WW,GD);

  ACanvas.Pen.Color := ACanvas.Brush.Color;

  dosel := (gdSelected in AState) and not (hcSelected in HoverRowCells);
  dofix := (gdFixed in AState) and not (hcFixed in HoverRowCells);

  // hover row color setting
  if (ARow = FHoverRowIdx) and FHoverRow and not dosel and not dofix then
  begin
    ACanvas.Brush.Color := FHoverRowColor;
    AColorTo := FHoverRowColorTo;
    AMirrorColor := clNone;
    AMirrorColorTo := clNone;
    GD := GradientVertical;
  end;

  // active row color has priority on hover row color
  if ((ActiveRowShow) and (ARow = Row) and
     ((ACol <> Col) or (not FShowSel)) and (ACol >= FixedCols)) or (not internal and (gdSelected in AState)) then
  begin
    ACanvas.Brush.Color := ActiveRowColor;
    //AColorTo := ActiveRowColor;
    //AMirrorColor := clNone;
    //AMirrorColorTo := clNone;
    AColorTo := ActiveRowColorTo;
    AMirrorColor := ActiveRowMirrorColor;
    AMirrorColorTo := ActiveRowMirrorColorTo;
  end;

  ACanvas.Pen.Color := ACanvas.Brush.Color;

  // if cell is selected and a special selection style is choosen
  if (gdSelected in AState) and (FSelectionStyle <> ssDefault) and FShowSel then
  begin
    ls := Selection.Left;
    rs := Selection.Right;
    if ls > rs then
    begin
      ms := rs;
      rs := ls;
      ls := ms;
    end;

    //if (ACol = ls) and (ACol = rs) then
      mode := gpFull;

    if (ACol = ls) and (ACol < rs) then
      mode := gpLeft;

    if (ACol = rs) and (ACol > ls) then
      mode := gpRight;

    if (ACol > ls) and (ACol < rs) then
      mode := gpMiddle;

    SR := ARect;

    if not (goHorzLine in Options) then
      SR.Bottom := SR.Bottom + 1;

    if not (goVertLine in Options) then
      SR.Right := SR.Right + 1;

    if HasCustomCellBorder and (FSelectionStyle = ssDefault) and FShowSel then
    begin
      SR.Left := SR.Left + 1;
      SR.Right := SR.Right - 1;
      SR.Bottom := SR.Bottom - 1;
      SR.Top := SR.Top + 1;
    end;

    if BiDiMode = bdRightToLeft then
    begin
      SR.Right := SR.Right + 1;
      SR.Bottom := SR.Bottom + 1;
    end;

    if (GetFocus = Handle) or (goDrawFocusSelected in Options) then
    begin
      case FSelectionStyle of
      ssVista:DrawSelectionGradient(ACanvas,$FDF8F1,$FCEFD5,clNone,clNone,$FDFBF6,$FDF5E7,$FDDE99,$FCEDCB,Color,SR,mode);
      ssWin7:DrawSelectionGradient(ACanvas,$FCEBDC,$FCDBC1,clNone,clNone,$FDF4EB,$FDEADB,$CEA27D,$E0C5AE,Color,SR,mode);
      ssOffice2007:DrawSelectionGradient(ACanvas,$D7FFFD,$58D4FC,$58D4FC,$B3F1FC,$F4FEFF,$CAF8FF,$8DD7D3,$C3D9DF,Color,SR,mode);
      end;
    end
    else
    begin
      DrawSelectionGradient(ACanvas,$F8F8F8,$E5E5E5,clNone,clNone,$FBFAFA,$F0F0F0,$D9D9D9,$EFEFEF,Color, SR,mode);
    end;
  end
  else
  begin
    if not UseVCLStyles or (not (gdFixed in AState) and not (gdSelected in AState)) then
    begin

      if (AMirrorColorTo <> clNone) then
        DrawVistaGradient(ACanvas,ARect,ACanvas.Brush.Color,AColorTo,AMirrorColor,AMirrorColorTo, true, clNone)
      else
        DrawGradient(ACanvas,ACanvas.Brush.Color,AColorTo,64,ARect,GD = GradientHorizontal)
    end;
  end;

  if gdFixed in AState then
  begin
    ACanvas.Pen.Color := FGridFixedLineColor;
    ACanvas.Pen.Width := 1;
    GLW := 1;
  end
  else
  begin
    if FGridLineWidth > 0 then
      ACanvas.Pen.Color := FGridLineColor;

    ACanvas.Pen.Width := FGridLineWidth;
    GLW := (FGridLineWidth + 1) shr 1;
  end;

  if (ACol = 0) and (ARow >= FixedRows) then
  begin
    //if NodeIndent(ARow) > 0 then
    //  ARect.Left := ColWidths[0] + 1;
    // ARect.Left := ARect.Left + NodeIndent(ARow);
  end;

  // Draw grid borders
  if not ((MCol = 0) and (ARow >= FixedRows) and HasNodes) then
  if ((goHorzLine in Options) and not (gdFixed in AState)) or
     ((goFixedHorzLine in Options) and (gdFixed in AState)) then
  begin
    ACanvas.MoveTo(ARect.Left - GLW + 1,ARect.Bottom - GLW);
    ACanvas.LineTo(ARect.Right - GLW + 1,ARect.Bottom - GLW);
  end;

  if ((goVertLine in Options) and not (gdFixed in AState)) or
     ((goFixedVertLine in Options) and (gdFixed in AState)) then
  begin
    if UseRightToLeftAlignment then
    begin
      ACanvas.MoveTo(ARect.Right - GLW + 1,ARect.Bottom - GLW);
      ACanvas.LineTo(ARect.Right - GLW + 1,ARect.Top - GLW);
    end
    else
    begin
      ACanvas.MoveTo(ARect.Right - GLW ,ARect.Bottom - GLW);
      ACanvas.LineTo(ARect.Right - GLW  ,ARect.Top -GLW);
    end;
  end;

  SR := ARect;

  if gdFixed in AState then
  begin
    //Inflaterect(ARect,-1,-1)
    ARect.Right := ARect.Right - 1;
    ARect.Bottom := ARect.Bottom - 2;
  end
  else
    Inflaterect(ARect, -FGridLineWidth, -FGridLineWidth);

  if internal then
  begin
    DrawGridCell(ACanvas,ACol,ARow,ARect,AState);
  end;

  ARect := SR;

  // DrawText(Canvas.Handle,PChar(Cells[ACol,ARow]),Length(Cells[ACol,ARow]),ARect,DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOCLIP);

  if OutOfBounds then
  begin
    SelectClipRgn(ACanvas.Handle, 0);
    DeleteObject(hrgn);
  end;

  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;
end;

procedure TBaseGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
begin
  PaintCellInt(Canvas, ACol, ARow, ARect, AState);
end;

function TBaseGrid.GetDefaultDrawing: boolean;
begin
  Result := FDefaultDrawing;
end;

procedure TBaseGrid.GetDefaultProps(ACol, ARow: Integer; AFont: TFont;
  ABrush: TBrush; var AColorTo, AMirrorColor, AMirrorColorTo: TColor; var HA: TAlignment;
  var VA: TVAlignment; var WW: boolean; var GD: TCellGradientDirection);
begin
  AFont.Assign(Font);
  ABrush.Color := Color;
  AColorTo := clNone;
  AMirrorColor := clNone;
  AMirrorColorTo := clNone;
  HA := taLeftJustify;
  VA := vtaTop;
  WW := WordWrap;
  GD := GradientVertical;
end;

function TBaseGrid.GetGridLineWidth: integer;
begin
  Result := FGridLineWidth;
end;

procedure TBaseGrid.SetObjectEx(c, r: integer; const Value: TObject);
var
  EC: Boolean;
begin
  EC := GridCells[c,r] = '';
  if EC then
    GridCells[c,r] := ' ';

  //if not Assigned(inherited Objects[c,r]) then
  //  inherited Objects[c,r] := TCellProperties.Create(self,c,r);

  //TCellProperties(inherited Objects[c,r]).CellObject := Value;

   // set cellobject in a correct initialized cellproperties object
  CellProperties[c,r].CellObject := TIObjType(Value);
  if EC then
    GridCells[c,r] := '';
end;


procedure TBaseGrid.SetSelectionStyle(const Value: TGridSelectionStyle);
begin
  if (Value <> FSelectionStyle) then
  begin
    FSelectionStyle := Value;
    Invalidate;
  end;
end;

function TBaseGrid.GetObjectEx(c, r: integer): TObject;
begin
  if Assigned(inherited Objects[c,r]) then
    Result := TObject(TCellProperties(inherited Objects[c,r]).CellObject)
  else
    Result := nil;
end;

procedure TBaseGrid.SetDefaultDrawing(const Value: boolean);
begin
  inherited DefaultDrawing := false;
  FDefaultDrawing := value;
end;

procedure TBaseGrid.SetGridLineColor(const Value: TColor);
begin
  FGridLineColor := Value;
  Invalidate;
end;

procedure TBaseGrid.SetGridFixedLineColor(const Value: TColor);
begin
  FGridFixedLineColor := Value;
  Invalidate;
end;


procedure TBaseGrid.SetGridLineWidth(const Value: integer);
begin
  FGridLineWidth := value;
  inherited GridLineWidth := 0;
  Invalidate;
end;

function TBaseGrid.GetCellRect(c,r: integer): TRect;
var
  i,x,y: Integer;
  ARect,tlr: TRect;
begin
  x := 0;
  y := 0;

  for i := 1 to c do x := x + ColWidths[i - 1];
  for i := 1 to r do y := y + RowHeights[i - 1];

  // absolute rectangle of cell
  ARect := Rect(x,y,x + ColWidths[c],y + RowHeights[r]);

  x := 0;
  y := 0;

  if c >= FixedCols then
    for i := 1 to LeftCol do
      x := x + ColWidths[i - 1];

  if r >= FixedRows then
    for i := 1 to TopRow do
      y := y + RowHeights[i - 1];

  OffsetRect(ARect, -x, -y);

  tlr := inherited CellRect(LeftCol,TopRow);

  if c < FixedCols then
    tlr.Left := 0;

  if r < FixedRows then
    tlr.Top := 0;

  OffsetRect(ARect,tlr.Left,tlr.Top);

  Result := ARect;
end;

function TBaseGrid.GetCPGraphicObject(cp: TCellProperties): TObject;
begin
  Result := cp.FGraphicObject;
end;

function TBaseGrid.CellRect(c,r: integer): TRect;
var
  r1,r2: TRect;
  BC: TPoint;
  rc,ro: integer;

begin
  rc := RealColIndex(c);

  if HasCellProperties(rc,r) then
    if (CellProperties[rc,r].CellSpanX <> -1) or (CellProperties[rc,r].CellSpanY > 0) then
    begin
      BC := CellProperties[rc,r].BaseCell[rc,r];

      // compensate for hidden cols here
      ro := DisplColIndex(BC.X);

      r1 := GetCellRect(ro,BC.Y);

      if r1.Left = r1.Right then
        r1.Right := r1.Right + 1;

      r2 := GetCellRect(ro + CellProperties[BC.X,BC.Y].CellSpanX,BC.Y + CellProperties[BC.X,BC.Y].CellSpanY);

      if r2.Left = r2.Right then
        r2.Left := r2.Left - 1;

      UnionRect(r1, r1, r2);
      Result := r1;
    end
    else
    begin
      if Assigned(CellProperties[c,r].Control) then
        Result := GetCellRect(c,r)
      else
      begin
        r1 := inherited CellRect(c,r);
        Result := Rect(r1.Left,r1.Top,r1.Left + ColWidths[c],r1.Top + RowHeights[r]);
      end;
    end
  else
  begin
    r1 := inherited CellRect(c,r);
    Result := Rect(r1.Left,r1.Top,r1.Left + ColWidths[c],r1.Top + RowHeights[r]);
  end;
end;

function TBaseGrid.CellSize(c,r: Integer): TPoint;
var
  r1,r2: TRect;
  BC: TPoint;
begin
  if HasCellProperties(c,r) then
    if (CellProperties[c,r].CellSpanX <> -1) or (CellProperties[c,r].CellSpanY > 0) then
    begin
      BC := CellProperties[c,r].BaseCell[c,r];
      r1 := GetCellRect(BC.X,BC.Y);
      r2 := GetCellRect(BC.X + CellProperties[BC.X,BC.Y].CellSpanX,BC.Y + CellProperties[BC.X,BC.Y].CellSpanY);

      UnionRect(r1, r1, r2);
    end
    else
      r1 := GetCellRect(c,r)
  else
    r1 := GetCellRect(c,r);

  Result := Point(r1.Right-r1.Left,r1.Bottom - r1.Top);
end;

function TBaseGrid.CellSpan(ACol,ARow: Integer): TPoint;
var
  BC: TPoint;
begin
  Result := Point(0,0);
  if HasCellProperties(ACol,ARow) then
    if (CellProperties[ACol,ARow].CellSpanX <> -1) or (CellProperties[ACol,ARow].CellSpanY > 0) then
    begin
      BC := CellProperties[ACol,ARow].BaseCell[ACol,ARow];
      Result := Point(CellProperties[BC.X,BC.Y].CellSpanX,CellProperties[BC.X,BC.Y].CellSpanY);
    end;
end;

procedure TBaseGrid.CellTextChange(ACol, ARow: integer);
begin
  //
end;

procedure TBaseGrid.ChangeScale(M, D: Integer);
begin
  inherited;

  {$IFNDEF DELPHI_UNICODE}
  if not (csDesigning in ComponentState) then
  begin
    if M <> D then
      DefaultRowHeight := MulDiv(DefaultRowHeight, M, D);
  end;
  {$ENDIF}
end;

function TBaseGrid.CellExt(ACol,ARow: Integer): TPoint;
var
  BC: TPoint;
  cp: TCellProperties;
begin
  Result := Point(0,0);
  if HasCellProperties(ACol,ARow) then
  begin
    cp := CellProperties[ACol,ARow];
    if (cp.CellSpanX <> -1) and (cp.CellSpanY > 0) then
    begin
      BC := cp.BaseCell[ACol,ARow];
      Result := Point(BC.X - ACol + CellProperties[BC.X,BC.Y].CellSpanX,
                      BC.Y - ARow + CellProperties[BC.X,BC.Y].CellSpanY);
    end;
  end;
end;

function TBaseGrid.IsMergedCell(ACol,ARow: Integer): Boolean;
var
  cp: TCellProperties;
begin
  Result := False;
  if HasCellProperties(ACol,ARow) then
  begin
    cp := CellProperties[ACol,ARow];
    Result := (cp.CellSpanX > 0) or (cp.CellSpanY > 0);
  end;
end;

function TBaseGrid.IsMergedNonBaseCell(ACol,ARow: Integer): Boolean;
var
  cp: TCellProperties;
begin
  Result := False;
  if HasCellProperties(ACol,ARow) then
  begin
    cp := CellProperties[ACol,ARow];
    Result := ((cp.CellSpanX > 0) or (cp.CellSpanY > 0)) and
      not cp.IsBaseCell;
  end;
end;

function TBaseGrid.IsXMergedCell(ACol, ARow: Integer): Boolean;
var
  BC: TPoint;
begin
  Result := False;
  if HasCellProperties(ACol,ARow) then
  begin
    BC := CellProperties[ACol,ARow].BaseCell[ACol,ARow];
    Result := (CellProperties[BC.X,BC.Y].CellSpanX > 0);
  end;
end;

function TBaseGrid.IsYMergedCell(ACol, ARow: Integer): Boolean;
var
  BC: TPoint;
begin
  Result := False;
  if HasCellProperties(ACol,ARow) then
  begin
    BC := CellProperties[ACol,ARow].BaseCell[ACol,ARow];
    Result := (CellProperties[BC.X,BC.Y].CellSpanY > 0);
  end;
end;

function TBaseGrid.CreateCellProperties(Col,Row: integer): TCellProperties;
var
  HA: TAlignment;
  VA: TVAlignment;
  WW: Boolean;
  GD: TCellGradientDirection;
  AColorTo, AMirrorColor, AMirrorColorTo: TColor;
  cp: TCellProperties;
begin
  cp := TCellProperties.Create(Self,Col,Row);
  inherited Objects[Col,Row] := cp;

  // set default values here
  WW := WordWrap;

  GetDefaultProps(Col,Row,FDefFont,FDefBrush,AColorTo,AMirrorColor,AMirrorColorTo,HA,VA,WW,GD);

  cp.FCalcType := acNone;
  cp.FAlignment := HA;
  cp.FVAlignment := VA;
  cp.FWordWrap := WW;
  cp.FFontName := FDefFont.Name;
  cp.FFontSize := FDefFont.Size;
  cp.FFontStyle := FDefFont.Style;
  cp.FFontColor := FDefFont.Color;
  cp.FBrushColor := FDefBrush.Color;
  cp.FBrushColorTo := AColorTo;
  cp.FGradientDirection := GD;

  Result := cp;
end;

function TBaseGrid.GetCellProperties(c, r: integer): TCellProperties;
var
  cp: TCellProperties;
begin
  if not Assigned(inherited Objects[c,r]) then
  begin
    cp := CreateCellProperties(c,r);
    Result := cp;
  end
  else
    Result := TCellProperties(inherited Objects[c,r]);
end;

procedure TBaseGrid.MergeCells(c, r, spanx, spany: Integer);
var
  i,j: Integer;
  cp: TCellProperties;
begin
  inc(FMergeCount);
  for i := c to c + spanx - 1 do
    for j := r to r + spany - 1 do
    begin
      cp := CellProperties[i,j];

      cp.IsBaseCell := (i = c) and (j = r);

      if cp.IsBaseCell then
      begin
        cp.CellSpanX := SpanX - 1;
        cp.CellSpanY := SpanY - 1;
      end
      else
      begin
        cp.CellSpanX := i - c;
        cp.CellSpanY := j - r;
      end;
    end;

  for i := c to c + spanx - 1 do
    for j := r to r + spany - 1 do
    begin
      RepaintCell(i,j);
    end;
end;

procedure TBaseGrid.SetCellProperties(c, r: integer; const Value: TCellProperties);
begin
  if Assigned(inherited Objects[c,r]) then
      TCellProperties(inherited Objects[c,r]).Free;

  inherited Objects[c,r] := Value
end;

procedure TBaseGrid.SplitCells(c, r: integer);
var
  i,j: integer;
  spanx,spany: integer;
  bc: TPoint;
begin
  bc := CellProperties[c,r].BaseCell[c,r];

  c := bc.X;
  r := bc.Y;

  SpanX := CellProperties[c,r].CellSpanX;
  SpanY := CellProperties[c,r].CellSpanY;

  for i := c to c + spanx do
    for j := r to r + spany do
    begin
      CellProperties[i,j].IsBaseCell := True;
      CellProperties[i,j].CellSpanX := -1;
      CellProperties[i,j].CellSpanY := -1;
   end;
  for i := c to c + spanx do
    for j := r to r + spany do
      RepaintCell(i,j);

  dec(FMergeCount);
end;

function TBaseGrid.HasCellProperties(ACol, ARow: integer): boolean;
begin
  Result := false;
  if (ACol > -1) and (ACol < TotalColCount) then
    if (ARow > -1) {and (ARow < RowCount)} then
      Result := Assigned(inherited Objects[ACol,ARow]);
end;

function TBaseGrid.HasCustomCellBorder: boolean;
begin
  Result := false;
end;

function TBaseGrid.GetCellEx(c, r: integer): string;
var
  BC: TPoint;
  ws: widestring;
begin
  if HasCellProperties(c,r) then
    BC := CellProperties[c,r].BaseCell[c,r]
  else
    BC := Point(c,r);

  Result := inherited Cells[BC.X,BC.Y];

  if BC.Y < NormalRowCount then
  begin
    GetDisplText(BC.X,BC.Y,Result);

    if Assigned(FOnGetDisplWideText) then
    begin
      ws := Result;
      ws := DecodeWideStr(ws);
      GetDisplWideText(BC.X,BC.Y,ws);
      Result := EncodeWideStr(ws);
    end;
  end;
end;

procedure TBaseGrid.SetCellEx(c, r: integer; const Value: String);
var
  BC: TPoint;
begin
  if HasCellProperties(c,r) then
  begin
    BC := CellProperties[c,r].BaseCell[c,r];
    inherited Cells[BC.X,BC.Y] := Value;

    {$IFDEF DELPHI2006_LVL}
    CellTextChange(BC.X,BC.Y);
    {$ENDIF}

    if Assigned(Parent) then
      RepaintCell(c,r);
  end
  else
  begin
    FEditCellSet := true;
    inherited Cells[c,r] := Value;
    FEditCellSet := false;
  end;

  if HideLastRow and (r = RowCount - 1) then
    FloatFooterUpdate;
end;

function TBaseGrid.SelectCell(ACol, ARow: Integer): Boolean;
begin
  if BiDiMode = bdRightToLeft then
  begin
    Result := inherited SelectCell(Acol, ARow);
    Invalidate;
  end
  else
  begin
    RepaintCell(Col,Row);
    Result := inherited SelectCell(Acol, ARow);
    RepaintCell(ACol,ARow);
  end;
end;

procedure TBaseGrid.RepaintCell(c, r: integer);
var
  ARect: TRect;
begin
  if HandleAllocated Then
  begin
    ARect := CellRect(c,r);
    InvalidateRect(Handle,@ARect,True);
  end;
end;

function TBaseGrid.IsSelected(ACol, ARow: Integer): Boolean;
var
  sr: TRect;
  BC: TPoint;
begin
  sr.Left := Selection.Left;
  sr.Right := Selection.Right;
  sr.Top := Selection.Top;
  sr.Bottom := Selection.Bottom;

  Result := (ACol >= sr.Left) and (ACol <= sr.Right) and (ARow <= sr.Bottom) and (ARow >= sr.Top);

  if Result then Exit;

  if HasCellProperties(ACol,ARow) then
  begin
   if (CellProperties[ACol,ARow].CellSpanX <> 0) or (CellProperties[ACol,ARow].CellSpanY <> 0) then
     begin
       BC := CellProperties[ACol,ARow].BaseCell[ACol,ARow];

       with CellProperties[BC.X,BC.Y] do
       Result := (Col >= BC.X) and (Col <= BC.X + CellSpanX) and
                 (Row >= BC.Y) and (Row <= BC.Y + CellSpanY);
     end
   end;
end;

procedure TBaseGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  BC: TPoint;
begin
  if HasCellProperties(Col,Row) then
    BC := CellProperties[Col,Row].BaseCell[Col,Row];

  case Key of
  VK_LEFT:
    begin
      if HasCellProperties(Col,Row) then
      with CellProperties[BC.X,BC.Y] do
      begin
        if (Col > BC.X) and (Col > FixedCols) and (BC.X > 0) then
          Col := BC.X - CellSpanX;
      end;
    end;
  VK_RIGHT:
    begin
      if HasCellProperties(Col,Row) then
      with CellProperties[BC.X,BC.Y] do
      begin
        if (Col <= BC.X + CellSpanX) and (Col < ColCount - 1) and
           (BC.X < ColCount -1) and (BC.X + CellSpanX < ColCount) then
          Col := BC.X + CellSpanX;
      end;
    end;
  VK_UP:
    begin
      if HasCellProperties(Col,Row) then
      with CellProperties[BC.X,BC.Y] do
      begin
        if (Row > BC.Y) and (Row > FixedRows) and (BC.Y > 0) then
          Row := BC.Y - 1;
      end;
    end;
  VK_DOWN:
    begin
      if HasCellProperties(Col,Row) then
      with CellProperties[BC.X,BC.Y] do
      begin
        if (Row <= BC.Y + CellSpanY) and (Row < RowCount - 1) and
            (BC.Y < RowCount - 1) then
          Row := BC.Y + CellSpanY;
      end;
    end;
  VK_NEXT:
    begin
      if (TopRow + VisibleRowCount - FixedRows = RowCount - 1) and
         FHideLastRow and (RowCount > 1) then
        Row := RowCount - 2;
    end;

  end;

  inherited;

  SelectBaseCell;
end;

function TBaseGrid.GetEditText(ACol, ARow: Integer): string;
begin
  Result := inherited GetEditText(ACol,ARow);
end;

procedure TBaseGrid.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  inherited SetEditText(ACol,ARow,Value);
end;

function TBaseGrid.BaseCell(ACol,ARow: Integer): TPoint;
begin
  if HasCellProperties(ACol,ARow) then
    with CellProperties[ACol,ARow] do
    begin
      Result := BaseCell[ACol,ARow];
    end
  else
    Result := Point(ACol,ARow);
end;

function TBaseGrid.IsBaseCellEx(ACol, ARow: Integer;var MCol,MRow: Integer): Boolean;
var
  BC: TPoint;
begin
  Result := True;
  MCol := ACol;
  MRow := ARow;

  if HasCellProperties(ACol,ARow) then
    with CellProperties[ACol,ARow] do
    begin
       Result := IsBaseCell;
       if not Result then
       begin
         BC := BaseCell[ACol,ARow];
         MCol := BC.X;
         MRow := BC.Y;
       end;
    end;
end;

function TBaseGrid.IsBaseCell(ACol, ARow: Integer): Boolean;
begin
  Result := True;
  if HasCellProperties(ACol,ARow) then
    Result := CellProperties[ACol,ARow].IsBaseCell;
end;


function TBaseGrid.NodeIndent(ARow: Integer): Integer;
begin
  Result := 0;
end;

procedure TBaseGrid.GetVisualProperties(ACol, ARow: Integer;
  var AState: TGridDrawState; Print, Select, Remap: Boolean; ABrush: TBrush; var AColorTo, AMirrorColor, AMirrorColorTo: TColor; AFont: TFont;
  var HA: TAlignment; var VA: TVAlignment; var WW: Boolean;var GD: TCellGradientDirection);
begin

end;

function TBaseGrid.GetGraphicObjectEx(c, r: Integer): TObject;
begin
  if Assigned(inherited Objects[c,r]) then
    Result := TCellProperties(inherited Objects[c,r]).GraphicObject
  else
    Result := nil;
end;

procedure TBaseGrid.SetGraphicObjectEx(c, r: Integer;
  const Value: TObject);
var
  EC: Boolean;
  AFont: TFont;
  ABrush: TBrush;
  cp: TCellProperties;
  HA: TAlignment;
  VA: TVAlignment;
  WW: boolean;
  GD: TCellGradientDirection;
  AColorTo,AMirrorColor,AMirrorColorTo: TColor;

begin
  if (csDestroying in ComponentState) then
  begin
    if (Value = nil) then
      TCellProperties(inherited Objects[c,r]).GraphicObject := Value;
    Exit;
  end;

  EC := Cells[c,r] = '';
  if EC then
    Cells[c,r] := ' ';

  if not Assigned(inherited Objects[c,r]) then
  begin
    cp := TCellProperties.Create(Self,c,r);
    inherited Objects[c,r] := cp;

    // assign default values
    AFont := TFont.Create;
    ABrush := TBrush.Create;

    GetDefaultProps(c,r,AFont,ABrush,AColorTo,AMirrorColor,AMirrorColorTo,HA,VA,WW,GD);

    cp.Alignment := HA;
    cp.VAlignment := VA;
    cp.WordWrap := WW;
    cp.FontName := AFont.Name;
    cp.FontSize := AFont.Size;
    cp.FontStyle := AFont.Style;
    cp.FontColor := AFont.Color;
    cp.BrushColor := ABrush.Color;
    cp.BrushColorTo := AColorTo;
    cp.GradientDirection := GD;

    ABrush.Free;
    AFont.Free;
  end;

  TCellProperties(inherited Objects[c,r]).GraphicObject := Value;

  if EC then
    Cells[c,r] := '';
end;

function TBaseGrid.FullCell(c, r: Integer): TRect;
var
  pt: TPoint;
begin
  pt := BaseCell(c,r);

  Result.Left := pt.X;
  Result.Top := pt.Y;

  pt := CellSpan(c,r);
  Result.Right := Result.Left + pt.X;
  Result.Bottom := Result.Top + pt.Y;
end;

function TBaseGrid.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift,MousePos);

  if (Row - TopRow > VisibleRowCount ) and
     (Row < RowCount - 1) and FHideLastRow then
    TopRow := TopRow + 1;

  SelectBaseCell;
end;

function TBaseGrid.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift,MousePos);
  SelectBaseCell;
end;

procedure TBaseGrid.SelectBaseCell;
var
  BC: TPoint;
begin
  if HasCellProperties(Col,Row) then
  begin
    BC := CellProperties[Col,Row].BaseCell[Col,Row];
    Col := BC.X;
    Row := BC.Y;
  end;
end;

function TBaseGrid.FixedColsWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to FixedCols do
    Result := Result + ColWidths[i - 1];
end;

function TBaseGrid.FixedRowsHeight: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to FixedRows do
    Result := Result + RowHeights[i - 1];
end;

procedure TBaseGrid.RepaintFixedMergedCols;
var
  i,j: Integer;
begin
  if FixedRows = 0 then
    Exit;

  for i := LeftCol to LeftCol + VisibleColCount do
    for j := 1 to FixedRows do
      if IsMergedCell(i, j - 1) then
        RepaintCell(i, j - 1);
end;

procedure TBaseGrid.RepaintFixedMergedRows;
var
  i,j: Integer;
begin
  if FixedCols = 0 then
    Exit;

  for i := TopRow to TopRow + VisibleRowCount do
    for j := 1 to FixedCols do
      if IsMergedCell(j - 1,i) then
        RepaintCell(j - 1,i);
end;

procedure TBaseGrid.TopLeftChanged;
begin
  inherited;
  RepaintFixedMergedRows;
  RepaintFixedMergedCols;
end;


function TBaseGrid.TotalColCount: Integer;
begin
  Result := ColCount;
end;

function TBaseGrid.UseVCLStyles: boolean;
begin
  Result := False;
end;

function TBaseGrid.IsFixed(ACol, ARow: Integer): Boolean;
begin
  Result := False;
end;

procedure TBaseGrid.CopyCols(ACol1, ACol2: Integer);
begin
  Cols[ACol1] := Cols[ACol2];
end;

procedure TBaseGrid.CopyRows(ARow1, ARow2: Integer);
var
  i: Integer;
begin
  for i := 1 to ColCount do
  begin
    Cells[i - 1,ARow1] := Cells[i - 1,ARow2];
    inherited Objects[i - 1,ARow1] := inherited Objects[i - 1,ARow2];
  end;
end;

procedure TBaseGrid.NilCol(ACol: Integer);
var
  i: Integer;
begin
  for i := 1 to RowCount do
  begin
    inherited Objects[ACol,i - 1] := nil;
    Cells[ACol,i - 1] := '';
  end;
end;

procedure TBaseGrid.NilRow(ARow: Integer);
var
  i: Integer;
begin
  for i := 1 to ColCount do
  begin
    inherited Objects[i - 1,ARow] := nil;
    Cells[i - 1,ARow] := '';
  end;
end;

procedure TBaseGrid.NilCell(ACol, ARow: Integer);
begin
  Cells[ACol,ARow] := '';
  inherited Objects[ACol,ARow] := nil;
end;


function TBaseGrid.HasNodes: Boolean;
begin
  Result := False;
end;

// gets the smallest row span for a given row
function TBaseGrid.MinRowSpan(ARow: Integer): Integer;
var
  c,ms,ns: Integer;
begin
  ms := RowCount;
  for c := 1 to ColCount do
  begin
    ns := CellSpan(c - 1,ARow).Y;
    if (ns > 0) and (ns < ms) then
      ms := ns;
  end;
  Result := ms;
end;

function TBaseGrid.MaxRowSpan(ARow: Integer): Integer;
var
  c,ms,ns: Integer;
begin
  ms := 0;
  for c := 1 to ColCount do
  begin
    ns := CellSpan(c - 1,ARow).Y;
    if (ns > ms) then
      ms := ns;
  end;
  Result := ms;
end;


function TBaseGrid.RowSpanIdentical(ARow1,ARow2: Integer): Boolean;
var
  c: Integer;
  IsMerged: Boolean;
begin
  Result := True;
  IsMerged := False;
  for c := 1 to ColCount do
  begin
    if (CellSpan(c - 1,ARow1).Y > 0) and
       (CellSpan(c - 1,ARow2).Y > 0) then
    begin
      IsMerged := True;
      if (BaseCell(c - 1,ARow1).Y <> BaseCell(c - 1,ARow2).Y) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  Result := Result and IsMerged;
end;

function TBaseGrid.ColSpanIdentical(ACol1,ACol2: Integer): Boolean;
var
  r: Integer;
  IsMerged: Boolean;  
begin
  Result := True;
  IsMerged := False;
  for r := 1 to RowCount do
  begin
    if (CellSpan(ACol1,r - 1).X > 0) and
       (CellSpan(ACol2,r - 1).X > 0) then
    begin
      if (BaseCell(ACol1,r - 1).X <> BaseCell(ACol1,r - 1).X) then
      begin
        Result := False;
        Break;
      end;
    end
    else
      if (CellSpan(ACol1,r - 1).X <> CellSpan(ACol2,r - 1).X) then
      begin
        Result := False;
        Break;
      end;
  end;
  Result := Result and IsMerged;
end;


function TBaseGrid.GetGridObject(c, r: Integer): TObject;
begin
  Result := inherited Objects[c,r]
end;

procedure TBaseGrid.SetGridObject(c, r: Integer; const Value: TObject);
begin
  inherited Objects[c,r] := Value;
end;

procedure TBaseGrid.FloatFooterUpdate;
begin

end;

function TBaseGrid.GetGridCell(c, r: Integer): string;
begin
  Result := inherited Cells[c,r];
end;

procedure TBaseGrid.SetGridCell(c, r: Integer; const Value: string);
begin
  inherited Cells[c,r] := Value;
end;

procedure TBaseGrid.ClearPropRect(ACol1, ARow1, ACol2, ARow2: Integer);
var
  c,r: Integer;
  cp: TCellProperties;
begin
  if not FHasCellProps then
    Exit;
  for c := ACol1 to ACol2 do
   for r := ARow1 to ARow2 do
     if Assigned(inherited Objects[c,r]) then
     begin
       cp := TCellProperties(inherited Objects[c,r]);
       if Assigned(cp.GraphicObject) and (cp.GraphicObject.ClassName = 'TCellGraphic') then
         cp.GraphicObject.Free;

       TCellProperties(inherited Objects[c,r]).Free;
       inherited Objects[c,r] := nil;
     end;
end;

procedure TBaseGrid.ClearPropRow(ARow: Integer);
begin
  ClearPropRect(0,ARow,ColCount - 1,ARow);
end;

procedure TBaseGrid.ClearPropCell(ACol, ARow: Integer);
begin
  if Assigned(inherited Objects[ACol,ARow]) then
  begin
    TCellProperties(inherited Objects[ACol,ARow]).Free;
    inherited Objects[ACol,ARow] := nil;
  end;
end;


procedure TBaseGrid.ClearProps;
begin
  ClearPropRect(0,0,ColCount - 1,RowCount - 1);
end;

{$IFNDEF DELPHI6_LVL}
procedure TBaseGrid.SetGridOrientation(RightToLeftOrientation: Boolean);
var
  Org: TPoint;
  Ext: TPoint;
begin
  if RightToLeftOrientation then
  begin
    Org := Point(ClientWidth,0);
    Ext := Point(-1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end
  else
  begin
    Org := Point(0,0);
    Ext := Point(1,1);
    SetMapMode(Canvas.Handle, mm_Anisotropic);
    SetWindowOrgEx(Canvas.Handle, Org.X, Org.Y, nil);
    SetViewportExtEx(Canvas.Handle, ClientWidth, ClientHeight, nil);
    SetWindowExtEx(Canvas.Handle, Ext.X*ClientWidth, Ext.Y*ClientHeight, nil);
  end;
end;
{$ENDIF}


procedure TBaseGrid.GetDisplText(c, r: Integer; var Value: string);
begin
  if Assigned(OnGetDisplText) then
    OnGetDisplText(Self,c,r,Value)
end;

procedure TBaseGrid.GetDisplWideText(c, r: Integer; var Value: widestring);
begin
  if Assigned(OnGetDisplWideText) then
    OnGetDisplWideText(Self,c,r,Value)
end;


procedure TBaseGrid.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FUndoRedo) then
    FUndoRedo := nil;

  inherited;
end;

procedure TBaseGrid.SetUndoRedo(const Value: TAdvGridUndoRedo);
begin
  FUndoRedo := Value;
  if Assigned(FUndoRedo) then
    FUndoRedo.Grid := Self;
end;


function TBaseGrid.DisplColIndex(ACol: Integer): Integer;
begin
  Result := ACol;
end;

function TBaseGrid.RealColIndex(ACol: Integer): Integer;
begin
  Result := ACol;
end;

procedure TBaseGrid.SetActiveRowColorTo(const Value: TColor);
begin
  if (FActiveRowColorTo <> Value) then
  begin
    FActiveRowColorTo := Value;
    Invalidate;
  end;
end;

procedure TBaseGrid.SetActiveRowMirrorColor(const Value: TColor);
begin
  if (FActiveRowMirrorColor <> Value) then
  begin
    FActiveRowMirrorColor := Value;
    Invalidate;
  end;
end;

procedure TBaseGrid.SetActiveRowMirrorColorTo(const Value: TColor);
begin
  if (FActiveRowMirrorColorTo <> Value) then
  begin
    FActiveRowMirrorColorTo := Value;
    Invalidate;
  end;
end;


procedure TBaseGrid.SetActiveRowColor(const Value: TColor);
begin
  if (FActiveRowColor <> Value) then
  begin
    FActiveRowColor := Value;
    Invalidate;
  end;
end;

procedure TBaseGrid.SetActiveRowShow(const Value: Boolean);
begin
  if (FActiveRowShow <> Value) then
  begin
    FActiveRowShow := Value;
    Invalidate;
  end;
end;

{ TCellProperties }

procedure TCellProperties.Assign(Source: TPersistent);
begin
  if (Source is TCellProperties) then
  begin
    FIsBaseCell := TCellProperties(Source).IsBaseCell;
    FCellSpanX := TCellProperties(Source).CellSpanX;
    FCellSpanY := TCellProperties(Source).CellSpanY;
    FOwnerRow := TCellProperties(Source).OwnerRow;
    FOwnerCol := TCellProperties(Source).OwnerCol;
    FObject := TCellProperties(Source).CellObject;
    FAlignment := TCellProperties(Source).Alignment;
    FBorderColor := TCellProperties(Source).BorderColor;
    FBorderWidth := TCellProperties(Source).BorderWidth;
    FBrushColor := TCellProperties(Source).BrushColor;
    FBrushStyle := TCellProperties(Source).BrushStyle;
    FFontColor := TCellProperties(Source).FontColor;
    FFontStyle := TCellProperties(Source).FontStyle;
    FFontName := TCellProperties(Source).FontName;
    FFontSize := TCellProperties(Source).FontSize;
    FReadOnly := TCellProperties(Source).ReadOnly;
    FEditor := TCellProperties(Source).Editor;
    FValignment := TCellProperties(Source).VAlignment;
    FWordWrap := TCellProperties(Source).WordWrap;
    FNodeLevel := TCellProperties(Source).NodeLevel;
    FControl := TCellProperties(Source).Control;
    FMergeCollaps := TCellProperties(Source).MergeCollaps;
  end;
end;

constructor TCellProperties.Create(AOwner: TBaseGrid; ACol, ARow:integer);
begin
  inherited Create;

  FObject := 0;
  FGraphicObject := nil;
  FIsBaseCell := True;
  FCellSpanX := -1;
  FCellSpanY := -1;

  FBrushColor := clNone;
  FBrushColorTo := clNone;
  FFontColor := clNone;
  FReadOnly := False;
  FOwnerGrid := AOwner;
  FOwnerCol := ACol;
  FOwnerRow := ARow;
  FCalcObject := nil;
  FWordWrap := AOwner.FWordWrap;
  FNodeLevel := 0;
  FMergeCollaps := False;
  AOwner.FHasCellProps := True;
end;

function TCellProperties.GetBaseCell(c, r: Integer): TPoint;
begin
  if IsBaseCell then
    Result := Point(c,r)
  else
  begin
    if (CellSpanX <> - 1) and (CellSpanY >= 0) then
      Result := Point(c - CellSpanX,r - CellSpanY)
    else
      Result := Point(c,r)
  end;
end;

function TCellProperties.GetCellSpanY: Integer;
begin
  if not FMergeCollaps then
    Result := FCellSpanY
  else
    Result := 0;
end;

procedure TCellProperties.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TCellProperties.SetBrushColor(const Value: TColor);
begin
  FBrushColor := Value;
  FOwnerGrid.RepaintCell(FOwnerCol, FOwnerRow);
end;

procedure TCellProperties.SetBrushColorTo(const Value: TColor);
begin
  FBrushColorTo := Value;
  FOwnerGrid.RepaintCell(FOwnerCol, FOwnerRow);
end;

procedure TCellProperties.SetVAlignment(const Value: TVAlignment);
begin
  FValignment := Value;
  FOwnerGrid.RepaintCell(FOwnerCol, FOwnerRow);
end;

procedure TCellProperties.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  FOwnerGrid.RepaintCell(FOwnerCol, FOwnerRow);
end;

{ TAdvGridUndoRedo }

function TAdvGridUndoRedo.CanRedo: Boolean;
begin
  Result := (FLevel < FItems.Count);
end;

function TAdvGridUndoRedo.CanUndo: Boolean;
begin
  Result := (FLevel > 0);
end;

constructor TAdvGridUndoRedo.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TUndoRedoCollection.Create;
  FLevel := 0;
end;

destructor TAdvGridUndoRedo.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TAdvGridUndoRedo.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
  inherited;
end;

procedure TAdvGridUndoRedo.Redo;
begin
  if Assigned(Grid) then
  begin
    if (FLevel < FItems.Count) then
    begin
      inc(FLevel);

      if TUndoRedoItem(FItems.Items[FLevel - 1]).SequenceStart then
      begin
        inc(FLevel);

        while (FLevel < FItems.Count) and not (TUndoRedoItem(FItems.Items[FLevel - 1]).SequenceStop) do
        begin
          with TUndoRedoItem(FItems.Items[FLevel - 1]) do
          begin
            if Assigned(FOnRedo) then
              FOnRedo(self, Grid, Col, Row, Grid.Cells[Col,Row], Value);

            Grid.Cells[Col,Row] := Value;
            Grid.Col := Grid.DisplColIndex(Col);
            Grid.Row := Row;
          end;
          inc(FLevel);
        end;
        Grid.Invalidate;
      end
      else
        with TUndoRedoItem(FItems.Items[FLevel - 1]) do
        begin
          if Assigned(FOnRedo) then
            FOnRedo(self, Grid, Col, Row, Grid.Cells[Col,Row], Value);

          Grid.Cells[Col,Row] := Value;
          Grid.Col := Grid.DisplColIndex(Col);
          Grid.Row := Row;
          Grid.RepaintCell(Grid.Col,Grid.Row);
        end;
    end;
  end;
end;

procedure TAdvGridUndoRedo.RegisterChange(ACol, ARow: Integer; OldValue,
  NewValue: string);
begin
  if (FItems.Count >= FMaxLevel) and (FMaxLevel > 0) then
    FItems.Items[0].Free;

  with TUndoRedoItem(FItems.Add) do
  begin
    Col := ACol;
    Row := ARow;
    Value := NewValue;
    OrigValue := OldValue;
    SequenceStart := false;
    SequenceStop := false;
  end;
  FLevel := FItems.Count;
end;

procedure TAdvGridUndoRedo.Reset;
begin
  while (FItems.Count > 0) do
    FItems.Items[0].Free;
  FLevel := 0;
end;

procedure TAdvGridUndoRedo.StartSequence;
begin
  with TUndoRedoItem(FItems.Add) do
  begin
    Col := -1;
    Row := -1;
    Value := '';
    OrigValue := '';
    SequenceStart := true;
    SequenceStop := false;
  end;
  FLevel := FItems.Count;
end;

procedure TAdvGridUndoRedo.StopSequence;
begin
  with TUndoRedoItem(FItems.Add) do
  begin
    Col := -1;
    Row := -1;
    Value := '';
    OrigValue := '';
    SequenceStart := false;
    SequenceStop := true;
  end;
  FLevel := FItems.Count;
end;

procedure TAdvGridUndoRedo.Undo;
begin
  if Assigned(Grid) and (FLevel > 0) and (FLevel <= FItems.Count) then
  begin
    if TUndoRedoItem(FItems.Items[FLevel - 1]).SequenceStop then
    begin
      dec(FLevel);
      while (FLevel > 0) and not (TUndoRedoItem(FItems.Items[FLevel - 1]).SequenceStart) do
      begin
        with TUndoRedoItem(FItems.Items[FLevel - 1]) do
        begin
          if Assigned(FOnUndo) then
             FOnUndo(self, Grid, Col, Row, Grid.Cells[Col,Row], OrigValue);
          Grid.Cells[Col,Row] := OrigValue;
          Grid.Col := Grid.DisplColIndex(Col);
          Grid.Row := Row;
        end;
        dec(FLevel);
      end;
      dec(FLevel);
      Grid.Invalidate;
    end
    else
    begin
      with TUndoRedoItem(FItems.Items[FLevel - 1]) do
      begin
        if Assigned(FOnUndo) then
          FOnUndo(self, Grid, Col, Row, Grid.Cells[Col,Row], OrigValue);
        Grid.Cells[Col,Row] := OrigValue;
        Grid.Col := Grid.DisplColIndex(Col);
        Grid.Row := Row;
        Grid.RepaintCell(Grid.Col,Row);
      end;
      dec(FLevel);
    end;
  end;
end;

{ TUndoRedoCollection }

constructor TUndoRedoCollection.Create;
begin
  inherited Create(TUndoRedoItem);
end;


end.
