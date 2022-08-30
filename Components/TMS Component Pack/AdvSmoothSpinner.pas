{**************************************************************************}
{ TAdvSmoothSpinner component                                              }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2010 - 2015                                                }
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

unit AdvSmoothSpinner;

interface

{$I TMSDEFS.INC}

uses
  ExtCtrls, Windows, Forms, Dialogs,  Messages, SysUtils, Classes, Graphics,
  Math, Controls, AdvStyleIF, GDIPFill, GDIPPictureContainer, ImgList,
  DateUtils,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Improved : formatting Number values with %0.2d to function as hours, minutes or seconds
  // v1.0.0.2 : Improved : Calculating DateTime with IncMonth and IncYear
  // v1.0.1.0 : New : ReadOnly property to Disable all user interactions
  // v1.0.2.0 : New : Property top and bottom fill to show / hide overlay effect.
  //          : Fixed : Animation returns to rangefrom when using cyclic and value is at maximum
  // v1.0.3.0 : New : Support for Windows Vista and Windows Seven Style
  //          : Fixed : Issue with SelectedValue returning larger and smaller numbers than Range
  // v1.0.4.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.5.0 : New : Delphi 2010 Touch Support
  // v1.0.5.1 : Fixed : Issue with SelectedValue set to RangeFrom when loading
  // v1.0.6.0 : New : Built-in support for Office 2010 colors
  // v1.1.0.0 : New : Smooth Scrolling support
  // v1.1.0.1 : Fixed : Issue with focusing
  // v1.2.0.0 : New : Metro style support
  // v1.2.0.1 : Fixed : Issue with Date selection
  // v1.2.0.2 : Fixed : Issue with negative values
  // v1.2.0.3 : Fixed : Issue with Month / Year selection
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.0.1 : Improved : public FocusedColumn function
  //          : Fixed : Issue with text calculation
  // v1.3.0.2 : Fixed : Issue with changing Enabled property
  // 1.4.0.0 : New : Windows 10, Office 2016 styles added

type
  TWinCtrl = class(TWinControl)
  public                 
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothSpinner = class;

  TAdvSmoothSpinnerRangeType = (rtNumber, rtDateTime, rtCustom);

  TAdvSmoothSpinnerStepType = (stNumber, stSecond, stMinute, stHour, stDay, stMonth, stYear);

  TAdvSmoothSpinnerCustomItem = class(TCollectionItem)
  private
    FOwner: TAdvSmoothSpinner;
    FValue: Double;
    FText: String;
    FImageIndex: integer;
    FpictureName: string;
    procedure SetText(const Value: String);
    procedure SetValue(const Value: Double);
    procedure SetImageIndex(const Value: integer);
    procedure SetPictureName(const Value: string);
  protected
    procedure Changed;
  published
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: String read FText write SetText;
    property Value: Double read FValue write SetValue;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property PictureName: string read FpictureName write SetPictureName;
  end;

  TAdvSmoothSpinnerCustomItems = class(TCollection)
  private
    FOwner: TAdvSmoothSpinner;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothSpinnerCustomItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothSpinnerCustomItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothSpinner);
    function Add: TAdvSmoothSpinnerCustomItem;
    function Insert(Index: Integer): TAdvSmoothSpinnerCustomItem;
    property Items[Index: Integer]: TAdvSmoothSpinnerCustomItem read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
    procedure Update(Item: TCollectionItem); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothSpinnerColumn = class(TCollectionItem)
  private
    FSaveDateRangeto, FSaveDateRangeFrom: TDateTime;
    FDrawIndex: integer;
    FSp: Double;
    FAnimating, FAnimate: Boolean;
    FMouseDown, FMouseUp: Boolean;
    FTimeStart, FTimeStop: integer;
    FDragY, FScrollY: integer;
    FClickX, FClickY: integer;
    FCurrentScPos, FScPosTo: integer;
    Fowner: TAdvSmoothSpinner;
    FEnabled: Boolean;
    FVisible: Boolean;
    FRangeFrom: Double;
    FRangeTo: Double;
    FSelectedValue: Double;
    FRangeType: TAdvSmoothSpinnerRangeType;
    FHint: String;
    FStep: Double;
    FValueFormat: String;
    FDateRangeTo: TDateTime;
    FDateRangeFrom: TDateTime;
    FDateTimeValueFormat: String;
    FStepType: TAdvSmoothSpinnerStepType;
    FCustomItems: TAdvSmoothSpinnerCustomItems;
    FCyclic: Boolean;
    FFont: TFont;
    FHoverFont: TFont;
    FWidth: integer;
    FTextAlign: TAlignment;
    FOnlyDate: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetRangeFrom(const Value: Double);
    procedure SetRangeTo(const Value: Double);
    procedure SetSelectedValue(const Value: Double);
    procedure SetRangeType(const Value: TAdvSmoothSpinnerRangeType);
    procedure SetHint(const Value: String);
    procedure SetStep(const Value: Double);
    procedure SetValueFormat(const Value: String);
    procedure SetDateRangeFrom(const Value: TDateTime);
    procedure SetDateRangeTo(const Value: TDateTime);
    procedure SetDateTimeValueFormat(const Value: String);
    procedure SetStepType(const Value: TAdvSmoothSpinnerStepType);
    procedure SetCustomItems(const Value: TAdvSmoothSpinnerCustomItems);
    procedure SetCyclic(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetWidth(const Value: integer);
    function GetSelectedDateTime: TDateTime;
    procedure SetSelectedDateTime(const Value: TDateTime);
    procedure SetTextAlign(const Value: TAlignment);
    function GetSelectedCustomIndex: integer;
    procedure SetSelectedCustomIndex(const Value: integer);
    procedure SetOnlyDate(const Value: Boolean);
    function GetSelectedValue: Double;
    procedure SetHoverFont(const Value: TFont);
  protected
    procedure Changed;
    procedure CustomItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics; r: TGPRectF);
    procedure SetAnimatedValue;
    function GetColumnRect: TGPRectF;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ScrollToValue(Value: double; Animation: Boolean = true; AnimationSpeed: integer = 4);
    procedure Next(Animation: Boolean = true);
    procedure Previous(Animation: Boolean = true);
    function IncSteps(StartValue: Double; nr: integer): double;
    function StepsFromTo(StartValue, EndValue: Double): integer;
    function GetStep: Double;
    function GetRangeCount: integer;
    function GetRangeTo: Double;
    function GetRangeFrom: Double;
    property SelectedDateTime: TDateTime read GetSelectedDateTime write SetSelectedDateTime;
    property SelectedCustomIndex: integer read GetSelectedCustomIndex write SetSelectedCustomIndex;        
  published
    property Visible: Boolean read FVisible write SetVisible default true;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Hint: String read FHint write SetHint;
    property Width: integer read FWidth write SetWidth default 20;
    property RangeFrom: Double read FRangeFrom write SetRangeFrom;
    property RangeTo: Double read FRangeTo write SetRangeTo;
    property DateRangeFrom: TDateTime read FDateRangeFrom write SetDateRangeFrom;
    property DateRangeTo: TDateTime read FDateRangeTo write SetDateRangeTo;
    property Step: Double read FStep write SetStep;
    property StepType: TAdvSmoothSpinnerStepType read FStepType write SetStepType default stNumber;
    property SelectedValue: Double read GetSelectedValue write SetSelectedValue;
    property ValueFormat: String read FValueFormat write SetValueFormat;
    property DateTimeValueFormat: String read FDateTimeValueFormat write SetDateTimeValueFormat;
    property RangeType: TAdvSmoothSpinnerRangeType read FRangeType write SetRangeType default rtNumber;
    property OnlyDate: Boolean read FOnlyDate write SetOnlyDate default false;
    property CustomItems: TAdvSmoothSpinnerCustomItems read FCustomItems write SetCustomItems;
    property Cyclic: Boolean read FCyclic write SetCyclic default false;
    property Font: TFont read FFont write SetFont;
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property TextAlign: TAlignment read FTextAlign write SetTextAlign default taCenter;
  end;

  TAdvSmoothSpinnerColumns = class(TCollection)
  private
    FOwner: TAdvSmoothSpinner;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothSpinnerColumn;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothSpinnerColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothSpinner);
    function Add: TAdvSmoothSpinnerColumn;
    function Insert(Index: Integer): TAdvSmoothSpinnerColumn;
    property Items[Index: Integer]: TAdvSmoothSpinnerColumn read GetItem write SetItem; default;
    procedure Delete(Index: Integer);
    procedure Update(Item: TCollectionItem); override;    
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothSpinnerColumnAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothSpinner;
    FAutoSize: Boolean;
    FSpacing: integer;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FTextSpacing: integer;
    FDisabledFill: TGDIPFill;
    FHoverFill: TGDIPFill;
    FAllowHovering: Boolean;
    FImageHeight: integer;
    FImageWidth: integer;
    procedure SetSpacing(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTextSpacing(const Value: integer);
    procedure SetDisabledFill(const Value: TGDIPFill);
    procedure SetHoverFill(const Value: TGDIPFill);
    procedure SetAllowHovering(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetImageHeight(const Value: integer);
    procedure SetImageWidth(const Value: integer);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothSpinner);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property AllowHovering: Boolean read FAllowHovering write SetAllowHovering default true;
    property Spacing: integer read FSpacing write SetSpacing default 5;
    property Fill: TGDIPFill read FFill write SetFill;
    property HoverFill: TGDIPFill read FHoverFill write SetHoverFill;
    property DisabledFill: TGDIPFill read FDisabledFill write SetDisabledFill;
    property TextSpacing: integer read FTextSpacing write SetTextSpacing default 25;
    property ImageWidth: integer read FImageWidth write SetImageWidth default 30;
    property ImageHeight: integer read FImageHeight write SetImageHeight default 30;    
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothSpinnerSelectedValueChangedEvent = procedure(Sender: TObject; Column, SelectedCustomIndex: integer; SelectedValue: Double; RangeType: TAdvSmoothSpinnerRangeType) of object;

  TAdvSmoothSpinnerColumnHintEvent = procedure(Sender: TObject; Column: integer; Hint: String) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothSpinner = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FMetroStyle: Boolean;
    FFocused, FConstructed: Boolean;
    FFocusedColumn: integer;
    FAnimateTimer: TTimer;    
    FDesignTime: Boolean;
    FHoveredColumn: integer;
    FColumnAppearance: TAdvSmoothSpinnerColumnAppearance;
    FFill: TGDIPFill;
    FSelectedFill: TGDIPFill;
    FSelectedHeight: integer;
    FColumns: TAdvSmoothSpinnerColumns;
    FAnimationFactor: integer;
    FOnSelectedValueChanged: TAdvSmoothSpinnerSelectedValueChangedEvent;
    FOnColumnHint: TAdvSmoothSpinnerColumnHintEvent;
    FContainer: TGDIPPictureContainer;
    FImages: TCustomImageList;
    FReadOnly: Boolean;
    FBottomLayerFill: TGDIPfill;
    FTopLayerFill: TGDIPFill;
    FSmoothScrolling: Boolean;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;    
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetColumnAppearance(const Value: TAdvSmoothSpinnerColumnAppearance);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSelectedFill(const Value: TGDIPFill);
    procedure SetSelectedHeight(const Value: integer);
    procedure SetColumns(const Value: TAdvSmoothSpinnerColumns);
    procedure SetAnimationFactor(const Value: integer);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetBottomLayerFill(const Value: TGDIPfill);
    procedure SetTopLayerFill(const Value: TGDIPFill);
    procedure SetSmoothScrolling(const Value: Boolean);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure CreateWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoEnter; override;
    procedure DoExit; override;        
    procedure Changed;
    procedure ColumnAppearanceChanged(Sender: TObject);
    procedure ColumnsChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure DrawBackGround;
    procedure DrawColumns;
    procedure DrawTopLayer;
    procedure DrawSelectedLayer;
    procedure InitPreview;
    procedure Animate(Sender: TObject);
    function InsideRect: TRect;
    function GetSelectedRect: TRect;
    function GetVersionNr: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    function XYToColumn(X, Y: integer): integer;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
    function FocusedColumn: Integer;
  published
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 4;
    property Columns: TAdvSmoothSpinnerColumns read FColumns write SetColumns;
    property ColumnAppearance: TAdvSmoothSpinnerColumnAppearance read FColumnAppearance write SetColumnAppearance;
    property SelectedFill: TGDIPFill read FSelectedFill write SetSelectedFill;
    property SelectedHeight: integer read FSelectedHeight write SetSelectedHeight default 30;
    property TopLayerFill: TGDIPFill read FTopLayerFill write SetTopLayerFill;
    property BottomLayerFill: TGDIPfill read FBottomLayerFill write SetBottomLayerFill;
    property Fill: TGDIPFill read FFill write SetFill;
    property PictureContainer: TGDIPPictureContainer read FContainer write FContainer;
    property Images: TCustomImageList read FImages write FImages;
    property Version: String read GetVersion write SetVersion;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property SmoothScrolling: Boolean read FSmoothScrolling write SetSmoothScrolling default false;

    property OnSelectedValueChanged: TAdvSmoothSpinnerSelectedValueChangedEvent read FOnSelectedValueChanged write FOnSelectedValueChanged;
    property OnColumnHint: TAdvSmoothSpinnerColumnHintEvent read FOnColumnHint write FOnColumnHint;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;   
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

function AnimateDouble(var Start: integer; Stop: integer; Delta: Double; Margin: integer): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

{ TAdvSmoothSpinner }

procedure TAdvSmoothSpinner.ColumnAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSpinner.ColumnsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSpinner.Animate(Sender: TObject);
var
  d: single;
  posTo: integer;
  i: integer;
begin
  if (csDesigning in ComponentState) or (Columns.Count = 0) then
    Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      if FAnimate and Visible and Enabled then
      begin
        posTo := FScPosTo;
        d := Abs(posto - FCurrentScPos) / Max(1, Abs(FSp) * FOwner.FAnimationFactor);
        FAnimating := AnimateDouble(FCurrentScPos, posto, d, 1);
        if FAnimating then
        begin
          if Cyclic then
          begin
            if FScPosTo >= (GetRangeCount + 1) * ColumnAppearance.TextSpacing then
            begin
              FCurrentScPos := FCurrentScPos - (GetRangeCount * ColumnAppearance.TextSpacing) - ColumnAppearance.TextSpacing;
              FScPosTo := FScPosTo - (GetRangeCount * ColumnAppearance.TextSpacing) - ColumnAppearance.TextSpacing;
              SetAnimatedValue;
            end
            else if FScPosTo <= -ColumnAppearance.TextSpacing then
            begin
              FCurrentScPos := FCurrentScPos + (GetRangeCount * ColumnAppearance.TextSpacing) +  ColumnAppearance.TextSpacing;
              FScPosTo := FScPosTo + (GetRangeCount * ColumnAppearance.TextSpacing) +  ColumnAppearance.TextSpacing;
              SetAnimatedValue;
            end;
          end;
          Changed;
        end
        else
        begin
          FAnimate := false;
          SetAnimatedValue;
          if Assigned(FOwner.FOnSelectedValueChanged) then
            FOwner.FOnSelectedValueChanged(Self, Index, Round(FselectedValue - GetRangeFrom), FSelectedValue, RangeType);
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothSpinner.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSpinner) then
  begin
    FColumnAppearance.Assign((Source as TAdvSmoothSpinner).ColumnAppearance);
    FFill.Assign((Source as TAdvSmoothSpinner).Fill);
    FSelectedFill.Assign((Source as TAdvSmoothSpinner).SelectedFill);
    FSelectedHeight := (Source as TAdvSmoothSpinner).SelectedHeight;
    FColumns.Assign((Source as TAdvSmoothSpinner).Columns);
    FAnimationFactor := (Source as TAdvSmoothSpinner).AnimationFactor;
    FReadOnly := (Source as TAdvSmoothSpinner).ReadOnly;
    FTopLayerFill.Assign((Source as TAdvSmoothSpinner).TopLayerFill);
    FBottomLayerFill.Assign((Source as TAdvSmoothSpinner).BottomLayerFill);
    FSmoothScrolling := (Source as TAdvSmoothSpinner).SmoothScrolling;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.Changed;
var
  di, i: integer;
begin
  di := 0;
  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      if Visible then
      begin
        FDrawIndex := di;
        Inc(di);
      end;
    end;
  end;
  Invalidate;
end;

procedure TAdvSmoothSpinner.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothSpinner.CMHintShow(var Message: TMessage);
var
  c: integer;
  pt: TPoint;
  dColumn: TAdvSmoothSpinnerColumn;
begin
  with TCMHintShow(Message).HintInfo^ do
  begin
    HintStr := self.Hint;
    pt := CursorPos;
    c := XYToColumn(pt.X, pt.Y);
    if c <> -1 then
    begin
      dColumn := Columns[c];
      if dColumn <> nil then
      begin
        if dColumn.Hint <> '' then
          HintStr := dColumn.Hint;
        if Assigned(FOnColumnHint) then
          FOnColumnHint(Self, c, HintStr);
      end;
    end;
    ReshowTimeout := 0;
  end;
end;

procedure TAdvSmoothSpinner.CMMouseLeave(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  FHoveredColumn := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    with Columns[I] do
    begin
      FMouseUp := true;
      FMouseDown := false;
    end;
  end;
    
  Application.CancelHint;
  Changed;
end;

constructor TAdvSmoothSpinner.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  DoubleBuffered := true;
  Width := 200;
  Height := 128;
  FColumnAppearance := TAdvSmoothSpinnerColumnAppearance.Create(Self);
  FColumnAppearance.OnChange := ColumnAppearanceChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FSelectedFill := TGDIPFill.Create;
  FSelectedFill.OnChange := FillChanged;
  FColumns := TAdvSmoothSpinnerColumns.Create(Self);
  FColumns.OnChange := ColumnsChanged;
  FSelectedHeight := 30;
  FAnimationFactor := 4;
  FHoveredColumn := -1;
  FAnimateTimer := TTimer.Create(Self);
  FAnimateTimer.Interval := 1;
  FAnimateTimer.Enabled := true;
  FAnimateTimer.OnTimer := Animate;
  FReadOnly := false;
  FTopLayerFill := TGDIPFill.Create;
  FTopLayerFill.OnChange := FillChanged;
  FBottomLayerFill := TGDIPFill.Create;
  FBottomLayerFill.OnChange := FillChanged;
  TabStop := true;
  FSmoothScrolling := false;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvSmoothSpinner.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;  
  if FDesignTime then
  begin
    InitPreview;
    SetComponentStyle(tsOffice2007Luna);
  end;
  FConstructed := true;
end;

procedure TAdvSmoothSpinner.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothSpinner.Destroy;
begin
  FAnimateTimer.free;
  FColumnAppearance.Free;
  FFill.Free;
  FSelectedFill.Free;
  FColumns.Free;
  FTopLayerFill.Free;
  FBottomLayerFill.Free;
  inherited;
end;

procedure TAdvSmoothSpinner.DoEnter;
begin
  inherited;
  if ReadOnly then
    Exit;

  FFocused := true;
  Changed;
end;

procedure TAdvSmoothSpinner.DoExit;
begin
  inherited;
  if ReadOnly then
    Exit;
      
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothSpinner.DrawBackGround;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  Fill.Fill(g, MakeRect(0, 0, ClientWidth - 1, ClientHeight - 1));
  g.Free;
end;

procedure TAdvSmoothSpinner.DrawColumns;
var
  I: Integer;
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

  for I := 0 to Columns.Count - 1 do
    with Columns[I] do
      if Visible then
        Draw(g, GetColumnRect);

  g.free;
end;

procedure TAdvSmoothSpinner.DrawSelectedLayer;
var
  g: TGPGraphics;
  r: TRect;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  r := GetSelectedRect;
  FSelectedFill.Fill(g, MakeRect(r.Left, r.Top, R.Right, r.Bottom - r.Top));
  g.Free;
end;

procedure TAdvSmoothSpinner.DrawTopLayer;
var
  g: TGPGraphics;
  rt: TRect;
begin
  rt := ClientRect;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  TopLayerFill.Fill(g, MakeRect(rt.Left, rt.Top, rt.Right - rt.Left, 36));
  BottomLayerFill.Fill(g, MakeRect(rt.Left, rt.Bottom - 37, rt.Right - rt.Left, 37));
  g.Free;
end;

procedure TAdvSmoothSpinner.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothSpinner.FocusedColumn: Integer;
begin
  Result := FFocusedColumn;
end;

function TAdvSmoothSpinner.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothSpinner.GetSelectedRect: TRect;
var
  h, hs: integer;
begin
  h := (InsideRect.Bottom - InsideRect.Top) div 2;
  hs := SelectedHeight div 2;
  Result := Rect(InsideRect.Left, h - hs, InsideRect.Right - InsideRect.Left, h + hs);
end;

function TAdvSmoothSpinner.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothSpinner.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothSpinner.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothSpinner.InitPreview;
var
  i: integer;
begin
  for I := 0 to 2 do
    Columns.Add;
end;

function TAdvSmoothSpinner.InsideRect: TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (Fill.ShadowColor <> clNone) {and not Transparent} then
    sh := Fill.ShadowOffset;

  Result := Rect(0, 0, Width, Height);
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1 - sh;
  Result.Bottom := Result.Bottom - 1 - sh;

  if (Fill.BorderColor <> clNone) {and not Transparent} then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

procedure TAdvSmoothSpinner.KeyDown(var Key: Word; Shift: TShiftState);
var
  sel: Double;
  fc: integer;
begin
  inherited;
  if ReadOnly then
    Exit;

  fc := FFocusedColumn;
  case Key of
    VK_LEFT: fc := fc - 1;
    VK_RIGHT: fc := fc + 1;
  end;

  fc := Min(Columns.Count - 1, Max(0, fc));
  
  if Columns[fc].Enabled and Columns[fc].Visible then
    FFocusedColumn := fc
  else
    Exit;

  with Columns[FFocusedColumn] do
  begin
    sel := FSelectedValue;
    case Key of
      VK_UP: sel := IncSteps(FSelectedValue, -1);
      VK_DOWN: sel := IncSteps(FSelectedValue, 1);
      VK_HOME: sel := GetRangeFrom;
      VK_END: sel := GetRangeTo;
      VK_PRIOR: sel := IncSteps(FSelectedValue, -10);
      VK_NEXT: sel := IncSteps(FSelectedValue, 10);
    end;

    if Cyclic then
    begin
      if sel = IncSteps(GetRangeFrom, 1) then
        sel := GetRangeFrom
      else if sel = IncSteps(GetRangeFrom, -1) then
        sel := GetRangeTo;
    end;

    sel := Min(GetRangeTo, Max(GetRangeFrom, sel));

    if (FSelectedValue <> sel) then
      ScrollToValue(sel, false);

    Changed;
  end;

end;

procedure TAdvSmoothSpinner.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothSpinner.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  c: TAdvSmoothSpinnerColumn;
  i: integer;
begin
  inherited;
  SetFocus;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;
    
  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      FMouseDown := true;
      FDragY := Y;
      FScrollY := Y;
      FTimeStart := GetTickCount;
      FClickY := Y;
      FClickX := X;
    end;
  end;
end;

procedure TAdvSmoothSpinner.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  c: TAdvSmoothSpinnerColumn;
  pos: integer;
begin
  inherited;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;

  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      if (csDesigning in ComponentState) then
      begin
        FMouseDown := false;
        FMouseUp := false;
        exit;
      end;

      if FMouseDown then
      begin
        if ((FDragY < y-ColumnAppearance.TextSpacing / 2) or (FDragY > Y+ColumnAppearance.TextSpacing / 2)) or SmoothScrolling then
        begin
          FSp := 2;
          FAnimate := false;

          pos := FCurrentScPos;

          if (Y - FDragY) > 0 then
          begin
            pos := pos - Abs(Y - FDragY);
            if not SmoothScrolling then
            begin
              while not ((pos mod ColumnAppearance.TextSpacing) = 0) do
                pos := pos - 1;
            end;
          end
          else
          begin
            pos := pos + Abs(Y - FDragY);
            if not SmoothScrolling then
            begin
              while not ((pos mod ColumnAppearance.TextSpacing) = 0) do
                pos := pos + 1;
            end;
          end;

          if Cyclic then
          begin
            if pos = (GetRangeCount + 1) * ColumnAppearance.TextSpacing then
              pos := 0
            else if pos = -ColumnAppearance.TextSpacing then
              pos := GetRangeCount * ColumnAppearance.TextSpacing;
          end;

          if (SmoothScrolling and not Cyclic) or not SmoothScrolling then
            pos := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, pos));

          if not SmoothScrolling then
          begin
            FSelectedValue := IncSteps(GetRangeFrom, pos div ColumnAppearance.TextSpacing);

            if Assigned(FOnSelectedValueChanged) then
              FOnSelectedValueChanged(Self, c.Index, Round(FSelectedvalue - GetRangeFrom), FSelectedValue, RangeType);
          end;

          FCurrentScPos := pos;
          FScPosTo := FCurrentScPos;
          if SmoothScrolling then
          begin
            if (Y - FDragY) > 0 then
            begin
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo - 1;
            end
            else
            begin
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo + 1;
            end;
          end;

          FDragY := Y;
          Changed;
        end;
      end
      else
      begin
        if FMouseUp then
        begin
          FMouseUp := false;

          if ((FTimeStop - FTimeStart) > 500) or ((FTimeStop - FTimeStart) = 0) then
            exit;

          FSp := Abs(Y - FScrollY) / (FTimeStop - FTimeStart);
          if FSp > 0 then
          begin
            if (Y - FScrollY) > 0 then
            begin
              FScPosTo := FScPosTo - Round(Abs(Y - FScrollY) * FSp);
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo - 1;
            end
            else
            begin
              FScPosTo := FScPosTo + Round(Abs(Y - FScrollY) * FSp);
              while not ((FScPosTo mod ColumnAppearance.TextSpacing) = 0) do
                FScPosTo := FScPosTo + 1;
            end;

            if not Cyclic then
              FScPosTo := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
          end;
        end;
      end;
    end;
  end;

  if i <> FHoveredColumn then
  begin
    FHoveredColumn := i;
    Application.CancelHint;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i: integer;
  c: TAdvSmoothSpinnerColumn;
  r: TGPRectF;
begin
  inherited;
  if (csDesigning in ComponentState) or (Columns.Count = 0) or ReadOnly then
    Exit;

  i := XYToColumn(X, Y);
  if i <> -1 then
  begin
    c := Columns[i];
    with c do
    begin
      if not c.Enabled then
        Exit;

      if FAnimating and FAnimate then
      begin
        FAnimate := false;
        FScrollY := FCurrentScPos;
        FScPosTo := FCurrentScPos;
        FTimeStart := 0;
        FTimeStop := 0;
      end;
      FMouseDown := false;
      FMouseUp := true;
      FTimeStop := GetTickCount;
      FAnimate := (FTimeStop - FTimeStart > 0);
    end;
    FFocusedColumn := i;
    if not FFocused then
    begin
      FFocused := true;
      SetFocus;
    end
    else
    begin
      with Columns[i] do
      begin
        if Abs(FScrollY - Y) <= 2 then
        begin
          r := GetColumnRect;
          if PtInRect(Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(30)), Point(X, Y)) then
            Previous(false);

          if PtInRect(Bounds(Round(r.X), Round(r.Height - 30), Round(r.Width), Round(30)), Point(X, Y)) then
            Next(false);
        end;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  if ReadOnly then
    Exit;

  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if (FHoveredColumn > -1) and (FHoveredColumn < Columns.Count) then
      begin
        with Columns[FHoveredColumn] do
        begin
          if integer(Message.WParam) < 0 then
          begin
            FSp := FAnimationFactor;
            FScPosTo := FScPosTo + ColumnAppearance.TextSpacing
          end
          else
          begin
            FSp := FAnimationFactor;
            FScPosTo := FScPosTo - ColumnAppearance.TextSpacing
          end;
          if not Cyclic then
            FScPosTo := Max(0, Min(ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
          FAnimate := true;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothSpinner.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FContainer) then
      FContainer := nil;

    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;
  end;
  inherited;
end;

procedure TAdvSmoothSpinner.Paint;
var
  rc: TRect;
  rgnor, rgn: HRGN;
  rnd: integer;
begin
  DrawBackGround;

  rc := InsideRect;

  rnd := 0;
  if Fill.RoundingType <> rtNone then
    rnd := Fill.Rounding;

  rgnor := CreateRectRgn(0, 0, ClientWidth, ClientHeight);
  rgn := CreateRoundRectRgn(rc.Left, rc.Top, rc.Right + 2, rc.Bottom + 2, rnd, rnd);
  SelectObject(Canvas.Handle, rgn);

  DrawColumns;
  if not FMetroStyle then
    DrawTopLayer;

  DrawSelectedLayer;

  DeleteObject(rgn);
//  set to full region
  SelectObject(Canvas.Handle, rgnor);
  DeleteObject(rgnor);
end;

procedure TAdvSmoothSpinner.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothSpinner.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothSpinner.SetAnimationFactor(const Value: integer);
begin
  if FAnimationFactor <> value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.SetBottomLayerFill(const Value: TGDIPfill);
begin
  if FBottomLayerFill <> value then
  begin
    FBottomLayerFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetColorTones(ATones: TColorTones);
var
  I: Integer;
begin
  FMetroStyle := True;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo :=  ATones.Background.BrushColor;
  Fill.ColorMirror :=  ATones.Background.BrushColor;
  Fill.ColorMirrorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  ColumnAppearance.Fill.Color := ATones.Background.BrushColor;
  ColumnAppearance.Fill.ColorTo := ATones.Background.BrushColor;
  ColumnAppearance.Fill.ColorMirror := ATones.Background.BrushColor;
  ColumnAppearance.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  ColumnAppearance.Fill.BorderColor := ATones.Background.BorderColor;

  ColumnAppearance.DisabledFill.Color := ATones.Disabled.BrushColor;
  ColumnAppearance.DisabledFill.ColorTo := ATones.Disabled.BrushColor;
  ColumnAppearance.DisabledFill.ColorMirror := ATones.Disabled.BrushColor;
  ColumnAppearance.DisabledFill.ColorMirrorTo := ATones.Disabled.BrushColor;
  ColumnAppearance.DisabledFill.BorderColor := ATones.Disabled.BorderColor;

  ColumnAppearance.HoverFill.Color := ATones.Selected.BrushColor;
  ColumnAppearance.HoverFill.ColorTo := ATones.Selected.BrushColor;
  ColumnAppearance.HoverFill.ColorMirror := ATones.Selected.BrushColor;
  ColumnAppearance.HoverFill.ColorMirrorTo := ATones.Selected.BrushColor;
  ColumnAppearance.HoverFill.BorderColor :=  ATones.Selected.BorderColor;

  for I := 0 to Columns.Count - 1 do
  begin
    Columns[I].Font.Color := ATones.Background.TextColor;
    Columns[I].Font.Name := GetMetroFont;

    Columns[I].HoverFont.Color := ATones.Selected.TextColor;
    Columns[I].HoverFont.Name := GetMetroFont;
  end;
end;

procedure TAdvSmoothSpinner.SetColumnAppearance(
  const Value: TAdvSmoothSpinnerColumnAppearance);
begin
  if FColumnAppearance <> value then
  begin
    FColumnAppearance.Assign(Value);
    ColumnAppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetColumns(const Value: TAdvSmoothSpinnerColumns);
begin
  if FColumns <> value then
  begin
    FColumns := Value;
    ColumnsChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  //todo office styles
  FMetroStyle := False;
  ColumnAppearance.HoverFill.Glow := gmNone;
  case AStyle of
    tsOffice2003Blue:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $00FFD2AF;
      Fill.ColorMirror := $00FFD2AF;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := clDkGray;

      with ColumnAppearance do
      begin
        Fill.Color := $FCE1CB;
        Fill.ColorTo := $E0A57D;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $962D00;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $962D00;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.BorderColor :=  $962D00;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Silver:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $00E6D8D8;
      Fill.ColorMirror := $00E6D8D8;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := clDkGray;

      with ColumnAppearance do
      begin
        Fill.Color := $ECE2E1;
        Fill.ColorTo := $B39698;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $947C7C;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $947C7C;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.BorderColor :=  $947C7C;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Olive:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $CFF0EA;
      Fill.ColorMirror := $CFF0EA;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $CFF0EA;
        Fill.ColorTo := $8CC0B1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $588060;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $588060;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.BorderColor :=  $947C7C;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Classic:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $00F2F2F2;
      Fill.ColorMirror := $00F2F2F2;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := $C9D1D5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $808080;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $D8D5D4;
        DisabledFill.ColorTo := $D8D5D4;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $808080;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $D2BDB6;
        HoverFill.ColorTo := $D2BDB6;
        HoverFill.BorderColor :=  $808080;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2007Luna:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $DCB698;
      Fill.ColorMirror := $DCB698;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;      

      with ColumnAppearance do
      begin
        Fill.Color := $FFEFE3;
        Fill.ColorTo := $FFDDC4;
        Fill.ColorMirror := $FFD1AD;
        Fill.ColorMirrorTo := $FFDBC0;
        Fill.BorderColor := $FFD1AD;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := $00B6B6B6;
        DisabledFill.ColorMirrorTo := $00F2F2F2;
        DisabledFill.BorderColor := $FFD1AD;//$00B6B6B6;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.ColorMirror := $59DAFF;
        HoverFill.ColorMirrorTo := $A4E9FF;
        HoverFill.BorderColor :=  $99CEDB;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2007Obsidian:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $5C534C;
      Fill.ColorMirror := $5C534C;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $F9F8F8;
        Fill.ColorTo := $E4E2DF;
        Fill.ColorMirror := $D1CBC7;
        Fill.ColorMirrorTo := $E2DEDB;
        Fill.BorderColor := clBlack;//$D1CBC7;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := $00B6B6B6;
        DisabledFill.ColorMirrorTo := $00F2F2F2;
        DisabledFill.BorderColor := clBlack;//$00B6B6B6;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.ColorMirror := $59DAFF;
        HoverFill.ColorMirrorTo := $A4E9FF;
        HoverFill.BorderColor :=  $99CEDB;
        HoverFill.GradientMirrorType := gtVertical;
      end;
      FSelectedFill.Color := clWhite;
      FSelectedfill.Opacity := 40;
      FSelectedFill.GradientType := gtVertical;
      FSelectedFill.ColorTo := clWhite;
      FSelectedfill.OpacityTo := 60;
      FSelectedfill.BorderColor := clsilver;
    end;
    tsWindowsXP:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $00B6B6B6;
      Fill.ColorMirror := $00B6B6B6;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
        Fill.GradientMirrorType := gtVertical;

        HoverFill.Color := clInactiveCaptionText;
        HoverFill.ColorTo := clInactiveCaptionText;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor := clBlack;
        HoverFill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00B6B6B6;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := clBlack;
        DisabledFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWhidbey:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $F5F9FA;
      Fill.ColorMirror := $F5F9FA;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $F5F9FA;
        Fill.ColorTo := $A8C0C0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $962D00;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $962D00;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $94E6FB;
        HoverFill.ColorTo := $1595EE;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor := clBlack;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsCustom: ;
    tsOffice2007Silver:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $00CAC1BA;
      Fill.ColorMirror := $00CAC1BA;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $FAEEEB;
        Fill.ColorTo := $E5DBD7;
        Fill.ColorMirror := $E2D8D4;
        Fill.ColorMirrorTo := $D1C7C5;
        Fill.BorderColor := clBlack;//$E2D8D4;
        Fill.GradientMirrorType := gtVertical;

        HoverFill.Color := $EBFDFF;
        HoverFill.ColorTo := $ACECFF;
        HoverFill.ColorMirror := $59DAFF;
        HoverFill.ColorMirrorTo := $A4E9FF;
        HoverFill.BorderColor := clBlack;//$42AEFE;
        HoverFill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := $00B6B6B6;
        DisabledFill.ColorMirrorTo := $00F2F2F2;
        DisabledFill.BorderColor := clBlack;//$00B6B6B6;
        DisabledFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWindowsVista:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $FFFDF9;
      Fill.ColorMirror := $FFFDF9;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $FFFDF9;
        Fill.ColorTo := $FFFAF0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FCF2DA;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $588060;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $FEF9F0;
        HoverFill.ColorTo := $FDF0D7;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $FEDF9A;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWindows7:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $FCEBDC;
      Fill.ColorMirror := $FCDBC1;
      Fill.ColorMirrorTo := clBlack;
      Fill.BorderColor := clDkGray;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;

      with ColumnAppearance do
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $CEA27D;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $588060;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $FDFBFA;
        HoverFill.ColorTo := $FDF3EB;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $FBD6B8;
        HoverFill.GradientMirrorType := gtVertical;
      end;
    end;
    tsTerminal:
    begin
      Fill.Color := clBtnFace;
      Fill.ColorTo := clBtnFace;
      Fill.ColorMirror := clNone;
      Fill.ColorMirrorTo := clNone;
      Fill.BorderColor := clGray;

      with ColumnAppearance do
      begin
        Fill.Color := clSilver;
        Fill.ColorTo := clSilver;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;

        DisabledFill.Color := clBtnFace;
        DisabledFill.ColorTo := clBtnFace;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := clGray;

        HoverFill.Color := clHighLight;
        HoverFill.ColorTo := clHighLight;
        HoverFill.ColorMirror := clNone;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  clGray;

      end;
    end;
    tsOffice2010Blue:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo :=  $EAD3BF;
      Fill.ColorMirror :=  $EAD3BF;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := clDkGray;

      with ColumnAppearance do
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C7B29F;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $962D00;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := RGB(253, 227, 138);
        HoverFill.ColorTo := clNone;
        HoverFill.BorderColor :=  RGB(242, 205, 96);
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;
    tsOffice2010Silver:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $D4CFCB;
      Fill.ColorMirror := $D4CFCB;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := clDkGray;

      with ColumnAppearance do
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D2CDC8;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $962D00;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := RGB(253, 227, 138);
        HoverFill.ColorTo := clNone;
        HoverFill.BorderColor :=  RGB(242, 205, 96);
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;
    tsOffice2010Black:
    begin
      Fill.Color := clBlack;
      Fill.ColorTo := $656565;
      Fill.ColorMirror := $656565;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := clDkGray;

      with ColumnAppearance do
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $6D6D6D;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $00F2F2F2;
        DisabledFill.ColorTo := $00B6B6B6;
        DisabledFill.ColorMirror := clNone;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $962D00;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := RGB(253, 227, 138);
        HoverFill.ColorTo := clNone;
        HoverFill.BorderColor :=  RGB(242, 205, 96);
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;

    tsWindows8, tsWindows10:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := clWhite;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $DCDBDA;

      with ColumnAppearance do
      begin
        Fill.Color := $F7F6F5;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := $F7F6F5;
        Fill.ColorMirrorTo := clNone;
        Fill.GradientType := gtVertical;
        Fill.GradientMirrorType := gtVertical;
        Fill.BorderColor := $E4E3E2;

        DisabledFill.Color := $F7F7F7;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $F7F7F7;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $DEDEDE;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $F7EFE8;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $F7EFE8;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $F9CEA4;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;

    tsOffice2013White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := clWhite;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $D4D4D4;

      with ColumnAppearance do
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clWhite;
        Fill.ColorMirrorTo := clNone;
        Fill.GradientType := gtVertical;
        Fill.GradientMirrorType := gtVertical;
        Fill.BorderColor := $D4D4D4;


        DisabledFill.Color := $EEEEEE;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $EEEEEE;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $ACACAC;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $FCF0E4;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $FCF0E4;;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $EAB47E;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;

    tsOffice2013LightGray:
    begin
      Fill.Color := $F6F6F6;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := $F6F6F6;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $C6C6C6;

      with ColumnAppearance do
      begin
        Fill.Color := $F6F6F6;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := $F6F6F6;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C6C6C6;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $EEEEEE;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $EEEEEE;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $ACACAC;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $FCF0E4;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $FCF0E4;;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $EAB47E;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
       end;
    end;

    tsOffice2013Gray:
    begin
      Fill.Color := $E5E5E5;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := $E5E5E5;
      Fill.ColorMirrorTo := clBlack;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $ABABAB;

      with ColumnAppearance do
      begin
        Fill.Color := $E5E5E5;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := $E5E5E5;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $ABABAB;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $EEEEEE;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $EEEEEE;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $ACACAC;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $FCF0E4;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $FCF0E4;;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $EAB47E;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
       end;
    end;

   tsOffice2016White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := clWhite;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $D4D4D4;

      with ColumnAppearance do
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := clWhite;
        Fill.ColorMirrorTo := clNone;
        Fill.GradientType := gtVertical;
        Fill.GradientMirrorType := gtVertical;
        Fill.BorderColor := $D4D4D4;


        DisabledFill.Color := clWhite;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := clWhite;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $D4D4D4;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $F2E1D5;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $F2E1D5;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $F2E1D5;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;

    tsOffice2016Gray:
    begin
      Fill.Color := $B2B2B2;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := $B2B2B2;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $444444;

      with ColumnAppearance do
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := $B2B2B2;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $B2B2B2;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $B2B2B2;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $444444;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $F2E1D5;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $F2E1D5;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $F2E1D5;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
       end;
    end;

    tsOffice2016Black:
    begin
      Fill.Color := $363636;
      Fill.ColorTo := clNone;
      Fill.ColorMirror := $363636;
      Fill.ColorMirrorTo := clNone;
      Fill.GradientType := gtVertical;
      Fill.GradientMirrorType := gtVertical;
      Fill.BorderColor := $4E4E4E;

      with ColumnAppearance do
      begin
        Fill.Color := $363636;
        Fill.ColorTo := clNone;
        Fill.ColorMirror := $363636;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
        Fill.GradientMirrorType := gtVertical;

        DisabledFill.Color := $363636;
        DisabledFill.ColorTo := clNone;
        DisabledFill.ColorMirror := $363636;
        DisabledFill.ColorMirrorTo := clNone;
        DisabledFill.BorderColor := $444444;
        DisabledFill.GradientMirrorType := gtVertical;

        HoverFill.Color := $6A6A6A;
        HoverFill.ColorTo := clNone;
        HoverFill.ColorMirror := $6A6A6A;
        HoverFill.ColorMirrorTo := clNone;
        HoverFill.BorderColor :=  $6A6A6A;
        HoverFill.GradientMirrorType := gtVertical;
        HoverFill.Glow := gmRadialGradient;
      end;
    end;

  end;

  //top and selected
  case AStyle of
    tsCustom:;
    else
    begin
      FSelectedFill.Color := clWhite;
      FSelectedfill.Opacity := 40;
      FSelectedFill.GradientType := gtVertical;
      FSelectedFill.ColorTo := clWhite;
      FSelectedfill.OpacityTo := 60;
      FSelectedfill.BorderColor := clsilver;
    end;
  end;

  TopLayerFill.Color := clblack;
  TopLayerFill.Opacity := 200;
  TopLayerFill.GradientType := gtVertical;
  TopLayerFill.ColorTo := clwhite;
  TopLayerFill.OpacityTo := 10;

  BottomLayerFill.Color := clWhite;
  BottomLayerFill.Opacity := 10;
  BottomLayerFill.GradientType := gtVertical;
  BottomLayerFill.ColorTo := clblack;
  BottomLayerFill.OpacityTo := 200;
end;

procedure TAdvSmoothSpinner.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.SetSelectedFill(const Value: TGDIPFill);
begin
  if FSelectedFill <> value then
  begin
    FSelectedFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetSelectedHeight(const Value: integer);
begin
  if FSelectedHeight <> value then
  begin
    FSelectedHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.SetSmoothScrolling(const Value: Boolean);
begin
  if FSmoothScrolling <> Value then
  begin
    FSmoothScrolling := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinner.SetTopLayerFill(const Value: TGDIPFill);
begin
  if FTopLayerFill <> Value then
  begin
    FTopLayerFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinner.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothSpinner.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if ReadOnly then
    Exit;
    
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothSpinner.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure TAdvSmoothSpinner.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothSpinner.XYToColumn(X, Y: integer): integer;
var
  I: Integer;
  r: TGPRectF;
begin
  Result := -1;
  for I := 0 to Columns.Count - 1 do
  begin
    with FColumns[i] do
    begin
      if FColumns[i].Visible and Enabled then
      begin
        r := GetColumnRect;
        if PtInRect(Bounds(Round(R.X), Round(R.Y), Round(R.Width), Round(R.Height)), Point(X, Y)) then
        begin
          result := I;
          break;
        end;
      end;
    end;
  end;
end;

{ TAdvSmoothSpinnerAppearance }

procedure TAdvSmoothSpinnerColumnAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSpinnerColumnAppearance) then
  begin
    FAutoSize := (Source as TAdvSmoothSpinnerColumnAppearance).AutoSize;
    FAllowHovering := (Source as TAdvSmoothSpinnerColumnAppearance).AllowHovering;
    FSpacing := (Source as TAdvSmoothSpinnerColumnAppearance).Spacing;
    FFill.Assign((Source as TAdvSmoothSpinnerColumnAppearance).Fill);
    FHoverFill.Assign((Source as TAdvSmoothSpinnerColumnAppearance).HoverFill);
    FDisabledFill.Assign((Source as TAdvSmoothSpinnerColumnAppearance).DisabledFill);
    FTextSpacing := (Source as TAdvSmoothSpinnerColumnAppearance).TextSpacing;
    FImageWidth := (Source as TAdvSmoothSpinnerColumnAppearance).ImageWidth;
    FImageHeight := (Source as TAdvSmoothSpinnerColumnAppearance).ImageHeight;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothSpinnerColumnAppearance.Create(AOwner: TAdvSmoothSpinner);
begin
  FOwner := AOwner;
  FAutoSize := True;
  FSpacing := 5;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FDisabledFill := TGDIPFill.Create;
  FDisabledFill.OnChange := FillChanged;
  FHoverFill := TGDIPFill.Create;
  FHoverFill.OnChange := FillChanged;
  FTextSpacing := 25;
  FImageWidth := 30;
  FImageHeight := 30;  
  FAllowHovering := true;
end;

destructor TAdvSmoothSpinnerColumnAppearance.Destroy;
begin
  FFill.Free;
  FHoverFill.Free;
  FDisabledFill.Free;
  inherited;
end;

procedure TAdvSmoothSpinnerColumnAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSpinnerColumnAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetAllowHovering(
  const Value: Boolean);
begin
  if FAllowHovering <> value then
  begin
    FAllowHovering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetDisabledFill(
  const Value: TGDIPFill);
begin
  if FDisabledFill <> value then
  begin
    FDisabledFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetHoverFill(
  const Value: TGDIPFill);
begin
  if FHoverFill <> value then
  begin
    FHoverFill.Assign(value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetImageHeight(
  const Value: integer);
begin
  if FImageHeight <> value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetImageWidth(const Value: integer);
begin
  if FImageWidth <> value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetTextSpacing(
  const Value: integer);
var
  I: Integer;
begin
  if FTextSpacing <> value then
  begin
    FTextSpacing := Max(1, Value);
    for I := 0 to FOwner.Columns.Count - 1 do
    begin
      with FOwner.Columns[i] do
      begin
        FCurrentScPos := StepsFromTo(GetRangeFrom, FSelectedValue) * FOwner.ColumnAppearance.TextSpacing;
        FScPosTo := FcurrentScPos;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumnAppearance.SetSpacing(const Value: integer);
begin
  if FSpacing <> value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

{ TAdvSmoothSpinnerColumn }

procedure TAdvSmoothSpinnerColumn.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSpinnerColumn) then
  begin
    FEnabled := (Source as TAdvSmoothSpinnerColumn).Enabled;
    FVisible := (Source as TAdvSmoothSpinnerColumn).Visible;
    FWidth := (Source as TAdvSmoothSpinnerColumn).Width;
    FRangeFrom := (Source as TAdvSmoothSpinnerColumn).RangeFrom;
    FRangeTo := (Source as TAdvSmoothSpinnerColumn).RangeTo;
    FSelectedValue := (Source as TAdvSmoothSpinnerColumn).SelectedValue;
    FRangeType := (Source as TAdvSmoothSpinnerColumn).RangeType;
    FDateRangeTo := (Source as TAdvSmoothSpinnerColumn).DateRangeTo;
    FDateRangeFrom := (Source as TAdvSmoothSpinnerColumn).DateRangeFrom;
    FDateTimeValueFormat := (Source as TAdvSmoothSpinnerColumn).DateTimeValueFormat;
    FCustomItems.Assign((Source as TAdvSmoothSpinnerColumn).CustomItems);
    FCyclic := (Source as TAdvSmoothSpinnerColumn).Cyclic;
    FFont.Assign((Source as TAdvSmoothSpinnerColumn).Font);
    FTextAlign := (Source as TAdvSmoothSpinnerColumn).TextAlign;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothSpinnerColumn.Create(Collection: TCollection);
begin
  inherited;
  FDrawIndex := Index;  
  Fowner := (Collection as TAdvSmoothSpinnerColumns).FOwner;
  FEnabled := true;
  FVisible := true;
  FWidth := 20;
  FRangeFrom := 0;
  FRangeTo := 20;
  FRangeType := rtNumber;
  FStep := 1;
  Fowner.Changed;
  FValueFormat := '%g';
  FDateRangeFrom := Now;
  FDateRangeTo := Now + 20;
  FSaveDateRangeto := FDateRangeto;
  FSaveDateRangefrom := FDateRangeFrom;
  FDateTimeValueFormat := 'dd/mm/yy';
  FStepType := stNumber;
  FCustomItems := TAdvSmoothSpinnerCustomItems.Create(FOwner);
  FCustomItems.OnChange := CustomItemsChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FHoverFont := TFont.Create;
  FHoverFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FOnlyDate := false;
  FTextAlign := taCenter;
end;

procedure TAdvSmoothSpinnerColumn.CustomItemsChanged(Sender: TObject);
begin
  Changed;
end;

destructor TAdvSmoothSpinnerColumn.Destroy;
begin
  Fowner.Changed;
  FCustomItems.Free;
  FFont.Free;
  FHoverFont.Free;
  inherited;
end;

procedure TAdvSmoothSpinnerColumn.Draw(g: TGPGraphics; r: TGPRectF);
var
  I: Integer;
  f: TGPFont;
  fs, dw: integer;
  sf: TGPStringFormat;
  ff: TGPFontFamily;
  b: TGPSolidBrush;
  s: String;
  sri: TGPRectF;
  pttext, ptimg: TGPPointF;
  d: Double;
  ypostext, yposimg: Double;
  sci, cnt, j, si, siw, sih: integer;
  focus: Boolean;
  st: String;
  cdt, cdf: integer;
  fnt: TFont;
begin
  with FOwner.FColumnAppearance do
  begin
    //fill
    fnt := Font;
    focus := FOwner.TabStop and FOwner.FFocused and (FOwner.FFocusedColumn = Index);
    if FEnabled and FOwner.Enabled then
    begin
      if (FOwner.FHoveredColumn = Index) and AllowHovering then
      begin
        FhoverFill.BeginUpdate;
        FhoverFill.FocusRect := r;
        FHoverFill.Focus := focus;
        FHoverFill.EndUpdate;
        FHoverFill.Fill(g, r);
        fnt := HoverFont;
      end
      else
      begin
        FFill.BeginUpdate;
        FFill.FocusRect := r;
        FFill.Focus := focus;
        FFill.EndUpdate;
        FFill.Fill(g, r);
      end
    end
    else
      FDisabledFill.Fill(g, r);

    if RangeType <> rtCustom then
    begin
      if GetRangeCount <= 0 then
        Exit;
    end;

    //textfont not selected
    ff := TGPFontFamily.Create(fnt.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in fnt.Style) then
      fs := fs + 1;
    if (fsItalic in fnt.Style) then
      fs := fs + 2;
    if (fsUnderline in fnt.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, fnt.Size, fs, UnitPoint);
    b := TGPSolidBrush.Create(ColorToARGB(fnt.Color));
    g.MeasureString('gh', length('gh'), f, MakeRect(0, 0, 10000, 10000), sf, sri);

    cnt := Round((FOwner.InsideRect.Bottom - FOwner.InsideRect.Top) / ((TextSpacing / 2) + sri.Height)) + 5;
    i := (FCurrentScPos div TextSpacing) - (cnt div 2);
    j := 0;
    si := -1;
    siw := 0;
    sih := 0;
    while j <= cnt do
    begin
      if (i < 0) and Cyclic then
        sci := ((i mod (GetRangeCount + 1) + GetRangeCount + 1) mod (GetRangeCount + 1))
      else if (i > GetRangeCount) and Cyclic then
        sci := ((i mod (GetRangeCount + 1)) mod (GetRangeCount + 1))
      else
        sci := i;

      d := IncSteps(GetRangeFrom, sci);
      if (RangeType = rtDateTime) then
      begin
        cdt := CompareDateTime(d, GetRangeTo);
        cdf := CompareDateTime(d, GetRangeFrom);
      end
      else
      begin
        cdt := CompareValue(d, GetRangeTo);
        cdf := CompareValue(d, GetRangeFrom);
      end;

      if (((cdf = 0) or (cdf = 1)) and ((cdt = 0) or (cdt = -1))) or Cyclic then
      begin
        case RangeType of
          rtNumber:
          begin
            if pos('d',ValueFormat) > 0 then
            begin
              dw := round(d);
              try
                s := Format(ValueFormat,[dw]);
              except
                s := '';
              end;
            end
            else
            begin
              try
                s := Format(ValueFormat, [d]);
              except
                s := '';
              end;
            end;
          end;
          rtDateTime:
          begin
            try
              s := FormatDateTime(DateTimeValueFormat, d);
            except
              s := '';
            end;
          end;
          rtCustom:
          begin
            with CustomItems[sci] do
            begin
              s := Text;
              if Assigned(FOwner.FImages) then
              begin
                si := ImageIndex;
                if (si > -1) and (si < FOwner.FImages.Count) then
                begin
                  siw := FOwner.FImages.Width;
                  sih := FOwner.FImages.Height;
                end;
              end
              else if Assigned(FOwner.FContainer) then
              begin
                st := PictureName;
                if Assigned(FOwner.FContainer.FindPicture(st)) then
                begin
                  siw := ImageWidth;
                  sih := ImageHeight;
                end;
              end;
            end;
          end;
        end;
        
        if s <> '' then
        begin
          ypostext := -FCurrentScPos + (i * TextSpacing) + (FOwner.InsideRect.Bottom - (sri.Height)) / 2;
          g.MeasureString(s, length(s), f, MakeRect(0, 0, 10000, 10000), sf, sri);
          case TextAlign of
            taLeftJustify: pttext := MakePoint(r.X, ypostext);
            taRightJustify: pttext := MakePoint(r.X + ((r.Width - sri.Width - siw)), ypostext);
            taCenter: pttext := MakePoint(r.X + ((r.Width - sri.Width - siw) / 2), ypostext);
          end;
        end;

        yposimg := -FCurrentScPos + (i * TextSpacing) + (FOwner.InsideRect.Bottom - (sih)) / 2;
        if s <> '' then
          ptimg := MakePoint(pttext.X, yposimg)
        else
          ptimg := MakePoint(r.X + ((r.Width - siw) / 2), yposimg);

        if Assigned(FOwner.FContainer) then
        begin
          if Assigned(FOwner.Fcontainer.FindPicture(st)) then
            FOwner.FContainer.FindPicture(st).GDIPDraw(g, Bounds(Round(ptimg.x), Round(ptimg.y), siw, sih));
        end
        else if Assigned(FOwner.FImages) then
        begin
          if (si > -1) and (si < FOwner.FImages.Count) then
            FOwner.FImages.Draw(FOwner.Canvas, Round(ptimg.x), Round(ptimg.y), si);
        end;

        pttext.X := pttext.X + siw;
        if s <> '' then
          g.DrawString(s, Length(s), f, pttext, sf, b);
      end;

      Inc(j);
      Inc(i);
    end;

    b.Free;
    f.Free;
    sf.Free;
    ff.Free;
  end;
end;

procedure TAdvSmoothSpinnerColumn.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothSpinnerColumn.GetColumnRect: TGPRectF;
var
  r: TRect;
  s, st: Double;
  i: integer;
  cc: integer;
begin
  with FOwner do
  begin
    cc := 0;
    for i := 0 to Columns.Count - 1 do
      if Columns[i].Visible then
        Inc(cc);

    r := Fowner.InsideRect;
    if ColumnAppearance.AutoSize then
    begin
      s := (r.Right - r.Left - ((cc + 1) * ColumnAppearance.Spacing)) / cc;
      st := r.Left + (FDrawIndex * s) + (FDrawIndex + 1) * ColumnAppearance.Spacing;
    end
    else
    begin
      s := Self.Width;
      i := FDrawIndex;
      st := r.Left;
      while (i > 0) do
      begin
        st := st + Columns[i - 1].Width + ColumnAppearance.Spacing;
        Dec(i);
      end;
    end;
    
    Result := Makerect(st, r.Top, s, r.Bottom - R.Top);
  end;
end;

function TAdvSmoothSpinnerColumn.GetRangeCount: integer;
begin
  result := StepsFromTo(GetRangeFrom, GetRangeTo);
end;

function TAdvSmoothSpinnerColumn.GetRangeFrom: Double;
begin
  Result := 0;
  case RangeType of
    rtNumber: result := RangeFrom;
    rtDateTime:
    begin
      if OnlyDate then
        result := Int(FSaveDateRangeFrom)
      else
        result := FSaveDateRangeFrom;
    end;
    rtCustom: result := 0;
  end;
end;

function TAdvSmoothSpinnerColumn.GetRangeTo: Double;
begin
  Result := 0;
  case RangeType of
    rtNumber: result := RangeTo;
    rtDateTime:
    begin
      if OnlyDate then
        result := int(FSaveDateRangeto)
      else
        result := FSaveDateRangeTo;
    end;
    rtCustom: Result := CustomItems.Count - 1;
  end;
end;

function TAdvSmoothSpinnerColumn.GetSelectedCustomIndex: integer;
begin
  Result := Round(SelectedValue - GetRangeFrom);
end;

function TAdvSmoothSpinnerColumn.GetSelectedDateTime: TDateTime;
begin
  result := SelectedValue;
end;

function TAdvSmoothSpinnerColumn.GetSelectedValue: Double;
begin
  Result := Max(GetRangeFrom, Min(FSelectedValue, GetRangeTo));
end;

function TAdvSmoothSpinnerColumn.IncSteps(StartValue: double; nr: integer): double;
var
  i: integer;
  d: integer;
begin
  case StepType of
    stMonth:
    begin
      if (nr > 0) then d := 1 else d := -1;
      for i := 1 to abs(nr) do
        StartValue := incmonth(StartValue, d);
      result := StartValue;
    end;
    stYear:
    begin
      if (nr > 0) then d := 1 else d := -1;
      for i := 1 to abs(nr) do
        startvalue := incyear(StartValue, d);
      result := StartValue;
    end;
    else result := StartValue + (nr * GetStep);
  end;
end;


function TAdvSmoothSpinnerColumn.StepsFromTo(startvalue, endvalue: double): integer;
var
  steps: integer;
begin
  case StepType of
  stMonth:
  begin
    steps := 0;
    while (startvalue < endvalue) do
    begin
      startvalue := incmonth(startvalue);
      Inc(steps);
      if startvalue >= endvalue then
        Break;
    end;
  end;
  stYear:
  begin
    steps := 0;
    while (startvalue < endvalue) do
    begin
      startvalue := incyear(startvalue);
      Inc(steps);
      if startvalue >= endvalue then
        Break;
    end;
  end;
  else Steps := Round(Abs(endvalue - startvalue) / getstep);
  end;

  Result := Steps;
end;


function TAdvSmoothSpinnerColumn.GetStep: Double;
begin
  Result := 1;
  case StepType of
    stNumber: Result := Step;
    stSecond: Result := Step / 86400;
    stMinute: Result := Step / 1440;
    stHour: Result := Step / 24;
    stDay: Result := Step;
  end;
end;

procedure TAdvSmoothSpinnerColumn.Next(Animation: Boolean = true);
begin
  ScrollToValue(IncSteps(SelectedValue, 1), Animation);
end;

procedure TAdvSmoothSpinnerColumn.Previous(Animation: Boolean = true);
begin
  ScrollToValue(IncSteps(SelectedValue, -1), Animation);
end;

procedure TAdvSmoothSpinnerColumn.ScrollToValue(Value: double; Animation: Boolean = true; AnimationSpeed: integer = 4);
begin
  if (FSelectedValue <> value) and Enabled and Visible then
  begin
    Fanimate := Animation;
    FSp := AnimationSpeed;

    FSelectedValue := Max(GetRangeFrom, Min(Value, GetRangeTo));

    if Value < GetRangeFrom then
      FScPosTo := -FOwner.ColumnAppearance.TextSpacing
    else
      FScPosTo := StepsFromTo(GetRangeFrom, Value) * FOwner.ColumnAppearance.TextSpacing;

    if not Cyclic then
    begin
      FScPosTo := Max(0, Min(FOwner.ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
      FSelectedValue := Min(Max(Value, RangeFrom), RangeTo);
    end;

    if not Animation then
    begin
      if Cyclic then
      begin
        if FScPosTo = (GetRangeCount + 1) * FOwner.ColumnAppearance.TextSpacing then
          FScPosTo := 0
        else if FScPosTo = -FOwner.ColumnAppearance.TextSpacing then
          FScPosTo := GetRangeCount * FOwner.ColumnAppearance.TextSpacing;
      end;

      FScPosTo := Max(0, Min(Fowner.ColumnAppearance.TextSpacing * GetRangeCount, FScPosTo));
      FSelectedValue := IncSteps(GetRangeFrom, FScPosTo div Fowner.ColumnAppearance.TextSpacing);
      FCurrentScPos := FScPosTo;

      if Assigned(FOwner.FOnSelectedValueChanged) then
        FOwner.FOnSelectedValueChanged(Self, Index, Round(FSelectedValue - GetRangeFrom), FSelectedValue, RangeType);
      Changed;
    end;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetAnimatedValue;
begin
  FSelectedValue := Max(GetRangeFrom, Min(IncSteps(GetRangeFrom, FScPosTo div FOwner.ColumnAppearance.TextSpacing), GetRangeTo));
end;

procedure TAdvSmoothSpinnerColumn.SetCustomItems(
  const Value: TAdvSmoothSpinnerCustomItems);
begin
  if FCustomItems <> value then
  begin
    FCustomItems.Assign(Value);
    CustomItemsChanged(Self);
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetCyclic(const Value: Boolean);
begin
  if FCyclic <> value then
  begin
    FCyclic := value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetDateRangeFrom(const Value: TDateTime);
begin
  if FDateRangeFrom <> Value then
  begin
    FDateRangeFrom := Value;
    FSaveDateRangeFrom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetDateRangeTo(const Value: TDateTime);
begin
  if FDateRangeTo <> value then
  begin
    FDateRangeTo := Value;
    FSaveDateRangeto := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetDateTimeValueFormat(const Value: String);
begin
  if FDateTimeValueFormat <> value then
  begin
    FDateTimeValueFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetHint(const Value: String);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetHoverFont(const Value: TFont);
begin
  if FHoverFont <> Value then
  begin
    FHoverFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetOnlyDate(const Value: Boolean);
begin
  if FOnlyDate <> value then
  begin
    FOnlyDate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetRangeFrom(const Value: Double);
begin
  if FRangeFrom <> value then
  begin
    FRangeFrom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetRangeTo(const Value: Double);
begin
  if FRangeTo <> value then
  begin
    FRangeTo := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetRangeType(
  const Value: TAdvSmoothSpinnerRangeType);
begin
  if FRangeType <> value then
  begin
    FRangeType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetSelectedCustomIndex(const Value: integer);
begin
  FSelectedValue := Value;
end;

procedure TAdvSmoothSpinnerColumn.SetSelectedDateTime(const Value: TDateTime);
begin
  FSelectedValue := value;
end;

procedure TAdvSmoothSpinnerColumn.SetSelectedValue(const Value: double);
begin
  if (FSelectedValue <> value) and (Value <= GetRangeTo) and (Value >= GetRangeFrom) then
  begin
    FSelectedValue := Value;
    if FSelectedValue <> -1 then
    begin
      if Abs(GetRangeTo - GetRangeFrom) = 0 then
      begin
        FSelectedValue := -1;
      end
      else
      begin
        FCurrentScPos := StepsFromTo(GetRangeFrom, FSelectedValue) * FOwner.ColumnAppearance.TextSpacing;
        FScPosTo := FcurrentScPos;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetStep(const Value: Double);
begin
  if (FStep <> value) and (value > 0) then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetStepType(
  const Value: TAdvSmoothSpinnerStepType);
begin
  if FStepType <> value then
  begin
    FStepType := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetTextAlign(const Value: TAlignment);
begin
  if FTextAlign <> value then
  begin
    FTextAlign := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetValueFormat(const Value: String);
begin
  if FValueFormat <> value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerColumn.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothSpinnerColumns }

function TAdvSmoothSpinnerColumns.Add: TAdvSmoothSpinnerColumn;
begin
  Result := TAdvSmoothSpinnerColumn(inherited Add);
end;

constructor TAdvSmoothSpinnerColumns.Create(AOwner: TAdvSmoothSpinner);
begin
  inherited Create(TAdvSmoothSpinnerColumn);
  FOwner := AOwner;
end;

procedure TAdvSmoothSpinnerColumns.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothSpinnerColumns.GetItem(
  Index: Integer): TAdvSmoothSpinnerColumn;
begin
  Result := TAdvSmoothSpinnerColumn(inherited Items[Index]);
end;

function TAdvSmoothSpinnerColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothSpinnerColumns.Insert(
  Index: Integer): TAdvSmoothSpinnerColumn;
begin
  Result := TAdvSmoothSpinnerColumn(inherited Insert(Index));
end;

procedure TAdvSmoothSpinnerColumns.SetItem(Index: Integer;
  const Value: TAdvSmoothSpinnerColumn);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothSpinnerColumns.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

{ TAdvSmoothSpinnerCustomItem }

procedure TAdvSmoothSpinnerCustomItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSpinnerCustomItem) then
  begin
    FText := (Source as TAdvSmoothSpinnerCustomItem).Text;
    FValue := (Source as TAdvSmoothSpinnerCustomItem).Value;
    FImageIndex := (Source as TAdvSmoothSpinnerCustomItem).ImageIndex;
    FpictureName := (source as TAdvSmoothSpinnerCustomItem).PictureName;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerCustomItem.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothSpinnerCustomItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothSpinnerCustomItems).FOwner;
  FImageIndex := -1;
end;

destructor TAdvSmoothSpinnerCustomItem.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothSpinnerCustomItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerCustomItem.SetPictureName(const Value: string);
begin
  if FpictureName <> value then
  begin
    FpictureName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerCustomItem.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSpinnerCustomItem.SetValue(const Value: Double);
begin
  if FValue <> value then
  begin
    FValue := Value;
    Changed;
  end;
end;

{ TAdvSmoothSpinnerCustomItems }

function TAdvSmoothSpinnerCustomItems.Add: TAdvSmoothSpinnerCustomItem;
begin
  Result := TAdvSmoothSpinnerCustomItem(inherited Add);
end;

constructor TAdvSmoothSpinnerCustomItems.Create(AOwner: TAdvSmoothSpinner);
begin
  inherited Create(TAdvSmoothSpinnerCustomItem);
  FOwner := AOwner;
end;

procedure TAdvSmoothSpinnerCustomItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothSpinnerCustomItems.GetItem(
  Index: Integer): TAdvSmoothSpinnerCustomItem;
begin
  Result := TAdvSmoothSpinnerCustomItem(inherited Items[Index]);
end;

function TAdvSmoothSpinnerCustomItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothSpinnerCustomItems.Insert(
  Index: Integer): TAdvSmoothSpinnerCustomItem;
begin
  Result := TAdvSmoothSpinnerCustomItem(inherited Insert(Index));
end;

procedure TAdvSmoothSpinnerCustomItems.SetItem(Index: Integer;
  const Value: TAdvSmoothSpinnerCustomItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvSmoothSpinnerCustomItems.Update(Item: TCollectionItem);
begin
  FOwner.Changed;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.

