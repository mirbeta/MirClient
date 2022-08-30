{***************************************************************************}
{ TAdvDBFormPanel component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014 - 2015                                        }
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

unit AdvDBFormPanel;

{$I TMSDEFS.INC}

interface

uses
  Classes, StdCtrls, Windows, ExtCtrls, Controls, Messages, DB, DBCtrls,
  AdvDBFormLayouter, Dialogs, Forms, Graphics, AdvLabelEdit, Generics.Collections, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : Handling different Windows DPI modes for control size
  // v1.0.1.1 : Fixed : Rare painting issue

type
  TAdvDBFormBox = class;

  TAdvDBFormPanel = class;

  TAdvDBFormPanelCaption = class;

  TEditMode = (emOn, emOff);

  TDBControlSet = (dbsTMS, dbsVCL, dbsCustom);

  TFieldMapEvent = procedure(Sender: TObject; AField: TField; var Allow: boolean) of object;
  TFieldMappedEvent = procedure(Sender: TObject; AField: TField; ALayoutItem: TLayoutItem) of object;

  TLayoutCreatedEvent = procedure(Sender: TObject; Bounds: TRect) of object;

  TGradientContainer = class(TCustomControl)
  private
    FColor: TColor;
    FColorTo: TColor;
    FBorderColor: TColor;
    FText: string;
    FBorderStyle: TBorderStyle;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetBorderStyle(const Value: TBorderStyle);
  protected
    procedure Paint; override;
    property Text: string read FText write SetText;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Images: TCustomImageList read FImages write FImages;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Color: TColor read FColor write SetColor default clWindow;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
  end;

  TFormCaption = class(TPersistent)
  private
    FColor: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FText: string;
    FHeight: integer;
    FFont: TFont;
    FVisible: boolean;
    FImageIndex: integer;
    FHintOK: string;
    FHintCancel: string;
    FHintEdit: string;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetText(const Value: string);
    procedure SetHeight(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure SetVisible(const Value: boolean);
    procedure SetImageIndex(const Value: integer);
    procedure SetHintCancel(const Value: string);
    procedure SetHintEdit(const Value: string);
    procedure SetHintOK(const Value: string);
  protected
    procedure Changed; virtual;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clWindow;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Font: TFont read FFont write SetFont;
    property Height: integer read FHeight write SetHeight default 24;
    property HintOK: string read FHintOK write SetHintOK;
    property HintCancel: string read FHintCancel write SetHintCancel;
    property HintEdit: string read FHintEdit write SetHintEdit;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Text: string read FText write SetText;
    property Visible: boolean read FVisible write SetVisible default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScrollArea = class(TPersistent)
  private
    FWidth: integer;
    FOnChange: TNotifyEvent;
    FHeight: integer;
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
  protected
    procedure Changed; virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDBFormBox = class(TCustomControl)
  private
    FScrollBox: TScrollBox;
    FPanel: TAdvDBFormPanel;
    FCaption: TAdvDBFormPanelCaption;
    FFormCaption: TFormCaption;
    FChildList: TStringList;
    FScrollArea: TScrollArea;
    FBorderColor: TColor;
    FShowEdit: boolean;
    FOnLayoutCreated: TLayoutCreatedEvent;
    FImages: TCustomImageList;
    FOnControlCreated: TControlCreatedEvent;
    FOnFieldMapped: TFieldMappedEvent;
    FOnFieldMap: TFieldMapEvent;
    function GetColor: TColor;
    function GetColorTo: TColor;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFormCaption(const Value: TFormCaption);
    procedure SetScrollArea(const Value: TScrollArea);
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetEditMode: TEditMode;
    procedure SetEditMode(const Value: TEditMode);
    procedure SetShowEdit(const Value: boolean);
    function GetLayoutSettings: TLayout;
    procedure SetLayoutSettings(const Value: TLayout);
    procedure SetImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetAutoLayout: boolean;
    procedure SetAutoLayout(const Value: boolean);
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragModePanel(const Value: TDragMode);
    function GetAutoEdit: boolean;
    procedure SetAutoEdit(const Value: boolean);
  protected
    procedure CaptionChanged(Sender: TObject);
    procedure ScrollAreaChanged(Sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteChildComponentList(writer: TWriter);
    procedure ReadChildComponentList(reader: TReader);
    procedure WriteCustomItems(writer: TWriter);
    procedure ReadCustomItems(reader: TReader);
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEdit(Sender: TObject); virtual;
    procedure DoCancel(Sender: TObject); virtual;
    procedure DoOK(Sender: TObject); virtual;
    procedure DoPanelLayoutCreated(Sender: TObject; Bounds: TRect);
    procedure DoPanelControlCreated(Sender: TObject; ALayoutItem: TLayoutItem; AControl: TControl; AField: TField);
    procedure DoPanelFieldMap(Sender: TObject; AField: TField; var Allow: Boolean);
    procedure DoPanelFieldMapped(Sender: TObject; AField: TField; ALayoutItem: TLayoutItem);
    procedure DoMouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoMouseEnter(Sender: TObject);
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function RealCaptionHeight: integer;
    function GetVersionNr: Integer; virtual;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property DBForm: TAdvDBFormPanel read FPanel;
    procedure InitLayout;
    procedure ShowLayout;
    procedure RemoveControls;
    procedure RemoveLayout;
  published
    property Align;
    property Anchors;
    property AutoEdit: boolean read GetAutoEdit write SetAutoEdit default false;
    property AutoLayout: boolean read GetAutoLayout write SetAutoLayout default true;
    property Caption: TFormCaption read FFormCaption write SetFormCaption;
    property Ctl3D;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property Color: TColor read GetColor write SetColor default clWindow;
    property ColorTo: TColor read GetColorTo write SetColorTo default clNone;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor: TCursor read GetDragCursor write SetDragCursor;
    property DragKind: TDragKind read GetDragKind write SetDragKind;
    property DragMode: TDragMode read GetDragMode write SetDragModePanel;
    property DoubleBuffered;
    property EditMode: TEditMode read GetEditMode write SetEditMode default emOn;
    property Hint;
    property Images: TCustomImageList read FImages write SetImages;
    property Margins;
    property Layout: TLayout read GetLayoutSettings write SetLayoutSettings;
    property Padding;
    property ScrollArea: TScrollArea read FScrollArea write SetScrollArea;
    property ShowEdit: boolean read FShowEdit write SetShowEdit default true;
    property ShowHint;
    property Touch;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnControlCreated: TControlCreatedEvent read FOnControlCreated write FOnControlCreated;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnFieldMap: TFieldMapEvent read FOnFieldMap write FOnFieldMap;
    property OnFieldMapped: TFieldMappedEvent read FOnFieldMapped write FOnFieldMapped;
    property OnGesture;
    property OnLayoutCreated: TLayoutCreatedEvent read FOnLayoutCreated write FOnLayoutCreated;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Version: string read GetVersion write SetVersion;
  end;

  TAdvDBFormPanelCaption = class(TGradientContainer)
  private
    FEditBtn: TPNGSpeedButton;
    FCancelBtn: TPNGSpeedButton;
    FOKBtn: TPNGSpeedButton;
    FOnEdit: TNotifyEvent;
    FOnOK: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FEditMode: boolean;
    procedure SetEditMode(const Value: boolean);
  protected
    procedure CreateWnd; override;
    procedure DoEdit(Sender: TObject);
    procedure DoCancel(Sender: TObject);
    procedure DoOK(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property EditMode: boolean read FEditMode write SetEditMode;
  published
    property OnEdit: TNotifyEvent read FOnEdit write FOnEdit;
    property OnOK: TNotifyEvent read FOnOK write FOnOK;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

  TFieldFormDataLink = class(TFieldDataLink)
  private
    FOnStateChange: TNotifyEvent;
  protected
    procedure EditingChanged; override;
  public
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDBFormPanel = class(TGradientContainer)
  private
    FLayout: TLayout;
    FLayouter: TAdvDBFormLayouter;
    FDataLink: TFieldFormDataLink;
    FInScrollBox: boolean;
    FLabels: TList<TLabel>;
    FEditMode: TEditMode;
    FOnControlCreated: TControlCreatedEvent;
    FOnFieldMap: TFieldMapEvent;
    FOnFieldMapped: TFieldMappedEvent;
    FAutoLayout: boolean;
    FOnLayoutCreated: TLayoutCreatedEvent;
    FCustomItems: boolean;
    FIsDesigning: boolean;
    FAutoEdit: boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetLayout(const Value: TLayout);
    procedure SetAutoLayout(const Value: boolean);
    function GetVersion: string;
  protected
    procedure DataActiveChange(Sender: TObject);
    procedure DataStateChange(Sender: TObject);
    procedure ControlCreated(Sender: TObject; ALayoutItem: TLayoutItem; AControl: TControl; AField: TField);
    procedure Paint; override;
    property InScrollBox: boolean read FInScrollBox write FInScrollBox;
    procedure DoFieldMap(AField: TField; var Allow: boolean);
    procedure DoFieldMapped(AField: TField; ALayoutItem: TLayoutItem);
    procedure DoLayoutCreated(Bounds: TRect); virtual;
    procedure UpdateLayout;
    procedure LayoutChanged(Sender: TObject);
    procedure BeforeItemDestroyed(Sender: TObject; AItem: TLayoutItem);
    procedure ItemsChanged(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetVersionNr: Integer; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteCustom(writer: TWriter);
    procedure ReadCustom(reader: TReader);
    property CustomItems: boolean read FCustomItems;
    function CheckDataset: boolean;
    procedure UpdateEditState;
    property IsDesigning: boolean read FIsDesigning write FIsDesigning;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitClassMap(ControlSet: TDBControlSet); virtual;
    procedure InitFields;
    procedure InitLayout;
    procedure ShowLayout;
    procedure RemoveControls;
    procedure RemoveLayout;
    procedure SetEditMode(OnOff: TEditMode);
    property Layouter: TAdvDBFormLayouter read FLayouter;
  published
    property Align;
    property Anchors;
    property AutoEdit: boolean read FAutoEdit write FAutoEdit default false;
    property AutoLayout: boolean read FAutoLayout write SetAutoLayout default true;
    property BorderColor default $B99D7F;
    property Ctl3D;
    property Cursor;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditMode: TEditMode read FEditMode write SetEditMode default emOn;
    property Layout: TLayout read FLayout write SetLayout;
    property Hint;
    property Margins;
    property Padding;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnFieldMap: TFieldMapEvent read FOnFieldMap write FOnFieldMap;
    property OnFieldMapped: TFieldMappedEvent read FOnFieldMapped write FOnFieldMapped;
    property OnCanResize;
    property OnControlCreated: TControlCreatedEvent read FOnControlCreated write FOnControlCreated;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnExit;
    property OnEnter;
    property OnGesture;
    property OnLayoutCreated: TLayoutCreatedEvent read FOnLayoutCreated write FOnLayoutCreated;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Touch;
    property Visible;
    property Version: string read GetVersion;
  end;

  TVCLRTCreator = class(TInterfacedObject, IComponentCreator)
    function CreateComponent(AClass: TControlClass; AOwner: TComponent): TControl;
    procedure DeleteComponent(AComponent: TComponent);
  end;

implementation

uses
  RTTI, TypInfo, SysUtils, Math, DBXJSON, DBXJSONReflect,
  AdvDatetimePicker, DBAdvEd, DBAdvSp, AdvDBDateTimePicker, DBAdvGDIPPicture, DBAdvLabel;

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  iend: Integer;
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
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw, R.Top, iend, R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw) + 1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left, R.Top + stepw, R.Right, iend);
      end;
    end;
  end;
end;

{ TAdvDBFormPanel }

procedure TAdvDBFormPanel.BeforeItemDestroyed(Sender: TObject;
  AItem: TLayoutItem);
begin
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(AItem.LabelCtrl) then
    AItem.LabelCtrl.Free;

  if Assigned(AItem.EditCtrl) then
    AItem.EditCtrl.Free;

  if Assigned(AItem.ReadOnlyCtrl) then
    AItem.ReadOnlyCtrl.Free;
end;

function TAdvDBFormPanel.CheckDataset: boolean;
begin
  Result := false;
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  Result := true;
end;

procedure TAdvDBFormPanel.ControlCreated(Sender: TObject; ALayoutItem: TLayoutItem; AControl: TControl;
  AField: TField);
begin
  if (AField.DataType = ftboolean) then
  begin
    if (AControl is TDBCheckBox) then
    begin
      (AControl as TDBCheckBox).Caption := '';
      (AControl as TDBCheckBox).Width := 16;
    end;
  end;

  if Assigned(OnControlCreated) then
    OnControlCreated(Self, ALayoutItem, AControl, AField);
end;

constructor TAdvDBFormPanel.Create(AOwner: TComponent);
begin
  inherited;

  FIsDesigning := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FDataLink := TFieldFormDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := DataActiveChange;
  FDataLink.OnStateChange := DataStateChange;

  FLayouter := TAdvDBFormLayouter.Create(Self);
  FLayouter.OnControlCreated := ControlCreated;


  InitClassMap(dbsTMS);

  FLayout := TLayout.Create(Self, FIsDesigning);
  FLayout.OnChange := LayoutChanged;
  FLayout.OnItemsChange := ItemsChanged;
  FLayout.OnBeforeItemDestroy := BeforeItemDestroyed;

  Width := 500;
  Height := 400;

  FLabels := TList<TLabel>.Create;
  FEditMode := emOn;

  FBorderColor := $B99D7F;

  FAutoLayout := true;
  DoubleBuffered := true;
end;

procedure TAdvDBFormPanel.DataActiveChange(Sender: TObject);
begin
  if (csLoading in ComponentState) then
    Exit;

  if FAutoLayout then
    UpdateLayout;

  if AutoEdit then
    UpdateEditState
  else
    SetEditMode(EditMode);

  Invalidate;
end;

procedure TAdvDBFormPanel.UpdateEditState;
begin
  if (FDataLink.DataSet.State = dsEdit)then
    EditMode := emOn;

  if (FDataLink.DataSet.State = dsBrowse) then
    EditMode := emOff;
end;

procedure TAdvDBFormPanel.DataStateChange(Sender: TObject);
begin
  if AutoEdit then
    UpdateEditState;
end;

procedure TAdvDBFormPanel.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('CustomItems',ReadCustom, WriteCustom, true);
end;

destructor TAdvDBFormPanel.Destroy;
begin
  FLabels.Free;
  FDataLink.Free;
  FLayouter.Free;
  FLayout.Free;
  inherited;
end;

procedure TAdvDBFormPanel.DoFieldMap(AField: TField; var Allow: boolean);
begin
  if Assigned(OnFieldMap) then
    OnFieldMap(Self, AField, Allow);
end;

procedure TAdvDBFormPanel.DoFieldMapped(AField: TField;
  ALayoutItem: TLayoutItem);
begin
  if Assigned(OnFieldMapped) then
    OnFieldMapped(Self, AField, ALayoutItem);
end;

procedure TAdvDBFormPanel.DoLayoutCreated(Bounds: TRect);
begin
  if Assigned(OnLayoutCreated) then
    OnLayoutCreated(Self, Bounds);
end;

function TAdvDBFormPanel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TAdvDBFormPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvDBFormPanel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvDBFormPanel.InitClassMap(ControlSet: TDBControlSet);
begin
  case ControlSet of
    dbsTMS:
      begin
        FLayouter.ClearClassMap;
        FLayouter.AddClassMap(dcEdit, TDBAdvEdit);
        FLayouter.AddClassMap(dcSpinEdit, TDBAdvSpinEdit);
        FLayouter.AddClassMap(dcCheckBox, TDBCheckBox);
        FLayouter.AddClassMap(dcDateTime, TAdvDBDateTimePicker);
        FLayouter.AddClassMap(dcDate, TAdvDBDateTimePicker);
        FLayouter.AddClassMap(dcTime, TAdvDBDateTimePicker);
        FLayouter.AddClassMap(dcMemo, TDBMemo);
        FLayouter.AddClassMap(dcComboBox, TDBComboBox);
        FLayouter.AddClassMap(dcRadioGroup, TDBRadioGroup);
        FLayouter.AddClassMap(dcLabel, TDBAdvLabel);
        FLayouter.AddClassMap(dcImage, TDBAdvGDIPPicture);
        FLayouter.AddClassMap(dcRichEdit, TDBRichEdit);
        FLayouter.AddClassMap(dcMaskEdit, TDBAdvMaskEdit);
        FLayouter.AddClassMap(dcLookupComboBox, TDBLookupComboBox);
      end;
    dbsVCL:
      begin
        FLayouter.ClearClassMap;
        FLayouter.AddClassMap(dcEdit, TDBEdit);
        FLayouter.AddClassMap(dcSpinEdit, TDBEdit);
        FLayouter.AddClassMap(dcCheckBox, TDBCheckBox);
        FLayouter.AddClassMap(dcDateTime, TDBEdit);
        FLayouter.AddClassMap(dcTime, TDBEdit);
        FLayouter.AddClassMap(dcDate, TDBEdit);
        FLayouter.AddClassMap(dcMemo, TDBMemo);
        FLayouter.AddClassMap(dcComboBox, TDBComboBox);
        FLayouter.AddClassMap(dcRadioGroup, TDBRadioGroup);
        FLayouter.AddClassMap(dcLabel, TDBText);
        FLayouter.AddClassMap(dcImage, TDBImage);
        FLayouter.AddClassMap(dcRichEdit, TDBRichEdit);
        FLayouter.AddClassMap(dcMaskEdit, TDBEdit);
        FLayouter.AddClassMap(dcLookupComboBox, TDBLookupComboBox);
      end;
    dbsCustom:
      begin
      end;
  end;
end;

procedure TAdvDBFormPanel.InitFields;
var
  i: integer;
  fld: TField;
  cond: boolean;
begin
  if not CheckDataSet then
    Exit;

  for i := 0 to Layout.Items.Count - 1 do
  begin
    if (Layout.Items[i].DataField <> '') then
    begin
      cond := (Assigned(Layout.Items[i].Field) and (Layout.Items[i].Field.FieldName <> Layout.Items[i].DataField))
        or not Assigned(Layout.Items[i].Field);

      if cond then
      begin
        fld := DataSource.DataSet.FieldByName(Layout.Items[i].DataField);
        if Assigned(fld) then
          Layout.Items[i].Field := fld;
      end;
    end;
  end;
end;

procedure TAdvDBFormPanel.InitLayout;
var
  i: integer;
  li: TLayoutItem;
  Allow: boolean;
begin
  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  FLayout.BeginUpdate;

  FLayout.Items.Clear;

  for i := 0 to DataSource.DataSet.FieldCount - 1 do
  begin
    Allow := true;

    DoFieldMap(DataSource.DataSet.Fields[i], Allow);

    // add all fields
    if Allow then
    begin
      li := FLayout.Items.Add;
      li.DataField := DataSource.DataSet.Fields[i].FieldName;
      li.LabelCaption := DataSource.DataSet.Fields[i].DisplayName;
      li.Field := DataSource.DataSet.Fields[i];
      li.ShowLabel := true;
      li.IsStatic := false;

      if DataSource.DataSet.Fields[i].FieldKind = fkLookup then
      begin
        li.DataControl := dcLookupComboBox;
      end
      else
      begin

        case DataSource.DataSet.Fields[i].DataType of
        ftString, ftWideString:
          begin
            if DataSource.DataSet.Fields[i].ReadOnly and Layout.Controls.ReadOnlyAsLabel then
              li.DataControl := dcLabel
            else
              li.DataControl := dcEdit;
          end;
        ftBoolean:
          begin
            li.DataControl := dcCheckBox;
            li.IsStatic := true;
          end;
        ftInteger, ftSmallint, ftWord, ftByte, ftCurrency:
          begin
            if DataSource.DataSet.Fields[i].ReadOnly and Layout.Controls.ReadOnlyAsLabel then
              li.DataControl := dcLabel
            else
              li.DataControl := dcSpinEdit;
          end;
        ftMemo, ftWideMemo:
          begin
            if DataSource.DataSet.Fields[i].ReadOnly and Layout.Controls.ReadOnlyAsLabel then
              li.DataControl := dcLabel
            else
              li.DataControl := dcMemo;
          end;
        ftGraphic:
          begin
            li.DataControl := dcImage;
            li.IsStatic := true;
          end;
        ftBlob, ftBytes:
          begin
            // try to guess whether it is a rich edit or image?
            li.DataControl := dcImage;
            li.IsStatic := true;
          end;
        ftDate, ftTime, ftDateTime:
          begin
            if DataSource.DataSet.Fields[i].ReadOnly and Layout.Controls.ReadOnlyAsLabel then
              li.DataControl := dcLabel
            else
            begin
              if DataSource.DataSet.Fields[i].DataType = ftDate then
                li.DataControl := dcDate
              else
              if DataSource.DataSet.Fields[i].DataType = ftTime then
                li.DataControl := dcTime
              else
                li.DataControl := dcDateTime;
            end;
          end;
        end;
      end;

      DoFieldMapped(DataSource.DataSet.Fields[i], li);
    end;
  end;

  FCustomItems := false;

  FLayout.ResetUpdate;
end;

procedure TAdvDBFormPanel.ItemsChanged(Sender: TObject);
begin
  if (csReading in ComponentState) or (csLoading in ComponentState) then
    Exit;

  if (csDesigning in ComponentState) then
  begin
    FCustomItems := Layout.Items.Count > 0;
    UpdateLayout;
  end;
end;

procedure TAdvDBFormPanel.LayoutChanged(Sender: TObject);
begin
  if (csReading in ComponentState) or (csLoading in ComponentState) then
    Exit;

  if not Assigned(DataSource) then
    Exit;

  if not Assigned(DataSource.DataSet) then
    Exit;

  UpdateLayout;
end;

procedure TAdvDBFormPanel.Loaded;
begin
  inherited;
end;

procedure TAdvDBFormPanel.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin

  if (Operation = opRemove) and not (csDestroying in ComponentState) then
  begin
    for i := 0 to Layout.Items.Count - 1 do
    begin
      if (Layout.Items[i].LabelCtrl = AComponent) then
        Layout.Items[i].LabelCtrl := nil;

      if (Layout.Items[i].EditCtrl = AComponent) then
        Layout.Items[i].EditCtrl := nil;

      if (Layout.Items[i].ReadOnlyCtrl = AComponent) then
        Layout.Items[i].ReadOnlyCtrl := nil;
    end;
  end;

  inherited;

  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;

  //
end;

procedure TAdvDBFormPanel.Paint;
begin
  if not FInScrollBox then
    inherited Paint;

  if (csDesigning in ComponentState) and (ControlCount = 0) then
  begin
    if not Assigned(DataSource) then
    begin
      Canvas.TextOut(20,20,'Assign a datasource ...');
    end
    else
    begin
      if not Assigned(DataSource.DataSet) then
      begin
        Canvas.TextOut(20,20,'Assign a dataset to the datasource ...');
      end
      else
        if not DataSource.DataSet.Active then
          Canvas.TextOut(20,20,'Set Dataset.Active = true ...');
    end;
  end;
end;

procedure TAdvDBFormPanel.ReadCustom(reader: TReader);
begin
  FCustomItems := reader.ReadBoolean;
end;

procedure TAdvDBFormPanel.RemoveControls;
var
  i: integer;
begin
  for i := 0 to Layout.Items.Count - 1 do
  begin
    if Assigned(Layout.Items[i].LabelCtrl) then
    begin
      Layout.Items[i].LabelCtrl.Free;
      Layout.Items[i].LabelCtrl := nil;
    end;

    if Assigned(Layout.Items[i].EditCtrl) then
    begin
      Layout.Items[i].EditCtrl.Free;
      Layout.Items[i].EditCtrl := nil;
    end;

    if Assigned(Layout.Items[i].ReadOnlyCtrl) then
    begin
      Layout.Items[i].ReadOnlyCtrl.Free;
      Layout.Items[i].ReadOnlyCtrl := nil;
    end;
  end;
end;

procedure TAdvDBFormPanel.RemoveLayout;
begin
  RemoveControls;
  Layout.Items.Clear;
end;

procedure TAdvDBFormPanel.SetAutoLayout(const Value: boolean);
begin
  if (FAutoLayout <> Value) then
  begin
    FAutoLayout := Value;

    if FAutoLayout then
      UpdateLayout;
  end;
end;

procedure TAdvDBFormPanel.SetDataSource(const Value: TDataSource);
var
  cmp: TComponent;
begin
  if (FDataLink.DataSource <> Value) then
  begin
    if not (csLoading in ComponentState) then
    begin
      cmp := (Owner as TComponent);

      if not (Assigned(cmp) and not (csReading in cmp.ComponentState)) then
      begin
        FCustomItems := false;
      end;
    end;

    FDataLink.DataSource := Value;

    if (Value = nil) and (FAutoLayout) then
      RemoveLayout;

    Repaint;
    Width := Width + 1;
    Width := Width - 1;
  end;
end;

procedure TAdvDBFormPanel.SetEditMode(OnOff: TEditMode);
var
  i: integer;
  ctrl,roctrl: TControl;
  fld: TField;
  richlbl: TDBAdvLabel;
  isStatic: boolean;
  dolbl: boolean;

begin
  FEditMode := OnOff;

  if (csDesigning in ComponentState) then
    Exit;

  for i := 0 to Layout.Items.Count - 1 do
  begin
    fld := Layout.Items[i].Field;
    ctrl := Layout.Items[i].EditCtrl;
    roctrl := Layout.Items[i].ReadOnlyCtrl;
    isStatic := Layout.Items[i].IsStatic;

    dolbl := (Layout.Items[i].DataControl = dcRichEdit) or (not isStatic);

    if Assigned(fld) and not Assigned(roctrl) and Assigned(ctrl) then
    begin
      if dolbl then
      begin
        richlbl := TDBAdvLabel.Create(Self);
        richlbl.Parent := Self;
        richlbl.Datasource := DataSource;
        richlbl.DataField := fld.FieldName;
        richlbl.Left := ctrl.Left + 3;

        if Layout.Labels.Position in [lsLeftCenter, lsLeftTop, lsLeftBottom] then
          richlbl.Top := ctrl.Top - 1 {+ Trunc((ctrl.Height - richlbl.Height + 1)/2)};
        if Layout.Labels.Position = lsTop then
          richlbl.Top := ctrl.Top;

        richlbl.Width := ctrl.Width;
        richlbl.Height := ctrl.Height;

        richlbl.WordWrap := (Layout.Items[i].DataControl in [dcRadioGroup, dcComboBox, dcRichEdit, dcMemo]);

        Layout.Items[i].ReadOnlyCtrl := richlbl;
        roctrl := richlbl;
      end;
    end;

    if Assigned(ctrl) and isStatic then
    begin
      ctrl.Enabled := OnOff = emOn;
    end;

    if Assigned(ctrl) and dolbl then
    begin
      ctrl.Visible := OnOff = emOn;
    end;

    if Assigned(roctrl) and dolbl then
    begin
      roctrl.Visible := OnOff = emOff;
    end;
  end;

  Repaint;
end;

procedure TAdvDBFormPanel.SetLayout(const Value: TLayout);
begin
  FLayout.Assign(Value);
end;

procedure TAdvDBFormPanel.ShowLayout;
var
  R: TRect;
begin
  R := ClientRect;

  Layouter.InitScale(Canvas.Handle);
  Layouter.DoLayout(Self,DataSource,Layout,r,TVCLRTCreator.Create);
  DoLayoutCreated(R);
  Repaint;
end;

procedure TAdvDBFormPanel.UpdateLayout;
begin
  if not AutoLayout then
    Exit;
  if not CheckDataSet then
    Exit;

  // Already item collection customization
  if FCustomItems then
  begin
    RemoveControls;
    if FDataLink.DataSource.DataSet.Active then
    begin
      InitFields;
      ShowLayout;
    end;
  end
  else
  begin
    if FDataLink.DataSource.DataSet.Active then
    begin
      RemoveLayout;
      InitLayout;
      ShowLayout;
    end
    else
    begin
      RemoveLayout;
    end;
  end;
end;

procedure TAdvDBFormPanel.WriteCustom(writer: TWriter);
begin
  writer.WriteBoolean(FCustomItems);
end;

{ TAdvDBFormBox }

procedure TAdvDBFormBox.CaptionChanged(Sender: TObject);
begin
  FCaption.Color := FFormCaption.Color;
  FCaption.ColorTo := FFormCaption.ColorTo;
  FCaption.Text := FFormCaption.Text;
  FCaption.Height := FFormCaption.Height;
  FCaption.Font.Assign(FFormCaption.Font);
  FCaption.Visible := FFormCaption.Visible;
  FCaption.ImageIndex := FFormCaption.ImageIndex;
  FCaption.Images := Images;
  FCaption.FEditBtn.Hint := FFormCaption.HintEdit;
  FCaption.FEditBtn.ShowHint := FFormCaption.HintEdit <> '';

  FCaption.FOKBtn.Hint := FFormCaption.HintOK;
  FCaption.FOKBtn.ShowHint := FFormCaption.HintOK <> '';

  FCaption.FCancelBtn.Hint := FFormCaption.HintCancel;
  FCaption.FCancelBtn.ShowHint := FFormCaption.HintCancel <> '';
end;

constructor TAdvDBFormBox.Create(AOwner: TComponent);
//var
//  FDesignTime: boolean;
begin
  inherited;
  Width := 500;
  Height := 400;

  FCaption := TAdvDBFormPanelCaption.Create(Self);
  FCaption.Parent := Self;
  FCaption.Height := 24;
  FCaption.Align := alTop;
  FCaption.BorderStyle := bsNone;

  FCaption.Margins.Left := 0;
  FCaption.Margins.Right := 0;
  FCaption.Margins.Top := 0;
  FCaption.Margins.Bottom := 1;
  FCaption.AlignWithMargins := true;

  FCaption.OnEdit := DoEdit;
  FCaption.OnOK := DoOK;
  FCaption.OnCancel := DoCancel;

  FScrollBox := TScrollBox.Create(Self);
  FScrollBox.BorderStyle := bsNone;
  FScrollBox.Parent := Self;
  FScrollBox.Align := alClient;

  AlignWithMargins := true;
  Padding.Left := 1;
  Padding.Right := 1;
  Padding.Top := 0;
  Padding.Bottom := 1;
  Padding.Top := 1;

//  FDesignTime := (csDesigning in ComponentState) and not
//    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FPanel := TAdvDBFormPanel.Create(Self);
  FPanel.Parent := FScrollBox;
  FPanel.Width := 500;
  FPanel.Height := 500;
  FPanel.BorderColor := clNone;
  FPanel.BorderStyle := bsNone;
  FPanel.OnLayoutCreated := DoPanelLayoutCreated;
  FPanel.OnControlCreated := DoPanelControlCreated;
  FPanel.OnFieldMap := DoPanelFieldMap;
  FPanel.OnFieldMapped := DoPanelFieldMapped;
  FPanel.OnMouseDown := DoMouseDown;
  FPanel.OnMouseUp := DoMouseUp;
  FPanel.OnMouseMove := DoMouseMove;

  FChildList := TStringList.Create;

  FFormCaption := TFormCaption.Create;
  FFormCaption.OnChange := CaptionChanged;

  FScrollArea := TScrollArea.Create;
  FScrollArea.OnChange := ScrollAreaChanged;

  FBorderColor := clGray;

  FShowEdit := true;
end;

procedure TAdvDBFormBox.CreateWnd;
begin
  inherited;
end;

procedure TAdvDBFormBox.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('ChildList',ReadChildComponentList,WriteChildComponentList,true);
  Filer.DefineProperty('CustomItems',ReadCustomItems,WriteCustomItems,true);
end;

destructor TAdvDBFormBox.Destroy;
begin
  FChildList.Free;
  FFormCaption.Free;
  FScrollArea.Free;
  inherited;
end;


procedure TAdvDBFormBox.DoCancel(Sender: TObject);
begin
  FPanel.SetEditMode(emOff);
end;

procedure TAdvDBFormBox.DoEdit(Sender: TObject);
begin
  FPanel.SetEditMode(emOn);
end;

procedure TAdvDBFormBox.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TAdvDBFormBox.DoMouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TAdvDBFormBox.DoMouseLeave(Sender: TObject);
begin
  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure TAdvDBFormBox.DoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y);
end;

procedure TAdvDBFormBox.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TAdvDBFormBox.DoOK(Sender: TObject);
begin
  FPanel.SetEditMode(emOff);
end;

procedure TAdvDBFormBox.DoPanelControlCreated(Sender: TObject;
  ALayoutItem: TLayoutItem; AControl: TControl; AField: TField);
begin
  if Assigned(OnControlCreated) then
    OnControlCreated(Self, ALayoutItem, AControl, AField);
end;

procedure TAdvDBFormBox.DoPanelFieldMap(Sender: TObject; AField: TField;
  var Allow: Boolean);
begin
  if Assigned(OnFieldMap) then
    OnFieldMap(Self, AField, Allow);
end;

procedure TAdvDBFormBox.DoPanelFieldMapped(Sender: TObject; AField: TField;
  ALayoutItem: TLayoutItem);
begin
  if Assigned(OnFieldMapped) then
    OnFieldMapped(Self, AField, ALayoutItem);
end;

procedure TAdvDBFormBox.DoPanelLayoutCreated(Sender: TObject; Bounds: TRect);
begin
  ScrollArea.Width := Bounds.Right - Bounds.Left;
  ScrollArea.Height := Bounds.Bottom - Bounds.Top;

  if Assigned(OnLayoutCreated) then
    OnLayoutCreated(Self, Bounds);
end;

function TAdvDBFormBox.GetAutoEdit: boolean;
begin
  Result := FPanel.AutoEdit;
end;

function TAdvDBFormBox.GetAutoLayout: boolean;
begin
  Result := FPanel.AutoLayout;
end;

procedure TAdvDBFormBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  frm: TCustomForm;
  i: integer;
  ctrl: TControl;
begin
  inherited GetChildren(Proc,Root);

  frm := GetParentForm(Self);

  for i := 0 to FScrollBox.ControlCount - 1 do
  begin
    ctrl := FScrollBox.Controls[i];

    if (ctrl.Owner = frm) then
      Proc(ctrl);
  end;

  for i := 0 to FPanel.ControlCount - 1 do
  begin
    ctrl := FPanel.Controls[i];

    if (ctrl.Owner = frm) then
      Proc(ctrl);
  end;
end;

function TAdvDBFormBox.GetColor: TColor;
begin
  Result := FPanel.Color;
end;

function TAdvDBFormBox.GetColorTo: TColor;
begin
  Result := FPanel.ColorTo;
end;

function TAdvDBFormBox.GetDataSource: TDataSource;
begin
  Result := FPanel.DataSource;
end;

function TAdvDBFormBox.GetDragCursor: TCursor;
begin
  Result := FPanel.DragCursor;
end;

function TAdvDBFormBox.GetDragKind: TDragKind;
begin
  Result := FPanel.DragKind;
end;

function TAdvDBFormBox.GetDragMode: TDragMode;
begin
  Result := FPanel.DragMode;
end;

function TAdvDBFormBox.GetEditMode: TEditMode;
begin
  Result := FPanel.EditMode;
end;

function TAdvDBFormBox.GetLayoutSettings: TLayout;
begin
  Result := FPanel.Layout;
end;

function TAdvDBFormBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvDBFormBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvDBFormBox.InitLayout;
begin
  FPanel.InitLayout;
end;

procedure TAdvDBFormBox.Loaded;
var
  i: integer;
  child: TComponent;
  frm: TCustomForm;
begin
  inherited;

  frm := GetParentForm(Self);
  for i := 0 to FChildList.Count - 1 do
  begin
    child := frm.FindComponent(FChildList.Strings[i]);
    if Assigned(child) then
      TControl(Child).Parent := FPanel;
  end;
end;

procedure TAdvDBFormBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;

  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TAdvDBFormBox.Paint;
begin
  inherited;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(ClientRect);

  if FCaption.Visible then
  begin
    Canvas.MoveTo(0, FCaption.Height + 1);
    Canvas.LineTo(Width, FCaption.Height + 1);
  end;
end;

procedure TAdvDBFormBox.ReadChildComponentList(reader: TReader);
begin
  FChildList.Clear;

  reader.ReadListBegin;

  while not reader.EndOfList() do
    FChildList.Add(reader.ReadIdent);

  reader.ReadListEnd;
end;

procedure TAdvDBFormBox.ReadCustomItems(reader: TReader);
begin
  FPanel.FCustomItems := reader.ReadBoolean;
end;

function TAdvDBFormBox.RealCaptionHeight: integer;
begin
  Result := 0;
  if Caption.Visible then
    Result := Caption.Height;
end;

procedure TAdvDBFormBox.RemoveControls;
begin
  FPanel.RemoveControls;
end;

procedure TAdvDBFormBox.RemoveLayout;
begin
  FPanel.RemoveLayout;
end;

procedure TAdvDBFormBox.Resize;
begin
  inherited;

  ScrollAreaChanged(Self);
  {
  if FPanel.Width < Width - 2 then
    FPanel.Width := Width - 2;

  if FPanel.Height < Height - Caption.Height - 3 then
    FPanel.Height := Height - Caption.Height - 3;
  }
end;

procedure TAdvDBFormBox.ScrollAreaChanged(Sender: TObject);
var
  sbw, sbh: integer;

begin
  sbw := 0;
  sbh := 0;

  if ScrollArea.Width > Width - 2 then
    sbh := GetSystemMetrics(SM_CYHSCROLL);

  if ScrollArea.Height > Height - 2 then
    sbw := GetSystemMetrics(SM_CXVSCROLL);

  // keep minimum
  if ScrollArea.Width < Width - 2 - sbw then
    ScrollArea.FWidth := Width - 2 - sbw;

  // correct when scrollbar visibility changes
  if (ScrollArea.Width <= Width - 2) and (sbw <> 0) then
    ScrollArea.FWidth := Width - 2 - sbw;

  if ScrollArea.Height < Height - RealCaptionHeight - 3 - sbh then
    ScrollArea.FHeight := Height - RealCaptionHeight - 3 - sbh;

  // correct when scrollbar visibility changes
  if (ScrollArea.Height = Height - RealCaptionHeight - 3) and (sbh <> 0) then
    ScrollArea.FHeight := Height - RealCaptionHeight - 3 - sbh;

  FPanel.Width := ScrollArea.Width;
  FPanel.Height := ScrollArea.Height;
end;

procedure TAdvDBFormBox.SetAutoEdit(const Value: boolean);
begin
  FPanel.AutoEdit := Value;
end;

procedure TAdvDBFormBox.SetAutoLayout(const Value: boolean);
begin
  FPanel.AutoLayout := Value;
end;

procedure TAdvDBFormBox.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
    FPanel.Invalidate;
  end;
end;

procedure TAdvDBFormBox.SetColor(const Value: TColor);
begin
  FPanel.Color := Value;
end;

procedure TAdvDBFormBox.SetColorTo(const Value: TColor);
begin
  FPanel.ColorTo := Value;
end;

procedure TAdvDBFormBox.SetDataSource(const Value: TDataSource);
var
  cmp: TComponent;
begin
  if (FPanel.DataSource <> Value) then
  begin
    if not (csLoading in ComponentState) then
    begin
      cmp := (Owner as TComponent);

      if not (Assigned(cmp) and not (csReading in cmp.ComponentState)) then
      begin
        FPanel.FCustomItems := false;
      end;
    end;

    FPanel.DataSource := Value;

    if (Value = nil) and (AutoLayout) then
      RemoveLayout;

    Invalidate;
  end;
end;

procedure TAdvDBFormBox.SetDragCursor(const Value: TCursor);
begin
  FPanel.DragCursor := Value;
end;

procedure TAdvDBFormBox.SetDragKind(const Value: TDragKind);
begin
  FPanel.DragKind := Value;
end;

procedure TAdvDBFormBox.SetDragModePanel(const Value: TDragMode);
begin
  FPanel.DragMode := Value;
end;

procedure TAdvDBFormBox.SetEditMode(const Value: TEditMode);
begin
  FPanel.EditMode := Value;
  FCaption.EditMode := Value = emOn;
end;

procedure TAdvDBFormBox.SetFormCaption(const Value: TFormCaption);
begin
  FFormCaption.Assign(Value);
end;


procedure TAdvDBFormBox.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  FCaption.Images := Value;
  Invalidate;
end;

procedure TAdvDBFormBox.SetLayoutSettings(const Value: TLayout);
begin
  FPanel.Layout.Assign(Value);
end;

procedure TAdvDBFormBox.SetScrollArea(const Value: TScrollArea);
begin
  FScrollArea := Value;
end;

procedure TAdvDBFormBox.SetShowEdit(const Value: boolean);
begin
  FShowEdit := Value;

  if Value then
  begin
    FCaption.SetEditMode(EditMode = emOn);
  end
  else
  begin
    FCaption.FEditBtn.Width := 0;
    FCaption.FOKBtn.Width := 0;
    FCaption.FCancelBtn.Width := 0;
  end;

  FCaption.FEditBtn.Visible := Value;
  FCaption.FOKBtn.Visible := Value;
  FCaption.FCancelBtn.Visible := Value;
end;

procedure TAdvDBFormBox.SetVersion(const Value: string);
begin
  // no writeable property but make sure it is persisted in the DFM file
end;

procedure TAdvDBFormBox.ShowLayout;
var
  r: TRect;

begin
  r := ClientRect;
  FPanel.Layouter.InitScale(Canvas.Handle);
  FPanel.Layouter.DoLayout(FPanel,DataSource,Layout, r, TVCLRTCreator.Create);
end;

procedure TAdvDBFormBox.WriteChildComponentList(writer: TWriter);
var
  i: integer;
begin
  writer.WriteListBegin;

  for i := 0 to FScrollBox.ControlCount - 1 do
  begin
    writer.WriteIdent(FScrollBox.Controls[i].Name);
  end;

  for i := 0 to FPanel.ControlCount - 1 do
  begin
    writer.WriteIdent(FPanel.Controls[i].Name);
  end;

  writer.WriteListEnd;
end;

procedure TAdvDBFormBox.WriteCustomItems(writer: TWriter);
begin
  writer.WriteBoolean(FPanel.CustomItems);
end;

{ TGradientContainer }

constructor TGradientContainer.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];

  FColor := clWindow;
  FColorTo := clNone;
  FBorderColor := clNone;
  FBorderStyle := bsSingle;
//  if not (csDesigning in ComponentState) then
//    DoubleBuffered := true;
end;

procedure TGradientContainer.Paint;
var
  cr: TRect;
  h: integer;
begin
  inherited;

  DrawGradient(Canvas, Color, ColorTo, 128, ClientRect,false);

  if (FBorderColor <> clNone) and (FBorderStyle = bsSingle) then
  begin
    Canvas.Pen.Color := FBorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(ClientRect);
  end;

  cr := ClientRect;
  cr.Left := 4;

  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    h := ((cr.Bottom - cr.Top) - Images.Height) div 2;
    h := Max(0,h);
    Images.Draw(Canvas, cr.Left, cr.Top + h, ImageIndex);
    cr.Left := cr.Left + Images.Width + 4;
  end;

  if Text <> '' then
  begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), cr, DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TGradientContainer.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TGradientContainer.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

procedure TGradientContainer.SetColor(const Value: TColor);
begin
  FColor := Value;
  Invalidate;
end;

procedure TGradientContainer.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TGradientContainer.SetText(const Value: string);
begin
  FText := Value;
  Invalidate;
end;

{ TFormCaption }

procedure TFormCaption.Assign(Source: TPersistent);
begin
  if (Source is TFormCaption) then
  begin
    FHeight := (Source as TFormCaption).Height;
    FColor := (Source as TFormCaption).Color;
    FColorTo := (Source as TFormCaption).ColorTo;
    FText := (Source as TFormCaption).Text;
    FImageIndex := (Source as TFormCaption).ImageIndex;
    FHintOK := (Source as TFormCaption).HintOK;
    FHintEdit := (Source as TFormCaption).HintEdit;
    FHintCancel := (Source as TFormCaption).HintCancel;
  end;
end;

procedure TFormCaption.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TFormCaption.Create;
begin
  inherited;
  FColor := clWindow;
  FColorTo := clNone;
  FHeight := 24;
  FImageIndex := -1;
  FText := '';
  FVisible := true;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor TFormCaption.Destroy;
begin
  FFont.Free;
end;

procedure TFormCaption.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TFormCaption.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TFormCaption.SetHeight(const Value: integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetHintCancel(const Value: string);
begin
  if (FHintCancel <> Value) then
  begin
    FHintCancel := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetHintEdit(const Value: string);
begin
  if (FHintEdit <> Value) then
  begin
    FHintEdit := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetHintOK(const Value: string);
begin
  if (FHintOK <> Value) then
  begin
    FHintOK := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TFormCaption.SetVisible(const Value: boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TScrollArea }

procedure TScrollArea.Assign(Source: TPersistent);
begin
  if (Source is TScrollArea) then
  begin
    FWidth := (Source as TScrollArea).Width;
    FHeight := (Source as TScrollArea).Height;
  end;
end;

procedure TScrollArea.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TScrollArea.SetHeight(const Value: integer);
begin
  FHeight := Value;
  Changed;
end;

procedure TScrollArea.SetWidth(const Value: integer);
begin
  FWidth := Value;
  Changed;
end;

{ TAdvDBFormPanelCaption }

constructor TAdvDBFormPanelCaption.Create(AOwner: TComponent);
begin
  inherited;
  FEditBtn := TPNGSpeedButton.Create(Self);
  FCancelBtn := TPNGSpeedButton.Create(Self);
  FOKBtn := TPNGSpeedButton.Create(Self);
  FEditBtn.Width := 0;
  FOKBtn.Width := 0;
  FCancelBtn.Width := 0;
  FEditMode := true;
end;

procedure TAdvDBFormPanelCaption.CreateWnd;
begin
  inherited;
  FEditBtn.Parent := Self;
  FCancelBtn.Parent := Self;
  FOKBtn.Parent := Self;

  FEditBtn.Flat := true;
  FEditBtn.PNGName := 'tms_gl_edit';
  FEditBtn.Left := Width - 24;
  FEditBtn.Anchors :=  [akRight, akTop];
  FEditBtn.OnClick := DoEdit;

  FOKBtn.Flat := true;
  FOKBtn.PNGName := 'tms_gl_accept';
  FOKBtn.Left := Width - 48;
  FOKBtn.Anchors := [akRight, akTop];
  FOKBtn.OnClick := DoOK;

  FCancelBtn.Flat := true;
  FCancelBtn.PNGName := 'tms_gl_cancel';
  FCancelBtn.Left := Width - 24;
  FCancelBtn.Anchors := [akRight, akTop];
  FCancelBtn.OnClick := DoCancel;

  SetEditMode(FEditMode);
end;

destructor TAdvDBFormPanelCaption.Destroy;
begin
  FEditBtn.Free;
  FCancelBtn.Free;
  FOKBtn.Free;
  inherited;
end;

procedure TAdvDBFormPanelCaption.DoCancel(Sender: TObject);
begin
  EditMode := false;

  if Assigned(OnCancel) then
    OnCancel(Self);
end;

procedure TAdvDBFormPanelCaption.DoEdit(Sender: TObject);
begin
  EditMode := true;

  if Assigned(OnEdit) then
    OnEdit(Self);
end;

procedure TAdvDBFormPanelCaption.DoOK(Sender: TObject);
begin
  EditMode := false;

  if Assigned(OnOk) then
    OnOK(Self);
end;

procedure TAdvDBFormPanelCaption.SetEditMode(const Value: boolean);
begin
  FEditMode := Value;

  if FEditMode then
  begin
    FCancelBtn.Width := 24;
    FOkBtn.Width := 24;
    FEditBtn.Width := 0;
  end
  else
  begin
    FCancelBtn.Width := 0;
    FOkBtn.Width := 0;
    FEditBtn.Width := 24;
  end;
end;

{ TVCLRTCreatore }

function TVCLRTCreator.CreateComponent(AClass: TControlClass;
  AOwner: TComponent): TControl;
begin
  Result := AClass.Create(AOwner);
end;


procedure TVCLRTCreator.DeleteComponent(AComponent: TComponent);
begin
  AComponent.Free;
end;

{ TFieldFormDataLink }

procedure TFieldFormDataLink.EditingChanged;
begin
  inherited;
  if Assigned(OnStateChange) then
    OnStateChange(Self);
end;

initialization
  Classes.RegisterClass(TAdvDBFormPanel);

end.
