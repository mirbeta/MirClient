{***************************************************************************}
{ TAdvDBFormLayouter component                                              }
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

unit AdvDBFormLayouter;

interface

{$I TMSDEFS.INC}

uses
  Classes, Controls, DB, Types, Generics.Collections, RTTI, StdCtrls, Graphics,
  MaskUtils, DBXJSON, DBXJSONReflect, ComCtrls, Windows
  {$IFDEF DELPHIXE6_LVL}
  , System.JSON
  {$ENDIF}
  ;

type
  TControlClass = class of TControl;

  TDataControl = (dcEdit, dcSpinEdit, dcCheckBox, dcComboBox, dcRadioGroup,
   dcImage, dcMemo, dcRichEdit, dcLookupComboBox, dcLabel, dcNone, dcDateTime,
   dcMaskEdit, dcDate, dcTime);

  // TPersistent proxy class because Delphi JSON serializer can't properly
  // handle TCollection/TCollectionItem instances
  TProxyLayoutItem = class(TPersistent)
  private
    FIsStatic: boolean;
    FShowLabel: boolean;
    FEditMask: TEditMask;
    FDataControl: TDataControl;
    FDataField: string;
    FLineBreak: boolean;
    FColumnBreak: boolean;
    FLabelCaption: string;
    FTag: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ColumnBreak: boolean read FColumnBreak write FColumnBreak default false;
    property DataField: string read FDataField write FDataField;
    property DataControl: TDataControl read FDataControl write FDataControl;
    property EditMask: TEditMask read FEditMask write FEditMask;
    property IsStatic: boolean read FIsStatic write FIsStatic default false;
    property LineBreak: boolean read FLineBreak write FLineBreak default false;
    property ShowLabel: boolean read FShowLabel write FShowLabel default true;
    property LabelCaption: string read FLabelCaption write FLabelCaption;
    property Tag: integer read FTag write FTag default 0;
  end;

  TLayoutItem = class(TCollectionItem)
  private
    [JSONMarshalled(False)]
    FField: TField;
    FDataControl: TDataControl;
    FDataField: string;
    FLineBreak: boolean;
    FShowLabel: boolean;
    FLabelCaption: string;
    FColumnBreak: boolean;
    [JSONMarshalled(False)]
    FValues: TStringList;
    [JSONMarshalled(False)]
    FLabelCtrl: TControl;
    [JSONMarshalled(False)]
    FEditCtrl: TControl;
    [JSONMarshalled(False)]
    FReadOnlyCtrl: TControl;
    FIsStatic: boolean;
    FEditMask: TEditMask;
    FProxy: TProxyLayoutItem;
    FTag: integer;
    procedure SetValues(const Value: TStringList);
    procedure SetDataField(const Value: string);
    procedure SetDataControl(const Value: TDataControl);
    procedure SetLabelCaption(const Value: string);
    procedure SetLineBreak(const Value: boolean);
    procedure SetShowLabel(const Value: boolean);
    procedure SetColumnBreak(const Value: boolean);
  protected
    function GetDisplayName: string; override;
    function GetProxy: TProxyLayoutItem;
    procedure ValuesChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    [JSONMarshalled(False)]
    property Field: TField read FField write FField;
    [JSONMarshalled(False)]
    property LabelCtrl: TControl read FLabelCtrl write FLabelCtrl;
    [JSONMarshalled(False)]
    property EditCtrl: TControl read FEditCtrl write FEditCtrl;
    [JSONMarshalled(False)]
    property ReadOnlyCtrl: TControl read FReadOnlyCtrl write FReadOnlyCtrl;
  published
    property ColumnBreak: boolean read FColumnBreak write SetColumnBreak default false;
    property DataField: string read FDataField write SetDataField;
    property DataControl: TDataControl read FDataControl write SetDataControl;
    property EditMask: TEditMask read FEditMask write FEditMask;
    property IsStatic: boolean read FIsStatic write FIsStatic default false;
    property LineBreak: boolean read FLineBreak write SetLineBreak default false;
    property ShowLabel: boolean read FShowLabel write SetShowLabel default true;
    property LabelCaption: string read FLabelCaption write SetLabelCaption;
    property Values: TStringList read FValues write SetValues;
    property Tag: integer read FTag write FTag default 0;
  end;

  TItemEvent = procedure(Sender: TObject; AItem: TLayoutItem) of object;

  TLayoutItems = class(TOwnedCollection)
  private
    FNotifyEvent: TNotifyEvent;
    FItemEvent: TItemEvent;
    FIsDesigning: boolean;
    function GetItem(Index: integer): TLayoutItem;
    procedure SetItem(Index: integer; const Value: TLayoutItem);
  protected
    function GetItemClass: TCollectionItemClass; virtual;
    procedure Update(Item: TCollectionItem); override;
    procedure DoBeforeItemDestroy(Item: TCollectionItem); virtual;
    property IsDesigning: boolean read FIsDesigning write FIsDesigning;
  public
    constructor Create(AOwner: TPersistent; Designing: boolean = false);
    property Items[Index: integer]: TLayoutItem read GetItem write SetItem; default;
    function ItemByField(const DataField: string): TLayoutItem;
    function Add: TLayoutItem;
    function Insert(Index: integer): TLayoutItem;
    property OnChange: TNotifyEvent read FNotifyEvent write FNotifyEvent;
    property OnBeforeItemDestroy: TItemEvent read FItemEvent write FItemEvent;
  end;

  TLabelPosition = (lsNone, lsLeftTop, lsTop, lsBottom, lsLeftCenter, lsLeftBottom);

  TLabelSettings = class(TPersistent)
  private
    FFormat: string;
    FPosition: TLabelPosition;
    FFont: TFont;
    FAutoSize: boolean;
    FSize: integer;
    FOnChange: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetAutoSize(const Value: boolean);
    procedure SetFormat(const Value: string);
    procedure SetPosition(const Value: TLabelPosition);
    procedure SetSize(const Value: integer);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default false;
    property Format: string read FFormat write SetFormat;
    property Font: TFont read FFont write SetFont;
    property Position: TLabelPosition read FPosition write SetPosition default lsLeftCenter;
    property Size: integer read FSize write SetSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TControlSettings = class(TPersistent)
  private
    FReadOnlyAsLabel: boolean;
    FAutoSize: boolean;
    FMaximumSize: integer;
    FSize: integer;
    FOnChange: TNotifyEvent;
    procedure SetAutoSize(const Value: boolean);
    procedure SetMaximumSize(const Value: integer);
    procedure SetReadOnlyAsLabel(const Value: boolean);
    procedure SetSize(const Value: integer);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default true;
    property MaximumSize: integer read FMaximumSize write SetMaximumSize default 192;
    property ReadOnlyAsLabel: boolean read FReadOnlyAsLabel write SetReadOnlyAsLabel default true;
    property Size: integer read FSize write SetSize default 96;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSpacing = class(TPersistent)
  private
    FHorz: integer;
    FVert: integer;
    FOnChange: TNotifyEvent;
    procedure SetHorz(const Value: integer);
    procedure SetVert(const Value: integer);
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Horz: integer read FHorz write SetHorz default 5;
    property Vert: integer read FVert write SetVert default 5;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TLayoutMode = (lmColumns, lmRows);

  // TPersistent proxy class to avoid Items TCollection (needed for easy design-time support)
  TProxyLayout = class(TPersistent)
  private
    FItems: TObjectList<TProxyLayoutItem>;
    FLabels: TLabelSettings;
    FControls: TControlSettings;
    FMode: TLayoutMode;
    FMargins: TMargins;
    FColumns: integer;
    FSpacing: TSpacing;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ToJSON: string;
    procedure FromJSON(const json: string);
  published
    property Mode: TLayoutMode read FMode write FMode;
    property Columns: integer read FColumns write FColumns;
    property Labels: TLabelSettings read FLabels write FLabels;
    property Spacing: TSpacing read FSpacing write FSpacing;
    property Margins: TMargins read FMargins write FMargins;
    property Controls: TControlSettings read FControls write FControls;
  end;


  TLayout = class(TPersistent)
  private
    [JSONMarshalled(False)]
    FOwner: TComponent;
    FLabels: TLabelSettings;
    FMode: TLayoutMode;
    FItems: TLayoutItems;
    FColumns: integer;
    FSpacing: TSpacing;
    FMargins: TMargins;
    FControls: TControlSettings;
    FOnChange: TNotifyEvent;
    FOnItemsChange: TNotifyEvent;
    FOnBeforeItemDestroy: TItemEvent;
    FIsDesigning: boolean;
    FUpdateCount: integer;
    procedure SetItems(const Value: TLayoutItems);
    procedure SetControls(const Value: TControlSettings);
    procedure SetLabels(const Value: TLabelSettings);
    procedure SetMargins(const Value: TMargins);
    procedure SetSpacing(const Value: TSpacing);
    procedure SetColumns(const Value: integer);
    procedure SetMode(const Value: TLayoutMode);
  protected
    function GetOwner: TPersistent; override;
    procedure SubPropsChanged(Sender: TObject);
    procedure DoItemsChanged(Sender: TObject);
    procedure DoBeforeItemDestroy(Sender: TObject; AItem: TLayoutItem);
    procedure DoChange; virtual;
    property IsDesigning: boolean read FIsDesigning write FIsDesigning;
  public
    constructor Create(AOwner: TComponent; Designing: boolean = false);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResetUpdate;
    function ToJSon: string;
    procedure FromJSon(const s: string);
    property Owner: TComponent read FOwner;
  published
    property Mode: TLayoutMode read FMode write SetMode default lmColumns;
    property Columns: integer read FColumns write SetColumns default 1;
    property Labels: TLabelSettings read FLabels write SetLabels;
    property Items: TLayoutItems read FItems write SetItems;
    property Spacing: TSpacing read FSpacing write SetSpacing;
    property Margins: TMargins read FMargins write SetMargins;
    property Controls: TControlSettings read FControls write SetControls;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemsChange: TNotifyEvent read FOnItemsChange write FOnItemsChange;
    property OnBeforeItemDestroy: TItemEvent read FOnBeforeItemDestroy write FOnBeforeItemDestroy;
  end;

  TClassMap = record
    DataControl: TDataControl;
    EditClass: TControlClass;
  end;

  IComponentCreator = interface
  ['{B3AAF35D-F47A-4453-84A9-C1BA47CD750A}']
    function CreateComponent(AClass: TControlClass; AOwner: TComponent): TControl;
    procedure DeleteComponent(AComponent: TComponent);
  end;

  TControlCreatedEvent = procedure(Sender: TObject; ALayoutItem: TLayoutItem; AControl: TControl; AField: TField) of object;

  TAdvDBFormLayouter = class(TComponent)
  private
    FClassMap: TList<TClassMap>;
    FOnControlCreated: TControlCreatedEvent;
    FScale: double;
  protected
    procedure DoControlCreated(ALayoutItem: TLayoutItem; AControl: TControl); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitScale(DC: HDC);
    procedure DoLayout(Control: TWinControl; DataSource: TDataSource; Layout: TLayout; var Bounds: TRect; Creator: IComponentCreator);
    procedure AddClassMap(DataControl: TDataControl; EditClass: TControlClass);
    function RemoveClassMap(DataControl: TDataControl): boolean;
    procedure ClearClassMap;
    function GetFieldClass(DataControl: TDataControl): TControlClass;
    function CreateFieldClass(DataSource: TDataSource; LayoutItem: TLayoutItem; Parent: TWinControl; x,y: integer; AClass: TControlClass; Creator: IComponentCreator): TControl;
    property Scale: double read FScale write FScale;
    property OnControlCreated: TControlCreatedEvent read FOnControlCreated write FOnControlCreated;
  end;

implementation

uses
  TypInfo, Dialogs, SysUtils, Math;


function ObjectToString(obj: TObject): string;
var
  jo: TJSONValue;
  Marshal: TJSONMarshal;
begin
  try
    Marshal := TJSONMarshal.Create(TJSONConverter.Create);

    try
      jo := Marshal.Marshal(obj);
      try
        Result := jo.ToString;
      finally
        jo.Free;
      end;
    finally
      Marshal.Free;
    end;
  finally
  end;
end;


{ TAdvDBFormLayouter }

procedure TAdvDBFormLayouter.AddClassMap(DataControl: TDataControl;
  EditClass: TControlClass);
var
  cm: TClassMap;
begin
  cm.DataControl := DataControl;
  cm.EditClass := EditClass;
  FClassMap.Add(cm);
end;

procedure TAdvDBFormLayouter.ClearClassMap;
begin
  FClassMap.Clear;
end;

constructor TAdvDBFormLayouter.Create(AOwner: TComponent);
begin
  inherited;
  FClassMap := TList<TClassMap>.Create;
  FScale := 1.0;
end;

function TAdvDBFormLayouter.CreateFieldClass(DataSource: TDataSource; LayoutItem: TLayoutItem; Parent: TWinControl; x, y: integer;
  AClass: TControlClass; Creator: IComponentCreator): TControl;
var
  C: TRttiContext;
  CompType: TRttiInstanceType;
  CompProp: TRttiProperty;
  PropInfo: PPropInfo;
  sl: TStrings;
begin
  Result := Creator.CreateComponent(AClass, Parent);

  Result.Left := x;
  Result.Top := y;
  Result.Parent := Parent;

  c := TRttiContext.Create;

  CompType :=  c.GetType(Result.ClassInfo) as TRttiInstanceType;

  CompProp := CompType.GetProperty('DataSource');
  if Assigned(CompProp) then
  begin
    CompProp.SetValue(Result, DataSource);
  end;

  CompProp := CompType.GetProperty('DataField');
  if Assigned(CompProp) then
  begin
  {$IFDEF DELPHIXE3_LVL}
    SetStrProp(Result,'DataField', LayoutItem.Field.FieldName);
  {$ENDIF}
  {$IFNDEF DELPHIXE3_LVL}
    SetWideStrProp(Result,'DataField', LayoutItem.Field.FieldName);
  {$ENDIF}
  end;

  if (LayoutItem.DataControl = dcMaskEdit) and (LayoutItem.EditMask <> '') then
  begin
    CompProp := CompType.GetProperty('EditMask');
    if Assigned(CompProp) then
    begin
      CompProp.SetValue(Result, LayoutItem.EditMask);
    end;
  end;

  if (LayoutItem.DataControl in [dcComboBox, dcRadioGroup]) then
  begin
    CompProp := CompType.GetProperty('Items');
    if Assigned(CompProp) then
    begin
      PropInfo := TypInfo.GetPropInfo(Result.ClassInfo, 'Items');
      if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
      begin
        {$IFDEF DELPHIXE2_LVL}
        sl := TStrings(GetOrdProp(Result, PropInfo));
        {$ENDIF}
        {$IFNDEF DELPHIXE2_LVL}
        sl := TStrings(GetOrdProp(Result, PropInfo)) as TPersistent;
        {$ENDIF}
        sl.Assign(LayoutItem.Values);
      end;
    end;
  end;

  if (LayoutItem.DataControl = dcDate) then
  begin
    CompProp := CompType.GetProperty('Kind');
    if Assigned(CompProp) then
      SetEnumProp(Result,'Kind','dkDate');
  end;

  if (LayoutItem.DataControl = dcTime) then
  begin
    CompProp := CompType.GetProperty('Kind');
    if Assigned(CompProp) then
      SetEnumProp(Result,'Kind','dkTime');
  end;

  CompProp := CompType.GetProperty('Stretch');
  if Assigned(CompProp) then
  begin
    CompProp.SetValue(Result, True);
  end;

  CompProp := CompType.GetProperty('Proportional');
  if Assigned(CompProp) then
  begin
    CompProp.SetValue(Result, True);
  end;

  //CompProp := CompType.GetProperty('DoubleBuffered');
  //if Assigned(CompProp) then
  //begin
  //  CompProp.SetValue(Result, True);
  //end;

  c.Free;
end;

destructor TAdvDBFormLayouter.Destroy;
begin
  FClassMap.Free;
  inherited;
end;

procedure TAdvDBFormLayouter.DoControlCreated(ALayoutItem: TLayoutItem; AControl: TControl);
begin
  if Assigned(OnControlCreated) then
    OnControlCreated(Self, ALayoutItem, AControl, ALayoutItem.Field);
end;

procedure TAdvDBFormLayouter.DoLayout(Control: TWinControl; DataSource: TDataSource; Layout: TLayout; var Bounds: TRect; Creator: IComponentCreator);
var
  i,j: integer;
  ctrl: TControl;
  fclass: TControlClass;
  x,y,lw,lh,cx,cy: integer;
  lbl: TStaticText;
  mw, mlw, mh, lc, mcw: integer;
  ctrls: TList<TControl>;
  lbls: TList<TStaticText>;
  colc: integer;
  sz: integer;
  d: integer;
begin
  if not Assigned(DataSource) then
  begin
    MessageDlg('No datasource assigned to control',mtError, [mbOK], 0);
    Exit;
  end;

  if not Assigned(DataSource.DataSet) then
  begin
    MessageDlg('No dataset assigned to datasource',mtError, [mbOK], 0);
    Exit;
  end;

  d := loword(GetDialogBaseUnits);

  // layout start position
  x := Bounds.Left + Layout.Margins.Left;
  y := Bounds.Top + Layout.Margins.Top;

  mw := 0;
  mlw := 0;
  mcw := 0;
  mh := 0;
  lc := 0;
  //lw := 0;
  ctrl := nil;

  ctrls := TList<TControl>.Create;
  lbls := TList<TStaticText>.Create;

  colc := Layout.Items.Count;

  if Layout.Columns > 0 then
  begin
    colc := Layout.Items.Count div Layout.Columns;
    if Layout.Items.Count mod Layout.Columns > 0 then
      inc(colc);
  end;

  for i := 0 to Layout.Items.Count - 1 do
  begin
    Layout.Items[i].LabelCtrl := nil;
    Layout.Items[i].EditCtrl := nil;

    fclass := nil;
    if Assigned(Layout.Items[i].Field) then
      fclass := GetFieldClass(Layout.Items[i].DataControl);

    lw := 0;
    lbl := nil;

    if Assigned(fclass) then
    begin
      if (Layout.Labels.Position <> lsNone) and Layout.Items[i].ShowLabel then
      begin
        // create label
        lbl := Creator.CreateComponent(TStaticText, Control) as TStaticText;
        lbl.Font.Assign(Layout.Labels.Font);

        // set caption
        lbl.Caption := Format(Layout.Labels.Format, [Layout.Items[i].LabelCaption]);
        lbl.Left := x;
        lbl.Top := y;
        lbl.Parent := Control;

        // set size
        if Layout.Labels.AutoSize or (Layout.Labels.Size = 0) then
        begin
          lbl.AutoSize := true;
        end
        else
          lbl.Width := Layout.Labels.Size;

        lw := lbl.Width;

        lbls.Add(lbl);

        // maximum label width
        if lw > mlw then
          mlw := lw;

        Layout.Items[i].LabelCtrl := lbl;
      end;

      cx := x;
      cy := y;

      case Layout.Labels.Position of
      lsLeftCenter:
        begin
          cx := x + lw + Layout.Spacing.Horz;
          cy := y - 1;
        end;
      lsLeftTop, lsLeftBottom:
        begin
          cx := x + lw + Layout.Spacing.Horz;
          cy := y - 2;
        end;
      lsTop:
        begin
          cy := y + lbl.Height + Layout.Spacing.Vert;
          lw := 0;
        end;
      lsBottom:
        begin
          lw := 0;
        end;
      end;

      ctrl := CreateFieldClass(DataSource, Layout.Items[i], Control, cx, cy, fclass, Creator);
      //ctrl.Name := Control.Name + 'DataForm' + IntToStr(i);

      if Layout.Controls.AutoSize and (Layout.Controls.Size > 0) then
      begin
        if (Layout.Items[i].DataControl in [dcImage, dcRichEdit, dcMemo, dcRadioGroup, dcLookupComboBox, dcDateTime, dcDate, dcTime]) then
        begin
          ctrl.Width := Layout.Controls.MaximumSize;
        end
        else
        begin
          sz := Layout.Items[i].Field.DisplayWidth;
          ctrl.Width := Min(Round(sz * d * FScale), Layout.Controls.MaximumSize);
        end;
      end
      else
        ctrl.Width := Layout.Controls.Size;

      ctrls.Add(ctrl);

      DoControlCreated(Layout.Items[i], ctrl);

      if (Layout.Labels.Position <> lsNone) and Layout.Items[i].ShowLabel then
      begin
        if ctrl is TWinControl then
          lbl.FocusControl := ctrl as TWinControl;

        case Layout.Labels.Position of
        lsLeftCenter: lbl.Top := lbl.Top + (ctrl.Height - lbl.Height) div 2;
               // center vertically
        lsLeftBottom: lbl.Top := (ctrl.Top + ctrl.Height - lbl.Height);

        lsBottom: lbl.Top := ctrl.Top + ctrl.Height + Layout.Spacing.Vert;
               // position at bottom of control
        end;
      end;

      Layout.Items[i].EditCtrl := ctrl;

      if ctrl.Width > mcw then
        mcw := ctrl.Width;

      if lw + ctrl.Width > mw then
        mw := lw + ctrl.Width;

      lh := 0;

      if Layout.Labels.Position in [lsLeftCenter, lsLeftTop, lsLeftBottom] then
        lh := ctrl.Height + Layout.Spacing.Vert;

      if Layout.Labels.Position in [lsTop, lsBottom] then
        lh := ctrl.Height + lbl.Height + 2 * Layout.Spacing.Vert;

      if lh > mh then
        mh := lh;

      if Layout.Mode = lmColumns then
      begin
        y := y + lh;

        // next control can't fit vertical
        if (Layout.Columns = 0) and (y + lh > Control.Height - Layout.Margins.Bottom) or (Layout.Items[i].ColumnBreak) then
        begin
          // move to the right to compensate for max. label width
          if Layout.Labels.Position in [lsLeftCenter, lsLeftTop, lsLeftBottom] then
          begin
            for j := lc to ctrls.Count - 1 do
            begin
              ctrls[j].Left := x + mlw;
            end;
          end;

          lc := ctrls.Count;
          x := x + mcw + mlw + Layout.Spacing.Horz * 2;
          y := Bounds.Top + Layout.Margins.Top;
          mw := 0;
          mlw := 0;
          mcw := 0;
        end;
      end;

      if (Layout.Mode = lmColumns) and (Layout.Columns > 0) and ((i + 1) mod colc = 0) and (i < Layout.Items.Count - 1) then
      begin
        if Layout.Labels.Position in [lsLeftCenter, lsLeftTop, lsLeftBottom] then
        begin
          for j := lc to ctrls.Count - 1 do
          begin
            ctrls[j].Left := x + mlw;
          end;
        end;

        lc := ctrls.Count;
        x := x + mcw + mlw + Layout.Spacing.Horz * 2;
        y := Bounds.Top + Layout.Margins.Top;
        mw := 0;
        mlw := 0;
        mcw := 0;
      end;

      if Layout.Mode = lmRows then
      begin
        x := x + lw + ctrl.Width + 2 * Layout.Spacing.Horz;

        // next control can't fit horizontally
        if (x + lw + ctrl.Width + 2 * Layout.Spacing.Horz > Control.Width - Layout.Margins.Right) or (Layout.Items[i].LineBreak) then
        begin
          x := Bounds.Left + Layout.Margins.Left;
          y := y + mh + Layout.Spacing.Vert;
          mh := 0;
        end;
      end;
    end;
  end;

  if (Layout.Labels.Position in [lsLeftCenter, lsLeftTop, lsLeftBottom]) and (Layout.Mode = lmColumns) then
  begin
    for j := lc to ctrls.Count - 1 do
    begin
      ctrls[j].Left := mlw + x;
    end;
  end;

  Bounds.Bottom := y + Layout.Spacing.Vert + Layout.Margins.Bottom;

  if Assigned(ctrl) then
    Bounds.Right := x + mlw + ctrl.Width + 2 * Layout.Spacing.Horz + Layout.Margins.Right;

  ctrls.Free;
  lbls.Free;
end;

function TAdvDBFormLayouter.RemoveClassMap(DataControl: TDataControl): boolean;
var
  i: integer;
begin
  Result := false;

  for i := 0 to FClassMap.Count - 1 do
  begin
    if FClassMap[i].DataControl = DataControl then
    begin
      Result := true;
      FClassMap.Delete(i);
      Break;
    end;
  end;
end;

function TAdvDBFormLayouter.GetFieldClass(DataControl: TDataControl): TControlClass;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FClassMap.Count - 1 do
  begin
    if FClassMap[i].DataControl = DataControl then
    begin
      Result := FClassMap[i].EditClass;
      Break;
    end;
  end;
end;

procedure TAdvDBFormLayouter.InitScale(DC: HDC);
var
  dpi: integer;
begin
  dpi := GetDeviceCaps(DC, LOGPIXELSX);

  Scale := 1;

  case dpi of
  120: Scale := 1.25;
  144: Scale := 1.5;
  end;
end;

{ TLayoutItems }

function TLayoutItems.Add: TLayoutItem;
begin
  Result := TLayoutItem(inherited Add);
end;

constructor TLayoutItems.Create(AOwner: TPersistent; Designing: boolean = false);
begin
  inherited Create(AOwner, GetItemClass);
  FIsDesigning := Designing;
end;

procedure TLayoutItems.DoBeforeItemDestroy(Item: TCollectionItem);
begin
  if Assigned(OnBeforeItemDestroy) then
    OnBeforeItemDestroy(Self, Item as TLayoutItem);
end;

function TLayoutItems.GetItem(Index: integer): TLayoutItem;
begin
  Result := TLayoutItem(inherited Items[Index]);
end;

function TLayoutItems.GetItemClass: TCollectionItemClass;
begin
  Result := TLayoutItem;
end;

function TLayoutItems.Insert(Index: integer): TLayoutItem;
begin
  Result := TLayoutItem(inherited Insert(Index));
end;

function TLayoutItems.ItemByField(const DataField: string): TLayoutItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if Items[i].DataField = DataField then
    begin
      Result := Items[i];
      Break;
    end;
  end;
end;

procedure TLayoutItems.SetItem(Index: integer; const Value: TLayoutItem);
begin
  inherited Items[Index] := Value;
end;

procedure TLayoutItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(OnChange) then
    OnChange(Self);
end;

{ TLayoutItem }

procedure TLayoutItem.Assign(Source: TPersistent);
begin
  if (Source is TLayoutItem) then
  begin
    FField := (Source as TLayoutItem).Field;
    FDataField := (Source as TLayoutItem).DataField;
    FDataControl := (Source as TLayoutItem).DataControl;
    FLineBreak := (Source as TLayoutItem).LineBreak;
    FColumnBreak := (Source as TLayoutItem).ColumnBreak;
    FLabelCaption := (Source as TLayoutItem).LabelCaption;
    FShowLabel := (Source as TLayoutItem).ShowLabel;
    FValues.Assign((Source as TLayoutItem).Values);
    FIsStatic := (Source as TLayoutItem).IsStatic;
    FEditMask := (Source as TLayoutItem).EditMask;
    FTag := (Source as TLayoutItem).Tag;
  end;

  if (Source is TProxyLayoutItem) then
  begin
    FDataField := (Source as TProxyLayoutItem).DataField;
    FDataControl := (Source as TProxyLayoutItem).DataControl;
    FLineBreak := (Source as TProxyLayoutItem).LineBreak;
    FColumnBreak := (Source as TProxyLayoutItem).ColumnBreak;
    FLabelCaption := (Source as TProxyLayoutItem).LabelCaption;
    FShowLabel := (Source as TProxyLayoutItem).ShowLabel;
//    FValues.Assign((Source as TProxyLayoutItem).Values);
    FIsStatic := (Source as TProxyLayoutItem).IsStatic;
    FEditMask := (Source as TProxyLayoutItem).EditMask;
    FTag := (Source as TProxyLayoutItem).Tag;
  end;
end;

constructor TLayoutItem.Create(Collection: TCollection);
begin
  inherited;
  FValues := TStringList.Create;
  FValues.OnChange := ValuesChanged;
  FShowLabel := true;
  FProxy := nil;
end;

destructor TLayoutItem.Destroy;
begin
  (Collection as TLayoutItems).DoBeforeItemDestroy(Self);
  if Assigned(FProxy) then
    FProxy.Free;
  FProxy := nil;

  FValues.Free;
  inherited;
end;


function TLayoutItem.GetDisplayName: string;
begin
  if DataField = '' then
    Result := 'TLayoutItem' +inttostr(Index)
  else
    Result := DataField;
end;

function TLayoutItem.GetProxy: TProxyLayoutItem;
begin
  if not Assigned(FProxy) then
    FProxy := TProxyLayoutItem.Create;

  FProxy.Assign(Self);
  Result := FProxy;
end;

procedure TLayoutItem.SetColumnBreak(const Value: boolean);
begin
  if (FColumnBreak <> Value) then
  begin
    Collection.BeginUpdate;
    FColumnBreak := Value;
    Collection.EndUpdate;
  end;
end;

procedure TLayoutItem.SetDataControl(const Value: TDataControl);
begin
  if (FDataControl <> Value) then
  begin
    Collection.BeginUpdate;
    FDataControl := Value;
    Collection.EndUpdate;
  end;
end;

procedure TLayoutItem.SetDataField(const Value: string);
begin
  if (FDataField <> Value) then
  begin
    Collection.BeginUpdate;
    FDataField := Value;
    if (FLabelCaption = '') and (Collection as TLayoutItems).IsDesigning then
      FLabelCaption := Value;

    Collection.EndUpdate;
  end;

  // only at design time
end;

procedure TLayoutItem.SetLabelCaption(const Value: string);
begin
  if (FLabelCaption <> Value) then
  begin
    Collection.BeginUpdate;
    FLabelCaption := Value;
    Collection.EndUpdate;
  end;
end;

procedure TLayoutItem.SetLineBreak(const Value: boolean);
begin
  if (FLineBreak <> Value) then
  begin
    Collection.BeginUpdate;
    FLineBreak := Value;
    Collection.EndUpdate;
  end;
end;

procedure TLayoutItem.SetShowLabel(const Value: boolean);
begin
  if (FShowLabel <> Value) then
  begin
    Collection.BeginUpdate;
    FShowLabel := Value;
    Collection.EndUpdate;
  end;
end;

procedure TLayoutItem.SetValues(const Value: TStringList);
begin
  FValues.Assign(Value);
end;

procedure TLayoutItem.ValuesChanged(Sender: TObject);
begin
  Collection.BeginUpdate;
  Collection.EndUpdate;
end;

{ TLayout }

procedure TLayout.Assign(Source: TPersistent);
var
  i: integer;
  li: TLayoutItem;
begin
  if (Source is TLayout) then
  begin
    FMode := (Source as TLayout).Mode;
    FColumns  := (Source as TLayout).Columns;
    FLabels.Assign((Source as TLayout).Labels);
    FItems.Assign((Source as TLayout).Items);
    FSpacing.Assign((Source as TLayout).Spacing);
    FMargins.Assign((Source as TLayout).Margins);
  end;

  if (Source is TProxyLayout) then
  begin
    FMode := (Source as TProxyLayout).Mode;
    FColumns  := (Source as TProxyLayout).Columns;
    FLabels.Assign((Source as TProxyLayout).Labels);
    FSpacing.Assign((Source as TProxyLayout).Spacing);
    FMargins.Assign((Source as TProxyLayout).Margins);

    FItems.Clear;

    for i := 0 to (Source as TProxyLayout).FItems.Count - 1 do
    begin
      li := FItems.Add;
      li.Assign((Source as TProxyLayout).FItems[i]);
    end;
  end;
end;

procedure TLayout.BeginUpdate;
begin
  inc(FUpdateCount);
end;

constructor TLayout.Create(AOwner: TComponent; Designing: boolean = false);
begin
  inherited Create;

  FOwner := AOwner;
  FIsDesigning := Designing;

  FLabels := TLabelSettings.Create;
  FLabels.OnChange := SubPropsChanged;

  FSpacing := TSpacing.Create;
  FSpacing.OnChange := SubPropsChanged;

  FMargins := TMargins.Create(nil);
  FMargins.OnChange := SubPropsChanged;

  FControls := TControlSettings.Create;
  FControls.OnChange := SubPropsChanged;

  FItems := TLayoutItems.Create(Self, Designing);
  FItems.OnChange := DoItemsChanged;
  FItems.OnBeforeItemDestroy := DoBeforeItemDestroy;

  FColumns := 1;
  FMode := lmColumns;
end;

destructor TLayout.Destroy;
begin
  FItems.Free;
  FMargins.Free;
  FSpacing.Free;
  FLabels.Free;
  FControls.Free;
  inherited;
end;

procedure TLayout.DoBeforeItemDestroy(Sender: TObject; AItem: TLayoutItem);
begin
  if Assigned(OnBeforeItemDestroy) then
    OnBeforeItemDestroy(Self, AItem);
end;

procedure TLayout.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

function TLayout.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TLayout.ResetUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
end;

procedure TLayout.DoItemsChanged(Sender: TObject);
begin
  if (FUpdateCount = 0) and Assigned(OnItemsChange) then
    OnItemsChange(Self);
end;

procedure TLayout.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);

  if FUpdateCount = 0 then
    DoItemsChanged(Self);
end;

procedure TLayout.FromJSon(const s: string);
var
  pl: TProxyLayout;

begin
  pl := TProxyLayout.Create;
  pl.FromJSON(s);
  try
    self.Assign(pl);
  finally
    pl.Free;
  end;
  DoChange;
end;

procedure TLayout.SetColumns(const Value: integer);
begin
  if (FColumns <> Value) then
  begin
    FColumns := Value;
    DoChange;
  end;
end;

procedure TLayout.SetControls(const Value: TControlSettings);
begin
  FControls.Assign(Value);
end;

procedure TLayout.SetItems(const Value: TLayoutItems);
begin
  FItems.Assign(Value);
end;

procedure TLayout.SetLabels(const Value: TLabelSettings);
begin
  FLabels.Assign(Value);
end;

procedure TLayout.SetMargins(const Value: TMargins);
begin
  FMargins.Assign(Value);
end;

procedure TLayout.SetMode(const Value: TLayoutMode);
begin
  if (FMode <> Value) then
  begin
    FMode := Value;
    DoChange;
  end;
end;

procedure TLayout.SetSpacing(const Value: TSpacing);
begin
  FSpacing.Assign(Value);
end;

procedure TLayout.SubPropsChanged(Sender: TObject);
begin
  DoChange;
end;

function TLayout.ToJSon: string;
var
  pl: TProxyLayout;
begin
  pl := TProxyLayout.Create;
  pl.Assign(Self);
  try
    Result := ObjectToString(pl);
  finally
    pl.Free;
  end;
end;

{ TLabelSettings }

procedure TLabelSettings.Assign(Source: TPersistent);
begin
  if (Source is TLabelSettings) then
  begin
    FPosition := (Source as TLabelSettings).Position;
    FFormat := (Source as TLabelSettings).Format;
    FFont.Assign((Source as TLabelSettings).Font);
    FSize := (Source as TLabelSettings).Size;
    FAutoSize := (Source as TLabelSettings).AutoSize;
  end;
end;

procedure TLabelSettings.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TLabelSettings.Create;
begin
  inherited Create;
  FFormat := '%s:';
  FPosition := lsLeftCenter;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FAutoSize := false;
  FSize := 0;
end;

destructor TLabelSettings.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLabelSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLabelSettings.SetAutoSize(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TLabelSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TLabelSettings.SetFormat(const Value: string);
begin
  if (FFormat <> Value) then
  begin
    FFormat := Value;
    Changed;
  end;
end;

procedure TLabelSettings.SetPosition(const Value: TLabelPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TLabelSettings.SetSize(const Value: integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TSpacing }

procedure TSpacing.Assign(Source: TPersistent);
begin
  if (Source is TSpacing) then
  begin
    FHorz := (Source as TSpacing).Horz;
    FVert := (Source as TSpacing).Vert;
  end;
end;

constructor TSpacing.Create;
begin
  FHorz := 5;
  FVert := 5;
end;

procedure TSpacing.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TSpacing.SetHorz(const Value: integer);
begin
  if (FHorz <> Value) then
  begin
    FHorz := Value;
    DoChange;
  end;
end;

procedure TSpacing.SetVert(const Value: integer);
begin
  if (FVert <> Value) then
  begin
    FVert := Value;
    DoChange;
  end;
end;

{ TControlSettings }

procedure TControlSettings.Assign(Source: TPersistent);
begin
  if (Source is TControlSettings) then
  begin
    FReadOnlyAsLabel := (Source as TControlSettings).ReadOnlyAsLabel;
    FAutoSize := (Source as TControlSettings).AutoSize;
    FMaximumSize := (Source as TControlSettings).MaximumSize;
    FSize := (Source as TControlSettings).Size;
  end;
end;

procedure TControlSettings.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TControlSettings.Create;
begin
  FReadOnlyAsLabel := true;
  FSize := 96;
  FAutoSize := true;
  FMaximumSize := 192;
end;

procedure TControlSettings.SetAutoSize(const Value: boolean);
begin
  if (FAutoSize <> Value) then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TControlSettings.SetMaximumSize(const Value: integer);
begin
  if (FMaximumSize <> Value) then
  begin
    FMaximumSize := Value;
    Changed;
  end;
end;

procedure TControlSettings.SetReadOnlyAsLabel(const Value: boolean);
begin
  if (FReadOnlyAsLabel <> Value) then
  begin
    FReadOnlyAsLabel := Value;
    Changed;
  end;
end;

procedure TControlSettings.SetSize(const Value: integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TProxyLayoutItem }

procedure TProxyLayoutItem.Assign(Source: TPersistent);
begin
  if (Source is TLayoutItem) then
  begin
    FDataField := (Source as TLayoutItem).DataField;
    FDataControl := (Source as TLayoutItem).DataControl;
    FLineBreak := (Source as TLayoutItem).LineBreak;
    FColumnBreak := (Source as TLayoutItem).ColumnBreak;
    FLabelCaption := (Source as TLayoutItem).LabelCaption;
    FShowLabel := (Source as TLayoutItem).ShowLabel;
    FIsStatic := (Source as TLayoutItem).IsStatic;
    FEditMask := (Source as TLayoutItem).EditMask;
    FTag := (Source as TLayoutItem).Tag;
  end;

  if (Source is TProxyLayoutItem) then
  begin
    FDataField := (Source as TProxyLayoutItem).DataField;
    FDataControl := (Source as TProxyLayoutItem).DataControl;
    FLineBreak := (Source as TProxyLayoutItem).LineBreak;
    FColumnBreak := (Source as TProxyLayoutItem).ColumnBreak;
    FLabelCaption := (Source as TProxyLayoutItem).LabelCaption;
    FShowLabel := (Source as TProxyLayoutItem).ShowLabel;
    FIsStatic := (Source as TProxyLayoutItem).IsStatic;
    FEditMask := (Source as TProxyLayoutItem).EditMask;
    FTag := (Source as TProxyLayoutItem).Tag;
  end;
end;



{ TProxyLayout }

procedure TProxyLayout.Assign(Source: TPersistent);
var
  i: integer;
  pli: TProxyLayoutItem;
begin
  if (Source is TProxyLayout) then
  begin
    FLabels.Assign((Source as TProxyLayout).Labels);
    FSpacing.Assign((Source as TProxyLayout).Spacing);
    FMargins.Assign((Source as TProxyLayout).Margins);
    FControls.Assign((Source as TProxyLayout).Controls);
    FColumns := (Source as TProxyLayout).Columns;
    FMode := (Source as TProxyLayout).Mode;

    for i := 0 to (Source as TProxyLayout).FItems.Count - 1 do
    begin
      pli := TProxyLayoutItem.Create;
      pli.Assign((Source as TProxyLayout).FItems[i]);
      FItems.Add(pli);
    end;
  end;

  if (Source is TLayout) then
  begin
    FLabels.Assign((Source as TLayout).Labels);
    FSpacing.Assign((Source as TLayout).Spacing);
    FMargins.Assign((Source as TLayout).Margins);
    FControls.Assign((Source as TLayout).Controls);
    FColumns := (Source as TLayout).Columns;
    FMode := (Source as TLayout).Mode;

    for i := 0 to (Source as TLayout).Items.Count - 1 do
    begin
      pli := TProxyLayoutItem.Create;
      pli.Assign((Source as TLayout).Items[i]);
      FItems.Add(pli);
    end;
  end;
end;

constructor TProxyLayout.Create;
begin
  inherited;
  FItems := TObjectList<TProxyLayoutItem>.Create;
  FLabels := TLabelSettings.Create;
  FSpacing := TSpacing.Create;
  FMargins := TMargins.Create(nil);
  FControls := TControlSettings.Create;
end;

destructor TProxyLayout.Destroy;
begin
  FItems.Free;
  FLabels.Free;
  FSpacing.Free;
  FMargins.Free;
  FControls.Free;
  inherited;
end;

procedure TProxyLayout.FromJSON(const json: string);
var
  jo: TJSONValue;
  UnMarshal: TJSONUnMarshal;
  LI: TProxyLayout;
begin
  jo := TJSONObject.ParseJSONValue(json) as TJSONObject;

  try
    UnMarshal := TJSONUnMarshal.Create;
    try
      LI := TProxyLayout(UnMarshal.Unmarshal(jo));
      try
        self.Assign(LI);
      finally
        LI.Free;
      end;
    finally
       UnMarshal.Free;
    end;
  finally
    jo.Free;
  end;
end;

function TProxyLayout.ToJSON: string;
begin
  Result := ObjectToString(Self);
end;

end.
