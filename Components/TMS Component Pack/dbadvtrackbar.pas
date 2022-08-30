{*************************************************************************}
{ TMS TDBAdvTrackBar component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2009                                             }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit DBAdvTrackBar;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, ComCtrls, Math, forms, DB,
  AdvTrackBar;

type



  TTrackFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;
    FControl: TComponent;
    FEditing: Boolean;
    FModified: Boolean;
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    function GetCanModify: Boolean;
    procedure SetEditing(Value: Boolean);
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: string);
    procedure UpdateField;
  protected
    procedure ActiveChanged; override;
    {$IFDEF DELPHIXE2_LVL}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$ENDIF}
    {$IFNDEF DELPHIXE2_LVL}
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
    {$ENDIF}
    procedure EditingChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create;
    function Edit: Boolean;
    procedure Modified;
    procedure Reset;
    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl write FControl;
    property Editing: Boolean read FEditing;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvTrackBar = class(TCustomTrackBar)
  private
    FDataLink: TTrackFieldDataLink;
    FFocused: Boolean;
    FInternalChange: Boolean;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetPosition(Value: Integer); override;

    function ValidField: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    property Field: TField read GetField;
    property Position;
  published
    property Align;
    property Anchors;
    property BackGroundStretched;
    property BackGround;
    property BackGroundDisabled;
    property BorderColor;
    property BorderColorDisabled;
    property Buttons;
    property ColorTo;
    property ColorDisabled;
    property ColorDisabledTo;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Direction;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property Color;
    property HelpContext;
    property Hint;
    property Max;
    property Min;
    property Orientation;
    property ParentCtl3D;
    property ParentShowHint;
    property PageSize;
    property PopupMenu;
    property RateActive;
    property RateInActive;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property Slider;
    property ShowFocus;
    property TabOrder;
    property TabStop default True;
    property Thumb;
    property TickMark;
    property TickImages;
    property TrackHint;
    property TrackLabel;
    property Transparent;
    property Visible;
    property OnContextPopup;
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDock;
    property OnStartDrag;

    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDrawTick;
    property OnGetTrackHint;
  end;

implementation


type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}
  IntPtr = Pointer;


//------------------------------------------------------------------------------

{ TTrackFieldDataLink }

constructor TTrackFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    FModified := False;
    if Assigned(FOnEditingChange) then FOnEditingChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName :=  Value;
    UpdateField;
  end;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.SetField(Value: TField);
begin
  if FField <> Value then
  begin
    FField := Value;
    EditingChanged;
    RecordChanged(nil);
    //UpdateRightToLeft;
  end;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.UpdateField;
begin
  if Active and (FFieldName <> '') then
  begin
    FField := nil;
    if Assigned(FControl) then
      SetField(GetFieldProperty(DataSource.DataSet, FControl, FFieldName)) else
      SetField(DataSource.DataSet.FieldByName(FFieldName));
  end else
    SetField(nil);
end;

//------------------------------------------------------------------------------

function TTrackFieldDataLink.Edit: Boolean;
begin
  if CanModify then inherited Edit;
  Result := FEditing;
end;

//------------------------------------------------------------------------------

function TTrackFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.Modified;
begin
  FModified := True;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.Reset;
begin
  RecordChanged(nil);
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.ActiveChanged;
begin
  UpdateField;
  if Assigned(FOnActiveChange) then FOnActiveChange(Self);
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.EditingChanged;
begin
  SetEditing(inherited Editing and CanModify);
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FField) and (FControl is TWinControl) then
    if TWinControl(FControl).CanFocus then
    begin
      Field^ := nil;
      TWinControl(FControl).SetFocus;
    end;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FField) then
  begin
    if Assigned(FOnDataChange) then FOnDataChange(Self);
    FModified := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.LayoutChanged;
begin
  UpdateField;
end;

//------------------------------------------------------------------------------

procedure TTrackFieldDataLink.UpdateData;
begin
  if FModified then
  begin
    if (Field <> nil) and Assigned(FOnUpdateData) then FOnUpdateData(Self);
    FModified := False;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHIXE2_LVL}
procedure TTrackFieldDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ENDIF}
{$IFNDEF DELPHIXE2_LVL}
procedure TTrackFieldDataLink.DataEvent(Event: TDataEvent; Info: Integer);
{$ENDIF}
begin
  inherited;
  {$IFDEF DELPHI7_LVL}
  if Event = deDisabledStateChange then
  begin
    if Boolean(Info) then
      UpdateField
    else
      FField := nil;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ TDBAdvTrackBar }

constructor TDBAdvTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TTrackFieldDataLink.Create;
  FDataLink.Control := Self;
  ReadOnly := False;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

//------------------------------------------------------------------------------

destructor TDBAdvTrackBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.DataChange(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and (FDataLink.Field.DataType in [ftSmallint, ftInteger, ftFloat, ftLargeint, ftVariant]) then
  begin
    FInternalChange := True;
    try
      if FFocused and FDataLink.CanModify then
        Position := FDataLink.Field.AsInteger
      else
      begin
        Position := FDataLink.Field.AsInteger;
      end;
    finally
      FInternalChange := False;
    end;
  end
  else
  begin
    if (Position <> Min) then
      Position := Min;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.ActiveChange(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.SetPosition(Value: Integer);
begin
  if (Value <> Position) then
  begin
    if FInternalChange or (csLoading in ComponentState) then
      inherited
    else
    begin
      if ValidField and FDataLink.Edit then
      begin
        inherited;
        FDataLink.Modified;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.Change;
begin
  inherited Change;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  {if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
  }
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  DoExit;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.EditingChange(Sender: TObject);
begin
  //ReadOnly := not FDataLink.Editing;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
{  if (ssShift in Shift) then
    Exit;

  if (Key = vk_Prior) or (Key = vk_Next) or (Key = vk_End) or (Key = vk_Home) or (Key = vk_Left) or
     (Key = vk_Down) or (Key = vk_Right) or (Key = vk_Up) then
    FDataLink.Edit;}
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (key =  #27) then
  begin
    FDataLink.Reset;
    Key := #0;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
    DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    FDataLink.Reset;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

procedure TDBAdvTrackBar.UpdateData(Sender: TObject);
begin
  if FDataLink.Field.DataType = ftInteger then
    FDataLink.Field.AsInteger := Position;
end;

//------------------------------------------------------------------------------

function TDBAdvTrackBar.ValidField: Boolean;
begin
  Result := (FDataLink.Field <> nil) and (FDataLink.Field.DataType in [ftSmallint, ftInteger, ftFloat, ftLargeint, ftVariant]);
end;

//------------------------------------------------------------------------------

end.
