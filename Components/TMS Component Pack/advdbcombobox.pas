{***************************************************************************}
{ TAdvDBComboBox component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2012                                        }
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

unit AdvDBComboBox;

interface

uses
  AdvCombo, Windows, Classes, StdCtrls, SysUtils, Graphics, DB, DBCtrls, Messages, Controls, ImgList;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.0.0.0 : First release
  // 1.0.1.0 : New : Added method AddDisplayedAndStoredValue()
  // 1.0.1.1 : Fixed : Issue when dataset.AutoEdit = false 


type
  TAdvDBComboBoxStrings = class(TCustomComboBoxStrings)
  private
    function GetStoredStrings(Index: Integer): String;
    procedure PutStoredStrings(Index: Integer; const Value: String);
    procedure ReadStoredData(Reader: TReader);
    procedure WriteStoredData(Writer: TWriter);
  protected
    FStoredStrings: TStringList;
    procedure DefineProperties(Filer: TFiler); override;
    procedure CheckStoredStrings;
    function Get(Index: Integer): string; override;
    function AddStored(StoredS: string): Integer; overload;
  public
    constructor Create; {$IFDEF DELPHI_UNICODE} override; {$ENDIF}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(const S: string): Integer; override;
    function AddStored(const S: string; StoredS: string): Integer; overload;
    function AddStoredObject(const S: string; StoredS: string; AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertStored(Index: Integer; const S: string; StoredS: string);
    procedure Clear; override;
    procedure ClearAll;
    procedure Delete(Index: Integer); override;
    function IndexOfStored(s: string): Integer;
    property StoredStrings[Index: Integer]: String read GetStoredStrings write PutStoredStrings;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDBComboBox = class(TAdvCustomCombo)
  private
    FDataLink: TFieldDataLink;
    FImages: TCustomImageList;
    FShowImages: Boolean;
    FLabelField: string;
    procedure ActiveChange(Sender: TObject);
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetComboText: string;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetComboText(const Value: string);
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function GetStoredStrings(Index: Integer): String;
    procedure PutStoredStrings(Index: Integer; const Value: String);
    function GetAdvItems: TAdvDBComboBoxStrings;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShowImages(const Value: Boolean);
    procedure SetLabelField(const Value: string);
    function GetLabelField: string;
  protected
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetItems(const Value: TStrings); override;
    procedure SetStyle(Value: TComboboxStyle); override;
    procedure WndProc(var Message: TMessage); override;
    function GetItemsClass: TCustomComboBoxStringsClass; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure UpdateDBLabel;
    function CheckDataSet: Boolean;
    function GetVersionNr: Integer; override;
    property AdvItems: TAdvDBComboBoxStrings read GetAdvItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    procedure AddItem(Item, StoredValue: String; AObject: TObject); reintroduce;
    procedure Clear; override;
    procedure AddDisplayAndStoredValue(Displayed, Stored: string);
    function StoredToDispText(StoredText: String): String;
    function GetDisplayValue(Index: Integer): String;
    function GetStoredValue(Index: Integer): String;
    property StoredStrings[Index: Integer]: String read GetStoredStrings write PutStoredStrings;
    property Field: TField read GetField;
    property Text;
  published
    property Style; // Must be published before Items
    property Align;
    property AutoComplete;
    property AutoFocus;
    property AutoDropDown;
    property BevelWidth;
    property ButtonWidth;
    property CharCase;
    property Color;
    property Ctl3D;
    property DisabledBorder;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property DropWidth;
    property Flat;
    property FlatLineColor;
    property FlatParentColor;
    property Etched;
    property FocusBorder;
    property FocusBorderColor;
    property FocusColor;
    property Enabled;
    property Font;
    property Images: TCustomImageList read FImages write SetImages;  // ShowImages must be true to display images
    property ImeMode;
    property ImeName;
    property ItemIndex;
    property ItemHeight;
    property Items write SetItems;
    property LabelCaption;
    property LabelField: string read GetLabelField write SetLabelField;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property ShowImages: Boolean read FShowImages write SetShowImages default False;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnDropUp;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$IFDEF DELPHI7_LVL}
    property BevelKind;
    property BevelInner;
    property BevelOuter;
    property BevelEdges;
    {$ENDIF}    
  end;


implementation

uses
  VDBConsts, Consts;

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

{ TAdvDBComboBox }

procedure TAdvDBComboBox.ActiveChange(Sender: TObject);
begin
  if Assigned(FDataLink) then
  begin
    if Assigned(FDataLink.DataSet) then
    begin
      if not FDataLink.DataSet.Active then
        Text := '';
    end
    else
    begin
      Text := '';
    end;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvDBComboBox.AddDisplayAndStoredValue(Displayed, Stored: string);
begin
  Items.Add(Displayed);
  StoredStrings[Items.Count - 1] := Stored;
end;

procedure TAdvDBComboBox.AddItem(Item, StoredValue: String; AObject: TObject);
begin
  TAdvDBComboBoxStrings(Items).AddStoredObject(Item, StoredValue, AObject);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.CheckDataSet: Boolean;
begin
  Result := Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.Clear;
begin
  inherited;
   TAdvDBComboBoxStrings(Items).ClearAll;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(False), 0);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LInteger(FDataLink);
end;

//------------------------------------------------------------------------------

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    if itemState and ODS_COMBOBOXEDIT <> 0 then
      Include(State, odComboBoxEdit);
    if itemState and ODS_DEFAULT <> 0 then
      Include(State, odDefault);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;

    if (Integer(itemID) >= 0) and (odSelected in State) and not (odComboBoxEdit in State) then
    begin
       if IsVista then
      begin
        Canvas.Brush.Color := $FF9933;
        Canvas.Font.Color := clWhite;
      end
      else
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText
      end;
    end;

    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      Canvas.FillRect(rcItem);

    if (odFocused in State) then
      DrawFocusRect(hDC, rcItem);

    Canvas.Handle := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then OnDrawItem(Self, Index, Rect, State)
  else
  begin
    Canvas.FillRect(Rect);
    if Index >= 0 then
    begin
      if ShowImages and Assigned(Images) then
      begin
        Images.Draw(Canvas, Rect.Left + 2, Rect.Top + 1, StrToInt(Items[Index]));
      end
      else
        Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetShowImages(const Value: Boolean);
begin
  //if (FShowImages <> Value) then
  begin
    FShowImages := Value;
    if FShowImages then
    begin
      //ReadOnly := True;
      if (Style <> csOwnerDrawVariable) then
        Style := csOwnerDrawFixed;

      {if Assigned(Images) then
      begin
        for I := 0 to Images.Count - 1 do
        begin
          Clear;
          Items[I] := InttoStr(I);
          AdvITems.StoredStrings[I] := InttoStr(I);
        end;
      end;
      }
    end
    else
    begin
      //ReadOnly := False;
      if (Style = csOwnerDrawFixed) then
        Style := csDropDown;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if (Value = csSimple) and Assigned(FDatalink) and FDatalink.DatasourceFixed then
    DatabaseError(SNotReplicatable);
  if not (Value in [csOwnerDrawFixed, csOwnerDrawVariable]) and ShowImages then
    raise Exception.Create('Style must be csOwnerDrawFixed or csOwnerDrawVariable when ShowImages = True');

  inherited SetStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetReadOnly(Value: Boolean);
begin
  //if not Value and ShowImages then
    //raise Exception.Create('ReadOnly must be True when ShowImages = True');

  FDataLink.ReadOnly := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

//------------------------------------------------------------------------------

constructor TAdvDBComboBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnActiveChange := ActiveChange;
  FShowImages := False;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.StoredToDispText(StoredText: String): String;
var
  i: Integer;
begin
  Result := StoredText;
  i := AdvItems.IndexOfStored(StoredText);
  if (i >= 0) and (i < Items.Count) then
  begin
    Result := AdvItems.Strings[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.DataChange(Sender: TObject);
begin
  if not Assigned(FDataLink.DataSet) then
    Exit;

  if Assigned(FDataLink.Field) then
  begin
    if not (Style = csSimple) and DroppedDown then Exit;
    if FDataLink.Field <> nil then
      SetComboText(StoredToDispText(FDataLink.Field.Text))
    else
      if csDesigning in ComponentState then
        SetComboText(Name)
      else
        SetComboText('');
    UpdateDBLabel;    
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.UpdateData(Sender: TObject);
begin
  if Assigned(FDataLink.Field) and FDataLink.Editing then
  begin
    if ItemIndex >= 0 then
      FDataLink.Field.Text := StoredStrings[ItemIndex];
    //else
      //FDataLink.Field.Text := GetComboText;
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then FDataLink.Edit else
          if not FDataLink.Editing then DataChange(Self); {Restore text}
      WM_CREATE,
      WM_WINDOWPOSCHANGED,
      CM_FONTCHANGED:
        ;//FPaintControl.DestroyHandle;
    end;
  inherited WndProc(Message);
end;

//------------------------------------------------------------------------------

destructor TAdvDBComboBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.DropDown;
begin
  inherited DropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetAdvItems: TAdvDBComboBoxStrings;
begin
  Result := TAdvDBComboBoxStrings(Items);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then Result := Text else
  begin
    I := ItemIndex;
    if I < 0 then Result := '' else Result := Items[I];
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetItemsClass: TCustomComboBoxStringsClass;
begin
  Result := TAdvDBComboBoxStrings;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetDisplayValue(Index: Integer): String;
begin
  Result := '';
  if Index < Items.Count then
    Result := Items[Index];
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetStoredValue(Index: Integer): String;
begin
  Result := '';
  if Index < Items.Count then
    Result := TAdvDBComboBoxStrings(Items).StoredStrings[Index];
end;

function TAdvDBComboBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.KeyPress(var Key: Char);
begin
  if Assigned(DataSource) and not FDataLink.Edit and not DataSource.AutoEdit then
  begin
    Key := #0;
    Exit;
  end;

  inherited KeyPress(Key);
  {if (Key in [#32..#255]) and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;}
  case Key of
    ^H, ^V, ^X, #32..#255:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (Operation = opRemove) and (FDataLink <> nil)
      and (AComponent = DataSource) then
    DataSource := nil;

  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.PutStoredStrings(Index: Integer; const Value: String);
begin
  TAdvDBComboBoxStrings(Items).StoredStrings[Index] := Value;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetStoredStrings(Index: Integer): String;
begin
  Result := TAdvDBComboBoxStrings(Items).StoredStrings[Index];
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> GetComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then I := -1 else I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage(EditHandle, EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  DataChange(Self);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.GetLabelField: string;
begin
  Result := FLabelField;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.SetLabelField(const Value: string);
begin
  if (FLabelField <> Value) then
  begin
    FLabelField := Value;
    UpdateDBLabel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBox.UpdateDBLabel;
var
  fld: TField;
begin
  if CheckDataSet then
  begin
    if Assigned(Self.DataSource) and Assigned(Self.DataSource.DataSet) and Self.DataSource.DataSet.Active and (LabelField <> '') then
      fld := Self.DataSource.DataSet.Fields.FieldByName(LabelField)
    else
      fld := nil;

    if Assigned(fld) then
      LabelCaption := fld.Text;
  end
  else
  begin
    if LabelField <> '' then
      LabelCaption := '';
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;

//------------------------------------------------------------------------------

{ TAdvDBComboBoxStrings }

constructor TAdvDBComboBoxStrings.Create;
begin
  inherited;
  CheckStoredStrings;
end;

//------------------------------------------------------------------------------

destructor TAdvDBComboBoxStrings.Destroy;
begin
  if Assigned(FStoredStrings) then
    FStoredStrings.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.DefineProperties(Filer: TFiler);
  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TStrings then
        Result := not Equals(TStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;
begin
  inherited;

  Filer.DefineProperty('StoredStrings', ReadStoredData, WriteStoredData, DoWrite);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.Delete(Index: Integer);
begin
  CheckStoredStrings;
  inherited;
  if Index < FStoredStrings.Count then
    FStoredStrings.Delete(Index);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.Add(const S: string): Integer;
begin
  CheckStoredStrings;
  Result := SendMessage(ComboBox.Handle, CB_ADDSTRING, 0, LParam(PChar(S)));
  if Result < 0 then
    raise EOutOfResources.Create(SInsertLineError)
  else
  begin
    while Result >= FStoredStrings.Count do
      FStoredStrings.Add('');
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.AddStored(const S: string; StoredS: string): Integer;
begin
  Result := Add(S);
  PutStoredStrings(Result, StoredS);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.AddStored(StoredS: string): Integer;
begin
  CheckStoredStrings;
  Result := FStoredStrings.Add(StoredS);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.AddStoredObject(const S: string; StoredS: string;
  AObject: TObject): Integer;
begin
  Result := AddObject(S, AObject);
  PutStoredStrings(Result, StoredS);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TAdvDBComboBoxStrings) then
  begin
    CheckStoredStrings;
    FStoredStrings.Assign((Source as TAdvDBComboBoxStrings).FStoredStrings);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.CheckStoredStrings;
begin
  if not Assigned(FStoredStrings) then
    FStoredStrings := TStringList.Create;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.Clear;
begin
  inherited;
  if Assigned(ComboBox) and not (csReading in ComboBox.ComponentState) and not (csLoading in ComboBox.ComponentState)
    and not (csDesigning in ComboBox.ComponentState) then
  begin
    //CheckStoredStrings;
    //FStoredStrings.Clear;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.ClearAll;
begin
  Clear;
  CheckStoredStrings;
  FStoredStrings.Clear;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.Get(Index: Integer): string;
begin
  Result := inherited Get(Index);
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.GetStoredStrings(Index: Integer): String;
begin
  CheckStoredStrings;
  Result := Strings[Index];
  if Index < FStoredStrings.Count then
    Result := FStoredStrings[Index];
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.PutStoredStrings(Index: Integer;
  const Value: String);
begin
  CheckStoredStrings;
  if Index < FStoredStrings.Count then
    FStoredStrings[Index] := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.ReadStoredData(Reader: TReader);
begin
  CheckStoredStrings;
  Reader.ReadListBegin;
  BeginUpdate;
  try
    FStoredStrings.Clear;
    while not Reader.EndOfList do AddStored(Reader.ReadString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.WriteStoredData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(StoredStrings[I]);
  Writer.WriteListEnd;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxStrings.IndexOfStored(s: string): Integer;
begin
  CheckStoredStrings;
  Result := FStoredStrings.IndexOf(S);
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.Insert(Index: Integer; const S: string);
begin
  CheckStoredStrings;

  if SendMessage(ComboBox.Handle, CB_INSERTSTRING, Index, LParam(PChar(S))) < 0 then
    raise EOutOfResources.Create(SInsertLineError)
  else
  begin
    while Index >= FStoredStrings.Count do
      FStoredStrings.Add('');
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxStrings.InsertStored(Index: Integer; const S: string;
  StoredS: string);
begin
  Insert(Index, S);
  PutStoredStrings(Index, StoredS);
end;

//------------------------------------------------------------------------------


end.
