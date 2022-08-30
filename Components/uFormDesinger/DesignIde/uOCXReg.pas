{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit uOCXReg;

interface

uses Windows, ActiveX, SysUtils, Variants, ComObj, Classes, Graphics, Controls,
  Forms, Dialogs, TypInfo, uDesignIntf, uDesignEditors, OleCtrls;
             
type
{ TOleControlEditor }

  TOleControlEditor = class(TDefaultEditor)
  private
    FVerbs: TStringList;
  protected
    property Verbs: TStringList read FVerbs;
    procedure DoVerb(Verb: Integer); virtual;
  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TOleObjectEditor = class
  private
    FPropertyEditor: TPropertyEditor;
  public
    constructor Create(PropertyEditor: TPropertyEditor); virtual;
    function Edit(OleObject: Variant): Variant; virtual;
    property PropertyEditor: TPropertyEditor read FPropertyEditor;
  end;

  TOleFontEditor = class(TOleObjectEditor)
    function Edit(OleObject: Variant): Variant; override;
  end;

  TOleObjectProperty = class(TVariantProperty)
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TOleCustomProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TOlePropPageProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TOleEnumProperty = class(TOrdinalProperty)
  private
    FEnumPropDesc: TEnumPropDesc;
  protected
    property EnumPropDesc: TEnumPropDesc read FEnumPropDesc;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Initialize; override;
    procedure SetValue(const Value: string); override;
  end;

  TOleObjectEditorClass = class of TOleObjectEditor;

procedure RegisterOleObjectEditor(const IID: TIID; const ClassName: string;
  EditorClass: TOleObjectEditorClass);

procedure Register;

implementation

uses uDesignConst, uVCLEditors;

type
  POleObjectClassRec = ^TOleObjectClassRec;
  TOleObjectClassRec = record
    Next: POleObjectClassRec;
    IID: TIID;
    ClassName: string;
    EditorClass: TOleObjectEditorClass;
  end;

var
  OleObjectClassList: POleObjectClassRec = nil;

function GetOleObjectClassRec(OleObject: Variant): POleObjectClassRec;
var
  Dispatch: IDispatch;
  Unknown: IUnknown;
begin
  if VarType(OleObject) = varDispatch then
  begin
    Dispatch := IUnknown(OleObject) as IDispatch;
    if Dispatch <> nil then
    begin
      Result := OleObjectClassList;
      while Result <> nil do
      begin
        if Dispatch.QueryInterface(Result^.IID, Unknown) = 0 then  Exit;
        Result := Result^.Next;
      end;
    end;
  end;
  Result := nil;
end;

{ TOleControlEditor }

constructor TOleControlEditor.Create(AComponent: TComponent;
  ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FVerbs := TStringList.Create;
end;

destructor TOleControlEditor.Destroy;
begin
  FVerbs.Free;
  inherited Destroy;
end;

procedure TOleControlEditor.DoVerb(Verb: Integer);
begin
  try
    if Verb = -65536 then
      TOleControl(Component).ShowAboutBox
    else
      TOleControl(Component).DoObjectVerb(Verb);
  except
    // Rather than raising exceptions here (which is what we were doing)
    // just use MessageDlg to display a bit friendlier dialog to the user.
    case Verb of
      -65536: MessageDlg(SNoAboutBoxAvailable, mtInformation, [mbOk], 0);
      OLEIVERB_PROPERTIES: MessageDlg(SNoPropertyPageAvailable, mtInformation, [mbOk], 0);
    else
      raise;
    end;
  end;
end;

procedure TOleControlEditor.Edit;
begin
  DoVerb(OLEIVERB_PROPERTIES);
end;

procedure TOleControlEditor.ExecuteVerb(Index: Integer);
begin
  DoVerb(Integer(FVerbs.Objects[Index]));
end;

function TOleControlEditor.GetVerb(Index: Integer): string;
begin
  Result := FVerbs[Index];
end;

function TOleControlEditor.GetVerbCount: Integer;
var
  TI: ITypeInfo;
  W: WideString;
  N: Integer;
begin
  TOleControl(Component).GetObjectVerbs(FVerbs);
  if ((IUnknown(TOleControl(Component).OleObject) as IDispatch).GetTypeInfo(0,0,TI) = S_OK) and
    (TI.GetNames(DISPID_ABOUTBOX, @W, 1, N) = S_OK) and
    (FVerbs.IndexOf(SAboutVerb) = -1) and
    (FVerbs.IndexOfObject(TObject(-65536)) = -1) then
    FVerbs.AddObject(SAboutVerb, TObject(-65536));
  Result := FVerbs.Count;
end;

function MapOleCustomProperty(Obj: TPersistent;
  PropInfo: PPropInfo): TPropertyEditorClass;
begin
  Result := nil;
  if (DWORD(PropInfo^.Index) <> $80000000) and (Obj is TOleControl) then
  begin
    if TOleControl(Obj).IsPropPageProperty(PropInfo^.Index) then
      Result := TOlePropPageProperty
    else if TOleControl(Obj).IsCustomProperty(PropInfo^.Index) then
      Result := TOleCustomProperty;
  end;
end;

{ TOleCustomProperty }

function TOleCustomProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

function TOleCustomProperty.GetValue: string;
begin
  Result := TOleControl(GetComponent(0)).GetPropDisplayString(
    GetPropInfo^.Index);
end;

procedure TOleCustomProperty.GetValues(Proc: TGetStrProc);
var
  Values: TStringList;
  I: Integer;
begin
  Values := TStringList.Create;
  try
    TOleControl(GetComponent(0)).GetPropDisplayStrings(
      GetPropInfo^.Index, Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TOleCustomProperty.SetValue(const Value: string);
begin
  TOleControl(GetComponent(0)).SetPropDisplayString(
    GetPropInfo^.Index, Value);
end;

{ TOlePropPageProperty }

function TOlePropPageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect];
end;

procedure TOlePropPageProperty.Edit;
var
  PPID: TCLSID;
  OleCtl: TOleControl;
  OleCtls: array of IDispatch;
  Params: TOCPFIParams;
  Caption: WideString;
  I, DispID: Integer;
begin
  SetLength(OleCtls, PropCount);
  for I := 0 to PropCount - 1 do
  begin
    OleCtls[I] := TOleControl(GetComponent(0)).DefaultDispatch;
    if Caption <> '' then Caption := Caption + ', ';
    Caption := Caption + TOleControl(GetComponent(0)).Name;
  end;
  OleCtl := TOleControl(GetComponent(0));
  if OleCtl.PerPropBrowsing <> nil then
  begin
    DispID := GetPropInfo^.Index;
    OleCtl.PerPropBrowsing.MapPropertyToPage(DispID, PPID);
    if not IsEqualCLSID(PPID, GUID_NULL) then
    begin
      with Params do
      begin
        cbStructSize := SizeOf(Params);
        hWndOwner := GetActiveWindow;
        x := 16;
        y := 16;
        lpszCaption := PWideChar(Caption);
        cObjects := PropCount;
        pObjects := @OleCtls[0];
        cPages := 1;
        pPages := @PPID;
        lcid := GetUserDefaultLCID;
        dispidInitialProperty := DispID;
      end;
      OleCreatePropertyFrameIndirect(Params);
    end;
  end;
end;

{ TOleEnumProperty }

function TOleEnumProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

function TOleEnumProperty.GetValue: string;
begin
  if FEnumPropDesc <> nil then
    Result := FEnumPropDesc.ValueToString(GetOrdValue)
  else
    Result := IntToStr(GetOrdValue);
end;

procedure TOleEnumProperty.GetValues(Proc: TGetStrProc);
begin
  if FEnumPropDesc <> nil then FEnumPropDesc.GetStrings(Proc);
end;

procedure TOleEnumProperty.Initialize;
begin
  FEnumPropDesc := TOleControl(GetComponent(0)).GetEnumPropDesc(
    GetPropInfo^.Index);
end;

procedure TOleEnumProperty.SetValue(const Value: string);
begin
  if FEnumPropDesc <> nil then
    SetOrdValue(FEnumPropDesc.StringToValue(Value))
  else
    SetOrdValue(StrToInt(Value));
end;

{ TOleObjectEditor }

constructor TOleObjectEditor.Create(PropertyEditor: TPropertyEditor);
begin
  FPropertyEditor := PropertyEditor;
end;

function TOleObjectEditor.Edit(OleObject: Variant): Variant;
begin
  VarClear(Result);
end;

{ TOleFontEditor }

function TOleFontEditor.Edit(OleObject: Variant): Variant;
begin
  VarClear(Result);
  with TFontDialog.Create(Application) do
    try
      OleFontToFont(OleObject, Font);
      Options := Options + [fdForceFontExist];
      if Execute then Result := FontToOleFont(Font);
    finally
      Free;
    end;
end;

{ TOleObjectProperty }

procedure TOleObjectProperty.Edit;
var
  P: POleObjectClassRec;
  Value: Variant;
  Editor: TOleObjectEditor;
begin
  Value := GetVarValue;
  P := GetOleObjectClassRec(Value);
  if P <> nil then
  begin
    Editor := P^.EditorClass.Create(Self);
    try
      Value := Editor.Edit(Value);
    finally
      Editor.Free;
    end;
    if VarType(Value) = varDispatch then
      SetVarValue(Value);
  end;
end;

function TOleObjectProperty.GetAttributes: TPropertyAttributes;
var
  Value: Variant;
begin
  Result := inherited GetAttributes;
  try
    Value := GetVarValue;
  except
    Value := Null;
  end;
  if GetOleObjectClassRec(Value) <> nil then
    Result := Result + [paReadOnly, paDialog];
end;

function TOleObjectProperty.GetValue: string;

  function GetVariantStr(const Value: Variant): string;
  begin
    case VarType(Value) of
      varEmpty: Result := '';
      varNull: Result := SNull;
      varBoolean:
        if Value then
          Result := BooleanIdents[True]
        else
          Result := BooleanIdents[False];
      varCurrency:
        Result := CurrToStr(Value);
    else
      Result := string(Value);
    end;
  end;

var
  P: POleObjectClassRec;
  Value: Variant;
begin
  Value := GetVarValue;
  if VarType(Value) <> varDispatch then
    Result := inherited GetValue
  else
  begin
    P := GetOleObjectClassRec(Value);
    if P <> nil then
      Result := '(' + P^.ClassName + ')'
    else
      Result := '(OleObject)';
  end
end;

procedure RegisterOleObjectEditor(const IID: TIID; const ClassName: string;
  EditorClass: TOleObjectEditorClass);
var
  P: POleObjectClassRec;
begin
  New(P);
  P^.Next := OleObjectClassList;
  P^.IID := IID;
  P^.ClassName := ClassName;
  P^.EditorClass := EditorClass;
  OleObjectClassList := P;
end;

{ Registration }

procedure Register;
begin
  RegisterComponentEditor(TOleControl, TOleControlEditor);
  RegisterPropertyMapper(MapOleCustomProperty);
  RegisterPropertyEditor(TypeInfo(TOleEnum), TOleControl, '', TOleEnumProperty);
  RegisterPropertyEditor(TypeInfo(Variant), nil, '', TOleObjectProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), TOleControl, 'Cursor', TCursorProperty);
  RegisterOleObjectEditor(IFontDisp, 'OleFont', TOleFontEditor);
end;

end.
