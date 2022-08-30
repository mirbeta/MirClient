{***************************************************************************}
{ TRTTIInspectorBar component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit RTTIInspectorBar;

{$I TMSDEFS.INC}

interface

uses
  InspectorBar, Classes, Messages, Windows, Controls, TypInfo, SysUtils,
  Graphics;

type
  TItemInsertEvent = procedure(Sender: TObject; var PropertyName: string; PropertyType: TTypeKind; var Allow: boolean) of object;

  TRTTIInspectorItem = class(TInspectorItem)
  private
    FObjectRef: TObject;
    FSetProp: Boolean;
    FSetVal: Integer;
    FPropInfo: PPropInfo;
    FPropName: string;
  protected
    procedure EditStart; override;
    procedure EditStop; override;
    procedure EditChange; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property ObjectRef: TObject read FObjectRef write FObjectRef;
    property SetProp: Boolean read FSetProp write FSetProp;
    property SetVal: Integer read FSetVal write FSetVal;
    property PropInfo: PPropInfo read FPropInfo write FPropInfo;
    property PropName: string read FPropName write FPropName;
  published
  end;

  TRTTIInspectorItems = class(TInspectorItems)
  private
    function GetItem(Index: Integer): TRTTIInspectorItem;
    procedure SetItem(Index: Integer; const Value: TRTTIInspectorItem);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TRTTIInspectorItem;
    function Insert(index: Integer): TRTTIInspectorItem;
    property Items[Index: Integer]: TRTTIInspectorItem read GetItem write SetItem; default;
  published
  end;

  TRTTIInspectorPanel = class(TInspectorPanel)
  private
    FRTTIComponent: TPersistent;
    procedure SetRTTIComponent(const Value: TPersistent);
  public
    constructor Create(Collection: TCollection); override;
    function CreateItems: TInspectorItems; override;
    procedure Refresh;
    procedure RefreshItem(InspectorItem: TInspectorItem);
  published
    property RTTIComponent: TPersistent read FRTTIComponent write SetRTTIComponent;
  end;

  TRTTIInspectorPanels = class(TInspectorPanels)
  private
    function GetItem(Index: Integer): TRTTIInspectorPanel;
    procedure SetItem(Index: Integer; const Value: TRTTIInspectorPanel);
  public
    function CreateItemClass: TCollectionItemClass; override;
    function Add: TRTTIInspectorPanel;
    function Insert(index: Integer): TRTTIInspectorPanel;
    property Items[Index: Integer]: TRTTIInspectorPanel read GetItem write SetItem; default;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRTTIInspectorBar = class(TInspectorBar)
  private
    FEdited: Boolean;
    FOnItemInsert: TItemInsertEvent;
    function GetPanels: TRTTIInspectorPanels;
    procedure SetPanels(const Value: TRTTIInspectorPanels);
  public
    function CreatePanels: TInspectorPanels; override;
    procedure StartEdit(InspectorItem: TInspectorItem); override;
    procedure StopEdit(InspectorItem: TInspectorItem); override;
    procedure GetValueList(InspectorItem: TInspectorItem; Values: TStringList); override;
  published
    property Panels: TRTTIInspectorPanels read GetPanels write SetPanels;
    property OnItemInsert: TItemInsertEvent read FOnItemInsert write FOnItemInsert;
  end;

implementation

{ TRTTIInspectorBar }

{$IFNDEF DELPHI6_LVL}
function SetToString(PropInfo: PPropInfo; Value: Integer; Brackets: Boolean): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  TypeInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;
{$ENDIF}

function TRTTIInspectorBar.CreatePanels: TInspectorPanels;
begin
  Result := TRTTIInspectorPanels.Create(Self);
end;

function TRTTIInspectorBar.GetPanels: TRTTIInspectorPanels;
begin
  Result := TRTTIInspectorPanels(inherited Panels);
end;

procedure TRTTIInspectorBar.GetValueList(InspectorItem: TInspectorItem;
  Values: TStringList);
var
  TypeData : PTypeData;
  PropInfo: PPropInfo;
  i: Integer;
begin
  inherited;

  PropInfo := GetPropInfo(TRTTIInspectorItem(InspectorItem).ObjectRef,
    TRTTIInspectorItem(InspectorItem).PropName,tkProperties);

   if PropInfo^.PropType^.Kind = tkEnumeration then
   begin
     TypeData := GetTypeData(PropInfo^.PropType^);

     for i := TypeData^.MinValue to TypeData^.MaxValue do
     begin
       Values.Add(GetEnumName(TypeData^.BaseType^,i));
     end;
   end;
end;

procedure TRTTIInspectorBar.SetPanels(const Value: TRTTIInspectorPanels);
begin
  inherited Panels := Value;
end;

procedure TRTTIInspectorBar.StartEdit(InspectorItem: TInspectorItem);
begin
  FEdited := True;
  inherited;
end;

procedure TRTTIInspectorBar.StopEdit(InspectorItem: TInspectorItem);
var
  TypeData : PTypeData;
  PropInfo: PPropInfo;
  i: Integer;
  AObject: TObject;
  NewVal: Integer;
  SuBItem: TRTTIInspectorItem;
  d: double;
begin
  inherited;

  if not FEdited then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  FEdited := False;

  if not Assigned(InspectorItem) then
    Exit;

  if InspectorItem.PropertyType = ptText then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

    if Assigned(AObject) then
    begin
      SetStrProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, InspectorItem.TextValue);

      InspectorItem.TextValue := GetStrProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
    end;
  end;

  if InspectorItem.PropertyType = ptBoolean then
  begin

    if TRTTIInspectorItem(InspectorItem).SetProp then
    begin
      AObject := TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).ObjectRef;

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('*'+TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).Caption+'*'));
      {$ENDIF}

      SuBItem := TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef);

      if InspectorItem.BoolValue then
        SetOrdProp(AObject, TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).PropName,
          SubItem.IntValue or TRTTIInspectorItem(InspectorItem).SetVal)
      else
        SetOrdProp(AObject, TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).PropName,
          SubItem.IntValue AND NOT TRTTIInspectorItem(InspectorItem).SetVal);

      NewVal := GetOrdProp(AObject,TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).PropName);
      SubItem.IntValue := NewVal;

      with SubItem do
      begin
        TextValue := SetToString(PropInfo, NewVal ,True);
        {$IFDEF TMSDEBUG}
        outputdebugstring(pchar(TextValue));
        {$ENDIF}
      end;

    end
    else
    begin
      AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

      if Assigned(AObject) then
      if InspectorItem.BoolValue then
      begin
        SetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, 1);
        InspectorItem.TextValue := 'True';
      end
      else
      begin
        SetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, 0);
        InspectorItem.TextValue := 'False';
      end;

    end;
  end;

  if InspectorItem.PropertyType in [ptInteger,ptIntSpin] then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

    if Assigned(AObject) then
    begin
      SetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, InspectorItem.IntValue);
      InspectorItem.IntValue := GetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
    end;
  end;

  if InspectorItem.PropertyType in [ptFloat] then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
    begin
      try
        d := StrToFloat(InspectorItem.TextValue);
      except
        d := 0;
      end;
      SetFloatProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, d);
      d := GetFloatProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
      InspectorItem.TextValue := FloatToStr(d);
    end;
  end;


  if InspectorItem.PropertyType = ptColor then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

    if Assigned(AObject) then
    begin
      SetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, Integer(InspectorItem.ColorValue));

      InspectorItem.ColorValue := TColor(GetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName));
    end;
  end;

  if InspectorItem.PropertyType = ptFont then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
      SetObjectProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, TObject(InspectorItem.FontValue));
  end;

  if (InspectorItem.PropertyType in [ptValues,ptValuesList]) then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

    if Assigned(AObject) then
    begin
      // PropInfo := GetPropInfo(TRTTIInspectorPanel(InspectorItem.InspectorPanel).RTTIComponent,
      // InspectorItem.Caption,tkProperties);

      PropInfo := GetPropInfo(AObject, TRTTIInspectorItem(InspectorItem).PropName,tkProperties);

      if PropInfo^.PropType^.Kind = tkEnumeration then
      begin
         TypeData := GetTypeData(PropInfo^.PropType^);
         i := GetEnumValue(TypeData^.BaseType^,InspectorItem.TextValue);

         if Assigned(AObject) then
           SetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, i);
     end
     else
       SetStrProp(AObject, TRTTIInspectorItem(InspectorItem).PropName, InspectorItem.TextValue);
    end;

  end;
end;

{ TRTTIInspectorPanels }

function TRTTIInspectorPanels.Add: TRTTIInspectorPanel;
begin
  Result := TRTTIInspectorPanel(inherited Add);
end;

function TRTTIInspectorPanels.CreateItemClass: TCollectionItemClass;
begin
  Result := TRTTIInspectorPanel;
end;

function TRTTIInspectorPanels.GetItem(Index: Integer): TRTTIInspectorPanel;
begin
  Result := TRTTIInspectorPanel(inherited Items[Index]);
end;

function TRTTIInspectorPanels.Insert(index: Integer): TRTTIInspectorPanel;
begin
  Result := TRTTIInspectorPanel(inherited Insert(Index));
end;

procedure TRTTIInspectorPanels.SetItem(Index: Integer;
  const Value: TRTTIInspectorPanel);
begin
  inherited Items[Index] := Value;
end;

{ TRTTIInspectorPanel }

constructor TRTTIInspectorPanel.Create(Collection: TCollection);
begin
  inherited;
  Style := psProperties;
  ItemHeight := 26;
end;


function TRTTIInspectorPanel.CreateItems: TInspectorItems;
begin
  Result := TRTTIInspectorItems.Create(Self);
end;

procedure TRTTIInspectorPanel.RefreshItem(InspectorItem: TInspectorItem);
var
  AObject: TObject;
  d: double;
  RTTIItem: TRTTIInspectorItem;

begin
  if InspectorItem.PropertyType = ptText then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
      InspectorItem.TextValue := GetStrProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
  end;

  if InspectorItem.PropertyType = ptBoolean then
  begin
    if TRTTIInspectorItem(InspectorItem).SetProp then
    begin
      AObject := TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).ObjectRef;

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('*'+TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).Caption+'*'));
      {$ENDIF}

      InspectorItem.BoolValue := (GetOrdProp(AObject, TRTTIInspectorItem(TRTTIInspectorItem(InspectorItem).ObjectRef).PropName) and
         TRTTIInspectorItem(InspectorItem).SetVal = TRTTIInspectorItem(InspectorItem).SetVal);
    end
    else
    begin
      AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

      if Assigned(AObject) then
      begin
        InspectorItem.BoolValue := GetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName) = 1;

        if InspectorItem.BoolValue then
          InspectorItem.TextValue := 'True'
        else
          InspectorItem.TextValue := 'False';
      end;

    end;
  end;

  if InspectorItem.PropertyType in [ptInteger,ptIntSpin] then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;

    if Assigned(AObject) then
      InspectorItem.IntValue := GetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
  end;

  if InspectorItem.PropertyType in [ptFloat] then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
    begin
       d := GetFloatProp(AObject,TRTTIInspectorItem(InspectorItem).PropName);
       InspectorItem.TextValue := Format('%g',[d]);
    end;
  end;

  {
  if InspectorItem.PropertyType in [ptValues, ptValuesList] then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
      InspectorItem.TextValue := GetStrProp(AObject, TRTTIInspectorItem(InspectorItem).PropName);
  end;
  }

  if InspectorItem.PropertyType = ptColor then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
      InspectorItem.ColorValue := TColor(GetOrdProp(AObject, TRTTIInspectorItem(InspectorItem).PropName));
  end;

  if InspectorItem.PropertyType = ptFont then
  begin
    AObject := TRTTIInspectorItem(InspectorItem).ObjectRef;
    if Assigned(AObject) then
      InspectorItem.FontValue.Assign(TFont(GetObjectProp(AObject,TRTTIInspectorItem(InspectorItem).PropName)));
  end;

  RTTIItem := TRTTIInspectorItem(InspectorItem);
  if Assigned(RTTIItem.PropInfo) then
  begin
    if (RTTIItem.PropInfo.PropType^.Kind in [tkSet]) then
    begin
      AObject := RTTIItem.ObjectRef;
      InspectorItem.TextValue := SetToString(RTTIItem.PropInfo, GetOrdProp(AObject,string(RTTIItem.PropInfo.Name)),True);
    end;
  end;


end;

procedure TRTTIInspectorPanel.Refresh;
var
  i: Integer;
begin
  for i := 1 to Items.Count - 1 do
    RefreshItem(Items[i - 1]);
end;

procedure TRTTIInspectorPanel.SetRTTIComponent(const Value: TPersistent);
var
  Allow: Boolean;
  APropname: string;

  procedure AddProps(AObject: TObject; TypeInfo: PTypeInfo; Level: Integer);
  var
    i,j,k,Count: Integer;
    PropList: PPropList;
    CurrentProperty: PPropInfo;
    RTTIItem: TRTTIInspectorItem;
    TypeData : PTypeData;
    SubClass: TClass;
    SubObject: TObject;
    SubTypeInfo: PTypeInfo;
    s:string;
  begin
    Count := GetPropList(TypeInfo,tkProperties,nil);
    GetMem(PropList, Count * SizeOf(PPropInfo));
    try
      GetPropList(TypeInfo, tkProperties, PropList);
      for I := 1 to Count do
      begin
        CurrentProperty := PropList^[i - 1];
        Allow := Assigned(CurrentProperty) and Assigned(CurrentProperty.GetProc);

        if Allow then
        begin
          APropName := string(CurrentProperty.Name);

          if Assigned(TRTTIInspectorBar(InspectorBar).OnItemInsert) then
            TRTTIInspectorBar(InspectorBar).OnItemInsert(InspectorBar, APropName,
               CurrentProperty.PropType^.Kind, Allow);
        end;

        if Allow then
        begin
          RTTIItem := TRTTIInspectorItem(Items.Add);
          RTTIItem.Caption := APropName;
          RTTIItem.PropName := string(CurrentProperty.Name);
          RTTIItem.Level := Level;
          RTTIItem.ObjectRef := AObject;
          RTTIItem.ReadOnly := not Assigned(CurrentProperty.SetProc);

          if CurrentProperty.PropType^.Name = 'TComponentName' then
            RTTIItem.ReadOnly := true;

          s := string(CurrentProperty.PropType^.Name);
          {$IFDEF TMSDEBUG}
          outputdebugstring(pchar(RTTIItem.Caption+':'+s));
          {$ENDIF}

          if CurrentProperty.PropType^.Name = 'Boolean' then
          begin
            RTTIItem.PropertyType := ptBoolean;
            RTTIItem.BoolValue := GetOrdProp(AObject,string(CurrentProperty.Name)) <> 0;
            if RTTIItem.BoolValue then
              RTTIItem.TextValue := 'True'
            else
              RTTIItem.TextValue := 'False';
          end;

          {$IFDEF DELPHI_UNICODE}
          if CurrentProperty.PropType^.Kind in [tkString,tkLString,tkUString] then
          {$ENDIF}
          {$IFNDEF DELPHI_UNICODE}
          if CurrentProperty.PropType^.Kind in [tkString,tkLString] then
          {$ENDIF}
          begin
            RTTIItem.PropertyType := ptText;
            RTTIItem.TextValue := GetStrProp(AObject,string(CurrentProperty.Name));
          end;

          if CurrentProperty.PropType^.Kind in [tkFloat] then
          begin
            RTTIItem.PropertyType := ptFloat;
            RTTIItem.TextValue := Format('%g',[GetFloatProp(AObject,string(CurrentProperty.Name))]);
          end;

          if CurrentProperty.PropType^.Kind in [tkInteger,tkInt64] then
          begin
            if CurrentProperty.PropType^.Name <> 'TColor' then
            begin
              RTTIItem.PropertyType := ptIntSpin;
              RTTIItem.IntValue := GetOrdProp(AObject,string(CurrentProperty.Name));

            end
            else
            begin
              RTTIItem.PropertyType := ptColor;
              RTTIItem.ColorValue := GetOrdProp(AObject,string(CurrentProperty.Name));
            end;
          end;

          if CurrentProperty.PropType^.Kind in [tkClass] then
          begin
            if CurrentProperty.PropType^.Name = 'TFont' then
            begin
              RTTIItem.PropertyType := ptFont;
              RTTIItem.FontValue.Assign(TFont(GetObjectProp(AObject,string(CurrentProperty.Name))));
            end
            else
            begin
              RTTIItem.ReadOnly := True;
              SubClass := GetTypeData(CurrentProperty.PropType^).ClassType;
              SubObject := TObject(GetOrdProp(AObject,string(CurrentProperty.Name)));
              if Assigned(SubObject) then
                AddProps(SubObject,SubClass.ClassInfo,Level + 1);
            end;
          end;

          if CurrentProperty.PropType^.Kind in [tkSet] then
          begin
            RTTIItem.ReadOnly := True;
            RTTIItem.IntValue := GetOrdProp(AObject,string(CurrentProperty.Name));
            RTTIItem.TextValue :=
              SetToString(CurrentProperty, GetOrdProp(AObject,string(CurrentProperty.Name)),True);

            RTTIItem.PropInfo := CurrentProperty;

            s := string(CurrentProperty.PropType^.Name);
            {$IFDEF TMSDEBUG}
            outputdebugstring(pchar(s));
            {$ENDIF}

            case GetTypeData(CurrentProperty.PropType^).CompType^.Kind of
            tkEnumeration:
              begin
                SubTypeInfo := GetTypeData(CurrentProperty.PropType^).CompType^;

                k := 1;
                for j := GetTypeData(SubTypeInfo).MinValue to
                         GetTypeData(SubTypeInfo).MaxValue do
                begin
                  with TRTTIInspectorItem(Items.Add) do
                  begin
                    Caption := GetEnumName(SubTypeInfo,j);
                    Level := Level + 1;
                    PropertyType := ptBoolean;
                    ObjectRef := RTTIItem;
                    BoolValue := GetOrdProp(AObject,string(CurrentProperty.Name)) and k = k;
                    SetProp := True;
                    SetVal := k;
                  end;
                  k := k * 2;
                end;

              end;
            end;
          end;

          if CurrentProperty.PropType^.Kind in [tkRecord,tkArray,tkDynArray] then
          begin
            if CurrentProperty.PropType^.Name = 'TFont' then
            begin
              RTTIItem.PropertyType := ptFont;
              RTTIItem.FontValue.Assign(TFont(GetObjectProp(AObject,string(CurrentProperty.Name))));
            end
            else
            begin
              RTTIItem.PropertyType := ptPropButton;
              RTTIItem.TextValue := '(' + string(CurrentProperty.PropType^.Name) + ')';
            end;
          end;

          if CurrentProperty.PropType^.Kind = tkEnumeration then
          begin
            if CurrentProperty.PropType^.Name <> 'Boolean' then
              RTTIItem.PropertyType := ptValues;

            TypeData := GetTypeData(CurrentProperty.PropType^);

            RTTIItem.TextValue := GetEnumName(TypeData.BaseType^, GetOrdProp(AObject,string(CurrentProperty.Name)));

          end;
        end;
      end;

    finally
      FreeMem(PropList);
    end;
  end;

begin
  FRTTIComponent := Value;

  InspectorBar.BeginUpdate;

  if Assigned(FRTTIComponent) then
  begin
    if Items.Count > 0 then
      Items.Clear;

    AddProps(Value,Value.ClassInfo,0);
  end
  else
    Items.Clear;

  InspectorBar.EndUpdate;

  CollapsAll;


end;

{ TRTTIInspectorItems }

function TRTTIInspectorItems.Add: TRTTIInspectorItem;
begin
  Result := TRTTIInspectorItem(inherited Add);
end;

function TRTTIInspectorItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TRTTIInspectorItem;
end;

function TRTTIInspectorItems.GetItem(Index: Integer): TRTTIInspectorItem;
begin
  Result := TRTTIInspectorItem(inherited Items[Index]);
end;

function TRTTIInspectorItems.Insert(index: Integer): TRTTIInspectorItem;
begin
  Result := TRTTIInspectorItem(inherited Insert(Index));
end;

procedure TRTTIInspectorItems.SetItem(Index: Integer;
  const Value: TRTTIInspectorItem);
begin
  inherited Items[Index] := Value;
end;

{ TRTTIInspectorItem }

constructor TRTTIInspectorItem.Create(Collection: TCollection);
begin
  inherited;
  FSetProp := False;
  FSetVal := 0;
end;

destructor TRTTIInspectorItem.Destroy;
begin
  inherited;
end;

procedure TRTTIInspectorItem.EditChange;
begin
  inherited;
end;

procedure TRTTIInspectorItem.EditStart;
begin
  inherited;
end;

procedure TRTTIInspectorItem.EditStop;
begin
  inherited;
end;

end.
