unit uPropertyManager;

interface
  uses Windows, Classes, Sysutils, uDesignIntf, uDesignEditors, TypInfo, Grids;

  type
  TuPropertyList = class;
  TuExpandProperties=class;
  TuCustomPropertiesManager=class;
  TuPropertyRec  = class;
  TuLastPropertyPath = class;

  IuRttiPainter  = interface(IInterface)
    ['{B06536BF-FA73-450D-A9E8-019944A344CF}']
    procedure PaintProperties(Path: TuLastPropertyPath; const SameClass: boolean=False{设计的同一个类型,不同实例});
    procedure ChangeProperties;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);   //将来自于其他地方输入的字符转到编辑器
    procedure Refresh;
    function GetActivePropertyRec: TuPropertyRec;
  end;

  TuPropertyRec  = class
  private
    FParentProperty: TuPropertyRec;
    FISReferenceSubProperty: boolean;
    FProperty: IProperty;
    FOwnerPropertyList,
    FSubProperty: TuPropertyList;
    FExpand: boolean;
    FHasChild: boolean;
    FExpandProp: TuExpandProperties;
    FISReference: boolean;
    procedure SetExpaned(const Value: boolean);
    procedure GetPropProc(const Prop: IProperty);
    procedure GetMethodProc(const Prop: IProperty);
    function ParentPropertyExpand: boolean;
  public
    constructor Create(ExpandProp: TuExpandProperties; AOwner: TuPropertyList; AProperty: IProperty);
    destructor Destroy; override;

    procedure GetSubProperties;
    procedure GetSubMethods;
    procedure RefreshReferenceProperties; //刷新子列表

    property _Property: IProperty read FProperty;
    property SubProperty: TuPropertyList read FSubProperty;
    property OwnerProperty: TuPropertyList read FOwnerPropertyList;
    property ParentProperty: TuPropertyRec read FParentProperty;
    property Expand: boolean read FExpand write SetExpaned;
    property HasChild: boolean read FHasChild;
    property ISReferenceSubProperty: boolean read FISReferenceSubProperty;
    property ISReference: boolean read FISReference;
  end;

  TuPropertyList = class(TList)
  private
    FOwnerProperty: TuPropertyRec;
    FManager: TuCustomPropertiesManager;
    FLevel: Integer;
    FISReferenceSubProperty: boolean;
    function GetProperty(Index: Integer): TuPropertyRec;
    procedure SetProperty(Index: Integer; const Value: TuPropertyRec);
    function GetISExpand: boolean;
  public
    constructor Create(Manager: TuCustomPropertiesManager; AOwner: TuPropertyRec);
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TuPropertyRec read GetProperty write SetProperty;
    property Level: Integer read FLevel;
    property ISReferenceSubProperty: boolean read FISReferenceSubProperty;
  end;

  TuExpandProperties = class(TList)
  private
    function GetProperty(Index: Integer): TuPropertyRec;
    procedure SetProperty(Index: Integer; const Value: TuPropertyRec);
  public
    property Items[Index: Integer]: TuPropertyRec read GetProperty write SetProperty;
  end;

  TuExpandTypes  = class(TList)
  private
    function GetTypeInfo(index: Integer): PTypeInfo;
    procedure SetTypeInfo(index: Integer; const Value: PTypeInfo);
  protected
    procedure CollapseTypeInfo(const TypInfo: PTypeInfo);
    procedure ExpandTypeInfo(const TypInfo: PTypeInfo);
    function Contain(const TypInfo: PTypeInfo): boolean;
  public
    property Items[index: Integer]: PTypeInfo read GetTypeInfo write SetTypeInfo;
  end;

  TuLastPropertyPath = class(TList)
  private
    function GetItem(index: Integer): PPropInfo;
  protected
    procedure WriteAcivePathList(CurProp: TuPropertyRec);
  public
    destructor Destroy; override;

    property Items[index: Integer]: PPropInfo read GetItem;
  end;

  TuOnAddProperty  = procedure(const Prop: IProperty; var CanAdd: boolean) of Object;
  TuCustomPropertiesManager  = class(TComponent)
  private
    FDesigner:  IDesigner;
    FSelection: IDesignerSelections;
    FPropList,
    FMethodList:  TuPropertyList;
    FExpandTypes: TuExpandTypes;
    FExpand,
    FExpandMethods:  TuExpandProperties;

    FPropPainter,
    FMethodPainter: IuRttiPainter;
    FOnAddMethod,
    FOnAddProperty:TuOnAddProperty;

    procedure GetPropProc(const Prop: IProperty);
    procedure GetMethodPropProc(const Prop: IProperty);
    procedure SetDesigner(const Value: IDesigner);
    procedure SetMethodPainter(const Value: IuRttiPainter);
    procedure SetPropPainter(const Value: IuRttiPainter);
  protected
    FActivePainter: IuRttiPainter;
    procedure ExpandProperty(AProperty: TuPropertyRec);
    procedure CollapseProperty(AProperty: TuPropertyRec);
    function CanAddProperty(const Prop: IProperty): boolean;
    function CanAddMethod(const Method: IProperty): boolean;
    function MakeLastPropertyPath: TuLastPropertyPath;
    procedure PaintProperties(Path: TuLastPropertyPath; const SameClass: boolean=False);
    function MakeLastMethodPath: TuLastPropertyPath;
    procedure PaintMethods(Path: TuLastPropertyPath; const SameClass: boolean=False);
    procedure ChangeProperties;
    procedure ChangeMethods;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwer: TComponent); override;
    destructor Destroy; override;

    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);

    procedure SetSelection(const List: IDesignerSelections);
    procedure RefreshPainter;

    property ExpandProperties: TuExpandProperties read FExpand;
    property ExpandMethodProperties: TuExpandProperties read FExpandMethods;
    property Designer: IDesigner read FDesigner write SetDesigner;
    property PropertyPainter: IuRttiPainter read FPropPainter write SetPropPainter;
    property MethodPainter: IuRttiPainter read FMethodPainter write SetMethodPainter;
    property OnAddMethod: TuOnAddProperty read FOnAddMethod write FOnAddMethod;
    property OnAddProperty:TuOnAddProperty read FOnAddProperty write FOnAddProperty;
  end;

  TuPropertiesManager = class(TuCustomPropertiesManager)
  published
    property OnAddMethod;
    property OnAddProperty;
  end;

implementation
//  uses VCLEditors;

{ TuPropertyList }

procedure TuPropertyList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].HasChild then
      Items[I].SubProperty.Clear;
  inherited;
end;

constructor TuPropertyList.Create(Manager: TuCustomPropertiesManager; AOwner: TuPropertyRec);
begin
  inherited Create;
  FManager    :=  Manager;
  FOwnerProperty :=  AOwner;
end;

destructor TuPropertyList.Destroy;
begin
  FOwnerProperty :=  nil;
  FManager    :=  nil;
  inherited;
end;

function TuPropertyList.GetISExpand: boolean;
begin
  Result  :=  (FOwnerProperty<>nil) and FOwnerProperty.FExpand;
end;

function TuPropertyList.GetProperty(Index: Integer): TuPropertyRec;
begin
  Result  :=  TuPropertyRec(inherited Items[Index])
end;

procedure TuPropertyList.SetProperty(Index: Integer; const Value: TuPropertyRec);
begin
  inherited Items[Index]  :=  Value;
end;

{ TuPropertyRec }

constructor TuPropertyRec.Create(ExpandProp: TuExpandProperties; AOwner: TuPropertyList; AProperty: IProperty);
begin
  inherited Create;
  FParentProperty     :=  AOwner.FOwnerProperty;
  FOwnerPropertyList  :=  AOwner;
  FProperty           :=  AProperty;
  FSubProperty        :=  nil;
  FExpandProp         :=  ExpandProp;
  FExpand             :=  False;
  FISReference        :=  Supports(AProperty, IReferenceProperty);
  FISReferenceSubProperty :=  AOwner.FISReferenceSubProperty;
  AOwner.Add(Self);
  if FOwnerPropertyList.FLevel=0 then
    ExpandProp.Add(Self);
end;

destructor TuPropertyRec.Destroy;
begin
  FOwnerPropertyList  :=  nil;
  FProperty           :=  nil;
  if Assigned(FSubProperty) then
    FreeAndNil(FSubProperty);
  inherited;
end;

procedure TuPropertyRec.GetMethodProc(const Prop: IProperty);
var
  PropRec:  TuPropertyRec;
begin
  if (Prop.GetPropType^.Kind=tkClass) and
    not Supports(Prop, IReferenceProperty) then Exit;

  if FOwnerPropertyList.FManager.CanAddMethod(Prop) then
  begin
    PropRec :=  TuPropertyRec.Create(FExpandProp, FSubProperty, Prop);
    PropRec.GetSubMethods;
    //只有当上级属性出于展开状态且当前属性可以展开才展开
    if PropRec.HasChild and ParentPropertyExpand and FOwnerPropertyList.FManager.FExpandTypes.Contain(PropRec.FProperty.GetPropType) then
      PropRec.Expand :=  True;
  end;
end;

procedure TuPropertyRec.GetPropProc(const Prop: IProperty);
var
  PropRec:  TuPropertyRec;
begin
  if FOwnerPropertyList.FManager.CanAddProperty(Prop) then
  begin
    PropRec :=  TuPropertyRec.Create(FExpandProp, FSubProperty, Prop);
    PropRec.GetSubProperties;
    //只有当上级属性出于展开状态且当前属性可以展开才展开
    if PropRec.HasChild and ParentPropertyExpand and FOwnerPropertyList.FManager.FExpandTypes.Contain(PropRec.FProperty.GetPropType) then
      PropRec.Expand :=  True;
  end;
end;

procedure TuPropertyRec.GetSubMethods;
var
  ISRefProp: boolean;
  RefProperty: IReferenceProperty;
  RefSel: IDesignerSelections;
begin
  ISRefProp :=  Supports(FProperty, IReferenceProperty, RefProperty) and
                (RefProperty.GetComponentReference<>nil);

  if ISRefProp {and (paSubProperties in FProperty.GetAttributes)}  then
  begin
    FHasChild           :=  True;
    FSubProperty        :=  TuPropertyList.Create(FOwnerPropertyList.FManager, Self);
    FSubProperty.FISReferenceSubProperty  :=  ISRefProp;
    FSubProperty.FLevel :=  FOwnerPropertyList.FLevel  + 1;
    if ISRefProp then
    begin
      RefSel :=  TDesignerSelections.Create;
      RefSel.Add(RefProperty.GetComponentReference);
      GetComponentProperties(RefSel, tkMethods+[tkClass, tkInterface], TPropertyEditor(FProperty).Designer, GetMethodProc);
    end;
  end;
end;

procedure TuPropertyRec.GetSubProperties;
var
  ISRefProp: boolean;
  RefProperty: IReferenceProperty;
  RefSel: IDesignerSelections;
begin
  ISRefProp :=  Supports(FProperty, IReferenceProperty, RefProperty) and
                (RefProperty.GetComponentReference<>nil);
  if (paSubProperties in FProperty.GetAttributes) or ISRefProp then
  begin
    FHasChild           :=  True;
    FSubProperty        :=  TuPropertyList.Create(FOwnerPropertyList.FManager, Self);
    FSubProperty.FISReferenceSubProperty  :=  ISRefProp;
    FSubProperty.FLevel :=  FOwnerPropertyList.FLevel  + 1;
    if ISRefProp then
    begin
      RefSel :=  TDesignerSelections.Create;
      RefSel.Add(RefProperty.GetComponentReference);
      GetComponentProperties(RefSel, tkProperties, TPropertyEditor(FProperty).Designer, GetPropProc);
    end
    else
      FProperty.GetProperties(GetPropProc);
  end;
end;

function TuPropertyRec.ParentPropertyExpand: boolean;
begin
  Result  :=  (FParentProperty<>nil) and FParentProperty.FExpand;
end;

procedure TuPropertyRec.RefreshReferenceProperties;
var
  RefProperty: IReferenceProperty;
  RefSel: IDesignerSelections;
  C: TComponent;
  I: Integer;
  APropList,
  AMethodList: TuPropertyList;
  PropInfo: PPropInfo;
  PRec, MRec: TuPropertyRec;
  PExd, MExd: boolean;
begin
  PRec  :=  nil;
  MRec  :=  nil;
  if Supports(FProperty, IReferenceProperty, RefProperty) then
  begin
    PropInfo    :=  FProperty.GetPropInfo;
    APropList   :=  FOwnerPropertyList.FManager.FPropList;
    AMethodList :=  FOwnerPropertyList.FManager.FMethodList;
    C :=  RefProperty.GetComponentReference;
    //找出属性列表对应项
    for I := 0 to APropList.Count - 1 do
      if APropList.Items[I].FProperty.GetPropInfo= PropInfo then
      begin
        PRec  :=  APropList.Items[I];
        PExd  :=  PRec.Expand;
        Break;
      end;
    //找出事件列表对应项
    for I := 0 to AMethodList.Count - 1 do
      if AMethodList.Items[I].FProperty.GetPropInfo=PropInfo then
      begin
        MRec  :=  AMethodList.Items[I];
        MExd  :=  MRec.Expand;
        Break;
      end;

    if PRec<>nil then
    begin
      if PRec.FSubProperty<>nil then
      begin
        PRec.Expand  :=  False;
        PRec.FSubProperty.Clear;
      end;
      PRec.FHasChild  :=  False;
      if C<>nil then
      begin
        PRec.GetSubProperties;
        PRec.Expand  :=  PExd;
      end;
    end;
    if MRec<>nil then
    begin
      if MRec.FSubProperty<>nil then
      begin
        MRec.Expand  :=  False;
        MRec.FSubProperty.Clear;
      end;
      MRec.FHasChild  :=  False;
      if C<>nil then
      begin
        MRec.GetSubMethods;
        MRec.Expand  :=  MExd;
      end;
    end;

    if PRec<>nil then
      FOwnerPropertyList.FManager.ChangeProperties;

    if MRec<>nil then
      FOwnerPropertyList.FManager.ChangeMethods;
  end;
end;

procedure TuPropertyRec.SetExpaned(const Value: boolean);
begin
  if (FExpand<>Value) and FHasChild then
  begin
    FExpand := Value;

    if FExpand then
    begin
      FSubProperty.FManager.ExpandProperty(Self);
      FSubProperty.FManager.FExpandTypes.ExpandTypeInfo(FProperty.GetPropType);
    end
    else
    begin
      FSubProperty.FManager.CollapseProperty(Self);
      FSubProperty.FManager.FExpandTypes.CollapseTypeInfo(FProperty.GetPropType);
    end;
  end;
end;

{ TuCustomPropertiesManager }

function TuCustomPropertiesManager.CanAddMethod(
  const Method: IProperty): boolean;
begin
  Result  :=  true;
  if (TObject(Method) is TMethodProperty) and Assigned(FOnAddMethod) then
    FOnAddMethod(Method, Result);
end;

function TuCustomPropertiesManager.CanAddProperty(
  const Prop: IProperty): boolean;
begin
  Result  :=  true;
  if Assigned(FOnAddProperty) then
    FOnAddProperty(Prop, Result);
end;

procedure TuCustomPropertiesManager.ChangeMethods;
begin
  if Assigned(FMethodPainter) then
    FMethodPainter.ChangeProperties;
end;

procedure TuCustomPropertiesManager.ChangeProperties;
begin
  if Assigned(FPropPainter) then
    FPropPainter.ChangeProperties;
end;

procedure TuCustomPropertiesManager.CollapseProperty(AProperty: TuPropertyRec);

  procedure DoCollapseProperty(_AProperty: TuPropertyRec);
  var
    I: Integer;
    Item: TuPropertyRec;
  begin
    if _AProperty.FHasChild then
      for I := 0 to _AProperty.SubProperty.Count - 1 do
      begin
        Item  :=  _AProperty.SubProperty.Items[I];
        DoCollapseProperty(Item);
        _AProperty.FExpandProp.Remove(Item);
      end;
  end;

begin
  DoCollapseProperty(AProperty);
  ChangeProperties;
  ChangeMethods;
end;

constructor TuCustomPropertiesManager.Create(AOwer: TComponent);
begin
  inherited;
  FDesigner         :=  nil;
  FPropPainter      :=  nil;
  FMethodPainter    :=  nil;
  FActivePainter    :=  nil;
  FSelection        :=  nil;
  FPropList         :=  TuPropertyList.Create(Self, nil);
  FMethodList       :=  TuPropertyList.Create(Self, nil);
  FPropList.FLevel  :=  0;
  FExpand           :=  TuExpandProperties.Create;
  FExpandMethods    :=  TuExpandProperties.Create;
  FExpandTypes      :=  TuExpandTypes.Create;
end;

destructor TuCustomPropertiesManager.Destroy;
begin
  FreeAndNil(FPropList);
  FreeAndNil(FMethodList);
  FreeAndNil(FExpand);
  FreeAndNil(FExpandMethods);
  FreeAndNil(FExpandTypes);
  FSelection      :=  nil;
  FDesigner       :=  nil;
  FPropPainter    :=  nil;
  FMethodPainter  :=  nil;
  inherited;
end;

procedure TuCustomPropertiesManager.ExpandProperty(AProperty: TuPropertyRec);

  procedure DoExpandProperty(_AProperty: TuPropertyRec);
  var
    IDX, I: Integer;
    Item: TuPropertyRec;
  begin
    IDX :=  _AProperty.FExpandProp.IndexOf(_AProperty);
    if _AProperty.SubProperty<>nil then
      for I := 0 to _AProperty.SubProperty.Count - 1 do
        _AProperty.FExpandProp.Insert(IDX+I+1, _AProperty.SubProperty.Items[I]);

    for I := 0 to _AProperty.SubProperty.Count - 1 do
    begin
      Item  :=  _AProperty.SubProperty.Items[I];
      if Item.FHasChild and Item.FExpand then
          DoExpandProperty(Item);
      Item  :=  nil;
    end;
  end;

begin
  DoExpandProperty(AProperty);
  ChangeProperties;
  ChangeMethods;
end;

procedure TuCustomPropertiesManager.GetMethodPropProc(const Prop: IProperty);
var
  PropRec:  TuPropertyRec;
begin
  if (Prop.GetPropType^.Kind=tkClass) and
    not Supports(Prop, IReferenceProperty) then Exit;
  if CanAddMethod(Prop) then
  begin
    PropRec :=  TuPropertyRec.Create(FExpandMethods, FMethodList, Prop);
    PropRec.GetSubMethods;
//    if FExpandTypes.Contain(PropRec.FProperty.GetPropType) then
//      PropRec.Expand :=  true;
  end;
end;

procedure TuCustomPropertiesManager.GetPropProc(const Prop: IProperty);
var
  PropRec:  TuPropertyRec;
begin
  if CanAddProperty(Prop) then
  begin
    PropRec :=  TuPropertyRec.Create(FExpand, FPropList, Prop);
    PropRec.GetSubProperties;
//    if FExpandTypes.Contain(PropRec.FProperty.GetPropType) then
//      PropRec.Expand :=  true;
    PropRec :=  nil;
  end;
end;

function TuCustomPropertiesManager.MakeLastMethodPath: TuLastPropertyPath;
begin
  Result  :=  TuLastPropertyPath.Create;
  if FMethodPainter<>nil then
    Result.WriteAcivePathList(FMethodPainter.GetActivePropertyRec);
end;

function TuCustomPropertiesManager.MakeLastPropertyPath: TuLastPropertyPath;
begin
  Result  :=  TuLastPropertyPath.Create;
  if FPropPainter<>nil then
    Result.WriteAcivePathList(FPropPainter.GetActivePropertyRec);
end;

procedure TuCustomPropertiesManager.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin
  if FActivePainter<>nil then
    FActivePainter.ModalEdit(EditKey, ReturnWindow);
end;

procedure TuCustomPropertiesManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation=opRemove then
  begin
    if (FMethodPainter<>nil) and AComponent.IsImplementorOf(FMethodPainter) then
      FMethodPainter  :=  nil
    else if (FPropPainter<>nil) and AComponent.IsImplementorOf(FPropPainter) then
      FPropPainter    :=  nil;
  end;
end;

procedure TuCustomPropertiesManager.PaintMethods(Path: TuLastPropertyPath; const SameClass: boolean=False);
begin
  if FMethodPainter<>nil then
    FMethodPainter.PaintProperties(Path, SameClass);
end;

procedure TuCustomPropertiesManager.PaintProperties(Path: TuLastPropertyPath; const SameClass: boolean=False);
begin
  if FPropPainter<>nil then
    FPropPainter.PaintProperties(Path, SameClass);
end;

procedure TuCustomPropertiesManager.RefreshPainter;
begin
  if FPropPainter<>nil then
    FPropPainter.Refresh;
  if FMethodPainter<>nil then
    FMethodPainter.Refresh;
end;

procedure TuCustomPropertiesManager.SetDesigner(const Value: IDesigner);
begin
  FDesigner := Value;
end;

procedure TuCustomPropertiesManager.SetMethodPainter(
  const Value: IuRttiPainter);
begin
//  ReferenceInterface(FMethodPainter, opRemove);
  if FMethodPainter<>Value then
    FMethodPainter := Value;
//  ReferenceInterface(FMethodPainter, opInsert);
end;

procedure TuCustomPropertiesManager.SetPropPainter(
  const Value: IuRttiPainter);
begin
//  ReferenceInterface(FPropPainter, opRemove);
  if FPropPainter<>Value then
    FPropPainter := Value;
//  ReferenceInterface(FPropPainter, opInsert);
end;

procedure TuCustomPropertiesManager.SetSelection(const List: IDesignerSelections);

  procedure _AutoOpenProps(PropList: TuPropertyList);
  var
    I: Integer;
    Prop: TuPropertyRec;
  begin
    for I := 0 to PropList.Count - 1 do
    begin
      Prop  :=  PropList.Items[I];
      if Prop.HasChild and FExpandTypes.Contain(Prop.FProperty.GetPropType) then
      begin
        Prop.Expand :=  true;
        _AutoOpenProps(Prop.SubProperty);
      end;
    end;
  end;

var
  _SameClass: boolean;
  PropPath,
  MethodPath: TuLastPropertyPath;
begin
  if (FSelection<>nil) and List.Equals(FSelection) then Exit;

  _SameClass  :=  (FSelection<>nil) and
                  (FSelection.Count=1) and
                  (List.Count=1) and
                  (FSelection.Items[0].ClassType=List.Items[0].ClassType);
  PropPath    :=  MakeLastPropertyPath;
  MethodPath  :=  MakeLastMethodPath;
  try
    if FSelection<>nil then
      FSelection  :=  nil;
    FSelection :=  TDesignerSelections.Copy(List);

    FExpand.Clear;
    FExpandMethods.Clear;
    FPropList.Clear;
    FMethodList.Clear;
    GetComponentProperties(FSelection, tkProperties, FDesigner, GetPropProc);
    GetComponentProperties(FSelection, tkMethods+[tkClass, tkInterface], FDesigner, GetMethodPropProc);
    if FExpandTypes.Count>0 then
    begin
      _AutoOpenProps(FMethodList);
      _AutoOpenProps(FPropList);
    end;
    PaintProperties(PropPath, _SameClass);
    PaintMethods(MethodPath, _SameClass);
  finally
    FreeAndNil(PropPath);
    FreeAndNil(MethodPath);
  end;
end;

{ TuExpandProperties }

function TuExpandProperties.GetProperty(Index: Integer): TuPropertyRec;
begin
  Result  :=  TuPropertyRec(inherited Items[Index]);
end;

procedure TuExpandProperties.SetProperty(Index: Integer;
  const Value: TuPropertyRec);
begin
  inherited Items[Index]  :=  Value;
end;

{ TuExpandTypes }

procedure TuExpandTypes.CollapseTypeInfo(const TypInfo: PTypeInfo);
var
  I: Integer;
begin
  for I := 0 to self.Count - 1 do
    if Items[I]=TypInfo then
    begin
      Delete(I);
      Break;
    end;
end;

function TuExpandTypes.Contain(const TypInfo: PTypeInfo): boolean;
begin
  Result  :=  IndexOf(TypInfo)>-1;
end;

procedure TuExpandTypes.ExpandTypeInfo(const TypInfo: PTypeInfo);
begin
  if not Contain(TypInfo) then
    Add(TypInfo);
end;

function TuExpandTypes.GetTypeInfo(index: Integer): PTypeInfo;
begin
  Result  :=  PTypeInfo(inherited Items[index]);
end;

procedure TuExpandTypes.SetTypeInfo(index: Integer; const Value: PTypeInfo);
begin
  inherited Items[index]  :=  Value;
end;

{ TuLastPropertyPath }

destructor TuLastPropertyPath.Destroy;
begin
  inherited;
end;

function TuLastPropertyPath.GetItem(index: Integer): PPropInfo;
begin
  Result  :=  PPropInfo(inherited Items[index]);
end;

procedure TuLastPropertyPath.WriteAcivePathList(CurProp: TuPropertyRec);
begin
  if CurProp<>nil then
  begin
    Insert(0, CurProp._Property.GetPropInfo);
    if not (CurProp.OwnerProperty.Level=0) and (CurProp.ParentProperty<>nil) then
      WriteAcivePathList(CurProp.ParentProperty);
  end;
end;

end.
