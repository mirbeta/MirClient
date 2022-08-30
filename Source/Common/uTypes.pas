unit uTypes;

interface
  uses Windows, SysUtils, Classes, Generics.Collections, NativeXmlObjectStorage, ComCtrls,uSyncObj;

type
//  PPlatfromChar   =  {$IFDEF VER220}PAnsiChar{$ELSE}PChar{$ENDIF};
//  PPlatfromChr    =  {$IFDEF VER220}AnsiChar{$ELSE}Char{$ENDIF};
//  PPlatfromString =  {$IFDEF VER220}AnsiString{$ELSE}String{$ENDIF};

  PPlatfromChar   = PAnsiChar;
  PPlatfromChr    =  AnsiChar;
  PPlatfromString = AnsiString;

  TSuitPropertyType = (
    sptNone,
    sptMaxHP, sptMaxMP, sptDC, sptMaxDC, sptMC, sptMaxMC, sptSC, sptMaxSC, sptTc, sptMaxTc, sptPc, sptMaxPc, sptWc, sptMaxWc,
    sptAC, sptMaxAC, sptMAC, sptMaxMAC,
    sptHitPoint{准确度}, sptSpeedPoint{敏捷度}, sptHealthRecover{体力恢复}, sptSpellRecover{魔法恢复},
    sptbtReserved{吸血(虹吸)},
    sptnEXPRATE{经验倍数}, sptnPowerRate{攻击倍数}, sptnMagicRate{魔法倍数}, sptnSCRate{道术倍数}, sptnTCRate, sptnPCRate, sptnWCRate,
    sptnACRate{防御倍数}, sptnMACRate{魔御倍数}, sptnAntiMagic{魔法躲避}, sptnAntiPoison{毒物躲避}, sptnPoisonRecover{中毒恢复},
    sptboTeleport{传送}, sptboParalysis{麻痹}, sptboRevival{复活}, sptboMagicShield{护身}, sptboUnParalysis{防麻痹},sptfMaxHP{血量万分比率},sptfMaxMP{蓝量万分比率}
  );

  //作为Buff的属性类型
  TPropertyType = (
    ptMaxHP,
    ptMaxMP,
    ptDC,
    ptMC,
    ptSC,
    ptTC,
    ptPC,
    ptWC,
    ptAC,
    ptMAC,
    ptHitPoint{准确},
    ptHitSpeed{攻击速度},
    ptSpeedPoint{敏捷},
    ptHealthRecover{体力恢复},
    ptSpellRecover{魔法恢复},
    ptAntiMagic{魔法躲避},
    ptAntiPoison{毒物躲避},
    ptPoisonRecover{中毒恢复},
    ptAbsorbing{吸收伤害},
    ptRebound{伤害反弹},
    ptAttackAdd{伤害加成},
    ptPunchHit{致命一击},
    ptCriticalHit{会心一击},
    ptChangeToMonster{变身},
    ptDrugAttack {致残buff 可能攻击对方导致对方不能跑}
  );

  TGList = class(TList)
  private
    CriticalSection: TFixedCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; inline;
    procedure UnLock; inline;
    function TryLock:Boolean;inline;
  end;

  TGStringList = class(TStringList)
  private
    CriticalSection: TFixedCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; inline;
    procedure UnLock; inline;
  end;

  TSortStringList = class(TGStringList)
  public
    procedure StringSort(Order: Boolean);
    procedure ObjectSort(Order: Boolean);
  end;

  TSStringList = class(TGStringList)
  public
    procedure QuickSort(Order: Boolean);
  end;

  TuSerialObject = class(TPersistent)
  public
    constructor Create; virtual;
    procedure LoadDefault; virtual;
    procedure LoadFromXMLFile(const AXMLFile: String); virtual;
    procedure SaveToXMLFile(const AXMLFile: String); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
  end;

  TSafeDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
  private
    FLockObj: TObject;
  public
    constructor Create(Ownerships: TDictionaryOwnerships);
    destructor Destroy; override;

    procedure Lock; inline;
    procedure UnLock; inline;
  end;

  TSafeList<T> = class(TList<T>)
  private
    CriticalSection: TFixedCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

  IuListViewer = interface
    ['{8B9C8C74-B507-452F-A36B-1C0C2006F38F}']
  end;

  IListViewTrace = interface
    ['{6C80331A-149A-4A64-866F-707DE14E3C97}']
    procedure SetListItems(ListItems: TListItems);
    function CanFillView: Boolean;
  end;

  TuCollectionItem  = class(TCollectionItem)
  private
    FNode: TTreeNode;
    FImageIndex: Integer;
    FListItem: TListItem;
    FEnable: Boolean;
    function isSaveName: Boolean;
  protected
    FName: String;
    procedure SetListItem(const Value: TListItem); virtual;
    procedure SetImageIndex(const Value: Integer); virtual;
    procedure SetNode(const Value: TTreeNode); virtual;
    procedure SetName(const Value: String); virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Parse; virtual;
    procedure AddListItem(ListItems: TListItems; ItemObj: TObject; ACaption: String; ImgIndex: Integer);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure LoadDefault; virtual;
    procedure SetListItems(Value: TListItems); virtual;
    function CanFillView: Boolean; virtual;
    property Node: TTreeNode read FNode write SetNode;
    property ListItem: TListItem read FListItem write SetListItem;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Enable: Boolean read FEnable write FEnable default True;
  published
    property Name: String read FName write SetName stored isSaveName;
  end;
  TuCollectionItemClass = class of TuCollectionItem;

  TuCollection = class(TCollection)
  private
    FImageIndex: Integer;
    FOwner: TPersistent;
    function GetItem(index: Integer): TuCollectionItem;
    procedure SetImageIndex(const Value: Integer);
  protected
    FNode: TTreeNode;
    FFillToTreeView: Boolean;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetNode(const Value: TTreeNode); virtual;
    function GetOwner: TPersistent; override;
    procedure Parse; virtual;
    procedure AddListItem(ListItems: TListItems; ItemObj: TObject; ACaption: String; ImgIndex: Integer); virtual;
  public
    function FindByName(const AName: String): TuCollectionItem;
    function TryGetByName(const AName: String; out Item: TuCollectionItem): Boolean;
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass); virtual;
    destructor Destroy; override;
    function Add: TuCollectionItem;
    function Insert(Index: Integer): TuCollectionItem;
    procedure SetListItems(Value: TListItems); virtual;
  public
    property Items[index: Integer]: TuCollectionItem read GetItem;
    property Node: TTreeNode read FNode write SetNode;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

  _PIntegerList = ^_TIntegerList;
  _TIntegerList = array[0..MaxListSize - 1] of Integer;

  TIntegerList = class
  private
    FCount, FCapacity: Integer;
    FIntegerList: _PIntegerList;
    procedure SetCapacity(NewCapacity: Integer);
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    destructor Destroy; override;
    procedure Clear;
    function Add(Item: Integer): Integer;
    function IndexOf(Item: Integer): Integer;
    procedure Delete(Index: Integer);
    function Remove(Item: Integer): Integer; inline;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

  TPackageHeader = record
    CreateTime: TDateTime;
    ModifyTime: TDateTime;
    Author: String[20];
    Password: String[32];
    Size: LongWord;
    Validate: Byte;
  end;
  PTPackageHeader = ^TPackageHeader;

  TStringsHlp = class helper for TStrings
  public
    function _Line(AIndex: Integer): String;
  end;


implementation

{ TGList }
constructor TGList.Create;
begin
  inherited;
  CriticalSection := TFixedCriticalSection.Create;
end;

destructor TGList.Destroy;
begin
  CriticalSection.Free;
  inherited;
end;

procedure TGList.Lock;
begin
  CriticalSection.Enter;
end;

function TGList.TryLock: Boolean;
begin
  Result := CriticalSection.TryEnter;
end;

procedure TGList.UnLock;
begin
  CriticalSection.Leave;
end;

{ TGStringList }

constructor TGStringList.Create;
begin
  inherited;
  CriticalSection := TFixedCriticalSection.Create;
end;

destructor TGStringList.Destroy;
begin
  CriticalSection.Free;
  inherited;
end;

procedure TGStringList.Lock;
begin
  CriticalSection.Enter;
end;

procedure TGStringList.UnLock;
begin
  CriticalSection.Leave;
end;

procedure TSortStringList.ObjectSort(Order: Boolean);
var
  nIndex, j: Integer;
  TempList: TStringList;
  MinList: TStringList;
  MaxList: TStringList;
  nMax, nMIN, nMaxIndex, nMinIndex: Integer;
begin
  TempList := TStringList.Create;
  MinList := TStringList.Create;
  MaxList := TStringList.Create;
  TempList.AddStrings(Self);
  Clear;
  while True do begin
    if TempList.Count <= 0 then Break;
    nMax := Low(Integer);
    nMIN := High(Integer);
    nMaxIndex := -1;
    nMinIndex := -1;
    nIndex := 0;
    while True do begin
      if TempList.Count <= nIndex then Break;
      j := Integer(TempList.Objects[nIndex]);
      if j > nMax then begin
        nMax := j;
        nMaxIndex := nIndex;
      end;
      if j < nMIN then begin
        nMIN := j;
        nMinIndex := nIndex;
      end;
      Inc(nIndex);
    end;
    if Order then begin
      if nMinIndex = nMaxIndex then begin
        if nMinIndex >= 0 then begin
          MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
          TempList.Delete(nMinIndex);
        end;
      end else begin
        if nMinIndex < nMaxIndex then begin
          if nMaxIndex >= 0 then begin
            MaxList.InsertObject(0, TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
          if nMinIndex >= 0 then begin
            MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
        end else begin
          if nMinIndex >= 0 then begin
            MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
          if nMaxIndex >= 0 then begin
            MaxList.InsertObject(0, TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
        end;
      end;
    end else begin
      if nMinIndex = nMaxIndex then begin
        if nMinIndex >= 0 then begin
          MaxList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
          TempList.Delete(nMinIndex);
        end;
      end else begin
        if nMinIndex < nMaxIndex then begin
          if nMaxIndex >= 0 then begin
            MaxList.AddObject(TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
          if nMinIndex >= 0 then begin
            MinList.InsertObject(0, TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
        end else begin
          if nMinIndex >= 0 then begin
            MinList.InsertObject(0, TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
          if nMaxIndex >= 0 then begin
            MaxList.AddObject(TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
        end;
      end;
    end;
  end;
  if Order then begin
    AddStrings(MinList);
    AddStrings(MaxList);
  end else begin
    AddStrings(MaxList);
    AddStrings(MinList);
  end;
  TempList.Free;
  MinList.Free;
  MaxList.Free;
end;

procedure TSortStringList.StringSort(Order: Boolean);
var
  nIndex, j: Integer;
  TempList: TStringList;
  MinList: TStringList;
  MaxList: TStringList;
  nMax, nMIN, nMaxIndex, nMinIndex: Integer;
begin
  TempList := TStringList.Create;
  MinList := TStringList.Create;
  MaxList := TStringList.Create;
  TempList.AddStrings(Self);
  Clear;
  while True do begin
    if TempList.Count <= 0 then Break;
    nMax := Low(Integer);
    nMIN := High(Integer);
    nMaxIndex := -1;
    nMinIndex := -1;
    nIndex := 0;
    while True do begin
      if TempList.Count <= nIndex then Break;
      j := StrToInt(TempList.Strings[nIndex]);
      if j > nMax then begin
        nMax := j;
        nMaxIndex := nIndex;
      end;
      if j < nMIN then begin
        nMIN := j;
        nMinIndex := nIndex;
      end;
      Inc(nIndex);
    end;
    if Order then begin
      if nMinIndex = nMaxIndex then begin
        if nMinIndex >= 0 then begin
          MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
          TempList.Delete(nMinIndex);
        end;
      end else begin
        if nMinIndex < nMaxIndex then begin
          if nMaxIndex >= 0 then begin
            MaxList.InsertObject(0, TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
          if nMinIndex >= 0 then begin
            MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
        end else begin
          if nMinIndex >= 0 then begin
            MinList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
          if nMaxIndex >= 0 then begin
            MaxList.InsertObject(0, TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
        end;
      end;
    end else begin
      if nMinIndex = nMaxIndex then begin
        if nMinIndex >= 0 then begin
          MaxList.AddObject(TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
          TempList.Delete(nMinIndex);
        end;
      end else begin
        if nMinIndex < nMaxIndex then begin
          if nMaxIndex >= 0 then begin
            MaxList.AddObject(TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
          if nMinIndex >= 0 then begin
            MinList.InsertObject(0, TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
        end else begin
          if nMinIndex >= 0 then begin
            MinList.InsertObject(0, TempList.Strings[nMinIndex], TempList.Objects[nMinIndex]);
            TempList.Delete(nMinIndex);
          end;
          if nMaxIndex >= 0 then begin
            MaxList.AddObject(TempList.Strings[nMaxIndex], TempList.Objects[nMaxIndex]);
            TempList.Delete(nMaxIndex);
          end;
        end;
      end;
    end;
  end;
  if Order then begin
    AddStrings(MinList);
    AddStrings(MaxList);
  end else begin
    AddStrings(MaxList);
    AddStrings(MinList);
  end;
  TempList.Free;
  MinList.Free;
  MaxList.Free;
end;

procedure TSStringList.QuickSort(Order: Boolean); //速度更快的排行
  procedure QuickSortStrListCase(List: TStringList; l, r: Integer);
  var
    I, j: Integer;
    p: string;
  begin
    if List.Count <= 0 then Exit;
    repeat
      I := l;
      j := r;
      p := List[(l + r) shr 1];
      repeat
        if Order then begin //升序
          while CompareStr(List[I], p) < 0 do Inc(I);
          while CompareStr(List[j], p) > 0 do Dec(j);
        end else begin //降序
          while CompareStr(p, List[I]) < 0 do Inc(I);
          while CompareStr(p, List[j]) > 0 do Dec(j);
        end;
        if I <= j then begin
          List.Exchange(I, j);
          Inc(I);
          Dec(j);
        end;
      until I > j;
      if l < j then QuickSortStrListCase(List, l, j);
      l := I;
    until I >= r;
  end;
  procedure AddList(TempList: TStringList; slen: string; s: string; AObject: TObject);
  var
    I: Integer;
    List: TStringList;
    boFound: Boolean;
  begin
    boFound := False;
    if TempList.Count > 0 then begin//20080630
      for I := 0 to TempList.Count - 1 do begin
        if CompareText(TempList.Strings[I], slen) = 0 then begin
          List := TStringList(TempList.Objects[I]);
          List.AddObject(s, AObject);
          boFound := True;
          Break;
        end;
      end;
    end;
    if not boFound then begin
      List := TStringList.Create;
      List.AddObject(s, AObject);
      TempList.AddObject(slen, List);
    end;
  end;
var
  TempList: TStringList;
  List: TStringList;
  I: Integer;
  nLen: Integer;
begin
  TempList := TStringList.Create;
  for I := 0 to Self.Count - 1 do begin
    nLen := Length(Self.Strings[I]);
    AddList(TempList, IntToStr(nLen), Self.Strings[I], Self.Objects[I]);
  end;
  QuickSortStrListCase(TempList, 0, TempList.Count - 1);
  Self.Clear;
  for I := 0 to TempList.Count - 1 do begin
    List := TStringList(TempList.Objects[I]);
    QuickSortStrListCase(List, 0, List.Count - 1);
    Self.AddStrings(List);
    List.Free;
  end;
  TempList.Free;
end;

{ TuSerialObject }

constructor TuSerialObject.Create;
begin
  LoadDefault;
end;

procedure TuSerialObject.LoadDefault;
begin
end;

procedure TuSerialObject.LoadFromStream(AStream: TStream);
begin
  NativeXmlObjectStorage.ObjectLoadFromXmlStream(Self, AStream);
end;

procedure TuSerialObject.LoadFromXMLFile(const AXMLFile: String);
begin
  if not FileExists(AXMLFile) then
    SaveToXMLFile(AXMLFile);
  NativeXmlObjectStorage.ObjectLoadFromXmlFile(Self, AXMLFile);
end;

procedure TuSerialObject.SaveToStream(AStream: TStream);
begin
  NativeXmlObjectStorage.ObjectSaveToXmlStream(Self, AStream);
end;

procedure TuSerialObject.SaveToXMLFile(const AXMLFile: String);
begin
  NativeXmlObjectStorage.ObjectSaveToXmlFile(Self, AXMLFile);
end;

{ TuCollectionItem }

procedure TuCollectionItem.AddListItem(ListItems: TListItems; ItemObj: TObject;
  ACaption: String; ImgIndex: Integer);
begin
  with ListItems.Add do
  begin
    Caption :=  ACaption;
    Data    :=  ItemObj;
    ImageIndex  :=  ImgIndex;
  end;
end;

procedure TuCollectionItem.AssignTo(Dest: TPersistent);
begin
  if not (Dest is TuCollectionItem) then Exit;

  TuCollectionItem(Dest).FName  :=  FName;
  TuCollectionItem(Dest).FEnable  :=  FEnable;
  if TuCollectionItem(Dest).FNode <> nil then
    TuCollectionItem(Dest).FNode.Text := FName;
  if TuCollectionItem(Dest).ListItem <> nil then
    TuCollectionItem(Dest).ListItem.Caption := FName;
end;

function TuCollectionItem.CanFillView: Boolean;
begin
  Result  :=  False;
end;

constructor TuCollectionItem.Create(ACollection: TCollection);
begin
  inherited;
  FEnable     :=  True;
  FImageIndex :=  8;
  FListItem   :=  nil;
  FNode       :=  nil;
end;

destructor TuCollectionItem.Destroy;
begin
  if Assigned(FNode) then
    FNode.Delete;
  inherited;
end;

function TuCollectionItem.isSaveName: Boolean;
begin
  Result := FName <> '';
end;

procedure TuCollectionItem.LoadDefault;
begin
end;

procedure TuCollectionItem.Parse;
begin
end;

procedure TuCollectionItem.SetImageIndex(const Value: Integer);
begin
  if Value<>FImageIndex then
  begin
    FImageIndex := Value;
    if Assigned(FNode) then
    begin
      FNode.ImageIndex  :=  FImageIndex;
      FNode.SelectedIndex :=  FImageIndex;
    end;
    if FListItem <> nil then
    begin
      FListItem.ImageIndex := FImageIndex;
      FListItem.StateIndex := FImageIndex;
    end;
  end;
end;

procedure TuCollectionItem.SetListItem(const Value: TListItem);
begin
  FListItem := Value;
  if Assigned(FListItem) then
  begin
    FListItem.Caption :=  Self.FName;
    FListItem.ImageIndex := Self.ImageIndex;
    FListItem.StateIndex := Self.ImageIndex;
    FListItem.Data := Self;
  end;
end;

procedure TuCollectionItem.SetListItems(Value: TListItems);
begin
end;

procedure TuCollectionItem.SetName(const Value: String);
begin
  if Value <> FName then
  begin
    FName := Value;
    if Assigned(FNode) then
      FNode.Text  :=  FName;
    if Assigned(FListItem) then
      FListItem.Caption :=  FName;
  end;
end;

procedure TuCollectionItem.SetNode(const Value: TTreeNode);
begin
  if FNode <> nil then
    FNode.Delete;

  FNode := Value;
  if Assigned(FNode) then
  begin
    FNode.ImageIndex  :=  FImageIndex;
    FNode.SelectedIndex  :=  FImageIndex;
  end;
end;

{ TuCollection }

function TuCollection.Add: TuCollectionItem;
var
  AChildNode: TTreeNode;
begin
  Result  :=  TuCollectionItem(inherited Add);
  if FFillToTreeView and Assigned(FNode) then
  begin
    AChildNode  :=  TTreeView(FNode.TreeView).Items.AddChildObject(FNode, Result.Name, Result);
    Result.Node :=  AChildNode;
  end;
end;

procedure TuCollection.AddListItem(ListItems: TListItems; ItemObj: TObject;
  ACaption: String; ImgIndex: Integer);
begin
  TuCollectionItem(ItemObj).FListItem := ListItems.Add;
  with TuCollectionItem(ItemObj).FListItem do
  begin
    Caption :=  ACaption;
    Data    :=  ItemObj;
    ImageIndex  :=  ImgIndex;
  end;
end;

procedure TuCollection.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  inherited;
  if not (Dest is TuCollection) then Exit;
  for I := 0 to Count - 1 do
    TuCollection(Dest).Add.Assign(Items[I]);
end;

constructor TuCollection.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FFillToTreeView := True;
  FOwner      :=  AOwner;
  FImageIndex :=  7;
end;

destructor TuCollection.Destroy;
begin
  FOwner  :=  nil;
  inherited;
end;

function TuCollection.FindByName(const AName: String): TuCollectionItem;
var
  I: Integer;
begin
  Result  :=  nil;
  for I := 0 to Count -1 do
    if SameText(AName, Items[I].FName) then
    begin
      Result  :=  Items[I];
      Exit;
    end;
end;

function TuCollection.GetItem(index: Integer): TuCollectionItem;
begin
  Result  :=  TuCollectionItem(inherited Items[index]);
end;

function TuCollection.GetOwner: TPersistent;
begin
  Result  :=  FOwner;
end;

function TuCollection.Insert(Index: Integer): TuCollectionItem;
var
  ARelative,
  AChildNode: TTreeNode;
begin
  Result := TuCollectionItem(inherited Insert(Index));
  if FFillToTreeView and Assigned(FNode) then
  begin
    ARelative := FNode.Item[Index];
    if ARelative <> nil then
      AChildNode  :=  TTreeView(FNode.TreeView).Items.AddNode(nil, ARelative, Result.Name, Result, naInsert)
    else
      AChildNode  :=  TTreeView(FNode.TreeView).Items.AddChildObject(FNode, Result.Name, Result);
    Result.Node :=  AChildNode;
  end;
end;

procedure TuCollection.Parse;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Parse;
end;

procedure TuCollection.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TuCollection.SetListItems(Value: TListItems);
var
  I: Integer;
  Item: TListItem;
begin
  if Value <> nil then
  begin
    Value.BeginUpdate;
    try
      for I := 0 to Count - 1 do
        AddListItem(Value, Items[I], Items[I].Name, Items[I].ImageIndex);
    finally
      Value.EndUpdate;
    end;
  end;
end;

procedure TuCollection.SetNode(const Value: TTreeNode);
var
  I: Integer;
  AChildNode: TTreeNode;
begin
  if Value <> nil then
  begin
    FNode := Value;
    FNode.ImageIndex  :=  FImageIndex;
    FNode.SelectedIndex  :=  FImageIndex;
    if FFillToTreeView then
    begin
      for I := 0 to Count -1 do
      begin
        AChildNode    :=  TTreeView(Value.TreeView).Items.AddChildObject(FNode, Items[I].Name, Items[I]);
        Items[I].Node :=  AChildNode;
      end;
    end;
  end
  else
  begin
    for I := 0 to Count -1 do
      Items[I].Node :=  nil;
    if FNode <> nil then
      FNode.Delete;
    FNode :=  nil;
  end;
end;

function TuCollection.TryGetByName(const AName: String;
  out Item: TuCollectionItem): Boolean;
begin
  Item := FindByName(AName);
  Result := Item <> nil;
end;

{ TSafeDictionary<TKey, TValue> }

constructor TSafeDictionary<TKey, TValue>.Create(Ownerships: TDictionaryOwnerships);
begin
  inherited Create(Ownerships);
  FLockObj  :=  TObject.Create;
end;

destructor TSafeDictionary<TKey, TValue>.Destroy;
begin
  FLockObj.Free;
  inherited;
end;

procedure TSafeDictionary<TKey, TValue>.Lock;
begin
  System.TMonitor.Enter(FLockObj);
end;

procedure TSafeDictionary<TKey, TValue>.UnLock;
begin
  System.TMonitor.Exit(FLockObj);
end;

{ TSafeList<T> }

constructor TSafeList<T>.Create;
begin
  inherited;
  CriticalSection := TFixedCriticalSection.Create;
end;

destructor TSafeList<T>.Destroy;
begin
  CriticalSection.Free;
  inherited;
end;

procedure TSafeList<T>.Lock;
begin
  CriticalSection.Enter;
end;

procedure TSafeList<T>.UnLock;
begin
  CriticalSection.Leave;
end;

{TIntegerList}

destructor TIntegerList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TIntegerList.Clear;
begin
  SetCapacity(0);
end;

procedure TIntegerList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FIntegerList, NewCapacity * SizeOf(Integer));
    FCapacity := NewCapacity;
  end;
end;

function TIntegerList.Add(Item: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + 16);
  FIntegerList^[Result] := Item;
  Inc(FCount);
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FIntegerList[Result] = Item then
      Exit;
  Result := -1;
end;

function TIntegerList.Get(Index: Integer): Integer;
begin
  Result := 0;
  if (Index >= 0) or (Index < FCount) then
    Result := FIntegerList^[Index];
end;

procedure TIntegerList.Put(Index: Integer; const Value: Integer);
begin
  if (Index >= 0) or (Index < FCount) then
  begin
    if Value <> FIntegerList^[Index] then
      FIntegerList^[Index] := Value;
  end;
end;

procedure TIntegerList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    Dec(FCount);
    if Index < FCount then
      System.Move(FIntegerList^[Index + 1], FIntegerList^[Index], (FCount - Index) * SizeOf(Integer));
  end;
end;

function TIntegerList.Remove(Item: Integer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

{ TStringsHlp }

function TStringsHlp._Line(AIndex: Integer): String;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
    Result := Strings[AIndex];
end;

end.
